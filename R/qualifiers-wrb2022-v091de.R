# ============================================================================
# WRB 2022 (4th ed.) -- Qualifiers Bloco D + E (v0.9.1).
#
# Closes the canonical Ch 4 principal-qualifier coverage by wiring the
# remaining 16 RSGs:
#
#   Bloco D (steppe + arid + cool moist):
#     CH Chernozems   KS Kastanozems   PH Phaeozems    UM Umbrisols
#     DU Durisols     GY Gypsisols     CL Calcisols    RT Retisols
#
#   Bloco E (argic-clay-rich + alluvial + minimal-development):
#     AC Acrisols     LX Lixisols      AL Alisols      LV Luvisols
#     CM Cambisols    AR Arenosols     RG Regosols     FL Fluvisols
#
# Most principals these RSGs need were already implemented across
# Blocos A / B / C. The four additions in this file are:
#
#   - Cutanic   (visible illuvial clay coatings on argic ped surfaces)
#   - Glossic   (mollic with albeluvic glossae penetrating below)
#   - Brunic    (cambic horizon present, used for Arenosols)
#   - Protic    (Arenosol with NO incipient / illuvial subsoil horizon
#                -- A over C, no cambic, no argic, no spodic)
#
# These four are sufficient to fully cover the Ch 4 lists for D + E
# without changing the v0.9 / v0.9.1.A-C function set.
# ============================================================================


#' Cutanic qualifier (ct): visible illuvial clay coatings on argic-
#' horizon ped surfaces (the "Cutanic Luvisol" / "Cutanic Argissol"
#' signature). v0.9.1: argic horizon passes AND the schema column
#' \code{clay_films_amount} contains "common", "many", or "continuous" (or
#' "shiny" -- common Brazilian descriptor for nitic surfaces) in some
#' argic layer.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_cutanic <- function(pedon) {
  arg <- argic(pedon)
  if (!isTRUE(arg$passed))
    return(DiagnosticResult$new(name = "Cutanic", passed = FALSE,
            layers = integer(0), evidence = list(argic = arg),
            missing = arg$missing %||% character(0),
            reference = "WRB (2022) Ch 5, Cutanic"))
  h <- pedon$horizons
  ly <- arg$layers
  films <- h$clay_films_amount[ly]
  ok <- !is.na(films) & grepl("common|many|continuous|shiny",
                                  films, ignore.case = TRUE)
  passed <- any(ok)
  DiagnosticResult$new(
    name = "Cutanic", passed = passed,
    layers = ly[ok],
    evidence = list(argic = arg, clay_films_amount = films),
    missing = if (all(is.na(films))) "clay_films_amount" else character(0),
    reference = "WRB (2022) Ch 5, Cutanic"
  )
}


#' Glossic qualifier (gs): mollic horizon penetrated by albeluvic
#' tongues (glossae). Diagnostic of Glossic Chernozems / Phaeozems on
#' the steppe / forest-steppe transition.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_glossic <- function(pedon) {
  mo <- mollic(pedon)
  gl <- albeluvic_glossae(pedon)
  passed <- isTRUE(mo$passed) && isTRUE(gl$passed)
  layers <- if (passed) union(mo$layers, gl$layers) else integer(0)
  DiagnosticResult$new(
    name = "Glossic", passed = passed,
    layers = layers,
    evidence = list(mollic = mo, albeluvic_glossae = gl),
    missing = unique(c(mo$missing, gl$missing)),
    reference = "WRB (2022) Ch 5, Glossic"
  )
}


#' Brunic qualifier (br): \emph{incipient-only} subsurface alteration --
#' cambic horizon within the upper 100 cm AND no argic, spodic,
#' ferralic, or nitic horizon present. Used by WRB 2022 Ch 4 for
#' Arenosols that have begun to develop a weak Bw without crossing
#' into Cambisol / Acrisol / Lixisol / Ferralsol territory; in those
#' RSGs the cambic alone is the gating diagnostic and Brunic would be
#' redundant.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_brunic <- function(pedon) {
  cm  <- cambic(pedon)
  if (!isTRUE(cm$passed))
    return(DiagnosticResult$new(name = "Brunic", passed = FALSE,
            layers = integer(0), evidence = list(cambic = cm),
            missing = cm$missing %||% character(0),
            reference = "WRB (2022) Ch 5, Brunic"))
  arg <- argic(pedon)
  sp  <- spodic(pedon)
  fr  <- ferralic(pedon)
  nt  <- nitic_horizon(pedon)
  has_other_b <- isTRUE(arg$passed) || isTRUE(sp$passed) ||
                   isTRUE(fr$passed) || isTRUE(nt$passed)
  in_upper <- intersect(cm$layers, .in_upper(pedon, 100))
  passed <- !has_other_b && length(in_upper) > 0L
  DiagnosticResult$new(
    name = "Brunic", passed = passed,
    layers = if (passed) in_upper else integer(0),
    evidence = list(cambic = cm, argic = arg, spodic = sp,
                    ferralic = fr, nitic = nt),
    missing = cm$missing %||% character(0),
    reference = "WRB (2022) Ch 5, Brunic"
  )
}


#' Protic qualifier (pr)
#'
#' WRB 2022, Chapter 5: "showing no soil horizon development, with the
#' exception of a cryic horizon, which may be present". v0.9.217: the function
#' checked only that no cambic, argic, spodic, ferralic or nitic horizon was
#' present, so a Leptosol with an umbric horizon, an Arenosol with sideralic
#' properties or a Technosol with a mollic horizon was Protic. Now any
#' diagnostic horizon or property of soil formation, or a genetic horizon in
#' the designations (O, A, E or B, bar a cryic one), rules it out; a profile
#' with nothing to tell from is NA.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_protic <- function(pedon) {
  h <- pedon$horizons
  diag <- list(
    cambic = cambic(pedon), argic = argic(pedon), spodic = spodic(pedon),
    ferralic = ferralic(pedon), nitic = nitic_horizon(pedon),
    mollic = mollic(pedon), umbric = umbric_horizon(pedon),
    chernic = chernic(pedon), natric = natric_horizon(pedon),
    calcic = calcic(pedon), gypsic = gypsic(pedon),
    sideralic = sideralic_properties(pedon))
  developed <- names(Filter(function(d) isTRUE(d$passed), diag))
  d <- h$designation
  cryic <- tryCatch(cryic_horizon(pedon)$layers, error = function(e) integer(0))
  genetic <- which(!is.na(d) & grepl("^[0-9]*\\s*[OAEB]", d))
  genetic <- setdiff(genetic, cryic)
  have_desg <- any(!is.na(d) & nzchar(trimws(d)))
  passed <- if (length(developed) || length(genetic)) FALSE
            else if (have_desg) TRUE
            else NA
  DiagnosticResult$new(
    name = "Protic", passed = passed,
    layers = if (isTRUE(passed)) seq_len(nrow(h)) else integer(0),
    evidence = list(developed = developed,
                    genetic_horizons = if (length(genetic)) d[genetic] else character(0)),
    missing = if (is.na(passed)) "designation" else character(0),
    reference = "WRB (2022) Ch 5, Protic"
  )
}
