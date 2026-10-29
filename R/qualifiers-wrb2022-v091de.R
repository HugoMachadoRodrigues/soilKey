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


#' Brunic qualifier (br): a layer that meets cambic criteria 3 and 4 but
#' fails criterion 1, not of claric material.
#'
#' v0.9.316: WRB 2022 Ch 5 (p. 129), "having a layer, >= 15 cm thick and
#' starting <= 50 cm from the mineral soil surface, that meets diagnostic
#' criteria 3 and 4 of the cambic horizon but fails diagnostic criterion 1 and
#' does not consist of claric material". Before v0.9.316 it asked for a whole
#' cambic horizon, which meets criterion 1 (a texture of sandy loam or finer),
#' so every Brunic it found was a layer the text rules out. Each layer is read
#' against the cambic criteria of Ch 3.1.5 (p. 39-40):
#' \itemize{
#'   \item criterion 1 failed: sand or loamy sand, and not very fine sand or
#'         loamy very fine sand (criterion 1.b). soilKey records no sand
#'         fractions, so a sand or loamy sand layer is undecided (NA), as for
#'         Neobrunic; a layer of sandy loam or finer is not Brunic;
#'   \item criterion 3, evidence of soil formation, from the Munsell colours
#'         (moist), clay, CaCO3 and Fe against the layers above and below
#'         (test_cambic_soil_formation()); the cambic horizon itself does not
#'         test it (Hugo, N9). Evidence found counts; none found leaves it
#'         undecided, as criteria 3.c (gypsum) and 3.d (Fe) are seldom
#'         recorded;
#'   \item criterion 4: not part of a plough layer or of the horizons it
#'         lists (.wrb_cambic_part_of_other());
#'   \item not claric material (test_claric_munsell(), per layer).
#' }
#' The layer is made of adjoining horizons that each qualify. The result is
#' therefore FALSE or NA until the very fine sand subclasses are recorded.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_brunic <- function(pedon) {
  h <- pedon$horizons
  n <- nrow(h)
  ref <- "WRB (2022) Ch 5, Brunic (p. 129); cambic criteria 1, 3 and 4, Ch 3.1.5 (p. 39-40)"
  ms <- .q_mineral_surface_cm(h)
  if (is.na(ms) || !n)
    return(DiagnosticResult$new(name = "Brunic", passed = NA, layers = integer(0),
                                evidence = list(), missing = "top_cm", reference = ref))
  d <- as.character(h$designation %||% rep(NA_character_, n))
  cand <- which(.wrb_mineral_layer(h) & !(!is.na(d) & grepl(.WRB_ROCK_DESIGNATION, d)) &
                  !is.na(h$top_cm) & !is.na(h$bottom_cm) & h$bottom_cm > ms)
  st <- rep(FALSE, n)
  # criterion 1 failed: sandy loam or finer meets it; sand or loamy sand may be
  # very fine sand (criterion 1.b), which no column tells
  fine <- .wrb_sandy_loam_or_finer(h, cand)
  cand <- cand[!(fine %in% TRUE)]
  if (length(cand)) {
    c1_fail <- rep(NA, length(cand))
    # criterion 3
    sf <- test_cambic_soil_formation(h, candidate_layers = cand)
    c3 <- ifelse(cand %in% sf$layers, TRUE, NA)
    # criterion 4
    part <- .wrb_cambic_part_of_other(pedon, cand)
    c4 <- !(part$excluded[cand] | .wrb_plough_designation(d[cand]) %in% TRUE)
    # not claric material
    cl <- test_claric_munsell(h, candidate_layers = cand)
    claric <- vapply(cand, function(i) cl$details[[as.character(i)]]$passed %||% NA, NA)
    all4 <- cbind(c1_fail, c3, c4, !claric)
    st[cand] <- ifelse(rowSums(!is.na(all4) & !all4) > 0, FALSE,
                       ifelse(rowSums(is.na(all4)) == 0, TRUE, NA))
  }
  run <- .q_run_tristate(h, st, 15, from = ms, max_start = ms + 50)
  DiagnosticResult$new(
    name = "Brunic", passed = run$passed,
    layers = if (isTRUE(run$passed)) run$layers else integer(0),
    evidence = list(layer_status = st, candidate_layers = cand,
                    mineral_surface_cm = ms),
    missing = if (is.na(run$passed))
                c("sand fractions (very fine sand, WRB Annex 1 Table 8.40)",
                  "munsell_hue_moist", "munsell_value_moist", "munsell_chroma_moist")
              else character(0),
    reference = ref
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
