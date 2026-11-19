# ============================================================================
# WRB 2022 (4th ed.) -- Qualifiers (Ch 5, pp 127-156).
#
# v0.9 implements ~50 core qualifiers covering the most common usage
# across the 32 RSGs. The remaining ~150 sub-qualifiers (Hyper-, Hypo-,
# Proto-, Endo-, Epi-, Bathy-, Thapto-, Supra-, Ano-, Panto-, Poly-,
# Amphi-, Kato- variants and the Novic combinations) are scheduled for
# v0.9.1.
#
# Each qualifier function returns a DiagnosticResult: passed = TRUE if
# the qualifier applies to the pedon at the canonical reference depth
# (typically <= 100 cm from the mineral soil surface, per Ch 5 marker
# (2)). The qualifier system in classify_wrb2022() filters the per-RSG
# applicable list (Ch 4 tables) and formats the result as
#   "<Principal>(s) <RSG> (<Supplementary>(s))"
# per the rules of Ch 2.2 (p 25).
# ============================================================================


# Helper: returns the layer indices that start within max_top_cm of the
# mineral soil surface. Used as a depth gate for almost every
# qualifier.
.in_upper <- function(pedon, max_top_cm = 100) {
  h <- pedon$horizons
  which(!is.na(h$top_cm) & h$top_cm <= max_top_cm)
}

# Helper: build a DiagnosticResult for a thin "presence" qualifier.
.q_presence <- function(name, base_diag, max_top_cm = 100, pedon = NULL) {
  passed <- isTRUE(base_diag$passed) &&
              (is.null(pedon) ||
                 length(intersect(base_diag$layers,
                                      .in_upper(pedon, max_top_cm))) > 0L)
  layers <- if (passed) base_diag$layers else integer(0)
  DiagnosticResult$new(
    name = name, passed = passed, layers = layers,
    evidence = list(base = base_diag),
    missing = base_diag$missing %||% character(0),
    reference = "WRB (2022) Ch 5"
  )
}


# ---------- HORIZON-BASED PRINCIPAL QUALIFIERS ------------------------------

#' Albic qualifier (ab): albic horizon <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_albic       <- function(pedon) .q_presence("Albic",       albic(pedon),       100, pedon)

#' Andic qualifier (an): andic OR vitric properties combined >= 30 cm.
#' v0.9 simplification: passes if andic_properties or vitric_properties
#' passes within 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_andic       <- function(pedon) {
  ap <- andic_properties(pedon); vp <- vitric_properties(pedon)
  passed <- (isTRUE(ap$passed) || isTRUE(vp$passed)) &&
              length(intersect(union(ap$layers, vp$layers),
                                   .in_upper(pedon, 100))) > 0L
  DiagnosticResult$new(
    name = "Andic", passed = passed,
    layers = union(ap$layers, vp$layers),
    evidence = list(andic = ap, vitric = vp),
    missing = unique(c(ap$missing, vp$missing)),
    reference = "WRB (2022) Ch 5, Andic"
  )
}

#' Anthric qualifier (ak): anthric properties.
#'
#' v0.9.217: WRB 2022 Ch 5, "having anthric properties" (Ch 3.2.4: human-made
#' mollic or umbric horizons -- a mollic or umbric horizon, signs of
#' ploughing / liming or >= 430 mg kg-1 Mehlich-3 P in the upper 20 cm, and
#' < 5 \% traces of animal activity at its base). The v0.9 wrapper read
#' anthric_horizons(), the Anthrosol test for hortic / irragric / plaggic /
#' pretic / terric horizons, which is a different diagnostic. soilKey has no
#' anthric-properties diagnostic, so the result is FALSE when there is no
#' mollic or umbric horizon (criterion 1) and NA otherwise.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_anthric <- function(pedon) {
  mo <- tryCatch(mollic(pedon), error = function(e) NULL)
  um <- tryCatch(umbric_horizon(pedon), error = function(e) NULL)
  none <- !is.null(mo) && isFALSE(mo$passed) && !is.null(um) && isFALSE(um$passed)
  DiagnosticResult$new(
    name = "Anthric", passed = if (none) FALSE else NA, layers = integer(0),
    evidence = list(mollic = mo, umbric = um),
    missing = if (none) character(0)
              else "anthric properties (no soilKey diagnostic for WRB Ch 3.2.4)",
    reference = "WRB (2022) Ch 5, Anthric")
}

# ---- v0.9.113: thin presence wrappers over existing diagnostics ------------
# Each wraps a diagnostic already implemented in
# diagnostics-{horizons,materials}-wrb-v033.R; wired per RSG in
# inst/rules/wrb2022/qualifiers.yaml. Verified byte-identical on the 44
# canonical fixtures (the diagnostics that fire do so only on RSGs whose
# Ch 4 applicable list does not list the qualifier).

#' Aeolic qualifier (ae): aeolic (wind-sorted) material <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_aeolic      <- function(pedon) .q_presence("Aeolic",      aeolic_material(pedon), 100, pedon)

#' Fragic qualifier (fg): fragic horizon <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_fragic      <- function(pedon) .q_presence("Fragic",      fragic(pedon),      100, pedon)

#' Limonic qualifier (lm): limonic (bog-iron) horizon <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_limonic     <- function(pedon) .q_presence("Limonic",     limonic(pedon),     100, pedon)

#' Tsitelic qualifier (ts): tsitelic (red, low-activity) horizon <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_tsitelic    <- function(pedon) .q_presence("Tsitelic",    tsitelic(pedon),    100, pedon)

#' Calcic qualifier (cc): calcic horizon <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_calcic      <- function(pedon) .q_presence("Calcic",      calcic(pedon),      100, pedon)

#' Cambic qualifier (cm): cambic horizon <= 50 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_cambic      <- function(pedon) {
  cb <- cambic(pedon)
  if (!isTRUE(cb$passed)) return(.q_presence("Cambic", cb, 50, pedon))
  # v0.9.412 (Hugo, L10): the qualifier also asks cambic criterion 3, evidence
  # of soil formation (WRB 2022 Ch 3.1.5, p. 39; Ch 5, Cambic, p. 130), of
  # every horizon of a run >= 15 cm starting <= 50 cm from the mineral soil
  # surface. As for Brunic, evidence found counts and none found leaves the
  # horizon undecided: 3.c (gypsum) and 3.d (Fe) are seldom recorded.
  h <- pedon$horizons
  ms <- .q_mineral_surface_cm(h); if (is.na(ms)) ms <- 0
  sf <- test_cambic_soil_formation(h, candidate_layers = cb$layers)
  st <- ifelse(seq_len(nrow(h)) %in% cb$layers,
               ifelse(seq_len(nrow(h)) %in% sf$layers, TRUE, NA), FALSE)
  run <- .q_run_tristate(h, st, 15, from = ms, max_start = ms + 50)
  DiagnosticResult$new(name = "Cambic", passed = run$passed, layers = run$layers,
    evidence = list(base = cb, soil_formation = sf),
    missing = if (is.na(run$passed)) "cambic criterion 3 (evidence of soil formation)"
              else character(0),
    reference = "WRB (2022) Ch 5, Cambic (p. 130); Ch 3.1.5 criterion 3 (p. 39)")
}

#' Cryic qualifier (cy): cryic horizon <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_cryic       <- function(pedon) .q_presence("Cryic",       cryic_conditions(pedon), 100, pedon)

#' Duric qualifier (du): duric horizon <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_duric       <- function(pedon) .q_presence("Duric",       duric_horizon(pedon),    100, pedon)

#' Ferralic qualifier (fl): ferralic horizon <= 150 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_ferralic    <- function(pedon) .q_presence("Ferralic",    ferralic(pedon),    150, pedon)

#' Ferric qualifier (fr): ferric horizon <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_ferric      <- function(pedon) .q_presence("Ferric",      ferric(pedon),      100, pedon)

#' Fluvic qualifier (fv): fluvic material >= 25 cm thick starting <= 75 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_fluvic      <- function(pedon) .q_presence("Fluvic",      fluvic_material(pedon),  75,  pedon)

#' Folic qualifier (fo): folic horizon at the soil surface. v0.9
#' delegates to histic_horizon with surface-only filter.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_folic       <- function(pedon) {
  h <- histic_horizon(pedon)
  surface <- length(h$layers) > 0L &&
               any(pedon$horizons$top_cm[h$layers] <= 5, na.rm = TRUE)
  passed <- isTRUE(h$passed) && surface
  DiagnosticResult$new(
    name = "Folic", passed = passed,
    layers = if (passed) h$layers else integer(0),
    evidence = list(histic = h),
    missing = h$missing,
    reference = "WRB (2022) Ch 5, Folic"
  )
}

#' Gleyic qualifier (gl): WRB 2022 Ch 5 (p. 135), a layer \\>= 25 cm thick
#' starting \\<= 75 cm from the mineral soil surface with gleyic properties
#' throughout and reducing conditions in some parts of every sublayer.
#' v0.9.312: the layer is read as the Gleysols gate reads its 25 cm
#' (adjoining sublayers count together, each gleyic and reducing); until
#' then gleyic properties in any layer starting \\<= 50 cm were enough, which
#' since the 2.5Y and 5Y colours of criterion 1.b would have named Gleyic a
#' dark 2.5Y 2/2 topsoil with no sign of reduction.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_gleyic <- function(pedon) {
  h <- pedon$horizons
  gp <- gleyic_properties(pedon, max_top_cm = Inf)
  z <- gp$evidence$redox_layers
  gl <- z$gleyic
  gl[gp$layers] <- TRUE                  # opt-in designation inference
  ms <- .q_mineral_surface_cm(h)
  if (is.na(ms)) ms <- 0
  run <- .adjoining_runs(h, .and3v(gl, z$reducing), min_cm = 25, max_top_cm = ms + 75)
  DiagnosticResult$new(
    name = "Gleyic", passed = run$passed,
    layers = if (isTRUE(run$passed)) run$layers else integer(0),
    evidence = list(base = gp),
    missing = if (is.na(run$passed)) gp$missing %||% character(0) else character(0),
    reference = "WRB (2022) Ch 5, Gleyic (p. 135)"
  )
}

#' Gypsic qualifier (gy): gypsic horizon <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_gypsic      <- function(pedon) .q_presence("Gypsic",      gypsic(pedon),      100, pedon)

#' Histic qualifier (hi): histic horizon at or near the surface.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_histic      <- function(pedon) .q_presence("Histic",      histic_horizon(pedon),   100, pedon)

#' Leptic qualifier (le): continuous rock <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_leptic      <- function(pedon) .q_presence("Leptic",      continuous_rock(pedon),  100, pedon)

#' Mollic qualifier (mo): mollic horizon.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_mollic      <- function(pedon) .q_presence("Mollic",      mollic(pedon),      100, pedon)

#' Natric qualifier (na): natric horizon <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_natric      <- function(pedon) .q_presence("Natric",      natric_horizon(pedon),   100, pedon)

#' Nitic qualifier (ni): nitic horizon <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_nitic       <- function(pedon) .q_presence("Nitic",       nitic_horizon(pedon),    100, pedon)

#' Petrocalcic qualifier (pc): petrocalcic horizon <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_petrocalcic <- function(pedon) .q_presence("Petrocalcic", petrocalcic(pedon),      100, pedon)

#' Petroduric qualifier (pd): petroduric horizon <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_petroduric  <- function(pedon) .q_presence("Petroduric",  petroduric(pedon),       100, pedon)

#' Petrogypsic qualifier (pg): petrogypsic horizon <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_petrogypsic <- function(pedon) .q_presence("Petrogypsic", petrogypsic(pedon),      100, pedon)

#' Petroplinthic qualifier (pp): petroplinthic horizon <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_petroplinthic <- function(pedon) .q_presence("Petroplinthic", petroplinthic(pedon), 100, pedon)

#' Plinthic qualifier (pl): plinthic horizon <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_plinthic    <- function(pedon) .q_presence("Plinthic",    plinthic(pedon),    100, pedon)

#' Retic qualifier (rt): retic properties <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_retic       <- function(pedon) .q_presence("Retic",       retic_properties(pedon), 100, pedon)

#' Salic qualifier (sz): salic horizon <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_salic       <- function(pedon) .q_presence("Salic",       salic(pedon),       100, pedon)

#' Spodic qualifier (sd): spodic horizon <= 200 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_spodic      <- function(pedon) .q_presence("Spodic",      spodic(pedon),      200, pedon)

#' Stagnic qualifier (st): WRB 2022 Ch 5 (p. 147), a layer \\>= 25 cm thick
#' starting \\<= 75 cm from the mineral soil surface with stagnic properties
#' in which reductimorphic plus oximorphic features cover \\>= 25\% of its
#' area (weighted average) and reducing conditions in some part of it.
#' v0.9.312: the layer, its area and its reducing conditions are read as the
#' Stagnosols gate reads them (adjoining sublayers count together); until
#' then stagnic properties in any layer starting \\<= 75 cm were enough. The
#' hydragric exclusion is not read.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_stagnic <- function(pedon) {
  h <- pedon$horizons
  st <- stagnic_properties(pedon, max_top_cm = Inf)
  z <- st$evidence$redox_layers
  ms <- .q_mineral_surface_cm(h)
  if (is.na(ms)) ms <- 0
  # a run of stagnic layers (sure, or sure and undecided) >= 25 cm thick,
  # starting <= 75 cm, with >= 25% redox area and reducing conditions
  test <- function(sel, reducing_ok) {
    run <- .adjoining_runs(h, sel, min_cm = 25, max_top_cm = ms + 75)
    if (!isTRUE(run$passed)) return(list(ok = FALSE, layers = integer(0)))
    L <- run$layers
    w <- pmax(0, h$bottom_cm[L] - h$top_cm[L])
    list(ok = sum(w * z$area[L]) / sum(w) >= 25 && any(z$reducing[L] %in% reducing_ok),
         layers = L)
  }
  sure  <- test(z$stagnic %in% TRUE, TRUE)
  maybe <- test(!(z$stagnic %in% FALSE), c(TRUE, NA))
  passed <- if (sure$ok) TRUE else if (maybe$ok) NA else FALSE
  DiagnosticResult$new(
    name = "Stagnic", passed = passed,
    layers = if (isTRUE(passed)) sure$layers else integer(0),
    evidence = list(base = st),
    missing = if (is.na(passed)) st$missing %||% character(0) else character(0),
    reference = "WRB (2022) Ch 5, Stagnic (p. 147)"
  )
}

#' Umbric qualifier (um): umbric horizon.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_umbric      <- function(pedon) .q_presence("Umbric",      umbric_horizon(pedon),   100, pedon)

#' Vertic qualifier (vr): vertic horizon <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_vertic      <- function(pedon) .q_presence("Vertic",      vertic_horizon(pedon),   100, pedon)

#' Vitric qualifier (vi): vitric properties >= 30 cm within 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_vitric      <- function(pedon) .q_presence("Vitric",      vitric_properties(pedon), 100, pedon)


# ---------- CHEMISTRY-BASED PRINCIPAL QUALIFIERS ----------------------------

# v0.9.310: the Acric, Alic, Lixic and Luvic qualifiers as WRB 2022 Ch 5
# defines them (p. 127-128 and 139-140): an argic horizon starting <= 100 cm
# from the mineral soil surface; a CEC of < 24 cmolc/kg clay in some
# subhorizon within 150 cm (Acric, Lixic) or of >= 24 throughout within 150 cm
# (Alic, Luvic); and exchangeable Al > exchangeable (Ca+Mg+K+Na) (Acric,
# Alic) or Al <= the bases (Lixic, Luvic) in half or more of 50-100 cm or of
# the lower half of the mineral soil above a limiting layer starting <= 100 cm.
# Until v0.9.309 they called the RSG gates, which used the WRB 2014 tests.
.qual_argic_family <- function(pedon, name, cec, al) {
  r <- .argic_derived_gate(pedon, tolower(name), cec = cec, al = al,
                           ref = paste0("WRB (2022) Ch 5, ", name))
  DiagnosticResult$new(name = name, passed = r$passed, layers = r$layers,
                       evidence = r$evidence, missing = r$missing,
                       reference = r$reference, notes = r$notes)
}

#' Acric qualifier (ac): argic <= 100 cm, CEC < 24 in some subhorizon within
#' 150 cm, exchangeable Al > bases (WRB 2022 Ch 5, p. 127).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_acric <- function(pedon) .qual_argic_family(pedon, "Acric", cec = "low", al = "gt")

#' Alic qualifier (al): argic <= 100 cm, CEC >= 24 throughout within 150 cm,
#' exchangeable Al > bases (WRB 2022 Ch 5, p. 128).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_alic <- function(pedon) .qual_argic_family(pedon, "Alic", cec = "high", al = "gt")

#' Lixic qualifier (lx): argic <= 100 cm, CEC < 24 in some subhorizon within
#' 150 cm, exchangeable Al <= bases (WRB 2022 Ch 5, p. 139).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_lixic <- function(pedon) .qual_argic_family(pedon, "Lixic", cec = "low", al = "le")

#' Luvic qualifier (lv): argic <= 100 cm, CEC >= 24 throughout within 150 cm,
#' exchangeable Al <= bases (WRB 2022 Ch 5, p. 140).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_luvic <- function(pedon) .qual_argic_family(pedon, "Luvic", cec = "high", al = "le")

#' WRB 2022 exchangeable-acidity status over a depth window.
#'
#' WRB 2022 (Ch 5) defines Dystric/Eutric (and the Hyper-/Ortho- variants) by
#' \strong{exchangeable Al vs exchangeable bases}, NOT by base saturation:
#' a layer is "dystric-side" when exch. Al \eqn{>} \code{factor} times exch.
#' (Ca+Mg+K+Na), and "eutric-side" when exch. bases \eqn{\ge} \code{factor}
#' times exch. Al (\code{factor = 1} for Dystric/Eutric, \code{4} for the Hyper-
#' variants). Mineral layers use \code{al_sat_pct} (primary) or \code{al_cmol}
#' against the summed base cations; organic layers (\code{oc_pct >= 20}) use the
#' WRB Histosol pH branch (pH_water cutoffs 5.5, or 4.5/6.5 when \code{factor >=
#' 4}). Per the user's "strict" policy there is NO base-saturation fallback: a
#' layer with neither Al datum nor (for organic layers) pH is undeterminable and
#' counts as NA thickness.
#'
#' Returns thickness-weighted fractions of the candidate layers (those
#' overlapping \code{[dmin, dmax]}) that are dystric-side / eutric-side / NA,
#' plus the qualifying layer indices.
#'
#' v0.9.410: \code{material} chooses the layers. "mineral" (Dystric, Eutric and
#' their subqualifiers outside Histosols: "having one or more layers consisting
#' of mineral material", WRB 2022 Ch 5, p. 131-133) leaves organic layers out of
#' the combined thickness; "organic" (Histosols: "the part with organic
#' material", p. 131-132) reads only organic layers, by their pH; "any" keeps the
#' reading before v0.9.410 (organic layers by pH, the rest by cations), used by
#' the Epi- and Endo- forms. The comparisons allow for the binary error of the
#' summed cations (0.4 + 0.3 + 0.1 is stored just below 4 x 0.2): WRB 2022 Ch
#' 2.1, rule 6 (p. 23) takes the values as recorded, and their sum is exact.
#' The result also carries the per-layer status (\code{status}, \code{cand},
#' \code{thk}) for Oligoeutric.
#' @noRd
.wrb_acidity_fracs <- function(pedon, dmin = 20, dmax = 100, factor = 1,
                               material = c("any", "mineral", "organic")) {
  material <- match.arg(material)
  h <- pedon$horizons
  empty <- list(dystric = 0, eutric = 0, mid = 0, na = 1, layers_d = integer(0),
                layers_e = integer(0), total = 0, cand = integer(0),
                thk = numeric(0), status = list())
  if (is.null(h) || nrow(h) == 0L) return(empty)
  ov_top <- pmax(h$top_cm, dmin)
  ov_bot <- pmin(h$bottom_cm, dmax)
  thk <- pmax(ov_bot - ov_top, 0)
  org <- (!is.na(h$oc_pct) & h$oc_pct >= 20) |
         (is.na(h$oc_pct) & !is.na(h$designation) &
            grepl("^[0-9]*[OH]", h$designation))
  keep <- switch(material, any = TRUE, mineral = !org, organic = org)
  cand <- which(!is.na(thk) & thk > 0 & keep)
  if (length(cand) == 0L) return(empty)
  total <- sum(thk[cand])
  d_thk <- 0; e_thk <- 0; mid_thk <- 0; na_thk <- 0
  layers_d <- integer(0); layers_e <- integer(0); st <- list()
  eps <- 1e-9
  for (i in cand) {
    oc <- h$oc_pct[i]
    # status: TRUE=strong dystric-side, FALSE=strong eutric-side,
    #         "mid"=data present but neither (only for factor>1), NA=data absent.
    status <- NA
    if ((material == "any" && !is.na(oc) && oc >= 20) || material == "organic") {
      # organic material -> WRB Histosol pH branch
      ph <- h$ph_h2o[i]
      d_cut <- if (factor >= 4) 4.5 else 5.5
      e_cut <- if (factor >= 4) 6.5 else 5.5
      if (!is.na(ph)) {
        if (ph < d_cut) status <- TRUE
        else if (ph >= e_cut) status <- FALSE
        else status <- "mid"
      }
    } else {
      # mineral material -> exchangeable Al vs bases
      alc <- h$al_cmol[i]
      bcmol <- c(h$ca_cmol[i], h$mg_cmol[i], h$k_cmol[i], h$na_cmol[i])
      als <- h$al_sat_pct[i]
      if (!is.na(alc) && any(!is.na(bcmol))) {
        b <- sum(bcmol, na.rm = TRUE)
        if (alc - factor * b > eps) status <- TRUE
        else if (b - factor * alc >= -eps) status <- FALSE
        else status <- "mid"
      } else if (!is.na(als)) {
        d_thr <- 100 * factor / (factor + 1)  # al_sat > this <=> Al > factor*bases
        e_thr <- 100 / (factor + 1)           # al_sat <= this <=> bases >= factor*Al
        if (als - d_thr > eps) status <- TRUE
        else if (als - e_thr <= eps) status <- FALSE
        else status <- "mid"
      } else {
        # v0.9.309: no exchangeable cations: the pH, as WRB 2022 Ch 5 allows
        # for these qualifiers ("If these data are not available, pH values
        # may be used according to Annex 2 (Chapter 9.13)"), by Table 9.3.
        status <- .wrb_ph_cation_status(h$ph_h2o[i], h$ph_kcl[i], oc, factor)
      }
    }
    st[[length(st) + 1L]] <- status
    if (isTRUE(status)) { d_thk <- d_thk + thk[i]; layers_d <- c(layers_d, i) }
    else if (isFALSE(status)) { e_thk <- e_thk + thk[i]; layers_e <- c(layers_e, i) }
    else if (identical(status, "mid")) mid_thk <- mid_thk + thk[i]
    else na_thk <- na_thk + thk[i]
  }
  list(dystric = d_thk / total, eutric = e_thk / total, mid = mid_thk / total,
       na = na_thk / total, layers_d = layers_d, layers_e = layers_e,
       total = total, cand = cand, thk = thk[cand], status = st)
}

# v0.9.410: the depth range of Dystric, Eutric and their subqualifiers outside
# Histosols (WRB 2022 Ch 5, p. 131-133): from 20 to 100 cm "of the mineral soil
# surface", or from 20 cm to "a limiting layer starting > 25 cm from the
# mineral soil surface" (Dystric, Eutric, Oligoeutric; `lim_min = 25`,
# `lim_strict = TRUE`) or "starting >= 50 cm" (Hyper- and Ortho-; `lim_min =
# 50`, `lim_strict = FALSE`), whichever is shallower. The limiting layer is
# that of Ch 2.1, rule 8 (p. 23), .wrb_limiting_layers(). A shallower limiting
# layer does not end the range. Depths are from the soil surface, as
# .wrb_acidity_fracs() reads them. Until v0.9.410 the range was 20-100 cm of
# the soil surface, so a rock at 80 cm counted as 20 cm of missing data.
.wrb_base_status_window <- function(pedon, dmin = 20, dmax = 100, lim_min = 25,
                                    lim_strict = TRUE) {
  h <- pedon$horizons
  ms <- .q_mineral_surface_cm(h)
  if (is.na(ms)) ms <- 0
  top <- ms + dmin; bot <- ms + dmax
  lim <- tryCatch(h$top_cm[.wrb_limiting_layers(pedon)], error = function(e) numeric(0))
  lim <- lim[!is.na(lim)]
  if (length(lim)) {
    l1 <- min(lim)
    if ((lim_strict && l1 > ms + lim_min) || (!lim_strict && l1 >= ms + lim_min))
      bot <- min(bot, l1)
  }
  c(top, bot)
}

# WRB 2022 Annex 2, Ch 9.13, Table 9.3 (p. 220): the pH that corresponds to
# exchangeable (Ca+Mg+K+Na) = exchangeable Al, to bases >= 4 times the Al, and
# to Al > 4 times the bases, for soil organic carbon < 2, 2 to < 7.5 and 7.5 to
# < 20 %. With the carbon unknown, the pH decides only where the three carbon
# classes agree.
.WRB_TABLE_9_3 <- list(
  eq    = list(water = c(4.6, 4.9, 5.4), kcl = c(3.8, 4.1, 4.6)),
  base4 = list(water = c(5.5, 5.9, 6.3), kcl = c(4.7, 5.0, 5.5)),
  al4   = list(water = c(3.9, 4.2, 4.5), kcl = c(3.2, 3.4, 3.7)))

# TRUE (dystric side), FALSE (eutric side), "mid" or NA, from the pH; water
# first, then KCl. factor 1 is Al > bases against bases >= Al; factor 4 is
# Al > 4 x bases against bases >= 4 x Al, with "mid" between.
.wrb_ph_cation_status <- function(ph_w, ph_k, oc, factor = 1) {
  cls <- if (is.na(oc)) NA_integer_ else if (oc < 2) 1L else if (oc < 7.5) 2L
         else if (oc < 20) 3L else NA_integer_
  for (k in c("water", "kcl")) {
    ph <- if (k == "water") ph_w else ph_k
    if (is.null(ph) || is.na(ph)) next
    if (factor < 4) {
      lim <- .WRB_TABLE_9_3$eq[[k]]
      if (!is.na(cls)) return(ph < lim[cls])
      if (ph < min(lim)) return(TRUE)
      if (ph >= max(lim)) return(FALSE)
      return(NA)
    }
    hi <- .WRB_TABLE_9_3$base4[[k]]; lo <- .WRB_TABLE_9_3$al4[[k]]
    if (!is.na(cls)) {
      if (ph >= hi[cls]) return(FALSE)
      if (ph < lo[cls]) return(TRUE)
      return("mid")
    }
    if (ph >= max(hi)) return(FALSE)
    if (ph < min(lo)) return(TRUE)
    if (ph >= max(lo) && ph < min(hi)) return("mid")
    return(NA)
  }
  NA
}

# v0.9.410: the fractions over the layers and depths WRB 2022 Ch 5 (p. 131-133)
# names: in Histosols the organic material within 100 cm of the soil surface,
# by its pH; in other soils the mineral layers of .wrb_base_status_window().
# Without an RSG the "other soils" branch is used, as for Alcalic.
.wrb_base_fracs <- function(pedon, factor, rsg_code = NULL, lim_min = 25,
                            lim_strict = TRUE) {
  if (identical(rsg_code, "HS")) {
    s0 <- suppressWarnings(min(pedon$horizons$top_cm, na.rm = TRUE))
    if (!is.finite(s0)) s0 <- 0
    return(.wrb_acidity_fracs(pedon, s0, s0 + 100, factor, material = "organic"))
  }
  w <- .wrb_base_status_window(pedon, lim_min = lim_min, lim_strict = lim_strict)
  .wrb_acidity_fracs(pedon, w[1], w[2], factor, material = "mineral")
}

# Build the Dystric/Eutric result for a depth window from the fractions.
# side = "dystric" (Al-dom in >= half) or "eutric" (base-dom in > half).
.wrb_base_status_result <- function(pedon, name, side, dmin = 20, dmax = 100,
                                    rsg_code = NULL) {
  f <- .wrb_base_fracs(pedon, 1, rsg_code, lim_min = 25, lim_strict = TRUE)
  miss <- c("al_sat_pct", "al_cmol")
  if (f$total == 0)
    return(DiagnosticResult$new(name = name, passed = NA, layers = integer(0),
      evidence = list(reason = "no layers in depth window"),
      missing = "top_cm", reference = paste0("WRB (2022) Ch 5, ", name)))
  if (side == "dystric") {
    passed <- if (f$dystric >= 0.5) TRUE
              else if (f$dystric + f$na < 0.5) FALSE else NA
    layers <- f$layers_d
  } else {
    passed <- if (f$eutric > 0.5) TRUE
              else if (f$eutric + f$na <= 0.5) FALSE else NA
    layers <- f$layers_e
  }
  DiagnosticResult$new(name = name, passed = passed,
    layers = if (isTRUE(passed)) layers else integer(0),
    evidence = list(dystric_frac = f$dystric, eutric_frac = f$eutric,
                    na_frac = f$na),
    missing = if (is.na(passed)) miss else character(0),
    reference = paste0("WRB (2022) Ch 5 (Dystric p.130-131, Eutric p.131-132): ",
                       "exchangeable Al vs exchangeable bases over 20-100 cm; ",
                       "Dystric = Al > bases in >= half, Eutric = bases >= Al in ",
                       "the major part. Not base saturation (that was WRB 2014). ",
                       name))
}

# Build a Hyper- result: factor-1 dominance THROUGHOUT + factor-4 in major part.
.wrb_hyper_status_result <- function(pedon, name, side, dmin = 20, dmax = 100,
                                     rsg_code = NULL) {
  f1 <- .wrb_base_fracs(pedon, 1, rsg_code, lim_min = 50, lim_strict = FALSE)
  f4 <- .wrb_base_fracs(pedon, 4, rsg_code, lim_min = 50, lim_strict = FALSE)
  miss <- c("al_sat_pct", "al_cmol")
  if (f1$total == 0)
    return(DiagnosticResult$new(name = name, passed = NA, layers = integer(0),
      evidence = list(reason = "no layers in depth window"),
      missing = "top_cm", reference = paste0("WRB (2022) Ch 5, ", name)))
  if (side == "dystric") {
    through <- f1$dystric            # 1 means all layers dystric-side
    other   <- f1$eutric
    major4  <- f4$dystric
    na4     <- f4$na
    layers  <- f1$layers_d
  } else {
    through <- f1$eutric
    other   <- f1$dystric
    major4  <- f4$eutric
    na4     <- f4$na
    layers  <- f1$layers_e
  }
  passed <- if (other > 0) FALSE                         # a contrary layer
            else if (f1$na > 0) NA                       # cannot confirm "throughout"
            else if (major4 > 0.5) TRUE                  # throughout + strong major part
            else if (major4 + na4 <= 0.5) FALSE
            else NA
  DiagnosticResult$new(name = name, passed = passed,
    layers = if (isTRUE(passed)) layers else integer(0),
    evidence = list(through_frac = through, contrary_frac = other,
                    strong_major_frac = major4),
    missing = if (is.na(passed)) miss else character(0),
    reference = paste0("WRB (2022) Ch 5 (Dystric p.130-131, Eutric p.131-132): ",
                       "exchangeable Al vs exchangeable bases over 20-100 cm; ",
                       "Dystric = Al > bases in >= half, Eutric = bases >= Al in ",
                       "the major part. Not base saturation (that was WRB 2014). ",
                       name))
}

#' Dystric qualifier (dy), WRB 2022 Ch 5.
#'
#' Exchangeable Al \eqn{>} exchangeable bases (Ca+Mg+K+Na) in half or more of
#' the combined thickness of mineral layers between 20 and 100 cm of the
#' mineral soil surface or a limiting layer (v0.9.410; in Histosols, pH_water
#' < 5.5 in the organic material, \code{rsg_code = "HS"}). Uses
#' \code{al_sat_pct} or \code{al_cmol} vs the base cations; no base-saturation
#' fallback (strict).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rsg_code The RSG being named (the resolver passes it).
#' @noRd
qual_dystric <- function(pedon, rsg_code = NULL) {
  .wrb_base_status_result(pedon, "Dystric", "dystric", 20, 100, rsg_code)
}

#' Eutric qualifier (eu), WRB 2022 Ch 5.
#'
#' Exchangeable bases (Ca+Mg+K+Na) \eqn{\ge} exchangeable Al in the major part
#' of the combined thickness of mineral layers between 20 and 100 cm of the
#' mineral soil surface or a limiting layer (v0.9.410; in Histosols, pH_water
#' \eqn{\ge} 5.5 in the organic material, \code{rsg_code = "HS"}). Strict: no
#' base-saturation fallback.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rsg_code The RSG being named (the resolver passes it).
#' @noRd
qual_eutric <- function(pedon, rsg_code = NULL) {
  .wrb_base_status_result(pedon, "Eutric", "eutric", 20, 100, rsg_code)
}

#' Oligoeutric subqualifier (ol), WRB 2022 Ch 5 (p. 133).
#'
#' v0.9.410: "in soils other than Histosols, having one or more layers
#' consisting of mineral material" between 20 and 100 cm of the mineral soil
#' surface (or a limiting layer starting > 25 cm) "that have exchangeable
#' (Ca+Mg+K+Na) >= exchangeable Al and exchangeable (Ca+Mg+K+Na) < 5 cmolc kg-1
#' clay in the major part of their combined thickness". "Oligoeutric has
#' preference over Hypereutric and Orthoeutric" (p. 133), which the name
#' builder applies (.wrb_optional_subqualifiers). A layer counts when it is on
#' the eutric side (as for Eutric) and its bases x 100 / clay is < 5; a missing
#' clay content or base cation leaves it undecided.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rsg_code The RSG being named (the resolver passes it).
#' @noRd
qual_oligoeutric <- function(pedon, rsg_code = NULL) {
  ref <- paste0("WRB (2022) Ch 5, Oligoeutric (p. 133): exchangeable bases >= ",
                "exchangeable Al and < 5 cmolc per kg of clay in the major part ",
                "of 20-100 cm. Not base saturation (that was WRB 2014).")
  if (identical(rsg_code, "HS"))
    return(DiagnosticResult$new(name = "Oligoeutric", passed = FALSE,
      layers = integer(0), evidence = list(reason = "not in Histosols"),
      missing = character(0), reference = ref))
  f <- .wrb_base_fracs(pedon, 1, rsg_code, lim_min = 25, lim_strict = TRUE)
  if (f$total == 0)
    return(DiagnosticResult$new(name = "Oligoeutric", passed = NA,
      layers = integer(0), evidence = list(reason = "no layers in depth window"),
      missing = "top_cm", reference = ref))
  h <- pedon$horizons
  ok <- vapply(seq_along(f$cand), function(k) {
    i <- f$cand[k]; s <- f$status[[k]]
    b <- c(h$ca_cmol[i], h$mg_cmol[i], h$k_cmol[i], h$na_cmol[i])
    cl <- h$clay_pct[i]
    bk <- if (is.na(cl) || cl <= 0) NA_real_ else sum(b, na.rm = TRUE) * 100 / cl
    low <- if (is.na(bk)) NA else if (bk >= 5) FALSE else if (anyNA(b)) NA else TRUE
    if (isTRUE(s) || identical(s, "mid") || isFALSE(low)) FALSE
    else if (isFALSE(s) && isTRUE(low)) TRUE else NA
  }, NA)
  yes <- sum(f$thk[ok %in% TRUE]) / f$total
  may <- sum(f$thk[!(ok %in% FALSE)]) / f$total
  passed <- if (yes > 0.5) TRUE else if (may <= 0.5) FALSE else NA
  DiagnosticResult$new(name = "Oligoeutric", passed = passed,
    layers = if (isTRUE(passed)) f$cand[ok %in% TRUE] else integer(0),
    evidence = list(oligoeutric_frac = yes, layer_status = ok),
    missing = if (is.na(passed)) c("ca_cmol", "mg_cmol", "k_cmol", "na_cmol",
                                   "clay_pct", "al_cmol") else character(0),
    reference = ref)
}

#' Magnesic qualifier (mg): exchangeable Ca/Mg < 1 in upper 100 cm.
#'
#' v0.9.410: WRB 2022 Ch 5 (p. 140), "having an exchangeable Ca to Mg ratio of
#' < 1 in a layer, >= 30 cm thick and within 100 cm of the mineral soil
#' surface, or in the major part between the mineral soil surface and a
#' limiting layer starting < 60 cm from the mineral soil surface". The layer is
#' made of adjoining horizons that each have the ratio (Ch 2.1, rule 7, p. 23),
#' counted only within the 100 cm. Until v0.9.410 the thickness of every
#' horizon with the ratio and its top within 100 cm was added, whether the
#' horizons touched or not and including their depth below 100 cm. A horizon
#' with Ca or Mg not recorded is undecided; Ca and Mg both 0 is not a ratio
#' < 1.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_magnesic <- function(pedon) {
  h <- pedon$horizons
  ref <- "WRB (2022) Ch 5, Magnesic (p. 140)"
  ms <- .q_mineral_surface_cm(h)
  if (is.na(ms) || !any(!is.na(h$top_cm) & h$top_cm < ms + 100))
    return(DiagnosticResult$new(name = "Magnesic", passed = NA,
            layers = integer(0), evidence = list(),
            missing = c("ca_cmol", "mg_cmol"), reference = ref))
  ca <- h$ca_cmol; mg <- h$mg_cmol
  ratios <- ca / mg
  ok <- ifelse(is.na(ca) | is.na(mg), NA,
               ifelse(ca == 0 & mg == 0, FALSE, ratios < 1))
  to <- min(ms + 100, suppressWarnings(max(h$bottom_cm, na.rm = TRUE)))
  run <- .q_run_tristate(h, ok, 30, from = ms, to = to)
  # Second branch: the major part above a limiting layer starting < 60 cm.
  lim <- tryCatch(h$top_cm[.wrb_limiting_layers(pedon)], error = function(e) numeric(0))
  lim <- lim[!is.na(lim) & lim > ms]
  shallow <- if (length(lim) && min(lim) < ms + 60) {
    l1 <- min(lim)
    thk <- pmax(pmin(h$bottom_cm, l1) - pmax(h$top_cm, ms), 0)
    thk[is.na(thk)] <- 0
    yes <- sum(thk[ok %in% TRUE]) / (l1 - ms)
    may <- sum(thk[!(ok %in% FALSE)]) / (l1 - ms) + 1 - sum(thk) / (l1 - ms)
    if (yes > 0.5) TRUE else if (may <= 0.5) FALSE else NA
  } else FALSE
  passed <- if (isTRUE(run$passed) || isTRUE(shallow)) TRUE
            else if (isFALSE(run$passed) && isFALSE(shallow)) FALSE else NA
  qly <- if (isTRUE(run$passed)) run$layers else which(ok %in% TRUE & h$top_cm < ms + 60)
  DiagnosticResult$new(name = "Magnesic", passed = passed,
    layers = if (isTRUE(passed)) qly else integer(0),
    evidence = list(ca_mg_ratios = ratios, thickness_cm = run$thickness,
                    above_limiting_layer = shallow),
    missing = if (is.na(passed)) c("ca_cmol", "mg_cmol") else character(0),
    reference = ref)
}

#' Sodic qualifier (so): a layer >= 20 cm thick, starting <= 100 cm, with
#' >= 15\% Na plus Mg and >= 6\% Na on the exchange complex; no natric horizon.
#'
#' v0.9.316: WRB 2022 Ch 5 (p. 147), "having a layer, >= 20 cm thick and
#' starting <= 100 cm from the mineral soil surface, that has >= 15% Na plus
#' Mg and >= 6% Na on the exchange complex; and not having a natric horizon
#' starting <= 100 cm from the soil surface". Before v0.9.316 any single
#' horizon was enough (the 12 cm Ah1 of ISRIC's ID046) and a natric horizon
#' did not rule it out. The layer is made of adjoining horizons that each meet
#' the percentages (Ch 2.1, rule 7). Na is the share of the CEC
#' (na_cmol / cec_cmol), else esp_pct; Na plus Mg adds mg_cmol / cec_cmol.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_sodic <- function(pedon) {
  h <- pedon$horizons
  n <- nrow(h)
  ref <- "WRB (2022) Ch 5, Sodic (p. 147)"
  ms <- .q_mineral_surface_cm(h)
  if (is.na(ms))
    return(DiagnosticResult$new(name = "Sodic", passed = NA,
            layers = integer(0), evidence = list(),
            missing = "top_cm", reference = ref))
  cec <- h$cec_cmol; na <- h$na_cmol; mg <- h$mg_cmol
  esp <- h$esp_pct %||% rep(NA_real_, n)
  has_cec <- !is.na(cec) & cec > 0
  na_pct <- ifelse(has_cec & !is.na(na), na / cec * 100, esp)
  mg_pct <- ifelse(has_cec & !is.na(mg), mg / cec * 100, NA_real_)
  # Na plus Mg: decided when both are known, or when Na alone reaches 15%
  namg_ok <- ifelse(!is.na(na_pct) & !is.na(mg_pct), na_pct + mg_pct >= 15,
                    ifelse(!is.na(na_pct) & na_pct >= 15, TRUE, NA))
  na_ok <- ifelse(is.na(na_pct), NA, na_pct >= 6)
  ok <- ifelse(na_ok %in% FALSE | namg_ok %in% FALSE, FALSE,
               ifelse(na_ok %in% TRUE & namg_ok %in% TRUE, TRUE, NA))
  run <- .q_run_tristate(h, ok, 20, from = ms, max_start = ms + 100)
  nat <- tryCatch(natric_horizon(pedon), error = function(e) NULL)
  surface <- suppressWarnings(min(h$top_cm, na.rm = TRUE))
  nat_top <- if (!is.null(nat) && isTRUE(nat$passed) && length(nat$layers))
               suppressWarnings(min(h$top_cm[nat$layers], na.rm = TRUE)) else Inf
  nat_state <- if (is.finite(nat_top) && nat_top - surface <= 100) TRUE
               else if (!is.null(nat) && isTRUE(nat$passed)) FALSE
               else if (is.null(nat) || is.na(nat$passed)) NA else FALSE
  passed <- if (isFALSE(run$passed) || isTRUE(nat_state)) FALSE
            else if (isTRUE(run$passed) && isFALSE(nat_state)) TRUE else NA
  DiagnosticResult$new(name = "Sodic", passed = passed,
    layers = if (isTRUE(passed)) run$layers else integer(0),
    evidence = list(na_pct = na_pct, mg_pct = mg_pct, layer_status = ok,
                    sodic_layer_cm = run$thickness, natric = nat),
    missing = if (is.na(passed))
                c(if (is.na(run$passed)) c("na_cmol", "mg_cmol", "cec_cmol"),
                  if (is.na(nat_state)) nat$missing %||% "natric_horizon")
              else character(0),
    reference = ref)
}


# ---------- COLOUR-BASED PRINCIPAL QUALIFIERS -------------------------------

# v0.9.412: the layer of the Rhodic and Chromic qualifiers, "between 25 and
# 150 cm of the mineral soil surface a layer, >= 30 cm thick" (WRB 2022 Ch 5,
# p. 145 and 130): the horizons that overlap that depth range, each counted
# only for its part inside it, and the layer is one run of adjoining
# horizons. Until v0.9.411 the horizons had to start between 25 and 150 cm
# and their whole thicknesses were added, adjoining or not.
.q_colour_window <- function(pedon) {
  h <- pedon$horizons
  ms <- .q_mineral_surface_cm(h); if (is.na(ms)) ms <- 0
  which(!is.na(h$top_cm) & !is.na(h$bottom_cm) & h$bottom_cm > ms + 25 &
          h$top_cm < ms + 150)
}
# Depths of the range that no horizon describes have no recorded colour, and
# a field observation not recorded counts as absent: they do not count.
.q_colour_run <- function(pedon, layers) {
  h <- pedon$horizons
  ms <- .q_mineral_surface_cm(h); if (is.na(ms)) ms <- 0
  .q_run_tristate(h, seq_len(nrow(h)) %in% layers, 30,
                  from = ms + 25, to = ms + 150)
}

#' Rhodic qualifier (ro): hue redder than 5YR + value < 4 + dry no
#' more than 1 unit higher than moist (in upper subsoil 25-150 cm).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_rhodic <- function(pedon) {
  # WRB 2022 Ch 5: a layer, >= 30 cm thick, between 25 and 150 cm, that shows
  # evidence of soil formation (cambic criterion 3) and has hue redder than
  # 5YR, value < 4 (moist) and a dry value not more than one unit higher than
  # the moist value.
  h <- pedon$horizons
  cand <- .q_colour_window(pedon)
  if (length(cand) == 0L)
    return(DiagnosticResult$new(name = "Rhodic", passed = FALSE,
            layers = integer(0), evidence = list(),
            missing = character(0),
            reference = "WRB (2022) Ch 5, Rhodic"))
  hu <- .munsell_hue_units(h$munsell_hue_moist[cand])  # < 12.5 == redder than 5YR
  vm <- h$munsell_value_moist[cand]
  vd <- h$munsell_value_dry[cand]
  # value-dry clause applied only where dry colour is recorded (refine-present).
  colour_ok <- !is.na(hu) & hu < 12.5 & !is.na(vm) & vm < 4 &
    (is.na(vd) | vd <= vm + 1)
  colour_layers <- cand[colour_ok]
  sf <- test_cambic_soil_formation(h, candidate_layers = colour_layers)
  qual_layers <- intersect(colour_layers, sf$layers)
  run <- .q_colour_run(pedon, qual_layers)
  thickness <- run$thickness; qual_layers <- run$layers
  passed <- isTRUE(run$passed)
  DiagnosticResult$new(name = "Rhodic", passed = passed,
    layers = if (passed) qual_layers else integer(0),
    evidence = list(thickness_cm = thickness),
    missing = if (all(is.na(hu))) "munsell_hue_moist" else character(0),
    reference = "WRB (2022) Ch 5, Rhodic")
}

#' Chromic qualifier (cr): hue redder than 7.5YR + chroma > 4 (in upper
#' subsoil 25-150 cm).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_chromic <- function(pedon) {
  # WRB 2022 Ch 5: a layer, >= 30 cm thick, between 25 and 150 cm, that shows
  # evidence of soil formation (cambic criterion 3) and has hue redder than
  # 7.5YR and chroma > 4 (moist), and does NOT meet the Rhodic qualifier.
  h <- pedon$horizons
  cand <- .q_colour_window(pedon)
  if (length(cand) == 0L)
    return(DiagnosticResult$new(name = "Chromic", passed = FALSE,
            layers = integer(0), evidence = list(),
            missing = character(0),
            reference = "WRB (2022) Ch 5, Chromic"))
  hu  <- .munsell_hue_units(h$munsell_hue_moist[cand])  # < 15 == redder than 7.5YR
  chr <- h$munsell_chroma_moist[cand]
  colour_ok <- !is.na(hu) & hu < 15 & !is.na(chr) & chr > 4
  colour_layers <- cand[colour_ok]
  sf <- test_cambic_soil_formation(h, candidate_layers = colour_layers)
  qual_layers <- intersect(colour_layers, sf$layers)
  run <- .q_colour_run(pedon, qual_layers)
  thickness <- run$thickness; qual_layers <- run$layers
  not_rhodic <- !isTRUE(qual_rhodic(pedon)$passed)
  passed <- isTRUE(run$passed) && not_rhodic
  DiagnosticResult$new(name = "Chromic", passed = passed,
    layers = if (passed) qual_layers else integer(0),
    evidence = list(thickness_cm = thickness, not_rhodic = not_rhodic),
    missing = if (all(is.na(hu))) "munsell_hue_moist" else character(0),
    reference = "WRB (2022) Ch 5, Chromic")
}

#' Xanthic qualifier (xa): ferralic + hue 7.5YR or yellower + value >=
#' 4 + chroma >= 5.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_xanthic <- function(pedon) {
  # WRB 2022 Ch 5: a ferralic horizon with a subhorizon >= 30 cm thick (within
  # 75 cm of the ferralic top) that has hue 7.5YR or yellower, value >= 4 and
  # chroma >= 5 (moist).
  fr <- ferralic(pedon)
  if (!isTRUE(fr$passed))
    return(DiagnosticResult$new(name = "Xanthic", passed = FALSE,
            layers = integer(0), evidence = list(ferralic = fr),
            missing = fr$missing, reference = "WRB (2022) Ch 5, Xanthic"))
  h <- pedon$horizons
  layers <- fr$layers
  hu  <- .munsell_hue_units(h$munsell_hue_moist[layers])  # >= 15 == 7.5YR or yellower
  v   <- h$munsell_value_moist[layers]
  chr <- h$munsell_chroma_moist[layers]
  ok <- !is.na(hu) & hu >= 15 & !is.na(v) & v >= 4 & !is.na(chr) & chr >= 5
  qual_layers <- layers[ok]
  thickness <- if (length(qual_layers) > 0L)
    sum(h$bottom_cm[qual_layers] - h$top_cm[qual_layers], na.rm = TRUE) else 0
  passed <- thickness >= 30
  DiagnosticResult$new(name = "Xanthic", passed = passed,
    layers = if (passed) qual_layers else integer(0),
    evidence = list(ferralic = fr, thickness_cm = thickness),
    missing = fr$missing, reference = "WRB (2022) Ch 5, Xanthic")
}


# ---------- TEXTURE / SKELETIC QUALIFIERS ----------------------------------

#' Arenic qualifier (ar): texture sand or loamy sand >= 30 cm in <= 100 cm.
#'
#' v0.9.217: WRB 2022 Ch 5, "consisting of mineral material and having, single
#' or in combination, a texture class of sand or loamy sand in one or more
#' layers with a combined thickness of >= 30 cm, occurring within 100 cm of the
#' mineral soil surface, or in the major part between the mineral soil surface
#' and a limiting layer starting > 10 and < 60 cm from the mineral soil
#' surface". The v0.9 wrapper used arenic_texture(), the Arenosol test (coarse
#' texture throughout the upper 100 cm), which misses a 30 cm sandy layer over
#' finer material.
#' v0.9.315: read as Loamic and Siltic are (.q_texture_qualifier(), p. 128):
#' the classes S and LS of the triangle (Annex 1, Table 8.39, p. 182), the
#' depths counted from the mineral soil surface (from 0 cm before), and the
#' limiting layer counted from it too.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_arenic <- function(pedon)
  .q_texture_qualifier(pedon, "Arenic", c("S", "LS"), 128)

#' Clayic qualifier (ce), WRB 2022 Ch 5.
#'
#' A texture class of \strong{clay, sandy clay or silty clay} in one or more
#' layers with a combined thickness of >= 30 cm within 100 cm of the mineral
#' soil surface (v0.9.130: not the v0.9 proxy of clay >= 60\%).
#' v0.9.315: read as Loamic and Siltic are (.q_texture_qualifier(), p. 130):
#' the classes C, SC and SIC of Annex 1, Table 8.39 (p. 182), so sandy clay is
#' clay >= 35\% with sand > 45\% (>= 45\% before); the depths counted from
#' the mineral soil surface (from 0 cm before); the second alternative of the
#' text, "in the major part between the mineral soil surface and a limiting
#' layer starting > 10 and < 60 cm", which was not read; organic layers do not
#' count; undecided where the texture is not recorded (it was FALSE).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_clayic <- function(pedon)
  .q_texture_qualifier(pedon, "Clayic", c("C", "SC", "SIC"), 130)

#' Loamic qualifier (lo): loam, sandy loam, clay loam, sandy clay loam or
#' silty clay loam, >= 30 cm combined within 100 cm.
#'
#' v0.9.316: WRB 2022 Ch 5 (p. 140) names the texture classes of the triangle
#' (Annex 1, Table 8.39, p. 182), "single or in combination ... in one or more
#' layers with a combined thickness of >= 30 cm, occurring within 100 cm of the
#' mineral soil surface, or in the major part between the mineral soil surface
#' and a limiting layer starting > 10 and < 60 cm". Before v0.9.316 it was clay
#' 8 to < 40% with >= 15% silt, which took silt loam (ISRIC's CN012) and sandy
#' clay for Loamic and left out sandy loam with < 8% clay; the depth was
#' counted from 0 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_loamic <- function(pedon)
  .q_texture_qualifier(pedon, "Loamic", c("L", "SL", "CL", "SCL", "SICL"), 140)

#' Siltic qualifier (sl): silt or silt loam, >= 30 cm combined within 100 cm.
#'
#' v0.9.316: WRB 2022 Ch 5 (p. 146), the texture classes silt and silt loam of
#' the triangle (Annex 1, Table 8.39, p. 182), with the thickness and depth
#' rules of Loamic. Before v0.9.316 it was clay < 35% with >= 50% silt, which
#' took silty clay loam (clay 27 to < 35%) for Siltic.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_siltic <- function(pedon)
  .q_texture_qualifier(pedon, "Siltic", c("SI", "SIL"), 146)

#' Skeletic qualifier (sk): coarse fragments >= 40\% averaged over 100 cm.
#'
#' v0.9.316: WRB 2022 Ch 5 (p. 146), "having >= 40% (by volume, related to the
#' whole soil) coarse fragments averaged over a depth of 100 cm from the
#' mineral soil surface or to a limiting layer, whichever is shallower". Before
#' v0.9.316 one layer with >= 40% was enough (a 4 cm layer of ISRIC's CO013, a
#' 2B of 85-105 cm in KE018). The average is weighted by thickness. A part of
#' the depth with no coarse fragments recorded (or not described) may hold
#' anything from 0 to 100%: TRUE when the recorded part already brings the
#' average to 40%, FALSE when the average stays below 40% even at 100%,
#' otherwise NA.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_skeletic <- function(pedon) {
  h <- pedon$horizons
  ref <- "WRB (2022) Ch 5, Skeletic (p. 146)"
  ms <- .q_mineral_surface_cm(h)
  if (is.na(ms))
    return(DiagnosticResult$new(name = "Skeletic", passed = NA,
            layers = integer(0), evidence = list(),
            missing = "coarse_fragments_pct", reference = ref))
  lim <- tryCatch(.barrier_top_cm(pedon), error = function(e) NA_real_)
  bot <- if (!is.na(lim) && lim < ms + 100) lim else ms + 100
  if (bot <= ms)
    return(DiagnosticResult$new(name = "Skeletic", passed = FALSE,
            layers = integer(0),
            evidence = list(reason = "limiting layer at the mineral soil surface",
                            limiting_layer_cm = lim),
            missing = character(0), reference = ref))
  cf <- h$coarse_fragments_pct %||% rep(NA_real_, nrow(h))
  ov <- pmax(0, pmin(h$bottom_cm, bot) - pmax(h$top_cm, ms))
  ov[is.na(ov)] <- 0
  # depths of the window covered by no described horizon count as unknown
  depth <- bot - ms
  known <- sum(ov[!is.na(cf)])
  unknown <- max(0, depth - known)
  lo <- sum((cf * ov)[!is.na(cf)]) / depth
  hi <- lo + 100 * unknown / depth
  passed <- if (lo >= 40) TRUE else if (hi < 40) FALSE else NA
  DiagnosticResult$new(name = "Skeletic", passed = passed,
    layers = if (isTRUE(passed)) which(ov > 0 & !is.na(cf) & cf >= 40) else integer(0),
    evidence = list(window_cm = c(ms, bot), mean_lower_bound_pct = lo,
                    mean_upper_bound_pct = hi, limiting_layer_cm = lim),
    missing = if (is.na(passed)) "coarse_fragments_pct" else character(0),
    reference = ref)
}


# ---------- ORGANIC / HUMUS QUALIFIERS --------------------------------------

#' Humic qualifier (hu): >= 1\% SOC as a weighted average to 50 cm.
#'
#' v0.9.309: WRB 2022 Ch 5 (p. 136): "having >= 1% soil organic carbon as a
#' weighted average to a depth of 50 cm from the mineral soil surface (if a
#' limiting layer starts within the specified depth, the depth range below
#' that contributes a 0 to the calculation of the weighted average)". Until
#' v0.9.308 every layer starting above 50 cm was counted whole and the depth
#' was taken from 0 cm: on ISRIC's KE009 that gave 0.83% instead of 1.15%.
#' A part of the 50 cm with no carbon recorded leaves it undecided unless the
#' recorded part alone reaches 1%, or (v0.9.411) could not reach it with every
#' unrecorded part at the 20% SOC that mineral material stays below (Ch 3.3.11,
#' p. 90; an unrecorded O or H layer has no such bound).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_humic <- function(pedon) {
  h <- pedon$horizons
  ms <- .q_mineral_surface_cm(h)
  if (is.na(ms))
    return(DiagnosticResult$new(name = "Humic", passed = NA,
            layers = integer(0), evidence = list(),
            missing = "oc_pct", reference = "WRB (2022) Ch 5, Humic"))
  lim <- tryCatch(.barrier_top_cm(pedon), error = function(e) NA_real_)
  end <- ms + 50
  if (!is.na(lim) && lim < end) end <- max(lim, ms)
  ov <- pmax(0, pmin(h$bottom_cm, end) - pmax(h$top_cm, ms))
  ov[is.na(ov)] <- 0
  oc <- h$oc_pct
  known <- sum((oc * ov)[!is.na(oc) & ov > 0])
  unknown_cm <- sum(ov[is.na(oc) & ov > 0]) + max(0, (end - ms) - sum(ov))
  wmean <- known / 50                    # the range below a limiting layer counts 0
  # v0.9.411: the most the unrecorded part could add (PK004: 0.09% known)
  org <- !is.na(h$designation) & grepl("^[0-9]*[OH]", h$designation)
  bound <- 20 * unknown_cm + if (any(org & is.na(oc) & ov > 0)) Inf else 0
  passed <- if (wmean >= 1) TRUE
            else if (unknown_cm > 1e-6 && known + bound >= 50) NA else FALSE
  layers <- which(ov > 0)
  DiagnosticResult$new(name = "Humic", passed = passed,
    layers = if (isTRUE(passed)) layers else integer(0),
    evidence = list(oc_weighted = wmean, mineral_surface_cm = ms,
                    window_bottom_cm = end, limiting_layer_top_cm = lim,
                    unrecorded_cm = unknown_cm, unrecorded_bound = bound),
    missing = if (is.na(passed)) "oc_pct" else character(0),
    reference = "WRB (2022) Ch 5, Humic")
}

#' Ochric qualifier (oh): >= 0.2\% SOC in the upper 10 cm of the mineral
#' soil, no mollic or umbric horizon, not Humic (WRB 2022 Ch 5, p. 142).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_ochric <- function(pedon) {
  ref <- "WRB (2022) Ch 5, Ochric"
  has_mollic <- isTRUE(mollic(pedon)$passed)
  has_umbric <- isTRUE(umbric_horizon(pedon)$passed)
  if (has_mollic || has_umbric)
    return(DiagnosticResult$new(name = "Ochric", passed = FALSE,
            layers = integer(0),
            evidence = list(mollic = has_mollic, umbric = has_umbric),
            missing = character(0), reference = ref))
  # v0.9.411: WRB 2022 Ch 5 (p. 142): "having >= 0.2% soil organic carbon
  # (weighted average) in the upper 10 cm of the mineral soil". Until v0.9.410
  # any layer starting <= 10 cm from the soil surface with 0.2% passed, and a
  # profile with no carbon recorded failed. A part with no carbon recorded is
  # undecided unless the recorded part reaches 0.2%, or could not reach it
  # with the unrecorded part at < 20% SOC (mineral material).
  h <- pedon$horizons
  ms <- .q_mineral_surface_cm(h)
  if (is.na(ms))
    return(DiagnosticResult$new(name = "Ochric", passed = FALSE,
            layers = integer(0), evidence = list(), missing = character(0),
            reference = ref))
  end <- min(ms + 10, suppressWarnings(max(h$bottom_cm, na.rm = TRUE)))
  ov <- pmax(0, pmin(h$bottom_cm, end) - pmax(h$top_cm, ms))
  ov[is.na(ov)] <- 0
  oc <- h$oc_pct
  surface <- which(ov > 0)
  known <- sum((oc * ov)[!is.na(oc) & ov > 0])
  unknown_cm <- sum(ov[is.na(oc) & ov > 0]) + max(0, (end - ms) - sum(ov))
  width <- max(end - ms, 1e-6)
  passed <- if (known / width >= 0.2) TRUE
            else if (unknown_cm > 1e-6 && (known + 20 * unknown_cm) / width >= 0.2) NA
            else FALSE
  # v0.9.309: WRB 2022 Ch 5 (p. 142): "and not meeting the set of diagnostic
  # criteria of the Humic qualifier"
  hu <- qual_humic(pedon)$passed
  if (isTRUE(hu)) passed <- FALSE
  else if (is.na(hu) && !isFALSE(passed)) passed <- NA
  DiagnosticResult$new(name = "Ochric", passed = passed,
    layers = if (isTRUE(passed)) surface else integer(0),
    evidence = list(oc_weighted = if (unknown_cm > 1e-6) NA_real_ else known / width,
                    mineral_surface_cm = ms),
    missing = if (is.na(passed)) "oc_pct" else character(0),
    reference = ref)
}


# ---------- GENERIC CATCH-ALL ----------------------------------------------

#' Haplic qualifier (ha): no other principal qualifier of the RSG
#' applies. Always passes; the qualifier resolution machinery uses it
#' as the default when no other qualifier matched.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_haplic <- function(pedon) {
  DiagnosticResult$new(name = "Haplic", passed = TRUE,
    layers = seq_len(nrow(pedon$horizons)),
    evidence = list(catch_all = TRUE),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Haplic (default)")
}


# ---------- v0.9 RESOLUTION ENGINE ------------------------------------------

# v0.9.2 family-suppression table.
# When several qualifiers from the same family pass for the same RSG,
# WRB convention is to print only the most-specific one. Each entry is
# ordered from most-specific (kept) to least-specific (suppressed).
# (Internal data; not exported. The full @export-tagged
# documentation for the resolve_wrb_qualifiers() function lives on
# the function definition itself, further down this file.)
.wrb_qualifier_families <- list(
  salinity   = c("Hypersalic",  "Salic"),
  calcic     = c("Hypercalcic", "Calcic",       "Protocalcic"),
  gypsic     = c("Hypergypsic", "Gypsic",       "Protogypsic"),
  vertic     = c("Vertic",      "Protovertic"),
  eutric     = c("Hypereutric", "Eutric"),
  # v0.9.411: optional subqualifiers of Humic (WRB 2022 Ch 5, p. 137; Ch 2.3
  # rule 1, p. 26), each used instead of Humic and not of each other
  hyperhumic = c("Hyperhumic", "Humic"),
  profundihumic = c("Profundihumic", "Humic"),
  dystric    = c("Hyperdystric", "Dystric"),
  alic       = c("Hyperalic",   "Alic"),
  geric      = c("Hypergeric",  "Geric")       # v0.9.412, WRB 2022 Ch 5 p. 135
)

# v0.9.410 (Hugo, 2026-11-19): optional subqualifiers. WRB 2022 Ch 2.3, rule
# 1 (p. 26): "If a subqualifier applies that fulfils all the criteria of the
# qualifier, the subqualifier can - but does not have to - be used instead of
# its qualifier"; "Optional and additional subqualifiers are recommended
# especially for naming soils". Subqualifiers with a given definition (Ch
# 2.3.2, p. 29) "are not listed with the RSGs in Chapter 4", so soilKey
# uses them in every RSG, wherever the qualifier itself is named.
#
# The table: a qualifier the name builder has taken -> the subqualifiers that
# may replace it, in order of preference; the first that applies replaces it
# in the same position (resolve_wrb_qualifiers(), .wrb_use_optional_subqualifiers()).
# When one is undecided, the later ones are not tried and the qualifier stays:
# which subqualifier comes first is then not known (as for the texture group,
# v0.9.315), while the qualifier itself holds.
# Each subqualifier needs its own qual_<name>() function. An entry may also
# carry a precedence that Ch 5 states: "Oligoeutric has preference over
# Hypereutric and Orthoeutric" (p. 133), so Hypereutric, where Ch 4 lists it
# (Luvisols, Lixisols, Vertisols), gives way to Oligoeutric. Other families
# are added with one entry each, e.g. Geric = "Hypergeric",
# Humic = c("Hyperhumic", "Profondihumic").
.wrb_optional_subqualifiers <- list(
  Dystric     = c("Hyperdystric"),
  Eutric      = c("Oligoeutric", "Hypereutric"),
  Hypereutric = c("Oligoeutric"),
  # v0.9.411: the subqualifiers of Humic (Ch 5, p. 137). WRB states no
  # preference between them; the stricter one (>= 5% to 50 cm) is tried first.
  Humic       = c("Hyperhumic", "Profundihumic")
)

# Replace each taken qualifier that has optional subqualifiers by the first
# one that applies; each one tested is added to the trace with a note.
.wrb_use_optional_subqualifiers <- function(pedon, w, rsg_code = NULL) {
  for (k in seq_along(w$matched)) {
    q <- w$matched[k]
    for (sq in .wrb_optional_subqualifiers[[q]] %||% character(0)) {
      if (sq %in% w$matched) break
      ev <- .evaluate_qualifier(pedon, sq, rsg_code)
      w$trace[[sq]] <- c(ev$trace_entry,
        list(note = sprintf("optional subqualifier of %s (WRB 2022 Ch 2.3, p. 26)", q)))
      if (isTRUE(ev$passed)) {
        w$matched[k] <- sq
        w$layers[[sq]] <- ev$layers %||% w$layers[[q]]
        w$layers[[q]] <- NULL
        break
      }
      if (is.na(ev$passed)) {
        w$trace[[sq]]$note <- paste0(w$trace[[sq]]$note,
          "; undecided, so the later ones are not tried and ", q, " stays")
        break
      }
    }
  }
  w
}

# v0.9.217 (WRB 2022 Ch 2.2): "Qualifiers conveying redundant information are
# not added. This is a general rule and applies even if the slash is not used.
# For example, Eutric is not added if the Calcaric qualifier applies." Each
# entry: the qualifier dropped -> the qualifiers that make it redundant
# (Dolomitic, like Calcaric, means carbonates and so a saturated complex).
.wrb_redundant_with <- list(
  Eutric = c("Calcaric", "Dolomitic"),
  # v0.9.411: Profundihumic (>= 1% throughout to 100 cm, p. 137) and
  # Hyperhumic carry Humic, also from the other list (Ferralsols, Nitisols)
  Humic  = c("Profundihumic", "Hyperhumic")
)
# Dropped by name only: their subqualifiers are what makes them redundant.
.wrb_redundant_exact <- c("Humic")

# A redundant qualifier is dropped with its subqualifiers (Hypereutric,
# Epieutric, Endoeutric are Eutric too).
.drop_redundant_qualifiers <- function(names_, all_matched) {
  drop <- names(Filter(function(by) any(by %in% all_matched),
                       .wrb_redundant_with))
  names_ <- names_[!names_ %in% intersect(drop, .wrb_redundant_exact)]
  drop <- setdiff(drop, .wrb_redundant_exact)
  if (!length(drop)) return(names_)
  pat <- paste0("(", paste(tolower(drop), collapse = "|"), ")$")
  names_[!(names_ %in% drop | grepl(pat, tolower(names_)))]
}

# Texture qualifiers, which open the supplementary list (WRB 2022 Ch 2.2).
.WRB_TEXTURE_QUALIFIERS <- c("Arenic", "Clayic", "Loamic", "Siltic")

# The qualifier a (sub)qualifier belongs to, for the alphabetical order of the
# supplementary qualifiers: "follow the alphabetical order of the qualifier,
# not the subqualifier" (Ch 2.3). A prefix is stripped only when what remains
# is itself a qualifier, so Epic, Endic or Protic keep their own name.
.WRB_SUBQUALIFIER_PREFIXES <- c("Amphi", "Ano", "Bathy", "Endo", "Epi", "Kato",
                                "Panto", "Poly", "Supra", "Thapto", "Hyper",
                                "Hypo", "Proto", "Ortho", "Oligo", "Profundi",
                                "Akro", "Nudi")
.wrb_base_qualifier <- function(qname, known) {
  for (p in .WRB_SUBQUALIFIER_PREFIXES) {
    if (startsWith(qname, p) && nchar(qname) > nchar(p)) {
      rest <- substring(qname, nchar(p) + 1L)
      rest <- paste0(toupper(substring(rest, 1, 1)), substring(rest, 2))
      if (rest %in% known) return(rest)
    }
  }
  qname
}

# Drop suppressed siblings within each family while preserving the
# original YAML order of the surviving names.
.suppress_qualifier_siblings <- function(matched) {
  if (length(matched) <= 1L) return(matched)
  drop <- character(0)
  for (family in .wrb_qualifier_families) {
    in_match <- intersect(family, matched)
    if (length(in_match) > 1L) {
      keeper <- in_match[which.min(match(in_match, family))]
      drop <- c(drop, setdiff(in_match, keeper))
    }
  }
  setdiff(matched, drop)
}

# Internal: evaluate a single qualifier name against a pedon. Returns
# a list(passed, layers, trace_entry). Used for both principal and
# supplementary slots.
.evaluate_qualifier <- function(pedon, qname, rsg_code = NULL) {
  # A qualifier with its own function is evaluated by it. Only otherwise is the
  # name read as specifier + qualifier: until v0.9.217 the specifier came
  # first, so Epic (ep) was read as Epi- + "c" and never evaluated.
  own <- exists(paste0("qual_", tolower(qname)), envir = asNamespace("soilKey"),
                inherits = FALSE)
  spec <- if (own) NULL else .detect_specifier(qname)
  if (!is.null(spec)) {
    res <- tryCatch(
      .apply_specifier(pedon, spec$prefix, spec$base, spec$spec),
      error = function(e) NULL)
    if (is.null(res)) {
      return(list(passed = NA,
                  trace_entry = list(passed = NA,
                                     note = "specifier dispatch threw error")))
    }
    return(list(
      passed = res$passed,
      layers = res$layers %||% integer(0),
      trace_entry = list(passed = res$passed,
                         missing = res$missing %||% character(0),
                         specifier = spec$prefix,
                         base = spec$base)
    ))
  }
  fn_name <- paste0("qual_", tolower(qname))
  fn <- tryCatch(get(fn_name, envir = asNamespace("soilKey")),
                   error = function(e) NULL)
  if (is.null(fn)) {
    return(list(passed = NA,
                trace_entry = list(passed = NA,
                                   note = "function not implemented in v0.9")))
  }
  # Qualifiers defined by the RSG's own horizon (Epic, Endic, Dorsic) take the
  # RSG being named.
  res <- tryCatch(
    if ("rsg_code" %in% names(formals(fn))) fn(pedon, rsg_code = rsg_code)
    else fn(pedon),
    error = function(e) NULL)
  if (is.null(res)) {
    return(list(passed = NA,
                trace_entry = list(passed = NA,
                                   note = "diagnostic threw error")))
  }
  list(passed = res$passed,
       layers = res$layers %||% integer(0),
       trace_entry = list(passed = res$passed,
                          missing = res$missing %||% character(0)))
}

# The qualifier lists of WRB 2022 (inst/rules/wrb2022/qualifiers.yaml), read
# once per session (v0.9.400; R/memo.R); NULL when the file is not found.
.wrb_qualifier_rules <- function() {
  cached <- .rules_cache[["wrb2022\rqualifiers"]]
  if (!is.null(cached)) return(cached)
  qfile <- system.file("rules/wrb2022/qualifiers.yaml",
                          package = "soilKey")
  if (!nzchar(qfile)) qfile <- "inst/rules/wrb2022/qualifiers.yaml"
  if (!file.exists(qfile)) return(NULL)
  qrules <- yaml::read_yaml(qfile)
  assign("wrb2022\rqualifiers", qrules, envir = .rules_cache)
  qrules
}

#' Resolve WRB 2022 qualifiers for a Reference Soil Group
#'
#' Walks the RSG's lists of principal and supplementary qualifiers (WRB 2022
#' Chapter 4, in \code{inst/rules/wrb2022/qualifiers.yaml}) and tests each
#' qualifier against the pedon, following the rules for naming soils of
#' Chapter 2.2: in a slash group (\code{"Rhodic/Xanthic"}) only the first
#' qualifier that applies is used ("If no specifier is used, only the first
#' applicable qualifier can be used", Ch 2.1, p. 24-25); with
#' \code{specifiers = TRUE} the texture qualifiers of the group
#' (\code{"Arenic/Clayic/Loamic/Siltic"}) that apply at different depths are
#' all used, each with its specifier of Ch 2.3.1 rule 2 (p. 27), from the top
#' to the bottom of the profile ("Anoloamic, Endoclayic", p. 26; since
#' v0.9.315); a qualifier made redundant by another is
#' left out (Eutric when Calcaric or Dolomitic applies); Haplic applies only
#' where the RSG lists it and no other principal qualifier does.
#'
#' Both vectors come back in the order of the name. Principal qualifiers are
#' written right to left, "the uppermost qualifier in the list is placed
#' closest to the name of the RSG", so \code{principal} reads the matched list
#' backwards. Supplementary qualifiers open with the texture qualifiers (top to
#' bottom of the profile when specifiers make several apply), then follow the
#' alphabetical order of the qualifier, not of the subqualifier. Until
#' v0.9.217 both kept the order of the old lists.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rsg_code Two-letter RSG code (e.g. \code{"FR"} for Ferralsols).
#' @param rules Optional pre-loaded rules list (saves I/O when many
#'        RSGs are tested).
#' @param specifiers If \code{TRUE}, auto-attach WRB Ch 5 depth specifiers
#'        (Epi-/Endo-/Bathy-/Amphi-/Panto-/Kato-) to depth-anchored
#'        qualifiers based on the feature's actual depth. Default
#'        \code{FALSE} leaves names byte-identical to earlier versions.
#' @return A list with \code{principal} and \code{supplementary} (character
#'         vectors, in the order they are written in the name), \code{trace}
#'         and \code{trace_supplementary}.
#' @export
resolve_wrb_qualifiers <- function(pedon, rsg_code, rules = NULL,
                                   specifiers = FALSE) {
  rules <- rules %||% load_rules("wrb2022")
  # v0.9.400: one memo scope for the qualifiers of one call (R/memo.R)
  if (.memo_open()) on.exit(.memo_close(), add = TRUE)
  qrules <- .wrb_qualifier_rules()
  if (is.null(qrules)) {
    return(list(principal = character(0), supplementary = character(0),
                  trace = list(), note = "qualifiers.yaml not found"))
  }
  per_rsg <- qrules$rsg_qualifiers[[rsg_code]]
  if (is.null(per_rsg)) {
    return(list(principal = character(0), supplementary = character(0),
                  trace = list(),
                  note = sprintf("No qualifiers defined for RSG %s",
                                  rsg_code)))
  }

  # One list entry is one qualifier or a slash group of alternatives
  # ("Rhodic/Xanthic"): mutually exclusive, or the later ones redundant, so
  # only the first that applies is used (WRB 2022 Ch 2.2). Every alternative
  # is still evaluated, so the trace shows each one's result.
  # v0.9.315: with specifiers, the texture qualifiers of one group that apply
  # at different depths are all used, each with its specifier (Ch 2.1,
  # p. 24-25: "they can be used both, each one with the respective specifier
  # ... If no specifier is used, only the first applicable qualifier can be
  # used"; .wrb_texture_group_specified()).
  walk <- function(entries) {
    trace <- list(); matched <- character(0); layers <- list()
    for (entry in entries) {
      taken <- FALSE
      alts <- strsplit(entry, "/", fixed = TRUE)[[1]]
      passed_alts <- character(0); alt_layers <- list()
      # v0.9.315 (Hugo, 2026-10-30): in the texture group, without specifiers,
      # "only the first applicable qualifier can be used" (Ch 2.1, p. 25); when
      # an earlier member is undecided, which one is first is not known, so the
      # group gives no qualifier. Other groups keep the first that passes (the
      # later member of a redundant pair, such as Eutric after Hypereutric, is
      # right whatever the earlier one is).
      texture_group <- length(alts) > 1L && all(alts %in% .WRB_TEXTURE_QUALIFIERS)
      undecided_before <- FALSE
      for (qname in alts) {
        if (identical(qname, "Haplic")) next
        ev <- .evaluate_qualifier(pedon, qname, rsg_code)
        trace[[qname]] <- ev$trace_entry
        if (isTRUE(ev$passed)) {
          passed_alts <- c(passed_alts, qname)
          alt_layers[[qname]] <- ev$layers %||% integer(0)
        }
        if (!taken && is.na(ev$passed)) undecided_before <- TRUE
        if (!taken && isTRUE(ev$passed) && texture_group && undecided_before &&
            !isTRUE(specifiers)) {
          taken <- TRUE
          trace[[qname]]$note <- "not named: an earlier member of its texture group is undecided"
        }
        if (!taken && isTRUE(ev$passed)) {
          matched <- c(matched, qname)
          layers[[qname]] <- ev$layers %||% integer(0)
          taken <- TRUE
        }
      }
      if (isTRUE(specifiers) && length(passed_alts) > 1L &&
          (all(alts %in% .WRB_TEXTURE_QUALIFIERS) ||
           all(alts %in% .WRB_ANDIC_GROUP))) {
        # v0.9.411: Aluandic and Silandic at different depths as well
        sp <- if (all(alts %in% .WRB_ANDIC_GROUP))
                .wrb_andic_group_specified(pedon, passed_alts, alt_layers)
              else .wrb_texture_group_specified(pedon, passed_alts, alt_layers)
        if (!is.null(sp)) {
          first <- passed_alts[1L]
          # in the place of the first (a principal group keeps its rank)
          pos <- match(first, matched)
          matched <- append(matched[-pos], unname(sp), after = pos - 1L)
          layers[[first]] <- NULL
          for (q in passed_alts) {
            layers[[sp[[q]]]] <- alt_layers[[q]]
            trace[[q]]$name_with_specifier <- sp[[q]]
          }
        }
      }
    }
    list(trace = trace, matched = matched, layers = layers)
  }
  wp <- walk(per_rsg$principal %||% character(0))
  ws <- walk(per_rsg$supplementary %||% character(0))
  wp <- .wrb_use_optional_subqualifiers(pedon, wp, rsg_code)
  ws <- .wrb_use_optional_subqualifiers(pedon, ws, rsg_code)
  trace_principal     <- wp$trace
  trace_supplementary <- ws$trace
  layers_principal     <- wp$layers
  layers_supplementary <- ws$layers

  matched_principal     <- .suppress_qualifier_siblings(wp$matched)
  matched_supplementary <- .suppress_qualifier_siblings(ws$matched)
  all_matched <- c(matched_principal, matched_supplementary)
  matched_principal     <- .drop_redundant_qualifiers(matched_principal, all_matched)
  matched_supplementary <- .drop_redundant_qualifiers(matched_supplementary, all_matched)

  # Haplic only where Chapter 4 lists it: "no other principal qualifier of the
  # respective RSG applies". 16 RSGs do not list it.
  if (length(matched_principal) == 0L && "Haplic" %in% (per_rsg$principal %||% character(0)))
    matched_principal <- "Haplic"

  # Order of the name (WRB 2022 Ch 2.2). Principal qualifiers are written right
  # to left: "the uppermost qualifier in the list is placed closest to the name
  # of the RSG", so the name reads the matched list backwards. Supplementary
  # qualifiers: texture first (top to bottom of the profile when several apply
  # with specifiers), then the rest in alphabetical order of the qualifier, not
  # of the subqualifier.
  matched_principal <- rev(matched_principal)
  if (length(matched_supplementary) > 1L) {
    known <- unique(unlist(strsplit(unlist(qrules$rsg_qualifiers), "/", fixed = TRUE)))
    base  <- vapply(matched_supplementary, .wrb_base_qualifier, character(1),
                    known = known)
    is_tex <- base %in% .WRB_TEXTURE_QUALIFIERS
    top <- vapply(matched_supplementary, function(q) {
      ly <- layers_supplementary[[q]]
      if (length(ly)) min(pedon$horizons$top_cm[ly], na.rm = TRUE) else Inf
    }, numeric(1))
    ord <- order(!is_tex, ifelse(is_tex, top, 0), tolower(base))
    matched_supplementary <- matched_supplementary[ord]
  }

  # v0.9.105: opt-in auto-attachment of WRB Ch 5 depth specifiers. Applied
  # AFTER ordering (the alphabetical order follows the qualifier, not the
  # subqualifier) so the specifier is just a prefix on the surviving name.
  # specifiers = FALSE leaves the names without specifiers.
  if (isTRUE(specifiers)) {
    matched_principal <-
      .apply_depth_specifiers(pedon, matched_principal, layers_principal)
    matched_supplementary <-
      .apply_depth_specifiers(pedon, matched_supplementary, layers_supplementary)
  }

  list(principal     = unname(matched_principal),
       supplementary = unname(matched_supplementary),
       trace         = trace_principal,
       trace_supplementary = trace_supplementary)
}


#' Format a WRB 2022 soil name with qualifiers
#'
#' @param rsg_name Full RSG name (e.g. "Ferralsols").
#' @param principal Character vector of principal-qualifier names, in the
#'        order they are written (as \code{\link{resolve_wrb_qualifiers}}
#'        returns them).
#' @param supplementary Character vector of supplementary-qualifier
#'        names, in the order they are written.
#' @return The name as WRB 2022 Chapter 2.2 writes it, e.g. "Geric Rhodic
#'         Ferralsol (Clayic, Eutric, Ferric, Humic)".
#' @export
format_wrb_name <- function(rsg_name, principal = character(0),
                              supplementary = character(0)) {
  rsg_singular <- sub("s$", "", rsg_name)
  prefix <- if (length(principal) > 0L)
              paste(principal, collapse = " ") else ""
  base <- paste(prefix, rsg_singular)
  base <- trimws(base)
  if (length(supplementary) > 0L) {
    suffix <- paste0(" (", paste(supplementary, collapse = ", "), ")")
    base <- paste0(base, suffix)
  }
  base
}
