# =============================================================
# USDA Soil Taxonomy 13th ed. -- subgroup tests of the Inceptisols
# (Ch. 11, pp. 207-246) and the Entisols (Ch. 8, pp. 165-188)
# =============================================================
#
# v0.9.425 (wave 3 of the Soil Taxonomy audit): the subgroup lists of
# inst/rules/usda/subgroups/inceptisols.yaml and entisols.yaml are the KST
# 13th edition lists, in KST order, and these are the tests they call that
# no existing helper reads as the KST prints them. Every test is
# three-valued: TRUE or FALSE when the recorded data decide it, NA (with the
# missing attributes) when missing data could change the answer. A field
# observation not recorded is absent (redox features, cracks, slickensides,
# artifacts, fragic and lamellar designations, cementation, worm holes,
# fluidity, reading D4 of the audit); a measurement not recorded is
# undecided (colour, base saturation, CEC, CaCO3, linear extensibility, ESP,
# saturation days).
#
# The functions named sg_incept_* are read by both lists (the criteria are
# printed in the same words in both chapters); sg_ent_* are the Entisols'
# own. None changes the behaviour of an existing helper.
# =============================================================


# ---- small shared pieces ----------------------------------------------------

.sg_ie_ref <- function(pages) paste0("Soil Survey Staff (2022), KST 13th ed., ", pages)

.sg_ie_res <- function(name, passed, layers = integer(0), evidence = list(),
                       missing = character(0), ref = .sg_ie_ref("Ch. 8 and 11"),
                       notes = NA_character_) {
  DiagnosticResult$new(name = name, passed = passed,
                       layers = if (isTRUE(passed)) as.integer(layers) else integer(0),
                       evidence = evidence,
                       missing = if (is.na(passed)) unique(as.character(missing)) else character(0),
                       reference = ref, notes = notes)
}

# Mineral soil surface (cm), 0 when unknown.
.sg_ie_ms <- function(h) {
  ms <- .mineral_soil_surface_cm(h)
  if (is.na(ms)) 0 else ms
}

# Layers overlapping the depth interval (lo, hi), in depth order.
.sg_ie_window <- function(h, lo, hi) {
  k <- which(!is.na(h$top_cm) & !is.na(h$bottom_cm) & h$bottom_cm > lo & h$top_cm < hi)
  k[order(h$top_cm[k])]
}

# Thickness of each layer inside (lo, hi).
.sg_ie_overlap <- function(h, k, lo, hi) pmax(pmin(h$bottom_cm[k], hi) - pmax(h$top_cm[k], lo), 0)

# Top of the shallowest densic, lithic or paralithic contact, Inf when none.
.sg_ie_contact <- function(h) {
  rl <- .usda_contact_top_cm(h)
  if (is.na(rl)) Inf else rl
}

# Bottom of the Ap horizon(s), NA when there is none.
.sg_ie_ap_bottom <- function(h) {
  d <- .usda_desig(h$designation)
  ap <- !is.na(d$master) & d$master == "A" & grepl("p", d$suffix)
  if (any(ap)) max(h$bottom_cm[ap], na.rm = TRUE) else NA_real_
}

# Share of a set of layers (weights w) in which flag is TRUE: TRUE when the
# TRUE part reaches `share` of the total, FALSE when the FALSE part exceeds
# 1 - share, NA otherwise.
.sg_ie_share3 <- function(flag, w, share = 0.5) {
  tot <- sum(w)
  if (tot <= 0) return(NA)
  yes <- sum(w[flag %in% TRUE]); no <- sum(w[flag %in% FALSE])
  if (yes >= share * tot) TRUE else if (no > (1 - share) * tot) FALSE else NA
}

# Thickness rule: TRUE when the TRUE layers reach min_cm, FALSE when the TRUE
# and undecided layers together cannot, NA otherwise.
.sg_ie_thick3 <- function(flag, w, min_cm) {
  yes <- sum(w[flag %in% TRUE]); may <- sum(w[is.na(flag)])
  if (yes >= min_cm) TRUE else if (yes + may < min_cm) FALSE else NA
}

.sg_ie_num <- function(h, col) suppressWarnings(as.numeric(.usda_col(h, col, NA_real_)))

.sg_ie_txt <- function(h, col) tolower(trimws(as.character(.usda_col(h, col, NA_character_))))


# ---- regimes ------------------------------------------------------------------

.sg_ie_smr <- function(pedon) {
  x <- pedon$site$soil_moisture_regime %||% NA_character_
  if (.usda_regime_blank(x)) NA_character_ else trimws(.usda_smr_value(as.character(x)[1L]))
}
.sg_ie_str <- function(pedon) {
  x <- pedon$site$soil_temperature_regime %||% NA_character_
  if (.usda_regime_blank(x)) NA_character_ else trimws(.usda_str_value(as.character(x)[1L]))
}

#' Moisture-regime intergrades of the Inceptisol and Entisol subgroups
#'
#' The KST words these subgroups by the days the moisture control section is
#' dry or moist; the site records only the regime class, so a test is decided
#' only where the recorded class settles it, and is undecided otherwise
#' (open question of v0.9.425: no regime subdivision is recorded).
#' \describe{
#'   \item{\code{"dry45"}}{Ustic subgroups of the Cryepts (pp. 215-219): dry
#'     in some part for 45 or more cumulative days. Ustic, aridic (torric) or
#'     xeric: TRUE; perudic: FALSE; udic or aquic: undecided (a udic soil may
#'     be dry up to 90 cumulative days).}
#'   \item{\code{"aridic"}}{Aridic, Torrertic, Torrifluventic,
#'     Haplocalcidic, Aridic Lithic and Vitritorrandic subgroups of the
#'     ustic great groups (e.g. p. 238): an ustic regime that borders on
#'     aridic. Aridic or torric: TRUE; udic, perudic or aquic: FALSE; ustic
#'     or xeric: undecided.}
#'   \item{\code{"udic"}}{Udic, Udertic, Udifluventic and Calcic Udic
#'     subgroups (e.g. p. 238): an ustic regime that borders on udic. Udic or
#'     perudic: TRUE; aridic or torric: FALSE; ustic, xeric or aquic:
#'     undecided.}
#'   \item{\code{"torric_ustic"}}{Ustic and Ustertic subgroups of the Torri-
#'     great groups (pp. 172, 177, 183): an aridic regime that borders on
#'     ustic, the control section dry in all parts less than three-fourths of
#'     the time. Aridic or torric: undecided; another regime: FALSE.
#'     \code{str} names the temperature regimes allowed.}
#'   \item{\code{"torric_xeric"}}{Xeric, Xerertic, Duric Xeric and
#'     Vitrixerandic subgroups of the Torri- great groups: the same, bordering
#'     on xeric, with a thermic, mesic or frigid regime.}
#' }
#' @param pedon A \code{\link{PedonRecord}}.
#' @param kind One of the names above.
#' @param str Temperature regimes allowed (the torric kinds); NULL for any.
#' @noRd
sg_incept_moisture <- function(pedon, kind = c("dry45", "aridic", "udic", "torric_ustic", "torric_xeric"),
                               str = NULL) {
  kind <- match.arg(kind)
  smr <- .sg_ie_smr(pedon)
  st <- .sg_ie_str(pedon)
  if (kind == "torric_xeric" && is.null(str)) str <- c("thermic", "mesic", "frigid")
  val <- if (is.na(smr)) NA else switch(kind,
    dry45  = if (smr %in% c("ustic", "aridic", "torric", "xeric")) TRUE
             else if (smr %in% "perudic") FALSE else NA,
    aridic = if (smr %in% c("aridic", "torric")) TRUE
             else if (smr %in% c("udic", "perudic", "aquic")) FALSE else NA,
    udic   = if (smr %in% c("udic", "perudic")) TRUE
             else if (smr %in% c("aridic", "torric")) FALSE else NA,
    torric_ustic = if (smr %in% c("aridic", "torric")) NA else FALSE,
    torric_xeric = if (smr %in% c("aridic", "torric")) NA else FALSE)
  if (!is.null(str) && !is.na(st) && !(st %in% str)) val <- FALSE
  .sg_ie_res("sg_incept_moisture", val,
             evidence = list(kind = kind, soil_moisture_regime = smr, soil_temperature_regime = st),
             missing = "site$soil_moisture_regime",
             ref = .sg_ie_ref("pp. 172-185 and 215-246 (moisture intergrades)"),
             notes = if (is.na(val) && !is.na(smr))
               "the regime class is recorded, not the days the control section is dry"
             else NA_character_)
}


# ---- Vertic -------------------------------------------------------------------

#' Vertic criteria of the Inceptisol and Entisol subgroups (e.g. p. 209)
#'
#' One or both of: (1) cracks within 125 cm of the mineral soil surface 5 mm
#' or more wide through 30 cm or more, and slickensides or wedge-shaped peds
#' in a layer 15 cm or more thick with its top within 125 cm (field
#' observations: not recorded, absent); (2) a linear extensibility of 6.0
#' cm or more between the mineral soil surface and 100 cm or a densic,
#' lithic or paralithic contact (the sum of COLE times thickness, a
#' measurement: a layer without COLE leaves it undecided unless the measured
#' layers already reach 6 cm).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_incept_vertic <- function(pedon) {
  h <- pedon$horizons
  ref <- .sg_ie_ref("p. 209 (e.g. Vertic Endoaquepts)")
  if (!nrow(h)) return(.sg_ie_res("sg_incept_vertic", FALSE, ref = ref))
  ms <- .sg_ie_ms(h)
  cw <- .sg_ie_num(h, "cracks_width_cm"); cd <- .sg_ie_num(h, "cracks_depth_cm")
  ct <- .sg_ie_num(h, "cracks_top_cm")
  cracks <- any(!is.na(cw) & cw >= 0.5 & !is.na(cd) & cd >= 30 &
                  (is.na(ct) | ct < ms + 125) & !is.na(h$top_cm) & h$top_cm < ms + 125)
  ss <- .sg_ie_txt(h, "slickensides")
  st <- .sg_ie_txt(h, "structure_type")
  wedge <- (!is.na(ss) & !ss %in% c("none", "no", "absent", "false", "0", "")) |
           (!is.na(st) & grepl("wedge", st))
  ord <- order(h$top_cm)
  runs <- .usda_runs(wedge, ord)
  slick <- any(vapply(runs, function(r) .usda_run_thickness(h, r) >= 15 &&
                        min(h$top_cm[r], na.rm = TRUE) < ms + 125, NA))
  b1 <- cracks && slick
  end <- min(ms + 100, .sg_ie_contact(h))
  k <- .sg_ie_window(h, ms, end)
  ov <- .sg_ie_overlap(h, k, ms, end)
  cole <- .sg_ie_num(h, "cole_value")[k]
  le <- sum(ov * cole, na.rm = TRUE)
  b2 <- if (le >= 6) TRUE else if (!length(k) || !anyNA(cole[ov > 0])) FALSE else NA
  passed <- .usda_any3(c(b1, b2))
  .sg_ie_res("sg_incept_vertic", passed, layers = k,
             evidence = list(cracks = cracks, slickensides_15cm = slick, linear_extensibility_cm = le),
             missing = "cole_value", ref = ref)
}


# ---- redox features and aquic conditions --------------------------------------

# Redox depletions with chroma of 2 or less in layer i (TRUE / FALSE / NA): a
# g in the designation or a reduced matrix recorded; or redox features
# recorded in a matrix of chroma 2 or less, or with mottle colours of chroma
# 2 or less. Redox features are a field observation (not recorded: absent);
# with features recorded and no matrix chroma, undecided.
.sg_ie_depletions <- function(h, i, d) {
  if (!is.na(d$suffix[i]) && grepl("g", d$suffix[i])) return(TRUE)
  rm <- .sg_ie_num(h, "reduced_matrix_pct")[i]
  if (!is.na(rm) && rm > 0) return(TRUE)
  if (!isTRUE(.mo_redox_conc(h, i))) return(FALSE)
  mc <- as.character(.usda_col(h, "mottle_colors", NA_character_)[i])
  if (!is.na(mc)) {
    ch <- suppressWarnings(as.numeric(unlist(regmatches(mc, gregexpr("(?<=/)[0-9.]+", mc, perl = TRUE)))))
    if (any(ch <= 2, na.rm = TRUE)) return(TRUE)
  }
  cm <- h$munsell_chroma_moist[i]
  if (is.na(cm)) NA else cm <= 2
}

# Evidence of aquic conditions in layer i (any redox feature, a g, a reduced
# matrix), TRUE / FALSE.
.sg_ie_aquic_evidence <- function(h, i, d) {
  (!is.na(d$suffix[i]) && grepl("g", d$suffix[i])) ||
    isTRUE(.sg_ie_num(h, "reduced_matrix_pct")[i] > 0) || isTRUE(.mo_redox_conc(h, i))
}

# Top of the duripan, Inf when none.
.sg_ie_duripan_top <- function(pedon) {
  dp <- .usda_try(duripan_usda(pedon, max_top_cm = 1000))
  h <- pedon$horizons
  if (isTRUE(.usda_pv(dp)) && length(dp$layers)) min(h$top_cm[dp$layers], na.rm = TRUE) else Inf
}

#' Aquic subgroups of the Inceptisols and Entisols
#'
#' In one or more horizons within \code{max_cm} of the mineral soil surface
#' (and above the duripan when \code{above_duripan}), redox depletions with
#' chroma of 2 or less (\code{redox = "depletions"}) or distinct or
#' prominent redox concentrations (\code{"concentrations"}), and also aquic
#' conditions for some time in normal years (or artificial drainage). The
#' redox features stand as the evidence of the aquic conditions (as in the
#' Mollisols, v0.9.421). \code{gley_cm} adds the second branch of the Aquic
#' Fluvents (pp. 173-175): within that depth a moist value of 4 or more with
#' neutral colours of zero chroma, or with a hue of 5GY, 5G, 5BG or 5B and
#' aquic conditions (or, with \code{gley_aquic = TRUE}, aquic conditions
#' alone: Aquic Xerofluvents, item 2c).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_cm Depth below the mineral soil surface.
#' @param redox "depletions" or "concentrations".
#' @param above_duripan Only the horizons above the duripan.
#' @param gley_cm Depth of the gley-colour branch (NULL: none).
#' @param gley_aquic Item 2c of Aquic Xerofluvents.
#' @param soil_surface Depths from the soil surface (Torri- great groups).
#' @noRd
sg_incept_aquic <- function(pedon, max_cm = 75, redox = c("depletions", "concentrations"),
                            above_duripan = FALSE, gley_cm = NULL, gley_aquic = FALSE,
                            soil_surface = FALSE) {
  redox <- match.arg(redox)
  h <- pedon$horizons
  ref <- .sg_ie_ref("pp. 165-246 (Aquic subgroups)")
  if (!nrow(h)) return(.sg_ie_res("sg_incept_aquic", FALSE, ref = ref))
  ms <- if (isTRUE(soil_surface)) min(h$top_cm, na.rm = TRUE) else .sg_ie_ms(h)
  hi <- ms + max_cm
  if (isTRUE(above_duripan)) hi <- min(hi, .sg_ie_duripan_top(pedon))
  d <- .usda_desig(h$designation)
  k <- .sg_ie_window(h, ms, hi)
  v <- vapply(k, function(i) {
    if (redox == "depletions") .sg_ie_depletions(h, i, d) else .mo_redox_conc(h, i, contrast = TRUE)
  }, NA)
  g <- NA
  if (!is.null(gley_cm)) {
    kg <- .sg_ie_window(h, ms, ms + gley_cm)
    g <- .usda_any3(vapply(kg, function(i) {
      vm <- h$munsell_value_moist[i]; cm <- h$munsell_chroma_moist[i]
      hu <- toupper(gsub(" ", "", as.character(h$munsell_hue_moist[i])))
      aq <- .sg_ie_aquic_evidence(h, i, d)
      if (is.na(vm) || is.na(hu)) return(if (isTRUE(gley_aquic) && !aq) FALSE else NA)
      if (vm < 4) return(FALSE)
      neutral <- grepl("^N", hu) && (is.na(cm) || cm == 0)
      gley <- grepl("^5(GY|G|BG|B)$", hu) || (isTRUE(gley_aquic) && grepl("(GY|G|BG|B)$", hu))
      neutral || (gley && aq) || (isTRUE(gley_aquic) && aq)
    }, NA))
  }
  passed <- .usda_any3(c(v, if (!is.null(gley_cm)) g))
  .sg_ie_res("sg_incept_aquic", passed, layers = k[v %in% TRUE],
             evidence = list(max_cm = max_cm, redox = redox, per_layer = v, gley = g),
             missing = c("munsell_chroma_moist", "mottle_contrast", "munsell_value_moist"),
             ref = ref)
}


# ---- andic and vitrandic properties --------------------------------------------

#' Andic, Vitrandic and Aquandic criteria (Inceptisols and Entisols)
#'
#' Throughout one or more horizons with a total thickness of 18 cm or more
#' within 75 cm of the mineral soil surface: \code{kind = "andic"}, a bulk
#' density of 1.0 g/cm3 or less and Al + 1/2 Fe (oxalate) more than 1.0 \%;
#' \code{"vitrandic"}, the cinder or the volcanic-glass criteria;
#' \code{"either"}, one of the two (the Aquandic subgroups); \code{"glass"},
#' a fine earth with 5 \% or more volcanic glass and 60 (Al + 1/2 Fe) plus
#' glass of 30 or more (Vitrandic Psamments, pp. 182-185). The layer
#' readings are those of \code{.andic_subgroup_layers_usda()} (v0.9.417);
#' \code{above_duripan} keeps only the horizons above the duripan
#' (Durudepts, Durixerepts).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param kind "andic", "vitrandic", "either" or "glass".
#' @param above_duripan Logical.
#' @noRd
sg_incept_andic <- function(pedon, kind = c("andic", "vitrandic", "either", "glass"),
                            above_duripan = FALSE) {
  kind <- match.arg(kind)
  h <- pedon$horizons
  ref <- .sg_ie_ref("pp. 182-246 (Andic, Vitrandic and Aquandic subgroups)")
  if (!nrow(h)) return(.sg_ie_res("sg_incept_andic", FALSE, ref = ref))
  L <- .andic_subgroup_layers_usda(h)
  ok <- switch(kind,
    andic = L$andic, vitrandic = L$vitrandic,
    either = .sg_ie_any3_vec(L$andic, L$vitrandic),
    glass = {
      vg <- .sg_ie_num(h, "volcanic_glass_pct")
      alfe <- .sg_ie_num(h, "al_ox_pct") + .sg_ie_num(h, "fe_ox_pct") / 2
      sc <- ifelse(!is.na(alfe) & !is.na(vg), 60 * alfe + vg >= 30,
            ifelse(!is.na(vg) & vg >= 30, TRUE, ifelse(!is.na(alfe) & 60 * alfe >= 30, TRUE, NA)))
      ifelse(!is.na(vg) & vg < 5, FALSE, ifelse(is.na(vg), NA, sc))
    })
  if (isTRUE(above_duripan)) {
    dt <- .sg_ie_duripan_top(pedon)
    ok[!is.na(h$top_cm) & h$top_cm >= dt] <- FALSE
  }
  r <- .andic_subgroup_18_usda(h, ok)
  miss <- switch(kind, andic = L$missing_andic, vitrandic = L$missing_vitrandic,
                 either = c(L$missing_andic, L$missing_vitrandic),
                 glass = c("volcanic_glass_pct", "al_ox_pct", "fe_ox_pct"))
  .sg_ie_res("sg_incept_andic", r$passed, layers = r$layers,
             evidence = list(kind = kind, thickness_cm = r$thickness_cm, undecided_cm = r$undecided_cm),
             missing = miss, ref = ref)
}

# Element-wise three-valued OR of two logical vectors.
.sg_ie_any3_vec <- function(a, b) ifelse(a %in% TRUE | b %in% TRUE, TRUE,
                                        ifelse(a %in% FALSE & b %in% FALSE, FALSE, NA))


# ---- buried soils --------------------------------------------------------------

#' Thapto-Humic subgroups (Inceptisols and Entisols, e.g. p. 209)
#'
#' A buried layer within 200 cm of the soil surface that meets the criteria
#' of a histic, mollic, umbric or melanic epipedon, or buried O and dark A
#' horizons (moist value 3 or less) with a combined thickness of 20 cm or
#' more and 1.0 \% or more organic carbon. Read from the buried layers
#' (suffix b, a field observation: none, absent): a buried layer counts when
#' it is organic soil material, or has a moist value of 3 or less and 1.0
#' \% organic carbon or more; 20 cm of such layers pass. Colour or organic
#' carbon not recorded in a buried layer leaves the test undecided where it
#' could reach 20 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_incept_thapto_humic <- function(pedon) {
  h <- pedon$horizons
  ref <- .sg_ie_ref("p. 209 (e.g. Thapto-Humic Endoaquepts)")
  if (!nrow(h)) return(.sg_ie_res("sg_incept_thapto_humic", FALSE, ref = ref))
  top0 <- min(h$top_cm, na.rm = TRUE)
  d <- .usda_desig(h$designation)
  b <- which(!is.na(d$suffix) & grepl("b", d$suffix) & !is.na(h$top_cm) & h$top_cm < top0 + 200)
  if (!length(b)) return(.sg_ie_res("sg_incept_thapto_humic", FALSE, ref = ref,
                                    evidence = list(reason = "no buried layer")))
  org <- .usda_organic_status(pedon)
  ok <- vapply(b, function(i) {
    if (isTRUE(org[i]) || (!is.na(d$master[i]) && grepl("^[OH]", d$master[i]))) return(TRUE)
    dark <- if (is.na(h$munsell_value_moist[i])) NA else h$munsell_value_moist[i] <= 3
    oc <- if (is.na(h$oc_pct[i])) NA else h$oc_pct[i] >= 1
    .and3v(dark, oc)
  }, NA)
  w <- .sg_ie_overlap(h, b, top0, top0 + 200)
  passed <- .sg_ie_thick3(ok, w, 20)
  .sg_ie_res("sg_incept_thapto_humic", passed, layers = b[ok %in% TRUE],
             evidence = list(buried = b, per_layer = ok),
             missing = c("munsell_value_moist", "oc_pct"), ref = ref)
}

#' Thapto-Histic subgroups (Entisols, pp. 167-188)
#'
#' A buried layer of organic soil materials, 40 cm or more thick, within 200
#' cm of the mineral soil surface: organic layers below a mineral layer
#' (organic carbon of 12 \% or more, or an O or H designation where it is
#' not measured).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_ent_thapto_histic <- function(pedon) {
  h <- pedon$horizons
  ref <- .sg_ie_ref("p. 167 (e.g. Thapto-Histic Fluvaquents)")
  if (!nrow(h)) return(.sg_ie_res("sg_ent_thapto_histic", FALSE, ref = ref))
  ms <- .sg_ie_ms(h)
  org <- .usda_organic_status(pedon)
  k <- .sg_ie_window(h, ms, ms + 200)
  first_min <- k[!(org[k] %in% TRUE)]
  if (!length(first_min)) return(.sg_ie_res("sg_ent_thapto_histic", FALSE, ref = ref))
  below <- k[h$top_cm[k] >= h$bottom_cm[first_min[1]]]
  passed <- if (length(below)) .sg_ie_thick3(org[below], .sg_ie_overlap(h, below, ms, ms + 200), 40) else FALSE
  .sg_ie_res("sg_ent_thapto_histic", passed, layers = below[org[below] %in% TRUE],
             missing = "oc_pct", ref = ref)
}


# ---- Fluventic criteria ---------------------------------------------------------

#' Fluventic and Fluvaquentic criteria (Inceptisols and Entisols)
#'
#' A slope of less than 25 \% (not recorded, read as met, as in the
#' Fluvents of v0.9.419), less than 50 cm of human-transported material at
#' the surface (\code{htm}), and 0.2 \% organic carbon or more at 125 cm
#' with no densic, lithic or paralithic contact above, or an irregular
#' decrease of organic carbon from 25 cm (\code{.usda_fluvent_oc()}, a rise
#' of more than 0.05 points). With \code{boundaries = TRUE} (Fluventic
#' Gelepts, pp. 221-222) no irregular or broken horizon boundary either: a
#' boundary topography not recorded leaves it undecided.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param slope,htm Read the slope and the human-transported-material items.
#' @param boundaries Logical.
#' @noRd
sg_incept_fluventic <- function(pedon, slope = TRUE, htm = TRUE, boundaries = FALSE) {
  h <- pedon$horizons
  ref <- .sg_ie_ref("p. 210 (e.g. Fluvaquentic Endoaquepts)")
  if (!nrow(h)) return(.sg_ie_res("sg_incept_fluventic", FALSE, ref = ref))
  sl <- suppressWarnings(as.numeric(pedon$site$slope_pct %||% NA_real_))[1]
  s_ok <- if (!isTRUE(slope) || is.na(sl)) TRUE else sl < 25
  h_ok <- if (!isTRUE(htm)) TRUE else .usda_htm_surface_cm(pedon) < 50
  oc <- .usda_fluvent_oc(pedon)
  oc_ok <- .usda_any3(c(oc$at_125, oc$irregular))
  b_ok <- TRUE
  if (isTRUE(boundaries)) {
    bt <- .sg_ie_txt(h, "boundary_topography")
    b_ok <- if (any(grepl("irregular|broken", bt))) FALSE else if (anyNA(bt)) NA else TRUE
  }
  passed <- .usda_all3(c(s_ok, h_ok, oc_ok, b_ok))
  .sg_ie_res("sg_incept_fluventic", passed, layers = oc$layers,
             evidence = list(slope_under_25 = s_ok, htm_under_50 = h_ok,
                             oc_125 = oc$at_125, oc_irregular = oc$irregular, boundaries = b_ok),
             missing = c("oc_pct", if (is.na(b_ok)) "boundary_topography"), ref = ref)
}


# ---- surface colour, epipedons ---------------------------------------------------

#' Dark surface of the Humic, Mollic and Humaqueptic subgroups
#'
#' A moist colour value of 3 or less and a dry value of 5 or less (crushed
#' and smoothed) either throughout the upper \code{depth_cm} (18, or 15 in
#' the Aquepts and Aquents) of the mineral soil, or after mixing that depth
#' (the thickness-weighted mean). Colour not recorded: undecided.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param depth_cm 18 or 15.
#' @noRd
sg_incept_dark <- function(pedon, depth_cm = 18) {
  h <- pedon$horizons
  ref <- .sg_ie_ref("e.g. pp. 210 and 224 (Humic and Mollic subgroups)")
  if (!nrow(h)) return(.sg_ie_res("sg_incept_dark", FALSE, ref = ref))
  ms <- .sg_ie_ms(h)
  k <- .sg_ie_window(h, ms, ms + depth_cm)
  if (!length(k)) return(.sg_ie_res("sg_incept_dark", FALSE, ref = ref))
  vm <- h$munsell_value_moist[k]; vd <- h$munsell_value_dry[k]
  w <- .sg_ie_overlap(h, k, ms, ms + depth_cm)
  per <- .and3v(ifelse(is.na(vm), NA, vm <= 3), ifelse(is.na(vd), NA, vd <= 5))
  unmixed <- .usda_all3(per)
  mixed <- if (anyNA(vm) || anyNA(vd) || sum(w) <= 0) NA
           else (sum(vm * w) / sum(w) <= 3) && (sum(vd * w) / sum(w) <= 5)
  passed <- .usda_any3(c(unmixed, mixed))
  .sg_ie_res("sg_incept_dark", passed, layers = k,
             evidence = list(depth_cm = depth_cm, value_moist = vm, value_dry = vd),
             missing = c(if (anyNA(vm)) "munsell_value_moist", if (anyNA(vd)) "munsell_value_dry"),
             ref = ref)
}

#' Epipedons named by a subgroup (Humic Aquepts, Humic Cryaquepts ...)
#'
#' One of the epipedons in \code{kinds} (histic, mollic, umbric), and, with
#' \code{min_cm}, a mollic or umbric epipedon at least that thick (the
#' Cumulic and Pachic subgroups: 60 cm in the Humaquepts, 50 cm in the
#' Humudepts and Humixerepts).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param kinds Character vector.
#' @param min_cm Minimum thickness (NULL: any).
#' @noRd
sg_incept_epipedon <- function(pedon, kinds = c("mollic", "umbric"), min_cm = NULL) {
  ref <- .sg_ie_ref("pp. 207-246 (epipedons of the subgroups)")
  out <- c()
  if ("histic" %in% kinds) out["histic"] <- .usda_pv(.usda_try(histic_epipedon_usda(pedon)))
  if ("mollic" %in% kinds) {
    mo <- .usda_try(mollic_epipedon_usda(pedon)); v <- .usda_pv(mo)
    if (!is.null(min_cm) && isTRUE(v)) {
      e <- mo$evidence
      v <- if ((e$thickness_cm %||% 0) >= min_cm) TRUE
           else if ((e$thickness_possible_cm %||% e$thickness_cm %||% 0) < min_cm) FALSE else NA
    }
    out["mollic"] <- v
  }
  if ("umbric" %in% kinds) {
    um <- .usda_try(umbric_epipedon_usda(pedon)); v <- .usda_pv(um)
    if (!is.null(min_cm) && isTRUE(v)) {
      h <- pedon$horizons
      v <- sum(h$bottom_cm[um$layers] - h$top_cm[um$layers], na.rm = TRUE) >= min_cm
    }
    out["umbric"] <- v
  }
  passed <- .usda_any3(out)
  .sg_ie_res("sg_incept_epipedon", passed, evidence = as.list(out),
             missing = c("munsell_value_moist", "oc_pct", "bs_pct"), ref = ref)
}


# ---- base saturation ---------------------------------------------------------------

#' Base saturation criteria of the subgroups
#'
#' Base saturation (\code{bs_pct}, NH4OAc; or by sum of cations with
#' \code{method = "sum"}) compared with \code{value} (\code{op} "lt" or
#' "ge") in the layers between \code{top} and \code{bottom} cm below the
#' mineral soil surface: in some layer (\code{mode = "some"}), in all
#' (\code{"all"}), or in half or more of the thickness (\code{"half"}).
#' \code{from_ap}: from the Ap horizon or \code{top}, whichever is deeper;
#' \code{clip_contact}: the window ends at a shallower densic, lithic or
#' paralithic contact; \code{contact10}: or in some part of the 10 cm
#' directly above such a contact within 50 cm (the Eutric subgroups);
#' \code{root_limiting}: or the horizon directly above a root-limiting
#' layer shallower than \code{top} (Dystric Xerorthents, p. 181).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_incept_bs <- function(pedon, op = c("lt", "ge"), value = 50, top = 0, bottom = 100,
                         mode = c("some", "all", "half"), method = c("nh4oac", "sum"),
                         from_ap = FALSE, clip_contact = FALSE, contact10 = FALSE,
                         root_limiting = FALSE) {
  op <- match.arg(op); mode <- match.arg(mode); method <- match.arg(method)
  h <- pedon$horizons
  ref <- .sg_ie_ref("pp. 165-246 (base saturation of the subgroups)")
  if (!nrow(h)) return(.sg_ie_res("sg_incept_bs", FALSE, ref = ref))
  ms <- .sg_ie_ms(h)
  bs <- if (method == "sum") .bs_sum_cations_usda(pedon)$bs else .sg_ie_num(h, "bs_pct")
  cmp <- function(x) if (is.na(x)) NA else if (op == "lt") x < value else x >= value
  rl <- .sg_ie_contact(h)
  lo <- ms + top
  if (isTRUE(from_ap)) { ap <- .sg_ie_ap_bottom(h); if (!is.na(ap)) lo <- max(lo, ap) }
  hi <- ms + bottom
  if (isTRUE(clip_contact)) hi <- min(hi, rl)
  k <- .sg_ie_window(h, lo, hi)
  above <- function() {
    j <- which(!is.na(h$bottom_cm) & h$bottom_cm <= rl)
    if (length(j)) j[which.max(h$bottom_cm[j])] else integer(0)
  }
  if (isTRUE(root_limiting) && is.finite(rl) && rl <= lo) k <- above()
  v <- vapply(k, function(i) cmp(bs[i]), NA)
  passed <- if (!length(k)) FALSE else switch(mode,
    some = .usda_any3(v), all = .usda_all3(v),
    half = .sg_ie_share3(v, .sg_ie_overlap(h, k, lo, hi)))
  if (isTRUE(contact10) && is.finite(rl) && rl < ms + 50) {
    kc <- .sg_ie_window(h, rl - 10, rl)
    passed <- .usda_any3(c(passed, vapply(kc, function(i) cmp(bs[i]), NA)))
  }
  .sg_ie_res("sg_incept_bs", passed, layers = k[v %in% TRUE],
             evidence = list(op = op, value = value, window = c(lo, hi), mode = mode, per_layer = v),
             missing = if (method == "sum") c("ca_cmol", "mg_cmol", "k_cmol", "na_cmol", "cec_ph82_cmol", "bs_pct")
                       else "bs_pct", ref = ref)
}


# ---- matrix colours (Aeric subgroups) --------------------------------------------------

#' Aeric subgroups of the Aquepts, Aquents and Wassents
#'
#' Matrix colours (moist) of one or more horizons, read per \code{rule}:
#' \describe{
#'   \item{endoaquept}{Endo- and Epiaquepts (pp. 210-211): between the A or
#'     Ap horizon and 75 cm, hue 7.5YR or redder with chroma 2 or more, or
#'     hue 10YR or yellower with value and chroma 3 or more, or chroma 2 or
#'     more with no redox concentrations.}
#'   \item{cryaquept}{Cryaquepts (p. 209): chroma 3 or more between 15 and
#'     50 cm.}
#'   \item{densi}{Densi- and Fragiaquepts (pp. 208, 212): between the plow
#'     layer (or 15 cm) and 75 cm, chroma 3 or more, or 2 or more with no
#'     redox concentrations.}
#'   \item{halaquept}{Halaquepts (p. 213): chroma 3 or more between 15 and
#'     75 cm.}
#'   \item{humaquept}{Humaquepts (p. 214): hue 5Y or redder and chroma 3 or
#'     more between 15 and 75 cm.}
#'   \item{aquent_endo, aquent_epi}{Endo-, Epi- and Fluvaquents (pp.
#'     166-168): between the Ap horizon or 25 cm (whichever is deeper) and
#'     75 cm, hue 2.5Y or redder with value 6 or more and chroma 3 or more,
#'     or value 5 or less and chroma 2 or more; hue 5Y with chroma 3 or more;
#'     or chroma 2 or more with no redox concentrations (hue 5Y or redder in
#'     the Endoaquents).}
#'   \item{wassent}{Wassents (pp. 186-188): chroma 3 or more between 15 and
#'     100 cm from the soil surface.}
#' }
#' The matrix colour stands for the share of the matrix the KST names. Redox
#' concentrations are a field observation (not recorded: absent); colour
#' not recorded leaves a horizon undecided.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rule One of the names above.
#' @noRd
sg_incept_aeric <- function(pedon, rule = c("endoaquept", "cryaquept", "densi", "halaquept",
                                            "humaquept", "aquent_endo", "aquent_epi", "wassent")) {
  rule <- match.arg(rule)
  h <- pedon$horizons
  ref <- .sg_ie_ref("pp. 166-214 and 186-188 (Aeric subgroups)")
  if (!nrow(h)) return(.sg_ie_res("sg_incept_aeric", FALSE, ref = ref))
  ms <- .sg_ie_ms(h)
  d <- .usda_desig(h$designation)
  ap <- .sg_ie_ap_bottom(h)
  win <- switch(rule,
    endoaquept = {
      ord <- order(h$top_cm); a <- integer(0)
      for (i in ord) { if (!is.na(d$master[i]) && grepl("^A", d$master[i])) a <- c(a, i) else break }
      c(if (length(a)) max(h$bottom_cm[a], na.rm = TRUE) else ms, ms + 75)
    },
    cryaquept = c(ms + 15, ms + 50),
    densi = c(if (!is.na(ap)) ap else ms + 15, ms + 75),
    halaquept = c(ms + 15, ms + 75), humaquept = c(ms + 15, ms + 75),
    aquent_endo = , aquent_epi = c(max(ms + 25, ap, na.rm = TRUE), ms + 75),
    wassent = { s0 <- min(h$top_cm, na.rm = TRUE); c(s0 + 15, s0 + 100) })
  k <- .sg_ie_window(h, win[1], win[2])
  hr <- .usda_hue_rank(h$munsell_hue_moist)
  ge <- function(x, t) if (is.na(x)) NA else x >= t
  le <- function(x, t) if (is.na(x)) NA else x <= t
  v <- vapply(k, function(i) {
    cm <- h$munsell_chroma_moist[i]; vm <- h$munsell_value_moist[i]; r <- hr[i]
    hu <- toupper(as.character(h$munsell_hue_moist[i]))
    neutral <- !is.na(hu) && grepl("^N", hu)
    rc <- isTRUE(.mo_redox_conc(h, i))
    switch(rule,
      endoaquept = {
        if (neutral) return(FALSE)
        if (is.na(r)) return(NA)
        if (r <= 17.5) ge(cm, 2)
        else if (r >= 20) .usda_any3(c(.and3v(ge(vm, 3), ge(cm, 3)), if (rc) FALSE else ge(cm, 2)))
        else FALSE
      },
      cryaquept = , halaquept = , wassent = ge(cm, 3),
      humaquept = if (neutral) FALSE else .and3v(le(r, 25), ge(cm, 3)),
      densi = .usda_any3(c(ge(cm, 3), if (rc) FALSE else ge(cm, 2))),
      aquent_endo = , aquent_epi = {
        if (neutral) return(FALSE)
        red <- le(r, 22.5)
        y5 <- if (is.na(r)) NA else r == 25
        last <- if (rc) FALSE else if (rule == "aquent_epi") ge(cm, 2) else .and3v(le(r, 25), ge(cm, 2))
        .usda_any3(c(.and3v(red, .and3v(ge(vm, 6), ge(cm, 3))),
                     .and3v(red, .and3v(le(vm, 5), ge(cm, 2))),
                     .and3v(y5, ge(cm, 3)), last))
      })
  }, NA)
  passed <- .usda_any3(v)
  .sg_ie_res("sg_incept_aeric", passed, layers = k[v %in% TRUE],
             evidence = list(rule = rule, window = win, per_layer = v),
             missing = c("munsell_hue_moist", "munsell_value_moist", "munsell_chroma_moist"), ref = ref)
}


# ---- other morphology ---------------------------------------------------------------

#' Fragic soil properties (Fragic and Fragiaquic subgroups, e.g. p. 210)
#'
#' Fragic soil properties in 30 \% or more of a layer 15 cm or more thick
#' with its top within 100 cm, or in 60 \% or more of a layer 15 cm or more
#' thick: read from the designations with the suffix x (fragic
#' properties, a field observation: none, absent), 15 cm or more of
#' adjoining such layers.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_incept_fragic <- function(pedon) {
  h <- pedon$horizons
  d <- .usda_desig(h$designation)
  x <- !is.na(d$suffix) & grepl("x", d$suffix)
  runs <- .usda_runs(x, order(h$top_cm))
  passed <- any(vapply(runs, function(r) .usda_run_thickness(h, r) >= 15, NA))
  .sg_ie_res("sg_incept_fragic", passed, layers = which(x),
             ref = .sg_ie_ref("p. 210 (e.g. Fragic Endoaquepts)"))
}

#' Lamellae within 200 cm (Lamellic subgroups, e.g. pp. 216, 226, 182)
#'
#' Read from the designations that mark lamellae (the KST notation E and Bt
#' with an ampersand, or a designation naming lamellae) or a recorded
#' cumulative thickness of lamellae. A layer so designated holds two or more
#' lamellae. Field observation: none recorded, absent.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_incept_lamellae <- function(pedon) {
  h <- pedon$horizons
  top0 <- if (nrow(h)) min(h$top_cm, na.rm = TRUE) else 0
  des <- as.character(h$designation)
  lc <- .sg_ie_num(h, "lamellae_cumulative_cm")
  k <- which(!is.na(h$top_cm) & h$top_cm < .sg_ie_ms(h) + 200 &
               ((!is.na(des) & grepl("&|lamel", des, ignore.case = TRUE)) | (!is.na(lc) & lc > 0)))
  .sg_ie_res("sg_incept_lamellae", length(k) > 0L, layers = k,
             ref = .sg_ie_ref("p. 216 (e.g. Lamellic Dystrocryepts)"))
}

#' Sandy particle-size class throughout the control section (Psammentic
#' subgroups, pp. 226, 231; Psammentic Frasiwassents, p. 186)
#'
#' Less than 35 \% rock fragments and loamy fine sand or coarser in all
#' layers of the particle-size control section (\code{.usda_psamment_texture()}).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_incept_psammentic <- function(pedon) {
  r <- .usda_psamment_texture(pedon)
  .sg_ie_res("sg_incept_psammentic", r$passed, layers = r$layers,
             missing = c("clay_pct", "sand_pct", "texture_class_usda"),
             ref = .sg_ie_ref("p. 231 (Psammentic Humudepts)"))
}

#' Arenic Eutrudepts (p. 229): a texture class of loamy fine sand or coarser
#' in all horizons within 50 cm of the mineral soil surface.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_incept_arenic <- function(pedon) {
  h <- pedon$horizons
  ms <- .sg_ie_ms(h)
  k <- .sg_ie_window(h, ms, ms + 50)
  s <- .usda_texture_grades(h)$lfs_or_coarser[k]
  passed <- if (length(k)) .usda_all3(s) else FALSE
  .sg_ie_res("sg_incept_arenic", passed, layers = k,
             missing = c("clay_pct", "sand_pct", "texture_class_usda"),
             ref = .sg_ie_ref("p. 229 (Arenic Eutrudepts)"))
}

#' Oxic subgroups (pp. 226, 231, 234, 236, 239)
#'
#' In 50 \% or more of the soil volume between 25 cm and 100 cm (or a
#' shallower densic, lithic or paralithic contact): a CEC (NH4OAc pH 7) of
#' less than 24 cmol(+)/kg clay, or a ratio of clay to 1500 kPa water of 0.6
#' or more and CEC / (3 x (1500 kPa water - organic carbon, at most 1.00))
#' less than 24, read per kg clay (x 100, as the apparent clay of the oxic
#' and kandic horizons, v0.9.420; the water by mass only).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_incept_oxic <- function(pedon) {
  h <- pedon$horizons
  ref <- .sg_ie_ref("p. 226 (Oxic Dystrudepts)")
  if (!nrow(h)) return(.sg_ie_res("sg_incept_oxic", FALSE, ref = ref))
  ms <- .sg_ie_ms(h)
  hi <- min(ms + 100, .sg_ie_contact(h))
  k <- .sg_ie_window(h, ms + 25, hi)
  if (!length(k)) return(.sg_ie_res("sg_incept_oxic", FALSE, ref = ref))
  cec <- .sg_ie_num(h, "cec_cmol"); cl <- .sg_ie_num(h, "clay_pct")
  w15 <- .sg_ie_num(h, "water_content_1500kpa"); oc <- .sg_ie_num(h, "oc_pct")
  w15[!(.w1500_by_mass(pedon) %in% TRUE)] <- NA
  v <- vapply(k, function(i) {
    c1 <- if (is.na(cec[i]) || is.na(cl[i]) || cl[i] <= 0) NA else 100 * cec[i] / cl[i] < 24
    c2 <- if (is.na(cec[i]) || is.na(cl[i]) || is.na(w15[i]) || is.na(oc[i])) NA else {
      den <- 3 * (w15[i] - min(oc[i], 1))
      (w15[i] > 0 && cl[i] / w15[i] >= 0.6) && den > 0 && 100 * cec[i] / den < 24
    }
    .usda_any3(c(c1, c2))
  }, NA)
  passed <- .sg_ie_share3(v, .sg_ie_overlap(h, k, ms + 25, hi))
  .sg_ie_res("sg_incept_oxic", passed, layers = k[v %in% TRUE], evidence = list(per_layer = v),
             missing = c("cec_cmol", "clay_pct", "water_content_1500kpa"), ref = ref)
}

#' No free carbonates throughout any horizon within \code{max_cm} (Dystric
#' and Aquic Dystric Eutrudepts, p. 228): free carbonates read from the CaCO3
#' equivalent or the effervescence (\code{.mo_free_carbonates()}).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_cm Depth below the mineral soil surface.
#' @noRd
sg_incept_no_carbonates <- function(pedon, max_cm = 100) {
  h <- pedon$horizons
  ms <- .sg_ie_ms(h)
  k <- .sg_ie_window(h, ms, ms + max_cm)
  fc <- vapply(k, function(i) .mo_free_carbonates(h, i), NA)
  passed <- .usda_not3(.usda_any3(fc))
  .sg_ie_res("sg_incept_no_carbonates", passed, layers = k,
             missing = c("caco3_pct", "effervescence_class"),
             ref = .sg_ie_ref("p. 228 (Dystric Eutrudepts)"))
}

#' Rendollic Eutrudepts (p. 229): a CaCO3 equivalent of 40 \% or more in all
#' horizons from the top of the cambic horizon to 100 cm or a shallower
#' densic, lithic or paralithic contact.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_incept_rendollic <- function(pedon) {
  h <- pedon$horizons
  ref <- .sg_ie_ref("p. 229 (Rendollic Eutrudepts)")
  cb <- .usda_try(cambic_horizon_usda(pedon))
  if (!isTRUE(.usda_pv(cb)) || !length(cb$layers))
    return(.sg_ie_res("sg_incept_rendollic", if (is.na(.usda_pv(cb))) NA else FALSE,
                      missing = cb$missing %||% character(0), ref = ref))
  ms <- .sg_ie_ms(h)
  lo <- min(h$top_cm[cb$layers], na.rm = TRUE)
  k <- .sg_ie_window(h, lo, min(ms + 100, .sg_ie_contact(h)))
  ca <- .sg_ie_num(h, "caco3_pct")[k]
  passed <- .usda_all3(ifelse(is.na(ca), NA, ca >= 40))
  .sg_ie_res("sg_incept_rendollic", passed, layers = k, missing = "caco3_pct", ref = ref)
}

#' Ruptic-Alfic and Ruptic-Ultic subgroups (pp. 226-229)
#'
#' A cambic horizon with 10 to 50 \% (by volume) illuvial parts that
#' otherwise meet an argillic, kandic or natric horizon: an observation of
#' the pedon no column records, so it is absent.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_incept_ruptic <- function(pedon) {
  .sg_ie_res("sg_incept_ruptic", FALSE, ref = .sg_ie_ref("pp. 226-229 (Ruptic subgroups)"),
             notes = "illuvial parts within the cambic horizon are not recorded: absent")
}

#' Entic Humudepts and Humixerepts (pp. 231, 246): no cambic horizon, and no
#' part of the umbric or mollic epipedon meets the cambic horizon except for
#' colour (read as no B horizon within the epipedon).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_incept_entic <- function(pedon) {
  h <- pedon$horizons
  cb <- .usda_pv(.usda_try(cambic_horizon_usda(pedon)))
  ep <- c(.usda_try(mollic_epipedon_usda(pedon))$layers, .usda_try(umbric_epipedon_usda(pedon))$layers)
  d <- .usda_desig(h$designation)
  b_in <- any(!is.na(d$master[ep]) & grepl("B", d$master[ep]))
  passed <- if (isTRUE(cb) || b_in) FALSE else if (is.na(cb)) NA else TRUE
  .sg_ie_res("sg_incept_entic", passed, missing = "cambic horizon",
             ref = .sg_ie_ref("p. 231 (Entic Humudepts)"))
}


# ---- carbonates, salts, sodium, cementation ----------------------------------------

# Layers with identifiable secondary carbonates (field records: a k suffix,
# secondary or soft carbonates recorded, a form of secondary carbonates).
.sg_ie_secondary_carb <- function(h) {
  d <- .usda_desig(h$designation)
  sc <- .sg_ie_num(h, "secondary_carbonates_pct"); so <- .sg_ie_num(h, "carbonate_soft_pct")
  fm <- .sg_ie_txt(h, "secondary_carbonates_form")
  (!is.na(d$suffix) & grepl("k", d$suffix)) | (!is.na(sc) & sc > 0) | (!is.na(so) & so > 0) |
    (!is.na(fm) & nzchar(fm) & !fm %in% c("none", "no"))
}

#' Calcic subgroups of the Haplocryepts and Haploxerepts
#'
#' \code{rule = "secondary"}: identifiable secondary carbonates within
#' \code{max_cm} (Calcic Haplocryepts, p. 219). \code{rule = "haploxerept"}
#' (Calcic Haploxerepts, p. 245): a calcic horizon or identifiable secondary
#' carbonates within 150 cm (sandy or sandy-skeletal particle-size class),
#' 90 cm (clayey, fine or very-fine) or 110 cm (any other class). Secondary
#' carbonates are a field observation (not recorded: absent); the calcic
#' horizon is \code{calcic_horizon_usda()}.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_incept_calcic <- function(pedon, rule = c("secondary", "haploxerept"), max_cm = 100) {
  rule <- match.arg(rule)
  h <- pedon$horizons
  ms <- .sg_ie_ms(h)
  ref <- .sg_ie_ref("pp. 219 and 245 (Calcic subgroups)")
  cal <- .usda_try(calcic_horizon_usda(pedon, max_top_cm = ms + 150))
  sec <- .sg_ie_secondary_carb(h)
  tops <- c(h$top_cm[sec], if (isTRUE(.usda_pv(cal))) h$top_cm[cal$layers])
  t0 <- if (length(tops)) min(tops, na.rm = TRUE) - ms else Inf
  if (rule == "secondary")
    return(.sg_ie_res("sg_incept_calcic", t0 < max_cm, layers = which(sec), ref = ref))
  passed <- if (t0 < 90) TRUE else if (t0 >= 150) {
    if (is.na(.usda_pv(cal)) && !any(sec)) NA else FALSE
  } else {
    sandy <- .usda_psamment_texture(pedon)$passed
    L <- .usda_pscs_layers(h)
    cl <- .sg_ie_num(h, "clay_pct")[L]
    w <- pmax(h$bottom_cm[L] - h$top_cm[L], 0)
    clayey <- if (!length(L) || anyNA(cl)) NA else sum(cl * w) / sum(w) >= 35
    lim <- if (isTRUE(sandy)) 150 else if (isTRUE(clayey)) 90 else if (isFALSE(sandy) && isFALSE(clayey)) 110 else NA
    if (is.na(lim)) { if (t0 < 110 && isFALSE(clayey)) TRUE else NA } else t0 < lim
  }
  .sg_ie_res("sg_incept_calcic", passed, layers = which(sec), evidence = list(top_cm = t0),
             missing = c("clay_pct", "sand_pct"), ref = ref)
}

#' Exchangeable sodium of the Sodic subgroups
#'
#' An ESP (or Na / CEC) of \code{min_esp} or more in one or more horizons
#' within \code{max_cm} of the mineral soil surface (15 in the Sodic
#' Calcixerepts and Aquents, 7 in the Sodic Vermaquepts), in a horizon at
#' least \code{min_thick_cm} thick (25 in the Anthrodensic Sodic and
#' Anthraltic Sodic subgroups, from the soil surface). The 6 months of the
#' Aquents are not recorded; the measured ESP stands for them.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_incept_sodic <- function(pedon, min_esp = 15, max_cm = 100, min_thick_cm = 0,
                            soil_surface = FALSE) {
  h <- pedon$horizons
  ref <- .sg_ie_ref("pp. 167, 214, 240 (Sodic subgroups)")
  if (!nrow(h)) return(.sg_ie_res("sg_incept_sodic", FALSE, ref = ref))
  ms <- if (isTRUE(soil_surface)) min(h$top_cm, na.rm = TRUE) else .sg_ie_ms(h)
  k <- .sg_ie_window(h, ms, ms + max_cm)
  esp <- .usda_esp(h)[k]
  v <- ifelse(is.na(esp), NA, esp >= min_esp)
  w <- .sg_ie_overlap(h, k, ms, ms + max_cm)
  passed <- if (min_thick_cm <= 0) .usda_any3(v) else {
    if (any(v %in% TRUE & w >= min_thick_cm)) TRUE
    else if (any(!(v %in% FALSE) & w >= min_thick_cm)) NA else FALSE
  }
  if (!length(k)) passed <- FALSE
  .sg_ie_res("sg_incept_sodic", passed, layers = k[v %in% TRUE],
             missing = c("esp_pct", "na_cmol", "cec_cmol"), ref = ref)
}

#' Duric Halaquepts (p. 213): within 100 cm, one or more horizons with a
#' combined thickness of 15 cm or more and 20 \% or more (by volume) of soil
#' material extremely weakly coherent or more coherent by pedogenic
#' cementation (a recorded cementation class or pan, or a recorded share of
#' 20 \% or more; field observations: none, absent).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_incept_cemented <- function(pedon) {
  h <- pedon$horizons
  ms <- .sg_ie_ms(h)
  k <- .sg_ie_window(h, ms, ms + 100)
  share <- .sg_ie_num(h, "duripan_pct")
  cem <- .usda_cemented(h) & (is.na(share) | share >= 20) | (!is.na(share) & share >= 20)
  passed <- sum(.sg_ie_overlap(h, k, ms, ms + 100)[cem[k]]) >= 15
  .sg_ie_res("sg_incept_cemented", passed, layers = k[cem[k]],
             ref = .sg_ie_ref("p. 213 (Duric Halaquepts)"))
}

#' Entic Durixerepts (p. 241): a duripan strongly coherent or less coherent
#' in all subhorizons (no very strongly cemented or indurated part).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_incept_entic_duripan <- function(pedon) {
  h <- pedon$horizons
  dp <- .usda_try(duripan_usda(pedon, max_top_cm = 1000))
  L <- if (isTRUE(.usda_pv(dp))) dp$layers else integer(0)
  cem <- tolower(trimws(as.character(h$cementation_class[L])))
  passed <- if (!length(L)) FALSE
            else if (any(cem %in% c("very strongly", "indurated"))) FALSE
            else if (anyNA(cem)) NA else TRUE
  .sg_ie_res("sg_incept_entic_duripan", passed, layers = L, missing = "cementation_class",
             ref = .sg_ie_ref("p. 241 (Entic Durixerepts)"))
}

#' Durinodes or brittle firm material (Duric and Durinodic Entisols, pp.
#' 172-185; Haploduridic Torripsamments, p. 183): a horizon within 100 cm, 15
#' cm or more thick, with 20 \% or more durinodes (read from a q suffix
#' without m, or a recorded share of 20 \% or more of silica-cemented
#' material) or brittle with a firm or firmer rupture resistance (brittleness
#' is not recorded: absent). Field observations: none, absent.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_ent_durinodes <- function(pedon) {
  h <- pedon$horizons
  ms <- .sg_ie_ms(h)
  d <- .usda_desig(h$designation)
  share <- .sg_ie_num(h, "duripan_pct")
  q <- (!is.na(d$suffix) & grepl("q", d$suffix) & !grepl("m", d$suffix)) | (!is.na(share) & share >= 20)
  k <- .sg_ie_window(h, ms, ms + 100)
  k <- k[q[k]]
  passed <- any(h$bottom_cm[k] - h$top_cm[k] >= 15, na.rm = TRUE)
  .sg_ie_res("sg_ent_durinodes", passed, layers = k,
             ref = .sg_ie_ref("p. 172 (Duric Torrifluvents)"))
}

#' Plinthite in the subgroups (Plinthic Petraquepts, p. 214: half or more of
#' a horizon within 125 cm; Plinthic Quartzipsamments, p. 182: 5 \% or more
#' within 100 cm). A field estimate: plinthite_pct, or a v suffix (plinthite
#' recorded, its share not, which passes a low threshold and leaves a high
#' one undecided); nothing recorded, absent.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_incept_plinthite <- function(pedon, min_pct = 50, max_cm = 125) {
  h <- pedon$horizons
  ms <- .sg_ie_ms(h)
  k <- .sg_ie_window(h, ms, ms + max_cm)
  pp <- .sg_ie_num(h, "plinthite_pct")[k]
  d <- .usda_desig(h$designation)
  v <- ifelse(!is.na(pp), pp >= min_pct,
         ifelse(!is.na(d$suffix[k]) & grepl("v", d$suffix[k]), if (min_pct <= 5) TRUE else NA, FALSE))
  passed <- .usda_any3(v)
  .sg_ie_res("sg_incept_plinthite", passed, layers = k[v %in% TRUE], missing = "plinthite_pct",
             ref = .sg_ie_ref("pp. 182 and 214 (Plinthic subgroups)"))
}

#' Sulfic subgroups (Inceptisols p. 208; Entisols pp. 166-188)
#'
#' Within \code{max_cm} of the mineral soil surface: a sulfuric horizon
#' (\code{sulfuric = TRUE}, Aquepts); sulfidic materials (at least
#' \code{sulfidic_cm} thick: 15 cm in the Wassents); or a horizon 15 cm or
#' more thick with all the characteristics of a sulfuric horizon except a pH
#' of 3.5 to 4.0 and no sulfide (read as pH 3.5 to 4.0 with jarosite or
#' schwertmannite recorded).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_incept_sulfic <- function(pedon, max_cm = 150, sulfuric = TRUE, sulfidic_cm = 1,
                             near_sulfuric = TRUE) {
  h <- pedon$horizons
  ms <- .sg_ie_ms(h)
  r <- c(if (isTRUE(sulfuric)) .usda_pv(.usda_try(sulfuric_horizon_usda(pedon, max_top_cm = ms + max_cm))),
         .usda_pv(.usda_try(sulfidic_materials_usda(pedon, max_top_cm = ms + max_cm,
                                                     min_thickness_cm = sulfidic_cm))))
  if (isTRUE(near_sulfuric)) {
    k <- .sg_ie_window(h, ms, ms + max_cm)
    ph <- .sg_ie_num(h, "ph_h2o")[k]
    ja <- .usda_col(h, "jarosite_present", NA)[k] %in% TRUE |
          .usda_col(h, "schwertmannite_present", NA)[k] %in% TRUE
    ns <- !is.na(ph) & ph >= 3.5 & ph <= 4.0 & ja & (h$bottom_cm[k] - h$top_cm[k]) >= 15
    r <- c(r, any(ns))
  }
  passed <- .usda_any3(r)
  .sg_ie_res("sg_incept_sulfic", passed, missing = "sulfidic_s_pct",
             ref = .sg_ie_ref("p. 208 (Sulfic Cryaquepts) and p. 166 (Sulfic Endoaquents)"))
}

#' Fluidity criteria (Hydraquentic, Haplic, Fluic and Grossic subgroups)
#'
#' A layer is slightly fluid or more where the n value is more than 0.7 or
#' the fluidity class is slightly, moderately or very fluid; moderately fluid
#' or more where n is more than 0.9 or the class moderately or very fluid. A
#' fluidity class not recorded is absent (reading D4) where no n value is
#' measured.
#' \describe{
#'   \item{hydraquentic}{Sulfaquepts (p. 214): slightly fluid and 8 \% or
#'     more clay in a layer at 20 to 50 cm, or moderately fluid in a layer at
#'     50 to 100 cm.}
#'   \item{hydraquentic_lowclay}{Humaquepts (p. 213), as printed: slightly
#'     fluid and less than 8 \% clay at 20 to 50 cm, or moderately fluid at
#'     50 to 100 cm.}
#'   \item{haplic}{Haplic Sulfaquents and Sulfiwassents (pp. 169, 188): in
#'     some horizon at 20 to 50 cm, an n value of 0.7 or less or nonfluid, or
#'     less than 8 \% clay.}
#'   \item{fluic}{Fluic Frasiwassents (p. 186): in all horizons at 20 to 50
#'     cm, slightly fluid and 8 \% or more clay.}
#'   \item{grossic}{Grossic Hydrowassents (p. 187): the same at 20 to 100
#'     cm.}
#' }
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rule One of the names above.
#' @noRd
sg_incept_fluid <- function(pedon, rule = c("hydraquentic", "hydraquentic_lowclay", "haplic",
                                            "fluic", "grossic")) {
  rule <- match.arg(rule)
  h <- pedon$horizons
  ref <- .sg_ie_ref("pp. 169, 186-188, 213-214 (fluidity)")
  if (!nrow(h)) return(.sg_ie_res("sg_incept_fluid", FALSE, ref = ref))
  ms <- .sg_ie_ms(h)
  nv <- .sg_ie_num(h, "n_value"); fc <- .sg_ie_txt(h, "fluidity_class"); cl <- .sg_ie_num(h, "clay_pct")
  slight <- ifelse(!is.na(nv), nv > 0.7, !is.na(fc) & grepl("slight|moderate|very", fc) & !grepl("non", fc))
  modf <- ifelse(!is.na(nv), nv > 0.9, !is.na(fc) & grepl("moderate|very", fc) & !grepl("non", fc))
  c8 <- ifelse(is.na(cl), NA, cl >= 8)
  k1 <- .sg_ie_window(h, ms + 20, ms + 50); k2 <- .sg_ie_window(h, ms + 50, ms + 100)
  passed <- switch(rule,
    hydraquentic = .usda_any3(c(.and3v(slight[k1], c8[k1]), modf[k2])),
    hydraquentic_lowclay = .usda_any3(c(.and3v(slight[k1], .sg_ie_any3_vec(FALSE, !c8[k1])), modf[k2])),
    haplic = .usda_any3(c(!slight[k1], !c8[k1])),
    fluic = if (length(k1)) .usda_all3(.and3v(slight[k1], c8[k1])) else FALSE,
    grossic = { k <- .sg_ie_window(h, ms + 20, ms + 100)
                if (length(k)) .usda_all3(.and3v(slight[k], c8[k])) else FALSE })
  if (!length(passed) || length(passed) > 1L) passed <- .usda_any3(passed)
  .sg_ie_res("sg_incept_fluid", passed, missing = c("n_value", "fluidity_class", "clay_pct"), ref = ref)
}


# ---- human-made materials -------------------------------------------------------------

#' Anthropogenic subgroups (Anthrodensic, Anthroportic, Anthraltic,
#' Anthraquic; pp. 179-181, 226, 228, 236)
#'
#' Field observations (not recorded: absent):
#' \describe{
#'   \item{anthrodensic}{a densic contact due to mechanical compaction in
#'     more than 90 \% of the pedon within 100 cm of the soil surface: a
#'     layer whose origin or anthropic material names compaction.}
#'   \item{anthroportic}{human-transported material 50 cm or more thick
#'     (\code{.usda_htm_layers()}).}
#'   \item{anthraltic}{50 cm or more of human-altered material: layers whose
#'     origin or anthropic material names altered material, deep ploughing,
#'     trenching or excavation in place.}
#'   \item{anthraquic}{anthraquic conditions: paddy land (flood irrigation or
#'     a land use naming paddy rice, \code{.cst_paddy()}), an anthrostagnic
#'     regime, or a puddled layer.}
#' }
#' @param pedon A \code{\link{PedonRecord}}.
#' @param kind One of the names above.
#' @noRd
sg_incept_anthro <- function(pedon, kind = c("anthrodensic", "anthroportic", "anthraltic", "anthraquic")) {
  kind <- match.arg(kind)
  h <- pedon$horizons
  ref <- .sg_ie_ref("pp. 178-181, 226-228, 236 (anthropogenic subgroups)")
  if (!nrow(h)) return(.sg_ie_res("sg_incept_anthro", FALSE, ref = ref))
  txt <- paste(.sg_ie_txt(h, "layer_origin"), .sg_ie_txt(h, "anthropic_material"))
  thk <- pmax(h$bottom_cm - h$top_cm, 0)
  top0 <- min(h$top_cm, na.rm = TRUE)
  passed <- switch(kind,
    anthrodensic = any(grepl("compact", txt) & !is.na(h$top_cm) & h$top_cm < top0 + 100),
    anthroportic = sum(thk[.usda_htm_layers(pedon)], na.rm = TRUE) >= 50,
    anthraltic = sum(thk[grepl("alter|deep.?plou?gh|deep.?plow|trench|rigol|excavat|\\bham\\b", txt)],
                     na.rm = TRUE) >= 50,
    anthraquic = {
      raw <- tolower(as.character(pedon$site$soil_moisture_regime %||% NA_character_))[1]
      pd <- .usda_try(.cst_paddy(pedon$site))
      isTRUE(pd$value) || (!is.na(raw) && grepl("anthrostagnic|anthraquic", raw)) ||
        any(.usda_col(h, "puddled", NA) %in% TRUE)
    })
  .sg_ie_res("sg_incept_anthro", passed, evidence = list(kind = kind), ref = ref)
}

#' Haploplaggic Udipsamments (p. 184): a surface horizon 25 to 50 cm thick
#' that meets the plaggen epipedon except thickness: plaggen material at
#' the surface (a plaggen layer origin or anthropic material, or a recorded
#' plaggen cultivation), moist value 4 or less, dry value 5 or less, chroma
#' 2 or less and 0.6 \% organic carbon or more. Nothing recorded, absent.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_ent_haploplaggic <- function(pedon) {
  h <- pedon$horizons
  ref <- .sg_ie_ref("p. 184 (Haploplaggic Udipsamments)")
  if (!nrow(h)) return(.sg_ie_res("sg_ent_haploplaggic", FALSE, ref = ref))
  txt <- paste(.sg_ie_txt(h, "layer_origin"), .sg_ie_txt(h, "anthropic_material"))
  pl <- grepl("plagg", txt)
  if (identical(tolower(as.character(pedon$site$deep_cultivation_kind %||% NA_character_))[1], "plaggen")) {
    d <- .usda_desig(h$designation)
    pl <- pl | (!is.na(d$master) & d$master == "A")
  }
  run <- .usda_surface_run(h, pl)
  if (!length(run)) return(.sg_ie_res("sg_ent_haploplaggic", FALSE, ref = ref))
  thk <- .usda_run_thickness(h, run)
  le <- function(x, t) ifelse(is.na(x), NA, x <= t)
  col <- .usda_all3(c(le(h$munsell_value_moist[run], 4), le(h$munsell_value_dry[run], 5),
                      le(h$munsell_chroma_moist[run], 2), ifelse(is.na(h$oc_pct[run]), NA, h$oc_pct[run] >= 0.6)))
  passed <- .usda_all3(c(thk >= 25 && thk <= 50, col))
  .sg_ie_res("sg_ent_haploplaggic", passed, layers = run,
             missing = c("munsell_value_moist", "munsell_value_dry", "oc_pct"), ref = ref)
}

#' Vermic Orthents (pp. 179-180): 50 \% or more (by volume) wormholes,
#' wormcasts and filled animal burrows between the Ap horizon or 25 cm
#' (whichever is deeper) and 100 cm or a densic, lithic, paralithic or
#' petroferric contact. worm_holes_pct is a field estimate: not recorded,
#' absent; the share is thickness-weighted.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_ent_vermic <- function(pedon) {
  h <- pedon$horizons
  ms <- .sg_ie_ms(h)
  lo <- max(ms + 25, .sg_ie_ap_bottom(h), na.rm = TRUE)
  hi <- min(ms + 100, .sg_ie_contact(h))
  k <- .sg_ie_window(h, lo, hi)
  wh <- .sg_ie_num(h, "worm_holes_pct")[k]; wh[is.na(wh)] <- 0
  w <- .sg_ie_overlap(h, k, lo, hi)
  passed <- length(k) > 0L && sum(w) > 0 && sum(wh * w) / sum(w) >= 50
  .sg_ie_res("sg_ent_vermic", passed, layers = k, ref = .sg_ie_ref("p. 178 (Vermic Udorthents)"))
}


# ---- Psamments ----------------------------------------------------------------------------

#' Rhodic Psamments (pp. 183-184): in all horizons from 25 to 100 cm, hue
#' 2.5YR or redder, moist value 3 or less, and a dry value no more than one
#' unit higher than the moist value.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_ent_rhodic <- function(pedon) {
  h <- pedon$horizons
  ms <- .sg_ie_ms(h)
  k <- .sg_ie_window(h, ms + 25, ms + 100)
  hr <- .usda_hue_rank(h$munsell_hue_moist)
  v <- vapply(k, function(i) {
    vm <- h$munsell_value_moist[i]; vd <- h$munsell_value_dry[i]
    .usda_all3(c(if (is.na(hr[i])) NA else hr[i] <= 2.5, if (is.na(vm)) NA else vm <= 3,
                 if (is.na(vm) || is.na(vd)) NA else vd - vm <= 1))
  }, NA)
  passed <- if (length(k)) .usda_all3(v) else FALSE
  .sg_ie_res("sg_ent_rhodic", passed, layers = k,
             missing = c("munsell_hue_moist", "munsell_value_moist", "munsell_value_dry"),
             ref = .sg_ie_ref("p. 183 (Rhodic Torripsamments)"))
}

#' Ustoxic and Udoxic Quartzipsamments (p. 182): an ustic (or udic) regime,
#' a clay fraction with a CEC (NH4OAc pH 7) of 16 cmol(+)/kg clay or less,
#' and weighted silt plus twice the weighted clay more than 5, over the
#' particle-size control section.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param regime "ustic" or "udic".
#' @noRd
sg_ent_quartz_oxic <- function(pedon, regime = c("ustic", "udic")) {
  regime <- match.arg(regime)
  h <- pedon$horizons
  L <- .usda_pscs_layers(h)
  w <- pmax(h$bottom_cm[L] - h$top_cm[L], 0)
  cl <- .sg_ie_num(h, "clay_pct")[L]; si <- .sg_ie_num(h, "silt_pct")[L]; cec <- .sg_ie_num(h, "cec_cmol")[L]
  smr <- .usda_pv(soil_moisture_regime_usda(pedon, if (regime == "udic") c("udic", "perudic") else "ustic"))
  cc <- if (!length(L) || anyNA(cl) || anyNA(cec) || sum(cl * w) <= 0) NA
        else 100 * sum(cec * w) / sum(cl * w) <= 16
  ss <- if (!length(L) || anyNA(cl) || anyNA(si)) NA else (sum(si * w) + 2 * sum(cl * w)) / sum(w) > 5
  passed <- .usda_all3(c(smr, cc, ss))
  .sg_ie_res("sg_ent_quartz_oxic", passed, layers = L,
             missing = c("site$soil_moisture_regime", "clay_pct", "silt_pct", "cec_cmol"),
             ref = .sg_ie_ref("p. 182 (Ustoxic and Udoxic Quartzipsamments)"))
}


# ---- Entisol aliases of the shared tests ------------------------------------------------

sg_ent_aquic <- sg_incept_aquic
sg_ent_andic <- sg_incept_andic
sg_ent_vertic <- sg_incept_vertic
sg_ent_thapto_humic <- sg_incept_thapto_humic
sg_ent_fluventic <- sg_incept_fluventic
sg_ent_dark <- sg_incept_dark
sg_ent_bs <- sg_incept_bs
sg_ent_aeric <- sg_incept_aeric
sg_ent_lamellae <- sg_incept_lamellae
sg_ent_psammentic <- sg_incept_psammentic
sg_ent_sodic <- sg_incept_sodic
sg_ent_sulfic <- sg_incept_sulfic
sg_ent_fluid <- sg_incept_fluid
sg_ent_anthro <- sg_incept_anthro
sg_ent_moisture <- sg_incept_moisture
sg_ent_plinthite <- sg_incept_plinthite
