# =============================================================
# USDA Soil Taxonomy 13th edition: subgroup tests of the Aridisols
# (Ch. 7, pp. 137-163), Vertisols (Ch. 16, pp. 343-354) and Andisols (Ch. 6,
# pp. 117-136)
# =============================================================
#
# v0.9.426: the subgroup lists of these three orders were rebuilt from the
# KST 13th edition (inst/rules/usda/subgroups/{aridisols,vertisols,
# andisols}.yaml). The tests below are the criteria those lists need that no
# helper of another order already implements as the KST writes them for
# these orders. Every test is three-valued: TRUE or FALSE where the recorded
# data decide it, NA (with the missing fields listed) where data not
# recorded could change the answer. Field observations not recorded (redox
# features, slickensides, cracks, nodules, skeletans, jarosite, contacts,
# cementation) are absent; measurements not recorded (ESP, EC, COLE, water
# retention, bases, organic carbon, colour) are undecided.
#
# Readings of the regimes (audit P2, L32 and the CST precedent Q15):
#   * The Aridisol subgroups "that border on ustic (xeric)" read the moisture
#     regime record: a record naming both regimes ("ustic aridic", "aridic
#     (ustic)") passes; a record of one regime is its typic part and fails;
#     no record, undecided.
#   * The crack patterns of the Vertisol subgroups read
#     site$cracks_open_days (cumulative days a year with cracks 5 mm wide
#     through 25 cm within 50 cm) where recorded, and otherwise the recorded
#     regimes they stand for (KST p. 343 note); an aquic regime does not tell
#     the climate and leaves them undecided.
# =============================================================


# ---- small shared helpers ---------------------------------------------------

.sgav_ref <- function(x) paste0("Soil Survey Staff (2022), KST 13th ed., ", x)

.sgav_result <- function(name, passed, layers = integer(0), evidence = list(),
                         missing = character(0), ref = "") {
  DiagnosticResult$new(
    name = name, passed = passed,
    layers = if (isTRUE(passed)) as.integer(layers) else integer(0),
    evidence = evidence,
    missing = if (is.na(passed)) unique(as.character(missing)) else character(0),
    reference = ref)
}

# Thickness (cm) of the layers flagged TRUE and of those flagged NA inside
# [from, to].
.sgav_thk <- function(h, ok, from, to) {
  ov <- pmax(pmin(h$bottom_cm, to) - pmax(h$top_cm, from), 0)
  ov[is.na(ov)] <- 0
  c(yes = sum(ov[ok %in% TRUE]), may = sum(ov[is.na(ok)]))
}

# Thickest run of adjoining layers inside [from, to] that are TRUE, and that
# are TRUE or NA: c(yes, may) in cm.
.sgav_run <- function(h, ok, from, to) {
  ov <- pmax(pmin(h$bottom_cm, to) - pmax(h$top_cm, from), 0)
  ov[is.na(ov)] <- 0
  ord <- order(h$top_cm)
  best <- function(flag) {
    runs <- .usda_runs(flag, ord)
    if (!length(runs)) 0 else max(vapply(runs, function(r) sum(ov[r]), 0))
  }
  c(yes = best(ok %in% TRUE), may = best(!(ok %in% FALSE)))
}

# TRUE / FALSE / NA from a (yes, may) thickness pair and a limit.
.sgav_ge <- function(th, need) {
  if (th[["yes"]] >= need) TRUE else if (th[["may"]] < need) FALSE else NA
}

.sgav_mss <- function(h) {
  ms <- .mineral_soil_surface_cm(h)
  if (is.na(ms)) 0 else ms
}

# Top (cm) of the shallowest densic, lithic or paralithic contact recorded in
# the designations (a field observation): .usda_contact_top_cm() (R; a C with
# r or d) and transitional rock layers whose master begins with R or is CR,
# C/R (e.g. "2RC", weathered rock over the parent rock). NA when none.
.sgav_contact_top <- function(h) {
  a <- .usda_contact_top_cm(h)
  d <- .usda_desig(h$designation)
  hit <- !is.na(d$master) & grepl("^R|^C/?R", d$master)
  b <- if (any(hit)) min(h$top_cm[hit], na.rm = TRUE) else NA_real_
  m <- suppressWarnings(min(c(a, b), na.rm = TRUE))
  if (is.finite(m)) m else NA_real_
}

# Top (cm) of the first layer of a TRUE result, NA otherwise.
.sgav_top <- function(r, h) {
  if (!isTRUE(.usda_pv(r)) || !length(r$layers)) return(NA_real_)
  min(h$top_cm[r$layers], na.rm = TRUE)
}

# The recorded moisture regime as a single class (NA when blank).
.sgav_smr <- function(pedon) {
  raw <- pedon$site$soil_moisture_regime %||% NA_character_
  if (.usda_regime_blank(raw)) return(NA_character_)
  x <- trimws(.usda_smr_value(as.character(raw)[1L]))
  if (x == "torric") "aridic" else x
}

# Exchangeable bases (cmol/kg) of each layer, NH4OAc: the sum of Ca, Mg, K
# and Na where all four are measured, NA otherwise.
.sgav_bases <- function(h) {
  ca <- .usda_col(h, "ca_cmol", NA_real_); mg <- .usda_col(h, "mg_cmol", NA_real_)
  k <- .usda_col(h, "k_cmol", NA_real_); na <- .usda_col(h, "na_cmol", NA_real_)
  ca + mg + k + na
}

# KCl-extractable Al (cmol/kg): al_kcl_cmol, else al_cmol (the exchangeable
# Al of soilKey is the 1 M KCl one).
.sgav_al_kcl <- function(h) {
  a <- .usda_col(h, "al_kcl_cmol", NA_real_)
  b <- .usda_col(h, "al_cmol", NA_real_)
  ifelse(is.na(a), b, a)
}

# Bases plus KCl Al: the sum where both are measured, else the ECEC
# (which is that sum by definition).
.sgav_bases_al <- function(h) {
  s <- .sgav_bases(h) + .sgav_al_kcl(h)
  ifelse(is.na(s), .usda_col(h, "ecec_cmol", NA_real_), s)
}

# Redox features, a g designation or a reduced matrix recorded in a layer
# (aquic conditions shown, reading N10). Field observations.
.sgav_aquic_layer <- function(h, i) {
  d <- .usda_desig(h$designation[i])
  red <- .usda_col(h, "reduced_matrix_pct", NA_real_)[i]
  isTRUE(.mo_redox_conc(h, i)) ||
    (!is.na(d$suffix) && grepl("g", d$suffix)) ||
    (!is.na(red) && red > 0)
}


# ---- ARIDISOLS (KST 13th ed., Ch. 7, pp. 137-163) ---------------------------

#' Aridisol subgroups that border on the ustic or xeric regime (KST 13th ed.,
#' pp. 137-163)
#'
#' "A soil moisture regime that borders on ustic (xeric) and a moisture
#' control section dry in all parts for less than three-fourths of the time"
#' (e.g. Ustic Haplocalcids, p. 148). The record of the moisture regime
#' decides: a record that names both the aridic regime and the bordering one
#' (e.g. "ustic aridic", "aridic (ustic)") passes; a record of a single
#' regime is its typic part and fails; no record leaves it undecided. With
#' \code{temperature = TRUE} (Xeric and Xereptic Haplodurids, p. 158) the
#' mean annual soil temperature must also be below 22 C with 5 C or more
#' between summer and winter: a frigid, mesic, thermic or cryic regime
#' passes, an iso or hyperthermic one fails.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param toward "ustic" or "xeric".
#' @param temperature Also test the temperature clause.
#' @noRd
sg_aridisols_border_usda <- function(pedon, toward = c("ustic", "xeric"),
                                     temperature = FALSE) {
  toward <- match.arg(toward)
  raw <- pedon$site$soil_moisture_regime %||% NA_character_
  miss <- character(0)
  m <- if (.usda_regime_blank(raw)) {
    miss <- "site$soil_moisture_regime"; NA
  } else {
    x <- tolower(as.character(raw)[1L])
    w <- unique(regmatches(x, gregexpr("aridic|torric|ustic|xeric|perudic|udic|aquic", x))[[1]])
    w[w == "torric"] <- "aridic"
    w <- unique(w)
    length(w) >= 2L && toward %in% w
  }
  t <- TRUE
  if (isTRUE(temperature)) {
    st <- pedon$site$soil_temperature_regime %||% NA_character_
    if (.usda_regime_blank(st)) { t <- NA; miss <- c(miss, "site$soil_temperature_regime") }
    else t <- trimws(.usda_str_value(as.character(st)[1L])) %in%
                c("frigid", "mesic", "thermic", "cryic")
  }
  passed <- .usda_all3(c(m, t))
  .sgav_result(paste0("sg_aridisols_border_", toward, "_usda"), passed,
               evidence = list(soil_moisture_regime = raw, borders_on = toward,
                               temperature_clause = if (temperature) t else NULL),
               missing = miss,
               ref = .sgav_ref(if (temperature) "p. 158" else "pp. 137-163 (e.g. p. 148)"))
}

#' Vertic subgroups of the Aridisols (KST 13th ed., pp. 137-163)
#'
#' One or both of: (1) cracks within 125 cm of the soil surface 5 mm or
#' more wide through 30 cm or more, and slickensides or wedge-shaped peds in
#' a layer 15 cm or more thick starting within 125 cm; (2) a linear
#' extensibility of 6.0 cm or more between the soil surface and 100 cm or a
#' shallower densic, lithic or paralithic contact. With
#' \code{above_duripan = TRUE} (Vertic Argidurids and Natridurids, pp. 156,
#' 158) both are read above the duripan. Cracks and slickensides are field
#' observations (absent unless recorded); the linear extensibility is the
#' sum of COLE x thickness, undecided where COLE is not measured in a layer
#' that could carry it to 6 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param above_duripan Read above the duripan.
#' @noRd
sg_aridisols_vertic_usda <- function(pedon, above_duripan = FALSE)
  .usda_vertic_field_reading(.sg_aridisols_vertic_usda_record(pedon, above_duripan), pedon)

# The test as the record gives it; the wrapper above reads a COLE not
# recorded from the field record (R/subgroups-usda-vertic-field.R).
.sg_aridisols_vertic_usda_record <- function(pedon, above_duripan = FALSE) {
  h <- pedon$horizons
  ref <- .sgav_ref(if (above_duripan) "pp. 156, 158" else "pp. 137-163 (e.g. p. 138)")
  if (!nrow(h)) return(.sgav_result("sg_aridisols_vertic_usda", FALSE, ref = ref))
  bot <- 125
  le_bot <- 100
  ct <- .sgav_contact_top(h)
  if (!is.na(ct)) le_bot <- min(le_bot, ct)
  if (above_duripan) {
    dp <- .sgav_top(duripan_usda(pedon, max_top_cm = Inf), h)
    if (!is.na(dp)) { bot <- dp; le_bot <- min(le_bot, dp) }
  }
  win <- which(!is.na(h$top_cm) & h$top_cm < bot)
  # (1) cracks with slickensides
  cw <- .usda_col(h, "cracks_width_cm", NA_real_)
  ss <- tolower(as.character(.usda_col(h, "slickensides", NA_character_)))
  pf <- .usda_col(h, "pressure_faces", NA)
  wedge <- tolower(as.character(.usda_col(h, "structure_type", NA_character_)))
  crack <- !is.na(cw) & cw >= 0.5
  slick <- (!is.na(ss) & ss %in% c("few", "common", "many", "continuous", "present", "yes", "true")) |
           (!is.na(wedge) & grepl("wedge", wedge))
  ck <- .sgav_thk(h, crack, 0, bot)[["yes"]] >= 30
  sl <- .sgav_run(h, slick, 0, bot)[["yes"]] >= 15
  c1 <- ck && sl
  # (2) linear extensibility
  cole <- .usda_col(h, "cole_value", NA_real_)
  ov <- pmax(pmin(h$bottom_cm, le_bot) - pmax(h$top_cm, 0), 0); ov[is.na(ov)] <- 0
  known <- sum((ov * cole)[!is.na(cole)])
  unknown <- sum(ov[is.na(cole)])
  c2 <- if (known >= 6) TRUE else if (unknown <= 0) FALSE else NA
  passed <- .usda_any3(c(c1, c2))
  .sgav_result("sg_aridisols_vertic_usda", passed, layers = win,
               evidence = list(cracks_and_slickensides = c1, le_cm = known,
                               le_unmeasured_cm = unknown, le_bottom_cm = le_bot),
               missing = "cole_value", ref = ref)
}

#' Aquic subgroups of the Aridisols (KST 13th ed., pp. 137-163)
#'
#' Either (1) irrigated, with aquic conditions for some time in one or more
#' layers within 100 cm of the soil surface, or (2) saturated with water in
#' one or more layers within 100 cm for 1 month or more in normal years. (2)
#' is read from \code{water_saturation_days} (30 or more passes; under 30 in
#' every layer fails); (1) from \code{site$irrigated} and redox features, a g
#' designation or a reduced matrix within 100 cm (field observations). With
#' \code{irrigated = FALSE} only item 2 (Aquic Salicryids, p. 156).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param irrigated Whether the irrigated branch applies.
#' @noRd
sg_aridisols_aquic_usda <- function(pedon, irrigated = TRUE) {
  h <- pedon$horizons
  ref <- .sgav_ref("pp. 137-163 (e.g. p. 138)")
  cand <- which(!is.na(h$top_cm) & h$top_cm < 100)
  sd <- .usda_col(h, "water_saturation_days", NA_real_)[cand]
  sat <- if (any(sd >= 30, na.rm = TRUE)) TRUE
         else if (length(cand) && !anyNA(sd)) FALSE else NA
  irr <- FALSE
  if (isTRUE(irrigated) && isTRUE(as.logical(pedon$site$irrigated %||% NA))) {
    irr <- any(vapply(cand, function(i) .sgav_aquic_layer(h, i), NA))
  }
  passed <- .usda_any3(c(sat, irr))
  .sgav_result("sg_aridisols_aquic_usda", passed,
               layers = cand[!is.na(sd) & sd >= 30],
               evidence = list(saturation_1_month = sat, irrigated_aquic = irr),
               missing = "water_saturation_days", ref = ref)
}

#' Arenic subgroups of the Aridisols (KST 13th ed., pp. 138-145)
#'
#' A texture class of coarse sand, sand, fine sand, loamy coarse sand, loamy
#' sand or loamy fine sand throughout a layer from the (mineral) soil
#' surface to the top of an argillic horizon at a depth of 50 cm or more.
#' The texture is read by \code{.usda_texture_grades()} (laboratory, else
#' the field texture class).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_aridisols_arenic_usda <- function(pedon) {
  h <- pedon$horizons
  ref <- .sgav_ref("pp. 138-145 (e.g. Arenic Haplargids, p. 141)")
  ar <- argillic_usda(pedon)
  av <- .usda_pv(ar)
  if (isFALSE(av) || !nrow(h))
    return(.sgav_result("sg_aridisols_arenic_usda", FALSE, ref = ref))
  ms <- .sgav_mss(h)
  top <- .sgav_top(ar, h)
  if (is.na(av) || is.na(top))
    return(.sgav_result("sg_aridisols_arenic_usda", NA, missing = ar$missing, ref = ref))
  if (top < ms + 50)
    return(.sgav_result("sg_aridisols_arenic_usda", FALSE,
                        evidence = list(argillic_top_cm = top), ref = ref))
  above <- which(!is.na(h$top_cm) & h$top_cm < top & h$bottom_cm > ms)
  s <- .usda_texture_grades(h)$lfs_or_coarser[above]
  passed <- .usda_all3(s)
  .sgav_result("sg_aridisols_arenic_usda", passed, layers = above,
               evidence = list(argillic_top_cm = top, sandy = s),
               missing = c("clay_pct", "sand_pct", "texture_class_usda"), ref = ref)
}

#' Durinodic subgroups of the Aridisols (KST 13th ed., pp. 138-152)
#'
#' One or more horizons within 100 cm of the soil surface, 15 cm or more
#' thick in all, with 20\% or more (by volume) durinodes, or brittle with a
#' firm or firmer rupture resistance moist. soilKey has no durinode column:
#' a q (silica) in the designation of an uncemented horizon is a possible
#' durinode horizon, undecided; with no such record the horizons are absent
#' (field observations).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_aridisols_durinodic_usda <- function(pedon) {
  h <- pedon$horizons
  ref <- .sgav_ref("pp. 138-152 (e.g. Durinodic Haplocalcids, p. 147)")
  if (!nrow(h)) return(.sgav_result("sg_aridisols_durinodic_usda", FALSE, ref = ref))
  d <- .usda_desig(h$designation)
  q <- !is.na(d$suffix) & grepl("q", d$suffix) & !grepl("m", d$suffix)
  ok <- ifelse(q, NA, FALSE)
  th <- .sgav_thk(h, ok, 0, 100)
  passed <- .sgav_ge(th, 15)
  .sgav_result("sg_aridisols_durinodic_usda", passed,
               evidence = list(q_horizons = which(q)),
               missing = "durinodes (volume %)", ref = ref)
}

#' Petronodic subgroups of the Aridisols (KST 13th ed., pp. 138-162)
#'
#' One or more horizons within 100 cm of the soil surface, 15 cm or more
#' thick in all, with 20\% or more (by volume) nodules or concretions (with
#' \code{durinodes = TRUE}, the Gypsids, pp. 159-161: durinodes, nodules or
#' concretions). Read from \code{carbonate_hard_pct},
#' \code{ferric_nodules_pct}, \code{fe_mn_nodules_pct} and
#' \code{shajiang_pct}, and from \code{secondary_carbonates_pct} where
#' \code{secondary_carbonates_form} names nodules or concretions; field
#' observations, absent where not recorded.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param durinodes Whether durinodes also count.
#' @noRd
sg_aridisols_petronodic_usda <- function(pedon, durinodes = FALSE) {
  h <- pedon$horizons
  ref <- .sgav_ref("pp. 138-162 (e.g. Petronodic Haplocalcids, p. 147)")
  if (!nrow(h)) return(.sgav_result("sg_aridisols_petronodic_usda", FALSE, ref = ref))
  cols <- c("carbonate_hard_pct", "ferric_nodules_pct", "fe_mn_nodules_pct", "shajiang_pct")
  v <- do.call(pmax, c(lapply(cols, function(cc) {
    x <- suppressWarnings(as.numeric(.usda_col(h, cc, NA_real_))); x[is.na(x)] <- 0; x }),
    list(na.rm = TRUE)))
  sc <- suppressWarnings(as.numeric(.usda_col(h, "secondary_carbonates_pct", NA_real_)))
  sf <- tolower(as.character(.usda_col(h, "secondary_carbonates_form", NA_character_)))
  nod <- !is.na(sc) & !is.na(sf) & grepl("nodul|concret", sf)
  v[nod] <- pmax(v[nod], sc[nod])
  ok <- v >= 20
  if (durinodes) {
    d <- .usda_desig(h$designation)
    q <- !is.na(d$suffix) & grepl("q", d$suffix) & !grepl("m", d$suffix)
    ok <- ifelse(ok, TRUE, ifelse(q, NA, FALSE))
  }
  th <- .sgav_thk(h, ok, 0, 100)
  passed <- .sgav_ge(th, 15)
  .sgav_result("sg_aridisols_petronodic_usda", passed,
               layers = which(ok %in% TRUE),
               evidence = list(nodules_pct = v, thickness_cm = th),
               missing = "durinodes (volume %)", ref = ref)
}

#' Sodic subgroups of the Aridisols (KST 13th ed., pp. 147, 149-151, 161)
#'
#' A horizon at least 25 cm thick within 100 cm of the (mineral) soil
#' surface with an exchangeable sodium percentage of 15 or more (or an SAR of
#' 13 or more) during at least 1 month in normal years. The measured ESP
#' (\code{esp_pct}, or Na / CEC) is read; adjoining layers of 15 or more are
#' taken as one horizon.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_aridisols_sodic_usda <- function(pedon) {
  h <- pedon$horizons
  ref <- .sgav_ref("pp. 147, 149-151, 161")
  if (!nrow(h)) return(.sgav_result("sg_aridisols_sodic_usda", FALSE, ref = ref))
  ms <- .sgav_mss(h)
  esp <- .usda_esp(h)
  ok <- esp >= 15
  th <- .sgav_run(h, ok, ms, ms + 100)
  passed <- .sgav_ge(th, 25)
  .sgav_result("sg_aridisols_sodic_usda", passed, layers = which(ok %in% TRUE),
               evidence = list(esp = esp, run_cm = th),
               missing = c("esp_pct", "na_cmol", "cec_cmol"), ref = ref)
}

#' Lithic Ruptic-Entic Haplargids (KST 13th ed., p. 140): an argillic
#' horizon that is discontinuous throughout each pedon. The continuity is a
#' field observation (\code{horizon_continuity} of the argillic layers:
#' broken or discontinuous); not recorded, continuous.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_aridisols_ruptic_usda <- function(pedon) {
  h <- pedon$horizons
  ref <- .sgav_ref("p. 140")
  ar <- argillic_usda(pedon)
  av <- .usda_pv(ar)
  if (!isTRUE(av))
    return(.sgav_result("sg_aridisols_ruptic_usda", if (is.na(av)) NA else FALSE,
                        missing = ar$missing, ref = ref))
  hc <- tolower(as.character(.usda_col(h, "horizon_continuity", NA_character_)))[ar$layers]
  passed <- length(hc) > 0L && all(!is.na(hc) & grepl("broken|discontin|interrupt", hc))
  .sgav_result("sg_aridisols_ruptic_usda", passed, layers = ar$layers,
               evidence = list(continuity = hc), ref = ref)
}

#' Haplic and Haploxeralfic Natrargids (KST 13th ed., p. 143): an
#' exchangeable sodium percentage of less than 15 (or an SAR of less than
#' 13) in 50\% or more of the natric horizon (by thickness).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_aridisols_haplic_natric_usda <- function(pedon) {
  h <- pedon$horizons
  ref <- .sgav_ref("p. 143")
  nr <- natric_horizon_usda(pedon)
  nv <- .usda_pv(nr)
  if (!isTRUE(nv) || !length(nr$layers))
    return(.sgav_result("sg_aridisols_haplic_natric_usda", if (is.na(nv)) NA else FALSE,
                        missing = nr$missing, ref = ref))
  L <- nr$layers
  esp <- .usda_esp(h)[L]
  tk <- pmax(h$bottom_cm[L] - h$top_cm[L], 0)
  tot <- sum(tk, na.rm = TRUE)
  yes <- sum(tk[!is.na(esp) & esp < 15]); no <- sum(tk[!is.na(esp) & esp >= 15])
  passed <- if (tot <= 0) NA else if (yes >= 0.5 * tot) TRUE
            else if (tot - no < 0.5 * tot) FALSE else NA
  .sgav_result("sg_aridisols_haplic_natric_usda", passed, layers = L,
               evidence = list(natric_layers = L, esp = esp),
               missing = c("esp_pct", "na_cmol", "cec_cmol"), ref = ref)
}

#' Glossic Natrargids (KST 13th ed., pp. 142-144): skeletans covering 10\% or
#' more of the ped surfaces at 2.5 cm or more below the upper boundary of the
#' natric horizon (\code{skeletans_pct}, a field observation: absent unless
#' recorded).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_aridisols_glossic_usda <- function(pedon) {
  h <- pedon$horizons
  ref <- .sgav_ref("pp. 142-144")
  nr <- natric_horizon_usda(pedon)
  nv <- .usda_pv(nr)
  if (!isTRUE(nv) || !length(nr$layers))
    return(.sgav_result("sg_aridisols_glossic_usda", if (is.na(nv)) NA else FALSE,
                        missing = nr$missing, ref = ref))
  L <- nr$layers
  top <- min(h$top_cm[L], na.rm = TRUE)
  k <- L[!is.na(h$bottom_cm[L]) & h$bottom_cm[L] > top + 2.5]
  sk <- .usda_col(h, "skeletans_pct", NA_real_)[k]
  passed <- any(!is.na(sk) & sk >= 10)
  .sgav_result("sg_aridisols_glossic_usda", passed, layers = k[!is.na(sk) & sk >= 10],
               evidence = list(skeletans_pct = sk), ref = ref)
}

#' A calcic horizon overlying another horizon (KST 13th ed., pp. 148, 160,
#' 162): Calcic Lithic and Calcic Petrocalcids (over the petrocalcic),
#' Calcic Argigypsids (over the gypsic), Calcic Petrogypsids (over the
#' petrogypsic).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param under "petrocalcic", "gypsic" or "petrogypsic".
#' @noRd
sg_aridisols_calcic_over_usda <- function(pedon, under = c("petrocalcic", "gypsic", "petrogypsic")) {
  under <- match.arg(under)
  h <- pedon$horizons
  ref <- .sgav_ref("pp. 148, 160, 162")
  ca <- calcic_horizon_usda(pedon, max_top_cm = Inf)
  lo <- switch(under,
    petrocalcic = petrocalcic_subgroup_usda(pedon, max_top_cm = Inf),
    gypsic = gypsic_horizon_usda(pedon, max_top_cm = Inf),
    petrogypsic = petrogypsic_horizon_usda(pedon, max_top_cm = Inf))
  cv <- .usda_pv(ca); lv <- .usda_pv(lo)
  passed <- if (isFALSE(cv) || isFALSE(lv)) FALSE
    else if (isTRUE(cv) && isTRUE(lv)) {
      ct <- .sgav_top(ca, h); lt <- .sgav_top(lo, h)
      if (is.na(ct) || is.na(lt)) NA else ct < lt
    } else NA
  .sgav_result(paste0("sg_aridisols_calcic_over_", under, "_usda"), passed,
               layers = ca$layers,
               evidence = list(calcic = cv, under = lv),
               missing = c(if (is.na(cv)) ca$missing, if (is.na(lv)) lo$missing), ref = ref)
}

#' A duripan that is not indurated, or strongly coherent or less coherent, in
#' all its subhorizons (KST 13th ed., pp. 155-158, 353-354): Xereptic
#' Petrocryids, Haploxeralfic and Argidic Argidurids, Aquicambidic,
#' Xereptic and Cambidic Haplodurids, the Natrargidic and Natrixeralfic
#' Natridurids (\code{max = "strongly"}: no subhorizon very strongly cemented
#' or indurated), and Haplic Durixererts (\code{max = "very strongly"}: no
#' subhorizon indurated). Read from \code{cementation_class} of the duripan
#' layers; a layer with no class recorded leaves it undecided.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max The strongest class allowed.
#' @param max_top_cm Depth of the duripan.
#' @noRd
sg_usda_duripan_weak_usda <- function(pedon, max = c("strongly", "very strongly"),
                                      max_top_cm = Inf) {
  max <- match.arg(max)
  h <- pedon$horizons
  ref <- .sgav_ref("pp. 155-158, 354")
  dp <- duripan_usda(pedon, max_top_cm = max_top_cm)
  dv <- .usda_pv(dp)
  if (!isTRUE(dv) || !length(dp$layers))
    return(.sgav_result("sg_usda_duripan_weak_usda", if (is.na(dv)) NA else FALSE,
                        missing = dp$missing, ref = ref))
  cem <- tolower(trimws(as.character(.usda_col(h, "cementation_class", NA_character_))))[dp$layers]
  too <- if (max == "strongly") c("very strongly", "indurated") else "indurated"
  passed <- if (any(cem %in% too)) FALSE else if (anyNA(cem)) NA else TRUE
  .sgav_result("sg_usda_duripan_weak_usda", passed, layers = dp$layers,
               evidence = list(cementation_class = cem, strongest_allowed = max),
               missing = "cementation_class", ref = ref)
}

#' Abruptic Argidurids (KST 13th ed., p. 157): an argillic horizon with 35\%
#' or more (noncarbonate) clay in one or more subhorizons, and a clay
#' increase of 15\% (absolute) or more within 2.5 cm at or in its upper
#' part, or of 10\% or more under an Ap horizon, or an abrupt textural
#' change at its upper boundary. A clay increase within 2.5 cm needs an
#' abrupt or very abrupt boundary (a clear one is undecided).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_aridisols_abruptic_usda <- function(pedon) {
  h <- pedon$horizons
  ref <- .sgav_ref("p. 157")
  ar <- argillic_usda(pedon)
  av <- .usda_pv(ar)
  if (!isTRUE(av) || !length(ar$layers))
    return(.sgav_result("sg_aridisols_abruptic_usda", if (is.na(av)) NA else FALSE,
                        missing = ar$missing, ref = ref))
  L <- ar$layers[order(h$top_cm[ar$layers])]
  cl <- h$clay_pct
  c35 <- .usda_any3(cl[L] >= 35)
  ord <- order(h$top_cm)
  pos <- match(L[1], ord)
  inc <- FALSE
  if (!is.na(pos) && pos > 1L) {
    o <- ord[pos - 1L]; i <- L[1]
    bd <- tolower(as.character(.usda_col(h, "boundary_distinctness", NA_character_)))[o]
    d <- .usda_desig(h$designation[o])
    ap <- !is.na(d$master) && d$master == "A" && grepl("p", d$suffix)
    dif <- cl[i] - cl[o]
    sharp <- if (is.na(bd)) NA else if (grepl("abrupt|sharp", bd)) TRUE
             else if (grepl("clear", bd)) NA else FALSE
    a <- if (is.na(dif)) NA else .usda_all3(c(dif >= 15, sharp))
    b <- if (is.na(dif)) NA else (ap && dif >= 10)
    cc <- .abrupt_textural_change_usda(cl[o], cl[i], h$boundary_distinctness[o])
    inc <- .usda_any3(c(a, b, cc))
  }
  passed <- .usda_all3(c(c35, inc))
  .sgav_result("sg_aridisols_abruptic_usda", passed, layers = L,
               evidence = list(clay_35 = c35, increase = inc),
               missing = c("clay_pct", "boundary_distinctness"), ref = ref)
}

#' The anhydritic horizon (KST 13th ed., p. 18; Anhydritic Aquisalids and
#' Haplosalids, p. 163) within 100 cm of the soil surface: 15 cm or more
#' thick with 5\% or more anhydrite. soilKey records no anhydrite: the
#' horizon is ruled out where the calcium sulfate measured
#' (\code{caso4_pct}) is under 5\% in every layer within 100 cm, and
#' undecided otherwise.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_aridisols_anhydritic_usda <- function(pedon) {
  h <- pedon$horizons
  ref <- .sgav_ref("pp. 18, 163")
  cand <- which(!is.na(h$top_cm) & h$top_cm < 100)
  cs <- .usda_col(h, "caso4_pct", NA_real_)[cand]
  ok <- ifelse(!is.na(cs) & cs < 5, FALSE, NA)
  th <- .sgav_run(h[cand, , drop = FALSE], ok, 0, 100)
  passed <- if (th[["may"]] < 15) FALSE else NA
  .sgav_result("sg_aridisols_anhydritic_usda", passed,
               evidence = list(caso4_pct = cs), missing = "anhydrite (%)", ref = ref)
}

#' Thapto-Humic Aquicambids and Haplocambids (KST 13th ed., pp. 150-151):
#' a buried layer within 200 cm of the soil surface that meets the criteria
#' of a histic, mollic, umbric or melanic epipedon, or buried O and dark A
#' horizons (moist value 3 or less), 20 cm or more thick in all, with 1.0\%
#' or more organic carbon. Read from the buried layers (b in the
#' designation, a field observation): dark, with 1\% organic carbon or more;
#' undecided where the colour or the carbon of a buried layer is missing.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_aridisols_thapto_humic_usda <- function(pedon) {
  h <- pedon$horizons
  ref <- .sgav_ref("pp. 150-151")
  if (!nrow(h)) return(.sgav_result("sg_aridisols_thapto_humic_usda", FALSE, ref = ref))
  d <- .usda_desig(h$designation)
  b <- !is.na(d$suffix) & grepl("b", d$suffix) & !is.na(d$master) & grepl("^[OAH]", d$master)
  vm <- h$munsell_value_moist; oc <- h$oc_pct
  org <- !is.na(d$master) & grepl("^[OH]", d$master)
  dark <- ifelse(org, TRUE, vm <= 3)
  ok <- ifelse(b, dark & (oc >= 1), FALSE)
  th <- .sgav_thk(h, ok, 0, 200)
  passed <- .sgav_ge(th, 20)
  .sgav_result("sg_aridisols_thapto_humic_usda", passed, layers = which(ok %in% TRUE),
               evidence = list(buried = which(b), thickness_cm = th),
               missing = c("munsell_value_moist", "oc_pct"), ref = ref)
}

#' Fluventic subgroups of the Cambids (KST 13th ed., pp. 150-151): less than
#' 50 cm of human-transported material at the surface and an irregular
#' decrease in organic carbon between 25 cm and 125 cm or a shallower densic,
#' lithic or paralithic contact (\code{.usda_fluvent_oc()}, audit L15).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_aridisols_fluventic_usda <- function(pedon) {
  ref <- .sgav_ref("pp. 150-151")
  htm <- .usda_htm_surface_cm(pedon) < 50
  oc <- .usda_fluvent_oc(pedon)
  passed <- .usda_all3(c(htm, oc$irregular))
  .sgav_result("sg_aridisols_fluventic_usda", passed, layers = oc$layers,
               evidence = list(htm_under_50 = htm, irregular_oc = oc$irregular),
               missing = "oc_pct", ref = ref)
}


# ---- VERTISOLS (KST 13th ed., Ch. 16, pp. 343-354) --------------------------

#' The crack patterns of the Vertisol subgroups (KST 13th ed., pp. 344-354)
#'
#' \itemize{
#'   \item \code{"aridic"}: cracks 5 mm or more wide through 25 cm or more
#'     within 50 cm for 210 or more cumulative days a year (Aridic Aquerts
#'     and Usterts);
#'   \item \code{"ustic"}: the same for 90 or more cumulative days (Ustic
#'     Aquerts);
#'   \item \code{"udic"}: for less than 150 cumulative days (Udic Usterts);
#'   \item \code{"xeric"}: a thermic, mesic or frigid temperature regime and
#'     the xeric pattern of the Xererts (Xeric Aquerts);
#'   \item \code{"xeric_aridic"}: open 180 or more consecutive days (Aridic
#'     Xererts);
#'   \item \code{"xeric_udic"}: open less than 90 consecutive days (Udic
#'     Xererts).
#' }
#' \code{site$cracks_open_days} (cumulative days) decides where recorded and
#' it can; otherwise the recorded moisture regime stands for the pattern
#' (KST p. 343 note; audit L32): aridic for "aridic", ustic or aridic for
#' "ustic", udic or perudic for "udic" and "xeric_udic", aridic for
#' "xeric_aridic", xeric for "xeric"; another recorded regime is its typic
#' part and fails. An aquic regime does not tell the crack pattern
#' (undecided), nor does a xeric one for "ustic".
#' @param pedon A \code{\link{PedonRecord}}.
#' @param pattern One of the patterns above.
#' @noRd
sg_vertisols_cracks_usda <- function(pedon, pattern = c("aridic", "ustic", "udic", "xeric",
                                                        "xeric_aridic", "xeric_udic")) {
  pattern <- match.arg(pattern)
  days <- suppressWarnings(as.numeric(pedon$site$cracks_open_days %||% NA_real_))[1]
  smr <- .sgav_smr(pedon)
  miss <- "site$soil_moisture_regime"
  by_smr <- function(yes, unknown = character(0)) {
    if (is.na(smr) || smr == "aquic" || smr %in% unknown) NA else smr %in% yes
  }
  passed <- switch(pattern,
    aridic = if (!is.na(days)) days >= 210 else by_smr("aridic"),
    ustic = if (!is.na(days)) days >= 90 else by_smr(c("ustic", "aridic"), unknown = "xeric"),
    udic = if (!is.na(days)) days < 150 else by_smr(c("udic", "perudic")),
    xeric = {
      if (!is.na(days) && days < 60) FALSE else {
        m <- by_smr("xeric")
        if (isTRUE(m)) {
          st <- pedon$site$soil_temperature_regime %||% NA_character_
          if (.usda_regime_blank(st)) { miss <- c(miss, "site$soil_temperature_regime"); NA }
          else trimws(.usda_str_value(as.character(st)[1L])) %in% c("thermic", "mesic", "frigid")
        } else m
      }
    },
    xeric_aridic = {
      if (!is.na(days) && days < 180) FALSE else {
        m <- by_smr("aridic")
        if (isFALSE(m) && !is.na(days)) NA else m
      }
    },
    xeric_udic = {
      if (!is.na(days) && days < 90) TRUE else {
        m <- by_smr(c("udic", "perudic"))
        if (isFALSE(m) && !is.na(days)) NA else m
      }
    })
  .sgav_result(paste0("sg_vertisols_cracks_", pattern, "_usda"), passed,
               evidence = list(cracks_open_days = days, soil_moisture_regime = smr,
                               pattern = pattern),
               missing = c("site$cracks_open_days", miss),
               ref = .sgav_ref("pp. 343-354 (crack patterns; p. 343 note)"))
}

# The depth window (top, bottom) of a Vertisol subgroup test: from the
# mineral soil surface to `max_cm` below it, or to the top of the duripan
# with `above_duripan`.
.sgav_vert_window <- function(pedon, max_cm = 100, above_duripan = FALSE) {
  h <- pedon$horizons
  ms <- .sgav_mss(h)
  bot <- ms + max_cm
  if (above_duripan) {
    dp <- .sgav_top(duripan_usda(pedon, max_top_cm = Inf), h)
    bot <- if (is.na(dp)) bot else min(bot, dp)
  }
  c(top = ms, bottom = bot)
}

#' Halic subgroups of the Vertisols (KST 13th ed., pp. 345-354)
#'
#' Throughout a layer 15 cm or more thick within 100 cm of the mineral soil
#' surface (above the duripan in the Durixererts), an electrical
#' conductivity of 15 dS/m or more (saturated paste) for 6 or more months in
#' normal years. The measured \code{ec_dS_m} is read as that of the season;
#' \code{ec_max_dS_m}, an upper bound, can rule a layer out.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param above_duripan Read above the duripan.
#' @noRd
sg_vertisols_halic_usda <- function(pedon, above_duripan = FALSE) {
  h <- pedon$horizons
  ref <- .sgav_ref("pp. 345-354 (e.g. Halic Haplusterts, p. 351)")
  if (!nrow(h)) return(.sgav_result("sg_vertisols_halic_usda", FALSE, ref = ref))
  w <- .sgav_vert_window(pedon, 100, above_duripan)
  ec <- .usda_col(h, "ec_dS_m", NA_real_)
  em <- .usda_col(h, "ec_max_dS_m", NA_real_)
  ok <- ifelse(!is.na(ec), ec >= 15, ifelse(!is.na(em) & em < 15, FALSE, NA))
  th <- .sgav_run(h, ok, w[["top"]], w[["bottom"]])
  passed <- .sgav_ge(th, 15)
  .sgav_result("sg_vertisols_halic_usda", passed, layers = which(ok %in% TRUE),
               evidence = list(ec_dS_m = ec, run_cm = th, window = w),
               missing = "ec_dS_m", ref = ref)
}

#' Sodic subgroups of the Vertisols (KST 13th ed., pp. 345-354)
#'
#' In one or more horizons within 100 cm of the mineral soil surface (above
#' the duripan in the Durixererts), an exchangeable sodium percentage of 15
#' or more (or an SAR of 13 or more) for 6 or more months in normal years;
#' the measured ESP (\code{esp_pct}, or Na / CEC) is read.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param above_duripan Read above the duripan.
#' @noRd
sg_vertisols_sodic_usda <- function(pedon, above_duripan = FALSE) {
  h <- pedon$horizons
  ref <- .sgav_ref("pp. 345-354 (e.g. Sodic Haplusterts, p. 351)")
  if (!nrow(h)) return(.sgav_result("sg_vertisols_sodic_usda", FALSE, ref = ref))
  w <- .sgav_vert_window(pedon, 100, above_duripan)
  inw <- which(!is.na(h$top_cm) & h$top_cm < w[["bottom"]] & h$bottom_cm > w[["top"]])
  esp <- .usda_esp(h)[inw]
  passed <- .usda_any3(esp >= 15)
  if (!length(inw)) passed <- FALSE
  .sgav_result("sg_vertisols_sodic_usda", passed, layers = inw[(esp >= 15) %in% TRUE],
               evidence = list(esp = esp, window = w),
               missing = c("esp_pct", "na_cmol", "cec_cmol"), ref = ref)
}

#' Aeric subgroups of the Aquerts (KST 13th ed., pp. 344-346)
#'
#' In one or more horizons between an Ap horizon or 25 cm from the mineral
#' soil surface, whichever is deeper, and 75 cm (or the top of the duripan,
#' if shallower), 50\% or more colours (the moist matrix colour) of: (1) hue
#' of 2.5Y or redder and either a moist value of 6 or more and chroma of 3
#' or more, or a moist value of 5 or less and chroma of 2 or more; or (2) hue
#' of 5Y and chroma of 3 or more; or (3) chroma of 2 or more and no redox
#' concentrations (a field observation).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_vertisols_aeric_usda <- function(pedon) {
  h <- pedon$horizons
  ref <- .sgav_ref("pp. 344-346")
  if (!nrow(h)) return(.sgav_result("sg_vertisols_aeric_usda", FALSE, ref = ref))
  ms <- .sgav_mss(h)
  d <- .usda_desig(h$designation)
  ap <- !is.na(d$master) & d$master == "A" & !is.na(d$suffix) & grepl("p", d$suffix)
  top <- max(c(ms + 25, h$bottom_cm[ap]), na.rm = TRUE)
  w <- .sgav_vert_window(pedon, 75, above_duripan = TRUE)
  cand <- which(!is.na(h$top_cm) & h$top_cm < w[["bottom"]] & h$bottom_cm > top)
  hr <- .mo_hue_rank(h$munsell_hue_moist); vm <- h$munsell_value_moist; cm <- h$munsell_chroma_moist
  one <- function(i) {
    if (is.na(cm[i])) return(NA)
    rc <- isTRUE(.mo_redox_conc(h, i))
    c3 <- cm[i] >= 2 && !rc
    if (c3) return(TRUE)
    if (is.na(hr[i])) return(NA)
    c1 <- hr[i] >= 1 && hr[i] <= 6 &&
          (if (is.na(vm[i])) NA else ((vm[i] >= 6 && cm[i] >= 3) || (vm[i] <= 5 && cm[i] >= 2)))
    c2 <- hr[i] == 7 && cm[i] >= 3
    .usda_any3(c(c1, c2))
  }
  v <- vapply(cand, one, NA)
  passed <- if (!length(cand)) FALSE else .usda_any3(v)
  .sgav_result("sg_vertisols_aeric_usda", passed, layers = cand[v %in% TRUE],
               evidence = list(window_top_cm = top, window_bottom_cm = w[["bottom"]]),
               missing = c("munsell_hue_moist", "munsell_value_moist", "munsell_chroma_moist"),
               ref = ref)
}

#' Leptic subgroups of the Vertisols (KST 13th ed., pp. 345-354)
#'
#' A densic, lithic or paralithic contact within 100 cm of the mineral soil
#' surface (the soil surface in the Torrerts); with \code{duripan = TRUE} also
#' a duripan, and with \code{petrocalcic = TRUE} also a petrocalcic horizon,
#' within that depth. Contacts are read from the designations (field
#' observations).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param duripan A duripan also qualifies.
#' @param petrocalcic A petrocalcic horizon also qualifies.
#' @noRd
sg_vertisols_leptic_usda <- function(pedon, duripan = FALSE, petrocalcic = FALSE) {
  h <- pedon$horizons
  ref <- .sgav_ref("pp. 345-354 (e.g. Leptic Haplusterts, p. 352)")
  if (!nrow(h)) return(.sgav_result("sg_vertisols_leptic_usda", FALSE, ref = ref))
  ms <- .sgav_mss(h)
  ct <- .sgav_contact_top(h)
  v <- c(contact = !is.na(ct) && ct < ms + 100)
  miss <- character(0)
  if (duripan) {
    r <- duripan_usda(pedon, max_top_cm = ms + 100)
    v <- c(v, duripan = .usda_pv(r)); miss <- c(miss, r$missing)
  }
  if (petrocalcic) {
    r <- petrocalcic_subgroup_usda(pedon, max_top_cm = ms + 100)
    v <- c(v, petrocalcic = .usda_pv(r)); miss <- c(miss, r$missing)
  }
  passed <- .usda_any3(v)
  .sgav_result("sg_vertisols_leptic_usda", passed, evidence = as.list(v),
               missing = miss, ref = ref)
}

# Clay of each layer below 27 % (TRUE / FALSE / NA): measured clay, else the
# field texture class where its range decides it.
.sgav_clay_lt27 <- function(h) {
  cl <- h$clay_pct
  out <- ifelse(is.na(cl), NA, cl < 27)
  tc <- .usda_col(h, "texture_class_usda", NA_character_)
  if (any(is.na(out) & !is.na(tc))) {
    k <- .texture_class_reading(tc)$class
    lt <- c("S", "COS", "MS", "FS", "VFS", "US", "LS", "LCOS", "LFS", "LVFS",
            "SL", "COSL", "FSL", "VFSL", "L", "SIL", "SI")
    ge <- c("CL", "SICL", "SC", "SIC", "C")
    use <- is.na(out) & !is.na(k)
    out[use & k %in% lt] <- TRUE
    out[use & k %in% ge] <- FALSE
  }
  out
}

#' Entic subgroups of the Vertisols (KST 13th ed., pp. 345-354)
#'
#' A layer 25 cm or more thick within 100 cm of the mineral soil surface
#' (the soil surface in the Torrerts) with less than 27\% clay in its fine
#' earth. The Vertisol meaning of Entic (audit F16, R5): not the Spodosol
#' one. Clay is the measured \code{clay_pct}, else the field texture class
#' where its range decides.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_vertisols_entic_usda <- function(pedon) {
  h <- pedon$horizons
  ref <- .sgav_ref("pp. 345-354 (e.g. Entic Haplusterts, p. 352)")
  if (!nrow(h)) return(.sgav_result("sg_vertisols_entic_usda", FALSE, ref = ref))
  ms <- .sgav_mss(h)
  ok <- .sgav_clay_lt27(h)
  ok[.mo_is_organic(h)] <- FALSE
  th <- .sgav_run(h, ok, ms, ms + 100)
  passed <- .sgav_ge(th, 25)
  .sgav_result("sg_vertisols_entic_usda", passed, layers = which(ok %in% TRUE),
               evidence = list(run_cm = th), missing = c("clay_pct", "texture_class_usda"),
               ref = ref)
}

#' Chromic subgroups of the Vertisols (KST 13th ed., pp. 344-354)
#'
#' In one or more horizons within 30 cm of the mineral soil surface, 50\% or
#' more colours (the matrix colour) with a moist value of 4 or more, or a dry
#' value of 6 or more, or (\code{chroma = TRUE}) a moist chroma of 3 or more.
#' The Dur-, Dystr-, Endo- and Epiaquerts (\code{chroma = FALSE}) have only
#' the two value clauses. Colour not recorded is undecided; a moist value of
#' 2 or less rules out a dry value of 6 (reading L27).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param chroma Whether the chroma clause applies.
#' @noRd
sg_vertisols_chromic_usda <- function(pedon, chroma = TRUE) {
  h <- pedon$horizons
  ref <- .sgav_ref("pp. 344-354 (e.g. Chromic Haplusterts, p. 352)")
  if (!nrow(h)) return(.sgav_result("sg_vertisols_chromic_usda", FALSE, ref = ref))
  ms <- .sgav_mss(h)
  cand <- which(!is.na(h$top_cm) & h$top_cm < ms + 30 & h$bottom_cm > ms & !.mo_is_organic(h))
  vm <- h$munsell_value_moist[cand]; vd <- h$munsell_value_dry[cand]
  cm <- h$munsell_chroma_moist[cand]
  a <- vm >= 4
  b <- ifelse(!is.na(vd), vd >= 6, ifelse(!is.na(vm) & vm <= 2, FALSE, NA))
  cc <- if (chroma) cm >= 3 else rep(FALSE, length(cand))
  v <- vapply(seq_along(cand), function(k) .usda_any3(c(a[k], b[k], cc[k])), NA)
  passed <- if (!length(cand)) FALSE else .usda_any3(v)
  .sgav_result("sg_vertisols_chromic_usda", passed, layers = cand[v %in% TRUE],
               evidence = list(layers = cand, chroma_clause = chroma),
               missing = c("munsell_value_moist", "munsell_value_dry",
                           if (chroma) "munsell_chroma_moist"), ref = ref)
}

#' Aquic subgroups of the Vertisols (KST 13th ed., pp. 348-354)
#'
#' In one or more horizons within 100 cm of the mineral soil surface (the
#' soil surface in the Torrerts; above the duripan in the Durixererts),
#' aquic conditions for some time in normal years (or artificial drainage)
#' and redoximorphic features (or a positive dipyridyl reaction, which
#' soilKey does not record). Read from redox features, a g designation or a
#' reduced matrix (reading N10); field observations, absent unless recorded.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param above_duripan Read above the duripan.
#' @noRd
sg_vertisols_aquic_usda <- function(pedon, above_duripan = FALSE) {
  h <- pedon$horizons
  ref <- .sgav_ref("pp. 348-354 (e.g. Aquic Hapluderts, p. 349)")
  if (!nrow(h)) return(.sgav_result("sg_vertisols_aquic_usda", FALSE, ref = ref))
  w <- .sgav_vert_window(pedon, 100, above_duripan)
  cand <- which(!is.na(h$top_cm) & h$top_cm < w[["bottom"]] & h$bottom_cm > w[["top"]])
  v <- vapply(cand, function(i) .sgav_aquic_layer(h, i), NA)
  passed <- any(v)
  .sgav_result("sg_vertisols_aquic_usda", passed, layers = cand[v], ref = ref)
}

#' Sulfaqueptic Dystraquerts (KST 13th ed., p. 344): within 100 cm of the
#' mineral soil surface, jarosite concentrations (a field observation,
#' \code{jarosite_present}) and a pH of 4.0 or less (1:1 water).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_vertisols_sulfaqueptic_usda <- function(pedon) {
  h <- pedon$horizons
  ref <- .sgav_ref("p. 344")
  ms <- .sgav_mss(h)
  cand <- which(!is.na(h$top_cm) & h$top_cm < ms + 100 & h$bottom_cm > ms)
  ja <- as.logical(.usda_col(h, "jarosite_present", NA))[cand]
  ph <- .usda_col(h, "ph_h2o_1to1", NA_real_)[cand]
  ph0 <- .usda_col(h, "ph_h2o", NA_real_)[cand]
  ph[is.na(ph)] <- ph0[is.na(ph)]
  v <- ifelse(ja %in% TRUE, ph <= 4.0, FALSE)
  passed <- if (!length(cand)) FALSE else .usda_any3(v)
  .sgav_result("sg_vertisols_sulfaqueptic_usda", passed, layers = cand[v %in% TRUE],
               missing = "ph_h2o", ref = ref)
}


# ---- ANDISOLS (KST 13th ed., Ch. 6, pp. 117-136) ----------------------------

# The top of the Andisol window (.andic_top_usda()).
.sgav_t0 <- function(pedon) {
  if (!nrow(pedon$horizons)) 0 else .andic_top_usda(pedon)$top
}

#' Lithic subgroups of the Andisols (KST 13th ed., pp. 118-135): a lithic
#' contact within 50 cm of the mineral soil surface or of the top of an
#' organic layer with andic soil properties, whichever is shallower (the
#' mineral soil surface in the Torrands, p. 125).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param from_mineral Measure from the mineral soil surface.
#' @noRd
sg_andisols_lithic_usda <- function(pedon, from_mineral = FALSE) {
  h <- pedon$horizons
  ref <- .sgav_ref("pp. 118-135")
  if (!nrow(h)) return(.sgav_result("sg_andisols_lithic_usda", FALSE, ref = ref))
  t0 <- if (from_mineral) .sgav_mss(h) else .sgav_t0(pedon)
  L <- which(!is.na(h$designation) & grepl("^[0-9]*R($|[0-9])", h$designation) &
             !is.na(h$top_cm) & h$top_cm <= t0 + 50)
  .sgav_result("sg_andisols_lithic_usda", length(L) > 0L, layers = L,
               evidence = list(top_cm = t0), ref = ref)
}

#' Thaptic subgroups of the Andisols with their depth (KST 13th ed., pp.
#' 118-135)
#'
#' At a depth between \code{from_cm} (25, or 40 in the Melanaquands,
#' Fulvudands and Melanudands, pp. 120, 127, 131) and 100 cm from the top of
#' the Andisol window, a layer 10 cm or more thick with more than 3.0\%
#' organic carbon and the colours of a mollic epipedon, under horizons 10 cm
#' or more thick whose moist value is 1 unit or more higher and organic
#' carbon 1\% (absolute) or more lower. The reading of
#' \code{thaptic_subgroup_usda()}, with the upper limit as a parameter.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param from_cm Upper limit of the depth range.
#' @noRd
sg_andisols_thaptic_usda <- function(pedon, from_cm = 25) {
  h <- pedon$horizons
  ref <- .sgav_ref(if (from_cm == 25) "pp. 118-135" else "pp. 120, 127, 131")
  if (!nrow(h)) return(.sgav_result("sg_andisols_thaptic_usda", FALSE, ref = ref))
  t0 <- .sgav_t0(pedon)
  oc <- h$oc_pct; vm <- h$munsell_value_moist; cm <- h$munsell_chroma_moist
  vd <- h$munsell_value_dry
  res <- logical(0); hits <- integer(0)
  for (i in seq_len(nrow(h))) {
    th <- min(h$bottom_cm[i], t0 + 100) - max(h$top_cm[i], t0 + from_cm)
    if (is.na(th) || th < 10) next
    dark <- (oc[i] > 3) & (vm[i] <= 3) & (cm[i] <= 3) & (vd[i] <= 5)
    if (isFALSE(dark)) next
    up <- which(!is.na(h$bottom_cm) & h$bottom_cm <= h$top_cm[i] & h$bottom_cm > t0)
    lt <- (vm[up] >= vm[i] + 1) & (oc[up] <= oc[i] - 1)
    tk <- pmax(h$bottom_cm[up] - pmax(h$top_cm[up], t0), 0)
    yes <- sum(tk[lt %in% TRUE]); may <- sum(tk[is.na(lt)])
    over <- if (yes >= 10) TRUE else if (yes + may < 10) FALSE else NA
    r <- .usda_all3(c(dark, over))
    res <- c(res, r)
    if (isTRUE(r)) hits <- c(hits, i)
  }
  passed <- if (length(hits)) TRUE else if (all(res %in% FALSE)) FALSE else NA
  .sgav_result("sg_andisols_thaptic_usda", passed, layers = hits,
               evidence = list(top_cm = t0, from_cm = from_cm),
               missing = c("oc_pct", "munsell_value_moist", "munsell_chroma_moist",
                           "munsell_value_dry"), ref = ref)
}

#' Acraquoxic and Acrudoxic subgroups of the Andisols (KST 13th ed., pp.
#' 118-131)
#'
#' A sum of extractable bases (NH4OAc) plus 1 N KCl-extractable Al of less
#' than 2.0 cmol(+)/kg in the fine earth of horizons 30 cm or more thick in
#' all, between 25 cm and 100 cm from the top of the Andisol window (or the
#' coherent, pedogenically cemented horizon or the placic horizon, with
#' \code{to = "cemented"} or \code{"placic"}, pp. 126, 131). The sum is that
#' of Ca, Mg, K, Na and KCl Al where measured, else the ECEC.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param to "100", "cemented" or "placic".
#' @noRd
sg_andisols_acric_usda <- function(pedon, to = c("100", "cemented", "placic")) {
  to <- match.arg(to)
  h <- pedon$horizons
  ref <- .sgav_ref("pp. 118-131 (e.g. Acrudoxic Hapludands, p. 128)")
  if (!nrow(h)) return(.sgav_result("sg_andisols_acric_usda", FALSE, ref = ref))
  t0 <- .sgav_t0(pedon)
  bot <- t0 + 100
  if (to == "cemented") {
    cm <- which(.usda_cemented(h) & !is.na(h$top_cm) & h$top_cm >= t0)
    if (length(cm)) bot <- min(h$top_cm[cm])
  } else if (to == "placic") {
    pl <- .sgav_top(placic_horizon_usda(pedon, max_top_cm = Inf), h)
    if (!is.na(pl)) bot <- pl
  }
  s <- .sgav_bases_al(h)
  ok <- s < 2.0
  ok[.mo_is_organic(h)] <- FALSE
  th <- .sgav_thk(h, ok, t0 + 25, bot)
  passed <- .sgav_ge(th, 30)
  .sgav_result("sg_andisols_acric_usda", passed, layers = which(ok %in% TRUE),
               evidence = list(window = c(t0 + 25, bot), thickness_cm = th),
               missing = c("ca_cmol", "mg_cmol", "k_cmol", "na_cmol", "al_kcl_cmol", "ecec_cmol"),
               ref = ref)
}

#' Alic subgroups of the Andisols, and their negation in the Eutric ones
#' (KST 13th ed., pp. 118-129)
#'
#' More than 2.0 cmol(+)/kg Al (1 N KCl) in the fine earth of horizons 10 cm
#' or more thick in all between 25 and 50 cm from the top of the Andisol
#' window (or the lithic contact, if shallower: Eutric Lithic Fulvudands,
#' p. 126). The Eutric subgroups of the Duricryands, Fulvicryands, Durudands
#' and Fulvudands are those with no such horizons (\code{none_of} this test
#' in the YAML).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_andisols_alic_usda <- function(pedon) {
  h <- pedon$horizons
  ref <- .sgav_ref("pp. 118-129 (e.g. Alic Hapludands, p. 127)")
  if (!nrow(h)) return(.sgav_result("sg_andisols_alic_usda", FALSE, ref = ref))
  t0 <- .sgav_t0(pedon)
  bot <- t0 + 50
  ct <- .sgav_contact_top(h)
  if (!is.na(ct) && ct > t0 + 25) bot <- min(bot, ct)
  al <- .sgav_al_kcl(h)
  ok <- al > 2.0
  ok[.mo_is_organic(h)] <- FALSE
  th <- .sgav_thk(h, ok, t0 + 25, bot)
  passed <- .sgav_ge(th, 10)
  .sgav_result("sg_andisols_alic_usda", passed, layers = which(ok %in% TRUE),
               evidence = list(window = c(t0 + 25, bot), thickness_cm = th),
               missing = c("al_kcl_cmol", "al_cmol"), ref = ref)
}

#' Eutric subgroups of the Hapludands, Hydrudands and Melanudands (KST 13th
#' ed., pp. 129-132)
#'
#' A sum of extractable bases (NH4OAc) of more than 25.0 cmol(+)/kg in the
#' fine earth throughout horizons 15 cm or more thick in all between 25 and
#' 75 cm from the top of the Andisol window.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_andisols_eutric_bases_usda <- function(pedon) {
  h <- pedon$horizons
  ref <- .sgav_ref("pp. 129-132 (e.g. Eutric Hapludands, p. 129)")
  if (!nrow(h)) return(.sgav_result("sg_andisols_eutric_bases_usda", FALSE, ref = ref))
  t0 <- .sgav_t0(pedon)
  ok <- .sgav_bases(h) > 25
  ok[.mo_is_organic(h)] <- FALSE
  th <- .sgav_thk(h, ok, t0 + 25, t0 + 75)
  passed <- .sgav_ge(th, 15)
  .sgav_result("sg_andisols_eutric_bases_usda", passed, layers = which(ok %in% TRUE),
               evidence = list(thickness_cm = th),
               missing = c("ca_cmol", "mg_cmol", "k_cmol", "na_cmol"), ref = ref)
}

#' Dystric subgroups of the Haplustands (KST 13th ed., pp. 133-134)
#'
#' A sum of extractable bases (NH4OAc) plus 1 N KCl-extractable Al of less
#' than 15.0 cmol(+)/kg in the fine earth throughout horizons 60 cm or more
#' thick in all within 75 cm of the top of the Andisol window.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_andisols_dystric_usda <- function(pedon) {
  h <- pedon$horizons
  ref <- .sgav_ref("pp. 133-134")
  if (!nrow(h)) return(.sgav_result("sg_andisols_dystric_usda", FALSE, ref = ref))
  t0 <- .sgav_t0(pedon)
  ok <- .sgav_bases_al(h) < 15
  th <- .sgav_thk(h, ok, t0, t0 + 75)
  passed <- .sgav_ge(th, 60)
  .sgav_result("sg_andisols_dystric_usda", passed, layers = which(ok %in% TRUE),
               evidence = list(thickness_cm = th),
               missing = c("ca_cmol", "mg_cmol", "k_cmol", "na_cmol", "al_kcl_cmol", "ecec_cmol"),
               ref = ref)
}

#' Pachic subgroups of the Andisols (KST 13th ed., pp. 119-136)
#'
#' More than 6.0\% organic carbon and the colours of a mollic epipedon (moist
#' value 3 or less and chroma 3 or less, dry value 5 or less) throughout a
#' layer 50 cm or more thick within 60 cm of the top of the Andisol window.
#' With \code{fulvic = TRUE} (Pachic Fulvicryands and Fulvudands, pp. 122,
#' 127): more than 6.0\% organic carbon by weighted average and more than
#' 4.0\% in all parts of that layer, no colour.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param fulvic The Fulvi- wording.
#' @noRd
sg_andisols_pachic_usda <- function(pedon, fulvic = FALSE) {
  h <- pedon$horizons
  ref <- .sgav_ref(if (fulvic) "pp. 122, 127" else "pp. 119-136 (e.g. Pachic Melanudands, p. 131)")
  if (!nrow(h)) return(.sgav_result("sg_andisols_pachic_usda", FALSE, ref = ref))
  t0 <- .sgav_t0(pedon)
  oc <- h$oc_pct
  if (fulvic) {
    ok <- oc > 4
    ov <- pmax(pmin(h$bottom_cm, t0 + 60) - pmax(h$top_cm, t0), 0); ov[is.na(ov)] <- 0
    runs <- .usda_runs(!(ok %in% FALSE), order(h$top_cm))
    res <- vapply(runs, function(r) {
      r <- r[ov[r] > 0]
      if (sum(ov[r]) < 50) return(FALSE)
      if (anyNA(oc[r])) return(NA)
      sum(ov[r] * oc[r]) / sum(ov[r]) > 6
    }, NA)
    passed <- if (!length(res)) FALSE else .usda_any3(res)
  } else {
    vm <- h$munsell_value_moist; cm <- h$munsell_chroma_moist; vd <- h$munsell_value_dry
    vdd <- ifelse(!is.na(vd), vd <= 5, ifelse(!is.na(vm) & vm <= 2, TRUE, NA))
    ok <- vapply(seq_len(nrow(h)), function(i)
      .usda_all3(c(oc[i] > 6, vm[i] <= 3, cm[i] <= 3, vdd[i])), NA)
    th <- .sgav_run(h, ok, t0, t0 + 60)
    passed <- .sgav_ge(th, 50)
  }
  .sgav_result("sg_andisols_pachic_usda", passed, layers = which(ok %in% TRUE),
               evidence = list(top_cm = t0, fulvic = fulvic),
               missing = c("oc_pct", if (!fulvic) c("munsell_value_moist",
                           "munsell_chroma_moist", "munsell_value_dry")), ref = ref)
}

#' Vitric subgroups of the Andisols (KST 13th ed., pp. 122-135)
#'
#' A 1500 kPa water retention of less than 15\% on air-dried samples and
#' less than 30\% on undried samples throughout one or more layers with
#' andic soil properties, 25 cm or more thick in all, within 100 cm of the
#' top of the Andisol window. Reads \code{water_content_1500kpa} as the
#' air-dried value (reading L4) and the undried value where recorded (it
#' does not count against the layer where it is not), and the andic status
#' of each layer (\code{.andic_layer_status_usda()}).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_andisols_vitric_usda <- function(pedon) {
  h <- pedon$horizons
  ref <- .sgav_ref("pp. 122-135 (e.g. Vitric Hapludands, p. 128)")
  if (!nrow(h)) return(.sgav_result("sg_andisols_vitric_usda", FALSE, ref = ref))
  t0 <- .sgav_t0(pedon)
  wr <- h$water_content_1500kpa
  wru <- .usda_col(h, "water_content_1500kpa_undried", NA_real_)
  w <- (wr < 15) & ifelse(is.na(wru), TRUE, wru < 30)
  w[!is.na(wru) & wru >= 30] <- FALSE
  an <- .andic_layer_status_usda(h)$status
  ok <- vapply(seq_len(nrow(h)), function(i) .usda_all3(c(w[i], an[i])), NA)
  th <- .sgav_thk(h, ok, t0, t0 + 100)
  passed <- .sgav_ge(th, 25)
  .sgav_result("sg_andisols_vitric_usda", passed, layers = which(ok %in% TRUE),
               evidence = list(thickness_cm = th),
               missing = c("water_content_1500kpa", "andic soil properties"), ref = ref)
}

#' Aquic subgroups of the Andisols other than the Aquands (KST 13th ed., pp.
#' 121-135)
#'
#' In one or more horizons between 50 and 100 cm from the top of the Andisol
#' window (above the coherent, pedogenically cemented horizon in the
#' Durudands, \code{to = "cemented"}, p. 126; between 50 cm and the placic
#' horizon in the Placudands, \code{to = "placic"}, p. 132), aquic
#' conditions and 2\% or more redox concentrations, or a moist value of 4 or
#' more with 50\% or more chroma of 2 or less in redox depletions or in the
#' matrix. Read from \code{redoximorphic_features_pct} (2\% or more; redox
#' features recorded without an amount are undecided) and from a g
#' designation or reduced matrix with a moist value of 4 or more and chroma
#' of 2 or less; field observations, absent unless recorded.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param to "100", "cemented" or "placic".
#' @noRd
sg_andisols_aquic_usda <- function(pedon, to = c("100", "cemented", "placic")) {
  to <- match.arg(to)
  h <- pedon$horizons
  ref <- .sgav_ref("pp. 121-135 (e.g. Aquic Hapludands, p. 128)")
  if (!nrow(h)) return(.sgav_result("sg_andisols_aquic_usda", FALSE, ref = ref))
  t0 <- .sgav_t0(pedon)
  from <- t0 + 50; bot <- t0 + 100
  if (to == "cemented") {
    cmn <- which(.usda_cemented(h) & !is.na(h$top_cm) & h$top_cm >= t0)
    from <- t0; bot <- if (length(cmn)) min(h$top_cm[cmn]) else t0 + 100
  } else if (to == "placic") {
    pl <- .sgav_top(placic_horizon_usda(pedon, max_top_cm = Inf), h)
    if (!is.na(pl)) bot <- pl
  }
  cand <- which(!is.na(h$top_cm) & h$top_cm < bot & h$bottom_cm > from)
  rdx <- .usda_col(h, "redoximorphic_features_pct", NA_real_)
  d <- .usda_desig(h$designation)
  red <- .usda_col(h, "reduced_matrix_pct", NA_real_)
  vm <- h$munsell_value_moist; cm <- h$munsell_chroma_moist
  one <- function(i) {
    a <- if (!is.na(rdx[i])) rdx[i] >= 2 else if (isTRUE(.mo_redox_conc(h, i))) NA else FALSE
    gl <- (!is.na(d$suffix[i]) && grepl("g", d$suffix[i])) || (!is.na(red[i]) && red[i] > 0)
    b <- if (!gl) FALSE else .usda_all3(c(vm[i] >= 4, cm[i] <= 2))
    .usda_any3(c(a, b))
  }
  v <- vapply(cand, one, NA)
  passed <- if (!length(cand)) FALSE else .usda_any3(v)
  .sgav_result("sg_andisols_aquic_usda", passed, layers = cand[v %in% TRUE],
               evidence = list(window = c(from, bot)),
               missing = c("redoximorphic_features_pct", "munsell_value_moist",
                           "munsell_chroma_moist"), ref = ref)
}

#' Duric subgroups of the Andisols (KST 13th ed., pp. 118-128)
#'
#' A horizon 15 cm or more thick with 20\% or more (by volume) cemented soil
#' material within 100 cm of the top of the Andisol window. Cemented: a
#' cementation class recorded, or an m in the designation (continuous,
#' which is more than 20\%); its share is \code{duripan_pct} or
#' \code{lateral_cover_pct}, undecided where a recorded cemented layer has
#' none and no m.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_andisols_duric_usda <- function(pedon) {
  h <- pedon$horizons
  ref <- .sgav_ref("pp. 118-128 (e.g. Duric Hapludands, p. 128)")
  if (!nrow(h)) return(.sgav_result("sg_andisols_duric_usda", FALSE, ref = ref))
  t0 <- .sgav_t0(pedon)
  d <- .usda_desig(h$designation)
  m <- !is.na(d$suffix) & grepl("m", d$suffix)
  cem <- .usda_cemented(h) | m
  share <- .usda_col(h, "duripan_pct", NA_real_)
  lat <- .usda_col(h, "lateral_cover_pct", NA_real_)
  share <- ifelse(is.na(share), lat, share)
  ok <- ifelse(!cem, FALSE, ifelse(!is.na(share), share >= 20, ifelse(m, TRUE, NA)))
  L <- which(!(ok %in% FALSE) & !is.na(h$top_cm) & h$top_cm < t0 + 100)
  v <- vapply(L, function(i) {
    tk <- h$bottom_cm[i] - h$top_cm[i]
    if (is.na(tk) || tk < 15) FALSE else ok[i]
  }, NA)
  passed <- if (!length(L)) FALSE else .usda_any3(v)
  .sgav_result("sg_andisols_duric_usda", passed, layers = L[v %in% TRUE],
               missing = "duripan_pct", ref = ref)
}

#' Spodic subgroups of the Andisols (KST 13th ed., pp. 122, 124): an albic
#' horizon over a cambic horizon in 50\% or more of each pedon, or a spodic
#' horizon in 50\% or more of each pedon (the USDA spodic, albic and cambic
#' horizons, \code{spodic_horizon_usda()}, \code{albic_horizon_usda()},
#' \code{cambic_horizon_usda()}).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_andisols_spodic_usda <- function(pedon) {
  h <- pedon$horizons
  ref <- .sgav_ref("pp. 122, 124")
  sp <- spodic_horizon_usda(pedon)
  al <- albic_horizon_usda(pedon)
  av <- .usda_pv(al)
  ab <- if (isFALSE(av)) FALSE else {
    cb <- cambic_horizon_usda(pedon)
    cv <- .usda_pv(cb)
    if (isTRUE(av) && isTRUE(cv)) {
      at <- .sgav_top(al, h); ct <- .sgav_top(cb, h)
      if (is.na(at) || is.na(ct)) NA else ct > at
    } else .usda_all3(c(av, cv))
  }
  passed <- .usda_any3(c(.usda_pv(sp), ab))
  .sgav_result("sg_andisols_spodic_usda", passed,
               layers = c(sp$layers, al$layers),
               evidence = list(spodic = .usda_pv(sp), albic_over_cambic = ab),
               missing = c(sp$missing, al$missing), ref = ref)
}

#' Alfic subgroups of the Andisols (KST 13th ed., pp. 124-136): an argillic
#' or kandic horizon within 125 cm of the top of the Andisol window.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_andisols_alfic_usda <- function(pedon) {
  h <- pedon$horizons
  ref <- .sgav_ref("pp. 124-136 (e.g. Alfic Hapludands, p. 129)")
  if (!nrow(h)) return(.sgav_result("sg_andisols_alfic_usda", FALSE, ref = ref))
  t0 <- .sgav_t0(pedon)
  ak <- argillic_or_kandic_usda(pedon)
  passed <- .usda_within3(ak, h, t0 + 125)
  .sgav_result("sg_andisols_alfic_usda", passed, layers = ak$layers,
               missing = ak$missing, ref = ref)
}

#' Oxic subgroups of the Andisols (KST 13th ed., pp. 129, 134): an oxic
#' horizon within 125 cm of the top of the Andisol window.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_andisols_oxic_usda <- function(pedon) {
  h <- pedon$horizons
  ref <- .sgav_ref("pp. 129, 134")
  if (!nrow(h)) return(.sgav_result("sg_andisols_oxic_usda", FALSE, ref = ref))
  t0 <- .sgav_t0(pedon)
  ox <- oxic_horizon_usda(pedon)
  passed <- .usda_within3(ox, h, t0 + 125)
  .sgav_result("sg_andisols_oxic_usda", passed, layers = ox$layers,
               missing = ox$missing, ref = ref)
}

#' Anthraquic subgroups of the Andisols (KST 13th ed., pp. 127, 130):
#' anthraquic conditions (the anthric saturation of flooded rice, p. 38).
#' Field records: an anthraquic or anthrostagnic moisture regime record,
#' puddled layers, paddy years (\code{site$paddy_years_frac}) or a land use
#' naming paddy rice; absent unless recorded.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_andisols_anthraquic_usda <- function(pedon) {
  h <- pedon$horizons
  ref <- .sgav_ref("pp. 38, 127, 130")
  s <- pedon$site
  raw <- tolower(as.character(s$soil_moisture_regime %||% NA_character_)[1])
  pdy <- suppressWarnings(as.numeric(s$paddy_years_frac %||% NA_real_))[1]
  lu <- tolower(as.character(s$land_use %||% NA_character_)[1])
  pud <- as.logical(.usda_col(h, "puddled", NA))
  passed <- (!is.na(raw) && grepl("anthraquic|anthrostagnic", raw)) ||
            (!is.na(pdy) && pdy > 0) ||
            (!is.na(lu) && grepl("paddy", lu)) || any(pud %in% TRUE)
  .sgav_result("sg_andisols_anthraquic_usda", passed, ref = ref)
}


# ---- a diagnostic within a depth --------------------------------------------

#' A diagnostic horizon of another helper whose upper boundary is within a
#' depth (KST 13th ed.: e.g. "an argillic horizon within 100 cm", Xeralfic
#' Petrocalcids, p. 148; "a natric horizon within 100 cm", Natric
#' Argicryids, p. 153): TRUE when the helper passes with a layer starting
#' above \code{max_top_cm} below the mineral soil surface (or the top of the
#' Andisol window, \code{andic_top = TRUE}), FALSE when it fails or its
#' layers start deeper, NA when it is undecided. A general wrapper, for any
#' helper that returns the layers of the horizon.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param fn Name of the helper (a soilKey function of the pedon).
#' @param max_top_cm Depth (cm).
#' @param andic_top Measure from the top of the Andisol window.
#' @noRd
sg_usda_within_usda <- function(pedon, fn, max_top_cm = 100, andic_top = FALSE) {
  h <- pedon$horizons
  f <- get(fn, envir = asNamespace("soilKey"), mode = "function")
  r <- f(pedon)
  t0 <- if (!nrow(h)) 0 else if (andic_top) .sgav_t0(pedon) else .sgav_mss(h)
  passed <- .usda_within3(r, h, t0 + max_top_cm)
  .sgav_result(paste0("sg_within_", fn), passed, layers = r$layers,
               evidence = list(helper = fn, max_top_cm = max_top_cm),
               missing = r$missing %||% character(0),
               ref = r$reference %||% .sgav_ref("subgroup keys"))
}
