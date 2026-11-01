# =============================================================================
# v0.9.326 -- the 27 groups of the Comprehensive Soil Classification System of
# Japan, First Approximation (CSCJ 2011, chapter VI, pp. 30-45). Within a
# great group each group after the first is "soils other than the above",
# and the last is the default. The groups the CSCJ shares with the SCSJ 2017
# are its gates read by the CSCJ edition, under CSCJ names; new: the Fulvic
# Andosols (the brown polyhumic kuroboku surface horizon) and the Calcaric
# and Dystric Dark Red soils. Three-valued.
# =============================================================================

#' @noRd
artifactual_soils_cscj <- .cscj_shared(function(p) artifactual_soils_scsj(p), "artifactual_soils_cscj", "VI A, p. 30")
#' @noRd
regosolic_andosols_cscj <- .cscj_shared(function(p) regosolic_andosols_scsj(p), "regosolic_andosols_cscj", "VI D, p. 31")
#' @noRd
groundwater_gley_group_cscj <- .cscj_shared(function(p) groundwater_gley_group_scsj(p), "groundwater_gley_group_cscj", "VI D, F")
#' @noRd
groundwater_wet_group_cscj <- .cscj_shared(function(p) groundwater_wet_group_scsj(p), "groundwater_wet_group_cscj", "VI D, F")
#' @noRd
non_allophanic_andosols_cscj <- .cscj_shared(function(p) non_allophanic_andosols_scsj(p), "non_allophanic_andosols_cscj", "VI D, p. 32")
#' @noRd
lowland_paddy_soils_cscj <- .cscj_shared(function(p) fluvic_paddy_soils_scsj(p), "lowland_paddy_soils_cscj", "VI F, p. 36")
#' @noRd
brown_lowland_soils_cscj <- .cscj_shared(function(p) brown_fluvic_soils_scsj(p), "brown_lowland_soils_cscj", "VI F, p. 37")
#' @noRd
argic_group_cscj <- .cscj_shared(function(p) argic_group_scsj(p), "argic_group_cscj", "VI G, p. 38")
#' @noRd
stagnogley_soils_cscj <- .cscj_shared(function(p) stagnogley_soils_scsj(p), "stagnogley_soils_cscj", "VI H, p. 40")
#' @noRd
volcanogenous_regosols_cscj <- .cscj_shared(function(p) volcanogenous_regosols_scsj(p), "volcanogenous_regosols_cscj", "VI J, p. 43")
#' @noRd
sandy_regosols_cscj <- .cscj_shared(function(p) sandy_regosols_scsj(p), "sandy_regosols_cscj", "VI J, p. 43")
#' @noRd
lithosols_cscj <- .cscj_shared(function(p) lithosols_scsj(p), "lithosols_cscj", "VI J, p. 44")

# ---- D. Andosols: the brown polyhumic kuroboku surface horizon ---------------

# The weighted mean of `v` (weights `w`) over the sure rows and a subset of
# the maybe rows, at its lowest (dir = -1) or highest (dir = 1): maybe rows
# are added, most extreme first, while they move the mean that way.
.cscj_wmean_bound <- function(v_sure, w_sure, v_maybe, w_maybe, dir) {
  o <- order(dir * v_maybe, decreasing = TRUE)
  v_maybe <- v_maybe[o]; w_maybe <- w_maybe[o]
  sv <- sum(v_sure * w_sure); sw <- sum(w_sure)
  if (sw == 0 && length(v_maybe)) { sv <- v_maybe[1L] * w_maybe[1L]; sw <- w_maybe[1L]; v_maybe <- v_maybe[-1L]; w_maybe <- w_maybe[-1L] }
  if (sw == 0) return(NA_real_)
  for (k in seq_along(v_maybe)) {
    if (dir * v_maybe[k] > dir * sv / sw) { sv <- sv + v_maybe[k] * w_maybe[k]; sw <- sw + w_maybe[k] } else break
  }
  sv / sw
}

# The brown polyhumic kuroboku surface horizon (CSCJ IV.1, p. 19): surface
# horizons (A, or undesignated; not buried, mineral) with the kuroboku
# feature, moist value 3 or more and chroma 3 or more, and 4 % organic carbon
# or more each, `thick` cm or more in all within `within` cm of the soil
# surface (30 in 40 cm; the Cumulic subgroup 50 in 60 cm), their weighted
# mean carbon 6 % or more. The layers that meet every requirement are added
# up; no allowance for layers that fail between them (Hugo, 2026-11-01).
# Three-valued: the thickness and the mean are bounded over the layers that
# may meet them.
.cscj_brown_polyhumic <- function(x, within = 40, thick = 30) {
  if (is.na(x$surface)) return(NA)
  v <- x$colour$value; c <- x$colour$chroma
  brownish <- .scsj_and(ifelse(is.na(v), NA, v >= 3), ifelse(is.na(c), NA, c >= 3))
  surf <- ifelse(is.na(x$role), TRUE, grepl("^A", x$role)) & !x$des$buried
  oc4 <- ifelse(is.na(x$oc), NA, x$oc >= 4)
  meets <- .scsj_and(.scsj_and(.scsj_and(x$kb, brownish), .scsj_and(oc4, x$mineral)), surf)
  meets <- ifelse(!is.na(x$dtop) & x$dtop < within, meets, FALSE)
  th <- .scsj_at_least(.scsj_cum(x, meets, 0, within), thick)
  w <- pmax(0, pmin(ifelse(is.na(x$dbot), within, x$dbot), within) - pmax(x$dtop, 0))
  sure <- which(meets %in% TRUE & !is.na(w)); maybe <- which(is.na(meets) & !is.na(w))
  oc_lo <- ifelse(is.na(x$oc), 4, x$oc); oc_hi <- ifelse(is.na(x$oc), 100, x$oc)
  lo <- .cscj_wmean_bound(oc_lo[sure], w[sure], oc_lo[maybe], w[maybe], -1)
  hi <- .cscj_wmean_bound(oc_hi[sure], w[sure], oc_hi[maybe], w[maybe], 1)
  mean6 <- if (is.na(lo) && is.na(hi)) FALSE else if (!is.na(lo) && lo >= 6) TRUE else if (!is.na(hi) && hi < 6) FALSE else NA
  # a mean over no sure layer is not yet decided
  if (isTRUE(mean6) && !length(sure)) mean6 <- NA
  .scsj_and(th, mean6)
}

# Fulvic Andosols (D4): the brown polyhumic kuroboku surface horizon.
fulvic_andosols_cscj <- function(pedon) {
  x <- .scsj_profile(.cscj_marked(pedon))
  if (is.na(x$surface)) return(.cscj_result("fulvic_andosols_cscj", NA, integer(0), list(), "oc_pct", "VI D, p. 32"))
  passed <- .cscj_brown_polyhumic(x, 40, 30)
  .cscj_result("fulvic_andosols_cscj", passed, integer(0), list(brown_polyhumic_kuroboku_surface = passed),
               if (is.na(passed)) c("oc_pct", "munsell_value_moist", "munsell_chroma_moist", "phosphate_absorption_coef")
               else character(0), "VI D, p. 32")
}

# ---- E. Dark Red soils ------------------------------------------------------------

# Calcaric Dark Red soils (E1): from limestone or calcareous deposits, every
# subhorizon of the subsurface at pH(H2O) 5.5 or more or base saturation 50 %
# or more.
calcaric_dark_red_cscj <- function(pedon) {
  x <- .scsj_profile(.cscj_marked(pedon))
  if (is.na(x$surface)) return(.cscj_result("calcaric_dark_red_cscj", NA, integer(0), list(), "ph_h2o", "VI E, p. 35"))
  lime <- .cscj_limestone_origin(x)
  bs <- .scsj_base_status(x)
  passed <- .scsj_and(lime, bs$high)
  .cscj_result("calcaric_dark_red_cscj", passed, bs$rows, list(limestone_origin = lime, base_rich_subsurface = bs$high),
               if (is.na(passed)) c("ph_h2o", "bs_pct") else character(0), "VI E, p. 35")
}

# Dystric Dark Red soils (E2): at least one subhorizon of the subsurface at
# pH(H2O) under 5.5 (pH only). Where liming is recorded, a pH of 5.5 or more
# may come from it: undecided (Hugo, 2026-11-01; the Eutric group's text
# excludes a pH raised by fertilising).
dystric_dark_red_cscj <- function(pedon) {
  x <- .scsj_profile(.cscj_marked(pedon))
  if (is.na(x$surface)) return(.cscj_result("dystric_dark_red_cscj", NA, integer(0), list(), "ph_h2o", "VI E, p. 35"))
  cs <- .scsj_control(x)
  ph <- .scsj_col(x$h, "ph_h2o")[cs$rows]
  low <- if (!length(cs$rows)) NA else .scsj_any(ifelse(is.na(ph), NA, ph < 5.5))
  if (!isTRUE(cs$covered) && isFALSE(low)) low <- NA
  if (isFALSE(low) && isTRUE(as.logical(x$site$acid_neutralising_amendment %||% NA)[1L])) low <- NA
  .cscj_result("dystric_dark_red_cscj", low, cs$rows, list(acid_subhorizon = low),
               if (is.na(low)) c("ph_h2o", "acid_neutralising_amendment") else character(0), "VI E, p. 35")
}
