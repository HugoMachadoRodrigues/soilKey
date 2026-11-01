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

# The depth within [from, to] cm that no horizon describes: gaps between
# horizons, and below a description that stops (not on rock, not complete);
# a horizon without a bottom covers the depth below its top.
.cscj_undescribed <- function(x, from, to) {
  ok <- which(!is.na(x$dtop))
  if (!length(ok)) return(to - from)
  a <- pmax(x$dtop[ok], from); b <- pmin(ifelse(is.na(x$dbot[ok]), to, x$dbot[ok]), to)
  keep <- b > a
  segs <- cbind(a[keep], b[keep]); segs <- segs[order(segs[, 1]), , drop = FALSE]
  covered <- 0; reach <- from
  for (k in seq_len(nrow(segs))) {
    s1 <- max(segs[k, 1], reach); if (segs[k, 2] > s1) covered <- covered + (segs[k, 2] - s1)
    reach <- max(reach, segs[k, 2])
  }
  e <- .scsj_ends(x)
  rock <- if (is.finite(x$rock_top %||% NA)) x$rock_top else Inf
  end <- if (e$on_rock || isTRUE(x$complete)) min(rock, max(c(x$dbot[ok], x$dtop[ok]), na.rm = TRUE)) else rock
  max(0, min(to, end) - from - covered)
}

# The weighted mean of `v` (weights `w`) over the sure rows and a subset of
# the maybe rows, at its lowest (dir = -1) or highest (dir = 1): maybe rows
# are added, most extreme first, while they move the mean that way.
.cscj_wmean_bound <- function(v_sure, w_sure, v_maybe, w_maybe, dir) {
  o <- order(dir * v_maybe, decreasing = TRUE)
  v_maybe <- v_maybe[o]; w_maybe <- w_maybe[o]
  keep <- w_maybe > 0; v_maybe <- v_maybe[keep]; w_maybe <- w_maybe[keep]
  ks <- w_sure > 0; v_sure <- v_sure[ks]; w_sure <- w_sure[ks]
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
  # surface horizons: A (AB, A/B, AC included) or undesignated, not buried;
  # an unreadable designation may be one
  surf <- ifelse(x$des$buried %in% TRUE, FALSE,
          ifelse(is.na(x$role), TRUE, ifelse(x$role %in% "other", NA, grepl("^A", x$role))))
  oc4 <- ifelse(is.na(x$oc), NA, x$oc >= 4)
  meets <- .scsj_and(.scsj_and(.scsj_and(x$kb, brownish), .scsj_and(oc4, x$mineral)), surf)
  meets <- ifelse(!is.na(x$dtop) & x$dtop < within, meets, FALSE)
  th <- .scsj_at_least(.scsj_cum(x, meets, 0, within), thick)
  # the pieces the mean is taken over (the parts of the layers within the
  # depth): a closed row whole; a row without a bottom as a small certain
  # piece and a possible extension to the depth; undescribed depth (gaps,
  # below a description that stops) as a possible piece of 4 to 100 % carbon
  eps <- 0.01
  top <- pmax(x$dtop, 0); open <- is.na(x$dbot)
  w_full <- pmax(0, pmin(ifelse(open, within, x$dbot), within) - top)
  ok <- !is.na(w_full) & w_full > 0 & !(meets %in% FALSE)
  pc <- list(); pm <- list()
  add <- function(lst, v_lo, v_hi, w) c(lst, list(c(v_lo, v_hi, w)))
  for (i in which(ok)) {
    vlo <- if (is.na(x$oc[i])) 4 else x$oc[i]; vhi <- if (is.na(x$oc[i])) 100 else x$oc[i]
    if (open[i]) {
      first <- min(eps, w_full[i])
      if (meets[i] %in% TRUE) pc <- add(pc, vlo, vhi, first) else pm <- add(pm, vlo, vhi, first)
      if (w_full[i] > first) pm <- add(pm, vlo, vhi, w_full[i] - first)
    } else if (meets[i] %in% TRUE) pc <- add(pc, vlo, vhi, w_full[i]) else pm <- add(pm, vlo, vhi, w_full[i])
  }
  und <- .cscj_undescribed(x, 0, within)
  if (und > 0) pm <- add(pm, 4, 100, und)
  col <- function(lst, k) vapply(lst, `[`, numeric(1), k)
  lo <- .cscj_wmean_bound(col(pc, 1), col(pc, 3), col(pm, 1), col(pm, 3), -1)
  hi <- .cscj_wmean_bound(col(pc, 2), col(pc, 3), col(pm, 2), col(pm, 3), 1)
  sure <- pc
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
  # no subsurface: the criterion is not met (Hugo, 2026-11-01); depth the
  # description does not cover may hold another subhorizon (5 cm or more)
  low <- if (!length(cs$rows)) { if (isTRUE(cs$covered)) FALSE else NA } else .scsj_any(ifelse(is.na(ph), NA, ph < 5.5))
  if (isFALSE(low) && .cscj_undescribed(x, cs$from, cs$to) >= 5) low <- NA
  if (isFALSE(low) && isTRUE(.scsj_site_flag(x$site$acid_neutralising_amendment))) low <- NA
  .cscj_result("dystric_dark_red_cscj", low, cs$rows, list(acid_subhorizon = low),
               if (is.na(low)) c("ph_h2o", "acid_neutralising_amendment") else character(0), "VI E, p. 35")
}
