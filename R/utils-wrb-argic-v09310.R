# =============================================================================
# v0.9.310 -- shared readings for the WRB 2022 argic family: the argic, natric
# and nitic horizons as subsurface horizons, the exchangeable Al against the
# bases of the Acrisol and Alisol keys, and the ESP of the natric horizon.
# =============================================================================

#' Layers that cannot be part of a subsurface diagnostic horizon: the layer at
#' the mineral soil surface and the A horizons (A, Ah2, Ap2, A1g; the material
#' number and the older notation read by .desg_master()). WRB 2022 calls the
#' argic (Ch 3.1.3, p. 35), natric (3.1.21, p. 53) and nitic (3.1.22, p. 55)
#' horizons subsurface horizons; Hugo's reading (2026-10-29) is that a layer
#' at the surface or an A horizon is not part of them. Transitional horizons
#' (AB, AE, A/B) are not excluded. Logical, one per layer.
#' @noRd
.wrb_not_subsurface <- function(h) {
  n <- nrow(h)
  if (!n) return(logical(0))
  ms <- .q_mineral_surface_cm(h)
  surface <- if (is.na(ms)) rep(FALSE, n)
             else !is.na(h$top_cm) & h$top_cm <= ms + 1e-6
  d <- .desg_master(h$designation %||% rep(NA_character_, n))
  a_horizon <- !is.na(d) & grepl("^A[a-z0-9]*$", d)
  surface | a_horizon
}

#' The pH below which exchangeable Al exceeds the exchangeable bases, by WRB
#' 2022 Annex 2 (Ch 9.13, "Relationships between cations", Table 9.3, p. 221):
#' the pH at which exchangeable (Ca+Mg+K+Na) = exchangeable Al, for soil
#' organic carbon < 2, 2 to < 7.5 and 7.5 to < 20%.
#' @noRd
.WRB_PH_AL_EQ_BASES <- list(water = c(4.6, 4.9, 5.4), kcl = c(3.8, 4.1, 4.6))

#' Per layer: exchangeable Al > exchangeable (Ca+Mg+K+Na)? TRUE, FALSE or NA,
#' with the source read. The cations first: al_cmol against the sum of the
#' bases recorded (as .wrb_acidity_fracs() reads them for Dystric and Eutric),
#' or against the sum of the bases given by the base saturation and the CEC
#' (bs_pct x cec_cmol / 100, WRB 2022 Annex 2 Ch 9.13) when no single base is
#' recorded; else al_sat_pct > 50. When the cations are not available, the pH
#' in water and then in KCl by Table 9.3 (WRB 2022 Ch 4, footnotes 8 and 9:
#' "If these data are not available, pH values may be used according to Annex
#' 2"; Hugo's reading N5). A pH between the limits of the carbon classes, with
#' the carbon unknown, is undecided; a pH at the limit is Al = bases, so not Al
#' > bases.
#' @noRd
.wrb_al_gt_bases <- function(h) {
  n <- nrow(h)
  col <- function(v) h[[v]] %||% rep(NA_real_, n)
  al <- col("al_cmol"); als <- col("al_sat_pct")
  bases <- cbind(col("ca_cmol"), col("mg_cmol"), col("k_cmol"), col("na_cmol"))
  bs <- col("bs_pct"); cec <- col("cec_cmol")
  oc <- col("oc_pct"); phw <- col("ph_h2o"); phk <- col("ph_kcl")
  status <- rep(NA, n); source <- rep(NA_character_, n)
  for (i in seq_len(n)) {
    b <- bases[i, ]
    if (!is.na(al[i]) && any(!is.na(b))) {
      status[i] <- round(al[i] - sum(b, na.rm = TRUE), 9) > 0; source[i] <- "cations"
    } else if (!is.na(al[i]) && !is.na(bs[i]) && !is.na(cec[i])) {
      status[i] <- round(al[i] - bs[i] * cec[i] / 100, 9) > 0; source[i] <- "al_cmol, bs_pct x cec_cmol"
    } else if (!is.na(als[i])) {
      status[i] <- als[i] > 50; source[i] <- "al_sat_pct"
    } else {
      for (k in c("water", "kcl")) {
        ph <- if (k == "water") phw[i] else phk[i]
        if (is.na(ph)) next
        lim <- .WRB_PH_AL_EQ_BASES[[k]]
        cls <- if (is.na(oc[i])) NA_integer_
               else if (oc[i] < 2) 1L else if (oc[i] < 7.5) 2L
               else if (oc[i] < 20) 3L else NA_integer_
        status[i] <- if (!is.na(cls)) ph < lim[cls]
                     else if (ph < min(lim)) TRUE
                     else if (ph >= max(lim)) FALSE else NA
        source[i] <- paste0("pH_", k, " (WRB 2022 Table 9.3)")
        break
      }
    }
  }
  list(status = status, source = source)
}

#' Exchangeable Al > exchangeable (Ca+Mg+K+Na) "in half or more of: a. the
#' depth range between 50 and 100 cm of the mineral soil surface; or b. the
#' lower half of the mineral soil above a limiting layer starting <= 100 cm
#' from the mineral soil surface, whichever is shallower" (WRB 2022 Ch 4,
#' Acrisols p. 119 and Alisols p. 121; the Acric, Alic, Lixic and Luvic
#' qualifiers, Ch 5). Returns \code{al_gt} (Al > bases in half or more) and
#' \code{al_le} (Al <= bases in half or more, the Lixic and Luvic reading),
#' each TRUE, FALSE or NA. A part of the depth range that is not described is
#' undecided.
#' @noRd
.wrb_al_dominance <- function(pedon) {
  h <- pedon$horizons
  ms <- .q_mineral_surface_cm(h)
  if (is.na(ms)) ms <- 0
  lim <- tryCatch(.barrier_top_cm(pedon), error = function(e) NA_real_)
  if (!is.na(lim) && lim - ms <= 100) {
    lo <- ms + (lim - ms) / 2; hi <- lim
  } else {
    lo <- ms + 50; hi <- ms + 100
  }
  width <- hi - lo
  if (!is.finite(width) || width <= 0)
    return(list(al_gt = NA, al_le = NA, window = c(lo, hi), layers_gt = integer(0),
                layers_le = integer(0), source = character(0)))
  ov <- pmax(pmin(h$bottom_cm, hi) - pmax(h$top_cm, lo), 0)
  ov[is.na(ov)] <- 0
  st <- .wrb_al_gt_bases(h)
  yes <- sum(ov[st$status %in% TRUE]); no <- sum(ov[st$status %in% FALSE])
  unknown <- width - yes - no
  half <- width / 2 - 1e-9
  tri <- function(a, u) if (a >= half) TRUE else if (a + u < half) FALSE else NA
  inwin <- which(ov > 0)
  list(al_gt = tri(yes, unknown), al_le = tri(no, unknown), window = c(lo, hi),
       layers_gt = inwin[st$status[inwin] %in% TRUE],
       layers_le = inwin[st$status[inwin] %in% FALSE],
       source = unique(stats::na.omit(st$source[inwin])))
}

#' The ESP of each layer for the natric horizon (WRB 2022 Annex 2, Ch 9.13:
#' "exchangeable Na x 100 / CEC"). When the EC is >= 4 dS/m, the exchangeable
#' Na extracted by ammonium acetate includes soluble Na, and only the ESP the
#' laboratory reports (esp_pct) is read (Hugo's reading N10). Otherwise the ESP
#' is computed from na_cmol and cec_cmol, and esp_pct is read when one of them
#' is not given.
#' @noRd
.natric_esp <- function(h) {
  n <- nrow(h)
  col <- function(v) h[[v]] %||% rep(NA_real_, n)
  na <- col("na_cmol"); cec <- col("cec_cmol"); lab <- col("esp_pct"); ec <- col("ec_dS_m")
  calc <- ifelse(!is.na(na) & !is.na(cec) & cec > 0, 100 * na / cec, NA_real_)
  ifelse(!is.na(ec) & ec >= 4, lab, ifelse(is.na(calc), lab, calc))
}
