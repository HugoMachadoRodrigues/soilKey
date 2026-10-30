# =============================================================================
# WRB 2022 mollic family (Chernozems, Kastanozems, Phaeozems, Umbrisols):
# shared helpers (v0.9.309) for
#   * base saturation, measured, summed from the bases or read from pH;
#   * the dark surface stack of the mollic and umbric horizons;
#   * the carbonate layer of the Chernozem and Kastanozem gates.
# =============================================================================


# One cell of a horizon column, NA when the column is absent.
.wrb_hcell <- function(h, col, i) {
  x <- h[[col]]
  if (is.null(x)) return(NA)
  x[i]
}

# Base saturation (1 M NH4OAc, pH 7) of layer i in %, or NA. WRB 2022 Annex 2,
# Ch 9.13 (p. 220): BS is exchangeable (Ca+Mg+K+Na) x 100 / CEC. The recorded
# bs_pct first, else the four bases over cec_cmol.
.wrb_bs_value <- function(h, i) {
  v <- suppressWarnings(as.numeric(.wrb_hcell(h, "bs_pct", i)))
  if (!is.na(v)) return(v)
  cec <- suppressWarnings(as.numeric(.wrb_hcell(h, "cec_cmol", i)))
  b <- suppressWarnings(as.numeric(c(.wrb_hcell(h, "ca_cmol", i), .wrb_hcell(h, "mg_cmol", i),
                                     .wrb_hcell(h, "k_cmol", i), .wrb_hcell(h, "na_cmol", i))))
  if (!is.na(cec) && cec > 0 && !anyNA(b)) return(sum(b) / cec * 100)
  NA_real_
}

# BS >= 50% read from pH. WRB 2022 Annex 2, Ch 9.13, Table 9.2 (p. 220): where
# BS data are not available pHwater may be used, and pHKCl where pHwater is
# not available either. A BS of 50% corresponds to pHwater 5.0 / 5.3 / 5.7
# (pHKCl 4.0 / 4.5 / 5.0) at Corg < 2, 2 to < 7.5 and 7.5 to < 20%. With Corg
# not recorded the pH decides only where the three rows agree; at Corg >= 20%
# (organic material) the table does not apply. TRUE / FALSE / NA.
.wrb_bs50_from_ph <- function(oc, ph_h2o, ph_kcl) {
  oc <- suppressWarnings(as.numeric(oc))
  ph_h2o <- suppressWarnings(as.numeric(ph_h2o))
  ph_kcl <- suppressWarnings(as.numeric(ph_kcl))
  water <- !is.na(ph_h2o)
  ph <- if (water) ph_h2o else ph_kcl
  if (is.na(ph)) return(NA)
  thr <- if (water) c(5.0, 5.3, 5.7) else c(4.0, 4.5, 5.0)
  if (!is.na(oc)) {
    if (oc >= 20) return(NA)
    return(ph >= thr[if (oc < 2) 1L else if (oc < 7.5) 2L else 3L])
  }
  if (ph >= max(thr)) TRUE else if (ph < min(thr)) FALSE else NA
}

# Whether layer i has BS >= min_bs: list(passed, value, source). The pH
# reading of Table 9.2 answers the 50% threshold only.
.wrb_bs_status <- function(h, i, min_bs = 50) {
  v <- .wrb_bs_value(h, i)
  if (!is.na(v)) {
    return(list(passed = v >= min_bs, value = v, source = "base saturation"))
  }
  if (isTRUE(all.equal(min_bs, 50))) {
    ph_w <- .wrb_hcell(h, "ph_h2o", i)
    p <- .wrb_bs50_from_ph(.wrb_hcell(h, "oc_pct", i), ph_w, .wrb_hcell(h, "ph_kcl", i))
    if (!is.na(p)) {
      src <- if (!is.na(ph_w)) "pH water, Annex 2 Table 9.2" else "pH KCl, Annex 2 Table 9.2"
      return(list(passed = p, value = NA_real_, source = src))
    }
  }
  list(passed = NA, value = NA_real_, source = "not recorded")
}

# BS >= min_bs throughout `layers`: TRUE when every layer passes, FALSE when one
# fails, NA otherwise. No layer at all passes (nothing to test).
.wrb_bs_throughout <- function(h, layers, min_bs = 50) {
  layers <- layers[order(h$top_cm[layers])]
  st <- lapply(layers, function(i) .wrb_bs_status(h, i, min_bs))
  ok <- vapply(st, function(s) s$passed, NA)
  passed <- if (any(ok %in% FALSE)) FALSE else if (all(ok %in% TRUE)) TRUE else NA
  list(
    passed  = passed,
    layers  = layers,
    missing = if (anyNA(ok)) c("bs_pct", "ph_h2o") else character(0),
    details = data.frame(layer = layers, top_cm = h$top_cm[layers],
                         bottom_cm = h$bottom_cm[layers],
                         bs_pct = vapply(st, function(s) s$value, 0),
                         source = vapply(st, function(s) s$source, ""),
                         passed = ok, stringsAsFactors = FALSE)
  )
}

# Weighted average BS of `layers` below max_bs (umbric horizon): TRUE / FALSE /
# NA. A layer read from pH is only known to be at or above, or below, 50%, so it
# is bounded on its side; a layer with nothing recorded is bounded by 0 and 100.
.wrb_bs_weighted_below <- function(h, layers, max_bs = 50) {
  w <- pmax(h$bottom_cm[layers] - h$top_cm[layers], 0)
  ok <- !is.na(w) & w > 0
  layers <- layers[ok]; w <- w[ok]
  if (!length(layers)) {
    return(list(passed = NA, average = NA_real_, missing = c("top_cm", "bottom_cm")))
  }
  st <- lapply(layers, function(i) .wrb_bs_status(h, i, max_bs))
  v <- vapply(st, function(s) s$value, 0)
  p <- vapply(st, function(s) s$passed, NA)
  lo <- ifelse(!is.na(v), v, ifelse(p %in% TRUE, max_bs, 0))
  hi <- ifelse(!is.na(v), v, ifelse(p %in% FALSE, max_bs, 100))
  avg_lo <- sum(w * lo) / sum(w)
  avg_hi <- sum(w * hi) / sum(w)
  # a layer read below 50% by pH lies under its bound, so the average does too
  below_bound <- any(is.na(v) & p %in% FALSE)
  passed <- if (avg_hi < max_bs || (below_bound && avg_hi <= max_bs)) TRUE
            else if (avg_lo >= max_bs) FALSE
            else NA
  list(passed = passed,
       average = if (!anyNA(v)) sum(w * v) / sum(w) else NA_real_,
       bounds = c(avg_lo, avg_hi),
       missing = if (is.na(passed)) c("bs_pct", "ph_h2o") else character(0),
       details = data.frame(layer = layers, thickness_cm = w, bs_pct = v,
                            source = vapply(st, function(s) s$source, ""),
                            stringsAsFactors = FALSE))
}

# v0.9.317: weighted average BS of `layers` at or above min_bs (mollic horizon,
# WRB 2022 Ch 3.1.20 criterion 5, p. 53: ">= 50% on a weighted average"):
# TRUE / FALSE / NA, the counterpart of .wrb_bs_weighted_below(). A layer read
# from pH is bounded on its side of 50%; one with nothing recorded by 0 and 100.
.wrb_bs_weighted_above <- function(h, layers, min_bs = 50) {
  w <- pmax(h$bottom_cm[layers] - h$top_cm[layers], 0)
  ok <- !is.na(w) & w > 0
  layers <- layers[ok]; w <- w[ok]
  if (!length(layers)) {
    return(list(passed = NA, average = NA_real_, missing = c("top_cm", "bottom_cm")))
  }
  st <- lapply(layers, function(i) .wrb_bs_status(h, i, min_bs))
  v <- vapply(st, function(s) s$value, 0)
  p <- vapply(st, function(s) s$passed, NA)
  lo <- ifelse(!is.na(v), v, ifelse(p %in% TRUE, min_bs, 0))
  hi <- ifelse(!is.na(v), v, ifelse(p %in% FALSE, min_bs, 100))
  avg_lo <- sum(w * lo) / sum(w)
  avg_hi <- sum(w * hi) / sum(w)
  # a layer read below 50% by pH lies under its bound, so the average does too
  below_bound <- any(is.na(v) & p %in% FALSE)
  passed <- if (avg_lo >= min_bs) TRUE
            else if (avg_hi < min_bs || (below_bound && avg_hi <= min_bs)) FALSE
            else NA
  list(passed = passed,
       average = if (!anyNA(v)) sum(w * v) / sum(w) else NA_real_,
       bounds = c(avg_lo, avg_hi),
       missing = if (is.na(passed)) c("bs_pct", "ph_h2o") else character(0),
       details = data.frame(layer = layers, thickness_cm = w, bs_pct = v,
                            source = vapply(st, function(s) s$source, ""),
                            stringsAsFactors = FALSE))
}

# Layers overlapping the depth window [top, bottom), in depth order, and how
# much of the window they cover (cm).
.wrb_window_layers <- function(h, top, bottom) {
  ok <- !is.na(h$top_cm) & !is.na(h$bottom_cm) &
        h$top_cm < bottom & h$bottom_cm > top
  idx <- which(ok)
  idx <- idx[order(h$top_cm[idx])]
  cov <- sum(pmax(0, pmin(h$bottom_cm[idx], bottom) - pmax(h$top_cm[idx], top)))
  list(layers = idx, covered_cm = cov, window_cm = max(0, bottom - top))
}

# The dark surface stack of the mollic and umbric horizons: the layers that
# pass the colour test (test_mollic_color), contiguous from the mineral soil
# surface. WRB 2022 3.1.20 and 3.1.39 (pp. 52, 69): "a surface horizon
# consisting of mineral material"; Ch 2.1 rule 3 (p. 23): the mineral soil
# surface is the upper limit of the uppermost layer of mineral material, so
# organic layers on top are not part of it. `layers` pass; `maybe` goes on
# through layers whose colour is undecided (NA) until one fails.
.wrb_dark_surface_stack <- function(h, surface_top_cm = 5) {
  ms <- .q_mineral_surface_cm(h)
  out <- list(layers = integer(0), maybe = integer(0),
              mineral_surface_cm = ms, missing = character(0))
  if (is.na(ms)) return(out)
  ord <- order(h$top_cm)
  ord <- ord[!is.na(h$top_cm[ord]) & h$top_cm[ord] >= ms - 1e-6]
  undecided <- FALSE
  last_bottom <- NA_real_
  for (i in ord) {
    if (!length(out$maybe)) {
      if (h$top_cm[i] > ms + surface_top_cm) break
    } else if (is.na(last_bottom) || abs(h$top_cm[i] - last_bottom) > 1) {
      break
    }
    col <- test_mollic_color(h, candidate_layers = i)
    if (isFALSE(col$passed)) break
    out$maybe <- c(out$maybe, i)
    last_bottom <- h$bottom_cm[i]
    if (isTRUE(col$passed) && !undecided) {
      out$layers <- c(out$layers, i)
    } else {
      undecided <- TRUE
      out$missing <- unique(c(out$missing, col$missing))
    }
  }
  out
}

# Chernozems (Ch 4, p. 111) and Kastanozems (p. 112), criteria 2 and 3: "a
# layer with protocalcic properties, >= 5 cm thick, or a calcic horizon",
# starting at most `max_top_cm` cm deep "and, if present, above a petrocalcic
# horizon"; and "a base saturation ... of >= 50% from the mineral soil surface
# to the layer with protocalcic properties or to the calcic horizon,
# throughout". Every layer that is, or may be, such a layer is tried; one that
# starts at the mineral soil surface has nothing above it to test.
.wrb_carbonate_bs_gate <- function(pedon, max_top_cm, min_bs = 50) {
  h <- pedon$horizons
  n <- nrow(h)
  ms <- .q_mineral_surface_cm(h)
  if (is.na(ms)) ms <- 0
  pc  <- protocalcic_properties(pedon)
  cal <- calcic(pedon)
  st  <- pc$evidence$layer_status %||% rep(NA, n)
  # protocalcic layers >= 5 cm thick, adjoining layers taken together
  proto <- rep(FALSE, n)
  for (r in .q_layer_runs(h, which(st %in% TRUE)))
    if (r$bottom - r$top >= 5) proto[r$layers] <- TRUE
  for (r in .q_layer_runs(h, which(st %in% TRUE | is.na(st))))
    if (r$bottom - r$top >= 5) proto[r$layers[!proto[r$layers]]] <- NA
  # calcic horizon: its layers; undecided where CaCO3 is not recorded
  ca <- h$caco3_pct %||% rep(NA_real_, n)
  calc <- ifelse(seq_len(n) %in% (if (isTRUE(cal$passed)) cal$layers else integer(0)),
                 TRUE, ifelse(is.na(ca), NA, FALSE))
  carb <- vapply(seq_len(n), function(i) .any3(c(proto[i], calc[i])), NA)
  pk <- petrocalcic(pedon)
  petro_top <- if (isTRUE(pk$passed) && length(pk$layers))
                 min(h$top_cm[pk$layers], na.rm = TRUE) else Inf
  cand <- which(!(carb %in% FALSE) & !is.na(h$top_cm) &
                  h$top_cm <= max_top_cm + 1e-6 & h$top_cm < petro_top)
  cand <- cand[order(h$top_cm[cand])]
  rows <- lapply(cand, function(i) {
    above <- .wrb_window_layers(h, ms, h$top_cm[i])$layers
    bs <- .wrb_bs_throughout(h, above, min_bs)
    list(layer = i, carbonates = carb[i], bs = bs,
         passed = .and3(carb[i], bs$passed))
  })
  g <- vapply(rows, function(r) r$passed, NA)
  passed <- .any3(g)
  hit <- if (isTRUE(passed)) rows[[which(g %in% TRUE)[1]]] else NULL
  missing <- character(0)
  if (is.na(passed)) {
    if (any(vapply(rows, function(r) is.na(r$carbonates), NA)))
      missing <- c(missing, "secondary_carbonates_pct")
    if (any(vapply(rows, function(r) is.na(r$bs$passed), NA)))
      missing <- c(missing, "bs_pct", "ph_h2o")
  }
  list(
    passed  = passed,
    layer   = if (!is.null(hit)) hit$layer else integer(0),
    missing = missing,
    protocalcic = pc, calcic = cal,
    candidates = data.frame(
      layer = cand, top_cm = h$top_cm[cand],
      carbonate_layer = vapply(rows, function(r) r$carbonates, NA),
      bs_above = vapply(rows, function(r) r$bs$passed, NA),
      passed = g),
    bs_above = if (!is.null(hit)) hit$bs else NULL,
    max_top_cm = max_top_cm, mineral_surface_cm = ms,
    petrocalcic_top_cm = if (is.finite(petro_top)) petro_top else NA_real_
  )
}
