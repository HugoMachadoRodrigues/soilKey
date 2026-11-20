# =============================================================
# USDA Soil Taxonomy 13ed -- Ultisols helpers (Cap 15, pp 321-342)
# =============================================================
#
# Ultisols are soils with an argillic, kandic, or kandilic horizon
# AND base saturation < 35\% in some part. 5 Suborders by SMR/aquic.
# =============================================================


#' Ultisol Order qualifier (USDA, KST 13ed, Ch 2)
#' Pass when argillic OR kandic horizon present + BS < 35\% in some
#' part of the upper 200 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
ultisol_qualifying_usda <- function(pedon) {
  ar <- argillic_or_kandic_usda(pedon, max_top_cm = 200)
  if (!isTRUE(ar$passed)) {
    return(DiagnosticResult$new(
      name = "ultisol_qualifying_usda", passed = FALSE,
      layers = integer(0),
      evidence = list(reason = "no argillic or kandic"),
      missing = ar$missing,
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 15"
    ))
  }
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < 200)
  bs <- h$bs_pct[cand]
  miss <- if (all(is.na(bs))) "bs_pct" else character(0)
  low_bs <- any(!is.na(bs) & bs < 35)
  passed <- isTRUE(low_bs)
  DiagnosticResult$new(
    name = "ultisol_qualifying_usda", passed = passed,
    layers = ar$layers,
    evidence = list(argic_or_kandic = ar, low_bs = low_bs),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 15"
  )
}


#' Aquult Suborder qualifier
#' Pass when aquic_conditions within 50 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
aquult_qualifying_usda <- function(pedon) {
  res <- aquic_conditions_usda(pedon, max_top_cm = 50)
  res$name <- "aquult_qualifying_usda"
  res
}


# ---- Base saturation at the depth of the KST (v0.9.420) ----------------

# Base saturation by sum of cations, layer by layer: the bases over the CEC at
# pH 8.2 (or the bases plus the acidity at pH 8.2). Where neither is recorded
# the NH4OAc base saturation (bs_pct) stands in for it, and the source says so
# (audit reading L18).
.bs_sum_cations_usda <- function(pedon) {
  h <- pedon$horizons
  n <- nrow(h)
  col <- function(x) h[[x]] %||% rep(NA_real_, n)
  bases <- col("ca_cmol") + col("mg_cmol") + col("k_cmol") + col("na_cmol")
  cec82 <- col("cec_ph82_cmol")
  acid <- col("extractable_acidity_ph82_cmol")
  cec82 <- ifelse(is.na(cec82) & !is.na(acid) & !is.na(bases), bases + acid, cec82)
  soc <- ifelse(!is.na(bases) & !is.na(cec82) & cec82 > 0,
                pmin(100, 100 * bases / cec82), NA_real_)
  nh4 <- col("bs_pct")
  list(bs = ifelse(!is.na(soc), soc, nh4),
       source = ifelse(!is.na(soc), "sum of cations",
                       ifelse(!is.na(nh4), "NH4OAc", NA_character_)))
}

# The base saturation at depth d: the layer that contains it (at a contact,
# the layer directly above it). When the profile ends above d, the deepest
# layer with a base saturation recorded, marked as an inference (audit
# reading L19).
.bs_at_depth_usda <- function(pedon, d, at_contact = FALSE) {
  h <- pedon$horizons
  b <- .bs_sum_cations_usda(pedon)
  top <- h$top_cm; bot <- h$bottom_cm
  i <- if (at_contact) which(!is.na(bot) & abs(bot - d) < 1e-6) else integer(0)
  if (!length(i)) i <- which(!is.na(top) & top <= d & (is.na(bot) | bot > d))
  if (length(i)) {
    i <- i[1L]
    return(list(bs = b$bs[i], source = b$source[i], depth = d, inferred = FALSE))
  }
  deepest <- suppressWarnings(max(bot, na.rm = TRUE))
  if (is.finite(deepest) && deepest <= d) {
    k <- which(!is.na(b$bs))
    if (length(k)) {
      k <- k[which.max(top[k])]
      return(list(bs = b$bs[k], source = b$source[k], depth = d, inferred = TRUE))
    }
  }
  list(bs = NA_real_, source = NA_character_, depth = d, inferred = FALSE)
}

# Is the epipedon above an upper boundary `t` sandy throughout (coarse sand,
# sand, fine sand, loamy coarse sand, loamy sand or loamy fine sand)? The
# epipedon is read as the A horizons at the surface (the layer at the
# mineral soil surface when no A is designated). TRUE / FALSE / NA.
.usda_epipedon_sandy <- function(pedon, t) {
  h <- as.data.frame(pedon$horizons)
  o <- order(h$top_cm); hs <- h[o, , drop = FALSE]
  ms <- .q_mineral_surface_cm(pedon$horizons); if (is.na(ms)) ms <- 0
  m <- .desg_master(as.character(hs$designation), site = pedon$site)
  epi <- which(!is.na(hs$top_cm) & hs$top_cm >= ms & hs$top_cm < t & !is.na(m) & grepl("^A", m))
  if (!length(epi)) epi <- which(!is.na(hs$top_cm) & hs$top_cm >= ms & hs$top_cm < t)[1L]
  if (!length(epi) || is.na(epi[1L])) return(NA)
  s <- vapply(epi, function(i) .usda_sandy_layer(hs, i), NA)
  .and3(s)
}

# Item 1 (no fragipan) or item 2 (fragipan) of the Ultisols (KST p. 69):
# is the base saturation below 35 % at the depth the item names? `tops` are
# the upper boundaries of the horizon (several while it is undecided); the
# answer holds when every case agrees.
.ultisol_bs_item <- function(pedon, tops, fragipan = FALSE) {
  h <- pedon$horizons
  ms <- .q_mineral_surface_cm(h); if (is.na(ms)) ms <- 0
  contact <- .kst_contact_top_cm(pedon, petroferric = TRUE)
  if (!length(tops)) return(list(low = NA, bs = NA_real_, depth = NA_real_, source = NA_character_, inferred = FALSE))
  cases <- list()
  for (t in tops) {
    ds <- if (fragipan) min(c(t + 75, ms + 200, contact), na.rm = TRUE)
    else {
      db <- min(c(t + 125, ms + 180, contact), na.rm = TRUE)
      da <- max(min(t + 125, ms + 200), ms + 180)
      if (!is.na(contact) && contact < da) da <- contact
      sandy <- .usda_epipedon_sandy(pedon, t)
      if (isTRUE(sandy)) da else if (isFALSE(sandy)) db else unique(c(db, da))
    }
    for (d in ds) {
      r <- .bs_at_depth_usda(pedon, d, at_contact = !is.na(contact) && abs(d - contact) < 1e-6)
      r$low <- if (is.na(r$bs)) NA else .kst_round(r$bs) < 35
      cases[[length(cases) + 1L]] <- r
    }
  }
  lows <- vapply(cases, function(x) x$low, NA)
  out <- cases[[1L]]
  out$low <- if (all(lows %in% TRUE)) TRUE else if (all(lows %in% FALSE)) FALSE else NA
  out
}


# ---- Organic carbon mass (Humults, Humic Oxisols) ------------------------

# Organic carbon (kg/m2) between the mineral soil surface and `depth`, tested
# against `threshold`: TRUE / FALSE / NA. v0.9.420 (audit item O4): a layer
# with no bulk density recorded is not given a default (1.0 or 1.2 g/cm3
# until v0.9.419); the answer is TRUE when the layers that are known already
# reach the threshold, FALSE when even a bulk density of 2.0 g/cm3 in the
# layers without one cannot, and undecided otherwise. A profile that ends
# above `depth` (no contact) decides only a TRUE.
.oc_mass_usda <- function(pedon, depth = 100, threshold = 12) {
  h <- pedon$horizons
  ms <- .q_mineral_surface_cm(h); if (is.na(ms)) ms <- 0
  lo <- ms; hi <- ms + depth
  dz <- pmax(0, pmin(h$bottom_cm, hi) - pmax(h$top_cm, lo))
  dz[is.na(dz)] <- 0
  w <- which(dz > 0)
  oc <- h$oc_pct[w]; bd <- h$bulk_density_g_cm3 %||% rep(NA_real_, nrow(h)); bd <- bd[w]
  known <- sum((oc / 100 * bd * dz[w] * 10)[!is.na(oc) & !is.na(bd)])
  upper <- known + sum((oc / 100 * 2.0 * dz[w] * 10)[!is.na(oc) & is.na(bd)])
  covered <- sum(dz[w])
  contact <- .kst_contact_top_cm(pedon, petroferric = TRUE)
  complete <- covered >= depth - 1e-6 || (!is.na(contact) && contact <= hi)
  passed <- if (.kst_round(known) >= threshold) TRUE
            else if (anyNA(oc) || !complete) NA
            else if (.kst_round(upper) < threshold) FALSE else NA
  list(passed = passed, known_kg_m2 = known, upper_kg_m2 = upper,
       missing = if (is.na(passed)) unique(c(if (anyNA(bd)) "bulk_density_g_cm3",
                                             if (anyNA(oc)) "oc_pct",
                                             if (!complete) "bottom_cm")) else character(0))
}


#' Humult Suborder qualifier (Ultisols with thick humus accumulation)
#'
#' Passes when either criterion of KST 13ed key HB holds: (1) >= 0.9\% organic
#' carbon (weighted average) in the upper 15 cm of the argillic or kandic
#' horizon; or (2) >= 12 kg/m2 organic carbon between the mineral soil surface
#' and 100 cm. Criterion 1's 15 cm window is anchored at the illuvial onset
#' (the shallowest diagnostic layer whose clay exceeds the horizon directly
#' above it) so a transitional B with no clay increase cannot inflate it.
#' v0.9.420: three-valued; a missing bulk density is no longer 1.2 g/cm3
#' (audit item O4, \code{.oc_mass_usda()}), and the horizon is the USDA
#' argillic or kandic horizon.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
humult_qualifying_usda <- function(pedon) {
  h <- pedon$horizons
  # --- Criterion 2: OC mass in 0-100 cm ---
  c2 <- .oc_mass_usda(pedon, depth = 100, threshold = 12)
  # --- Criterion 1: 0.9% weighted-avg OC in upper 15 cm of argillic/kandic ---
  # The 15 cm window starts at the ILLUVIAL ONSET -- the shallowest diagnostic
  # layer whose clay exceeds the horizon immediately above it -- not at the
  # diagnostic's reported top.
  ar <- argillic_usda(pedon)
  kd <- kandic_horizon_usda(pedon)
  dx_layers <- if (isTRUE(ar$passed)) ar$layers else if (isTRUE(kd$passed)) kd$layers else integer(0)
  hor <- .usda_or3(ar$passed, kd$passed)
  oc_top15 <- NA_real_
  crit1 <- if (isFALSE(hor)) FALSE else NA
  if (length(dx_layers) > 0L) {
    dx_layers <- dx_layers[order(h$top_cm[dx_layers])]
    onset <- NA_integer_
    for (i in dx_layers) {
      above <- which(!is.na(h$bottom_cm) & abs(h$bottom_cm - h$top_cm[i]) < 1e-6)
      ca <- h$clay_pct[i]; cb <- if (length(above)) h$clay_pct[above[1L]] else NA_real_
      if (!is.na(ca) && !is.na(cb) && ca > cb) { onset <- i; break }
    }
    if (is.na(onset)) onset <- dx_layers[1L]
    z0 <- h$top_cm[onset]; z1 <- z0 + 15
    ov <- pmax(0, pmin(h$bottom_cm, z1) - pmax(h$top_cm, z0)); ov[is.na(ov)] <- 0
    w <- which(ov > 0)
    if (length(w) && !anyNA(h$oc_pct[w])) {
      oc_top15 <- sum(h$oc_pct[w] * ov[w]) / sum(ov[w])
      crit1 <- .kst_round(oc_top15, 1) >= 0.9
    } else crit1 <- NA
  }
  passed <- .usda_or3(crit1, c2$passed)
  DiagnosticResult$new(
    name = "humult_qualifying_usda", passed = passed,
    layers = if (isTRUE(crit1)) dx_layers else integer(0),
    evidence = list(oc_mass_kg_m2 = c2$known_kg_m2, oc_mass_upper_kg_m2 = c2$upper_kg_m2,
                    mass_threshold = 12, oc_pct_top15_dx = oc_top15, oc_pct_threshold = 0.9,
                    via_crit1 = isTRUE(crit1), via_crit2 = isTRUE(c2$passed)),
    missing = if (is.na(passed)) unique(c(c2$missing, if (is.na(crit1)) "oc_pct")) else character(0),
    reference = "Soil Survey Staff (2022), KST 13th ed., Ch. 15, Humults (p. 321)"
  )
}


#' Albic-over-argillic qualifying (Albaquults)
#' Pass when albic horizon overlies an argillic horizon directly.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
albaquult_qualifying_usda <- function(pedon) {
  al <- albic(pedon)
  arg <- argillic_within_usda(pedon, max_top_cm = 100)
  passed <- isTRUE(al$passed) && isTRUE(arg$passed)
  if (passed) {
    h <- pedon$horizons
    al_top <- min(h$top_cm[al$layers], na.rm = TRUE)
    arg_top <- min(h$top_cm[arg$layers], na.rm = TRUE)
    passed <- al_top < arg_top
  }
  DiagnosticResult$new(
    name = "albaquult_qualifying_usda", passed = passed,
    layers = c(al$layers, arg$layers),
    evidence = list(albic = al, argillic = arg),
    missing = unique(c(al$missing %||% character(0),
                          arg$missing %||% character(0))),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 15"
  )
}


# ---- Pale-, Kandi- and Kanhapl- great groups (v0.9.420) ---------------

# Hue "7.5YR or redder" / "2.5YR or redder" of a Munsell hue string.
.hue_redder_than <- function(hue, limit = c("7.5YR", "2.5YR")) {
  limit <- match.arg(limit)
  hue <- toupper(gsub("\\s+", "", as.character(hue)))
  set <- if (limit == "7.5YR") "^(7\\.5YR|5YR|2\\.5YR|10R|7\\.5R|5R|2\\.5R)$"
         else "^(2\\.5YR|10R|7\\.5R|5R|2\\.5R)$"
  ifelse(is.na(hue) | !nzchar(hue), NA, grepl(set, hue))
}

# The core of the Pale- and Kandi- great groups (KST 13th ed., pp. 86, 97,
# 109, 321-341): no contact within 150 cm of the mineral soil surface (densic,
# lithic, paralithic; and petroferric where `petroferric`), and within 150 cm
# no clay decrease with increasing depth of 20 % or more (relative) from the
# maximum clay content, unless the layer with the lower clay has 5 % or more
# skeletans (clay depletions) on ped faces (or, `plinthite`, 5 % or more
# plinthite: Palexerults) and a clay increase of 3 % (absolute) or more
# below it. With `apparent`, the clay is the measured clay or 2.5 x (water at
# 1500 kPa - organic carbon), whichever is higher (the Alfisol text). Skeletans
# and plinthite are field observations: absent when not recorded. A profile
# that ends above 150 cm without a contact is undecided (audit reading L20).
.pale_core_usda <- function(pedon, petroferric = TRUE, apparent = FALSE,
                            plinthite = FALSE) {
  h <- as.data.frame(pedon$horizons)
  ms <- .q_mineral_surface_cm(pedon$horizons); if (is.na(ms)) ms <- 0
  contact <- .kst_contact_top_cm(pedon, petroferric = petroferric)
  if (!is.na(contact) && contact < ms + 150)
    return(list(passed = FALSE, reason = sprintf("contact at %g cm", contact)))
  cl_all <- if (apparent) .apparent_clay_usda(pedon, factor = 2.5) else h$clay_pct
  w <- which(!is.na(h$top_cm) & h$top_cm < ms + 150 & h$top_cm >= ms)
  w <- w[order(h$top_cm[w])]
  if (!length(w)) return(list(passed = NA, reason = "no mineral layers", missing = "clay_pct"))
  cl <- cl_all[w]
  if (all(is.na(cl))) return(list(passed = NA, reason = "clay not recorded", missing = "clay_pct"))
  k <- which.max(cl); mx <- cl[k]
  sk <- h$skeletans_pct %||% rep(NA_real_, nrow(h))
  pl <- h$plinthite_pct %||% rep(NA_real_, nrow(h))
  below <- seq_along(w)[seq_along(w) > k]
  for (b in below) {
    if (is.na(cl[b])) next
    if (.kst_round(100 * (mx - cl[b]) / mx) >= 20) {
      i <- w[b]
      feat <- (!is.na(sk[i]) && sk[i] >= 5) || (plinthite && !is.na(pl[i]) && pl[i] >= 5)
      rise <- any(cl[seq_along(w) > b] - cl[b] >= 3, na.rm = TRUE)
      if (!(feat && rise))
        return(list(passed = FALSE, reason = sprintf("clay decrease of 20%% or more at %g cm", h$top_cm[i])))
    }
  }
  if (anyNA(cl)) return(list(passed = NA, reason = "clay not recorded in every layer within 150 cm", missing = "clay_pct"))
  deepest <- suppressWarnings(max(h$bottom_cm, na.rm = TRUE))
  if (is.na(contact) && (!is.finite(deepest) || deepest < ms + 150))
    return(list(passed = NA, reason = "the profile ends above 150 cm (audit reading L20)", missing = "bottom_cm"))
  list(passed = TRUE, reason = "no clay decrease of 20% or more within 150 cm")
}

# The horizon a Pale- test reads: the argillic horizon, or, with `kandic`,
# the argillic or kandic horizon (the kandic when the argillic is absent).
.pale_horizon_usda <- function(pedon, kandic = FALSE) {
  ar <- argillic_usda(pedon)
  if (!kandic || isTRUE(ar$passed)) return(ar)
  ka <- kandic_horizon_usda(pedon)
  if (isTRUE(ka$passed)) return(ka)
  r <- ar
  r$passed <- .usda_or3(ar$passed, ka$passed)
  r$missing <- unique(c(ar$missing, ka$missing))
  r
}

# In the lower one-half of the horizon, a subhorizon with hue 7.5YR or
# redder and chroma 5 or more (moist matrix). TRUE / FALSE / NA.
.pale_lower_half_colour <- function(pedon, layers) {
  h <- pedon$horizons
  L <- layers[order(h$top_cm[layers])]
  t0 <- min(h$top_cm[L]); t1 <- max(h$bottom_cm[L])
  if (is.na(t1)) return(NA)
  mid <- (t0 + t1) / 2
  lo <- L[h$bottom_cm[L] > mid]
  red <- .hue_redder_than(h$munsell_hue_moist[lo], "7.5YR")
  ch <- h$munsell_chroma_moist[lo]
  ok <- ifelse(is.na(red) | is.na(ch), NA, red & ch >= 5)
  .usda_or3(ok)
}

# Horizons totalling more than half the thickness of the horizon with hue
# 2.5YR or redder, value moist 3 or less and value dry 4 or less (Paleudalfs,
# item 3.b). TRUE / FALSE / NA.
.pale_red_half <- function(pedon, layers) {
  h <- pedon$horizons
  thk <- h$bottom_cm[layers] - h$top_cm[layers]
  if (anyNA(thk)) return(NA)
  red <- .hue_redder_than(h$munsell_hue_moist[layers], "2.5YR")
  vm <- h$munsell_value_moist[layers]; vd <- h$munsell_value_dry[layers]
  ok <- ifelse(red %in% FALSE | (!is.na(vm) & vm > 3) | (!is.na(vd) & vd > 4), FALSE,
               ifelse(is.na(red) | is.na(vm) | is.na(vd), NA, TRUE))
  yes <- sum(thk[ok %in% TRUE]); may <- sum(thk[!ok %in% FALSE])
  if (yes > sum(thk) / 2) TRUE else if (may > sum(thk) / 2) NA else FALSE
}

# Item 3 of the Paleustalfs and Palexeralfs (pp. 98, 109): no contact within
# 50 cm, the horizon has 35 % or more clay in a subhorizon of its upper part,
# and at its upper boundary or within it a clay increase of 20 % (absolute)
# within 7.5 cm (an abrupt or clear boundary) or 15 % within 2.5 cm (an
# abrupt one), or an abrupt textural change between the eluvial horizon and
# the horizon.
.pale_abrupt_route <- function(pedon, layers) {
  h <- as.data.frame(pedon$horizons)
  ms <- .q_mineral_surface_cm(pedon$horizons); if (is.na(ms)) ms <- 0
  contact <- .kst_contact_top_cm(pedon, petroferric = FALSE)
  if (!is.na(contact) && contact < ms + 50) return(FALSE)
  L <- layers[order(h$top_cm[layers])]
  up <- L[h$top_cm[L] < min(h$top_cm[L]) + 50]
  c35 <- .usda_or3(ifelse(is.na(h$clay_pct[up]), NA, .kst_round(h$clay_pct[up]) >= 35))
  if (isFALSE(c35)) return(FALSE)
  bd <- tolower(as.character(h$boundary_distinctness %||% rep(NA_character_, nrow(h))))
  pairs <- lapply(L, function(i) {
    a <- which(!is.na(h$bottom_cm) & abs(h$bottom_cm - h$top_cm[i]) < 1e-6)
    if (length(a)) c(a[1L], i) else NULL
  })
  jumps <- vapply(pairs, function(p) {
    if (is.null(p)) return(FALSE)
    over <- h$clay_pct[p[1L]]; here <- h$clay_pct[p[2L]]
    if (is.na(over) || is.na(here)) return(NA)
    d <- .kst_round(here - over); b <- bd[p[1L]]
    if (d < 15) return(.abrupt_textural_change_usda(over, here, b) %in% TRUE && p[2L] == L[1L])
    if (is.na(b) || !nzchar(b)) return(NA)
    if (grepl("abrupt", b)) return(TRUE)
    if (grepl("clear", b) && d >= 20) return(TRUE)
    if (p[2L] == L[1L]) return(.abrupt_textural_change_usda(over, here, b))
    FALSE
  }, NA)
  .and3(c35, .usda_or3(jumps))
}

# Palecryalfs and item 4 of the Paleudalfs (pp. 82, 86): the horizon starts
# 60 cm or more below the mineral soil surface, a horizon above it is finer
# than loamy fine sand, and there is a glossic horizon or interfingering of
# albic materials into it (glossic_subgroup_usda()). A volcanic surface
# mantle is not recorded, so it is read as absent.
.pale_glossic_route <- function(pedon, hz) {
  h <- pedon$horizons
  if (!isTRUE(hz$passed)) return(if (isFALSE(hz$passed)) FALSE else NA)
  ms <- .q_mineral_surface_cm(h); if (is.na(ms)) ms <- 0
  t <- min(h$top_cm[hz$layers])
  if (t < ms + 60) return(FALSE)
  above <- which(!is.na(h$top_cm) & h$top_cm >= ms & h$top_cm < t)
  hs <- as.data.frame(h)
  finer <- .usda_or3(vapply(above, function(i) { s <- .usda_sandy_layer(hs, i); if (is.na(s)) NA else !s }, NA))
  .and3(finer, glossic_subgroup_usda(pedon)$passed)
}

#' Pale qualifying helper (Pale- and Kandi- great groups)
#'
#' v0.9.420 (audit item G2): the criteria of each great group of the KST.
#' \code{kst = NULL} keeps the earlier reading (the Paleargid proxy), which
#' the Mollisol great groups still use. Otherwise:
#' \describe{
#'   \item{\code{"ultisols"}}{Paleaquults, Palehumults, Paleudults,
#'     Paleustults and the Kandi- great groups of the Ultisols (pp. 321-337):
#'     the core test, with the petroferric contact.}
#'   \item{\code{"xerults"}}{Palexerults (p. 340): the core test without the
#'     petroferric contact, plinthite as well as skeletans.}
#'   \item{\code{"kandi_alfisols"}}{Kandiudalfs, Kandiustalfs (pp. 86, 97):
#'     the core test with the petroferric contact and the apparent clay.}
#'   \item{\code{"udalfs"}}{Paleudalfs (p. 86): the core test (densic, lithic,
#'     paralithic contact) and an argillic horizon with, in its lower half,
#'     hue 7.5YR or redder and chroma 5 or more, or red colours in more than
#'     half of it; or the frigid glossic route.}
#'   \item{\code{"ustalfs"}}{Paleustalfs (p. 97): a petrocalcic horizon within
#'     150 cm; or the core test and the lower-half colour of the argillic
#'     horizon; or the abrupt clay increase of item 3.}
#'   \item{\code{"xeralfs"}}{Palexeralfs (p. 109): a petrocalcic horizon
#'     within 150 cm; or the core test and an argillic or kandic horizon whose
#'     base is at 150 cm or more; or the abrupt clay increase of item 3.}
#'   \item{\code{"cryalfs"}}{Palecryalfs (p. 82): the glossic route.}
#' }
#' Redox concentrations ("many coarse ...") are field observations that the
#' data rarely record: absent when not recorded. The clay decrease is read
#' only below the maximum clay content.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param kst The reading, see above.
#' @noRd
pale_qualifying_usda <- function(pedon, kst = NULL) {
  if (is.null(kst)) {
    res <- paleargid_qualifying_usda(pedon)
    res$name <- "pale_qualifying_usda"
    return(res)
  }
  kst <- match.arg(kst, c("ultisols", "xerults", "kandi_alfisols", "udalfs",
                          "ustalfs", "xeralfs", "cryalfs"))
  h <- pedon$horizons
  ref <- "Soil Survey Staff (2022), KST 13th ed., Pale- and Kandi- great groups (pp. 82-109, 321-341)"
  core <- switch(kst,
    ultisols = .pale_core_usda(pedon, petroferric = TRUE),
    xerults = .pale_core_usda(pedon, petroferric = FALSE, plinthite = TRUE),
    kandi_alfisols = .pale_core_usda(pedon, petroferric = TRUE, apparent = TRUE),
    cryalfs = list(passed = NA),
    .pale_core_usda(pedon, petroferric = FALSE, apparent = TRUE))
  passed <- switch(kst,
    ultisols = , xerults = , kandi_alfisols = core$passed,
    cryalfs = {
      ar <- argillic_usda(pedon); ka <- kandic_horizon_usda(pedon); na <- natric_horizon_usda(pedon)
      hz <- if (isTRUE(ar$passed)) ar else if (isTRUE(ka$passed)) ka else if (isTRUE(na$passed)) na
            else list(passed = .usda_or3(ar$passed, ka$passed, na$passed))
      .pale_glossic_route(pedon, hz)
    },
    udalfs = {
      ar <- argillic_usda(pedon)
      col <- if (isTRUE(ar$passed)) .usda_or3(.pale_lower_half_colour(pedon, ar$layers),
                                              .pale_red_half(pedon, ar$layers))
             else ar$passed
      frig <- {
        tr <- tolower(as.character(pedon$site$soil_temperature_regime %||% NA_character_))
        .and3(if (is.na(tr) || !nzchar(tr)) NA else tr == "frigid",
              .pale_glossic_route(pedon, ar))
      }
      .usda_or3(.and3(core$passed, ar$passed, col), frig)
    },
    ustalfs = {
      ar <- argillic_usda(pedon)
      pc <- isTRUE(petrocalcic_subgroup_usda(pedon, max_top_cm = 150)$passed)
      r2 <- if (isTRUE(ar$passed)) .and3(core$passed, .pale_lower_half_colour(pedon, ar$layers)) else ar$passed
      r3 <- if (isTRUE(ar$passed)) .pale_abrupt_route(pedon, ar$layers) else ar$passed
      .usda_or3(pc, r2, r3)
    },
    xeralfs = {
      hz <- .pale_horizon_usda(pedon, kandic = TRUE)
      pc <- isTRUE(petrocalcic_subgroup_usda(pedon, max_top_cm = 150)$passed)
      base150 <- if (isTRUE(hz$passed)) {
        b <- max(h$bottom_cm[hz$layers])
        ms <- .q_mineral_surface_cm(h); if (is.na(ms)) ms <- 0
        if (is.na(b)) NA else b >= ms + 150
      } else hz$passed
      r2 <- .and3(core$passed, base150)
      r3 <- if (isTRUE(hz$passed)) .pale_abrupt_route(pedon, hz$layers) else hz$passed
      .usda_or3(pc, r2, r3)
    })
  DiagnosticResult$new(
    name = "pale_qualifying_usda", passed = passed, layers = integer(0),
    evidence = list(kst = kst, core = core),
    missing = if (is.na(passed)) unique(c(core$missing %||% character(0))) else character(0),
    reference = ref)
}


#' Kanhapl qualifying helper (Kanhapludults / Kanhaplustults / etc.)
#'
#' v0.9.420 (audit item G2): "Other ... that have a kandic horizon", keyed
#' after the Kandi- great group; it was kandic and not Pale, which the
#' Kandi- great group (kandic only, until v0.9.419) left unreachable.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
kanhapl_qualifying_usda <- function(pedon) {
  res <- kandic_horizon_usda(pedon)
  res$name <- "kanhapl_qualifying_usda"
  res
}


#' Plinth qualifying helper (Plinth*ults)
#' Pass when plinthite >= 5\% in 50\%+ of layers within 150 cm.
#' v0.9.420 (\code{gg = TRUE}, the Plinth- great groups, audit item G3): a
#' horizon between \code{min_top_cm} and \code{max_top_cm} in which plinthite
#' forms a continuous phase or one-half or more of the volume (KST pp. 73,
#' 97, 109, 321-336), read as \code{plinthite_pct} of 50 or more. Plinthite is
#' a field observation: absent when not recorded.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Numeric threshold or option (see Details).
#' @param gg The great-group reading.
#' @param min_top_cm Upper limit of the depth range (30 for Plinthaqualfs).
#' @noRd
plinth_subgroup_usda <- function(pedon, max_top_cm = 150, gg = FALSE, min_top_cm = 0) {
  if (!isTRUE(gg)) {
    res <- plinthic_subgroup_usda(pedon, max_top_cm = max_top_cm)
    res$name <- "plinth_subgroup_usda"
    return(res)
  }
  h <- pedon$horizons
  ms <- .q_mineral_surface_cm(h); if (is.na(ms)) ms <- 0
  pp <- h$plinthite_pct %||% rep(NA_real_, nrow(h))
  layers <- which(!is.na(h$top_cm) & h$top_cm < ms + max_top_cm &
                    (is.na(h$bottom_cm) | h$bottom_cm > ms + min_top_cm) &
                    !is.na(pp) & pp >= 50)
  DiagnosticResult$new(
    name = "plinth_subgroup_usda", passed = length(layers) > 0L, layers = layers,
    evidence = list(min_top_cm = min_top_cm, max_top_cm = max_top_cm, threshold_pct = 50),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13th ed., Plinth- great groups (pp. 73, 97, 109, 321-336)")
}


#' Albic Subgroup helper (Albaquultic / Albaquic)
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
albic_subgroup_usda <- function(pedon) {
  res <- albic_horizon_usda(pedon)
  res$name <- "albic_subgroup_usda"
  res
}
