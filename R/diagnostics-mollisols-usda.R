# =============================================================
# USDA Soil Taxonomy 13ed -- Mollisols helpers (Ch 12, pp 247-294)
# =============================================================
#
# v0.9.421 (audit section 2.5, items M1 to M6; readings L19, L27 to L29,
# L33; Hugo, 2026-11-20): the mollic epipedon of KST 13th ed. pp. 14-16,
# the Mollisols of the order key (item I, pp. 69-70), the suborders of
# p. 247-248 (Albolls, Aquolls, Rendolls) and the Calci-, Pale- and Vermi-
# great groups (pp. 253-284), each three-valued: TRUE or FALSE where the
# recorded data decide, NA with the missing data listed where they do not.
# Field observations not recorded (redox features, slickensides, worm
# holes, effervescence) are absent; measurements, colour, structure and
# texture not recorded are undecided.
# =============================================================


# ---- Three-valued helpers -------------------------------------------------

.mo_and <- function(...) {
  v <- as.logical(unlist(list(...)))
  if (any(v %in% FALSE)) FALSE else if (all(v %in% TRUE)) TRUE else NA
}
.mo_or <- function(...) {
  v <- as.logical(unlist(list(...)))
  if (any(v %in% TRUE)) TRUE else if (all(v %in% FALSE)) FALSE else NA
}
.mo_not <- function(x) if (is.na(x)) NA else !x
.mo_col <- function(h, n, na = NA) if (is.null(h[[n]])) rep(na, nrow(h)) else h[[n]]

# A value known only as a range c(lo, hi); a recorded value is c(x, x).
.mo_rng <- function(x, lo, hi) if (is.na(x)) c(lo, hi) else c(x, x)
.mo_le <- function(r, t) if (r[2] <= t) TRUE else if (r[1] > t) FALSE else NA
.mo_ge <- function(r, t) if (r[1] >= t) TRUE else if (r[2] < t) FALSE else NA
# Weighted mean of a property over layers of weights w, as a range: the
# unrecorded layers take the bounds lo and hi (KST note 10, p. 56: mixing is
# a weighted average).
.mo_wrange <- function(x, w, lo, hi) {
  k <- !is.na(x); s <- sum(w)
  if (s <= 0) return(c(lo, hi))
  known <- sum(x[k] * w[k])
  c((known + lo * sum(w[!k])) / s, (known + hi * sum(w[!k])) / s)
}

# Designations of a densic, lithic or paralithic contact, a petrocalcic
# horizon or a duripan (the root-limiting layers the mollic is mixed above).
.MO_CONTACT_RX <- "^[0-9]*(R|Cr|Cd)($|[^a-z])|^[0-9]*C/R|km|qm"
.mo_is_contact <- function(h) {
  d <- as.character(.mo_col(h, "designation"))
  cem <- tolower(as.character(.mo_col(h, "cementation_class")))
  (!is.na(d) & grepl(.MO_CONTACT_RX, d)) |
    (!is.na(cem) & cem %in% c("strongly", "indurated"))
}
.mo_contact_top <- function(h) {
  x <- which(.mo_is_contact(h) & !is.na(h$top_cm))
  if (length(x)) min(h$top_cm[x]) else NA_real_
}
.mo_is_organic <- function(h) {
  d <- as.character(.mo_col(h, "designation"))
  oc <- .mo_col(h, "oc_pct")
  (!is.na(d) & grepl("^[0-9]*[OH]", d)) | (!is.na(oc) & oc >= 12)
}
# Lower boundary of the deepest layer described
.mo_described_to <- function(h) {
  b <- h$bottom_cm[!is.na(h$bottom_cm)]
  if (length(b)) max(b) else NA_real_
}

# Free carbonates (CaCO3 equivalent) of each layer as a range. Not measured,
# it is taken as nil where the layer is recorded without effervescence or
# its pH(H2O) is below 7, where calcium carbonate cannot persist; otherwise
# it is unknown (0 to 100).
.mo_caco3_known <- function(h) {
  ca <- .mo_col(h, "caco3_pct")
  ef <- .field_term(.mo_col(h, "effervescence_class"))
  ph <- .mo_col(h, "ph_h2o")
  nil <- is.na(ca) & ((!is.na(ef) & ef %in% c("none", "non-calcareous", "noncalcareous", "no")) |
                        (!is.na(ph) & ph < 7))
  ca[nil] <- 0
  ca
}

# Base saturation by NH4OAc of each layer: bs_pct, or the bases over the CEC
# at pH 7 where all four bases and the CEC are measured. The pH and Al
# saturation proxies are not used (L28).
.mo_bs <- function(h) {
  bs <- .mo_col(h, "bs_pct")
  ca <- .mo_col(h, "ca_cmol"); mg <- .mo_col(h, "mg_cmol")
  k <- .mo_col(h, "k_cmol"); na <- .mo_col(h, "na_cmol"); cec <- .mo_col(h, "cec_cmol")
  calc <- !is.na(ca) & !is.na(mg) & !is.na(k) & !is.na(na) & !is.na(cec) & cec > 0
  fill <- is.na(bs) & calc
  bs[fill] <- pmin(100, (ca + mg + k + na)[fill] / cec[fill] * 100)
  bs
}

# Texture of a layer "loamy fine sand or coarser" (TRUE), finer (FALSE), or
# unknown (NA): sands and loamy sands have silt + 2 x clay < 30. The field
# texture class stands in where clay and sand are not measured.
.mo_lfs_or_coarser <- function(h) {
  clay <- .mo_col(h, "clay_pct"); silt <- .mo_col(h, "silt_pct"); sand <- .mo_col(h, "sand_pct")
  silt <- ifelse(is.na(silt) & !is.na(clay) & !is.na(sand), 100 - clay - sand, silt)
  out <- ifelse(!is.na(clay) & !is.na(silt), silt + 2 * clay < 30,
                ifelse(!is.na(clay) & clay >= 15, FALSE, NA))
  tc <- h[["texture_class_usda"]]
  if (!is.null(tc)) {
    r <- .texture_class_reading(tc)
    use <- is.na(out) & is.na(clay) & is.na(sand) & !is.na(r$class)
    out[use] <- r$lfs_or_coarser[use]
  }
  as.logical(out)
}

# Dry rupture resistance harder than moderately hard. FAO (2006) "hard" is
# read as KST "moderately hard" (L33); "very hard" and harder fail.
.mo_harder_than_moderately_hard <- function(x) {
  x <- .field_term(x)
  ifelse(is.na(x), NA,
         x %in% c("very hard", "extremely hard", "rigid", "very rigid",
                  "hard to very hard", "very hard to extremely hard"))
}

# Structure and consistence of a layer (items 1 to 3, KST p. 15): fails
# only where the structural units are larger than 30 cm (massive) and the
# layer is harder than moderately hard when dry, where rock structure is
# recorded, or where it is fluid.
.mo_structure_ok <- function(h, i) {
  g <- .field_term(.mo_col(h, "structure_grade")[i])
  t <- .field_term(.mo_col(h, "structure_type")[i])
  hard <- .mo_harder_than_moderately_hard(.mo_col(h, "rupture_resistance")[i])
  rs <- .mo_col(h, "rock_structure_pct")[i]
  nv <- .mo_col(h, "n_value")[i]
  fl <- .field_term(.mo_col(h, "fluidity_class")[i])
  if ((!is.na(t) && grepl("rock structure|stratified", t)) || (!is.na(rs) && rs >= 50))
    return(FALSE)
  if ((!is.na(nv) && nv >= 0.7) || (!is.na(fl) && grepl("fluid", fl) && !grepl("non", fl)))
    return(FALSE)
  if (isFALSE(hard)) return(TRUE)
  massive <- if (!is.na(t)) grepl("massive|structureless", t)
             else if (!is.na(g)) grepl("structureless|massive", g) && !grepl("single", g)
             else NA
  if (!is.na(t) && grepl("single grain", t)) massive <- FALSE
  .mo_not(.mo_and(massive, hard))
}

# Colour of a layer or mixture (item 4, KST pp. 15-16), from ranges of the
# moist value, moist chroma, dry value and CaCO3 equivalent.
.mo_colour_ok <- function(vm, cm, vd, ca) {
  a <- .mo_and(.mo_le(vm, 3), .mo_le(cm, 3), .mo_le(vd, 5))
  b <- .mo_and(.mo_ge(ca, 15), .mo_le(ca, 40), .mo_le(vm, 3), .mo_le(cm, 3))
  c <- .mo_and(.mo_ge(ca, 40), .mo_le(vm, 5))
  .mo_or(a, b, c)
}

# Organic carbon (item 6a and 6c): 2.5 % where the moist value is 4 or 5,
# 0.6 % otherwise.
.mo_oc_ok <- function(oc, vm) {
  need25 <- if (vm[1] > 3) TRUE else if (vm[2] <= 3) FALSE else NA
  if (isTRUE(need25)) return(.mo_ge(oc, 2.5))
  if (isFALSE(need25)) return(.mo_ge(oc, 0.6))
  if (isTRUE(.mo_ge(oc, 2.5))) TRUE else if (isFALSE(.mo_ge(oc, 0.6))) FALSE else NA
}

# The dry value of each layer; where not recorded it is the moist value + 1
# only when the moist value is 2 or less (L27), and undecided otherwise.
.mo_dry_value <- function(h) {
  vm <- .mo_col(h, "munsell_value_moist"); vd <- .mo_col(h, "munsell_value_dry")
  inf <- is.na(vd) & !is.na(vm) & vm <= 2
  vd[inf] <- vm[inf] + 1
  list(value = vd, inferred = inf)
}

# Each layer judged alone against items 1 to 6 (the layers of the epipedon
# below the mixed upper 18 cm).
.mo_layer_ok <- function(h, i, vd, ca, bs) {
  vm <- .mo_rng(h$munsell_value_moist[i], 0, 10)
  colour <- .mo_colour_ok(vm, .mo_rng(h$munsell_chroma_moist[i], 0, 20),
                          .mo_rng(vd[i], 0, 10), .mo_rng(ca[i], 0, 100))
  .mo_and(colour, .mo_oc_ok(.mo_rng(h$oc_pct[i], 0, 100), vm),
          .mo_ge(.mo_rng(bs[i], 0, 100), 50), .mo_structure_ok(h, i))
}

# Depth limits of item 7 (KST p. 16): D1, the upper boundary of the
# shallowest identifiable secondary carbonates, calcic or petrocalcic
# horizon, duripan or fragipan; D2, the lower boundary of the deepest
# argillic, cambic, natric, oxic or spodic horizon. Only horizons that pass
# are counted.
.mo_item7_depths <- function(pedon) {
  h <- pedon$horizons
  d <- as.character(.mo_col(h, "designation"))
  lay <- function(f) tryCatch({ r <- f(); if (isTRUE(r$passed)) r$layers else integer(0) },
                              error = function(e) integer(0))
  arg <- lay(function() argillic_usda(pedon))
  nat <- lay(function() natric_horizon_usda(pedon))
  cal <- lay(function() calcic_horizon_usda(pedon, max_top_cm = Inf))
  pca <- lay(function() petrocalcic_subgroup_usda(pedon, max_top_cm = Inf))
  dur <- lay(function() duripan_usda(pedon, max_top_cm = Inf))
  fra <- lay(function() fragipan_usda(pedon, max_top_cm = Inf))
  cam <- which(!is.na(d) & grepl("^[0-9]*B[A-Za-z]*w", d))
  oxi <- which(!is.na(d) & grepl("^[0-9]*B[A-Za-z]*o", d))
  spo <- which(!is.na(d) & grepl("^[0-9]*B[A-Za-z]*(hs|s)([0-9]|$)", d))
  sec <- which((!is.na(d) & grepl("^[0-9]*[ABC][A-Za-z]*k", d)) |
               (!is.na(.mo_col(h, "secondary_carbonates_pct")) &
                  .mo_col(h, "secondary_carbonates_pct") > 0))
  dur <- c(dur, which(!is.na(d) & grepl("qm", d)))
  up <- c(sec, cal, pca, dur, fra); lo <- c(arg, cam, nat, oxi, spo)
  D1 <- if (length(up)) min(h$top_cm[up], na.rm = TRUE) else NA_real_
  D2 <- if (length(lo)) max(h$bottom_cm[lo], na.rm = TRUE) else NA_real_
  list(D1 = D1, D2 = D2, any = length(c(up, lo)) > 0L)
}

# Minimum thickness of item 7 (KST p. 16). `coarse` is the texture of the
# epipedon (loamy fine sand or coarser throughout, TRUE / FALSE), `over_contact`
# whether it lies directly above a contact, petrocalcic horizon or duripan.
# The one-third rule takes the deeper of D1 and D2 (L29).
.mo_required_thickness <- function(ms, dep, coarse, over_contact, irregular_oc) {
  D <- suppressWarnings(max(c(dep$D1, dep$D2), na.rm = TRUE))
  D <- if (is.finite(D)) D - ms else NA_real_
  if (isTRUE(coarse)) return(list(cm = 25, rule = "7a(1) loamy fine sand or coarser"))
  if (!dep$any && isTRUE(irregular_oc))
    return(list(cm = 25, rule = "7a(2) no diagnostic horizon below, irregular organic carbon"))
  if (!is.na(D) && D >= 75) return(list(cm = 25, rule = "7a(3) limits 75 cm or deeper"))
  if (isTRUE(over_contact)) return(list(cm = 10, rule = "7b directly above a contact"))
  if (!is.na(D)) return(list(cm = max(18, min(25, D / 3)),
                             rule = sprintf("7c one-third of %s cm", format(round(D, 1)))))
  list(cm = 18, rule = "7d")
}


# ---- Mollic epipedon (KST 13th ed., pp. 14-16) -----------------------------

#' Mollic epipedon (USDA Soil Taxonomy, 13th edition)
#'
#' The mollic epipedon of KST 13th ed. (pp. 14-16), three-valued. The upper
#' 18 cm of the mineral soil (or the whole mineral soil above a densic,
#' lithic or paralithic contact, a petrocalcic horizon or a duripan, if
#' shallower) are mixed, by a thickness-weighted average (note 10, p. 56),
#' whatever the horizon designations; the layers below are judged one by
#' one, and the epipedon extends down while they meet the requirements.
#' \itemize{
#'   \item Structure (items 1-3): fails only where the layer is massive
#'         (structural units over 30 cm) and harder than moderately hard
#'         when dry, has rock structure, or is fluid. FAO "hard" counts as
#'         moderately hard (L33).
#'   \item Colour (item 4): moist value and chroma 3 or less and dry value 5
#'         or less; or CaCO3 equivalent 15-40 \% with moist value and chroma
#'         3 or less; or CaCO3 equivalent 40 \% or more with moist value 5
#'         or less. A dry value not recorded is the moist value + 1 only
#'         where the moist value is 2 or less; otherwise the colour test is
#'         undecided (L27). A CaCO3 equivalent not measured is nil in a
#'         layer recorded without effervescence or with pH(H2O) below 7.
#'   \item Base saturation (item 5) by NH4OAc 50 \% or more throughout; not
#'         measured is undecided (L28; no pH or Al proxy).
#'   \item Organic carbon (item 6): 2.5 \% where the moist value is 4 or 5,
#'         0.6 \% otherwise, and 0.6 \% more than the C horizon where the
#'         colour is not clearly darker than it (6b, read from the moist
#'         colours).
#'   \item Thickness (item 7): 25, 10, one-third (18 to 25) or 18 cm; the
#'         one-third rule takes the deeper of the carbonate limit and the
#'         lower boundary of the argillic or cambic horizon (L29).
#' }
#' The moisture requirement (item 8) is not tested. Layers with 20 \% or
#' more artefacts are not part of it (v0.9.10).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_bs Base saturation, default 50 percent.
#' @param min_oc_pct Organic carbon, default 0.6 percent.
#' @return A \code{\link{DiagnosticResult}}; \code{evidence} carries the
#'   upper and lower boundaries (\code{upper_cm}, \code{lower_cm},
#'   \code{lower_possible_cm}), the thickness known and possible, the
#'   required thickness and the rule that sets it.
#' @references Soil Survey Staff (2022), KST 13th ed., pp. 14-16, 56.
#' @noRd
mollic_epipedon_usda <- function(pedon, min_bs = 50, min_oc_pct = 0.6) {
  .memo(pedon, "mollic_epipedon_usda", list(min_bs, min_oc_pct),
        .mollic_epipedon_usda_uncached(pedon, min_bs, min_oc_pct))
}

.mollic_epipedon_usda_uncached <- function(pedon, min_bs = 50, min_oc_pct = 0.6) {
  ref <- "Soil Survey Staff (2022), KST 13th ed., Ch. 3, pp. 14-16"
  H <- pedon$horizons
  res <- function(passed, layers = integer(0), ev = list(), miss = character(0))
    DiagnosticResult$new(name = "mollic_epipedon_usda", passed = passed,
                         layers = layers, evidence = ev, missing = miss,
                         reference = ref)
  if (is.null(H) || nrow(H) == 0L) return(res(FALSE, ev = list(reason = "empty horizons")))
  ok <- which(!is.na(H$top_cm) & !is.na(H$bottom_cm) & H$bottom_cm > H$top_cm)
  ok <- ok[order(H$top_cm[ok])]
  h <- H[ok, , drop = FALSE]
  org <- .mo_is_organic(h)
  if (all(org)) return(res(FALSE, ev = list(reason = "no mineral layer")))
  ms <- h$top_cm[which(!org)[1]]
  contact <- .mo_is_contact(h)
  ct <- .mo_contact_top(h)
  if (!is.na(ct) && ct <= ms) return(res(FALSE, ev = list(reason = "contact at the mineral soil surface")))
  art <- .mo_col(h, "artefacts_pct")
  dry <- .mo_dry_value(h)
  vd <- dry$value
  ca <- .mo_caco3_known(h)
  bs <- .mo_bs(h)
  bs_ok <- function(r) .mo_ge(r, min_bs)

  # The mixed upper 18 cm (KST p. 14).
  mixb <- ms + 18; if (!is.na(ct) && ct < mixb) mixb <- ct
  m <- which(!org & !contact & h$bottom_cm > ms & h$top_cm < mixb)
  if (!length(m)) return(res(FALSE, ev = list(reason = "no mineral layer above the contact")))
  if (any(!is.na(art[m]) & art[m] >= 20))
    return(res(FALSE, ev = list(reason = "artefacts 20 % or more in the upper 18 cm")))
  w <- pmin(h$bottom_cm[m], mixb) - pmax(h$top_cm[m], ms)
  vm_mix <- .mo_wrange(h$munsell_value_moist[m], w, 0, 10)
  cm_mix <- .mo_wrange(h$munsell_chroma_moist[m], w, 0, 20)
  vd_mix <- .mo_wrange(vd[m], w, 0, 10)
  ca_mix <- .mo_wrange(ca[m], w, 0, 100)
  oc_mix <- .mo_wrange(h$oc_pct[m], w, 0, 100)
  bs_mix <- .mo_wrange(bs[m], w, 0, 100)
  colour <- .mo_colour_ok(vm_mix, cm_mix, vd_mix, ca_mix)
  oc_ok <- .mo_oc_ok(oc_mix, vm_mix)
  if (min_oc_pct > 0.6) oc_ok <- .mo_and(oc_ok, .mo_ge(oc_mix, min_oc_pct))
  # Item 6b: the C horizon, where its moist colour is recorded.
  cidx <- which(!org & !contact & h$top_cm >= mixb &
                  grepl("^[0-9]*C", as.character(.mo_col(h, "designation"))))[1]
  c6b <- TRUE
  if (!is.na(cidx)) {
    vC <- .mo_rng(h$munsell_value_moist[cidx], 0, 10)
    cC <- .mo_rng(h$munsell_chroma_moist[cidx], 0, 20)
    dv <- c(vC[1] - vm_mix[2], vC[2] - vm_mix[1])
    dc <- c(cC[1] - cm_mix[2], cC[2] - cm_mix[1])
    applies <- .mo_or(if (dv[2] < 1) TRUE else if (dv[1] >= 1) FALSE else NA,
                      if (dc[2] < 2) TRUE else if (dc[1] >= 2) FALSE else NA)
    ocC <- h$oc_pct[cidx]
    more <- if (is.na(ocC)) NA else .mo_ge(oc_mix, ocC + 0.6)
    c6b <- .mo_or(.mo_not(applies), more)
    oc_ok <- .mo_and(oc_ok, c6b)
  }
  bs_mok <- bs_ok(bs_mix)
  st_mix <- .mo_and(vapply(m, function(i) .mo_structure_ok(h, i), NA))
  top_ok <- .mo_and(colour, oc_ok, bs_mok, st_mix)

  # Below the mixed part, layer by layer.
  below <- which(h$bottom_cm > mixb & h$top_cm >= ms)
  known_bot <- mixb; poss_bot <- mixb; still <- TRUE
  for (i in below) {
    if (h$top_cm[i] > poss_bot + 0.01) break
    if (org[i] || contact[i] || (!is.na(art[i]) && art[i] >= 20)) break
    v <- .mo_layer_ok(h, i, vd, ca, bs)
    if (isFALSE(v)) break
    poss_bot <- h$bottom_cm[i]
    if (still && isTRUE(v) && h$top_cm[i] <= known_bot + 0.01) known_bot <- h$bottom_cm[i]
    else still <- FALSE
  }
  if (isFALSE(top_ok)) { known_bot <- ms; poss_bot <- ms }
  thk_known <- if (isTRUE(top_ok)) known_bot - ms else 0
  thk_poss <- poss_bot - ms

  # Item 7: the required thickness.
  ep <- which(!org & !contact & h$top_cm < poss_bot & h$bottom_cm > ms)
  lfs <- .mo_lfs_or_coarser(h)
  coarse <- if (length(ep)) .mo_and(lfs[ep]) else NA
  finer_mix <- .mo_not(.mo_and(lfs[m]))
  dep <- .mo_item7_depths(pedon)
  ocb <- h$oc_pct[h$top_cm >= poss_bot & !contact]
  ocb <- ocb[!is.na(ocb)]
  irregular <- length(ocb) >= 2L && any(diff(ocb) > 0)
  over_contact <- !is.na(ct) && abs(ct - poss_bot) < 0.5
  b_ok <- if (over_contact) finer_mix else FALSE
  scen <- if (is.na(coarse) || is.na(b_ok)) {
    list(.mo_required_thickness(ms, dep, TRUE, FALSE, irregular),
         .mo_required_thickness(ms, dep, FALSE, TRUE, irregular),
         .mo_required_thickness(ms, dep, FALSE, FALSE, irregular))
  } else list(.mo_required_thickness(ms, dep, coarse, b_ok, irregular))
  if (is.na(coarse) && isFALSE(b_ok)) scen <- scen[-2]
  if (isFALSE(coarse) && is.na(b_ok)) scen <- scen[-1]
  thick_one <- function(req) if (isTRUE(top_ok) && thk_known >= req - 1e-6) TRUE
                             else if (thk_poss < req - 1e-6) FALSE else NA
  tv <- vapply(scen, function(s) thick_one(s$cm), NA)
  thick_ok <- if (all(tv %in% TRUE)) TRUE else if (all(tv %in% FALSE)) FALSE else NA
  req <- if (length(scen) == 1L) scen[[1]]$cm else range(vapply(scen, `[[`, 0, "cm"))
  rule <- vapply(scen, `[[`, "", "rule")

  passed <- .mo_and(top_ok, thick_ok)
  miss <- character(0)
  if (is.na(passed)) {
    na_in <- function(col, idx) anyNA(.mo_col(h, col)[idx])
    idx <- if (is.na(top_ok)) m else integer(0)
    deeper <- which(h$top_cm >= mixb - 0.01 & h$top_cm < poss_bot)
    idx <- c(idx, if (is.na(thick_ok)) deeper)
    if (is.na(colour) || is.na(thick_ok)) {
      if (na_in("munsell_value_moist", idx)) miss <- c(miss, "munsell_value_moist")
      if (na_in("munsell_chroma_moist", idx)) miss <- c(miss, "munsell_chroma_moist")
      if (anyNA(vd[idx])) miss <- c(miss, "munsell_value_dry")
      if (anyNA(ca[idx])) miss <- c(miss, "caco3_pct")
    }
    if (na_in("oc_pct", idx) || is.na(c6b)) miss <- c(miss, "oc_pct")
    if (anyNA(bs[idx])) miss <- c(miss, "bs_pct")
    st_na <- idx[vapply(idx, function(i) is.na(.mo_structure_ok(h, i)), NA)]
    if (length(st_na)) {
      if (na_in("structure_type", st_na)) miss <- c(miss, "structure_type")
      if (na_in("rupture_resistance", st_na)) miss <- c(miss, "rupture_resistance")
    }
    if (is.na(coarse)) miss <- c(miss, "clay_pct", "sand_pct")
    if (!length(miss)) miss <- "munsell_value_dry"
  }
  lower <- if (isTRUE(passed)) known_bot else if (is.na(passed)) poss_bot else NA_real_
  layers <- if (!isFALSE(passed))
    which(H$top_cm >= ms - 0.01 & H$top_cm < lower & !is.na(H$top_cm) &
            !(seq_len(nrow(H)) %in% ok[org]))
  else integer(0)
  inferred <- which(dry$inferred[c(m, below[h$top_cm[below] < poss_bot])])
  ev <- list(
    upper_cm = ms, lower_cm = if (isTRUE(top_ok)) known_bot else NA_real_,
    lower_possible_cm = poss_bot,
    thickness_cm = thk_known, thickness_possible_cm = thk_poss,
    required_cm = req, thickness_rule = rule,
    mix_depth_cm = c(ms, mixb),
    mixed = list(value_moist = vm_mix, chroma_moist = cm_mix, value_dry = vd_mix,
                 caco3_pct = ca_mix, oc_pct = oc_mix, bs_pct = bs_mix),
    colour = colour, organic_carbon = oc_ok, item_6b = c6b, base_saturation = bs_mok,
    structure = st_mix, upper_part = top_ok, thickness = thick_ok,
    texture_lfs_or_coarser = coarse, D1_cm = dep$D1, D2_cm = dep$D2,
    dry_value_inferred = if (length(inferred))
      "dry value read as the moist value + 1 where the moist value is 2 or less (L27)")
  res(passed, layers = if (isTRUE(passed)) layers else integer(0), ev = ev,
      miss = unique(miss))
}


# ---- I. Mollisols (KST 13th ed., pp. 69-70) ---------------------------------

# Item I.1.b (p. 69): a surface horizon that meets the mollic requirements
# except thickness, after mixing to 18 cm, and a subhorizon more than 7.5 cm
# thick in the upper part of an argillic, kandic or natric horizon that meets
# the colour, organic carbon, base saturation and structure requirements,
# separated from it by an albic horizon.
.mollisol_albic_alternative <- function(pedon, mo) {
  if (!isTRUE(mo$evidence$upper_part)) return(if (isFALSE(mo$evidence$upper_part)) FALSE else NA)
  al <- tryCatch(albic_horizon_usda(pedon), error = function(e) NULL)
  if (is.null(al) || !isTRUE(al$passed) || !length(al$layers)) return(FALSE)
  h <- pedon$horizons
  arg <- tryCatch(c(argillic_usda(pedon)$layers, natric_horizon_usda(pedon)$layers),
                  error = function(e) integer(0))
  if (!length(arg)) return(FALSE)
  albot <- max(h$bottom_cm[al$layers], na.rm = TRUE)
  top_arg <- arg[h$top_cm[arg] >= albot - 0.5]
  if (!length(top_arg)) return(FALSE)
  first <- top_arg[which.min(h$top_cm[top_arg])]
  if (is.na(h$bottom_cm[first]) || h$bottom_cm[first] - h$top_cm[first] <= 7.5) return(FALSE)
  dry <- .mo_dry_value(h)
  .mo_layer_ok(h, first, dry$value, .mo_caco3_known(h), .mo_bs(h))
}

# Item I.2 (p. 70): base saturation (NH4OAc) of 50 % or more in all horizons
# between the upper boundary of any argillic, kandic or natric horizon and
# 125 cm below it, or between the mineral soil surface and 180 cm, or a
# densic, lithic or paralithic contact, whichever is shallowest. Below the
# deepest horizon sampled, that horizon stands in, recorded as an inference
# (L19).
.mollisol_bs_usda <- function(pedon) {
  H <- pedon$horizons
  ok <- which(!is.na(H$top_cm) & !is.na(H$bottom_cm))
  h <- H[ok[order(H$top_cm[ok])], , drop = FALSE]
  org <- .mo_is_organic(h)
  ms <- if (any(!org)) h$top_cm[which(!org)[1]] else 0
  d <- as.character(.mo_col(h, "designation"))
  rock <- !is.na(d) & grepl("^[0-9]*(R|Cr|Cd)($|[^a-z])|^[0-9]*C/R", d)
  ct <- if (any(rock)) min(h$top_cm[rock]) else Inf
  lay <- function(f) tryCatch({ r <- f(); if (isTRUE(r$passed)) r$layers else integer(0) },
                              error = function(e) integer(0))
  B <- c(lay(function() argillic_usda(pedon)), lay(function() natric_horizon_usda(pedon)),
         lay(function() kandic_horizon_usda(pedon)))
  at <- if (length(B)) min(H$top_cm[B], na.rm = TRUE) else NA_real_
  lims <- c(a = if (!is.na(at)) at + 125 else Inf, b = ms + 180, c = ct)
  which_lim <- names(which.min(lims))
  zone <- c(if (which_lim == "a") at else ms, min(lims))
  idx <- which(!org & !rock & h$bottom_cm > zone[1] & h$top_cm < zone[2])
  bs <- .mo_bs(h)[idx]
  base <- list(zone_cm = zone, limit = which_lim, bs_pct = bs, inferences = character(0))
  if (!length(idx)) return(c(base, list(passed = NA, missing = "bs_pct")))
  if (any(!is.na(bs) & bs < 50)) return(c(base, list(passed = FALSE, missing = character(0))))
  meas <- which(!is.na(bs))
  if (!length(meas)) return(c(base, list(passed = NA, missing = "bs_pct")))
  last <- max(meas)
  if (last > 1L && anyNA(bs[seq_len(last - 1L)]))
    return(c(base, list(passed = NA, missing = "bs_pct")))
  reach <- h$bottom_cm[idx[last]]
  if (reach >= zone[2] - 0.5) return(c(base, list(passed = TRUE, missing = character(0))))
  # L19: the deepest horizon sampled stands in for the rest of the zone.
  note <- sprintf(paste0("base saturation read at %s cm, the deepest horizon sampled, ",
                         "as a stand-in for %s cm"),
                  format(round(reach, 1)), format(round(zone[2], 1)))
  base$inferences <- note
  c(base, list(passed = TRUE, missing = character(0)))
}

#' Mollisol order qualifier (KST 13th ed., pp. 69-70, item I)
#'
#' Item 1: a mollic epipedon, or the albic alternative of item I.1.b; item
#' 2: base saturation (NH4OAc) 50 \% or more in all horizons to the
#' shallowest of 125 cm below the upper boundary of an argillic, kandic or
#' natric horizon, 180 cm, or a densic, lithic or paralithic contact. A
#' profile sampled shallower than that depth is read at its deepest horizon
#' sampled, and \code{evidence$inferences} says so (the name then carries
#' grade C).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
mollisol_qualifying_usda <- function(pedon) {
  mo <- mollic_epipedon_usda(pedon)
  mo <- .apply_nasis_tiebreaker(mo, pedon, pattern = "^Mollic epipedon$",
                                feature_label = "Mollic epipedon")
  alt <- if (isTRUE(mo$passed)) FALSE else .mollisol_albic_alternative(pedon, mo)
  item1 <- .mo_or(mo$passed, alt)
  bs <- if (isFALSE(item1)) list(passed = NA, missing = character(0), inferences = character(0))
        else .mollisol_bs_usda(pedon)
  passed <- .mo_and(item1, bs$passed)
  if (isFALSE(item1)) passed <- FALSE
  DiagnosticResult$new(
    name = "mollisol_qualifying_usda", passed = passed,
    layers = mo$layers,
    evidence = list(mollic = mo, albic_alternative = alt, item1 = item1,
                    base_saturation = bs[c("passed", "zone_cm", "limit", "bs_pct")],
                    inferences = if (isTRUE(passed)) bs$inferences else character(0)),
    missing = unique(c(if (is.na(item1)) mo$missing,
                       if (is.na(bs$passed)) bs$missing)),
    notes = if (isTRUE(passed) && length(bs$inferences)) bs$inferences else NA_character_,
    reference = "Soil Survey Staff (2022), KST 13th ed., pp. 69-70, item I"
  )
}


# ---- Mollic epipedon geometry, for the suborders and subgroups --------------

# Lower boundary of the mollic (known), the layers of its lower part, the
# layer directly below it.
.mo_geometry <- function(pedon) {
  mo <- mollic_epipedon_usda(pedon)
  h <- pedon$horizons
  bot <- mo$evidence$lower_cm %||% NA_real_
  if (!isTRUE(mo$passed) || is.na(bot)) return(list(mollic = mo, bottom = NA_real_,
                                                     lower = integer(0), below = NA_integer_))
  lower <- which(!is.na(h$bottom_cm) & abs(h$bottom_cm - bot) < 0.5 & h$top_cm < bot)
  below <- which(!is.na(h$top_cm) & abs(h$top_cm - bot) < 0.5)
  list(mollic = mo, bottom = bot, lower = lower,
       below = if (length(below)) below[1] else NA_integer_)
}

# Redox concentrations recorded in a layer (a field observation: not
# recorded is absent). `contrast`: TRUE to ask for distinct or prominent
# ones, which is undecided where the contrast is not recorded.
.mo_redox_conc <- function(h, i, contrast = FALSE) {
  rdx <- .mo_col(h, "redoximorphic_features_pct")[i]
  mp <- .mo_col(h, "mottles_present")[i]
  mc <- .mo_col(h, "mottle_colors")[i]
  nod <- .mo_col(h, "fe_mn_nodules_pct")[i]
  present <- (!is.na(rdx) && rdx > 0) || isTRUE(as.logical(mp)) ||
             (!is.na(mc) && nzchar(mc)) || (!is.na(nod) && nod > 0)
  if (!present) return(FALSE)
  if (!contrast) return(TRUE)
  ct <- .field_term(.mo_col(h, "mottle_contrast")[i])
  if (is.na(ct)) NA else grepl("distinct|prominent", ct)
}

.mo_hue_rank <- function(hue) {
  r <- c("10R" = 1, "2.5YR" = 2, "5YR" = 3, "7.5YR" = 4, "10YR" = 5, "2.5Y" = 6, "5Y" = 7)
  x <- toupper(gsub("\\s", "", as.character(hue)))
  out <- unname(r[x])
  out[!is.na(x) & grepl("^N", x)] <- 0
  out[!is.na(x) & grepl("GY|G$|BG|B$", x) & !grepl("^N", x)] <- 8
  out
}


# ---- IA. Albolls (KST 13th ed., p. 247) ------------------------------------

#' Albolls qualifier (KST 13th ed., p. 247, item IA)
#'
#' An argillic or natric horizon; an albic horizon with chroma 2 or less,
#' 2.5 cm or more thick, its lower boundary 18 cm or more below the mineral
#' soil surface, directly below the mollic epipedon (or separating parts of
#' it); redox concentrations in the albic or argillic (natric) horizon
#' within 100 cm, as evidence of aquic conditions; and a soil temperature
#' regime warmer than cryic (undecided where not recorded).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
alboll_qualifying_usda <- function(pedon) {
  h <- pedon$horizons
  ref <- "Soil Survey Staff (2022), KST 13th ed., p. 247, item IA"
  arg <- tryCatch(argillic_usda(pedon), error = function(e) NULL)
  nat <- tryCatch(natric_horizon_usda(pedon), error = function(e) NULL)
  c1 <- .mo_or(arg$passed %||% NA, nat$passed %||% NA)
  al <- albic_horizon_usda(pedon)
  geo <- .mo_geometry(pedon)
  ms <- geo$mollic$evidence$upper_cm %||% 0
  c2 <- FALSE
  if (isTRUE(al$passed) && length(al$layers)) {
    L <- al$layers
    chroma <- .mo_and(vapply(L, function(i) .mo_le(.mo_rng(h$munsell_chroma_moist[i], 0, 20), 2), NA))
    thick <- sum(h$bottom_cm[L] - h$top_cm[L], na.rm = TRUE) >= 2.5
    deep <- max(h$bottom_cm[L], na.rm = TRUE) >= ms + 18
    top <- min(h$top_cm[L], na.rm = TRUE)
    mb <- geo$mollic$evidence$lower_possible_cm %||% NA_real_
    under <- if (!is.na(mb) && abs(top - mb) < 0.5) TRUE
             else if (isTRUE(geo$mollic$evidence$upper_part) && top >= ms + 18 - 0.5) TRUE
             else if (is.na(geo$mollic$passed)) NA else FALSE
    c2 <- .mo_and(chroma, thick, deep, under)
  } else if (is.na(al$passed)) c2 <- NA
  B <- unique(c(al$layers, if (isTRUE(arg$passed)) arg$layers, if (isTRUE(nat$passed)) nat$layers))
  B <- B[!is.na(h$top_cm[B]) & h$top_cm[B] < ms + 100]
  c3 <- length(B) > 0L && any(vapply(B, function(i) isTRUE(.mo_redox_conc(h, i)), NA))
  st <- soil_temperature_regime_usda(pedon, setdiff(.USDA_STR_TARGETS, c("cryic", "gelic")))
  passed <- .mo_and(c1, c2, c3, st$passed)
  DiagnosticResult$new(
    name = "alboll_qualifying_usda", passed = passed, layers = al$layers,
    evidence = list(argillic_or_natric = c1, albic = c2, redox_concentrations = c3,
                    warmer_than_cryic = st$passed),
    missing = if (is.na(passed)) unique(c(if (is.na(c1)) c(arg$missing, nat$missing),
                                          if (is.na(c2)) c(al$missing, "munsell_chroma_moist"),
                                          if (is.na(st$passed)) st$missing)) else character(0),
    reference = ref)
}


# ---- IB. Aquolls (KST 13th ed., pp. 247-248) --------------------------------

# The colour sets of item IB.5 (p. 247) in the layer directly below the
# mollic epipedon, or the first layer with moist value 4 or more within 75
# cm where a calcic horizon intervenes: (a) chroma 1 with hue 10YR or redder
# and redox concentrations, or hue 2.5Y or yellower; (b) chroma 2 or less,
# hue 2.5Y, redox concentrations; (c) chroma 3 or less, hue 5Y, redox
# concentrations; (d) neutral; (e) gley hues; (f) uncoated sand grains.
.aquoll_colours_below <- function(h, i) {
  vm <- h$munsell_value_moist[i]; cm <- h$munsell_chroma_moist[i]
  hr <- .mo_hue_rank(h$munsell_hue_moist[i])
  if (is.na(vm)) return(NA)
  if (vm < 4) return(FALSE)
  rc <- .mo_redox_conc(h, i)
  if (!is.na(hr) && hr == 0) return(TRUE)            # (d) N, chroma 0
  if (!is.na(hr) && hr == 8) return(TRUE)            # (e) gley hues
  if (is.na(cm) || is.na(hr)) return(NA)
  a <- cm <= 1 && ((hr <= 5 && rc) || hr >= 6)
  b <- cm <= 2 && hr == 6 && rc
  c <- cm <= 3 && hr == 7 && rc
  a || b || c
}

#' Aquolls qualifier (KST 13th ed., pp. 247-248, item IB)
#'
#' Aquic conditions (redox features, a g designation or a reduced matrix
#' recorded) in a layer above a densic, lithic or paralithic contact or
#' between 40 and 50 cm, whichever is shallower, and one or more of: ESP 15
#' or more (SAR 13) in the upper part of the mollic epipedon, decreasing
#' below 50 cm; a histic epipedon over the mollic; a calcic or petrocalcic
#' horizon within 40 cm; a mollic epipedon of chroma 1 or less to a lithic
#' contact within 30 cm; the colours of the lower part of the mollic and
#' below it (item 5). The alpha,alpha-dipyridyl test (item 6) is not
#' recorded in soilKey and is absent.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
aquoll_qualifying_usda <- function(pedon) {
  h <- pedon$horizons
  ref <- "Soil Survey Staff (2022), KST 13th ed., pp. 247-248, item IB"
  geo <- .mo_geometry(pedon)
  ms <- geo$mollic$evidence$upper_cm %||% .mineral_soil_surface_cm(h)
  if (is.na(ms)) ms <- 0
  ct <- .mo_contact_top(h)
  z <- if (!is.na(ct) && ct < ms + 40) c(ms, ct) else c(ms + 40, ms + 50)
  zone <- which(!is.na(h$top_cm) & h$bottom_cm > z[1] & h$top_cm < z[2])
  dsg <- as.character(.mo_col(h, "designation"))
  red <- .mo_col(h, "reduced_matrix_pct")
  aquic <- any(vapply(zone, function(i) isTRUE(.mo_redox_conc(h, i)) ||
                        (!is.na(dsg[i]) && grepl("g", dsg[i])) ||
                        (!is.na(red[i]) && red[i] > 0), NA))
  if (!aquic) {
    return(DiagnosticResult$new(name = "aquoll_qualifying_usda", passed = FALSE,
      layers = integer(0), evidence = list(aquic_conditions = FALSE, zone_cm = z),
      missing = character(0), reference = ref))
  }
  # 1. elevated sodium
  esp <- .mo_col(h, "esp_pct"); sar <- .mo_col(h, "sar")
  up <- if (length(geo$lower)) which(h$top_cm < geo$bottom & h$top_cm >= ms) else integer(0)
  sod_up <- if (length(up)) .mo_or(vapply(up[1], function(i)
    .mo_or(if (is.na(esp[i])) NA else esp[i] >= 15, if (is.na(sar[i])) NA else sar[i] >= 13), NA)) else FALSE
  deep <- which(h$top_cm >= ms + 50)
  e_deep <- esp[deep]
  dec <- if (length(e_deep) >= 1L && !anyNA(e_deep) && length(up) && !is.na(esp[up[1]]))
    all(e_deep < esp[up[1]]) else NA
  i1 <- .mo_and(sod_up, dec)
  # 2. histic epipedon over the mollic
  hi <- tryCatch(histic_epipedon_usda(pedon)$passed, error = function(e) NA)
  i2 <- .mo_and(hi, geo$mollic$passed)
  # 3. calcic or petrocalcic within 40 cm
  i3 <- .mo_or(calcic_horizon_usda(pedon, max_top_cm = ms + 40)$passed,
               petrocalcic_subgroup_usda(pedon, max_top_cm = ms + 40)$passed)
  # 4. chroma 1 or less to a lithic contact within 30 cm
  lith <- tryCatch(lithic_contact_usda(pedon, max_top_cm = ms + 30)$passed, error = function(e) FALSE)
  i4 <- FALSE
  if (isTRUE(lith) && isTRUE(geo$mollic$passed)) {
    L <- which(h$top_cm < geo$bottom & h$top_cm >= ms)
    i4 <- .mo_and(abs(geo$bottom - .mo_contact_top(h)) < 0.5,
                  .mo_and(vapply(L, function(i) .mo_le(.mo_rng(h$munsell_chroma_moist[i], 0, 20), 1), NA)))
  }
  # 5. colours in the lower part of the mollic epipedon
  i5 <- FALSE
  if (isTRUE(geo$mollic$passed) && length(geo$lower)) {
    lc <- .mo_rng(suppressWarnings(max(h$munsell_chroma_moist[geo$lower])), 0, 20)
    if (anyNA(h$munsell_chroma_moist[geo$lower])) lc <- c(0, 20)
    waived <- isTRUE(lith)
    rc_low <- .mo_or(vapply(geo$lower, function(i) .mo_redox_conc(h, i, contrast = TRUE), NA))
    nb <- geo$below
    # (a)(2): directly below, or within 75 cm if a calcic horizon intervenes
    cal <- calcic_horizon_usda(pedon, max_top_cm = ms + 75)
    cand <- nb
    if (isTRUE(cal$passed) && !is.na(nb) && nb %in% cal$layers) {
      cand <- which(h$top_cm >= h$top_cm[nb] & h$top_cm < ms + 75 &
                      !(seq_len(nrow(h)) %in% cal$layers))[1]
    }
    a2 <- if (is.na(cand)) FALSE else .aquoll_colours_below(h, cand)
    a <- .mo_and(.mo_le(lc, 1), .mo_or(waived, rc_low, a2))
    # (b) chroma 2 in the lower part
    b2 <- FALSE
    if (!is.na(nb)) {
      vm <- h$munsell_value_moist[nb]; cm <- h$munsell_chroma_moist[nb]
      dep <- .mo_redox_conc(h, nb)
      b2 <- if (is.na(vm) || is.na(cm)) NA else
        (vm == 4 && cm == 2 && dep) || (vm >= 5 && cm <= 2 && dep) || (vm == 4 && cm <= 1)
    }
    is2 <- if (lc[1] == lc[2]) lc[1] > 1 && lc[1] <= 2 else if (lc[1] > 2) FALSE else NA
    b <- .mo_and(is2, .mo_or(waived, rc_low, b2))
    i5 <- .mo_or(a, b)
  } else if (is.na(geo$mollic$passed)) i5 <- NA
  passed <- .mo_or(i1, i2, i3, i4, i5)
  DiagnosticResult$new(
    name = "aquoll_qualifying_usda", passed = passed, layers = zone,
    evidence = list(aquic_conditions = TRUE, zone_cm = z, sodium = i1, histic = i2,
                    calcic_40 = i3, lithic_30 = i4, colours = i5),
    missing = if (is.na(passed)) unique(c("munsell_chroma_moist", "munsell_hue_moist",
                                          if (is.na(i1)) "esp_pct")) else character(0),
    reference = ref)
}


# ---- IC. Rendolls (KST 13th ed., p. 248) ------------------------------------

#' Rendolls qualifier (KST 13th ed., p. 248, item IC)
#'
#' A mollic epipedon less than 50 cm thick; no argillic or calcic horizon;
#' a CaCO3 equivalent of 40 \% or more within or directly below the mollic
#' epipedon; and a udic moisture regime or a cryic temperature regime
#' (undecided where neither is recorded).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
rendoll_qualifying_usda <- function(pedon) {
  h <- pedon$horizons
  mo <- mollic_epipedon_usda(pedon)
  e <- mo$evidence
  thin <- if (isTRUE(mo$passed)) {
    if ((e$thickness_possible_cm %||% Inf) < 50) TRUE
    else if ((e$thickness_cm %||% 0) >= 50) FALSE else NA
  } else if (is.na(mo$passed)) NA else FALSE
  arg <- tryCatch(argillic_usda(pedon)$passed, error = function(e) NA)
  cal <- tryCatch(calcic_horizon_usda(pedon, max_top_cm = Inf)$passed, error = function(e) NA)
  no_arg_cal <- .mo_not(.mo_or(arg, cal))
  ms <- e$upper_cm %||% 0
  bot <- e$lower_possible_cm %||% ms
  ca <- .mo_caco3_known(h)
  zone <- which(!is.na(h$top_cm) & h$top_cm >= ms - 0.01 & h$top_cm <= bot + 0.5 &
                  !.mo_is_contact(h) & !.mo_is_organic(h))
  c3 <- .mo_or(vapply(zone, function(i) if (is.na(ca[i])) NA else ca[i] >= 40, NA))
  if (!length(zone)) c3 <- FALSE
  ud <- smr_udic_usda(pedon); cr <- str_cryic_usda(pedon)
  c4 <- .mo_or(ud$passed, cr$passed)
  passed <- .mo_and(mo$passed, thin, no_arg_cal, c3, c4)
  DiagnosticResult$new(
    name = "rendoll_qualifying_usda", passed = passed,
    layers = zone[!is.na(ca[zone]) & ca[zone] >= 40],
    evidence = list(mollic_thinner_than_50 = thin, no_argillic_or_calcic = no_arg_cal,
                    caco3_40 = c3, udic_or_cryic = c4),
    missing = if (is.na(passed)) unique(c(if (is.na(c3)) "caco3_pct",
                                          if (is.na(c4)) c(ud$missing, cr$missing),
                                          if (is.na(thin)) mo$missing)) else character(0),
    reference = "Soil Survey Staff (2022), KST 13th ed., p. 248, item IC")
}


# ---- Calci- great groups (KST 13th ed., pp. 249, 254, 259, 267, 284) --------

# Free carbonates in a layer: CaCO3 equivalent > 0 or effervescence; not
# measured and no effervescence recorded, undecided.
.mo_free_carbonates <- function(h, i) {
  ca <- .mo_col(h, "caco3_pct")[i]
  ef <- .field_term(.mo_col(h, "effervescence_class")[i])
  if (!is.na(ca)) return(ca > 0)
  if (!is.na(ef)) return(!ef %in% c("none", "non-calcareous", "noncalcareous", "no"))
  ph <- .mo_col(h, "ph_h2o")[i]
  if (!is.na(ph) && ph < 7) return(FALSE)
  NA
}

#' Calci- great groups of the Mollisols
#'
#' \describe{
#'   \item{udoll}{Calciudolls (p. 259): a calcic or petrocalcic horizon within
#'     100 cm, no argillic horizon above it, free carbonates in all parts
#'     above it after mixing the upper 18 cm.}
#'   \item{ustoll}{Calciustolls (p. 267): a calcic or gypsic horizon within
#'     100 cm or a petrocalcic horizon within 150 cm, no argillic horizon
#'     above it, and above it either free carbonates or loamy fine sand or
#'     coarser in all parts.}
#'   \item{xeroll}{Calcixerolls (p. 284): a calcic or gypsic horizon within
#'     150 cm, and above it free carbonates or loamy fine sand or coarser.}
#'   \item{cryoll}{Calcicryolls (p. 254): a calcic or petrocalcic horizon
#'     within 100 cm, and above it free carbonates or loamy fine sand or
#'     coarser.}
#'   \item{aquoll}{Calciaquolls (p. 249): a calcic or gypsic horizon within 40
#'     cm and no argillic horizon (unless buried).}
#' }
#' @param pedon A \code{\link{PedonRecord}}.
#' @param suborder One of the names above.
#' @noRd
calci_mollisol_usda <- function(pedon, suborder = c("udoll", "ustoll", "xeroll", "cryoll", "aquoll")) {
  suborder <- match.arg(suborder)
  h <- pedon$horizons
  ms <- .mineral_soil_surface_cm(h); if (is.na(ms)) ms <- 0
  ref <- "Soil Survey Staff (2022), KST 13th ed., pp. 249, 254, 259, 267, 284"
  lay <- function(r) if (isTRUE(r$passed)) r$layers else integer(0)
  pas <- function(r) r$passed
  cal_d <- switch(suborder, udoll = 100, ustoll = 100, xeroll = 150, cryoll = 100, aquoll = 40)
  rs <- list(calcic = calcic_horizon_usda(pedon, max_top_cm = ms + cal_d))
  if (suborder %in% c("ustoll", "xeroll", "aquoll"))
    rs$gypsic <- gypsic_horizon_usda(pedon, max_top_cm = ms + cal_d)
  if (suborder %in% c("udoll", "cryoll", "ustoll"))
    rs$petrocalcic <- petrocalcic_subgroup_usda(pedon,
                        max_top_cm = ms + if (suborder == "ustoll") 150 else 100)
  present <- .mo_or(vapply(rs, pas, NA))
  hor <- unique(unlist(lapply(rs, lay)))
  if (!isTRUE(present)) {
    return(DiagnosticResult$new(name = "calci_mollisol_usda", passed = present,
      layers = integer(0), evidence = list(horizon = present),
      missing = if (is.na(present)) unique(unlist(lapply(rs, `[[`, "missing"))) else character(0),
      reference = ref))
  }
  t0 <- min(h$top_cm[hor], na.rm = TRUE)
  arg <- tryCatch(argillic_usda(pedon), error = function(e) NULL)
  no_arg <- TRUE
  if (suborder %in% c("udoll", "ustoll")) {
    no_arg <- if (isTRUE(arg$passed)) !any(h$top_cm[arg$layers] < t0)
              else if (is.na(arg$passed %||% FALSE)) NA else TRUE
  } else if (suborder == "aquoll") {
    no_arg <- .mo_not(arg$passed %||% FALSE)
  }
  above <- which(!is.na(h$top_cm) & h$top_cm < t0 & h$top_cm >= ms - 0.01 &
                   !.mo_is_organic(h))
  above_ok <- TRUE
  if (suborder != "aquoll" && length(above)) {
    # the upper 18 cm mixed: free carbonates if any part of it has them
    mix <- above[h$top_cm[above] < ms + 18]
    rest <- setdiff(above, mix)
    fc <- function(i) .mo_free_carbonates(h, i)
    mix_fc <- if (length(mix)) .mo_or(vapply(mix, fc, NA)) else TRUE
    lfs <- .mo_lfs_or_coarser(h)
    if (suborder == "udoll") {
      above_ok <- .mo_and(mix_fc, vapply(rest, fc, NA))
    } else {
      mix_ok <- .mo_or(mix_fc, if (length(mix)) .mo_and(lfs[mix]) else TRUE)
      above_ok <- .mo_and(mix_ok, vapply(rest, function(i) .mo_or(fc(i), lfs[i]), NA))
    }
  }
  passed <- .mo_and(present, no_arg, above_ok)
  DiagnosticResult$new(
    name = "calci_mollisol_usda", passed = passed, layers = hor,
    evidence = list(horizon_top_cm = t0, no_argillic_above = no_arg,
                    above = above_ok, suborder = suborder),
    missing = if (is.na(passed)) unique(c(if (is.na(above_ok)) c("caco3_pct", "effervescence_class"),
                                          if (is.na(no_arg)) arg$missing)) else character(0),
    reference = ref)
}


# ---- Pale- great groups (KST 13th ed., pp. 254, 259, 267-268, 284) ----------

# No clay decrease of 20 % (relative) or more from the maximum clay within
# 150 cm, and no densic, lithic or paralithic contact within 150 cm. A
# profile described shallower than 150 cm is undecided (L20).
.mo_no_clay_decrease_150 <- function(h, ms) {
  ct <- .mo_contact_top(h)
  if (!is.na(ct) && ct < ms + 150) return(FALSE)
  in150 <- which(!is.na(h$top_cm) & h$top_cm < ms + 150 & !.mo_is_organic(h))
  clay <- h$clay_pct[in150]
  if (all(is.na(clay))) return(NA)
  mx <- which.max(replace(clay, is.na(clay), -Inf))
  after <- seq_along(in150) > mx
  if (any(!is.na(clay[after]) & clay[after] <= 0.8 * clay[mx])) return(FALSE)
  if (anyNA(clay) || (.mo_described_to(h) %||% 0) < ms + 150) return(NA)
  TRUE
}

#' Pale- great groups of the Mollisols
#'
#' \describe{
#'   \item{udoll}{Paleudolls (p. 259): no contact and no clay decrease of 20
#'     \% within 150 cm, with reddish colours (hue 7.5YR or redder and chroma
#'     5 or more in the lower half of the argillic horizon, hue 2.5YR or
#'     redder with moist value 3 or less and dry value 4 or less, or many
#'     redox concentrations of hue 5YR or redder or chroma 6 or more); or a
#'     frigid regime, an argillic horizon whose upper boundary is 60 cm or
#'     more deep, and finer than loamy fine sand above it.}
#'   \item{ustoll, xeroll}{Paleustolls (pp. 267-268), Palexerolls (p. 284):
#'     a petrocalcic horizon within 150 cm; or an argillic horizon with no
#'     clay decrease of 20 \% within 150 cm (and no contact) and hue 7.5YR or
#'     redder with chroma 5 or more, or common redox concentrations of hue
#'     7.5YR or redder or chroma 6 or more; or 35 \% or more clay in its upper
#'     part with an abrupt textural change (no contact within 50 cm).}
#'   \item{cryoll}{Palecryolls (p. 254): an argillic horizon whose upper
#'     boundary is 60 cm or more deep and finer than loamy fine sand in all
#'     horizons above it.}
#' }
#' @param pedon A \code{\link{PedonRecord}}.
#' @param suborder One of the names above.
#' @noRd
pale_mollisol_usda <- function(pedon, suborder = c("udoll", "ustoll", "xeroll", "cryoll")) {
  suborder <- match.arg(suborder)
  h <- pedon$horizons
  ms <- .mineral_soil_surface_cm(h); if (is.na(ms)) ms <- 0
  ref <- "Soil Survey Staff (2022), KST 13th ed., pp. 254, 259, 267-268, 284"
  arg <- tryCatch(argillic_usda(pedon), error = function(e) NULL)
  A <- if (isTRUE(arg$passed)) arg$layers[order(h$top_cm[arg$layers])] else integer(0)
  hue <- .mo_hue_rank(h$munsell_hue_moist); cm <- h$munsell_chroma_moist
  red_chroma <- function(i, rank, chroma) {
    if (is.na(hue[i]) || is.na(cm[i])) NA else hue[i] >= 1 && hue[i] <= rank && cm[i] >= chroma
  }
  deep_arg <- function() {
    if (!length(A)) return(if (is.na(arg$passed %||% FALSE)) NA else FALSE)
    at <- min(h$top_cm[A])
    if (at < ms + 60) return(FALSE)
    above <- which(!is.na(h$top_cm) & h$top_cm < at & h$top_cm >= ms - 0.01 & !.mo_is_organic(h))
    .mo_and(at >= ms + 60, .mo_not(.mo_or(.mo_lfs_or_coarser(h)[above])))
  }
  rc_many <- function(i, rank) {
    rc <- .mo_redox_conc(h, i)
    if (!rc) return(FALSE)
    mc <- as.character(.mo_col(h, "mottle_colors")[i])
    if (is.na(mc)) return(NA)
    hr <- .mo_hue_rank(sub("\\s.*$", "", trimws(mc)))
    ch <- suppressWarnings(as.numeric(sub(".*/", "", mc)))
    .mo_or(if (is.na(hr)) NA else hr >= 1 && hr <= rank, if (is.na(ch)) NA else ch >= 6)
  }
  v <- switch(suborder,
    cryoll = deep_arg(),
    udoll = {
      i1 <- FALSE
      if (length(A)) {
        nd <- .mo_no_clay_decrease_150(h, ms)
        mid <- (min(h$top_cm[A]) + max(h$bottom_cm[A])) / 2
        low <- A[h$bottom_cm[A] > mid]
        c1 <- .mo_or(vapply(low, function(i) red_chroma(i, 4, 5), NA))
        thk <- h$bottom_cm[A] - h$top_cm[A]
        dark <- vapply(A, function(i) {
          vm <- h$munsell_value_moist[i]; vd <- h$munsell_value_dry[i]
          .mo_and(if (is.na(hue[i])) NA else hue[i] >= 1 && hue[i] <= 2,
                  if (is.na(vm)) NA else vm <= 3, if (is.na(vd)) NA else vd <= 4) }, NA)
        c2 <- if (sum(thk[dark %in% TRUE]) > sum(thk) / 2) TRUE
              else if (sum(thk[!dark %in% FALSE]) <= sum(thk) / 2) FALSE else NA
        c3 <- .mo_or(vapply(A, function(i) rc_many(i, 3), NA))
        i1 <- .mo_and(nd, .mo_or(c1, c2, c3))
      } else if (is.na(arg$passed %||% FALSE)) i1 <- NA
      fr <- soil_temperature_regime_usda(pedon, "frigid")$passed
      .mo_or(i1, .mo_and(fr, deep_arg()))
    },
    {
      pc <- petrocalcic_subgroup_usda(pedon, max_top_cm = ms + 150)$passed
      a <- FALSE; b <- FALSE
      if (length(A)) {
        nd <- .mo_no_clay_decrease_150(h, ms)
        col <- .mo_or(vapply(A, function(i) red_chroma(i, 4, 5), NA),
                      vapply(A, function(i) rc_many(i, 4), NA))
        a <- .mo_and(nd, col)
        ct <- .mo_contact_top(h)
        up <- A[1]
        prev <- which(!is.na(h$bottom_cm) & abs(h$bottom_cm - h$top_cm[up]) < 0.5)
        prev <- if (length(prev)) prev[length(prev)] else NA
        clay35 <- if (is.na(h$clay_pct[up])) NA else h$clay_pct[up] >= 35
        abrupt <- if (is.na(prev)) FALSE else {
          inc <- h$clay_pct[up] - h$clay_pct[prev]
          bd <- .field_term(.mo_col(h, "boundary_distinctness")[prev])
          .mo_and(if (is.na(inc)) NA else inc >= 20,
                  if (is.na(bd)) NA else bd %in% c("very abrupt", "abrupt"))
        }
        b <- .mo_and(clay35, abrupt, is.na(ct) || ct >= ms + 50)
      } else if (is.na(arg$passed %||% FALSE)) a <- NA
      .mo_or(pc, a, b)
    })
  DiagnosticResult$new(
    name = "pale_mollisol_usda", passed = v, layers = A,
    evidence = list(suborder = suborder),
    missing = if (is.na(v)) c("clay_pct", "munsell_hue_moist", "munsell_chroma_moist") else character(0),
    reference = ref)
}


# ---- Vermi- great groups (KST 13th ed., pp. 259, 268) -----------------------

#' Vermudolls and Vermustolls (KST 13th ed., pp. 259, 268)
#'
#' A mollic epipedon that, below an Ap horizon or 18 cm, has 50 \% or more
#' (by volume, a thickness-weighted mean) wormholes, wormcasts or filled
#' animal burrows, and either rests on a
#' lithic contact or has a transition zone to the horizon below with 25 \%
#' or more. Worm holes are a field observation: not recorded, absent.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
vermic_mollisol_usda <- function(pedon) {
  h <- pedon$horizons
  geo <- .mo_geometry(pedon)
  wh <- .mo_col(h, "worm_holes_pct")
  ref <- "Soil Survey Staff (2022), KST 13th ed., pp. 259, 268"
  if (!isTRUE(geo$mollic$passed)) {
    return(DiagnosticResult$new(name = "vermic_mollisol_usda",
      passed = if (is.na(geo$mollic$passed)) NA else FALSE, layers = integer(0),
      evidence = list(reason = "no mollic epipedon"), missing = geo$mollic$missing,
      reference = ref))
  }
  ms <- geo$mollic$evidence$upper_cm
  dsg <- as.character(.mo_col(h, "designation"))
  ap <- which(!is.na(dsg) & grepl("^Ap", dsg))
  from <- max(ms + 18, if (length(ap)) max(h$bottom_cm[ap]) else -Inf)
  L <- which(!is.na(h$top_cm) & h$bottom_cm > from & h$top_cm < geo$bottom)
  wl <- pmin(h$bottom_cm[L], geo$bottom) - pmax(h$top_cm[L], from)
  # by volume of that part of the epipedon: a thickness-weighted mean
  c1 <- length(L) > 0L && sum(ifelse(is.na(wh[L]), 0, wh[L]) * wl) / sum(wl) >= 50
  nb <- geo$below
  lith <- !is.na(nb) && isTRUE(.mo_is_contact(h)[nb])
  c2 <- lith || (!is.na(nb) && !is.na(wh[nb]) && wh[nb] >= 25)
  passed <- c1 && c2
  DiagnosticResult$new(name = "vermic_mollisol_usda", passed = passed,
    layers = if (passed) L else integer(0),
    evidence = list(worm_holes_50 = c1, rests_on_rock_or_transition_25 = c2),
    missing = character(0), reference = ref)
}


#' Vermic Subgroup helper (Vermaqualfs, Vermudepts and Vermi- subgroups)
#' Pass when worm_holes_pct >= 50\% in some horizon. A field observation:
#' not recorded, absent (FALSE).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
vermic_subgroup_usda <- function(pedon) {
  h <- pedon$horizons
  wh <- h$worm_holes_pct
  passing <- which(!is.na(wh) & wh >= 50)
  passed <- length(passing) > 0L
  DiagnosticResult$new(
    name = "vermic_subgroup_usda", passed = passed, layers = passing,
    evidence = list(threshold_pct = 50),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 12"
  )
}


#' Argic Mollisol Suborder helper -- delegates argillic_within_usda.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
argic_mollisol_usda <- function(pedon) {
  res <- argillic_within_usda(pedon, max_top_cm = 200)
  res$name <- "argic_mollisol_usda"
  res
}


#' Entic Durustolls (KST 13th ed., p. 273): no argillic horizon above the
#' duripan.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
entic_durustoll_usda <- function(pedon) {
  h <- pedon$horizons
  du <- duripan_usda(pedon, max_top_cm = Inf)
  arg <- tryCatch(argillic_usda(pedon), error = function(e) NULL)
  dt <- if (isTRUE(du$passed) && length(du$layers)) min(h$top_cm[du$layers]) else Inf
  passed <- if (isTRUE(arg$passed)) !any(h$top_cm[arg$layers] < dt)
            else if (is.na(arg$passed %||% FALSE)) NA else TRUE
  DiagnosticResult$new(name = "entic_durustoll_usda", passed = passed,
    layers = integer(0), evidence = list(duripan_top_cm = dt),
    missing = if (is.na(passed)) arg$missing else character(0),
    reference = "Soil Survey Staff (2022), KST 13th ed., p. 273")
}
