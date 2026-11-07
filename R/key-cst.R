# =============================================================================
# v0.9.373 -- the Chinese Soil Taxonomy (Cooperative Research Group on Chinese
# Soil Taxonomy, ISSAS. Chinese Soil Taxonomy. Science Press, Beijing and New
# York, 2001; the English edition, system id cst2001), keyed to its 14 orders
# (key 5.1, pp. 72-75) and 39 suborders (key 5.2).
#
# The key is of the KST kind: at each level the first class whose tests pass
# is assigned, a class lacking the properties of those before it, so
# classify_cst2001() is a thin wrapper around classify_hierarchical(). The
# gates (inst/rules/cst2001/key.yaml) call the diagnostic horizons
# (R/diagnostics-cst-horizons.R, R/diagnostics-cst-anthropic.R), the
# diagnostic characteristics (R/diagnostics-cst-properties.R) and the regimes
# (R/cst-regimes.R) of the earlier parts, and the few criteria the key
# defines where it uses them, written below: the share of the upper 60 cm
# with andic property, the organic soil materials of the Histosols, an argic
# horizon above the spodic one, the base saturation window of the
# Isohumosols, runs of layers with a feature in a depth window, the claypan
# with illuviation argillans, the humic spodic subhorizons, the texture of
# the Vitric Andosols, the crack days of the Ustic Vertosols, the low n value
# or clay of the Cambosols, irrigation and anthro-silting within 50 cm.
#
# Experimental and opt-in: classify_all() runs it only when asked. The groups
# (v0.9.374) are keyed from inst/rules/cst2001/groups/, with the criteria of
# R/key-cst-groups.R; the subgroups follow in a later part. Readings where
# the text is silent:
# inst/rules/cst2001/readings.yaml.
# =============================================================================

.CST_REF_KEY <- "Chinese Soil Taxonomy (2001), 5.1 Key to soil orders, pp. 72-75; 5.2 Key to suborders"

# A DiagnosticResult of a key criterion.
.cst_key_result <- function(name, passed, layers = integer(0), missing = character(0),
                            flags = character(0), notes = character(0), evidence = list(),
                            page = NA_character_) {
  flags <- unique(as.character(stats::na.omit(flags)))
  nt <- unique(as.character(stats::na.omit(c(notes, flags))))
  DiagnosticResult$new(
    name = name, passed = passed, layers = as.integer(layers),
    evidence = c(evidence, list(flags = flags, provisional = isTRUE(passed) && "provisional" %in% flags)),
    missing = if (is.na(passed)) unique(as.character(missing)) else character(0),
    reference = if (is.na(page)) .CST_REF_KEY else sprintf("Chinese Soil Taxonomy (2001), p. %s", page),
    notes = if (length(nt)) paste(nt, collapse = "; ") else NA_character_)
}

# Three-valued NOT.
.cst_not <- function(v) if (is.na(v)) NA else !v

# A range [low, high] against a minimum: TRUE when low reaches it, FALSE when
# high does not, NA otherwise (zero never reaches a positive minimum).
.cst_at_least <- function(low, high, min) {
  if (!is.na(low) && low >= min - 1e-9 && low > 0) TRUE
  else if (!is.na(high) && high < min - 1e-9) FALSE else NA
}

# The per-layer reading of a horizon or a layer characteristic, in depth
# order: full (TRUE / FALSE / NA), the provisional layers, the attributes that
# would decide it.
.cst_layer_feature <- function(x, feature, grade = c("full", "at_least_evidence")) {
  grade <- match.arg(grade)
  r <- if (feature %in% .cst_horizon_table()$id) .cst_hz(x, feature) else .cst_ch(x, feature)
  # v0.9.374: at least the evidence grade where the key asks for an evidence
  # (Q19), the full grade passing too
  list(full = if (grade == "full") r$full else r$evid %||% r$full, prov = r$prov %||% rep(FALSE, x$n), missing = r$missing %||% character(0),
       flags = r$flags %||% character(0))
}

# The overlap of each layer (depth order) with the window [a, b).
.cst_overlap <- function(x, a, b) {
  pmax(0, pmin(x$bot, b) - pmax(x$top, a))
}


# --- the share of the upper 60 cm with andic property (A 1, C 2, D) ---------------------

#' Andic property in a share of the upper 60 cm (key to orders, A 1, C 2, D;
#' p. 72-73): the share of the thickness, from the soil surface (`datum =
#' "surface"`, the Histosols) or from the mineral soil surface or the top of
#' an organic layer with andic property, whichever is shallower (`datum =
#' "andic"`, the Spodosols and Andosols), to 60 cm below it or to a lithic or
#' paralithic contact if shallower, with andic property. `min` asks for that
#' share or more, `below` for less than it. The share is by thickness (Q22);
#' a layer left undecided counts at both ends of the range.
#' @noRd
andic_share_cst <- function(pedon, min = NULL, below = NULL, datum = c("surface", "andic")) {
  datum <- match.arg(datum)
  x <- .cst_ctx(pedon)
  nm <- "andic_share_cst"
  if (!x$n) return(.cst_key_result(nm, if (is.null(min)) TRUE else FALSE, page = "72-73"))
  r <- .cst_ch(x, "andic_property")
  s <- if (datum == "surface") x$surf else {
    org_andic <- which(x$kind %in% "O" & r$full %in% TRUE & !is.na(x$top))
    min(c(x$msurf, x$top[org_andic]), na.rm = FALSE)
  }
  if (is.na(s)) return(.cst_key_result(nm, NA, missing = c("designation", "oc_pct"), page = "72-73"))
  window <- .cst_thr("key", "andic_window_cm")
  end <- s + window
  ct <- .cst_contact_cm(pedon)
  if (!is.na(ct) && ct < end) end <- ct
  if (end <= s) {
    # rock at the datum: no thickness, no andic share
    return(.cst_key_result(nm, if (is.null(min)) TRUE else FALSE, page = "72-73",
                           notes = "a contact at the datum: no thickness to share"))
  }
  sh <- .cst_window_share(pedon, r$full, s, end)
  frac <- min %||% below
  ok <- .cst_share_at_least(sh, frac)
  passed <- if (is.null(min)) .cst_not(ok) else ok
  rows <- which(r$full %in% TRUE & .cst_overlap(x, s, end) > 0)
  .cst_key_result(nm, passed, layers = x$idx[rows], missing = r$missing,
                  flags = r$flags,
                  evidence = list(share = sh$value, window = c(top = s, bottom = end), datum = datum),
                  notes = sprintf("andic share of %s-%s cm: %s", format(s), format(end),
                                  paste(format(round(sh$value, 2)), collapse = " to ")),
                  page = "72-73")
}


# --- the organic soil materials of the Histosols (A 2) ------------------------------------

# Cindery, fragmental or pumiceous materials (the Histosols, p. 72): 90 % or
# more of coarse fragments (fragmental), or 35 % or more of pyroclastic
# fragments (cinders, scoria, pumice). Field observations: not recorded,
# absent (default 21 of 2026-11-04; the 35 % is soilKey's reading).
.cst_cindery <- function(x) {
  t <- .cst_thr("key", "histosols")
  cf <- .cst_v(x, "coarse_fragments_pct"); py <- .cst_v(x, "pyroclastic_fragments_pct")
  (cf >= t$fragmental_min_pct) %in% TRUE | (py >= t$cindery_pyroclastic_min_pct) %in% TRUE
}

# The thickness of the layers (low: TRUE; high: TRUE or NA) of a per-layer
# test within [a, b).
.cst_thk_range <- function(x, p, a, b) {
  ov <- .cst_overlap(x, a, b)
  ov[is.na(ov)] <- 0
  c(low = sum(ov[p %in% TRUE]), high = sum(ov[!(p %in% FALSE)]))
}

#' The organic soil materials of the Histosols (key to orders, A 2, p. 72):
#' (a) over cindery, fragmental or pumiceous materials that lie directly on a
#' lithic or paralithic contact; (b) with the cindery materials under them, 40
#' cm or more between the soil surface and 50 cm; (c) two thirds or more of
#' the soil above a contact, the mineral layers 10 cm or less in all; (d)
#' saturated 6 months or more (or artificially drained), the upper boundary
#' within 40 cm of the soil surface, and 60 cm thick when three fourths of
#' their volume is moss or their bulk density is under 0.1, 40 cm thick when
#' sapric or hemic, or fibric with less moss and a bulk density of 0.1-0.4.
#' @noRd
histosol_materials_cst <- function(pedon) {
  nm <- "histosol_materials_cst"
  x <- .cst_ctx(pedon)
  if (!x$n || is.na(x$surf)) return(.cst_key_result(nm, if (x$n) NA else FALSE,
                                                     missing = c("top_cm", "bottom_cm"), page = 72))
  t <- .cst_thr("key", "histosols")
  r <- .cst_ch(x, "organic_soil_materials")
  o <- r$full
  s <- x$surf
  cin <- .cst_cindery(x)
  ct <- .cst_contact_cm(pedon)
  above <- if (is.na(ct)) integer(0) else which(!is.na(x$top) & x$top < ct & !(x$kind %in% "R"))
  # (a) organic over cindery materials directly on the contact
  a <- if (!length(above)) FALSE else {
    last <- above[length(above)]
    ok <- o[above] %in% TRUE | cin[above]
    may <- is.na(o[above]) & !cin[above]
    first_cin <- which(cin[above])[1L]
    order_ok <- !is.na(first_cin) && all(!(o[above][seq_along(above) > first_cin] %in% TRUE))
    if (!cin[last] || is.na(first_cin) || !order_ok) FALSE
    else if (!any(o[above] %in% TRUE) && !any(is.na(o[above]))) FALSE
    else if (all(ok) && any(o[above] %in% TRUE)) TRUE
    else if (all(ok | may)) NA else FALSE
  }
  # (b) organic with the cindery materials under it, 40 cm within 50 cm
  first_org <- which(!(o %in% FALSE))[1L]
  cin_below <- cin & !is.na(first_org) & seq_len(x$n) > first_org
  pb <- ifelse(cin_below, TRUE, o)
  rb <- .cst_thk_range(x, pb, s, s + t$with_cindery_within_cm)
  b <- .cst_at_least(rb[["low"]], rb[["high"]], t$with_cindery_min_cm)
  # (c) two thirds of the soil above a contact, mineral layers 10 cm or less
  c3 <- if (is.na(ct) || ct <= s) FALSE else {
    total <- ct - s
    ro <- .cst_thk_range(x, o, s, ct)
    rm <- .cst_thk_range(x, .cst_not_v(o), s, ct)
    frac <- t$two_thirds
    if (ro[["low"]] >= frac * total - 1e-9 && rm[["high"]] <= t$mineral_max_cm) TRUE
    else if (ro[["high"]] < frac * total - 1e-9 || rm[["low"]] > t$mineral_max_cm) FALSE else NA
  }
  # (d) saturated, near the surface, thick enough for its kind
  drained <- isTRUE(.cst_site_lgl(pedon$site, "artificially_drained"))
  sat <- .cst_v(x, "water_saturation_days")
  sat_ok <- if (drained) TRUE else {
    sv <- sat[o %in% TRUE]
    if (any(sv >= t$saturated_days_min, na.rm = TRUE)) TRUE
    else if (length(sv) && !anyNA(sv)) FALSE else NA
  }
  top_ok <- {
    k <- which(o %in% TRUE)[1L]; kk <- which(!(o %in% FALSE))[1L]
    if (!is.na(k) && x$top[k] - s <= t$top_within_cm) TRUE
    else if (is.na(kk) || x$top[kk] - s > t$top_within_cm) FALSE else NA
  }
  rd <- .cst_thk_range(x, o, s, Inf)
  orgrows <- which(o %in% TRUE)
  wmean <- function(v) {
    th <- x$thk[orgrows]
    if (!length(orgrows) || anyNA(v[orgrows]) || anyNA(th)) NA_real_ else sum(v[orgrows] * th) / sum(th)
  }
  sph <- wmean(.cst_v(x, "sphagnum_fibres_pct")); bd <- wmean(.cst_v(x, "bulk_density_g_cm3"))
  moss <- .cst_or(sph >= t$moss_min_pct, bd < t$bd_low_max)
  kinds <- r$kind[orgrows]
  dom <- if (!length(orgrows) || anyNA(kinds)) NA_character_ else {
    th <- tapply(x$thk[orgrows], kinds, sum); names(th)[which.max(th)]
  }
  c40 <- if (is.na(dom)) NA else if (dom %in% c("sapric", "hemic")) TRUE
         else if (dom == "fibric") .cst_and(sph < t$moss_min_pct, bd >= t$bd_low_max, bd <= t$bd_fibric_max)
         else FALSE
  th60 <- .cst_at_least(rd[["low"]], rd[["high"]], t$thick_moss_cm)
  th40 <- .cst_at_least(rd[["low"]], rd[["high"]], t$thick_other_cm)
  d <- .cst_and(sat_ok, top_ok, .cst_or(.cst_and(th60, .cst_or(moss, c40)), .cst_and(th40, c40)))
  passed <- .cst_or(a, b, c3, d)
  .cst_key_result(nm, passed, layers = x$idx[orgrows], missing = c(r$missing, if (is.na(d)) c(
                    "water_saturation_days", "sphagnum_fibres_pct", "bulk_density_g_cm3", "fiber_content_rubbed_pct")),
                  flags = r$flags,
                  evidence = list(items = c(a = a, b = b, c = c3, d = d), organic_thickness = rd,
                                  dominant_kind = dom),
                  notes = "cindery, fragmental or pumiceous: coarse fragments 90 % or pyroclastic fragments 35 % (soilKey's reading)",
                  page = 72)
}

# Vectorised three-valued NOT.
.cst_not_v <- function(v) ifelse(is.na(v), NA, !v)


# --- an argic horizon above the spodic horizon (C) ----------------------------------------

#' An argic horizon above the spodic horizon (key to orders, C, p. 73): an
#' argic layer whose top lies above the top of the first spodic layer.
#' @noRd
argic_above_spodic_cst <- function(pedon) {
  nm <- "argic_above_spodic_cst"
  x <- .cst_ctx(pedon)
  if (!x$n) return(.cst_key_result(nm, FALSE, page = 73))
  sp <- .cst_hz(x, "spodic_horizon")$full
  ag <- .cst_hz(x, "argic_horizon")$full
  k <- which(sp %in% TRUE)[1L]
  if (is.na(k)) {
    # no spodic horizon (or only undecided ones): the Spodosol gate fails or
    # stays undecided on the spodic horizon itself
    return(.cst_key_result(nm, if (any(is.na(sp)) && any(!(ag %in% FALSE))) NA else FALSE, page = 73,
                           missing = .cst_hz(x, "argic_horizon")$missing))
  }
  above <- which(!is.na(x$top) & x$top < x$top[k])
  passed <- if (any(ag[above] %in% TRUE)) TRUE else if (any(is.na(ag[above]))) NA else FALSE
  .cst_key_result(nm, passed, layers = x$idx[above[ag[above] %in% TRUE]],
                  missing = .cst_hz(x, "argic_horizon")$missing, page = 73)
}


# --- the base saturation of the Isohumosols (J 3) -----------------------------------------

#' Base saturation (NH4OAc) of 50 % or more in every horizon of the window of
#' the Isohumosols (key to orders, J 3, p. 74): from the mineral soil surface
#' to 180 cm, to 125 cm below the top of an argic horizon, or to a lithic or
#' paralithic contact, whichever is shallowest. An argic horizon left
#' undecided gives the answer only when both windows agree.
#' @noRd
isohumosol_base_saturation_cst <- function(pedon) {
  nm <- "isohumosol_base_saturation_cst"
  x <- .cst_ctx(pedon)
  if (!x$n || is.na(x$surf) || is.na(x$msurf))
    return(.cst_key_result(nm, if (x$n) NA else FALSE, missing = c("top_cm", "designation"), page = 74))
  t <- .cst_thr("key", "isohumosols")
  ms <- x$msurf
  ag <- .cst_hz(x, "argic_horizon")$full
  ct <- .cst_contact_cm(pedon)
  depth <- function(argic_rows) {
    d <- c(ms + t$depth_cm,
           if (length(argic_rows)) x$top[argic_rows[1L]] + t$below_argic_cm,
           if (!is.na(ct)) ct)
    min(d)
  }
  # a window reaching below the description (Hugo, 2026-11-06): under a
  # calcareous lowest horizon (CaCO3 over 0 or secondary carbonates) the
  # soil below is read as base-saturated; otherwise the window stops at the
  # base of the description and a pass is provisional
  deep <- suppressWarnings(max(x$bot, na.rm = TRUE))
  low <- which(!is.na(x$bot) & x$bot == deep)[1L]
  calc <- !is.na(low) && ((.cst_v(x, "caco3_pct")[low] > 0) %in% TRUE ||
                            (.cst_v(x, "secondary_carbonates_pct")[low] > 0) %in% TRUE)
  flags <- character(0)
  bs <- function(d) {
    if (is.finite(deep) && d > deep) {
      flags <<- unique(c(flags, if (calc) "base_saturated_below_calcareous_base" else "window_cut_at_description_base"))
      d <- deep
    }
    base_saturation_cst(pedon, "eutric", top = ms - x$surf, bottom = d - x$surf)
  }
  d1 <- depth(which(ag %in% TRUE))
  r1 <- bs(d1)
  passed <- r1$passed
  if (any(is.na(ag))) {
    r2 <- bs(depth(which(!(ag %in% FALSE))))
    if (!identical(r1$passed, r2$passed)) passed <- NA
  }
  if (isTRUE(passed) && "window_cut_at_description_base" %in% flags) flags <- c(flags, "provisional")
  .cst_key_result(nm, passed, layers = r1$layers, missing = c(r1$missing, if (any(is.na(ag))) "clay_pct"),
                  flags = flags,
                  evidence = list(window = c(top = ms, bottom = d1)),
                  notes = sprintf("base saturation window %s-%s cm%s", format(ms), format(d1),
                                  if (length(flags)) sprintf(" (the description ends at %s cm)", format(deep)) else ""),
                  page = 74)
}


# --- runs of layers with a feature in a depth window ----------------------------------------

#' A feature (a horizon or layer characteristic id: gleyic_features,
#' redoxic_features ...) in layers within the window [from_cm, to_cm) below
#' the mineral soil surface (`datum = "mineral"`) or the soil surface:
#' `total = FALSE` asks for one horizon (a run of contiguous layers that
#' share it, default 6 of 2026-11-04) with min_cm or more of it inside the
#' window; `total = TRUE` for layers totalling min_cm. A result resting on a
#' provisional layer (gley colours without a test, Q11) is marked
#' provisional.
#' @noRd
layer_run_cst <- function(pedon, feature, min_cm, from_cm = 0, to_cm, total = FALSE,
                          datum = c("mineral", "surface"), grade = c("full", "at_least_evidence")) {
  datum <- match.arg(datum)
  grade <- match.arg(grade)
  nm <- "layer_run_cst"
  x <- .cst_ctx(pedon)
  if (!x$n) return(.cst_key_result(nm, FALSE))
  f <- .cst_layer_feature(x, feature, grade)
  s <- if (datum == "mineral") x$msurf else x$surf
  if (is.na(s)) return(.cst_key_result(nm, NA, missing = c("designation", "oc_pct")))
  a <- s + from_cm; b <- s + to_cm
  ov <- .cst_overlap(x, a, b); ov[is.na(ov)] <- 0
  p <- f$full
  # rock carries no feature; the gleyic and redoxic features are of a
  # mineral matrix (Hugo, 2026-11-06)
  p[x$kind %in% "R"] <- FALSE
  if (feature %in% c("gleyic_features", "redoxic_features")) p[x$kind %in% "O"] <- FALSE
  if (total) {
    lo <- sum(ov[p %in% TRUE]); hi <- sum(ov[!(p %in% FALSE)])
    used <- which(p %in% TRUE & ov > 0)
  } else {
    runs <- function(q) {
      best <- 0; rows <- integer(0); i <- 1L
      while (i <= x$n) {
        if (q[i]) {
          j <- i
          while (j < x$n && q[j + 1L]) j <- j + 1L
          v <- sum(ov[i:j])
          if (v > best) { best <- v; rows <- i:j }
          i <- j + 1L
        } else i <- i + 1L
      }
      list(v = best, rows = rows)
    }
    rl <- runs(p %in% TRUE); rh <- runs(!(p %in% FALSE))
    lo <- rl$v; hi <- rh$v
    used <- rl$rows[ov[rl$rows] > 0]
  }
  passed <- .cst_at_least(lo, hi, min_cm)
  prov <- isTRUE(passed) && all(f$prov[used] %in% TRUE)
  .cst_key_result(nm, passed, layers = x$idx[used], missing = f$missing,
                  flags = c(f$flags, if (prov) "provisional"),
                  evidence = list(feature = feature, thickness = c(low = lo, high = hi),
                                  window = c(top = a, bottom = b), total = total),
                  notes = sprintf("%s in %s %s cm or more within %s-%s cm of the %s", gsub("_", " ", feature),
                                  if (total) "layers totalling" else "a horizon", format(min_cm),
                                  format(from_cm), format(to_cm),
                                  if (datum == "mineral") "mineral soil surface" else "soil surface"))
}


# --- the claypan with illuviation argillans (L 2) ---------------------------------------------

#' A claypan with illuviation argillans 0.5 mm or more thick in some part,
#' starting within `top_max` cm of the mineral soil surface (key to orders, L
#' 2, p. 75). The thickness is clay_films_thickness_mm, or distinct or
#' prominent clay films read as thick, flagged (C13); not recorded, absent.
#' @noRd
claypan_argillans_cst <- function(pedon, top_max = 100) {
  nm <- "claypan_argillans_cst"
  cp <- claypan_cst(pedon, top_max = top_max, datum = "mineral")
  if (!isTRUE(cp$passed) && !is.na(cp$passed))
    return(.cst_key_result(nm, FALSE, page = 75))
  x <- .cst_ctx(pedon)
  f <- .cst_clay_films(x, min_mm = 0.5, strict = FALSE)
  rows <- match(cp$layers, x$idx)
  full <- .cst_hz(x, "claypan")$full
  if (is.na(cp$passed)) rows <- which(is.na(full))
  thick <- f$thick[rows]
  ok <- any(thick %in% TRUE)
  passed <- if (isTRUE(cp$passed)) ok else if (ok) NA else FALSE
  .cst_key_result(nm, passed, layers = x$idx[rows[thick %in% TRUE]], missing = cp$missing,
                  flags = f$flag[rows[thick %in% TRUE]],
                  notes = "illuviation argillans not recorded are absent", page = 75)
}


# --- the humic subhorizons of the Humic Spodosols (C1) ---------------------------------------

#' Subhorizons of the spodic horizon 10 cm or more thick with 60 g/kg organic
#' carbon or more (C1, p. 88): contiguous spodic layers with that carbon,
#' totalling 10 cm (C5).
#' @noRd
humic_spodic_cst <- function(pedon) {
  nm <- "humic_spodic_cst"
  x <- .cst_ctx(pedon)
  if (!x$n) return(.cst_key_result(nm, FALSE, page = 88))
  t <- .cst_thr("key", "humic_spodosols")
  sp <- .cst_hz(x, "spodic_horizon")$full
  oc <- .cst_v(x, "oc_pct")
  p <- ifelse(sp %in% FALSE, FALSE, ifelse(is.na(oc), NA, sp & oc >= .cst_pct(t$humic_oc_min_gkg)))
  g <- .cst_run_grade(x, p, t$humic_thick_min_cm)
  passed <- if (any(g %in% TRUE)) TRUE else if (any(is.na(g))) NA else FALSE
  .cst_key_result(nm, passed, layers = x$idx[which(g %in% TRUE)],
                  missing = c(if (anyNA(oc[!(sp %in% FALSE)])) "oc_pct", .cst_hz(x, "spodic_horizon")$missing),
                  page = 88)
}


# --- the texture of the Vitric Andosols (D2) --------------------------------------------------

#' A particle-size distribution coarser than silt loam, by the weighted
#' average of the horizons within 100 cm of the mineral soil surface or above
#' a shallower lithic contact (D2, p. 89): the texture class of the weighted
#' clay, silt and sand is sand, loamy sand or sandy loam (Hugo,
#' 2026-11-06; loam is not coarser than silt loam here).
#' @noRd
vitric_texture_cst <- function(pedon) {
  nm <- "vitric_texture_cst"
  x <- .cst_ctx(pedon)
  if (!x$n || is.na(x$msurf)) return(.cst_key_result(nm, if (x$n) NA else FALSE,
                                                      missing = c("designation", "oc_pct"), page = 89))
  t <- .cst_thr("key", "vitric_andosols")
  a <- x$msurf; b <- a + t$window_cm
  ct <- .cst_contact_cm(pedon)
  if (!is.na(ct) && ct < b) b <- ct
  if (b <= a) return(.cst_key_result(nm, FALSE, page = 89))
  w <- lapply(c("clay_pct", "silt_pct", "sand_pct"), function(k) .cst_weighted(pedon, k, a, b)$value)
  names(w) <- c("clay_pct", "silt_pct", "sand_pct")
  if (is.na(w$silt_pct) && !is.na(w$clay_pct) && !is.na(w$sand_pct)) w$silt_pct <- 100 - w$clay_pct - w$sand_pct
  if (is.na(w$sand_pct) && !is.na(w$clay_pct) && !is.na(w$silt_pct)) w$sand_pct <- 100 - w$clay_pct - w$silt_pct
  cls <- .wrb_texture_class(data.frame(clay_pct = w$clay_pct, silt_pct = w$silt_pct, sand_pct = w$sand_pct))
  passed <- if (is.na(cls)) NA else cls %in% unlist(t$coarser_than_silt_loam)
  .cst_key_result(nm, passed, missing = c("clay_pct", "silt_pct", "sand_pct"),
                  evidence = list(texture_class = cls, window = c(top = a, bottom = b), weighted = unlist(w)),
                  notes = sprintf("weighted texture of %s-%s cm: %s", format(a), format(b), cls %||% "NA"),
                  page = 89)
}


# --- the crack days of the Ustic Vertosols (F2) -----------------------------------------------

#' Cracks 5 mm or more wide through 25 cm or more within 50 cm of the
#' mineral soil surface for `days` cumulative days a year in most years,
#' unless irrigated (F2, p. 96; the Aridic subgroups ask 210). Hugo's
#' decision Q15: site$cracks_open_days when recorded; else cracks observed in
#' the profile stand for the 90 days under an ustic regime and for the 210
#' days under an aridic one only, provisional; no cracks recorded, absent.
#' Irrigated land passes (the clause does not apply).
#' @noRd
crack_days_cst <- function(pedon, days = 90) {
  nm <- "crack_days_cst"
  t <- .cst_thr("characteristics", "vertic")
  site <- pedon$site %||% list()
  ir <- .cst_irrigated(site)
  if (isTRUE(ir$value))
    return(.cst_key_result(nm, TRUE, notes = "irrigated: the crack clause does not apply", page = 96))
  cd <- .cst_site_num(site, "cracks_open_days")
  if (!is.na(cd)) return(.cst_key_result(nm, cd >= days, notes = sprintf("cracks open %s days", format(cd)),
                                         page = 96))
  x <- .cst_ctx(pedon)
  if (!x$n || is.na(x$surf)) return(.cst_key_result(nm, NA, missing = "cracks_open_days", page = 96))
  wd <- .cst_v(x, "cracks_width_cm")
  obs <- .cst_feature_run(x, (wd >= t$crack_width_min_cm) %in% TRUE, t$crack_run_min_cm, t$window_cm)
  if (!obs) {
    return(.cst_key_result(nm, if (is.na(ir$value)) FALSE else FALSE,
                           notes = "no cracks recorded: absent", page = 96))
  }
  mr <- .cst_moisture_regime(pedon)$climatic
  regime_ok <- if (days <= 90) all(mr$candidates %in% c("ustic", "aridic")) else all(mr$candidates %in% "aridic")
  .cst_key_result(nm, if (regime_ok) TRUE else NA,
                  missing = c("cracks_open_days", if (is.na(ir$value)) "irrigated"),
                  flags = if (regime_ok) "provisional",
                  notes = sprintf("cracks observed read as open %s days under the %s regime (Hugo's decision Q15)",
                                  format(days), paste(mr$candidates, collapse = " or ")),
                  page = 96)
}


# --- the low n value or clay of the Cambosols (M 3) --------------------------------------------

#' In some horizon 10 cm or more thick between 20 and 50 cm below the mineral
#' soil surface, an n value under 0.7 (printed "less 0.7") or less than 80
#' g/kg clay in the fine earth (key to orders, M 3, p. 75).
#' @noRd
low_n_or_clay_cst <- function(pedon) {
  nm <- "low_n_or_clay_cst"
  x <- .cst_ctx(pedon)
  if (!x$n || is.na(x$msurf)) return(.cst_key_result(nm, if (x$n) NA else FALSE, page = 75))
  t <- .cst_thr("key", "cambosols")
  nv <- .cst_ch(x, "n_value")$full
  low_n <- .cst_not_v(nv)
  # an n value not measured is under 0.7 in a layer that is not saturated
  # (Hugo, 2026-11-06): no saturation regime read (a well-drained record),
  # or the layer recorded saturated under 30 days
  sat_none <- identical(.cst_moisture_regime(pedon)$saturation$regime, "none")
  sd <- .cst_v(x, "water_saturation_days")
  dry <- sat_none | (sd < 30) %in% TRUE
  low_n[is.na(low_n) & dry] <- TRUE
  clay <- .cst_v(x, "clay_pct") < .cst_pct(t$clay_max_gkg)
  p <- ifelse(low_n %in% TRUE | clay %in% TRUE, TRUE, ifelse(low_n %in% FALSE & clay %in% FALSE, FALSE, NA))
  p[x$kind %in% c("O", "R")] <- FALSE
  a <- x$msurf + t$window_top_cm; b <- x$msurf + t$window_bottom_cm
  ov <- .cst_overlap(x, a, b); ov[is.na(ov)] <- 0
  best <- function(q) {
    v <- 0; i <- 1L
    while (i <= x$n) {
      if (q[i]) { j <- i; while (j < x$n && q[j + 1L]) j <- j + 1L; v <- max(v, sum(ov[i:j])); i <- j + 1L }
      else i <- i + 1L
    }
    v
  }
  passed <- .cst_at_least(best(p %in% TRUE), best(!(p %in% FALSE)), t$thick_min_cm)
  .cst_key_result(nm, passed, layers = x$idx[which(p %in% TRUE & ov > 0)], missing = c("n_value", "clay_pct"),
                  page = 75)
}


# --- irrigation and anthro-silting within 50 cm (M3, N1) -------------------------------------

#' Irrigated land (M3, p. 146): site$irrigated, which flood_irrigated
#' implies.
#' @noRd
irrigated_cst <- function(pedon) {
  ir <- .cst_irrigated(pedon$site %||% list())
  .cst_key_result("irrigated_cst", ir$value, missing = ir$missing,
                  notes = if (!is.na(ir$source)) sprintf("read from %s", ir$source), page = 146)
}

#' Anthro-silting materials within `top_max` cm of the mineral soil surface
#' (N1, p. 167): the profile characteristic, with a layer it rests on (the
#' stratified or sieve-tested layers) starting there.
#' @noRd
anthro_silting_within_cst <- function(pedon, top_max = 50) {
  r <- anthro_silting_materials_cst(pedon)
  if (!isTRUE(r$passed)) {
    return(.cst_key_result("anthro_silting_within_cst", r$passed, missing = r$missing,
                           flags = r$evidence$flags, page = 167))
  }
  x <- .cst_ctx(pedon)
  pos <- match(r$layers, x$idx)
  ok <- !is.na(x$msurf) && any(x$top[pos] - x$msurf <= top_max, na.rm = TRUE)
  .cst_key_result("anthro_silting_within_cst", if (is.na(x$msurf)) NA else ok, layers = r$layers,
                  flags = r$evidence$flags, missing = "designation", page = 167)
}


# --- the classifier ---------------------------------------------------------------------------

# The correlation of Tables 4.2 and 4.3, read once.
.cst_correlation <- function() {
  if (is.null(.cst_cache$correlation)) {
    f <- system.file("rules", "cst2001", "correlation.yaml", package = "soilKey")
    .cst_cache$correlation <- if (nzchar(f)) yaml::read_yaml(f) else list()
  }
  .cst_cache$correlation
}

# The correlation of one order, as a list (order, name, us_st_1999 and
# wrb_1998 data frames of degree and class), NULL when the order has none.
.cst_order_correlation <- function(code) {
  cr <- .cst_correlation()$correlation
  e <- Find(function(z) identical(as.character(z$order), as.character(code)), cr)
  if (is.null(e)) return(NULL)
  df <- function(v) data.frame(degree = vapply(v, function(z) as.character(z$degree), ""),
                               class = vapply(v, function(z) as.character(z$class), ""),
                               stringsAsFactors = FALSE)
  list(order = as.character(e$order), name = as.character(e$name),
       us_st_1999 = df(e$us_st_1999), wrb_1998 = df(e$wrb_1998))
}

# The correlation of a result as lines for the report and the app ("US Soil
# Taxonomy (1999), Table 4.2: mostly Histosols; partly Gelisols"); NULL for a
# result without it.
.cst_correlation_lines <- function(res) {
  cr <- tryCatch(res$qualifiers$correlation_cst, error = function(e) NULL)
  if (!is.list(cr) || is.null(cr$order)) return(NULL)
  one <- function(d) {
    if (!is.data.frame(d) || !nrow(d)) return("")
    deg <- unique(d$degree)
    paste(vapply(deg, function(g) sprintf("%s %s", g, paste(d$class[d$degree == g], collapse = ", ")), ""),
          collapse = "; ")
  }
  c(sprintf("%s, US Soil Taxonomy (1999), Table 4.2: %s.", cr$name, one(cr$us_st_1999)),
    sprintf("%s, World Reference Base (1998), Table 4.3: %s.", cr$name, one(cr$wrb_1998)))
}

# Every DiagnosticResult evidence below a trace entry marked provisional
# (a reading the book's text leaves open: gley colours without a test, cracks
# for their duration ...).
.cst_trace_provisional <- function(node) {
  if (is.null(node) || !is.list(node)) return(FALSE)
  if (isTRUE(node$provisional)) return(TRUE)
  if (!is.null(node$passed) && !isTRUE(node$passed)) return(FALSE)
  ev <- node$evidence
  if (is.list(ev) && isTRUE(ev$provisional)) return(TRUE)
  kids <- if (is.list(ev) && is.null(names(ev))) ev else if (is.list(ev)) ev[vapply(ev, is.list, NA)] else list()
  any(vapply(kids, .cst_trace_provisional, NA))
}

# The flags of the key criteria that passed below a trace entry (v0.9.374:
# the stand-ins of the group keys).
.cst_trace_flags <- function(node) {
  if (is.null(node) || !is.list(node)) return(character(0))
  if (!is.null(node$passed) && !isTRUE(node$passed)) return(character(0))
  ev <- node$evidence
  out <- if (is.list(ev) && is.character(ev$flags)) ev$flags else character(0)
  kids <- if (is.list(ev) && is.null(names(ev))) ev else if (is.list(ev)) ev[vapply(ev, is.list, NA)] else list()
  unique(c(out, unlist(lapply(kids, .cst_trace_flags))))
}

# The stand-ins of the group keys that the result names in a warning
# (Hugo, 2026-11-07), with their wording.
.CST_KEY_STAND_INS <- c(
  "stand_in:drainage_class" = "a drainage class of well drained or better for the days of saturation",
  "stand_in:crust_thickness" = "the thickness of the surface crust for the depth of the cracks")

# The horizon panel (.cst_identify()) as trace entries, one per layer: the
# designation and depths as the code, the reading as the name.
.cst_panel_trace <- function(pedon) {
  fr <- tryCatch(.cst_identify(pedon), error = function(e) NULL)
  if (is.null(fr) || !nrow(fr)) return(NULL)
  out <- lapply(seq_len(nrow(fr)), function(i) {
    list(code = sprintf("%s %s-%s", ifelse(is.na(fr$designation[i]), "?", fr$designation[i]),
                        format(fr$top_cm[i]), format(fr$bottom_cm[i])),
         name = fr$evidence[i],
         passed = fr$passed[i],
         missing = character(0),
         diagnostics = fr$diagnostics[[i]], evidences = fr$evidences[[i]],
         undecided = fr$undecided[[i]], characteristics = fr$characteristics[[i]])
  })
  names(out) <- vapply(out, `[[`, "", "code")
  attr(out, "cst_panel") <- TRUE
  out
}

# The class the key reaches when the gate of the class at row `i` of the
# levels (assigned on a provisional reading) is set to FALSE: a one-row data
# frame (kind "alternative", the level and class replaced, the deepest class
# reached), or NULL when nothing is reached.
.cst_alternative <- function(pedon, rules, lv, i, gapfill = FALSE) {
  r2 <- rules
  never <- list(any_of = list())
  if (lv$level[i] == "orders") {
    k <- which(vapply(r2$orders, function(z) identical(as.character(z$code), lv$code[i]), NA))
    r2$orders[[k]]$tests <- never
  } else {
    parent <- lv$code[i - 1L]
    lst <- r2[[lv$level[i]]][[parent]]
    k <- which(vapply(lst, function(z) identical(as.character(z$code), lv$code[i]), NA))
    r2[[lv$level[i]]][[parent]][[k]]$tests <- never
  }
  a <- tryCatch(classify_hierarchical("cst2001", pedon, rules = r2, on_missing = "silent", gapfill = gapfill),
                error = function(e) NULL)
  if (is.null(a) || !is.data.frame(a$levels) || !nrow(a$levels)) return(NULL)
  n <- nrow(a$levels)
  data.frame(kind = "alternative", level = a$levels$level[n], code = a$levels$code[n],
             name = a$levels$name[n], path = paste(a$levels$code, collapse = " > "),
             instead_of = lv$code[i], provisional = length(a$provisional_because) > 0L,
             stringsAsFactors = FALSE)
}

#' Classify a pedon under the Chinese Soil Taxonomy (2001)
#'
#' Keys a \code{\link{PedonRecord}} through the Chinese Soil Taxonomy
#' (Cooperative Research Group on Chinese Soil Taxonomy, 2001, the English
#' edition published by Science Press). \strong{Experimental}: this release
#' keys the 14 orders (key 5.1: Histosols, Anthrosols, Spodosols, Andosols,
#' Ferralosols, Vertosols, Aridosols, Halosols, Gleyosols, Isohumosols,
#' Ferrosols, Argosols, Cambosols and Primosols), their 39 suborders and
#' their 138 groups (key 5.2), with soilKey's residuals \code{[E2] [Other
#' Ferralosols]}, \code{[A1.4] [Other Permagelic Histosols]} and
#' \code{[A2.5] [Other Orthic Histosols]} where the book prints none (the
#' name stops there, with a note). The 588 subgroups follow in a later
#' release.
#'
#' The key is read in the book's order, the first class whose criteria pass
#' assigned, as the book prescribes (each class lacks the properties of the
#' classes before it). The criteria read the 33 diagnostic horizons and the
#' 25 diagnostic characteristics of chapter 3 on the recorded data (see
#' \code{\link{identify_horizons}} with \code{"cst2001"}), with their
#' weaker evidence grades where the book gives them, the soil moisture and
#' temperature regimes (recorded on the site, or from the aridity index, the
#' site's records of paddy, groundwater and permafrost, and the air
#' temperature), and the few criteria the key defines where it uses them.
#' Every test is three-valued: a measurement not recorded leaves it
#' undecided, a field observation not recorded is absent. A class tested
#' before the assigned one that could not be decided makes the name
#' provisional (\code{provisional_because}); a class assigned on a reading
#' the book leaves open (gley colours without a ferrous-iron test, cracks
#' observed for their duration) carries a warning; a class the book itself
#' marks provisional is noted as such (\code{levels$book_provisional}).
#'
#' Names are the English of the book, in the plural of the key
#' ("Permagelic Histosols"), with soilKey's Portuguese gloss in
#' \code{levels$name_pt}. The approximate equivalents of the order in US Soil
#' Taxonomy (1999) and the WRB (1998), Tables 4.2 and 4.3, are added as a
#' note (\code{qualifiers$correlation_cst} and a line in \code{warnings});
#' they never change the classification. A residual suborder or group named
#' for the udic regime (the Udic Andosols, Vertosols, Isohumosols, Ferrosols,
#' Argosols and Cambosols, the Udi- groups) or for the aquic one (the
#' Aqui-Orthic Halosols) reached by a soil whose regime read is another
#' keeps its class, with a note. The diagnostic horizons and characteristics
#' of each layer are in the trace (phase \code{horizons}). The readings where
#' the text is silent are listed in \code{system.file("rules", "cst2001",
#' "readings.yaml", package = "soilKey")}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rules Optional pre-loaded rule set (\code{load_rules("cst2001")}).
#' @param on_missing One of \code{"warn"} (default), \code{"silent"},
#'        \code{"error"}.
#' @param gapfill Opt-in within-pedon gap-fill, as in
#'        \code{\link{classify_usda}}.
#' @return A \code{\link{ClassificationResult}} with \code{system_id}
#'   \code{"cst2001"}, the group (or the deepest class assigned) in
#'   \code{name}, the order in \code{rsg_or_order}, the classes
#'   of each level in \code{levels} (with \code{name_pt} and
#'   \code{book_provisional}), the key trace with the horizon panel,
#'   \code{provisional_because}, and, when a class was assigned on a
#'   provisional reading, in \code{candidates} the class the key would
#'   reach without it (\code{kind} "alternative").
#' @references Cooperative Research Group on Chinese Soil Taxonomy (2001).
#'   Chinese Soil Taxonomy. Science Press, Beijing and New York.
#' @seealso \code{\link{make_cst2001_example}}, \code{\link{identify_horizons}},
#'   \code{\link{soilkey_systems}}.
#' @examples
#' res <- classify_cst2001(make_cst2001_example("K3"), on_missing = "silent")
#' res$name
#' res$levels
#' @export
classify_cst2001 <- function(pedon, rules = NULL,
                             on_missing = c("warn", "silent", "error"),
                             gapfill = FALSE) {
  on_missing <- match.arg(on_missing)
  rules <- rules %||% load_rules("cst2001")
  res <- classify_hierarchical("cst2001", pedon, rules = rules,
                               on_missing = on_missing, gapfill = gapfill)
  lv <- res$levels
  if (!is.data.frame(lv) || !nrow(lv)) {
    if (inherits(pedon, "PedonRecord")) res$trace$horizons <- .cst_panel_trace(pedon)
    return(res)
  }
  entry <- function(level, code) {
    lst <- if (level == "orders") rules$orders else {
      parent <- lv$code[match(level, lv$level) - 1L]
      rules[[level]][[parent]]
    }
    Find(function(z) identical(as.character(z$code), code), lst)
  }
  ents <- lapply(seq_len(nrow(lv)), function(i) entry(lv$level[i], lv$code[i]))
  lv$name_pt <- vapply(ents, function(e) as.character(e$name_pt %||% NA_character_), "")
  lv$book_provisional <- vapply(ents, function(e) isTRUE(e$book_provisional), NA)
  res$levels <- lv
  notes <- character(0)
  for (i in which(lv$book_provisional))
    notes <- c(notes, sprintf("%s %s is marked provisional in CST 2001 (the book's own mark).",
                              lv$code[i], lv$name[i]))
  # a class assigned on a provisional reading
  prov_i <- integer(0)
  for (i in seq_len(nrow(lv))) {
    tr <- res$trace[[lv$level[i]]][[lv$code[i]]]
    if (!is.null(tr) && isTRUE(tr$passed) && .cst_trace_provisional(list(evidence = tr$evidence))) {
      prov_i <- c(prov_i, i)
      notes <- c(notes, sprintf(paste(
        "Provisional: %s %s rests on a reading the book leaves open (see the trace):",
        "a ferrous-iron test, a measurement or a duration would confirm it."), lv$code[i], lv$name[i]))
    }
  }
  # a class assigned on a stand-in of a group key (Hugo, 2026-11-07)
  for (i in seq_len(nrow(lv))) {
    tr <- res$trace[[lv$level[i]]][[lv$code[i]]]
    if (is.null(tr) || !isTRUE(tr$passed)) next
    si <- intersect(names(.CST_KEY_STAND_INS), .cst_trace_flags(list(evidence = tr$evidence)))
    if (length(si))
      notes <- c(notes, sprintf("Stand-in: %s %s rests on %s (flagged in the trace).", lv$code[i], lv$name[i],
                                paste(.CST_KEY_STAND_INS[si], collapse = " and ")))
  }
  # the class the key would reach if that reading failed (Hugo, 2026-11-06):
  # the key run again with the gate of the first such class set to FALSE,
  # given in $candidates as an alternative, never as the class
  if (length(prov_i) && inherits(pedon, "PedonRecord")) {
    alt <- .cst_alternative(pedon, rules, lv, prov_i[1L], gapfill)
    if (!is.null(alt)) {
      res$candidates <- alt
      tr_alt <- list(list(code = alt$code, name = sprintf("%s (alternative if the reading of %s fails)", alt$name, alt$instead_of),
                          missing = character(0)))
      names(tr_alt) <- alt$code
      attr(tr_alt, "cst_panel") <- TRUE
      res$trace$alternative <- tr_alt
      notes <- c(notes, sprintf("Alternative if that reading fails: %s %s.", alt$code, alt$name))
    }
  }
  # a residual named for a regime (the udic one; the aquic one for the
  # Aqui-Orthic Halosols, v0.9.374), reached under another regime
  for (i in seq_len(nrow(lv))) {
    nf <- ents[[i]]$residual_named_for
    if (is.null(nf) || !inherits(pedon, "PedonRecord")) next
    mr <- tryCatch(.cst_moisture_regime(pedon), error = function(e) NULL)
    ok <- if (identical(nf, "aquic")) c("aquic", "peraquic") else c("udic", "perudic")
    cand <- if (identical(nf, "aquic")) mr$saturation$candidates else mr$climatic$candidates
    if (length(cand) && !any(cand %in% ok))
      notes <- c(notes, sprintf(paste(
        "%s %s is the residual of its key, named for the %s regime; the moisture regime read is %s.",
        "The key places the soil here all the same (the class does not change)."),
        lv$code[i], lv$name[i], nf, paste(cand, collapse = " or ")))
  }
  # soilKey's bracketed residual, where the book prints none (Q4): the name
  # stops there (v0.9.374)
  last <- ents[[nrow(lv)]]
  if (isTRUE(last$bracketed))
    notes <- c(notes, sprintf(paste(
      "%s %s is soilKey's residual: the book prints no class for this soil at this level",
      "(Hugo's decision Q4); the name stops there."), lv$code[nrow(lv)], lv$name[nrow(lv)]))
  # the correlation of the order (Tables 4.2 and 4.3), a note only
  cr <- .cst_order_correlation(lv$code[1L])
  if (!is.null(cr)) {
    res$qualifiers$correlation_cst <- cr
    notes <- c(notes, sprintf("Correlation (a note, never a class): %s",
                              paste(.cst_correlation_lines(res), collapse = " ")))
  }
  res$warnings <- c(res$warnings, notes)
  if (inherits(pedon, "PedonRecord")) res$trace$horizons <- .cst_panel_trace(pedon)
  res
}


# --- the examples ----------------------------------------------------------------------------

# A PedonRecord from columns (scalars recycled) and the site. The columns a
# profile does not give get the values that rule out what it is not: low
# oxalate Al and Fe and no volcanic glass (no andic property), few soluble
# salts, a low ESP, no gypsum and no carbonate.
.cst_ex_build <- function(h, site) {
  n <- max(lengths(h))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  h <- as.data.frame(h, stringsAsFactors = FALSE)
  # the data that rule out what the profile is not: low oxalate Al and Fe
  # (no andic property), few soluble salts, a low ESP, no gypsum
  defaults <- list(al_ox_pct = 0.1, fe_ox_pct = 0.2, soluble_salts_pct = 0.05, esp_pct = 2, caso4_pct = 0, caco3_pct = 0, volcanic_glass_pct = 0)
  for (k in names(defaults)) if (is.null(h[[k]])) h[[k]] <- defaults[[k]]
  site <- utils::modifyList(list(country = "CN", drainage_class = "well drained"), site)
  PedonRecord$new(site = site, horizons = ensure_horizon_schema(data.table::as.data.table(h)))
}

.CST_EXAMPLE_CODES <- c("A1", "A2", "B1", "B2", "C1", "C2", "D1", "D2", "D3", "E1", "F1", "F2", "F3",
                        "G1", "G2", "H1", "H2", "I1", "I2", "I3", "J1", "J2", "J3", "K1", "K2", "K3",
                        "L1", "L2", "L3", "L4", "M1", "M2", "M3", "M4", "M5", "N1", "N2", "N3", "N4")
.CST_NEAR_MISS_CODES <- c("A1", "B2", "C1", "D1", "E1", "F1", "G1", "H1", "I1", "J2", "K1", "L1", "M3", "N2")

#' Example pedons for the suborders and groups of the Chinese Soil Taxonomy (2001)
#'
#' Small, plausible profiles written for the tests and examples, one per
#' suborder of the Chinese Soil Taxonomy (2001), with FAO designations, the
#' regimes on the site and the data the key reads; each keys to its own
#' suborder, not provisionally (with the group it reaches). Since v0.9.374 a
#' group code (\code{"A1.1"} ... \code{"N4.7"}, and soilKey's
#' \code{"[A1.4]"} and \code{"[A2.5]"}) gives one profile per group, 140 in
#' all, each keying to its own group, not provisionally. They are not real
#' profiles. With
#' \code{near_miss = TRUE}, fourteen of them (one per order) change one
#' property so that the profile misses its class and lands in another:
#' A1 without permafrost (Orthic Histosols), B2 with a cumulic layer of 35 cm
#' (cumulic evidence only: Orthic Primosols), C1 with 40 g/kg carbon in the
#' Bhs (Orthic Spodosols), D1 frigid (Udic Andosols), E1 with a CEC of 18
#' cmol/kg clay (a LAC-ferric horizon: Udic Ferrosols), F1 with rock at 45 cm
#' (Aquic Cambosols), G1 without the crust and pavement (Udic Cambosols, with
#' the note on the residual's name), H1 with an ESP of 22 (alkalic evidence:
#' Ustic Argosols), I1 without permafrost (Orthic Gleyosols), J2 with its
#' humus near the surface (no isohumic property: Ustic Cambosols), K1 with a
#' CEC of 26 cmol/kg clay (Ustic Argosols), L1 gelic (Udic Argosols), M3
#' aridic and not irrigated (Udic Cambosols, with the note) and N2 with 2
#' g/kg carbon (Orthic Primosols).
#'
#' @param suborder The code of the suborder, as printed (\code{"A1"} ...
#'   \code{"N4"}), or of a group (\code{"A1.1"} ... \code{"N4.7"}).
#' @param near_miss Give the near miss of the suborder (one per order:
#'   \code{"A1"}, \code{"B2"}, \code{"C1"}, \code{"D1"}, \code{"E1"},
#'   \code{"F1"}, \code{"G1"}, \code{"H1"}, \code{"I1"}, \code{"J2"},
#'   \code{"K1"}, \code{"L1"}, \code{"M3"}, \code{"N2"}).
#' @return A \code{\link{PedonRecord}}.
#' @seealso \code{\link{classify_cst2001}}.
#' @examples
#' p <- make_cst2001_example("E1")
#' p$horizons[, c("designation", "top_cm", "bottom_cm", "clay_pct", "cec_cmol")]
#' classify_cst2001(make_cst2001_example("E1", near_miss = TRUE), on_missing = "silent")$name
#' classify_cst2001(make_cst2001_example("K3.2"), on_missing = "silent")$levels
#' @export
make_cst2001_example <- function(suborder = .CST_EXAMPLE_CODES, near_miss = FALSE) {
  # v0.9.374: a group code gives the example of the group
  if (length(suborder) == 1L && suborder %in% .CST_GROUP_EXAMPLE_CODES) {
    if (isTRUE(near_miss)) rlang::abort(sprintf("No near miss for %s; near misses exist for %s.", suborder,
                                                paste(.CST_NEAR_MISS_CODES, collapse = ", ")))
    return(.cst_group_example(suborder))
  }
  suborder <- match.arg(suborder)
  nm <- isTRUE(near_miss)
  if (nm && !suborder %in% .CST_NEAR_MISS_CODES)
    rlang::abort(sprintf("No near miss for %s; near misses exist for %s.", suborder,
                         paste(.CST_NEAR_MISS_CODES, collapse = ", ")))
  id <- paste0("cst2001-", suborder, if (nm) "-near-miss")
  mk <- function(h, site) .cst_ex_build(h, c(list(id = id), site))
  switch(suborder,
  A1 = mk(list(
    designation = c("Oi", "Oe", if (nm) "C" else "Cf"), top_cm = c(0, 25, 70), bottom_cm = c(25, 70, 100),
    oc_pct = c(42, 38, 2), clay_pct = c(NA, NA, 20), sand_pct = c(NA, NA, 40), silt_pct = c(NA, NA, 40),
    fiber_content_rubbed_pct = c(70, 45, NA), sphagnum_fibres_pct = c(80, 80, NA),
    bulk_density_g_cm3 = c(0.08, 0.09, 1.3), water_saturation_days = c(200, 200, 200),
    permafrost_temp_C = c(NA, NA, if (nm) NA else -1)),
    list(soil_temperature_regime = if (nm) "frigid" else "gelic", soil_moisture_regime = "aquic",
         drainage_class = "very poorly drained")),
  A2 = mk(list(
    designation = c("Oe", "Oa", "Cg"), top_cm = c(0, 30, 90), bottom_cm = c(30, 90, 120),
    oc_pct = c(35, 30, 1), clay_pct = c(NA, NA, 25), sand_pct = c(NA, NA, 30), silt_pct = c(NA, NA, 45),
    fiber_content_rubbed_pct = c(30, 10, NA), sphagnum_fibres_pct = c(5, 0, NA),
    bulk_density_g_cm3 = c(0.2, 0.25, 1.4), water_saturation_days = c(250, 250, 250)),
    list(soil_temperature_regime = "mesic", soil_moisture_regime = "aquic", drainage_class = "very poorly drained")),
  B1 = mk(list(
    designation = c("Ap1", "Ap2", "Bg", "C"), top_cm = c(0, 15, 22, 60), bottom_cm = c(15, 22, 60, 100),
    clay_pct = 25, sand_pct = 30, silt_pct = 45, oc_pct = c(1.8, 1.2, 0.6, 0.3),
    munsell_hue_moist = c("10YR", "10YR", "10YR", "2.5Y"), munsell_value_moist = c(4, 4, 5, 6),
    munsell_chroma_moist = c(1, 2, 4, 2), mottle_colors = c("7.5YR 5/6", NA, "7.5YR 5/8", NA),
    bulk_density_g_cm3 = c(1.2, 1.45, 1.45, 1.5), fe_dcb_pct = c(1.2, 1.2, 2.1, 1.2),
    humus_clay_films_mm = c(NA, NA, 0.6, NA), structure_type = c("granular", "platy", "prismatic", "massive"),
    structure_grade = c("weak", "weak", "moderate", NA), ph_h2o = 6.2, cec_cmol = 15, bs_pct = 70,
    childs_test = c("negative", "negative", "negative", NA)),
    list(flood_irrigated = TRUE, land_use = "paddy rice", soil_moisture_regime = "udic",
         soil_temperature_regime = "thermic", drainage_class = "imperfectly drained")),
  B2 = {
    k <- if (nm) c(1, 2, 4) else 1:4
    mk(lapply(list(
      designation = c("Aup1", "Aup2", "Aup3", "2C"), top_cm = c(0, 20, 40, if (nm) 35 else 60),
      bottom_cm = c(20, if (nm) 35 else 40, 60, 130),
      clay_pct = c(18, 18, 18, 5), silt_pct = c(60, 60, 60, 10), sand_pct = c(22, 22, 22, 85),
      munsell_hue_moist = c("10YR", "10YR", "10YR", "2.5Y"), munsell_value_moist = c(4, 4, 4, 6),
      munsell_chroma_moist = c(3, 3, 3, 4), munsell_value_dry = c(6, 6, 6, 7), oc_pct = c(0.8, 0.6, 0.4, 0.1),
      foreign_fragments_pct = c(5, NA, NA, NA), artefacts_pct = c(1, NA, NA, NA), caco3_pct = c(2, 2, 2, 0), ph_h2o = c(7.2, 7.2, 7.2, 7.4),
      bs_pct = c(90, 90, 90, 95), cec_cmol = c(12, 12, 12, 4),
      structure_type = c("granular", "blocky", "blocky", "single grain")), function(v) v[k]),
      list(soil_moisture_regime = "ustic", soil_temperature_regime = "mesic", land_use = "cropland"))
  },
  C1 = mk(list(
    designation = c("Oa", "E", "Bhs", "Bs", "C"), top_cm = c(0, 5, 15, 30, 60), bottom_cm = c(5, 15, 30, 60, 100),
    oc_pct = c(30, 0.5, if (nm) 4 else 7, 1.2, 0.2), clay_pct = c(NA, 4, 6, 5, 3), sand_pct = c(NA, 88, 84, 86, 92),
    silt_pct = c(NA, 8, 10, 9, 5), ph_h2o = c(4.0, 4.3, 4.4, 4.8, 5.1),
    munsell_hue_moist = c("10YR", "10YR", "5YR", "7.5YR", "10YR"), munsell_value_moist = c(2, 6, 3, 4, 6),
    munsell_chroma_moist = c(1, 2, 3, 4, 4), munsell_value_dry = c(3, 7, 4, 5, 7),
    al_ox_pct = c(NA, 0.05, 0.5, 0.6, 0.1), fe_ox_pct = c(NA, 0.05, 0.4, 0.4, 0.1),
    water_saturation_days = c(10, 0, 0, 0, 0), bs_pct = c(20, 10, 10, 15, 20), cec_cmol = c(40, 2, 15, 8, 2),
    structure_type = c(NA, "single grain", "massive", "massive", "single grain")),
    list(soil_moisture_regime = "perudic", soil_temperature_regime = "cryic", mean_summer_soil_temp_C = 10)),
  C2 = mk(list(
    designation = c("E", "Bs", "C"), top_cm = c(0, 12, 45), bottom_cm = c(12, 45, 100),
    oc_pct = c(0.8, 1.2, 0.2), clay_pct = c(4, 6, 3), sand_pct = c(88, 84, 92), silt_pct = c(8, 10, 5),
    ph_h2o = c(4.5, 4.8, 5.1), munsell_hue_moist = c("10YR", "7.5YR", "10YR"), munsell_value_moist = c(6, 4, 6),
    munsell_chroma_moist = c(2, 4, 4), munsell_value_dry = c(7, 5, 7),
    al_ox_pct = c(0.05, 0.6, 0.1), fe_ox_pct = c(0.05, 0.4, 0.1), bs_pct = c(10, 15, 20), cec_cmol = c(2, 8, 2),
    structure_type = c("single grain", "massive", "single grain")),
    list(soil_moisture_regime = "udic", soil_temperature_regime = "frigid")),
  D1 = mk(list(
    designation = c("Ah", "Bw", "C"), top_cm = c(0, 20, 70), bottom_cm = c(20, 70, 110),
    oc_pct = c(6, 2.5, 0.8), clay_pct = c(20, 22, 18), silt_pct = c(55, 55, 50), sand_pct = c(25, 23, 32),
    al_ox_pct = c(2.0, 2.2, 1.5), fe_ox_pct = c(0.8, 0.8, 0.6), bulk_density_g_cm3 = c(0.7, 0.75, 0.9),
    phosphate_retention_pct = c(90, 92, 85), ph_h2o = c(5.8, 6.0, 6.2),
    munsell_hue_moist = c("10YR", "10YR", "10YR"), munsell_value_moist = c(2, 3, 4), munsell_chroma_moist = c(2, 3, 3),
    munsell_value_dry = c(3, 5, 6), bs_pct = c(30, 25, 30), cec_cmol = c(35, 25, 20),
    structure_type = c("granular", "blocky", "massive")),
    list(soil_moisture_regime = "udic", soil_temperature_regime = if (nm) "frigid" else "cryic",
         mean_summer_soil_temp_C = if (nm) 17 else 10)),
  D2 = mk(list(
    designation = c("Ah", "C1", "C2"), top_cm = c(0, 15, 60), bottom_cm = c(15, 60, 110),
    oc_pct = c(1.5, 0.4, 0.2), clay_pct = c(6, 4, 4), silt_pct = c(14, 10, 10), sand_pct = c(80, 86, 86),
    al_ox_pct = c(0.4, 0.4, 0.4), fe_ox_pct = c(0.4, 0.4, 0.4), volcanic_glass_pct = 35, phosphate_retention_pct = 30,
    particles_002_2mm_pct = c(85, 90, 90), bulk_density_g_cm3 = c(1.0, 1.1, 1.1), ph_h2o = 6.5,
    munsell_hue_moist = "10YR", munsell_value_moist = c(3, 4, 5), munsell_chroma_moist = c(2, 3, 3),
    munsell_value_dry = c(5, 6, 7), bs_pct = 60, cec_cmol = c(10, 5, 5),
    structure_type = c("granular", "single grain", "single grain")),
    list(soil_moisture_regime = "ustic", soil_temperature_regime = "mesic")),
  D3 = mk(list(
    designation = c("Ah", "Bw", "C"), top_cm = c(0, 35, 100), bottom_cm = c(35, 100, 130),
    oc_pct = c(8, 2.5, 0.8), clay_pct = c(30, 32, 25), silt_pct = c(55, 55, 50), sand_pct = c(15, 13, 25),
    al_ox_pct = c(2.5, 2.8, 1.5), fe_ox_pct = c(1.0, 1.0, 0.6), bulk_density_g_cm3 = c(0.6, 0.7, 0.9),
    phosphate_retention_pct = c(95, 95, 85), ph_h2o = c(5.2, 5.4, 5.6),
    munsell_hue_moist = "10YR", munsell_value_moist = c(2, 3, 4), munsell_chroma_moist = c(1, 3, 3),
    munsell_value_dry = c(3, 5, 6), bs_pct = c(20, 15, 20), cec_cmol = c(40, 30, 20),
    structure_type = c("granular", "blocky", "massive")),
    list(soil_moisture_regime = "udic", soil_temperature_regime = "mesic")),
  E1 = mk(list(
    designation = c("A", "Bo1", "Bo2"), top_cm = c(0, 15, 80), bottom_cm = c(15, 80, 200),
    clay_pct = c(45, 55, 55), sand_pct = 20, silt_pct = c(35, 25, 25), oc_pct = c(2.0, 0.5, 0.3),
    cec_cmol = c(9, if (nm) 9.9 else 6, if (nm) 9.9 else 6), ca_cmol = 0.8, mg_cmol = 0.4, k_cmol = 0.1,
    na_cmol = 0.05, al_kcl_cmol = 0.8, k_total_pct = c(NA, 0.3, 0.3), weatherable_minerals_pct = c(NA, 3, 3),
    bs_pct = c(15, 20, 20), ph_h2o = 4.9, fe_dcb_pct = c(5, 6, 6), fe_total_pct = c(8, 9, 9),
    munsell_hue_moist = c("5YR", "2.5YR", "2.5YR"), munsell_value_moist = c(3, 3, 3), munsell_chroma_moist = c(3, 6, 6),
    munsell_value_dry = c(4, 4, 4), structure_type = c("granular", "subangular blocky", "subangular blocky")),
    list(soil_moisture_regime = "udic", soil_temperature_regime = "hyperthermic")),
  F1 = mk(list(
    designation = c("Ap", "Bss1", "Bss2", if (nm) "R" else "BC"), top_cm = c(0, 18, 45, if (nm) 45 else 90),
    bottom_cm = c(18, 45, if (nm) 45 else 90, 130), clay_pct = c(45, 50, 52, 48), silt_pct = c(35, 30, 30, 32),
    sand_pct = c(20, 20, 18, 20), oc_pct = c(1.5, 0.8, 0.5, 0.3), ph_h2o = c(7.0, 7.3, 7.6, 7.8),
    cec_cmol = 35, bs_pct = 90, cracks_width_cm = c(1.5, 1.5, 1.0, NA), cracks_depth_cm = c(60, 60, 60, NA),
    slickensides = c("absent", "common", "many", "few"), structure_type = c("granular", "wedge-shaped", "wedge-shaped", "blocky"),
    munsell_hue_moist = "10YR", munsell_value_moist = c(3, 3, 4, 4), munsell_chroma_moist = c(2, 2, 2, 3),
    munsell_value_dry = c(4, 4, 5, 5), mottle_colors = c(NA, "7.5YR 5/6", NA, NA)),
    list(soil_moisture_regime = "aquic", soil_temperature_regime = "thermic", drainage_class = "poorly drained")),
  F2 = mk(list(
    designation = c("Ap", "Bss", "Bkss", "BC"), top_cm = c(0, 18, 60, 100), bottom_cm = c(18, 60, 100, 140),
    clay_pct = c(45, 50, 50, 46), silt_pct = c(35, 30, 30, 34), sand_pct = c(20, 20, 20, 20),
    oc_pct = c(1.2, 0.7, 0.4, 0.2), ph_h2o = c(7.6, 7.9, 8.2, 8.3), cec_cmol = 35, bs_pct = 100,
    caco3_pct = c(1, 2, 18, 8), secondary_carbonates_pct = c(0, 0, 8, 2),
    cracks_width_cm = c(1.5, 1.5, NA, NA), cracks_depth_cm = c(60, 60, NA, NA),
    slickensides = c("absent", "many", "common", "few"), structure_type = c("granular", "wedge-shaped", "wedge-shaped", "blocky"),
    munsell_hue_moist = "10YR", munsell_value_moist = c(3, 3, 5, 5), munsell_chroma_moist = c(2, 2, 3, 3),
    munsell_value_dry = c(4, 4, 6, 6)),
    list(soil_moisture_regime = "ustic", soil_temperature_regime = "thermic", cracks_open_days = 140, irrigated = FALSE)),
  F3 = mk(list(
    designation = c("Ap", "Bss1", "Bss2", "BC"), top_cm = c(0, 18, 45, 90), bottom_cm = c(18, 45, 90, 130),
    clay_pct = c(45, 50, 52, 48), silt_pct = c(35, 30, 30, 32), sand_pct = c(20, 20, 18, 20),
    oc_pct = c(2.0, 1.2, 0.8, 0.4), ph_h2o = c(6.5, 6.8, 7.0, 7.2), cec_cmol = 35, bs_pct = 85,
    cracks_width_cm = c(1.2, 1.0, NA, NA), cracks_depth_cm = c(50, 50, NA, NA),
    slickensides = c("absent", "common", "many", "few"), structure_type = c("granular", "wedge-shaped", "wedge-shaped", "blocky"),
    munsell_hue_moist = "10YR", munsell_value_moist = c(3, 3, 4, 4), munsell_chroma_moist = c(2, 2, 2, 3),
    munsell_value_dry = c(4, 4, 5, 5)),
    list(soil_moisture_regime = "udic", soil_temperature_regime = "thermic")),
  G1 = mk(list(
    designation = c("Avk", "Bw", "Bk", "C"), top_cm = c(0, 8, 20, 60), bottom_cm = c(8, 20, 60, 110),
    desert_pavement_pct = c(if (nm) 0 else 30, NA, NA, NA), structure_type = c(if (nm) "granular" else "platy", "blocky", "blocky", "massive"),
    clay_pct = 14, sand_pct = 52, silt_pct = 34,
    oc_pct = c(0.4, 0.3, 0.2, 0.1), caco3_pct = c(8, 10, 18, 9), secondary_carbonates_pct = c(0, 1, 8, 1),
    soluble_salts_pct = c(0.1, 0.1, 0.2, 0.2), ph_h2o = c(8.4, 8.5, 8.6, 8.5), bs_pct = 100, cec_cmol = 8,
    munsell_hue_moist = "10YR", munsell_value_moist = c(5, 5, 6, 6), munsell_chroma_moist = c(3, 3, 3, 3),
    munsell_value_dry = c(6, 6, 7, 7)),
    list(soil_moisture_regime = "aridic", soil_temperature_regime = "cryic", mean_summer_soil_temp_C = 10,
         surface_crust_thickness_cm = if (nm) NA else 1, irrigated = FALSE,
         freeze_thaw_days_pct = 20)),
  G2 = mk(list(
    designation = c("Av", "Bw", "By", "C"), top_cm = c(0, 5, 25, 55), bottom_cm = c(5, 25, 55, 100),
    desert_pavement_pct = c(40, NA, NA, NA), structure_type = c("platy", "blocky", "massive", "massive"),
    clay_pct = c(10, 14, 12, 10), sand_pct = c(60, 52, 55, 62), silt_pct = c(30, 34, 33, 28),
    oc_pct = c(0.3, 0.2, 0.1, 0.1), caco3_pct = c(5, 6, 6, 5), caso4_pct = c(0.5, 1, 12, 3),
    gypsum_visible_pct = c(0, 0, 3, 0), soluble_salts_pct = c(0.1, 0.1, 0.3, 0.2), ph_h2o = 8.2, bs_pct = 100,
    cec_cmol = 6, munsell_hue_moist = "7.5YR", munsell_value_moist = c(5, 5, 6, 6), munsell_chroma_moist = 4,
    munsell_value_dry = c(6, 6, 7, 7)),
    list(soil_moisture_regime = "aridic", soil_temperature_regime = "mesic", surface_crust_thickness_cm = 1,
         irrigated = FALSE)),
  H1 = mk(list(
    designation = c("A", "Btn", "Bk", "C"), top_cm = c(0, 15, 45, 90), bottom_cm = c(15, 45, 90, 130),
    clay_pct = c(15, 30, 25, 20), silt_pct = 40, sand_pct = c(45, 30, 35, 40),
    esp_pct = c(5, if (nm) 22 else 38, 20, 15), ph_h2o = c(8.5, 9.3, 9.0, 8.8),
    soluble_salts_pct = c(0.2, 0.4, 0.5, 0.4), oc_pct = c(1.0, 0.5, 0.3, 0.1),
    caco3_pct = c(2, 3, 12, 6), secondary_carbonates_pct = c(0, 0, 5, 1), na_cmol = c(0.6, if (nm) 4 else 7, 4, 2), mg_cmol = 2,
    cec_cmol = c(12, 18, 16, 12), bs_pct = 100,
    structure_type = c("platy", "columnar", "blocky", "massive"),
    munsell_hue_moist = "10YR", munsell_value_moist = c(4, 4, 5, 6), munsell_chroma_moist = c(3, 3, 3, 3),
    munsell_value_dry = c(6, 6, 7, 7)),
    list(soil_moisture_regime = "ustic", soil_temperature_regime = "mesic")),
  H2 = mk(list(
    designation = c("Az", "Bz", "C"), top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 110),
    clay_pct = c(20, 22, 18), silt_pct = c(50, 48, 50), sand_pct = c(30, 30, 32),
    soluble_salts_pct = c(2.8, 1.0, 0.4), esp_pct = c(12, 10, 8), ph_h2o = c(8.4, 8.3, 8.2),
    oc_pct = c(0.6, 0.3, 0.2), caco3_pct = c(6, 6, 5), na_cmol = c(1.5, 1.2, 1), cec_cmol = 12, bs_pct = 100,
    structure_type = c("granular", "blocky", "massive"),
    munsell_hue_moist = "10YR", munsell_value_moist = c(5, 5, 6), munsell_chroma_moist = c(2, 3, 3),
    munsell_value_dry = c(7, 7, 7)),
    list(soil_moisture_regime = "aridic", soil_temperature_regime = "mesic", irrigated = FALSE)),
  I1 = mk(list(
    designation = c("Oe", "Bg", "Cg", if (nm) "C" else "Cf"), top_cm = c(0, 15, 50, 90),
    bottom_cm = c(15, 50, 90, 120), oc_pct = c(30, 1.5, 0.6, 0.4), clay_pct = c(NA, 25, 22, 20),
    silt_pct = c(NA, 50, 50, 50), sand_pct = c(NA, 25, 28, 30), fiber_content_rubbed_pct = c(30, NA, NA, NA),
    water_saturation_days = c(120, 200, 250, 250), bulk_density_g_cm3 = c(0.2, 1.3, 1.4, 1.5),
    munsell_hue_moist = c("10YR", "5Y", "5Y", "5Y"), munsell_value_moist = c(2, 5, 5, 5),
    munsell_chroma_moist = c(1, 1, 1, 2), childs_test = c(NA, "positive", "positive", NA),
    permafrost_temp_C = c(NA, NA, NA, if (nm) NA else -1), ph_h2o = 6.0, bs_pct = 60, cec_cmol = 20,
    structure_type = c(NA, "massive", "massive", "massive")),
    list(soil_moisture_regime = "aquic", soil_temperature_regime = if (nm) "frigid" else "gelic",
         drainage_class = "poorly drained")),
  I2 = mk(list(
    designation = c("Ap", "Eg", "Btg", "C"), top_cm = c(0, 20, 40, 80), bottom_cm = c(20, 40, 80, 120),
    oc_pct = c(1.8, 0.5, 0.4, 0.2), clay_pct = c(20, 18, 38, 30), silt_pct = c(55, 57, 42, 45),
    sand_pct = c(25, 25, 20, 25), munsell_hue_moist = c("10YR", "10YR", "10YR", "10YR"),
    munsell_value_moist = c(4, 6, 5, 5), munsell_chroma_moist = c(2, 1, 2, 4), munsell_value_dry = c(5, 7, 6, 6),
    childs_test = c(NA, "positive", "positive", NA), mottle_colors = c(NA, NA, "7.5YR 5/6", NA),
    ph_h2o = 5.8, bs_pct = 55, cec_cmol = c(15, 8, 20, 15), structure_type = c("granular", "platy", "prismatic", "massive")),
    list(soil_moisture_regime = "stagnic", soil_temperature_regime = "mesic", drainage_class = "poorly drained")),
  I3 = mk(list(
    designation = c("Ah", "Bg", "Cg"), top_cm = c(0, 30, 70), bottom_cm = c(30, 70, 120),
    oc_pct = c(3.0, 0.8, 0.4), clay_pct = c(28, 30, 25), silt_pct = c(50, 48, 50), sand_pct = c(22, 22, 25),
    munsell_hue_moist = c("10YR", "2.5Y", "5Y"), munsell_value_moist = c(2, 5, 5), munsell_chroma_moist = c(1, 2, 1),
    munsell_value_dry = c(4, 6, 6), childs_test = c(NA, "positive", "positive"),
    ph_h2o = 7.0, bs_pct = 80, cec_cmol = c(30, 22, 18), structure_type = c("granular", "massive", "massive")),
    list(soil_moisture_regime = "aquic", soil_temperature_regime = "mesic", drainage_class = "poorly drained")),
  J1 = mk(list(
    designation = c("Ah", "AB", "C1", "C2"), top_cm = c(0, 40, 70, 120), bottom_cm = c(40, 70, 120, 180),
    oc_pct = c(2.5, 1.5, 0.8, 0.3), clay_pct = c(10, 8, 4, 3), silt_pct = c(15, 12, 6, 5), sand_pct = c(75, 80, 90, 92),
    caco3_pct = c(30, 45, 60, 70), p_total_pct = c(2.0, 1.2, 0.5, 0.2), ph_h2o = c(8.0, 8.2, 8.4, 8.5), bs_pct = 100,
    cec_cmol = c(25, 15, 6, 4), munsell_hue_moist = "10YR", munsell_value_moist = c(2, 3, 5, 6),
    munsell_chroma_moist = c(2, 2, 3, 3), munsell_value_dry = c(4, 5, 7, 8),
    structure_type = c("granular", "blocky", "single grain", "single grain")),
    list(soil_moisture_regime = "ustic", soil_temperature_regime = "hyperthermic", parent_material = "coral sand")),
  J2 = mk(list(
    designation = c("Ah", "AB", "Bk", "C"), top_cm = c(0, 45, 60, 110), bottom_cm = c(45, 60, 110, 180),
    oc_pct = if (nm) c(4.0, 0.3, 0.2, 0.1) else c(2.5, 1.5, 0.8, 0.3), clay_pct = c(25, 26, 24, 20),
    silt_pct = c(50, 50, 52, 55), sand_pct = c(25, 24, 24, 25), caco3_pct = c(0.5, 2, 15, 8),
    secondary_carbonates_pct = c(0, 0, 6, 1), ph_h2o = c(7.2, 7.8, 8.3, 8.3), bs_pct = c(90, 95, 100, 100),
    cec_cmol = c(30, 25, 18, 15), munsell_hue_moist = "10YR", munsell_value_moist = c(2, 3, 5, 6),
    munsell_chroma_moist = c(2, 2, 3, 3), munsell_value_dry = c(4, 5, 7, 7),
    structure_type = c("granular", "blocky", "blocky", "massive")),
    list(soil_moisture_regime = "ustic", soil_temperature_regime = "mesic")),
  J3 = mk(list(
    designation = c("Ah", "Bt", "BC", "C"), top_cm = c(0, 35, 90, 130), bottom_cm = c(35, 90, 130, 180),
    oc_pct = c(2.5, 1.0, 0.5, 0.3), clay_pct = c(22, 32, 26, 22), silt_pct = c(55, 48, 50, 52),
    sand_pct = c(23, 20, 24, 26), ph_h2o = c(6.6, 6.8, 7.0, 7.2), bs_pct = c(85, 85, 90, 90),
    cec_cmol = c(28, 25, 20, 18), caco3_pct = 0, clay_films_strength = c(NA, "distinct", NA, NA),
    clay_films_amount = c(NA, "common", NA, NA), munsell_hue_moist = "10YR", munsell_value_moist = c(2, 4, 5, 5),
    munsell_chroma_moist = c(2, 3, 4, 4), munsell_value_dry = c(4, 6, 6, 7),
    structure_type = c("granular", "blocky", "blocky", "massive")),
    list(soil_moisture_regime = "udic", soil_temperature_regime = "mesic")),
  K1 = mk(list(
    designation = c("A", "Bw1", "Bw2"), top_cm = c(0, 15, 60), bottom_cm = c(15, 60, 130),
    clay_pct = c(40, 50, 50), sand_pct = 25, silt_pct = c(35, 25, 25), oc_pct = c(1.5, 0.5, 0.3),
    cec_cmol = c(12, if (nm) 13 else 9.5, if (nm) 13 else 9.5), ca_cmol = 2, mg_cmol = 1, k_cmol = 0.2, na_cmol = 0.1,
    al_kcl_cmol = 0.5, bs_pct = 40, ph_h2o = 5.5, fe_dcb_pct = c(3, 4, 4), fe_total_pct = c(6, 7, 7),
    munsell_hue_moist = "2.5YR", munsell_value_moist = c(3, 4, 4), munsell_chroma_moist = c(4, 6, 6),
    munsell_value_dry = c(4, 5, 5), structure_type = c("granular", "subangular blocky", "subangular blocky")),
    list(soil_moisture_regime = "ustic", soil_temperature_regime = "thermic")),
  K2 = mk(list(
    designation = c("A", "Bw1", "Bw2"), top_cm = c(0, 15, 60), bottom_cm = c(15, 60, 130),
    clay_pct = c(40, 50, 50), sand_pct = 25, silt_pct = c(35, 25, 25), oc_pct = c(2.5, 0.5, 0.3),
    cec_cmol = c(14, 10, 10), ca_cmol = 0.6, mg_cmol = 0.3, k_cmol = 0.1, na_cmol = 0.05,
    al_kcl_cmol = 3, bs_pct = 10, ph_h2o = 4.6, fe_dcb_pct = c(2.5, 3, 3), fe_total_pct = c(5, 6, 6),
    sio2_al2o3_clay = c(NA, 1.7, 1.7),
    munsell_hue_moist = "5YR", munsell_value_moist = c(3, 4, 4), munsell_chroma_moist = c(4, 6, 6),
    munsell_value_dry = c(4, 5, 5), structure_type = c("granular", "subangular blocky", "subangular blocky")),
    list(soil_moisture_regime = "perudic", soil_temperature_regime = "thermic")),
  K3 = mk(list(
    designation = c("A", "Bt1", "Bt2"), top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 130),
    clay_pct = c(20, 38, 40), sand_pct = c(45, 30, 30), silt_pct = c(35, 32, 30), oc_pct = c(1.5, 0.5, 0.3),
    cec_cmol = c(8, 8.5, 9), ca_cmol = 1.5, mg_cmol = 0.8, k_cmol = 0.2, na_cmol = 0.1, al_kcl_cmol = 1,
    bs_pct = 30, ph_h2o = 5.0, fe_dcb_pct = c(1.5, 2.2, 2.3), fe_total_pct = c(3, 4, 4),
    sio2_al2o3_clay = c(NA, 2.4, 2.4), sio2_al2o3_naoh = c(NA, 1.5, 1.5), sio2_sulfuric_pct = c(NA, 20, 20),
    al2o3_sulfuric_pct = c(NA, 15, 15),
    clay_films_strength = c(NA, "distinct", "distinct"), clay_films_amount = c(NA, "common", "common"),
    munsell_hue_moist = "5YR", munsell_value_moist = c(4, 4, 4), munsell_chroma_moist = c(4, 6, 6),
    munsell_value_dry = c(5, 5, 5), structure_type = c("granular", "subangular blocky", "subangular blocky")),
    list(soil_moisture_regime = "udic", soil_temperature_regime = "thermic")),
  L1 = mk(list(
    designation = c("Ah", "E", "Bt", "C"), top_cm = c(0, 15, 25, 80), bottom_cm = c(15, 25, 80, 120),
    clay_pct = c(18, 14, 30, 24), silt_pct = c(55, 58, 45, 48), sand_pct = c(27, 28, 25, 28),
    oc_pct = c(3.0, 0.6, 0.4, 0.2), ph_h2o = c(5.8, 5.6, 6.0, 6.3), bs_pct = c(60, 50, 70, 75),
    cec_cmol = c(20, 8, 22, 18), munsell_hue_moist = "10YR", munsell_value_moist = c(3, 6, 4, 5),
    munsell_chroma_moist = c(2, 2, 4, 4), munsell_value_dry = c(5, 7, 6, 6),
    clay_films_strength = c(NA, NA, "distinct", NA), clay_films_amount = c(NA, NA, "common", NA),
    structure_type = c("granular", "platy", "blocky", "massive")),
    list(soil_moisture_regime = "udic", soil_temperature_regime = if (nm) "gelic" else "frigid",
         mean_summer_soil_temp_C = 17)),
  L2 = mk(list(
    designation = c("Ap", "Bt", "Bk", "C"), top_cm = c(0, 20, 70, 110), bottom_cm = c(20, 70, 110, 150),
    clay_pct = c(18, 30, 26, 22), silt_pct = c(55, 45, 48, 50), sand_pct = c(27, 25, 26, 28),
    oc_pct = c(1.0, 0.5, 0.3, 0.2), ph_h2o = c(7.2, 7.5, 8.2, 8.3), bs_pct = 100, cec_cmol = c(14, 22, 18, 15),
    caco3_pct = c(0.5, 0.5, 15, 8), secondary_carbonates_pct = c(0, 0, 8, 2), na_cmol = 0.2, mg_cmol = 3,
    munsell_hue_moist = "7.5YR", munsell_value_moist = c(4, 4, 5, 6), munsell_chroma_moist = c(3, 4, 4, 4),
    munsell_value_dry = c(6, 6, 7, 7), clay_films_strength = c(NA, "distinct", NA, NA),
    clay_films_amount = c(NA, "common", NA, NA), structure_type = c("granular", "blocky", "blocky", "massive")),
    list(soil_moisture_regime = "ustic", soil_temperature_regime = "mesic")),
  L3 = mk(list(
    designation = c("A", "Bt1", "Bt2", "C"), top_cm = c(0, 15, 50, 100), bottom_cm = c(15, 50, 100, 140),
    clay_pct = c(20, 35, 38, 30), silt_pct = c(45, 40, 38, 42), sand_pct = c(35, 25, 24, 28),
    oc_pct = c(3.0, 0.5, 0.3, 0.2), ph_h2o = c(4.5, 4.6, 4.7, 4.8), bs_pct = c(15, 10, 10, 12),
    cec_cmol = c(14, 11, 11.5, 9), al_kcl_cmol = c(3, 6, 6, 5), ca_cmol = 0.5, mg_cmol = 0.3, k_cmol = 0.2, na_cmol = 0.1,
    fe_dcb_pct = c(1.0, 1.2, 1.2, 1.0), fe_total_pct = c(3, 3.5, 3.5, 3),
    munsell_hue_moist = "10YR", munsell_value_moist = c(3, 5, 5, 6), munsell_chroma_moist = c(2, 6, 6, 6),
    munsell_value_dry = c(5, 6, 6, 7), clay_films_strength = c(NA, "distinct", "distinct", NA),
    clay_films_amount = c(NA, "common", "common", NA), structure_type = c("granular", "blocky", "blocky", "massive")),
    list(soil_moisture_regime = "perudic", soil_temperature_regime = "thermic")),
  L4 = mk(list(
    designation = c("A", "Bt1", "Bt2", "C"), top_cm = c(0, 20, 60, 100), bottom_cm = c(20, 60, 100, 140),
    clay_pct = c(20, 34, 36, 30), silt_pct = c(45, 40, 38, 42), sand_pct = c(35, 26, 26, 28),
    oc_pct = c(1.5, 0.5, 0.3, 0.2), ph_h2o = c(5.8, 6.0, 6.1, 6.3), bs_pct = c(55, 60, 60, 65),
    cec_cmol = c(12, 15, 16, 13), fe_dcb_pct = c(1.5, 2.0, 2.0, 1.8), fe_total_pct = c(3, 3.5, 3.5, 3),
    ph_kcl = c(5.0, 5.1, 5.2, 5.4), al_kcl_cmol = 0.2, al_sat_pct = 5,
    munsell_hue_moist = "5YR", munsell_value_moist = c(4, 4, 4, 5), munsell_chroma_moist = c(4, 6, 6, 6),
    munsell_value_dry = c(5, 5, 5, 6), clay_films_strength = c(NA, "distinct", "distinct", NA),
    clay_films_amount = c(NA, "common", "common", NA), structure_type = c("granular", "blocky", "blocky", "massive")),
    list(soil_moisture_regime = "udic", soil_temperature_regime = "thermic")),
  M1 = mk(list(
    designation = c("Ah", "Bw", "C"), top_cm = c(0, 8, 40), bottom_cm = c(8, 40, 90),
    root_mat = c(TRUE, NA, NA), root_mat_pct = c(60, NA, NA), bulk_density_g_cm3 = c(0.8, 1.2, 1.4),
    n_total_pct = c(1.8, 0.2, 0.1), water_saturation_days = c(10, 10, 10),
    oc_pct = c(15, 1.5, 0.4), clay_pct = c(18, 20, 15), silt_pct = c(50, 50, 48), sand_pct = c(32, 30, 37),
    munsell_hue_moist = c("10YR", "7.5YR", "10YR"), munsell_value_moist = c(3, 4, 5), munsell_chroma_moist = c(2, 4, 3),
    munsell_value_dry = c(5, 6, 6), structure_type = c("granular", "lenticular", "massive"),
    structure_grade = c("moderate", "moderate", "structureless"), ph_h2o = 6.5, bs_pct = 60, cec_cmol = c(40, 18, 12)),
    list(soil_moisture_regime = "udic", soil_temperature_regime = "cryic", mean_summer_soil_temp_C = 8,
         frost_regime = "seasonal")),
  M2 = mk(list(
    designation = c("Ap", "Bw", "Bg", "C"), top_cm = c(0, 20, 60, 100), bottom_cm = c(20, 60, 100, 130),
    clay_pct = c(20, 22, 20, 18), silt_pct = c(50, 50, 52, 52), sand_pct = c(30, 28, 28, 30),
    oc_pct = c(1.5, 0.5, 0.3, 0.2), mottle_colors = c(NA, "7.5YR 5/6", "7.5YR 5/8", NA),
    childs_test = c(NA, "negative", "negative", NA),
    munsell_hue_moist = c("10YR", "7.5YR", "10YR", "10YR"), munsell_value_moist = c(4, 4, 5, 5),
    munsell_chroma_moist = c(3, 4, 3, 3), munsell_value_dry = c(6, 6, 6, 7),
    structure_type = c("granular", "blocky", "blocky", "massive"),
    structure_grade = c("moderate", "moderate", "weak", "structureless"), ph_h2o = 6.8, bs_pct = 80, cec_cmol = 14),
    list(soil_moisture_regime = "aquic", soil_temperature_regime = "mesic", drainage_class = "somewhat poorly drained")),
  M3 = mk(list(
    designation = c("Ap", "Bw", "C"), top_cm = c(0, 20, 70), bottom_cm = c(20, 70, 120),
    clay_pct = c(18, 20, 15), silt_pct = c(52, 50, 55), sand_pct = 30, oc_pct = c(1.2, 0.5, 0.2),
    munsell_hue_moist = c("10YR", "7.5YR", "10YR"), munsell_value_moist = c(4, 5, 5), munsell_chroma_moist = c(3, 6, 3),
    munsell_value_dry = c(6, 6, 7), structure_type = c("granular", "blocky", "massive"),
    structure_grade = c("moderate", "moderate", "structureless"), ph_h2o = 7.2, bs_pct = 90, cec_cmol = 15,
    caco3_pct = c(0.5, 0.5, 1), fe_dcb_pct = c(0.8, 0.8, 0.7), fe_total_pct = 3),
    list(soil_moisture_regime = if (nm) "aridic" else "ustic", soil_temperature_regime = "mesic", irrigated = FALSE)),
  M4 = mk(list(
    designation = c("A", "Bw", "C"), top_cm = c(0, 15, 90), bottom_cm = c(15, 90, 130),
    clay_pct = c(22, 25, 20), silt_pct = c(45, 45, 48), sand_pct = c(33, 30, 32), oc_pct = c(3.0, 0.5, 0.3),
    munsell_hue_moist = c("10YR", "7.5YR", "10YR"), munsell_value_moist = c(3, 5, 5), munsell_chroma_moist = c(3, 6, 4),
    munsell_value_dry = c(5, 6, 7), structure_type = c("granular", "blocky", "massive"),
    structure_grade = c("moderate", "moderate", "structureless"), ph_h2o = 4.8, bs_pct = 30, cec_cmol = 15,
    al_kcl_cmol = 2, ph_kcl = 4.3),
    list(soil_moisture_regime = "perudic", soil_temperature_regime = "thermic")),
  M5 = mk(list(
    designation = c("A", "Bw", "C", "R"), top_cm = c(0, 15, 60, 100), bottom_cm = c(15, 60, 100, 130),
    clay_pct = c(22, 25, 22, NA), silt_pct = c(45, 45, 48, NA), sand_pct = c(33, 30, 30, NA),
    oc_pct = c(1.5, 0.5, 0.2, NA), munsell_hue_moist = c("10R", "10RP", "10RP", NA),
    munsell_value_moist = c(4, 4, 4, NA), munsell_chroma_moist = c(3, 3, 3, NA),
    munsell_value_dry = c(5, 5, 5, NA), rock_fragment_max_cm = c(2, 2, 3, NA), coarse_fragments_pct = c(5, 10, 20, NA),
    structure_type = c("granular", "blocky", "massive", NA),
    structure_grade = c("moderate", "moderate", "structureless", NA), ph_h2o = 6.5, bs_pct = 70, cec_cmol = 15,
    caco3_pct = 0),
    list(soil_moisture_regime = "udic", soil_temperature_regime = "thermic", parent_material = "purplish shale")),
  N1 = mk(list(
    designation = c("Ap", "Au", "C"), top_cm = c(0, 30, 80), bottom_cm = c(30, 80, 130),
    clay_pct = c(18, 20, 15), silt_pct = c(50, 48, 55), sand_pct = c(32, 32, 30), oc_pct = c(1.0, 0.4, 0.2),
    foreign_fragments_pct = c(NA, 5, NA), artefacts_pct = c(0, 0, NA), munsell_hue_moist = "10YR", munsell_value_moist = c(4, 5, 5),
    munsell_chroma_moist = c(3, 3, 3), munsell_value_dry = c(6, 6, 7),
    structure_type = c("granular", "massive", "massive"), structure_grade = c("weak", "structureless", "structureless"),
    ph_h2o = 7.0, bs_pct = 85, cec_cmol = 12),
    list(soil_moisture_regime = "udic", soil_temperature_regime = "thermic", land_use = "terraced cropland",
         flood_frequency = "none")),
  N2 = mk(list(
    designation = c("C1", "C2"), top_cm = c(0, 40), bottom_cm = c(40, 130),
    clay_pct = 3, silt_pct = 5, sand_pct = 92, oc_pct = if (nm) c(0.25, 0.15) else c(0.1, 0.06),
    structure_type = "single grain", structure_grade = "structureless",
    munsell_hue_moist = "10YR", munsell_value_moist = 6, munsell_chroma_moist = 4, munsell_value_dry = 7,
    ph_h2o = 8.0, bs_pct = 100, cec_cmol = 2, caco3_pct = 1),
    list(soil_moisture_regime = "aridic", soil_temperature_regime = "mesic", flood_frequency = "none")),
  N3 = mk(list(
    designation = c("Ah", "C1", "2C2", "3C3"), top_cm = c(0, 15, 50, 100), bottom_cm = c(15, 50, 100, 150),
    clay_pct = c(15, 10, 25, 8), silt_pct = c(55, 40, 55, 30), sand_pct = c(30, 50, 20, 62),
    oc_pct = c(0.8, 0.3, 0.5, 0.25), stratification_pattern = c(NA, "continuous", "continuous", NA),
    munsell_hue_moist = "10YR", munsell_value_moist = c(4, 5, 5, 6), munsell_chroma_moist = c(3, 3, 3, 3),
    munsell_value_dry = c(6, 6, 6, 7), structure_type = c("granular", "massive", "massive", "single grain"),
    structure_grade = c("weak", "structureless", "structureless", "structureless"),
    ph_h2o = 7.8, bs_pct = 100, cec_cmol = c(12, 8, 15, 5), caco3_pct = c(3, 4, 5, 3)),
    list(soil_moisture_regime = "ustic", soil_temperature_regime = "mesic", flood_frequency = "annual",
         silting_rate_cm_yr = 0)),
  N4 = mk(list(
    designation = c("Ah", "C"), top_cm = c(0, 10), bottom_cm = c(10, 130),
    clay_pct = c(15, 14), silt_pct = c(65, 66), sand_pct = c(20, 20), oc_pct = c(0.6, 0.2),
    munsell_hue_moist = "10YR", munsell_value_moist = c(4, 5), munsell_chroma_moist = c(3, 4),
    munsell_value_dry = c(6, 7), munsell_chroma_dry = c(3, 4), caco3_pct = c(9, 11),
    structure_type = c("granular", "massive"), structure_grade = c("weak", "structureless"),
    ph_h2o = 8.2, bs_pct = 100, cec_cmol = 9),
    list(soil_moisture_regime = "ustic", soil_temperature_regime = "mesic", parent_material = "loess",
         flood_frequency = "none")),
  rlang::abort(sprintf("No example for %s.", suborder)))
}
