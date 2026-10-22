# =============================================================================
# v0.9.268 -- the great groups of the Canadian System of Soil Classification,
# 3rd edition (Key to Soil Great Groups, Ch 3, pp. 35-36), with the order
# chapters where the key refers to them. Same conventions as
# R/diagnostics-cssc.R: measured properties first, designations in the
# Canadian notation second, NA when neither decides; readings in
# inst/rules/cssc3/readings.yaml. The last great group of each order is the
# key's "other soils" and is a default entry in the YAML.
# =============================================================================


# ---- shared readings ---------------------------------------------------------

# Master letters of designations written in another notation (FAO/USDA),
# which the Canadian reading leaves aside: A, B, C, E, O and R mean the same
# in every notation, only the suffixes differ. NA where there is none, and
# all NA when the designations are read as Canadian.
.cssc_raw_master <- function(x) {
  .cssc_other_masters(x$h$designation %||% rep(NA_character_, nrow(x$h)), x$notation$ok)
}

# The surface A: the contiguous Ah, Ahe and Ap horizons from the mineral
# surface (Ch 2, pp. 12-13). A horizon is read
#   - from its designation in the Canadian notation;
#   - in another notation, from its master letter: an A (an Ap when it carries
#     a p) is tested as below; B, C, E, O, H, L, M, R, W, a transition (AB,
#     BC) or a buried A (b) ends the A; a letter those notations do not use
#     is read as no designation;
#   - with no designation, by the Ah definition: at least one moist value unit
#     darker than the mineral horizon below it, or at least 0.5 percentage
#     points more organic C than the deepest mineral horizon. Below the first
#     horizon an undesignated horizon must pass both tests and be less than
#     one value unit lighter than the horizon above it, since without a
#     designation nothing else tells an A from a dark B.
# A horizon that cannot be decided may end the A or not, so the result lists
# every A that the profile allows ($prefixes, each with rows, ap_rows, ah_cm
# and ap_cm), the shortest first; .cssc_over_a() tests them all. An unknown
# thickness makes the sums NA.
.cssc_surface_a <- function(x) {
  h <- x$h
  empty <- list(rows = integer(0), ap_rows = integer(0), ah_cm = 0, ap_cm = 0)
  out <- list(rows = integer(0), ah_cm = 0, ap_cm = 0, known = TRUE,
              undecidable = FALSE, prefixes = list(empty))
  if (is.na(x$ms)) {
    # no mineral horizon: no A; mineral horizons without depths: unknown
    if (!any(x$mineral)) return(out)
    return(utils::modifyList(out, list(known = NA, undecidable = TRUE)))
  }
  ord <- order(h$top_cm)
  mins <- ord[x$mineral[ord] & !is.na(h$top_cm[ord]) & h$top_cm[ord] >= x$ms - 0.01]
  # a buried A (b) is not part of the surface A
  buried <- .cssc_has_suffix(x$parsed, "b", weak = "any") %in% TRUE
  ah_d <- .cssc_is(x$parsed, "A", "h", "any") %in% TRUE & !buried
  ap_d <- .cssc_is(x$parsed, "A", "p", "any") %in% TRUE & !buried
  raw <- .cssc_raw_master(x)
  d_other <- .cssc_other_case(trimws(as.character(h$designation)))
  raw_p <- grepl("^[0-9]*A[a-z0-9]*p", d_other)
  raw_b <- grepl("^[0-9]*[A-Z]+[a-z0-9]*b", d_other)
  v <- h$munsell_value_moist
  deepest <- mins[length(mins)]
  status <- function(k) {
    i <- mins[k]
    if (x$desig_known[i]) return(if (ap_d[i]) "ap" else if (ah_d[i]) "ah" else "no")
    foreign <- !is.na(raw[i]) &&
      (raw[i] == "A" || nchar(raw[i]) > 1L ||
         raw[i] %in% c("B", "C", "E", "O", "H", "L", "M", "R", "W"))
    if (foreign && (raw[i] != "A" || raw_b[i])) return("no")
    if (foreign && raw_p[i]) return("ap")
    below <- if (k < length(mins)) mins[k + 1L] else NA_integer_
    darker <- if (is.na(below) || is.na(v[i]) || is.na(v[below])) NA
              else v[i] <= v[below] - 1
    richer <- if (i == deepest || is.na(h$oc_pct[i]) || is.na(h$oc_pct[deepest])) NA
              else h$oc_pct[i] >= h$oc_pct[deepest] + 0.5
    ok <- if (foreign || k == 1L) .any3(c(darker, richer))
          else {
            above <- mins[k - 1L]
            .and3(darker, richer,
                  if (is.na(v[i]) || is.na(v[above])) NA else v[i] < v[above] + 1)
          }
    if (is.na(ok)) NA_character_ else if (ok) "ah" else "no"
  }
  rows <- integer(0); st <- character(0); depth <- x$ms
  for (k in seq_along(mins)) {
    i <- mins[k]
    if (!isTRUE(abs(h$top_cm[i] - depth) < 0.01)) break
    s <- status(k)
    if (identical(s, "no")) break
    rows <- c(rows, i); st <- c(st, s)
    depth <- h$bottom_cm[i]
    if (is.na(depth)) break
  }
  sums <- function(r, s) list(rows = r, ap_rows = r[s %in% "ap"],
                              ah_cm = sum(x$thick[r][!s %in% "ap"]),
                              ap_cm = sum(x$thick[r][s %in% "ap"]))
  ends <- c(which(is.na(st)) - 1L, length(rows))
  out$prefixes <- lapply(sort(unique(ends)), function(e) sums(rows[seq_len(e)], st[seq_len(e)]))
  first <- out$prefixes[[1L]]
  out$rows <- first$rows; out$ah_cm <- first$ah_cm; out$ap_cm <- first$ap_cm
  if (length(out$prefixes) > 1L) out$known <- NA
  out
}

# Apply a test f(a) to the surface A, where a has rows, ah_cm and ap_cm: the
# test is decided when it gives the same answer for every A the profile
# allows.
.cssc_over_a <- function(a, f) {
  if (isTRUE(a$undecidable)) return(NA)
  res <- lapply(a$prefixes, f)
  if (all(vapply(res, identical, logical(1), res[[1L]]))) res[[1L]] else NA
}

# The horizon right under a set of rows (the first whose top is their
# lowest bottom), or NA.
.cssc_row_under <- function(x, rows) {
  if (length(rows) == 0L) return(NA_integer_)
  bottom <- suppressWarnings(max(x$h$bottom_cm[rows]))
  if (is.na(bottom)) return(NA_integer_)
  which(!is.na(x$h$top_cm) & abs(x$h$top_cm - bottom) < 0.01)[1L]
}

# The Ap of a poorly drained cultivated Vertisolic soil that keeps it out of
# the Humic Vertisols (Ch 13, p. 133, the Vertisol text): one of less than
# 2 % organic C; a moist value above 3.5 or a dry value above 5.0; or little
# contrast with the layer underneath (less than 1.5 moist value units when
# that layer has value 4 or more, less than 1 unit when it is below 4). The
# layer underneath is the one under the whole surface A, so a remnant Ah
# under the Ap counts with the Ap.
.cssc_ap_light <- function(x, ap_rows, a_rows) {
  oc <- .cssc_wmean(x, "oc_pct", ap_rows)
  vm <- .cssc_wmean(x, "munsell_value_moist", ap_rows)
  vd <- .cssc_wmean(x, "munsell_value_dry", ap_rows)
  under <- .cssc_row_under(x, a_rows)
  vu <- if (is.na(under)) NA else x$h$munsell_value_moist[under]
  little <- if (is.na(vm) || is.na(vu)) NA else (vu - vm) < (if (vu >= 4) 1.5 else 1)
  .any3(c(if (is.na(oc)) NA else oc < 2,
          .any3(c(if (is.na(vm)) NA else vm > 3.5, if (is.na(vd)) NA else vd > 5.0)),
          little))
}

# Weighted mean of a column over rows (by thickness); NA when a row with a
# value has no thickness.
.cssc_wmean <- function(x, col, rows) {
  if (length(rows) == 0L) return(NA_real_)
  v <- x$h[[col]][rows]; w <- pmax(0, x$thick[rows])
  ok <- !is.na(v)
  if (!any(ok) || anyNA(w[ok])) return(NA_real_)
  if (sum(w[ok]) == 0) return(NA_real_)
  sum(v[ok] * w[ok]) / sum(w[ok])
}

# Drainage group from site$drainage_class (or site$drainage): "free" for very
# rapidly to imperfectly drained, "poor" for poorly and very poorly drained.
# The Canadian classes very rapidly and rapidly drained are read as the
# excessively drained of the shared scale; a range that spans both groups
# ("imperfectly to poorly drained") is undecided.
.cssc_drainage_group <- function(site) {
  t <- tolower(as.character(site$drainage_class %||% site$drainage %||% NA))
  if (length(t) != 1L || is.na(t)) return(NA_character_)
  t <- gsub("(very )?rapidly", "excessively", t)
  grp <- function(k) if (is.na(k)) NA_character_ else if (k >= 7L) "poor" else "free"
  dry <- grp(.drainage_class(t, "drier")); wet <- grp(.drainage_class(t, "wetter"))
  if (identical(dry, wet)) dry else NA_character_
}

# Mean annual soil temperature of at least 8 C (Gray Brown Luvisols): read
# from site$mean_annual_soil_temp_C, or from the USDA temperature regime
# (mesic, thermic and warmer pass; frigid, cryic, gelic fail).
.cssc_mast_at_least_8 <- function(site) {
  t <- suppressWarnings(as.numeric(site$mean_annual_soil_temp_C %||% NA))
  if (!is.na(t)) return(t >= 8)
  r <- tolower(as.character(site$soil_temperature_regime %||% NA))
  if (is.na(r)) return(NA)
  if (grepl("frigid|cryic|gelic", r)) FALSE
  else if (grepl("mesic|thermic", r)) TRUE else NA
}

# Thickness of the Ae horizons (A with e, strong or weak, not an Ahe; an E
# in another notation; not a buried one).
.cssc_ae_cm <- function(x) {
  raw <- .cssc_raw_master(x)
  if (!any(x$desig_known) && all(is.na(raw))) return(NA_real_)
  ae <- if (any(x$desig_known))
          (.cssc_is(x$parsed, "A", "e", "any") %in% TRUE) &
            !(.cssc_is(x$parsed, "A", "h", "any") %in% TRUE)
        else raw %in% "E"
  ae <- ae & !.cssc_buried_rows(x)
  sum(x$thick[ae], na.rm = TRUE)
}


# ---- A. Cryosolic order (p. 35; Ch 6) ---------------------------------------

# Organic material from the surface, the lithic contact and the water or ice
# layers, as the Organic order reads them. The organic column runs from the
# surface through the organic layers; base is where it certainly ends and
# base_max where it would end if the layers of unknown organic status below
# it were organic (Inf when it ends on a layer with no bottom depth). Limnic layers (Ch 2, p. 19): coprogenous earth (Oco), an L
# master in another notation (a limnic layer in FAO/USDA), or a layer that is
# not rock and whose origin is recorded as limnic, coprogenous,
# diatomaceous, marl or gyttja (diatomaceous earth and marl are designated C
# and Ck, so the designation alone does not tell them from mineral layers).
.cssc_organic_column <- function(pedon) {
  h <- pedon$horizons
  pp <- .cssc_parsed(pedon); parsed <- pp$parsed
  lk <- .cssc_layer_kinds(h, pp)
  organic <- lk$organic
  ord <- order(h$top_cm)
  top0 <- min(h$top_cm, na.rm = TRUE)
  walk <- function(accept_unknown) {
    depth <- top0
    for (i in ord) {
      ok <- isTRUE(organic[i]) || (accept_unknown && is.na(organic[i]))
      if (!ok || !isTRUE(h$top_cm[i] <= depth + 0.01)) break
      # a layer with no bottom depth may go on indefinitely
      if (is.na(h$bottom_cm[i])) return(if (accept_unknown) Inf else max(depth, h$top_cm[i]))
      depth <- max(depth, h$bottom_cm[i])
    }
    depth
  }
  base <- walk(FALSE); base_max <- walk(TRUE)
  origin <- tolower(paste(as.character(h$layer_origin %||% NA), as.character(h$rock_origin %||% NA)))
  other <- .cssc_other_masters(h$designation %||% rep(NA_character_, nrow(h)), pp$notation$ok)
  limnic <- (.cssc_is(parsed, "O", "co", "any") %in% TRUE) | (other %in% "L") |
            (grepl("(^|[^a-z])(limnic|coprogenous|diatomaceous|diatomite|marl|gyttja)([^a-z]|$)", origin) &
               !lk$rock)
  list(h = h, parsed = parsed, notation = pp$notation, organic = organic,
       limnic = limnic,
       ord = ord, top0 = top0, organic_cm = base - top0, base = base,
       organic_cm_max = base_max - top0, base_max = base_max,
       rock = lk$rock, water = lk$water, folic = lk$folic,
       desig = vapply(parsed, length, integer(1)) > 0L,
       thick = h$bottom_cm - h$top_cm)
}

#' Organic Cryosol (CSSC great group AA)
#'
#' Formed primarily in organic material, read with the Ch 6 rule (p. 79):
#' organic material more than 40 cm thick from the surface, or more than
#' 10 cm over a lithic contact or over a water or ice layer at least 30 cm
#' thick; with permafrost within 1 m of the surface. A layer of unknown
#' organic status that could change the answer leaves it undecided.
#' @noRd
organic_cryosol_cssc <- function(pedon) {
  oc <- .cssc_organic_column(pedon); h <- oc$h
  primarily_at <- function(base) {
    cm <- base - oc$top0
    nxt <- oc$ord[which(abs(h$top_cm[oc$ord] - base) < 0.01)][1L]
    over <- !is.na(nxt) &&
      (oc$rock[nxt] || (oc$water[nxt] && isTRUE(oc$thick[nxt] >= 30)))
    cm > 40 || (cm > 10 && over)
  }
  lo <- primarily_at(oc$base); hi <- primarily_at(oc$base_max)
  primarily <- if (identical(lo, hi)) lo else NA
  frozen <- cryosolic_cssc(pedon)
  pf_1m <- any(frozen$layers %in% which(h$top_cm - oc$top0 <= 100))
  passed <- if (all(is.na(oc$organic))) NA else if (!pf_1m) FALSE else primarily
  .cssc_result("organic_cryosol_cssc", passed, which(oc$organic %in% TRUE),
               list(route = frozen$evidence$route, organic_cm = oc$organic_cm,
                    organic_cm_max = oc$organic_cm_max, permafrost_within_1m = pf_1m),
               if (is.na(passed)) c("oc_pct", "designation") else character(0),
               "Ch 3, p. 35; Ch 6, p. 79")
}

#' Turbic Cryosol (CSSC great group AB)
#'
#' Marked cryoturbation (a y suffix in the Canadian notation) and permafrost
#' within 2 m of the surface.
#' @noRd
turbic_cryosol_cssc <- function(pedon) {
  pp <- .cssc_parsed(pedon); h <- pedon$horizons
  y <- .cssc_has_suffix(pp$parsed, "y", weak = "any")
  frozen <- (!is.na(h$permafrost_temp_C) & h$permafrost_temp_C <= 0) |
            (.cssc_has_suffix(pp$parsed, "z", weak = "any") %in% TRUE)
  top0 <- min(h$top_cm, na.rm = TRUE)
  pf_2m <- any(frozen & !is.na(h$top_cm) & h$top_cm - top0 <= 200)
  cryo <- if (any(y %in% TRUE)) TRUE else if (all(is.na(y))) NA else FALSE
  passed <- if (identical(cryo, FALSE) || !pf_2m) FALSE else cryo
  .cssc_result("turbic_cryosol_cssc", passed, which(y %in% TRUE),
               list(route = "designations", notation = pp$notation$why,
                    cryoturbated = cryo, permafrost_within_2m = pf_2m),
               if (is.na(passed)) "designation" else character(0),
               "Ch 3, p. 35; Ch 6, p. 74")
}


# ---- B. Organic order (p. 35; Ch 9) -----------------------------------------

#' Folisol (CSSC great group BA)
#'
#' Formed primarily in upland folic material (L, F, H), rarely saturated.
#' With designations, the folic horizons that run from the surface must meet
#' one of the Ch 9 criteria (p. 104): 40 cm or more of folic material; or at
#' least 10 cm lying directly on a lithic contact, or on a mineral layer that
#' lies on the contact and is thinner than 20 cm and less than half the folic
#' thickness (fragmental material is not read). An organic layer with no
#' designation may be folic or peat, and a layer of unknown organic status
#' may be mineral or organic; either leaves the test undecided when that
#' changes the answer. Without designations, a site recorded as rapidly to
#' imperfectly drained counts as upland folic material and a poorly drained
#' one does not.
#' @noRd
folisol_cssc <- function(pedon) {
  oc <- .cssc_organic_column(pedon); h <- oc$h
  ord <- oc$ord[!is.na(h$top_cm[oc$ord])]
  fol_lo <- fol_hi <- NA_real_
  if (any(oc$desig)) {
    # the folic run from the surface; with accept, undesignated layers that
    # may be folic continue it
    run_from_top <- function(accept) {
      depth <- oc$top0; k_end <- 0L; cm <- 0
      for (k in seq_along(ord)) {
        i <- ord[k]
        ok <- oc$folic[i] ||
          (accept && !oc$desig[i] && !(oc$organic[i] %in% FALSE) && !oc$rock[i] && !oc$water[i])
        if (!ok || !isTRUE(abs(h$top_cm[i] - depth) < 0.01)) break
        cm <- cm + oc$thick[i]; depth <- h$bottom_cm[i]; k_end <- k
        if (is.na(depth)) break
      }
      list(cm = cm, k = k_end, depth = depth)
    }
    meets <- function(run) {
      f <- run$cm
      if (run$k == 0L) return(FALSE)
      if (is.na(f)) return(NA)
      mineral_cm <- 0; on_rock <- FALSE; unknown <- FALSE; depth <- run$depth
      for (i in ord[seq_along(ord) > run$k]) {
        if (!isTRUE(abs(h$top_cm[i] - depth) < 0.01)) break
        if (oc$rock[i]) { on_rock <- TRUE; break }
        if (oc$water[i] || isTRUE(oc$organic[i])) break
        if (is.na(oc$organic[i])) unknown <- TRUE
        mineral_cm <- mineral_cm + oc$thick[i]; depth <- h$bottom_cm[i]
        if (is.na(depth)) break
      }
      over_rock <- function(rock) {
        if (!rock || f < 10) return(FALSE)
        if (isTRUE(mineral_cm == 0)) return(TRUE)
        if (is.na(mineral_cm)) return(NA)
        mineral_cm < 20 && f > 2 * mineral_cm
      }
      lo <- f >= 40 || isTRUE(over_rock(on_rock && !unknown))
      hi <- .any3(c(f >= 40, over_rock(on_rock)))
      if (identical(lo, hi)) lo else NA
    }
    r_lo <- run_from_top(FALSE); r_hi <- run_from_top(TRUE)
    fol_lo <- r_lo$cm; fol_hi <- r_hi$cm
    lo <- meets(r_lo); hi <- meets(r_hi)
    passed <- if (identical(lo, hi)) lo else NA
    route <- "designations"
  } else {
    g <- .cssc_drainage_group(pedon$site)
    passed <- if (is.na(g)) NA else g == "free"
    route <- "properties"
  }
  .cssc_result("folisol_cssc", passed, if (isTRUE(passed)) which(oc$folic) else integer(0),
               list(route = route, notation = oc$notation$why,
                    folic_cm = fol_lo, folic_cm_max = fol_hi),
               if (is.na(passed)) c("designation", "site$drainage_class") else character(0),
               "Ch 3, p. 35; Ch 9, pp. 98-99 and 104")
}

# Fibric, mesic or humic kind of an organic layer: by rubbed fibre when it is
# measured (40 % or more, 10 to less than 40 %, below 10 %; Ch 2, p. 18; the
# pyrophosphate index is not read, no attribute holds it), otherwise by an
# Of, Om or Oh designation, otherwise by the von Post class the book gives as
# usual (1-4 fibric, 5-6 mesic, 7 or more humic).
.cssc_organic_kind <- function(oc, i) {
  if (!isTRUE(oc$organic[i])) return(NA_character_)
  fib <- oc$h$fiber_content_rubbed_pct[i]
  if (!is.na(fib)) return(if (fib >= 40) "fibric" else if (fib >= 10) "mesic" else "humic")
  codes <- c(f = "fibric", m = "mesic", h = "humic")
  for (sfx in names(codes))
    if (.cssc_is(oc$parsed[i], "O", sfx, "any") %in% TRUE) return(codes[[sfx]])
  vp <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(oc$h$von_post_index[i]))))
  if (length(vp) == 1L && !is.na(vp))
    return(if (vp <= 4) "fibric" else if (vp <= 6) "mesic" else "humic")
  NA_character_
}

# Terric, lithic and hydric layers of an Organic soil (Ch 2, p. 19), TRUE,
# FALSE or NA on the row where each begins. Lithic: bedrock within 10-160 cm
# of the surface. Terric: a mineral layer not underlain by organic matter, or
# one continuous mineral layer more than 30 cm thick; a thinner mineral layer
# with organic matter below is a cumulic layer, not a contact. Hydric: water
# from 40 cm or deeper that reaches below 160 cm; a water layer with other
# layers recorded under it within 160 cm is not hydric, and one that is the
# deepest layer recorded but ends above 160 cm is undecided. A layer of
# unknown organic status may be mineral or organic, and leaves the contact
# undecided when that changes it.
.cssc_organic_contacts <- function(oc) {
  h <- oc$h; n <- nrow(h); ord <- oc$ord[!is.na(h$top_cm[oc$ord])]
  rel_top <- h$top_cm - oc$top0; rel_bot <- h$bottom_cm - oc$top0
  out <- rep(FALSE, n)
  out[oc$rock & !is.na(rel_top) & rel_top >= 10 & rel_top <= 160] <- TRUE
  for (k in seq_along(ord)) {
    i <- ord[k]
    if (oc$water[i] && rel_top[i] >= 40) {
      out[i] <- if (isTRUE(rel_bot[i] > 160)) TRUE
                else if (k == length(ord)) NA else FALSE
    }
  }
  maybe_org <- (oc$organic %in% TRUE) | (is.na(oc$organic) & !oc$rock & !oc$water)
  after <- function(pos) {
    l <- ord[seq_along(ord) > pos]
    l[!is.na(rel_top[l]) & rel_top[l] < 160]
  }
  cand <- !(oc$organic %in% TRUE) & !oc$rock & !oc$water & !oc$limnic
  k <- 1L
  while (k <= length(ord)) {
    if (!cand[ord[k]]) { k <- k + 1L; next }
    j <- k
    while (j < length(ord) && cand[ord[j + 1L]] &&
           isTRUE(abs(h$top_cm[ord[j + 1L]] - h$bottom_cm[ord[j]]) < 0.01)) j <- j + 1L
    run <- ord[k:j]
    # possibly terric: the unknown layers of the run taken as mineral
    possible <- !any(oc$organic[after(j)] %in% TRUE) ||
                isTRUE(sum(oc$thick[run]) > 30)
    # certainly terric: known mineral layers more than 30 cm thick together,
    # or with nothing below them that could be organic matter
    sure <- oc$organic[run] %in% FALSE
    certain <- NA_integer_
    kk <- 1L
    while (kk <= length(run)) {
      if (!sure[kk]) { kk <- kk + 1L; next }
      jj <- kk
      while (jj < length(run) && sure[jj + 1L]) jj <- jj + 1L
      sub <- run[kk:jj]
      if (isTRUE(sum(oc$thick[sub]) > 30) || !any(maybe_org[after(k + jj - 1L)])) {
        certain <- sub[1L]; break
      }
      kk <- jj + 1L
    }
    if (!is.na(certain)) out[certain] <- TRUE
    else out[run[1L]] <- if (possible) NA else FALSE
    k <- j + 1L
  }
  out
}

# Kind (fibric, mesic, humic) of every layer, NA where it is not peat or not
# known (.cssc_organic_kind).
.cssc_kinds <- function(oc) {
  vapply(seq_len(nrow(oc$h)), function(i) .cssc_organic_kind(oc, i), character(1))
}

# The dominant material among thicknesses by kind: with two materials, the
# one making up more than half; with three, the thickest (Ch 9, p. 100,
# stated for fibric and applied to all three).
.cssc_pick_dominant <- function(t) {
  if (length(t[t > 0]) == 0L) return(NA_character_)
  if (length(t[t > 0]) <= 2L) {
    k <- names(t)[which.max(t)]
    if (t[[k]] > sum(t) / 2) k else NA_character_
  } else {
    m <- max(t); k <- names(t)[t == m]
    if (length(k) == 1L) k else NA_character_
  }
}

# Thickness of peat by kind between depths `from` and `to` below the surface,
# with the thickness that could be peat of any kind (organic layers of unknown
# kind, layers of unknown organic status) and, by kind, the thickness that a
# peat layer with no bottom depth could add (open_cm: it may reach the end of
# the window); the dominant kind is given only when no such unknown can
# change it. Limnic and folic (L, F, H) layers are not fibric, mesic or humic
# material and are left out.
.cssc_kind_tally <- function(oc, from, to, kind = .cssc_kinds(oc)) {
  h <- oc$h
  rel_top <- h$top_cm - oc$top0
  overlap <- pmax(0, pmin(h$bottom_cm - oc$top0, to) - pmax(rel_top, from))
  open <- ifelse(is.na(overlap) & !is.na(rel_top) & rel_top < to, to - pmax(rel_top, from), 0)
  overlap[is.na(overlap)] <- 0
  peat_ok <- !oc$limnic & !oc$folic & !oc$rock & !oc$water
  org <- oc$organic %in% TRUE & peat_ok
  kinds <- c("fibric", "mesic", "humic")
  tot <- vapply(kinds, function(k) sum(overlap[org & kind %in% k]), numeric(1))
  open_cm <- vapply(kinds, function(k) sum(open[org & kind %in% k]), numeric(1))
  unk_layer <- is.na(oc$organic) & peat_ok
  unknown <- sum(overlap[org & is.na(kind)]) + sum(overlap[unk_layer]) +
             sum(open[(org & is.na(kind)) | unk_layer])
  # every reading the unknowns allow must give the same dominant kind: open
  # layers at their least (an open layer that starts in the window has some
  # thickness in it), one at a time at their most, or all at once, and the
  # unknown thickness added to each kind in turn
  base <- tot
  for (k in kinds[open_cm > 0]) {
    starts_in <- any(org & kind %in% k & open > 0 & rel_top >= from)
    if (starts_in) base[[k]] <- base[[k]] + 1e-6
  }
  scen <- list(base)
  for (k in kinds[open_cm > 0]) { t2 <- base; t2[[k]] <- tot[[k]] + open_cm[[k]]; scen <- c(scen, list(t2)) }
  if (sum(open_cm > 0) > 1L) scen <- c(scen, list(tot + open_cm))
  alts <- unlist(lapply(scen, function(t) {
    if (unknown > 0) vapply(kinds, function(k) { t2 <- t; t2[[k]] <- t2[[k]] + unknown; .cssc_pick_dominant(t2) }, "")
    else .cssc_pick_dominant(t)
  }))
  dom <- if (length(unique(alts)) == 1L) alts[[1L]] else NA_character_
  any_peat <- any(tot > 0) || any(open_cm > 0)
  list(dominant = dom, thickness = tot, open_cm = open_cm, unknown_cm = unknown,
       window = c(from, to),
       decided = !is.na(dom) || (unknown == 0 && sum(open_cm) == 0 && any(tot > 0)),
       empty = !any_peat && unknown == 0)
}

# The dominant organic material of the middle tier (40-120 cm from the
# surface), or of the surface and middle tiers together (0-120 cm) when a
# terric, lithic or hydric layer (.cssc_organic_contacts) begins above
# 120 cm (Ch 9, pp. 99-102).
.cssc_tier_dominant <- function(pedon) {
  oc <- .cssc_organic_column(pedon); h <- oc$h
  kind <- .cssc_kinds(oc)
  rel_top <- h$top_cm - oc$top0
  contact <- .cssc_organic_contacts(oc)
  reach <- !is.na(rel_top) & rel_top < 120
  if (any(contact[reach] %in% TRUE)) return(.cssc_kind_tally(oc, 0, 120, kind))
  r <- .cssc_kind_tally(oc, 40, 120, kind)
  # a middle tier of limnic material only, with no peat: the surface and
  # middle tiers are read together, as with a contact
  lim_mid <- oc$limnic & !is.na(rel_top) & rel_top < 120 &
             !((h$bottom_cm - oc$top0 <= 40) %in% TRUE)
  if (isTRUE(r$empty) && any(lim_mid)) {
    return(.cssc_kind_tally(oc, 0, 120, kind))
  }
  if (anyNA(contact[reach])) {
    r0 <- .cssc_kind_tally(oc, 0, 120, kind)
    if (!identical(r0$dominant, r$dominant) || !identical(r0$decided, r$decided)) {
      r$dominant <- NA_character_; r$decided <- FALSE
    }
  }
  r
}

.cssc_tier_gate <- function(pedon, material, name, where) {
  t <- .cssc_tier_dominant(pedon)
  passed <- if (!is.na(t$dominant)) t$dominant == material
            else if (isTRUE(t$decided)) FALSE else NA
  .cssc_result(name, passed, integer(0),
               list(route = "properties+designations", dominant = t$dominant,
                    thickness_cm = t$thickness, unknown_cm = t$unknown_cm,
                    tier_window_cm = t$window),
               if (is.na(passed)) c("fiber_content_rubbed_pct", "designation") else character(0),
               where)
}

#' Fibrisol, Mesisol and Humisol (CSSC great groups BB, BC, BD)
#'
#' The dominant material of the middle tier is fibric, mesic or humic.
#' @noRd
fibrisol_cssc <- function(pedon) .cssc_tier_gate(pedon, "fibric", "fibrisol_cssc",
                                                 "Ch 3, p. 35; Ch 9, pp. 99-100")
#' @noRd
mesisol_cssc  <- function(pedon) .cssc_tier_gate(pedon, "mesic", "mesisol_cssc",
                                                 "Ch 3, p. 35; Ch 9, p. 101")
#' @noRd
humisol_cssc  <- function(pedon) .cssc_tier_gate(pedon, "humic", "humisol_cssc",
                                                 "Ch 3, p. 35; Ch 9, p. 102")


# ---- C. Vertisolic order (p. 35; Ch 13) -------------------------------------

#' Vertisol (CSSC great group CA)
#'
#' Well to imperfectly drained: a surface dry colour value of 3.5 or more.
#' Poorly drained: an Ah thinner than 10 cm or, in a cultivated soil (an Ap
#' at the surface), an Ap with one of the properties of the Ch 13 Vertisol
#' text (p. 133; see .cssc_ap_light()).
#' Drainage is read from site$drainage_class; rapidly drained soils count
#' with the well drained and very poorly drained with the poorly drained.
#' With the drainage unknown, the great group is decided only when both
#' branches agree.
#' @noRd
vertisol_cssc <- function(pedon) {
  x <- .cssc_horizons(pedon)
  a <- .cssc_surface_a(x)
  first <- order(x$h$top_cm)[x$mineral[order(x$h$top_cm)]][1L]
  light <- .cssc_over_a(a, function(s) {
    v <- .cssc_wmean(x, "munsell_value_dry", if (length(s$rows)) s$rows else first)
    if (is.na(v)) NA else v >= 3.5
  })
  poor_vertisol <- .cssc_over_a(a, function(s) {
    if (length(s$ap_rows)) .cssc_ap_light(x, s$ap_rows, s$rows) else s$ah_cm < 10
  })
  g <- .cssc_drainage_group(pedon$site)
  passed <- if (identical(g, "free")) light
            else if (identical(g, "poor")) poor_vertisol
            else if (isTRUE(light) && isTRUE(poor_vertisol)) TRUE
            else if (identical(light, FALSE) && identical(poor_vertisol, FALSE)) FALSE
            else NA
  surf_rows <- if (length(a$rows)) a$rows else first
  .cssc_result("vertisol_cssc", passed, surf_rows,
               list(route = "properties", drainage = g,
                    surface_value_dry = .cssc_wmean(x, "munsell_value_dry", surf_rows),
                    ah_cm = a$ah_cm, ap_cm = a$ap_cm),
               if (is.na(passed)) c("munsell_value_dry", "site$drainage_class") else character(0),
               "Ch 3, p. 35; Ch 13, pp. 132-133")
}


# ---- D. Podzolic order (p. 35; Ch 10) ---------------------------------------

#' Humic Podzol and Ferro-Humic Podzol (CSSC great groups DA, DB)
#'
#' A Bh horizon (Bh chemistry) at least 10 cm thick; or a Bhf horizon (Bf
#' chemistry with organic C above 5 %) at least 10 cm thick.
#' @noRd
humic_podzol_cssc <- function(pedon) {
  x <- .cssc_horizons(pedon)
  r <- .cssc_podzolic_run(x, "bh", "d_bh")
  .cssc_result("humic_podzol_cssc", r$passed, r$rows,
               list(route = r$route, by_properties = r$by_props, by_designations = r$by_desig,
                    thickness_properties = r$thickness_properties,
                    thickness_designations = r$thickness_designations),
               if (is.na(r$passed)) c("oc_pct", "fe_pyro_pct", "designation") else character(0),
               "Ch 3, p. 35; Ch 10, p. 110")
}

#' @noRd
ferro_humic_podzol_cssc <- function(pedon) {
  x <- .cssc_horizons(pedon)
  r <- .cssc_podzolic_run(x, "bhf", "d_bhf")
  .cssc_result("ferro_humic_podzol_cssc", r$passed, r$rows,
               list(route = r$route, by_properties = r$by_props, by_designations = r$by_desig,
                    thickness_properties = r$thickness_properties,
                    thickness_designations = r$thickness_designations),
               if (is.na(r$passed)) c("oc_pct", "fe_pyro_pct", "al_pyro_pct", "designation")
               else character(0),
               "Ch 3, p. 35; Ch 10, p. 112")
}


# ---- E. Gleysolic order (p. 35; Ch 7) ---------------------------------------

#' Luvic Gleysol (CSSC great group EA)
#'
#' A Btg horizon: a Bt horizon that is gleyed, by its colours or by a g in a
#' Bt designation (Btg).
#' @noRd
luvic_gleysol_cssc <- function(pedon) {
  x <- .cssc_horizons(pedon)
  bt <- bt_horizon_cssc(pedon)
  gley <- .cssc_gley_by_colour(x)
  # gley colours on the Bt layers, or on layers that may be part of the Bt
  g_sure <- gley[bt$layers]; g_maybe <- gley[bt$evidence$maybe_rows %||% integer(0)]
  by_props <- if (identical(bt$passed, FALSE)) FALSE
              else if (any(g_sure %in% TRUE)) TRUE
              else if (!isTRUE(bt$passed) || anyNA(g_sure) || any(!(g_maybe %in% FALSE))) NA
              else FALSE
  # a Btg: a Bt by designation (adjacent Bt horizons at least 5 cm in all, as
  # for the Bt; the deepest horizon with no bottom may go on; a mineral
  # horizon with no designation may be one) of which a part is a Btg
  btg <- .cssc_is(x$parsed, "B", c("t", "g"), "no")
  bt_d <- .cssc_is(x$parsed, "B", "t", "no")
  bt_d[!x$mineral] <- FALSE
  rt <- .cssc_run_test(x, bt_d, 5)
  # runs that may reach 5 cm and may hold a Btg (a Btg, or a horizon with no
  # designation)
  maybe_btg <- vapply(.cssc_run_groups(x, !(bt_d %in% FALSE)), function(r) {
    long <- anyNA(x$thick[r]) || isTRUE(.cssc_run_span(x, r) >= 5)
    long && (any(btg[r] %in% TRUE) || anyNA(bt_d[r]))
  }, logical(1))
  by_desig <- if (!any(x$desig_known)) NA
              else if (isTRUE(rt$passed) && any(btg[rt$rows] %in% TRUE)) TRUE
              else if (any(maybe_btg)) NA else FALSE
  r <- .cssc_route(by_props, by_desig)
  .cssc_result("luvic_gleysol_cssc", r$passed,
               if (r$route == "properties") bt$layers else which(btg %in% TRUE),
               list(route = r$route, by_properties = by_props, by_designations = by_desig),
               if (is.na(r$passed)) c("munsell_chroma_moist", "designation") else character(0),
               "Ch 3, p. 35; Ch 7, p. 84")
}

#' Humic Gleysol (CSSC great group EB)
#'
#' An Ah at least 10 cm thick or an Ap at least 15 cm thick, with at least
#' 2 % organic C in that surface horizon.
#' @noRd
humic_gleysol_cssc <- function(pedon) {
  x <- .cssc_horizons(pedon)
  a <- .cssc_surface_a(x)
  passed <- .cssc_over_a(a, function(s) {
    if (length(s$rows) == 0L) return(FALSE)
    oc <- .cssc_wmean(x, "oc_pct", s$rows)
    .and3(s$ah_cm >= 10 || s$ap_cm >= 15, if (is.na(oc)) NA else oc >= 2)
  })
  oc <- .cssc_wmean(x, "oc_pct", a$rows)
  .cssc_result("humic_gleysol_cssc", passed, a$rows,
               list(route = if (any(x$desig_known)) "designations+properties" else "properties",
                    ah_cm = a$ah_cm, ap_cm = a$ap_cm, organic_c = oc),
               if (is.na(passed)) c("oc_pct", "designation") else character(0),
               "Ch 3, p. 35; Ch 7, p. 85")
}


# ---- F. Solonetzic order (p. 36; Ch 12) -------------------------------------

#' Vertic Solonetz (CSSC great group FA)
#'
#' A slickenside horizon whose top lies within 1 m of the mineral surface,
#' read horizon by horizon (slickenside_horizon_cssc): an ss in a designation,
#' or common, many or continuous slickensides.
#' @noRd
vertic_solonetz_cssc <- function(pedon) {
  r <- slickenside_horizon_cssc(pedon, 100)
  .cssc_result("vertic_solonetz_cssc", r$passed, r$rows,
               list(route = "properties+designations"),
               if (is.na(r$passed)) c("slickensides", "designation") else character(0),
               "Ch 3, p. 36; Ch 12, p. 128")
}

#' Solod and Solodized Solonetz (CSSC great groups FB, FC)
#'
#' An Ae horizon at least 2 cm thick; the Solod also has a distinct AB or BA
#' horizon, the upper part of the solonetzic B breaking down. Read from the
#' designations.
#' @noRd
solod_cssc <- function(pedon) {
  x <- .cssc_horizons(pedon)
  ae <- .cssc_ae_cm(x)
  ab <- .cssc_is(x$parsed, c("AB", "BA"), weak = "any")
  # in another notation the transition between the E and the B (EB, BE) is
  # the Canadian AB or BA
  ab <- ifelse(!is.na(ab), ab, .cssc_raw_master(x) %in% c("AB", "BA", "EB", "BE"))
  passed <- if (is.na(ae)) NA else ae >= 2 && any(ab %in% TRUE)
  .cssc_result("solod_cssc", passed, which(ab %in% TRUE),
               list(route = "designations", ae_cm = ae),
               if (is.na(passed)) "designation" else character(0),
               "Ch 3, p. 36; Ch 12, p. 126")
}

#' @noRd
solodized_solonetz_cssc <- function(pedon) {
  x <- .cssc_horizons(pedon)
  ae <- .cssc_ae_cm(x)
  passed <- if (is.na(ae)) NA else ae >= 2
  .cssc_result("solodized_solonetz_cssc", passed, integer(0),
               list(route = "designations", ae_cm = ae),
               if (is.na(passed)) "designation" else character(0),
               "Ch 3, p. 36; Ch 12, p. 124")
}


# ---- G. Chernozemic order (p. 36; Ch 5) -------------------------------------

# Dry colour of the chernozemic A (means rounded to 1e-6, so that a mean of
# 3.5 is 3.5), whether it is an Ap, and signs of eluviation under forest: an
# Ahe, Aej or Ae in the profile (not a buried one), or a cultivated A whose
# Ap has a dry chroma of 1.5 or less, the Ahe the plough has mixed in (the
# key gives the Ap of the Dark Gray Chernozems its own value limit, and
# Ch 5, p. 69, admits a mixed Ap).
.cssc_chernozemic_colour <- function(pedon) {
  x <- .cssc_horizons(pedon)
  a <- chernozemic_a_cssc(pedon)
  rows <- a$layers
  is_ap <- .cssc_is(x$parsed[rows], "A", "p", "any") %in% TRUE
  ap <- length(rows) > 0L && any(is_ap)
  r6 <- function(v) if (is.na(v)) v else round(v, 6)
  chroma <- r6(.cssc_wmean(x, "munsell_chroma_dry", rows))
  buried <- .cssc_has_suffix(x$parsed, "b", weak = "any") %in% TRUE
  elu_desig <- if (!any(x$desig_known)) NA
               else any(.cssc_is(x$parsed, "A", "e", "any") %in% TRUE & !buried)
  elu <- elu_desig; source <- if (isTRUE(elu_desig)) "designation" else NA_character_
  if (ap && identical(elu_desig, FALSE)) {
    ap_chroma <- r6(.cssc_wmean(x, "munsell_chroma_dry", rows[is_ap]))
    elu <- if (is.na(ap_chroma)) NA else ap_chroma <= 1.5
    if (isTRUE(elu)) source <- "Ap chroma"
  }
  list(value = r6(.cssc_wmean(x, "munsell_value_dry", rows)), chroma = chroma, ap = ap,
       eluviated = elu, eluviated_by_designation = elu_desig, eluviation_from = source)
}

# The dry value 3.5 is the limit of both the Black ("darker than 3.5") and
# the Dark Brown Chernozems ("3.5 to 4.5"); there the chroma decides: 1.5 or
# less, the chroma of the Black, makes it Black, unless an Ahe, Aej or Ae
# marks the eluviation of the Dark Gray, whose range holds 3.5 by the key.
.cssc_black_at_limit <- function(cc) {
  if (is.na(cc$value) || cc$value != 3.5) return(FALSE)
  if (isTRUE(cc$eluviated_by_designation)) return(FALSE)
  low <- if (is.na(cc$chroma)) NA else cc$chroma <= 1.5
  if (identical(low, FALSE)) return(FALSE)
  if (isTRUE(low) && identical(cc$eluviated_by_designation, FALSE)) TRUE else NA
}

.cssc_chern_gate <- function(pedon, name, test, where) {
  cc <- .cssc_chernozemic_colour(pedon)
  passed <- test(cc)
  .cssc_result(name, passed, integer(0),
               list(route = "properties", value_dry = cc$value, chroma_dry = cc$chroma,
                    ap = cc$ap, eluviated = cc$eluviated, eluviation_from = cc$eluviation_from),
               if (is.na(passed)) c("munsell_value_dry", "munsell_chroma_dry") else character(0),
               where)
}

#' Brown, Dark Brown, Black and Dark Gray Chernozem (CSSC great groups GA-GD)
#'
#' The dry colour of the chernozemic A: value 4.5 to 5.5 (Brown); 3.5 to 4.5
#' (Dark Brown) unless the chroma is 1.5 or less with signs of eluviation
#' under forest, which make it Dark Gray, or the value is 3.5 with a chroma
#' of 1.5 or less, which makes it Black; below 3.5 (Black); 3.5 to 4.5, or
#' to 5.0 for an Ap, with chroma 1.5 or less and signs of eluviation (Dark
#' Gray: an Ahe, Aej or Ae in the profile, not a buried one, or an Ap of
#' chroma 1.5 or less). The chroma limits are "usually" in the key and are
#' read only to tell Dark Brown from Dark Gray (where, in an Ap, the chroma
#' also stands for the eluviation), and Dark Brown from Black at the value
#' 3.5 (an Ahe, Aej or Ae keeps 3.5 for the Dark Gray).
#' @noRd
brown_chernozem_cssc <- function(pedon) .cssc_chern_gate(pedon, "brown_chernozem_cssc",
  function(cc) if (is.na(cc$value)) NA else cc$value >= 4.5 && cc$value <= 5.5,
  "Ch 3, p. 36; Ch 5, p. 63")

#' @noRd
dark_brown_chernozem_cssc <- function(pedon) .cssc_chern_gate(pedon, "dark_brown_chernozem_cssc",
  function(cc) {
    if (is.na(cc$value)) return(NA)
    if (!(cc$value >= 3.5 && cc$value <= 4.5)) return(FALSE)
    black <- .cssc_black_at_limit(cc)
    if (isTRUE(black)) return(FALSE)
    if (is.na(black)) return(NA)
    gd_like <- if (is.na(cc$chroma)) NA else cc$chroma <= 1.5
    if (identical(gd_like, FALSE)) return(TRUE)
    if (identical(cc$eluviated, FALSE)) return(TRUE)
    if (isTRUE(gd_like) && isTRUE(cc$eluviated)) return(FALSE)
    NA
  }, "Ch 3, p. 36; Ch 5, p. 66")

#' @noRd
black_chernozem_cssc <- function(pedon) .cssc_chern_gate(pedon, "black_chernozem_cssc",
  function(cc) {
    if (is.na(cc$value)) return(NA)
    if (cc$value < 3.5) return(TRUE)
    .cssc_black_at_limit(cc)
  }, "Ch 3, p. 36; Ch 5, p. 68")

#' @noRd
dark_gray_chernozem_cssc <- function(pedon) .cssc_chern_gate(pedon, "dark_gray_chernozem_cssc",
  function(cc) {
    if (is.na(cc$value)) return(NA)
    upper <- if (cc$ap) 5.0 else 4.5
    in_range <- cc$value >= 3.5 && cc$value <= upper
    if (!in_range) return(FALSE)
    if (identical(cc$eluviated, FALSE)) return(FALSE)
    chroma_ok <- if (is.na(cc$chroma)) NA else cc$chroma <= 1.5
    if (identical(chroma_ok, FALSE)) return(FALSE)
    if (isTRUE(chroma_ok) && isTRUE(cc$eluviated)) TRUE else NA
  }, "Ch 3, p. 36; Ch 5, pp. 69-70")


# ---- H. Luvisolic order (p. 36; Ch 8) ---------------------------------------

#' Gray Brown Luvisol (CSSC great group HA)
#'
#' A forest mull Ah (an Ah or Ap at the mineral surface; the chapter asks for
#' more than 5 cm) and a mean annual soil temperature of at least 8 C.
#' @noRd
gray_brown_luvisol_cssc <- function(pedon) {
  x <- .cssc_horizons(pedon)
  a <- .cssc_surface_a(x)
  mull <- .cssc_over_a(a, function(s) (s$ah_cm + s$ap_cm) > 5)
  warm <- .cssc_mast_at_least_8(pedon$site)
  passed <- if (identical(mull, FALSE) || identical(warm, FALSE)) FALSE
            else if (isTRUE(mull) && isTRUE(warm)) TRUE else NA
  .cssc_result("gray_brown_luvisol_cssc", passed, a$rows,
               list(route = "designations+site", mull_ah = mull, mast_at_least_8 = warm),
               if (is.na(passed)) c("designation", "site$mean_annual_soil_temp_C") else character(0),
               "Ch 3, p. 36; Ch 8, p. 91")
}


# ---- I. Brunisolic order (p. 36; Ch 4) --------------------------------------

# pH 5.5 or more (0.01 M CaCl2) on each of the rows, TRUE, FALSE or NA;
# without pH in CaCl2, pH in water of 6.5 or more is high and below 5.5 low.
.cssc_ph_high <- function(h, rows) {
  ph <- h$ph_cacl2[rows]; pw <- h$ph_h2o[rows]
  ifelse(!is.na(ph), ph >= 5.5, ifelse(!is.na(pw) & pw >= 6.5, TRUE,
         ifelse(!is.na(pw) & pw < 5.5, FALSE, NA)))
}

# pH (0.01 M CaCl2) in the uppermost 25 cm of the B (or of the B and the
# material below it to 25 cm): "high" when some part reaches 5.5, "low" when
# all of it is below 5.5. Without pH in CaCl2, pH in water stands in: 6.5 or
# more counts as high and below 5.5 as low (pH in CaCl2 is lower than in
# water); between, it is undecided. The B is found by a B designation (in
# any notation) or, without one, under the surface A; an A that cannot be
# decided leaves the pH undecided when the window moves the answer.
.cssc_b_ph <- function(x) {
  h <- x$h
  none <- list(high = NA, low = NA)
  at <- function(b_top) {
    if (is.na(b_top)) return(none)
    win <- !is.na(h$top_cm) & h$top_cm < b_top + 25 & !(h$bottom_cm %in% NA) &
           h$bottom_cm > b_top
    high <- .cssc_ph_high(h, which(win))
    if (length(high) == 0L) return(none)
    some_high <- .any3(high)
    list(high = some_high, low = if (is.na(some_high)) NA else !some_high)
  }
  is_b <- (.cssc_is(x$parsed, "B", weak = "any") %in% TRUE) |
          (.cssc_raw_master(x) %in% "B")
  if (any(is_b & !is.na(h$top_cm))) return(at(min(h$top_cm[is_b], na.rm = TRUE)))
  a <- .cssc_surface_a(x)
  if (isTRUE(a$undecidable)) return(none)
  res <- lapply(a$prefixes, function(s)
    at(if (length(s$rows)) suppressWarnings(max(h$bottom_cm[s$rows])) else x$ms))
  if (all(vapply(res, identical, logical(1), res[[1L]]))) res[[1L]] else none
}

# An Ah or Ap at least 10 cm thick; an Ap qualifies with a moist value
# below 4 (Ch 4, p. 55).
.cssc_thick_dark_a <- function(x) {
  .cssc_over_a(.cssc_surface_a(x), function(s) {
    if (isTRUE(s$ah_cm >= 10)) return(TRUE)
    ap_ok <- if (isTRUE(s$ap_cm >= 10)) {
      v <- .cssc_wmean(x, "munsell_value_moist", s$rows)
      if (is.na(v)) NA else v < 4
    } else if (is.na(s$ap_cm)) NA else FALSE
    if (isTRUE(ap_ok)) TRUE else if (is.na(s$ah_cm) || is.na(ap_ok)) NA else FALSE
  })
}

.cssc_bruni_gate <- function(pedon, name, need_thick_a, need_high, where) {
  x <- .cssc_horizons(pedon)
  ta <- if (is.na(need_thick_a)) TRUE else {
    t <- .cssc_thick_dark_a(x); if (is.na(t)) NA else t == need_thick_a
  }
  p <- .cssc_b_ph(x)
  ph_ok <- if (need_high) p$high else p$low
  passed <- if (identical(ta, FALSE) || identical(ph_ok, FALSE)) FALSE
            else if (isTRUE(ta) && isTRUE(ph_ok)) TRUE else NA
  .cssc_result(name, passed, integer(0),
               list(route = "properties+designations", thick_dark_a = ta, ph = p),
               if (is.na(passed)) c("ph_cacl2", "designation") else character(0), where)
}

#' Melanic, Eutric and Sombric Brunisol (CSSC great groups IA, IB, IC)
#'
#' Melanic: an Ah or Ap at least 10 cm thick and pH (CaCl2) of 5.5 or more in
#' the upper B. Eutric: other soils with that pH. Sombric: an Ah or Ap at
#' least 10 cm thick and pH below 5.5. Dystric (ID) is the default.
#' @noRd
melanic_brunisol_cssc <- function(pedon) .cssc_bruni_gate(pedon, "melanic_brunisol_cssc",
  TRUE, TRUE, "Ch 3, p. 36; Ch 4, p. 55")
#' @noRd
eutric_brunisol_cssc <- function(pedon) .cssc_bruni_gate(pedon, "eutric_brunisol_cssc",
  NA, TRUE, "Ch 3, p. 36; Ch 4, p. 56")
#' @noRd
sombric_brunisol_cssc <- function(pedon) .cssc_bruni_gate(pedon, "sombric_brunisol_cssc",
  TRUE, FALSE, "Ch 3, p. 36; Ch 4, p. 57")


# ---- J. Regosolic order (p. 36; Ch 11) --------------------------------------

#' Humic Regosol (CSSC great group JA)
#'
#' An Ah or Ap at least 10 cm thick at the mineral surface.
#' @noRd
humic_regosol_cssc <- function(pedon) {
  x <- .cssc_horizons(pedon)
  a <- .cssc_surface_a(x)
  passed <- .cssc_over_a(a, function(s) (s$ah_cm + s$ap_cm) >= 10)
  .cssc_result("humic_regosol_cssc", passed, a$rows,
               list(route = if (any(x$desig_known)) "designations" else "properties",
                    a_cm = a$ah_cm + a$ap_cm),
               if (is.na(passed)) c("designation", "munsell_value_moist", "oc_pct")
               else character(0),
               "Ch 3, p. 36; Ch 11, p. 119")
}
