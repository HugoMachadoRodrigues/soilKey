# =============================================================================
# v0.9.269 -- subgroups of the Canadian System of Soil Classification, 3rd
# edition (Key to Soil Subgroups, Ch 3, pp. 37-51): the Cryosolic, Organic and
# Vertisolic orders (v0.9.269); the Podzolic, Gleysolic, Brunisolic and
# Regosolic orders (v0.9.270). Same conventions as R/diagnostics-cssc.R and
# R/diagnostics-cssc-great-groups.R: measured properties first, designations
# in the Canadian notation second, NA when neither decides; readings in
# inst/rules/cssc3/readings.yaml. Depths of organic soils are measured from
# the surface (the top of the uppermost layer), those of mineral soils from
# the mineral surface, as the key words them.
# =============================================================================


# ---- shared readings ---------------------------------------------------------

# Combine the tests of a smallest and a largest reading: decided when both
# give the same answer.
.cssc_lo_hi <- function(lo, hi) if (identical(lo, hi)) lo else NA

# A thickness test that knows a certain thickness (lo) and a possible one
# (hi): passes above `min_cm` (or at it when strict = FALSE) with certainty,
# fails when even the possible thickness does not reach it.
.cssc_thick_test <- function(lo, hi, min_cm, strict = TRUE) {
  if (is.na(lo) || is.na(hi)) return(NA)
  meets <- function(v) if (strict) v > min_cm else v >= min_cm
  if (meets(lo)) TRUE else if (!meets(hi)) FALSE else NA
}

# B horizons by designation: a B master in the Canadian notation, or a B
# master in another notation (a transition such as BC is not a B). NA where
# a horizon has no designation read.
.cssc_b_rows <- function(x) {
  b <- .cssc_is(x$parsed, "B", weak = "any")
  raw <- .cssc_raw_master(x)
  ifelse(!is.na(b), b, ifelse(!is.na(raw), raw == "B", NA))
}

# Thickness of the Bm horizons (B with m, strong or weak, any other suffix),
# certain and possible: in another notation m means cementation, so a B there
# may or may not be a Bm, and an undesignated mineral horizon may be one.
.cssc_bm_cm <- function(x) {
  bm <- .cssc_is(x$parsed, "B", "m", "any")
  raw <- .cssc_raw_master(x)
  bm <- ifelse(!is.na(bm), bm, ifelse(!is.na(raw), ifelse(raw == "B", NA, FALSE), NA))
  bm[!x$mineral] <- FALSE
  lo <- sum(x$thick[bm %in% TRUE])
  list(lo = lo, hi = lo + sum(x$thick[is.na(bm)]), rows = which(bm %in% TRUE))
}

# Three-valued "some row passes" over rows that are certainly of a kind
# (`sure`) and rows that may be (`maybe`): TRUE when a sure row passes, FALSE
# when no sure or maybe row can.
.cssc_some <- function(pass, sure, maybe) {
  if (any(pass[sure] %in% TRUE)) return(TRUE)
  if (anyNA(pass[sure]) || any(!(pass[maybe] %in% FALSE))) return(NA)
  FALSE
}


# ---- A. Cryosolic subgroups (pp. 37-38; Ch 6) -------------------------------

#' Glacic layer (CSSC subgroup AAA)
#'
#' An ice layer more than 30 cm thick whose top lies within 1 m of the
#' surface. A layer is ice by measured ice content (more than 95 % by volume,
#' Ch 6, p. 79), otherwise by a W designation with a z suffix or a frozen
#' temperature; a W layer with no sign of being frozen is undecided.
#' @noRd
glacic_layer_cssc <- function(pedon, min_cm = 30, max_top_cm = 100) {
  oc <- .cssc_organic_column(pedon); h <- oc$h
  frozen <- .cssc_has_suffix(oc$parsed, "z", weak = "any") %in% TRUE |
            (!is.na(h$permafrost_temp_C) & h$permafrost_temp_C <= 0)
  ice <- ifelse(!is.na(h$ice_pct), h$ice_pct > 95,
                ifelse(oc$water, ifelse(frozen, TRUE, NA), FALSE))
  rel_top <- h$top_cm - oc$top0
  ord <- oc$ord[!is.na(h$top_cm[oc$ord])]
  run_test <- function(ok) {
    best <- FALSE
    k <- 1L
    while (k <= length(ord)) {
      if (!ok[ord[k]]) { k <- k + 1L; next }
      j <- k
      while (j < length(ord) && ok[ord[j + 1L]] &&
             isTRUE(abs(h$top_cm[ord[j + 1L]] - h$bottom_cm[ord[j]]) < 0.01)) j <- j + 1L
      run <- ord[k:j]
      th <- sum(oc$thick[run])
      if (rel_top[run[1L]] <= max_top_cm) {
        r <- if (is.na(th)) NA else th > min_cm
        if (isTRUE(r)) return(TRUE)
        if (is.na(r)) best <- NA
      }
      k <- j + 1L
    }
    best
  }
  passed <- .cssc_lo_hi(run_test(ice %in% TRUE), run_test(!(ice %in% FALSE)))
  .cssc_result("glacic_layer_cssc", passed, which(ice %in% TRUE),
               list(route = "properties+designations"),
               if (is.na(passed)) c("ice_pct", "designation") else character(0),
               "Ch 3, p. 37; Ch 6, p. 79")
}

# The mineral contact under the organic column: TRUE when the column ends on
# a mineral layer or rock within `max_cm` of the surface, FALSE when it ends
# deeper, on water or ice, or reaches past `max_cm`; NA when the record ends
# within `max_cm` or a layer of unknown organic status could move it.
.cssc_mineral_contact <- function(oc, max_cm = 100) {
  h <- oc$h
  at <- function(base) {
    if (base - oc$top0 > max_cm) return(FALSE)
    nxt <- oc$ord[which(abs(h$top_cm[oc$ord] - base) < 0.01)][1L]
    if (is.na(nxt)) return(NA)
    if (oc$water[nxt]) return(FALSE)
    if (oc$rock[nxt] || identical(oc$organic[nxt], FALSE)) return(TRUE)
    NA
  }
  .cssc_lo_hi(at(oc$base), at(oc$base_max))
}

#' Terric Organic Cryosols (CSSC subgroups AAB, AAC, AAD)
#'
#' A mineral contact within 1 m of the surface, with mainly fibric, mesic or
#' humic organic material above it ("mainly" read as for the organic tiers:
#' more than half with two materials, the thickest with three). Rock counts
#' as a mineral contact.
#' @noRd
terric_organic_cryosol_cssc <- function(pedon, material = c("fibric", "mesic", "humic")) {
  material <- match.arg(material)
  oc <- .cssc_organic_column(pedon)
  contact <- .cssc_mineral_contact(oc, 100)
  # the material above the contact, layers of unknown organic status that may
  # belong to the organic column included
  t <- .cssc_kind_tally(oc, 0, min(oc$base_max - oc$top0, 100))
  mainly <- if (!is.na(t$dominant)) t$dominant == material
            else if (isTRUE(t$decided) || isTRUE(t$empty)) FALSE
            else NA
  passed <- .and3(contact, mainly)
  .cssc_result("terric_organic_cryosol_cssc", passed, integer(0),
               list(route = "properties+designations", mineral_contact = contact,
                    dominant = t$dominant, thickness_cm = t$thickness),
               if (is.na(passed)) c("oc_pct", "fiber_content_rubbed_pct", "designation")
               else character(0),
               "Ch 3, p. 37; Ch 6, p. 79")
}

#' Fibric, Mesic and Humic Organic Cryosols (CSSC subgroups AAE, AAF, AAG)
#'
#' Organic material dominantly fibric, mesic or humic below 40 cm, within the
#' control section of Organic Cryosols (to 1 m or a lithic contact, Ch 2,
#' p. 11). No organic material below 40 cm fails all three.
#' @noRd
organic_below_40_cssc <- function(pedon, material = c("fibric", "mesic", "humic")) {
  material <- match.arg(material)
  oc <- .cssc_organic_column(pedon); h <- oc$h
  rock_top <- suppressWarnings(min((h$top_cm - oc$top0)[oc$rock], na.rm = TRUE))
  to <- min(100, rock_top)
  t <- .cssc_kind_tally(oc, 40, to)
  passed <- if (!is.na(t$dominant)) t$dominant == material
            else if (isTRUE(t$decided) || isTRUE(t$empty)) FALSE
            else NA
  .cssc_result("organic_below_40_cssc", passed, integer(0),
               list(route = "properties+designations", dominant = t$dominant,
                    thickness_cm = t$thickness, unknown_cm = t$unknown_cm, window_cm = c(40, to)),
               if (is.na(passed)) c("fiber_content_rubbed_pct", "designation") else character(0),
               "Ch 3, p. 37; Ch 6, p. 79")
}

#' Gleysolic Cryosols (CSSC subgroups ABA, ACA)
#'
#' A gleyed layer similar to soils of the Gleysolic order, read as the
#' uppermost mineral horizon within 50 cm of the mineral surface that is not
#' an A, gleyed by the colours of the Gleysolic order test or, failing them,
#' by a g in its own designation; as Ch 6 describes gleying up to the mineral
#' surface (pp. 76 and 78). An A is an A master in the Canadian notation or
#' an A or E in another one; an undesignated horizon is an A when the
#' surface-A reading of the great groups (.cssc_surface_a) says so, unless
#' its own colours are gleyed (Ch 6: the uppermost mineral horizon of these
#' soils is a Bg or Cg). A gleyed horizon lower down only, which Ch 6 says
#' the other subgroups usually have just above the permafrost table, does not
#' qualify.
#' @noRd
gleysolic_cryosol_cssc <- function(pedon) {
  gr <- .cssc_gley_rows(pedon, 50)
  x <- gr$x; h <- x$h
  other <- .cssc_other_masters(h$designation %||% rep(NA_character_, nrow(h)), x$notation$ok)
  desig_a <- (.cssc_is(x$parsed, "A", weak = "any") %in% TRUE) | (other %in% c("A", "E"))
  undes <- !x$desig_known & is.na(other)
  win <- which(x$mineral & !is.na(x$top_ms) & x$top_ms >= -0.01 & x$top_ms <= 50)
  win <- win[order(h$top_cm[win])]
  a <- .cssc_surface_a(x)
  first_after <- function(a_rows) {
    skip <- desig_a | (undes & seq_len(nrow(h)) %in% a_rows & !(gr$gley %in% TRUE))
    c1 <- win[!skip[win]]
    if (length(c1) == 0L) FALSE else gr$row[c1[1L]]
  }
  res <- if (isTRUE(a$undecidable)) list(NA)
         else lapply(a$prefixes, function(pf) first_after(pf$rows))
  passed <- if (all(vapply(res, identical, logical(1), res[[1L]]))) res[[1L]] else NA
  .cssc_result("gleysolic_cryosol_cssc", passed, integer(0),
               list(route = "properties+designations"),
               if (is.na(passed)) c("munsell_chroma_moist", "mottle_contrast", "designation")
               else character(0),
               "Ch 3, pp. 37-38; Ch 6, pp. 76 and 78")
}

#' Histic Cryosols (CSSC subgroups ABB-ABD, ACB-ACD)
#'
#' Organic horizons in the upper 1 m, more than 15 cm thick together (Ch 6:
#' surface and subsurface organic horizons count together).
#' @noRd
histic_cryosol_cssc <- function(pedon, min_cm = 15, within_cm = 100) {
  oc <- .cssc_organic_column(pedon); h <- oc$h
  rel_top <- h$top_cm - oc$top0
  overlap <- pmax(0, pmin(h$bottom_cm - oc$top0, within_cm) - pmax(rel_top, 0))
  open <- ifelse(is.na(overlap) & !is.na(rel_top) & rel_top < within_cm,
                 within_cm - pmax(rel_top, 0), 0)
  overlap[is.na(overlap)] <- 0
  lo <- sum(overlap[oc$organic %in% TRUE])
  hi <- lo + sum(overlap[is.na(oc$organic) & !oc$rock & !oc$water]) +
        sum(open[!(oc$organic %in% FALSE) & !oc$rock & !oc$water])
  passed <- .cssc_thick_test(lo, hi, min_cm)
  .cssc_result("histic_cryosol_cssc", passed, which(oc$organic %in% TRUE),
               list(route = "properties+designations", organic_cm = lo, organic_cm_max = hi),
               if (is.na(passed)) c("oc_pct", "designation") else character(0),
               "Ch 3, pp. 37-38; Ch 6, pp. 76-78")
}

#' pH of the B horizons (CSSC Cryosolic subgroups)
#'
#' high = TRUE: pH (0.01 M CaCl2) 5.5 or more in some or all of the B
#' horizons; high = FALSE: below 5.5 in some or all of them (keyed after the
#' first, so below 5.5 throughout). Without pH in CaCl2, pH in water stands in
#' as for the Brunisolic great groups. No B horizon fails both.
#' @noRd
b_ph_cssc <- function(pedon, high = TRUE) {
  x <- .cssc_horizons(pedon)
  b <- .cssc_b_rows(x) & x$mineral
  hi <- .cssc_ph_high(x$h, seq_len(nrow(x$h)))
  pass <- if (high) hi else !hi
  passed <- .cssc_some(pass, which(b %in% TRUE), which(is.na(b)))
  .cssc_result("b_ph_cssc", passed, which(b %in% TRUE & pass %in% TRUE),
               list(route = "properties+designations", high = high),
               if (is.na(passed)) c("ph_cacl2", "designation") else character(0),
               "Ch 3, pp. 37-38; Ch 6, pp. 74-78")
}

#' No B horizon (CSSC Histic Regosolic Cryosols, ABD and ACD)
#' @noRd
no_b_horizon_cssc <- function(pedon) {
  x <- .cssc_horizons(pedon)
  b <- .cssc_b_rows(x) & x$mineral
  passed <- if (any(b %in% TRUE)) FALSE else if (anyNA(b)) NA else TRUE
  .cssc_result("no_b_horizon_cssc", passed, which(b %in% TRUE),
               list(route = "designations"),
               if (is.na(passed)) "designation" else character(0),
               "Ch 3, pp. 37-38")
}

#' Bm horizon thickness (CSSC Brunisolic and Orthic Cryosols)
#'
#' thick = TRUE: Bm horizons (Bm, Bmy, Bmk...) 10 cm thick or more in all
#' (ABE, ABF, ACF, ACG). thick = FALSE: a B horizon, with Bm horizons thinner
#' than 10 cm in all (ABG, ABH, ACH, ACI); a soil with no B horizon at all is
#' left to the Regosolic subgroup, as Ch 6 describes it.
#' @noRd
bm_horizon_cssc <- function(pedon, thick = TRUE, min_cm = 10) {
  x <- .cssc_horizons(pedon)
  bm <- .cssc_bm_cm(x)
  thick_ok <- .cssc_thick_test(bm$lo, bm$hi, min_cm, strict = FALSE)
  passed <- if (thick) thick_ok else {
    b <- .cssc_b_rows(x) & x$mineral
    .and3(.any3(b), if (is.na(thick_ok)) NA else !thick_ok)
  }
  .cssc_result("bm_horizon_cssc", passed, bm$rows,
               list(route = "designations", bm_cm = bm$lo, bm_cm_max = bm$hi, thick = thick),
               if (is.na(passed)) "designation" else character(0),
               "Ch 3, pp. 37-38; Ch 6, pp. 74-77")
}

#' Luvisolic Static Cryosol (CSSC subgroup ACE)
#'
#' An eluvial horizon (an A with e: Ae, Aej, Ahe, Aeg; an E in another
#' notation) and a Bt horizon (bt_horizon_cssc) at least 10 cm thick. The key
#' writes Bty; the y is not required, as Ch 6 (p. 78) writes Bt.
#' @noRd
luvisolic_cryosol_cssc <- function(pedon, min_cm = 10) {
  x <- .cssc_horizons(pedon)
  e_can <- .cssc_is(x$parsed, "A", "e", "any")
  raw <- .cssc_raw_master(x)
  elu <- ifelse(!is.na(e_can), e_can, ifelse(!is.na(raw), raw == "E", NA))
  has_e <- .any3(elu)
  bt <- bt_horizon_cssc(pedon)
  bt_ok <- if (identical(bt$passed, FALSE)) FALSE
           else if (isTRUE(bt$passed)) {
             th <- sum(x$thick[bt$layers]); if (is.na(th)) NA else th >= min_cm
           } else NA
  passed <- .and3(has_e, bt_ok)
  .cssc_result("luvisolic_cryosol_cssc", passed, bt$layers,
               list(route = "properties+designations", eluvial = has_e, bt = bt_ok),
               if (is.na(passed)) c("designation", "clay_pct") else character(0),
               "Ch 3, p. 38; Ch 6, pp. 77-78")
}


# ---- B. Organic subgroups (pp. 38-40; Ch 9) ---------------------------------

# Master of each layer in the Canadian notation ("F", "H", "O", "L", "LFH"
# ...), NA where none is read.
.cssc_masters <- function(parsed) {
  vapply(parsed, function(comps) {
    if (length(comps) == 0L) NA_character_ else comps[[1L]]$master
  }, character(1))
}

#' Histic Folisol (CSSC subgroup BAA)
#'
#' An O horizon more than 10 cm thick below the F or H horizons (adjacent O
#' horizons added up). Read from the designations.
#' @noRd
histic_folisol_cssc <- function(pedon, min_cm = 10) {
  oc <- .cssc_organic_column(pedon); h <- oc$h
  m <- .cssc_masters(oc$parsed)
  if (!any(oc$desig)) {
    return(.cssc_result("histic_folisol_cssc", NA, integer(0), list(route = "designations"),
                        "designation", "Ch 3, p. 38; Ch 9, p. 105"))
  }
  fh <- m %in% c("F", "H", "FH", "LFH", "LF")
  first_fh <- suppressWarnings(min(h$top_cm[fh], na.rm = TRUE))
  o_below <- (m %in% "O") & !is.na(h$top_cm) & h$top_cm >= first_fh
  ord <- oc$ord[!is.na(h$top_cm[oc$ord])]
  r_lo <- .cssc_runs(o_below[ord], oc$thick[ord])
  maybe <- o_below | (!oc$desig & oc$organic %in% TRUE & !is.na(h$top_cm) & h$top_cm >= first_fh)
  r_hi <- .cssc_runs(maybe[ord], oc$thick[ord])
  passed <- .cssc_thick_test(r_lo$thickness, r_hi$thickness, min_cm)
  .cssc_result("histic_folisol_cssc", passed, ord[r_lo$rows],
               list(route = "designations", o_cm = r_lo$thickness),
               if (is.na(passed)) "designation" else character(0),
               "Ch 3, p. 38; Ch 9, p. 105")
}

#' Lignic Folisol (CSSC subgroup BAB)
#'
#' F or H horizons composed primarily of woody material, read as a
#' thickness-weighted woody_fragments_pct above 30 % over the folic horizons
#' other than an L (F, H, and combined LF, FH, LFH; Ch 9, p. 105, gives "more
#' than 30 %" of the surface area). Woody material that is not recorded is
#' read as absent and listed as missing.
#' @noRd
lignic_folisol_cssc <- function(pedon, min_pct = 30) {
  oc <- .cssc_organic_column(pedon); h <- oc$h
  m <- .cssc_masters(oc$parsed)
  rows <- which(oc$folic & !(m %in% "L"))
  x <- list(h = h, thick = oc$thick)
  w <- .cssc_wmean(x, "woody_fragments_pct", rows)
  passed <- if (length(rows) == 0L) { if (any(oc$desig)) FALSE else NA }
            else if (is.na(w)) FALSE else w > min_pct
  .cssc_result("lignic_folisol_cssc", passed, if (isTRUE(passed)) rows else integer(0),
               list(route = "properties+designations", woody_pct = w),
               if (length(rows) && is.na(w)) "woody_fragments_pct"
               else if (is.na(passed)) "designation" else character(0),
               "Ch 3, p. 38; Ch 9, p. 105")
}

#' Hemic and Humic Folisols (CSSC subgroups BAC, BAD)
#'
#' The organic column composed primarily of F horizon (Hemic) or of H horizon
#' (Humic), read as the dominant of the F, H and O horizons by thickness
#' (more than half with two, the thickest with three); L horizons are parent
#' material and not counted, and combined horizons (LF, FH, LFH) or
#' undesignated organic layers are of unknown kind.
#' @noRd
folisol_horizon_cssc <- function(pedon, horizon = c("F", "H")) {
  horizon <- match.arg(horizon)
  oc <- .cssc_organic_column(pedon); h <- oc$h
  m <- .cssc_masters(oc$parsed)
  col <- !is.na(h$top_cm) & h$top_cm < oc$base - 0.01
  tot <- c(F = sum(oc$thick[col & m %in% "F"]), H = sum(oc$thick[col & m %in% "H"]),
           O = sum(oc$thick[col & m %in% "O"]))
  unknown <- sum(oc$thick[col & !(m %in% c("F", "H", "O", "L"))])
  if (anyNA(tot) || is.na(unknown)) {
    passed <- NA
  } else {
    dom <- .cssc_pick_dominant(tot)
    if (unknown > 0) {
      alts <- vapply(names(tot), function(k) {
        t2 <- tot; t2[[k]] <- t2[[k]] + unknown; .cssc_pick_dominant(t2)
      }, "")
      if (!all(alts %in% dom)) dom <- NA_character_
      passed <- if (is.na(dom)) { if (all(!alts %in% horizon)) FALSE else NA }
                else dom == horizon
    } else {
      passed <- if (is.na(dom)) FALSE else dom == horizon
    }
  }
  .cssc_result("folisol_horizon_cssc", passed, which(col & m %in% horizon),
               list(route = "designations", thickness_cm = tot, unknown_cm = unknown),
               if (is.na(passed)) "designation" else character(0),
               "Ch 3, p. 38; Ch 9, pp. 104-105")
}

#' Hydric layer (CSSC subgroups BBA, BCA, BDA)
#'
#' Water from 40 cm or deeper reaching below 160 cm (Ch 2, p. 19), read as
#' for the organic tiers (.cssc_organic_contacts).
#' @noRd
hydric_layer_cssc <- function(pedon) {
  oc <- .cssc_organic_column(pedon); h <- oc$h
  rel_top <- h$top_cm - oc$top0
  contact <- .cssc_organic_contacts(oc)
  w <- which(oc$water & !is.na(rel_top) & rel_top >= 40)
  passed <- if (length(w) == 0L) FALSE else .any3(contact[w])
  .cssc_result("hydric_layer_cssc", passed, w[contact[w] %in% TRUE],
               list(route = "designations"),
               if (is.na(passed)) c("designation", "bottom_cm") else character(0),
               "Ch 3, pp. 38-39; Ch 2, p. 19")
}

# Mineral layers of an Organic soil (not organic, rock, water or limnic):
# each run of adjacent ones, with its thickness between `from` and `to` below
# the surface (cm_lo certain; cm_hi if a layer with no bottom depth reaches
# the end of the window), whether it is the deepest layer recorded and ends
# above `to` (open: it may go on), and whether organic matter lies below it
# within `to` (underlain). Runs of known mineral layers (sure) and of layers
# that may be mineral (maybe, with layers of unknown organic status).
# Cumulic layers are the runs with organic matter below them; a run with
# none is a substratum (Ch 2, p. 19).
.cssc_mineral_in_window <- function(oc, from = 40, to = 160) {
  h <- oc$h
  rel_top <- h$top_cm - oc$top0; rel_bot <- h$bottom_cm - oc$top0
  clip <- pmax(0, pmin(rel_bot, to) - pmax(rel_top, from))
  clip_lo <- ifelse(is.na(clip), 0, clip)
  clip_hi <- ifelse(is.na(clip) & !is.na(rel_top) & rel_top < to,
                    to - pmax(rel_top, from), clip_lo)
  sure <- oc$organic %in% FALSE & !oc$rock & !oc$water & !oc$limnic
  maybe <- sure | (is.na(oc$organic) & !oc$rock & !oc$water & !oc$limnic)
  ord <- oc$ord[!is.na(h$top_cm[oc$ord])]
  organic_below <- function(pos, accept_unknown) {
    later <- ord[seq_along(ord) > pos]
    later <- later[!is.na(rel_top[later]) & rel_top[later] < to]
    any(oc$organic[later] %in% TRUE) ||
      (accept_unknown && any(is.na(oc$organic[later]) & !oc$rock[later] & !oc$water[later]))
  }
  runs <- function(ok, accept_unknown) {
    out <- list(); k <- 1L
    while (k <= length(ord)) {
      if (!ok[ord[k]]) { k <- k + 1L; next }
      j <- k
      while (j < length(ord) && ok[ord[j + 1L]] &&
             isTRUE(abs(h$top_cm[ord[j + 1L]] - h$bottom_cm[ord[j]]) < 0.01)) j <- j + 1L
      run <- ord[k:j]
      out[[length(out) + 1L]] <- list(
        rows = run, cm_lo = sum(clip_lo[run]), cm_hi = sum(clip_hi[run]),
        open = j == length(ord) && !isTRUE(rel_bot[ord[j]] >= to),
        underlain = organic_below(j, accept_unknown))
      k <- j + 1L
    }
    out
  }
  sure_runs <- runs(sure, FALSE); maybe_runs <- runs(maybe, TRUE)
  # cumulic layers, layer by layer: a known mineral layer with organic matter
  # certainly below it counts for sure; a layer that may be mineral, with a
  # layer below it that may be organic, may count
  pos <- match(seq_len(nrow(h)), ord)
  below_sure <- vapply(seq_len(nrow(h)), function(i)
    !is.na(pos[i]) && organic_below(pos[i], FALSE), logical(1))
  below_maybe <- vapply(seq_len(nrow(h)), function(i)
    !is.na(pos[i]) && organic_below(pos[i], TRUE), logical(1))
  list(sure = sure_runs, maybe = maybe_runs,
       cumulic_lo = sum(clip_lo[sure & below_sure]),
       cumulic_hi = sum(clip_hi[maybe & below_maybe]),
       cumulic_rows = which(sure & below_sure))
}

#' Terric layer (CSSC subgroups BBB-BBD, BCB-BCD, BDB-BDD)
#'
#' A terric layer at least 30 cm thick beneath the surface tier: a run of
#' adjacent mineral layers with at least 30 cm between 40 and 160 cm. A run
#' that is the deepest layer recorded and ends above 160 cm may go on, and is
#' undecided when it is thinner.
#' @noRd
terric_layer_cssc <- function(pedon, min_cm = 30) {
  oc <- .cssc_organic_column(pedon)
  w <- .cssc_mineral_in_window(oc, 40, 160)
  best <- function(runs, field) {
    if (length(runs) == 0L) return(FALSE)
    r <- vapply(runs, function(rn) {
      if (rn[[field]] >= min_cm) TRUE else if (rn$open) NA else FALSE
    }, logical(1))
    .any3(r)
  }
  lo <- best(w$sure, "cm_lo"); hi <- best(w$maybe, "cm_hi")
  passed <- if (isTRUE(lo)) TRUE else if (identical(hi, FALSE)) FALSE else NA
  .cssc_result("terric_layer_cssc", passed, unlist(lapply(w$sure, `[[`, "rows")),
               list(route = "properties+designations"),
               if (is.na(passed)) c("oc_pct", "designation", "bottom_cm") else character(0),
               "Ch 3, pp. 38-40; Ch 2, p. 19")
}

#' Limnic layer (CSSC subgroups BBE, BCE, BDE)
#'
#' Limnic layers (coprogenous earth, diatomaceous earth, marl) more than 5 cm
#' thick in all beneath the surface tier (40-160 cm). Limnic material is read
#' from an Oco designation or a limnic origin (.cssc_organic_column); without
#' either, no limnic layer is read.
#' @noRd
limnic_layer_cssc <- function(pedon, min_cm = 5) {
  oc <- .cssc_organic_column(pedon); h <- oc$h
  rel_top <- h$top_cm - oc$top0
  clip <- pmax(0, pmin(h$bottom_cm - oc$top0, 160) - pmax(rel_top, 40))
  lo <- sum(ifelse(is.na(clip), 0, clip)[oc$limnic])
  hi <- sum(ifelse(is.na(clip) & !is.na(rel_top) & rel_top < 160,
                   160 - pmax(rel_top, 40), ifelse(is.na(clip), 0, clip))[oc$limnic])
  cm <- lo
  passed <- .cssc_thick_test(lo, hi, min_cm)
  .cssc_result("limnic_layer_cssc", passed, which(oc$limnic),
               list(route = "properties+designations", limnic_cm = cm),
               if (is.na(passed)) "bottom_cm" else character(0),
               "Ch 3, pp. 38-40; Ch 2, p. 19")
}

#' Cumulic layers (CSSC subgroups BBF, BCF, BDF)
#'
#' Mineral layers with organic matter below them, more than 5 cm thick in all
#' beneath the surface tier (40-160 cm). A mineral layer at the base of the
#' profile, with no organic matter below it, is a substratum and not a
#' cumulic layer (Ch 2, p. 19; Ch 9, p. 100).
#' @noRd
cumulic_layers_cssc <- function(pedon, min_cm = 5) {
  oc <- .cssc_organic_column(pedon)
  w <- .cssc_mineral_in_window(oc, 40, 160)
  passed <- .cssc_thick_test(w$cumulic_lo, w$cumulic_hi, min_cm)
  .cssc_result("cumulic_layers_cssc", passed, w$cumulic_rows,
               list(route = "properties+designations", cumulic_cm = w$cumulic_lo),
               if (is.na(passed)) c("oc_pct", "designation") else character(0),
               "Ch 3, pp. 38-40; Ch 2, p. 19")
}

#' Fibric, mesic or humic layers (CSSC Organic subgroups)
#'
#' Layers of one kind of peat more than `min_cm` thick in all, in the control
#' section (from = 0) or in the middle and bottom tiers (from = 40), to
#' 160 cm. The kind is read as for the organic tiers (.cssc_organic_kind);
#' layers of unknown kind or organic status may be of that kind.
#' @noRd
organic_layer_cssc <- function(pedon, material = c("fibric", "mesic", "humic"),
                               min_cm, from = 40) {
  material <- match.arg(material)
  oc <- .cssc_organic_column(pedon)
  t <- .cssc_kind_tally(oc, from, 160)
  lo <- t$thickness[[material]]
  passed <- .cssc_thick_test(lo, lo + t$open_cm[[material]] + t$unknown_cm, min_cm)
  .cssc_result("organic_layer_cssc", passed, integer(0),
               list(route = "properties+designations", material = material,
                    thickness_cm = t$thickness, unknown_cm = t$unknown_cm, window_cm = c(from, 160)),
               if (is.na(passed)) c("fiber_content_rubbed_pct", "designation") else character(0),
               "Ch 3, pp. 38-40; Ch 9, pp. 100-103")
}


# ---- C. Vertisolic subgroups (p. 40; Ch 13) ---------------------------------

# Mottles of the given contrasts in the mineral horizons of a depth window
# below the mineral surface, horizon by horizon: by mottle_contrast (mottles
# recorded as absent, by redoximorphic_features_pct of 0, rule a horizon
# out); otherwise by the horizon's own Canadian designation, a strong g
# standing for prominent mottles and a weak g (gj) for faint to distinct
# ones (Ch 2, p. 14; the 2016 key reads the Gleyed Podzols as a gj horizon);
# otherwise undecided. The window holds the horizons whose top lies from
# `from` to `to` cm (both included) or, with overlap = TRUE, the horizons
# that reach into it.
.cssc_mottles <- function(x, contrast, from = 0, to = 50, overlap = FALSE) {
  h <- x$h
  win <- if (overlap) x$mineral & !is.na(x$top_ms) & x$top_ms < to &
                      !((x$bottom_ms <= from) %in% TRUE)
         else x$mineral & !is.na(x$top_ms) & x$top_ms >= from - 0.01 & x$top_ms <= to
  ct <- tolower(trimws(as.character(h$mottle_contrast)))
  absent <- !is.na(h$redoximorphic_features_pct) & h$redoximorphic_features_pct == 0
  other <- setdiff(c("faint", "distinct", "prominent"), contrast)
  prop <- ifelse(ct %in% contrast, TRUE, ifelse(ct %in% other | absent, FALSE, NA))
  g_strong <- .cssc_has_suffix(x$parsed, "g", weak = "no") %in% TRUE
  g_weak <- .cssc_has_suffix(x$parsed, "g", weak = "yes") %in% TRUE
  des <- (g_strong & "prominent" %in% contrast) |
         (g_weak & any(c("faint", "distinct") %in% contrast))
  row <- ifelse(!is.na(prop), prop, ifelse(x$desig_known, des, NA))
  passed <- if (any(win)) .any3(row[win]) else FALSE
  route <- if (isTRUE(passed)) {
             if (any(win & prop %in% TRUE)) "properties" else "designations"
           } else if (identical(passed, FALSE) && all(!is.na(prop[win]))) "properties"
           else "designations"
  list(passed = passed, route = route, rows = which(win & row %in% TRUE))
}

#' Mottles within 50 cm of the mineral surface (CSSC subgroups CAA, CAB,
#' CBA, CBB)
#'
#' contrast = "prominent" (Gleysolic subgroups) or "faint_distinct" (Gleyed
#' subgroups), in the mineral horizons whose top is within 50 cm of the
#' mineral surface (50 cm included, as for the gley colours), read horizon by
#' horizon (.cssc_mottles).
#' @noRd
vertisolic_mottles_cssc <- function(pedon, contrast = c("prominent", "faint_distinct"),
                                    max_cm = 50) {
  contrast <- match.arg(contrast)
  x <- .cssc_horizons(pedon)
  set <- if (contrast == "prominent") "prominent" else c("faint", "distinct")
  m <- .cssc_mottles(x, set, 0, max_cm)
  .cssc_result("vertisolic_mottles_cssc", m$passed, m$rows,
               list(route = m$route, contrast = contrast),
               if (is.na(m$passed)) c("mottle_contrast", "designation") else character(0),
               "Ch 3, p. 40; Ch 13, pp. 133-134")
}


# ---- shared readings of the mineral orders -----------------------------------

# Suffix letters of designations written in another notation (the lowercase
# letters after the master), NA where there is none or the designations are
# read as Canadian.
.cssc_other_suffix <- function(x) {
  d <- trimws(as.character(x$h$designation %||% rep(NA_character_, nrow(x$h))))
  if (isTRUE(x$notation$ok)) return(rep(NA_character_, length(d)))
  out <- gsub("[^a-z]", "", sub("^[0-9]*[A-Z]+", "", sub("/.*$", "", d)))
  out[is.na(d) | is.na(.cssc_raw_master(x))] <- NA_character_
  out
}

# Depth of the control section of mineral soils below the mineral surface
# (Ch 2, p. 11): 25 cm below the top of the C (or the IIC), or 2 m, whichever
# is less; 1 m when the C begins within 75 cm; down to a lithic contact
# between 10 cm and 1 m. The top of the C may be uncertain: an undesignated
# mineral horizon above the first designated C may be the C, and in a profile
# with no C designated the C begins at or below the deepest horizon
# described. The result is the shortest and the longest section the profile
# allows.
.cssc_control_section_range <- function(x) {
  f <- function(ctop) if (ctop < 75) 100 else min(ctop + 25, 200)
  rock <- (.cssc_is(x$parsed, "R") %in% TRUE) | (.cssc_raw_master(x) %in% "R")
  rtop <- suppressWarnings(min(x$top_ms[rock], na.rm = TRUE))
  if (is.finite(rtop) && rtop >= 10 && rtop < 100) return(c(rtop, rtop))
  is_c <- (.cssc_is(x$parsed, "C", weak = "any") %in% TRUE) | (.cssc_raw_master(x) %in% "C")
  is_c <- is_c & x$mineral
  undes <- x$mineral & !x$desig_known & is.na(.cssc_raw_master(x))
  ctop_d <- suppressWarnings(min(x$top_ms[is_c], na.rm = TRUE))
  c_top_unknown <- any(is_c & is.na(x$top_ms))
  cand <- x$top_ms[undes & !is.na(x$top_ms) & (if (is.finite(ctop_d)) x$top_ms < ctop_d else TRUE)]
  if (is.finite(ctop_d)) {
    lo <- f(min(c(ctop_d, cand)))
    hi <- if (c_top_unknown) 200 else f(ctop_d)
  } else {
    deepest <- suppressWarnings(max(x$bottom_ms[x$mineral], na.rm = TRUE))
    starts <- c(cand, if (is.finite(deepest)) deepest)
    lo <- if (length(starts)) f(min(starts)) else 100
    hi <- 200
  }
  c(lo, max(lo, hi))
}

.cssc_control_section <- function(x) {
  r <- .cssc_control_section_range(x)
  if (r[1L] == r[2L]) r[1L] else NA_real_
}

# Whether each horizon begins within the control section: TRUE above the
# shortest section the profile allows, FALSE below the longest, NA between.
.cssc_in_control_section <- function(x) {
  r <- .cssc_control_section_range(x)
  ifelse(is.na(x$top_ms), NA, ifelse(x$top_ms < r[1L], TRUE, ifelse(x$top_ms >= r[2L], FALSE, NA)))
}

# Strongly cemented horizons, horizon by horizon: by cementation_class when
# recorded (strongly or very strongly cemented, or indurated; weakly,
# moderately or not cemented fail),
# otherwise by a c in the Canadian notation (Ch 2, p. 12; a weak cj is not
# strongly cemented: Ch 10, p. 116 writes the ortstein Bfcgj over a Bfjcjgj)
# or an m in another notation, otherwise NA.
.cssc_cemented <- function(x) {
  cc <- tolower(trimws(as.character(x$h$cementation_class)))
  # weak words first: "extremely weakly cemented" is the weakest class
  prop <- ifelse(grepl("weak|moder|^none|^non|^not|^un", cc), FALSE,
          ifelse(grepl("strong|indurat", cc), TRUE, NA))
  can <- .cssc_has_suffix(x$parsed, "c", weak = "no")
  osfx <- .cssc_other_suffix(x)
  des <- ifelse(!is.na(can), can, ifelse(!is.na(osfx), grepl("m", osfx), NA))
  ifelse(!is.na(prop), prop, des)
}

# Podzolic B material, horizon by horizon: by the podzolic B chemistry and
# colour of each layer (.cssc_podzolic_layers) when they decide, otherwise by
# a strong h, hf or f in a Canadian B designation (a Bs or Bh in another
# notation), otherwise NA.
.cssc_podzolic_kind <- function(pedon, x) {
  pl <- .cssc_podzolic_layers(x)
  osfx <- .cssc_other_suffix(x)
  raw <- .cssc_raw_master(x)
  oth <- ifelse(!is.na(raw), raw == "B" & !is.na(osfx) & grepl("[sh]", gsub("ss", "", osfx)), NA)
  des <- ifelse(!is.na(pl$d_any), pl$d_any, oth)
  out <- ifelse(!is.na(pl$any), pl$any, des)
  out[!x$mineral] <- FALSE   # organic layers are not podzolic B material
  out
}

# Elementwise three-valued and / or.
.cssc_and3v <- function(a, b) ifelse(a %in% FALSE | b %in% FALSE, FALSE,
                                ifelse(a %in% TRUE & b %in% TRUE, TRUE, NA))
.cssc_or3v <- function(a, b) ifelse(a %in% TRUE | b %in% TRUE, TRUE,
                               ifelse(a %in% FALSE & b %in% FALSE, FALSE, NA))

# Horizons of fragipan character: an x in a designation (Canadian or other
# notation), NA where a horizon has no designation read.
.cssc_fragic_rows <- function(x) {
  can <- .cssc_has_suffix(x$parsed, "x", weak = "any")
  osfx <- .cssc_other_suffix(x)
  ifelse(!is.na(can), can, ifelse(!is.na(osfx), grepl("x", osfx), NA))
}

# Thickness of the largest run of adjacent rows, certain (ok TRUE) and
# possible (ok TRUE or NA).
.cssc_run_lo_hi <- function(x, ok) {
  ord <- order(x$h$top_cm)
  lo <- .cssc_runs((ok %in% TRUE)[ord], x$thick[ord])
  hi <- .cssc_runs((!(ok %in% FALSE))[ord], x$thick[ord])
  list(lo = lo$thickness, hi = hi$thickness, rows = ord[lo$rows])
}


# ---- D. Podzolic subgroups (pp. 40-41; Ch 10) -------------------------------

#' Ortstein (CSSC subgroups DAA, DBA, DBB, DCA, DCB)
#'
#' A strongly cemented podzolic B (Bhc, Bhfc, Bfc; Ch 2, p. 17) at least 3 cm
#' thick, adjacent layers added up. The book's "more than one-third of the
#' exposed face" is not recorded by any attribute and is not read.
#' @noRd
ortstein_cssc <- function(pedon, min_cm = 3) {
  x <- .cssc_horizons(pedon)
  pod <- .cssc_podzolic_kind(pedon, x)
  cem <- .cssc_cemented(x)
  r <- .cssc_run_lo_hi(x, .cssc_and3v(pod, cem))
  passed <- .cssc_thick_test(r$lo, r$hi, min_cm, strict = FALSE)
  .cssc_result("ortstein_cssc", passed, r$rows,
               list(route = "properties+designations", ortstein_cm = r$lo, ortstein_cm_max = r$hi),
               if (is.na(passed)) c("cementation_class", "designation") else character(0),
               "Ch 3, pp. 40-41; Ch 2, p. 17; Ch 10, pp. 111-116")
}

#' Placic horizon within the control section (CSSC subgroups DAB, DBC, DCC)
#'
#' A thin, strongly cemented layer or series of layers cemented by Fe,
#' Al-organic complexes or Fe and Mn oxides (Bhfc, Bfc, Bgfc; Ch 2, p. 17):
#' read as a strongly cemented podzolic B, or a strongly cemented B with gf,
#' beginning within the control section, each layer thinner than 3 cm (a
#' thicker cemented layer is an ortstein or a duric horizon).
#' @noRd
placic_cssc <- function(pedon) {
  x <- .cssc_horizons(pedon)
  pod <- .cssc_podzolic_kind(pedon, x)
  gf <- .cssc_is(x$parsed, "B", "gf", "any")
  gf[is.na(gf) & !is.na(.cssc_raw_master(x))] <- FALSE   # not read in other notations
  cem <- .cssc_cemented(x)
  cs <- .cssc_in_control_section(x)
  b <- .cssc_b_rows(x) & x$mineral   # a placic horizon is a B (Bhfc, Bfc, Bgfc)
  kind <- .cssc_and3v(.cssc_and3v(b | is.na(b), .cssc_or3v(pod, gf)), cs)
  kind[b %in% FALSE] <- FALSE
  # a placic horizon is a thin layer or a series of thin layers (Ch 2, p. 17):
  # a cemented layer 3 cm thick or more is an ortstein or a duric horizon
  kind[(x$thick <= 0 | x$thick >= 3) %in% TRUE] <- FALSE
  passed <- .cssc_some(cem, which(kind %in% TRUE), which(is.na(kind)))
  .cssc_result("placic_cssc", passed, which(kind %in% TRUE & cem %in% TRUE),
               list(route = "properties+designations"),
               if (is.na(passed)) c("cementation_class", "designation") else character(0),
               "Ch 3, pp. 40-41; Ch 2, p. 17")
}

#' Duric horizon within the control section (CSSC subgroups DAC, DBD, DCD,
#' ICA, IDA)
#'
#' A strongly cemented B, BC or C horizon that is not a podzolic B (Ch 2,
#' p. 16), beginning within the control section. A horizon whose podzolic
#' status is undecided may be an ortstein or placic layer instead, and leaves
#' the test undecided when it is cemented.
#' @noRd
duric_cssc <- function(pedon) {
  x <- .cssc_horizons(pedon)
  pod <- .cssc_podzolic_kind(pedon, x)
  cem <- .cssc_cemented(x)
  can_a <- .cssc_is(x$parsed, "A", weak = "any") %in% TRUE
  raw <- .cssc_raw_master(x)
  cs <- .cssc_in_control_section(x)
  not_a <- x$mineral & !can_a & !(raw %in% c("A", "E"))
  kind <- .cssc_and3v(.cssc_and3v(!pod, not_a), cs)
  kind[(x$thick <= 0) %in% TRUE] <- FALSE   # a layer of no thickness
  passed <- .cssc_some(cem, which(kind %in% TRUE), which(is.na(kind)))
  .cssc_result("duric_cssc", passed, which(kind %in% TRUE & cem %in% TRUE),
               list(route = "properties+designations"),
               if (is.na(passed)) c("cementation_class", "designation") else character(0),
               "Ch 3, pp. 40-41 and 50; Ch 2, p. 16; Ch 4, p. 58")
}

#' Fragipan within the control section (CSSC subgroups DAD, DBE, DCE)
#'
#' A horizon of fragipan character (an x in its designation; Ch 2, p. 16)
#' beginning within the control section. No attribute records the fragipan's
#' brittleness or slaking, so it is read from the designations.
#' @noRd
fragipan_cssc <- function(pedon) {
  x <- .cssc_horizons(pedon)
  fr <- .cssc_fragic_rows(x)
  cs <- .cssc_in_control_section(x)
  passed <- .cssc_some(fr, which(x$mineral & cs %in% TRUE), which(x$mineral & is.na(cs)))
  .cssc_result("fragipan_cssc", passed, which(fr %in% TRUE),
               list(route = "designations"),
               if (is.na(passed)) "designation" else character(0),
               "Ch 3, pp. 40-41; Ch 2, p. 16")
}

#' Mottles (CSSC subgroups: Gleyed Podzols, Brunisols, Regosols)
#'
#' contrast is a set of contrast classes (faint, distinct, prominent) read in
#' the mineral horizons whose top lies from `from` to `to` cm below the
#' mineral surface, or that reach into that window when overlap = TRUE
#' (.cssc_mottles).
#' @noRd
mottles_cssc <- function(pedon, contrast = c("distinct", "prominent"), from = 0, to = 100,
                         overlap = FALSE) {
  x <- .cssc_horizons(pedon)
  m <- .cssc_mottles(x, contrast, from, to, overlap)
  .cssc_result("mottles_cssc", m$passed, m$rows,
               list(route = m$route, contrast = contrast, window_cm = c(from, to)),
               if (is.na(m$passed)) c("mottle_contrast", "designation") else character(0),
               "Ch 3, pp. 40-51")
}

#' Gleyed Brunisols (CSSC subgroups IAA, IAC, IBA, IBC, ICB, ICD, IDB, IDD)
#'
#' Faint to distinct mottles within 50 cm of the mineral surface, or distinct
#' to prominent mottles at depths of 50-100 cm (horizons that reach into
#' that window).
#' @noRd
gleyed_brunisol_cssc <- function(pedon) {
  x <- .cssc_horizons(pedon)
  m1 <- .cssc_mottles(x, c("faint", "distinct"), 0, 50)
  m2 <- .cssc_mottles(x, c("distinct", "prominent"), 50, 100, overlap = TRUE)
  passed <- .any3(c(m1$passed, m2$passed))
  .cssc_result("gleyed_brunisol_cssc", passed, union(m1$rows, m2$rows),
               list(route = if (isTRUE(m1$passed)) m1$route else m2$route),
               if (is.na(passed)) c("mottle_contrast", "designation") else character(0),
               "Ch 3, pp. 49-50; Ch 4, pp. 55-59")
}

#' Deep Bt horizon (CSSC subgroups DBF, DCF)
#'
#' A Bt horizon (bt_horizon_cssc) whose upper boundary is more than 50 cm
#' below the mineral surface.
#' @noRd
deep_bt_cssc <- function(pedon, min_top_cm = 50) {
  x <- .cssc_horizons(pedon)
  bt <- bt_horizon_cssc(pedon)
  passed <- if (identical(bt$passed, FALSE)) FALSE
            else if (isTRUE(bt$passed)) {
              t <- suppressWarnings(min(x$top_ms[bt$layers], na.rm = TRUE))
              if (!is.finite(t)) NA else t > min_top_cm
            } else NA
  .cssc_result("deep_bt_cssc", passed, bt$layers,
               list(route = bt$evidence$route %||% "properties+designations"),
               if (is.na(passed)) c("clay_pct", "designation") else character(0),
               "Ch 3, p. 41; Ch 10, pp. 113-116")
}

#' Sombric Podzols (CSSC subgroups DBG, DBH, DCG, DCH)
#'
#' An Ah horizon at least 10 cm thick: the Ah and Ahe horizons of the surface
#' A (.cssc_surface_a). The key writes Ah, so an Ap does not count.
#' @noRd
thick_ah_cssc <- function(pedon, min_cm = 10) {
  x <- .cssc_horizons(pedon)
  a <- .cssc_surface_a(x)
  passed <- .cssc_over_a(a, function(s) if (is.na(s$ah_cm)) NA else s$ah_cm >= min_cm)
  .cssc_result("thick_ah_cssc", passed, a$rows,
               list(route = if (any(x$desig_known)) "designations+properties" else "properties",
                    ah_cm = a$ah_cm),
               if (is.na(passed)) c("designation", "munsell_value_moist", "oc_pct") else character(0),
               "Ch 3, p. 41; Ch 10, pp. 114-116")
}


# ---- E. Gleysolic subgroups (p. 42; Ch 7) -----------------------------------

#' Slickenside horizon within 1 m (CSSC subgroups EAA, EBA, ECA; great group FA)
#'
#' Horizon by horizon, within `max_top_cm` of the mineral surface: an ss in
#' a designation (more than two slickensides, Ch 2, p. 15; ss also means
#' slickensides in the FAO and USDA notations) passes; otherwise common, many
#' or continuous slickensides pass and any other record fails, as in the
#' order test; otherwise a horizon designated without ss fails and one with
#' no designation is undecided.
#' @noRd
slickenside_horizon_cssc <- function(pedon, max_top_cm = 100) {
  x <- .cssc_horizons(pedon); h <- x$h
  in_depth <- x$mineral & !is.na(x$top_ms) & x$top_ms <= max_top_cm
  osfx <- .cssc_other_suffix(x)
  ss_desig <- (.cssc_has_suffix(x$parsed, "ss", weak = "any") %in% TRUE) |
              (!is.na(osfx) & grepl("ss", osfx))
  sl <- tolower(trimws(as.character(h$slickensides)))
  # as the order test reads them: more than two slickensides are common,
  # many or continuous
  prop <- ifelse(is.na(h$slickensides), NA, sl %in% c("common", "many", "continuous"))
  row <- ifelse(ss_desig, TRUE, ifelse(!is.na(prop), prop,
                 ifelse(x$desig_known | !is.na(osfx), FALSE, NA)))
  passed <- if (any(in_depth)) .any3(row[in_depth]) else FALSE
  list(passed = passed, rows = which(row %in% TRUE & in_depth))
}

#' @noRd
vertic_subgroup_cssc <- function(pedon) {
  r <- slickenside_horizon_cssc(pedon)
  .cssc_result("vertic_subgroup_cssc", r$passed, r$rows,
               list(route = "properties+designations"),
               if (is.na(r$passed)) c("slickensides", "designation") else character(0),
               "Ch 3, p. 42; Ch 7, pp. 84-86")
}

#' Fragic Luvic Gleysol (CSSC subgroup EAC)
#'
#' A fragipan (an x in a designation) within or below the Btg horizon: in a
#' horizon whose top lies at or below the top of the Bt (bt_horizon_cssc).
#' @noRd
fragic_luvic_cssc <- function(pedon) {
  x <- .cssc_horizons(pedon)
  bt <- bt_horizon_cssc(pedon)
  fr <- .cssc_fragic_rows(x)
  bt_top <- suppressWarnings(min(x$top_ms[bt$layers], na.rm = TRUE))
  if (!is.finite(bt_top)) {
    btd <- .cssc_is(x$parsed, "B", "t", "any") %in% TRUE
    bt_top <- suppressWarnings(min(x$top_ms[btd], na.rm = TRUE))
  }
  below <- x$mineral & !is.na(x$top_ms) & (if (is.finite(bt_top)) x$top_ms >= bt_top - 0.01 else TRUE)
  passed <- .cssc_some(fr, which(below), integer(0))
  .cssc_result("fragic_luvic_cssc", passed, which(fr %in% TRUE & below),
               list(route = "designations"),
               if (is.na(passed)) "designation" else character(0),
               "Ch 3, p. 42; Ch 7, p. 84")
}

# Bgf horizons, horizon by horizon: by dithionite-extractable Fe at least 1
# percentage point above that of the parent material (the lowest C of the
# first material, IC; Ch 2, pp. 13-14) on a B horizon (or an undesignated
# mineral horizon) when both are measured; otherwise by a gf in a Canadian
# designation (Bgf, Btgf); otherwise NA.
.cssc_bgf_rows <- function(x) {
  h <- x$h
  ord <- order(h$top_cm)
  is_c <- (.cssc_is(x$parsed, "C", weak = "any") %in% TRUE) | (.cssc_raw_master(x) %in% "C")
  roman <- vapply(x$parsed, function(cmp) if (length(cmp)) cmp[[1]]$roman else "", "")
  d <- as.character(h$designation)
  ic_cand <- ord[is_c[ord] & roman[ord] %in% c("", "I") & !grepl("^[0-9]", d[ord] %||% "")]
  ic <- if (length(ic_cand)) ic_cand[length(ic_cand)] else NA_integer_
  fe_ic <- if (is.na(ic)) NA_real_ else h$fe_dcb_pct[ic]
  b <- .cssc_b_rows(x) & x$mineral
  cand <- b %in% TRUE | (is.na(b) & x$mineral)
  prop <- ifelse(cand & !is.na(h$fe_dcb_pct) & !is.na(fe_ic), h$fe_dcb_pct >= fe_ic + 1, NA)
  gf <- .cssc_is(x$parsed, "B", "gf", "any")
  ifelse(!is.na(prop), prop, ifelse(!is.na(gf), gf, ifelse(b %in% FALSE, FALSE, NA)))
}

#' Fera Gleysols (CSSC subgroups EAE, EBC, ECC)
#'
#' A Bgf horizon at least 10 cm thick (adjacent Bgf horizons added up), or,
#' for the Luvic Gleysols (btgf = TRUE), also a Btgf horizon of any
#' thickness.
#' @noRd
fera_gleysol_cssc <- function(pedon, btgf = FALSE, min_cm = 10) {
  x <- .cssc_horizons(pedon)
  bgf <- .cssc_bgf_rows(x)
  r <- .cssc_run_lo_hi(x, bgf)
  thick <- .cssc_thick_test(r$lo, r$hi, min_cm, strict = FALSE)
  passed <- if (btgf) {
    # a Btgf counts when its own Bgf reading holds (measured Fe first)
    btgf_row <- .cssc_is(x$parsed, "B", c("t", "gf"), "any") %in% TRUE
    .any3(c(thick, if (any(btgf_row)) .any3(bgf[btgf_row]) else FALSE))
  } else thick
  .cssc_result("fera_gleysol_cssc", passed, r$rows,
               list(route = "properties+designations", bgf_cm = r$lo),
               if (is.na(passed)) c("fe_dcb_pct", "designation") else character(0),
               "Ch 3, p. 42; Ch 2, pp. 13-14; Ch 7, pp. 85-87")
}

#' Rego Gleysols (CSSC subgroups EBD, ECD)
#'
#' No B horizon 10 cm or more thick (adjacent B horizons added up; a
#' transition such as BC is not a B). Undesignated mineral horizons below the
#' surface A may be B horizons.
#' @noRd
no_thick_b_cssc <- function(pedon, min_cm = 10) {
  x <- .cssc_horizons(pedon)
  b <- .cssc_b_rows(x) & x$mineral
  a <- .cssc_surface_a(x)
  b[a$rows] <- ifelse(is.na(b[a$rows]), FALSE, b[a$rows])
  r <- .cssc_run_lo_hi(x, b)
  has_b <- .cssc_thick_test(r$lo, r$hi, min_cm, strict = FALSE)
  passed <- if (is.na(has_b)) NA else !has_b
  .cssc_result("no_thick_b_cssc", passed, r$rows,
               list(route = "designations", b_cm = r$lo, b_cm_max = r$hi),
               if (is.na(passed)) "designation" else character(0),
               "Ch 3, p. 42; Ch 7, pp. 86-87")
}


# ---- I. Brunisolic subgroups (pp. 49-50; Ch 4) ------------------------------

#' Eluviated Brunisols (CSSC subgroups IAA, IAB, IBA, IBB, ICB, ICC, IDB, IDC)
#'
#' An Ae or Aej horizon at least 2 cm thick (not an Ahe; an E in another
#' notation), adjacent ones added up.
#' @noRd
ae_horizon_cssc <- function(pedon, min_cm = 2) {
  x <- .cssc_horizons(pedon)
  can_ae <- (.cssc_is(x$parsed, "A", "e", "any") %in% TRUE) &
            !(.cssc_is(x$parsed, "A", "h", "any") %in% TRUE)
  raw <- .cssc_raw_master(x)
  ae <- ifelse(x$desig_known, can_ae, ifelse(!is.na(raw), raw == "E", NA))
  ae[!x$mineral] <- FALSE
  r <- .cssc_run_lo_hi(x, ae)
  passed <- .cssc_thick_test(r$lo, r$hi, min_cm, strict = FALSE)
  .cssc_result("ae_horizon_cssc", passed, r$rows,
               list(route = "designations", ae_cm = r$lo),
               if (is.na(passed)) "designation" else character(0),
               "Ch 3, pp. 49-50; Ch 4, pp. 55-59")
}


# ---- J. Regosolic subgroups (pp. 50-51; Ch 11) ------------------------------

#' Cumulic Regosols (CSSC subgroups JAA, JAB, JBA, JBB)
#'
#' Layers below the Ah horizon (below the surface A, or the mineral surface
#' when there is none) within the control section that differ in moist colour
#' value by one unit or more, or organic C that varies irregularly with depth
#' (some layer holding at least 0.2 percentage points more than the one above
#' it); otherwise a buried A (b) below the surface in the designations. With
#' fewer than two such layers nothing can vary, and only a buried A designated
#' as such makes the soil cumulic. Layers of unknown depth may be among them, and every surface
#' A the profile allows is tried (.cssc_surface_a).
#' @noRd
cumulic_regosol_cssc <- function(pedon, min_value_diff = 1, min_oc_rise = 0.2) {
  x <- .cssc_horizons(pedon); h <- x$h
  a <- .cssc_surface_a(x)
  cs_in <- .cssc_in_control_section(x)
  buried <- .cssc_has_suffix(x$parsed, "b", weak = "any")
  osfx <- .cssc_other_suffix(x)
  buried <- ifelse(!is.na(buried), buried, ifelse(!is.na(osfx), grepl("b", osfx), NA))
  lost <- which(x$mineral & is.na(x$top_ms))   # layers of unknown depth
  at <- function(a_rows) {
    base <- if (length(a_rows)) max(h$bottom_cm[a_rows]) - x$ms else 0
    if (is.na(base)) return(NA)
    rows <- which(x$mineral & !is.na(x$top_ms) & x$top_ms >= base - 0.01 & !(cs_in %in% FALSE))
    rows <- rows[order(h$top_cm[rows])]
    n_all <- length(rows) + length(lost)
    v <- h$munsell_value_moist[rows]
    oc <- h$oc_pct[rows]
    rises <- if (length(rows) >= 2L) diff(oc) else numeric(0)
    complete <- length(lost) == 0L && all(cs_in[rows] %in% TRUE)
    colour <- if (sum(!is.na(v)) >= 2L && diff(range(v, na.rm = TRUE)) >= min_value_diff) TRUE
              else if (n_all < 2L || (complete && !anyNA(v))) FALSE else NA
    irregular <- if (any(rises >= min_oc_rise, na.rm = TRUE)) TRUE
                 else if (n_all < 2L || (complete && !anyNA(oc))) FALSE else NA
    by_props <- .any3(c(colour, irregular))
    b_rows <- c(rows, lost)
    by_desig <- if (any(buried[b_rows] %in% TRUE)) TRUE
                else if (length(b_rows) == 0L || all(!is.na(buried[b_rows]))) FALSE else NA
    # with fewer than two layers nothing varies: only a buried A designated as
    # such makes the soil cumulic
    if (n_all < 2L) return(isTRUE(by_desig))
    .cssc_route(by_props, by_desig)$passed
  }
  res <- if (isTRUE(a$undecidable)) list(NA) else lapply(a$prefixes, function(pf) at(pf$rows))
  passed <- if (all(vapply(res, identical, logical(1), res[[1L]]))) res[[1L]] else NA
  .cssc_result("cumulic_regosol_cssc", passed, integer(0),
               list(route = "properties+designations"),
               if (is.na(passed)) c("munsell_value_moist", "oc_pct", "designation")
               else character(0),
               "Ch 3, pp. 50-51; Ch 11, pp. 118-119")
}
