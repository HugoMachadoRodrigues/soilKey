# =============================================================================
# v0.9.300 -- the keys to subgroups of the New Zealand Soil Classification,
# 4th ed.: the Podzol, Allophanic, Pumice and Melanic Soils (79 subgroups of
# 17 groups). As in R/diagnostics-nzsc-subgroups.R, each subgroup after the
# first is "other soils that ...", so a gate tests only its own criteria,
# and the last subgroup of each group is the default. Three-valued; readings
# in inst/rules/nzsc4/readings.yaml. Hugo's decisions of 2026-10-28: the
# colour of coatings on pores and of gel-like masses (LOVA, LOA) is recorded
# in three new attributes, and not recorded is none; "hue 7.5YR or more"
# (MOZ) is 7.5YR or redder; a silty layer (ZPOZ, ZPZ) is one of the silty
# texture classes; any ignimbrite is the Taupo ignimbrite of MIMC and MIC;
# the Bh of ZDYH tops a B horizon that is clayey and prismatic as a whole;
# the layer of MOL is itself vitric and allophanic; a scoria of no named
# composition is basaltic (MOS).
# =============================================================================

# ---- helpers -----------------------------------------------------------------------

# Bh horizon of each row: a B horizon marked h (Bh, Bhs; a humus-pan Bhm is a
# pan; not buried). A mineral row with no designation may be one unless its
# colour or its carbon rules it out (the Bh of the podzolic-B horizon: moist
# value and chroma 3 or less, or value 4 with chroma 2; 1 % or more organic
# carbon).
.nzsc_bh_rows <- function(x) {
  h <- x$h
  v <- h$munsell_value_moist; ch <- h$munsell_chroma_moist; oc <- h$oc_pct
  dark <- ifelse(!is.na(v) & v > 4, FALSE,
          ifelse(!is.na(ch) & ch > 3, FALSE,
          ifelse(is.na(v) | is.na(ch), NA, v <= 3 | ch <= 2)))
  rich <- ifelse(is.na(oc), NA, oc >= 1)
  maybe <- x$unknown_role & x$mineral
  ifelse(.nzsc_is(x, "B") & x$des$h, TRUE,
         ifelse(maybe, ifelse(dark %in% FALSE | rich %in% FALSE, FALSE, NA), FALSE))
}

# A run of adjacent rows where `ok` holds, anywhere in the profile, more than
# `min_cm` thick (at least, with strict = FALSE).
.nzsc_run_any <- function(x, ok, min_cm, strict = TRUE) .nzsc_run_starting(x, ok, -1e6, 1e6, min_cm, strict)

# The adjacent rows from row i down where `keep` (TRUE or FALSE) holds, each
# row once (a row of no thickness, or one whose bottom lies above its top,
# ends the walk).
.nzsc_walk_down <- function(x, keep, i) {
  h <- x$h; rows <- .nzsc_rows(x)
  run <- i; z <- h$bottom_cm[i]
  while (!is.na(z) && is.finite(z)) {
    k <- rows[!(rows %in% run) & abs(h$top_cm[rows] - z) <= 0.5 & keep[rows]]
    if (!length(k)) break
    k <- k[1L]; run <- c(run, k)
    if (is.na(h$bottom_cm[k]) || h$bottom_cm[k] <= z) { z <- h$bottom_cm[k]; break }
    z <- h$bottom_cm[k]
  }
  list(rows = run, bottom = if (is.na(z)) Inf else z)
}

# Evaluate `fun(fill)` over every TRUE/FALSE filling of `k` undecided items
# (up to 2^8 fillings): the verdict holds only when every filling gives it.
.nzsc_over_fillings <- function(k, fun, max_k = 8L) {
  if (k == 0L) return(fun(logical(0)))
  if (k > max_k) return(NA)
  out <- NULL
  for (m in 0:(2L^k - 1L)) {
    fill <- bitwAnd(as.integer(m), as.integer(2^(0:(k - 1L)))) > 0L
    v <- fun(fill)
    if (is.na(v)) return(NA)
    if (is.null(out)) out <- v else if (!identical(out, v)) return(NA)
  }
  out
}

# Some row where `lower` holds lies at or below the base of a row where
# `upper` holds (not necessarily directly).
.nzsc_beneath <- function(x, upper, lower) {
  h <- x$h; rows <- .nzsc_rows(x)
  cand <- rows[!(upper[rows] %in% FALSE)]
  if (!length(cand)) return(FALSE)
  .nzsc_any(vapply(cand, function(i) {
    z <- h$bottom_cm[i]
    below <- if (is.na(z)) rows[h$top_cm[rows] > h$top_cm[i] + 0.5] else rows[h$top_cm[rows] >= z - 0.5 & rows != i]
    l <- if (length(below)) .nzsc_any(lower[below]) else FALSE
    v <- .and3(c(upper[i], l))
    if (is.na(z) && isTRUE(v)) NA else v
  }, NA))
}

# Thickness of the block of adjacent rows around row i where `keep` (TRUE or
# FALSE, no NA) holds; a row with no bottom reaches anywhere.
.nzsc_block <- function(x, keep, i) {
  h <- x$h; rows <- .nzsc_rows(x)
  if (!isTRUE(keep[i]) || is.na(h$top_cm[i])) return(0)
  th <- function(j) { t <- x$thick[j]; if (is.na(t)) Inf else max(0, t) }
  tot <- th(i); seen <- i
  z <- h$top_cm[i]
  repeat {
    up <- rows[!(rows %in% seen) & !is.na(h$bottom_cm[rows]) & abs(h$bottom_cm[rows] - z) <= 0.5 & keep[rows]]
    if (!length(up)) break
    j <- up[1L]; tot <- tot + th(j); z <- h$top_cm[j]; seen <- c(seen, j)
  }
  z <- h$bottom_cm[i]
  while (!is.na(z)) {
    dn <- rows[!(rows %in% seen) & abs(h$top_cm[rows] - z) <= 0.5 & keep[rows]]
    if (!length(dn)) break
    j <- dn[1L]; tot <- tot + th(j); z <- h$bottom_cm[j]; seen <- c(seen, j)
  }
  tot
}

# Silty texture of each row: silt, silt loam, silty clay loam or silty clay
# (Hugo's decision, 2026-10-28), from the field texture, else the lab
# triangle; organic material, litter and rock are not silty.
.nzsc_silty_rows <- function(x) {
  cls <- .nzsc_texture_class(x)
  ifelse(x$organic %in% TRUE | x$role %in% c("L", "O", "R", "CR"), FALSE,
         ifelse(is.na(cls), NA, cls %in% c("silt", "silt loam", "silty clay loam", "silty clay")))
}

# The colour of the coatings on pores or of the gel-like masses (LOVA,
# LOA): hue 7.5YR or redder, moist value 5 or less, chroma 3 or more. Recorded
# when met (Hugo's decision, 2026-10-28): not recorded is none.
.nzsc_pore_coatings_acidic <- function(x) {
  h <- x$h; n <- x$n
  hue_txt <- as.character(h$pore_coatings_hue_moist %||% rep(NA_character_, n))
  v <- h$pore_coatings_value_moist %||% rep(NA_real_, n)
  ch <- h$pore_coatings_chroma_moist %||% rep(NA_real_, n)
  rec <- (!is.na(hue_txt) & nzchar(trimws(hue_txt))) | !is.na(v) | !is.na(ch)
  red <- .nzsc_hue_side(hue_txt, "redder_7.5YR")
  ok <- .and3v(red, .and3v(ifelse(is.na(v), NA, v <= 5), ifelse(is.na(ch), NA, ch >= 3)))
  ifelse(rec, ok, FALSE)
}

# Mineral rows of unknown role (the uppermost mineral row aside, which is the
# topsoil) may be a B (or BC) horizon or not: `fun(rows)` is evaluated with
# the designated rows of `roles` (pans aside) plus each choice of those rows
# (up to eight; beyond, all of them and none), and the verdict holds only
# when every choice gives it (v0.9.301).
.nzsc_roles_eval <- function(x, roles, zb, fun) {
  h <- x$h
  b <- which(.nzsc_is(x, roles) & !x$des$m & !is.na(h$top_cm)); b <- b[h$top_cm[b] < zb - 0.5]
  rows <- .nzsc_rows(x); first <- rows[x$mineral[rows]][1L]
  maybe <- .nzsc_maybe_rows(x); maybe <- maybe[h$top_cm[maybe] < zb - 0.5 & !(maybe %in% first)]
  k <- length(maybe)
  if (k <= 8L) return(.nzsc_over_fillings(k, function(fill) fun(sort(c(b, maybe[fill])))))
  v1 <- fun(b); v2 <- fun(sort(c(b, maybe)))
  if (identical(v1, v2)) v1 else NA
}

# "In some part of the B or BC horizon" (or of `roles`) to `zb`.
.nzsc_b_bc_some_part <- function(x, ok, zb, roles = c("B", "BC")) {
  .nzsc_roles_eval(x, roles, zb, function(rows) if (!length(rows)) FALSE else .nzsc_some_part_in(x, rows, ok, -1e6, zb))
}

# Hue on the Munsell circle and its two readings here: 7.5YR or redder (red
# purple included, a neutral hue not), and 10YR or yellower (to 10Y, a
# neutral hue included, as the colour classes of the Pallic Soils read it).
.nzsc_hue_side <- function(hue_txt, side = c("redder_7.5YR", "yellower_10YR")) {
  side <- match.arg(side)
  neutral <- !is.na(hue_txt) & grepl("^N", toupper(trimws(as.character(hue_txt))))
  hue <- .nzsc_hue_num(hue_txt)
  v <- if (side == "redder_7.5YR") hue <= 17.5 + 0.01 else hue >= 20 - 0.01 & hue <= 30 + 0.01
  ifelse(neutral, side == "yellower_10YR", ifelse(is.na(hue), NA, v))
}

# A thickness-weighted verdict that `value` reaches `thr` (more than, with
# strict): `lo` and `hi` bound the value of each row (equal when measured).
# Rows with no bottom have an unknown weight: the verdict holds only when it
# is the same with them weighing nothing and weighing all.
.nzsc_weighted <- function(x, rows, lo, hi, thr, strict = FALSE) {
  if (!length(rows)) return(NA)
  th <- x$thick[rows]; open <- is.na(th)
  pass <- function(v) if (strict) v > thr else v >= thr
  one <- function(w) {
    if (sum(w) <= 0) return(NA)
    l <- sum(w * lo[rows]) / sum(w); u <- sum(w * hi[rows]) / sum(w)
    if (anyNA(c(l, u))) NA else if (pass(l)) TRUE else if (!pass(u)) FALSE else NA
  }
  w0 <- ifelse(open, 0, pmax(0, th))
  if (!any(open)) return(one(w0))
  if (sum(w0) <= 0) return(one(ifelse(open, 1, 0)))
  a <- one(w0); b <- one(ifelse(open, 1e6, w0))
  if (identical(a, b)) a else NA
}

# Sand of each row as bounds: the lab sand_pct when measured, else the range
# of its texture class (USDA triangle).
.nzsc_sand_bounds <- function(x) {
  s <- x$h$sand_pct; cls <- .nzsc_texture_class(x)
  lo_c <- c("sand" = 85, "loamy sand" = 70, "sandy loam" = 43, "loam" = 23, "silt loam" = 0, "silt" = 0,
            "sandy clay loam" = 45, "clay loam" = 20, "silty clay loam" = 0, "sandy clay" = 45, "silty clay" = 0,
            "clay" = 0)
  hi_c <- c("sand" = 100, "loamy sand" = 90, "sandy loam" = 85, "loam" = 52, "silt loam" = 50, "silt" = 20,
            "sandy clay loam" = 80, "clay loam" = 45, "silty clay loam" = 20, "sandy clay" = 65, "silty clay" = 20,
            "clay" = 45)
  lo <- ifelse(!is.na(s), s, unname(lo_c[cls])); hi <- ifelse(!is.na(s), s, unname(hi_c[cls]))
  list(lo = ifelse(is.na(lo), 0, lo), hi = ifelse(is.na(hi), 100, hi))
}

# Origin of each row: its own layer or rock origin, else the site's parent
# material ("" when none is recorded).
.nzsc_origin_text <- function(pedon, x) {
  h <- x$h; n <- x$n
  own <- paste(.nzsc_txt(h$layer_origin, n), .nzsc_txt(h$rock_origin, n))
  own <- gsub("\\bna\\b", "", own, ignore.case = TRUE)
  site_pm <- tolower(as.character(.nzsc_site1(pedon, "parent_material") %|NA|% ""))
  ifelse(grepl("[a-z]", own), own, site_pm)
}

# The base of the A horizon read two ways: as .nzsc_a_base() reads it, and
# extended through mineral rows of unknown role directly under the A, which
# may be A too.
.nzsc_a_bases <- function(x) {
  h <- x$h; rows <- .nzsc_rows(x)
  zt <- .nzsc_a_base(x); zt2 <- zt; seen <- integer(0)
  while (!is.na(zt2) && is.finite(zt2)) {
    j <- rows[!(rows %in% seen) & abs(h$top_cm[rows] - zt2) <= 0.5 & x$unknown_role[rows] & x$mineral[rows]]
    if (!length(j)) break
    j <- j[1L]; seen <- c(seen, j)
    b <- h$bottom_cm[j]
    if (is.na(b)) { zt2 <- Inf; break }
    if (b <= zt2) break
    zt2 <- b
  }
  c(zt, zt2)
}

# ---- Z: Podzol Soils ---------------------------------------------------------------------

# Bh subgroups (ZDH, ZPH, ZGH, ZOH; ZPHQ, ZPHP with a pan): a Bh horizon more
# than `min_cm` thick (at least, with strict = FALSE; `min_cm: 0`, any Bh).
# `clayey_prismatic` (ZDYH; Hugo's decision, 2026-10-28): the Bh tops a B
# horizon that is clayey and prismatic as a whole, read from the B row
# directly below the Bh, or from the Bh itself when no B lies below it.
bh_subgroup_nzsc <- function(pedon, min_cm = 5, strict = TRUE, clayey_prismatic = FALSE) {
  x <- .nzsc_horizons(pedon); h <- x$h
  miss <- c("designation", if (isTRUE(clayey_prismatic)) c("structure_type", "clay_pct", "field_texture"))
  bh <- .nzsc_bh_rows(x)
  min_cm <- as.numeric(min_cm)
  if (!isTRUE(clayey_prismatic)) {
    v <- .nzsc_run_any(x, bh, min_cm, strict = isTRUE(strict))
    return(.nzsc_subgroup("bh_subgroup_nzsc", v, "Z, Bh subgroups", list(min_cm = min_cm), miss, which(bh %in% TRUE)))
  }
  t <- tolower(as.character(h$structure_type))
  prism <- ifelse(grepl("prism", t), TRUE, ifelse(is.na(h$structure_type), NA, FALSE))
  clay <- ifelse(is.na(x$tex$group), NA, x$tex$group %in% "clayey")
  cp <- .and3v(prism, clay)
  isb <- ifelse(.nzsc_is(x, "B") & !x$des$m, TRUE, ifelse(x$unknown_role & x$mineral, NA, FALSE))
  rows <- .nzsc_rows(x)
  pass <- function(t) if (isTRUE(strict)) t > min_cm else t >= min_cm
  cand <- rows[!(bh[rows] %in% FALSE)]
  v <- .nzsc_any(vapply(cand, function(i) {
    up <- rows[rows != i & !is.na(h$bottom_cm[rows]) & abs(h$bottom_cm[rows] - h$top_cm[i]) <= 0.5]
    if (any(bh[up] %in% TRUE)) return(FALSE)          # the Bh starts higher up
    r <- .nzsc_run_down(x, bh, i)
    th <- if (pass(r$sure)) TRUE else if (pass(r$poss)) NA else FALSE
    if (isFALSE(th)) return(FALSE)
    run <- .nzsc_walk_down(x, bh %in% TRUE, i)$rows
    z <- r$sure_bottom
    j <- if (is.finite(z)) rows[!(rows %in% run) & abs(h$top_cm[rows] - z) <= 0.5] else integer(0)
    j <- j[!(bh[j] %in% TRUE)]
    own <- .and3(cp[run])
    # the B horizon the Bh tops: the B row below it, or the Bh itself when
    # no B lies below (a row of unknown role may be either)
    whole <- if (!length(j) || isFALSE(isb[j[1L]])) own
             else if (isTRUE(isb[j[1L]])) cp[j[1L]]
             else if (identical(cp[j[1L]], own)) own else NA
    out <- .and3(c(th, bh[i], whole))
    if (isTRUE(out) && any(is.na(bh[up]))) NA else out
  }, NA))
  .nzsc_subgroup("bh_subgroup_nzsc", v, "Z, ZDYH", list(min_cm = min_cm), miss, which(bh %in% TRUE))
}

# ZDJH, ZOJH: an argillic horizon beneath a Bh horizon; ZXJH (`upper:
# ortstein_or_placic`): beneath the ortstein-pan or placic horizon. A pan (m)
# is not an argillic horizon.
argillic_beneath_nzsc <- function(pedon, upper = c("bh", "ortstein_or_placic")) {
  upper <- match.arg(upper)
  x <- .nzsc_horizons(pedon)
  up <- if (upper == "bh") .nzsc_bh_rows(x) else .nzsc_pan_rows(x, "ortstein") | .nzsc_pan_rows(x, "placic")
  arg <- ifelse(x$des$m, FALSE, .nzsc_argillic_rows(x))
  .nzsc_subgroup("argillic_beneath_nzsc", .nzsc_beneath(x, up, arg), "Z, argillic subgroups",
                 list(upper = upper), c("designation", "clay_pct", "clay_films_amount"), which(arg %in% TRUE))
}

# ZPF: a moderately or very fluid horizon in some part (10 cm) above the
# podzolic-B horizon. Fluidity not recorded is non-fluid. The top of the
# podzolic-B is the first sure podzolic-B row, or any undecided one above
# it: the verdict holds only when it is the same for each.
fluid_above_podzolic_b_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h
  miss <- c("fluidity_class", "designation", "naf_test")
  fl <- .nzsc_fluid_rows(x); pz <- .nzsc_podzolic_b_rows(x)
  rows <- .nzsc_rows(x)
  if (!any(fl[rows])) return(.nzsc_subgroup("fluid_above_podzolic_b_nzsc", FALSE, "Z, ZPF"))
  at <- function(z) {
    above <- rows[h$top_cm[rows] < z - 0.5]
    if (!length(above)) FALSE else .nzsc_some_part_in(x, above, fl, -1e6, z)
  }
  sure <- rows[pz[rows] %in% TRUE]
  z1 <- if (length(sure)) min(h$top_cm[sure]) else Inf
  open <- rows[is.na(pz[rows]) & h$top_cm[rows] < z1 - 0.5]
  tops <- c(if (length(sure)) z1, h$top_cm[open])
  # no podzolic-B at all: nothing lies above one
  vs <- c(lapply(tops, at), if (!length(sure)) list(FALSE))
  if (!length(vs)) vs <- list(FALSE)
  v <- if (all(vapply(vs, identical, NA, vs[[1L]]))) vs[[1L]] else NA
  .nzsc_subgroup("fluid_above_podzolic_b_nzsc", v, "Z, ZPF", missing = miss)
}

# ZPOZ, ZPZ: an eluvial horizon of slightly firm or greater moist strength,
# apedal massive or prismatic with peds of 60 to 200 mm (ped_size_max_mm, the
# upper limit of the size class), in a silty layer (adjacent silty rows) 15
# cm or more thick.
silt_mantled_podzol_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; n <- x$n
  miss <- c("designation", "strength_moist", "structure_grade", "structure_type", "ped_size_max_mm", "field_texture")
  e <- .nzsc_eluvial_rows(x)
  rows <- .nzsc_rows(x)
  cand <- rows[!(e[rows] %in% FALSE) & x$mineral[rows]]
  if (!length(cand)) return(.nzsc_subgroup("silt_mantled_podzol_nzsc", FALSE, "Z, ZPOZ/ZPZ"))
  str <- .nzsc_class_index(h$strength_moist %||% rep(NA, n), "strength")
  firm <- ifelse(is.na(str), NA, str >= 3)
  t <- tolower(as.character(h$structure_type))
  pedal <- .nzsc_pedal(h)
  # structureless with no type recorded may be single grain
  massive <- ifelse(grepl("massive", t), TRUE, ifelse(grepl("single", t) | pedal %in% TRUE, FALSE,
             ifelse(pedal %in% FALSE & !is.na(h$structure_type), TRUE, NA)))
  size <- h$ped_size_max_mm %||% rep(NA_real_, n)
  prism <- ifelse(grepl("prism", t), ifelse(is.na(size), NA, size >= 60 & size <= 200),
                  ifelse(is.na(h$structure_type), NA, FALSE))
  silty <- .nzsc_silty_rows(x)
  layer <- vapply(seq_len(n), function(i) {
    if (!(i %in% cand) || silty[i] %in% FALSE) return(FALSE)
    s <- .nzsc_block(x, silty %in% TRUE, i); p <- .nzsc_block(x, !(silty %in% FALSE), i)
    if (s >= 15) TRUE else if (p >= 15) NA else FALSE
  }, NA)
  ok <- .and3v(.and3v(e, firm), .and3v(.any3v(massive, prism), layer))
  .nzsc_subgroup("silt_mantled_podzol_nzsc", .nzsc_any(ok[cand]), "Z, ZPOZ/ZPZ", missing = miss,
                 layers = which(ok %in% TRUE))
}

# ---- L: Allophanic Soils ------------------------------------------------------------------

# Acidic subgroups read in some part of the B or BC horizon to `depth` cm
# from the mineral soil surface: pH below 5.5 (EMA, `depth: 60`); LOA and
# LOVA (`coatings`), or coatings on pores or gel-like masses of hue 7.5YR or
# redder, value 5 or less and chroma 3 or more. v0.9.301, the Granular
# Soils: pH below `ph` 5.1 (NXA, NXMA, NOMA), 5.1 or less (NPA, `strict:
# false`), or in the major part of the B or BC (NOA, `part: major`).
acidic_b_subgroup_nzsc <- function(pedon, depth = 60, coatings = FALSE, ph = 5.5, strict = TRUE,
                                   part = c("some", "major")) {
  part <- match.arg(part)
  x <- .nzsc_horizons(pedon); h <- x$h
  miss <- c("ph_h2o", if (isTRUE(coatings)) c("pore_coatings_hue_moist", "pore_coatings_value_moist", "pore_coatings_chroma_moist"))
  pre <- .nzsc_sub_ms_or_na(x, "acidic_b_subgroup_nzsc", "acidic subgroups (B or BC)", miss); if (!is.null(pre)) return(pre)
  lim <- as.numeric(ph)
  ok <- ifelse(is.na(h$ph_h2o), NA, if (isTRUE(strict)) h$ph_h2o < lim else h$ph_h2o <= lim)
  if (isTRUE(coatings)) ok <- .any3v(.nzsc_pore_coatings_acidic(x), ok)
  zb <- x$ms + as.numeric(depth)
  v <- if (part == "some") .nzsc_b_bc_some_part(x, ok, zb) else .nzsc_b_bc_major_part(x, ok, zb)
  .nzsc_subgroup("acidic_b_subgroup_nzsc", v, "acidic subgroups (B or BC)",
                 list(depth = depth, coatings = isTRUE(coatings), ph = lim, part = part), miss)
}

# "In the major part of the B or BC horizon" (or of `roles`) to `zb`, or to
# its base when shallower.
.nzsc_b_bc_major_part <- function(x, ok, zb, roles = c("B", "BC")) {
  h <- x$h
  .nzsc_roles_eval(x, roles, zb, function(rows) {
    if (!length(rows)) return(FALSE)
    base <- if (anyNA(h$bottom_cm[rows])) Inf else max(h$bottom_cm[rows])
    .nzsc_major_part_b(x, rows, ok, zb = min(zb, base))
  })
}

# LOV (and LOVA): the allophanic soil material is formed predominantly from
# tephric soil material (more than half of its thickness) and has 50 % or
# more sand by weighted average (lab sand, else the range of the texture
# class). Rows of undecided allophanic status are tried in every filling.
vitric_allophanic_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon)
  miss <- c("phosphate_retention_pct", "bulk_density_g_cm3", "layer_origin", "volcanic_glass_pct", "sand_pct")
  al <- .nzsc_allophanic_rows(x); te <- .nzsc_tephric_rows(pedon, x)
  sb <- .nzsc_sand_bounds(x)
  tl <- ifelse(te %in% TRUE, 100, 0); tu <- ifelse(te %in% FALSE, 0, 100)
  rows <- .nzsc_rows(x)
  na_rows <- rows[is.na(al[rows])]
  judge <- function(fill) {
    a <- al %in% TRUE; a[na_rows] <- fill
    r <- rows[a[rows]]
    if (!length(r)) return(FALSE)
    .and3(c(.nzsc_weighted(x, r, tl, tu, 50, strict = TRUE), .nzsc_weighted(x, r, sb$lo, sb$hi, 50)))
  }
  .nzsc_subgroup("vitric_allophanic_nzsc", .nzsc_over_fillings(length(na_rows), judge), "L, LOV/LOVA", missing = miss)
}

# LOBG: allophanic soil material over a non-allophanic B horizon (buried or
# not) that, to its base or to 90 cm from the mineral soil surface
# (whichever is less), is clayey throughout, with any rock fragments very
# highly or completely weathered (weathering_stage saprolite or completely
# weathered; fragments not recorded read as none), and in most of it a
# cutanoxidic horizon or a moderately or strongly pedal cutanic or argillic
# horizon with polyhedral peds of 20 mm or less. Rows of undecided
# allophanic status, and mineral rows of unknown role (which may be a B), are
# tried in every filling.
buried_granular_allophanic_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; n <- x$n
  miss <- c("designation", "phosphate_retention_pct", "bulk_density_g_cm3", "clay_pct", "structure_grade",
            "structure_type", "ped_size_max_mm", "weathering_stage")
  pre <- .nzsc_sub_ms_or_na(x, "buried_granular_allophanic_nzsc", "L, LOBG", miss); if (!is.null(pre)) return(pre)
  al <- .nzsc_allophanic_rows(x)
  rows <- .nzsc_rows(x)
  isb <- ifelse(x$role %in% "B" & !x$des$m, TRUE, ifelse(x$unknown_role & x$mineral, NA, FALSE))
  clay <- ifelse(is.na(x$tex$group), NA, x$tex$group %in% "clayey")
  cf <- h$coarse_fragments_pct; ws <- .nzsc_txt(h$weathering_stage, n)
  frag <- ifelse(is.na(cf) | cf <= 0, TRUE, ifelse(grepl("complete|saprolit|very high", ws), TRUE,
                 ifelse(is.na(ws) | !nzchar(ws), NA, FALSE)))
  ok1 <- .and3v(clay, frag)
  # the B is buried by the allophanic material: read it whether or not it
  # carries the buried prefix
  xu <- x; xu$des$buried <- rep(FALSE, n)
  ok2 <- .any3v(.nzsc_cutanoxidic_rows(xu), .nzsc_polyhedral_cutanic(xu))
  na_al <- rows[is.na(al[rows])]; na_b <- rows[is.na(isb[rows])]
  judge <- function(fill) {
    a <- al %in% TRUE; b <- isb %in% TRUE
    a[na_al] <- fill[seq_along(na_al)]; b[na_b] <- fill[length(na_al) + seq_along(na_b)]
    cand <- rows[b[rows] & !a[rows]]
    cand <- cand[vapply(cand, function(i) any(a[rows] & !is.na(h$bottom_cm[rows]) & h$bottom_cm[rows] <= h$top_cm[i] + 0.5), NA)]
    if (!length(cand)) return(FALSE)
    j <- cand[1L]
    w <- .nzsc_walk_down(x, b & !a, j)
    zt <- h$top_cm[j]; zb <- min(w$bottom, x$ms + 90)
    if (zb <= zt + 0.5) return(FALSE)
    .and3(c(.nzsc_all_in(x, ok1, zt, zb), .nzsc_major_part_b(x, w$rows, ok2, zb = zb)))
  }
  v <- .nzsc_over_fillings(length(na_al) + length(na_b), judge)
  .nzsc_subgroup("buried_granular_allophanic_nzsc", v, "L, LOBG", missing = miss)
}

# ---- M: Pumice Soils ----------------------------------------------------------------------

# MIMC, MIC: a layer of ignimbrite (any ignimbrite named in the layer or rock
# origin, or the site's parent material: Hugo's decision, 2026-10-28; an
# origin naming pumice, tephra or a pyroclastic flow without saying
# ignimbrite may be one) that is densely packed (packing high or more) or of
# firm or stronger moist strength, with brittle failure (recorded when met:
# the x or (x) notation, a brittle consistence or pan; a row with no
# designation and none of these recorded may be brittle) and no roots, with
# its top within 90 cm of the mineral soil surface.
compacted_ignimbrite_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; n <- x$n
  miss <- c("layer_origin", "packing_degree", "strength_moist", "roots_abundance")
  pre <- .nzsc_sub_ms_or_na(x, "compacted_ignimbrite_nzsc", "M, MIMC/MIC", miss); if (!is.null(pre)) return(pre)
  org <- .nzsc_origin_text(pedon, x)
  ign <- ifelse(grepl("ignimbrite", org), TRUE,
         ifelse(grepl("pumice|tephra|pyroclast", org) | !grepl("[a-z]", org), NA, FALSE))
  pk <- .nzsc_packing_index(h$packing_degree %||% rep(NA, n))
  str <- .nzsc_class_index(h$strength_moist %||% rep(NA, n), "strength")
  compact <- ifelse((!is.na(pk) & pk >= 3) | (!is.na(str) & str >= 4), TRUE,
             ifelse(!is.na(pk) & !is.na(str), FALSE, NA))
  cons <- .nzsc_txt(h$consistence_moist, n); pt <- .nzsc_txt(h$pan_type, n)
  marked <- x$des$x | x$des$x_weak | grepl("brittle", cons) | grepl("brittle|fragipan", pt)
  unrec <- (is.na(cons) | !nzchar(cons)) & (is.na(pt) | !nzchar(pt))
  brittle <- ifelse(marked, TRUE, ifelse(x$unknown_role & unrec, NA, FALSE))
  roots <- .nzsc_txt(h$roots_abundance, n)
  barrier <- ifelse(is.na(roots) | !nzchar(roots), NA, roots %in% c("none", "no roots", "nil"))
  ok <- ifelse(x$mineral, .and3v(.and3v(brittle, ign), .and3v(compact, barrier)), FALSE)
  rows <- .nzsc_rows(x); rows <- rows[h$top_cm[rows] < x$ms + 90]
  .nzsc_subgroup("compacted_ignimbrite_nzsc", .nzsc_any(ok[rows]), "M, MIMC/MIC", missing = miss,
                 layers = which(ok %in% TRUE))
}

# MOZ: a B horizon, or subhorizon of the B, more than 20 cm thick with hue
# 7.5YR or redder (Hugo's decision, 2026-10-28: "7.5YR or more" read as
# redder, against the 10YR or yellower of MOI).
podzolic_pumice_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon)
  red <- .nzsc_hue_side(x$h$munsell_hue_moist, "redder_7.5YR")
  b <- .nzsc_is(x, "B") & !x$des$m
  ok <- ifelse(b, red, ifelse(x$unknown_role & x$mineral, ifelse(red %in% FALSE, FALSE, NA), FALSE))
  .nzsc_subgroup("podzolic_pumice_nzsc", .nzsc_run_any(x, ok, 20), "M, MOZ",
                 missing = c("designation", "munsell_hue_moist"), layers = which(ok %in% TRUE))
}

# MOL: a layer between the upper and lower depth of the vitric soil material
# that is itself vitric and meets allophanic soil material apart from the
# bulk density (Hugo's decision, 2026-10-28: "which also meets" asks both).
allophanic_in_vitric_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon)
  miss <- c("phosphate_retention_pct", "naf_test", "sand_pct", "volcanic_glass_pct")
  ok <- .and3v(.nzsc_vitric_rows(x), .nzsc_allophanic_rows(x, density = FALSE))
  rows <- .nzsc_rows(x)
  .nzsc_subgroup("allophanic_in_vitric_nzsc", .nzsc_any(ok[rows]), "M, MOL", missing = miss,
                 layers = which(ok %in% TRUE))
}

# MOBL: a vitric layer in the B or BC horizon (a row with no designation may
# be either) with its base less than 60 cm from the mineral soil surface,
# over a layer (directly below it, vitric or not) that meets allophanic soil
# material apart from the bulk density. A description that ends at the base
# of the vitric layer (not on rock or a hard layer) leaves it open.
buried_allophanic_pumice_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h
  miss <- c("phosphate_retention_pct", "naf_test", "sand_pct", "volcanic_glass_pct")
  pre <- .nzsc_sub_ms_or_na(x, "buried_allophanic_pumice_nzsc", "M, MOBL", miss); if (!is.null(pre)) return(pre)
  vi <- .nzsc_vitric_rows(x); al <- .nzsc_allophanic_rows(x, density = FALSE)
  isbc <- ifelse(.nzsc_is(x, c("B", "BC")) & !x$des$m, TRUE, ifelse(x$unknown_role & x$mineral, NA, FALSE))
  nxt <- .nzsc_next_row(x)
  rows <- .nzsc_rows(x)
  cand <- rows[!(isbc[rows] %in% FALSE) & !(vi[rows] %in% FALSE) &
               !is.na(h$bottom_cm[rows]) & h$bottom_cm[rows] < x$ms + 60]
  v <- .nzsc_any(vapply(cand, function(i) {
    j <- nxt[i]
    below <- if (is.na(j)) { if (.nzsc_ends_hard(x)) FALSE else NA } else al[j]
    .and3(c(isbc[i], vi[i], below))
  }, NA))
  .nzsc_subgroup("buried_allophanic_pumice_nzsc", v, "M, MOBL", missing = miss)
}

# MOS: from the mineral soil surface to 25 cm, predominantly (more than 50 %
# by thickness-weighted average) gravel-sized clasts (gravel_pct, else coarse
# fragments; not recorded read as none) of basaltic scoria: an origin (the
# layer or rock origin, else the site's parent material) naming scoria, not
# of another composition (Hugo's decision, 2026-10-28: a scoria of no named
# composition is basaltic). A description ending above 25 cm (not on rock or
# a hard layer) may hold more below.
scoriaceous_pumice_nzsc <- function(pedon, depth = 25, reach = FALSE) {
  x <- .nzsc_horizons(pedon); h <- x$h; n <- x$n
  miss <- c("gravel_pct", "coarse_fragments_pct", "layer_origin", "rock_origin")
  where <- if (isTRUE(reach)) "R, RTS" else "M, MOS"
  pre <- .nzsc_sub_ms_or_na(x, "scoriaceous_pumice_nzsc", where, miss); if (!is.null(pre)) return(pre)
  # `depth` 60 and `reach` for the Scoriaceous Tephric Recent Soils
  # (v0.9.302): the scoria also extends to that depth
  .nzsc_scoria_window(pedon, x, x$ms, x$ms + as.numeric(depth), where, reach = isTRUE(reach))
}

# Predominantly (more than 50 % by thickness-weighted average) gravel-sized
# basaltic scoria over [zt, zb]: the verdict of MOS, RTS and RTSP.
.nzsc_scoria_window <- function(pedon, x, zt, zb, where = "scoriaceous subgroups", reach = FALSE) {
  h <- x$h; n <- x$n
  miss <- c("gravel_pct", "coarse_fragments_pct", "layer_origin", "rock_origin")
  org <- .nzsc_origin_text(pedon, x)
  sc <- ifelse(grepl("scori", org), grepl("basalt", org) | !grepl("andesit|dacit|rhyolit", org),
               ifelse(grepl("[a-z]", org), FALSE, NA))
  gr <- h$gravel_pct %||% rep(NA_real_, n)
  fr <- ifelse(!is.na(gr), gr, h$coarse_fragments_pct); fr <- ifelse(is.na(fr), 0, fr)
  if (is.na(zt) || is.na(zb) || zb <= zt) return(.nzsc_subgroup("scoriaceous_pumice_nzsc", FALSE, where))
  rows <- .nzsc_rows_in(x, zt, zb)
  w <- .asc_overlap(h$top_cm[rows], ifelse(is.na(h$bottom_cm[rows]), zb, h$bottom_cm[rows]), zt, zb)
  gap <- max(0, (zb - zt) - sum(w))
  gap_hi <- if (gap > 0 && .nzsc_short_of(x, zb)) gap else 0
  lo <- sum(w * ifelse(sc[rows] %in% TRUE, fr[rows], 0)) / (zb - zt)
  hi <- (sum(w * ifelse(sc[rows] %in% FALSE, 0, fr[rows])) + 100 * gap_hi) / (zb - zt)
  v <- if (lo > 50) TRUE else if (hi <= 50) FALSE else NA
  if (isTRUE(reach)) {
    # the horizon at the base of the window is predominantly scoria too
    at <- rows[h$top_cm[rows] < zb - 0.5 & (is.na(h$bottom_cm[rows]) | h$bottom_cm[rows] >= zb - 0.5)]
    end <- if (!length(at)) { if (.nzsc_short_of(x, zb)) NA else FALSE } else .nzsc_any(.and3v(sc, fr > 50)[at])
    v <- .and3(c(v, end))
  }
  .nzsc_subgroup("scoriaceous_pumice_nzsc", v, where, list(lo = lo, hi = hi), miss)
}

# MOI: a Bw horizon (adjacent Bw rows) 30 cm or less thick with hue 10YR or
# yellower (a neutral hue included) and chroma 4 or less (to the nearest
# chip) in all its rows.
immature_pumice_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h
  miss <- c("designation", "munsell_hue_moist", "munsell_chroma_moist")
  bw <- .nzsc_is(x, "B") & x$des$w & !x$des$m
  ch <- round(h$munsell_chroma_moist)
  neutral <- !is.na(h$munsell_hue_moist) & grepl("^N", toupper(trimws(as.character(h$munsell_hue_moist))))
  ch[neutral & is.na(ch)] <- 0
  col <- .and3v(.nzsc_hue_side(h$munsell_hue_moist, "yellower_10YR"), ifelse(is.na(ch), NA, ch <= 4))
  rows <- .nzsc_rows(x)
  starts <- rows[bw[rows] & vapply(rows, function(i) {
    up <- rows[rows != i & !is.na(h$bottom_cm[rows]) & abs(h$bottom_cm[rows] - h$top_cm[i]) <= 0.5]
    !any(bw[up])
  }, NA)]
  maybe <- rows[x$unknown_role[rows] & x$mineral[rows]]
  if (!length(starts)) {
    th <- x$thick[maybe]
    open <- maybe[!(col[maybe] %in% FALSE) & (is.na(th) | th <= 30)]
    return(.nzsc_subgroup("immature_pumice_nzsc", if (length(open)) NA else FALSE, "M, MOI", missing = miss))
  }
  v <- .nzsc_any(vapply(starts, function(i) {
    w <- .nzsc_walk_down(x, bw, i)
    th <- sum(ifelse(is.na(x$thick[w$rows]), Inf, pmax(0, x$thick[w$rows])))
    out <- .and3(c(th <= 30, col[w$rows]))
    # an undesignated row next to the Bw (above or below) may thicken it
    z <- w$bottom
    nb_dn <- if (is.finite(z)) rows[x$unknown_role[rows] & abs(h$top_cm[rows] - z) <= 0.5] else integer(0)
    nb_up <- rows[x$unknown_role[rows] & !is.na(h$bottom_cm[rows]) & abs(h$bottom_cm[rows] - h$top_cm[i]) <= 0.5]
    if (isTRUE(out) && length(c(nb_dn, nb_up))) NA else out
  }, NA))
  .nzsc_subgroup("immature_pumice_nzsc", v, "M, MOI", missing = miss)
}

# ---- E: Melanic Soils ----------------------------------------------------------------------

# EVMC, EVM: redox segregations within `depth` cm of the mineral soil
# surface (a percentage or the notation; recorded when met).
redox_subgroup_nzsc <- function(pedon, depth = 60) {
  x <- .nzsc_horizons(pedon); h <- x$h
  miss <- c("redoximorphic_features_pct", "designation")
  pre <- .nzsc_sub_ms_or_na(x, "redox_subgroup_nzsc", "E, EVMC/EVM", miss); if (!is.null(pre)) return(pre)
  rows <- .nzsc_rows(x); rows <- rows[x$mineral[rows] & h$top_cm[rows] < x$ms + as.numeric(depth)]
  r <- .nzsc_redox_present(x)
  .nzsc_subgroup("redox_subgroup_nzsc", .nzsc_any(r[rows]), "E, EVMC/EVM", missing = miss,
                 layers = rows[r[rows] %in% TRUE])
}

# EPV: the vertic criteria of the Vertic Melanic Soils (cracks, or a COLE
# with blocky or prismatic peds), with no condition on redox segregations.
vertic_subgroup_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon)
  v <- .nzsc_vertic_cracks_cole(x)
  .nzsc_subgroup("vertic_subgroup_nzsc", v$passed, "E, EPV", list(cracks = v$cracks, cole = v$cole),
                 c("cracks_width_cm", "cracks_depth_cm", "cole_value", "structure_grade"))
}

# ERW: a weathered-B or cutanic horizon (adjacent rows of either) 10 cm or
# more thick.
weathered_or_cutanic_nzsc <- function(pedon, min_cm = 10) {
  x <- .nzsc_horizons(pedon)
  ok <- .any3v(.nzsc_weathered_b_rows(x), .nzsc_cutanic_or_coated(x))
  .nzsc_subgroup("weathered_or_cutanic_nzsc", .nzsc_run_any(x, ok, as.numeric(min_cm), strict = FALSE), "E, ERW",
                 missing = c("designation", "structure_grade", "munsell_hue_moist"), layers = which(ok %in% TRUE))
}

# EML: P retention 85 % or more, or a strong reactive-aluminium test, in some
# part of the B or BC horizon within `depth` cm; a measured P retention
# decides, as for allophanic soil material (Hugo's decision, 2026-10-28).
allophanic_b_subgroup_nzsc <- function(pedon, depth = 90) {
  x <- .nzsc_horizons(pedon); h <- x$h; n <- x$n
  miss <- c("phosphate_retention_pct", "naf_test")
  pre <- .nzsc_sub_ms_or_na(x, "allophanic_b_subgroup_nzsc", "E, EML", miss); if (!is.null(pre)) return(pre)
  pr <- h$phosphate_retention_pct %||% rep(NA_real_, n)
  naf <- .nzsc_class_index(h$naf_test %||% rep(NA, n), "naf")
  ok <- ifelse(!is.na(pr), pr >= 85, ifelse(is.na(naf), NA, naf >= 3))
  .nzsc_subgroup("allophanic_b_subgroup_nzsc", .nzsc_b_bc_some_part(x, ok, x$ms + as.numeric(depth)), "E, EML",
                 missing = miss)
}

# EODC: a moderately or strongly pedal weathered-B horizon 15 cm or more
# thick immediately beneath the A horizon. A mineral row of unknown role
# directly under the A may be A too: both bases are tried, and the verdict
# holds only when they agree.
pedal_weathered_b_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h
  miss <- c("designation", "structure_grade")
  ab <- .nzsc_a_bases(x)
  g <- tolower(as.character(h$structure_grade))
  grade <- ifelse(grepl("moderate|strong", g), TRUE, ifelse(is.na(h$structure_grade), NA, FALSE))
  ok <- .and3v(.nzsc_weathered_b_rows(x), grade)
  at <- function(z) if (is.na(z)) NA else if (!is.finite(z)) FALSE else .nzsc_run_starting(x, ok, z, z, 15, strict = FALSE)
  v1 <- at(ab[1L]); v2 <- at(ab[2L])
  .nzsc_subgroup("pedal_weathered_b_nzsc", if (identical(v1, v2)) v1 else NA, "E, EODC", list(a_base = ab), miss,
                 which(ok %in% TRUE))
}
