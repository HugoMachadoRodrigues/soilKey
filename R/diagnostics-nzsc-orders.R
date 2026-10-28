# =============================================================================
# v0.9.293 -- the key to soil orders of the New Zealand Soil Classification,
# 4th ed. (Key to Soil Orders, in its order: O, G, U, Z, L, M, E, S, X, N, P,
# B, A, R, W). Each order after the first is "other soils that ...", so a
# gate tests only its own criteria: the key's order supplies the
# exclusions. Three-valued; readings in inst/rules/nzsc4/readings.yaml.
# =============================================================================

# Thickness of the rows where `ok` holds inside [zt, zb]: c(sure, possible).
# The part of the window no row covers may be anything, unless the
# description ends on rock or a hard layer above it.
.nzsc_cum <- function(x, ok, zt, zb) {
  h <- x$h
  if (is.na(zt) || is.na(zb) || zb <= zt) return(c(0, 0))
  rows <- which(!is.na(h$top_cm) & !x$trailing)
  top <- h$top_cm[rows]
  # a row with no bottom depth reaches anywhere below its top
  bot <- ifelse(is.na(h$bottom_cm[rows]), Inf, h$bottom_cm[rows]); o <- ok[rows]
  open_bot <- is.na(h$bottom_cm[rows])
  # the window cut at every row boundary: a segment counts once, sure when
  # every row over it holds, possible when none fails; no row, possible
  # (unless the description ends on rock or a hard layer above it)
  cuts <- sort(unique(c(zt, zb, top[top > zt & top < zb], bot[is.finite(bot) & bot > zt & bot < zb])))
  hard_end <- .nzsc_ends_hard(x)
  last_bot <- suppressWarnings(max(bot, na.rm = TRUE))
  sure <- 0; poss <- 0
  for (k in seq_len(length(cuts) - 1L)) {
    a <- cuts[k]; b <- cuts[k + 1L]; mid <- (a + b) / 2
    v <- o[top <= mid & bot > mid]
    if (!length(v)) { if (!(hard_end && mid > last_bot)) poss <- poss + (b - a); next }
    if (any(v %in% FALSE) && !any(open_bot[top <= mid & bot > mid])) next
    if (all(v %in% TRUE)) sure <- sure + (b - a)
    if (!any(v %in% FALSE) || any(open_bot[top <= mid & bot > mid])) poss <- poss + (b - a)
  }
  c(sure, poss)
}

.nzsc_max <- function(v) { v <- v[!is.na(v)]; if (length(v)) max(v) else NA_real_ }
.nzsc_at_least <- function(v, cm) if (v[1L] >= cm) TRUE else if (v[2L] < cm) FALSE else NA

.nzsc_ends_hard <- function(x) {
  h <- x$h; ok <- which(!is.na(h$top_cm) & !x$trailing)
  if (!length(ok)) return(FALSE)
  isTRUE(x$hard[ok[which.max(h$top_cm[ok])]])
}

# Any row in `rows` where ok is TRUE / FALSE everywhere / NA.
.nzsc_any <- function(v) if (any(v %in% TRUE)) TRUE else if (anyNA(v)) NA else FALSE

# "In some part" (Diagnostics): in some subhorizon 10 cm or more thick
# within `rows` (adjacent rows where `ok` holds count together).
.nzsc_some_part <- function(x, rows, ok) {
  h <- x$h
  rows <- rows[!is.na(h$top_cm[rows])]
  if (!length(rows)) return(FALSE)
  rows <- rows[order(h$top_cm[rows])]
  best <- c(0, 0); cur <- c(0, 0); prev <- NA_real_
  for (i in rows) {
    th <- x$thick[i]; th <- if (is.na(th)) Inf else th
    if (!is.na(prev) && abs(h$top_cm[i] - prev) > 0.5) cur <- c(0, 0)
    if (ok[i] %in% FALSE) cur <- c(0, 0)
    else cur <- c(if (ok[i] %in% TRUE && cur[1L] == cur[2L]) cur[1L] + th else cur[1L], cur[2L] + th)
    best <- pmax(best, cur); prev <- h$bottom_cm[i]
  }
  # a horizon (or set of rows) thinner than 10 cm in all is read whole
  tot <- sum(ifelse(is.na(x$thick[rows]), Inf, pmax(0, x$thick[rows])))
  need <- min(10, tot)
  if (best[1L] >= need && best[1L] > 0) TRUE else if (best[2L] >= need && best[2L] > 0) NA else FALSE
}

# Mineral rows of unknown role (no designation, or one that cannot be read):
# any of them may be a B horizon.
.nzsc_maybe_rows <- function(x) which(x$unknown_role & x$mineral & !is.na(x$h$top_cm))
.nzsc_none <- function(x) if (length(.nzsc_maybe_rows(x))) NA else FALSE

# The base of the A horizon: the bottom of the A horizons (role A, not
# transitional) from the mineral soil surface down; the mineral soil surface
# itself when there is no A there.
.nzsc_a_base <- function(x) {
  h <- x$h; ms <- x$ms
  if (is.na(ms)) return(NA_real_)
  base <- ms
  for (i in x$ord) {
    if (is.na(h$top_cm[i]) || h$top_cm[i] < ms - 0.5) next
    if (abs(h$top_cm[i] - base) > 0.5) break
    if (!(x$role[i] %in% "A") || x$des$buried[i]) break
    if (is.na(h$bottom_cm[i])) return(NA_real_)
    base <- h$bottom_cm[i]
  }
  base
}

# B horizon rows (role B, not buried, not a pan).
.nzsc_b_rows <- function(x) which(.nzsc_is(x, c("B")) & !x$des$m & !is.na(x$h$top_cm))

# The uppermost subhorizon of the B horizon 10 cm or more thick (NA when
# there is none, or when a thinner one above leaves the thickness unknown).
.nzsc_upper_b <- function(x) {
  b <- .nzsc_b_rows(x)
  if (!length(b)) return(NA_integer_)
  b <- b[order(x$h$top_cm[b])]
  ok <- b[!is.na(x$thick[b]) & x$thick[b] >= 10]
  if (length(ok)) ok[1L] else NA_integer_
}

# ---- O: Organic Soils ----------------------------------------------------------------

organic_soils_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h
  s0 <- x$soil_top
  dr <- tolower(as.character(.nzsc_site1(pedon, "drainage_class") %|NA|% .nzsc_site1(pedon, "drainage")))
  wet_site <- if (is.na(dr)) NA else if (grepl("very poor|^poor", dr)) TRUE
              else if (grepl("well|rapid|excess|imperfect|somewhat", dr)) FALSE else NA
  # accumulated under wet conditions: an O horizon; not an L horizon; an
  # undesignated organic horizon by the site drainage
  # (an O of another notation, as USDA's Oi/Oe/Oa, may be a forest floor:
  # read by the site drainage as well)
  nz <- isTRUE(x$des$notation$nz)
  wet <- ifelse(x$role %in% "O", if (nz) TRUE else wet_site, ifelse(x$role %in% "L", FALSE,
                ifelse(x$organic %in% TRUE, wet_site, FALSE)))
  o_ok <- .and3v(x$organic, wet)
  lmh <- ifelse(x$role %in% "L", ifelse(x$des$decomp %in% c("mesic", "humified"), TRUE,
                                         ifelse(x$des$decomp %in% "fibric", FALSE, NA)),
                ifelse(x$unknown_role & x$organic %in% TRUE, NA, FALSE))
  p1 <- .nzsc_at_least(.nzsc_cum(x, o_ok, s0, s0 + 60), 30)
  p2 <- .nzsc_at_least(.nzsc_run_max(x, lmh, s0, s0 + 60), 35)
  passed <- .any3(c(p1, p2))
  .nzsc_result("organic_soils_nzsc", passed, which(o_ok %in% TRUE | lmh %in% TRUE),
               list(wet_peat = p1, decomposed_litter = p2),
               if (is.na(passed)) c("designation", "oc_pct", "site$drainage_class") else character(0),
               "Key to Soil Orders, O; Diagnostics, organic soil material")
}

# ---- G: Gley Soils --------------------------------------------------------------------

gley_soils_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; ms <- x$ms
  miss <- c("designation", "munsell_value_moist", "munsell_chroma_moist")
  if (is.na(ms)) return(.nzsc_result("gley_soils_nzsc", NA, missing = miss, where = "Key to Soil Orders, G"))
  red <- .nzsc_reductimorphic_rows(x)
  rows <- x$ord[!x$trailing[x$ord] & !is.na(h$top_cm[x$ord])]
  ts <- .nzsc_distinct_topsoil_rows(x)
  # (c) a distinct topsoil at the surface or buried within 60 cm
  c_ok <- .nzsc_any(ts[!is.na(h$top_cm) & h$top_cm < ms + 60])
  # (d) no moderately or very fluid horizon within 30 cm
  d_ok <- !any(.nzsc_fluid_rows(x)[!is.na(h$top_cm) & h$top_cm < ms + 30])
  # (e) less than 35 cm of allophanic soil material within 60 cm
  al <- .nzsc_cum(x, .nzsc_allophanic_rows(x), ms, ms + 60)
  e_ok <- if (al[2L] < 35) TRUE else if (al[1L] >= 35) FALSE else NA
  # (b) fragipans whose matrix chroma is 3 or more (a recorded fragipan of
  # unrecorded chroma may be one), duripans, podzolic-B horizons
  chr <- h$munsell_chroma_moist
  frag <- ifelse(.nzsc_pan_rows(x, "fragipan"), ifelse(is.na(chr), NA, chr >= 3), FALSE)
  duri <- .nzsc_pan_rows(x, "duripan"); podz <- .nzsc_podzolic_b_rows(x)
  # the base of the B horizon (of the present soil)
  b_base <- suppressWarnings(max(h$bottom_cm[.nzsc_is(x, "B")], na.rm = TRUE))
  # the gley profile form: a reductimorphic horizon with its top less than
  # 30 cm from the mineral soil surface
  cand <- rows[h$top_cm[rows] < ms + 30 & !(red[rows] %in% FALSE) & h$top_cm[rows] >= ms - 0.5]
  verdict <- function(r) {
    # the reductimorphic horizon from row r down
    run <- integer(0); prev <- h$top_cm[r]; sure <- TRUE
    for (i in rows[h$top_cm[rows] >= h$top_cm[r] - 0.5]) {
      if (abs(h$top_cm[i] - prev) > 0.5 || red[i] %in% FALSE) break
      if (is.na(red[i])) sure <- FALSE
      run <- c(run, i); prev <- h$bottom_cm[i]
      if (is.na(prev)) break
    }
    if (!length(run)) return(FALSE)
    low <- prev
    # (a) its lower boundary 90 cm or more down; shallower, over a lithic or
    # paralithic contact, or over a permeable sandy-skeletal layer reaching
    # 90 cm, or reaching the base of the B horizon (Hugo's decision,
    # 2026-10-28: the last alternative belongs to the reductimorphic horizon)
    nxt <- rows[!is.na(low) & abs(h$top_cm[rows] - low) <= 0.5]
    skel <- function(i) x$tex$group[i] %in% "sandy" & !is.na(h$coarse_fragments_pct[i]) & h$coarse_fragments_pct[i] > 35
    skel_unknown <- function(i) !(x$tex$group[i] %in% c("loamy", "clayey")) &&
                                !(!is.na(h$coarse_fragments_pct[i]) && h$coarse_fragments_pct[i] <= 35) && !skel(i)
    a_ok <- if (is.na(low)) NA else if (low >= ms + 90) TRUE
            else if (is.finite(b_base) && low >= b_base - 0.5) TRUE
            else if (length(nxt) && (x$role[nxt[1L]] %in% c("R", "CR"))) TRUE
            else if (length(nxt) && skel(nxt[1L])) {
              # the sandy-skeletal layer: adjacent such rows from there
              z <- low; ok <- FALSE
              for (i in rows[h$top_cm[rows] >= low - 0.5]) {
                if (abs(h$top_cm[i] - z) > 0.5 || !skel(i)) break
                z <- h$bottom_cm[i]; if (is.na(z)) break
                if (z >= ms + 90) { ok <- TRUE; break }
              }
              ok
            } else if (!length(nxt)) { if (.nzsc_ends_hard(x)) TRUE else NA }
            else if (skel_unknown(nxt[1L])) NA else FALSE
    # (b) nothing of the kind below it
    below <- rows[h$top_cm[rows] >= h$top_cm[r]]
    b_ok <- !.nzsc_any(c(frag[below], duri[below], podz[below]))
    v <- .and3(c(a_ok, b_ok, c_ok, d_ok, e_ok))
    if ((!sure || is.na(red[r])) && isTRUE(v)) NA else v
  }
  route1 <- if (!length(cand)) FALSE else .nzsc_any(vapply(cand, verdict, NA))
  # 2: an ironstone-pan within 30 cm and a positive Childs' test
  ip <- .nzsc_pan_rows(x, "ironstone_pan") & !is.na(h$top_cm) & h$top_cm <= ms + 30
  childs <- tolower(as.character(h$childs_test %||% rep(NA_character_, x$n)))
  cpos <- .nzsc_any(ifelse(is.na(childs) | !nzchar(childs), NA, grepl("^pos|^yes|^true|^\\+", childs)))
  route2 <- if (any(ip)) cpos else FALSE
  passed <- .any3(c(route1, route2))
  .nzsc_result("gley_soils_nzsc", passed, which(red %in% TRUE),
               list(gley_profile_form = route1, ironstone = route2, distinct_topsoil = c_ok),
               if (is.na(passed)) c(miss, "childs_test", "phosphate_retention_pct") else character(0),
               "Key to Soil Orders, G; Diagnostics, gley profile form, reductimorphic horizon")
}

# ---- U: Ultic Soils --------------------------------------------------------------------

ultic_soils_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; ms <- x$ms; n <- x$n
  miss <- c("ph_h2o", "designation", "clay_pct", "silt_pct", "clay_films_amount", "strength_moist")
  if (is.na(ms)) return(.nzsc_result("ultic_soils_nzsc", NA, missing = miss, where = "Key to Soil Orders, U"))
  ab <- .nzsc_a_base(x)
  # 1. pH below 5.5 in the major part from the base of the A to 60 cm
  c1 <- if (is.na(ab)) NA else if (ab >= ms + 60) FALSE
        else .asc_major_part(x, x$ord, ifelse(is.na(h$ph_h2o), NA, h$ph_h2o < 5.5), ab, ms + 60)
  # 2. the B horizon, in its major part: pedal with coatings on 10 % or more
  # of ped faces; slightly firm or stronger moist (unless sandy loam or sandy
  # clay loam); less silt than clay; loamy or clayey; and its base below 100 cm
  b <- .nzsc_b_rows(x)
  g <- tolower(as.character(h$structure_grade)); t <- tolower(as.character(h$structure_type))
  pedal <- ifelse(grepl("weak|moderate|strong", g) & !grepl("massive|single", t), TRUE,
           ifelse(grepl("structureless|apedal", g) | grepl("massive|single", t), FALSE, NA))
  films <- tolower(as.character(h$clay_films_amount %||% rep(NA, n)))
  coat <- ifelse(grepl("common|many|continuous|abundant", films), TRUE,
          ifelse(grepl("very few|none|^no|absent", films), FALSE,
          ifelse(grepl("few", films), NA, ifelse(x$des$t | x$des$h_weak, TRUE, NA))))
  str <- .nzsc_class_index(h$strength_moist %||% rep(NA, n), "strength")
  sl <- x$tex$class %in% c("sandy loam", "sandy clay loam")
  strong <- ifelse(sl, TRUE, ifelse(is.na(str), NA, str >= 3))
  # less silt than clay, unless there are weathered rock fragments (their
  # weathering is not recorded: rock fragments leave it open)
  cf <- h$coarse_fragments_pct
  silt_lt <- ifelse(is.na(h$silt_pct) | is.na(h$clay_pct), NA,
             ifelse(h$silt_pct < h$clay_pct, TRUE, ifelse(!is.na(cf) & cf > 0, NA, FALSE)))
  group <- ifelse(is.na(x$tex$group), NA, x$tex$group %in% c("loamy", "clayey"))
  ok_b <- .and3v(.and3v(.and3v(pedal, coat), .and3v(strong, silt_lt)), group)
  b_top <- if (length(b)) min(h$top_cm[b]) else NA
  b_bot <- if (length(b) && !anyNA(h$bottom_cm[b])) max(h$bottom_cm[b]) else NA
  c2a <- if (!length(b)) .nzsc_none(x) else .asc_major_part(x, b, ok_b, b_top, if (is.na(b_bot)) b_top + 1 else b_bot)
  c2e <- if (!length(b)) .nzsc_none(x) else if (is.na(b_bot)) { if (.nzsc_max(h$top_cm[b]) > ms + 100) TRUE else NA }
         else if (b_bot > ms + 100) TRUE
         else if (b_bot >= .nzsc_max(h$bottom_cm[x$ord]) - 0.5 && !.nzsc_ends_hard(x)) NA else FALSE
  # 3. no oxidic or cutanoxidic horizon, no allophanic soil material 35 cm or
  # more within 60 cm, no podzolic-B (strongly indurated non-quartz fragments
  # are not recorded and are read as absent)
  ox <- .nzsc_any(.any3v(.nzsc_oxidic_rows(x), .nzsc_cutanoxidic_rows(x)))
  al <- .nzsc_at_least(.nzsc_cum(x, .nzsc_allophanic_rows(x), ms, ms + 60), 35)
  pz <- .nzsc_any(.nzsc_podzolic_b_rows(x))
  c3 <- .and3(c(!ox, !al, !pz))
  # 4. an eluvial horizon, or value 5 or more in the uppermost B subhorizon
  el <- .nzsc_any(.nzsc_eluvial_rows(x))
  up_v <- if (length(b)) h$munsell_value_moist[b[which.min(h$top_cm[b])]] else if (length(.nzsc_maybe_rows(x))) NA else -Inf
  c4 <- .any3(c(el, if (is.na(up_v)) NA else up_v >= 5))
  passed <- .and3(c(c1, c2a, c2e, c3, c4))
  .nzsc_result("ultic_soils_nzsc", passed, b, list(acid = c1, b_horizon = c2a, b_base_below_100 = c2e,
               exclusions = c3, eluvial_or_pale_b = c4),
               if (is.na(passed)) miss else character(0), "Key to Soil Orders, U")
}

# ---- Z: Podzol Soils ---------------------------------------------------------------------

podzol_soils_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h
  pz <- .nzsc_podzolic_b_rows(x); os <- .nzsc_pan_rows(x, "ortstein"); hp <- .nzsc_pan_rows(x, "humus_pan")
  # a podzolic-B horizon or an ortstein-pan with pH below 5.5 in some part
  cand <- which(pz %in% TRUE | os)
  maybe <- which(is.na(pz))
  acid <- function(rows) .nzsc_some_part(x, rows, ifelse(is.na(h$ph_h2o), NA, h$ph_h2o < 5.5))
  r1 <- if (length(cand)) acid(cand) else FALSE
  if (isFALSE(r1) && length(maybe)) r1 <- if (isFALSE(acid(maybe))) FALSE else NA
  passed <- .any3(c(r1, any(hp)))
  .nzsc_result("podzol_soils_nzsc", passed, c(cand, which(hp)), list(podzolic_b = r1, humus_pan = any(hp)),
               if (is.na(passed)) c("designation", "ph_h2o", "naf_test") else character(0),
               "Key to Soil Orders, Z; Diagnostics, podzolic-B horizon, ortstein-pan, humus-pan")
}

# ---- L: Allophanic Soils ------------------------------------------------------------------

allophanic_soils_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; ms <- x$ms
  miss <- c("phosphate_retention_pct", "bulk_density_g_cm3", "naf_test", "sensitivity_class")
  if (is.na(ms)) return(.nzsc_result("allophanic_soils_nzsc", NA, missing = miss, where = "Key to Soil Orders, L"))
  al <- .nzsc_allophanic_rows(x)
  contact <- which((x$role %in% c("R", "CR")) & !is.na(h$top_cm))
  ct <- if (length(contact)) min(h$top_cm[contact]) else NA
  passed <- if (!is.na(ct) && ct < ms + 35) {
    above <- x$ord[x$mineral[x$ord] & !is.na(h$top_cm[x$ord]) & h$top_cm[x$ord] >= ms - 0.5 & h$top_cm[x$ord] < ct]
    bw <- .nzsc_cum(x, .nzsc_weathered_b_rows(x), ms, ct)
    .and3(c(if (length(above)) .and3(al[above]) else FALSE, .nzsc_at_least(bw, 5)))
  } else .nzsc_at_least(.nzsc_cum(x, al, ms, ms + 60), 35)
  .nzsc_result("allophanic_soils_nzsc", passed, which(al %in% TRUE), list(contact_cm = ct),
               if (is.na(passed)) miss else character(0), "Key to Soil Orders, L; Diagnostics, allophanic soil material")
}

# ---- M: Pumice Soils ---------------------------------------------------------------------------

# The thickest run of adjacent rows where `ok` holds, inside [zt, zb]:
# c(sure, possible): the sure run counts only rows where it holds, the
# possible one also rows where it may; a gap in the record ends both, a row
# with no bottom depth may go on to zb. With from_top, only the run from zt.
.nzsc_run_max <- function(x, ok, zt, zb, from_top = FALSE) {
  h <- x$h
  rows <- x$ord[!x$trailing[x$ord]]
  bs <- 0; bp <- 0; cs <- 0; cp <- 0; prev <- NA_real_; started <- FALSE
  for (i in rows) {
    bot <- if (is.na(h$bottom_cm[i])) zb else h$bottom_cm[i]
    w <- .asc_overlap(h$top_cm[i], bot, zt, zb)
    if (w <= 0) { if (h$top_cm[i] >= zb) break; next }
    gap <- if (is.na(prev)) (from_top && h$top_cm[i] > zt + 0.5) else abs(h$top_cm[i] - prev) > 0.5
    if (gap) { if (from_top) break; cs <- 0; cp <- 0 }
    if (ok[i] %in% FALSE) { if (from_top) break; cs <- 0; cp <- 0 }
    else {
      cp <- cp + w
      cs <- if (ok[i] %in% TRUE) cs + w else 0
      if (from_top && !(ok[i] %in% TRUE)) cs <- -Inf   # the sure run from the top is over
    }
    bs <- max(bs, cs); bp <- max(bp, cp)
    prev <- bot
    if (is.na(h$bottom_cm[i])) break
  }
  c(max(0, bs), bp)
}

pumice_soils_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; ms <- x$ms
  miss <- c("coarse_fragments_pct", "rock_origin", "sand_pct", "volcanic_glass_pct")
  if (is.na(ms)) return(.nzsc_result("pumice_soils_nzsc", NA, missing = miss, where = "Key to Soil Orders, M"))
  vt <- .nzsc_vitric_rows(x)
  top_run <- .nzsc_run_max(x, vt, ms, ms + 200, from_top = TRUE)
  any_run <- .nzsc_run_max(x, vt, ms, ms + 60)
  c1 <- .any3(c(.nzsc_at_least(top_run, 25), .nzsc_at_least(any_run, 35)))
  bw <- .nzsc_at_least(.nzsc_cum(x, .nzsc_weathered_b_rows(x), ms, ms + 500), 5)
  passed <- .and3(c(c1, bw))
  .nzsc_result("pumice_soils_nzsc", passed, which(vt %in% TRUE), list(vitric = c1, weathered_b = bw),
               if (is.na(passed)) miss else character(0), "Key to Soil Orders, M; Diagnostics, vitric soil material")
}

# ---- E: Melanic Soils --------------------------------------------------------------------------

# The parent material is calcareous, mafic or ultramafic, by a recorded rock
# or layer origin (or site$parent_material).
.nzsc_melanic_parent <- function(pedon, x) {
  txt <- tolower(paste(c(pedon$site$parent_material %||% NA, x$h$rock_origin %||% NA,
                         x$h$layer_origin %||% NA), collapse = " "))
  txt <- gsub("non[- ]?calcareous|not calcareous|non[- ]?basaltic", " ", txt)
  if (!grepl("[a-z]", gsub("\\bna\\b", "", txt))) return(NA)
  yes <- grepl(paste0("limestone|marl|calcare|chalk|dolomit|coquina|shell|basalt|basic|mafic|dolerite|",
                      "gabbro|scoria|ultramafic|serpentin|peridot|dunite|ophiolit"), txt)
  if (yes) TRUE else FALSE
}

melanic_soils_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; ms <- x$ms; n <- x$n
  miss <- c("rock_origin", "munsell_value_moist", "structure_grade", "ph_h2o", "stickiness", "effervescence_class")
  if (is.na(ms)) return(.nzsc_result("melanic_soils_nzsc", NA, missing = miss, where = "Key to Soil Orders, E"))
  c1 <- .nzsc_melanic_parent(pedon, x)
  # 2. an A horizon, in its major part, value 3 or less and well structured
  a <- which(.nzsc_is(x, "A") & !is.na(h$top_cm))
  g <- tolower(as.character(h$structure_grade)); t <- tolower(as.character(h$structure_type))
  size <- h$ped_size_max_mm %||% rep(NA_real_, n)
  pedal_ms <- ifelse(grepl("moderate|strong", g), ifelse(is.na(size), TRUE, size < 60),
              ifelse(grepl("weak|structureless|apedal", g), FALSE, NA))
  earthy <- grepl("earthy", t)
  cracks <- ifelse(!is.na(h$cracks_width_cm) & h$cracks_width_cm >= 0.4,
                   ifelse(is.na(h$cracks_depth_cm), NA, h$cracks_depth_cm >= 30), FALSE)
  peaty <- x$tex$class %in% "peat"
  struct <- ifelse(pedal_ms %in% TRUE | earthy | cracks %in% TRUE | peaty, TRUE,
                   ifelse(pedal_ms %in% FALSE & cracks %in% FALSE, FALSE, NA))
  dark <- ifelse(is.na(h$munsell_value_moist), NA, h$munsell_value_moist <= 3)
  a_bot <- .nzsc_max(h$bottom_cm[a])
  c2 <- if (!length(a)) .nzsc_none(x) else if (is.na(a_bot)) NA
        else .asc_major_part(x, a, .and3v(dark, struct), min(h$top_cm[a]), a_bot)
  # 3. HCl reaction within 60 cm, or a B (weathered-B, argillic or cutanic,
  # more than 10 cm) whose major part to 60 cm is pH 5.9 or more, moderately
  # or strongly pedal and moderately or very sticky; or below pH 5.9 with
  # value 4 or less, moderate to strong polyhedral peds and as sticky
  # a visible reaction to HCl in the matrix: effervescence, a k, or CaCO3;
  # segregations alone do not show the matrix, so they leave it open
  calc <- .nzsc_calcareous_rows(x)
  eff <- tolower(as.character(h$effervescence_class %||% rep(NA_character_, n)))
  matrix_hcl <- ifelse(calc %in% TRUE & !(grepl("slight|moderate|strong|very|violent", eff) & !grepl("^non|^none", eff)) &
                       !x$des$k & (is.na(h$caco3_pct) | h$caco3_pct == 0), NA, calc)
  c3a <- .nzsc_any(matrix_hcl[!is.na(h$top_cm) & h$top_cm <= ms + 60 & x$mineral])
  bkind <- .any3v(.nzsc_weathered_b_rows(x), .any3v(.nzsc_argillic_rows(x), .nzsc_cutanic_rows(x)))
  b <- which(bkind %in% TRUE & !is.na(h$top_cm))
  stk <- .nzsc_class_index(h$stickiness %||% rep(NA, n), "stickiness")
  sticky <- ifelse(is.na(stk), NA, stk >= 2)
  ms_ped <- ifelse(grepl("moderate|strong", g), TRUE, ifelse(grepl("weak|structureless", g), FALSE, NA))
  poly <- ifelse(grepl("polyhed", t), TRUE, ifelse(is.na(h$structure_type), NA, FALSE))
  hi_ph <- .and3v(ms_ped, sticky)
  lo_ph <- .and3v(.and3v(ifelse(is.na(h$munsell_value_moist), NA, h$munsell_value_moist <= 4), .and3v(ms_ped, poly)), sticky)
  okb <- ifelse(is.na(h$ph_h2o), NA, ifelse(h$ph_h2o >= 5.9, hi_ph, lo_ph))
  b_th <- if (length(b)) sum(x$thick[b], na.rm = TRUE) else 0
  c3b <- if (!length(b) || b_th <= 10) FALSE else {
    zt <- min(h$top_cm[b]); zb <- min(ms + 60, .nzsc_max(h$bottom_cm[b]), na.rm = TRUE)
    if (zb <= zt) FALSE else .asc_major_part(x, b, okb, zt, zb)
  }
  # horizons that may be such a B leave a failed reading open
  if (isFALSE(c3b) && any(is.na(bkind) & !is.na(h$top_cm))) c3b <- NA
  c3 <- .any3(c(c3a, c3b))
  # 4. in some part of the B to its base or 90 cm: rock fragments (their
  # weathering not recorded: present counts), a subhorizon not clayey, or
  # value 5 or more in the uppermost subhorizon
  bb <- .nzsc_b_rows(x); bb <- bb[h$top_cm[bb] < ms + 90]
  c4 <- if (!length(bb)) .nzsc_none(x) else .any3(c(
    .nzsc_some_part(x, bb, ifelse(is.na(h$coarse_fragments_pct), NA, h$coarse_fragments_pct > 0)),
    .nzsc_some_part(x, bb, ifelse(is.na(x$tex$group), NA, x$tex$group != "clayey")),
    { v <- h$munsell_value_moist[bb[which.min(h$top_cm[bb])]]; if (is.na(v)) NA else v >= 5 }))
  passed <- .and3(c(c1, c2, c3, c4))
  .nzsc_result("melanic_soils_nzsc", passed, a, list(parent = c1, dark_a = c2, calcareous_or_b = c3, b_part = c4),
               if (is.na(passed)) miss else character(0), "Key to Soil Orders, E")
}

# ---- S: Semiarid Soils -------------------------------------------------------------------------

semiarid_soils_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; s0 <- x$soil_top; ms <- x$ms; n <- x$n
  miss <- c("designation", "secondary_carbonates_pct", "phosphate_retention_pct", "ph_h2o")
  if (is.na(ms)) return(.nzsc_result("semiarid_soils_nzsc", NA, missing = miss, where = "Key to Soil Orders, S"))
  c1 <- !any(.nzsc_pan_rows(x, "fragipan"))
  sandy <- .nzsc_any(x$tex$group[x$mineral] %in% "sandy") %in% TRUE &&
           mean(x$tex$group[x$mineral] %in% "sandy") > 0.5
  depth <- if (sandy) 150 else 90
  calc <- .nzsc_calcareous_rows(x)
  seg <- h$secondary_carbonates_pct
  # evidence of pedogenic carbonate: recorded carbonate segregations (a k
  # marks any calcareous horizon, primary or secondary, and leaves it open)
  ped <- ifelse(!is.na(seg) & seg > 0, TRUE, ifelse(calc %in% FALSE, FALSE, NA))
  within <- !is.na(h$top_cm) & h$top_cm < s0 + depth
  c2a <- .nzsc_any(ped[within & x$mineral])
  # no primary carbonate within 90 cm: carbonate there with no sign of
  # pedogenic origin may be primary
  w90 <- !is.na(h$top_cm) & h$top_cm < s0 + 90 & x$mineral
  no_prim <- if (!any(calc[w90] %in% TRUE | is.na(calc[w90]))) TRUE
             else if (all(ped[w90 & !(calc %in% FALSE)] %in% TRUE)) TRUE else NA
  bw <- .nzsc_at_least(.nzsc_cum(x, .nzsc_weathered_b_rows(x), ms, ms + 500), 10)
  b <- .nzsc_b_rows(x); b <- b[h$top_cm[b] < ms + 60]
  pr <- h$phosphate_retention_pct %||% rep(NA_real_, n)
  pr_ok <- if (!length(b)) .nzsc_none(x) else .asc_major_part(x, b, ifelse(is.na(pr), NA, pr <= 15), min(h$top_cm[b]),
                                                     min(ms + 60, .nzsc_max(h$bottom_cm[b]), na.rm = TRUE))
  ph_ok <- .nzsc_any(ifelse(is.na(h$ph_h2o[within]), NA, h$ph_h2o[within] >= 7.5))
  c2b <- .and3(c(bw, no_prim, .any3(c(pr_ok, ph_ok))))
  passed <- .and3(c(c1, .any3(c(c2a, c2b))))
  .nzsc_result("semiarid_soils_nzsc", passed, which(ped %in% TRUE), list(pedogenic_carbonate = c2a, weathered_b = c2b),
               if (is.na(passed)) miss else character(0), "Key to Soil Orders, S")
}

# ---- X: Oxidic Soils ---------------------------------------------------------------------------

oxidic_soils_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; ms <- x$ms
  # an oxidic horizon, or an A horizon with its properties (or both), with a
  # lower boundary 30 cm or more down. An A's oxidic properties are not
  # recorded: an A with no reaction to the reactive-aluminium test recorded
  # may have them
  orows <- .nzsc_oxidic_rows(x)
  naf <- .nzsc_class_index(h$naf_test %||% rep(NA, x$n), "naf")
  a_ox <- ifelse(.nzsc_is(x, "A") & naf %in% 0, NA, FALSE)
  okr <- .any3v(orows, a_ox)
  # a horizon with no bottom depth reaches below its top
  low <- ifelse(is.na(h$bottom_cm), h$top_cm + 1e-6, h$bottom_cm)
  open <- is.na(h$bottom_cm)
  ox <- which(okr %in% TRUE & !is.na(low)); mx <- which(is.na(okr) & !is.na(low))
  passed <- if (is.na(ms)) NA
            else if (length(ox) && max(low[ox]) >= ms + 30) TRUE
            else if ((length(mx) && max(low[c(ox, mx)]) >= ms + 30) || any(open[c(ox, mx)])) NA else FALSE
  .nzsc_result("oxidic_soils_nzsc", passed, ox, list(),
               if (is.na(passed)) "designation" else character(0), "Key to Soil Orders, X; Diagnostics, oxidic horizon")
}

# ---- N: Granular Soils -------------------------------------------------------------------------

granular_soils_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; ms <- x$ms; n <- x$n
  miss <- c("designation", "clay_pct", "structure_type", "ped_size_max_mm")
  if (is.na(ms)) return(.nzsc_result("granular_soils_nzsc", NA, missing = miss, where = "Key to Soil Orders, N"))
  wb <- .nzsc_weathered_b_rows(x)
  bw <- which(wb %in% TRUE & !is.na(h$top_cm) & h$top_cm < ms + 90)
  bwm <- which(is.na(wb) & !is.na(h$top_cm) & h$top_cm < ms + 90)
  # 1. a weathered-B clayey throughout to its base or 90 cm (rock fragments
  # of unrecorded weathering leave it open; horizons that may be part of it
  # too)
  c1 <- if (!length(bw)) { if (length(bwm)) NA else FALSE } else .and3(c(
    .and3(ifelse(is.na(x$tex$group[c(bw, bwm)]), NA, x$tex$group[c(bw, bwm)] == "clayey")),
    .and3(ifelse(is.na(h$coarse_fragments_pct[bw]) | h$coarse_fragments_pct[bw] == 0, TRUE, NA))))
  if (isTRUE(c1) && length(bwm)) c1 <- NA
  # 2. a cutanoxidic horizon, or a moderately or strongly pedal cutanic or
  # argillic horizon, with a reductimorphic horizon within 15 cm of the base
  # of the A (or 30 cm of the surface), or polyhedral peds of 20 mm or less in
  # most of the B within 60 cm
  g <- tolower(as.character(h$structure_grade)); t <- tolower(as.character(h$structure_type))
  ms_ped <- ifelse(grepl("moderate|strong", g), TRUE, ifelse(grepl("weak|structureless", g), FALSE, NA))
  cut <- .any3v(.nzsc_cutanoxidic_rows(x), .and3v(.any3v(.nzsc_cutanic_rows(x), .nzsc_argillic_rows(x)), ms_ped))
  has_cut <- .nzsc_any(cut)
  red <- .nzsc_reductimorphic_rows(x); ab <- .nzsc_a_base(x)
  lim <- max(c(ab + 15, ms + 30), na.rm = TRUE)
  a2 <- .nzsc_any(red[!is.na(h$top_cm) & h$top_cm < lim & x$mineral])
  b <- .nzsc_b_rows(x); b <- b[h$top_cm[b] < ms + 60]
  size <- h$ped_size_max_mm %||% rep(NA_real_, n)
  fine_poly <- ifelse(grepl("polyhed", t), ifelse(is.na(size), NA, size <= 20), ifelse(is.na(h$structure_type), NA, FALSE))
  b2 <- if (!length(b)) .nzsc_none(x) else .asc_major_part(x, b, fine_poly, min(h$top_cm[b]),
                                                  min(ms + 60, .nzsc_max(h$bottom_cm[b]), na.rm = TRUE))
  passed <- .and3(c(c1, has_cut, .any3(c(a2, b2))))
  .nzsc_result("granular_soils_nzsc", passed, bw, list(clayey_bw = c1, cutanic = has_cut),
               if (is.na(passed)) miss else character(0), "Key to Soil Orders, N")
}

# ---- P: Pallic Soils ---------------------------------------------------------------------------

# Matrix colour classes of the Pallic and Brown keys (moist): pale (hue 10YR
# or yellower, chroma 3 or less, value 4 or more); intermediate (hue 10YR or
# yellower with chroma 4 or 5, or chroma 3 with value 3, or hue 2.5Y or
# yellower with chroma 6 or more); brown (hue 7.5YR or redder, or 10YR with
# chroma 6 or more).
.nzsc_colour_class <- function(h) {
  hue <- .nzsc_hue_num(h$munsell_hue_moist)
  # value and chroma to the nearest chip (continuous Munsell notations, as
  # from spectra, fall between them); a neutral hue (N) has chroma 0
  v <- round(h$munsell_value_moist); c <- round(h$munsell_chroma_moist)
  neutral <- grepl("^N", toupper(trimws(as.character(h$munsell_hue_moist))))
  c[neutral & is.na(c)] <- 0
  # 10YR = 20 on this scale; "10YR or yellower" runs to 10Y (30)
  yel <- (!is.na(hue) & hue >= 20 - 0.01 & hue <= 30 + 0.01) | neutral
  y25 <- !is.na(hue) & hue >= 22.5 - 0.01 & hue <= 30 + 0.01
  red <- !is.na(hue) & hue <= 17.5 + 0.01
  hue[neutral] <- 25
  ifelse(is.na(hue) | is.na(v) | is.na(c), NA_character_,
  ifelse(red | (hue >= 19.99 & hue <= 20.01 & c >= 6), "brown",
  ifelse(yel & c <= 3 & v >= 4, "pale",
  ifelse((yel & c %in% c(4, 5)) | (yel & c == 3 & v == 3) | (y25 & c >= 6), "intermediate", "other"))))
}

pallic_soils_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; ms <- x$ms; n <- x$n
  miss <- c("designation", "munsell_hue_moist", "phosphate_retention_pct", "structure_grade", "strength_moist")
  if (is.na(ms)) return(.nzsc_result("pallic_soils_nzsc", NA, missing = miss, where = "Key to Soil Orders, P"))
  ab <- .nzsc_a_base(x)
  bot_o <- ifelse(is.na(h$bottom_cm), Inf, h$bottom_cm)
  win <- x$ord[!is.na(h$top_cm[x$ord]) & x$mineral[x$ord] &
               bot_o[x$ord] > ab & h$top_cm[x$ord] < ms + 60 & !x$trailing[x$ord]]
  c0 <- if (is.na(ab) || ab >= ms + 60) FALSE else .nzsc_some_part(x, win, x$tex$fine_loamy_sand_or_finer)
  red <- .nzsc_reductimorphic_rows(x); el <- .nzsc_eluvial_rows(x)
  frag <- .nzsc_pan_rows(x, "fragipan"); duri <- .nzsc_pan_rows(x, "duripan"); arg <- .nzsc_argillic_rows(x)
  over <- function(up, down) {
    u <- which(up %in% TRUE); d <- which(down %in% TRUE)
    if (!length(u) || !length(d)) return(if (any(is.na(up)) && length(d)) NA else FALSE)
    any(outer(h$top_cm[u], h$top_cm[d], "<"), na.rm = TRUE)
  }
  r1 <- over(.any3v(red, el), frag | duri | (arg %in% TRUE))
  # 2(a) the matrix throughout from the base of the A to 60 cm pale, or
  # intermediate with P retention below 30 % in the uppermost B subhorizon
  cc <- .nzsc_colour_class(h)
  up_b <- .nzsc_upper_b(x)
  pr <- h$phosphate_retention_pct %||% rep(NA_real_, n)
  low_pr <- if (is.na(up_b)) NA else if (is.na(pr[up_b])) NA else pr[up_b] < 30
  row_ok <- ifelse(is.na(cc), NA, ifelse(cc == "pale", TRUE, ifelse(cc == "intermediate", low_pr, FALSE)))
  c2a <- if (!length(win)) FALSE else .and3(row_ok[win])
  # 2(b) a B or BC 10 cm or more thick with a fragipan, duripan or brittle-B;
  # or a weathered-B with a moderately or strongly pedal blocky, polyhedral or
  # prismatic subhorizon, slightly firm or stronger moist; or an argillic,
  # calcareous or cutanic horizon with sodic features in or under it; or an
  # eluvial horizon (skeletans are not recorded and are read as absent)
  bb <- which(.nzsc_is(x, c("B", "BC")) & !is.na(h$top_cm))
  bth <- if (length(bb)) sum(x$thick[bb], na.rm = TRUE) else 0
  bth_open <- length(bb) && anyNA(x$thick[bb])
  g <- tolower(as.character(h$structure_grade)); t <- tolower(as.character(h$structure_type))
  str <- .nzsc_class_index(h$strength_moist %||% rep(NA, n), "strength")
  ped_ok <- .and3v(ifelse(grepl("moderate|strong", g),
                          ifelse(grepl("block|polyhed|prism", t), TRUE, ifelse(is.na(h$structure_type), NA, FALSE)),
                          ifelse(grepl("weak|structureless", g), FALSE, NA)),
                   ifelse(is.na(str), NA, str >= 3))
  bw <- .nzsc_weathered_b_rows(x)
  i <- .nzsc_any(frag[bb] | duri[bb] | .nzsc_pan_rows(x, "brittle")[bb])
  ii <- .nzsc_any(.and3v(bw, ped_ok)[bb])
  sod <- .nzsc_sodic_rows(x); hcut <- .any3v(.any3v(arg, .nzsc_calcareous_rows(x) & x$role %in% c("B", "BC")),
                                              .nzsc_cutanic_rows(x))
  # sodic features in the horizon or in the one directly beneath it
  bel <- .nzsc_below(x)
  per <- vapply(seq_len(n), function(i) .any3(c(sod[i], if (is.na(bel[i])) NA else sod[bel[i]])), NA)
  iii <- .nzsc_any(.and3v(hcut, per))
  iv <- .nzsc_any(el)
  c2b <- if (bth < 10 && !bth_open) { if (!length(bb) && length(.nzsc_maybe_rows(x))) NA else FALSE }
         else { v <- .any3(c(i, ii, iii, iv)); if (bth < 10 && isTRUE(v)) NA else v }
  passed <- .and3(c(c0, .any3(c(r1, .and3(c(c2a, c2b))))))
  .nzsc_result("pallic_soils_nzsc", passed, win, list(fine_texture = c0, pan_route = r1, colour = c2a, b_horizon = c2b),
               if (is.na(passed)) miss else character(0), "Key to Soil Orders, P")
}

# ---- B: Brown Soils ----------------------------------------------------------------------------

brown_soils_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; ms <- x$ms; n <- x$n
  miss <- c("designation", "munsell_hue_moist", "phosphate_retention_pct", "naf_test")
  if (is.na(ms)) return(.nzsc_result("brown_soils_nzsc", NA, missing = miss, where = "Key to Soil Orders, B"))
  kind <- .any3v(.nzsc_weathered_b_rows(x), .any3v(.nzsc_argillic_rows(x), .nzsc_cutanic_rows(x)))
  kind[x$des$buried | x$des$prefix != ""] <- FALSE
  b <- which(kind %in% TRUE & !is.na(h$top_cm))
  th <- .nzsc_cum(x, kind, ms, ms + 500)
  low <- if (length(b)) .nzsc_max(ifelse(is.na(h$bottom_cm[b]), h$top_cm[b] + 1e-6, h$bottom_cm[b])) else NA
  c1 <- .and3(c(.nzsc_at_least(th, 10), if (is.na(low)) NA else if (low >= ms + 30) TRUE
                else if (anyNA(h$bottom_cm[b])) NA else FALSE))
  if (length(b) == 0L) c1 <- if (any(is.na(kind))) NA else FALSE
  cc <- .nzsc_colour_class(h)
  bb <- .nzsc_b_rows(x); up_b <- .nzsc_upper_b(x)
  pr <- h$phosphate_retention_pct %||% rep(NA_real_, n)
  naf <- .nzsc_class_index(h$naf_test %||% rep(NA, n), "naf")
  reactive <- if (is.na(up_b)) NA else .any3(c(if (is.na(pr[up_b])) NA else pr[up_b] >= 30,
                                               if (is.na(naf[up_b])) NA else naf[up_b] >= 2))
  sub_ok <- ifelse(is.na(cc), NA, ifelse(cc == "brown", TRUE, ifelse(cc == "intermediate", reactive, FALSE)))
  c2ab <- if (!length(bb)) .nzsc_none(x) else .nzsc_any(sub_ok[bb])
  sb <- bb[h$top_cm[bb] < ms + 60]
  c2c <- if (!length(sb)) .nzsc_none(x) else .and3(ifelse(is.na(x$tex$group[sb]), NA, x$tex$group[sb] == "sandy"))
  c2 <- .any3(c(c2ab, c2c))
  passed <- .and3(c(c1, c2))
  .nzsc_result("brown_soils_nzsc", passed, b, list(b_horizon = c1, colour_or_sandy = c2),
               if (is.na(passed)) miss else character(0), "Key to Soil Orders, B")
}

# ---- A: Anthropic Soils ------------------------------------------------------------------------

anthropic_soils_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; s0 <- x$soil_top; n <- x$n
  org <- tolower(paste(h$layer_origin %||% rep(NA, n)))
  art <- h$artefacts_pct %||% rep(NA_real_, n)
  anth <- x$des$anthropic | grepl("anthrop|\\bfill\\b|spoil|dredg|garbic|urbic|landfill|made ground|spolic|mine tailing", org) |
          (!is.na(art) & art > 0)
  th <- .nzsc_cum(x, anth, s0, s0 + 90)
  c1a <- th[1L] > 30
  trunc <- tolower(as.character(pedon$site$anthropic_truncation %||% NA)[1L]) %in% c("true", "t", "yes", "y", "1")
  c1 <- c1a || trunc
  # anthropic soil material mainly of manufactured materials (more than half
  # artefacts, thickness-weighted over it) must lie under 30 cm or more of soil
  ar <- which(anth & !is.na(h$top_cm))
  w <- pmax(0, x$thick[ar]); a_art <- ifelse(is.na(art[ar]), 0, art[ar])
  mainly <- length(ar) && sum(w, na.rm = TRUE) > 0 && sum(a_art * w, na.rm = TRUE) / sum(w, na.rm = TRUE) > 50
  c2 <- !mainly || min(h$top_cm[ar]) >= s0 + 30
  passed <- c1 && c2
  .nzsc_result("anthropic_soils_nzsc", passed, which(anth), list(anthropic_cm = th[1L], truncated = trunc),
               character(0), "Key to Soil Orders, A; Diagnostics, anthropic soil material")
}

# ---- R: Recent Soils ----------------------------------------------------------------------------

recent_soils_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; ms <- x$ms
  miss <- c("designation", "munsell_value_moist", "structure_grade")
  if (is.na(ms)) return(.nzsc_result("recent_soils_nzsc", NA, missing = miss, where = "Key to Soil Orders, R"))
  ts <- .nzsc_any(.nzsc_distinct_topsoil_rows(x)[!is.na(h$top_cm) & h$top_cm < ms + 60])
  g <- tolower(as.character(h$structure_grade))
  struct <- .nzsc_any(ifelse(grepl("moderate|strong", g), TRUE, ifelse(grepl("weak|structureless", g), FALSE, NA))[
    !is.na(h$top_cm) & h$top_cm < ms + 15 & x$mineral])
  bw <- .nzsc_any(.nzsc_weathered_b_rows(x))
  fluid <- any(.nzsc_fluid_rows(x)[!is.na(h$top_cm) & h$top_cm < ms + 30])
  passed <- .and3(c(.any3(c(ts, struct, bw)), !fluid))
  .nzsc_result("recent_soils_nzsc", passed, integer(0), list(distinct_topsoil = ts, structure = struct, weathered_b = bw),
               if (is.na(passed)) miss else character(0), "Key to Soil Orders, R")
}

# Overthickened A horizons (note to the key): a soil recorded as having an
# overthickened A horizon (site$overthickened_a; the term is not defined)
# with its base at 45 cm or more, less than 30 cm of anthropic soil material
# within 90 cm and no mottles within 30 cm enters the key at Recent Soils.
.nzsc_overthickened_entry <- function(pedon) {
  flag <- tolower(as.character(pedon$site$overthickened_a %||% NA)[1L])
  if (!(flag %in% c("true", "t", "yes", "y", "1"))) return(FALSE)
  x <- .nzsc_horizons(pedon); h <- x$h; ms <- x$ms
  ab <- .nzsc_a_base(x)
  if (is.na(ms) || is.na(ab) || ab < ms + 45) return(FALSE)
  if (isTRUE(anthropic_soils_nzsc(pedon)$evidence$anthropic_cm > 30)) return(FALSE)
  top30 <- !is.na(h$top_cm) & h$top_cm < ms + 30
  mott <- (x$des$g | x$des$g_weak | x$des$f_weak)[top30] |
          (!is.na(h$redoximorphic_features_pct) & h$redoximorphic_features_pct > 0)[top30]
  !any(mott)
}
