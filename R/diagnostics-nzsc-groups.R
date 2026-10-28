# =============================================================================
# v0.9.298 -- the keys to groups of the New Zealand Soil Classification, 4th
# ed. (one key per order, 77 groups). Each group after the first is "other
# soils that ...", so a gate tests only its own criteria; the last group of
# an order is the default. Three-valued; readings in
# inst/rules/nzsc4/readings.yaml. Hugo's decisions of 2026-10-28: features
# a describer records when met (sedimentary stratification, nodules,
# cracks, refuse or cultural layers, permafrost, surface water, mechanical
# mixing) are absent when not recorded; the pans the text names as often
# working as slowly permeable layers count as such; new attributes record
# what the groups read (packing, Ksat, penetration resistance, where the
# redox segregations are, sulfate S, roots, Maori cultural layer, coating
# colour; on the site, soil temperature at 30 cm, air temperature, surface
# water, methane odour, mechanical mixing depth).
# =============================================================================

.nzsc_txt <- function(v, n) tolower(trimws(as.character(v %||% rep(NA_character_, n))))
.nzsc_yes <- function(v) {
  v <- tolower(trimws(as.character(v)))
  ifelse(is.na(v) | !nzchar(v), NA, v %in% c("true", "t", "yes", "y", "1"))
}

# One scalar from a site field (length 0 -> NA, more than one -> the first).
.nzsc_site1 <- function(pedon, field) {
  v <- pedon$site[[field]]
  if (is.null(v) || length(v) < 1L) NA else v[[1L]]
}
# (a %|NA|% b, the first unless NA or empty, is defined in R/natsoil.R)

# Rows in depth order that are described (not trailing), with a top.
.nzsc_rows <- function(x) x$ord[!x$trailing[x$ord] & !is.na(x$h$top_cm[x$ord])]

# The row directly below each row (any material), or NA.
.nzsc_next_row <- function(x) {
  h <- x$h
  vapply(seq_len(x$n), function(i) {
    if (is.na(h$bottom_cm[i])) return(NA_integer_)
    j <- which(!is.na(h$top_cm) & seq_len(x$n) != i & abs(h$top_cm - h$bottom_cm[i]) <= 0.5)
    if (length(j)) j[which.min(abs(h$top_cm[j] - h$bottom_cm[i]))] else NA_integer_
  }, 0L)
}

# The run of adjacent rows from row i down where `ok` holds: c(sure,
# possible) thickness and the bottom of the possible run.
.nzsc_run_down <- function(x, ok, i) {
  h <- x$h; rows <- .nzsc_rows(x)
  k <- match(i, rows)
  sure <- 0; poss <- 0; z <- h$top_cm[i]; certain <- TRUE; bottom <- z; sure_bottom <- z
  if (is.na(k)) return(list(sure = 0, poss = 0, bottom = bottom, sure_bottom = sure_bottom))
  for (j in rows[k:length(rows)]) {
    if (abs(h$top_cm[j] - z) > 0.5 || ok[j] %in% FALSE) break
    th <- x$thick[j]; th <- if (is.na(th)) Inf else max(0, th)
    if (ok[j] %in% TRUE && certain) { sure <- sure + th; sure_bottom <- if (is.na(h$bottom_cm[j])) Inf else h$bottom_cm[j] }
    else certain <- FALSE
    poss <- poss + th; z <- h$bottom_cm[j]; bottom <- z
    if (is.na(z)) { bottom <- Inf; break }
  }
  list(sure = sure, poss = poss, bottom = bottom, sure_bottom = sure_bottom)
}

# A run (adjacent rows where ok holds) starting at a row whose top lies in
# [zt, zb], thicker than `min_cm` (`strict`: more than; else at least).
.nzsc_run_starting <- function(x, ok, zt, zb, min_cm, strict = TRUE) {
  h <- x$h; rows <- .nzsc_rows(x)
  cand <- rows[h$top_cm[rows] >= zt - 0.5 & h$top_cm[rows] <= zb + 0.5 & !(ok[rows] %in% FALSE)]
  if (!length(cand)) return(FALSE)
  pass <- function(t) if (strict) t > min_cm else t >= min_cm
  .nzsc_any(vapply(cand, function(i) {
    up <- rows[rows != i & !is.na(h$bottom_cm[rows]) & abs(h$bottom_cm[rows] - h$top_cm[i]) <= 0.5]
    if (any(ok[up] %in% TRUE)) return(FALSE)          # the run starts higher up
    r <- .nzsc_run_down(x, ok, i)
    v <- if (pass(r$sure)) TRUE else if (pass(r$poss)) NA else FALSE
    if (isTRUE(v) && any(is.na(ok[up]) & h$top_cm[up] < zt - 0.5)) NA else v
  }, NA))
}

# All rows inside [zt, zb] hold `ok` (the window fully covered).
.nzsc_all_in <- function(x, ok, zt, zb) {
  if (is.na(zt) || is.na(zb)) return(NA)
  if (zb <= zt) return(NA)   # no horizon in the window: undecided
  h <- x$h; rows <- .nzsc_rows(x)
  bot <- ifelse(is.na(h$bottom_cm[rows]), Inf, h$bottom_cm[rows])
  inside <- rows[h$top_cm[rows] < zb - 0.5 & bot > zt + 0.5]
  if (any(ok[inside] %in% FALSE)) return(FALSE)
  v <- .nzsc_cum(x, ok, zt, zb); span <- zb - zt
  if (v[1L] >= span - 0.5) TRUE else if (v[2L] < span - 0.5) FALSE else NA
}

# Rows of the B or BC horizon (not buried, not pans).
.nzsc_b_bc_rows <- function(x) which(.nzsc_is(x, c("B", "BC")) & !x$des$m & !is.na(x$h$top_cm))

# The row of the B or BC horizon at 60 cm from the mineral soil surface, or
# its deepest row when the B or BC is shallower (NA when there is none, or
# when undesignated rows leave it unknown).
.nzsc_b_at_60 <- function(x) {
  h <- x$h; b <- .nzsc_b_bc_rows(x); z <- x$ms + 60
  if (!length(b)) return(NA_integer_)
  at <- b[h$top_cm[b] <= z & (is.na(h$bottom_cm[b]) | h$bottom_cm[b] > z)]
  if (length(at)) return(at[1L])
  deep <- b[which.max(h$top_cm[b])]
  if (!is.na(h$bottom_cm[deep]) && h$bottom_cm[deep] <= z + 0.5) deep else NA_integer_
}

# ---- materials and features ----------------------------------------------------

# Redox segregations recorded in each row: a percentage, or the notation
# (g, r, (g), (f)); recorded when met, so neither reads as none.
.nzsc_redox_present <- function(x) {
  pct <- x$h$redoximorphic_features_pct %||% rep(NA_real_, x$n)
  # g, (g) and (f) mark segregations; r (uniformly reduced) does not
  mark <- x$des$g | x$des$g_weak | x$des$f_weak
  ifelse(!is.na(pct), pct > 0, mark)
}

# Redox-mottled horizon: 2 % or more redox segregations with low-chroma
# colours on less than half of the matrix; the notation (g) or (f) decides
# for a designated horizon, and a reductimorphic one is not redox-mottled.
.nzsc_redox_mottled_rows <- function(x) {
  pct <- x$h$redoximorphic_features_pct %||% rep(NA_real_, x$n)
  red <- .nzsc_reductimorphic_rows(x)
  ifelse(x$des$g_weak | x$des$f_weak, TRUE,
  ifelse(red %in% TRUE, FALSE,
  ifelse(!is.na(pct), ifelse(pct >= 2, ifelse(red %in% FALSE, TRUE, NA), FALSE), FALSE)))
}

# Perch-gley features (profile): a horizon with redox segregations mainly
# within peds (or within the matrix of an apedal horizon), or a horizon with
# redox segregations over one with fewer that is not reductimorphic.
.nzsc_perch_gley <- function(x) {
  h <- x$h; n <- x$n
  pct <- h$redoximorphic_features_pct %||% rep(NA_real_, n)
  present <- .nzsc_redox_present(x)
  loc <- .nzsc_txt(h$redox_segregations_location, n)
  within <- present & grepl("within|inside|interior|matrix", loc)
  red <- .nzsc_reductimorphic_rows(x)
  nxt <- .nzsc_next_row(x)
  over <- vapply(seq_len(n), function(i) {
    if (!isTRUE(present[i]) || !x$mineral[i]) return(FALSE)
    j <- nxt[i]
    if (is.na(j) || !x$mineral[j] || red[j] %in% TRUE) return(FALSE)
    fewer <- if (!is.na(pct[i]) && !is.na(pct[j])) pct[j] < pct[i]
             else if (isFALSE(present[j])) TRUE else NA
    if (is.na(red[j]) && isTRUE(fewer)) NA else fewer
  }, NA)
  .nzsc_any(c(within, over))
}

# Gley profile form: a reductimorphic horizon with its upper boundary
# within 30 cm of the mineral soil surface.
.nzsc_gley_profile_form <- function(x) {
  h <- x$h; ms <- x$ms
  if (is.na(ms)) return(NA)
  red <- .nzsc_reductimorphic_rows(x)
  rows <- .nzsc_rows(x)
  rows <- rows[h$top_cm[rows] >= ms - 0.5 & h$top_cm[rows] < ms + 30]
  .nzsc_any(red[rows])
}

# Peaty topsoil: 10 cm or more thick from the soil surface, saturated for
# 30 or more consecutive days (unless artificially drained), with more than
# 30 % organic matter (peat, peaty textures, an O horizon, or 17.4 % carbon,
# reading organic matter as 1.72 times the carbon), or slightly peaty (17-30
# % organic matter, 9.9-17.4 % carbon) with less than 18 % clay.
.nzsc_peaty_topsoil <- function(pedon, x) {
  h <- x$h; n <- x$n
  # the topsoil starts below any litter (L) at the surface
  start <- x$ord[!(x$role[x$ord] %in% "L")]
  if (!length(start)) return(FALSE)
  i0 <- start[1L]; s0 <- h$top_cm[i0]
  if (is.na(s0)) return(NA)
  oc <- h$oc_pct; clay <- h$clay_pct; ft <- .nzsc_txt(h$field_texture, n)
  peat <- ifelse(grepl("slightly peaty", ft), ifelse(is.na(clay), NA, clay < 18),
          ifelse(grepl("peat", ft) | x$role %in% "O", TRUE,
          ifelse(x$role %in% "L", FALSE,
          ifelse(!is.na(oc), ifelse(oc >= 17.4, TRUE,
                             ifelse(oc >= 9.9, ifelse(is.na(clay), NA, clay < 18), FALSE)), NA))))
  r <- .nzsc_run_down(x, peat, i0)
  thick <- if (r$sure >= 10) TRUE else if (r$poss >= 10) NA else FALSE
  if (isFALSE(thick)) return(FALSE)
  if (isTRUE(.nzsc_yes(.nzsc_site1(pedon, "artificially_drained")))) return(thick)
  wsd <- h$water_saturation_days %||% rep(NA_real_, n)
  top_rows <- x$ord[!is.na(h$top_cm[x$ord]) & h$top_cm[x$ord] >= s0 - 0.5 & h$top_cm[x$ord] < s0 + 10]
  sat_days <- .nzsc_max(wsd[top_rows])
  dr <- tolower(as.character(.nzsc_site1(pedon, "drainage_class") %|NA|% .nzsc_site1(pedon, "drainage")))
  sat <- if (!is.na(sat_days)) sat_days >= 30
         else if (is.na(dr)) NA
         else if (grepl("very poor|^poor", dr)) TRUE
         else if (grepl("well|rapid|excess", dr)) FALSE else NA
  .and3(c(thick, sat))
}

# Slowly permeable layer of each row: a measured Ksat below 4 mm/h decides;
# otherwise the morphology (pedal with peds coarser than 10 mm and a degree
# of packing for their size; sand or loamy sand extremely highly packed; a
# texture group other than sandy at least highly packed); and the horizons
# the text names as often working as one count too (Hugo's decision,
# 2026-10-28): fragipans, argillic horizons, densipans, humus-pans,
# ortstein-pans, and lithic (R) and paralithic (CR) contacts. A packing not
# recorded reads as none.
.nzsc_packing_index <- function(v) {
  v <- tolower(trimws(gsub("[_-]", " ", as.character(v))))
  unname(c("very low" = 0, "low" = 1, "moderate" = 2, "high" = 3, "very high" = 4,
           "extremely high" = 5)[v])
}
.nzsc_slowly_permeable_rows <- function(x) {
  h <- x$h; n <- x$n
  ks <- h$ksat_mm_h %||% rep(NA_real_, n)
  pk <- .nzsc_packing_index(h$packing_degree %||% rep(NA, n))
  size <- h$ped_size_max_mm %||% rep(NA_real_, n)
  pedal <- .nzsc_pedal(h)
  ls <- x$tex$class %in% c("sand", "loamy sand")
  # peds 20 to 50 mm, 50 to 100 mm, 100 mm or larger (a size on a shared
  # limit is read in the stricter class below it, 100 mm in the last)
  m1 <- pedal %in% TRUE & !is.na(size) & !is.na(pk) &
        ((size >= 20 & size <= 50 & pk >= 5) | (size > 50 & size < 100 & pk >= 4) | (size >= 100 & pk >= 3))
  m2 <- !is.na(pk) & ((ls & pk >= 5) | (!is.na(x$tex$group) & !(x$tex$group %in% "sandy") & !ls & pk >= 3))
  dens <- .nzsc_densipan_rows(x); arg <- .nzsc_argillic_rows(x)
  pans <- .nzsc_pan_rows(x, "fragipan") | dens %in% TRUE |
          .nzsc_pan_rows(x, "humus_pan") | .nzsc_pan_rows(x, "ortstein") |
          x$role %in% c("R", "CR") | arg %in% TRUE
  maybe <- is.na(dens) | is.na(arg)
  ifelse(!is.na(ks), ks < 4, ifelse(m1 | m2 | pans, TRUE, ifelse(maybe, NA, FALSE)))
}

# Densipan: an eluvial horizon marked d (Ed), or a recorded densipan; an
# eluvial horizon that is hard or very hard, or resists penetration above
# 4,000 kPa, may be one (its slaking is not recorded).
.nzsc_densipan_rows <- function(x) {
  h <- x$h; n <- x$n
  e <- .nzsc_is(x, "E")
  pt <- .nzsc_txt(h$pan_type, n)
  sm <- .nzsc_class_index(h$strength_moist %||% rep(NA, n), "strength")
  sd <- .nzsc_class_index(h$strength_dry %||% rep(NA, n), "strength")
  pen <- h$penetration_resistance_kpa %||% rep(NA_real_, n)
  hard <- (!is.na(sm) & sm >= 6) | (!is.na(sd) & sd >= 6) | (!is.na(pen) & pen > 4000)
  ifelse((e & x$des$d) | grepl("densipan", pt), TRUE, ifelse(e & hard, NA, FALSE))
}

# Nodules (iron or aluminium oxyhydroxides) of each row, in %: the recorded
# ferric and bauxitic nodules; an i suffix marks a nodular horizon.
.nzsc_nodular_rows <- function(x) {
  h <- x$h; n <- x$n
  fe <- h$ferric_nodules_pct %||% rep(NA_real_, n); al <- h$bauxitic_nodules_pct %||% rep(NA_real_, n)
  tot <- ifelse(is.na(fe) & is.na(al), NA, ifelse(is.na(fe), 0, fe) + ifelse(is.na(al), 0, al))
  ifelse(x$des$i, TRUE, ifelse(!is.na(tot), tot > 5, FALSE))
}

# Tephric soil material of each row: a layer or rock origin naming tephra
# (or the site's parent material, for rows with none); or volcanic glass in
# the sand, more than 40 % (any source) or more than 10 % from an andesitic
# or basaltic source. A recorded origin of another kind with no glass
# recorded is not tephric.
.nzsc_tephric_rows <- function(pedon, x) {
  h <- x$h; n <- x$n
  own <- paste(.nzsc_txt(h$layer_origin, n), .nzsc_txt(h$rock_origin, n))
  own <- gsub("\\bna\\b", "", own, ignore.case = TRUE)
  site_pm <- tolower(as.character(.nzsc_site1(pedon, "parent_material") %|NA|% ""))
  org <- ifelse(grepl("[a-z]", own), own, site_pm)
  word <- grepl("tephra|tephric|pyroclast|volcanic ash|\\bash\\b|lapilli|scoria|pumice|volcanogenic", org) |
          (grepl("ignimbrite", org) & (!grepl("welded", org) | grepl("non[- ]?welded", org)))
  known <- grepl("[a-z]", org)
  vg <- h$volcanic_glass_pct %||% rep(NA_real_, n)
  ab <- grepl("andesit|basalt", org)
  out <- ifelse(word, TRUE,
         ifelse(!is.na(vg), ifelse(vg > 40, TRUE, ifelse(vg > 10, ifelse(ab, TRUE, ifelse(known, FALSE, NA)), FALSE)),
         ifelse(known, FALSE, NA)))
  ifelse(x$mineral, out, FALSE)
}

# Sulfuric soil material of each row: pH in water below 4.0 (below 3.0 in
# organic soil material) and jarosite, 0.05 % or more sulfate S, or sulfidic
# material (more than 0.01 % sulfide S) directly below. Jarosite is
# recorded when met.
.nzsc_sulfuric_rows <- function(x) {
  h <- x$h; n <- x$n
  lim <- ifelse(x$organic %in% TRUE, 3, 4)
  acid <- ifelse(is.na(h$ph_h2o), NA, h$ph_h2o < lim)
  jar <- .nzsc_yes(h$jarosite_present %||% rep(NA, n)) %in% TRUE
  so4 <- h$sulfate_s_pct %||% rep(NA_real_, n)
  sulfide <- h$sulfidic_s_pct %||% rep(NA_real_, n)
  nxt <- .nzsc_next_row(x)
  below <- vapply(seq_len(n), function(i) if (is.na(nxt[i])) NA_real_ else sulfide[nxt[i]], 0)
  # no: only when both measures are recorded and short (jarosite is recorded
  # when met)
  second <- ifelse(jar | (!is.na(so4) & so4 >= 0.05) | (!is.na(below) & below > 0.01), TRUE,
            ifelse(!is.na(so4) & !is.na(below), FALSE, NA))
  .and3v(acid, second)
}

# Fluvial features (profile): a fluvial landform on the site, or rows of
# fluvial origin, sedimentary bedding in a C horizon, a buried A horizon, or
# a moderately or very fluid horizon within 120 cm; and no cover of more
# than 30 cm of material of another recorded origin over the uppermost of
# them. Recorded when met: none of these reads as no fluvial features.
.nzsc_fluvial <- function(pedon, x, from) {
  h <- x$h; n <- x$n
  land <- tolower(paste(.nzsc_site1(pedon, "landform") %|NA|% "", .nzsc_site1(pedon, "landform_element") %|NA|% ""))
  # the landforms the Diagnostics list
  site_fluvial <- grepl(paste0("flood|estuar|lacustr|alluvial fan|\\bfan\\b|levee|backplain|\\bbar\\b|",
                               "channel|delta|bench|outwash|swamp"), land)
  orig <- .nzsc_txt(h$layer_origin, n)
  fluv_row <- grepl("fluv|alluv|estuar|lacustr|lagoon|deltaic|flood", orig)
  strat <- .nzsc_txt(h$stratification_pattern, n)
  bedded <- x$role %in% c("C", "BC", "CB") & !is.na(strat) & nzchar(strat) &
            !grepl("^none|^no|^absent|lithologic", strat)
  fluid <- .nzsc_fluid_rows(x) & !is.na(h$top_cm) & h$top_cm < x$soil_top + 120
  # a buried A confirms only alongside another fluvial sign
  buried_a <- x$des$buried & x$role %in% "A"
  sign <- site_fluvial || any(fluv_row | bedded | fluid)
  ev <- fluv_row | bedded | fluid | (buried_a & sign)
  if (!sign) return(FALSE)
  other <- grepl("tephra|tephric|loess|aeolian|eolian|colluv|fill|dune|pumice|ash", orig) & !fluv_row
  # on a fluvial landform the evidence starts at the first row of no other
  # recorded origin; otherwise at the uppermost evidence
  tops_ok <- !is.na(h$top_cm)
  first <- if (site_fluvial) { s <- !other & tops_ok; if (any(s)) min(h$top_cm[s]) else Inf }
           else if (any(ev & tops_ok)) min(h$top_cm[ev & tops_ok]) else from
  cover <- sum(pmax(0, pmin(h$bottom_cm, first) - pmax(h$top_cm, from))[other & tops_ok & !is.na(h$bottom_cm)],
               na.rm = TRUE)
  cover <= 30
}

# Hydrothermal: the mean annual soil temperature at 30 cm at least 2.5
# degrees above the mean annual air temperature. Neither recorded reads as
# not hydrothermal (geothermal ground is recorded when met); one alone
# leaves it open.
.nzsc_hydrothermal <- function(pedon) {
  st <- suppressWarnings(as.numeric(.nzsc_site1(pedon, "soil_temp_30cm_C")))
  at <- suppressWarnings(as.numeric(.nzsc_site1(pedon, "mean_annual_air_temp_C")))
  if (is.na(st) && is.na(at)) return(FALSE)
  if (is.na(st) || is.na(at)) return(NA)
  st - at >= 2.5
}

# Parent material from the site and the layer and rock origins.
.nzsc_pm_text <- function(pedon, x) {
  tolower(paste(c(.nzsc_site1(pedon, "parent_material"), x$h$rock_origin %||% NA, x$h$layer_origin %||% NA), collapse = " "))
}
.nzsc_andesitic_basaltic <- function(pedon, x) {
  pm <- tolower(as.character(.nzsc_site1(pedon, "parent_material")))
  if (!is.na(pm) && nzchar(pm)) return(grepl("andesit|basalt", pm))
  # otherwise the origin that dominates the described thickness
  h <- x$h; n <- x$n
  org <- paste(.nzsc_txt(h$rock_origin, n), .nzsc_txt(h$layer_origin, n))
  org <- gsub("\\bna\\b", "", org, ignore.case = TRUE)
  rec <- grepl("[a-z]", org) & !is.na(x$thick)
  if (!any(rec)) return(NA)
  ab <- grepl("andesit|basalt", org)
  sum(pmax(0, x$thick[rec & ab])) > sum(pmax(0, x$thick[rec & !ab]))
}

# Coarse rock fragments that are mainly mafic or ultramafic (not
# tuffaceous greywacke), 5 % or more, in row i.
.nzsc_mafic_fragments <- function(pedon, x, i) {
  cf <- x$h$coarse_fragments_pct[i]
  ro <- tolower(as.character(x$h$rock_origin[i] %||% NA))
  if (is.na(ro) || !nzchar(ro)) ro <- tolower(as.character(.nzsc_site1(pedon, "parent_material")))
  # mafic or ultramafic, not tuffaceous greywacke (andesite is intermediate)
  maf <- if (is.na(ro)) NA else grepl("basalt|dolerite|gabbro|mafic|ultramafic|serpentin|peridot|dunite|ophiolit", ro) &&
                                 !grepl("tuffaceous", ro)
  if (isFALSE(maf)) return(FALSE)
  if (is.na(cf)) return(NA)
  if (cf < 5) return(FALSE)
  maf
}

# Exchangeable Ca:Mg 0.2 or less with exchangeable Mg 1.5 cmol/kg or more.
.nzsc_ca_mg <- function(x, i) {
  ca <- x$h$ca_cmol[i]; mg <- x$h$mg_cmol[i]
  if (is.na(ca) || is.na(mg)) return(NA)
  mg >= 1.5 && mg > 0 && ca / mg <= 0.2
}

# Sand or loamy sand (the sandy texture group), less than 35 % coarse rock
# fragments (fragments not recorded read as none), in every horizon of the
# window; `lamellae`: sandy loam lamellae that are not an argillic horizon
# may occur.
.nzsc_sandy_window <- function(x, zt, zb, lamellae = FALSE) {
  h <- x$h
  cf <- h$coarse_fragments_pct
  sandy <- ifelse(is.na(x$tex$group), NA, x$tex$group %in% "sandy")
  sandy <- ifelse(x$role %in% c("R", "CR") | x$organic %in% TRUE, FALSE, sandy)
  if (lamellae) {
    lam <- grepl("lamell", tolower(as.character(h$designation))) & x$tex$class %in% "sandy loam"
    sandy <- ifelse(lam & !(.nzsc_argillic_rows(x) %in% TRUE), TRUE, sandy)
  }
  ok <- .and3v(sandy, ifelse(is.na(cf), TRUE, cf < 35))
  .nzsc_all_in(x, ok, zt, zb)
}

# "In some part" within a window: the rows count only inside it.
.nzsc_some_part_in <- function(x, rows, ok, zt, zb) {
  h <- x$h
  x$thick <- ifelse(is.na(h$top_cm), NA, .asc_overlap(h$top_cm, ifelse(is.na(h$bottom_cm), Inf, h$bottom_cm), zt, zb))
  .nzsc_some_part(x, rows, ok)
}

# The major part of a B whose last row may be open: an open row reaches
# the window's base when it has one; otherwise the closed rows decide and the
# open row only when it agrees with them.
.nzsc_major_part_b <- function(x, b, ok, zb = Inf) {
  h <- x$h
  top <- min(h$top_cm[b])
  open <- b[is.na(h$bottom_cm[b])]
  if (is.finite(zb)) {
    hh <- list(top_cm = h$top_cm, bottom_cm = ifelse(is.na(h$bottom_cm), zb, h$bottom_cm))
    return(if (zb <= top) NA else .asc_major_part(list(h = hh), b, ok, top, zb))
  }
  closed <- setdiff(b, open)
  kp <- if (length(closed)) .asc_major_part(x, closed, ok, top, max(h$bottom_cm[closed])) else NA
  if (!length(open)) return(kp)
  if (all(ok[open] %in% TRUE) && (isTRUE(kp) || !length(closed))) TRUE
  else if (all(ok[open] %in% FALSE) && (isFALSE(kp) || !length(closed))) FALSE else NA
}

# pH of 4.8 or less in some part between 20 and 60 cm from the mineral soil
# surface.
.nzsc_acid_20_60 <- function(x) {
  h <- x$h; ms <- x$ms
  rows <- .nzsc_rows(x)
  rows <- rows[h$top_cm[rows] < ms + 60 & (is.na(h$bottom_cm[rows]) | h$bottom_cm[rows] > ms + 20)]
  .nzsc_some_part_in(x, rows, ifelse(is.na(h$ph_h2o), NA, h$ph_h2o <= 4.8), ms + 20, ms + 60)
}

.nzsc_group <- function(name, passed, where, evidence = list(), missing = character(0), layers = integer(0)) {
  .nzsc_result(name, passed, layers, evidence, if (is.na(passed)) missing else character(0),
               paste("Key to groups,", where))
}

.nzsc_ms_or_na <- function(x, name, where, missing) {
  if (is.na(x$ms)) .nzsc_group(name, NA, where, missing = missing) else NULL
}

# ---- shared group gates ----------------------------------------------------------

# Perch-gley groups (UP, MP, XP, NP, PP; EP): a gley profile form (or, for
# the Melanic Soils, a peaty topsoil or a gley profile form) and perch-gley
# features.
perch_gley_group_nzsc <- function(pedon, with = c("gley_profile_form", "peaty_or_gley")) {
  with <- match.arg(with)
  x <- .nzsc_horizons(pedon)
  miss <- c("designation", "redoximorphic_features_pct", "redox_segregations_location")
  pre <- .nzsc_ms_or_na(x, "perch_gley_group_nzsc", "perch-gley", miss); if (!is.null(pre)) return(pre)
  gpf <- .nzsc_gley_profile_form(x)
  first <- if (with == "peaty_or_gley") .any3(c(.nzsc_peaty_topsoil(pedon, x), gpf)) else gpf
  pg <- .nzsc_perch_gley(x)
  .nzsc_group("perch_gley_group_nzsc", .and3(c(first, pg)), "perch-gley groups",
              list(gley_profile_form = gpf, perch_gley = pg), miss)
}

# Sandy groups (GS, RS from the base of the A to 60 cm; BS the same, with
# sandy loam laminations; WS from the soil surface to 60 cm).
sandy_group_nzsc <- function(pedon, from = c("a_base", "soil_surface"), laminations = FALSE) {
  from <- match.arg(from)
  x <- .nzsc_horizons(pedon)
  miss <- c("field_texture", "clay_pct", "silt_pct", "coarse_fragments_pct")
  if (from == "a_base") {
    pre <- .nzsc_ms_or_na(x, "sandy_group_nzsc", "sandy groups", miss); if (!is.null(pre)) return(pre)
    zt <- .nzsc_a_base(x); zb <- x$ms + 60
  } else {
    zt <- x$soil_top; zb <- x$soil_top + 60
  }
  passed <- if (is.na(zt)) NA else .nzsc_sandy_window(x, zt, zb, lamellae = isTRUE(laminations))
  .nzsc_group("sandy_group_nzsc", passed, "sandy groups", list(from = zt, to = zb), miss)
}

# Acid groups (GA, BA): pH 4.8 or less in some part between 20 and 60 cm.
acid_group_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon)
  pre <- .nzsc_ms_or_na(x, "acid_group_nzsc", "acid groups", "ph_h2o"); if (!is.null(pre)) return(pre)
  .nzsc_group("acid_group_nzsc", .nzsc_acid_20_60(x), "acid groups", missing = "ph_h2o")
}

# Tephric groups (GT, RT): tephric soil material from the mineral soil
# surface to 30 cm or more.
tephric_group_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon)
  miss <- c("layer_origin", "rock_origin", "volcanic_glass_pct")
  pre <- .nzsc_ms_or_na(x, "tephric_group_nzsc", "tephric groups", miss); if (!is.null(pre)) return(pre)
  t <- .nzsc_tephric_rows(pedon, x)
  .nzsc_group("tephric_group_nzsc", .nzsc_all_in(x, t, x$ms, x$ms + 30), "tephric groups",
              missing = miss, layers = which(t %in% TRUE))
}

# Fluvial groups (RF from the mineral soil surface, WF from the soil
# surface).
fluvial_group_nzsc <- function(pedon, from = c("mineral_surface", "soil_surface")) {
  from <- match.arg(from)
  x <- .nzsc_horizons(pedon)
  z <- if (from == "mineral_surface") x$ms else x$soil_top
  passed <- if (is.na(z)) NA else .nzsc_fluvial(pedon, x, z)
  .nzsc_group("fluvial_group_nzsc", passed, "fluvial groups", missing = c("layer_origin", "site$landform"))
}

# Hydrothermal groups (RH: or pH 4.8 or less in a subsurface horizon within
# 60 cm; WH: the temperature only).
hydrothermal_group_nzsc <- function(pedon, acid = FALSE) {
  x <- .nzsc_horizons(pedon); h <- x$h
  temp <- .nzsc_hydrothermal(pedon)
  # RH: pH 4.8 or less in some part (10 cm) of a subsurface horizon within
  # 60 cm: a horizon below the uppermost mineral horizon (not a surface A)
  ph_part <- if (!isTRUE(acid)) FALSE else if (is.na(x$ms)) NA else {
    rows <- .nzsc_rows(x); min_rows <- rows[x$mineral[rows]]
    first <- if (length(min_rows)) min_rows[1L] else NA_integer_
    sub <- rows[rows != first & h$top_cm[rows] < x$ms + 60 & h$top_cm[rows] > x$ms + 0.5 &
                !(x$role[rows] %in% c("L", "O")) & !(x$role[rows] %in% "A" & !x$des$buried[rows])]
    if (!length(sub)) FALSE else
      .nzsc_some_part_in(x, sub, ifelse(is.na(h$ph_h2o), NA, h$ph_h2o <= 4.8), x$ms, x$ms + 60)
  }
  .nzsc_group("hydrothermal_group_nzsc", .any3(c(temp, ph_part)), "hydrothermal groups",
              list(temperature = temp, acid = ph_part),
              c("site$soil_temp_30cm_C", "site$mean_annual_air_temp_C", "ph_h2o"))
}

# Densipan groups (UD, ZD).
densipan_group_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon)
  d <- .nzsc_densipan_rows(x)
  .nzsc_group("densipan_group_nzsc", .nzsc_any(d), "densipan groups",
              missing = c("designation", "strength_dry", "penetration_resistance_kpa"), layers = which(d %in% TRUE))
}

# Mafic groups (EM, BM): in the B or BC at 60 cm (or at its base when
# shallower), value 4 or less with chroma 3 or more (Melanic) or with
# moderately or strongly pedal polyhedral peds of 20 mm or less (Brown);
# or 5 % or more mafic or ultramafic coarse fragments; or exchangeable
# Ca:Mg 0.2 or less with Mg 1.5 cmol/kg or more.
mafic_group_nzsc <- function(pedon, colour = c("melanic", "brown")) {
  colour <- match.arg(colour)
  x <- .nzsc_horizons(pedon); h <- x$h
  miss <- c("munsell_value_moist", "munsell_chroma_moist", "coarse_fragments_pct", "rock_origin", "ca_cmol", "mg_cmol")
  pre <- .nzsc_ms_or_na(x, "mafic_group_nzsc", "mafic groups", miss); if (!is.null(pre)) return(pre)
  i <- .nzsc_b_at_60(x)
  if (is.na(i)) {
    return(.nzsc_group("mafic_group_nzsc", if (length(.nzsc_b_bc_rows(x))) NA else .nzsc_none(x),
                       "mafic groups", missing = miss))
  }
  v <- h$munsell_value_moist[i]; ch <- h$munsell_chroma_moist[i]
  c1 <- if (colour == "melanic") {
    if (is.na(v) || is.na(ch)) NA else v <= 4 && ch >= 3
  } else {
    g <- tolower(as.character(h$structure_grade[i])); t <- tolower(as.character(h$structure_type[i]))
    sz <- (h$ped_size_max_mm %||% rep(NA_real_, x$n))[i]
    ped <- if (grepl("moderate|strong", g) && grepl("polyhed", t)) { if (is.na(sz)) NA else sz <= 20 }
           else if (is.na(g) || is.na(t)) NA else FALSE
    .and3(c(if (is.na(v)) NA else v <= 4, ped))
  }
  .nzsc_group("mafic_group_nzsc", .any3(c(c1, .nzsc_mafic_fragments(pedon, x, i), .nzsc_ca_mg(x, i))),
              "mafic groups", missing = miss, layers = i)
}

# Gley-profile-and-perch features are read above; a slowly permeable layer
# (or another impeding layer) within 90 cm: LI (or at least weakly
# indurated), MI (or compact and brittle with no roots).
impeded_group_nzsc <- function(pedon, also = c("indurated", "brittle_no_roots")) {
  also <- match.arg(also)
  x <- .nzsc_horizons(pedon); h <- x$h; n <- x$n
  miss <- c("packing_degree", "ksat_mm_h", "pan_type", "roots_abundance")
  pre <- .nzsc_ms_or_na(x, "impeded_group_nzsc", "impeded groups", miss); if (!is.null(pre)) return(pre)
  rows <- .nzsc_rows(x); rows <- rows[h$top_cm[rows] < x$ms + 90]
  sp <- .nzsc_slowly_permeable_rows(x)
  other <- if (also == "indurated") {
    cem <- .nzsc_txt(h$cementation_class, n); pt <- .nzsc_txt(h$pan_type, n)
    x$des$m | grepl("weak|moder|strong|indurat", cem) | (!is.na(pt) & nzchar(pt) & !grepl("^none", pt))
  } else {
    roots <- .nzsc_txt(h$roots_abundance, n)
    cons <- .nzsc_txt(h$consistence_moist, n)
    compact <- .nzsc_pan_rows(x, "fragipan") | .nzsc_pan_rows(x, "brittle") | grepl("brittle", cons)
    ifelse(compact, ifelse(is.na(roots), NA, roots %in% c("none", "no roots", "nil")), FALSE)
  }
  .nzsc_group("impeded_group_nzsc", .any3(c(sp[rows], other[rows])), "impeded groups", missing = miss)
}

# ---- O: Organic Soils --------------------------------------------------------------

litter_organic_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon)
  # fresh litter (Lf) is not organic soil material
  org <- which(x$organic %in% TRUE & !(x$role %in% "L" & x$des$decomp %in% "fibric"))
  if (!length(org)) return(.nzsc_group("litter_organic_nzsc", if (any(is.na(x$organic))) NA else FALSE, "O, OL"))
  l_rows <- org[x$role[org] %in% "L"]
  all_l <- length(l_rows) == length(org)
  lettered <- all(x$des$decomp[l_rows] %in% c("mesic", "humified"))
  unknown <- any(x$unknown_role[org])
  dr <- tolower(as.character(.nzsc_site1(pedon, "drainage_class") %|NA|% .nzsc_site1(pedon, "drainage")))
  not_wet <- if (is.na(dr)) NA else !grepl("very poor", dr)
  passed <- if (all_l) { if (lettered) not_wet else .and3(c(NA, not_wet)) }
            else if (unknown && !any(x$role[org] %in% "O")) NA else FALSE
  .nzsc_group("litter_organic_nzsc", passed, "O, OL", missing = c("designation", "site$drainage_class"), layers = org)
}

# OF, OM, OH: the decomposition that dominates, by accumulated thickness,
# the O horizons from the upper surface of the organic soil material to 60
# cm (or to its base).
organic_decomposition_nzsc <- function(pedon, kind = c("fibric", "mesic", "humified")) {
  kind <- match.arg(kind)
  x <- .nzsc_horizons(pedon); h <- x$h
  # organic soil material (fresh litter, Lf, aside)
  om <- x$organic %in% TRUE & !(x$role %in% "L" & x$des$decomp %in% "fibric")
  org <- x$ord[om[x$ord]]
  if (!length(org)) return(.nzsc_group("organic_decomposition_nzsc", NA, "O, OF/OM/OH", missing = "designation"))
  zt <- h$top_cm[org[1L]]
  base <- zt; for (i in org) { if (is.na(h$bottom_cm[i])) { base <- Inf; break }; base <- max(base, h$bottom_cm[i]) }
  zb <- min(zt + 60, base)
  part <- function(ok) .nzsc_cum(x, ok, zt, zb)[1L]
  # O horizons and partly or well decomposed litter (Lm, Lh) by their
  # letter; any other organic soil material counts as unknown
  th <- c(fibric = part(om & x$role %in% "O" & x$des$decomp %in% "fibric"),
          mesic = part(om & x$des$decomp %in% "mesic"),
          humified = part(om & x$des$decomp %in% "humified"))
  unk <- part(om & is.na(x$des$decomp))
  others <- max(th[setdiff(names(th), kind)])
  # a tie between decompositions leaves the dominance undecided
  passed <- if (th[[kind]] > others + unk) TRUE else if (th[[kind]] + unk < others) FALSE else NA
  .nzsc_group("organic_decomposition_nzsc", passed, "O, OF/OM/OH", as.list(c(th, unknown = unk)),
              missing = "designation")
}

# ---- G: Gley Soils ------------------------------------------------------------------

sulfuric_gley_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h
  miss <- c("ph_h2o", "jarosite_present", "sulfate_s_pct", "sulfidic_s_pct")
  pre <- .nzsc_ms_or_na(x, "sulfuric_gley_nzsc", "G, GU", miss); if (!is.null(pre)) return(pre)
  s <- .nzsc_sulfuric_rows(x)
  rows <- .nzsc_rows(x); rows <- rows[h$top_cm[rows] < x$ms + 90]
  .nzsc_group("sulfuric_gley_nzsc", .nzsc_some_part_in(x, rows, s, -Inf, x$ms + 90), "G, GU", missing = miss, layers = which(s %in% TRUE))
}

oxidic_gley_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon)
  pre <- .nzsc_ms_or_na(x, "oxidic_gley_nzsc", "G, GX", "designation"); if (!is.null(pre)) return(pre)
  .nzsc_group("oxidic_gley_nzsc", .nzsc_run_starting(x, .nzsc_oxidic_rows(x), x$ms + 20, x$ms + 60, 30),
              "G, GX", missing = "designation")
}

# GR: sedimentary stratification within 60 cm, or a buried A (or an increase
# of organic carbon with depth) between 30 and 120 cm; and no buried B within
# 60 cm, no B or BC with its base below 30 cm, no placic or ortstein, no
# more than 2 % nodules or concretions within 60 cm.
recent_gley_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; n <- x$n; ms <- x$ms
  miss <- c("designation", "stratification_pattern", "oc_pct")
  pre <- .nzsc_ms_or_na(x, "recent_gley_nzsc", "G, GR", miss); if (!is.null(pre)) return(pre)
  strat <- .nzsc_txt(h$stratification_pattern, n)
  bedded <- !is.na(strat) & nzchar(strat) & !grepl("^none|^no|^absent|lithologic", strat) &
            !is.na(h$top_cm) & h$top_cm <= ms + 60
  in_30_120 <- !is.na(h$top_cm) & h$top_cm > ms + 30 & h$top_cm <= ms + 120
  buried_a <- x$des$buried & x$role %in% "A" & in_30_120
  nxt <- .nzsc_next_row(x); oc <- h$oc_pct
  carbon_up <- vapply(seq_len(n), function(i) { j <- nxt[i]
    !is.na(j) && in_30_120[j] && !is.na(oc[i]) && !is.na(oc[j]) && oc[j] > oc[i] }, NA)
  c1 <- any(bedded) || any(buried_a) || any(carbon_up)
  buried_b <- any(x$des$buried & x$role %in% "B" & !is.na(h$top_cm) & h$top_cm <= ms + 60)
  # any B or BC (pans included) with its base below 30 cm
  bbc <- .nzsc_is(x, c("B", "BC"))
  deep_b <- any(bbc & (is.na(h$bottom_cm) | h$bottom_cm > ms + 30))
  maybe_b <- any(x$unknown_role & x$mineral & (is.na(h$bottom_cm) | h$bottom_cm > ms + 30))
  pans <- any(.nzsc_pan_rows(x, "placic") | .nzsc_pan_rows(x, "ortstein"))
  nod <- vapply(c("ferric_nodules_pct", "manganiferous_nodules_pct", "bauxitic_nodules_pct"),
                function(k) { v <- h[[k]] %||% rep(NA_real_, n); ifelse(is.na(v), 0, v) }, numeric(n))
  nod <- if (is.matrix(nod)) rowSums(nod) else sum(nod)
  concr <- any((nod > 2 | x$des$i) & !is.na(h$top_cm) & h$top_cm < ms + 60)
  c2 <- !buried_b && !deep_b && !pans && !concr
  passed <- if (!c1 || !c2) FALSE else if (maybe_b) NA else TRUE
  .nzsc_group("recent_gley_nzsc", passed, "G, GR", list(stratified_or_buried = c1), miss)
}

# ---- U: Ultic Soils -------------------------------------------------------------------

# UE: an Eg or Er horizon over a firm, clayey B horizon with prismatic peds
# and humus or clay coatings in some part.
albic_ultic_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; n <- x$n
  miss <- c("designation", "strength_moist", "structure_type", "clay_films_amount")
  e <- which(.nzsc_is(x, "E") & (x$des$g | x$des$r) & !is.na(h$top_cm))
  if (!length(e)) return(.nzsc_group("albic_ultic_nzsc", .nzsc_none(x), "U, UE", missing = miss))
  b <- .nzsc_b_rows(x); b <- b[h$top_cm[b] > min(h$top_cm[e])]
  str <- .nzsc_class_index(h$strength_moist %||% rep(NA, n), "strength")
  films <- .nzsc_txt(h$clay_films_amount, n)
  coat <- ifelse(x$des$t | x$des$h_weak | grepl("few|common|many|continuous", films), TRUE,
          ifelse(grepl("^none|^no", films), FALSE, NA))
  t <- tolower(as.character(h$structure_type))
  ok <- .and3v(.and3v(ifelse(is.na(str), NA, str >= 4), ifelse(is.na(x$tex$group), NA, x$tex$group %in% "clayey")),
               .and3v(ifelse(grepl("prism", t), TRUE, ifelse(is.na(h$structure_type), NA, FALSE)), coat))
  .nzsc_group("albic_ultic_nzsc", if (!length(b)) .nzsc_none(x) else .nzsc_some_part(x, b, ok), "U, UE", missing = miss)
}

sandy_ultic_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h
  b <- .nzsc_b_rows(x)
  if (!length(b)) return(.nzsc_group("sandy_ultic_nzsc", .nzsc_none(x), "U, US", missing = "sand_pct"))
  .nzsc_group("sandy_ultic_nzsc", .nzsc_major_part_b(x, b, ifelse(is.na(h$sand_pct), NA, h$sand_pct > 60)),
              "U, US", missing = "sand_pct")
}

# ---- Z: Podzol Soils -----------------------------------------------------------------

.nzsc_low_chroma_or_red <- function(x) {
  lc <- .nzsc_low_chroma(x$h)
  ifelse(x$des$g | x$des$r, TRUE, lc)
}

# ZP: an eluvial horizon with low-chroma colours, a peaty topsoil, or a
# placic horizon with redox segregations in the eluvial horizon; and a
# slowly permeable layer or perch-gley features.
perch_gley_podzol_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon)
  miss <- c("designation", "munsell_value_moist", "munsell_chroma_moist", "packing_degree", "redox_segregations_location")
  # an eluvial horizon, with low-chroma colours, a peaty topsoil, or a
  # placic horizon with redox segregations in it
  e <- which(.nzsc_eluvial_rows(x) %in% TRUE)
  lc <- .nzsc_low_chroma_or_red(x); red_p <- .nzsc_redox_present(x)
  placic <- any(.nzsc_pan_rows(x, "placic"))
  c1 <- if (!length(e)) .nzsc_none(x) else
    .any3(c(.nzsc_any(lc[e]), .nzsc_peaty_topsoil(pedon, x), placic && any(red_p[e] %in% TRUE)))
  c2 <- .any3(c(.nzsc_any(.nzsc_slowly_permeable_rows(x)[.nzsc_rows(x)]), .nzsc_perch_gley(x)))
  .nzsc_group("perch_gley_podzol_nzsc", .and3(c(c1, c2)), "Z, ZP", list(eluvial = c1, impeded = c2), miss)
}

# ZG: an eluvial horizon with low-chroma colours, or a peaty topsoil; and a
# BC or C horizon with low-chroma colours that extends below 90 cm or has
# redox segregations mainly as coatings on voids or skeletans.
groundwater_gley_podzol_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; n <- x$n
  miss <- c("designation", "munsell_value_moist", "munsell_chroma_moist", "redox_segregations_location")
  pre <- .nzsc_ms_or_na(x, "groundwater_gley_podzol_nzsc", "Z, ZG", miss); if (!is.null(pre)) return(pre)
  e <- which(.nzsc_eluvial_rows(x) %in% TRUE)
  lc <- .nzsc_low_chroma_or_red(x)
  # an eluvial horizon with low-chroma colours, or with a peaty topsoil
  c1 <- if (!length(e)) .nzsc_none(x) else .any3(c(.nzsc_any(lc[e]), .nzsc_peaty_topsoil(pedon, x)))
  is_c <- .nzsc_is(x, c("BC", "C"))
  lc_c <- ifelse(is_c, lc, ifelse(x$unknown_role & x$mineral, ifelse(lc %in% FALSE, FALSE, NA), FALSE))
  c_rows <- which(is_c & !is.na(h$top_cm))
  loc <- .nzsc_txt(h$redox_segregations_location, n)
  coats <- grepl("void|pore|skeletan|coating", loc)
  c2 <- if (!length(c_rows)) .nzsc_none(x) else .nzsc_any(vapply(c_rows, function(i) {
    if (lc[i] %in% FALSE) return(FALSE)
    r <- .nzsc_run_down(x, lc_c, i)
    v <- if (r$sure_bottom > x$ms + 90 || coats[i]) TRUE else if (r$bottom > x$ms + 90) NA else FALSE
    if (is.na(lc[i]) && isTRUE(v)) NA else v
  }, NA))
  .nzsc_group("groundwater_gley_podzol_nzsc", .and3(c(c1, c2)), "Z, ZG", list(eluvial = c1, gley_c = c2), miss)
}

# ZX: a subhorizon of the B horizon apedal massive with firm or stronger
# moist strength, or a penetration resistance of 3,100 kPa or more.
pan_podzol_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; n <- x$n
  miss <- c("structure_grade", "structure_type", "strength_moist", "penetration_resistance_kpa")
  # any subhorizon of the B, pans (Bsm, Bhm) included
  b <- which(.nzsc_is(x, "B") & !is.na(h$top_cm))
  if (!length(b)) return(.nzsc_group("pan_podzol_nzsc", .nzsc_none(x), "Z, ZX", missing = miss))
  single <- grepl("single", tolower(as.character(h$structure_type)))
  massive <- ifelse(single, FALSE, ifelse(.nzsc_pedal(h) %in% FALSE, TRUE, ifelse(.nzsc_pedal(h) %in% TRUE, FALSE, NA)))
  str <- .nzsc_class_index(h$strength_moist %||% rep(NA, n), "strength")
  pen <- h$penetration_resistance_kpa %||% rep(NA_real_, n)
  strong <- ifelse((!is.na(str) & str >= 4) | (!is.na(pen) & pen >= 3100), TRUE,
            ifelse(!is.na(str) | !is.na(pen), FALSE, NA))
  .nzsc_group("pan_podzol_nzsc", .nzsc_any(.and3v(massive, strong)[b]), "Z, ZX", missing = miss)
}

# ---- L: Allophanic Soils --------------------------------------------------------------

# LP (with perch-gley features) and LG: a peaty topsoil, or within 30 cm a
# reductimorphic horizon or (on andesitic or basaltic parent material) a
# redox-mottled horizon.
wet_allophanic_nzsc <- function(pedon, perch = FALSE) {
  x <- .nzsc_horizons(pedon); h <- x$h
  miss <- c("designation", "redoximorphic_features_pct", "site$parent_material")
  pre <- .nzsc_ms_or_na(x, "wet_allophanic_nzsc", "L, LP/LG", miss); if (!is.null(pre)) return(pre)
  rows <- .nzsc_rows(x); rows <- rows[h$top_cm[rows] < x$ms + 30 & h$top_cm[rows] >= x$ms - 0.5]
  red <- .nzsc_any(.nzsc_reductimorphic_rows(x)[rows])
  ab <- .nzsc_andesitic_basaltic(pedon, x)
  mot <- .nzsc_any(.nzsc_redox_mottled_rows(x)[rows])
  # a redox-mottled horizon counts on predominantly andesitic or basaltic
  # parent material (not recorded: open)
  rm <- if (isTRUE(ab)) mot else if (is.na(ab) && !isFALSE(mot)) NA else FALSE
  wet <- .any3(c(.nzsc_peaty_topsoil(pedon, x), red, rm))
  passed <- if (isTRUE(perch)) .and3(c(.nzsc_perch_gley(x), wet)) else wet
  .nzsc_group("wet_allophanic_nzsc", passed, "L, LP/LG", list(wet = wet), miss)
}

# ---- E: Melanic Soils ------------------------------------------------------------------

# EV: cracks 4 mm wide or more reaching 30 cm (or in the B infilled with A
# material), or a COLE of 0.09 or more with moderate or strong blocky or
# prismatic peds in the major part of the B; and no redox segregations
# within 30 cm.
vertic_melanic_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; ms <- x$ms
  miss <- c("cracks_width_cm", "cracks_depth_cm", "cole_value", "structure_grade")
  pre <- .nzsc_ms_or_na(x, "vertic_melanic_nzsc", "E, EV", miss); if (!is.null(pre)) return(pre)
  cw <- h$cracks_width_cm; cd <- h$cracks_depth_cm
  cracks <- .nzsc_any(ifelse(!is.na(cw) & cw >= 0.4, ifelse(is.na(cd), NA, cd >= 30), FALSE))
  b <- .nzsc_b_rows(x)
  g <- tolower(as.character(h$structure_grade)); t <- tolower(as.character(h$structure_type))
  shape <- ifelse(grepl("moderate|strong", g) & grepl("block|prism", t), TRUE,
           ifelse(is.na(h$structure_grade) | is.na(h$structure_type), NA, FALSE))
  cole <- ifelse(is.na(h$cole_value), NA, h$cole_value >= 0.09)
  # the COLE and the pedality, both in the major part of the B
  cle <- if (!length(b)) FALSE else .nzsc_major_part_b(x, b, .and3v(cole, shape))
  rows <- .nzsc_rows(x); rows <- rows[h$top_cm[rows] < ms + 30 & x$mineral[rows]]
  no_redox <- !.nzsc_any(.nzsc_redox_present(x)[rows])
  .nzsc_group("vertic_melanic_nzsc", .and3(c(.any3(c(cracks, cle)), no_redox)), "E, EV",
              list(cracks = cracks, cole = cle), miss)
}

# ER: limestone or other calcareous parent material as a lithic or
# paralithic contact, or as rock rubble (70 % or more coarse fragments),
# with its upper boundary at 60 cm or less; the rubble continuing below 90
# cm.
rendzic_melanic_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; n <- x$n; ms <- x$ms
  miss <- c("rock_origin", "coarse_fragments_pct", "site$parent_material")
  pre <- .nzsc_ms_or_na(x, "rendzic_melanic_nzsc", "E, ER", miss); if (!is.null(pre)) return(pre)
  ro <- .nzsc_txt(h$rock_origin, n)
  site_pm <- tolower(as.character(.nzsc_site1(pedon, "parent_material")))
  ro <- ifelse(is.na(ro) | !nzchar(ro), site_pm, ro)
  calc <- ifelse(is.na(ro), NA, grepl("limestone|marl|calcare|chalk|dolomit|coquina|shell", ro) &
                                 !grepl("non[- ]?calcareous|not calcareous", ro))
  contact <- x$role %in% c("R", "CR") & !is.na(h$top_cm) & h$top_cm <= ms + 60
  c_contact <- .nzsc_any(ifelse(contact, calc, FALSE))
  cf <- h$coarse_fragments_pct
  rubble <- .and3v(ifelse(is.na(cf), FALSE, cf >= 70), calc)
  rows <- .nzsc_rows(x)
  c_rubble <- .nzsc_any(vapply(rows[h$top_cm[rows] <= ms + 60 & !(rubble[rows] %in% FALSE)], function(i) {
    r <- .nzsc_run_down(x, rubble, i)
    if (r$sure_bottom > ms + 90) TRUE else if (r$bottom > ms + 90) NA else FALSE
  }, NA))
  .nzsc_group("rendzic_melanic_nzsc", .any3(c(c_contact, c_rubble)), "E, ER", missing = miss)
}

# ---- S: Semiarid Soils -------------------------------------------------------------------

# SA: an argillic horizon whose matrix has, in part, a hue of 7.5YR or
# redder, or 10YR with chroma 6 or more.
aged_argillic_semiarid_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h
  a <- .nzsc_argillic_rows(x)
  hue <- .nzsc_hue_num(h$munsell_hue_moist); ch <- h$munsell_chroma_moist
  neutral <- grepl("^N", toupper(trimws(as.character(h$munsell_hue_moist))))
  red <- ifelse(neutral, FALSE, ifelse(is.na(hue), NA, ifelse(hue <= 17.5, TRUE,
         ifelse(abs(hue - 20) < 0.01, ifelse(is.na(ch), NA, ch >= 6), FALSE))))
  .nzsc_group("aged_argillic_semiarid_nzsc", .nzsc_any(.and3v(a, red)), "S, SA",
              missing = c("designation", "munsell_hue_moist", "munsell_chroma_moist"))
}

# SZ: an argillic horizon with prismatic or blocky peds, more than 10 %
# coatings of value 4 or less, and pH 8.6 or more in some part.
solonetzic_semiarid_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; n <- x$n
  a <- .nzsc_argillic_rows(x)
  t <- tolower(as.character(h$structure_type))
  shape <- ifelse(grepl("block|prism|column", t), TRUE, ifelse(is.na(h$structure_type), NA, FALSE))
  films <- .nzsc_txt(h$clay_films_amount, n)
  # more than 10 % of ped faces: common or many coatings; few leaves it open
  much <- ifelse(grepl("common|many|continuous|abundant", films), TRUE,
          ifelse(grepl("very few|none|^no|absent", films), FALSE, NA))
  cv <- h$coatings_value_moist %||% rep(NA_real_, n)
  dark <- ifelse(is.na(cv), NA, cv <= 4)
  peds <- .nzsc_any(.and3v(.and3v(a, shape), .and3v(much, dark)))
  # pH 8.6 or more in some part (10 cm) of the argillic horizon
  arows <- which(!(a %in% FALSE) & !is.na(h$top_cm))
  ph <- if (!length(arows)) FALSE else .nzsc_some_part(x, arows, .and3v(a, ifelse(is.na(h$ph_h2o), NA, h$ph_h2o >= 8.6)))
  .nzsc_group("solonetzic_semiarid_nzsc", .and3(c(peds, ph)), "S, SZ",
              missing = c("clay_films_amount", "coatings_value_moist", "ph_h2o"))
}

# SJ: an argillic horizon, or a cutanic horizon that is a slowly permeable
# layer.
argillic_semiarid_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon)
  ok <- .any3v(.nzsc_argillic_rows(x), .and3v(.nzsc_cutanic_rows(x), .nzsc_slowly_permeable_rows(x)))
  .nzsc_group("argillic_semiarid_nzsc", .nzsc_any(ok), "S, SJ", missing = c("designation", "packing_degree"))
}

# ---- X, N: Oxidic and Granular Soils ------------------------------------------------------

nodular_oxidic_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon)
  pre <- .nzsc_ms_or_na(x, "nodular_oxidic_nzsc", "X, XN", "ferric_nodules_pct"); if (!is.null(pre)) return(pre)
  # more than 10 cm thick, with its top within (less than) 60 cm
  .nzsc_group("nodular_oxidic_nzsc", .nzsc_run_starting(x, .nzsc_nodular_rows(x), x$ms - 1000, x$ms + 59, 10),
              "X, XN", missing = c("designation", "ferric_nodules_pct"))
}

# NE: pH 5.9 or more in the major part of the B horizon to 60 cm.
melanic_granular_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h
  pre <- .nzsc_ms_or_na(x, "melanic_granular_nzsc", "N, NE", "ph_h2o"); if (!is.null(pre)) return(pre)
  b <- .nzsc_b_rows(x)
  if (!length(b)) return(.nzsc_group("melanic_granular_nzsc", .nzsc_none(x), "N, NE", missing = "ph_h2o"))
  bot <- min(x$ms + 60, if (anyNA(h$bottom_cm[b])) Inf else max(h$bottom_cm[b]))
  .nzsc_group("melanic_granular_nzsc", .nzsc_major_part_b(x, b, ifelse(is.na(h$ph_h2o), NA, h$ph_h2o >= 5.9), bot),
              "N, NE", missing = "ph_h2o")
}

# NX: a cutanoxidic horizon more than 30 cm thick with its upper boundary
# at 25 cm or more.
oxidic_granular_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon)
  pre <- .nzsc_ms_or_na(x, "oxidic_granular_nzsc", "N, NX", "designation"); if (!is.null(pre)) return(pre)
  .nzsc_group("oxidic_granular_nzsc", .nzsc_run_starting(x, .nzsc_cutanoxidic_rows(x), x$ms + 25, x$ms + 1000, 30),
              "N, NX", missing = c("designation", "ped_size_max_mm", "stickiness", "plasticity"))
}

# ---- P: Pallic Soils --------------------------------------------------------------------

pan_group_nzsc <- function(pedon, kind = c("duripan", "fragipan")) {
  kind <- match.arg(kind)
  x <- .nzsc_horizons(pedon)
  p <- .nzsc_pan_rows(x, kind)
  .nzsc_group("pan_group_nzsc", if (any(p)) TRUE else FALSE, paste("P,", kind), layers = which(p))
}

# PL: a brittle-B horizon and an argillic horizon predominantly of
# lamellae.
laminar_pallic_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h
  brittle <- any(.nzsc_pan_rows(x, "brittle"))
  a <- .nzsc_argillic_rows(x)
  lam <- grepl("lamell", tolower(as.character(h$designation)))
  th <- ifelse(is.na(x$thick), 0, x$thick)
  passed <- if (!brittle || !any(a %in% TRUE)) FALSE else sum(th[a %in% TRUE & lam]) > sum(th[a %in% TRUE & !lam])
  .nzsc_group("laminar_pallic_nzsc", passed, "P, PL", missing = "designation")
}

# PJ: an argillic horizon, or a cutanic horizon with sodic features within
# it or directly beneath it.
argillic_pallic_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon)
  cut <- .nzsc_cutanic_rows(x); sod <- .nzsc_sodic_rows(x); nxt <- .nzsc_next_row(x)
  sod_below <- vapply(seq_len(x$n), function(i) if (is.na(nxt[i])) FALSE else sod[nxt[i]], NA)
  ok <- .any3v(.nzsc_argillic_rows(x), .and3v(cut, .any3v(sod, sod_below)))
  .nzsc_group("argillic_pallic_nzsc", .nzsc_any(ok), "P, PJ", missing = c("designation", "esp_pct", "na_cmol"))
}

# ---- B: Brown Soils -------------------------------------------------------------------

# BL: a subhorizon of the B or BC, 10 cm or more thick with its top at 60
# cm or less, of allophanic soil material (its bulk density aside).
allophanic_brown_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; n <- x$n
  miss <- c("phosphate_retention_pct", "sensitivity_class", "strength_moist", "stickiness", "naf_test")
  pre <- .nzsc_ms_or_na(x, "allophanic_brown_nzsc", "B, BL", miss); if (!is.null(pre)) return(pre)
  sens <- .nzsc_class_index(h$sensitivity_class %||% rep(NA, n), "sensitivity")
  str <- .nzsc_class_index(h$strength_moist %||% rep(NA, n), "strength")
  stk <- .nzsc_class_index(h$stickiness %||% rep(NA, n), "stickiness")
  naf <- .nzsc_class_index(h$naf_test %||% rep(NA, n), "naf")
  pr <- h$phosphate_retention_pct %||% rep(NA_real_, n)
  route_a <- .and3v(.and3v(ifelse(is.na(sens), NA, sens >= 3), ifelse(is.na(str), NA, str %in% c(1, 2))),
                    .and3v(ifelse(is.na(stk), NA, stk <= 1), ifelse(is.na(naf), NA, naf >= 3)))
  allo <- ifelse(!is.na(pr), pr >= 85, route_a)
  bbc <- .nzsc_is(x, c("B", "BC")) & !x$des$m
  ok <- ifelse(bbc, allo, ifelse(x$unknown_role & x$mineral, ifelse(allo %in% FALSE, FALSE, NA), FALSE))
  .nzsc_group("allophanic_brown_nzsc", .nzsc_run_starting(x, ok, x$ms - 1000, x$ms + 60, 10, strict = FALSE),
              "B, BL", missing = miss)
}

# BX: in some part of the B within 60 cm, value 4 or less, friable or very
# friable failure and polyhedral peds of 10 mm or less.
oxidic_brown_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; n <- x$n
  miss <- c("munsell_value_moist", "consistence_moist", "structure_type", "ped_size_max_mm")
  pre <- .nzsc_ms_or_na(x, "oxidic_brown_nzsc", "B, BX", miss); if (!is.null(pre)) return(pre)
  b <- .nzsc_b_rows(x); b <- b[h$top_cm[b] < x$ms + 60]
  if (!length(b)) return(.nzsc_group("oxidic_brown_nzsc", .nzsc_none(x), "B, BX", missing = miss))
  cons <- .nzsc_txt(h$consistence_moist, n)
  friable <- ifelse(grepl("friable", cons) & !grepl("firm", cons), TRUE, ifelse(is.na(cons) | !nzchar(cons), NA, FALSE))
  t <- tolower(as.character(h$structure_type)); sz <- h$ped_size_max_mm %||% rep(NA_real_, n)
  poly <- ifelse(grepl("polyhed", t), ifelse(is.na(sz), NA, sz <= 10), ifelse(is.na(h$structure_type), NA, FALSE))
  ok <- .and3v(.and3v(ifelse(is.na(h$munsell_value_moist), NA, h$munsell_value_moist <= 4), friable), poly)
  .nzsc_group("oxidic_brown_nzsc", .nzsc_some_part_in(x, b, ok, -Inf, x$ms + 60), "B, BX", missing = miss)
}

# BF: a fragipan, or an apedal subhorizon of the B or BC slightly firm or
# stronger (moist), with its top within 90 cm.
firm_brown_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; n <- x$n
  miss <- c("structure_grade", "structure_type", "strength_moist")
  pre <- .nzsc_ms_or_na(x, "firm_brown_nzsc", "B, BF", miss); if (!is.null(pre)) return(pre)
  rows <- .nzsc_rows(x); rows <- rows[h$top_cm[rows] < x$ms + 90]
  frag <- .nzsc_pan_rows(x, "fragipan")
  str <- .nzsc_class_index(h$strength_moist %||% rep(NA, n), "strength")
  apedal <- ifelse(.nzsc_pedal(h) %in% FALSE, TRUE, ifelse(.nzsc_pedal(h) %in% TRUE, FALSE, NA))
  bbc <- .nzsc_is(x, c("B", "BC"))
  f <- .and3v(apedal, ifelse(is.na(str), NA, str >= 3))
  firm <- ifelse(bbc, f, ifelse(x$unknown_role & x$mineral, ifelse(f %in% FALSE, FALSE, NA), FALSE))
  .nzsc_group("firm_brown_nzsc", .nzsc_any(.any3v(frag, firm)[rows]), "B, BF", missing = miss)
}

# ---- A: Anthropic Soils ------------------------------------------------------------------

# AT: truncated by people at least to the base of an in situ A or AB (the
# first in situ horizon is a B or deeper), with any relocated material over
# it less than 30 cm thick.
truncated_anthropic_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; n <- x$n
  tr <- .nzsc_yes(.nzsc_site1(pedon, "anthropic_truncation"))
  if (!isTRUE(tr)) return(.nzsc_group("truncated_anthropic_nzsc", FALSE, "A, AT"))
  orig <- .nzsc_txt(h$layer_origin, n)
  moved <- x$des$anthropic | grepl("fill|spoil|dredg|landscap|topsoil import|relocat", orig)
  rows <- .nzsc_rows(x); insitu <- rows[!moved[rows] & x$mineral[rows]]
  if (!length(insitu)) return(.nzsc_group("truncated_anthropic_nzsc", NA, "A, AT", missing = "designation"))
  i <- insitu[1L]
  c1 <- if (x$unknown_role[i]) NA else !(x$role[i] %in% c("A", "AB", "A/B"))
  c2 <- h$top_cm[i] <= x$soil_top + 30
  .nzsc_group("truncated_anthropic_nzsc", .and3(c(c1, c2)), "A, AT", missing = "designation")
}

# AO: horizons modified through Maori cultural practice, 30 cm or more in
# all, with the upper boundary at the surface or within 90 cm if buried.
maori_anthropic_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h
  m <- .nzsc_yes(h$maori_cultural_layer %||% rep(NA, x$n)) %in% TRUE
  if (!any(m)) return(.nzsc_group("maori_anthropic_nzsc", FALSE, "A, AO"))
  th <- sum(pmax(0, x$thick[m]), na.rm = TRUE)
  tops <- h$top_cm[m & !is.na(h$top_cm)]
  ok <- th >= 30 && length(tops) && min(tops) < x$soil_top + 90
  .nzsc_group("maori_anthropic_nzsc", isTRUE(ok), "A, AO", layers = which(m))
}

# AR: a layer of natural organic waste or manufactured organic matter
# (plastics included) 30 cm or more thick within 90 cm, or methane that can
# be smelt.
refuse_anthropic_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; n <- x$n
  methane <- isTRUE(.nzsc_yes(.nzsc_site1(pedon, "methane_odour")))
  orig <- .nzsc_txt(h$layer_origin, n)
  refuse <- grepl("refuse|garbage|garbic|landfill|rubbish|waste|plastic", orig)
  # 30 cm or more thick, with its top within (less than) 90 cm
  layer <- if (any(refuse)) .nzsc_run_starting(x, refuse, x$soil_top - 1000, x$soil_top + 89, 30, strict = FALSE) else FALSE
  .nzsc_group("refuse_anthropic_nzsc", methane || isTRUE(layer), "A, AR", layers = which(refuse))
}

# AM: original horizons mechanically disturbed below 30 cm.
mixed_anthropic_nzsc <- function(pedon) {
  d <- suppressWarnings(as.numeric(.nzsc_site1(pedon, "mechanical_mixing_depth_cm")))
  .nzsc_group("mixed_anthropic_nzsc", if (is.na(d)) FALSE else d > 30, "A, AM")
}

# ---- R, W: Recent and Raw Soils -----------------------------------------------------------

# RX: a lithic or paralithic contact directly beneath the A horizon, or a
# weathered-B or unconsolidated C horizon less than 10 cm thick over it.
rocky_recent_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h
  pre <- .nzsc_ms_or_na(x, "rocky_recent_nzsc", "R, RX", "designation"); if (!is.null(pre)) return(pre)
  contact <- which(x$role %in% c("R", "CR") & !is.na(h$top_cm))
  if (!length(contact)) return(.nzsc_group("rocky_recent_nzsc", FALSE, "R, RX"))
  ab <- .nzsc_a_base(x)
  gap <- min(h$top_cm[contact]) - ab
  .nzsc_group("rocky_recent_nzsc", if (is.na(gap)) NA else gap < 10, "R, RX", missing = "designation")
}

# WG: within 30 cm, the top of a reductimorphic horizon, a positive Childs'
# test, or saturation for 30 or more days.
gley_raw_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; n <- x$n
  miss <- c("designation", "childs_test", "water_saturation_days")
  pre <- .nzsc_ms_or_na(x, "gley_raw_nzsc", "W, WG", miss); if (!is.null(pre)) return(pre)
  rows <- .nzsc_rows(x); rows <- rows[h$top_cm[rows] >= x$ms - 0.5 & h$top_cm[rows] <= x$ms + 30]
  red <- .nzsc_reductimorphic_rows(x)
  ch <- .nzsc_txt(h$childs_test, n)
  cpos <- grepl("^pos|^yes|^true|^\\+", ch)
  wsd <- h$water_saturation_days %||% rep(NA_real_, n)
  sat <- !is.na(wsd) & wsd >= 30
  .nzsc_group("gley_raw_nzsc", .any3(c(red[rows], cpos[rows], sat[rows])), "W, WG", missing = miss)
}

# WW: surface water for the majority of the time, but not constantly.
hydric_raw_nzsc <- function(pedon) {
  sw <- tolower(as.character(.nzsc_site1(pedon, "surface_water")))
  .nzsc_group("hydric_raw_nzsc", !is.na(sw) && grepl("\\bmost\\b|majority", sw) && !grepl("almost|constant", sw), "W, WW")
}

# WC: permafrost or cryoturbation within 1 m of the soil surface.
cryic_raw_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; n <- x$n
  pf <- h$permafrost_temp_C %||% rep(NA_real_, n); ice <- h$ice_pct %||% rep(NA_real_, n)
  d <- .nzsc_txt(h$designation, n)
  cryo <- (!is.na(pf) & pf <= 0) | (!is.na(ice) & ice > 0) | grepl("jj|@", d)
  .nzsc_group("cryic_raw_nzsc", any(cryo & !is.na(h$top_cm) & h$top_cm < x$soil_top + 100), "W, WC")
}

# WX: a lithic or paralithic contact within 30 cm of the soil surface.
rocky_raw_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h
  .nzsc_group("rocky_raw_nzsc", any(x$role %in% c("R", "CR") & !is.na(h$top_cm) & h$top_cm <= x$soil_top + 30),
              "W, WX")
}

# WT: predominantly gravel-sized mafic or intermediate (basaltic,
# andesitic) tephric soil material from the soil surface to 30 cm or more.
tephric_raw_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; n <- x$n
  miss <- c("layer_origin", "rock_origin", "coarse_fragments_pct")
  t <- .nzsc_tephric_rows(pedon, x)
  org <- paste(.nzsc_txt(h$layer_origin, n), .nzsc_txt(h$rock_origin, n), tolower(as.character(.nzsc_site1(pedon, "parent_material") %|NA|% "")))
  mafic <- grepl("basalt|andesit|scoria|mafic|intermediate", org)
  cf <- h$coarse_fragments_pct
  gravel <- ifelse(is.na(cf), NA, cf >= 50)
  ok <- .and3v(.and3v(t, gravel), mafic)
  .nzsc_group("tephric_raw_nzsc", .nzsc_all_in(x, ok, x$soil_top, x$soil_top + 30), "W, WT", missing = miss)
}
