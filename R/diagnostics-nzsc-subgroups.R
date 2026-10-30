# =============================================================================
# v0.9.299 -- the keys to subgroups of the New Zealand Soil Classification,
# 4th ed.: the Organic, Gley and Ultic Soils (70 subgroups of 16 groups).
# Each subgroup after the first is "other soils that ...", so a gate tests
# only its own criteria; the last subgroup of a group is the default, except
# under the Densipan and Perch-gley Ultic Soils (UD, UP), which the text
# gives no default: a profile that passes none keeps the group name.
# Three-valued; readings in inst/rules/nzsc4/readings.yaml. Hugo's decisions
# of 2026-10-28: sulfidic soil material not measured is absent when no
# horizon within the window has its field look (a reductimorphic horizon of
# hue 5Y or less red, or black); saline soil material not measured is absent
# when the horizons are designated and none carries z; the Sphagnum share of
# the fibres and a shelly layer are recorded when met.
# =============================================================================

# ---- materials and features ------------------------------------------------------

# Sulfidic soil material of each row: more than 0.01 % inorganic sulfide S
# when measured. Not measured: the text says sulfidic material in the field
# is waterlogged and meets the reductimorphic horizon with a hue of 5Y or
# less red, or is black; a row that cannot look so is not sulfidic, and one
# that can stays undecided (Hugo's decision, 2026-10-28).
.nzsc_sulfidic_rows <- function(x) {
  h <- x$h; n <- x$n
  s <- h$sulfidic_s_pct %||% rep(NA_real_, n)
  hue_txt <- toupper(trimws(as.character(h$munsell_hue_moist %||% rep(NA_character_, n))))
  neutral <- !is.na(hue_txt) & grepl("^N", hue_txt)
  hue <- .nzsc_hue_num(hue_txt); v <- h$munsell_value_moist; ch <- h$munsell_chroma_moist
  black <- !is.na(v) & v <= 2.5 & (neutral | (!is.na(ch) & ch <= 1))
  # 5Y or less red: 5Y, 10Y, GY, G, BG, B, PB (purple and red-purple are
  # redder again)
  # a redder hue with the value or chroma not recorded may still be black
  maybe_black <- is.na(v) | (v <= 2.5 & is.na(ch))
  look <- ifelse(neutral | black, TRUE, ifelse(is.na(hue), NA,
          ifelse(hue >= 25 & hue < 80, TRUE, ifelse(maybe_black, NA, FALSE))))
  red <- ifelse(x$role %in% c("R", "CR"), FALSE, .nzsc_reductimorphic_rows(x))
  sign <- .and3v(red, look)
  ifelse(!is.na(s), s > 0.01, ifelse(sign %in% FALSE, FALSE, NA))
}

# Saline soil material of each row: EC of a 1:5 soil:water extract above 0.8
# dS/m (0.8 mS/cm), or the notation suffix z. Not measured: a designated
# horizon with no z is not saline (the notation marks it); a row with no
# designation stays undecided (Hugo's decision, 2026-10-28). The
# saturated-paste EC (ec_dS_m) is not read: the text names the 1:5 extract.
.nzsc_saline_rows <- function(x) {
  ec <- x$h$ec_1to5_dS_m %||% rep(NA_real_, x$n)
  ifelse(x$des$z, TRUE, ifelse(!is.na(ec), ec > 0.8, ifelse(x$known, FALSE, NA)))
}

# Concretions or nodules of each row: more than 2 % (ferric, manganiferous
# and bauxitic nodules together), or the i suffix; recorded when met.
.nzsc_concretion_rows <- function(x) {
  h <- x$h; n <- x$n
  nod <- vapply(c("ferric_nodules_pct", "manganiferous_nodules_pct", "bauxitic_nodules_pct"),
                function(k) { v <- h[[k]] %||% rep(NA_real_, n); ifelse(is.na(v), 0, v) }, numeric(n))
  nod <- if (is.matrix(nod)) rowSums(nod) else sum(nod)
  nod > 2 | x$des$i
}

# A shelly layer (recorded when met: shelly_layer).
.nzsc_shelly_rows <- function(x) .nzsc_yes(x$h$shelly_layer %||% rep(NA, x$n)) %in% TRUE

# Redox-mottled horizon of the (g) kind: 2 % or more redox segregations
# with low-chroma colours on less than half of the matrix (notation (g)).
# The mottled profile form and the UDM and UEM keys ask for this kind; the
# 4th edition excludes (f) horizons from the mottled profile form (its
# Changelog). A designated horizon is read by its notation, as the
# reductimorphic horizon is; a row with no designation and 2 % or more
# segregations stays undecided (the share of low-chroma colours is not
# recorded), and one with none recorded has none (recorded when met).
.nzsc_redox_mottled_g_rows <- function(x) {
  pct <- x$h$redoximorphic_features_pct %||% rep(NA_real_, x$n)
  red <- .nzsc_reductimorphic_rows(x)
  ifelse(x$des$g_weak, TRUE, ifelse(x$known | red %in% TRUE, FALSE,
         ifelse(is.na(pct) | pct < 2, FALSE, NA)))
}

# Mottled profile form: a redox-mottled horizon (the (g) kind) within 30 cm
# of the mineral soil surface, or a reductimorphic horizon with its upper
# boundary at 30 cm or more and less than 60 cm.
.nzsc_mottled_profile_form <- function(x) {
  h <- x$h; ms <- x$ms
  if (is.na(ms)) return(NA)
  rows <- .nzsc_rows(x)
  near <- rows[h$top_cm[rows] >= ms - 0.5 & h$top_cm[rows] < ms + 30]
  p1 <- .nzsc_any(.nzsc_redox_mottled_g_rows(x)[near])
  # the upper boundary of the reductimorphic horizon: a row whose run does
  # not start higher up
  red <- .nzsc_reductimorphic_rows(x)
  cand <- rows[h$top_cm[rows] >= ms + 30 - 0.5 & h$top_cm[rows] < ms + 60]
  p2 <- .nzsc_any(vapply(cand, function(i) {
    up <- rows[!is.na(h$bottom_cm[rows]) & abs(h$bottom_cm[rows] - h$top_cm[i]) <= 0.5]
    if (any(red[up] %in% TRUE)) return(FALSE)
    if (isTRUE(red[i]) && any(is.na(red[up]))) NA else red[i]
  }, NA))
  .any3(c(p1, p2))
}

# The description ends above zb (and not on rock or a hard layer): the part
# of the window below it may be anything, as .nzsc_cum reads it.
.nzsc_short_of <- function(x, zb) {
  rows <- .nzsc_rows(x)
  if (!length(rows)) return(TRUE)
  bot <- x$h$bottom_cm[rows]
  low <- if (anyNA(bot)) Inf else max(bot)
  low < zb - 0.5 && !.nzsc_ends_hard(x)
}
# A FALSE over a window the description does not reach becomes NA.
.nzsc_open_below <- function(v, x, zb) if (isFALSE(v) && .nzsc_short_of(x, zb)) NA else v

# Rows in depth order that overlap [zt, zb).
.nzsc_rows_in <- function(x, zt, zb) {
  h <- x$h; rows <- .nzsc_rows(x)
  rows[h$top_cm[rows] < zb - 0.5 & (is.na(h$bottom_cm[rows]) | h$bottom_cm[rows] > zt + 0.5)]
}

# Coatings on 10 % or more of the ped faces: common or many coatings decide;
# few leaves it open; none recorded is none unless the notation marks
# coatings (t, h, (h)).
.nzsc_coatings_10 <- function(x) {
  films <- .nzsc_txt(x$h$clay_films_amount, x$n)
  ifelse(grepl("common|many|continuous|abundant", films), TRUE,
  ifelse(grepl("very few|none|^no|absent", films), FALSE,
  ifelse(grepl("few", films), NA,
  ifelse(x$des$t | x$des$h | x$des$h_weak, NA, FALSE))))
}

# Dark-coloured coatings: moist value 4 or less; `value5`: or value 5 with
# chroma 3 (GAPH, GAH), undecided as the coatings chroma is not recorded.
# Humus coatings (h, (h)) are dark by definition.
.nzsc_dark_coatings <- function(x, value5 = TRUE) {
  cv <- x$h$coatings_value_moist %||% rep(NA_real_, x$n)
  ifelse(x$des$h | x$des$h_weak, TRUE, ifelse(is.na(cv), NA, ifelse(cv <= 4, TRUE,
         ifelse(cv <= 5 & isTRUE(value5), NA, FALSE))))
}

# Texture class from the field texture, else from lab clay, silt and sand
# (the USDA triangle).
.nzsc_texture_class <- function(x) {
  cls <- x$tex$class
  ab <- texture_class_from_pct(x$h$clay_pct, x$h$silt_pct, x$h$sand_pct)
  lab <- unname(c(S = "sand", LS = "loamy sand", SL = "sandy loam", L = "loam", SIL = "silt loam",
                  SI = "silt", SCL = "sandy clay loam", CL = "clay loam", SICL = "silty clay loam",
                  SC = "sandy clay", SIC = "silty clay", C = "clay")[ab])
  ifelse(is.na(cls), lab, cls)
}

# Less silt than clay: lab silt and clay; else the texture class where it
# settles it.
.nzsc_less_silt_than_clay <- function(x) {
  h <- x$h; cls <- .nzsc_texture_class(x)
  ifelse(!is.na(h$silt_pct) & !is.na(h$clay_pct), h$silt_pct < h$clay_pct,
  ifelse(!is.na(h$clay_pct) & h$clay_pct > 50, TRUE,
  ifelse(!is.na(h$silt_pct) & h$silt_pct > 50, FALSE,
  ifelse(cls %in% c("clay", "sandy clay"), TRUE,
  ifelse(cls %in% c("silt", "silt loam", "silty clay loam", "loam"), FALSE, NA)))))
}

# Peat material of each row, as the peaty topsoil reads it (peat or peaty
# textures): carbon decides when measured; otherwise a recorded mineral
# field texture is not peat (v0.9.299).
.nzsc_peat_rows <- function(x) {
  h <- x$h; n <- x$n
  oc <- h$oc_pct; clay <- h$clay_pct; ft <- .nzsc_txt(h$field_texture, n)
  mineral_ft <- !is.na(ft) & nzchar(ft) & !grepl("peat", ft)
  ifelse(grepl("slightly peaty", ft), ifelse(is.na(clay), NA, clay < 18),
  ifelse(grepl("peat", ft) | x$role %in% "O", TRUE,
  ifelse(x$role %in% "L", FALSE,
  ifelse(!is.na(oc), ifelse(oc >= 17.4, TRUE,
                     ifelse(oc >= 9.9, ifelse(is.na(clay), NA, clay < 18), FALSE)),
  ifelse(mineral_ft, FALSE, NA)))))
}

# A peaty topsoil at the surface, or one buried (the text: "buried by a
# surface layer of new material of up to 60 cm") with its upper surface
# within 60 cm of z0 (the soil surface or the mineral soil surface, as the
# subgroup says).
.nzsc_peaty_or_buried <- function(pedon, x, z0) {
  surf <- .nzsc_peaty_topsoil(pedon, x)
  if (isTRUE(surf)) return(TRUE)
  if (is.na(z0)) return(NA)
  h <- x$h; rows <- .nzsc_rows(x); peat <- .nzsc_peat_rows(x)
  start <- x$ord[!(x$role[x$ord] %in% "L")]
  s0 <- if (length(start)) h$top_cm[start[1L]] else NA
  # a buried topsoil is an O or A horizon (or a row of unknown role), not a
  # designated E, B or C
  top_like <- x$role %in% c("O", "A") | x$unknown_role
  cand <- rows[h$top_cm[rows] > s0 + 0.5 & h$top_cm[rows] < z0 + 60 & !(peat[rows] %in% FALSE) & top_like[rows]]
  bur <- vapply(cand, function(i) {
    up <- rows[!is.na(h$bottom_cm[rows]) & abs(h$bottom_cm[rows] - h$top_cm[i]) <= 0.5]
    # buried under new material: the run starts higher up only when the row
    # above is itself a peaty topsoil row (an O or A)
    if (!length(up) || any(peat[up] %in% TRUE & top_like[up])) return(FALSE)
    v <- .nzsc_peaty_run(pedon, x, i, peat)
    if (isTRUE(v) && any(is.na(peat[up]) & top_like[up])) {
      # the row above may be peat: the run would then start there
      pu <- peat; pu[up] <- TRUE
      if (isTRUE(.nzsc_peaty_run(pedon, x, up[1L], pu))) TRUE else NA
    } else v
  }, NA)
  .any3(c(surf, bur))
}

# ---- shared subgroup gates ---------------------------------------------------------

.nzsc_subgroup <- function(name, passed, where, evidence = list(), missing = character(0), layers = integer(0)) {
  .nzsc_result(name, passed, layers, evidence, if (is.na(passed)) missing else character(0),
               paste("Key to subgroups,", where))
}
.nzsc_sub_ms_or_na <- function(x, name, where, missing) {
  if (is.na(x$ms)) .nzsc_subgroup(name, NA, where, missing = missing) else NULL
}

# Sulfidic subgroups (OFU, OMU, OHU from the soil surface; GSU, GRU, GOU
# from the mineral soil surface): sulfidic soil material in some part
# within 90 cm.
sulfidic_subgroup_nzsc <- function(pedon, from = c("mineral_surface", "soil_surface")) {
  from <- match.arg(from)
  x <- .nzsc_horizons(pedon)
  miss <- c("sulfidic_s_pct", "munsell_hue_moist")
  z0 <- if (from == "soil_surface") x$soil_top else x$ms
  if (is.na(z0)) return(.nzsc_subgroup("sulfidic_subgroup_nzsc", NA, "sulfidic subgroups", missing = miss))
  s <- .nzsc_sulfidic_rows(x)
  rows <- .nzsc_rows_in(x, z0, z0 + 90)
  .nzsc_subgroup("sulfidic_subgroup_nzsc", .nzsc_open_below(.nzsc_some_part_in(x, rows, s, z0, z0 + 90), x, z0 + 90),
                 "sulfidic subgroups", list(from = from), miss, which(s %in% TRUE))
}

# Saline subgroups (GSQ, GRQ, GOQ): saline soil material within 60 cm of
# the mineral soil surface.
saline_subgroup_nzsc <- function(pedon, depth = 60) {
  x <- .nzsc_horizons(pedon)
  miss <- c("ec_1to5_dS_m", "designation")
  pre <- .nzsc_sub_ms_or_na(x, "saline_subgroup_nzsc", "saline subgroups", miss); if (!is.null(pre)) return(pre)
  s <- .nzsc_saline_rows(x)
  # `depth` 30 for the Gley Raw Soils (v0.9.302)
  zb <- x$ms + as.numeric(depth)
  .nzsc_subgroup("saline_subgroup_nzsc", .nzsc_open_below(.nzsc_any(s[.nzsc_rows_in(x, x$ms, zb)]), x, zb),
                 "saline subgroups", missing = miss, layers = which(s %in% TRUE))
}

# Fluid subgroups (GUF, GRF; GUFQ also saline): a moderately or very fluid
# horizon within 60 cm of the mineral soil surface (and saline soil material
# within 60 cm, not necessarily in the same horizon).
fluid_subgroup_nzsc <- function(pedon, saline = FALSE, depth = 60, limit = c("within", "at_or_less")) {
  limit <- match.arg(limit)
  x <- .nzsc_horizons(pedon)
  miss <- c("fluidity_class", if (isTRUE(saline)) c("ec_1to5_dS_m", "designation"))
  pre <- .nzsc_sub_ms_or_na(x, "fluid_subgroup_nzsc", "fluid subgroups", miss); if (!is.null(pre)) return(pre)
  # `depth` 30 for the Gley Raw Soils, "at a depth of 30 cm or less" (WGFU,
  # `limit: at_or_less`) or "within 30 cm" (WGFQ, WGF; v0.9.302)
  zb <- x$ms + as.numeric(depth)
  rows <- .nzsc_rows_in(x, x$ms, if (limit == "within") zb else zb + 1)
  f <- .nzsc_any(.nzsc_fluid_rows(x)[rows])
  s <- if (isTRUE(saline)) .nzsc_open_below(.nzsc_any(.nzsc_saline_rows(x)[rows]), x, zb) else TRUE
  .nzsc_subgroup("fluid_subgroup_nzsc", .and3(c(f, s)), "fluid subgroups",
                 list(fluid = f, saline = if (isTRUE(saline)) s else NULL), miss)
}

# Peaty subgroups: a peaty topsoil at the surface or buried with its upper
# surface within 60 cm of the soil surface (GUO, GRO, GAO, GOO) or of the
# mineral soil surface (GTO, GSO); `sulfidic`: and sulfidic soil material in
# some part within 90 cm of the mineral soil surface (GSOU, GROU, GOOU).
# `buried = FALSE`: a peaty topsoil at the surface only, where the text names
# no buried one (LGO, ERO; v0.9.300).
peaty_subgroup_nzsc <- function(pedon, from = c("soil_surface", "mineral_surface"), sulfidic = FALSE,
                                buried = TRUE) {
  from <- match.arg(from)
  x <- .nzsc_horizons(pedon)
  miss <- c("oc_pct", "field_texture", "site$drainage_class", if (isTRUE(sulfidic)) "sulfidic_s_pct")
  z0 <- if (from == "soil_surface") x$soil_top else x$ms
  p <- if (isTRUE(buried)) .nzsc_peaty_or_buried(pedon, x, z0) else .nzsc_peaty_topsoil(pedon, x)
  s <- if (!isTRUE(sulfidic)) TRUE else if (is.na(x$ms)) NA else {
    sr <- .nzsc_sulfidic_rows(x)
    .nzsc_open_below(.nzsc_some_part_in(x, .nzsc_rows_in(x, x$ms, x$ms + 90), sr, x$ms, x$ms + 90), x, x$ms + 90)
  }
  .nzsc_subgroup("peaty_subgroup_nzsc", .and3(c(p, s)), "peaty subgroups",
                 list(peaty_topsoil = p, sulfidic = if (isTRUE(sulfidic)) s else NULL), miss)
}

# GUSQ: sandy by weighted average to 90 cm from the mineral soil surface,
# and saline soil material within 60 cm of it. The weighted average is taken
# from lab silt and clay when every horizon of the window has them;
# otherwise a window all sandy (or with no sandy horizon) decides.
sandy_saline_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h
  miss <- c("clay_pct", "silt_pct", "field_texture", "ec_1to5_dS_m")
  pre <- .nzsc_sub_ms_or_na(x, "sandy_saline_nzsc", "G, GUSQ", miss); if (!is.null(pre)) return(pre)
  zt <- x$ms; zb <- x$ms + 90
  rows <- .nzsc_rows_in(x, zt, zb)
  w <- .asc_overlap(h$top_cm[rows], ifelse(is.na(h$bottom_cm[rows]), zb, h$bottom_cm[rows]), zt, zb)
  sandy <- if (length(rows) && sum(w) > 0 && !anyNA(h$silt_pct[rows]) && !anyNA(h$clay_pct[rows])) {
    sum(w * (h$silt_pct[rows] + 2 * h$clay_pct[rows])) / sum(w) < 30
  } else {
    g <- x$tex$group[rows]
    if (!length(rows)) NA else if (all(g %in% "sandy")) TRUE else if (!any(g %in% "sandy") && !anyNA(g)) FALSE else NA
  }
  if (.nzsc_short_of(x, zb)) sandy <- NA
  sal <- .nzsc_open_below(.nzsc_any(.nzsc_saline_rows(x)[.nzsc_rows_in(x, x$ms, x$ms + 60)]), x, x$ms + 60)
  .nzsc_subgroup("sandy_saline_nzsc", .and3(c(sandy, sal)), "G, GUSQ", list(sandy = sandy, saline = sal), miss)
}

# Acidic subgroups (GTA, GSA, GRA, GOA): pH below 5.5 in some part from the
# base of the A horizon to 60 cm from the mineral soil surface.
acidic_subgroup_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h
  pre <- .nzsc_sub_ms_or_na(x, "acidic_subgroup_nzsc", "acidic subgroups", "ph_h2o"); if (!is.null(pre)) return(pre)
  zb <- x$ms + 60
  # rock (R, CR) is not read for pH (v0.9.302)
  acid <- ifelse(x$role %in% c("R", "CR"), FALSE, ifelse(is.na(h$ph_h2o), NA, h$ph_h2o < 5.5))
  at <- function(z) if (is.na(z)) NA else if (z >= zb - 0.5) FALSE else
    .nzsc_some_part_in(x, .nzsc_rows_in(x, z, zb), acid, z, zb)
  # mineral rows of unknown role directly under the A may be A too: the
  # base may lie lower, and the verdict holds only when both agree (v0.9.300:
  # a row of no thickness no longer stops the walk forever)
  zz <- .nzsc_a_bases(x); zt <- zz[1L]; zt2 <- zz[2L]
  v1 <- at(zt); v2 <- at(zt2)
  passed <- if (identical(v1, v2)) v1 else NA
  .nzsc_subgroup("acidic_subgroup_nzsc", passed, "acidic subgroups", list(from = zt, to = zb), "ph_h2o")
}

# Concretionary subgroups (GSC): more than 2 % concretions in some horizon
# at 60 cm or less from the mineral soil surface (a top at 60 cm included).
concretionary_subgroup_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon)
  miss <- c("ferric_nodules_pct", "manganiferous_nodules_pct")
  pre <- .nzsc_sub_ms_or_na(x, "concretionary_subgroup_nzsc", "concretionary subgroups", miss); if (!is.null(pre)) return(pre)
  cc <- .nzsc_concretion_rows(x)
  .nzsc_subgroup("concretionary_subgroup_nzsc", .nzsc_any(cc[.nzsc_rows_in(x, x$ms, x$ms + 61)]),
                 "concretionary subgroups", missing = miss, layers = which(cc))
}

# Calcareous subgroups: a calcareous horizon or a shelly layer within 60 cm
# of the mineral soil surface (GRC, `shelly`); a calcareous horizon at 60 cm
# or less (GOC, `limit: at_or_less`, a top at 60 cm included); `depth` 90
# for the Melanic Soils (EVC within 90 cm; EOC with an upper surface at 90
# cm or less; v0.9.300).
calcareous_subgroup_nzsc <- function(pedon, shelly = FALSE, limit = c("within", "at_or_less"), depth = 60,
                                     anywhere = FALSE) {
  limit <- match.arg(limit)
  x <- .nzsc_horizons(pedon)
  miss <- c("effervescence_class", "caco3_pct", "ph_h2o")
  # `anywhere` (PLC, PXJC; v0.9.301): a calcareous horizon at any depth
  if (isTRUE(anywhere)) {
    calc <- .nzsc_calcareous_rows(x); rows <- .nzsc_rows(x); rows <- rows[x$mineral[rows]]
    return(.nzsc_subgroup("calcareous_subgroup_nzsc", .nzsc_any(calc[rows]), "calcareous subgroups",
                          missing = miss, layers = which(calc %in% TRUE)))
  }
  pre <- .nzsc_sub_ms_or_na(x, "calcareous_subgroup_nzsc", "calcareous subgroups", miss); if (!is.null(pre)) return(pre)
  d <- as.numeric(depth)
  rows <- .nzsc_rows_in(x, x$ms, x$ms + if (limit == "within") d else d + 1)
  calc <- .nzsc_calcareous_rows(x)
  sh <- if (isTRUE(shelly)) any(.nzsc_shelly_rows(x)[rows]) else FALSE
  .nzsc_subgroup("calcareous_subgroup_nzsc", .any3(c(.nzsc_any(calc[rows]), sh)), "calcareous subgroups",
                 list(shelly = sh), miss, which(calc %in% TRUE))
}

# Pan subgroups (GOI): a pan of the kind at `depth` cm or less from the
# mineral soil surface; `limit: within` (LPI, LIMI, MPU), with its top less
# than `depth` cm down; `anywhere`: at any depth (the Podzol subgroups name
# no depth; v0.9.300).
pan_subgroup_nzsc <- function(pedon, kind = c("ironstone_pan", "placic", "humus_pan", "ortstein", "fragipan", "duripan"),
                              depth = 90, anywhere = FALSE, limit = c("at_or_less", "within")) {
  kind <- match.arg(kind); limit <- match.arg(limit)
  x <- .nzsc_horizons(pedon); h <- x$h
  p <- .nzsc_pan_rows(x, kind)
  rows <- .nzsc_rows(x)
  if (isTRUE(anywhere))
    return(.nzsc_subgroup("pan_subgroup_nzsc", any(p[rows]), "pan subgroups", list(kind = kind), layers = which(p)))
  pre <- .nzsc_sub_ms_or_na(x, "pan_subgroup_nzsc", "pan subgroups", c("designation", "pan_type")); if (!is.null(pre)) return(pre)
  rows <- rows[if (limit == "within") h$top_cm[rows] < x$ms + as.numeric(depth) - 0.5
                else h$top_cm[rows] <= x$ms + as.numeric(depth) + 0.5]
  .nzsc_subgroup("pan_subgroup_nzsc", any(p[rows]), "pan subgroups", list(kind = kind), layers = which(p))
}

# Mottled subgroups (USM, UYM): a mottled profile form.
mottled_subgroup_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon)
  miss <- c("designation", "redoximorphic_features_pct", "munsell_chroma_moist")
  .nzsc_subgroup("mottled_subgroup_nzsc", .nzsc_mottled_profile_form(x), "mottled subgroups", missing = miss)
}

# Gley-profile subgroups (OLBG): a reductimorphic horizon less than 30 cm
# below the upper surface of the (underlying) mineral soil.
gley_profile_subgroup_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon)
  v <- .nzsc_gley_profile_form(x)
  if (!is.na(x$ms)) v <- .nzsc_open_below(v, x, x$ms + 30)
  .nzsc_subgroup("gley_profile_subgroup_nzsc", v, "gley-profile subgroups",
                 missing = c("designation", "munsell_value_moist", "munsell_chroma_moist"))
}

# Nodular subgroups (GXN): a nodular horizon (more than 5 % nodules in a
# layer more than 10 cm thick) with its upper boundary within 60 cm of the
# mineral soil surface.
nodular_subgroup_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon)
  pre <- .nzsc_sub_ms_or_na(x, "nodular_subgroup_nzsc", "nodular subgroups", "ferric_nodules_pct"); if (!is.null(pre)) return(pre)
  .nzsc_subgroup("nodular_subgroup_nzsc", .nzsc_run_starting(x, .nzsc_nodular_rows(x), x$ms - 1000, x$ms + 59, 10),
                 "nodular subgroups", missing = c("designation", "ferric_nodules_pct"))
}

# Argillic subgroups (GOJ): an argillic horizon; `min_cm` (SAH, v0.9.301):
# adjacent argillic rows that thick or more.
argillic_subgroup_nzsc <- function(pedon, min_cm = 0) {
  x <- .nzsc_horizons(pedon)
  # a pan (m) is not an argillic horizon
  a <- ifelse(x$des$m, FALSE, .nzsc_argillic_rows(x))
  v <- if (as.numeric(min_cm) > 0) .nzsc_run_any(x, a, as.numeric(min_cm), strict = FALSE) else .nzsc_any(a)
  .nzsc_subgroup("argillic_subgroup_nzsc", v, "argillic subgroups", list(min_cm = min_cm),
                 missing = c("designation", "clay_pct", "clay_films_amount"), layers = which(a %in% TRUE))
}

# Below-a-layer subgroups of the Ultic Soils: a reductimorphic horizon below
# the densipan (UDP) or the eluvial horizon (UEP); a redox-mottled horizon of
# the (g) kind immediately under the densipan (UDM) or below the eluvial
# horizon (UEM).
below_layer_subgroup_nzsc <- function(pedon, layer = c("densipan", "eluvial"),
                                      feature = c("reductimorphic", "redox_mottled"), immediately = FALSE) {
  layer <- match.arg(layer); feature <- match.arg(feature)
  x <- .nzsc_horizons(pedon); h <- x$h
  miss <- c("designation", "redoximorphic_features_pct")
  lay <- if (layer == "densipan") .nzsc_densipan_rows(x) else .nzsc_is(x, "E")
  if (any(lay %in% TRUE & (is.na(h$top_cm) | is.na(h$bottom_cm))))
    return(.nzsc_subgroup("below_layer_subgroup_nzsc", NA, "Ultic subgroups", missing = c(miss, "top_cm", "bottom_cm")))
  lr <- which(lay %in% TRUE & !is.na(h$top_cm))
  if (!length(lr)) return(.nzsc_subgroup("below_layer_subgroup_nzsc", if (anyNA(lay)) NA else FALSE,
                                         "Ultic subgroups", missing = miss))
  base <- max(h$bottom_cm[lr])
  if (is.na(base)) return(.nzsc_subgroup("below_layer_subgroup_nzsc", NA, "Ultic subgroups", missing = miss))
  feat <- if (feature == "reductimorphic") .nzsc_reductimorphic_rows(x) else .nzsc_redox_mottled_g_rows(x)
  rows <- .nzsc_rows(x)
  below <- if (isTRUE(immediately)) rows[abs(h$top_cm[rows] - base) <= 0.5] else rows[h$top_cm[rows] >= base - 0.5]
  passed <- if (!length(below)) { if (.nzsc_ends_hard(x)) FALSE else NA } else .nzsc_any(feat[below])
  .nzsc_subgroup("below_layer_subgroup_nzsc", passed, "Ultic subgroups",
                 list(layer = layer, feature = feature, base = base), miss, below[feat[below] %in% TRUE])
}

# ---- O: Organic Soils ------------------------------------------------------------------

# OLBZ: the underlying mineral soil, with its upper boundary less than 90 cm
# from the soil surface, has a podzolic-B or placic horizon.
buried_podzol_litter_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon)
  miss <- c("designation", "naf_test")
  if (is.na(x$ms)) return(.nzsc_subgroup("buried_podzol_litter_nzsc", NA, "O, OLBZ", missing = miss))
  if (x$ms >= x$soil_top + 90) return(.nzsc_subgroup("buried_podzol_litter_nzsc", FALSE, "O, OLBZ"))
  # the underlying mineral soil is buried by the litter: read it whether or
  # not its horizons carry the buried prefix (bE, bBh; FAO Eb, Bhb)
  xu <- x; xu$des$buried <- rep(FALSE, x$n)
  pz <- .nzsc_podzolic_b_rows(xu); pl <- .nzsc_pan_rows(x, "placic")
  ok <- ifelse(pl, TRUE, pz)
  rows <- .nzsc_rows(x); rows <- rows[x$mineral[rows] & x$h$top_cm[rows] >= x$ms - 0.5]
  # nothing described under the litter (and not on rock): undecided
  v <- if (!length(rows)) { if (.nzsc_ends_hard(x)) FALSE else NA } else .nzsc_any(ok[rows])
  .nzsc_subgroup("buried_podzol_litter_nzsc", v, "O, OLBZ", missing = miss,
                 layers = rows[ok[rows] %in% TRUE])
}

# The organic soil material to 60 cm from the soil surface (or to its base,
# if shallower): its rows, the window and the thickness of each row inside
# it (mineral interlayers are left out).
.nzsc_organic_window <- function(x) {
  h <- x$h
  om <- x$organic %in% TRUE & !(x$role %in% "L" & x$des$decomp %in% "fibric")
  org <- x$ord[om[x$ord]]
  if (!length(org) || is.na(x$soil_top)) return(NULL)
  zt <- h$top_cm[org[1L]]
  base <- zt; for (i in org) { if (is.na(h$bottom_cm[i])) { base <- Inf; break }; base <- max(base, h$bottom_cm[i]) }
  zb <- min(x$soil_top + 60, base)
  th <- .asc_overlap(h$top_cm[org], ifelse(is.na(h$bottom_cm[org]), zb, h$bottom_cm[org]), zt, zb)
  list(rows = org, zt = zt, zb = zb, om = om, th = th)
}

# OFS: the organic fibres to 60 cm from the soil surface (or to the base of
# the organic soil material) are more than 70 % Sphagnum: the thickness-
# weighted share of the organic rows; a share not recorded reads as none
# (recorded when met, Hugo's decision, 2026-10-28).
sphagnic_organic_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h
  w <- .nzsc_organic_window(x)
  if (is.null(w)) return(.nzsc_subgroup("sphagnic_organic_nzsc", NA, "O, OFS", missing = "designation"))
  sp <- h$sphagnum_fibres_pct %||% rep(NA_real_, x$n)
  rows <- w$rows; th <- w$th
  passed <- if (sum(th) <= 0) NA else sum(th * ifelse(is.na(sp[rows]), 0, sp[rows])) / sum(th) > 70
  .nzsc_subgroup("sphagnic_organic_nzsc", passed, "O, OFS", list(window = c(w$zt, w$zb)))
}

# OFA, OMA, OHA: the organic soil material to 60 cm (or to its base) has pH
# 4.5 or less throughout the major part.
acid_organic_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h
  w <- .nzsc_organic_window(x)
  if (is.null(w)) return(.nzsc_subgroup("acid_organic_nzsc", NA, "O, acid subgroups", missing = c("designation", "ph_h2o")))
  # the major part of the organic material inside the window
  ok <- ifelse(is.na(h$ph_h2o[w$rows]), NA, h$ph_h2o[w$rows] <= 4.5)
  tot <- sum(w$th); yes <- sum(w$th[ok %in% TRUE]); no <- sum(w$th[ok %in% FALSE])
  passed <- if (tot <= 0) NA else if (yes > tot / 2) TRUE else if (no >= tot / 2) FALSE else NA
  .nzsc_subgroup("acid_organic_nzsc", passed, "O, acid subgroups", missing = "ph_h2o")
}

# ---- G: Gley Soils --------------------------------------------------------------------

# GAG: the major part of the B horizon to 60 cm from the mineral soil
# surface (or to its base, when shallower) is a clayey, moderately or strongly pedal cutanic or argillic
# horizon with polyhedral peds of 20 mm or less.
granular_acid_gley_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; n <- x$n
  miss <- c("designation", "structure_grade", "structure_type", "ped_size_max_mm", "clay_pct")
  pre <- .nzsc_sub_ms_or_na(x, "granular_acid_gley_nzsc", "G, GAG", miss); if (!is.null(pre)) return(pre)
  b <- .nzsc_b_rows(x); b <- b[h$top_cm[b] < x$ms + 60]
  if (!length(b)) return(.nzsc_subgroup("granular_acid_gley_nzsc", .nzsc_none(x), "G, GAG", missing = miss))
  clay <- ifelse(is.na(x$tex$group), NA, x$tex$group %in% "clayey")
  ok <- .and3v(clay, .nzsc_polyhedral_cutanic(x))
  base <- if (anyNA(h$bottom_cm[b])) Inf else max(h$bottom_cm[b])
  .nzsc_subgroup("granular_acid_gley_nzsc", .nzsc_major_part_b(x, b, ok, zb = min(x$ms + 60, base)), "G, GAG",
                 missing = miss)
}

# Cutanic horizon of each row: the (h) notation, or recorded dark coatings
# (moist value 4 or less) on an E, B or BC horizon, as the Diagnostics
# define it (only "usually" written (h)).
.nzsc_cutanic_or_coated <- function(x) {
  h <- x$h; n <- x$n
  films <- .nzsc_txt(h$clay_films_amount, n)
  coated <- !is.na(films) & nzchar(films) & !grepl("^none|^no|absent", films)
  cv <- h$coatings_value_moist %||% rep(NA_real_, n)
  ebc <- .nzsc_is(x, c("E", "B", "BC"))
  cut <- .nzsc_cutanic_rows(x)
  # value 5 passes with chroma 3 or less, and the coatings chroma is not
  # recorded (v0.9.300); coatings recorded with no colour value may be dark
  # (v0.9.301)
  ifelse(coated & !is.na(cv) & cv <= 4 & ebc, TRUE,
         ifelse(coated & ebc & !(cut %in% TRUE) & (is.na(cv) | (cv > 4 & cv <= 5)), NA, cut))
}

# A moderately or strongly pedal cutanic or argillic horizon with polyhedral
# peds of 20 mm or less (GAG; LOBG, v0.9.300).
.nzsc_polyhedral_cutanic <- function(x) {
  h <- x$h; n <- x$n
  g <- tolower(as.character(h$structure_grade))
  size <- h$ped_size_max_mm %||% rep(NA_real_, n)
  grade <- ifelse(grepl("moderate|strong", g), TRUE, ifelse(is.na(h$structure_grade), NA, FALSE))
  poly <- .nzsc_by_shape(h, function(t) ifelse(grepl("polyhed", t), TRUE, ifelse(is.na(t), NA, FALSE)))
  ca <- .any3v(.nzsc_cutanic_or_coated(x), .nzsc_argillic_rows(x))
  .and3v(.and3v(grade, ca), .and3v(poly, ifelse(is.na(size), NA, size <= 20)))
}

# GAY: in the major part of the B horizon, 10 % or more clay or humus
# coatings on ped faces and less silt than clay.
ultic_acid_gley_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon)
  miss <- c("clay_films_amount", "silt_pct", "clay_pct")
  b <- .nzsc_b_rows(x)
  if (!length(b)) return(.nzsc_subgroup("ultic_acid_gley_nzsc", .nzsc_none(x), "G, GAY", missing = miss))
  ok <- .and3v(.nzsc_coatings_10(x), .nzsc_less_silt_than_clay(x))
  .nzsc_subgroup("ultic_acid_gley_nzsc", .nzsc_major_part_b(x, b, ok), "G, GAY", missing = miss)
}

# GAPH, GAH: a B horizon with 10 % or more dark-coloured coats (moist value
# 4 or less, or value 5 and chroma 3); `placic`: and a placic horizon.
humose_acid_gley_nzsc <- function(pedon, placic = FALSE) {
  x <- .nzsc_horizons(pedon)
  miss <- c("clay_films_amount", "coatings_value_moist", "designation")
  b <- .nzsc_b_rows(x)
  coats <- if (!length(b)) .nzsc_none(x) else .nzsc_any(.and3v(.nzsc_coatings_10(x), .nzsc_dark_coatings(x))[b])
  pl <- if (isTRUE(placic)) any(.nzsc_pan_rows(x, "placic")) else TRUE
  .nzsc_subgroup("humose_acid_gley_nzsc", .and3(c(coats, pl)), "G, GAPH/GAH",
                 list(dark_coats = coats, placic = if (isTRUE(placic)) pl else NULL), miss)
}

# GOE: a B horizon whose major part to 60 cm from the mineral soil surface
# (or to its base, whichever is less) is moderately or strongly pedal,
# moderately or very sticky, with pH 5.9 or more.
melanic_orthic_gley_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; n <- x$n
  miss <- c("structure_grade", "stickiness", "ph_h2o")
  pre <- .nzsc_sub_ms_or_na(x, "melanic_orthic_gley_nzsc", "G, GOE", miss); if (!is.null(pre)) return(pre)
  b <- .nzsc_b_rows(x); b <- b[h$top_cm[b] < x$ms + 60]
  if (!length(b)) return(.nzsc_subgroup("melanic_orthic_gley_nzsc", .nzsc_none(x), "G, GOE", missing = miss))
  g <- tolower(as.character(h$structure_grade))
  grade <- ifelse(grepl("moderate|strong", g), TRUE, ifelse(is.na(h$structure_grade), NA, FALSE))
  stk <- .nzsc_class_index(h$stickiness %||% rep(NA, n), "stickiness")
  ok <- .and3v(.and3v(grade, ifelse(is.na(stk), NA, stk >= 2)), ifelse(is.na(h$ph_h2o), NA, h$ph_h2o >= 5.9))
  base <- if (anyNA(h$bottom_cm[b])) Inf else max(h$bottom_cm[b])
  .nzsc_subgroup("melanic_orthic_gley_nzsc", .nzsc_major_part_b(x, b, ok, zb = min(x$ms + 60, base)),
                 "G, GOE", missing = miss)
}

# ---- U: Ultic Soils --------------------------------------------------------------------

# UPT: a reductimorphic horizon with its upper boundary within 15 cm of the
# base of the A horizon, or within 30 cm of the mineral soil surface.
perch_gley_ultic_typic_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h
  miss <- c("designation", "munsell_value_moist", "munsell_chroma_moist")
  pre <- .nzsc_sub_ms_or_na(x, "perch_gley_ultic_typic_nzsc", "U, UPT", miss); if (!is.null(pre)) return(pre)
  red <- .nzsc_reductimorphic_rows(x)
  rows <- .nzsc_rows(x)
  # read at every base the A may have (v0.9.304)
  v <- .nzsc_at_a_bases(x, function(ab) {
    lim <- max(c(x$ms + 30, ab + 15), na.rm = TRUE)
    v <- .nzsc_any(red[rows[h$top_cm[rows] >= x$ms - 0.5 & h$top_cm[rows] < lim]])
    if (is.na(ab) && !isTRUE(v)) NA else v
  })
  .nzsc_subgroup("perch_gley_ultic_typic_nzsc", v, "U, UPT", missing = miss)
}

# USE: an Ea or Er horizon.
albic_sandy_ultic_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon)
  ea <- .nzsc_is(x, "E") & vapply(x$des$strong, function(s) any(c("a", "r") %in% s), NA)
  ok <- ifelse(ea, TRUE, ifelse(x$unknown_role & x$mineral, NA, FALSE))
  .nzsc_subgroup("albic_sandy_ultic_nzsc", .nzsc_any(ok), "U, USE", missing = "designation", layers = which(ea))
}

# UYG: in some part of the B horizon to 60 cm from the mineral soil surface,
# 5 % or more coarse rock fragments mainly of ultramafic rocks (fragments
# not recorded read as none), or exchangeable Ca:Mg 0.2 or less with Mg 1.5
# cmol/kg or more.
magnesic_yellow_ultic_nzsc <- function(pedon) magnesic_subgroup_nzsc(pedon)

# Magnesic subgroups: UYG in the B horizon; EMG (`bc`) in the B or BC
# horizon (v0.9.300), to 60 cm from the mineral soil surface.
magnesic_subgroup_nzsc <- function(pedon, bc = FALSE) {
  x <- .nzsc_horizons(pedon); h <- x$h
  nm <- if (isTRUE(bc)) "magnesic_subgroup_nzsc" else "magnesic_yellow_ultic_nzsc"
  where <- if (isTRUE(bc)) "E, EMG" else "U, UYG"
  miss <- c("ca_cmol", "mg_cmol", "rock_origin", "coarse_fragments_pct")
  pre <- .nzsc_sub_ms_or_na(x, nm, where, miss); if (!is.null(pre)) return(pre)
  pm <- tolower(as.character(.nzsc_site1(pedon, "parent_material")))
  ok <- rep(FALSE, x$n)
  for (i in which(x$mineral)) {
    cf <- h$coarse_fragments_pct[i]
    ro <- tolower(as.character(h$rock_origin[i] %||% NA)); if (is.na(ro) || !nzchar(ro)) ro <- pm
    um <- if (is.na(ro) || !nzchar(ro)) NA else grepl("ultramafic|serpentin|peridot|dunite|pyroxenit|harzburg|lherzol", ro)
    frag <- if (is.na(cf) || cf < 5) FALSE else um
    ok[i] <- .any3(c(frag, .nzsc_ca_mg(x, i)))
  }
  # v0.9.300: mineral rows of unknown role may be B (or BC) too; v0.9.301:
  # each choice of them is tried
  v1 <- .nzsc_b_bc_some_part(x, ok, x$ms + 60, roles = if (isTRUE(bc)) c("B", "BC") else "B")
  .nzsc_subgroup(nm, v1, where, missing = miss)
}

# UYZ (UYMZ also a mottled profile form): pH 4.8 or less in the eluvial
# horizon (if present) or the uppermost B subhorizon, and 10 % or more humus
# coatings or coatings of colour value 4 or less on peds in some part of the
# B horizon.
podzolic_yellow_ultic_nzsc <- function(pedon, mottled = FALSE) {
  x <- .nzsc_horizons(pedon); h <- x$h
  miss <- c("ph_h2o", "clay_films_amount", "coatings_value_moist")
  b <- .nzsc_b_rows(x); b <- b[order(h$top_cm[b])]
  if (!length(b)) return(.nzsc_subgroup("podzolic_yellow_ultic_nzsc", .nzsc_none(x), "U, UYZ/UYMZ", missing = miss))
  acid <- ifelse(is.na(h$ph_h2o), NA, h$ph_h2o <= 4.8)
  e <- which(.nzsc_is(x, "E") & !is.na(h$top_cm))
  p1 <- if (length(e)) .nzsc_some_part(x, e, acid) else acid[b[1L]]
  p2 <- .nzsc_some_part(x, b, .and3v(.nzsc_coatings_10(x), .nzsc_dark_coatings(x, value5 = FALSE)))
  m <- if (isTRUE(mottled)) .nzsc_mottled_profile_form(x) else TRUE
  .nzsc_subgroup("podzolic_yellow_ultic_nzsc", .and3(c(p1, p2, m)), "U, UYZ/UYMZ",
                 list(acid = p1, coatings = p2, mottled = if (isTRUE(mottled)) m else NULL), miss)
}

# UYBG: a silt loam or silty clay texture dominant from the soil surface to
# 35 cm or more, over a texture contrast to a horizon of clay texture class
# with moist value 4 or less (the clay horizon directly below the silty
# one).
buried_granular_ultic_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h
  miss <- c("field_texture", "clay_pct", "silt_pct", "munsell_value_moist")
  s0 <- x$soil_top
  if (is.na(s0)) return(.nzsc_subgroup("buried_granular_ultic_nzsc", NA, "U, UYBG", missing = miss))
  cls <- .nzsc_texture_class(x)
  silty <- ifelse(is.na(cls), NA, cls %in% c("silt loam", "silty clay"))
  v <- h$munsell_value_moist
  clay_dark <- .and3v(ifelse(is.na(cls), NA, cls %in% "clay"), ifelse(is.na(v), NA, v <= 4))
  rows <- .nzsc_rows(x)
  cand <- rows[h$top_cm[rows] >= s0 + 35 - 0.5 & !(clay_dark[rows] %in% FALSE)]
  res <- vapply(cand, function(j) {
    zj <- h$top_cm[j]
    up <- rows[!is.na(h$bottom_cm[rows]) & abs(h$bottom_cm[rows] - zj) <= 0.5]
    if (!length(up)) return(NA)              # a gap above the clay
    above <- rows[h$top_cm[rows] < zj - 0.5]
    hh <- list(h = list(top_cm = h$top_cm, bottom_cm = ifelse(is.na(h$bottom_cm), zj, h$bottom_cm)))
    .and3(c(clay_dark[j], silty[up[1L]], .asc_major_part(hh, above, silty, s0, zj)))
  }, NA)
  .nzsc_subgroup("buried_granular_ultic_nzsc", .any3(res), "U, UYBG", missing = miss)
}
