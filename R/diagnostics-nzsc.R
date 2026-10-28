# =============================================================================
# v0.9.293 -- diagnostics of the New Zealand Soil Classification, 4th edition
# (Manaaki Whenua 2026; Quarto source of 7 October 2026), for the key to soil
# orders and the definitions it reads (chapter "Diagnostic horizons and other
# differentiae"). Three-valued: TRUE, FALSE or NA (undecided).
#
# The NZSC reads horizon notations and field tests that international data
# rarely carry: P retention, the reactive-aluminium (NaF) test, Childs' test,
# sensitivity, fluidity, unconfined strength, failure, stickiness, packing.
# Each diagnostic reads the measured property when recorded and the
# surveyor's notation otherwise (a Bw is a weathered-B horizon, a g or r a
# reductimorphic horizon, a Bh or Bs a podzolic-B horizon, a Bo an oxidic
# horizon). Depths are from the mineral soil surface (the top of the first
# mineral horizon, below any L or O horizons) unless the key says the soil
# surface. Choices where the text is silent are in
# inst/rules/nzsc4/readings.yaml. Names carry the _nzsc suffix.
# =============================================================================

.NZSC_REF <- "Manaaki Whenua (2026). New Zealand Soil Classification, 4th ed."

.nzsc_result <- function(name, passed, layers = integer(0), evidence = list(),
                         missing = character(0), where = "", notes = NA_character_) {
  DiagnosticResult$new(name = name, passed = passed, layers = as.integer(layers),
                       evidence = evidence, missing = unique(missing),
                       reference = sprintf("%s %s", .NZSC_REF, where), notes = notes)
}

# ---- field classes ------------------------------------------------------------

# Ordered classes of the field tests, from text (New Zealand vocabulary; the
# Australian strength classes strong, very strong and rigid read as hard and
# stronger). NA when the value is not one of them.
.nzsc_class_index <- function(v, kind) {
  v <- tolower(trimws(gsub("[_-]", " ", as.character(v))))
  tab <- switch(kind,
    strength = c("loose" = 0, "very weak" = 1, "weak" = 2, "slightly firm" = 3, "firm" = 4,
                 "very firm" = 5, "hard" = 6, "strong" = 6, "very hard" = 7, "very strong" = 7,
                 "rigid" = 8, "extremely hard" = 8),
    stickiness = c("non sticky" = 0, "nonsticky" = 0, "not sticky" = 0, "slightly sticky" = 1,
                   "moderately sticky" = 2, "sticky" = 2, "very sticky" = 3),
    naf = c("n" = 0, "none" = 0, "no reaction" = 0, "nil" = 0, "l" = 1, "slight" = 1,
            "slight reaction" = 1, "weak" = 1, "m" = 2, "moderate" = 2, "moderate reaction" = 2,
            "h" = 3, "strong" = 3, "strong reaction" = 3),
    fluidity = c("non fluid" = 0, "nonfluid" = 0, "not fluid" = 0, "slightly fluid" = 1,
                 "moderately fluid" = 2, "very fluid" = 3),
    sensitivity = c("insensitive" = 0, "not sensitive" = 0, "slightly sensitive" = 1,
                    "moderately sensitive" = 2, "sensitive" = 3, "strongly sensitive" = 4,
                    "very sensitive" = 4))
  out <- unname(tab[v])
  out[is.na(v)] <- NA
  out
}

# Texture class of a horizon, as a USDA-style class name, from a New Zealand
# or Australian field texture, else from lab sand, silt and clay; and its
# texture group: sandy (sand, loamy sand), clayey (35 % clay or more), loamy
# (between). NA when neither is recorded.
.nzsc_texture <- function(h) {
  n <- nrow(h)
  ft <- tolower(trimws(as.character(h$field_texture %||% rep(NA_character_, n))))
  word <- rep(NA_character_, n)
  pat <- c("silty clay loam" = "silty clay loam", "sandy clay loam" = "sandy clay loam",
           "clay loam" = "clay loam", "silty clay" = "silty clay", "sandy clay" = "sandy clay",
           "loamy sand" = "loamy sand", "sandy loam" = "sandy loam", "silt loam" = "silt loam",
           "^(fine |medium |coarse )?sand$" = "sand", "^silt$" = "silt", "^loam$" = "loam",
           "^clay$" = "clay", "peat" = "peat")
  for (k in names(pat)) word[is.na(word) & grepl(k, ft)] <- pat[[k]]
  # Australian field texture codes, by their grade
  au <- .asc_texture(h$field_texture %||% rep(NA_character_, n))$grade
  au_map <- c(S = "sand", LS = "loamy sand", CS = "loamy sand", SL = "sandy loam", L = "loam",
              ZL = "silt loam", SCL = "sandy clay loam", CLS = "clay loam", CL = "clay loam",
              ZCL = "silty clay loam", LC = "clay", LMC = "clay", MC = "clay", MHC = "clay",
              HC = "clay", C = "clay", peat = "peat")
  word[is.na(word) & !is.na(au)] <- unname(au_map[au[is.na(word) & !is.na(au)]])
  clay <- h$clay_pct; silt <- h$silt_pct
  lab_sandy <- ifelse(is.na(silt) | is.na(clay), NA, silt + 2 * clay < 30)
  group <- ifelse(!is.na(clay) & clay >= 35, "clayey",
           ifelse(!is.na(word) & word %in% c("clay", "silty clay", "sandy clay"), "clayey",
           ifelse(!is.na(word) & word %in% c("sand", "loamy sand"), "sandy",
           ifelse(!is.na(word) & word != "peat", "loamy",
           ifelse(lab_sandy %in% TRUE, "sandy", ifelse(lab_sandy %in% FALSE & !is.na(clay), "loamy", NA_character_))))))
  list(class = word, group = group, fine_loamy_sand_or_finer =
         ifelse(!is.na(word), !(word %in% c("sand") | grepl("coarse|medium", ft) & word %in% "loamy sand"),
                ifelse(is.na(silt) | is.na(clay), NA, ifelse(silt + 2 * clay >= 30, TRUE,
                       ifelse(silt + 1.5 * clay < 15, FALSE, NA)))))
}

# ---- the profile as the NZSC reads it -----------------------------------------

.nzsc_horizons <- function(pedon) {
  h <- pedon$horizons
  n <- nrow(h)
  des <- .nzsc_designations(h$designation %||% rep(NA_character_, n), pedon$site)
  role <- des$role
  top <- h$top_cm; bot <- h$bottom_cm
  oc <- h$oc_pct
  fib <- h$fiber_content_unrubbed_pct %||% rep(NA_real_, n)
  # organic soil material: an L or O horizon; otherwise 18 % or more carbon,
  # or more than 20 % unrubbed fibre; a measured carbon below 18 % with no
  # fibre recorded is mineral
  organic <- ifelse(role %in% c("L", "O"), TRUE,
             ifelse((!is.na(oc) & oc >= 18) | (!is.na(fib) & fib > 20), TRUE,
             ifelse(des$known | !is.na(oc), FALSE, NA)))
  rock <- role %in% c("R")
  cem <- tolower(as.character(h$cementation_class %||% rep(NA_character_, n)))
  hard_rows <- rock | role %in% "CR" | cem %in% c("moderately", "strongly", "indurated")
  ord <- order(top, na.last = NA)
  # the soil surface (top of the first horizon) and the mineral soil surface
  # (the top of the first horizon that is not organic)
  soil_top <- if (length(ord)) top[ord[1L]] else NA_real_
  ms <- NA_real_
  for (i in ord) {
    if (isTRUE(organic[i])) next
    if (is.na(organic[i])) { ms <- NA_real_; break }
    ms <- top[i]; break
  }
  if (is.na(ms) && length(ord) && all(organic[ord] %in% TRUE)) {
    ms <- suppressWarnings(max(bot[ord], na.rm = TRUE))
    if (!is.finite(ms) || anyNA(bot[ord])) ms <- NA_real_
  }
  # a row with no designation, or one that cannot be read, may be any
  # horizon (rows below the last designated one included)
  trailing <- rep(FALSE, n)
  unknown_role <- !des$known
  tx <- .nzsc_texture(h)
  list(h = h, n = n, des = des, role = role, known = des$known, any_known = any(des$known),
       unknown_role = unknown_role, trailing = trailing, organic = organic,
       mineral = !(organic %in% TRUE) & !rock, rock = rock, hard = hard_rows,
       thick = bot - top, soil_top = soil_top, ms = ms, ord = ord, tex = tx)
}

# Rows by role (not buried): A (A and AB), E, B (B, BA, BC and B/C read as B
# where the key says "B horizon", with BC kept apart where it says "B or BC").
.nzsc_is <- function(x, roles) !x$des$buried & x$role %in% roles

# ---- colours ----------------------------------------------------------------------

# Low-chroma colours (moist): value 4 or more with chroma 2 or less, or value
# 6 or more with chroma 3 or less.
.nzsc_low_chroma <- function(h) {
  v <- h$munsell_value_moist; c <- h$munsell_chroma_moist
  ifelse(is.na(v) | is.na(c), NA, (v >= 4 & c <= 2) | (v >= 6 & c <= 3))
}

# Hue as a number on the Munsell circle (YR = 10, Y = 20 ...), larger is
# yellower; NA when not readable.
.nzsc_hue_num <- function(hue) {
  hue <- toupper(gsub("\\s+", "", as.character(hue)))
  num <- suppressWarnings(as.numeric(sub("^([0-9.]+).*$", "\\1", hue)))
  fam <- sub("^[0-9.]+", "", hue)
  base <- c(R = 0, YR = 10, Y = 20, GY = 30, G = 40, BG = 50, B = 60, PB = 70, P = 80, RP = 90)
  # a red-purple hue is redder than R: read below R on this scale
  out <- ifelse(is.na(num) | !fam %in% names(base), NA, unname(base[fam]) + num)
  ifelse(!is.na(out) & fam %in% "RP", out - 100, out)
}

# ---- diagnostic horizons and materials ---------------------------------------------

# Reductimorphic horizon of each row: a g or r suffix, or an O horizon
# (peat). The NZSC defines it by low-chroma colours in 50 % or more of the
# matrix, "consistent with long saturation", excluding those of eluvial
# horizons and of unweathered parent material, and identifies it by g or r;
# a designated horizon is read by its notation (Hugo's decision,
# 2026-10-28), as pale B horizons of Pallic Soils have low chroma without
# being reduced. Only a row with no designation is read by its moist matrix
# colour (organic material counting as peat).
.nzsc_reductimorphic_rows <- function(x) {
  h <- x$h; des <- x$des
  lc <- .nzsc_low_chroma(h)
  ifelse(des$g | des$r | x$role %in% "O", TRUE,
         ifelse(x$known, FALSE, ifelse(x$organic %in% TRUE, TRUE, lc)))
}

# The horizon directly below each row in the same material (same
# lithologic prefix), or NA.
.nzsc_below <- function(x) {
  h <- x$h; des <- x$des
  vapply(seq_len(x$n), function(i) {
    if (is.na(h$bottom_cm[i])) return(NA_integer_)
    j <- which(!is.na(h$top_cm) & abs(h$top_cm - h$bottom_cm[i]) <= 0.5 & des$prefix == des$prefix[i])
    if (length(j)) j[1L] else NA_integer_
  }, 0L)
}

# Pedal (weak or stronger grade, not massive or single grain) of each row.
.nzsc_pedal <- function(h) {
  g <- tolower(as.character(h$structure_grade)); t <- tolower(as.character(h$structure_type))
  ifelse(grepl("weak|moderate|strong", g) & !grepl("massive|single", t), TRUE,
         ifelse(grepl("structureless|apedal|none", g) | grepl("massive|single", t), FALSE, NA))
}

# Weathered-B horizon of each row: a B horizon (not a pan, not a transition)
# marked w, or with a pedal structure of pedogenic origin, or a redder hue or
# higher chroma than the horizon below it in the same material, or less
# carbonate than it. A mineral row with no designation that shows any of
# these may be one (it may also be an A); a designated horizon of another
# kind is not.
.nzsc_weathered_b_rows <- function(x) {
  h <- x$h; des <- x$des; n <- x$n
  b <- .nzsc_is(x, "B") & !des$m
  pedal <- .nzsc_pedal(h)
  hue <- .nzsc_hue_num(h$munsell_hue_moist); chr <- h$munsell_chroma_moist; ca <- h$caco3_pct
  below <- .nzsc_below(x)
  colour <- vapply(seq_len(n), function(i) {
    j <- below[i]; if (is.na(j)) return(NA)
    if (anyNA(c(hue[i], hue[j], chr[i], chr[j]))) return(NA)
    hue[i] < hue[j] || chr[i] > chr[j]
  }, NA)
  decalc <- vapply(seq_len(n), function(i) { j <- below[i]
    if (is.na(j) || is.na(ca[i]) || is.na(ca[j])) NA else ca[i] < ca[j] }, NA)
  prop <- .any3v3(pedal, colour, decalc)
  maybe <- x$unknown_role & x$mineral
  ifelse(b, ifelse(des$w, TRUE, prop), ifelse(maybe, ifelse(prop %in% FALSE, FALSE, NA), FALSE))
}

.any3v3 <- function(a, b, c) ifelse(a %in% TRUE | b %in% TRUE | c %in% TRUE, TRUE,
                                     ifelse(a %in% FALSE & b %in% FALSE & c %in% FALSE, FALSE, NA))

# Distinct topsoil: an A horizon, at the surface or buried, 5 cm or more
# thick (with any L or O horizon over it) whose moist value or chroma is less
# than that of the horizon below it.
.nzsc_distinct_topsoil_rows <- function(x) {
  h <- x$h; n <- x$n
  a <- x$role %in% c("A") & x$known
  v <- h$munsell_value_moist; c <- h$munsell_chroma_moist
  vapply(seq_len(n), function(i) {
    if (!a[i]) return(if (x$unknown_role[i]) NA else FALSE)
    if (is.na(h$bottom_cm[i])) return(NA)
    j <- which(!is.na(h$top_cm) & abs(h$top_cm - h$bottom_cm[i]) <= 0.5)
    if (!length(j)) return(NA)
    j <- j[1L]
    # thickness with the A horizons and the L or O horizons directly over it
    t0 <- h$top_cm[i]; seen <- i
    repeat {
      up <- which(!is.na(h$bottom_cm) & abs(h$bottom_cm - t0) <= 0.5 & (x$role %in% c("A", "L", "O")) &
                  !(seq_len(n) %in% seen))
      if (!length(up)) break
      if (anyNA(h$top_cm[up])) return(NA)
      up <- up[h$top_cm[up] < t0]
      if (!length(up)) break
      seen <- c(seen, up); t0 <- min(h$top_cm[up])
    }
    thick <- h$bottom_cm[i] - t0
    if (is.na(thick)) return(NA)
    if (thick < 5) return(FALSE)
    if (anyNA(c(v[i], v[j], c[i], c[j]))) return(NA)
    v[i] < v[j] || c[i] < c[j]
  }, NA)
}

# Allophanic soil material of each row: sensitive or strongly sensitive, very
# weak or weak moist strength, non or slightly sticky and a strong
# reactive-aluminium test, or a P retention of 85 % or more; and a dry bulk
# density of the fine earth below 0.9 Mg/m3. A measured P retention decides
# the first part (the field tests stand in for it only when it is not
# measured: Hugo's decision, 2026-10-28).
.nzsc_allophanic_rows <- function(x) {
  h <- x$h; n <- x$n
  sens <- .nzsc_class_index(h$sensitivity_class %||% rep(NA, n), "sensitivity")
  str <- .nzsc_class_index(h$strength_moist %||% rep(NA, n), "strength")
  stk <- .nzsc_class_index(h$stickiness %||% rep(NA, n), "stickiness")
  naf <- .nzsc_class_index(h$naf_test %||% rep(NA, n), "naf")
  pr <- h$phosphate_retention_pct %||% rep(NA_real_, n)
  bd <- h$bulk_density_g_cm3
  route_a <- .and3v(.and3v(ifelse(is.na(sens), NA, sens >= 3), ifelse(is.na(str), NA, str %in% c(1, 2))),
                    .and3v(ifelse(is.na(stk), NA, stk <= 1), ifelse(is.na(naf), NA, naf >= 3)))
  either <- ifelse(!is.na(pr), pr >= 85, route_a)
  dens <- ifelse(is.na(bd), NA, bd < 0.9)
  ifelse(x$mineral, .and3v(either, dens), FALSE)
}

# Vitric soil material of each row: more than 35 % rock fragments of which at
# least 60 % rhyolitic pumice (fragments of a layer or rock origin naming
# pumice), or more than 40 % sand of which more than 30 % volcanic glass
# (volcanic_glass_pct, read as a share of the sand grains).
.nzsc_vitric_rows <- function(x) {
  h <- x$h; n <- x$n
  cf <- h$coarse_fragments_pct; sand <- h$sand_pct; vg <- h$volcanic_glass_pct %||% rep(NA_real_, n)
  org <- tolower(paste(h$layer_origin %||% rep(NA, n), h$rock_origin %||% rep(NA, n)))
  pumice <- grepl("pumice", org)
  a <- ifelse(is.na(cf), NA, ifelse(cf > 35, ifelse(pumice, TRUE, NA), FALSE))
  b <- ifelse(is.na(sand) | is.na(vg), NA, sand > 40 & vg > 30)
  ifelse(x$mineral, .any3v(a, b), FALSE)
}

# Calcareous horizon of each row: a k suffix, carbonate in the fine earth
# (CaCO3, carbonate segregations, or a visible reaction to HCl). With no
# carbonate recorded, a pH in water below 7.0 is read as no carbonate (free
# carbonate keeps the pH higher), as for the Australian key.
.nzsc_calcareous_rows <- function(x) {
  h <- x$h
  eff <- tolower(as.character(h$effervescence_class %||% rep(NA_character_, x$n)))
  eff_yes <- grepl("slight|moderate|strong|very|violent", eff) & !grepl("^non|^none", eff)
  eff_no <- grepl("^non|^none|^nil", eff)
  ca <- h$caco3_pct; seg <- h$secondary_carbonates_pct
  no_record <- (is.na(eff) | !nzchar(eff)) & is.na(ca) & (is.na(seg) | seg == 0) & !x$des$k
  acid <- !is.na(h$ph_h2o) & h$ph_h2o < 7.0
  ifelse(x$des$k | eff_yes | (!is.na(ca) & ca > 0) | (!is.na(seg) & seg > 0), TRUE,
         ifelse(eff_no | (!is.na(ca) & ca == 0) | (no_record & acid), FALSE, NA))
}

# Sodic features of each row: an n suffix; or an ESP of 6 or more (or 0.7
# cmol/kg exchangeable Na or more) with prismatic or blocky peds (the other
# morphological routes, dark clay coatings or skeletans or an Ew above, are
# not recorded apart from these).
.nzsc_sodic_rows <- function(x) {
  h <- x$h; n <- x$n
  esp <- h$esp_pct %||% rep(NA_real_, n)
  esp_c <- ifelse(!is.na(h$na_cmol) & !is.na(h$cec_cmol) & h$cec_cmol > 0, 100 * h$na_cmol / h$cec_cmol, esp)
  sod <- ifelse(!is.na(esp_c) & esp_c >= 6, TRUE, ifelse(!is.na(h$na_cmol) & h$na_cmol >= 0.7, TRUE,
         ifelse(is.na(esp_c) & is.na(h$na_cmol), NA, FALSE)))
  t <- tolower(as.character(h$structure_type))
  shape <- ifelse(grepl("block|prism|column", t), TRUE, NA)
  ifelse(x$des$n, TRUE, .and3v(sod, shape))
}

# Fluid horizons: moderately or very fluid. A fluidity class not recorded is
# read as non-fluid (fluid materials are recorded when met).
.nzsc_fluid_rows <- function(x) {
  f <- .nzsc_class_index(x$h$fluidity_class %||% rep(NA, x$n), "fluidity")
  ifelse(is.na(f), FALSE, f >= 2)
}

# Podzolic-B horizon of each row (Diagnostics, three routes):
# 1. a Bh (not a humus-pan);
# 2. a Bs, or a B horizon 5 cm or more thick with a strong reactive-aluminium
#    test and one of: a reddish hue with the highest chroma at its top, earthy
#    structure or weak blocks or polyhedra, very weak or weak strength; under
#    an eluvial horizon (not necessarily directly);
# 3. a Bs with dark coatings on 50 % of ped faces (not recorded: a Bs with no
#    eluvial horizon above stays undecided).
# A designated B of another kind passes route 2 only when its properties
# show it; a row with no designation under an eluvial horizon may be one.
.nzsc_podzolic_b_rows <- function(x) {
  h <- x$h; des <- x$des; n <- x$n
  b <- .nzsc_is(x, "B") & !des$m
  e_above <- vapply(seq_len(n), function(i) any(x$role %in% "E" & !x$des$buried & !is.na(h$top_cm) &
                                                 !is.na(h$top_cm[i]) & h$top_cm < h$top_cm[i]), NA)
  naf <- .nzsc_class_index(h$naf_test %||% rep(NA, n), "naf")
  g <- tolower(as.character(h$structure_grade)); t <- tolower(as.character(h$structure_type))
  str_m <- .nzsc_class_index(h$strength_moist %||% rep(NA, n), "strength")
  str_d <- .nzsc_class_index(h$strength_dry %||% rep(NA, n), "strength")
  weakish <- grepl("earthy", t) | (grepl("weak", g) & !grepl("moderate|strong", g) & grepl("block|polyhed", t)) |
             (!is.na(str_m) & str_m %in% c(1, 2)) | (!is.na(str_d) & str_d %in% c(1, 2))
  thick5 <- !is.na(x$thick) & x$thick >= 5
  props <- ifelse(is.na(naf), NA, ifelse(naf >= 3, ifelse(weakish, thick5, NA), FALSE))
  ifelse(b & des$h, TRUE,
  ifelse(b & des$s, ifelse(e_above, TRUE, NA),
  ifelse(b & e_above, ifelse(props %in% TRUE, TRUE, FALSE),
  ifelse(x$unknown_role & x$mineral & e_above, ifelse(props %in% FALSE, FALSE, NA), FALSE))))
}

# Pans and contacts from the notation, the recorded pan or cementation:
# fragipan (x), brittle-B ((x)), duripan (qm), humus-pan (Bhm), ironstone-pan
# (Blm), ortstein-pan (Bsm), placic (Bfm); lithic contact (R), paralithic
# contact (CR).
.nzsc_pan_rows <- function(x, kind = c("fragipan", "brittle", "duripan", "humus_pan", "ironstone_pan",
                                       "ortstein", "placic")) {
  kind <- match.arg(kind)
  pt <- tolower(as.character(x$h$pan_type %||% rep(NA_character_, x$n)))
  des <- x$des
  switch(kind,
    fragipan = des$x | grepl("fragipan", pt),
    brittle = des$x_weak,
    duripan = des$duripan | grepl("duripan|silcrete", pt),
    humus_pan = des$humus_pan | grepl("organic pan|humus", pt),
    ironstone_pan = des$ironstone_pan | grepl("ironstone|ferricrete", pt),
    ortstein = des$ortstein | grepl("ortstein", pt),
    placic = des$placic | grepl("thin ironpan|placic", pt))
}

# Oxidic horizon: a Bo (the definition is a B horizon); cutanic horizon: an
# E, B or BC marked (h); a mineral row with no designation may be either
# (an oxidic one only without a reactive-aluminium reaction). Cutanoxidic: a
# cutanic, clayey B horizon with polyhedral peds of 2-6 mm, moderately sticky
# or stickier and very plastic (the friable-failure and dusty-face criteria
# are not recorded and are not read).
.nzsc_oxidic_rows <- function(x) {
  naf <- .nzsc_class_index(x$h$naf_test %||% rep(NA, x$n), "naf")
  ifelse(.nzsc_is(x, "B") & x$des$o, TRUE,
         ifelse(x$unknown_role & x$mineral & !(naf %in% c(1, 2, 3)), NA, FALSE))
}
.nzsc_cutanic_rows <- function(x) {
  ifelse(.nzsc_is(x, c("E", "B", "BC")) & x$des$h_weak, TRUE, ifelse(x$unknown_role & x$mineral, NA, FALSE))
}
.nzsc_cutanoxidic_rows <- function(x) {
  h <- x$h
  cut <- .nzsc_cutanic_rows(x)
  clay <- ifelse(is.na(x$tex$group), NA, x$tex$group %in% "clayey")
  t <- tolower(as.character(h$structure_type))
  poly <- ifelse(grepl("polyhed", t), TRUE, ifelse(is.na(h$structure_type), NA, FALSE))
  size <- h$ped_size_max_mm %||% rep(NA_real_, x$n)
  stk <- .nzsc_class_index(h$stickiness %||% rep(NA, x$n), "stickiness")
  pl <- tolower(as.character(h$plasticity %||% rep(NA, x$n)))
  very_plastic <- ifelse(is.na(pl) | !nzchar(pl), NA, grepl("^very", pl))
  ok <- .and3v(.and3v(ifelse(is.na(size), NA, size >= 2 & size <= 6), ifelse(is.na(stk), NA, stk >= 2)), very_plastic)
  b_or_maybe <- .nzsc_is(x, "B") | (x$unknown_role & x$mineral)
  ifelse(b_or_maybe & !(cut %in% FALSE), .and3v(.and3v(cut, clay), .and3v(poly, ok)), FALSE)
}

# Argillic horizon of each row (a B or BC, as its notation Bt, BCt): a t
# suffix; clay-enriched lamellae (a designation naming lamellae); or 5 % more
# clay than the horizon above it in the same material with clay coatings, or
# under an eluvial horizon at an abrupt or sharp boundary. A mineral row with
# no designation may be one unless its clay shows no increase.
.nzsc_argillic_rows <- function(x) {
  h <- x$h; n <- x$n; des <- x$des
  lam <- grepl("lamell", tolower(as.character(h$designation %||% rep(NA, n))))
  films <- tolower(as.character(h$clay_films_amount %||% rep(NA, n)))
  films_yes <- nzchar(films) & !is.na(films) & !grepl("^none|^no|^absent", films)
  maybe <- x$unknown_role & x$mineral
  incr <- vapply(seq_len(n), function(i) {
    if (!(x$role[i] %in% c("B", "BC") || maybe[i]) || is.na(h$top_cm[i])) return(FALSE)
    j <- which(!is.na(h$bottom_cm) & abs(h$bottom_cm - h$top_cm[i]) <= 0.5 & des$prefix == des$prefix[i])
    if (!length(j) || is.na(h$clay_pct[i]) || is.na(h$clay_pct[j[1L]])) return(NA)
    if (h$clay_pct[i] < h$clay_pct[j[1L]] + 5) return(FALSE)
    abrupt <- tolower(as.character(h$boundary_distinctness[j[1L]] %||% NA)) %in% c("abrupt", "sharp", "very abrupt")
    if (films_yes[i] || (x$role[j[1L]] %in% "E" && abrupt)) TRUE else NA
  }, NA)
  ifelse(.nzsc_is(x, c("B", "BC")), ifelse(des$t | lam, TRUE, incr %in% TRUE),
         ifelse(maybe, ifelse(incr %in% FALSE, FALSE, NA), FALSE))
}

# Eluvial horizon: an E (Ew, Eg, Er, Ed ...).
.nzsc_eluvial_rows <- function(x) ifelse(.nzsc_is(x, "E"), TRUE, ifelse(x$unknown_role, NA, FALSE))
