# =============================================================================
# v0.9.302 -- the keys to subgroups of the New Zealand Soil Classification,
# 4th ed.: the Brown, Anthropic, Recent and Raw Soils (109 subgroups of 20
# groups; the Hydrothermal Recent Soils and six groups of Raw Soils have
# none). As in R/diagnostics-nzsc-subgroups.R, each subgroup after the first
# is "other soils that ...", so a gate tests only its own criteria, and the
# last subgroup of each group is the default. Three-valued; readings in
# inst/rules/nzsc4/readings.yaml. Hugo's decisions of 2026-10-29: the Maori
# Anthropic subgroups read the existing fields (artefacts, layer origin);
# the purpose of mechanical mixing and acid-neutralising amendments are two
# new site fields, absent when not recorded; a coarse fill layer has 80 % or
# more coarse fragments and artefacts; the Humose Orthic Brown Soils have
# 10 % or less clay in every horizon to 90 cm; a compacted fill has one
# horizon of bulk density 1.5 or more.
# =============================================================================

# ---- helpers -----------------------------------------------------------------------

# "In some part" of the horizons of `roles` (pans aside) to `zb`, each choice
# of the rows of unknown role tried (.nzsc_b_bc_some_part).
.nzsc_roles_some_part <- function(x, ok, zb, roles = c("B", "BC")) .nzsc_b_bc_some_part(x, ok, zb, roles)

# Anthropic soil material of each row, as the order key reads it: the e
# prefix, a layer origin naming anthropic material (fill, spoil, dredgings,
# garbic, urbic, landfill, made ground, mine tailings), or artefacts.
.nzsc_anthropic_rows <- function(x) {
  h <- x$h; n <- x$n
  org <- tolower(paste(h$layer_origin %||% rep(NA, n)))
  art <- h$artefacts_pct %||% rep(NA_real_, n)
  x$des$anthropic | grepl("anthrop|\\bfill\\b|spoil|dredg|garbic|urbic|landfill|made ground|spolic|mine tailing", org) |
    (!is.na(art) & art > 0)
}

# Earthy structure, or, if weakly pedal or apedal, a dry bulk density of the
# fine earth below 1.1 (the Allophanic Brown subgroups).
.nzsc_earthy_or_light <- function(x) {
  h <- x$h
  g <- tolower(as.character(h$structure_grade)); t <- tolower(as.character(h$structure_type))
  earthy <- ifelse(grepl("earthy", t), TRUE, ifelse(is.na(h$structure_type), NA, FALSE))
  weak <- ifelse(grepl("moderate|strong", g), FALSE,
          ifelse(grepl("weak|structureless|apedal|none", g) | grepl("massive|single", t), TRUE, NA))
  bd <- h$bulk_density_g_cm3
  .any3v(earthy, .and3v(weak, ifelse(is.na(bd), NA, bd < 1.1)))
}

# Firm horizon of each row (the Firm Brown Soils), as the group reads it: a
# fragipan, or an apedal subhorizon of the B or BC of slightly firm or
# stronger moist strength (a mineral row of unknown role may be one).
.nzsc_firm_rows <- function(x) {
  h <- x$h; n <- x$n
  str <- .nzsc_class_index(h$strength_moist %||% rep(NA, n), "strength")
  apedal <- ifelse(.nzsc_pedal(h) %in% FALSE, TRUE, ifelse(.nzsc_pedal(h) %in% TRUE, FALSE, NA))
  f <- .and3v(apedal, ifelse(is.na(str), NA, str >= 3))
  bbc <- .nzsc_is(x, c("B", "BC"))
  firm <- ifelse(bbc, f, ifelse(x$unknown_role & x$mineral, ifelse(f %in% FALSE, FALSE, NA), FALSE))
  .any3v(.nzsc_pan_rows(x, "fragipan"), firm)
}

# Buried horizons (the b prefix or a trailing b), mineral.
.nzsc_buried_rows <- function(x) x$des$buried & x$mineral & !is.na(x$h$top_cm)

# Subsurface horizons within `depth` cm: below the uppermost mineral
# horizon, not litter or peat, not a surface A (as the Hydrothermal Recent
# Soils read them).
.nzsc_subsurface_rows <- function(x, depth = 60) {
  h <- x$h; rows <- .nzsc_rows(x)
  min_rows <- rows[x$mineral[rows]]
  first <- if (length(min_rows)) min_rows[1L] else NA_integer_
  rows[rows != first & h$top_cm[rows] < x$ms + depth - 0.5 & h$top_cm[rows] > x$ms + 0.5 &
       !(x$role[rows] %in% c("L", "O", "R", "CR")) & !(x$role[rows] %in% "A" & !x$des$buried[rows])]
}

# ---- B: Brown Soils -------------------------------------------------------------------

# BLA, BLT (`roles` B and BC), BLF part 1 (`roles: B`): in some part of the
# horizons to 60 cm from the mineral soil surface, an earthy structure, or,
# if weakly pedal or apedal, a bulk density below 1.1; `acid` (BLA): and pH
# below 5.5 in the same part.
earthy_b_subgroup_nzsc <- function(pedon, roles = c("B", "BC"), acid = FALSE) {
  x <- .nzsc_horizons(pedon); h <- x$h
  miss <- c("structure_grade", "structure_type", "bulk_density_g_cm3", if (isTRUE(acid)) "ph_h2o")
  pre <- .nzsc_sub_ms_or_na(x, "earthy_b_subgroup_nzsc", "B, BLA/BLF/BLT", miss); if (!is.null(pre)) return(pre)
  ok <- .nzsc_earthy_or_light(x)
  if (isTRUE(acid)) ok <- .and3v(ok, ifelse(is.na(h$ph_h2o), NA, h$ph_h2o < 5.5))
  .nzsc_subgroup("earthy_b_subgroup_nzsc", .nzsc_roles_some_part(x, ok, x$ms + 60, unlist(roles)), "B, BLA/BLF/BLT",
                 list(roles = roles, acid = isTRUE(acid)), miss)
}

# BLF part 2: under the earthy (or light) B, a fragipan (its pedality aside)
# or an apedal subhorizon of slightly firm or stronger moist strength, with
# its top within 90 cm of the mineral soil surface.
firm_beneath_subgroup_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h
  miss <- c("structure_grade", "strength_moist", "designation")
  pre <- .nzsc_sub_ms_or_na(x, "firm_beneath_subgroup_nzsc", "B, BLF", miss); if (!is.null(pre)) return(pre)
  el <- .nzsc_earthy_or_light(x)
  within <- !is.na(h$top_cm) & h$top_cm < x$ms + 60
  up <- ifelse(.nzsc_is(x, "B") & !x$des$m & within, el,
        ifelse(x$unknown_role & x$mineral & within, ifelse(el %in% FALSE, FALSE, NA), FALSE))
  firm <- ifelse(!is.na(h$top_cm) & h$top_cm < x$ms + 90 - 0.5, .nzsc_firm_rows(x), FALSE)
  .nzsc_subgroup("firm_beneath_subgroup_nzsc", .nzsc_beneath(x, up, firm), "B, BLF", missing = miss)
}

# BSP: an argillic horizon composed of lamellae (read as for the Laminar
# Argillic Semiarid Soils), or an eluvial horizon; skeletans are not
# recorded and read as absent.
pallic_sandy_brown_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon)
  lam <- lamellar_argillic_nzsc(pedon)$passed
  e <- .nzsc_any(.nzsc_eluvial_rows(x)[.nzsc_rows(x)])
  .nzsc_subgroup("pallic_sandy_brown_nzsc", .any3(c(lam, e)), "B, BSP", missing = "designation")
}

# BMMG, BMG: 5 % or more coarse rock fragments consisting mainly of
# ultramafic rocks (in any mineral horizon; not recorded read as none), or
# an exchangeable Ca:Mg of 0.2 or less in some part of the B or BC horizon to
# 60 cm (the text names no Mg minimum here).
magnesic_brown_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; n <- x$n
  miss <- c("ca_cmol", "mg_cmol", "rock_origin", "coarse_fragments_pct")
  pre <- .nzsc_sub_ms_or_na(x, "magnesic_brown_nzsc", "B, BMMG/BMG", miss); if (!is.null(pre)) return(pre)
  pm <- tolower(as.character(.nzsc_site1(pedon, "parent_material")))
  ro <- .nzsc_txt(h$rock_origin, n); ro <- ifelse(is.na(ro) | !nzchar(ro), pm, ro)
  um <- ifelse(is.na(ro) | !nzchar(ro), NA, grepl("ultramafic|serpentin|peridot|dunite|pyroxenit|harzburg|lherzol", ro))
  cf <- h$coarse_fragments_pct
  frag_rows <- ifelse(is.na(cf) | cf < 5 | !x$mineral, FALSE, um)
  frag <- .nzsc_any(frag_rows[.nzsc_rows(x)])
  ca <- h$ca_cmol; mg <- h$mg_cmol
  ratio <- ifelse(is.na(ca) | is.na(mg), NA, ifelse(mg > 0, ca / mg <= 0.2, FALSE))
  part <- .nzsc_roles_some_part(x, ratio, x$ms + 60)
  .nzsc_subgroup("magnesic_brown_nzsc", .any3(c(frag, part)), "B, BMMG/BMG", list(fragments = frag, ca_mg = part), miss)
}

# BAMP: a placic horizon or an ortstein-pan at 60 cm or less, and a mottled or
# gley profile form with perch-gley features.
impeded_acid_brown_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h
  miss <- c("designation", "redoximorphic_features_pct", "redox_segregations_location")
  pre <- .nzsc_sub_ms_or_na(x, "impeded_acid_brown_nzsc", "B, BAMP", miss); if (!is.null(pre)) return(pre)
  pan <- .nzsc_pan_rows(x, "placic") | .nzsc_pan_rows(x, "ortstein")
  rows <- .nzsc_rows(x)
  p <- any(pan[rows] & h$top_cm[rows] <= x$ms + 60 + 0.5)
  form <- .any3(c(.nzsc_mottled_profile_form(x), .nzsc_gley_profile_form(x)))
  .nzsc_subgroup("impeded_acid_brown_nzsc", .and3(c(p, form, .nzsc_perch_gley(x))), "B, BAMP", missing = miss)
}

# BFMP, BFP, BOMP, BOP: in some part of the B or BC horizon to `depth` cm, a
# matrix hue of 2.5Y or yellower, or of 10YR with chroma 4 or less with no
# more than a slight reactive-aluminium reaction, a cutanic horizon, or P
# retention below 30 %. Chroma to the nearest chip; a neutral hue reads as
# yellower, as the colour classes of the Pallic Soils read them.
pallic_colour_subgroup_nzsc <- function(pedon, depth = 90) {
  x <- .nzsc_horizons(pedon); h <- x$h; n <- x$n
  miss <- c("munsell_hue_moist", "munsell_chroma_moist", "naf_test", "phosphate_retention_pct")
  pre <- .nzsc_sub_ms_or_na(x, "pallic_colour_subgroup_nzsc", "B, pallic subgroups", miss); if (!is.null(pre)) return(pre)
  hue <- .nzsc_hue_num(h$munsell_hue_moist)
  neutral <- !is.na(h$munsell_hue_moist) & grepl("^N", toupper(trimws(as.character(h$munsell_hue_moist))))
  ch <- round(h$munsell_chroma_moist); ch[neutral & is.na(ch)] <- 0
  y25 <- ifelse(neutral, TRUE, ifelse(is.na(hue), NA, hue >= 22.5 - 0.01 & hue <= 30 + 0.01))
  h10 <- ifelse(neutral, FALSE, ifelse(is.na(hue), NA, abs(hue - 20) < 0.01))
  naf <- .nzsc_class_index(h$naf_test %||% rep(NA, n), "naf")
  pr <- h$phosphate_retention_pct %||% rep(NA_real_, n)
  extra <- .any3v(.any3v(ifelse(is.na(naf), NA, naf <= 1), .nzsc_cutanic_or_coated(x)), ifelse(is.na(pr), NA, pr < 30))
  ok <- .any3v(y25, .and3v(.and3v(h10, ifelse(is.na(ch), NA, ch <= 4)), extra))
  .nzsc_subgroup("pallic_colour_subgroup_nzsc", .nzsc_roles_some_part(x, ok, x$ms + as.numeric(depth)),
                 "B, pallic subgroups", missing = miss)
}

# BFA: in some part of the B or BC horizon above the firm apedal horizon, pH
# below 5.5, or a subhorizon with 10 % or more humus or humus-clay coatings
# of moist value 4 or less (value 5 with chroma 3 stays open, the coatings
# chroma not recorded). The top of the firm horizon is its first sure row or
# any undecided one above it: the verdict holds when all agree.
firm_acid_brown_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h
  miss <- c("ph_h2o", "clay_films_amount", "coatings_value_moist", "strength_moist")
  firm <- .nzsc_firm_rows(x)
  rows <- .nzsc_rows(x)
  ok <- .any3v(ifelse(is.na(h$ph_h2o), NA, h$ph_h2o < 5.5), .and3v(.nzsc_coatings_10(x), .nzsc_dark_coatings(x)))
  # the B or BC above the firm horizon, rows of unknown role tried as B
  at <- function(z) .nzsc_b_bc_some_part(x, ok, if (is.finite(z)) z else 1e6)
  sure <- rows[firm[rows] %in% TRUE]
  z1 <- if (length(sure)) min(h$top_cm[sure]) else Inf
  open <- rows[is.na(firm[rows]) & h$top_cm[rows] < z1 - 0.5]
  vs <- lapply(c(if (length(sure)) z1, h$top_cm[open]), at)
  if (!length(sure)) vs <- c(vs, list(FALSE))
  v <- if (all(vapply(vs, identical, NA, vs[[1L]]))) vs[[1L]] else NA
  .nzsc_subgroup("firm_acid_brown_nzsc", v, "B, BFA", missing = miss)
}

# BOH: in the greater part of the B horizon, a moist matrix value of 4 or
# less with hue 2.5Y or redder, or 10 % or more coatings of value 4 or less;
# and 10 % or less clay in every mineral horizon within 90 cm (Hugo's
# decision, 2026-10-29: lab clay, else the class range).
humose_brown_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h
  miss <- c("munsell_value_moist", "munsell_hue_moist", "clay_films_amount", "coatings_value_moist", "clay_pct")
  pre <- .nzsc_sub_ms_or_na(x, "humose_brown_nzsc", "B, BOH", miss); if (!is.null(pre)) return(pre)
  b <- .nzsc_b_rows(x)
  hue <- .nzsc_hue_num(h$munsell_hue_moist); v <- h$munsell_value_moist
  neutral <- !is.na(h$munsell_hue_moist) & grepl("^N", toupper(trimws(as.character(h$munsell_hue_moist))))
  red25 <- ifelse(neutral, FALSE, ifelse(is.na(hue), NA, hue <= 22.5 + 0.01))
  dark <- .and3v(ifelse(is.na(v), NA, v <= 4), red25)
  coat <- .and3v(.nzsc_coatings_10(x), .nzsc_dark_coatings(x, value5 = FALSE))
  c1 <- .nzsc_b_bc_major_part(x, .any3v(dark, coat), 1e6, roles = "B")
  cls <- .nzsc_texture_class(x); clay <- h$clay_pct
  lo_c <- c("sand" = 0, "loamy sand" = 0, "sandy loam" = 0, "loam" = 7, "silt loam" = 0, "silt" = 0, "sandy clay loam" = 20,
            "clay loam" = 27, "silty clay loam" = 27, "sandy clay" = 35, "silty clay" = 40, "clay" = 40)
  hi_c <- c("sand" = 10, "loamy sand" = 15, "sandy loam" = 20, "loam" = 27, "silt loam" = 27, "silt" = 12,
            "sandy clay loam" = 35, "clay loam" = 40, "silty clay loam" = 40, "sandy clay" = 55, "silty clay" = 60,
            "clay" = 100)
  low <- ifelse(!is.na(clay), clay <= 10, ifelse(is.na(cls), NA, ifelse(unname(hi_c[cls]) <= 10, TRUE,
         ifelse(unname(lo_c[cls]) > 10, FALSE, NA))))
  c2 <- .nzsc_all_in(x, ifelse(x$mineral & !(x$role %in% "CR"), low, TRUE), x$ms, x$ms + 90)
  .nzsc_subgroup("humose_brown_nzsc", .and3(c(c1, c2)), "B, BOH", list(dark_b = c1, low_clay = c2), miss)
}

# BOI: a buried A horizon within 120 cm of the mineral soil surface (by its
# notation; recorded when met), or a weathered-B horizon 30 cm or less thick
# that throughout has moist chroma below 6 and is apedal (massive or single
# grain) with weak or very weak moist strength.
immature_brown_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; n <- x$n
  miss <- c("designation", "munsell_chroma_moist", "structure_grade", "strength_moist")
  pre <- .nzsc_sub_ms_or_na(x, "immature_brown_nzsc", "B, BOI", miss); if (!is.null(pre)) return(pre)
  rows <- .nzsc_rows(x)
  bur <- any(x$des$buried[rows] & x$role[rows] %in% "A" & h$top_cm[rows] < x$ms + 120 - 0.5)
  bw <- .nzsc_weathered_b_rows(x)
  str <- .nzsc_class_index(h$strength_moist %||% rep(NA, n), "strength")
  apedal <- ifelse(.nzsc_pedal(h) %in% FALSE, TRUE, ifelse(.nzsc_pedal(h) %in% TRUE, FALSE, NA))
  ok_row <- .and3v(.and3v(ifelse(is.na(h$munsell_chroma_moist), NA, h$munsell_chroma_moist < 6), apedal),
                   ifelse(is.na(str), NA, str %in% c(1, 2)))
  # each choice of the rows of undecided weathered-B status is tried: a run
  # of weathered-B rows, 30 cm or less thick, all of them chroma below 6,
  # apedal and weak
  open_bw <- rows[is.na(bw[rows])]
  one <- function(fill) {
    b <- bw %in% TRUE; b[open_bw] <- fill
    starts <- rows[b[rows] & vapply(rows, function(i) {
      up <- rows[rows != i & !is.na(h$bottom_cm[rows]) & abs(h$bottom_cm[rows] - h$top_cm[i]) <= 0.5]
      !any(b[up])
    }, NA)]
    .nzsc_any(vapply(starts, function(i) {
      w <- .nzsc_walk_down(x, b, i)
      th <- sum(ifelse(is.na(x$thick[w$rows]), Inf, pmax(0, x$thick[w$rows])))
      .and3(c(th <= 30, ok_row[w$rows]))
    }, NA))
  }
  wb <- if (length(open_bw) <= 8L) .nzsc_over_fillings(length(open_bw), one) else NA
  .nzsc_subgroup("immature_brown_nzsc", .any3(c(bur, wb)), "B, BOI", list(buried_a = bur, thin_b = wb), miss)
}

# BOC: a calcareous horizon in part of the B or BC horizon within 60 cm of
# the mineral soil surface.
calcareous_b_subgroup_nzsc <- function(pedon, depth = 60) {
  x <- .nzsc_horizons(pedon); h <- x$h
  miss <- c("effervescence_class", "caco3_pct", "ph_h2o")
  pre <- .nzsc_sub_ms_or_na(x, "calcareous_b_subgroup_nzsc", "B, BOC", miss); if (!is.null(pre)) return(pre)
  calc <- .nzsc_calcareous_rows(x)
  b <- which(.nzsc_is(x, c("B", "BC")) & !x$des$m & !is.na(h$top_cm) & h$top_cm < x$ms + as.numeric(depth) - 0.5)
  maybe <- .nzsc_maybe_rows(x); maybe <- maybe[h$top_cm[maybe] < x$ms + as.numeric(depth) - 0.5]
  v <- .nzsc_any(c(calc[b], ifelse(calc[maybe] %in% FALSE, FALSE, NA)))
  .nzsc_subgroup("calcareous_b_subgroup_nzsc", v, "B, BOC", missing = miss, layers = b[calc[b] %in% TRUE])
}

# ---- A: Anthropic Soils -----------------------------------------------------------------

# AOA, AOO: a Maori cultural layer (maori_cultural_layer) holding artefacts
# or other manufactured material (artefacts_pct), or of a layer origin naming
# construction (pa, terracing, trenches, fortifications, urupa, burials,
# earthworks, excavated or redistributed material), for AOA; or of a layer
# origin naming plant- or animal-derived additions (shell, midden, charcoal,
# hangi or umu, ash other than volcanic, seaweed, kelp, bone, plant
# material, organic additions), for AOO (Hugo's decision, 2026-10-29: the
# existing fields).
maori_subgroup_nzsc <- function(pedon, kind = c("artefact", "organic")) {
  kind <- match.arg(kind)
  x <- .nzsc_horizons(pedon); h <- x$h; n <- x$n
  m <- .nzsc_yes(h$maori_cultural_layer %||% rep(NA, n)) %in% TRUE
  org <- .nzsc_txt(h$layer_origin, n); org[is.na(org)] <- ""
  ok <- if (kind == "artefact") {
    art <- h$artefacts_pct %||% rep(NA_real_, n)
    (!is.na(art) & art > 0) |
      grepl("\\bpa\\b|p\u0101|terracing|\\btrench|fortif|urup|burial|earthwork|construct|excavat|redistribut", org)
  } else grepl("\\bshells?\\b|midden|charcoal|\\bhangi\\b|\\bumu\\b|seaweed|\\bkelp\\b|\\bbones?\\b|\\bplants? (material|remains)", org) |
         (grepl("\\bash\\b", org) & !grepl("volcanic ash|tephra", org)) |
         (grepl("\\borganic\\b", org) & !grepl("\\binorganic\\b", org))
  ok <- ok & !(kind == "organic" & grepl("bone dry", org) & !grepl("midden|shell|charcoal", org))
  .nzsc_subgroup("maori_subgroup_nzsc", any(m & ok), "A, AOA/AOO", list(kind = kind),
                 c("maori_cultural_layer", "artefacts_pct", "layer_origin"), which(m & ok))
}

# ARB: the refuse (a layer origin naming refuse, as the group reads it) lies
# under an overburden of soil or rock more than 60 cm thick (its top more
# than 60 cm below the soil surface). Refuse known only by its odour leaves
# the depth open.
buried_refuse_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; n <- x$n
  orig <- .nzsc_txt(h$layer_origin, n)
  refuse <- grepl("refuse|garbage|garbic|landfill|rubbish|waste|plastic", orig) & !is.na(h$top_cm)
  if (!any(refuse)) {
    methane <- isTRUE(.nzsc_yes(.nzsc_site1(pedon, "methane_odour")))
    return(.nzsc_subgroup("buried_refuse_nzsc", if (methane) NA else FALSE, "A, ARB", missing = "layer_origin"))
  }
  .nzsc_subgroup("buried_refuse_nzsc", min(h$top_cm[refuse]) > x$soil_top + 60 + 0.5, "A, ARB",
                 layers = which(refuse))
}

# AMI, AMZ: the purpose of the mechanical mixing (site
# mechanical_mixing_purpose; Hugo's decision, 2026-10-29: recorded when met):
# tephra flipping (AMI), or breaking up drainage-impeding horizons (AMZ).
mixing_purpose_subgroup_nzsc <- function(pedon, purpose = c("tephra", "impeded")) {
  purpose <- match.arg(purpose)
  v <- tolower(as.character(.nzsc_site1(pedon, "mechanical_mixing_purpose")))
  ok <- !is.na(v) && if (purpose == "tephra") grepl("tephra", v) else grepl("imped|\\bpans?\\b|drainage", v)
  .nzsc_subgroup("mixing_purpose_subgroup_nzsc", ok, "A, AMI/AMZ", list(purpose = purpose))
}

# AFU: sulfidic or sulfuric soil material in the major part within 90 cm of
# the mineral soil surface, with acid-neutralising amendments (site
# acid_neutralising_amendment; Hugo's decision, 2026-10-29: recorded when
# met).
treated_sulfidic_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h
  miss <- c("sulfidic_s_pct", "ph_h2o", "sulfate_s_pct", "site$acid_neutralising_amendment")
  am <- isTRUE(.nzsc_yes(.nzsc_site1(pedon, "acid_neutralising_amendment")))
  if (!am) return(.nzsc_subgroup("treated_sulfidic_nzsc", FALSE, "A, AFU"))
  pre <- .nzsc_sub_ms_or_na(x, "treated_sulfidic_nzsc", "A, AFU", miss); if (!is.null(pre)) return(pre)
  zb <- x$ms + 90
  ok <- ifelse(x$role %in% c("R", "CR"), FALSE, .any3v(.nzsc_sulfidic_rows(x), .nzsc_sulfuric_rows(x)))
  rows <- .nzsc_rows_in(x, x$ms, zb)
  hh <- list(h = list(top_cm = h$top_cm, bottom_cm = ifelse(is.na(h$bottom_cm), zb, h$bottom_cm)))
  v <- if (!length(rows)) FALSE else .asc_major_part(hh, rows, ok, x$ms, zb)
  .nzsc_subgroup("treated_sulfidic_nzsc", v, "A, AFU", missing = miss)
}

# AFC: a fill horizon (anthropic soil material, as the order key reads it:
# the e prefix, a layer origin naming fill or other anthropic material, or
# artefacts) with a bulk density of 1.5 or more (Hugo's decision, 2026-10-29:
# one horizon).
compacted_fill_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; n <- x$n
  # the fill: anthropic soil material as the order key reads it
  fill <- .nzsc_anthropic_rows(x)
  bd <- h$bulk_density_g_cm3
  ok <- ifelse(fill, ifelse(is.na(bd), NA, bd >= 1.5), FALSE)
  .nzsc_subgroup("compacted_fill_nzsc", .nzsc_any(ok[.nzsc_rows(x)]), "A, AFC", missing = "bulk_density_g_cm3",
                 layers = which(ok %in% TRUE))
}

# AFW: wet within 60 cm of the mineral soil surface at some time of the year:
# a reductimorphic horizon, redox segregations, saturation recorded
# (water_saturation_days), or a positive Childs' test, in a horizon with its
# top less than 60 cm down; or a poorly or very poorly drained site. Signs
# of wetness not recorded read as absent.
wet_fill_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; n <- x$n
  pre <- .nzsc_sub_ms_or_na(x, "wet_fill_nzsc", "A, AFW", "designation"); if (!is.null(pre)) return(pre)
  rows <- .nzsc_rows(x); rows <- rows[x$mineral[rows] & h$top_cm[rows] < x$ms + 60 - 0.5]
  red <- .nzsc_reductimorphic_rows(x)
  wsd <- h$water_saturation_days %||% rep(NA_real_, n)
  ch <- .nzsc_txt(h$childs_test, n)
  sign <- red %in% TRUE | .nzsc_redox_present(x) %in% TRUE | (!is.na(wsd) & wsd > 0) | grepl("^pos|^yes|^true|^\\+", ch)
  dr <- tolower(as.character(.nzsc_site1(pedon, "drainage_class") %|NA|% .nzsc_site1(pedon, "drainage")))
  poor <- !is.na(dr) && grepl("very poor|^poor", dr)
  .nzsc_subgroup("wet_fill_nzsc", poor || any(sign[rows]), "A, AFW", layers = rows[sign[rows]])
}

# AFX: a coarse layer of added material (anthropic soil material) more than
# 30 cm thick with its top within 60 cm of the mineral soil surface, whose
# coarse fragments and artefacts together are 80
# % or more (Hugo's decision, 2026-10-29: too little fine earth to fill more
# than half the interstices); not recorded reads as none.
coarse_fill_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; n <- x$n
  pre <- .nzsc_sub_ms_or_na(x, "coarse_fill_nzsc", "A, AFX", "coarse_fragments_pct"); if (!is.null(pre)) return(pre)
  cf <- h$coarse_fragments_pct; art <- h$artefacts_pct %||% rep(NA_real_, n)
  tot <- ifelse(is.na(cf), 0, cf) + ifelse(is.na(art), 0, art)
  # the coarse layer is added material (anthropic soil material)
  ok <- x$mineral & tot >= 80 & .nzsc_anthropic_rows(x)
  .nzsc_subgroup("coarse_fill_nzsc", .nzsc_run_starting(x, ok, x$ms - 1000, x$ms + 59.4, 30, strict = TRUE), "A, AFX",
                 layers = which(ok))
}

# ---- R, W: Recent and Raw Soils -----------------------------------------------------------

# pH below 5.5 in some part (10 cm) of the mineral horizons between `from`
# and `to` cm from the mineral soil surface (RXOA, RXA: below 20 cm; RSA:
# between 20 and 60 cm); `subsurface` (RFAW, RFA): in some part of the
# subsurface horizons within 60 cm.
acid_window_subgroup_nzsc <- function(pedon, from = 20, to = 1000, subsurface = FALSE) {
  x <- .nzsc_horizons(pedon); h <- x$h
  pre <- .nzsc_sub_ms_or_na(x, "acid_window_subgroup_nzsc", "R, acidic subgroups", "ph_h2o"); if (!is.null(pre)) return(pre)
  acid <- ifelse(is.na(h$ph_h2o), NA, h$ph_h2o < 5.5)
  if (isTRUE(subsurface)) {
    zb <- x$ms + as.numeric(to)
    part <- function(rows) if (!length(rows)) FALSE else .nzsc_some_part_in(x, rows, acid, x$ms, zb)
    rows <- .nzsc_subsurface_rows(x, as.numeric(to))
    # rows of unknown role directly under the A may be A too: both bases
    ab <- .nzsc_a_bases(x)
    v1 <- part(rows)
    v2 <- if (is.na(ab[2L])) NA else part(rows[h$top_cm[rows] >= ab[2L] - 0.5 | !x$unknown_role[rows]])
    v <- if (identical(v1, v2)) v1 else NA
  } else {
    zt <- x$ms + as.numeric(from); zb <- x$ms + as.numeric(to)
    rows <- .nzsc_rows_in(x, zt, zb); rows <- rows[x$mineral[rows] & !(x$role[rows] %in% "CR")]
    v <- if (!length(rows)) FALSE else .nzsc_some_part_in(x, rows, acid, zt, zb)
  }
  .nzsc_subgroup("acid_window_subgroup_nzsc", v, "R, acidic subgroups", list(from = from, to = to), "ph_h2o")
}

# Sulfuric subgroups (RSR, RFR, WGR): sulfuric soil material in some part
# within 90 cm of the mineral soil surface (a window the description does
# not reach may hold it).
sulfuric_subgroup_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon)
  miss <- c("ph_h2o", "sulfate_s_pct", "jarosite_present", "sulfidic_s_pct")
  pre <- .nzsc_sub_ms_or_na(x, "sulfuric_subgroup_nzsc", "sulfuric subgroups", miss); if (!is.null(pre)) return(pre)
  s <- ifelse(x$role %in% c("R", "CR"), FALSE, .nzsc_sulfuric_rows(x)); zb <- x$ms + 90
  rows <- .nzsc_rows_in(x, x$ms, zb)
  .nzsc_subgroup("sulfuric_subgroup_nzsc", .nzsc_open_below(.nzsc_some_part_in(x, rows, s, x$ms, zb), x, zb),
                 "sulfuric subgroups", missing = miss, layers = which(s %in% TRUE))
}

# Tephric subgroups (RSK from the mineral soil surface, WSK from the soil
# surface): tephric soil material to 30 cm or more.
tephric_subgroup_nzsc <- function(pedon, from = c("mineral_surface", "soil_surface")) {
  from <- match.arg(from)
  x <- .nzsc_horizons(pedon)
  miss <- c("layer_origin", "rock_origin", "volcanic_glass_pct")
  z0 <- if (from == "soil_surface") x$soil_top else x$ms
  if (is.na(z0)) return(.nzsc_subgroup("tephric_subgroup_nzsc", NA, "tephric subgroups", missing = miss))
  t <- .nzsc_tephric_rows(pedon, x)
  .nzsc_subgroup("tephric_subgroup_nzsc", .nzsc_all_in(x, t, z0, z0 + 30), "tephric subgroups", list(from = from),
                 miss, which(t %in% TRUE))
}

# RFMW, RFAW, RFW, ROAW, ROW: a weathered-B horizon with its lower boundary
# at 30 cm or more from the mineral soil surface (an open bottom counts when
# its top is that deep).
weathered_b_deep_nzsc <- function(pedon, depth = 30) {
  x <- .nzsc_horizons(pedon); h <- x$h
  pre <- .nzsc_sub_ms_or_na(x, "weathered_b_deep_nzsc", "R, weathered subgroups", "designation"); if (!is.null(pre)) return(pre)
  bw <- .nzsc_weathered_b_rows(x)
  z <- x$ms + as.numeric(depth)
  deep <- ifelse(!is.na(h$bottom_cm), h$bottom_cm >= z - 0.5, ifelse(!is.na(h$top_cm) & h$top_cm >= z - 0.5, TRUE, NA))
  .nzsc_subgroup("weathered_b_deep_nzsc", .nzsc_any(.and3v(bw, deep)[.nzsc_rows(x)]), "R, weathered subgroups",
                 missing = c("designation", "structure_grade"), layers = which(.and3v(bw, deep) %in% TRUE))
}

# RTSP, RTBP, RTBL: a buried soil (buried horizons, the b prefix or a
# trailing b) with its upper surface at 60 cm or less from the mineral soil
# surface, with a buried B or BC horizon of vitric soil material (`material:
# vitric`) or of allophanic soil material apart from the bulk density
# (`allophanic`); `scoria` (RTSP): over it, predominantly gravel-sized
# basaltic scoria from the mineral soil surface to the buried soil.
buried_soil_subgroup_nzsc <- function(pedon, material = c("vitric", "allophanic"), scoria = FALSE) {
  material <- match.arg(material)
  x <- .nzsc_horizons(pedon); h <- x$h
  miss <- c("designation", "sand_pct", "volcanic_glass_pct", "phosphate_retention_pct")
  pre <- .nzsc_sub_ms_or_na(x, "buried_soil_subgroup_nzsc", "R, RTSP/RTBP/RTBL", miss); if (!is.null(pre)) return(pre)
  bur <- .nzsc_buried_rows(x)
  if (!any(bur)) return(.nzsc_subgroup("buried_soil_subgroup_nzsc", FALSE, "R, RTSP/RTBP/RTBL"))
  zb <- min(h$top_cm[bur])
  if (zb > x$ms + 60 + 0.5) return(.nzsc_subgroup("buried_soil_subgroup_nzsc", FALSE, "R, RTSP/RTBP/RTBL"))
  mat <- if (material == "vitric") .nzsc_vitric_rows(x) else .nzsc_allophanic_rows(x, density = FALSE)
  # the uppermost buried soil: its adjacent buried rows
  first <- which(bur & !is.na(h$top_cm) & h$top_cm == zb)[1L]
  soil <- seq_len(x$n) %in% .nzsc_walk_down(x, bur, first)$rows
  bb <- soil & x$role %in% c("B", "BC") & !x$des$m
  v <- .nzsc_any(mat[bb])
  if (isTRUE(scoria)) v <- .and3(c(v, .nzsc_scoria_window(pedon, x, x$ms, zb, "R, RTSP")$passed))
  .nzsc_subgroup("buried_soil_subgroup_nzsc", v, "R, RTSP/RTBP/RTBL", list(material = material, scoria = isTRUE(scoria)),
                 miss, which(bb & mat %in% TRUE))
}

# ROMP part 2: part 2(b) of the requirements for Pallic Soils.
pallic_2b_subgroup_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon)
  .nzsc_subgroup("pallic_2b_subgroup_nzsc", .nzsc_pallic_2b(x), "R, ROMP",
                 missing = c("designation", "structure_grade", "strength_moist"))
}

# WGS: sand or loamy sand with less than 35 % coarse rock fragments in all
# horizons from the soil surface to 60 cm or more (as the sandy groups read
# it).
sandy_subgroup_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon)
  z0 <- x$soil_top
  v <- if (is.na(z0)) NA else .nzsc_sandy_window(x, z0, z0 + 60)
  .nzsc_subgroup("sandy_subgroup_nzsc", v, "W, WGS", missing = c("field_texture", "clay_pct", "coarse_fragments_pct"))
}
