# =============================================================================
# v0.9.321 -- the ten great groups of the Soil Classification System of Japan
# (SCSJ 2017, chapter II, the key to great groups, pp. 2-5). Each great group
# after the first is "soils other than the above", so a gate tests only its
# own requirements: the key's order supplies the exclusions. Three-valued.
# =============================================================================

.scsj_any <- function(v) if (any(v %in% TRUE)) TRUE else if (all(v %in% FALSE)) FALSE else NA
.scsj_all <- function(v) if (all(v %in% TRUE)) TRUE else if (any(v %in% FALSE)) FALSE else NA
.scsj_or3 <- function(...) { v <- c(...); if (any(v %in% TRUE)) TRUE else if (all(v %in% FALSE)) FALSE else NA }
.scsj_and3 <- function(...) { v <- c(...); if (any(v %in% FALSE)) FALSE else if (all(v %in% TRUE)) TRUE else NA }

# A gate's result when the soil surface cannot be placed (IV.4): every depth
# is then unknown.
.scsj_no_surface <- function(name, x, where) {
  .scsj_result(name, NA, integer(0), list(soil_surface = x$surface_note),
               c("oc_pct", "designation", "drainage_class"), where)
}

# The fill (morido, kyakudo: fill and soil dressing) at the top of the soil:
# the run of rows, from the first one at or below the soil surface, whose
# recorded origin is fill. Its rows, certain thickness, bottom (from the soil
# surface) and whether its last row has no bottom.
.scsj_fill_run <- function(x) {
  rows <- integer(0); open <- FALSE
  if (!is.na(x$surface)) {
    for (i in x$ord[!is.na(x$dtop[x$ord]) & x$dtop[x$ord] >= -0.5]) {
      if (!x$orig$fill[i]) break
      if (length(rows)) {
        pb <- x$dbot[rows[length(rows)]]
        if (is.na(pb) || abs(x$dtop[i] - pb) > 0.5) break
      }
      rows <- c(rows, i)
      if (is.na(x$dbot[i])) { open <- TRUE; break }
    }
  }
  th <- if (length(rows)) sum(x$thick[rows], na.rm = TRUE) else 0
  list(rows = rows, thick = th, open = open,
       bottom = if (length(rows) && !open) x$dbot[rows[length(rows)]] else NA_real_)
}

# A material gate (Organic Soils, Andosols, Fluvic soils): 25 cm or more
# (cumulative) of the material within 50 cm of the soil surface; or, under a
# fill of other material less than 35 cm thick, 25 cm or more of it
# continuous from directly below the fill.
.scsj_material_gate <- function(x, rows) {
  c1 <- .scsj_at_least(.scsj_cum(x, rows, 0, 50), 25)
  fr <- .scsj_fill_run(x)
  c2 <- FALSE
  if (length(fr$rows) && fr$open) {
    c2 <- if (fr$thick < 35) NA else FALSE
  } else if (fr$thick > 0 && fr$thick < 35 && !is.na(fr$bottom)) {
    sure <- Filter(function(r) abs(r$top - fr$bottom) <= 0.5,
                   .scsj_runs(x, ifelse(rows %in% TRUE, TRUE, FALSE)))
    poss <- Filter(function(r) abs(r$top - fr$bottom) <= 0.5, .scsj_runs(x, rows, bridge = TRUE))
    e <- .scsj_ends(x)
    short <- !is.na(e$depth) && e$depth < fr$bottom + 25 && !e$on_rock
    c2 <- if (length(sure) && sure[[1L]]$sure >= 25) TRUE else
          if ((length(poss) && poss[[1L]]$poss >= 25) || short) NA else FALSE
  }
  list(passed = .scsj_or3(c1, c2), c1 = c1, c2 = c2, fill = fr)
}

# ---- A. Man-made soils -----------------------------------------------------------

# The great group of a part of a profile (the fill, or the soil under it),
# keyed through the gates B to J; NA when a gate before the assigned one is
# undecided.
.scsj_part_pedon <- function(pedon, rows, complete = TRUE) {
  if (!length(rows)) return(NULL)
  h <- as.data.frame(pedon$horizons)[rows, , drop = FALSE]
  if (all(is.na(h$top_cm))) return(NULL)
  shift <- min(h$top_cm, na.rm = TRUE)
  h$top_cm <- h$top_cm - shift; h$bottom_cm <- h$bottom_cm - shift
  # a part keyed on its own reads the fill's own material; it is complete
  # (nothing below its last horizon) only where the original ends there
  site <- pedon$site; site$scsj_part <- TRUE; site$scsj_part_complete <- isTRUE(complete)
  PedonRecord$new(site = site, horizons = data.table::as.data.table(h))
}

# The gates B to I of the key a part of a profile is keyed through, by the
# edition: the SCSJ 2017 or the CSCJ 2011 (Podzols before Andosols, Dark Red
# soils, its own Red-Yellow soils).
.scsj_part_gates <- function(p) {
  if (identical(.scsj_edition(p$site %||% list()), "cscj2011"))
    return(list(B = organic_soils_scsj, C = podzols_scsj, D = andosols_scsj, E = dark_red_soils_cscj,
                F = fluvic_soils_scsj, G = red_yellow_soils_cscj, H = stagnic_soils_scsj,
                I = brown_forest_soils_scsj))
  list(B = organic_soils_scsj, C = andosols_scsj, D = podzols_scsj, E = fluvic_soils_scsj,
       F = red_yellow_soils_scsj, G = stagnic_soils_scsj, H = eutrosols_scsj,
       I = brown_forest_soils_scsj)
}

# The great groups a part of a profile may key to: those of the gates up to
# the first that passes, the undecided ones included.
.scsj_part_possible <- function(p) {
  if (is.null(p)) return(c("B", "C", "D", "E", "F", "G", "H", "I", "J"))
  gates <- .scsj_part_gates(p)
  out <- character(0)
  for (k in names(gates)) {
    r <- gates[[k]](p)$passed
    if (isTRUE(r)) return(c(out, k))
    if (is.na(r)) out <- c(out, k)
  }
  c(out, "J")
}

# Do two parts certainly differ (TRUE), certainly agree (FALSE), or is it
# undecided? Their possible classes compared.
.scsj_differ <- function(a, b) {
  if (!length(intersect(a, b))) return(TRUE)
  if (length(a) == 1L && length(b) == 1L && identical(a, b)) return(FALSE)
  NA
}

.scsj_part_great_group <- function(p) {
  if (is.null(p)) return(NA_character_)
  gates <- .scsj_part_gates(p)
  for (k in names(gates)) {
    r <- gates[[k]](p)$passed
    if (isTRUE(r)) return(k)
    if (is.na(r)) return(NA_character_)
  }
  "J"
}

# Man-made soils (SCSJ 2017, great group A)
#
# The fill shows none of the paddy iron accumulation, paddy greyed,
# podzolic accumulation, pseudogley, argic or weathered horizons, nor the
# gley or wetness features; and within 50 cm 25 cm or more (cumulative) of
# layers with 20 % or more artificial material, a continuous impermeable
# artificial layer starting within 30 cm, or 35 cm or more of fill of soil
# material of another great group (in Regosols, of another group).
man_made_soils_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("man_made_soils_scsj", x, "II A, p. 2"))
  h <- x$h
  art <- .scsj_col(h, "artefacts_pct")
  art20 <- ifelse(is.na(art), FALSE, art >= 20)
  a <- .scsj_at_least(.scsj_cum(x, art20, 0, 50), 25)
  imp <- .scsj_impermeable_rows(x)
  b <- .scsj_impermeable_within(x, 30)
  fr <- .scsj_fill_run(x)
  c <- FALSE
  if (fr$thick >= 35 || (fr$open && length(fr$rows))) {
    if (fr$thick < 35) {
      c <- NA
    } else {
      below <- if (fr$open) integer(0) else
        x$ord[!is.na(x$dtop[x$ord]) & x$dtop[x$ord] >= fr$bottom - 0.5 & !(x$ord %in% fr$rows)]
      p_fill <- .scsj_part_pedon(pedon, fr$rows, complete = TRUE)
      p_below <- .scsj_part_pedon(pedon, below, complete = isTRUE(x$complete))
      # the classes each part may key to, a Regosol by its group (SCSJ IV.3:
      # in Regosols, material of another group is heterogeneous too; the CSCJ
      # has no such rule)
      expand <- function(g, p) if ("J" %in% g && identical(x$edition, "scsj2017"))
        c(setdiff(g, "J"), .scsj_regosol_possible(p)) else g
      g_fill <- expand(.scsj_part_possible(p_fill), p_fill)
      g_below <- expand(.scsj_part_possible(p_below), p_below)
      c <- .scsj_differ(g_fill, g_below)
    }
  }
  cond2 <- .scsj_or3(a, b, c)
  # (1) the man-made layers (the fill, or the layers of artificial material)
  # show none of the natural horizons and features
  # the layers that make criterion (2): the fill, the layers of artificial
  # material within 50 cm and the impermeable layer within 30 cm
  layers <- sort(unique(c(fr$rows, which(art20 & !is.na(x$dtop) & x$dtop < 50),
                          which(imp & !is.na(x$dtop) & x$dtop <= 30))))
  cond1 <- TRUE
  if (length(layers) && !isFALSE(cond2)) {
    f <- .scsj_horizon_flags(x)
    bad <- .scsj_or(.scsj_or(.scsj_or(f$paddy_fe, f$paddy_grey), .scsj_or(f$podzolic, f$pseudogley)),
                    .scsj_or(.scsj_or(f$argic, f$weathered), .scsj_or(f$gley, f$wet)))
    cond1 <- .scsj_not(.scsj_any(bad[layers]))
  }
  passed <- .scsj_and3(cond1, cond2)
  .scsj_result("man_made_soils_scsj", passed, layers,
               list(artefacts_20_cm = .scsj_cum(x, art20, 0, 50)$sure, impermeable_within_30 = b,
                    fill_cm = fr$thick, fill_other_great_group = c, natural_horizons_absent = cond1),
               if (is.na(passed)) c("artefacts_pct", "layer_origin") else character(0),
               "II A, p. 2")
}

# Row flags of the horizons and features (with their thickness), used by
# Man-made soils (1) and by the weathered horizon's criterion 5.
.scsj_horizon_flags <- function(x) {
  ag <- .scsj_argic_rows(x)
  agh <- .scsj_argic_horizon_rows(x, ag)
  pz <- .scsj_podzolic_rows(x, argic = ag)
  wet <- .scsj_wetness_rows(x)
  ps <- .scsj_paddy_surface_rows(x)
  gley_layer <- .scsj_horizon_rows(x, wet$gley, 10)
  paddy_any <- .scsj_any(ps$any)
  pseudo <- .scsj_horizon_rows(x, .scsj_and(wet$surface, rep(.scsj_not(paddy_any), x$n)), 10)
  pfe <- .scsj_paddy_fe_rows(x, ps)
  pgr <- .scsj_paddy_grey_rows(x, ps, wet)
  wr <- .scsj_weathered_rows(x)
  other <- .scsj_or(.scsj_or(agh, gley_layer), .scsj_or(pseudo, .scsj_or(pfe, pgr)))
  wr <- .scsj_horizon_rows(x, .scsj_and(wr, .scsj_not(other)), 15)
  list(argic = agh, podzolic = .scsj_or(.scsj_horizon_rows(x, pz$morph, 2.5),
                                        .scsj_horizon_rows(x, pz$analysis, 2.5)),
       gley = wet$gley, gley_layer = gley_layer, wet = wet$any, pseudogley = pseudo,
       paddy_fe = pfe, paddy_grey = pgr, weathered = wr)
}

# The rows directly under the paddy surface horizon (in depth order, while
# adjacent).
.scsj_under_paddy_surface <- function(x, ps) {
  surf <- which(ps$surface %in% c(TRUE, NA) & ps$any %in% c(TRUE, NA))
  if (!length(surf) || is.na(x$surface)) return(list(rows = integer(0), surf = surf))
  bs <- x$dbot[surf]
  if (anyNA(bs)) return(list(rows = integer(0), surf = surf))
  base <- max(bs)
  rows <- integer(0)
  for (i in x$ord) {
    if (i %in% surf || is.na(x$dtop[i]) || x$dtop[i] < base - 0.5) next
    if (x$dtop[i] - base > 0.5) break
    rows <- c(rows, i)
    if (is.na(x$dbot[i])) break
    base <- x$dbot[i]
  }
  list(rows = rows, surf = surf)
}

# The paddy iron accumulation horizon rows (IV.1): directly under a paddy
# surface horizon, 2 cm or more, Fe(d) twice that of the plough layer or
# more (1.6 times for 5 cm or more, 1.4 times for 10 cm or more, as the
# note allows); an ir or mn of the notation under a paddy surface when Fe(d)
# is not measured.
.scsj_paddy_fe_rows <- function(x, ps = .scsj_paddy_surface_rows(x)) {
  out <- rep(FALSE, x$n)
  u <- .scsj_under_paddy_surface(x, ps)
  if (!length(u$surf)) return(out)
  fed <- .scsj_col(x$h, "fe_dcb_pct")
  ap_fed <- fed[u$surf]
  ap_fed <- if (all(is.na(ap_fed))) NA_real_ else max(ap_fed, na.rm = TRUE)
  pad <- .scsj_any(ps$surface[u$surf])
  for (i in u$rows) {
    th <- x$thick[i]
    ratio <- if (!is.na(th) && th >= 10) 1.4 else if (!is.na(th) && th >= 5) 1.6 else 2
    # 2 cm or more, by either route (thickness unknown: undecided)
    th_ok <- if (is.na(th)) NA else th >= 2
    v <- if (!is.na(fed[i]) && !is.na(ap_fed)) {
           .scsj_and3(fed[i] >= ratio * ap_fed, th_ok)
         } else if (!is.na(fed[i])) NA
           else if (x$des$ir[i] || x$des$mn[i]) th_ok
           else if (x$des$known[i]) FALSE else NA
    out[i] <- if (isFALSE(pad) || isFALSE(v)) FALSE else if (isTRUE(v) && isTRUE(pad)) TRUE else NA
    if (!isTRUE(out[i])) break
  }
  out
}

# The paddy greyed horizon rows (IV.1): directly under a paddy surface
# horizon, with the surface-water wetness feature, 10 cm or more.
.scsj_paddy_grey_rows <- function(x, ps = .scsj_paddy_surface_rows(x), wet = .scsj_wetness_rows(x)) {
  out <- rep(FALSE, x$n)
  u <- .scsj_under_paddy_surface(x, ps)
  if (!length(u$rows)) return(out)
  pad <- .scsj_any(ps$surface[u$surf])
  rows <- integer(0)
  for (i in u$rows) { if (wet$surface[i] %in% FALSE) break; rows <- c(rows, i) }
  if (!length(rows)) return(out)
  th <- sum(x$thick[rows], na.rm = TRUE)
  open <- anyNA(x$thick[rows])
  v <- if (all(wet$surface[rows] %in% TRUE) && th >= 10) TRUE else if (th < 10 && !open) FALSE else NA
  out[rows] <- if (isFALSE(pad) || isFALSE(v)) FALSE else if (isTRUE(v) && isTRUE(pad)) TRUE else NA
  out
}

# ---- B. Organic Soils ------------------------------------------------------------

# Organic Soils (SCSJ 2017, great group B): 25 cm or more (cumulative) of
# peat material within 50 cm of the soil surface; or, under a fill of other
# material less than 35 cm thick, 25 cm or more of peat continuous from
# directly below the fill.
organic_soils_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("organic_soils_scsj", x, "II B, p. 2"))
  g <- .scsj_material_gate(x, x$peat)
  .scsj_result("organic_soils_scsj", g$passed, which(x$peat %in% TRUE),
               list(peat_cm_within_50 = .scsj_cum(x, x$peat, 0, 50)$sure, fill_cm = g$fill$thick,
                    soil_surface = x$surface_note),
               if (is.na(g$passed)) c("oc_pct", "designation") else character(0), "II B, p. 2")
}

# ---- C. Andosols -------------------------------------------------------------------

# Andosols (SCSJ 2017, great group C, kuroboku soils): 25 cm or more
# (cumulative) of layers with the kuroboku or the immature kuroboku feature
# within 50 cm of the soil surface; or, under a fill of other material less
# than 35 cm thick, 25 cm or more of them continuous from directly below it.
andosols_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("andosols_scsj", x, "II C, pp. 2-3"))
  # either feature: a layer undecided between them but certainly one of the
  # two (the raw immature criteria met) counts
  rows <- .scsj_or(x$kb, x$ikb_raw)
  g <- .scsj_material_gate(x, rows)
  .scsj_result("andosols_scsj", g$passed, which(rows %in% TRUE),
               list(kuroboku_cm_within_50 = .scsj_cum(x, rows, 0, 50)$sure, fill_cm = g$fill$thick),
               if (is.na(g$passed)) c("phosphate_absorption_coef", "phosphate_retention_pct",
                                      "al_ox_pct", "fe_ox_pct", "active_al_test") else character(0),
               "II C, pp. 2-3")
}

# ---- D. Podzols -----------------------------------------------------------------------

# Is there an accumulation horizon (rows `a`, `a_thick` cm) under a horizon
# `b` (`b_thick` cm), in one parent material? Runs of the minimum
# thicknesses are paired, `b` above `a` (the bleached horizon above the
# accumulation horizon, the sequence of the Podzols' concept: Hugo,
# 2026-10-31); with above = FALSE at any depth (route b with morphology).
.scsj_pair <- function(x, a, a_thick, b, b_thick, above = TRUE, same_material = TRUE) {
  # runs split at lithologic boundaries, so a pair inside one material is
  # found even when an adjacent qualifying layer is of another
  test <- function(sure) {
    ra <- if (sure) .scsj_runs(x, ifelse(a %in% TRUE, TRUE, FALSE), split_lith = TRUE) else
      .scsj_runs(x, a, bridge = TRUE, split_lith = TRUE)
    rb <- if (sure) .scsj_runs(x, ifelse(b %in% TRUE, TRUE, FALSE), split_lith = TRUE) else
      .scsj_runs(x, b, bridge = TRUE, split_lith = TRUE)
    for (p in ra) {
      if ((if (sure) p$sure else p$poss) < a_thick) next
      for (q in rb) {
        if ((if (sure) q$sure else q$poss) < b_thick) next
        if (above && (is.na(q$bottom) || q$bottom > p$top + 0.5)) next
        li <- unique(stats::na.omit(x$des$lith[c(p$rows, q$rows)]))
        if (same_material && length(li) > 1L) next
        return(c(p$rows, q$rows))
      }
    }
    NULL
  }
  s <- test(TRUE)
  if (!is.null(s)) return(list(passed = TRUE, rows = s))
  pp <- test(FALSE)
  if (!is.null(pp)) return(list(passed = NA, rows = pp))
  list(passed = FALSE, rows = integer(0))
}

# Podzols (SCSJ 2017, great group D): from one parent material; no paddy
# surface horizon; no layer with the red-yellow feature; and a podzolic
# accumulation horizon (by morphology) under a bleached horizon, or one by
# analysis under a bleached horizon or with one by morphology.
podzols_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("podzols_scsj", x, "II D, p. 3"))
  ag <- .scsj_argic_rows(x)
  ry <- .scsj_red_yellow_rows(x)
  pz <- .scsj_podzolic_rows(x, red_yellow = ry, argic = ag)
  # a bleached horizon is not itself an accumulation horizon
  bl <- .scsj_and(.scsj_bleached_rows(x), .scsj_not(.scsj_or(pz$morph, pz$analysis)))
  r_mb <- .scsj_pair(x, pz$morph, 2.5, bl, 1)
  r_ab <- .scsj_pair(x, pz$analysis, 2.5, bl, 1)
  r_am <- .scsj_pair(x, pz$analysis, 2.5, pz$morph, 2.5, above = FALSE)
  c4 <- .scsj_or3(r_mb$passed, r_ab$passed, r_am$passed)
  ps <- .scsj_paddy_surface_rows(x)
  c2 <- .scsj_not(.scsj_any(ps$unbleached))
  c3 <- .scsj_not(.scsj_any(ry))
  passed <- .scsj_and3(c2, c3, c4)
  used <- unique(c(r_mb$rows, r_ab$rows, r_am$rows))
  .scsj_result("podzols_scsj", passed, used,
               list(morphology_under_bleached = r_mb$passed, analysis_under_bleached = r_ab$passed,
                    analysis_with_morphology = r_am$passed, no_paddy_surface = c2, no_red_yellow = c3),
               if (is.na(passed)) c("munsell_hue_moist", "al_ox_pct", "fe_ox_pct", "land_use") else character(0),
               "II D, p. 3")
}

# ---- E. Fluvic soils --------------------------------------------------------------------

# Fluvic soils (SCSJ 2017, great group E): 25 cm or more (cumulative) of
# alluvial deposits (Holocene fluvial, marine or lacustrine) within 50 cm of
# the soil surface; or, under a fill of other material less than 35 cm
# thick, 25 cm or more of them continuous from directly below the fill.
fluvic_soils_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("fluvic_soils_scsj", x, "II E, p. 3"))
  rows <- x$orig$alluvial
  g <- .scsj_material_gate(x, rows)
  .scsj_result("fluvic_soils_scsj", g$passed, which(rows),
               list(alluvial_cm_within_50 = .scsj_cum(x, rows, 0, 50)$sure, fill_cm = g$fill$thick),
               character(0), "II E, p. 3")
}

# ---- the subsurface control section (IV.4) ---------------------------------------------

# The subsurface (jihyoso): the horizons between 20 and 60 cm of the soil
# surface, the plough layer excepted, or from 20 cm to the top of a gravel
# layer (20 % or more gravel) or the bedrock when one appears within 60 cm
# of the ground surface. A horizon counts when 5 cm or more of it lie in the
# section (Hugo, 2026-10-31). `covered`: whether the description covers
# the section.
.scsj_control <- function(x) {
  if (is.na(x$surface)) return(list(rows = integer(0), covered = NA, from = 20, to = 60))
  h <- x$h
  gv <- .scsj_col(h, "gravel_pct"); cf <- .scsj_col(h, "coarse_fragments_pct")
  grav <- ifelse(!is.na(gv), gv >= 20, ifelse(!is.na(cf), cf >= 20, FALSE))
  gdep <- x$top - x$ground                    # depth below the ground surface
  lim <- 60
  cut <- grav & !is.na(x$dtop) & x$dtop >= 20 & !is.na(gdep) & gdep <= 60
  if (any(cut)) lim <- min(lim, x$dtop[cut])
  if (is.finite(x$rock_top %||% NA) && x$rock_top + (x$surface - x$ground) <= 60) lim <- min(lim, x$rock_top)
  if (lim <= 20) return(list(rows = integer(0), covered = TRUE, from = 20, to = lim))
  db <- ifelse(is.na(x$dbot), Inf, x$dbot)
  ov <- pmin(db, lim) - pmax(x$dtop, 20)
  # an open horizon (no bottom) may hold 5 cm or more: it counts
  keep <- !is.na(x$dtop) & !is.na(ov) & (ov >= 5 | (is.na(x$dbot) & x$dtop < lim)) &
          !.scsj_plough_rows(x) & !(x$mineral %in% FALSE)
  rows <- which(keep)
  # coverage of [20, lim] by the closed rows (the plough layer included);
  # a horizon without a bottom covers only possibly: NA
  reach_of <- function(bots) {
    segs <- cbind(pmax(x$dtop[desc], 20), pmin(bots, lim))
    segs <- segs[segs[, 2] > segs[, 1], , drop = FALSE]
    reach <- 20
    if (nrow(segs)) {
      segs <- segs[order(segs[, 1]), , drop = FALSE]
      for (k in seq_len(nrow(segs))) { if (segs[k, 1] > reach + 0.5) break; reach <- max(reach, segs[k, 2]) }
    }
    reach >= lim - 0.5
  }
  desc <- which(!is.na(x$dtop))
  closed <- ifelse(is.na(x$dbot[desc]), -Inf, x$dbot[desc])
  covered <- if (reach_of(closed)) TRUE else if (reach_of(db[desc])) NA else FALSE
  list(rows = rows, covered = covered, from = 20, to = lim)
}

# Base status of the subsurface: `low` = some subhorizon has pH(H2O) under
# 5.5 or base saturation under 50 %; `high` = every subhorizon has pH(H2O)
# 5.5 or more or base saturation 50 % or more (not from liming).
.scsj_base_status <- function(x) {
  cs <- .scsj_control(x)
  rows <- cs$rows
  # no subsurface (a gravel layer or the bedrock from 20 cm, or the plough
  # layer through it): a criterion on its subhorizons is not met (Hugo,
  # 2026-11-01); a section the description may not cover: undecided
  if (!length(rows)) {
    v <- if (isTRUE(cs$covered)) FALSE else NA
    return(list(low = v, high = v, rows = rows))
  }
  ph <- .scsj_col(x$h, "ph_h2o")[rows]; bs <- .scsj_col(x$h, "bs_pct")[rows]
  low_r <- .scsj_or(ifelse(is.na(ph), NA, ph < 5.5), ifelse(is.na(bs), NA, bs < 50))
  high_r <- .scsj_or(ifelse(is.na(ph), NA, ph >= 5.5), ifelse(is.na(bs), NA, bs >= 50))
  low <- .scsj_any(low_r); high <- .scsj_all(high_r)
  # a section the description does not cover may hold another subhorizon
  if (!isTRUE(cs$covered)) { if (isFALSE(low)) low <- NA; if (isTRUE(high)) high <- NA }
  # on farmland the high status must not come from liming
  if (isTRUE(high) && isTRUE(as.logical(x$site$acid_neutralising_amendment %||% NA)[1L])) high <- NA
  list(low = low, high = high, rows = rows)
}

# ---- F. Red-Yellow soils ------------------------------------------------------------

# Red-Yellow soils (SCSJ 2017, great group F): an argic horizon with the
# red-yellow feature, 25 cm or more (adjacent layers), its top within 50 cm, or
# red-yellow weathered horizons, 25 cm or more, their top within 35 cm; a
# subhorizon of the subsurface with pH(H2O) under 5.5 or base saturation
# under 50 %; no bedrock within 30 cm.
red_yellow_soils_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("red_yellow_soils_scsj", x, "II F, pp. 3-4"))
  f <- .scsj_horizon_flags(x)
  ry <- .scsj_red_yellow_rows(x)
  agry <- .scsj_and(f$argic, ry)
  # 25 cm or more in the horizon (adjacent layers) whose top is within the
  # depth, not added up over separate horizons (Hugo, 2026-11-01)
  a <- .scsj_horizon_within(x, agry, 25, 50)$passed
  ryw <- .scsj_red_yellow_weathered_rows(x, f$weathered)
  b <- .scsj_horizon_within(x, ryw, 25, 35)$passed
  bs <- .scsj_base_status(x)
  rock <- .scsj_no_rock_within(x, 30)
  passed <- .scsj_and3(.scsj_or3(a, b), bs$low, rock)
  .scsj_result("red_yellow_soils_scsj", passed, which(agry %in% TRUE | ryw %in% TRUE),
               list(argic_red_yellow = a, red_yellow_weathered = b, acid_or_unsaturated_subsurface = bs$low,
                    no_rock_within_30 = rock),
               if (is.na(passed)) c("munsell_hue_moist", "clay_pct", "ph_h2o", "bs_pct") else character(0),
               "II F, pp. 3-4")
}

# ---- G. Stagnic soils ------------------------------------------------------------------

# Stagnic soils (SCSJ 2017, great group G): less than 25 cm (cumulative) of
# volcanic ejecta within 50 cm; a gley layer 10 cm or more, or a layer 10 cm
# or more with the groundwater or the surface-water wetness feature, its top
# within 50 cm; and these layers reaching 50 cm or deeper, or the bedrock.
stagnic_soils_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("stagnic_soils_scsj", x, "II G, p. 4"))
  ej <- .scsj_at_least(.scsj_cum(x, x$orig$ejecta, 0, 50), 25)
  c1 <- .scsj_not(ej)
  wet <- .scsj_wetness_rows(x)
  rows <- .scsj_or(wet$gley, wet$any)
  rt <- x$rock_top
  on_rock <- function(b) !is.na(b) && is.finite(rt %||% NA) && abs(b - rt) <= 0.5
  e <- .scsj_ends(x)
  c23 <- FALSE
  for (r in .scsj_runs(x, ifelse(rows %in% TRUE, TRUE, FALSE)))
    if (r$top <= 50 && r$sure >= 10 && (r$deep >= 50 || on_rock(r$bottom))) { c23 <- TRUE; break }
  if (!isTRUE(c23)) {
    for (r in .scsj_runs(x, rows, bridge = TRUE)) {
      # deep enough, or reaching the end of a description that stops above
      # 50 cm (not on rock)
      deep <- r$open || (!is.na(r$bottom) && r$bottom >= 50) || on_rock(r$bottom) ||
              (!is.na(r$bottom) && !is.na(e$depth) && abs(r$bottom - e$depth) <= 0.5 && e$depth < 50 && !e$on_rock)
      if (r$top <= 50 && r$poss >= 10 && deep) { c23 <- NA; break }
    }
  }
  if (isFALSE(c23) && !is.na(e$depth) && e$depth < 50 && !e$on_rock) c23 <- NA
  passed <- .scsj_and3(c1, c23)
  .scsj_result("stagnic_soils_scsj", passed, which(rows %in% TRUE),
               list(ejecta_25_within_50 = ej, wet_layers_to_50 = c23),
               if (is.na(passed)) c("redoximorphic_features_pct", "childs_test", "layer_origin") else character(0),
               "II G, p. 4")
}

# ---- H. Eutrosols ------------------------------------------------------------------------

# The top of a weathered or argic horizon within 50 cm of the soil surface.
# The weathered horizon has its other diagnostic horizons removed (IV.1,
# criterion 5: argic, gley, pseudogley, paddy iron accumulation and paddy
# greyed horizons; a podzolic accumulation horizon is not removed, since
# Brown Forest soils have a Podzolic subgroup).
.scsj_b_within_50 <- function(x) {
  f <- .scsj_horizon_flags(x)
  a <- .scsj_horizon_within(x, f$argic, 7.5, 50)
  w <- .scsj_horizon_within(x, f$weathered, 15, 50)
  list(passed = .scsj_or3(a$passed, w$passed), rows = c(a$rows, w$rows), argic = a$passed, weathered = w$passed)
}

# Eutrosols (SCSJ 2017, great group H): the top of a weathered or argic
# horizon within 50 cm; every subhorizon of the subsurface with pH(H2O) 5.5
# or more or base saturation 50 % or more (not from liming); no bedrock
# within 30 cm.
eutrosols_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("eutrosols_scsj", x, "II H, p. 4"))
  b <- .scsj_b_within_50(x)
  bs <- .scsj_base_status(x)
  rock <- .scsj_no_rock_within(x, 30)
  passed <- .scsj_and3(b$passed, bs$high, rock)
  .scsj_result("eutrosols_scsj", passed, b$rows,
               list(weathered_or_argic_within_50 = b$passed, base_rich_subsurface = bs$high,
                    no_rock_within_30 = rock),
               if (is.na(passed)) c("ph_h2o", "bs_pct") else character(0), "II H, p. 4")
}

# ---- I. Brown Forest soils ---------------------------------------------------------

# Brown Forest soils (SCSJ 2017, great group I): the top of a weathered or
# argic horizon within 50 cm; no bedrock within 30 cm.
brown_forest_soils_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("brown_forest_soils_scsj", x, "II I, pp. 4-5"))
  b <- .scsj_b_within_50(x)
  rock <- .scsj_no_rock_within(x, 30)
  passed <- .scsj_and3(b$passed, rock)
  .scsj_result("brown_forest_soils_scsj", passed, b$rows,
               list(weathered_or_argic_within_50 = b$passed, no_rock_within_30 = rock),
               if (is.na(passed)) c("designation", "structure_grade") else character(0), "II I, pp. 4-5")
}
