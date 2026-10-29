# =============================================================================
# v0.9.301 -- the keys to subgroups of the New Zealand Soil Classification,
# 4th ed.: the Semiarid, Oxidic, Granular and Pallic Soils (78 subgroups of
# 17 groups). As in R/diagnostics-nzsc-subgroups.R, each subgroup after the
# first is "other soils that ...", so a gate tests only its own criteria,
# and the last subgroup of each group is the default. Three-valued; readings
# in inst/rules/nzsc4/readings.yaml. Hugo's decision of 2026-10-29: the share
# of the coarse rock fragments weathered enough to break easily by hammer or
# spade is recorded in a new attribute (weathered_fragments_pct), and not
# recorded reads as none; sodic features not measured are absent from a
# designated horizon without the n suffix.
# =============================================================================

# ---- helpers -----------------------------------------------------------------------

# A layer of adjacent rows where `ok` holds, at least `min_cm` thick inside
# [zt, zb] (only the part inside counts); a window the description does not
# reach (not on rock or a hard layer) may hold it below.
.nzsc_layer_in <- function(x, ok, zt, zb, min_cm) {
  h <- x$h; rows <- .nzsc_rows_in(x, zt, zb)
  if (!length(rows)) return(.nzsc_open_below(FALSE, x, zb))
  th <- .asc_overlap(h$top_cm[rows], ifelse(is.na(h$bottom_cm[rows]), zb, h$bottom_cm[rows]), zt, zb)
  best <- c(0, 0); sure <- 0; poss <- 0; prev <- NA_real_
  for (k in seq_along(rows)) {
    i <- rows[k]
    if (!is.na(prev) && abs(h$top_cm[i] - prev) > 0.5) { sure <- 0; poss <- 0 }
    if (ok[i] %in% FALSE) { sure <- 0; poss <- 0 }
    else { sure <- if (ok[i] %in% TRUE) sure + th[k] else 0; poss <- poss + th[k] }
    best <- pmax(best, c(sure, poss)); prev <- h$bottom_cm[i]
  }
  v <- if (best[1L] >= min_cm) TRUE else if (best[2L] >= min_cm) NA else FALSE
  .nzsc_open_below(v, x, zb)
}

# `feat` in a row of `layer`, or in the row directly beneath (side
# "beneath") or directly above ("above") the layer. A row of the layer next
# to another counts as within it. A layer the description ends in (not on
# rock or a hard layer) may have the feature beneath it.
.nzsc_near <- function(x, layer, feat, side = c("beneath", "above")) {
  side <- match.arg(side)
  h <- x$h; rows <- .nzsc_rows(x)
  lr <- rows[!(layer[rows] %in% FALSE)]
  if (!length(lr)) return(FALSE)
  nxt <- .nzsc_next_row(x)
  .nzsc_any(vapply(lr, function(i) {
    j <- if (side == "beneath") nxt[i] else {
      u <- rows[rows != i & !is.na(h$bottom_cm[rows]) & abs(h$bottom_cm[rows] - h$top_cm[i]) <= 0.5]
      if (length(u)) u[1L] else NA_integer_
    }
    # nothing described directly beneath (not on rock): open (v0.9.301);
    # nothing above a layer at the surface: no
    nb <- if (!is.na(j)) feat[j] else if (side == "beneath" && !.nzsc_ends_hard(x)) NA else FALSE
    .and3(c(layer[i], .any3(c(feat[i], nb))))
  }, NA))
}

# Sodic features of each row in the subgroups: as .nzsc_sodic_rows() reads
# them, but with neither ESP nor exchangeable Na measured a designated
# horizon without the n suffix is not sodic (Hugo's decision, 2026-10-29, as
# the z suffix for saline soil material); a row with no designation stays
# undecided. The group key of the Argillic Pallic Soils is unchanged.
.nzsc_sodic_subgroup_rows <- function(x) {
  h <- x$h; n <- x$n
  esp <- h$esp_pct %||% rep(NA_real_, n)
  # exchangeable Na below 0.7 cmol/kg with neither CEC nor ESP does not
  # settle the ESP (v0.9.301): read as not measured
  na <- h$na_cmol; cec <- h$cec_cmol
  measured <- !is.na(esp) | (!is.na(na) & (!is.na(cec) | na >= 0.7))
  ifelse(x$des$n, TRUE, ifelse(measured, .nzsc_sodic_rows(x), ifelse(x$known, FALSE, NA)))
}

# Argillic horizon rows (a pan, m, is not one).
.nzsc_argillic_np <- function(x) ifelse(x$des$m, FALSE, .nzsc_argillic_rows(x))

# The layer a Pallic or Semiarid subgroup reads, by name.
.nzsc_layer_rows <- function(x, layer = c("argillic", "argillic_or_cutanic", "fragipan")) {
  layer <- match.arg(layer)
  switch(layer,
    argillic = .nzsc_argillic_np(x),
    argillic_or_cutanic = .any3v(.nzsc_argillic_np(x), .nzsc_cutanic_or_coated(x)),
    fragipan = .nzsc_pan_rows(x, "fragipan"))
}

# ---- shared subgroup gates -----------------------------------------------------------------

# Alkaline subgroups (SAK, SJK, SIK): pH 8.6 or more in some part within
# `depth` cm of the mineral soil surface.
alkaline_subgroup_nzsc <- function(pedon, depth = 60) {
  x <- .nzsc_horizons(pedon); h <- x$h
  pre <- .nzsc_sub_ms_or_na(x, "alkaline_subgroup_nzsc", "alkaline subgroups", "ph_h2o"); if (!is.null(pre)) return(pre)
  zb <- x$ms + as.numeric(depth)
  ok <- ifelse(is.na(h$ph_h2o), NA, h$ph_h2o >= 8.6)
  rows <- .nzsc_rows_in(x, x$ms, zb); rows <- rows[x$mineral[rows]]
  v <- if (!length(rows)) FALSE else .nzsc_some_part_in(x, rows, ok, x$ms, zb)
  .nzsc_subgroup("alkaline_subgroup_nzsc", v, "alkaline subgroups", missing = "ph_h2o")
}

# Weathered subgroups (SAW, PPWJ, PJMW, PJW): coarse rock fragments, the
# majority of which (more than half by volume, weighed by each horizon's
# fragments and thickness) are weathered enough to break easily by hammer
# or spade (weathered_fragments_pct; recorded when met, Hugo's decision of
# 2026-10-29: not recorded reads as none). No coarse fragments: no.
weathered_fragments_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h; n <- x$n
  wf0 <- h$weathered_fragments_pct %||% rep(NA_real_, n)
  cf0 <- h$coarse_fragments_pct
  # a weathered share recorded on a row whose fragments are not recorded
  # shows fragments of unknown amount there (v0.9.301)
  cf <- ifelse(is.na(cf0), ifelse(!is.na(wf0), NA, 0), pmax(0, cf0))
  wf <- ifelse(is.na(wf0), 0, pmin(100, pmax(0, wf0)))
  rows <- .nzsc_rows(x); rows <- rows[x$mineral[rows] & !(cf[rows] %in% 0)]
  if (!length(rows)) return(.nzsc_subgroup("weathered_fragments_nzsc", FALSE, "weathered subgroups"))
  th <- x$thick[rows]; open <- is.na(th) | is.na(cf[rows])
  share <- function(w) if (sum(w) <= 0) NA else sum(w * wf[rows]) / sum(w) > 50
  known_w <- ifelse(is.na(th), 0, pmax(0, th)) * ifelse(is.na(cf[rows]), 0, cf[rows])
  w0 <- ifelse(open, 0, known_w)
  v <- if (!any(open)) share(w0) else if (sum(w0) <= 0) share(ifelse(open, 1, 0)) else {
    a <- share(w0); b <- share(ifelse(open, 1e6, known_w)); if (identical(a, b)) a else NA }
  .nzsc_subgroup("weathered_fragments_nzsc", v, "weathered subgroups", missing = "weathered_fragments_pct",
                 layers = rows[wf[rows] > 50])
}

# Lithic or paralithic contact within `depth` cm of the mineral soil surface
# (XNX, XOX): an R or CR horizon with its top less than `depth` cm down; a
# description that ends above (not on rock) may meet one below.
contact_subgroup_nzsc <- function(pedon, depth = 90) {
  x <- .nzsc_horizons(pedon); h <- x$h
  pre <- .nzsc_sub_ms_or_na(x, "contact_subgroup_nzsc", "rocky subgroups", "designation"); if (!is.null(pre)) return(pre)
  zb <- x$ms + as.numeric(depth)
  rows <- .nzsc_rows(x)
  rock <- x$role %in% c("R", "CR")
  # a row of unknown role with no fine-earth record (texture, clay, pH,
  # carbon) may be rock (v0.9.301)
  fe <- !is.na(h$field_texture) | !is.na(h$clay_pct) | !is.na(h$sand_pct) | !is.na(h$ph_h2o) | !is.na(h$oc_pct)
  maybe <- x$unknown_role & !fe
  v <- .nzsc_any(ifelse(rock, TRUE, ifelse(maybe, NA, FALSE))[rows[h$top_cm[rows] < zb - 0.5]])
  .nzsc_subgroup("contact_subgroup_nzsc", .nzsc_open_below(v, x, zb), "rocky subgroups", missing = "designation",
                 layers = rows[rock[rows]])
}

# Mottled-acidic and acidic subgroups of the Granular Soils use
# acidic_b_subgroup_nzsc() (R/diagnostics-nzsc-subgroups-zlme.R) with `ph`
# 5.1. Allophanic subgroups (NEL, NXL, NOL): a layer 10 cm or more thick
# within 60 cm of the mineral soil surface that meets allophanic soil
# material apart from the bulk density.
allophanic_layer_subgroup_nzsc <- function(pedon, min_cm = 10, depth = 60) {
  x <- .nzsc_horizons(pedon)
  miss <- c("phosphate_retention_pct", "naf_test", "sensitivity_class")
  pre <- .nzsc_sub_ms_or_na(x, "allophanic_layer_subgroup_nzsc", "allophanic subgroups", miss); if (!is.null(pre)) return(pre)
  al <- .nzsc_allophanic_rows(x, density = FALSE)
  .nzsc_subgroup("allophanic_layer_subgroup_nzsc",
                 .nzsc_layer_in(x, al, x$ms, x$ms + as.numeric(depth), as.numeric(min_cm)),
                 "allophanic subgroups", missing = miss, layers = which(al %in% TRUE))
}

# A feature within, or directly beneath or above, a layer (PUJN, PXJN, PXMC,
# PXCN, PXC, PJN): `layer` argillic, argillic or cutanic, or fragipan;
# `feature` sodic features or a calcareous horizon (a calcareous fragipan is
# read as calcium carbonate within its prism interiors).
near_layer_subgroup_nzsc <- function(pedon, layer = c("argillic", "argillic_or_cutanic", "fragipan"),
                                     feature = c("sodic", "calcareous"), side = c("beneath", "above")) {
  layer <- match.arg(layer); feature <- match.arg(feature); side <- match.arg(side)
  x <- .nzsc_horizons(pedon)
  lay <- .nzsc_layer_rows(x, layer)
  feat <- if (feature == "sodic") .nzsc_sodic_subgroup_rows(x) else .nzsc_calcareous_rows(x)
  .nzsc_subgroup("near_layer_subgroup_nzsc", .nzsc_near(x, lay, feat, side), "P, S subgroups",
                 list(layer = layer, feature = feature, side = side),
                 c("designation", if (feature == "sodic") c("esp_pct", "na_cmol") else c("effervescence_class", "caco3_pct")))
}

# An argillic (or argillic or cutanic) horizon overlying the fragipan (PXJN,
# PXMJ, PXJ): its base at or above the top of a fragipan row.
over_fragipan_subgroup_nzsc <- function(pedon, upper = c("argillic", "argillic_or_cutanic")) {
  upper <- match.arg(upper)
  x <- .nzsc_horizons(pedon)
  up <- .nzsc_layer_rows(x, upper)
  fr <- .nzsc_pan_rows(x, "fragipan")
  .nzsc_subgroup("over_fragipan_subgroup_nzsc", .nzsc_beneath(x, up, fr), "P, PX", list(upper = upper),
                 c("designation", "clay_pct", "clay_films_amount"))
}

# In some part of the argillic horizon (PJMY, PJY: `test: acid`, pH below 5.5,
# with an eluvial horizon when `eluvial`; PJA: `test: aged_colour`, hue
# 7.5YR or redder, or 10YR with chroma 6 or more).
argillic_part_subgroup_nzsc <- function(pedon, test = c("acid", "aged_colour"), eluvial = FALSE) {
  test <- match.arg(test)
  x <- .nzsc_horizons(pedon); h <- x$h
  a <- .nzsc_argillic_np(x)
  cond <- if (test == "acid") ifelse(is.na(h$ph_h2o), NA, h$ph_h2o < 5.5) else {
    hue <- .nzsc_hue_num(h$munsell_hue_moist); ch <- h$munsell_chroma_moist
    neutral <- !is.na(h$munsell_hue_moist) & grepl("^N", toupper(trimws(as.character(h$munsell_hue_moist))))
    ifelse(neutral, FALSE, ifelse(is.na(hue), NA, ifelse(hue <= 17.5 + 0.01, TRUE,
           ifelse(abs(hue - 20) < 0.01, ifelse(is.na(ch), NA, ch >= 6), FALSE))))
  }
  rows <- which(!(a %in% FALSE) & !is.na(h$top_cm))
  part <- if (!length(rows)) FALSE else .nzsc_some_part(x, rows, .and3v(a, cond))
  e <- if (isTRUE(eluvial)) .nzsc_any(.nzsc_eluvial_rows(x)[.nzsc_rows(x)]) else TRUE
  .nzsc_subgroup("argillic_part_subgroup_nzsc", .and3(c(part, e)), "P, argillic subgroups",
                 list(test = test, eluvial = if (isTRUE(eluvial)) e else NULL),
                 c("designation", if (test == "acid") "ph_h2o" else c("munsell_hue_moist", "munsell_chroma_moist")))
}

# PJC: a calcareous horizon with its top less than 90 cm from the mineral
# soil surface, or within or directly below the argillic horizon.
calcareous_argillic_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h
  miss <- c("effervescence_class", "caco3_pct", "ph_h2o")
  pre <- .nzsc_sub_ms_or_na(x, "calcareous_argillic_nzsc", "P, PJC", miss); if (!is.null(pre)) return(pre)
  calc <- .nzsc_calcareous_rows(x)
  rows <- .nzsc_rows(x)
  within90 <- .nzsc_any(calc[rows[h$top_cm[rows] < x$ms + 90 - 0.5 & x$mineral[rows]]])
  .nzsc_subgroup("calcareous_argillic_nzsc", .any3(c(within90, .nzsc_near(x, .nzsc_argillic_np(x), calc, "beneath"))),
                 "P, PJC", missing = miss)
}

# SJL: an argillic horizon predominantly in the form of lamellae (a
# designation naming lamellae), as the Laminar Pallic Soils read it: the
# lamellae are more than half of the argillic thickness.
lamellar_argillic_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon); h <- x$h
  a <- .nzsc_argillic_np(x)
  lam <- grepl("lamell", tolower(as.character(h$designation)))
  # a row with no bottom weighs both lightly and heavily (v0.9.301)
  judge <- function(th) {
    yes <- sum(th[a %in% TRUE & lam]); no <- sum(th[a %in% TRUE & !lam])
    may_yes <- sum(th[is.na(a) & lam]); may_no <- sum(th[is.na(a) & !lam])
    if (yes > no + may_no) TRUE else if (yes + may_yes <= no) FALSE else NA
  }
  v1 <- judge(ifelse(is.na(x$thick), 0, pmax(0, x$thick))); v2 <- judge(ifelse(is.na(x$thick), 1e6, pmax(0, x$thick)))
  v <- if (identical(v1, v2)) v1 else NA
  .nzsc_subgroup("lamellar_argillic_nzsc", v, "S, SJL", missing = "designation")
}

# PPC: a horizon at least weakly indurated (a pan, m; a cementation class of
# weakly cemented or more; a recorded pan) with its top less than `depth`
# cm from the mineral soil surface, as the Impeded Allophanic Soils read it.
cemented_subgroup_nzsc <- function(pedon, depth = 90) {
  x <- .nzsc_horizons(pedon); h <- x$h; n <- x$n
  pre <- .nzsc_sub_ms_or_na(x, "cemented_subgroup_nzsc", "P, PPC", c("cementation_class", "designation")); if (!is.null(pre)) return(pre)
  cem <- .nzsc_txt(h$cementation_class, n); pt <- .nzsc_txt(h$pan_type, n)
  # a recorded pan, apart from the uncemented fragipans and densipans
  # (v0.9.301)
  ok <- x$des$m | grepl("weak|moder|strong|indurat", cem) |
        (!is.na(pt) & nzchar(pt) & !grepl("^none|fragipan|densipan", pt))
  rows <- .nzsc_rows(x); rows <- rows[h$top_cm[rows] < x$ms + as.numeric(depth) - 0.5]
  .nzsc_subgroup("cemented_subgroup_nzsc", any(ok[rows]), "P, PPC", layers = rows[ok[rows]])
}

# PIMD, PID: moderately or strongly pedal with peds of `max_mm` or less
# (ped_size_max_mm) throughout the major part of the B horizon to 60 cm from
# the mineral soil surface, or to its base when shallower.
pedal_b_subgroup_nzsc <- function(pedon, max_mm = 100) {
  x <- .nzsc_horizons(pedon); h <- x$h; n <- x$n
  miss <- c("structure_grade", "ped_size_max_mm", "designation")
  pre <- .nzsc_sub_ms_or_na(x, "pedal_b_subgroup_nzsc", "P, PIMD/PID", miss); if (!is.null(pre)) return(pre)
  g <- tolower(as.character(h$structure_grade)); t <- tolower(as.character(h$structure_type))
  # a massive or single-grain horizon with no grade recorded is apedal
  grade <- ifelse(grepl("moderate|strong", g), TRUE,
           ifelse(!is.na(h$structure_grade) | grepl("massive|single", t), FALSE, NA))
  size <- h$ped_size_max_mm %||% rep(NA_real_, n)
  ok <- .and3v(grade, ifelse(is.na(size), NA, size <= as.numeric(max_mm)))
  # mineral rows of unknown role are tried as B and as not B (v0.9.301)
  .nzsc_subgroup("pedal_b_subgroup_nzsc", .nzsc_b_bc_major_part(x, ok, x$ms + 60, roles = "B"), "P, PIMD/PID",
                 missing = miss)
}

# ---- X: Oxidic Soils -----------------------------------------------------------------------

# XNB, XOB: within 90 cm of the mineral soil surface, a decrease in clay with
# depth of more than 20 % of the clay of the horizon with the most clay (lab
# clay; any horizon of the window without it leaves this open), or more
# than 5 % coarse rock fragments in a horizon (not recorded read as none);
# XOB (`contact`) also a lithic or paralithic contact.
brown_oxidic_nzsc <- function(pedon, contact = FALSE) {
  x <- .nzsc_horizons(pedon); h <- x$h
  miss <- c("clay_pct", "coarse_fragments_pct")
  pre <- .nzsc_sub_ms_or_na(x, "brown_oxidic_nzsc", "X, XNB/XOB", miss); if (!is.null(pre)) return(pre)
  zb <- x$ms + 90
  rows <- .nzsc_rows_in(x, x$ms, zb); rows <- rows[x$mineral[rows]]
  clay <- h$clay_pct[rows]
  # with clays missing, a decrease below the measured maximum holds whatever
  # they are when every missing row lies above the decreasing row (v0.9.301)
  dec <- if (length(rows) < 2L) FALSE else if (!anyNA(clay)) {
    k <- which.max(clay); low <- seq_along(rows) > k
    any(clay[low] < 0.8 * clay[k])
  } else {
    known <- which(!is.na(clay)); miss_i <- which(is.na(clay))
    if (!length(known)) NA else {
      k <- known[which.max(clay[known])]
      j <- known[known > k & clay[known] < 0.8 * clay[k]]
      if (length(j) && all(miss_i < max(j))) TRUE else NA
    }
  }
  dec <- .nzsc_open_below(dec, x, zb)
  cf <- h$coarse_fragments_pct[rows]
  frag <- any(!is.na(cf) & cf > 5)
  rock <- if (isTRUE(contact)) {
    rr <- .nzsc_rows(x); any(x$role[rr] %in% c("R", "CR") & h$top_cm[rr] < zb - 0.5)
  } else FALSE
  .nzsc_subgroup("brown_oxidic_nzsc", .any3(c(dec, frag, rock)), "X, XNB/XOB",
                 list(clay_decrease = dec, fragments = frag, contact = if (isTRUE(contact)) rock else NULL), miss)
}

# ---- N: Granular Soils ---------------------------------------------------------------------

# NPX: a cutanoxidic horizon more than 30 cm thick with its upper boundary
# below (deeper than) 25 cm from the mineral soil surface.
cutanoxidic_subgroup_nzsc <- function(pedon) {
  x <- .nzsc_horizons(pedon)
  miss <- c("designation", "ped_size_max_mm", "stickiness", "plasticity")
  pre <- .nzsc_sub_ms_or_na(x, "cutanoxidic_subgroup_nzsc", "N, NPX", miss); if (!is.null(pre)) return(pre)
  .nzsc_subgroup("cutanoxidic_subgroup_nzsc",
                 .nzsc_run_starting(x, .nzsc_cutanoxidic_rows(x), x$ms + 25 + 1, x$ms + 1000, 30), "N, NPX",
                 missing = miss)
}
