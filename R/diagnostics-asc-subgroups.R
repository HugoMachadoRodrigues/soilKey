# =============================================================================
# v0.9.286 -- subgroups of the Australian Soil Classification (3rd ed.). Most
# subgroups combine diagnostics the orders, suborders and great groups read
# already; this file holds those they add. Three-valued; the readings are in
# inst/rules/asc3/readings.yaml.
# =============================================================================

#' Peaty, humose, melacic and melanic horizons (subgroups)
#'
#' Read as for the Chernic suborders (\code{.asc_chernic_parts}), each kind on
#' its own.
#' @noRd
chernic_sg_asc <- function(pedon, kind = c("peaty", "humose", "melacic", "melanic")) {
  kind <- match.arg(kind)
  p <- .asc_chernic_parts(pedon)
  passed <- p[[kind]]
  .asc_result(paste0("sg_", kind), passed, integer(0), list(),
              if (is.na(passed)) c("oc_pct", "munsell_value_moist", "munsell_chroma_moist", "munsell_value_dry",
                                   "ph_h2o", "structure_grade", "ped_size_max_mm") else character(0),
              sprintf("Subgroups; glossary '%s horizon'", tools::toTitleCase(kind)))
}

# The B horizons of the present soil (B1 to BC, pans and buried horizons left
# out), and the rows of unknown role that may be B horizons: below the A
# horizons (or below the last designated row); one between two B horizons
# is a B horizon.
.asc_b_and_maybe <- function(x) {
  b <- .asc_b_rows(x)
  maybe <- which((x$maybe_b2 | (x$maybe_soil & x$trailing)) & !x$des$m)
  sw <- .asc_sandwiched(x, maybe, b)
  list(b = sort(c(b, sw)), maybe = setdiff(maybe, sw))
}

# The same for the B2 horizons (pans left out), as the order reads them:
# rows of unknown role among the designated ones, below the A horizons, may
# be B2 horizons (rows below the last designated one are not read); one
# between two B2 horizons is one.
.asc_b2_and_maybe <- function(x) {
  b2 <- which(x$b2 %in% TRUE & !x$des$m)
  maybe <- setdiff(.asc_b2_maybe(x), which(x$des$m))
  sw <- .asc_sandwiched(x, maybe, b2)
  list(b = sort(c(b2, sw)), maybe = setdiff(maybe, sw))
}

#' Acidic subgroups: the major part of the B2 horizon strongly acid (pHw
#' below 5.5, or pHCa below 4.6 without pHw), read as the order reads the
#' major part of the B2
#' @noRd
acidic_b2_asc <- function(pedon) {
  x <- .asc_horizons(pedon)
  passed <- .asc_b_major(pedon, x, .asc_strongly_acid(x$h), "b2")
  .asc_result("sg_acidic", passed, integer(0), list(),
              if (is.na(passed)) c("ph_h2o", "ph_cacl2", "designation") else character(0),
              "Subgroups Acidic; glossary 'Strongly acid'")
}

#' Snuffy subgroup (Ferrosols): an A horizon with a very fine granular
#' structure (peds under 2 mm; the finest size class of the Field Handbook,
#' recorded as 2 mm at most) and a dry strength of weak or very weak. The
#' low bulk density and water repellence the text adds ("usually", "may")
#' are not tested.
#' @noRd
snuffy_asc <- function(pedon) {
  x <- .asc_horizons(pedon); h <- x$h
  st <- tolower(trimws(as.character(h$structure_type %||% rep(NA_character_, x$n))))
  gr <- tolower(trimws(as.character(h$structure_grade %||% rep(NA_character_, x$n))))
  granular <- ifelse(grepl("structureless|none|apedal", gr) | grepl("massive|single", st), FALSE,
                     ifelse(is.na(st) | !nzchar(st), NA, grepl("granular", st)))
  ps <- suppressWarnings(as.numeric(h$ped_size_max_mm %||% rep(NA_real_, x$n)))
  fine <- ifelse(is.na(ps), NA, ps <= 2)
  sd <- tolower(trimws(as.character(h$strength_dry %||% rep(NA_character_, x$n))))
  weak <- ifelse(is.na(sd) | !nzchar(sd), NA, sd %in% c("very weak", "weak"))
  ok <- .and3v(.and3v(granular, fine), weak)
  a <- !is.na(x$role) & grepl("^A", x$role) & !x$des$buried
  # a row of unknown role above the first B horizon may be an A horizon
  b_top <- suppressWarnings(min(h$top_cm[!is.na(x$role) & grepl("^B", x$role) & !x$des$buried], na.rm = TRUE))
  may <- x$maybe_soil & !x$trailing & !is.na(h$top_cm) & h$top_cm < b_top
  passed <- if (!x$any_known) { if (all(ok %in% FALSE)) FALSE else NA }
            else if (any(ok[a] %in% TRUE)) TRUE
            else if (any(is.na(ok[a])) || any(!(ok[may] %in% FALSE))) NA else FALSE
  .asc_result("sg_snuffy", passed, which(a & ok %in% TRUE), list(),
              if (is.na(passed)) c("structure_type", "ped_size_max_mm", "strength_dry") else character(0),
              "Ferrosols, subgroup Snuffy")
}

#' Vertic subgroups: a B horizon in which at least 0.3 m has vertic
#' properties (clayey, with slickensides or lenticular peds)
#' @noRd
vertic_b_asc <- function(pedon) {
  x <- .asc_horizons(pedon)
  vert <- .asc_vertic_rows(x)
  bm <- .asc_b_and_maybe(x)
  th <- x$thick
  vb <- vert[bm$b]; tb <- th[bm$b]
  sure <- sum(tb[vb %in% TRUE & !is.na(tb)])
  # what may add to it: B rows of unknown verdict or thickness, and rows that
  # may be B horizons (a row of unknown thickness may be thick)
  tb_open <- ifelse(is.na(tb), Inf, tb)
  tm <- th[bm$maybe]; tm_open <- ifelse(is.na(tm), Inf, tm)
  open <- sum(tb_open[is.na(vb) | (vb %in% TRUE & is.na(tb))]) + sum(tm_open[!(vert[bm$maybe] %in% FALSE)])
  passed <- if (!x$any_known && !length(bm$maybe)) NA
            else if (sure >= 30) TRUE else if (sure + open < 30) FALSE else NA
  .asc_result("sg_vertic_b", passed, bm$b[vert[bm$b] %in% TRUE], list(vertic_cm = sure),
              if (is.na(passed)) c("slickensides", "structure_type", "field_texture", "clay_pct", "designation")
              else character(0),
              "Subgroups Vertic; glossary 'Vertic properties'")
}

#' Sodic subgroups: at least the lower part of the B horizon is sodic (the
#' deepest B horizon has an ESP of 6 or more; ESP as for the Sodosols). A
#' row of unknown role below it may be the lower part of the B. With
#' \code{part = "b2"} (Ferrosols): the lower part of the B2 horizon, the
#' deepest B2.
#' @noRd
lower_b_sodic_asc <- function(pedon, part = c("b", "b2")) {
  part <- match.arg(part)
  x <- .asc_horizons(pedon); h <- x$h
  bm <- if (part == "b") .asc_b_and_maybe(x) else .asc_b2_and_maybe(x)
  sod <- .asc_sodic(h)
  passed <- if (!length(bm$b)) {
    # no B designated: rows of unknown role may be the B
    if (x$any_known && (!length(bm$maybe) || all(sod[bm$maybe] %in% FALSE))) FALSE else NA
  } else if (all(is.na(h$top_cm[bm$b]))) NA else {
    last <- bm$b[which.max(h$top_cm[bm$b])]
    below <- bm$maybe[!is.na(h$top_cm[bm$maybe]) & h$top_cm[bm$maybe] > h$top_cm[last]]
    v <- sod[last]
    if (identical(v, TRUE) && !length(below)) TRUE
    else if (length(below)) { if (all(c(v, sod[below]) %in% FALSE)) FALSE else if (all(c(v, sod[below]) %in% TRUE)) TRUE else NA }
    else v
  }
  .asc_result("sg_lower_b_sodic", passed, integer(0), list(part = part),
              if (is.na(passed)) c("na_cmol", "cec_cmol", "ecec_cmol", "esp_pct", "designation") else character(0),
              "Subgroups Sodic; glossary 'Sodic'")
}

#' Mottled and Haplic subgroups: the major part of the B2t (or B2) mottled,
#' or whole coloured (not mottled); mottles as for the Hydrosols
#' @noRd
b_colour_sg_asc <- function(pedon, kind = c("mottled", "whole_coloured"), window = c("b2t", "b2")) {
  kind <- match.arg(kind); window <- match.arg(window)
  x <- .asc_horizons(pedon); h <- x$h
  m <- .asc_mottled_rows(h)
  passed <- .asc_b_major(pedon, x, if (kind == "mottled") m else !m, window)
  .asc_result(paste0("sg_", kind), passed, integer(0), list(window = window),
              if (is.na(passed)) c("redoximorphic_features_pct", "mottle_contrast", "designation") else character(0),
              "Subgroups Mottled and Haplic; glossary 'Mottled', 'Whole coloured'")
}

#' Reticulate subgroups: a reticulate horizon below the B2t (or B2): a row
#' below the deepest B2 horizon whose layer or rock origin or pan record names
#' a reticulate horizon (not a petroreticulate one); a reticulate horizon
#' shows, so with none named there is none.
#' @noRd
reticulate_below_asc <- function(pedon) {
  x <- .asc_horizons(pedon); h <- x$h
  txt <- tolower(paste(h$layer_origin %||% rep(NA, x$n), h$rock_origin %||% rep(NA, x$n),
                       h$pan_type %||% rep(NA, x$n)))
  named <- grepl("reticul", txt) & !grepl("petroreticul", txt)
  b2 <- which(x$role %in% "B2" & !x$des$buried & !is.na(h$top_cm))
  passed <- if (!any(named)) FALSE
            else if (!length(b2)) NA
            else {
              # the lower boundary of the B2t: the bottom of its deepest
              # horizon; with that bottom unrecorded, rows do not overlap, so
              # a row that starts below its top lies below it
              deep <- b2[which.max(h$top_cm[b2])]; bot <- h$bottom_cm[deep]
              cand <- which(named & !(seq_len(x$n) %in% b2) & !is.na(h$top_cm))
              below <- if (is.na(bot)) h$top_cm[cand] > h$top_cm[deep] + 0.5
                       else h$top_cm[cand] >= bot - 0.5
              .asc_any_rows(below)
            }
  .asc_result("sg_reticulate", passed, which(named), list(),
              if (is.na(passed)) c("layer_origin", "designation") else character(0),
              "Subgroups Reticulate; glossary 'Reticulate horizon'")
}

#' Silpanic subgroups: a siliceous pan in the lower A and/or upper B horizon
#' (an A2, A3, AB, A/B, BA, B/A or B1, or the uppermost B2); read as the pans
#' of the great groups
#' @noRd
silpan_ab_asc <- function(pedon) {
  x <- .asc_horizons(pedon); h <- x$h
  v <- .asc_pan_rows(x, c("silcrete", "siliceous"))
  b2 <- which(x$role %in% "B2" & !x$des$buried)
  first_b2 <- if (length(b2)) b2[which.min(h$top_cm[b2])] else integer(0)
  rows <- c(which(x$role %in% c("A2", "A3", "AB", "A/B", "BA", "B/A", "B1") & !x$des$buried), first_b2)
  # a row of unknown role between two of them is lower A or upper B; one
  # below the A1 horizons and no deeper than the uppermost B2 may be
  maybe <- which(x$maybe_soil)
  sw <- .asc_sandwiched(x, maybe, rows)
  first_top <- suppressWarnings(min(h$top_cm[rows], na.rm = TRUE))
  a1 <- which(x$role %in% "A1" & !x$des$buried & !is.na(h$bottom_cm) & h$top_cm < first_top)
  lo <- if (length(a1)) max(h$bottom_cm[a1]) else -Inf
  # a row that starts at or below the bottom of the uppermost B2 lies below
  # it (rows do not overlap: with that bottom unrecorded, one that starts
  # below its top); with no B2, one at the bottom of the listed rows may be
  # the B2
  mt <- h$top_cm[maybe]
  upper <- if (length(first_b2)) {
    if (!is.na(h$bottom_cm[first_b2])) mt < h$bottom_cm[first_b2] - 0.5 else mt <= h$top_cm[first_b2] + 0.5
  } else {
    hi <- if (length(rows)) suppressWarnings(max(h$bottom_cm[rows], na.rm = TRUE)) else Inf
    mt <= (if (is.finite(hi)) hi + 0.5 else Inf)
  }
  near <- setdiff(maybe[!is.na(mt) & mt >= lo - 0.5 & upper], sw)
  v_near <- ifelse(v[near] %in% FALSE, FALSE, NA)
  passed <- if (!x$any_known) { if (all(v %in% FALSE)) FALSE else NA }
            else .asc_any_rows(c(v[c(rows, sw)], v_near))
  .asc_result("sg_silpanic", passed, rows[v[rows] %in% TRUE], list(),
              if (is.na(passed)) c("pan_type", "designation", "cementation_class") else character(0),
              "Subgroups Silpanic; glossary 'Siliceous pan'")
}
