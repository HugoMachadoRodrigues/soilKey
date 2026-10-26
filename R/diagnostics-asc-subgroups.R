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
#' row of unknown role below it may be the lower part of the B.
#' @noRd
lower_b_sodic_asc <- function(pedon) {
  x <- .asc_horizons(pedon); h <- x$h
  bm <- .asc_b_and_maybe(x)
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
  .asc_result("sg_lower_b_sodic", passed, integer(0), list(),
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
