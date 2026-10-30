# =============================================================================
# soilKey -- the WRB 2022 redox reading of a profile (v0.9.312).
#
# Gleyic properties (Ch 3.2.6, p. 75), stagnic properties (Ch 3.2.14, p. 81)
# and reducing conditions (Ch 3.2.10, p. 79), layer by layer, as the Gleysols,
# Stagnosols and Planosols gates and gleyic_properties() and
# stagnic_properties() read them.
#
# Field descriptions seldom say where the mottles sit (on the aggregate
# surfaces, gleyic criterion 2, or inside the aggregates, stagnic criterion
# 2). Hugo's reading (2026-10-29, option c): a layer has gleyic properties
# when its matrix has a reductimorphic colour (criterion 1, a or b: with up
# to 5% oximorphic features the layer is criterion 1 itself, with more they
# are read as the gleyic features of criterion 2 over a reduced matrix),
# when its oximorphic features lie directly on a layer with such a matrix
# (criterion 3), or when the site records groundwater within 150 cm that is
# not perched; otherwise its oximorphic features are stagnic. A claric layer
# directly above the mottled layer is the stagnic sequence of 3.2.14
# criterion 5 ("an overlying Fe-poor layer"). With neither colour nor water
# evidence the layer stays undecided (NA), and so do the Gleysols and
# Stagnosols.
# =============================================================================


#' Hues of the reductimorphic colours in WRB 2022 3.2.6 criterion 1.b
#' (p. 75): 2.5Y or 5Y with a chroma of 2 or less.
#' @noRd
.GLEYIC_HUE_1B_REGEX <- "^(2\\.5Y|5Y)(\\s|$)"


#' Reductimorphic colour of the matrix, WRB 2022 3.2.6 criterion 1 (p. 75)
#'
#' TRUE for a moist hue of N, 10Y, GY, G, BG, B or PB (1.a), or of 2.5Y or 5Y
#' with a chroma of 2 or less (1.b); FALSE for any other hue; NA when the hue,
#' or the chroma of a 2.5Y or 5Y hue, is not recorded.
#' @noRd
.reductimorphic_colour <- function(hue, chroma) {
  hu <- toupper(trimws(as.character(hue)))
  chroma <- suppressWarnings(as.numeric(chroma))
  a <- grepl(.GLEYIC_HUE_REGEX, hu, perl = TRUE)
  b <- grepl(.GLEYIC_HUE_1B_REGEX, hu, perl = TRUE)
  out <- ifelse(a, TRUE, ifelse(b, chroma <= 2, FALSE))
  out[is.na(hu) | !nzchar(hu)] <- NA
  out
}


#' Groundwater recorded at the site within 150 cm and not perched
#'
#' Hugo's reading of 3.2.6 against 3.2.14 (2026-10-29): TRUE when a water
#' table that is not perched (observed, apparent or flooded) is recorded
#' within 150 cm; FALSE when none was observed, it is perched or it lies
#' deeper; NA when the site does not say, or gives the kind with no depth.
#' Reads \code{site$groundwater_kind} and \code{site$groundwater_depth_cm}.
#' @noRd
.groundwater_within_150 <- function(site) {
  kind <- tolower(trimws(as.character(site$groundwater_kind %||% NA_character_)))[1]
  depth <- suppressWarnings(as.numeric(site$groundwater_depth_cm %||% NA_real_))[1]
  if (!is.na(kind) && kind %in% c("none observed", "none", "perched")) return(FALSE)
  if (!is.na(depth)) return(depth <= 150)
  if (!is.na(kind) && kind == "flooded") return(TRUE)
  NA
}


#' Runs of adjoining layers that together reach a thickness
#'
#' \code{ok} is TRUE, FALSE or NA per layer. A run is a group of layers that
#' touch (the top of one at or above the bottom of the one above). Returns
#' \code{passed} TRUE when a run of TRUE layers is at least \code{min_cm}
#' thick and starts at or above \code{max_top_cm}; NA when only a run of TRUE
#' and NA layers would; FALSE otherwise. \code{layers} are those of the
#' passing runs (of the possible ones when NA), \code{top} the shallowest
#' start among them.
#' @noRd
.adjoining_runs <- function(h, ok, min_cm, max_top_cm = Inf) {
  runs <- function(sel) {
    idx <- which(sel & !is.na(h$top_cm) & !is.na(h$bottom_cm))
    if (!length(idx)) return(list())
    idx <- idx[order(h$top_cm[idx])]
    grp <- cumsum(c(TRUE, h$top_cm[idx][-1L] > cummax(h$bottom_cm[idx])[-length(idx)] + 0.5))
    out <- lapply(split(idx, grp), function(r) list(
      layers = r, top = min(h$top_cm[r]), bottom = max(h$bottom_cm[r]),
      thickness = max(h$bottom_cm[r]) - min(h$top_cm[r])))
    Filter(function(r) r$thickness >= min_cm && r$top <= max_top_cm, out)
  }
  sure <- runs(ok %in% TRUE)
  if (length(sure))
    return(list(passed = TRUE, layers = sort(unlist(lapply(sure, `[[`, "layers"))),
                top = min(vapply(sure, `[[`, 0, "top"))))
  maybe <- runs(!(ok %in% FALSE))
  if (length(maybe))
    return(list(passed = NA, layers = sort(unlist(lapply(maybe, `[[`, "layers"))),
                top = min(vapply(maybe, `[[`, 0, "top"))))
  list(passed = FALSE, layers = integer(0), top = NA_real_)
}


#' Per-layer status of a sub-test that keeps \code{details[[as.character(i)]]$passed}
#' (layers it could not evaluate are NA).
#' @noRd
.subtest_by_layer <- function(res, n) {
  vapply(seq_len(n), function(i) {
    d <- res$details[[as.character(i)]]
    if (is.null(d) || is.null(d$passed)) NA else as.logical(d$passed)[1]
  }, NA)
}


#' What would decide the undecided layers \code{idx} of a redox reading.
#' @noRd
.redox_missing <- function(z, idx) {
  if (!length(idx)) return(character(0))
  m <- character(0)
  if (any(is.na(z$reductimorphic[idx])))
    m <- c(m, "munsell_hue_moist", "munsell_chroma_moist")
  if (any(is.na(z$oximorphic[idx]))) m <- c(m, "redoximorphic_features_pct")
  if (any(z$oximorphic[idx] %in% TRUE))
    m <- c(m, "site$groundwater_kind", "site$groundwater_depth_cm")
  unique(m)
}


#' The redox reading of every layer (WRB 2022 3.2.6, 3.2.10, 3.2.14)
#'
#' Returns a list of per-layer vectors (TRUE, FALSE or NA):
#' \itemize{
#'   \item \code{reductimorphic}: matrix colour of 3.2.6 criterion 1;
#'   \item \code{oximorphic}: redoximorphic features over \code{min_redox_pct}
#'         (3.2.6 criterion 2, "> 5\%");
#'   \item \code{crit1}: 3.2.6 criterion 1, the reductimorphic matrix with
#'         no more than \code{min_redox_pct} oximorphic features;
#'   \item \code{gleyic}: gleyic properties, by Hugo's reading (file header);
#'   \item \code{stagnic}: stagnic properties: oximorphic features that are not
#'         gleyic (criteria 1-3), or claric colours in 95\% or more of the layer
#'         above an abrupt textural difference or a layer of 1.5 kg/dm3 or more
#'         (criterion 4), or above a layer with stagnic oximorphic features
#'         (criterion 5);
#'   \item \code{area}: the area of reductimorphic plus oximorphic features
#'         (\%) that the Stagnosols and Planosols keys weigh: 100 for a claric
#'         layer of criteria 4-5 and for a mottled layer whose matrix is grey
#'         (moist value 4 or more, chroma 2 or less: the matrix is the
#'         reductimorphic part), the mottles otherwise (100 where unknown);
#'   \item \code{reducing}: reducing conditions as \code{test_reducing_conditions}
#'         reads them.
#' }
#' Continuous rock (R) has no gleyic or stagnic properties ("refer to mineral
#' material").
#' @noRd
.wrb_redox_layers <- function(pedon, min_redox_pct = 5) {
  h <- pedon$horizons
  n <- nrow(h)
  col <- function(v, d = NA) if (is.null(h[[v]])) rep(d, n) else h[[v]]
  desg <- as.character(col("designation", NA_character_))
  rock <- !is.na(desg) & grepl(.WRB_ROCK_DESIGNATION, desg)
  rx <- suppressWarnings(as.numeric(col("redoximorphic_features_pct", NA_real_)))
  red <- .reductimorphic_colour(col("munsell_hue_moist"), col("munsell_chroma_moist"))
  oxi <- rx > min_redox_pct
  clr <- .subtest_by_layer(test_claric_munsell(h), n)
  # the layer directly under each layer (its top within 1 cm of the bottom)
  below <- vapply(seq_len(n), function(i) {
    j <- which(!is.na(h$top_cm) & !is.na(h$bottom_cm[i]) &
                 abs(h$top_cm - h$bottom_cm[i]) < 1 & seq_len(n) != i)
    if (length(j)) j[1] else NA_integer_
  }, 0L)
  above <- vapply(seq_len(n), function(i) {
    j <- which(!is.na(h$bottom_cm) & !is.na(h$top_cm[i]) &
                 abs(h$bottom_cm - h$top_cm[i]) < 1 & seq_len(n) != i)
    if (length(j)) j[1] else NA_integer_
  }, 0L)
  at <- function(x, j) ifelse(is.na(j), NA, x[j])
  claric95 <- .and3v(clr, !oxi)              # claric in 95% or more of the layer
  # water: the site's groundwater; a claric layer directly above is the
  # stagnic sequence (3.2.14 criterion 5), not groundwater
  gw <- .groundwater_within_150(pedon$site %||% list())
  water <- rep(gw, n)
  water[is.na(water) & at(claric95, above) %in% TRUE] <- FALSE
  # Hugo's reading of criteria 1-3 (file header): a reductimorphic matrix
  # (with up to 5% oximorphic features criterion 1 itself; with more, the
  # matrix of a gleyic criterion-2 layer), or oximorphic features directly
  # on a layer with such a matrix (criterion 3), or with groundwater
  # Hugo (2026-10-29): criterion 1 asks >= 95% reductimorphic features, so a
  # layer with more than 5% oximorphic features does not meet it by its
  # matrix; mottles not recorded do not rule it out (the matrix colour is
  # then the colour of the layer). Criterion 3: oximorphic features on a
  # layer that meets criterion 1.
  crit1 <- .and3v(red, !(oxi %in% TRUE))
  red[rock] <- FALSE
  crit1[rock] <- FALSE
  on_red <- at(crit1, below)
  gleyic <- .any3v(crit1, .and3v(oxi, .any3v(on_red, water)))
  gleyic[rock] <- FALSE
  stag_ox <- .and3v(oxi, !gleyic)
  stag_ox[rock] <- FALSE
  # criterion 4: claric above an abrupt textural difference or a dense layer
  atd <- abrupt_textural_difference(pedon)
  over_atd <- rep(FALSE, n)
  for (p in atd$evidence$layer_pairs %||% list()) {
    if (isTRUE(p$passed)) over_atd[p$between[1]] <- TRUE
    else if (is.na(p$passed) && isFALSE(over_atd[p$between[1]])) over_atd[p$between[1]] <- NA
  }
  bd_below <- at(suppressWarnings(as.numeric(col("bulk_density_g_cm3", NA_real_))), below)
  crit4 <- .and3v(claric95, .any3v(over_atd, bd_below >= 1.5))
  crit5 <- .and3v(claric95, at(stag_ox, below))
  stagnic <- .any3v(stag_ox, .any3v(crit4, crit5))
  stagnic[rock] <- FALSE
  grey <- !is.na(col("munsell_value_moist")) & !is.na(col("munsell_chroma_moist")) &
    col("munsell_value_moist") >= 4 & col("munsell_chroma_moist") <= 2
  area <- ifelse(.any3v(crit4, crit5) %in% TRUE | grey, 100, ifelse(is.na(rx), 100, pmin(100, rx)))
  red_res <- test_reducing_conditions(h, min_redox_pct = min_redox_pct)
  list(reductimorphic = red, oximorphic = oxi, claric = clr, crit1 = crit1, gleyic = gleyic,
       stagnic = stagnic, stagnic_oximorphic = stag_ox, area = area,
       reducing = .subtest_by_layer(red_res, n), groundwater = gw, rock = rock)
}
