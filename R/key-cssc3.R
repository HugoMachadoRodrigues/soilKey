# =============================================================================
# v0.9.267 -- the Canadian System of Soil Classification, 3rd edition: the
# first national system on the registry, keyed to the ten orders (Ch 3,
# pp. 33-34); v0.9.268 adds the 31 great groups (pp. 35-36), v0.9.269 the
# subgroups of the Cryosolic, Organic and Vertisolic orders (pp. 37-40) and
# v0.9.270 those of the Podzolic, Gleysolic, Brunisolic and Regosolic orders
# (pp. 40-51). The other subgroups follow. The system is experimental and
# opt-in: classify_all() runs it only when asked.
# =============================================================================


# The pedon as the Canadian key reads it: rows in depth order (by top, a
# stable sort), and a horizon with no bottom depth ending where the next
# horizon down begins (the deepest one keeps its missing bottom). Works on a
# copy, made only when something changes, so the caller's pedon is never
# altered; the layer numbers of the trace refer to the rows in depth order.
.cssc_prepare_pedon <- function(pedon) {
  h <- pedon$horizons
  if (is.null(h) || nrow(h) == 0L) return(pedon)
  ord <- order(h$top_cm, na.last = TRUE, method = "radix")
  unsorted <- !identical(ord, seq_len(nrow(h)))
  hs <- h
  if (unsorted) {
    hs <- data.table::copy(h)
    data.table::setorderv(hs, "top_cm", na.last = TRUE)
  }
  miss <- which(is.na(hs$bottom_cm) & !is.na(hs$top_cm))
  fill <- vapply(miss, function(i) {
    below <- hs$top_cm[!is.na(hs$top_cm) & hs$top_cm > hs$top_cm[i]]
    if (length(below)) min(below) else NA_real_
  }, numeric(1))
  if (!unsorted && all(is.na(fill))) return(pedon)
  p <- pedon$clone(deep = TRUE)
  h2 <- data.table::copy(hs)
  if (any(!is.na(fill))) {
    data.table::set(h2, j = "bottom_cm", value = as.numeric(h2$bottom_cm))
    data.table::set(h2, i = miss[!is.na(fill)], j = "bottom_cm",
                    value = as.numeric(fill[!is.na(fill)]))
  }
  p$horizons <- h2
  p
}

# Routes of the tests that passed for a taxon, read from its trace entry.
.passing_routes <- function(entry) {
  routes <- character(0)
  walk <- function(ev) {
    for (t in ev) {
      if (!is.list(t)) next
      if (isTRUE(t$passed) && is.list(t$evidence) && !is.null(t$evidence$route))
        routes <<- c(routes, t$evidence$route)
      if ((t$test_name %||% "") %in% c("all_of", "any_of", "none_of"))
        walk(t$evidence)
    }
  }
  walk(entry$evidence)
  unique(routes)
}


#' Classify a pedon under the Canadian System of Soil Classification
#'
#' Keys a \code{\link{PedonRecord}} through the Canadian System of Soil
#' Classification, 3rd edition (Soil Classification Working Group 1998).
#' \strong{Experimental}: this release keys the ten orders (Key to Soil
#' Orders, Ch 3, pp. 33-34), their 31 great groups (Key to Soil Great Groups,
#' pp. 35-36) and their 231 subgroups (Key to Soil Subgroups, pp. 37-51).
#' The name is the deepest class assigned: the subgroup (e.g. "Terric
#' Mesisol"), or the great group (e.g. "Solonetz") when no subgroup can be
#' given: the Solonetzic subgroups are named by the colour of the A, and the
#' key has no other subgroup for a Solonetzic soil without an A (one eroded
#' down to its solonetzic B); the order is in \code{rsg_or_order} and every
#' level in \code{levels}.
#'
#' The Canadian keys are written largely in terms of horizon designations,
#' which the book defines partly by measurable properties. Each diagnostic
#' reads the measured properties first (podzolic B chemistry with
#' pyrophosphate-extractable Fe and Al, the Bt clay increase, the solonetzic
#' B Ca/Na ratio, the chernozemic A, the gley colours) and the surveyor's
#' designations in the Canadian notation second (Bt, Bf, Bm, Btj, Bn, Bv,
#' Bss, Bg, Cz...). When the assigned order rests on designations alone, a
#' warning says so. A profile that declares the FAO notation in
#' \code{site$designation_system} (\code{"fao2006"}, as
#' \code{\link{isis_profile_to_pedon}} sets it, \code{"fao"}, \code{"wrb"} or
#' \code{"wrb2022"}) is read through a translation of its designations into
#' the Canadian notation where the letters mean the same (the book's Table 6:
#' E = Ae, Bw = Bm, Bs = Bf, Btn = Bnt, f = z, z = s ...); the letters with no
#' Canadian equivalent are not read, and the evidence lists what each
#' designation was read as. The choices where the book is silent are listed in
#' \code{system.file("rules", "cssc3", "readings.yaml", package = "soilKey")}.
#'
#' The chernozemic A horizon includes a soil-climate criterion (mean annual
#' soil temperature at least 0 C, moisture subclass drier than humid), read
#' from \code{site$mean_annual_soil_temp_C} and
#' \code{site$soil_moisture_subclass}, or approximated from the USDA regimes.
#' Without it the Chernozemic order is undecided and any order keyed after it
#' is reported as provisional (\code{$provisional_because}). The Gray and
#' Dark Gray subgroups of the Solods and Solodized Solonetz also ask for a
#' subhumid climate (\code{site$soil_moisture_subclass}).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rules Optional pre-loaded rule set (\code{load_rules("cssc3")}).
#' @param on_missing One of \code{"warn"} (default), \code{"silent"},
#'        \code{"error"}.
#' @param gapfill Opt-in within-pedon gap-fill, as in
#'        \code{\link{classify_usda}}.
#' @return A \code{\link{ClassificationResult}} with \code{system_id}
#'   \code{"cssc3"}, the deepest class assigned in \code{name}, the order in
#'   \code{rsg_or_order}, every level in \code{levels}, the key trace, and
#'   \code{provisional_because}.
#' @references Soil Classification Working Group (1998). The Canadian System
#'   of Soil Classification, 3rd ed. Agriculture and Agri-Food Canada
#'   Publication 1646. NRC Research Press, Ottawa.
#' @seealso \code{\link{make_cssc3_example}}, \code{\link{soilkey_systems}}.
#' @examples
#' res <- classify_cssc3(make_cssc3_example("podzolic"), on_missing = "silent")
#' res$name
#' @export
classify_cssc3 <- function(pedon, rules = NULL,
                           on_missing = c("warn", "silent", "error"),
                           gapfill = FALSE) {
  on_missing <- match.arg(on_missing)
  if (inherits(pedon, "PedonRecord")) pedon <- .cssc_prepare_pedon(pedon)
  res <- classify_hierarchical("cssc3", pedon, rules = rules,
                               on_missing = on_missing, gapfill = gapfill)
  if (nrow(res$levels) > 0L) {
    entry <- res$trace$orders[[res$levels$code[1L]]]
    routes <- .passing_routes(entry)
    if (length(routes) > 0L && all(routes == "designations")) {
      res$warnings <- c(res$warnings, paste(
        "The order rests on the surveyor's horizon designations, not on",
        "measured properties; see $trace."))
    }
  }
  res
}


#' Example pedons for the Canadian orders
#'
#' Small, plausible profiles written for the tests and examples, one per
#' order of the Canadian System of Soil Classification, with designations in
#' the Canadian notation and the properties the order, great-group and
#' subgroup keys read: a Brunisolic Dystric Turbic Cryosol, a Terric Humic
#' Mesisol, an Orthic Vertisol, an Orthic Humo-Ferric Podzol, an Orthic Humic
#' Gleysol, a Dark Brown Solonetz, an Orthic Dark Brown Chernozem, an Orthic
#' Gray Luvisol, an Orthic Eutric Brunisol and an Orthic Regosol. They are not
#' records of real pedons.
#'
#' @param order One of \code{"cryosolic"}, \code{"organic"},
#'   \code{"vertisolic"}, \code{"podzolic"}, \code{"gleysolic"},
#'   \code{"solonetzic"}, \code{"chernozemic"}, \code{"luvisolic"},
#'   \code{"brunisolic"}, \code{"regosolic"}.
#' @return A \code{\link{PedonRecord}}.
#' @examples
#' p <- make_cssc3_example("chernozemic")
#' p$horizons[, c("designation", "top_cm", "bottom_cm", "oc_pct")]
#' @export
make_cssc3_example <- function(order = c("cryosolic", "organic", "vertisolic",
                                         "podzolic", "gleysolic", "solonetzic",
                                         "chernozemic", "luvisolic",
                                         "brunisolic", "regosolic")) {
  order <- match.arg(order)
  hz <- function(...) data.table::as.data.table(data.frame(..., stringsAsFactors = FALSE))
  site <- list(id = paste0("cssc3-example-", order), country = "CA",
               lat = 53.5, lon = -113.5)
  h <- switch(order,
    cryosolic = hz(
      designation = c("Ahy", "Bmy", "Cz"), top_cm = c(0, 8, 35), bottom_cm = c(8, 35, 80),
      munsell_hue_moist = c("10YR", "10YR", "2.5Y"), munsell_value_moist = c(3, 4, 4),
      munsell_chroma_moist = c(2, 3, 2), clay_pct = c(18, 20, 22), oc_pct = c(4, 1.2, 0.8),
      ph_cacl2 = c(5.0, 5.2, 5.6), permafrost_temp_C = c(NA, NA, -1.5)),
    organic = hz(
      designation = c("Of", "Om", "Oh", "C"), top_cm = c(0, 30, 100, 130),
      bottom_cm = c(30, 100, 130, 160), oc_pct = c(46, 42, 38, 0.6),
      fiber_content_rubbed_pct = c(60, 25, 8, NA),
      bulk_density_g_cm3 = c(0.06, 0.12, 0.2, 1.5)),
    vertisolic = hz(
      designation = c("Ah", "Bv", "Bssk", "Ck"), top_cm = c(0, 15, 50, 95),
      bottom_cm = c(15, 50, 95, 130), clay_pct = c(62, 65, 66, 64),
      munsell_hue_moist = c("10YR", "10YR", "2.5Y", "2.5Y"),
      munsell_value_moist = c(3, 3, 4, 4), munsell_chroma_moist = c(2, 2, 2, 2),
      munsell_value_dry = c(4, 4, 5, 5),
      slickensides = c(NA, "few", "many", "common"), oc_pct = c(3, 1.5, 0.8, 0.5)),
    podzolic = hz(
      designation = c("LFH", "Ae", "Bf", "BC", "C"), top_cm = c(0, 5, 12, 35, 60),
      bottom_cm = c(5, 12, 35, 60, 100), oc_pct = c(40, 0.6, 1.5, 0.4, 0.2),
      clay_pct = c(NA, 5, 6, 5, 5), sand_pct = c(NA, 78, 76, 80, 82),
      fe_pyro_pct = c(NA, 0.05, 0.5, 0.1, 0.05), al_pyro_pct = c(NA, 0.04, 0.4, 0.1, 0.05),
      munsell_hue_moist = c(NA, "10YR", "5YR", "7.5YR", "10YR"),
      munsell_value_moist = c(NA, 6, 4, 4, 5), munsell_chroma_moist = c(NA, 2, 6, 4, 3)),
    gleysolic = hz(
      designation = c("Ah", "Bg", "Ckg"), top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 100),
      munsell_hue_moist = c("10YR", "2.5Y", "5Y"), munsell_value_moist = c(2, 5, 5),
      munsell_chroma_moist = c(1, 1, 2), clay_pct = c(20, 22, 20), oc_pct = c(5, 0.8, 0.4)),
    solonetzic = hz(
      designation = c("Ah", "Bnt", "Csk"), top_cm = c(0, 12, 35), bottom_cm = c(12, 35, 80),
      munsell_hue_moist = c("10YR", "10YR", "2.5Y"), munsell_value_moist = c(3, 3, 5),
      munsell_chroma_moist = c(2, 2, 3), munsell_value_dry = c(4, 4, 6),
      munsell_chroma_dry = c(2, 2, 3), clay_pct = c(18, 32, 28),
      ca_cmol = c(12, 8, 10), na_cmol = c(0.5, 4, 6), ph_h2o = c(7.2, 8.4, 8.6),
      structure_type = c("granular", "columnar", "massive"),
      rupture_resistance = c("soft", "very hard", "hard"), oc_pct = c(3, 0.9, 0.4)),
    chernozemic = hz(
      designation = c("Ah", "Bm", "Cca"), top_cm = c(0, 18, 40), bottom_cm = c(18, 40, 90),
      munsell_hue_moist = c("10YR", "10YR", "10YR"), munsell_value_moist = c(2, 3, 5),
      munsell_chroma_moist = c(2, 3, 3), munsell_hue_dry = c("10YR", "10YR", "10YR"),
      munsell_value_dry = c(4, 5, 7), munsell_chroma_dry = c(2, 3, 3),
      oc_pct = c(3, 1, 0.4), n_total_pct = c(0.25, 0.09, NA), bs_pct = c(95, 98, 100),
      ca_cmol = c(18, 16, 20), mg_cmol = c(4, 4, 5), k_cmol = c(1, 0.6, 0.4),
      na_cmol = c(0.1, 0.1, 0.2), clay_pct = c(25, 26, 24),
      structure_type = c("granular", "prismatic", "massive")),
    luvisolic = hz(
      designation = c("LFH", "Ae", "Bt", "Ck"), top_cm = c(0, 4, 20, 60),
      bottom_cm = c(4, 20, 60, 100), oc_pct = c(35, 0.5, 0.4, 0.3),
      clay_pct = c(NA, 12, 28, 26), clay_films_amount = c(NA, "none", "common", NA),
      munsell_hue_moist = c(NA, "10YR", "10YR", "2.5Y"), munsell_value_moist = c(NA, 6, 4, 5),
      munsell_chroma_moist = c(NA, 2, 3, 3)),
    brunisolic = hz(
      designation = c("Ah", "Bm", "C"), top_cm = c(0, 8, 40), bottom_cm = c(8, 40, 90),
      munsell_hue_moist = c("10YR", "7.5YR", "10YR"), munsell_value_moist = c(3, 4, 5),
      munsell_chroma_moist = c(3, 4, 3), clay_pct = c(14, 15, 14), oc_pct = c(3, 0.8, 0.3),
      ph_cacl2 = c(5.6, 5.9, 6.4)),
    regosolic = hz(
      designation = c("Ah", "C"), top_cm = c(0, 6), bottom_cm = c(6, 100),
      munsell_hue_moist = c("10YR", "10YR"), munsell_value_moist = c(3, 5),
      munsell_chroma_moist = c(2, 3), clay_pct = c(10, 9), oc_pct = c(2, 0.3))
  )
  if (order == "chernozemic") {
    site$mean_annual_soil_temp_C <- 4
    site$soil_moisture_subclass <- "subhumid"
  }
  if (order == "cryosolic") { site$lat <- 64.06; site$lon <- -139.43 }
  if (order == "vertisolic") site$drainage_class <- "well drained"
  PedonRecord$new(site = site, horizons = ensure_horizon_schema(h))
}
