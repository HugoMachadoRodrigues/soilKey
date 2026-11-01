# =============================================================================
# v0.9.325 -- the Comprehensive Soil Classification System of Japan, First
# Approximation (CSCJ 2011; Obara et al., NIAES Bulletin 29), keyed to the
# ten great groups of chapter V and the 27 groups of chapter VI (0.9.326).
# The subgroups, soil series groups and phases follow. The gates it shares with the SCSJ 2017 read the pedon by
# the CSCJ edition (site$jp_edition). Experimental and opt-in.
# =============================================================================

#' Classify a pedon under the Comprehensive Soil Classification System of Japan
#'
#' Keys a \code{\link{PedonRecord}} through the Comprehensive Soil
#' Classification System of Japan, First Approximation (Obara et al. 2011),
#' following its key to great groups. \strong{Experimental}: this release
#' keys the ten great groups (Man-made soils, Organic soils, Podzols,
#' Andosols, Dark Red soils, Lowland soils, Red-Yellow soils, Stagnic soils,
#' Brown Forest soils, Regosols) and their 27 groups (for instance Fulvic and
#' Allophanic Andosols, Calcaric Dark Red soils, Brown Lowland soils); the
#' 116 subgroups, 381 soil series groups and the phases follow. Each level gives the English name
#' of the text's appendix 2 and, in \code{levels$name_native}, the Japanese
#' one.
#'
#' The CSCJ shares most of its diagnostics with the Soil Classification
#' System of Japan (2017), and soilKey reads them with the same code (see
#' \code{\link{classify_scsj2017}} for the attributes read), by the CSCJ's
#' own limits where it has them: the argic horizon (the WRB 2006 clay
#' increase), the immature kuroboku feature (P retention as its only
#' substitute), and the dark red colour of the Dark Red soils, which also
#' read the origin from limestone or calcareous deposits (\code{rock_origin},
#' \code{layer_origin}, the site \code{parent_material}, \code{caco3_pct}).
#' The choices where the text is silent are listed in
#' \code{system.file("rules", "cscj2011", "readings.yaml", package =
#' "soilKey")}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rules Optional pre-loaded rule set (\code{load_rules("cscj2011")}).
#' @param on_missing One of \code{"warn"} (default), \code{"silent"},
#'        \code{"error"}.
#' @param gapfill Opt-in within-pedon gap-fill, as in
#'        \code{\link{classify_usda}}.
#' @return A \code{\link{ClassificationResult}} with \code{system_id}
#'   \code{"cscj2011"}, the deepest class in \code{name}, the great group
#'   in \code{rsg_or_order}, the classes of each level in \code{levels} (with
#'   \code{name_native}), the key trace and \code{provisional_because}
#'   (classes before the assigned one that could not be decided).
#' @references Obara, H., Ohkura, T., Takata, Y., Kohyama, K., Maejima, Y.
#'   and Hamazaki, T. (2011). Comprehensive Soil Classification System of
#'   Japan, First Approximation. Bulletin of the National Institute for
#'   Agro-Environmental Sciences 29: 1-73.
#' @seealso \code{\link{make_cscj2011_example}}, \code{\link{classify_scsj2017}},
#'   \code{\link{soilkey_systems}}.
#' @examples
#' res <- classify_cscj2011(make_cscj2011_example("dark_red"), on_missing = "silent")
#' res$name
#' res$levels
#' @export
classify_cscj2011 <- function(pedon, rules = NULL,
                              on_missing = c("warn", "silent", "error"),
                              gapfill = FALSE) {
  on_missing <- match.arg(on_missing)
  if (inherits(pedon, "PedonRecord")) pedon <- .cscj_mark(.cssc_prepare_pedon(pedon))
  rules <- rules %||% load_rules("cscj2011")
  res <- classify_hierarchical("cscj2011", pedon, rules = rules,
                               on_missing = on_missing, gapfill = gapfill)
  if (inherits(pedon, "PedonRecord") &&
      all(is.na(pedon$horizons$designation %||% NA) | !nzchar(trimws(pedon$horizons$designation %||% "")))) {
    res$warnings <- c(res$warnings, paste(
      "No horizon designations: the CSCJ reads several horizons from the notation",
      "(H peat, G gley, Bs/Bh podzolic, Bt clay, Ap plough layer), which are then read from properties alone."))
  }
  res
}

# A copy of the pedon marked to be read by the CSCJ edition.
.cscj_mark <- function(pedon) {
  p <- pedon$clone(deep = TRUE)
  site <- p$site %||% list()
  site$jp_edition <- "cscj2011"
  p$site <- site
  p
}

#' Example pedons for the CSCJ great groups
#'
#' Small, plausible profiles written for the tests and examples, one per
#' great group of the Comprehensive Soil Classification System of Japan
#' (2011), with designations in the Japanese notation. Most are the
#' \code{\link{make_scsj2017_example}} profiles of the great group of the
#' same concept; the Dark Red soil (from limestone) is the CSCJ's own. They
#' are not real profiles.
#'
#' @param great_group One of \code{"man_made"}, \code{"organic"},
#'   \code{"podzol"}, \code{"andosol"}, \code{"dark_red"},
#'   \code{"lowland"}, \code{"red_yellow"}, \code{"stagnic"},
#'   \code{"brown_forest"}, \code{"regosol"}.
#' @return A \code{\link{PedonRecord}}.
#' @examples
#' p <- make_cscj2011_example("dark_red")
#' p$horizons[, c("designation", "top_cm", "bottom_cm")]
#' @export
make_cscj2011_example <- function(great_group = c("man_made", "organic", "podzol", "andosol", "dark_red",
                                                  "lowland", "red_yellow", "stagnic", "brown_forest",
                                                  "regosol")) {
  great_group <- match.arg(great_group)
  p <- switch(great_group,
    lowland = make_scsj2017_example("fluvic"),
    dark_red = {
      h <- list(designation = c("A", "Bw1", "Bw2", "C", "R"), top_cm = c(0, 12, 40, 70, 95),
                bottom_cm = c(12, 40, 70, 95, 120), oc_pct = c(3.5, 1.2, 0.6, 0.3, NA),
                phosphate_absorption_coef = c(900, 800, 700, 500, NA),
                munsell_hue_moist = c("7.5YR", "5YR", "5YR", "5YR", NA), munsell_value_moist = c(3, 3, 3, 4, NA),
                munsell_chroma_moist = c(2, 4, 4, 6, NA), clay_pct = c(35, 45, 48, 40, NA),
                field_texture = c("clay loam", "clay", "clay", "clay", NA),
                structure_grade = c("moderate", "strong", "moderate", "weak", NA),
                ph_h2o = c(6.8, 7.1, 7.3, 7.6, NA), bs_pct = c(85, 95, 98, 100, NA),
                caco3_pct = c(0, 0, 1, 5, NA), layer_origin = "weathered limestone",
                rock_origin = c(NA, NA, NA, NA, "Ryukyu limestone"), redoximorphic_features_pct = c(0, 0, 0, 0, NA))
      n <- max(lengths(h))
      h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
      PedonRecord$new(site = list(id = "cscj2011-dark_red", country = "JP", designation_system = "scsj",
                                  drainage_class = "well drained", land_use = "sugarcane field",
                                  parent_material = "limestone"),
                      horizons = ensure_horizon_schema(data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
    },
    make_scsj2017_example(great_group))
  site <- p$site; site$id <- paste0("cscj2011-", great_group); p$site <- site
  p
}
