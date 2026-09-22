# =============================================================================
# v0.9.62 -- Canonical reference data from NCSS-tech / Andrew Brown's work.
# v0.9.204 -- read from the installed SoilTaxonomy package, never shipped.
#
# The data come from the `SoilTaxonomy` R package
# (https://github.com/ncss-tech/SoilTaxonomy, on CRAN), which is GPL-3:
#
#   WRB_4th_2022       3-element list of parsed IUSS WRB 2022 criteria:
#                        $rsg : 118 obs (RSG name + criteria text per clause)
#                        $pq  : 661 obs (principal qualifiers per RSG)
#                        $sq  : 1167 obs (supplementary qualifiers per RSG)
#   ST_criteria_13th   3,153-element nested list of parsed Keys to Soil
#                      Taxonomy 13th edition (USDA-NRCS, 2022)
#   ST_features        84-row data.frame of USDA Soil Taxonomy diagnostic
#                      features (group / name / chapter / page / criteria)
#
# Why no copy is shipped: until v0.9.203 soilKey vendored all three under
# inst/extdata/canonical/. soilKey is MIT, and redistributing GPL-3 material
# inside an MIT package grants rights the GPL withholds. Using an installed GPL
# package is not redistributing it, so soilKey now loads the data from
# SoilTaxonomy (Suggests) and ships nothing of it. The vendored copies were
# byte-identical to SoilTaxonomy 0.2.8 on CRAN, checked with identical() before
# they were removed, so no result changes.
# =============================================================================


# Kept separate so tests can exercise the "not installed" path without
# uninstalling anything.
#' @noRd
.has_soiltaxonomy <- function() requireNamespace("SoilTaxonomy", quietly = TRUE)


#' Load a canonical reference dataset from the SoilTaxonomy package
#'
#' Reads one of the parsed WRB 2022 / Keys to Soil Taxonomy datasets from the
#' installed \pkg{SoilTaxonomy} package (NCSS-tech, GPL-3). soilKey ships no
#' copy of these data: install SoilTaxonomy from CRAN to use this function.
#'
#' @param name One of \code{"WRB_4th_2022"}, \code{"ST_criteria_13th"},
#'        \code{"ST_features"}.
#' @param prefer_pkg Retained for compatibility and no longer used. Until
#'        v0.9.203 \code{FALSE} selected a copy bundled with soilKey; there is
#'        no bundled copy any more, so the data always come from SoilTaxonomy.
#' @return The dataset as the original R object (list or data.frame).
#' @seealso \code{\link{wrb2022_canonical}}, \code{\link{kst13_canonical}},
#'   \code{\link{st_features_canonical}}.
#' @export
canonical_reference <- function(name = c("WRB_4th_2022",
                                            "ST_criteria_13th",
                                            "ST_features"),
                                   prefer_pkg = TRUE) {
  name <- match.arg(name)
  if (!.has_soiltaxonomy()) {
    stop(sprintf("canonical_reference(\"%s\") needs the SoilTaxonomy package. ", name),
         "soilKey does not ship a copy of its data (GPL-3, NCSS-tech). ",
         "Install it with install.packages(\"SoilTaxonomy\").",
         call. = FALSE)
  }
  e <- new.env(parent = emptyenv())
  ok <- tryCatch({
    utils::data(list = name, package = "SoilTaxonomy", envir = e)
    exists(name, envir = e, inherits = FALSE)
  }, error = function(err) FALSE, warning = function(w) FALSE)
  if (!isTRUE(ok)) {
    stop(sprintf("canonical_reference(): the installed SoilTaxonomy (%s) ",
                 as.character(utils::packageVersion("SoilTaxonomy"))),
         sprintf("does not provide the dataset %s. ", name),
         "soilKey was validated against SoilTaxonomy 0.2.8.",
         call. = FALSE)
  }
  get(name, envir = e)
}


#' WRB 2022 canonical reference (parsed IUSS Working Group WRB 2022)
#'
#' Convenience wrapper for \code{canonical_reference("WRB_4th_2022")}.
#' Returns a 3-element list:
#' \itemize{
#'   \item \code{$rsg} (118 obs): Reference Soil Group + criteria text
#'   \item \code{$pq}  (661 obs): principal qualifiers per RSG
#'   \item \code{$sq}  (1167 obs): supplementary qualifiers per RSG
#' }
#'
#' Source: NCSS-tech \code{SoilTaxonomy} R package. Original: IUSS
#' Working Group WRB (2022). \emph{World Reference Base for Soil
#' Resources}, 4th edition.
#'
#' @inheritParams canonical_reference
#' @return The canonical WRB 2022 reference data (a list / data.frame of RSG and qualifier criteria), read from the \pkg{SoilTaxonomy} package.
#' @export
wrb2022_canonical <- function(prefer_pkg = TRUE) {
  canonical_reference("WRB_4th_2022", prefer_pkg = prefer_pkg)
}


#' Keys to Soil Taxonomy 13th edition canonical reference
#'
#' Convenience wrapper for \code{canonical_reference("ST_criteria_13th")}.
#' Returns a nested list of 3,153 parsed Keys-to-Soil-Taxonomy clauses
#' per chapter / page / key / taxon / code / clause / logic.
#'
#' Source: NCSS-tech \code{SoilTaxonomy} R package. Original:
#' \href{https://www.nrcs.usda.gov/sites/default/files/2022-09/Keys-to-Soil-Taxonomy.pdf}{USDA-NRCS (2022). \emph{Keys to Soil Taxonomy}, 13th edition.}
#'
#' @inheritParams canonical_reference
#' @return The canonical \emph{Keys to Soil Taxonomy} (13th ed.) criteria reference (a list / data.frame).
#' @export
kst13_canonical <- function(prefer_pkg = TRUE) {
  canonical_reference("ST_criteria_13th", prefer_pkg = prefer_pkg)
}


#' USDA Soil Taxonomy diagnostic features canonical table
#'
#' Convenience wrapper for \code{canonical_reference("ST_features")}.
#' Returns an 84-row data.frame with one row per diagnostic feature
#' (epipedon / subsurface horizon / property / material) and columns:
#' \code{group, name, chapter, page, description, criteria}. The
#' \code{criteria} column is a list-column; each element holds the
#' parsed criteria text per feature.
#'
#' @inheritParams canonical_reference
#' @return The canonical Soil Taxonomy diagnostic-features reference (a list / data.frame).
#' @export
st_features_canonical <- function(prefer_pkg = TRUE) {
  canonical_reference("ST_features", prefer_pkg = prefer_pkg)
}
