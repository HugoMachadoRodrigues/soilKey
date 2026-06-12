# =============================================================================
# v0.9.62 -- Canonical reference data from NCSS-tech / Andrew Brown's work.
#
# Vendored from the `SoilTaxonomy` R package
# (https://github.com/ncss-tech/SoilTaxonomy):
#
#   inst/extdata/canonical/WRB_4th_2022.rda    (~8 KB)
#       3-element list of parsed IUSS WRB 2022 criteria:
#         $rsg : 118 obs (RSG name + criteria text per clause)
#         $pq  : 661 obs (principal qualifiers per RSG)
#         $sq  : 1167 obs (supplementary qualifiers per RSG)
#
#   inst/extdata/canonical/ST_criteria_13th.rda  (~104 KB)
#       3,153-element nested list of parsed Keys to Soil Taxonomy
#       13th edition (USDA-NRCS, 2022). Each key is a (chapter, page,
#       key, taxon, code, clause, logic) row of the Key text.
#
#   inst/extdata/canonical/ST_features.rda       (~29 KB)
#       84-row data.frame of canonical USDA Soil Taxonomy diagnostic
#       features (group / name / chapter / page / description / criteria).
#
# Why vendored: offline-first. soilKey works without SoilTaxonomy
# installed; if the user has it, the helpers below can read directly
# from the installed package for the freshest copy.
# =============================================================================


#' Load a canonical reference dataset from soilKey or SoilTaxonomy
#'
#' Resolution order:
#' \enumerate{
#'   \item If the \code{SoilTaxonomy} package is installed AND the
#'         \code{prefer_pkg} argument is \code{TRUE} (default), load
#'         the dataset from the installed package (always fresh).
#'   \item Otherwise, load from the vendored copy at
#'         \code{inst/extdata/canonical/<name>.rda}.
#' }
#'
#' @param name One of \code{"WRB_4th_2022"}, \code{"ST_criteria_13th"},
#'        \code{"ST_features"}.
#' @param prefer_pkg If \code{TRUE} (default), prefer the installed
#'        SoilTaxonomy package over the vendored copy. Set to
#'        \code{FALSE} to force the vendored copy (e.g. for
#'        reproducibility of a specific soilKey release).
#' @return The dataset as the original R object (list or data.frame).
#' @seealso \code{\link{wrb2022_canonical}}, \code{\link{kst13_canonical}},
#'   \code{\link{st_features_canonical}}.
#' @export
canonical_reference <- function(name = c("WRB_4th_2022",
                                            "ST_criteria_13th",
                                            "ST_features"),
                                   prefer_pkg = TRUE) {
  name <- match.arg(name)
  if (isTRUE(prefer_pkg) &&
        requireNamespace("SoilTaxonomy", quietly = TRUE)) {
    # v0.9.65 (Copilot review #4): wrap the SoilTaxonomy data() call
    # in tryCatch and verify the dataset materialises in the env. If
    # the installed SoilTaxonomy version drops or renames the dataset,
    # we silently fall through to the vendored copy below instead of
    # erroring out.
    e <- new.env(parent = emptyenv())
    pkg_loaded <- tryCatch({
      utils::data(list = name, package = "SoilTaxonomy", envir = e)
      exists(name, envir = e, inherits = FALSE)
    }, error = function(err) {
      warning(sprintf(
        "canonical_reference(): SoilTaxonomy::%s lookup failed (%s); ",
        name, conditionMessage(err)),
        "falling back to vendored copy.", call. = FALSE)
      FALSE
    })
    if (isTRUE(pkg_loaded)) return(get(name, envir = e))
  }
  rda_path <- system.file("extdata", "canonical",
                            paste0(name, ".rda"),
                            package = "soilKey")
  if (!nzchar(rda_path) || !file.exists(rda_path)) {
    # During devtools/pkgload usage the system.file() path may not
    # resolve. Fall back to the project tree.
    cand <- file.path("inst", "extdata", "canonical",
                       paste0(name, ".rda"))
    if (file.exists(cand)) rda_path <- cand
  }
  if (!nzchar(rda_path) || !file.exists(rda_path)) {
    stop(sprintf("canonical_reference(): cannot locate %s.rda. ",
                 name),
         "Install SoilTaxonomy (install.packages('SoilTaxonomy', ",
         "repos = c(NCSS = 'https://ncss-tech.r-universe.dev'))) ",
         "or check the soilKey installation.",
         call. = FALSE)
  }
  e <- new.env(parent = emptyenv())
  load(rda_path, envir = e)
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
#' @return The canonical WRB 2022 reference data (a list / data.frame of RSG and qualifier criteria), as vendored or sourced from the \pkg{SoilTaxonomy} package.
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
