# =============================================================================
# v0.9.62 -- Canonical USDA KST 13th edition reference (JSON, vendored).
#
# Vendored from ncss-tech/SoilKnowledgeBase (BSD-style USDA license):
#
#   inst/rules/usda/canonical/2022_KST_codes.json    (~196 KB)
#       3,153-row data.frame: {code, name} mapping each taxon code
#       (e.g. "A", "AB", "AAA", ...) to its English name (e.g.
#       "Gelisols", "Histels", "Folistels"). Hierarchical: single-
#       letter codes are Orders; double-letter are Suborders;
#       three-letter are Great Groups; four-letter are Subgroups.
#
#   inst/rules/usda/canonical/2022_KST_criteria_EN.json  (~3.1 MB)
#       3,153-element nested list keyed by code. Each element has:
#         $content : data.frame with the parsed clause text (English),
#                    chapter/page references, and clause/logic flags.
#
# Why JSON not RDA: the JSON is canonical-format from SKB, parseable
# by any language (Python, Julia, etc.) -- supports the future
# polyglot soilKey ecosystem (Python soilKey, Julia integration).
# =============================================================================


#' Resolve the path to a vendored canonical KST 13th JSON file
#' @keywords internal
.kst13_path <- function(filename) {
  p <- system.file("rules", "usda", "canonical", filename,
                     package = "soilKey")
  if (!nzchar(p) || !file.exists(p)) {
    cand <- file.path("inst", "rules", "usda", "canonical", filename)
    if (file.exists(cand)) p <- cand
  }
  if (!nzchar(p) || !file.exists(p)) {
    stop(sprintf("Vendored KST file not found: %s", filename),
         call. = FALSE)
  }
  p
}


#' Package-level cache for the parsed KST 13ed JSON files
#'
#' v0.9.65 (Copilot review #5): \code{kst13_criteria()} previously
#' parsed the full ~3.1 MB criteria JSON on every call. Looping over
#' a few hundred codes was crippling. This cache loads each JSON
#' once per session.
#'
#' Kept in a private environment so package-internal code can reach
#' the cached objects via \code{.KST13_CACHE$<filename>} but external
#' callers must go through \code{\link{kst13_codes}} /
#' \code{\link{kst13_criteria}}.
#'
#' @keywords internal
.KST13_CACHE <- new.env(parent = emptyenv())


#' Read + cache a KST13 JSON file
#' @keywords internal
.kst13_load_cached <- function(filename) {
  if (!exists(filename, envir = .KST13_CACHE, inherits = FALSE)) {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop(".kst13_load_cached(): the 'jsonlite' package is required.",
           call. = FALSE)
    }
    parsed <- jsonlite::fromJSON(.kst13_path(filename))
    assign(filename, parsed, envir = .KST13_CACHE)
  }
  get(filename, envir = .KST13_CACHE, inherits = FALSE)
}


#' Clear the in-memory KST13 cache
#'
#' Useful when the vendored JSON files are updated mid-session.
#' Frees ~3.1 MB.
#' @export
clear_kst13_cache <- function() {
  rm(list = ls(envir = .KST13_CACHE, all.names = TRUE),
     envir = .KST13_CACHE)
  invisible(NULL)
}


#' Load the canonical KST 13ed code -> taxon-name lookup table
#'
#' Returns the 3,153-row data.frame from
#' \code{inst/rules/usda/canonical/2022_KST_codes.json}, vendored from
#' NCSS-tech/SoilKnowledgeBase. Each row is a (code, name) pair.
#'
#' Code structure:
#' \itemize{
#'   \item Single letter (\code{"A"}-\code{"L"}): Soil Order
#'         (Gelisols, Histosols, ..., Entisols)
#'   \item Two letters (\code{"AB"}, \code{"AC"}, ...): Suborder
#'   \item Three letters: Great Group
#'   \item Four letters: Subgroup
#' }
#'
#' @return A data.frame with columns \code{code, name}.
#' @seealso \code{\link{kst13_criteria}}, \code{\link{kst13_canonical}}.
#' @export
kst13_codes <- function() {
  .kst13_load_cached("2022_KST_codes.json")
}


#' Load the canonical KST 13ed criteria for a single taxon code
#'
#' Returns the parsed clause data.frame for one code (e.g. \code{"A"}
#' for Gelisols, \code{"ABA"} for Histels.Folistels, etc.). Each row
#' is one clause of the diagnostic text with \code{content},
#' \code{chapter}, \code{page} columns.
#'
#' For the full 3,153-element nested list (all codes), use
#' \code{\link{kst13_canonical}} (which loads the SoilTaxonomy R-package
#' RDA equivalent).
#'
#' @param code Character. Taxon code in the KST 13ed code system
#'        (e.g. \code{"A"} for Gelisols, \code{"ABCDA"} for the
#'        Lithic Folistels subgroup).
#' @return A data.frame with the parsed clauses for that code, or
#'   \code{NULL} if the code is not present.
#' @seealso \code{\link{kst13_codes}}, \code{\link{kst13_canonical}}.
#' @export
kst13_criteria <- function(code) {
  if (length(code) != 1L || !is.character(code) || !nzchar(code)) {
    stop("kst13_criteria(): `code` must be a single non-empty string.",
         call. = FALSE)
  }
  # v0.9.65 (Copilot review #5): cached so loop callers don't re-parse
  # the 3.1 MB JSON on every invocation.
  blob <- .kst13_load_cached("2022_KST_criteria_EN.json")
  if (!(code %in% names(blob))) return(NULL)
  blob[[code]]$content
}
