# =============================================================================
# v0.9.62 -- Canonical USDA KST 13th edition reference (JSON).
# v0.9.204 -- fetched from its source on first use, never shipped.
#
# The two files come from ncss-tech/SoilKnowledgeBase, whose DESCRIPTION
# declares License: GPL-3. (Earlier versions of this header called it a
# "BSD-style USDA license"; that was wrong.)
#
#   2022_KST_codes.json         (~196 KB)
#       3,153-row data.frame: {code, name} mapping each taxon code
#       (e.g. "A", "AB", "AAA", ...) to its English name (e.g.
#       "Gelisols", "Histels", "Folistels"). Hierarchical: single-
#       letter codes are Orders; double-letter are Suborders;
#       three-letter are Great Groups; four-letter are Subgroups.
#
#   2022_KST_criteria_EN.json   (~3.1 MB)
#       3,153-element nested list keyed by code. Each element has:
#         $content : data.frame with the parsed clause text (English),
#                    chapter/page references, and clause/logic flags.
#
# Why no copy is shipped: soilKey is MIT, and redistributing GPL-3 material
# inside an MIT package grants rights the GPL withholds. Fetching the files from
# their source on the user's machine is not redistribution. The download is
# pinned to one SoilKnowledgeBase commit and verified by MD5, and the pinned
# files are byte-identical to the copies soilKey vendored until v0.9.203, so
# every result is unchanged. Only these lookup helpers and coverage_report()
# read them; the classification keys never do, so classifying works offline.
# =============================================================================

# Pinned source. Change all three together, and only after re-verifying.
.KST13_SOURCE_COMMIT <- "9e78a75114ef4c67bab7fa6bb1248790e83d22ca"
.KST13_SOURCE_URL    <- paste0(
  "https://raw.githubusercontent.com/ncss-tech/SoilKnowledgeBase/",
  .KST13_SOURCE_COMMIT, "/inst/extdata/KST/")
.KST13_MD5 <- c(
  "2022_KST_codes.json"       = "3aed3ce28bf5b94710d110ad460613f2",
  "2022_KST_criteria_EN.json" = "a7ab462702417ecf733c2a270c8e6a98")


# The one network call, kept separate so tests can simulate a failure without
# touching the network.
#' @noRd
.kst13_download <- function(url, dest) {
  utils::download.file(url, dest, mode = "wb", quiet = TRUE)
}


#' Resolve a KST 13th JSON file, fetching it from its source if needed
#'
#' Looks in, in order: a directory named by options(soilKey.kst13_dir), for
#' offline use with files obtained separately; the user cache; and finally the
#' pinned SoilKnowledgeBase commit, downloading into the user cache. A file is
#' used only if its MD5 matches the pin, so a truncated download or a changed
#' upstream file is refused instead of silently altering results.
#' @noRd
.kst13_path <- function(filename) {
  want <- unname(.KST13_MD5[filename])
  if (is.na(want)) stop(sprintf("Unknown KST file: %s", filename), call. = FALSE)
  good <- function(p) nzchar(p) && file.exists(p) &&
    identical(unname(tools::md5sum(p)), want)

  local_dir <- getOption("soilKey.kst13_dir", default = "")
  if (nzchar(local_dir) && good(file.path(local_dir, filename)))
    return(file.path(local_dir, filename))

  cache_dir <- file.path(tools::R_user_dir("soilKey", which = "cache"), "kst13")
  cached <- file.path(cache_dir, filename)
  if (good(cached)) return(cached)

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  op <- options(timeout = max(120, getOption("timeout")))
  on.exit(options(op), add = TRUE)
  ok <- tryCatch({
    .kst13_download(paste0(.KST13_SOURCE_URL, filename), tmp)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!isTRUE(ok) || !good(tmp)) {
    stop(sprintf("Could not obtain %s. ", filename),
         "soilKey does not ship this file: it comes from ncss-tech/",
         "SoilKnowledgeBase (GPL-3) and is downloaded on first use, pinned to ",
         "commit ", substr(.KST13_SOURCE_COMMIT, 1, 7), " and checked by MD5. ",
         "Connect to the internet and retry, or set options(soilKey.kst13_dir = ",
         "<folder>) to a folder holding the file.",
         call. = FALSE)
  }
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  if (file.copy(tmp, cached, overwrite = TRUE)) return(cached)
  # The cache is not writable. Keep the verified file for this session only:
  # returning `tmp` would hand back a path that on.exit() deletes before the
  # caller can read it.
  session_copy <- file.path(tempdir(), filename)
  file.copy(tmp, session_copy, overwrite = TRUE)
  session_copy
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
#' @noRd
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
#' Useful after pointing \code{options(soilKey.kst13_dir)} at other copies
#' of the files mid-session.
#' Frees ~3.1 MB.
#' @return \code{NULL}, invisibly. Called for its side effect of emptying the KST 13th-edition lookup cache.
#' @export
clear_kst13_cache <- function() {
  rm(list = ls(envir = .KST13_CACHE, all.names = TRUE),
     envir = .KST13_CACHE)
  invisible(NULL)
}


#' Load the canonical KST 13ed code -> taxon-name lookup table
#'
#' Returns the 3,153-row data.frame from
#' \code{2022_KST_codes.json} in NCSS-tech/SoilKnowledgeBase (GPL-3), which
#' soilKey downloads on first use and caches (it ships no copy; see
#' \code{\link{clear_kst13_cache}}). Each row is a (code, name) pair.
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
