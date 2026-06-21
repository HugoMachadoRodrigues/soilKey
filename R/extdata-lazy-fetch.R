# =============================================================================
# v0.9.94 -- Lazy-fetch architecture for the four large bundled caches.
#
# To bring the source tarball under CRAN's preferred 5 MB ceiling (the
# v0.9.93 build was ~10 MB, dominated by four ~1 MB .rds samples), the
# four caches below are NO LONGER bundled in `inst/extdata/`. Instead,
# each loader looks for the file in three places, in order:
#
#   1. Bundled `system.file("extdata", "<name>.rds", package = "soilKey")`
#      -- preserved for back-compat with developer checkouts that still
#      have the file in `inst/extdata/` (e.g.\ this repo's `data-raw/`
#      lazy-fetch staging area), and for users who copy the .rds from
#      a release tarball.
#   2. User cache at `tools::R_user_dir("soilKey", "data")` -- populated
#      on first call by an explicit download invocation (see step 3).
#   3. On-demand download from a versioned GitHub Release attachment.
#      The download path requires either an interactive session (the
#      function prompts) OR an explicit call to
#      `download_extdata_cache(<name>)`.
#
# The bundled-canonical-fixture caches under `inst/extdata/` (one ~62 KB
# .rds per WRB Reference Soil Group, total ~1.6 MB) ARE still bundled --
# they're integral to the test suite and to the deterministic key.
# =============================================================================


#' Caches managed by the v0.9.94 lazy-fetch system
#' @keywords internal
.SOILKEY_LAZY_FETCH_CACHES <- c(
  "afsp_sample",
  "kssl_sample",
  "kssl_nasis_sample",
  "wosis_stratified_sample"
)


#' Versioned GitHub Release tag where the lazy-fetch caches are pinned
#' @keywords internal
.SOILKEY_LAZY_FETCH_RELEASE <- "v0.9.94-data"


#' Build the GitHub Release download URL for a lazy-fetch cache
#' @noRd
.lazy_fetch_url <- function(name, release = .SOILKEY_LAZY_FETCH_RELEASE) {
  sprintf(
    "https://github.com/HugoMachadoRodrigues/soilKey/releases/download/%s/%s.rds",
    release, name
  )
}


#' Resolve the local path of a v0.9.94 lazy-fetch cache file
#'
#' Internal helper used by every lazy-fetch loader (\code{load_kssl_sample},
#' \code{load_kssl_nasis_sample}, \code{load_afsp_sample},
#' \code{load_wosis_stratified_sample}). Looks in three places:
#' \enumerate{
#'   \item Bundled \code{inst/extdata/<name>.rds} (back-compat for
#'         developer checkouts and pre-v0.9.94 install paths).
#'   \item User cache at \code{tools::R_user_dir("soilKey", "data")}.
#'   \item Returns \code{NULL} if neither exists -- the caller then
#'         decides whether to prompt the user for an on-demand download.
#' }
#'
#' @param name Base name without \code{.rds} extension. Must be one of
#'        \code{.SOILKEY_LAZY_FETCH_CACHES}.
#' @return Character path to a readable .rds file, or \code{NULL} if
#'         the cache is not yet present locally.
#' @noRd
.lazy_fetch_local_path <- function(name) {
  stopifnot(name %in% .SOILKEY_LAZY_FETCH_CACHES)
  # 1. system.file() resolves the file in BOTH installed packages and
  #    pkgload::load_all() development sessions. Under load_all the
  #    file IS in the source tree (inst/extdata/) and resolves; under
  #    a CRAN-installed package the file is absent because
  #    `^inst/extdata/<name>\\.rds$` is in `.Rbuildignore` (so the
  #    file doesn't ship in the source tarball -- v0.9.94 Rbuildignore
  #    rules added to bring the tarball under 5 MB).
  bundled <- system.file("extdata", paste0(name, ".rds"),
                            package = "soilKey")
  if (nzchar(bundled) && file.exists(bundled)) return(bundled)
  # 2. User cache populated by `download_extdata_cache()`.
  cache_dir  <- tools::R_user_dir("soilKey", which = "data")
  cache_file <- file.path(cache_dir, paste0(name, ".rds"))
  if (file.exists(cache_file)) return(cache_file)
  NULL
}


#' Download one or more soilKey lazy-fetch caches from GitHub Release
#'
#' soilKey ships four large benchmark caches (KSSL, KSSL+NASIS, AfSP,
#' WoSIS stratified) that are too large to embed in the CRAN source
#' tarball. Since v0.9.94 they are pinned to a versioned GitHub Release
#' and downloaded on demand into the user cache directory at
#' \code{tools::R_user_dir("soilKey", "data")}.
#'
#' On first call to any of \code{load_kssl_sample()},
#' \code{load_kssl_nasis_sample()}, \code{load_afsp_sample()}, or
#' \code{load_wosis_stratified_sample()}, soilKey checks for the file
#' in the user cache. If missing, the loader prompts (interactive
#' sessions only) to download. Use \code{download_extdata_cache()}
#' to eagerly populate the cache without prompting.
#'
#' @param which Character vector of cache names to download.
#'        \code{"all"} (default) downloads every lazy-fetch cache. Valid
#'        names: \code{"afsp_sample"}, \code{"kssl_sample"},
#'        \code{"kssl_nasis_sample"}, \code{"wosis_stratified_sample"}.
#' @param release GitHub Release tag to pull from (default
#'        \code{"v0.9.94-data"}). Override only if you maintain a
#'        local mirror.
#' @param overwrite If \code{TRUE}, redownload even if the file is
#'        already present in the user cache (default \code{FALSE}).
#' @param verbose Print progress (default \code{TRUE}).
#'
#' @return Invisibly, a named character vector of local paths to the
#'         downloaded files.
#'
#' @examples
#' \dontrun{
#' # Download every lazy-fetch cache once, ahead of any benchmark run:
#' download_extdata_cache()
#'
#' # Or just the WRB AfSP sample:
#' download_extdata_cache("afsp_sample")
#' }
#' @export
download_extdata_cache <- function(which     = "all",
                                      release   = .SOILKEY_LAZY_FETCH_RELEASE,
                                      overwrite = FALSE,
                                      verbose   = TRUE) {
  which <- if (identical(which, "all")) .SOILKEY_LAZY_FETCH_CACHES
            else match.arg(which, .SOILKEY_LAZY_FETCH_CACHES,
                              several.ok = TRUE)
  cache_dir <- tools::R_user_dir("soilKey", which = "data")
  if (!dir.exists(cache_dir))
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  out <- character(length(which))
  names(out) <- which
  for (i in seq_along(which)) {
    name        <- which[i]
    target      <- file.path(cache_dir, paste0(name, ".rds"))
    if (!isTRUE(overwrite) && file.exists(target)) {
      if (isTRUE(verbose))
        message(sprintf("[soilKey] cache '%s' already present at %s",
                          name, target))
      out[i] <- target
      next
    }
    url <- .lazy_fetch_url(name, release)
    if (isTRUE(verbose))
      message(sprintf("[soilKey] downloading '%s' from %s", name, url))
    rc <- tryCatch(utils::download.file(url, target, mode = "wb",
                                          quiet = !verbose),
                    error = function(e) {
                      stop(sprintf(
                        "Failed to download '%s' from %s: %s",
                        name, url, conditionMessage(e)
                      ), call. = FALSE)
                    })
    if (rc != 0L)
      stop(sprintf("download.file() returned non-zero status %d for '%s'",
                   rc, name), call. = FALSE)
    out[i] <- target
  }
  invisible(out)
}


#' Read a lazy-fetch cache, downloading on first call if needed
#'
#' Internal entry point used by every lazy-fetch loader. Encapsulates
#' the three-step resolution (bundled / user-cache / on-demand
#' download with interactive prompt).
#'
#' @noRd
.lazy_fetch_readRDS <- function(name) {
  path <- .lazy_fetch_local_path(name)
  if (!is.null(path)) return(readRDS(path))
  if (!interactive()) {
    stop(sprintf(paste0(
      "soilKey cache '%s' not found locally. v0.9.94+ moves the four ",
      "large benchmark caches to lazy fetch from GitHub Release. ",
      "Run soilKey::download_extdata_cache(\"%s\") in an interactive ",
      "R session first."), name, name), call. = FALSE)
  }
  msg <- sprintf(paste0(
    "soilKey: the '%s' cache is not present in your install.\n",
    "It will be downloaded (~1 MB) from GitHub Release %s into\n",
    "  %s\n",
    "Proceed?"),
    name, .SOILKEY_LAZY_FETCH_RELEASE,
    tools::R_user_dir("soilKey", "data"))
  ans <- tryCatch(utils::askYesNo(msg, default = TRUE),
                   error = function(e) NA)
  if (!isTRUE(ans))
    stop(sprintf("Download declined; cannot load '%s'.", name),
         call. = FALSE)
  paths <- download_extdata_cache(name, verbose = FALSE)
  readRDS(paths[name])
}
