# =============================================================================
# v0.9.30 -- Bundled WoSIS sample for offline tests + reproducible benchmarks.
#
# The ISRIC WoSIS GraphQL endpoint is intermittently unstable (the
# v0.9.27 retry path documented this, see canceling-statement-timeouts
# observed at offset >= 40-50 profiles). To allow tests + CI + casual
# users to exercise the WRB benchmark path without depending on server
# availability, we bundle a 40-profile South-America snapshot pulled
# on 2026-05-03 as inst/extdata/wosis_sa_sample.rds.
# =============================================================================


#' Load the bundled WoSIS South-America sample
#'
#' Returns a 40-profile snapshot of WoSIS GraphQL data pulled on
#' 2026-05-03 with \code{continent = "South America"}. The data is a
#' frozen artefact -- do NOT use it for current paper-grade
#' benchmarks (the WoSIS database is updated periodically; the bundled
#' snapshot is for reproducible tests and offline development only).
#'
#' For up-to-date benchmarks, call \code{run_wosis_benchmark_graphql()}
#' directly against the live ISRIC GraphQL endpoint.
#'
#' @section Returned data:
#' A list with elements:
#' \itemize{
#'   \item \code{profiles_raw} -- the parsed GraphQL response (one
#'         element per profile; nested layer arrays).
#'   \item \code{pedons} -- \code{PedonRecord} objects ready for
#'         classification (one per profile).
#'   \item \code{pulled_on} -- \code{Date} of the snapshot.
#'   \item \code{endpoint}, \code{filter}, \code{n_pulled} -- metadata.
#' }
#'
#' @return A list as described above.
#' @examples
#' \dontrun{
#' sample <- load_wosis_sample()
#' length(sample$pedons)
#' #> 40
#' classify_wrb2022(sample$pedons[[1]])$rsg_or_order
#' }
#' @export
load_wosis_sample <- function() {
  path <- system.file("extdata", "wosis_sa_sample.rds", package = "soilKey")
  if (!nzchar(path) || !file.exists(path)) {
    # In a development checkout (load_all), system.file may return "".
    # Fall back to the in-tree path.
    dev_path <- file.path("inst", "extdata", "wosis_sa_sample.rds")
    if (file.exists(dev_path)) path <- dev_path
  }
  if (!nzchar(path) || !file.exists(path))
    stop("Bundled WoSIS sample not found at inst/extdata/wosis_sa_sample.rds.")
  readRDS(path)
}
