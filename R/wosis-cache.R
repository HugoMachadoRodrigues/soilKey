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


#' Load the bundled WoSIS stratified RSG-balanced sample (v0.9.73)
#'
#' Returns a 130-profile snapshot of WoSIS GraphQL data pulled on
#' 2026-05-09 with **stratified sampling by WRB Reference Soil Group**:
#' 5 profiles per RSG across 26 RSGs (Acrisol, Andosol, Arenosol,
#' Calcisol, Cambisol, Chernozem, Cryosol, Ferralsol, Fluvisol,
#' Gleysol, Gypsisol, Histosol, Kastanozem, Leptosol, Luvisol,
#' Nitisol, Phaeozem, Planosol, Plinthosol, Podzol, Regosol,
#' Solonchak, Solonetz, Stagnosol, Umbrisol, Vertisol).
#'
#' This is the recommended cache for global WRB benchmarking. Compared
#' to \code{load_wosis_sample()} (40 SA-only profiles, mostly Solonetz
#' and Phaeozem from Argentina), the stratified sample provides:
#'
#' \itemize{
#'   \item Even coverage across the 26 most important RSGs.
#'   \item Richer analytical attributes -- CEC available on 26%,
#'         ECEC on 37%, BS on 14%, CaCO3 on 26% (vs ~0% for those
#'         in the SA snapshot).
#'   \item Geographic diversity (Angola, Brazil, USA, China, Russia,
#'         South Africa, Indonesia, Argentina, etc.).
#' }
#'
#' First-ever benchmark on this sample (soilKey v0.9.73, full v0.9.69-72
#' fallback stack):
#' \itemize{
#'   \item Overall top-1 accuracy: 16.2\\% (n = 130)
#'   \item Histosol 100\\%, Arenosol 80\\%, **Leptosol 80\\%** (lifted
#'         from 20\\% baseline by v0.9.66 leptic rock-evidence gate),
#'         Cambisol 60\\%, Calcisol 40\\%
#'   \item 18 RSGs at 0\\% recall -- limited by data WoSIS does not
#'         expose (Munsell colours, base saturation, sodium for
#'         Solonetz, slickensides for Vertisols, etc.). Documented
#'         data ceiling.
#' }
#'
#' For the live API, call \code{run_wosis_benchmark_graphql()} or
#' the \code{read_wosis_profiles_graphql(wrb_rsg = "...", n_max = N)}
#' helper (small RSG-filtered queries are tractable; large unfiltered
#' pulls time out as of 2026-05).
#'
#' @return A list with:
#' \itemize{
#'   \item \code{pedons}: list of 130 \code{PedonRecord} objects.
#'   \item \code{meta}: named integer vector of profiles per RSG.
#'   \item \code{pulled_on}: pull date.
#'   \item \code{endpoint}: ISRIC GraphQL endpoint URL.
#'   \item \code{filter}: pull strategy metadata.
#'   \item \code{n_pulled}: 130.
#' }
#'
#' @section Reference:
#'   Batjes, N. H., Ribeiro, E., van Oostrum, A. (2020). Standardised
#'   soil profile data to support global mapping and modelling
#'   (WoSIS snapshot 2019). \emph{Earth System Science Data}, 12,
#'   299-320. \doi{10.5194/essd-12-299-2020}.
#'
#' @examples
#' \dontrun{
#' s <- load_wosis_stratified_sample()
#' length(s$pedons)
#' #> 130
#' table(vapply(s$pedons, function(p) p$site$wosis_rsg, character(1)))
#' #> 5 of each: Acrisol, Andosol, ... Vertisol
#' }
#'
#' @export
load_wosis_stratified_sample <- function() {
  path <- system.file("extdata", "wosis_stratified_sample.rds",
                        package = "soilKey")
  if (!nzchar(path) || !file.exists(path)) {
    dev_path <- file.path("inst", "extdata", "wosis_stratified_sample.rds")
    if (file.exists(dev_path)) path <- dev_path
  }
  if (!nzchar(path) || !file.exists(path))
    stop("Bundled WoSIS stratified sample not found at ",
          "inst/extdata/wosis_stratified_sample.rds.")
  readRDS(path)
}
