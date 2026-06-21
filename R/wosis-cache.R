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
  # v0.9.94: routed through the lazy-fetch helper.
  s <- .lazy_fetch_readRDS("wosis_stratified_sample")
  # v0.9.88: alias `wosis_rsg` -> `reference_wrb` on every pedon so
  # generic benchmark loops that call `p$site$reference_wrb` (the
  # canonical field used by KSSL / AfSP / Redape pedons) work
  # off-the-shelf on the WoSIS bundled cache. The original
  # `wosis_rsg` slot is preserved for back-compat with code that
  # already reads it directly.
  if (is.list(s) && !is.null(s$pedons)) {
    s$pedons <- lapply(s$pedons, function(p) {
      if (!inherits(p, "PedonRecord")) return(p)
      # Strict access via [[]] to bypass R's partial-matching footgun
      # (v0.9.91: $reference_wrb otherwise resolves to other reference_*
      # fields via partial matching, masking the canonical-field gap).
      has_canonical <- !is.null(p$site[["reference_wrb"]]) &&
                         !is.na(p$site[["reference_wrb"]])
      has_wosis     <- !is.null(p$site[["wosis_rsg"]]) &&
                         !is.na(p$site[["wosis_rsg"]])
      if (!has_canonical && has_wosis) {
        p$site[["reference_wrb"]] <- p$site[["wosis_rsg"]]
      }
      p
    })
  }
  s
}


#' Load the bundled KSSL/NCSS lab-data sample (v0.9.74)
#'
#' Returns a 100-profile snapshot from the NCSS Lab Data Mart
#' (KSSL gpkg, \code{head = 100}) pre-annotated with derived WRB
#' Reference Soil Group via \code{\link{usda_to_wrb_rsg}}.
#'
#' This is the bundled offline counterpart to
#' \code{\link{load_kssl_pedons_gpkg}} -- use this for tests and
#' demos when the 5.5 GB gpkg is not available locally.
#'
#' Each pedon has BOTH:
#' \itemize{
#'   \item \code{site$reference_usda} (Order, Suborder, Greatgroup,
#'         Subgroup) -- the canonical KSSL classification.
#'   \item \code{site$reference_wrb_from_usda} -- the derived WRB
#'         RSG via the IUSS WRB 2022 Annex 6 cross-walk.
#' }
#'
#' First-ever KSSL WRB benchmark (soilKey v0.9.74, full v0.9.69-72
#' fallback stack):
#' \itemize{
#'   \item Top-1 accuracy: 20.1\\% (n = 199, head = 200)
#'   \item Calcisol 69\\%, Cambisol 73\\% -- well-handled
#'   \item Phaeozem / Kastanozem / Solonetz 0\\% -- need Munsell + ESP
#'         data not in KSSL lab tables (in companion NASIS).
#' }
#'
#' @return A list with \code{pedons}, \code{pulled_on}, \code{source},
#'   \code{cross_walk}.
#'
#' @section Reference:
#' Beaudette, D., Skovlin, J., Roecker, S., Brown, A. (2024). aqp:
#' Algorithms for Quantitative Pedology. R package version 2.x.
#' \code{https://github.com/ncss-tech/aqp}.
#'
#' @examples
#' \dontrun{
#' s <- load_kssl_sample()
#' length(s$pedons)
#' #> 100
#' table(vapply(s$pedons, function(p) p$site$reference_wrb_from_usda,
#'              character(1)))
#' }
#'
#' @export
load_kssl_sample <- function() {
  # v0.9.94: routed through the lazy-fetch helper.
  s <- .lazy_fetch_readRDS("kssl_sample")
  # v0.9.91: alias `reference_wrb_from_usda` -> `reference_wrb` on every
  # pedon so generic benchmark loops that call `p$site$reference_wrb`
  # (the canonical field used by WoSIS / AfSP / Redape pedons after
  # v0.9.88) work off-the-shelf on KSSL too. The original
  # `reference_wrb_from_usda` slot is preserved for back-compat.
  s$pedons <- .kssl_alias_reference_wrb(s$pedons)
  s
}


#' Alias `reference_wrb_from_usda` -> `reference_wrb` on KSSL pedons
#'
#' Internal helper used by both \code{load_kssl_sample()} and
#' \code{load_kssl_nasis_sample()} since v0.9.91 to populate the
#' canonical \code{reference_wrb} field from the KSSL-specific
#' \code{reference_wrb_from_usda} cross-walk slot. Only sets the
#' field when it is currently NULL, so explicit annotations are
#' preserved.
#' @noRd
.kssl_alias_reference_wrb <- function(pedons) {
  if (!is.list(pedons) || length(pedons) == 0L) return(pedons)
  lapply(pedons, function(p) {
    if (!inherits(p, "PedonRecord")) return(p)
    # Strict access via [[]] to bypass R's partial-matching footgun
    # ($reference_wrb otherwise resolves to reference_wrb_from_usda
    # via partial matching, masking the missing canonical field).
    has_canonical <- !is.null(p$site[["reference_wrb"]]) &&
                       !is.na(p$site[["reference_wrb"]])
    has_xwalk     <- !is.null(p$site[["reference_wrb_from_usda"]]) &&
                       !is.na(p$site[["reference_wrb_from_usda"]])
    if (!has_canonical && has_xwalk) {
      p$site[["reference_wrb"]] <- p$site[["reference_wrb_from_usda"]]
    }
    p
  })
}


#' Load the bundled KSSL + NASIS morphological-enriched sample (v0.9.75)
#'
#' Returns a 99-profile snapshot built by joining the NCSS Lab Data
#' Mart (\code{ncss_labdata.gpkg}) with the companion NASIS
#' Morphological sqlite (\code{NASIS_Morphological_*.sqlite}) via
#' \code{\link{load_kssl_pedons_with_nasis}}. Pre-annotated with
#' derived WRB Reference Soil Group via \code{\link{usda_to_wrb_rsg}}.
#'
#' Compared to \code{\link{load_kssl_sample}} (KSSL lab tables only),
#' this sample carries the morphological evidence that several WRB
#' diagnostic horizons need:
#'
#' | Field | KSSL-only | KSSL + NASIS |
#' |-------|----------:|-------------:|
#' | munsell_hue_moist     | 0% | **89.6%** |
#' | munsell_value_moist   | 0% | **89.6%** |
#' | munsell_chroma_moist  | 0% | **89.6%** |
#' | munsell_hue_dry       | 0% | **65.2%** |
#' | structure_grade       | 0% | **53.8%** |
#' | structure_type        | 0% | **79.2%** |
#' | clay_films_amount     | 0% | 8.2% |
#' | slickensides          | 0% | 1.7% |
#'
#' First-ever benchmark on this enriched sample (soilKey v0.9.75,
#' full v0.9.69-72 fallback stack):
#' \itemize{
#'   \item Top-1 baseline: 19.1\\% (vs 15.6\\% on KSSL-only -- a
#'         **+3.5pp lift purely from NASIS morphology**)
#'   \item Top-1 full stack: 20.6\\% (vs 20.1\\%)
#'   \item Phaeozem: 1/33 -> 2/33 (Munsell-driven mollic detection)
#'   \item Podzol:   0/15 -> 1/15
#' }
#'
#' Remaining ceiling driven by attributes neither dataset preserves:
#' Solonetz needs Na/ESP, Vertisols need slickensides + cracks
#' (NASIS records 1.7% / 0%), Kastanozems need mollic + chroma
#' on subsoil samples NASIS often lacks.
#'
#' @return A list with \code{pedons}, \code{pulled_on}, \code{source},
#'   \code{join_helper}, \code{cross_walk}.
#'
#' @section Reference:
#' Beaudette, D., Skovlin, J., Roecker, S., Brown, A. (2024). aqp:
#' Algorithms for Quantitative Pedology. R package version 2.x.
#' \code{https://github.com/ncss-tech/aqp}.
#'
#' @examples
#' \dontrun{
#' s <- load_kssl_nasis_sample()
#' length(s$pedons)
#' #> 99
#' # Munsell now populated (KSSL-only sample had 0%):
#' mean(vapply(s$pedons,
#'             function(p) any(!is.na(p$horizons$munsell_hue_moist)),
#'             logical(1)))
#' #> 0.99
#' }
#'
#' @export
load_kssl_nasis_sample <- function() {
  # v0.9.94: routed through the lazy-fetch helper.
  s <- .lazy_fetch_readRDS("kssl_nasis_sample")
  # v0.9.91: same reference_wrb aliasing as load_kssl_sample().
  s$pedons <- .kssl_alias_reference_wrb(s$pedons)
  s
}
