#' soilKey: Automated Soil Profile Classification per WRB 2022 and SiBCS
#'
#' soilKey implements deterministic classification keys for the World
#' Reference Base for Soil Resources 2022 (4th edition) and the Brazilian
#' System of Soil Classification (SiBCS, 5th edition). It separates concerns
#' strictly: the taxonomic key is a pure function of structured profile
#' data, while optional modules provide vision-language extraction, spatial
#' priors from SoilGrids, and gap-filling of soil attributes from Vis-NIR or
#' MIR spectra via the Open Soil Spectral Library (OSSL).
#'
#' @section Design principle: never delegate the key.
#' Vision-language models are restricted to schema-validated extraction of
#' soil attributes from unstructured sources (PDFs, photos, field sheets).
#' The taxonomic key itself is always evaluated by deterministic R code
#' driven by versioned YAML rules.
#'
#' @section Core types:
#' \itemize{
#'   \item \code{\link{PedonRecord}} — site, horizons, spectra, images,
#'         documents, and a per-attribute provenance log.
#'   \item \code{\link{DiagnosticResult}} — return type of every diagnostic
#'         function (e.g. \code{\link{argic}}, \code{\link{ferralic}},
#'         \code{\link{mollic}}); always carries the sub-test evidence and
#'         missing-attribute report alongside the boolean.
#'   \item \code{\link{ClassificationResult}} — return type of
#'         \code{\link{classify_wrb2022}}; carries the full key trace,
#'         ambiguities, missing-data hints, and a provenance-aware evidence
#'         grade.
#' }
#'
#' @section Provenance and evidence grade:
#' Every attribute used by the key carries a provenance tag from
#' \code{c("measured", "extracted_vlm", "predicted_spectra",
#' "inferred_prior", "user_assumed")}. The final classification evidence
#' grade is one of \code{c("A", "B", "C", "D")} where A is fully
#' laboratory-measured and unambiguous and D is tentative or multimodal.
#'
#' @section v0.1 scope:
#' v0.1 implements three WRB 2022 horizon diagnostics — argic, ferralic,
#' mollic — and the Ferralsols path of the WRB key end-to-end. The full
#' 32-RSG key, 202 qualifiers, the SiBCS key, and the multimodal extraction,
#' spatial-prior, and OSSL-spectroscopy modules are scheduled for subsequent
#' releases. See \code{ARCHITECTURE.md}.
#'
#' @references
#' IUSS Working Group WRB (2022). \emph{World Reference Base for Soil
#' Resources}, 4th edition. International Union of Soil Sciences, Vienna.
#'
#' Embrapa (2018). \emph{Sistema Brasileiro de Classificação de Solos},
#' 5ª edição. Embrapa Solos, Brasília.
#'
#' Beaudette, D. E., Roudier, P., & O'Geen, A. T. (2013). Algorithms for
#' Quantitative Pedology: A toolkit for soil scientists. \emph{Computers &
#' Geosciences}, 52, 258--268.
#'
#' @keywords internal
#' @importFrom R6 R6Class
#' @importFrom stats aggregate predict rnorm runif setNames weighted.mean
#' @importFrom utils tail
"_PACKAGE"
