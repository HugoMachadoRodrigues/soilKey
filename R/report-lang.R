# =============================================================================
# v0.9.117 -- bilingual report labels. report()/report_html()/report_pdf() take
# a `lang` argument ("en" default, "pt" for Brazilian Portuguese); they set the
# process option `soilKey.report_lang` for the duration of the render (restored
# on exit) and the .html_* / .build_report_rmd helpers look their fixed labels
# up through .report_msg(). The English catalogue holds the pre-i18n labels
# VERBATIM, and "en" is the default, so a default-language report is
# byte-identical to before. Taxonomic nomenclature and horizon column headers
# are data, not labels, and are never translated.
#
# The label catalogue lives in inst/i18n/report_translations.yaml (a data file)
# so the Portuguese text stays out of the package's R sources (keeping them
# ASCII-only, as CRAN requires).
# =============================================================================

.report_i18n_env <- new.env(parent = emptyenv())

# Load + cache the report label catalogue (graceful empty fallback if absent).
.report_catalog <- function() {
  if (is.null(.report_i18n_env$cat)) {
    path <- system.file("i18n", "report_translations.yaml", package = "soilKey")
    if (!nzchar(path) || !file.exists(path))
      path <- file.path("inst", "i18n", "report_translations.yaml")  # dev tree
    .report_i18n_env$cat <-
      if (file.exists(path)) yaml::read_yaml(path)
      else list(en = list(), pt = list())
  }
  .report_i18n_env$cat
}

# Current report language, clamped to a supported value.
.report_lang <- function() {
  lang <- getOption("soilKey.report_lang", "en")
  if (length(lang) != 1L || !lang %in% c("en", "pt")) "en" else lang
}

#' Look up a fixed report label in the current report language.
#'
#' Falls back to English, then to the key itself. \code{...} are passed to
#' \code{sprintf} when present (for labels with placeholders).
#' @noRd
.report_msg <- function(key, ...) {
  lang <- .report_lang()
  cat  <- .report_catalog()
  val  <- cat[[lang]][[key]]
  if (is.null(val)) val <- cat[["en"]][[key]]
  if (is.null(val)) return(key)
  dots <- list(...)
  if (length(dots)) val <- do.call(sprintf, c(list(val), dots))
  val
}
