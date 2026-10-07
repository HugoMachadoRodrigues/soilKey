# =============================================================================
# soilKey Pro -- internationalization (i18n) helper (v0.9.114).
#
# Dependency-free string translation for the app. Translatable strings live in
# inst/i18n/translations.yaml (an `en:` and a `pt:` section keyed by the same
# semantic keys), loaded once and cached. i18n("key") returns the string for
# the current app language, falling back to English and then to the key itself.
# Dynamic strings pass sprintf args:
#   i18n("pedon.loaded_n", nrow(df))   # en: "Loaded 5 horizon(s)"
#
# Language is per session (v0.9.210). The page asks for it in its URL
# (?lang=pt); without that it is the app default, the option `soilKey.app_lang`
# set at launch by run_classify_app(lang=). The navbar selector rewrites the URL
# and reloads, so ui() rebuilds in the new language. Until v0.9.210 the selector
# flipped the process-wide option instead, and on the hosted app one visitor's
# choice became every other visitor's language at their next page load. The
# English catalog holds the pre-i18n strings VERBATIM, so the default ("en")
# renders byte-identically to the pre-i18n app -- the regression anchor.
# =============================================================================

.sk_i18n_env <- new.env(parent = emptyenv())

# Current app language, clamped to a supported value: the page being built
# (.sk_with_lang()), else the session's own choice, else the app default.
.sk_app_lang <- function() {
  lang <- .sk_i18n_env$ui_lang
  if (is.null(lang) && requireNamespace("shiny", quietly = TRUE)) {
    s <- shiny::getDefaultReactiveDomain()
    if (!is.null(s)) lang <- tryCatch(s$userData$sk_lang, error = function(e) NULL)
  }
  if (is.null(lang)) lang <- getOption("soilKey.app_lang", "en")
  if (length(lang) != 1L || !lang %in% c("en", "pt")) "en" else lang
}

# The language a URL query asks for ("?lang=pt"), or NULL.
.sk_lang_from_query <- function(query) {
  q <- tryCatch(shiny::parseQueryString(if (is.null(query)) "" else query),
                error = function(e) list())
  l <- q$lang
  if (length(l) == 1L && l %in% c("en", "pt")) l else NULL
}

# A session's language: its URL's, else the app default.
.sk_lang_for <- function(query) {
  .sk_lang_from_query(query) %||% getOption("soilKey.app_lang", "en")
}

# Evaluate `expr` (building a page) in language `lang`.
.sk_with_lang <- function(lang, expr) {
  old <- .sk_i18n_env$ui_lang
  .sk_i18n_env$ui_lang <- lang
  on.exit(.sk_i18n_env$ui_lang <- old, add = TRUE)
  force(expr)
}

# Load + cache the YAML catalog (graceful empty fallback if absent).
.sk_i18n_catalog <- function() {
  if (is.null(.sk_i18n_env$cat)) {
    path <- system.file("i18n", "translations.yaml", package = "soilKey")
    if (!nzchar(path) || !file.exists(path))
      path <- file.path("inst", "i18n", "translations.yaml")  # dev checkout
    .sk_i18n_env$cat <-
      if (file.exists(path)) yaml::read_yaml(path)
      else list(en = list(), pt = list())
  }
  .sk_i18n_env$cat
}

# Translate `key` to the current (or given) language.
#   i18n("nav.pedon")                      -> "Pedon" / "Perfil"
#   i18n("pedon.loaded_n", nrow(df))       -> sprintf the matched template
# Falls back: requested lang -> English -> the key itself (so a missing key is
# visible rather than crashing). `...` are passed to sprintf when present.
i18n <- function(key, ..., lang = NULL) {
  if (is.null(lang)) lang <- .sk_app_lang()
  cat <- .sk_i18n_catalog()
  val <- cat[[lang]][[key]]
  if (is.null(val)) val <- cat[["en"]][[key]]
  if (is.null(val)) return(key)
  dots <- list(...)
  if (length(dots)) val <- do.call(sprintf, c(list(val), dots))
  val
}
