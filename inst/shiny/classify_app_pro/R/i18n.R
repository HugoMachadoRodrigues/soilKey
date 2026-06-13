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
# Language is a process-level option (`soilKey.app_lang`, default "en") set at
# launch by run_classify_app(lang=) and flipped by the navbar selector, which
# also calls session$reload() so the per-session ui() rebuilds in the new
# language. The English catalog holds the pre-i18n strings VERBATIM, so the
# default ("en") renders byte-identically to the pre-i18n app -- the regression
# anchor. (The option is process-global, which is correct for the local
# single-user `run_classify_app()` workflow; a multi-user Shiny Server would
# prefer a per-session store.)
# =============================================================================

.sk_i18n_env <- new.env(parent = emptyenv())

# Current app language, clamped to a supported value.
.sk_app_lang <- function() {
  lang <- getOption("soilKey.app_lang", "en")
  if (length(lang) != 1L || !lang %in% c("en", "pt")) "en" else lang
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
