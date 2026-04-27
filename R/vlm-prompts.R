# ================================================================
# Module 2 -- prompt template loading and variable substitution
#
# Prompts live in inst/prompts/ as Markdown files with mustache-style
# `{varname}` placeholders. Loading them resolves the file, reads it
# as UTF-8, and substitutes any provided variables.
# ================================================================


#' Path to a packaged prompt template
#'
#' @param name Template base name, with or without \code{.md}.
#' @return Absolute file path. Errors if not found.
#' @keywords internal
prompt_path <- function(name) {
  if (!grepl("\\.md$", name)) name <- paste0(name, ".md")
  p <- system.file("prompts", name, package = "soilKey")
  if (!nzchar(p)) {
    candidate <- file.path("inst", "prompts", name)
    if (file.exists(candidate)) p <- normalizePath(candidate)
  }
  if (!nzchar(p) || !file.exists(p)) {
    rlang::abort(sprintf("Prompt template '%s' not found in inst/prompts/", name))
  }
  p
}


#' Load and render a packaged prompt template
#'
#' Reads \code{inst/prompts/<name>.md} as UTF-8 and substitutes
#' \code{\{varname\}} placeholders with values from \code{vars}. The
#' substitution is intentionally simple (literal string replacement,
#' no escaping, no logic) -- the prompt templates are author-curated
#' and the only callers are internal extraction functions.
#'
#' Unknown placeholders (i.e. \code{\{foo\}} appearing in the template
#' without a matching entry in \code{vars}) are left as-is, which
#' makes typos visible at runtime in the rendered prompt.
#'
#' @param name Template base name, e.g. \code{"extract_horizons"}.
#' @param vars Named list of substitution values. Each value is
#'        coerced to character via \code{as.character}.
#' @return Character scalar with the rendered prompt.
#' @keywords internal
load_prompt <- function(name, vars = list()) {
  p <- prompt_path(name)
  raw <- paste(readLines(p, warn = FALSE, encoding = "UTF-8"),
               collapse = "\n")

  if (length(vars) == 0L) return(raw)
  if (is.null(names(vars)) || any(!nzchar(names(vars)))) {
    rlang::abort("`vars` must be a named list")
  }

  rendered <- raw
  for (nm in names(vars)) {
    needle <- paste0("{", nm, "}")
    val    <- as.character(vars[[nm]] %||% "")
    rendered <- gsub(needle, val, rendered, fixed = TRUE)
  }
  rendered
}
