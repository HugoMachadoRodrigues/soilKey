# ================================================================
# Module 2 -- JSON schema loading and validation
#
# Schemas live in inst/schemas/ as plain JSON Schema (draft-07) files.
# Validation is performed via the `jsonvalidate` package. We keep the
# schemas as files (not R objects) so that they can be (a) shipped
# with the package, (b) versioned in git, (c) referenced from prompt
# templates by mustache substitution.
# ================================================================


#' Path to a packaged JSON schema file
#'
#' Resolves \code{name} to an absolute file path under the package's
#' \code{inst/schemas/} directory (which becomes \code{schemas/} in
#' the installed package). The \code{.json} extension is added if
#' missing.
#'
#' @param name Schema base name, e.g. \code{"horizon"} or
#'        \code{"site"}. Either with or without the \code{.json}
#'        suffix.
#' @return Absolute file path. Errors if the schema is not found.
#' @keywords internal
schema_path <- function(name) {
  if (!grepl("\\.json$", name)) name <- paste0(name, ".json")
  p <- system.file("schemas", name, package = "soilKey")
  if (!nzchar(p)) {
    # Fallback for during development / before install
    candidate <- file.path("inst", "schemas", name)
    if (file.exists(candidate)) p <- normalizePath(candidate)
  }
  if (!nzchar(p) || !file.exists(p)) {
    rlang::abort(sprintf("JSON schema '%s' not found in inst/schemas/", name))
  }
  p
}


#' Load a packaged JSON schema as a string
#'
#' Reads \code{inst/schemas/<name>.json} and returns its contents as a
#' single character scalar. The JSON is not parsed -- callers either
#' pass the string straight to \code{\link{validate_against_schema}}
#' or substitute it into a prompt template via
#' \code{\link{load_prompt}}.
#'
#' @param name Schema base name, e.g. \code{"horizon"}, \code{"site"}.
#' @return Character scalar containing the schema JSON.
#' @keywords internal
load_schema <- function(name) {
  p <- schema_path(name)
  paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}


#' Validate a JSON string against a packaged schema
#'
#' Thin wrapper around \code{jsonvalidate::json_validate} that
#' resolves a schema by short name (\code{"horizon"}, \code{"site"})
#' and returns a normalized result list with \code{valid} (logical)
#' and \code{errors} (character vector, possibly empty).
#'
#' @param json_string A character scalar holding the JSON document to
#'        validate (e.g. the raw string returned by a VLM call).
#' @param schema_name Short schema name as accepted by
#'        \code{\link{load_schema}}.
#' @param engine Validation engine to use; passed through to
#'        \code{jsonvalidate::json_validate}. Default
#'        \code{"ajv"} supports draft-07.
#' @return A list with elements:
#'   \itemize{
#'     \item \code{valid}: \code{TRUE} / \code{FALSE}.
#'     \item \code{errors}: character vector of validation error
#'           messages (empty if \code{valid}).
#'   }
#' @keywords internal
validate_against_schema <- function(json_string,
                                     schema_name,
                                     engine = "ajv") {

  if (!requireNamespace("jsonvalidate", quietly = TRUE)) {
    rlang::abort(paste0(
      "Package 'jsonvalidate' is required for VLM extraction but is not ",
      "installed. Install it with install.packages('jsonvalidate')."
    ))
  }

  schema <- load_schema(schema_name)

  # jsonvalidate::json_validate(verbose = TRUE) returns FALSE plus an
  # "errors" attribute when the document does not validate.
  ok <- tryCatch(
    jsonvalidate::json_validate(
      json    = json_string,
      schema  = schema,
      verbose = TRUE,
      engine  = engine
    ),
    error = function(e) {
      structure(FALSE,
                errors = data.frame(message = conditionMessage(e),
                                     stringsAsFactors = FALSE))
    }
  )

  errs <- attr(ok, "errors")
  err_msgs <- if (is.null(errs) || (is.data.frame(errs) && nrow(errs) == 0L)) {
    character()
  } else if (is.data.frame(errs)) {
    if ("message" %in% names(errs)) {
      as.character(errs$message)
    } else {
      apply(errs, 1, function(row) paste(row, collapse = "; "))
    }
  } else {
    as.character(errs)
  }

  list(valid = isTRUE(as.logical(ok)), errors = err_msgs)
}
