# =============================================================================
# v0.9.43 -- JSON Schema for PedonRecord.
#
# Generates a Draft-2020-12 JSON Schema from horizon_column_spec() so external
# systems (web APIs, ETL pipelines, multimodal extraction validation) can
# validate input data BEFORE it reaches the classifier. Schema is also
# written to inst/schemas/pedon-schema.json for direct file access.
# =============================================================================


#' JSON Schema for a soilKey PedonRecord
#'
#' Returns a Draft-2020-12 JSON Schema describing the canonical
#' \code{PedonRecord} structure: a \code{site} object with site-level
#' metadata plus a \code{horizons} array where each element matches
#' the canonical horizon schema documented by
#' \code{\link{horizon_column_spec}}.
#'
#' @param as One of \code{"list"} (default; returns a structured R list
#'        ready to serialise) or \code{"json"} (returns a JSON string;
#'        requires the \code{jsonlite} package).
#' @param pretty Logical, only used for \code{as = "json"}.
#' @return A list (default) or a JSON string.
#' @examples
#' \dontrun{
#' schema <- pedon_json_schema()
#' names(schema)
#' #> [1] "$schema"     "$id"         "title"       "type"        "required"   "properties"
#'
#' # Validate a JSON profile against the schema:
#' if (requireNamespace("jsonvalidate", quietly = TRUE)) {
#'   schema_json <- pedon_json_schema(as = "json")
#'   jsonvalidate::json_validate('{"site":{...},"horizons":[...]}',
#'                                  schema_json, engine = "ajv")
#' }
#' }
#' @export
pedon_json_schema <- function(as = c("list", "json"), pretty = TRUE) {
  as <- match.arg(as)

  spec <- horizon_column_spec()

  # Map soilKey type strings to JSON Schema types.
  json_type <- function(rtype) {
    switch(rtype,
             numeric    = list(type = c("number", "null")),
             integer    = list(type = c("integer", "null")),
             logical    = list(type = c("boolean", "null")),
             character  = list(type = c("string", "null")),
             list(type = c("string", "null")))
  }

  # Build per-column property dictionary.
  hzn_props <- list()
  for (col in names(spec)) {
    hzn_props[[col]] <- json_type(spec[[col]])
  }

  schema <- list(
    "$schema"     = "https://json-schema.org/draft/2020-12/schema",
    "$id"         = "https://hugomachadorodrigues.github.io/soilKey/schemas/pedon-schema.json",
    title         = "soilKey PedonRecord schema",
    description   = paste("Canonical structure of a single soilKey PedonRecord.",
                            "Use this schema to validate JSON-serialised soil",
                            "profile data before passing to classify_wrb2022 /",
                            "classify_sibcs / classify_usda."),
    type          = "object",
    required      = list("site", "horizons"),
    properties    = list(
      site = list(
        type        = "object",
        description = "Site-level metadata (one record per pedon).",
        required    = list("id"),
        properties  = list(
          id              = list(type = "string"),
          lat             = list(type = c("number", "null"),
                                    minimum = -90, maximum = 90),
          lon             = list(type = c("number", "null"),
                                    minimum = -180, maximum = 180),
          country         = list(type = c("string", "null")),
          parent_material = list(type = c("string", "null")),
          date            = list(type = c("string", "null"),
                                    description = "ISO 8601 date or NULL"),
          reference_wrb         = list(type = c("string", "null")),
          reference_sibcs       = list(type = c("string", "null")),
          reference_usda        = list(type = c("string", "null")),
          reference_usda_subgroup = list(type = c("string", "null")),
          reference_usda_grtgroup = list(type = c("string", "null")),
          reference_usda_suborder = list(type = c("string", "null")),
          nasis_diagnostic_features = list(
            type = c("array", "null"),
            items = list(type = "string"),
            description = "NASIS pediagfeatures.featkind vector (one per surveyor-flagged diagnostic)"
          ),
          reference_source = list(type = c("string", "null"))
        ),
        additionalProperties = TRUE
      ),
      horizons = list(
        type        = "array",
        description = "Ordered list of horizons, top to bottom.",
        minItems    = 0,
        items       = list(
          type                 = "object",
          required             = list("top_cm", "bottom_cm"),
          properties           = hzn_props,
          additionalProperties = FALSE
        )
      ),
      provenance = list(
        type        = c("array", "null"),
        description = "Per-attribute provenance log; one row per measurement.",
        items       = list(
          type       = "object",
          required   = list("attribute", "source"),
          properties = list(
            attribute  = list(type = "string"),
            value      = list(type = c("number", "string", "null")),
            source     = list(
              type = "string",
              enum = list("measured", "predicted_spectra",
                            "extracted_vlm", "inferred_prior",
                            "user_assumed")
            ),
            confidence = list(type = c("number", "null"),
                                 minimum = 0, maximum = 1),
            notes      = list(type = c("string", "null"))
          )
        )
      )
    ),
    additionalProperties = TRUE
  )

  if (as == "json") {
    if (!requireNamespace("jsonlite", quietly = TRUE))
      stop("Package 'jsonlite' is required for as='json'.")
    return(jsonlite::toJSON(schema, auto_unbox = TRUE, pretty = pretty,
                              null = "null"))
  }
  schema
}


#' Validate a PedonRecord against the JSON schema
#'
#' Convenience wrapper that converts a \code{\link{PedonRecord}} (or a
#' compatible list) to JSON and validates it via
#' \code{jsonvalidate::json_validate} against the canonical schema
#' returned by \code{\link{pedon_json_schema}}.
#'
#' Use this BEFORE calling \code{classify_*} when ingesting data from
#' external systems (web APIs, ETL pipelines, multimodal extraction)
#' to catch schema violations early.
#'
#' @param x A \code{\link{PedonRecord}} or a list with the same shape.
#' @return A logical scalar (\code{TRUE} when valid). Validation errors
#'         appear as the \code{errors} attribute when \code{FALSE}.
#' @examples
#' \dontrun{
#' p <- make_ferralsol_canonical()
#' validate_pedon_json(p)
#' #> [1] TRUE
#' }
#' @export
validate_pedon_json <- function(x) {
  if (!requireNamespace("jsonlite", quietly = TRUE))
    stop("Package 'jsonlite' is required.")
  if (!requireNamespace("jsonvalidate", quietly = TRUE))
    stop("Package 'jsonvalidate' is required.")

  # Build a list that matches the schema shape.
  if (inherits(x, "PedonRecord")) {
    payload <- list(
      site     = x$site,
      horizons = as.list(as.data.frame(x$horizons))
    )
    # Convert horizons from columnar list to row list.
    n <- nrow(x$horizons)
    if (n > 0L) {
      horizon_rows <- vector("list", n)
      for (i in seq_len(n)) {
        horizon_rows[[i]] <- as.list(x$horizons[i, ])
      }
      payload$horizons <- horizon_rows
    } else {
      payload$horizons <- list()
    }
  } else {
    payload <- x
  }

  json_payload <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
  schema_json  <- pedon_json_schema(as = "json", pretty = FALSE)

  jsonvalidate::json_validate(json_payload, schema_json,
                                  engine = "ajv", verbose = TRUE)
}


#' Internal helper: write the schema to inst/schemas/pedon-schema.json
#' Called by data-raw / build scripts only -- not exported.
#' @keywords internal
.write_pedon_schema_to_disk <- function(
    path = file.path("inst", "schemas", "pedon-schema.json")
) {
  if (!requireNamespace("jsonlite", quietly = TRUE))
    stop("Package 'jsonlite' is required.")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  schema <- pedon_json_schema(as = "list")
  json   <- jsonlite::toJSON(schema, auto_unbox = TRUE, pretty = TRUE,
                                null = "null")
  writeLines(json, path)
  invisible(path)
}
