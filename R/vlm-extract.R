# ================================================================
# Module 2 -- user-facing extraction functions
#
# These three functions are the public surface of Module 2:
#
#   - extract_horizons_from_pdf(pedon, pdf_path, provider, ...)
#   - extract_munsell_from_photo(pedon, image_path, provider, ...)
#   - extract_site_from_fieldsheet(pedon, image_path, provider, ...)
#
# They share a common contract:
#
#   1. Load source (PDF text or image), schema, and rendered prompt.
#   2. Call validate_or_retry() to get a schema-valid JSON object.
#   3. Walk the parsed object, calling pedon$add_measurement(idx, attr,
#      value, source = "extracted_vlm", confidence, notes) for every
#      extracted attribute. The PedonRecord's authority order ensures
#      `measured` values are never silently overwritten by
#      `extracted_vlm` (unless `overwrite = TRUE`).
#   4. Append a one-line entry to pedon$documents / pedon$images to
#      record the source.
#   5. Return the (mutated) pedon, invisibly, plus optionally the raw
#      response on the result attributes for debugging.
#
# Critical: the VLM never classifies. Every attribute the user sees in
# the pedon after extraction has provenance = "extracted_vlm", and the
# deterministic key (classify_wrb2022) consumes the result.
# ================================================================


# ---- internal helpers -------------------------------------------------------


#' Map a parsed VLM attribute object to a (value, confidence, quote) triple
#'
#' Both schemas wrap most attributes in
#' \code{\{"value": ..., "confidence": ..., "source_quote": "..."\}}.
#' This helper unpacks one such entry, returning \code{NULL} if the
#' field is absent or null (so callers can skip it cleanly).
#'
#' @keywords internal
unpack_vlm_attr <- function(x) {
  if (is.null(x)) return(NULL)
  if (!is.list(x)) return(NULL)
  if (is.null(x$value)) return(NULL)
  list(
    value      = x$value,
    confidence = as.numeric(x$confidence %||% NA_real_),
    quote      = as.character(x$source_quote %||% NA_character_)
  )
}


#' Find or create a horizon row matching the given (top, bottom)
#'
#' For PDF-extracted horizons we may be merging into a pedon that
#' already has the horizons table from another source, or starting
#' from scratch. Strategy:
#'
#' \enumerate{
#'   \item If the pedon already has horizons, find an existing row
#'         whose \code{(top_cm, bottom_cm)} match within 1 cm.
#'   \item If none matches, append a new row with the canonical
#'         schema and return the new index.
#' }
#'
#' @keywords internal
#' @param pedon A \code{\link{PedonRecord}}.
find_or_append_horizon <- function(pedon, top_cm, bottom_cm) {
  h <- pedon$horizons
  n <- nrow(h)

  if (n > 0L && !is.null(top_cm) && !is.null(bottom_cm) &&
      !is.na(top_cm) && !is.na(bottom_cm)) {
    match_idx <- which(
      !is.na(h$top_cm) & !is.na(h$bottom_cm) &
      abs(h$top_cm    - top_cm)    <= 1 &
      abs(h$bottom_cm - bottom_cm) <= 1
    )
    if (length(match_idx) > 0L) return(match_idx[1L])
  }

  # Append a new empty row.
  new_row <- make_empty_horizons(1L)
  if (!is.null(top_cm))    new_row$top_cm[1L]    <- as.numeric(top_cm)
  if (!is.null(bottom_cm)) new_row$bottom_cm[1L] <- as.numeric(bottom_cm)

  pedon$horizons <- ensure_horizon_schema(
    data.table::rbindlist(list(h, new_row), fill = TRUE)
  )
  nrow(pedon$horizons)
}


#' Mapping from VLM-schema field names to PedonRecord horizon columns
#'
#' Some schema field names match horizon column names directly
#' (\code{clay_pct}, \code{ph_h2o}, etc.); a few do not
#' (\code{munsell_moist} expands to three columns). This helper lists
#' the simple 1-to-1 mappings; complex ones are handled inline in the
#' extraction body.
#'
#' @keywords internal
horizon_simple_attr_map <- function() {
  c(
    coarse_fragments_pct = "coarse_fragments_pct",
    clay_pct             = "clay_pct",
    silt_pct             = "silt_pct",
    sand_pct             = "sand_pct",
    ph_h2o               = "ph_h2o",
    ph_kcl               = "ph_kcl",
    oc_pct               = "oc_pct",
    n_total_pct          = "n_total_pct",
    cec_cmol             = "cec_cmol",
    ecec_cmol            = "ecec_cmol",
    bs_pct               = "bs_pct",
    al_sat_pct           = "al_sat_pct",
    ca_cmol              = "ca_cmol",
    mg_cmol              = "mg_cmol",
    k_cmol               = "k_cmol",
    na_cmol              = "na_cmol",
    al_cmol              = "al_cmol",
    caco3_pct            = "caco3_pct",
    caso4_pct            = "caso4_pct",
    fe_dcb_pct           = "fe_dcb_pct",
    ec_dS_m              = "ec_dS_m",
    bulk_density_g_cm3   = "bulk_density_g_cm3",
    structure_grade      = "structure_grade",
    structure_size       = "structure_size",
    structure_type       = "structure_type",
    consistence_moist    = "consistence_moist",
    clay_films_amount    = "clay_films_amount",
    clay_films_strength  = "clay_films_strength",
    boundary_distinctness = "boundary_distinctness",
    boundary_topography   = "boundary_topography"
  )
}


#' Apply a parsed horizons-extraction result to a pedon
#'
#' Walks the \code{horizons} array of a parsed extraction response,
#' creating / matching horizon rows and recording each non-null
#' attribute via \code{pedon$add_measurement(... source = "extracted_vlm")}.
#'
#' Returns the count of provenance entries added.
#'
#' @keywords internal
#' @param pedon A \code{\link{PedonRecord}}.
apply_horizons_extraction <- function(pedon,
                                       parsed,
                                       overwrite = FALSE,
                                       source_label = "extracted_vlm") {

  added <- 0L
  if (is.null(parsed$horizons) || length(parsed$horizons) == 0L) {
    return(added)
  }

  attr_map <- horizon_simple_attr_map()

  for (hz in parsed$horizons) {
    top_cm    <- hz$top_cm    %||% NA_real_
    bottom_cm <- hz$bottom_cm %||% NA_real_
    idx <- find_or_append_horizon(pedon,
                                   top_cm    = top_cm,
                                   bottom_cm = bottom_cm)

    # designation: simple field, no value/confidence wrapper.
    if (!is.null(hz$designation) && !is.na(hz$designation)) {
      pedon$add_measurement(
        idx, "designation",
        value      = as.character(hz$designation),
        source     = source_label,
        confidence = NA_real_,
        notes      = "VLM-extracted designation",
        overwrite  = overwrite
      )
      added <- added + 1L
    }

    # Munsell moist / dry: three columns each.
    for (slot_pair in list(
        list(slot = "munsell_moist",
             cols = c(munsell_hue_moist    = "hue",
                      munsell_value_moist  = "value",
                      munsell_chroma_moist = "chroma")),
        list(slot = "munsell_dry",
             cols = c(munsell_hue_dry      = "hue",
                      munsell_value_dry    = "value",
                      munsell_chroma_dry   = "chroma"))
    )) {
      m <- hz[[slot_pair$slot]]
      if (!is.null(m) && is.list(m)) {
        conf  <- as.numeric(m$confidence %||% NA_real_)
        quote <- as.character(m$source_quote %||% NA_character_)
        for (col_name in names(slot_pair$cols)) {
          field <- slot_pair$cols[[col_name]]
          if (!is.null(m[[field]]) && !is.na(m[[field]])) {
            val <- if (col_name == names(slot_pair$cols)[1L]) {
                       as.character(m[[field]])
                   } else {
                       as.numeric(m[[field]])
                   }
            pedon$add_measurement(
              idx, col_name,
              value      = val,
              source     = source_label,
              confidence = conf,
              notes      = quote,
              overwrite  = overwrite
            )
            added <- added + 1L
          }
        }
      }
    }

    # Simple {value, confidence, quote} attributes.
    for (schema_key in names(attr_map)) {
      col <- attr_map[[schema_key]]
      unpacked <- unpack_vlm_attr(hz[[schema_key]])
      if (is.null(unpacked)) next

      val <- unpacked$value
      if (col %in% names(horizon_column_spec()) &&
          horizon_column_spec()[[col]] == "numeric") {
        val <- suppressWarnings(as.numeric(val))
      } else {
        val <- as.character(val)
      }
      if (is.na(val)) next

      pedon$add_measurement(
        idx, col,
        value      = val,
        source     = source_label,
        confidence = unpacked$confidence,
        notes      = unpacked$quote,
        overwrite  = overwrite
      )
      added <- added + 1L
    }
  }

  added
}


#' Apply a parsed site-extraction result to a pedon
#'
#' Site metadata is not under provenance control (PedonRecord$site is
#' a free-form list, not a column with an authority-ranked log). We
#' therefore set the missing fields directly and emit a provenance
#' entry against horizon_idx 0 (sentinel for "site-level") only when
#' there is at least one horizon to anchor it.
#'
#' For attributes that already exist in \code{pedon$site}, we leave
#' them alone unless \code{overwrite = TRUE}. The VLM contract
#' (extracted_vlm < measured) is preserved by attribute origin: a
#' user-built site list is treated as authoritative; an empty / NULL
#' field can be filled by the VLM.
#'
#' @keywords internal
#' @param pedon A \code{\link{PedonRecord}}.
apply_site_extraction <- function(pedon, parsed, overwrite = FALSE) {
  if (is.null(parsed$site)) return(0L)

  added <- 0L
  if (is.null(pedon$site)) pedon$site <- list()

  # id and crs are flat strings/integers in the schema.
  for (flat_field in c("id", "crs")) {
    val <- parsed$site[[flat_field]]
    if (!is.null(val) && (overwrite || is.null(pedon$site[[flat_field]]))) {
      pedon$site[[flat_field]] <- val
      added <- added + 1L
    }
  }

  for (schema_key in c("lat", "lon", "date", "country", "elevation_m",
                        "slope_pct", "aspect_deg", "landform",
                        "parent_material", "land_use", "vegetation",
                        "drainage_class")) {
    unpacked <- unpack_vlm_attr(parsed$site[[schema_key]])
    if (is.null(unpacked)) next

    if (!is.null(pedon$site[[schema_key]]) && !overwrite) next
    pedon$site[[schema_key]] <- unpacked$value
    added <- added + 1L
  }

  added
}


# ---- user-facing extractors -------------------------------------------------


#' Extract horizons from a soil description PDF
#'
#' Reads a PDF (typically a soil survey chapter, field-sheet scan, or
#' thesis appendix), prompts the configured VLM to extract horizon
#' attributes against \code{inst/schemas/horizon.json}, and merges
#' the result into \code{pedon}. Every extracted attribute is recorded
#' with \code{source = "extracted_vlm"} and the model's reported
#' confidence and verbatim source quote.
#'
#' The PedonRecord's authority order guarantees that values already
#' tagged \code{"measured"} are never silently overwritten by VLM
#' extraction unless \code{overwrite = TRUE}.
#'
#' If the PDF is long (more than ~30,000 characters), it is chunked
#' page-by-page and each page is sent independently. This is a
#' conservative-but-simple strategy; for very long surveys callers
#' should pre-chunk and call this function once per profile.
#'
#' @section Failure modes:
#' \itemize{
#'   \item If \code{pdftools} is not installed -> error.
#'   \item If the PDF cannot be read -> error.
#'   \item If the VLM response fails JSON parse / schema validation
#'         after \code{max_retries + 1} attempts -> error from
#'         \code{validate_or_retry}.
#' }
#'
#' @param pedon A \code{\link{PedonRecord}} to merge into. Mutated in
#'        place AND returned invisibly.
#' @param pdf_path Path to the PDF file. Either \code{pdf_path} or
#'        \code{pdf_text} must be supplied.
#' @param pdf_text Optional alternative to \code{pdf_path}: the
#'        already-extracted description text. Useful for smoke
#'        tests, unit tests without \code{pdftools}, and for
#'        already-OCR'd field-sheet text.
#' @param provider A chat provider from \code{\link{vlm_provider}} (or
#'        a \code{\link{MockVLMProvider}} for testing).
#' @param max_retries Integer; how many times to re-prompt on
#'        validation failure. Default 3.
#' @param overwrite If \code{TRUE}, lower-authority values are allowed
#'        to clobber higher-authority ones. Default \code{FALSE}.
#' @param prompt_name Override the default prompt template
#'        (\code{"extract_horizons"}).
#' @param schema_name Override the default schema (\code{"horizon"}).
#' @return Invisibly, the (mutated) \code{pedon}. Carries a
#'         \code{"vlm_extraction"} attribute with the parsed response,
#'         number of attempts, and number of provenance entries added.
#' @export
extract_horizons_from_pdf <- function(pedon,
                                       pdf_path = NULL,
                                       provider,
                                       max_retries = 3L,
                                       overwrite   = FALSE,
                                       prompt_name = "extract_horizons",
                                       schema_name = "horizon",
                                       pdf_text    = NULL) {

  if (!inherits(pedon, "PedonRecord")) {
    rlang::abort("`pedon` must be a PedonRecord")
  }
  if (is.null(pdf_path) && is.null(pdf_text)) {
    rlang::abort("Provide either `pdf_path` (a PDF file) or `pdf_text` (the description text directly).")
  }
  if (!is.null(pdf_path)) {
    if (!file.exists(pdf_path)) {
      rlang::abort(sprintf("PDF not found: %s", pdf_path))
    }
    if (!requireNamespace("pdftools", quietly = TRUE)) {
      rlang::abort(paste0(
        "Package 'pdftools' is required to read PDFs but is not installed. ",
        "Install it with install.packages('pdftools')."
      ))
    }
    pages <- pdftools::pdf_text(pdf_path)
    full  <- paste(pages, collapse = "\n\n")
  } else {
    pages <- list(pdf_text)
    full  <- pdf_text
  }

  # Chunk only if the document is unusually long; most field
  # descriptions fit comfortably under the threshold.
  chunks <- if (nchar(full) <= 30000L) list(full) else as.list(pages)

  schema_json <- load_schema(schema_name)

  total_added    <- 0L
  total_attempts <- 0L
  parsed_list    <- vector("list", length(chunks))

  for (i in seq_along(chunks)) {
    rendered <- load_prompt(prompt_name, vars = list(
      schema_json   = schema_json,
      document_text = chunks[[i]]
    ))
    res <- validate_or_retry(provider, rendered, schema_name,
                              max_retries = max_retries)
    parsed_list[[i]] <- res$data
    total_attempts   <- total_attempts + res$attempts
    total_added      <- total_added +
      apply_horizons_extraction(pedon, res$data, overwrite = overwrite)
  }

  # Record document provenance.
  if (is.null(pedon$documents)) pedon$documents <- list()
  pedon$documents[[length(pedon$documents) + 1L]] <- list(
    type        = if (!is.null(pdf_path)) "pdf" else "pdf_text_inline",
    path        = if (!is.null(pdf_path)) normalizePath(pdf_path, mustWork = FALSE)
                  else "<inline pdf_text>",
    extracted_via = "VLM",
    extracted_at  = as.character(Sys.time()),
    attempts      = total_attempts,
    fields_added  = total_added
  )

  attr(pedon, "vlm_extraction") <- list(
    parsed       = parsed_list,
    attempts     = total_attempts,
    fields_added = total_added
  )
  invisible(pedon)
}


#' Extract Munsell color from a profile photo
#'
#' Sends the photo to a multimodal VLM with a prompt that asks the
#' model to estimate Munsell hue / value / chroma per visible horizon
#' (when a Munsell reference card is in frame). Recorded as
#' \code{extracted_vlm} with the model's self-reported confidence;
#' photos without a reference card should yield confidence below 0.5
#' per the prompt specification.
#'
#' Quantitative non-color attributes (clay \%, CEC, pH, etc.) are
#' \strong{never} extracted from photos, by prompt-level instruction.
#' If the model returns one anyway, it is silently dropped.
#'
#' @inheritParams extract_horizons_from_pdf
#' @param image_path Path to the image file (JPG / PNG).
#' @return Invisibly, the mutated \code{pedon}, with the photo added
#'         to \code{pedon$images}.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
extract_munsell_from_photo <- function(pedon,
                                        image_path,
                                        provider,
                                        max_retries = 3L,
                                        overwrite   = FALSE,
                                        prompt_name = "extract_munsell_from_photo",
                                        schema_name = "horizon") {

  if (!inherits(pedon, "PedonRecord")) {
    rlang::abort("`pedon` must be a PedonRecord")
  }
  if (!file.exists(image_path)) {
    rlang::abort(sprintf("Image not found: %s", image_path))
  }

  schema_json <- load_schema(schema_name)
  rendered <- load_prompt(prompt_name, vars = list(schema_json = schema_json))

  image_content <- if (requireNamespace("ellmer", quietly = TRUE) &&
                        exists("content_image_file",
                               envir = asNamespace("ellmer"),
                               inherits = FALSE)) {
    ellmer::content_image_file(image_path)
  } else {
    NULL
  }

  res <- validate_or_retry(provider, rendered, schema_name,
                            max_retries = max_retries,
                            image       = image_content)

  # Drop any quantitative non-color attributes the model may have
  # extracted; only Munsell entries should win provenance.
  if (!is.null(res$data$horizons)) {
    color_only_keys <- c("top_cm", "bottom_cm", "designation",
                          "munsell_moist", "munsell_dry")
    res$data$horizons <- lapply(res$data$horizons, function(h) {
      h[intersect(names(h), color_only_keys)]
    })
  }

  added <- apply_horizons_extraction(pedon, res$data, overwrite = overwrite)

  if (is.null(pedon$images)) pedon$images <- list()
  pedon$images[[length(pedon$images) + 1L]] <- list(
    type          = "profile_photo",
    path          = normalizePath(image_path, mustWork = FALSE),
    extracted_via = "VLM",
    extracted_at  = as.character(Sys.time()),
    attempts      = res$attempts,
    fields_added  = added
  )

  attr(pedon, "vlm_extraction") <- list(
    parsed       = res$data,
    attempts     = res$attempts,
    fields_added = added
  )
  invisible(pedon)
}


#' Extract site metadata from a field-sheet image
#'
#' Sends a photographed / scanned field sheet to a multimodal VLM and
#' merges the extracted site-level metadata (lat, lon, elevation,
#' parent material, land use, etc.) into \code{pedon$site}. Existing
#' fields are preserved unless \code{overwrite = TRUE}; only NULL
#' fields are filled.
#'
#' @inheritParams extract_horizons_from_pdf
#' @param image_path Path to the field-sheet image.
#' @return Invisibly, the mutated \code{pedon}.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
extract_site_from_fieldsheet <- function(pedon,
                                          image_path,
                                          provider,
                                          max_retries = 3L,
                                          overwrite   = FALSE,
                                          prompt_name = "extract_site_metadata",
                                          schema_name = "site") {

  if (!inherits(pedon, "PedonRecord")) {
    rlang::abort("`pedon` must be a PedonRecord")
  }
  if (!file.exists(image_path)) {
    rlang::abort(sprintf("Image not found: %s", image_path))
  }

  schema_json <- load_schema(schema_name)
  rendered <- load_prompt(prompt_name, vars = list(schema_json = schema_json))

  image_content <- if (requireNamespace("ellmer", quietly = TRUE) &&
                        exists("content_image_file",
                               envir = asNamespace("ellmer"),
                               inherits = FALSE)) {
    ellmer::content_image_file(image_path)
  } else {
    NULL
  }

  res <- validate_or_retry(provider, rendered, schema_name,
                            max_retries = max_retries,
                            image       = image_content)

  added <- apply_site_extraction(pedon, res$data, overwrite = overwrite)

  if (is.null(pedon$images)) pedon$images <- list()
  pedon$images[[length(pedon$images) + 1L]] <- list(
    type          = "field_sheet",
    path          = normalizePath(image_path, mustWork = FALSE),
    extracted_via = "VLM",
    extracted_at  = as.character(Sys.time()),
    attempts      = res$attempts,
    fields_added  = added
  )

  attr(pedon, "vlm_extraction") <- list(
    parsed       = res$data,
    attempts     = res$attempts,
    fields_added = added
  )
  invisible(pedon)
}
