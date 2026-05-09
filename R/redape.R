# =============================================================================
# Redape (Embrapa) loader + downloader
#
# v0.9.71 -- Integrate the curated GeoTab dataset published by
# Vaz, Silva Jr & Silva Neto (2023) at the Embrapa Redape repository
# (DOI: 10.48432/PYKKA7), "Brazilian soil data for taxonomic
# classification". The dataset ships ~96 curated soil profiles in
# structured JSON format plus the original RTF profile sheets.
# Pedologists hand-reviewed each profile, so the dataset is suitable
# as a gold-standard benchmark for classify_sibcs / classify_via_smartsolos_api
# without the data-quality artifacts that plague the raw BDsolos export
# (missing CEC, missing texture, implausible al_cmol values).
#
# Citation:
#   Vaz, G. J., Silva Jr, A. F., & Silva Neto, L. de F. da. (2023).
#   Brazilian soil data for taxonomic classification. Redape, V1.
#   https://doi.org/10.48432/PYKKA7
# =============================================================================


#' Embrapa Redape Dataverse API endpoint
#' @keywords internal
.REDAPE_API_BASE <- "https://www.redape.dados.embrapa.br/api"

#' Default DOI for the Vaz et al. 2023 curated GeoTab dataset
#' @keywords internal
.REDAPE_GEOTAB_DOI <- "10.48432/PYKKA7"


#' Download the curated Redape GeoTab dataset (Vaz et al 2023)
#'
#' Enumerates the dataset via the Dataverse API and downloads all
#' JSON profile files (the structured / interoperable format used
#' by the curators) into \code{dest_dir}. Skips files already
#' present unless \code{overwrite = TRUE}.
#'
#' @param dest_dir Destination directory for the JSON files.
#' @param dataset_doi DOI of the dataset (default: the Vaz 2023 dataset).
#' @param include_rtf If \code{TRUE}, also download the original RTF
#'        profile sheets (default \code{FALSE}; the JSON files alone
#'        are enough for classification).
#' @param overwrite If \code{TRUE}, re-download files that already
#'        exist locally.
#' @param verbose Print progress (default \code{TRUE}).
#'
#' @return Character vector of paths to the downloaded files.
#' @references Vaz, G. J., Silva Jr, A. F., & Silva Neto, L. de F. da
#'   (2023). Brazilian soil data for taxonomic classification. Redape, V1.
#'   \doi{10.48432/PYKKA7}.
#'
#' @export
download_redape_dataset <- function(dest_dir,
                                     dataset_doi = .REDAPE_GEOTAB_DOI,
                                     include_rtf = FALSE,
                                     overwrite   = FALSE,
                                     verbose     = TRUE) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("download_redape_dataset() requires the 'jsonlite' package.")
  }
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  meta_url <- sprintf("%s/datasets/:persistentId/?persistentId=doi:%s",
                       .REDAPE_API_BASE, dataset_doi)
  if (isTRUE(verbose))
    cat(sprintf("[redape] fetching metadata for DOI %s ...\n", dataset_doi))
  meta_raw <- tryCatch(
    suppressWarnings(readLines(meta_url, warn = FALSE)),
    error = function(e) {
      stop(sprintf("Failed to fetch Redape metadata: %s\n%s",
                    conditionMessage(e),
                    "(Redape may be temporarily down -- try again later.)"))
    }
  )
  meta <- jsonlite::fromJSON(paste(meta_raw, collapse = "\n"),
                                simplifyVector = FALSE)
  files <- meta$data$latestVersion$files
  if (length(files) == 0L)
    stop(sprintf("No files found in dataset DOI %s", dataset_doi))

  paths <- character(0)
  for (f in files) {
    df <- f$dataFile
    fn <- df$filename
    ct <- df$contentType %||% ""
    if (!isTRUE(include_rtf) && grepl("rtf", ct, ignore.case = TRUE)) next
    fid <- df$id
    dest <- file.path(dest_dir, fn)
    if (file.exists(dest) && !isTRUE(overwrite)) {
      if (isTRUE(verbose))
        cat(sprintf("  - %s (cached)\n", fn))
      paths <- c(paths, dest)
      next
    }
    url <- sprintf("%s/access/datafile/%s", .REDAPE_API_BASE, fid)
    if (isTRUE(verbose)) cat(sprintf("  - %s\n", fn))
    tryCatch(
      utils::download.file(url, dest, mode = "wb", quiet = !verbose),
      error = function(e) {
        warning(sprintf("Failed to download %s: %s", fn, conditionMessage(e)))
      }
    )
    if (file.exists(dest)) paths <- c(paths, dest)
  }
  if (isTRUE(verbose))
    cat(sprintf("[redape] downloaded %d files to %s\n",
                 length(paths), dest_dir))
  invisible(paths)
}


#' Read a single Redape GeoTab JSON file
#'
#' The Redape JSON format wraps profiles in \code{{"items": [{...}]}}.
#' Some files in the published dataset have a stray trailing brace
#' that breaks strict JSON parsers; this helper tolerates it.
#'
#' @param path Path to a JSON file.
#' @return List of items (typically length 1).
#' @keywords internal
.redape_read_json <- function(path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop(".redape_read_json() requires 'jsonlite'.")
  }
  raw <- paste(readLines(path, warn = FALSE), collapse = "\n")
  raw <- trimws(raw)
  parsed <- tryCatch(
    jsonlite::fromJSON(raw, simplifyVector = FALSE),
    error = function(e) {
      # Tolerate the stray trailing brace observed in some Redape files.
      if (endsWith(raw, "}}")) {
        return(jsonlite::fromJSON(substr(raw, 1, nchar(raw) - 1L),
                                    simplifyVector = FALSE))
      }
      stop(sprintf("Failed to parse %s: %s", path, conditionMessage(e)))
    }
  )
  parsed$items %||% list()
}


#' Convert one Redape GeoTab horizon record to a soilKey horizon row
#' @keywords internal
.redape_horizon_to_soilkey <- function(h) {
  # Texture: g/kg -> %
  argila_g    <- h$ARGILA   %||% NA_real_
  silte_g     <- h$SILTE    %||% NA_real_
  areia_gros  <- h$AREIA_GROS %||% NA_real_
  areia_fina  <- h$AREIA_FINA %||% NA_real_
  sand_pct <- if (!is.na(areia_gros) || !is.na(areia_fina))
                (sum(c(areia_gros, areia_fina), na.rm = TRUE)) / 10
              else NA_real_
  # CEC = Valor T = S + H + Al  (cmol_c/kg)
  ca <- h$CA_TROC %||% NA_real_
  mg <- h$MG_TROC %||% NA_real_
  k  <- h$K_TROC  %||% NA_real_
  na <- h$NA_TROC %||% NA_real_
  al <- h$AL_TROC %||% NA_real_
  hh <- h$H_TROC  %||% NA_real_
  s_value <- sum(c(ca, mg, k, na), na.rm = TRUE)
  # Only compute CEC if we have at least Ca, Mg, K, Na, Al, H
  components <- c(ca, mg, k, na, al, hh)
  cec <- if (any(is.na(components))) NA_real_ else
            sum(c(s_value, hh, al))
  bs_pct <- if (!is.na(cec) && cec > 0) 100 * s_value / cec else NA_real_
  # OC: g/kg -> %
  oc_g <- h$C_ORG %||% NA_real_
  oc_pct <- if (!is.na(oc_g)) oc_g / 10 else NA_real_

  list(
    top_cm                  = h$LIMITE_SUP %||% NA_real_,
    bottom_cm               = h$LIMITE_INF %||% NA_real_,
    designation             = h$SIMB_HORIZ %||% NA_character_,
    munsell_hue_moist       = h$COR_UMIDA_MATIZ %||% NA_character_,
    munsell_value_moist     = h$COR_UMIDA_VALOR %||% NA_real_,
    munsell_chroma_moist    = h$COR_UMIDA_CROMA %||% NA_real_,
    munsell_hue_dry         = h$COR_SECA_MATIZ %||% NA_character_,
    munsell_value_dry       = h$COR_SECA_VALOR %||% NA_real_,
    munsell_chroma_dry      = h$COR_SECA_CROMA %||% NA_real_,
    clay_pct                = if (!is.na(argila_g))    argila_g / 10 else NA_real_,
    silt_pct                = if (!is.na(silte_g))     silte_g  / 10 else NA_real_,
    sand_pct                = sand_pct,
    ph_h2o                  = h$PH_AGUA %||% NA_real_,
    ph_kcl                  = h$PH_KCL  %||% NA_real_,
    oc_pct                  = oc_pct,
    cec_cmol                = cec,
    bs_pct                  = bs_pct,
    ca_cmol                 = ca,
    mg_cmol                 = mg,
    k_cmol                  = k,
    na_cmol                 = na,
    al_cmol                 = al,
    p_mehlich3_mg_kg        = h$P_ASSIM %||% NA_real_,
    fe_dcb_pct              = h$TEOR_FE %||% NA_real_,
    plinthite_pct           = if (isTRUE(h$PETROPLINTICO) || isTRUE(h$LITOPLINTICO))
                                30 else NA_real_,
    redoximorphic_features_pct = if (isTRUE(h$REDOXICO)) 10 else NA_real_,
    structure_grade         = NA_character_,  # numeric code -> categorical TBD
    structure_size          = NA_character_,
    structure_type          = NA_character_,
    cordic_horizon          = isTRUE(h$COESO),
    saprolite_pct           = if (isTRUE(h$FRAGMENTARIO)) 50 else NA_real_
  )
}


#' Convert one Redape GeoTab item to a soilKey PedonRecord
#' @keywords internal
.redape_item_to_pedon <- function(item) {
  hz_rows <- lapply(item$HORIZONTES %||% list(),
                     .redape_horizon_to_soilkey)
  if (length(hz_rows) == 0L) return(NULL)
  hz <- data.table::rbindlist(hz_rows, fill = TRUE)
  hz <- ensure_horizon_schema(hz)

  ref_sibcs_parts <- c(item$ORDEM     %||% NA_character_,
                        item$SUBORDEM  %||% NA_character_,
                        item$GDE_GRUPO %||% NA_character_,
                        item$SUBGRUPO  %||% NA_character_)
  ref_sibcs <- paste(ref_sibcs_parts[!is.na(ref_sibcs_parts) &
                                          nzchar(ref_sibcs_parts)],
                      collapse = " ")

  PedonRecord$new(
    site = list(
      id                = item$ID_PONTO %||% NA_character_,
      country           = "BR",
      reference_sibcs   = ref_sibcs,
      reference_sibcs_order    = item$ORDEM     %||% NA_character_,
      reference_sibcs_subordem = item$SUBORDEM  %||% NA_character_,
      reference_sibcs_gg       = item$GDE_GRUPO %||% NA_character_,
      reference_sibcs_subgrupo = item$SUBGRUPO  %||% NA_character_,
      curation_note     = item$CURADORIA       %||% NA_character_,
      curation_authors  = item$AUTOR_CURADORIA %||% NA_character_,
      reference_source  = "Embrapa Redape (Vaz et al. 2023, GeoTab)"
    ),
    horizons = hz
  )
}


#' Load curated soil profiles from the Embrapa Redape GeoTab dataset
#'
#' Reads the structured JSON files (one profile per file) published
#' by Vaz et al. 2023 at the Embrapa Redape repository (DOI
#' \code{10.48432/PYKKA7}) and converts each one to a soilKey
#' \code{\link{PedonRecord}}.
#'
#' The dataset is unique in two ways:
#' \itemize{
#'   \item Every profile was hand-reviewed by experienced pedologists
#'         (the curation note and author list are preserved on each
#'         pedon site record), so it is suitable as a gold-standard
#'         benchmark.
#'   \item Unlike BDsolos, all profiles ship the full exchange complex
#'         (Ca, Mg, K, Na, Al \emph{and H}), so \code{cec_cmol}
#'         (Valor T = S + H + Al) is computed directly without any
#'         fallback option.
#' }
#'
#' @param json_dir Directory containing the GeoTab JSON files (or a
#'        character vector of file paths).
#' @param max_n If non-\code{NULL}, take a random sample of this size.
#' @param verbose Print progress (default \code{TRUE}).
#'
#' @return A list of \code{\link{PedonRecord}} objects.
#'
#' @section Reference:
#'   Vaz, G. J., Silva Jr, A. F., & Silva Neto, L. de F. da (2023).
#'   Brazilian soil data for taxonomic classification. Redape, V1.
#'   \doi{10.48432/PYKKA7}.
#'
#' @seealso \code{\link{download_redape_dataset}},
#'   \code{\link{benchmark_redape}}.
#'
#' @export
load_redape_pedons <- function(json_dir, max_n = NULL, verbose = TRUE) {
  if (length(json_dir) == 1L && dir.exists(json_dir)) {
    files <- list.files(json_dir, pattern = "\\.json$",
                          full.names = TRUE, recursive = FALSE)
    # Skip the state-level "*_all.json" aggregate files: they duplicate
    # the individual per-profile JSONs. Loading both would double-count.
    files <- files[!grepl("_all\\.json$", basename(files))]
  } else {
    files <- json_dir
  }
  if (length(files) == 0L) {
    stop("No JSON files found.")
  }
  if (!is.null(max_n) && length(files) > max_n)
    files <- sample(files, size = max_n)
  if (isTRUE(verbose))
    cat(sprintf("[redape] reading %d JSON files...\n", length(files)))

  pedons <- list()
  seen_ids <- character(0)
  for (f in files) {
    items <- tryCatch(.redape_read_json(f), error = function(e) {
      warning(sprintf("Skipping %s: %s", basename(f), conditionMessage(e)))
      list()
    })
    for (it in items) {
      id <- it$ID_PONTO %||% NA_character_
      # Defensive de-duplication in case some pedon ID appears twice.
      if (!is.na(id) && id %in% seen_ids) next
      pr <- tryCatch(.redape_item_to_pedon(it), error = function(e) NULL)
      if (!is.null(pr)) {
        pedons[[length(pedons) + 1L]] <- pr
        if (!is.na(id)) seen_ids <- c(seen_ids, id)
      }
    }
  }
  if (isTRUE(verbose))
    cat(sprintf("[redape] loaded %d pedons\n", length(pedons)))
  pedons
}


# =============================================================================
# v0.9.81 -- benchmark_redape() now actually computes Subordem / Grande
# Grupo / Subgrupo accuracy. The previous implementation accepted the
# `level` argument but discarded it: pred was ALWAYS res$rsg_or_order
# (Order) and ref was ALWAYS the order field, so all four levels
# returned identical accuracy and identical confusion matrices.
#
# The classifier already computes deeper levels in res$trace -- the
# benchmark just was not reading them. v0.9.81 wires:
#   * level="subordem"  -> res$trace$subordem_assigned$name
#   * level="gde_grupo" -> res$trace$grande_grupo_assigned$name
#   * level="subgrupo"  -> res$trace$subgrupo_assigned$name
# and builds the comparison key from the matching reference fields.
# =============================================================================


#' Strip Portuguese accents and lowercase / collapse whitespace
#'
#' Internal helper used by the Redape benchmark to canonicalise SiBCS
#' labels before string comparison. Maps accented Latin letters to
#' their ASCII equivalents (\code{A}-acute, \code{O}-tilde,
#' \code{C}-cedilla, etc., for all five Portuguese vowel classes).
#'
#' @keywords internal
.redape_strip_accents <- function(s) {
  if (is.null(s)) return(NA_character_)
  s <- as.character(s)[1L]
  if (is.na(s) || !nzchar(s)) return(NA_character_)
  s <- gsub("[\u00C1\u00C0\u00C2\u00C3\u00E1\u00E0\u00E2\u00E3]", "a", s)
  s <- gsub("[\u00C9\u00CA\u00E9\u00EA]",                              "e", s)
  s <- gsub("[\u00CD\u00ED]",                                            "i", s)
  s <- gsub("[\u00D3\u00D4\u00D5\u00F3\u00F4\u00F5]",                "o", s)
  s <- gsub("[\u00DA\u00FA]",                                            "u", s)
  s <- gsub("[\u00C7\u00E7]",                                            "c", s)
  tolower(gsub("\\s+", " ", trimws(s)))
}


#' Pluralise a single Portuguese token using SiBCS conventions
#'
#' Rules:
#' \itemize{
#'   \item Tokens of <= 2 chars are kept as-is (abbreviations like
#'         "tb"/"ta" used in SiBCS Cambissolo activity modifiers).
#'   \item Tokens already ending in "s" are kept as-is.
#'   \item Otherwise: append "s" (covers all -o, -ico, -oso, -eo, -io
#'         endings present in SiBCS Order, Subordem, GG, and Subgrupo
#'         modifiers; -al / -el / -ol words don't appear in the
#'         SiBCS taxonomy at these levels).
#' }
#' @keywords internal
.redape_pluralise_pt <- function(w) {
  if (is.na(w) || !nzchar(w)) return(w)
  if (nchar(w) <= 2L) return(w)
  if (substr(w, nchar(w), nchar(w)) == "s") return(w)
  paste0(w, "s")
}


#' Normalise a Portuguese SiBCS label to a plural-canonical comparison key
#' @keywords internal
.redape_canonical_label <- function(s, pluralise = TRUE) {
  s <- .redape_strip_accents(s)
  if (is.na(s) || !nzchar(s)) return(NA_character_)
  if (!isTRUE(pluralise)) return(s)
  toks <- strsplit(s, " ", fixed = TRUE)[[1L]]
  toks <- vapply(toks, .redape_pluralise_pt, character(1L))
  paste(toks, collapse = " ")
}


#' Compose the canonical reference label for a given SiBCS level
#'
#' Concatenates the relevant Redape reference fields (singular Portuguese
#' nominal phrase) and applies plural-canonical normalisation so the
#' result is comparable to the soilKey predicted label
#' (e.g.\ "Argissolos Amarelos Distroficos abrupticos").
#' @keywords internal
.redape_compose_ref <- function(pedon, level) {
  s <- pedon$site
  parts <- switch(level,
    order     = s$reference_sibcs_order,
    subordem  = c(s$reference_sibcs_order, s$reference_sibcs_subordem),
    gde_grupo = c(s$reference_sibcs_order, s$reference_sibcs_subordem,
                    s$reference_sibcs_gg),
    subgrupo  = c(s$reference_sibcs_order, s$reference_sibcs_subordem,
                    s$reference_sibcs_gg,  s$reference_sibcs_subgrupo))
  if (is.null(parts) || length(parts) == 0L) return(NA_character_)
  parts <- vapply(parts,
                    function(x) if (is.null(x)) NA_character_
                                else trimws(as.character(x)[1L]),
                    character(1L))
  if (any(is.na(parts) | !nzchar(parts))) return(NA_character_)
  combined <- paste(parts, collapse = " ")
  .redape_canonical_label(combined, pluralise = TRUE)
}


#' Extract the predicted label at a given SiBCS level from a classify_sibcs() result
#'
#' @details The returned label is a \code{ClassificationResult} field
#'   for the requested level (Order / Subordem / Grande Grupo / Subgrupo).
#' @keywords internal
.redape_extract_pred <- function(res, level) {
  if (is.null(res)) return(NA_character_)
  raw <- switch(level,
    order     = res$rsg_or_order,
    subordem  = res$trace$subordem_assigned$name,
    gde_grupo = res$trace$grande_grupo_assigned$name,
    subgrupo  = res$trace$subgrupo_assigned$name)
  if (is.null(raw) || (is.na(raw) %||% FALSE) || !nzchar(raw %||% ""))
    return(NA_character_)
  .redape_canonical_label(raw, pluralise = FALSE)
}


#' Benchmark soilKey SiBCS predictions against the Redape gold standard
#'
#' Runs \code{\link{classify_sibcs}} on each pedon and compares against
#' the curator-validated reference label (Order / Suborder / Great
#' Group / Subgroup). Returns per-level accuracy and the confusion
#' matrix at the requested granularity.
#'
#' @section v0.9.81 level-aware comparison:
#' Earlier versions accepted the \code{level} argument but always used
#' \code{rsg_or_order} for the prediction and the order field for the
#' reference, so all four levels reported identical accuracy. v0.9.81
#' reads the level-specific slots from \code{res$trace} (subordem,
#' grande_grupo, subgrupo) and concatenates the matching reference
#' fields, applying SiBCS-aware Portuguese pluralisation so the
#' comparison key matches the predictor's plural Title Case form.
#'
#' @param pedons List of \code{\link{PedonRecord}} objects (typically
#'        from \code{\link{load_redape_pedons}}).
#' @param level One of \code{"order"} (default), \code{"subordem"},
#'        \code{"gde_grupo"}, or \code{"subgrupo"}.
#' @param verbose Print progress (default \code{TRUE}).
#'
#' @return A list with \code{accuracy}, \code{n_compared},
#'   \code{confusion}, \code{per_class_recall}, and the per-pedon
#'   \code{predictions} table. \code{predictions} now also includes
#'   columns \code{ref_norm} and \code{pred_norm} -- the canonical
#'   comparison keys -- for downstream auditing.
#'
#' @export
benchmark_redape <- function(pedons, level = c("order", "subordem",
                                                  "gde_grupo", "subgrupo"),
                              verbose = TRUE) {
  level <- match.arg(level)
  if (length(pedons) == 0L) stop("Empty pedon list.")
  if (isTRUE(verbose))
    cat(sprintf("[redape] benchmarking %d pedons at level=%s\n",
                 length(pedons), level))

  rows <- vector("list", length(pedons))
  for (i in seq_along(pedons)) {
    pr <- pedons[[i]]
    res <- tryCatch(classify_sibcs(pr, on_missing = "silent"),
                     error = function(e) NULL)
    ref_norm  <- .redape_compose_ref(pr,  level)
    pred_norm <- .redape_extract_pred(res, level)
    raw_pred  <- if (!is.null(res)) {
      switch(level,
        order     = res$rsg_or_order,
        subordem  = res$trace$subordem_assigned$name,
        gde_grupo = res$trace$grande_grupo_assigned$name,
        subgrupo  = res$trace$subgrupo_assigned$name) %||% NA_character_
    } else NA_character_
    raw_ref <- switch(level,
      order     = pr$site$reference_sibcs_order,
      subordem  = paste(pr$site$reference_sibcs_order,
                          pr$site$reference_sibcs_subordem),
      gde_grupo = paste(pr$site$reference_sibcs_order,
                          pr$site$reference_sibcs_subordem,
                          pr$site$reference_sibcs_gg),
      subgrupo  = paste(pr$site$reference_sibcs_order,
                          pr$site$reference_sibcs_subordem,
                          pr$site$reference_sibcs_gg,
                          pr$site$reference_sibcs_subgrupo)) %||% NA_character_
    rows[[i]] <- data.frame(
      id        = pr$site$id %||% NA_character_,
      ref       = trimws(raw_ref),
      pred      = trimws(raw_pred %||% NA_character_),
      ref_norm  = ref_norm  %||% NA_character_,
      pred_norm = pred_norm %||% NA_character_,
      stringsAsFactors = FALSE
    )
  }
  pred_df <- do.call(rbind, rows)

  in_scope <- !is.na(pred_df$ref_norm) & nzchar(pred_df$ref_norm) &
                !is.na(pred_df$pred_norm) & nzchar(pred_df$pred_norm)
  n_compared <- sum(in_scope)
  n_correct  <- sum(in_scope & pred_df$ref_norm == pred_df$pred_norm)
  acc <- if (n_compared > 0L) n_correct / n_compared else NA_real_

  conf <- table(reference = pred_df$ref_norm[in_scope],
                  predicted = pred_df$pred_norm[in_scope])
  per_class <- data.frame(
    reference_rsg = rownames(conf),
    n             = rowSums(conf),
    n_correct     = vapply(rownames(conf),
                            function(r) if (r %in% colnames(conf)) conf[r, r] else 0L,
                            integer(1)),
    stringsAsFactors = FALSE
  )
  per_class$recall <- per_class$n_correct / per_class$n

  if (isTRUE(verbose))
    cat(sprintf("[redape] accuracy = %.1f%% on n = %d in-scope pedons\n",
                 100 * acc, n_compared))

  list(level = level,
       accuracy = acc,
       n_compared = n_compared,
       n_total = nrow(pred_df),
       confusion = conf,
       per_class_recall = per_class,
       predictions = pred_df)
}
