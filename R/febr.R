# =============================================================================
# v0.9.57 -- FEBR (Free Brazilian Repository for Open Soil Data) loader.
#
# FEBR is the UFSM-curated repository of Brazilian soil profiles
# (https://www.pedometria.org/febr/). Diagnostic scan in May 2026
# confirmed: 200 / 249 FEBR datasets (80.3%) carry Munsell colors,
# totalling 36,275 horizons with non-NA Munsell. Single dataset
# ctb0032 alone has 10,577 horizons with Munsell.
#
# This is the operational answer to the "Embrapa BDsolos doesn't have
# Munsell in the FEBR exports Hugo received" gap: many OTHER FEBR
# datasets (especially the ctb0500-ctb0700 series and ctb0032) DO
# have full morphology including Munsell. soilKey wires the febr R
# package (CRAN-stable) and adapts to the variable column-name
# conventions different FEBR datasets use.
#
# Two functions:
#
#   read_febr_pedons(dataset_codes, ...) -- wraps febr::readFEBR,
#       returns a list of PedonRecord. Auto-detects ~6 distinct
#       FEBR Munsell column conventions (cor_munsell_umida,
#       cor_cod_munsell_umida_1, cor_munsell_umida_matiz/valor/croma,
#       etc.). Parses PT-BR Munsell strings with comma-decimal
#       (e.g. "2,5YR 3/6").
#
#   febr_index_munsell(min_coverage = 0.1) -- returns the catalog
#       of FEBR datasets that have Munsell columns populated above
#       the requested coverage threshold. Cached on disk.
# =============================================================================


# ---- Munsell parser (PT-BR aware) -------------------------------------

#' Parse a single Brazilian-style Munsell color string into hue/value/chroma
#'
#' Handles the FEBR / SiBCS-canonical \code{"<matiz> <valor>/<croma>"}
#' format with PT-BR comma-decimal in any numeric component
#' (e.g. \code{"2,5YR 3/6"} -> hue \code{"2.5YR"}, value 3, chroma 6;
#' \code{"10YR 5,5/3,5"} -> hue \code{"10YR"}, value 5.5, chroma 3.5).
#'
#' Returns \code{c(hue = NA_character_, value = NA_real_, chroma =
#' NA_real_)} when the input is empty / unparseable.
#'
#' @keywords internal
.parse_febr_munsell <- function(s) {
  if (is.null(s) || length(s) == 0L || is.na(s) || !nzchar(trimws(s))) {
    return(list(hue = NA_character_, value = NA_real_, chroma = NA_real_))
  }
  s <- trimws(as.character(s))
  parts <- strsplit(s, "\\s+")[[1L]]
  if (length(parts) < 2L) {
    return(list(hue = NA_character_, value = NA_real_, chroma = NA_real_))
  }
  hue_raw <- parts[1L]
  vc_raw  <- parts[2L]
  # Normalise PT-BR comma-decimal in the hue numeric prefix (e.g. "2,5YR")
  hue <- gsub(",", ".", hue_raw, fixed = TRUE)
  # Split value/croma
  vc_parts <- strsplit(vc_raw, "/", fixed = TRUE)[[1L]]
  if (length(vc_parts) < 2L) {
    return(list(hue = hue, value = NA_real_, chroma = NA_real_))
  }
  v <- suppressWarnings(as.numeric(gsub(",", ".", vc_parts[1L], fixed = TRUE)))
  c <- suppressWarnings(as.numeric(gsub(",", ".", vc_parts[2L], fixed = TRUE)))
  list(hue = hue, value = v, chroma = c)
}


#' Vectorised Munsell-string parser
#'
#' Returns a data.frame with columns hue / value / chroma, one row per
#' input string.
#' @keywords internal
.parse_febr_munsell_vec <- function(x) {
  if (length(x) == 0L) {
    return(data.frame(hue = character(0), value = numeric(0),
                       chroma = numeric(0), stringsAsFactors = FALSE))
  }
  parts <- lapply(x, .parse_febr_munsell)
  data.frame(
    hue    = vapply(parts, function(p) p$hue %||% NA_character_, character(1L)),
    value  = vapply(parts, function(p) p$value %||% NA_real_,    numeric(1L)),
    chroma = vapply(parts, function(p) p$chroma %||% NA_real_,   numeric(1L)),
    stringsAsFactors = FALSE
  )
}


# ---- Munsell column auto-detection ------------------------------------

#' Discover Munsell-related columns in a FEBR layer table
#'
#' Returns a list with elements
#' \code{moist_string} (single column with the Munsell string),
#' \code{moist_hue}, \code{moist_value}, \code{moist_chroma} (separate
#' columns), \code{dry_string}, \code{dry_hue}, \code{dry_value},
#' \code{dry_chroma}. Each is either a column name (character) or
#' \code{NA_character_}. The loader uses the parsed columns when
#' available, falls back to parsing the string column.
#'
#' Recognised conventions (from the May 2026 scan of 249 FEBR datasets):
#'
#' \itemize{
#'   \item \code{cor_munsell_umida}           (ctb0039, ctb0572)
#'   \item \code{cor_cod_munsell_umida}       (ctb0032)
#'   \item \code{cor_cod_munsell_umida_1} +
#'         \code{cor_nome_munsell_umida_1}    (ctb0005)
#'   \item \code{cor_cod_munsell_umida_i}     (ctb0019)
#'   \item \code{cor_munsell_umida_matiz} +
#'         \code{cor_munsell_umida_valor} +
#'         \code{cor_munsell_umida_croma}     (ctb0562-ctb0700+)
#'   \item \code{cor_munsell_umida_nome}      (ctb0562+)
#'   \item \code{cor_matriz_umido_munsell}    (canonical, morphology())
#' }
#'
#' Same patterns apply for "seca" (dry).
#'
#' @keywords internal
.detect_febr_munsell_columns <- function(cols) {
  pick <- function(patterns) {
    for (p in patterns) {
      hits <- grep(p, cols, ignore.case = TRUE, perl = TRUE, value = TRUE)
      if (length(hits) > 0L) return(hits[1L])
    }
    NA_character_
  }
  list(
    # ---- Moist (umida) ----
    moist_hue    = pick(c("^cor_munsell_umida_matiz$",
                            "^cor_matriz_umido_munsell$")),
    moist_value  = pick(c("^cor_munsell_umida_valor$")),
    moist_chroma = pick(c("^cor_munsell_umida_croma$")),
    moist_string = pick(c(
      "^cor_munsell_umida$",
      "^cor_cod_munsell_umida$",
      "^cor_cod_munsell_umida_1$",
      "^cor_cod_munsell_umida_i$",
      "^cor_munsell_umida_nome$"
    )),
    # ---- Dry (seca) ----
    dry_hue    = pick(c("^cor_munsell_seca_matiz$",
                          "^cor_matriz_seco_munsell$")),
    dry_value  = pick(c("^cor_munsell_seca_valor$")),
    dry_chroma = pick(c("^cor_munsell_seca_croma$")),
    dry_string = pick(c(
      "^cor_munsell_seca$",
      "^cor_cod_munsell_seca$",
      "^cor_cod_munsell_seca_1$",
      "^cor_cod_munsell_seca_i$",
      "^cor_munsell_seca_nome$"
    ))
  )
}


# ---- Public: read_febr_pedons ------------------------------------------

#' Map FEBR layer-table columns to soilKey horizon attributes
#'
#' The FEBR \code{camada} (layer) table uses standardised variable
#' codes documented at
#' \url{https://www.pedometria.org/febr/dictionary/}. This internal
#' table records the regex patterns that map the most useful FEBR
#' codes onto the soilKey horizon schema. Multi-method codes (e.g.
#' clay determined by hydrometer vs sieve) are collapsed onto the
#' single soilKey column.
#'
#' @keywords internal
.FEBR_TO_HORIZON_MAP <- list(
  designation       = "^camada_nome$",
  top_cm            = "^profund_sup$",
  bottom_cm         = "^profund_inf$",
  ph_h2o            = "^ph_h2o(_|$)|^ph_em_agua",
  ph_kcl            = "^ph_kcl(_|$)",
  ph_cacl2          = "^ph_cacl2(_|$)",
  oc_pct            = "^carbono(_|$)|^c_org",
  cec_cmol          = "^ctc(_|$)|^cec(_|$)",
  bs_pct            = "^v_(percent|pct)|^saturacao_bases",
  al_sat_pct        = "^m_(percent|pct)|^saturacao_aluminio",
  ca_cmol           = "^ca_troc(_|$)|^calcio_trocavel",
  mg_cmol           = "^mg_troc(_|$)|^magnesio_trocavel",
  k_cmol            = "^k_troc(_|$)|^potassio_trocavel",
  na_cmol           = "^na_troc(_|$)|^sodio_trocavel",
  al_cmol           = "^al_troc(_|$)|^aluminio_trocavel",
  caco3_pct         = "^caco3(_|$)|^carbonato_calcio",
  p_mehlich3_mg_kg  = "^p_(mehlich|assim)|^fosforo_assim",
  bulk_density_g_cm3 = "^densidade_solo|^ds(_|$)|^bd(_|$)",
  fe_dcb_pct        = "^fe2o3(_|$)|^ferro_dcb",
  fe_ox_pct         = "^ferro_oxalato",
  al_ox_pct         = "^aluminio_oxalato",
  clay_pct          = "^argila(_|$)|^argila_total",
  silt_pct          = "^silte(_|$)|^silte_total",
  sand_pct          = "^areia(_|$)|^areia_total",
  coarse_fragments_pct = "^cascalho(_|$)|^coarse"
)


#' Load FEBR datasets as a list of PedonRecord objects
#'
#' Wraps \code{febr::readFEBR()} (CRAN package, FEBR v1.9.9+ recommended)
#' and adapts the returned \code{camada} (layer) +
#' \code{observacao} tables to the soilKey schema. Auto-detects
#' Munsell columns across the ~6 distinct conventions found in the
#' 200 FEBR datasets that carry color data, parses PT-BR Munsell
#' strings (\code{"2,5YR 3/6"}) and converts FEBR's standard units
#' to soilKey conventions.
#'
#' Per the May 2026 scan, ~80% of FEBR datasets have Munsell. Use
#' \code{\link{febr_index_munsell}} to get the curated list of
#' Munsell-bearing dataset IDs.
#'
#' @param dataset_codes Character vector of FEBR dataset IDs
#'        (e.g. \code{c("ctb0032", "ctb0562")}). Pass \code{"all"}
#'        to download every Munsell-bearing dataset; this is heavy
#'        (network calls per dataset). Default: a small curated
#'        sample for development.
#' @param febr_repo Optional override for the FEBR repository
#'        location, forwarded to \code{febr::readFEBR}.
#' @param min_munsell_coverage Drop pedons whose horizons are
#'        \emph{all} missing Munsell. Default 0 (keep all);
#'        set to 0.5 to keep only pedons with at least 50% of
#'        horizons having a Munsell hue.
#' @param verbose If \code{TRUE} (default), prints per-dataset
#'        join statistics.
#' @return A list of \code{\link{PedonRecord}} objects with
#'         \code{site$id} = FEBR \code{observacao_id},
#'         \code{site$reference_sibcs} = the surveyor's classification
#'         when available, and one horizon per FEBR \code{camada}
#'         row.
#'
#' @examples
#' \dontrun{
#' # Single dataset (35 perfis, 100% Munsell coverage)
#' pedons <- read_febr_pedons("ctb0039")
#'
#' # Multiple datasets
#' pedons <- read_febr_pedons(c("ctb0032", "ctb0562", "ctb0568"))
#'
#' # All Munsell-bearing datasets (slow; 200 datasets, ~36k horizons)
#' all_pedons <- read_febr_pedons("all")
#' }
#' @seealso \code{\link{febr_index_munsell}},
#'          \code{\link{load_bdsolos_csv}}.
#' @export
read_febr_pedons <- function(dataset_codes      = c("ctb0039"),
                              febr_repo          = NULL,
                              min_munsell_coverage = 0,
                              verbose            = TRUE) {
  if (!requireNamespace("febr", quietly = TRUE)) {
    stop("read_febr_pedons() requires the 'febr' package. ",
         "Install with `remotes::install_github(\"febr-team/febr-package\")`.")
  }
  if (length(dataset_codes) == 1L && identical(dataset_codes, "all")) {
    if (isTRUE(verbose)) {
      cli::cli_alert_info(
        "read_febr_pedons('all') -- using febr_index_munsell() to enumerate datasets..."
      )
    }
    idx <- febr_index_munsell(verbose = FALSE)
    dataset_codes <- idx$dataset_id
  }

  out <- list()
  for (ds in dataset_codes) {
    tbls <- tryCatch(
      febr::readFEBR(data.set = ds,
                      data.table = c("identificacao", "observacao", "camada"),
                      febr.repo = febr_repo,
                      verbose = FALSE),
      error = function(e) NULL
    )
    if (is.null(tbls)) {
      if (isTRUE(verbose)) cli::cli_alert_warning(sprintf("%s: read failed -- skipped.", ds))
      next
    }
    camada <- tbls$camada     %||% tbls[["camada"]]
    obs    <- tbls$observacao %||% tbls[["observacao"]]
    ident  <- tbls$identificacao %||% tbls[["identificacao"]]
    if (is.null(camada) || nrow(camada) == 0L) next

    # Munsell column detection
    mcols <- .detect_febr_munsell_columns(colnames(camada))

    # Soil-attribute column mapping
    sk_map <- .febr_match_layer_columns(colnames(camada))

    # Build pedons grouped by observacao_id
    obs_ids <- unique(camada$observacao_id %||% character(0))
    if (length(obs_ids) == 0L) next
    n_with_munsell <- 0L
    for (oid in obs_ids) {
      rows <- camada[camada$observacao_id == oid, , drop = FALSE]
      if (nrow(rows) == 0L) next
      hz <- .febr_rows_to_horizons(rows, sk_map, mcols)
      if (sum(!is.na(hz$munsell_hue_moist)) /
            max(1L, nrow(hz)) < min_munsell_coverage) next
      if (any(!is.na(hz$munsell_hue_moist))) n_with_munsell <- n_with_munsell + 1L
      ob_row <- if (!is.null(obs)) {
        sub <- obs[obs$observacao_id == oid, , drop = FALSE]
        if (nrow(sub) >= 1L) sub[1L, ] else NULL
      } else NULL
      out[[length(out) + 1L]] <- .febr_pedon_from_rows(oid, rows, ob_row,
                                                        ident, hz, ds)
    }
    if (isTRUE(verbose)) {
      cli::cli_alert_success(sprintf(
        "%s: %d perfis (Munsell em %d), %d horizons total.",
        ds, length(obs_ids), n_with_munsell, nrow(camada)
      ))
    }
  }
  out
}


#' Map FEBR layer-table columns to soilKey horizon column names
#' @keywords internal
.febr_match_layer_columns <- function(cols) {
  out <- list()
  for (sk in names(.FEBR_TO_HORIZON_MAP)) {
    pat <- .FEBR_TO_HORIZON_MAP[[sk]]
    hits <- grep(pat, cols, ignore.case = TRUE, perl = TRUE, value = TRUE)
    if (length(hits) > 0L) out[[sk]] <- hits[1L]
  }
  out
}


#' Build a soilKey horizons table from a subset of FEBR camada rows
#' @keywords internal
.febr_rows_to_horizons <- function(rows, sk_map, mcols) {
  spec <- horizon_column_spec()
  hz <- list()
  for (sk in names(sk_map)) {
    raw <- sk_map[[sk]]
    val <- rows[[raw]]
    type_target <- spec[[sk]] %||% "character"
    if (type_target == "numeric") {
      val <- suppressWarnings(as.numeric(gsub(",", ".",
                                                as.character(val), fixed = TRUE)))
      # FEBR clay/sand/silt/oc are commonly g/kg; convert to %
      if (sk %in% c("clay_pct", "silt_pct", "sand_pct")) {
        med <- stats::median(val[is.finite(val)], na.rm = TRUE)
        if (is.finite(med) && med > 100) val <- val / 10
      }
      if (sk == "oc_pct") {
        med <- stats::median(val[is.finite(val)], na.rm = TRUE)
        if (is.finite(med) && med > 25) val <- val / 10
      }
    } else if (type_target == "integer") {
      val <- suppressWarnings(as.integer(val))
    } else {
      val <- as.character(val)
    }
    hz[[sk]] <- val
  }
  # Munsell: prefer the parsed columns if present; fall back to the
  # string column.
  if (!is.na(mcols$moist_hue) && !is.na(mcols$moist_value) &&
        !is.na(mcols$moist_chroma)) {
    hz$munsell_hue_moist    <- gsub(",", ".",
                                       as.character(rows[[mcols$moist_hue]]),
                                       fixed = TRUE)
    hz$munsell_value_moist  <- suppressWarnings(as.numeric(gsub(",", ".",
                                       as.character(rows[[mcols$moist_value]]),
                                       fixed = TRUE)))
    hz$munsell_chroma_moist <- suppressWarnings(as.numeric(gsub(",", ".",
                                       as.character(rows[[mcols$moist_chroma]]),
                                       fixed = TRUE)))
  } else if (!is.na(mcols$moist_string)) {
    parsed <- .parse_febr_munsell_vec(rows[[mcols$moist_string]])
    hz$munsell_hue_moist    <- parsed$hue
    hz$munsell_value_moist  <- parsed$value
    hz$munsell_chroma_moist <- parsed$chroma
  }
  if (!is.na(mcols$dry_hue) && !is.na(mcols$dry_value) &&
        !is.na(mcols$dry_chroma)) {
    hz$munsell_hue_dry    <- gsub(",", ".",
                                     as.character(rows[[mcols$dry_hue]]),
                                     fixed = TRUE)
    hz$munsell_value_dry  <- suppressWarnings(as.numeric(gsub(",", ".",
                                     as.character(rows[[mcols$dry_value]]),
                                     fixed = TRUE)))
    hz$munsell_chroma_dry <- suppressWarnings(as.numeric(gsub(",", ".",
                                     as.character(rows[[mcols$dry_chroma]]),
                                     fixed = TRUE)))
  } else if (!is.na(mcols$dry_string)) {
    parsed <- .parse_febr_munsell_vec(rows[[mcols$dry_string]])
    hz$munsell_hue_dry    <- parsed$hue
    hz$munsell_value_dry  <- parsed$value
    hz$munsell_chroma_dry <- parsed$chroma
  }
  if (length(hz) == 0L) return(make_empty_horizons(nrow(rows)))
  hz <- data.table::as.data.table(hz)
  if ("top_cm" %in% names(hz)) hz <- hz[order(hz$top_cm), ]
  ensure_horizon_schema(hz)
}


#' Build a single PedonRecord from FEBR rows
#' @keywords internal
.febr_pedon_from_rows <- function(oid, camada_rows, ob_row, ident, hz, ds) {
  taxon <- if (!is.null(ob_row)) {
    grep_cols <- grep("^taxon|sibcs|classifica",
                       names(ob_row), ignore.case = TRUE, value = TRUE)
    if (length(grep_cols) > 0L) {
      vals <- vapply(grep_cols,
                       function(c) as.character(ob_row[[c]]),
                       character(1L))
      vals <- vals[nzchar(vals) & !is.na(vals)]
      if (length(vals) > 0L) paste(unique(vals), collapse = "; ") else NA_character_
    } else NA_character_
  } else NA_character_
  lat <- if (!is.null(ob_row) && "coord_y" %in% names(ob_row))
           suppressWarnings(as.numeric(ob_row$coord_y)) else NA_real_
  lon <- if (!is.null(ob_row) && "coord_x" %in% names(ob_row))
           suppressWarnings(as.numeric(ob_row$coord_x)) else NA_real_
  estado <- if (!is.null(ob_row) && "estado_id" %in% names(ob_row))
              as.character(ob_row$estado_id) else NA_character_
  PedonRecord$new(
    site = list(
      id      = as.character(oid),
      lat     = lat,
      lon     = lon,
      country = "BR",
      state   = estado,
      reference_sibcs  = taxon,
      reference_source = sprintf("FEBR / %s", ds)
    ),
    horizons = hz
  )
}


# ---- febr_index_munsell -------------------------------------------------

#' Curated index of FEBR datasets that carry Munsell colors
#'
#' Returns a data.frame listing FEBR dataset IDs that have at least
#' one Munsell-related column populated in their \code{camada} table,
#' with metadata: \code{n_horizons}, \code{n_finite_munsell},
#' \code{coverage}, \code{column_pattern}.
#'
#' Backed by a precomputed cache shipped in
#' \code{R/sysdata.rda} (\code{.FEBR_MUNSELL_INDEX}; results of the
#' May 2026 scan over 249 datasets). On first call after install,
#' returns the cache instantly. Pass \code{refresh = TRUE} to
#' re-scan FEBR live (slow, network-dependent; updates the
#' in-memory copy but does not modify the bundled cache).
#'
#' @param min_coverage Drop datasets whose Munsell coverage (fraction
#'        of horizons with non-NA hue) is below this. Default 0.1.
#' @param refresh Logical. If \code{TRUE}, re-scan FEBR over the
#'        network instead of using the bundled May-2026 cache.
#' @param verbose If \code{TRUE} (default), prints a one-line summary.
#' @return A \code{data.frame} sorted by \code{n_finite_munsell}
#'         descending.
#' @seealso \code{\link{read_febr_pedons}}.
#' @export
febr_index_munsell <- function(min_coverage = 0.1,
                                 refresh      = FALSE,
                                 verbose      = TRUE) {
  if (isTRUE(refresh)) {
    if (!requireNamespace("febr", quietly = TRUE)) {
      stop("refresh = TRUE requires the febr package.")
    }
    if (isTRUE(verbose)) {
      cli::cli_alert_info("Live scan of FEBR (slow, ~10-15 min)...")
    }
    idx_full <- febr::readIndex()
    n_total  <- nrow(idx_full)
    out_rows <- list()
    for (i in seq_len(n_total)) {
      ds <- idx_full$dados_id[i]
      tbl <- tryCatch(suppressWarnings(suppressMessages(
        febr::readFEBR(data.set = ds, data.table = "camada",
                        verbose = FALSE)
      )), error = function(e) NULL)
      if (is.null(tbl)) next
      if (is.list(tbl) && !is.data.frame(tbl)) tbl <- tbl[[1]]
      mcols <- .detect_febr_munsell_columns(colnames(tbl))
      first_col <- mcols$moist_hue %||% mcols$moist_string
      if (is.na(first_col)) next
      n_horizons <- nrow(tbl)
      n_finite <- sum(!is.na(tbl[[first_col]]))
      if (n_finite == 0L) next
      cov <- n_finite / max(1L, n_horizons)
      out_rows[[length(out_rows) + 1L]] <- data.frame(
        dataset_id      = ds,
        n_horizons      = as.integer(n_horizons),
        n_finite_munsell = as.integer(n_finite),
        coverage        = cov,
        column_pattern  = first_col,
        stringsAsFactors = FALSE
      )
      if (i %% 25L == 0L && isTRUE(verbose)) {
        cli::cli_alert_info(sprintf("scanned %d / %d", i, n_total))
      }
    }
    res <- do.call(rbind, out_rows)
  } else {
    res <- get0(".FEBR_MUNSELL_INDEX", envir = asNamespace("soilKey"))
    if (is.null(res)) {
      stop("febr_index_munsell(): bundled cache not available -- ",
           "call with refresh = TRUE to scan FEBR live.")
    }
  }
  res <- res[res$coverage >= min_coverage, ]
  res <- res[order(-res$n_finite_munsell), ]
  rownames(res) <- NULL
  if (isTRUE(verbose)) {
    cli::cli_alert_success(sprintf(
      "%d FEBR datasets with Munsell coverage >= %.0f%% (%d horizonts total).",
      nrow(res), 100 * min_coverage, sum(res$n_finite_munsell)
    ))
  }
  res
}
