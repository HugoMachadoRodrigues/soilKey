# =============================================================================
# v0.9.55 -- BDsolos (Sistema de Informacao de Solos Brasileiros) R-side helpers.
#
# BDsolos is the canonical Embrapa Brazilian soil profile database
# (~9,000 perfis). Distribuicao: Single-Page App em PHP de 2014 com
# fluxo interativo de 3 etapas; o caminho de export e CSV ZIP ou HTML.
# A API REST nao e publica.
#
# This module ships three R-side helpers:
#
#   load_bdsolos_csv(path)         -- robust loader: auto-detects which
#                                      column convention the CSV uses
#                                      (Munsell, granulometry, chemistry,
#                                      taxonomy) and returns a list of
#                                      PedonRecord. Always works once the
#                                      CSV is on disk.
#
#   inspect_bdsolos_csv(path)      -- diagnostic helper. Prints the raw
#                                      schema, the soilKey column mapping
#                                      that load_bdsolos_csv() will use,
#                                      and any columns it cannot map.
#                                      Run this before load_bdsolos_csv()
#                                      to validate the CSV shape.
#
#   download_bdsolos(out_dir,      -- best-effort programmatic download
#                     accept_terms,    via headless Chrome (chromote).
#                     filter_uf,        Drives the 3-step web form. Heavy
#                     ...)              queries (no UF filter, all 9k
#                                      profiles) frequently overload the
#                                      Embrapa server -- prefer batching
#                                      by UF and stitching the resulting
#                                      CSVs. Marked experimental.
#
# Per the Embrapa terms-of-use (consulta_publica.html splash):
# - The data is licensed for personal / academic use; commercial use
#   requires a separate Embrapa licence.
# - Publications using BDsolos data must cite the source per ABNT.
# - The user must accept the terms before downloading; this package
#   surfaces that via the explicit `accept_terms = TRUE` argument so
#   no download happens without informed consent.
# =============================================================================


# ---- Column-name detection ---------------------------------------------

#' Canonical mapping from BDsolos column-name variants to soilKey schema
#'
#' BDsolos exports use Portuguese column names with variable casing and
#' diacritic handling. This table records the regex patterns that
#' identify each soilKey horizon column. Patterns are matched
#' case-insensitively, after stripping diacritics and the underscore
#' between word fragments.
#'
#' @keywords internal
.BDSOLOS_COLUMN_PATTERNS <- list(
  # ---- horizon geometry ----
  designation       = "(simb_horiz|simbolo_horizonte|^horizonte$|^simbolo$)",
  top_cm            = "(limite_sup|profundidade_superior|prof_sup|^topo)",
  bottom_cm         = "(limite_inf|profundidade_inferior|prof_inf|^base)",
  # ---- Munsell (matiz / valor / croma) ----
  munsell_hue_moist    = "(cor_umida_matiz|matiz_umido|matiz_umida|^matiz$)",
  munsell_value_moist  = "(cor_umida_valor|valor_umido|valor_umida)",
  munsell_chroma_moist = "(cor_umida_croma|croma_umido|croma_umida)",
  munsell_hue_dry      = "(cor_seca_matiz|matiz_seco|matiz_seca)",
  munsell_value_dry    = "(cor_seca_valor|valor_seco|valor_seca)",
  munsell_chroma_dry   = "(cor_seca_croma|croma_seco|croma_seca)",
  # ---- structure / consistence / clay films ----
  structure_grade   = "(estrutura_grau|grau_estrutura)",
  structure_size    = "(estrutura_tamanho|tamanho_estrutura)",
  structure_type    = "(estrutura_tipo|tipo_estrutura)",
  clay_films_amount = "(cerosidade_quantidade|cerosidade_qtd)",
  clay_films_strength = "(cerosidade_grau|grau_cerosidade)",
  consistence_dry   = "(consistencia_seco|consistencia_seca)",
  consistence_moist = "(consistencia_umido|consistencia_umida)",
  # ---- texture (g/kg in BDsolos -> percent in soilKey) ----
  clay_pct          = "(^argila$|argila_total)",
  silt_pct          = "(^silte$|silte_total)",
  sand_pct          = "(^areia$|areia_total)",
  # ---- chemistry ----
  ph_h2o            = "(ph_em_agua|ph_h2o|ph_agua|^ph_water)",
  ph_kcl            = "(ph_em_kcl|ph_kcl)",
  ph_cacl2          = "(ph_em_cacl2|ph_cacl2)",
  oc_pct            = "(c_org|carbono_organico|^oc$|^c$|c_organico)",
  cec_cmol          = "(cec|ctc|capacidade_troca_cationica)",
  bs_pct            = "(^v$|saturacao_bases|sat_bases|^bs$)",
  al_sat_pct        = "(saturacao_aluminio|sat_aluminio|^m$)",
  ca_cmol           = "(ca_troc|calcio_trocavel|^ca$)",
  mg_cmol           = "(mg_troc|magnesio_trocavel|^mg$)",
  k_cmol            = "(k_troc|potassio_trocavel|^k$)",
  na_cmol           = "(na_troc|sodio_trocavel|^na$)",
  al_cmol           = "(al_troc|aluminio_trocavel|^al$)",
  caco3_pct         = "(caco3|carbonato_calcio)",
  p_mehlich3_mg_kg  = "(p_assim|fosforo_assimilavel|^p$|p_mehlich)",
  bulk_density_g_cm3 = "(densidade_solo|densidade_aparente|^ds$|^bd$)",
  fe_dcb_pct        = "(fe2o3|ferro_dcb|fe_dcb)",
  coarse_fragments_pct = "(cascalho|^cf$|coarse_frag)"
)


#' Strip Latin-1 diacritics + lowercase for fuzzy matching
#'
#' iconv ASCII//TRANSLIT renders Portuguese diacritics as bigraphs
#' (e.g. a-tilde -> ~a, c-cedilla -> c') which then get collapsed
#' into spurious underscores. Pre-replace the common Portuguese
#' diacritics by hand for deterministic output.
#'
#' @keywords internal
.bdsolos_norm <- function(x) {
  s <- tolower(as.character(x))
  # Portuguese diacritic map (24 chars in / 24 chars out), written
  # with Unicode escapes so the package source stays ASCII-pure
  # (R CMD check --as-cran requirement).
  # In:  a-acute a-grave a-circ a-tilde a-uml e-acute e-grave e-circ
  #      e-uml i-acute i-grave i-circ i-uml o-acute o-grave o-circ
  #      o-tilde o-uml u-acute u-grave u-circ u-uml c-cedilla n-tilde
  # Out: 5 a + 4 e + 4 i + 5 o + 4 u + c + n
  # Diacritic input source built from integer code points so the
  # package source stays ASCII-pure (R CMD check --as-cran).
  diac_in <- intToUtf8(c(
    0xe1L, 0xe0L, 0xe2L, 0xe3L, 0xe4L,    # a-acute, grave, circ, tilde, uml
    0xe9L, 0xe8L, 0xeaL, 0xebL,           # e-acute, grave, circ, uml
    0xedL, 0xecL, 0xeeL, 0xefL,           # i-acute, grave, circ, uml
    0xf3L, 0xf2L, 0xf4L, 0xf5L, 0xf6L,    # o-acute, grave, circ, tilde, uml
    0xfaL, 0xf9L, 0xfbL, 0xfcL,           # u-acute, grave, circ, uml
    0xe7L, 0xf1L                          # c-cedilla, n-tilde
  ))
  s <- chartr(diac_in, "aaaaaeeeeiiiiooooouuuucn", s)
  s <- gsub("[^a-z0-9_]+", "_", s)
  s <- gsub("_+", "_", s)
  s <- gsub("^_|_$", "", s)
  s
}


#' Guess the soilKey horizon column for a BDsolos column name
#'
#' Returns the canonical soilKey column name, or \code{NA_character_}
#' if no pattern matches.
#' @keywords internal
.bdsolos_match_column <- function(raw_name) {
  norm <- .bdsolos_norm(raw_name)
  if (!nzchar(norm)) return(NA_character_)
  for (sk_col in names(.BDSOLOS_COLUMN_PATTERNS)) {
    pat <- .BDSOLOS_COLUMN_PATTERNS[[sk_col]]
    if (grepl(pat, norm, ignore.case = TRUE, perl = TRUE)) {
      return(sk_col)
    }
  }
  NA_character_
}


#' Discover taxonomic column (the surveyor's SiBCS classification)
#' @keywords internal
.bdsolos_match_taxon_column <- function(raw_name) {
  norm <- .bdsolos_norm(raw_name)
  pat <- "(classificacao|taxon_sibcs|sibcs_class|nome_classe|^classe$|class_sibcs)"
  if (grepl(pat, norm, ignore.case = TRUE, perl = TRUE)) "taxon_sibcs"
  else NA_character_
}


# ---- Public: inspect_bdsolos_csv ---------------------------------------

#' Diagnostic inspection of a BDsolos CSV before loading
#'
#' Reads the CSV header, attempts to map each column to the soilKey
#' horizon schema via \code{\link{.bdsolos_match_column}}, and prints
#' three sections:
#'
#' \itemize{
#'   \item \strong{Mapped columns} -- BDsolos name -> soilKey name
#'   \item \strong{Unmapped columns} -- columns the loader will ignore
#'         (review these before running \code{load_bdsolos_csv} to make
#'         sure no critical attribute is silently dropped)
#'   \item \strong{Munsell coverage} -- whether matiz / valor / croma
#'         are present in either umido or seco variants
#' }
#'
#' Run this before \code{\link{load_bdsolos_csv}} on any new CSV from
#' BDsolos, especially if the export schema looks unfamiliar (BDsolos
#' has shipped multiple schema versions over the years).
#'
#' @param path Path to the CSV downloaded from BDsolos.
#' @param sep Field separator (default \code{","}; some BDsolos exports
#'        use \code{";"} or tab).
#' @return Invisibly, a list with \code{mapped}, \code{unmapped},
#'         \code{munsell_present}, \code{taxon_column}.
#' @export
inspect_bdsolos_csv <- function(path, sep = ",") {
  if (!file.exists(path)) {
    stop(sprintf("inspect_bdsolos_csv(): file not found: %s", path))
  }
  hdr <- readLines(path, n = 1L, encoding = "UTF-8")
  cols <- strsplit(hdr, sep, fixed = TRUE)[[1L]]
  cols <- trimws(gsub('"', "", cols))
  mapped   <- character(0)
  unmapped <- character(0)
  taxon_column <- NA_character_
  for (raw in cols) {
    sk <- .bdsolos_match_column(raw)
    tax <- .bdsolos_match_taxon_column(raw)
    if (!is.na(sk)) {
      mapped[raw] <- sk
    } else if (!is.na(tax)) {
      taxon_column <- raw
    } else if (nzchar(raw)) {
      unmapped <- c(unmapped, raw)
    }
  }
  has_matiz_um <- any(grepl("munsell_hue_moist", mapped, fixed = TRUE))
  has_valor_um <- any(grepl("munsell_value_moist", mapped, fixed = TRUE))
  has_croma_um <- any(grepl("munsell_chroma_moist", mapped, fixed = TRUE))
  munsell_present <- list(matiz_umido = has_matiz_um,
                            valor_umido = has_valor_um,
                            croma_umido = has_croma_um)
  cli::cli_h2(sprintf("inspect_bdsolos_csv: %s", basename(path)))
  cli::cli_alert_info(sprintf("Total columns: %d", length(cols)))
  cli::cli_alert_info(sprintf("Mapped to soilKey: %d", length(mapped)))
  cli::cli_alert_info(sprintf("Unmapped: %d", length(unmapped)))
  if (length(mapped) > 0L) {
    cli::cli_h3("Mapped columns")
    for (raw in names(mapped)) {
      cli::cli_text(sprintf("  {.field %-30s} -> {.code %s}", raw, mapped[raw]))
    }
  }
  if (length(unmapped) > 0L) {
    cli::cli_h3("Unmapped columns (will be ignored)")
    for (raw in unmapped) cli::cli_text(sprintf("  {.field %s}", raw))
  }
  cli::cli_h3("Munsell coverage")
  cli::cli_text(sprintf("  matiz_umido:  %s", if (has_matiz_um) "FOUND" else "MISSING"))
  cli::cli_text(sprintf("  valor_umido:  %s", if (has_valor_um) "FOUND" else "MISSING"))
  cli::cli_text(sprintf("  croma_umido:  %s", if (has_croma_um) "FOUND" else "MISSING"))
  cli::cli_h3("Taxon column (surveyor's SiBCS reference)")
  cli::cli_text(sprintf("  %s", taxon_column %||% "(NOT FOUND)"))
  invisible(list(
    mapped          = mapped,
    unmapped        = unmapped,
    munsell_present = munsell_present,
    taxon_column    = taxon_column
  ))
}


# ---- Public: load_bdsolos_csv ------------------------------------------

#' Load a BDsolos CSV export as a list of PedonRecord objects
#'
#' Reads the long-format BDsolos CSV (one row per horizon, with a
#' profile-id key) and returns a list of \code{\link{PedonRecord}}
#' objects. Auto-detects the column-name convention via
#' \code{\link{inspect_bdsolos_csv}} and maps to the soilKey horizon
#' schema. Texture (argila / silte / areia) is converted from g/kg to
#' percent (BDsolos canonical unit).
#'
#' Profile-id columns are auto-detected: looks for any column whose
#' normalised name matches
#' \code{"id_perfil|profile_id|cod_perfil|^perfil$|sample_id|^id$"};
#' falls back to the first column when none match.
#'
#' @param path Path to the BDsolos CSV.
#' @param sep Field separator. Default \code{","}; BDsolos sometimes
#'        exports with \code{";"} or tab -- pass it explicitly.
#' @param verbose If \code{TRUE} (default), prints a one-line summary.
#' @return A list of \code{\link{PedonRecord}} objects. Each pedon
#'         has \code{site$id} from the profile-id column, the
#'         taxonomic reference (when present) at
#'         \code{site$reference_sibcs}, and one horizon row per CSV
#'         row matching the profile id.
#' @seealso \code{\link{inspect_bdsolos_csv}},
#'          \code{\link{download_bdsolos}}.
#' @export
load_bdsolos_csv <- function(path, sep = ",", verbose = TRUE) {
  if (!file.exists(path)) {
    stop(sprintf("load_bdsolos_csv(): file not found: %s", path))
  }
  d <- data.table::fread(path, sep = sep, encoding = "UTF-8")
  if (nrow(d) == 0L) {
    stop("load_bdsolos_csv(): CSV is empty.")
  }

  # Map columns
  raw_names <- names(d)
  norm_names <- vapply(raw_names, .bdsolos_norm, character(1L))

  sk_map <- character(0)
  taxon_col <- NA_character_
  id_col <- NA_character_
  lat_col <- NA_character_
  lon_col <- NA_character_
  for (i in seq_along(raw_names)) {
    raw <- raw_names[i]
    nrm <- norm_names[i]
    if (grepl("id_perfil|profile_id|cod_perfil|^perfil$|sample_id|^id$",
                nrm, ignore.case = TRUE)) {
      if (is.na(id_col)) id_col <- raw
    } else if (grepl("^lat$|latitude|^y$|coord_y", nrm, ignore.case = TRUE)) {
      if (is.na(lat_col)) lat_col <- raw
    } else if (grepl("^lon$|^lng$|longitude|^x$|coord_x", nrm, ignore.case = TRUE)) {
      if (is.na(lon_col)) lon_col <- raw
    }
    sk <- .bdsolos_match_column(raw)
    if (!is.na(sk)) sk_map[raw] <- sk
    tax <- .bdsolos_match_taxon_column(raw)
    if (!is.na(tax) && is.na(taxon_col)) taxon_col <- raw
  }
  if (is.na(id_col)) id_col <- raw_names[1L]

  ids <- as.character(d[[id_col]])
  uids <- unique(ids)
  out <- vector("list", length(uids))
  for (k in seq_along(uids)) {
    rid <- uids[k]
    rows <- d[ids == rid, ]
    if (nrow(rows) == 0L) next
    hz <- .bdsolos_rows_to_horizons(rows, sk_map)
    site <- list(
      id      = rid,
      lat     = if (!is.na(lat_col)) suppressWarnings(as.numeric(rows[[lat_col]][1L])) else NA_real_,
      lon     = if (!is.na(lon_col)) suppressWarnings(as.numeric(rows[[lon_col]][1L])) else NA_real_,
      country = "BR",
      reference_sibcs  = if (!is.na(taxon_col)) as.character(rows[[taxon_col]][1L]) else NA_character_,
      reference_source = "Embrapa BDsolos"
    )
    out[[k]] <- PedonRecord$new(site = site, horizons = hz)
  }
  if (isTRUE(verbose)) {
    n_with_munsell <- sum(vapply(out, function(p) {
      any(!is.na(p$horizons$munsell_hue_moist))
    }, logical(1L)))
    n_with_taxon <- sum(vapply(out, function(p) {
      !is.na(p$site$reference_sibcs %||% NA_character_)
    }, logical(1L)))
    cli::cli_alert_success(sprintf(
      "load_bdsolos_csv(): %d perfis (Munsell em B em %d, taxon em %d)",
      length(out), n_with_munsell, n_with_taxon
    ))
  }
  out
}


#' Convert a subset of BDsolos rows (one perfil) to a soilKey horizons table
#' @keywords internal
.bdsolos_rows_to_horizons <- function(rows, sk_map) {
  spec <- horizon_column_spec()
  hz_list <- list()
  for (raw in names(sk_map)) {
    sk <- sk_map[[raw]]
    val <- rows[[raw]]
    type_target <- spec[[sk]] %||% "character"
    if (type_target == "numeric") {
      val <- suppressWarnings(as.numeric(val))
      # BDsolos canonical columns are always g/kg for texture + OC.
      # Detect the source name to decide unit conversion deterministically.
      raw_norm <- .bdsolos_norm(raw)
      if (sk %in% c("clay_pct", "silt_pct", "sand_pct") &&
            grepl("^(argila|silte|areia)", raw_norm)) {
        val <- val / 10  # g/kg -> %
      } else if (sk %in% c("clay_pct", "silt_pct", "sand_pct")) {
        # Generic source -> heuristic: median > 100 means g/kg
        med <- stats::median(val[is.finite(val)], na.rm = TRUE)
        if (is.finite(med) && med > 100) val <- val / 10
      }
      if (sk == "oc_pct" &&
            grepl("(c_org|carbono_org)", raw_norm)) {
        val <- val / 10  # BDsolos g/kg -> %
      } else if (sk == "oc_pct") {
        med <- stats::median(val[is.finite(val)], na.rm = TRUE)
        if (is.finite(med) && med > 25) val <- val / 10
      }
    } else if (type_target == "integer") {
      val <- suppressWarnings(as.integer(val))
    } else if (type_target == "character") {
      val <- as.character(val)
    } else if (type_target == "logical") {
      val <- as.logical(val)
    }
    hz_list[[sk]] <- val
  }
  if (length(hz_list) == 0L) {
    return(make_empty_horizons(nrow(rows)))
  }
  hz <- data.table::as.data.table(hz_list)
  # Order by top_cm if present
  if ("top_cm" %in% names(hz)) {
    hz <- hz[order(hz$top_cm), ]
  }
  ensure_horizon_schema(hz)
}


# ---- Public: download_bdsolos (experimental) ---------------------------

#' Download the BDsolos consulta-publica CSV (experimental, requires chromote)
#'
#' Drives the Embrapa BDsolos web form via headless Chrome
#' (\code{chromote}) to produce a CSV of all profiles + all attributes.
#' Marked **experimental**: heavy queries (no UF filter) frequently
#' overload the Embrapa server. Prefer \code{filter_uf =} batches of
#' one or two states at a time and stitch the resulting CSVs.
#'
#' Per the Embrapa terms-of-use, the data is licensed for personal /
#' academic use and publications must cite the source per ABNT.
#' \strong{Set \code{accept_terms = TRUE} to acknowledge this and let
#' the function click "Concordo" on your behalf.}
#'
#' @param out_path File path for the downloaded CSV.
#' @param accept_terms Logical. Must be \code{TRUE} to proceed; the
#'        function aborts otherwise. Documents informed consent to
#'        the BDsolos terms (personal/academic use, ABNT citation).
#' @param filter_uf Optional 2-letter UF code (e.g. \code{"RJ"},
#'        \code{"SC"}). Strongly recommended -- the full-table query
#'        often times out.
#' @param attributes Character vector. Which attribute groups to
#'        request. Defaults to the full SiBCS-classification-relevant
#'        set (Identificacao + Localizacao + Classificacao for Pontos
#'        de Amostragem, Identificacao + Morfologicas + Fisicas +
#'        Quimicas for Horizontes; Mineralogicas excluded for
#'        performance). Pass \code{"all"} to include Mineralogicas.
#' @param timeout_seconds Total timeout for the AJAX query.
#'        Default 600 (10 min).
#' @param chromote_session Optional pre-built \code{chromote::ChromoteSession}.
#'        Useful to share a session across calls.
#' @param verbose If \code{TRUE} (default), prints progress.
#' @return File path to the downloaded CSV (invisible).
#'
#' @examples
#' \dontrun{
#' # Single UF (fast, recommended)
#' download_bdsolos("soil_data/bdsolos/RJ.csv",
#'                   accept_terms = TRUE,
#'                   filter_uf    = "RJ")
#'
#' # Stitch multiple UFs
#' for (uf in c("RJ", "SP", "MG", "ES")) {
#'   download_bdsolos(file.path("soil_data/bdsolos",
#'                                paste0(uf, ".csv")),
#'                     accept_terms = TRUE, filter_uf = uf)
#' }
#'
#' # Then load all of them
#' csvs <- list.files("soil_data/bdsolos", "\\.csv$", full.names = TRUE)
#' all_pedons <- unlist(lapply(csvs, load_bdsolos_csv), recursive = FALSE)
#' length(all_pedons)
#' }
#' @seealso \code{\link{load_bdsolos_csv}},
#'          \code{\link{inspect_bdsolos_csv}}.
#' @export
download_bdsolos <- function(out_path,
                               accept_terms      = FALSE,
                               filter_uf         = NULL,
                               attributes        = "default",
                               timeout_seconds   = 600,
                               chromote_session  = NULL,
                               verbose           = TRUE) {
  if (!isTRUE(accept_terms)) {
    stop("download_bdsolos(): the BDsolos terms-of-use require explicit ",
         "acceptance. Read the terms at ",
         "https://www.bdsolos.cnptia.embrapa.br/consulta_publica.html ",
         "and re-run with `accept_terms = TRUE`. The data is licensed for ",
         "personal / academic use; commercial use requires a separate ",
         "Embrapa licence; publications must cite the source per ABNT.")
  }
  if (!requireNamespace("chromote", quietly = TRUE)) {
    stop("download_bdsolos() requires the 'chromote' package. ",
         "Install with `install.packages(\"chromote\")`.")
  }

  if (is.null(chromote_session)) {
    if (isTRUE(verbose)) cli::cli_alert_info("Starting headless Chrome session...")
    chromote_session <- chromote::ChromoteSession$new()
    on.exit(try(chromote_session$close(), silent = TRUE), add = TRUE)
  }

  url <- "https://www.bdsolos.cnptia.embrapa.br/consulta_publica.html"
  chromote_session$Page$navigate(url)
  chromote_session$Page$loadEventFired(timeout_ = 30)
  Sys.sleep(2)  # let the SPA bootstrap

  # Step 0: accept terms
  if (isTRUE(verbose)) cli::cli_alert_info("Accepting terms...")
  .bdsolos_eval(chromote_session, "
    var btns = document.querySelectorAll('input[type=button], button');
    for (var i = 0; i < btns.length; i++) {
      if ((btns[i].value || btns[i].textContent || '').match(/Concordo/i)) {
        btns[i].click(); break;
      }
    }
  ")
  Sys.sleep(2)

  # Step 1: select all attributes from Pontos de Amostragem and Horizontes
  if (isTRUE(verbose)) cli::cli_alert_info("Step 1: selecting attributes...")
  attr_js <- if (identical(attributes, "all")) "" else
    "['Mineralogicas', 'Mineralogicas']"
  .bdsolos_eval(chromote_session, sprintf("
    document.querySelectorAll('input[type=checkbox]').forEach(function(cb) {
      var lbl = (cb.parentElement && cb.parentElement.textContent || '').trim();
      // Always tick: Pontos (Identificacao, Localizacao, Classificacao,
      // Descricao do Ambiente) + Horizontes (Identificacao, Morfologicas,
      // Fisicas, Quimicas).
      if (/Identificacao|Localizacao|Classificacao|Ambiente|Morfologicas|Fisicas|Quimicas/i.test(lbl)) {
        if (!cb.checked) cb.click();
      }
      if (/Mineralogicas/i.test(lbl) && %s.length > 0) {
        if (!cb.checked) cb.click();
      }
    });
    // Click 'Ir para a etapa 2'
    var nextBtn = document.querySelector('input[onclick*=\"goStep2\"], button[onclick*=\"goStep2\"]');
    if (nextBtn) nextBtn.click();
  ", attr_js))
  Sys.sleep(3)

  # Step 2: optionally apply UF filter, then submit
  if (!is.null(filter_uf)) {
    if (isTRUE(verbose)) cli::cli_alert_info(sprintf("Step 2: filter UF = %s", filter_uf))
    .bdsolos_eval(chromote_session, sprintf("
      // Find the Localizacao filter checkbox under Pontos de Amostragem
      // and trigger it; then set the UF dropdown.
      // (Heuristic: filter UI varies; this is best-effort.)
      document.querySelectorAll('input[type=checkbox]').forEach(function(cb) {
        var lbl = (cb.parentElement && cb.parentElement.textContent || '').trim();
        if (/^Localizacao$/i.test(lbl) && !cb.checked) cb.click();
      });
      // Wait, then look for a UF select
      setTimeout(function() {
        var sels = document.querySelectorAll('select');
        for (var i = 0; i < sels.length; i++) {
          var nm = (sels[i].name || sels[i].id || '').toLowerCase();
          if (/uf|estado/.test(nm)) {
            sels[i].value = '%s'.toUpperCase();
            sels[i].dispatchEvent(new Event('change'));
            break;
          }
        }
      }, 500);
    ", filter_uf))
    Sys.sleep(3)
  }

  # Submit Etapa 2 -> server-side query
  if (isTRUE(verbose)) cli::cli_alert_info("Submitting Etapa 2 (server-side query, may take minutes)...")
  .bdsolos_eval(chromote_session, "
    if (typeof realizaBusca === 'function') realizaBusca();
  ")

  # Wait for Etapa 3 to materialise
  start <- Sys.time()
  while (as.numeric(difftime(Sys.time(), start, units = "secs")) < timeout_seconds) {
    Sys.sleep(5)
    state <- .bdsolos_eval(chromote_session, "
      JSON.stringify({
        has_e3: document.body.innerText.indexOf('ETAPA 3') >= 0 ||
                document.body.innerText.indexOf('Etapa 3') >= 0,
        has_csv_radio: document.querySelectorAll('input[type=radio][value=csv]').length > 0
      });
    ")
    parsed <- tryCatch(jsonlite::fromJSON(state %||% "{}"),
                        error = function(e) list())
    if (isTRUE(parsed$has_e3) && isTRUE(parsed$has_csv_radio)) break
  }
  if (!(isTRUE(parsed$has_e3) && isTRUE(parsed$has_csv_radio))) {
    stop("download_bdsolos(): server query timed out after ",
         timeout_seconds, "s. Try a smaller filter (filter_uf = ...) or ",
         "increase timeout_seconds.")
  }

  # Step 3: select all results, choose CSV radio, submit
  if (isTRUE(verbose)) cli::cli_alert_info("Step 3: selecting all results, format = CSV")
  .bdsolos_eval(chromote_session, "
    // Select the 'Todos' link if present
    var links = document.querySelectorAll('a, span');
    for (var i = 0; i < links.length; i++) {
      if ((links[i].textContent || '').trim().match(/^Todos$/i)) {
        links[i].click(); break;
      }
    }
    // Pick the CSV radio
    var radios = document.querySelectorAll('input[type=radio]');
    for (var i = 0; i < radios.length; i++) {
      if ((radios[i].value || '').toLowerCase() === 'csv') {
        radios[i].click(); break;
      }
    }
    // Click 'Visualizar Resultados Selecionados'
    var btns = document.querySelectorAll('input[type=button], button');
    for (var i = 0; i < btns.length; i++) {
      if ((btns[i].value || btns[i].textContent || '').match(/Resultados Selecionados/i)) {
        btns[i].click(); break;
      }
    }
  ")

  # Capture the CSV: BDsolos serves it as a new tab / inline content.
  # Best-effort: read the response body via fetch.
  Sys.sleep(5)
  csv_text <- .bdsolos_eval(chromote_session, "
    // Try to grab the visible CSV text directly from the page.
    var pre = document.querySelector('pre, textarea');
    if (pre) return pre.textContent || pre.value;
    return document.body.innerText;
  ")
  if (is.null(csv_text) || nchar(csv_text) < 100L) {
    stop("download_bdsolos(): the page did not contain CSV-like content. ",
         "The Embrapa export flow may have changed; please download manually ",
         "and use load_bdsolos_csv() instead.")
  }
  writeLines(csv_text, out_path, useBytes = TRUE)
  if (isTRUE(verbose)) {
    cli::cli_alert_success(sprintf("Saved CSV to %s (%d bytes)",
                                     out_path,
                                     file.info(out_path)$size))
  }
  invisible(out_path)
}


#' Evaluate JS in a chromote session, returning a string result
#' @keywords internal
.bdsolos_eval <- function(chromote_session, js) {
  out <- chromote_session$Runtime$evaluate(js, returnByValue = TRUE)
  if (is.null(out$result)) return(NULL)
  out$result$value
}
