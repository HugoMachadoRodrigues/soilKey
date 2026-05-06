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
  designation       = "(simb_horiz|^simbolo_horizonte$|^horizonte$|^simbolo$)",
  top_cm            = "(limite_sup|^profundidade_superior$|prof_sup|^topo)",
  bottom_cm         = "(limite_inf|^profundidade_inferior$|prof_inf|^base)",
  # ---- Munsell (matiz / valor / croma)
  # BDsolos full schema uses "Cor da Amostra Umida - Matiz/Valor/Croma"
  # which normalises to "cor_da_amostra_umida_matiz" etc. Earlier exports
  # used shorter forms.
  munsell_hue_moist    = "(cor_da_amostra_umida_matiz|cor_umida_matiz|matiz_umido|matiz_umida|^matiz$)",
  munsell_value_moist  = "(cor_da_amostra_umida_valor|cor_umida_valor|valor_umido|valor_umida)",
  munsell_chroma_moist = "(cor_da_amostra_umida_croma|cor_umida_croma|croma_umido|croma_umida)",
  munsell_hue_dry      = "(cor_da_amostra_seca_matiz|cor_seca_matiz|matiz_seco|matiz_seca)",
  munsell_value_dry    = "(cor_da_amostra_seca_valor|cor_seca_valor|valor_seco|valor_seca)",
  munsell_chroma_dry   = "(cor_da_amostra_seca_croma|cor_seca_croma|croma_seco|croma_seca)",
  # ---- structure / consistence / clay films ----
  structure_grade   = "(estrutura_grau|grau_estrutura|grau_de_desenvolvimento_1)",
  structure_size    = "(estrutura_tamanho|tamanho_estrutura|^tamanho_1)",
  structure_type    = "(estrutura_tipo|tipo_estrutura|^forma_1)",
  clay_films_amount = "(cerosidade_quantidade|cerosidade_qtd)",
  clay_films_strength = "(cerosidade_grau|grau_cerosidade|cerosidade_grau_de_desenvolvimento)",
  consistence_dry   = "(consistencia_seco|consistencia_seca|grau_de_consistencia_seca)",
  consistence_moist = "(consistencia_umido|consistencia_umida|grau_de_consistencia_umida)",
  # ---- texture (BDsolos g/kg -> soilKey %)
  # Full BDsolos: "Composicao Granulometrica da terra fina - Argila (g/Kg)"
  clay_pct          = "(composicao_granulometrica_da_terra_fina_argila|^argila_g_kg|^argila$|argila_total)",
  silt_pct          = "(composicao_granulometrica_da_terra_fina_silte|^silte_g_kg|^silte$|silte_total)",
  sand_pct          = "(composicao_granulometrica_da_terra_fina_areia_total|^areia_total$|^areia_g_kg|^areia$)",
  coarse_fragments_pct = "(cascalho|^cf$|coarse_frag|fracoes_da_amostra_total_cascalho)",
  # ---- acidity ----
  ph_h2o            = "(^ph_h2o$|ph_em_agua|ph_agua|^ph_water)",
  ph_kcl            = "(^ph_kcl$|ph_em_kcl)",
  ph_cacl2          = "(^ph_cacl2$|ph_em_cacl2)",
  # ---- organics ----
  oc_pct            = "(c_org|^carbono_organico|^oc$|^c$|c_organico)",
  n_total_pct       = "(^nitrogenio_total|^n_total)",
  # ---- exchange complex (full BDsolos: Complexo Sortivo - <X>)
  ca_cmol           = "(complexo_sortivo_calcio|ca_troc|calcio_trocavel|^ca$)",
  mg_cmol           = "(complexo_sortivo_magnesio|mg_troc|magnesio_trocavel|^mg$)",
  k_cmol            = "(complexo_sortivo_potassio|k_troc|potassio_trocavel|^k$)",
  na_cmol           = "(complexo_sortivo_sodio|na_troc|sodio_trocavel|^na$)",
  al_cmol           = "(complexo_sortivo_aluminio_trocavel|al_troc|aluminio_trocavel|^al$)",
  cec_cmol          = "(complexo_sortivo_valor_t|^cec$|^ctc$|capacidade_troca_cationica)",
  bs_pct            = "(complexo_sortivo_valor_v|^v$|saturacao_bases|sat_bases|^bs$)",
  al_sat_pct        = "(complexo_sortivo_saturacao_por_aluminio|saturacao_aluminio|sat_aluminio|^m$)",
  caco3_pct         = "(equivalente_de_carbonato_de_calcio|caco3|carbonato_calcio)",
  p_mehlich3_mg_kg  = "(fosforo_assimilavel|p_assim|^p$|p_mehlich)",
  # ---- physics ----
  bulk_density_g_cm3 = "(densidade_solo_aparente|densidade_solo|densidade_aparente|^ds$|^bd$)",
  # ---- iron / aluminium oxides ----
  fe_dcb_pct        = "(cdb_ferro|ataque_sulfurico_fe2o3|fe2o3|ferro_dcb|fe_dcb)",
  fe_ox_pct         = "(oxalato_de_amonio_ferro)",
  al_ox_pct         = "(oxalato_de_amonio_aluminio)",
  si_ox_pct         = "(oxalato_de_amonio_silica)"
)


#' Site-level columns (BDsolos full export). Mapped at the site, not
#' horizon, level.
#' @keywords internal
.BDSOLOS_SITE_PATTERNS <- list(
  profile_id      = "(^codigo_pa$|^id_perfil$|^profile_id$|^cod_perfil$)",
  profile_id_alt  = "(^numero_pa$)",
  uf              = "(^uf$|^estado$)",
  municipio       = "(^municipio$)",
  altitude_m      = "(^altitude_m$|^altitude$)",
  reference_sibcs = "(^classificacao_atual$|^classificacao$|^taxon_sibcs$|^classe_sibcs$)",
  reference_wrb   = "(^classificacao_fao_wrb$|^classificacao_wrb$|^taxon_wrb$)",
  reference_st    = "(^classificacao_soil_taxonomy$|^taxon_st$|^taxon_soil_taxonomy$)",
  drainage        = "(^classe_de_drenagem$|^drenagem$)",
  parent_material = "(^material_de_origem$|^material_origem$)",
  vegetacao       = "(^uso_atual$|^vegetacao$|^fase_de_vegetacao_primaria$)",
  lat_graus       = "(^latitude_graus$)",
  lat_minutos     = "(^latitude_minutos$)",
  lat_segundos    = "(^latitude_segundos$)",
  lat_hemisferio  = "(^latitude_hemisferio$)",
  lon_graus       = "(^longitude_graus$)",
  lon_minutos     = "(^longitude_minutos$)",
  lon_segundos    = "(^longitude_segundos$)",
  lon_hemisferio  = "(^longitude_hemisferio$)",
  # Direct decimal lat/lon (legacy / FEBR-style exports)
  lat_decimal     = "(^latitude$|^lat$|^coord_y$|^y$)",
  lon_decimal     = "(^longitude$|^lon$|^lng$|^coord_x$|^x$)"
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


# ---- v0.9.58: real BDsolos export support ------------------------------

#' Detect the line where the BDsolos CSV header starts
#'
#' BDsolos exports prepend a 1-line preamble plus an empty line before
#' the actual schema header (a long quoted-string row with hundreds of
#' fields). This walks the first N lines and returns the 1-based index
#' of the header row.
#'
#' @keywords internal
.bdsolos_find_header_line <- function(path, n_probe = 10L) {
  lines <- readLines(path, n = n_probe, encoding = "UTF-8", warn = FALSE)
  if (length(lines) == 0L) return(1L)
  # The header is the first line with the maximum number of fields
  # (separator-agnostic: tries comma + semicolon + tab and picks the
  # most-populous line). Real BDsolos has ~222 fields on the header
  # row; minimal synthetic tests may have only ~5. Either way the
  # header has more fields than the preamble (1-2 fields).
  field_counts <- vapply(lines, function(s) {
    if (!nzchar(s)) return(0L)
    n_semi  <- length(strsplit(s, ";",  fixed = TRUE)[[1L]])
    n_comma <- length(strsplit(s, ",",  fixed = TRUE)[[1L]])
    n_tab   <- length(strsplit(s, "\t", fixed = TRUE)[[1L]])
    max(n_semi, n_comma, n_tab)
  }, integer(1L))
  best <- unname(which.max(field_counts))
  if (length(best) == 0L || field_counts[best] < 2L) return(1L)
  as.integer(best)
}


#' Auto-detect the BDsolos field separator (`,`, `;`, or tab)
#' @keywords internal
.bdsolos_detect_sep <- function(path, header_line = 1L) {
  hdr <- readLines(path, n = header_line, encoding = "UTF-8")[header_line]
  candidates <- c(";" = ";", "," = ",", "\t" = "\t")
  counts <- vapply(candidates,
                     function(s) length(strsplit(hdr, s, fixed = TRUE)[[1L]]),
                     integer(1L))
  names(candidates)[which.max(counts)]
}


#' Convert BDsolos coords (graus / minutos / segundos / hemisferio) to decimal
#'
#' @keywords internal
.bdsolos_dms_to_decimal <- function(graus, minutos, segundos, hemisferio) {
  g <- suppressWarnings(as.numeric(graus))
  m <- suppressWarnings(as.numeric(minutos))
  s <- suppressWarnings(as.numeric(segundos))
  if (is.na(g)) return(NA_real_)
  if (is.na(m)) m <- 0
  if (is.na(s)) s <- 0
  dec <- g + m / 60 + s / 3600
  hem <- toupper(trimws(as.character(hemisferio)))
  if (length(hem) == 1L && nzchar(hem) && hem %in% c("S", "W", "O")) {
    dec <- -dec
  }
  dec
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
inspect_bdsolos_csv <- function(path, sep = NULL) {
  if (!file.exists(path)) {
    stop(sprintf("inspect_bdsolos_csv(): file not found: %s", path))
  }
  hdr_line <- .bdsolos_find_header_line(path)
  if (is.null(sep) || !nzchar(sep)) {
    sep <- .bdsolos_detect_sep(path, header_line = hdr_line)
  }
  hdr <- readLines(path, n = hdr_line, encoding = "UTF-8")[hdr_line]
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
  cli::cli_alert_info(sprintf("Header line: %d   separator: %s",
                                hdr_line,
                                if (sep == "\t") "TAB" else sep))
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
load_bdsolos_csv <- function(path, sep = NULL, verbose = TRUE) {
  if (!file.exists(path)) {
    stop(sprintf("load_bdsolos_csv(): file not found: %s", path))
  }
  hdr_line <- .bdsolos_find_header_line(path)
  if (is.null(sep) || !nzchar(sep)) {
    sep <- .bdsolos_detect_sep(path, header_line = hdr_line)
  }
  skip <- max(0L, hdr_line - 1L)
  # data.table::fread is fast but strict; ~25% of real BDsolos UF
  # exports (DF, MT, PA, PB, PE, RN, SP in the May 2026 audit)
  # contain malformed UTF-8 sequences that trip fread with
  # "attempt to set index N/N in SET_STRING_ELT". Fall back to
  # base R utils::read.csv2 (much slower, much more lenient) when
  # fread errors out.
  d <- tryCatch(
    suppressWarnings(suppressMessages(
      data.table::fread(path, sep = sep, encoding = "UTF-8",
                          skip = skip, header = TRUE,
                          fill = TRUE, blank.lines.skip = TRUE)
    )),
    error = function(e) NULL
  )
  if (is.null(d) || nrow(d) == 0L) {
    if (isTRUE(verbose)) {
      cli::cli_alert_info(
        "fread failed on this file; falling back to utils::read.csv2 (slower).")
    }
    d <- tryCatch(
      data.table::as.data.table(
        utils::read.csv2(path, skip = skip, header = TRUE,
                          fileEncoding = "UTF-8",
                          stringsAsFactors = FALSE,
                          sep = sep,
                          na.strings = c("", "NA", "n.d.", "ND"))
      ),
      error = function(e) {
        stop(sprintf(
          "load_bdsolos_csv(): both fread and read.csv2 failed on '%s': %s",
          path, conditionMessage(e)
        ))
      }
    )
  }
  if (is.null(d) || nrow(d) == 0L) {
    stop("load_bdsolos_csv(): CSV is empty.")
  }

  # Map columns: build normalised lookup once
  raw_names <- names(d)
  norm_names <- vapply(raw_names, .bdsolos_norm, character(1L))
  norm_to_raw <- setNames(raw_names, norm_names)

  # Site-level column resolution
  pick_site <- function(pattern) {
    hits <- names(norm_to_raw)[grepl(pattern, names(norm_to_raw),
                                        ignore.case = TRUE, perl = TRUE)]
    if (length(hits) == 0L) return(NA_character_)
    norm_to_raw[[hits[1L]]]
  }
  site_cols <- lapply(.BDSOLOS_SITE_PATTERNS, pick_site)

  # Horizon-level column mapping
  sk_map <- character(0)
  for (i in seq_along(raw_names)) {
    sk <- .bdsolos_match_column(raw_names[i])
    if (!is.na(sk) && !(sk %in% sk_map)) sk_map[raw_names[i]] <- sk
  }
  # Taxon column fallback (when classificacao_atual is absent)
  taxon_col <- site_cols$reference_sibcs
  if (is.na(taxon_col)) {
    for (raw in raw_names) {
      tax <- .bdsolos_match_taxon_column(raw)
      if (!is.na(tax)) { taxon_col <- raw; break }
    }
  }
  # Profile id: prefer codigo_pa, fall back to numero_pa or first column
  id_col <- site_cols$profile_id
  if (is.na(id_col)) id_col <- site_cols$profile_id_alt
  if (is.na(id_col)) id_col <- raw_names[1L]

  ids <- as.character(d[[id_col]])
  uids <- unique(ids[!is.na(ids) & nzchar(ids)])
  out <- vector("list", length(uids))
  for (k in seq_along(uids)) {
    rid <- uids[k]
    # Use %in% (returns FALSE for NA) rather than == (returns NA)
    # to avoid data.table's NA-row inclusion via NA index.
    rows <- d[ids %in% rid, ]
    if (nrow(rows) == 0L) next
    hz <- .bdsolos_rows_to_horizons(rows, sk_map)
    # Coords: prefer direct decimal lat/lon when present, fall back to
    # graus / minutos / segundos / hemisferio (BDsolos full schema).
    lat <- NA_real_; lon <- NA_real_
    if (!is.na(site_cols$lat_decimal)) {
      lat <- suppressWarnings(as.numeric(rows[[site_cols$lat_decimal]][1L]))
    }
    if (!is.na(site_cols$lon_decimal)) {
      lon <- suppressWarnings(as.numeric(rows[[site_cols$lon_decimal]][1L]))
    }
    if (!is.finite(lat) && !is.na(site_cols$lat_graus)) {
      lat <- .bdsolos_dms_to_decimal(
        rows[[site_cols$lat_graus]][1L],
        if (!is.na(site_cols$lat_minutos))    rows[[site_cols$lat_minutos]][1L]    else 0,
        if (!is.na(site_cols$lat_segundos))   rows[[site_cols$lat_segundos]][1L]   else 0,
        if (!is.na(site_cols$lat_hemisferio)) rows[[site_cols$lat_hemisferio]][1L] else "")
    }
    if (!is.finite(lon) && !is.na(site_cols$lon_graus)) {
      lon <- .bdsolos_dms_to_decimal(
        rows[[site_cols$lon_graus]][1L],
        if (!is.na(site_cols$lon_minutos))    rows[[site_cols$lon_minutos]][1L]    else 0,
        if (!is.na(site_cols$lon_segundos))   rows[[site_cols$lon_segundos]][1L]   else 0,
        if (!is.na(site_cols$lon_hemisferio)) rows[[site_cols$lon_hemisferio]][1L] else "")
    }
    safe_field <- function(col_name) {
      if (is.na(col_name)) return(NA_character_)
      v <- as.character(rows[[col_name]][1L])
      if (length(v) == 0L || is.na(v) || !nzchar(trimws(v))) NA_character_
      else trimws(v)
    }
    site <- list(
      id      = rid,
      lat     = lat,
      lon     = lon,
      country = "BR",
      state           = safe_field(site_cols$uf),
      municipality    = safe_field(site_cols$municipio),
      altitude_m      = suppressWarnings(as.numeric(safe_field(site_cols$altitude_m))),
      reference_sibcs = if (!is.na(taxon_col)) safe_field(taxon_col) else NA_character_,
      reference_wrb   = safe_field(site_cols$reference_wrb),
      reference_st    = safe_field(site_cols$reference_st),
      drainage        = safe_field(site_cols$drainage),
      parent_material = safe_field(site_cols$parent_material),
      land_cover      = safe_field(site_cols$vegetacao),
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
    n_with_coords <- sum(vapply(out, function(p) {
      isTRUE(is.finite(p$site$lat)) && isTRUE(is.finite(p$site$lon))
    }, logical(1L)))
    cli::cli_alert_success(sprintf(
      "load_bdsolos_csv(): %d perfis (Munsell em %d, taxon em %d, coords em %d)",
      length(out), n_with_munsell, n_with_taxon, n_with_coords
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
            grepl("^(argila|silte|areia)|composicao_granulometrica.*?(argila|silte|areia)|.*g_kg$",
                    raw_norm)) {
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
  # Bump per-CDP-command timeout for resilience against the slow
  # Embrapa server. chromote's default is ~5s for Runtime.evaluate;
  # individual evals here are quick (the heavy work is deferred via
  # setTimeout) but the term-acceptance redirect / SPA bootstrap can
  # block briefly.
  Sys.setenv(CHROMOTE_TIMEOUT = as.character(max(60L, as.integer(timeout_seconds))))

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

  # Submit Etapa 2 -> server-side query.
  # realizaBusca() fires a synchronous-ish AJAX that can take minutes
  # on the Embrapa server. chromote's default Runtime.evaluate timeout
  # (~5-10s) cannot wait that long, so we DEFER the call via
  # setTimeout(0) -- the JS frame returns immediately, the AJAX runs
  # in the background, and we then poll the DOM for Etapa 3 from R.
  if (isTRUE(verbose)) {
    cli::cli_alert_info("Submitting Etapa 2 (server-side query, may take minutes)...")
  }
  tryCatch(
    .bdsolos_eval(chromote_session, "
      setTimeout(function() {
        if (typeof realizaBusca === 'function') realizaBusca();
      }, 0);
      'fired';
    "),
    error = function(e) {
      # If the eval itself times out, the AJAX is likely still running
      # in the background -- continue to the polling loop.
      if (isTRUE(verbose)) {
        cli::cli_alert_warning(sprintf(
          "Eval reported timeout (%s); will poll DOM for Etapa 3 anyway.",
          conditionMessage(e)
        ))
      }
      invisible(NULL)
    }
  )

  # Wait for Etapa 3 to materialise. Each polling probe is a tiny eval
  # so it should never trip the chromote timeout.
  parsed <- list(has_e3 = FALSE, has_csv_radio = FALSE)
  start <- Sys.time()
  poll_count <- 0L
  while (as.numeric(difftime(Sys.time(), start, units = "secs")) < timeout_seconds) {
    Sys.sleep(5)
    poll_count <- poll_count + 1L
    state <- tryCatch(
      .bdsolos_eval(chromote_session, "
        JSON.stringify({
          has_e3: document.body.innerText.indexOf('ETAPA 3') >= 0 ||
                  document.body.innerText.indexOf('Etapa 3') >= 0,
          has_csv_radio: document.querySelectorAll('input[type=radio][value=csv]').length > 0,
          loading: /aguarde|carregando|processando/i.test(document.body.innerText)
        });
      "),
      error = function(e) NULL
    )
    if (is.null(state)) next
    parsed <- tryCatch(jsonlite::fromJSON(state),
                        error = function(e) list(has_e3 = FALSE,
                                                  has_csv_radio = FALSE))
    if (isTRUE(verbose) && poll_count %% 6L == 0L) {
      cli::cli_alert_info(sprintf(
        "  ... polling Etapa 3 (%ds elapsed, has_e3 = %s, loading = %s)",
        round(as.numeric(difftime(Sys.time(), start, units = "secs"))),
        isTRUE(parsed$has_e3),
        isTRUE(parsed$loading)
      ))
    }
    if (isTRUE(parsed$has_e3) && isTRUE(parsed$has_csv_radio)) break
  }
  if (!(isTRUE(parsed$has_e3) && isTRUE(parsed$has_csv_radio))) {
    stop("download_bdsolos(): server query timed out after ",
         timeout_seconds, "s polling for Etapa 3. ",
         "Try a smaller filter (filter_uf = '...') or increase ",
         "timeout_seconds. The Embrapa server is often slow under load.")
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
