# =============================================================================
# Embrapa FEBR loader (Free Brazilian Repository of soil data)
#
# The Embrapa BDsolos / FEBR snapshot ships as two semicolon-separated
# UTF-8 files with comma decimal:
#   febr-observacao.txt    -- one row per profile (site + classification)
#   febr-superconjunto.txt -- one row per (profile, layer); also carries
#                             the profile-level classification on every
#                             row (denormalised). We use the latter
#                             because it lets us load camada-level data
#                             in a single pass.
#
# The taxon columns are taxon_sibcs / taxon_st / taxon_wrb. SiBCS names
# in this archive use ALL-CAPS Portuguese (e.g. "LATOSSOLO VERMELHO",
# "NEOSSOLO LITOLICO") -- distinct from our classify_sibcs() output
# which uses Title Case plural ("Latossolos Vermelhos"). Normalisation
# is applied at benchmark time.
# =============================================================================


#' Load the Embrapa FEBR superconjunto into a list of PedonRecords
#'
#' Reads the FEBR \code{febr-superconjunto.txt} export (one row per
#' camada / horizon, with the profile-level classification denormalised
#' onto every row), groups rows by \code{(dataset_id, observacao_id)},
#' and returns a list of \code{\link{PedonRecord}} objects with all
#' three reference taxa attached on \code{$site}: \code{reference_sibcs}
#' (raw FEBR string, e.g. "LATOSSOLO VERMELHO"),
#' \code{reference_usda}, \code{reference_wrb}.
#'
#' Drops profiles whose \code{taxon_sibcs} is NA (no usable reference).
#' Drops camadas with no horizon-depth information (no
#' \code{profund_sup} / \code{profund_inf}).
#'
#' @param path Path to \code{febr-superconjunto.txt}.
#' @param head Optional integer; if not \code{NULL}, return only the
#'        first \code{head} unique profiles after grouping.
#' @param require_classification One of \code{c("any", "sibcs", "wrb",
#'        "usda")}. Default \code{"sibcs"}: drop profiles whose
#'        chosen classification is NA. \code{"any"} keeps profiles
#'        with at least one of the three.
#' @param verbose If \code{TRUE} (default), summarises the load.
#' @return A list of \code{\link{PedonRecord}} objects.
#' @export
load_febr_pedons <- function(path,
                                head                   = NULL,
                                require_classification = c("sibcs", "any",
                                                           "wrb",  "usda"),
                                verbose                = TRUE) {
  require_classification <- match.arg(require_classification)
  if (!file.exists(path)) stop(sprintf("FEBR file not found: %s", path))

  d <- data.table::fread(path, sep = ";", dec = ",",
                            encoding = "UTF-8",
                            na.strings = c("", "NA"))

  # Filter by classification availability up-front.
  has_taxon <- function(col) !is.na(d[[col]])
  keep_mask <- switch(require_classification,
    any   = has_taxon("taxon_sibcs") | has_taxon("taxon_wrb") |
              has_taxon("taxon_st"),
    sibcs = has_taxon("taxon_sibcs"),
    wrb   = has_taxon("taxon_wrb"),
    usda  = has_taxon("taxon_st")
  )
  d <- d[keep_mask, ]

  # Group by (dataset_id, observacao_id).
  d$.profile_key <- paste(d$dataset_id, d$observacao_id, sep = "/")
  ids <- unique(d$.profile_key)
  if (!is.null(head)) ids <- utils::head(ids, n = head)

  out <- vector("list", length(ids))
  for (i in seq_along(ids)) {
    rid <- ids[i]
    layers <- d[d$.profile_key == rid, ]
    if (nrow(layers) == 0L) next

    # Drop layers without depth.
    layers <- layers[!is.na(layers$profund_sup) & !is.na(layers$profund_inf), ]
    if (nrow(layers) == 0L) next

    # Order by top depth (camada_id is sometimes a label, not numeric).
    layers <- layers[order(layers$profund_sup), ]

    hz <- data.table::data.table(
      top_cm      = as.numeric(layers$profund_sup),
      bottom_cm   = as.numeric(layers$profund_inf),
      designation = as.character(layers$camada_nome),
      clay_pct    = as.numeric(layers$argila)  / 10,   # FEBR g/kg -> %
      silt_pct    = as.numeric(layers$silte)   / 10,
      sand_pct    = as.numeric(layers$areia)   / 10,
      ph_h2o      = as.numeric(layers$ph),
      oc_pct      = as.numeric(layers$carbono) / 10,
      cec_cmol    = as.numeric(layers$ctc),
      bulk_density_g_cm3 = as.numeric(layers$dsi),
      ec_dS_m     = as.numeric(layers$ce)
    )
    hz <- ensure_horizon_schema(hz)

    p <- layers[1, ]
    out[[i]] <- PedonRecord$new(
      site = list(
        id              = rid,
        lat             = .febr_num(p$coord_y),
        lon             = .febr_num(p$coord_x),
        country         = p$pais_id %||% "BR",
        date            = p$observacao_data,
        parent_material = NA_character_,
        reference_sibcs  = p$taxon_sibcs %||% NA_character_,
        reference_wrb    = p$taxon_wrb   %||% NA_character_,
        reference_usda   = p$taxon_st    %||% NA_character_,
        reference_source = "Embrapa FEBR / BDsolos"
      ),
      horizons = hz
    )
  }
  out <- out[!vapply(out, is.null, logical(1))]
  if (isTRUE(verbose))
    cli::cli_alert_success("FEBR: loaded {.val {length(out)}} pedons (require_classification = {.field {require_classification}})")
  out
}


# Internal: parse FEBR coordinates which may be numeric, strings with
# comma decimal, or already-numeric. Falls back to NA on failure.
.febr_num <- function(x) {
  if (is.null(x)) return(NA_real_)
  if (is.numeric(x)) return(x[1])
  v <- suppressWarnings(as.numeric(gsub(",", ".", x[1])))
  v
}


#' Pre-2018 SiBCS Order names -> SiBCS 5a edicao plural Title Case map
#'
#' Internal lookup applied by \code{normalise_febr_sibcs()} when
#' \code{level = "order"}. BDsolos exports collected before the SiBCS
#' 5a edicao (2018) carry historical Order names that the modern
#' classifier does not emit.
#'
#' BDsolos exports collected before the SiBCS 5a edicao (2018) carry
#' historical Order names that the modern classifier does not emit.
#' The most common cases observed on RJ.csv (722 perfis):
#'
#' \itemize{
#'   \item \code{Podzolicos} (54 perfis em RJ) -> \code{Argissolos}
#'         (post-2018 a Order Argissolos absorveu o Podzolico Vermelho-
#'         Amarelo, Podzolico Vermelho-Escuro, etc.)
#'   \item \code{Gleis}      (44 perfis em RJ) -> \code{Gleissolos}
#'         (Gleis Humico, Gleis Pouco Humico colapsaram em Gleissolos)
#'   \item \code{Aluviais}   (13 perfis em RJ) -> \code{Neossolos}
#'         (Solos Aluviais foram reclassificados para Neossolos
#'         Fluvicos no SiBCS 5a edicao, mas a normalisacao aqui
#'         emite apenas a Ordem moderna \code{Neossolos} -- a
#'         Subordem \code{Neossolos Fluvicos} nao eh recuperavel
#'         do label legado antigo \code{ALUVIAIS} (a granularidade
#'         de Subordem se perde). Para benchmark Order-level isso e
#'         suficiente; para Subordem o legado nao se mapeia.)
#'   \item \code{Solos}      -> \code{NA}
#'         ("Solos Halomorficos", "Solos Hidromorficos", e fragmentos
#'         de label do UI antigo do BDsolos onde a Ordem nao foi
#'         registrada). NA aqui significa "fora de scope para a
#'         comparacao".
#' }
#'
#' Aplicado em \code{normalise_febr_sibcs(level = "order")} apos a
#' pluralisacao normal. Para subordem o legacy mapping ainda nao e
#' aplicado (ver TODO no v0.9.61: estender para Subordem com
#' "Podzolico Vermelho-Amarelo" -> "Argissolos Vermelho-Amarelos").
#'
#' @keywords internal
.SIBCS_LEGACY_ORDER_MAP <- c(
  "Podzolicos" = "Argissolos",
  "Gleis"      = "Gleissolos",
  "Aluviais"   = "Neossolos",
  "Solos"      = NA_character_
)


#' Canonicalise FEBR SiBCS names to match soilKey rule outputs.
#'
#' FEBR ships SiBCS labels in mixed legacy/modern form
#' (\code{"Podzolicos"} for old name of Argissolos, singular vs plural,
#' Portuguese accents). This helper folds them to the form produced by
#' \code{run_sibcs_key()} so that benchmark accuracies can be computed
#' without false negatives.
#'
#' @param x Character vector of FEBR SiBCS names.
#' @param level One of \code{"order"} (default) or \code{"subordem"}.
#' @return Character vector of normalised SiBCS names; \code{NA} for
#'   labels that are out-of-scope for the comparison
#'   (e.g.\ legacy \code{"Solos"} category).
#' @seealso \code{\link{normalise_febr_wrb}}, \code{\link{normalise_febr_usda}}
#' @export
normalise_febr_sibcs <- function(x, level = c("order", "subordem")) {
  level <- match.arg(level)
  if (length(x) == 0L) return(character(0))
  s <- toupper(as.character(x))
  # Strip Portuguese accents to ASCII (CRAN portability requires
  # \uXXXX escapes for non-ASCII source characters).
  s <- gsub("\u00C1|\u00C0|\u00C2|\u00C3", "A", s)  # A acute / grave / circumflex / tilde
  s <- gsub("\u00C9|\u00CA",                "E", s)  # E acute / circumflex
  s <- gsub("\u00CD",                       "I", s)  # I acute
  s <- gsub("\u00D3|\u00D4|\u00D5",         "O", s)  # O acute / circumflex / tilde
  s <- gsub("\u00DA",                       "U", s)  # U acute
  s <- gsub("\u00C7",                       "C", s)  # C cedilla

  pluralise <- function(word) {
    if (is.na(word) || !nzchar(word)) return(NA_character_)
    last <- substr(word, nchar(word), nchar(word))
    if (last == "S") return(word)
    if (last == "L") return(paste0(substr(word, 1, nchar(word)-1), "IS"))
    if (last == "M") return(paste0(substr(word, 1, nchar(word)-1), "NS"))
    paste0(word, "S")
  }
  title <- function(word) {
    if (is.na(word)) return(NA_character_)
    paste0(substr(word, 1, 1),
            tolower(substr(word, 2, nchar(word))))
  }

  # v0.9.60: legacy -> modern Order map. Applied AFTER the normal
  # pluralise + title pipeline so the comparison key matches what
  # `classify_sibcs()$rsg_or_order` emits today.
  apply_legacy_map <- function(ordem_titled) {
    if (is.na(ordem_titled)) return(NA_character_)
    if (ordem_titled %in% names(.SIBCS_LEGACY_ORDER_MAP))
      return(unname(.SIBCS_LEGACY_ORDER_MAP[ordem_titled]))
    ordem_titled
  }

  out <- character(length(s))
  for (i in seq_along(s)) {
    if (is.na(s[i])) { out[i] <- NA_character_; next }
    parts <- strsplit(s[i], "\\s+", fixed = FALSE)[[1]]
    ordem <- pluralise(parts[1])
    ordem_t <- title(ordem)
    ordem_t <- apply_legacy_map(ordem_t)
    # If the Order itself is out-of-scope (legacy map -> NA, e.g.
    # "SOLOS HALOMORFICOS"), the entire output is NA, not "NA <sub>".
    if (is.na(ordem_t)) { out[i] <- NA_character_; next }
    if (level == "order" || length(parts) < 2L) {
      out[i] <- ordem_t
    } else {
      sub <- pluralise(parts[2])
      out[i] <- paste(ordem_t, title(sub))
    }
  }
  out
}


#' Normalise FEBR USDA taxon strings to USDA Soil Taxonomy Order
#'
#' FEBR ships USDA Soil Taxonomy labels at the subgroup or great-group
#' granularity (e.g. "TYPIC HAPLUDULT", "ACRUSTOX"). The suffix of the
#' final word encodes the Order: \code{...OX} -> Oxisols, \code{...ULT}
#' -> Ultisols, \code{...EPT} -> Inceptisols, etc. This helper extracts
#' the Order from the suffix so the benchmark can compare against
#' \code{classify_usda()$rsg_or_order} at \code{level = "order"}.
#'
#' @param x Character vector of FEBR USDA names.
#' @return Character vector of normalised Order names ("Oxisols",
#'         "Ultisols", "Inceptisols", ...).
#' @export
normalise_febr_usda <- function(x) {
  if (length(x) == 0L) return(character(0))
  s <- toupper(as.character(x))
  s <- gsub("\\s*\\(.*$", "", s)
  out <- character(length(s))
  # Order from suffix of the LAST word.
  for (i in seq_along(s)) {
    if (is.na(s[i])) { out[i] <- NA_character_; next }
    parts <- strsplit(trimws(s[i]), "\\s+", fixed = FALSE)[[1]]
    if (length(parts) == 0L) { out[i] <- NA_character_; next }
    last <- parts[length(parts)]
    if (is.na(last) || !nzchar(last)) { out[i] <- NA_character_; next }
    last <- gsub("S$", "", last)  # singular
    out[i] <- if (grepl("OX$", last))   "Oxisols"
              else if (grepl("ULT$", last)) "Ultisols"
              else if (grepl("AND$", last)) "Andisols"
              else if (grepl("EPT$", last)) "Inceptisols"
              else if (grepl("ALF$", last)) "Alfisols"
              else if (grepl("OLL$", last)) "Mollisols"
              else if (grepl("OD$",  last)) "Spodosols"
              else if (grepl("ERT$", last)) "Vertisols"
              else if (grepl("IST$", last)) "Histosols"
              else if (grepl("ENT$", last)) "Entisols"
              else if (grepl("EL$",  last)) "Gelisols"
              else if (grepl("ID$",  last)) "Aridisols"
              else NA_character_
  }
  out
}


#' Normalise FEBR WRB taxon strings to RSG-only
#'
#' FEBR ships WRB names with full qualifier strings, e.g.
#' "HUMIC FERRALSOL", "HAPLIC ACRISOL (ALUMIC, HYPERDYSTRIC, ...)".
#' The trailing word (before any qualifier parens) is the RSG.
#' This helper extracts and normalises it to soilKey's plural Title
#' Case form ("Ferralsols", "Acrisols"), matching
#' \code{ClassificationResult$rsg_or_order}.
#'
#' @param x Character vector of FEBR WRB names.
#' @return Character vector of normalised RSG names.
#' @export
normalise_febr_wrb <- function(x) {
  if (length(x) == 0L) return(character(0))
  s <- toupper(as.character(x))
  # Strip qualifiers in parens.
  s <- gsub("\\s*\\(.*$", "", s)
  out <- character(length(s))
  # v0.9.18: legacy <-> canonical RSG spelling map. FEBR uses pre-2014
  # RSG names ("NITOSOL", "GREYZEM", "AGRISOL", ...) that the WRB 2022
  # 4th edition has merged or renamed (NITISOL, PHAEOZEM, ACRISOL, ...).
  # Canonicalising here lets the benchmark compare apples to apples.
  legacy_map <- c(
    "NITOSOL"           = "Nitisols",
    "NITOSOLS"          = "Nitisols",
    "GREYZEM"           = "Phaeozems",
    "GREYZEMS"          = "Phaeozems",
    "CREYZEM"           = "Phaeozems",
    "CREYZEMS"          = "Phaeozems",
    "AGRISOL"           = "Acrisols",
    "AGRISOLS"          = "Acrisols",
    "ARGISOL"           = "Acrisols",
    "ARGISOLS"          = "Acrisols",
    "LUVISSOL"          = "Luvisols",
    "LUVISSOLS"         = "Luvisols",
    "SOLONETZS"         = "Solonetz",
    "VERMELHO-AMARELO"  = NA_character_,
    "VERMELHO-AMARELOS" = NA_character_,
    "NATRAQUOLL"        = "Solonetz",
    "NATRAQUOLLS"       = "Solonetz"
  )
  for (i in seq_along(s)) {
    if (is.na(s[i])) { out[i] <- NA_character_; next }
    parts <- strsplit(trimws(s[i]), "\\s+", fixed = FALSE)[[1]]
    if (length(parts) == 0L) { out[i] <- NA_character_; next }
    last <- parts[length(parts)]
    if (last %in% names(legacy_map)) {
      out[i] <- legacy_map[[last]]
      next
    }
    rsg <- gsub("S$", "", last)
    out[i] <- paste0(substr(rsg, 1, 1),
                       tolower(substr(rsg, 2, nchar(rsg))),
                       "s")
  }
  out
}
