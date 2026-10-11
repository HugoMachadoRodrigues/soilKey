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
#' Since v0.9.226 the map also holds the names of the classifications in use
#' before SiBCS that the 5th edition (Santos et al., 2018) gives to one
#' order, by the "Abrangencia" paragraph of each order in its chapter 3
#' (p. 87-106) and its Anexo I (p. 342): Latosol, Planosol,
#' Solonetz-Solodizado, Brunizem, Rendzina, Regossolo, Litosol, Solos
#' Litolicos, Solos Organicos, Vertisol, Cambisol, Solos Concrecionarios and
#' Rubrozem, with the misspellings of them and of the present orders found in
#' the national BDsolos export. Names that take two words are in
#' \code{.SIBCS_LEGACY_TWO_WORD_MAP}.
#'
#' Left out on purpose, so that their labels stay \code{NA}: names the 5th
#' edition divides between orders by something the name does not say
#' (Hidromorfico Cinzento: Gleissolos without an abrupt textural change,
#' Planossolos with one; Solonchak: Gleissolos only with a glei horizon;
#' Alissolo), and names that are not in its correlation (Lateritico Bruno
#' Avermelhado, which is not Laterita Hidromorfica; Solos Brunos Acidos; Solos
#' Coluviais; Mediterraneo Vermelho-Amarelo).
#'
#' The key is the first word of the label, pluralised and in Title Case, as
#' \code{normalise_febr_sibcs()} forms it ("LATOSOL" is "Latosois", "BRUNIZEM"
#' "Brunizens", "SOLONETZ" "Solonetzs").
#'
#' @keywords internal
.SIBCS_LEGACY_ORDER_MAP <- c(
  # legacy / old-nomenclature names
  "Podzolicos"    = "Argissolos",    # Podzolico Vermelho-Amarelo (old)
  "Podzois"       = "Espodossolos",  # Podzol (NB: distinct from Podzolico above)
  "Gleis"         = "Gleissolos",
  "Gleys"         = "Gleissolos",
  "Aluviais"      = "Neossolos",     # Solos Aluviais (old)
  "Areias"        = "Neossolos",     # Areias Quartzosas -> Neossolos Quartzarenicos
  "Lateritas"     = "Plintossolos",  # Laterita Hidromorfica -> plinthite
  # v0.9.187 -- dense national archives (BDsolos) carry OCR/legacy typos that
  # were scored as misses purely because the reference string was malformed.
  "Latosolos"     = "Latossolos",    # single-S typo
  "Gleiossolos"   = "Gleissolos",    # extra-i typo
  "Cambissoos"    = "Cambissolos",   # dropped-L typo
  "Cambissolicos" = "Cambissolos",
  "Esposdossolos" = "Espodossolos",  # transposition typo
  # v0.9.226 -- names of the classifications in use before SiBCS that the 5th
  # edition gives to one order (chapter 3, "Abrangencia" of each order,
  # p. 87-106, and Anexo I, p. 342), in the spellings of the BDsolos export.
  "Latosois"        = "Latossolos",    # Latosol; "todos os antigos Latossolos" (p. 95)
  "Latossois"       = "Latossolos",    # Latossol
  "Planosois"       = "Planossolos",   # Planosol (p. 102)
  "Planossois"      = "Planossolos",   # Planossol
  "Solonetzs"       = "Planossolos",   # Solonetz-Solodizado (p. 102)
  "Brunizens"       = "Chernossolos",  # Brunizem, Brunizem Avermelhado (p. 90)
  "Rendzinas"       = "Chernossolos",  # Rendzina (p. 90)
  "Regossolos"      = "Neossolos",     # Regossolo (p. 97)
  "Regosois"        = "Neossolos",     # Regosol
  "Litosois"        = "Neossolos",     # Litosol, Litossolo (p. 97)
  "Litolicos"       = "Neossolos",     # (Solos) Litolicos (p. 97)
  "Organicos"       = "Organossolos",  # (Solos) Organicos (p. 101)
  "Vertisois"       = "Vertissolos",   # Vertisol (p. 106)
  "Cambisois"       = "Cambissolos",   # Cambisol (p. 89)
  "Concrecionarios" = "Plintossolos",  # (Solos) Concrecionarios, Concrecionarios
                                       # Lateriticos (p. 104)
  "Rubrozens"       = "Argissolos",    # Rubrozem (Anexo I)
  # v0.9.226 -- misspellings, of the names above and of the present orders
  "Planosolos"      = "Planossolos",
  "Planossos"       = "Planossolos",
  "Bruizens"        = "Chernossolos",  # Brunizem
  "Chenossolos"     = "Chernossolos",
  "Liticos"         = "Neossolos",     # Litolico
  "Litolifos"       = "Neossolos",     # Litolico
  "Are1as"          = "Neossolos",     # Areia Quartzosa
  "Neo0ssolos"      = "Neossolos",
  "Neossolis"       = "Neossolos",
  "Neosssolos"      = "Neossolos",
  "Vertisolos"      = "Vertissolos",
  "Cambisolos"      = "Cambissolos",
  "Concrecionerios" = "Plintossolos",
  "Laterias"        = "Plintossolos",  # Laterita Hidromorfica
  "Lateriras"       = "Plintossolos",  # Laterita Hidromorfica
  "Plitossolos"     = "Plintossolos",
  "Pozdolicos"      = "Argissolos",    # Podzolico, as "Podzolicos" above
  "Pozolicos"       = "Argissolos",
  "Podzolcios"      = "Argissolos",
  "Podzolicas"      = "Argissolos",
  "Podzoucos"       = "Argissolos",
  "Argisslos"       = "Argissolos",
  "Argilossolos"    = "Argissolos",
  "Nitssolos"       = "Nitossolos",
  "Gelissolos"      = "Gleissolos",    # "Gelissolo Melanico"
  "Solos"         = NA_character_    # "Solo(s)" is not a name: old_name() in
                                     # normalise_febr_sibcs() reads what follows
)


#' Old soil names that take two words to tell apart
#'
#' Consulted by \code{normalise_febr_sibcs()} before
#' \code{.SIBCS_LEGACY_ORDER_MAP} when the first word of a label gives no
#' order. The key is the first two words of the name, upper case, without
#' accents, as written (a leading "Solo" or "Solos" is not counted, and a
#' hyphen separates words as a space does). One word would not do: "Bruno Nao
#' Calcico" is Luvissolos and "Solos Brunos Acidos" has no order in the
#' correlation of the 5th edition; "Terra" and "Humic" are not names.
#'
#' \itemize{
#'   \item \code{Terra Roxa} and \code{Terra Bruna} (Estruturada, Estruturada
#'         Similar) -> \code{Nitossolos}: "na maioria" (SiBCS 2018, p. 98). A
#'         "pequena parte", with the texture gradient of a B textural, went to
#'         Argissolos (p. 87); the name does not say which, and the label is
#'         read as the majority.
#'   \item \code{Bruno Nao Calcico} -> \code{Luvissolos} (p. 95-96, Anexo I).
#'   \item \code{Humic Glei} and \code{Low-Humic Glei} (the English names of
#'         Glei Humico and Glei Pouco Humico) -> \code{Gleissolos} (p. 93).
#'   \item \code{Solos Indiscriminados Concrecionarios} ->
#'         \code{Plintossolos} (p. 104).
#' }
#'
#' @keywords internal
.SIBCS_LEGACY_TWO_WORD_MAP <- c(
  "TERRA ROXA"                      = "Nitossolos",
  "TERRA BRUNA"                     = "Nitossolos",
  "BRUNO NAO"                       = "Luvissolos",
  "BRUNOS NAO"                      = "Luvissolos",
  "HUMIC GLEI"                      = "Gleissolos",
  "LOW HUMIC"                       = "Gleissolos",
  "INDISCRIMINADOS CONCRECIONARIOS" = "Plintossolos",
  "INDISCRIMONADOS CONCRECIONARIOS" = "Plintossolos"   # misspelling
)


#' The orders of SiBCS 5 (2018)
#'
#' As \code{classify_sibcs()$rsg_or_order} names them. A reference label that
#' is none of these after \code{.SIBCS_LEGACY_ORDER_MAP} has no class the key
#' can return, so \code{\link{normalise_febr_sibcs}} gives \code{NA} for it.
#'
#' @keywords internal
.SIBCS_ORDERS <- c(
  "Argissolos", "Cambissolos", "Chernossolos", "Espodossolos", "Gleissolos",
  "Latossolos", "Luvissolos", "Neossolos", "Nitossolos", "Organossolos",
  "Planossolos", "Plintossolos", "Vertissolos"
)


#' Canonicalise FEBR SiBCS names to match soilKey rule outputs.
#'
#' FEBR ships SiBCS labels in mixed legacy/modern form
#' (\code{"Podzolicos"} for old name of Argissolos, singular vs plural,
#' Portuguese accents). This helper folds them to the form produced by
#' \code{run_sibcs_key()} so that benchmark accuracies can be computed
#' without false negatives.
#'
#' A label is read by its first word, with any punctuation around it dropped
#' (\code{"ESPODOSSOLO."} is Espodossolos). Since v0.9.225 the result must be
#' one of the 13 orders of SiBCS 5 after the legacy map; otherwise it is
#' \code{NA}. Until then the first word came out whatever it was, so
#' \code{"CLASSIFICACAO ? CAMBISSOLO ..."} became the class "Classificacaos",
#' \code{"ALISSOLO CROMICO"} "Alissolos" and \code{"LATOSOL AMARELO"}
#' "Latosois": labels no prediction can equal, which the benchmarks counted
#' as scored profiles and as errors (631 of the 7,694 labels of the national
#' BDsolos export, in 79 forms).
#'
#' Since v0.9.226, when the first word gives no order the label is read once
#' more as an old name: a leading "Solo" or "Solos" is set aside ("SOLO
#' LITOLICO" is Neossolos, "SOLOS ORGANICOS" Organossolos), a hyphen separates
#' words as a space does ("LATOSSOLO-VERMELHO", "SOLONETZ-SOLODIZADO"), and the
#' names of \code{.SIBCS_LEGACY_TWO_WORD_MAP} ("TERRA ROXA ESTRUTURADA",
#' "BRUNO NAO CALCICO") are looked up before those of
#' \code{.SIBCS_LEGACY_ORDER_MAP}. A label the first word already places is
#' read as before. At \code{level = "subordem"} an old name gives its order
#' followed by the next word of the label, which is not a suborder of SiBCS 5
#' in general: the old names are for comparisons at the order level.
#'
#' @param x Character vector of FEBR SiBCS names.
#' @param level One of \code{"order"} (default) or \code{"subordem"}.
#' @return Character vector of normalised SiBCS names; \code{NA} for
#'   labels that are out-of-scope for the comparison: any label that is not
#'   an order of SiBCS 5 after the legacy maps (old names without an entry
#'   there, "Solos ..." followed by none of them, names of other systems,
#'   text that is not a class).
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

  # v0.9.226: the order of an old name the first word alone does not give.
  # "Solo(s)" in front is not part of the name, a hyphen separates words as a
  # space does, and the two-word names are looked up before the one-word ones.
  # Returns the order (NA when there is none) and the words after the name.
  old_name <- function(label) {
    w <- strsplit(label, "[[:space:]-]+")[[1]]
    w <- gsub("^[^A-Z0-9]+|[^A-Z0-9]+$", "", w)
    w <- w[nzchar(w)]
    if (length(w) > 0L && w[1] %in% c("SOLO", "SOLOS")) w <- w[-1]
    if (length(w) >= 2L) {
      two <- paste(w[1], w[2])
      if (two %in% names(.SIBCS_LEGACY_TWO_WORD_MAP))
        return(list(order = unname(.SIBCS_LEGACY_TWO_WORD_MAP[two]),
                    rest = w[-(1:2)]))
    }
    if (length(w) >= 1L) {
      one <- apply_legacy_map(title(pluralise(w[1])))
      if (!is.na(one) && one %in% .SIBCS_ORDERS)
        return(list(order = one, rest = w[-1]))
    }
    list(order = NA_character_, rest = character(0))
  }

  out <- character(length(s))
  for (i in seq_along(s)) {
    if (is.na(s[i])) { out[i] <- NA_character_; next }
    parts <- strsplit(trimws(s[i]), "\\s+", fixed = FALSE)[[1]]
    if (length(parts) == 0L) { out[i] <- NA_character_; next }
    # v0.9.225: punctuation around the first word is not part of the name
    # ("ESPODOSSOLO.", "PLANOSSOLO(???)").
    parts[1] <- gsub("^[^A-Z0-9]+|[^A-Z0-9]+$", "", parts[1])
    ordem <- pluralise(parts[1])
    ordem_t <- title(ordem)
    ordem_t <- apply_legacy_map(ordem_t)
    rest <- parts[-1]
    # v0.9.226: when the first word gives no order, the label may still be an
    # old name that takes more than it ("SOLO LITOLICO", "TERRA ROXA
    # ESTRUTURADA", "LATOSSOLO-VERMELHO").
    if (is.na(ordem_t) || !(ordem_t %in% .SIBCS_ORDERS)) {
      old <- old_name(s[i])
      ordem_t <- old$order
      rest <- old$rest
    }
    # If the Order itself is out-of-scope (e.g. "SOLOS HALOMORFICOS"), the
    # entire output is NA, not "NA <sub>". v0.9.225: so is a label that is
    # not an order of SiBCS 5 by any of the readings above.
    if (is.na(ordem_t)) {
      out[i] <- NA_character_; next
    }
    if (level == "order" || length(rest) < 1L) {
      out[i] <- ordem_t
    } else {
      sub <- pluralise(rest[1])
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
