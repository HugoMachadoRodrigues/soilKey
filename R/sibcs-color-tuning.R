# =============================================================================
# v0.9.61 -- SiBCS color tuning: thickness-weighted dominant B-horizon color.
#
# The SiBCS subordem key for color-driven Ordens (Argissolos / Latossolos /
# Nitossolos) is currently "first-match-wins" -- whichever color predicate
# fires first in the YAML captures the profile. Mixed profiles (e.g.
# Bt1 = 7.5YR amarelo, Bt2 = 2.5YR vermelho) get whichever subordem appears
# first in canonical key order, instead of the color that actually
# DOMINATES the B horizon by thickness.
#
# This module computes the thickness-weighted dominant color CATEGORY of
# all B horizons of a pedon and -- when the dominant differs from the
# first-match assignment -- overrides the subordem deterministically.
#
# Categories (per SiBCS 5a ed., Cap 1, Caracteristicas diferenciaveis):
#   * VERMELHO          -- hue <= 2.5YR (10R, 7.5R, 5R, 2.5R, 2.5YR)
#   * VERMELHO_AMARELO  -- hue == 5YR (intermediate)
#   * AMARELO           -- hue >= 7.5YR with chroma >= 4
#   * BRUNO_ACINZENTADO -- value <= 4 AND chroma <= 4 (dark, regardless hue)
#   * ACINZENTADO       -- hue >= 7.5YR with value >= 5 AND chroma < 4
#                          (pale/grey)
#
# Ordem -> dominant -> subordem code mapping:
#   P (Argissolos): PV / PA / PVA / PBAC / PAC
#   L (Latossolos): LV / LA / LVA / LB   / LVA   (no greyish subordem)
#   N (Nitossolos): NV / NX  / NX  / NB  / NX    (only Bruno + Vermelho
#                                                  exist; rest -> Haplico)
#
# Luvissolos (T) are left untouched: TC vs TX is a chroma intensity test
# (caracter cromico), not a color hue partition.
# =============================================================================


# ---- single-horizon color category ----------------------------------------

#' Classify a single Munsell color into a SiBCS B-horizon color category
#'
#' @param hue    Munsell hue, e.g. "5YR" or "2.5Y".
#' @param value  Munsell value (numeric).
#' @param chroma Munsell chroma (numeric).
#'
#' @return Character scalar: one of `"VERMELHO"`, `"VERMELHO_AMARELO"`,
#'   `"AMARELO"`, `"BRUNO_ACINZENTADO"`, `"ACINZENTADO"`, or `NA`
#'   when any of the three Munsell components is missing.
#'
#' @keywords internal
.classify_b_color <- function(hue, value, chroma) {
  if (is.na(hue) || is.na(value) || is.na(chroma)) return(NA_character_)
  hu <- toupper(trimws(hue))

  # 1. BRUNO_ACINZENTADO: dark (value <= 4, chroma <= 4) and at least
  #    moderately yellow (hue >= 5YR) -- catches the dark-brown / dark-grey
  #    end of the B color spectrum.
  if (value <= 4 && chroma <= 4 &&
        grepl("^(5YR|7\\.5YR|10YR|2\\.5Y|5Y|10Y)\\b", hu)) {
    return("BRUNO_ACINZENTADO")
  }

  # 2. ACINZENTADO: pale grey (value >= 5, chroma < 4) on yellow side.
  if (value >= 5 && chroma < 4 &&
        grepl("^(7\\.5YR|10YR|2\\.5Y|5Y|10Y)\\b", hu)) {
    return("ACINZENTADO")
  }

  # 3. VERMELHO: red end of hue ladder.
  if (grepl("^(10R|7\\.5R|5R|2\\.5R|2\\.5YR)\\b", hu)) {
    return("VERMELHO")
  }

  # 4. VERMELHO_AMARELO: intermediate (5YR) with non-grey chroma.
  if (grepl("^5YR\\b", hu)) {
    return("VERMELHO_AMARELO")
  }

  # 5. AMARELO: yellow side with chroma >= 4.
  if (grepl("^(7\\.5YR|10YR|2\\.5Y|5Y|10Y)\\b", hu) && chroma >= 4) {
    return("AMARELO")
  }

  NA_character_
}


# ---- thickness-weighted dominant ------------------------------------------

#' Thickness-weighted dominant B-horizon color category for a pedon
#'
#' Walks every B-like horizon (designation matching `^B[wt]?` and not
#' `^BC|^Bt0`), classifies each into a SiBCS color category via
#' [.classify_b_color()], sums horizon thickness per category, and
#' returns the category with the largest cumulative thickness. Ties are
#' broken in canonical SiBCS order (BRUNO_ACINZENTADO > ACINZENTADO >
#' AMARELO > VERMELHO > VERMELHO_AMARELO).
#'
#' @param pedon A `[PedonRecord]`.
#'
#' @return List with `dominant` (character scalar or `NA`),
#'   `thickness_by_category` (named numeric vector), `n_b_layers`
#'   (integer), and `n_classified` (integer).
#'
#' @keywords internal
.dominant_b_color <- function(pedon) {
  h  <- pedon$horizons
  bl <- .b_layers(pedon)
  if (length(bl) == 0L) {
    return(list(dominant = NA_character_,
                thickness_by_category = numeric(0),
                n_b_layers = 0L, n_classified = 0L))
  }
  hues <- h$munsell_hue_moist[bl]
  vals <- h$munsell_value_moist[bl]
  chrs <- h$munsell_chroma_moist[bl]
  tops <- h$top_cm[bl]
  bots <- h$bottom_cm[bl]
  thk  <- pmax(bots - tops, 0, na.rm = FALSE)
  thk[is.na(thk) | thk <= 0] <- 1  # default unit weight when depth missing

  cats <- vapply(seq_along(bl),
                  function(i) .classify_b_color(hues[i], vals[i], chrs[i]),
                  character(1))

  classified <- !is.na(cats)
  if (!any(classified)) {
    return(list(dominant = NA_character_,
                thickness_by_category = numeric(0),
                n_b_layers = length(bl), n_classified = 0L))
  }

  by_cat <- tapply(thk[classified], cats[classified], sum)
  if (length(by_cat) == 0L) {
    return(list(dominant = NA_character_,
                thickness_by_category = numeric(0),
                n_b_layers = length(bl), n_classified = sum(classified)))
  }

  # Canonical tie-break order (most-specific first).
  tie_order <- c("BRUNO_ACINZENTADO", "ACINZENTADO",
                  "AMARELO", "VERMELHO", "VERMELHO_AMARELO")
  ord <- order(-as.numeric(by_cat),
                match(names(by_cat), tie_order, nomatch = length(tie_order) + 1L))
  dominant <- names(by_cat)[ord[1L]]

  list(dominant = dominant,
       thickness_by_category = as.numeric(by_cat) |> setNames(names(by_cat)),
       n_b_layers = length(bl),
       n_classified = sum(classified))
}


# ---- ordem-specific dominant -> subordem code mapping ---------------------

# Returns NA when the Ordem has no color partition (or when dominant is NA).
# Each mapping is keyed first by Ordem code, then by color category.
.SIBCS_DOMINANT_TO_SUBORDEM <- list(
  "P" = c(VERMELHO          = "PV",
           AMARELO           = "PA",
           VERMELHO_AMARELO  = "PVA",
           BRUNO_ACINZENTADO = "PBAC",
           ACINZENTADO       = "PAC"),
  "L" = c(VERMELHO          = "LV",
           AMARELO           = "LA",
           VERMELHO_AMARELO  = "LVA",
           BRUNO_ACINZENTADO = "LB",
           ACINZENTADO       = "LVA"),
  "N" = c(VERMELHO          = "NV",
           AMARELO           = "NX",
           VERMELHO_AMARELO  = "NX",
           BRUNO_ACINZENTADO = "NB",
           ACINZENTADO       = "NX")
)


#' Resolve the subordem code dictated by the dominant B-horizon color
#'
#' @param pedon      A `[PedonRecord]`.
#' @param ordem_code Single-letter Ordem code, e.g. `"P"`.
#'
#' @return List with `code` (target subordem code or `NA`) and
#'   `evidence` (the diagnostic returned by [.dominant_b_color()]).
#'
#' @keywords internal
.dominant_b_color_subordem <- function(pedon, ordem_code) {
  if (!ordem_code %in% names(.SIBCS_DOMINANT_TO_SUBORDEM)) {
    return(list(code = NA_character_, evidence = NULL))
  }
  ev   <- .dominant_b_color(pedon)
  if (is.na(ev$dominant)) {
    return(list(code = NA_character_, evidence = ev))
  }
  mapping <- .SIBCS_DOMINANT_TO_SUBORDEM[[ordem_code]]
  list(code = unname(mapping[ev$dominant]), evidence = ev)
}


# ---- post-processor that overrides a first-match-wins assignment ---------

#' Override a first-match-wins SiBCS subordem with the dominant-color rule
#'
#' Called from [classify_sibcs()] after the YAML key has assigned a
#' subordem. When the Ordem is one of the color-partitioned ones (P, L,
#' N) and the dominant-color rule produces a DIFFERENT subordem code,
#' replaces the assigned entry with the YAML block matching the new
#' code. The function does nothing for non-color Ordens, when no Munsell
#' B color is available, when the dominant matches the first-match
#' assignment, or when the YAML lacks an entry for the dominant code.
#'
#' @param subordem    The subordem entry assigned by the YAML key
#'                     (`list(code, name, tests, ...)`) or `NULL`.
#' @param pedon       A `[PedonRecord]`.
#' @param ordem_code  Single-letter Ordem code.
#' @param rules       Loaded SiBCS rule set (with `$subordens[[ordem_code]]`).
#'
#' @return List with `subordem` (the possibly-overridden YAML entry) and
#'   `override` (NULL when no change, else
#'   `list(from_code, to_code, dominant_evidence)`).
#'
#' @keywords internal
.apply_color_dominant_override <- function(subordem, pedon,
                                              ordem_code, rules) {
  if (is.null(subordem)) {
    return(list(subordem = NULL, override = NULL))
  }
  if (!ordem_code %in% names(.SIBCS_DOMINANT_TO_SUBORDEM)) {
    return(list(subordem = subordem, override = NULL))
  }
  dom <- .dominant_b_color_subordem(pedon, ordem_code)
  if (is.na(dom$code) || identical(dom$code, subordem$code)) {
    return(list(subordem = subordem, override = NULL))
  }
  ord_block <- rules$subordens[[ordem_code]]
  if (is.null(ord_block)) {
    return(list(subordem = subordem, override = NULL))
  }
  match_idx <- vapply(ord_block,
                       function(s) identical(s$code, dom$code),
                       logical(1))
  if (!any(match_idx)) {
    return(list(subordem = subordem, override = NULL))
  }
  new_sub <- ord_block[[which(match_idx)[1L]]]
  list(
    subordem = new_sub,
    override = list(
      from_code         = subordem$code,
      from_name         = subordem$name,
      to_code           = new_sub$code,
      to_name           = new_sub$name,
      dominant_evidence = dom$evidence,
      reason            = sprintf(
        paste0("Subordem trocada de '%s' para '%s' pela regra ",
                "dominante-de-cor em B (categoria %s, espessura ",
                "%.0f cm de %d horizonte(s) classificado(s)/",
                "%d horizonte(s) B)."),
        subordem$name, new_sub$name,
        dom$evidence$dominant,
        sum(dom$evidence$thickness_by_category, na.rm = TRUE),
        dom$evidence$n_classified, dom$evidence$n_b_layers
      )
    )
  )
}
