# ================================================================
# SiBCS 5a edicao -- chave
#
# v0.7   -- 13 ordens (1o nivel)        diagnostics-rsg-sibcs.R
# v0.7.1 -- 44 subordens (2o nivel)     diagnostics-subordens-sibcs.R
# v0.7.2 -- engine refactor + 7 atributos pendentes
# v0.7.3 -- in-progress: Grandes Grupos (3o nivel) por ordem.
#           Cap 14 (Organossolos) wired:
#           inst/rules/sibcs5/grandes-grupos/organossolos.yaml.
#           Demais ordens (Caps 5-13, 15-17) progressivamente.
# ================================================================


#' Roda a chave SiBCS 5a edicao sobre um pedon
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rules Conjunto de regras pre-carregado; se NULL, le
#'        \code{inst/rules/sibcs5/key.yaml}.
#' @return Lista com \code{assigned} (entrada YAML da ordem atribuida)
#'         e \code{trace}.
#' @export
run_sibcs_key <- function(pedon, rules = NULL) {
  rules <- rules %||% load_rules("sibcs5")
  run_taxonomic_key(pedon, rules, level_key = "ordens")
}


#' Resolve a subordem de um pedon ja classificado em uma ordem SiBCS
#'
#' Itera as subordens da ordem em ordem canonica via o engine generico
#' \code{\link{run_taxa_list}}; a primeira cuja test-block passa captura
#' o perfil. Se nenhuma passar, retorna a ultima subordem (catch-all
#' \code{tests:{default:true}}).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param ordem_code Codigo de uma letra da ordem (e.g. "L" para
#'        Latossolos).
#' @param rules Lista de regras carregada via \code{\link{load_rules}}.
#' @return Lista com \code{assigned} (entrada YAML da subordem ou
#'         \code{NULL} se a ordem nao tiver bloco) e \code{trace}.
#' @export
run_sibcs_subordem <- function(pedon, ordem_code, rules = NULL) {
  rules <- rules %||% load_rules("sibcs5")
  if (is.null(rules$subordens) || is.null(rules$subordens[[ordem_code]])) {
    return(list(assigned = NULL, trace = list()))
  }
  run_taxa_list(pedon, rules$subordens[[ordem_code]])
}


#' Resolve o grande grupo (3o nivel) de um pedon classificado em uma
#' subordem SiBCS
#'
#' v0.7.3: itera os Grandes Grupos da subordem em ordem canonica via o
#' engine generico \code{\link{run_taxa_list}}; a primeira test-block
#' que passa captura o perfil. Os Grandes Grupos sao carregados de
#' \code{inst/rules/sibcs5/grandes-grupos/<ordem>.yaml} (split por
#' ordem) e mergeados pelo \code{\link{load_rules}}.
#'
#' Quando a subordem nao tem bloco de Grandes Grupos definido (ainda
#' nao wirado para todas as ordens), retorna
#' \code{list(assigned = NULL, trace = list())} -- comportamento
#' nao-fatal que permite \code{\link{classify_sibcs}} parar no 2o
#' nivel sem erro.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param subordem_code Codigo da subordem (e.g. "OJ" para Organossolos
#'        Tiomorficos).
#' @param rules Lista de regras carregada via \code{\link{load_rules}}.
#' @return Lista com \code{assigned} (entrada YAML do Grande Grupo ou
#'         \code{NULL}) e \code{trace}.
#' @export
run_sibcs_grande_grupo <- function(pedon, subordem_code, rules = NULL) {
  rules <- rules %||% load_rules("sibcs5")
  if (is.null(rules$grandes_grupos) ||
      is.null(rules$grandes_grupos[[subordem_code]])) {
    return(list(assigned = NULL, trace = list()))
  }
  run_taxa_list(pedon, rules$grandes_grupos[[subordem_code]])
}


#' Resolve o subgrupo (4o nivel) de um pedon classificado em um Grande
#' Grupo SiBCS
#'
#' v0.7.3.B: itera os Subgrupos do Grande Grupo em ordem canonica via o
#' engine generico \code{\link{run_taxa_list}}; a primeira test-block
#' que passa captura o perfil. Os Subgrupos sao carregados de
#' \code{inst/rules/sibcs5/subgrupos/<ordem>.yaml} (split por ordem) e
#' mergeados pelo \code{\link{load_rules}}.
#'
#' Em contraste com o 3o nivel (Grandes Grupos de Organossolos),
#' Subgrupos de Cap 14 SEMPRE tem catch-all \code{tests:{default:true}}
#' como ultima entrada de cada lista (subgrupo "tipico"), entao a
#' classificacao sempre desce ao 4o nivel quando o GG foi resolvido.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param gg_code Codigo do Grande Grupo (e.g. "OJF" para Organossolos
#'        Tiomorficos Fibricos).
#' @param rules Lista de regras carregada via \code{\link{load_rules}}.
#' @return Lista com \code{assigned} (entrada YAML do Subgrupo ou
#'         \code{NULL}) e \code{trace}.
#' @export
run_sibcs_subgrupo <- function(pedon, gg_code, rules = NULL) {
  rules <- rules %||% load_rules("sibcs5")
  if (is.null(rules$subgrupos) ||
      is.null(rules$subgrupos[[gg_code]])) {
    return(list(assigned = NULL, trace = list()))
  }
  run_taxa_list(pedon, rules$subgrupos[[gg_code]])
}


# ============================================================
# v0.9.45 -- color-undetermined graceful path.
#
# Subordens cuja regra discrimina por matiz Munsell em B (PV/PA/PVA,
# LV/LA/LVA, NV/NB/NX, TC/TX) cairam silenciosamente no catch-all
# quando o matiz nao foi medido. O codigo abaixo detecta esse padrao
# e marca o resultado como "cor a determinar", para o classificador
# parar no nivel da Ordem e expor o atributo que resolveria a duvida.
# ============================================================

.SIBCS_COLOR_CATCH_ALL_CODES <- c("PVA", "LVA", "NX", "TX")

#' Detecta fallback "cor a determinar" no nivel de subordem SiBCS
#'
#' Quando a subordem atribuida e uma catch-all de cor (PVA, LVA, NX,
#' TX) E pelo menos um predicado anterior na trace falhou exatamente
#' por ausencia de \code{munsell_hue_moist}, considera-se que o
#' fallback foi forçado pela ausencia de matiz, nao pelo conteudo
#' do perfil. Retorna NULL se a situacao nao se aplica.
#'
#' @keywords internal
.detect_color_undetermined_fallback <- function(sub_result, subordem) {
  if (is.null(subordem)) return(NULL)
  if (!isTRUE(subordem$code %in% .SIBCS_COLOR_CATCH_ALL_CODES)) return(NULL)
  trace <- sub_result$trace %||% list()
  if (length(trace) == 0L) return(NULL)
  earlier_codes <- setdiff(names(trace), subordem$code)
  if (length(earlier_codes) == 0L) return(NULL)
  hue_blocked <- vapply(earlier_codes, function(code) {
    t <- trace[[code]]
    miss <- t$missing %||% character(0)
    !isTRUE(t$passed) && "munsell_hue_moist" %in% miss
  }, logical(1))
  if (!any(hue_blocked)) return(NULL)
  rejected_codes <- earlier_codes[hue_blocked]
  rejected_names <- vapply(rejected_codes, function(code) {
    trace[[code]]$name %||% code
  }, character(1))
  list(
    detected = TRUE,
    missing_attribute = "munsell_hue_moist_horizon_B",
    horizon_target = "B",
    fallback_subordem = list(code = subordem$code, name = subordem$name),
    rejected_alternatives = data.frame(
      code = rejected_codes,
      name = rejected_names,
      stringsAsFactors = FALSE
    ),
    would_resolve_with = "munsell_hue_moist_horizon_B",
    reason = sprintf(
      paste0("Subordem '%s' atribuida por fallback porque o matiz ",
             "Munsell em B esta ausente. Medindo a cor seria possivel ",
             "discriminar entre: %s."),
      subordem$name, paste(rejected_names, collapse = ", ")
    )
  )
}


#' Classifica um pedon segundo o SiBCS 5a edicao (1o + 2o + 3o + 4o niveis)
#'
#' v0.7 ligou as 13 ordens; v0.7.1 desce ao 2o nivel (subordens) via
#' \code{\link{run_sibcs_subordem}}; v0.7.3 desce ao 3o nivel (Grandes
#' Grupos) via \code{\link{run_sibcs_grande_grupo}} para as ordens
#' progressivamente wiradas em
#' \code{inst/rules/sibcs5/grandes-grupos/<ordem>.yaml} (Cap 14
#' Organossolos primeiro). Quando a subordem ainda nao tem bloco de
#' Grandes Grupos, ou quando nenhum Grande Grupo passa (e nao ha
#' catch-all default), a classificacao para no 2o nivel.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rules Conjunto de regras pre-carregado.
#' @param on_missing Um de \code{"warn"} (default), \code{"silent"},
#'        \code{"error"}.
#' @param include_familia Quando \code{TRUE} (default \code{FALSE}),
#'        adiciona o 5o nivel categorico (Familia) via
#'        \code{\link{classify_sibcs_familia}}. O label textual da
#'        Familia aparece em \code{$trace$familia_label}, e a lista
#'        de \code{\link{FamilyAttribute}}s em \code{$trace$familia}.
#' @return Um \code{\link{ClassificationResult}} cujo \code{name} eh o
#'         nome completo da classe atribuida no nivel mais profundo
#'         (Grande Grupo > Subordem > Ordem) e \code{rsg_or_order} eh
#'         o nome da ordem (e.g. "Organossolos"). Os codigos de cada
#'         nivel e o trace ficam em \code{$trace}.
#' @export
classify_sibcs <- function(pedon,
                             rules      = NULL,
                             on_missing = c("warn", "silent", "error"),
                             include_familia = FALSE) {
  on_missing <- match.arg(on_missing)
  rules      <- rules %||% load_rules("sibcs5")

  # Nivel 1: ordem
  key_result <- run_sibcs_key(pedon, rules)
  ordem      <- key_result$assigned

  # Nivel 2: subordem
  sub_result <- run_sibcs_subordem(pedon, ordem$code, rules)
  subordem   <- sub_result$assigned

  # v0.9.45: detectar fallback "cor a determinar" -- quando a subordem
  # atribuida e a catch-all de cor (PVA/LVA/NX/TX) E pelo menos um
  # predicado anterior falhou por ausencia de matiz Munsell em B, o
  # classificador deve parar no nivel da ordem e expor o gap em vez
  # de aceitar o catch-all em silencio.
  color_fallback <- .detect_color_undetermined_fallback(sub_result, subordem)

  # Nivel 3: grande grupo (v0.7.3) -- so desce se a subordem foi
  # resolvida E nao houve fallback de cor; ordem tem bloco de Grandes
  # Grupos no YAML.
  gg_result <- if (!is.null(subordem) && is.null(color_fallback))
                 run_sibcs_grande_grupo(pedon, subordem$code, rules)
               else list(assigned = NULL, trace = list())
  gg <- gg_result$assigned
  # O engine generico retorna o ULTIMO taxon como fallback quando nenhum
  # passa. Para o 3o nivel sem catch-all 'default: true', isso e um
  # falso positivo -- demote para NULL se o trace mostra que o
  # candidato escolhido nao passou de fato.
  if (!is.null(gg) && !isTRUE(gg_result$trace[[gg$code]]$passed)) {
    gg <- NULL
  }

  # Nivel 4: subgrupo (v0.7.3.B) -- so desce se o GG foi resolvido e
  # o YAML tem bloco de Subgrupos para esse GG. Como Subgrupos do
  # Cap 14 SEMPRE tem catch-all 'tipico' (default: true), o engine
  # generico vai assinalar deterministicamente uma entrada quando o
  # GG e wirado.
  sg_result <- if (!is.null(gg))
                 run_sibcs_subgrupo(pedon, gg$code, rules)
               else list(assigned = NULL, trace = list())
  sg <- sg_result$assigned

  # Nivel 5 (v0.7.14.D): familia (5o nivel categorico). Multi-rotulo,
  # nao chave -- desce sempre que include_familia=TRUE e o pedon
  # tem ordem/sg conhecidos.
  familia_attrs <- NULL
  familia_lbl <- NULL
  if (isTRUE(include_familia)) {
    familia_attrs <- tryCatch(
      classify_sibcs_familia(
        pedon,
        ordem_code = ordem$code,
        sg_code = if (!is.null(sg)) sg$code else NULL
      ),
      error = function(e) list()
    )
    familia_lbl <- familia_label(familia_attrs)
  }

  # Display name = (Subgrupo + Familia) > Subgrupo > Grande Grupo > ...
  # v0.9.45: quando o fallback "cor a determinar" e detectado, o
  # display name para no nivel da Ordem com sufixo explicativo, em
  # vez de aceitar o catch-all PVA/LVA/NX/TX em silencio.
  display_name <- if (!is.null(sg))            sg$name
                  else if (!is.null(gg))       gg$name
                  else if (!is.null(color_fallback)) sprintf("%s (cor a determinar)", ordem$name)
                  else if (!is.null(subordem)) subordem$name
                  else                         ordem$name
  if (isTRUE(include_familia) && !is.null(familia_lbl) &&
        nzchar(familia_lbl)) {
    display_name <- paste0(display_name, ", ", familia_lbl)
  }
  trace_combined <- list(
    ordens                = key_result$trace,
    subordens             = sub_result$trace,
    subordem_assigned     = subordem,
    grandes_grupos        = gg_result$trace,
    grande_grupo_assigned = gg,
    subgrupos             = sg_result$trace,
    subgrupo_assigned     = sg,
    familia               = familia_attrs,
    familia_label         = familia_lbl,
    color_undetermined    = color_fallback
  )

  ambiguities  <- find_ambiguities(key_result$trace, current = ordem$code)
  grade        <- compute_evidence_grade(pedon, key_result$trace)
  missing_data <- collect_missing_attributes(key_result$trace)

  # v0.9.45: quando ha fallback de cor, garantir que
  # munsell_hue_moist_horizon_B aparece em missing_data (usuario pode
  # consultar o atributo a medir) e rebaixar evidence_grade para no
  # maximo "C" (classificacao parcial).
  if (!is.null(color_fallback)) {
    missing_data <- unique(c(missing_data,
                              color_fallback$would_resolve_with))
    if (grade %in% c("A", "B", NA_character_)) grade <- "C"
  }

  warnings <- character(0)
  if (length(missing_data) > 0L) {
    msg <- sprintf(
      "%d atributo(s) faltando ao longo do trace -- veja $missing_data",
      length(missing_data)
    )
    if      (on_missing == "warn")  warnings <- c(warnings, msg)
    else if (on_missing == "error") rlang::abort(msg)
  }
  if (!is.null(color_fallback)) {
    warnings <- c(warnings, color_fallback$reason)
  }

  ClassificationResult$new(
    system         = "SiBCS 5a edicao",
    name           = display_name,
    rsg_or_order   = ordem$name,
    qualifiers     = list(),
    trace          = trace_combined,
    ambiguities    = ambiguities,
    missing_data   = missing_data,
    evidence_grade = grade,
    prior_check    = NULL,
    warnings       = warnings
  )
}
