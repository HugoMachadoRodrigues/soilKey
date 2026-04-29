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


#' Classifica um pedon segundo o SiBCS 5a edicao (1o + 2o + 3o niveis)
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
#' @return Um \code{\link{ClassificationResult}} cujo \code{name} eh o
#'         nome completo da classe atribuida no nivel mais profundo
#'         (Grande Grupo > Subordem > Ordem) e \code{rsg_or_order} eh
#'         o nome da ordem (e.g. "Organossolos"). Os codigos de cada
#'         nivel e o trace ficam em \code{$trace}.
#' @export
classify_sibcs <- function(pedon,
                             rules      = NULL,
                             on_missing = c("warn", "silent", "error")) {
  on_missing <- match.arg(on_missing)
  rules      <- rules %||% load_rules("sibcs5")

  # Nivel 1: ordem
  key_result <- run_sibcs_key(pedon, rules)
  ordem      <- key_result$assigned

  # Nivel 2: subordem
  sub_result <- run_sibcs_subordem(pedon, ordem$code, rules)
  subordem   <- sub_result$assigned

  # Nivel 3: grande grupo (v0.7.3) -- so desce se a subordem foi
  # resolvida e a ordem tem bloco de Grandes Grupos no YAML.
  gg_result <- if (!is.null(subordem))
                 run_sibcs_grande_grupo(pedon, subordem$code, rules)
               else list(assigned = NULL, trace = list())
  gg <- gg_result$assigned
  # O engine genero retorna o ULTIMO taxon como fallback quando nenhum
  # passa. Para o 3o nivel sem catch-all 'default: true', isso e um
  # falso positivo -- demote para NULL se o trace mostra que o
  # candidato escolhido nao passou de fato.
  if (!is.null(gg) && !isTRUE(gg_result$trace[[gg$code]]$passed)) {
    gg <- NULL
  }

  # Display name = Grande Grupo (se houver) > Subordem > Ordem
  display_name <- if (!is.null(gg))         gg$name
                  else if (!is.null(subordem)) subordem$name
                  else                       ordem$name
  trace_combined <- list(
    ordens                = key_result$trace,
    subordens             = sub_result$trace,
    subordem_assigned     = subordem,
    grandes_grupos        = gg_result$trace,
    grande_grupo_assigned = gg
  )

  ambiguities  <- find_ambiguities(key_result$trace, current = ordem$code)
  grade        <- compute_evidence_grade(pedon, key_result$trace)
  missing_data <- collect_missing_attributes(key_result$trace)

  warnings <- character(0)
  if (length(missing_data) > 0L) {
    msg <- sprintf(
      "%d atributo(s) faltando ao longo do trace -- veja $missing_data",
      length(missing_data)
    )
    if      (on_missing == "warn")  warnings <- c(warnings, msg)
    else if (on_missing == "error") rlang::abort(msg)
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
