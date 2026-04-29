# ================================================================
# SiBCS 5a edicao -- chave (v0.7.1: 13 ordens + 44 subordens)
#
# v0.7 wired all 13 ordens via diagnostics-rsg-sibcs.R.
# v0.7.1 adds the 2nd categorical level (subordens, Caps 5-17) --
# 44 classes -- via diagnostics-subordens-sibcs.R and a `subordens`
# block in inst/rules/sibcs5/key.yaml.
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


#' Classifica um pedon segundo o SiBCS 5a edicao (1o + 2o niveis)
#'
#' v0.7 ligou as 13 ordens; v0.7.1 desce ao 2o nivel (subordens) via
#' \code{\link{run_sibcs_subordem}} usando a chave em
#' \code{inst/rules/sibcs5/key.yaml}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rules Conjunto de regras pre-carregado.
#' @param on_missing Um de \code{"warn"} (default), \code{"silent"},
#'        \code{"error"}.
#' @return Um \code{\link{ClassificationResult}} cujo \code{name} eh o
#'         nome completo da subordem (e.g. "Latossolos Vermelhos") e
#'         \code{rsg_or_order} eh o nome da ordem (e.g. "Latossolos").
#'         O codigo da subordem e o trace ficam em
#'         \code{$trace$subordem}.
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

  display_name <- if (!is.null(subordem)) subordem$name else ordem$name
  trace_combined <- list(
    ordens     = key_result$trace,
    subordens  = sub_result$trace,
    subordem_assigned = subordem
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
