# ================================================================
# SiBCS 5a edicao -- chave (v0.2 scaffold)
#
# v0.2 wires duas ordens end-to-end (Latossolos via B_latossolico,
# Argissolos via B_textural). As outras 11 ordens estao stubadas em
# inst/rules/sibcs5/key.yaml e voltam NA no trace.
#
# Implementacao completa do SiBCS -- 13 ordens, subordens, grandes
# grupos, subgrupos, validacao com Embrapa Solos -- agendada para v0.7.
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


#' Classifica um pedon segundo o SiBCS 5a edicao
#'
#' v0.2 scaffold: apenas os caminhos de Latossolos (via
#' \code{\link{B_latossolico}}) e Argissolos (via
#' \code{\link{B_textural}}) estao ligados; ambos delegam aos
#' diagnosticos WRB. As outras 11 ordens estao stubadas e qualquer
#' perfil que nao satisfaca essas tem recolhido na ordem default
#' (Neossolos -- catch-all).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rules Conjunto de regras pre-carregado.
#' @param on_missing Um de \code{"warn"} (default), \code{"silent"},
#'        \code{"error"}.
#' @return Um \code{\link{ClassificationResult}}.
#' @export
classify_sibcs <- function(pedon,
                             rules      = NULL,
                             on_missing = c("warn", "silent", "error")) {
  on_missing <- match.arg(on_missing)
  rules      <- rules %||% load_rules("sibcs5")

  key_result <- run_sibcs_key(pedon, rules)
  ordem      <- key_result$assigned

  codes      <- vapply(rules$ordens, function(o) o$code, character(1))
  is_default <- identical(ordem$code, tail(codes, 1L))

  ambiguities  <- find_ambiguities(key_result$trace, current = ordem$code)
  grade        <- compute_evidence_grade(pedon, key_result$trace)
  missing_data <- collect_missing_attributes(key_result$trace)

  warnings <- character(0)
  if (is_default) {
    warnings <- c(warnings, paste0(
      "Perfil chaveou para Neossolos catch-all. v0.2 liga apenas ",
      "Latossolos (via B_latossolico) e Argissolos (via B_textural); ",
      "as outras 11 ordens estao agendadas para v0.7."
    ))
  }

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
    name           = ordem$name,
    rsg_or_order   = ordem$name,
    qualifiers     = list(),
    trace          = key_result$trace,
    ambiguities    = ambiguities,
    missing_data   = missing_data,
    evidence_grade = grade,
    prior_check    = NULL,
    warnings       = warnings
  )
}
