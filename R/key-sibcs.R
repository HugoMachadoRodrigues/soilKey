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

  # Nivel 3: grande grupo (v0.7.3) -- so desce se a subordem foi
  # resolvida e a ordem tem bloco de Grandes Grupos no YAML.
  gg_result <- if (!is.null(subordem))
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
  display_name <- if (!is.null(sg))            sg$name
                  else if (!is.null(gg))       gg$name
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
    familia_label         = familia_lbl
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
