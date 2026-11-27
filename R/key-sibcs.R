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
#' @noRd
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


# v0.9.444: the levels of the SiBCS key, as named in the warnings.
.SIBCS_LEVEL_LABEL <- c(ordens = "Ordem", subordens = "Subordem",
                        grandes_grupos = "Grande grupo", subgrupos = "Subgrupo")

#' The name of a SiBCS result with no order (v0.9.444, G1)
#'
#' No order passed. With orders left undecided, the soil keys to one of them
#' or to none, so the name lists them in brackets as the candidates; with
#' every order FALSE, the soil is not classified, since the key ends with the
#' Argissolos and no residual order. In brackets because the name is
#' soilKey's, not the system's.
#' @noRd
.sibcs_no_order_name <- function(trace, candidates) {
  if (length(candidates) == 0L) return("[N\u00e3o classificado no SiBCS]")
  nm <- vapply(candidates, function(k) as.character(trace[[k]]$name %||% k), "")
  sprintf("[Ordem indeterminada no SiBCS: %s, ou n\u00e3o classificado]",
          paste(nm, collapse = ", "))
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
#' @param gapfill Preenchimento opcional de lacunas por interpolacao
#'        intra-perfil, default \code{FALSE} (no-op; classificacao
#'        byte-identica). \code{TRUE} preenche celulas \code{NA} interiores
#'        dos atributos continuos por profundidade; um vetor de caracteres
#'        restringe aos atributos citados; uma lista nomeada e repassada a
#'        \code{\link{gapfill_within_pedon}}. Celulas preenchidas recebem
#'        proveniencia \code{inferred_prior}, baixando o grau de evidencia
#'        para \code{"C"}. Opera sobre copia profunda -- o pedon do chamador
#'        nunca e modificado.
#' @section Undecided tests:
#' Since v0.9.444 (audit decision G1) the key follows the rule of
#' \code{\link{classify_usda}()} and the FAO legends. A test whose data are
#' missing is undecided (NA), not failed. Every class tested before the one
#' assigned, at the order, subordem, grande grupo and subgrupo levels, whose
#' result is NA is listed in \code{$ambiguities} (with \code{level}: one of
#' \code{"ordens"}, \code{"subordens"}, \code{"grandes_grupos"},
#' \code{"subgrupos"}), and \code{$missing_data} collects what was missing
#' at every level. A name reached past an undecided test holds only if that
#' test fails, so the evidence grade falls from A or B to C and a warning
#' says so.
#'
#' A class is assigned only when its test passes. The lists end with the
#' residuals of the text ("Outros solos...": the Haplicos, Orticos and
#' tipicos), which take whatever is left; a class the text defines by a
#' criterion of its own (the Argissolos at the end of the order key, the Tb
#' and Eutroficos great groups) is never reached by elimination. When no
#' class of a level passes and some were undecided, the name stops at the
#' level above. When no order passes, the name lists the undecided orders in
#' brackets as the candidates ("[Ordem indeterminada no SiBCS: ..., ou nao
#' classificado]", grade C at most), or is "[Nao classificado no SiBCS]"
#' when every order failed; \code{rsg_or_order} is then \code{NA}.
#' @return Um \code{\link{ClassificationResult}} cujo \code{name} eh o
#'         nome completo da classe atribuida no nivel mais profundo
#'         (Grande Grupo > Subordem > Ordem) e \code{rsg_or_order} eh
#'         o nome da ordem (e.g. "Organossolos"). Os codigos de cada
#'         nivel e o trace ficam em \code{$trace}.
#' @examples
#' pedon <- make_latossolo_canonical()
#' res <- classify_sibcs(pedon)
#' res$name
#' @export
classify_sibcs <- function(pedon,
                             rules      = NULL,
                             on_missing = c("warn", "silent", "error"),
                             include_familia = FALSE,
                             gapfill    = FALSE) {
  on_missing <- match.arg(on_missing)
  .classify_require_horizons(pedon, "classify_sibcs")
  # v0.9.400: one memo scope for the length of this call (R/memo.R)
  if (.memo_open()) on.exit(.memo_close(), add = TRUE)
  rules      <- rules %||% load_rules("sibcs5")

  # Opt-in within-pedon gap-fill (default off => byte-identical). Deep copy,
  # so the caller's pedon is never mutated.
  pedon <- .classify_apply_gapfill(pedon, gapfill)

  # Nivel 1: ordem
  key_result <- run_sibcs_key(pedon, rules)
  ordem      <- key_result$assigned
  # v0.9.444 (audit decision G1, Hugo, 2026-11-27): a class is assigned only
  # when its test passes. The engine returns the last class of the list when
  # none passes; at the order level that was the Argissolos, named with their
  # own test FALSE or undecided ("Outros solos que apresentam horizonte B
  # textural", p. 114, a class with a criterion of its own, not a residual).
  if (!is.null(ordem) && !isTRUE(key_result$trace[[ordem$code]]$passed)) {
    ordem <- NULL
  }

  # Nivel 2: subordem
  sub_result <- if (!is.null(ordem))
                  run_sibcs_subordem(pedon, ordem$code, rules)
                else list(assigned = NULL, trace = list())
  subordem   <- sub_result$assigned
  # v0.9.444: every list of subordens ends with the text's "Outros solos",
  # a default that passes by definition; the guard is for the rule sets of
  # a user, where a list may end with a class of its own.
  if (!is.null(subordem) && !isTRUE(sub_result$trace[[subordem$code]]$passed)) {
    subordem <- NULL
  }

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
  # v0.9.444: as above, a subgrupo whose test did not pass is not named.
  if (!is.null(sg) && !isTRUE(sg_result$trace[[sg$code]]$passed)) {
    sg <- NULL
  }

  # Nivel 5 (v0.7.14.D): familia (5o nivel categorico). Multi-rotulo,
  # nao chave -- desce sempre que include_familia=TRUE e o pedon
  # tem ordem/sg conhecidos.
  familia_attrs <- NULL
  familia_lbl <- NULL
  if (isTRUE(include_familia) && !is.null(ordem)) {
    familia_attrs <- tryCatch(
      classify_sibcs_familia(
        pedon,
        ordem_code = ordem$code,
        sg_code = if (!is.null(sg)) sg$code else NULL,
        nome_superior = if (!is.null(sg)) sg$name
                        else if (!is.null(gg)) gg$name
                        else if (!is.null(subordem)) subordem$name
                        else ordem$name
      ),
      error = function(e) list()
    )
    familia_lbl <- familia_label(familia_attrs)
  }

  # Display name = (Subgrupo + Familia) > Subgrupo > Grande Grupo > ...
  # v0.9.45: quando o fallback "cor a determinar" e detectado, o
  # display name para no nivel da Ordem com sufixo explicativo, em
  # vez de aceitar o catch-all PVA/LVA/NX/TX em silencio.
  # v0.9.444 (G1): undecided tests at every level, as in classify_usda().
  level_traces <- list(ordens         = key_result$trace,
                       subordens      = sub_result$trace,
                       grandes_grupos = gg_result$trace,
                       subgrupos      = sg_result$trace)
  level_assigned <- list(ordens         = ordem$code,
                         subordens      = subordem$code,
                         grandes_grupos = gg$code,
                         subgrupos      = sg$code)
  und <- .usda_undecided_by_level(level_traces, level_assigned)
  ord_candidates <- und$by_level$ordens %||% character(0)

  display_name <- if (!is.null(sg))            sg$name
                  else if (!is.null(gg))       gg$name
                  else if (!is.null(color_fallback)) sprintf("%s (cor a determinar)", ordem$name)
                  else if (!is.null(subordem)) subordem$name
                  else if (!is.null(ordem))    ordem$name
                  else .sibcs_no_order_name(key_result$trace, ord_candidates)
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

  # v0.9.444 (G1): every taxon tested before the one assigned, at the order,
  # subordem, grande grupo and subgrupo levels, whose result is undecided
  # joins $ambiguities with its level, and $missing_data collects from every
  # level. A name reached past an undecided test holds only if that test
  # fails: it is inferred by elimination, so the evidence grade falls from A
  # or B to C. Up to 0.9.443 only the order level was read and the grade came
  # from the provenance of the data alone.
  ambiguities  <- und$ambiguities
  grade        <- compute_evidence_grade(pedon, key_result$trace)
  missing_data <- unique(unlist(lapply(level_traces, collect_missing_attributes),
                                use.names = FALSE)) %||% character(0)
  warnings <- character(0)
  if (length(und$before_assigned) > 0L) {
    if (!is.na(grade) && grade %in% c("A", "B")) grade <- "C"
    warnings <- c(warnings, sprintf(paste0(
      "The name was reached past %d undecided test(s) (%s): it holds only if ",
      "they fail, so it is inferred by elimination and the evidence grade ",
      "is at most C. See $ambiguities and $missing_data."),
      length(und$before_assigned), paste(und$before_assigned, collapse = ", ")))
  }
  if (is.null(ordem) && length(ord_candidates) > 0L) {
    # no order passed and some were undecided: one of them, or none, by what
    # the missing data would show; no firm answer either way (as FAO, G1)
    if (!is.na(grade) && grade %in% c("A", "B")) grade <- "C"
    warnings <- c(warnings, sprintf(paste0(
      "No order passed and %d were left undecided (%s): the soil keys to one ",
      "of them, or to none, which the data do not decide. The evidence grade ",
      "is at most C. See $ambiguities and $missing_data."),
      length(ord_candidates), paste(ord_candidates, collapse = ", ")))
  } else if (is.null(ordem)) {
    warnings <- c(warnings, paste0(
      "Not classified: none of the 13 orders passed, and the key has no ",
      "residual order (the Argissolos ask for a B textural, p. 114). See ",
      "$trace$ordens for the test that failed in each."))
  }
  for (lv in setdiff(und$undecided_levels, "ordens")) {
    warnings <- c(warnings, sprintf(paste0(
      "%s undecided: no class passed and %s could not be decided, so the ",
      "name stops at the level above. See $ambiguities."),
      .SIBCS_LEVEL_LABEL[[lv]], paste(und$by_level[[lv]], collapse = ", ")))
  }

  # v0.9.45: quando ha fallback de cor, garantir que
  # munsell_hue_moist_horizon_B aparece em missing_data (usuario pode
  # consultar o atributo a medir) e rebaixar evidence_grade para no
  # maximo "C" (classificacao parcial).
  if (!is.null(color_fallback)) {
    missing_data <- unique(c(missing_data,
                              color_fallback$would_resolve_with))
    # v0.9.213: NA now means "no soil property at all" and stays NA.
    if (!is.na(grade) && grade %in% c("A", "B")) grade <- "C"
  }

  if (is.na(grade)) warnings <- c(warnings, .NO_PROPERTIES_WARNING)
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
    rsg_or_order   = ordem$name %||% NA_character_,
    qualifiers     = list(),
    trace          = trace_combined,
    ambiguities    = ambiguities,
    missing_data   = missing_data,
    evidence_grade = grade,
    prior_check    = NULL,
    warnings       = warnings,
    system_id      = "sibcs5"
  )
}
