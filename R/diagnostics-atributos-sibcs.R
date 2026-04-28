# ============================================================================
# SiBCS 5a edicao (Embrapa, 2018) -- atributos diagnosticos (Cap 1, pp 29-48)
#
# Cada atributo e implementado como funcao pura no padrao DiagnosticResult.
# Muitos sao wrappers finos sobre sub-tests ja existentes (WRB Ch 3.1/3.2/3.3
# tem cobertura completa em soilKey >= v0.3.5); aqui usamos a nomenclatura e
# os limiares canonicos do SiBCS, que diferem em alguns casos do WRB.
#
# Referencia exata: Empresa Brasileira de Pesquisa Agropecuaria. Embrapa
# Solos. Sistema Brasileiro de Classificacao de Solos. 5a ed. rev. e ampl.
# Brasilia, DF: Embrapa, 2018, Cap 1, p 29-48.
# ============================================================================


# ---- Atividade da fracao argila (Ta / Tb) ---------------------------------

#' Atividade da fracao argila (SiBCS Cap 1, p 30)
#'
#' Calcula a atividade da fracao argila Ta = CEC * 1000 / argila (em
#' cmolc/kg de argila, sem correcao para carbono) por horizonte e
#' classifica como **alta (Ta)** se >= 27 cmolc/kg argila ou **baixa (Tb)**
#' se < 27. Nao se aplica a texturas areia / areia franca.
#'
#' Para distincao de classes pelo SiBCS, considera-se a atividade no
#' horizonte B (incl. BA, exc. BC) ou no horizonte C (incl. CA), quando
#' nao existe B.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return Um \code{\link{DiagnosticResult}}; \code{passed = TRUE} sse
#'   pelo menos um horizonte B ou C tem Ta. \code{layers} = horizontes
#'   com atividade alta (Ta).
#' @references Embrapa (2018), SiBCS 5a ed., Cap 1, "Atividade da fracao
#'   argila", p. 30.
#' @export
atividade_argila_alta <- function(pedon, min_ta = 27) {
  h <- pedon$horizons
  layers_b_c <- which(!is.na(h$designation) &
                        grepl("^(B|C)", h$designation, ignore.case = FALSE))
  if (length(layers_b_c) == 0L) {
    return(DiagnosticResult$new(
      name = "atividade_argila_alta", passed = NA, layers = integer(0),
      evidence = list(reason = "no B or C horizons in profile"),
      missing = "designation",
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p. 30"
    ))
  }
  details <- list(); passing <- integer(0); missing <- character(0)
  for (i in layers_b_c) {
    cec <- h$cec_cmol[i]; clay <- h$clay_pct[i]
    if (is.na(cec) || is.na(clay) || clay <= 0) {
      missing <- c(missing, "cec_cmol", "clay_pct"); next
    }
    # SiBCS exclui texturas areia / areia franca: clay >= 150 g/kg = 15%.
    if (clay < 15) {
      details[[as.character(i)]] <- list(idx = i, clay_pct = clay,
                                          excluded = "areia / areia franca")
      next
    }
    # CEC em cmolc/kg solo; clay em pct (g/100g). Ta em cmolc/kg argila.
    ta <- cec * 1000 / (clay * 10)   # cec * 1000/g_kg_clay; clay_pct * 10 -> g_kg
    details[[as.character(i)]] <- list(
      idx = i, cec_cmol = cec, clay_pct = clay,
      ta_cmolc_per_kg_clay = ta, threshold = min_ta,
      passed = ta >= min_ta
    )
    if (ta >= min_ta) passing <- c(passing, i)
  }
  evaluated <- length(details) - length(missing)
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  DiagnosticResult$new(
    name = "atividade_argila_alta",
    passed = passed, layers = passing,
    evidence = list(layers = details),
    missing = unique(missing),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p. 30"
  )
}


# ---- Saturacao por bases (V) -- eutrofico / distrofico --------------------

#' Solo eutrofico (SiBCS Cap 1, p 30)
#'
#' Returns TRUE se a saturacao por bases (V%) >= 50% no horizonte
#' diagnostico subsuperficial (B ou C). 65% para A chernozemico.
#'
#' @export
eutrofico <- function(pedon, min_v = 50) {
  h <- pedon$horizons
  # Procurar horizontes B (preferencial) ou C
  candidates <- which(!is.na(h$designation) &
                        grepl("^B|^C", h$designation))
  if (length(candidates) == 0L) candidates <- seq_len(nrow(h))
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in candidates) {
    v <- h$bs_pct[i]
    if (is.na(v)) { missing <- c(missing, "bs_pct"); next }
    details[[as.character(i)]] <- list(idx = i, bs_pct = v,
                                        threshold = min_v,
                                        passed = v >= min_v)
    if (v >= min_v) passing <- c(passing, i)
  }
  passed <- if (length(passing) > 0L) TRUE
            else if (length(details) == 0L && length(missing) > 0L) NA
            else FALSE
  DiagnosticResult$new(
    name = "eutrofico",
    passed = passed, layers = passing,
    evidence = list(layers = details), missing = unique(missing),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p. 30"
  )
}

#' Solo distrofico (SiBCS Cap 1, p 30)
#'
#' Negacao operacional de \code{\link{eutrofico}}: V < 50% no
#' horizonte diagnostico subsuperficial.
#' @export
distrofico <- function(pedon, max_v = 50) {
  e <- eutrofico(pedon, min_v = max_v)
  passed <- if (is.na(e$passed)) NA else !isTRUE(e$passed)
  DiagnosticResult$new(
    name = "distrofico", passed = passed,
    layers = integer(0),
    evidence = list(eutrofico = e),
    missing = e$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p. 30"
  )
}


# ---- Carater alitico (criterio de Argissolos / Nitossolos) ----------------

#' Carater alitico (SiBCS Cap 1, p 32)
#'
#' Critérios canônicos: Al(extr) >= 4 cmolc/kg solo, saturacao por
#' aluminio [100 * Al / (S + Al)] >= 50%, e saturacao por bases V < 50%.
#' Avaliado no horizonte B (ou C, na ausencia de B).
#'
#' @export
carater_alitico <- function(pedon, min_al = 4, min_al_sat = 50, max_v = 50) {
  h <- pedon$horizons
  candidates <- which(!is.na(h$designation) &
                        grepl("^B|^C", h$designation))
  if (length(candidates) == 0L) candidates <- seq_len(nrow(h))
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in candidates) {
    al <- h$al_cmol[i]
    v  <- h$bs_pct[i]
    s  <- (h$ca_cmol[i] %||% NA_real_) +
            (h$mg_cmol[i] %||% NA_real_) +
            (h$k_cmol[i]  %||% NA_real_) +
            (h$na_cmol[i] %||% NA_real_)
    if (is.na(al) || is.na(v)) {
      missing <- c(missing, c("al_cmol", "bs_pct")[c(is.na(al), is.na(v))])
      next
    }
    al_sat <- if (!is.na(s) && (s + al) > 0) 100 * al / (s + al) else NA_real_
    if (is.na(al_sat) && !is.na(h$al_sat_pct[i])) al_sat <- h$al_sat_pct[i]
    if (is.na(al_sat)) { missing <- c(missing, "al_sat_pct"); next }
    layer_pass <- al >= min_al && al_sat >= min_al_sat && v < max_v
    details[[as.character(i)]] <- list(
      idx = i, al_cmol = al, al_sat_pct = al_sat, bs_pct = v,
      threshold_al = min_al, threshold_al_sat = min_al_sat,
      threshold_v_max = max_v, passed = layer_pass
    )
    if (layer_pass) passing <- c(passing, i)
  }
  passed <- if (length(passing) > 0L) TRUE
            else if (length(details) == 0L && length(missing) > 0L) NA
            else FALSE
  DiagnosticResult$new(
    name = "carater_alitico", passed = passed, layers = passing,
    evidence = list(layers = details),
    missing = unique(missing),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p. 32"
  )
}


# ---- Carater carbonatico ---------------------------------------------------

#' Carater carbonatico (SiBCS Cap 1, p 33)
#'
#' >= 150 g/kg (15\%) de CaCO3 equivalente em qualquer forma de
#' segregacao (incl. nodulos, concrecoes). Excludente: nao satisfaz
#' aos requisitos de horizonte calcico.
#' @export
carater_carbonatico <- function(pedon, min_caco3_pct = 15) {
  h <- pedon$horizons
  res <- test_caco3_concentration(h, min_pct = min_caco3_pct)
  DiagnosticResult$new(
    name = "carater_carbonatico",
    passed = res$passed, layers = res$layers,
    evidence = list(caco3 = res), missing = res$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p. 33"
  )
}

#' Carater hipocarbonatico (SiBCS Cap 1, p 33): CaCO3 entre 50 e 150 g/kg.
#' @export
carater_hipocarbonatico <- function(pedon) {
  h <- pedon$horizons
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in seq_len(nrow(h))) {
    val <- h$caco3_pct[i]
    if (is.na(val)) { missing <- c(missing, "caco3_pct"); next }
    layer_pass <- val >= 5 && val < 15
    details[[as.character(i)]] <- list(idx = i, caco3_pct = val,
                                        passed = layer_pass)
    if (layer_pass) passing <- c(passing, i)
  }
  passed <- if (length(passing) > 0L) TRUE
            else if (length(details) == 0L && length(missing) > 0L) NA
            else FALSE
  DiagnosticResult$new(
    name = "carater_hipocarbonatico",
    passed = passed, layers = passing,
    evidence = list(layers = details), missing = unique(missing),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p. 33"
  )
}


# ---- Carater eutrico (subordem-diagnostic; pH alto + S alto) --------------

#' Carater eutrico (SiBCS Cap 1, p 35)
#'
#' Distinto de "eutrofico": exige pH(H2O) >= 5.7 conjugado com
#' S (soma de bases) >= 2.0 cmolc/kg solo dentro da secao de controle.
#' @export
carater_eutrico <- function(pedon, min_pH = 5.7, min_s = 2.0) {
  h <- pedon$horizons
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in seq_len(nrow(h))) {
    pH <- h$ph_h2o[i]
    s  <- (h$ca_cmol[i] %||% NA_real_) +
            (h$mg_cmol[i] %||% NA_real_) +
            (h$k_cmol[i]  %||% NA_real_) +
            (h$na_cmol[i] %||% NA_real_)
    if (is.na(pH) || is.na(s)) {
      missing <- c(missing, c("ph_h2o", "ca_cmol")[c(is.na(pH), is.na(s))])
      next
    }
    layer_pass <- pH >= min_pH && s >= min_s
    details[[as.character(i)]] <- list(idx = i, ph_h2o = pH, s_cmol = s,
                                        passed = layer_pass)
    if (layer_pass) passing <- c(passing, i)
  }
  passed <- if (length(passing) > 0L) TRUE
            else if (length(details) == 0L && length(missing) > 0L) NA
            else FALSE
  DiagnosticResult$new(
    name = "carater_eutrico", passed = passed, layers = passing,
    evidence = list(layers = details), missing = unique(missing),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p. 35"
  )
}


# ---- Carateres flúvico, plíntico, redóxico, sódico, sólódico, sálico ----

#' Carater fluvico (SiBCS Cap 1, p 35-36): camadas estratificadas +
#' distribuicao irregular de C organico. Reuso de fluvic_material (WRB).
#' @export
carater_fluvico <- function(pedon) {
  res <- fluvic_material(pedon)
  DiagnosticResult$new(
    name = "carater_fluvico", passed = res$passed,
    layers = res$layers, evidence = list(fluvic_material = res),
    missing = res$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p. 35"
  )
}

#' Carater plintico (SiBCS Cap 1, p 36): plintita >= 5% em quantidade
#' insuficiente para horizonte plintico.
#' @export
carater_plintico <- function(pedon, min_plinthite_pct = 5,
                                 max_plinthite_pct = 15) {
  h <- pedon$horizons
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in seq_len(nrow(h))) {
    val <- h$plinthite_pct[i]
    if (is.na(val)) { missing <- c(missing, "plinthite_pct"); next }
    layer_pass <- val >= min_plinthite_pct && val < max_plinthite_pct
    details[[as.character(i)]] <- list(idx = i, plinthite_pct = val,
                                        passed = layer_pass)
    if (layer_pass) passing <- c(passing, i)
  }
  passed <- if (length(passing) > 0L) TRUE
            else if (length(details) == 0L && length(missing) > 0L) NA
            else FALSE
  DiagnosticResult$new(
    name = "carater_plintico", passed = passed, layers = passing,
    evidence = list(layers = details), missing = unique(missing),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p. 36"
  )
}

#' Carater redoxico (SiBCS Cap 1, p 36-37): feicoes redoximorficas
#' em quantidade pelo menos comum, dentro da secao de controle.
#' \code{epirredoxico} se dentro de 50 cm; \code{endorredoxico} se
#' 50-150 cm.
#' @export
carater_redoxico <- function(pedon, min_redox_pct = 5, max_top_cm = 150) {
  h <- pedon$horizons
  layers_in_section <- which(!is.na(h$top_cm) & h$top_cm <= max_top_cm)
  test <- test_numeric_above(h, "redoximorphic_features_pct",
                                threshold = min_redox_pct,
                                candidate_layers = layers_in_section)
  position <- if (length(test$layers) > 0L &&
                    any(h$top_cm[test$layers] <= 50, na.rm = TRUE))
                "epirredoxico" else if (length(test$layers) > 0L)
                "endorredoxico" else NA_character_
  DiagnosticResult$new(
    name = "carater_redoxico", passed = test$passed,
    layers = test$layers,
    evidence = list(redox = test, position = position),
    missing = test$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p. 36"
  )
}

#' Carater sodico (SiBCS Cap 1, p 39): saturacao por sodio (PST) >= 15%.
#' @export
carater_sodico <- function(pedon, min_pst = 15) {
  h <- pedon$horizons
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in seq_len(nrow(h))) {
    cec <- h$cec_cmol[i]; na <- h$na_cmol[i]
    if (is.na(cec) || is.na(na) || cec <= 0) {
      missing <- c(missing, "cec_cmol", "na_cmol"); next
    }
    pst <- 100 * na / cec
    layer_pass <- pst >= min_pst
    details[[as.character(i)]] <- list(idx = i, na_cmol = na, cec_cmol = cec,
                                        pst_pct = pst, threshold = min_pst,
                                        passed = layer_pass)
    if (layer_pass) passing <- c(passing, i)
  }
  passed <- if (length(passing) > 0L) TRUE
            else if (length(details) == 0L && length(missing) > 0L) NA
            else FALSE
  DiagnosticResult$new(
    name = "carater_sodico", passed = passed, layers = passing,
    evidence = list(layers = details), missing = unique(missing),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p. 39"
  )
}

#' Carater solodico (SiBCS Cap 1, p 39): PST entre 6% e < 15%.
#' @export
carater_solodico <- function(pedon, min_pst = 6, max_pst = 15) {
  h <- pedon$horizons
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in seq_len(nrow(h))) {
    cec <- h$cec_cmol[i]; na <- h$na_cmol[i]
    if (is.na(cec) || is.na(na) || cec <= 0) {
      missing <- c(missing, "cec_cmol", "na_cmol"); next
    }
    pst <- 100 * na / cec
    layer_pass <- pst >= min_pst && pst < max_pst
    details[[as.character(i)]] <- list(idx = i, pst_pct = pst,
                                        passed = layer_pass)
    if (layer_pass) passing <- c(passing, i)
  }
  passed <- if (length(passing) > 0L) TRUE
            else if (length(details) == 0L && length(missing) > 0L) NA
            else FALSE
  DiagnosticResult$new(
    name = "carater_solodico", passed = passed, layers = passing,
    evidence = list(layers = details), missing = unique(missing),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p. 39"
  )
}

#' Carater salico (SiBCS Cap 1, p 38): CE >= 7 dS/m em alguma epoca.
#' @export
carater_salico <- function(pedon, min_ec = 7) {
  h <- pedon$horizons
  res <- test_ec_concentration(h, min_dS_m = min_ec)
  DiagnosticResult$new(
    name = "carater_salico", passed = res$passed,
    layers = res$layers,
    evidence = list(ec = res), missing = res$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p. 38"
  )
}

#' Carater salino (SiBCS Cap 1, p 39): 4 <= CE < 7 dS/m.
#' @export
carater_salino <- function(pedon, min_ec = 4, max_ec = 7) {
  h <- pedon$horizons
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in seq_len(nrow(h))) {
    val <- h$ec_dS_m[i]
    if (is.na(val)) { missing <- c(missing, "ec_dS_m"); next }
    layer_pass <- val >= min_ec && val < max_ec
    details[[as.character(i)]] <- list(idx = i, ec_dS_m = val,
                                        passed = layer_pass)
    if (layer_pass) passing <- c(passing, i)
  }
  passed <- if (length(passing) > 0L) TRUE
            else if (length(details) == 0L && length(missing) > 0L) NA
            else FALSE
  DiagnosticResult$new(
    name = "carater_salino", passed = passed, layers = passing,
    evidence = list(layers = details), missing = unique(missing),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p. 39"
  )
}


# ---- Mudanca textural abrupta (SiBCS-tunable) -----------------------------

#' Mudanca textural abrupta (SiBCS Cap 1, p 30-31)
#'
#' Aumento consideravel de argila em pequena distancia vertical
#' (\\<= 7.5 cm) na transicao A/E -> B:
#' \itemize{
#'   \item argila A < 200 g/kg: argila B \\>= 2x A; OR
#'   \item argila A 200-400 g/kg: incremento absoluto \\>= 200 g/kg
#'         (i.e. de 300 -> 500); OR
#'   \item argila A \\>= 400 g/kg: incremento absoluto \\>= 220 g/kg
#'         (i.e. de 420 -> 640).
#' }
#' Reuso de \code{\link{abrupt_textural_difference}} (WRB Ch 3.2.1)
#' que ja codifica criterios essencialmente equivalentes.
#' @export
mudanca_textural_abrupta <- function(pedon) {
  res <- abrupt_textural_difference(pedon)
  DiagnosticResult$new(
    name = "mudanca_textural_abrupta",
    passed = res$passed, layers = res$layers,
    evidence = list(abrupt = res), missing = res$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p. 30-31"
  )
}


# ---- Contato litico / litico fragmentario --------------------------------

#' Contato litico (SiBCS Cap 1, p 40): rocha continua dura. Reuso de
#' \code{\link{continuous_rock}} via designacao R / Cr.
#' @export
contato_litico <- function(pedon) {
  res <- continuous_rock(pedon)
  DiagnosticResult$new(
    name = "contato_litico", passed = res$passed,
    layers = res$layers, evidence = list(rock = res),
    missing = res$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p. 40"
  )
}

#' Contato litico fragmentario (SiBCS Cap 1, p 40): rocha fragmentada.
#' @export
contato_litico_fragmentario <- function(pedon) {
  h <- pedon$horizons
  res <- test_pattern_match(h, "designation", "^Cr|^Crf|^R/Cr|fragm")
  DiagnosticResult$new(
    name = "contato_litico_fragmentario",
    passed = res$passed, layers = res$layers,
    evidence = list(pattern = res), missing = res$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p. 40"
  )
}
