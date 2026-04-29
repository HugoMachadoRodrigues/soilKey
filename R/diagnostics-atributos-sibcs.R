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
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_caco3_pct Limite de CaCO3 (default 15\%).
#' @param max_depth_cm Profundidade maxima (\code{top_cm}) em que
#'        camadas qualificam (default \code{NULL} = sem restricao).
#'        SiBCS Cap 14 Subgrupos usam \code{max_depth_cm = 150}.
#' @export
carater_carbonatico <- function(pedon, min_caco3_pct = 15,
                                   max_depth_cm = NULL) {
  h <- pedon$horizons
  res <- test_caco3_concentration(h, min_pct = min_caco3_pct)
  layers <- res$layers
  if (!is.null(max_depth_cm) && length(layers) > 0L) {
    in_depth <- !is.na(h$top_cm[layers]) & h$top_cm[layers] < max_depth_cm
    layers <- layers[in_depth]
  }
  passed <- if (isTRUE(res$passed) && length(layers) == 0L) FALSE else res$passed
  DiagnosticResult$new(
    name = "carater_carbonatico",
    passed = passed, layers = layers,
    evidence = list(caco3 = res, max_depth_cm = max_depth_cm),
    missing = res$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p. 33"
  )
}

#' Carater hipocarbonatico (SiBCS Cap 1, p 33): CaCO3 entre 50 e 150 g/kg.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_depth_cm Profundidade maxima em que camadas qualificam
#'        (default \code{NULL} = sem restricao). SiBCS Cap 14 Subgrupos
#'        de Organossolos Haplicos Sapricos usam
#'        \code{max_depth_cm = 150}.
#' @export
carater_hipocarbonatico <- function(pedon, max_depth_cm = NULL) {
  h <- pedon$horizons
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in seq_len(nrow(h))) {
    val <- h$caco3_pct[i]
    if (is.na(val)) { missing <- c(missing, "caco3_pct"); next }
    if (!is.null(max_depth_cm) && !is.na(h$top_cm[i]) &&
          h$top_cm[i] >= max_depth_cm) next
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
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_pst PST minimo (\%) (default 15).
#' @param max_depth_cm Profundidade maxima em que camadas qualificam
#'        (default \code{NULL}). SiBCS Cap 14 Subgrupos usam 150.
#' @export
carater_sodico <- function(pedon, min_pst = 15, max_depth_cm = NULL) {
  h <- pedon$horizons
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in seq_len(nrow(h))) {
    cec <- h$cec_cmol[i]; na <- h$na_cmol[i]
    if (is.na(cec) || is.na(na) || cec <= 0) {
      missing <- c(missing, "cec_cmol", "na_cmol"); next
    }
    if (!is.null(max_depth_cm) && !is.na(h$top_cm[i]) &&
          h$top_cm[i] >= max_depth_cm) next
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
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_pst PST minimo (\%) (default 6).
#' @param max_pst PST maximo (\%) (default 15).
#' @param max_depth_cm Profundidade maxima em que camadas qualificam
#'        (default \code{NULL}). SiBCS Cap 14 Subgrupos usam 150.
#' @export
carater_solodico <- function(pedon, min_pst = 6, max_pst = 15,
                                max_depth_cm = NULL) {
  h <- pedon$horizons
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in seq_len(nrow(h))) {
    cec <- h$cec_cmol[i]; na <- h$na_cmol[i]
    if (is.na(cec) || is.na(na) || cec <= 0) {
      missing <- c(missing, "cec_cmol", "na_cmol"); next
    }
    if (!is.null(max_depth_cm) && !is.na(h$top_cm[i]) &&
          h$top_cm[i] >= max_depth_cm) next
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
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_ec Limite de CE em dS/m (default 7).
#' @param max_depth_cm Profundidade maxima em que camadas qualificam
#'        (default \code{NULL}). SiBCS Cap 14 Subgrupos usam 150.
#' @export
carater_salico <- function(pedon, min_ec = 7, max_depth_cm = NULL) {
  h <- pedon$horizons
  res <- test_ec_concentration(h, min_dS_m = min_ec)
  layers <- res$layers
  if (!is.null(max_depth_cm) && length(layers) > 0L) {
    in_depth <- !is.na(h$top_cm[layers]) & h$top_cm[layers] < max_depth_cm
    layers <- layers[in_depth]
  }
  passed <- if (isTRUE(res$passed) && length(layers) == 0L) FALSE else res$passed
  DiagnosticResult$new(
    name = "carater_salico", passed = passed,
    layers = layers,
    evidence = list(ec = res, max_depth_cm = max_depth_cm),
    missing = res$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p. 38"
  )
}

#' Carater salino (SiBCS Cap 1, p 39): 4 <= CE < 7 dS/m.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_ec Limite inferior de CE em dS/m (default 4).
#' @param max_ec Limite superior (exclusivo) (default 7).
#' @param max_depth_cm Profundidade maxima em que camadas qualificam
#'        (default \code{NULL}). SiBCS Cap 14 Subgrupos usam 150.
#' @export
carater_salino <- function(pedon, min_ec = 4, max_ec = 7,
                              max_depth_cm = NULL) {
  h <- pedon$horizons
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in seq_len(nrow(h))) {
    val <- h$ec_dS_m[i]
    if (is.na(val)) { missing <- c(missing, "ec_dS_m"); next }
    if (!is.null(max_depth_cm) && !is.na(h$top_cm[i]) &&
          h$top_cm[i] >= max_depth_cm) next
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
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_depth_cm Profundidade maxima do contato (default
#'        \code{NULL}). SiBCS Cap 14 Subgrupos liticos de Folicos usam
#'        \code{max_depth_cm = 50}.
#' @export
contato_litico <- function(pedon, max_depth_cm = NULL) {
  h <- pedon$horizons
  res <- continuous_rock(pedon)
  layers <- res$layers
  if (!is.null(max_depth_cm) && length(layers) > 0L) {
    in_depth <- !is.na(h$top_cm[layers]) & h$top_cm[layers] < max_depth_cm
    layers <- layers[in_depth]
  }
  passed <- if (isTRUE(res$passed) && length(layers) == 0L) FALSE else res$passed
  DiagnosticResult$new(
    name = "contato_litico", passed = passed,
    layers = layers, evidence = list(rock = res, max_depth_cm = max_depth_cm),
    missing = res$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p. 40"
  )
}

#' Contato litico fragmentario (SiBCS Cap 1, p 40): rocha fragmentada.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_depth_cm Profundidade maxima do contato (default
#'        \code{NULL}). SiBCS Cap 14 Subgrupos fragmentarios de Folicos
#'        usam \code{max_depth_cm = 50}.
#' @export
contato_litico_fragmentario <- function(pedon, max_depth_cm = NULL) {
  h <- pedon$horizons
  res <- test_pattern_match(h, "designation", "^Cr|^Crf|^R/Cr|fragm")
  layers <- res$layers
  if (!is.null(max_depth_cm) && length(layers) > 0L) {
    in_depth <- !is.na(h$top_cm[layers]) & h$top_cm[layers] < max_depth_cm
    layers <- layers[in_depth]
  }
  passed <- if (isTRUE(res$passed) && length(layers) == 0L) FALSE else res$passed
  DiagnosticResult$new(
    name = "contato_litico_fragmentario",
    passed = passed, layers = layers,
    evidence = list(pattern = res, max_depth_cm = max_depth_cm),
    missing = res$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p. 40"
  )
}


# ============================================================================
# v0.7.2: SiBCS pendentes
#
# Sete diagnosticos prontos para usar nos niveis 3o-4o (Grandes Grupos /
# Subgrupos) e nas chamadas de B_latossolico / B_nitico:
#
#   1. saprico / hemico / fibrico -- grau de decomposicao do material
#      organico em horizontes histicos. Cap 14 (Organossolos) usa no 3o
#      nivel para distinguir Organossolos Sapricos / Hemicos / Fibricos.
#
#   2. carater_acrico -- DeltapH (KCl - H2O) >= 0 e CECef <= 1.5 cmolc/kg
#      argila em horizontes B. Cap 1, p 31; Cap 10 (Latossolos Acricos).
#
#   3. carater_ebanico -- preto (value <= 3 e chroma <= 2 em umido) +
#      atividade da argila Ta + V% >= 65 em TODO horizonte B. Cap 7
#      (Chernossolos Ebanicos) e Cap 17 (Vertissolos Ebanicos).
#
#   4. carater_retratil -- COLE >= 0.06 OU slickensides + cracks. Cap 1,
#      p 33; usado por Vertissolos / Cambissolos / Argissolos retrateis.
#
#   5. carater_espodico -- evidencia iluvial de Al/Fe/MO em camada
#      >= 2.5 cm, insuficiente para B espodico mas indicando
#      espodicidade. Cap 1, p 35.
#
#   6. compute_ki / compute_kr / latossolo_ki_kr -- indices molares
#      SiO2/Al2O3 e SiO2/(Al2O3+Fe2O3) por ataque sulfurico-NaOH
#      (Embrapa Manual de Metodos). Limites canonicos para Latossolos:
#      Ki <= 2.2 e Kr <= 1.7 (Cap 10, p 173-176).
#
#   7. cerosidade -- diagnostico parametrizado quantidade x intensidade,
#      consumindo as colunas v0.7.2 clay_films_amount + clay_films_strength
#      (substituem o legado clay_films). Discriminante critico
#      Nitossolos vs Argissolos no Cap 13 (>= comum + >= moderada).
# ============================================================================


# ---- Three-valued ALL helper ---------------------------------------------

#' Three-valued ALL across a logical vector, NA-aware
#'
#' Returns FALSE if any element is exactly FALSE; TRUE if every element is
#' exactly TRUE; NA if no FALSE but at least one NA. Used inside SiBCS
#' pendente diagnostics that combine per-layer tests with proper
#' propagation.
#' @keywords internal
.three_valued_all <- function(x) {
  if (length(x) == 0L) return(NA)
  if (any(x %in% FALSE)) return(FALSE)
  if (all(x %in% TRUE))  return(TRUE)
  NA
}


# ---- Grau de decomposicao do material organico (von Post / fibras) -------

#' Classifica grau de decomposicao por camada: saprico / hemico / fibrico
#'
#' SiBCS Cap 14 adota o criterio USDA Soil Taxonomy:
#'
#'   Saprico:  < 17\% fibras esfregadas  ou  von Post H7-H10
#'   Hemico:   17-40\% fibras            ou  von Post H5-H6
#'   Fibrico:  >= 40\% fibras            ou  von Post H1-H4
#'
#' @keywords internal
.classify_decomposition <- function(fiber_pct, von_post) {
  out <- rep(NA_character_, length(fiber_pct))
  for (i in seq_along(fiber_pct)) {
    fp <- fiber_pct[i]; vp <- von_post[i]
    cls <- if (!is.na(fp)) {
      if (fp >= 40)      "fibrico"
      else if (fp >= 17) "hemico"
      else               "saprico"
    } else if (!is.na(vp)) {
      if (vp <= 4)       "fibrico"
      else if (vp <= 6)  "hemico"
      else               "saprico"
    } else NA_character_
    out[i] <- cls
  }
  out
}


.histic_layers <- function(h) {
  # Horizontes histicos canonicos: H (saturado) ou O (folico).
  which(!is.na(h$designation) & grepl("^[HO]", h$designation))
}


.decomposition_diagnostic <- function(pedon, target,
                                         page = "Cap 14, pp 224-226") {
  h <- pedon$horizons
  hist_idx <- .histic_layers(h)
  if (length(hist_idx) == 0L) {
    return(DiagnosticResult$new(
      name = paste0(target, "_decomposicao"), passed = FALSE,
      layers = integer(0),
      evidence = list(reason = "no histic (H/O) layers"),
      missing = character(0),
      reference = paste0("Embrapa (2018), SiBCS 5a ed., ", page),
      notes = "Sem horizontes H/O -- grau de decomposicao nao se aplica."
    ))
  }
  fiber <- h$fiber_content_rubbed_pct[hist_idx]
  vp    <- h$von_post_index[hist_idx]
  cls   <- .classify_decomposition(fiber, vp)

  passing <- hist_idx[!is.na(cls) & cls == target]
  passed  <- length(passing) > 0L
  missing <- if (all(is.na(fiber)) && all(is.na(vp)))
               c("fiber_content_rubbed_pct", "von_post_index")
             else character(0)
  DiagnosticResult$new(
    name = paste0(target, "_decomposicao"),
    passed = passed, layers = passing,
    evidence = list(
      histic_layers            = hist_idx,
      fiber_content_rubbed_pct = fiber,
      von_post_index           = vp,
      classification           = cls
    ),
    missing = missing,
    reference = paste0("Embrapa (2018), SiBCS 5a ed., ", page)
  )
}


#' Material organico saprico (SiBCS Cap 14)
#'
#' Material organico altamente decomposto: < 17\% de fibras esfregadas
#' OU indice de von Post H7-H10. Discrimina Organossolos Sapricos no
#' 3o nivel categorico.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return \code{\link{DiagnosticResult}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 14 (Organossolos),
#'             pp 224-226.
#' @export
saprico <- function(pedon) {
  .decomposition_diagnostic(pedon, "saprico")
}


#' Material organico hemico (SiBCS Cap 14)
#'
#' Material organico em decomposicao intermediaria: 17-40\% de fibras
#' esfregadas OU indice de von Post H5-H6. Discrimina Organossolos
#' Hemicos no 3o nivel.
#' @inherit saprico
#' @export
hemico <- function(pedon) {
  .decomposition_diagnostic(pedon, "hemico")
}


#' Material organico fibrico (SiBCS Cap 14)
#'
#' Material organico pouco decomposto: >= 40\% de fibras esfregadas
#' OU indice de von Post H1-H4. Discrimina Organossolos Fibricos no
#' 3o nivel.
#' @inherit saprico
#' @export
fibrico <- function(pedon) {
  .decomposition_diagnostic(pedon, "fibrico")
}


# ---- Carater acrico (DeltapH >= 0 e CECef baixa) -------------------------

#' Carater acrico (SiBCS Cap 1, p 31)
#'
#' Indica solos com balanca de cargas predominante eletropositiva ou
#' eletricamente neutra. Discrimina Latossolos Acricos / Acriferricos no
#' 3o nivel (Cap 10).
#'
#' Criterios canonicos (todos verificados em horizontes B):
#'
#'   1. \eqn{\Delta pH = pH(KCl) - pH(H_2O) \ge 0}
#'   2. CECef por kg de argila \eqn{\le} 1.5 cmolc/kg argila
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_ecec_clay Limite superior de CECef/argila em cmolc/kg
#'        argila (default 1.5).
#' @param min_delta_ph Limite inferior de \eqn{\Delta pH} (default 0).
#' @return \code{\link{DiagnosticResult}}; \code{passed = TRUE} se
#'         pelo menos um horizonte B satisfaz ambos os criterios.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 1, p 31; Cap 10
#'             (Latossolos), pp 173-176.
#' @export
carater_acrico <- function(pedon,
                              max_ecec_clay = 1.5,
                              min_delta_ph  = 0) {
  h <- pedon$horizons
  b_layers <- which(!is.na(h$designation) & grepl("^B", h$designation))
  if (length(b_layers) == 0L) {
    return(DiagnosticResult$new(
      name = "carater_acrico", passed = FALSE, layers = integer(0),
      evidence = list(reason = "no B horizons"),
      missing = "designation",
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p 31"
    ))
  }
  details <- list(); passing <- integer(0); missing <- character(0)
  evaluated <- 0L
  for (i in b_layers) {
    pkcl <- h$ph_kcl[i]; ph2o <- h$ph_h2o[i]
    ecec <- h$ecec_cmol[i]; clay <- h$clay_pct[i]
    if (is.na(pkcl) || is.na(ph2o)) {
      missing <- c(missing, "ph_kcl", "ph_h2o"); next
    }
    if (is.na(ecec) || is.na(clay) || clay <= 0) {
      missing <- c(missing, "ecec_cmol", "clay_pct"); next
    }
    delta_ph  <- pkcl - ph2o
    ecec_clay <- ecec * 100 / clay   # cmolc/kg argila
    pass <- delta_ph >= min_delta_ph && ecec_clay <= max_ecec_clay
    details[[as.character(i)]] <- list(
      idx = i, ph_h2o = ph2o, ph_kcl = pkcl, delta_ph = delta_ph,
      ecec_cmol = ecec, clay_pct = clay, ecec_per_kg_clay = ecec_clay,
      passed = pass
    )
    evaluated <- evaluated + 1L
    if (pass) passing <- c(passing, i)
  }
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  DiagnosticResult$new(
    name = "carater_acrico", passed = passed, layers = passing,
    evidence = list(layers = details,
                      max_ecec_clay = max_ecec_clay,
                      min_delta_ph  = min_delta_ph),
    missing = unique(missing),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p 31"
  )
}


# ---- Carater ebanico (preto + Ta + V alta) -------------------------------

#' Carater ebanico (SiBCS Cap 1; Cap 7 e Cap 17)
#'
#' Cor preta uniforme (value \eqn{\le} 3 e chroma \eqn{\le} 2 em umido) em
#' TODO o horizonte B + atividade da argila alta (Ta) + saturacao por
#' bases V\% \eqn{\ge} 65. Discrimina Chernossolos Ebanicos (Cap 7) e
#' Vertissolos Ebanicos (Cap 17) no 2o nivel.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_value Limite superior de Munsell value em umido (default 3).
#' @param max_chroma Limite superior de chroma em umido (default 2).
#' @param min_v Limite inferior de V\% (default 65).
#' @return \code{\link{DiagnosticResult}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 1; Cap 7
#'             (Chernossolos), pp 144-148; Cap 17 (Vertissolos),
#'             pp 271-274.
#' @export
carater_ebanico <- function(pedon,
                               max_value  = 3,
                               max_chroma = 2,
                               min_v      = 65) {
  h <- pedon$horizons
  b_layers <- which(!is.na(h$designation) & grepl("^B", h$designation))
  if (length(b_layers) == 0L) {
    return(DiagnosticResult$new(
      name = "carater_ebanico", passed = FALSE, layers = integer(0),
      evidence = list(reason = "no B horizons"),
      missing = "designation",
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 1"
    ))
  }
  ta <- atividade_argila_alta(pedon)
  details <- list(); missing <- character(0)
  preto_results <- v_results <- rep(NA, length(b_layers))
  for (k in seq_along(b_layers)) {
    i <- b_layers[k]
    val <- h$munsell_value_moist[i]; chr <- h$munsell_chroma_moist[i]
    bs  <- h$bs_pct[i]
    if (!is.na(val) && !is.na(chr)) {
      preto_results[k] <- val <= max_value && chr <= max_chroma
    } else {
      missing <- c(missing, "munsell_value_moist", "munsell_chroma_moist")
    }
    if (!is.na(bs)) {
      v_results[k] <- bs >= min_v
    } else {
      missing <- c(missing, "bs_pct")
    }
    details[[as.character(i)]] <- list(
      idx = i, value = val, chroma = chr, bs_pct = bs,
      preto = preto_results[k], v_ge_min = v_results[k]
    )
  }
  preto_all <- .three_valued_all(preto_results)
  v_all     <- .three_valued_all(v_results)
  combined  <- c(preto_all, v_all, ta$passed)
  passed <- if (any(combined %in% FALSE)) FALSE
            else if (all(combined %in% TRUE)) TRUE
            else NA
  DiagnosticResult$new(
    name = "carater_ebanico", passed = passed,
    layers = if (isTRUE(passed)) b_layers else integer(0),
    evidence = list(layers = details,
                      atividade_argila_alta = ta,
                      preto_all = preto_all,
                      v_all = v_all,
                      max_value = max_value, max_chroma = max_chroma,
                      min_v = min_v),
    missing = unique(c(missing, ta$missing %||% character(0))),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 1; Cap 7; Cap 17"
  )
}


# ---- Carater retratil (COLE / contracao) ---------------------------------

#' Carater retratil (SiBCS Cap 1, p 33)
#'
#' Solos com retracao significativa quando secos: COLE \eqn{\ge} 0,06
#' sobre a secao de controle, OU presenca de slickensides + fendas
#' (cracks) suficientemente desenvolvidas. Discrimina Cambissolos
#' retrateis (Cap 6), Vertissolos (Cap 17) e Argissolos retrateis
#' (Cap 5).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_cole Limite inferior de COLE (default 0,06).
#' @param min_crack_width Largura minima de fenda em cm para o caminho
#'        slickensides+cracks (default 1).
#' @return \code{\link{DiagnosticResult}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 1, p 33.
#' @export
carater_retratil <- function(pedon,
                                min_cole        = 0.06,
                                min_crack_width = 1) {
  h <- pedon$horizons
  cole     <- h$cole_value
  cracks_w <- h$cracks_width_cm
  slks     <- h$slickensides
  cole_ok   <- !is.na(cole) & cole >= min_cole
  cracks_ok <- !is.na(cracks_w) & cracks_w >= min_crack_width
  slick_ok  <- !is.na(slks) & grepl("few|common|many|continuous",
                                       slks, ignore.case = TRUE)
  passed_layers <- which(cole_ok | (slick_ok & cracks_ok))
  passed <- length(passed_layers) > 0L
  missing <- if (all(is.na(cole)) && all(is.na(cracks_w)) &&
                  all(is.na(slks)))
               c("cole_value", "cracks_width_cm", "slickensides")
             else character(0)
  DiagnosticResult$new(
    name = "carater_retratil", passed = passed, layers = passed_layers,
    evidence = list(cole_value = cole, cracks_width_cm = cracks_w,
                      slickensides = slks,
                      min_cole = min_cole,
                      min_crack_width = min_crack_width),
    missing = missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p 33"
  )
}


# ---- Carater espodico (subsuperficial, incipiente) -----------------------

#' Carater espodico (SiBCS Cap 1, p 35; Cap 8)
#'
#' Evidencia iluvial de Al / Fe / materia organica em camada de pelo
#' menos 2,5 cm de espessura, em quantidade insuficiente para qualificar
#' como horizonte B espodico (\code{\link{B_espodico}}), mas suficiente
#' para indicar espodicidade incipiente. Usado em Cambissolos /
#' Argissolos / Plintossolos espodicos (Caps 5, 6 e 16) e em
#' Espodossolos rasos (Cap 8).
#'
#' Diferenca para \code{\link{B_espodico}}: thickness >= 2,5 cm em vez
#' de exigir o gate completo de espessura espodica; OC >= 0,5\% em vez
#' do gate de iluviacao quantitativa; sinais de iluviacao Fe/Al
#' (\code{al_ox_pct} ou \code{fe_ox_pct} ou \code{fe_dcb_pct}).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Espessura minima da camada espodica incipiente
#'        em cm (default 2,5).
#' @param min_oc_pct OC\% minimo em camada candidata (default 0,5).
#' @return \code{\link{DiagnosticResult}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 1, p 35; Cap 8
#'             (Espodossolos), pp 156-160.
#' @export
carater_espodico <- function(pedon,
                                min_thickness = 2.5,
                                min_oc_pct    = 0.5) {
  h <- pedon$horizons
  candidate <- which(!is.na(h$designation) &
                       grepl("^Bh|^Bs|^Bsh|^Bhs", h$designation))
  if (length(candidate) == 0L) {
    candidate <- which(!is.na(h$designation) & grepl("^B", h$designation))
  }
  if (length(candidate) == 0L) {
    return(DiagnosticResult$new(
      name = "carater_espodico", passed = FALSE, layers = integer(0),
      evidence = list(reason = "no B horizons"),
      missing = "designation",
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p 35"
    ))
  }
  thickness <- h$bottom_cm[candidate] - h$top_cm[candidate]
  oc        <- h$oc_pct[candidate]
  fe_dcb    <- h$fe_dcb_pct[candidate]
  al_ox     <- h$al_ox_pct[candidate]
  fe_ox     <- h$fe_ox_pct[candidate]
  thick_ok  <- !is.na(thickness) & thickness >= min_thickness
  oc_ok     <- !is.na(oc) & oc >= min_oc_pct
  iluv_ok   <- (!is.na(al_ox)  & al_ox  >= 0.2) |
               (!is.na(fe_ox)  & fe_ox  >= 0.1) |
               (!is.na(fe_dcb) & fe_dcb >= 0.5)
  pass <- thick_ok & oc_ok & iluv_ok
  passed_layers <- candidate[pass]
  passed <- length(passed_layers) > 0L
  missing <- character(0)
  if (all(is.na(oc))) missing <- c(missing, "oc_pct")
  if (all(is.na(al_ox)) && all(is.na(fe_ox)) && all(is.na(fe_dcb)))
    missing <- c(missing, "al_ox_pct", "fe_ox_pct", "fe_dcb_pct")
  DiagnosticResult$new(
    name = "carater_espodico", passed = passed, layers = passed_layers,
    evidence = list(candidate_layers = candidate,
                      thickness_cm = thickness, oc_pct = oc,
                      al_ox_pct = al_ox, fe_ox_pct = fe_ox,
                      fe_dcb_pct = fe_dcb,
                      min_thickness = min_thickness,
                      min_oc_pct    = min_oc_pct),
    missing = unique(missing),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p 35"
  )
}


# ---- Ki / Kr quantitativos (ataque sulfurico) ----------------------------

#' Ki (silica:alumina molar) -- SiBCS Cap 1, p 32
#'
#' Calcula o indice molar Ki = SiO2 / Al2O3 a partir de teores
#' percentuais por ataque sulfurico-NaOH (Embrapa Manual de Metodos).
#' Massas molares: 60.08 (SiO2), 101.96 (Al2O3):
#'
#'   Ki (molar) = (\% SiO2 / 60.08) / (\% Al2O3 / 101.96)
#'              \eqn{\approx} 1.6973 \eqn{\times} (\% SiO2 / \% Al2O3)
#'
#' @param sio2_pct Teor de SiO2 por ataque sulfurico (\%).
#' @param al2o3_pct Teor de Al2O3 por ataque sulfurico (\%).
#' @return Ki molar (numeric); NA se algum input for NA ou Al2O3 \eqn{\le} 0.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 1, p 32; Embrapa Manual
#'             de Metodos de Analise de Solo (3a ed., 2017).
#' @export
compute_ki <- function(sio2_pct, al2o3_pct) {
  ifelse(is.na(sio2_pct) | is.na(al2o3_pct) | al2o3_pct <= 0,
           NA_real_,
           (sio2_pct / 60.08) / (al2o3_pct / 101.96))
}


#' Kr (silica:sesquioxidos molar) -- SiBCS Cap 1, p 32
#'
#' Calcula o indice molar Kr = SiO2 / (Al2O3 + Fe2O3) usando massas
#' molares 60.08 (SiO2), 101.96 (Al2O3) e 159.69 (Fe2O3):
#'
#'   Kr (molar) = (\% SiO2 / 60.08) /
#'                (\% Al2O3 / 101.96 + \% Fe2O3 / 159.69)
#'
#' @param sio2_pct Teor de SiO2 por ataque sulfurico (\%).
#' @param al2o3_pct Teor de Al2O3 por ataque sulfurico (\%).
#' @param fe2o3_pct Teor de Fe2O3 por ataque sulfurico (\%).
#' @return Kr molar (numeric); NA se algum input for NA ou denominador
#'         \eqn{\le} 0.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 1, p 32.
#' @export
compute_kr <- function(sio2_pct, al2o3_pct, fe2o3_pct) {
  denom <- al2o3_pct / 101.96 + fe2o3_pct / 159.69
  ifelse(is.na(sio2_pct) | is.na(al2o3_pct) | is.na(fe2o3_pct) |
            denom <= 0,
           NA_real_,
           (sio2_pct / 60.08) / denom)
}


#' Ki/Kr para Latossolos (SiBCS Cap 10, p 173-176)
#'
#' Diagnostico SiBCS estrito sobre o B latossolico: requer Ki
#' \eqn{\le} max_ki em todos os horizontes B avaliados, e Kr
#' \eqn{\le} max_kr quando Fe2O3 estiver disponivel. Sub-classes
#' acricas (Latossolos Acricos) e acriferricas adicionalmente exigem
#' \code{\link{carater_acrico}}.
#'
#' Quando os campos de ataque sulfurico
#' (\code{sio2_sulfuric_pct}, \code{al2o3_sulfuric_pct},
#' \code{fe2o3_sulfuric_pct}) estao todos NA, o diagnostico retorna
#' \code{passed = NA} com \code{missing} explicito.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_ki Ki limite superior (default 2.2 -- limite kaolinitico
#'        SiBCS Cap 10).
#' @param max_kr Kr limite superior (default 1.7).
#' @return \code{\link{DiagnosticResult}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 10 (Latossolos),
#'             pp 173-176.
#' @export
latossolo_ki_kr <- function(pedon, max_ki = 2.2, max_kr = 1.7) {
  h <- pedon$horizons
  b_layers <- which(!is.na(h$designation) & grepl("^B", h$designation))
  if (length(b_layers) == 0L) {
    return(DiagnosticResult$new(
      name = "latossolo_ki_kr", passed = FALSE, layers = integer(0),
      evidence = list(reason = "no B horizons"),
      missing = "designation",
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 10, p 173-176"
    ))
  }
  sio2  <- h$sio2_sulfuric_pct[b_layers]
  al2o3 <- h$al2o3_sulfuric_pct[b_layers]
  fe2o3 <- h$fe2o3_sulfuric_pct[b_layers]
  ki <- compute_ki(sio2, al2o3)
  kr <- compute_kr(sio2, al2o3, fe2o3)
  ki_ok <- !is.na(ki) & ki <= max_ki
  # Kr is optional: only check when fe2o3 present (kr non-NA).
  kr_ok <- is.na(kr) | kr <= max_kr
  passing <- b_layers[ki_ok & kr_ok]
  evaluated <- sum(!is.na(ki))
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L) NA
            else FALSE
  missing <- character(0)
  if (all(is.na(ki)))
    missing <- c(missing, "sio2_sulfuric_pct", "al2o3_sulfuric_pct")
  if (all(is.na(kr)))
    missing <- c(missing, "fe2o3_sulfuric_pct")
  DiagnosticResult$new(
    name = "latossolo_ki_kr", passed = passed, layers = passing,
    evidence = list(b_layers           = b_layers,
                      sio2_sulfuric_pct  = sio2,
                      al2o3_sulfuric_pct = al2o3,
                      fe2o3_sulfuric_pct = fe2o3,
                      ki = ki, kr = kr,
                      max_ki = max_ki, max_kr = max_kr),
    missing = unique(missing),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 10, p 173-176"
  )
}


# ---- Cerosidade quantitativa (clay films amount x strength) --------------

# Look-up tables for ordinal mapping. Aceita PT-BR e EN; "shiny" -> "strong".
# Usar list() (nao c()) garante que `[[chave_inexistente]]` devolva NULL em
# vez de erro `subscript out of bounds`, o que permite a validacao explicita
# em cerosidade() reportar mensagens claras quando o usuario passa um termo
# desconhecido em min_amount / min_strength.
.cerosidade_amount_levels <- list(
  few = 1L, common = 2L, many = 3L, continuous = 4L,
  pouca = 1L, comum = 2L, abundante = 3L, continua = 4L
)
.cerosidade_strength_levels <- list(
  weak = 1L, moderate = 2L, strong = 3L,
  fraca = 1L, moderada = 2L, forte = 3L,
  shiny = 3L
)


.rank_term <- function(x, levels) {
  out <- rep(NA_integer_, length(x))
  if (is.null(x) || all(is.na(x))) return(out)
  nm <- tolower(trimws(x))
  for (i in seq_along(nm)) {
    if (is.na(nm[i])) next
    v <- levels[[nm[i]]]
    if (!is.null(v)) out[i] <- v
  }
  out
}


#' Cerosidade quantitativa (SiBCS Cap 13, p 207; Cap 1)
#'
#' Diagnostico parametrizado quantidade x intensidade de cerosidade
#' (clay films / cutans). Consume as colunas v0.7.2
#' \code{clay_films_amount} (ordinal: few/pouca, common/comum,
#' many/abundante, continuous/continua) e \code{clay_films_strength}
#' (ordinal: weak/fraca, moderate/moderada, strong/forte; "shiny"
#' mapeado a "strong"), introduzidas em substituicao ao legado
#' \code{clay_films}.
#'
#' Discriminante critico Nitossolos vs Argissolos no Cap 13:
#' Nitossolos exigem cerosidade \eqn{\ge} comum + \eqn{\ge} moderada
#' (defaults).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_amount Quantidade minima: \code{"few"}, \code{"common"},
#'        \code{"many"}, \code{"continuous"} (ou equivalentes em PT-BR).
#'        Default \code{"common"}.
#' @param min_strength Intensidade minima: \code{"weak"},
#'        \code{"moderate"}, \code{"strong"}. Default \code{"moderate"}.
#'        Pass \code{NULL} para ignorar a dimensao de intensidade.
#' @return \code{\link{DiagnosticResult}}; \code{passed = TRUE} se ao
#'         menos um horizonte B atende ambos os limiares.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 13 (Nitossolos), p 207;
#'             Cap 1 (atributos diagnosticos).
#' @export
cerosidade <- function(pedon,
                        min_amount   = "common",
                        min_strength = "moderate") {
  min_amt_rank <- .cerosidade_amount_levels[[tolower(min_amount)]]
  if (is.null(min_amt_rank))
    rlang::abort(sprintf("Unknown min_amount '%s'", min_amount))
  if (!is.null(min_strength)) {
    min_str_rank <- .cerosidade_strength_levels[[tolower(min_strength)]]
    if (is.null(min_str_rank))
      rlang::abort(sprintf("Unknown min_strength '%s'", min_strength))
  } else {
    min_str_rank <- 0L
  }

  h <- pedon$horizons
  b_layers <- which(!is.na(h$designation) & grepl("^B", h$designation))
  if (length(b_layers) == 0L) {
    return(DiagnosticResult$new(
      name = "cerosidade", passed = FALSE, layers = integer(0),
      evidence = list(reason = "no B horizons"),
      missing = "designation",
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 13, p 207"
    ))
  }
  amt <- h$clay_films_amount[b_layers]
  str <- h$clay_films_strength[b_layers]
  amt_rank <- .rank_term(amt, .cerosidade_amount_levels)
  str_rank <- .rank_term(str, .cerosidade_strength_levels)
  amt_ok <- !is.na(amt_rank) & amt_rank >= min_amt_rank
  str_ok <- if (is.null(min_strength))
              rep(TRUE, length(b_layers))
            else
              !is.na(str_rank) & str_rank >= min_str_rank
  passing <- b_layers[amt_ok & str_ok]
  passed <- length(passing) > 0L
  missing <- character(0)
  if (all(is.na(amt))) missing <- c(missing, "clay_films_amount")
  if (!is.null(min_strength) && all(is.na(str)))
    missing <- c(missing, "clay_films_strength")
  DiagnosticResult$new(
    name = "cerosidade", passed = passed, layers = passing,
    evidence = list(b_layers = b_layers,
                      clay_films_amount   = amt,
                      clay_films_strength = str,
                      amount_rank   = amt_rank,
                      strength_rank = str_rank,
                      min_amount = min_amount,
                      min_strength = min_strength),
    missing = unique(missing),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 13, p 207"
  )
}


# ============================================================================
# v0.7.3: SiBCS Cap 14 -- 2 novos atributos para os Subgrupos de Organossolos
# ============================================================================
#
# carater_terrico       -- horizontes minerais (A/Ag/Big/Cg) totalizando
#                          >= 30 cm dentro de 100 cm da superficie. Cap 14
#                          subgrupos terricos.
#
# carater_cambissolico  -- B_incipiente abaixo do hístico ou A. Cap 14
#                          subgrupos cambissolicos (Folicos).
# ============================================================================


# ---- Carater terrico (Cap 14, p 246) -------------------------------------

#' Carater terrico (SiBCS Cap 14)
#'
#' Solos com horizontes ou camadas constituidos por materiais minerais
#' (horizonte A, Ag, Big e/ou Cg), com espessura cumulativa
#' \eqn{\ge} \code{min_thickness_cm} dentro de \code{within_depth_cm}
#' da superficie do solo. Discrimina os Subgrupos terricos de
#' Organossolos (Cap 14, pp 245-250) e Cambissolos terricos (Cap 6).
#'
#' Padroes de designacao reconhecidos para horizonte mineral:
#' \itemize{
#'   \item \code{A}, \code{Ap}, \code{An} (mineral superficial)
#'   \item \code{Ag} (mineral hidromorfico)
#'   \item \code{Big}, \code{Bg} (B mineral hidromorfico)
#'   \item \code{Cg} (C mineral hidromorfico)
#'   \item \code{C}, \code{Cr}, \code{Crf} (mineral subsuperficial)
#' }
#'
#' Excluidos do somatorio: horizontes histicos (\code{H*}, \code{O*})
#' e horizontes cementados puros sem material mineral.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness_cm Espessura cumulativa minima de material
#'        mineral (default 30 cm).
#' @param within_depth_cm Profundidade de busca (default 100 cm).
#' @return \code{\link{DiagnosticResult}}; \code{passed = TRUE} se a
#'         soma da espessura dos horizontes minerais (truncada em
#'         \code{within_depth_cm}) for \eqn{\ge}
#'         \code{min_thickness_cm}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 14, p 246
#'             (subgrupos terricos de Organossolos).
#' @export
carater_terrico <- function(pedon,
                               min_thickness_cm = 30,
                               within_depth_cm  = 100) {
  h <- pedon$horizons
  if (nrow(h) == 0L) {
    return(DiagnosticResult$new(
      name = "carater_terrico", passed = FALSE, layers = integer(0),
      evidence = list(reason = "empty horizons"),
      missing = "designation",
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 14, p 246"
    ))
  }
  # Mineral horizon designation pattern -- aceita A/Ap/An, Ag, Big/Bg, Cg/C/Cr.
  # Exclui horizontes histicos H/O.
  is_mineral <- !is.na(h$designation) &
                  grepl("^(A|Ag|Big|Bg|C|Cg|Cr|Crf)", h$designation) &
                  !grepl("^[HO]", h$designation)
  if (!any(is_mineral)) {
    return(DiagnosticResult$new(
      name = "carater_terrico", passed = FALSE, layers = integer(0),
      evidence = list(reason = "no mineral horizons in profile"),
      missing = character(0),
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 14, p 246"
    ))
  }
  mineral_idx <- which(is_mineral)
  # Truncate thickness contribution to within_depth_cm.
  top  <- h$top_cm[mineral_idx]
  bot  <- h$bottom_cm[mineral_idx]
  if (any(is.na(top)) || any(is.na(bot))) {
    return(DiagnosticResult$new(
      name = "carater_terrico", passed = NA, layers = integer(0),
      evidence = list(mineral_layers = mineral_idx),
      missing = c("top_cm", "bottom_cm"),
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 14, p 246"
    ))
  }
  # Layer's contribution is bot - top, but capped at within_depth_cm
  # for the upper bound and at top for the lower bound (skip layers
  # entirely below within_depth_cm).
  capped_bot <- pmin(bot, within_depth_cm)
  capped_top <- pmax(top, 0)
  in_range   <- capped_bot > capped_top
  contributing <- mineral_idx[in_range]
  thickness_contrib <- pmax(0, capped_bot[in_range] - capped_top[in_range])
  cumulative <- sum(thickness_contrib, na.rm = TRUE)
  passed <- cumulative >= min_thickness_cm
  DiagnosticResult$new(
    name = "carater_terrico", passed = passed,
    layers = if (passed) contributing else integer(0),
    evidence = list(
      mineral_layers     = mineral_idx,
      contributing       = contributing,
      thickness_contrib  = thickness_contrib,
      cumulative_cm      = cumulative,
      min_thickness_cm   = min_thickness_cm,
      within_depth_cm    = within_depth_cm
    ),
    missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 14, p 246"
  )
}


# ---- Carater cambissolico (Cap 14, p 247) --------------------------------

# ---- v0.7.5.A: Caracteres para Cap 6 (Cambissolos) ----------------------
#
# 3 diagnosticos novos:
#
#   carater_perferrico    Fe2O3 >= 360 g/kg (= 36%) por sulfurico em B
#                          (vs ferrico = 18-36%)
#   carater_vertissolico  horizonte vertico OU caracter vertico em
#                          posicao nao-Vertissolo, dentro de 150 cm
#   carater_argiluvico    B textural ou caracter argiluvico em posicao
#                          nao diagnostica, dentro de 150 cm
# -------------------------------------------------------------------------

#' Carater perferrico (SiBCS Cap 1; Cap 6 CX Perferricos)
#'
#' Teor de Fe2O3 (pelo ataque sulfurico-NaOH) >= 360 g/kg de solo
#' (= 36\%) na maior parte dos primeiros 100 cm do horizonte B.
#' Discrimina os Grandes Grupos Perferricos (acima do range
#' "ferrico" 180-360 g/kg). Cap 6 CX 4.3 e Cap 10 (Latossolos
#' Perferricos).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_fe2o3_pct Limite inferior em \% mass (default 36 = 360 g/kg).
#' @param max_depth_cm Profundidade maxima de B avaliado (default 100).
#' @return \code{\link{DiagnosticResult}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 1; Cap 6, p 142;
#'             Cap 10 (Latossolos Perferricos).
#' @export
carater_perferrico <- function(pedon,
                                  min_fe2o3_pct = 36,
                                  max_depth_cm  = 100) {
  h <- pedon$horizons
  b_layers <- which(!is.na(h$designation) & grepl("^B", h$designation))
  if (length(b_layers) == 0L) {
    return(DiagnosticResult$new(
      name = "carater_perferrico", passed = FALSE, layers = integer(0),
      evidence = list(reason = "no B horizons"),
      missing = "designation",
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 1; Cap 6, p 142"
    ))
  }
  passing <- integer(0); missing <- character(0); details <- list()
  evaluated <- 0L
  for (i in b_layers) {
    fe <- h$fe2o3_sulfuric_pct[i]
    if (is.na(fe)) {
      missing <- c(missing, "fe2o3_sulfuric_pct"); next
    }
    if (!is.null(max_depth_cm) && !is.na(h$top_cm[i]) &&
          h$top_cm[i] >= max_depth_cm) next
    layer_pass <- fe >= min_fe2o3_pct
    details[[as.character(i)]] <- list(idx = i,
                                         fe2o3_sulfuric_pct = fe,
                                         passed = layer_pass)
    evaluated <- evaluated + 1L
    if (layer_pass) passing <- c(passing, i)
  }
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  DiagnosticResult$new(
    name = "carater_perferrico", passed = passed, layers = passing,
    evidence = list(layers = details, min_fe2o3_pct = min_fe2o3_pct,
                      max_depth_cm = max_depth_cm),
    missing = unique(missing),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 1; Cap 6, p 142"
  )
}


#' Carater vertissolico (SiBCS Cap 6)
#'
#' Solos com horizonte vertico OU caracter vertico em posicao nao
#' diagnostica para Vertissolos, dentro de \code{max_depth_cm}
#' (default 150). Discrimina os Subgrupos vertissolicos de
#' Cambissolos Carbonaticos / Eutroficos / Tb Eutroferricos
#' (Cap 6 CY 3.1.3, 3.6.2, CX 4.1.5, 4.7.7, 4.11.4).
#'
#' Implementacao: passa se \code{\link{horizonte_vertico}} retornar
#' TRUE em ao menos uma camada com \code{top_cm} \eqn{<}
#' \code{max_depth_cm}. SiBCS estrito requer "posicao nao
#' diagnostica para Vertissolos" -- aproximamos isso confiando no
#' dispatcher (apenas chamamos quando ja sabemos que nao e
#' Vertissolo).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_depth_cm Default 150 cm.
#' @return \code{\link{DiagnosticResult}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 6, pp 146-153;
#'             Cap 17 (Vertissolos).
#' @export
carater_vertissolico <- function(pedon, max_depth_cm = 150) {
  res <- horizonte_vertico(pedon)
  layers <- res$layers
  h <- pedon$horizons
  if (!is.null(max_depth_cm) && length(layers) > 0L) {
    in_depth <- !is.na(h$top_cm[layers]) &
                  h$top_cm[layers] < max_depth_cm
    layers <- layers[in_depth]
  }
  passed <- length(layers) > 0L
  DiagnosticResult$new(
    name = "carater_vertissolico", passed = passed, layers = layers,
    evidence = list(horizonte_vertico = res, max_depth_cm = max_depth_cm),
    missing = res$missing %||% character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 6; Cap 17"
  )
}


#' Carater argiluvico (SiBCS Cap 1; Cap 6)
#'
#' Solos com B textural (\code{\link{B_textural}}) em posicao NAO
#' diagnostica para Argissolos, dentro de \code{max_depth_cm}.
#' Discrimina os Subgrupos argissolicos de Cambissolos (Cap 6 CX
#' 4.7.8, 4.10.5).
#'
#' Implementacao v0.7.5: requer \code{\link{B_textural}} passa em
#' alguma camada com \code{top_cm} \eqn{<} \code{max_depth_cm}.
#' Distingue-se de Argissolo pleno por contexto: chamado dentro de
#' Cambissolos onde B incipiente (\code{\link{B_incipiente}}) ja
#' definiu a ordem como Cambissolo.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_depth_cm Default 150 cm.
#' @return \code{\link{DiagnosticResult}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 1; Cap 6, p 153.
#' @export
carater_argiluvico <- function(pedon, max_depth_cm = 150) {
  bt <- B_textural(pedon)
  layers <- bt$layers
  h <- pedon$horizons
  if (!is.null(max_depth_cm) && length(layers) > 0L) {
    in_depth <- !is.na(h$top_cm[layers]) &
                  h$top_cm[layers] < max_depth_cm
    layers <- layers[in_depth]
  }
  passed <- length(layers) > 0L
  DiagnosticResult$new(
    name = "carater_argiluvico", passed = passed, layers = layers,
    evidence = list(B_textural = bt, max_depth_cm = max_depth_cm),
    missing = bt$missing %||% character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 1; Cap 6, p 153"
  )
}


# ---- v0.7.4.C.2: Carater sombrico (Cap 5 PV Aluminicos sombricos) -------

#' Carater sombrico (SiBCS Cap 1; Cap 5 PV)
#'
#' Camada subsuperficial com acumulacao iluvial de materia organica,
#' caracterizada por cores escuras (\code{munsell_value_moist}
#' \eqn{\le} 4, \code{munsell_chroma_moist} \eqn{\le} 3) e
#' \code{oc_pct} \eqn{\ge} 0.5\%, em B abaixo de A/E. Distinto de B
#' espodico por nao requerer iluviacao Al/Fe. Tipico de solos
#' altitudinais (planaltos sul-brasileiros). Discrimina o Subgrupo
#' sombricos de Argissolos Vermelhos Aluminicos (Cap 5 PV 4.2.6).
#'
#' Implementacao v0.7.4 (aproximacao):
#' \itemize{
#'   \item Camada B (\code{designation} matches \code{^B}) com
#'         value \eqn{\le} max_value E chroma \eqn{\le} max_chroma E
#'         oc_pct \eqn{\ge} min_oc_pct, dentro de max_depth_cm.
#' }
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_value Default 4 (escuro).
#' @param max_chroma Default 3.
#' @param min_oc_pct Default 0.5\%.
#' @param max_depth_cm Default 150.
#' @return \code{\link{DiagnosticResult}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 1; Cap 5 PV 4.2.6,
#'             p 130 (Lunardi Neto, 2012, perfil PVa).
#' @export
carater_sombrico <- function(pedon,
                                max_value    = 4,
                                max_chroma   = 3,
                                min_oc_pct   = 0.5,
                                max_depth_cm = 150) {
  h <- pedon$horizons
  b_layers <- which(!is.na(h$designation) & grepl("^B", h$designation))
  if (!is.null(max_depth_cm)) {
    b_layers <- b_layers[!is.na(h$top_cm[b_layers]) &
                            h$top_cm[b_layers] < max_depth_cm]
  }
  if (length(b_layers) == 0L) {
    return(DiagnosticResult$new(
      name = "carater_sombrico", passed = FALSE, layers = integer(0),
      evidence = list(reason = "no B horizons within max_depth_cm"),
      missing = "designation",
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 1; Cap 5 PV 4.2.6"
    ))
  }
  passing <- integer(0); missing <- character(0)
  for (i in b_layers) {
    val <- h$munsell_value_moist[i]
    chr <- h$munsell_chroma_moist[i]
    oc  <- h$oc_pct[i]
    if (is.na(val) || is.na(chr) || is.na(oc)) {
      missing <- c(missing, "munsell_value_moist", "munsell_chroma_moist",
                     "oc_pct")
      next
    }
    if (val <= max_value && chr <= max_chroma && oc >= min_oc_pct) {
      passing <- c(passing, i)
    }
  }
  passed <- if (length(passing) > 0L) TRUE
            else if (length(missing) > 0L && length(passing) == 0L) NA
            else FALSE
  DiagnosticResult$new(
    name = "carater_sombrico", passed = passed, layers = passing,
    evidence = list(b_layers = b_layers, max_value = max_value,
                      max_chroma = max_chroma, min_oc_pct = min_oc_pct,
                      max_depth_cm = max_depth_cm),
    missing = unique(missing),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 1; Cap 5 PV 4.2.6"
  )
}


# ---- v0.7.5.D: Caracteres para Cap 8 (Espodossolos) ---------------------
#
# 2 diagnosticos novos:
#
#   carater_espodico_profundo  B espodico com top em [200, 400] cm
#                              (Hiperespessos / Hidro-hiperespessos)
#   carater_hidromorfico       saturacao com agua em <100 cm + indicios
#                              hidromorficos (horizonte_glei OU redoxico)
# -------------------------------------------------------------------------

#' Carater B espodico profundo (SiBCS Cap 8)
#'
#' Solos com horizonte B espodico cujo \code{top_cm} esta entre
#' \code{min_top_cm} (default 200) e \code{max_top_cm} (default 400).
#' Discrimina os Grandes Grupos Hiperespessos / Hidro-hiperespessos
#' de Espodossolos (Cap 8 1.1, 1.3, 2.1, 2.3, 3.1, 3.3).
#'
#' Implementacao: chama \code{\link{carater_espodico}} e filtra por
#' \code{top_cm} no intervalo [\code{min_top_cm}, \code{max_top_cm}].
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_top_cm Default 200.
#' @param max_top_cm Default 400.
#' @return \code{\link{DiagnosticResult}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 8, pp 165-168.
#' @export
carater_espodico_profundo <- function(pedon,
                                         min_top_cm = 200,
                                         max_top_cm = 400) {
  ce <- carater_espodico(pedon)
  if (!isTRUE(ce$passed) || length(ce$layers) == 0L) {
    return(DiagnosticResult$new(
      name = "carater_espodico_profundo", passed = FALSE,
      layers = integer(0),
      evidence = list(carater_espodico = ce,
                        reason = "B espodico nao passa"),
      missing = ce$missing %||% character(0),
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 8"
    ))
  }
  h <- pedon$horizons
  in_window <- !is.na(h$top_cm[ce$layers]) &
                 h$top_cm[ce$layers] >= min_top_cm &
                 h$top_cm[ce$layers] <= max_top_cm
  passing <- ce$layers[in_window]
  passed <- length(passing) > 0L
  DiagnosticResult$new(
    name = "carater_espodico_profundo", passed = passed,
    layers = passing,
    evidence = list(carater_espodico = ce,
                      min_top_cm = min_top_cm,
                      max_top_cm = max_top_cm),
    missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 8"
  )
}


#' Carater hidromorfico (SiBCS Cap 8)
#'
#' Solos saturados com agua em camada(s) dentro de \code{max_depth_cm}
#' (default 100 cm), evidenciado por horizonte glei
#' (\code{\link{horizonte_glei}}) OU caracter redoxico
#' (\code{\link{carater_redoxico}}) OU horizonte Eg na designation OU
#' acumulacao de Mn em horizonte E ou B espodico. Discrimina os
#' Grandes Grupos Hidromorficos / Hidro-hiperespessos de Espodossolos
#' (Cap 8 1.1, 1.2, 2.1, 2.2, 3.1, 3.2).
#'
#' Implementacao v0.7.5 (aproximacao):
#' \itemize{
#'   \item \code{\link{horizonte_glei}} dentro de max_depth_cm, OR
#'   \item \code{\link{carater_redoxico}} ate max_depth_cm, OR
#'   \item designation pattern \code{Eg} dentro de max_depth_cm.
#' }
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_depth_cm Default 100 cm.
#' @return \code{\link{DiagnosticResult}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 8, pp 165-168.
#' @export
carater_hidromorfico <- function(pedon, max_depth_cm = 100) {
  gl <- horizonte_glei(pedon)
  rx <- carater_redoxico(pedon, max_top_cm = max_depth_cm)
  h <- pedon$horizons
  eg_layers <- which(!is.na(h$designation) &
                       grepl("^Eg|Eg[0-9]?$", h$designation))
  if (length(eg_layers) > 0L) {
    eg_layers <- eg_layers[!is.na(h$top_cm[eg_layers]) &
                              h$top_cm[eg_layers] < max_depth_cm]
  }
  gl_in_depth <- if (!isTRUE(gl$passed) || length(gl$layers) == 0L)
                   FALSE
                 else any(!is.na(h$top_cm[gl$layers]) &
                              h$top_cm[gl$layers] < max_depth_cm)
  passed <- isTRUE(gl_in_depth) ||
              isTRUE(rx$passed) ||
              length(eg_layers) > 0L
  DiagnosticResult$new(
    name = "carater_hidromorfico", passed = passed,
    layers = unique(c(if (isTRUE(gl_in_depth)) gl$layers else integer(0),
                          rx$layers,
                          eg_layers)),
    evidence = list(horizonte_glei = gl,
                      carater_redoxico = rx,
                      eg_layers = eg_layers,
                      max_depth_cm = max_depth_cm),
    missing = unique(c(gl$missing %||% character(0),
                          rx$missing %||% character(0))),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 8"
  )
}


# ---- v0.7.4.C: Caracteres para Subgrupos de Argissolos PA + PV ---------
#
# 3 diagnosticos novos (Cap 5 PA + PV subgrupos):
#
#   carater_gleissolico       horizonte_glei dentro de max_depth_cm
#   carater_cambissolico_arg  4%+ minerais alteraveis OU 5%+ frag rocha em B
#                              (Cap 5 Argissolos -- distinto do
#                               carater_cambissolico Cap 14 Folicos)
#   carater_placico           horizonte placico (Fe/Mn cementado)
# -------------------------------------------------------------------------

#' Carater gleissolico (SiBCS Cap 5; horizonte_glei em posicao nao-Gleissolo)
#'
#' Solos com horizonte glei (\code{\link{horizonte_glei}}) em posicao
#' nao diagnostica para Gleissolos (i.e., dentro de
#' \code{max_depth_cm} mas NAO satisfazendo os requisitos completos de
#' Gleissolo). Discrimina os Subgrupos gleissolicos de Argissolos
#' (Cap 5 PA), Cambissolos (Cap 6) e outros.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_depth_cm Profundidade maxima onde camadas qualificam
#'        (default 150).
#' @return \code{\link{DiagnosticResult}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 5, p 126; Cap 9
#'             (Gleissolos).
#' @export
carater_gleissolico <- function(pedon, max_depth_cm = 150) {
  res <- horizonte_glei(pedon)
  layers <- res$layers
  h <- pedon$horizons
  if (!is.null(max_depth_cm) && length(layers) > 0L) {
    in_depth <- !is.na(h$top_cm[layers]) &
                  h$top_cm[layers] < max_depth_cm
    layers <- layers[in_depth]
  }
  passed <- if (isTRUE(res$passed) && length(layers) == 0L) FALSE
            else isTRUE(res$passed) || (is.na(res$passed) && length(layers) > 0L)
  if (length(layers) > 0L) passed <- TRUE
  DiagnosticResult$new(
    name = "carater_gleissolico", passed = passed, layers = layers,
    evidence = list(horizonte_glei = res, max_depth_cm = max_depth_cm),
    missing = res$missing %||% character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 5, p 126"
  )
}


#' Carater cambissolico (Argissolos -- Cap 5)
#'
#' Solos com 4\% ou mais de minerais alteraveis visiveis E/OU 5\% ou
#' mais de fragmentos de rocha (\code{coarse_fragments_pct}) no
#' horizonte B (exclusive BC ou B/C), dentro de \code{max_depth_cm}.
#' Discrimina os Subgrupos cambissolicos de Argissolos PA (Cap 5,
#' p 126) -- DISTINTO do \code{\link{carater_cambissolico}} (Cap 14
#' Organossolos Folicos: B incipiente abaixo de histico/A).
#'
#' Implementacao v0.7.4 (aproximacao): apenas \code{coarse_fragments_pct}
#' \eqn{\ge} \code{min_coarse_pct} (default 5) eh testado. O criterio
#' "minerais alteraveis visiveis" exigiria campo adicional no schema
#' (e.g. \code{weatherable_minerals_pct}) que sera adicionado em release
#' futura. Documentado como limitacao conhecida.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_coarse_pct Default 5\% volume.
#' @param max_depth_cm Default 150 cm.
#' @return \code{\link{DiagnosticResult}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 5, p 126.
#' @export
carater_cambissolico_arg <- function(pedon,
                                        min_coarse_pct = 5,
                                        max_depth_cm   = 150) {
  h <- pedon$horizons
  b_only <- which(!is.na(h$designation) &
                     grepl("^B[twiog]?[0-9]?$|^B$", h$designation) &
                     !grepl("^BC|^B/C", h$designation))
  if (length(b_only) == 0L) {
    return(DiagnosticResult$new(
      name = "carater_cambissolico_arg", passed = FALSE,
      layers = integer(0),
      evidence = list(reason = "no B horizons (excluding BC/B/C)"),
      missing = "designation",
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 5, p 126"
    ))
  }
  if (!is.null(max_depth_cm)) {
    b_only <- b_only[!is.na(h$top_cm[b_only]) &
                        h$top_cm[b_only] < max_depth_cm]
  }
  passing <- integer(0); missing <- character(0)
  for (i in b_only) {
    cf <- h$coarse_fragments_pct[i]
    if (is.na(cf)) {
      missing <- c(missing, "coarse_fragments_pct"); next
    }
    if (cf >= min_coarse_pct) passing <- c(passing, i)
  }
  passed <- if (length(passing) > 0L) TRUE
            else if (length(b_only) == length(missing) && length(missing) > 0L) NA
            else FALSE
  DiagnosticResult$new(
    name = "carater_cambissolico_arg", passed = passed, layers = passing,
    evidence = list(b_layers = b_only, min_coarse_pct = min_coarse_pct,
                      max_depth_cm = max_depth_cm,
                      note = "v0.7.4 simplification: minerais_alteraveis_pct nao no schema"),
    missing = unique(missing),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 5, p 126"
  )
}


#' Carater placico (SiBCS Cap 5; horizonte placico cementado por Fe/Mn)
#'
#' Camada cimentada por Fe/Mn (geralmente fina, 1-25 mm), detectada via
#' \code{cementation_class \%in\% \{"strongly", "indurated"\}} dentro
#' de \code{max_depth_cm}. Discrimina os Subgrupos placicos de
#' Argissolos PA (Cap 5).
#'
#' Implementacao v0.7.4 (aproximacao): \code{cementation_class} forte
#' ou indurada. SiBCS estrito requeria espessura minima e composicao
#' Fe/Mn confirmada. Refinamento planejado para v0.8.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_depth_cm Default 150 cm.
#' @return \code{\link{DiagnosticResult}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 5, p 125.
#' @export
carater_placico <- function(pedon, max_depth_cm = 150) {
  h <- pedon$horizons
  passing <- integer(0); missing <- character(0)
  strong_or_ind <- c("strongly", "indurated")
  for (i in seq_len(nrow(h))) {
    if (!is.null(max_depth_cm) && !is.na(h$top_cm[i]) &&
          h$top_cm[i] >= max_depth_cm) next
    cem <- h$cementation_class[i]
    if (is.na(cem)) { missing <- c(missing, "cementation_class"); next }
    if (tolower(cem) %in% strong_or_ind) passing <- c(passing, i)
  }
  passed <- if (length(passing) > 0L) TRUE
            else if (length(missing) > 0L && length(passing) == 0L) NA
            else FALSE
  DiagnosticResult$new(
    name = "carater_placico", passed = passed, layers = passing,
    evidence = list(max_depth_cm = max_depth_cm,
                      criterion = "cementation_class strongly/indurated"),
    missing = unique(missing),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 5, p 125"
  )
}


# ---- v0.7.4.B.3: Caracteres para Subgrupos de Argissolos PVA -----------
#
# 9 diagnosticos novos (todos para Subgrupos do Cap 5: PVA, parte de PV
# e PA tambem):
#
#   carater_espessarenico    textura arenosa 100-200 cm
#   carater_petroplintico    concrecionario/litoplintico nao diagnostico
#   carater_planossolico     caracter planico (B planico ou abrupta+sodico)
#   carater_nitossolico      B/A > 1.5 + policromia + cerosidade comum+
#   carater_leptico          contato litico em 50-100 cm
#   carater_leptofragmentario contato litico fragmentario em 50-100 cm
#   carater_saprolitico      Cr (brando) <= 100 cm + sem contato litico
#   carater_luvissolico      Ta (>= 20) + S (>= 5 cmolc/kg)
#   carater_chernossolico    A chernozemico + atividade argila >= 20
# -------------------------------------------------------------------------

#' Carater espessarenico (SiBCS Cap 5)
#'
#' Textura arenosa (clay\% < \code{max_clay_pct}) da superficie ate
#' boundary em [100, 200] cm. Variante "espessa" do
#' \code{\link{carater_arenico}}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_clay_pct Limite superior de \% argila (default 15).
#' @param min_depth_cm Profundidade minima do boundary (default 100).
#' @param max_depth_cm Profundidade maxima do boundary (default 200).
#' @return \code{\link{DiagnosticResult}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 5, pp 130-131.
#' @export
carater_espessarenico <- function(pedon,
                                     max_clay_pct = 15,
                                     min_depth_cm = 100,
                                     max_depth_cm = 200) {
  res <- carater_arenico(pedon,
                            max_clay_pct = max_clay_pct,
                            min_depth_cm = min_depth_cm,
                            max_depth_cm = max_depth_cm)
  res$name <- "carater_espessarenico"
  res$reference <- "Embrapa (2018), SiBCS 5a ed., Cap 5, pp 130-131"
  res
}


#' Carater petroplintico (SiBCS Cap 5)
#'
#' Caracteres concrecionario e/ou litoplintico ou horizontes
#' concrecionario / litoplintico em posicao NAO diagnostica para
#' Plintossolos Petricos, dentro de \code{max_depth_cm} (default 150).
#' Discrimina os Subgrupos petroplinticos de Argissolos (Cap 5: PA, PVA,
#' PV).
#'
#' Implementacao: passa se \code{\link{horizonte_concrecionario}} OU
#' \code{\link{horizonte_litoplintico}} retornarem TRUE em ao menos
#' uma camada com top < max_depth_cm.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_depth_cm Default 150 cm.
#' @return \code{\link{DiagnosticResult}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 5; Cap 16
#'             (Plintossolos).
#' @export
carater_petroplintico <- function(pedon, max_depth_cm = 150) {
  conc <- horizonte_concrecionario(pedon)
  lito <- horizonte_litoplintico(pedon)
  h <- pedon$horizons
  filter_layers <- function(layers) {
    if (length(layers) == 0L) return(integer(0))
    in_depth <- !is.na(h$top_cm[layers]) &
                  h$top_cm[layers] < max_depth_cm
    layers[in_depth]
  }
  conc_layers <- filter_layers(conc$layers)
  lito_layers <- filter_layers(lito$layers)
  passing <- unique(c(conc_layers, lito_layers))
  passed <- length(passing) > 0L
  DiagnosticResult$new(
    name = "carater_petroplintico", passed = passed,
    layers = passing,
    evidence = list(concrecionario = conc, litoplintico = lito,
                      conc_layers_filtered = conc_layers,
                      lito_layers_filtered = lito_layers,
                      max_depth_cm = max_depth_cm),
    missing = unique(c(conc$missing %||% character(0),
                          lito$missing %||% character(0))),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 5; Cap 16"
  )
}


#' Carater planossolico (SiBCS Cap 5)
#'
#' Caracter planico em posicao NAO diagnostica para Planossolos.
#' Discrimina os Subgrupos planossolicos de Argissolos (Cap 5: PA,
#' PVA, PV).
#'
#' Implementacao v0.7.4: aproxima como
#' \code{\link{B_planico}} OR (\code{\link{mudanca_textural_abrupta}} AND
#' \code{\link{carater_sodico}}). SiBCS Cap 1 estritamente define
#' caracter planico via mudanca textural abrupta + horizonte/caracter
#' sodico em B + cores neutras.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_depth_cm Profundidade maxima (default 150).
#' @return \code{\link{DiagnosticResult}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 5; Cap 1, p 36;
#'             Cap 15 (Planossolos).
#' @export
carater_planossolico <- function(pedon, max_depth_cm = 150) {
  bp  <- B_planico(pedon)
  if (isTRUE(bp$passed)) {
    h <- pedon$horizons
    in_depth <- !is.na(h$top_cm[bp$layers]) &
                  h$top_cm[bp$layers] < max_depth_cm
    bp_layers <- bp$layers[in_depth]
    if (length(bp_layers) > 0L) {
      return(DiagnosticResult$new(
        name = "carater_planossolico", passed = TRUE, layers = bp_layers,
        evidence = list(B_planico = bp, max_depth_cm = max_depth_cm),
        missing = character(0),
        reference = "Embrapa (2018), SiBCS 5a ed., Cap 5; Cap 1, p 36"
      ))
    }
  }
  abr <- mudanca_textural_abrupta(pedon)
  sod <- carater_sodico(pedon, max_depth_cm = max_depth_cm)
  passed <- isTRUE(abr$passed) && isTRUE(sod$passed)
  DiagnosticResult$new(
    name = "carater_planossolico", passed = passed,
    layers = if (passed) sod$layers else integer(0),
    evidence = list(B_planico = bp,
                      mudanca_textural_abrupta = abr,
                      carater_sodico = sod,
                      max_depth_cm = max_depth_cm),
    missing = unique(c(bp$missing %||% character(0),
                          abr$missing %||% character(0),
                          sod$missing %||% character(0))),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 5; Cap 1, p 36"
  )
}


#' Carater nitossolico (SiBCS Cap 5)
#'
#' Solos com morfologia (estrutura e cerosidade) semelhante aos
#' Nitossolos, mas diferindo por apresentar relacao textural B/A
#' \eqn{>} 1,5 OU policromia (multiplas matizes Munsell em horizontes
#' B) dentro de \code{max_depth_cm} cm. Discrimina os Subgrupos
#' nitossolicos de Argissolos (Cap 5: PV, PVA).
#'
#' Implementacao v0.7.4 (aproximacao):
#' \itemize{
#'   \item \code{\link{cerosidade}} \eqn{\ge} comum + moderada, AND
#'   \item Razao textural B/A > \code{max_b_a_ratio} (default 1.5),
#'         OR policromia (\eqn{\ge} 2 matizes distintos em B).
#' }
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_b_a_ratio Razao maxima B/A para Nitossolos (default 1.5);
#'        Argissolos nitossolicos tem ratio > 1.5.
#' @param max_depth_cm Profundidade maxima do B avaliado (default 150).
#' @return \code{\link{DiagnosticResult}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 5, pp 129-131; Cap 13.
#' @export
carater_nitossolico <- function(pedon,
                                   max_b_a_ratio = 1.5,
                                   max_depth_cm  = 150) {
  cer <- cerosidade(pedon, min_amount = "common", min_strength = "moderate")
  if (!isTRUE(cer$passed)) {
    return(DiagnosticResult$new(
      name = "carater_nitossolico", passed = FALSE, layers = integer(0),
      evidence = list(cerosidade = cer, reason = "cerosidade < comum/moderada"),
      missing = cer$missing %||% character(0),
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 5"
    ))
  }
  h <- pedon$horizons
  a_layers <- which(!is.na(h$designation) & grepl("^A", h$designation) &
                       !grepl("^B", h$designation))
  b_layers <- which(!is.na(h$designation) & grepl("^B", h$designation))
  if (!is.null(max_depth_cm) && length(b_layers) > 0L) {
    b_layers <- b_layers[!is.na(h$top_cm[b_layers]) &
                             h$top_cm[b_layers] < max_depth_cm]
  }
  ratio <- if (length(a_layers) > 0L && length(b_layers) > 0L)
             mean(h$clay_pct[b_layers], na.rm = TRUE) /
               mean(h$clay_pct[a_layers], na.rm = TRUE)
           else NA_real_
  ratio_ok <- !is.na(ratio) && ratio > max_b_a_ratio
  hues <- unique(h$munsell_hue_moist[b_layers])
  hues <- hues[!is.na(hues)]
  policromia_ok <- length(hues) >= 2L
  passed <- isTRUE(ratio_ok) || isTRUE(policromia_ok)
  DiagnosticResult$new(
    name = "carater_nitossolico", passed = passed,
    layers = if (passed) b_layers else integer(0),
    evidence = list(cerosidade = cer,
                      a_layers = a_layers, b_layers = b_layers,
                      b_a_ratio = ratio, ratio_ok = ratio_ok,
                      hues_in_b = hues, policromia_ok = policromia_ok,
                      max_b_a_ratio = max_b_a_ratio,
                      max_depth_cm = max_depth_cm),
    missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 5, pp 129-131"
  )
}


#' Carater leptico (SiBCS Cap 5; contato litico em 50-100 cm)
#'
#' Solos com contato litico (\code{\link{contato_litico}}) a profundidade
#' entre 50 e 100 cm. Discrimina os Subgrupos lepticos de Argissolos
#' (Cap 5: PA, PV, PVA).
#'
#' Implementacao: chama \code{contato_litico(pedon)} sem bound, depois
#' filtra layers para top em [\code{min_depth_cm}, \code{max_depth_cm}].
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_depth_cm Default 50.
#' @param max_depth_cm Default 100.
#' @return \code{\link{DiagnosticResult}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 5, pp 127, 132.
#' @export
carater_leptico <- function(pedon,
                              min_depth_cm = 50,
                              max_depth_cm = 100) {
  res <- contato_litico(pedon)
  layers <- res$layers
  h <- pedon$horizons
  if (length(layers) > 0L) {
    in_window <- !is.na(h$top_cm[layers]) &
                   h$top_cm[layers] >= min_depth_cm &
                   h$top_cm[layers] <= max_depth_cm
    layers <- layers[in_window]
  }
  passed <- length(layers) > 0L
  DiagnosticResult$new(
    name = "carater_leptico", passed = passed, layers = layers,
    evidence = list(contato_litico = res,
                      min_depth_cm = min_depth_cm,
                      max_depth_cm = max_depth_cm),
    missing = res$missing %||% character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 5, pp 127, 132"
  )
}


#' Carater leptofragmentario (SiBCS Cap 5; Cr / fragmentary 50-100 cm)
#'
#' Solos com contato litico fragmentario (Cr / Crf) a profundidade
#' entre 50 e 100 cm. Discrimina os Subgrupos leptofragmentarios de
#' Argissolos (Cap 5: PA, PV, PVA).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_depth_cm Default 50.
#' @param max_depth_cm Default 100.
#' @return \code{\link{DiagnosticResult}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 5, pp 127, 132.
#' @export
carater_leptofragmentario <- function(pedon,
                                         min_depth_cm = 50,
                                         max_depth_cm = 100) {
  res <- contato_litico_fragmentario(pedon)
  layers <- res$layers
  h <- pedon$horizons
  if (length(layers) > 0L) {
    in_window <- !is.na(h$top_cm[layers]) &
                   h$top_cm[layers] >= min_depth_cm &
                   h$top_cm[layers] <= max_depth_cm
    layers <- layers[in_window]
  }
  passed <- length(layers) > 0L
  DiagnosticResult$new(
    name = "carater_leptofragmentario", passed = passed, layers = layers,
    evidence = list(contato_litico_fragmentario = res,
                      min_depth_cm = min_depth_cm,
                      max_depth_cm = max_depth_cm),
    missing = res$missing %||% character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 5, pp 127, 132"
  )
}


#' Carater saprolitico (SiBCS Cap 5)
#'
#' Solos com horizonte Cr (brando) e ausencia de contato litico ou
#' litico fragmentario, todos dentro de \code{max_depth_cm} (default
#' 100 cm). Discrimina os Subgrupos saproliticos de Argissolos
#' (Cap 5: PA, PV).
#'
#' Implementacao: requer (a) designation pattern \code{Cr}/\code{Crf}
#' (sem \code{R} continuo) em camada com \code{top < max_depth_cm}, e
#' (b) \code{\link{contato_litico}}\code{(pedon)} retorna FALSE.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_depth_cm Default 100.
#' @return \code{\link{DiagnosticResult}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 5, pp 122, 132.
#' @export
carater_saprolitico <- function(pedon, max_depth_cm = 100) {
  h <- pedon$horizons
  # Detecta R (rocha continua, NAO Cr/Crf brando) explicitamente para
  # evitar falso positivo do helper continuous_rock que pode casar Cr.
  r_layers <- which(!is.na(h$designation) &
                       grepl("^R$|^R[^/]|^R$", h$designation) &
                       !grepl("^Cr", h$designation))
  if (length(r_layers) > 0L) {
    r_layers <- r_layers[!is.na(h$top_cm[r_layers]) &
                            h$top_cm[r_layers] < max_depth_cm]
    if (length(r_layers) > 0L) {
      return(DiagnosticResult$new(
        name = "carater_saprolitico", passed = FALSE, layers = integer(0),
        evidence = list(R_layers = r_layers,
                          reason = "perfil tem contato litico R dentro de max_depth_cm"),
        missing = character(0),
        reference = "Embrapa (2018), SiBCS 5a ed., Cap 5, pp 122, 132"
      ))
    }
  }
  cr_layers <- which(!is.na(h$designation) &
                       grepl("^Cr|^Crf", h$designation))
  if (length(cr_layers) > 0L) {
    cr_layers <- cr_layers[!is.na(h$top_cm[cr_layers]) &
                              h$top_cm[cr_layers] < max_depth_cm]
  }
  passed <- length(cr_layers) > 0L
  DiagnosticResult$new(
    name = "carater_saprolitico", passed = passed, layers = cr_layers,
    evidence = list(cr_layers = cr_layers,
                      max_depth_cm = max_depth_cm),
    missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 5, pp 122, 132"
  )
}


#' Carater luvissolico (SiBCS Cap 5; Ta + S alta)
#'
#' Solos com atividade da argila \eqn{\ge} \code{min_ta} (default 20
#' cmolc/kg argila) E soma de bases (S) \eqn{\ge} \code{min_s} (default
#' 5 cmolc/kg solo), ambos na maior parte dos primeiros 100 cm do
#' horizonte B. Discrimina os Subgrupos luvissolicos de Argissolos
#' (Cap 5: PV, PVA).
#'
#' Note: o threshold de Ta para "luvissolico" e 20 (vs 27 para
#' \code{atividade_argila_alta} canonico). S = Ca + Mg + K + Na trocaveis.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_ta Threshold de atividade da argila em cmolc/kg argila
#'        (default 20).
#' @param min_s Threshold de S em cmolc/kg solo (default 5).
#' @param max_depth_cm Profundidade maxima de B avaliado (default 100).
#' @return \code{\link{DiagnosticResult}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 5, p 134; Cap 11
#'             (Luvissolos).
#' @export
carater_luvissolico <- function(pedon,
                                   min_ta       = 20,
                                   min_s        = 5,
                                   max_depth_cm = 100) {
  h <- pedon$horizons
  b_layers <- which(!is.na(h$designation) & grepl("^B", h$designation))
  if (!is.null(max_depth_cm) && length(b_layers) > 0L) {
    b_layers <- b_layers[!is.na(h$top_cm[b_layers]) &
                             h$top_cm[b_layers] < max_depth_cm]
  }
  if (length(b_layers) == 0L) {
    return(DiagnosticResult$new(
      name = "carater_luvissolico", passed = FALSE, layers = integer(0),
      evidence = list(reason = "no B horizons within max_depth_cm"),
      missing = "designation",
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 5, p 134"
    ))
  }
  passing <- integer(0); missing <- character(0); details <- list()
  evaluated <- 0L
  for (i in b_layers) {
    cec <- h$cec_cmol[i]; clay <- h$clay_pct[i]
    ca <- h$ca_cmol[i]; mg <- h$mg_cmol[i]
    k  <- h$k_cmol[i];  na_ <- h$na_cmol[i]
    ta <- if (!is.na(cec) && !is.na(clay) && clay > 0)
            cec * 100 / clay else NA_real_
    s_total <- sum(c(ca, mg, k, na_), na.rm = TRUE)
    if (all(is.na(c(ca, mg, k, na_)))) s_total <- NA_real_
    if (is.na(ta)) { missing <- c(missing, "cec_cmol", "clay_pct"); next }
    if (is.na(s_total)) {
      missing <- c(missing, "ca_cmol", "mg_cmol", "k_cmol", "na_cmol")
      next
    }
    layer_pass <- ta >= min_ta && s_total >= min_s
    details[[as.character(i)]] <- list(
      idx = i, ta_cmolc_per_kg_clay = ta, s_cmol = s_total,
      passed = layer_pass
    )
    evaluated <- evaluated + 1L
    if (layer_pass) passing <- c(passing, i)
  }
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  DiagnosticResult$new(
    name = "carater_luvissolico", passed = passed, layers = passing,
    evidence = list(layers = details, min_ta = min_ta, min_s = min_s,
                      max_depth_cm = max_depth_cm),
    missing = unique(missing),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 5, p 134"
  )
}


#' Carater chernossolico (SiBCS Cap 5; A chernozemico + Ta alta)
#'
#' Solos com horizonte A chernozemico
#' (\code{\link{horizonte_A_chernozemico}}) E atividade da argila
#' \eqn{\ge} \code{min_ta} (default 20 cmolc/kg argila) na maior parte
#' dos primeiros 100 cm do B (inclusive BA). Discrimina os Subgrupos
#' chernossolicos de Argissolos (Cap 5: PV, PVA).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_ta Threshold de atividade da argila (default 20).
#' @return \code{\link{DiagnosticResult}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 5, p 134; Cap 7
#'             (Chernossolos).
#' @export
carater_chernossolico <- function(pedon, min_ta = 20) {
  ach <- horizonte_A_chernozemico(pedon)
  if (!isTRUE(ach$passed)) {
    return(DiagnosticResult$new(
      name = "carater_chernossolico", passed = FALSE, layers = integer(0),
      evidence = list(A_chernozemico = ach,
                        reason = "A chernozemico nao passa"),
      missing = ach$missing %||% character(0),
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 5, p 134"
    ))
  }
  ta_res <- atividade_argila_alta(pedon, min_ta = min_ta)
  passed <- isTRUE(ta_res$passed)
  DiagnosticResult$new(
    name = "carater_chernossolico", passed = passed,
    layers = if (passed) ta_res$layers else integer(0),
    evidence = list(A_chernozemico = ach, atividade_argila = ta_res,
                      min_ta = min_ta),
    missing = unique(c(ach$missing %||% character(0),
                          ta_res$missing %||% character(0))),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 5, p 134"
  )
}


# ---- Carater arenico (Cap 5; textura arenosa 50-100 cm) ------------------

#' Carater arenico (SiBCS Cap 5)
#'
#' Solos com textura arenosa (clay\% < \code{max_clay_pct}, default
#' 15\%) desde a superficie ate uma profundidade entre
#' \code{min_depth_cm} e \code{max_depth_cm} (default 50-100 cm).
#' Discrimina os Subgrupos arenicos de Argissolos (Cap 5: PAC, PA,
#' PV, PVA) e Neossolos (Cap 12).
#'
#' Implementacao: ordena horizontes por \code{top_cm}, identifica o
#' PRIMEIRO horizonte com \code{clay_pct >= max_clay_pct}, e verifica
#' que (a) todos os horizontes acima desse boundary sao arenosos
#' (sem camada argilosa intercalada acima) e (b) o boundary
#' (\code{top_cm}) cai no intervalo \code{[min_depth_cm,
#' max_depth_cm]}.
#'
#' Para "espessarenicos" (boundary 100-200 cm), use
#' \code{carater_espessarenico} (planejado v0.7.4.B.3).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_clay_pct Limite superior de \% argila para "arenoso"
#'        (default 15 = areia / areia franca).
#' @param min_depth_cm Profundidade minima do boundary (default 50).
#' @param max_depth_cm Profundidade maxima do boundary (default 100).
#' @return \code{\link{DiagnosticResult}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 5 (Argissolos),
#'             pp 120-138.
#' @export
carater_arenico <- function(pedon,
                              max_clay_pct = 15,
                              min_depth_cm = 50,
                              max_depth_cm = 100) {
  h <- pedon$horizons
  if (nrow(h) == 0L) {
    return(DiagnosticResult$new(
      name = "carater_arenico", passed = FALSE, layers = integer(0),
      evidence = list(reason = "empty horizons"), missing = character(0),
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 5"
    ))
  }
  if (all(is.na(h$clay_pct))) {
    return(DiagnosticResult$new(
      name = "carater_arenico", passed = NA, layers = integer(0),
      evidence = list(reason = "all clay_pct NA"),
      missing = "clay_pct",
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 5"
    ))
  }
  ord <- order(h$top_cm)
  ordered_clay <- h$clay_pct[ord]
  ordered_top  <- h$top_cm[ord]
  # Identify first clearly non-sandy layer (clay >= max_clay_pct).
  non_sandy_seq <- !is.na(ordered_clay) & ordered_clay >= max_clay_pct
  if (!any(non_sandy_seq)) {
    return(DiagnosticResult$new(
      name = "carater_arenico", passed = FALSE, layers = integer(0),
      evidence = list(reason = "profile sem camada argilosa identificavel"),
      missing = character(0),
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 5"
    ))
  }
  first_idx <- which(non_sandy_seq)[1]
  # Verify all layers above first non-sandy are sandy (sem clay intercalada).
  if (first_idx > 1L) {
    above_clay <- ordered_clay[seq_len(first_idx - 1L)]
    if (any(!is.na(above_clay) & above_clay >= max_clay_pct)) {
      return(DiagnosticResult$new(
        name = "carater_arenico", passed = FALSE, layers = integer(0),
        evidence = list(reason = "camada argilosa intercalada acima do boundary"),
        missing = character(0),
        reference = "Embrapa (2018), SiBCS 5a ed., Cap 5"
      ))
    }
  }
  boundary_top <- ordered_top[first_idx]
  passed <- !is.na(boundary_top) &&
              boundary_top >= min_depth_cm &&
              boundary_top <= max_depth_cm
  DiagnosticResult$new(
    name = "carater_arenico", passed = passed,
    layers = if (passed) ord[seq_len(first_idx - 1L)] else integer(0),
    evidence = list(boundary_top_cm = boundary_top,
                      first_non_sandy_idx = ord[first_idx],
                      ordered_clay_pct = ordered_clay,
                      min_depth_cm = min_depth_cm,
                      max_depth_cm = max_depth_cm),
    missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 5"
  )
}


# ---- Carater durico (Cap 1; cimentacao parcial por silica) ---------------

#' Carater durico (SiBCS Cap 1)
#'
#' Solos com endurecimento por cimentacao parcial de silica (SiO2),
#' insuficiente para qualificar como horizonte durico (\code{\link{duripa}})
#' completo. Detectado quando:
#'
#' \itemize{
#'   \item \code{duripan_pct} > 0 (presenca de noduros / concrecoes
#'         de silica), OR
#'   \item \code{cementation_class} \eqn{\in}\{"weakly", "moderately"\}
#'         (cimentacao fraca a moderada, NAO indurada/strongly).
#' }
#'
#' Discrimina os Subgrupos duricos / abrupticos duricos de Argissolos
#' Acinzentados (Cap 5 PAC) e Latossolos com caracter durico
#' (Cap 10).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_depth_cm Profundidade maxima onde camadas qualificam
#'        (default 150, conforme SiBCS Cap 5: "dentro de 150 cm").
#' @return \code{\link{DiagnosticResult}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 1; Cap 5 (Argissolos
#'             Acinzentados Distrocoesos abrupticos duricos), p 120.
#' @export
carater_durico <- function(pedon, max_depth_cm = 150) {
  h <- pedon$horizons
  weak_or_mod <- c("weakly", "moderately")
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in seq_len(nrow(h))) {
    if (!is.null(max_depth_cm) && !is.na(h$top_cm[i]) &&
          h$top_cm[i] >= max_depth_cm) next
    dur <- h$duripan_pct[i]
    cem <- h$cementation_class[i]
    if (is.na(dur) && is.na(cem)) {
      missing <- c(missing, "duripan_pct", "cementation_class"); next
    }
    by_pct <- !is.na(dur) && dur > 0
    by_cem <- !is.na(cem) && tolower(cem) %in% weak_or_mod
    layer_pass <- isTRUE(by_pct) || isTRUE(by_cem)
    details[[as.character(i)]] <- list(idx = i,
                                         duripan_pct = dur,
                                         cementation_class = cem,
                                         passed = layer_pass)
    if (layer_pass) passing <- c(passing, i)
  }
  passed <- if (length(passing) > 0L) TRUE
            else if (length(details) == 0L && length(missing) > 0L) NA
            else FALSE
  DiagnosticResult$new(
    name = "carater_durico", passed = passed, layers = passing,
    evidence = list(layers = details, max_depth_cm = max_depth_cm),
    missing = unique(missing),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 1"
  )
}


# ---- Carater latossolico (Cap 5; B latossolico abaixo do B textural) -----

#' Carater latossolico (SiBCS Cap 5)
#'
#' Solos com horizonte B latossolico (\code{\link{B_latossolico}}) abaixo
#' do horizonte B textural (\code{\link{B_textural}}), dentro de
#' \code{max_depth_cm} (default 150 cm). Discrimina os Subgrupos
#' latossolicos de Argissolos (Cap 5: PAC, PA, PV, PVA) -- transicao
#' entre Argissolo e Latossolo dentro do mesmo perfil.
#'
#' Implementacao: requer (1) \code{B_textural()} passa, (2)
#' \code{B_latossolico()} passa, e (3) ao menos uma camada com
#' B latossolico tem \code{top_cm} maior que o \code{top_cm} maximo
#' das camadas com B textural (i.e., latossolico ocorre abaixo).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_depth_cm Profundidade maxima do B latossolico (default 150).
#' @return \code{\link{DiagnosticResult}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 5 (Argissolos),
#'             pp 121-138.
#' @export
carater_latossolico <- function(pedon, max_depth_cm = 150) {
  bt <- B_textural(pedon)
  if (!isTRUE(bt$passed)) {
    return(DiagnosticResult$new(
      name = "carater_latossolico", passed = FALSE, layers = integer(0),
      evidence = list(B_textural = bt,
                        reason = "B textural nao passa"),
      missing = bt$missing %||% character(0),
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 5"
    ))
  }
  bl <- B_latossolico(pedon)
  if (!isTRUE(bl$passed)) {
    return(DiagnosticResult$new(
      name = "carater_latossolico", passed = FALSE, layers = integer(0),
      evidence = list(B_textural = bt, B_latossolico = bl,
                        reason = "B latossolico nao passa"),
      missing = bl$missing %||% character(0),
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 5"
    ))
  }
  h <- pedon$horizons
  bt_tops <- h$top_cm[bt$layers]
  if (all(is.na(bt_tops))) {
    return(DiagnosticResult$new(
      name = "carater_latossolico", passed = NA, layers = integer(0),
      evidence = list(B_textural = bt, B_latossolico = bl),
      missing = "top_cm",
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 5"
    ))
  }
  bt_max_top <- max(bt_tops, na.rm = TRUE)
  bl_below <- bl$layers[!is.na(h$top_cm[bl$layers]) &
                          h$top_cm[bl$layers] > bt_max_top &
                          h$top_cm[bl$layers] < max_depth_cm]
  passed <- length(bl_below) > 0L
  DiagnosticResult$new(
    name = "carater_latossolico", passed = passed,
    layers = bl_below,
    evidence = list(B_textural = bt, B_latossolico = bl,
                      bt_max_top = bt_max_top,
                      bl_below = bl_below,
                      max_depth_cm = max_depth_cm),
    missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 5"
  )
}


# ---- Carater espesso-humico (Cap 5 PBAC subgrupos) -----------------------

#' Carater espesso-humico (SiBCS Cap 5, p 119)
#'
#' Solos com horizonte A humico e conteudo de carbono >= \code{min_oc_pct}
#' (default 1\% = 10 g/kg) extendendo-se ate \code{min_depth_cm} (default
#' 80 cm) ou mais de profundidade. Discrimina os Subgrupos
#' "espesso-humicos" de Argissolos Bruno-Acinzentados Ta Aluminicos
#' (Cap 5 PBAC 1.1.2) -- camadas humosas espessas tipicas de
#' Argissolos do RS.
#'
#' Implementacao: requer (1) \code{\link{horizonte_A_humico}} passa
#' AND (2) ha camada com \code{oc_pct} >= \code{min_oc_pct} cuja
#' \code{bottom_cm} >= \code{min_depth_cm}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_oc_pct Limite inferior de OC\% nas camadas inferiores
#'        (default 1.0 = 10 g/kg).
#' @param min_depth_cm Profundidade minima de extensao do C alto
#'        (default 80 cm).
#' @return \code{\link{DiagnosticResult}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 5 (Argissolos), p 119.
#' @export
carater_humico_espesso <- function(pedon,
                                       min_oc_pct   = 1.0,
                                       min_depth_cm = 80) {
  ah <- horizonte_A_humico(pedon)
  if (!isTRUE(ah$passed)) {
    return(DiagnosticResult$new(
      name = "carater_humico_espesso", passed = FALSE,
      layers = integer(0),
      evidence = list(A_humico = ah,
                        reason = "horizonte A humico nao passa"),
      missing = ah$missing %||% character(0),
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 5, p 119"
    ))
  }
  h <- pedon$horizons
  if (all(is.na(h$oc_pct))) {
    return(DiagnosticResult$new(
      name = "carater_humico_espesso", passed = NA,
      layers = integer(0),
      evidence = list(A_humico = ah),
      missing = "oc_pct",
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 5, p 119"
    ))
  }
  carbon_layers <- which(!is.na(h$oc_pct) & h$oc_pct >= min_oc_pct)
  passed <- FALSE
  deepest_bottom <- NA_real_
  if (length(carbon_layers) > 0L) {
    bottoms <- h$bottom_cm[carbon_layers]
    if (any(!is.na(bottoms))) {
      deepest_bottom <- max(bottoms, na.rm = TRUE)
      passed <- deepest_bottom >= min_depth_cm
    }
  }
  DiagnosticResult$new(
    name = "carater_humico_espesso", passed = passed,
    layers = if (passed) carbon_layers else integer(0),
    evidence = list(A_humico = ah,
                      carbon_layers = carbon_layers,
                      deepest_bottom_cm = deepest_bottom,
                      min_oc_pct = min_oc_pct,
                      min_depth_cm = min_depth_cm),
    missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 5, p 119"
  )
}


# ---- Carater coeso (Cap 1, pp 32-33) -------------------------------------

#' Carater coeso (SiBCS Cap 1, pp 32-33)
#'
#' Solos com horizontes coesos: muito duros a extremamente duros
#' quando secos, friaveis a firmes quando umidos, decorrentes do
#' empacotamento das particulas e/ou cimentacao. Discrimina os
#' Grandes Grupos Distrocoesos / Eutrocoesos de Argissolos
#' (Cap 5, pp 117-119) e Latossolos (Cap 10).
#'
#' Criterios canonicos:
#' \itemize{
#'   \item \code{rupture_resistance} \eqn{\in}\{"very hard",
#'         "extremely hard"\} (em estado seco)
#'   \item \code{consistence_moist} \eqn{\in}\{"friable", "firm"\}
#'         (em estado umido)
#'   \item Excluido: textura areia / areia franca (\code{clay_pct} < 15\%)
#' }
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_depth_cm Profundidade maxima onde camadas qualificam
#'        (default \code{150}, conforme SiBCS Cap 5: "dentro de
#'        150 cm a partir da superficie").
#' @return \code{\link{DiagnosticResult}}; \code{passed = TRUE} se ao
#'         menos uma camada (com textura suficiente) atende aos dois
#'         criterios de consistencia.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 1, pp 32-33;
#'             Cap 5 (Argissolos), pp 117-119.
#' @export
carater_coeso <- function(pedon, max_depth_cm = 150) {
  h <- pedon$horizons
  hard_dry      <- c("very hard", "extremely hard")
  friable_moist <- c("friable", "firm")
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in seq_len(nrow(h))) {
    if (!is.null(max_depth_cm) && !is.na(h$top_cm[i]) &&
          h$top_cm[i] >= max_depth_cm) next
    rr <- h$rupture_resistance[i]
    cm <- h$consistence_moist[i]
    clay <- h$clay_pct[i]
    if (is.na(rr) && is.na(cm)) {
      missing <- c(missing, "rupture_resistance", "consistence_moist"); next
    }
    # Textura excluida: areia / areia franca (clay < 15%).
    if (!is.na(clay) && clay < 15) {
      details[[as.character(i)]] <- list(idx = i, clay_pct = clay,
                                          excluded = "sandy texture (clay < 15%)")
      next
    }
    rr_ok <- !is.na(rr) && tolower(rr) %in% hard_dry
    cm_ok <- !is.na(cm) && tolower(cm) %in% friable_moist
    layer_pass <- isTRUE(rr_ok) && isTRUE(cm_ok)
    details[[as.character(i)]] <- list(
      idx = i, rupture_resistance = rr, consistence_moist = cm,
      clay_pct = clay, passed = layer_pass
    )
    if (layer_pass) passing <- c(passing, i)
  }
  evaluated <- length(details) -
                 sum(vapply(details, function(x) !is.null(x$excluded),
                              logical(1)))
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  DiagnosticResult$new(
    name = "carater_coeso", passed = passed, layers = passing,
    evidence = list(layers = details, max_depth_cm = max_depth_cm),
    missing = unique(missing),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, pp 32-33"
  )
}


# ---- Carater ferrico (Fe2O3 sulfurico em B) -----------------------------

#' Carater ferrico (SiBCS Cap 1, p 35; Cap 5 e Cap 10)
#'
#' Teor de Fe2O3 (pelo ataque sulfurico-NaOH) entre 180 e 360 g/kg
#' de solo (= 18\%-36\% mass) na maior parte dos primeiros 100 cm do
#' horizonte B. Acima de 360 g/kg = "perferrico" (nao implementado
#' aqui). Discrimina os Grandes Grupos Eutroferricos / Distroferricos
#' / Aluminoferricos de Latossolos (Cap 10), Argissolos (Cap 5
#' Eutroferricos) e Cambissolos (Cap 6 Aluminoferricos).
#'
#' Implementacao v0.7.4: testa se \emph{algum} horizonte B dentro de
#' \code{max_depth_cm} atende ao intervalo. SiBCS estrito ("na maior
#' parte de") seria uma media ponderada por espessura -- refinamento
#' planejado para v0.7.5.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_fe2o3_pct Limite inferior de Fe2O3 sulfurico em \% mass
#'        (default 18 = 180 g/kg).
#' @param max_fe2o3_pct Limite superior (exclusivo) em \% mass
#'        (default 36 = 360 g/kg).
#' @param max_depth_cm Profundidade maxima de B avaliado (default 100).
#' @return \code{\link{DiagnosticResult}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 1, p 35; Cap 5
#'             (Argissolos Eutroferricos, p 118); Cap 10 (Latossolos).
#' @export
carater_ferrico <- function(pedon,
                              min_fe2o3_pct = 18,
                              max_fe2o3_pct = 36,
                              max_depth_cm  = 100) {
  h <- pedon$horizons
  b_layers <- which(!is.na(h$designation) & grepl("^B", h$designation))
  if (length(b_layers) == 0L) {
    return(DiagnosticResult$new(
      name = "carater_ferrico", passed = FALSE, layers = integer(0),
      evidence = list(reason = "no B horizons"),
      missing = "designation",
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p 35"
    ))
  }
  passing <- integer(0); missing <- character(0); details <- list()
  evaluated <- 0L
  for (i in b_layers) {
    fe <- h$fe2o3_sulfuric_pct[i]
    if (is.na(fe)) {
      missing <- c(missing, "fe2o3_sulfuric_pct"); next
    }
    if (!is.null(max_depth_cm) && !is.na(h$top_cm[i]) &&
          h$top_cm[i] >= max_depth_cm) next
    layer_pass <- fe >= min_fe2o3_pct && fe < max_fe2o3_pct
    details[[as.character(i)]] <- list(idx = i,
                                         fe2o3_sulfuric_pct = fe,
                                         passed = layer_pass)
    evaluated <- evaluated + 1L
    if (layer_pass) passing <- c(passing, i)
  }
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  DiagnosticResult$new(
    name = "carater_ferrico", passed = passed, layers = passing,
    evidence = list(layers = details,
                      min_fe2o3_pct = min_fe2o3_pct,
                      max_fe2o3_pct = max_fe2o3_pct,
                      max_depth_cm  = max_depth_cm),
    missing = unique(missing),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p 35"
  )
}


#' Carater cambissolico (SiBCS Cap 14)
#'
#' Solos com B incipiente (\code{\link{B_incipiente}}) abaixo do
#' horizonte hístico (H/O) ou A. Discrimina os Subgrupos cambissolicos
#' de Organossolos Folicos (Cap 14, pp 247-248): Folicos Fibricos /
#' Hemicos / Sapricos cambissolicos.
#'
#' Implementado como uma interseccao de duas condicoes:
#' \enumerate{
#'   \item \code{B_incipiente} passa em ao menos um horizonte
#'   \item Esse horizonte B incipiente esta abaixo de um horizonte
#'         H/O (hístico) ou A
#' }
#' Em pedons sem H/O ou A acima do B incipiente, o teste falha
#' (B incipiente isolado nao caracteriza Organossolo Cambissolico).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return \code{\link{DiagnosticResult}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 14, pp 247-248.
#' @export
carater_cambissolico <- function(pedon) {
  h <- pedon$horizons
  if (nrow(h) == 0L) {
    return(DiagnosticResult$new(
      name = "carater_cambissolico", passed = FALSE, layers = integer(0),
      evidence = list(reason = "empty horizons"),
      missing = "designation",
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 14, pp 247-248"
    ))
  }
  # Step 1: B incipiente em alguma camada
  bi <- B_incipiente(pedon)
  if (!isTRUE(bi$passed)) {
    return(DiagnosticResult$new(
      name = "carater_cambissolico", passed = FALSE,
      layers = integer(0),
      evidence = list(B_incipiente = bi,
                        reason = "no B incipiente layer"),
      missing = bi$missing %||% character(0),
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 14, pp 247-248"
    ))
  }
  # Step 2: pelo menos um horizonte B incipiente esta ABAIXO de um H/O ou A
  bi_layers <- bi$layers
  hist_or_a_idx <- which(!is.na(h$designation) &
                            grepl("^[HO]|^A", h$designation))
  if (length(hist_or_a_idx) == 0L) {
    return(DiagnosticResult$new(
      name = "carater_cambissolico", passed = FALSE, layers = integer(0),
      evidence = list(B_incipiente = bi,
                        reason = "no histic (H/O) or A layer above B incipiente"),
      missing = character(0),
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 14, pp 247-248"
    ))
  }
  # Para cada B incipiente, verificar se HÁ H/O ou A com bottom_cm <= top_cm do B.
  passing_bi <- integer(0)
  for (bi_i in bi_layers) {
    bi_top <- h$top_cm[bi_i]
    above_match <- any(!is.na(h$bottom_cm[hist_or_a_idx]) &
                          h$bottom_cm[hist_or_a_idx] <= bi_top)
    if (isTRUE(above_match)) passing_bi <- c(passing_bi, bi_i)
  }
  passed <- length(passing_bi) > 0L
  DiagnosticResult$new(
    name = "carater_cambissolico", passed = passed,
    layers = passing_bi,
    evidence = list(B_incipiente = bi,
                      hist_or_a_layers = hist_or_a_idx,
                      passing_bi = passing_bi),
    missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 14, pp 247-248"
  )
}
