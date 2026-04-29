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
