# ============================================================================
# SiBCS 5a edicao (Embrapa, 2018) -- Diagnosticos das 13 ordens (Cap 4,
# pp 110-114). Cada funcao implementa o gate canonico do 1o nivel
# categorico, na ordem de precedencia da chave (Organossolos primeiro,
# Argissolos como catch-all final). Cada gate aplica o criterio
# especifico AND a exclusao das classes que vem antes na chave.
# ============================================================================


# ---- 1. Organossolos ------------------------------------------------------

#' Organossolos (SiBCS Cap 4, chave do 1o nivel; conceito Cap 3, p 99-101)
#'
#' Solos com horizonte hístico atendendo a um dos criterios de
#' espessura: \\>= 20 cm sobre rocha, \\>= 40 cm continuo OR cumulativo
#' nos 80 cm superficiais, OR \\>= 60 cm se \\>= 75\% volume tecido
#' vegetal.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
organossolo <- function(pedon) {
  res <- horizonte_histico(pedon)
  DiagnosticResult$new(
    name = "organossolo", passed = res$passed,
    layers = res$layers, evidence = list(histico = res),
    missing = res$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 4, p. 111"
  )
}


# ---- 2. Neossolos ---------------------------------------------------------

#' Neossolos (SiBCS Cap 4, p 111-112; conceito Cap 3, p 96-97)
#'
#' Solos pouco evoluidos: SEM horizonte B diagnostico + ausencia de:
#' (a) glei dentro 150 cm, (b) plintico dentro 40 cm, (c) vertico
#' imediatamente abaixo de A, (d) A chernozemico conjugado com
#' carbonatico ou cálcico.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
neossolo <- function(pedon) {
  # v0.9.10: when `carater_fluvico` is TRUE, the apparent B-textural
  # pattern is depositional / allochthonous (Neossolos Fluvicos), not a
  # pedogenic B horizon. We therefore short-circuit the has-B exclusion
  # for fluvic profiles -- otherwise the canonical Fluvisol fixture is
  # rejected by neossolo() (because argic-via-clay-jump triggers on the
  # C1->C2 stratification) and falls through to Luvissolos.
  fluv <- carater_fluvico(pedon)
  has_b <- any(c(
    isTRUE(B_textural(pedon)$passed),
    isTRUE(B_latossolico(pedon)$passed),
    isTRUE(B_incipiente(pedon)$passed),
    isTRUE(B_nitico(pedon)$passed),
    isTRUE(B_espodico(pedon)$passed),
    isTRUE(B_planico(pedon)$passed)
  ))
  if (has_b && !isTRUE(fluv$passed)) {
    return(DiagnosticResult$new(
      name = "neossolo", passed = FALSE,
      layers = integer(0),
      evidence = list(reason = "has B diagnostic horizon",
                       carater_fluvico = fluv),
      missing = character(0),
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 4, p. 111-112"
    ))
  }
  h <- pedon$horizons
  glei_dentro_150 <- isTRUE(horizonte_glei(pedon)$passed) &&
                       any(h$top_cm[horizonte_glei(pedon)$layers] <= 150,
                              na.rm = TRUE)
  plint_dentro_40 <- isTRUE(horizonte_plintico(pedon)$passed) &&
                       any(h$top_cm[horizonte_plintico(pedon)$layers] <= 40,
                              na.rm = TRUE)
  vert_below_a <- isTRUE(horizonte_vertico(pedon)$passed) &&
                    any(h$top_cm[horizonte_vertico(pedon)$layers] <= 50,
                           na.rm = TRUE)
  chern_calc <- isTRUE(horizonte_A_chernozemico(pedon)$passed) &&
                  (isTRUE(carater_carbonatico(pedon)$passed) ||
                     isTRUE(horizonte_calcico(pedon)$passed))
  exclusion_failed <- glei_dentro_150 || plint_dentro_40 ||
                        vert_below_a || chern_calc
  passed <- !exclusion_failed
  DiagnosticResult$new(
    name = "neossolo", passed = passed,
    layers = if (passed) seq_len(nrow(h)) else integer(0),
    evidence = list(
      no_b_diagnostic = !has_b,
      glei_dentro_150 = glei_dentro_150,
      plintico_dentro_40 = plint_dentro_40,
      vertico_below_a = vert_below_a,
      chernozemic_carbonatic = chern_calc
    ),
    missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 4, p. 111-112"
  )
}


# ---- 3. Vertissolos -------------------------------------------------------

#' Vertissolos (SiBCS Cap 4, p 112; conceito Cap 3, p 105-106)
#'
#' Horizonte vertico iniciando \\<= 100 cm + clay \\>= 30\% nos 20 cm
#' superficiais + fendas verticais + ausencia de contato litico /
#' petrocalcico / duripa nos 30 cm + COLE \\>= 0.06.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
vertissolo <- function(pedon) {
  v <- horizonte_vertico(pedon)
  if (!isTRUE(v$passed)) {
    return(DiagnosticResult$new(
      name = "vertissolo", passed = v$passed,
      layers = integer(0), evidence = list(vertico = v),
      missing = v$missing,
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 4, p. 112"
    ))
  }
  h <- pedon$horizons
  vert_top <- min(h$top_cm[v$layers], na.rm = TRUE)
  starts_in_100 <- vert_top <= 100
  surface_clay <- mean(h$clay_pct[h$top_cm <= 20], na.rm = TRUE)
  surface_clay_ok <- !is.na(surface_clay) && surface_clay >= 30
  # Cracks tested via vertic_horizon's internal sub-test
  shallow_30 <- which(!is.na(h$top_cm) & h$top_cm <= 30)
  has_lithic_petric <- any(grepl("^R|^Cr", h$designation[shallow_30])) ||
                         any(!is.na(h$cementation_class[shallow_30]) &
                                h$cementation_class[shallow_30] %in%
                                  c("strongly", "indurated"))
  passed <- starts_in_100 && surface_clay_ok && !has_lithic_petric
  DiagnosticResult$new(
    name = "vertissolo", passed = passed, layers = v$layers,
    evidence = list(
      vertico = v, vert_top_cm = vert_top,
      surface_clay_pct = surface_clay,
      no_lithic_petric_in_30 = !has_lithic_petric
    ),
    missing = v$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 4, p. 112"
  )
}


# ---- 4. Espodossolos ------------------------------------------------------

#' Espodossolos (SiBCS Cap 4, p 112; conceito Cap 3, p 90-91)
#'
#' Horizonte B espodico imediatamente abaixo de horizontes E ou A,
#' dentro de 200 cm (ou 400 cm se A+E ou histico+E ultrapassam 200).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Numeric threshold or option (see Details).
#' @export
espodossolo <- function(pedon, max_top_cm = 200) {
  res <- B_espodico(pedon)
  if (!isTRUE(res$passed)) {
    return(DiagnosticResult$new(
      name = "espodossolo", passed = res$passed,
      layers = res$layers, evidence = list(espodico = res),
      missing = res$missing,
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 4, p. 112"
    ))
  }
  h <- pedon$horizons
  espod_top <- min(h$top_cm[res$layers], na.rm = TRUE)
  passed <- espod_top <= max_top_cm
  DiagnosticResult$new(
    name = "espodossolo", passed = passed,
    layers = res$layers,
    evidence = list(espodico = res, top_cm = espod_top),
    missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 4, p. 112"
  )
}


# ---- 5. Planossolos -------------------------------------------------------

#' Planossolos (SiBCS Cap 4, p 112; conceito Cap 3, p 101-102)
#'
#' Horizonte B planico nao coincidente com plintico (sem carater
#' sodico), imediatamente abaixo de A ou E.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
planossolo <- function(pedon) {
  bp <- B_planico(pedon)
  pl <- horizonte_plintico(pedon)
  sod <- carater_sodico(pedon)
  if (!isTRUE(bp$passed)) {
    return(DiagnosticResult$new(
      name = "planossolo", passed = bp$passed,
      layers = integer(0), evidence = list(planico = bp),
      missing = bp$missing,
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 4, p. 112"
    ))
  }
  # Coincidente com plintico AND sem sodico -> falha
  excludes <- isTRUE(pl$passed) && !isTRUE(sod$passed) &&
                length(intersect(bp$layers, pl$layers)) > 0L
  passed <- !excludes
  DiagnosticResult$new(
    name = "planossolo", passed = passed,
    layers = bp$layers,
    evidence = list(planico = bp, plintico = pl, sodico = sod,
                     excludes_plintico = excludes),
    missing = bp$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 4, p. 112"
  )
}


# ---- 6. Gleissolos --------------------------------------------------------

#' Gleissolos (SiBCS Cap 4, p 112-113; conceito Cap 3, p 91-93)
#'
#' Horizonte glei iniciando \\<= 50 cm OR entre 50-150 cm imediatamente
#' subjacente a A/E ou H histico (com espessura insuficiente para
#' Organossolo), sem horizonte plintico/concrecionario/litoplintico
#' dentro de 200 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
gleissolo <- function(pedon) {
  g <- horizonte_glei(pedon)
  if (!isTRUE(g$passed)) {
    return(DiagnosticResult$new(
      name = "gleissolo", passed = g$passed,
      layers = integer(0), evidence = list(glei = g),
      missing = g$missing,
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 4, p. 112-113"
    ))
  }
  h <- pedon$horizons
  glei_top <- min(h$top_cm[g$layers], na.rm = TRUE)
  in_50  <- glei_top <= 50
  in_150 <- glei_top <= 150
  # Exclusoes: plintico/concrecionario/litoplintico dentro de 200 cm
  pl <- horizonte_plintico(pedon)
  co <- horizonte_concrecionario(pedon)
  li <- horizonte_litoplintico(pedon)
  has_plinthic_200 <- any(c(
    isTRUE(pl$passed) && any(h$top_cm[pl$layers] <= 200, na.rm = TRUE),
    isTRUE(co$passed) && any(h$top_cm[co$layers] <= 200, na.rm = TRUE),
    isTRUE(li$passed) && any(h$top_cm[li$layers] <= 200, na.rm = TRUE)
  ))
  passed <- (in_50 || in_150) && !has_plinthic_200
  DiagnosticResult$new(
    name = "gleissolo", passed = passed,
    layers = g$layers,
    evidence = list(glei = g, glei_top_cm = glei_top,
                     plinthic_within_200 = has_plinthic_200,
                     plintico = pl, concrecionario = co, litoplintico = li),
    missing = g$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 4, p. 112-113"
  )
}


# ---- 7. Latossolos --------------------------------------------------------

#' Latossolos (SiBCS Cap 4, p 113; conceito Cap 3, p 93-94)
#'
#' Horizonte B latossolico imediatamente abaixo de qualquer tipo de A
#' (exceto histico), dentro de 200 cm (ou 300 se A > 150 cm).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Numeric threshold or option (see Details).
#' @export
latossolo <- function(pedon, max_top_cm = 200) {
  res <- B_latossolico(pedon)
  if (!isTRUE(res$passed)) {
    return(DiagnosticResult$new(
      name = "latossolo", passed = res$passed,
      layers = res$layers, evidence = list(latossolico = res),
      missing = res$missing,
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 4, p. 113"
    ))
  }
  h <- pedon$horizons
  lat_top <- min(h$top_cm[res$layers], na.rm = TRUE)
  passed <- lat_top <= max_top_cm
  DiagnosticResult$new(
    name = "latossolo", passed = passed, layers = res$layers,
    evidence = list(latossolico = res, top_cm = lat_top),
    missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 4, p. 113"
  )
}


# ---- 8. Chernossolos ------------------------------------------------------

#' Chernossolos (SiBCS Cap 4, p 113; conceito Cap 3, p 89-90)
#'
#' A chernozemico seguido de:
#' (a) Bi OR Bt, ambos com argila ativ alta + V alta; OR
#' (b) Bi com espessura < 10 cm OR C, ambos calcicos, petrocalcicos
#'     OR carbonaticos; OR
#' (c) Calcico OR carater carbonatico no A, seguido de contato
#'     litico / fragmentario.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
chernossolo <- function(pedon) {
  ch <- horizonte_A_chernozemico(pedon)
  if (!isTRUE(ch$passed)) {
    return(DiagnosticResult$new(
      name = "chernossolo", passed = ch$passed,
      layers = integer(0), evidence = list(A_chernozemico = ch),
      missing = ch$missing,
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 4, p. 113"
    ))
  }
  ta <- atividade_argila_alta(pedon)
  eut <- eutrofico(pedon, min_v = 50)
  bi <- B_incipiente(pedon)
  bt <- B_textural(pedon)
  carb <- carater_carbonatico(pedon)
  cal <- horizonte_calcico(pedon)
  contato <- contato_litico(pedon)
  contato_frag <- contato_litico_fragmentario(pedon)
  path_a <- (isTRUE(bi$passed) || isTRUE(bt$passed)) &&
              isTRUE(ta$passed) && isTRUE(eut$passed)
  path_b <- (isTRUE(carb$passed) || isTRUE(cal$passed))
  path_c <- (isTRUE(contato$passed) || isTRUE(contato_frag$passed)) &&
              isTRUE(carb$passed)
  passed <- path_a || path_b || path_c
  DiagnosticResult$new(
    name = "chernossolo", passed = passed, layers = ch$layers,
    evidence = list(
      A_chernozemico = ch,
      path_a_bi_bt_alta = path_a,
      path_b_calcico_carbonatico = path_b,
      path_c_contato_carbonatico = path_c
    ),
    missing = ch$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 4, p. 113"
  )
}


# ---- 9. Cambissolos -------------------------------------------------------

#' Cambissolos (SiBCS Cap 4, p 113; conceito Cap 3, p 88-89)
#'
#' Horizonte B incipiente imediatamente abaixo de A ou histico < 40 cm,
#' com plintita/petroplintita (se presente) que NAO satisfaca aos
#' requisitos para Plintossolos.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
cambissolo <- function(pedon) {
  bi <- B_incipiente(pedon)
  if (!isTRUE(bi$passed)) {
    return(DiagnosticResult$new(
      name = "cambissolo", passed = bi$passed,
      layers = integer(0), evidence = list(B_incipiente = bi),
      missing = bi$missing,
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 4, p. 113"
    ))
  }
  pl <- horizonte_plintico(pedon)
  co <- horizonte_concrecionario(pedon)
  li <- horizonte_litoplintico(pedon)
  is_plintossolo <- any(c(isTRUE(pl$passed), isTRUE(co$passed),
                            isTRUE(li$passed)))
  passed <- !is_plintossolo
  DiagnosticResult$new(
    name = "cambissolo", passed = passed, layers = bi$layers,
    evidence = list(B_incipiente = bi,
                     plintossolo_excluded = is_plintossolo,
                     plintico = pl, concrecionario = co,
                     litoplintico = li),
    missing = bi$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 4, p. 113"
  )
}


# ---- 10. Plintossolos -----------------------------------------------------

#' Plintossolos (SiBCS Cap 4, p 113; conceito Cap 3, p 102-104)
#'
#' Horizonte plintico (nao coincidente com B planico de carater sodico),
#' OR litoplintico, OR concrecionario, iniciando dentro de 40 cm OR
#' dentro de 200 cm precedido de glei OR A/E OR horizonte com cores
#' palidas / variegadas / mosqueados.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
plintossolo <- function(pedon) {
  pl <- horizonte_plintico(pedon)
  co <- horizonte_concrecionario(pedon)
  li <- horizonte_litoplintico(pedon)
  bp <- B_planico(pedon); sod <- carater_sodico(pedon)
  any_plinthic <- any(c(isTRUE(pl$passed), isTRUE(co$passed),
                          isTRUE(li$passed)))
  if (!any_plinthic) {
    return(DiagnosticResult$new(
      name = "plintossolo",
      passed = if (any(c(is.na(pl$passed), is.na(co$passed),
                           is.na(li$passed)))) NA else FALSE,
      layers = integer(0),
      evidence = list(plintico = pl, concrecionario = co,
                       litoplintico = li),
      missing = unique(c(pl$missing, co$missing, li$missing)),
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 4, p. 113"
    ))
  }
  h <- pedon$horizons
  all_layers <- unique(c(pl$layers, co$layers, li$layers))
  pl_top <- min(h$top_cm[all_layers], na.rm = TRUE)
  in_40 <- pl_top <= 40
  in_200 <- pl_top <= 200
  # Exclusao: B planico de carater sodico
  excluded <- isTRUE(bp$passed) && isTRUE(sod$passed) &&
                length(intersect(all_layers, bp$layers)) > 0L
  passed <- (in_40 || in_200) && !excluded
  DiagnosticResult$new(
    name = "plintossolo", passed = passed,
    layers = all_layers,
    evidence = list(
      plintico = pl, concrecionario = co, litoplintico = li,
      top_cm = pl_top, in_40 = in_40, in_200 = in_200,
      excluded_b_planico_sodico = excluded
    ),
    missing = unique(c(pl$missing, co$missing, li$missing)),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 4, p. 113"
  )
}


# ---- 11. Luvissolos -------------------------------------------------------

#' Luvissolos (SiBCS Cap 4, p 113; conceito Cap 3, p 95-96)
#'
#' Horizonte B textural com argila ativ alta E saturacao por bases
#' alta (V \\>= 50\%) na maior parte dos primeiros 100 cm do B
#' (incl. BA), abaixo de A ou E.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
luvissolo <- function(pedon) {
  bt <- B_textural(pedon)
  if (!isTRUE(bt$passed)) {
    return(DiagnosticResult$new(
      name = "luvissolo", passed = bt$passed,
      layers = integer(0), evidence = list(B_textural = bt),
      missing = bt$missing,
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 4, p. 113"
    ))
  }
  ta <- atividade_argila_alta(pedon)
  eut <- eutrofico(pedon, min_v = 50)
  passed <- isTRUE(bt$passed) && isTRUE(ta$passed) && isTRUE(eut$passed)
  DiagnosticResult$new(
    name = "luvissolo", passed = passed, layers = bt$layers,
    evidence = list(B_textural = bt, ativ_alta = ta, eutrofico = eut),
    missing = c(bt$missing, ta$missing, eut$missing),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 4, p. 113"
  )
}


# ---- 12. Nitossolos -------------------------------------------------------

#' Nitossolos (SiBCS Cap 4, p 114; conceito Cap 3, p 97-98)
#'
#' \\>= 350 g/kg argila incluindo no horizonte A, com B nitico abaixo
#' do A, com argila ativ baixa OR ativ alta + carater alumínico, na
#' maior parte dos primeiros 100 cm do B (incl. BA).
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
nitossolo <- function(pedon) {
  bn <- B_nitico(pedon)
  if (!isTRUE(bn$passed)) {
    return(DiagnosticResult$new(
      name = "nitossolo", passed = bn$passed,
      layers = integer(0), evidence = list(B_nitico = bn),
      missing = bn$missing,
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 4, p. 114"
    ))
  }
  h <- pedon$horizons
  # Argila >= 35% from surface
  surface_layers <- which(!is.na(h$top_cm) & h$top_cm <= 30)
  surface_clay_ok <- length(surface_layers) > 0L &&
                       all(!is.na(h$clay_pct[surface_layers]) &
                              h$clay_pct[surface_layers] >= 35)
  passed <- isTRUE(bn$passed) && surface_clay_ok
  DiagnosticResult$new(
    name = "nitossolo", passed = passed, layers = bn$layers,
    evidence = list(B_nitico = bn, surface_clay_ok = surface_clay_ok),
    missing = bn$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 4, p. 114"
  )
}


# ---- 13. Argissolos (catch-all) ------------------------------------------

#' Argissolos (SiBCS Cap 4, p 114; conceito Cap 3, p 86-88)
#'
#' Horizonte B textural -- catch-all final na chave SiBCS apos
#' Luvissolos / Nitossolos terem sido excluidos. v0.7 enforce: B
#' textural + (argila ativ baixa OR ativ alta + V baixa OR carater
#' alumínico).
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
argissolo <- function(pedon) {
  bt <- B_textural(pedon)
  if (!isTRUE(bt$passed)) {
    return(DiagnosticResult$new(
      name = "argissolo", passed = bt$passed,
      layers = integer(0), evidence = list(B_textural = bt),
      missing = bt$missing,
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 4, p. 114"
    ))
  }
  # Argissolos catch B textural that doesn't fit Luvissolos / Nitossolos.
  # We accept any B textural here; the order in key.yaml ensures Luvissolos
  # and Nitossolos are tested first.
  DiagnosticResult$new(
    name = "argissolo", passed = TRUE, layers = bt$layers,
    evidence = list(B_textural = bt),
    missing = bt$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 4, p. 114"
  )
}
