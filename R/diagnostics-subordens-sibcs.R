# ============================================================================
# SiBCS 5a edicao (Embrapa, 2018) -- Diagnosticos do 2o nivel categorico
# (subordens), Caps 5-17.
#
# 44 subordens distribuidas em 13 ordens. Cada funcao testa o criterio
# diferenciador da subordem -- assumindo que o pedon ja passou no
# diagnostico da ordem (1o nivel). A funcao retorna um DiagnosticResult.
#
# A ordem das subordens dentro de cada ordem segue a chave do livro
# (Cap 5-17): a primeira subordem que satisfaz o criterio captura o
# perfil; "Haplicos" (ou equivalente) e o catch-all da ordem.
# ============================================================================


# Helper: extrai o B-horizon-like layer principal para testes Munsell
# tipicos (matiz/valor/croma na maior parte do horizonte B).
.b_layers <- function(pedon) {
  h <- pedon$horizons
  desg <- h$designation
  if (any(!is.na(desg))) {
    b <- which(grepl("^B[wt]?", desg, ignore.case = FALSE) &
                 !grepl("^BC|^Bt0", desg))
    if (length(b) > 0L) return(b)
  }
  # Fallback: 2nd horizon and below (excludes A surface).
  if (nrow(h) >= 2L) seq.int(2L, nrow(h)) else integer(0)
}

# Helper: hue-name predicate. Aceita patterns como "5YR" ou cores mais
# vermelhas/amarelas. WRB uses 7.5R, 5R, 2.5YR, 5YR, 7.5YR, 10YR, 2.5Y,
# 5Y, 10Y.
.hue_redder_or_eq <- function(hues, target_hue) {
  ladder <- c("10R" = 10, "7.5R" = 9, "5R" = 8, "2.5R" = 7,
              "10YR" = 6, "7.5YR" = 5, "5YR" = 4, "2.5YR" = 3,
              "10Y" = 2, "7.5Y" = 1.5, "5Y" = 1, "2.5Y" = 0)
  # NOTE: index goes redder -> yellower; invert for "redder than".
  red_score <- function(h) {
    if (is.na(h)) return(NA_real_)
    key <- toupper(trimws(h))
    if (key %in% names(ladder)) ladder[[key]] else NA_real_
  }
  target <- red_score(target_hue)
  vapply(hues, function(h) {
    s <- red_score(h)
    if (is.na(s) || is.na(target)) NA else s >= target
  }, logical(1))
}


# ============================================================
# 1. ARGISSOLOS (Cap 5) -- 5 subordens
# ============================================================

#' Argissolos Bruno-Acinzentados (SiBCS Cap 5)
#'
#' Argissolos com horizonte B textural, com matiz \\>= 5YR e cor escura
#' (valor \\<= 4 + croma \\<= 4 umidos) na maior parte do B (inclusive
#' BA).
#' @export
argissolo_bruno_acinzentado <- function(pedon) {
  h <- pedon$horizons
  bl <- .b_layers(pedon)
  hues <- h$munsell_hue_moist[bl]
  vals <- h$munsell_value_moist[bl]
  chrs <- h$munsell_chroma_moist[bl]
  ok <- !is.na(hues) & !is.na(vals) & !is.na(chrs) &
          .hue_redder_or_eq(hues, "5YR") &
          vals <= 4 & chrs <= 4
  passed <- any(ok, na.rm = TRUE)
  DiagnosticResult$new(
    name = "argissolo_bruno_acinzentado",
    passed = passed,
    layers = bl[which(ok)],
    evidence = list(b_layers = bl, hues = hues,
                     values = vals, chromas = chrs),
    missing = if (all(is.na(hues))) "munsell_hue_moist" else character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 5, Argissolos -- Bruno-Acinzentados"
  )
}


#' Argissolos Acinzentados (SiBCS Cap 5)
#'
#' Matiz \\>= 7.5YR, valor \\>= 5, croma \\< 4 (cores mais cinzentas /
#' palidas), na maior parte do B (inclusive BA).
#' @export
argissolo_acinzentado <- function(pedon) {
  h <- pedon$horizons
  bl <- .b_layers(pedon)
  hues <- h$munsell_hue_moist[bl]
  vals <- h$munsell_value_moist[bl]
  chrs <- h$munsell_chroma_moist[bl]
  ok <- !is.na(hues) & !is.na(vals) & !is.na(chrs) &
          .hue_redder_or_eq(hues, "7.5YR") &
          vals >= 5 & chrs < 4
  passed <- any(ok, na.rm = TRUE)
  DiagnosticResult$new(
    name = "argissolo_acinzentado",
    passed = passed,
    layers = bl[which(ok)],
    evidence = list(b_layers = bl, hues = hues,
                     values = vals, chromas = chrs),
    missing = if (all(is.na(hues))) "munsell_hue_moist" else character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 5, Argissolos -- Acinzentados"
  )
}


#' Argissolos Amarelos (SiBCS Cap 5)
#'
#' Matiz \\>= 7.5YR (mais amarelo) na maior parte do B, sem ser
#' Acinzentado (croma \\>= 4).
#' @export
argissolo_amarelo <- function(pedon) {
  h <- pedon$horizons
  bl <- .b_layers(pedon)
  hues <- h$munsell_hue_moist[bl]
  vals <- h$munsell_value_moist[bl]
  chrs <- h$munsell_chroma_moist[bl]
  ok <- !is.na(hues) & .hue_redder_or_eq(hues, "7.5YR") &
          (is.na(chrs) | chrs >= 4)
  passed <- any(ok, na.rm = TRUE)
  DiagnosticResult$new(
    name = "argissolo_amarelo", passed = passed,
    layers = bl[which(ok)],
    evidence = list(b_layers = bl, hues = hues, chromas = chrs),
    missing = if (all(is.na(hues))) "munsell_hue_moist" else character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 5, Argissolos -- Amarelos"
  )
}


#' Argissolos Vermelhos (SiBCS Cap 5)
#'
#' Matiz \\<= 2.5YR (mais vermelho) na maior parte do B.
#' @export
argissolo_vermelho <- function(pedon) {
  h <- pedon$horizons
  bl <- .b_layers(pedon)
  hues <- h$munsell_hue_moist[bl]
  ok <- !is.na(hues) & sapply(hues, function(hu) {
    if (is.na(hu)) FALSE else
      grepl("^(2\\.5YR|10R|7\\.5R|5R|2\\.5R)\\b", hu, ignore.case = TRUE)
  })
  passed <- any(ok, na.rm = TRUE)
  DiagnosticResult$new(
    name = "argissolo_vermelho", passed = passed,
    layers = bl[which(ok)],
    evidence = list(b_layers = bl, hues = hues),
    missing = if (all(is.na(hues))) "munsell_hue_moist" else character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 5, Argissolos -- Vermelhos"
  )
}


#' Argissolos Vermelho-Amarelos (catch-all dos Argissolos)
#' @export
argissolo_vermelho_amarelo <- function(pedon) {
  DiagnosticResult$new(
    name = "argissolo_vermelho_amarelo", passed = TRUE,
    layers = .b_layers(pedon),
    evidence = list(catch_all = TRUE),
    missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 5, Argissolos -- Vermelho-Amarelos (catch-all)"
  )
}


# ============================================================
# 2. CAMBISSOLOS (Cap 6) -- 4 subordens
# ============================================================

#' Cambissolos Histicos (Cap 6): horizonte histico sem espessura para Organossolo.
#' @export
cambissolo_histico <- function(pedon) {
  h <- horizonte_histico(pedon)
  passed <- isTRUE(h$passed) && !isTRUE(organossolo(pedon)$passed)
  DiagnosticResult$new(
    name = "cambissolo_histico", passed = passed,
    layers = h$layers, evidence = list(histico = h),
    missing = h$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 6, Cambissolos -- Histicos"
  )
}

#' Cambissolos Humicos (Cap 6): horizonte A humico.
#' @export
cambissolo_humico <- function(pedon) {
  h <- horizonte_A_humico(pedon)
  DiagnosticResult$new(
    name = "cambissolo_humico", passed = h$passed,
    layers = h$layers, evidence = list(a_humico = h),
    missing = h$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 6, Cambissolos -- Humicos"
  )
}

#' Cambissolos Fluvicos (Cap 6): carater fluvico.
#' @export
cambissolo_fluvico <- function(pedon) {
  h <- carater_fluvico(pedon)
  DiagnosticResult$new(
    name = "cambissolo_fluvico", passed = h$passed,
    layers = h$layers, evidence = list(fluvico = h),
    missing = h$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 6, Cambissolos -- Fluvicos"
  )
}

#' Cambissolos Haplicos (catch-all).
#' @export
cambissolo_haplico <- function(pedon) {
  DiagnosticResult$new(
    name = "cambissolo_haplico", passed = TRUE,
    layers = seq_len(nrow(pedon$horizons)),
    evidence = list(catch_all = TRUE), missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 6, Cambissolos -- Haplicos (catch-all)"
  )
}


# ============================================================
# 3. CHERNOSSOLOS (Cap 7) -- 4 subordens
# ============================================================

#' Chernossolos Rendzicos (Cap 7): A chernozemico + (calcico/petrocalcico
#' OR carater carbonatico).
#' @export
chernossolo_rendzico <- function(pedon) {
  cal <- horizonte_calcico(pedon)
  pet <- horizonte_petrocalcico(pedon)
  cb  <- carater_carbonatico(pedon)
  passed <- isTRUE(cal$passed) || isTRUE(pet$passed) || isTRUE(cb$passed)
  DiagnosticResult$new(
    name = "chernossolo_rendzico", passed = passed,
    layers = unique(c(cal$layers, pet$layers, cb$layers)),
    evidence = list(calcico = cal, petrocalcico = pet, carbonatico = cb),
    missing = unique(c(cal$missing, pet$missing, cb$missing)),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 7, Chernossolos -- Rendzicos"
  )
}

#' Chernossolos Ebanicos (Cap 7): caracter ebanico em B.
#' v0.7.1: detecta via Munsell em B - hue 7.5YR ou mais amarelo: V<4 +
#' C<3 umido; OR hue mais vermelho 7.5YR: preto/cinza muito escuro.
#' @export
chernossolo_ebanico <- function(pedon) {
  h <- pedon$horizons
  bl <- .b_layers(pedon)
  hues <- h$munsell_hue_moist[bl]
  vals <- h$munsell_value_moist[bl]
  chrs <- h$munsell_chroma_moist[bl]
  ok <- mapply(function(hu, v, c) {
    if (is.na(v) || is.na(c)) return(FALSE)
    if (!is.na(hu) && grepl("(7\\.5YR|10YR|2\\.5Y|5Y|10Y)", hu,
                                 ignore.case = TRUE)) {
      v < 4 && c < 3
    } else {
      v <= 3 && c <= 2  # darker / more reddish
    }
  }, hues, vals, chrs)
  passed <- any(ok, na.rm = TRUE)
  DiagnosticResult$new(
    name = "chernossolo_ebanico", passed = passed,
    layers = bl[which(ok)],
    evidence = list(hues = hues, values = vals, chromas = chrs),
    missing = if (all(is.na(vals))) "munsell_value_moist" else character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 7, Chernossolos -- Ebanicos"
  )
}

#' Chernossolos Argiluvicos (Cap 7): B textural abaixo do A chernozemico.
#' @export
chernossolo_argiluvico <- function(pedon) {
  bt <- B_textural(pedon)
  DiagnosticResult$new(
    name = "chernossolo_argiluvico", passed = bt$passed,
    layers = bt$layers, evidence = list(b_textural = bt),
    missing = bt$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 7, Chernossolos -- Argiluvicos"
  )
}

#' Chernossolos Haplicos (catch-all).
#' @export
chernossolo_haplico <- function(pedon) {
  DiagnosticResult$new(
    name = "chernossolo_haplico", passed = TRUE,
    layers = seq_len(nrow(pedon$horizons)),
    evidence = list(catch_all = TRUE), missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 7, Chernossolos -- Haplicos (catch-all)"
  )
}


# ============================================================
# 4. ESPODOSSOLOS (Cap 8) -- 3 subordens
# ============================================================

# v0.7.1: separamos via designation pattern. Bh = Humilúvico,
# Bs/Bhs = Ferrilúvico/Ferri-humilúvico. Mais robusto seria checar
# Al-Fe-OC ratios mas v5 usa o B espódico subtype.

#' Espodossolos Humiluvicos (Cap 8): B espodico tipo Bh (org. + Al,
#' pouco/sem Fe).
#' @export
espodossolo_humiluvico <- function(pedon) {
  h <- pedon$horizons
  has_bh <- any(!is.na(h$designation) & grepl("^Bh\\b", h$designation))
  has_bhs_or_bs <- any(!is.na(h$designation) &
                          grepl("^Bhs|^Bs", h$designation))
  passed <- has_bh && !has_bhs_or_bs
  DiagnosticResult$new(
    name = "espodossolo_humiluvico", passed = passed,
    layers = which(grepl("^Bh\\b", h$designation %||% character(0))),
    evidence = list(has_bh = has_bh, has_bhs_or_bs = has_bhs_or_bs),
    missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 8, Espodossolos -- Humiluvicos"
  )
}

#' Espodossolos Ferriluvicos (Cap 8): B espodico tipo Bs (Fe + Al, baixo
#' OC iluvial).
#' @export
espodossolo_ferriluvico <- function(pedon) {
  h <- pedon$horizons
  has_bs <- any(!is.na(h$designation) & grepl("^Bs\\b", h$designation))
  has_bhs <- any(!is.na(h$designation) & grepl("^Bhs", h$designation))
  passed <- has_bs && !has_bhs
  DiagnosticResult$new(
    name = "espodossolo_ferriluvico", passed = passed,
    layers = which(grepl("^Bs\\b", h$designation %||% character(0))),
    evidence = list(has_bs = has_bs, has_bhs = has_bhs),
    missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 8, Espodossolos -- Ferriluvicos"
  )
}

#' Espodossolos Ferri-humiluvicos (Cap 8): B espodico tipo Bhs OR
#' catch-all dos espodossolos.
#' @export
espodossolo_ferri_humiluvico <- function(pedon) {
  h <- pedon$horizons
  has_bhs <- any(!is.na(h$designation) & grepl("^Bhs", h$designation))
  passed <- has_bhs ||
              # catch-all: passa se nao casou nas duas primeiras
              (!isTRUE(espodossolo_humiluvico(pedon)$passed) &&
                 !isTRUE(espodossolo_ferriluvico(pedon)$passed))
  DiagnosticResult$new(
    name = "espodossolo_ferri_humiluvico", passed = passed,
    layers = which(grepl("^Bhs", h$designation %||% character(0))),
    evidence = list(has_bhs = has_bhs),
    missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 8, Espodossolos -- Ferri-humiluvicos (or catch-all)"
  )
}


# ============================================================
# 5. GLEISSOLOS (Cap 9) -- 4 subordens
# ============================================================

#' Gleissolos Tiomorficos (Cap 9): materiais sulfidricos OR horizonte
#' sulfurico em < 100 cm.
#' @export
gleissolo_tiomorfico <- function(pedon) {
  hs <- horizonte_sulfurico(pedon)
  passed <- isTRUE(hs$passed)
  DiagnosticResult$new(
    name = "gleissolo_tiomorfico", passed = passed,
    layers = hs$layers, evidence = list(sulfurico = hs),
    missing = hs$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 9, Gleissolos -- Tiomorficos"
  )
}

#' Gleissolos Salicos (Cap 9): caracter salico em < 100 cm.
#' @export
gleissolo_salico <- function(pedon) {
  cs <- carater_salico(pedon)
  DiagnosticResult$new(
    name = "gleissolo_salico", passed = cs$passed,
    layers = cs$layers, evidence = list(salico = cs),
    missing = cs$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 9, Gleissolos -- Salicos"
  )
}

#' Gleissolos Melanicos (Cap 9): horizonte hístico < 40 cm OR A humico,
#' proeminente, chernozemico.
#' @export
gleissolo_melanico <- function(pedon) {
  h <- horizonte_histico(pedon)
  ah <- horizonte_A_humico(pedon)
  ap <- horizonte_A_proeminente(pedon)
  ac <- horizonte_A_chernozemico(pedon)
  passed <- isTRUE(h$passed) || isTRUE(ah$passed) ||
              isTRUE(ap$passed) || isTRUE(ac$passed)
  DiagnosticResult$new(
    name = "gleissolo_melanico", passed = passed,
    layers = unique(c(h$layers, ah$layers, ap$layers, ac$layers)),
    evidence = list(histico = h, a_humico = ah,
                     a_proeminente = ap, a_chernozemico = ac),
    missing = unique(c(h$missing, ah$missing, ap$missing, ac$missing)),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 9, Gleissolos -- Melanicos"
  )
}

#' Gleissolos Haplicos (catch-all).
#' @export
gleissolo_haplico <- function(pedon) {
  DiagnosticResult$new(
    name = "gleissolo_haplico", passed = TRUE,
    layers = seq_len(nrow(pedon$horizons)),
    evidence = list(catch_all = TRUE), missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 9, Gleissolos -- Haplicos"
  )
}


# ============================================================
# 6. LATOSSOLOS (Cap 10) -- 4 subordens
# ============================================================

#' Latossolos Brunos (Cap 10): matiz \\>= 7.5YR + valor \\<= 4 + croma
#' \\<= 5 (cores brunadas) OR caracter retratil.
#' @export
latossolo_bruno <- function(pedon) {
  h <- pedon$horizons
  bl <- .b_layers(pedon)
  hues <- h$munsell_hue_moist[bl]
  vals <- h$munsell_value_moist[bl]
  chrs <- h$munsell_chroma_moist[bl]
  ok <- !is.na(hues) & !is.na(vals) & !is.na(chrs) &
          .hue_redder_or_eq(hues, "7.5YR") & vals <= 4 & chrs <= 5
  passed <- any(ok, na.rm = TRUE)
  DiagnosticResult$new(
    name = "latossolo_bruno", passed = passed,
    layers = bl[which(ok)],
    evidence = list(hues = hues, values = vals, chromas = chrs),
    missing = if (all(is.na(hues))) "munsell_hue_moist" else character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 10, Latossolos -- Brunos"
  )
}

#' Latossolos Amarelos (Cap 10): matiz \\>= 7.5YR (mais amarelo).
#' @export
latossolo_amarelo <- function(pedon) {
  h <- pedon$horizons
  bl <- .b_layers(pedon)
  hues <- h$munsell_hue_moist[bl]
  ok <- !is.na(hues) & .hue_redder_or_eq(hues, "7.5YR")
  passed <- any(ok, na.rm = TRUE)
  DiagnosticResult$new(
    name = "latossolo_amarelo", passed = passed,
    layers = bl[which(ok)],
    evidence = list(hues = hues),
    missing = if (all(is.na(hues))) "munsell_hue_moist" else character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 10, Latossolos -- Amarelos"
  )
}

#' Latossolos Vermelhos (Cap 10): matiz \\<= 2.5YR (mais vermelho).
#' @export
latossolo_vermelho <- function(pedon) {
  h <- pedon$horizons
  bl <- .b_layers(pedon)
  hues <- h$munsell_hue_moist[bl]
  ok <- !is.na(hues) & sapply(hues, function(hu) {
    if (is.na(hu)) FALSE else
      grepl("^(2\\.5YR|10R|7\\.5R|5R|2\\.5R)\\b", hu, ignore.case = TRUE)
  })
  passed <- any(ok, na.rm = TRUE)
  DiagnosticResult$new(
    name = "latossolo_vermelho", passed = passed,
    layers = bl[which(ok)],
    evidence = list(hues = hues),
    missing = if (all(is.na(hues))) "munsell_hue_moist" else character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 10, Latossolos -- Vermelhos"
  )
}

#' Latossolos Vermelho-Amarelos (catch-all).
#' @export
latossolo_vermelho_amarelo <- function(pedon) {
  DiagnosticResult$new(
    name = "latossolo_vermelho_amarelo", passed = TRUE,
    layers = .b_layers(pedon),
    evidence = list(catch_all = TRUE), missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 10, Latossolos -- Vermelho-Amarelos"
  )
}


# ============================================================
# 7. LUVISSOLOS (Cap 11) -- 2 subordens
# ============================================================

#' Luvissolos Cromicos (Cap 11): caracter cromico (cores fortes em B).
#' Aplicado pela presenca de Munsell vermelho-amarelado em B com cromas
#' altos.
#' @export
luvissolo_cromico <- function(pedon) {
  h <- pedon$horizons
  bl <- .b_layers(pedon)
  hues <- h$munsell_hue_moist[bl]
  vals <- h$munsell_value_moist[bl]
  chrs <- h$munsell_chroma_moist[bl]
  # caracter cromico: B com cores definidas (ver SiBCS Cap 1, Caracter cromico)
  # 5YR ou mais vermelho com V>=3 e C>=4; OR matiz 5YR-10YR V>=4 C>=4;
  # OR matiz 10YR-5Y V>=5 C>4
  ok <- mapply(function(hu, v, c) {
    if (is.na(hu) || is.na(v) || is.na(c)) return(FALSE)
    if (grepl("^(2\\.5YR|10R|7\\.5R|5R|2\\.5R|5YR)", hu,
                  ignore.case = TRUE)) v >= 3 && c >= 4
    else if (grepl("^(7\\.5YR|10YR)", hu, ignore.case = TRUE)) v >= 4 && c >= 4
    else v >= 5 && c > 4
  }, hues, vals, chrs)
  passed <- any(ok, na.rm = TRUE)
  DiagnosticResult$new(
    name = "luvissolo_cromico", passed = passed,
    layers = bl[which(ok)],
    evidence = list(hues = hues, values = vals, chromas = chrs),
    missing = if (all(is.na(hues))) "munsell_hue_moist" else character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 11, Luvissolos -- Cromicos"
  )
}

#' Luvissolos Haplicos (catch-all).
#' @export
luvissolo_haplico <- function(pedon) {
  DiagnosticResult$new(
    name = "luvissolo_haplico", passed = TRUE,
    layers = seq_len(nrow(pedon$horizons)),
    evidence = list(catch_all = TRUE), missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 11, Luvissolos -- Haplicos"
  )
}


# ============================================================
# 8. NEOSSOLOS (Cap 12) -- 4 subordens
# ============================================================

#' Neossolos Litolicos (Cap 12): contato litico ou litico fragmentario
#' \\<= 50 cm.
#' @export
neossolo_litolico <- function(pedon) {
  cl <- contato_litico(pedon)
  cf <- contato_litico_fragmentario(pedon)
  h <- pedon$horizons
  shallow <- any(h$top_cm <= 50, na.rm = TRUE)
  passed <- (isTRUE(cl$passed) || isTRUE(cf$passed)) && shallow
  DiagnosticResult$new(
    name = "neossolo_litolico", passed = passed,
    layers = unique(c(cl$layers, cf$layers)),
    evidence = list(litico = cl, litico_fragmentario = cf),
    missing = unique(c(cl$missing, cf$missing)),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 12, Neossolos -- Litolicos"
  )
}

#' Neossolos Fluvicos (Cap 12): caracter fluvico em < 150 cm.
#' @export
neossolo_fluvico <- function(pedon) {
  cf <- carater_fluvico(pedon)
  DiagnosticResult$new(
    name = "neossolo_fluvico", passed = cf$passed,
    layers = cf$layers, evidence = list(fluvico = cf),
    missing = cf$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 12, Neossolos -- Fluvicos"
  )
}

#' Neossolos Quartzarenicos (Cap 12): textura areia/areia franca em
#' todos os horizontes ate 150 cm + 95% quartzo.
#' @export
neossolo_quartzarenico <- function(pedon) {
  h <- pedon$horizons
  layers_in_150 <- which(h$top_cm <= 150)
  if (length(layers_in_150) == 0L) {
    return(DiagnosticResult$new(
      name = "neossolo_quartzarenico", passed = FALSE,
      layers = integer(0), evidence = list(),
      missing = character(0),
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 12, Neossolos -- Quartzarenicos"
    ))
  }
  sand <- h$sand_pct[layers_in_150]
  clay <- h$clay_pct[layers_in_150]
  # textura areia (sand >= 700, clay < 150) ou areia franca (sand >= 700,
  # clay < 200)
  is_arenoso <- !is.na(sand) & !is.na(clay) &
                  sand >= 700 & clay < 200
  passed <- length(is_arenoso) > 0L && all(is_arenoso, na.rm = TRUE)
  DiagnosticResult$new(
    name = "neossolo_quartzarenico", passed = passed,
    layers = if (passed) layers_in_150 else integer(0),
    evidence = list(sand = sand, clay = clay),
    missing = if (all(is.na(sand))) "sand_pct" else character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 12, Neossolos -- Quartzarenicos"
  )
}

#' Neossolos Regoliticos (catch-all dos Neossolos).
#' @export
neossolo_regolitico <- function(pedon) {
  DiagnosticResult$new(
    name = "neossolo_regolitico", passed = TRUE,
    layers = seq_len(nrow(pedon$horizons)),
    evidence = list(catch_all = TRUE), missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 12, Neossolos -- Regoliticos"
  )
}


# ============================================================
# 9. NITOSSOLOS (Cap 13) -- 3 subordens
# ============================================================

#' Nitossolos Brunos (Cap 13): matiz \\>= 7.5YR + valor <= 4 + croma <= 5.
#' @export
nitossolo_bruno <- function(pedon) {
  h <- pedon$horizons
  bl <- .b_layers(pedon)
  hues <- h$munsell_hue_moist[bl]
  vals <- h$munsell_value_moist[bl]
  chrs <- h$munsell_chroma_moist[bl]
  ok <- !is.na(hues) & !is.na(vals) & !is.na(chrs) &
          .hue_redder_or_eq(hues, "7.5YR") & vals <= 4 & chrs <= 5
  passed <- any(ok, na.rm = TRUE)
  DiagnosticResult$new(
    name = "nitossolo_bruno", passed = passed,
    layers = bl[which(ok)],
    evidence = list(hues = hues, values = vals, chromas = chrs),
    missing = if (all(is.na(hues))) "munsell_hue_moist" else character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 13, Nitossolos -- Brunos"
  )
}

#' Nitossolos Vermelhos (Cap 13): matiz \\<= 2.5YR.
#' @export
nitossolo_vermelho <- function(pedon) {
  h <- pedon$horizons
  bl <- .b_layers(pedon)
  hues <- h$munsell_hue_moist[bl]
  ok <- !is.na(hues) & sapply(hues, function(hu) {
    if (is.na(hu)) FALSE else
      grepl("^(2\\.5YR|10R|7\\.5R|5R|2\\.5R)\\b", hu, ignore.case = TRUE)
  })
  passed <- any(ok, na.rm = TRUE)
  DiagnosticResult$new(
    name = "nitossolo_vermelho", passed = passed,
    layers = bl[which(ok)],
    evidence = list(hues = hues),
    missing = if (all(is.na(hues))) "munsell_hue_moist" else character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 13, Nitossolos -- Vermelhos"
  )
}

#' Nitossolos Haplicos (catch-all).
#' @export
nitossolo_haplico <- function(pedon) {
  DiagnosticResult$new(
    name = "nitossolo_haplico", passed = TRUE,
    layers = seq_len(nrow(pedon$horizons)),
    evidence = list(catch_all = TRUE), missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 13, Nitossolos -- Haplicos"
  )
}


# ============================================================
# 10. ORGANOSSOLOS (Cap 14) -- 3 subordens
# ============================================================

#' Organossolos Tiomorficos (Cap 14): materiais sulfidricos OR
#' horizonte sulfurico em < 100 cm.
#' @export
organossolo_tiomorfico <- function(pedon) {
  hs <- horizonte_sulfurico(pedon)
  DiagnosticResult$new(
    name = "organossolo_tiomorfico", passed = hs$passed,
    layers = hs$layers, evidence = list(sulfurico = hs),
    missing = hs$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 14, Organossolos -- Tiomorficos"
  )
}

#' Organossolos Folicos (Cap 14): horizonte O histico (drenado).
#' Detectado via designation pattern \"^O\".
#' @export
organossolo_folico <- function(pedon) {
  h <- pedon$horizons
  ok <- !is.na(h$designation) & grepl("^O[ahde]?", h$designation)
  passed <- any(ok)
  DiagnosticResult$new(
    name = "organossolo_folico", passed = passed,
    layers = which(ok),
    evidence = list(designations = h$designation),
    missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 14, Organossolos -- Folicos"
  )
}

#' Organossolos Haplicos (catch-all).
#' @export
organossolo_haplico <- function(pedon) {
  DiagnosticResult$new(
    name = "organossolo_haplico", passed = TRUE,
    layers = seq_len(nrow(pedon$horizons)),
    evidence = list(catch_all = TRUE), missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 14, Organossolos -- Haplicos"
  )
}


# ============================================================
# 11. PLANOSSOLOS (Cap 15) -- 2 subordens
# ============================================================

#' Planossolos Natricos (Cap 15): caracter sodico em \\< 100 cm.
#' @export
planossolo_natrico <- function(pedon) {
  cs <- carater_sodico(pedon)
  DiagnosticResult$new(
    name = "planossolo_natrico", passed = cs$passed,
    layers = cs$layers, evidence = list(sodico = cs),
    missing = cs$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 15, Planossolos -- Natricos"
  )
}

#' Planossolos Haplicos (catch-all).
#' @export
planossolo_haplico <- function(pedon) {
  DiagnosticResult$new(
    name = "planossolo_haplico", passed = TRUE,
    layers = seq_len(nrow(pedon$horizons)),
    evidence = list(catch_all = TRUE), missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 15, Planossolos -- Haplicos"
  )
}


# ============================================================
# 12. PLINTOSSOLOS (Cap 16) -- 3 subordens
# ============================================================

#' Plintossolos Petricos (Cap 16): horizonte concrecionario OR
#' litoplintico (sem horizonte plintico precedendo).
#' @export
plintossolo_petrico <- function(pedon) {
  hc <- horizonte_concrecionario(pedon)
  hl <- horizonte_litoplintico(pedon)
  hp <- horizonte_plintico(pedon)
  passed <- (isTRUE(hc$passed) || isTRUE(hl$passed)) &&
              !(isTRUE(hp$passed) &&
                  any(pedon$horizons$top_cm[hp$layers] <
                          min(c(hc$layers, hl$layers), Inf), na.rm = TRUE))
  DiagnosticResult$new(
    name = "plintossolo_petrico", passed = passed,
    layers = unique(c(hc$layers, hl$layers)),
    evidence = list(concrecionario = hc, litoplintico = hl,
                     plintico_above = hp),
    missing = unique(c(hc$missing, hl$missing)),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 16, Plintossolos -- Petricos"
  )
}

#' Plintossolos Argiluvicos (Cap 16): horizonte plintico + B textural OR
#' carater argiluvico.
#' @export
plintossolo_argiluvico <- function(pedon) {
  hp <- horizonte_plintico(pedon)
  bt <- B_textural(pedon)
  passed <- isTRUE(hp$passed) && isTRUE(bt$passed)
  DiagnosticResult$new(
    name = "plintossolo_argiluvico", passed = passed,
    layers = intersect(hp$layers, bt$layers),
    evidence = list(plintico = hp, b_textural = bt),
    missing = unique(c(hp$missing, bt$missing)),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 16, Plintossolos -- Argiluvicos"
  )
}

#' Plintossolos Haplicos (catch-all).
#' @export
plintossolo_haplico <- function(pedon) {
  DiagnosticResult$new(
    name = "plintossolo_haplico", passed = TRUE,
    layers = seq_len(nrow(pedon$horizons)),
    evidence = list(catch_all = TRUE), missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 16, Plintossolos -- Haplicos"
  )
}


# ============================================================
# 13. VERTISSOLOS (Cap 17) -- 3 subordens
# ============================================================

#' Vertissolos Hidromorficos (Cap 17): horizonte glei OR caracter
#' redoxico.
#' @export
vertissolo_hidromorfico <- function(pedon) {
  hg <- horizonte_glei(pedon)
  cr <- carater_redoxico(pedon)
  passed <- isTRUE(hg$passed) || isTRUE(cr$passed)
  DiagnosticResult$new(
    name = "vertissolo_hidromorfico", passed = passed,
    layers = unique(c(hg$layers, cr$layers)),
    evidence = list(glei = hg, redoxico = cr),
    missing = unique(c(hg$missing, cr$missing)),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 17, Vertissolos -- Hidromorficos"
  )
}

#' Vertissolos Ebanicos (Cap 17): caracter ebanico em B (cores escuras
#' dominantes).
#' @export
vertissolo_ebanico <- function(pedon) {
  # reuse the ebanico Munsell test from chernossolo_ebanico but layers
  # already vertic.
  ce <- chernossolo_ebanico(pedon)
  DiagnosticResult$new(
    name = "vertissolo_ebanico", passed = ce$passed,
    layers = ce$layers, evidence = list(ebanico = ce),
    missing = ce$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 17, Vertissolos -- Ebanicos"
  )
}

#' Vertissolos Haplicos (catch-all).
#' @export
vertissolo_haplico <- function(pedon) {
  DiagnosticResult$new(
    name = "vertissolo_haplico", passed = TRUE,
    layers = seq_len(nrow(pedon$horizons)),
    evidence = list(catch_all = TRUE), missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 17, Vertissolos -- Haplicos"
  )
}
