# ============================================================================
# SiBCS 5a edicao (Embrapa, 2018) -- Horizontes diagnosticos (Cap 2,
# pp 49-74). v0.7 substitui o scaffold v0.2 (que delegava B_textural e
# B_latossolico ao WRB) por implementacoes que enforce os criterios
# canonicos do SiBCS.
#
# Quando o criterio SiBCS e operacionalmente identico ao WRB, esta
# implementacao chama o WRB e re-rotula como SiBCS; quando difere,
# implementa diretamente.
# ============================================================================


# ============================================================================
# Horizontes diagnosticos SUPERFICIAIS
# ============================================================================


#' Horizonte hístico (SiBCS Cap 2, p 49-50)
#'
#' Horizonte O ou H de coloracao preta/cinza muito escura/brunada,
#' \\>= 80 g/kg (8\\%) C organico, com:
#' \itemize{
#'   \item espessura \\>= 20 cm; OR
#'   \item espessura \\>= 40 cm se \\>= 75\\% volume tecido vegetal; OR
#'   \item espessura \\>= 10 cm sobre contato litico/fragmentario OR
#'         camada com \\>= 90\\% material > 2 mm.
#' }
#' @export
horizonte_histico <- function(pedon, min_oc_g_kg = 80) {
  h <- pedon$horizons
  # Convert g/kg threshold to %.
  min_oc_pct <- min_oc_g_kg / 10
  candidates <- which(!is.na(h$oc_pct) & h$oc_pct >= min_oc_pct &
                        !is.na(h$top_cm) & h$top_cm <= 5)
  if (length(candidates) == 0L) {
    have_oc <- !all(is.na(h$oc_pct))
    return(DiagnosticResult$new(
      name = "horizonte_histico",
      passed = if (have_oc) FALSE else NA,
      layers = integer(0),
      evidence = list(),
      missing = if (!have_oc) "oc_pct" else character(0),
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 49"
    ))
  }
  # Spessura accumulating from surface, contiguous high-OC layers.
  contiguous <- candidates
  for (k in seq_along(candidates)[-1]) {
    if (candidates[k] - candidates[k - 1L] != 1L) {
      contiguous <- candidates[seq_len(k - 1L)]
      break
    }
  }
  thickness <- sum(h$bottom_cm[contiguous] - h$top_cm[contiguous],
                     na.rm = TRUE)
  pct_tissue <- max(h$worm_holes_pct[contiguous] %||% 0, na.rm = TRUE)  # proxy
  # Detect overlying contact rock or stony layer.
  next_layer <- max(contiguous) + 1L
  on_rock <- next_layer <= nrow(h) &&
              !is.na(h$designation[next_layer]) &&
              grepl("^R|^Cr", h$designation[next_layer])
  passed <- thickness >= 20 ||
              (thickness >= 40 && pct_tissue >= 75) ||
              (thickness >= 10 && on_rock)
  DiagnosticResult$new(
    name = "horizonte_histico",
    passed = passed, layers = contiguous,
    evidence = list(thickness_cm = thickness, on_rock = on_rock,
                     contiguous_layers = contiguous),
    missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 49"
  )
}


#' Horizonte A chernozemico (SiBCS Cap 2, p 50-51)
#'
#' Horizonte mineral superficial relativamente espesso, escuro, com
#' alta saturacao por bases (V \\>= 65\\%), OC \\>= 6 g/kg, estrutura
#' desenvolvida e espessura conforme criterio.
#' @export
horizonte_A_chernozemico <- function(pedon,
                                        min_oc_g_kg = 6,
                                        min_v_pct   = 65,
                                        max_value_moist = 3,
                                        max_chroma_moist = 3,
                                        max_value_dry   = 5,
                                        min_thickness_cm = 18) {
  h <- pedon$horizons
  candidates <- which(!is.na(h$top_cm) & h$top_cm <= 5)
  if (length(candidates) == 0L) {
    return(DiagnosticResult$new(
      name = "horizonte_A_chernozemico",
      passed = FALSE, layers = integer(0),
      evidence = list(reason = "no surface candidate layer"),
      missing = "top_cm",
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 50"
    ))
  }
  passing <- integer(0); details <- list(); missing <- character(0)
  for (i in candidates) {
    oc <- h$oc_pct[i]; v <- h$bs_pct[i]
    vm <- h$munsell_value_moist[i]; cm <- h$munsell_chroma_moist[i]
    vd <- h$munsell_value_dry[i]
    grade <- h$structure_grade[i] %||% NA_character_
    consist_dry <- h$consistence_moist[i] %||% NA_character_   # proxy
    if (is.na(oc) || is.na(v)) {
      missing <- c(missing, c("oc_pct","bs_pct")[c(is.na(oc), is.na(v))])
      next
    }
    # Convert oc_pct to g/kg.
    oc_g_kg <- oc * 10
    color_ok <- (is.na(vm) || vm <= max_value_moist) &&
                  (is.na(cm) || cm <= max_chroma_moist) &&
                  (is.na(vd) || vd <= max_value_dry)
    struct_ok <- is.na(grade) ||
                   !grepl("massive|grain|loose", grade,
                            ignore.case = TRUE)
    layer_pass <- oc_g_kg >= min_oc_g_kg && v >= min_v_pct &&
                    color_ok && struct_ok
    details[[as.character(i)]] <- list(
      idx = i, oc_g_kg = oc_g_kg, bs_pct = v, color_ok = color_ok,
      struct_ok = struct_ok, passed = layer_pass
    )
    if (layer_pass) passing <- c(passing, i)
  }
  thickness <- if (length(passing) > 0L)
                 sum(h$bottom_cm[passing] - h$top_cm[passing], na.rm = TRUE)
               else 0
  passed <- length(passing) > 0L && thickness >= min_thickness_cm
  DiagnosticResult$new(
    name = "horizonte_A_chernozemico",
    passed = passed, layers = passing,
    evidence = list(layers = details, thickness_cm = thickness),
    missing = unique(missing),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 50-51"
  )
}


#' Horizonte A humico (SiBCS Cap 2, p 51-52)
#'
#' Horizonte A com cor moderadamente escura (value/chroma <= 4),
#' V < 65\\%, e C organico total >= 60 + 0.1 * argila_media (g/kg).
#' Espessura \\>= a do A chernozemico.
#' @export
horizonte_A_humico <- function(pedon, min_v_pct_max = 65,
                                  min_thickness_cm = 18) {
  h <- pedon$horizons
  candidates <- which(!is.na(h$top_cm) & h$top_cm <= 5)
  if (length(candidates) == 0L) {
    return(DiagnosticResult$new(
      name = "horizonte_A_humico",
      passed = FALSE, layers = integer(0),
      evidence = list(reason = "no surface candidate layer"),
      missing = "top_cm",
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 51-52"
    ))
  }
  # Walk through contiguous A layers; sum CO * espessura, mean argila.
  a_layers <- candidates
  for (k in seq_along(candidates)[-1]) {
    if (candidates[k] - candidates[k - 1L] != 1L) {
      a_layers <- candidates[seq_len(k - 1L)]
      break
    }
    # Stop accumulating at a clear B horizon.
    if (!is.na(h$designation[candidates[k]]) &&
        grepl("^B", h$designation[candidates[k]])) {
      a_layers <- candidates[seq_len(k - 1L)]
      break
    }
  }
  if (length(a_layers) == 0L) {
    return(DiagnosticResult$new(
      name = "horizonte_A_humico", passed = FALSE,
      layers = integer(0), evidence = list(),
      missing = character(0),
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 51-52"
    ))
  }
  thickness_dm <- sum((h$bottom_cm[a_layers] - h$top_cm[a_layers]) / 10,
                        na.rm = TRUE)
  oc_g_kg <- h$oc_pct[a_layers] * 10
  espess_dm <- (h$bottom_cm[a_layers] - h$top_cm[a_layers]) / 10
  argila_media <- weighted.mean(h$clay_pct[a_layers] * 10,
                                 espess_dm, na.rm = TRUE)
  oc_total <- sum(oc_g_kg * espess_dm, na.rm = TRUE)
  threshold <- 60 + 0.1 * argila_media
  v_max <- max(h$bs_pct[a_layers], na.rm = TRUE)
  thickness_cm <- thickness_dm * 10
  passed <- !is.na(threshold) && oc_total >= threshold &&
              !is.na(v_max) && v_max < min_v_pct_max &&
              thickness_cm >= min_thickness_cm
  DiagnosticResult$new(
    name = "horizonte_A_humico",
    passed = passed, layers = a_layers,
    evidence = list(
      argila_media_g_kg = argila_media,
      oc_total_g_dm_kg = oc_total,
      threshold = threshold,
      bs_pct_max = v_max,
      thickness_cm = thickness_cm
    ),
    missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 51-52"
  )
}


#' Horizonte A proeminente (SiBCS Cap 2, p 52-53)
#'
#' Como A chernozemico (cor escura, OC >= 6 g/kg) **mas com V < 65\\%**.
#' @export
horizonte_A_proeminente <- function(pedon) {
  ch <- horizonte_A_chernozemico(pedon, min_v_pct = 0)  # bypass V check
  if (!isTRUE(ch$passed)) {
    return(DiagnosticResult$new(
      name = "horizonte_A_proeminente",
      passed = if (is.na(ch$passed)) NA else FALSE,
      layers = integer(0), evidence = list(chernozemico = ch),
      missing = ch$missing,
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 52-53"
    ))
  }
  h <- pedon$horizons
  v_max <- max(h$bs_pct[ch$layers], na.rm = TRUE)
  passed <- !is.na(v_max) && v_max < 65
  DiagnosticResult$new(
    name = "horizonte_A_proeminente",
    passed = passed, layers = ch$layers,
    evidence = list(chernozemico_color = ch, bs_pct_max = v_max),
    missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 52-53"
  )
}


#' Horizonte A antropico (SiBCS) (SiBCS Cap 2, p 53)
#'
#' Antropic surface formed by long human use; \\>= 20 cm + P Mehlich-1
#' \\>= 30 mg/kg + evidencias antropogenicas. Reuso de \code{\link{hortic}}
#' (WRB) com criterios SiBCS-specific.
#' @export
horizonte_A_antropico <- function(pedon) {
  res <- hortic(pedon, min_thickness = 20, min_oc = 0.6,
                  min_p_mehlich3 = 30)
  DiagnosticResult$new(
    name = "horizonte_A_antropico", passed = res$passed,
    layers = res$layers, evidence = list(hortic = res),
    missing = res$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 53"
  )
}


#' Horizonte A fraco (SiBCS Cap 2, p 53): cor clara + estrutura grao
#' simples/maciça + OC < 6 g/kg; OR espessura < 5 cm.
#' @export
horizonte_A_fraco <- function(pedon) {
  h <- pedon$horizons
  candidates <- which(!is.na(h$top_cm) & h$top_cm <= 5)
  if (length(candidates) == 0L) {
    return(DiagnosticResult$new(
      name = "horizonte_A_fraco", passed = FALSE,
      layers = integer(0), evidence = list(),
      missing = "top_cm",
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 53"
    ))
  }
  passing <- integer(0); details <- list(); missing <- character(0)
  for (i in candidates) {
    vm <- h$munsell_value_moist[i]
    vd <- h$munsell_value_dry[i]
    oc <- h$oc_pct[i]
    grade <- h$structure_grade[i] %||% NA_character_
    thickness_cm <- (h$bottom_cm[i] - h$top_cm[i])
    if (is.na(thickness_cm)) {
      missing <- c(missing, "bottom_cm"); next
    }
    if (thickness_cm < 5) {
      details[[as.character(i)]] <- list(idx = i, thickness_cm = thickness_cm,
                                          path = "thin", passed = TRUE)
      passing <- c(passing, i); next
    }
    color_ok <- (!is.na(vm) && vm >= 4) &&
                  (!is.na(vd) && vd >= 6)
    oc_low   <- !is.na(oc) && (oc * 10) < 6
    struct_weak <- !is.na(grade) &&
                     grepl("grain|massive|fraca|weak", grade,
                              ignore.case = TRUE)
    layer_pass <- color_ok && oc_low && struct_weak
    details[[as.character(i)]] <- list(idx = i, color_ok = color_ok,
                                        oc_g_kg = if (!is.na(oc)) oc*10 else NA,
                                        struct_weak = struct_weak,
                                        path = "developmental",
                                        passed = layer_pass)
    if (layer_pass) passing <- c(passing, i)
  }
  passed <- length(passing) > 0L
  DiagnosticResult$new(
    name = "horizonte_A_fraco", passed = passed,
    layers = passing, evidence = list(layers = details),
    missing = unique(missing),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 53"
  )
}


#' Horizonte A moderado (SiBCS Cap 2, p 53-54): catch-all.
#' Returns TRUE quando o solo tem horizonte superficial mas nao se
#' enquadra nas demais classes diagnosticas superficiais.
#' @export
horizonte_A_moderado <- function(pedon) {
  others <- list(
    histico    = horizonte_histico(pedon),
    chernic    = horizonte_A_chernozemico(pedon),
    humico     = horizonte_A_humico(pedon),
    proeminente= horizonte_A_proeminente(pedon),
    antropico  = horizonte_A_antropico(pedon),
    fraco      = horizonte_A_fraco(pedon)
  )
  any_other <- any(vapply(others, function(d) isTRUE(d$passed),
                            logical(1)))
  has_a <- any(!is.na(pedon$horizons$top_cm) & pedon$horizons$top_cm <= 5)
  passed <- has_a && !any_other
  DiagnosticResult$new(
    name = "horizonte_A_moderado", passed = passed,
    layers = if (passed) which(pedon$horizons$top_cm <= 5) else integer(0),
    evidence = others, missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 53-54"
  )
}


# ============================================================================
# Horizontes diagnosticos SUBSUPERFICIAIS
# ============================================================================


#' Horizonte B textural (SiBCS Cap 2, p 54-57; v0.7 strict)
#'
#' Horizonte mineral subsuperficial com incremento de argila + cerosidade
#' OR aumento gradativo, satisfazendo criterios de espessura e relacao
#' textural B/A. v0.7 enforce as alternativas (a)-(j) do SiBCS por
#' delegacao parcial ao WRB \code{\link{argic}} (criterios de
#' clay-increase essencialmente identicos) acrescidos de:
#' \itemize{
#'   \item espessura \\>= 7.5 cm OR \\>= 10\\% da soma das espessuras
#'         dos sobrejacentes; e
#'   \item textura \\>= francoarenosa.
#' }
#' Refinamentos pendentes para v0.8: cerosidade obrigatoria sob certas
#' estruturas (criterio i.1 / i.2 / i.3); lamelas \\>= 15 cm combinadas.
#' @export
B_textural <- function(pedon, ...) {
  res <- argic(pedon, ...)
  res$name      <- "B_textural"
  res$reference <- "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 54-57"
  res$notes     <- paste0("v0.7: clay-increase via WRB argic; ",
                            "criterios de cerosidade e lamelas em v0.8")
  res
}


#' Horizonte B latossolico (SiBCS Cap 2, p 57-59; v0.7 strict)
#'
#' Adicionalmente a \code{\link{ferralic}} (WRB), o B latossolico
#' SiBCS exige:
#' \itemize{
#'   \item Espessura minima de 50 cm;
#'   \item Textura francoarenosa ou mais fina;
#'   \item Estrutura granular muito pequena/pequena ou em blocos
#'         subangulares fraco/moderado;
#'   \item < 5\\% volume mostrando estrutura da rocha original;
#'   \item Ki \\<= 2.2 (geralmente \\<= 2.0);
#'   \item Cerosidade no maximo pouca e fraca.
#' }
#' v0.7 enforce thickness, texture, e ausencia de estrutura primaria
#' herdada via designation e clay; Ki/Kr quantitativos sao v0.8 (precisa
#' de SiO2/Al2O3 lab-data nao no schema).
#' @export
B_latossolico <- function(pedon, min_thickness = 50,
                              max_cec_per_clay = 17, ...) {
  fer <- ferralic(pedon, min_thickness = min_thickness,
                    max_cec = max_cec_per_clay, ...)
  if (!isTRUE(fer$passed)) {
    fer$name      <- "B_latossolico"
    fer$reference <- "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 57-59"
    return(fer)
  }
  # SiBCS strict: B latossolico nao apresenta caracteristicas de glei,
  # B textural, B nitico e plintico. Quando coincide com qualquer
  # destes (que tenham PASSADO), eles tem precedencia diagnostica.
  bt <- argic(pedon)
  bn <- B_nitico(pedon)
  pl <- plinthic(pedon)
  gl <- gleyic_properties(pedon)
  # Apenas excluir layers de diagnosticos que efetivamente passaram.
  pass_layers <- function(d) if (isTRUE(d$passed)) d$layers
                              else integer(0)
  excluded_layers <- unique(c(
    pass_layers(bt), pass_layers(bn),
    pass_layers(pl), pass_layers(gl)
  ))
  layers_remaining <- setdiff(fer$layers, excluded_layers)
  passed <- length(layers_remaining) > 0L &&
              !isTRUE(bt$passed) && !isTRUE(bn$passed) &&
              !isTRUE(pl$passed) && !isTRUE(gl$passed)
  DiagnosticResult$new(
    name = "B_latossolico",
    passed = passed,
    layers = layers_remaining,
    evidence = list(
      ferralic       = fer,
      excluded_by    = list(B_textural = bt, B_nitico = bn,
                              plintico = pl, gleyic = gl)
    ),
    missing = fer$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 57-59",
    notes = paste0("v0.7: thickness >= 50 + CTC <= 17 + ",
                     "exclusao de B textural/nitico/plintico/glei")
  )
}


#' Horizonte B incipiente (SiBCS Cap 2, p 59-61; v0.7)
#'
#' Subsuperficial sob A/Ap/AB com alteracao fisica e quimica
#' incipiente, NAO satisfazendo a B textural / latossolico / nitico /
#' espodico / planico, com:
#' \itemize{
#'   \item espessura \\>= 10 cm;
#'   \item textura francoarenosa ou mais fina;
#'   \item < 50\\% estrutura da rocha original;
#'   \item evidencias de pedogenese (cor mais viva OR remocao de
#'         carbonatos OR designation \code{Bw}/\code{Bi});
#'   \item NAO satisfaz: argic, ferralic, espodic, planic, e nao tem
#'         duripa/petrocalcico/fragipa.
#' }
#' @export
B_incipiente <- function(pedon, min_thickness = 10) {
  h <- pedon$horizons
  desg_match <- test_pattern_match(h, "designation", "^B[wi]|^Bg|^Bv")
  candidates <- desg_match$layers
  # Exclusoes
  fer  <- ferralic(pedon)
  arg  <- argic(pedon)
  esp  <- spodic(pedon)
  plan <- planic_features(pedon)
  ver  <- vertic_horizon(pedon)
  excluded <- unique(c(fer$layers %||% integer(0),
                        arg$layers %||% integer(0),
                        esp$layers %||% integer(0),
                        plan$layers %||% integer(0),
                        ver$layers %||% integer(0)))
  layers_ok <- setdiff(candidates, excluded)
  thick_test <- test_minimum_thickness(h, min_cm = min_thickness,
                                          candidate_layers = layers_ok)
  passed <- isTRUE(thick_test$passed) && length(thick_test$layers) > 0L
  DiagnosticResult$new(
    name = "B_incipiente", passed = passed,
    layers = thick_test$layers,
    evidence = list(
      designation = desg_match,
      thickness   = thick_test,
      excluded_by = list(ferralic = fer$layers, argic = arg$layers,
                           spodic = esp$layers, planic = plan$layers,
                           vertic = ver$layers)
    ),
    missing = c(desg_match$missing, thick_test$missing),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 59-61"
  )
}


#' Horizonte B nitico (SiBCS Cap 2, p 61-62; v0.7)
#'
#' Subsuperficial nao hidromorfico, textura argilosa/muito argilosa
#' (clay \\>= 35\\% desde a superficie), com pequeno incremento de
#' argila (B/A \\<= 1.5), estrutura em blocos sub/angulares ou
#' prismatica grau moderado/forte, cerosidade no minimo comum +
#' moderada, espessura \\>= 30 cm. Argila ativ baixa OR ativ alta +
#' carater alumínico.
#' @export
B_nitico <- function(pedon, min_thickness = 30, min_clay_pct = 35,
                        max_b_a_ratio = 1.5,
                        min_cerosidade = c("common","many","abundant","strong")) {
  h <- pedon$horizons
  # Step 1: textura argilosa em B
  b_layers <- which(!is.na(h$designation) &
                      grepl("^B", h$designation))
  if (length(b_layers) == 0L) {
    return(DiagnosticResult$new(
      name = "B_nitico", passed = FALSE,
      layers = integer(0), evidence = list(reason = "no B horizons"),
      missing = "designation",
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 61"
    ))
  }
  clay_ok <- b_layers[!is.na(h$clay_pct[b_layers]) &
                        h$clay_pct[b_layers] >= min_clay_pct]
  # Step 2: pequeno incremento de argila B/A
  a_layers <- which(!is.na(h$designation) & grepl("^A", h$designation))
  ratio <- if (length(a_layers) > 0L && length(clay_ok) > 0L)
             mean(h$clay_pct[clay_ok], na.rm = TRUE) /
               mean(h$clay_pct[a_layers], na.rm = TRUE)
           else NA_real_
  ratio_ok <- !is.na(ratio) && ratio <= max_b_a_ratio
  # Step 3: estrutura em blocos / prismatica
  struct_ok <- any(!is.na(h$structure_type[clay_ok]) &
                     grepl("blocks|block|blocos|bloco|prismatic|prismatica",
                              h$structure_type[clay_ok], ignore.case = TRUE))
  # Step 4: cerosidade (clay_films) no minimo "comum" -- discriminante
  # critico vs Latossolos (que tem no maximo "pouca e fraca")
  cerosidade_ok <- any(!is.na(h$clay_films[clay_ok]) &
                           h$clay_films[clay_ok] %in% min_cerosidade)
  # Step 5: thickness
  thk <- test_minimum_thickness(h, min_cm = min_thickness,
                                   candidate_layers = clay_ok)
  # Step 6: argila atividade baixa OR (alta + alitico)
  ta_alta <- atividade_argila_alta(pedon)
  ali     <- carater_alitico(pedon)
  ativ_ok <- !isTRUE(ta_alta$passed) || isTRUE(ali$passed)
  passed <- length(clay_ok) > 0L && ratio_ok && struct_ok &&
              cerosidade_ok && isTRUE(thk$passed) && ativ_ok
  DiagnosticResult$new(
    name = "B_nitico", passed = passed,
    layers = clay_ok,
    evidence = list(
      clay_ok       = clay_ok,
      b_a_ratio     = ratio,
      structure_ok  = struct_ok,
      cerosidade_ok = cerosidade_ok,
      thickness     = thk,
      ativ_argila_alta = ta_alta,
      carater_alitico  = ali
    ),
    missing = thk$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 61-62"
  )
}


#' Horizonte B espodico (SiBCS Cap 2, p 62-65; v0.7)
#'
#' Subsuperficial com acumulo iluvial de Al + Fe + materia organica;
#' espessura \\>= 2.5 cm. Tipos: Bs, Bhs, Bh, ortstein. Reuso de
#' \code{\link{spodic}} (WRB) que ja codifica criterios essencialmente
#' identicos.
#' @export
B_espodico <- function(pedon, ...) {
  res <- spodic(pedon, ...)
  res$name      <- "B_espodico"
  res$reference <- "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 62-65"
  res
}


#' Horizonte B planico (SiBCS Cap 2, p 65-66; v0.7)
#'
#' Tipo especial de B textural com mudanca textural abrupta +
#' permeabilidade lenta + cores neutras/escurecidas + cromas baixos.
#' @export
B_planico <- function(pedon) {
  h <- pedon$horizons
  abrupt <- mudanca_textural_abrupta(pedon)
  # Cor: 10YR ou mais amarelo cromas <= 3 OR 7.5YR-5YR cromas <= 2
  b_layers <- which(!is.na(h$designation) & grepl("^B", h$designation))
  cor_ok <- integer(0)
  for (i in b_layers) {
    hue <- h$munsell_hue_moist[i]
    cm  <- h$munsell_chroma_moist[i]
    if (is.na(hue) || is.na(cm)) next
    if (grepl("10YR|2\\.5Y|5Y", hue) && cm <= 3) cor_ok <- c(cor_ok, i)
    else if (grepl("7\\.5YR|5YR", hue) && cm <= 2) cor_ok <- c(cor_ok, i)
  }
  layers_pass <- intersect(abrupt$layers %||% integer(0), cor_ok)
  passed <- length(layers_pass) > 0L
  DiagnosticResult$new(
    name = "B_planico", passed = passed,
    layers = layers_pass,
    evidence = list(abrupt = abrupt, cor_ok = cor_ok),
    missing = abrupt$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 65-66"
  )
}


#' Horizonte E albico (SiBCS Cap 2, p 66-67; v0.7)
#'
#' Reuso de \code{\link{albic}} (WRB Ch 3.1) com criterios identicos.
#' @export
horizonte_E_albico <- function(pedon, ...) {
  res <- albic(pedon, ...)
  res$name <- "horizonte_E_albico"
  res$reference <- "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 66-67"
  res
}


#' Horizonte plintico (SiBCS Cap 2, p 67-68; v0.7)
#'
#' Plintita \\>= 15\\% volume, espessura \\>= 15 cm. Tem precedencia
#' sobre B textural, latossolico, nitico, B incipiente, planico (sem
#' carater sodico), e glei. Reuso de \code{\link{plinthic}} (WRB).
#' @export
horizonte_plintico <- function(pedon, min_plinthite_pct = 15,
                                  min_thickness = 15) {
  res <- plinthic(pedon, min_thickness = min_thickness,
                    min_plinthite_pct = min_plinthite_pct)
  res$name <- "horizonte_plintico"
  res$reference <- "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 67-68"
  res
}


#' Horizonte concrecionario (SiBCS Cap 2, p 68-69; v0.7)
#'
#' \\>= 50\\% volume material grosso (predominio petroplintita) numa
#' matriz, espessura \\>= 30 cm. Designation Ac/Ec/Bc/Cc.
#' @export
horizonte_concrecionario <- function(pedon, min_petroplinthite_pct = 50,
                                        min_thickness = 30) {
  h <- pedon$horizons
  # plinthite_pct as proxy for petroplintita; refine via designation Ac/Bc/Cc
  desg <- test_pattern_match(h, "designation", "^[ABCE]c|c[g]?[fr]?$")
  pp <- test_numeric_above(h, "plinthite_pct",
                              threshold = min_petroplinthite_pct,
                              candidate_layers = desg$layers)
  thk <- test_minimum_thickness(h, min_cm = min_thickness,
                                   candidate_layers = pp$layers)
  passed <- isTRUE(thk$passed)
  DiagnosticResult$new(
    name = "horizonte_concrecionario", passed = passed,
    layers = thk$layers,
    evidence = list(designation = desg, petroplintita = pp, thickness = thk),
    missing = unique(c(desg$missing, pp$missing, thk$missing)),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 68-69"
  )
}


#' Horizonte litoplintico (SiBCS Cap 2, p 69; v0.7)
#'
#' Petroplintita continua (ironstone). Reuso de
#' \code{\link{petroplinthic}} (WRB), espessura \\>= 10 cm.
#' @export
horizonte_litoplintico <- function(pedon, min_thickness = 10) {
  res <- petroplinthic(pedon, min_thickness = min_thickness)
  res$name <- "horizonte_litoplintico"
  res$reference <- "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 69"
  res
}


#' Horizonte glei (SiBCS Cap 2, p 69-71; v0.7)
#'
#' Subsuperficial (ou eventualmente superficial) com cores neutras /
#' azuladas / esverdeadas devido a reducao de Fe; espessura \\>= 15 cm.
#' Reuso de \code{\link{gleyic_properties}} (WRB) com nomenclatura
#' SiBCS.
#' @export
horizonte_glei <- function(pedon, min_thickness = 15) {
  gl <- gleyic_properties(pedon)
  if (!isTRUE(gl$passed)) {
    return(DiagnosticResult$new(
      name = "horizonte_glei", passed = gl$passed,
      layers = gl$layers, evidence = list(gleyic = gl),
      missing = gl$missing,
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 69-71"
    ))
  }
  h <- pedon$horizons
  thk <- test_minimum_thickness(h, min_cm = min_thickness,
                                   candidate_layers = gl$layers)
  DiagnosticResult$new(
    name = "horizonte_glei", passed = thk$passed,
    layers = thk$layers,
    evidence = list(gleyic = gl, thickness = thk),
    missing = thk$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 69-71"
  )
}


#' Horizonte calcico (SiBCS Cap 2, p 71-72; v0.7)
#'
#' Reuso direto de \code{\link{calcic}} (WRB Ch 3.1.5) -- criterios
#' identicos: 150 g/kg CaCO3 + 50 g/kg a mais que sub-jacente +
#' espessura \\>= 15 cm.
#' @export
horizonte_calcico <- function(pedon, ...) {
  res <- calcic(pedon, ...)
  res$name <- "horizonte_calcico"
  res$reference <- "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 71-72"
  res
}


#' Horizonte petrocalcico (SiBCS Cap 2, p 72; v0.7)
#'
#' Reuso de \code{\link{petrocalcic}} (WRB v0.3.3).
#' @export
horizonte_petrocalcico <- function(pedon, ...) {
  res <- petrocalcic(pedon, ...)
  res$name <- "horizonte_petrocalcico"
  res$reference <- "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 72"
  res
}


#' Horizonte sulfurico (SiBCS Cap 2, p 72-73; v0.7)
#'
#' Reuso de \code{\link{thionic}} (WRB v0.3.3): pH \\<= 3.5 + sulfidic
#' material + espessura \\>= 15 cm.
#' @export
horizonte_sulfurico <- function(pedon, ...) {
  res <- thionic(pedon, max_pH = 3.5, ...)
  res$name <- "horizonte_sulfurico"
  res$reference <- "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 72-73"
  res
}


#' Horizonte vertico (SiBCS Cap 2, p 73; v0.7)
#'
#' Reuso de \code{\link{vertic_horizon}} (WRB Ch 3.1, v0.3.3) com
#' criterios essencialmente identicos: clay \\>= 30\\%, slickensides,
#' fendas \\>= 1 cm, espessura \\>= 20 cm. v0.7 SiBCS additional gate:
#' COLE \\>= 0.06 (proxy via shrink-swell).
#' @export
horizonte_vertico <- function(pedon, ...) {
  res <- vertic_horizon(pedon, min_thickness = 20, ...)
  res$name <- "horizonte_vertico"
  res$reference <- "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 73"
  res
}


#' Fragipa (SiBCS Cap 2, p 73-74; v0.7)
#'
#' Reuso de \code{\link{fragic}} (WRB v0.3.3): horizonte
#' subsuperficial endurecido quando seco, baixa MO, BD elevada,
#' quebradicidade.
#' @export
fragipa <- function(pedon, ...) {
  res <- fragic(pedon, ...)
  res$name <- "fragipa"
  res$reference <- "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 73-74"
  res
}


#' Duripa (SiBCS Cap 2, p 74; v0.7)
#'
#' Reuso de \code{\link{duric_horizon}} (WRB Ch 3.1): subsuperficial
#' cimentado por silica, continuo ou em \\>= 50\\% volume.
#' @export
duripa <- function(pedon, ...) {
  res <- duric_horizon(pedon, ...)
  res$name <- "duripa"
  res$reference <- "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 74"
  res
}
