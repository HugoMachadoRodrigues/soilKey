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


#' Horizonte histico (SiBCS Cap 2, p 49-50)
#'
#' Horizonte O ou H de coloracao preta/cinza muito escura/brunada,
#' \\>= 80 g/kg (8\%) C organico, com:
#' \itemize{
#'   \item espessura \\>= 20 cm; OR
#'   \item espessura \\>= 40 cm se \\>= 75\% volume tecido vegetal; OR
#'   \item espessura \\>= 10 cm sobre contato litico/fragmentario OR
#'         camada com \\>= 90\% material > 2 mm.
#' }
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_oc_g_kg Numeric threshold or option (see Details).
#' @noRd
horizonte_histico <- function(pedon, min_oc_g_kg = 80) {
  h <- pedon$horizons
  # Convert g/kg threshold to %.
  min_oc_pct <- min_oc_g_kg / 10
  # v0.9.10: candidate layers must EITHER carry an organic-horizon
  # designation (O, H, Op, Oa, Oe, Oi, Hr, Hd) OR have OC well above
  # the mineral-soil ceiling (>= 12 % is the threshold for organic-
  # dominant material per SiBCS Cap 1). Pre-v0.9.10 the test only
  # checked oc_pct >= 8 %, which falsely matched mineral A horizons of
  # high-OC andic profiles (canonical Andosol with Ah of 8 % OC was
  # being routed into Organossolos).
  desg <- h$designation
  is_organic_designation <- !is.na(desg) &
                              grepl("^[OH][a-z]?", desg)
  is_dominantly_organic  <- !is.na(h$oc_pct) & h$oc_pct >= 12
  candidates <- which((is_organic_designation | is_dominantly_organic) &
                        !is.na(h$oc_pct) & h$oc_pct >= min_oc_pct &
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
#' alta saturacao por bases (V \\>= 65\%), OC \\>= 6 g/kg, estrutura
#' desenvolvida e espessura conforme criterio.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_oc_g_kg Numeric threshold or option (see Details).
#' @param min_v_pct Numeric threshold or option (see Details).
#' @param max_value_moist Numeric threshold or option (see Details).
#' @param max_chroma_moist Numeric threshold or option (see Details).
#' @param max_value_dry Numeric threshold or option (see Details).
#' @param min_thickness_cm Numeric threshold or option (see Details).
#' @noRd
horizonte_A_chernozemico <- function(pedon,
                                        min_oc_g_kg = 6,
                                        min_v_pct   = 65,
                                        max_value_moist = 3,
                                        max_chroma_moist = 3,
                                        max_value_dry   = 5,
                                        min_thickness_cm = 18) {
  h <- pedon$horizons
  # v0.9.107: the chernic A may be split across stacked A horizons (A1/A2/...);
  # take the CONTIGUOUS run of A-master horizons from the surface so the
  # thickness test aggregates the whole chernic A, not just the topmost slice.
  ord <- order(h$top_cm, na.last = NA)
  candidates <- integer(0); prev_bot <- 0
  for (i in ord) {
    if (is.na(h$top_cm[i])) next
    is_A <- grepl("^[0-9]*A", h$designation[i] %||% "")
    if (length(candidates) == 0L) {
      if (h$top_cm[i] <= 5 && is_A) {
        candidates <- i
        prev_bot <- h$bottom_cm[i] %||% h$top_cm[i]
      }
    } else if (is_A && h$top_cm[i] <= prev_bot + 1) {
      candidates <- c(candidates, i)
      prev_bot <- h$bottom_cm[i] %||% prev_bot
    } else break
  }
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
    # v0.9.136: SiBCS Cap 2 p.50 (criterion a) requires structure of grade
    # "predominantemente moderado ou forte". The prior test only excluded
    # massive/grain/loose, so a WEAK grade wrongly passed. refine-when-present:
    # a RECORDED grade must read moderate/strong; absent grade (NA) leaves the
    # result byte-identical to the pre-v0.9.136 behaviour.
    struct_ok <- is.na(grade) ||
                   grepl("moder|strong|forte", grade, ignore.case = TRUE)
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
  # v0.9.136: SiBCS Cap 2 p.51 (criterion e) makes the minimum thickness
  # conditional on solum depth and lithic contact, not a flat 18 cm:
  #   - >= 10 cm if the A sits directly over a lithic / lithic-fragmentary
  #     contact (no B horizon);
  #   - >= 18 cm AND > 1/3 of the solum (A+B), when the solum < 75 cm;
  #   - >= 25 cm, when the solum >= 75 cm.
  # min_thickness_cm (default 18) is retained as the fallback used only when
  # the solum depth cannot be established (no designation / bottom data).
  desig_all <- h$designation %||% rep(NA_character_, nrow(h))
  is_B    <- !is.na(desig_all) & grepl("^[0-9]*B", desig_all)
  is_rock <- !is.na(desig_all) & grepl("^[0-9]*(R|Cr|Cd)", desig_all)
  a_bottom <- if (length(passing) > 0L)
                suppressWarnings(max(h$bottom_cm[passing], na.rm = TRUE))
              else NA_real_
  if (any(is_B)) {
    solum_cm <- suppressWarnings(max(h$bottom_cm[is_B], na.rm = TRUE))
  } else {
    nonrock <- which(!is_rock & !is.na(h$bottom_cm))
    solum_cm <- if (length(nonrock))
                  suppressWarnings(max(h$bottom_cm[nonrock], na.rm = TRUE))
                else NA_real_
  }
  over_rock <- !any(is_B) && any(is_rock & !is.na(h$top_cm) &
                                   !is.na(a_bottom) &
                                   h$top_cm <= a_bottom + 1)
  if (length(passing) == 0L) {
    thick_ok <- FALSE
  } else if (over_rock) {
    thick_ok <- thickness >= 10
  } else if (is.finite(solum_cm) && solum_cm >= 75) {
    thick_ok <- thickness >= 25
  } else if (is.finite(solum_cm)) {
    thick_ok <- thickness >= 18 && thickness > solum_cm / 3
  } else {
    thick_ok <- thickness >= min_thickness_cm
  }
  passed <- length(passing) > 0L && thick_ok
  DiagnosticResult$new(
    name = "horizonte_A_chernozemico",
    passed = passed, layers = passing,
    evidence = list(layers = details, thickness_cm = thickness,
                     solum_cm = solum_cm, over_rock = over_rock),
    missing = unique(missing),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 50-51"
  )
}


#' Horizonte A humico (SiBCS Cap 2, p 51-52)
#'
#' Horizonte A com cor moderadamente escura (value/chroma <= 4),
#' V < 65\%, e C organico total >= 60 + 0.1 * argila_media (g/kg).
#' Espessura \\>= a do A chernozemico.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_v_pct_max Numeric threshold or option (see Details).
#' @param min_thickness_cm Numeric threshold or option (see Details).
#' @noRd
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
  # v0.9.27: guard against all-NA bs_pct returning -Inf with warning.
  bs_vals <- h$bs_pct[a_layers]
  v_max <- if (any(!is.na(bs_vals))) max(bs_vals, na.rm = TRUE) else NA_real_
  thickness_cm <- thickness_dm * 10
  # v0.9.136: SiBCS Cap 2 p.51 opens the A humico definition with "valor e
  # croma (cor do solo umido) iguais ou inferiores a 4". The prior code never
  # checked colour, so an A meeting only the CO inequation could pass even if
  # light-coloured. refine-when-present: an A sub-horizon carrying a RECORDED
  # value/chroma (moist) > 4 disqualifies; absent colour (all NA) leaves the
  # result byte-identical to the pre-v0.9.136 behaviour.
  vm_a <- h$munsell_value_moist[a_layers]
  cm_a <- h$munsell_chroma_moist[a_layers]
  color_ok <- all(is.na(vm_a) | vm_a <= 4) && all(is.na(cm_a) | cm_a <= 4)
  passed <- !is.na(threshold) && oc_total >= threshold &&
              !is.na(v_max) && v_max < min_v_pct_max &&
              thickness_cm >= min_thickness_cm && color_ok
  DiagnosticResult$new(
    name = "horizonte_A_humico",
    passed = passed, layers = a_layers,
    evidence = list(
      argila_media_g_kg = argila_media,
      oc_total_g_dm_kg = oc_total,
      threshold = threshold,
      bs_pct_max = v_max,
      color_ok = color_ok,
      thickness_cm = thickness_cm
    ),
    missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 51-52"
  )
}


#' Horizonte A proeminente (SiBCS Cap 2, p 52-53)
#'
#' Como A chernozemico (cor escura, OC >= 6 g/kg) **mas com V < 65\%**.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
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
  # v0.9.27: guard against all-NA bs_pct returning -Inf with warning.
  bs_vals <- h$bs_pct[ch$layers]
  v_max <- if (any(!is.na(bs_vals))) max(bs_vals, na.rm = TRUE) else NA_real_
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
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
horizonte_A_antropico <- function(pedon) {
  res <- hortic(pedon, min_thickness = 20, min_oc = 0.6,
                  min_p_mehlich3 = 30)
  # SiBCS Cap 2 p.53: the espessura >= 20 cm "e" P-Mehlich1 >= 30 mg/kg
  # requirements are an AND (verbatim connector "e"), which hortic already
  # enforces -- so no logic change there. But the manual ALSO makes the
  # presence of human artefacts (ceramica / litico / ossos / conchas /
  # carvao-cinzas) "de presenca OBRIGATORIA". The prior wrapper omitted that
  # gate, so any P-rich thick surface keyed as antropico without artefacts.
  # v0.9.136 refine-when-present: when artefacts_pct is RECORDED and absent
  # (all zero) in the diagnostic layers, the horizon cannot be antropico;
  # when the column is absent / NA we cannot disprove it and defer to hortic
  # (byte-identical on data lacking the field, e.g. every benchmark pedon).
  h <- pedon$horizons
  art <- h[["artefacts_pct"]]
  has_art_data <- !is.null(art) && any(!is.na(art))
  if (has_art_data) {
    lyr <- if (length(res$layers)) res$layers else which(!is.na(art))
    artefacts_present <- any(!is.na(art[lyr]) & art[lyr] > 0)
    passed <- if (!artefacts_present) FALSE else res$passed
  } else {
    passed <- res$passed
  }
  DiagnosticResult$new(
    name = "horizonte_A_antropico", passed = passed,
    layers = res$layers,
    evidence = list(hortic = res, artefacts_recorded = has_art_data),
    missing = res$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 53"
  )
}


#' Horizonte A fraco (SiBCS Cap 2, p 53): cor clara + estrutura grao
#' simples/macica + OC < 6 g/kg; OR espessura < 5 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
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
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
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
#'   \item espessura \\>= 7.5 cm OR \\>= 10\% da soma das espessuras
#'         dos sobrejacentes; e
#'   \item textura \\>= francoarenosa.
#' }
#' Refinamentos pendentes para v0.8: cerosidade obrigatoria sob certas
#' estruturas (criterio i.1 / i.2 / i.3); lamelas \\>= 15 cm combinadas.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param ... Reserved for future arguments.
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
B_textural <- function(pedon, ...) {
  res <- argic(pedon, ...)
  # v0.9.138: UNION the verbatim SiBCS Cap 2 p.56 item (h) relacao-textural
  # ratio (test_ratio_textural_sibcs) with the WRB argic clay-increase. The two
  # mostly coincide -- (h) is a subset of argic EXCEPT for very sandy A horizons
  # (clay < ~7.5%), where the ratio (>1.80) is a smaller absolute jump than
  # argic's +6 pp. The union therefore only ADDS sandy-A B-textural cases argic
  # misses; it can never remove an argic pass. Other paths -- (f) E-horizon,
  # (g) abrupt change, (i) cerosidade, (j) lithologic discontinuity -- remain
  # delegated/deferred (cerosidade morphology is data-sparse).
  h_ratio <- test_ratio_textural_sibcs(pedon$horizons)
  if (isTRUE(h_ratio$passed)) {
    res$layers <- union(res$layers %||% integer(0), h_ratio$layers)
    res$passed <- length(res$layers) > 0L
    res$evidence <- c(res$evidence %||% list(),
                       list(relacao_textural_sibcs = h_ratio))
  }
  res$name      <- "B_textural"
  res$reference <- "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 54-57"
  res$notes     <- paste0("v0.9.138: clay-increase via WRB argic UNION SiBCS ",
                            "relacao-textural (h); cerosidade (i)/lamelas em v0.8")
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
#'   \item < 5\% volume mostrando estrutura da rocha original;
#'   \item Ki \\<= 2.2 (geralmente \\<= 2.0);
#'   \item Cerosidade no maximo pouca e fraca.
#' }
#' v0.7 enforce thickness, texture, e ausencia de estrutura primaria
#' herdada via designation e clay; Ki/Kr quantitativos sao v0.8 (precisa
#' de SiO2/Al2O3 lab-data nao no schema).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Numeric threshold or option (see Details).
#' @param max_cec_per_clay Numeric threshold or option (see Details).
#'   Defaults to \code{NULL} (engine-aware): 17 in soilkey engine
#'   (the SiBCS-loose threshold, slightly more permissive than
#'   strict WRB ferralic 16) or 20 in aqp engine (v0.9.68 regional
#'   tolerance for Embrapa lab methodology offset).
#' @param engine One of \code{"soilkey"} (default) or \code{"aqp"};
#'   \code{NULL} reads \code{getOption("soilKey.diagnostic_engine")}.
#'   Forwarded to \code{\link{ferralic}}.
#' @param ... Reserved for future arguments.
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
B_latossolico <- function(pedon, min_thickness = 50,
                              max_cec_per_clay = NULL,
                              engine = NULL, ...) {
  if (is.null(engine))
    engine <- getOption("soilKey.diagnostic_engine", "soilkey")
  engine <- match.arg(engine, c("soilkey", "aqp"))
  if (is.null(max_cec_per_clay))
    max_cec_per_clay <- if (engine == "aqp") 20 else 17
  fer <- ferralic(pedon, min_thickness = min_thickness,
                    max_cec = max_cec_per_clay, engine = engine, ...)
  if (!isTRUE(fer$passed)) {
    fer$name      <- "B_latossolico"
    fer$reference <- "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 57-59"
    return(fer)
  }
  # v0.9.61 -- precedencia revisada conforme SiBCS Cap 18.
  #
  # Antes (v0.7): excluia layers que tambem passassem argic / B_nitico
  # / plintico / gleyic. Para Latossolos com B horizon que casualmente
  # tinha clay increase marginal (~6 pp ou >= 1.4x), o argic test
  # passava e B_latossolico falhava -- caindo na pedra Argissolos
  # catch-all do key.yaml. Resultado empirico: 24 / 114 Latossolos do
  # BDsolos RJ.csv re-classificados erroneamente como Argissolos
  # (v0.9.60 RJ benchmark, 2026-05-06).
  #
  # SiBCS Cap 18 e explicito: um Latossolo pode ter B textural fraco
  # (gradacional, clay films pouca / fraca) -- desde que as features
  # latossolicas dominem (CTC argila <= 17 cmolc/kg, ferralic, thickness
  # >= 50). Nesse caso a precedencia eh do B latossolico, NAO do B
  # textural. Argic forte (clay films comuns + sharp clay increase) eh
  # outra historia, mas o teste argic atual nao distingue forca, so
  # threshold-pass.
  #
  # Plintico e gleyic continuam excludentes (sao diagnostic horizons
  # que definem ordens distintas -- Plintossolos e Gleissolos -- e a
  # ordem do key.yaml ja os coloca antes de Latossolos). B nitico
  # idem (Nitossolos).
  pl <- plinthic(pedon)
  gl <- gleyic_properties(pedon)
  bn <- B_nitico(pedon)
  bt <- argic(pedon)
  # v0.9.61 -- argic exclui APENAS quando ha clay-films comuns/
  # abundantes em layers do B horizon. Per SiBCS Cap 18: cerosidade
  # "ausente / pouca / fraca" = Latossolo; "comum / abundante" =
  # Argissolo. Ferralic + CTC<=17 + thickness>=50 + cerosidade fraca
  # = Latossolo mesmo com clay increase marginal.
  #
  # v0.9.83: the strong-clay-films decision is delegated to
  # argic_with_strong_clay_films() so the same rule can be audited
  # with audit_argic_strong_films(). Behaviour is bit-for-bit
  # identical: bt is the same DiagnosticResult, the films are pulled
  # from the same bt$layers, and the strong-qualifier match is the
  # same Portuguese-aware regex.
  argic_films <- argic_with_strong_clay_films(pedon)
  argic_with_strong_films <- isTRUE(argic_films$passed)
  pass_layers <- function(d) if (isTRUE(d$passed)) d$layers
                              else integer(0)
  argic_excluded <- argic_films$layers
  excluded_layers <- unique(c(
    pass_layers(pl), pass_layers(gl), pass_layers(bn),
    argic_excluded
  ))
  layers_remaining <- setdiff(fer$layers, excluded_layers)
  passed <- length(layers_remaining) > 0L &&
              !isTRUE(pl$passed) && !isTRUE(gl$passed) &&
              !isTRUE(bn$passed) &&
              !argic_with_strong_films
  DiagnosticResult$new(
    name = "B_latossolico",
    passed = passed,
    layers = layers_remaining,
    evidence = list(
      ferralic        = fer,
      argic_concurrent = bt,                # v0.9.61: NOT excluding
      excluded_by     = list(plintico = pl, gleyic = gl, B_nitico = bn)
    ),
    missing = fer$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 57-59",
    notes = paste0("v0.9.61: precedencia revisada -- argic concurrent ",
                     "NO LONGER exclui (per SiBCS Cap 18 latossolic ",
                     "features dominam quando ferralic + CTC<=17 + ",
                     "thickness>=50). plintic + gleyic + nitic ainda ",
                     "excluem (definem ordens distintas).")
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
#'   \item < 50\% estrutura da rocha original;
#'   \item evidencias de pedogenese (cor mais viva OR remocao de
#'         carbonatos OR designation \code{Bw}/\code{Bi});
#'   \item NAO satisfaz: argic, ferralic, espodic, planic, e nao tem
#'         duripa/petrocalcico/fragipa.
#' }
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Numeric threshold or option (see Details).
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
B_incipiente <- function(pedon, min_thickness = 10) {
  h <- pedon$horizons
  # v0.9.10: extended designation pattern to cover the full set of
  # "weakly developed B" suffixes used in canonical descriptions:
  # Bw (weathered), Bi (incipient), Bk (calcic accumulation),
  # Bv (vertic-like), Bg (gleyed), Bn (natric-incipient), Bj
  # (jarositic), Bz (salt-accumulating). Excludes Bt (argic / textural),
  # Bo (latossolic / oxic), Bs / Bh (spodic), Bp (plinthic) -- those
  # are caught by the exclusion list below. Pre-v0.9.10 the regex was
  # `^B[wi]|^Bg|^Bv`, which dropped chernozem-style Bk and similar
  # calcic / saline B horizons and is what made the canonical
  # chernozem fixture fall through chernossolo() (no Bi nor Bt
  # path) into Neossolos.
  desg_match <- test_pattern_match(h, "designation", "^B[wikvgnzj]")
  candidates <- desg_match$layers
  # Exclusoes
  fer  <- ferralic(pedon)
  arg  <- argic(pedon)
  esp  <- spodic(pedon)
  plan <- planic_features(pedon)
  ver  <- vertic_horizon(pedon)
  # v0.9.137: SiBCS Cap 2 p.60 (a) -- B incipiente must ALSO NOT show
  # cementation/hardening (duripa, petrocalcico), fragipa brittleness, the
  # plinthite of a plintico, nor distinct gleyic reduction. The prior list
  # missed these five, so a cemented Bkm (petrocalcico) or a gleyed Bg could
  # leak through the ^B[wikvgnzj] designation gate (which admits k and g).
  # Each test returns no layers when its evidence is absent, so the added
  # exclusions are byte-identical on pedons lacking that evidence.
  dur  <- duric_horizon(pedon)
  pet  <- petrocalcic(pedon)
  fra  <- fragic(pedon)
  pli  <- plinthic(pedon)
  gle  <- gleyic_properties(pedon)
  excluded <- unique(c(fer$layers %||% integer(0),
                        arg$layers %||% integer(0),
                        esp$layers %||% integer(0),
                        plan$layers %||% integer(0),
                        ver$layers %||% integer(0),
                        dur$layers %||% integer(0),
                        pet$layers %||% integer(0),
                        fra$layers %||% integer(0),
                        pli$layers %||% integer(0),
                        gle$layers %||% integer(0)))
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
                           vertic = ver$layers, duric = dur$layers,
                           petrocalcic = pet$layers, fragic = fra$layers,
                           plinthic = pli$layers, gleyic = gle$layers)
    ),
    missing = c(desg_match$missing, thick_test$missing),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 59-61"
  )
}


#' Horizonte B nitico (SiBCS Cap 2, p 61-62; v0.7)
#'
#' Subsuperficial nao hidromorfico, textura argilosa/muito argilosa
#' (clay \\>= 35\% desde a superficie), com pequeno incremento de
#' argila (B/A \\<= 1.5), estrutura em blocos sub/angulares ou
#' prismatica grau moderado/forte, cerosidade no minimo comum +
#' moderada, espessura \\>= 30 cm. Argila ativ baixa OR ativ alta +
#' carater aluminico.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Numeric threshold or option (see Details).
#' @param min_clay_pct Numeric threshold or option (see Details).
#' @param max_b_a_ratio Numeric threshold or option (see Details).
#' @param min_cerosidade Numeric threshold or option (see Details).
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
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
  # Step 3: estrutura em blocos / prismatica / polyhedral.
  # v0.9.10: added "polyhedral" / "polyedric" -- canonical SiBCS B
  # nitico structure descriptor in tropical Nitossolos (estrutura em
  # blocos sub-angulares evoluindo para "poliedrica") that the older
  # regex was missing.
  # v0.9.137: SiBCS Cap 2 p.62 (c) requires the structure GRADE to be moderate
  # or strong (not merely the type in blocks/prismatic). refine-when-present:
  # a RECORDED structure_grade must read moderate/strong; NA grade -> the layer
  # is admitted as before (byte-identical).
  type_match <- !is.na(h$structure_type[clay_ok]) &
                  grepl(paste0("blocks|block|blocos|bloco|",
                                 "prismatic|prismatica|",
                                 "polyhedral|polyedric|poliedric"),
                          h$structure_type[clay_ok], ignore.case = TRUE)
  grade_ok   <- is.na(h$structure_grade[clay_ok]) |
                  grepl("moder|strong|forte", h$structure_grade[clay_ok],
                          ignore.case = TRUE)
  struct_ok  <- any(type_match & grade_ok)
  # Step 4: cerosidade. SiBCS Cap 2 p.62 (c): quantity >= "comum" AND GRADE
  # >= moderate ("grau forte ou moderado") -- the critical discriminant vs
  # Latossolos (which carry at most "pouca e fraca" clay-skins). The prior test
  # checked only quantity; refine-when-present adds the grade gate via
  # clay_films_strength (NA strength -> byte-identical).
  ceros_amount  <- !is.na(h$clay_films_amount[clay_ok]) &
                     h$clay_films_amount[clay_ok] %in% min_cerosidade
  ceros_grade   <- is.na(h$clay_films_strength[clay_ok]) |
                     grepl("moder|strong|forte", h$clay_films_strength[clay_ok],
                             ignore.case = TRUE)
  cerosidade_ok <- any(ceros_amount & ceros_grade)
  # Step 5: thickness. SiBCS Cap 2 p.62 (a): >= 30 cm, EXCEPT >= 15 cm when a
  # lithic / lithic-fragmentary contact occurs within the first 50 cm.
  contact_shallow <- any(!is.na(h$designation) &
                           grepl("^[0-9]*(R|Cr|Cd)", h$designation) &
                           !is.na(h$top_cm) & h$top_cm <= 50)
  eff_min_thick <- if (contact_shallow) 15 else min_thickness
  thk <- test_minimum_thickness(h, min_cm = eff_min_thick,
                                   candidate_layers = clay_ok)
  # Step 6: argila atividade baixa OR (atividade alta + carater aluminico).
  # v0.9.137: this is the VERBATIM SiBCS Cap 2 p.62 criterion (d) -- there is
  # no "ferric / high-Fe" alternative path in the B nitico DEFINITION. The
  # earlier v0.9.10 `ferri_ok` short-circuit (>= 8% Fe-DCB) was a deviation
  # added on the premise that high-activity ferric Nitossolos were being lost
  # to Argissolos; it is REMOVED here because (1) it is not in the verbatim
  # definition and (2) measured removal is benchmark-neutral (BDsolos RJ
  # confusion and Redape order accuracy both unchanged) -- ferric Nitossolos
  # are oxidic, hence low-activity, and already pass via the low-activity path.
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
#' @param pedon A \code{\link{PedonRecord}}.
#' @param ... Reserved for future arguments.
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
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
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
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
#' @param pedon A \code{\link{PedonRecord}}.
#' @param ... Reserved for future arguments.
#' @noRd
horizonte_E_albico <- function(pedon, ...) {
  res <- albic(pedon, ...)
  res$name <- "horizonte_E_albico"
  res$reference <- "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 66-67"
  res
}


#' Horizonte plintico (SiBCS Cap 2, p 67-68; v0.7)
#'
#' Plintita \\>= 15\% volume, espessura \\>= 15 cm. Tem precedencia
#' sobre B textural, latossolico, nitico, B incipiente, planico (sem
#' carater sodico), e glei. Reuso de \code{\link{plinthic}} (WRB).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_plinthite_pct Numeric threshold or option (see Details).
#' @param min_thickness Numeric threshold or option (see Details).
#' @noRd
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
#' \\>= 50\% volume material grosso (predominio petroplintita) numa
#' matriz, espessura \\>= 30 cm. Designation Ac/Ec/Bc/Cc.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_petroplinthite_pct Numeric threshold or option (see Details).
#' @param min_thickness Numeric threshold or option (see Details).
#' @noRd
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
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Numeric threshold or option (see Details).
#' @noRd
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
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Numeric threshold or option (see Details).
#' @noRd
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
#' @param pedon A \code{\link{PedonRecord}}.
#' @param ... Reserved for future arguments.
#' @noRd
horizonte_calcico <- function(pedon, ...) {
  res <- calcic(pedon, ...)
  # v0.9.139: SiBCS Cap 2 p.71 requires the calcic horizon to carry >= 50 g/kg
  # (5% absolute) MORE CaCO3 than the subjacent layer -- and, unlike WRB/USDA,
  # SiBCS has NO protocalcic morphological alternative (the "expresso em volume"
  # caveat is only a measurement method for gravelly secondary carbonate, still
  # the same +50 enrichment). So the enrichment is enforced HERE (SiBCS-only),
  # not in the shared calcic() core, whose WRB/USDA consumers rely on the
  # unmeasured protocalcic OR-path (see calcic_enrichment_v09139.md). Refine-
  # when-present: only drops a layer that can be disproven.
  if (isTRUE(res$passed)) {
    hh  <- pedon$horizons
    enr <- test_caco3_enrichment(hh, candidate_layers = res$layers)
    # v0.9.142: the SiBCS "expresso em volume" alternative -- the +50 enrichment
    # may instead be shown as >= 5% by-volume secondary carbonate (gravelly /
    # concretionary / powdery). refine-when-present: absent -> byte-identical.
    sc    <- hh[["secondary_carbonates_pct"]]
    byvol <- if (!is.null(sc))
               res$layers[!is.na(sc[res$layers]) & sc[res$layers] >= 5]
             else integer(0)
    keep <- union(enr$layers, byvol)
    if (length(keep) == 0L) {
      res$passed <- FALSE
      res$layers <- integer(0)
    } else {
      res$layers <- keep
    }
    res$evidence <- c(res$evidence %||% list(),
                       list(enrichment = enr, by_volume_layers = byvol))
  }
  res$name <- "horizonte_calcico"
  res$reference <- "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 71-72"
  res
}


#' Horizonte petrocalcico (SiBCS Cap 2, p 72; v0.7)
#'
#' Reuso de \code{\link{petrocalcic}} (WRB v0.3.3).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param ... Reserved for future arguments.
#' @noRd
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
#' @param pedon A \code{\link{PedonRecord}}.
#' @param ... Reserved for future arguments.
#' @noRd
horizonte_sulfurico <- function(pedon, ...) {
  res <- thionic(pedon, max_pH = 3.5, ...)
  # SiBCS Cap 2 p.72-73: sulfurico = thickness >= 15 cm + pH(H2O 1:2.5) <= 3.5
  # AND >= 1 of (a) jarosite, (b) sulfidic material immediately below,
  # (c) >= 0.05% water-soluble sulfate. thionic encodes only the sulfidic-
  # material path. v0.9.137 adds the JAROSITE OR-path (jarosite_present field,
  # v0.9.133) -- refine-when-present, so a pedon with no jarosite datum keeps
  # thionic's result unchanged. (The soluble-sulfate path stays schema-blocked:
  # no soluble-sulfate column.)
  h <- pedon$horizons
  jar <- h[["jarosite_present"]]
  if (!isTRUE(res$passed) && !is.null(jar) && any(jar %in% TRUE)) {
    ph_low     <- test_ph_below(h, max_ph = 3.5)
    jar_layers <- which(jar %in% TRUE)
    cand       <- intersect(ph_low$layers, jar_layers)
    if (length(cand)) {
      thk <- test_minimum_thickness(h, min_cm = 15, candidate_layers = cand)
      if (isTRUE(thk$passed)) {
        return(DiagnosticResult$new(
          name = "horizonte_sulfurico", passed = TRUE, layers = thk$layers,
          evidence = list(thionic = res, jarosite_path = TRUE,
                           ph = ph_low, thickness = thk),
          missing = character(0),
          reference = "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 72-73"))
      }
    }
  }
  res$name <- "horizonte_sulfurico"
  res$reference <- "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 72-73"
  res
}


#' Horizonte vertico (SiBCS Cap 2, p 73; v0.7)
#'
#' Reuso de \code{\link{vertic_horizon}} (WRB Ch 3.1, v0.3.3) com
#' criterios essencialmente identicos: clay \\>= 30\%, slickensides,
#' fendas \\>= 1 cm, espessura \\>= 20 cm. v0.7 SiBCS additional gate:
#' COLE \\>= 0.06 (proxy via shrink-swell).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param ... Reserved for future arguments.
#' @noRd
horizonte_vertico <- function(pedon, ...) {
  # v0.9.137: SiBCS Cap 2 p.73 requires cracks ">= 1 cm" wide (vs the WRB/USDA
  # 0.5 cm). Pass min_crack_width_cm = 1.0 so the SiBCS vertico is stricter on
  # the field-crack path; the COLE and 'v'-designation paths are unaffected, so
  # a Vertissolo recorded via COLE or a v-modifier designation still passes.
  res <- vertic_horizon(pedon, min_thickness = 20, min_crack_width_cm = 1.0, ...)
  res$name <- "horizonte_vertico"
  res$reference <- "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 73"
  res
}


#' Fragipa (SiBCS Cap 2, p 73-74; v0.7)
#'
#' Reuso de \code{\link{fragic}} (WRB v0.3.3): horizonte
#' subsuperficial endurecido quando seco, baixa MO, BD elevada,
#' quebradicidade.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param ... Reserved for future arguments.
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
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
#' cimentado por silica, continuo ou em \\>= 50\% volume.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param ... Reserved for future arguments.
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
duripa <- function(pedon, ...) {
  res <- duric_horizon(pedon, ...)
  res$name <- "duripa"
  res$reference <- "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 74"
  res
}
