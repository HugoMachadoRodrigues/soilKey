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
#'
#' Since v0.9.253 the horizon is read wherever it lies and with the whole of
#' its thickness. The book says of it that it is so "ainda que, no presente,
#' possa encontrar-se recoberto por horizontes ou depositos minerais" (p. 49),
#' and its thickness is that of the horizon, made of its adjoining
#' sub-horizons (Oo1, Oo2, Oo3; Hd, Hod1, Hod2). Until v0.9.252 only a layer
#' starting within 5 cm of the surface was looked at, so that a horizon
#' described in sub-horizons counted the first of them alone, and a buried
#' one was not seen.
#'
#' The layers returned are those of every horizon that meets one of the
#' thicknesses. The evidence gives, for each run of adjoining organic layers
#' (\code{horizons}), its limits, its thickness, whether it lies on a lithic
#' contact (a layer R or Cr right below it) and whether it passes;
#' \code{thickness_cm} and \code{on_rock} are those of the thickest horizon
#' that passes, or of the thickest run when none does.
#'
#' Since v0.9.254 an O or H layer whose upper depth is greater than its lower
#' one ("O 120-0"; "Hd 110-60" above "Hoj 60-0") is read as the older
#' descriptions record it, upward from the top of the mineral soil: it lies
#' above the mineral surface and has the thickness between its two figures.
#' Until v0.9.253 such a layer, of a thickness below zero, was never part of
#' the horizon. \code{surface_cm} in the evidence is the top of the soil in
#' the depths of the description: zero, or the top of the uppermost layer
#' recorded upward (minus 120 for "O 120-0"). The depths of the profile are
#' not changed, and a layer that is no O or H is not read this way.
#'
#' Not tested: the volume of plant tissue is no column of the profile, so the
#' 40 cm asked where it makes 75\% or more of the horizon (item b) are not
#' asked; and a layer with 90\% or more of its volume in fragments over 2 mm
#' is not told from the weight of gravel the sheets record.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_oc_g_kg Numeric threshold or option (see Details).
#' @noRd
horizonte_histico <- function(pedon, min_oc_g_kg = 80) {
  h <- pedon$horizons
  ref <- "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 49-50"
  # Convert g/kg threshold to %.
  min_oc_pct <- min_oc_g_kg / 10
  # v0.9.10: candidate layers must EITHER carry an organic-horizon
  # designation (O, H, Op, Oa, Oe, Oi, Hr, Hd) OR have OC well above
  # the mineral-soil ceiling (>= 12 % is the threshold for organic-
  # dominant material per SiBCS Cap 1). Pre-v0.9.10 the test only
  # checked oc_pct >= 8 %, which falsely matched mineral A horizons of
  # high-OC andic profiles (canonical Andosol with Ah of 8 % OC was
  # being routed into Organossolos).
  desg <- .desg_master(h$designation)
  is_organic_designation <- !is.na(desg) &
                              grepl("^[OH][a-z]?", desg)
  is_dominantly_organic  <- !is.na(h$oc_pct) & h$oc_pct >= 12
  # v0.9.254: an O or H layer recorded upward from the mineral surface
  dp <- .histico_depths(h, is_organic_designation)
  top <- dp$top; bottom <- dp$bottom
  candidates <- which((is_organic_designation | is_dominantly_organic) &
                        !is.na(h$oc_pct) & h$oc_pct >= min_oc_pct &
                        !is.na(top) & !is.na(bottom) & bottom > top)
  if (length(candidates) == 0L) {
    have_oc <- !all(is.na(h$oc_pct))
    return(DiagnosticResult$new(
      name = "horizonte_histico",
      passed = if (have_oc) FALSE else NA,
      layers = integer(0),
      evidence = list(),
      missing = if (!have_oc) "oc_pct" else character(0),
      reference = ref
    ))
  }
  # v0.9.253: the horizon is made of its adjoining sub-horizons, wherever
  # they lie in the profile
  cs <- candidates[order(top[candidates])]
  run <- integer(length(cs)); reach <- -Inf; k <- 0L
  for (j in seq_along(cs)) {
    if (top[cs[j]] > reach + 1e-6) k <- k + 1L
    run[j] <- k
    reach <- max(reach, bottom[cs[j]])
  }
  known <- which(!is.na(top))
  by_depth <- known[order(top[known])]
  hz <- do.call(rbind, lapply(seq_len(k), function(g) {
    L <- cs[run == g]
    g_top <- min(top[L]); g_bottom <- max(bottom[L])
    # the layer right below the horizon: a lithic contact?
    below <- by_depth[!(by_depth %in% L) & top[by_depth] >= g_bottom - 1e-6]
    on_rock <- length(below) > 0L && !is.na(desg[below[1L]]) &&
                 grepl("^R|^Cr", desg[below[1L]])
    data.frame(top_cm = g_top, bottom_cm = g_bottom,
               thickness_cm = g_bottom - g_top, on_rock = on_rock)
  }))
  hz$passed <- hz$thickness_cm >= 20 | (hz$thickness_cm >= 10 & hz$on_rock)
  layers <- sort(cs[run %in% which(hz$passed)])
  best <- if (any(hz$passed))
            which(hz$passed)[which.max(hz$thickness_cm[hz$passed])]
          else which.max(hz$thickness_cm)
  DiagnosticResult$new(
    name = "horizonte_histico",
    passed = any(hz$passed), layers = layers,
    evidence = list(thickness_cm = hz$thickness_cm[best],
                     on_rock = hz$on_rock[best],
                     contiguous_layers = layers,
                     horizons = hz,
                     surface_cm = min(0, top[dp$upward]),
                     recorded_upward = dp$upward),
    missing = character(0),
    reference = ref
  )
}


#' The depths of the layers as the histic horizon reads them. An O or H layer
#' whose upper depth is greater than its lower one is recorded upward from the
#' top of the mineral soil ("O 120-0"): it lies above that surface, from minus
#' its first figure to minus its second. Every other layer keeps its depths.
#' Returns \code{top}, \code{bottom} and \code{upward}, the layers read so.
#' @noRd
.histico_depths <- function(h, organic) {
  top <- h$top_cm; bottom <- h$bottom_cm
  up <- organic & !is.na(top) & !is.na(bottom) & top > bottom
  top[up] <- -h$top_cm[up]; bottom[up] <- -h$bottom_cm[up]
  list(top = top, bottom = bottom, upward = which(up))
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
    is_A <- grepl("^[0-9]*A", .desg_master(h$designation)[i] %||% "")
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
  desig_all <- .desg_master(h$designation %||% rep(NA_character_, nrow(h)))
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
        grepl("^B", .desg_master(h$designation)[candidates[k]])) {
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


#' Horizonte B textural (SiBCS Cap 2, p 54-57)
#'
#' The B textural as the book states it: a subsurface horizon of texture
#' francoarenosa or finer, at least 7.5 cm thick (thickness items a to e),
#' that meets one or more of the items below (v0.9.241).
#' \itemize{
#'   \item (f) an E horizon above the B, the B having more clay than the
#'         horizons above it and being no B espodico, plintico or planico;
#'   \item (g) a mudanca textural abrupta from the A or E to the B (Cap 1,
#'         p 30-31): at least twice the clay when the A or E has less than
#'         200 g/kg, 200 g/kg more otherwise, within 7.5 cm. The distance is
#'         read from the distinctness of the transition
#'         (\code{boundary_distinctness}: abrupt or clear); where it is not
#'         recorded the two layers are compared;
#'   \item (h) the relacao textural B/A of footnote 4, above 1.50, 1.70 or
#'         1.80 according to the clay of the A
#'         (\code{test_ratio_textural_sibcs});
#'   \item (i) where the increase is below item (h), the cerosidade of one or
#'         more sub-horizons of the upper part of the B (read as its upper
#'         half): (1) texture media with no macroaggregates and cerosidade of
#'         moderate grade; (2) texture media with prismatic or blocky
#'         structure of moderate or strong grade and cerosidade of at least
#'         moderate grade; (3) texture argilosa or muito argilosa with that
#'         structure and cerosidade at least common and weak or few and
#'         moderate; (4) relacao textural of 1.4 or more with a fragipa
#'         within 200 cm, the B being no B espodico;
#'   \item (j) with a discontinuity of parent material between the A or E and
#'         the B, or with only a ploughed layer above the B, items (h) or (i)
#'         are needed.
#' }
#' Amounts and grades written as ranges are read by their lower end, the
#' requirements being minima. Lamellae (item d) are not tested.
#'
#' Until v0.9.240 this function was the WRB \code{\link{argic}} horizon, whose
#' clay increase (6 percentage points, a ratio of 1.4 or 20 points, between a
#' layer and the least clayey layer anywhere above it) is not the one of the
#' book, joined to item (h); items (f), (g) and (i) were not tested.
#'
#' The layers returned are the B sub-horizons (BC excluded) of texture
#' francoarenosa or finer; \code{evidence$upper_layers} are those that carry
#' the evidence, from the top of the B down.
#'
#' The items are stated for the A, E and B horizons. A profile with no horizon
#' designation at all (depth slices, for instance) cannot be read by them and
#' is tested as before, by the clay increase between layers.
#'
#' In the morphological mode
#' (\code{options(soilKey.morphological_inference = TRUE)}) a "t" in the
#' designation of a B horizon stands for the B textural when the items do not
#' show it. The diagnostic engine
#' (\code{options(soilKey.diagnostic_engine = "aqp")}) does not change this
#' test: it changed the argic horizon that this function used to be.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param ... Passed to \code{\link{argic}} for a profile with no horizon
#'   designation; ignored otherwise.
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
B_textural <- function(pedon, ...) {
  d <- pedon$horizons$designation
  if (!any(!is.na(d) & nzchar(trimws(as.character(d))))) {
    res <- argic(pedon, lithic_discontinuity = FALSE, ...)
    res$name      <- "B_textural"
    res$reference <- "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 54-57"
    res$notes     <- paste0("no horizon designation: tested by the clay ",
                            "increase between layers (WRB argic)")
    return(res)
  }
  .b_textural_sibcs(pedon)
}


#' The B textural by the items of SiBCS 2018, Cap 2, p. 54-57, for a profile
#' with horizon designations. See \code{\link{B_textural}}.
#' @noRd
.b_textural_sibcs <- function(pedon) {
  h   <- pedon$horizons
  dm  <- .desg_master(h$designation)
  ref <- "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 54-57"
  hm  <- data.table::copy(h); hm$designation <- dm
  out <- function(passed, layers = integer(0), evidence = list(),
                  missing = character(0)) {
    # the morphological mode: a "t" in the designation of a B horizon
    if (!isTRUE(passed) && .morph_inference_enabled()) {
      ml <- .morph_layers(hm, "argic")
      if (length(ml) > 0L) {
        evidence$morphological_inference <- list(
          passed = TRUE, layers = ml, diagnostic = "argic",
          provenance = "morphological_designation")
        passed <- TRUE; layers <- ml
      }
    }
    DiagnosticResult$new(name = "B_textural", passed = passed, layers = layers,
                         evidence = evidence, missing = missing,
                         reference = ref)
  }
  is_a <- !is.na(dm) & grepl("^A", dm)
  is_e <- !is.na(dm) & grepl("^E", dm)
  is_b <- !is.na(dm) & grepl("^B", dm) & !grepl("^B/?C", dm)
  b <- which(is_b & !is.na(h$top_cm))
  b <- b[order(h$top_cm[b])]
  if (length(b) == 0L) return(out(FALSE, evidence = list(reason = "no B horizon")))

  # texture francoarenosa or finer
  tex <- .francoarenosa_or_finer(h$silt_pct[b], h$clay_pct[b])
  bt <- b[!is.na(tex) & tex]
  if (length(bt) == 0L) {
    unknown <- all(is.na(tex))
    return(out(if (unknown) NA else FALSE,
               evidence = list(reason = "texture"),
               missing = if (unknown) c("sand_pct", "silt_pct", "clay_pct")
                         else character(0)))
  }
  # thickness, items (a) to (e): 7.5 cm in every case
  thickness <- sum(h$bottom_cm[bt] - h$top_cm[bt], na.rm = TRUE)
  if (!(thickness >= 7.5))
    return(out(FALSE, evidence = list(reason = "thickness",
                                      thickness_cm = thickness)))

  b_top <- min(h$top_cm[b])
  ord   <- order(h$top_cm)
  pos   <- match(b[1], ord)
  up    <- if (!is.na(pos) && pos > 1L) ord[pos - 1L] else NA_integer_
  above <- which((is_a | is_e) & !is.na(h$top_cm) & h$top_cm < b_top)

  # (h) relacao textural
  rt <- test_ratio_textural_sibcs(h)
  item_h <- isTRUE(rt$passed)
  ratio  <- rt$details$ratio %||% NA_real_

  # (g) mudanca textural abrupta, from the A or E to the B: the increase of
  # clay "em uma distancia vertical <= 7,5 cm". The distance is what the
  # field sheet records as the distinctness of the transition to the horizon
  # below (abrupt, under 2.5 cm; clear, 2.5 to 7.5 cm); where it is recorded
  # as gradual or diffuse there is no abrupt change, and where it is not
  # recorded the two layers are compared.
  item_g <- isTRUE(.mudanca_abrupta_a_b(h)$passed)

  # (f) an E horizon above the B
  item_f <- FALSE
  if (any(is_e[above])) {
    b_clay <- suppressWarnings(max(h$clay_pct[bt], na.rm = TRUE))
    a_clay <- suppressWarnings(max(h$clay_pct[above], na.rm = TRUE))
    item_f <- is.finite(b_clay) && is.finite(a_clay) && b_clay > a_clay &&
      !isTRUE(B_espodico(pedon)$passed) &&
      !isTRUE(horizonte_plintico(pedon)$passed) &&
      !isTRUE(B_planico(pedon)$passed)
  }

  # (i) cerosidade, where the increase is below item (h)
  item_i <- NA_character_
  i_layers <- integer(0)
  if (!item_h) {
    b_thick <- max(h$bottom_cm[b], na.rm = TRUE) - b_top
    upper   <- bt[h$top_cm[bt] < b_top + b_thick / 2]
    for (j in upper) {
      it <- .b_textural_item_i(h$clay_pct[j], h$structure_type[j],
                                h$structure_grade[j], h$clay_films_amount[j],
                                h$clay_films_strength[j])
      if (!is.na(it)) {
        if (is.na(item_i)) item_i <- it
        i_layers <- c(i_layers, j)
      }
    }
    if (is.na(item_i) && is.finite(ratio) && ratio >= 1.4 &&
          isTRUE(fragipa(pedon)$passed) && !isTRUE(B_espodico(pedon)$passed))
      item_i <- "i4"
  }

  # (j) a discontinuity of parent material between the A or E and the B, or
  # only a ploughed layer above the B: items (h) or (i) are needed
  mat <- .desg_material_sibcs(h$designation)
  discontinuity <- !is.na(up) && !is.na(mat[up]) && !is.na(mat[b[1]]) &&
    mat[up] != mat[b[1]]
  over_b <- which(!is.na(h$top_cm) & h$top_cm < b_top)
  only_ap <- length(over_b) == 1L && !is.na(dm[over_b]) &&
    grepl("^Ap", dm[over_b])
  item_j <- discontinuity || only_ap

  passed <- item_h || !is.na(item_i) ||
    (!item_j && (item_f || item_g))

  # the sub-horizons that carry the evidence, from the top of the B down
  deepest <- -Inf
  if (item_h) deepest <- max(deepest, h$top_cm[rt$layers %||% integer(0)])
  if (length(i_layers)) deepest <- max(deepest, h$top_cm[i_layers])
  if (!is.finite(deepest)) deepest <- b_top
  upper_layers <- bt[h$top_cm[bt] <= deepest]

  out(passed, if (passed) bt else integer(0),
      evidence = list(item_f = item_f, item_g = item_g, item_h = item_h,
                      item_i = item_i, item_j = item_j,
                      relacao_textural = rt, ratio = ratio,
                      thickness_cm = thickness,
                      upper_layers = if (passed) upper_layers else integer(0)))
}


#' Which condition of item (i) of the B textural a B sub-horizon meets, by its
#' texture, structure and cerosidade (SiBCS 2018, Cap 2, p. 56): "i1", "i2",
#' "i3" or NA. Texture media is read as less than 35% of clay, argilosa and
#' muito argilosa as 35% or more. Amounts and grades written as ranges count
#' by their lower end.
#' @noRd
.b_textural_item_i <- function(clay, structure_type, structure_grade,
                                films_amount, films_grade) {
  if (is.na(clay)) return(NA_character_)
  st  <- .field_term(structure_type)
  sg  <- .field_grade_class(structure_grade)
  amt <- .clay_films_class(.field_term(films_amount))
  gr  <- .field_grade_class(films_grade)
  if (is.na(amt) || amt < 1L) return(NA_character_)
  # a cerosidade that is present is at least weak: "comum e fraca" asks no
  # more of the grade, so that an unrecorded grade does not fail it
  weak_or_more <- is.na(gr) || gr >= 1L
  gr <- if (is.na(gr)) 0L else gr
  blocky <- !is.na(st) && !is.na(sg) && sg >= 2L &&
    grepl("bloc|block|prism|polyhedr|polyedr|poliedr", st)
  no_macro <- !is.na(st) &&
    grepl("graos? simples|single grain|macica|massive", st)
  if (clay < 35) {
    if (no_macro && gr >= 2L) return("i1")
    if (blocky && gr >= 2L) return("i2")
  } else if (blocky && ((amt >= 3L && weak_or_more) || (amt >= 2L && gr >= 2L))) {
    return("i3")
  }
  NA_character_
}


#' Horizonte B latossolico (SiBCS Cap 2, p 57-59)
#'
#' The requirements of the book that the data of a profile can show
#' (v0.9.241):
#' \itemize{
#'   \item (d) texture francoarenosa or finer;
#'   \item (g) cation exchange capacity below 17 cmolc per kg of clay, with no
#'         correction for carbon;
#'   \item (e) a molecular ratio SiO2/Al2O3 (Ki) of 2.2 or less, where the
#'         sulfuric attack is recorded (\code{sio2_sulfuric_pct},
#'         \code{al2o3_sulfuric_pct}): a layer with a Ki above 2.20 is no
#'         B latossolico (v0.9.243);
#'   \item (h) cerosidade, if present, at most few and weak: a layer with a
#'         larger amount or a higher grade is no B latossolico (a range
#'         counts by its upper end, the requirement being a maximum);
#'   \item (b) a thickness of 50 cm or more, of the horizon: of the adjoining
#'         sub-horizons that meet the other requirements, taken together;
#'   \item it "nao apresenta caracteristicas diagnosticas de horizontes glei,
#'         B textural, B nitico e plintico": the layers of a
#'         \code{\link{B_textural}}, of a horizonte glei, of a horizonte
#'         plintico and of a \code{\link{B_nitico}} are no B latossolico.
#'         Since v0.9.249 they take out their own layers only: until then the
#'         glei and the plinthic horizon were those of WRB (gleyic
#'         properties, plinthic horizon), and one of them, or a B nitico,
#'         anywhere in the profile voided the B latossolico of the whole
#'         profile.
#' }
#' Where the designations are recorded, only B horizons are candidates: the
#' B latossolico is a subsurface horizon below the A. Items (a) structure,
#' (c) rock structure and (f) weatherable minerals are not tested.
#'
#' Until v0.9.240 the 50 cm were asked of each layer on its own, so that a
#' B latossolico described as B1, B21 and B22 of 35 cm each was none; and
#' since v0.9.61 a B textural did not exclude it unless its clay films were
#' common or more, on a rule the comments attributed to Cap 18 of the book,
#' which deals with families and series.
#'
#' In the morphological mode a "Bw" or "Bo" of a tropical profile stands for
#' the B latossolico when the data do not show it.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Minimum thickness of the horizon, in cm (50).
#' @param max_cec_per_clay CEC per kg clay the horizon must stay below.
#'   Defaults to \code{NULL} (engine-aware): 17 in soilkey engine
#'   (SiBCS Cap 2: "menor que 17 cmolc kg-1 de argila, sem correcao
#'   para carbono") or 20 in aqp engine (v0.9.68 regional tolerance for
#'   Embrapa lab methodology offset). Strict since v0.9.212: a value of
#'   exactly 17 is not latossolic, it is the B incipiente's "17 ou maior".
#' @param engine One of \code{"soilkey"} (default) or \code{"aqp"};
#'   \code{NULL} reads \code{getOption("soilKey.diagnostic_engine")}.
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
  bt <- B_textural(pedon)
  d  <- pedon$horizons$designation
  described <- any(!is.na(d) & nzchar(trimws(as.character(d))))
  # a profile with no horizon designation has its B textural tested by the
  # clay increase between layers, which did not exclude the B latossolico
  not_latossolic <- if (described && isTRUE(bt$passed)) bt$layers
                    else integer(0)
  .b_latossolico_core(pedon, min_thickness, max_cec_per_clay,
                       not_latossolic = not_latossolic, B_textural = bt)
}


#' The B latossolico of a profile, given the layers that are not latossolic
#' because they are a B textural. \code{\link{B_latossolico}} gives all the
#' layers of the B textural; \code{carater_latossolico()}, which looks for a
#' B latossolico below a B textural, gives its upper sub-horizons only.
#' @noRd
.b_latossolico_core <- function(pedon, min_thickness, max_cec_per_clay,
                                 not_latossolic = integer(0),
                                 B_textural = NULL) {
  h  <- pedon$horizons
  dm <- .desg_master(h$designation)
  tropical <- .pedon_is_tropical(pedon)
  tests <- list()
  tests$texture      <- test_ferralic_texture(h, tropical = tropical)
  tests$cec_per_clay <- test_cec_per_clay(h,
                                          max_cmol_per_kg_clay = max_cec_per_clay)
  tex_layers <- union(tests$texture$layers %||% integer(0),
                      which(.francoarenosa_or_finer(h$silt_pct, h$clay_pct)))
  cand <- intersect(tex_layers, tests$cec_per_clay$layers %||% integer(0))
  # a subsurface horizon below the A: B horizons, where designations exist
  cand <- cand[is.na(dm[cand]) | !nzchar(dm[cand]) | grepl("^B", dm[cand])]

  # v0.9.249: "nao apresenta caracteristicas diagnosticas de horizontes glei,
  # B textural, B nitico e plintico" (p. 59) is said of the horizon. The
  # layers of those horizons, as this system defines them, are no part of
  # the B latossolico; the layers that are left make it if they make its
  # 50 cm. Until v0.9.248 the horizons tested were those of WRB (gleyic
  # properties, plinthic horizon), and one of them anywhere in the profile
  # voided the B latossolico of the whole profile.
  pl <- horizonte_plintico(pedon)
  gl <- horizonte_glei(pedon)
  bn <- B_nitico(pedon)
  pass_layers <- function(x) if (isTRUE(x$passed)) x$layers else integer(0)
  # (h) cerosidade at most few and weak
  films_over <- which(.films_above_few_and_weak(h$clay_films_amount,
                                                h$clay_films_strength))
  # (e) "Relacao molecular SiO2/Al2O3 (Ki) igual ou inferior a 2,2": a layer
  # whose Ki is recorded above it is no B latossolico (v0.9.243)
  ki <- compute_ki(h$sio2_sulfuric_pct, h$al2o3_sulfuric_pct)
  ki_over <- which(!is.na(ki) & round(ki, 2) > 2.2)
  excluded <- unique(c(not_latossolic, pass_layers(pl), pass_layers(gl),
                       pass_layers(bn), films_over, ki_over))
  cand <- setdiff(cand, excluded)

  tests$thickness <- test_minimum_thickness(h, min_cm = min_thickness,
                                            candidate_layers = cand,
                                            adjoining = TRUE)
  layers <- tests$thickness$layers %||% integer(0)
  passed <- length(layers) > 0L
  missing <- unique(c(tests$texture$missing, tests$cec_per_clay$missing,
                      tests$thickness$missing))
  if (length(layers) == 0L &&
        (is.na(tests$texture$passed) || is.na(tests$cec_per_clay$passed)) &&
        length(missing) > 0L)
    passed <- NA

  evidence <- c(tests, list(
    max_cec_used = max_cec_per_clay,
    B_textural   = B_textural,
    films_above_few_and_weak = films_over,
    ki = ki, ki_above_2_2 = ki_over,
    excluded_by  = list(plintico = pl, glei = gl, B_nitico = bn)))

  # the morphological mode: a "Bw" or "Bo" of a tropical profile
  if (!isTRUE(passed) && .morph_inference_enabled()) {
    hm <- data.table::copy(h); hm$designation <- dm
    ml <- setdiff(.morph_layers(hm, "ferralic", min_top = 25,
                                tropical = tropical), excluded)
    if (length(ml) > 0L) {
      evidence$morphological_inference <- list(
        passed = TRUE, layers = ml, diagnostic = "ferralic",
        provenance = "morphological_designation")
      layers <- ml
      passed <- TRUE
    }
  }

  DiagnosticResult$new(
    name = "B_latossolico", passed = passed,
    layers = if (isTRUE(passed)) layers else integer(0),
    evidence = evidence, missing = missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 57-59"
  )
}


#' Whether the cerosidade recorded for a layer is above "pouca e fraca", the
#' most a B latossolico may have (SiBCS 2018, Cap 2, p. 59, item h): an
#' amount of common or more, or a grade of moderate or more. The requirement
#' is a maximum, so that a range counts by its upper end ("Pouca a comum",
#' "Fraca a moderada"). FALSE where nothing is recorded or the cerosidade is
#' recorded as absent. Vectorised.
#' @noRd
.films_above_few_and_weak <- function(amount, grade) {
  a <- .field_term(amount)
  g <- .field_term(grade)
  present <- !is.na(a) & nzchar(a) &
    !grepl("^(none|absent|no|nenhuma|ausente|sem)\\b", a)
  amount_over <- present &
    grepl("comu|common|abundan|\\bmany\\b|muita|dominan", a)
  grade_over <- present & !is.na(g) & grepl("moder|fort|strong", g)
  amount_over | grade_over
}


#' Whether the texture of a layer is francoarenosa or finer: silt plus twice
#' the clay of 30% or more. With 15% of clay or more it is so whatever the
#' silt, which a sheet may lack; NA where the two cannot tell. Vectorised.
#' @noRd
.francoarenosa_or_finer <- function(silt, clay) {
  out <- silt + 2 * clay >= 30
  out[!is.na(clay) & clay >= 15] <- TRUE
  out
}


#' Whether the transition to the horizon below, as its distinctness is
#' recorded on the field sheet, takes place within 7.5 cm: TRUE for abrupt
#' ("abrupta", under 2.5 cm) and clear ("clara", 2.5 to 7.5 cm), FALSE for
#' gradual and diffuse ("difusa"), NA where it is not recorded or not read.
#' The limit is a maximum, so that a range counts by its wider end ("Clara a
#' gradual" is FALSE). Vectorised.
#' @noRd
.boundary_within_7_5_cm <- function(x) {
  t <- .field_term(x)
  out <- rep(NA, length(t))
  out[!is.na(t) & grepl("abrupt|clear|clara|sharp", t)] <- TRUE
  out[!is.na(t) & grepl("gradual|diffuse|difus", t)] <- FALSE
  out
}


#' The number of the material a designation records, the Roman numerals of
#' the older surveys included ("IIBt" and "2Bt" are 2, "Bt" is 1). NA where
#' there is no designation.
#' @noRd
.desg_material_sibcs <- function(d) {
  d <- trimws(as.character(d))
  out <- rep(1L, length(d))
  ar <- grepl("^[0-9]+", d)
  out[ar] <- suppressWarnings(as.integer(sub("^([0-9]+).*", "\\1", d[ar])))
  ro <- !ar & grepl("^[IVX]+(?=[A-HJ-UWYZ(])", d, perl = TRUE)
  out[ro] <- suppressWarnings(as.integer(utils::as.roman(
    sub("^([IVX]+).*", "\\1", d[ro]))))
  out[is.na(d) | !nzchar(d)] <- NA_integer_
  out
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
  desg_match <- test_pattern_match(.h_master(h), "designation", "^B[wikvgnzj]")
  candidates <- desg_match$layers
  # Exclusoes
  # v0.9.241: item (a), "nao satisfazer aos requisitos estabelecidos para
  # caracterizar um horizonte B textural, B nitico, B espodico, B planico e
  # B latossolico": the B textural and the B latossolico of this system. The
  # WRB argic and ferralic horizons (30 cm, 16 cmolc) were tested.
  fer  <- B_latossolico(pedon)
  arg  <- B_textural(pedon)
  esp  <- spodic(pedon)
  plan <- planic_features(pedon)
  # v0.9.245: the vertic horizon of this system (20 cm, cracks of 1 cm, the
  # structure of the book), which "tem precedencia diagnostica sobre os
  # horizontes B incipiente, B nitico e glei" (p. 73); the WRB one was tested.
  ver  <- horizonte_vertico(pedon)
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
  excluded <- unique(c(if (isTRUE(fer$passed)) fer$layers else integer(0),
                        if (isTRUE(arg$passed)) arg$layers else integer(0),
                        esp$layers %||% integer(0),
                        plan$layers %||% integer(0),
                        if (isTRUE(ver$passed)) ver$layers else integer(0),
                        dur$layers %||% integer(0),
                        pet$layers %||% integer(0),
                        fra$layers %||% integer(0),
                        pli$layers %||% integer(0),
                        gle$layers %||% integer(0)))
  layers_ok <- setdiff(candidates, excluded)
  # v0.9.246: "deve ter no minimo 10 cm de espessura" is said of the horizon:
  # the thickness is that of its adjoining sub-horizons, as in the
  # B latossolico since v0.9.241.
  thick_test <- test_minimum_thickness(h, min_cm = min_thickness,
                                          candidate_layers = layers_ok,
                                          adjoining = TRUE)
  passed <- isTRUE(thick_test$passed) && length(thick_test$layers) > 0L
  DiagnosticResult$new(
    name = "B_incipiente", passed = passed,
    layers = thick_test$layers,
    evidence = list(
      designation = desg_match,
      thickness   = thick_test,
      excluded_by = list(B_latossolico = fer$layers, B_textural = arg$layers,
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
#'
#' The amount of cerosidade (\code{clay_films_amount}) and the type of
#' structure (\code{structure_type}) are read as the field sheet words them,
#' in English or in Portuguese, whatever the case and the accents: "Comum",
#' "Abundante" and "Prismatica que se desfaz" (written with its accent) of the
#' Brazilian surveys count as "common", "many" and "prismatic" do. Until v0.9.226 the amount was
#' compared with the literal English terms of \code{min_cerosidade}, so no
#' profile described in Portuguese could have a B nitico.
#'
#' The criteria of the B nitico are minima ("cerosidade em quantidade no
#' minimo comum e com grau forte ou moderado", "estrutura ... de grau de
#' desenvolvimento moderado ou forte"), and a range meets a minimum only if
#' all of it does: an amount or a grade written as a range is read by its
#' lower end. "Pouca a comum" is below "comum" and, since v0.9.231, "Fraca a
#' moderada" is below "moderada" for the cerosidade and for the structure.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Numeric threshold or option (see Details).
#' @param min_clay_pct Numeric threshold or option (see Details).
#' @param max_b_a_ratio Numeric threshold or option (see Details).
#' @param min_cerosidade Terms of the amount of cerosidade that satisfy
#'   criterion (c). The lowest of them on the scale none < few < common < many
#'   sets the threshold (the default is "common", the "comum" of SiBCS), and
#'   an amount of that class or above passes, in English or Portuguese. A
#'   term the scale does not know passes only if it is literally listed.
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
B_nitico <- function(pedon, min_thickness = 30, min_clay_pct = 35,
                        max_b_a_ratio = 1.5,
                        min_cerosidade = c("common","many","abundant","strong")) {
  h <- pedon$horizons
  # Step 1: textura argilosa em B
  b_layers <- which(!is.na(h$designation) &
                      grepl("^B", .desg_master(h$designation)))
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
  a_layers <- which(!is.na(h$designation) & grepl("^A", .desg_master(h$designation)))
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
  # v0.9.227: matched on the term without accents (the accented "Prismatica"
  # and "Poliedrica" of the Brazilian field sheets did not match).
  type_match <- !is.na(h$structure_type[clay_ok]) &
                  grepl(paste0("blocks|block|blocos|bloco|",
                                 "prismatic|prismatica|",
                                 "polyhedral|polyedric|poliedric"),
                          .field_term(h$structure_type[clay_ok]))
  # v0.9.231: the grade is read on the scale of .field_grade_class(), where a range
  # counts by its lower end: "Fraca a moderada" is not "moderado ou forte".
  # The pattern "moder|strong|forte" let it through.
  grade_ok   <- is.na(h$structure_grade[clay_ok]) |
                  .field_grade_class(h$structure_grade[clay_ok]) %in% 2:3
  struct_ok  <- any(type_match & grade_ok)
  # Step 4: cerosidade. SiBCS Cap 2 p.62 (c): quantity >= "comum" AND GRADE
  # >= moderate ("grau forte ou moderado") -- the critical discriminant vs
  # Latossolos (which carry at most "pouca e fraca" clay-skins). The prior test
  # checked only quantity; refine-when-present adds the grade gate via
  # clay_films_strength (NA strength -> byte-identical).
  # v0.9.227: the amount is read on the ordinal scale of .clay_films_class(),
  # which knows the Portuguese terms beside the English ones. The literal
  # comparison with `min_cerosidade` let only "common", "many", "abundant" and
  # "strong" through, so "Comum" and "Abundante" never passed.
  amt       <- h$clay_films_amount[clay_ok]
  amt_class <- .clay_films_class(.field_term(amt))
  min_class <- suppressWarnings(min(.clay_films_class(min_cerosidade),
                                      na.rm = TRUE))
  ceros_amount  <- !is.na(amt) &
                     (amt %in% min_cerosidade |
                        (is.finite(min_class) & !is.na(amt_class) &
                           amt_class >= min_class))
  ceros_grade   <- is.na(h$clay_films_strength[clay_ok]) |
                     .field_grade_class(h$clay_films_strength[clay_ok]) %in% 2:3
  cerosidade_ok <- any(ceros_amount & ceros_grade)
  # Step 5: thickness. SiBCS Cap 2 p.62 (a): >= 30 cm, EXCEPT >= 15 cm when a
  # lithic / lithic-fragmentary contact occurs within the first 50 cm.
  contact_shallow <- any(!is.na(h$designation) &
                           grepl("^[0-9]*(R|Cr|Cd)", .desg_master(h$designation)) &
                           !is.na(h$top_cm) & h$top_cm <= 50)
  eff_min_thick <- if (contact_shallow) 15 else min_thickness
  # v0.9.246: the thickness of the horizon, made of adjoining sub-horizons
  thk <- test_minimum_thickness(h, min_cm = eff_min_thick,
                                   candidate_layers = clay_ok,
                                   adjoining = TRUE)
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
#'
#' In the morphological mode the horizon is also read from its designation.
#' Since v0.9.247 that includes the "ir" of the older surveys (\code{Bir},
#' \code{B2ir}, \code{B22irx}), the illuvial iron that the present notation
#' writes "s": the mode read "h" and "s" only.
#'
#' Since v0.9.255 a horizon that the WRB test does not find is looked for by
#' item (a) of the book (p. 65), in both modes. The B espodico has an
#' "acumulacao iluvial de compostos organometalicos" and, by that item, "um
#' horizonte E (albico ou nao) sobrejacente e cores umidas de acordo com um
#' dos itens a seguir: 1) matiz 5YR ou mais vermelho; 2) matiz 7,5YR com valor
#' 5 ou menor e croma 4 ou menor; 3) matiz 10YR, com valor e croma 3 ou menor;
#' 4) cores neutras com valor 3 ou menor". A B layer of 2.5 cm or more passes
#' when:
#' \itemize{
#'   \item an E lies above it in the profile;
#'   \item its moist colour is one of the four;
#'   \item its organic carbon is 0.5\% or more and at least twice that of
#'         the nearest E above it; and
#'   \item no cerosidade is recorded in it.
#' }
#' The last two stand for the illuvial accumulation, which the sheets do not
#' measure where there is no oxalate extraction. The first takes the form the
#' book gives in item (b)(3) for aluminium and iron by oxalate, "valor 0,50 ou
#' maior, sendo este valor pelo menos o dobro do encontrado no horizonte
#' sobrejacente"; the second keeps out the B whose illuvial material is clay.
#' It is a reading by analogy, decided on 2026-10-13, and not the letter of
#' the book: the colours alone are those of the B textural of many an
#' Argissolo below an E. Items (b) and (c), the cementation and the ortstein,
#' are not tested.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param ... Reserved for future arguments.
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
B_espodico <- function(pedon, ...) {
  res <- spodic(pedon, ...)
  if (!isTRUE(res$passed)) {
    it <- .b_espodico_item_a(pedon$horizons)
    if (length(it$layers) > 0L) {
      ev <- res$evidence
      ev$item_a <- it
      res$evidence <- ev
      res$passed <- TRUE
      res$layers <- it$layers
    }
  }
  if (.morph_inference_enabled() && !isTRUE(res$passed)) {
    dm <- .desg_master(pedon$horizons$designation)
    ir <- which(!is.na(dm) & grepl("^B[^A-Z]*ir", dm))
    if (length(ir) > 0L) {
      ev <- res$evidence
      ev$morphological_inference <- list(
        passed = TRUE, layers = ir, diagnostic = "spodic",
        provenance = "morphological_designation",
        note = paste0("v0.9.247: the designation carries 'ir', the ",
                      "illuvial iron of the older notation"))
      res$evidence <- ev
      res$passed <- TRUE
      res$layers <- ir
    }
  }
  res$name      <- "B_espodico"
  res$reference <- "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 62-65"
  res
}


#' Item (a) of the B espodico (SiBCS Cap 2, p 65), with the illuvial
#' accumulation read from the organic carbon: the B layers, of 2.5 cm or more,
#' that lie below an E, have one of the moist colours of the item, hold 0.5\%
#' of organic carbon or more and at least twice that of the nearest E above
#' them, and have no cerosidade recorded. Returns the layers and, for each,
#' the E it was compared with and the two carbon contents.
#' @noRd
.b_espodico_item_a <- function(h) {
  none <- list(layers = integer(0), e_above = integer(0),
               oc_pct = numeric(0), oc_e_pct = numeric(0))
  dm <- .desg_master(h$designation)
  known <- !is.na(dm) & !is.na(h$top_cm)
  e <- which(known & grepl("^E", dm))
  b <- which(known & grepl("^B", dm) & !grepl("^B/?C", dm))
  if (length(e) == 0L || length(b) == 0L) return(none)
  out <- none
  for (i in b) {
    above <- e[h$top_cm[e] < h$top_cm[i]]
    if (length(above) == 0L) next
    if (is.na(h$bottom_cm[i]) || h$bottom_cm[i] - h$top_cm[i] < 2.5) next
    hp <- .munsell_hue_parts(h$munsell_hue_moist[i])
    if (!isTRUE(.b_espodico_colour_a(hp$number, hp$family,
                                     h$munsell_value_moist[i],
                                     h$munsell_chroma_moist[i]))) next
    up <- above[which.max(h$top_cm[above])]
    oc_b <- h$oc_pct[i]; oc_e <- h$oc_pct[up]
    if (is.na(oc_b) || is.na(oc_e)) next
    if (oc_b < 0.5 || oc_b < 2 * oc_e) next
    if (isTRUE(.amount_at_least(h$clay_films_amount[i], "few"))) next
    out$layers <- c(out$layers, i); out$e_above <- c(out$e_above, up)
    out$oc_pct <- c(out$oc_pct, oc_b); out$oc_e_pct <- c(out$oc_e_pct, oc_e)
  }
  out
}

#' The moist colours of item (a) of the B espodico (p. 65): hue 5YR or redder;
#' 7.5YR with value 5 or less and chroma 4 or less; 10YR with value and chroma
#' 3 or less; neutral with value 3 or less. A hue between two of them takes
#' the limits of the yellower one; one yellower than 10YR is none of them.
#' One colour at a time.
#' @noRd
.b_espodico_colour_a <- function(number, family, value, chroma) {
  if (is.na(family)) return(FALSE)
  if (family == "N") return(!is.na(value) && value <= 3)
  if (family == "R") return(TRUE)
  if (family != "YR" || is.na(number)) return(FALSE)
  if (number <= 5) return(TRUE)
  if (is.na(value) || is.na(chroma)) return(FALSE)
  if (number <= 7.5) return(value <= 5 && chroma <= 4)
  if (number <= 10) return(value <= 3 && chroma <= 3)
  FALSE
}


#' Horizonte B planico (SiBCS Cap 2, p 65-66)
#'
#' "E um tipo especial de horizonte B textural, com ou sem carater sodico,
#' subjacente a horizontes A ou E, apresentando mudanca textural abrupta ou
#' transicao abrupta associada a relacao textural com valor dentro do
#' especificado para o horizonte B textural (subitem h), porem calculado entre
#' o primeiro sub-horizonte B e o horizonte imediatamente acima (A ou E)".
#'
#' Since v0.9.244 the horizon is looked for where the book puts it, in the
#' first B sub-horizon, below an A or an E, by one of the two:
#' \itemize{
#'   \item a \code{\link{mudanca_textural_abrupta}} from the A or E to it; or
#'   \item a transition recorded as abrupt (\code{boundary_distinctness}) with
#'         a ratio of its clay to that of the horizon above over the limit of
#'         item (h) of the B textural (1.50, 1.70 or 1.80).
#' }
#' As a type of B textural, that sub-horizon has a texture francoarenosa or
#' finer (p. 54): one known to be coarser is no B planico, whatever its clay
#' over that of the E. The structure and the colours below are asked of that
#' first sub-horizon.
#' Until v0.9.243 any layer with a jump of clay over the layer above it could
#' be the B planico, the second way was not tested, and the structure and the
#' colours were asked of the layer with the jump. A profile with no horizon
#' designation at all is still tested that way.
#'
#' The structure (p. 65): "Apresenta estrutura prismatica, colunar ou em
#' blocos angulares e subangulares grandes ou medios e, as vezes, estrutura
#' macica". It is tested where the sheet records it
#' (\code{structure_type}, \code{structure_size}): a prismatic, columnar or
#' massive structure passes; blocks pass when medium or larger, or when
#' their size is not recorded; any other recorded structure does not pass.
#'
#' The colours "devem atender a pelo menos um dos seguintes requisitos"
#' (p. 65-66):
#' \itemize{
#'   \item (a) the matrix colour, with or without mottles: "Matiz 10YR ou mais
#'         amarelo, cromas <= 3" or "Matizes 7,5YR ou 5YR, cromas <= 2";
#'   \item (b) "Coloracao variegada com pelo menos uma cor apresentando matiz
#'         e croma conforme especificado no item (a)" (\code{variegated_colors});
#'   \item (c) "matiz 10YR ou mais amarelo, cromas >= 4, combinado com
#'         mosqueado de croma conforme especificado no item (a)": a mottle
#'         colour (\code{mottle_colors}) that satisfies item (a).
#' }
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
B_planico <- function(pedon) {
  h <- pedon$horizons
  abrupt <- mudanca_textural_abrupta(pedon)
  described <- any(!is.na(h$designation) &
                     nzchar(trimws(as.character(h$designation))))
  ab <- if (described) .mudanca_abrupta_a_b(h) else NULL
  b_layers <- which(!is.na(h$designation) & grepl("^B", .desg_master(h$designation)))
  cor_ok <- integer(0)
  cor_item <- character(0)
  for (i in b_layers) {
    # v0.9.233: the hue is read as a number and a family: the pattern
    # "10YR|2\\.5Y|5Y" also matched "5YR", "7.5YR" and "2.5YR", which all
    # contain "5Y", so that those hues passed with chroma 3.
    hue <- .munsell_hue_parts(h$munsell_hue_moist[i])
    cm  <- h$munsell_chroma_moist[i]
    item <- NA_character_
    if (isTRUE(.b_planico_item_a(hue$number, hue$family, cm))) {
      item <- "a"
    } else {
      # v0.9.238: items (b) and (c), which need the colours a horizon has
      # beside its matrix colour.
      vc <- .munsell_notations(h$variegated_colors[i])
      mc <- .munsell_notations(h$mottle_colors[i])
      yellow4 <- !is.na(hue$family) && !is.na(cm) && cm >= 4 &&
        ((hue$family == "YR" && hue$number >= 10) || hue$family == "Y")
      if (nrow(vc) > 0L &&
            any(.b_planico_item_a(vc$number, vc$family, vc$chroma))) {
        item <- "b"
      } else if (yellow4 && nrow(mc) > 0L &&
                   any(.b_planico_item_a(mc$number, mc$family, mc$chroma))) {
        item <- "c"
      }
    }
    if (!is.na(item)) {
      cor_ok <- c(cor_ok, i)
      cor_item <- c(cor_item, item)
    }
  }
  # v0.9.238: the structure the definition describes, where it is recorded
  estrutura_ok <- b_layers[.b_planico_structure_ok(h$structure_type[b_layers],
                                                    h$structure_size[b_layers])]
  texture_ok <- NA
  if (!described) {
    layers_pass <- intersect(intersect(abrupt$layers %||% integer(0), cor_ok),
                             estrutura_ok)
    by_ratio <- FALSE
  } else {
    # the first B sub-horizon, below an A or an E: an abrupt textural change,
    # or an abrupt transition with the textural ratio of item (h)
    by_ratio <- isTRUE(ab$abrupt_transition) && is.finite(ab$ratio) &&
      ab$ratio > ab$ratio_limit
    # "um tipo especial de horizonte B textural", which has a texture
    # francoarenosa or finer: the sandy Bh below the E of an Espodossolo,
    # 4% of clay over 1%, has twice the clay and is none
    if (!is.na(ab$first_b))
      texture_ok <- !isFALSE(.francoarenosa_or_finer(h$silt_pct[ab$first_b],
                                                     h$clay_pct[ab$first_b]))
    first_b <- if (!is.na(ab$above) && texture_ok &&
                     (isTRUE(ab$passed) || by_ratio))
      ab$first_b else integer(0)
    layers_pass <- intersect(intersect(first_b, cor_ok), estrutura_ok)
  }
  passed <- length(layers_pass) > 0L
  DiagnosticResult$new(
    name = "B_planico", passed = passed,
    layers = layers_pass,
    evidence = list(abrupt = abrupt, transition = ab,
                     abrupt_transition_with_ratio = by_ratio,
                     texture_ok = texture_ok,
                     cor_ok = cor_ok, cor_item = cor_item,
                     estrutura_ok = estrutura_ok),
    missing = abrupt$missing,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 65-66"
  )
}


#' Whether a recorded structure is one the definition of the B planico
#' describes (SiBCS 2018, Cap 2, p. 65): "prismatica, colunar ou em blocos
#' angulares e subangulares grandes ou medios e, as vezes, estrutura macica".
#' English and Portuguese terms. A structure that is not recorded passes, and
#' so do blocks whose size is not recorded. Vectorised.
#' @noRd
.b_planico_structure_ok <- function(type, size) {
  t <- .field_term(type)
  k <- .structure_size_class(size)
  unrecorded <- is.na(t) | !nzchar(t)
  shaped <- grepl("prismat|colun|column|macic|massive", t)
  blocks <- grepl("bloco|blocky", t) & (is.na(k) | k >= 3L)
  unrecorded | shaped | blocks
}


#' Item (a) of the colours of the B planico (SiBCS 2018, Cap 2, p. 65-66), for
#' a colour given by the number and the family of its hue and by its chroma:
#' "Matiz 10YR ou mais amarelo, cromas <= 3" or "Matizes 7,5YR ou 5YR, cromas
#' <= 2". Neutral and gley hues are not among them. Vectorised; a colour that
#' is not recorded does not satisfy.
#' @noRd
.b_planico_item_a <- function(number, family, chroma) {
  known <- !is.na(family) & !is.na(chroma)
  yellow <- known & ((family == "YR" & !is.na(number) & number >= 10) | family == "Y")
  brown  <- known & family == "YR" & !is.na(number) & number >= 5 & number < 10
  (yellow & chroma <= 3) | (brown & chroma <= 2)
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
  # v0.9.234: the Brazilian surveys before the present notation wrote
  # plinthite as "pl" (B21pl, Cpl) where "f" is written today (Btf). The
  # morphological mode reads the "f" in plinthic(); here it reads the "pl"
  # too, with the same thickness.
  # v0.9.248: and the "p1" that stands for it in B and C horizons (B2tp1,
  # Cp1, B2tp1cn), the letter "l" written as the figure 1. It is read so
  # unless the profile numbers such horizons on ("p2"), in which case the
  # "p1" is the first of a series. "Ap1" is a ploughed layer.
  if (.morph_inference_enabled() && !isTRUE(res$passed)) {
    h  <- pedon$horizons
    d  <- .desg_master(h$designation)
    numbered <- any(!is.na(d) & grepl("^[BC][a-z0-9]*p[2-9]", d))
    pl <- which(!is.na(d) & (grepl("^[A-Z][a-z0-9]*pl", d) |
                               (!numbered & grepl("^[BC][a-z0-9]*p1", d))))
    thk <- sum(pmax(h$bottom_cm[pl] - h$top_cm[pl], 0), na.rm = TRUE)
    if (length(pl) > 0L && thk >= min_thickness) {
      res$passed <- TRUE
      res$layers <- pl
      ev <- res$evidence
      ev$designation_inference <- list(
        passed = TRUE, layers = pl, source = "designation_pl",
        details = list(matched_designations = as.character(h$designation[pl]),
                       total_thickness_cm = thk, min_thickness = min_thickness))
      res$evidence <- ev
    }
  }
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
  desg <- test_pattern_match(.h_master(h), "designation", "^[ABCE]c|c[g]?[fr]?$")
  pp <- test_numeric_above(h, "plinthite_pct",
                              threshold = min_petroplinthite_pct,
                              candidate_layers = desg$layers)
  # v0.9.246: "deve apresentar no minimo 30 cm de espessura", of adjoining
  # sub-horizons
  thk <- test_minimum_thickness(h, min_cm = min_thickness,
                                   candidate_layers = pp$layers,
                                   adjoining = TRUE)
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


#' Horizonte glei (SiBCS Cap 2, p 69-71)
#'
#' "Em sintese, o horizonte glei e um horizonte mineral, com espessura minima
#' de 15 cm, com menos de 15% de plintita e e saturado com agua por influencia
#' do lencol freatico durante algum periodo ou o ano todo [...], apresentando
#' evidencias de processos de reducao, com ou sem segregacao de ferro,
#' caracterizadas por um ou mais dos seguintes requisitos" (p. 70-71):
#' \itemize{
#'   \item (a) the dominant colour, moist: 1) neutral, or bluer than 10Y; 2)
#'         hues redder than 5YR with value 4 or more, chroma 1 or less; 3)
#'         hues 5YR or yellower with value 4 or more, chroma 2 or less, or 3
#'         in a hue 10YR or yellower when the chroma decreases in the horizon
#'         below; 4) any hue and value, chroma 2 or less, with mottles;
#'   \item (b) "Coloracao variegada com pelo menos uma das cores de acordo com
#'         um dos itens anteriores" (\code{variegated_colors});
#'   \item (c) reduced iron shown by a field test, which no profile records.
#' }
#'
#' The saturation is not measured. What a survey records of it is the
#' drainage class of the site (\code{site$drainage}), and Anexo B of the book
#' (p. 315) describes the classes: in a "mal drenado" soil "o lencol freatico
#' comumente esta a superficie ou proximo dela durante uma consideravel parte
#' do ano", in a "muito mal drenado" one "durante a maior parte do ano", while
#' an "imperfeitamente drenado" soil shows "indicios de gleizacao nos seus
#' horizontes mais profundos". The colours count as a glei horizon when the
#' site is "mal drenado" or "muito mal drenado". A class written as a range
#' counts by its wetter end ("imperfeitamente a mal drenado" is "mal
#' drenado").
#'
#' When the drainage class is not recorded, the sign of the saturation is
#' asked of the layer itself, beside its colours: mottles ("O horizonte sendo
#' saturado com agua periodicamente ou o solo tendo sido drenado deve
#' apresentar algum mosqueado", p. 70), a neutral, bluish or greenish colour
#' (item a1), or a variegated colour (item b). Without it, a greyish A or a
#' pale E of any soil would satisfy item (a3).
#'
#' In the morphological mode the "g" of a horizon designation (Cg, Btg) is the
#' surveyor's record of the gleying and stands for the colours and for the
#' saturation.
#'
#' Until v0.9.238 this was the WRB test of gleyic properties: redoximorphic
#' features of 5% or more in a layer that starts within 50 cm, with no colour
#' asked.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Minimum thickness of the glei horizon, in cm (default
#'   15); since v0.9.246 that of its adjoining layers.
#' @noRd
horizonte_glei <- function(pedon, min_thickness = 15) {
  h <- pedon$horizons
  colour <- .glei_colour_layers(pedon)
  term <- pedon$site[["drainage"]] %||% pedon$site[["drainage_class"]] %||%
    NA_character_
  drainage <- .drainage_class(term, end = "wetter")
  saturated <- if (is.na(drainage)) NA else drainage >= 7L
  layers <- if (isTRUE(saturated)) {
    colour$layers
  } else if (is.na(saturated)) {
    colour$layers[colour$mottled | colour$items %in% c("a1", "b")]
  } else {
    integer(0)
  }
  by_letter <- integer(0)
  if (.morph_inference_enabled()) {
    d <- .desg_master(h$designation)
    by_letter <- which(!is.na(d) & grepl("^[A-Z][a-z0-9]*g", d))
    layers <- union(layers, by_letter)
  }
  # v0.9.246: "com espessura de 15 cm ou mais", of adjoining sub-horizons
  thk <- test_minimum_thickness(h, min_cm = min_thickness,
                                candidate_layers = sort(layers),
                                adjoining = TRUE)
  no_colour <- nrow(h) > 0L && all(is.na(h$munsell_hue_moist)) &&
    all(is.na(h$variegated_colors))
  DiagnosticResult$new(
    name = "horizonte_glei", passed = isTRUE(thk$passed),
    layers = thk$layers,
    evidence = list(colour_layers = colour$layers, colour_items = colour$items,
                     drainage = list(recorded = term, class = drainage,
                                     saturated = saturated),
                     designation_g = by_letter, thickness = thk),
    missing = if (no_colour) "munsell_hue_moist" else character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 69-71"
  )
}


#' Which item of requirement (a) of the glei horizon a colour satisfies (SiBCS
#' 2018, Cap 2, p. 70-71), "a1" to "a4", or NA. The colour is given by the
#' number and the family of its hue, its value and its chroma; `mottled` says
#' whether the horizon has mottles (item 4), and `chroma_below` is the chroma
#' of the horizon below (the chroma 3 that item 3 admits "devera diminuir no
#' horizonte subjacente").
#' @noRd
.glei_colour_item <- function(number, family, value, chroma,
                              mottled = FALSE, chroma_below = NA_real_) {
  if (is.na(family)) return(NA_character_)
  if (family %in% c("N", "GY", "G", "BG", "B", "PB")) return("a1")
  if (is.na(chroma)) return(NA_character_)
  redder <- family %in% c("R", "RP") ||
    (family == "YR" && !is.na(number) && number < 5)
  yellower <- (family == "YR" && !is.na(number) && number >= 5) || family == "Y"
  light <- !is.na(value) && value >= 4
  if (redder && light && chroma <= 1) return("a2")
  if (yellower && light && chroma <= 2) return("a3")
  yellow10 <- (family == "YR" && !is.na(number) && number >= 10) || family == "Y"
  if (yellow10 && light && chroma <= 3 &&
        !is.na(chroma_below) && chroma_below < chroma) return("a3")
  if (chroma <= 2 && isTRUE(mottled)) return("a4")
  NA_character_
}


#' The mineral layers with less than 15% of plinthite whose colours satisfy
#' requirement (a) or (b) of the glei horizon, the item each one satisfies
#' ("a1" to "a4", or "b" for a variegated layer) and whether it has mottles
#' recorded (an amount of redoximorphic features, or the colours of mottles).
#' @noRd
.glei_colour_layers <- function(pedon) {
  h <- pedon$horizons
  n <- nrow(h)
  if (n == 0L) return(list(layers = integer(0), items = character(0),
                           mottled = logical(0)))
  o <- order(h$top_cm)
  d <- .desg_master(h$designation)
  item <- rep(NA_character_, n)
  has_mottles <- (!is.na(h$redoximorphic_features_pct) &
                    h$redoximorphic_features_pct > 0) | !is.na(h$mottle_colors)
  for (k in seq_len(n)) {
    i <- o[k]
    if (!is.na(h$plinthite_pct[i]) && h$plinthite_pct[i] >= 15) next
    if (!is.na(d[i]) && grepl("^[OH]", d[i])) next
    mottled <- has_mottles[i]
    below <- if (k < n) h$munsell_chroma_moist[o[k + 1L]] else NA_real_
    hue <- .munsell_hue_parts(h$munsell_hue_moist[i])
    item[i] <- .glei_colour_item(hue$number, hue$family,
                                 h$munsell_value_moist[i],
                                 h$munsell_chroma_moist[i], mottled, below)
    if (is.na(item[i])) {
      vc <- .munsell_notations(h$variegated_colors[i])
      for (r in seq_len(nrow(vc))) {
        if (!is.na(.glei_colour_item(vc$number[r], vc$family[r], vc$value[r],
                                     vc$chroma[r]))) { item[i] <- "b"; break }
      }
    }
  }
  ok <- which(!is.na(item))
  list(layers = ok, items = item[ok], mottled = has_mottles[ok])
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
  # v0.9.246: the 15 cm are those of the horizon, of adjoining sub-horizons,
  # on this path and on the jarosite path below
  res <- thionic(pedon, max_pH = 3.5, adjoining = TRUE, ...)
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
      thk <- test_minimum_thickness(h, min_cm = 15, candidate_layers = cand,
                                    adjoining = TRUE)
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
#'
#' Since v0.9.245 the horizon also passes by the features of the book that
#' \code{vertic_horizon()} does not read
#' (\code{.horizonte_vertico_sibcs()}): wedge-shaped or parallelepipedic
#' structural units in place of the slickensides ("e/ou"), and 20 cm made
#' of adjoining layers.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param ... Reserved for future arguments.
#' @noRd
horizonte_vertico <- function(pedon, ...) {
  # v0.9.137: SiBCS Cap 2 p.73 requires cracks ">= 1 cm" wide (vs the WRB/USDA
  # 0.5 cm). Pass min_crack_width_cm = 1.0 so the SiBCS vertico is stricter on
  # the field-crack path; the COLE and 'v'-designation paths are unaffected, so
  # a Vertissolo recorded via COLE or a v-modifier designation still passes.
  # v0.9.234: in the Brazilian notation the suffix "v" marks vertic features
  # (Bv, Cv, Biv) and the morphological mode reads it. vertic_horizon() keeps
  # that reading behind an option of its own because it serves WRB and USDA
  # too, where "v" is plinthite (KST, FAO); it is turned on here, for SiBCS.
  if (.morph_inference_enabled()) {
    old <- options(soilKey.vertic_designation_inference = TRUE)
    on.exit(options(old), add = TRUE)
  }
  res <- vertic_horizon(pedon, min_thickness = 20, min_crack_width_cm = 1.0, ...)
  # v0.9.245: the features of the book. vertic_horizon(), which serves WRB
  # and USDA too, asks for slickensides and 20 cm of one layer; the book
  # (p. 73) gives "superficies de friccao (slickensides) em quantidade no
  # minimo comum, e/ou unidades estruturais cuneiformes e/ou
  # paralelepipedicas", and its 20 cm are those of the horizon.
  own <- .horizonte_vertico_sibcs(pedon)
  if (isTRUE(own$passed)) {
    res$layers <- if (isTRUE(res$passed))
      sort(unique(c(res$layers, own$layers))) else own$layers
    res$passed <- TRUE
  }
  ev <- res$evidence
  ev$sibcs <- own
  res$evidence <- ev
  res$name <- "horizonte_vertico"
  res$reference <- "Embrapa (2018), SiBCS 5a ed., Cap 2, p. 73"
  res
}


#' The vertic horizon by the features of SiBCS 2018 (Cap 2, p. 73): layers
#' with 300 g/kg of clay or more that have slickensides in amount of common
#' or more, and/or wedge-shaped or parallelepipedic structural units, with
#' cracks of 1 cm where their width is recorded, and 20 cm of adjoining
#' layers. Returns a list (\code{passed}, \code{layers}, and the layers with
#' each feature).
#' @noRd
.horizonte_vertico_sibcs <- function(pedon, min_clay = 30, min_thickness = 20,
                                      min_crack_width_cm = 1) {
  h <- pedon$horizons
  clay  <- !is.na(h$clay_pct) & h$clay_pct >= min_clay
  slick <- .amount_at_least(h$slickensides, c("common", "many", "continuous"))
  st    <- .field_term(h$structure_type)
  wedge <- !is.na(st) & grepl("cuneiform|paralelepip|parallelepiped|wedge", st)
  cracks_ok <- is.na(h$cracks_width_cm) | h$cracks_width_cm >= min_crack_width_cm
  cand <- which(clay & (slick | wedge) & cracks_ok)
  thk <- test_minimum_thickness(h, min_cm = min_thickness,
                                candidate_layers = cand, adjoining = TRUE)
  list(passed = isTRUE(thk$passed), layers = thk$layers %||% integer(0),
       slickensides = which(slick), structure = which(wedge), thickness = thk)
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
