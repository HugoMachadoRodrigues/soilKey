# =============================================================================
# v0.9.316 -- shared readings for WRB 2022 Chapter 5 qualifiers read again from
# the text: Calcaric, Skeletic, Geric, Aric, Sodic, Loamic, Siltic, Brunic and
# Pellic (the qualifier functions stay in their files).
# =============================================================================

#' Calcaric material per layer, TRUE / FALSE / NA. WRB 2022 Ch 3.3.3 (p. 86):
#' the material "contains >= 2% calcium carbonate equivalent" and "shows
#' visible effervescence with 1 M HCl throughout the fine earth". The
#' laboratory value decides when it is recorded (caco3_pct >= 2); otherwise
#' the field reaction (effervescence_class): visible effervescence is
#' moderately, strongly or extremely calcareous, while none and slight
#' (audible, not visible) are not (Annex 1, Table 8.62, p. 200).
#' @noRd
.wrb_calcaric_status <- function(h) {
  n <- nrow(h)
  ca <- h$caco3_pct %||% rep(NA_real_, n)
  eff <- .field_term(h$effervescence_class %||% rep(NA_character_, n))
  eff_no <- !is.na(eff) & (grepl("slight", eff) | grepl("^(non|none|nil|no)\\b", eff))
  eff_yes <- !is.na(eff) & !eff_no & grepl("moderat|strong|high|extrem|very", eff)
  ifelse(!is.na(ca), ca >= 2, ifelse(eff_yes, TRUE, ifelse(eff_no, FALSE, NA)))
}

#' The lower boundary of a layer is abrupt or very abrupt, TRUE / FALSE / NA.
#' WRB 2022 Annex 1, Table 8.30 (p. 176): very abrupt <= 0.5 cm, abrupt
#' > 0.5-2 cm; clear, gradual and diffuse are wider. The Portuguese terms of
#' BDsolos (abrupta, clara, gradual, difusa) are read too.
#' @noRd
.wrb_abrupt_boundary <- function(x) {
  t <- .field_term(x)
  out <- rep(NA, length(t))
  out[!is.na(t) & grepl("clear|clara|gradual|diffuse|difus", t)] <- FALSE
  out[!is.na(t) & grepl("abrupt|sharp", t)] <- TRUE
  out
}

#' Horizons whose designation records ploughing, TRUE / FALSE / NA (no
#' designation): a "p" suffix ("Ap", "Ap1", "Apk", "A(p)", "Ap/C", the older
#' "A11p"; FAO 2006 Guidelines and KST 13). The "pl" of the plinthite in the
#' older Brazilian notation ("Apl", "B1pl") is not a p suffix.
#' @noRd
.wrb_plough_designation <- function(d) {
  m <- .desg_master(d)
  ifelse(is.na(m) | !nzchar(trimws(m)), NA,
         grepl("^[A-Z]+[^A-Z/]*p(?!l)", m, perl = TRUE))
}

#' Texture class of each layer by the triangle of WRB 2022 Annex 1 (Table 8.39,
#' p. 182, Soil Science Division Staff 2017): the USDA classes of
#' texture_class_from_pct() ("S", "LS", "SL", "L", "SIL", "SI", "SCL", "CL",
#' "SICL", "SC", "SIC", "C"). Silt is taken as 100 - sand - clay when it is not
#' recorded (and sand likewise). NA where clay and one of silt and sand are not
#' recorded. Until v0.9.321 clay 35 to < 40% with > 45% sand was set to "SC"
#' here, as texture_class_from_pct() read it as sandy clay loam; the function
#' follows Table 8.39 at every boundary since v0.9.322.
#' @noRd
.wrb_texture_class <- function(h) {
  cl <- h$clay_pct; si <- h$silt_pct; sa <- h$sand_pct
  si <- ifelse(is.na(si) & !is.na(sa) & !is.na(cl), 100 - sa - cl, si)
  sa <- ifelse(is.na(sa) & !is.na(si) & !is.na(cl), 100 - si - cl, sa)
  texture_class_from_pct(cl, si, sa)
}

#' A texture qualifier of WRB 2022 Ch 5 (Loamic p. 140, Siltic p. 146): mineral
#' material with, "single or in combination", a texture class of `classes`
#' "in one or more layers with a combined thickness of >= 30 cm, occurring
#' within 100 cm of the mineral soil surface, or in the major part between the
#' mineral soil surface and a limiting layer starting > 10 and < 60 cm from
#' the mineral soil surface". Layers of unknown class may be either; organic
#' layers (.wrb_mineral_layer()) are not.
#' @noRd
.q_texture_qualifier <- function(pedon, name, classes, page) {
  h <- pedon$horizons
  ref <- sprintf("WRB (2022) Ch 5, %s (p. %s); texture classes, Annex 1 Table 8.39 (p. 182)",
                 name, page)
  ms <- .q_mineral_surface_cm(h)
  if (is.na(ms))
    return(DiagnosticResult$new(name = name, passed = NA, layers = integer(0),
                                evidence = list(), missing = "top_cm", reference = ref))
  tex <- .wrb_texture_class(h)
  st <- ifelse(is.na(tex), NA, tex %in% classes)
  # clay >= 40 % is a clay class whatever the silt and sand
  st[is.na(st) & !is.na(h$clay_pct) & h$clay_pct >= 40] <- FALSE
  st[!.wrb_mineral_layer(h)] <- FALSE
  a <- .q_thickness_rule(h, st, 30, win_top = ms, win_bot = ms + 100)
  lim <- tryCatch(.barrier_top_cm(pedon), error = function(e) NA_real_)
  b <- if (is.na(lim) || lim - ms <= 10 || lim - ms >= 60) FALSE else {
    yes <- .q_layers_thickness(h, which(st %in% TRUE), ms, lim)
    may <- .q_layers_thickness(h, which(!(st %in% FALSE)), ms, lim)
    if (yes > (lim - ms) / 2) TRUE else if (may <= (lim - ms) / 2) FALSE else NA
  }
  x <- c(a$passed, b)
  passed <- if (any(x %in% TRUE)) TRUE else if (anyNA(x)) NA else FALSE
  win <- !is.na(h$top_cm) & !is.na(h$bottom_cm) & h$bottom_cm > ms & h$top_cm < ms + 100
  DiagnosticResult$new(
    name = name, passed = passed,
    layers = if (isTRUE(passed)) which(st %in% TRUE & win) else integer(0),
    evidence = list(texture_class = tex, thickness_cm = a$thickness_cm,
                    mineral_surface_cm = ms, limiting_layer_cm = lim),
    missing = if (is.na(passed)) c("clay_pct", "silt_pct", "sand_pct") else character(0),
    reference = ref)
}
