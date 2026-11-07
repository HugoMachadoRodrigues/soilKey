# =============================================================================
# v0.9.376 -- the subgroups of orders H to N of the Chinese Soil Taxonomy
# (Cooperative Research Group on Chinese Soil Taxonomy, ISSAS. Chinese Soil
# Taxonomy. Science Press, Beijing and New York, 2001; the English edition,
# system id cst2001): the criteria the keys to subgroups of the Halosols,
# Gleyosols, Isohumosols, Ferrosols, Argosols, Cambosols and Primosols (key
# 5.2, pp. 108-180) define where they use them, and the examples of those
# subgroups. The keys are inst/rules/cst2001/subgroups/{H..N}.yaml; most rows
# call a preset of inst/rules/cst2001/presets.yaml (expanded at load time,
# R/rule-presets.R) whose template calls one of the criteria below, of
# R/key-cst-subgroups.R (part 0.9.375) or of the earlier parts.
#
# Written here, three-valued like the rest (a measurement not recorded leaves
# the test undecided, a field observation not recorded is absent):
#   * the albic horizon, or a glossic horizon or glossic evidence, anywhere,
#     within a depth, below the mollic epipedon or above the argic horizon
#     (Albic, Mottli-albic, Glossi-para-alkalic);
#   * the calcic horizon alone, with soft powdery lime, or with the
#     hypercalcic horizon and the calcipan, with its upper boundary within
#     (or between) depths (Calcic);
#   * a per-layer test (base saturation and pH, pH alone, the ferric
#     property) throughout a window, the B, the argic horizon or the claypan,
#     in a run of a thickness or in a share of the window (Dystric, Eutric,
#     Leachic, Ferric);
#   * a feature in a share of the window, or in every layer of it, or in a
#     window below the top of a horizon (Phosphic, Epigleyic, Gleyic of the
#     Albi-Boric Argosols);
#   * salic evidence with its duration (Parasalic of M2.2.3 and M3.3.1),
#     gleyic evidence of perched water (Stagnic), vertic evidence within a
#     depth (Verti-lithic), the kind of the histic epipedon (Fibric, Sapric),
#     an indurated nodular subhorizon (Concretic), cinders (Cinderic),
#     carbon in the upper 20 cm (Grayic), and the torrential and marine salt
#     profiles of the Halosols (Dilluvic, Marinic).
# The readings where the text is silent are in
# inst/rules/cst2001/readings.yaml (part 0.9.376).
# =============================================================================

# A number of the subgroup thresholds (thresholds.yaml, key: subgroups).
.cst_sgthr <- function(...) .cst_thr("key", "subgroups", ...)

# The vectorised three-valued OR is .cst_or_v() of R/diagnostics-cst-horizons.R.

# The per-layer reading of several features read together (any of them),
# rock carrying none.
.cst_sg_or_layers <- function(x, features, grade = c("full", "at_least_evidence")) {
  grade <- match.arg(grade)
  ps <- lapply(as.character(features), function(f) {
    p <- .cst_sg_feature(x, f, grade)$full
    p[x$kind %in% "R"] <- FALSE
    if (f %in% c("gleyic_features", "redoxic_features")) p[x$kind %in% "O"] <- FALSE
    p
  })
  Reduce(.cst_or_v, ps)
}

# Three-valued "some layer passes" over the per-layer q.
.cst_any3 <- function(q) if (any(q %in% TRUE)) TRUE else if (any(is.na(q))) NA else FALSE


# --- the Albic subgroups --------------------------------------------------------------------------

#' The albic horizon (`features` "albic"), the albic horizon or a glossic
#' horizon or glossic evidence ("albic_glossic"), or a glossic horizon or
#' glossic evidence alone ("glossic"; Glossi-para-alkalic, J2.4.3, J2.5.2), in
#' some layer: `position` "anywhere" (M1.5.4 ...), "below_mollic" (its top at
#' or below the base of the mollic epipedon, J3.1.1, J3.2.1) or "above_argic"
#' (an albic layer whose top is above the top of the argic horizon, a glossic
#' one whose top is at it or above, L1.2.3 ...); `within` limits the top of
#' the layer to that depth (M4.1.3: 125 cm; M4.2.1: the albic horizon within
#' 50 cm).
#' @noRd
albic_sg_cst <- function(pedon, features = c("albic_glossic", "albic", "glossic"),
                         position = c("anywhere", "below_mollic", "above_argic"),
                         within = NULL, datum = c("mineral", "surface")) {
  features <- match.arg(features); position <- match.arg(position); datum <- match.arg(datum)
  nm <- "albic_sg_cst"
  x <- .cst_ctx(pedon)
  if (!x$n) return(.cst_key_result(nm, FALSE))
  al <- if (features != "glossic") .cst_hz(x, "albic_horizon")$full else rep(FALSE, x$n)
  gl <- if (features != "albic") .cst_hz(x, "glossic_horizon")$evid else rep(FALSE, x$n)
  p <- .cst_or_v(al, gl)
  p[x$kind %in% "R"] <- FALSE
  if (!is.null(within)) {
    s <- .cst_sg_datum(x, datum)
    if (is.na(s)) return(.cst_key_result(nm, NA, missing = c("designation", "oc_pct")))
    p[is.na(x$top) | x$top - s > within] <- FALSE
  }
  miss <- c(.cst_hz(x, "albic_horizon")$missing, if (features != "albic") .cst_hz(x, "glossic_horizon")$missing)
  ok <- rep(TRUE, x$n)
  if (position != "anywhere") {
    ref <- if (position == "below_mollic") "mollic_epipedon" else "argic_horizon"
    h <- .cst_hz(x, ref)$full
    k <- which(h %in% TRUE)
    if (!length(k)) {
      return(.cst_key_result(nm, if (any(is.na(h)) && any(!(p %in% FALSE))) NA else FALSE,
                             missing = c(miss, .cst_hz(x, ref)$missing),
                             notes = sprintf("no %s", gsub("_", " ", ref))))
    }
    if (position == "below_mollic") {
      base <- max(x$bot[k], na.rm = TRUE)
      ok <- !is.na(x$top) & x$top >= base - 1e-9
    } else {
      ta <- x$top[k[1L]]
      ok <- !is.na(x$top) & ifelse(al %in% TRUE, x$top < ta, x$top <= ta)
    }
  }
  q <- ifelse(ok, p, FALSE)
  passed <- .cst_any3(q)
  .cst_key_result(nm, passed, layers = x$idx[which(q %in% TRUE)], missing = miss,
                  notes = sprintf("%s %s%s", switch(features, albic = "albic horizon",
                                                    albic_glossic = "albic horizon, glossic horizon or glossic evidence",
                                                    glossic = "glossic horizon or glossic evidence"),
                                  switch(position, anywhere = "", below_mollic = "below the mollic epipedon",
                                         above_argic = "above the argic horizon"),
                                  if (is.null(within)) "" else sprintf(" within %s cm", format(within))),
                  page = "122-167")
}


# --- the Calcic subgroups -------------------------------------------------------------------------

#' The calcic horizon and its kin with the upper boundary within `top_max` cm
#' (and at `top_min` cm or deeper when given) of the mineral soil surface:
#' `kinds` any of "calcic_only" (the calcic horizon named alone: a
#' hypercalcic horizon does not count, J2.1.2, M1.3.1, L1.2.2), "soft_lime"
#' (a horizon with soft powdery lime concentrations, J2.3.5: calcic evidence,
#' the calcic horizon passing too, or soft powdery secondary carbonates
#' recorded in the field, flagged, C17), "hypercalcic_horizon" and
#' "calcipan" (M3.1.2, M3.4.1). The window below the base of the description
#' is undecided unless the description ends on rock or a contact.
#' @noRd
calcic_sg_cst <- function(pedon, kinds = "calcic_only", top_min = NULL, top_max = 100,
                          datum = c("mineral", "surface")) {
  datum <- match.arg(datum)
  nm <- "calcic_sg_cst"
  x <- .cst_ctx(pedon)
  if (!x$n) return(.cst_key_result(nm, FALSE, page = "118-156"))
  s <- .cst_sg_datum(x, datum)
  if (is.na(s)) return(.cst_key_result(nm, NA, missing = c("designation", "oc_pct")))
  co <- .cst_calcic_only(x)
  flags <- character(0)
  one <- function(k) switch(k,
    calcic_only = co$full,
    soft_lime = {
      form <- .cst_t(x, "secondary_carbonates_form")
      pw <- !is.na(form) & grepl("powder", form)
      ev <- co$evid
      if (any(pw & !(ev %in% TRUE))) flags <<- c(flags, "soft_lime_from_powdery_carbonates")
      ifelse(pw, TRUE, ev)
    },
    hypercalcic_horizon = .cst_hz(x, "hypercalcic_horizon")$full,
    calcipan = .cst_hz(x, "calcipan")$full,
    rlang::abort(sprintf("Unknown calcic kind '%s'.", k)))
  p <- Reduce(.cst_or_v, lapply(as.character(kinds), one))
  p[x$kind %in% "R"] <- FALSE
  d <- x$top - s
  lo <- if (is.null(top_min)) -Inf else top_min
  inside <- !is.na(d) & d >= lo - 1e-9 & d <= top_max + 1e-9
  # the upper boundary: the first layer of a run (a layer under one that
  # surely or maybe belongs continues it)
  above <- c(FALSE, (p %in% TRUE)[-x$n])
  above_na <- c(FALSE, is.na(p)[-x$n])
  start_ok <- if (is.null(top_min)) rep(TRUE, x$n) else !above & !above_na
  sure <- p %in% TRUE & inside & start_ok
  may <- !(p %in% FALSE) & inside & (if (is.null(top_min)) TRUE else !above)
  gap <- .cst_sg_gap(x, s + max(0, lo), s + top_max)
  passed <- if (any(sure)) TRUE else if (any(may) || gap > 0) NA else FALSE
  .cst_key_result(nm, passed, layers = x$idx[which(sure)], flags = if (isTRUE(passed)) flags,
                  missing = c(co$missing, if (gap > 0) "bottom_cm"),
                  notes = sprintf("%s with the upper boundary %s%s cm of the %s", paste(gsub("_", " ", kinds), collapse = " or "),
                                  if (is.null(top_min)) "within " else sprintf("between %s and ", format(top_min)),
                                  format(top_max), if (datum == "mineral") "mineral soil surface" else "soil surface"),
                  page = "118-156")
}


# --- per-layer tests in a window, the B, the argic horizon or the claypan ---------------------------

# The per-layer tests of layer_test_cst(): "acid_or" (base saturation under
# 50 % or pH under 5.5, Dystric), "eutric_or" (base saturation 50 % or more
# or pH 5.5 or more, Eutric N4.3.2), "ph_7" (pH 7.0 or more, Eutric L4.3.3),
# "leachic" (pH under 6.5, Leachic) and "ferric" (the layer test of the
# ferric property, p. 61); pH in water (Q16).
.cst_sg_layer_test <- function(x, test) {
  t <- .cst_sgthr("layer_tests")
  ph <- .cst_v(x, "ph_h2o"); bs <- .cst_v(x, "bs_pct")
  switch(test,
    acid_or = .cst_b_test(x, "acid_or"),
    eutric_or = list(p = .cst_or_v(bs >= t$eutric_bs_min_pct, ph >= t$eutric_ph_min), missing = c("bs_pct", "ph_h2o")),
    ph_7 = list(p = ph >= t$argic_ph_min, missing = "ph_h2o"),
    leachic = list(p = ph < t$leachic_ph_below, missing = "ph_h2o"),
    ferric = .cst_b_test(x, "ferric"),
    rlang::abort(sprintf("Unknown layer test '%s'.", test)))
}

# The layers of a scope per layer TRUE / FALSE / NA: the B within 125 cm
# (.cst_b_layers()), or the layers of the argic horizon, the claypan or
# either.
.cst_sg_scope_layers <- function(x, scope) {
  if (scope == "b") {
    B <- .cst_b_layers(x)
    return(list(m = B$b, flag = B$flag, missing = if (any(is.na(B$b))) "designation"))
  }
  hz <- switch(scope, argic = "argic_horizon", claypan = "claypan", argic_or_claypan = c("argic_horizon", "claypan"))
  m <- Reduce(.cst_or_v, lapply(hz, function(h) .cst_hz(x, h)$full))
  list(m = m, flag = character(0), missing = unlist(lapply(hz, function(h) .cst_hz(x, h)$missing)))
}

#' A per-layer test (.cst_sg_layer_test()) read over a scope: `scope`
#' "window" the layers of [from_cm, to_cm) below the mineral soil surface (or
#' the soil surface), stopped at a shallower contact and cut by the base of
#' the description as the windows of 0.9.375 are; "b" the B within 125 cm;
#' "argic", "claypan" or "argic_or_claypan" the layers of those horizons.
#' `mode` "throughout" (every layer of the scope, Q23; a layer that may
#' belong to the scope counts only when it meets the test), "some" (a run of
#' `min_cm` there, 0 for any layer) or "share" (`share` of the thickness,
#' `strict` for "more than", Q22).
#' @noRd
layer_test_cst <- function(pedon, test, from_cm = 0, to_cm = 125, scope = c("window", "b", "argic", "claypan", "argic_or_claypan"),
                           mode = c("throughout", "some", "share"), min_cm = 10, share = 0.5, strict = TRUE,
                           datum = c("mineral", "surface")) {
  scope <- match.arg(scope); mode <- match.arg(mode); datum <- match.arg(datum)
  nm <- "layer_test_cst"
  x <- .cst_ctx(pedon)
  if (!x$n) return(.cst_key_result(nm, FALSE, page = "115-177"))
  lt <- .cst_sg_layer_test(x, test)
  p <- lt$p
  flags <- character(0)
  lab <- sprintf("%s %s %s", test, mode, scope)
  if (scope == "window") {
    s <- .cst_sg_datum(x, datum)
    if (is.na(s)) return(.cst_key_result(nm, NA, missing = c("designation", "oc_pct")))
    w <- .cst_sg_window(pedon, s + from_cm, s + to_cm)
    if (w[["b"]] <= w[["a"]]) return(.cst_key_result(nm, FALSE, notes = "a contact above the window", page = "115-177"))
    if (mode == "throughout") {
      ev <- .cst_every_cut(pedon, x, p, w[["a"]], w[["b"]])
      passed <- ev$passed; rows <- ev$rows; flags <- ev$flags
    } else if (mode == "some") {
      rr <- .cst_sg_run(x, p, w[["a"]], w[["b"]], min_cm)
      passed <- rr$passed; rows <- rr$rows
    } else {
      sh <- .cst_share_cut(pedon, x, p, w[["a"]], w[["b"]], share, strict)
      passed <- sh$passed; rows <- which(p %in% TRUE & .cst_overlap(x, w[["a"]], w[["b"]]) > 0); flags <- sh$flags
    }
    return(.cst_key_result(nm, passed, layers = x$idx[rows], flags = flags,
                           missing = if (is.na(passed)) lt$missing,
                           notes = sprintf("%s %s-%s cm", lab, format(w[["a"]]), format(w[["b"]])), page = "115-177"))
  }
  sc <- .cst_sg_scope_layers(x, scope)
  m <- sc$m
  inS <- m %in% TRUE; maybe <- is.na(m)
  if (!any(inS) && !any(maybe))
    return(.cst_key_result(nm, FALSE, flags = sc$flag, notes = sprintf("no %s", scope), page = "115-177"))
  if (mode == "throughout") {
    passed <- if (any(inS)) .cst_and(c(p[inS], ifelse(p[maybe] %in% TRUE, TRUE, NA))) else NA
    rows <- which(inS)
  } else if (mode == "some") {
    q <- ifelse(m %in% FALSE, FALSE, ifelse(is.na(m), ifelse(p %in% FALSE, FALSE, NA), p))
    g <- .cst_run_grade(x, q, max(min_cm, 1e-9))
    passed <- .cst_any3(g); rows <- which(g %in% TRUE)
  } else {
    rows <- which(inS)
    if (!length(rows)) passed <- NA
    else {
      a <- min(x$top[rows]); b <- max(x$bot[rows])
      q <- ifelse(inS, p, FALSE)
      passed <- .cst_share_at_least(.cst_window_share(pedon, q, a, b), share, strict)
    }
  }
  .cst_key_result(nm, passed, layers = x$idx[rows], flags = sc$flag,
                  missing = if (is.na(passed)) c(lt$missing, sc$missing), notes = lab, page = "115-177")
}


# --- features in a share, in every layer, below a horizon ---------------------------------------------

#' Features in a share of the thickness of the window from the soil surface
#' (or the mineral soil surface) to `within` cm or a shallower contact
#' (Phosphic of M5.2.1: phosphic property or evidence in more than half of
#' the horizons within 100 cm of the soil surface, p. 163; Q22), cut by the
#' base of the description as the shares are.
#' @noRd
feature_share_cst <- function(pedon, features, grade = c("at_least_evidence", "full"), share = 0.5, strict = TRUE,
                              within = 100, datum = c("surface", "mineral")) {
  grade <- match.arg(grade); datum <- match.arg(datum)
  nm <- "feature_share_cst"
  x <- .cst_ctx(pedon)
  if (!x$n) return(.cst_key_result(nm, FALSE))
  s <- .cst_sg_datum(x, datum)
  if (is.na(s)) return(.cst_key_result(nm, NA, missing = c("designation", "oc_pct")))
  p <- .cst_sg_or_layers(x, features, grade)
  w <- .cst_sg_window(pedon, s, s + within)
  if (w[["b"]] <= w[["a"]]) return(.cst_key_result(nm, FALSE))
  sh <- .cst_share_cut(pedon, x, p, w[["a"]], w[["b"]], share, strict)
  miss <- unique(unlist(lapply(features, function(f) .cst_sg_feature(x, f, grade)$missing)))
  .cst_key_result(nm, sh$passed, layers = x$idx[which(p %in% TRUE)], flags = sh$flags,
                  missing = if (is.na(sh$passed)) miss,
                  evidence = list(share = sh$share, window = w),
                  notes = sprintf("%s in %s of %s-%s cm", paste(gsub("_", " ", features), collapse = " or "),
                                  paste(format(round(sh$share, 2)), collapse = " to "), format(w[["a"]]), format(w[["b"]])))
}

#' Features in every layer of the window [from_cm, to_cm) (Epigleyic of
#' N2.1.2: gleyic evidence at least between 20 and 50 cm, p. 170; Q23),
#' stopped at a shallower contact and cut by the base of the description; a
#' pass resting on provisional layers (gley colours without a test, Q11) is
#' provisional.
#' @noRd
feature_every_cst <- function(pedon, features, grade = c("at_least_evidence", "full"), from_cm = 0, to_cm,
                              datum = c("mineral", "surface")) {
  grade <- match.arg(grade); datum <- match.arg(datum)
  nm <- "feature_every_cst"
  x <- .cst_ctx(pedon)
  if (!x$n) return(.cst_key_result(nm, FALSE))
  s <- .cst_sg_datum(x, datum)
  if (is.na(s)) return(.cst_key_result(nm, NA, missing = c("designation", "oc_pct")))
  p <- .cst_sg_or_layers(x, features, grade)
  w <- .cst_sg_window(pedon, s + from_cm, s + to_cm)
  if (w[["b"]] <= w[["a"]]) return(.cst_key_result(nm, FALSE))
  ev <- .cst_every_cut(pedon, x, p, w[["a"]], w[["b"]])
  prov <- isTRUE(ev$passed) && any(unlist(lapply(features, function(f) .cst_sg_feature(x, f, grade)$prov[ev$rows])) %in% TRUE)
  miss <- unique(unlist(lapply(features, function(f) .cst_sg_feature(x, f, grade)$missing)))
  .cst_key_result(nm, ev$passed, layers = x$idx[ev$rows], flags = c(ev$flags, if (prov) "provisional"),
                  missing = if (is.na(ev$passed)) miss,
                  notes = sprintf("%s in every layer of %s-%s cm", paste(gsub("_", " ", features), collapse = " or "),
                                  format(w[["a"]]), format(w[["b"]])))
}

#' Features in a run of `min_cm` within `to_cm` cm below the upper boundary of
#' a horizon (Gleyic of L1.1.2: gleyic evidence in some subhorizon 10 cm or
#' more thick within 100 cm below the top of the claypan, p. 134).
#' @noRd
feature_below_cst <- function(pedon, features, horizon = "claypan", to_cm = 100, min_cm = 10,
                              grade = c("at_least_evidence", "full")) {
  grade <- match.arg(grade)
  nm <- "feature_below_cst"
  x <- .cst_ctx(pedon)
  if (!x$n) return(.cst_key_result(nm, FALSE, page = 134))
  h <- .cst_hz(x, horizon)
  k <- which(h$full %in% TRUE)[1L]
  if (is.na(k)) return(.cst_key_result(nm, if (any(is.na(h$full))) NA else FALSE, missing = h$missing,
                                       notes = sprintf("no %s", horizon), page = 134))
  p <- .cst_sg_or_layers(x, features, grade)
  a <- x$top[k]
  rr <- .cst_sg_run(x, p, a, a + to_cm, min_cm)
  miss <- unique(unlist(lapply(features, function(f) .cst_sg_feature(x, f, grade)$missing)))
  .cst_key_result(nm, rr$passed, layers = x$idx[rr$rows], missing = if (is.na(rr$passed)) miss,
                  notes = sprintf("%s in a run of %s cm within %s cm below the top of the %s (%s cm)",
                                  paste(gsub("_", " ", features), collapse = " or "), format(min_cm), format(to_cm),
                                  horizon, format(a)),
                  page = 134)
}


# --- durations, perched water, vertic evidence ------------------------------------------------------

#' Salic evidence (the salic horizon passing too, Q19) in some horizon
#' `min_cm` thick within `within` cm, for a month or more a year in most
#' years (M2.2.3: within 25 cm, one month a year in 6 of 10 years, p. 152;
#' M3.3.1: 10 cm within 100 cm, in most years, p. 156): the duration is
#' site$salic_months_per_year (months a year, in most years); not recorded,
#' the evidence observed meets it, provisionally (Hugo's decision Q15).
#' @noRd
salic_duration_cst <- function(pedon, within, min_cm = 10, total = FALSE, datum = c("mineral", "surface")) {
  datum <- match.arg(datum)
  nm <- "salic_duration_cst"
  r <- feature_run_cst(pedon, "salic_horizon", min_cm = min_cm, from_cm = 0, to_cm = within, total = total,
                       datum = datum, grade = "at_least_evidence")
  if (!isTRUE(r$passed)) return(.cst_key_result(nm, r$passed, missing = r$missing, page = "152, 156"))
  mo <- .cst_site_num(pedon$site %||% list(), "salic_months_per_year")
  t <- .cst_sgthr("salic_duration")
  passed <- if (is.na(mo)) TRUE else mo >= t$months_min
  .cst_key_result(nm, passed, layers = r$layers,
                  flags = c(r$evidence$flags, if (is.na(mo)) c("salic_duration_not_recorded", "provisional")),
                  notes = if (is.na(mo)) "the salic evidence observed stands for its duration (not recorded), provisionally"
                          else sprintf("salic evidence %s months a year", format(mo)),
                  page = "152, 156")
}

#' Gleyic evidence (gleyic features passing too) linked to seasonal
#' saturation by perched water, in some horizon `min_cm` thick within
#' `within` cm (Stagnic of K2.2.5, K2.3.5, p. 126; default 22): perched water
#' is site$groundwater_kind "perched" or the stagnic regime; a recorded
#' groundwater table of another kind, the regime not stagnic, fails; neither
#' recorded, undecided.
#' @noRd
stagnic_perched_cst <- function(pedon, within = 50, min_cm = 10) {
  nm <- "stagnic_perched_cst"
  g <- feature_run_cst(pedon, "gleyic_features", min_cm = min_cm, from_cm = 0, to_cm = within, grade = "at_least_evidence")
  if (!isTRUE(g$passed)) return(.cst_key_result(nm, g$passed, missing = g$missing, page = "126-127"))
  gk <- .cst_site_txt(pedon$site %||% list(), "groundwater_kind")
  st <- moisture_regime_cst(pedon, "stagnic")$passed
  perched <- if (identical(gk, "perched") || isTRUE(st)) TRUE
             else if (!is.na(gk) && gk %in% c("observed", "apparent", "flooded") && isFALSE(st)) FALSE
             else NA
  .cst_key_result(nm, perched, layers = g$layers, flags = g$evidence$flags,
                  missing = if (is.na(perched)) c("groundwater_kind", "soil_moisture_regime"),
                  notes = sprintf("gleyic evidence within %s cm; perched water %s", format(within),
                                  if (isTRUE(perched)) "recorded" else if (isFALSE(perched)) "ruled out (groundwater)"
                                  else "not recorded"),
                  page = "126-127")
}

#' Vertic evidence (vertic features passing too, Q19) within `within` cm of
#' the mineral soil surface (Verti-lithic of N4.7.4: within 25 cm, p. 179):
#' the profile reading of vertic evidence (p. 50) with one of its layers
#' (cracks, slickensides or humic infillings) starting within the depth.
#' @noRd
vertic_within_cst <- function(pedon, within = 25) {
  nm <- "vertic_within_cst"
  r <- vertic_features_cst(pedon, grade = "at_least_evidence")
  if (!isTRUE(r$passed)) return(.cst_key_result(nm, r$passed, missing = r$missing, page = 179))
  x <- .cst_ctx(pedon)
  pos <- match(r$layers, x$idx)
  pos <- pos[!is.na(pos)]
  s <- if (is.na(x$msurf)) x$surf else x$msurf
  ok <- pos[!is.na(x$top[pos]) & x$top[pos] - s <= within]
  .cst_key_result(nm, length(ok) > 0L, layers = x$idx[ok],
                  notes = sprintf("vertic evidence with its features within %s cm", format(within)), page = 179)
}


# --- organic, nodular, pyroclastic and carbon readings ------------------------------------------------

#' A histic epipedon of a kind (Fibric, Sapric: I1.1.1, I2.1.1, I3.1.1,
#' I3.1.2, pp. 112-115): the kind of organic soil material that predominates
#' by thickness in the layers of the histic epipedon.
#' @noRd
histic_kind_cst <- function(pedon, kind = c("fibric", "hemic", "sapric")) {
  kind <- match.arg(kind)
  nm <- "histic_kind_cst"
  r <- histic_epipedon_cst(pedon)
  if (!isTRUE(r$passed)) return(.cst_key_result(nm, r$passed, missing = r$missing, page = "112-115"))
  x <- .cst_ctx(pedon)
  pos <- match(r$layers, x$idx)
  k <- .cst_ch(x, "organic_soil_materials")$kind[pos]
  th <- x$thk[pos]
  if (all(is.na(k))) return(.cst_key_result(nm, NA, missing = c("fiber_content_rubbed_pct", "von_post_index"), page = "112-115"))
  tot <- tapply(th[!is.na(k)], k[!is.na(k)], sum)
  unk <- sum(th[is.na(k)], na.rm = TRUE)
  best <- names(tot)[which.max(tot)]
  passed <- if (!kind %in% names(tot)) { if (max(tot) > unk) FALSE else NA }
            else if (tot[[kind]] > max(c(0, tot[names(tot) != kind])) + unk) TRUE
            else if (tot[[kind]] + unk <= max(c(0, tot[names(tot) != kind]))) FALSE else NA
  .cst_key_result(nm, passed, layers = r$layers, missing = if (is.na(passed)) "fiber_content_rubbed_pct",
                  notes = sprintf("histic epipedon, predominantly %s", best), page = "112-115")
}

#' An indurated subhorizon `min_cm` thick or more below the albic horizon
#' with Fe-Mn nodules and concretions `min_pct` % or more by volume
#' (Concretic, L4.1.1, p. 142): fe_mn_nodules_pct, or the sum of
#' ferric_nodules_pct and manganiferous_nodules_pct (flagged); induration a
#' cementation class strongly or indurated, or a rupture resistance very
#' hard or harder. Field observations: not recorded, absent.
#' @noRd
concretic_cst <- function(pedon, min_pct = NULL, min_cm = NULL) {
  t <- .cst_sgthr("concretic")
  min_pct <- min_pct %||% t$nodules_min_pct; min_cm <- min_cm %||% t$thick_min_cm
  nm <- "concretic_cst"
  x <- .cst_ctx(pedon)
  if (!x$n) return(.cst_key_result(nm, FALSE, page = 142))
  al <- .cst_hz(x, "albic_horizon")$full
  k <- which(al %in% TRUE)
  if (!length(k)) return(.cst_key_result(nm, if (any(is.na(al))) NA else FALSE, missing = .cst_hz(x, "albic_horizon")$missing,
                                         notes = "no albic horizon", page = 142))
  base <- x$bot[k[1L]]
  fm <- .cst_v(x, "fe_mn_nodules_pct")
  sum2 <- rowSums(cbind(.cst_v(x, "ferric_nodules_pct"), .cst_v(x, "manganiferous_nodules_pct")), na.rm = TRUE)
  any2 <- !is.na(.cst_v(x, "ferric_nodules_pct")) | !is.na(.cst_v(x, "manganiferous_nodules_pct"))
  fb <- is.na(fm) & any2
  v <- ifelse(is.na(fm), ifelse(any2, sum2, 0), fm)
  cem <- .cst_t(x, "cementation_class"); rup <- .cst_t(x, "rupture_resistance")
  ind <- (!is.na(cem) & cem %in% c("strongly", "indurated")) | (!is.na(rup) & rup %in% c("very hard", "extremely hard"))
  p <- v >= min_pct & ind & !is.na(x$top) & x$top >= base - 1e-9
  p[x$kind %in% "R"] <- FALSE
  g <- .cst_run_grade(x, p, min_cm)
  passed <- any(g %in% TRUE)
  rows <- which(g %in% TRUE)
  .cst_key_result(nm, passed, layers = x$idx[rows], flags = if (passed && any(fb[rows])) "fe_mn_nodules_from_sum",
                  notes = "induration and nodules are field observations: not recorded, absent", page = 142)
}

#' Cinders or other pyroclastic materials, `min_pct` % or more as the
#' thickness-weighted mean of the upper `depth_cm` cm of the mineral soil
#' (Cinderic, N4.4.2, N4.7.2, pp. 177, 179; default 21): a field observation,
#' not recorded absent, except in a layer of tephric (pyroclastic) origin,
#' read as meeting it, flagged. The depth below the description is
#' undecided.
#' @noRd
pyroclastic_cst <- function(pedon, min_pct = NULL, depth_cm = NULL) {
  t <- .cst_sgthr("cinderic")
  min_pct <- min_pct %||% t$pyroclastic_min_pct; depth_cm <- depth_cm %||% t$depth_cm
  nm <- "pyroclastic_cst"
  x <- .cst_ctx(pedon)
  if (!x$n || is.na(x$msurf)) return(.cst_key_result(nm, if (x$n) NA else FALSE, page = "177, 179"))
  a <- x$msurf; b <- a + depth_cm
  ct <- .cst_contact_cm(pedon)
  if (!is.na(ct) && ct < b) b <- ct
  if (b <= a) return(.cst_key_result(nm, FALSE, page = "177, 179"))
  py <- .cst_v(x, "pyroclastic_fragments_pct")
  lo <- .cst_t(x, "layer_origin")
  teph <- is.na(py) & !is.na(lo) & grepl("tephr|pyroclast|volcan", lo)
  v <- ifelse(is.na(py), ifelse(teph, min_pct, 0), py)
  v[x$kind %in% "R"] <- 0
  ov <- .cst_overlap(x, a, b); ov[is.na(ov)] <- 0
  gap <- .cst_sg_gap(x, a, b)
  tot <- sum(ov) + gap
  mean_lo <- sum(v * ov) / tot
  mean_hi <- (sum(v * ov) + 100 * gap) / tot
  passed <- if (mean_lo >= min_pct - 1e-9) TRUE else if (mean_hi < min_pct - 1e-9) FALSE else NA
  .cst_key_result(nm, passed, layers = x$idx[which(ov > 0 & v >= min_pct)],
                  flags = if (isTRUE(passed) && any(teph & ov > 0)) "pyroclastic_from_tephric_origin",
                  missing = if (is.na(passed)) "bottom_cm",
                  notes = sprintf("pyroclastic fragments %s %% over %s-%s cm", format(round(mean_lo, 1)), format(a), format(b)),
                  page = "177, 179")
}

#' Organic carbon of `min_pct` % or more as the thickness-weighted mean of the
#' upper `depth_cm` cm of the mineral soil (Grayic, N4.1.1: 6 g/kg within 20
#' cm, p. 176).
#' @noRd
oc_weighted_cst <- function(pedon, min_pct = NULL, depth_cm = NULL) {
  t <- .cst_sgthr("grayic")
  min_pct <- min_pct %||% .cst_pct(t$oc_min_gkg); depth_cm <- depth_cm %||% t$depth_cm
  nm <- "oc_weighted_cst"
  m <- .cst_mixed_surface(pedon, "oc_pct", depth = depth_cm, datum = "mineral")
  passed <- if (is.na(m$value)) NA else m$value >= min_pct - 1e-9
  .cst_key_result(nm, passed, missing = if (is.na(passed)) c("oc_pct", m$missing),
                  evidence = list(oc_pct = m$value),
                  notes = sprintf("organic carbon %s %% over the upper %s cm", format(round(m$value, 2)), format(depth_cm)),
                  page = 176)
}


# --- the Halosols ------------------------------------------------------------------------------------

#' Dilluvic (H2.1.1, p. 110; Hugo's decision Q18): a torrential (flash
#' flood) origin recorded (layer_origin of a layer, or the site's
#' layer_origin, parent_material or landform naming a torrential, diluvial or
#' proluvial deposit or an alluvial fan), which stands for the uneven
#' particle-size distribution, and soluble salts within 200 cm that rise and
#' fall with depth (a change of direction over three or more measured layers).
#' The origin is a field record: not recorded, absent.
#' @noRd
dilluvic_cst <- function(pedon, within = 200) {
  nm <- "dilluvic_cst"
  x <- .cst_ctx(pedon)
  if (!x$n || is.na(x$msurf)) return(.cst_key_result(nm, if (x$n) NA else FALSE, page = 110))
  pat <- "torrent|flash|diluv|proluv|alluvial fan|debris flow"
  site <- pedon$site %||% list()
  txt <- c(.cst_t(x, "layer_origin"), vapply(c("layer_origin", "parent_material", "landform"),
                                              function(k) .cst_site_txt(site, k), ""))
  origin <- any(!is.na(txt) & grepl(pat, txt))
  if (!origin) return(.cst_key_result(nm, FALSE, notes = "no torrential origin recorded", page = 110))
  rows <- which(!is.na(x$top) & x$top < x$msurf + within & !(x$kind %in% c("R", "O")))
  v <- .cst_v(x, "soluble_salts_pct")[rows]
  v <- v[!is.na(v)]
  passed <- if (length(v) < 3L) NA else {
    d <- sign(diff(v)); d <- d[d != 0]
    length(d) >= 2L && any(diff(d) != 0)
  }
  .cst_key_result(nm, passed, missing = if (is.na(passed)) "soluble_salts_pct",
                  notes = sprintf("torrential origin recorded; soluble salts %s %%", paste(format(v), collapse = ", ")),
                  page = 110)
}

#' Marinic (H2.2.3, p. 111; Hugo's decision Q18): in every layer of the salic
#' horizon (or, with none, of the salic evidence) chloride is the main anion
#' of the soluble salts (over half of them, or more than the sulfate and the
#' carbonate) and sodium the main cation (soluble_salts_sodium_pct over half
#' of the soluble cations; under a quarter it cannot be; exchangeable sodium
#' among the exchangeable bases stands in, flagged), and the deposits are
#' distinctly stratified (stratification_pattern recorded in some layer, a
#' field record: not recorded, absent). The seawater influence is read
#' through the Cl and Na; item 3 (similar salt contents of texture-alike
#' subsoil and substratum) is not tested.
#' @noRd
marinic_cst <- function(pedon) {
  nm <- "marinic_cst"
  x <- .cst_ctx(pedon)
  if (!x$n) return(.cst_key_result(nm, FALSE, page = 111))
  sa <- .cst_hz(x, "salic_horizon")
  rows <- which(sa$full %in% TRUE)
  if (!length(rows)) rows <- which(sa$evid %in% TRUE)
  if (!length(rows)) return(.cst_key_result(nm, if (any(is.na(sa$evid))) NA else FALSE, missing = sa$missing,
                                            notes = "no salic horizon or salic evidence", page = 111))
  t <- .cst_sgthr("marinic")
  cl <- .cst_v(x, "soluble_salts_chloride_pct"); so <- .cst_v(x, "soluble_salts_sulfate_pct")
  co <- .cst_v(x, "soluble_salts_carbonate_pct")
  cl_ok <- ifelse(is.na(cl), NA, ifelse(cl > t$main_share_pct, TRUE,
                  ifelse(is.na(so), NA, cl > so & (is.na(co) | cl > co))))
  na <- .cst_v(x, "soluble_salts_sodium_pct")
  ex <- .cst_v(x, "na_cmol") / rowSums(cbind(.cst_v(x, "na_cmol"), .cst_v(x, "ca_cmol"), .cst_v(x, "mg_cmol"), .cst_v(x, "k_cmol")))
  ex <- ex * 100
  fb <- is.na(na) & !is.na(ex)
  nav <- ifelse(is.na(na), ex, na)
  na_ok <- ifelse(is.na(nav), NA, ifelse(nav > t$main_share_pct, TRUE, ifelse(nav < t$cation_floor_pct, FALSE, NA)))
  salts <- .cst_and(cl_ok[rows], na_ok[rows])
  strat <- any(!is.na(.cst_t(x, "stratification_pattern")) & !(.cst_t(x, "stratification_pattern") %in% "none"))
  passed <- .cst_and(salts, strat)
  .cst_key_result(nm, passed, layers = x$idx[rows], flags = if (isTRUE(passed) && any(fb[rows])) "sodium_from_exchangeable_sodium",
                  missing = if (is.na(passed)) c("soluble_salts_chloride_pct", "soluble_salts_sulfate_pct", "soluble_salts_sodium_pct"),
                  notes = "Cl the main anion and Na the main cation of the salts of the salic horizon, the deposits stratified",
                  page = 111)
}


# --- the examples of the subgroups of orders H to N --------------------------------------------------

# The subgroups no profile can reach under the readings in force (readings.yaml,
# part 0.9.376, for Hugo's word): alkalic evidence is read on the argic items
# (0.9.371), so it is always an argic horizon, and the Argic subgroup (an
# argic horizon within 50 cm) is keyed before it in J2.4 and J2.5; the
# anthrostagnic evidence asks for the anthrostagnic regime, which the
# saturation axis cannot hold beside the aquic regime of the Aquic Cambosols
# (M2); the lithologic character of sandy deposits reads the carbon of the
# upper 100 cm from the soil surface, which a litter layer of histic
# evidence always exceeds. They have no example.
.CST_SUBGROUP_UNREACHABLE <- c(
  "J2.4.3" = "alkalic evidence is an argic horizon (0.9.371), and Argic (J2.4.2) is keyed first",
  "J2.4.4" = "alkalic evidence is an argic horizon (0.9.371), and Argic (J2.4.2) is keyed first",
  "J2.5.2" = "alkalic evidence is an argic horizon (0.9.371), and Argic (J2.5.1) is keyed first",
  "J2.5.3" = "alkalic evidence is an argic horizon (0.9.371), and Argic (J2.5.1) is keyed first",
  "M2.2.1" = "anthrostagnic evidence asks for the anthrostagnic regime, the Aquic Cambosols for the aquic one",
  "M2.3.1" = "anthrostagnic evidence asks for the anthrostagnic regime, the Aquic Cambosols for the aquic one",
  "M2.4.1" = "anthrostagnic evidence asks for the anthrostagnic regime, the Aquic Cambosols for the aquic one",
  "N2.4.4" = "the sandy deposits allow 1.5 g/kg carbon over 0-100 cm from the soil surface, below any histic evidence")

# The profile of each subgroup example of orders H to N, as
# .cst_subgroup_example_spec() reads it (R/key-cst-subgroups.R): a group,
# suborder or subgroup example (`base`) with some of its layers (`rows`)
# and some columns replaced (`set`, whole columns, recycled) and some site
# fields (`site`), or a profile of its own (`h`, `site`). Each keys to its
# own subgroup, not provisionally.
.cst_subgroup_example_spec_hn <- function(code) {
  nosulf <- list(sulfidic_s_pct = 0)
  # recurring changes: no coarse fragments (the Lithic alternative decided),
  # a lithic contact at 45 cm, the first mineral layer removed (Trunic),
  # humus coatings and carbon (Humic), plinthite (Plinthic), mottles
  # (Mottlic), base saturation (Eutrophic), an agric horizon under an Ap
  cf0 <- list(coarse_fragments_pct = 0)
  lith3 <- function(d) list(designation = d, top_cm = c(0, 15, 45), bottom_cm = c(15, 45, 65))
  hum3 <- function(oc) list(oc_pct = oc, bulk_density_g_cm3 = c(1.0, 1.2, 1.3), humus_coatings = c(NA, "common", "common"))
  plin3 <- list(plinthite_pct = c(NA, NA, 20), plinthite_hardens = c(NA, NA, TRUE))
  mot3 <- list(mottle_colors = c(NA, NA, "7.5YR 5/6"))
  agr3 <- function(d) list(designation = d, humus_clay_films_mm = c(NA, 0.6, NA), humus_clay_films_pct = c(NA, 8, NA))
  stag3 <- list(reduced_matrix_pct = c(NA, 40, NA), childs_test = c(NA, "positive", NA))
  perched <- list(groundwater_kind = "perched")
  xan <- list(udic_perudic_border = TRUE)
  switch(code,
  # --- H ---
  "H1.1.1" = list(base = "H1.1", set = list(soluble_salts_pct = c(0.4, 0.8, 0.5, 0.4))),
  "H1.1.2" = list(base = "H1.1"),
  "H1.2.1" = list(base = "H1.2"),
  "H1.2.2" = list(base = "H1.2", set = list(soluble_salts_pct = 0.1)),
  "H1.3.1" = list(base = "H1.3"),
  "H1.3.2" = list(base = "H1.3", set = list(soluble_salts_pct = 0.1)),
  "H2.1.1" = list(base = "H2.1.4", set = list(soluble_salts_pct = c(2.8, 0.6, 1.5)),
                  site = list(parent_material = "torrential flood deposits")),
  "H2.1.2" = list(base = "H2.1.4", set = list(soluble_salts_pct = c(2.8, 25, 0.4), cementation_class = c(NA, "strongly", NA),
                                              caso4_pct = c(0, 0, 12), gypsum_visible_pct = c(0, 0, 3))),
  "H2.1.3" = list(base = "H2.1.4", set = list(caso4_pct = c(0, 0, 12), gypsum_visible_pct = c(0, 0, 3))),
  "H2.1.4" = list(base = "H2.1"),
  "H2.2.1" = list(base = "H2.2.8", site = list(soil_temperature_regime = "cryic", mean_summer_soil_temp_C = 8,
                                               freeze_thaw_days_pct = 75)),
  "H2.2.2" = list(base = "H2.2.8", set = list(sulfidic_s_pct = c(0, 1.2, 0), incubation_ph = c(NA, 3.5, NA))),
  "H2.2.3" = list(base = "H2.2.8", set = list(soluble_salts_chloride_pct = c(70, 65, 60), soluble_salts_sulfate_pct = c(20, 25, 30),
                                              soluble_salts_sodium_pct = c(80, 75, 70),
                                              stratification_pattern = c(NA, "continuous", "continuous"))),
  "H2.2.4" = list(base = "H2.2.8", rows = c(1L, 1L, 2L, 3L), set = list(top_cm = c(0, 3, 25, 60), bottom_cm = c(3, 25, 60, 110),
                                                                     designation = c("Az", "A", "Bz", "C"),
                                                                     soluble_salts_pct = c(15, 2.8, 1.0, 0.4))),
  "H2.2.5" = list(base = "H2.2.8", set = list(esp_pct = c(4, 12, 8), clay_pct = c(15, 28, 20), silt_pct = c(50, 45, 50),
                                              sand_pct = c(35, 27, 30))),
  "H2.2.6" = list(base = "H2.2.8", set = list(munsell_hue_moist = c("10YR", "5Y", "5Y"), munsell_chroma_moist = c(2, 1, 1),
                                              childs_test = c(NA, "positive", "positive"))),
  "H2.2.7" = list(base = "H2.2.8", set = list(munsell_value_moist = c(2, 5, 6), munsell_value_dry = c(4, 7, 7),
                                              oc_pct = c(2.0, 0.3, 0.2), munsell_chroma_moist = c(2, 3, 3))),
  "H2.2.8" = list(base = "H2.2", set = c(nosulf, list(esp_pct = c(4, 4, 3), munsell_chroma_moist = c(3, 4, 4)))),
  # --- I ---
  "I1.1.1" = list(base = "I1.1", set = list(fiber_content_rubbed_pct = c(80, NA, NA, NA))),
  "I1.1.2" = list(base = "I1.1"),
  "I1.2.1" = list(base = "I1.2.4", set = list(designation = c("A", "Bg", "Cf", "R"), top_cm = c(0, 15, 30, 45),
                                              bottom_cm = c(15, 30, 45, 65), permafrost_temp_C = c(NA, NA, -1, NA))),
  "I1.2.2" = list(base = "I1.2"),
  "I1.2.3" = list(base = "I1.2.4", set = list(top_cm = c(0, 25, 50, 90), bottom_cm = c(25, 50, 90, 120),
                                              munsell_value_moist = c(2, 5, 5, 5), munsell_value_dry = c(4, 6, 6, 6),
                                              oc_pct = c(3, 1.5, 0.6, 0.4))),
  "I1.2.4" = list(base = "I1.2", set = list(designation = c("A", "Bg", "Cg", "Cf"), oc_pct = c(1.5, 1.5, 0.6, 0.4),
                                            clay_pct = c(25, 25, 22, 20), silt_pct = 50, sand_pct = c(25, 25, 28, 30),
                                            munsell_value_moist = c(4, 5, 5, 5), munsell_value_dry = c(6, 6, 6, 6),
                                            fiber_content_rubbed_pct = NA, bulk_density_g_cm3 = c(1.2, 1.3, 1.4, 1.5),
                                            water_saturation_days = c(150, 200, 250, 250))),
  "I2.1.1" = list(base = "I2.1", set = list(fiber_content_rubbed_pct = c(80, NA, NA, NA))),
  "I2.1.2" = list(base = "I2.1"),
  "I2.2.1" = list(base = "I2.2.3", set = list(designation = c("Ap", "Eg", "Btg", "R"), top_cm = c(0, 20, 40, 48),
                                              bottom_cm = c(20, 40, 48, 70))),
  "I2.2.2" = list(base = "I2.2.3", set = list(munsell_value_moist = c(2, 6, 5, 5), munsell_value_dry = c(4, 7, 6, 6),
                                              oc_pct = c(2.5, 0.5, 0.4, 0.2), top_cm = c(0, 25, 40, 80),
                                              bottom_cm = c(25, 40, 80, 120))),
  "I2.2.3" = list(base = "I2.2"),
  "I3.1.1" = list(base = "I3.1", set = list(fiber_content_rubbed_pct = c(80, NA, NA))),
  "I3.1.2" = list(base = "I3.1"),
  "I3.1.3" = list(base = "I3.1", set = list(fiber_content_rubbed_pct = c(30, NA, NA))),
  "I3.2.1" = list(base = "I3.2.5", set = list(sulfidic_s_pct = c(0, 1.2, 0), incubation_ph = c(NA, 3.5, NA))),
  "I3.2.2" = list(base = "I3.2.5", set = list(ph_h2o = c(5.0, 5.0, 7))),
  "I3.2.3" = list(base = "I3.2.5", set = list(soluble_salts_pct = c(0.05, 0.5, 0.05))),
  "I3.2.4" = list(base = "I3.2.5", set = list(caco3_pct = c(2, 2, 1), ph_h2o = 7.8)),
  "I3.2.5" = list(base = "I3.2", set = nosulf),
  "I3.3.1" = list(base = "I3.3.6", set = list(designation = c("Ah", "Bg", "R"), top_cm = c(0, 30, 45), bottom_cm = c(30, 45, 65))),
  "I3.3.2" = list(base = "I3.3.6", set = list(munsell_value_moist = c(2, 5, 5), munsell_value_dry = c(4, 6, 6),
                                              oc_pct = c(3, 0.5, 0.3), bs_pct = c(30, 80, 80))),
  "I3.3.3" = list(base = "I3.3.6", set = list(ph_h2o = c(5.0, 5.0, 7))),
  "I3.3.4" = list(base = "I3.3.6", set = list(soluble_salts_pct = c(0.05, 0.5, 0.05))),
  "I3.3.5" = list(base = "I3.3.6", set = list(caco3_pct = c(2, 2, 1), ph_h2o = 7.8)),
  "I3.3.6" = list(base = "I3.3"),
  # --- J ---
  "J1.1.1" = list(base = "J1.1.4", set = list(p_olsen_mg_kg = c(40, 30, 5, 5), worm_holes_pct = c(60, 55, NA, NA),
                                              artefacts_pct = c(2, 1, NA, NA))),
  "J1.1.2" = list(base = "J1.1.4", set = list(munsell_hue_moist = c("10YR", "10YR", "5Y", "10YR"), munsell_chroma_moist = c(2, 2, 1, 3),
                                              childs_test = c(NA, NA, "positive", NA))),
  "J1.1.3" = list(base = "J1.1.4", rows = c(1L, 1L, 2L, 3L, 4L),
                  set = list(top_cm = c(0, 15, 30, 70, 120), bottom_cm = c(15, 30, 70, 120, 180),
                             designation = c("Ah", "Bm", "AB", "C1", "C2"), p_total_pct = c(5, 4.8, 1.2, 0.5, 0.2),
                             caco3_pct = c(30, 55, 56, 60, 70), cementation_class = c(NA, "indurated", NA, NA, NA))),
  "J1.1.4" = list(base = "J1.1"),
  "J1.2.1" = list(base = "J1.2"),
  "J2.1.1" = list(base = "J2.1", set = list(designation = c("Ah", "Bt", "Bk", "C"), clay_pct = c(22, 32, 24, 20),
                                            clay_films_strength = c(NA, "distinct", NA, NA), clay_films_amount = c(NA, "common", NA, NA))),
  "J2.1.2" = list(base = "J2.1"),
  "J2.1.3" = list(base = "J2.1", set = list(caco3_pct = c(0.5, 1, 1.5, 2), secondary_carbonates_pct = 0, designation = c("Ah", "AB", "Bw", "C"))),
  "J2.2.1" = list(base = "J2.2", set = list(mottle_colors = c(NA, NA, "7.5YR 5/6", NA))),
  "J2.2.2" = list(base = "J2.2"),
  "J2.2.3" = list(base = "J2.2", set = list(caco3_pct = c(0.5, 1, 1.5, 2), secondary_carbonates_pct = 0, designation = c("Ah", "AB", "Bw", "C"))),
  "J2.3.1" = list(base = "J2.3.6", set = list(mottle_colors = c(NA, NA, "7.5YR 5/6", NA), designation = c("Ah", "Bt", "Bw", "C"),
                                              clay_pct = c(22, 32, 24, 20), clay_films_strength = c(NA, "distinct", NA, NA),
                                              clay_films_amount = c(NA, "common", NA, NA))),
  "J2.3.2" = list(base = "J2.3", set = list(mottle_colors = c(NA, NA, "7.5YR 5/6", NA))),
  "J2.3.3" = list(base = "J2.3", set = list(designation = c("Ah", "Bt", "Bk", "C"), clay_pct = c(22, 32, 24, 20),
                                            clay_films_strength = c(NA, "distinct", NA, NA), clay_films_amount = c(NA, "common", NA, NA))),
  "J2.3.4" = list(base = "J2.3.6", set = list(designation = c("Ah", "Bt", "Bw", "C"), clay_pct = c(22, 32, 24, 20),
                                              clay_films_strength = c(NA, "distinct", NA, NA), clay_films_amount = c(NA, "common", NA, NA))),
  "J2.3.5" = list(base = "J2.3"),
  "J2.3.6" = list(base = "J2.3", set = list(caco3_pct = c(0.5, 1, 1.5, 2), secondary_carbonates_pct = 0, designation = c("Ah", "AB", "Bw", "C"))),
  "J2.4.1" = list(base = "J2.4", set = list(mottle_colors = c(NA, NA, "7.5YR 5/6", NA))),
  "J2.4.2" = list(base = "J2.4", set = list(designation = c("Ah", "Bt", "Bk", "C"), clay_pct = c(22, 32, 24, 20), munsell_chroma_moist = 3,
                                            clay_films_strength = c(NA, "distinct", NA, NA), clay_films_amount = c(NA, "common", NA, NA))),
  "J2.4.5" = list(base = "J2.4"),
  "J2.5.1" = list(base = "J2.5", set = list(designation = c("Ah", "Bt", "Bw", "C"), clay_pct = c(22, 32, 24, 20), munsell_chroma_moist = 3,
                                            clay_films_strength = c(NA, "distinct", NA, NA), clay_films_amount = c(NA, "common", NA, NA))),
  "J2.5.4" = list(base = "J2.5"),
  "J3.1.1" = list(base = "J3.1", rows = c(1L, 2L, 2L, 3L, 4L),
                  set = list(top_cm = c(0, 35, 45, 60, 130), bottom_cm = c(35, 45, 60, 130, 180),
                             designation = c("Ah", "E", "Bw", "Bg", "C"), munsell_value_moist = c(2, 6, 4, 5, 5),
                             munsell_chroma_moist = c(2, 2, 3, 2, 4), munsell_value_dry = c(4, 7, 6, 6, 7),
                             mottle_colors = c(NA, NA, "7.5YR 5/6", NA, NA), oc_pct = c(2.5, 1.1, 1.0, 0.5, 0.3), bulk_density_g_cm3 = c(1.2, 1.35, 1.4, 1.45, 1.5),
                             clay_pct = c(22, 22, 24, 25, 22))),
  "J3.1.2" = list(base = "J3.1", set = list(munsell_value_moist = c(2, 3, 5, 5), munsell_value_dry = c(4, 5, 6, 7),
                                            oc_pct = c(2.5, 1.2, 0.5, 0.3))),
  "J3.1.3" = list(base = "J3.1"),
  "J3.2.1" = list(base = "J3.2.4", rows = c(1L, 1L, 2L, 3L, 4L),
                  set = list(top_cm = c(0, 30, 40, 90, 130), bottom_cm = c(30, 40, 90, 130, 180),
                             designation = c("Ah", "E", "Bt", "BC", "C"), munsell_value_moist = c(2, 6, 4, 5, 5),
                             munsell_chroma_moist = c(2, 2, 3, 4, 4), munsell_value_dry = c(4, 7, 6, 6, 7),
                             oc_pct = c(2.5, 1.1, 1.0, 0.5, 0.3), bulk_density_g_cm3 = c(1.2, 1.35, 1.4, 1.45, 1.5), clay_pct = c(22, 22, 32, 26, 22))),
  "J3.2.2" = list(base = "J3.2.4", set = list(mottle_colors = c(NA, NA, "7.5YR 5/6", NA))),
  "J3.2.3" = list(base = "J3.2.4", set = list(munsell_value_moist = c(2, 3, 5, 5), munsell_value_dry = c(4, 5, 6, 7),
                                              oc_pct = c(2.5, 1.2, 0.5, 0.3))),
  "J3.2.4" = list(base = "J3.2"),
  "J3.3.1" = list(base = "J3.3", set = list(mottle_colors = c(NA, NA, "7.5YR 5/6", NA))),
  "J3.3.2" = list(base = "J3.3"),
  # --- K ---
  "K1.1.1" = list(base = "K1.1"),
  "K1.2.1" = list(base = "K1.2.3", set = lith3(c("A", "Bw1", "R"))),
  "K1.2.2" = list(base = "K1.2.3", rows = c(2L, 3L), set = list(top_cm = c(0, 45), bottom_cm = c(45, 115))),
  "K1.2.3" = list(base = "K1.2", set = cf0),
  "K2.1.1" = list(base = "K2.1.4", set = list(oc_pct = c(2.5, 0.8, NA), bulk_density_g_cm3 = c(1.1, 1.3, NA),
                                              humus_coatings = c(NA, "common", NA))),
  "K2.1.2" = list(base = "K2.1"),
  "K2.1.3" = list(base = "K2.1.4", set = list(designation = c("A", "Bt", "R"), clay_pct = c(30, 50, NA), silt_pct = c(45, 25, NA),
                                              clay_films_strength = c(NA, "distinct", NA), clay_films_amount = c(NA, "common", NA))),
  "K2.1.4" = list(base = "K2.1", set = list(ph_h2o = c(6.8, 6.6, NA), clay_pct = c(46, 50, NA), silt_pct = c(29, 25, NA))),
  "K2.2.1" = list(base = "K2.2.9", set = lith3(c("A", "Bw1", "R"))),
  "K2.2.2" = list(base = "K2.2.9", rows = c(2L, 3L), set = list(top_cm = c(0, 45), bottom_cm = c(45, 115))),
  "K2.2.3" = list(base = "K2.2.9", set = agr3(c("Ap", "Bw1", "Bw2"))),
  "K2.2.4" = list(base = "K2.2.9", set = hum3(c(3.5, 1.2, 0.6))),
  "K2.2.5" = list(base = "K2.2.9", set = stag3, site = perched),
  "K2.2.6" = list(base = "K2.2.9", set = mot3),
  "K2.2.7" = list(base = "K2.2.9", set = list(bs_pct = 40)),
  "K2.2.8" = list(base = "K2.2", set = cf0),
  "K2.2.9" = list(base = "K2.2", set = c(cf0, list(clay_pct = c(46, 50, 50), silt_pct = c(29, 25, 25)))),
  "K2.3.1" = list(base = "K2.3.8", set = lith3(c("A", "Bw1", "R"))),
  "K2.3.2" = list(base = "K2.3.8", rows = c(2L, 3L), set = list(top_cm = c(0, 45), bottom_cm = c(45, 115))),
  "K2.3.3" = list(base = "K2.3.8", set = agr3(c("Ap", "Bw1", "Bw2"))),
  "K2.3.4" = list(base = "K2.3.8", set = hum3(c(3.5, 1.2, 0.6))),
  "K2.3.5" = list(base = "K2.3.8", set = stag3, site = perched),
  "K2.3.6" = list(base = "K2.3.8", set = mot3),
  "K2.3.7" = list(base = "K2.3.8", set = list(bs_pct = 40)),
  "K2.3.8" = list(base = "K2.3", set = c(cf0, list(clay_pct = c(46, 50, 50), silt_pct = c(29, 25, 25)))),
  "K3.1.1" = list(base = "K3.1.6", rows = c(2L, 3L), set = list(top_cm = c(0, 90), bottom_cm = c(90, 110))),
  "K3.1.2" = list(base = "K3.1.6", set = list(oc_pct = c(3.0, 1.0, NA), bulk_density_g_cm3 = c(1.0, 1.2, NA),
                                              humus_coatings = c(NA, "common", NA))),
  "K3.1.3" = list(base = "K3.1.6", set = list(ph_h2o = c(6.0, 5.8, NA))),
  "K3.1.4" = list(base = "K3.1", set = list(ph_h2o = c(6.8, 6.6, NA), munsell_hue_moist = "2.5YR")),
  "K3.1.5" = list(base = "K3.1.6", set = list(munsell_hue_moist = "5YR")),
  "K3.1.6" = list(base = "K3.1", set = list(ph_h2o = c(6.8, 6.6, NA), munsell_hue_moist = "2.5YR", designation = c("A", "Bw", "R"),
                                            clay_pct = c(36, 40, NA), clay_films_strength = NA, clay_films_amount = NA)),
  "K3.2.1" = list(base = "K3.2.4", rows = 2:4, set = list(top_cm = c(0, 40, 60), bottom_cm = c(40, 60, 110))),
  "K3.2.2" = list(base = "K3.2.4", set = list(oc_pct = c(3.0, 1.2, 0.8, 0.6), bulk_density_g_cm3 = c(1.0, 1.2, 1.3, 1.3),
                                              humus_coatings = c(NA, "common", "common", "common"))),
  "K3.2.3" = list(base = "K3.2"),
  "K3.2.4" = list(base = "K3.2", set = list(designation = c("A", "Bw1", "Bw2", "Bw3"), clay_pct = c(36, 38, 40, 40),
                                            sand_pct = c(30, 30, 30, 30), silt_pct = c(34, 32, 30, 30),
                                            clay_films_strength = NA, clay_films_amount = NA)),
  "K3.3.1" = list(base = "K3.3.10", set = lith3(c("A", "Bw1", "R"))),
  "K3.3.2" = list(base = "K3.3.10", rows = c(2L, 3L), set = list(top_cm = c(0, 40), bottom_cm = c(40, 110))),
  "K3.3.3" = list(base = "K3.3.10", set = hum3(c(3.0, 1.2, 0.6))),
  "K3.3.4" = list(base = "K3.3.10", set = list(munsell_hue_moist = "7.5YR"), site = xan),
  "K3.3.5" = list(base = "K3.3.10", set = mot3),
  "K3.3.6" = list(base = "K3.3.10", set = plin3),
  "K3.3.7" = list(base = "K3.3.10", set = list(munsell_hue_moist = "2.5YR", munsell_hue_dry = "2.5YR")),
  "K3.3.8" = list(base = "K3.3.10", set = list(bs_pct = 40)),
  "K3.3.9" = list(base = "K3.3", set = cf0),
  "K3.3.10" = list(base = "K3.3", set = c(cf0, list(designation = c("A", "Bw1", "Bw2"), clay_pct = c(36, 38, 40),
                                                    silt_pct = c(34, 32, 30), sand_pct = 30,
                                                    clay_films_strength = NA, clay_films_amount = NA))),
  "K3.4.1" = list(base = "K3.4.7", rows = c(2L, 3L), set = list(top_cm = c(0, 40), bottom_cm = c(40, 110))),
  "K3.4.2" = list(base = "K3.4.7", set = hum3(c(3.0, 1.2, 0.6))),
  "K3.4.3" = list(base = "K3.4.7", set = list(munsell_hue_moist = "7.5YR"), site = xan),
  "K3.4.4" = list(base = "K3.4.7", set = mot3),
  "K3.4.5" = list(base = "K3.4.7", set = plin3),
  "K3.4.6" = list(base = "K3.4.7", set = list(bs_pct = 40)),
  "K3.4.7" = list(base = "K3.4"),
  "K3.5.1" = list(base = "K3.5.10", set = lith3(c("A", "Bw1", "R"))),
  "K3.5.2" = list(base = "K3.5.10", rows = c(2L, 3L), set = list(top_cm = c(0, 40), bottom_cm = c(40, 110))),
  "K3.5.3" = list(base = "K3.5.10", set = agr3(c("Ap", "Bw1", "Bw2"))),
  "K3.5.4" = list(base = "K3.5.10", set = hum3(c(3.0, 1.2, 0.6))),
  "K3.5.5" = list(base = "K3.5.10", set = list(munsell_hue_moist = "7.5YR"), site = xan),
  "K3.5.6" = list(base = "K3.5.10", set = mot3),
  "K3.5.7" = list(base = "K3.5.10", set = plin3),
  "K3.5.8" = list(base = "K3.5.10", set = list(munsell_hue_moist = "2.5YR", munsell_hue_dry = "2.5YR")),
  "K3.5.9" = list(base = "K3.5.10", set = list(bs_pct = 40)),
  "K3.5.10" = list(base = "K3.5", set = cf0),
  # --- L ---
  "L1.1.1" = list(base = "L1.1.5", rows = c(1L, 1L, 2L, 3L, 4L),
                  set = list(top_cm = c(0, 8, 15, 25, 80), bottom_cm = c(8, 15, 25, 80, 120),
                             designation = c("Oa", "Ah", "E", "Bt", "C"), oc_pct = c(25, 3, 0.6, 0.4, 0.2),
                             clay_pct = c(NA, 18, 12, 34, 24), silt_pct = c(NA, 55, 58, 45, 48), sand_pct = c(NA, 27, 30, 21, 28),
                             water_saturation_days = c(60, NA, NA, NA, NA), bulk_density_g_cm3 = c(0.3, NA, NA, NA, NA),
                             munsell_value_moist = c(2, 3, 6, 4, 5), munsell_chroma_moist = c(1, 2, 2, 4, 4))),
  "L1.1.2" = list(base = "L1.1.5", set = list(reduced_matrix_pct = c(NA, NA, 40, NA), childs_test = c(NA, NA, "positive", NA))),
  "L1.1.3" = list(base = "L1.1.5", set = list(top_cm = c(0, 25, 35, 80), bottom_cm = c(25, 35, 80, 150),
                                              bulk_density_g_cm3 = c(1.1, 1.3, 1.4, 1.5))),
  "L1.1.4" = list(base = "L1.1.5", set = list(bs_pct = c(60, 50, 40, 75))),
  "L1.1.5" = list(base = "L1.1"),
  "L1.2.1" = list(base = "L1.2.6", rows = 1:3, set = list(designation = c("Ah", "Bt1", "R"), top_cm = c(0, 25, 45),
                                                         bottom_cm = c(25, 45, 65))),
  "L1.2.2" = list(base = "L1.2.6", set = list(designation = c("Ah", "Bt1", "Bt2", "Ck"), caco3_pct = c(0, 0, 0, 15),
                                              secondary_carbonates_pct = c(0, 0, 0, 8), ph_h2o = c(5.8, 6.0, 6.5, 8.0))),
  "L1.2.3" = list(base = "L1.2.6", set = list(designation = c("Ah", "E", "Bt", "C"), munsell_value_moist = c(2, 6, 4, 5),
                                              munsell_chroma_moist = c(2, 2, 4, 4), munsell_value_dry = c(4, 7, 6, 6),
                                              clay_pct = c(18, 14, 30, 24), silt_pct = c(55, 58, 45, 48), sand_pct = c(27, 28, 25, 28),
                                              clay_films_strength = c(NA, NA, "distinct", NA), clay_films_amount = c(NA, NA, "common", NA),
                                              oc_pct = c(3, 0.6, 0.4, 0.2), bulk_density_g_cm3 = c(1.1, 1.3, 1.4, 1.5))),
  "L1.2.4" = list(base = "L1.2.6", set = list(reduced_matrix_pct = c(40, NA, NA, NA), childs_test = c("positive", NA, NA, NA))),
  "L1.2.5" = list(base = "L1.2.6", set = list(mottle_colors = c(NA, NA, "7.5YR 5/6", NA))),
  "L1.2.6" = list(base = "L1.2", set = c(cf0, list(bottom_cm = c(25, 45, 80, 150)))),
  "L1.3.1" = list(base = "L1.3.6", set = list(designation = c("Ah", "AB", "Bt", "R"), top_cm = c(0, 15, 25, 45),
                                              bottom_cm = c(15, 25, 45, 65))),
  "L1.3.2" = list(base = "L1.3.6", set = list(designation = c("Ah", "AB", "Bt", "Cg"), munsell_hue_moist = c("10YR", "10YR", "10YR", "5Y"),
                                              munsell_chroma_moist = c(2, 3, 4, 1), childs_test = c(NA, NA, NA, "positive"))),
  "L1.3.3" = list(base = "L1.3.4", set = list(mottle_colors = c(NA, NA, "7.5YR 5/6", NA))),
  "L1.3.4" = list(base = "L1.3", set = c(cf0, list(bottom_cm = c(15, 25, 80, 150)))),
  "L1.3.5" = list(base = "L1.3.6", set = list(mottle_colors = c(NA, NA, "7.5YR 5/6", NA))),
  "L1.3.6" = list(base = "L1.3", set = c(cf0, list(bottom_cm = c(15, 25, 80, 150), designation = c("Ah", "AB", "Bt", "C"),
                                                   munsell_value_moist = c(3, 4, 4, 5), munsell_chroma_moist = c(2, 3, 4, 4),
                                                   munsell_value_dry = c(5, 6, 6, 6), clay_pct = c(18, 18, 30, 24),
                                                   oc_pct = c(3, 0.8, 0.4, 0.2)))),
  "L2.1.1" = list(base = "L2.1.3", set = list(top_cm = c(0, 25, 70, 110), bottom_cm = c(25, 70, 110, 140), munsell_value_moist = c(3, 4, 5, 6),
                                              munsell_value_dry = c(5, 6, 7, 7), oc_pct = c(2.5, 0.2, 0.1, 0.1))),
  "L2.1.2" = list(base = "L2.1.3", set = list(munsell_hue_moist = "2.5YR")),
  "L2.1.3" = list(base = "L2.1"),
  "L2.2.1" = list(base = "L2.2.2", set = list(mottle_colors = c(NA, NA, "7.5YR 5/6", NA))),
  "L2.2.2" = list(base = "L2.2"),
  "L2.3.1" = list(base = "L2.3.3", rows = 2:4, set = list(top_cm = c(0, 50, 90), bottom_cm = c(50, 90, 130))),
  "L2.3.2" = list(base = "L2.3.3", set = list(mottle_colors = c(NA, NA, "7.5YR 5/6", NA))),
  "L2.3.3" = list(base = "L2.3"),
  "L2.4.1" = list(base = "L2.4.5", rows = 1:3, set = list(designation = c("Ap", "Bt1", "R"), top_cm = c(0, 20, 45),
                                                         bottom_cm = c(20, 45, 65)), site = list(parent_material = "granite")),
  "L2.4.2" = list(base = "L2.4.5", set = list(foreign_fragments_pct = c(5, NA, NA, NA), artefacts_pct = c(1, 0, 0, 0),
                                              clay_pct = c(18, 40, 30, 22), silt_pct = c(55, 42, 47, 50), sand_pct = c(27, 18, 23, 28),
                                              munsell_chroma_moist = c(3, 6, 4, 4))),
  "L2.4.3" = list(base = "L2.4.5", set = list(caco3_pct = c(6, 4, 2, 1))),
  "L2.4.4" = list(base = "L2.4.5", set = list(mottle_colors = c(NA, NA, "7.5YR 5/6", NA))),
  "L2.4.5" = list(base = "L2.4", set = cf0),
  "L3.1.1" = list(base = "L3.1.2", set = list(oc_pct = c(3.0, 1.2, 0.6, NA), bulk_density_g_cm3 = c(1.0, 1.2, 1.3, NA),
                                              humus_coatings = c(NA, "common", "common", NA))),
  "L3.1.2" = list(base = "L3.1"),
  "L3.2.1" = list(base = "L3.2.3", set = list(sio2_al2o3_clay = c(NA, 1.8, 1.8, NA), sio2_al2o3_naoh = c(NA, 0.8, 0.8, NA),
                                              sio2_sulfuric_pct = c(NA, 15, 15, NA), al2o3_sulfuric_pct = c(NA, 17, 17, NA))),
  "L3.2.2" = list(base = "L3.2.3", set = list(oc_pct = c(3.0, 1.2, 0.6, 0.2), bulk_density_g_cm3 = c(1.0, 1.2, 1.3, 1.4),
                                              humus_coatings = c(NA, "common", "common", NA))),
  "L3.2.3" = list(base = "L3.2", set = list(sio2_al2o3_clay = c(NA, 2.4, 2.4, NA), sio2_al2o3_naoh = c(NA, 1.5, 1.5, NA),
                                            sio2_sulfuric_pct = c(NA, 20, 20, NA), al2o3_sulfuric_pct = c(NA, 15, 15, NA))),
  "L3.3.1" = list(base = "L3.3.3", set = list(oc_pct = c(3.0, 1.2, 0.6, 0.2), bulk_density_g_cm3 = c(1.0, 1.2, 1.3, 1.4),
                                              humus_coatings = c(NA, "common", "common", NA))),
  "L3.3.2" = list(base = "L3.3.3", set = list(al_kcl_cmol = c(1, 6, 1, 1), ph_kcl = c(4.5, 3.8, 4.5, 4.5))),
  "L3.3.3" = list(base = "L3.3"),
  "L4.1.1" = list(base = "L4.1.3", rows = c(1L, 2L, 3L, 3L, 4L),
                  set = list(top_cm = c(0, 20, 60, 68, 100), bottom_cm = c(20, 60, 68, 100, 140),
                             designation = c("A", "E", "Btc", "Bt", "C"), fe_mn_nodules_pct = c(NA, NA, 35, NA, NA),
                             cementation_class = c(NA, NA, "strongly", NA, NA))),
  "L4.1.2" = list(base = "L4.1"),
  "L4.1.3" = list(base = "L4.1", set = list(fe_dcb_pct = 0.8, fe_total_pct = 3)),
  "L4.2.1" = list(base = "L4.2.3", set = list(oc_pct = c(3.0, 1.2, 0.6, NA), bulk_density_g_cm3 = c(1.0, 1.2, 1.3, NA),
                                              humus_coatings = c(NA, "common", "common", NA))),
  "L4.2.2" = list(base = "L4.2.1", set = list(munsell_hue_moist = "2.5YR")),
  "L4.2.3" = list(base = "L4.2"),
  "L4.3.1" = list(base = "L4.3.5", rows = c(2L, 2L, 3L, 4L),
                  set = list(top_cm = c(0, 20, 60, 100), bottom_cm = c(20, 60, 100, 140), designation = c("Bt1", "Bt2", "Bt3", "C"),
                             clay_pct = c(30, 45, 40, 30), silt_pct = c(42, 35, 38, 42), sand_pct = c(28, 20, 22, 28),
                             structure_type = c("blocky", "prismatic", "blocky", "massive"),
                             micromorph_features = c(NA, "clay_stripes", NA, NA),
                             clay_films_strength = c("distinct", "distinct", "distinct", NA),
                             clay_films_amount = c("common", "common", "common", NA))),
  "L4.3.2" = list(base = "L4.3.5", set = list(shajiang_pct = c(NA, NA, 15, NA))),
  "L4.3.3" = list(base = "L4.3.5", set = list(ph_h2o = c(6.5, 7.2, 7.3, 7.5))),
  "L4.3.4" = list(base = "L4.3.5", set = list(bs_pct = c(55, 40, 40, 65))),
  "L4.3.5" = list(base = "L4.3"),
  "L4.4.1" = list(base = "L4.4.5", set = list(sio2_al2o3_clay = c(NA, 1.8, 1.8, NA), sio2_al2o3_naoh = c(NA, 0.8, 0.8, NA),
                                              sio2_sulfuric_pct = c(NA, 15, 15, NA), al2o3_sulfuric_pct = c(NA, 17, 17, NA))),
  "L4.4.2" = list(base = "L4.4.5", rows = 1:3, set = list(designation = c("A", "Bt1", "R"), top_cm = c(0, 15, 45),
                                                         bottom_cm = c(15, 45, 65))),
  "L4.4.3" = list(base = "L4.4.5", set = list(oc_pct = c(3.0, 1.2, 0.6, 0.2), bulk_density_g_cm3 = c(1.0, 1.2, 1.3, 1.4),
                                              humus_coatings = c(NA, "common", "common", NA))),
  "L4.4.4" = list(base = "L4.4.5", site = list(udic_perudic_border = TRUE)),
  "L4.4.5" = list(base = "L4.4", set = c(cf0, list(sio2_al2o3_clay = c(NA, 2.4, 2.4, NA), sio2_al2o3_naoh = c(NA, 1.5, 1.5, NA),
                                                   sio2_sulfuric_pct = c(NA, 20, 20, NA), al2o3_sulfuric_pct = c(NA, 15, 15, NA))),
                  site = list(udic_perudic_border = FALSE)),
  "L4.5.1" = list(base = "L4.5.5", set = list(al_kcl_cmol = c(0.2, 6, 0.2, 0.2), ph_kcl = c(5.0, 3.8, 5.2, 5.4))),
  "L4.5.2" = list(base = "L4.5.5", set = list(munsell_hue_moist = c("10YR", "5YR", "5YR", "10YR"))),
  "L4.5.3" = list(base = "L4.5"),
  "L4.5.4" = list(base = "L4.5.5", set = list(mottle_colors = c(NA, NA, "7.5YR 5/6", NA))),
  "L4.5.5" = list(base = "L4.5", set = list(fe_dcb_pct = 0.8, fe_total_pct = 3)),
  "L4.6.1" = list(base = "L4.6.7", rows = 1:3, set = list(designation = c("A", "Bt1", "R"), top_cm = c(0, 20, 45),
                                                         bottom_cm = c(20, 45, 65)), site = list(parent_material = "granite")),
  "L4.6.2" = list(base = "L4.6.7", set = list(oc_pct = c(3.0, 1.2, 0.6, 0.2), bulk_density_g_cm3 = c(1.0, 1.2, 1.3, 1.4),
                                              humus_coatings = c(NA, "common", "common", NA))),
  "L4.6.3" = list(base = "L4.6.7", set = list(designation = c("Ap", "Bt1", "Bt2", "C"), humus_clay_films_mm = c(NA, 0.6, NA, NA),
                                              humus_clay_films_pct = c(NA, 8, NA, NA))),
  "L4.6.4" = list(base = "L4.6.7", rows = c(1L, 1L, 2L, 3L, 4L),
                  set = list(top_cm = c(0, 15, 25, 60, 100), bottom_cm = c(15, 25, 60, 100, 140),
                             designation = c("A", "E", "Bt1", "Bt2", "C"), munsell_hue_moist = c("7.5YR", "10YR", "7.5YR", "7.5YR", "7.5YR"),
                             munsell_value_moist = c(4, 6, 4, 4, 5), munsell_chroma_moist = c(4, 2, 6, 6, 6),
                             munsell_value_dry = c(5, 7, 5, 5, 6), clay_pct = c(20, 14, 34, 36, 30))),
  "L4.6.5" = list(base = "L4.6", set = cf0),
  "L4.6.6" = list(base = "L4.6.7", set = list(mottle_colors = c(NA, NA, "7.5YR 5/6", NA))),
  "L4.6.7" = list(base = "L4.6", set = c(cf0, list(munsell_hue_moist = "7.5YR"))),
  "L4.7.1" = list(base = "L4.7.5", rows = 1:3, set = list(designation = c("A", "Bt1", "R"), top_cm = c(0, 20, 45),
                                                         bottom_cm = c(20, 45, 65)), site = list(parent_material = "granite")),
  "L4.7.2" = list(base = "L4.7.5", set = list(designation = c("Ap", "Bt1", "Bt2", "C"), humus_clay_films_mm = c(NA, 0.6, NA, NA),
                                              humus_clay_films_pct = c(NA, 8, NA, NA))),
  "L4.7.3" = list(base = "L4.7.5", rows = c(1L, 1L, 2L, 3L, 4L),
                  set = list(top_cm = c(0, 15, 25, 60, 100), bottom_cm = c(15, 25, 60, 100, 140),
                             designation = c("A", "E", "Bt1", "Bt2", "C"), munsell_hue_moist = c("10YR", "10YR", "7.5YR", "7.5YR", "10YR"),
                             munsell_value_moist = c(4, 6, 4, 4, 5), munsell_chroma_moist = c(4, 2, 6, 6, 6),
                             munsell_value_dry = c(5, 7, 5, 5, 6), clay_pct = c(20, 14, 34, 36, 30))),
  "L4.7.4" = list(base = "L4.7.5", set = list(mottle_colors = c(NA, NA, "7.5YR 5/6", NA))),
  "L4.7.5" = list(base = "L4.7", set = cf0),
  # --- M ---
  "M1.1.1" = list(base = "M1.1.2", set = list(water_saturation_days = c(60, 10, 10))),
  "M1.1.2" = list(base = "M1.1"),
  "M1.2.1" = list(base = "M1.2.4", set = list(water_saturation_days = c(60, 10, 10))),
  "M1.2.2" = list(base = "M1.2.4", set = list(top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 120), oc_pct = c(4, 1.5, 0.4),
                                              munsell_value_moist = c(3, 4, 5), munsell_chroma_moist = c(2, 4, 3),
                                              munsell_value_dry = c(5, 6, 6), bulk_density_g_cm3 = c(1.0, 1.2, 1.4),
                                              root_mat_pct = NA, root_mat = NA, n_total_pct = c(0.3, 0.1, 0.05))),
  "M1.2.3" = list(base = "M1.2.4", rows = c(1L, 2L, 3L, 3L),
                  set = list(top_cm = c(0, 8, 40, 60), bottom_cm = c(8, 40, 60, 120), designation = c("Ah", "Bw", "C", "Cg"),
                             munsell_hue_moist = c("10YR", "7.5YR", "10YR", "5Y"), munsell_chroma_moist = c(2, 4, 3, 1),
                             childs_test = c(NA, NA, NA, "positive"))),
  "M1.2.4" = list(base = "M1.2", set = list(bottom_cm = c(8, 40, 120))),
  "M1.3.1" = list(base = "M1.3.4", set = list(designation = c("Ah", "Bw", "Ck"), caco3_pct = c(0, 0, 15),
                                              secondary_carbonates_pct = c(0, 0, 8), ph_h2o = c(6.5, 6.5, 8.0))),
  "M1.3.2" = list(base = "M1.3.4", set = list(caco3_pct = 2, ph_h2o = 7.8)),
  "M1.3.3" = list(base = "M1.3.4", set = list(bs_pct = c(60, 40, 40))),
  "M1.3.4" = list(base = "M1.3", set = list(bottom_cm = c(8, 40, 120))),
  "M1.4.1" = list(base = "M1.4.5", rows = c(1L, 1L, 2L, 3L),
                  set = list(top_cm = c(0, 6, 25, 50), bottom_cm = c(6, 25, 50, 110), designation = c("Oa", "Ah", "Bw", "C"),
                             oc_pct = c(20, 3, 1, 0.4), clay_pct = c(NA, 18, 20, 15), silt_pct = c(NA, 50, 50, 48),
                             sand_pct = c(NA, 32, 30, 37), water_saturation_days = c(60, 10, 10, 10),
                             bulk_density_g_cm3 = c(0.3, 1.1, 1.3, 1.4), n_total_pct = c(1, 0.25, 0.1, 0.05))),
  "M1.4.2" = list(base = "M1.4.5", set = list(designation = c("Ah", "Bw", "Ck"), caco3_pct = c(0, 0, 15),
                                              secondary_carbonates_pct = c(0, 0, 8), ph_h2o = c(6.5, 6.5, 8.0))),
  "M1.4.3" = list(base = "M1.4.5", set = list(caco3_pct = 2, ph_h2o = 7.8)),
  "M1.4.4" = list(base = "M1.4.5", set = list(mottle_colors = c(NA, NA, "7.5YR 5/6"))),
  "M1.4.5" = list(base = "M1.4"),
  "M1.5.1" = list(base = "M1.5.6", rows = c(1L, 1L, 2L, 3L),
                  set = list(top_cm = c(0, 6, 25, 50), bottom_cm = c(6, 25, 50, 110), designation = c("Oa", "Ah", "Bw", "C"),
                             oc_pct = c(20, 3, 1, 0.4), clay_pct = c(NA, 18, 20, 15), silt_pct = c(NA, 50, 50, 48),
                             sand_pct = c(NA, 32, 30, 37), water_saturation_days = c(60, 10, 10, 10),
                             bulk_density_g_cm3 = c(0.3, 1.1, 1.3, 1.4), n_total_pct = c(1, 0.25, 0.1, 0.05))),
  "M1.5.2" = list(base = "M1.5.6", set = list(ph_h2o = c(5.5, 5.0, 5.5), oc_pct = c(3, 1.2, 0.4))),
  "M1.5.3" = list(base = "M1.5.6", set = list(mottle_colors = c(NA, NA, "7.5YR 5/6")),
                  site = list(soil_moisture_regime = "stagnic", climatic_moisture_regime = "udic",
                              drainage_class = "imperfectly drained")),
  "M1.5.4" = list(base = "M1.5.6", rows = c(1L, 2L, 2L, 3L),
                  set = list(top_cm = c(0, 25, 35, 50), bottom_cm = c(25, 35, 50, 110), designation = c("Ah", "E", "Bw", "C"),
                             munsell_hue_moist = c("10YR", "10YR", "7.5YR", "10YR"), munsell_value_moist = c(3, 6, 4, 5),
                             munsell_chroma_moist = c(2, 2, 4, 3), munsell_value_dry = c(5, 7, 6, 6), oc_pct = c(3, 0.6, 1.0, 0.4))),
  "M1.5.5" = list(base = "M1.5.6", set = list(mottle_colors = c(NA, NA, "7.5YR 5/6"))),
  "M1.5.6" = list(base = "M1.5"),
  "M1.6.1" = list(base = "M1.6.6", rows = c(2L, 3L), set = list(top_cm = c(0, 32), bottom_cm = c(32, 112))),
  "M1.6.2" = list(base = "M1.6.6", set = list(designation = c("Ah", "Bw", "Ck"), caco3_pct = c(0, 0, 6), ph_h2o = c(6.5, 6.5, 8.0))),
  "M1.6.3" = list(base = "M1.6.6", set = list(caco3_pct = 2, ph_h2o = 7.8)),
  "M1.6.4" = list(base = "M1.6.6", rows = c(1L, 2L, 2L, 3L),
                  set = list(top_cm = c(0, 8, 18, 40), bottom_cm = c(8, 18, 40, 120), designation = c("Ah", "E", "Bw", "C"),
                             munsell_hue_moist = c("10YR", "10YR", "7.5YR", "10YR"), munsell_value_moist = c(3, 6, 4, 5),
                             munsell_chroma_moist = c(2, 2, 4, 3), munsell_value_dry = c(5, 7, 6, 6), oc_pct = c(15, 0.6, 1.5, 0.4))),
  "M1.6.5" = list(base = "M1.6.6", set = list(mottle_colors = c(NA, NA, "7.5YR 5/6"))),
  "M1.6.6" = list(base = "M1.6", set = list(bottom_cm = c(8, 40, 120))),
  "M2.1.1" = list(base = "M2.1.4", set = list(munsell_hue_moist = c("10YR", "10YR", "7.5YR", "5Y", "10YR"),
                                              munsell_chroma_moist = c(1, 3, 4, 1, 3), childs_test = c(NA, NA, "negative", "positive", NA))),
  "M2.1.2" = list(base = "M2.1.4", set = list(esp_pct = c(2, 35, 2, 2, 2), na_cmol = c(0.2, 3, 0.2, 0.2, 0.2))),
  "M2.1.3" = list(base = "M2.1.4", set = list(soluble_salts_pct = c(0.05, 0.05, 0.5, 0.05, 0.05))),
  "M2.1.4" = list(base = "M2.1", set = list(na_cmol = 0.2, mg_cmol = 1)),
  "M2.2.2" = list(base = "M2.2.6", set = list(esp_pct = c(35, 2, 2, 2), na_cmol = c(3, 0.2, 0.2, 0.2))),
  "M2.2.3" = list(base = "M2.2.6", set = list(soluble_salts_pct = c(0.5, 0.05, 0.05, 0.05)), site = list(salic_months_per_year = 2)),
  "M2.2.4" = list(base = "M2.2.6", rows = c(1L, 2L, 2L, 3L, 4L),
                  set = list(top_cm = c(0, 20, 30, 60, 100), bottom_cm = c(20, 30, 60, 100, 130),
                             designation = c("Ap", "E", "Bw", "Bg", "C"), munsell_hue_moist = c("10YR", "10YR", "7.5YR", "10YR", "10YR"),
                             munsell_value_moist = c(4, 6, 4, 5, 5), munsell_chroma_moist = c(3, 2, 4, 3, 3),
                             munsell_value_dry = c(6, 7, 6, 6, 7), mottle_colors = c(NA, NA, "7.5YR 5/6", "7.5YR 5/8", NA),
                             shajiang_pct = c(NA, NA, 15, 5, NA))),
  "M2.2.5" = list(base = "M2.2.6", set = list(clay_pct = c(32, 35, 35, 30), silt_pct = c(45, 42, 42, 45), sand_pct = c(23, 23, 23, 25),
                                              cracks_width_cm = c(0.6, 0.6, NA, NA), slickensides = c(NA, "few", "few", NA))),
  "M2.2.6" = list(base = "M2.2", set = list(na_cmol = 0.2, mg_cmol = 1)),
  "M2.3.2" = list(base = "M2.3.4", rows = c(1L, 2L, 2L, 3L, 4L),
                  set = list(top_cm = c(0, 30, 40, 60, 100), bottom_cm = c(30, 40, 60, 100, 130),
                             designation = c("Ap", "E", "Bw", "Bg", "C"), munsell_hue_moist = c("10YR", "10YR", "7.5YR", "10YR", "10YR"),
                             munsell_value_moist = c(2, 6, 4, 5, 5), munsell_chroma_moist = c(2, 2, 4, 3, 3),
                             munsell_value_dry = c(4, 7, 6, 6, 7), mottle_colors = c(NA, NA, "7.5YR 5/6", "7.5YR 5/8", NA),
                             oc_pct = c(2.5, 0.5, 0.5, 0.3, 0.2), bulk_density_g_cm3 = c(1.1, 1.3, 1.35, 1.4, 1.45))),
  "M2.3.3" = list(base = "M2.3.4", set = list(ph_h2o = c(5.0, 5.0, 6.8, 6.8))),
  "M2.3.4" = list(base = "M2.3"),
  "M2.4.2" = list(base = "M2.4.5", set = list(soluble_salts_pct = c(0.05, 0.5, 0.05, 0.05))),
  "M2.4.3" = list(base = "M2.4.5", set = list(caco3_pct = 2, ph_h2o = 7.8)),
  "M2.4.4" = list(base = "M2.4.5", set = list(ph_h2o = c(5.0, 5.0, 6.8, 6.8))),
  "M2.4.5" = list(base = "M2.4", set = list(na_cmol = 0.2, mg_cmol = 1)),
  "M3.1.1" = list(base = "M3.1.6", set = list(munsell_chroma_moist = c(2, 6, 3), mottle_colors = c("7.5YR 5/6", NA, NA),
                                              childs_test = c("negative", NA, NA)),
                  site = list(land_use = "paddy rice", paddy_years_frac = 0.4, soil_moisture_regime = "anthrostagnic",
                              climatic_moisture_regime = "ustic")),
  "M3.1.2" = list(base = "M3.1.6", set = list(designation = c("Ap", "Bw", "Ck"), caco3_pct = c(0.5, 0.5, 15),
                                              secondary_carbonates_pct = c(0, 0, 8), ph_h2o = c(7.2, 7.2, 8.0))),
  "M3.1.3" = list(base = "M3.1.6", set = list(esp_pct = c(2, 35, 10), na_cmol = c(0.2, 3, 1))),
  "M3.1.4" = list(base = "M3.1.6", set = list(soluble_salts_pct = c(0.5, 0.05, 0.05))),
  "M3.1.5" = list(base = "M3.1.6", set = list(mottle_colors = c(NA, NA, "7.5YR 5/6"))),
  "M3.1.6" = list(base = "M3.1", set = list(na_cmol = 0.2, mg_cmol = 1, bottom_cm = c(30, 70, 150))),
  "M3.2.1" = list(base = "M3.2.4", set = list(designation = c("Ap", "Bw", "R"), top_cm = c(0, 20, 45), bottom_cm = c(20, 45, 65))),
  "M3.2.2" = list(base = "M3.2.4", rows = c(2L, 3L), set = list(top_cm = c(0, 50), bottom_cm = c(50, 100))),
  "M3.2.3" = list(base = "M3.2.4", set = list(bs_pct = c(90, 40, 90))),
  "M3.2.4" = list(base = "M3.2", set = cf0),
  "M3.3.1" = list(base = "M3.3.5", set = list(soluble_salts_pct = c(0.05, 0.5, 0.05)), site = list(salic_months_per_year = 2)),
  "M3.3.2" = list(base = "M3.3.5", rows = c(1L, 2L, 3L, 3L),
                  set = list(top_cm = c(0, 20, 80, 110), bottom_cm = c(20, 80, 110, 140), designation = c("Ap", "Bw", "Btn", "C"),
                             clay_pct = c(18, 20, 30, 15), silt_pct = c(52, 50, 45, 55), sand_pct = c(30, 30, 25, 30),
                             structure_type = c("granular", "blocky", "columnar", "massive"),
                             esp_pct = c(2, 4, 35, 20), na_cmol = c(0.2, 0.4, 4, 2), ph_h2o = c(7.2, 7.4, 9.2, 9.0),
                             mottle_colors = c(NA, "7.5YR 5/6", NA, NA))),
  "M3.3.3" = list(base = "M3.3.5", set = list(caco3_pct = 2, ph_h2o = 7.8)),
  "M3.3.4" = list(base = "M3.3.5", set = list(top_cm = c(0, 25, 70), bottom_cm = c(25, 70, 120), munsell_value_moist = c(3, 5, 5), munsell_chroma_moist = c(2, 6, 3),
                                              munsell_value_dry = c(5, 6, 7), oc_pct = c(1.5, 0.5, 0.2))),
  "M3.3.5" = list(base = "M3.3"),
  "M3.4.1" = list(base = "M3.4.2", set = list(designation = c("Ap", "Bw", "Ck"), caco3_pct = c(0.5, 0.5, 15),
                                              secondary_carbonates_pct = c(0, 0, 8), ph_h2o = c(7.2, 7.2, 8.0))),
  "M3.4.2" = list(base = "M3.4"),
  "M3.5.1" = list(base = "M3.5"),
  "M4.1.1" = list(base = "M4.1.5", set = list(oc_pct = c(3.5, 1.2, 0.6), bulk_density_g_cm3 = c(1.0, 1.2, 1.3),
                                              humus_coatings = c(NA, "common", NA))),
  "M4.1.2" = list(base = "M4.1.5", set = list(munsell_value_moist = c(3, 4, 5), munsell_chroma_moist = c(3, 4, 4), oc_pct = c(3, 1.2, 0.3))),
  "M4.1.3" = list(base = "M4.1.5", rows = c(1L, 2L, 2L, 3L),
                  set = list(top_cm = c(0, 15, 25, 90), bottom_cm = c(15, 25, 90, 130), designation = c("A", "E", "Bw", "C"),
                             munsell_hue_moist = c("10YR", "10YR", "7.5YR", "10YR"), munsell_value_moist = c(3, 6, 5, 5),
                             munsell_chroma_moist = c(3, 2, 6, 4), munsell_value_dry = c(5, 7, 6, 7), oc_pct = c(3, 0.5, 0.5, 0.3))),
  "M4.1.4" = list(base = "M4.1.5", set = list(al_kcl_cmol = c(2, 6, 2))),
  "M4.1.5" = list(base = "M4.1"),
  "M4.2.1" = list(base = "M4.2.4", rows = c(1L, 2L, 2L, 3L),
                  set = list(top_cm = c(0, 15, 25, 90), bottom_cm = c(15, 25, 90, 130), designation = c("A", "E", "Bw", "C"),
                             munsell_hue_moist = c("10YR", "10YR", "7.5YR", "10YR"), munsell_value_moist = c(3, 6, 5, 5),
                             munsell_chroma_moist = c(3, 2, 6, 4), munsell_value_dry = c(5, 7, 6, 7), oc_pct = c(3, 0.5, 0.5, 0.3),
                             mottle_colors = c(NA, NA, "7.5YR 5/6", NA), childs_test = c(NA, "negative", NA, NA))),
  "M4.2.2" = list(base = "M4.2.4", rows = c(1L, 1L, 2L, 3L),
                  set = list(top_cm = c(0, 6, 15, 90), bottom_cm = c(6, 15, 90, 130), designation = c("Oa", "A", "Bw", "C"),
                             oc_pct = c(20, 3, 0.5, 0.3), clay_pct = c(NA, 22, 25, 20), silt_pct = c(NA, 45, 45, 48),
                             sand_pct = c(NA, 33, 30, 32), water_saturation_days = c(60, NA, NA, NA),
                             bulk_density_g_cm3 = c(0.3, NA, NA, NA))),
  "M4.2.3" = list(base = "M4.2.4", set = list(top_cm = c(0, 25, 90), bottom_cm = c(25, 90, 130))),
  "M4.2.4" = list(base = "M4.2"),
  "M4.3.1" = list(base = "M4.3.3", set = list(top_cm = c(0, 15, 45), bottom_cm = c(15, 45, 65)),
                  site = list(lithic_contact_extent_pct = 80)),
  "M4.3.2" = list(base = "M4.3.3", set = list(oc_pct = c(3.5, 1.2, NA), bulk_density_g_cm3 = c(1.0, 1.2, NA),
                                              humus_coatings = c(NA, "common", NA))),
  "M4.3.3" = list(base = "M4.3"),
  "M4.4.1" = list(base = "M4.4.5", set = list(designation = c("A", "Bw", "R"), top_cm = c(0, 15, 45), bottom_cm = c(15, 45, 65))),
  "M4.4.2" = list(base = "M4.4.5", rows = c(1L, 1L, 2L, 3L),
                  set = list(top_cm = c(0, 6, 15, 90), bottom_cm = c(6, 15, 90, 130), designation = c("Oa", "A", "Bw", "C"),
                             oc_pct = c(20, 3, 0.5, 0.3), clay_pct = c(NA, 22, 25, 20), silt_pct = c(NA, 45, 45, 48),
                             sand_pct = c(NA, 33, 30, 32), water_saturation_days = c(60, NA, NA, NA),
                             bulk_density_g_cm3 = c(0.3, NA, NA, NA))),
  "M4.4.3" = list(base = "M4.4.5", set = list(oc_pct = c(3.5, 1.2, 0.6), bulk_density_g_cm3 = c(1.0, 1.2, 1.3),
                                              humus_coatings = c(NA, "common", NA))),
  "M4.4.4" = list(base = "M4.4.5", set = list(mottle_colors = c(NA, "7.5YR 5/6", NA))),
  "M4.4.5" = list(base = "M4.4", set = cf0),
  "M4.5.1" = list(base = "M4.5.5", rows = c(1L, 1L, 2L, 3L),
                  set = list(top_cm = c(0, 6, 15, 90), bottom_cm = c(6, 15, 90, 130), designation = c("Oa", "A", "Bw", "C"),
                             oc_pct = c(20, 3, 0.5, 0.3), clay_pct = c(NA, 22, 25, 20), silt_pct = c(NA, 45, 45, 48),
                             sand_pct = c(NA, 33, 30, 32), water_saturation_days = c(60, NA, NA, NA),
                             bulk_density_g_cm3 = c(0.3, NA, NA, NA))),
  "M4.5.2" = list(base = "M4.5.5", set = list(oc_pct = c(3.5, 1.2, 0.6), bulk_density_g_cm3 = c(1.0, 1.2, 1.3),
                                              humus_coatings = c(NA, "common", NA))),
  "M4.5.3" = list(base = "M4.5.5", rows = c(1L, 2L, 2L, 3L),
                  set = list(top_cm = c(0, 15, 50, 90), bottom_cm = c(15, 50, 90, 130), designation = c("A", "Bw1", "Bw2", "C"),
                             al_kcl_cmol = c(2, 6, 2, 2), ph_kcl = c(4.3, 3.8, 4.3, 4.3))),
  "M4.5.4" = list(base = "M4.5.5", set = list(fe_dcb_pct = c(0.8, 2.0, 0.8))),
  "M4.5.5" = list(base = "M4.5", set = list(fe_dcb_pct = 0.8, fe_total_pct = 3)),
  "M4.6.1" = list(base = "M4.6.4", set = list(designation = c("A", "Bw", "R"), top_cm = c(0, 15, 45), bottom_cm = c(15, 45, 65)),
                  site = list(parent_material = "granite")),
  "M4.6.2" = list(base = "M4.6.4", set = list(oc_pct = c(3.5, 1.2, 0.6), bulk_density_g_cm3 = c(1.0, 1.2, 1.3),
                                              humus_coatings = c(NA, "common", NA))),
  "M4.6.3" = list(base = "M4.6.4", set = list(fe_dcb_pct = c(0.8, 2.0, 0.8))),
  "M4.6.4" = list(base = "M4.6", set = c(cf0, list(fe_dcb_pct = 0.8, fe_total_pct = 3))),
  "M5.1.1" = list(base = "M5.1.5", rows = c(1L, 2L, 2L, 3L, 4L),
                  set = list(top_cm = c(0, 15, 25, 60, 100), bottom_cm = c(15, 25, 60, 100, 130),
                             designation = c("A", "E", "Bw", "C", "R"), munsell_hue_moist = c("10R", "10YR", "10RP", "10RP", NA),
                             munsell_value_moist = c(4, 6, 4, 4, NA), munsell_chroma_moist = c(3, 2, 3, 3, NA),
                             munsell_value_dry = c(5, 7, 5, 5, NA))),
  "M5.1.2" = list(base = "M5.1.5", set = list(bs_pct = c(70, 40, 70, 70))),
  "M5.1.3" = list(base = "M5.1.5", set = list(top_cm = c(0, 25, 60, 100), bottom_cm = c(25, 60, 100, 130),
                                              munsell_value_moist = c(3, 4, 4, NA), munsell_value_dry = c(4, 5, 5, NA),
                                              oc_pct = c(2.0, 0.5, 0.2, NA))),
  "M5.1.4" = list(base = "M5.1.5", set = list(mottle_colors = c(NA, NA, "7.5YR 5/6", NA))),
  "M5.1.5" = list(base = "M5.1"),
  "M5.2.1" = list(h = list(designation = c("A", "Bw", "C1", "C2"), top_cm = c(0, 15, 60, 100), bottom_cm = c(15, 60, 100, 150),
                           clay_pct = 15, silt_pct = 25, sand_pct = 60, oc_pct = c(1.2, 0.5, 0.3, 0.2),
                           munsell_hue_moist = c("10YR", "7.5YR", "10YR", "10YR"), munsell_value_moist = c(4, 5, 6, 6),
                           munsell_chroma_moist = c(3, 6, 4, 4), munsell_value_dry = c(6, 6, 7, 7),
                           structure_type = c("granular", "blocky", "massive", "massive"),
                           structure_grade = c("moderate", "moderate", "structureless", "structureless"),
                           caco3_pct = c(55, 60, 65, 70), p_total_pct = c(1.0, 0.8, 0.5, 0.3), ph_h2o = 8.2, bs_pct = 100,
                           cec_cmol = 8, coarse_fragments_pct = 0),
                  site = list(soil_moisture_regime = "udic", soil_temperature_regime = "hyperthermic", parent_material = "coral sand")),
  "M5.2.2" = list(base = "M5.2.7", rows = c(1L, 2L, 4L), set = list(top_cm = c(0, 15, 45), bottom_cm = c(15, 45, 65)),
                  site = list(lithic_contact_extent_pct = 80)),
  "M5.2.3" = list(base = "M5.2.7", rows = 2:4, set = list(top_cm = c(0, 45, 85), bottom_cm = c(45, 85, 115))),
  "M5.2.4" = list(base = "M5.2.7", set = list(ph_h2o = c(6.0, 6.0, 6.2, NA), bs_pct = c(80, 75, 80, NA))),
  "M5.2.5" = list(base = "M5.2.7", set = list(oc_pct = c(3.5, 1.2, 0.6, NA), bulk_density_g_cm3 = c(1.0, 1.2, 1.3, NA),
                                              humus_coatings = c(NA, "common", NA, NA))),
  "M5.2.6" = list(base = "M5.2"),
  "M5.2.7" = list(base = "M5.2", set = list(munsell_hue_moist = c("2.5YR", "2.5YR", "2.5YR", NA))),
  "M5.3.1" = list(base = "M5.3.6", set = list(caco3_pct = c(2, 2, 2, 0), ph_h2o = 7.8)),
  "M5.3.2" = list(base = "M5.3.6", set = list(bs_pct = c(70, 40, 70, 70))),
  "M5.3.3" = list(base = "M5.3.6", rows = 2:4, set = list(top_cm = c(0, 45, 85), bottom_cm = c(45, 85, 115))),
  "M5.3.4" = list(base = "M5.3.6", set = list(designation = c("Ap", "Bw", "C", "R"), humus_clay_films_mm = c(NA, 0.6, NA, NA),
                                              humus_clay_films_pct = c(NA, 8, NA, NA))),
  "M5.3.5" = list(base = "M5.3.6", set = list(mottle_colors = c(NA, NA, "7.5YR 5/6", NA))),
  "M5.3.6" = list(base = "M5.3"),
  "M5.4.1" = list(base = "M5.4"),
  "M5.4.2" = list(base = "M5.4.8", rows = c(2L, 3L), set = list(top_cm = c(0, 45), bottom_cm = c(45, 115))),
  "M5.4.3" = list(base = "M5.4.8", set = list(oc_pct = c(3.5, 1.2, 0.6), bulk_density_g_cm3 = c(1.0, 1.2, 1.3),
                                              humus_coatings = c(NA, "common", NA))),
  "M5.4.4" = list(base = "M5.4.8", site = list(udic_perudic_border = TRUE)),
  "M5.4.5" = list(base = "M5.4.8", set = list(mottle_colors = c(NA, NA, "7.5YR 5/6"))),
  "M5.4.6" = list(base = "M5.4.8", set = plin3),
  "M5.4.7" = list(base = "M5.4.8", set = list(cole_value = 0.1, cracks_width_cm = c(0.6, 0.6, NA), slickensides = c(NA, "few", "few"))),
  "M5.4.8" = list(base = "M5.4", rows = 1:3, set = c(cf0, list(bottom_cm = c(15, 60, 130))), site = list(udic_perudic_border = FALSE)),
  "M5.5.1" = list(base = "M5.5.4", rows = 2:4, set = list(top_cm = c(0, 45, 85), bottom_cm = c(45, 85, 115))),
  "M5.5.2" = list(base = "M5.5"),
  "M5.5.3" = list(base = "M5.5.4", set = list(top_cm = c(0, 25, 60, 100), bottom_cm = c(25, 60, 100, 130),
                                              munsell_value_moist = c(3, 4, 4, NA), munsell_value_dry = c(4, 5, 5, NA),
                                              oc_pct = c(2.0, 0.5, 0.2, NA))),
  "M5.5.4" = list(base = "M5.5", set = list(munsell_hue_moist = c("7.5YR", "7.5YR", "7.5YR", NA), fe_dcb_pct = 2.0, fe_total_pct = 4)),
  "M5.6.1" = list(base = "M5.6.4", rows = 2:4, set = list(top_cm = c(0, 45, 85), bottom_cm = c(45, 85, 115))),
  "M5.6.2" = list(base = "M5.6.4", rows = c(1L, 2L, 2L, 3L, 4L),
                  set = list(top_cm = c(0, 15, 25, 60, 100), bottom_cm = c(15, 25, 60, 100, 130),
                             designation = c("A", "E", "Bw", "C", "R"), munsell_value_moist = c(4, 6, 4, 4, NA),
                             munsell_chroma_moist = c(3, 2, 3, 3, NA), munsell_value_dry = c(5, 7, 5, 5, NA))),
  "M5.6.3" = list(base = "M5.6.4", set = list(oc_pct = c(3.5, 1.2, 0.6, NA), bulk_density_g_cm3 = c(1.0, 1.2, 1.3, NA),
                                              humus_coatings = c(NA, "common", NA, NA))),
  "M5.6.4" = list(base = "M5.6"),
  "M5.7.1" = list(base = "M5.7.4", rows = c(1L, 2L, 2L, 3L, 4L),
                  set = list(top_cm = c(0, 15, 25, 60, 100), bottom_cm = c(15, 25, 60, 100, 130),
                             designation = c("A", "E", "Bw", "C", "R"), munsell_value_moist = c(4, 6, 4, 4, NA),
                             munsell_chroma_moist = c(3, 2, 3, 3, NA), munsell_value_dry = c(5, 7, 5, 5, NA))),
  "M5.7.2" = list(base = "M5.7.4", set = list(top_cm = c(0, 25, 60, 100), bottom_cm = c(25, 60, 100, 130),
                                              munsell_value_moist = c(3, 4, 4, NA), munsell_value_dry = c(4, 5, 5, NA),
                                              oc_pct = c(2.0, 0.5, 0.2, NA))),
  "M5.7.3" = list(base = "M5.7.4", set = list(mottle_colors = c(NA, NA, "7.5YR 5/6", NA))),
  "M5.7.4" = list(base = "M5.7"),
  # --- N ---
  "N1.1.1" = list(base = "N1.1.3", set = list(caco3_pct = 2, ph_h2o = 7.8)),
  "N1.1.2" = list(base = "N1.1.3", set = list(ph_h2o = c(5.0, 5.0, 7))),
  "N1.1.3" = list(base = "N1.1"),
  "N1.2.1" = list(base = "N1.2.5", set = list(soluble_salts_pct = c(0.05, 0.5, 0.05))),
  "N1.2.2" = list(base = "N1.2.5", set = list(mottle_colors = c(NA, "7.5YR 5/6", NA))),
  "N1.2.3" = list(base = "N1.2.5", set = list(caco3_pct = 2, ph_h2o = 7.8)),
  "N1.2.4" = list(base = "N1.2.5", set = list(ph_h2o = c(5.0, 5.0, 7))),
  "N1.2.5" = list(base = "N1.2"),
  "N2.1.1" = list(base = "N2.1.5", set = list(permafrost_temp_C = c(NA, -1)), site = list(frost_regime = NA)),
  "N2.1.2" = list(base = "N2.1.5", set = list(reduced_matrix_pct = 40, childs_test = "positive")),
  "N2.1.3" = list(base = "N2.1.5", set = list(ph_h2o = 5.0)),
  "N2.1.4" = list(base = "N2.1", site = list(frost_regime = "seasonal")),
  "N2.1.5" = list(base = "N2.1", set = list(caco3_pct = 0), site = list(frost_regime = "seasonal")),
  "N2.2.1" = list(base = "N2.2", set = list(na_cmol = 0.05, mg_cmol = 0.2)),
  "N2.2.2" = list(base = "N2.2.4", set = list(esp_pct = c(10, 2))),
  "N2.2.3" = list(base = "N2.2.4", set = list(soluble_salts_pct = c(0.5, 0.05))),
  "N2.2.4" = list(base = "N2.2", set = list(caco3_pct = 0, na_cmol = 0.05, mg_cmol = 0.2)),
  "N2.3.1" = list(base = "N2.3.5", set = list(mottle_colors = c(NA, "7.5YR 5/6"))),
  "N2.3.2" = list(base = "N2.3", set = list(na_cmol = 0.05, mg_cmol = 0.2)),
  "N2.3.3" = list(base = "N2.3.5", set = list(na_cmol = c(0.8, 0.05), mg_cmol = c(0.4, 0.2))),
  "N2.3.4" = list(base = "N2.3.5", set = list(soluble_salts_pct = c(0.8, 0.05))),
  "N2.3.5" = list(base = "N2.3", set = list(caco3_pct = 0, na_cmol = 0.05, mg_cmol = 0.2)),
  "N2.4.1" = list(base = "N2.4.5", site = list(soil_temperature_regime = "hyperthermic")),
  "N2.4.2" = list(base = "N2.4.5", set = list(mottle_colors = c(NA, "7.5YR 5/6"))),
  "N2.4.3" = list(base = "N2.4"),
  "N2.4.5" = list(base = "N2.4", set = list(caco3_pct = 0)),
  "N2.5.1" = list(base = "N2.5"),
  "N2.5.2" = list(base = "N2.5.3", rows = c(1L, 1L, 2L),
                  set = list(top_cm = c(0, 21, 40), bottom_cm = c(21, 40, 130), designation = c("Ap", "C1", "C2"),
                             clay_pct = c(5, 3, 3), silt_pct = c(10, 5, 5), sand_pct = c(85, 92, 92), oc_pct = c(0.46, 0.05, 0.05),
                             caco3_pct = c(0, 3, 3), ph_h2o = c(7.0, 8.0, 8.0), artefacts_pct = c(1, 0, 0),
                             silting_sieve_test = c("positive", NA, NA)), site = list(silting_rate_cm_yr = 0)),
  "N2.5.3" = list(base = "N2.5", set = list(caco3_pct = 0)),
  "N3.1.1" = list(base = "N3.1.3", set = list(permafrost_temp_C = c(NA, NA, NA, -1)), site = list(frost_regime = NA)),
  "N3.1.2" = list(base = "N3.1.3", set = list(mottle_colors = c(NA, "7.5YR 5/6", NA, NA))),
  "N3.1.3" = list(base = "N3.1", site = list(frost_regime = "seasonal")),
  "N3.2.1" = list(base = "N3.2.3", set = list(munsell_hue_moist = c("10YR", "10YR", "5Y", "10YR"), munsell_chroma_moist = c(3, 3, 1, 3),
                                              childs_test = c(NA, NA, "positive", NA))),
  "N3.2.2" = list(base = "N3.2"),
  "N3.2.3" = list(base = "N3.2", set = list(caco3_pct = 0, ph_h2o = 7)),
  "N3.3.1" = list(base = "N3.3.3", set = list(mottle_colors = c(NA, NA, "7.5YR 5/6", NA))),
  "N3.3.2" = list(base = "N3.3.3", set = list(soluble_salts_pct = c(0.05, 0.8, 0.05, 0.05))),
  "N3.3.3" = list(base = "N3.3"),
  "N3.4.1" = list(base = "N3.4.3", set = list(mottle_colors = c(NA, NA, "7.5YR 5/6", NA))),
  "N3.4.2" = list(base = "N3.4"),
  "N3.4.3" = list(base = "N3.4", set = list(caco3_pct = 0, ph_h2o = 7)),
  "N3.5.1" = list(base = "N3.5.3", set = list(mottle_colors = c(NA, NA, "7.5YR 5/6", NA))),
  "N3.5.2" = list(base = "N3.5.3", set = list(caco3_pct = c(3, 0, 5, 3), ph_h2o = c(7.8, 5.0, 7.8, 7.8))),
  "N3.5.3" = list(base = "N3.5"),
  "N4.1.1" = list(base = "N4.1.2", set = list(oc_pct = c(1.0, 0.6))),
  "N4.1.2" = list(base = "N4.1"),
  "N4.2.1" = list(base = "N4.2.3", set = list(caco3_pct = 2, ph_h2o = 7.8)),
  "N4.2.2" = list(base = "N4.2.3", set = list(ph_h2o = 5.0)),
  "N4.2.3" = list(base = "N4.2"),
  "N4.3.1" = list(base = "N4.3.3", set = list(caco3_pct = 2, ph_h2o = 7.8, bs_pct = 100)),
  "N4.3.2" = list(base = "N4.3"),
  "N4.3.3" = list(base = "N4.3", set = list(ph_h2o = 5.0, bs_pct = 40)),
  "N4.4.1" = list(base = "N4.4.5", set = list(permafrost_temp_C = c(NA, -1)), site = list(frost_regime = NA)),
  "N4.4.2" = list(base = "N4.4.5", set = list(pyroclastic_fragments_pct = 40)),
  "N4.4.3" = list(base = "N4.4.5", rows = c(1L, 2L, 2L), set = list(designation = c("A", "C", "R"), top_cm = c(0, 15, 45),
                                                                    bottom_cm = c(15, 45, 65))),
  "N4.4.4" = list(base = "N4.4.5", set = list(caco3_pct = 2, ph_h2o = 7.8)),
  "N4.4.5" = list(base = "N4.4", set = cf0),
  "N4.5.1" = list(base = "N4.5.5", set = list(mottle_colors = c(NA, "7.5YR 5/6"))),
  "N4.5.2" = list(base = "N4.5.5", set = list(caco3_pct = 2, ph_h2o = 7.8)),
  "N4.5.3" = list(base = "N4.5.5", set = list(esp_pct = c(35, 10), na_cmol = c(3, 0.8))),
  "N4.5.4" = list(base = "N4.5.5", set = list(soluble_salts_pct = c(0.8, 0.05))),
  "N4.5.5" = list(base = "N4.5", set = c(cf0, list(na_cmol = 0.1, mg_cmol = 0.5))),
  "N4.6.1" = list(base = "N4.6.4", rows = c(1L, 2L, 2L), set = list(designation = c("A", "C", "R"), top_cm = c(0, 15, 80),
                                                                    bottom_cm = c(15, 80, 100), ph_h2o = 7.5, bs_pct = 100),
                  site = list(parent_material = "limestone")),
  "N4.6.2" = list(base = "N4.6.4", rows = c(1L, 2L, 2L), set = list(designation = c("A", "C", "R"), top_cm = c(0, 15, 45),
                                                                    bottom_cm = c(15, 45, 65))),
  "N4.6.3" = list(base = "N4.6.4", set = list(caco3_pct = 2, ph_h2o = 7.8)),
  "N4.6.4" = list(base = "N4.6", set = cf0),
  "N4.7.1" = list(base = "N4.7.6", set = list(caco3_pct = c(55, 60), p_total_pct = c(0.5, 0.3), ph_h2o = 8.2, bs_pct = 100),
                  site = list(parent_material = "coral sand")),
  "N4.7.2" = list(base = "N4.7.6", set = list(pyroclastic_fragments_pct = 40)),
  "N4.7.3" = list(base = "N4.7.6", rows = c(1L, 2L, 2L), set = list(designation = c("A", "C", "R"), top_cm = c(0, 15, 80),
                                                                    bottom_cm = c(15, 80, 100), ph_h2o = 7.5, bs_pct = 100),
                  site = list(parent_material = "limestone")),
  "N4.7.4" = list(base = "N4.7.6", rows = c(1L, 2L, 2L),
                  set = list(designation = c("A", "C", "R"), top_cm = c(0, 12, 25), bottom_cm = c(12, 25, 45),
                             clay_pct = c(45, 48, NA), silt_pct = c(30, 30, NA), sand_pct = c(25, 22, NA),
                             cracks_width_cm = c(0.6, 0.6, NA), slickensides = c("few", "few", NA), cole_value = c(0.25, 0.25, NA))),
  "N4.7.5" = list(base = "N4.7.6", rows = c(1L, 2L, 2L), set = list(designation = c("A", "C", "R"), top_cm = c(0, 15, 45),
                                                                    bottom_cm = c(15, 45, 65))),
  "N4.7.6" = list(base = "N4.7", set = cf0),
  NULL)
}
