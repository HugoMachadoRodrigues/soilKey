# =============================================================================
# v0.9.316 -- HSCS 2024 subtype properties, the chemical group (Micheli et al.
# 2024, 8.5, p. 29-32, with the carbonatic soil materials of 8.4, p. 28): the
# degrees of carbonate, the base status (telitett, telitetlen, savanyu),
# gypsum, magnesium, the salts (szoloncsakos, szolonyeces, karbonat-szulfatos,
# karbonat-kloridos), the carbonate coatings (meszlepedekes) and the leached
# horizon (szologyos). Each .hscs_p_<id>(x) returns .hscs_prop() for the
# engine in R/hscs2024-properties.R. Three-valued throughout. Where the text
# is silent, the reading taken is written beside the code.
# =============================================================================

# ---- helpers ------------------------------------------------------------------

# A layer `min_th` cm or more thick with its top within `top_max` cm, as
# list(passed, top). `open`: the part of the profile not described (a gap, or
# the soil below a description that does not end on rock) may hold the layer
# (.hscs_run); FALSE for a feature recorded when met, which is absent there
# (readings.yaml, "Depths and the soil surface").
.hscs_chem_run <- function(x, ok, top_max, min_th, open = TRUE) {
  top <- .hscs_run_top(x, ok, top_max, min_th)
  if (!is.na(top)) return(list(passed = TRUE, top = top))
  if (open) return(list(passed = .hscs_run(x, ok, top_max, min_th), top = NA_real_))
  # closed: a run of described rows that may hold
  h <- x$h; p_top <- NA; p_th <- 0; prev <- NA
  for (i in x$ord) {
    t <- h$top_cm[i]; th <- x$thick[i]
    if (!is.na(prev) && abs(t - prev) > 0.5) { p_top <- NA; p_th <- 0 }
    if (!(ok[i] %in% FALSE)) {
      if (is.na(p_top)) p_top <- t
      p_th <- p_th + (if (is.na(th)) Inf else th)
      if (p_top < top_max - 0.25 && p_th >= min_th - 1e-9) return(list(passed = NA, top = NA_real_))
    } else { p_top <- NA; p_th <- 0 }
    prev <- h$bottom_cm[i]
  }
  list(passed = FALSE, top = NA_real_)
}

# The strongest of the forms of one property (strongest first): the first
# that holds when every stronger one fails; undecided when a stronger one is
# undecided (as the engine reads a row of degrees, Hugo's decision of
# 2026-10-30). Each form: list(passed, top, variant, layers, name).
.hscs_strongest_form <- function(forms, missing) {
  for (k in seq_along(forms)) {
    f <- forms[[k]]
    if (is.na(f$passed)) {
      weaker <- forms[seq_along(forms) > k]
      held <- vapply(weaker, function(w) isTRUE(w$passed), NA)
      note <- if (any(held)) sprintf("%s undecided; %s holds", f$name, weaker[[which(held)[1L]]]$name)
      return(.hscs_prop(NA, missing = missing, note = note))
    }
    if (isTRUE(f$passed)) return(.hscs_prop(TRUE, top = f$top, variant = f$variant, layers = f$layers))
  }
  .hscs_prop(FALSE)
}

# The field reaction to 10 % HCl as soilKey records it (none / slight /
# moderate / strong / very strong), with the usual synonyms.
.hscs_effervescence <- function(x) {
  e <- .hscs_txt(x$h$effervescence_class, x$n)
  out <- rep(NA_character_, x$n)
  out[grepl("^(none|no|nil|non)", e)] <- "none"
  out[is.na(out) & grepl("slight|weak", e)] <- "slight"
  out[is.na(out) & grepl("moderate|medium", e)] <- "moderate"
  out[is.na(out) & grepl("very strong|very high|violent|extreme", e)] <- "very strong"
  out[is.na(out) & grepl("strong|high", e)] <- "strong"
  out
}

# Secondary carbonates shown in the field in a row: those of
# .hscs_secondary_carbonate_rows (secondary_carbonates_pct or
# carbonate_soft_pct above 0, or a k suffix) and hard ones (nodules,
# concretions: carbonate_hard_pct above 0). Recorded when met.
.hscs_secondary_carbonates_any <- function(x) {
  hd <- x$h$carbonate_hard_pct %||% rep(NA_real_, x$n)
  .hscs_secondary_carbonate_rows(x) | (!is.na(hd) & hd > 0)
}

# ---- carbonate degrees (8.5 p. 29-31; materials 8.4 p. 28) --------------------

# The carbonatic soil material of a degree, per row. 8.4 joins a reaction to
# 10 % HCl and a CaCO3 content ("es", p. 28):
#   nyomokban: a very weak reaction ("csak hallhato", audible only), CaCO3
#     below 2 %;
#   gyengen: a weak reaction (+), CaCO3 2 % or more;
#   kozepesen: a strong reaction (++; +++), CaCO3 5-15 %;
#   eros: an explosive reaction (+++), CaCO3 15 % or more in the fine earth,
#     part of it secondary carbonate shown in the field.
# soilKey's classes are read as: slight = the very weak or the weak (+)
# reaction (as readings.yaml reads slight: it may be the trace class);
# moderate = weak (+); strong = ++; very strong = +++ (kozepesen or eros).
# Both recorded: both must hold (a slight reaction is then the very weak or
# the weak one as the CaCO3 says). Only CaCO3: it decides, with the reaction
# that goes with it in 8.4 (below 2 % but above 0: very weak; 2-5 %: weak, as
# 5 % or more goes with the strong reaction of kozepesen; 5-15 %: strong;
# 15 % or more: explosive); 0 % has no reaction. Only the reaction: it
# decides, with the CaCO3 that goes with it (slight leaves the very weak and
# the weak class undecided). Neither: a k suffix shows 2 % or more
# (readings.yaml), not which degree; otherwise undecided.
.hscs_carbonate_material_rows <- function(x, degree = c("nyomokban", "gyengen", "kozepesen", "eros")) {
  degree <- match.arg(degree)
  h <- x$h; n <- x$n
  ca <- h$caco3_pct %||% rep(NA_real_, n)
  eff <- .hscs_effervescence(x)
  react <- switch(degree, nyomokban = "slight", gyengen = c("slight", "moderate"),
                  kozepesen = c("strong", "very strong"), eros = "very strong")
  ca_both <- switch(degree, nyomokban = ca < 2, gyengen = ca >= 2,
                    kozepesen = ca >= 5 & ca <= 15, eros = ca >= 15)
  ca_only <- switch(degree, nyomokban = ca > 0 & ca < 2, gyengen = ca >= 2 & ca < 5,
                    kozepesen = ca >= 5 & ca <= 15, eros = ca >= 15)
  eff_only <- if (degree %in% c("nyomokban", "gyengen")) {
    ifelse(eff %in% "slight", NA, eff %in% react)
  } else eff %in% react
  by_k <- if (degree == "nyomokban") ifelse(x$des$k, FALSE, NA) else rep(NA, n)
  out <- ifelse(!is.na(ca) & !is.na(eff), ca_both & eff %in% react,
         ifelse(!is.na(ca), ca_only,
         ifelse(!is.na(eff), eff_only, by_k)))
  if (degree == "eros") out <- .and3v(out, .hscs_secondary_carbonates_any(x))
  out[x$rock] <- FALSE
  out
}

# A degree: 15 cm or more of its material within 100 cm of the surface ("a
# felszintol szamitott 100 cm-en belul legalabb 15 cm vastag", p. 29-31). The
# secondary carbonates of erosen are recorded when met, so the part not
# described holds none.
.hscs_p_carbonate_degree <- function(x, degree) {
  miss <- c("caco3_pct", "effervescence_class", if (degree == "eros") "secondary_carbonates_pct")
  if (is.na(x$surface)) return(.hscs_prop(NA, missing = miss))
  ok <- .hscs_carbonate_material_rows(x, degree)
  r <- .hscs_chem_run(x, ok, x$surface + 100, 15, open = degree != "eros")
  .hscs_prop(r$passed, top = r$top, layers = which(ok %in% TRUE),
             missing = if (is.na(r$passed)) miss else character(0))
}
.hscs_p_eros_karbonatos <- function(x) .hscs_p_carbonate_degree(x, "eros")
.hscs_p_kozepesen_karbonatos <- function(x) .hscs_p_carbonate_degree(x, "kozepesen")
.hscs_p_gyengen_karbonatos <- function(x) .hscs_p_carbonate_degree(x, "gyengen")
.hscs_p_nyomokban_karbonatos <- function(x) .hscs_p_carbonate_degree(x, "nyomokban")

# ---- base status (p. 32) ------------------------------------------------------

# A base status condition `ok` (per row) in the window of telitett and
# telitetlen (p. 32): "a talaj donto reszeben" (the major part, more than
# half) of 20-100 cm from the surface; or of 20 cm to continuous hard rock or
# a cemented layer (the text repeats no "major part" for this window: read as
# the same, the window ending higher); or, in a soil shallower than 20 cm
# (rock or a cemented layer at 20 cm or less), a layer 5 cm or more thick.
# As readings.yaml reads base saturation for the Mezosegi talajok, a
# description that ends (every bottom recorded) above the window's base is
# read to its end when the full window is undecided.
.hscs_base_window <- function(x, ok) {
  s <- x$surface
  if (is.na(s)) return(NA)
  stop <- min(c(s + 100, .hscs_rock_top(x), .hscs_cemented_top(x)), na.rm = TRUE)
  ok[x$rock] <- FALSE
  if (stop <= s + 20 + 0.25) {
    ok[!is.na(x$h$top_cm) & x$h$top_cm >= stop - 0.25] <- FALSE
    return(.hscs_run(x, ok, top_max = stop, min_th = 5, below_open = FALSE))
  }
  major <- function(zb) {
    sh <- .hscs_share(x, ok, s + 20, zb)
    if (sh[["lo"]] > 0.5 + 1e-9) TRUE else if (sh[["hi"]] <= 0.5 + 1e-9) FALSE else NA
  }
  v <- major(stop)
  if (is.na(v) && !anyNA(x$h$bottom_cm)) {
    deepest <- suppressWarnings(max(x$h$bottom_cm, na.rm = TRUE))
    if (is.finite(deepest) && deepest < stop - 0.25 && deepest > s + 20 + 0.25) v <- major(deepest)
  }
  v
}

# Base saturation (1 M NH4OAc, bs_pct) per row: above 80 % (`saturated`
# TRUE) or 50-80 % (FALSE). A calcareous row (2 % CaCO3 or more, as
# .hscs_calcareous_rows reads it) with no measure reads as saturated, above
# 80 % (readings.yaml: "a calcareous horizon reads as saturated").
.hscs_bs_rows <- function(x, saturated) {
  bs <- x$h$bs_pct %||% rep(NA_real_, x$n)
  calc <- .hscs_calcareous_rows(x) %in% TRUE
  in_r <- if (saturated) bs > 80 else bs >= 50 & bs <= 80
  ifelse(!is.na(bs), in_r, ifelse(calc, saturated, NA))
}

# Telitett: base saturation above 80 % ("80% feletti").
.hscs_p_telitett <- function(x) {
  v <- .hscs_base_window(x, .hscs_bs_rows(x, saturated = TRUE))
  .hscs_prop(v, missing = if (is.na(v)) c("bs_pct", "caco3_pct") else character(0))
}

# Telitetlen: base saturation 50-80 % ("50-80% kozotti").
.hscs_p_telitetlen <- function(x) {
  v <- .hscs_base_window(x, .hscs_bs_rows(x, saturated = FALSE))
  .hscs_prop(v, missing = if (is.na(v)) c("bs_pct", "caco3_pct") else character(0))
}

# Savanyu: "A pH(H2O) 5,5 vagy alacsonyabb" (p. 32), with no depth. Read in
# the window of the base status beside which the table lists it (the major
# part of 20-100 cm, or to shallower rock or a cemented layer; a layer 5 cm
# or more thick in a soil shallower than 20 cm).
.hscs_p_savanyu <- function(x) {
  ph <- x$h$ph_h2o %||% rep(NA_real_, x$n)
  v <- .hscs_base_window(x, ifelse(is.na(ph), NA, ph <= 5.5))
  .hscs_prop(v, missing = if (is.na(v)) "ph_h2o" else character(0),
             note = "read in the window of the base status (p. 32 gives no depth)")
}

# ---- gypsum, magnesium (p. 29-30) ---------------------------------------------

# Gipszes: a layer 15 cm or more thick with 5 % gypsum or more within 100 cm
# of the surface; erosen gipszes: 15 % or more. Gypsum is caso4_pct; with no
# measure, visible gypsum (gypsum_visible_pct) at the threshold shows it (a
# part of the gypsum), below it leaves it undecided.
.hscs_p_gipszes <- function(x) {
  miss <- "caso4_pct"
  if (is.na(x$surface)) return(.hscs_prop(NA, missing = miss))
  g <- x$h$caso4_pct %||% rep(NA_real_, x$n)
  vis <- x$h$gypsum_visible_pct %||% rep(NA_real_, x$n)
  form <- function(thr, variant, name) {
    ok <- ifelse(!is.na(g), g >= thr, ifelse(!is.na(vis) & vis >= thr, TRUE, NA))
    ok[x$rock] <- FALSE
    r <- .hscs_chem_run(x, ok, x$surface + 100, 15)
    c(r, list(variant = variant, layers = which(ok %in% TRUE), name = name))
  }
  .hscs_strongest_form(list(form(15, "eros", "er\u0151sen gipszes"), form(5, NULL, "gipszes")), miss)
}

# Magneziumos: exchangeable Ca / Mg below 1 in a layer 30 cm or more thick
# within 100 cm of the surface (ca_cmol, mg_cmol).
.hscs_p_magneziumos <- function(x) {
  miss <- c("ca_cmol", "mg_cmol")
  if (is.na(x$surface)) return(.hscs_prop(NA, missing = miss))
  ca <- x$h$ca_cmol %||% rep(NA_real_, x$n); mg <- x$h$mg_cmol %||% rep(NA_real_, x$n)
  ok <- ifelse(is.na(ca) | is.na(mg), NA, ifelse(mg > 0, ca / mg < 1, ifelse(ca > 0, FALSE, NA)))
  ok[x$rock] <- FALSE
  r <- .hscs_chem_run(x, ok, x$surface + 100, 30)
  .hscs_prop(r$passed, top = r$top, layers = which(ok %in% TRUE),
             missing = if (is.na(r$passed)) miss else character(0))
}

# ---- salts (p. 30, 32) ----------------------------------------------------------

# Szoloncsakos: a solonchak horizon (8.2, p. 26: salts 0.15 % or more, 15 cm
# or more; .hscs_solonchak_rows) within 100 cm of the surface. Gyengen
# szoloncsakos: a horizon 15 cm or more thick with 0.05 % or more. The plain
# property wins where its definition holds (the weak form is its
# complement); when it is undecided the weak form is not named. Erosen
# szoloncsakos (0.5 % or more with pH 8.5 or more) is defined "csak
# Szoloncsak talajokban" (p. 32), and the table of the Szoloncsak talajok
# (p. 13) does not list szoloncsakos: in the types that list it, the erosen
# form never applies.
.hscs_p_szoloncsakos <- function(x) {
  miss <- "soluble_salts_pct"
  if (is.na(x$surface)) return(.hscs_prop(NA, missing = miss))
  s <- x$h$soluble_salts_pct %||% rep(NA_real_, x$n)
  form <- function(ok, variant, name) {
    r <- .hscs_chem_run(x, ok, x$surface + 100, 15)
    c(r, list(variant = variant, layers = which(ok %in% TRUE), name = name))
  }
  plain <- .hscs_solonchak_rows(x)
  weak <- .and3v(ifelse(is.na(s), NA, s >= 0.05), x$mineral3)
  .hscs_strongest_form(list(form(plain, NULL, "szoloncs\u00e1kos"),
                            form(weak, "gyenge", "gyeng\u00e9n szoloncs\u00e1kos")), miss)
}

# Szolonyeces: a layer 15 cm or more thick within 100 cm of the surface with
# adsorbed Na 6 % or more and Na + Mg 15 % or more of the exchange complex;
# erosen: Na 6 % or more and Na + Mg 30 % or more; gyengen: Na 6 % or more.
# The shares are of the CEC (na_cmol / cec_cmol, or esp_pct when only it is
# given), as Hugo decided for the solonetz horizon (2026-10-29). A layer, not
# the solonetz horizon: no structure, any soil material but rock. Erosen
# wins over the plain property, the plain property over gyengen (each a
# part of the next); a stronger form undecided leaves the property so.
.hscs_p_szolonyeces <- function(x) {
  miss <- c("na_cmol", "mg_cmol", "cec_cmol", "esp_pct")
  if (is.na(x$surface)) return(.hscs_prop(NA, missing = miss))
  h <- x$h; n <- x$n
  na <- h$na_cmol; mg <- h$mg_cmol; cec <- h$cec_cmol
  esp <- ifelse(!is.na(na) & !is.na(cec) & cec > 0, 100 * na / cec, h$esp_pct %||% rep(NA_real_, n))
  namg <- ifelse(!is.na(na) & !is.na(mg) & !is.na(cec) & cec > 0, 100 * (na + mg) / cec, NA_real_)
  na6 <- ifelse(is.na(esp), NA, esp >= 6)
  form <- function(ok, variant, name) {
    ok[x$rock] <- FALSE
    r <- .hscs_chem_run(x, ok, x$surface + 100, 15)
    c(r, list(variant = variant, layers = which(ok %in% TRUE), name = name))
  }
  .hscs_strongest_form(list(
    form(.and3v(na6, ifelse(is.na(namg), NA, namg >= 30)), "eros", "er\u0151sen szolonyeces"),
    form(.and3v(na6, ifelse(is.na(namg), NA, namg >= 15)), NULL, "szolonyeces"),
    form(na6, "gyenge", "gyeng\u00e9n szolonyeces")), miss)
}

# Karbonat-szulfatos / karbonat-kloridos (p. 30): "A vizben oldhato sok
# legalabb 50%-a szulfat (30%-a klorid) a 20% karbonattartalom mellett". The
# share of the anion in the water-soluble salts (soluble_salts_sulfate_pct,
# soluble_salts_chloride_pct). The clause on carbonate is read as the text
# uses "karbonattartalom" elsewhere (8.2, cambic horizon), the CaCO3 of the
# soil: 20 % or more, in the same layer. No depth is given: any layer of the
# description.
.hscs_salt_anion <- function(x, share, thr, attr) {
  miss <- c(attr, "caco3_pct")
  ca <- x$h$caco3_pct %||% rep(NA_real_, x$n)
  ok <- .and3v(ifelse(is.na(share), NA, share >= thr), ifelse(is.na(ca), NA, ca >= 20))
  r <- x$ord[!x$rock[x$ord]]
  v <- if (!length(r)) NA else .any3(ok[r])
  .hscs_prop(v, layers = which(ok %in% TRUE), missing = if (is.na(v)) miss else character(0),
             note = "the carbonate clause read as CaCO3 of 20 % or more in the same layer")
}
.hscs_p_karbonat_szulfatos <- function(x)
  .hscs_salt_anion(x, x$h$soluble_salts_sulfate_pct %||% rep(NA_real_, x$n), 50, "soluble_salts_sulfate_pct")
.hscs_p_karbonat_kloridos <- function(x)
  .hscs_salt_anion(x, x$h$soluble_salts_chloride_pct %||% rep(NA_real_, x$n), 30, "soluble_salts_chloride_pct")

# ---- carbonate coatings, leached horizon (p. 31-32) ----------------------------

# Meszlepedekes (only Mezosegi talajok): secondary carbonates precipitated on
# the faces of the structural elements as a whitish, mould-like coating
# ("penesz-szeru bevonat", p. 31). soilKey records no form or place of
# the secondary carbonates; carbonate_soft_pct (soft, finely divided
# pedogenic carbonate) is the record closest to it and is read as it. A
# recorded 0, or secondary carbonates recorded only as hard ones, or none
# recorded (they are recorded when met), fails; secondary carbonates of
# unrecorded form (secondary_carbonates_pct, a k suffix) leave it undecided.
# A row recorded structureless (massive, single grain) has no ped faces. No
# depth is given: any row of the description.
.hscs_p_meszlepedekes <- function(x) {
  h <- x$h; n <- x$n
  soft <- h$carbonate_soft_pct %||% rep(NA_real_, n)
  sec <- h$secondary_carbonates_pct %||% rep(NA_real_, n)
  g <- .hscs_txt(h$structure_grade, n); t <- .hscs_txt(h$structure_type, n)
  apedal <- (!is.na(g) & grepl("structureless|apedal", g)) | (!is.na(t) & grepl("massive|single", t))
  ok <- ifelse(!is.na(soft), soft > 0,
        ifelse((!is.na(sec) & sec > 0) | x$des$k, NA, FALSE))
  ok[apedal | x$rock] <- FALSE
  r <- x$ord
  v <- if (!length(r)) NA else .any3(ok[r])
  top <- if (isTRUE(v)) h$top_cm[r[ok[r] %in% TRUE][1L]] else NA_real_
  .hscs_prop(v, top = top, layers = which(ok %in% TRUE),
             missing = if (is.na(v)) "carbonate_soft_pct" else character(0))
}

# Szologyos (only Szolonyec and Szoloncsak talajok): a strongly leached
# horizon at or near the surface (its top within 50 cm), dry Munsell value 7
# or 8 with chroma 3 or less, or value 5 or 6 with chroma 3 or less, pH(H2O)
# 5 or less, 5 cm or more thick (p. 32). The criteria listed are read; the
# dry colour only (the text names it).
.hscs_p_szologyos <- function(x) {
  miss <- c("munsell_value_dry", "munsell_chroma_dry", "ph_h2o")
  if (is.na(x$surface)) return(.hscs_prop(NA, missing = miss))
  h <- x$h
  dv <- h$munsell_value_dry; dc <- h$munsell_chroma_dry; ph <- h$ph_h2o
  col <- ifelse(!is.na(dv) & (dv < 5 | dv > 8) | !is.na(dc) & dc > 3, FALSE,
         ifelse(is.na(dv) | is.na(dc), NA, TRUE))
  ok <- .and3v(.and3v(col, ifelse(is.na(ph), NA, ph <= 5)), x$mineral3)
  r <- .hscs_chem_run(x, ok, x$surface + 50, 5)
  .hscs_prop(r$passed, top = r$top, layers = which(ok %in% TRUE),
             missing = if (is.na(r$passed)) miss else character(0))
}
