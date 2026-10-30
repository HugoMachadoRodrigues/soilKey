# =============================================================================
# v0.9.316 -- HSCS 2024 (Micheli et al. 2024, second approximation): the
# morphological, hydromorphic and rock subtype and variety properties of 8.5
# (p. 29-33) and 8.6 (p. 34-36), on the horizons, properties and materials of
# 8.2 (p. 25-26) and 8.3 (p. 27). Each is a .hscs_p_<id>(x) of the frame in
# R/hscs2024-properties.R, three-valued (TRUE / FALSE / NA).
#
# Readings where the text is silent (also in the comments of each function):
#   * colours: moist colours unless the text says dry; the hue is read on
#     the Munsell circle, "redder" toward R, one hue page (2.5 steps) as one
#     unit of hue; a neutral (N) or GLEY hue is the least red;
#   * mottles are recorded when met, as readings.yaml reads them: not
#     recorded is none; soilKey reads concretions, lamellae, a stagnic
#     pattern and cemented layers the same way;
#   * continuous hard rock is an R horizon (readings.yaml); the rock of the
#     rendzina, ranker and erubaz is read from the site's parent_material.
# =============================================================================

# ---- colours ------------------------------------------------------------------

# A hue as a number on the Munsell circle (R = 0, YR = 10, Y = 20 ...; red-
# purple below R), as .nzsc_hue_num() reads it; a neutral (N) or GLEY hue,
# which has no red in it, is Inf (the least red); NA when not readable.
.hscs_pm_hue <- function(hue) {
  hn <- .nzsc_hue_num(hue)
  hs <- toupper(gsub("[[:space:]]", "", as.character(hue)))
  hn[is.na(hn) & !is.na(hs) & grepl("^N|GLEY", hs)] <- Inf
  hn
}

# The same from the parts .munsell_notations() gives (number, family).
.hscs_pm_parts_hue <- function(number, family) {
  base <- c(R = 0, YR = 10, Y = 20, GY = 30, G = 40, BG = 50, B = 60, PB = 70, P = 80, RP = 90)
  fam <- as.character(family)
  b <- unname(base[ifelse(is.na(fam), "none", fam)])
  out <- ifelse(is.na(number) | is.na(b), NA_real_, b + number)
  out <- ifelse(!is.na(out) & fam %in% "RP", out - 100, out)
  out[fam %in% "N"] <- Inf
  out
}

# "A hue redder than 7.5YR, or 7.5YR with a moist chroma above 4" (the
# podzolic horizon, p. 25; voros, p. 35), per row.
.hscs_pm_red_75yr <- function(hn, chroma) {
  ifelse(is.na(hn), NA,
  ifelse(hn < 17.5 - 1e-6, TRUE,
  ifelse(abs(hn - 17.5) <= 1e-6, ifelse(is.na(chroma), NA, chroma > 4), FALSE)))
}

# The colours of a row's mottles (mottle_colors) and of a variegated matrix
# (variegated_colors): hue number, value, chroma. A neutral colour has
# chroma 0.
.hscs_pm_spot_colours <- function(x, i) {
  h <- x$h
  mc <- if (is.null(h$mottle_colors)) NA_character_ else as.character(h$mottle_colors[i])
  vc <- if (is.null(h$variegated_colors)) NA_character_ else as.character(h$variegated_colors[i])
  p <- rbind(.munsell_notations(mc), .munsell_notations(vc))
  ch <- p$chroma
  ch[is.na(ch) & p$family %in% "N"] <- 0
  list(hue = .hscs_pm_parts_hue(p$number, p$family), value = p$value, chroma = ch,
       variegated = !is.na(vc) && nzchar(trimws(vc)))
}

# ---- runs -----------------------------------------------------------------------

# A run of adjacent rows (depth order) where `ok` holds, starting above
# `top_max` and `min_th` cm or more thick, with `some` holding in at least
# one of its rows that begins above `top_max` (the part of the layer within
# the window): TRUE (and the top of the first such run) when rows that
# surely hold make one; FALSE when not even rows that may hold do; NA
# otherwise. A gap in the description, and the soil below a description
# that does not end on rock (`below_open`), may hold such rows, as in
# .hscs_run().
.hscs_pm_run_with <- function(x, ok, some, top_max = Inf, min_th = 0, below_open = TRUE) {
  h <- x$h; r <- x$ord
  s_top <- NA; s_th <- 0; s_some <- FALSE
  p_top <- NA; p_th <- 0; p_some <- FALSE
  prev <- NA; sure_top <- NA_real_; poss <- FALSE
  p_chk <- function(t, th, sm) !is.na(t) && t < top_max - 0.25 && th >= min_th - 1e-9 && sm
  for (i in r) {
    t <- h$top_cm[i]; th <- x$thick[i]
    if (!is.na(prev) && abs(t - prev) > 0.5) {
      s_top <- NA; s_th <- 0; s_some <- FALSE
      if (t > prev) {
        if (is.na(p_top)) p_top <- prev
        p_th <- Inf; p_some <- p_some || prev < top_max - 0.25
        if (p_chk(p_top, p_th, p_some)) poss <- TRUE
      } else { p_top <- NA; p_th <- 0; p_some <- FALSE }
    }
    if (ok[i] %in% TRUE) {
      if (is.na(s_top)) s_top <- t
      s_th <- s_th + (if (is.na(th)) 0 else th)
      s_some <- s_some || (some[i] %in% TRUE && t < top_max - 0.25)
      if (is.na(sure_top) && p_chk(s_top, s_th, s_some)) sure_top <- s_top
    } else { s_top <- NA; s_th <- 0; s_some <- FALSE }
    if (!(ok[i] %in% FALSE)) {
      if (is.na(p_top)) p_top <- t
      p_th <- p_th + (if (is.na(th)) Inf else th)
      p_some <- p_some || (!(some[i] %in% FALSE) && t < top_max - 0.25)
      if (p_chk(p_top, p_th, p_some)) poss <- TRUE
    } else { p_top <- NA; p_th <- 0; p_some <- FALSE }
    prev <- h$bottom_cm[i]
  }
  if (below_open && !x$ends_on_rock && !is.na(prev)) {
    if (is.na(p_top)) p_top <- prev
    p_th <- Inf; p_some <- p_some || prev < top_max - 0.25
    if (p_chk(p_top, p_th, p_some)) poss <- TRUE
  }
  list(passed = if (!is.na(sure_top)) TRUE else if (poss) NA else FALSE, top = sure_top)
}

# A layer property "a run of `ok` rows `min_th` cm or more thick with its top
# within `zmax` cm of the soil surface", with its top for the 8.7 modifier.
.hscs_pm_layer <- function(x, ok, zmax, min_th, missing, below_open = TRUE) {
  if (is.na(x$surface)) return(.hscs_prop(NA, missing = c("top_cm", missing)))
  v <- .hscs_run(x, ok, top_max = x$surface + zmax, min_th = min_th, below_open = below_open)
  top <- if (isTRUE(v)) .hscs_run_top(x, ok, top_max = x$surface + zmax, min_th = min_th) else NA_real_
  .hscs_prop(v, top = top, layers = which(ok %in% TRUE), missing = if (is.na(v)) missing else character(0))
}

# ---- 8.2 subsurface horizons -------------------------------------------------------

# agyagbemosodasos (p. 29): a clay accumulation horizon within 100 cm of the
# soil surface (8.2; the frame's .hscs_clay_accum(), lamellae included). The
# modifier reads the top of its run, or of its uppermost lamella.
.hscs_p_agyagbemosodasos <- function(x) {
  if (is.na(x$surface)) return(.hscs_prop(NA, missing = "top_cm"))
  ok <- .hscs_clay_accum_rows(x)
  v <- .hscs_clay_accum(x, 100)
  top <- NA_real_
  if (isTRUE(v)) {
    top <- .hscs_run_top(x, ok, top_max = x$surface + 100, min_th = 10)
    lam <- grepl("lamell", tolower(as.character(x$h$designation))) & ok %in% TRUE &
           (x$h$top_cm < x$surface + 100) %in% TRUE
    if (any(lam)) top <- min(c(top, x$h$top_cm[lam]), na.rm = TRUE)
  }
  .hscs_prop(v, top = top, layers = which(ok %in% TRUE),
             missing = if (is.na(v)) c("clay_pct", "clay_films_amount", "designation") else character(0))
}

# Rows that are lamellae of clay accumulation (8.2 criterion 3, "kovarvany-
# csikok", each 1 cm or more thick). soilKey records lamellae in two ways: a
# designation naming them (as the frame's clay accumulation horizon reads
# them; the thickness of each lamella is not recorded), or a thin row of its
# own, as the WRB Lamellic qualifier reads it: 1 cm (the HSCS minimum) to
# less than 7.5 cm (the WRB lamella's upper limit, soilKey's reading of a
# band), with more clay than the row directly below it. Either must be a row
# of the clay accumulation horizon (.hscs_clay_accum_rows: 1.4 times the clay
# of the horizon above, or clay films). Lamellae are conspicuous: a thin row
# with no clay measured is not read as one.
.hscs_pm_lamella_rows <- function(x) {
  h <- x$h; n <- x$n
  ca <- .hscs_clay_accum_rows(x)
  named <- grepl("lamell", tolower(as.character(h$designation %||% rep(NA, n))))
  bel <- .hscs_below(x)
  cl <- h$clay_pct
  clb <- ifelse(is.na(bel), NA_real_, cl[ifelse(is.na(bel), 1L, bel)])
  band <- !named & !is.na(x$thick) & x$thick >= 1 & x$thick < 7.5 & !is.na(cl) & !is.na(clb) & cl > clb
  ifelse(named, ifelse(ca %in% FALSE, FALSE, ifelse(ca %in% TRUE, TRUE, NA)), band & ca %in% TRUE)
}

# kovarvanyos (p. 30): within 100 cm of the soil surface a horizon in which
# the clay accumulation shows as lamellae. The definition of 8.5 asks for
# lamellae, not for the 15 cm in all of 8.2 criterion 3 (`min_total`, 0):
# with 15 cm a soil within 100 cm would key out as Agyagbemosodasos talaj,
# and the Homoktalajok, whose table lists kovarvanyos, could never have it.
.hscs_p_kovarvanyos <- function(x, min_total = 0) {
  if (is.na(x$surface)) return(.hscs_prop(NA, missing = "top_cm"))
  lam <- .hscs_pm_lamella_rows(x)
  inw <- !is.na(x$h$top_cm) & x$h$top_cm < x$surface + 100 - 0.25
  sure <- which(lam %in% TRUE & inw)
  may <- which(!(lam %in% FALSE) & inw)
  tot <- function(i) sum(x$thick[i], na.rm = TRUE)
  v <- if (length(sure) && tot(sure) >= min_total) TRUE
       else if (length(may) && (min_total <= 0 || anyNA(x$thick[may]) || tot(may) >= min_total)) NA
       else FALSE
  .hscs_prop(v, top = if (isTRUE(v)) min(x$h$top_cm[sure]) else NA_real_, layers = sure,
             missing = if (is.na(v)) c("clay_pct", "clay_films_amount") else character(0))
}

# podzolos (p. 31; the podzolic horizon of 8.2, p. 25): within 100 cm of the
# soil surface a subsurface horizon 10 cm or more thick with pH (H2O) below
# 6, organic carbon 0.5 % or more, and a red colour, "moist, rubbed": a hue
# redder than 7.5YR, or 7.5YR with a moist chroma above 4. soilKey records
# no rubbed colour: the moist matrix colour stands for it.
.hscs_p_podzolos <- function(x) {
  h <- x$h
  ph <- h$ph_h2o; oc <- h$oc_pct
  red <- .hscs_pm_red_75yr(.hscs_pm_hue(h$munsell_hue_moist), h$munsell_chroma_moist)
  ok <- .and3v(.and3v(.and3v(.and3v(ifelse(is.na(ph), NA, ph < 6), ifelse(is.na(oc), NA, oc >= 0.5)),
                             red), x$mineral3), x$sub3)
  .hscs_pm_layer(x, ok, 100, 10, c("ph_h2o", "oc_pct", "munsell_hue_moist", "munsell_chroma_moist"))
}

# Rows of an iron accumulation horizon (vaskivalasos szint, 8.2, p. 26), a
# subsurface horizon: 1. 15 % or more of the matrix in large (over 20 mm)
# Fe or Fe-Mn mottles with a moist hue redder than 7.5YR and a chroma above
# 5; or 2. 5 % or more by volume of discrete Fe or Fe-Mn concretions of 2 mm
# or more, at least weakly cemented (ferric_nodules_pct). Mottles
# (redoximorphic_features_pct, mottle_colors) and concretions are recorded
# when met: not recorded reads as none (but a mottle colour recorded with no
# abundance leaves the abundance unknown). Large mottles: mottle_size_class
# coarse (FAO 2006: over 20 mm; Hugo's decision, 2026-10-30); another class
# recorded is not large; no size recorded leaves route 1 undecided. With
# several mottle colours of which only some are red, the share of the red
# ones is not known: undecided.
.hscs_pm_iron_rows <- function(x) {
  h <- x$h; n <- x$n
  mot <- h$redoximorphic_features_pct %||% rep(NA_real_, n)
  sz <- .hscs_txt(h$mottle_size_class %||% rep(NA_character_, n), n)
  big <- ifelse(is.na(sz), NA, grepl("coarse", sz) & !grepl("fine|medium", sz))
  c1 <- rep(FALSE, n)
  for (i in seq_len(n)) {
    sp <- .hscs_pm_spot_colours(x, i)
    if (!is.na(mot[i]) && mot[i] < 15) next
    if (is.na(mot[i]) && !length(sp$hue)) next
    abund <- if (is.na(mot[i])) NA else TRUE
    q <- ifelse(is.na(sp$hue) | is.na(sp$chroma), NA, sp$hue < 17.5 - 1e-6 & sp$chroma > 5)
    red <- if (!length(q)) NA else if (all(q %in% FALSE)) FALSE else if (all(q %in% TRUE)) TRUE else NA
    c1[i] <- .and3(c(abund, red, big[i]))
  }
  nod <- h$ferric_nodules_pct %||% rep(NA_real_, n)
  c2 <- !is.na(nod) & nod >= 5
  .and3v(.and3v(.any3v(c1, c2), x$mineral3), x$sub3)
}

# vaskivalasos (p. 33): an iron accumulation horizon, 15 cm or more thick,
# within 100 cm of the soil surface.
.hscs_p_vaskivalasos <- function(x) {
  .hscs_pm_layer(x, .hscs_pm_iron_rows(x), 100, 15,
                 c("ferric_nodules_pct", "redoximorphic_features_pct", "mottle_colors", "mottle_size_class"))
}

# duzzado (variety, p. 34): a swelling horizon (8.2: 25 cm or more thick) within
# 100 cm of the soil surface; the frame's .hscs_swelling_rows().
.hscs_p_duzzado <- function(x) {
  .hscs_pm_layer(x, .hscs_swelling_rows(x), 100, 25, c("clay_pct", "slickensides", "structure_type"))
}

# ---- 8.3 hydromorphic properties -------------------------------------------------

# glejes (p. 29): for part of the year, within 100 cm of the mineral soil
# surface, a layer 25 cm or more thick with reductive conditions at times and
# the groundwater gley colour pattern "in the whole layer". Read as a run of
# rows, each with the pattern (.hscs_gw_gley_rows), with reductive
# conditions (.hscs_reductive_rows, Hugo's decision for the Reti talajok) in
# at least one of them; methane at the site counts, as for the Reti talajok.
.hscs_p_glejes <- function(x) {
  if (is.na(x$ms)) return(.hscs_prop(NA, missing = "top_cm"))
  gley <- .and3v(.hscs_gw_gley_rows(x), x$mineral3)
  red <- .hscs_reductive_rows(x)
  if (isTRUE(.hscs_truthy(.hscs_site1(x, "methane_odour")))) red[!x$rock] <- TRUE
  r <- .hscs_pm_run_with(x, gley, red, top_max = x$ms + 100, min_th = 25)
  .hscs_prop(r$passed, top = r$top, layers = which(gley %in% TRUE),
             missing = if (is.na(r$passed)) c("childs_test", "designation", "munsell_hue_moist",
                                              "munsell_chroma_moist", "redoximorphic_features_pct")
                       else character(0))
}

# The pair test of the stagnic colour pattern (pangovizglejes szinmintazat,
# 8.3, p. 27): some parts lighter (value 1 or more higher) and paler (chroma
# 1 or more lower), other parts redder (hue one page or more redder) and
# brighter (chroma 1 or more higher), "relative to each other, or to the
# parts that are not reductimorphic" (read as the matrix colour `m`, an
# index into the colours, NA when not known). TRUE / FALSE / NA.
.hscs_pm_stagnic_pair <- function(hue, value, chroma, m = NA_integer_) {
  k <- length(hue)
  ge <- function(a, b, d) { z <- a - b; if (is.na(z)) { if (is.nan(z)) FALSE else NA } else z >= d - 1e-9 }
  v <- logical(0)
  for (p in seq_len(k)) for (q in seq_len(k)) {
    if (p == q) next
    # p the pale part, q the red one, against each other
    v <- c(v, .and3(c(ge(value[p], value[q], 1), ge(chroma[q], chroma[p], 1), ge(hue[p], hue[q], 2.5))))
    # against the matrix
    if (!is.na(m) && p != m && q != m)
      v <- c(v, .and3(c(ge(value[p], value[m], 1), ge(chroma[m], chroma[p], 1),
                        ge(hue[m], hue[q], 2.5), ge(chroma[q], chroma[m], 1))))
  }
  if (!length(v)) return(NA)
  .any3(v)
}

# Rows with the stagnic colour pattern. Read from: a g suffix (FAO 2006:
# the mottling of alternating reduction and oxidation by surface water);
# else the colours of the mottles (mottle_colors) or of a variegated matrix
# (variegated_colors) with the moist matrix colour, by the pair test above.
# Mottles are recorded when met: no mottles recorded (or 0 %) is no pattern;
# mottles recorded with no colours leave it undecided; a single mottle
# colour with no matrix colour too.
.hscs_pm_stagnic_rows <- function(x) {
  h <- x$h; n <- x$n
  mot <- h$redoximorphic_features_pct %||% rep(NA_real_, n)
  mh <- .hscs_pm_hue(h$munsell_hue_moist)
  mv <- h$munsell_value_moist; mc <- h$munsell_chroma_moist
  out <- rep(FALSE, n)
  for (i in seq_len(n)) {
    if (x$rock[i]) next
    if (x$des$g[i]) { out[i] <- TRUE; next }
    sp <- .hscs_pm_spot_colours(x, i)
    if (!length(sp$hue)) {
      out[i] <- if (!is.na(mot[i]) && mot[i] > 0) NA else FALSE
      next
    }
    known_m <- !sp$variegated && !is.na(mh[i]) && !is.na(mv[i]) && !is.na(mc[i])
    if (known_m) {
      out[i] <- .hscs_pm_stagnic_pair(c(mh[i], sp$hue), c(mv[i], sp$value), c(mc[i], sp$chroma), m = 1L)
    } else {
      pr <- .hscs_pm_stagnic_pair(sp$hue, sp$value, sp$chroma)
      # the matrix colour, not recorded, could make the pair
      out[i] <- if (isTRUE(pr)) TRUE else if (sp$variegated && isFALSE(pr)) FALSE else NA
    }
  }
  .and3v(out, x$mineral3)
}

# pangovizes (p. 31): for part of the year, within 100 cm of the mineral soil
# surface, reductive conditions; and the stagnic colour pattern in 25 % or
# more of the soil volume. The soil volume is read as that of the same
# window, from the mineral soil surface to 100 cm or to shallower hard rock,
# by the thickness of the rows with the pattern; the pattern is recorded
# when met, so a part of the window the description does not cover has none.
# Reductive conditions as for the Reti talajok (Hugo's decision of
# 2026-10-29): a positive dipyridyl test, iron sulfides, an l or r suffix, or
# methane; and, for pangovizes only, a g suffix (FAO 2006 stagnic conditions;
# Hugo's decision, 2026-10-30). Negative tests over the whole window are
# none; otherwise undecided. The modifier reads the top of the uppermost row
# with the pattern.
.hscs_p_pangovizes <- function(x) {
  if (is.na(x$ms)) return(.hscs_prop(NA, missing = "top_cm"))
  h <- x$h
  rt <- .hscs_rock_top(x)
  zb <- min(c(x$ms + 100, rt), na.rm = TRUE)
  if (zb <= x$ms + 0.25) return(.hscs_prop(FALSE, note = "no soil above the rock"))
  stag <- .hscs_pm_stagnic_rows(x)
  r <- .hscs_rows_in(x, x$ms, zb)
  ov <- .asc_overlap(h$top_cm[r], ifelse(is.na(h$bottom_cm[r]), zb, h$bottom_cm[r]), x$ms, zb)
  span <- zb - x$ms
  lo <- sum(ov[stag[r] %in% TRUE]) / span; hi <- sum(ov[!(stag[r] %in% FALSE)]) / span
  share <- if (lo >= 0.25 - 1e-9) TRUE else if (hi < 0.25 - 1e-9) FALSE else NA
  red <- .hscs_reductive_rows(x)
  red[x$des$g & !x$rock] <- TRUE
  rr <- r[!x$rock[r]]
  meth <- .hscs_truthy(.hscs_site1(x, "methane_odour"))
  c1 <- if (isTRUE(meth) || any(red[rr] %in% TRUE)) TRUE
        else if (isTRUE(.hscs_all_in(x, ifelse(red %in% FALSE, TRUE, NA), x$ms, zb))) FALSE else NA
  v <- .and3(c(c1, share))
  st <- r[stag[r] %in% TRUE]
  .hscs_prop(v, top = if (isTRUE(v)) min(h$top_cm[st]) else NA_real_, layers = st,
             missing = if (is.na(v)) c("childs_test", "mottle_colors", "redoximorphic_features_pct",
                                       "munsell_hue_moist", "designation") else character(0),
             note = sprintf("stagnic pattern in %.0f-%.0f %% of the soil volume", 100 * lo, 100 * hi))
}

# ---- colours ------------------------------------------------------------------

# Eluvial rows (Hugo's decision, 2026-10-30, for fako): an E horizon (an E
# master, or a transition led by E: EB, E/B), or the subsurface horizon
# directly above a clay accumulation horizon (a run of its rows 10 cm or
# more thick from the row below) or a solonetz horizon, with less clay than
# that horizon's top row (clay measured, else the range of the texture
# class). A row with no designation may be an E.
.hscs_pm_eluvial_rows <- function(x) {
  h <- x$h; n <- x$n
  dsg <- as.character(h$designation %||% rep(NA_character_, n))
  e_role <- x$role %in% c("E", "EB", "E/B") | (x$role %in% "other" & grepl("^[0-9]*E", dsg))
  ca <- .hscs_clay_accum_rows(x)
  sn <- .hscs_solonetz_rows(x)
  bel <- .hscs_below(x)
  cr <- .hscs_clay_range(x)
  # the clay accumulation rows from row i down: a sure run 10 cm or more
  # thick, or one that may be
  ca_from <- function(i) {
    if (ca[i] %in% FALSE) return(FALSE)
    s_th <- 0; p_th <- 0; sure <- TRUE; k <- i
    while (!is.na(k) && !(ca[k] %in% FALSE)) {
      th <- x$thick[k]
      if (sure && ca[k] %in% TRUE) s_th <- s_th + (if (is.na(th)) 0 else th) else sure <- FALSE
      p_th <- p_th + (if (is.na(th)) Inf else th)
      k <- bel[k]
    }
    if (is.na(k) && !x$ends_on_rock) p_th <- Inf
    if (s_th >= 10 - 1e-9) TRUE else if (p_th >= 10 - 1e-9) NA else FALSE
  }
  above <- rep(FALSE, n)
  for (j in seq_len(n)) {
    i <- bel[j]
    if (is.na(i)) next
    hz <- .any3(c(if (ca[j] %in% TRUE) FALSE else ca_from(i), if (sn[j] %in% TRUE) FALSE else sn[i]))
    less <- if (!is.na(cr$hi[j]) && !is.na(cr$lo[i]) && cr$hi[j] < cr$lo[i]) TRUE
            else if (!is.na(cr$lo[j]) && !is.na(cr$hi[i]) && cr$lo[j] >= cr$hi[i]) FALSE else NA
    above[j] <- .and3(c(hz, less))
  }
  out <- ifelse(e_role, TRUE, ifelse(is.na(x$role), .any3v(NA, above), above))
  .and3v(.and3v(out, x$mineral3), x$sub3)
}

# fako (p. 29): a subsurface horizon 5 cm or more thick with a dry colour of
# value 7 or 8 and chroma 3 or less, or value 5 or 6 and chroma 3 or less
# (dry value 5 to 8, chroma 3 or less); the horizon an eluvial one (Hugo's
# decision, 2026-10-30; .hscs_pm_eluvial_rows). The text gives no depth: the
# horizons the description records are read, not the soil below it.
.hscs_p_fako <- function(x) {
  h <- x$h
  dv <- h$munsell_value_dry; dc <- h$munsell_chroma_dry
  col <- ifelse(is.na(dv) | is.na(dc), ifelse(!is.na(dc) & dc > 3 | !is.na(dv) & (dv < 5 | dv > 8), FALSE, NA),
                dv >= 5 & dv <= 8 & dc <= 3)
  ok <- .and3v(col, .hscs_pm_eluvial_rows(x))
  .hscs_pm_layer(x, ok, Inf, 5, c("munsell_value_dry", "munsell_chroma_dry", "designation", "clay_pct"),
                 below_open = FALSE)
}

# rozsdabarna (p. 31): within 100 cm of the soil surface a layer 30 cm or more
# thick with a moist hue of 10YR or redder "and/or" a moist chroma of 5 or
# more: read as both (Hugo's decision, 2026-10-30; a rust-brown colour).
# Mineral soil material.
.hscs_p_rozsdabarna <- function(x) {
  h <- x$h
  hn <- .hscs_pm_hue(h$munsell_hue_moist); ch <- h$munsell_chroma_moist
  hue_ok <- ifelse(is.na(hn), NA, hn <= 20 + 1e-6)
  ch_ok <- ifelse(is.na(ch), NA, ch >= 5)
  ok <- .and3v(.and3v(hue_ok, ch_ok), x$mineral3)
  .hscs_pm_layer(x, ok, 100, 30, c("munsell_hue_moist", "munsell_chroma_moist"))
}

# voros (variety, p. 35): within 150 cm of the soil surface a layer 15 cm or
# more thick with a hue redder than 7.5YR, or 7.5YR with a moist chroma above
# 4. The text does not say moist for the hue: the moist hue is read, the dry
# one when no moist hue is recorded. Mineral soil material.
.hscs_p_voros <- function(x) {
  h <- x$h
  hn <- .hscs_pm_hue(h$munsell_hue_moist)
  hd <- .hscs_pm_hue(h$munsell_hue_dry %||% rep(NA_character_, x$n))
  hn <- ifelse(is.na(hn), hd, hn)
  ok <- .and3v(.hscs_pm_red_75yr(hn, h$munsell_chroma_moist), x$mineral3)
  .hscs_pm_layer(x, ok, 150, 15, c("munsell_hue_moist", "munsell_chroma_moist"))
}

# ---- surface ------------------------------------------------------------------

# kerges (p. 30; Szolonyec talajok only): the upper boundary of the columnar
# horizon within 10 cm of the soil surface: a row with a columnar structure
# whose top lies within 10 cm. No when every row that begins within 10 cm
# has a recorded structure that is not columnar.
.hscs_p_kerges <- function(x) {
  if (is.na(x$surface)) return(.hscs_prop(NA, missing = "top_cm"))
  h <- x$h
  t <- .hscs_txt(h$structure_type, x$n)
  col <- ifelse(is.na(t), NA, grepl("column", t))
  r <- x$ord[h$top_cm[x$ord] < x$surface + 10 - 0.25 & !x$rock[x$ord]]
  v <- if (any(col[r] %in% TRUE)) TRUE else if (length(r) && all(col[r] %in% FALSE)) FALSE else NA
  .hscs_prop(v, layers = r[col[r] %in% TRUE], missing = if (is.na(v)) "structure_type" else character(0))
}

# futo (p. 29; Homoktalajok only): a raw surface, bare of permanent
# vegetation, structureless single grains, with marks of wind deflation. The
# site's wind_deflation records all of it; without it, surface_condition
# naming deflation or drifting sand, unless the surface horizon is organic
# (not bare) or has a recorded structure of peds (not single grain). No when
# wind_deflation is FALSE, or the surface horizon is organic or pedal.
.hscs_p_futo <- function(x) {
  wd <- .hscs_truthy(.hscs_site1(x, "wind_deflation"))
  if (!is.na(wd)) return(.hscs_prop(wd))
  sc <- tolower(paste(as.character(unlist(x$site$surface_condition %||% "")), collapse = " "))
  sc <- chartr("\u00e1\u00e9\u00ed\u00f3\u00f6\u0151\u00fa\u00fc\u0171", "aeiooouuu", sc)
  says <- grepl("deflat|wind[- ]?(erosion|blown|drift|scour|rework)|(drifting|blowing|shifting|mobile|moving) sand|futohomok|szelerozi|szelfuvas", sc)
  i <- if (length(x$ord)) x$ord[1L] else NA_integer_
  if (is.na(i)) return(.hscs_prop(NA, missing = "wind_deflation"))
  g <- .hscs_txt(x$h$structure_grade, x$n)[i]; t <- .hscs_txt(x$h$structure_type, x$n)[i]
  pedal <- !is.na(g) && grepl("weak|moderate|strong", g) && !(!is.na(t) && grepl("single|massive", t))
  if (isTRUE(x$organic[i]) || pedal) return(.hscs_prop(FALSE))
  .hscs_prop(if (says) TRUE else NA, missing = if (says) character(0) else "wind_deflation")
}

# ---- rock and cemented layers -------------------------------------------------------

# The kind of rock the site's parent_material names (English or Hungarian;
# accents optional): "carbonate" (rendzina: limestone, dolomite, compact marl
# ..., p. 31), "acid" (ranker: acid igneous rocks, sandstones ..., p. 31),
# "basic" (erubaz: basic and neutral igneous rocks and tuffs, p. 29); also
# "other" (a hard rock of none of these) and "loose" (an unconsolidated
# material). Where the text says "X over Y", the soil's material is X. A
# logical vector by kind, or NULL when no parent material is recorded.
.hscs_pm_rock_kinds <- function(x) {
  pm <- x$site$parent_material
  pm <- paste(as.character(unlist(pm %||% character(0))), collapse = " ")
  pm <- tolower(trimws(pm))
  if (!nzchar(pm) || pm %in% c("na", "nan")) return(NULL)
  s <- chartr("\u00e1\u00e9\u00ed\u00f3\u00f6\u0151\u00fa\u00fc\u0171", "aeiooouuu", pm)
  s <- sub("\\s+(over|overlying|on top of)\\s.*$", "", s, perl = TRUE)
  # a tuff is a tuff whatever its lava (rhyolite tuff, andezittufa);
  # calcareous tufa (mesztufa) is a carbonate rock
  s <- gsub("(mesz|calcareous |lime )tufa", " travertine ", s, perl = TRUE)
  s <- gsub("[a-z]*(rhyolit|riolit|dacit|andesit|andezit|basalt|bazalt|trachyt|trachit)[a-z]*[ -]?(tuff|tufa)[a-z]*",
            " tuff ", s, perl = TRUE)
  s <- gsub("granodiorit", " granite ", s, fixed = TRUE)
  s <- gsub("(mica[ -]?schist|micaschist)", " gneiss ", s, perl = TRUE)
  has <- function(p) grepl(p, s, perl = TRUE)
  carbonate <- has(paste0("limestone|dolomit|dolostone|\\bmarl|chalk|marble|travertin|",
                          "carbonate rock|calcareous rock|meszko|\\bmarga|margas|marvany|karbonatos kozet"))
  acid <- has(paste0("granit|rhyolit|riolit|dacit|quartz[ -]?porphyr|kvarcporfir|pegmatit|aplit|sandstone|",
                     "homokko|quartzit|kvarcit|gneis|csillampala|phyllit|fillit|acid(ic)? (igneous|volcanic|plutonic)|",
                     "felsic|savanyu (magmas|kiomlesi|melysegi|vulkani)"))
  basic <- has(paste0("basalt|bazalt|andesit|andezit|diorit|gabbr|dolerit|diabas|diabaz|trachyt|trachit|",
                      "syenit|szienit|phonolit|fonolit|basanit|bazanit|picrit|pikrit|\\btuff|tufa|ignimbrit|",
                      "(basic|intermediate|neutral|mafic) (igneous|volcanic|plutonic)|",
                      "(bazikus|semleges) (magmas|kiomlesi|melysegi|vulkani)"))
  other <- has(paste0("shale|claystone|mudstone|siltstone|slate|agyagko|agyagpala|aleurolit|conglomerat|",
                      "konglomerat|breccia|schist|\\bpala\\b|serpentinit|amphibolit|amfibolit"))
  loose <- has(paste0("loess|\\bloss\\b|\\blosz|\\bsands?\\b|homok(?!ko)|alluvi|fluvi|sediment|deposit|colluvi|",
                      "slope ?wash|\\btill\\b|glacial|eolian|aeolian|\\bclay(?!stone)|\\bsilt\\b|gravel|kavics|debris|",
                      "rubble|scree|tormelek|hordalek|ontes|agyag(?!ko|pala)|iszap|peat|tozeg|organic|",
                      "lacustrine|\\btavi\\b|volcanic ash|tephra|vulkani hamu|unconsolidated|\\blaza\\b|\\bfill\\b"))
  c(carbonate = carbonate, acid = acid, basic = basic, other = other, loose = loose)
}

# rendzina, ranker, erubaz (p. 29, 31): a soil formed on continuous hard
# rock of the kind (an R horizon; readings.yaml) that the parent material
# names. The rock kind: yes when it is named alone; undecided when another
# hard kind or a loose material is named too (which did the soil form on?)
# or the text names no rock soilKey knows; no when only other kinds are
# named. On rock: an R horizon at any depth; with none, a description that
# does not end on rock leaves it undecided (the rock may lie below). The
# smectitic weathering products of the erubaz are read as the text's
# description of those rocks, not a further test.
.hscs_pm_on_rock <- function(x, kind) {
  k <- .hscs_pm_rock_kinds(x)
  if (is.null(k)) return(.hscs_prop(NA, missing = "parent_material"))
  hard <- k[c("carbonate", "acid", "basic")]
  rk <- if (k[[kind]]) { if (sum(hard) > 1 || k[["loose"]]) NA else TRUE }
        else if (any(hard) || k[["other"]] || k[["loose"]]) FALSE else NA
  rt <- .hscs_rock_top(x)
  on <- if (!is.na(rt)) TRUE else if (x$ends_on_rock) TRUE else NA
  v <- .and3(c(rk, on))
  .hscs_prop(v, layers = if (isTRUE(v)) which(x$rock) else integer(0),
             missing = if (is.na(v)) c(if (is.na(rk)) "parent_material", if (is.na(on)) "designation")
                       else character(0))
}
.hscs_p_rendzina <- function(x) .hscs_pm_on_rock(x, "carbonate")
.hscs_p_ranker <- function(x) .hscs_pm_on_rock(x, "acid")
.hscs_p_erubaz <- function(x) .hscs_pm_on_rock(x, "basic")

# kozethataros (variety, p. 34): within 100 cm of the surface continuous hard
# rock (an R horizon), or debris with less than 20 % fine earth by volume (a
# row with more than 80 % coarse fragments; coarse fragments not recorded
# read as none, as for the Kozethatasu talajok). No when the description
# reaches 100 cm (or rock) without either; a description that ends above
# 100 cm (not on rock) leaves it undecided. The modifier reads the top of the
# rock or of the debris, whichever is higher.
.hscs_p_kozethataros <- function(x) {
  if (is.na(x$surface)) return(.hscs_prop(NA, missing = "top_cm"))
  h <- x$h; zb <- x$surface + 100
  rt <- .hscs_rock_top(x)
  cf <- h$coarse_fragments_pct
  deb <- !x$rock & !is.na(cf) & cf > 80 & !is.na(h$top_cm) & h$top_cm < zb - 0.25
  tops <- c(if (!is.na(rt) && rt < zb - 0.25) rt, h$top_cm[deb])
  if (length(tops)) return(.hscs_prop(TRUE, top = min(tops), layers = which(deb | x$rock)))
  deepest <- suppressWarnings(max(h$bottom_cm, na.rm = TRUE))
  v <- if (x$ends_on_rock || (!anyNA(h$bottom_cm) && is.finite(deepest) && deepest >= zb - 0.25)) FALSE else NA
  .hscs_prop(v, missing = if (is.na(v)) c("designation", "coarse_fragments_pct", "bottom_cm") else character(0))
}

# Rows of a cemented, hard, root-restricting layer (cementalt, 8.6, p. 34):
# an m suffix, a cementation class moderately cemented or stronger, or a
# pan_type of a cemented pan; not rock (8.3: cemented soil horizons are not
# rock). A pan of no named kind ("other pan") leaves the row undecided; a
# fragipan or a densipan is compact, not cemented. Cemented layers are
# recorded when met, as the frame reads them (.hscs_cemented_top).
.hscs_pm_cemented_rows <- function(x) {
  h <- x$h; n <- x$n
  cem <- .hscs_txt(h$cementation_class, n)
  pt <- .hscs_txt(h$pan_type %||% rep(NA_character_, n), n)
  pan <- !is.na(pt) & grepl("calcrete|ferricrete|silcrete|alcrete|duripan|hardpan|ironpan|ortstein|organic pan", pt)
  other <- !is.na(pt) & grepl("other", pt)
  ok <- x$des$m | cem %in% c("moderately", "strongly", "indurated") | pan
  out <- ifelse(ok, TRUE, ifelse(other & !(cem %in% c("none", "weakly")), NA, FALSE))
  out[x$rock] <- FALSE
  out
}

# The cementing agent of a cemented row, as the words of p. 34 name it
# ("cementalt karbonatos / kavicsos / vaskivalasos / gipszes / kovasavas"):
# from the FAO (2006) suffixes with m (km carbonates, sm iron, vm
# petroplinthite, ym gypsum, qm silica), else the pan_type (calcrete;
# ferricrete or ironpan; silcrete, duripan or a red-brown hardpan), else the
# composition by the limits of the WRB petric horizons (CaCO3 15 % or more,
# gypsum 5 % or more, duripan 10 % or more, plinthite 15 % or more);
# kavicsos is cemented gravel, 40 % or more rounded coarse fragments (the
# limit of the kavicsos variety). With several, the first in the text's order.
.hscs_pm_agent <- function(x, i) {
  h <- x$h
  sfx <- c(x$des$strong[[i]], x$des$weak[[i]])
  pt <- tolower(as.character(h$pan_type[i] %||% NA))
  num <- function(nm) { v <- h[[nm]]; if (is.null(v)) NA_real_ else suppressWarnings(as.numeric(v[i])) }
  ge <- function(nm, thr) { v <- num(nm); !is.na(v) && v >= thr }
  shape <- tolower(as.character(h$coarse_fragments_shape[i] %||% NA))
  gravel <- (ge("gravel_pct", 40) || ge("coarse_fragments_pct", 40)) && !is.na(shape) && shape %in% c("rounded", "mixed")
  by_sfx <- c(karbonatos = "k" %in% sfx, kavicsos = FALSE, vaskivalasos = any(c("s", "v") %in% sfx),
              gipszes = "y" %in% sfx, kovasavas = "q" %in% sfx)
  by_pan <- c(karbonatos = grepl("calcrete", pt), kavicsos = FALSE,
              vaskivalasos = grepl("ferricrete|ironpan", pt), gipszes = grepl("gypcrete|gypsum", pt),
              kovasavas = grepl("silcrete|duripan|red-brown hardpan", pt))
  by_chem <- c(karbonatos = ge("caco3_pct", 15), kavicsos = FALSE, vaskivalasos = ge("plinthite_pct", 15),
               gipszes = ge("caso4_pct", 5), kovasavas = ge("duripan_pct", 10))
  a <- if (any(by_sfx)) by_sfx else if (any(by_pan)) by_pan else by_chem
  a[["kavicsos"]] <- gravel
  if (!any(a)) return(NULL)
  words <- c(karbonatos = "karbon\u00e1tos", kavicsos = "kavicsos", vaskivalasos = "vaskiv\u00e1l\u00e1sos",
             gipszes = "gipszes", kovasavas = "kovasavas")
  paste("cement\u00e1lt", words[[names(a)[a][1L]]])
}

# cementalt (variety, p. 34): within 100 cm of the soil surface a surface or
# subsurface horizon forming a cemented, hard, root-restricting layer; the
# label names the agent of its uppermost row when it is recorded. A
# description that ends above 100 cm (not on rock) leaves it undecided, as
# for rock.
.hscs_p_cementalt <- function(x) {
  if (is.na(x$surface)) return(.hscs_prop(NA, missing = "top_cm"))
  ok <- .hscs_pm_cemented_rows(x)
  v <- .hscs_run(x, ok, top_max = x$surface + 100, min_th = 0)
  top <- NA_real_; lab <- NULL
  if (isTRUE(v)) {
    top <- .hscs_run_top(x, ok, top_max = x$surface + 100, min_th = 0)
    i <- x$ord[ok[x$ord] %in% TRUE & abs(x$h$top_cm[x$ord] - top) <= 0.5][1L]
    lab <- .hscs_pm_agent(x, i)
  }
  .hscs_prop(v, top = top, label = lab, layers = which(ok %in% TRUE),
             missing = if (is.na(v)) c("cementation_class", "designation", "pan_type") else character(0))
}
