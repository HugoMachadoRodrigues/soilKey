# =============================================================================
# v0.9.316 -- the humus, organic and peat properties of the Hungarian
# diagnostic soil classification (HSCS 2024; Micheli et al. 2024, second
# approximation): the humus classes of 8.5 (p. 30-31) on the surface
# horizons of 8.1 (p. 23-24); the organic layers at the surface, tozeges and
# szaraztozeges (p. 32-33); the peat subtypes of the Laptalajok (p. 30-33).
# Each is a .hscs_p_<id>(x) of the frame (R/hscs2024-properties.R), x =
# .hscs_horizons(pedon). The readings where the text is silent are written
# beside each one.
#
# Organic carbon of a horizon described in several layers (A1, A2): read
# layer by layer, as the frame reads the 1 % of the Mezosegi talajok key. A
# humus class holds when every layer of a humic horizon (20 cm or more, from
# the mineral soil surface) reaches the class's lower limit and no humic
# horizon reaches the next class's; so a Mezosegi talaj, whose key asks 1 %,
# is always humuszgazdag or igen erosen humuszos, the two its table lists.
# A surface horizon with no thickness asked (humuszszegeny, lapfold, a raw
# surface) is read on the layer at the mineral soil surface.
# =============================================================================

# Organic carbon of each row in [lo, hi), three-valued.
.hscs_oc_between <- function(x, lo, hi = Inf) {
  oc <- x$h$oc_pct
  ifelse(is.na(oc), NA, oc >= lo - 1e-9 & oc < hi - 1e-9)
}

# A humic horizon (8.1 humuszos talajszint: favourable structure, dark,
# organic carbon 0.6 % or more, base saturation 50 % or more, 20 cm or more
# thick) at the mineral soil surface whose every row has `oc_min` % organic
# carbon or more and holds `ok` (a colour); `bs = FALSE` for the umbric
# horizon (base saturation below 50 %); `min_th` its thickness. A soil with
# no mineral row has none.
.hscs_humic_with <- function(x, oc_min = 0.6, bs = TRUE, ok = TRUE, min_th = 20) {
  if (is.na(x$surface)) return(NA)
  if (is.na(x$ms)) return(FALSE)
  rows <- .and3v(.hscs_humic_rows(x, oc_min = oc_min, bs = bs), ok)
  .hscs_run(x, rows, top_max = x$ms + 0.5, min_th = min_th)
}

.HSCS_HUMIC_MISSING <- c("oc_pct", "structure_type", "munsell_value_moist", "munsell_chroma_moist", "bs_pct")

# A humus class of 8.5 (p. 30): a humic horizon whose organic carbon is
# between `lo` and `hi` % ("0,6-1%", "1-2%", "2-8%"): a humic horizon with
# every row at `lo` or more, and none with every row at `hi` or more (the
# shared limit goes to the higher class, as the Mezosegi key's "at least 1 %").
.hscs_humus_class <- function(x, lo, hi) {
  a <- .hscs_humic_with(x, oc_min = lo)
  b <- .hscs_humic_with(x, oc_min = hi)
  v <- .and3(a, !b)
  ok <- .hscs_humic_rows(x, oc_min = lo)
  .hscs_prop(v, layers = if (isTRUE(v)) which(ok %in% TRUE) else integer(0),
             missing = if (is.na(v)) .HSCS_HUMIC_MISSING else character(0))
}

# Humuszos (p. 30): a humic horizon with organic carbon 0.6-1 %.
.hscs_p_humuszos <- function(x) .hscs_humus_class(x, 0.6, 1)

# Humuszgazdag (p. 30): a humic horizon with organic carbon 1-2 %.
.hscs_p_humuszgazdag <- function(x) .hscs_humus_class(x, 1, 2)

# Igen erosen humuszos (p. 30): a humic horizon with organic carbon 2-8 %
# (8 % or more is lapfold, 8.1).
.hscs_p_igen_erosen_humuszos <- function(x) .hscs_humus_class(x, 2, 8)

# The row at the mineral soil surface: the surface horizon that 8.1 names
# by its organic carbon alone (humuszszegeny talajszint, lapfold, a raw
# surface). None when the soil has no mineral row, or when organic layers
# 40 cm or more thick lie over the mineral soil (a Laptalaj, whose surface
# horizon is the peat: the 40 cm of its key).
.hscs_surface_row <- function(x) {
  if (is.na(x$ms) || is.na(x$surface)) return(integer(0))
  if (x$ms - x$surface >= 40 - 0.25) return(integer(0))
  r <- x$ord[abs(x$h$top_cm[x$ord] - x$ms) <= 0.5 & x$mineral[x$ord]]
  if (length(r)) r[1L] else integer(0)
}

# A surface horizon with organic carbon in [lo, hi).
.hscs_surface_oc <- function(x, lo, hi) {
  if (is.na(x$surface)) return(.hscs_prop(NA, missing = "top_cm"))
  r <- .hscs_surface_row(x)
  if (!length(r)) return(.hscs_prop(FALSE, note = "no mineral surface horizon"))
  v <- .hscs_oc_between(x, lo, hi)[r]
  .hscs_prop(v, layers = if (isTRUE(v)) r else integer(0),
             missing = if (is.na(v)) "oc_pct" else character(0))
}

# Humuszszegeny (p. 30; 8.1 p. 24): a surface horizon with organic carbon
# 0.2-0.6 %.
.hscs_p_humuszszegeny <- function(x) .hscs_surface_oc(x, 0.2, 0.6)

# Lapfoldes (p. 30): a surface horizon with organic carbon 8-20 % (the
# lapfold of 8.1, a material short of organic: 20 % is organic).
.hscs_p_lapfoldes <- function(x) .hscs_surface_oc(x, 8, 20)

# Fekete (p. 29): a surface humic horizon with a moist chroma of 2 or less
# (in every row of it).
.hscs_p_fekete <- function(x) {
  ch <- x$h$munsell_chroma_moist
  v <- .hscs_humic_with(x, ok = ifelse(is.na(ch), NA, ch <= 2))
  .hscs_prop(v, missing = if (is.na(v)) .HSCS_HUMIC_MISSING else character(0))
}

# Mely humuszos retegu (p. 30): a surface humic horizon 50 cm or more thick.
.hscs_p_mely_humuszos_retegu <- function(x) {
  v <- .hscs_humic_with(x, min_th = 50)
  .hscs_prop(v, missing = if (is.na(v)) .HSCS_HUMIC_MISSING else character(0))
}

# Umbrikus (p. 33; 8.1 p. 24): a surface umbric horizon, a humic horizon
# with base saturation below 50 % (in every row of it).
.hscs_p_umbrikus <- function(x) {
  v <- .hscs_humic_with(x, bs = FALSE)
  .hscs_prop(v, missing = if (is.na(v)) .HSCS_HUMIC_MISSING else character(0))
}

# ---- nyers: no diagnostic horizon ---------------------------------------------

# Rows of a podzolic horizon (8.2, p. 25): a subsurface horizon with pH (H2O)
# below 6, organic carbon 0.5 % or more and a red moist colour (a hue redder
# than 7.5YR, or 7.5YR with a chroma above 4); the 10 cm are the run's. Read
# for the nyers check (the colour of a crushed sample is not recorded apart).
.hscs_podzolic_rows_nyers <- function(x) {
  h <- x$h
  hue <- toupper(gsub("[[:space:]]", "", as.character(h$munsell_hue_moist)))
  hn <- .nzsc_hue_num(hue); ch <- h$munsell_chroma_moist
  red <- ifelse(grepl("^N", hue), FALSE, ifelse(is.na(hn), NA,
         ifelse(hn < 17.5 - 1e-9, TRUE, ifelse(abs(hn - 17.5) < 1e-9, ifelse(is.na(ch), NA, ch > 4), FALSE))))
  ph <- h$ph_h2o; oc <- h$oc_pct
  Reduce(.and3v, list(x$sub3, x$mineral3, ifelse(is.na(ph), NA, ph < 6), ifelse(is.na(oc), NA, oc >= 0.5), red))
}

# Rows of an iron accumulation horizon (8.2, p. 26): 5 % or more iron or
# iron-manganese concretions (ferric and manganiferous nodules; their size
# and cementation are not recorded apart), or 15 % or more large red mottles
# (their size is not recorded: undecided at most). Mottles and nodules are
# recorded when met. Read for the nyers check.
.hscs_iron_rows_nyers <- function(x) {
  h <- x$h; n <- x$n
  fe <- h$ferric_nodules_pct %||% rep(NA_real_, n)
  mn <- h$manganiferous_nodules_pct %||% rep(NA_real_, n)
  nod <- ifelse(is.na(fe), 0, fe) + ifelse(is.na(mn), 0, mn)
  rx <- h$redoximorphic_features_pct
  big <- ifelse(is.na(rx) | rx < 15, FALSE, NA)
  Reduce(.and3v, list(x$sub3, x$mineral3, .any3v(nod >= 5, big)))
}

# The diagnostic horizons of 8.1 and 8.2 a soil may have, three-valued,
# within 100 cm of the soil surface (the nyers definition gives no depth;
# the key and 8.5 read every subsurface horizon within 100 cm, and the soil
# below a description that ends above it, not on rock, may hold one). The surface horizons read by
# their organic carbon (lapfold, humic, umbric, humuszszegeny) are left to
# the organic carbon of the surface that nyers reads; the anthropogenic
# horizon by a raised surface of human-made or carried material 20 cm or
# more thick (8.1 item 2) or Al-P P2O5 of 500 mg/kg or more in the upper
# 20 cm where it is recorded (as the Antropogen key, the P route is not read
# where it is not).
.hscs_diagnostic_horizons <- function(x) {
  h <- x$h; s <- x$surface
  org <- .hscs_organic_rows(x)
  an <- .hscs_anthropic_rows(x)
  terric <- isTRUE(.hscs_run(x, an, top_max = s + 0.5, min_th = 20, below_open = FALSE))
  p <- h$p2o5_al_mg_kg %||% rep(NA_real_, x$n)
  r20 <- .hscs_rows_in(x, s, s + 20)
  hort <- if (!length(r20) || all(is.na(p[r20]))) FALSE else {
    w <- .hscs_wmean(x, p, s, s + 20, vmin = 0, vmax = 1e9)
    if (w[["lo"]] >= 500) TRUE else if (w[["hi"]] < 500) FALSE else NA
  }
  z <- s + 100
  v <- c(organic = .hscs_run(x, org, top_max = z),
         anthropogenic = .any3(c(terric, hort)),
         clay_accumulation = .hscs_clay_accum(x, 100),
         swelling = .hscs_run(x, .hscs_swelling_rows(x), top_max = z, min_th = 25),
         cambic = .hscs_run(x, .hscs_cambic_rows(x), top_max = z, min_th = 15),
         podzolic = .hscs_run(x, .hscs_podzolic_rows_nyers(x), top_max = z, min_th = 10),
         solonetz = .hscs_run(x, .hscs_solonetz_rows(x), top_max = z),
         solonchak = .hscs_run(x, .hscs_solonchak_rows(x), top_max = z, min_th = 15),
         iron = .hscs_run(x, .hscs_iron_rows_nyers(x), top_max = z, min_th = 15))
  v
}

.HSCS_HORIZON_MISSING <- list(
  organic = c("oc_pct", "designation"), anthropogenic = c("layer_origin", "p2o5_al_mg_kg"),
  clay_accumulation = c("clay_pct", "clay_films_amount"),
  swelling = c("clay_pct", "slickensides", "structure_type"),
  cambic = c("designation", "structure_grade", "munsell_value_moist", "munsell_chroma_moist", "caco3_pct"),
  podzolic = c("ph_h2o", "oc_pct", "munsell_hue_moist"),
  solonetz = c("na_cmol", "mg_cmol", "cec_cmol", "structure_type"),
  solonchak = "soluble_salts_pct", iron = "redoximorphic_features_pct")

# Nyers (p. 31; 8.1 p. 24, "nyers / jellegtelen felszinek"): no surface or
# subsurface diagnostic horizon (within 100 cm), and organic carbon below
# 0.2 % (read at the surface, as 8.1 defines the raw surface).
.hscs_p_nyers <- function(x) {
  if (is.na(x$surface)) return(.hscs_prop(NA, missing = "top_cm"))
  r <- .hscs_surface_row(x)
  if (!length(r)) return(.hscs_prop(FALSE, note = "no mineral surface horizon"))
  low <- .hscs_oc_between(x, -Inf, 0.2)[r]
  if (isFALSE(low)) return(.hscs_prop(FALSE))
  dh <- .hscs_diagnostic_horizons(x)
  v <- .and3(low, !.any3(dh))
  miss <- if (is.na(v)) unique(c(if (is.na(low)) "oc_pct", unlist(.HSCS_HORIZON_MISSING[names(dh)[is.na(dh)]])))
          else character(0)
  .hscs_prop(v, layers = if (isTRUE(v)) r else integer(0), missing = miss,
             note = if (any(dh %in% TRUE)) paste("has:", paste(names(dh)[dh %in% TRUE], collapse = ", ")) else NULL)
}

# ---- organic layers: peat and litter (8.1, p. 23-24) ----------------------------

# A forest floor written in the L, F, H notation (an F, LF or FH layer
# shows it): the run of L, F and H layers from the surface. Its H is
# humified litter, not the peat (H) of the FAO (2006) notation, and its L is
# litter, not limnic material.
.hscs_forest_floor_rows <- function(x) {
  d <- trimws(as.character(x$h$designation))
  out <- rep(FALSE, x$n)
  if (!any(!is.na(d) & grepl("^(F|LF|FH)[a-z]*[0-9]*$", d))) return(out)
  prev <- NA
  for (i in x$ord) {
    if (is.na(d[i]) || !grepl("^(L|F|H|LF|FH)[a-z]*[0-9]*$", d[i])) break
    if (!is.na(prev) && abs(x$h$top_cm[i] - prev) > 0.5) break
    out[i] <- TRUE
    prev <- x$h$bottom_cm[i]
    if (is.na(prev)) break
  }
  out
}

# Organic horizons (8.1: 20 % organic carbon or more), as the frame reads
# them; a forest-floor layer with no carbon measured is organic too.
.hscs_organic_rows <- function(x) {
  org <- x$organic
  ff <- .hscs_forest_floor_rows(x)
  org[ff & is.na(org)] <- TRUE
  org
}

# Water-saturated for the greater part of the year ("az ev nagyobb reszeben
# vizzel telitett", the peat horizon of 8.1) or aerated ("atszellozott", the
# litter and grass-mat horizons), per row: the days of saturation where
# recorded (183 or more); else the FAO (2006) notation the text adopts, in
# which an H horizon is saturated for prolonged periods (or was, and is now
# artificially drained) and an O horizon is not (unless the site declares
# the USDA notation, whose O holds both); the L, F, H layers of a forest
# floor are aerated; else the site's drainage class (very poorly drained:
# saturated; moderately well drained or drier: aerated, unless the site is
# artificially drained, when a drained peat may lie there).
.hscs_saturated_rows <- function(x) {
  h <- x$h; n <- x$n
  days <- suppressWarnings(as.numeric(h$water_saturation_days %||% rep(NA_real_, n)))
  ff <- .hscs_forest_floor_rows(x)
  sysd <- tolower(as.character(.hscs_site1(x, "designation_system")))
  usda <- !is.na(sysd) && grepl("usda|kst|nasis|soil taxonomy", sysd)
  by_des <- ifelse(ff, FALSE, ifelse(x$role %in% "H", TRUE, ifelse(x$role %in% "O" & !usda, FALSE, NA)))
  dr <- .hscs_site1(x, "drainage_class")
  if (is.na(dr)) dr <- .hscs_site1(x, "drainage")
  drier <- .drainage_class(dr, "drier"); wetter <- .drainage_class(dr, "wetter")
  drained <- .hscs_truthy(.hscs_site1(x, "artificially_drained"))
  by_site <- if (!is.na(drier) && drier >= 8L) TRUE
             else if (!is.na(wetter) && wetter <= 5L && !isTRUE(drained)) FALSE else NA
  ifelse(!is.na(days), days >= 183, ifelse(is.na(by_des), by_site, by_des))
}

# Rows of a peat horizon (tozeg szint): an organic horizon water-saturated
# for the greater part of the year. Limnic material (an L of the FAO
# notation) is not peat.
.hscs_peat_rows <- function(x) {
  ff <- .hscs_forest_floor_rows(x)
  limnic <- x$role %in% "L" & !ff
  ifelse(limnic, FALSE, .and3v(.hscs_organic_rows(x), .hscs_saturated_rows(x)))
}

# Forest vegetation at the site, from its vegetation, land use or land
# cover (English or Hungarian words): TRUE for a forest, FALSE for
# grassland or cropland, NA when not recorded or both.
.hscs_forest_site <- function(x) {
  s <- x$site
  txt <- unlist(lapply(c("vegetation", "land_use", "land_cover"), function(k) as.character(unlist(s[[k]]))))
  txt <- .field_term(txt[!is.na(txt) & nzchar(trimws(txt))])
  if (!length(txt)) return(NA)
  txt <- paste(txt, collapse = " ")
  f <- grepl(paste0("forest|wood|\\btrees?\\b|conifer|decidu|beech|\\boak|\\bpine|spruce|\\bfirs?\\b|larch|",
                    "hornbeam|poplar|willow|alder|plantation|erd[\u0151o]|b[\u00fcu]kk|t[\u00f6o]lgy|",
                    "feny[\u0151o]|gyertyan|akac|\\bf[u\u0171]z"), txt)
  g <- grepl(paste0("grass|meadow|pasture|steppe|prairie|crop|arable|cultivat|gyep|\\bret\\b|",
                    "legel[\u0151o]|kaszal|szanto"), txt)
  if (f && !g) TRUE else if (g && !f) FALSE else NA
}

# Rows of a litter horizon (alom szint): an organic horizon aerated for the
# greater part of the year with the plant remains of a forest (the layers of
# a forest floor; else the site's vegetation). The grass-mat horizon (gyep
# szint) is the one under grassland.
.hscs_litter_rows <- function(x) {
  ff <- .hscs_forest_floor_rows(x)
  sat <- .hscs_saturated_rows(x)
  forest <- ifelse(ff, TRUE, .hscs_forest_site(x))
  aer <- ifelse(is.na(sat), NA, !sat)
  .and3v(.and3v(.hscs_organic_rows(x), aer), forest)
}

.HSCS_ORGANIC_MISSING <- c("oc_pct", "designation", "water_saturation_days", "site$drainage_class")

# Tozeges (p. 33): a peat horizon 10 cm or more thick at the soil surface
# (its top at the surface; not in the Laptalajok).
.hscs_p_tozeges <- function(x) {
  if (is.na(x$surface)) return(.hscs_prop(NA, missing = "top_cm"))
  ok <- .hscs_peat_rows(x)
  v <- .hscs_run(x, ok, top_max = x$surface + 0.5, min_th = 10)
  .hscs_prop(v, layers = if (isTRUE(v)) which(ok %in% TRUE) else integer(0),
             missing = if (is.na(v)) .HSCS_ORGANIC_MISSING else character(0))
}

# Szaraztozeges (p. 32): a litter horizon 10 cm or more thick at the soil
# surface (not in the Laptalajok).
.hscs_p_szaraztozeges <- function(x) {
  if (is.na(x$surface)) return(.hscs_prop(NA, missing = "top_cm"))
  ok <- .hscs_litter_rows(x)
  v <- .hscs_run(x, ok, top_max = x$surface + 0.5, min_th = 10)
  .hscs_prop(v, layers = if (isTRUE(v)) which(ok %in% TRUE) else integer(0),
             missing = if (is.na(v)) c(.HSCS_ORGANIC_MISSING, "site$vegetation") else character(0))
}

# ---- peat of the Laptalajok -------------------------------------------------------

# Thickness-weighted mean of the bounds [lo, hi] over the rows where
# `inside` holds; a row of unknown thickness, or one that may or may not be
# inside (NA), widens the bounds to its own. NULL when no row may be inside;
# `present`: some row surely is.
.hscs_wmean_rows <- function(x, inside, lo, hi) {
  th <- x$thick
  sure <- which(inside %in% TRUE & !is.na(th) & th > 0)
  maybe <- which(is.na(inside) | (inside %in% TRUE & is.na(th)))
  if (!length(sure) && !length(maybe)) return(NULL)
  if (length(sure)) {
    w <- th[sure]
    L <- sum(w * lo[sure]) / sum(w); H <- sum(w * hi[sure]) / sum(w)
  } else { L <- Inf; H <- -Inf }
  if (length(maybe)) { L <- min(L, lo[maybe]); H <- max(H, hi[maybe]) }
  list(lo = L, hi = H, present = any(inside %in% TRUE))
}

# Recognisable plant parts of each row, % by volume, as bounds. The rubbed
# fibre content: the limits 2/3 and 1/6 are those of the WRB Fibric, Hemic
# and Sapric, read after rubbing (unrubbed fibre counts fragments of
# aggregates). Else the von Post class (H1-H4 more than 2/3, H5-H6 between,
# H7-H10 less than 1/6, as soilKey reads them for the WRB), else the suffix
# of the organic horizon in the FAO (2006) notation (i slightly, e
# moderately, a highly decomposed); the unrubbed fibre, which rubbing only
# lowers, caps the bounds.
.hscs_fibre_bounds <- function(x) {
  h <- x$h; n <- x$n
  rub <- suppressWarnings(as.numeric(h$fiber_content_rubbed_pct %||% rep(NA_real_, n)))
  unr <- suppressWarnings(as.numeric(h$fiber_content_unrubbed_pct %||% rep(NA_real_, n)))
  vp <- suppressWarnings(as.numeric(h$von_post_index %||% rep(NA_real_, n)))
  suf <- vapply(seq_len(n), function(i) {
    s <- x$des$strong[[i]]
    if ("i" %in% s) "f" else if ("e" %in% s) "h" else if ("a" %in% s) "s" else NA_character_
  }, "")
  cls <- ifelse(!is.na(vp), ifelse(vp <= 4, "f", ifelse(vp <= 6, "h", "s")),
                ifelse(x$role %in% c("H", "O"), suf, NA_character_))
  two3 <- 200 / 3; one6 <- 100 / 6
  lo <- ifelse(cls %in% "f", two3 + 1e-6, ifelse(cls %in% "h", one6, 0))
  hi <- ifelse(cls %in% "s", one6 - 1e-6, ifelse(cls %in% "h", two3, 100))
  cap <- is.na(rub) & !is.na(unr)
  hi[cap] <- pmin(hi[cap], unr[cap])
  lo[cap & lo > hi] <- 0
  lo[!is.na(rub)] <- rub[!is.na(rub)]; hi[!is.na(rub)] <- rub[!is.na(rub)]
  list(lo = lo, hi = hi)
}

# The share of recognisable plant parts in the volume of the peat
# horizon(s) ("a tozeg talajszint(ek) terfogatanak"), over the peat rows the
# description holds.
.hscs_peat_fibre <- function(x) {
  b <- .hscs_fibre_bounds(x)
  .hscs_wmean_rows(x, .hscs_peat_rows(x), b$lo, b$hi)
}

.HSCS_FIBRE_MISSING <- c("fiber_content_rubbed_pct", "von_post_index")

.hscs_fibre_prop <- function(x, cls) {
  f <- .hscs_peat_fibre(x)
  if (is.null(f)) return(.hscs_prop(FALSE, note = "no peat horizon"))
  two3 <- 200 / 3; one6 <- 100 / 6; e <- 1e-9
  v <- switch(cls,
    rostos = if (f$present && f$lo > two3 + e) TRUE else if (f$hi <= two3 + e) FALSE else NA,
    vegyes = if (f$present && f$lo >= one6 - e && f$hi <= two3 + e) TRUE
             else if (f$hi < one6 - e || f$lo > two3 + e) FALSE else NA,
    kotus  = if (f$present && f$hi < one6 - e) TRUE else if (f$lo >= one6 - e) FALSE else NA)
  .hscs_prop(v, missing = if (is.na(v)) .HSCS_FIBRE_MISSING else character(0),
             note = sprintf("fibre %.0f-%.0f %%", f$lo, f$hi))
}

# Rostostozeges (p. 31): more than 2/3 of the volume of the peat horizon(s)
# has recognisable plant parts.
.hscs_p_rostos_tozeges <- function(x) .hscs_fibre_prop(x, "rostos")

# Vegyes tozeges (p. 33): between 1/6 and 2/3.
.hscs_p_vegyes_tozeges <- function(x) .hscs_fibre_prop(x, "vegyes")

# Kotus (p. 30): less than 1/6.
.hscs_p_kotus <- function(x) .hscs_fibre_prop(x, "kotus")

# Mohatozeges (p. 31): the organic soil material is undecomposed or
# partially decomposed Sphagnum: more than half of the fibres of the organic
# horizons, thickness-weighted, are Sphagnum (sphagnum_fibres_pct, recorded
# when met).
.hscs_p_mohatozeges <- function(x) {
  s <- suppressWarnings(as.numeric(x$h$sphagnum_fibres_pct %||% rep(NA_real_, x$n)))
  s <- ifelse(is.na(s), 0, s)
  m <- .hscs_wmean_rows(x, .hscs_organic_rows(x), s, s)
  if (is.null(m)) return(.hscs_prop(FALSE, note = "no organic horizon"))
  v <- if (m$present && m$lo > 50) TRUE else if (m$hi <= 50) FALSE else NA
  .hscs_prop(v, missing = if (is.na(v)) c("oc_pct", "bottom_cm", "sphagnum_fibres_pct") else character(0))
}

# Lecsapolt (p. 30): a Laptalaj whose groundwater level is regulated: the
# site is artificially drained (artificially_drained; else a mark of
# artificial drainage in the drainage class or the land use, as the WRB
# Drainic reads it).
.hscs_p_lecsapolt <- function(x) {
  d <- .hscs_truthy(.hscs_site1(x, "artificially_drained"))
  if (!is.na(d)) return(.hscs_prop(d))
  txt <- as.character(c(.hscs_site1(x, "drainage_class"), .hscs_site1(x, "drainage"), unlist(x$site$land_use)))
  txt <- paste(txt[!is.na(txt)], collapse = " ")
  re <- "artific|reclaim|drained\\s+(organic|peat|mire|wetland|fen|bog)|\\bditch|drained\\)|lecsapol"
  if (nzchar(txt) && grepl(re, .field_term(txt))) return(.hscs_prop(TRUE))
  .hscs_prop(NA, missing = "site$artificially_drained")
}

# The base of the peat, cm from the soil surface, as bounds: `lo` the
# deepest bottom of a row surely peat, `hi` how deep the peat may reach (a
# row that may be peat, a peat row with no bottom, a gap in the description
# under a peat row, or peat at the end of a description that does not end
# on rock, may go deeper). NULL when no row may be peat.
.hscs_peat_base <- function(x) {
  h <- x$h; pt <- .hscs_peat_rows(x)
  sure <- which(pt %in% TRUE); maybe <- which(!(pt %in% FALSE))
  if (!length(maybe)) return(NULL)
  b <- h$bottom_cm
  lo <- if (length(sure)) max(ifelse(is.na(b[sure]), h$top_cm[sure], b[sure])) else -Inf
  hi <- max(ifelse(is.na(b[maybe]), Inf, b[maybe]))
  bel <- .hscs_below(x)
  for (i in maybe) {
    if (!is.na(bel[i])) next
    if (identical(i, x$last)) { if (!x$ends_on_rock) hi <- Inf; next }
    nxt <- x$ord[h$top_cm[x$ord] > h$top_cm[i] + 0.5]
    if (length(nxt)) hi <- max(hi, h$top_cm[nxt[1L]])
  }
  list(lo = lo - x$surface, hi = hi - x$surface, present = length(sure) > 0)
}

# Mely (p. 30) and sekely (p. 32): a peat horizon deeper, or not deeper,
# than 50 cm: read by the depth its base reaches from the soil surface (the
# thickness when the peat starts at the surface; the peat under a mineral
# cover reaches deeper than its thickness).
.hscs_peat_depth_prop <- function(x, deep) {
  if (is.na(x$surface)) return(.hscs_prop(NA, missing = "top_cm"))
  b <- .hscs_peat_base(x)
  if (is.null(b)) return(.hscs_prop(FALSE, note = "no peat horizon"))
  lim <- 50 + 0.25
  v <- if (deep) { if (b$present && b$lo > lim) TRUE else if (b$hi <= lim) FALSE else NA }
       else { if (b$present && b$hi <= lim) TRUE else if (b$lo > lim) FALSE else NA }
  .hscs_prop(v, missing = if (is.na(v)) c("bottom_cm", "designation", .HSCS_ORGANIC_MISSING[-2]) else character(0),
             note = sprintf("peat base %s-%s cm", format(b$lo), format(b$hi)))
}

.hscs_p_mely <- function(x) .hscs_peat_depth_prop(x, TRUE)
.hscs_p_sekely <- function(x) .hscs_peat_depth_prop(x, FALSE)
