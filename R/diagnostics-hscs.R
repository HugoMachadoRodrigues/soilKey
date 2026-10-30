# =============================================================================
# v0.9.305 -- diagnostics of the Hungarian diagnostic soil classification
# (HSCS 2024; Micheli et al., second approximation), section 8: the
# diagnostic horizons (talajszintek), properties (talajtulajdonsagok) and
# materials (talajanyagok) that the key to the 15 types reads. Three-valued:
# TRUE, FALSE or NA (undecided).
#
# Depths are from the soil surface (the top of the first horizon, organic
# layers included), as the key writes "from the soil surface"; where the
# text says the mineral soil surface, from the top of the first mineral
# horizon. Texture classes are those of the USDA triangle, which the text
# prints with Hungarian names (p. 36). Hugo's decisions of 2026-10-29 and
# the choices where the text is silent are in
# inst/rules/hscs2024/readings.yaml. Names carry the _hscs suffix.
# =============================================================================

.HSCS_REF <- "Micheli et al. (2024). A diagnosztikus szemleletu hazai talajosztalyozasi rendszer, masodik kozelites."

.hscs_result <- function(name, passed, layers = integer(0), evidence = list(),
                         missing = character(0), where = "") {
  DiagnosticResult$new(name = name, passed = passed, layers = as.integer(layers),
                       evidence = evidence, missing = unique(missing),
                       reference = sprintf("%s %s", .HSCS_REF, where), notes = NA_character_)
}

.hscs_txt <- function(v, n) {
  v <- v %||% rep(NA_character_, n)
  out <- tolower(trimws(as.character(v)))
  out[!nzchar(out)] <- NA_character_
  out
}

# ---- texture ------------------------------------------------------------------

# Texture class of each horizon as a USDA class name: from a Hungarian field
# texture (the names of the triangle on p. 36, with or without accents), an
# English one or an Australian code (as the NZSC reads them), else from lab
# clay, silt and sand.
.hscs_texture_class <- function(h) {
  n <- nrow(h)
  ft <- .hscs_txt(h$field_texture, n)
  a <- "[a\u00e1]"
  hu <- stats::setNames(
    c("silty clay loam", "sandy clay loam", "clay loam", "silty clay", "sandy clay",
      "loamy sand", "sandy loam", "silt loam", "sand", "silt", "loam", "clay"),
    c(paste0("iszapos agyagos v", a, "lyog"), paste0("homokos agyagos v", a, "lyog"),
      paste0("agyagos v", a, "lyog"), "iszapos agyag", "homokos agyag",
      paste0("v", a, "lyogos homok"), paste0("homokos v", a, "lyog"), paste0("iszapos v", a, "lyog"),
      "^(durva |finom )?homok$", "^iszap$", paste0("^v", a, "lyog$"), "^agyag$"))
  word <- rep(NA_character_, n)
  for (k in names(hu)) word[is.na(word) & !is.na(ft) & grepl(k, ft)] <- hu[[k]]
  en <- .nzsc_texture(h)$class
  word[is.na(word)] <- en[is.na(word)]
  word[word %in% "peat"] <- NA_character_
  lab <- texture_class_from_pct(h$clay_pct, h$silt_pct, h$sand_pct)
  lab <- unname(c(S = "sand", LS = "loamy sand", SL = "sandy loam", L = "loam", SIL = "silt loam",
                  SI = "silt", SCL = "sandy clay loam", CL = "clay loam", SICL = "silty clay loam",
                  SC = "sandy clay", SIC = "silty clay", C = "clay")[lab])
  ifelse(is.na(word), lab, word)
}

# Clay of each horizon as a range: measured clay, else the range of its
# texture class on the USDA triangle.
.hscs_clay_range <- function(x) {
  cl <- x$h$clay_pct
  lo_c <- c("sand" = 0, "loamy sand" = 0, "sandy loam" = 0, "loam" = 7, "silt loam" = 0, "silt" = 0,
            "sandy clay loam" = 20, "clay loam" = 27, "silty clay loam" = 27, "sandy clay" = 35,
            "silty clay" = 40, "clay" = 40)
  hi_c <- c("sand" = 10, "loamy sand" = 15, "sandy loam" = 20, "loam" = 27, "silt loam" = 27, "silt" = 12,
            "sandy clay loam" = 35, "clay loam" = 40, "silty clay loam" = 40, "sandy clay" = 55,
            "silty clay" = 60, "clay" = 100)
  lo <- ifelse(!is.na(cl), cl, unname(lo_c[x$tex]))
  hi <- ifelse(!is.na(cl), cl, unname(hi_c[x$tex]))
  list(lo = lo, hi = hi)
}

# "At least `thr` % clay" per horizon, three-valued.
.hscs_clay_at_least <- function(x, thr) {
  r <- .hscs_clay_range(x)
  ifelse(!is.na(r$lo) & r$lo >= thr, TRUE, ifelse(!is.na(r$hi) & r$hi < thr, FALSE, NA))
}


# Sandy texture (coarse sand, sand, loamy sand); with no texture, clay above
# 15 % is not sandy.
.hscs_sandy_rows <- function(x) {
  cl <- x$h$clay_pct
  ifelse(!is.na(x$tex), x$tex %in% c("sand", "loamy sand"), ifelse(!is.na(cl) & cl > 15, FALSE, NA))
}

# Loamy sand or finer (not sand); with no texture, clay above 10 % is not
# sand.
.hscs_not_sand_rows <- function(x) {
  cl <- x$h$clay_pct
  ifelse(!is.na(x$tex), x$tex != "sand", ifelse(!is.na(cl) & cl > 10, TRUE, NA))
}

# ---- the profile as the HSCS reads it ----------------------------------------------

.hscs_horizons <- function(pedon) {
  h <- as.data.frame(pedon$horizons, stringsAsFactors = FALSE)
  n <- nrow(h)
  des <- .hscs_designations(h$designation %||% rep(NA_character_, n))
  role <- des$role
  top <- h$top_cm; bot <- h$bottom_cm
  ord <- order(top, na.last = NA)
  thick <- ifelse(is.na(top) | is.na(bot), NA_real_, pmax(0, bot - top))
  oc <- h$oc_pct
  rock <- role %in% "R"
  # a recorded mineral texture or particle size shows mineral material
  ft <- .hscs_txt(h$field_texture, n)
  min_ev <- (!is.na(ft) & !grepl("peat|t\u0151zeg|tozeg", ft)) | !is.na(h$clay_pct) | !is.na(h$sand_pct)
  # an organic horizon: 20 % organic carbon or more; a peat (H) or organic
  # (O) master with no carbon measured
  organic <- ifelse(!is.na(oc), oc >= 20, ifelse(role %in% c("H", "O"), TRUE,
             ifelse(des$known | min_ev, FALSE, NA)))
  organic[rock] <- FALSE
  surface <- if (length(ord)) top[ord[1L]] else NA_real_
  first_min <- ord[!(organic[ord] %in% TRUE) & !rock[ord]][1L]
  ms <- if (length(first_min) && !is.na(first_min)) top[first_min] else NA_real_
  first <- if (length(ord)) ord[1L] else NA_integer_
  last <- if (length(ord)) ord[length(ord)] else NA_integer_
  site <- pedon$site %||% list()
  land <- tolower(paste(as.character(unlist(site$land_use %||% "")), collapse = " "))
  # subsurface horizons: not the uppermost row, not an A, H or O horizon; a
  # row with no designation in the run of A (or undesignated) rows from the
  # surface may be an A, so it may or may not be one
  sub3 <- ifelse(seq_len(n) == first | role %in% c("A", "H", "O"), FALSE, TRUE)
  prevb <- NA
  for (i in ord) {
    if (!(is.na(role[i]) || role[i] %in% c("A", "H", "O"))) break
    if (!is.na(prevb) && abs(top[i] - prevb) > 0.5) break
    if (is.na(role[i]) && !identical(i, first)) sub3[i] <- NA
    prevb <- bot[i]
    if (is.na(prevb)) break
  }
  # ploughed: an Ap (not buried) at the soil or mineral soil surface, or a
  # site land use naming cropland
  at_top <- !is.na(top) & ((!is.na(surface) & abs(top - surface) <= 0.5) | (!is.na(ms) & abs(top - ms) <= 0.5))
  list(h = h, n = n, des = des, role = role, ord = ord, thick = thick,
       organic = organic, rock = rock,
       # soil material, not rock: `mineral` for indexing (a row of unknown
       # material counts), `mineral3` three-valued
       mineral = !(organic %in% TRUE) & !rock,
       mineral3 = ifelse(rock, FALSE, ifelse(is.na(organic), NA, !organic)),
       sub3 = sub3,
       surface = surface, ms = ms, last = last,
       ends_on_rock = !is.na(last) && rock[last],
       site = site, tex = .hscs_texture_class(h),
       ploughed = any(des$p & !des$buried & at_top) || grepl("crop|arable|plough|plow|tilled|cultivat", land))
}

# The site field `nm` (first value), or NA.
.hscs_site1 <- function(x, nm) {
  v <- x$site[[nm]]
  if (is.null(v) || !length(v)) NA else v[[1L]]
}

.hscs_truthy <- function(v) {
  v <- tolower(as.character(v))
  if (is.na(v)) NA else if (v %in% c("true", "t", "yes", "y", "1")) TRUE
  else if (v %in% c("false", "f", "no", "n", "0")) FALSE else NA
}

# Rows (depth order) overlapping the window [zt, zb).
.hscs_rows_in <- function(x, zt, zb) {
  h <- x$h; r <- x$ord
  bot <- ifelse(is.na(h$bottom_cm[r]), Inf, h$bottom_cm[r])
  r[h$top_cm[r] < zb - 0.5 & bot > zt + 0.5]
}

# A run of adjacent rows (depth order) where `ok` holds, starting above
# `top_max` ("within top_max cm": a top at top_max is not within) and
# `min_th` cm or more thick: TRUE when rows that surely hold make one, FALSE
# when not even rows that may hold do. A row with no bottom adds an unknown
# thickness. A gap in the description ends a sure run, but what the gap holds
# is unknown, so a possible run goes on through it (or starts in it); below a
# description that does not end on rock the soil may go on likewise.
.hscs_run <- function(x, ok, top_max = Inf, min_th = 0, below_open = TRUE) {
  h <- x$h; r <- x$ord
  s_top <- NA; s_th <- 0; p_top <- NA; p_th <- 0; prev <- NA
  sure <- FALSE; poss <- FALSE
  chk <- function() {
    if (!is.na(s_top) && s_top < top_max - 0.25 && s_th >= min_th - 1e-9) sure <<- TRUE
    if (!is.na(p_top) && p_top < top_max - 0.25 && p_th >= min_th - 1e-9) poss <<- TRUE
  }
  for (i in r) {
    t <- h$top_cm[i]; th <- x$thick[i]
    if (!is.na(prev) && abs(t - prev) > 0.5) {
      s_top <- NA; s_th <- 0
      if (t > prev) { if (is.na(p_top)) p_top <- prev; p_th <- Inf; chk() }
      else { p_top <- NA; p_th <- 0 }
    }
    if (ok[i] %in% TRUE) { if (is.na(s_top)) s_top <- t; s_th <- s_th + (if (is.na(th)) 0 else th) }
    else { s_top <- NA; s_th <- 0 }
    if (!(ok[i] %in% FALSE)) { if (is.na(p_top)) p_top <- t; p_th <- p_th + (if (is.na(th)) Inf else th) }
    else { p_top <- NA; p_th <- 0 }
    chk()
    prev <- h$bottom_cm[i]
  }
  if (below_open && !x$ends_on_rock && !is.na(prev)) {
    if (is.na(p_top)) p_top <- prev
    p_th <- Inf; chk()
  }
  if (sure) TRUE else if (!poss) FALSE else NA
}

# Every part of the window [zt, zb] holds `ok`: FALSE when a row in it fails,
# TRUE when rows that hold cover it, NA otherwise.
.hscs_all_in <- function(x, ok, zt, zb) {
  if (is.na(zt) || is.na(zb) || zb <= zt) return(NA)
  h <- x$h; r <- .hscs_rows_in(x, zt, zb)
  if (any(ok[r] %in% FALSE)) return(FALSE)
  t <- r[ok[r] %in% TRUE]
  cov <- sum(.asc_overlap(h$top_cm[t], ifelse(is.na(h$bottom_cm[t]), zb, h$bottom_cm[t]), zt, zb))
  if (cov >= zb - zt - 0.5) TRUE else NA
}

# Share of the window [zt, zb] where `ok` holds, as bounds: lo counts the
# rows that hold, hi everything but the rows that fail (an uncovered part is
# unknown). An open bottom reaches the window's base.
.hscs_share <- function(x, ok, zt, zb) {
  h <- x$h; r <- .hscs_rows_in(x, zt, zb)
  ov <- .asc_overlap(h$top_cm[r], ifelse(is.na(h$bottom_cm[r]), zb, h$bottom_cm[r]), zt, zb)
  span <- zb - zt
  c(lo = sum(ov[ok[r] %in% TRUE]) / span, hi = 1 - sum(ov[ok[r] %in% FALSE]) / span)
}

# Thickness-weighted average of `v` over [zt, zb], as bounds when rows are
# missing a value (lo and hi read a missing value as `vmin` and `vmax`); the
# part of the window no row covers counts as missing.
.hscs_wmean <- function(x, v, zt, zb, vmin = 0, vmax = 100) {
  h <- x$h; r <- .hscs_rows_in(x, zt, zb)
  ov <- .asc_overlap(h$top_cm[r], ifelse(is.na(h$bottom_cm[r]), zb, h$bottom_cm[r]), zt, zb)
  span <- zb - zt
  gap <- max(0, span - sum(ov))
  vv <- v[r]
  lo <- (sum(ov * ifelse(is.na(vv), vmin, vv)) + gap * vmin) / span
  hi <- (sum(ov * ifelse(is.na(vv), vmax, vv)) + gap * vmax) / span
  c(lo = lo, hi = hi)
}

# The row directly above / below each row (depth order, adjacent), or NA.
.hscs_above <- function(x) {
  h <- x$h; out <- rep(NA_integer_, x$n); r <- x$ord
  for (k in seq_along(r)[-1L]) {
    i <- r[k]; j <- r[k - 1L]
    if (!is.na(h$bottom_cm[j]) && abs(h$top_cm[i] - h$bottom_cm[j]) <= 0.5) out[i] <- j
  }
  out
}
.hscs_below <- function(x) {
  ab <- .hscs_above(x); out <- rep(NA_integer_, x$n)
  for (i in which(!is.na(ab))) out[ab[i]] <- i
  out
}

# ---- 8.3 properties -------------------------------------------------------------

# Continuous hard rock (osszefuggo kemeny kozet): an R horizon; cemented soil
# horizons are not rock. The top of the uppermost one, or NA.
.hscs_rock_top <- function(x) {
  r <- x$ord[x$rock[x$ord]]
  if (length(r)) x$h$top_cm[r[1L]] else NA_real_
}

# A cemented layer (rooting-restricting): an m suffix or a cementation class
# moderately cemented or stronger. Its top, or NA.
.hscs_cemented_top <- function(x) {
  cem <- .hscs_txt(x$h$cementation_class, x$n)
  hard <- x$des$m | cem %in% c("moderately", "strongly", "indurated")
  r <- x$ord[hard[x$ord]]
  if (length(r)) x$h$top_cm[r[1L]] else NA_real_
}

# Reductive conditions (reduktiv viszonyok) shown in a row: a positive
# dipyridyl (Childs') test, iron sulfides, or (Hugo's decision, 2026-10-29) an
# l or r suffix of the FAO (2006) notation; methane is a site record.
# TRUE where shown, FALSE where a negative test is recorded, NA otherwise.
.hscs_reductive_rows <- function(x) {
  h <- x$h; n <- x$n
  ch <- .hscs_txt(h$childs_test, n)
  sulf <- h$sulfidic_s_pct %||% rep(NA_real_, n)
  pos <- grepl("^pos|^yes|^true|^\\+", ch) | (!is.na(sulf) & sulf > 0) | x$des$l | x$des$r
  ifelse(pos, TRUE, ifelse(grepl("^neg|^no|^false|^-", ch), FALSE, NA))
}

# Reduced (gley) colours of the groundwater gley pattern: the hues the text
# lists, 2.5Y and 5Y read with moist chroma 2 or less (grey, as the text
# names them), and green, blue or neutral (GLEY) hues.
.hscs_reduced_colour <- function(x) {
  h <- x$h
  hue <- toupper(gsub("[[:space:]]", "", as.character(h$munsell_hue_moist)))
  ch <- h$munsell_chroma_moist
  gley <- grepl("^(N|GLEY|[0-9.]*(G|B|GY|BG|PB))$", hue) | grepl("GLEY", hue)
  yel <- grepl("^(2\\.5|5)Y$", hue)
  ifelse(is.na(hue), NA, ifelse(gley, TRUE, ifelse(yel, ifelse(is.na(ch), NA, ch <= 2), FALSE)))
}

# The groundwater gley colour pattern (talajvizglejes szinmintazat) in a row:
# reduced colours (the matrix colour stands for the 90 % of the text), or
# red mottling on 5 % or more, or an l or r suffix. Mottles not recorded
# read as none.
.hscs_gw_gley_rows <- function(x) {
  h <- x$h
  red <- .hscs_reduced_colour(x)
  mot <- h$redoximorphic_features_pct
  mott <- ifelse(is.na(mot), FALSE, mot >= 5)
  ifelse(x$des$l | x$des$r | mott, TRUE, red)
}

# Carbonate classes of the soil material (8.4): `min_pct` % CaCO3 or more.
# Without a measure: effervescence none is below 2 %; slight may be the
# trace class (audible only, below 2 %), so it is undecided at 2 % and below
# any higher limit; moderate or stronger is at least 2 %; a k suffix is
# weakly calcareous.
.hscs_calcareous_rows <- function(x, min_pct = 2) {
  h <- x$h; n <- x$n
  ca <- h$caco3_pct
  eff <- .hscs_txt(h$effervescence_class, n)
  by_eff <- if (min_pct <= 2) {
    ifelse(is.na(eff), NA, ifelse(eff %in% "none", FALSE, ifelse(eff %in% "slight", NA, TRUE)))
  } else {
    ifelse(is.na(eff), NA, ifelse(eff %in% c("none", "slight"), FALSE, NA))
  }
  ifelse(!is.na(ca), ca >= min_pct, ifelse(x$des$k & min_pct <= 2, TRUE, by_eff))
}

# Secondary carbonates seen in a row (pseudomycelia, coatings, soft masses,
# nodules): secondary_carbonates_pct or carbonate_soft_pct above 0, or a k
# suffix; recorded when met.
.hscs_secondary_carbonate_rows <- function(x) {
  h <- x$h; n <- x$n
  sc <- h$secondary_carbonates_pct %||% rep(NA_real_, n)
  cs <- h$carbonate_soft_pct %||% rep(NA_real_, n)
  (!is.na(sc) & sc > 0) | (!is.na(cs) & cs > 0) | x$des$k
}

# Base saturation 50 % or more in a row; a calcareous row reads as
# saturated (R20).
.hscs_bs50_rows <- function(x) {
  bs <- x$h$bs_pct
  ifelse(!is.na(bs), bs >= 50, ifelse(.hscs_calcareous_rows(x) %in% TRUE, TRUE, NA))
}

# ---- 8.1 surface horizons ---------------------------------------------------------

# Favourable structure (kedvezo szerkezet): granular, crumb or subangular
# blocky; not structureless, platy, prismatic, columnar or angular blocky.
.hscs_favourable_structure <- function(x) {
  h <- x$h; n <- x$n
  t <- .hscs_txt(h$structure_type, n); g <- .hscs_txt(h$structure_grade, n)
  ifelse(!is.na(g) & grepl("structureless|apedal", g), FALSE,
  ifelse(is.na(t), NA,
  ifelse(grepl("granul|crumb|subangular", t), TRUE, FALSE)))
}

# Dark colour of the humic horizon: moist value and chroma 3 or less, and
# (when recorded) dry value and chroma 5 or less.
.hscs_dark_rows <- function(x) {
  h <- x$h
  mv <- h$munsell_value_moist; mc <- h$munsell_chroma_moist
  dv <- h$munsell_value_dry; dc <- h$munsell_chroma_dry
  moist <- ifelse(is.na(mv) | is.na(mc), NA, mv <= 3 & mc <= 3)
  dry <- ifelse(is.na(dv) & is.na(dc), TRUE,
         ifelse(!is.na(dv) & dv > 5 | !is.na(dc) & dc > 5, FALSE, ifelse(is.na(dv) | is.na(dc), NA, TRUE)))
  .and3v(moist, dry)
}

# Rows of the humic horizon (humuszos talajszint) with organic carbon of
# `oc_min` % or more: favourable structure, dark, base saturation 50 % or
# more (`bs`: TRUE; FALSE for the umbric horizon, below 50 %).
.hscs_humic_rows <- function(x, oc_min = 0.6, bs = TRUE) {
  oc <- x$h$oc_pct
  ocok <- ifelse(is.na(oc), NA, oc >= oc_min)
  bsr <- .hscs_bs50_rows(x)
  bsok <- if (isTRUE(bs)) bsr else !bsr
  .and3v(.and3v(.and3v(.and3v(.hscs_favourable_structure(x), .hscs_dark_rows(x)), ocok), bsok), x$mineral3)
}

# A humic horizon at the mineral soil surface: a run of humic rows from it,
# 20 cm or more thick (a run that reaches the end of the description may go
# on below it).
.hscs_humic_horizon <- function(x, ok) {
  if (is.na(x$ms)) return(NA)
  .hscs_run(x, ok, top_max = x$ms + 0.5, min_th = 20)
}

# ---- 8.2 subsurface horizons -------------------------------------------------------

# Rows of a clay accumulation horizon (agyagfelhalmozodasi szint): a
# subsurface horizon with clay 8 % or more and loamy sand or finer; where the
# soil is not ploughed, truncated or of more than one material, 1.4 times the
# clay of the horizon above it (the horizon as a whole: a run of rows, each
# read against the clay of the horizon directly above the run's top);
# otherwise well-visible clay films (common or more; a t suffix when the
# films are not recorded). The thickness (10 cm) is the run's. A
# transitional AB, BA, EB or BE with no clay recorded is passed over.
.hscs_clay_accum_rows <- function(x) {
  h <- x$h; n <- x$n; r <- x$ord
  base <- .and3v(.and3v(.and3v(.hscs_clay_at_least(x, 8), .hscs_not_sand_rows(x)), x$mineral3), x$sub3)
  films <- .hscs_txt(h$clay_films_amount, n)
  film_ok <- ifelse(!is.na(films), grepl("common|many|continuous|abundant", films),
             ifelse(x$des$t, TRUE, NA))
  trunc <- .hscs_truthy(.hscs_site1(x, "anthropic_truncation")) %in% TRUE
  if (x$ploughed || trunc) return(.and3v(base, film_ok))
  cr <- .hscs_clay_range(x)
  lo <- ifelse(is.na(cr$lo), 0, cr$lo); hi <- ifelse(is.na(cr$hi), 100, cr$hi)
  ab <- .hscs_above(x)
  pre <- ifelse(nzchar(x$des$prefix), x$des$prefix, "1")
  first <- x$ord[1L]
  # the smallest reference clay of a run through each row: sure (from the
  # upper bounds of the references) and possible (from the lower bounds). A
  # lithologic change between a row and the horizon above it leaves the
  # ratio meaningless there: the clay films decide that row. A gap in the
  # description above a row may hold a horizon of any clay.
  # Hugo's reading of 2026-10-30: a transitional horizon between the surface
  # or eluvial horizon and the B (AB, BA, EB, BE) with no clay recorded
  # (neither measured nor a texture class) is passed over, and the row is
  # read against the horizon above it.
  skip <- x$role %in% c("AB", "BA", "EB", "BE") & is.na(cr$lo) & is.na(cr$hi)
  sref <- rep(NA_real_, n); pref <- rep(NA_real_, n); ratio <- rep(FALSE, n)
  for (i in r) {
    if (base[i] %in% FALSE) next
    j <- ab[i]
    while (!is.na(j) && skip[j]) j <- ab[j]
    if (is.na(j)) {
      if (!identical(i, first)) { ratio[i] <- NA; pref[i] <- 0 }
      next
    }
    if (pre[i] != pre[j]) { ratio[i] <- film_ok[i]; next }
    rs <- c(); rp <- c()
    if (x$mineral[j]) { rs <- c(rs, hi[j]); rp <- c(rp, lo[j]) }
    if (!is.na(sref[j])) rs <- c(rs, sref[j])
    if (!is.na(pref[j])) rp <- c(rp, pref[j])
    s_ok <- length(rs) > 0 && lo[i] >= 1.4 * min(rs)
    p_ok <- length(rp) > 0 && hi[i] >= 1.4 * min(rp)
    ratio[i] <- if (s_ok) TRUE else if (p_ok) NA else FALSE
    if (s_ok && base[i] %in% TRUE) sref[i] <- min(rs)
    if (p_ok) pref[i] <- min(rp)
  }
  .and3v(base, ratio)
}

# A clay accumulation horizon with its top within `zmax` cm of the surface:
# a run 10 cm or more thick; or lamellae (a designation naming them) of 15 cm
# in all.
.hscs_clay_accum <- function(x, zmax = 100) {
  ok <- .hscs_clay_accum_rows(x)
  v <- .hscs_run(x, ok, top_max = x$surface + zmax, min_th = 10)
  lam <- grepl("lamell", tolower(as.character(x$h$designation))) & (ok %in% TRUE)
  lam_th <- sum(x$thick[lam & x$h$top_cm < x$surface + zmax], na.rm = TRUE)
  if (lam_th >= 15) TRUE else v
}

# Rows of a swelling horizon (duzzado talajszint), a subsurface horizon: 30 %
# clay or more and slickensides common or more (Hugo's decision: >= 10 %),
# or wedge-shaped peds, or an i suffix (FAO slickensides).
.hscs_swelling_rows <- function(x) {
  h <- x$h; n <- x$n
  sl <- .hscs_txt(h$slickensides, n)
  t <- .hscs_txt(h$structure_type, n)
  slick <- ifelse(!is.na(sl), sl %in% c("common", "many", "continuous"), NA)
  # a structure not described may be wedge-shaped
  wedge <- ifelse(is.na(t), NA, grepl("wedge", t))
  feat <- ifelse(x$des$i, TRUE, .any3v(wedge, slick))
  .and3v(.and3v(.and3v(.hscs_clay_at_least(x, 30), feat), x$mineral3), x$sub3)
}

# Rows of a cambic horizon: in a subsurface horizon (a B, or a transition
# with B; or a row with no designation below the uppermost), signs of soil
# formation: against the parent material (the first C horizon below it in
# the same material), a more developed structure or a moist colour one or
# more Munsell units different (value, chroma, or one hue page); against
# the horizon directly below, a different carbonate content (2 percentage
# points of CaCO3 or more, so that measurement noise does not count); or a
# w suffix (the surveyor's mark of developed colour or structure).
.hscs_cambic_rows <- function(x) {
  h <- x$h; n <- x$n; r <- x$ord
  role <- x$role
  cand <- (grepl("B", role) | (is.na(role) & !(x$sub3 %in% FALSE))) & x$mineral & !x$des$buried
  g <- .hscs_txt(h$structure_grade, n); t <- .hscs_txt(h$structure_type, n)
  t0 <- ifelse(is.na(t), "", t)
  ped <- ifelse(!is.na(g) & grepl("weak|moderate|strong", g) & !grepl("massive|single", t0), TRUE,
         ifelse(!is.na(g) & grepl("structureless", g) | !is.na(t) & grepl("massive|single", t), FALSE, NA))
  grade_n <- ifelse(is.na(g), NA, ifelse(grepl("strong", g), 3, ifelse(grepl("moderate", g), 2,
             ifelse(grepl("weak", g), 1, ifelse(grepl("structureless", g), 0, NA)))))
  hue_n <- .nzsc_hue_num(h$munsell_hue_moist)
  ca <- h$caco3_pct
  bel <- .hscs_below(x)
  out <- rep(FALSE, n)
  for (k in seq_along(r)) {
    i <- r[k]
    if (!cand[i]) next
    if (x$des$w[i]) { out[i] <- TRUE; next }
    below <- r[seq_along(r) > k]
    below <- below[x$role[below] %in% c("C") & x$des$prefix[below] == x$des$prefix[i]]
    j <- if (length(below)) below[1L] else NA_integer_
    struct <- if (is.na(j) || is.na(ped[i]) || is.na(ped[j])) NA else (ped[i] && !ped[j]) ||
      (!is.na(grade_n[i]) && !is.na(grade_n[j]) && grade_n[i] > grade_n[j])
    colour <- if (is.na(j)) NA else {
      d <- c(abs(h$munsell_value_moist[i] - h$munsell_value_moist[j]),
             abs(h$munsell_chroma_moist[i] - h$munsell_chroma_moist[j]),
             abs(hue_n[i] - hue_n[j]) / 2.5)
      if (any(d >= 1, na.rm = TRUE)) TRUE else if (anyNA(d)) NA else FALSE
    }
    b <- bel[i]
    carb <- if (is.na(b) || is.na(ca[i]) || is.na(ca[b])) NA else abs(ca[i] - ca[b]) >= 2
    v <- .any3(c(struct, colour, carb))
    # a row with no designation may also be a C (or an A): at most undecided
    out[i] <- if (is.na(role[i]) && isTRUE(v)) NA else v
  }
  out
}

# Rows of a solonetz horizon (szolonyeces szint): a subsurface horizon with
# exchangeable Na 15 % or more of the CEC, or Na + Mg 25 % or more (Hugo's
# decision: shares of the CEC); and a columnar structure, or, where the soil
# is ploughed, a coarse granular one, very hard when dry.
.hscs_solonetz_rows <- function(x) {
  h <- x$h; n <- x$n
  na <- h$na_cmol; mg <- h$mg_cmol; cec <- h$cec_cmol
  esp <- ifelse(!is.na(na) & !is.na(cec) & cec > 0, 100 * na / cec, h$esp_pct %||% rep(NA_real_, n))
  namg <- ifelse(!is.na(na) & !is.na(mg) & !is.na(cec) & cec > 0, 100 * (na + mg) / cec, NA)
  chem <- .any3v(ifelse(is.na(esp), NA, esp >= 15), ifelse(is.na(namg), NA, namg >= 25))
  t <- .hscs_txt(h$structure_type, n); sz <- .hscs_txt(h$structure_size, n)
  dry <- .hscs_txt(h$strength_dry %||% h$rupture_resistance, n)
  col <- ifelse(is.na(t), NA, grepl("column", t))
  plough_ok <- ifelse(is.na(t), NA, grepl("column", t) |
                 (grepl("granul", t) & grepl("coarse|thick", ifelse(is.na(sz), "", sz)) &
                  grepl("very hard|extremely hard|rigid", ifelse(is.na(dry), "", dry))))
  struct <- if (x$ploughed) plough_ok else col
  .and3v(.and3v(.and3v(chem, struct), x$mineral3), x$sub3)
}

# Rows of a solonchak horizon (szoloncsakos szint): total water-soluble salts
# 0.15 % or more (soluble_salts_pct; Hugo's decision: the EC is not
# converted, so a row without it is undecided). The thickness (15 cm) is the
# run's.
.hscs_solonchak_rows <- function(x) {
  s <- x$h$soluble_salts_pct %||% rep(NA_real_, x$n)
  .and3v(ifelse(is.na(s), NA, s >= 0.15), x$mineral3)
}

# ---- 8.4 materials ---------------------------------------------------------------

# Artefacts (human-made material) as a weighted average, % by volume, from
# the surface to 100 cm or to shallower hard rock. Artefacts are recorded
# when met: a row without a record, and the part below the description,
# read as none.
.hscs_artefacts_wmean <- function(x) {
  a <- x$h$artefacts_pct %||% rep(NA_real_, x$n)
  zt <- x$surface
  zb <- min(c(x$surface + 100, .hscs_rock_top(x)), na.rm = TRUE)
  if (is.na(zt) || !is.finite(zb) || zb <= zt) return(NA_real_)
  a0 <- ifelse(is.na(a), 0, a)
  r <- .hscs_rows_in(x, zt, zb); r <- r[!x$rock[r]]
  ov <- .asc_overlap(x$h$top_cm[r], ifelse(is.na(x$h$bottom_cm[r]), zb, x$h$bottom_cm[r]), zt, zb)
  sum(ov * a0[r]) / (zb - zt)
}

# Rows of anthropogenic material: a layer origin naming fill or other
# human-made or human-carried material, or a u suffix (FAO: artefacts).
# v0.9.316: also the hortic route of the anthropogenic horizon (8.1, p. 24,
# criterion 1): where the upper 20 cm hold P2O5 (AL method) of 500 mg/kg or
# more on a thickness-weighted average (p2o5_al_mg_kg), the run of A rows from
# the surface, not buried, is that horizon.
.hscs_anthropic_rows <- function(x) {
  lo <- .hscs_txt(x$h$layer_origin, x$n)
  out <- (!is.na(lo) & grepl("fill|terric|anthrop|artefact|artifact|technic|urban|spoil|dredg|garden|hortic|made|refuse|waste|rubble",
                             lo)) | x$des$u
  p <- x$h$p2o5_al_mg_kg
  if (!is.null(p) && any(!is.na(p)) && !is.na(x$surface)) {
    w <- .hscs_wmean(x, p, x$surface, x$surface + 20, vmin = 0, vmax = Inf)
    if (is.finite(w[["lo"]]) && w[["lo"]] >= 500) {
      prev <- NA
      for (i in x$ord) {
        if (!(x$role[i] %in% "A") || x$des$buried[i]) break
        if (!is.na(prev) && abs(x$h$top_cm[i] - prev) > 0.5) break
        out[i] <- TRUE
        prev <- x$h$bottom_cm[i]
        if (is.na(prev)) break
      }
    }
  }
  out
}

# Stratified material (retegzett talajanyag) within the window [zt, zb]:
# adjacent mineral layers that differ by 5 points or more of coarse
# fragments or by 10 points or more of sand (Hugo's decision: USDA sand,
# 2-0.05 mm), or an organic carbon that rises with depth (by 0.2 points or
# more); or organic carbon of 0.2 % or more in the horizon at 100 cm. A gap
# in the description above a layer leaves its comparison open; a profile
# that ends on rock above 100 cm has no horizon at 100 cm.
.hscs_stratified <- function(x, zt, zb) {
  h <- x$h
  r <- .hscs_rows_in(x, zt, zb); r <- r[x$mineral[r]]
  ab <- .hscs_above(x)
  first <- x$ord[1L]
  cf <- h$coarse_fragments_pct; sa <- h$sand_pct; oc <- h$oc_pct
  pair <- vapply(r, function(i) {
    if (h$top_cm[i] >= zb - 0.5) return(FALSE)
    j <- ab[i]
    if (is.na(j)) return(if (identical(i, first)) FALSE else NA)
    if (!x$mineral[j]) return(FALSE)
    v <- c(if (is.na(cf[i]) || is.na(cf[j])) NA else abs(cf[i] - cf[j]) >= 5,
           if (is.na(sa[i]) || is.na(sa[j])) NA else abs(sa[i] - sa[j]) >= 10,
           if (is.na(oc[i]) || is.na(oc[j])) NA else oc[i] - oc[j] >= 0.2 - 1e-9)
    .any3(v)
  }, NA)
  # a part of the window no row covers (a gap, or the soil below a
  # description that ends above it) may hold a layer that differs
  allr <- .hscs_rows_in(x, zt, zb)
  cov <- sum(.asc_overlap(h$top_cm[allr], ifelse(is.na(h$bottom_cm[allr]), zb, h$bottom_cm[allr]), zt, zb))
  open_part <- if (cov < zb - zt - 0.5) NA else FALSE
  rt <- .hscs_rock_top(x)
  deep <- if (!is.na(rt) && rt <= x$surface + 100) FALSE else {
    # the horizon at 100 cm (one ending at 100 cm included, one starting there not)
    at100 <- .hscs_rows_in(x, x$surface + 99, x$surface + 100.25)
    at100 <- at100[x$mineral[at100]]
    if (length(at100)) .any3(ifelse(is.na(oc[at100]), NA, oc[at100] >= 0.2)) else NA
  }
  .any3(c(pair, deep, open_part))
}
