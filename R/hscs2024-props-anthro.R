# =============================================================================
# v0.9.316 -- HSCS 2024 (Micheli et al. 2024, second approximation): the
# subtype and variety properties of human influence, tillage, layering,
# burial, coarse fragments and texture (8.5, p. 29-33; 8.6, p. 34-36), read
# by the frame in R/hscs2024-properties.R. Each .hscs_p_<id>(x) takes
# x = .hscs_horizons(pedon) and returns .hscs_prop().
#
# Where the text leaves a choice, the reading taken is written beside the
# code. Field observations (a plough pan, a compacted layer, clods, a
# pulverised surface, added material, disturbed horizons) are read from the
# FAO (2006) designations (p ploughing, d dense root-restricting layer, x
# fragipan, b buried, the lithologic prefix), structure, consistence, bulk
# density and penetration resistance; an observation not recorded leaves the
# property undecided, as Hugo decided on 2026-10-30.
# =============================================================================

# Set while the buried part of a profile is keyed (eltemetett), so that a
# buried soil is never asked for its own buried soil.
.hscs_state <- new.env(parent = emptyenv())
.hscs_state$buried <- FALSE

# The Hungarian names of the texture classes of the triangle (p. 36), by the
# USDA class names .hscs_texture_class() gives.
.HSCS_TEX_HU <- c(
  "sand" = "homok", "loamy sand" = "v\u00e1lyogos homok", "sandy loam" = "homokos v\u00e1lyog",
  "loam" = "v\u00e1lyog", "silt loam" = "iszapos v\u00e1lyog", "silt" = "iszap",
  "sandy clay loam" = "homokos agyagos v\u00e1lyog", "clay loam" = "agyagos v\u00e1lyog",
  "silty clay loam" = "iszapos agyagos v\u00e1lyog", "sandy clay" = "homokos agyag",
  "silty clay" = "iszapos agyag", "clay" = "agyag")

# Bulk densities (g/cm3) that may affect root growth and that restrict it,
# by texture class (USDA NRCS, Soil Quality Indicators: Bulk Density, table
# 2). "clay" here is the 35-45 % clay row; a clay above 45 % takes the lower
# values (below).
.HSCS_BD_LIMIT <- list(
  affect = c("sand" = 1.69, "loamy sand" = 1.69, "sandy loam" = 1.63, "loam" = 1.63,
             "sandy clay loam" = 1.60, "clay loam" = 1.60, "silt" = 1.60, "silt loam" = 1.60,
             "silty clay loam" = 1.55, "sandy clay" = 1.49, "silty clay" = 1.49, "clay" = 1.49),
  restrict = c("sand" = 1.80, "loamy sand" = 1.80, "sandy loam" = 1.80, "loam" = 1.80,
               "sandy clay loam" = 1.75, "clay loam" = 1.75, "silt" = 1.75, "silt loam" = 1.75,
               "silty clay loam" = 1.65, "sandy clay" = 1.58, "silty clay" = 1.58, "clay" = 1.58))

# Penetration resistance (kPa, moist) read as compaction: 2000 kPa, where root
# growth becomes restricted (the plough pan); 3000 kPa, strongly compacted
# (the tomodott layer, through which rooting is hindered).
.HSCS_PEN_LIMIT <- c(affect = 2000, restrict = 3000)

# Layer origins (layer_origin, rock_origin, the site's parent material).
.HSCS_COLLUVIAL <- "colluv|solimov|deluv|slope|hillwash|lejt\u0151"
.HSCS_FLUVIAL <- "fluv|alluv|lacustr|limn|\u00f6nt\u00e9s"
# Material carried in by people (terrikus): fill, dredged or spoil material,
# topsoil brought in; "urban fill" counts, "urban" or "garden" alone does not.
.HSCS_ADDED <- "terric|fill|dredg|spoil|carried|added|import|made|raised|dump|tipped|odahord|felt\u00f6lt"
# A land use that names tillage (as .hscs_horizons() reads a ploughed soil).
.HSCS_TILLED_USE <- "crop|arable|plough|plow|tilled|tillage|cultivat"
# A surface condition naming an artificial sealing material ("seal" alone is
# not read: a natural crust is also called a seal).
.HSCS_SEALED_SURFACE <- "asphalt|tarmac|concrete|paved|pavement|beton|aszfalt|geomembrane|plastic sheet|foil"

# ---- helpers -----------------------------------------------------------------------

.hscs_num_site <- function(x, nm) suppressWarnings(as.numeric(.hscs_site1(x, nm)))

.hscs_site_text <- function(x, nm) {
  v <- x$site[[nm]]
  if (is.null(v) || !length(v)) return(NA_character_)
  t <- tolower(trimws(paste(as.character(unlist(v)), collapse = " ")))
  if (is.na(t) || !nzchar(t) || t %in% c("na", "nan")) NA_character_ else t
}

# The first soil row (not organic, not rock): the mineral soil surface.
.hscs_first_mineral <- function(x) {
  r <- x$ord[!(x$organic[x$ord] %in% TRUE) & !x$rock[x$ord]]
  if (length(r)) r[1L] else NA_integer_
}

# A land use naming tillage: TRUE, FALSE when a land use is recorded that
# does not, NA when none is recorded.
.hscs_tilled_use <- function(x) {
  t <- .hscs_site_text(x, "land_use")
  if (is.na(t)) NA else grepl(.HSCS_TILLED_USE, t)
}

# The ploughed horizons: the run of p horizons (not buried) from the soil
# surface, or from the mineral soil surface under an organic layer. Returns
# the rows, their base and whether the run reaches the end of a description
# that does not end on rock (the ploughing may go deeper than described).
.hscs_plough_run <- function(x) {
  h <- x$h; rows <- integer(0); prev <- NA_real_; started <- FALSE
  for (i in x$ord) {
    t <- h$top_cm[i]
    if (!is.na(prev) && abs(t - prev) > 0.5) break
    p <- x$des$p[i] && !x$des$buried[i]
    if (!started && !p && x$organic[i] %in% TRUE) { prev <- h$bottom_cm[i]; if (is.na(prev)) break; next }
    if (!p) break
    started <- TRUE
    rows <- c(rows, i); prev <- h$bottom_cm[i]
    if (is.na(prev)) break
  }
  base <- if (length(rows)) h$bottom_cm[rows[length(rows)]] else NA_real_
  open <- length(rows) > 0L && (is.na(base) || (identical(rows[length(rows)], x$last) && !x$ends_on_rock))
  list(rows = rows, base = base, open = open)
}

# Moist Munsell colours one unit or more apart (value, chroma, or one hue
# page), as the cambic horizon reads them.
.hscs_colour_differs <- function(x, i, j) {
  h <- x$h
  hue <- .nzsc_hue_num(h$munsell_hue_moist[c(i, j)])
  d <- c(abs(h$munsell_value_moist[i] - h$munsell_value_moist[j]),
         abs(h$munsell_chroma_moist[i] - h$munsell_chroma_moist[j]),
         abs(hue[1] - hue[2]) / 2.5)
  if (any(d >= 1, na.rm = TRUE)) TRUE else if (anyNA(d)) NA else FALSE
}

# A different composition: another lithologic prefix, another texture class,
# or 2 points of CaCO3 or more (as the cambic horizon reads carbonate).
.hscs_composition_differs <- function(x, i, j) {
  h <- x$h
  pre <- if (x$des$known[i] && x$des$known[j]) x$des$prefix[i] != x$des$prefix[j] else NA
  tex <- if (is.na(x$tex[i]) || is.na(x$tex[j])) NA else x$tex[i] != x$tex[j]
  ca <- h$caco3_pct
  carb <- if (is.na(ca[i]) || is.na(ca[j])) NA else abs(ca[i] - ca[j]) >= 2
  .any3(c(pre, tex, carb))
}

# Maximal runs of adjacent rows (depth order) where `keep` is TRUE; a gap in
# the description ends a run.
.hscs_segments <- function(x, keep) {
  h <- x$h; out <- list(); cur <- integer(0); prev <- NA_real_
  for (i in x$ord) {
    if (length(cur) && (is.na(prev) || abs(h$top_cm[i] - prev) > 0.5)) {
      out[[length(out) + 1L]] <- cur; cur <- integer(0)
    }
    if (isTRUE(keep[i])) cur <- c(cur, i)
    else if (length(cur)) { out[[length(out) + 1L]] <- cur; cur <- integer(0) }
    prev <- h$bottom_cm[i]
  }
  if (length(cur)) out[[length(out) + 1L]] <- cur
  out
}

# The description has a gap, or ends (not on rock) above the depth `zb`:
# that part may hold anything.
.hscs_open_above <- function(x, zb) {
  h <- x$h; r <- x$ord; prev <- NA_real_
  for (i in r) {
    if (h$top_cm[i] >= zb - 0.25) break
    if (!is.na(prev) && h$top_cm[i] > prev + 0.5) return(TRUE)
    prev <- h$bottom_cm[i]
    if (is.na(prev)) return(TRUE)
  }
  deepest <- suppressWarnings(max(h$bottom_cm, na.rm = TRUE))
  !x$ends_on_rock && (!is.finite(deepest) || deepest < zb - 0.25)
}

# ---- 8.5 subtypes of the Antropogen talajok (p. 29-33) ----------------------------

# mutermekes (p. 31): "legalabb 20 terfogat % (sulyozott atlag)" artefacts
# within 100 cm, as the key reads them (recorded when met).
.hscs_p_mutermekes <- function(x) {
  if (is.na(x$surface)) return(.hscs_prop(NA))
  a <- .hscs_artefacts_wmean(x)
  if (is.na(a)) return(.hscs_prop(NA, missing = "artefacts_pct"))
  .hscs_prop(a >= 20, layers = which((x$h$artefacts_pct %||% rep(NA_real_, x$n)) > 0),
             note = sprintf("artefacts %.1f %% by volume, weighted to 100 cm", a))
}

# fedett (p. 29): at the soil surface, a continuous artificial material at
# least 1 mm thick that water does not pass, or passes very slowly
# ("beton, aszfalt, folia"). Read: technic hard material of 95 % or more in a
# row at the surface, a site geomembrane at 0 cm, or a surface condition
# naming asphalt, concrete, paving or a sheet. A geomembrane in the uppermost
# row, or a site geomembrane of unknown depth, may lie at the surface. As the
# key reads it, an artificial material is recorded when met. The 1 mm is not
# recorded and is read as met.
.hscs_p_fedett <- function(x) {
  h <- x$h; n <- x$n
  if (is.na(x$surface)) return(.hscs_prop(NA))
  r0 <- x$ord[abs(h$top_cm[x$ord] - x$surface) <= 0.5]
  thm <- h$technic_hardmaterial_pct %||% rep(NA_real_, n)
  gm <- h$geomembrane_present %||% rep(NA, n)
  sgm <- .hscs_truthy(.hscs_site1(x, "geomembrane"))
  sgd <- .hscs_num_site(x, "geomembrane_depth_cm")
  sc <- .hscs_site_text(x, "surface_condition")
  if (any(!is.na(thm[r0]) & thm[r0] >= 95) || (isTRUE(sgm) && !is.na(sgd) && sgd <= 0.5) ||
      (!is.na(sc) && grepl(.HSCS_SEALED_SURFACE, sc)))
    return(.hscs_prop(TRUE, layers = r0))
  if (any(gm[r0] %in% TRUE) || (isTRUE(sgm) && is.na(sgd)))
    return(.hscs_prop(NA, missing = "geomembrane_depth_cm",
                      note = "a geomembrane in the uppermost horizon or of unknown depth may lie at the surface"))
  .hscs_prop(FALSE)
}

# vizzaro (p. 33): the same material within 100 cm, as the key reads its
# criterion 2b: geomembrane_present in a row within 100 cm, technic hard
# material of 95 % or more, the site geomembrane above 100 cm or of unknown
# depth (no depth modifier then), or a sealed surface. The layer starts at the
# top of the row that holds it.
.hscs_p_vizzaro <- function(x) {
  h <- x$h; n <- x$n; s <- x$surface
  if (is.na(s)) return(.hscs_prop(NA))
  r <- .hscs_rows_in(x, s, s + 100)
  thm <- h$technic_hardmaterial_pct %||% rep(NA_real_, n)
  gm <- h$geomembrane_present %||% rep(NA, n)
  hit <- r[gm[r] %in% TRUE | (!is.na(thm[r]) & thm[r] >= 95)]
  sc <- .hscs_site_text(x, "surface_condition")
  sgm <- .hscs_truthy(.hscs_site1(x, "geomembrane"))
  sgd <- .hscs_num_site(x, "geomembrane_depth_cm")
  tops <- c(if (length(hit)) h$top_cm[hit[1L]],
            if (!is.na(sc) && grepl(.HSCS_SEALED_SURFACE, sc)) s,
            if (isTRUE(sgm) && !is.na(sgd) && sgd < 100) s + sgd)
  if (length(tops)) return(.hscs_prop(TRUE, top = min(tops), layers = hit))
  if (isTRUE(sgm) && is.na(sgd))
    return(.hscs_prop(TRUE, note = "site geomembrane of unknown depth (read within 100 cm, as the key reads it)"))
  .hscs_prop(FALSE)
}

# hortikus (p. 29; 8.1, p. 24): the P2O5 by the AL method of the upper 20 cm
# ("a felszini 20 cm") 500 mg/kg or more, as a thickness-weighted mean of
# p2o5_al_mg_kg over 0-20 cm from the soil surface.
.hscs_p_hortikus <- function(x) {
  s <- x$surface
  if (is.na(s)) return(.hscs_prop(NA))
  v <- x$h$p2o5_al_mg_kg %||% rep(NA_real_, x$n)
  w <- .hscs_wmean(x, v, s, s + 20, vmin = 0, vmax = 1e9)
  if (w[["lo"]] >= 500) return(.hscs_prop(TRUE, note = sprintf("AL-P2O5 %.0f mg/kg in 0-20 cm", w[["lo"]])))
  if (w[["hi"]] < 500) return(.hscs_prop(FALSE))
  .hscs_prop(NA, missing = "p2o5_al_mg_kg")
}

# Rows of material people carried in: a layer origin naming fill, dredged or
# spoil material or the like, or a designation with the USDA "^" (human-
# transported material). FALSE where another origin is recorded, NA where
# none is.
.hscs_added_rows <- function(x) {
  lo <- .hscs_txt(x$h$layer_origin, x$n)
  d <- trimws(as.character(x$h$designation %||% rep(NA_character_, x$n)))
  hit <- (!is.na(lo) & grepl(.HSCS_ADDED, lo)) | (!is.na(d) & startsWith(d, "^"))
  ifelse(hit, TRUE, ifelse(!is.na(lo), FALSE, NA))
}

# terrikus (p. 33; 8.1, p. 24): a surface raised by 20 cm or more of added
# material whose colour and composition differ from the original soil
# surface. Read: a run of added rows from the mineral soil surface, 20 cm or
# more thick; its uppermost row (the raised surface) against the row directly
# below the run (the original surface): moist colour one Munsell unit or more
# apart AND a different composition (lithologic prefix, texture class, or 2
# points of CaCO3). With no row below the run the original surface is not
# described: undecided.
.hscs_p_terrikus <- function(x) {
  h <- x$h
  if (is.na(x$surface)) return(.hscs_prop(NA))
  f <- .hscs_first_mineral(x)
  if (is.na(f)) return(.hscs_prop(FALSE))
  ad <- .and3v(.hscs_added_rows(x), x$mineral3)
  miss <- c("layer_origin", "munsell_value_moist", "munsell_chroma_moist", "munsell_hue_moist")
  k0 <- match(f, x$ord)
  sure <- integer(0); poss_th <- 0; prev <- h$top_cm[f]; sure_on <- TRUE
  for (i in x$ord[k0:length(x$ord)]) {
    if (abs(h$top_cm[i] - prev) > 0.5) { poss_th <- Inf; break }
    if (ad[i] %in% FALSE) break
    th <- x$thick[i]
    if (sure_on && ad[i] %in% TRUE) sure <- c(sure, i) else sure_on <- FALSE
    poss_th <- poss_th + (if (is.na(th)) Inf else th)
    prev <- h$bottom_cm[i]
    if (is.na(prev)) break
  }
  sure_th <- sum(x$thick[sure], na.rm = TRUE)
  if (sure_th >= 20 - 1e-9) {
    last <- sure[length(sure)]
    b <- .hscs_below(x)[last]
    # the first row below the added material that is not marked as added is
    # read as the original surface (rock is no soil surface)
    if (!is.na(b) && x$rock[b]) b <- NA_integer_
    if (is.na(b)) return(.hscs_prop(NA, missing = miss, layers = sure,
                                    note = "the original soil surface below the added material is not described"))
    v <- .and3(c(.hscs_colour_differs(x, f, b), .hscs_composition_differs(x, f, b)))
    return(.hscs_prop(v, layers = sure, missing = if (is.na(v)) miss else character(0)))
  }
  if (poss_th >= 20 - 1e-9) return(.hscs_prop(NA, missing = miss))
  .hscs_prop(FALSE)
}

# bolygatott (p. 29): within 100 cm the original sequence of horizons
# changed by human activity. Read from the site: mechanical_mixing_depth_cm
# above 0 (people mechanically disturbed the original horizons: mixing, deep
# ripping, subsoil lifting) or a mechanical_mixing_purpose (recorded when
# met); a mixing depth of 0 is no; otherwise undecided. Truncation alone
# (anthropic_truncation) removes horizons without reordering them: not read.
.hscs_p_bolygatott <- function(x) {
  mix <- .hscs_num_site(x, "mechanical_mixing_depth_cm")
  pur <- .hscs_site_text(x, "mechanical_mixing_purpose")
  if ((!is.na(mix) && mix > 0) || !is.na(pur)) return(.hscs_prop(TRUE))
  if (!is.na(mix)) return(.hscs_prop(FALSE))
  .hscs_prop(NA, missing = "mechanical_mixing_depth_cm")
}

# szennyezett (p. 31): a layer within 50 cm above a contamination or toxicity
# limit of the regulation in force: the site attribute contaminated. It
# carries no depth, so no depth modifier is given.
.hscs_p_szennyezett <- function(x) {
  v <- .hscs_truthy(.hscs_site1(x, "contaminated"))
  .hscs_prop(v, missing = if (is.na(v)) "contaminated" else character(0))
}

# ---- 8.6 tillage and the soil surface (p. 34-35) ----------------------------------

# muvelt (p. 35): "legalabb 10 cm-es melysegig talajmuvelo eszkozzel
# bolygatott". Read: the ploughed horizons (p, not buried) from the surface
# reach 10 cm, or the site mixing depth is 10 cm or more; with no p horizon,
# a land use naming tillage (arable land is read as tilled to the usual
# plough depth). No: p horizons that end above 10 cm, or a designated surface
# horizon without p on a land use that does not name tillage.
.hscs_p_muvelt <- function(x) {
  s <- x$surface
  if (is.na(s)) return(.hscs_prop(NA))
  pr <- .hscs_plough_run(x)
  mix <- .hscs_num_site(x, "mechanical_mixing_depth_cm")
  if (!is.na(mix) && mix >= 10) return(.hscs_prop(TRUE))
  if (length(pr$rows)) {
    if (!is.na(pr$base) && pr$base >= s + 10 - 1e-9) return(.hscs_prop(TRUE, layers = pr$rows))
    return(.hscs_prop(if (pr$open) NA else FALSE, layers = pr$rows))
  }
  lu <- .hscs_tilled_use(x)
  if (isTRUE(lu)) return(.hscs_prop(TRUE, note = "land use names tillage"))
  f <- .hscs_first_mineral(x)
  if (!is.na(f) && x$des$known[f]) return(.hscs_prop(FALSE))
  .hscs_prop(NA, missing = c("designation", "land_use"))
}

# rigolozott (p. 35): ploughed or turned to 50 cm or more, as the key's
# criterion 1 reads it (the base of the p horizons from the surface, or the
# site mixing depth). No: p horizons that end above 50 cm, or a designated
# surface horizon without p.
.hscs_p_rigolozott <- function(x) {
  s <- x$surface
  if (is.na(s)) return(.hscs_prop(NA))
  pr <- .hscs_plough_run(x)
  mix <- .hscs_num_site(x, "mechanical_mixing_depth_cm")
  if ((!is.na(pr$base) && pr$base >= s + 50 - 1e-9) || (!is.na(mix) && mix >= 50))
    return(.hscs_prop(TRUE, layers = pr$rows))
  if (length(pr$rows)) return(.hscs_prop(if (pr$open) NA else FALSE, layers = pr$rows))
  f <- .hscs_first_mineral(x)
  if (!is.na(f) && x$des$known[f]) return(.hscs_prop(FALSE))
  .hscs_prop(NA, missing = c("designation", "mechanical_mixing_depth_cm"))
}

# teraszolt (p. 35): an artificial terraced surface: the site attribute
# terraced; else a land use naming terraces.
.hscs_p_teraszolt <- function(x) {
  v <- .hscs_truthy(.hscs_site1(x, "terraced"))
  if (!is.na(v)) return(.hscs_prop(v))
  lu <- .hscs_site_text(x, "land_use")
  if (!is.na(lu) && grepl("terrac|terasz", lu)) return(.hscs_prop(TRUE, note = "land use names terraces"))
  .hscs_prop(NA, missing = "terraced")
}

# arasztott (p. 34): under flood-irrigated cultivation: the site attribute
# flood_irrigated; else a land use naming rice, paddies or flood irrigation.
.hscs_p_arasztott <- function(x) {
  v <- .hscs_truthy(.hscs_site1(x, "flood_irrigated"))
  if (!is.na(v)) return(.hscs_prop(v))
  lu <- .hscs_site_text(x, "land_use")
  if (!is.na(lu) && grepl("rice|paddy|flood irrig|flooded cultiv|rizs|\u00e1raszt", lu))
    return(.hscs_prop(TRUE, note = "land use names flood irrigation"))
  .hscs_prop(NA, missing = "flood_irrigated")
}

# Bulk density limit of each row (`which`: "affect" or "restrict"), by its
# texture class; a clay with more than 45 % clay, and a clay loam with more
# than 35 %, take the clayey rows of the table.
.hscs_bd_limit <- function(x, which) {
  tab <- .HSCS_BD_LIMIT[[which]]
  lim <- unname(tab[x$tex])
  cl <- x$h$clay_pct
  clay45 <- if (which == "restrict") 1.47 else 1.39
  lim <- ifelse(!is.na(cl) & cl > 45 & x$tex %in% "clay", clay45, lim)
  ifelse(!is.na(cl) & cl > 35 & x$tex %in% "clay loam", tab[["clay"]], lim)
}

# A d suffix (FAO 2006: a dense, physically root-restricting layer) or an x
# suffix (fragipan) on the designation.
.hscs_dense_rows <- function(x) vapply(x$des$strong, function(s) "d" %in% s, NA) | x$des$x

# Signs of compaction in each row (`which`: "affect", the plough pan;
# "restrict", the strongly compacted tomodott layer): a d suffix (FAO 2006:
# a dense, physically root-restricting layer) or an x suffix (fragipan) on
# the designation; a penetration resistance of 2000 (3000) kPa or more; a
# bulk density at (above) the limit that affects (restricts) root growth for
# its texture; a moist consistence very firm or extremely firm (extremely
# firm). TRUE where one shows; FALSE where one of these is recorded and none
# shows; NA where none is recorded.
.hscs_compact_rows <- function(x, which = c("affect", "restrict")) {
  which <- match.arg(which)
  h <- x$h; n <- x$n
  d <- .hscs_dense_rows(x)
  pen <- h$penetration_resistance_kpa %||% rep(NA_real_, n)
  bd <- h$bulk_density_g_cm3 %||% rep(NA_real_, n)
  lim <- .hscs_bd_limit(x, which)
  cm <- .hscs_txt(h$consistence_moist, n)
  pen_ok <- ifelse(is.na(pen), NA, pen >= .HSCS_PEN_LIMIT[[which]])
  bd_ok <- ifelse(is.na(bd) | is.na(lim), NA, if (which == "restrict") bd > lim else bd >= lim)
  cm_ok <- ifelse(is.na(cm), NA,
                  grepl(if (which == "restrict") "extremely firm" else "very firm|extremely firm", cm))
  any_t <- pen_ok %in% TRUE | bd_ok %in% TRUE | cm_ok %in% TRUE
  any_f <- pen_ok %in% FALSE | bd_ok %in% FALSE | cm_ok %in% FALSE
  ifelse(d | any_t, TRUE, ifelse(any_f, FALSE, NA))
}

# eketalpas (p. 34): a compacted, pore-poor layer of degraded structure at
# plough depth. Read: the row directly below the ploughed horizons (p, not
# buried, from the surface), and the lowest of two or more ploughed rows
# (an Ap2 compacted in the furrow), with a d or x suffix, or with a sign of
# compaction (.hscs_compact_rows, "affect") that the uppermost ploughed row
# does not show (a pan is denser than the tilled layer; a soil compacted
# from the surface down has none). A soil with no p horizon whose land use
# names tillage has a plough depth that is not recorded: undecided. No p
# horizon and no tillage: no plough pan.
.hscs_p_eketalpas <- function(x) {
  h <- x$h
  if (is.na(x$surface)) return(.hscs_prop(NA))
  miss <- c("bulk_density_g_cm3", "penetration_resistance_kpa", "consistence_moist", "designation")
  pr <- .hscs_plough_run(x)
  if (!length(pr$rows)) {
    mix <- .hscs_num_site(x, "mechanical_mixing_depth_cm")
    lu <- .hscs_tilled_use(x)
    f <- .hscs_first_mineral(x)
    if (isTRUE(lu) || (!is.na(mix) && mix > 0) || is.na(f) || !x$des$known[f])
      return(.hscs_prop(NA, missing = miss, note = "plough depth not recorded"))
    return(.hscs_prop(FALSE, note = "not ploughed"))
  }
  comp <- .and3v(.hscs_compact_rows(x, "affect"), x$mineral3)
  # against the tilled layer above it, unless the describer marked it
  comp <- ifelse(.hscs_dense_rows(x), comp, .and3v(comp, !comp[pr$rows[1L]]))
  last_p <- pr$rows[length(pr$rows)]
  b <- .hscs_below(x)[last_p]
  cand <- c(if (length(pr$rows) >= 2L) last_p, if (!is.na(b) && !x$rock[b]) b)
  hit <- cand[comp[cand] %in% TRUE]
  if (length(hit)) return(.hscs_prop(TRUE, top = h$top_cm[hit[1L]], layers = hit))
  # the description ends in the plough layer, or has a gap below it: a pan
  # may lie there
  v <- if (pr$open || is.na(b) || anyNA(comp[cand])) NA else FALSE
  .hscs_prop(v, missing = if (is.na(v)) miss else character(0))
}

# elporosodott (p. 34): a pulverised, structureless soil surface from
# intensive tillage. Read on the uppermost mineral row: tilled (a p suffix, or
# a land use naming tillage; a designated row without p on another land use
# is not) and structureless in a pulverised form: a structure type naming
# powder or dust, or single grain in a row that is not sand or loamy sand (a
# sand is single-grained by nature). Massive (coherent) or a pedal structure
# is not.
.hscs_p_elporosodott <- function(x) {
  if (is.na(x$surface)) return(.hscs_prop(NA))
  f <- .hscs_first_mineral(x)
  if (is.na(f)) return(.hscs_prop(FALSE))
  h <- x$h; n <- x$n
  lu <- .hscs_tilled_use(x)
  tilled <- if (x$des$p[f] && !x$des$buried[f]) TRUE else if (isTRUE(lu)) TRUE
            else if (x$des$known[f]) FALSE else NA
  t <- .hscs_txt(h$structure_type, n)[f]; g <- .hscs_txt(h$structure_grade, n)[f]
  sandy <- x$tex[f] %in% c("sand", "loamy sand")
  st <- if (!is.na(t) && grepl("powder|dust|pulveri", t)) TRUE
        else if (!is.na(t) && grepl("single", t)) (if (sandy) FALSE else if (is.na(x$tex[f])) NA else TRUE)
        else if (!is.na(t)) FALSE
        else if (!is.na(g) && grepl("weak|moderate|strong", g)) FALSE
        else NA
  v <- .and3(c(tilled, st))
  .hscs_prop(v, layers = f, missing = if (is.na(v)) c("structure_type", "designation", "land_use") else character(0))
}

# hantos (p. 34): the upper 20 cm or more of hard or very hard, very coarse
# (> 30 cm) clods with no secondary structure. Read: a run from the mineral
# soil surface, 20 cm or more thick, of rows whose structure type names clods
# (or "hant"), of size very coarse or extremely coarse (the class names; the
# FAO classes begin below 30 cm), hard, very hard or extremely hard when dry
# (rupture_resistance; strength_dry strong, very strong or rigid), with no
# secondary structure recorded.
.hscs_p_hantos <- function(x) {
  if (is.na(x$surface)) return(.hscs_prop(NA))
  f <- .hscs_first_mineral(x)
  if (is.na(f)) return(.hscs_prop(FALSE))
  h <- x$h; n <- x$n
  t <- .hscs_txt(h$structure_type, n); sz <- .hscs_txt(h$structure_size, n)
  rr <- .hscs_txt(h$rupture_resistance, n); sd <- .hscs_txt(h$strength_dry, n)
  sec <- .hscs_txt(h$structure_type_secondary, n)
  clod <- ifelse(is.na(t), NA, grepl("clod|hant", t))
  big <- ifelse(is.na(sz), NA, grepl("very coarse|extremely coarse|very thick|extremely thick", sz))
  hard <- ifelse(!is.na(rr), grepl("hard", rr) & !grepl("slightly", rr),
          ifelse(!is.na(sd), grepl("strong|rigid", sd), NA))
  nosec <- is.na(sec) | sec %in% c("none", "no", "structureless")
  ok <- .and3v(.and3v(.and3v(.and3v(clod, big), hard), nosec), x$mineral3)
  v <- .hscs_run(x, ok, top_max = h$top_cm[f] + 0.5, min_th = 20)
  .hscs_prop(v, layers = which(ok %in% TRUE),
             missing = if (is.na(v)) c("structure_type", "structure_size", "rupture_resistance") else character(0))
}

# tomodott (p. 35): within 100 cm a strongly compacted layer through which
# rooting is hindered, or only along the faces of the peds. Read: a row with
# its top within 100 cm showing strong compaction (.hscs_compact_rows,
# "restrict"); a d or x suffix also passes (a fragipan admits roots only
# along its faces).
.hscs_p_tomodott <- function(x) {
  s <- x$surface
  if (is.na(s)) return(.hscs_prop(NA))
  ok <- .and3v(.hscs_compact_rows(x, "restrict"), x$mineral3)
  v <- .hscs_run(x, ok, top_max = s + 100, min_th = 0)
  top <- if (isTRUE(v)) .hscs_run_top(x, ok, top_max = s + 100, min_th = 0) else NA_real_
  .hscs_prop(v, top = top, layers = which(ok %in% TRUE),
             missing = if (is.na(v)) c("bulk_density_g_cm3", "penetration_resistance_kpa", "consistence_moist",
                                       "designation") else character(0))
}

# digozott (p. 34): traces of digo earth in 5 % or more of the soil matrix
# within 50 cm: digo_pct of 5 or more in a row with its top within 50 cm.
.hscs_p_digozott <- function(x) {
  s <- x$surface
  if (is.na(s)) return(.hscs_prop(NA))
  dg <- x$h$digo_pct %||% rep(NA_real_, x$n)
  ok <- .and3v(ifelse(is.na(dg), NA, dg >= 5), x$mineral3)
  v <- .hscs_run(x, ok, top_max = s + 50, min_th = 0)
  top <- if (isTRUE(v)) .hscs_run_top(x, ok, top_max = s + 50, min_th = 0) else NA_real_
  .hscs_prop(v, top = top, layers = which(ok %in% TRUE), missing = if (is.na(v)) "digo_pct" else character(0))
}

# ---- 8.6 layering and burial ---------------------------------------------------------

# tobbretegu (p. 35): within 100 cm "kozettani kulonbseg, eltemetett talaj,
# talajszint ('folytonossagi hiany')": a lithic difference, a buried soil, or
# a discontinuity. Read: a row with its top within 100 cm (not the uppermost)
# that is buried (b), or whose lithologic prefix differs from the row
# directly above it, or whose recorded origin (layer_origin, rock_origin)
# differs from that row's. A gap above a row, a row or a neighbour with no
# designation, and the soil below a description that ends (not on rock)
# above 100 cm may hold one: undecided. The layer starts at the top of the
# first such row.
.hscs_p_tobbretegu <- function(x) {
  h <- x$h; s <- x$surface
  if (is.na(s)) return(.hscs_prop(NA))
  zb <- s + 100
  ab <- .hscs_above(x); first <- x$ord[1L]
  lo <- .hscs_txt(h$layer_origin, x$n); ro <- .hscs_txt(h$rock_origin, x$n)
  r <- x$ord[h$top_cm[x$ord] < zb - 0.25]
  ev <- vapply(r, function(i) {
    if (identical(i, first)) return(FALSE)
    if (x$des$buried[i]) return(TRUE)
    j <- ab[i]
    if (is.na(j)) return(NA)
    pre <- if (x$des$known[i] && x$des$known[j]) x$des$prefix[i] != x$des$prefix[j] else NA
    org <- (!is.na(lo[i]) && !is.na(lo[j]) && lo[i] != lo[j]) || (!is.na(ro[i]) && !is.na(ro[j]) && ro[i] != ro[j])
    if (org) TRUE else pre
  }, NA)
  hit <- r[ev %in% TRUE]
  if (length(hit)) return(.hscs_prop(TRUE, top = h$top_cm[hit[1L]], layers = hit))
  deepest <- suppressWarnings(max(h$bottom_cm, na.rm = TRUE))
  below <- !x$ends_on_rock && (!is.finite(deepest) || deepest < zb - 0.25)
  v <- if (anyNA(ev) || below) NA else FALSE
  .hscs_prop(v, missing = if (is.na(v)) "designation" else character(0))
}

# The origin of each mineral row against the pattern `pat`: the layer's own
# origin (layer_origin) first, else the parent rock's (rock_origin); TRUE
# where it matches, FALSE where an origin is recorded that does not, NA where
# none is. A row with no origin takes the site's parent material when that
# names one (a parent material naming none is free text, not read as another
# origin).
.hscs_origin_rows <- function(x, pat) {
  lo <- .hscs_txt(x$h$layer_origin, x$n); ro <- .hscs_txt(x$h$rock_origin, x$n)
  o <- ifelse(!is.na(lo), lo, ro)
  out <- ifelse(is.na(o), NA, grepl(pat, o))
  pm <- .hscs_site_text(x, "parent_material")
  if (!is.na(pm) && grepl(pat, pm)) out[is.na(out)] <- TRUE
  out
}

# Stratification (8.4, p. 28) shown in a layer made of the adjacent rows `r`:
# a row of it with its top within 100 cm that differs from the mineral row
# directly above it by 5 points of coarse fragments, 10 points of sand, or an
# organic carbon higher by 0.2 points (as .hscs_stratified() reads them); or
# organic carbon of 0.2 % or more in the row of it at 100 cm. A gap above the
# layer leaves its first comparison open.
.hscs_strat_layer <- function(x, r) {
  h <- x$h; s <- x$surface
  ab <- .hscs_above(x); first <- x$ord[1L]
  cf <- h$coarse_fragments_pct; sa <- h$sand_pct; oc <- h$oc_pct
  v <- vapply(r, function(i) {
    if (h$top_cm[i] >= s + 100 - 0.5) return(FALSE)
    j <- ab[i]
    if (is.na(j)) return(if (identical(i, first)) FALSE else NA)
    if (!x$mineral[j]) return(FALSE)
    .any3(c(if (is.na(cf[i]) || is.na(cf[j])) NA else abs(cf[i] - cf[j]) >= 5,
            if (is.na(sa[i]) || is.na(sa[j])) NA else abs(sa[i] - sa[j]) >= 10,
            if (is.na(oc[i]) || is.na(oc[j])) NA else oc[i] - oc[j] >= 0.2 - 1e-9))
  }, NA)
  rt <- .hscs_rock_top(x)
  at100 <- if (!is.na(rt) && rt <= s + 100) integer(0) else intersect(r, .hscs_rows_in(x, s + 99, s + 100.25))
  deep <- if (length(at100)) .any3(ifelse(is.na(oc[at100]), NA, oc[at100] >= 0.2)) else FALSE
  .any3(c(v, deep))
}

# Stratified material of one origin (`origin`, per row) in a layer `min_th`
# cm or more thick with its top within 100 cm: a run of adjacent mineral rows
# of that origin, showing stratification (.hscs_strat_layer). TRUE with the
# run's top; FALSE when no stratified material lies within 100 cm, or no run
# of rows that may be of that origin is thick enough; NA otherwise.
.hscs_origin_layer <- function(x, origin, min_th = 25) {
  h <- x$h; s <- x$surface
  zb <- s + 100
  if (isFALSE(.hscs_stratified(x, s, zb))) return(list(passed = FALSE, top = NA_real_, rows = integer(0)))
  ok <- .and3v(origin, x$mineral3)
  for (r in .hscs_segments(x, ok %in% TRUE)) {
    if (h$top_cm[r[1L]] >= zb - 0.25 || sum(x$thick[r], na.rm = TRUE) < min_th - 1e-9) next
    if (isTRUE(.hscs_strat_layer(x, r))) return(list(passed = TRUE, top = h$top_cm[r[1L]], rows = r))
  }
  poss <- .hscs_open_above(x, zb)
  for (r in .hscs_segments(x, !(ok %in% FALSE))) {
    if (poss) break
    if (h$top_cm[r[1L]] >= zb - 0.25) next
    th <- sum(ifelse(is.na(x$thick[r]), Inf, x$thick[r]))
    if (identical(r[length(r)], x$last) && !x$ends_on_rock) th <- Inf
    if (th >= min_th - 1e-9 && !isFALSE(.hscs_strat_layer(x, r))) poss <- TRUE
  }
  list(passed = if (poss) NA else FALSE, top = NA_real_, rows = integer(0))
}

# lejtohordalekos (p. 30 and 35): stratified material laid down by mass
# movement on slopes (colluvial) in a layer 25 cm or more thick within 100
# cm. The origin is read from layer_origin, rock_origin or the site's parent
# material (colluvial, solimovic, deluvial, slope deposits).
.hscs_p_lejtohordalekos <- function(x) {
  if (is.na(x$surface)) return(.hscs_prop(NA))
  o <- .hscs_origin_layer(x, .hscs_origin_rows(x, .HSCS_COLLUVIAL))
  .hscs_prop(o$passed, top = o$top, layers = o$rows,
             missing = if (is.na(o$passed)) c("layer_origin", "rock_origin", "sand_pct", "coarse_fragments_pct", "oc_pct")
                       else character(0))
}

# ontes (p. 35): stratified material of river or lake floods ("folyovizi vagy
# tavi elontesbol") in a layer 25 cm or more thick within 100 cm. Fluviatile
# (alluvial) or lacustrine origin; a marine origin is not.
.hscs_p_ontes <- function(x) {
  if (is.na(x$surface)) return(.hscs_prop(NA))
  o <- .hscs_origin_layer(x, .hscs_origin_rows(x, .HSCS_FLUVIAL))
  .hscs_prop(o$passed, top = o$top, layers = o$rows,
             missing = if (is.na(o$passed)) c("layer_origin", "rock_origin", "sand_pct", "coarse_fragments_pct", "oc_pct")
                       else character(0))
}

# A designation as the buried soil would carry it at the surface: without the
# b suffix (or prefix), with its lithologic number counted from the buried
# soil's own material (`prefix0`).
.hscs_unbury_designation <- function(d, prefix0 = "") {
  d <- as.character(d)
  n0 <- if (nzchar(prefix0)) as.integer(prefix0) else 1L
  vapply(d, function(z) {
    if (is.na(z) || !nzchar(trimws(z))) return(z)
    m <- regmatches(trimws(z), regexec("^([0-9]*)b?([HOLAEBCRIW]/?[HOLAEBCRIW]?)([^[:space:]]*)(.*)$", trimws(z)))[[1]]
    if (!length(m)) return(z)
    rest <- gsub("()", "", gsub("b", "", m[4L], fixed = TRUE), fixed = TRUE)
    k <- (if (nzchar(m[2L])) as.integer(m[2L]) else 1L) - n0 + 1L
    paste0(if (k > 1L) as.character(k) else "", m[3L], rest, m[5L])
  }, "", USE.NAMES = FALSE)
}

# eltemetett (p. 34): a soil buried by 50 cm or more of new material; the new
# surface soil is classified, and the variety names the type of the buried
# soil ("eltemetett Barnafold"). Read: the uppermost buried horizon (b, not
# rock) starts 50 cm or more below the soil surface, and no row above it with
# no designation may be buried. The rows from its top down, with depths
# counted from it, the b suffix removed and the lithologic numbers counted
# from the buried soil's material, are keyed by the HSCS key alone (the
# types, as classify_hscs2024() keys them, without properties; the site
# keeps only its id and country, since land use and the like belong to the
# new surface). A buried soil keyed while keying a buried soil is not
# classified again. Buried horizons are recorded when met: with designations
# recorded and none buried, no.
.hscs_p_eltemetett <- function(x) {
  h <- x$h; s <- x$surface
  if (is.na(s)) return(.hscs_prop(NA))
  if (isTRUE(.hscs_state$buried)) return(.hscs_prop(NA, note = "inside a buried soil"))
  bur <- x$ord[x$des$buried[x$ord] & !x$rock[x$ord]]
  # rows with no designation (the uppermost row is the new surface, not buried)
  unk <- setdiff(x$ord[!x$des$known[x$ord] & !x$rock[x$ord]], x$ord[1L])
  if (!length(bur)) {
    # a row with no designation that starts 50 cm or more down may be buried
    deep_unk <- unk[h$top_cm[unk] >= s + 50 - 0.25]
    if (length(deep_unk)) return(.hscs_prop(NA, missing = "designation"))
    return(.hscs_prop(FALSE))
  }
  b <- bur[1L]; bt <- h$top_cm[b]
  if (bt < s + 50 - 0.25) return(.hscs_prop(FALSE, note = sprintf("buried horizon at %g cm, under less than 50 cm", bt - s)))
  if (any(h$top_cm[unk] < bt - 0.25)) return(.hscs_prop(NA, missing = "designation"))
  rows <- x$ord[h$top_cm[x$ord] >= bt - 0.25]
  hb <- h[rows, , drop = FALSE]
  hb$top_cm <- hb$top_cm - bt
  hb$bottom_cm <- hb$bottom_cm - bt
  hb$designation <- .hscs_unbury_designation(hb$designation, x$des$prefix[b])
  site <- list(id = paste0(as.character(x$site$id %||% "pedon")[1L], "-buried"),
               country = x$site$country %||% NA_character_,
               designation_system = x$site$designation_system %||% NA_character_)
  old <- .hscs_state$buried
  .hscs_state$buried <- TRUE
  on.exit(.hscs_state$buried <- old, add = TRUE)
  res <- tryCatch({
    pb <- PedonRecord$new(site = site, horizons = ensure_horizon_schema(data.table::as.data.table(hb)))
    classify_hierarchical("hscs2024", .cssc_prepare_pedon(pb), rules = load_rules("hscs2024"),
                          on_missing = "silent")
  }, error = function(e) e)
  if (inherits(res, "error"))
    return(.hscs_prop(TRUE, layers = rows, note = paste("the buried soil could not be keyed:", conditionMessage(res))))
  ty <- res$levels$name[res$levels$level == "types"][1L]
  if (is.null(ty) || is.na(ty)) return(.hscs_prop(TRUE, layers = rows, note = "the buried soil keyed to no type"))
  prov <- res$provisional_because
  .hscs_prop(TRUE, label = paste("eltemetett", ty), layers = rows,
             note = sprintf("buried soil from %g cm keyed as %s%s", bt - s, ty,
                            if (length(prov)) paste0(" (provisional: ", paste(prov, collapse = ", "), ")") else ""))
}

# ---- 8.6 coarse fragments (p. 34) ----------------------------------------------------

# kavicsos / kozettormelekes: rounded (gravel) or angular coarse fragments
# 40 % or more of the total volume of the soil material within 100 cm, or
# above hard rock or a cemented layer when shallower; "erosen" 80 % or more.
# Read as a thickness-weighted average over that window (the "teljes
# terfogat", total volume). Coarse fragments not recorded read as none (as
# the Kozethatasu key reads them). The shape is coarse_fragments_shape: a
# mixed shape, or none recorded, leaves the share of each shape between 0 and
# all of the fragments. The part of the window the description does not reach
# may hold anything. The layer (depth modifier) starts at the shallowest row
# with the share of the degree named.
.hscs_coarse_shape <- function(x, shape) {
  h <- x$h; s <- x$surface
  if (is.na(s)) return(.hscs_prop(NA))
  zb <- min(c(s + 100, .hscs_rock_top(x), .hscs_cemented_top(x)), na.rm = TRUE)
  if (zb <= s + 0.5) return(.hscs_prop(FALSE, note = "no soil material above rock or a cemented layer"))
  other <- if (shape == "rounded") "angular" else "rounded"
  cf <- h$coarse_fragments_pct; cf0 <- ifelse(is.na(cf), 0, cf)
  sh <- .hscs_txt(h$coarse_fragments_shape %||% rep(NA_character_, x$n), x$n)
  v_lo <- ifelse(sh %in% shape, cf0, 0)
  v_hi <- ifelse(sh %in% other, 0, cf0)
  lo <- .hscs_wmean(x, v_lo, s, zb)[["lo"]]
  hi <- .hscs_wmean(x, v_hi, s, zb)[["hi"]]
  miss <- c("coarse_fragments_pct", "coarse_fragments_shape")
  if (lo >= 40 - 1e-9) {
    eros <- lo >= 80 - 1e-9
    thr <- if (eros) 80 else 40
    r <- .hscs_rows_in(x, s, zb)
    top <- h$top_cm[r[v_lo[r] >= thr - 1e-9][1L]]
    note <- sprintf("%s fragments %.0f-%.0f %% of the volume to %g cm", shape, lo, hi, zb - s)
    if (!eros && hi >= 80) note <- paste0(note, "; erosen undecided")
    return(.hscs_prop(TRUE, top = top, variant = if (eros) "eros" else NULL,
                      layers = r[v_lo[r] >= thr - 1e-9], note = note))
  }
  if (hi < 40) return(.hscs_prop(FALSE))
  .hscs_prop(NA, missing = miss, note = sprintf("%s fragments %.0f-%.0f %% of the volume", shape, lo, hi))
}

.hscs_p_kavicsos <- function(x) .hscs_coarse_shape(x, "rounded")
.hscs_p_kozettormelekes <- function(x) .hscs_coarse_shape(x, "angular")

# ---- 8.6 texture (p. 35-36) -------------------------------------------------------------

# The texture class of the window [zt, zb] that covers the most thickness
# (mineral rows; organic rows and rock hold none). Decided when no class
# could overtake it with the part of unknown texture (rows without a class,
# and the part no row covers); a tie goes to the class met first from the
# surface.
.hscs_dominant_texture <- function(x, zt, zb) {
  h <- x$h
  r <- .hscs_rows_in(x, zt, zb); r <- r[!x$rock[r]]
  ov <- .asc_overlap(h$top_cm[r], ifelse(is.na(h$bottom_cm[r]), zb, h$bottom_cm[r]), zt, zb)
  org <- x$organic[r] %in% TRUE
  tex <- x$tex[r]
  known <- !org & !is.na(tex) & ov > 0
  unknown <- sum(ov[!org & is.na(tex)]) + max(0, (zb - zt) - sum(ov))
  if (!any(known)) return(list(class = NA_character_, decided = if (unknown > 0.5) NA else FALSE))
  sums <- tapply(ov[known], tex[known], sum)
  first <- tapply(h$top_cm[r][known], tex[known], min)
  o <- order(-sums, first[names(sums)])
  lead <- names(sums)[o[1L]]
  second <- if (length(o) > 1L) sums[[o[2L]]] else 0
  decided <- if (unknown <= 0.5) TRUE else if (sums[[lead]] > second + unknown) TRUE else NA
  list(class = lead, decided = decided, cm = sums)
}

# textura (p. 35): the dominant texture class within 50 cm of the soil
# surface (by the triangle, p. 36), by thickness; the window ends at hard
# rock. The label is the Hungarian name of the class (p. 36).
.hscs_p_textura <- function(x) {
  s <- x$surface
  if (is.na(s)) return(.hscs_prop(NA))
  zb <- min(c(s + 50, .hscs_rock_top(x)), na.rm = TRUE)
  if (zb <= s + 0.5) return(.hscs_prop(FALSE, note = "rock at the surface"))
  d <- .hscs_dominant_texture(x, s, zb)
  miss <- c("field_texture", "clay_pct", "silt_pct", "sand_pct")
  if (isTRUE(d$decided)) return(.hscs_prop(TRUE, label = .HSCS_TEX_HU[[d$class]],
                                           note = paste(sprintf("%s %g cm", names(d$cm), d$cm), collapse = ", ")))
  .hscs_prop(d$decided, missing = if (is.na(d$decided)) miss else character(0))
}

# textura_feku (p. 8, Laptalajok only): the texture of the mineral
# substratum (feku) the peat lies on: the row directly below the deepest
# organic row. Labelled "<class> feku" ("agyag feku"). Peat on rock has no
# mineral substratum (no); a description that ends in peat, or a gap below
# it, leaves it undecided.
.hscs_p_textura_feku <- function(x) {
  if (is.na(x$surface)) return(.hscs_prop(NA))
  org <- x$ord[x$organic[x$ord] %in% TRUE]
  if (!length(org)) return(.hscs_prop(FALSE, note = "no organic horizon"))
  d <- org[length(org)]
  b <- .hscs_below(x)[d]
  if (is.na(b)) return(.hscs_prop(NA, missing = "bottom_cm", note = "the mineral substratum is not described"))
  if (x$rock[b]) return(.hscs_prop(FALSE, note = "peat on rock"))
  if (is.na(x$tex[b]) || is.na(x$organic[b]))
    return(.hscs_prop(NA, layers = b, missing = c("field_texture", "clay_pct", "silt_pct", "sand_pct")))
  .hscs_prop(TRUE, label = paste(.HSCS_TEX_HU[[x$tex[b]]], "fek\u00fc"), layers = b)
}
