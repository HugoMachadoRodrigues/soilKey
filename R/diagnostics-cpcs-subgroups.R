# =============================================================================
# v0.9.333 -- the 276 subgroups of the French soil classification (CPCS
# 1967), the fourth and last level, under the 101 groups. The text names most
# subgroups by a secondary process (hydromorphy, salinity, a vertic
# character, a crust) or by one side of a measure (pH, clay index, depth of
# the gley); soilKey's readings of them are Hugo's (2026-11-01) and are listed
# in readings.yaml. A subgroup the data cannot decide leaves the name
# provisional; a group whose listed subgroups all fail stops the name at the
# group (no subgroup is invented). Three-valued. Names carry the _cpcs suffix.
# =============================================================================

# ---- shared readings --------------------------------------------------------------

.CPCS_NONE_RE <- "^(none|no|nil|absent|aucune?|nulle?|0|-|na|false|non)$"

# A free text that is recorded and is not a negation.
.cpcs_recorded_txt <- function(v) {
  v <- tolower(trimws(as.character(v)))
  !is.na(v) & nzchar(v) & !grepl(.CPCS_NONE_RE, v)
}

# The hydromorphic signs of each row (the IX/X reading, Hugo, 2026-11-01):
# redox features above 0 %, mottle colours recorded, a reduced matrix, or,
# where no mottles are recorded, a g or G of the notation. Pronounced: 15 %
# or more, a reduced matrix, or a G where no mottles are recorded; the other
# signs are slight. Not recorded: absent.
.cpcs_signs <- function(x) {
  rf <- .cpcs_num(x$h, "redoximorphic_features_pct")
  mt <- .cpcs_recorded_txt(.scsj_col(x$h, "mottle_colors", NA_character_))
  red <- .cpcs_reduced_rows(x)
  none_rec <- is.na(rf) & !mt
  G <- x$des$G %in% TRUE
  sign <- (!is.na(rf) & rf > 0) | (is.na(rf) & mt) | red | (none_rec & (x$des$g %in% TRUE | G))
  pron <- (!is.na(rf) & rf >= 15) | red | (none_rec & G)
  list(sign = sign, pronounced = pron)
}

# Concretions: iron nodules of 5 % or more (with manganese nodules where
# the text speaks of Fe-Mn), or a Cn of the notation where no nodules are
# recorded.
.cpcs_concretion_rows <- function(x, manganese = FALSE) {
  fe <- .cpcs_num(x$h, "ferric_nodules_pct")
  mn <- if (isTRUE(manganese)) .cpcs_num(x$h, "manganiferous_nodules_pct") else rep(NA_real_, x$n)
  (!is.na(fe) & fe >= 5) | (!is.na(mn) & mn >= 5) | (is.na(fe) & is.na(mn) & x$des$cn %in% TRUE)
}

# A water table recorded within `within` cm (not one recorded as none).
.cpcs_water_table <- function(x, within = 100) {
  d <- suppressWarnings(as.numeric(as.character(x$site$groundwater_depth_cm %||% NA)[1L]))
  gw <- .cpcs_gw_kind(x)
  if (is.na(d)) return(FALSE)
  if (!is.na(gw) && grepl("^none|not observed|no water|absent|aucune", gw)) return(FALSE)
  d < within
}

# The mineral rows (with a depth, not rock) of a window: rows starting from
# `from` to under `within` cm ("depth"), the B rows ("b"), the B rows and the
# lowest A2 row ("b_or_a2_base"), the A rows and the top 50 cm of the B (the
# window of the ferrallitic soils, "x"), or the first mineral row ("first").
.cpcs_window_rows <- function(x, window = "depth", from = 0, within = 100) {
  rows <- .cpcs_mineral_rows(x); rows <- rows[!x$rock[rows] & !is.na(x$dtop[rows])]
  b <- intersect(.cpcs_group_b_rows(x), rows)
  switch(window,
    depth = rows[x$dtop[rows] >= from & x$dtop[rows] < within],
    b = b,
    b_or_a2_base = { e <- intersect(.cpcs_e_rows(x), rows); unique(c(b, if (length(e)) e[length(e)])) },
    x = { tb <- if (length(b)) min(x$dtop[b]) else NA_real_
          unique(c(intersect(.cpcs_a_rows(x), rows), if (!is.na(tb)) b[x$dtop[b] < tb + 50])) },
    first = rows[seq_len(min(1L, length(rows)))],
    rows)
}

# Hard rock under a set of rows: an R, or a layer strongly cemented or
# indurated (or an m of the notation) that is not a calcareous crust.
.cpcs_hard_rock_rows <- function(x) {
  cem <- .scsj_txt(x$h, "cementation_class")
  x$rock | (((!is.na(cem) & grepl("strong|indurat", cem)) | x$des$m %in% TRUE) & !(x$des$ca %in% TRUE))
}

# A calcareous layer: carbonate, a positive effervescence, or a calcareous
# rock named (for the row, else the site); not recorded, not calcareous.
.cpcs_calcareous_row <- function(x, i) {
  cc <- .cpcs_num(x$h, "caco3_pct")[i]; ef <- .scsj_txt(x$h, "effervescence_class")[i]
  efp <- !is.na(ef) && grepl("slight|weak|moderate|strong|violent|faible|forte|moyenne|vive", ef) &&
    !grepl("^(none|no|not|non|absent|nil|0)", ef)
  ro <- .scsj_txt(x$h, "rock_origin")[i]
  txt <- .cpcs_drop_negated(.cpcs_lc(if (!is.na(ro)) ro else .cpcs_parent_txt(x)))
  (!is.na(cc) && cc > 0) || efp || grepl(.CPCS_CALC_ROCK_RE, txt)
}

# The kind of each podzolic B row (Q11): the notation first (h, fe, s), then
# the moist colour: Bh value 3 or less and chroma 2 or less, Bfe chroma 4 or
# more. Rows: the B rows that are neither a transition nor a (B); in an
# undesignated profile, the rows below the A with more than 0.5 % OM.
.cpcs_b_kind_rows <- function(x) {
  b <- .cpcs_b_rows(x)
  b <- b[!(x$des$trans[b] %in% TRUE) & !(x$des$bstruct[b] %in% TRUE) & !is.na(x$dtop[b])]
  b <- b[x$role[b] %in% "B" | is.na(x$om[b]) | x$om[b] > 0.5]
  hs <- x$des$h[b] %in% TRUE; fs <- x$des$fe[b] %in% TRUE
  v <- .cpcs_num(x$h, "munsell_value_moist")[b]; cr <- .cpcs_num(x$h, "munsell_chroma_moist")[b]
  sym <- hs | fs
  kh <- ifelse(sym, hs, ifelse(!is.na(v) & !is.na(cr), v <= 3 & cr <= 2, NA))
  kf <- ifelse(sym, fs, ifelse(!is.na(cr), cr >= 4, NA))
  list(b = b, h = kh, fe = kf)
}

# The kind of a podzol from the kinds of its B rows, three-valued: a row of
# unknown kind is tried as a Bh, a Bfe and neither. A row marked both (Bhs,
# Bhfe) is a humo-ferruginous B.
.cpcs_podzol_kind <- function(x, kind) {
  k <- .cpcs_b_kind_rows(x)
  if (!length(k$b)) return(FALSE)
  top <- x$dtop[k$b]
  judge <- function(kh, kf) {
    H <- which(kh & !kf); F <- which(kf & !kh); HF <- which(kh & kf)
    switch(kind,
      humic = length(H) > 0L && !length(F) && !length(HF),
      ferruginous = length(F) > 0L && !length(H) && !length(HF),
      humoferruginous = length(HF) > 0L || (length(H) && length(F) && min(top[H]) < max(top[F])),
      NA)
  }
  fill <- function(v, w) ifelse(is.na(v), w, v)
  out <- c(judge(fill(k$h, FALSE), fill(k$fe, FALSE)), judge(fill(k$h, TRUE), fill(k$fe, FALSE)),
           judge(fill(k$h, FALSE), fill(k$fe, TRUE)))
  if (all(out)) TRUE else if (!any(out)) FALSE else NA
}

#' CPCS 1967 subgroup gates (internal)
#'
#' The tests of the 276 subgroups of the French soil classification (CPCS
#' 1967), read by \code{\link{classify_cpcs1967}} in the order of
#' \code{inst/rules/cpcs1967/subgroups/}. Each returns a
#' \code{\link{DiagnosticResult}}.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param within,from,window,grade,water_table,concretions,manganese,none_above,kind,unrecorded,ec,esp,in_textural_b,prismatic_b,coarse,mineral,where,under,noncalcareous,at_most,above,below,at_least,rows,not_eroded,deflation,or_eroded,sand_fractions,volcanic,path,depth
#'   Arguments written in the rule files.
#' @return A \code{\link{DiagnosticResult}}.
#' @keywords internal
#' @name cpcs1967_subgroups
NULL

# ---- hydromorphy --------------------------------------------------------------------

#' @rdname cpcs1967_subgroups
#' @export
hydromorphic_signs_cpcs <- function(pedon, within = 100, from = 0, window = "depth", grade = "any",
                                    water_table = FALSE, concretions = FALSE, manganese = FALSE,
                                    none_above = FALSE) {
  x <- .cpcs_profile(pedon)
  within <- suppressWarnings(as.numeric(within %||% Inf)); if (is.na(within)) within <- Inf
  rows <- .cpcs_window_rows(x, window, from, within)
  s <- .cpcs_signs(x)
  sign <- any(s$sign[rows]); pron <- any(s$pronounced[rows])
  hyd <- switch(grade, pronounced = pron, slight = sign && !pron, sign)
  conc <- isTRUE(concretions) && any(.cpcs_concretion_rows(x, manganese)[rows])
  wt <- isTRUE(water_table) && .cpcs_water_table(x, within)
  p <- hyd || conc || wt
  # "de profondeur": no sign in the rows above
  if (p && isTRUE(none_above)) {
    up <- .cpcs_mineral_rows(x); up <- up[!is.na(x$dtop[up]) & x$dtop[up] < from]
    if (any(s$sign[up])) p <- FALSE
  }
  .cpcs_sub_result("hydromorphic_signs_cpcs", p, x,
                   list(signs = sign, pronounced = pron, concretions = conc, water_table = wt, window = window),
                   "subgroups, pp. 18-79")
}

#' @rdname cpcs1967_subgroups
#' @export
deep_hydromorphy_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # a hydromorphic layer starting at or below the base of the podzolic B,
  # none above the B (Hugo, 2026-11-01)
  b <- .cpcs_b_rows(x); b <- b[!is.na(x$dtop[b])]
  if (!length(b)) return(.cpcs_sub_result("deep_hydromorphy_cpcs", FALSE, x, where = "VIII, pp. 50-52"))
  base <- max(ifelse(is.na(x$dbot[b]), x$dtop[b], x$dbot[b]))
  hyd <- .cpcs_hydromorphic_rows(x)
  rows <- .cpcs_mineral_rows(x); rows <- rows[!is.na(x$dtop[rows])]
  deep <- any(hyd[rows[x$dtop[rows] >= base]] %in% TRUE)
  above <- rows[x$dtop[rows] < min(x$dtop[b])]
  p <- deep && !any(hyd[above] %in% TRUE)
  .cpcs_sub_result("deep_hydromorphy_cpcs", p, x, list(base_of_B_cm = base, below_B = deep), "VIII, pp. 50-52")
}

# ---- cryogenic forms (I-2, II-1) ------------------------------------------------------

.CPCS_FROST_RE <- list(
  wedges = "wedge|en coin|crack network|network of cracks|large cracks?|r\u00e9seaux? de fentes",
  earth_polygons = "earth polygon|nonsorted polygon|soil polygon|polygones? de terre",
  stone_polygons = paste0("\\bsorted (polygon|circle|net)|stone (polygon|circle|net|ring)|polygones? de pierres|",
                          "cercles? de pierres|roses? de pierres|rosette"),
  mud_boils = "mud ?-?boil|frost ?-?boil|pustule|ostiole",
  stripes = "stripe|stri\u00e9|\\bstrie",
  microrelief = "hummock|thufur|solifluct|bourrelet|bossel|lobe",
  palsa = "palsa|palse",
  thufur = "thufur|earth hummock",
  low_centre = "low-?cent(re|er)|ice-?wedge polygon|partie centrale|cent(re|er) (en )?d\u00e9pression|depressed cent",
  ice_cored = "ice-?cored|noyau de glace",
  small_hummocks = "small hummock|hummocks? (separated|with)[a-z ]* crack|nonsorted (circle|net)|earth circle|buttes de faibles")
.CPCS_ICE_RE <- list(palsa = "large (ice )?lens|grandes? lentilles?", thufur = "small (ice )?lens|petites? lentilles?")

#' @rdname cpcs1967_subgroups
#' @export
frost_form_cpcs <- function(pedon, kind, unrecorded = "undecided") {
  x <- .cpcs_profile(pedon)
  kind <- as.character(kind)
  clean <- function(v) gsub("non[- ]?sorted", "nonsorted", .cpcs_drop_negated(.cpcs_lc(v)))
  pg <- .cpcs_lc(x$site$patterned_ground %||% character(0))
  src <- pg
  if (kind == "microrelief")
    src <- .cpcs_lc(c(pg, x$site$microrelief_form %||% character(0), .scsj_txt(x$h, "microrelief_form")))
  ice <- if (kind %in% names(.CPCS_ICE_RE)) .cpcs_lc(.scsj_txt(x$h, "ice_form")) else ""
  rec <- (nzchar(trimws(src)) && trimws(src) != "na") || nzchar(trimws(ice))
  p <- if (!rec) { if (identical(unrecorded, "absent")) FALSE else NA } else
    grepl(.CPCS_FROST_RE[[kind]], clean(src)) ||
      (nzchar(ice) && grepl(.CPCS_ICE_RE[[kind]] %||% "$^", clean(ice)))
  .cpcs_sub_result("frost_form_cpcs", p, x, list(form = kind), "I/22-23, p. 19; II-1, pp. 22-23")
}

# ---- humus (II/21, XI/21) ---------------------------------------------------------------

.CPCS_HUMUS_RE <- list(mor = "(^|[^a-z])mor\\b|humus brut", moder = "(^|[^a-z])moder\\b", mull = "(^|[^a-z])mull\\b",
                       hydromor = "hydromor\\b", anmoor = "anmoor", tourbe = "tourbe|peat")

.cpcs_humus_form <- function(x) {
  v <- trimws(.cpcs_drop_negated(.cpcs_lc(x$site$humus_form %||% character(0))))
  if (!nzchar(v) || v == "na") NA_character_ else v
}

#' @rdname cpcs1967_subgroups
#' @export
humus_form_cpcs <- function(pedon, kind) {
  x <- .cpcs_profile(pedon)
  hf <- .cpcs_humus_form(x)
  p <- if (is.na(hf)) NA else grepl(.CPCS_HUMUS_RE[[as.character(kind)]], hf)
  .cpcs_sub_result("humus_form_cpcs", p, x, list(humus_form = hf), "II/21, p. 24")
}

#' @rdname cpcs1967_subgroups
#' @export
alpine_belt_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # the alpine reading of VIII/22; with no climate and no elevation recorded,
  # not alpine (Hugo, 2026-11-01)
  el <- suppressWarnings(as.numeric(as.character(x$site$elevation_m %||% NA)[1L]))
  p <- if (is.na(.cpcs_koppen(x$site)) && is.na(el)) FALSE else alpine_podzol_cpcs(pedon)$passed
  .cpcs_sub_result("alpine_belt_cpcs", p, x, list(elevation_m = el), "II/213, p. 24", climatic = TRUE)
}

# ---- salt and sodium ------------------------------------------------------------------------

#' @rdname cpcs1967_subgroups
#' @export
salt_cpcs <- function(pedon, within = 100, ec = 4) {
  x <- .cpcs_profile(pedon)
  # the EC of the saturated paste at `ec` dS/m or more in a layer within
  # 100 cm; salts seen with no EC leave it undecided (Hugo, 2026-11-01)
  rows <- .cpcs_rows_within(x, within); rows <- rows[!x$rock[rows]]
  e <- .cpcs_num(x$h, "ec_dS_m")[rows]
  seen <- x$des$sa[rows] %in% TRUE | (.cpcs_num(x$h, "halite_visible_pct")[rows] > 0) %in% TRUE
  p <- if (any(e >= ec, na.rm = TRUE)) TRUE else if (any(is.na(e) & seen)) NA else FALSE
  .cpcs_sub_result("salt_cpcs", p, x, list(ec_max = if (any(!is.na(e))) max(e, na.rm = TRUE) else NA_real_),
                   "subgroups, pp. 24-79")
}

#' @rdname cpcs1967_subgroups
#' @export
sodium_cpcs <- function(pedon, within = 100, esp = 6, in_textural_b = FALSE) {
  x <- .cpcs_profile(pedon)
  # exchangeable Na at `esp` % of the CEC or more in a layer within 100 cm
  # (in the textural B for VI/116); not recorded, absent (Hugo, 2026-11-01)
  rows <- .cpcs_rows_within(x, within); rows <- rows[!x$rock[rows]]
  if (isTRUE(in_textural_b)) rows <- intersect(rows, .cpcs_clay_index(x)$b)
  v <- .cpcs_esp(x)[rows]
  p <- any(v >= esp, na.rm = TRUE)
  .cpcs_sub_result("sodium_cpcs", p, x, list(esp_max = if (any(!is.na(v))) max(v, na.rm = TRUE) else NA_real_),
                   "subgroups, pp. 39-44")
}

# ---- vertic characters --------------------------------------------------------------------

#' @rdname cpcs1967_subgroups
#' @export
vertic_cpcs <- function(pedon, from = 0, prismatic_b = FALSE, coarse = FALSE) {
  x <- .cpcs_profile(pedon)
  # soilKey's protovertic horizon (and the vertic horizon it leaves out);
  # for the isohumic soils also a (B) of coarse or very coarse prismatic
  # structure; from `from` cm for "en profondeur"; IX/113 with a coarse
  # structure there when recorded (Hugo, 2026-11-01)
  pv <- tryCatch(protovertic(pedon), error = function(e) NULL)
  vh <- tryCatch(vertic_horizon(pedon), error = function(e) NULL)
  lay <- unique(c(if (isTRUE(pv$passed)) as.integer(pv$layers), if (isTRUE(vh$passed)) as.integer(vh$layers)))
  lay <- lay[!is.na(x$dtop[lay]) & x$dtop[lay] >= from]
  sz <- .scsj_txt(x$h, "structure_size")
  big <- !is.na(sz) & grepl("coarse|gross", sz)
  if (isTRUE(coarse)) lay <- lay[is.na(sz[lay]) | big[lay]]
  prism <- FALSE
  if (isTRUE(prismatic_b)) {
    b <- .cpcs_group_b_rows(x); b <- b[!is.na(x$dtop[b]) & x$dtop[b] >= from]
    st <- .scsj_txt(x$h, "structure_type")
    prism <- any(!is.na(st[b]) & grepl("prism", st[b]) & big[b])
  }
  p <- length(lay) > 0L || prism
  .cpcs_sub_result("vertic_cpcs", p, x, list(protovertic_layers = lay, coarse_prismatic_B = prism),
                   "subgroups, pp. 37-61")
}

#' @rdname cpcs1967_subgroups
#' @export
slickensides_moderate_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # "caracteres vertiques moyennement accentues": slickensides recorded, none
  # many or continuous (common at most, the minimum of the class) (Hugo,
  # 2026-11-01)
  rows <- .cpcs_rows_within(x, 150)
  sl <- .scsj_txt(x$h, "slickensides")[rows]
  rec <- .cpcs_recorded_txt(sl)
  strong <- rec & grepl("many|continuous|abundant|nombreu|abondant", sl)
  p <- any(rec) && !any(strong)
  .cpcs_sub_result("slickensides_moderate_cpcs", p, x, list(slickensides = paste(unique(sl[rec]), collapse = ", ")),
                   "III, p. 28")
}

#' @rdname cpcs1967_subgroups
#' @export
self_mulching_cpcs <- function(pedon, depth = 10) {
  x <- .cpcs_profile(pedon)
  # a self-mulching surface, with the top rows of fine or very fine structure
  # reaching `depth` cm; recorded with no structure size, 10 cm is met and
  # 20 cm undecided (Hugo, 2026-11-01)
  sc <- .cpcs_drop_negated(.cpcs_lc(x$site$surface_condition %||% character(0)))
  sm <- grepl("self-?mulch|auto-?division|autodivision", sc)
  p <- FALSE; reach <- NA_real_
  if (sm) {
    rows <- .cpcs_mineral_rows(x); rows <- rows[!x$rock[rows] & !is.na(x$dtop[rows])]
    sz <- .scsj_txt(x$h, "structure_size")
    if (!length(rows) || all(is.na(sz[rows]))) p <- if (depth <= 10) TRUE else NA else {
      reach <- 0
      for (i in rows) {
        if (is.na(sz[i]) || !grepl("fine|fin\\b", sz[i])) break
        reach <- if (is.na(x$dbot[i])) x$dtop[i] else x$dbot[i]
      }
      p <- reach >= depth
    }
  }
  .cpcs_sub_result("self_mulching_cpcs", p, x, list(self_mulching = sm, fine_structure_to_cm = reach), "III, pp. 28-29")
}

# ---- crusts and carbonates ------------------------------------------------------------------

.cpcs_diag_layers <- function(f, pedon) {
  r <- tryCatch(f(pedon), error = function(e) NULL)
  if (isTRUE(r$passed)) as.integer(r$layers) else integer(0)
}

#' @rdname cpcs1967_subgroups
#' @export
calcareous_crust_cpcs <- function(pedon, kind = "accumulation", mineral = "carbonate", where = "any") {
  x <- .cpcs_profile(pedon)
  # encroutement: soilKey's calcic horizon, 5 % secondary carbonates or more,
  # or a Ca of the notation where neither is measured; croute: a cemented
  # layer (petrocalcic, calcrete, a Ca cemented moderately or more). The
  # same for gypsum (gypsic, 5 % visible gypsum; petrogypsic, a cemented
  # Cs) (Hugo, 2026-11-01). The calcic and gypsic layers count where the
  # accumulation is shown, as the WRB asks: secondary carbonates recorded,
  # 5 % more carbonate than the layer below, or a Ca of the notation; 1 %
  # visible gypsum or a Cs (soilKey's reading: soilKey's calcic horizon takes
  # unrecorded secondary carbonates as present, its gypsic horizon does not
  # read visible gypsum, and the soils of V/12 and V/32 are calcareous or
  # gypseous by definition)
  h <- x$h; n <- x$n
  cem <- .scsj_txt(h, "cementation_class"); pan <- .scsj_txt(h, "pan_type")
  hard <- (!is.na(cem) & grepl("moderat|strong|indurat", cem)) | x$des$m %in% TRUE
  hard_c <- seq_len(n) %in% .cpcs_diag_layers(petrocalcic, pedon) | .cpcs_calc_crust_rows(x)
  hard_g <- seq_len(n) %in% .cpcs_diag_layers(petrogypsic, pedon) | (x$des$cs %in% TRUE & hard) |
    (!is.na(pan) & grepl("petrogyps|gypcrete|gypsum crust|cro\u00fbte gypseuse", pan))
  sc <- .cpcs_num(h, "secondary_carbonates_pct"); cc <- .cpcs_num(h, "caco3_pct")
  gv <- .cpcs_num(h, "gypsum_visible_pct"); cs <- .cpcs_num(h, "caso4_pct")
  cal <- tryCatch(calcic(pedon), error = function(e) NULL)
  cal_l <- if (isTRUE(cal$passed)) {
    d <- cal$evidence$secondary$details
    shown <- c(d$layers[(d$protocalcic %in% TRUE) | (d$enrichment %in% TRUE)], which(x$des$ca %in% TRUE))
    intersect(as.integer(cal$layers), shown)
  } else integer(0)
  gyp_l <- intersect(.cpcs_diag_layers(gypsic, pedon), which((!is.na(gv) & gv >= 1) | x$des$cs %in% TRUE))
  acc_c <- hard_c | seq_len(n) %in% cal_l | (!is.na(sc) & sc >= 5) |
    (is.na(sc) & is.na(cc) & x$des$ca %in% TRUE)
  acc_g <- hard_g | seq_len(n) %in% gyp_l | (!is.na(gv) & gv >= 5) |
    (is.na(gv) & is.na(cs) & x$des$cs %in% TRUE)
  v <- switch(paste(kind, mineral),
    "accumulation carbonate" = acc_c, "accumulation gypsum" = acc_g, "accumulation any" = acc_c | acc_g,
    "cemented carbonate" = hard_c, "cemented gypsum" = hard_g, "cemented any" = hard_c | hard_g,
    rep(FALSE, n))
  rows <- .cpcs_mineral_rows(x); rows <- rows[!x$rock[rows] & !is.na(x$dtop[rows])]
  a <- .cpcs_a_rows(x)
  rows <- switch(where,
    under_a = { k <- if (length(a)) match(a[length(a)], rows) else 0L; rows[seq_along(rows) == k + 1L] },
    top_of_c = { cr <- rows[x$role[rows] %in% "C"]
                 if (!length(cr)) integer(0) else {
                   k <- match(cr[1L], rows)
                   up <- if (k > 1L) rows[k - 1L] else integer(0)
                   c(cr[1L], up[x$role[up] %in% "B" & x$des$trans[up] %in% TRUE & x$des$ca[up] %in% TRUE]) } },
    rows)
  p <- any(v[rows])
  .cpcs_sub_result("calcareous_crust_cpcs", p, x, list(kind = kind, mineral = mineral, where = where),
                   "subgroups, pp. 37-44, 76")
}

#' @rdname cpcs1967_subgroups
#' @export
carbonate_nodules_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # nodules or concretions of carbonate within 100 cm: their form recorded,
  # hard carbonate, or a Can of the notation (Hugo, 2026-11-01)
  rows <- .cpcs_rows_within(x, 100)
  f <- .scsj_txt(x$h, "secondary_carbonates_form")[rows]
  hc <- .cpcs_num(x$h, "carbonate_hard_pct")[rows]
  sy <- vapply(as.character(.scsj_col(x$h, "designation", NA_character_))[rows], .cpcs_symbol, "", USE.NAMES = FALSE)
  p <- any(!is.na(f) & grepl("concretion|nodul", f)) || any(hc > 0, na.rm = TRUE) ||
    any(!is.na(sy) & grepl("[Cc]an", sy))
  .cpcs_sub_result("carbonate_nodules_cpcs", p, x, where = "XI/36, p. 76")
}

#' @rdname cpcs1967_subgroups
#' @export
recalcified_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # carbonate in the A or the (B): 1 % or more, an effervescence moderate or
  # stronger, or a Ca of the notation; a Cca alone is the modal's (Hugo,
  # 2026-11-01)
  rows <- unique(c(.cpcs_a_rows(x), .cpcs_group_b_rows(x)))
  cc <- .cpcs_num(x$h, "caco3_pct")[rows]; ef <- .scsj_txt(x$h, "effervescence_class")[rows]
  efp <- !is.na(ef) & grepl("moderat|strong|violent|forte|moyenne|vive|extrem", ef)
  p <- any(cc >= 1, na.rm = TRUE) || any(efp) || any(is.na(cc) & x$des$ca[rows] %in% TRUE)
  .cpcs_sub_result("recalcified_cpcs", p, x, where = "IX/215, p. 61")
}

#' @rdname cpcs1967_subgroups
#' @export
rendzina_carbonate_cpcs <- function(pedon, kind) {
  x <- .cpcs_profile(pedon)
  a <- .cpcs_a_rows(x)
  # 112: a very strong effervescence in the A, or 10 % active carbonate;
  # 114: under 10 % carbonate in the fine earth of the A with 15 % coarse
  # fragments or more (Hugo, 2026-11-01)
  p <- if (identical(kind, "active")) {
    ef <- .scsj_txt(x$h, "effervescence_class")[a]; act <- .cpcs_num(x$h, "caco3_active_pct")[a]
    any(!is.na(ef) & grepl("very strong|violent|extrem|tr\u00e8s fort|tr\u00e8s vive", ef)) || any(act >= 10, na.rm = TRUE)
  } else {
    cc <- .cpcs_wmean(x, .cpcs_num(x$h, "caco3_pct"), a); cf <- .cpcs_wmean(x, .cpcs_num(x$h, "coarse_fragments_pct"), a)
    !is.na(cc) && !is.na(cf) && cc < 10 && cf >= 15
  }
  .cpcs_sub_result("rendzina_carbonate_cpcs", p, x, list(kind = kind), "V/11, p. 36")
}

# ---- rock, depth, thickness, organic matter ------------------------------------------------------

#' @rdname cpcs1967_subgroups
#' @export
hard_rock_contact_cpcs <- function(pedon, under = "A", within = NA, noncalcareous = FALSE) {
  x <- .cpcs_profile(pedon)
  # hard rock directly under the A (or under the (B)), with no C between
  # (Hugo, 2026-11-01); rock not recorded, absent
  rows <- .cpcs_mineral_rows(x); rows <- rows[!is.na(x$dtop[rows])]
  hard <- .cpcs_hard_rock_rows(x)
  top <- if (identical(under, "B")) intersect(.cpcs_group_b_rows(x), rows) else intersect(.cpcs_a_rows(x), rows)
  nxt <- if (length(top)) rows[match(top[length(top)], rows) + 1L] else if (identical(under, "B")) NA_integer_ else rows[1L]
  p <- !is.na(nxt) && isTRUE(hard[nxt])
  if (p && !is.na(within)) p <- x$dtop[nxt] <= within
  if (p && isTRUE(noncalcareous)) p <- !.cpcs_calcareous_row(x, nxt)
  .cpcs_sub_result("hard_rock_contact_cpcs", p, x, list(rock_top_cm = if (is.na(nxt)) NA_real_ else x$dtop[nxt]),
                   "subgroups, pp. 18-47")
}

#' @rdname cpcs1967_subgroups
#' @export
solum_depth_cpcs <- function(pedon, at_most = NA, above = NA) {
  x <- .cpcs_profile(pedon)
  # the top of the first C, G or R; with none, the profile described below
  # the limit tells only that the solum reaches it (Hugo, 2026-11-01)
  rows <- .cpcs_mineral_rows(x)
  c <- rows[(x$role[rows] %in% c("C", "R", "G") | x$rock[rows]) & !is.na(x$dtop[rows])]
  bot <- suppressWarnings(max(x$dbot[rows], na.rm = TRUE)); if (!is.finite(bot)) bot <- NA_real_
  d <- if (length(c)) min(x$dtop[c]) else NA_real_
  p <- if (!is.na(at_most)) {
    if (!is.na(d)) d <= at_most else if (is.na(bot)) NA else if (bot > at_most) FALSE else NA
  } else {
    if (!is.na(d)) d > above else if (is.na(bot)) NA else if (bot >= above) TRUE else NA
  }
  .cpcs_sub_result("solum_depth_cpcs", p, x, list(solum_cm = d, described_to_cm = bot), "subgroups, pp. 33-47")
}

#' @rdname cpcs1967_subgroups
#' @export
a1_thickness_cpcs <- function(pedon, above = 25) {
  x <- .cpcs_profile(pedon)
  th <- .cpcs_thickness(x, .cpcs_a1_rows(x))
  .cpcs_sub_result("a1_thickness_cpcs", if (is.na(th)) NA else th > above, x, list(a1_cm = th), "IV/21-22, p. 33")
}

#' @rdname cpcs1967_subgroups
#' @export
organic_matter_cpcs <- function(pedon, rows = "a", at_least = NA, above = NA, below = NA) {
  x <- .cpcs_profile(pedon)
  # the thickness-weighted OM of the A rows, of the A1 rows, or of the top
  # 20 cm of the mineral soil
  m <- switch(rows, top20 = .cpcs_om20(x), a1 = .cpcs_wmean(x, x$om, .cpcs_a1_rows(x)),
              .cpcs_wmean(x, x$om, .cpcs_a_rows(x)))
  p <- if (is.na(m)) NA else (is.na(at_least) || m >= at_least) && (is.na(above) || m > above) &&
    (is.na(below) || m < below)
  .cpcs_sub_result("organic_matter_cpcs", p, x, list(om = m, rows = rows), "subgroups, pp. 25-70")
}

#' @rdname cpcs1967_subgroups
#' @export
climate_cpcs <- function(pedon, kind) {
  x <- .cpcs_profile(pedon)
  .cpcs_sub_result("climate_cpcs", .cpcs_climate(x, as.character(kind)), x, list(kind = kind), "V/115, p. 36",
                   climatic = TRUE)
}

# ---- texture, colour and pH of the B --------------------------------------------------------

#' @rdname cpcs1967_subgroups
#' @export
clay_index_cpcs <- function(pedon, above = NA, at_most = NA, below = NA, not_eroded = FALSE) {
  x <- .cpcs_profile(pedon)
  ci <- .cpcs_clay_index(x); r <- ci$r
  # no B, no clay index
  p <- if (!length(ci$b)) FALSE else if (is.na(r)) NA else (is.na(above) || r > above) && (is.na(at_most) || r <= at_most) &&
    (is.na(below) || r < below)
  # "en l'absence d'erosion" (VII/114): recorded erosion rules it out
  if (isTRUE(not_eroded) && .cpcs_recent_erosion(x)) p <- FALSE
  .cpcs_sub_result("clay_index_cpcs", p, x, list(clay_index = r), "subgroups, pp. 46-70")
}

#' @rdname cpcs1967_subgroups
#' @export
b_colour_cpcs <- function(pedon, kind) {
  x <- .cpcs_profile(pedon)
  b <- .cpcs_group_b_rows(x)
  hc <- .cpcs_hue_chroma(x)
  # reddened: 5YR or redder with chroma 4 or more; yellow: 7.5YR or yellower;
  # both in the larger part of the B. Ochre: a B row of hue 10YR to 5YR with
  # chroma 6 or more (Hugo, 2026-11-01)
  p <- switch(kind,
    reddened = .cpcs_larger_part_b(x, .cpcs_reddened_rows(x), b),
    yellow = .cpcs_larger_part_b(x, ifelse(hc$neu, FALSE, ifelse(is.na(hc$hn), NA, hc$hn >= 17.5)), b),
    ochre = if (!length(b)) FALSE else .cpcs_any(ifelse(hc$neu, FALSE, ifelse(is.na(hc$hn) | is.na(hc$cr), NA,
                                                     hc$hn >= 15 & hc$hn <= 20 & hc$cr >= 6))[b]),
    NA)
  .cpcs_sub_result("b_colour_cpcs", p, x, list(kind = kind), "subgroups, pp. 41-70")
}

#' @rdname cpcs1967_subgroups
#' @export
b_ph_cpcs <- function(pedon, below = 5.5) {
  x <- .cpcs_profile(pedon)
  ph <- .cpcs_num(x$h, "ph_h2o")
  p <- .cpcs_larger_part_b(x, ifelse(is.na(ph), NA, ph < below), .cpcs_group_b_rows(x))
  .cpcs_sub_result("b_ph_cpcs", p, x, where = "VII/11-12, pp. 46-47")
}

#' @rdname cpcs1967_subgroups
#' @export
allophane_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  .cpcs_sub_result("allophane_cpcs", .cpcs_allophanic(x), x, where = "VII/113, p. 46")
}

#' @rdname cpcs1967_subgroups
#' @export
tongues_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  .cpcs_sub_result("tongues_cpcs", .cpcs_tongues(x, pedon), x, where = "VII/126, p. 47")
}

# ---- reworking, rejuvenation, aeolian additions (IX, X) -------------------------------------------

# The sand fractions of the A and the B: a relative change of 20 % or more in
# the ratio of coarse to fine sand between the A rows and the B rows within
# 100 cm, when both fractions are recorded (Hugo, 2026-11-01).
.cpcs_sand_contrast <- function(x) {
  cs <- .cpcs_num(x$h, "coarse_sand_pct"); fs <- .cpcs_num(x$h, "fine_sand_pct")
  ratio <- ifelse(!is.na(cs) & !is.na(fs) & fs > 0, cs / fs, NA_real_)
  a <- .cpcs_a_rows(x); b <- .cpcs_group_b_rows(x); b <- b[!is.na(x$dtop[b]) & x$dtop[b] < 100]
  ra <- .cpcs_wmean(x, ratio, a); rb <- .cpcs_wmean(x, ratio, b)
  if (is.na(ra) || is.na(rb) || max(ra, rb) <= 0) return(FALSE)
  abs(ra - rb) / max(ra, rb) >= 0.2
}

#' @rdname cpcs1967_subgroups
#' @export
reworked_cpcs <- function(pedon, where = "profile", or_eroded = FALSE, sand_fractions = FALSE) {
  x <- .cpcs_profile(pedon)
  if (identical(where, "a_base")) {
    # IX/125: a stone line in the last A row, a discontinuity at the A/B
    # boundary, or a colluvial origin of the A rows (Hugo, 2026-11-01)
    a <- .cpcs_a_rows(x)
    rw <- .cpcs_reworked(x, cand = if (length(a)) a[length(a)] else integer(0))
    b <- .cpcs_group_b_rows(x)
    disc <- length(a) && length(b) && !identical(x$des$lith[a[length(a)]], x$des$lith[b[1L]])
    coll <- length(a) && grepl(.CPCS_ORIGIN_RE$colluvial, .cpcs_drop_negated(.cpcs_lc(.scsj_txt(x$h, "layer_origin")[a])))
    p <- rw$line || disc || coll
  } else {
    rw <- .cpcs_reworked(x)
    p <- rw$p
  }
  sand <- isTRUE(sand_fractions) && .cpcs_sand_contrast(x)
  ero <- isTRUE(or_eroded) && .cpcs_recent_erosion(x)
  .cpcs_sub_result("reworked_cpcs", p || sand || ero, x,
                   list(reworked = p, sand_fractions = sand, erosion = ero), "IX/125, p. 59; X, pp. 67-70")
}

#' @rdname cpcs1967_subgroups
#' @export
weatherable_minerals_cpcs <- function(pedon, at_least = 3, volcanic = FALSE) {
  x <- .cpcs_profile(pedon)
  # 3 % weatherable minerals in the sand, under the 10 % of the rejuvenated
  # groups; X/223 also volcanic glass, or a volcanic or aeolian deposit,
  # within 50 cm (Hugo, 2026-11-01)
  p <- .cpcs_weatherable(x, at_least)
  if (!p && isTRUE(volcanic)) {
    rows <- .cpcs_rows_within(x, 50)
    txt <- .cpcs_drop_negated(.cpcs_lc(c(.scsj_txt(x$h, "layer_origin")[rows], .scsj_txt(x$h, "rock_origin")[rows])))
    p <- any(.cpcs_num(x$h, "volcanic_glass_pct")[rows] > 0, na.rm = TRUE) ||
      grepl(.CPCS_ORIGIN_RE$volcanic, txt) || grepl(.CPCS_ORIGIN_RE$aeolian, txt)
  }
  .cpcs_sub_result("weatherable_minerals_cpcs", p, x, where = "X, pp. 67-70")
}

#' @rdname cpcs1967_subgroups
#' @export
aeolian_addition_cpcs <- function(pedon, deflation = FALSE) {
  x <- .cpcs_profile(pedon)
  # an aeolian addition within 50 cm: aeolian words in the origin of a row,
  # an aeolian morphology recorded, or an aeolian material; II/313 also
  # recorded deflation
  rows <- .cpcs_rows_within(x, 50)
  txt <- .cpcs_drop_negated(.cpcs_lc(c(.scsj_txt(x$h, "layer_origin")[rows], .scsj_txt(x$h, "rock_origin")[rows])))
  am <- .cpcs_recorded_txt(.scsj_col(x$h, "aeolian_morphology", NA_character_))[rows]
  aeo <- grepl(.CPCS_ORIGIN_RE$aeolian, txt) || any(am) || identical(.cpcs_origin_kind(x), "aeolian")
  defl <- isTRUE(deflation) && isTRUE(.nzsc_yes(x$site$wind_deflation))
  .cpcs_sub_result("aeolian_addition_cpcs", aeo || defl, x, list(aeolian = aeo, deflation = defl),
                   "II/313, p. 24; X, pp. 68-70")
}

#' @rdname cpcs1967_subgroups
#' @export
eluviated_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # a row at the A/(B) boundary (the last A row, an AB or BA, the first B
  # row) with at least twice the coarse elements (coarse fragments, else
  # gravel, plus iron nodules) of the row above and of the row below, or
  # twice their coarse sand (Hugo, 2026-11-01)
  rows <- .cpcs_mineral_rows(x); rows <- rows[!x$rock[rows] & !is.na(x$dtop[rows])]
  cf <- .cpcs_num(x$h, "coarse_fragments_pct"); gr <- .cpcs_num(x$h, "gravel_pct")
  nod <- .cpcs_num(x$h, "ferric_nodules_pct"); csd <- .cpcs_num(x$h, "coarse_sand_pct")
  frag <- ifelse(!is.na(cf), cf, gr)
  ce <- ifelse(is.na(frag) & is.na(nod), NA_real_, ifelse(is.na(frag), 0, frag) + ifelse(is.na(nod), 0, nod))
  a <- intersect(.cpcs_a_rows(x), rows); b <- intersect(.cpcs_group_b_rows(x), rows)
  cand <- unique(c(if (length(a)) a[length(a)], rows[x$role[rows] %in% "AB" | (x$role[rows] %in% "B" & x$des$trans[rows] %in% TRUE &
                                                      seq_along(rows) <= match(b[1L], rows, nomatch = 0L))],
                   if (length(b)) b[1L]))
  twice <- function(v, i) {
    k <- match(i, rows); if (is.na(k) || k <= 1L || k >= length(rows)) return(FALSE)
    up <- v[rows[k - 1L]]; lo <- v[rows[k + 1L]]
    !is.na(v[i]) && !is.na(up) && !is.na(lo) && v[i] > 0 && v[i] >= 2 * up && v[i] >= 2 * lo
  }
  p <- any(vapply(cand, function(i) twice(ce, i) || twice(csd, i), NA))
  .cpcs_sub_result("eluviated_cpcs", p, x, where = "X/135, X/246, p. 68")
}

#' @rdname cpcs1967_subgroups
#' @export
indurated_cpcs <- function(pedon, where = "b", within = 100) {
  x <- .cpcs_profile(pedon)
  # a carapace or cuirasse: cemented moderately or more, a ferricrete or
  # ironpan, an m of the notation, or soilKey's petroplinthic horizon; in a
  # B row (X), within 100 cm (IX)
  rows <- if (identical(where, "b")) .cpcs_group_b_rows(x) else .cpcs_rows_within(x, within)
  cem <- .scsj_txt(x$h, "cementation_class"); pan <- .scsj_txt(x$h, "pan_type")
  ind <- (!is.na(cem) & grepl("moderat|strong|indurat", cem)) | x$des$m %in% TRUE |
    (!is.na(pan) & grepl("ferricrete|iron ?pan|ironstone|cuirasse|carapace|laterit|petroplinth", pan)) |
    seq_len(x$n) %in% .cpcs_diag_layers(petroplinthic, pedon)
  .cpcs_sub_result("indurated_cpcs", any(ind[rows]), x, list(where = where), "IX/123, p. 59; X, pp. 67-71")
}

#' @rdname cpcs1967_subgroups
#' @export
concretions_cpcs <- function(pedon, where = "within", within = 100) {
  x <- .cpcs_profile(pedon)
  rows <- if (identical(where, "b")) .cpcs_group_b_rows(x) else .cpcs_rows_within(x, within)
  .cpcs_sub_result("concretions_cpcs", any(.cpcs_concretion_rows(x)[rows]), x, list(where = where),
                   "IX/122, p. 59; VII/414, p. 48")
}

#' @rdname cpcs1967_subgroups
#' @export
saturated_poor_sand_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # V 80 % or more in the B, under 1 % OM in the A, and 8 % clay or less in
  # every row to 100 cm (Hugo, 2026-11-01). The V is read in a B row of the
  # first 100 cm of the B, not in its larger part: the sub-class IX-1 already
  # needs V under 65 % in the larger part of the B (soilKey's reading)
  bs <- .cpcs_bs(x)
  b <- .cpcs_group_b_rows(x); b <- b[!is.na(x$dtop[b])]
  b <- if (length(b)) b[x$dtop[b] < min(x$dtop[b]) + 100] else b
  v80 <- if (!length(b)) FALSE else .cpcs_any(ifelse(is.na(bs[b]), NA, bs[b] >= 80))
  om <- .cpcs_wmean(x, x$om, .cpcs_a_rows(x))
  rows <- .cpcs_rows_within(x, 100); rows <- rows[!x$rock[rows]]
  clay <- if (!length(rows)) NA else .cpcs_all(ifelse(is.na(x$clay[rows]), NA, x$clay[rows] <= 8))
  p <- .cpcs_and(v80, if (is.na(om)) NA else om < 1, clay)
  .cpcs_sub_result("saturated_poor_sand_cpcs", p, x, list(bs_80_in_B = v80, om_A = om, clay_8 = clay), "IX/114, p. 59")
}

#' @rdname cpcs1967_subgroups
#' @export
humic_horizon_cpcs <- function(pedon, kind) {
  x <- .cpcs_profile(pedon)
  a <- .cpcs_a_rows(x)
  # contrasted: an A of moist value 3 or less over a (B) of hue 5YR or
  # redder; deep: the A and AB 50 cm or more and half the described depth
  # or more (Hugo, 2026-11-01)
  if (identical(kind, "contrasted")) {
    v <- .cpcs_num(x$h, "munsell_value_moist")
    dark <- if (!length(a)) FALSE else .cpcs_all(ifelse(is.na(v[a]), NA, v[a] <= 3))
    hc <- .cpcs_hue_chroma(x)
    red <- .cpcs_larger_part_b(x, ifelse(hc$neu, FALSE, ifelse(is.na(hc$hn), NA, hc$hn <= 15)), .cpcs_group_b_rows(x))
    p <- .cpcs_and(dark, red)
  } else {
    th <- .cpcs_thickness(x, a)
    rows <- .cpcs_mineral_rows(x)
    bot <- suppressWarnings(max(x$dbot[rows], na.rm = TRUE))
    p <- if (is.na(th) || !is.finite(bot)) NA else th >= 50 && th >= (bot - x$surface) / 2
  }
  .cpcs_sub_result("humic_horizon_cpcs", p, x, list(kind = kind), "X/221-222, p. 69")
}

#' @rdname cpcs1967_subgroups
#' @export
gibbsitic_dark_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # a dark brown B (7.5YR to 10YR, value 3 or less, chroma 3 or less), pH
  # under 4.5, and gibbsite: 25 % of the clay or more where it is measured,
  # else Ki under 1.0 where Ki exists (Hugo, 2026-11-01)
  b <- .cpcs_group_b_rows(x)
  hc <- .cpcs_hue_chroma(x); v <- .cpcs_num(x$h, "munsell_value_moist"); ph <- .cpcs_num(x$h, "ph_h2o")
  dark <- .cpcs_larger_part_b(x, ifelse(is.na(hc$hn) | is.na(v) | is.na(hc$cr), NA,
                                        hc$hn >= 17.5 & hc$hn <= 20 & v <= 3 & hc$cr <= 3), b)
  acid <- .cpcs_larger_part_b(x, ifelse(is.na(ph), NA, ph < 4.5), b)
  gb <- .cpcs_num(x$h, "gibbsite_pct"); ki <- .cpcs_ki(x)
  gibb <- if (any(!is.na(gb[b]))) .cpcs_larger_part_b(x, ifelse(is.na(gb), NA, gb >= 25), b) else
    if (any(!is.na(ki[b]))) .cpcs_larger_part_b(x, ifelse(is.na(ki), NA, ki < 1), b) else TRUE
  .cpcs_sub_result("gibbsitic_dark_cpcs", .cpcs_and(dark, acid, gibb), x,
                   list(dark_brown_B = dark, ph_under_4_5 = acid, gibbsitic = gibb), "X/322, p. 70")
}

# ---- podzolised soils (VIII) ----------------------------------------------------------------

#' @rdname cpcs1967_subgroups
#' @export
podzol_b_kind_cpcs <- function(pedon, kind) {
  x <- .cpcs_profile(pedon)
  .cpcs_sub_result("podzol_b_kind_cpcs", .cpcs_podzol_kind(x, as.character(kind)), x, list(kind = kind),
                   "VIII/11, p. 50")
}

#' @rdname cpcs1967_subgroups
#' @export
no_b_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  .cpcs_sub_result("no_b_cpcs", .cpcs_not(.cpcs_horizon_types(x, pedon)$b_any), x, where = "VIII/114, p. 51; XI/221, p. 75")
}

#' @rdname cpcs1967_subgroups
#' @export
podzolic_path_cpcs <- function(pedon, path) {
  x <- .cpcs_profile(pedon)
  p <- switch(path,
    a2_over_bg = .cpcs_podzolic_paths(x)$b,
    ap_over_b = .cpcs_podzolic_paths(x)$c,
    humic_a1 = .cpcs_crypto_paths(x)$humic,
    brown_b = .cpcs_crypto_paths(x)$brown,
    NA)
  .cpcs_sub_result("podzolic_path_cpcs", p, x, list(path = path), "VIII/12, p. 51; VIII/14, pp. 52-53")
}

#' @rdname cpcs1967_subgroups
#' @export
stagnogley_a2_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # between the A2 and the B: a g master, or a reduced greenish-white layer
  # (5Y or bluer, or neutral; value 6 or more; chroma 2 or less; or a
  # positive Childs test) (Hugo, 2026-11-01)
  rows <- .cpcs_mineral_rows(x); rows <- rows[!is.na(x$dtop[rows])]
  b <- intersect(.cpcs_b_rows(x), rows)
  if (!length(b)) return(.cpcs_sub_result("stagnogley_a2_cpcs", FALSE, x, where = "VIII/124, p. 51"))
  tb <- min(x$dtop[b])
  e <- intersect(.cpcs_e_rows(x), rows)
  from <- if (length(e)) { l <- e[length(e)]; if (is.na(x$dbot[l])) x$dtop[l] else x$dbot[l] } else -Inf
  gm <- .cpcs_g_master(x)
  zone <- rows[x$dtop[rows] < tb & (x$dtop[rows] >= from | gm[rows]) & !(x$role[rows] %in% c("A", "AB", "AC"))]
  hue <- as.character(.scsj_col(x$h, "munsell_hue_moist", NA_character_))
  hn <- .scsj_hue_num(hue); neu <- .scsj_neutral(hue)
  v <- .cpcs_num(x$h, "munsell_value_moist"); cr <- .cpcs_num(x$h, "munsell_chroma_moist")
  gw <- (neu | (!is.na(hn) & hn >= 25)) & !is.na(v) & v >= 6 & (neu | (!is.na(cr) & cr <= 2))
  p <- any(gm[zone] | gw[zone] | .scsj_dipyridyl(x$h)[zone] %in% TRUE)
  .cpcs_sub_result("stagnogley_a2_cpcs", p, x, where = "VIII/124, p. 51")
}

#' @rdname cpcs1967_subgroups
#' @export
podzol_gley_cpcs <- function(pedon, kind) {
  x <- .cpcs_profile(pedon)
  if (identical(kind, "concretioned")) {
    return(.cpcs_sub_result("podzol_gley_cpcs", .cpcs_concretioned_b(x), x, list(kind = kind), "VIII/312, p. 54"))
  }
  # a Bh with a gley (a reduced matrix, or a G) above it or in contact
  k <- .cpcs_b_kind_rows(x)
  bh <- k$b[k$h %in% TRUE]
  rows <- .cpcs_mineral_rows(x); rows <- rows[!is.na(x$dtop[rows])]
  r3 <- .cpcs_reduced3(x)
  gl <- r3 %in% TRUE | x$des$G %in% TRUE
  p <- if (length(bh)) {
    last <- bh[length(bh)]; nxt <- rows[match(last, rows) + 1L]
    zone <- c(rows[x$dtop[rows] <= x$dtop[last]], if (!is.na(nxt)) nxt)
    if (any(gl[zone])) TRUE else if (any(is.na(r3[zone]))) NA else FALSE
  } else if (any(is.na(k$h))) NA else FALSE
  .cpcs_sub_result("podzol_gley_cpcs", p, x, list(kind = kind), "VIII/311, p. 54")
}

# ---- peats and humic hydromorphic soils (XI-1, XI-2) -------------------------------------------

#' @rdname cpcs1967_subgroups
#' @export
peat_trophic_cpcs <- function(pedon, kind) {
  x <- .cpcs_profile(pedon)
  # the thickness-weighted pH(H2O) of the peat layers of the top 80 cm of
  # the ground: under 5.5 oligotrophic; with no pH, peat_moor_type (high-moor
  # oligotrophic, low-moor meso- and eutrophic) (Hugo, 2026-11-01)
  g <- .cpcs_from_ground(x)
  to <- min(80, g$rock_top, na.rm = TRUE)
  pr <- which(.cpcs_peat_rows(x) %in% TRUE & !is.na(g$dtop))
  th <- vapply(pr, function(i) max(0, min(if (is.na(g$dbot[i])) to else g$dbot[i], to) - max(g$dtop[i], 0)), 0)
  pr <- pr[th > 0]; th <- th[th > 0]
  ph <- .cpcs_num(x$h, "ph_h2o")[pr]
  k <- !is.na(ph)
  m <- if (any(k)) sum(ph[k] * th[k]) / sum(th[k]) else NA_real_
  oligo <- if (!is.na(m)) m < 5.5 else {
    mt <- .scsj_txt(x$h, "peat_moor_type")[pr]
    hi <- sum(th[!is.na(mt) & grepl("high|haut|ombro|bog", mt)]); lo <- sum(th[!is.na(mt) & grepl("low|bas|fen|minero", mt)])
    tr <- sum(th[!is.na(mt) & grepl("trans", mt)])
    if (hi > lo + tr) TRUE else if (lo > hi + tr) FALSE else NA
  }
  p <- if (identical(kind, "oligotrophic")) oligo else .cpcs_not(oligo)
  .cpcs_sub_result("peat_trophic_cpcs", p, x, list(peat_ph = m), "XI-1, p. 74")
}

#' @rdname cpcs1967_subgroups
#' @export
anmoor_cpcs <- function(pedon, kind) {
  x <- .cpcs_profile(pedon)
  # the humic run from the mineral surface (8 to 30 % OM): anmoor with a C/N
  # under 20, hydromor with 20 or more; a recorded humus form decides first.
  # Calcic: carbonate 1 % or more, or an effervescence, in the run; with
  # neither recorded, pH 7 or more (Hugo, 2026-11-01)
  mrows <- .cpcs_mineral_rows(x); run <- integer(0)
  for (i in mrows) { if (!is.na(x$om[i]) && x$om[i] >= 8 && x$om[i] < 30) run <- c(run, i) else break }
  n <- .cpcs_num(x$h, "n_total_pct")
  oc <- .cpcs_wmean(x, x$oc, run); nn <- .cpcs_wmean(x, n, run)
  cn <- if (!length(run) || is.na(oc) || is.na(nn) || nn <= 0) NA_real_ else oc / nn
  hf <- .cpcs_humus_form(x)
  hyd_form <- if (is.na(hf)) NA else if (grepl("hydromor\\b", hf)) TRUE else if (grepl("anmoor", hf)) FALSE else NA
  hydromor <- if (!is.na(hyd_form)) hyd_form else if (is.na(cn)) NA else cn >= 20
  cc <- .cpcs_num(x$h, "caco3_pct")[run]; ef <- .scsj_txt(x$h, "effervescence_class")[run]
  efp <- !is.na(ef) & grepl("slight|weak|moderate|strong|violent|faible|forte|moyenne|vive", ef) &
    !grepl("^(none|no|not|non|absent|nil|0)", ef)
  ph <- .cpcs_wmean(x, .cpcs_num(x$h, "ph_h2o"), run)
  calcic <- if (any(cc >= 1, na.rm = TRUE) || any(efp)) TRUE else if (any(!is.na(cc)) || any(!is.na(ef))) FALSE else
    if (is.na(ph)) NA else ph >= 7
  p <- switch(kind,
    hydromor = hydromor,
    acid = .cpcs_and(.cpcs_not(hydromor), .cpcs_not(calcic)),
    calcic = .cpcs_and(.cpcs_not(hydromor), calcic),
    NA)
  .cpcs_sub_result("anmoor_cpcs", p, x, list(c_n = cn, humus_form = hf, calcic = calcic), "XI/21, p. 75")
}

# ---- mineral hydromorphic soils (XI-3) ----------------------------------------------------------

#' @rdname cpcs1967_subgroups
#' @export
gley_depth_cpcs <- function(pedon, below = NA, at_least = NA) {
  x <- .cpcs_profile(pedon)
  # the top of the first reduced row; a row above that may be reduced leaves
  # the deep gley undecided
  fr <- .cpcs_first_reduced(x)
  d <- if (is.na(fr$row)) NA_real_ else x$dtop[fr$row]
  open <- fr$open_top
  p <- if (!is.na(below)) {
    if (!is.na(d) && d < below) TRUE else if (!is.na(open) && open < below) NA else FALSE
  } else {
    if (!is.na(open) && open < at_least) NA else !is.na(d) && d >= at_least
  }
  .cpcs_sub_result("gley_depth_cpcs", p, x, list(gley_top_cm = d), "XI/311-312, p. 75")
}

#' @rdname cpcs1967_subgroups
#' @export
pseudogley_kind_cpcs <- function(pedon, kind) {
  x <- .cpcs_profile(pedon)
  rows <- .cpcs_mineral_rows(x); rows <- rows[!x$rock[rows] & !is.na(x$dtop[rows])]
  if (identical(kind, "surface")) {
    # hydromorphic signs already in the first mineral row (Hugo, 2026-11-01)
    p <- length(rows) > 0L && isTRUE(.cpcs_signs(x)$sign[rows[1L]])
  } else {
    # a perched water table recorded, or a slowly permeable layer directly
    # under a pseudogley row: a clay step of the argic horizon, a fragipan, a
    # cemented layer (Hugo, 2026-11-01)
    gw <- .cpcs_gw_kind(x)
    hyd <- .cpcs_hydromorphic_rows(x)
    cem <- .scsj_txt(x$h, "cementation_class"); pan <- .scsj_txt(x$h, "pan_type")
    slow <- function(i, j) isTRUE(.cpcs_argic_step(x$clay[i], x$clay[j])) || isTRUE(x$des$x[j]) ||
      isTRUE(x$des$m[j]) || (!is.na(cem[j]) && grepl("moderat|strong|indurat", cem[j])) ||
      (!is.na(pan[j]) && grepl("fragipan|duripan|clay ?pan|densipan", pan[j]))
    perched <- !is.na(gw) && grepl("perch|suspend", gw)
    p <- perched || any(vapply(seq_along(rows), function(k)
      k < length(rows) && isTRUE(hyd[rows[k]]) && slow(rows[k], rows[k + 1L]), NA))
  }
  .cpcs_sub_result("pseudogley_kind_cpcs", p, x, list(kind = kind), "XI/321-322, p. 76")
}

#' @rdname cpcs1967_subgroups
#' @export
amphigley_kind_cpcs <- function(pedon, kind) {
  x <- .cpcs_profile(pedon)
  # a layer without hydromorphy between the pseudogley and the gley: two
  # water tables; the pseudogley passing straight into the gley (g over G,
  # or an FAO g over the gley): a strongly fluctuating one (Hugo, 2026-11-01)
  am <- .cpcs_amphigley_parts(x)
  p <- if (identical(kind, "two_tables")) am$sep else !am$sep && am$notation
  .cpcs_sub_result("amphigley_kind_cpcs", p, x, list(kind = kind), "XI/341-342, p. 76")
}

# ---- sodic soils (XII) ------------------------------------------------------------------------

#' @rdname cpcs1967_subgroups
#' @export
solonchak_surface_cpcs <- function(pedon, kind) {
  x <- .cpcs_profile(pedon)
  # the surface of the Solontchak; nothing recorded, no subgroup (Hugo,
  # 2026-11-01)
  rows <- .cpcs_mineral_rows(x); rows <- rows[!is.na(x$dtop[rows])]
  first <- rows[seq_len(min(1L, length(rows)))]
  top <- rows[x$dtop[rows] < 10]
  sc <- .cpcs_drop_negated(.cpcs_lc(x$site$surface_condition %||% character(0)))
  scp <- .cpcs_lc(.scsj_txt(x$h, "salt_crust_pattern")[top])
  p <- switch(kind,
    efflorescence = grepl("efflorescen", scp) || grepl("efflorescen", sc),
    friable = any(.nzsc_yes(.scsj_col(x$h, "surface_puff_layer", NA)[top]) %in% TRUE) ||
      any(grepl("^(loose|very friable|friable)$|meuble|tr\u00e8s friable|^friable", .scsj_txt(x$h, "consistence_moist")[first])) ||
      grepl("puff|friable|souffl", sc),
    crust = grepl("crust|hardpan|cro\u00fbte|croute", scp) ||
      any(grepl("evapor", .scsj_txt(x$h, "surface_crust_type")[top])) ||
      grepl("salt crust|saline crust|cro\u00fbte saline|encro\u00fbtement salin|croute saline", sc),
    NA)
  .cpcs_sub_result("solonchak_surface_cpcs", isTRUE(p), x, list(kind = kind), "XII/11, p. 78")
}

#' @rdname cpcs1967_subgroups
#' @export
acid_sulphate_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # soilKey's thionic horizon, or pH(H2O) under 4 with jarosite or sulfidic
  # S in the same row within 100 cm (Hugo, 2026-11-01)
  rows <- .cpcs_rows_within(x, 100)
  ph <- .cpcs_num(x$h, "ph_h2o")[rows]
  jar <- .nzsc_yes(.scsj_col(x$h, "jarosite_present", NA)[rows]) %in% TRUE
  sul <- (.cpcs_num(x$h, "sulfidic_s_pct")[rows] > 0) %in% TRUE
  p <- isTRUE(tryCatch(thionic(pedon)$passed, error = function(e) NA)) || any(!is.na(ph) & ph < 4 & (jar | sul))
  .cpcs_sub_result("acid_sulphate_cpcs", p, x, where = "XII/114, p. 78")
}

#' @rdname cpcs1967_subgroups
#' @export
solod_cpcs <- function(pedon, kind) {
  x <- .cpcs_profile(pedon)
  # Na above 10 % of the CEC in the first B row under the bleached A2: a
  # solodised Solonetz; 10 % or less: a solod (Hugo, 2026-11-01)
  e <- .cpcs_e_rows(x); b <- .cpcs_group_b_rows(x); b <- b[!is.na(x$dtop[b])]
  base <- if (length(e)) max(ifelse(is.na(x$dbot[e]), x$dtop[e], x$dbot[e]), na.rm = TRUE) else -Inf
  bb <- b[x$dtop[b] >= base]
  v <- if (length(bb)) .cpcs_esp(x)[bb[1L]] else NA_real_
  p <- if (is.na(v)) NA else if (identical(kind, "solod")) v <= 10 else v > 10
  .cpcs_sub_result("solod_cpcs", p, x, list(esp_first_B = v), "XII/23, p. 79")
}

#' @rdname cpcs1967_subgroups
#' @export
solonetz_structure_cpcs <- function(pedon, kind) {
  x <- .cpcs_profile(pedon)
  # the structure of the Solonetz B (the B rows with Na above 10 % of the
  # CEC, else all the B rows): columnar, or prismatic or massive
  b <- .cpcs_b_rows(x); esp <- .cpcs_esp(x)
  sb <- b[!is.na(esp[b]) & esp[b] > 10]; if (!length(sb)) sb <- b
  st <- .scsj_txt(x$h, "structure_type")[sb]; gr <- .scsj_txt(x$h, "structure_grade")[sb]
  col <- !is.na(st) & grepl("column|colonn", st)
  pm <- (!is.na(st) & grepl("prism|massive|massif", st)) | (!is.na(gr) & grepl("structureless", gr))
  rec <- !is.na(st) | !is.na(gr)
  p <- if (!any(rec)) NA else if (identical(kind, "columnar")) any(col) else any(pm)
  .cpcs_sub_result("solonetz_structure_cpcs", p, x, list(kind = kind), "XII/22, p. 79")
}

#' @rdname cpcs1967_subgroups
#' @export
solonchak_solonetz_cpcs <- function(pedon, kind) {
  x <- .cpcs_profile(pedon)
  # the surface structure: powdery, single grain, loose or puffed is saline;
  # massive or structureless is moderately saline; otherwise the EC route of
  # class XII decides (met: saline) (Hugo, 2026-11-01)
  rows <- .cpcs_mineral_rows(x); rows <- rows[!is.na(x$dtop[rows])]
  i <- rows[seq_len(min(1L, length(rows)))]
  txt <- .cpcs_lc(c(.scsj_txt(x$h, "structure_type")[i], .scsj_txt(x$h, "structure_grade")[i],
                    .scsj_txt(x$h, "consistence_moist")[i]))
  puff <- isTRUE(.nzsc_yes(.scsj_col(x$h, "surface_puff_layer", NA)[i]))
  saline <- if (puff || grepl("powder|pulv|poudr|single ?grain|loose|meuble|puff", txt)) TRUE else
    if (grepl("massive|massif|structureless", txt)) FALSE else
      isTRUE(sodic_soils_cpcs(pedon)$evidence$ec_above_7_in_solum)
  p <- if (identical(kind, "saline")) saline else !saline
  .cpcs_sub_result("solonchak_solonetz_cpcs", p, x, list(kind = kind), "XII/21, pp. 78-79")
}

# ---- trace notes ------------------------------------------------------------------------------

# After the key: the other subgroups of the group whose tests pass go to the
# trace as "intergrade vers ..."; a brown soil (VII/11) with hydromorphic
# signs only below its (B) gets the "facies hydromorphe" note (p. 46).
.cpcs_subgroup_notes <- function(pedon, res, rules) {
  lv <- res$levels
  # a group with subgroups none of which fits: the name stops at the group,
  # as the text defines no other subgroup (Hugo, 2026-11-01)
  if (inherits(pedon, "PedonRecord") && !is.null(lv) && nrow(lv) == 3L &&
      length(rules$subgroups[[lv$code[3L]]] %||% list()) &&
      !any(grepl("^subgroups:", res$provisional_because)))
    res$warnings <- c(res$warnings, sprintf(
      "Sous-groupe non d\u00e9fini par le texte: no subgroup of %s fits, so the name stops at the group.", lv$code[3L]))
  if (!inherits(pedon, "PedonRecord") || is.null(lv) || nrow(lv) < 4L) return(res)
  grp <- lv$code[3L]; sg <- lv$code[4L]
  taxa <- rules$subgroups[[grp]] %||% list()
  codes <- vapply(taxa, function(t) as.character(t$code), "")
  k <- match(sg, codes)
  if (is.na(k)) return(res)
  hits <- character(0)
  for (t in taxa[seq_along(taxa) > k]) {
    if (isTRUE(t$tests$default)) next
    ok <- tryCatch(evaluate_rsg_tests(pedon, t$tests)$passed, error = function(e) NA)
    if (isTRUE(ok)) hits <- c(hits, sub("{parent}, ", "", as.character(t$name), fixed = TRUE))
  }
  notes <- if (length(hits)) paste0("intergrade vers ", paste(hits, collapse = "; ")) else character(0)
  if (identical(grp, "VII/11")) {
    x <- .cpcs_profile(pedon)
    s <- .cpcs_signs(x)
    rows <- .cpcs_mineral_rows(x); rows <- rows[!is.na(x$dtop[rows])]
    b <- intersect(.cpcs_group_b_rows(x), rows)
    if (length(b)) {
      base <- max(ifelse(is.na(x$dbot[b]), x$dtop[b], x$dbot[b]))
      if (any(s$sign[rows[x$dtop[rows] >= base]]) && !any(s$sign[rows[x$dtop[rows] < base]]))
        notes <- c(notes, "faci\u00e8s hydromorphe: hydromorphic signs only below the (B)")
    }
  }
  if (length(notes) && !is.null(res$trace$subgroups[[sg]])) {
    tr <- res$trace
    tr$subgroups[[sg]]$notes <- c(tr$subgroups[[sg]]$notes, notes)
    res$trace <- tr
  }
  res
}
