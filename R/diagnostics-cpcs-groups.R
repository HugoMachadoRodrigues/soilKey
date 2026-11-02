# =============================================================================
# v0.9.332 -- the 101 groups of the French soil classification (CPCS 1967),
# under the 35 sub-classes. The text defines most groups by a profile type,
# a colour word or a property with no number; soilKey's readings of them are
# Hugo's (2026-11-01) and are listed in readings.yaml. A group the data cannot
# decide leaves the name provisional; a sub-class whose groups all fail stops
# the name at the sub-class. Three-valued. Names carry the _cpcs suffix.
# =============================================================================

# ---- shared readings -------------------------------------------------------------

.cpcs_lc <- function(v) {
  v <- as.character(unlist(v))
  tolower(paste(v[!is.na(v) & nzchar(v)], collapse = " "))
}

# The mineral rows in depth order.
.cpcs_mineral_rows <- function(x) x$ord[!(x$organic[x$ord] %in% TRUE)]

# The thickness of some rows (NA when one has no depth).
.cpcs_thickness <- function(x, rows) {
  if (!length(rows)) return(0)
  t <- x$thick[rows]
  if (any(is.na(t))) NA_real_ else sum(t)
}

# The thickness-weighted mean of a property over some rows (NA when a row
# with a thickness has no value).
.cpcs_wmean <- function(x, v, rows) {
  rows <- rows[!is.na(x$thick[rows]) & x$thick[rows] > 0]
  if (!length(rows) || any(is.na(v[rows]))) return(NA_real_)
  sum(v[rows] * x$thick[rows]) / sum(x$thick[rows])
}

# The parent material named for the site.
.cpcs_parent_txt <- function(x) .cpcs_lc(x$site$parent_material %||% character(0))

.CPCS_CALC_ROCK_RE <- "limestone|chalk|marl|dolom|calcar|calcaire|craie|marne|travertin|tufa|marble|marbre|calcrete"

# ---- the origin of the material (I-1, II-4) ---------------------------------------

.CPCS_ORIGIN_RE <- list(
  anthropic = paste0("anthrop|fill\\b|remblai|d\u00e9blai|deblai|spoil|dredg|made ground|refuse|rubble|tailing|",
                     "artefact|artifact|technogen|garbage|ordure"),
  volcanic = "volcan|tephr|\\bash\\b|cendre|pumic|ponce|scori|lapilli|pyroclast|cinder",
  alluvial = paste0("alluv|fluvi|river|stream|lacustr|lake|marine|\\bmarin|estuar|lagoon|lagun|delta|tidal|",
                    "littoral|beach|lacustre"),
  colluvial = "colluv|solimov|slope deposit|scree|talus|\u00e9boulis|eboulis|hillwash",
  aeolian = "aeol|eolian|eolien|\u00e9olien|dune|wind-?blown|blown sand|cover ?sand")

# The origin of the material: in a stratified profile the material under
# the A within 50 cm decides, else the parent material of the site; the
# order is anthropic (artefacts 20 % or more, or a fill), volcanic,
# alluvial (with lacustrine and marine), colluvial, aeolian. NA: no origin
# recorded, the erosion group (Hugo, 2026-11-01). Glacial deposits and loess
# are not recent deposits (as for class II).
.cpcs_origin_kind <- function(x) {
  a <- .cpcs_a_rows(x)
  rows <- .cpcs_rows_within(x, 50)
  rows <- rows[!x$rock[rows]]
  under <- setdiff(rows, a)
  use <- if (length(under)) under else rows
  if (any(.cpcs_num(x$h, "artefacts_pct")[rows] >= 20, na.rm = TRUE)) return("anthropic")
  pick <- function(txt) {
    txt <- gsub("fluvio-?glac[a-z]*|glacio-?[a-z]*|glacial[a-z]*|\\bloess\\b", " ", txt)
    txt <- .cpcs_drop_negated(txt)
    for (k in names(.CPCS_ORIGIN_RE)) if (grepl(.CPCS_ORIGIN_RE[[k]], txt)) return(k)
    NA_character_
  }
  k <- pick(.cpcs_lc(c(.scsj_txt(x$h, "layer_origin")[use], .scsj_txt(x$h, "rock_origin")[use])))
  if (is.na(k)) k <- pick(.cpcs_parent_txt(x))
  k
}

# ---- hard rock and the cold-desert forms (I, II-1) ---------------------------------

# The top of the hard rock or of a hardened layer, from the mineral surface.
.cpcs_hard_top <- function(x) {
  cem <- .scsj_txt(x$h, "cementation_class")
  hard <- x$rock | (!is.na(cem) & grepl("strong|indurat", cem)) | x$des$m %in% TRUE
  r <- which(hard & !is.na(x$dtop) & !(x$organic %in% TRUE))
  if (length(r)) min(x$dtop[r]) else NA_real_
}

# Patterned ground: geometric figures or a network (polygons, circles,
# nets, stripes, mud boils, wedges, cracks); site$patterned_ground, NA when
# not recorded (Hugo, 2026-11-01). Hummocks, thufurs and palsas alone are
# not a network.
.CPCS_PATTERN_RE <- paste0("polygon|circle|cercle|\\bnets?\\b|network|r\u00e9seau|reseau|stripe|stri[e\u00e9]|",
                           "mud ?boil|ostiole|pustule|wedge|en coin|sorted|fente|crack|fissur|garland|guirlande|",
                           "rosette|roses? de pierre")
.cpcs_patterned <- function(x) {
  v <- .cpcs_lc(x$site$patterned_ground %||% character(0))
  if (!nzchar(trimws(v)) || trimws(v) == "na") return(NA)
  if (trimws(v) %in% c("true", "yes", "present", "oui")) return(TRUE)
  if (trimws(v) %in% c("false", "no", "absent", "non")) return(FALSE)
  # "non-sorted" (circles, polygons) is a kind of patterned ground, not a negation
  v <- gsub("non[- ]?sorted", "nonsorted", v)
  grepl(.CPCS_PATTERN_RE, .cpcs_drop_negated(v))
}

# Strong ice segregation: 20 % ice or more in a layer, ice lenses (horizon
# ice_form), or ice-wedge polygons; NA when none is recorded (Hugo,
# 2026-11-01).
.cpcs_strong_ice <- function(x) {
  # ice-wedge polygons recorded are a strong segregation (II/12; Hugo,
  # 2026-11-01)
  pg <- .cpcs_drop_negated(.cpcs_lc(x$site$patterned_ground %||% character(0)))
  if (grepl("ice[- ]?wedge|wedge[- ]ice|coins? de glace", pg)) return(TRUE)
  ice <- .cpcs_num(x$h, "ice_pct")
  f <- .scsj_txt(x$h, "ice_form")
  lens <- !is.na(f) & grepl("lens|lentil|lenticul|segregat|s\u00e9gr\u00e9g", .cpcs_drop_negated(ifelse(is.na(f), "", f)))
  if (any(ice >= 20, na.rm = TRUE) || any(lens)) return(TRUE)
  if (any(!is.na(ice)) || any(!is.na(f))) FALSE else NA
}

# ---- II-2, II-3 -------------------------------------------------------------------

# Allophane in the top 50 cm: pH NaF 9.5 or more, or Alo + 1/2 Feo 2 % or
# more; with no such data, none without a sign of volcanic material.
.cpcs_allophanic <- function(x) {
  rows <- .cpcs_rows_within(x, 50)
  rows <- rows[!x$rock[rows]]
  if (!length(rows)) return(NA)
  naf <- .cpcs_num(x$h, "ph_naf")[rows]
  alo <- .cpcs_num(x$h, "al_ox_pct")[rows]; feo <- .cpcs_num(x$h, "fe_ox_pct")[rows]
  ox <- alo + 0.5 * ifelse(is.na(feo), 0, feo)
  v <- ifelse(!is.na(naf) & naf >= 9.5, TRUE, ifelse(!is.na(ox) & ox >= 2, TRUE,
       ifelse(is.na(naf) & is.na(ox), NA, FALSE)))
  r <- .cpcs_any(v)
  if (is.na(r) && !.cpcs_volcanic(x)) r <- FALSE
  r
}

# A calcareous rock under the A: carbonate of 2 % or more, effervescence,
# or a calcareous rock named; NA when nothing about it is recorded.
.cpcs_calcareous_rock <- function(x) {
  rows <- .cpcs_mineral_rows(x)
  low <- setdiff(rows, .cpcs_a_rows(x))
  cc <- .cpcs_num(x$h, "caco3_pct")[low]
  ef <- .scsj_txt(x$h, "effervescence_class")[low]
  efp <- !is.na(ef) & grepl("slight|weak|moderate|strong|violent|faible|forte|moyenne|vive", ef) &
    !grepl("^(none|no|not|non|absent|nil|0)", ef)
  txt <- .cpcs_drop_negated(.cpcs_lc(c(.scsj_txt(x$h, "rock_origin")[low], .cpcs_parent_txt(x))))
  if (any(cc >= 2, na.rm = TRUE) || any(efp) || grepl(.CPCS_CALC_ROCK_RE, txt) ||
      any(is.na(cc) & x$des$ca[low] %in% TRUE)) return(TRUE)
  if (any(!is.na(cc)) || any(!is.na(ef)) || nzchar(trimws(txt))) FALSE else NA
}

# ---- structure ------------------------------------------------------------------------

# The structure of one row: TRUE rounded (granular, crumb, nutty, subangular
# blocky), FALSE angular (angular blocky, polyhedral, prismatic, platy,
# columnar, wedge), NA massive, single grain or not recorded.
.cpcs_round_row <- function(s) {
  if (is.na(s)) return(NA)
  # the first structure named decides ("prismatic parting to subangular
  # blocky" is prismatic)
  r <- regexpr("sub-?angul|nucif|nutty|granul|crumb|grumel|grenu", s)
  s2 <- gsub("sub-?angul", "subxxxxx", s)
  a <- regexpr("angular|anguleu|prism|platy|plat|lamel|lamin|column|colonn|wedge|cuneif|lenticul|polyhedr|poly\u00e9dr|polyedr", s2)
  if (r < 0 && a < 0) return(NA)
  if (a < 0) TRUE else if (r < 0) FALSE else r < a
}

# A rounded structure over the top 15 cm (III); a self-mulching surface
# counts as angular (Hugo, 2026-11-01).
.cpcs_rounded_top <- function(x) {
  sc <- .cpcs_drop_negated(.cpcs_lc(x$site$surface_condition %||% character(0)))
  if (grepl("self-?mulch|auto-?division|autodivision", sc)) return(FALSE)
  rows <- .cpcs_rows_within(x, 15)
  rows <- rows[!x$rock[rows]]
  if (!length(rows)) return(NA)
  v <- vapply(.scsj_txt(x$h, "structure_type")[rows], .cpcs_round_row, NA, USE.NAMES = FALSE)
  if (any(v %in% FALSE)) FALSE else if (all(v %in% TRUE)) TRUE else NA
}

# ---- clay and the B (VII, IX, X, XII) -------------------------------------------------

# The B rows read by the groups: in an undesignated profile, a layer of
# albic material is an A2, not a B (as for class VIII).
.cpcs_group_b_rows <- function(x) {
  b <- .cpcs_b_rows(x)
  rows <- .cpcs_mineral_rows(x)
  if (all(is.na(x$role[rows]) | x$role[rows] %in% "other")) b <- setdiff(b, .cpcs_e_rows(x))
  b
}

# The clay index of the text: the largest clay of the B over the smallest
# clay of the A rows above it (A1, A2, A3), with the absolute difference
# (Hugo, 2026-11-01).
.cpcs_clay_index <- function(x) {
  b <- .cpcs_group_b_rows(x); b <- b[!is.na(x$dtop[b])]
  none <- list(r = NA_real_, d = NA_real_, b = b, a = integer(0), bmax = NA_real_, amin = NA_real_)
  if (!length(b)) return(none)
  tb <- min(x$dtop[b])
  rows <- .cpcs_mineral_rows(x)
  a <- rows[!is.na(x$dtop[rows]) & x$dtop[rows] < tb & !x$rock[rows] & !(rows %in% b)]
  none$a <- a
  if (!length(a) || all(is.na(x$clay[a])) || all(is.na(x$clay[b]))) return(none)
  amin <- min(x$clay[a], na.rm = TRUE); bmax <- max(x$clay[b], na.rm = TRUE)
  list(r = if (amin > 0) bmax / amin else Inf, d = bmax - amin, b = b, a = a, bmax = bmax, amin = amin)
}

# Clay films in some rows: TRUE common or more in one of them, FALSE none
# or few recorded in all of them, NA otherwise.
.cpcs_films <- function(x, rows) {
  f <- .scsj_txt(x$h, "clay_films_amount")[rows]
  if (!length(rows)) return(NA)
  many <- !is.na(f) & grepl("common|many|abundant|frequent|continuous|nombreu|abondant|fr\u00e9quent", f) &
    !grepl("discontinu", f)
  if (any(many)) TRUE else if (all(!is.na(f))) FALSE else NA
}

# The leached soils (VII/12, X/36): a textural B, the clay index above 1.4
# with 5 % clay or more; clay films recorded as none or few rule it out, not
# recorded they do not (Hugo, 2026-11-01). Without clay, the notation gives
# the profile type: a Bt is a textural B, a (B) alone is not.
.cpcs_leached <- function(x) {
  ci <- .cpcs_clay_index(x)
  fl <- .cpcs_films(x, ci$b)
  rows <- .cpcs_mineral_rows(x)
  if (!length(ci$b) && !all(is.na(x$role[rows]) | x$role[rows] %in% "other")) return(FALSE)
  if (is.na(ci$r)) {
    # a Bt, or a plain B or B2 of the CPCS notation (an A B C profile, p. 46),
    # is a textural B; a Bh, Bfe or (B) is not (Hugo, 2026-11-01)
    plain <- isTRUE(x$des$cpcs) & x$role[ci$b] %in% "B" & x$des$benriched[ci$b] %in% TRUE &
      !(x$des$h[ci$b] %in% TRUE) & !(x$des$fe[ci$b] %in% TRUE) & !(x$des$bstruct[ci$b] %in% TRUE)
    if (any(x$des$t[ci$b] %in% TRUE) || any(plain)) return(!isFALSE(fl))
    if (length(ci$b) && all(x$des$bstruct[ci$b] %in% TRUE)) return(FALSE)
    return(NA)
  }
  ci$r > 1.4 && ci$d >= 5 && !isFALSE(fl)
}

# The WRB argic step of clay from an upper to a lower layer: 6 % more under
# 15 % clay, a ratio of 1.4 from 15 to under 50 %, 20 % more from 50 %.
.cpcs_argic_step <- function(up, lo) {
  if (is.na(up) || is.na(lo)) return(NA)
  if (up < 15) lo - up >= 6 else if (up < 50) lo / up >= 1.4 else lo - up >= 20
}

# A compact consistence: very or extremely firm, or very or extremely hard;
# NA when not recorded.
.cpcs_compact_rows <- function(x) {
  cm <- .scsj_txt(x$h, "consistence_moist"); rr <- .scsj_txt(x$h, "rupture_resistance")
  ifelse(is.na(cm) & is.na(rr), NA,
         (!is.na(cm) & grepl("very firm|extremely firm|tr\u00e8s ferme", cm)) |
         (!is.na(rr) & grepl("very hard|extremely hard|tr\u00e8s dur", rr)))
}

# Exchangeable sodium in % of the CEC.
.cpcs_esp <- function(x) {
  na <- .cpcs_num(x$h, "na_cmol"); cec <- .cpcs_num(x$h, "cec_cmol")
  ifelse(!is.na(na) & !is.na(cec) & cec > 0, 100 * na / cec, .cpcs_num(x$h, "esp_pct"))
}

# The depth of the solum: the top of the first C, G or R, else the bottom
# of the deepest row.
.cpcs_solum_depth <- function(x) {
  rows <- .cpcs_mineral_rows(x)
  c <- rows[(x$role[rows] %in% c("C", "R", "G") | x$rock[rows]) & !is.na(x$dtop[rows])]
  if (length(c)) return(min(x$dtop[c]))
  b <- x$dbot[rows]
  if (any(!is.na(b))) max(b, na.rm = TRUE) else NA_real_
}

# ---- colours (VIII) -------------------------------------------------------------------

.cpcs_claric_rows <- function(x) {
  cl <- tryCatch(test_claric_munsell(x$h), error = function(e) NULL)
  vapply(seq_len(x$n), function(i) cl$details[[as.character(i)]]$passed %||% NA, NA)
}

# A g of the notation written as the master horizon (the g that takes the
# place of the A2), not the g suffix of an A2g.
.cpcs_g_master <- function(x) {
  d <- vapply(as.character(.scsj_col(x$h, "designation", NA_character_)), .cpcs_symbol, "", USE.NAMES = FALSE)
  !is.na(d) & grepl("^[IVX0-9]*g", d)
}

# The A2 rows: the designated A2 or E rows that are not a g master; in an
# undesignated profile, a layer of albic material over one that is not.
.cpcs_e_rows <- function(x) {
  rows <- .cpcs_mineral_rows(x)
  gm <- .cpcs_g_master(x)
  e <- rows[x$role[rows] %in% "E" & !gm[rows] & !(x$des$buried[rows] %in% TRUE)]
  if (length(e) || !all(is.na(x$role[rows]) | x$role[rows] %in% "other")) return(e)
  cl <- .cpcs_claric_rows(x)
  rows <- rows[!x$rock[rows]]
  rows[vapply(seq_along(rows), function(k) isTRUE(cl[rows[k]]) && any(cl[rows[seq_along(rows) > k]] %in% FALSE), NA)]
}

# A Munsell value step: the A2 at least one unit paler than the row above
# and the row below, in the dry colours when all three are recorded, else in
# the moist ones (Hugo, 2026-11-01).
.cpcs_paler <- function(x, i, above, below) {
  for (col in c("munsell_value_dry", "munsell_value_moist")) {
    v <- .cpcs_num(x$h, col)
    nb <- c(above, below)
    if (!length(nb) || is.na(v[i]) || any(is.na(v[nb]))) next
    return(all(v[i] >= v[nb] + 1))
  }
  NA
}

.cpcs_hue_chroma <- function(x) {
  hue <- as.character(.scsj_col(x$h, "munsell_hue_moist", NA_character_))
  list(hn = .scsj_hue_num(hue), neu = .scsj_neutral(hue), cr = .cpcs_num(x$h, "munsell_chroma_moist"))
}

# The next mineral row below a row.
.cpcs_next_row <- function(x, i) {
  rows <- .cpcs_mineral_rows(x)
  rows[match(i, rows) + 1L]
}

#' CPCS 1967 group gates (internal)
#'
#' The tests of the 101 groups of the French soil classification (CPCS
#' 1967), read by \code{\link{classify_cpcs1967}} in the order of
#' \code{inst/rules/cpcs1967/groups/}. Each returns a
#' \code{\link{DiagnosticResult}}.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param kind,within,strong,network,above,at_least Arguments written in
#'   the rule files.
#' @return A \code{\link{DiagnosticResult}}.
#' @keywords internal
#' @name cpcs1967_groups
NULL

# ---- I and II: deposits, erosion, rock ------------------------------------------------

#' @rdname cpcs1967_groups
#' @export
deposit_origin_cpcs <- function(pedon, kind) {
  x <- .cpcs_profile(pedon)
  k <- .cpcs_origin_kind(x)
  p <- identical(k, as.character(kind))
  # a recent deposit recorded whose kind cannot be read: the deposit groups
  # are undecided, not the erosion group
  if (is.na(k) && .cpcs_recent_deposit(x)) p <- NA
  .cpcs_sub_result("deposit_origin_cpcs", p, x,
                   list(origin = if (is.na(k)) "none recorded" else k), "I-1, p. 18; II-4, p. 25")
}

#' @rdname cpcs1967_groups
#' @export
rock_near_surface_cpcs <- function(pedon, within = 10) {
  x <- .cpcs_profile(pedon)
  t <- .cpcs_hard_top(x)
  .cpcs_sub_result("rock_near_surface_cpcs", !is.na(t) && t <= within, x, list(rock_top_cm = t), "I/21, I/31, pp. 19-20")
}

#' @rdname cpcs1967_groups
#' @export
patterned_ground_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  .cpcs_sub_result("patterned_ground_cpcs", .cpcs_patterned(x), x, where = "I/23, p. 19")
}

#' @rdname cpcs1967_groups
#' @export
yermic_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  p <- tryCatch(yermic_properties(pedon)$passed, error = function(e) NA)
  .cpcs_sub_result("yermic_cpcs", p, x, where = "I/33, p. 20")
}

#' @rdname cpcs1967_groups
#' @export
takyric_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  p <- tryCatch(takyric_properties(pedon)$passed, error = function(e) NA)
  .cpcs_sub_result("takyric_cpcs", p, x, where = "I/35, p. 20")
}

#' @rdname cpcs1967_groups
#' @export
aeolian_sands_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # dunes: an aeolian deposit (or a dune landform) of sand
  lf <- .cpcs_lc(c(x$site$landform, x$site$landform_element, .scsj_txt(x$h, "aeolian_morphology")))
  aeo <- identical(.cpcs_origin_kind(x), "aeolian") || grepl("dune|erg\\b", .cpcs_drop_negated(lf))
  rows <- .cpcs_rows_within(x, 50); rows <- rows[!x$rock[rows]]
  tex <- vapply(rows, function(i) .cpcs_texture(x, i), "")
  sandy <- if (!length(rows)) NA else .cpcs_all(ifelse(is.na(tex), NA, tex == "sandy"))
  .cpcs_sub_result("aeolian_sands_cpcs", .cpcs_and(aeo, sandy), x, list(aeolian = aeo, sandy = sandy), "I/34, p. 20")
}

#' @rdname cpcs1967_groups
#' @export
ice_segregation_cpcs <- function(pedon, strong = TRUE, network = FALSE) {
  x <- .cpcs_profile(pedon)
  s <- .cpcs_strong_ice(x); n <- .cpcs_patterned(x)
  p <- .cpcs_and(if (isTRUE(strong)) s else .cpcs_not(s), if (isTRUE(network)) n else .cpcs_not(n))
  .cpcs_sub_result("ice_segregation_cpcs", p, x, list(strong_ice = s, network = n), "II-1, pp. 22-23")
}

#' @rdname cpcs1967_groups
#' @export
allophanic_weakly_developed_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  .cpcs_sub_result("allophanic_weakly_developed_cpcs", .cpcs_allophanic(x), x, where = "II/23, p. 24")
}

#' @rdname cpcs1967_groups
#' @export
lithocalcic_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  .cpcs_sub_result("lithocalcic_cpcs", .cpcs_calcareous_rock(x), x, where = "II/22, p. 24")
}

#' @rdname cpcs1967_groups
#' @export
subdesert_grey_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # an arid climate and under 1 % OM in the surface horizons (p. 24)
  a <- .cpcs_a_rows(x)
  om <- if (length(a)) .cpcs_all(ifelse(is.na(x$om[a]), NA, x$om[a] < 1)) else NA
  p <- .cpcs_and(.cpcs_climate(x, "arid"), om)
  .cpcs_sub_result("subdesert_grey_cpcs", p, x, list(om_under_1 = om), "II/31, p. 24", climatic = TRUE)
}

# ---- III, IV ------------------------------------------------------------------------

#' @rdname cpcs1967_groups
#' @export
rounded_surface_structure_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  .cpcs_sub_result("rounded_surface_structure_cpcs", .cpcs_rounded_top(x), x, where = "III, pp. 28-29")
}

#' @rdname cpcs1967_groups
#' @export
saturated_andosol_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # base saturation of 50 % or more in the (B); without it, pH 6 or more
  bs <- .cpcs_bs(x)
  p <- .cpcs_larger_part_b(x, ifelse(is.na(bs), NA, bs >= 50))
  if (is.na(p)) {
    ph <- .cpcs_num(x$h, "ph_h2o")
    p <- .cpcs_larger_part_b(x, ifelse(is.na(ph), NA, ph >= 6))
  }
  .cpcs_sub_result("saturated_andosol_cpcs", p, x, where = "IV/21, p. 33")
}

# ---- V ----------------------------------------------------------------------------

#' @rdname cpcs1967_groups
#' @export
cryptorendzina_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  ty <- .cpcs_horizon_types(x, pedon)
  # dolomite: a dolomitic material named, effervescence only when hot, or
  # carbonate with no or a slight cold effervescence (Hugo, 2026-11-01)
  txt <- .cpcs_drop_negated(.cpcs_lc(c(.scsj_txt(x$h, "rock_origin"), .scsj_txt(x$h, "layer_origin"), .cpcs_parent_txt(x))))
  a <- .cpcs_a_rows(x)
  ef <- .scsj_txt(x$h, "effervescence_class")[a]; cc <- .cpcs_num(x$h, "caco3_pct")[a]
  hot <- any(!is.na(ef) & grepl("hot|chaud", ef))
  weak <- !is.na(ef) & grepl("^(none|no|non|nil|absent|0)|slight|weak|faible|nulle", ef) &
    !grepl("moderate|strong|violent|forte|vive|moyenne", ef)
  dol <- grepl("dolom", txt) || hot || any(!is.na(cc) & cc > 0 & weak)
  p <- .cpcs_and(dol, .cpcs_not(ty$b_any))
  .cpcs_sub_result("cryptorendzina_cpcs", p, x, list(dolomite = dol, no_B = .cpcs_not(ty$b_any)), "V/13, p. 37")
}

#' @rdname cpcs1967_groups
#' @export
rendzina_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  ty <- .cpcs_horizon_types(x, pedon)
  .cpcs_sub_result("rendzina_cpcs", .cpcs_not(ty$b_any), x, list(no_B = .cpcs_not(ty$b_any)), "V/11, p. 36")
}

#' @rdname cpcs1967_groups
#' @export
carbonated_humic_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # OM above 20 % in the A, over a hard carbonate rock (p. 37)
  om <- .cpcs_wmean(x, x$om, .cpcs_a_rows(x))
  r <- which(x$rock)
  txt <- .cpcs_drop_negated(.cpcs_lc(c(.scsj_txt(x$h, "rock_origin")[r], .cpcs_parent_txt(x))))
  rock <- if (!length(r)) FALSE else if (grepl(.CPCS_CALC_ROCK_RE, txt)) TRUE else if (nzchar(trimws(txt))) FALSE else NA
  p <- .cpcs_and(if (is.na(om)) NA else om > 20, rock)
  .cpcs_sub_result("carbonated_humic_cpcs", p, x, list(om_A = om, hard_carbonate_rock = rock), "V/22, p. 37")
}

# A calcareous crust in each row: a Ca of the notation cemented moderately or
# more (or an m), or a calcrete pan recorded.
.cpcs_calc_crust_rows <- function(x) {
  cem <- .scsj_txt(x$h, "cementation_class"); pan <- .scsj_txt(x$h, "pan_type")
  (x$des$ca %in% TRUE & (x$des$m %in% TRUE | (!is.na(cem) & grepl("moderat|strong|indurat", cem)))) |
    (!is.na(pan) & grepl("calcrete|petrocalc|cro\u00fbte|croute|crust|encro\u00fbt", pan))
}

#' @rdname cpcs1967_groups
#' @export
melanised_calcic_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # a fine polyhedral A, a CEC of the clay of 50 or more, on hard rock or a
  # calcareous crust (p. 38)
  a <- .cpcs_a_rows(x)
  st <- .scsj_txt(x$h, "structure_type")[a]; sz <- .scsj_txt(x$h, "structure_size")[a]
  fp <- if (!length(a)) NA else .cpcs_any(vapply(seq_along(a), function(k) {
    poly <- if (is.na(st[k])) NA else
      grepl("angul|polyhedr|poly\u00e9dr|polyedr|blocky", st[k]) && !grepl("sub-?angul", st[k])
    fine <- if (is.na(sz[k])) NA else grepl("fine|fin\\b", sz[k])
    .cpcs_and(poly, fine)
  }, NA))
  cc <- .cpcs_cec_clay(x)[a]
  cec <- if (!length(a) || all(is.na(cc))) NA else .cpcs_all(cc[!is.na(cc)] >= 50)
  crust <- any(.cpcs_calc_crust_rows(x))
  p <- .cpcs_and(fp, cec, any(x$rock) || crust)
  .cpcs_sub_result("melanised_calcic_cpcs", p, x, list(fine_polyhedral_A = fp, cec_clay_50 = cec), "V/23, p. 38")
}

#' @rdname cpcs1967_groups
#' @export
gypseous_rendziniform_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  ty <- .cpcs_horizon_types(x, pedon)
  # an AR or ACR profile: no A3 and no (B); the text's thinness (under
  # 50 cm) is a description (p. 38; Hugo, 2026-11-01)
  rows <- .cpcs_mineral_rows(x)
  p <- .cpcs_and(!any(x$role[rows] %in% "AB"), .cpcs_not(ty$b_any))
  .cpcs_sub_result("gypseous_rendziniform_cpcs", p, x, list(rock_top_cm = .cpcs_hard_top(x)), "V/31, p. 38")
}

# ---- VI ---------------------------------------------------------------------------

# The mean OM of the top 20 cm (thickness-weighted).
.cpcs_om20 <- function(x) {
  rows <- .cpcs_rows_within(x, 20); rows <- rows[!x$rock[rows]]
  if (!length(rows)) return(NA_real_)
  w <- vapply(rows, function(i) max(0, min(if (is.na(x$dbot[i])) 20 else x$dbot[i], 20) - max(x$dtop[i], 0)), 0)
  if (any(is.na(x$om[rows][w > 0])) || sum(w) <= 0) return(NA_real_)
  sum(x$om[rows][w > 0] * w[w > 0]) / sum(w)
}

#' @rdname cpcs1967_groups
#' @export
isohumic_om20_cpcs <- function(pedon, above = NA, at_least = NA) {
  x <- .cpcs_profile(pedon)
  m <- .cpcs_om20(x)
  p <- if (is.na(m)) NA else (is.na(above) || m > above) && (is.na(at_least) || m >= at_least)
  .cpcs_sub_result("isohumic_om20_cpcs", p, x, list(om_top_20 = m), "VI, pp. 41-42")
}

# ---- VII --------------------------------------------------------------------------

#' @rdname cpcs1967_groups
#' @export
textural_b_leached_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  ci <- .cpcs_clay_index(x)
  .cpcs_sub_result("textural_b_leached_cpcs", .cpcs_leached(x), x,
                   list(clay_index = ci$r, clay_increase = ci$d, films = .cpcs_films(x, ci$b)),
                   "VII/12, p. 46; X/36, p. 70")
}

# An A2 tonguing into the B: an E/B, B/E or glossic designation, a tonguing
# lower boundary of an A2, or soilKey's retic properties.
.cpcs_tongues <- function(x, pedon) {
  d <- tolower(as.character(.scsj_col(x$h, "designation", NA_character_)))
  bt <- .scsj_txt(x$h, "boundary_topography")
  any(!is.na(d) & grepl("glossic|tongu|e/b|b/e|a2/b|b/a2|e&b|b&e", d)) ||
    any(x$role %in% "E" & !is.na(bt) & grepl("tongu|glossic|langu|interfing", bt)) ||
    isTRUE(tryCatch(retic_properties(pedon)$passed, error = function(e) NA))
}

#' @rdname cpcs1967_groups
#' @export
sod_podzolic_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # a thick A1 (25 cm or more) and an A2 tonguing into the textural B (p. 48)
  th <- .cpcs_thickness(x, .cpcs_a1_rows(x))
  tongues <- .cpcs_tongues(x, pedon)
  p <- .cpcs_and(if (is.na(th)) NA else th >= 25, tongues)
  .cpcs_sub_result("sod_podzolic_cpcs", p, x, list(a1_cm = th, tongues = tongues), "VII/22, p. 48")
}

# ---- VIII -------------------------------------------------------------------------

#' @rdname cpcs1967_groups
#' @export
podzol_ashy_a2_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # an ashy A2: albic material (Hugo, 2026-11-01)
  e <- .cpcs_e_rows(x)
  cl <- .cpcs_claric_rows(x)
  p <- if (length(e)) .cpcs_any(cl[e]) else FALSE
  .cpcs_sub_result("podzol_ashy_a2_cpcs", p, x, where = "VIII/11, p. 50")
}

#' @rdname cpcs1967_groups
#' @export
podzolic_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  pp <- .cpcs_podzolic_paths(x)
  p <- .cpcs_or(pp$a, pp$b, pp$c)
  .cpcs_sub_result("podzolic_cpcs", p, x, list(pale_A2_over_B = pp$a, A2_over_Bg = pp$b, Ap_over_B = pp$c),
                   "VIII/12, p. 51")
}

# The three paths of the sols podzoliques (p. 51): (a) a pale A2 that is not
# ashy over a B with more than 1 % OM, (b) an A2 directly over a Bg, (c) an Ap
# directly over a podzolic B.
.cpcs_podzolic_paths <- function(x) {
  rows <- .cpcs_mineral_rows(x)
  e <- .cpcs_e_rows(x)
  b <- .cpcs_b_rows(x)
  cl <- .cpcs_claric_rows(x)
  bom <- if (length(b)) .cpcs_any(ifelse(is.na(x$om[b]), NA, x$om[b] > 1)) else FALSE
  # (a) a pale A2 that is not ashy, over a B with more than 1 % OM
  light <- .cpcs_any(vapply(e, function(i) {
    if (isTRUE(cl[i])) return(FALSE)
    k <- match(i, rows)
    above <- rows[seq_along(rows) < k & x$role[rows] %in% c("A", "AB")]
    base <- if (is.na(x$dbot[i])) x$dtop[i] else x$dbot[i]
    below <- b[!is.na(x$dtop[b]) & !is.na(base) & x$dtop[b] >= base]
    if (!length(below)) return(FALSE)
    .cpcs_paler(x, i, if (length(above)) above[length(above)] else integer(0), below[1L])
  }, NA))
  path_a <- .cpcs_and(light, bom)
  # (b) an A2 directly over a Bg (the sols podzoliques a pseudogley)
  path_b <- any(vapply(e, function(i) { j <- .cpcs_next_row(x, i); !is.na(j) && x$role[j] %in% "B" && isTRUE(x$des$g[j]) }, NA))
  # (c) an Ap directly over a podzolic B with more than 1 % OM (the A2 ploughed in)
  path_c <- .cpcs_any(vapply(rows[x$role[rows] %in% "A" & x$des$p[rows] %in% TRUE], function(i) {
    j <- .cpcs_next_row(x, i)
    if (is.na(j) || !(x$role[j] %in% "B")) return(FALSE)
    if (!is.na(x$om[j])) return(x$om[j] > 1)
    if (isTRUE(x$des$fe[j]) || isTRUE(x$des$h[j])) TRUE else NA
  }, NA))
  list(a = path_a, b = path_b, c = path_c)
}

#' @rdname cpcs1967_groups
#' @export
ochre_podzolic_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # no A2 (or a broken one, or one under 5 cm), an ochre B (hue 10YR to 5YR,
  # chroma 6 or more), and never a Bh over 3 cm (p. 52; Hugo, 2026-11-01)
  e <- .cpcs_e_rows(x)
  bt <- .scsj_txt(x$h, "boundary_topography")
  th <- .cpcs_thickness(x, e)
  no_a2 <- if (!length(e) || all(!is.na(bt[e]) & grepl("broken|discontinu", bt[e]))) TRUE else
    if (is.na(th)) NA else th < 5
  b <- .cpcs_b_rows(x)
  hc <- .cpcs_hue_chroma(x)
  och <- ifelse(hc$neu, FALSE, ifelse(is.na(hc$hn) | is.na(hc$cr), NA, hc$hn >= 15 & hc$hn <= 20 & hc$cr >= 6))
  bh <- b[x$des$h[b] %in% TRUE & !(x$des$fe[b] %in% TRUE)]
  bh_th <- .cpcs_thickness(x, bh)
  no_bh <- if (!length(bh)) TRUE else if (is.na(bh_th)) NA else bh_th <= 3
  p <- .cpcs_and(no_a2, if (length(b)) .cpcs_any(och[b]) else FALSE, no_bh)
  .cpcs_sub_result("ochre_podzolic_cpcs", p, x, list(no_A2 = no_a2, no_Bh = no_bh), "VIII/13, p. 52")
}

#' @rdname cpcs1967_groups
#' @export
crypto_podzolic_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  cp <- .cpcs_crypto_paths(x)
  p <- .cpcs_and(cp$thin, .cpcs_or(cp$brown, cp$humic))
  .cpcs_sub_result("crypto_podzolic_cpcs", p, x, list(thin_A2 = cp$thin, brown_B = cp$brown, humic_A1 = cp$humic),
                   "VIII/14, pp. 52-53")
}

# The sols crypto-podzoliques (pp. 52-53): no thick A2 (under 5 cm); a brown
# B (chroma 4 or less) with more than 2 % OM, or an A1 of 30 to 60 cm with
# more than 10 % OM.
.cpcs_crypto_paths <- function(x) {
  e <- .cpcs_e_rows(x)
  th <- .cpcs_thickness(x, e)
  thin <- if (!length(e)) TRUE else if (is.na(th)) NA else th < 5
  b <- .cpcs_b_rows(x)
  cr <- .cpcs_num(x$h, "munsell_chroma_moist")
  brown <- if (!length(b)) FALSE else .cpcs_any(vapply(b, function(i)
    .cpcs_and(if (is.na(cr[i])) NA else cr[i] <= 4, if (is.na(x$om[i])) NA else x$om[i] > 2), NA))
  a1 <- .cpcs_a1_rows(x)
  a1th <- .cpcs_thickness(x, a1); a1om <- .cpcs_wmean(x, x$om, a1)
  humic <- if (!length(a1)) FALSE else .cpcs_and(if (is.na(a1th)) NA else a1th >= 30 && a1th <= 60,
                                                if (is.na(a1om)) NA else a1om > 10)
  list(thin = thin, brown = brown, humic = humic)
}

#' @rdname cpcs1967_groups
#' @export
boreal_podzol_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # cold by latitude: Koppen D with c or d, or a latitude of 50 degrees or
  # more; not at 1,500 m or more below 60 degrees, which is alpine (Hugo,
  # 2026-11-01)
  k <- .cpcs_koppen(x$site); lat <- .cpcs_lat(x$site)
  el <- suppressWarnings(as.numeric(as.character(x$site$elevation_m %||% NA)[1L]))
  high <- if (is.na(el) || el < 1500) FALSE else if (is.na(lat)) NA else abs(lat) < 60
  p <- .cpcs_and(.cpcs_or(if (is.na(k)) NA else grepl("^D[fsw][cd]", k), if (is.na(lat)) NA else abs(lat) >= 50),
                 .cpcs_not(high))
  .cpcs_sub_result("boreal_podzol_cpcs", p, x, where = "VIII/21, p. 54", climatic = TRUE)
}

#' @rdname cpcs1967_groups
#' @export
alpine_podzol_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # cold by altitude: Koppen ET or EF below 60 degrees of latitude, or an
  # elevation of 1,500 m or more (Hugo, 2026-11-01)
  k <- .cpcs_koppen(x$site); lat <- .cpcs_lat(x$site)
  el <- suppressWarnings(as.numeric(as.character(x$site$elevation_m %||% NA)[1L]))
  kp <- if (is.na(k)) NA else .cpcs_and(grepl("^E", k), if (is.na(lat)) NA else abs(lat) < 60)
  p <- .cpcs_or(kp, if (is.na(el)) NA else el >= 1500)
  .cpcs_sub_result("alpine_podzol_cpcs", p, x, list(elevation_m = el), "VIII/22, p. 54", climatic = TRUE)
}

#' @rdname cpcs1967_groups
#' @export
molken_podzol_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # a g in the place of the A2, under a perched water table (p. 54); a
  # recorded phreatic water table rules it out
  rows <- .cpcs_mineral_rows(x)
  g <- any(x$role[rows] %in% "E" & .cpcs_g_master(x)[rows])
  gw <- .cpcs_gw_kind(x)
  ok_gw <- is.na(gw) || grepl(.CPCS_GW_STAGNANT_RE, gw)
  p <- g && !length(.cpcs_e_rows(x)) && ok_gw
  .cpcs_sub_result("molken_podzol_cpcs", p, x, list(g_master = g), "VIII/32, p. 54")
}

# A concretioned or cemented B: iron nodules, a Cn or an m of the notation,
# a cementation moderately or more, or a pan recorded (not a fragipan).
.cpcs_concretioned_b <- function(x) {
  b <- .cpcs_b_rows(x)
  nod <- .cpcs_num(x$h, "ferric_nodules_pct"); cem <- .scsj_txt(x$h, "cementation_class")
  pan <- .scsj_txt(x$h, "pan_type")
  pan <- ifelse(is.na(pan), NA, trimws(.cpcs_drop_negated(pan)))
  pan[!is.na(pan) & (!nzchar(pan) | grepl("^none|fragipan|densipan", pan))] <- NA
  any((!is.na(nod[b]) & nod[b] > 0) | x$des$cn[b] %in% TRUE | x$des$m[b] %in% TRUE |
        (!is.na(cem[b]) & grepl("moderat|strong|indurat", cem[b])) |
        !is.na(pan[b]))
}

#' @rdname cpcs1967_groups
#' @export
tropical_groundwater_podzol_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # tropical, a very thick A2 (50 cm or more), a concretioned or cemented B
  e <- .cpcs_e_rows(x); th <- .cpcs_thickness(x, e)
  conc <- .cpcs_concretioned_b(x)
  p <- .cpcs_and(.cpcs_tropical(x), if (!length(e)) FALSE else if (is.na(th)) NA else th >= 50, conc)
  .cpcs_sub_result("tropical_groundwater_podzol_cpcs", p, x, list(a2_cm = th, concretioned_B = conc), "VIII/33, p. 55",
                   climatic = TRUE)
}

# ---- IX ---------------------------------------------------------------------------

# The leaching of the ferruginous soils (pp. 59-60): the clay index above
# 1.4 with 5 % clay more, and an A of 15 cm or more or a third of the solum
# or more; the C intermediate (under 0.9 x the largest clay of the B) for
# the leached soils, or clay films in the B whatever the A (a thin A with
# visible films is leached, p. 59); the C as clayey and no films for the
# impoverished ones (Hugo, 2026-11-01).
.cpcs_ix_leaching <- function(x) {
  ci <- .cpcs_clay_index(x)
  ath <- .cpcs_thickness(x, ci$a); sol <- .cpcs_solum_depth(x)
  thick <- if (is.na(ath)) NA else ath >= 15 || (!is.na(sol) && sol > 0 && ath >= sol / 3)
  rows <- .cpcs_mineral_rows(x)
  bbot <- if (length(ci$b)) max(x$dtop[ci$b], na.rm = TRUE) else Inf
  cr <- rows[x$role[rows] %in% c("C", "G") & !is.na(x$dtop[rows]) & x$dtop[rows] > bbot & !is.na(x$clay[rows])]
  cint <- if (!length(cr) || is.na(ci$bmax)) NA else x$clay[cr[1L]] < 0.9 * ci$bmax
  idx <- if (is.na(ci$r)) NA else ci$r > 1.4 && ci$d >= 5
  list(idx = idx, thick = thick, c_intermediate = cint, films = isTRUE(.cpcs_films(x, ci$b)), ci = ci)
}

#' @rdname cpcs1967_groups
#' @export
ferruginous_leached_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  l <- .cpcs_ix_leaching(x)
  # clay films in the B make it leached even under a thin A (p. 59)
  p <- .cpcs_and(l$idx, .cpcs_or(l$films, .cpcs_and(l$thick, l$c_intermediate)))
  .cpcs_sub_result("ferruginous_leached_cpcs", p, x, list(clay_index = l$ci$r, A_thick = l$thick,
                   C_intermediate = l$c_intermediate, films = l$films), "IX/12, p. 59")
}

#' @rdname cpcs1967_groups
#' @export
ferruginous_impoverished_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  l <- .cpcs_ix_leaching(x)
  p <- .cpcs_and(l$idx, l$thick, .cpcs_not(l$c_intermediate), !l$films)
  .cpcs_sub_result("ferruginous_impoverished_cpcs", p, x, list(clay_index = l$ci$r, A_thick = l$thick,
                   C_intermediate = l$c_intermediate, films = l$films), "IX/13, p. 60")
}

#' @rdname cpcs1967_groups
#' @export
fersiallitic_calcium_reserve_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # a calcium reserve: carbonate within 100 cm, calcareous fragments or
  # rock, or a (B) of V 95 % or more with pH 7 to 7.5 (pp. 60-61; Hugo,
  # 2026-11-01)
  rows <- .cpcs_rows_within(x, 100)
  cc <- .cpcs_num(x$h, "caco3_pct")[rows]; sc <- .cpcs_num(x$h, "secondary_carbonates_pct")[rows]
  ef <- .scsj_txt(x$h, "effervescence_class")[rows]
  efp <- !is.na(ef) & grepl("slight|weak|moderate|strong|violent|faible|forte|moyenne|vive", ef) &
    !grepl("^(none|no|not|non|absent|nil|0)", ef)
  txt <- .cpcs_drop_negated(.cpcs_lc(c(.scsj_txt(x$h, "rock_origin")[rows], .cpcs_parent_txt(x))))
  carb <- any(cc > 0, na.rm = TRUE) || any(sc > 0, na.rm = TRUE) || any(efp) ||
    any(x$des$ca[rows] %in% TRUE) || grepl(.CPCS_CALC_ROCK_RE, txt)
  bs <- .cpcs_bs(x); ph <- .cpcs_num(x$h, "ph_h2o")
  chem <- .cpcs_larger_part_b(x, vapply(seq_len(x$n), function(i)
    .cpcs_and(if (is.na(bs[i])) NA else bs[i] >= 95, if (is.na(ph[i])) NA else ph[i] >= 7 && ph[i] <= 7.5), NA))
  p <- if (carb) TRUE else if (isTRUE(chem)) TRUE else if (isFALSE(chem)) FALSE else
    if (any(!is.na(cc)) || any(!is.na(ef))) FALSE else NA
  .cpcs_sub_result("fersiallitic_calcium_reserve_cpcs", p, x, list(carbonate = carb, saturated_neutral_B = chem),
                   "IX/21, p. 60")
}

# ---- X ----------------------------------------------------------------------------

#' @rdname cpcs1967_groups
#' @export
ferrallitic_humic_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # a mean OM of 7 % or more over 0-20 cm, or more than 1 % in every horizon
  # to 100 cm (Hugo, 2026-11-01)
  m <- .cpcs_om20(x)
  rows <- .cpcs_rows_within(x, 100); rows <- rows[!x$rock[rows]]
  deep <- if (!length(rows)) NA else .cpcs_all(ifelse(is.na(x$om[rows]), NA, x$om[rows] > 1))
  # 'jusqu'a au moins 1 m' (p. 68): a description that stops short of 100 cm
  # (above any rock) leaves it open
  reach <- suppressWarnings(max(x$dbot[rows], na.rm = TRUE))
  if (isTRUE(deep) && (!is.finite(reach) || reach < min(100, x$rock_top, na.rm = TRUE))) deep <- NA
  p <- .cpcs_or(if (is.na(m)) NA else m >= 7, deep)
  .cpcs_sub_result("ferrallitic_humic_cpcs", p, x, list(om_top_20 = m, om_1_to_100 = deep), "X, pp. 68-70")
}

#' @rdname cpcs1967_groups
#' @export
ferrallitic_impoverished_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # the clay index 1.4 or more, no common or many clay films
  ci <- .cpcs_clay_index(x)
  p <- if (is.na(ci$r)) NA else ci$r >= 1.4 && !isTRUE(.cpcs_films(x, ci$b))
  .cpcs_sub_result("ferrallitic_impoverished_cpcs", p, x, list(clay_index = ci$r), "X, pp. 67-70")
}

#' @rdname cpcs1967_groups
#' @export
ferrallitic_reworked_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  rw <- .cpcs_reworked(x)
  .cpcs_sub_result("ferrallitic_reworked_cpcs", rw$p, x, list(stone_line = rw$line, discontinuity = rw$disc,
                   colluvial = rw$coll), "X, pp. 67-70")
}

# The reworked material (X/13, X/24, X/34): a stone line (15 % coarse
# fragments or more at the base of the A or in the B, more than in the
# recorded rows above and below), a lithological discontinuity, or a
# colluvial origin (Hugo, 2026-11-01); not recorded, none. `cand` restricts
# the rows where the stone line may lie.
.cpcs_reworked <- function(x, cand = NULL) {
  rows <- .cpcs_mineral_rows(x); rows <- rows[!x$rock[rows]]
  cf <- .cpcs_num(x$h, "coarse_fragments_pct")
  a <- .cpcs_a_rows(x)
  if (is.null(cand)) cand <- unique(c(if (length(a)) a[length(a)], .cpcs_b_rows(x)))
  line <- any(vapply(cand, function(i) {
    if (is.na(cf[i]) || cf[i] < 15) return(FALSE)
    k <- match(i, rows); if (is.na(k) || k >= length(rows)) return(FALSE)
    up <- if (k > 1L) cf[rows[k - 1L]] else 0; lo <- cf[rows[k + 1L]]
    !is.na(up) && !is.na(lo) && cf[i] > up && cf[i] > lo
  }, NA))
  txt <- .cpcs_lc(c(.scsj_col(x$h, "designation", NA_character_), .scsj_txt(x$h, "layer_origin")))
  line <- line || grepl("stone ?-?line|ligne (de )?(cailloux|pierres|graviers)|nappe de gravats|gravel line", txt)
  disc <- length(unique(x$des$lith[rows])) > 1L
  coll <- identical(.cpcs_origin_kind(x), "colluvial") ||
    grepl(.CPCS_ORIGIN_RE$colluvial, .cpcs_drop_negated(.cpcs_lc(.scsj_txt(x$h, "layer_origin"))))
  list(p = line || disc || coll, line = line, disc = disc, coll = coll)
}

#' @rdname cpcs1967_groups
#' @export
ferrallitic_rejuvenated_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  .cpcs_sub_result("ferrallitic_rejuvenated_cpcs", .cpcs_weatherable(x, 10), x, where = "X, pp. 68-70")
}

# Weatherable minerals in the sand within 100 cm: feldspars, micas and
# amphiboles at `at_least` % or more, or named in the sand mineralogy without
# 'traces'; not recorded, none (Hugo, 2026-11-01: 10 % for the groups, 3 % for
# the subgroups).
.cpcs_weatherable <- function(x, at_least = 10) {
  rows <- .cpcs_rows_within(x, 100); rows <- rows[!x$rock[rows]]
  s <- cbind(.cpcs_num(x$h, "sand_feldspar_pct"), .cpcs_num(x$h, "sand_mica_pct"), .cpcs_num(x$h, "sand_amphibole_pct"))
  tot <- ifelse(rowSums(!is.na(s)) > 0, rowSums(s, na.rm = TRUE), NA_real_)
  mn <- .scsj_txt(x$h, "sand_mineralogy")[rows]
  mn <- ifelse(is.na(mn), "", gsub("(traces?|few|rare|some|little|peu de|quelques)( of)? [a-z\u00e0-\u00ff]+", " ", .cpcs_drop_negated(mn)))
  any(tot[rows] >= at_least, na.rm = TRUE) || any(grepl("mica|feldsp|anfib|amphib", mn))
}

# ---- XI ---------------------------------------------------------------------------

# The fibre class of a peat row: unrubbed fibre in thirds, else von Post,
# else rubbed fibre as the SiBCS (Hugo, 2026-11-01); with no data, the
# suffix of an FAO or USDA O or H (i fibric, e hemic, a sapric).
.cpcs_fibre_class <- function(x, i) {
  un <- .cpcs_num(x$h, "fiber_content_unrubbed_pct")[i]
  if (!is.na(un)) return(if (un > 200 / 3) "fibric" else if (un >= 100 / 3) "hemic" else "sapric")
  vp <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(.scsj_col(x$h, "von_post_index", NA_character_))[i])))
  if (!is.na(vp)) return(if (vp <= 3) "fibric" else if (vp <= 6) "hemic" else "sapric")
  rb <- .cpcs_num(x$h, "fiber_content_rubbed_pct")[i]
  if (!is.na(rb)) return(if (rb >= 40) "fibric" else if (rb >= 17) "hemic" else "sapric")
  d <- .cpcs_symbol(as.character(.scsj_col(x$h, "designation", NA_character_))[i])
  if (!isTRUE(x$des$cpcs) && !is.na(d)) {
    m <- regmatches(d, regexpr("^[IVX0-9]*[OH][iea]", d))
    if (length(m)) return(switch(substring(m, nchar(m)), i = "fibric", e = "hemic", a = "sapric"))
  }
  NA_character_
}

#' @rdname cpcs1967_groups
#' @export
peat_fibre_cpcs <- function(pedon, kind) {
  x <- .cpcs_profile(pedon)
  # the class with the greatest thickness among the peat layers of the top
  # 80 cm of the ground (the window of the peat, p. 73)
  g <- .cpcs_from_ground(x)
  to <- min(80, g$rock_top, na.rm = TRUE)
  pr <- which(.cpcs_peat_rows(x) %in% TRUE & !is.na(g$dtop))
  th <- vapply(pr, function(i) max(0, min(if (is.na(g$dbot[i])) to else g$dbot[i], to) - max(g$dtop[i], 0)), 0)
  cls <- vapply(pr, function(i) .cpcs_fibre_class(x, i), "")
  kinds <- c("fibric", "hemic", "sapric")
  tot <- vapply(kinds, function(k) sum(th[cls %in% k]), 0)
  u <- sum(th[is.na(cls)])
  kind <- as.character(kind)
  others <- max(tot[kinds != kind])
  # a tie goes to the first class of the key (the fibrous peat)
  best <- kinds[which.max(tot)]
  # with layers of unknown class: decided when the class wins (or loses)
  # whatever the unknown layers are
  ki <- match(kind, kinds)
  beats <- function(a, ia, b, ib) a > b || (a == b && ia < ib)
  win <- all(vapply(setdiff(seq_along(kinds), ki), function(j) beats(tot[[ki]], ki, tot[[j]] + u, j), NA))
  lose <- any(vapply(setdiff(seq_along(kinds), ki), function(j) beats(tot[[j]], j, tot[[ki]] + u, ki), NA))
  p <- if (!length(pr) || sum(th) <= 0) FALSE else if (u == 0) identical(best, kind) else
    if (win) TRUE else if (lose) FALSE else NA
  .cpcs_sub_result("peat_fibre_cpcs", p, x, as.list(c(tot, unknown = u)), "XI-1, p. 74")
}

#' @rdname cpcs1967_groups
#' @export
humic_gley_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # the layer under the humic (8-30 % OM) horizon is a gley (a reduced matrix)
  mrows <- .cpcs_mineral_rows(x)
  run <- integer(0)
  for (i in mrows) { if (!is.na(x$om[i]) && x$om[i] >= 8 && x$om[i] < 30) run <- c(run, i) else break }
  nxt <- if (length(run)) mrows[match(run[length(run)], mrows) + 1L] else NA_integer_
  p <- if (is.na(nxt)) NA else .cpcs_reduced3(x)[nxt]
  .cpcs_sub_result("humic_gley_cpcs", p, x, where = "XI/21, p. 75")
}

#' @rdname cpcs1967_groups
#' @export
iron_pan_hydromorphic_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # a layer cemented by iron within 100 cm: a carapace or a cuirasse
  rows <- .cpcs_rows_within(x, 100)
  pan <- .scsj_txt(x$h, "pan_type")[rows]; cem <- .scsj_txt(x$h, "cementation_class")[rows]
  pl <- .cpcs_num(x$h, "plinthite_pct")[rows]; nod <- .cpcs_num(x$h, "ferric_nodules_pct")[rows]
  iron <- x$des$fe[rows] %in% TRUE | (!is.na(pl) & pl > 0) | (!is.na(nod) & nod >= 5)
  p <- any(!is.na(pan) & grepl("ferricrete|iron ?pan|cuirasse|carapace|laterit|petroplinth|ferrug", pan)) ||
    any(x$des$m[rows] %in% TRUE & iron) ||
    any(!is.na(cem) & grepl("moderat|strong|indurat", cem) & iron) ||
    isTRUE(tryCatch(petroplinthic(pedon)$passed, error = function(e) NA))
  .cpcs_sub_result("iron_pan_hydromorphic_cpcs", p, x, where = "XI/35, p. 76")
}

#' @rdname cpcs1967_groups
#' @export
calcium_redistribution_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # secondary carbonates or visible gypsum, 5 % or more within 100 cm; not
  # measured, a Ca or Cs accumulation of the notation (Hugo, 2026-11-01)
  rows <- .cpcs_rows_within(x, 100)
  sc <- .cpcs_num(x$h, "secondary_carbonates_pct")[rows]; gv <- .cpcs_num(x$h, "gypsum_visible_pct")[rows]
  p <- any(sc >= 5, na.rm = TRUE) || any(gv >= 5, na.rm = TRUE) ||
    any(is.na(sc) & is.na(gv) & (x$des$ca[rows] %in% TRUE | x$des$cs[rows] %in% TRUE))
  .cpcs_sub_result("calcium_redistribution_cpcs", p, x, where = "XI/36, p. 76")
}

# A reduced matrix for the groups, three-valued: TRUE as the class reads it,
# NA where the matrix colour that would decide is not recorded (no G of the
# notation, no dipyridyl test), FALSE otherwise; a g of the CPCS notation
# with no colour is a pseudogley.
.cpcs_reduced3 <- function(x) {
  red <- .cpcs_reduced_rows(x)
  hue <- as.character(.scsj_col(x$h, "munsell_hue_moist", NA_character_))
  v <- .cpcs_num(x$h, "munsell_value_moist"); cr <- .cpcs_num(x$h, "munsell_chroma_moist")
  neu <- .scsj_neutral(hue)
  cr <- ifelse(neu & is.na(cr), 0, cr)
  grey_known <- !is.na(v) & !is.na(cr)
  open <- (!grey_known | (v >= 4 & cr <= 2 & is.na(hue))) & is.na(.scsj_dipyridyl(x$h))
  cpcs_g <- isTRUE(x$des$cpcs) & x$des$g %in% TRUE
  ifelse(red, TRUE, ifelse(open & !cpcs_g & !x$rock & !(x$role %in% c("E", "O")) & !(x$organic %in% TRUE), NA, FALSE))
}

# The first mineral row with a reduced matrix, and whether a row that may be
# reduced lies above it (or, with none, anywhere).
.cpcs_first_reduced <- function(x) {
  r3 <- .cpcs_reduced3(x)
  rows <- .cpcs_mineral_rows(x); rows <- rows[!is.na(x$dtop[rows])]
  f <- rows[r3[rows] %in% TRUE][1L]
  open <- rows[is.na(r3[rows]) & (is.na(f) | x$dtop[rows] < (x$dtop[f] %||% Inf))]
  list(row = f, open_top = if (length(open)) min(x$dtop[open]) else NA_real_, r3 = r3)
}

.cpcs_gw_kind <- function(x) {
  g <- tolower(trimws(as.character(x$site$groundwater_kind %||% NA)[1L]))
  if (is.na(g) || !nzchar(g) || g == "na") NA_character_ else g
}

# A water table that is perched, or none: the stagnogley's (p. 76).
.CPCS_GW_STAGNANT_RE <- "perch|suspend|^none|no water|not observed|absent|aucune"

# The stagnogley morphology: a reduced matrix just under the A, over the
# larger part of the profile from the base of the A to 100 cm.
.cpcs_stagno_morph <- function(x) {
  base <- .cpcs_a1_base(x)
  r3 <- .cpcs_reduced3(x)
  rows <- .cpcs_mineral_rows(x)
  nxt <- rows[!(rows %in% .cpcs_a1_rows(x))][1L]
  to <- min(100, x$rock_top, na.rm = TRUE)
  if (is.na(base) || is.na(nxt)) return(if (is.na(nxt)) FALSE else NA)
  if (base >= to) return(FALSE)
  cu <- .scsj_cum(x, r3, base, to)
  maj <- if (cu$sure > (to - base) / 2) TRUE else if (cu$poss <= (to - base) / 2) FALSE else NA
  .cpcs_and(r3[nxt], maj)
}

#' @rdname cpcs1967_groups
#' @export
amphigley_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # a pseudogley (mottled, not reduced) over a deep gley, starting at 80 cm
  # or deeper (the 'gley profond' of XI/312, p. 75). The two are told apart
  # from the oxidised top of one gley by a layer without hydromorphy between
  # them, or by the notation: a g of the CPCS notation over a G, or in a
  # declared FAO notation a g (stagnic) rather than an l (the capillary
  # fringe of the gley); continuous mottling over the reduced matrix is the
  # gley's own (Hugo, 2026-11-01)
  am <- .cpcs_amphigley_parts(x)
  .cpcs_sub_result("amphigley_cpcs", am$p, x, list(separating_layer = am$sep, notation = am$notation),
                   "XI/34, p. 76")
}

# The amphigley: the result, and the two ways of telling the pseudogley from
# the gley (a separating layer without hydromorphy; the notation).
.cpcs_amphigley_parts <- function(x) {
  fr <- .cpcs_first_reduced(x); f <- fr$row
  hyd <- .cpcs_hydromorphic_rows(x)
  rows <- .cpcs_mineral_rows(x); rows <- rows[!is.na(x$dtop[rows])]
  p <- FALSE; sep <- FALSE; nota <- FALSE
  ds <- tolower(as.character(x$site$designation_system %||% NA)[1L])
  fao <- !is.na(ds) && grepl("fao|wrb", ds)
  suf <- vapply(as.character(.scsj_col(x$h, "designation", NA_character_)), function(d) {
    sy <- .cpcs_symbol(d); if (is.na(sy)) "" else gsub("[^a-z]", "", sub("^[IVX0-9]*[A-Z]+", "", sy)) }, "", USE.NAMES = FALSE)
  if (!is.na(f) && x$dtop[f] >= 80) {
    above <- rows[x$dtop[rows] < x$dtop[f]]
    ps <- above[hyd[above] %in% TRUE & fr$r3[above] %in% FALSE]
    if (length(ps)) {
      last <- ps[length(ps)]
      between <- above[x$dtop[above] > x$dtop[last]]
      sep <- any(hyd[between] %in% FALSE)
      nota <- (isTRUE(x$des$cpcs) && any(x$des$g[ps] %in% TRUE) && isTRUE(x$des$G[f])) ||
        (fao && any(grepl("g", suf[ps]) & !grepl("l", suf[ps])))
      p <- sep || nota
      # a row above that may be reduced may start the gley higher
      if (p && !is.na(fr$open_top) && fr$open_top < 80) p <- NA
    }
  }
  list(p = p, sep = sep, notation = nota)
}

#' @rdname cpcs1967_groups
#' @export
gley_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # a reduced matrix starting within 130 cm (p. 75). A perched water table
  # is a stagnogley's; a water table recorded as none, or not recorded, is
  # one too when the profile has the stagnogley morphology (Hugo, 2026-11-01)
  fr <- .cpcs_first_reduced(x)
  red <- if (!is.na(fr$row) && x$dtop[fr$row] < 130) TRUE else if (!is.na(fr$open_top) && fr$open_top < 130) NA else FALSE
  gw <- .cpcs_gw_kind(x)
  st <- .cpcs_stagno_morph(x)
  ok <- if (is.na(gw)) { if (isFALSE(st)) TRUE else NA } else
    if (grepl("perch|suspend", gw)) FALSE else
      if (grepl(.CPCS_GW_STAGNANT_RE, gw)) { if (isTRUE(st)) FALSE else if (is.na(st)) NA else TRUE } else TRUE
  .cpcs_sub_result("gley_cpcs", .cpcs_and(red, ok), x, list(groundwater = gw, stagnogley_morphology = st), "XI/31, p. 75")
}

#' @rdname cpcs1967_groups
#' @export
stagnogley_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # the stagnogley morphology, with a perched water table, none, or none
  # recorded (p. 76)
  gw <- .cpcs_gw_kind(x)
  ok <- is.na(gw) || grepl(.CPCS_GW_STAGNANT_RE, gw)
  .cpcs_sub_result("stagnogley_cpcs", .cpcs_and(.cpcs_stagno_morph(x), ok), x, list(groundwater = gw), "XI/33, p. 76")
}

# ---- XII --------------------------------------------------------------------------

#' @rdname cpcs1967_groups
#' @export
solodised_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # an albic A2 over a B with the clay increase of the WRB argic horizon, and
  # an acid surface (pH under 7); with no pH, undecided (p. 79; Hugo,
  # 2026-11-01)
  e0 <- .cpcs_e_rows(x); cl <- .cpcs_claric_rows(x)
  b <- .cpcs_group_b_rows(x)
  over_b <- function(e) {
    if (!length(e)) return(FALSE)
    emin <- if (any(!is.na(x$clay[e]))) min(x$clay[e], na.rm = TRUE) else NA_real_
    etop <- if (any(!is.na(x$dtop[e]))) min(x$dtop[e], na.rm = TRUE) else NA_real_
    below <- b[!is.na(x$dtop[b]) & !is.na(etop) & x$dtop[b] > etop]
    if (!length(below)) FALSE else .cpcs_any(vapply(below, function(i) .cpcs_argic_step(emin, x$clay[i]), NA))
  }
  # an A2 whose albic test is undecided may still be the bleached horizon
  s1 <- over_b(e0[cl[e0] %in% TRUE]); s2 <- over_b(e0[is.na(cl[e0])])
  step <- if (isTRUE(s1)) TRUE else if (isFALSE(s1) && isFALSE(s2)) FALSE else NA
  rows <- .cpcs_mineral_rows(x)
  ph <- .cpcs_num(x$h, "ph_h2o")[rows[1L]]
  acid <- if (is.na(ph)) NA else ph < 7
  p <- .cpcs_and(step, acid)
  .cpcs_sub_result("solodised_cpcs", p, x, list(albic_A2_over_B = step, surface_ph = ph), "XII/23, p. 79")
}

#' @rdname cpcs1967_groups
#' @export
solonetz_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # a B with the clay increase of the WRB argic horizon over the layers
  # above, compact, with Na above 10 % of the CEC (pp. 78-79)
  b <- .cpcs_b_rows(x); b <- b[!is.na(x$dtop[b])]
  rows <- .cpcs_mineral_rows(x)
  comp <- .cpcs_compact_rows(x); esp <- .cpcs_esp(x)
  # without a consistence, a columnar B is very compact (Hugo, 2026-11-01)
  st <- .scsj_txt(x$h, "structure_type")
  comp <- ifelse(is.na(comp) & !is.na(st) & grepl("column|colonn", st), TRUE, comp)
  p <- if (!length(b)) FALSE else .cpcs_any(vapply(b, function(i) {
    up <- rows[!is.na(x$dtop[rows]) & x$dtop[rows] < x$dtop[i] & !(rows %in% b)]
    upc <- if (length(up) && any(!is.na(x$clay[up]))) min(x$clay[up], na.rm = TRUE) else NA_real_
    .cpcs_and(.cpcs_argic_step(upc, x$clay[i]), comp[i], if (is.na(esp[i])) NA else esp[i] > 10)
  }, NA))
  .cpcs_sub_result("solonetz_cpcs", p, x, where = "XII/22, p. 79")
}
