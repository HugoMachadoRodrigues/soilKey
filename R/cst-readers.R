# =============================================================================
# v0.9.370 -- the shared readers of the Chinese Soil Taxonomy (Cooperative
# Research Group on Chinese Soil Taxonomy, ISSAS. Chinese Soil Taxonomy.
# Science Press, Beijing and New York, 2001; the English edition, system id
# cst2001).
#
# The book writes its criteria in g/kg of the element, per kg of clay, as
# weighted means of a mixed surface, as storages in kg/m2 with its own default
# bulk densities, by Munsell hue ("5YR or redder") and by shares of the
# horizons of a window. The readers here turn the recorded data into those
# quantities, three-valued (a number, TRUE, FALSE, or NA when the data leave
# it open), and say what they read: the attributes that would decide an
# undecided answer (`missing`), the fallbacks they used (`flags`) and a note
# in soilKey's words (`note`). The diagnostics of later parts call them; the
# numbers come from inst/rules/cst2001/thresholds.yaml.
#
# Hugo's decisions of 2026-11-04 read here (inst/rules/cst2001/readings.yaml):
#   Q10 the book's default bulk densities, by horizon and texture, flagged;
#   Q14 the arid region from the regional or the soil's regime, else the
#     aridity index; unknown otherwise;
#   Q16 methods: a CEC by another method stands in, flagged;
#   Q21 the moist hue, the dry one as a flagged fallback;
#   Q22 shares of a window by thickness;
#   Q23 "throughout a window": every horizon that touches it;
#   defaults 1 (three values), 9 (units, the mixed surface).
# =============================================================================

.cst_cache <- new.env(parent = emptyenv())

# The numbers of thresholds.yaml, read once.
.cst_thresholds <- function() {
  if (is.null(.cst_cache$thr)) {
    f <- system.file("rules", "cst2001", "thresholds.yaml", package = "soilKey")
    if (!nzchar(f)) rlang::abort("inst/rules/cst2001/thresholds.yaml not found.")
    .cst_cache$thr <- yaml::read_yaml(f)
  }
  .cst_cache$thr
}

# One number (or list) of thresholds.yaml by its path of names; an entry
# written as {value: ..., page: ...} gives its value.
.cst_thr <- function(...) {
  path <- c(...)
  x <- .cst_thresholds()
  for (k in path) {
    x <- x[[k]]
    if (is.null(x)) {
      rlang::abort(sprintf("thresholds.yaml has no entry %s.", paste(path, collapse = "$")))
    }
  }
  if (is.list(x) && !is.null(x$value)) x$value else x
}

# A reader's answer.
.cst_ans <- function(value = NA, missing = character(0), flags = character(0),
                     note = character(0), source = NA_character_, ...) {
  undecided <- length(value) == 1L && is.atomic(value) && is.na(value)
  c(list(value = value, source = source,
         flags = unique(as.character(flags[!is.na(flags)])),
         missing = if (undecided) unique(as.character(missing)) else character(0),
         note = unique(as.character(note[!is.na(note)]))),
    list(...))
}

# Three-valued AND / OR of plain logicals.
.cst_and <- function(...) {
  v <- c(...)
  if (any(v %in% FALSE)) FALSE else if (length(v) && all(v %in% TRUE)) TRUE else NA
}
.cst_or <- function(...) {
  v <- c(...)
  if (any(v %in% TRUE)) TRUE else if (length(v) && all(v %in% FALSE)) FALSE else NA
}


# --- the site -----------------------------------------------------------------

# A site property as lower-case text (NA when not recorded).
.cst_site_txt <- function(site, name) {
  v <- (site %||% list())[[name]]
  if (is.null(v) || !length(v)) return(NA_character_)
  v <- tolower(trimws(as.character(unlist(v))[1L]))
  if (is.na(v) || !nzchar(v) || v %in% c("na", "nan", "null")) NA_character_ else v
}

# A site property as a number (a decimal comma read too).
.cst_site_num <- function(site, name) {
  v <- (site %||% list())[[name]]
  if (is.null(v) || !length(v)) return(NA_real_)
  suppressWarnings(as.numeric(sub(",", ".", as.character(unlist(v))[1L], fixed = TRUE)))
}

# A site property as TRUE / FALSE / NA (yes / no words read).
.cst_site_lgl <- function(site, name) {
  v <- (site %||% list())[[name]]
  if (is.null(v) || !length(v)) return(NA)
  x <- unlist(v)[1L]
  if (is.logical(x)) return(as.logical(x))
  if (is.numeric(x)) return(if (is.na(x)) NA else x != 0)
  x <- tolower(trimws(as.character(x)))
  if (x %in% c("true", "t", "yes", "y", "1")) TRUE
  else if (x %in% c("false", "f", "no", "n", "0")) FALSE else NA
}

# Paddy land: flood_irrigated, or a land use that names paddy or irrigated
# rice as whole words (English, Chinese, Japanese, Indonesian sawah,
# Portuguese and Spanish arroz irrigado). Upland, dryland and rainfed rice
# and uses of rice straw or mulch are not paddy. TRUE / FALSE (another land
# use, or flood_irrigated FALSE) / NA (nothing recorded). Hugo's decisions
# Q12 and D5 (2026-11-05).
.cst_paddy <- function(site) {
  fi <- .cst_site_lgl(site, "flood_irrigated")
  if (isTRUE(fi)) return(.cst_ans(TRUE, source = "flood_irrigated"))
  lu <- .cst_site_txt(site, "land_use")
  if (is.na(lu)) {
    return(.cst_ans(if (isFALSE(fi)) FALSE else NA,
                    missing = c("flood_irrigated", "land_use")))
  }
  lu2 <- gsub(paste0("\\b(upland|dryland|dry-land|rainfed|rain-fed)\\s+rice\\b|",
                     "\\brice\\s+(straw|husks?|hulls?|mulch)\\b|",
                     "\u65f1\u7a3b|\u9646\u7a3b|\u9678\u7a32|\u7a3b\u8349|\u7a32\u308f\u3089"),
              " ", lu, perl = TRUE)
  .cst_ans(grepl(paste0("\\b(rice|paddy|paddies|sawah)\\b|\\barroz\\s+(irrigado|inundado|de riego)\\b|",
                        "\u6c34\u7530|\u6c34\u7a3b|\u7a3b\u7530|\u7a3b|\u7a32"), lu2, perl = TRUE),
           source = "land_use")
}

# Irrigated land: site$irrigated, which site$flood_irrigated TRUE implies
# (default 10 of 2026-11-04).
.cst_irrigated <- function(site) {
  if (isTRUE(.cst_site_lgl(site, "flood_irrigated")))
    return(.cst_ans(TRUE, source = "flood_irrigated"))
  ir <- .cst_site_lgl(site, "irrigated")
  .cst_ans(ir, source = if (is.na(ir)) NA_character_ else "irrigated",
           missing = c("irrigated", "flood_irrigated"))
}


# --- units --------------------------------------------------------------------

# g/kg from soilKey's % by mass, and back (p. 20).
.cst_gkg <- function(pct) as.numeric(pct) * .cst_thr("units", "gkg_per_pct")
.cst_pct <- function(gkg) as.numeric(gkg) / .cst_thr("units", "gkg_per_pct")

# A column of the horizons, NA when the column is absent.
.cst_col <- function(h, name) {
  v <- h[[name]]
  if (is.null(v)) rep(NA, nrow(h)) else v
}
.cst_num <- function(h, name) suppressWarnings(as.numeric(.cst_col(h, name)))

# CEC by NH4OAc at pH 7 per kg of clay (p. 31, footnote 1): cec_cmol x 100 /
# clay_pct. A CEC recorded by another method (cec_method) stands in, flagged
# (Hugo's decision Q16, read as the RP reads its CEC).
.cst_clay_cec <- function(h) {
  cec <- .cst_num(h, "cec_cmol")
  clay <- .cst_num(h, "clay_pct")
  v <- ifelse(!is.na(clay) & clay > 0, cec * 100 / clay, NA_real_)
  m <- tolower(trimws(as.character(.cst_col(h, "cec_method"))))
  flag <- ifelse(!is.na(v) & !is.na(m) & nzchar(m) & m != "acetate_ph7",
                 "cec_method_substitute", NA_character_)
  list(value = v, flag = flag)
}

# ECEC per kg of clay (p. 31, footnote 2): the bases by NH4OAc at pH 7 plus
# KCl-extractable Al (al_kcl_cmol, else al_cmol) x 100 / clay_pct; the
# recorded ecec_cmol where that sum cannot be made.
.cst_clay_ecec <- function(h) {
  clay <- .cst_num(h, "clay_pct")
  al <- .cst_num(h, "al_kcl_cmol")
  al <- ifelse(is.na(al), .cst_num(h, "al_cmol"), al)
  bases <- .cst_num(h, "ca_cmol") + .cst_num(h, "mg_cmol") +
    .cst_num(h, "k_cmol") + .cst_num(h, "na_cmol")
  s <- bases + al
  ecec <- .cst_num(h, "ecec_cmol")
  src <- ifelse(!is.na(s), "bases_plus_al", ifelse(!is.na(ecec), "ecec_cmol", NA_character_))
  tot <- ifelse(!is.na(s), s, ecec)
  v <- ifelse(!is.na(clay) & clay > 0, tot * 100 / clay, NA_real_)
  list(value = v, source = ifelse(is.na(v), NA_character_, src))
}

# Free over total iron (pp. 31, 61): DCB iron (fe_dcb_pct, as Fe) over total
# iron (fe_total_pct, as Fe2O3) times 0.699.
.cst_fe_free_total <- function(h) {
  fd <- .cst_num(h, "fe_dcb_pct")
  ft <- .cst_num(h, "fe_total_pct") * .cst_thr("units", "fe2o3_to_fe")
  ifelse(!is.na(ft) & ft > 0, fd / ft, NA_real_)
}


# --- texture --------------------------------------------------------------------

# USDA texture class of each horizon (S, LS, SL, L, SIL, SI, SCL, CL, SICL,
# SC, SIC, C), as the WRB readers read it (silt or sand by difference).
.cst_texture_class <- function(h) .wrb_texture_class(h)

# The two texture groups of the default bulk densities (pp. 58-59): loamy or
# sandier, clay loamy or clayey. Without a class, clay under 20 % is loamy or
# sandier and clay of 27 % or more clay loamy or clayey (the classes allow
# nothing else); 20 to 27 % stays undecided.
.cst_texture_group <- function(h) {
  tex <- .cst_texture_class(h)
  sandy <- .cst_thr("storage", "bulk_density_default", "loamy_or_sandier_classes")
  clayey <- .cst_thr("storage", "bulk_density_default", "clay_loamy_or_clayey_classes")
  clay <- .cst_num(h, "clay_pct")
  ifelse(!is.na(tex) & tex %in% sandy, "loamy_or_sandier",
    ifelse(!is.na(tex) & tex %in% clayey, "clay_loamy_or_clayey",
      ifelse(is.na(tex) & !is.na(clay) & clay < 20, "loamy_or_sandier",
        ifelse(is.na(tex) & !is.na(clay) & clay >= 27, "clay_loamy_or_clayey",
               NA_character_))))
}

# "Very fine sand, loamy very fine sand or finer" (the cambic and LAC-ferric
# horizons, pp. 30-31): any class other than sand and loamy sand; a sand or
# loamy sand is not, when its 0.05-0.2 mm sand (fine_sand_pct) is under half
# of the fine earth (its very fine sand cannot reach 50 %); otherwise
# undecided.
.cst_vfs_or_finer <- function(h) {
  tex <- .cst_texture_class(h)
  fs <- .cst_num(h, "fine_sand_pct")
  ifelse(is.na(tex), NA,
    ifelse(!tex %in% c("S", "LS"), TRUE,
      ifelse(!is.na(fs) & fs < 50, FALSE, NA)))
}

# The USDA particle-size class of each horizon, as the calcic horizon reads
# it (items c and d, p. 40): fragmental, sandy-, loamy- or clayey-skeletal
# (35 % rock fragments or more by volume; a fragment content not recorded is
# none), sandy (sand or loamy sand), coarse- or fine-loamy, coarse- or
# fine-silty (clay 18 %), fine or very-fine (clay 35 %, 60 %). Loamy needs 15
# % sand coarser than very fine sand: silty under 15 % sand, loamy when the
# sand coarser than 0.2 mm (sand_pct - fine_sand_pct) reaches 15 %, otherwise
# undecided (the loess of the plateau; Hugo's decision Q1, 2026-11-05).
.cst_psc <- function(h) {
  tex <- .cst_texture_class(h)
  clay <- .cst_num(h, "clay_pct")
  sand <- .cst_num(h, "sand_pct")
  silt <- .cst_num(h, "silt_pct")
  sand <- ifelse(is.na(sand) & !is.na(silt) & !is.na(clay), 100 - silt - clay, sand)
  fr <- .cst_num(h, "coarse_fragments_pct")
  fr[is.na(fr)] <- 0
  coarse <- sand - .cst_num(h, "fine_sand_pct")
  lo <- ifelse(is.na(sand), NA, ifelse(sand < 15, FALSE, ifelse(!is.na(coarse) & coarse >= 15, TRUE, NA)))
  fe <- ifelse(!is.na(tex) & tex %in% c("S", "LS"), "sandy",
    ifelse(!is.na(clay) & clay >= 60, "very-fine",
      ifelse(!is.na(clay) & clay >= 35, "fine",
        ifelse(is.na(clay) | is.na(lo), NA_character_,
          ifelse(clay < 18, ifelse(lo, "coarse-loamy", "coarse-silty"),
                 ifelse(lo, "fine-loamy", "fine-silty"))))))
  base <- ifelse(!is.na(fe) & fe == "sandy", "sandy",
    ifelse(!is.na(fe) & fe %in% c("fine", "very-fine"), "clayey",
      ifelse(!is.na(clay) & clay < 35 & !(!is.na(tex) & tex %in% c("S", "LS")), "loamy", NA_character_)))
  ifelse(fr >= 90, "fragmental",
    ifelse(fr >= 35, ifelse(is.na(base), NA_character_, paste0(base, "-skeletal")), fe))
}


# --- colour -----------------------------------------------------------------------

# A Munsell hue as a position from red to blue: 10RP 0, 5R 5, 10R 10, 2.5YR
# 12.5, 5YR 15, 7.5YR 17.5, 10YR 20, 2.5Y 22.5, 5Y 25, 7.5Y 27.5, then GY, G,
# BG, B, PB, P from 30 on. "5YR or redder" is 15 or less, "7.5YR or
# yellower" 17.5 or more. Neutral and unread hues are NA.
.cst_hue_rank <- function(hue) {
  base <- c(RP = -10, R = 0, YR = 10, Y = 20, GY = 30, G = 40, BG = 50, B = 60,
            PB = 70, P = 80)
  vapply(as.character(hue), function(x) {
    p <- .munsell_hue_parts(x)
    if (is.na(p$family) || p$family == "N" || is.na(p$number)) return(NA_real_)
    unname(base[p$family]) + p$number
  }, numeric(1), USE.NAMES = FALSE)
}

# The hue of each horizon as a rank: moist; the dry hue where no moist one is
# recorded, flagged (Hugo's decision Q21). state = "dry" reads the dry hue
# alone (where the book writes "dry").
.cst_hue <- function(h, state = c("moist", "dry")) {
  state <- match.arg(state)
  dry <- .cst_hue_rank(.cst_col(h, "munsell_hue_dry"))
  if (state == "dry") {
    return(list(rank = dry, state = ifelse(is.na(dry), NA_character_, "dry"),
                flag = rep(NA_character_, length(dry))))
  }
  moist <- .cst_hue_rank(.cst_col(h, "munsell_hue_moist"))
  use_dry <- is.na(moist) & !is.na(dry)
  list(rank = ifelse(use_dry, dry, moist),
       state = ifelse(!is.na(moist), "moist", ifelse(use_dry, "dry", NA_character_)),
       flag = ifelse(use_dry, "hue_dry_for_moist", NA_character_))
}


# --- designations, depths and windows ------------------------------------------------

# The kind of each horizon by its designation (a numeral of a lithological
# discontinuity taken off): O (O, H), AB (AB, BA, A/B, B/A), BC (BC, CB, B/C,
# C/B), AC (AC, CA), E (E, EA, EB), A, B, C, R (R: a lithic contact; Cr: a
# paralithic contact only where `cr_contact` says so, otherwise a C with the
# redox suffix r of the Chinese field notation, Hugo's decision Q2 of
# 2026-11-05); the older Chinese paddy notation P (plough pan) as A, W and G
# (the waterlogged and gley horizons) as B; "other" for anything else, NA
# when no designation is recorded.
.cst_horizon_kind <- function(d, cr_contact = FALSE) {
  cr_contact <- rep_len(as.logical(cr_contact), length(d))
  d <- .desg_master(as.character(d))
  d <- gsub("[[:space:]]", "", d)
  out <- rep(NA_character_, length(d))
  set <- function(pat, kind) {
    hit <- is.na(out) & !is.na(d) & nzchar(d) & grepl(pat, d, perl = TRUE)
    out[hit] <<- kind
  }
  set("^R(?![A-Z])", "R")
  cr <- is.na(out) & !is.na(d) & grepl("^(Cr|CR)(?![A-Z])", d, perl = TRUE)
  out[cr] <- ifelse(cr_contact[cr] %in% TRUE, "R", "C")
  set("^P[0-9]*$", "A")
  set("^[WG][0-9a-z]*$", "B")
  set("^[OH]", "O")
  set("^(AB|BA|A/B|B/A)", "AB")
  set("^(BC|CB|B/C|C/B)", "BC")
  set("^(AC|CA)", "AC")
  set("^E", "E")
  set("^A", "A")
  set("^B", "B")
  set("^C", "C")
  out[is.na(out) & !is.na(d) & nzchar(d)] <- "other"
  out
}

# Whether a Cr of each horizon is a paralithic contact (Q2): a declared FAO,
# WRB or USDA notation (site$designation_system), or a contact recorded on the
# horizon itself (cementation, rupture resistance hard or harder,
# saprolite, a weathering stage, the rock of origin).
.cst_cr_contact <- function(h, site) {
  ds <- .cst_site_txt(site, "designation_system")
  if (!is.na(ds) && grepl("^(fao|wrb|usda|kst|soil taxonomy|nasis)", ds)) return(rep(TRUE, nrow(h)))
  cem <- tolower(as.character(.cst_col(h, "cementation_class")))
  rup <- tolower(as.character(.cst_col(h, "rupture_resistance")))
  sap <- .cst_num(h, "saprolite_pct")
  ws <- as.character(.cst_col(h, "weathering_stage"))
  ro <- as.character(.cst_col(h, "rock_origin"))
  (!is.na(cem) & cem %in% c("weakly", "moderately", "strongly", "indurated")) |
    (!is.na(rup) & grepl("hard", rup) & !grepl("slightly", rup)) |
    (!is.na(sap) & sap > 0) | (!is.na(ws) & nzchar(ws)) | (!is.na(ro) & nzchar(ro))
}

# The kinds of the horizons of a pedon in depth order (.cst_horizon_kind()
# with the Cr read by .cst_cr_contact()).
.cst_kinds <- function(pedon, h = .cst_ordered(pedon)) {
  if (is.null(h) || !nrow(h)) return(character(0))
  .cst_horizon_kind(h$designation, .cst_cr_contact(h, pedon$site))
}

# The horizons in depth order.
.cst_ordered <- function(pedon) {
  h <- pedon$horizons
  if (is.null(h) || !nrow(h)) return(h)
  h[order(h$top_cm, na.last = TRUE), ]
}

# The depth (stored scale) of the soil surface (the top of the profile,
# organic layers included) or of the mineral soil surface (the top of the
# first horizon that is not organic: not designated O or H; without a
# designation, by its carbon (pp. 21-22): under 12 % mineral, 20 % or more
# organic, between the two organic from 12 + clay/10 % (18 % at 60 % clay),
# undecided without clay; D4 of 2026-11-05).
.cst_surface_cm <- function(pedon, datum = c("surface", "mineral")) {
  datum <- match.arg(datum)
  h <- .cst_ordered(pedon)
  if (is.null(h) || !nrow(h) || all(is.na(h$top_cm))) return(NA_real_)
  if (datum == "surface") return(min(h$top_cm, na.rm = TRUE))
  kind <- .cst_kinds(pedon, h)
  oc <- .cst_num(h, "oc_pct")
  clay <- .cst_num(h, "clay_pct")
  for (i in seq_len(nrow(h))) {
    if (is.na(h$top_cm[i])) next
    if (!is.na(kind[i])) {
      if (kind[i] != "O") return(h$top_cm[i])
      next
    }
    if (is.na(oc[i])) return(NA_real_)
    if (oc[i] < 12) return(h$top_cm[i])
    if (oc[i] >= 20) next
    if (is.na(clay[i])) return(NA_real_)
    if (oc[i] < min(12 + clay[i] / 10, 18)) return(h$top_cm[i])
  }
  NA_real_
}

# The top of the first lithic or paralithic contact recorded by its
# designation (R; Cr as .cst_cr_contact() reads it), NA when none is.
.cst_contact_cm <- function(pedon) {
  h <- .cst_ordered(pedon)
  if (is.null(h) || !nrow(h)) return(NA_real_)
  k <- which(.cst_kinds(pedon, h) %in% "R" & !is.na(h$top_cm))
  if (length(k)) h$top_cm[k[1L]] else NA_real_
}

# The solum (pp. 23-24, the mollic epipedon): from the mineral soil surface to
# the base of the deepest A, E or B horizon (AB, BC and the like included)
# above the first C or contact. Undecided without designations.
.cst_solum <- function(pedon) {
  h <- .cst_ordered(pedon)
  if (is.null(h) || !nrow(h)) return(.cst_ans(NA, missing = "designation"))
  kind <- .cst_kinds(pedon, h)
  if (all(is.na(kind))) return(.cst_ans(NA, missing = "designation"))
  top <- .cst_surface_cm(pedon, "mineral")
  if (is.na(top)) return(.cst_ans(NA, missing = c("designation", "oc_pct")))
  stop_at <- which(kind %in% c("C", "R", "AC"))
  last <- if (length(stop_at)) stop_at[1L] - 1L else nrow(h)
  rows <- which(seq_len(nrow(h)) <= last & kind %in% c("A", "AB", "E", "B", "BC"))
  unread <- which(seq_len(nrow(h)) <= last & is.na(kind) & seq_len(nrow(h)) > max(c(0L, rows)))
  if (length(unread)) return(.cst_ans(NA, missing = "designation"))
  if (!length(rows)) return(.cst_ans(0, note = "no A, E or B horizon above the first C or contact"))
  bot <- h$bottom_cm[max(rows)]
  if (is.na(bot)) return(.cst_ans(NA, missing = "bottom_cm"))
  .cst_ans(bot - top, rows = rows)
}

# The rows of the horizons that touch the window [top, bottom) (stored
# depths), and the parts of the window no horizon covers.
.cst_window <- function(pedon, top, bottom) {
  h <- .cst_ordered(pedon)
  if (is.null(h) || !nrow(h) || is.na(top) || is.na(bottom) || bottom <= top)
    return(list(rows = integer(0), cover = matrix(numeric(0), ncol = 2), gap = max(0, bottom - top, na.rm = TRUE), h = h))
  ok <- !is.na(h$top_cm) & !is.na(h$bottom_cm)
  rows <- which(ok & h$top_cm < bottom & h$bottom_cm > top)
  seg <- cbind(pmax(h$top_cm[rows], top), pmin(h$bottom_cm[rows], bottom))
  covered <- 0
  at <- top
  for (k in order(seg[, 1L])) {
    a <- max(seg[k, 1L], at); b <- seg[k, 2L]
    if (b > a) { covered <- covered + (b - a); at <- b }
  }
  list(rows = rows, cover = seg, gap = (bottom - top) - covered, h = h)
}

# Thickness-weighted mean of a column over [top, bottom) (stored depths).
# Undecided when a horizon in the window has no value or no horizon covers
# part of it.
.cst_weighted <- function(pedon, attr, top, bottom) {
  w <- .cst_window(pedon, top, bottom)
  if (!length(w$rows)) return(.cst_ans(NA, missing = c(attr, "top_cm", "bottom_cm")))
  v <- .cst_num(w$h, attr)[w$rows]
  thk <- w$cover[, 2L] - w$cover[, 1L]
  if (anyNA(v)) return(.cst_ans(NA, missing = attr))
  if (w$gap > 1e-9) return(.cst_ans(NA, missing = c("top_cm", "bottom_cm"),
                                    note = "part of the window is not described"))
  .cst_ans(sum(v * thk) / sum(thk), rows = w$rows)
}

# The mixed surface (p. 21): the weighted mean of the upper 18 cm (25 cm for
# a cultivated histic epipedon) from the soil surface, or from the mineral
# soil surface when datum = "mineral".
.cst_mixed_surface <- function(pedon, attr,
                               depth = .cst_thr("mixed_surface", "depth_cm"),
                               datum = c("surface", "mineral")) {
  datum <- match.arg(datum)
  s <- .cst_surface_cm(pedon, datum)
  if (is.na(s)) return(.cst_ans(NA, missing = c("top_cm", "designation")))
  .cst_weighted(pedon, attr, s, s + depth)
}

# Every horizon that touches the window meets the test (Hugo's decision Q23):
# `pass` is one TRUE / FALSE / NA per horizon of pedon$horizons in depth
# order (as .cst_ordered() gives them). FALSE when one fails; undecided when
# one is undecided or part of the window is not described.
.cst_window_every <- function(pedon, pass, top, bottom) {
  w <- .cst_window(pedon, top, bottom)
  if (!length(w$rows)) return(.cst_ans(NA, missing = c("top_cm", "bottom_cm")))
  p <- pass[w$rows]
  if (any(p %in% FALSE)) return(.cst_ans(FALSE, rows = w$rows[p %in% FALSE]))
  if (anyNA(p) || w$gap > 1e-9) return(.cst_ans(NA, rows = w$rows))
  .cst_ans(TRUE, rows = w$rows)
}

# The share of the thickness of the window [top, bottom) whose horizons meet
# the test (Hugo's decision Q22), as a range: `low` counts only the horizons
# that meet it, `high` also the undecided ones and the depth not described.
.cst_window_share <- function(pedon, pass, top, bottom) {
  w <- .cst_window(pedon, top, bottom)
  total <- bottom - top
  if (is.na(total) || total <= 0) return(.cst_ans(NA, missing = c("top_cm", "bottom_cm")))
  thk <- if (length(w$rows)) w$cover[, 2L] - w$cover[, 1L] else numeric(0)
  p <- pass[w$rows]
  yes <- sum(thk[p %in% TRUE])
  maybe <- sum(thk[is.na(p)]) + w$gap
  .cst_ans(c(low = yes / total, high = (yes + maybe) / total), rows = w$rows)
}

# Compare a share range with a fraction: "more than" (strict) or "or more".
.cst_share_at_least <- function(share, fraction, strict = FALSE) {
  v <- share$value
  if (length(v) != 2L || anyNA(v)) return(NA)
  if (strict) {
    if (v[["low"]] > fraction) TRUE else if (v[["high"]] <= fraction) FALSE else NA
  } else {
    if (v[["low"]] >= fraction - 1e-9) TRUE else if (v[["high"]] < fraction - 1e-9) FALSE else NA
  }
}


# --- storage (pp. 58-59) -------------------------------------------------------------

# The bulk density of each horizon as a range (low, high): measured
# (bulk_density_g_cm3); else the book's default for its kind and texture
# group (Hugo's decision Q10), flagged bd_default_cst: loamy or sandier A 1.2,
# AB 1.3, B and BC 1.4, a C of earthy sediments 1.4; clay loamy or clayey A
# 1.3, AB 1.4, B and BC 1.5; else, for an O horizon, 0.1 to 0.4 Mg/m3, the
# bulk densities of the peaty histic epipedon (p. 21), flagged
# bd_organic_range_cst (Hugo's decision Q3 of 2026-11-05). No default for E,
# AC, a clayey C or a horizon with no designation. `kind` as .cst_kinds().
.cst_bulk_density <- function(h, kind = .cst_horizon_kind(.cst_col(h, "designation"))) {
  bd <- .cst_num(h, "bulk_density_g_cm3")
  bd[!is.na(bd) & bd <= 0] <- NA
  grp <- .cst_texture_group(h)
  tab <- .cst_thr("storage", "bulk_density_default")
  def <- vapply(seq_len(nrow(h)), function(i) {
    if (is.na(kind[i]) || is.na(grp[i])) return(NA_real_)
    v <- tab[[grp[i]]][[kind[i]]]
    if (is.null(v)) NA_real_ else as.numeric(v)
  }, numeric(1))
  org <- .cst_thr("storage", "bulk_density_organic_range")
  is_o <- is.na(bd) & is.na(def) & !is.na(kind) & kind == "O"
  low <- ifelse(!is.na(bd), bd, ifelse(!is.na(def), def, ifelse(is_o, org[1L], NA_real_)))
  high <- ifelse(!is.na(bd), bd, ifelse(!is.na(def), def, ifelse(is_o, org[2L], NA_real_)))
  list(low = low, high = high, value = ifelse(low == high, low, NA_real_),
       flag = ifelse(is.na(bd) & !is.na(def), "bd_default_cst",
                     ifelse(is_o, "bd_organic_range_cst", NA_character_)))
}

# Storage of organic matter (what = "om", OC x 1.724; the isohumic property)
# or of organic carbon (what = "oc"; the humic property) in kg/m2 over
# [from, to) cm below the soil surface: thickness (m) x bulk density (kg/m3)
# x content (kg/kg), summed. `value` is a range c(low, high) (equal ends
# unless an O horizon takes the range of Q3). The window ends at a lithic or
# paralithic contact; with the contact between 50 and 100 cm, the storage of
# the solum stands for it (the footnote of p. 59). Undecided when a horizon in
# it lacks its carbon or a bulk density, or part of it is not described; a
# horizon with no carbon needs no density.
.cst_storage <- function(pedon, to = 100, from = 0, what = c("om", "oc")) {
  what <- match.arg(what)
  s <- .cst_surface_cm(pedon, "surface")
  if (is.na(s)) return(.cst_ans(NA, missing = c("top_cm", "bottom_cm")))
  ct <- .cst_contact_cm(pedon)
  bottom <- s + to
  note <- character(0)
  if (!is.na(ct) && ct < bottom) {
    bottom <- ct
    note <- sprintf("the window ends at the contact at %s cm", format(ct))
    if (ct - s >= .cst_thr("storage", "contact_scaled_from_cm")) {
      so <- .cst_solum(pedon)
      ms <- .cst_surface_cm(pedon, "mineral")
      if (!is.na(so$value) && !is.na(ms) && ms + so$value < bottom && ms + so$value > s + from) {
        bottom <- ms + so$value
        note <- sprintf("the storage of the solum (to %s cm) stands for the window, the contact lying at %s cm",
                        format(bottom), format(ct))
      }
    }
  }
  if (bottom <= s + from) return(.cst_ans(c(low = 0, high = 0), note = note, contact_cm = ct))
  w <- .cst_window(pedon, s + from, bottom)
  if (!length(w$rows)) return(.cst_ans(NA, missing = c("top_cm", "bottom_cm")))
  h <- w$h
  kind <- .cst_kinds(pedon, h)
  oc <- .cst_num(h, "oc_pct")[w$rows]
  bd <- .cst_bulk_density(h, kind)
  lo <- bd$low[w$rows]; hi <- bd$high[w$rows]
  kd <- kind[w$rows]
  grp <- .cst_texture_group(h)[w$rows]
  thk_m <- (w$cover[, 2L] - w$cover[, 1L]) / 100
  frac <- oc / 100 * if (what == "om") .cst_thr("units", "oc_to_om") else 1
  used <- !is.na(frac) & frac > 0
  p_lo <- ifelse(!is.na(frac) & frac == 0, 0, thk_m * lo * 1000 * frac)
  p_hi <- ifelse(!is.na(frac) & frac == 0, 0, thk_m * hi * 1000 * frac)
  nobd <- used & is.na(lo)
  miss <- c(if (anyNA(oc)) "oc_pct",
            if (any(nobd)) "bulk_density_g_cm3",
            if (any(nobd & is.na(kd))) "designation",
            if (any(nobd & !is.na(kd) & kd %in% c("A", "AB", "B", "BC", "C") & is.na(grp))) "clay_pct")
  flags <- unique(stats::na.omit(bd$flag[w$rows][used]))
  if (anyNA(p_lo)) return(.cst_ans(NA, missing = miss, flags = flags, note = note, contact_cm = ct))
  if (w$gap > 1e-9) {
    return(.cst_ans(NA, missing = c("top_cm", "bottom_cm"), flags = flags,
                    note = c(note, "part of the window is not described"), contact_cm = ct))
  }
  .cst_ans(c(low = sum(p_lo), high = sum(p_hi)), flags = flags, note = note, rows = w$rows,
           bd_default_rows = w$rows[used & !is.na(bd$flag[w$rows])], contact_cm = ct)
}

# A storage range against a limit: TRUE when the whole range is at or above
# it, FALSE when the whole range is below it, NA otherwise (Q3).
.cst_storage_at_least <- function(storage, limit) {
  v <- storage$value
  if (length(v) != 2L || anyNA(v)) return(NA)
  if (v[["low"]] >= limit) TRUE else if (v[["high"]] < limit) FALSE else NA
}

# The depths of the storage ratio of the isohumic property (p. 58 and its
# footnote): 20 and 100 cm, scaled in proportion when a lithic or paralithic
# contact lies between 50 and 100 cm, to the thickness of the solum (75 cm:
# 15 and 75); the contact depth when the solum is not known.
.cst_storage_depths <- function(pedon) {
  up <- .cst_thr("storage", "isohumic_upper_cm")
  lo <- .cst_thr("storage", "isohumic_lower_cm")
  s <- .cst_surface_cm(pedon, "surface")
  ct <- .cst_contact_cm(pedon)
  if (!is.na(s) && !is.na(ct)) {
    d <- ct - s
    if (d >= .cst_thr("storage", "contact_scaled_from_cm") && d < lo) {
      so <- .cst_solum(pedon)$value
      if (!is.na(so) && so > 0 && so < d) d <- so
      return(c(upper = up * d / lo, lower = d))
    }
  }
  c(upper = up, lower = lo)
}


# --- the arid region (Hugo's decision Q14) ---------------------------------------------

# An arid region (the salic horizon and its evidence, p. 42; the Parasalic
# subgroups): the regional regime (climatic_moisture_regime) when recorded,
# aridic or torric being arid; else the aridity index (over 3 arid, the
# overlap of 3 to 3.5 included); else the soil's recorded regime (aridic or
# torric arid; ustic, udic and perudic not); otherwise undecided. A
# saturation regime (aquic, peraquic, stagnic, anthrostagnic) or xeric says
# nothing of the region.
.cst_arid_region <- function(pedon) {
  site <- pedon$site %||% list()
  cr <- .cst_site_txt(site, "climatic_moisture_regime")
  if (!is.na(cr) && cr %in% c("aridic", "torric", "ustic", "udic", "perudic")) {
    return(.cst_ans(cr %in% c("aridic", "torric"), source = "climatic_moisture_regime"))
  }
  ai <- .cst_site_num(site, "aridity_index")
  if (!is.na(ai)) {
    return(.cst_ans(ai > .cst_thr("arid_region", "aridity_min_exclusive"),
                    source = "aridity_index"))
  }
  sr <- .cst_site_txt(site, "soil_moisture_regime")
  if (!is.na(sr) && sr %in% c("aridic", "torric", "ustic", "udic", "perudic")) {
    return(.cst_ans(sr %in% c("aridic", "torric"), source = "soil_moisture_regime"))
  }
  .cst_ans(NA, missing = c("climatic_moisture_regime", "aridity_index", "soil_moisture_regime"),
           note = if (!is.na(sr)) sprintf("the %s regime says nothing of the region", sr))
}
