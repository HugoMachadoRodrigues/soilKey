# =============================================================================
# v0.9.324 -- the soil phases of the Soil Classification System of Japan
# (SCSJ 2017, chapter V, pp. 32-36, and the supplement, pp. 47-49): the soil
# temperature phase, the man-made phases (zosei-so) and the forest soil
# phases. A phase is written after the class name, in parentheses; soilKey
# keeps the class name as it is and returns the phases in
# ClassificationResult$phases (Hugo, 2026-10-31), a data.frame with the kind,
# the code, the English name (a gloss: the text names the phases in Japanese
# only), the Japanese name and the basis. A phase is given only when it is
# decided: a test earlier in the order that the data leave undecided gives
# no phase (Hugo, 2026-11-01).
# =============================================================================

.SCSJ_PHASE_NAMES <- list(
  frigid = c("Frigid", "\u30d5\u30ea\u30b8\u30c3\u30c9"),
  mesic = c("Mesic", "\u30e1\u30b7\u30c3\u30af"),
  thermic = c("Thermic", "\u30b5\u30fc\u30df\u30c3\u30af"),
  hyperthermic = c("Hyperthermic", "\u30cf\u30a4\u30d1\u30fc\u30b5\u30fc\u30df\u30c3\u30af"),
  mixed = c("Mixed-layer man-made phase", "\u6df7\u5c64\u9020\u6210\u76f8"),
  cut = c("Cut man-made phase", "\u5207\u571f\u9020\u6210\u76f8"),
  BA = c("Fine-granular dry phase", "\u7d30\u7c92\u72b6\u69cb\u9020\u578b\u4e7e\u6027\u76f8"),
  BB = c("Granular dry phase", "\u7c92\u72b6\u69cb\u9020\u578b\u4e7e\u6027\u76f8"),
  BC = c("Moderately dry phase", "\u5f31\u4e7e\u6027\u76f8"),
  BD = c("Moderately moist phase", "\u9069\u6f64\u6027\u76f8"),
  BDd = c("Moderately moist phase, drier type", "\u504f\u4e7e\u578b\u9069\u6f64\u6027\u76f8"),
  BE = c("Slightly wet phase", "\u5f31\u6e7f\u6027\u76f8"),
  BF = c("Wet phase", "\u6e7f\u6027\u76f8"),
  PDI = c("Highly developed phase", "\u9ad8\u5ea6\u767a\u9054\u76f8"),
  PDII = c("Moderately developed phase", "\u4e2d\u5ea6\u767a\u9054\u76f8"),
  PDIII = c("Weakly developed phase", "\u5f31\u5ea6\u767a\u9054\u76f8")
)

# The endings of the Man-made soils' phases (V.2): plain, gley, wet, gravelly.
.SCSJ_MAN_MADE_ENDINGS <- list(
  plain = c("man-made phase", "\u9020\u6210\u76f8"),
  gley = c("gley phase", "\u30b0\u30e9\u30a4\u76f8"),
  wet = c("wet phase", "\u6e7f\u6027\u76f8"),
  gravelly = c("gravelly phase", "\u792b\u8cea\u76f8")
)

.scsj_phase_row <- function(kind, code, name = NULL, native = NULL, basis = "") {
  nm <- .SCSJ_PHASE_NAMES[[code]] %||% c(name, native)
  data.frame(kind = kind, code = code, name = name %||% nm[1L], name_native = native %||% nm[2L],
             basis = basis, stringsAsFactors = FALSE)
}

# Site text items, lower case; NA, "", "na", "none" and the like are not a
# record.
.scsj_site_items <- function(v) {
  t <- tolower(trimws(as.character(unlist(v %||% character(0)))))
  t[!is.na(t) & !(t %in% c("", "na", "nan", "none", "no", "nil", "-", "n/a", "unknown"))]
}

# A yes / no site record: TRUE, FALSE or NA.
.scsj_site_flag <- function(v) {
  t <- tolower(trimws(as.character(unlist(v %||% NA))[1L]))
  if (is.na(t)) return(NA)
  if (t %in% c("true", "t", "yes", "y", "1")) TRUE else if (t %in% c("false", "f", "no", "n", "0")) FALSE else NA
}

# The code of the first rule that holds when every rule before it fails; NA
# when a rule before it (or every rule) is undecided.
.scsj_first_code <- function(rules) {
  for (k in names(rules)) {
    v <- rules[[k]]
    if (isTRUE(v)) return(k)
    if (is.na(v)) return(NA_character_)
  }
  NA_character_
}

# ---- the soil temperature phase (V.1) ------------------------------------------------

# From the mean annual soil temperature (site$mean_annual_soil_temp_C, read
# as the temperature at 50 cm the text names): frigid under 8, mesic 8 to
# under 15, thermic 15 to under 22, hyperthermic 22 or more; else the USDA
# soil temperature regime (cryic and gelic read as frigid, iso- regimes as
# their base; two regimes at once: none); neither: no phase (Hugo,
# 2026-10-31: no estimate from the air temperature).
.scsj_temperature_phase <- function(site) {
  tv <- suppressWarnings(as.numeric(sub(",", ".", as.character(unlist(site$mean_annual_soil_temp_C %||% NA)), fixed = TRUE)))
  tv <- tv[!is.na(tv)]
  if (length(tv)) {
    t <- tv[1L]
    code <- if (t < 8) "frigid" else if (t < 15) "mesic" else if (t < 22) "thermic" else "hyperthermic"
    return(.scsj_phase_row("temperature", code, basis = sprintf("mean annual soil temperature %.1f C", t)))
  }
  r <- .scsj_site_items(site$soil_temperature_regime)
  if (!length(r)) return(NULL)
  r <- paste(sub("^iso", "", r), collapse = " ")
  hit <- c(hyperthermic = grepl("hyperthermic", r), thermic = grepl("(^|[^r])thermic", r),
           mesic = grepl("mesic", r), frigid = grepl("frigid|cryic|gelic", r))
  if (sum(hit) != 1L) return(NULL)
  .scsj_phase_row("temperature", names(hit)[hit], basis = sprintf("soil temperature regime %s", r))
}

# ---- the man-made phases (V.2) ---------------------------------------------------------

# The name of a great group as a material (the Japanese name without 'great
# group'): English and Japanese; NA when undecided.
.scsj_material_name <- function(code) {
  if (is.na(code)) return(c(NA_character_, NA_character_))
  rules <- load_rules("scsj2017")
  gg <- Filter(function(t) identical(t$code, code), rules$great_groups)
  if (!length(gg)) return(c(code, code))
  c(gg[[1L]]$name, sub("\u5927\u7fa4$", "", gg[[1L]]$name_native))
}

# Words that only say a layer is fill, taken out of a recorded material
# (longer words first), and what may be left that names no material.
.SCSJ_FILL_WORDS_RE <- paste0("(?i)\\b(back|land)?fill(s|ed|ing)?( material)?\\b|\\bmade ground\\b|\\bspoils?\\b|",
                              "\\bsoil dressing\\b|\\bdredged\\b|",
                              "\u57cb\u3081\u7acb\u3066\u5730|\u57cb\u7acb\u5730|\u57cb\u3081\u7acb\u3066|\u57cb\u7acb|",
                              "\u9020\u6210\u571f|\u9020\u6210\u5730|\u9020\u6210|\u76db\u571f|\u5ba2\u571f|\u6d5a\u6e2b")
.SCSJ_NO_MATERIAL <- c("", "material", "materials", "soil", "soils", "deposit", "deposits",
                       "\u571f", "\u5730", "\u7269", "\u7269\u8cea", "\u571f\u7802")

# The recorded name of the artificial material: the layer_origin of the
# first layer (in depth order, among `rows`) with 20 % or more artefacts,
# without the words that only say fill; else 'artificial material'. A name
# recorded in English keeps the generic Japanese word.
.scsj_artefact_name <- function(x, rows = x$ord) {
  art <- .scsj_col(x$h, "artefacts_pct")
  lo <- as.character(.scsj_col(x$h, "layer_origin", NA_character_))
  rows <- rows[!is.na(art[rows]) & art[rows] >= 20 & !is.na(lo[rows]) & nzchar(trimws(lo[rows]))]
  for (i in rows) {
    v <- gsub(.SCSJ_FILL_WORDS_RE, " ", lo[i], perl = TRUE)
    v <- trimws(gsub("\\s+", " ", gsub("[(),;\uff08\uff09\u3001]", " ", v, perl = TRUE)))
    if (!(tolower(v) %in% .SCSJ_NO_MATERIAL)) {
      jp <- grepl("[^\\x01-\\x7f]", v, perl = TRUE)
      return(c(v, if (jp) gsub(" ", "", v, fixed = TRUE) else "\u4eba\u5de5\u7269\u8cea"))
    }
  }
  c("artificial material", "\u4eba\u5de5\u7269\u8cea")
}

# The material of a fill outside the Man-made soils (the fill phase): its
# artificial material (20 % or more artefacts), dredged sediment, or the
# great group its material features name, in the order of the key: peat
# (Organic soils), the kuroboku feature (Andosols), alluvial deposits
# (Fluvic soils), the red-yellow colour (Red-Yellow soils) (Hugo,
# 2026-11-01: a fill thinner than the groups' 25 cm is named by its
# materials, as the text's example of a thin kuroboku fill). NA when none
# is decided.
.scsj_fill_material <- function(x, rows, open = FALSE) {
  rows <- x$ord[x$ord %in% rows]
  if (!length(rows)) return(c(NA_character_, NA_character_))
  art <- .scsj_col(x$h, "artefacts_pct")[rows]
  if (any(!is.na(art) & art >= 20)) return(.scsj_artefact_name(x, rows))
  # a fill that may go on below the description may hold artificial
  # material there, which comes first
  if (open) return(c(NA_character_, NA_character_))
  txt <- tolower(paste(.scsj_txt(x$h, "layer_origin")[rows], collapse = " "))
  if (grepl("dredg|\u6d5a\u6e2b", txt)) return(c("dredged sediment", "\u6d5a\u6e2b\u571f\u7802"))
  feats <- list(B = x$peat, C = x$kb, E = x$orig$alluvial, F = .scsj_red_yellow_rows(x))
  for (k in names(feats)) {
    v <- .scsj_any(feats[[k]][rows])
    if (isTRUE(v)) return(.scsj_material_name(k))
    if (is.na(v)) break
  }
  c(NA_character_, NA_character_)
}

# The material of the fill of the Reformed soils (35 cm or more: it keys on
# its own): artificial material, dredged sediment, or its great group.
.scsj_reformed_fill_material <- function(pedon, x, rows) {
  rows <- x$ord[x$ord %in% rows]
  art <- .scsj_col(x$h, "artefacts_pct")[rows]
  if (any(!is.na(art) & art >= 20)) return(.scsj_artefact_name(x, rows))
  txt <- tolower(paste(.scsj_txt(x$h, "layer_origin")[rows], collapse = " "))
  if (grepl("dredg|\u6d5a\u6e2b", txt)) return(c("dredged sediment", "\u6d5a\u6e2b\u571f\u7802"))
  .scsj_material_name(.scsj_part_great_group(.scsj_part_pedon(pedon, rows, complete = TRUE)))
}

# "A layer with the feature within d cm of the ground surface": TRUE, FALSE
# or NA (an undecided row, or a part of the depth no horizon describes).
.scsj_ground_within <- function(x, rows, d) {
  gdep <- x$top - x$ground
  if (any(rows %in% TRUE & !is.na(gdep) & gdep <= d)) return(TRUE)
  if (any(is.na(rows) & !is.na(gdep) & gdep <= d)) return(NA)
  to <- d - (x$surface - x$ground)
  if (!is.na(to) && to > 0 && .scsj_cum(x, rep(FALSE, x$n), 0, to)$poss > 0) return(NA)
  FALSE
}

# The phase of the Man-made soils (V.2): one only (Hugo, 2026-10-31): gley (a
# layer with the gley feature within 75 cm of the ground surface), else wet
# (a wetness feature within 75 cm), else gravelly (a gravel layer, 20 % or
# more gravel, within 60 cm), else plain; NA when a test before the one
# that holds is undecided.
.scsj_man_made_kind <- function(x) {
  wet <- .scsj_wetness_rows(x)
  # gravel (the text's word covers all coarse fragments): 20 % or more in
  # either record; under 20 % in coarse fragments, or in gravel with coarse
  # fragments not recorded, is not a gravel layer
  gv <- .scsj_col(x$h, "gravel_pct"); cf <- .scsj_col(x$h, "coarse_fragments_pct")
  grav <- ifelse((!is.na(gv) & gv >= 20) | (!is.na(cf) & cf >= 20), TRUE,
                 ifelse(!is.na(cf) | !is.na(gv), FALSE, NA))
  k <- .scsj_first_code(list(gley = .scsj_ground_within(x, wet$gley, 75),
                             wet = .scsj_ground_within(x, wet$any, 75),
                             gravelly = .scsj_ground_within(x, grav, 60),
                             plain = TRUE))
  k
}

.scsj_man_made_phases <- function(pedon, x, res) {
  gg <- res$levels$code[1L] %||% NA_character_
  grp <- if (nrow(res$levels) >= 2L) res$levels$code[2L] else NA_character_
  site <- x$site
  fr <- .scsj_fill_run(x)
  if (identical(gg, "A")) {
    kind <- .scsj_man_made_kind(x)
    if (is.na(kind)) return(NULL)
    end <- .SCSJ_MAN_MADE_ENDINGS[[kind]]
    if (identical(grp, "A1")) {
      mat <- .scsj_artefact_name(x)
      return(.scsj_phase_row("man-made", paste0("A1-", kind), sprintf("%s %s", mat[1L], end[1L]),
                             paste0(mat[2L], end[2L]), "Artifactual soils (V.2)"))
    }
    if (identical(grp, "A2")) {
      # the fill's material over the original soil's, with the same phase
      # ending as the Artifactual soils (Hugo, 2026-11-01: the supplement's
      # example)
      below <- if (length(fr$rows) && !fr$open)
        x$ord[!is.na(x$dtop[x$ord]) & x$dtop[x$ord] >= fr$bottom - 0.5 & !(x$ord %in% fr$rows)] else integer(0)
      mf <- .scsj_reformed_fill_material(pedon, x, fr$rows)
      mb <- .scsj_material_name(.scsj_part_great_group(.scsj_part_pedon(pedon, below, complete = isTRUE(x$complete))))
      if (anyNA(c(mf, mb))) return(NULL)
      return(.scsj_phase_row("man-made", paste0("A2-", kind), sprintf("%s over %s %s", mf[1L], mb[1L], end[1L]),
                             paste0(mf[2L], "\uff0f", mb[2L], end[2L]), "Reformed soils (V.2)"))
    }
    return(NULL)
  }
  out <- list()
  # mixed: mechanical mixing below the plough layer (below the Ap, or below
  # 30 cm with no Ap), or deep ploughing or inversion named (Hugo,
  # 2026-11-01; the supplement gives no phase when only the plough layer was
  # turned)
  depth <- suppressWarnings(as.numeric(as.character(unlist(site$mechanical_mixing_depth_cm %||% NA))[1L]))
  lu <- .scsj_site_items(site$land_use)
  words <- c(lu, .scsj_site_items(site$mechanical_mixing_purpose))
  ap <- .scsj_plough_rows(x)
  # the bottom of the plough layer from the soil surface (the mixing depth
  # is recorded from the mineral soil surface); an Ap without a bottom
  # leaves it undecided
  ap_bottom <- if (!any(ap)) 30 else if (anyNA(x$dbot[ap])) NA_real_ else max(x$dbot[ap])
  deep_words <- any(grepl("deep plough|deep plow|deep ripping|inversion|tephra flipping|flipping|\u5929\u5730\u8fd4\u3057|\u6df1\u8015", words))
  # a recorded depth decides (an Ap without a bottom: undecided); without
  # one, deep ploughing or inversion named is mixing below the plough layer
  mixed <- if (!is.na(depth)) { if (is.na(ap_bottom)) NA else depth > ap_bottom + 0.5 } else deep_words
  if (isTRUE(mixed))
    out[[length(out) + 1L]] <- .scsj_phase_row("man-made", "mixed", basis = "mechanical mixing below the plough layer recorded")
  # cut: truncation recorded, or a cut named (not land consolidation alone,
  # not a clear-cut forest)
  cut_words <- any(grepl("\u5207\u571f|\\bcut slope|\\bcut[- ]and[- ]fill|\\bcut land\\b|\\bcutting\\b of (the )?(soil|slope)", lu) &
                   !grepl("clear[- ]?cut|cut[- ]?over|cut flower", lu))
  if (isTRUE(.scsj_site_flag(site$anthropic_truncation)) || cut_words)
    out[[length(out) + 1L]] <- .scsj_phase_row("man-made", "cut", basis = "cut recorded")
  if (length(fr$rows)) {
    e <- .scsj_ends(x)
    last_bot <- x$dbot[fr$rows[length(fr$rows)]]
    open <- fr$open || (!is.na(e$depth) && !is.na(last_bot) && abs(last_bot - e$depth) <= 0.5 && !e$on_rock)
    mat <- .scsj_fill_material(x, fr$rows, open)
    out[[length(out) + 1L]] <- if (anyNA(mat))
      .scsj_phase_row("man-made", "fill", "fill man-made phase", "\u76db\u571f\u9020\u6210\u76f8",
                      sprintf("a fill of %.0f cm recorded; its material undecided", fr$thick))
    else .scsj_phase_row("man-made", "fill", sprintf("%s fill man-made phase", mat[1L]),
                         paste0(mat[2L], "\u76db\u571f\u9020\u6210\u76f8"), sprintf("a fill of %.0f cm recorded", fr$thick))
  }
  if (length(out)) do.call(rbind, out) else NULL
}

# ---- the forest soil phases (V.3) ---------------------------------------------------

# Forest structure classes of each row, from structure_type words and USDA
# codes: fine granular (or granular of fine size), crumb, granular, nutty
# (nut, polyhedral, angular blocky: the text's angular, shiny, dense 1 to 3
# cm structure; Hugo, 2026-11-01), blocky (subangular blocky), massive; NA
# when not recorded.
.scsj_forest_structure <- function(h) {
  st <- .scsj_txt(h, "structure_type")
  sz <- .scsj_txt(h, "structure_size")
  en_gran <- !is.na(st) & grepl("granular|^gr$", st) & !grepl("fine granular", st)
  ifelse(is.na(st), NA_character_,
  ifelse(grepl("fine granular|\u7d30\u7c92\u72b6|powdery", st) |
           (en_gran & !is.na(sz) & grepl("very fine|fine|^vf$|^f$", sz)), "fine_granular",
  ifelse(grepl("crumb|\u56e3\u7c92|^cr$", st), "crumb",
  # English granular without a size may be the text's fine granular
  ifelse(en_gran & is.na(sz), "granular_any",
  ifelse(grepl("granular|\u7c92\u72b6|^gr$", st), "granular",
  ifelse(!grepl("sub[- ]?angular", st) &
           (grepl("nut|\u5805\u679c|polyhedral|angular blocky|^abk$", st) |
            grepl("(?<!\u4e9c)\u89d2\u584a\u72b6", st, perl = TRUE)), "nutty",
  ifelse(grepl("blocky|\u584a\u72b6|^(sbk|bk)$", st), "blocky",
  ifelse(grepl("massive|\u30ab\u30d9|\u58c1\u72b6|structureless|^ma$", st), "massive", "other"))))))))
}

# The forest soil type of the 1975 classification recorded for the site
# (site$forest_soil_type): BA, BB, BC, BD, BD(d), BE, BF, PDI, PDII, PDIII, in
# any spelling (full-width letters, Roman numerals, hyphens); the types of
# the other 1975 groups (BlD, lBlD, RC, YD(d), dRB ...) by their moisture
# letter (Hugo, 2026-11-01); a wet podzol (PW) as such.
.scsj_recorded_forest_type <- function(site) {
  v <- as.character(unlist(site$forest_soil_type %||% NA))[1L]
  if (is.na(v) || !nzchar(trimws(v))) return(NA_character_)
  cp <- utf8ToInt(v); fw <- cp >= 0xFF01 & cp <= 0xFF5E; cp[fw] <- cp[fw] - 0xFEE0L; v <- intToUtf8(cp)
  v <- gsub("\u2162", "III", gsub("\u2161", "II", gsub("\u2160", "I", v, fixed = TRUE), fixed = TRUE), fixed = TRUE)
  v <- gsub("[[:space:]_-]", "", v)
  # the drier subtype: (d) in brackets, or a small d after the D
  drier <- grepl("\\([dD]\\)$", v) || grepl("Dd$", v)
  core <- toupper(sub("\\([dD]\\)$", "", if (grepl("Dd$", v)) sub("d$", "", v) else v))
  if (grepl("^PW", core)) return("PW")
  if (grepl("^PD?(III|II|I)$", core)) return(sub("^PD?(III|II|I)$", "PD\\1", core))
  # the types without a moisture letter (muck, peat, immature, eroded, gley)
  if (grepl("^(MC|PT|PP|IM|G|PG|PSG)$|^ER", core)) return("none")
  m <- regmatches(core, regexec("^(B|RB|YB|GB|R|Y|RY|LR|LY|BL|LBL|DR)([A-F])$", core))[[1L]]
  if (length(m)) return(paste0("B", m[3L], if (drier && m[3L] == "D") "d" else ""))
  NA_character_
}

# Forest or grazing land (V.3: "forest or grazing land and the like"), from
# the items of site$land_use: TRUE, FALSE, or NA when no land use is
# recorded.
.SCSJ_FOREST_RE <- paste0("\\b(forest(s|ed|ry)?|afforest\\w*|reforest\\w*|woodlands?|woods|wooded|pastures?|grazing|",
                          "rangelands?|meadows?)\\b|\u68ee\u6797|\u6797|\u7267\u91ce|\u653e\u7267")
.SCSJ_NOT_FOREST_RE <- "deforest|agroforest|non[- ]?forest|not forest|\u8fb2\u6797|\u679c\u6a39|\u975e\u68ee\u6797"
.scsj_forest_land <- function(site) {
  lu <- .scsj_site_items(site$land_use)
  if (!length(lu)) return(NA)
  any(grepl(.SCSJ_FOREST_RE, lu, perl = TRUE) & !grepl(.SCSJ_NOT_FOREST_RE, lu, perl = TRUE))
}

# The moisture phases of the Andosols, Fluvic, Red-Yellow, Eutrosols and
# Brown Forest soils, from the profile where the type is not recorded
# (soilKey's reading of the descriptions of V.3, which give no figures), in
# this order, three-valued (Hugo, 2026-11-01):
#   BF wet: a massive B with mottles, or grey or blue-grey;
#   BA fine-granular dry: fine granular structure in the A and the B;
#   BB granular dry: granular structure in the A, no crumb, an O horizon;
#   BC moderately dry: nutty structure in the lower A and in the upper B;
#   BE slightly wet: no O horizon, a crumb A of 25 cm or more (soilKey's
#     figure for 'very thick'; the text's footnote declines to set one), a B
#     without blocky, nutty or granular structure (its colour is not read);
#   BDd moderately moist, drier type: granular structure in the upper A or
#     nutty structure in the lower A;
#   BD moderately moist: a crumb A, or a blocky upper B.
# No structure recorded in the A or the B: no phase.
.scsj_moisture_phase <- function(x) {
  fs <- .scsj_forest_structure(x$h)
  role <- x$role
  isA <- !is.na(role) & grepl("^A", role) & !x$des$buried
  isB <- !is.na(role) & grepl("^B", role) & !x$des$buried
  A <- which(isA); B <- which(isB)
  # the rules read the A and the B: either not described, no phase
  if (!length(A) || !length(B) || !any(!is.na(fs[c(A, B)]))) return(NA_character_)
  Bup <- if (length(B)) B[order(x$top[B])][1L] else integer(0)
  Alo <- if (length(A)) A[order(x$top[A], decreasing = TRUE)][1L] else integer(0)
  Aup <- if (length(A)) A[order(x$top[A])][1L] else integer(0)
  # some row of `rows` has a structure of `cls`: TRUE / FALSE / NA
  gr <- c("granular", "fine_granular")
  has <- function(rows, cls) {
    if (!length(rows)) return(FALSE)
    f <- fs[rows]
    amb <- f %in% "granular_any"
    if (any(f %in% cls | (amb & all(gr %in% cls)))) TRUE
    else if (anyNA(f) || (any(amb) && any(gr %in% cls))) NA else FALSE
  }
  rf <- .scsj_col(x$h, "redoximorphic_features_pct")
  # the wet phase: one B horizon both massive and mottled, grey or blue-grey
  massive_b <- ifelse(is.na(fs[B]), NA, fs[B] %in% "massive")
  wet_b <- .scsj_or(ifelse(is.na(rf[B]), NA, rf[B] >= 2), .scsj_or(x$colour$grey[B], x$colour$bluegrey[B]))
  bf <- .scsj_any(.scsj_and(massive_b, wet_b))
  o_rec <- if (any(role %in% "O")) TRUE else if (all(x$des$known)) FALSE else NA
  a_th <- sum(x$thick[A], na.rm = TRUE)
  a_thick <- if (a_th >= 25) TRUE else if (anyNA(x$thick[A])) NA else FALSE
  rules <- list(
    BF = bf,
    BA = .scsj_and(has(A, "fine_granular"), has(B, "fine_granular")),
    BB = .scsj_and(.scsj_and(has(A, "granular"), .scsj_not(has(A, "crumb"))), o_rec),
    BC = .scsj_and(has(Alo, "nutty"), has(Bup, "nutty")),
    BE = .scsj_and(.scsj_and(.scsj_not(o_rec), has(A, "crumb")),
                   .scsj_and(a_thick, .scsj_not(has(B, c("blocky", "nutty", "granular", "fine_granular"))))),
    BDd = .scsj_or(has(Aup, "granular"), has(Alo, "nutty")),
    BD = .scsj_or(has(A, "crumb"), has(Bup, "blocky")))
  .scsj_first_code(rules)
}

# The development phases of the Podzols (V.3 2), on the key's own pairings
# (a podzolic accumulation horizon under a bleached horizon): highly
# developed, one by morphology with a bleached horizon; moderately
# developed, one by analysis with a bleached horizon, or with one by
# morphology and leached spots (the new attribute leached_spots); weakly
# developed, one by analysis with one by morphology. Three-valued in order.
.scsj_podzol_phase <- function(x) {
  ag <- .scsj_argic_rows(x); ry <- .scsj_red_yellow_rows(x)
  pz <- .scsj_podzolic_rows(x, red_yellow = ry, argic = ag)
  bl <- .scsj_and(.scsj_bleached_rows(x), .scsj_not(.scsj_or(pz$morph, pz$analysis)))
  mb <- .scsj_pair(x, pz$morph, 2.5, bl, 1)$passed
  ab <- .scsj_pair(x, pz$analysis, 2.5, bl, 1)$passed
  am <- .scsj_pair(x, pz$analysis, 2.5, pz$morph, 2.5, above = FALSE)$passed
  # leached spots in the mineral soil (not the forest floor)
  in_soil <- .scsj_and(!is.na(x$dtop) & x$dtop >= -0.5, x$mineral)
  spots <- .scsj_any(.scsj_and(in_soil, as.logical(.scsj_col(x$h, "leached_spots", NA))))
  .scsj_first_code(list(PDI = mb, PDII = .scsj_or(ab, .scsj_and(am, spots)), PDIII = am))
}

.scsj_forest_phases <- function(x, res) {
  rec <- .scsj_recorded_forest_type(x$site)
  # a recorded 1975 forest soil type says forest land when no land use is
  # recorded
  fl <- .scsj_forest_land(x$site)
  forest <- if (is.na(fl)) !is.na(rec) else fl
  gg <- res$levels$code[1L] %||% NA_character_
  if (!isTRUE(forest) || is.na(gg)) return(NULL)
  if (identical(gg, "D")) {
    if (identical(rec, "PW")) return(NULL)
    from_rec <- !is.na(rec) && grepl("^PD", rec)
    code <- if (from_rec) rec else .scsj_podzol_phase(x)
    basis <- if (from_rec) "forest soil type recorded" else "podzolic and bleached horizons"
  } else if (gg %in% c("C", "E", "F", "H", "I")) {
    if (identical(rec, "none")) return(NULL)
    from_rec <- !is.na(rec) && grepl("^B", rec)
    code <- if (from_rec) rec else .scsj_moisture_phase(x)
    basis <- if (from_rec) "forest soil type recorded" else "structure and horizons (soilKey's reading)"
  } else return(NULL)
  if (is.na(code)) return(NULL)
  .scsj_phase_row("forest", code, basis = basis)
}

# All the phases of a classified pedon. A failure in one part is kept as a
# warning of the result, not an error of the classification.
.scsj_phases <- function(pedon, res) {
  if (is.null(res$levels) || !nrow(res$levels)) return(NULL)
  x <- .scsj_profile(pedon)
  part <- function(f, what) tryCatch(f(), error = function(e) {
    res$warnings <- c(res$warnings, sprintf("SCSJ %s phase not read: %s", what, conditionMessage(e)))
    NULL
  })
  rows <- list(.scsj_temperature_phase(x$site),
               part(function() .scsj_man_made_phases(pedon, x, res), "man-made"),
               part(function() .scsj_forest_phases(x, res), "forest"))
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(NULL)
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
