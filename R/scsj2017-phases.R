# =============================================================================
# v0.9.324 -- the soil phases of the Soil Classification System of Japan
# (SCSJ 2017, chapter V, pp. 32-36): the soil temperature phase, the
# man-made phases (zosei-so) and the forest soil phases. A phase is written
# after the class name, in parentheses; soilKey keeps the class name as it
# is and returns the phases in ClassificationResult$phases (Hugo,
# 2026-10-31), a data.frame with the kind, the code, the English name (a
# gloss: the text names the phases in Japanese only) and the Japanese name.
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

.scsj_phase_row <- function(kind, code, name = NULL, native = NULL, basis = "") {
  nm <- .SCSJ_PHASE_NAMES[[code]] %||% c(name, native)
  data.frame(kind = kind, code = code, name = name %||% nm[1L], name_native = native %||% nm[2L],
             basis = basis, stringsAsFactors = FALSE)
}

# ---- the soil temperature phase (V.1) ------------------------------------------------

# From the mean annual soil temperature (site$mean_annual_soil_temp_C, read
# as the temperature at 50 cm the text names): frigid under 8, mesic 8 to
# under 15, thermic 15 to under 22, hyperthermic 22 or more; else the USDA
# soil temperature regime (cryic and gelic read as frigid, iso- regimes as
# their base); neither: no phase (Hugo, 2026-10-31: no estimate from the
# air temperature).
.scsj_temperature_phase <- function(site) {
  t <- suppressWarnings(as.numeric(site$mean_annual_soil_temp_C %||% NA)[1L])
  if (!is.na(t)) {
    code <- if (t < 8) "frigid" else if (t < 15) "mesic" else if (t < 22) "thermic" else "hyperthermic"
    return(.scsj_phase_row("temperature", code, basis = sprintf("mean annual soil temperature %.1f C", t)))
  }
  r <- tolower(trimws(as.character(site$soil_temperature_regime %||% NA)[1L]))
  if (is.na(r) || !nzchar(r)) return(NULL)
  r <- sub("^iso", "", r)
  code <- if (grepl("hyperthermic", r)) "hyperthermic" else if (grepl("thermic", r)) "thermic" else
          if (grepl("mesic", r)) "mesic" else if (grepl("frigid|cryic|gelic|pergelic", r)) "frigid" else NA
  if (is.na(code)) return(NULL)
  .scsj_phase_row("temperature", code, basis = sprintf("soil temperature regime %s", r))
}

# ---- the man-made phases (V.2) ---------------------------------------------------------

# The name of the material of a part of a profile, by the great group it
# keys to (the Japanese name without 'great group'): English and Japanese.
.scsj_material_name <- function(code) {
  if (is.na(code)) return(c("undecided material", "\u672a\u5224\u5b9a"))
  rules <- load_rules("scsj2017")
  gg <- Filter(function(t) identical(t$code, code), rules$great_groups)
  if (!length(gg)) return(c(code, code))
  c(gg[[1L]]$name, sub("\u5927\u7fa4$", "", gg[[1L]]$name_native))
}

# The recorded name of the artificial material (the layer_origin of the
# layers with 20 % or more artefacts), else 'artificial material'.
.scsj_artefact_name <- function(x) {
  art <- .scsj_col(x$h, "artefacts_pct")
  lo <- as.character(.scsj_col(x$h, "layer_origin", NA_character_))
  rows <- which(!is.na(art) & art >= 20 & !is.na(lo) & nzchar(trimws(lo)))
  for (i in rows) {
    # the material, without the words that only say it is fill
    v <- trimws(gsub("\\s+", " ", gsub(paste0(.SCSJ_FILL_RE, "|[(),;]"), " ", lo[i], perl = TRUE, ignore.case = TRUE)))
    if (nzchar(v)) return(c(v, v))
  }
  c("artificial material", "\u4eba\u5de5\u7269\u8cea")
}

.scsj_man_made_phases <- function(pedon, x, res) {
  gg <- res$levels$code[1L] %||% NA_character_
  grp <- if (nrow(res$levels) >= 2L) res$levels$code[2L] else NA_character_
  out <- list()
  site <- x$site
  fr <- .scsj_fill_run(x)
  below <- if (length(fr$rows) && !fr$open)
    x$ord[!is.na(x$dtop[x$ord]) & x$dtop[x$ord] >= fr$bottom - 0.5 & !(x$ord %in% fr$rows)] else integer(0)
  if (identical(gg, "A")) {
    if (identical(grp, "A1")) {
      # the gley, wet and gravelly phases, from the ground surface; one only
      # (Hugo, 2026-10-31: gley before wet before gravelly)
      gdep <- x$top - x$ground
      wet <- .scsj_wetness_rows(x)
      within <- function(rows, d) any(rows %in% TRUE & !is.na(gdep) & gdep <= d)
      gv <- .scsj_col(x$h, "gravel_pct"); cf <- .scsj_col(x$h, "coarse_fragments_pct")
      grav <- ifelse(!is.na(gv), gv >= 20, ifelse(!is.na(cf), cf >= 20, FALSE))
      kind <- if (within(wet$gley, 75)) "gley" else if (within(wet$any, 75)) "wet" else
              if (within(grav, 60)) "gravelly" else "plain"
      mat <- .scsj_artefact_name(x)
      suffix <- switch(kind, gley = c("gley phase", "\u30b0\u30e9\u30a4\u76f8"),
                       wet = c("wet phase", "\u6e7f\u6027\u76f8"),
                       gravelly = c("gravelly phase", "\u7901\u8cea\u76f8"),
                       plain = c("man-made phase", "\u9020\u6210\u76f8"))
      out[[length(out) + 1L]] <- .scsj_phase_row("man-made", paste0("A1-", kind),
        sprintf("%s %s", mat[1L], suffix[1L]), paste0(mat[2L], suffix[2L]), "Artifactual soils (V.2)")
    } else {
      # Reformed soils: the fill's material over the original soil
      gf <- .scsj_part_great_group(.scsj_part_pedon(pedon, fr$rows, complete = TRUE))
      gb <- .scsj_part_great_group(.scsj_part_pedon(pedon, below, complete = isTRUE(x$complete)))
      mf <- .scsj_material_name(gf); mb <- .scsj_material_name(gb)
      out[[length(out) + 1L]] <- .scsj_phase_row("man-made", "A2-fill",
        sprintf("%s over %s man-made phase", mf[1L], mb[1L]),
        paste0(mf[2L], "\uff0f", mb[2L], "\u9020\u6210\u76f8"), "Reformed soils (V.2)")
    }
    return(do.call(rbind, out))
  }
  # other great groups: mixed, cut and fill phases
  depth <- suppressWarnings(as.numeric(site$mechanical_mixing_depth_cm %||% NA)[1L])
  purpose <- tolower(paste(as.character(unlist(site$mechanical_mixing_purpose %||% character(0))), collapse = " "))
  lu <- tolower(paste(as.character(unlist(site$land_use %||% character(0))), collapse = " "))
  if ((!is.na(depth) && depth > 0) || nzchar(trimws(purpose)) ||
      grepl("deep plough|deep plow|inversion|\u5929\u5730\u8fd4\u3057|\u6df1\u8015", lu))
    out[[length(out) + 1L]] <- .scsj_phase_row("man-made", "mixed", basis = "mechanical mixing recorded")
  if (isTRUE(as.logical(site$anthropic_truncation %||% NA)[1L]) ||
      grepl("\\bcut\\b|\u5207\u571f|land consolidation|\u5703\u5834\u6574\u5099", lu))
    out[[length(out) + 1L]] <- .scsj_phase_row("man-made", "cut", basis = "cut recorded")
  if (length(fr$rows)) {
    txt <- tolower(paste(.scsj_txt(x$h, "layer_origin")[fr$rows], collapse = " "))
    art <- .scsj_col(x$h, "artefacts_pct")[fr$rows]
    mat <- if (any(!is.na(art) & art >= 20)) .scsj_artefact_name(x) else
           if (grepl("dredg|\u6d5a\u6e2b", txt)) c("dredged sediment", "\u6d5a\u6e2b\u571f\u7802") else
           .scsj_material_name(.scsj_part_great_group(.scsj_part_pedon(pedon, fr$rows, complete = TRUE)))
    out[[length(out) + 1L]] <- .scsj_phase_row("man-made", "fill",
      sprintf("%s fill man-made phase", mat[1L]), paste0(mat[2L], "\u76db\u571f\u9020\u6210\u76f8"),
      sprintf("a fill of %.0f cm recorded", fr$thick))
  }
  if (length(out)) do.call(rbind, out) else NULL
}

# ---- the forest soil phases (V.3) ---------------------------------------------------

# Forest structure classes of each row, from structure_type words: fine
# granular, granular, nutty (nut, angular blocky small and hard), crumb,
# blocky (subangular blocky), massive; NA when not recorded.
.scsj_forest_structure <- function(h) {
  st <- .scsj_txt(h, "structure_type")
  sz <- .scsj_txt(h, "structure_size")
  ifelse(is.na(st), NA_character_,
  ifelse(grepl("fine granular|\u7d30\u7c92\u72b6|powdery", st) | (grepl("granular", st) & !is.na(sz) & grepl("very fine|fine", sz)), "fine_granular",
  ifelse(grepl("granular|\u7c92\u72b6", st), "granular",
  ifelse(grepl("nut|\u5805\u679c|polyhedral", st), "nutty",
  ifelse(grepl("crumb|\u56e3\u7c92", st), "crumb",
  ifelse(grepl("blocky|\u584a\u72b6", st), "blocky",
  ifelse(grepl("massive|\u30ab\u30d9|structureless", st), "massive", "other")))))))
}

# The forest soil type of the 1975 classification recorded for the site
# (site$forest_soil_type: BA, BB, BC, BD, BD(d), BE, BF; PDI, PDII, PDIII).
.scsj_recorded_forest_type <- function(site) {
  v <- toupper(gsub("[[:space:]]", "", as.character(site$forest_soil_type %||% NA)[1L]))
  if (is.na(v) || !nzchar(v)) return(NA_character_)
  v <- sub("^BD\\(D\\)$", "BDd", v)
  v <- sub("^P(D)?(III|II|I)$", "PD\\2", v)
  if (v %in% c("BA", "BB", "BC", "BD", "BDd", "BE", "BF", "PDI", "PDII", "PDIII")) v else NA_character_
}

# The moisture phases of the Andosols, Fluvic, Red-Yellow, Eutrosols and
# Brown Forest soils, derived from the profile where the type is not
# recorded (soilKey's reading of the descriptions of V.3; the text gives no
# figures and asks for a relative judgement, so undecided when the
# structures are not recorded or point two ways):
#   BF wet: a massive B with mottles, or grey or blue-grey;
#   BA fine-granular dry: fine granular structure in the A and the B;
#   BB granular dry: granular structure in the A, no crumb, an Oe/Oa or O;
#   BC moderately dry: nutty structure in the lower A or the upper B;
#   BE slightly wet: no O horizon, a crumb A of 25 cm or more (the text's
#     'very thick'; the footnote reads 25 cm as thick);
#   BDd: a crumb A with granular structure in its upper part or nutty in its
#     lower part;
#   BD moderately moist: a crumb A, or a blocky upper B.
.scsj_moisture_phase <- function(x) {
  fs <- .scsj_forest_structure(x$h)
  role <- x$role
  isA <- !is.na(role) & grepl("^A", role) & !x$des$buried
  isB <- !is.na(role) & grepl("^B", role) & !x$des$buried
  isO <- role %in% "O"
  if (!any(!is.na(fs[isA | isB]))) return(NA_character_)
  A <- which(isA); B <- which(isB)
  Bup <- if (length(B)) B[order(x$top[B])][1L] else integer(0)
  Alo <- if (length(A)) A[order(x$top[A], decreasing = TRUE)][1L] else integer(0)
  Aup <- if (length(A)) A[order(x$top[A])][1L] else integer(0)
  has <- function(rows, cls) any(fs[rows] %in% cls)
  rf <- .scsj_col(x$h, "redoximorphic_features_pct")
  b_massive <- has(B, "massive")
  b_wet <- any(rf[B] >= 2, na.rm = TRUE) | any(x$colour$grey[B] %in% TRUE | x$colour$bluegrey[B] %in% TRUE)
  a_th <- sum(x$thick[A], na.rm = TRUE)
  if (b_massive && b_wet) return("BF")
  if (has(A, "fine_granular") && has(B, "fine_granular")) return("BA")
  if (has(A, "granular") && !has(A, "crumb") && any(isO)) return("BB")
  if (has(c(Alo, Bup), "nutty") && !has(A, "crumb")) return("BC")
  if (!any(isO) && has(A, "crumb") && a_th >= 25 && !has(B, c("nutty", "granular", "fine_granular"))) return("BE")
  if (has(A, "crumb") && (has(Aup, "granular") || has(Alo, "nutty"))) return("BDd")
  if (has(A, "crumb") || has(Bup, "blocky")) return("BD")
  NA_character_
}

# The development phases of the Podzols: highly developed (a podzolic
# accumulation horizon by morphology with a bleached horizon); moderately
# developed (one by analysis with a bleached horizon, or with one by
# morphology and leached spots); weakly developed (one by analysis with one
# by morphology).
.scsj_podzol_phase <- function(x) {
  pz <- .scsj_podzolic_rows(x)
  bl <- .scsj_and(.scsj_bleached_rows(x), .scsj_not(.scsj_or(pz$morph, pz$analysis)))
  m <- .scsj_horizon_within(x, pz$morph, 2.5)$passed
  a <- .scsj_horizon_within(x, pz$analysis, 2.5)$passed
  b <- .scsj_horizon_within(x, bl, 1)$passed
  spots <- .scsj_any(as.logical(.scsj_col(x$h, "leached_spots", NA)))
  if (isTRUE(m) && isTRUE(b)) return("PDI")
  if (isTRUE(a) && (isTRUE(b) || (isTRUE(m) && isTRUE(spots)))) return("PDII")
  if (isTRUE(a) && isTRUE(m)) return("PDIII")
  NA_character_
}

.scsj_forest_phases <- function(x, res) {
  lu <- tolower(paste(as.character(unlist(x$site$land_use %||% character(0))), collapse = " "))
  forest <- grepl("forest|woodland|\\bwood\\b|plantation|pasture|range|grazing|\u68ee\u6797|\u6797|\u7267\u91ce|\u653e\u7267", lu) &&
            !grepl("deforest", lu)
  gg <- res$levels$code[1L] %||% NA_character_
  if (!forest || is.na(gg)) return(NULL)
  rec <- .scsj_recorded_forest_type(x$site)
  if (identical(gg, "D")) {
    code <- if (!is.na(rec) && grepl("^PD", rec)) rec else .scsj_podzol_phase(x)
    basis <- if (!is.na(rec) && grepl("^PD", rec)) "forest soil type recorded" else "podzolic and bleached horizons"
  } else if (gg %in% c("C", "E", "F", "H", "I")) {
    code <- if (!is.na(rec) && !grepl("^PD", rec)) rec else .scsj_moisture_phase(x)
    basis <- if (!is.na(rec) && !grepl("^PD", rec)) "forest soil type recorded" else "structure and horizons (soilKey's reading)"
  } else return(NULL)
  if (is.na(code)) return(NULL)
  .scsj_phase_row("forest", code, basis = basis)
}

# All the phases of a classified pedon.
.scsj_phases <- function(pedon, res) {
  x <- .scsj_profile(pedon)
  if (is.null(res$levels) || !nrow(res$levels)) return(NULL)
  rows <- list(.scsj_temperature_phase(x$site),
               tryCatch(.scsj_man_made_phases(pedon, x, res), error = function(e) NULL),
               tryCatch(.scsj_forest_phases(x, res), error = function(e) NULL))
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(NULL)
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
