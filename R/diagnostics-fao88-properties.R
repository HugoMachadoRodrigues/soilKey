# =============================================================================
# v0.9.383 -- the diagnostic properties of the FAO-Unesco Revised Legend
# (FAO 1988, World Soil Resources Report 60; the text of the ISRIC reprint of
# 1997, Technical Paper 20), chapter V, pp. 34-41: twenty-seven predicates
# <name>_fao88(pedon), internal, each a DiagnosticResult with passed TRUE /
# FALSE / NA, the layers, what would decide it (`missing`), the fallbacks used
# (evidence$flags) and the per-layer answers (evidence$values). The criteria
# are paraphrased; no text of the book is shipped. The numbers are those of
# data-raw/fao_thresholds_1988.yaml, the readings those of
# data-raw/fao_readings.yaml.
#
# Conventions (plan sections 4.2 and 5.2; Hugo's decisions of 2026-11-08 and
# 2026-11-09; the readers of R/fao-readers.R and the profile context of
# R/diagnostics-fao88-horizons.R):
#   * depths from the literal soil surface, H and O included (Q9);
#   * three values: a measurement not recorded leaves its criterion
#     undecided, a field observation not recorded is absent;
#   * layers buried under 50 cm or more of new material (35 cm or more of an
#     andic mantle, read here) are not diagnostic (p. 42) for the properties
#     bound to a horizon (ferralic, ferric, nitic, tonguing, interfingering);
#     the properties of the present state (gleyic, stagnic, permafrost,
#     salic, sodic, calcareous, sulfidic and the others) are read in them
#     too (Hugo, 2026-11-09);
#   * a property with no depth in the key has its upper limit within 125 cm
#     (p. 42; `max_top_cm = NULL`); a depth printed or passed by the caller
#     is read as "within X cm" by the rule of 2026-11-09 (it fails only when
#     the description is filled to below X, or to rock above it);
#     `window_cm = c(top, bottom)` reads "throughout top-bottom cm" (plan
#     section 4.2).
# =============================================================================

.F88_PR <- data.frame(stringsAsFactors = FALSE,
  id = c("abrupt_textural_change", "andic_properties", "calcareous", "calcaric",
         "continuous_hard_rock", "ferralic_properties", "ferric_properties",
         "fluvic_properties", "geric_properties", "gleyic_properties", "stagnic_properties",
         "gypsiferous", "interfingering", "nitic_properties", "organic_soil_materials",
         "permafrost", "plinthite", "salic_properties", "slickensides", "smeary",
         "sodic_properties", "soft_powdery_lime", "strongly_humic", "sulfidic_materials",
         "tonguing", "vertic_properties", "weatherable_minerals"),
  name = c("Abrupt textural change", "Andic properties", "Calcareous", "Calcaric",
           "Continuous hard rock", "Ferralic properties", "Ferric properties",
           "Fluvic properties", "Geric properties", "Gleyic properties", "Stagnic properties",
           "Gypsiferous", "Interfingering", "Nitic properties", "Organic soil materials",
           "Permafrost", "Plinthite", "Salic properties", "Slickensides",
           "Smeary consistence", "Sodic properties", "Soft powdery lime", "Strongly humic",
           "Sulfidic materials", "Tonguing", "Vertic properties", "Weatherable minerals"),
  page = c("34", "34", "35", "35", "35", "35", "35", "35", "36", "36-38", "36-38", "38",
           "38", "38", "39", "39", "39", "39", "39", "39", "40", "40", "40", "40", "41",
           "41", "41"))


# --- reading a per-layer answer over the profile -------------------------------------------

# The status of a per-layer answer `val` (TRUE / FALSE / NA, one per layer of
# the context): with `window_cm`, throughout that window (.fao_in_window());
# with a numeric `max_top_cm`, "within X cm" by the rule of 2026-11-09
# (.fao_within()); with neither, the upper limit within 125 cm of p. 42 read as
# the horizons of 0.9.382 read it (a layer starting at 125 cm or above).
.f88_pr_status <- function(x, val, max_top_cm = NULL, window_cm = NULL) {
  val <- as.logical(val)
  if (!is.null(window_cm)) {
    w <- .fao_in_window(x$h, val, window_cm[1L], window_cm[2L], "throughout")
    return(list(value = w$value, layers = w$layers %||% integer(0), missing = w$missing,
                flags = w$flags, scope = w$layers %||% integer(0)))
  }
  if (!is.null(max_top_cm)) {
    w <- .fao_within(x$h, val, max_top_cm)
    scope <- which(!is.na(x$top) & x$top <= max_top_cm)
    return(list(value = w$value, layers = w$layers %||% integer(0), missing = w$missing,
                flags = w$flags, scope = scope))
  }
  cand <- which(!is.na(x$top) & x$top <= 125)
  v <- val[cand]
  st <- if (any(v %in% TRUE)) TRUE else if (anyNA(v)) NA else FALSE
  lay <- if (isTRUE(st)) cand[v %in% TRUE] else cand[is.na(v)]
  list(value = st, layers = lay, missing = character(0), flags = character(0), scope = cand)
}

# The DiagnosticResult of a property.
.f88_pr_result <- function(x, id, st, flags = character(0), missing = character(0),
                           evidence = list(), notes = character(0), stand_in = character(0),
                           values = NULL, max_top_cm = NULL, window_cm = NULL) {
  info <- .F88_PR[.F88_PR$id == id, , drop = FALSE]
  passed <- st$value
  if (length(passed) != 1L) passed <- NA
  missing <- c(st$missing, missing)
  if (x$n && all(is.na(x$top) | is.na(x$bot))) {
    passed <- NA
    missing <- c("top_cm", "bottom_cm", missing)
  }
  if (!x$n) passed <- FALSE
  lay <- st$layers %||% integer(0)
  depth_note <- if (!is.null(window_cm))
    sprintf("throughout %s-%s cm of the soil surface", format(window_cm[1L]), format(window_cm[2L]))
  else if (!is.null(max_top_cm))
    sprintf("within %s cm of the soil surface", format(max_top_cm))
  else "upper limit within 125 cm of the soil surface (p. 42)"
  flags <- unique(as.character(c(flags, st$flags)))
  flags <- flags[!is.na(flags) & nzchar(flags)]
  notes <- unique(c(notes, depth_note))
  ev <- c(evidence, list(
    edition = "1988",
    page = info$page[1L],
    flags = flags,
    stand_in = unique(as.character(stand_in)),
    lower_grade = length(stand_in) > 0L,
    values = if (!is.null(values) && x$n) data.frame(layer = x$idx, value = as.logical(values))
             else NULL))
  DiagnosticResult$new(
    name = paste0(id, "_fao88"),
    passed = passed,
    layers = x$idx[lay],
    evidence = ev,
    missing = if (is.na(passed)) unique(c(missing, if (any(is.na(x$buried))) x$buried_missing))
              else character(0),
    reference = sprintf("FAO (1988), Soil Map of the World, Revised Legend (ISRIC reprint 1997), ch. V, %s, p. %s",
                        info$name[1L], info$page[1L]),
    notes = paste(unique(c(notes, flags)), collapse = "; "))
}

# A property read per layer: `reader(x)` gives list(value, flags (one per
# layer, "; " lists), missing, evidence).
.f88_pr_layers <- function(pedon, id, reader, max_top_cm = NULL, window_cm = NULL) {
  x <- .f88_ctx(pedon)
  if (!x$n) return(.f88_pr_result(x, id, list(value = FALSE, layers = integer(0))))
  r <- reader(x)
  v <- as.logical(r$value)
  st <- .f88_pr_status(x, v, max_top_cm, window_cm)
  # the flags of the layers that could pass; when none can, of every layer
  # read (the readings that made them fail)
  rel <- st$scope[!(v[st$scope] %in% FALSE)]
  if (isFALSE(st$value)) rel <- st$scope
  .f88_pr_result(x, id, st, flags = .f88_flags(r$flags %||% rep(NA_character_, x$n), rel),
                 missing = r$missing %||% character(0), evidence = r$evidence %||% list(),
                 notes = r$notes %||% character(0), stand_in = r$stand_in %||% character(0),
                 values = v, max_top_cm = max_top_cm, window_cm = window_cm)
}

# A comparison of a computed quantity, rounded to 6 decimals (so that 0.6 /
# 40 x 100 is 1.5).
.f88_cmpr <- function(v, op, k) .f88_cmp(round(v, 6), op, k)

# Layer-wise flags joined.
.f88_addf <- function(f, sel, lab) {
  sel <- sel %in% TRUE
  ifelse(sel, ifelse(is.na(f), lab, paste(f, lab, sep = "; ")), f)
}

# A per-layer answer with rock taken out and, for the properties bound to a
# horizon (`bury = TRUE`), the buried layers (p. 42; Hugo, 2026-11-09).
.f88_pr_clean <- function(x, v, bury = FALSE) {
  v <- as.logical(v)
  if (bury) v <- .f88_unburied(x, v)
  v[x$rock] <- FALSE
  v
}

# Whether the site's designations are in the FAO notation (or the WRB or
# USDA one, which write the suffixes f and v alike), or in no declared
# notation: then the suffixes f (frozen) and v (plinthite) are read.
.f88_fao_notation <- function(x) {
  ds <- tolower(trimws(as.character(x$site$designation_system %||% NA)[1L]))
  is.na(ds) || !nzchar(ds) || grepl("^(fao|wrb|usda|kst|soil taxonomy|guidelines)", ds)
}

# Membership of the layers in the units of a horizon reader (TRUE in a sure
# unit, NA in an undecided one, FALSE elsewhere).
.f88_in_units <- function(x, r) .f88_member(x, r$units)


# --- abrupt textural change (p. 34) ------------------------------------------------------------

# A clay increase between two adjoining layers over less than 5 cm: the lower
# layer has at least twice the clay of the upper where the upper has less
# than 20 %, or 20 points more where it has 20 % or more (the two items are
# joined by "and" in the print; read as either-or by the upper clay, default
# 5). "Over less than 5 cm" is the lower boundary of the upper layer recorded
# abrupt or clear (.fao_boundary_sharp(), the reader of argic item 6 too);
# gradual and diffuse fail; not recorded, absent, flagged (Hugo,
# 2026-11-09). A layer whose top is less than 5 cm below the base of the
# upper one, across a thin transitional layer, is compared too, the
# distance read from the depths. Answered on the lower layer, whose top is
# the depth of the change.
.f88_abrupt_v <- function(x) {
  .f88_memo(x, "pr_abrupt", function() {
    n <- x$n
    clay <- .f88_num(x, "clay_pct")
    sharp <- .fao_boundary_sharp(x$h)
    v <- rep(FALSE, n); flag <- rep(NA_character_, n)
    inc_of <- function(j, i) {
      if (is.na(clay[j]) || is.na(clay[i])) NA
      else if (clay[j] < 20) clay[i] > clay[j] && clay[i] >= 2 * clay[j]
      else clay[i] - clay[j] >= 20
    }
    for (i in seq_len(n)[-1L]) {
      if (x$rock[i] || is.na(x$top[i])) next
      res <- logical(0)
      for (j in seq_len(i - 1L)) {
        if (x$rock[j] || is.na(x$bot[j])) next
        gap <- x$top[i] - x$bot[j]
        if (j == i - 1L && abs(gap) <= 0.5) {
          inc <- inc_of(j, i)
          r <- .fao_and(inc, sharp$value[j])
          if (!isFALSE(inc) && !is.na(sharp$flags[j])) flag[i] <- sharp$flags[j]
        } else if (gap > 0.5 && gap < 5) {
          r <- inc_of(j, i)
          if (isTRUE(r)) flag[i] <- .f88_addf(flag[i], TRUE, "change_across_thin_layer")
        } else next
        res <- c(res, r)
      }
      v[i] <- if (length(res)) .fao_or(res) else FALSE
    }
    list(value = .f88_pr_clean(x, v), flags = flag, missing = "clay_pct")
  })
}

abrupt_textural_change_fao88 <- function(pedon, max_top_cm = NULL) {
  .f88_pr_layers(pedon, "abrupt_textural_change", .f88_abrupt_v, max_top_cm)
}


# --- andic properties (p. 34) ------------------------------------------------------------------

# Per layer, one or more of:
# (1) Alox + 1/2 Feox 2.0 % or more, bulk density (field moist) 0.9 or less
#     and P retention more than 85 %;
# (2) more than 60 % by volume of volcaniclastic material coarser than 2 mm
#     (pyroclastic_fragments_pct; a field observation, absent if not
#     recorded) and Alox + 1/2 Feox 0.4 % or more;
# (3) the 0.02-2 mm fraction 30 % or more of the fine earth (sand_pct of 30
#     or more suffices) with volcanic glass from 30 % (Alox + 1/2 Feox 0.4 or
#     less) to 5 % (2.0 or more), proportional between.
# Volcanic glass not recorded is none, flagged, and andic properties left
# undecided are absent, flagged, unless the record names volcanic material
# (the site's parent material, a layer_origin, pyroclastic fragments), where
# they stay undecided (Hugo, 2026-11-09). Item 7 of the ferralic B reads this
# function.
.f88_andic_layer <- function(x) {
  .f88_memo(x, "andic", function() {
    alfe <- round(.f88_num(x, "al_ox_pct") + 0.5 * .f88_num(x, "fe_ox_pct"), 6)
    bd <- .f88_num(x, "bulk_density_g_cm3"); pr <- .f88_num(x, "phosphate_retention_pct")
    p1 <- .f88_and(.f88_cmpr(alfe, ">=", 2), .f88_cmp(bd, "<=", 0.9), .f88_cmp(pr, ">", 85))
    vol <- .f88_num(x, "pyroclastic_fragments_pct"); vol[is.na(vol)] <- 0
    p2 <- .f88_and(vol > 60, .f88_cmpr(alfe, ">=", 0.4))
    fr <- .f88_num(x, "particles_002_2mm_pct"); sa <- .f88_num(x, "sand_pct")
    frac <- ifelse(!is.na(fr), fr >= 30, ifelse(!is.na(sa) & sa >= 30, TRUE, NA))
    gl <- .f88_num(x, "volcanic_glass_pct")
    flag <- rep(NA_character_, x$n)
    volc_re <- "volcan|tephr|ash|pumic|cinder|scoria|lava|andesit|dacit|rhyolit|obsidian|tuff|ignimbrit|pyroclast|cinza"
    pm <- .f88_site(x, "parent_material")
    pm_volc <- !is.na(pm) && grepl(volc_re, .field_term(as.character(pm)))
    lo <- .f88_chr(x, "layer_origin")
    pf <- .f88_num(x, "pyroclastic_fragments_pct")
    volc <- pm_volc | (!is.na(lo) & grepl(volc_re, lo)) | (!is.na(pf) & pf > 0)
    use <- is.na(gl) & !volc
    gl[use] <- 0; flag[use] <- "volcanic_glass_not_recorded_none"
    need <- ifelse(is.na(alfe), NA, ifelse(alfe <= 0.4, 30, ifelse(alfe >= 2, 5,
                   30 - (alfe - 0.4) / 1.6 * 25)))
    g_ok <- ifelse(!is.na(need), round(gl - need, 6) >= 0,
            ifelse(!is.na(gl) & gl >= 30, TRUE, ifelse(!is.na(gl) & gl < 5, FALSE, NA)))
    p3 <- .f88_and(frac, as.logical(g_ok))
    v <- .f88_or(p1, p2, p3)
    ab <- is.na(v) & !volc
    v[ab] <- FALSE
    flag <- .f88_addf(flag, ab, "andic_absent_no_volcanic_material")
    list(value = v, flags = flag, items = data.frame(item1 = p1, item2 = p2, item3 = p3),
         missing = c("al_ox_pct", "fe_ox_pct", "bulk_density_g_cm3", "phosphate_retention_pct",
                     "volcanic_glass_pct", "particles_002_2mm_pct"))
  })
}

# The andic reading of a mantle of new material over buried horizons (p. 42:
# 35 cm of an andic mantle bury the horizons below): the mantle has andic
# properties when every one of its layers has them (soilKey's reading),
# three-valued.
.f88_mantle_andic <- function(x, first_buried) {
  if (is.na(first_buried) || first_buried <= 1L) return(NA)
  m <- seq_len(first_buried - 1L)
  m <- m[!x$rock[m]]
  if (!length(m)) return(NA)
  .f88_all(.f88_andic_layer(x)$value[m])
}

andic_properties_fao88 <- function(pedon, max_top_cm = NULL, window_cm = NULL) {
  .f88_pr_layers(pedon, "andic_properties", function(x) {
    a <- .f88_andic_layer(x)
    list(value = .f88_pr_clean(x, a$value), flags = a$flags, missing = a$missing,
         evidence = list(items = cbind(layer = x$idx, a$items)))
  }, max_top_cm, window_cm)
}


# --- calcareous and calcaric (p. 35) ------------------------------------------------------------

# Calcareous: strong effervescence with 10 % HCl in most of the fine earth,
# or more than 2 % CaCO3 equivalent. caco3_pct decides (the reading of the
# carbonate from the pH or an HCl reaction recorded as none, made
# for the horizons is not read here). soilKey's effervescence_class (1 M
# HCl) is read for the 10 % HCl, flagged: strong, very strong or violent in
# most of the fine earth (an extent recorded as local does not count)
# passes. Without caco3_pct only that decides; any other effervescence, or
# none recorded, leaves it undecided (Hugo, 2026-11-09).
.f88_effervescence_class <- function(x) {
  e <- .field_term(.fao_col(x$h, "effervescence_class", "character"))
  out <- rep(NA_integer_, length(e))
  out[grepl("^(none|no|non|nenhuma|ausente|sem)", e)] <- 0L
  out[is.na(out) & grepl("^(very slight|slight|weak|ligeira|fraca|muito fraca)", e)] <- 1L
  out[is.na(out) & grepl("^(moderate|moderada)", e)] <- 2L
  out[is.na(out) & grepl("^(very strong|violent|extreme|muito forte|extremamente)", e)] <- 4L
  out[is.na(out) & grepl("^(strong|forte)", e)] <- 3L
  out[is.na(out) & grepl("^(strongly|fortemente)", e)] <- 3L
  out
}

.f88_calcareous_v <- function(x) {
  .f88_memo(x, "pr_calcareous", function() {
    cce <- .f88_num(x, "caco3_pct")
    cls <- .f88_effervescence_class(x)
    ext <- .f88_chr(x, "effervescence_extent")
    most <- is.na(ext) | !grepl("^local", ext)
    flag <- rep(NA_character_, x$n)
    strong <- !is.na(cls) & cls >= 3L & most
    by_cce <- .f88_cmp(cce, ">", 2)
    # strong effervescence passes; anything else decides nothing
    v <- ifelse(strong, TRUE, by_cce)
    flag <- .f88_addf(flag, strong & !(by_cce %in% TRUE), "effervescence_1m_hcl_for_10pct_hcl")
    list(value = .f88_pr_clean(x, v), flags = flag,
         missing = c("caco3_pct", "effervescence_class"))
  })
}

calcareous_fao88 <- function(pedon, max_top_cm = NULL, window_cm = NULL) {
  .f88_pr_layers(pedon, "calcareous", .f88_calcareous_v, max_top_cm, window_cm)
}

# Calcaric: calcareous throughout 20-50 cm.
calcaric_fao88 <- function(pedon) {
  .f88_pr_layers(pedon, "calcaric", .f88_calcareous_v, window_cm = c(20, 50))
}


# --- continuous hard rock (p. 35) ---------------------------------------------------------------

# Rock that a spade cannot dig when moist, continuous but for few cracks 10
# cm or more apart; duripans, petrocalcic and petrogypsic horizons and the
# petroferric phase are not rock. A layer is rock as the readers read it
# (.fao_is_rock(): an R designation, or bedrock_pct of 90 or more with a hard
# rupture resistance), or bedrock_pct of 90 or more recorded as not diggable
# (rock_diggable FALSE), flagged; a cemented pan (suffix m) is not rock. The
# crack spacing is not recorded and is taken as met by such a layer.
continuous_hard_rock_fao88 <- function(pedon, max_top_cm = NULL) {
  x <- .f88_ctx(pedon)
  id <- "continuous_hard_rock"
  if (!x$n) return(.f88_pr_result(x, id, list(value = FALSE, layers = integer(0))))
  rk <- x$rock
  b <- .f88_num(x, "bedrock_pct"); dg <- .f88_lgl(x, "rock_diggable")
  extra <- !rk & !is.na(b) & b >= 90 & dg %in% FALSE
  suf <- ifelse(is.na(x$desg), NA_character_, sub("^[A-Z]+(/[A-Z]+)?", "", x$desg))
  # a cemented pan is not rock, even where the rock reader counts it (a Bkm of
  # 90 % 'bedrock' recorded very hard): a suffix m, or a petrocalcic or
  # petrogypsic horizon
  r_desg <- !is.na(x$desg) & grepl(.WRB_ROCK_DESIGNATION, x$desg)
  petro <- .f88_member(x, .f88_hz_petro(x, "calcic")$units) %in% TRUE |
    .f88_member(x, .f88_hz_petro(x, "gypsic")$units) %in% TRUE
  pan <- !r_desg & ((!is.na(suf) & grepl("m", suf)) | petro)
  v <- (rk | extra) & !pan
  flag <- ifelse(extra & !pan, "rock_from_bedrock_not_diggable", NA_character_)
  lim <- if (is.null(max_top_cm)) 125 else max_top_cm
  hit <- which(v & !is.na(x$top) & x$top <= lim)
  if (length(hit)) {
    st <- list(value = TRUE, layers = hit)
  } else if (is.null(max_top_cm)) {
    st <- list(value = FALSE, layers = integer(0))
  } else {
    w <- .fao_window(x$h, 0, max_top_cm + 1e-3)
    st <- if (w$covered) list(value = FALSE, layers = integer(0))
          else c(list(value = NA, layers = integer(0)), .fao_window_missing(w)[c("missing", "flags")])
  }
  .f88_pr_result(x, id, st, flags = .f88_flags(flag, hit), values = v,
                 max_top_cm = max_top_cm)
}


# --- ferralic properties (p. 35) ----------------------------------------------------------------

# CEC (NH4OAc) below 24 cmol(+)/kg clay, or below 4 cmol(+)/kg soil, in at
# least some subhorizon of the cambic B or of the horizon right below the A
# (the first layer below the run of A layers; no A at the surface, only the
# cambic B is read). No clay minimum (unlike WRB sideralic properties).
ferralic_properties_fao88 <- function(pedon, max_top_cm = NULL) {
  .f88_pr_layers(pedon, "ferralic_properties", function(x) {
    cb <- .f88_in_units(x, .f88_hz_cambic_b(x))
    below_a <- rep(FALSE, x$n)
    a <- .f88_a_run(x)
    if (!is.null(a)) {
      k <- max(a$layers) + 1L
      if (k <= x$n && !x$rock[k]) below_a[k] <- TRUE
    }
    where <- .f88_or(cb, below_a)
    cc <- .fao_cec_clay(x$h, "nh4oac", "clay"); cs <- .fao_cec_clay(x$h, "nh4oac", "soil")
    low <- .f88_or(.f88_cmpr(cc$value, "<", 24), .f88_cmpr(cs$value, "<", 4))
    v <- .f88_and(where, low)
    flag <- ifelse(is.na(cc$flags), cs$flags, cc$flags)
    flag <- .f88_addf(flag, below_a & !(cb %in% TRUE), "horizon_below_a")
    list(value = .f88_pr_clean(x, v, bury = TRUE), flags = flag, missing = c("cec_cmol", "clay_pct"),
         evidence = list(cambic = cb, below_a = below_a))
  }, max_top_cm)
}


# --- ferric properties (p. 35) ------------------------------------------------------------------

# Mottles recorded in a layer: redoximorphic_features_pct above 0 or mottle
# colours that do not say none.
.f88_mottles <- function(x) {
  .f88_memo(x, "mottles", function() {
    rp <- .f88_num(x, "redoximorphic_features_pct")
    mc <- .fao_col(x$h, "mottle_colors", "character")
    has_col <- .cst_mottles_recorded(mc)
    none <- (!is.na(rp) & rp == 0) & !has_col
    list(present = ((!is.na(rp) & rp > 0) | has_col) & !none, pct = rp, colours = mc,
         has_colours = has_col)
  })
}

# Mottle colours of a layer as a table (hue string, chroma).
.f88_mottle_table <- function(mc) {
  m <- .munsell_notations(mc)
  if (!nrow(m)) return(m)
  m$hue <- ifelse(is.na(m$family), NA_character_,
                  ifelse(m$family == "N", "N", paste0(m$number, m$family)))
  m
}

# Many coarse mottles with hues redder than 7.5YR or chromas above 5, or
# both ("many" 15 % or more, "coarse" more than 20 mm, Q22); or discrete
# nodules up to 2 cm with enriched, cemented exteriors redder or stronger in
# chroma than the interiors (ferric_nodules_pct above 0; fe_mn_nodules_pct
# alone leaves it undecided, flagged; nodule_size_max_cm 2 or less, not recorded
# accepted, flagged; the exteriors are not recorded and are taken as such).
# Field observations: not recorded, absent; mottles recorded without their
# amount, size or colour leave the mottle path undecided.
.f88_ferric_v <- function(x) {
  .f88_memo(x, "pr_ferric", function() {
    n <- x$n
    mo <- .f88_mottles(x)
    size <- .field_term(.fao_col(x$h, "mottle_size_class", "character"))
    many <- ifelse(!mo$present, FALSE, ifelse(is.na(mo$pct), NA, mo$pct >= 15))
    coarse <- ifelse(!mo$present, FALSE, ifelse(is.na(size), NA, grepl("^(coarse|grande|very coarse)", size)))
    colour <- vapply(seq_len(n), function(i) {
      if (!mo$present[i]) return(FALSE)
      if (!mo$has_colours[i]) return(NA)
      m <- .f88_mottle_table(mo$colours[i])
      if (!nrow(m)) return(NA)
      red <- .fao_hue_cmp(m$hue, "7.5YR", "redder")
      ok <- .f88_or(red, .f88_cmp(m$chroma, ">", 5))
      .f88_any(ok)
    }, NA)
    mott <- .f88_and(many, coarse, colour)
    nod <- .f88_num(x, "ferric_nodules_pct"); flag <- rep(NA_character_, n)
    fm <- .f88_num(x, "fe_mn_nodules_pct")
    # Fe-Mn nodules recorded without ferric nodules: undecided, flagged (Hugo,
    # 2026-11-09)
    use_fm <- is.na(nod) & !is.na(fm) & fm > 0
    flag <- .f88_addf(flag, use_fm, "fe_mn_nodules_not_read_as_ferric")
    sz <- .f88_num(x, "nodule_size_max_cm")
    has_nod <- !is.na(nod) & nod > 0
    nod_ok <- has_nod & (is.na(sz) | sz <= 2)
    nod_ok[use_fm] <- NA
    flag <- .f88_addf(flag, has_nod & is.na(sz), "nodule_size_not_recorded_accepted")
    v <- .f88_or(mott, nod_ok)
    list(value = .f88_pr_clean(x, v, bury = TRUE), flags = flag,
         missing = c("redoximorphic_features_pct", "mottle_size_class", "mottle_colors"),
         evidence = list(mottles = data.frame(layer = x$idx, many = many, coarse = coarse,
                                              colour = colour, nodules = nod_ok)))
  })
}

ferric_properties_fao88 <- function(pedon, max_top_cm = NULL) {
  .f88_pr_layers(pedon, "ferric_properties", .f88_ferric_v, max_top_cm)
}


# --- fluvic properties (p. 35) ------------------------------------------------------------------

.F88_SEDIMENT_RE <- "alluvi|aluvi|fluvi|fluvial|marin|lacustr|limnic|estuar|delta|tidal|lagoon|lagun|backswamp|floodplain|flood plain|levee|varzea|sedimentos? (fluvia|marinh|lacustr)"
.F88_NOT_SEDIMENT_RE <- "granit|basalt|gneiss|gnaiss|schist|xisto|quartzit|limeston|calcari|dolomit|sandston|arenito|shale|folhelho|slate|ardosia|residu|saprolit|loess|aeolian|eolic|eolian|dune|duna|till|moraine|glacial|colluv|coluvi|volcan|tephr|marl|mudston|siltston|argilit|peridot|serpentin|gabbro|diabas|andesit|rhyolit|granodior|migmat|phyllit|filit"

# The origin of the sediments, as recorded: TRUE where the parent material,
# a layer_origin of the upper 125 cm or the landform names fluviatile, marine
# or lacustrine sediments; FALSE where the parent material names something
# else and nothing names such sediments; NA otherwise (default 6).
.f88_sediment_origin <- function(x) {
  pm <- .f88_site(x, "parent_material")
  pm <- if (is.na(pm)) NA_character_ else .field_term(as.character(pm))
  lf <- .f88_site(x, "landform")
  lf <- if (is.na(lf)) NA_character_ else .field_term(as.character(lf))
  lo <- .f88_chr(x, "layer_origin")
  upper <- !is.na(x$top) & x$top < 125
  lo_sed <- any(!is.na(lo[upper]) & grepl(paste0(.F88_SEDIMENT_RE, "|tidal-"), lo[upper]))
  sed <- lo_sed || (!is.na(pm) && grepl(.F88_SEDIMENT_RE, pm)) ||
    (!is.na(lf) && grepl("floodplain|flood plain|levee|delta|tidal|backswamp|lagoon|varzea", lf))
  if (sed) return(TRUE)
  if (!is.na(pm) && grepl(.F88_NOT_SEDIMENT_RE, pm)) return(FALSE)
  NA
}

# Fluviatile, marine or lacustrine sediments receiving fresh material, with
# one or both of: (1) organic carbon that decreases irregularly with depth
# (an increase of more than `oc_increase_min`, 0.2 points, from one layer to
# the next deeper one within 125 cm; Hugo, 2026-11-09) or stays above 0.20 % to
# 125 cm (a sand or loamy sand stratum with less is exempt where a finer
# layer below it within 125 cm, not a buried A, has more than 0.20 %); (2)
# stratification (stratification_pattern recorded) in 25 % or more of the
# soil volume within 125 cm. The origin is read where it is recorded; where
# it is not, the two criteria decide, flagged (default 6).
fluvic_properties_fao88 <- function(pedon, oc_increase_min = 0.2) {
  x <- .f88_ctx(pedon)
  id <- "fluvic_properties"
  if (!x$n) return(.f88_pr_result(x, id, list(value = FALSE, layers = integer(0))))
  flags <- character(0)
  origin <- .f88_sediment_origin(x)
  if (is.na(origin)) flags <- c(flags, "sediment_origin_not_recorded_criteria_decide")
  oc <- .f88_num(x, "oc_pct")
  w <- .fao_window(x$h, 0, 125)
  L <- w$layers
  # (1a) an irregular decrease: an increase with depth between adjoining layers
  inc <- vapply(seq_along(L)[-1L], function(k) {
    i <- L[k]; j <- L[k - 1L]
    if (abs(x$top[i] - x$bot[j]) > 0.5) return(NA)
    .f88_cmpr(oc[i] - oc[j], ">", oc_increase_min)
  }, NA)
  c1a <- if (any(inc %in% TRUE)) TRUE else if (length(inc) && all(inc %in% FALSE) && w$covered) FALSE
         else if (!length(inc) && w$covered) FALSE else NA
  # (1b) above 0.20 % to 125 cm, thin sand strata exempt
  tex <- .fao_texture_usda(x$h)
  bur_a <- !is.na(x$desg) & grepl("^A[a-z0-9]*b", x$desg)
  t1 <- .f88_cmp(oc, ">", 0.2)
  ex <- rep(FALSE, x$n)
  for (i in L) {
    if (!(t1[i] %in% FALSE) || !(tex[i] %in% c("S", "LS"))) next
    below <- L[L > i]
    ex[i] <- any(t1[below] %in% TRUE & !(tex[below] %in% c("S", "LS")) & !bur_a[below])
  }
  t1[ex] <- TRUE
  if (any(ex)) flags <- c(flags, "thin_sand_stratum_exempt")
  c1b <- .fao_in_window(x$h, t1, 0, 125, "throughout")
  # (2) stratification in 25 % or more of the soil volume within 125 cm
  sp <- .f88_chr(x, "stratification_pattern")
  strat <- !is.na(sp) & !(sp %in% c("none", "no", "absent"))
  sth <- sum(w$thickness[strat[L]])
  soil <- w$soil_bottom
  c2 <- if (sth >= 0.25 * soil) TRUE else if (w$covered) FALSE else NA
  crit <- .fao_or(c1a, c1b$value, c2)
  passed <- if (isFALSE(origin)) FALSE else crit
  miss <- c(if (is.na(c1a) || is.na(c1b$value)) "oc_pct", c1b$missing,
            if (is.na(origin)) "parent_material")
  st <- list(value = passed, layers = if (isTRUE(passed) || is.na(passed)) L else integer(0))
  .f88_pr_result(x, id, st, flags = flags, missing = miss,
                 evidence = list(origin = origin, oc_irregular = c1a, oc_above_0.2 = c1b$value,
                                 stratified_cm = sth, stratification = c2,
                                 flood_frequency = .f88_site(x, "flood_frequency")),
                 max_top_cm = 125)
}


# --- geric properties (p. 36) -------------------------------------------------------------------

# (1) exchangeable bases plus unbuffered 1 M KCl exchangeable acidity 1.5
# cmol(+)/kg clay or less (the "plus" missing in the print is read, default
# 20; the acidity and its fallbacks as the ECEC of the ferralic B,
# .fao_ecec_clay()); or (2) delta pH (pH KCl - pH H2O) of +0.1 or more.
.f88_geric_v <- function(x) {
  .f88_memo(x, "pr_geric", function() {
    ec <- .fao_ecec_clay(x$h, "1988")
    d <- .f88_num(x, "ph_kcl") - .f88_num(x, "ph_h2o")
    v <- .f88_or(.f88_cmpr(ec$value, "<=", 1.5), .f88_cmpr(d, ">=", 0.1))
    list(value = .f88_pr_clean(x, v), flags = ec$flags,
         missing = c(ec$missing, "ph_kcl", "ph_h2o"),
         evidence = list(delta_ph = data.frame(layer = x$idx, delta_ph = d, ecec_clay = ec$value)))
  })
}

geric_properties_fao88 <- function(pedon, max_top_cm = NULL) {
  .f88_pr_layers(pedon, "geric_properties", .f88_geric_v, max_top_cm)
}


# --- gleyic and stagnic properties (pp. 36-38) ---------------------------------------------------

# Reduction (item 1): rH = Eh / 29 + 2 pH of 19 or less (rh_value; else
# eh_mv with ph_h2o, flagged), a positive ferricyanide test or a positive
# dipyridyl test (childs_test), read as an OR over the three: a negative test
# with the others not made is undecided, not a refutation (Hugo, 2026-11-09).
# The colour pattern confirms reduction but never refutes it (Q21; Hugo,
# 2026-11-09): a matrix of moist value 4 or more with chroma 2 or less or a
# hue N, GY, BG, G or B, or reduced_matrix_pct / reduction_colours_pct above
# 0, is reduction, flagged; any other colour leaves it undecided; an E or
# albic E layer never takes the colour route.
.f88_gley_hue <- function(hue) {
  vapply(as.character(hue), function(h) {
    p <- .munsell_hue_parts(h)
    if (is.na(p$family)) NA else p$family %in% c("N", "GY", "BG", "G", "B")
  }, NA, USE.NAMES = FALSE)
}

.f88_reduction <- function(x) {
  .f88_memo(x, "reduction", function() {
    n <- x$n
    rh <- .f88_num(x, "rh_value")
    eh <- .f88_num(x, "eh_mv"); ph <- .f88_num(x, "ph_h2o")
    flag <- rep(NA_character_, n)
    calc <- is.na(rh) & !is.na(eh) & !is.na(ph)
    rh[calc] <- eh[calc] / 29 + 2 * ph[calc]
    flag <- .f88_addf(flag, calc, "rh_from_eh_and_ph")
    t_rh <- .f88_cmpr(rh, "<=", 19)
    pos <- function(v) ifelse(is.na(v), NA, ifelse(grepl("^pos", v), TRUE, ifelse(grepl("^neg", v), FALSE, NA)))
    fc <- pos(.f88_chr(x, "ferricyanide_test")); dp <- pos(.f88_chr(x, "childs_test"))
    tests <- .f88_or(t_rh, fc, dp)
    # the colour route: confirms only
    col <- .f88_colours(x)
    gh <- .f88_gley_hue(col$hue)
    rm <- .f88_num(x, "reduced_matrix_pct"); rc <- .f88_num(x, "reduction_colours_pct")
    gley_col <- (!is.na(col$vm) & col$vm >= 4 & ((gh %in% TRUE) | (!is.na(col$cm) & col$cm <= 2))) |
      (!is.na(rm) & rm > 0) | (!is.na(rc) & rc > 0)
    e_layer <- (!is.na(x$master) & grepl("^E", x$master)) |
      (.f88_member(x, .f88_hz_albic_e(x)$units) %in% TRUE)
    use <- !(tests %in% TRUE) & gley_col & !e_layer
    v <- tests
    v[use] <- TRUE
    flag <- .f88_addf(flag, use, "reduction_from_colour_pattern")
    list(value = as.logical(v), flags = flag,
         missing = c("rh_value", "ferricyanide_test", "childs_test"))
  })
}

# Groundwater standing in a borehole at a depth from which the capillary
# fringe reaches the surface (gleyic item 2b): a water table at or above the
# surface, or one not perched within `fringe_cm` (100 cm, soilKey's number)
# of the surface, recorded at the site (groundwater_depth_cm with
# groundwater_kind); none observed, perched or deeper: no; not recorded:
# absent, flagged (a field observation). The dye test is not recorded.
.f88_groundwater <- function(x, fringe_cm = 100) {
  gk <- .f88_site(x, "groundwater_kind")
  gk <- if (is.na(gk)) NA_character_ else .field_term(as.character(gk))
  gd <- suppressWarnings(as.numeric(.f88_site(x, "groundwater_depth_cm")))
  wa <- suppressWarnings(as.numeric(.f88_site(x, "water_table_cm_above_surface")))
  if (!is.na(wa) && wa >= 0) return(list(value = TRUE, flags = character(0)))
  if (!is.na(gk) && grepl("flood", gk)) return(list(value = TRUE, flags = character(0)))
  if (!is.na(gk) && grepl("perched", gk)) return(list(value = FALSE, flags = character(0)))
  if (!is.na(gd)) {
    return(list(value = gd <= fringe_cm,
                flags = if (gd <= fringe_cm) "capillary_fringe_from_groundwater_within_100cm"
                        else character(0)))
  }
  if (!is.na(gk) && grepl("none", gk)) return(list(value = FALSE, flags = character(0)))
  list(value = FALSE, flags = "groundwater_not_recorded_absent")
}

# Gleyic item 2c: white to black (N) or blue to green (GY, BG, G, B) in
# more than 95 % of the matrix (reduced_matrix_pct, else the matrix colour
# read as 100 %, flagged), with high-chroma mottles, if any, only on ped
# faces or in channels (redox_segregations_location ped faces or voids;
# within peds or the matrix fails; not recorded with mottles present,
# undecided). In Podzols, the sand and silt grains free of iron in or right
# below the spodic B (an ignition test that does not turn redder,
# .fao_ignition_redder()) show the property (item 4), flagged.
.f88_gley_colours <- function(x) {
  .f88_memo(x, "gley_colours", function() {
    n <- x$n
    col <- .f88_colours(x)
    gh <- .f88_gley_hue(col$hue)
    rm <- .f88_num(x, "reduced_matrix_pct")
    flag <- rep(NA_character_, n)
    mo <- .f88_mottles(x)
    loc <- .f88_chr(x, "redox_segregations_location")
    loc_ok <- ifelse(!mo$present, TRUE, ifelse(is.na(loc), NA,
                     ifelse(grepl("ped face|void|channel|skeletan", loc), TRUE, FALSE)))
    pct_ok <- ifelse(!is.na(rm), rm > 95, ifelse(is.na(gh), NA, TRUE))
    flag <- .f88_addf(flag, is.na(rm) & gh %in% TRUE, "matrix_colour_read_as_100pct")
    v <- .f88_and(gh, pct_ok, loc_ok)
    # the Podzol route (item 4)
    pod <- .f88_podzol_route(x)
    v[pod$value %in% TRUE] <- TRUE
    flag <- .f88_addf(flag, pod$value %in% TRUE, pod$flag)
    list(value = v, flags = flag, podzol = pod$value)
  })
}

.f88_podzol_route <- function(x) {
  sp <- .f88_hz_spodic_b(x)
  sure <- sort(unique(unlist(lapply(Filter(function(u) isTRUE(u$status), sp$units), `[[`, "layers"))))
  out <- rep(FALSE, x$n)
  if (!length(sure)) return(list(value = out, flag = NA_character_))
  zone <- sort(unique(c(sure, max(sure) + 1L)))
  zone <- zone[zone <= x$n & !x$rock[zone]]
  ig <- .fao_ignition_redder(x$h)
  out[zone] <- ig$value[zone] %in% FALSE
  list(value = out, flag = "podzol_grains_free_of_iron_ignition")
}

# Gleyic properties (item 2): at least two of reduction (item 1), the
# groundwater of item 2b and the colours of item 2c, per layer.
.f88_gleyic_v <- function(x) {
  .f88_memo(x, "pr_gleyic", function() {
    red <- .f88_reduction(x)
    gw <- .f88_groundwater(x)
    gc <- .f88_gley_colours(x)
    b <- rep(gw$value, x$n)
    signs <- cbind(a = red$value, b = b, c = gc$value)
    yes <- apply(signs, 1L, function(s) sum(s %in% TRUE))
    may <- apply(signs, 1L, function(s) sum(is.na(s)))
    v <- ifelse(yes >= 2L, TRUE, ifelse(yes + may < 2L, FALSE, NA))
    flag <- red$flags
    flag <- ifelse(is.na(gc$flags), flag, ifelse(is.na(flag), gc$flags, paste(flag, gc$flags, sep = "; ")))
    if (length(gw$flags)) flag <- .f88_addf(flag, rep(TRUE, x$n), gw$flags)
    list(value = .f88_pr_clean(x, v), flags = flag,
         missing = c(red$missing, "groundwater_depth_cm", "reduced_matrix_pct",
                     "redox_segregations_location"),
         evidence = list(signs = data.frame(layer = x$idx, reduction = red$value,
                                            groundwater = b, gley_colours = gc$value)))
  })
}

gleyic_properties_fao88 <- function(pedon, max_top_cm = NULL) {
  .f88_pr_layers(pedon, "gleyic_properties", .f88_gleyic_v, max_top_cm)
}

# Stagnic properties (item 3), within 50 cm: reduction (item 1) and, where
# mottled, a dominant moist chroma of 2 or less in the matrix with mottles
# of higher chroma (a recorded mottle colour of higher chroma; mottles
# recorded without colours are taken as higher, flagged) or Fe-Mn
# concretions, or of 2 or less on ped surfaces (coatings_chroma_moist) with
# such mottles within the peds; where not mottled, a moist chroma of 1 or
# less on ped surfaces or in the matrix. Item 3d (the chroma increasing with
# depth) supports the property and is reported, not required (Q21).
.f88_stagnic_v <- function(x) {
  .f88_memo(x, "pr_stagnic", function() {
    n <- x$n
    red <- .f88_reduction(x)
    col <- .f88_colours(x)
    mo <- .f88_mottles(x)
    cc <- .f88_num(x, "coatings_chroma_moist")
    loc <- .f88_chr(x, "redox_segregations_location")
    fm <- .f88_num(x, "fe_mn_nodules_pct"); mn <- .f88_num(x, "manganiferous_nodules_pct")
    conc <- (!is.na(fm) & fm > 0) | (!is.na(mn) & mn > 0)
    flag <- red$flags
    higher <- vapply(seq_len(n), function(i) {
      if (!mo$present[i]) return(FALSE)
      if (!mo$has_colours[i]) return(TRUE)
      m <- .f88_mottle_table(mo$colours[i])
      ref <- col$cm[i]
      if (!nrow(m) || all(is.na(m$chroma)) || is.na(ref)) return(NA)
      any(m$chroma > ref, na.rm = TRUE)
    }, NA)
    flag <- .f88_addf(flag, mo$present & !mo$has_colours, "mottle_chroma_not_recorded_taken_higher")
    within_peds <- !is.na(loc) & grepl("within ped", loc)
    # the colour of the ped surfaces is a field observation: not recorded, absent
    cc2 <- !is.na(cc) & cc <= 2; cc1 <- !is.na(cc) & cc <= 1
    b <- .f88_or(.f88_and(.f88_cmp(col$cm, "<=", 2), .f88_or(higher, conc)),
                 .f88_and(cc2, higher, within_peds))
    unm <- .f88_or(.f88_cmp(col$cm, "<=", 1), cc1)
    pattern <- ifelse(mo$present | conc, b, unm)
    v <- .f88_and(red$value, pattern)
    # the Podzol route of item 4 (grains free of iron in or right below the
    # spodic B) shows stagnic as well as gleyic properties
    pod <- .f88_gley_colours(x)$podzol
    v[pod %in% TRUE] <- TRUE
    flag <- .f88_addf(flag, pod %in% TRUE, "podzol_grains_free_of_iron_ignition")
    # item 3d, support only: the chroma of a deeper layer above that of the layer
    d <- vapply(seq_len(n), function(i) {
      deeper <- which(seq_len(n) > i & !x$rock)
      if (!length(deeper) || is.na(col$cm[i])) return(NA)
      .f88_any(col$cm[deeper] > col$cm[i])
    }, NA)
    list(value = .f88_pr_clean(x, v), flags = flag,
         missing = c(red$missing, "mottle_colors"),
         evidence = list(items = data.frame(layer = x$idx, reduction = red$value,
                                            mottled = mo$present | conc, pattern = pattern,
                                            chroma_increasing_with_depth = d)))
  })
}

stagnic_properties_fao88 <- function(pedon, max_top_cm = 50) {
  .f88_pr_layers(pedon, "stagnic_properties", .f88_stagnic_v, max_top_cm)
}


# --- gypsiferous (p. 38) ------------------------------------------------------------------------

# 5 % gypsum or more (caso4_pct; not recorded, an ECe below 2 dS/m is no
# gypsum, flagged, as for the gypsic horizon).
gypsiferous_fao88 <- function(pedon, max_top_cm = NULL, window_cm = NULL) {
  .f88_pr_layers(pedon, "gypsiferous", function(x) {
    g <- .f88_gypsum(x)
    list(value = .f88_pr_clean(x, .f88_cmp(g$value, ">=", 5)), flags = g$flags,
         missing = "caso4_pct")
  }, max_top_cm, window_cm)
}


# --- interfingering (p. 38) ---------------------------------------------------------------------

# Penetrations of an albic E into the argic or natric B below it, too narrow
# for tonguing, as skeletans (clean silt or sand coatings) more than 1 mm
# thick on the ped faces (skeletan_thickness_mm, the coating on each face;
# more than 2 mm across two faces) making more than 15 % of the volume of
# the subhorizon (skeletans_pct). Not recorded: absent, flagged (a field
# observation; Hugo, 2026-11-09). No grouping or unit of 1988 uses it.
interfingering_fao88 <- function(pedon, max_top_cm = NULL) {
  .f88_pr_layers(pedon, "interfingering", function(x) {
    bt <- .f88_or(.f88_in_units(x, .f88_hz_argic_natric(x, FALSE)),
                  .f88_in_units(x, .f88_hz_argic_natric(x, TRUE)))
    ae <- .f88_in_units(x, .f88_hz_albic_e(x))
    above <- vapply(seq_len(x$n), function(i) {
      if (i == 1L) return(FALSE)
      .f88_any(ae[seq_len(i - 1L)])
    }, NA)
    sp <- .f88_num(x, "skeletans_pct"); st <- .f88_num(x, "skeletan_thickness_mm")
    sk <- .f88_and(.f88_cmp(sp, ">", 15), .f88_cmp(st, ">", 1))
    none <- is.na(sp) & is.na(st)
    sk[none] <- FALSE
    v <- .f88_and(bt, above, sk)
    flag <- ifelse(none & !(.f88_and(bt, above) %in% FALSE), "skeletans_not_recorded_absent", NA_character_)
    list(value = .f88_pr_clean(x, v, bury = TRUE), flags = flag,
         missing = c("skeletans_pct", "skeletan_thickness_mm"))
  }, max_top_cm)
}


# --- nitic properties (p. 38) -------------------------------------------------------------------

# 30 % clay or more; moderate or strong angular blocky structure breaking
# into flat-edged polyhedric ("nutty") elements (structure_type_secondary;
# or a polyhedral or nutty structure_type itself) with shiny ped faces (clay
# coatings or pressure faces, few or more). Field observations: a structure,
# grade or shiny faces not recorded are absent; but, as soilKey's WRB nitic
# horizon reads it, an angular blocky structure with no second-level
# structure recorded leaves the
# property undecided (Hugo, 2026-11-09). Oxalate iron above 0.2 % and at
# least 5 % of the DCB iron are read only where measured, and can reject
# (default 7); the text gives them as Fe2O3, so the 0.2 % is compared with
# fe_ox_pct (Fe) as 0.2 x 0.6994 = 0.14 % Fe (the ratio is the same in
# either form).
.f88_nitic_v <- function(x) {
  .f88_memo(x, "pr_nitic", function() {
    n <- x$n
    clay <- .f88_num(x, "clay_pct")
    st <- .field_term(.fao_col(x$h, "structure_type", "character"))
    st2 <- .field_term(.fao_col(x$h, "structure_type_secondary", "character"))
    gr <- .field_grade_class(.fao_col(x$h, "structure_grade", "character"))
    poly_re <- "polyhedr|polied|nutty|nucif|flat.edged"
    poly <- !is.na(st) & grepl(poly_re, st)
    blocky <- !is.na(st) & grepl("angular|block|bloco", st) & !grepl("subangular|subang", st)
    poly2 <- ifelse(is.na(st2), NA, grepl(poly_re, st2))
    grade <- !is.na(gr) & gr >= 2L
    cf <- .fao_col(x$h, "clay_films_amount", "character"); pf <- .fao_col(x$h, "pressure_faces", "character")
    shiny <- .amount_at_least(cf, "few") | .amount_at_least(pf, "few")
    struct <- ifelse(poly, TRUE, .f88_and(blocky, poly2))
    morph <- .f88_and(.f88_cmp(clay, ">=", 30), grade, struct, shiny)
    fo <- .f88_num(x, "fe_ox_pct"); fd <- .f88_num(x, "fe_dcb_pct")
    lab <- ifelse(is.na(fo), TRUE, ifelse(fo <= 0.2 * 0.6994, FALSE,
                  ifelse(is.na(fd) | fd <= 0, TRUE, fo / fd >= 0.05)))
    v <- .f88_and(morph, lab)
    list(value = .f88_pr_clean(x, v, bury = TRUE),
         missing = c("clay_pct", "structure_type_secondary", "fe_dcb_pct"))
  })
}

nitic_properties_fao88 <- function(pedon, max_top_cm = NULL) {
  .f88_pr_layers(pedon, "nitic_properties", .f88_nitic_v, max_top_cm)
}


# --- organic soil materials (p. 39) ---------------------------------------------------------------

# Saturated for long periods or artificially drained: OC 12 + 0.1 x clay %
# or more (12 to 18); never saturated more than a few days: 20 % or more
# (Q15; .f88_organic()).
organic_soil_materials_fao88 <- function(pedon, max_top_cm = NULL) {
  .f88_pr_layers(pedon, "organic_soil_materials", function(x) {
    o <- .f88_organic(x)
    list(value = .f88_pr_clean(x, o$value), missing = o$missing)
  }, max_top_cm)
}


# --- permafrost (p. 39) ----------------------------------------------------------------------------

# A layer perennially at or below 0 C: permafrost_temp_C of 0 or less; else a
# designation with the frozen suffix of its notation or ice recorded
# (ice_pct above 0), flagged. Not recorded: absent, flagged (a field observation; as the USDA
# permafrost of soilKey). The depth is read without the coverage rule: a
# description that ends above `max_top_cm` is not undecided (soilKey's
# reading).
permafrost_fao88 <- function(pedon, max_top_cm = NULL) {
  x <- .f88_ctx(pedon)
  id <- "permafrost"
  if (!x$n) return(.f88_pr_result(x, id, list(value = FALSE, layers = integer(0))))
  t <- .f88_num(x, "permafrost_temp_C")
  # the frozen suffix by the notation the site declares
  # (.frozen_designation_pattern(): f in the FAO, WRB and USDA notations, z
  # in the Canadian one); in another declared notation an f is not read and
  # leaves the layer undecided (Hugo, 2026-11-09)
  own <- .f88_fao_notation(x) ||
    !identical(.frozen_designation_pattern(x$site), .frozen_designation_pattern(list()))
  pat <- .frozen_designation_pattern(if (own) x$site else list())
  fz <- !is.na(x$desg) & grepl(pat, x$desg)
  ice <- .f88_num(x, "ice_pct")
  icey <- !is.na(ice) & ice > 0
  v <- ifelse(!is.na(t), t <= 0, ifelse(icey | (fz & own), TRUE, ifelse(fz, NA, FALSE)))
  flag <- rep(NA_character_, x$n)
  flag <- .f88_addf(flag, is.na(t) & fz & own, "permafrost_from_designation")
  flag <- .f88_addf(flag, is.na(t) & fz & !own & !icey, "frozen_suffix_other_notation_not_read")
  flag <- .f88_addf(flag, is.na(t) & !(fz & own) & icey, "permafrost_from_ice")
  v <- .f88_pr_clean(x, v)
  lim <- if (is.null(max_top_cm)) 125 else max_top_cm
  cand <- !is.na(x$top) & x$top <= lim
  hit <- which(v %in% TRUE & cand)
  und <- which(is.na(v) & cand)
  fl <- .f88_flags(flag, c(hit, und))
  if (!length(hit)) {
    if (!length(und)) fl <- c(fl, if (all(is.na(t))) "permafrost_not_recorded_absent")
    deepest <- suppressWarnings(max(x$bot, na.rm = TRUE))
    if (is.finite(deepest) && deepest < lim) fl <- c(fl, "description_ends_above_depth")
  }
  st <- list(value = if (length(hit)) TRUE else if (length(und)) NA else FALSE,
             layers = if (length(hit)) hit else und)
  .f88_pr_result(x, id, st, flags = fl, values = v, max_top_cm = max_top_cm,
                 evidence = list(temperature_regime_rules_out = .str_rules_out_permafrost(x$site)))
}


# --- plinthite (p. 39) -------------------------------------------------------------------------

# Iron-rich, humus-poor material that hardens irreversibly on repeated
# wetting and drying; the amount is set by the key. plinthite_pct above 0
# (`min_pct` NULL) or at least `min_pct`; not recorded, a designation with
# the suffix v (plinthite, in the FAO notation or none declared) is present
# with its amount unknown, flagged (an amount asked is then undecided); plinthite_hardens recorded FALSE is not
# plinthite. `min_thickness_cm` asks for a run of such layers that thick.
# Field observations: not recorded, absent.
plinthite_fao88 <- function(pedon, min_pct = NULL, min_thickness_cm = 0, max_top_cm = NULL) {
  .f88_pr_layers(pedon, "plinthite", function(x) {
    p <- .f88_num(x, "plinthite_pct")
    suf <- ifelse(is.na(x$desg), NA_character_, sub("^[A-Z]+(/[A-Z]+)?", "", x$desg))
    vd <- !is.na(suf) & grepl("v", suf)
    own <- .f88_fao_notation(x)
    v <- if (is.null(min_pct)) p > 0 else p >= min_pct
    dv <- is.na(p) & vd
    # the suffix v is plinthite only in the FAO notation (or none declared);
    # in another notation it leaves the layer undecided (Hugo, 2026-11-09)
    v[dv] <- if (is.null(min_pct) && own) TRUE else NA
    v[is.na(p) & !vd] <- FALSE
    flag <- ifelse(dv, if (own) "plinthite_from_designation_v" else "suffix_v_other_notation_not_read",
                   NA_character_)
    hd <- .f88_lgl(x, "plinthite_hardens")
    v[hd %in% FALSE] <- FALSE
    v <- .f88_pr_clean(x, v)
    if (min_thickness_cm > 0) {
      units <- .f88_units(x, v, function(r) r$thickness >= min_thickness_cm)
      v <- .f88_member(x, units)
    }
    list(value = v, flags = flag, missing = "plinthite_pct")
  }, max_top_cm)
}


# --- salic properties (p. 39) --------------------------------------------------------------------

# Within 30 cm of the surface, an EC of the saturation extract above 15
# dS/m, or above 4 dS/m where the pH (H2O 1:1) is above 8.5 (ph_h2o stands
# in, flagged, Q12). No thickness. ec_max_dS_m (an upper bound) can rule a
# layer out where ec_dS_m is not recorded.
.f88_salic_v <- function(x) {
  .f88_memo(x, "pr_salic", function() {
    ec <- .f88_num(x, "ec_dS_m"); em <- .f88_num(x, "ec_max_dS_m")
    ph <- .fao_ph(x$h, "1to1")
    alk <- .f88_cmp(ph$value, ">", 8.5)
    v <- .f88_or(.f88_cmp(ec, ">", 15), .f88_and(.f88_cmp(ec, ">", 4), alk))
    ub <- is.na(ec) & !is.na(em)
    v[ub & em <= 4] <- FALSE
    v[ub & em > 4 & em <= 15 & alk %in% FALSE] <- FALSE
    flag <- ifelse(!is.na(ec) & ec > 4 & ec <= 15, ph$flags, NA_character_)
    flag <- .f88_addf(flag, ub & !is.na(v), "ec_upper_bound_rules_out")
    list(value = .f88_pr_clean(x, v), flags = flag, missing = c("ec_dS_m", ph$missing))
  })
}

salic_properties_fao88 <- function(pedon, max_top_cm = 30) {
  .f88_pr_layers(pedon, "salic_properties", .f88_salic_v, max_top_cm)
}


# --- slickensides and smeary consistence (p. 39) ----------------------------------------------------

# Slickensides: any presence recorded (very few, few, distinct, yes ...)
# passes; none, absent, no or 0 does not; not recorded, absent, flagged
# (Hugo, 2026-11-09).
.F88_NONE_RE <- "^(none|absent|no|nenhum|nenhuma|ausente|ausentes|sem|0|false|f|nil|-)$"
slickensides_fao88 <- function(pedon, max_top_cm = NULL) {
  .f88_pr_layers(pedon, "slickensides", function(x) {
    s <- .fao_col(x$h, "slickensides", "character")
    rec <- !is.na(s) & nzchar(trimws(s))
    v <- rec & !grepl(.F88_NONE_RE, .field_term(s))
    flag <- ifelse(rec, NA_character_, "slickensides_not_recorded_absent")
    list(value = .f88_pr_clean(x, v), flags = flag)
  }, max_top_cm)
}

# Smeary (thixotropic) consistence: smeary_consistence; else a
# thixotropic_index of 50 or more, flagged; else absent (.fao_smeary()).
smeary_fao88 <- function(pedon, max_top_cm = NULL) {
  .f88_pr_layers(pedon, "smeary", function(x) {
    s <- .fao_smeary(x$h)
    list(value = .f88_pr_clean(x, s$value), flags = s$flags)
  }, max_top_cm)
}


# --- sodic properties (p. 40) -------------------------------------------------------------------

# Exchangeable Na 15 % or more of the exchange complex, or Na + Mg 50 % or
# more; the complex is the NH4OAc CEC (plan D20 a; esp_pct where Na or the
# CEC is missing, .f88_esp()).
.f88_sodic_v <- function(x) {
  .f88_memo(x, "pr_sodic", function() {
    e <- .f88_esp(x)
    na <- .f88_num(x, "na_cmol"); mg <- .f88_num(x, "mg_cmol"); cec <- .f88_num(x, "cec_cmol")
    nm <- ifelse(!is.na(cec) & cec > 0, 100 * (na + mg) / cec, NA_real_)
    v <- .f88_or(.f88_cmpr(e$value, ">=", 15), .f88_cmpr(nm, ">=", 50))
    list(value = .f88_pr_clean(x, v), flags = e$flags,
         missing = c("na_cmol", "mg_cmol", "cec_cmol"))
  })
}

sodic_properties_fao88 <- function(pedon, max_top_cm = NULL, window_cm = NULL) {
  .f88_pr_layers(pedon, "sodic_properties", .f88_sodic_v, max_top_cm, window_cm)
}


# --- soft powdery lime (p. 40) -------------------------------------------------------------------

# Soft secondary lime related to the structure: coatings on 50 % or more of
# the structural faces (carbonate_coatings_faces_pct), or soft nodules 5 % or
# more by volume (carbonate_soft_pct); coatings, soft concretions or powdery
# forms named in secondary_carbonates_form without the amount leave it
# undecided, flagged (Hugo, 2026-11-09). Pseudomycelia never count. Field
# observations: nothing recorded, absent, flagged.
.f88_spl_v <- function(x) {
  .f88_memo(x, "pr_spl", function() {
    cf <- .f88_num(x, "carbonate_coatings_faces_pct")
    sn <- .f88_num(x, "carbonate_soft_pct")
    form <- .f88_chr(x, "secondary_carbonates_form")
    pseudo <- !is.na(form) & grepl("pseudomyc", form)
    coat_form <- !is.na(form) & grepl("coating", form) & !pseudo
    soft_form <- !is.na(form) & grepl("soft|powder", form) & !pseudo
    flag <- rep(NA_character_, x$n)
    c1 <- ifelse(!is.na(cf), cf >= 50, ifelse(coat_form, NA, FALSE))
    flag <- .f88_addf(flag, is.na(cf) & coat_form, "coatings_named_without_amount")
    c2 <- ifelse(!is.na(sn), sn >= 5, ifelse(soft_form, NA, FALSE))
    v <- .f88_or(c1, c2)
    none <- is.na(cf) & is.na(sn) & is.na(form)
    flag <- .f88_addf(flag, none, "soft_powdery_lime_not_recorded_absent")
    list(value = .f88_pr_clean(x, v), flags = flag, missing = "carbonate_soft_pct")
  })
}

soft_powdery_lime_fao88 <- function(pedon, max_top_cm = NULL) {
  .f88_pr_layers(pedon, "soft_powdery_lime", .f88_spl_v, max_top_cm)
}


# --- strongly humic (p. 40) ----------------------------------------------------------------------

# More than 1.4 % organic carbon as the thickness-weighted mean over 0-100 cm,
# the depth below a soil on rock counting as zero (.fao_weighted(); Hugo,
# 2026-11-09, for soils shallower than 50 cm too).
strongly_humic_fao88 <- function(pedon) {
  x <- .f88_ctx(pedon)
  id <- "strongly_humic"
  if (!x$n) return(.f88_pr_result(x, id, list(value = FALSE, layers = integer(0))))
  w <- .fao_weighted(x$h, "oc_pct", 0, 100, zero_below_rock = TRUE)
  v <- .f88_cmpr(w$value, ">", 1.4)
  lay <- w$layers %||% integer(0)
  if (is.na(v)) {
    # a description that stops above 100 cm passes when the carbon of the
    # layers described already exceeds 1.4 % x 100 cm (the rest counts 0 or
    # more)
    oc <- .f88_num(x, "oc_pct")
    th <- pmax(pmin(x$bot, 100) - pmax(x$top, 0), 0)
    ok <- !is.na(th) & th > 0 & !x$rock & !is.na(oc)
    if (round(sum(oc[ok] * th[ok]), 6) > 140) {
      v <- TRUE; lay <- which(ok)
      w$flags <- c(w$flags, "carbon_sum_exceeds_140_before_100cm")
      w$missing <- character(0)
    }
  }
  st <- list(value = v, layers = lay)
  .f88_pr_result(x, id, st, flags = w$flags, missing = w$missing,
                 evidence = list(oc_weighted_0_100 = w$value), notes = w$note,
                 window_cm = c(0, 100))
}


# --- sulfidic materials (p. 40) ------------------------------------------------------------------

# 0.75 % sulfur or more (dry weight; s_total_pct, else sulfidic_s_pct plus
# sulfate_s_pct as a lower bound, flagged), less than three times as much
# CaCO3 equivalent as sulfur (caco3_pct, read as for the horizons) and a pH
# above 3.5 (ph_h2o; the ratio is not printed). The waterlogged state is
# reported (the site's drainage), not tested (Hugo, 2026-11-09). With only
# the lower bound of S, CaCO3 of three times it leaves the test undecided.
.f88_sulfidic_v <- function(x) {
  .f88_memo(x, "pr_sulfidic", function() {
    s <- .f88_num(x, "s_total_pct")
    sd <- .f88_num(x, "sulfidic_s_pct"); ss <- .f88_num(x, "sulfate_s_pct")
    lb <- ifelse(is.na(sd), NA_real_, sd + ifelse(is.na(ss), 0, ss))
    flag <- rep(NA_character_, x$n)
    s_ok <- .f88_cmp(s, ">=", 0.75)
    use_lb <- is.na(s) & !is.na(lb)
    s_ok[use_lb] <- ifelse(lb[use_lb] >= 0.75, TRUE, NA)
    flag <- .f88_addf(flag, use_lb, "sulfidic_plus_sulfate_s_lower_bound")
    sv <- ifelse(!is.na(s), s, lb)
    cc <- .f88_caco3(x)
    c_ok <- ifelse(is.na(cc$value) | is.na(sv), NA, round(cc$value - 3 * sv, 6) < 0)
    # with only the lower bound of S, a CCE of three times it does not refute
    c_ok[use_lb & c_ok %in% FALSE] <- NA
    ph <- .f88_num(x, "ph_h2o"); p1 <- .fao_ph(x$h, "1to1")
    ph[is.na(ph)] <- p1$value[is.na(ph)]
    v <- .f88_and(s_ok, as.logical(c_ok), .f88_cmp(ph, ">", 3.5))
    flag <- ifelse(is.na(flag), cc$flags, ifelse(is.na(cc$flags), flag, paste(flag, cc$flags, sep = "; ")))
    list(value = .f88_pr_clean(x, v), flags = flag,
         missing = c("s_total_pct", "caco3_pct", "ph_h2o"),
         evidence = list(saturated = .f88_saturated(x)))
  })
}

sulfidic_materials_fao88 <- function(pedon, max_top_cm = NULL) {
  .f88_pr_layers(pedon, "sulfidic_materials", .f88_sulfidic_v, max_top_cm)
}


# --- tonguing (p. 41) --------------------------------------------------------------------------

# kind "albic": tongues of an albic E into the argic B below it, deeper than
# wide, 5 / 10 / 15 mm or more across in a fine / moderately fine / medium or
# coarser argic B (.fao_tonguing_texture()), making more than 15 % of the
# upper part of the argic B (its first layer). kind "chernozem": tongues of
# the A into a cambic B or a C below it, deeper than wide, more than 15 % of
# the upper part of that horizon. The tongues are read on the receiving
# layer (tongue_vol_pct, tongue_width_mm, tongue_depth_cm), else on the layer
# above, flagged. Field observations: none recorded, absent; recorded in
# part, undecided.
tonguing_fao88 <- function(pedon, kind = c("albic", "chernozem"), max_top_cm = NULL) {
  kind <- match.arg(kind)
  .f88_pr_layers(pedon, "tonguing", function(x) {
    n <- x$n
    vol <- .f88_num(x, "tongue_vol_pct"); wd <- .f88_num(x, "tongue_width_mm")
    dp <- .f88_num(x, "tongue_depth_cm")
    flag <- rep(NA_character_, n)
    target <- rep(FALSE, n)
    if (kind == "albic") {
      ar <- .f88_hz_argic_natric(x, FALSE)
      ae <- .f88_in_units(x, .f88_hz_albic_e(x))
      for (u in ar$units) {
        i <- u$layers[1L]
        e_above <- if (i > 1L) ae[i - 1L] else FALSE
        target[i] <- .fao_and(if (isTRUE(u$status)) TRUE else NA, e_above)
      }
      tex <- .fao_tonguing_texture(x$h)
      need <- ifelse(tex == "fine", 5, ifelse(tex == "moderately fine", 10,
                     ifelse(tex == "medium or coarser", 15, NA_real_)))
    } else {
      a <- .f88_a_run(x)
      cb <- .f88_in_units(x, .f88_hz_cambic_b(x))
      if (!is.null(a)) {
        k <- max(a$layers) + 1L
        if (k <= n && !x$rock[k]) {
          target[k] <- .f88_or(cb[k], !is.na(x$master[k]) & grepl("^C", x$master[k]))
        }
      }
      need <- rep(0, n)
    }
    rd <- function(v) {
      out <- v
      for (i in seq_len(n)) if (is.na(out[i]) && i > 1L) {
        out[i] <- v[i - 1L]
        if (!is.na(out[i])) flag[i] <<- paste(c(flag[i][!is.na(flag[i])], "tongues_read_on_layer_above"),
                                             collapse = "; ")
      }
      out
    }
    vol2 <- rd(vol); wd2 <- rd(wd); dp2 <- rd(dp)
    rec <- !is.na(vol2) | !is.na(wd2) | !is.na(dp2)
    deeper <- ifelse(is.na(dp2) | is.na(wd2), NA, dp2 * 10 > wd2)
    wide <- if (kind == "albic") .f88_cmp(wd2, ">=", need) else rep(TRUE, n)
    tg <- .f88_and(.f88_cmp(vol2, ">", 15), deeper, wide)
    tg[!rec] <- FALSE
    flag <- .f88_addf(flag, !rec & !(target %in% FALSE), "tongues_not_recorded_absent")
    v <- .f88_and(target, tg)
    list(value = .f88_pr_clean(x, v, bury = TRUE), flags = flag,
         missing = c("tongue_vol_pct", "tongue_width_mm", "tongue_depth_cm"),
         evidence = list(kind = kind))
  }, max_top_cm)
}


# --- vertic properties (p. 41) -----------------------------------------------------------------

# Clayey (30 % clay or more, default 8) with, at some period in most years,
# one or more of cracks (cracks_width_cm above 0), slickensides (few or more)
# or wedge-shaped or parallelepiped aggregates (structure_type). Short of a
# Vertisol: the key reads the Vertisols first. Field observations: not
# recorded, absent.
.f88_vertic_v <- function(x) {
  .f88_memo(x, "pr_vertic", function() {
    clay <- .f88_num(x, "clay_pct")
    cw <- .f88_num(x, "cracks_width_cm")
    s <- .fao_col(x$h, "slickensides", "character")
    st <- .field_term(.fao_col(x$h, "structure_type", "character"))
    feat <- (!is.na(cw) & cw > 0) |
      (!is.na(s) & nzchar(trimws(s)) & !grepl(.F88_NONE_RE, .field_term(s))) |
      (!is.na(st) & grepl("wedge|parallelepiped|parallelipiped|lenticular|cunei", st))
    v <- .f88_and(.f88_cmp(clay, ">=", 30), feat)
    list(value = .f88_pr_clean(x, v), missing = "clay_pct")
  })
}

vertic_properties_fao88 <- function(pedon, max_top_cm = NULL) {
  .f88_pr_layers(pedon, "vertic_properties", .f88_vertic_v, max_top_cm)
}


# --- weatherable minerals (p. 41) ----------------------------------------------------------------

# The weatherable minerals of the 0.02-0.2 mm fraction (feldspars,
# feldspathoids, ferromagnesians, glasses, micas, zeolites;
# weatherable_minerals_pct, recorded for 20-200 um, the fraction of p. 41),
# `min_pct` or more in a layer. The weatherable clays (all 2:1 clays but
# Al-interlayered chlorite; sepiolite, talc, glauconite) are reported from
# clay_mineralogy, not counted in the per cent.
weatherable_minerals_fao88 <- function(pedon, min_pct = 10, max_top_cm = NULL) {
  .f88_pr_layers(pedon, "weatherable_minerals", function(x) {
    wm <- .f88_num(x, "weatherable_minerals_pct")
    cm <- .f88_chr(x, "clay_mineralogy")
    two1 <- !is.na(cm) & grepl("smectit|illit|vermicul|interstrat|mixed|montmor|sepiol|talc|glauconit", cm)
    list(value = .f88_pr_clean(x, .f88_cmp(wm, ">=", min_pct)),
         missing = "weatherable_minerals_pct",
         evidence = list(weatherable_clays = data.frame(layer = x$idx, two_to_one = two1)))
  }, max_top_cm)
}
