# =============================================================================
# v0.9.386 -- the phases of the FAO-Unesco Revised Legend (FAO 1988, World
# Soil Resources Report 60; the text of the ISRIC reprint of 1997, Technical
# Paper 20), chapter VIII, pp. 67-71, and the texture and slope classes of
# the map symbol (Appendix 2, p. 107).
#
# Phases are features of the land that may limit its use; they cut across
# the soil units and never define one (p. 67). classify_fao() gives them in
# `$phases`, beside the unit: one row per phase (16) and one per class of
# the map symbol (texture, slope), each three-valued, never changing the
# unit (Hugo's decision Q25 of 2026-11-08). The criteria, pages and numbers
# are in inst/rules/fao1988/phases.yaml (the numbers also in the `phases`
# block of thresholds.yaml); the readings in readings.yaml.
#
# Conventions: depths from the soil surface (Q9); "within X cm" by the top
# of the feature (default 11), fails only when the description reaches below
# X cm (or rock above it); a field observation not recorded is absent, a
# measurement not recorded leaves the phase undecided (default 16); the
# duripan, fragipan, petroferric and placic phases read soilKey's USDA
# predicates (Soil Taxonomy 1975, p. 67) with the depths of the legend, the
# pan kind recorded in the field, and the WRB horizons only as stand-ins,
# flagged (default 17). A phase the text does not show for some units is
# `not shown` there; when such a unit is only a candidate (undecided before
# the one assigned) a phase whose criterion is met is undecided.
# =============================================================================

.f88ph_cache <- new.env(parent = emptyenv())

# The phase rules (inst/rules/fao1988/phases.yaml), read once.
.fao88_phase_rules <- function() {
  if (is.null(.f88ph_cache$rules)) {
    f <- system.file("rules", "fao1988", "phases.yaml", package = "soilKey")
    .f88ph_cache$rules <- yaml::read_yaml(f)
  }
  .f88ph_cache$rules
}

# A phase answer: value TRUE / FALSE / NA, the layers (rows of the pedon's
# horizon table) that decide it, the fallbacks used, what would decide it.
.f88ph_ans <- function(value, layers = integer(0), flags = character(0),
                       missing = character(0), note = character(0)) {
  value <- if (length(value) == 1L) as.logical(value) else NA
  list(value = value, layers = layers,
       flags = unique(as.character(flags[!is.na(flags) & nzchar(flags)])),
       missing = if (is.na(value)) unique(as.character(missing[!is.na(missing)])) else character(0),
       note = unique(as.character(note)))
}

# A per-layer answer (rows of the ordered context x$h) read "within X cm"
# (.fao_within()); the layers returned as rows of the pedon.
.f88ph_within <- function(x, v, depth, flags = character(0), missing = character(0)) {
  v <- as.logical(v)
  v[x$rock] <- FALSE
  w <- .fao_within(x$h, v, depth)
  lay <- x$idx[w$layers %||% integer(0)]
  rel <- if (isTRUE(w$value)) w$layers else seq_len(x$n)
  .f88ph_ans(w$value, lay, flags = c(.f88_flags(flags, rel), w$flags),
             missing = c(missing, w$missing))
}

# A site text, lower case; NA when not recorded.
.f88ph_site_txt <- function(x, name) {
  v <- .f88_site(x, name)
  if (is.null(v) || length(v) != 1L || is.na(v)) return(NA_character_)
  v <- tolower(trimws(as.character(v)))
  if (!nzchar(v) || v %in% c("na", "nan", "null")) NA_character_ else v
}
.f88ph_site_num <- function(x, name) {
  suppressWarnings(as.numeric(.f88_site(x, name)))
}

# The first mineral layer (not an H or O, not rock): the surface horizon of
# the takyric and yermic phases. NA when there is none.
.f88ph_surface <- function(x) {
  if (!x$n) return(NA_integer_)
  oc <- .f88_num(x, "oc_pct")
  organic <- (!is.na(x$master) & grepl("^(O|H)", x$master)) |
    (is.na(x$master) & !is.na(oc) & oc >= 20)
  which(!organic & !x$rock & !is.na(x$top))[1L]
}

# A pan read three ways (default 17): the USDA predicate (its layers), the
# pan kind recorded in the field (pan_type matching `pan_re`), a designation
# matching `desg_re` in the FAO notation (or none declared), flagged; when
# none of them shows the pan, the WRB horizon `wrb_fun` as a stand-in,
# flagged, in the layers where `open(x)` says the attributes the USDA
# predicate reads are not recorded. A pan not recorded is absent (a field
# observation).
.f88ph_pan <- function(x, id, depth, usda_fun, pan_re, desg_re = NULL, wrb_fun = NULL,
                       wrb_name = NULL, open = NULL, usda_check = NULL, exclude = NULL,
                       missing = character(0)) {
  if (!x$n) return(.f88ph_ans(FALSE))
  v <- rep(FALSE, x$n)
  flag <- rep(NA_character_, x$n)
  u <- tryCatch(usda_fun(x$pedon, max_top_cm = 1e6), error = function(e) NULL)
  nasis <- FALSE
  # the USDA layers count only when the predicate passes (a firm layer too
  # thin for a fragipan is no fragipan)
  if (!is.null(u) && isTRUE(u$passed)) {
    sel <- match(u$layers, x$idx, nomatch = 0L)
    if (is.null(usda_check)) v[sel] <- TRUE
    else { ck <- usda_check(x, sel); v[sel] <- ck$value; flag[sel] <- ck$flag }
    nasis <- !length(u$layers)
  }
  pan <- .f88_chr(x, "pan_type")
  vp <- !is.na(pan) & grepl(pan_re, pan) & !(v %in% TRUE)
  v[vp] <- TRUE; flag[vp] <- paste0("pan_type_", id)
  if (!is.null(desg_re) && .f88_fao_notation(x)) {
    d <- .fao_col(x$h, "designation", "character")
    hit <- if (is.function(desg_re)) desg_re(x) else !is.na(d) & grepl(desg_re, d)
    vd <- hit %in% TRUE & !(v %in% TRUE)
    v[vd] <- TRUE; flag[vd] <- paste0(id, "_from_designation")
  }
  if (!is.null(exclude)) { ex <- exclude(x) %in% TRUE; v[ex] <- FALSE }
  if (nasis && !any(v %in% TRUE)) {
    # a NASIS feature without its depth: present, but within 100 cm or not
    # is not known
    return(.f88ph_ans(NA, flags = "nasis_feature_no_depth", missing = "top_cm",
                      note = "a NASIS diagnostic feature, depth not recorded"))
  }
  if (!any(v %in% TRUE) && !is.null(wrb_fun)) {
    w <- tryCatch(wrb_fun(x$pedon), error = function(e) NULL)
    if (!is.null(w) && isTRUE(w$passed)) {
      vw <- match(w$layers, x$idx, nomatch = 0L)
      # only in layers where the USDA predicate's own attributes are not
      # recorded: a stand-in never overrules the definition of 1975
      if (!is.null(open)) vw <- vw[open(x)[vw] %in% TRUE]
      if (!is.null(exclude)) vw <- vw[!(exclude(x)[vw] %in% TRUE)]
      v[vw] <- TRUE; flag[vw] <- paste0("wrb_", wrb_name, "_stand_in")
    }
  }
  r <- .f88ph_within(x, v, depth, flags = flag, missing = missing)
  # nothing shows the pan: absent, flagged, when the description decides it
  if (!any(v %in% c(TRUE, NA)) && isFALSE(r$value)) r$flags <- unique(c(r$flags, paste0(id, "_not_recorded_absent")))
  r
}


# --- the sixteen phases (pp. 67-71) -------------------------------------------------------

# Anthraquic (p. 67): stagnic properties within 50 cm and the irrigation
# history of the site: flood irrigation, a share of years under rice, or a
# land use naming rice or paddy; irrigation recorded without its kind is
# undecided; nothing recorded, absent (a site history is a field record).
.f88ph_anthraquic <- function(x, a) {
  st <- stagnic_properties_fao88(x$pedon, max_top_cm = a$stagnic_within_cm)
  fi <- .f88_site_lgl(x, "flood_irrigated")
  ir <- .f88_site_lgl(x, "irrigated")
  py <- .f88ph_site_num(x, "paddy_years_frac")
  lu <- .f88ph_site_txt(x, "land_use")
  rice <- !is.na(lu) && grepl("rice|paddy|arroz|sawah", lu)
  fl <- character(0); miss <- character(0)
  # a recorded "not irrigated" or "not flood irrigated" decides first
  irr <- if (isFALSE(ir) || isFALSE(fi)) FALSE
         else if (isTRUE(fi) || (!is.na(py) && py > 0)) TRUE
         else if (rice) { fl <- "rice_land_use_for_irrigation"; TRUE }
         else if (isTRUE(ir)) { miss <- "flood_irrigated"; NA }
         else if (!is.na(py) && py == 0) FALSE
         else { fl <- "irrigation_history_not_recorded_absent"; FALSE }
  v <- .fao_and(st$passed, irr)
  .f88ph_ans(v, layers = if (isTRUE(v)) st$layers else integer(0),
             flags = c(fl, if (!isFALSE(v)) st$evidence$flags),
             missing = c(if (is.na(st$passed)) st$missing, miss))
}

.f88ph_duripan <- function(x, a) {
  .f88ph_pan(x, "duripan", a$within_cm, duripan_usda, "duripan|silcrete|duric",
             desg_re = "qm", wrb_fun = petroduric, wrb_name = "petroduric",
             open = function(x) is.na(.f88_chr(x, "cementation_class")) | is.na(.f88_num(x, "duripan_pct")))
}

# Fragipan (pp. 67-68; Hugo, 2026-11-09): the firm layers of
# fragipan_usda() are a sign only (flagged
# usda_fragipan_rupture_resistance_only, undecided) unless a second sign holds
# in the layer: very or extremely firm with a massive structure, or a bulk
# density of `bd_min` g/cm3 or more (the high bulk density of p. 67 has no
# number; 1.65 is the number of soilKey's WRB fragic(), the fragic horizon
# of WRB 2022, Ch. 3.1.10). A designation with x (E/Bx too) or pan_type
# fragipan is a fragipan by itself.
.f88ph_fragipan <- function(x, a, bd_min = 1.65) {
  check <- function(x, sel) {
    rr <- .f88_chr(x, "rupture_resistance")[sel]
    st <- paste(.f88_chr(x, "structure_type")[sel], .f88_chr(x, "structure_grade")[sel])
    bd <- .f88_num(x, "bulk_density_g_cm3")[sel]
    second <- (rr %in% c("very firm", "extremely firm") & grepl("massive", st)) |
      (!is.na(bd) & bd >= bd_min)
    list(value = ifelse(second, TRUE, NA),
         flag = ifelse(second, NA_character_, "usda_fragipan_rupture_resistance_only"))
  }
  .f88ph_pan(x, "fragipan", a$within_cm, fragipan_usda, "fragipan",
             desg_re = "(^|[/&])[0-9]*[A-Z]+[a-z]*x", wrb_fun = fragic, wrb_name = "fragic",
             open = function(x) is.na(.f88_chr(x, "rupture_resistance")),
             usda_check = check, missing = c("bulk_density_g_cm3", "structure_type"))
}

# Petroferric (p. 68; Hugo, 2026-11-09): iron must be shown: plinthite_pct
# of 50 or more, pan_type ironstone or ferricrete, or a designation with m
# and v, c or s (thicker than a placic pan of 2.5 cm); a layer cemented by
# carbonates, gypsum or silica (designation km, ym, qm, or pan_type calcrete,
# gypcrete, silcrete, duripan) is not petroferric.
.f88ph_iron_desg <- function(x) {
  suf <- sub("^[0-9]*[A-Z]+(/[A-Z]+)?", "", .fao_col(x$h, "designation", "character"))
  th <- x$bot - x$top
  !is.na(suf) & grepl("m", suf) & grepl("[vcs]", suf) & !grepl("[kyq]", suf) &
    !is.na(th) & th > 2.5
}
.f88ph_petroferric <- function(x, a) {
  iron_pan <- "ferricrete|ironstone|petroferric|laterite crust|canga"
  check <- function(x, sel) {
    pl <- .f88_num(x, "plinthite_pct")[sel]
    pan <- .f88_chr(x, "pan_type")[sel]
    iron <- (!is.na(pl) & pl >= 50) | (!is.na(pan) & grepl(iron_pan, pan)) | .f88ph_iron_desg(x)[sel]
    list(value = iron, flag = ifelse(iron, NA_character_, "petroferric_without_iron"))
  }
  excl <- function(x) {
    suf <- sub("^[0-9]*[A-Z]+(/[A-Z]+)?", "", .fao_col(x$h, "designation", "character"))
    pan <- .f88_chr(x, "pan_type")
    (!is.na(suf) & grepl("m", suf) & grepl("[kyq]", suf)) |
      (!is.na(pan) & grepl("calcrete|gypcrete|silcrete|duripan|petrocalcic|petrogypsic", pan))
  }
  .f88ph_pan(x, "petroferric", a$within_cm, petroferric_contact_usda, iron_pan,
             desg_re = .f88ph_iron_desg, wrb_fun = petroplinthic, wrb_name = "petroplinthic",
             open = function(x) is.na(.f88_chr(x, "cementation_class")) |
               (is.na(.f88_num(x, "plinthite_pct")) & is.na(.f88_num(x, "coarse_fragments_pct"))),
             usda_check = check, exclude = excl)
}

.f88ph_placic <- function(x, a) {
  .f88ph_pan(x, "placic", a$within_cm, placic_horizon_usda, "thin ?iron ?pan|placic")
}

# Gelundic (p. 68): frost polygons at the surface, from patterned_ground.
.f88ph_gelundic <- function(x, a) {
  pg <- .f88ph_site_txt(x, "patterned_ground")
  if (is.na(pg)) return(.f88ph_ans(FALSE, flags = "patterned_ground_not_recorded_absent"))
  .f88ph_ans(grepl("polygon|pol\u00edgono|poligono", pg))
}

# Gilgai (p. 68): gilgai_presence; else a microrelief recorded at the site
# or on the surface layer naming gilgai (any other recorded microrelief
# rules it out); else absent.
.f88ph_gilgai <- function(x, a) {
  g <- .f88_site_lgl(x, "gilgai_presence")
  if (!is.na(g)) return(.f88ph_ans(g))
  mr <- .f88ph_site_txt(x, "microrelief_form")
  if (is.na(mr) && x$n) {
    s <- .f88ph_surface(x)
    if (!is.na(s)) mr <- .f88_chr(x, "microrelief_form")[s]
  }
  if (is.na(mr)) return(.f88ph_ans(FALSE, flags = "gilgai_not_recorded_absent"))
  .f88ph_ans(grepl("gilgai", mr), flags = "gilgai_from_microrelief")
}

# Inundic (p. 68): more than 10 days of surface water in the growing period.
.f88ph_inundic <- function(x, a) {
  d <- .f88ph_site_num(x, "flooding_days_growing_period")
  .f88ph_ans(if (is.na(d)) NA else d > a$days_gt, missing = "flooding_days_growing_period")
}

# Lithic (p. 68): continuous hard rock within 50 cm.
.f88ph_lithic <- function(x, a) {
  r <- continuous_hard_rock_fao88(x$pedon, max_top_cm = a$within_cm)
  .f88ph_ans(r$passed, layers = r$layers, flags = r$evidence$flags, missing = r$missing)
}

# Phreatic (p. 69; Q25): a groundwater table within 500 cm and no gleyic
# properties (the water table not reflected in the morphology); the
# exclusion of the Fluvisols and Gleysols is a "not shown for" rule.
.f88ph_phreatic <- function(x, a) {
  gw <- .f88ph_site_num(x, "groundwater_depth_cm")
  crit <- if (is.na(gw)) NA else gw <= a$within_cm
  gl <- if (isFALSE(crit)) NULL else gleyic_properties_fao88(x$pedon)
  v <- .fao_and(crit, if (is.null(gl)) NA else .f88_not(gl$passed))
  if (isFALSE(crit)) v <- FALSE
  .f88ph_ans(v, missing = c(if (is.na(crit)) "groundwater_depth_cm",
                            if (!is.null(gl) && is.na(gl$passed)) gl$missing),
             flags = if (!is.null(gl) && !isFALSE(gl$passed)) gl$evidence$flags,
             note = if (!is.null(gl) && isTRUE(gl$passed)) "gleyic properties: the water table shows in the morphology")
}

# Rudic (p. 69): the field record rudic_recorded; not recorded, absent.
.f88ph_rudic <- function(x, a) {
  r <- .f88_site_lgl(x, "rudic_recorded")
  if (is.na(r)) return(.f88ph_ans(FALSE, flags = "rudic_not_recorded_absent"))
  .f88ph_ans(r)
}

# The salic phase criterion per layer (pp. 69-70): an ECe above 4 dS/m;
# ec_max_dS_m (an upper bound) of 4 or less rules the layer out.
.f88ph_salic_v <- function(x, a) {
  ec <- .f88_num(x, "ec_dS_m"); em <- .f88_num(x, "ec_max_dS_m")
  v <- ec > a$ece_gt_ds_m
  ub <- is.na(ec) & !is.na(em) & em <= a$ece_gt_ds_m
  v[ub] <- FALSE
  list(value = v, flags = ifelse(ub, "ec_upper_bound_rules_out", NA_character_))
}
.f88ph_salic <- function(x, a) {
  if (!x$n) return(.f88ph_ans(FALSE))
  s <- .f88ph_salic_v(x, a)
  .f88ph_within(x, s$value, a$within_cm, flags = s$flags, missing = "ec_dS_m")
}

# Skeletic (p. 70): a run of layers with 40 % or more coarse fragments by
# volume, 25 cm thick or more, its top within 50 cm; rock and strongly
# cemented or indurated layers are not part of it (not continuously
# cemented). A run that reaches the base of the description, thinner than 25
# cm, is undecided.
.f88ph_skeletic <- function(x, a) {
  if (!x$n) return(.f88ph_ans(FALSE))
  cf <- .f88_num(x, "coarse_fragments_pct")
  cem <- .f88_chr(x, "cementation_class")
  ok <- cf >= a$coarse_min_pct_vol
  # coarse fragments not recorded: absent, flagged (Hugo, 2026-11-09)
  nrec <- is.na(cf) & !x$rock
  ok[nrec] <- FALSE
  ok[!is.na(cem) & cem %in% c("strongly", "indurated")] <- FALSE
  ok[x$rock] <- FALSE
  base <- suppressWarnings(max(x$bot, na.rm = TRUE))
  test_run <- function(r) {
    if (r$top > a$within_cm) return(FALSE)
    if (r$thickness >= a$thickness_min_cm) return(TRUE)
    last <- max(r$layers)
    open <- isTRUE(abs(r$bottom - base) < 0.5) && !any(x$rock[x$top >= r$bottom - 0.5], na.rm = TRUE)
    if (open && last == max(which(!is.na(x$bot)))) NA else FALSE
  }
  units <- .f88_units(x, ok, test_run)
  sure <- Filter(function(u) isTRUE(u$status), units)
  nfl <- if (any(nrec)) "coarse_fragments_not_recorded_absent" else character(0)
  if (length(sure)) return(.f88ph_ans(TRUE, layers = x$idx[sure[[1L]]$layers]))
  if (length(units)) {
    lay <- unique(unlist(lapply(units, `[[`, "layers")))
    # coarse fragments not recorded, or a sure run open at the base of the
    # description
    open_run <- any(vapply(units, function(u) all(ok[u$layers] %in% TRUE), NA))
    return(.f88ph_ans(NA, layers = x$idx[lay],
                      missing = c(if (anyNA(ok[lay])) "coarse_fragments_pct",
                                  if (open_run) "bottom_cm"), flags = nfl))
  }
  .f88ph_ans(FALSE, flags = nfl)
}

# Sodic (p. 70): more than 6 % exchangeable sodium within 100 cm (esp_pct,
# else Na / CEC x 100, flagged).
.f88ph_sodic <- function(x, a) {
  if (!x$n) return(.f88ph_ans(FALSE))
  e <- .f88_esp(x)
  .f88ph_within(x, e$value > a$esp_gt_pct, a$within_cm, flags = e$flags,
                missing = c("esp_pct", "na_cmol", "cec_cmol"))
}

# Takyric (p. 70): in the first mineral layer, heavy texture (clay above 35
# %, soilKey's reading), polygonal cracks recorded and a platy or massive
# structure (the crust); cracks or crust not recorded, absent.
.f88ph_takyric <- function(x, a) {
  s <- .f88ph_surface(x)
  if (is.na(s)) return(.f88ph_ans(FALSE))
  fl <- character(0)
  sp <- .f88_num(x, "polygonal_cracks_spacing_cm")[s]
  poly <- !is.na(sp) && sp > 0
  if (is.na(sp)) fl <- c(fl, "polygonal_cracks_not_recorded_absent")
  st <- .f88_chr(x, "structure_type")[s]
  crust <- !is.na(st) && grepl("platy|massive|laminar|maci", st)
  if (is.na(st)) fl <- c(fl, "surface_structure_not_recorded_absent")
  # heavy textured: the fine class (3) of the upper 30 cm (p. 107; Hugo,
  # 2026-11-09)
  t <- .fao_texture_upper30(x$h, "1988", site = x$site)
  heavy <- if (is.na(t$value)) NA else identical(t$value, "fine")
  v <- .fao_and(poly, crust, heavy)
  .f88ph_ans(v, layers = if (isTRUE(v)) x$idx[s] else integer(0), flags = c(fl, t$flags),
             missing = c("clay_pct", "sand_pct", t$missing))
}

# Yermic (pp. 70-71): the organic carbon of the surface 18 cm mixed below 0.6
# % (0.20 % where coarser than sandy loam; waived when the salic phase
# criterion is met) and one of the six desert features, each a field record
# (not recorded, absent).
.f88ph_yermic <- function(x, a, salic = NA) {
  if (!x$n) return(.f88ph_ans(FALSE))
  fl <- character(0); miss <- character(0)
  # (a) organic carbon
  if (isTRUE(salic)) {
    oc_ok <- TRUE; fl <- c(fl, "organic_carbon_waived_salic")
  } else {
    oc <- .fao_mixed(x$h, "oc_pct", a$mixed_cm)
    p <- .fao_psd(x$h)
    cl <- .fao_weighted(x$h, "clay_pct", 0, a$mixed_cm, values = p$clay)$value
    si <- .fao_weighted(x$h, "silt_pct", 0, a$mixed_cm, values = p$silt)$value
    coarse <- if (is.na(cl) || is.na(si)) NA else (si + 2 * cl) < 30
    o <- oc$value
    oc_ok <- if (is.na(o)) NA
             else if (o < a$oc_lt_pct_coarse) TRUE
             else if (o >= a$oc_lt_pct) FALSE
             else if (is.na(coarse)) NA else !coarse
    if (is.na(oc_ok)) miss <- c(miss, if (is.na(o)) c("oc_pct", oc$missing) else c("clay_pct", "silt_pct", "sand_pct"))
    # the carbon fails, but an undecided salic criterion could waive it
    if (isFALSE(oc_ok) && is.na(salic)) { oc_ok <- NA; miss <- c(miss, "ec_dS_m") }
  }
  # (b) the six features
  s <- .f88ph_surface(x)
  f <- c(varnish = FALSE, quartz = FALSE, palygorskite = FALSE, cracks = FALSE,
         platy = FALSE, blown_sand = FALSE)
  if (!is.na(s)) {
    ve <- .f88_num(x, "ventifact_pct")[s]; va <- .f88_num(x, "varnish_pct")[s]
    f["varnish"] <- (!is.na(ve) && ve > 0) || (!is.na(va) && va > 0)
    pq <- .f88_num(x, "pitted_quartz_pct")[s]
    f["quartz"] <- !is.na(pq) && pq >= a$pitted_quartz_min_pct
    st <- .f88_chr(x, "structure_type")[s]
    f["platy"] <- !is.na(st) && grepl("platy|laminar", st)
    ae <- .f88_chr(x, "aeolian_morphology")[s]
    # blown sand on a stable surface: a sand sheet or cover sand, not a dune
    # or shifting sand, and not a negation
    f["blown_sand"] <- !is.na(ae) &&
      grepl("sand ?sheet|blown sand|cover ?sand|aeolian sand|eolian sand|areia e[o\u00f3]lica", ae) &&
      !grepl("dune|duna|shifting|mobile|\\bno\\b|\\bnot\\b|none|absent|without|\\bsem\\b|\\bn\u00e3o\\b", ae)
  }
  pg <- .fao_palygorskite(x$h)
  near <- !is.na(x$top) & x$top <= a$palygorskite_within_cm & !x$rock
  hit <- near & pg$value >= a$palygorskite_min_pct
  f["palygorskite"] <- any(hit)
  if (any(hit)) fl <- c(fl, pg$flags[hit])
  ci <- .fao_field_obs(x$h, "aeolian_crack_infill")
  f["cracks"] <- any(ci$value %in% TRUE)
  feat <- any(f)
  rec <- function(nm) any(!is.na(.fao_col(x$h, nm, "character")))
  recorded <- any(vapply(c("ventifact_pct", "varnish_pct", "pitted_quartz_pct", "palygorskite_pct",
                           "clay_mineralogy", "aeolian_crack_infill", "structure_type",
                           "aeolian_morphology"), rec, NA))
  if (!feat && !recorded) fl <- c(fl, "yermic_features_not_recorded_absent")
  v <- .fao_and(oc_ok, feat)
  .f88ph_ans(v, flags = fl, missing = miss,
             note = if (feat) paste("features:", paste(names(f)[f], collapse = ", ")) else character(0))
}


# --- the classes of the map symbol (p. 107) -----------------------------------------------

# The texture class of the upper 30 cm: 1 coarse, 2 medium, 3 fine.
.f88ph_texture <- function(x, m) {
  t <- .fao_texture_upper30(x$h, "1988", depth = m$texture$upper_cm, site = x$site)
  cls <- Find(function(k) identical(k$id, t$value), m$texture$classes)
  list(code = if (is.null(cls)) NA_character_ else cls$code, class = cls,
       flags = t$flags, missing = t$missing,
       basis = if (is.null(cls)) "" else sprintf("clay %.1f %%, sand %.1f %% in the upper %d cm",
                                                t$clay, t$sand, as.integer(m$texture$upper_cm)))
}

# The slope class: a up to 8 %, b more than 8 up to 30 %, c more than 30 %
# (the printed ranges share their ends; soilKey's reading puts 8 in a and 30
# in b).
.f88ph_slope <- function(x, m) {
  s <- .f88ph_site_num(x, "slope_pct")
  cls <- NULL
  if (!is.na(s)) {
    for (k in m$slope$classes) if (is.null(k$max_pct) || s <= k$max_pct) { cls <- k; break }
  }
  list(code = if (is.null(cls)) NA_character_ else cls$code, class = cls,
       flags = character(0), missing = if (is.null(cls)) "slope_pct" else character(0),
       basis = if (is.null(cls)) "" else sprintf("slope %s %%", format(s)))
}


# --- the "not shown for" rules ------------------------------------------------------------------

# The grouping and unit assigned, and the candidates undecided before them.
.f88ph_assigned <- function(res) {
  lv <- res$levels
  g <- u <- NA_character_
  if (is.data.frame(lv) && nrow(lv)) {
    g <- lv$code[lv$level == "major_groups"][1L]
    u <- lv$code[lv$level == "units"][1L]
  }
  pb <- as.character(res$provisional_because %||% character(0))
  cg <- sub("^major_groups:", "", pb[startsWith(pb, "major_groups:")])
  cu <- sub("^units:", "", pb[startsWith(pb, "units:")])
  if (is.na(g)) {
    tr <- res$trace$major_groups %||% list()
    st <- vapply(tr, function(e) { p <- e$passed; if (length(p) == 1L) as.logical(p) else NA }, NA)
    cg <- unique(c(cg, names(st)[is.na(st)]))
  }
  list(grouping = g, unit = u, cand_groupings = cg, cand_units = cu)
}

# TRUE when the phase is not shown for the soil assigned, NA when it is not
# shown for a candidate, FALSE otherwise. `glen`: the length of the group
# code at the head of a unit code (2 in 1988, "LVx"; 1 in 1974, "Lc").
.f88ph_excluded <- function(rule, asg, glen = 2L) {
  if (is.null(rule)) return(FALSE)
  gs <- as.character(unlist(rule$groupings)); us <- as.character(unlist(rule$units))
  if (asg$grouping %in% gs || asg$unit %in% us) return(TRUE)
  cg <- asg$cand_groupings[asg$cand_groupings %in% gs]
  cu <- asg$cand_units[asg$cand_units %in% us | substr(asg$cand_units, 1L, glen) %in% gs]
  cand <- c(if (length(cg)) paste0("major_groups:", cg), if (length(cu)) paste0("units:", cu))
  if (length(cand)) return(structure(NA, candidates = cand))
  FALSE
}


# --- all phases ------------------------------------------------------------------------------------

# The phases of a pedon and its classify_fao() result (edition 1988): a
# data.frame with one row per phase and one per class of the map symbol:
# kind ("phase", "texture_class", "slope_class"), code, name, name_native
# (the English name, the legend's language), name_pt, status ("present",
# "absent", "undecided", "not shown"), present (TRUE / FALSE / NA, after the
# "not shown for" rules), criterion (TRUE / FALSE / NA, the criterion alone),
# page, basis, flags, missing (an attribute, or the candidate unit that
# leaves a "not shown for" rule open, as major_groups:SC), layers and note
# ("; " lists).
.fao88_phases <- function(pedon, res) {
  rules <- .fao88_phase_rules()
  x <- .f88_ctx(pedon)
  asg <- .f88ph_assigned(res)
  ans <- list()
  for (ph in rules$phases) {
    id <- ph$id
    fun <- get(paste0(".f88ph_", id), mode = "function")
    a <- tryCatch(
      if (id == "yermic") fun(x, ph, salic = ans$salic$value) else fun(x, ph),
      error = function(e) .f88ph_ans(NA, flags = sprintf("phase_not_read: %s", conditionMessage(e)),
                                     note = sprintf("not read: %s", conditionMessage(e))))
    ans[[id]] <- a
  }
  rows <- lapply(rules$phases, function(ph) {
    a <- ans[[ph$id]]
    ex <- .f88ph_excluded(ph$not_shown_for, asg)
    crit <- a$value
    present <- if (isFALSE(crit) || isTRUE(ex)) FALSE
               else if (isTRUE(crit) && isFALSE(ex)) TRUE else NA
    status <- if (isTRUE(present)) "present"
              else if (isTRUE(ex) && !isFALSE(crit)) "not shown"
              else if (isFALSE(present)) "absent" else "undecided"
    data.frame(kind = "phase", code = ph$id, name = ph$name, name_native = ph$name,
               name_pt = ph$name_pt, status = status, present = present, criterion = crit,
               page = as.character(ph$page), basis = ph$criterion,
               flags = paste(a$flags, collapse = "; "),
               missing = paste(if (is.na(present))
                                 unique(c(a$missing, if (isTRUE(crit)) attr(ex, "candidates")))
                               else character(0), collapse = "; "),
               layers = paste(sort(unique(a$layers)), collapse = "; "),
               note = paste(c(a$note, if (is.na(present) && isTRUE(crit) && length(attr(ex, "candidates")))
                 "not shown for a candidate unit tested before the one assigned"), collapse = "; "),
               stringsAsFactors = FALSE)
  })
  m <- rules$map_symbol
  tex <- if (x$n) .f88ph_texture(x, m)
         else list(code = NA_character_, flags = character(0), missing = "clay_pct", basis = "")
  # the slope is a site record: given even for a pedon with no horizons
  for (k in list(list("texture_class", tex), list("slope_class", .f88ph_slope(x, m)))) {
      r <- k[[2L]]
      ok <- !is.na(r$code)
      rows[[length(rows) + 1L]] <- data.frame(
        kind = k[[1L]], code = r$code,
        name = if (ok) r$class$name else NA_character_,
        name_native = if (ok) r$class$name else NA_character_,
        name_pt = if (ok) r$class$name_pt else NA_character_,
        status = if (ok) "present" else "undecided", present = if (ok) TRUE else NA,
        criterion = if (ok) TRUE else NA, page = as.character(m$page),
        basis = if (ok) sprintf("%s (%s)", r$class$criterion, r$basis) else "",
        flags = paste(r$flags, collapse = "; "),
        missing = paste(if (ok) character(0) else r$missing, collapse = "; "),
        layers = "", note = "", stringsAsFactors = FALSE)
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# =============================================================================
# v0.9.390 -- the phases of the FAO-Unesco Legend of 1974 (Soil Map of the
# World, Vol. I, Unesco Paris), pp. 5-7, and the texture and slope classes of
# its map symbol (pp. 4-5), in inst/rules/fao1974/phases.yaml. Twelve
# phases: stony, lithic, petric, petrocalcic, petrogypsic, petroferric,
# phreatic, fragipan, duripan, saline, sodic and cerrado (the list of p. 5
# prints "stony lithic" without a comma). The readers of 1988 above run on
# the 1974 context where the two texts define the same phase (lithic,
# petroferric, fragipan, duripan; the saline and sodic numbers; the petric
# phase is the skeletic reader with its own depth of 100 cm); the stony,
# petrocalcic, petrogypsic, phreatic and cerrado phases are read here. Not
# shown: lithic for the Lithosols, saline for the Solonchaks, sodic for the
# Solonetz (pp. 6-7). The conventions are those of 1988.
# =============================================================================

.fao74_phase_rules <- function() {
  if (is.null(.f88ph_cache$rules74)) {
    f <- system.file("rules", "fao1974", "phases.yaml", package = "soilKey")
    .f88ph_cache$rules74 <- yaml::read_yaml(f)
  }
  .f88ph_cache$rules74
}

# Stony (p. 5): the condition of the rudic phase of 1988, recorded as
# rudic_recorded; not recorded, absent.
.f74ph_stony <- function(x, a) {
  r <- .f88_site_lgl(x, "rudic_recorded")
  if (is.na(r)) return(.f88ph_ans(FALSE, flags = "stony_not_recorded_absent"))
  .f88ph_ans(r, flags = "rudic_recorded_for_stony")
}

.f74ph_lithic <- function(x, a) .f88ph_lithic(x, a)
.f74ph_petric <- function(x, a) .f88ph_skeletic(x, a)
.f74ph_petroferric <- function(x, a) .f88ph_petroferric(x, a)
.f74ph_fragipan <- function(x, a) .f88ph_fragipan(x, a)
.f74ph_duripan <- function(x, a) .f88ph_duripan(x, a)
.f74ph_saline <- function(x, a) .f88ph_salic(x, a)
.f74ph_sodic <- function(x, a) .f88ph_sodic(x, a)

# Petrocalcic and petrogypsic (p. 6): the cemented calcic or gypsic horizon
# of the shared reader (.f88_hz_petro(): Soil Taxonomy's horizon, the
# petrocalcic and petrogypsic horizons of 1988), or a pan recorded as
# calcrete / petrocalcic (gypcrete / petrogypsic), flagged; its top within
# 100 cm.
.f74ph_petro <- function(x, a, kind) {
  if (!x$n) return(.f88ph_ans(FALSE))
  v <- .f88_member(x, .f88_hz_petro(x, kind)$units)
  pan <- .f88_chr(x, "pan_type")
  re <- if (kind == "calcic") "calcrete|petrocalcic" else "gypcrete|petrogypsic"
  vp <- !is.na(pan) & grepl(re, pan) & !(v %in% TRUE)
  v[vp] <- TRUE
  flag <- ifelse(vp, paste0("pan_type_petro", kind), NA_character_)
  .f88ph_within(x, v, a$within_cm, flags = flag,
                missing = if (kind == "calcic") c("caco3_pct", "cementation_class") else c("caso4_pct", "cementation_class"))
}
.f74ph_petrocalcic <- function(x, a) .f74ph_petro(x, a, "calcic")
.f74ph_petrogypsic <- function(x, a) .f74ph_petro(x, a, "gypsic")

# Phreatic (p. 6): a groundwater table between 300 and 500 cm, both ends
# included (soilKey's reading); not recorded, undecided. The text's "not
# normally reflected in the morphology" describes the depth, not a test.
.f74ph_phreatic <- function(x, a) {
  gw <- .f88ph_site_num(x, "groundwater_depth_cm")
  .f88ph_ans(if (is.na(gw)) NA else gw >= a$min_cm && gw <= a$max_cm, missing = "groundwater_depth_cm")
}

# Cerrado (p. 7): the vegetation of the site (vegetation, else land_cover)
# naming cerrado; another vegetation recorded rules it out; not recorded,
# absent.
.f74ph_cerrado <- function(x, a) {
  v <- .f88ph_site_txt(x, "vegetation")
  if (is.na(v)) v <- .f88ph_site_txt(x, "land_cover")
  if (is.na(v)) return(.f88ph_ans(FALSE, flags = "vegetation_not_recorded_absent"))
  .f88ph_ans(grepl("cerrad", v))
}

# The texture class of the upper 30 cm with the classes of 1974 (p. 5).
.f74ph_texture <- function(x, m) {
  t <- .fao_texture_upper30(x$h, "1974", depth = m$texture$upper_cm, site = x$site)
  cls <- Find(function(k) identical(k$id, t$value), m$texture$classes)
  list(code = if (is.null(cls)) NA_character_ else cls$code, class = cls,
       flags = t$flags, missing = t$missing,
       basis = if (is.null(cls)) "" else sprintf("clay %.1f %%, sand %.1f %% in the upper %d cm",
                                                t$clay, t$sand, as.integer(m$texture$upper_cm)))
}

# The phases of a pedon and its classify_fao(edition = "1974") result: the
# data.frame of .fao88_phases(), with the 12 phases of 1974 and the classes
# of its map symbol.
.fao74_phases <- function(pedon, res) {
  rules <- .fao74_phase_rules()
  x <- .f74_ctx(pedon)
  asg <- .f88ph_assigned(res)
  ans <- list()
  for (ph in rules$phases) {
    id <- ph$id
    fun <- get(paste0(".f74ph_", id), mode = "function")
    ans[[id]] <- tryCatch(fun(x, ph),
      error = function(e) .f88ph_ans(NA, flags = sprintf("phase_not_read: %s", conditionMessage(e)),
                                     note = sprintf("not read: %s", conditionMessage(e))))
  }
  rows <- lapply(rules$phases, function(ph) {
    a <- ans[[ph$id]]
    ex <- .f88ph_excluded(ph$not_shown_for, asg, glen = 1L)
    crit <- a$value
    present <- if (isFALSE(crit) || isTRUE(ex)) FALSE
               else if (isTRUE(crit) && isFALSE(ex)) TRUE else NA
    status <- if (isTRUE(present)) "present"
              else if (isTRUE(ex) && !isFALSE(crit)) "not shown"
              else if (isFALSE(present)) "absent" else "undecided"
    data.frame(kind = "phase", code = ph$id, name = ph$name, name_native = ph$name,
               name_pt = ph$name_pt, status = status, present = present, criterion = crit,
               page = as.character(ph$page), basis = ph$criterion,
               flags = paste(a$flags, collapse = "; "),
               missing = paste(if (is.na(present))
                                 unique(c(a$missing, if (isTRUE(crit)) attr(ex, "candidates")))
                               else character(0), collapse = "; "),
               layers = paste(sort(unique(a$layers)), collapse = "; "),
               note = paste(c(a$note, if (is.na(present) && isTRUE(crit) && length(attr(ex, "candidates")))
                 "not shown for a candidate unit tested before the one assigned"), collapse = "; "),
               stringsAsFactors = FALSE)
  })
  m <- rules$map_symbol
  tex <- if (x$n) .f74ph_texture(x, m)
         else list(code = NA_character_, flags = character(0), missing = "clay_pct", basis = "")
  for (k in list(list("texture_class", tex), list("slope_class", .f88ph_slope(x, m)))) {
    r <- k[[2L]]
    ok <- !is.na(r$code)
    rows[[length(rows) + 1L]] <- data.frame(
      kind = k[[1L]], code = r$code,
      name = if (ok) r$class$name else NA_character_,
      name_native = if (ok) r$class$name else NA_character_,
      name_pt = if (ok) r$class$name_pt else NA_character_,
      status = if (ok) "present" else "undecided", present = if (ok) TRUE else NA,
      criterion = if (ok) TRUE else NA, page = as.character(m$page),
      basis = if (ok) sprintf("%s (%s)", r$class$criterion, r$basis) else "",
      flags = paste(r$flags, collapse = "; "),
      missing = paste(if (ok) character(0) else r$missing, collapse = "; "),
      layers = "", note = "", stringsAsFactors = FALSE)
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}


# The lines print() and the reports show for phases read three-valued (the
# FAO-Unesco legend): the phases present, those undecided, those not shown
# for the unit, and the classes of the map symbol. `lang` "en" or "pt".
.fao_phase_print_lines <- function(ph, lang = "en") {
  nm <- if (identical(lang, "pt")) ph$name_pt else ph$name
  p <- ph$kind == "phase"
  pres <- nm[p & ph$status == "present"]
  und <- nm[p & ph$status == "undecided"]
  ns <- nm[p & ph$status == "not shown"]
  out <- c(Phases = if (length(pres)) paste(pres, collapse = "; ") else "none")
  if (length(und)) out <- c(out, `Phases undecided` = paste(und, collapse = "; "))
  if (length(ns)) out <- c(out, `Not shown for this unit` = paste(ns, collapse = "; "))
  cls <- function(kind, word) {
    i <- which(ph$kind == kind)[1L]
    if (is.na(i)) return(NULL)
    if (is.na(ph$code[i])) sprintf("%s undecided", word)
    else sprintf("%s %s (%s)", word, ph$code[i], tolower(nm[i]))
  }
  ms <- c(cls("texture_class", "texture"), cls("slope_class", "slope"))
  if (length(ms)) out <- c(out, `Map symbol` = paste(ms, collapse = "; "))
  out
}

# The phase lines of a report (FAO-Unesco legend results only; NULL for the
# others): each phase present, undecided (with what would decide it) or not
# shown, and the classes of the map symbol, in the report language.
.fao_phase_lines <- function(res) {
  ph <- tryCatch(res$phases, error = function(e) NULL)
  if (!is.data.frame(ph) || !nrow(ph) || !"status" %in% names(ph)) return(NULL)
  pt <- identical(.report_lang(), "pt")
  nm <- if (pt) ph$name_pt else ph$name
  lines <- character(0)
  for (i in seq_len(nrow(ph))) {
    if (ph$kind[i] == "phase") {
      if (ph$status[i] == "absent") next
      st <- .report_msg(paste0("report.fao_phase_", gsub(" ", "_", ph$status[i])))
      extra <- if (ph$status[i] == "undecided" && nzchar(ph$missing[i]))
        sprintf(" (%s)", .report_msg("report.needs", ph$missing[i])) else ""
      lines <- c(lines, sprintf("%s: %s, p. %s%s.", nm[i], st, ph$page[i], extra))
    } else {
      lab <- .report_msg(paste0("report.fao_map_", sub("_class$", "", ph$kind[i])))
      lines <- c(lines, if (is.na(ph$code[i]))
        sprintf("%s: %s (%s).", lab, .report_msg("report.fao_phase_undecided"),
                .report_msg("report.needs", ph$missing[i]))
      else sprintf("%s: %s %s, p. %s.", lab, ph$code[i], nm[i], ph$page[i]))
    }
  }
  lines
}
