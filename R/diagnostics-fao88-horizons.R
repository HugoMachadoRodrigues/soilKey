# =============================================================================
# v0.9.382 -- the diagnostic horizons of the FAO-Unesco Revised Legend
# (FAO 1988, World Soil Resources Report 60; the text of the ISRIC reprint of
# 1997, Technical Paper 20), chapter IV, pp. 25-33: the histic H, the mollic,
# fimic, umbric and ochric A, the argic, natric, cambic, spodic and ferralic
# B, the calcic, petrocalcic, gypsic, petrogypsic and sulfuric horizons and
# the albic E. Sixteen predicates <name>_fao88(pedon), internal, each a
# DiagnosticResult with passed TRUE / FALSE / NA, the layers, what would
# decide it (`missing`), the fallbacks used (evidence$flags) and the stand-ins
# (evidence$stand_in). The criteria are paraphrased; no text of the book is
# shipped. The numbers are those of data-raw/fao_thresholds_1988.yaml.
#
# Conventions (plan sections 4.2 and 5.1; Hugo's decisions of 2026-11-08 in
# data-raw/fao_readings.yaml; the readers of R/fao-readers.R):
#   * depths from the literal soil surface, H and O included (Q9);
#   * three values: a measurement not recorded leaves its criterion
#     undecided, a field observation not recorded is absent;
#   * a horizon N cm thick is a run of adjoining layers that meet the
#     per-layer criteria;
#   * horizons buried under 50 cm or more of new material are not diagnostic
#     (p. 42; .fao_buried(); the andic mantle of 35 cm is read with the
#     andic properties of R/diagnostics-fao88-properties.R since 0.9.383);
#   * the subsurface horizons have their upper limit within 125 cm unless the
#     text says otherwise (p. 42; argument max_top_cm);
#   * a WRB or USDA function of soilKey is called only where the 1988
#     descriptor is not recorded, and marks the result as a stand-in.
# =============================================================================

.F88_HZ <- data.frame(stringsAsFactors = FALSE,
  id = c("histic_h", "mollic_a", "fimic_a", "umbric_a", "ochric_a", "argic_b",
         "natric_b", "cambic_b", "spodic_b", "ferralic_b", "calcic", "petrocalcic",
         "gypsic", "petrogypsic", "sulfuric", "albic_e"),
  name = c("Histic H horizon", "Mollic A horizon", "Fimic A horizon", "Umbric A horizon",
           "Ochric A horizon", "Argic B horizon", "Natric B horizon", "Cambic B horizon",
           "Spodic B horizon", "Ferralic B horizon", "Calcic horizon", "Petrocalcic horizon",
           "Gypsic horizon", "Petrogypsic horizon", "Sulfuric horizon", "Albic E horizon"),
  page = c("25-26", "26-27", "27", "27", "27", "27-29", "29", "29-30", "30-31", "31",
           "31", "32", "32", "32", "32", "32-33"))

.f88_cache <- new.env(parent = emptyenv())


# --- the profile ------------------------------------------------------------------

# The context of a profile: the horizons in depth order (a data.frame), their
# positions in pedon$horizons (idx), the depths from the soil surface (Q9),
# masters, rock layers, buried layers and a memo of the readers already run.
# The last contexts are kept and reused while the horizons and site are the
# same (a hash of their content).
.f88_ctx <- function(pedon) {
  key <- rlang::hash(list(pedon$horizons, pedon$site))
  cache <- .f88_cache$ctx %||% list()
  for (e in cache) if (identical(e$key, key)) return(e$x)
  x <- .f88_ctx_build(pedon)
  .f88_cache$ctx <- c(list(list(key = key, x = x)), utils::head(cache, 7L))
  x
}

.f88_ctx_build <- function(pedon, edition = "1988") {
  h <- pedon$horizons
  n <- if (is.null(h)) 0L else nrow(h)
  hh <- if (is.null(h)) data.frame() else as.data.frame(h)
  ord <- if (n) order(suppressWarnings(as.numeric(hh$top_cm)), na.last = TRUE) else integer(0)
  if (n) hh <- hh[ord, , drop = FALSE]
  rownames(hh) <- NULL
  x <- new.env(parent = emptyenv())
  x$pedon <- pedon
  x$h <- hh
  x$n <- n
  x$idx <- ord
  x$site <- pedon$site %||% list()
  # the edition of the context (0.9.387): "1974" for the predicates of
  # R/diagnostics-fao74.R, which share the readers below where the 1974 text
  # equals the 1988 one (Appendix 4 "unchanged") and branch where it differs
  x$edition <- edition
  if (n) {
    d <- .fao_depths(hh)
    x$top <- d$top; x$bot <- d$bottom
    x$master <- .fao_master(hh, site = pedon$site)
    x$desg <- .desg_master(.fao_col(hh, "designation", "character"), site = pedon$site)
    x$rock <- .fao_is_rock(hh)
    x$memo <- new.env(parent = emptyenv())
    # 1974 (74 p. 32): buried by 50 cm or more of new material, no andic rule
    b <- .fao_buried(hh, edition, site = pedon$site)
    # a mantle of 35 to 50 cm buries the horizons below it when it has andic
    # properties (p. 42), read since 0.9.383 (.f88_mantle_andic())
    if (edition == "1988" && anyNA(b$value)) {
      b <- .fao_buried(hh, "1988", mantle_andic = .f88_mantle_andic(x, b$buried_from %||% NA_integer_),
                      site = pedon$site)
    }
    x$buried <- as.logical(b$value)
    x$buried_missing <- b$missing
  } else {
    x$top <- x$bot <- numeric(0); x$master <- x$desg <- character(0)
    x$rock <- x$buried <- logical(0); x$buried_missing <- character(0)
  }
  if (is.null(x$memo)) x$memo <- new.env(parent = emptyenv())
  x
}

.f88_memo <- function(x, key, fun) {
  if (!is.null(x$memo[[key]])) return(x$memo[[key]])
  r <- fun()
  assign(key, r, envir = x$memo)
  r
}

# Columns of the ordered horizons.
.f88_num <- function(x, name) .fao_col(x$h, name)
.f88_chr <- function(x, name) {
  v <- tolower(trimws(.fao_col(x$h, name, "character")))
  v[!is.na(v) & (!nzchar(v) | v %in% c("na", "nan", "null"))] <- NA_character_
  v
}
.f88_lgl <- function(x, name) {
  v <- if (name %in% names(x$h)) x$h[[name]] else rep(NA, x$n)
  if (is.logical(v)) return(v)
  if (is.numeric(v)) return(ifelse(is.na(v), NA, v != 0))
  v <- tolower(trimws(as.character(v)))
  ifelse(v %in% c("true", "t", "yes", "y", "1", "present"), TRUE,
         ifelse(v %in% c("false", "f", "no", "n", "0", "absent", "none"), FALSE, NA))
}
.f88_site <- function(x, name) {
  v <- x$site[[name]]
  if (is.null(v) || !length(v)) NA else v[[1L]]
}
.f88_site_lgl <- function(x, name) {
  v <- .f88_site(x, name)
  if (is.logical(v)) return(v)
  v <- tolower(trimws(as.character(v)))
  if (v %in% c("true", "t", "yes", "y", "1")) TRUE else if (v %in% c("false", "f", "no", "n", "0")) FALSE else NA
}

# Three-valued helpers on vectors.
.f88_and <- function(...) {
  a <- list(...)
  Reduce(function(p, q) ifelse(p %in% FALSE | q %in% FALSE, FALSE,
                               ifelse(p %in% TRUE & q %in% TRUE, TRUE, NA)), a)
}
.f88_or <- function(...) {
  a <- list(...)
  Reduce(function(p, q) ifelse(p %in% TRUE | q %in% TRUE, TRUE,
                               ifelse(p %in% FALSE & q %in% FALSE, FALSE, NA)), a)
}
.f88_not <- function(p) ifelse(is.na(p), NA, !p)
.f88_cmp <- function(v, op, k) {
  out <- switch(op, ">=" = v >= k, ">" = v > k, "<=" = v <= k, "<" = v < k)
  as.logical(out)
}
# Any / all of a vector, three-valued.
.f88_any <- function(v) { v <- as.logical(v); if (any(v %in% TRUE)) TRUE else if (length(v) && all(v %in% FALSE)) FALSE else if (!length(v)) FALSE else NA }
.f88_all <- function(v) { v <- as.logical(v); if (any(v %in% FALSE)) FALSE else if (all(v %in% TRUE)) TRUE else NA }

# The flags of some layers (semicolon lists split).
.f88_flags <- function(f, idx = seq_along(f)) {
  f <- as.character(f[idx])
  f <- f[!is.na(f) & nzchar(f)]
  unique(unlist(strsplit(f, "; ", fixed = TRUE)))
}

# A per-layer criterion with the buried layers taken out (p. 42): FALSE
# where buried, undecided where the burial rests on an undecided andic mantle.
.f88_unburied <- function(x, ok) {
  ok <- as.logical(ok)
  ok[x$buried %in% TRUE] <- FALSE
  ok[is.na(x$buried) & ok %in% TRUE] <- NA
  ok
}

# Layers that may hold a subsurface (B) horizon: not rock, not a layer
# designated A, E, H or O (a B in an E, E&Bt or E/B, is a B), and not the
# first layer of an undesignated profile surface (soilKey's reading: the
# legend's B horizons are below the A).
.f88_subsurface <- function(x) {
  m <- x$master
  # a B in lamellae or tongues inside an E (E&Bt, E/B) is a B
  eb <- !is.na(x$desg) & grepl("[&/]B", x$desg)
  out <- !x$rock & !( !is.na(m) & grepl("^(A|E|H|O)", m) & !eb )
  first <- which(!is.na(x$top))[1L]
  if (!is.na(first) && is.na(m[first])) out[first] <- FALSE
  out
}

# Layers that may hold one of the B horizons of the legend (argic, natric,
# cambic, spodic, ferralic): the subsurface layers that are not designated C
# (CB, C, Cr; BC is a B), soilKey's reading.
.f88_b_layer <- function(x) {
  .f88_subsurface(x) & !(!is.na(x$master) & grepl("^C", x$master))
}

# Contiguous runs of the layers where `sel` holds (adjoining in depth and in
# order): layers, top, bottom, thickness.
.f88_run_list <- function(x, sel) {
  idx <- which(as.logical(sel) %in% TRUE & !is.na(x$top) & !is.na(x$bot))
  if (!length(idx)) return(list())
  brk <- c(TRUE, diff(idx) != 1L | abs(x$top[idx][-1L] - x$bot[idx][-length(idx)]) > 0.5)
  lapply(unname(split(idx, cumsum(brk))), function(r) {
    list(layers = r, top = x$top[r[1L]], bottom = x$bot[r[length(r)]],
         thickness = x$bot[r[length(r)]] - x$top[r[1L]])
  })
}

# Units (candidate horizons) from a per-layer criterion: the runs of sure
# layers, read by `test_run` (TRUE / FALSE / NA), and the runs of sure and
# undecided layers, which can only make an undecided unit.
.f88_units <- function(x, ok, test_run) {
  ok <- as.logical(ok)
  out <- list()
  for (r in .f88_run_list(x, ok %in% TRUE)) {
    s <- test_run(r)
    if (!isFALSE(s)) out[[length(out) + 1L]] <- c(r, list(status = if (isTRUE(s)) TRUE else NA))
  }
  for (r in .f88_run_list(x, !(ok %in% FALSE))) {
    if (all(ok[r$layers] %in% TRUE)) next
    s <- test_run(r)
    if (!isFALSE(s)) out[[length(out) + 1L]] <- c(r, list(status = NA))
  }
  out
}

# Per-layer membership of the units: TRUE in a sure unit, NA in an undecided
# one only, FALSE elsewhere.
.f88_member <- function(x, units) {
  m <- rep(FALSE, x$n)
  for (u in units) if (is.na(u$status)) m[u$layers][!(m[u$layers] %in% TRUE)] <- NA
  for (u in units) if (isTRUE(u$status)) m[u$layers] <- TRUE
  m
}

# The DiagnosticResult of a reader: its units cut at max_top_cm (the upper
# limit of the unit), passed TRUE when a sure unit remains, NA when only
# undecided ones do.
.f88_result <- function(x, id, r, max_top_cm = Inf, extra = list()) {
  info <- .F88_HZ[.F88_HZ$id == id, , drop = FALSE]
  units <- Filter(function(u) !is.na(u$top) && u$top <= max_top_cm, r$units)
  sure <- Filter(function(u) isTRUE(u$status), units)
  passed <- if (length(sure)) TRUE else if (length(units)) NA else FALSE
  if (isFALSE(passed) && isTRUE(r$undecided)) passed <- NA
  # a profile whose layers have no depths is not described
  if (x$n && all(is.na(x$top) | is.na(x$bot))) {
    passed <- NA
    r$missing <- c("top_cm", "bottom_cm", r$missing)
  }
  lay <- if (isTRUE(passed)) sort(unique(unlist(lapply(sure, `[[`, "layers"))))
         else sort(unique(unlist(lapply(units, `[[`, "layers"))))
  notes <- unique(c(r$notes, if (is.finite(max_top_cm))
    sprintf("upper limit within %s cm of the soil surface (p. 42)", format(max_top_cm))))
  ev <- c(r$evidence %||% list(), extra, list(
    edition = "1988",
    page = info$page[1L],
    flags = unique(as.character(r$flags)),
    stand_in = unique(as.character(r$stand_in)),
    lower_grade = length(r$stand_in) > 0L,
    units = lapply(units, function(u) list(layers = x$idx[u$layers], top = u$top,
                                           bottom = u$bottom, status = u$status))))
  DiagnosticResult$new(
    name = paste0(id, "_fao88"),
    passed = passed,
    layers = x$idx[lay],
    evidence = ev,
    missing = if (is.na(passed)) unique(c(r$missing, if (any(is.na(x$buried))) x$buried_missing))
              else character(0),
    reference = sprintf("FAO (1988), Soil Map of the World, Revised Legend (ISRIC reprint 1997), ch. IV, %s, p. %s",
                        info$name[1L], info$page[1L]),
    notes = if (length(c(notes, r$flags))) paste(unique(c(notes, r$flags)), collapse = "; ")
            else NA_character_)
}

# The status of a reader at any depth: TRUE with a sure unit, NA with an
# undecided one (or an undecided answer without layers), else FALSE.
.f88_status <- function(r) {
  s <- vapply(r$units, function(u) u$status, NA)
  if (any(s %in% TRUE)) TRUE else if (length(s) || isTRUE(r$undecided)) NA else FALSE
}

# A reader's answer with one unit (or none).
.f88_one <- function(status, layers, x, ...) {
  if (isFALSE(status) || !length(layers)) {
    return(c(list(units = list(), undecided = is.na(status) && !length(layers)), list(...)))
  }
  u <- list(layers = layers, top = x$top[layers[1L]], bottom = x$bot[layers[length(layers)]],
            thickness = x$bot[layers[length(layers)]] - x$top[layers[1L]],
            status = if (isTRUE(status)) TRUE else NA)
  c(list(units = list(u)), list(...))
}


# --- shared field readings ----------------------------------------------------------

# Water saturation of the organic soil materials (p. 39; Q15): "saturated for
# long periods or artificially drained" from the drainage recorded at the
# site (drainage, then drainage_class; English or Portuguese), TRUE for very
# poorly, poorly, somewhat poorly and imperfectly drained or an artificially
# drained site, FALSE for well drained and better; moderately well drained and
# anything else NA (Hugo, 2026-11-09).
.f88_saturated <- function(x) {
  if (isTRUE(.f88_site_lgl(x, "artificially_drained"))) return(TRUE)
  d <- .f88_site(x, "drainage")
  if (is.na(d) || !nzchar(trimws(as.character(d)))) d <- .f88_site(x, "drainage_class")
  if (is.na(d)) return(NA)
  d <- .field_term(as.character(d))
  if (grepl("moderately well|moderadamente drenad", d)) return(NA)
  wet <- grepl("poorly|imperfect|mal drenad|imperfeit", d)
  dry <- grepl("well|excessiv|bem drenad|acentuad|fortemente drenad", d)
  if (wet && !dry) TRUE else if (dry && !wet) FALSE else NA
}

# Organic soil materials (p. 39; Q15), vectorised over organic carbon and the
# clay of the mineral fraction: 20 % OC or more passes; below the saturated
# limit (12 + 0.1 x clay, at most 18; 12 when the clay is not recorded) fails;
# between the two the recorded drainage decides, else undecided.
.f88_organic_v <- function(oc, clay, sat) {
  lim <- 12 + 0.1 * pmin(clay, 60)
  out <- rep(NA, length(oc))
  out[!is.na(oc) & oc >= 20] <- TRUE
  lo <- ifelse(is.na(lim), 12, lim)
  out[!is.na(oc) & oc < lo] <- FALSE
  mid <- is.na(out) & !is.na(oc)
  if (isTRUE(sat)) {
    out[mid & ((!is.na(lim) & oc >= lim) | oc >= 18)] <- TRUE
  } else if (isFALSE(sat)) {
    out[mid] <- FALSE
  }
  out
}

.f88_organic <- function(x) {
  # 1974: the H and O horizons of 74 p. 21, in organic matter (0.9.387)
  if (identical(x$edition, "1974")) return(.f74_organic(x))
  .f88_memo(x, "organic", function() {
    v <- .f88_organic_v(.f88_num(x, "oc_pct"), .f88_num(x, "clay_pct"), .f88_saturated(x))
    v[x$rock] <- FALSE
    list(value = v, missing = c("oc_pct", "drainage_class", "clay_pct"))
  })
}

# Clay skins or oriented clay on 1 % or more (argic B item 4, p. 28; Q13):
# clay_films_amount "few" or more; else micromorph_argillans_pct or
# clay_coatings_thin_section_pct of 1 % or more; else a B horizon designated
# with the suffix t counts as the observation, flagged; else absent.
.f88_clay_skins <- function(x) {
  .f88_memo(x, "skins", function() {
    amt <- .fao_col(x$h, "clay_films_amount", "character")
    ts <- suppressWarnings(pmax(.f88_num(x, "micromorph_argillans_pct"),
                                .f88_num(x, "clay_coatings_thin_section_pct"), na.rm = TRUE))
    v <- rep(NA, x$n); flag <- rep(NA_character_, x$n)
    rec <- !is.na(amt) & nzchar(trimws(amt))
    v[rec] <- .amount_at_least(amt[rec], "few")
    v[!is.na(ts) & ts >= 1] <- TRUE
    v[is.na(v) & !is.na(ts)] <- FALSE
    # a Bt, and the Bt of an E&Bt or E/Bt (the B inside an E)
    bt <- is.na(v) & !is.na(x$desg) &
      ((grepl("B", x$master) & grepl("^[A-Z]+(/[A-Z]+)?[a-z0-9]*t", x$desg)) |
         grepl("[&/]B[a-z0-9]*t", x$desg))
    v[bt] <- TRUE; flag[bt] <- "bt_designation_for_clay_skins"
    none <- is.na(v)
    v[none] <- FALSE; flag[none] <- "clay_skins_not_recorded_absent"
    list(value = v, flags = flag)
  })
}

# Cemented layers: cementation_class moderately cemented or harder ("moderately
# cemented" suffices, Hugo 2026-11-09); not recorded, a designation with the
# suffix m (Bkm, Bym, Bqm, Bsm), flagged; else not cemented (a field
# observation). `agent` asks the designation for the cementing agent too: "k"
# (carbonate, km), "y" (gypsum, ym); a q-only cementation (qm) is then not
# read as theirs.
.f88_cemented <- function(x, agent = NULL) {
  .f88_memo(x, paste0("cemented", agent %||% ""), function() {
    cem <- .f88_chr(x, "cementation_class")
    v <- ifelse(is.na(cem), NA, cem %in% c("moderately", "strongly", "indurated"))
    flag <- rep(NA_character_, x$n)
    suf <- ifelse(is.na(x$desg), NA_character_, sub("^[A-Z]+(/[A-Z]+)?", "", x$desg))
    dm <- is.na(v) & !is.na(suf) & grepl("m", suf)
    if (!is.null(agent)) dm <- dm & grepl(agent, suf)
    v[dm] <- TRUE; flag[dm] <- "cementation_from_designation"
    none <- is.na(v)
    v[none] <- FALSE; flag[none] <- "cementation_not_recorded_absent"
    list(value = as.logical(v), flags = flag)
  })
}

# The shared vocabulary of the anthropic record (v0.9.385, one list for the
# Anthrosols gate, the fimic A and the Cumulic and Urbic Anthrosols):
# manuring (plaggen, hortic, manure, compost, midden, kitchen garden; the
# sign the fimic A asks), fills and wastes (fill, waste, refuse, spoil,
# rubble, garbage, landfill, mine as a word, dredged material) and man-made
# sediments (irrigation sediments, irragric, terric, a raised surface).
# English and Portuguese terms.
.F88_MANURE_RE <- "plaggen|plagg|hortic|manur|compost|midden|kitchen|dung|esterco|estrume|adubad|adubo organico|composto|horta\\b|terra preta"
.F88_FILL_RE <- "\\bfills?\\b|waste|refuse|spoil|rubble|garbage|landfill|\\bmines?\\b|mining|dredg|\\baterro\\b|entulho|rejeito|\\blixo"
.F88_MAN_SEDIMENT_RE <- "irrig|irragric|terric|raised|man-made|man made|sedimento de irrigac|alteamento"
.F88_ANTHROPIC_RE <- paste("anthrop|antrop", .F88_MANURE_RE, .F88_FILL_RE, .F88_MAN_SEDIMENT_RE, sep = "|")

# Anthropic layers that the citric P2O5 limit of the mollic and umbric A
# (p. 27) is meant to remove, as recorded in the field (Q14): artefacts,
# anthropic material, a layer_origin in the shared vocabulary above
# (.F88_ANTHROPIC_RE), a positive silting sieve test (irrigation silt), or
# an overthickened A recorded at the site. Not recorded: absent.
.f88_anthropic <- function(x) {
  .f88_memo(x, "anthropic", function() {
    art <- .f88_num(x, "artefacts_pct")
    am <- .f88_chr(x, "anthropic_material")
    amp <- .f88_num(x, "anthropic_material_pct")
    lo <- .f88_chr(x, "layer_origin")
    sst <- .f88_chr(x, "silting_sieve_test")
    v <- (!is.na(art) & art > 0) | (!is.na(am) & !(am %in% c("none", "no", "false"))) |
      (!is.na(amp) & amp > 0) | (!is.na(lo) & grepl(.F88_ANTHROPIC_RE, lo)) |
      (!is.na(sst) & grepl("^pos", sst))
    if (isTRUE(.f88_site_lgl(x, "overthickened_a"))) v[!is.na(x$master) & grepl("^A", x$master)] <- TRUE
    rec <- !is.na(art) | !is.na(am) | !is.na(amp) | !is.na(lo) | !is.na(sst)
    list(value = v, recorded = rec)
  })
}

# Base saturation (NH4OAc) of each layer, with the reader's flags.
.f88_bs <- function(x) .f88_memo(x, "bs", function() .fao_bs(x$h))

# Exchangeable sodium percentage: esp_pct; else Na / CEC (NH4OAc) x 100,
# flagged.
.f88_esp <- function(x) {
  e <- .f88_num(x, "esp_pct")
  na <- .f88_num(x, "na_cmol"); cec <- .f88_num(x, "cec_cmol")
  comp <- is.na(e) & !is.na(na) & !is.na(cec) & cec > 0
  e[comp] <- 100 * na[comp] / cec[comp]
  list(value = e, flags = ifelse(comp, "esp_computed", NA_character_))
}

# The moist and dry Munsell value and chroma; a dry value not recorded is
# the moist value + 1, flagged (Hugo's reading of the WRB mollic horizon of
# 2026-10-29, carried over).
.f88_colours <- function(x) {
  .f88_memo(x, "colours", function() {
    vm <- .f88_num(x, "munsell_value_moist"); cm <- .f88_num(x, "munsell_chroma_moist")
    vd <- .f88_num(x, "munsell_value_dry"); cd <- .f88_num(x, "munsell_chroma_dry")
    inf <- is.na(vd) & !is.na(vm)
    vd2 <- ifelse(inf, vm + 1, vd)
    list(vm = vm, cm = cm, vd = vd, cd = cd, vd_or_inferred = vd2,
         hue = .fao_col(x$h, "munsell_hue_moist", "character"),
         flags = ifelse(inf, "dry_value_from_moist_plus_1", NA_character_))
  })
}

# Massive and hard or very hard when dry (mollic A item 1, p. 26): massive
# (structure_type massive, or a structureless grade that is not single
# grain, or prisms over 30 cm across with no secondary structure) together
# with a dry rupture resistance of hard or harder. Not recorded: absent.
.f88_massive_hard <- function(x) {
  st <- .f88_chr(x, "structure_type"); gr <- .field_grade_class(.fao_col(x$h, "structure_grade", "character"))
  ps <- .f88_num(x, "ped_size_max_mm"); sec <- .f88_chr(x, "structure_type_secondary")
  massive <- (!is.na(st) & grepl("massive|macica", st)) |
    (!is.na(gr) & gr == 0L & !(!is.na(st) & grepl("single|grao", st))) |
    (!is.na(st) & grepl("prism|column", st) & !is.na(ps) & ps > 300 & is.na(sec))
  rr <- .f88_chr(x, "rupture_resistance")
  hard <- !is.na(rr) & rr %in% c("hard", "very hard", "extremely hard")
  massive & hard
}

# Structure present: a grade weak or stronger and a type that is not massive
# or single grain. Not recorded: absent.
.f88_structure_present <- function(x) {
  st <- .f88_chr(x, "structure_type"); gr <- .field_grade_class(.fao_col(x$h, "structure_grade", "character"))
  ok_type <- !is.na(st) & !grepl("massive|single|macica|grao", st)
  (!is.na(gr) & gr >= 1L & !(!is.na(st) & grepl("massive|single|macica|grao", st))) |
    (is.na(gr) & ok_type)
}

# Texture sandy loam or finer with 8 % clay or more (argic, cambic and
# ferralic B, pp. 28-31; Q16 for the ferralic B).
.f88_texture_b <- function(x) {
  .f88_and(.fao_sandy_loam_or_finer(x$h), .f88_cmp(.f88_num(x, "clay_pct"), ">=", 8))
}

# The andic properties of p. 34 (item 7 of the ferralic B) are read by
# .f88_andic_layer() in R/diagnostics-fao88-properties.R (0.9.383).


# --- histic H (pp. 25-26) -------------------------------------------------------------

# (a) An H horizon (organic soil materials) with its upper boundary within 40
# cm and more than 20 cm thick; the text's upper bound (less than 40 cm, 60
# cm if sphagnum or very light) only separates the Histosols, whose gate is
# read first in the key, and is reported as evidence (soilKey's reading);
# (b) an organic layer at the surface, thinner than 25 cm, whose mixture over
# 0-25 cm holds OC >= 8 + 8 x clay / 60 % (8 % without clay, 16 % at more
# than 60 % clay); (c) that test read on a surface plough layer (Ap, Hp, Op)
# of 25 cm or more.
.f88_hz_histic_h <- function(x) {
  .f88_memo(x, "histic_h", function() {
    if (!x$n) return(list(units = list()))
    org <- .f88_organic(x)
    ok <- .f88_unburied(x, org$value)
    e74 <- identical(x$edition, "1974")
    # 1974 (74 p. 24): an H horizon (saturated or drained, p. 21; .f74_h_v())
    # more than 20 cm thick, no depth printed (the upper limit of 125 cm is
    # applied by the caller, Q10); the thin surface and plough layer paths
    # below read the organic layers
    okh <- if (e74) .f88_unburied(x, .f74_h_v(x)) else ok
    units <- .f88_units(x, okh, function(r) {
      if (!e74 && r$top > 40) return(FALSE)
      r$thickness > 20
    })
    # mixed to 25 cm
    oc25 <- .fao_mixed(x$h, "oc_pct", 25)
    cl25 <- .fao_mixed(x$h, "clay_pct", 25)
    # 1988: OC 8 + 8 x clay / 60 (8 to 16 %); 1974: organic matter 14 + 14 x
    # clay / 60 (14 to 28 %), OM = OC x 1.724 (Q11)
    k <- if (e74) 14 else 8
    m25 <- if (e74) .fao_om(oc25$value)$value else oc25$value
    line <- function() {
      if (is.na(m25)) return(NA)
      if (!is.na(cl25$value)) return(m25 >= k + k * min(cl25$value, 60) / 60)
      if (m25 >= 2 * k) TRUE else if (m25 < k) FALSE else NA
    }
    first <- which(!is.na(x$top) & x$top == 0)[1L]
    thin <- NULL; plough <- NULL
    if (!is.na(first)) {
      runs <- .f88_run_list(x, !(ok %in% FALSE))
      sr <- Filter(function(r) r$layers[1L] == first, runs)
      if (length(sr) && sr[[1L]]$thickness < 25) {
        s <- .f88_and(.f88_all(ok[sr[[1L]]$layers]), line())
        if (!isFALSE(s)) thin <- c(sr[[1L]], list(status = if (isTRUE(s)) TRUE else NA))
      }
      ap <- !is.na(x$desg) & grepl("^[AHO][a-z0-9]*p", x$desg) & !(x$buried %in% TRUE)
      pr <- Filter(function(r) r$layers[1L] == first, .f88_run_list(x, ap))
      if (length(pr) && pr[[1L]]$thickness >= 25) {
        s <- line()
        if (!isFALSE(s)) plough <- c(pr[[1L]], list(status = if (isTRUE(s)) TRUE else NA))
      }
    }
    sph <- .f88_num(x, "sphagnum_fibres_pct"); bd <- .f88_num(x, "bulk_density_g_cm3")
    ev <- list(paths = c(h_horizon = length(units) > 0L, thin_surface = !is.null(thin),
                         plough_layer = !is.null(plough)),
               oc_mixed_25cm = oc25$value, clay_mixed_25cm = cl25$value,
               thickness_upper_bound = vapply(units, function(u) {
                 light <- any(sph[u$layers] >= 75 | bd[u$layers] < 0.1, na.rm = TRUE)
                 u$thickness < if (light) 60 else 40
               }, logical(1)))
    list(units = c(units, if (!is.null(thin)) list(thin), if (!is.null(plough)) list(plough)),
         missing = unique(c(org$missing, oc25$missing, cl25$missing)),
         flags = c(oc25$flags, cl25$flags, if (e74) "om_from_oc_x1.724"), evidence = ev,
         notes = "the upper bound of 40 (60) cm separates the Histosols and is not tested here")
  })
}

histic_h_fao88 <- function(pedon, base = c("any", "eutric", "dystric")) {
  base <- match.arg(base)
  x <- .f88_ctx(pedon)
  r <- .f88_hz_histic_h(x)
  if (base != "any" && length(r$units)) {
    # eutric: pH (H2O 1:5) 5.5 or more throughout the H; dystric: below 5.5 in
    # some part (74 p. 24, read in both editions, Q15)
    ph <- .fao_ph(x$h, "1to5")
    r$units <- lapply(r$units, function(u) {
      v <- ph$value[u$layers]
      s <- if (base == "eutric") .f88_all(v >= 5.5) else .f88_any(v < 5.5)
      u$status <- .f88_and(u$status, s)
      u
    })
    r$units <- Filter(function(u) !isFALSE(u$status), r$units)
    r$flags <- c(r$flags, .f88_flags(ph$flags, unlist(lapply(r$units, `[[`, "layers"))))
    r$missing <- c(r$missing, ph$missing)
  }
  res <- .f88_result(x, "histic_h", r)
  res$evidence$base <- base
  res
}


# --- mollic, umbric, fimic and ochric A (pp. 26-27) ---------------------------------------

# The A: the run of layers from the first mineral layer (not H or O, not
# organic soil material) designated A (AB, AE, AC and A/B included) or
# undesignated; NULL when the first mineral layer is designated otherwise.
.f88_a_run <- function(x) {
  org <- .f88_organic(x)$value
  m <- x$master
  i0 <- which(!(!is.na(m) & grepl("^(O|H)", m)) & !(org %in% TRUE) & !x$rock &
                !is.na(x$top) & !is.na(x$bot))[1L]
  if (is.na(i0)) return(NULL)
  if (!is.na(m[i0]) && !grepl("^A", m[i0])) return(NULL)
  run <- i0
  j <- i0 + 1L
  while (j <= x$n && !x$rock[j] && (is.na(m[j]) || grepl("^A", m[j])) &&
         !is.na(x$top[j]) && !is.na(x$bot[j - 1L]) && abs(x$top[j] - x$bot[j - 1L]) <= 0.5) {
    run <- c(run, j); j <- j + 1L
  }
  list(layers = run, first = i0, undesignated = anyNA(m[run]))
}

# The mollic (umbric = FALSE) or umbric (TRUE) A.
.f88_hz_dark_a <- function(x, umbric = FALSE) {
  .f88_memo(x, if (umbric) "umbric_a" else "mollic_a", function() {
    a <- .f88_a_run(x)
    if (is.null(a) || !x$n) return(list(units = list(), notes = "no A horizon at the surface"))
    h <- x$h
    col <- .f88_colours(x)
    bs <- .f88_bs(x)
    org <- .f88_organic(x)
    caco3 <- .f88_num(x, "caco3_pct")
    sat <- .f88_saturated(x)
    t0 <- x$top[a$first]
    mix <- function(var, values = NULL) .fao_weighted(h, var, t0, t0 + 18, values = values)
    m_vm <- mix("munsell_value_moist"); m_cm <- mix("munsell_chroma_moist")
    m_vd <- mix("munsell_value_dry", col$vd_or_inferred)
    m_oc <- mix("oc_pct"); m_bs <- mix("bs_pct", bs$value)
    m_cl <- mix("clay_pct")
    win <- .fao_window(h, t0, t0 + 18)$layers
    flags <- c(.f88_flags(col$flags, win), .f88_flags(bs$flags, win),
               if (a$undesignated) "a_horizon_partly_undesignated")
    missing <- character(0)
    # criteria 2 and 4 with the lime waiver (more than 40 % finely divided
    # lime): the colour of the text with 0.6 % OC, or, with the lime, a moist
    # value of 5 or less (the dry value waived) with 2.5 % OC. CaCO3 not
    # measured leaves the waiver undecided unless .f88_caco3() reads it as
    # absent (no HCl reaction, pH below 6.5, flagged).
    cc_l <- .f88_caco3(x)
    m_cc <- mix("caco3_pct", cc_l$value)
    flags <- c(flags, .f88_flags(cc_l$flags, win))
    # 1974 (74 p. 24): organic matter 1 % (4 % with the lime waiver), OM =
    # OC x 1.724 (Q11)
    e74 <- identical(x$edition, "1974")
    col_oc <- function(vm, cm, vd, oc, lime) {
      if (e74) {
        om <- .fao_om(oc)$value
        strict <- .f88_and(cm < 3.5, vm < 3.5, vd < 5.5, .f88_cmp(om, ">=", 1))
        relaxed <- .f88_and(lime, cm < 3.5, vm <= 5, .f88_cmp(om, ">=", 4))
        return(.f88_or(strict, relaxed))
      }
      strict <- .f88_and(cm < 3.5, vm < 3.5, vd < 5.5, .f88_cmp(oc, ">=", 0.6))
      relaxed <- .f88_and(lime, cm < 3.5, vm <= 5, .f88_cmp(oc, ">=", 2.5))
      .f88_or(strict, relaxed)
    }
    if (e74) flags <- c(flags, "om_from_oc_x1.724")
    lime <- .f88_cmp(m_cc$value, ">", 40)
    colour_mix <- col_oc(m_vm$value, m_cm$value, m_vd$value, m_oc$value, lime)
    if (is.na(colour_mix)) missing <- c(missing, "munsell_value_moist", "munsell_chroma_moist",
                                        "munsell_value_dry", "oc_pct", "caco3_pct")
    not_org <- .f88_not(if (e74) .f74_organic_v(m_oc$value, m_cl$value, sat)
                        else .f88_organic_v(m_oc$value, m_cl$value, sat))
    if (is.na(not_org)) missing <- c(missing, "oc_pct")
    bs_mix <- if (umbric) .f88_cmp(m_bs$value, "<", 50) else .f88_cmp(m_bs$value, ">=", 50)
    if (is.na(bs_mix)) missing <- c(missing, "bs_pct")
    mh <- .f88_massive_hard(x)
    # 1974 umbric A (74 p. 24): the massive-and-hard restriction applies only
    # to an A that becomes dry: not to one in a perudic or peraquic moisture
    # regime recorded at the site (soilKey's reading)
    if (e74 && umbric && .f74_always_moist(x)) {
      mh <- rep(FALSE, x$n); flags <- c(flags, "umbric_always_moist_no_consistence_restriction")
    }
    struct_mix <- !any(mh[intersect(win, a$layers)])
    # every A layer whose base lies below the mixed 18 cm (one that straddles
    # 18 cm included) is also read on its own values (Hugo, 2026-11-09)
    below <- a$layers[x$bot[a$layers] > t0 + 18]
    per <- rep(TRUE, x$n)
    occ <- .f88_num(x, "oc_pct")
    for (i in below) {
      cl <- col_oc(col$vm[i], col$cm[i], col$vd_or_inferred[i], occ[i],
                   .f88_cmp(cc_l$value[i], ">", 40))
      bsi <- if (umbric) bs$value[i] < 50 else bs$value[i] >= 50
      per[i] <- .f88_and(cl, .f88_not(org$value[i]), as.logical(bsi), !mh[i])
      flags <- c(flags, .f88_flags(col$flags, i), .f88_flags(bs$flags, i), .f88_flags(cc_l$flags, i))
    }
    if (anyNA(per[below])) missing <- c(missing, "oc_pct", "bs_pct", "munsell_value_moist",
                                        "munsell_chroma_moist", "caco3_pct")
    per <- .f88_unburied(x, per)
    # thickness of the A: the layers wholly within the mixed 18 cm, then each
    # layer below while it passes
    sure <- setdiff(a$layers, below); maybe <- sure
    stop_sure <- FALSE
    for (i in below) {
      if (isFALSE(per[i])) break
      maybe <- c(maybe, i)
      if (isTRUE(per[i]) && !stop_sure) sure <- c(sure, i) else stop_sure <- TRUE
    }
    # resting directly on hard rock, a petrocalcic, petrogypsic horizon or a
    # duripan: 10 cm suffices
    pc <- .f88_member(x, .f88_hz_petro(x, "calcic")$units)
    pg <- .f88_member(x, .f88_hz_petro(x, "gypsic")$units)
    cem <- .f88_cemented(x)$value
    dp <- .f88_num(x, "duripan_pct")
    duri <- cem & ((!is.na(dp) & dp > 0) | (!is.na(x$desg) & grepl("^[A-Z]+[a-z0-9]*qm", x$desg)))
    # the solum and the A both measured from the mineral soil surface, the
    # top of the A (Hugo, 2026-11-09)
    solum <- .fao_solum(h, site = x$site)
    S <- solum$value - t0
    th_ok <- function(layers) {
      if (!length(layers)) return(FALSE)
      last <- layers[length(layers)]
      T <- x$bot[last] - t0
      nxt <- last + 1L
      hard <- if (nxt <= x$n) .f88_or(x$rock[nxt], pc[nxt], pg[nxt], duri[nxt]) else FALSE
      rule <- if (T > 25) TRUE else if (T < 18) FALSE
              else if (is.na(S)) NA
              else if (S >= 75) FALSE
              else if (T <= S / 3) FALSE      # an open solum is only deeper
              else if (isTRUE(solum$open)) NA
              else TRUE
      .f88_or(.f88_and(hard, T >= 10), rule)
    }
    # the A may end at the base of the sure layers or of any undecided layer
    # after them
    th_sure <- th_ok(sure)
    ks <- if (length(maybe)) seq.int(max(length(sure), 1L), length(maybe)) else integer(0)
    th_any <- vapply(ks, function(k) th_ok(maybe[seq_len(k)]), NA)
    thick <- if (isTRUE(th_sure)) TRUE else if (any(th_any %in% TRUE) || anyNA(th_any)) NA else FALSE
    if (is.na(thick)) missing <- c(missing, "designation", "munsell_value_moist")
    # the colour contrast with the C, or the horizon right below the A; waived
    # when strongly humic (p. 40) or on dark parent material (a field
    # observation soilKey does not record: absent); with rock right below the A it cannot be read and is taken as
    # met, flagged (soilKey's reading)
    last <- if (length(maybe)) maybe[length(maybe)] else a$first
    cbelow <- which(seq_len(x$n) > last & !is.na(x$master) & grepl("^C", x$master) & !x$rock)[1L]
    ref <- if (!is.na(cbelow)) cbelow else if (last < x$n) last + 1L else NA_integer_
    sh <- .fao_weighted(h, "oc_pct", 0, 100, zero_below_rock = TRUE)
    humic <- .f88_cmp(sh$value, ">", 1.4)
    contrast <- if (is.na(ref)) NA
      else if (x$rock[ref]) { flags <- c(flags, "rock_below_a_contrast_not_read"); TRUE }
      # more than 40 % finely divided lime waives the limits of the dry
      # value, the dry contrast with the C included (1988 p. 26 and 74 p.
      # 24, the same words; Hugo, 2026-11-10; a correction to 0.9.382)
      else .f88_and(m_vm$value <= col$vm[ref] - 1,
                    .f88_or(m_vd$value <= col$vd_or_inferred[ref] - 1, lime))
    if (!is.na(ref) && !x$rock[ref]) flags <- c(flags, .f88_flags(col$flags, ref))
    # the strongly humic waiver is of 1988 (p. 26); 1974 has none (74 p. 24)
    if (!e74) contrast <- .f88_or(contrast, humic)
    if (is.na(contrast)) missing <- c(missing, "munsell_value_moist", "munsell_value_dry", "oc_pct")
    # criterion 6, citric P2O5 below 250 mg/kg (Q14): measured decides (not
    # when it increases below the A; phosphate nodules, the other exception,
    # are a field observation that soilKey does not record: absent); else the
    # anthropic layers it targets, absent if not recorded
    p <- .f88_num(x, "p2o5_citric_mg_kg")
    an <- .f88_anthropic(x)
    al <- maybe
    nb <- if (last < x$n) last + 1L else NA_integer_
    p_ok <- vapply(al, function(i) {
      if (!is.na(p[i])) {
        if (p[i] < 250) return(TRUE)
        if (!is.na(nb) && !is.na(p[nb]) && p[nb] > p[i]) return(TRUE)
        return(FALSE)
      }
      !an$value[i]
    }, logical(1))
    if (any(is.na(p[al]))) {
      flags <- c(flags, if (any(an$value[al][is.na(p[al])])) "p2o5_exclusion_from_anthropic_record"
                        else "p2o5_not_recorded_no_anthropic_layer")
    }
    p_all <- all(p_ok)
    # 1974 umbric A (74 p. 24): plaggen A horizons, built by slow additions
    # under cultivation, are excluded: a manuring record in a layer of the A
    # (.F88_MANURE_RE) or a WRB plaggic horizon; not recorded, absent
    if (e74 && umbric) {
      pl <- .f74_plaggen(x, al)
      if (isTRUE(pl)) { p_all <- FALSE; flags <- c(flags, "plaggen_a_not_umbric") }
    }
    crit <- list(structure = struct_mix, contrast = contrast,
                 base_saturation = bs_mix, colour_and_carbon = colour_mix, not_organic = not_org,
                 thickness = thick, p2o5 = p_all, lime_waiver = lime,
                 strongly_humic = humic)
    status <- .f88_and(struct_mix, colour_mix, contrast, bs_mix, not_org, thick, p_all)
    lay <- if (isTRUE(status)) sure else maybe
    flags <- c(flags, m_vm$flags, m_oc$flags, m_bs$flags)
    out <- .f88_one(status, if (isFALSE(status)) integer(0) else lay, x,
                    missing = unique(missing), flags = unique(flags),
                    evidence = list(criteria = crit,
                                    mixed_18cm = c(value_moist = m_vm$value, chroma_moist = m_cm$value,
                                                   value_dry = m_vd$value, oc = m_oc$value,
                                                   bs = m_bs$value, caco3 = m_cc$value),
                                    solum_cm = S, solum_open = isTRUE(solum$open),
                                    thickness_cm = if (length(sure)) x$bot[sure[length(sure)]] - t0 else 0),
                    notes = "the surface 18 cm of the A are mixed; the layers below are read each on its own")
    if (any(x$buried[a$layers] %in% TRUE)) out$units <- list()
    out
  })
}

mollic_a_fao88 <- function(pedon) {
  x <- .f88_ctx(pedon)
  .f88_result(x, "mollic_a", .f88_hz_dark_a(x, umbric = FALSE))
}

umbric_a_fao88 <- function(pedon) {
  x <- .f88_ctx(pedon)
  .f88_result(x, "umbric_a", .f88_hz_dark_a(x, umbric = TRUE))
}

# The fimic A (p. 27): a man-made surface layer 50 cm or more thick from long
# manuring. The layer is the run from the surface of layers with an
# anthropic record (.f88_anthropic(): the shared vocabulary), citric P2O5
# above 250 mg/kg, a manuring term, or a WRB plaggic or hortic horizon; it
# is fimic only with a sign of manuring (Hugo, 2026-11-09): citric P2O5
# above 250 mg/kg in a layer of it, a manuring term (.F88_MANURE_RE) in its
# layer_origin, anthropic_material or designation or in the site's
# land_use, or the WRB plaggic() or hortic() passing (always tried; a
# stand-in, flagged). A manuring land use marks the run of A layers at the
# surface as the man-made layer, flagged. Artefacts neither make nor exclude it: fills, wastes
# and man-made sediments without manuring are not fimic (the Anthrosols
# gate still reads them, and they key to the Cumulic or Urbic units). A
# layer that meets the mollic or umbric A with citric P2O5 measured at 250
# or less is that A, not fimic. Nothing recorded: absent.
.f88_hz_fimic_a <- function(x) {
  .f88_memo(x, "fimic_a", function() {
    if (!x$n) return(list(units = list()))
    p <- .f88_num(x, "p2o5_citric_mg_kg")
    an <- .f88_anthropic(x)
    lo <- .f88_chr(x, "layer_origin"); am <- .f88_chr(x, "anthropic_material")
    dsg <- tolower(.fao_col(x$h, "designation", "character"))
    term <- (!is.na(lo) & grepl(.F88_MANURE_RE, lo)) | (!is.na(am) & grepl(.F88_MANURE_RE, am)) |
      (!is.na(dsg) & grepl(.F88_MANURE_RE, dsg))
    lu <- .f88_site(x, "land_use")
    site_term <- !is.na(lu) && grepl(.F88_MANURE_RE, tolower(as.character(lu)))
    # the WRB plaggic and hortic horizons, always tried
    wrb <- list(plaggic = plaggic(x$pedon), hortic = hortic(x$pedon))
    wrb_l <- rep(FALSE, x$n); stand_in <- character(0)
    for (k in names(wrb)) {
      if (isTRUE(wrb[[k]]$passed)) {
        i <- match(wrb[[k]]$layers, x$idx); i <- i[!is.na(i)]
        wrb_l[i] <- TRUE; stand_in <- c(stand_in, k)
      }
    }
    hp <- !is.na(p) & p > 250
    # a manuring land use recorded at the site marks the A at the surface
    # (its run of A layers, AB included) as the man-made layer, flagged
    lu_l <- rep(FALSE, x$n)
    if (site_term) { a <- .f88_a_run(x); if (!is.null(a)) lu_l[a$layers] <- TRUE }
    sign <- hp | term | wrb_l | lu_l
    man <- .f88_unburied(x, hp | an$value | term | wrb_l | lu_l)
    first <- which(!is.na(x$top))[1L]
    run <- Filter(function(r) r$layers[1L] == first, .f88_run_list(x, man))
    if (!length(run) || run[[1L]]$thickness < 50) {
      none <- !any(an$recorded) && all(is.na(p)) && !any(wrb_l)
      return(list(units = list(), flags = if (none) "anthropic_layer_not_recorded_absent"))
    }
    u <- run[[1L]]
    manured <- any(sign[u$layers]) || site_term
    if (!manured)
      return(list(units = list(), flags = "anthropic_layers_without_manuring_not_fimic"))
    if (any(!is.na(p[u$layers]) & p[u$layers] <= 250) &&
        (isTRUE(.f88_status(.f88_hz_dark_a(x, FALSE))) ||
         isTRUE(.f88_status(.f88_hz_dark_a(x, TRUE)))))
      return(list(units = list(), flags = "dark_a_with_p2o5_measured_not_fimic"))
    used_wrb <- !any(hp[u$layers] | term[u$layers]) && !site_term
    if (site_term && any(lu_l[u$layers] & !(hp | term | an$value | wrb_l)[u$layers]))
      used_wrb <- FALSE
    list(units = list(c(u, list(status = TRUE))),
         stand_in = if (used_wrb) stand_in[vapply(wrb[stand_in], function(r) any(match(r$layers, x$idx) %in% u$layers), NA)],
         flags = c(if (any(is.na(p[u$layers]))) "fimic_from_manuring_record",
                   if (any(lu_l[u$layers])) "fimic_layers_from_manuring_land_use",
                   if (used_wrb) "wrb_anthropic_horizon_for_fimic"))
  })
}

fimic_a_fao88 <- function(pedon) {
  x <- .f88_ctx(pedon)
  .f88_result(x, "fimic_a", .f88_hz_fimic_a(x))
}

# The ochric A (p. 27): an A that is not mollic or umbric; finely stratified
# fresh alluvium at the surface (stratification recorded in a recent or
# fluvial surface layer) is not ochric.
.f88_hz_ochric_a <- function(x) {
  .f88_memo(x, "ochric_a", function() {
    a <- .f88_a_run(x)
    if (is.null(a)) return(list(units = list(), notes = "no A horizon at the surface"))
    mo <- .f88_hz_dark_a(x, FALSE); um <- .f88_hz_dark_a(x, TRUE)
    st <- .f88_status
    dark <- .f88_or(st(mo), st(um))
    i <- a$first
    strat <- .f88_chr(x, "stratification_pattern")[i]
    rec <- .f88_lgl(x, "recent_deposit")[i]
    lo <- .f88_chr(x, "layer_origin")[i]
    fresh <- !is.na(strat) && !(strat %in% c("none", "no")) &&
      (isTRUE(rec) || (!is.na(lo) && grepl("fluv|alluv", lo)))
    # the fresh-alluvium exclusion is of 1988 (p. 27); 1974 has none (74 p. 25)
    if (identical(x$edition, "1974")) fresh <- FALSE
    status <- if (fresh) FALSE else .f88_not(dark)
    status <- .f88_and(status, !any(x$buried[a$layers] %in% TRUE))
    .f88_one(status, a$layers, x, missing = unique(c(mo$missing, um$missing)),
             flags = if (a$undesignated) "a_horizon_partly_undesignated",
             evidence = list(mollic = st(mo), umbric = st(um), fresh_stratified_alluvium = fresh))
  })
}

ochric_a_fao88 <- function(pedon) {
  x <- .f88_ctx(pedon)
  .f88_result(x, "ochric_a", .f88_hz_ochric_a(x))
}


# --- argic and natric B (pp. 27-29) -------------------------------------------------------

# Items 1 to 6 of the argic B, for each layer that could start it (the layer
# where the clay increase is reached): the B is that layer and the adjoining
# layers below it that keep the texture and the increase over the same
# coarser layer above.
.f88_argic_core <- function(x) {
  .f88_memo(x, "argic_core", function() {
    n <- x$n
    if (n < 2L) return(list(starts = list()))
    h <- x$h
    clay <- .f88_num(x, "clay_pct")
    tex <- .f88_texture_b(x)
    sub <- .f88_unburied(x, .f88_b_layer(x))
    ferr <- .f88_member(x, .f88_hz_ferralic_b(x)$units)
    inc30 <- .fao_clay_increase(h, distance = 30)
    inc15 <- .fao_clay_increase(h, distance = 15)
    sk <- .f88_clay_skins(x)
    mat <- .desg_material_number(.fao_col(h, "designation", "character"))
    sharp <- .fao_boundary_sharp(x$h)
    lam <- .f88_num(x, "lamellae_cumulative_cm")
    trunc <- isTRUE(.f88_site_lgl(x, "anthropic_truncation"))
    starts <- list(); dropped <- character(0)
    for (i in seq_len(n)[-1L]) {
      st0 <- .f88_and(sub[i], tex[i], .f88_not(ferr[i]), .f88_or(inc30$value[i], inc15$value[i]))
      if (isFALSE(st0) || is.na(x$top[i]) || is.na(x$bot[i])) next
      up <- inc30$upper[i]
      if (is.na(up)) up <- inc15$upper[i]
      if (is.na(up)) up <- i - 1L
      # the B below the starting layer
      cont <- function(j) .f88_and(sub[j], tex[j], .f88_not(ferr[j]),
                                    .fao_clay_increase_pair(clay[up], clay[j]))
      sure <- i; maybe <- i; open <- TRUE
      j <- i + 1L
      while (j <= n && !is.na(x$top[j]) && !is.na(x$bot[j - 1L]) &&
             abs(x$top[j] - x$bot[j - 1L]) <= 0.5) {
        cj <- cont(j)
        if (isFALSE(cj)) break
        maybe <- c(maybe, j)
        if (isTRUE(cj) && open) sure <- c(sure, j) else open <- FALSE
        j <- j + 1L
      }
      sk_s <- any(sk$value[sure]); sk_m <- any(sk$value[maybe])
      # item 4: within 30 cm with clay skins, 15 cm without
      inc <- if (sk_s) inc30$value[i]
             else if (sk_m) (if (isTRUE(inc15$value[i])) TRUE else if (isFALSE(inc30$value[i])) FALSE else NA)
             else inc15$value[i]
      # item 5: one tenth of the overlying horizons and 7.5 cm; lamellae 15
      # cm combined
      need <- max(7.5, 0.1 * x$top[i])
      th_of <- function(L) {
        if (all(!is.na(lam[L]) & lam[L] > 0)) {
          s <- sum(lam[L]); return(s >= 15 && s >= 0.1 * x$top[i])
        }
        (x$bot[L[length(L)]] - x$top[i]) >= need
      }
      thick <- if (th_of(sure)) TRUE else if (!th_of(maybe)) FALSE else NA
      # item 6: without clay skins, the coarser overlying horizon at least 18
      # cm (5 cm over an abrupt transition); and clay skins are required
      # after erosion, over or within a lithological discontinuity, or under
      # a plough layer only
      i6 <- function() {
        k <- i - 1L; topc <- x$top[i]
        while (k >= 1L && !x$rock[k] && !is.na(clay[i])) {
          if (is.na(clay[k])) return(NA)          # the walk stops on a missing clay
          if (clay[k] >= clay[i]) break
          topc <- x$top[k]; k <- k - 1L
        }
        thc <- x$top[i] - topc
        # an abrupt transition: over less than 5 cm, a boundary recorded
        # abrupt or clear (default 5); not recorded, absent, flagged (Hugo,
        # 2026-11-09; the reader of the abrupt textural change)
        abrupt <- isTRUE(sharp$value[i - 1L])
        ok <- thc >= if (abrupt) 5 else 18
        ms <- mat[seq_len(maybe[length(maybe)])]
        disc <- length(unique(ms[!is.na(ms)])) > 1L
        above <- seq_len(i - 1L)
        plough_only <- all(!is.na(x$desg[above]) & grepl("^A[a-z0-9]*p", x$desg[above]))
        ok && !disc && !plough_only && !trunc
      }
      r6 <- if (sk_s || is.na(clay[i])) NA else i6()
      item6 <- if (sk_s) TRUE else if (is.na(clay[i])) NA else if (isTRUE(r6)) TRUE
               else if (sk_m || is.na(r6)) NA else FALSE
      status <- .f88_and(st0, inc, thick, item6)
      if (isFALSE(status)) {
        # a start discarded because the boundary was not recorded keeps the flag
        if (isFALSE(item6) && !sk_s && i > 1L && !is.na(sharp$flags[i - 1L]))
          dropped <- c(dropped, sharp$flags[i - 1L])
        next
      }
      starts[[length(starts) + 1L]] <- list(
        start = i, upper = up, sure = sure, maybe = maybe, status = status,
        skins = sk_s, items = c(texture = tex[i], not_ferralic = .f88_not(ferr[i]),
                                clay_increase = inc, thickness = thick, no_skins_rule = item6),
        flags = c(.f88_flags(sk$flags, maybe),
                  if (!sk_s && i > 1L && !is.na(sharp$flags[i - 1L])) sharp$flags[i - 1L]))
    }
    list(starts = starts, dropped_flags = unique(dropped),
         missing = c("clay_pct", "silt_pct", "sand_pct", "clay_films_amount"))
  })
}

# Natric items 1 and 2 (p. 29) on a B of the argic core.
.f88_natric_extra <- function(x, s) {
  st <- .f88_chr(x, "structure_type")
  L <- s$maybe
  colpr <- any(!is.na(st[L]) & grepl("column|prism", st[L]))
  td <- .f88_num(x, "tongue_depth_cm")
  ug <- .f88_num(x, "uncoated_grains_pct")
  tl <- c(s$start - 1L, L)
  tongues <- any(!is.na(st[L]) & grepl("block", st[L])) &&
    any(!is.na(td[tl]) & td[tl] > 2.5 & !(!is.na(ug[tl]) & ug[tl] == 0))
  i1 <- colpr || tongues
  esp <- .f88_esp(x)
  t0 <- x$top[s$start]
  up40 <- L[x$top[L] < t0 + 40]
  a <- .f88_any(esp$value[up40] > 15)
  mg <- .f88_num(x, "mg_cmol"); na <- .f88_num(x, "na_cmol"); ca <- .f88_num(x, "ca_cmol")
  ea <- .f88_num(x, "extractable_acidity_ph82_cmol")
  b1 <- .f88_any((mg[up40] + na[up40]) > (ca[up40] + ea[up40]))
  w200 <- which(!is.na(x$top) & x$top <= 200 & !x$rock)
  b2 <- .f88_any(esp$value[w200] > 15)
  i2 <- .f88_or(a, .f88_and(b1, b2))
  list(status = .f88_and(i1, i2), i1 = i1, i2 = i2,
       flags = .f88_flags(esp$flags, up40),
       missing = c("esp_pct", "na_cmol", "cec_cmol", "extractable_acidity_ph82_cmol"))
}

.f88_hz_argic_natric <- function(x, natric = FALSE) {
  .f88_memo(x, if (natric) "natric_b" else "argic_b", function() {
    core <- .f88_argic_core(x)
    units <- list(); flags <- core$dropped_flags %||% character(0); miss <- core$missing; ev <- list()
    for (s in core$starts) {
      nx <- .f88_natric_extra(x, s)
      st <- if (natric) .f88_and(s$status, nx$status) else .f88_and(s$status, .f88_not(nx$status))
      flags <- c(flags, s$flags, nx$flags)
      if (is.na(nx$status)) miss <- c(miss, nx$missing)
      ev[[length(ev) + 1L]] <- c(list(start = x$idx[s$start], upper = x$idx[s$upper],
                                      skins = s$skins, natric_structure = nx$i1, natric_sodium = nx$i2),
                                 as.list(s$items))
      if (isFALSE(st)) next
      L <- if (isTRUE(st)) s$sure else s$maybe
      units[[length(units) + 1L]] <- list(layers = L, top = x$top[L[1L]], bottom = x$bot[L[length(L)]],
                                          thickness = x$bot[L[length(L)]] - x$top[L[1L]],
                                          status = if (isTRUE(st)) TRUE else NA)
    }
    list(units = units, flags = unique(flags), missing = unique(miss), evidence = list(starts = ev))
  })
}

argic_b_fao88 <- function(pedon, max_top_cm = 125) {
  x <- .f88_ctx(pedon)
  .f88_result(x, "argic_b", .f88_hz_argic_natric(x, natric = FALSE), max_top_cm)
}

natric_b_fao88 <- function(pedon, max_top_cm = 125) {
  x <- .f88_ctx(pedon)
  .f88_result(x, "natric_b", .f88_hz_argic_natric(x, natric = TRUE), max_top_cm)
}


# --- cambic B (pp. 29-30) ---------------------------------------------------------------

.f88_hz_cambic_b <- function(x) {
  .f88_memo(x, "cambic_b", function() {
    if (!x$n) return(list(units = list()))
    clay <- .f88_num(x, "clay_pct"); silt <- .fao_psd(x$h)$silt
    col <- .f88_colours(x)
    # not part of an argic, natric, spodic B, a histic H, a mollic or umbric A
    excl <- .f88_or(.f88_member(x, .f88_hz_argic_natric(x, FALSE)$units),
                    .f88_member(x, .f88_hz_argic_natric(x, TRUE)$units),
                    .f88_member(x, .f88_hz_spodic_b(x)$units),
                    .f88_member(x, .f88_hz_histic_h(x)$units),
                    .f88_member(x, .f88_hz_dark_a(x, FALSE)$units),
                    .f88_member(x, .f88_hz_dark_a(x, TRUE)$units))
    c1 <- .f88_texture_b(x)
    # 3: structure at least moderate, or rock structure absent in at least
    # half the volume
    gr <- .field_grade_class(.fao_col(x$h, "structure_grade", "character"))
    rs <- .fao_rock_structure(x$h, site = x$site)
    c3 <- .f88_or(!is.na(gr) & gr >= 2L, .f88_cmp(rs$value, "<=", 50))
    # 4: not ferralic in at least one way
    cc <- .fao_cec_clay(x$h, "nh4oac")
    wm <- .f88_num(x, "weatherable_minerals_pct"); wdc <- .f88_num(x, "water_dispersible_clay_pct")
    sc <- ifelse(!is.na(silt) & !is.na(clay) & clay > 0, silt / clay, NA_real_)
    c4 <- .f88_or(.f88_cmp(cc$value, ">", 16), .f88_cmp(wm, ">=", 10),
                  .f88_cmp(wdc, ">", 10), .f88_cmp(sc, ">", 0.2))
    # 5: alteration
    n <- x$n
    caco3 <- .f88_caco3(x)$value
    calc <- .f88_member(x, .f88_hz_calcic(x)$units)
    gc <- .f88_chr(x, "gravel_carbonate_coating")
    eff <- .f88_chr(x, "effervescence_class")
    sp <- .f88_structure_present(x)
    c5 <- rep(NA, n)
    for (i in seq_len(n)) {
      nb <- if (i < n) i + 1L else NA_integer_
      a <- if (is.na(nb)) NA else if (x$rock[nb]) FALSE else
        .f88_or(.f88_cmp(col$cm[i] - col$cm[nb], ">", 0),
                .fao_hue_cmp(col$hue[i], col$hue[nb], "redder"),
                .f88_cmp(clay[i] - clay[nb], ">", 0))
      deeper <- which(seq_len(n) > i & calc %in% TRUE)
      b <- FALSE
      if (length(deeper)) {
        b <- .f88_any(caco3[i] < caco3[deeper])
        g <- any(!is.na(gc[deeper]) & ((gc[deeper] == "complete" & !is.na(gc[i]) & gc[i] %in% c("partial", "none", "bottom")) |
                                         (gc[deeper] == "bottom" & !is.na(gc[i]) & gc[i] == "none")))
        b <- .f88_or(b, g)
      }
      pm <- which(seq_len(n) > i & !is.na(x$master) & grepl("^C", x$master) & !x$rock)
      if (!length(pm)) pm <- which(seq_len(n) >= i & !x$rock)
      absent <- .f88_all(ifelse(!is.na(caco3[pm]), caco3[pm] <= 0,
                                ifelse(!is.na(eff[pm]), grepl("^(none|non|no)", eff[pm]), NA)))
      cc5 <- .f88_and(absent, sp[i], .f88_cmp(rs$value[i], "<", 50))
      c5[i] <- .f88_or(a, b, cc5)
    }
    # 6: not cemented, indurated or brittle when moist
    cem <- .f88_cemented(x)$value
    cm6 <- .f88_chr(x, "consistence_moist")
    c6 <- !(cem | (!is.na(cm6) & grepl("brittle", cm6)))
    ok <- .f88_and(.f88_b_layer(x), .f88_not(excl), c1, c3, c4, c5, c6)
    ok <- .f88_unburied(x, ok)
    units <- .f88_units(x, ok, function(r) r$thickness >= 15 && r$bottom >= 25)
    below_ok <- which(seq_len(n) >= min(c(which(ok %in% TRUE), n + 1L)))
    flags <- c(.f88_flags(rs$flags, which(ok %in% TRUE)), .f88_flags(cc$flags, which(ok %in% TRUE)),
               .f88_flags(.f88_caco3(x)$flags, below_ok),
               if (any(!is.na(wm[!(ok %in% FALSE)]))) "weatherable_minerals_20_200um_for_50_200um")
    list(units = units, flags = flags,
         missing = c("clay_pct", "cec_cmol", "weatherable_minerals_pct", "water_dispersible_clay_pct",
                     "munsell_chroma_moist", "caco3_pct"),
         evidence = list(criteria = data.frame(layer = x$idx, texture = c1, structure = c3,
                                               not_ferralic = c4, alteration = c5,
                                               not_cemented = c6, excluded = excl)))
  })
}

cambic_b_fao88 <- function(pedon, max_top_cm = 125) {
  x <- .f88_ctx(pedon)
  .f88_result(x, "cambic_b", .f88_hz_cambic_b(x), max_top_cm)
}


# --- spodic B (pp. 30-31) -----------------------------------------------------------------

# Below 12.5 cm, or below an A or E when present; one or more of: (1) a
# subhorizon more than 2.5 cm thick continuously cemented by organic matter
# with Fe or Al (cemented, with an h or s suffix); (2) a sandy or
# coarse-loamy texture (clay below 18 %) with dark pellets (spodic_pellets) or
# cracked coatings on the sand grains; (3) the pyrophosphate chemistry of Soil
# Taxonomy 1975 (Q17): a. with pyrophosphate Fe of 0.1 % or more, (Fe + Al)pyro
# / clay >= 0.2, else (Al pyro + OC) / clay >= 0.2; b. (Fe + Al)pyro at least
# half of (Fe + Al)dithionite; c. the index of accumulation, sum of (CEC at pH
# 8.2 per kg clay - clay / 2) x thickness over the subhorizons meeting a and b,
# 65 or more. Without the 1975 data, spodic_horizon_usda() and then the WRB
# spodic() stand in, flagged (Q17).
.f88_hz_spodic_b <- function(x) {
  .f88_memo(x, "spodic_b", function() {
    if (!x$n) return(list(units = list()))
    n <- x$n
    m <- x$master
    ae <- !is.na(m) & grepl("^(A|E)", m)
    bl <- .f88_b_layer(x)
    pos <- vapply(seq_len(n), function(i) {
      if (!bl[i]) return(FALSE)
      if (any(ae[seq_len(i - 1L)])) return(TRUE)
      !is.na(x$top[i]) && x$top[i] >= 12.5
    }, logical(1))
    if (identical(x$edition, "1974")) {
      # 1974 (74 p. 26): the requirements met below 12.5 cm and, when an Ap
      # is present, below the Ap: the deeper of the two (soilKey's reading);
      # a layer that reaches below that depth counts
      ap <- !is.na(x$desg) & grepl("^A[a-z0-9]*p", x$desg)
      last_ap <- if (any(ap)) max(which(ap)) else 0L
      lim <- max(12.5, if (last_ap) x$bot[last_ap] else 0, na.rm = TRUE)
      pos <- vapply(seq_len(n), function(i) {
        bl[i] && i > last_ap && !is.na(x$bot[i]) && x$bot[i] > lim
      }, logical(1))
    }
    pos <- .f88_unburied(x, pos)
    cem <- .f88_cemented(x)$value
    # the illuvial humus or sesquioxides of the designation: h, s, or the
    # older ir (Hugo, 2026-11-09)
    hs <- !is.na(x$desg) & grepl("^[A-Z]+(/[A-Z]+)?[a-z0-9]*([hs]|ir)", x$desg)
    thk <- x$bot - x$top
    i1 <- cem & hs & !is.na(thk) & thk > 2.5
    clay <- .f88_num(x, "clay_pct")
    pel <- .f88_lgl(x, "spodic_pellets"); pel[is.na(pel)] <- FALSE
    cc <- .f88_num(x, "cracked_coatings_pct")
    i2 <- .f88_and(.f88_cmp(clay, "<", 18), pel | (!is.na(cc) & cc > 0))
    fep <- .f88_num(x, "fe_pyro_pct"); alp <- .f88_num(x, "al_pyro_pct")
    oc <- .f88_num(x, "oc_pct"); fed <- .f88_num(x, "fe_dcb_pct"); ald <- .f88_num(x, "al_dcb_pct")
    ratio <- function(num) ifelse(is.na(num) | is.na(clay), NA, ifelse(clay <= 0, TRUE, num / clay >= 0.2))
    a <- ifelse(is.na(fep), NA, ifelse(fep >= 0.1, ratio(fep + alp), ratio(alp + oc)))
    b <- .f88_cmp(fep + alp, ">=", 0.5 * (fed + ald))
    # a layer with free carbonates or a water pH of 7.0 or more holds no
    # spodic accumulation of amorphous material, flagged (Hugo, 2026-11-09)
    cc0 <- .f88_num(x, "caco3_pct"); eff <- .f88_chr(x, "effervescence_class")
    ph <- .f88_num(x, "ph_h2o")
    alk <- (!is.na(cc0) & cc0 > 0) | (!is.na(eff) & grepl("^(slight|moderate|strong|very strong|violent)", eff)) |
      (!is.na(ph) & ph >= 7)
    a[alk] <- FALSE
    ab <- .f88_and(pos, a, b)
    c82 <- .f88_num(x, "cec_ph82_cmol")
    idx <- ifelse(!is.na(clay) & clay > 0 & !is.na(c82) & !is.na(thk),
                  (c82 * 100 / clay - clay / 2) * thk, NA_real_)
    lo <- sum(idx[ab %in% TRUE], na.rm = TRUE)
    hi_na <- any(is.na(idx[!(ab %in% FALSE)]))
    hi <- sum(pmax(idx[!(ab %in% FALSE)], 0), na.rm = TRUE)
    i3 <- if (any(ab %in% TRUE) && !anyNA(idx[ab %in% TRUE]) && lo >= 65) TRUE
          else if (!any(!(ab %in% FALSE)) || (!hi_na && hi < 65)) FALSE else NA
    mk_units <- function(per) {
      u <- lapply(.f88_run_list(x, per %in% TRUE), function(r) c(r, list(status = TRUE)))
      for (r in .f88_run_list(x, !(per %in% FALSE))) {
        if (!all(per[r$layers] %in% TRUE)) u[[length(u) + 1L]] <- c(r, list(status = NA))
      }
      u
    }
    item12 <- .f88_or(.f88_and(pos, i1), .f88_and(pos, i2))
    per <- .f88_or(item12, if (isTRUE(i3)) ab else if (isFALSE(i3)) rep(FALSE, n)
                           else ifelse(ab %in% FALSE, FALSE, NA))
    units <- mk_units(per)
    sp_flags <- if (any(alk & !(per %in% TRUE) & pos %in% TRUE)) "not_spodic_calcareous_or_ph_7"
    ev <- list(cemented = which(i1), pellets_or_coatings = i2, pyro_ratio = a,
               pyro_over_dithionite = b, accumulation_index = c(sure = lo, possible = hi))
    miss <- c("fe_pyro_pct", "al_pyro_pct", "fe_dcb_pct", "al_dcb_pct", "cec_ph82_cmol")
    sure_unit <- any(vapply(units, function(u) isTRUE(u$status), logical(1)))
    if (!sure_unit && is.na(i3)) {
      # the 1975 chemistry undecided: the stand-ins of Q17, which decide only
      # the chemistry; the units of items 1 and 2 stay as they are
      si <- spodic_horizon_usda(x$pedon); who <- "spodic_horizon_usda"
      if (is.na(si$passed)) { si <- spodic(x$pedon); who <- "spodic" }
      if (isTRUE(si$passed)) {
        lay <- match(si$layers, x$idx)
        lay <- lay[!is.na(lay) & !(x$buried[lay] %in% TRUE)]
        sel <- item12 %in% TRUE; sel[lay] <- TRUE
        if (length(lay)) {
          return(list(units = mk_units(ifelse(sel, TRUE, item12)), stand_in = who, evidence = ev,
                      flags = c(sp_flags, "stand_in_for_spodic_chemistry_1975")))
        }
      }
      if (isFALSE(si$passed) || (isTRUE(si$passed))) {
        # the stand-in rules the chemistry out: items 1 and 2 decide
        return(list(units = mk_units(item12), stand_in = who, evidence = ev,
                    flags = c(sp_flags, "stand_in_for_spodic_chemistry_1975"),
                    missing = c("clay_pct", "spodic_pellets")))
      }
      return(list(units = units, evidence = ev, flags = sp_flags,
                  notes = "the stand-ins for the 1975 chemistry are undecided too",
                  missing = unique(c(miss, si$missing))))
    }
    list(units = units, evidence = ev, flags = sp_flags, missing = miss)
  })
}

spodic_b_fao88 <- function(pedon, max_top_cm = 125) {
  x <- .f88_ctx(pedon)
  .f88_result(x, "spodic_b", .f88_hz_spodic_b(x), max_top_cm)
}


# --- ferralic B (p. 31) -------------------------------------------------------------------

# 1 sandy loam or finer with 8 % clay (Q16); 2 at least 30 cm thick; 3 CEC
# (NH4OAc) 16 or less, or ECEC (NH4OAc bases + 1 M KCl acidity) 12 or less,
# cmol(+)/kg clay; 4 weatherable minerals below 10 % (the 20-200 um column
# for the 50-200 um fraction, flagged, Q16); 5 water-dispersible clay below 10
# %; 6 silt/clay 0.2 or less; 7 no andic properties; 8 rock structure below 5
# % by volume.
.f88_hz_ferralic_b <- function(x) {
  .f88_memo(x, "ferralic_b", function() {
    if (!x$n) return(list(units = list()))
    clay <- .f88_num(x, "clay_pct"); silt <- .fao_psd(x$h)$silt
    c1 <- .f88_texture_b(x)
    cc <- .fao_cec_clay(x$h, "nh4oac"); ec <- .fao_ecec_clay(x$h, "1988")
    c3 <- .f88_or(.f88_cmp(cc$value, "<=", 16), .f88_cmp(ec$value, "<=", 12))
    wm <- .f88_num(x, "weatherable_minerals_pct")
    c4 <- .f88_cmp(wm, "<", 10)
    c5 <- .f88_cmp(.f88_num(x, "water_dispersible_clay_pct"), "<", 10)
    sc <- ifelse(!is.na(silt) & !is.na(clay) & clay > 0, silt / clay, NA_real_)
    c6 <- .f88_cmp(sc, "<=", 0.2)
    an <- .f88_andic_layer(x)
    c7 <- .f88_not(an$value)
    rs <- .fao_rock_structure(x$h, site = x$site)
    c8 <- .f88_cmp(rs$value, "<", 5)
    ok <- .f88_unburied(x, .f88_and(.f88_b_layer(x), c1, c3, c4, c5, c6, c7, c8))
    units <- .f88_units(x, ok, function(r) r$thickness >= 30)
    L <- which(!(ok %in% FALSE))
    miss <- c(if (anyNA(c1[L])) c("clay_pct", "silt_pct", "sand_pct"),
              if (anyNA(c3[L])) c("cec_cmol", "exch_acidity_kcl_cmol"),
              if (anyNA(c4[L])) "weatherable_minerals_pct",
              if (anyNA(c5[L])) "water_dispersible_clay_pct",
              if (anyNA(c7[L])) an$missing)
    flags <- c(.f88_flags(cc$flags, L), .f88_flags(ec$flags, L), .f88_flags(an$flags, L),
               .f88_flags(rs$flags, L),
               if (any(!is.na(wm[L]))) "weatherable_minerals_20_200um_for_50_200um")
    list(units = units, missing = unique(miss), flags = unique(flags),
         evidence = list(criteria = data.frame(layer = x$idx, texture = c1, cec = c3,
                                               weatherable = c4, wdc = c5, silt_clay = c6,
                                               not_andic = c7, rock_structure = c8)))
  })
}

ferralic_b_fao88 <- function(pedon, max_top_cm = 125) {
  x <- .f88_ctx(pedon)
  .f88_result(x, "ferralic_b", .f88_hz_ferralic_b(x), max_top_cm)
}


# --- calcic, petrocalcic, gypsic and petrogypsic (pp. 31-32) --------------------------------

# Calcium carbonate of each layer as the calcic horizons read it: caco3_pct;
# not recorded, a field reaction with HCl recorded as none, or a water pH below
# 6.5, reads as no free carbonate (0), flagged (soilKey's reading).
.f88_caco3 <- function(x) {
  .f88_memo(x, "caco3", function() {
    cc <- .f88_num(x, "caco3_pct")
    eff <- .f88_chr(x, "effervescence_class")
    ph <- .f88_num(x, "ph_h2o")
    flag <- rep(NA_character_, x$n)
    e0 <- is.na(cc) & !is.na(eff) & grepl("^(none|non|no |no$|nenhuma|ausente)", eff)
    cc[e0] <- 0; flag[e0] <- "no_carbonate_from_effervescence"
    p0 <- is.na(cc) & !is.na(ph) & ph < 6.5
    cc[p0] <- 0; flag[p0] <- "no_carbonate_at_ph_below_6.5"
    list(value = cc, flags = flag)
  })
}

# Gypsum of each layer: caso4_pct; not recorded, an electrical conductivity of
# the saturation extract below 2 dS/m (a solution saturated with gypsum has
# about 2.2) reads as no gypsum (0), flagged (soilKey's reading). v0.9.392
# (Hugo, 2026-11-10): with no ECe either, an upper bound of the ECe
# (ec_max_dS_m, as the ISIS reader derives from an EC 1:2.5) below 2 dS/m
# also reads as no gypsum, flagged. Shared by the 1974 gypsic horizon.
.f88_gypsum <- function(x) {
  .f88_memo(x, "gypsum", function() {
    g <- .f88_num(x, "caso4_pct")
    ec <- .f88_num(x, "ec_dS_m")
    em <- .f88_num(x, "ec_max_dS_m")
    flag <- rep(NA_character_, x$n)
    z <- is.na(g) & !is.na(ec) & ec < 2
    g[z] <- 0; flag[z] <- "no_gypsum_at_ece_below_2"
    zm <- is.na(g) & is.na(ec) & !is.na(em) & em < 2
    g[zm] <- 0; flag[zm] <- "no_gypsum_at_ece_upper_bound_below_2"
    list(value = g, flags = flag)
  })
}

# Calcic (p. 31), per layer: CaCO3 equivalent 15 % or more and 5 % (points)
# more than a deeper layer; or secondary carbonates in soft forms of 5 % or
# more by volume (secondary_carbonates_pct, carbonate_soft_pct); or, resting
# on very calcareous material (40 % or more in the layer below), secondary
# carbonate recorded (no decrease needed). 15 cm or more.
.f88_calcic_layer <- function(x) {
  .f88_memo(x, "calcic_layer", function() {
    n <- x$n
    cc <- .f88_caco3(x)$value
    sec <- .f88_num(x, "secondary_carbonates_pct"); soft <- .f88_num(x, "carbonate_soft_pct")
    form <- .f88_chr(x, "secondary_carbonates_form")
    kd <- !is.na(x$desg) & grepl("^[A-Z]+(/[A-Z]+)?[a-z0-9]*k", x$desg)
    obs <- (!is.na(sec) & sec > 0) | (!is.na(soft) & soft > 0) | !is.na(form) | kd
    vol <- (!is.na(sec) & sec >= 5) | (!is.na(soft) & soft >= 5)
    v <- rep(NA, n)
    for (i in seq_len(n)) {
      deeper <- which(seq_len(n) > i & !x$rock)
      diff <- if (!length(deeper)) NA else .f88_any(cc[i] - cc[deeper] >= 5)
      nb <- if (i < n && !x$rock[i + 1L]) i + 1L else NA_integer_
      base40 <- !is.na(nb) && !is.na(cc[nb]) && cc[nb] >= 40 && obs[i]
      v[i] <- .f88_and(.f88_cmp(cc[i], ">=", 15), .f88_or(diff, vol[i], base40))
    }
    v[x$rock] <- FALSE
    .f88_unburied(x, v)
  })
}

.f88_hz_calcic <- function(x) {
  .f88_memo(x, "calcic", function() {
    if (!x$n) return(list(units = list()))
    ok <- .f88_calcic_layer(x)
    list(units = .f88_units(x, ok, function(r) r$thickness >= 15),
         flags = .f88_flags(.f88_caco3(x)$flags, which(!(ok %in% FALSE))),
         missing = c("caco3_pct", "secondary_carbonates_pct"))
  })
}

calcic_fao88 <- function(pedon, max_top_cm = 125) {
  x <- .f88_ctx(pedon)
  .f88_result(x, "calcic", .f88_hz_calcic(x), max_top_cm)
}

# Gypsic (p. 32), per layer: 5 % (points) more gypsum (caso4_pct) than the
# underlying C horizon (the first C below; with no C designated, the layer
# right below, flagged); the horizon 15 cm or more thick and its thickness x
# mean gypsum % 150 or more.
.f88_gypsic_layer <- function(x) {
  .f88_memo(x, "gypsic_layer", function() {
    n <- x$n
    gg <- .f88_gypsum(x)
    g <- gg$value
    v <- rep(NA, n); flag <- gg$flags
    for (i in seq_len(n)) {
      cb <- which(seq_len(n) > i & !is.na(x$master) & grepl("^C", x$master) & !x$rock)[1L]
      if (is.na(cb) && i < n && !x$rock[i + 1L]) {
        cb <- i + 1L
        flag[i] <- paste(c(flag[i][!is.na(flag[i])], "no_c_horizon_layer_below_compared"), collapse = "; ")
      }
      v[i] <- if (!is.na(g[i]) && g[i] < 5) FALSE else if (is.na(cb)) NA else .f88_cmp(g[i] - g[cb], ">=", 5)
    }
    v[x$rock] <- FALSE
    list(value = .f88_unburied(x, v), flags = flag)
  })
}

.f88_hz_gypsic <- function(x) {
  .f88_memo(x, "gypsic", function() {
    if (!x$n) return(list(units = list()))
    gl <- .f88_gypsic_layer(x)
    g <- .f88_gypsum(x)$value
    # 1988 (p. 32) 15 cm or more; 1974 (74 p. 27) more than 15 cm
    e74 <- identical(x$edition, "1974")
    units <- .f88_units(x, gl$value, function(r) {
      if (r$thickness < 15 || (e74 && r$thickness <= 15)) return(FALSE)
      th <- pmin(x$bot[r$layers], r$bottom) - pmax(x$top[r$layers], r$top)
      if (anyNA(g[r$layers])) return(NA)
      sum(g[r$layers] * th) >= 150
    })
    list(units = units, flags = .f88_flags(gl$flags, which(!(gl$value %in% FALSE))),
         missing = "caso4_pct")
  })
}

gypsic_fao88 <- function(pedon, max_top_cm = 125) {
  x <- .f88_ctx(pedon)
  .f88_result(x, "gypsic", .f88_hz_gypsic(x), max_top_cm)
}

# Petrocalcic and petrogypsic (p. 32): the calcic or gypsic material of a
# layer, cemented (.f88_cemented()), with no minimum thickness (default 4).
# The material: CaCO3 equivalent 15 % or more, the enrichment not asked (a
# cemented carbonate layer is itself the accumulation; soilKey's reading); for
# the gypsum, the per-layer test of the gypsic horizon.
.f88_hz_petro <- function(x, kind = c("calcic", "gypsic")) {
  kind <- match.arg(kind)
  .f88_memo(x, paste0("petro", kind), function() {
    if (!x$n) return(list(units = list()))
    cem <- .f88_cemented(x, if (kind == "calcic") "k" else "y")
    mat <- if (kind == "calcic") .f88_cmp(.f88_caco3(x)$value, ">=", 15)
           else .f88_gypsic_layer(x)$value
    ok <- .f88_unburied(x, .f88_and(mat, cem$value))
    ok[x$rock] <- FALSE
    units <- .f88_units(x, ok, function(r) TRUE)
    fl <- if (kind == "calcic") .f88_caco3(x)$flags else .f88_gypsum(x)$flags
    list(units = units, flags = c(.f88_flags(cem$flags, which(!(ok %in% FALSE))),
                                  .f88_flags(fl, which(!(ok %in% FALSE)))),
         missing = if (kind == "calcic") "caco3_pct" else "caso4_pct")
  })
}

petrocalcic_fao88 <- function(pedon, max_top_cm = 125) {
  x <- .f88_ctx(pedon)
  .f88_result(x, "petrocalcic", .f88_hz_petro(x, "calcic"), max_top_cm)
}

petrogypsic_fao88 <- function(pedon, max_top_cm = 125) {
  x <- .f88_ctx(pedon)
  .f88_result(x, "petrogypsic", .f88_hz_petro(x, "gypsic"), max_top_cm)
}


# --- sulfuric horizon (p. 32) ---------------------------------------------------------------

# pH (H2O 1:1) below 3.5 over 15 cm or more; ph_h2o stands in, flagged (Q12).
# Jarosite mottles are "generally" present and not required (Q18); they are
# reported.
.f88_hz_sulfuric <- function(x) {
  .f88_memo(x, "sulfuric", function() {
    if (!x$n) return(list(units = list()))
    ph <- .fao_ph(x$h, "1to1")
    ok <- .f88_unburied(x, .f88_cmp(ph$value, "<", 3.5))
    ok[x$rock] <- FALSE
    units <- .f88_units(x, ok, function(r) r$thickness >= 15)
    L <- which(!(ok %in% FALSE))
    jar <- .f88_lgl(x, "jarosite_present")
    list(units = units, flags = .f88_flags(ph$flags, L), missing = ph$missing,
         evidence = list(jarosite = any(jar[L] %in% TRUE)))
  })
}

sulfuric_fao88 <- function(pedon, max_top_cm = 125) {
  x <- .f88_ctx(pedon)
  .f88_result(x, "sulfuric", .f88_hz_sulfuric(x), max_top_cm)
}


# --- albic E (pp. 32-33) ------------------------------------------------------------------

# A layer designated E (EB, E/B; the A2 of the older notation), or an
# undesignated one below a surface layer and above a B or C, read by its
# colour only, flagged: value moist 4 or more or value dry 5 or more; chroma
# (dry or moist) 3 or less where the value dry is 7 or more or the value
# moist 6 or more, below 2.5 ("closer to 2 than to 3") where the value dry is
# 5 or 6 or the value moist 4 or 5; where both apply, the lighter class
# governs (Hugo, 2026-11-09). On a parent material (the first C below) of hue
# 5YR or redder a moist chroma of 3 is allowed, flagged. No minimum
# thickness.
.f88_hz_albic_e <- function(x) {
  .f88_memo(x, "albic_e", function() {
    if (!x$n) return(list(units = list()))
    n <- x$n
    col <- .f88_colours(x)
    vm <- col$vm; cm <- col$cm; cd <- col$cd
    # a dry value not recorded is the moist value + 1, flagged, as for the
    # mollic A (Hugo, 2026-11-09); a dry chroma not recorded is the moist
    # chroma, flagged (soilKey's reading)
    vd <- col$vd_or_inferred
    dry_c <- is.na(cd) & !is.na(cm)
    cd[dry_c] <- cm[dry_c]
    m <- x$master
    is_e <- !is.na(m) & grepl("^E", m)
    undes <- is.na(m)
    light <- .f88_or(.f88_cmp(vm, ">=", 4), .f88_cmp(vd, ">=", 5))
    high <- .f88_or(.f88_cmp(vd, ">=", 7), .f88_cmp(vm, ">=", 6))
    mid <- .f88_or(.f88_and(.f88_cmp(vd, ">=", 5), .f88_cmp(vd, "<=", 6)),
                   .f88_and(.f88_cmp(vm, ">=", 4), .f88_cmp(vm, "<=", 5)))
    le3 <- .f88_or(.f88_cmp(cd, "<=", 3), .f88_cmp(cm, "<=", 3))
    # "closer to 2 than to 3": below 2.5
    le2 <- .f88_or(.f88_cmp(cd, "<", 2.5), .f88_cmp(cm, "<", 2.5))
    flag <- rep(NA_character_, n)
    red <- rep(FALSE, n)
    for (i in seq_len(n)) {
      cb <- which(seq_len(n) > i & !is.na(m) & grepl("^C", m) & !x$rock)[1L]
      if (!is.na(cb)) red[i] <- isTRUE(.fao_hue_cmp(col$hue[cb], "5YR", "as_red"))
    }
    le2r <- .f88_or(le2, red & !is.na(cm) & cm == 3)
    flag[red & !(le2 %in% TRUE) & !is.na(cm) & cm == 3] <- "chroma_3_on_red_parent_material"
    addf <- function(f, sel, lab) ifelse(sel, ifelse(is.na(f), lab, paste(f, lab, sep = "; ")), f)
    e74 <- identical(x$edition, "1974")
    if (e74) {
      # 1974 (74 p. 27): the text prints no chroma limit at a dry value of 5
      # or 6 or a moist value of 4 or 5, yet allows a chroma of 3 on red
      # parent materials, which presupposes one: the chroma below 2.5 of
      # Soil Taxonomy 1975 and of 1988 is read (Hugo, 2026-11-10), flagged
      flag <- addf(flag, mid %in% TRUE & !(high %in% TRUE), "albic_middle_values_chroma_below_2.5")
    }
    flag <- addf(flag, !is.na(col$flags), "dry_value_from_moist_plus_1")
    flag <- addf(flag, dry_c, "dry_chroma_from_moist")
    chroma <- ifelse(high %in% TRUE, le3, ifelse(mid %in% TRUE, le2r,
                     ifelse(high %in% FALSE & mid %in% FALSE, FALSE, NA)))
    colour <- .f88_and(light, as.logical(chroma))
    # an undesignated layer: read by colour only where it lies below a surface
    # layer and above a layer designated B or C; elsewhere undecided when the
    # colour passes (Hugo, 2026-11-09)
    bc <- !is.na(m) & grepl("^(B|C)", m)
    placed <- vapply(seq_len(n), function(i) i > 1L && any(bc[seq_len(n) > i]), logical(1))
    ok <- ifelse(is_e, colour, ifelse(undes, ifelse(placed, colour, ifelse(colour %in% FALSE, FALSE, NA)),
                                      FALSE))
    ok <- .f88_unburied(x, ok)
    ok[x$rock] <- FALSE
    flag <- addf(flag, undes & ok %in% TRUE, "albic_by_colour_only")
    units <- .f88_units(x, ok, function(r) TRUE)
    if (e74 && length(units)) {
      # 1974 (74 p. 27; Hugo, 2026-11-10): an albic E overlies one of the
      # kinds the text names: a spodic, argillic or natric B, a fragipan
      # (a designation with x, or pan_type fragipan) or an impervious layer
      # (rock, a cemented layer, a pan recorded); with none of them below it
      # the layers are albic material, not an albic E (albic_material_fao74())
      sp <- .f88_member(x, .f88_hz_spodic_b(x)$units)
      ar <- .f88_member(x, .f74_hz_argillic_natric(x, FALSE)$units)
      na <- .f88_member(x, .f74_hz_argillic_natric(x, TRUE)$units)
      pan <- .f88_chr(x, "pan_type")
      frag <- (!is.na(x$desg) & grepl("^[A-Z]+(/[A-Z]+)?[a-z0-9]*x", x$desg)) |
        (!is.na(pan) & grepl("fragipan", pan))
      imp <- x$rock | (.f88_cemented(x)$value %in% TRUE) | (!is.na(pan) & !grepl("^(none|no)$", pan))
      kind <- .f88_or(sp, ar, na, frag | imp)
      units <- lapply(units, function(u) {
        below <- seq_len(n)[seq_len(n) > max(u$layers)]
        kb <- if (length(below)) .f88_any(kind[below]) else FALSE
        u$status <- .f88_and(u$status, kb)
        if (is.na(u$status)) u$status <- NA
        u
      })
      dropped <- unlist(lapply(Filter(function(u) isFALSE(u$status), units), `[[`, "layers"))
      units <- Filter(function(u) !isFALSE(u$status), units)
      flag <- addf(flag, seq_len(n) %in% dropped, "e_without_spodic_argillic_natric_fragipan_or_impervious_below")
    }
    list(units = units, flags = .f88_flags(flag, which(!(ok %in% FALSE))),
         missing = c("munsell_value_moist", "munsell_value_dry", "munsell_chroma_moist",
                     "munsell_chroma_dry", if (any(undes & is.na(ok))) "designation"))
  })
}

albic_e_fao88 <- function(pedon, max_top_cm = 125) {
  x <- .f88_ctx(pedon)
  .f88_result(x, "albic_e", .f88_hz_albic_e(x), max_top_cm)
}
