# =============================================================================
# v0.9.387 -- the diagnostic horizons of the Legend of the Soil Map of the
# World (FAO-Unesco 1974, Vol. I, Unesco Paris), pp. 21-27: the H horizon of
# the master horizons (p. 21) and the histic H, the mollic, umbric and ochric
# A (with the very weak and weak grades of the ochric A), the argillic,
# natric, cambic, spodic and oxic B, the calcic, gypsic and sulfuric
# horizons and the albic E. Fourteen predicates <name>_fao74(pedon),
# internal, each a DiagnosticResult with passed TRUE / FALSE / NA, the
# layers, what would decide it (`missing`), the fallbacks used
# (evidence$flags) and the stand-ins (evidence$stand_in). The criteria are
# paraphrased; no text of the book is shipped. The numbers are those of
# data-raw/fao_thresholds_1974.yaml; a _fao74 predicate reads no number of
# 1988.
#
# The 1974 definitions come from the Soil Taxonomy drafts of the time (74 p.
# 23). Where Appendix 4 of the Revised Legend (1988 pp. 113-114) calls a
# horizon unchanged, the readers of R/diagnostics-fao88-horizons.R are shared
# through the edition of the profile context (x$edition = "1974") and branch
# where the printed text differs: the histic H (organic matter, no depth),
# the mollic and umbric A (organic matter 1 % and 4 %, no strongly humic
# waiver, the umbric consistence and plaggen rules), the ochric A (no
# fresh-alluvium rule), the spodic B (below 12.5 cm and below an Ap), the
# gypsic horizon (more than 15 cm) and the albic E (no chroma limit at the
# middle values). The argillic, natric (its argillic base), cambic and oxic B,
# the H horizon, the ochric grades and the sulfuric horizon are read here.
#
# Conventions (plan sections 4.2 and 6; Hugo's decisions of 2026-11-08 in
# data-raw/fao_readings.yaml):
#   * depths from the literal soil surface, H and O included (Q9);
#   * organic matter = organic carbon x 1.724, flagged (Q11);
#   * three values: a measurement not recorded leaves its criterion
#     undecided, a field observation not recorded is absent;
#   * horizons buried under 50 cm or more of new material are not diagnostic
#     (74 p. 32; no andic rule in 1974);
#   * a depth the text does not write is read within 125 cm (Q10; argument
#     max_top_cm);
#   * where the 1974 answer stays undecided, the USDA reader of soilKey stands
#     in, then the WRB one; the result is marked as a stand-in.
# =============================================================================

.F74_HZ <- data.frame(stringsAsFactors = FALSE,
  id = c("h_horizon", "histic_h", "mollic_a", "umbric_a", "ochric_a", "argillic_b",
         "natric_b", "cambic_b", "spodic_b", "oxic_b", "calcic", "gypsic", "sulfuric",
         "albic_e"),
  name = c("H horizon", "Histic H horizon", "Mollic A horizon", "Umbric A horizon",
           "Ochric A horizon", "Argillic B horizon", "Natric B horizon", "Cambic B horizon",
           "Spodic B horizon", "Oxic B horizon", "Calcic horizon", "Gypsic horizon",
           "Sulfuric horizon", "Albic E horizon"),
  page = c("21", "24", "24", "24", "25", "25-26", "26", "26", "26", "27", "27", "27", "27",
           "27"),
  # the stand-ins tried in order when the 1974 answer is undecided (USDA
  # first, WRB second; plan section 6)
  stand_in = c("", "histic_epipedon_usda,histic_horizon", "mollic_epipedon_usda,mollic",
               "umbric_epipedon_usda,umbric_horizon", "", "argillic_usda,argic",
               "natric_horizon_usda,natric_horizon", "cambic", "", "oxic_horizon_usda,ferralic",
               "calcic_horizon_usda,calcic", "gypsic_horizon_usda,gypsic",
               "sulfuric_horizon_usda,thionic", "albic_horizon_usda,albic"))

.f74_cache <- new.env(parent = emptyenv())


# --- the profile ------------------------------------------------------------------

# The context of a profile for the 1974 legend: the context of
# R/diagnostics-fao88-horizons.R built with edition "1974" (buried horizons
# by the 50 cm rule of 74 p. 32), cached apart from the 1988 contexts.
.f74_ctx <- function(pedon) {
  key <- rlang::hash(list(pedon$horizons, pedon$site, "1974"))
  cache <- .f74_cache$ctx %||% list()
  for (e in cache) if (identical(e$key, key)) return(e$x)
  x <- .f88_ctx_build(pedon, edition = "1974")
  .f74_cache$ctx <- c(list(list(key = key, x = x)), utils::head(cache, 7L))
  x
}

# The DiagnosticResult of a 1974 reader: its units cut at max_top_cm, passed
# TRUE when a sure unit remains, NA when only undecided ones do; then, when
# undecided, the stand-ins of .F74_HZ.
.f74_result <- function(x, id, r, max_top_cm = Inf, extra = list(), stand_in = TRUE) {
  info <- .F74_HZ[.F74_HZ$id == id, , drop = FALSE]
  units <- Filter(function(u) !is.na(u$top) && u$top <= max_top_cm, r$units)
  sure <- Filter(function(u) isTRUE(u$status), units)
  passed <- if (length(sure)) TRUE else if (length(units)) NA else FALSE
  if (isFALSE(passed) && isTRUE(r$undecided)) passed <- NA
  if (x$n && all(is.na(x$top) | is.na(x$bot))) {
    passed <- NA
    r$missing <- c("top_cm", "bottom_cm", r$missing)
  }
  lay <- if (isTRUE(passed)) sort(unique(unlist(lapply(sure, `[[`, "layers"))))
         else sort(unique(unlist(lapply(units, `[[`, "layers"))))
  notes <- unique(c(r$notes, if (is.finite(max_top_cm))
    sprintf("upper limit within %s cm of the soil surface (no depth printed; Q10)",
            format(max_top_cm))))
  ev <- c(r$evidence %||% list(), extra, list(
    edition = "1974",
    page = info$page[1L],
    flags = unique(as.character(r$flags)),
    stand_in = unique(as.character(r$stand_in)),
    lower_grade = length(r$stand_in) > 0L,
    units = lapply(units, function(u) list(layers = x$idx[u$layers], top = u$top,
                                           bottom = u$bottom, status = u$status))))
  missing <- if (is.na(passed)) unique(c(r$missing, if (any(is.na(x$buried))) x$buried_missing))
             else character(0)
  layers <- x$idx[lay]
  # the stand-ins, only where the 1974 answer is undecided
  if (is.na(passed) && stand_in && !isTRUE(r$no_stand_in) && nzchar(info$stand_in[1L])) {
    for (fn in strsplit(info$stand_in[1L], ",", fixed = TRUE)[[1L]]) {
      si <- tryCatch(get(fn, mode = "function")(x$pedon), error = function(e) NULL)
      if (is.null(si) || !inherits(si, "DiagnosticResult") || is.na(si$passed)) next
      sl <- as.integer(si$layers)
      if (isTRUE(si$passed)) {
        # the stand-in's layers held to the upper limit, buried layers out
        pos <- match(sl, x$idx)
        keep <- !is.na(pos) & !is.na(x$top[pos]) & x$top[pos] <= max_top_cm &
          !(x$buried[pos] %in% TRUE)
        sl <- sl[keep]
        if (!length(sl)) next
      }
      passed <- isTRUE(si$passed)
      layers <- if (passed) sl else integer(0)
      ev$stand_in <- unique(c(ev$stand_in, fn))
      ev$lower_grade <- TRUE
      ev$flags <- unique(c(ev$flags, paste0("stand_in_", fn)))
      ev$undecided_1974 <- missing
      missing <- character(0)
      break
    }
  }
  DiagnosticResult$new(
    name = paste0(id, "_fao74"),
    passed = passed,
    layers = layers,
    evidence = ev,
    missing = missing,
    reference = sprintf("FAO-Unesco (1974), Soil Map of the World 1:5 000 000, Vol. I Legend, %s, p. %s",
                        info$name[1L], info$page[1L]),
    notes = if (length(c(notes, ev$flags))) paste(unique(c(notes, ev$flags)), collapse = "; ")
            else NA_character_)
}


# --- the H and O horizons (p. 21) -----------------------------------------------------

# The lower limit of organic matter of the H horizon (74 p. 21): 20 % with no
# clay in the mineral fraction to 30 % at more than 60 % clay, linear
# between. Vectorised; NA where the clay is not recorded.
.f74_h_line <- function(clay) 20 + 10 * pmin(clay, 60) / 60

# Saturation of each layer for the H horizon (saturated for prolonged
# periods unless artificially drained, 74 p. 21): a layer designated H is
# saturated and one designated O is not (the text's own master horizons);
# else the drainage of the site (.f88_saturated(): poorly drained or
# artificially drained TRUE, well drained FALSE, else NA).
.f74_saturated <- function(x) {
  m <- x$master
  sat <- rep(.f88_saturated(x), x$n)
  sat[!is.na(m) & grepl("^H", m)] <- TRUE
  sat[!is.na(m) & grepl("^O", m)] <- FALSE
  sat
}

# Organic layers (H or O) by organic matter (74 p. 21 and its footnote 4: a
# mineral horizon has less organic matter than the H and O horizons),
# vectorised over OC, clay and saturation: 35 % OM or more is organic either
# way; below the H line (20 % without the clay) it is mineral; between, a
# saturated layer at or above the line is H and one not saturated is mineral.
.f74_organic_v <- function(oc, clay, sat) {
  om <- as.numeric(oc) * 1.724
  line <- .f74_h_line(clay)
  sat <- rep_len(as.logical(sat), length(om))
  out <- rep(NA, length(om))
  out[!is.na(om) & om >= 35] <- TRUE
  lo <- ifelse(is.na(line), 20, line)
  out[!is.na(om) & om < lo] <- FALSE
  mid <- is.na(out) & !is.na(om)
  out[mid & sat %in% TRUE & ((!is.na(line) & om >= line) | om >= 30)] <- TRUE
  out[mid & sat %in% FALSE] <- FALSE
  out
}

.f74_organic <- function(x) {
  .f88_memo(x, "organic", function() {
    v <- .f74_organic_v(.f88_num(x, "oc_pct"), .f88_num(x, "clay_pct"), .f74_saturated(x))
    v[x$rock] <- FALSE
    list(value = v, missing = c("oc_pct", "drainage_class", "clay_pct"))
  })
}

# The H horizon itself: organic matter at or above the line and saturated;
# per layer (.f74_h_v()) and as units.
.f74_h_v <- function(x) {
  .f88_memo(x, "h_layer", function() {
    om <- .fao_om(.f88_num(x, "oc_pct"))$value
    line <- .f74_h_line(.f88_num(x, "clay_pct"))
    enough <- ifelse(is.na(om), NA, ifelse(om >= 30, TRUE, ifelse(om < 20, FALSE,
                     ifelse(is.na(line), NA, om >= line))))
    v <- .f88_and(enough, .f74_saturated(x))
    v[x$rock] <- FALSE
    v
  })
}

.f74_hz_h <- function(x) {
  .f88_memo(x, "h_horizon", function() {
    if (!x$n) return(list(units = list()))
    om <- .fao_om(.f88_num(x, "oc_pct"))$value
    line <- .f74_h_line(.f88_num(x, "clay_pct"))
    sat <- .f74_saturated(x)
    enough <- ifelse(is.na(om), NA, ifelse(om >= 30, TRUE, ifelse(om < 20, FALSE,
                     ifelse(is.na(line), NA, om >= line))))
    v <- .f74_h_v(x)
    units <- .f88_units(x, v, function(r) TRUE)
    list(units = units, flags = "om_from_oc_x1.724",
         missing = c("oc_pct", "clay_pct", if (anyNA(sat[!(enough %in% FALSE)])) "drainage_class"),
         evidence = list(thickness_cm = vapply(units, function(u) u$thickness, numeric(1))))
  })
}

h_horizon_fao74 <- function(pedon) {
  x <- .f74_ctx(pedon)
  .f74_result(x, "h_horizon", .f74_hz_h(x))
}


# --- histic H (p. 24) -------------------------------------------------------------

# An H horizon more than 20 cm thick (the upper bound, less than 40 cm, 60
# cm with 75 % sphagnum or a bulk density below 0.1, separates the Histosols
# and is reported, not tested, as in 1988); a surface organic layer thinner
# than 25 cm, or a plough layer of 25 cm or more, that mixed to 25 cm holds
# 14 + 14 x clay / 60 % organic matter (14 % without clay, 28 % at more than
# 60 %). Eutric: pH (H2O 1:5) 5.5 or more throughout; dystric: below 5.5 in
# some part. The reader of 1988 with the edition's numbers.
histic_h_fao74 <- function(pedon, base = c("any", "eutric", "dystric"), max_top_cm = 125) {
  base <- match.arg(base)
  x <- .f74_ctx(pedon)
  r <- .f88_hz_histic_h(x)
  if (base != "any" && length(r$units)) {
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
  res <- .f74_result(x, "histic_h", r, max_top_cm, stand_in = base == "any")
  res$evidence$base <- base
  res
}


# --- mollic, umbric and ochric A (pp. 24-25) ------------------------------------------

# Always moist (the umbric A of 74 p. 24 is held to the massive-and-hard
# rule only if it becomes dry): a perudic or peraquic soil_moisture_regime
# recorded at the site (soilKey's reading). Not recorded: it may dry.
.f74_always_moist <- function(x) {
  r <- .f88_site(x, "soil_moisture_regime")
  !is.na(r) && grepl("perudic|peraquic", tolower(as.character(r)))
}

# A plaggen A (74 p. 24, excluded from the umbric A): a manuring term
# (.F88_MANURE_RE) in the layer_origin, anthropic_material or designation of
# a layer of the A, or the WRB plaggic() horizon on one of them. Not
# recorded: absent.
.f74_plaggen <- function(x, layers) {
  if (!length(layers)) return(FALSE)
  lo <- .f88_chr(x, "layer_origin"); am <- .f88_chr(x, "anthropic_material")
  dsg <- tolower(.fao_col(x$h, "designation", "character"))
  term <- (!is.na(lo) & grepl(.F88_MANURE_RE, lo)) | (!is.na(am) & grepl(.F88_MANURE_RE, am)) |
    (!is.na(dsg) & grepl(.F88_MANURE_RE, dsg))
  if (any(term[layers])) return(TRUE)
  pl <- tryCatch(plaggic(x$pedon), error = function(e) NULL)
  isTRUE(pl$passed) && any(match(pl$layers, x$idx) %in% layers)
}

# The mollic A of 1974 (74 p. 24): the reader of 1988 with organic matter 1 %
# (4 % with more than 40 % finely divided lime) and without the strongly
# humic waiver of the colour contrast.
mollic_a_fao74 <- function(pedon) {
  x <- .f74_ctx(pedon)
  .f74_result(x, "mollic_a", .f88_hz_dark_a(x, umbric = FALSE))
}

# The umbric A of 1974 (74 p. 24): as the mollic A with a base saturation
# below 50 %; the massive-and-hard restriction only where the A dries; plaggen
# A horizons excluded.
umbric_a_fao74 <- function(pedon) {
  x <- .f74_ctx(pedon)
  .f74_result(x, "umbric_a", .f88_hz_dark_a(x, umbric = TRUE))
}

# The very weak ochric A (74 p. 25): weighted organic matter in the upper 40
# cm below a limit that falls from 1 % at a weighted sand/clay ratio of 1 or
# less to 0.5 % at 13 or more, linear between (Q26); where hard rock, a
# petrocalcic or petrogypsic horizon or a duripan begins within 40 cm, the
# limits are 1.2 and 0.6 % in the upper 18 cm. The ratio is the weighted
# sand over the weighted clay of the same window (soilKey's reading). The
# weak ochric A is any other ochric A (soilKey's reading of "intermediate
# between the very weak ochric A and the mollic A").
.f74_ochric_limit <- function(ratio, lo, hi) {
  ifelse(is.na(ratio), NA_real_, ifelse(ratio <= 1, lo, ifelse(ratio >= 13, hi,
         lo + (hi - lo) * (ratio - 1) / 12)))
}

.f74_ochric_grade <- function(x) {
  .f88_memo(x, "ochric_grade", function() {
    h <- x$h
    pc <- .f88_member(x, .f88_hz_petro(x, "calcic")$units)
    pg <- .f88_member(x, .f88_hz_petro(x, "gypsic")$units)
    cem <- .f88_cemented(x)$value
    dp <- .f88_num(x, "duripan_pct")
    duri <- cem & ((!is.na(dp) & dp > 0) | (!is.na(x$desg) & grepl("^[A-Z]+[a-z0-9]*qm", x$desg)))
    pan <- .f88_or(x$rock, pc, pg, duri)
    w40 <- which(!is.na(x$top) & x$top <= 40)
    shallow <- .f88_any(pan[w40])
    grade_in <- function(depth, lo, hi) {
      p <- .fao_psd(h)
      oc <- .fao_weighted(h, "oc_pct", 0, depth)
      cl <- .fao_weighted(h, "clay_pct", 0, depth, values = p$clay)
      sa <- .fao_weighted(h, "sand_pct", 0, depth, values = p$sand)
      om <- .fao_om(oc$value)$value
      ratio <- if (is.na(cl$value) || is.na(sa$value)) NA_real_
               else if (cl$value <= 0) Inf else sa$value / cl$value
      lim <- .f74_ochric_limit(ratio, lo, hi)
      list(very_weak = if (is.na(om) || is.na(lim)) NA else om < lim, om = om, ratio = ratio,
           limit = lim, missing = unique(c(oc$missing, cl$missing, sa$missing)),
           flags = unique(c(oc$flags, cl$flags, sa$flags)))
    }
    deep <- grade_in(40, 1, 0.5); shal <- grade_in(18, 1.2, 0.6)
    vw <- if (isTRUE(shallow)) shal$very_weak else if (isFALSE(shallow)) deep$very_weak
          else if (identical(shal$very_weak, deep$very_weak)) deep$very_weak else NA
    list(very_weak = vw, shallow_pan = shallow, deep = deep[c("om", "ratio", "limit")],
         shallow = shal[c("om", "ratio", "limit")],
         missing = unique(c(deep$missing, shal$missing, if (is.na(shallow)) "cementation_class")),
         flags = unique(c(deep$flags, shal$flags, "om_from_oc_x1.724")))
  })
}

# The ochric A (74 p. 25): an A that is not mollic or umbric (the results of
# mollic_a_fao74() and umbric_a_fao74(), their stand-ins included). `grade`
# asks for the very weak or the weak ochric A of the Yermosols and Xerosols;
# evidence$grade gives it ("very weak", "weak", or NA when undecided).
ochric_a_fao74 <- function(pedon, grade = c("any", "very_weak", "weak")) {
  grade <- match.arg(grade)
  x <- .f74_ctx(pedon)
  a <- .f88_a_run(x)
  mo <- mollic_a_fao74(pedon); um <- umbric_a_fao74(pedon)
  if (is.null(a)) {
    r <- list(units = list(), notes = "no A horizon at the surface")
  } else {
    dark <- .f88_or(mo$passed, um$passed)
    status <- .f88_and(.f88_not(dark), !any(x$buried[a$layers] %in% TRUE))
    r <- .f88_one(status, a$layers, x,
                  missing = unique(c(mo$missing, um$missing)),
                  flags = if (a$undesignated) "a_horizon_partly_undesignated",
                  stand_in = unique(c(mo$evidence$stand_in, um$evidence$stand_in)),
                  evidence = list(mollic = mo$passed, umbric = um$passed))
  }
  g <- .f74_ochric_grade(x)
  gr <- if (is.na(g$very_weak)) NA_character_ else if (g$very_weak) "very weak" else "weak"
  if (grade != "any" && length(r$units)) {
    want <- if (grade == "very_weak") g$very_weak else .f88_not(g$very_weak)
    r$units <- lapply(r$units, function(u) { u$status <- .f88_and(u$status, want); u })
    r$units <- Filter(function(u) !isFALSE(u$status), r$units)
    if (is.na(want)) r$missing <- c(r$missing, g$missing, "oc_pct", "clay_pct", "sand_pct")
    r$flags <- c(r$flags, g$flags)
  }
  res <- .f74_result(x, "ochric_a", r)
  res$evidence$grade <- if (isFALSE(res$passed)) NA_character_ else gr
  res$evidence$grade_detail <- g[c("very_weak", "shallow_pan", "deep", "shallow")]
  res$evidence$grade_asked <- grade
  res
}

# The grade of the ochric A: "very weak", "weak", NA (undecided) or "none"
# (no ochric A).
ochric_grade_fao74 <- function(pedon) {
  r <- ochric_a_fao74(pedon)
  if (isFALSE(r$passed)) return("none")
  if (is.na(r$passed)) return(NA_character_)
  r$evidence$grade
}


# --- argillic and natric B (pp. 25-26) ------------------------------------------------

# Swelling 2:1 clays dominant: smectite_pct or vermiculite_pct of 50 or more,
# else a clay_mineralogy naming smectite, montmorillonite or vermiculite.
.f74_two_to_one <- function(x) {
  sm <- .f88_num(x, "smectite_pct"); ve <- .f88_num(x, "vermiculite_pct")
  cm <- .f88_chr(x, "clay_mineralogy")
  (!is.na(sm) & sm >= 50) | (!is.na(ve) & ve >= 50) |
    (is.na(sm) & !is.na(cm) & grepl("smect|montmor|vermicul", cm))
}

# The illuviation evidence of a layer (items 3 to 5, 74 p. 25): clay skins or
# oriented clay (.f88_clay_skins(): clay films few or more, 1 % in thin
# section, or a Bt designation, flagged, Q13), clay bridges between the sand
# grains, or, in a clayey (35 % clay or more) B of swelling 2:1 clays, one of
# the alternatives of item 4 (p. 25): the evidence of pressure (slickensides
# recorded, or a wavy upper boundary with uncoated grains recorded in the
# layer above), a fine/total clay ratio a third higher than the layer above
# or below, or more than 8 points of fine clay over it (the fine-clay tests
# undecided where fine clay is not recorded). The clauses on where the skins must lie (the lower part under a
# broken boundary or a clayey surface; some part over a discontinuity or under
# an Ap) are met by skins in any layer of the B (soilKey's reading).
.f74_illuviation <- function(x) {
  .f88_memo(x, "illuviation74", function() {
    sk <- .f88_clay_skins(x)
    br <- .f88_lgl(x, "clay_bridges")
    clay <- .f88_num(x, "clay_pct"); fc <- .f88_num(x, "fine_clay_pct")
    s <- .fao_col(x$h, "slickensides", "character")
    slick <- !is.na(s) & nzchar(trimws(s)) & !grepl(.F88_NONE_RE, .field_term(s))
    rt <- ifelse(!is.na(clay) & clay > 0, fc / clay, NA_real_)
    n <- x$n
    two <- .f74_two_to_one(x)
    bt <- .field_term(.fao_col(x$h, "boundary_topography", "character"))
    ug <- .f88_num(x, "uncoated_grains_pct")
    pressure <- vapply(seq_len(n), function(i) {
      if (!isTRUE(clay[i] >= 35) || !two[i]) return(FALSE)
      adj <- c(i - 1L, i + 1L); adj <- adj[adj >= 1L & adj <= n]
      wavy <- i > 1L && !is.na(bt[i - 1L]) && grepl("wavy|ondulad", bt[i - 1L]) &&
        isTRUE(ug[i - 1L] > 0)
      ratio <- .f88_any(rt[i] >= rt[adj] * 4 / 3)
      more <- .f88_any(fc[i] - fc[adj] > 8)
      .f88_or(slick[i] || wavy, ratio, more)
    }, NA)
    v <- .f88_or(sk$value, br %in% TRUE, pressure)
    flag <- sk$flags
    flag[pressure %in% TRUE & !(sk$value %in% TRUE)] <- "pressure_faces_of_2to1_clays"
    list(value = v, flags = flag)
  })
}

# Items 1, 2 and 3-5 of the argillic B for each layer that could start it
# (the layer where the clay increase over an eluvial layer is reached within
# 30 cm): (1) the increase of 3 points, a ratio of 1.2 or 8 points (breaks 15
# and 40, default 12), and more fine clay than the eluvial layer where
# fine_clay_pct is recorded in both (else not compared, flagged); (2) one
# tenth of the overlying horizons, or more than 15 cm where the eluvial and
# illuvial horizons reach below 150 cm; 15 cm in sand or loamy sand, else
# 7.5 cm; lamellae 15 cm combined; (3-5) illuviation evidence in some layer
# of the B. No texture limit (1974 has none).
.f74_argillic_core <- function(x) {
  .f88_memo(x, "argillic_core74", function() {
    n <- x$n
    if (n < 2L) return(list(starts = list()))
    h <- x$h
    clay <- .f88_num(x, "clay_pct"); fc <- .f88_num(x, "fine_clay_pct")
    sub <- .f88_unburied(x, .f88_b_layer(x))
    inc <- .fao_clay_increase(h, distance = 30)
    ill <- .f74_illuviation(x)
    tex <- .fao_texture_usda(h)
    lam <- .f88_num(x, "lamellae_cumulative_cm")
    starts <- list()
    for (i in seq_len(n)[-1L]) {
      st0 <- .f88_and(sub[i], inc$value[i])
      if (isFALSE(st0) || is.na(x$top[i]) || is.na(x$bot[i])) next
      up <- inc$upper[i]
      if (is.na(up)) up <- i - 1L
      fine <- if (!is.na(fc[up]) && !is.na(fc[i])) fc[i] > fc[up] else TRUE
      ffl <- if (is.na(fc[up]) || is.na(fc[i])) "fine_clay_not_compared"
      cont <- function(j) .f88_and(sub[j], .fao_clay_increase_pair(clay[up], clay[j]))
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
      th_of <- function(L) {
        if (all(!is.na(lam[L]) & lam[L] > 0)) return(sum(lam[L]) >= 15)
        th <- x$bot[L[length(L)]] - x$top[i]
        sandy <- tex[L]
        if (anyNA(sandy)) return(NA)
        need <- if (all(sandy %in% c("S", "LS"))) 15 else 7.5
        frac <- th >= 0.1 * x$top[i] || (x$bot[L[length(L)]] > 150 && th > 15)
        th >= need && frac
      }
      ts <- th_of(sure); tm <- th_of(maybe)
      thick <- if (isTRUE(ts)) TRUE else if (isFALSE(tm) && isFALSE(ts)) FALSE else NA
      il <- if (any(ill$value[sure] %in% TRUE)) TRUE
            else if (any(!(ill$value[maybe] %in% FALSE))) NA else FALSE
      status <- .f88_and(st0, fine, thick, il)
      if (isFALSE(status)) next
      starts[[length(starts) + 1L]] <- list(
        start = i, upper = up, sure = sure, maybe = maybe, status = status,
        items = c(clay_increase = inc$value[i], fine_clay = fine, thickness = thick,
                  illuviation = il),
        flags = c(.f88_flags(ill$flags, maybe), ffl))
    }
    list(starts = starts, missing = c("clay_pct", "silt_pct", "sand_pct", "clay_films_amount"))
  })
}

# Argillic (natric = FALSE) or natric (TRUE) B: the core and, for the natric
# B, the structure and sodium of 74 p. 26 (columnar or prismatic structure or
# blocky with tongues of more than 2.5 cm; ESP above 15 % in the upper 40 cm,
# or Mg + Na above Ca + acidity at pH 8.2 there with ESP above 15 % within
# 200 cm), read by the shared .f88_natric_extra(); the argillic B lacks them.
.f74_hz_argillic_natric <- function(x, natric = FALSE) {
  .f88_memo(x, if (natric) "natric74" else "argillic74", function() {
    core <- .f74_argillic_core(x)
    units <- list(); flags <- character(0); miss <- core$missing; ev <- list()
    only_natric <- TRUE; nat_si <- character(0)
    for (s in core$starts) {
      nx <- .f88_natric_extra(x, s)
      st <- if (natric) .f88_and(s$status, nx$status) else .f88_and(s$status, .f88_not(nx$status))
      # the argillic B with only its natric exclusion (item 6, p. 26)
      # undecided: natric_horizon_usda(), then the WRB natric_horizon(),
      # decide the exclusion alone, flagged; the 1974 layers are kept
      if (!natric && isTRUE(s$status) && is.na(nx$status)) {
        for (fn in c("natric_horizon_usda", "natric_horizon")) {
          si <- tryCatch(get(fn, mode = "function")(x$pedon), error = function(e) NULL)
          if (is.null(si) || !inherits(si, "DiagnosticResult") || is.na(si$passed)) next
          st <- !isTRUE(si$passed); nat_si <- c(nat_si, fn)
          flags <- c(flags, paste0("stand_in_", fn, "_for_natric_exclusion"))
          break
        }
      }
      if (is.na(st) && !(isTRUE(s$status) && is.na(nx$status))) only_natric <- FALSE
      flags <- c(flags, s$flags, nx$flags)
      if (is.na(nx$status)) miss <- c(miss, nx$missing)
      ev[[length(ev) + 1L]] <- c(list(start = x$idx[s$start], upper = x$idx[s$upper],
                                      natric_structure = nx$i1, natric_sodium = nx$i2),
                                 as.list(s$items))
      if (isFALSE(st)) next
      L <- if (isTRUE(st)) s$sure else s$maybe
      units[[length(units) + 1L]] <- list(layers = L, top = x$top[L[1L]], bottom = x$bot[L[length(L)]],
                                          thickness = x$bot[L[length(L)]] - x$top[L[1L]],
                                          status = if (isTRUE(st)) TRUE else NA)
    }
    list(units = units, flags = unique(flags), missing = unique(miss), evidence = list(starts = ev),
         stand_in = unique(nat_si), no_stand_in = !natric && only_natric)
  })
}

argillic_b_fao74 <- function(pedon, max_top_cm = 125) {
  x <- .f74_ctx(pedon)
  .f74_result(x, "argillic_b", .f74_hz_argillic_natric(x, natric = FALSE), max_top_cm)
}

natric_b_fao74 <- function(pedon, max_top_cm = 125) {
  x <- .f74_ctx(pedon)
  .f74_result(x, "natric_b", .f74_hz_argillic_natric(x, natric = TRUE), max_top_cm)
}


# --- cambic B (p. 26) -------------------------------------------------------------------

# Not part of an argillic, natric or spodic B, a histic H, a mollic or
# umbric A; not cemented, indurated or brittle when moist; (1) very fine
# sand, loamy very fine sand or finer: sandy loam or finer passes, sand and
# loamy sand fail, flagged (soilKey records no very fine sand; soilKey's
# reading); (2) soil structure, or no rock structure in at least half the
# volume; (3) CEC (NH4OAc) above 16 me per 100 g clay, or more than 3 %
# weatherable minerals other than muscovite (weatherable_minerals_pct less
# muscovite_pct; muscovite not recorded, the total, flagged), or more than 6
# % muscovite; (4) alteration: more clay, a stronger chroma or a redder hue
# than the layer below; the removal of carbonates (as 1988); or the colours
# of reduction (moist chroma 2 or less with mottles; without mottles below 1
# at a value below 4, 1 or less at 4 or more); (5) its base 25 cm or more
# below the surface. No minimum thickness.
.f74_hz_cambic <- function(x) {
  .f88_memo(x, "cambic74", function() {
    if (!x$n) return(list(units = list()))
    n <- x$n
    clay <- .f88_num(x, "clay_pct")
    col <- .f88_colours(x)
    excl <- .f88_or(.f88_member(x, .f74_hz_argillic_natric(x, FALSE)$units),
                    .f88_member(x, .f74_hz_argillic_natric(x, TRUE)$units),
                    .f88_member(x, .f88_hz_spodic_b(x)$units),
                    .f88_member(x, .f88_hz_histic_h(x)$units),
                    .f88_member(x, .f88_hz_dark_a(x, FALSE)$units),
                    .f88_member(x, .f88_hz_dark_a(x, TRUE)$units))
    tex <- .fao_texture_usda(x$h)
    c1 <- ifelse(is.na(tex), NA, !(tex %in% c("S", "LS")))
    flag <- ifelse(tex %in% c("S", "LS"), "sand_or_loamy_sand_read_as_coarser_than_very_fine_sand",
                   NA_character_)
    rs <- .fao_rock_structure(x$h)
    c2 <- .f88_or(.f88_structure_present(x), .f88_cmp(rs$value, "<=", 50))
    cc <- .fao_cec_clay(x$h, "nh4oac")
    wm <- .f88_num(x, "weatherable_minerals_pct"); mu <- .f88_num(x, "muscovite_pct")
    other <- ifelse(is.na(wm), NA_real_, ifelse(is.na(mu), wm, pmax(wm - mu, 0)))
    flag <- .f88_addf(flag, !is.na(wm) & is.na(mu) & wm > 3, "weatherable_minerals_muscovite_not_separated")
    # a CEC by NH4Cl above 16 decides; 16 or less leaves the NH4OAc CEC
    # undecided (as the oxic B)
    mth <- .fao_cec_method(x$h)
    cecv <- .f88_cmp(cc$value, ">", 16)
    cecv[!is.na(mth) & mth == "nh4cl" & cecv %in% FALSE] <- NA
    c3 <- .f88_or(cecv, .f88_cmp(other, ">", 3), .f88_cmp(mu, ">", 6))
    caco3 <- .f88_caco3(x)$value
    calc <- .f88_member(x, .f88_hz_calcic(x)$units)
    gc <- .f88_chr(x, "gravel_carbonate_coating")
    mo <- .f88_mottles(x)$present
    red <- ifelse(is.na(col$cm), NA,
                  ifelse(mo, col$cm <= 2, ifelse(is.na(col$vm), NA,
                         ifelse(col$vm < 4, col$cm < 1, col$cm <= 1))))
    c4 <- rep(NA, n)
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
      c4[i] <- .f88_or(a, b, red[i])
    }
    cem <- .f88_cemented(x)$value
    cm6 <- .f88_chr(x, "consistence_moist")
    c6 <- !(cem | (!is.na(cm6) & grepl("brittle", cm6)))
    ok <- .f88_and(.f88_b_layer(x), .f88_not(excl), c1, c2, c3, c4, c6)
    ok <- .f88_unburied(x, ok)
    units <- .f88_units(x, ok, function(r) r$bottom >= 25)
    L <- which(!(ok %in% FALSE))
    flags <- c(.f88_flags(flag, which(ok %in% TRUE | tex %in% c("S", "LS"))),
               .f88_flags(rs$flags, which(ok %in% TRUE)), .f88_flags(cc$flags, which(ok %in% TRUE)))
    list(units = units, flags = unique(flags),
         missing = unique(c(if (anyNA(c1[L])) c("clay_pct", "silt_pct", "sand_pct"),
                            if (anyNA(c3[L])) c("cec_cmol", "weatherable_minerals_pct"),
                            if (anyNA(c4[L])) c("munsell_chroma_moist", "caco3_pct"))),
         evidence = list(criteria = data.frame(layer = x$idx, texture = c1, structure = c2,
                                               weatherable = c3, alteration = c4,
                                               reduction = red, not_cemented = c6,
                                               excluded = excl)))
  })
}

cambic_b_fao74 <- function(pedon, max_top_cm = 125) {
  x <- .f74_ctx(pedon)
  .f74_result(x, "cambic_b", .f74_hz_cambic(x), max_top_cm)
}


# --- spodic B (p. 26) -------------------------------------------------------------------

# The spodic B of 1988 (Appendix 4: unchanged; the pyrophosphate chemistry
# of Soil Taxonomy 1975, Q17, with its stand-ins) placed below 12.5 cm and,
# when an Ap is present, below the Ap (74 p. 26).
spodic_b_fao74 <- function(pedon, max_top_cm = 125) {
  x <- .f74_ctx(pedon)
  .f74_result(x, "spodic_b", .f88_hz_spodic_b(x), max_top_cm)
}


# --- oxic B (p. 27) -----------------------------------------------------------------------

# Not argillic or natric; (1) 30 cm or more; (2) NH4+ retained from
# unbuffered 1 N NH4Cl 10 me or less per 100 g clay (cec_cmol with cec_method
# nh4cl, else ecec_cmol, flagged: .fao_cec_clay(method = "nh4cl"), Q12), or
# NH4OAc bases + KCl aluminium below 10 me per 100 g clay
# (.fao_ecec_clay(edition = "1974")); (3) CEC (NH4OAc) 16 me or less per 100 g
# clay (the Al-interlayered chlorite exception is not read); (4) no more than
# traces of primary aluminosilicates: weatherable_minerals_pct of 3 or less
# (soilKey's number: the line of "significant amounts" of the cambic B, 74
# p. 26), flagged; (5) sandy loam or finer with more than 15 % clay; (6)
# mostly gradual or diffuse boundaries between its subhorizons: the recorded
# internal boundaries, more than half of them abrupt or clear fails; none
# recorded passes, flagged (soilKey's reading); (7) rock structure below 5 %.
.f74_hz_oxic <- function(x) {
  .f88_memo(x, "oxic74", function() {
    if (!x$n) return(list(units = list()))
    n <- x$n
    clay <- .f88_num(x, "clay_pct")
    excl <- .f88_or(.f88_member(x, .f74_hz_argillic_natric(x, FALSE)$units),
                    .f88_member(x, .f74_hz_argillic_natric(x, TRUE)$units))
    m <- .fao_cec_method(x$h)
    nh <- .fao_cec_clay(x$h, "nh4cl")
    ec <- .fao_ecec_clay(x$h, "1974")
    c2 <- .f88_or(.f88_cmpr(nh$value, "<=", 10), .f88_cmpr(ec$value, "<", 10))
    cc <- .fao_cec_clay(x$h, "nh4oac")
    cc$value[!is.na(m) & m == "nh4cl"] <- NA_real_
    c3 <- .f88_cmpr(cc$value, "<=", 16)
    wm <- .f88_num(x, "weatherable_minerals_pct")
    c4 <- .f88_cmp(wm, "<=", 3)
    c5 <- .f88_and(.fao_sandy_loam_or_finer(x$h), .f88_cmp(clay, ">", 15))
    rs <- .fao_rock_structure(x$h)
    c7 <- .f88_cmp(rs$value, "<", 5)
    ok <- .f88_unburied(x, .f88_and(.f88_b_layer(x), .f88_not(excl), c2, c3, c4, c5, c7))
    bd <- .field_term(.fao_col(x$h, "boundary_distinctness", "character"))
    bflags <- character(0)
    units <- .f88_units(x, ok, function(r) {
      if (r$thickness < 30) return(FALSE)
      inner <- r$layers[-length(r$layers)]
      b <- bd[inner]; b <- b[!is.na(b) & nzchar(b)]
      if (!length(inner)) return(TRUE)
      if (!length(b)) { bflags <<- c(bflags, "oxic_internal_boundaries_not_recorded"); return(TRUE) }
      sharp <- sum(grepl("abrupt|clear|clara", b))
      sharp <= length(b) / 2
    })
    L <- which(!(ok %in% FALSE))
    miss <- c(if (anyNA(c2[L])) c("cec_cmol (cec_method nh4cl)", "al_kcl_cmol"),
              if (anyNA(c3[L])) "cec_cmol",
              if (anyNA(c4[L])) "weatherable_minerals_pct",
              if (anyNA(c5[L])) c("clay_pct", "silt_pct", "sand_pct"))
    flags <- c(.f88_flags(ec$flags, L), .f88_flags(nh$flags, L), .f88_flags(cc$flags, L),
               .f88_flags(rs$flags, L),
               if (any(!is.na(wm[L]))) "traces_of_primary_minerals_read_as_3pct", bflags)
    list(units = units, missing = unique(miss), flags = unique(flags),
         evidence = list(criteria = data.frame(layer = x$idx, retention = c2, cec = c3,
                                               primary_minerals = c4, texture = c5,
                                               rock_structure = c7, excluded = excl)))
  })
}

oxic_b_fao74 <- function(pedon, max_top_cm = 125) {
  x <- .f74_ctx(pedon)
  .f74_result(x, "oxic_b", .f74_hz_oxic(x), max_top_cm)
}


# --- calcic and gypsic horizons (p. 27) ---------------------------------------------------

# The calcic horizon of 1988 (Appendix 4: unchanged; the 74 p. 27 numbers
# are the same: 15 cm, 15 % CaCO3 equivalent, 5 % more than the C, no
# decrease needed over 40 % material).
calcic_fao74 <- function(pedon, max_top_cm = 125) {
  x <- .f74_ctx(pedon)
  .f74_result(x, "calcic", .f88_hz_calcic(x), max_top_cm)
}

# The gypsic horizon (74 p. 27): more than 15 cm thick, 5 % more gypsum than
# the C, thickness x gypsum % 150 or more (the reader of 1988 with the
# strict thickness of 1974).
gypsic_fao74 <- function(pedon, max_top_cm = 125) {
  x <- .f74_ctx(pedon)
  .f74_result(x, "gypsic", .f88_hz_gypsic(x), max_top_cm)
}


# --- sulfuric horizon (p. 27) ---------------------------------------------------------------

# pH (H2O 1:1) below 3.5 (ph_h2o stands in, flagged, Q12) and jarosite
# mottles of hue 2.5Y or yellower and chroma 6 or more (Q18: required in
# 1974): jarosite_present, or a recorded mottle colour of that hue and
# chroma; not recorded, absent, flagged. No thickness is printed: none is
# asked (soilKey's reading).
.f74_jarosite <- function(x) {
  .f88_memo(x, "jarosite74", function() {
    jp <- .f88_lgl(x, "jarosite_present")
    mc <- .fao_col(x$h, "mottle_colors", "character")
    col <- vapply(seq_len(x$n), function(i) {
      if (is.na(mc[i]) || !nzchar(trimws(mc[i]))) return(FALSE)
      m <- .f88_mottle_table(mc[i])
      if (!nrow(m)) return(FALSE)
      any((.fao_hue_cmp(m$hue, "2.5Y", "as_yellow") & m$chroma >= 6) %in% TRUE)
    }, logical(1))
    v <- (jp %in% TRUE) | col
    flag <- ifelse(is.na(jp) & !col, "jarosite_not_recorded_absent", NA_character_)
    list(value = v, flags = flag)
  })
}

.f74_hz_sulfuric <- function(x) {
  .f88_memo(x, "sulfuric74", function() {
    if (!x$n) return(list(units = list()))
    ph <- .fao_ph(x$h, "1to1")
    jar <- .f74_jarosite(x)
    ok <- .f88_unburied(x, .f88_and(.f88_cmp(ph$value, "<", 3.5), jar$value))
    ok[x$rock] <- FALSE
    units <- .f88_units(x, ok, function(r) TRUE)
    acid <- which(.f88_cmp(ph$value, "<", 3.5) %in% TRUE | is.na(ph$value))
    list(units = units, missing = ph$missing,
         flags = c(.f88_flags(ph$flags, which(!(ok %in% FALSE))), .f88_flags(jar$flags, acid)),
         evidence = list(jarosite = which(jar$value)))
  })
}

sulfuric_fao74 <- function(pedon, max_top_cm = 125) {
  x <- .f74_ctx(pedon)
  .f74_result(x, "sulfuric", .f74_hz_sulfuric(x), max_top_cm)
}


# --- albic E (p. 27) ----------------------------------------------------------------------

# The albic E of 1988 (Appendix 4: amended, 1988 p. 113) read as printed in
# 1974, with the colour reader of 1988 and its 1974 branch: a
# moist value of 4 or more or a dry value of 5 or more; chroma 3 or less
# where the dry value is 7 or more or the moist value 6 or more; no chroma
# limit at the middle values (74 p. 27, checked on the page image).
albic_e_fao74 <- function(pedon, max_top_cm = 125) {
  x <- .f74_ctx(pedon)
  .f74_result(x, "albic_e", .f88_hz_albic_e(x), max_top_cm)
}


# =============================================================================
# v0.9.388 -- the diagnostic properties of the 1974 legend (74 pp. 27-32) and
# the terms the 1974 key uses without a definition section: 28 predicates
# <name>_fao74(pedon), internal, each a DiagnosticResult as the horizons
# above, with the per-layer answers in evidence$values. Where Appendix 4 of
# 1988 calls a property unchanged (interfingering, permafrost, plinthite,
# slickensides, smeary consistence, soft powdery lime, tonguing, weatherable
# minerals), the reader of R/diagnostics-fao88-properties.R runs on the 1974
# context (its argillic and cambic B are those of 1974); calcareous and soft
# powdery lime read the numbers of 1988, flagged (Q23). The rest is read
# here. A property with no depth in the text has its upper limit within 125
# cm (Q10; argument max_top_cm); `window_cm` reads "throughout top-bottom".
# =============================================================================

.F74_PR <- data.frame(stringsAsFactors = FALSE,
  id = c("abrupt_textural_change", "albic_material", "aridic_regime", "amorphous_complex",
         "ferralic_properties", "ferric_properties", "gilgai", "high_om_b", "high_salinity",
         "hydromorphic_properties", "interfingering", "permafrost", "plinthite",
         "slickensides", "smeary", "soft_powdery_lime", "sulfidic_materials",
         "takyric_features", "thin_iron_pan", "tonguing", "vertic_properties",
         "weatherable_minerals", "recent_alluvial_deposits", "calcareous",
         "bleached_coatings", "heavy_clay", "exchangeable_sodium", "acric_cec"),
  name = c("Abrupt textural change", "Albic material", "Aridic moisture regime",
           "Exchange complex dominated by amorphous material", "Ferralic properties",
           "Ferric properties", "Gilgai microrelief", "High organic matter content in the B horizon",
           "High salinity", "Hydromorphic properties", "Interfingering", "Permafrost",
           "Plinthite", "Slickensides", "Smeary consistence", "Soft powdery lime",
           "Sulfidic materials", "Takyric features", "Thin iron pan", "Tonguing",
           "Vertic properties", "Weatherable minerals", "Recent alluvial deposits",
           "Calcareous", "Bleached coatings on ped surfaces", "Heavy clay",
           "Exchangeable sodium", "Acric CEC"),
  page = c("27-28", "28", "28", "28", "28", "28", "28", "28-29", "29", "29-30", "30", "30",
           "30", "30-31", "31", "31", "31", "31", "31", "31", "31", "32", "32", "33-53",
           "48", "48", "48", "47"),
  stand_in = c("", "", "", "andic_soil_properties_usda,andic_properties", "", "", "", "",
               "salic", "aquic_conditions_usda,gleyic_properties,stagnic_properties", "", "",
               "", "", "", "", "sulfidic_materials_usda", "takyric_properties", "", "", "",
               "", "", "", "", "", "", ""))

# The DiagnosticResult of a 1974 property (.f88_pr_result() hands a 1974
# context here): the 1974 table, names and pages; the stand-ins of .F74_PR
# when the 1974 answer is undecided (their layers held to `max_top_cm` when
# given).
.f74_pr_result <- function(x, id, st, flags = character(0), missing = character(0),
                           evidence = list(), notes = character(0), stand_in = character(0),
                           values = NULL, max_top_cm = NULL, window_cm = NULL) {
  info <- .F74_PR[.F74_PR$id == id, , drop = FALSE]
  passed <- st$value
  if (length(passed) != 1L) passed <- NA
  missing <- c(st$missing, missing)
  if (x$n && all(is.na(x$top) | is.na(x$bot))) {
    passed <- NA
    missing <- c("top_cm", "bottom_cm", missing)
  }
  if (!x$n) passed <- FALSE
  lay <- x$idx[st$layers %||% integer(0)]
  depth_note <- if (!is.null(window_cm))
    sprintf("throughout %s-%s cm of the soil surface", format(window_cm[1L]), format(window_cm[2L]))
  else if (!is.null(max_top_cm))
    sprintf("within %s cm of the soil surface", format(max_top_cm))
  else "upper limit within 125 cm of the soil surface (no depth printed; Q10)"
  flags <- unique(as.character(c(flags, st$flags)))
  flags <- flags[!is.na(flags) & nzchar(flags)]
  stand_in <- unique(as.character(stand_in))
  missing <- if (is.na(passed)) unique(c(missing, if (any(is.na(x$buried))) x$buried_missing))
             else character(0)
  und <- character(0)
  if (is.na(passed) && nzchar(info$stand_in[1L])) {
    for (fn in strsplit(info$stand_in[1L], ",", fixed = TRUE)[[1L]]) {
      si <- tryCatch(get(fn, mode = "function")(x$pedon), error = function(e) NULL)
      if (is.null(si) || !inherits(si, "DiagnosticResult") || is.na(si$passed)) next
      sl <- as.integer(si$layers)
      if (isTRUE(si$passed) && !is.null(max_top_cm) && length(sl)) {
        tp <- x$top[match(sl, x$idx)]
        sl <- sl[!is.na(tp) & tp <= max_top_cm]
        if (!length(sl)) next
      }
      passed <- isTRUE(si$passed)
      lay <- if (passed) sl else integer(0)
      stand_in <- unique(c(stand_in, fn))
      flags <- unique(c(flags, paste0("stand_in_", fn)))
      und <- missing; missing <- character(0)
      break
    }
  }
  ev <- c(evidence, list(
    edition = "1974",
    page = info$page[1L],
    flags = flags,
    stand_in = stand_in,
    lower_grade = length(stand_in) > 0L,
    undecided_1974 = und,
    values = if (!is.null(values) && x$n) data.frame(layer = x$idx, value = as.logical(values))
             else NULL))
  DiagnosticResult$new(
    name = paste0(id, "_fao74"),
    passed = passed,
    layers = lay,
    evidence = ev,
    missing = missing,
    reference = sprintf("FAO-Unesco (1974), Soil Map of the World 1:5 000 000, Vol. I Legend, %s, p. %s",
                        info$name[1L], info$page[1L]),
    notes = paste(unique(c(notes, depth_note, flags)), collapse = "; "))
}

# A 1974 property read per layer (the reader of .f88_pr_layers() on the 1974
# context).
.f74_pr_layers <- function(pedon, id, reader, max_top_cm = NULL, window_cm = NULL) {
  .f88_pr_layers(pedon, id, reader, max_top_cm, window_cm, ctx = .f74_ctx)
}

# A site-level answer as a result.
.f74_pr_site <- function(pedon, id, value, flags = character(0), missing = character(0),
                         evidence = list(), layers = integer(0)) {
  x <- .f74_ctx(pedon)
  st <- list(value = value, layers = layers)
  r <- .f74_pr_result(x, id, st, flags = flags, missing = missing, evidence = evidence)
  if (x$n == 0L && !is.na(value)) r$passed <- value
  r
}

# Mottles that are distinct or prominent (mottle_contrast); mottles recorded
# without their contrast are not read as distinct (soilKey's reading).
.f74_mottles_dp <- function(x) {
  mo <- .f88_mottles(x)$present
  ct <- .field_term(.fao_col(x$h, "mottle_contrast", "character"))
  mo & !is.na(ct) & grepl("distinct|prominent|distint|proeminent", ct)
}

# The NH4Cl CEC per 100 g clay (me = cmol(+)/kg) of the ferralic and ferric
# properties and the Acric Ferralsols (Q12).
.f74_nh4cl <- function(x) .f88_memo(x, "nh4cl74", function() .fao_cec_clay(x$h, "nh4cl"))


# --- abrupt textural change (pp. 27-28) -----------------------------------------------------

# At the contact of an A or E horizon with the horizon below: where the A or
# E has less than 20 % clay, the clay at least doubles; where it has 20 % or
# more, it increases by 20 points and some part of the lower horizon (the
# layer and its adjoining layers of the same master horizon) has at least
# double the clay of the A or E; within 8 cm or less. "Within 8 cm" is a
# boundary recorded abrupt or clear (the classes of the 1967 Guidelines the
# text uses, 74 p. 24: abrupt below 2.5 cm, clear 2.5 to 6.5 cm); gradual
# (6.5 to 12.5 cm) leaves it undecided; diffuse fails; not recorded, absent, flagged (the
# reading of 1988 at 5 cm, Hugo 2026-11-09). A layer whose top is 8 cm or
# less below the base of the A or E, across a thin layer, is compared too.
# An undesignated upper layer may be an A or E (undecided where the clay
# passes). Answered on the lower layer.
.f74_abrupt_v <- function(x) {
  .f88_memo(x, "pr_abrupt74", function() {
    n <- x$n
    clay <- .f88_num(x, "clay_pct")
    m <- x$master
    ae <- ifelse(is.na(m), NA, grepl("^(A|E)", m))
    bd <- .field_term(.fao_col(x$h, "boundary_distinctness", "character"))
    rec <- !is.na(bd) & nzchar(bd)
    near <- ifelse(!rec, FALSE, ifelse(grepl("abrupt|clear|clara", bd), TRUE,
                   ifelse(grepl("gradual", bd), NA, FALSE)))
    v <- rep(FALSE, n); flag <- rep(NA_character_, n)
    grp <- substr(ifelse(is.na(m), "", m), 1L, 1L)
    lower_part <- function(i) {
      k <- i
      while (k < n && !x$rock[k + 1L] && nzchar(grp[i]) && grp[k + 1L] == grp[i]) k <- k + 1L
      seq.int(i, k)
    }
    inc_of <- function(j, i) {
      if (is.na(clay[j]) || is.na(clay[i])) return(NA)
      if (clay[j] < 20) return(clay[i] > clay[j] && clay[i] >= 2 * clay[j])
      if (clay[i] - clay[j] < 20) return(FALSE)
      lp <- clay[lower_part(i)]
      if (any(lp >= 2 * clay[j], na.rm = TRUE)) TRUE else if (anyNA(lp)) NA else FALSE
    }
    for (i in seq_len(n)[-1L]) {
      if (x$rock[i] || is.na(x$top[i])) next
      res <- logical(0)
      for (j in seq_len(i - 1L)) {
        if (x$rock[j] || is.na(x$bot[j]) || isFALSE(ae[j])) next
        gap <- x$top[i] - x$bot[j]
        if (j == i - 1L && abs(gap) <= 0.5) {
          inc <- inc_of(j, i)
          r <- .fao_and(ae[j], inc, near[j])
          if (!isFALSE(inc) && !rec[j]) flag[i] <- .f88_addf(flag[i], TRUE, "boundary_distinctness_not_recorded_absent")
        } else if (gap > 0.5 && gap <= 8) {
          r <- .fao_and(ae[j], inc_of(j, i))
          if (isTRUE(r)) flag[i] <- .f88_addf(flag[i], TRUE, "change_across_thin_layer")
        } else next
        res <- c(res, r)
      }
      v[i] <- if (length(res)) .fao_or(res) else FALSE
    }
    list(value = .f88_pr_clean(x, v), flags = flag, missing = c("clay_pct", "designation"))
  })
}

abrupt_textural_change_fao74 <- function(pedon, max_top_cm = NULL) {
  .f74_pr_layers(pedon, "abrupt_textural_change", .f74_abrupt_v, max_top_cm)
}


# --- albic material (p. 28) --------------------------------------------------------------------

# The colours of the albic E in a layer that is not an albic E: a moist value
# of 4 or more or a dry value of 5 or more; chroma 3 or less where the dry
# value is 7 or more or the moist value 6 or more; below 2.5 at the middle
# values (a dry value of 5 or 6, a moist value of 4 or 5), 3 allowed over a
# parent material of hue 5YR or redder (Hugo, 2026-11-10: the 1974 text
# prints no limit there, but its 5YR clause presupposes this one, that of
# Soil Taxonomy 1975 and of 1988), flagged. A dry value not recorded is the
# moist value + 1, a dry chroma the moist chroma, flagged (the readings of
# the albic E).
.f74_albic_colour <- function(x) {
  col <- .f88_colours(x)
  vm <- col$vm; cm <- col$cm; cd <- col$cd; vd <- col$vd_or_inferred
  dc <- is.na(cd) & !is.na(cm); cd[dc] <- cm[dc]
  m <- x$master
  light <- .f88_or(.f88_cmp(vm, ">=", 4), .f88_cmp(vd, ">=", 5))
  high <- .f88_or(.f88_cmp(vd, ">=", 7), .f88_cmp(vm, ">=", 6))
  mid <- .f88_or(.f88_and(.f88_cmp(vd, ">=", 5), .f88_cmp(vd, "<=", 6)),
                 .f88_and(.f88_cmp(vm, ">=", 4), .f88_cmp(vm, "<=", 5)))
  le3 <- .f88_or(.f88_cmp(cd, "<=", 3), .f88_cmp(cm, "<=", 3))
  le2 <- .f88_or(.f88_cmp(cd, "<", 2.5), .f88_cmp(cm, "<", 2.5))
  red <- vapply(seq_len(x$n), function(i) {
    cb <- which(seq_len(x$n) > i & !is.na(m) & grepl("^C", m) & !x$rock)[1L]
    !is.na(cb) && isTRUE(.fao_hue_cmp(col$hue[cb], "5YR", "as_red"))
  }, logical(1))
  le2r <- .f88_or(le2, red & !is.na(cm) & cm == 3)
  chroma <- ifelse(high %in% TRUE, le3, ifelse(mid %in% TRUE, le2r,
                   ifelse(high %in% FALSE & mid %in% FALSE, FALSE, NA)))
  flag <- rep(NA_character_, x$n)
  flag <- .f88_addf(flag, !is.na(col$flags), "dry_value_from_moist_plus_1")
  flag <- .f88_addf(flag, dc, "dry_chroma_from_moist")
  flag <- .f88_addf(flag, mid %in% TRUE & !(high %in% TRUE), "albic_middle_values_chroma_below_2.5")
  flag <- .f88_addf(flag, red & !(le2 %in% TRUE) & !is.na(cm) & cm == 3 & mid %in% TRUE,
                    "chroma_3_on_red_parent_material")
  list(value = .f88_and(light, as.logical(chroma)), flags = flag)
}

# Albic material: the colours above in a layer that is not part of an albic E
# (an E with none of the kinds of 74 p. 27 below it is albic material, Hugo
# 2026-11-10; an E whose albic E is undecided leaves it undecided).
albic_material_fao74 <- function(pedon, max_top_cm = NULL, window_cm = NULL) {
  .f74_pr_layers(pedon, "albic_material", function(x) {
    a <- .f74_albic_colour(x)
    ae <- .f88_member(x, .f88_hz_albic_e(x)$units)
    v <- a$value
    v[ae %in% TRUE] <- FALSE
    v[is.na(ae) & v %in% TRUE] <- NA
    list(value = .f88_pr_clean(x, v), flags = a$flags,
         missing = c("munsell_value_moist", "munsell_value_dry", "munsell_chroma_moist"))
  }, max_top_cm, window_cm)
}


# --- aridic moisture regime (p. 28) ----------------------------------------------------------

# The Soil Taxonomy rule of the moisture control section (Q26): the
# soil_moisture_regime recorded at the site decides (aridic or torric TRUE,
# any other FALSE); smr_aridic_usda() reads the same record, so a regime not
# recorded is undecided.
aridic_regime_fao74 <- function(pedon) {
  r <- pedon$site$soil_moisture_regime %||% NA_character_
  r <- if (length(r) && !is.na(r[[1L]]) && nzchar(trimws(as.character(r[[1L]]))))
         tolower(trimws(as.character(r[[1L]]))) else NA_character_
  v <- if (is.na(r)) NA else grepl("aridic|torric", r)
  .f74_pr_site(pedon, "aridic_regime", v, missing = "soil_moisture_regime",
               evidence = list(soil_moisture_regime = r))
}


# --- exchange complex dominated by amorphous material (p. 28) --------------------------------

# Q20: a fine-earth bulk density below 0.85 (bulk_density_g_cm3, read for
# the 1/3-bar density, flagged) and at least one of the other marks: CEC at
# pH 8.2 above 150 me per 100 g clay; where the 15-bar water is 20 % or more
# (or not recorded), a NaF pH above 9.4; 15-bar water over clay above 1.0;
# organic carbon above 0.6 %; a low-temperature DTA endotherm. Undecided
# without the density, or with no other mark decided; then the stand-ins.
.f74_amorphous_v <- function(x) {
  .f88_memo(x, "pr_amorphous74", function() {
    bd <- .f88_num(x, "bulk_density_g_cm3"); clay <- .f88_num(x, "clay_pct")
    c82 <- .f88_num(x, "cec_ph82_cmol"); naf <- .f88_num(x, "ph_naf")
    w15 <- .f88_num(x, "water_content_1500kpa"); oc <- .f88_num(x, "oc_pct")
    dta <- .f88_lgl(x, "dta_low_endotherm")
    m1 <- .f88_cmpr(ifelse(!is.na(clay) & clay > 0, c82 * 100 / clay, NA_real_), ">", 150)
    m2 <- ifelse(!is.na(w15) & w15 < 20, FALSE, .f88_cmp(naf, ">", 9.4))
    m3 <- .f88_cmpr(ifelse(!is.na(clay) & clay > 0, w15 / clay, NA_real_), ">", 1)
    m4 <- .f88_cmp(oc, ">", 0.6)
    m5 <- dta
    other <- .f88_or(m1, as.logical(m2), m3, m4, m5)
    v <- .f88_and(.f88_cmp(bd, "<", 0.85), other)
    flag <- ifelse(!is.na(bd), "bulk_density_as_recorded_for_one_third_bar", NA_character_)
    list(value = .f88_pr_clean(x, v), flags = flag,
         missing = c("bulk_density_g_cm3", "cec_ph82_cmol", "ph_naf", "water_content_1500kpa",
                     "oc_pct", "dta_low_endotherm"),
         evidence = list(marks = data.frame(layer = x$idx, cec82 = m1, naf = as.logical(m2),
                                            water15 = m3, oc = m4, dta = m5)))
  })
}

amorphous_complex_fao74 <- function(pedon, max_top_cm = NULL, window_cm = NULL) {
  .f74_pr_layers(pedon, "amorphous_complex", .f74_amorphous_v, max_top_cm, window_cm)
}


# --- ferralic and ferric properties (p. 28) ------------------------------------------------

# Ferralic: CEC (NH4Cl) below 24 me per 100 g clay (Q12: ecec_cmol for the
# NH4Cl CEC, flagged), "respectively" (p. 28; Hugo, 2026-11-09): in some
# subhorizon of the cambic B (of 1974) for the Cambisols (kind "cambisol"),
# in the horizon right below the A for the Arenosols (kind "arenosol").
ferralic_properties_fao74 <- function(pedon, kind = c("cambisol", "arenosol"), max_top_cm = NULL) {
  kind <- match.arg(kind)
  .f74_pr_layers(pedon, "ferralic_properties", function(x) {
    cb <- if (kind == "cambisol") .f88_in_units(x, .f74_hz_cambic(x)) else rep(FALSE, x$n)
    below_a <- rep(FALSE, x$n)
    a <- .f88_a_run(x)
    if (kind == "arenosol" && !is.null(a)) {
      k <- max(a$layers) + 1L
      if (k <= x$n && !x$rock[k]) below_a[k] <- TRUE
    }
    cc <- .f74_nh4cl(x)
    v <- .f88_and(.f88_or(cb, below_a), .f88_cmpr(cc$value, "<", 24))
    flag <- .f88_addf(cc$flags, below_a & !(cb %in% TRUE), "horizon_below_a")
    list(value = .f88_pr_clean(x, v, bury = TRUE), flags = flag, missing = cc$missing,
         evidence = list(kind = kind, cambic = cb, below_a = below_a))
  }, max_top_cm)
}

# Ferric: the mottles and nodules of 1988 (the same text: many coarse
# mottles redder than 7.5YR or of chroma above 5, "many" 15 % and coarse
# above 20 mm, Q22; nodules up to 2 cm), or a CEC (NH4Cl) below 24 me per 100
# g clay in some subhorizon of the argillic B (of 1974).
ferric_properties_fao74 <- function(pedon, max_top_cm = NULL) {
  .f74_pr_layers(pedon, "ferric_properties", function(x) {
    f <- .f88_ferric_v(x)
    ar <- .f88_in_units(x, .f74_hz_argillic_natric(x, FALSE))
    cc <- .f74_nh4cl(x)
    low <- .f88_and(ar, .f88_cmpr(cc$value, "<", 24))
    v <- .f88_or(f$value, low)
    flag <- .f88_addf(f$flags, low %in% TRUE, "ferric_by_nh4cl_cec_in_argillic_b")
    flag <- .f88_addf(flag, low %in% TRUE & !is.na(cc$flags), "ecec_for_nh4cl_cec")
    list(value = .f88_pr_clean(x, v, bury = TRUE), flags = flag,
         missing = c(f$missing, cc$missing), evidence = f$evidence)
  }, max_top_cm)
}


# --- gilgai (p. 28) ----------------------------------------------------------------------------

# gilgai_presence; else a microrelief_form of the site or of the surface
# layer naming gilgai (another recorded microrelief rules it out), flagged;
# else absent (the reading of the gilgai phase of 1988, Hugo 2026-11-09).
gilgai_fao74 <- function(pedon) {
  x <- .f74_ctx(pedon)
  a <- .f88ph_gilgai(x, list())
  .f74_pr_site(pedon, "gilgai", a$value, flags = a$flags)
}


# --- high organic matter content in the B (pp. 28-29) ---------------------------------------

# "ferralsol_nitosol": a thickness-weighted organic matter of 1.35 % or more
# over 100 cm (an O horizon excluded: the 100 cm start below it); the depth
# below a soil on rock counts as zero, as the strongly humic soils of 1988
# (soilKey's reading). "acrisol": that, or 1.5 % organic matter or more in
# the upper part of the B (the first layer of the argillic B of 1974).
high_om_b_fao74 <- function(pedon, kind = c("ferralsol_nitosol", "acrisol")) {
  kind <- match.arg(kind)
  x <- .f74_ctx(pedon)
  id <- "high_om_b"
  if (!x$n) return(.f74_pr_result(x, id, list(value = FALSE, layers = integer(0))))
  o <- !is.na(x$master) & grepl("^O", x$master)
  first <- which(!o & !is.na(x$top))[1L]
  t0 <- if (is.na(first)) 0 else x$top[first]
  h <- x$h
  w <- .fao_weighted(h, "oc_pct", t0, t0 + 100, zero_below_rock = TRUE,
                     values = ifelse(o, 0, .f88_num(x, "oc_pct")))
  om <- .fao_om(w$value)$value
  v <- .f88_cmpr(om, ">=", 1.35)
  lay <- w$layers %||% integer(0)
  flags <- c(w$flags, "om_from_oc_x1.724", if (any(o)) "o_horizon_excluded")
  upper_b <- NA
  if (kind == "acrisol") {
    ar <- .f74_hz_argillic_natric(x, FALSE)
    us <- Filter(function(u) isTRUE(u$status), ar$units)
    if (length(us)) {
      i <- us[[1L]]$layers[1L]
      upper_b <- .f88_cmpr(.fao_om(.f88_num(x, "oc_pct")[i])$value, ">=", 1.5)
      if (isTRUE(upper_b)) lay <- i
    } else if (length(ar$units)) upper_b <- NA else upper_b <- FALSE
    v <- .f88_or(v, upper_b)
  }
  st <- list(value = v, layers = if (isFALSE(v)) integer(0) else lay)
  .f74_pr_result(x, id, st, flags = flags, missing = c(w$missing, "oc_pct"),
                 evidence = list(kind = kind, om_weighted_100cm = om, om_upper_b = upper_b),
                 window_cm = c(t0, t0 + 100))
}


# --- high salinity (p. 29) ---------------------------------------------------------------------

# An ECe above 15 dS/m (mmhos/cm) at some time of the year within 125, 90 or
# 75 cm of the surface by the texture class of the upper 30 cm (coarse,
# medium, fine; Q10, .fao_depth_by_texture()), or above 4 within 25 cm where
# the pH (H2O 1:1) is above 8.5 (ph_h2o stands in, flagged, Q12).
# ec_max_dS_m (an upper bound) rules a layer out where ec_dS_m is not
# recorded. Texture not recorded: a layer within 75 cm decides a pass, none
# within 125 cm a failure, else undecided.
high_salinity_fao74 <- function(pedon) {
  x <- .f74_ctx(pedon)
  id <- "high_salinity"
  if (!x$n) return(.f74_pr_result(x, id, list(value = FALSE, layers = integer(0))))
  ec <- .f88_num(x, "ec_dS_m"); em <- .f88_num(x, "ec_max_dS_m")
  ub <- is.na(ec) & !is.na(em)
  e15 <- .f88_cmp(ec, ">", 15); e15[ub & em <= 15] <- FALSE
  ph <- .fao_ph(x$h, "1to1")
  e4 <- .f88_and(.f88_cmp(ec, ">", 4), .f88_cmp(ph$value, ">", 8.5)); e4[ub & em <= 4] <- FALSE
  e15 <- .f88_pr_clean(x, e15); e4 <- .f88_pr_clean(x, e4)
  d <- .fao_depth_by_texture(x$h)
  if (!is.na(d$value)) {
    a <- .fao_within(x$h, e15, d$value)
  } else {
    a75 <- .fao_within(x$h, e15, 75); a125 <- .fao_within(x$h, e15, 125)
    a <- if (isTRUE(a75$value)) a75 else if (isFALSE(a125$value)) a125
         else .fao_ans(NA, missing = c("clay_pct", "sand_pct", a125$missing), layers = a125$layers)
  }
  b <- .fao_within(x$h, e4, 25)
  v <- .fao_or(a$value, b$value)
  lay <- unique(c(if (!isFALSE(a$value)) a$layers, if (!isFALSE(b$value)) b$layers))
  .f74_pr_result(x, id, list(value = v, layers = lay),
                 flags = c(a$flags, b$flags, .f88_flags(ph$flags, b$layers %||% integer(0))),
                 missing = c("ec_dS_m", a$missing, b$missing, d$missing),
                 evidence = list(depth_cm = d$value, texture_class = d$class %||% NA_character_),
                 max_top_cm = if (is.na(d$value)) 125 else d$value)
}


# --- hydromorphic properties (pp. 29-30) --------------------------------------------------------

# One or more of (Q22: the 1974 test itself, with aquic_conditions_usda()
# and the WRB gleyic and stagnic properties as stand-ins only):
# (1) groundwater whose capillary fringe reaches the surface (the reader of
#     the gleyic properties of 1988: a water table not perched within 100 cm,
#     .f88_groundwater()), read on the surface layer;
# (2) a histic H;
# (3) a dominant hue N or bluer than 10Y;
# (4) the colours of reduction, by the kind of soil (74 pp. 29-30), the first
#     that applies: 4.1 a spodic B; 4.2 a mollic A; 4.3 an argillic B right
#     below an Ap or an A of rubbed moist value below 3.5; 4.4 an oxic B; 4.5
#     other soils. The saturation the text names is not tested: the colours
#     are its evidence (soilKey's reading, as Q21 for 1988). Mottles are
#     mottles recorded (.f88_mottles()); "distinct or prominent" is the
#     mottle_contrast. The waivers for red parent materials and the colours
#     of uncoated grains are not read.
.f74_hydro_v <- function(x) {
  .f88_memo(x, "pr_hydro74", function() {
    n <- x$n
    col <- .f88_colours(x)
    vm <- col$vm; cm <- col$cm
    hr <- .fao_hue_rank(col$hue)
    neutral <- vapply(as.character(col$hue), function(h) identical(.munsell_hue_parts(h)$family, "N"),
                      logical(1), USE.NAMES = FALSE)
    mo <- .f88_mottles(x)$present
    dp <- .f74_mottles_dp(x)
    mc <- .fao_col(x$h, "mottle_colors", "character")
    mtab <- lapply(mc, .f88_mottle_table)
    flag <- rep(NA_character_, n)
    # (1)-(3)
    gw <- .f88_groundwater(x)
    i1 <- rep(FALSE, n)
    s0 <- which(!x$rock & !is.na(x$top))[1L]
    if (!is.na(s0) && isTRUE(gw$value)) i1[s0] <- TRUE
    if (!is.na(s0) && length(gw$flags)) flag[s0] <- .f88_addf(flag[s0], TRUE, paste(gw$flags, collapse = "; "))
    i2 <- .f88_member(x, .f88_hz_histic_h(x)$units)
    i3 <- ifelse(neutral, TRUE, ifelse(is.na(hr), NA, hr > 30))
    # the kind of soil
    sp_u <- .f88_hz_spodic_b(x)$units
    sp <- .f88_member(x, sp_u)
    k1 <- .f88_any(sp)
    mol <- .f88_hz_dark_a(x, FALSE)
    k2 <- .f88_status(mol)
    ar <- .f74_hz_argillic_natric(x, FALSE)
    arm <- .f88_member(x, ar$units)
    rub <- .fao_colour_rubbed(x$h)
    placed <- NA
    if (length(ar$units)) {
      i0 <- min(vapply(ar$units, function(u) u$layers[1L], 1L))
      above <- i0 - 1L
      if (above >= 1L) {
        ap <- !is.na(x$desg[above]) && grepl("^A[a-z0-9]*p", x$desg[above])
        dark <- !is.na(x$master[above]) && grepl("^A", x$master[above]) &&
          isTRUE(rub$value[above] < 3.5)
        placed <- ap || dark
      } else placed <- FALSE
      placed <- .fao_and(.f88_status(ar), placed)
    } else placed <- FALSE
    ox <- .f88_member(x, .f74_hz_oxic(x)$units)
    k4 <- .f88_any(ox)
    # 4.1 spodic soils
    b41 <- rep(FALSE, n)
    ae <- .f88_member(x, .f88_hz_albic_e(x)$units)
    sp_sure <- which(sp %in% TRUE)
    if (length(sp_sure)) {
      top_sp <- min(sp_sure)
      b41[ae %in% TRUE & mo] <- TRUE
      if (mo[top_sp]) b41[top_sp] <- TRUE
      cem <- .f88_cemented(x)$value; dpp <- .f88_num(x, "duripan_pct")
      duri <- cem & ((!is.na(dpp) & dpp > 0) | (!is.na(x$desg) & grepl("^[A-Z]+[a-z0-9]*qm", x$desg)))
      b41[ae %in% TRUE & duri] <- TRUE
      lowfe <- isTRUE(vm[top_sp] < 4) || isFALSE(.fao_ignition_redder(x$h)$value[top_sp])
      bl <- max(sp_sure) + 1L
      if (lowfe && bl <= n && mo[bl]) {
        sz <- .field_term(.fao_col(x$h, "mottle_size_class", "character"))[bl]
        if (!is.na(sz) && grepl("fine|medium|fino|medio", sz)) b41[bl] <- TRUE
      }
      tp <- .f88_member(x, .f74_hz_thin_pan(x)$units) %in% TRUE
      for (i in which(tp)) {
        nb <- i + 1L
        on <- nb <= n && (isTRUE(sp[nb]) || (!is.na(x$desg[nb]) && grepl("^[A-Z]+[a-z0-9]*x", x$desg[nb])))
        if (on || isTRUE(ae[i])) b41[i] <- TRUE
      }
    }
    # 4.2 mollic soils
    b42 <- rep(FALSE, n)
    mu <- Filter(function(u) !isFALSE(u$status), mol$units)
    if (length(mu)) {
      L <- max(mu[[1L]]$layers)
      cl <- cm[L]
      below0 <- if (L < n) L + 1L else NA_integer_
      below <- below0
      # "or within 75 cm if a calcic horizon intervenes": the chroma 1 or
      # less branch only
      calc <- .f88_member(x, .f88_hz_calcic(x)$units)
      if (!is.na(below) && isTRUE(calc[below])) {
        nxt <- which(seq_len(n) > L & !(calc %in% TRUE) & !x$rock & !is.na(x$top) & x$top <= 75)[1L]
        below <- nxt
      }
      ok_below <- !is.na(below) && !x$rock[below]
      ok_below0 <- !is.na(below0) && !x$rock[below0]
      if (is.na(cl)) {
        b42[L] <- NA
      } else if (cl <= 1) {
        if (dp[L]) b42[L] <- TRUE
        if (ok_below) {
          i <- below; r <- hr[i]; c <- cm[i]
          v <- if (neutral[i]) TRUE else if (is.na(r) || is.na(c)) NA
               else if (r < 21.25) (if (mo[i]) c < 1.5 else c < 1)
               else if (r >= 21.25 && r < 23.75) (if (dp[i]) c <= 2 else c <= 1)
               else if (r <= 30) (if (dp[i]) c <= 3 else c <= 2)
               else TRUE
          if (!isFALSE(v)) b42[i] <- v
        }
      } else if (cl <= 2) {
        if (dp[L]) b42[L] <- TRUE
        if (ok_below0) {
          i <- below0
          mt <- mtab[[i]]
          low_m <- nrow(mt) > 0 && any((mt$value >= 4 & mt$chroma < 2) %in% TRUE)
          high_m <- nrow(mt) > 0 && any((mt$chroma > 2) %in% TRUE)
          v <- if (is.na(vm[i]) || is.na(cm[i])) NA
               else (vm[i] == 4 && cm[i] == 2 && low_m) || (vm[i] == 4 && cm[i] < 2) ||
                 (vm[i] >= 5 && cm[i] <= 2 && high_m)
          if (!isFALSE(v)) b42[i] <- v
        }
      }
    }
    # 4.3 argillic soils
    cc <- .f88_num(x, "coatings_chroma_moist")
    fmn <- .f88_num(x, "fe_mn_nodules_pct"); nsz <- .f88_num(x, "nodule_size_max_cm")
    conc <- !is.na(fmn) & fmn > 0 & (is.na(nsz) | nsz > 0.2)
    flag <- .f88_addf(flag, !is.na(fmn) & fmn > 0 & is.na(nsz) & arm %in% TRUE,
                      "fe_mn_concretion_size_not_recorded_accepted")
    mhigh <- vapply(seq_len(n), function(i) {
      mt <- mtab[[i]]; nrow(mt) > 0 && !is.na(cm[i]) && any((mt$chroma > cm[i]) %in% TRUE)
    }, logical(1))
    y25 <- vapply(as.character(col$hue), function(h) {
      p <- .munsell_hue_parts(h); identical(p$family, "Y") && isTRUE(p$number %in% c(2.5, 5))
    }, logical(1), USE.NAMES = FALSE)
    A3 <- .f88_or(.f88_cmp(cm, "<=", 2), mo, conc)
    B3 <- .f88_or(.f88_and(.f88_cmp(cc, "<=", 2), mo), .f88_and(.f88_cmp(cm, "<=", 2), mo & mhigh),
                  .f88_cmp(cc, "<=", 1), .f88_cmp(cm, "<=", 1), y25 & dp)
    b43 <- .f88_and(arm, A3, B3)
    # 4.4 oxic soils
    b44 <- rep(FALSE, n)
    pl <- .f88_num(x, "plinthite_pct")
    b44[!is.na(pl) & pl >= 50 & !is.na(x$top) & x$top <= 30] <- TRUE
    a <- .f88_a_run(x)
    if (!is.null(a)) {
      k <- max(a$layers) + 1L
      if (k <= n && !x$rock[k] && isTRUE(vm[max(a$layers)] < 3.5) && !mo[k]) {
        b44[k] <- if (is.na(cm[k])) NA else cm[k] <= 2
      }
    }
    w50 <- !is.na(x$top) & x$top <= 50
    b44 <- .f88_or(b44, w50 & dp & .f88_cmp(cm, "<=", 3))
    # 4.5 other soils
    tex <- .fao_texture_usda(x$h)
    coarse <- ifelse(is.na(tex), NA, tex %in% c("S", "LS"))
    unm <- ifelse(is.na(vm) | is.na(cm), NA, ifelse(vm < 4, cm < 1, cm <= 1))
    fine45 <- ifelse(mo, .f88_cmp(cm, "<=", 2), unm)
    red45 <- ifelse(is.na(hr), NA, hr <= 20)
    crs45 <- ifelse(red45 %in% TRUE, fine45,
             ifelse(red45 %in% FALSE & hr <= 30, ifelse(dp, .f88_cmp(cm, "<=", 3), .f88_cmp(cm, "<=", 1)),
             ifelse(red45 %in% FALSE, FALSE, NA)))
    b45 <- ifelse(coarse %in% FALSE, fine45, ifelse(coarse %in% TRUE, crs45, NA))
    b45 <- as.logical(b45)
    # the first kind that applies chooses the branch; an undecided kind (a
    # spodic, argillic or oxic B or a mollic A left undecided) is read both
    # ways and a layer is decided where every reading agrees, else undecided
    # (Hugo, 2026-11-09)
    bl <- list(as.logical(b41), as.logical(b42), as.logical(b43), as.logical(b44), as.logical(b45))
    ks <- c(k1, k2, placed, k4)
    opts <- lapply(ks, function(k) if (is.na(k)) c(TRUE, FALSE) else k)
    combos <- expand.grid(opts)
    branches <- unique(apply(combos, 1L, function(r) { w <- which(unlist(r)); if (length(w)) w[1L] else 5L }))
    vals <- do.call(cbind, bl[branches])
    i4 <- apply(vals, 1L, function(v) if (all(v %in% TRUE)) TRUE else if (all(v %in% FALSE)) FALSE else NA)
    v <- .f88_or(i1, i2, i3, i4)
    flag <- .f88_addf(flag, i4 %in% TRUE & !(i1 | i2 %in% TRUE), "colours_read_as_evidence_of_saturation")
    if (length(branches) > 1L)
      flag <- .f88_addf(flag, !(v %in% FALSE), "kind_of_soil_undecided_branches_read_both_ways")
    kind <- if (length(branches) == 1L) c("spodic", "mollic", "argillic", "oxic", "other")[branches]
            else NA_character_
    list(value = .f88_pr_clean(x, v), flags = flag,
         missing = c("munsell_chroma_moist", "munsell_value_moist", "munsell_hue_moist",
                     "mottle_colors", "mottle_contrast"),
         evidence = list(kind_of_soil = kind,
                         items = data.frame(layer = x$idx, groundwater = i1, histic = i2,
                                            hue = i3, colours = i4)))
  })
}

hydromorphic_properties_fao74 <- function(pedon, max_top_cm = 50) {
  .f74_pr_layers(pedon, "hydromorphic_properties", .f74_hydro_v, max_top_cm)
}


# --- properties shared with 1988 (Appendix 4: unchanged) -----------------------------------

interfingering_fao74 <- function(pedon, max_top_cm = NULL) {
  .f74_pr_layers(pedon, "interfingering", .f88_interfingering_v, max_top_cm)
}

permafrost_fao74 <- function(pedon, max_top_cm = NULL) {
  .f88_permafrost_x(.f74_ctx(pedon), max_top_cm)
}

# Plinthite (74 p. 30): no amount printed; present = recorded above 0 % (Q23).
plinthite_fao74 <- function(pedon, max_top_cm = NULL) {
  .f74_pr_layers(pedon, "plinthite", function(x) .f88_plinthite_v(x), max_top_cm)
}

slickensides_fao74 <- function(pedon, max_top_cm = NULL) {
  .f74_pr_layers(pedon, "slickensides", .f88_slickensides_v, max_top_cm)
}

smeary_fao74 <- function(pedon, max_top_cm = NULL) {
  .f74_pr_layers(pedon, "smeary", .f88_smeary_v, max_top_cm)
}

# Soft powdery lime (74 p. 31): "a significant accumulation", read with the
# numbers of 1988 (50 % of the faces, 5 % soft nodules), flagged (Q23).
soft_powdery_lime_fao74 <- function(pedon, max_top_cm = NULL) {
  .f74_pr_layers(pedon, "soft_powdery_lime", function(x) {
    r <- .f88_spl_v(x)
    r$flags <- .f88_addf(r$flags, !(r$value %in% FALSE), "numbers_of_1988_q23")
    r
  }, max_top_cm)
}

tonguing_fao74 <- function(pedon, kind = c("albic", "chernozem"), max_top_cm = NULL) {
  kind <- match.arg(kind)
  .f74_pr_layers(pedon, "tonguing", function(x) .f88_tonguing_v(x, kind), max_top_cm)
}

# Weatherable minerals (74 p. 32): the 0.02-0.2 mm fraction is the fraction
# of weatherable_minerals_pct (no flag); `min_pct` defaults to the 3 % of
# the cambic B (74 p. 26).
weatherable_minerals_fao74 <- function(pedon, min_pct = 3, max_top_cm = NULL) {
  .f74_pr_layers(pedon, "weatherable_minerals", function(x) .f88_weatherable_v(x, min_pct),
                 max_top_cm)
}

# Calcareous (the 1974 key, pp. 33-53, has no definition): more than 2 %
# CaCO3 equivalent or strong effervescence, the definition of 1988 (p. 35),
# flagged (Q23); window_cm = c(20, 50) for "calcareous at least between 20
# and 50 cm".
calcareous_fao74 <- function(pedon, max_top_cm = NULL, window_cm = NULL) {
  .f74_pr_layers(pedon, "calcareous", function(x) {
    r <- .f88_calcareous_v(x)
    r$flags <- .f88_addf(r$flags, !(r$value %in% FALSE), "numbers_of_1988_q23")
    r
  }, max_top_cm, window_cm)
}


# --- sulfidic materials (p. 31) ------------------------------------------------------------------

# 0.75 % sulfur or more (s_total_pct, else sulfidic_s_pct plus sulfate_s_pct
# as a lower bound, flagged, as 1988), less than three times as much CaCO3
# equivalent as sulfur, and no jarosite mottles (hue 2.5Y or more, chroma 6
# or more: what separates them from the sulfuric horizon). The waterlogged
# state and the pH before drainage are reported, not tested (as 1988).
.f74_sulfidic_v <- function(x) {
  .f88_memo(x, "pr_sulfidic74", function() {
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
    c_ok[use_lb & c_ok %in% FALSE] <- NA
    jar <- .f74_jarosite(x)$value
    v <- .f88_and(s_ok, as.logical(c_ok), !jar)
    flag <- ifelse(is.na(flag), cc$flags, ifelse(is.na(cc$flags), flag, paste(flag, cc$flags, sep = "; ")))
    list(value = .f88_pr_clean(x, v), flags = flag, missing = c("s_total_pct", "caco3_pct"),
         evidence = list(saturated = .f88_saturated(x)))
  })
}

sulfidic_materials_fao74 <- function(pedon, max_top_cm = NULL) {
  .f74_pr_layers(pedon, "sulfidic_materials", .f74_sulfidic_v, max_top_cm)
}


# --- takyric features (p. 31) ----------------------------------------------------------------

# Read on the first mineral layer: a heavy texture (the fine class of the
# upper 30 cm, clay above 35 %, 74 p. 5; the reading of the takyric phase of
# 1988, Hugo 2026-11-09), polygonal cracks (polygonal_cracks_spacing_cm above
# 0) and a platy or massive crust (structure_type of that layer). Cracks or
# crust not recorded, absent; texture not recorded, undecided.
takyric_features_fao74 <- function(pedon) {
  x <- .f74_ctx(pedon)
  id <- "takyric_features"
  if (!x$n) return(.f74_pr_result(x, id, list(value = FALSE, layers = integer(0))))
  s <- .f88ph_surface(x)
  if (is.na(s)) return(.f74_pr_result(x, id, list(value = FALSE, layers = integer(0))))
  fl <- character(0)
  sp <- .f88_num(x, "polygonal_cracks_spacing_cm")[s]
  poly <- !is.na(sp) && sp > 0
  if (is.na(sp)) fl <- c(fl, "polygonal_cracks_not_recorded_absent")
  st <- .f88_chr(x, "structure_type")[s]
  crust <- !is.na(st) && grepl("platy|massive|laminar|maci", st)
  if (is.na(st)) fl <- c(fl, "surface_structure_not_recorded_absent")
  t <- .fao_texture_upper30(x$h, "1974")
  heavy <- if (is.na(t$value)) NA else identical(t$value, "fine")
  v <- .fao_and(poly, crust, heavy)
  .f74_pr_result(x, id, list(value = v, layers = if (!isFALSE(v)) s else integer(0)),
                 flags = c(fl, t$flags), missing = c("clay_pct", "sand_pct", t$missing))
}


# --- thin iron pan (p. 31) ---------------------------------------------------------------------

# A black to dark reddish layer cemented by iron, iron and manganese or an
# iron-organic complex, generally 2 to 10 mm (1 to 40 mm at the extremes),
# in the solum: pan_type naming a thin iron pan or placic pan; else a
# cemented layer (.f88_cemented()) with an s designation (Bsm, Bms, placic)
# of 4 cm or less, flagged. Not recorded: absent (a field observation).
.f74_hz_thin_pan <- function(x) {
  .f88_memo(x, "thin_pan74", function() {
    pt <- .f88_chr(x, "pan_type")
    thk <- x$bot - x$top
    # a thin iron or placic pan as recorded; an "iron pan" of another kind
    # within the limit of 40 mm
    v <- !is.na(pt) & (grepl("thin iron|placic", pt) |
                         (grepl("ironpan|iron pan", pt) & !is.na(thk) & thk <= 4))
    cem <- .f88_cemented(x)$value
    dsg <- !is.na(x$desg) & (grepl("^[A-Z]+[a-z0-9]*(sm|ms)", x$desg) | grepl("placic", tolower(x$desg)))
    alt <- !v & cem & dsg & !is.na(thk) & thk <= 4
    v <- v | alt
    v[x$rock] <- FALSE
    flag <- ifelse(alt, "thin_iron_pan_from_designation", NA_character_)
    list(units = .f88_units(x, .f88_unburied(x, v), function(r) TRUE), value = v, flags = flag)
  })
}

thin_iron_pan_fao74 <- function(pedon, max_top_cm = NULL) {
  .f74_pr_layers(pedon, "thin_iron_pan", function(x) {
    r <- .f74_hz_thin_pan(x)
    none <- all(is.na(.f88_chr(x, "pan_type")))
    fl <- r$flags
    if (none) fl <- .f88_addf(fl, !(r$value %in% TRUE), "pan_type_not_recorded_absent")
    list(value = .f88_pr_clean(x, r$value, bury = TRUE), flags = fl)
  }, max_top_cm)
}


# --- vertic properties (p. 31) ----------------------------------------------------------------

# Cracks 1 cm or more wide (cracks_width_cm) at some period in most years
# within 50 cm of the upper boundary of the B (a layer overlapping the 50 cm
# below the top of the first B layer), reaching the surface or at least the
# upper part of the B: the cracks begin (cracks_top_cm) at or above the base
# of the first B layer, or the cracked layer is itself that layer or above
# it. Not recorded: absent. No B horizon: no vertic properties.
.f74_vertic_v <- function(x) {
  .f88_memo(x, "pr_vertic74", function() {
    n <- x$n
    cw <- .f88_num(x, "cracks_width_cm"); ct <- .f88_num(x, "cracks_top_cm")
    b <- which(!is.na(x$master) & grepl("^B", x$master) & !x$rock & !is.na(x$top))
    v <- rep(FALSE, n)
    flag <- rep(NA_character_, n)
    if (length(b)) {
      b0 <- b[1L]; tb <- x$top[b0]; bb <- x$bot[b0]
      win <- !is.na(x$top) & !is.na(x$bot) & x$bot > tb & x$top < tb + 50
      reach <- (!is.na(ct) & ct <= bb) | seq_len(n) <= b0
      v <- win & !is.na(cw) & cw >= 1 & reach
    }
    flag[is.na(cw)] <- "cracks_not_recorded_absent"
    list(value = .f88_pr_clean(x, v), flags = flag)
  })
}

vertic_properties_fao74 <- function(pedon, max_top_cm = NULL) {
  .f74_pr_layers(pedon, "vertic_properties", .f74_vertic_v, max_top_cm)
}


# --- recent alluvial deposits (p. 32) --------------------------------------------------------

# Fluviatile, marine, lacustrine or colluvial sediments (the origin read as
# for the fluvic properties of 1988, colluvium added; not recorded, the
# criteria decide, flagged) with one or more of: (a) organic matter that
# decreases irregularly with depth (an increase of more than 0.2 points of
# organic carbon from a layer to the next, the reading of 1988) or stays
# above 0.35 % (OC above 0.35 / 1.724) to 125 cm, a thin sand stratum with
# less exempt where a finer layer below it has more; (b) fresh material at
# regular intervals or fine stratification: a stratification_pattern
# recorded within 125 cm, or a recorded flood_frequency other than none;
# (c) sulfidic materials within 125 cm (sulfur measured nowhere and no
# marine, tidal or brackish setting recorded: absent, flagged).
recent_alluvial_deposits_fao74 <- function(pedon) {
  x <- .f74_ctx(pedon)
  id <- "recent_alluvial_deposits"
  if (!x$n) return(.f74_pr_result(x, id, list(value = FALSE, layers = integer(0))))
  flags <- c("om_from_oc_x1.724")
  origin <- .f88_sediment_origin(x)
  pm <- .f88_site(x, "parent_material")
  lo <- .f88_chr(x, "layer_origin")
  coll <- (!is.na(pm) && grepl("colluv|coluvi", .field_term(as.character(pm)))) ||
    any(!is.na(lo) & grepl("colluv|coluvi", lo))
  if (coll) origin <- TRUE
  if (is.na(origin)) flags <- c(flags, "sediment_origin_not_recorded_criteria_decide")
  oc <- .f88_num(x, "oc_pct")
  w <- .fao_window(x$h, 0, 125)
  L <- w$layers
  inc <- vapply(seq_along(L)[-1L], function(k) {
    i <- L[k]; j <- L[k - 1L]
    if (abs(x$top[i] - x$bot[j]) > 0.5) return(NA)
    .f88_cmpr(oc[i] - oc[j], ">", 0.2)
  }, NA)
  ca <- if (any(inc %in% TRUE)) TRUE else if (all(inc %in% FALSE) && w$covered) FALSE else NA
  om <- .fao_om(oc)$value
  tex <- .fao_texture_usda(x$h)
  t1 <- .f88_cmp(om, ">", 0.35)
  ex <- rep(FALSE, x$n)
  for (i in L) {
    if (!(t1[i] %in% FALSE) || !(tex[i] %in% c("S", "LS"))) next
    below <- L[L > i]
    ex[i] <- any(t1[below] %in% TRUE & !(tex[below] %in% c("S", "LS")))
  }
  t1[ex] <- TRUE
  if (any(ex)) flags <- c(flags, "thin_sand_stratum_exempt")
  cb <- .fao_in_window(x$h, t1, 0, 125, "throughout")
  sp <- .f88_chr(x, "stratification_pattern")
  strat <- any(!is.na(sp[L]) & !(sp[L] %in% c("none", "no", "absent")))
  ff <- .f88_site(x, "flood_frequency")
  fresh <- !is.na(ff) && !grepl("^(none|never|no|nunca)", tolower(as.character(ff)))
  cs <- strat || fresh
  sul <- .f74_sulfidic_v(x)$value
  sw <- .fao_within(x$h, sul, 125)
  # sulfur not measured in any layer: sulfidic materials are read as absent,
  # flagged, unless the site or a layer names a marine, tidal or brackish
  # setting (soilKey's reading)
  srec <- any(!is.na(.f88_num(x, "s_total_pct")) | !is.na(.f88_num(x, "sulfidic_s_pct")))
  lf <- .f88_site(x, "landform")
  marine_re <- "marin|tidal|brackish|mangrove|estuar|salt marsh|lagoon|lagun|mangue"
  marine <- any(!is.na(lo) & grepl(marine_re, lo)) ||
    (!is.na(pm) && grepl(marine_re, .field_term(as.character(pm)))) ||
    (!is.na(lf) && grepl(marine_re, .field_term(as.character(lf))))
  if (is.na(sw$value) && !srec && !marine) {
    sw$value <- FALSE
    flags <- c(flags, "sulfur_not_measured_no_marine_setting_absent")
  }
  crit <- .fao_or(ca, cb$value, cs, sw$value)
  passed <- if (isFALSE(origin)) FALSE else crit
  miss <- c(if (is.na(ca) || is.na(cb$value)) "oc_pct", cb$missing, sw$missing,
            if (is.na(origin)) "parent_material")
  .f74_pr_result(x, id, list(value = passed, layers = if (isFALSE(passed)) integer(0) else L),
                 flags = flags, missing = miss,
                 evidence = list(origin = origin, om_irregular = ca, om_above_0.35 = cb$value,
                                 stratified_or_fresh = cs, sulfidic_within_125 = sw$value),
                 max_top_cm = 125)
}


# --- terms of the 1974 key without a definition section -------------------------------------

# Bleached coatings on structural ped surfaces (the Greyzems, 74 p. 48, and
# the exclusions of G, C and H): bleached_ped_coatings; not recorded, ped
# coatings lighter than the matrix by one moist value or more
# (coatings_value_moist) with uncoated silt and sand grains recorded
# (uncoated_grains_pct above 0), flagged (soilKey's reading); else absent.
bleached_coatings_fao74 <- function(pedon, max_top_cm = NULL) {
  .f74_pr_layers(pedon, "bleached_coatings", function(x) {
    b <- .f88_lgl(x, "bleached_ped_coatings")
    cv <- .f88_num(x, "coatings_value_moist"); vm <- .f88_num(x, "munsell_value_moist")
    ug <- .f88_num(x, "uncoated_grains_pct")
    alt <- is.na(b) & !is.na(cv) & !is.na(vm) & cv >= vm + 1 & !is.na(ug) & ug > 0
    v <- ifelse(!is.na(b), b, alt)
    flag <- ifelse(alt, "bleached_coatings_from_coating_colour_and_uncoated_grains",
                   ifelse(is.na(b), "bleached_ped_coatings_not_recorded_absent", NA_character_))
    list(value = .f88_pr_clean(x, v), flags = flag)
  }, max_top_cm)
}

# Heavy clay (the slowly permeable horizon of the Planosols, 74 p. 48): 60 %
# clay or more [soilKey's number, default 14 of the questions], flagged.
heavy_clay_fao74 <- function(pedon, max_top_cm = NULL, min_clay = 60) {
  .f74_pr_layers(pedon, "heavy_clay", function(x) {
    v <- .f88_cmp(.f88_num(x, "clay_pct"), ">=", min_clay)
    list(value = .f88_pr_clean(x, v), missing = "clay_pct",
         flags = ifelse(v %in% TRUE, "heavy_clay_60pct_soilkey_number", NA_character_))
  }, max_top_cm)
}

# More than 6 % sodium in the exchange complex (the Solodic Planosols, 74 p.
# 48): esp_pct, else Na / CEC (NH4OAc) x 100, flagged (.f88_esp()).
exchangeable_sodium_fao74 <- function(pedon, min_pct = 6, max_top_cm = NULL, window_cm = NULL) {
  .f74_pr_layers(pedon, "exchangeable_sodium", function(x) {
    e <- .f88_esp(x)
    list(value = .f88_pr_clean(x, .f88_cmpr(e$value, ">", min_pct)), flags = e$flags,
         missing = c("esp_pct", "na_cmol", "cec_cmol"))
  }, max_top_cm, window_cm)
}

# The CEC (from NH4Cl) of the Acric Ferralsols (74 p. 47): 1.5 me or less per
# 100 g clay (Q12: ecec_cmol for the NH4Cl CEC, flagged).
acric_cec_fao74 <- function(pedon, max_top_cm = NULL) {
  .f74_pr_layers(pedon, "acric_cec", function(x) {
    cc <- .f74_nh4cl(x)
    list(value = .f88_pr_clean(x, .f88_cmpr(cc$value, "<=", 1.5)), flags = cc$flags,
         missing = cc$missing)
  }, max_top_cm)
}
