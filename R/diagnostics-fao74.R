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
  if (is.na(passed) && stand_in && nzchar(info$stand_in[1L])) {
    for (fn in strsplit(info$stand_in[1L], ",", fixed = TRUE)[[1L]]) {
      si <- tryCatch(get(fn, mode = "function")(x$pedon), error = function(e) NULL)
      if (is.null(si) || !inherits(si, "DiagnosticResult") || is.na(si$passed)) next
      passed <- isTRUE(si$passed)
      layers <- if (passed) as.integer(si$layers) else integer(0)
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

# The H horizon itself: organic matter at or above the line and saturated.
.f74_hz_h <- function(x) {
  .f88_memo(x, "h_horizon", function() {
    if (!x$n) return(list(units = list()))
    om <- .fao_om(.f88_num(x, "oc_pct"))$value
    line <- .f74_h_line(.f88_num(x, "clay_pct"))
    sat <- .f74_saturated(x)
    enough <- ifelse(is.na(om), NA, ifelse(om >= 30, TRUE, ifelse(om < 20, FALSE,
                     ifelse(is.na(line), NA, om >= line))))
    v <- .f88_and(enough, sat)
    v[x$rock] <- FALSE
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
# grains, or, in a clayey (35 % clay or more) B of swelling 2:1 clays, the
# evidence of pressure (slickensides recorded) with a fine/total clay ratio a
# third higher than, or more than 8 points of fine clay over, the layer above
# or below. The clauses on where the skins must lie (the lower part under a
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
    pressure <- vapply(seq_len(n), function(i) {
      if (!isTRUE(clay[i] >= 35) || !two[i] || !slick[i]) return(FALSE)
      adj <- c(i - 1L, i + 1L); adj <- adj[adj >= 1L & adj <= n]
      any((rt[i] >= rt[adj] * 4 / 3) %in% TRUE | (fc[i] - fc[adj] > 8) %in% TRUE)
    }, logical(1))
    v <- sk$value | (br %in% TRUE) | pressure
    flag <- sk$flags
    flag[pressure & !(sk$value %in% TRUE)] <- "pressure_faces_of_2to1_clays"
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
      il <- if (any(ill$value[sure])) TRUE else if (any(ill$value[maybe])) NA else FALSE
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
    for (s in core$starts) {
      nx <- .f88_natric_extra(x, s)
      st <- if (natric) .f88_and(s$status, nx$status) else .f88_and(s$status, .f88_not(nx$status))
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
    list(units = units, flags = unique(flags), missing = unique(miss), evidence = list(starts = ev))
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
    c3 <- .f88_or(.f88_cmp(cc$value, ">", 16), .f88_cmp(other, ">", 3), .f88_cmp(mu, ">", 6))
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

# The albic E of 1988 (Appendix 4: unchanged) read as printed in 1974: a
# moist value of 4 or more or a dry value of 5 or more; chroma 3 or less
# where the dry value is 7 or more or the moist value 6 or more; no chroma
# limit at the middle values (74 p. 27, checked on the page image).
albic_e_fao74 <- function(pedon, max_top_cm = 125) {
  x <- .f74_ctx(pedon)
  .f74_result(x, "albic_e", .f88_hz_albic_e(x), max_top_cm)
}
