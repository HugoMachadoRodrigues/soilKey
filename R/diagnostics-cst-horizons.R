# =============================================================================
# v0.9.371 -- the diagnostic horizons of the Chinese Soil Taxonomy
# (Cooperative Research Group on Chinese Soil Taxonomy, ISSAS. Chinese Soil
# Taxonomy. Science Press, Beijing and New York, 2001; the English edition,
# system id cst2001), read on the recorded data, layer by layer.
#
# The book defines 33 diagnostic horizons (pp. 20-43): 11 epipedons, 20
# subsurface horizons and 2 "other" horizons, and gives 20 of its horizons
# and characteristics a weaker grade, the diagnostic evidence, whose upper
# limit is the lower limit of the full horizon (p. 19). Every reader here
# gives each layer a grade: "full", "evidence", "none" or NA (undecided), and
# the profile the best grade a layer reaches. The anthropic epipedons and the
# agric and hydragric horizons are in R/diagnostics-cst-anthropic.R.
#
# Conventions (cn_plan 4.4 and the decisions of 2026-11-04 and 2026-11-05,
# inst/rules/cst2001/readings.yaml):
#   * three values: a measurement not recorded leaves its item undecided, a
#     field observation not recorded is absent;
#   * "a horizon N cm thick" is a run of contiguous layers that share the
#     property; separate layers add up only where the book says "total";
#   * contents in g/kg as printed in thresholds.yaml, divided by 10 once;
#   * a WRB or USDA diagnostic of soilKey stands in for a CST descriptor that
#     is not recorded, flagged, with a lower grade in the report: it is asked
#     only where the CST reading is undecided or false for want of a field
#     descriptor, and it can only make a layer pass (default 11);
#   * FAO designations are hints for the identity of a horizon (the A, the
#     cultivated layer, the C, R) and never pass a property test.
#
# Each reader .cst_hz_<name>(x) returns, for the layers of the profile in
# depth order, `full` (TRUE / FALSE / NA) and `evid` (at least the evidence
# grade), with `kind`, `flags`, `notes`, `stand_in` and the attributes that
# would decide it. <name>_cst(pedon, grade) wraps it into a DiagnosticResult;
# .cst_identify(pedon) gives the horizon panel. Until the registry entry
# cst2001 exists (a later part), identify_horizons() does not reach
# .cst_identify(), which is called directly.
# =============================================================================

.CST_REF_HZ <- "Chinese Soil Taxonomy (2001), 3.2 Diagnostic horizons, pp. 20-43"


# --- the profile ---------------------------------------------------------------

# The context of a profile, built once: the horizons in depth order, their
# positions in pedon$horizons (idx), depths, kinds, the soil and mineral
# surfaces, and a memo of the readers already run. The last contexts are
# kept (.cst_cache$ctx) and reused while the same pedon holds the same
# horizons and site (a hash of their content, since a data.table may change
# in place), so the readers of the key and the panel share one (D13).
.cst_ctx <- function(pedon) {
  cache <- .cst_cache$ctx %||% list()
  key <- rlang::hash(list(pedon$horizons, pedon$site, isTRUE(.cst_cache$argic_firm)))
  for (e in cache) {
    if (identical(e$pedon, pedon) && identical(e$key, key)) return(e$x)
  }
  x <- .cst_ctx_build(pedon)
  .cst_cache$ctx <- c(list(list(pedon = pedon, key = key, x = x)), utils::head(cache, 7L))
  x
}

.cst_ctx_build <- function(pedon) {
  h <- pedon$horizons
  n <- if (is.null(h)) 0L else nrow(h)
  ord <- if (n) order(h$top_cm, na.last = TRUE) else integer(0)
  hh <- if (n) h[ord, , drop = FALSE] else h
  x <- new.env(parent = emptyenv())
  x$pedon <- pedon
  x$h <- hh
  x$n <- n
  x$idx <- ord
  x$top <- if (n) as.numeric(hh$top_cm) else numeric(0)
  x$bot <- if (n) as.numeric(hh$bottom_cm) else numeric(0)
  x$thk <- x$bot - x$top
  x$kind <- if (n) .cst_kinds(pedon, hh) else character(0)
  x$surf <- if (n) .cst_surface_cm(pedon, "surface") else NA_real_
  x$msurf <- if (n) .cst_surface_cm(pedon, "mineral") else NA_real_
  x$first_min <- if (n && !is.na(x$msurf)) which(!is.na(x$top) & x$top >= x$msurf)[1L] else NA_integer_
  x$memo <- new.env(parent = emptyenv())
  # v0.9.377: the run of the alternative, where an argic increase without
  # fine clay fails (.cst_alternative())
  x$argic_firm <- isTRUE(.cst_cache$argic_firm)
  x
}

# Columns of the ordered horizons.
.cst_v <- function(x, name) .cst_num(x$h, name)
.cst_t <- function(x, name) {
  v <- tolower(trimws(as.character(.cst_col(x$h, name))))
  v[!is.na(v) & (!nzchar(v) | v %in% c("na", "nan", "null"))] <- NA_character_
  v
}
.cst_l <- function(x, name) {
  v <- .cst_col(x$h, name)
  if (is.logical(v)) return(v)
  if (is.numeric(v)) return(ifelse(is.na(v), NA, v != 0))
  v <- tolower(trimws(as.character(v)))
  ifelse(v %in% c("true", "t", "yes", "y", "1", "present", "positive"), TRUE,
         ifelse(v %in% c("false", "f", "no", "n", "0", "absent", "none", "negative"), FALSE, NA))
}

# The value of a column one layer above or below each layer (NA at the ends).
.cst_shift <- function(v, by) {
  n <- length(v)
  if (!n) return(v)
  if (by > 0) c(v[-seq_len(by)], rep(NA, by)) else c(rep(NA, -by), v[seq_len(n + by)])
}

# Munsell colours of each layer: moist value and chroma, the dry ones, and the
# moist chroma with the dry one where no moist one is recorded (flagged, as
# Q21 reads the hue).
.cst_colours <- function(x) {
  vm <- .cst_v(x, "munsell_value_moist"); vd <- .cst_v(x, "munsell_value_dry")
  cm <- .cst_v(x, "munsell_chroma_moist"); cd <- .cst_v(x, "munsell_chroma_dry")
  hue <- .cst_hue(x$h)
  hue_txt <- toupper(gsub("[[:space:]]", "", as.character(.cst_col(x$h, "munsell_hue_moist"))))
  hue_dry <- toupper(gsub("[[:space:]]", "", as.character(.cst_col(x$h, "munsell_hue_dry"))))
  hue_txt <- ifelse(is.na(hue_txt) | !nzchar(hue_txt), hue_dry, hue_txt)
  list(vm = vm, vd = vd, cm = cm, cd = cd, c_any = ifelse(is.na(cm), cd, cm),
       c_flag = ifelse(is.na(cm) & !is.na(cd), "chroma_dry_for_moist", NA_character_),
       rank = hue$rank, hue_flag = hue$flag, hue = hue_txt,
       neutral = !is.na(hue_txt) & grepl("^N", hue_txt))
}

# Hue tests with the neutral hue N (Hugo, 2026-11-06): N has no hue, so it
# is never "5YR or redder" nor within a range of hues (7.5YR-10YR), and it
# passes "7.5YR or yellower, even GY, B or BG" and a grey range that reaches
# the greens (10YR-7.5Y, the hydragric grey subhorizon).
.cst_hue_le <- function(col, r) ifelse(col$neutral, FALSE, col$rank <= r)
.cst_hue_ge <- function(col, r) ifelse(col$neutral, TRUE, col$rank >= r)
.cst_hue_in <- function(col, a, b, neutral = FALSE) ifelse(col$neutral, neutral, col$rank >= a & col$rank <= b)

# The cultivated layer and its plough pan (pp. 26-27), per layer: TRUE an Ap
# (any FAO A with the suffix p), or in the older Chinese paddy notation
# (A-P-W-G) the P (the plough pan) and the A directly above a P (or, with no
# P, above a W); FALSE a layer designated otherwise (B, C, E, O, R, W, G ...),
# and an A in a profile written with FAO suffixes; NA where cultivation
# cannot be identified (no designation, or a plain A in a profile with no
# suffixes) (D1, Hugo, 2026-11-06).
.cst_cultivated <- function(x) {
  d <- .desg_master(as.character(.cst_col(x$h, "designation")), site = x$pedon$site)
  n <- length(d)
  out <- rep(NA, n)
  if (!n) return(logical(0))
  has <- !is.na(d) & nzchar(d)
  ap <- has & grepl("^A[a-z]*p", d)
  pan <- has & grepl("^P[0-9]*$", d)
  w <- has & grepl("^W", d)
  plain_a <- has & grepl("^A[0-9]*$", d)
  old <- any(pan | w)
  suff <- any(has & grepl("^[A-Z]+[a-z]", d))
  out[has] <- FALSE
  out[ap | pan] <- TRUE
  if (old) {
    k <- if (any(pan)) which(pan)[1L] else which(w)[1L]
    j <- k - 1L
    while (j >= 1L && plain_a[j]) { out[j] <- TRUE; j <- j - 1L }
  } else if (!suff) {
    out[plain_a] <- NA
  }
  out
}

# Exchangeable sodium percentage: esp_pct, else na_cmol / cec_cmol x 100.
.cst_esp <- function(x) {
  e <- .cst_v(x, "esp_pct")
  na <- .cst_v(x, "na_cmol"); cec <- .cst_v(x, "cec_cmol")
  ifelse(is.na(e) & !is.na(na) & !is.na(cec) & cec > 0, na / cec * 100, e)
}

# The sodic property (p. 63) of each layer, as the argic horizon reads it
# (its full grade; the property itself is read in a later part): ESP 30 or
# more with exchangeable Na 2 cmol(+)/kg or more, or (Na + Mg) half or more
# of the CEC.
.cst_sodic_layer <- function(x) {
  esp <- .cst_esp(x)
  na <- .cst_v(x, "na_cmol"); mg <- .cst_v(x, "mg_cmol"); cec <- .cst_v(x, "cec_cmol")
  t <- .cst_thr("horizons", "argic", "sodic")
  nm <- ifelse(!is.na(cec) & cec > 0, (na + mg) / cec * 100, NA_real_)
  # sodium (or magnesium) not measured: absent unless the layer is alkaline
  # (pH 8.5 or more), where it stays undecided; the same for the ESP and
  # for the Na + Mg share (Hugo, 2026-11-06)
  ph <- .cst_v(x, "ph_h2o")
  not_alk <- !(ph >= t$alkaline_ph_min & !is.na(ph))
  a <- esp >= t$esp_min & na >= t$na_min_cmol
  a[is.na(a) & is.na(esp) & is.na(na) & not_alk] <- FALSE
  b <- nm >= t$na_mg_share_min
  b[is.na(b) & not_alk] <- FALSE
  a | b
}

# Cemented layers: a cementation class moderately cemented or harder, or a
# dry rupture resistance very hard or harder. Not recorded: not cemented.
.cst_cemented <- function(x, strong = FALSE) {
  cem <- .cst_t(x, "cementation_class")
  rup <- .cst_t(x, "rupture_resistance")
  lev <- if (strong) c("strongly", "indurated") else c("moderately", "strongly", "indurated")
  out <- (!is.na(cem) & cem %in% lev) | (!is.na(rup) & rup %in% c("very hard", "extremely hard"))
  list(value = out, recorded = !is.na(cem) | !is.na(rup))
}

# Structure recorded: TRUE a pedological structure, FALSE massive or single
# grain or structureless, NA nothing recorded.
.cst_structured <- function(x) {
  ty <- .cst_t(x, "structure_type"); gr <- .cst_t(x, "structure_grade")
  none <- (!is.na(gr) & gr == "structureless") |
    (!is.na(ty) & grepl("massive|single", ty))
  some <- (!is.na(gr) & gr %in% c("weak", "moderate", "strong")) |
    (!is.na(ty) & grepl("granular|blocky|prism|column|platy|crumb|angular|wedge|lentic", ty))
  ifelse(some & !none, TRUE, ifelse(none, FALSE, NA))
}

# Clay films thicker than 0.5 mm on 5 % or more of the faces (the argic
# horizon, item 2a, p. 35): clay_films_thickness_mm, else distinct or
# prominent films read as thicker, flagged (C13, Q20); the share by
# clay_films_amount (common or many; few is under 5 %). Not recorded:
# absent.
.cst_clay_films <- function(x, min_mm = 0.5, strict = TRUE) {
  mm <- .cst_v(x, "clay_films_thickness_mm")
  st <- .cst_t(x, "clay_films_strength")
  am <- .cst_t(x, "clay_films_amount")
  thick <- if (strict) mm > min_mm else mm >= min_mm
  by_strength <- is.na(mm) & !is.na(st)
  thick <- ifelse(is.na(mm), ifelse(is.na(st), FALSE, st %in% c("distinct", "prominent")), thick)
  share <- ifelse(is.na(am), NA, am %in% c("common", "many", "abundant", "very many"))
  share <- ifelse(is.na(share) & !is.na(mm) & mm > 0, NA, share)
  share[is.na(share) & is.na(mm) & is.na(st)] <- FALSE
  list(value = thick & share, thick = thick,
       flag = ifelse(by_strength, "clay_films_strength_for_thickness", NA_character_),
       recorded = !is.na(mm) | !is.na(st) | !is.na(am))
}


# Layers whose designation says they are no B horizon (an O, A, E, C or R):
# the identity of the argic, ferralic, LAC-ferric and spodic horizons, which
# the book calls subsurface (illuvial or B) horizons. A layer without a
# designation may be any (Hugo, 2026-11-06).
.CST_NOT_B <- c("O", "A", "E", "C", "R")


# --- runs, grades, results -----------------------------------------------------------

# The run of contiguous layers that holds each layer, by a per-layer property
# p: `low` the thickness of the run of layers where p is TRUE, `high` the run
# where it is TRUE or NA (0 where p is FALSE); `lo_rows` / `hi_rows` the
# layers of each run. With `start`, only the run that begins at that layer
# counts (an epipedon).
.cst_runs <- function(x, p, start = NULL) {
  n <- x$n
  low <- high <- rep(0, n)
  lo_id <- hi_id <- rep(NA_integer_, n)
  if (!n) return(list(low = low, high = high, lo_id = lo_id, hi_id = hi_id))
  p <- as.logical(p)
  if (!is.null(start)) {
    if (is.na(start)) p[] <- FALSE
    else {
      if (start > 1L) p[seq_len(start - 1L)] <- FALSE
      # only the run from the start
      k <- start
      while (k <= n && !isFALSE(p[k])) k <- k + 1L
      if (k <= n) p[k:n] <- FALSE
    }
  }
  sumthk <- function(rows) { s <- sum(x$thk[rows]); if (is.na(s)) NA_real_ else s }
  mark <- function(ok) {
    th <- rep(0, n); id <- rep(NA_integer_, n); i <- 1L; b <- 0L
    while (i <= n) {
      if (ok[i]) {
        j <- i
        while (j < n && ok[j + 1L]) j <- j + 1L
        b <- b + 1L
        th[i:j] <- sumthk(i:j); id[i:j] <- b
        i <- j + 1L
      } else i <- i + 1L
    }
    list(th = th, id = id)
  }
  a <- mark(p %in% TRUE); b <- mark(!(p %in% FALSE))
  list(low = a$th, high = b$th, lo_id = a$id, hi_id = b$id)
}

# A thickness range [low, high] against [min, max]: TRUE when the whole range
# lies within, FALSE when none of it does, NA otherwise.
.cst_thk_ok <- function(low, high, min, max = Inf) {
  t <- low >= min & high <= max
  f <- high < min | low > max
  ifelse(!is.na(t) & t & low > 0, TRUE, ifelse(!is.na(f) & f, FALSE, NA))
}

# Per-layer grade from a property and a thickness window.
.cst_run_grade <- function(x, p, min, max = Inf, start = NULL) {
  r <- .cst_runs(x, p, start)
  g <- .cst_thk_ok(r$low, r$high, min, max)
  g[r$high == 0] <- FALSE
  g
}

# Sum over the run of each layer of a per-layer quantity (thickness x
# content): the low end over the layers that surely belong, the high end
# over those that may.
.cst_run_sum <- function(x, r, q) {
  lo <- hi <- rep(0, x$n)
  for (b in unique(stats::na.omit(r$lo_id))) { k <- which(r$lo_id == b); lo[k] <- sum(q[k]) }
  for (b in unique(stats::na.omit(r$hi_id))) { k <- which(r$hi_id == b); hi[k] <- sum(q[k]) }
  list(low = lo, high = hi)
}

# Monotone limit on a range: TRUE when the low end meets it, FALSE when the
# high end does not, NA otherwise.
.cst_range_min <- function(low, high, min) {
  ifelse(!is.na(low) & low >= min & low > 0, TRUE, ifelse(!is.na(high) & high < min, FALSE, NA))
}

# The reader's answer for the layers: full, evid (at least evidence), with
# what it read.
.cst_hz_ans <- function(x, full, evid = full, kind = NULL, flags = character(0),
                        notes = character(0), missing = character(0), stand_in = NULL,
                        lflags = NULL, sub = list()) {
  full <- as.logical(full); evid <- as.logical(evid)
  evid[full %in% TRUE] <- TRUE
  evid[is.na(full) & evid %in% FALSE] <- NA
  list(full = full, evid = evid,
       kind = kind %||% rep(NA_character_, x$n),
       flags = unique(as.character(stats::na.omit(flags))),
       notes = unique(as.character(stats::na.omit(notes))),
       missing = unique(as.character(missing)),
       stand_in = stand_in %||% rep(NA_character_, x$n),
       lflags = lflags %||% rep(list(character(0)), x$n),
       sub = sub)
}

# The attributes not recorded in the layers left undecided.
.cst_missing_in <- function(x, rows, attrs) {
  if (!length(rows)) return(character(0))
  attrs[vapply(attrs, function(a) anyNA(.cst_col(x$h, a)[rows]), NA)]
}

# A WRB or USDA diagnostic of soilKey standing in where the CST reading is
# undecided, or false only for want of a field descriptor (`eligible`, per
# layer): the layers it passes become full, flagged (default 11). It never
# makes a layer fail.
.cst_stand_in <- function(x, ans, fns, eligible) {
  eligible <- as.logical(eligible) & !(ans$full %in% TRUE)
  if (!any(eligible %in% TRUE)) return(ans)
  for (f in fns) {
    fn <- get0(f$fn, envir = asNamespace("soilKey"), mode = "function", inherits = FALSE)
    if (is.null(fn)) next
    res <- tryCatch(do.call(fn, c(list(x$pedon), f$args %||% list())), error = function(e) NULL)
    if (is.null(res) || !isTRUE(res$passed)) next
    pos <- match(res$layers, x$idx)
    pos <- pos[!is.na(pos)]
    if (!length(pos)) pos <- which(eligible %in% TRUE)
    pos <- pos[eligible[pos] %in% TRUE]
    if (!length(pos)) next
    ans$full[pos] <- TRUE
    ans$evid[pos] <- TRUE
    ans$stand_in[pos] <- f$fn
    ans$flags <- unique(c(ans$flags, sprintf("stand_in:%s", f$fn)))
    eligible[pos] <- FALSE
    if (!any(eligible %in% TRUE)) break
  }
  ans
}

# Run a reader once per context.
.cst_hz <- function(x, name) {
  if (!is.null(x$memo[[name]])) return(x$memo[[name]])
  fn <- get(paste0(".cst_hz_", name), envir = asNamespace("soilKey"), mode = "function")
  if (!x$n) {
    r <- .cst_hz_ans(x, logical(0))
  } else {
    r <- fn(x)
  }
  x$memo[[name]] <- r
  r
}

# The grade of each layer from its full and evid values.
.cst_layer_grade <- function(full, evid) {
  ifelse(full %in% TRUE, "full",
    ifelse(full %in% FALSE & evid %in% TRUE, "evidence",
      ifelse(full %in% FALSE & evid %in% FALSE, "none", NA_character_)))
}

# The DiagnosticResult of a horizon for the grade asked: "full" (default),
# "evidence" (the weaker grade only, the full horizon excluded) or
# "at_least_evidence". `top_max`: only the layers whose top lies within that
# depth of the soil surface ("surface") or of the mineral soil surface
# ("mineral") count.
.cst_hz_result <- function(pedon, name, grade = c("full", "evidence", "at_least_evidence"),
                           top_max = NULL, datum = c("surface", "mineral"), x = NULL, r = NULL) {
  grade <- match.arg(grade)
  datum <- match.arg(datum)
  info <- .cst_horizon_table()
  row <- info[info$id == name, , drop = FALSE]
  if (!nrow(row) && name == "phos_agric_horizon")
    row <- data.frame(id = name, name = "Phos-agric horizon (the agric horizon by its Olsen P)",
                      page = "33-34", evidence = TRUE, stringsAsFactors = FALSE)
  # the layer characteristics (0.9.372) are read the same way
  if (!nrow(row)) {
    ch <- .cst_characteristic_table()
    row <- ch[ch$id == name, , drop = FALSE]
  }
  x <- x %||% .cst_ctx(pedon)
  r <- r %||% .cst_hz(x, name)
  full <- r$full; evid <- r$evid
  notes <- r$notes
  if (!is.null(top_max) && x$n) {
    s <- if (datum == "surface") x$surf else x$msurf
    if (is.na(s)) {
      full[!(full %in% FALSE)] <- NA; evid[!(evid %in% FALSE)] <- NA
    } else {
      out <- is.na(x$top) | x$top - s > top_max
      full[out] <- FALSE; evid[out] <- FALSE
    }
    notes <- c(notes, sprintf("upper boundary within %s cm of the %s", format(top_max),
                              if (datum == "surface") "soil surface" else "mineral soil surface"))
  }
  Fp <- .cst_or(full); Ep <- .cst_or(evid)
  if (!length(full)) { Fp <- FALSE; Ep <- FALSE }
  g <- if (isTRUE(Fp)) "full" else if (isFALSE(Fp) && isTRUE(Ep)) "evidence"
       else if (isFALSE(Fp) && isFALSE(Ep)) "none" else NA_character_
  passed <- switch(grade,
    full = Fp,
    at_least_evidence = Ep,
    evidence = if (isTRUE(Fp)) FALSE else if (isFALSE(Ep)) FALSE
               else if (isFALSE(Fp) && isTRUE(Ep)) TRUE else NA)
  pos <- switch(grade,
    full = which(full %in% TRUE),
    at_least_evidence = which(evid %in% TRUE),
    evidence = which(evid %in% TRUE & full %in% FALSE))
  lg <- .cst_layer_grade(full, evid)
  names(lg) <- as.character(x$idx)
  kinds <- unique(stats::na.omit(r$kind[pos]))
  si <- unique(stats::na.omit(r$stand_in[pos]))
  ev <- c(r$sub, list(
    grade = g,
    grade_min = if (is.na(g) && isTRUE(Ep)) "evidence" else NA_character_,
    grade_asked = grade,
    kind = if (length(kinds)) kinds else NA_character_,
    stand_in = si,
    lower_grade = length(si) > 0L,
    flags = r$flags,
    layer_grade = lg,
    has_evidence_grade = isTRUE(row$evidence[1L]),
    page = row$page[1L]))
  # v0.9.377: a pass resting on provisional layers only (r$prov, the argic
  # increase without fine clay; r$prov_evid for the evidence grades) is
  # provisional
  pv <- if (grade != "full" && !is.null(r$prov_evid)) r$prov_evid else r$prov
  if (!is.null(pv) && isTRUE(passed) && length(pos) && all(pv[pos] %in% TRUE)) {
    ev$provisional <- TRUE
    if (!is.null(r$prov_flag)) ev$provisional_fine_clay <- TRUE
    ev$flags <- unique(c(ev$flags, r$prov_flag, "provisional"))
  }
  DiagnosticResult$new(
    name = paste0(name, "_cst"),
    passed = passed,
    layers = x$idx[pos],
    evidence = ev,
    missing = if (is.na(passed)) r$missing else character(0),
    reference = sprintf("Chinese Soil Taxonomy (2001), %s, p. %s", row$name[1L], row$page[1L]),
    notes = if (length(c(notes, r$flags))) paste(unique(c(notes, r$flags)), collapse = "; ") else NA_character_)
}

# The grade of a DiagnosticResult of a CST horizon ("full", "evidence",
# "none" or NA).
.cst_grade <- function(res) {
  g <- res$evidence$grade
  if (is.null(g)) NA_character_ else g
}


# --- the table of the 33 horizons -------------------------------------------------------

# id (the reader .cst_hz_<id>), the book's name, its group, page, whether
# the book gives it an evidence grade, and the function of the key.
.cst_horizon_table <- function() {
  if (!is.null(.cst_cache$hz_table)) return(.cst_cache$hz_table)
  t <- data.frame(stringsAsFactors = FALSE,
    id = c("histic_epipedon", "mattic_epipedon", "mollic_epipedon", "umbric_epipedon",
           "ochric_epipedon", "siltigic_epipedon", "cumulic_epipedon", "fimic_epipedon",
           "anthrostagnic_epipedon", "aridic_epipedon", "salic_crust",
           "albic_horizon", "glossic_horizon", "cambic_horizon", "ferralic_horizon",
           "lac_ferric_horizon", "plinthic_horizon", "spodic_horizon", "agric_horizon",
           "hydragric_horizon", "argic_horizon", "claypan", "alkalic_horizon",
           "hypersalic_horizon", "salipan", "gypsic_horizon", "hypergypsic_horizon",
           "calcic_horizon", "hypercalcic_horizon", "calcipan", "phosphipan",
           "salic_horizon", "sulfuric_horizon"),
    name = c("Histic epipedon", "Mattic epipedon", "Mollic epipedon", "Umbric epipedon",
             "Ochric epipedon", "Siltigic epipedon", "Cumulic epipedon", "Fimic epipedon",
             "Anthrostagnic epipedon", "Aridic epipedon", "Salic crust",
             "Albic horizon", "Glossic horizon", "Cambic horizon", "Ferralic horizon",
             "LAC-ferric horizon", "Plinthic horizon", "Spodic horizon", "Agric horizon",
             "Hydragric horizon", "Argic horizon", "Clay-pan", "Alkalic horizon",
             "Hypersalic horizon", "Salipan", "Gypsic horizon", "Hypergypsic horizon",
             "Calcic horizon", "Hypercalcic horizon", "Calcipan", "Phosphipan",
             "Salic horizon", "Sulfuric horizon"),
    group = c(rep("surface", 11), rep("subsurface", 20), rep("other", 2)),
    page = c("21-22", "22-23", "23-24", "24", "24", "24-25", "25", "25-26", "26-27", "27-28", "28",
             "28-29", "29-30", "29-31", "31", "31", "31-32", "32-33", "33-34", "34-35", "35-38",
             "38", "38-39", "39", "39", "39", "40", "40-41", "41", "41", "41-42", "42", "43"),
    evidence = c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE,
                 FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE,
                 FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE))
  t$fun <- paste0(t$id, "_cst")
  .cst_cache$hz_table <- t
  t
}


# --- organic epipedons (pp. 21-23) --------------------------------------------------------

# Organic carbon of organic soil materials (pp. 21-22): base + slope x clay %,
# the cap at 60 % clay or more; without clay, decided only outside the range.
.cst_oc_organic <- function(oc, clay, base, slope, cap) {
  lim <- ifelse(!is.na(clay) & clay >= 60, cap, base + slope * clay)
  ifelse(!is.na(lim), oc >= lim, ifelse(oc >= cap, TRUE, ifelse(oc < base, FALSE, NA)))
}

# The days of saturation of the profile: the most recorded in the upper 50
# cm; site$artificially_drained TRUE reads as saturated (the peaty kind,
# "unless artificially drained").
.cst_sat_days <- function(x) {
  s <- .cst_v(x, "water_saturation_days")
  rows <- which(!is.na(x$top) & !is.na(x$surf) & x$top < x$surf + 50)
  v <- s[rows]
  if (all(is.na(v))) NA_real_ else max(v, na.rm = TRUE)
}

.cst_hz_histic_epipedon <- function(x) {
  t <- .cst_thr("horizons", "histic")
  oc <- .cst_v(x, "oc_pct"); clay <- .cst_v(x, "clay_pct")
  peaty_oc <- .cst_oc_organic(oc, clay, t$peaty_oc_base_pct, t$peaty_oc_clay_slope, t$peaty_oc_cap_pct)
  flags <- character(0); notes <- character(0)
  cult <- .cst_cultivated(x)
  if (x$n && isTRUE(cult[1L])) {
    m <- .cst_mixed_surface(x$pedon, "oc_pct", depth = .cst_thr("mixed_surface", "histic_cultivated_cm"))
    mc <- .cst_mixed_surface(x$pedon, "clay_pct", depth = .cst_thr("mixed_surface", "histic_cultivated_cm"))
    ok_b <- .cst_oc_organic(m$value, mc$value, t$ap_oc_base_pct, t$ap_oc_clay_slope, t$ap_oc_cap_pct)
    rows <- which(x$top < x$surf + .cst_thr("mixed_surface", "histic_cultivated_cm"))
    peaty_oc[rows] <- ok_b
    flags <- c(flags, "mixed_surface_25cm")
  }
  litter_oc <- oc >= t$littery_oc_min_pct
  sat <- .cst_sat_days(x)
  drained <- .cst_site_lgl(x$pedon$site, "artificially_drained")
  peaty_sat <- if (isTRUE(drained)) TRUE else sat >= t$saturated_days_min
  lit_sat <- if (isTRUE(drained)) FALSE else sat < t$saturated_days_min
  # the thickness of the peaty kind: 60 cm with sphagnum or a very low
  # density, 40 cm otherwise
  sph <- .cst_v(x, "sphagnum_fibres_pct"); bd <- .cst_v(x, "bulk_density_g_cm3")
  wide <- .cst_or(sph[1L] >= t$sphagnum_min_pct, bd[1L] < t$bd_low_max)
  rp <- .cst_runs(x, peaty_oc, start = 1L)
  pmax_lo <- if (isTRUE(wide)) t$peaty_thick_max_sphagnum_cm else t$peaty_thick_max_cm
  pmax_hi <- if (isFALSE(wide)) t$peaty_thick_max_cm else t$peaty_thick_max_sphagnum_cm
  p_lo <- .cst_thk_ok(rp$low, rp$high, t$thick_min_cm, pmax_lo)
  p_hi <- .cst_thk_ok(rp$low, rp$high, t$thick_min_cm, pmax_hi)
  p_full <- ifelse(p_lo %in% TRUE, TRUE, ifelse(p_hi %in% FALSE, FALSE, NA))
  p_full[rp$high == 0] <- FALSE
  # (c): 40 cm or more in total within the upper 50 cm, with mineral layers
  # over or among the organic ones
  lim <- x$surf + t$total_window_cm
  part <- pmax(0, pmin(x$bot, lim) - pmax(x$top, x$surf))
  tot_lo <- sum(part[peaty_oc %in% TRUE]); tot_hi <- sum(part[!(peaty_oc %in% FALSE)])
  tot <- .cst_range_min(tot_lo, tot_hi, t$total_min_cm)
  in50 <- !is.na(x$top) & x$top < lim & !(peaty_oc %in% FALSE)
  org50 <- which(in50)
  mineral_among <- length(org50) && any(peaty_oc[seq_len(max(org50))] %in% FALSE & x$top[seq_len(max(org50))] < lim)
  if (!mineral_among) tot <- FALSE
  p_full <- ifelse(in50 & tot %in% TRUE, TRUE, ifelse(in50 & is.na(tot) & !(p_full %in% TRUE), NA, p_full))
  over <- !is.na(rp$low) & rp$low > t$peaty_thick_max_sphagnum_cm
  if (any(over & peaty_oc %in% TRUE)) notes <- c(notes, "the organic layers are thicker than a histic epipedon allows (an organic soil)")
  l_full <- .cst_run_grade(x, litter_oc, t$littery_thick_min_cm, start = 1L)
  # either kind
  if (is.na(sat) && !isTRUE(drained)) {
    full <- ifelse(p_full %in% TRUE & l_full %in% TRUE, TRUE,
                   ifelse(p_full %in% FALSE & l_full %in% FALSE, FALSE, NA))
  } else {
    full <- (peaty_sat & p_full) | (lit_sat & l_full)
  }
  # evidence: all but the thickness, 5 cm (3 cm in an arid region)
  arid <- .cst_arid_region(x$pedon)$value
  ev_min <- if (isTRUE(arid)) t$evidence_min_arid_cm else t$evidence_min_cm
  ev_lo <- if (isFALSE(arid)) t$evidence_min_cm else t$evidence_min_arid_cm
  any_oc <- if (is.na(sat) && !isTRUE(drained)) peaty_oc | litter_oc
            else (peaty_sat & peaty_oc) | (lit_sat & litter_oc)
  re <- .cst_runs(x, any_oc, start = 1L)
  ev_sure <- .cst_thk_ok(re$low, re$high, ev_min)
  ev_poss <- .cst_thk_ok(re$low, re$high, ev_lo)
  evid <- ifelse(ev_sure %in% TRUE, TRUE, ifelse(ev_poss %in% FALSE, FALSE, NA))
  evid[re$high == 0] <- FALSE
  kind <- ifelse(full %in% TRUE | evid %in% TRUE,
                 ifelse(isTRUE(peaty_sat), "peaty", ifelse(isTRUE(lit_sat), "littery", NA_character_)),
                 NA_character_)
  fib <- .cst_v(x, "fiber_content_rubbed_pct")
  sub <- ifelse(kind %in% "littery", "folic",
           ifelse(fib >= 75, "fibric", ifelse(fib >= 100 / 6, "hemic", ifelse(fib < 100 / 6, "sapric", NA_character_))))
  kind <- ifelse(!is.na(kind) & !is.na(sub), paste0(kind, " (", sub, ")"), kind)
  rows <- which(!(full %in% FALSE) | !(evid %in% FALSE))
  ans <- .cst_hz_ans(x, full, evid, kind = kind, flags = flags, notes = notes,
                     missing = c(.cst_missing_in(x, rows, c("oc_pct", "clay_pct", "top_cm", "bottom_cm")),
                                 if (is.na(sat)) "water_saturation_days",
                                 if (is.na(wide)) c("sphagnum_fibres_pct", "bulk_density_g_cm3"),
                                 if (is.na(arid)) "aridity_index"))
  .cst_stand_in(x, ans, list(list(fn = "histic_epipedon_usda"), list(fn = "folic")),
                eligible = is.na(full) & seq_len(x$n) == 1L)
}

.cst_hz_mattic_epipedon <- function(x) {
  t <- .cst_thr("horizons", "mattic")
  roots <- .cst_v(x, "root_mat_pct"); mat <- .cst_l(x, "root_mat")
  col <- .cst_colours(x)
  hue_ok <- .cst_hue_in(col, t$hue_rank_min, t$hue_rank_max)
  col_ok <- hue_ok & col$vm < t$value_moist_max & col$vd < t$value_dry_max & col$cm < t$chroma_moist_max
  cn <- .cst_v(x, "oc_pct") / .cst_v(x, "n_total_pct")
  cn_ok <- cn >= t$cn_min & cn <= t$cn_max
  sat <- .cst_sat_days(x)
  sat_ok <- sat < .cst_thr("horizons", "histic", "saturated_days_min")
  bd <- .cst_v(x, "bulk_density_g_cm3")
  bd_ok <- bd >= t$bd_min & bd <= t$bd_max
  reg <- temperature_regime_cst(x$pedon, "cryic", or_colder = TRUE)$passed
  # the mat: recorded; a recorded share of mat roots stands for it
  # (Hugo, 2026-11-06)
  mat_ok <- ifelse(mat %in% TRUE, TRUE, ifelse(mat %in% FALSE, FALSE,
                   ifelse(is.na(roots), FALSE, roots >= t$roots_min_pct)))
  full_p <- mat_ok & roots >= t$roots_min_pct & col_ok & cn_ok & sat_ok & bd_ok & reg
  full <- .cst_run_grade(x, full_p, t$thick_min_cm, start = 1L)
  ev_p <- ifelse(is.na(roots), ifelse(mat %in% TRUE, NA, FALSE), roots >= t$evidence_roots_min_pct)
  evid <- .cst_run_grade(x, ev_p, t$evidence_thick_min_cm, start = 1L)
  rows <- which(is.na(full) | is.na(evid))
  .cst_hz_ans(x, full, evid,
              notes = "evidence read on the thickness and the share of mat roots (Hugo, 2026-11-06)",
              flags = col$hue_flag[seq_len(min(1L, x$n))],
              missing = c(.cst_missing_in(x, rows, c("root_mat_pct", "munsell_value_moist", "munsell_value_dry",
                                                     "munsell_chroma_moist", "oc_pct", "n_total_pct", "bulk_density_g_cm3")),
                          if (is.na(sat)) "water_saturation_days", if (is.na(reg)) "soil_temperature_regime"))
}


# --- humic epipedons (pp. 23-24) -----------------------------------------------------------

# The colour, carbon, base saturation and structure of the layers from the
# mineral soil surface, a cultivated surface read as the mixed upper 18 cm
# (p. 21), and the thickness the solum asks (p. 23).
.cst_humic_layers <- function(x) {
  if (!is.null(x$memo$humic_layers)) return(x$memo$humic_layers)
  t <- .cst_thr("horizons", "mollic")
  col <- .cst_colours(x)
  vm <- col$vm; vd <- col$vd; cm <- col$cm
  oc <- .cst_v(x, "oc_pct")
  flags <- character(0)
  s <- x$first_min
  cult <- .cst_cultivated(x)
  if (!is.na(s) && isTRUE(cult[s])) {
    d <- .cst_thr("mixed_surface", "depth_cm")
    mix <- function(a) .cst_weighted(x$pedon, a, x$msurf, x$msurf + d)$value
    rows <- which(!is.na(x$top) & x$top < x$msurf + d & seq_len(x$n) >= s)
    m <- c(oc = mix("oc_pct"), vm = mix("munsell_value_moist"), vd = mix("munsell_value_dry"),
           cm = mix("munsell_chroma_moist"))
    if (!is.na(m[["oc"]])) oc[rows] <- m[["oc"]]
    if (!is.na(m[["vm"]])) vm[rows] <- m[["vm"]]
    if (!is.na(m[["vd"]])) vd[rows] <- m[["vd"]]
    if (!is.na(m[["cm"]])) cm[rows] <- m[["cm"]]
    flags <- c(flags, "mixed_surface_18cm")
  }
  dark <- vm < t$value_moist_max & vd < t$value_dry_max & cm < t$chroma_moist_max
  # the contrast with the C: 1 value or 2 chroma lower, moist and dry; no C
  # described: not tested (Hugo, 2026-11-06)
  cpos <- which(x$kind %in% "C")
  if (length(cpos)) {
    c1 <- cpos[1L]
    dv <- (col$vm[c1] - vm >= t$c_value_diff) & ifelse(is.na(col$vd[c1]) | is.na(vd), TRUE, col$vd[c1] - vd >= t$c_value_diff)
    dc <- (col$cm[c1] - cm >= t$c_chroma_diff) & ifelse(is.na(col$cd[c1]) | is.na(col$cd), TRUE, col$cd[c1] - col$cd >= t$c_chroma_diff)
    contrast <- dv | dc
    if (any(is.na(col$vd[c1]) | is.na(vd))) flags <- c(flags, "c_contrast_moist_only")
  } else contrast <- rep(TRUE, x$n)
  oc_ok <- oc >= t$oc_min_pct
  bs <- .cst_v(x, "bs_pct")
  st <- .cst_structured(x)
  ty <- .cst_t(x, "structure_type"); sz <- .cst_t(x, "structure_size")
  rup <- .cst_t(x, "rupture_resistance")
  massive_hard <- !is.na(ty) & grepl("massive", ty) & !is.na(rup) & grepl("hard", rup)
  st_ok <- ifelse(massive_hard, FALSE,
             ifelse(is.na(ty), st,
               grepl("granular|crumb|blocky|angular", ty) & !(sz %in% c("coarse", "very coarse"))))
  # the thickness the solum asks
  solum <- .cst_solum(x$pedon)$value
  below_contact <- function(last) {
    if (is.na(last) || last >= x$n) return(FALSE)
    k <- last + 1L
    x$kind[k] %in% "R" || .cst_cemented(x, strong = TRUE)$value[k]
  }
  need <- function(T, last) {
    if (is.na(T)) return(NA)
    if (below_contact(last) && T >= t$on_contact_min_cm) return(TRUE)
    if (is.na(solum)) return(if (T >= t$thick_deep_solum_cm) TRUE else if (T < t$thick_min_cm) FALSE else NA)
    if (solum >= t$deep_solum_cm) T >= t$thick_deep_solum_cm else (T > solum / 3 & T >= t$thick_min_cm)
  }
  r <- list(dark = dark, contrast = contrast, oc_ok = oc_ok, bs = bs, st_ok = st_ok, need = need,
            flags = c(flags, col$c_flag[!is.na(col$c_flag)]), vm = vm, vd = vd, cm = cm, oc = oc, s = s)
  x$memo$humic_layers <- r
  r
}

# The epipedon from the mineral soil surface: the run of layers that meet p,
# with the thickness the solum asks.
.cst_humic_run <- function(x, p, L) {
  s <- L$s
  full <- rep(FALSE, x$n)
  if (is.na(s)) return(rep(NA, x$n))
  r <- .cst_runs(x, p, start = s)
  lo_rows <- which(!is.na(r$lo_id) & r$lo_id == r$lo_id[s])
  hi_rows <- which(!is.na(r$hi_id) & r$hi_id == r$hi_id[s])
  if (!length(hi_rows)) return(full)
  a <- if (length(lo_rows) && isTRUE(p[s])) L$need(r$low[s], max(lo_rows)) else FALSE
  b <- L$need(r$high[s], max(hi_rows))
  g <- if (isTRUE(a)) TRUE else if (isFALSE(b) && !(isTRUE(a))) FALSE else NA
  if (isTRUE(g)) full[lo_rows] <- TRUE
  else if (is.na(g)) full[hi_rows] <- NA
  full
}

.cst_hz_mollic_epipedon <- function(x) {
  L <- .cst_humic_layers(x)
  p <- L$dark & L$contrast & L$oc_ok & L$bs >= .cst_thr("horizons", "mollic", "bs_min_pct") & L$st_ok
  full <- .cst_humic_run(x, p, L)
  rows <- which(is.na(full))
  ans <- .cst_hz_ans(x, full, flags = L$flags,
                     notes = if (!any(x$kind %in% "C")) "no C horizon described: the colour contrast is not tested",
                     missing = .cst_missing_in(x, rows, c("munsell_value_moist", "munsell_value_dry", "munsell_chroma_moist",
                                                          "oc_pct", "bs_pct", "structure_type", "designation")))
  .cst_stand_in(x, ans, list(list(fn = "mollic_epipedon_usda")),
                eligible = is.na(full) | (full %in% FALSE & is.na(.cst_t(x, "structure_type")) & seq_along(full) == L$s))
}

.cst_hz_umbric_epipedon <- function(x) {
  L <- .cst_humic_layers(x)
  p <- L$dark & L$contrast & L$oc_ok & L$bs < .cst_thr("horizons", "mollic", "bs_min_pct")
  full <- .cst_humic_run(x, p, L)
  rows <- which(is.na(full))
  ans <- .cst_hz_ans(x, full, flags = L$flags,
                     missing = .cst_missing_in(x, rows, c("munsell_value_moist", "munsell_value_dry", "munsell_chroma_moist",
                                                          "oc_pct", "bs_pct", "designation")))
  .cst_stand_in(x, ans, list(list(fn = "umbric_epipedon_usda"), list(fn = "umbric_horizon")), eligible = is.na(full))
}

# Ochric (p. 24): the surface layer is light coloured, or poor in carbon, or
# dark and rich enough but too thin for a mollic or umbric epipedon.
.cst_hz_ochric_epipedon <- function(x) {
  t <- .cst_thr("horizons", "mollic")
  L <- .cst_humic_layers(x)
  full <- rep(FALSE, x$n)
  s <- L$s
  if (is.na(s)) return(.cst_hz_ans(x, rep(NA, x$n), missing = c("designation", "oc_pct")))
  mo <- .cst_or(.cst_hz(x, "mollic_epipedon")$full); um <- .cst_or(.cst_hz(x, "umbric_epipedon")$full)
  light <- L$vm[s] >= t$value_moist_max | L$vd[s] >= t$value_dry_max | L$cm[s] >= t$chroma_moist_max
  low_oc <- L$oc[s] < t$oc_min_pct
  thin <- L$dark[s] & L$oc_ok[s]
  g <- .cst_and(!.cst_or(mo, um), .cst_or(light, low_oc, thin))
  full[s] <- g
  .cst_hz_ans(x, full, flags = L$flags,
              missing = if (is.na(g)) .cst_missing_in(x, s, c("munsell_value_moist", "munsell_value_dry",
                                                              "munsell_chroma_moist", "oc_pct", "bs_pct")))
}


# --- crustic epipedons (pp. 27-28) -------------------------------------------------------

.cst_hz_aridic_epipedon <- function(x) {
  t <- .cst_thr("horizons", "aridic")
  full <- rep(FALSE, x$n)
  site <- x$pedon$site
  pos1 <- function(name) { v <- .cst_v(x, name)[1L]; if (is.na(v)) .cst_site_num(site, name) else v }
  txt1 <- function(name) { v <- .cst_t(x, name)[1L]; if (is.na(v)) .cst_site_txt(site, name) else v }
  tex <- .cst_texture_class(x$h)
  # (1) the surface: a pavement, a sand layer or dunes, a biological crust,
  # or polygonal cracks a few mm to 1 cm wide and 1-4 cm deep
  pav <- .cst_or(pos1("desert_pavement_pct") > 0, pos1("varnish_pct") > 0, pos1("ventifact_pct") > 0)
  aeo <- txt1("aeolian_morphology")
  sand <- .cst_or(!is.na(aeo) && aeo %in% c("dune", "sandsheet"), tex[1L] %in% c("S", "LS"))
  crust <- txt1("surface_crust_type")
  bio <- !is.na(crust) && grepl("bio", crust)
  sp <- pos1("polygonal_cracks_spacing_cm"); cw <- pos1("cracks_width_cm"); cd <- pos1("cracks_depth_cm")
  cracks <- !is.na(sp) && sp > 0 && !is.na(cw) && cw <= t$crack_width_max_cm &&
    (is.na(cd) || (cd >= t$crack_depth_cm[1L] && cd <= t$crack_depth_cm[2L]))
  i1 <- .cst_or(pav %in% TRUE, sand %in% TRUE, bio, cracks)
  unrec1 <- all(is.na(c(pos1("desert_pavement_pct"), pos1("varnish_pct"), pos1("ventifact_pct"), aeo, crust, sp)))
  if (is.na(i1)) i1 <- FALSE
  # (2) no salic horizon and no salt crust from the surface
  sal <- .cst_hz(x, "salic_horizon")$full
  scp <- txt1("salt_crust_pattern")
  i2 <- .cst_and(!(if (x$n) sal[1L] else FALSE), is.na(scp) || scp %in% c("none", "absent"))
  # (3) a vesicular crust of 0.5 cm or more over a platy layer of 10 cm or
  # less; (4) the platy layer weak or missing under much gypsum; (5) a
  # transitional regime: dark colours with item (1)b or (1)c
  flags <- character(0)
  ct <- .cst_site_num(site, "surface_crust_thickness_cm")
  ves <- .cst_t(x, "vesicular_pores")
  if (is.na(ct) && x$n && !is.na(ves[1L]) && ves[1L] %in% c("few", "common", "many")) {
    ct <- x$thk[1L]; flags <- c(flags, "crust_thickness_from_top_horizon")
  }
  vcrust <- !is.na(ct) && ct >= t$vesicular_crust_min_cm
  # v0.9.373 (Hugo, 2026-11-06): under an aridic regime a crust not recorded
  # is undecided, not absent
  if (is.na(ct) && identical(.cst_moisture_regime(x$pedon)$climatic$regime, "aridic")) vcrust <- NA
  ty <- .cst_t(x, "structure_type")
  k <- seq_len(min(2L, x$n))
  platy <- any(!is.na(ty[k]) & ty[k] == "platy" & !is.na(x$thk[k]) & x$thk[k] <= t$platy_max_cm)
  gyp <- any(.cst_v(x, "caso4_pct")[k] >= t$gypsum_much_pct | .cst_v(x, "gypsum_visible_pct")[k] > 0, na.rm = TRUE)
  col <- .cst_colours(x)
  trans <- x$n && isTRUE(col$vm[1L] < 3.5 & col$vd[1L] < 5.5 & col$cm[1L] < 3.5) && (isTRUE(sand) || bio)
  i3 <- .cst_or(vcrust & platy, vcrust & gyp, isTRUE(trans))
  if (is.na(i3) && !is.na(vcrust)) i3 <- FALSE
  g <- .cst_and(i1, i2, i3)
  if (x$n) full[1L] <- g
  ans <- .cst_hz_ans(x, full, flags = flags,
                     notes = "the surface features, crusts and platy layers are field observations: not recorded, absent",
                     missing = if (is.na(g)) c(if (is.na(i2)) c("soluble_salts_pct", "ec_1to1_dS_m"),
                                               if (is.na(i3)) "surface_crust_thickness_cm"))
  .cst_stand_in(x, ans, list(list(fn = "yermic_properties"), list(fn = "takyric_properties")),
                eligible = (seq_len(x$n) == 1L) & isTRUE(i2) & unrec1)
}

.cst_hz_salic_crust <- function(x) {
  t <- .cst_thr("horizons", "salic_crust")
  p <- .cst_v(x, "soluble_salts_pct") >= t$salts_min_pct
  full <- .cst_run_grade(x, p, t$thick_min_cm, start = 1L)
  .cst_hz_ans(x, full, missing = .cst_missing_in(x, which(is.na(full)), "soluble_salts_pct"))
}


# --- subsurface horizons (pp. 28-42) --------------------------------------------------------

.cst_hz_albic_horizon <- function(x) {
  t <- .cst_thr("horizons", "albic")
  col <- .cst_colours(x)
  c <- col$c_any; vm <- col$vm; vd <- col$vd
  va <- (vm >= 3 & vd >= 6) | (vm >= 4 & vd >= 5)
  a <- c <= 2 & va
  ug <- .cst_v(x, "uncoated_grains_pct")
  ug_ok <- ifelse(is.na(ug), FALSE, ug >= t$uncoated_grains_min_pct)
  b <- (c <= 3 & (vm >= 6 | vd >= 7)) | (ug_ok & .cst_hue_le(col, 15) & va)
  colour <- a | b
  share <- .cst_v(x, "albic_materials_pct")
  flags <- character(0)
  mat <- ifelse(is.na(share), colour, colour & share >= t$materials_min_pct)
  if (any(is.na(share) & colour %in% TRUE)) flags <- c(flags, "albic_materials_from_colour")
  # tongues are recorded on the layer they penetrate (the glossic layer): the
  # albic layer reads those of the layer below it (R6, Hugo, 2026-11-06)
  td <- .cst_shift(.cst_v(x, "tongue_depth_cm"), 1L)
  tongues_ok <- is.na(td) | td < t$tongue_max_cm
  # an eluvial (E) horizon under another layer (an A, or an O): a layer
  # designated O, B, BC, C or R is none (Hugo, 2026-11-06)
  below <- seq_len(x$n) > 1L & !(x$kind %in% c("O", "B", "BC", "C", "R"))
  p <- mat & tongues_ok & below
  full <- .cst_run_grade(x, p, t$thick_min_cm)
  ans <- .cst_hz_ans(x, full, flags = c(flags, col$c_flag),
                     missing = .cst_missing_in(x, which(is.na(full)), c("munsell_value_moist", "munsell_value_dry",
                                                                       "munsell_chroma_moist")))
  .cst_stand_in(x, ans, list(list(fn = "albic")), eligible = is.na(full))
}

.cst_hz_glossic_horizon <- function(x) {
  t <- .cst_thr("horizons", "glossic")
  td <- .cst_v(x, "tongue_depth_cm"); vol <- .cst_v(x, "tongue_vol_pct"); w <- .cst_v(x, "tongue_width_mm")
  tex <- .cst_texture_class(x$h)
  wmin <- ifelse(tex %in% c("C", "SIC", "SC"), t$width_clayey_mm,
            ifelse(tex %in% c("CL", "SICL", "SCL"), t$width_clay_loamy_mm,
              ifelse(is.na(tex), NA_real_, t$width_loamy_mm)))
  wide <- ifelse(is.na(wmin), ifelse(w >= t$width_loamy_mm, TRUE, ifelse(w < t$width_clayey_mm, FALSE, NA)), w >= wmin)
  deep <- ifelse(is.na(td), FALSE, td >= t$tongue_min_cm)
  full <- deep & vol >= t$vol_range_pct[1L] & vol <= t$vol_range_pct[2L] & wide
  evid <- deep & w >= t$evidence_width_min_mm & vol <= t$vol_range_pct[2L]
  ans <- .cst_hz_ans(x, full, evid,
                     notes = "tongues not recorded are absent (tongue_depth_cm)",
                     missing = .cst_missing_in(x, which(is.na(full) | is.na(evid)), c("tongue_vol_pct", "tongue_width_mm", "clay_pct")))
  .cst_stand_in(x, ans, list(list(fn = "retic_properties"), list(fn = "albeluvic_glossae")),
                eligible = is.na(td) & seq_len(x$n) > 1L)
}

.cst_hz_cambic_horizon <- function(x) {
  t <- .cst_thr("horizons", "cambic")
  col <- .cst_colours(x)
  cand <- (x$kind %in% c("B", "AB", "BC")) | (is.na(x$kind) & seq_len(x$n) > (x$first_min %||% 1L))
  tex <- .cst_vfs_or_finer(x$h)
  sap <- .cst_v(x, "saprolite_pct")
  st <- ifelse(!is.na(sap) & sap >= 50, FALSE, .cst_structured(x))
  r_b <- .cst_shift(col$rank, 1L); c_b <- .cst_shift(col$c_any, 1L); n_b <- .cst_shift(col$neutral, 1L)
  # a hue other than that below, up to 5Y; a neutral layer is never redder or
  # yellower, a hue over a neutral layer is
  hue_diff <- ifelse(col$neutral, FALSE, ifelse(n_b %in% TRUE, col$rank <= 25,
                     col$rank != r_b & col$rank <= 25 & r_b <= 25))
  colour <- (col$c_any > c_b) | hue_diff
  cc <- .cst_v(x, "caco3_pct"); cc_b <- .cst_shift(cc, 1L)
  carb <- cc_b >= t$carbonate_parent_min_pct & cc <= cc_b - t$carbonate_removed_min_pct
  carb[is.na(carb) & !is.na(cc_b) & cc_b < t$carbonate_parent_min_pct] <- FALSE
  # a natro-argic layer holds the illuvial evidence of the argic horizon:
  # it is no cambic horizon (D7)
  natro <- .cst_hz(x, "argic_horizon")$natro %||% rep(FALSE, x$n)
  not_other <- !(.cst_hz(x, "argic_horizon")$full %in% TRUE) & !(natro %in% TRUE) & !(.cst_hz(x, "spodic_horizon")$full %in% TRUE) &
    !(.cst_hz(x, "ferralic_horizon")$full %in% TRUE) & !(.cst_hz(x, "lac_ferric_horizon")$full %in% TRUE)
  undecided_other <- is.na(.cst_hz(x, "argic_horizon")$full) | is.na(.cst_hz(x, "spodic_horizon")$full) |
    is.na(.cst_hz(x, "ferralic_horizon")$full) | is.na(.cst_hz(x, "lac_ferric_horizon")$full)
  excl <- ifelse(!not_other, FALSE, ifelse(undecided_other, NA, TRUE))
  p <- cand & tex & (st | colour | carb) & excl
  thin <- .cst_or(moisture_regime_cst(x$pedon, "aridic")$passed,
                  temperature_regime_cst(x$pedon, c("cryic", "frigid"), or_colder = TRUE)$passed)
  r <- .cst_runs(x, p)
  min_lo <- if (isTRUE(thin)) t$thick_min_thin_cm else t$thick_min_cm
  min_hi <- if (isFALSE(thin)) t$thick_min_cm else t$thick_min_thin_cm
  g_sure <- .cst_thk_ok(r$low, r$high, min_lo); g_poss <- .cst_thk_ok(r$low, r$high, min_hi)
  full <- ifelse(g_sure %in% TRUE, TRUE, ifelse(g_poss %in% FALSE, FALSE, NA))
  full[r$high == 0] <- FALSE
  # the lower boundary 25 cm or more below the soil surface; in an aridic,
  # cryic or frigid soil 5 cm waives both limits (R1, Hugo, 2026-11-06)
  base <- vapply(seq_len(x$n), function(i) {
    id <- r$hi_id[i]; if (is.na(id)) return(NA_real_)
    max(x$bot[which(r$hi_id == id)]) - x$surf
  }, numeric(1))
  shallow <- !is.na(base) & base < t$base_min_cm
  if (isFALSE(thin)) full[shallow] <- FALSE
  else if (is.na(thin)) full[shallow & !(full %in% FALSE)] <- NA
  ans <- .cst_hz_ans(x, full,
                     missing = c(.cst_missing_in(x, which(is.na(full)), c("structure_type", "structure_grade",
                                                                          "munsell_hue_moist", "munsell_chroma_moist",
                                                                          "sand_pct", "clay_pct")),
                                 if (is.na(thin)) c("soil_moisture_regime", "soil_temperature_regime")))
  .cst_stand_in(x, ans, list(list(fn = "cambic")), eligible = is.na(full) & cand)
}

.cst_hz_ferralic_horizon <- function(x) {
  t <- .cst_thr("horizons", "ferralic")
  tex <- .cst_texture_class(x$h)
  clay <- .cst_v(x, "clay_pct")
  tex_ok <- ifelse(is.na(tex), NA, !tex %in% c("S", "LS")) & clay >= t$clay_min_pct
  cec <- .cst_clay_cec(x$h); ecec <- .cst_clay_ecec(x$h)
  act <- cec$value < t$cec_clay_max & ecec$value < t$ecec_clay_max
  wm <- .cst_v(x, "weatherable_minerals_pct"); k <- .cst_v(x, "k_total_pct")
  weath <- .cst_or_v(k < t$k_total_max_pct, wm < t$weatherable_max_pct)
  sap <- .cst_v(x, "saprolite_pct")
  rock <- is.na(sap) | sap < t$rock_structure_max_pct
  p <- tex_ok & act & weath & rock & !(x$kind %in% .CST_NOT_B)
  full <- .cst_run_grade(x, p, t$thick_min_cm)
  flags <- c(if (any(!is.na(cec$flag[full %in% TRUE]))) "cec_method_substitute",
             if (any(is.na(k) & !is.na(wm) & full %in% TRUE)) "weatherables_20_200um_for_50_200um")
  ans <- .cst_hz_ans(x, full, flags = flags,
                     missing = .cst_missing_in(x, which(is.na(full)), c("clay_pct", "cec_cmol", "ecec_cmol",
                                                                       "weatherable_minerals_pct", "k_total_pct")))
  .cst_stand_in(x, ans, list(list(fn = "ferralic")), eligible = is.na(full))
}

# Element-wise three-valued OR of two vectors.
.cst_or_v <- function(a, b) a | b

.cst_hz_lac_ferric_horizon <- function(x) {
  t <- .cst_thr("horizons", "lac_ferric")
  col <- .cst_colours(x)
  iron <- .cst_hue_le(col, 15) | .cst_v(x, "fe_dcb_pct") >= t$fe_dcb_min_pct | .cst_fe_free_total(x$h) >= t$free_total_min
  fer <- .cst_hz(x, "ferralic_horizon")$full
  p <- .cst_vfs_or_finer(x$h) & iron & !(fer %in% TRUE) & !(x$kind %in% .CST_NOT_B)
  p[is.na(fer) & !(p %in% FALSE)] <- NA
  r <- .cst_runs(x, p)
  thick <- .cst_thk_ok(r$low, r$high, t$thick_min_cm)
  thick[r$high == 0] <- FALSE
  # a part of 10 cm or more with a CEC under 24 cmol(+)/kg clay
  low_cec <- .cst_clay_cec(x$h)$value < t$cec_clay_max
  full <- thick
  for (b in unique(stats::na.omit(r$hi_id))) {
    rows <- which(r$hi_id == b)
    q <- rep(FALSE, x$n); q[rows] <- low_cec[rows]
    sub <- .cst_run_grade(x, q, t$cec_part_min_cm)
    s_ok <- .cst_or(sub[rows])
    full[rows] <- .cst_and_v(thick[rows], s_ok)
  }
  .cst_hz_ans(x, full, flags = col$hue_flag,
              missing = .cst_missing_in(x, which(is.na(full)), c("munsell_hue_moist", "fe_dcb_pct", "fe_total_pct",
                                                                 "cec_cmol", "clay_pct")))
}

.cst_and_v <- function(a, b) a & b

.cst_hz_plinthic_horizon <- function(x) {
  t <- .cst_thr("horizons", "plinthic")
  pl <- .cst_v(x, "plinthite_pct")
  hard <- .cst_l(x, "plinthite_hardens")
  # the hardening not recorded is absent; the WRB plinthic horizon at 10 %
  # may stand in (D12, Hugo, 2026-11-06)
  hard_ok <- hard %in% TRUE
  flags <- character(0)
  pf <- ifelse(is.na(pl), FALSE, pl >= t$plinthite_min_pct) & hard_ok
  pe <- ifelse(is.na(pl), FALSE, pl >= t$evidence_plinthite_min_pct)
  full <- .cst_run_grade(x, pf, t$thick_min_cm)
  evid <- .cst_run_grade(x, pe, t$evidence_thick_min_cm)
  ans <- .cst_hz_ans(x, full, evid, flags = flags,
                     notes = "plinthite and its hardening not recorded are absent (plinthite_pct, plinthite_hardens)")
  .cst_stand_in(x, ans, list(list(fn = "plinthic", args = list(min_plinthite_pct = t$plinthite_min_pct))),
                eligible = is.na(pl) | (is.na(hard) & pl >= t$plinthite_min_pct))
}

.cst_hz_spodic_horizon <- function(x) {
  t <- .cst_thr("horizons", "spodic")
  col <- .cst_colours(x)
  ph <- .cst_v(x, "ph_h2o"); oc <- .cst_v(x, "oc_pct")
  a <- ph <= t$ph_max & oc >= t$oc_min_pct
  vm <- col$vm; c <- col$c_any; hue <- col$hue
  is75 <- hue %in% "7.5YR"; is10 <- hue %in% "10YR" | col$neutral
  b <- .cst_hue_le(col, 15) | (is75 & vm <= 5 & c <= 4) | (is10 & vm <= 2 & c <= 2) | (hue %in% "10YR" & vm == 3 & c == 1)
  b[is.na(hue)] <- NA
  alfe <- .cst_v(x, "al_ox_pct") + .cst_v(x, "fe_ox_pct") / 2
  odoe <- .cst_v(x, "odoe")
  cem <- .cst_cemented(x)
  cr <- .cst_v(x, "cracked_coatings_pct")
  c_signs <- (cem$value & cem$recorded) | (!is.na(cr) & cr >= t$cracked_min_pct) |
    (alfe >= t$alfe_min_pct & .cst_shift(alfe, -1L) <= alfe / 2) |
    (odoe >= t$odoe_min & .cst_shift(odoe, -1L) <= odoe / 2)
  cc <- is75 & vm <= 5 & c >= 5 & c <= 6 & c_signs
  # the materials of the evidence grade: the colours (and signs) as before
  mat_e <- a & (b | cc)
  # v0.9.373 (Hugo, 2026-11-06; pp. 32-33 after the KST): the colours alone
  # count only directly below an albic horizon or an E layer; elsewhere one
  # of the other signs is needed (cementation, cracked coatings, Alo + 1/2
  # Feo 0.50 or more with half or less above, ODOE 0.25 or more with half or
  # less above). Measured Alo + 1/2 Feo without the other signs decides the
  # signs absent (spodic evidence at most); with neither oxalate measurement
  # nor ODOE the signs are undecided.
  alb <- .cst_hz(x, "albic_horizon")$full
  up_k <- .cst_shift(x$kind, -1L); up_a <- .cst_shift(alb, -1L)
  below_e <- ifelse(up_k %in% "E" | up_a %in% TRUE, TRUE,
                    ifelse(is.na(up_k) & is.na(up_a) & seq_len(x$n) > 1L, NA, FALSE))
  other <- (cem$value & cem$recorded) | (!is.na(cr) & cr >= t$cracked_min_pct)
  a_s <- ifelse(is.na(alfe), NA, alfe >= t$alfe_min_pct & .cst_shift(alfe, -1L) <= alfe / 2)
  o_s <- ifelse(is.na(odoe), NA, odoe >= t$odoe_min & .cst_shift(odoe, -1L) <= odoe / 2)
  sig <- ifelse(other | a_s %in% TRUE | o_s %in% TRUE, TRUE,
                ifelse((!is.na(alfe) & !(a_s %in% TRUE) & !(o_s %in% TRUE)) |
                         (is.na(alfe) & o_s %in% FALSE), FALSE, NA))
  col2 <- b | (is75 & vm <= 5 & c >= 5 & c <= 6)
  mat <- a & ((b & below_e) | (col2 & sig))
  share <- .cst_v(x, "spodic_materials_pct")
  flags <- c(if (any(!is.na(ph))) "ph_ratio_substitute",
             if (any(is.na(share) & mat %in% TRUE)) "spodic_materials_from_layer")
  sh <- ifelse(is.na(share), 100, share)
  pf <- mat & sh >= t$materials_min_pct & !(x$kind %in% .CST_NOT_B)
  sign <- (!is.na(cr) & cr >= 1) | !is.na(alfe) | !is.na(odoe) | (cem$value & cem$recorded)
  pe <- mat_e & sh >= t$evidence_materials_min_pct & sign & !(x$kind %in% .CST_NOT_B)
  full <- .cst_run_grade(x, pf, t$thick_min_cm)
  evid <- .cst_run_grade(x, pe, 0.1)
  ans <- .cst_hz_ans(x, full, evid, flags = c(flags, col$hue_flag, col$c_flag),
                     missing = .cst_missing_in(x, which(is.na(full)), c("ph_h2o", "oc_pct", "munsell_hue_moist",
                                                                       "munsell_value_moist", "munsell_chroma_moist",
                                                                       "al_ox_pct", "fe_ox_pct")))
  .cst_stand_in(x, ans, list(list(fn = "spodic")), eligible = is.na(full))
}


# --- the argic horizon and the clay-pan (pp. 35-38) ----------------------------------------------

# The CST clay increase over an eluvial clay content e (item 2b, p. 36): 3
# points under 15 %, 1.2 times from 15 to 40 %, 8 points from 40 to 60 %, and
# 8 points of fine clay from 60 %.
.cst_clay_increase <- function(here, e, fine_here = NA_real_, fine_e = NA_real_) {
  t <- .cst_thr("horizons", "argic", "clay_increase")
  n <- max(length(here), length(e))
  here <- rep_len(here, n); e <- rep_len(e, n)
  fine_here <- rep_len(fine_here, n); fine_e <- rep_len(fine_e, n)
  ifelse(is.na(e) | is.na(here), NA,
    ifelse(e < t$low_below_pct, here - e >= t$low_plus_points - 1e-9,
      ifelse(e < t$mid_below_pct, here / e >= t$mid_ratio - 1e-9,
        ifelse(e < t$high_below_pct, here - e >= t$high_plus_points - 1e-9,
               fine_here - fine_e >= t$fine_plus_points - 1e-9))))
}

# The illuvial argic items (2) a-d of each layer, without item e: the
# alkalic horizon reads them too. A lithologic discontinuity (another
# material number in the designation, or another layer_origin) between a
# layer and the eluvial layers above leaves those out of item b.
.cst_argic_core <- function(x) {
  if (!is.null(x$memo$argic_core)) return(x$memo$argic_core)
  t <- .cst_thr("horizons", "argic")
  s <- x$first_min
  clay <- .cst_v(x, "clay_pct"); fine <- .cst_v(x, "fine_clay_pct")
  mat <- .desg_material_number(as.character(.cst_col(x$h, "designation")))
  lo <- .cst_t(x, "layer_origin")
  films <- .cst_clay_films(x)
  incr <- rep(FALSE, x$n); ref <- rep(NA_integer_, x$n); fine_prov <- rep(FALSE, x$n)
  for (i in seq_len(x$n)) {
    if (is.na(s) || i <= s) next
    up <- seq.int(s, i - 1L)
    up <- up[!(x$kind[up] %in% "O")]
    # item 2b: the eluvial layers whose base lies within 30 cm above the
    # layer's top (pp. 35-36, D2)
    up <- up[!is.na(x$bot[up]) & !is.na(x$top[i]) & x$bot[up] >= x$top[i] - t$eluvial_within_cm]
    same <- up[(is.na(mat[up]) | is.na(mat[i]) | mat[up] == mat[i]) &
                 (is.na(lo[up]) | is.na(lo[i]) | lo[up] == lo[i])]
    if (!length(same)) { incr[i] <- FALSE; next }
    cl <- clay[same]
    if (all(is.na(cl)) || is.na(clay[i])) { incr[i] <- NA; next }
    j <- same[which.min(cl)]
    ref[i] <- j
    incr[i] <- .cst_clay_increase(clay[i], clay[j], fine[i], fine[j])
    # v0.9.377 (Hugo, 2026-11-08, p. 36): under 60 % clay item 2b asks more
    # fine clay than the eluvial layer as well as more total clay; measured
    # in both and not higher, it fails; not measured, the total-clay
    # increase passes provisionally (as gley colours without a test, Q11).
    # The fine-clay ratio a third higher is "normally", not required.
    # The eluvial layers compared are those of the same material whose
    # total clay the layer exceeds by the increase (under 60 %): it fails
    # when every one with fine clay measured has as much or more, and is
    # provisional when none has it measured (F6, Hugo, 2026-11-08).
    if (isTRUE(incr[i]) && clay[j] < t$clay_increase$high_below_pct) {
      el <- same[!is.na(clay[same]) & clay[same] < t$clay_increase$high_below_pct]
      el <- el[.cst_clay_increase(rep(clay[i], length(el)), clay[el]) %in% TRUE]
      em <- el[!is.na(fine[el])]
      if (is.na(fine[i]) || !length(em)) fine_prov[i] <- TRUE
      else incr[i] <- any(fine[i] > fine[em])
    }
  }
  micro <- .cst_v(x, "micromorph_argillans_pct") >= t$thin_section_min_pct
  micro[is.na(micro)] <- FALSE
  # (1) not a sedimentary clay layer: a recorded stratification or a fluvial,
  # marine or lacustrine layer of its own (Hugo, 2026-11-06)
  strat <- .cst_t(x, "stratification_pattern")
  sed <- (!is.na(strat) & !strat %in% c("none", "absent")) |
    (!is.na(lo) & grepl("fluv|marine|lacustr|alluv", lo))
  tex <- .cst_texture_class(x$h)
  min_t <- ifelse(tex %in% c("S", "LS"), t$thick_min_sandy_cm, t$thick_min_cm)
  # items 2a-d on an increase vector: the illuvial layers, their thickness
  # and the core
  items <- function(incr) {
    illuv <- films$value | incr | micro
    # a subsurface horizon: below the first mineral layer, not an O, A, E, C
    # or R (Hugo, 2026-11-06); a B exposed at the surface by erosion may pass
    # by its clay films or thin section (items 2a, 2c), never by item 2b (D8,
    # Hugo, 2026-11-06, pp. 35, 37-38)
    eroded <- !is.na(s) & seq_len(x$n) == s & x$kind %in% "B"
    illuv[eroded] <- films$value[eroded] | micro[eroded]
    illuv[(seq_len(x$n) <= (if (is.na(s)) x$n else s) & !eroded) | x$kind %in% .CST_NOT_B] <- FALSE
    # (2d) thickness: a tenth of the layers above, 7.5 cm (15 cm sandy)
    r <- .cst_runs(x, illuv & !sed)
    top_run <- vapply(seq_len(x$n), function(i) {
      id <- r$hi_id[i]; if (is.na(id)) NA_real_ else min(x$top[which(r$hi_id == id)])
    }, numeric(1))
    need <- pmax(min_t, (top_run - x$surf) / 10)
    d_ok <- .cst_thk_ok(r$low, r$high, need)
    d_ok[r$high == 0] <- FALSE
    list(illuv = illuv, d_ok = d_ok, core = (illuv & !sed) & d_ok)
  }
  # the alternative of a provisional class reads those increases as failing
  if (isTRUE(x$argic_firm)) incr[fine_prov] <- FALSE
  it <- items(incr)
  # the layers whose core rests on an increase without fine clay: the core
  # read again with those increases failing
  prov <- rep(FALSE, x$n)
  if (any(fine_prov) && !isTRUE(x$argic_firm)) {
    firm <- items(ifelse(fine_prov, FALSE, incr))
    prov <- it$core %in% TRUE & !(firm$core %in% TRUE)
  }
  out <- list(incr = incr, ref = ref, films = films, micro = micro, illuv = it$illuv, sed = sed,
              core = it$core, d_ok = it$d_ok, prov = prov, fine_prov = fine_prov)
  x$memo$argic_core <- out
  out
}

.cst_hz_argic_horizon <- function(x) {
  t <- .cst_thr("horizons", "argic")
  A <- .cst_argic_core(x)
  alk <- .cst_hz(x, "alkalic_horizon")$full
  sodic <- .cst_sodic_layer(x)
  sodic_f <- ifelse(is.na(sodic), FALSE, sodic)
  e_ok <- !(alk %in% TRUE)
  e_ok[is.na(alk)] <- NA
  full_ill <- A$core & e_ok
  # (3) secondary clayification: more clay than above and below, redder and
  # higher chroma than both, the increase over the surface layer, the thin
  # section item met unless contradicted (Q17), 10 cm (5 cm in an aridic
  # regime), the carbonate coatings of the gravel
  col <- .cst_colours(x)
  clay <- .cst_v(x, "clay_pct")
  ab <- function(v) list(a = .cst_shift(v, -1L), b = .cst_shift(v, 1L))
  cl <- ab(clay); ch <- ab(col$c_any); rk <- ab(col$rank); nn <- ab(col$neutral)
  ki <- compute_ki(.cst_v(x, "sio2_sulfuric_pct"), .cst_v(x, "al2o3_sulfuric_pct"))
  kk <- ab(ki)
  ki_same <- abs(ki - kk$a) <= t$ki_same_rel * ki & abs(ki - kk$b) <= t$ki_same_rel * ki
  bd <- .cst_v(x, "bulk_density_g_cm3"); bb <- ab(bd)
  s_a <- clay > cl$a & clay > cl$b & ki_same
  # redder than a neighbour: a hue over a neutral neighbour is; a neutral
  # layer never is (D11)
  redder <- function(rn, nb) ifelse(col$neutral, FALSE, ifelse(nb %in% TRUE, TRUE, col$rank < rn))
  s_b <- col$c_any > ch$a & col$c_any > ch$b & redder(rk$a, nn$a) & redder(rk$b, nn$b) & bd > bb$a & bd > bb$b
  s <- x$first_min
  s_c <- if (is.na(s)) rep(NA, x$n) else .cst_clay_increase(clay, clay[s])
  mf <- .cst_t(x, "micromorph_features")
  # items c and d are alternatives ("c; or d"); d only from a recorded thin
  # section (D9, Hugo, 2026-11-06)
  s_d <- !is.na(mf) & grepl("pseudomorph|fibrous_oriented_clay|clay_border|clay_domain", mf)
  thin <- moisture_regime_cst(x$pedon, "aridic")$passed
  gc <- .cst_t(x, "gravel_carbonate_coating"); gb <- .cst_shift(gc, 1L)
  s_f <- ifelse(is.na(gc) | is.na(gb), TRUE,
                ifelse(gb == "complete", gc %in% c("none", "partial"), ifelse(gb == "bottom", gc == "none", TRUE)))
  q <- s_a & s_b & (s_c | s_d) & s_f & !A$sed
  q[seq_len(x$n) <= (if (is.na(s)) x$n else s) | x$kind %in% .CST_NOT_B] <- FALSE
  rq <- .cst_runs(x, q)
  q_lo <- if (isTRUE(thin)) t$secondary_thick_min_arid_cm else t$secondary_thick_min_cm
  q_hi <- if (isFALSE(thin)) t$secondary_thick_min_cm else t$secondary_thick_min_arid_cm
  sec <- ifelse(.cst_thk_ok(rq$low, rq$high, q_lo) %in% TRUE, TRUE,
                ifelse(.cst_thk_ok(rq$low, rq$high, q_hi) %in% FALSE, FALSE, NA))
  sec[rq$high == 0] <- FALSE
  # an alkalic horizon is no argic horizon, whichever path
  full <- (full_ill | sec) & e_ok
  # item e: the sodic property excludes it, except as the natro-argic horizon
  # of the Aridosols, which the key asks for by name
  natro <- full %in% TRUE & sodic_f
  full_strict <- ifelse(natro, FALSE, full)
  full_strict[is.na(sodic) & full %in% TRUE] <- NA
  kind <- ifelse(full %in% TRUE | is.na(full),
                 ifelse(natro, "natro-argic", ifelse(full_ill %in% TRUE, "illuvial",
                        ifelse(sec %in% TRUE, "secondary clayification", NA_character_))), NA_character_)
  flags <- c(A$films$flag[full %in% TRUE])
  ans <- .cst_hz_ans(x, full_strict, kind = kind, flags = flags,
                     notes = c(if (any(natro)) "a natro-argic horizon (sodic property): argic only in the Aridosols (natro = TRUE)",
                               "clay films not recorded are absent; a lithologic discontinuity not recorded is none (reading 13)"),
                     missing = .cst_missing_in(x, which(is.na(full_strict)), c("clay_pct", "fine_clay_pct", "esp_pct",
                                                                               "na_cmol", "cec_cmol", "top_cm", "bottom_cm")),
                     sub = list(natro_layers = x$idx[natro]))
  ans$natro <- natro
  ans <- .cst_stand_in(x, ans, list(list(fn = "argic"), list(fn = "argillic_usda")),
                       eligible = (is.na(full_strict) | (full_strict %in% FALSE & !A$films$recorded & is.na(A$incr) &
                                                           seq_len(x$n) > (x$first_min %||% x$n))) &
                         !is.na(sodic) & !is.na(alk))
  # v0.9.377: a layer argic only by a total-clay increase without fine clay
  # (item 2b) is argic provisionally, unless secondary clayification holds
  ans$prov <- (full %in% TRUE) & A$prov & !(sec %in% TRUE) & is.na(ans$stand_in)
  ans$prov_flag <- "argic_increase_without_fine_clay"
  ans
}

.cst_hz_claypan <- function(x) {
  t <- .cst_thr("horizons", "claypan")
  A <- .cst_argic_core(x)
  s <- x$first_min
  clay <- .cst_v(x, "clay_pct")
  ty <- .cst_t(x, "structure_type"); cons <- .cst_t(x, "consistence_moist")
  st <- ifelse(is.na(ty), FALSE, grepl("prism|column", ty)) &
    (is.na(cons) | grepl("firm", cons))
  # (3) the argic increase over the humic (surface) layer and twice the clay
  # of an albic horizon above it
  hum <- if (is.na(s)) rep(NA, x$n) else .cst_clay_increase(clay, clay[s])
  alb <- .cst_hz(x, "albic_horizon")$full
  twice <- vapply(seq_len(x$n), function(i) {
    a <- which(alb[seq_len(i - 1L)] %in% TRUE)
    if (!length(a)) return(TRUE)
    all(clay[i] >= t$albic_clay_ratio * clay[a])
  }, NA)
  i3 <- hum & twice
  i4 <- .cst_clay_films(x, min_mm = t$argillan_min_mm, strict = FALSE)$thick
  # (5) the thin section: met unless a record contradicts it (Q17)
  mf <- .cst_t(x, "micromorph_features"); arg <- .cst_v(x, "micromorph_argillans_pct")
  i5 <- ifelse(is.na(mf) & is.na(arg), TRUE,
               ifelse(is.na(mf), arg < 1, grepl("clay_stripes|fibrous_oriented_clay", mf)))
  # v0.9.376 (Hugo on the review, 2026-11-08): item 4 (argillans 0.5 mm or
  # more) qualifies the first mineral layer too, item 3 only a layer below it
  below <- seq_len(x$n) > (if (is.na(s)) x$n else s)
  first <- !is.na(s) & seq_len(x$n) == s
  p <- st & ifelse(below, i3 | i4, ifelse(first, i4, FALSE)) & i5
  full <- .cst_run_grade(x, p, t$thick_min_cm)
  flags <- if (any(full %in% TRUE & is.na(mf) & is.na(arg))) "thin_section_assumed" else character(0)
  ans <- .cst_hz_ans(x, full, flags = flags,
                     notes = "the thin-section item is met unless a record contradicts it, provisionally (Q17)",
                     missing = .cst_missing_in(x, which(is.na(full)), c("clay_pct", "structure_type")))
  .cst_stand_in(x, ans, list(list(fn = "planic_features"), list(fn = "abrupt_textural_difference")),
                eligible = (is.na(full) | (full %in% FALSE & is.na(ty))) & seq_len(x$n) > (x$first_min %||% x$n))
}

.cst_hz_alkalic_horizon <- function(x) {
  t <- .cst_thr("horizons", "alkalic")
  A <- .cst_argic_core(x)
  ty <- .cst_t(x, "structure_type")
  td <- .cst_v(x, "tongue_depth_cm")
  st <- ifelse(is.na(ty), FALSE, grepl("column", ty) |
                 (grepl("blocky", ty) & !is.na(td) & td >= t$tongue_min_cm))
  esp <- .cst_esp(x); ph <- .cst_v(x, "ph_h2o")
  # within 40 cm of the top of the layer
  within <- function(test) vapply(seq_len(x$n), function(i) {
    j <- which(seq_len(x$n) >= i & !is.na(x$top) & x$top < x$top[i] + t$within_cm & !(A$core %in% FALSE))
    .cst_or(test[j])
  }, NA)
  e_full <- within(esp >= t$esp_min & ph >= t$ph_min)
  s <- x$first_min
  surf_salt <- if (is.na(s)) NA else .cst_v(x, "soluble_salts_pct")[s] < t$surface_salts_max_pct
  full <- A$core & st & e_full & surf_salt
  # v0.9.376 (Hugo on the review, 2026-11-08; p. 39): the evidence read on
  # its printed items only, without the argic items 2a-d: a subsurface
  # layer (below the first mineral layer, not an O, A, E, C or R) with the
  # weakly developed structure of the alkalic horizon and an ESP of 5 or
  # more within 40 cm of its top; the structure is a field observation, not
  # recorded absent (as for the full horizon), the ESP a measurement
  sub <- seq_len(x$n) > (if (is.na(s)) x$n else s) & !(x$kind %in% .CST_NOT_B)
  e_ev2 <- vapply(seq_len(x$n), function(i) {
    j <- which(seq_len(x$n) >= i & !is.na(x$top) & x$top < x$top[i] + t$within_cm & !(x$kind %in% "R"))
    .cst_or((esp >= t$evidence_esp_min)[j])
  }, NA)
  evid <- sub & .cst_and_v(st, e_ev2)
  ans <- .cst_hz_ans(x, full, evid,
                     notes = "evidence read on its printed items: the alkalic structure and an ESP of 5 or more within 40 cm, in a subsurface layer (Hugo, 2026-11-08)",
                     missing = .cst_missing_in(x, which(is.na(full) | is.na(evid)), c("esp_pct", "na_cmol", "cec_cmol", "ph_h2o",
                                                                                     "soluble_salts_pct", "clay_pct")))
  ans <- .cst_stand_in(x, ans, list(list(fn = "natric_horizon", args = list(min_esp = t$esp_min, min_pH_h2o = t$ph_min))),
                       eligible = is.na(ty) & A$core %in% TRUE & e_full %in% TRUE & isTRUE(surf_salt))
  # v0.9.377: the argic items 2a-d met only by a total-clay increase without
  # fine clay (item 2b) give the alkalic horizon provisionally
  ans$prov <- (ans$full %in% TRUE) & A$prov & is.na(ans$stand_in)
  # the evidence grade, read on its own printed items, stays firm
  ans$prov_evid <- ans$prov & !(evid %in% TRUE)
  ans$prov_flag <- "argic_increase_without_fine_clay"
  ans
}


# --- salts, gypsum, carbonates (pp. 39-42) -----------------------------------------------------

.cst_hz_hypersalic_horizon <- function(x) {
  t <- .cst_thr("horizons", "hypersalic")
  p <- .cst_v(x, "soluble_salts_pct") >= t$salts_min_pct & !.cst_cemented(x)$value
  full <- .cst_run_grade(x, p, t$thick_min_cm)
  .cst_hz_ans(x, full, missing = .cst_missing_in(x, which(is.na(full)), "soluble_salts_pct"))
}

.cst_hz_salipan <- function(x) {
  t <- .cst_thr("horizons", "salipan")
  cem <- .cst_cemented(x)
  ty <- .cst_t(x, "structure_type")
  st_ok <- is.na(ty) | grepl("platy|blocky|massive", ty)
  p <- .cst_v(x, "soluble_salts_pct") >= t$salts_min_pct & cem$value & st_ok
  full <- .cst_run_grade(x, p, t$thick_min_cm)
  .cst_hz_ans(x, full, notes = "the cementation is a field observation: not recorded, absent",
              missing = .cst_missing_in(x, which(is.na(full)), "soluble_salts_pct"))
}

.cst_hz_gypsic_horizon <- function(x) {
  t <- .cst_thr("horizons", "gypsic")
  g <- .cst_v(x, "caso4_pct"); vis <- .cst_v(x, "gypsum_visible_pct")
  vis_ok <- ifelse(is.na(vis), FALSE, vis >= t$visible_min_pct)
  p <- g >= t$gypsum_range_pct[1L] & g <= t$gypsum_range_pct[2L] & vis_ok
  r <- .cst_runs(x, p)
  thick <- .cst_thk_ok(r$low, r$high, t$thick_min_cm)
  prod <- .cst_run_sum(x, r, x$thk * .cst_gkg(g))
  prod_ok <- .cst_range_min(prod$low, prod$high, t$product_min)
  full <- thick & prod_ok
  full[r$high == 0] <- FALSE
  below <- .cst_shift(g, 1L)
  pe <- g >= t$evidence_gypsum_min_pct & g <= t$evidence_gypsum_max_pct & g > below
  evid <- full | pe
  # v0.9.375: an evidence left open by the layer below, not described,
  # names the gypsum to measure there
  open_below <- any(is.na(evid) & !is.na(g) & is.na(below))
  ans <- .cst_hz_ans(x, full, evid, notes = c("visible gypsum not recorded is absent (gypsum_visible_pct)",
                                              if (open_below) "the gypsum of the layer below is not known"),
                     missing = c(.cst_missing_in(x, which(is.na(full) | is.na(evid)), "caso4_pct"),
                                 if (open_below) "caso4_pct"))
  .cst_stand_in(x, ans, list(list(fn = "gypsic"), list(fn = "gypsic_horizon_usda")),
                eligible = is.na(vis) & g >= t$gypsum_range_pct[1L] & g <= t$gypsum_range_pct[2L])
}

.cst_hz_hypergypsic_horizon <- function(x) {
  t <- .cst_thr("horizons", "hypergypsic")
  p <- .cst_v(x, "caso4_pct") >= t$gypsum_min_pct & !.cst_cemented(x)$value
  full <- .cst_run_grade(x, p, t$thick_min_cm)
  .cst_hz_ans(x, full, missing = .cst_missing_in(x, which(is.na(full)), "caso4_pct"))
}

# Secondary carbonates by volume: secondary_carbonates_pct, else the soft and
# hard carbonates. Not recorded: none (a field observation).
.cst_sec_carb <- function(x) {
  s <- .cst_v(x, "secondary_carbonates_pct")
  alt <- .cst_v(x, "carbonate_soft_pct") + .cst_v(x, "carbonate_hard_pct")
  alt <- ifelse(is.na(alt), pmax(.cst_v(x, "carbonate_soft_pct"), .cst_v(x, "carbonate_hard_pct"), na.rm = TRUE), alt)
  v <- ifelse(is.na(s), alt, s)
  list(value = ifelse(is.na(v), 0, v), recorded = !is.na(v))
}

# More than the layer above or below by `k` (three-valued; a missing
# neighbour does not count).
.cst_more_than_neighbour <- function(v, k) {
  n <- length(v)
  a <- v - .cst_shift(v, -1L) >= k - 1e-9; b <- v - .cst_shift(v, 1L) >= k - 1e-9
  if (n) { a[1L] <- FALSE; b[n] <- FALSE }
  a | b
}

.cst_hz_calcic_horizon <- function(x) {
  t <- .cst_thr("horizons", "calcic")
  cc <- .cst_v(x, "caco3_pct")
  sc <- .cst_sec_carb(x)
  cem <- .cst_cemented(x, strong = TRUE)$value
  psc <- .cst_psc(x$h)
  clay <- .cst_v(x, "clay_pct")
  hi <- cc >= t$caco3_high_range_pct[1L] & cc <= t$caco3_high_range_pct[2L]
  lo <- cc >= t$caco3_low_range_pct[1L] & cc < t$caco3_low_range_pct[2L]
  a <- hi & .cst_more_than_neighbour(cc, t$a_plus_pct)
  b <- hi & sc$value >= t$b_secondary_min_pct
  coarse <- psc %in% c("sandy", "sandy-skeletal", "coarse-loamy", "loamy-skeletal")
  fine <- psc %in% c("fine", "very-fine", "clayey-skeletal")
  psc_na <- is.na(psc)
  c_ <- lo & clay < t$c_clay_max_pct & ifelse(psc_na, NA, coarse) & .cst_more_than_neighbour(sc$value, t$c_secondary_plus_pct)
  d_ <- lo & ifelse(psc_na, NA, fine) & .cst_more_than_neighbour(sc$value, t$d_secondary_plus_pct)
  pf <- !cem & (a | b | c_ | d_)
  full <- .cst_run_grade(x, pf, t$thick_min_cm)
  # evidence: (1) a or b over 5-14 cm; (2) 15 cm of the CaCO3 of c or d with
  # fewer secondary carbonates; (3) 2-5 % more CaCO3 than a neighbour, or 2-5
  # % secondary carbonates
  e1 <- .cst_run_grade(x, !cem & (a | b), t$evidence_thick_min_cm)
  e2 <- .cst_run_grade(x, !cem & lo & ifelse(psc_na, NA, (coarse & clay < t$c_clay_max_pct) | fine), t$thick_min_cm)
  e3 <- !cem & cc > 0 & (.cst_more_than_neighbour(cc, t$evidence_plus_pct) | sc$value >= t$evidence_secondary_min_pct)
  evid <- full | e1 | e2 | e3
  flags <- if (any((c_ | d_) %in% TRUE)) "secondary_carbonates_volume_for_mass" else character(0)
  ans <- .cst_hz_ans(x, full, evid, flags = flags,
                     notes = "secondary carbonates not recorded are none; items c and d compare their volume (soilKey's reading)",
                     missing = .cst_missing_in(x, which(is.na(full) | is.na(evid)), c("caco3_pct", "clay_pct", "sand_pct")))
  .cst_stand_in(x, ans, list(list(fn = "calcic")), eligible = is.na(full))
}

.cst_hz_hypercalcic_horizon <- function(x) {
  t <- .cst_thr("horizons", "hypercalcic")
  cc <- .cst_v(x, "caco3_pct"); sc <- .cst_sec_carb(x)
  more <- (cc - .cst_shift(cc, 1L) >= t$plus_pct) | (sc$value - .cst_shift(sc$value, 1L) >= t$secondary_plus_pct)
  p <- cc >= t$caco3_min_pct & sc$value > t$secondary_min_pct & more & !.cst_cemented(x, strong = TRUE)$value
  full <- .cst_run_grade(x, p, t$thick_min_cm)
  .cst_hz_ans(x, full, notes = "secondary carbonates not recorded are none",
              missing = .cst_missing_in(x, which(is.na(full)), "caco3_pct"))
}

.cst_hz_calcipan <- function(x) {
  t <- .cst_thr("horizons", "calcipan")
  cc <- .cst_v(x, "caco3_pct")
  cem <- .cst_cemented(x, strong = TRUE)
  p <- cem$value & cc > 0
  r <- .cst_runs(x, p)
  on_rock <- vapply(seq_len(x$n), function(i) {
    id <- r$hi_id[i]; if (is.na(id)) return(FALSE)
    last <- max(which(r$hi_id == id)); last < x$n && x$kind[last + 1L] %in% "R"
  }, NA)
  thick <- ifelse(on_rock, r$high > 0, .cst_thk_ok(r$low, r$high, t$thick_min_cm))
  prod <- .cst_run_sum(x, r, x$thk * .cst_gkg(cc))
  full <- thick & .cst_range_min(prod$low, prod$high, t$product_min)
  full[r$high == 0] <- FALSE
  ans <- .cst_hz_ans(x, full, notes = "the cementation is a field observation: not recorded, absent",
                     missing = .cst_missing_in(x, which(is.na(full)), "caco3_pct"))
  .cst_stand_in(x, ans, list(list(fn = "petrocalcic")), eligible = !cem$recorded & !(cc %in% FALSE) & !is.na(cc))
}

.cst_hz_phosphipan <- function(x) {
  t <- .cst_thr("horizons", "phosphipan")
  p <- .cst_v(x, "p_total_pct") >= t$p_total_min_pct & .cst_v(x, "caco3_pct") >= t$caco3_min_pct &
    .cst_cemented(x)$value & !is.na(x$top) & x$top - x$surf >= t$top_min_cm
  full <- .cst_run_grade(x, p, t$thick_min_cm)
  .cst_hz_ans(x, full, missing = .cst_missing_in(x, which(is.na(full)), c("p_total_pct", "caco3_pct")))
}

# Salic horizon (p. 42): salts or the 1:1 EC, the limits of an arid region
# or of other regions; with the region unknown, a layer passes above the
# higher limit, fails below the lower and is undecided between (Q14).
.cst_hz_salic_horizon <- function(x) {
  t <- .cst_thr("horizons", "salic")
  arid <- .cst_arid_region(x$pedon)
  av <- arid$value
  sal <- .cst_v(x, "soluble_salts_pct"); ec <- .cst_v(x, "ec_1to1_dS_m")
  band <- function(v, a, o) if (isTRUE(av)) v >= a else if (isFALSE(av)) v >= o
                            else ifelse(v >= a, TRUE, ifelse(v < o, FALSE, NA))
  bs <- band(sal, t$salts_arid_min_pct, t$salts_other_min_pct); be <- band(ec, t$ec_arid_min, t$ec_other_min)
  # the salts decide where measured (the 1:1 EC may still pass the layer),
  # the 1:1 EC where they are not (Q13)
  p <- ifelse(is.na(sal), be, bs | (be %in% TRUE))
  # v0.9.373 (Hugo, 2026-11-06): salts not measured in a layer with a pH
  # (H2O) under 5.5, or under a perudic regime, are read as too few for a
  # salic horizon, flagged
  ph <- .cst_v(x, "ph_h2o")
  perud <- identical(.cst_moisture_regime(x$pedon)$climatic$regime, "perudic")
  ruled <- is.na(p) & ((ph < t$no_salts_ph_max) %in% TRUE | perud)
  p[ruled] <- FALSE
  r <- .cst_runs(x, p)
  thick <- .cst_thk_ok(r$low, r$high, t$thick_min_cm)
  ps <- .cst_run_sum(x, r, x$thk * .cst_gkg(sal)); pe <- .cst_run_sum(x, r, x$thk * ec)
  pr_s <- .cst_range_min(ps$low, ps$high, t$salt_product_min); pr_e <- .cst_range_min(pe$low, pe$high, t$ec_product_min)
  prod <- ifelse(is.na(pr_s) & !is.na(pr_e), pr_e, pr_s | (pr_e %in% TRUE))
  full <- thick & prod
  full[r$high == 0] <- FALSE
  # the evidence on the salts, or on the 1:1 EC where they are not measured
  # (soilKey's convention, R10, Hugo, 2026-11-06)
  es <- band(sal, t$evidence_arid_min_pct, t$evidence_other_min_pct)
  ee <- band(ec, t$evidence_ec_arid_min, t$evidence_ec_other_min)
  evid <- full | ifelse(is.na(sal), ee, es | (ee %in% TRUE))
  evid[ruled & is.na(evid)] <- FALSE
  .cst_hz_ans(x, full, evid, flags = if (any(ruled)) "salic_ruled_out_by_acidity_or_perudic_regime",
              notes = c(sprintf("arid region: %s (%s)", format(av), arid$source %||% NA),
                        "the saturated-paste EC and the 1:5 EC are not read (Q13)"),
              missing = c(.cst_missing_in(x, which(is.na(full) | is.na(evid)), c("soluble_salts_pct", "ec_1to1_dS_m")),
                          if (is.na(av)) arid$missing))
}

.cst_hz_sulfuric_horizon <- function(x) {
  t <- .cst_thr("horizons", "sulfuric")
  ss <- .cst_v(x, "sulfidic_s_pct")
  sulfidic_below <- .cst_shift(ss >= t$sulfidic_s_min_pct, 1L)
  jar <- .cst_l(x, "jarosite_present")
  mot <- as.character(.cst_col(x$h, "mottle_colors"))
  jar_mot <- vapply(mot, function(m) {
    if (is.na(m) || !nzchar(m)) return(FALSE)
    any(vapply(strsplit(m, ";")[[1L]], function(one) {
      p <- regmatches(one, regexec("([0-9.]*[A-Z]+)\\s*([0-9.]+)/([0-9.]+)", trimws(one)))[[1L]]
      if (length(p) < 4L) return(FALSE)
      rk <- .cst_hue_rank(p[2L]); ch <- as.numeric(p[4L])
      isTRUE(rk >= 22.5 & ch >= 6)
    }, NA))
  }, NA, USE.NAMES = FALSE)
  jarosite <- (jar %in% TRUE) | jar_mot
  sulf <- .cst_v(x, "sulfate_s_pct") >= t$sulfate_s_min_pct | .cst_v(x, "s_total_pct") >= t$s_total_min_pct
  ph <- .cst_v(x, "ph_h2o") < t$ph_max
  # the horizon on jarosite, sulfate and pH; the layer under its base holds
  # sulfidic materials (D10, p. 43)
  p <- jarosite & sulf & ph
  r <- .cst_runs(x, p)
  full <- .cst_thk_ok(r$low, r$high, t$thick_min_cm)
  full[r$high == 0] <- FALSE
  # the sulfidic materials as the characteristic reads them (0.9.372): the
  # WRB sulfidic S, an incubation pH under 4, or the WRB stand-ins
  sm <- .cst_ch(x, "sulfidic_materials")
  sid <- sm$full
  # a lower grade only where the layer below rests on the WRB hyper- or
  # hyposulfidic stand-ins (Hugo, 2026-11-06)
  si <- rep(NA_character_, x$n)
  for (b in unique(stats::na.omit(r$hi_id))) {
    rows <- which(r$hi_id == b); last <- max(rows)
    below <- if (last < x$n) sid[last + 1L] else NA
    full[rows] <- full[rows] & below
    if (last < x$n && !is.na(sm$stand_in[last + 1L])) si[rows[full[rows] %in% TRUE]] <- sm$stand_in[last + 1L]
  }
  ans <- .cst_hz_ans(x, full, stand_in = si,
                     flags = c(if (any(!is.na(si))) sprintf("stand_in:%s", unique(stats::na.omit(si))),
                               if (any(!is.na(.cst_v(x, "ph_h2o")))) "ph_ratio_substitute"),
                     notes = c("jarosite not recorded is absent", "sulfidic materials below read by the characteristic (sulfidic_materials_cst())"),
                     missing = .cst_missing_in(x, which(is.na(full)), c("sulfidic_s_pct", "sulfate_s_pct", "s_total_pct", "ph_h2o")))
  .cst_stand_in(x, ans, list(list(fn = "thionic"), list(fn = "sulfuric_horizon_usda")),
                eligible = is.na(jar) & !jar_mot & !(ph %in% FALSE) & !(sulf %in% FALSE))
}


# --- the horizon panel ---------------------------------------------------------------------

#' The diagnostic horizons of a pedon by the Chinese Soil Taxonomy (2001)
#'
#' The horizon panel of the namespace cst: every layer read against the 33
#' diagnostic horizons of the book (pp. 20-43), with the grade each reaches
#' (full, evidence, none or undecided), the kind where the book names kinds,
#' and the WRB or USDA stand-ins used. The book defines no horizon notation,
#' so every layer is read on the data (route "data").
#'
#' @param pedon A PedonRecord.
#' @param ... Not used.
#' @return The frame of identify_horizons(), one row per layer: `code` the
#'   first full horizon of the layer in the book's order, `overlay` the other
#'   full ones, `features` the evidence grades ("<name> evidence"),
#'   `features_na` and `candidates` the horizons left undecided, `passed`
#'   (TRUE a horizon, FALSE none, NA undecided), plus the CST columns
#'   `diagnostics` (every full horizon), `evidences`, `undecided`, `kinds`
#'   ("<name>: <kind>"), `stand_ins` (the soilKey diagnostics that stood in)
#'   and `lower_grade` (a stand-in was used); since 0.9.372 the layer
#'   characteristics: `characteristics` (those the layer holds, with their
#'   kind), `characteristic_evidences` and `characteristic_stand_ins`.
#' @noRd
.cst_identify <- function(pedon, ...) {
  if (!inherits(pedon, "PedonRecord")) rlang::abort("'pedon' must be a PedonRecord.")
  fr <- .horizon_frame(pedon)
  n <- nrow(fr)
  lst <- rep(list(character(0)), n)
  fr$diagnostics <- lst; fr$evidences <- lst; fr$undecided <- lst; fr$kinds <- lst
  fr$stand_ins <- lst; fr$lower_grade <- rep(FALSE, n)
  fr$characteristics <- lst; fr$characteristic_evidences <- lst; fr$characteristic_stand_ins <- lst
  if (!n) return(fr)
  x <- .cst_ctx(pedon)
  tab <- .cst_horizon_table()
  for (k in seq_len(nrow(tab))) {
    r <- .cst_hz(x, tab$id[k])
    if (tab$id[k] == "argic_horizon" && !is.null(r$natro)) r$full[r$natro %in% TRUE] <- TRUE
    for (p in seq_len(n)) {
      i <- x$idx[p]
      nm <- tab$name[k]
      if (r$full[p] %in% TRUE) {
        fr$diagnostics[[i]] <- c(fr$diagnostics[[i]], nm)
        if (!is.na(r$kind[p])) fr$kinds[[i]] <- c(fr$kinds[[i]], sprintf("%s: %s", nm, r$kind[p]))
        if (!is.na(r$stand_in[p])) {
          fr$stand_ins[[i]] <- unique(c(fr$stand_ins[[i]], r$stand_in[p]))
          fr$lower_grade[i] <- TRUE
        }
      } else if (r$full[p] %in% FALSE && r$evid[p] %in% TRUE) {
        fr$evidences[[i]] <- c(fr$evidences[[i]], paste(nm, "evidence"))
      } else if (is.na(r$full[p])) {
        fr$undecided[[i]] <- c(fr$undecided[[i]], nm)
      }
    }
  }
  # the layer characteristics (0.9.372): those a layer holds (with their
  # kind) and their evidences; undecided ones are not listed
  ch <- .cst_characteristic_table()
  ch <- ch[ch$scope == "layer", , drop = FALSE]
  for (k in seq_len(nrow(ch))) {
    r <- .cst_ch(x, ch$id[k])
    for (p in seq_len(n)) {
      i <- x$idx[p]
      nm <- ch$name[k]
      if (r$full[p] %in% TRUE) {
        lab <- if (!is.na(r$kind[p])) sprintf("%s (%s)", nm, r$kind[p]) else nm
        if (isTRUE(r$prov[p])) lab <- paste(lab, "(provisional)")
        fr$characteristics[[i]] <- c(fr$characteristics[[i]], lab)
        if (!is.na(r$stand_in[p])) fr$characteristic_stand_ins[[i]] <- unique(c(fr$characteristic_stand_ins[[i]], r$stand_in[p]))
      } else if (r$full[p] %in% FALSE && r$evid[p] %in% TRUE) {
        fr$characteristic_evidences[[i]] <- c(fr$characteristic_evidences[[i]], paste(sub(" (features|property)$", "", nm), "evidence"))
      }
    }
  }
  for (i in seq_len(n)) {
    d <- fr$diagnostics[[i]]
    fr$code[i] <- if (length(d)) d[1L] else NA_character_
    fr$overlay[[i]] <- if (length(d) > 1L) d[-1L] else character(0)
    fr$features[[i]] <- fr$evidences[[i]]
    fr$features_na[[i]] <- fr$undecided[[i]]
    fr$candidates[[i]] <- if (!length(d)) fr$undecided[[i]] else character(0)
    fr$passed[i] <- if (length(d)) TRUE else if (length(fr$undecided[[i]])) NA else FALSE
    fr$route[i] <- "data"
    parts <- c(if (length(d)) sprintf("[soilKey's reading of the CST horizons] %s", paste(d, collapse = ", "))
               else if (length(fr$undecided[[i]])) sprintf("[soilKey's reading of the CST horizons] undecided: %s",
                                                           paste(fr$undecided[[i]], collapse = ", "))
               else "[soilKey's reading of the CST horizons] no diagnostic horizon",
               if (length(fr$evidences[[i]])) paste(fr$evidences[[i]], collapse = ", "),
               fr$kinds[[i]],
               if (length(fr$stand_ins[[i]])) sprintf("soilKey's stand-in, flagged: %s",
                                                      paste0(fr$stand_ins[[i]], "()", collapse = ", ")),
               if (length(c(fr$characteristics[[i]], fr$characteristic_evidences[[i]])))
                 sprintf("characteristics: %s", paste(c(fr$characteristics[[i]], fr$characteristic_evidences[[i]]),
                                                      collapse = ", ")))
    fr$evidence[i] <- paste(parts, collapse = "; ")
  }
  fr
}


# --- the readers of the key -----------------------------------------------------------------

# One reader per horizon (the names the key will use), each
# f(pedon, grade = "full", top_max = NULL, datum = "surface").
.cst_hz_reader <- function(id) {
  force(id)
  function(pedon, grade = c("full", "evidence", "at_least_evidence"), top_max = NULL,
           datum = c("surface", "mineral")) {
    .cst_hz_result(pedon, id, grade = match.arg(grade), top_max = top_max, datum = match.arg(datum))
  }
}

histic_epipedon_cst        <- .cst_hz_reader("histic_epipedon")
mattic_epipedon_cst        <- .cst_hz_reader("mattic_epipedon")
mollic_epipedon_cst        <- .cst_hz_reader("mollic_epipedon")
umbric_epipedon_cst        <- .cst_hz_reader("umbric_epipedon")
ochric_epipedon_cst        <- .cst_hz_reader("ochric_epipedon")
siltigic_epipedon_cst      <- .cst_hz_reader("siltigic_epipedon")
cumulic_epipedon_cst       <- .cst_hz_reader("cumulic_epipedon")
fimic_epipedon_cst         <- .cst_hz_reader("fimic_epipedon")
anthrostagnic_epipedon_cst <- .cst_hz_reader("anthrostagnic_epipedon")
aridic_epipedon_cst        <- .cst_hz_reader("aridic_epipedon")
salic_crust_cst            <- .cst_hz_reader("salic_crust")
albic_horizon_cst          <- .cst_hz_reader("albic_horizon")
glossic_horizon_cst        <- .cst_hz_reader("glossic_horizon")
cambic_horizon_cst         <- .cst_hz_reader("cambic_horizon")
ferralic_horizon_cst       <- .cst_hz_reader("ferralic_horizon")
lac_ferric_horizon_cst     <- .cst_hz_reader("lac_ferric_horizon")
plinthic_horizon_cst       <- .cst_hz_reader("plinthic_horizon")
spodic_horizon_cst         <- .cst_hz_reader("spodic_horizon")
agric_horizon_cst          <- .cst_hz_reader("agric_horizon")
phos_agric_horizon_cst     <- .cst_hz_reader("phos_agric_horizon")
hydragric_horizon_cst      <- .cst_hz_reader("hydragric_horizon")
claypan_cst                <- .cst_hz_reader("claypan")
alkalic_horizon_cst        <- .cst_hz_reader("alkalic_horizon")
hypersalic_horizon_cst     <- .cst_hz_reader("hypersalic_horizon")
salipan_cst                <- .cst_hz_reader("salipan")
gypsic_horizon_cst         <- .cst_hz_reader("gypsic_horizon")
hypergypsic_horizon_cst    <- .cst_hz_reader("hypergypsic_horizon")
calcic_horizon_cst         <- .cst_hz_reader("calcic_horizon")
hypercalcic_horizon_cst    <- .cst_hz_reader("hypercalcic_horizon")
calcipan_cst               <- .cst_hz_reader("calcipan")
phosphipan_cst             <- .cst_hz_reader("phosphipan")
salic_horizon_cst          <- .cst_hz_reader("salic_horizon")
sulfuric_horizon_cst       <- .cst_hz_reader("sulfuric_horizon")

# The argic horizon; natro = TRUE also passes the natro-argic horizon (an
# argic horizon with the sodic property), which the Aridosols take (p. 37).
argic_horizon_cst <- function(pedon, grade = c("full", "evidence", "at_least_evidence"),
                              top_max = NULL, datum = c("surface", "mineral"), natro = FALSE) {
  grade <- match.arg(grade); datum <- match.arg(datum)
  x <- .cst_ctx(pedon)
  r <- NULL
  if (isTRUE(natro) && x$n) {
    r <- .cst_hz(x, "argic_horizon")
    r$full[r$natro %in% TRUE] <- TRUE
    r$evid[r$natro %in% TRUE] <- TRUE
  }
  res <- .cst_hz_result(pedon, "argic_horizon", grade = grade, top_max = top_max, datum = datum, x = x, r = r)
  res$evidence$natro_allowed <- isTRUE(natro)
  res
}
