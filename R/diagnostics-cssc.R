# =============================================================================
# v0.9.267 -- diagnostics of the Canadian System of Soil Classification, 3rd
# edition (Soil Classification Working Group 1998), for its Key to Soil Orders
# (Ch 3, pp. 33-34) and the definitions it relies on (Ch 2).
#
# The Canadian keys are written largely in terms of horizon designations ("a
# Bt horizon", "Bm, Btj or Bfj horizons"), whose definitions in Ch 2 are partly
# quantitative. Each diagnostic here therefore reads the measured properties
# first and the surveyor's designations second (decision D12 of the national
# roadmap):
#   * properties decide when they can (podzolic B chemistry, the Bt clay
#     increase, the solonetzic B Ca/Na ratio, the chernozemic A, gley
#     colours), whether they pass or fail;
#   * the designations decide only what the properties leave open, and the
#     evidence records it (route = "designations");
#   * with neither, the result is NA.
# Designations are read in the Canadian notation only when the pedon's
# notation is Canadian (.cssc_notation(): declared, a Canadian site, or no
# sign of the FAO/USDA notation); otherwise only properties are read.
# Depths in the keys are measured from the mineral surface, which the book
# uses without defining; it is read here as the top of the first mineral
# horizon below any L, F, H or O horizons.
#
# Choices where the text is silent or the key and a chapter differ are written
# in inst/rules/cssc3/readings.yaml. Names carry the _cssc suffix: the
# Canadian Chernozemic, Gleysolic or Luvisolic orders are not WRB's
# chernozem(), gleysol() or luvisol().
# =============================================================================

.CSSC_REF <- "Soil Classification Working Group (1998). The Canadian System of Soil Classification, 3rd ed."

.cssc_ref <- function(where) sprintf("%s %s", .CSSC_REF, where)

# Parsed designations of a pedon, or nothing (as if none were recorded) when
# they are not in the Canadian notation.
.cssc_parsed <- function(pedon) {
  h <- pedon$horizons
  d <- h$designation %||% rep(NA_character_, nrow(h))
  notation <- .cssc_notation(d, pedon$site)
  parsed <- if (notation$ok) .parse_designation_cssc(d)
            else rep(list(list()), nrow(h))
  list(parsed = parsed, notation = notation)
}

# Horizons of a pedon with their parsed designations and depths below the
# mineral surface. A horizon is organic by its designation (O, L, F, H) or,
# without one, by more than 17 % organic C; anything else is mineral. In
# another notation the masters that mean the same everywhere are read: W
# (water) and R (rock) are never mineral horizons, and an O or H is organic
# unless its organic C says otherwise.
.cssc_horizons <- function(pedon) {
  h <- pedon$horizons
  pp <- .cssc_parsed(pedon)
  parsed <- pp$parsed
  mineral <- .cssc_is_mineral(parsed)
  organic_by_oc <- !is.na(h$oc_pct) & h$oc_pct > 17
  other <- substr(.cssc_other_masters(h$designation %||% rep(NA_character_, nrow(h)),
                                      pp$notation$ok), 1L, 1L)
  mineral <- ifelse(!is.na(mineral), mineral,
             ifelse(other %in% c("W", "R"), FALSE,
             ifelse(other %in% c("O", "H") & is.na(h$oc_pct), FALSE, !organic_by_oc)))
  # the shallowest mineral horizon, whatever the order of the rows
  cand <- which(mineral & !is.na(h$top_cm))
  ms <- if (length(cand) == 0L) NA_real_ else min(h$top_cm[cand])
  list(h = h, parsed = parsed, notation = pp$notation, mineral = mineral,
       ms = ms, top_ms = h$top_cm - ms, bottom_ms = h$bottom_cm - ms,
       thick = h$bottom_cm - h$top_cm,
       desig_known = vapply(parsed, length, integer(1)) > 0L)
}

# Position of a Munsell hue on the hue circle, 0-100 (10R = 10, 5YR = 15,
# 10YR = 20, 10Y = 30, 5GY = 35); RP hues count as redder than R (negative).
.cssc_hue_pos <- function(hue) {
  vapply(as.character(hue), function(x) {
    p <- .munsell_hue_parts(x)
    if (is.na(p$family) || p$family == "N") return(NA_real_)
    off <- c(R = 0, YR = 10, Y = 20, GY = 30, G = 40, BG = 50, B = 60,
             PB = 70, P = 80, RP = -10)[[p$family]]
    off + p$number
  }, numeric(1), USE.NAMES = FALSE)
}

.cssc_is_neutral <- function(hue) {
  toupper(substr(trimws(as.character(hue)), 1, 1)) %in% "N"
}

.cssc_result <- function(name, passed, layers = integer(0), evidence = list(),
                         missing = character(0), where = "", notes = NA_character_) {
  DiagnosticResult$new(name = name, passed = passed, layers = as.integer(layers),
                       evidence = evidence, missing = unique(missing),
                       reference = .cssc_ref(where), notes = notes)
}

# Combine a property route and a designation route (each TRUE/FALSE/NA):
# the properties decide whenever they can, pass or fail; the designations
# decide only what the properties leave open.
.cssc_route <- function(by_props, by_desig) {
  if (isTRUE(by_props)) return(list(passed = TRUE, route = "properties"))
  if (identical(by_props, FALSE)) return(list(passed = FALSE, route = "properties"))
  if (isTRUE(by_desig)) return(list(passed = TRUE, route = "designations"))
  if (identical(by_desig, FALSE)) return(list(passed = FALSE, route = "designations"))
  list(passed = NA, route = "none")
}

# Three-valued any(): TRUE if any is TRUE, NA if none is TRUE and some is NA.
.any3 <- function(x) {
  if (any(x %in% TRUE)) TRUE else if (anyNA(x)) NA else FALSE
}

# Runs of adjacent rows in depth order where `keep` is TRUE; a gap in the
# record (a top below the deepest bottom of the run so far) ends a run.
.cssc_run_groups <- function(x, keep) {
  ord <- order(x$h$top_cm)
  top <- x$h$top_cm; bot <- x$h$bottom_cm
  out <- list(); cur <- integer(0)
  for (i in ord) {
    deepest <- if (length(cur)) suppressWarnings(max(bot[cur], na.rm = TRUE)) else -Inf
    gap <- length(cur) && !is.na(top[i]) && is.finite(deepest) && top[i] > deepest + 0.01
    if (gap) { out[[length(out) + 1L]] <- cur; cur <- integer(0) }
    if (isTRUE(keep[i])) cur <- c(cur, i)
    else { if (length(cur)) out[[length(out) + 1L]] <- cur; cur <- integer(0) }
  }
  if (length(cur)) out[[length(out) + 1L]] <- cur
  out
}

# The depth a run spans (overlapping rows are not counted twice); NA when a
# row has no depth.
.cssc_run_span <- function(x, r) {
  top <- x$h$top_cm[r]; bot <- x$h$bottom_cm[r]
  if (anyNA(top) || anyNA(bot)) NA_real_ else max(bot) - min(top)
}

# Runs of adjacent horizons in depth order, three-valued: TRUE when a run of
# rows that certainly qualify (TRUE in `ok`) is at least `min_cm` thick and
# begins within `max_top_cm` of the mineral surface (any depth when NULL);
# NA when a run of rows that may qualify (TRUE or NA) could be (a row of
# unknown thickness may go on, a run of unknown top may begin there);
# FALSE otherwise. The runs are formed before the depth limit is applied, so
# a run that begins within the limit counts with its parts below it.
# $rows: the rows of the certain runs that pass; $maybe_rows: the rows of
# the runs that may pass.
.cssc_run_test <- function(x, ok, min_cm, max_top_cm = NULL) {
  top_ok <- function(r) {
    if (is.null(max_top_cm)) return(TRUE)
    t <- x$top_ms[r[1L]]
    if (is.na(t)) NA else t <= max_top_cm
  }
  sure <- .cssc_run_groups(x, ok %in% TRUE)
  pass <- vapply(sure, function(r) {
    th <- .cssc_run_span(x, r); if (is.na(th)) th <- sum(x$thick[r], na.rm = TRUE)
    th >= min_cm && isTRUE(top_ok(r))
  }, logical(1))
  maybe <- .cssc_run_groups(x, !(ok %in% FALSE))
  could <- vapply(maybe, function(r) {
    long <- anyNA(x$thick[r]) || .cssc_run_span(x, r) >= min_cm
    long && !identical(top_ok(r), FALSE)
  }, logical(1))
  list(passed = if (any(pass)) TRUE else if (any(could)) NA else FALSE,
       rows = sort(unlist(sure[pass])), maybe_rows = sort(unlist(maybe[could])))
}

# Depth covered by a set of rows, overlapping rows counted once: $cm over the
# rows with both depths, $open when a row lacks one (it may add more).
.cssc_cover <- function(x, rows) {
  rows <- rows[!is.na(rows)]
  top <- x$h$top_cm[rows]; bot <- x$h$bottom_cm[rows]
  ok <- !is.na(top) & !is.na(bot) & bot > top
  open <- any(is.na(top) | is.na(bot))
  if (!any(ok)) return(list(cm = 0, open = open))
  iv <- cbind(top[ok], bot[ok])[order(top[ok]), , drop = FALSE]
  cm <- 0; s0 <- iv[1, 1]; e0 <- iv[1, 2]
  if (nrow(iv) > 1L) for (k in 2:nrow(iv)) {
    if (iv[k, 1] <= e0) e0 <- max(e0, iv[k, 2])
    else { cm <- cm + e0 - s0; s0 <- iv[k, 1]; e0 <- iv[k, 2] }
  }
  list(cm = cm + e0 - s0, open = open)
}

# Rows of every run of adjacent qualifying horizons (TRUE in `ok`, in depth
# order) that is at least `min_cm` thick; a row of unknown thickness adds
# nothing.
.cssc_run_rows_all <- function(x, ok, min_cm) {
  ord <- order(x$h$top_cm); out <- integer(0); cur <- integer(0)
  flush <- function() if (length(cur) && sum(x$thick[cur], na.rm = TRUE) >= min_cm) cur else integer(0)
  for (i in ord) {
    if (isTRUE(ok[i])) cur <- c(cur, i) else { out <- c(out, flush()); cur <- integer(0) }
  }
  sort(c(out, flush()))
}

# Total thickness of runs of adjacent qualifying horizons (TRUE in `ok`);
# returns the largest run and its rows.
.cssc_runs <- function(ok, thick) {
  best <- 0; best_rows <- integer(0); cur <- 0; cur_rows <- integer(0)
  for (i in seq_along(ok)) {
    if (isTRUE(ok[i]) && !is.na(thick[i])) {
      cur <- cur + thick[i]; cur_rows <- c(cur_rows, i)
      if (cur > best) { best <- cur; best_rows <- cur_rows }
    } else {
      cur <- 0; cur_rows <- integer(0)
    }
  }
  list(thickness = best, rows = best_rows)
}


# ---- Bt horizon (Ch 2, p. 15) ----------------------------------------------

#' Bt horizon (CSSC)
#'
#' A B horizon of illuvial clay at least 5 cm thick: by properties, the clay
#' increase over the eluvial horizon above (at least 3 percentage points when
#' that horizon has less than 15 % clay, a ratio of 1.2 between 15 and 40 %,
#' 8 points above 40 %, the limits of USDA's argillic horizon) together with
#' clay films; otherwise a strong t in the designation (Bt, Btg, Bnt; not
#' Btj). The clay-increase test does not apply to a designated Bt with no
#' eluvial horizon above it (a truncated soil, or only an Ap above) or across
#' a lithologic discontinuity, where the book accepts clay skins alone
#' (criterion 5); there the designation decides.
#'
#' Each horizon is decided on its own, by its properties when they decide and
#' otherwise by its designation; adjacent Bt horizons are then added up
#' towards the 5 cm (.cssc_run_test), and a Bt "within" a depth is a run
#' that begins there. By properties a horizon is compared only with the
#' mineral horizons above it; it fails when its clay and the clay of every
#' mineral horizon above are recorded and show no increase, or when its clay
#' films are recorded as absent, and is undecided otherwise. The first
#' mineral horizon has nothing to be compared with.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Upper boundary within this depth below the mineral
#'   surface, or \code{NULL} for any depth.
#' @noRd
bt_horizon_cssc <- function(pedon, max_top_cm = NULL) {
  x <- .cssc_horizons(pedon); h <- x$h; n <- nrow(h)
  raw <- .cssc_raw_master(x); osfx <- .cssc_other_suffix(x)
  # a B by designation (in another notation by its master letter), or a
  # horizon with no designation; organic layers are not
  is_b <- .cssc_is(x$parsed, "B")
  is_b <- ifelse(!is.na(is_b), is_b, ifelse(!is.na(raw), raw == "B", NA))
  b_or_unknown <- (is.na(is_b) | is_b) & x$mineral
  films <- tolower(trimws(as.character(h$clay_films_amount)))
  has_films <- ifelse(is.na(films) | !nzchar(films), NA,
                      !films %in% c("none", "absent", "0", "no"))
  # the clay increase over the eluvial horizon above (Ch 2, p. 15, criterion
  # 1), with USDA's argillic limits; a horizon is compared only with mineral
  # horizons above it
  clay <- h$clay_pct
  eluvial <- (.cssc_is(x$parsed, "A", "e", "any") %in% TRUE) | (raw %in% "E")
  undesignated <- !x$desig_known & is.na(raw)
  above_of <- function(i) which(x$mineral & !is.na(h$bottom_cm) & !is.na(h$top_cm[i]) &
                                  h$bottom_cm <= h$top_cm[i] + 0.01)
  increase <- function(above, here) {
    if (above < 15) here - above >= 3 else if (above < 40) here / above >= 1.2
    else here - above >= 8
  }
  props <- vapply(seq_len(n), function(i) {
    if (!b_or_unknown[i]) return(FALSE)
    if (identical(has_films[i], FALSE)) return(FALSE)
    above <- above_of(i)
    if (length(above) == 0L) return(FALSE)
    # the horizon it is compared with is the eluvial one (or one that may be
    # eluvial); with none designated, any mineral horizon above. The limits
    # only get harder as that clay rises, so the lowest clay recorded decides
    # a pass, and a fail once every such clay is recorded.
    ref <- above[eluvial[above] | undesignated[above]]
    if (length(ref) == 0L) ref <- above
    if (is.na(clay[i])) return(NA)
    known <- ref[!is.na(clay[ref])]
    if (length(known) == 0L) return(NA)
    if (increase(min(clay[known]), clay[i])) has_films[i]
    else if (length(known) == length(ref)) FALSE else NA
  }, logical(1))
  # by designation: a strong t on a B (Bt, Btg, Bnt; not Btj); in another
  # notation, where t also means illuvial clay, a B carrying a t; a horizon
  # with no designation may be one
  bt <- .cssc_is(x$parsed, "B", "t", weak = "no")
  bt <- ifelse(!is.na(bt), bt, ifelse(!is.na(raw), raw == "B" & grepl("t", osfx), NA))
  bt[!x$mineral] <- FALSE
  known <- x$desig_known | !is.na(raw)
  # criterion 5: a designated Bt with no eluvial horizon above it, or across
  # a discontinuity, is not put to the clay-increase test; the clay skins it
  # then rests on must not be recorded as absent
  roman <- vapply(x$parsed, function(cmp) if (length(cmp)) cmp[[1]]$roman else "", "")
  # in another notation a discontinuity is an Arabic number before the master
  other_disc <- sub("^([0-9]*).*$", "\\1", trimws(as.character(h$designation)))
  roman <- ifelse(x$desig_known, roman, ifelse(is.na(other_disc), "", other_disc))
  # (a discontinuity between the eluvial horizon and the Bt: no eluvial
  # horizon above in the Bt's own material)
  for (r in which(bt %in% TRUE)) {
    above <- which(!is.na(h$bottom_cm) & h$bottom_cm <= h$top_cm[r] + 0.01)
    elu <- above[eluvial[above]]
    if (!any(roman[elu] == roman[r]) && !identical(has_films[r], FALSE)) props[r] <- NA
  }
  # each horizon by its properties when they decide, otherwise by its designation
  row <- ifelse(!is.na(props), props, bt)
  rt <- .cssc_run_test(x, row, 5, max_top_cm)
  by_props <- .cssc_run_test(x, props, 5, max_top_cm)$passed
  by_desig <- if (!any(known)) NA else .cssc_run_test(x, bt, 5, max_top_cm)$passed
  label <- function(src) if (!length(src) || all(src)) "properties"
                        else if (!any(src)) "designations" else "properties+designations"
  route <- if (is.na(rt$passed)) "none"
           else if (isTRUE(rt$passed)) label(!is.na(props[rt$rows]))
           else label(!is.na(props[b_or_unknown]))
  miss <- if (is.na(rt$passed)) c("clay_pct", "clay_films_amount", "designation")
          else character(0)
  .cssc_result("bt_horizon_cssc", rt$passed, rt$rows,
               list(route = route, by_properties = by_props,
                    by_designations = by_desig, notation = x$notation$why,
                    maybe_rows = setdiff(rt$maybe_rows, rt$rows),
                    by_properties_rows = props),
               miss, "Ch 2, p. 15 (suffix t); Ch 3 key, p. 34")
}



# ---- Podzolic B horizon (Ch 2, pp. 13-14, 17) -------------------------------

# Per-layer reading of podzolic B material (Ch 2, pp. 13-14 and 17), used by
# the Podzolic order and its great groups. By properties: bh, bhf and bf are
# the Bh, Bhf and Bf chemistries with the colour of the definition, on B
# horizons (or horizons without a designation), each TRUE, FALSE or NA;
# any = podzolic B material of either kind. By designations: strong h, hf or
# f suffixes on B horizons.
.cssc_podzolic_layers <- function(x) {
  h <- x$h
  is_b <- .cssc_is(x$parsed, "B")
  # in another notation the master letter tells a B from an A, E, C or O;
  # organic layers are never podzolic B material
  raw <- .cssc_raw_master(x)
  is_b <- ifelse(!is.na(is_b), is_b, ifelse(!is.na(raw), raw == "B", NA))
  b_or_unknown <- (is.na(is_b) | is_b) & x$mineral
  oc <- h$oc_pct; fe <- h$fe_pyro_pct; al <- h$al_pyro_pct; clay <- h$clay_pct
  val <- h$munsell_value_moist; chr <- h$munsell_chroma_moist
  pos <- .cssc_hue_pos(h$munsell_hue_moist)
  sands <- !is.na(h$sand_pct) & h$sand_pct >= 85
  # colour: black, or 10YR or redder with chroma > 3 or value <= 3
  black <- !is.na(val) & !is.na(chr) & val <= 2.5 & chr <= 2
  colour <- ifelse(black, TRUE,
            ifelse(is.na(pos) | is.na(val) | is.na(chr), NA,
                   pos <= 20 & (chr > 3 | val <= 3)))
  # Bh chemistry (three-valued), with moist value and chroma <= 3 (Ch 10)
  bh <- ifelse(is.na(oc) | is.na(fe), NA,
               oc > 1 & fe < 0.3 & (fe == 0 | oc / fe >= 20))
  bh <- ifelse(bh %in% TRUE & !is.na(val) & !is.na(chr), val <= 3 & chr <= 3,
               ifelse(bh %in% TRUE, NA, bh))
  # Bf/Bhf chemistry (three-valued)
  alfe <- al + fe
  bf <- ifelse(is.na(oc) | is.na(fe) | is.na(al) | is.na(clay), NA,
               oc > 0.5 & alfe >= ifelse(sands, 0.4, 0.6) &
                 (clay == 0 | alfe / clay > 0.05) &
                 (fe >= 0.3 | (fe > 0 & oc / fe < 20)))
  layer <- function(chem) ifelse(!b_or_unknown, FALSE,
                         ifelse(chem %in% FALSE | colour %in% FALSE, FALSE,
                                ifelse(chem %in% TRUE & colour %in% TRUE, TRUE, NA)))
  chem_any <- ifelse(bh %in% TRUE | bf %in% TRUE, TRUE,
                     ifelse(is.na(bh) | is.na(bf), NA, FALSE))
  bhf_chem <- ifelse(bf %in% TRUE, ifelse(is.na(oc), NA, oc > 5), bf)
  # by designation: strong h, hf or f on a B; in another notation a Bs or Bh
  # (any podzolic kind, but not the Bh or Bhf of the great groups)
  osfx <- .cssc_other_suffix(x)
  oth <- ifelse(!is.na(raw), raw == "B" & !is.na(osfx) & grepl("[sh]", gsub("ss|se", "", osfx)), NA)
  d_any <- .cssc_is(x$parsed, "B", "h", "no") | .cssc_is(x$parsed, "B", "hf", "no") |
           .cssc_is(x$parsed, "B", "f", "no")
  list(any = layer(chem_any), bh = layer(bh), bhf = layer(bhf_chem),
       d_any = ifelse(!is.na(d_any), d_any, oth),
       d_bh = .cssc_is(x$parsed, "B", "h", "no"),
       d_bhf = .cssc_is(x$parsed, "B", "hf", "no"))
}

# At least `min_cm` of one kind of podzolic material (props_kind and
# desig_kind name elements of .cssc_podzolic_layers()), properties first.
.cssc_podzolic_run <- function(x, props_kind, desig_kind, min_cm = 10) {
  L <- .cssc_podzolic_layers(x)
  ok <- L[[props_kind]]
  # by designation; a mineral horizon with no designation may be one
  dk <- L[[desig_kind]]
  dk[!x$mineral] <- FALSE
  # each layer by its chemistry and colour when they decide, otherwise by its
  # designation; then adjacent layers are added up (.cssc_run_test)
  row <- ifelse(!is.na(ok), ok, dk)
  rt <- .cssc_run_test(x, row, min_cm)
  pt <- .cssc_run_test(x, ok, min_cm)
  known <- x$desig_known | !is.na(.cssc_raw_master(x))
  by_desig <- if (!any(known)) NA else .cssc_run_test(x, dk, min_cm)$passed
  src <- !is.na(ok[if (isTRUE(rt$passed)) rt$rows else which(x$mineral)])
  route <- if (is.na(rt$passed)) "none"
           else if (!length(src) || all(src)) "properties"
           else if (!any(src)) "designations" else "properties+designations"
  run <- .cssc_runs(ok %in% TRUE, ifelse(is.na(x$thick), 0, x$thick))
  drun <- .cssc_runs(dk %in% TRUE, ifelse(is.na(x$thick), 0, x$thick))
  list(passed = rt$passed, route = route, by_props = pt$passed,
       by_desig = by_desig, rows = rt$rows,
       thickness_properties = run$thickness, thickness_designations = drun$thickness)
}

#' Podzolic B horizon (CSSC)
#'
#' At least 10 cm of Bh, Bhf or Bf material. By properties, each layer meets
#' one of the two chemistries of p. 17: Bh (organic C > 1 %, pyrophosphate Fe
#' < 0.3 %, organic C / pyrophosphate Fe >= 20, moist value and chroma <= 3)
#' or Bf/Bhf (organic C > 0.5 %, pyrophosphate Al + Fe >= 0.6 %, or 0.4 % in
#' sands, (Al + Fe) / clay > 0.05, and pyrophosphate Fe >= 0.3 % or organic C
#' / pyrophosphate Fe < 20), with the moist colour of the definition: black,
#' or hue 10YR or redder with chroma above 3 or value 3 or less. Otherwise
#' strong h, hf or f suffixes on B horizons totalling 10 cm.
#' @noRd
podzolic_b_cssc <- function(pedon) {
  x <- .cssc_horizons(pedon)
  r <- .cssc_podzolic_run(x, "any", "d_any")
  miss <- if (is.na(r$passed)) c("oc_pct", "fe_pyro_pct", "al_pyro_pct", "clay_pct",
                                 "munsell_value_moist", "designation")
          else character(0)
  .cssc_result("podzolic_b_cssc", r$passed, r$rows,
               list(route = r$route, by_properties = r$by_props,
                    by_designations = r$by_desig, notation = x$notation$why,
                    thickness_properties = r$thickness_properties,
                    thickness_designations = r$thickness_designations),
               miss, "Ch 2, pp. 13-14 and 17; Ch 10, p. 108")
}


# ---- Solonetzic B horizon (Ch 2, p. 17) -------------------------------------

#' Solonetzic B horizon (CSSC)
#'
#' A B horizon with exchangeable Ca / Na <= 10, prismatic or columnar
#' structure and a hard to extremely hard dry consistence; otherwise a strong
#' n in a B designation (Bn, Bnt).
#' @noRd
solonetzic_b_cssc <- function(pedon) {
  x <- .cssc_horizons(pedon); h <- x$h
  is_b <- .cssc_is(x$parsed, "B")
  b_or_unknown <- is.na(is_b) | is_b
  ratio <- ifelse(!is.na(h$na_cmol) & h$na_cmol > 0 & !is.na(h$ca_cmol),
                  h$ca_cmol / h$na_cmol,
                  ifelse(!is.na(h$na_cmol) & h$na_cmol == 0, Inf, NA_real_))
  st <- tolower(as.character(h$structure_type))
  prism <- ifelse(is.na(h$structure_type), NA, grepl("prism|column", st))
  rr <- tolower(as.character(h$rupture_resistance))
  hard <- ifelse(is.na(h$rupture_resistance), NA,
                 rr %in% c("hard", "very hard", "extremely hard"))
  ok <- ifelse(!b_or_unknown, FALSE,
        ifelse((!is.na(ratio) & ratio > 10) | prism %in% FALSE | hard %in% FALSE, FALSE,
        ifelse(!is.na(ratio) & prism %in% TRUE & hard %in% TRUE, TRUE, NA)))
  by_props <- .any3(ok)
  bn <- .cssc_is(x$parsed, "B", "n", weak = "no")
  by_desig <- if (any(bn %in% TRUE)) TRUE else if (!any(x$desig_known)) NA else FALSE
  r <- .cssc_route(by_props, by_desig)
  .cssc_result("solonetzic_b_cssc", r$passed,
               if (r$route == "properties") which(ok %in% TRUE) else which(bn %in% TRUE),
               list(route = r$route, by_properties = by_props,
                    by_designations = by_desig, notation = x$notation$why,
                    ca_na_ratio = ratio),
               if (is.na(r$passed)) c("ca_cmol", "na_cmol", "structure_type",
                                      "rupture_resistance", "designation")
               else character(0),
               "Ch 2, p. 17; Ch 12, p. 122")
}


# ---- Vertic and slickenside horizons (Ch 2, pp. 15-17; Ch 13, p. 131) -------

#' Vertic and slickenside horizons (CSSC Vertisolic order)
#'
#' A vertic horizon (a strong v in a designation; the book gives no
#' measurable limit) and a slickenside horizon (ss in a designation, or
#' common, many or continuous slickensides: more than two) whose top lies
#' within \code{max_top_cm} of the mineral surface. Slickensides recorded as
#' absent or few on every horizon within that depth rule the order out even
#' without designations.
#' @noRd
vertisolic_cssc <- function(pedon, max_top_cm = 100) {
  x <- .cssc_horizons(pedon); h <- x$h
  in_depth <- !is.na(x$top_ms) & x$top_ms <= max_top_cm
  vert <- .cssc_has_suffix(x$parsed, "v", weak = "no")
  ss_desig <- .cssc_has_suffix(x$parsed, "ss", weak = "any") %in% TRUE
  sl <- tolower(trimws(as.character(h$slickensides)))
  ss_prop <- ifelse(is.na(h$slickensides), NA, sl %in% c("common", "many", "continuous"))
  ss_here <- ifelse(ss_desig, TRUE, ss_prop)
  has_ss <- .any3(ss_here[in_depth])
  has_v <- if (any(vert %in% TRUE)) TRUE else if (!any(x$desig_known)) NA else FALSE
  passed <- if (identical(has_ss, FALSE) || identical(has_v, FALSE)) FALSE
            else if (isTRUE(has_ss) && isTRUE(has_v)) TRUE else NA
  .cssc_result("vertisolic_cssc", passed,
               which((vert %in% TRUE) | (ss_here %in% TRUE & in_depth)),
               list(route = if (any(ss_prop %in% TRUE)) "properties+designations"
                            else "designations",
                    notation = x$notation$why,
                    vertic_horizon = has_v, slickenside_horizon = has_ss),
               if (is.na(passed)) c("designation", "slickensides") else character(0),
               "Ch 2, pp. 15-17; Ch 3 key, p. 34; Ch 13, p. 131")
}


# ---- Gleying (Ch 3 key E, p. 34; Ch 7, p. 81) -------------------------------

# Gley colours per layer (Ch 3 key E, p. 34), TRUE, FALSE or NA, on every
# layer whatever its depth or kind; see gleying_cssc() for the clauses.
.cssc_gley_colours <- function(h) {
  neutral <- .cssc_is_neutral(h$munsell_hue_moist)
  pos <- .cssc_hue_pos(h$munsell_hue_moist)
  chroma <- ifelse(neutral & is.na(h$munsell_chroma_moist), 0, h$munsell_chroma_moist)
  value <- h$munsell_value_moist
  contrast <- tolower(trimws(as.character(h$mottle_contrast)))
  redox <- h$redoximorphic_features_pct
  mottles_known_absent <- (!is.na(redox) & redox == 0)
  prominent <- ifelse(contrast %in% "prominent", TRUE,
               ifelse(contrast %in% c("faint", "distinct") | mottles_known_absent, FALSE, NA))
  dist_prom <- ifelse(contrast %in% c("distinct", "prominent"), TRUE,
               ifelse(contrast %in% "faint" | mottles_known_absent, FALSE, NA))
  red <- !is.na(pos) & pos <= 15
  colour_known <- (neutral | !is.na(pos)) & !is.na(chroma)
  clause <- function(cond, extra = TRUE) ifelse(cond %in% FALSE, FALSE,
                                         ifelse(cond %in% TRUE, extra, NA))
  a <- clause((neutral | (!red & !is.na(pos))) & chroma <= 1,
              ifelse(is.na(value), NA, value >= 4))
  b <- clause(!is.na(pos) & pos >= 17.5 & pos <= 20 & chroma <= 2, prominent)
  cc <- clause(!is.na(pos) & pos > 20 & pos <= 30 & chroma <= 3, prominent)
  d <- !is.na(pos) & pos > 30
  rr <- clause(red, ifelse(is.na(redox), NA, redox >= 2) & dist_prom)
  ifelse(!colour_known, NA,
         ifelse(a %in% TRUE | b %in% TRUE | cc %in% TRUE | d | rr %in% TRUE, TRUE,
                ifelse(is.na(a) | is.na(b) | is.na(cc) | is.na(rr), NA, FALSE)))
}

#' Gleysolic gleying features (CSSC)
#'
#' Within \code{max_top_cm} of the mineral surface, a horizon other than an
#' Ah, Ap or Ae (as Ch 7 specifies; an Ae or E over 20 cm thick reaching below
#' 60 cm still counts; an undesignated surface horizon is taken for the A;
#' when the surface A is more than 50 cm thick, the mineral horizon right
#' below it is tested instead) whose moist colours show reduction: chroma <= 1 in a grey
#' matrix (value 4 or more: a dark, humus-stained horizon does not turn
#' brighter when oxidised); or, in hues 10YR and 7.5YR, chroma <= 2 with
#' prominent mottles; or, in hues yellower than 10YR, chroma <= 3 with
#' prominent mottles; or a hue bluer than 10Y; or, in red materials (hue 5YR
#' or redder), distinct or prominent mottles covering at least 2 %. A
#' neutral hue (N) has chroma 0. Unknown colours or mottle contrast leave a
#' horizon undecided by its colours; such a horizon is then read from its own
#' designation (a strong g or gf, the 2016 key's test), and stays undecided
#' when it has none.
#' @noRd
gleying_cssc <- function(pedon, max_top_cm = 50) {
  gr <- .cssc_gley_rows(pedon, max_top_cm)
  x <- gr$x
  cand <- gr$cand
  passed <- if (any(cand)) .any3(gr$row[cand]) else FALSE
  by_props <- if (any(cand)) .any3(gr$gley[cand]) else FALSE
  route <- if (isTRUE(passed)) {
             if (any(cand & gr$gley %in% TRUE)) "properties" else "designations"
           } else if (identical(passed, FALSE)) {
             if (all(!is.na(gr$gley[cand]))) "properties" else "designations"
           } else "none"
  .cssc_result("gleying_cssc", passed, which(cand & gr$row %in% TRUE),
               list(route = route, by_properties = by_props,
                    notation = x$notation$why),
               if (is.na(passed)) c("munsell_hue_moist", "munsell_chroma_moist",
                                    "munsell_value_moist", "mottle_contrast",
                                    "designation")
               else character(0),
               "Ch 3 key E, p. 34; Ch 7, pp. 81-82")
}

# The horizons gleying_cssc() tests and, for each, whether it is gleyed: by
# its colours when they decide, otherwise by its own designation (a strong g
# or gf, when it is designated), otherwise NA. Deciding horizon by horizon
# keeps a horizon with no data from being read as not gleyed, and a horizon
# with unknown colours from overriding measured ones elsewhere.
.cssc_gley_rows <- function(pedon, max_top_cm = 50) {
  x <- .cssc_horizons(pedon); h <- x$h
  d <- trimws(as.character(h$designation %||% rep(NA_character_, nrow(h))))
  other_full <- .cssc_other_masters(d, x$notation$ok)
  other <- substr(other_full, 1L, 1L)
  other[nchar(other_full) > 1L] <- NA_character_   # transitions (AB, BC) are neither
  # buried horizons (b), in the Canadian notation or another one
  buried <- (.cssc_has_suffix(x$parsed, "b", weak = "any") %in% TRUE) |
            (!is.na(other_full) & grepl("^[0-9]*[A-Z]+[a-z0-9]*b", .cssc_other_case(d)))
  ahp <- (.cssc_is(x$parsed, "A", "h", "any") | .cssc_is(x$parsed, "A", "p", "any")) %in% TRUE
  ae  <- (.cssc_is(x$parsed, "A", "e", "any") %in% TRUE) | (other %in% "E")
  # an Ae (or an E) more than 20 cm thick reaching below 60 cm is tested
  deep_ae <- ae & !is.na(x$thick) & x$thick > 20 & !is.na(x$bottom_ms) & x$bottom_ms > 60
  excluded <- ahp | (ae & !deep_ae) | (other %in% "A")
  # a surface horizon with no designation stands for the A
  undesignated <- !x$desig_known & is.na(other_full)
  surface_undes <- x$mineral & undesignated & !is.na(x$top_ms) & abs(x$top_ms) < 0.01
  excluded <- excluded | surface_undes
  # the surface A (adjacent Ah and Ap horizons from the mineral surface, an A
  # in another notation, or the undesignated surface horizon taken for it; a
  # buried A does not count): when it is more than 50 cm thick, the mineral
  # horizon right below it is tested instead, whatever it is (Ch 7, p. 81),
  # a gap in the record allowed
  a_row <- ((ahp | other %in% "A") & !buried) | surface_undes
  ord <- order(h$top_cm)
  a_cm <- 0; a_base <- x$ms
  if (!is.na(x$ms)) {
    for (i in ord) {
      if (is.na(h$top_cm[i]) || h$top_cm[i] < x$ms - 0.01 || !x$mineral[i]) next
      if (!a_row[i] || !isTRUE(abs(h$top_cm[i] - a_base) < 0.01) || is.na(h$bottom_cm[i])) break
      a_cm <- a_cm + x$thick[i]; a_base <- h$bottom_cm[i]
    }
  }
  in_depth <- !is.na(x$top_ms) & x$top_ms <= max_top_cm
  cand <- in_depth & !excluded & x$mineral
  if (a_cm > 50) {
    nxt <- which(x$mineral & !is.na(h$top_cm) & h$top_cm >= a_base - 0.01)
    below <- if (length(nxt)) nxt[which.min(h$top_cm[nxt])] else NA_integer_
    cand <- seq_len(nrow(h)) %in% below
  }
  gley <- .cssc_gley_colours(h)
  g <- .cssc_has_suffix(x$parsed, "g", weak = "no")
  row <- ifelse(!is.na(gley), gley, ifelse(x$desig_known, g %in% TRUE, NA))
  list(x = x, cand = cand, gley = gley, g = g, row = row)
}


# ---- Organic order (Ch 3 key B, pp. 33-34) ----------------------------------

# Master letters of designations written in another notation (FAO/USDA),
# which the Canadian reading leaves aside; callers read only the masters
# that mean the same in every notation. All NA when the designations are
# read as Canadian.
# A designation written all in capitals in another notation ("BHS", "2BT1",
# "AP") is read as its master and lowercase suffixes ("Bhs", "2Bt1", "Ap");
# a transition (two of A, B, C, E) keeps both letters ("BCT" is "BCt", "BC"
# stays "BC").
.cssc_other_case <- function(d) {
  core <- gsub("[^A-Za-z]", "", sub("/.*$", "", d))
  caps <- !is.na(d) & nchar(core) > 1L & !grepl("[a-z]", core)
  d[caps] <- vapply(d[caps], function(s) {
    i <- regexpr("[A-Z]", s)
    k <- if (grepl("^[ABCE][ABCE]", substring(s, i))) i + 1L else i
    paste0(substr(s, 1L, k), tolower(substring(s, k + 1L)))
  }, character(1))
  d
}

.cssc_other_masters <- function(designation, notation_ok) {
  n <- length(designation)
  if (isTRUE(notation_ok)) return(rep(NA_character_, n))
  d <- .cssc_other_case(trimws(as.character(designation)))
  d <- sub("^[0-9]+", "", sub("/.*$", "", d))
  m <- regmatches(d, regexec("^([A-Z]+)", d))
  vapply(seq_len(n), function(i) {
    if (is.na(d[i]) || length(m[[i]]) < 2L) NA_character_ else m[[i]][2L]
  }, character(1))
}

# Kinds of layers for the Organic order and its great groups. Organic: more
# than 17 % organic C or, without organic C, an O, L, F or H designation in
# the Canadian notation. Rock (R) and water (W). In another notation, an A,
# B, C or E master is mineral, R rock and W water (L there is a limnic layer
# and is not read as folic).
.cssc_layer_kinds <- function(h, pp) {
  parsed <- pp$parsed
  master_in <- function(set) vapply(parsed, function(comps) {
    if (length(comps) == 0L) return(NA)
    any(vapply(comps, function(cmp)
      all(strsplit(cmp$master, "")[[1]] %in% set), logical(1)))
  }, logical(1))
  other <- .cssc_other_masters(h$designation %||% rep(NA_character_, nrow(h)),
                               pp$notation$ok)
  rock <- (master_in("R") %in% TRUE) | (other %in% "R")
  water <- (master_in("W") %in% TRUE) | (other %in% "W")
  o_desig <- master_in(c("O", "L", "F", "H"))
  o_desig[is.na(o_desig) & (substr(other, 1L, 1L) %in% c("A", "B", "C", "E") | rock | water)] <- FALSE
  list(organic = ifelse(!is.na(h$oc_pct), h$oc_pct > 17, o_desig),
       folic = master_in(c("L", "F", "H")) %in% TRUE, rock = rock, water = water)
}

#' Organic soil (CSSC Key to Soil Orders, item B)
#'
#' Option 1: organic horizons (organic C > 17 %, or O, L, F, H designations)
#' from the surface to 60 cm when the surface layer is fibric (rubbed fibre
#' >= 40 % or, unmeasured, an Of; bulk density below 0.075 g/cm3), to 40 cm
#' when it is mesic or humic; folic material (L, F, H) to more than 40 cm, or
#' at least 10 cm when a lithic contact (R) follows, any mineral layer between
#' them being thinner than 20 cm and less than half the folic thickness. A
#' folic surface layer lying directly on an O horizon is litter over peat:
#' the peat limits then apply too, to the organic material from the top of
#' that O horizon, whose kind is the surface kind. Option 2, only
#' when a mineral layer starts within 40 cm of the surface: more than 40 cm
#' of organic material in the upper 80 cm, and at least 40 cm of it under a
#' surface mineral layer when there is one. A layer of unknown organic status
#' leaves the order undecided when it could change the answer.
#' @noRd
organic_soil_cssc <- function(pedon) {
  h <- pedon$horizons
  if (nrow(h) == 0L) return(.cssc_result("organic_soil_cssc", FALSE, where = "Ch 3, p. 33"))
  pp <- .cssc_parsed(pedon); parsed <- pp$parsed
  lk <- .cssc_layer_kinds(h, pp)
  organic <- lk$organic; folic <- lk$folic; rock <- lk$rock
  thick <- h$bottom_cm - h$top_cm
  top0 <- min(h$top_cm, na.rm = TRUE)
  ord <- order(h$top_cm)
  ord <- ord[!is.na(h$top_cm[ord])]
  if (length(ord) == 0L) return(.cssc_result("organic_soil_cssc", NA,
                                             missing = "top_cm", where = "Ch 3, p. 33"))
  s <- ord[1L]
  # where continuous organic material starting at position k0 ends; with
  # accept, layers of unknown organic status continue it
  col_end <- function(k0, accept) {
    depth <- h$top_cm[ord[k0]]
    for (k in seq.int(k0, length(ord))) {
      i <- ord[k]
      ok <- isTRUE(organic[i]) || (accept && is.na(organic[i]))
      if (!ok || !isTRUE(h$top_cm[i] <= depth + 0.01)) break
      # an organic layer with no bottom depth may go on indefinitely
      if (is.na(h$bottom_cm[i])) return(if (accept) Inf else depth)
      depth <- max(depth, h$bottom_cm[i])
    }
    depth
  }
  cont <- col_end(1L, FALSE) - top0
  surf_folic <- isTRUE(folic[s])
  # the peat surface layer: the surface horizon, or the layer right under a
  # folic (L, F, H) surface layer when it may be peat (litter over peat)
  n_folic <- sum(cumprod(folic[ord]))
  peat_k <- if (!surf_folic) 1L
            else if (n_folic < length(ord) && !(organic[ord[n_folic + 1L]] %in% FALSE) &&
                     !folic[ord[n_folic + 1L]]) n_folic + 1L
            else NA_integer_
  peat_row <- if (is.na(peat_k)) NA_integer_ else ord[peat_k]
  # the kind of surface organic material (Ch 3, p. 33): measured rubbed fibre
  # first, then the designation, then the bulk density
  surf_fibric <- NA
  if (!is.na(peat_row)) {
    fib <- h$fiber_content_rubbed_pct[peat_row]; bd <- h$bulk_density_g_cm3[peat_row]
    surf_fibric <- if (!is.na(fib)) fib >= 40
                   else if (.cssc_is(parsed[peat_row], "O", "f", "any") %in% TRUE) TRUE
                   else if (.cssc_is(parsed[peat_row], "O", "m", "any") %in% TRUE ||
                            .cssc_is(parsed[peat_row], "O", "h", "any") %in% TRUE) FALSE
                   else if (!is.na(bd)) bd < 0.075 else NA
    if (isTRUE(surf_fibric) && !is.na(bd) && bd >= 0.075) surf_fibric <- FALSE
  }
  peat_test <- function(cm) {
    if (is.na(cm)) return(NA)
    need <- if (isTRUE(surf_fibric)) 60 else if (identical(surf_fibric, FALSE)) 40
            else if (cm >= 60) 60 else if (cm < 40) 40 else NA
    if (is.na(need)) NA else cm >= need
  }
  by_peat <- if (is.na(peat_k)) FALSE else {
    from <- h$top_cm[peat_row]
    lo <- peat_test(col_end(peat_k, FALSE) - from)
    # an open organic layer is read as possibly organic all the way down
    hi_end <- col_end(peat_k, TRUE)
    hi <- if (is.infinite(hi_end)) peat_test(1e6) else peat_test(hi_end - from)
    if (identical(lo, hi)) lo else NA
  }
  # option 1
  b1 <- if (identical(organic[s], FALSE)) FALSE
        else if (surf_folic) {
          # with Canadian designations, an undesignated layer that may be
          # organic can continue the folic run (it may be an F or H)
          desig <- vapply(parsed, length, integer(1)) > 0L
          may_folic <- folic | (!desig & !(organic %in% FALSE) & !rock & !lk$water)
          n_hi <- if (any(desig)) sum(cumprod(may_folic[ord])) else n_folic
          by_run <- function(n_run) {
            folic_cm <- sum(thick[ord[seq_len(n_run)]])
            mineral_cm <- 0; on_rock <- FALSE; unknown_between <- FALSE
            for (i in ord[seq_along(ord) > n_run]) {
              if (rock[i]) { on_rock <- TRUE; break }
              if (identical(organic[i], FALSE)) mineral_cm <- mineral_cm + thick[i]
              else if (is.na(organic[i])) {
                unknown_between <- TRUE; mineral_cm <- mineral_cm + thick[i]
              } else break
            }
            over_rock <- function(rock_here) {
              if (!rock_here) return(FALSE)
              .and3(folic_cm >= 10,
                    .any3(c(mineral_cm == 0, mineral_cm < 20 && folic_cm > 2 * mineral_cm)))
            }
            lo <- .any3(c(folic_cm > 40, over_rock(on_rock && !unknown_between)))
            hi <- .any3(c(folic_cm > 40, over_rock(on_rock)))
            if (identical(lo, hi)) lo else NA
          }
          f_lo <- by_run(n_folic); f_hi <- by_run(n_hi)
          by_folic <- if (identical(f_lo, f_hi)) f_lo else NA
          if (isTRUE(by_folic) || isTRUE(by_peat)) TRUE
          else if (is.na(by_folic) || is.na(by_peat)) NA else FALSE
        } else by_peat
  # option 2: a mineral layer within 40 cm of the surface
  mineral_within_40 <- any(organic %in% FALSE & !is.na(h$top_cm) & h$top_cm - top0 < 40)
  in80 <- pmax(0, pmin(h$bottom_cm, top0 + 80) - pmax(h$top_cm, top0))
  # a layer with no bottom depth may fill the rest of the upper 80 cm
  open80 <- ifelse(is.na(in80) & !is.na(h$top_cm) & h$top_cm < top0 + 80,
                   top0 + 80 - pmax(h$top_cm, top0), 0)
  in80[is.na(in80)] <- 0
  org80 <- sum(in80[organic %in% TRUE])
  org80_hi <- org80 + sum(open80[!(organic %in% FALSE)]) + sum(in80[is.na(organic)])
  under_mineral <- if (identical(organic[s], FALSE) && !is.na(thick[s]) && thick[s] < 40) {
    tot <- 0
    for (i in ord[-1L]) if (isTRUE(organic[i])) tot <- tot + thick[i] else break
    tot >= 40
  } else TRUE
  b2 <- .and3(mineral_within_40,
              if (org80 > 40) TRUE else if (org80_hi > 40) NA else FALSE,
              under_mineral)
  unknown <- all(is.na(organic))
  passed <- if (isTRUE(b1) || isTRUE(b2)) TRUE
            else if (unknown || is.na(b1) || is.na(b2)) NA else FALSE
  .cssc_result("organic_soil_cssc", passed, which(organic %in% TRUE),
               list(route = if (any(!is.na(h$oc_pct))) "properties" else "designations",
                    notation = pp$notation$why,
                    organic_from_surface_cm = cont, organic_in_upper_80_cm = org80,
                    surface_fibric = surf_fibric, surface_folic = surf_folic),
               if (is.na(passed)) c("oc_pct", "designation", "fiber_content_rubbed_pct",
                                    "bulk_density_g_cm3") else character(0),
               "Ch 3 key B, pp. 33-34; Ch 2, pp. 17-20; Ch 9, p. 97")
}


# ---- Cryosolic order (Ch 3 key A, p. 33) ------------------------------------

#' Permafrost for the Cryosolic order (CSSC)
#'
#' Permafrost (a horizon at or below 0 C, or a z suffix in the Canadian
#' notation) within 100 cm of the surface, or within 200 cm when the pedon is
#' strongly cryoturbated (a y suffix). As in USDA's
#' \code{permafrost_within_usda()}, the absence of any sign of permafrost is
#' read as none, with \code{permafrost_temp_C} listed as missing.
#' @noRd
cryosolic_cssc <- function(pedon) {
  h <- pedon$horizons
  pp <- .cssc_parsed(pedon)
  z <- .cssc_has_suffix(pp$parsed, "z", weak = "any") %in% TRUE
  y <- any(.cssc_has_suffix(pp$parsed, "y", weak = "any") %in% TRUE)
  frozen <- (!is.na(h$permafrost_temp_C) & h$permafrost_temp_C <= 0) | z
  limit <- if (y) 200 else 100
  rows <- which(frozen & !is.na(h$top_cm) & h$top_cm - min(h$top_cm, na.rm = TRUE) <= limit)
  .cssc_result("cryosolic_cssc", length(rows) > 0L, rows,
               list(route = if (any(!is.na(h$permafrost_temp_C))) "properties" else "designations",
                    notation = pp$notation$why, cryoturbated = y, depth_limit_cm = limit),
               if (all(is.na(h$permafrost_temp_C)) && length(rows) == 0L)
                 "permafrost_temp_C" else character(0),
               "Ch 3 key A, p. 33; Ch 2, p. 16 (y, z); Ch 6, p. 73")
}


# ---- Chernozemic A horizon (Ch 2, p. 16) -------------------------------------

# Soil climate of the chernozemic A (criterion 7): mean annual soil
# temperature >= 0 C and a moisture subclass drier than humid. Read from the
# site fields mean_annual_soil_temp_C and soil_moisture_subclass (CSSC
# vocabulary, Table 4, p. 143; the aquic subclasses belong to a separate
# scale and leave the criterion undecided); failing those, approximated from
# the USDA soil_temperature_regime (pergelic or gelic fails) and
# soil_moisture_regime (aridic, torric, ustic or xeric passes; perudic fails;
# udic and aquic, which straddle the Canadian subhumid-humid limit or lie on
# the other scale, leave it undecided).
.cssc_chernozemic_climate <- function(site) {
  t <- suppressWarnings(as.numeric(site$mean_annual_soil_temp_C %||% NA)[1L])
  temp_ok <- if (!is.na(t)) t >= 0 else {
    r <- tolower(as.character(site$soil_temperature_regime %||% NA)[1L])
    if (is.na(r)) NA else if (grepl("pergelic|gelic", r)) FALSE else TRUE
  }
  m <- gsub("[^a-z]", "", tolower(as.character(site$soil_moisture_subclass %||% NA)[1L]))
  if (!is.na(m) && !nzchar(m)) m <- NA_character_
  moist_ok <- if (!is.na(m)) {
    if (m %in% c("subhumid", "semiarid", "subarid", "arid")) TRUE
    else if (m %in% c("humid", "perhumid")) FALSE else NA
  } else {
    r <- tolower(as.character(site$soil_moisture_regime %||% NA)[1L])
    if (is.na(r)) NA
    else if (grepl("arid|torric|ustic|xeric", r)) TRUE
    else if (grepl("perudic", r)) FALSE else NA
  }
  list(passed = if (identical(temp_ok, FALSE) || identical(moist_ok, FALSE)) FALSE
                else if (isTRUE(temp_ok) && isTRUE(moist_ok)) TRUE else NA,
       temperature = temp_ok, moisture = moist_ok)
}

#' Chernozemic A horizon (CSSC)
#'
#' The Ah, Ahe or Ap at the mineral surface (or the uppermost mineral horizon
#' when there are no designations) meeting the seven criteria of Ch 2, p. 16:
#' at least 10 cm thick; dry value < 5.5, moist value < 3.5 and moist chroma <
#' 3.5; at least one value unit darker than the parent material (the IC: the
#' lowest C of the first material, when its colour is recorded); organic C
#' 1-17 % and C:N < 17; base saturation > 80 % with Ca the dominant cation;
#' not massive and hard nor single-grained when that is recorded; and a soil
#' climate of at least 0 C and drier than humid.
#' @noRd
chernozemic_a_cssc <- function(pedon) {
  x <- .cssc_horizons(pedon); h <- x$h
  if (is.na(x$ms)) return(.cssc_result("chernozemic_a_cssc", NA,
                                       missing = c("designation", "oc_pct"),
                                       where = "Ch 2, p. 16"))
  ahp <- .cssc_is(x$parsed, "A", "h", "any") | .cssc_is(x$parsed, "A", "p", "any")
  ord <- order(h$top_cm)
  rows <- integer(0); depth <- x$ms
  for (i in ord) {
    if (is.na(h$top_cm[i]) || h$top_cm[i] < x$ms) next
    a_here <- if (is.na(ahp[i])) length(rows) == 0L else ahp[i]
    if (isTRUE(a_here) && isTRUE(abs(h$top_cm[i] - depth) < 0.01)) {
      rows <- c(rows, i); depth <- h$bottom_cm[i]
    } else break
  }
  if (length(rows) == 0L) {
    return(.cssc_result("chernozemic_a_cssc", if (!any(x$desig_known)) NA else FALSE,
                        evidence = list(route = "designations",
                                        reason = "no Ah, Ahe or Ap at the mineral surface"),
                        missing = if (!any(x$desig_known)) "designation" else character(0),
                        where = "Ch 2, p. 16"))
  }
  # thickness-weighted means; a horizon with no bottom (the deepest) has an
  # unknown weight, so the criteria are read with it weighing nothing and
  # weighing everything, and decided only where both agree
  w0 <- pmax(0, x$thick[rows]); open_a <- anyNA(w0)
  thick <- sum(w0, na.rm = TRUE)
  # the parent material: the lowest C of the first material (IC), not a IIC
  is_c <- .cssc_is(x$parsed, "C", weak = "any")
  roman <- vapply(x$parsed, function(cmp) if (length(cmp)) cmp[[1]]$roman else "", "")
  ic <- which(is_c %in% TRUE & roman %in% c("", "I"))
  ic <- if (length(ic)) ic[length(ic)] else NA_integer_
  st <- tolower(as.character(h$structure_type[rows]))
  rr <- tolower(as.character(h$rupture_resistance[rows]))
  # each threshold is read on its own (a mean moves monotonically between the
  # two weights), then the criteria join them three-valued
  atoms <- function(w) {
    wmean <- function(v) { ok <- !is.na(v) & w > 0
      if (!any(ok)) NA_real_ else sum(v[ok] * w[ok]) / sum(w[ok]) }
    lt <- function(a, b) if (is.na(a) || is.na(b)) NA else a < b
    val_dry <- wmean(h$munsell_value_dry[rows]); val_moist <- wmean(h$munsell_value_moist[rows])
    chr_moist <- wmean(h$munsell_chroma_moist[rows])
    oc <- wmean(h$oc_pct[rows]); n <- wmean(h$n_total_pct[rows])
    bs <- wmean(h$bs_pct[rows]); ca <- wmean(h$ca_cmol[rows])
    others <- c(mg = wmean(h$mg_cmol[rows]), k = wmean(h$k_cmol[rows]),
                na = wmean(h$na_cmol[rows]), al = wmean(h$al_cmol[rows]))
    pv <- if (is.na(ic)) NA_real_ else h$munsell_value_moist[ic]
    c(list(vd = lt(val_dry, 5.5), vm = lt(val_moist, 3.5), cm = lt(chr_moist, 3.5),
           darker = if (is.na(pv) || is.na(val_moist)) TRUE else val_moist <= pv - 1,
           oc_lo = if (is.na(oc)) NA else oc >= 1, oc_hi = if (is.na(oc)) NA else oc <= 17,
           cn = if (is.na(oc) || is.na(n) || n <= 0) NA else oc / n < 17,
           bs = if (is.na(bs)) NA else bs > 80),
      # Ca the dominant cation: each other cation recorded is not above it
      lapply(others, function(o) if (is.na(ca) || is.na(o)) TRUE else ca >= o))
  }
  at <- if (!open_a) atoms(w0) else {
    light <- ifelse(is.na(w0), 0, w0); heavy <- ifelse(is.na(w0), 1e6, w0)
    ab <- atoms(heavy)
    if (sum(light) == 0) ab else Map(function(a, b) if (identical(a, b)) a else NA, atoms(light), ab)
  }
  pc <- list(
    colour = .and3(at$vd, at$vm, at$cm),
    darker_than_parent = at$darker,
    organic_c = .and3(at$oc_lo, at$oc_hi),
    c_to_n = at$cn,
    base_saturation = .and3(at$bs, at$mg, at$k, at$na, at$al))
  crit <- c(list(thickness = if (thick >= 10) TRUE else if (open_a) NA else FALSE),
            pc,
            list(not_massive_or_loose = !any(grepl("single", st)) &&
                   !any(grepl("massive", st) & rr %in% c("hard", "very hard", "extremely hard")),
                 climate = .cssc_chernozemic_climate(pedon$site)$passed))
  vals <- unlist(crit)
  passed <- if (any(vals %in% FALSE)) FALSE else if (all(vals %in% TRUE)) TRUE else NA
  miss <- c(colour = "munsell_value_dry", organic_c = "oc_pct", c_to_n = "n_total_pct",
            base_saturation = "bs_pct",
            climate = "site$mean_annual_soil_temp_C / site$soil_moisture_subclass")
  .cssc_result("chernozemic_a_cssc", passed, rows,
               c(list(route = "properties", notation = x$notation$why,
                      thickness_cm = thick), crit),
               if (is.na(passed)) unname(miss[intersect(names(crit)[is.na(vals)], names(miss))])
               else character(0),
               "Ch 2, p. 16; Ch 5, pp. 61-62")
}

#' The Ae conditions of the Chernozemic order (CSSC key G, p. 34)
#'
#' Every Ae or Aej (not an Ahe) must meet one of: an Aej with a dry value
#' below 5 (G.2); an Ae or Aej thinner than the Ah or Ap above it that does
#' not look eluviated (G.3); an Ae or Aej no more than 5 cm thick under an
#' Ahe (G.4). No Ae at all also passes (G.1). An Aej with no dry value that
#' meets neither G.3 nor G.4 leaves the test undecided. Read from the
#' designations; in another notation an E (not a buried one) is read as an Ae
#' of unknown strength under the A horizons above it: G.3 passes it, a dry
#' value of 5 or more fails it (as an Ae or as an Aej), and anything else
#' leaves it undecided.
#' @noRd
chernozemic_ae_cssc <- function(pedon) {
  x <- .cssc_horizons(pedon); h <- x$h
  raw <- .cssc_raw_master(x)
  if (!any(x$desig_known) && all(is.na(raw))) {
    return(.cssc_result("chernozemic_ae_cssc", NA, missing = "designation",
                        where = "Ch 3 key G, p. 34"))
  }
  thick <- x$thick
  above_of <- function(i) which(!is.na(h$bottom_cm) & h$bottom_cm <= h$top_cm[i] + 0.01)
  if (!any(x$desig_known)) {
    buried <- grepl("^[0-9]*[A-Z]+[a-z0-9]*b", .cssc_other_case(trimws(as.character(h$designation))))
    a_rows_o <- raw %in% "A" & !buried
    ae_rows <- which(raw %in% "E" & !buried)
    each <- vapply(ae_rows, function(i) {
      a_above <- above_of(i)[a_rows_o[above_of(i)]]
      if (length(a_above) > 0L && !is.na(thick[i]) &&
          thick[i] < sum(thick[a_above], na.rm = TRUE)) return(TRUE)
      dv <- h$munsell_value_dry[i]
      if (!is.na(dv) && dv >= 5) FALSE else NA
    }, logical(1))
  } else {
    has_h <- .cssc_is(x$parsed, "A", "h", "any") %in% TRUE
    ae_any <- (.cssc_is(x$parsed, "A", "e", "any") %in% TRUE) & !has_h
    ae_weak <- ae_any & (.cssc_is(x$parsed, "A", "e", "yes") %in% TRUE)
    ahe <- .cssc_is(x$parsed, "A", c("h", "e"), "any") %in% TRUE
    ahp_plain <- (has_h | .cssc_is(x$parsed, "A", "p", "any") %in% TRUE) & !ahe
    ae_rows <- which(ae_any)
    each <- vapply(ae_rows, function(i) {
      above <- above_of(i)
      a_above <- above[ahp_plain[above]]
      g3 <- length(a_above) > 0L && !is.na(thick[i]) &&
            thick[i] < sum(thick[a_above], na.rm = TRUE)
      g4 <- any(ahe[above]) && !is.na(thick[i]) && thick[i] <= 5
      if (g3 || g4) return(TRUE)
      if (ae_weak[i]) {
        dv <- h$munsell_value_dry[i]
        return(if (is.na(dv)) NA else dv < 5)
      }
      FALSE
    }, logical(1))
  }
  passed <- if (length(ae_rows) == 0L) TRUE
            else if (any(each %in% FALSE)) FALSE
            else if (all(each %in% TRUE)) TRUE else NA
  .cssc_result("chernozemic_ae_cssc", passed, ae_rows,
               list(route = "designations", notation = x$notation$why,
                    ae_horizons = ae_rows),
               if (is.na(passed)) c("munsell_value_dry", "designation") else character(0),
               "Ch 3 key G, p. 34")
}


# ---- Brunisolic order (Ch 3 key I, p. 34) -----------------------------------

#' Brunisolic B horizons (CSSC key I)
#'
#' Bm, Btj or Bfj horizons at least 5 cm thick in all, or a Bf thinner than
#' 10 cm; read from the designations (the book defines these horizons by
#' alteration and weak illuviation, without measurable limits).
#' @noRd
brunisolic_b_cssc <- function(pedon) {
  x <- .cssc_horizons(pedon)
  bm  <- .cssc_is(x$parsed, "B", "m", "any")
  btj <- .cssc_is(x$parsed, "B", "t", "yes")
  bfj <- .cssc_is(x$parsed, "B", "f", "yes")
  bf  <- .cssc_is(x$parsed, "B", "f", "no")
  weak_b <- (bm %in% TRUE) | (btj %in% TRUE) | (bfj %in% TRUE)
  # depth covered (overlapping rows once); the deepest horizon with no bottom
  # may go on
  cw <- .cssc_cover(x, which(weak_b)); cb <- .cssc_cover(x, which(bf %in% TRUE))
  t_weak <- cw$cm; t_bf <- cb$cm
  open_weak <- cw$open; open_bf <- cb$open
  passed <- if (t_weak >= 5 || (t_bf > 0 && t_bf < 10 && !open_bf)) TRUE
            else if (open_weak || (open_bf && t_bf < 10) || !any(x$desig_known)) NA else FALSE
  .cssc_result("brunisolic_b_cssc", passed, which(weak_b | (bf %in% TRUE)),
               list(route = "designations", notation = x$notation$why,
                    bm_btj_bfj_cm = t_weak, bf_cm = t_bf),
               if (is.na(passed)) "designation" else character(0),
               "Ch 3 key I, p. 34; Ch 2, p. 14")
}
