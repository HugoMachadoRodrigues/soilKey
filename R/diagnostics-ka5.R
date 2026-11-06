# =============================================================================
# v0.9.362 -- the predicates of the KA5 key to the type (Tab. 51, pp.
# 283-288; Hugo's decisions Q5-Q8, Q15, Q17-Q20, Q22, Q23).
#
# Every predicate reads the horizon frame of identify_horizons(pedon, "ka5")
# (.ka5_identify(): the KA5 notation where the profile is written in it,
# else the order of tests on the data), with depths from the GOF (p. 79) and
# the carrying horizon of each row (the last symbol carries it, Q8: Bv-Go is
# a Go, Go-Bv a Bv). A row the data leave undecided counts by its
# candidates: a gate it could meet is undecided, never a silent FALSE. The
# gates follow Tab. 51 with the numbers of the type texts where they differ
# (Q6: S under 4 dm, Axh 4 dm or more, Organomarsch humus 8 % and clay 45 %,
# Gyttja rH 13 or more, Nassgley by its Gr, Ex in the Hortisol) and two
# rules of the figures (Q5: a Bv or Bt over a T, P, Bu or Bj reaches 1.5 dm;
# the Auenboeden have no B, P, T, E or R, and an S only under an aM).
# Predicates whose verdict holds only on the designation, a measurement
# missing, say so in `provisional`; classify_ka5() lists them.
# =============================================================================

.ka5_state <- new.env(parent = emptyenv())

# The view the predicates read: the frame, the profile and the parsed
# candidates; the one of the classification in progress, else built.
.ka5_view <- function(pedon) {
  if (!is.null(.ka5_state$view) && identical(.ka5_state$pedon, pedon)) return(.ka5_state$view)
  .ka5_make_view(pedon)
}

.ka5_make_view <- function(pedon) {
  fr <- .ka5_identify(pedon)
  x <- .ka5_profile(pedon)
  ord <- order(ifelse(is.na(fr$top_gof), Inf, fr$top_gof), seq_len(nrow(fr)))
  cand <- lapply(fr$candidates, function(cc) {
    cc <- unique(cc[!is.na(cc)])
    lapply(cc, function(s) {
      r <- tryCatch(.ka5_parse_one(s), error = function(e) NULL)
      if (is.null(r) || !isTRUE(r$known)) list(code = s, master = substr(sub("^[a-z]*", "", s), 1L, 1L), prefixes = character(0))
      else list(code = r$code, master = r$master, prefixes = r$prefixes)
    })
  })
  list(frame = fr, x = x, ord = ord, cand = cand, pedon = pedon)
}

# A predicate's answer.
.ka5_p <- function(passed, layers = integer(0), missing = character(0), notes = character(0),
                   provisional = FALSE, evidence = list()) {
  list(passed = if (length(passed) == 1L) as.logical(passed) else NA, layers = as.integer(layers),
       missing = unique(as.character(missing)), notes = unique(as.character(notes[!is.na(notes) & nzchar(notes)])),
       provisional = isTRUE(provisional), evidence = evidence)
}


# --- matching rows ---------------------------------------------------------------------

# The moor prefixes of the H and the rock prefixes of the C: a data row that
# writes none of a family leaves a test on it undecided.
.KA5_PREFIX_FAMILIES <- list(H = c("h", "n", "u"), C = c("i", "e", "c", "l", "m", "x"))

# Does a decided row (or a candidate) match a spec: `codes` (carrying codes),
# `pattern` (a regular expression on the carrying code), `masters`,
# `prefixes` (any of them), `with` (another part, e.g. Go in Go-Ah),
# `fossil` (f or r). TRUE, FALSE or NA (a prefix the data leave open).
.ka5_match1 <- function(code, master, prefixes, with = character(0), spec, data_row = FALSE,
                        prefixes_na = character(0), fossil = FALSE, relict = FALSE) {
  if (is.na(code)) return(FALSE)
  if (!is.null(spec$codes) && !code %in% unlist(spec$codes)) return(FALSE)
  if (!is.null(spec$pattern) && !grepl(spec$pattern, code)) return(FALSE)
  if (!is.null(spec$masters) && !isTRUE(master %in% unlist(spec$masters))) return(FALSE)
  if (!is.null(spec$with)) {
    wc <- sub("^[a-z]*", "", with)
    if (!any(wc %in% unlist(spec$with) | substr(wc, 1L, 2L) %in% unlist(spec$with))) return(FALSE)
  }
  if (isTRUE(spec$fossil) && !(fossil || relict)) return(FALSE)
  if (!is.null(spec$prefixes)) {
    want <- unlist(spec$prefixes)
    if (any(want %in% prefixes)) return(TRUE)
    if (any(want %in% prefixes_na)) return(NA)
    fam <- .KA5_PREFIX_FAMILIES[[master %||% ""]]
    if (data_row && length(fam) && all(want %in% fam) && !any(fam %in% prefixes)) return(NA)
    return(FALSE)
  }
  TRUE
}

# Per row of the view: TRUE (the row is such a horizon), FALSE, or NA (an
# undecided row with such a candidate, or a prefix left open).
.ka5_rows <- function(v, spec, buried = FALSE) {
  fr <- v$frame
  bur <- if (is.null(fr$buried)) integer(nrow(fr)) else as.integer(fr$buried)
  vapply(seq_len(nrow(fr)), function(i) {
    # a buried soil (from the first fossil row of a layer II down) does not
    # decide the present soil (D1, p. 283)
    if (!buried && isTRUE(bur[i] > 0L)) return(FALSE)
    dr <- identical(fr$route[i], "data")
    if (!is.na(fr$carrying[i]))
      return(.ka5_match1(fr$carrying[i], fr$master[i], fr$prefixes[[i]], fr$with[[i]], spec, dr,
                         fr$prefixes_na[[i]], isTRUE(fr$fossil[i]), isTRUE(fr$relict[i])))
    if (!length(v$cand[[i]])) return(FALSE)
    m <- vapply(v$cand[[i]], function(cc) {
      r <- .ka5_match1(cc$code, cc$master, cc$prefixes, character(0), spec, TRUE)
      if (isTRUE(r)) NA else r
    }, NA)
    if (any(is.na(m))) NA else FALSE
  }, NA)
}

# A depth condition on rows: top < max_cm (<= when strict FALSE), top >=
# min_cm, bottom >= reaching_cm; NA where a depth is missing.
.ka5_depth_ok <- function(fr, i, max_cm = NULL, min_cm = NULL, reaching_cm = NULL, strict = TRUE,
                          reach_strict = FALSE) {
  t <- fr$top_gof[i]; b <- fr$bottom_gof[i]
  ok <- TRUE
  if (!is.null(max_cm)) ok <- .kidpr_and(ok, if (is.na(t)) NA else if (strict) t < max_cm else t <= max_cm)
  if (!is.null(min_cm)) ok <- .kidpr_and(ok, if (is.na(t)) NA else t >= min_cm)
  if (!is.null(reaching_cm)) {
    r <- if (is.na(b)) NA else if (reach_strict) b > reaching_cm else b >= reaching_cm
    # the description ends in the row: how deep it reaches is not known (D6)
    if (isFALSE(r) && .ka5_at_bottom(fr, i)) r <- NA
    ok <- .kidpr_and(ok, r)
  }
  ok
}

# Does a row reach the bottom of the description?
.ka5_at_bottom <- function(fr, i) {
  deepest <- suppressWarnings(max(fr$bottom_gof, na.rm = TRUE))
  is.finite(deepest) && isTRUE(fr$bottom_gof[i] >= deepest)
}

# The rows and verdict of "a horizon of the spec within the depths".
.ka5_within <- function(v, spec, max_cm = NULL, min_cm = NULL, reaching_cm = NULL, strict = TRUE,
                        buried = FALSE, reach_strict = FALSE) {
  fr <- v$frame
  m <- .ka5_rows(v, spec, buried = buried)
  d <- vapply(seq_len(nrow(fr)), function(i) .ka5_depth_ok(fr, i, max_cm, min_cm, reaching_cm, strict, reach_strict), NA)
  r <- mapply(function(a, b) .kidpr_and(a, b), m, d)
  list(passed = if (!length(r)) FALSE else .kidpr_or(r), rows = which(r %in% TRUE), na_rows = which(is.na(r)))
}

# The attributes that would decide the undecided rows.
.ka5_rows_missing <- function(v, rows) unique(unlist(v$frame$missing[rows]))

# The run of rows from the GOF down that match a spec, and its thickness.
.ka5_top_run <- function(v, spec, start_max_cm = NULL) {
  fr <- v$frame
  m <- .ka5_rows(v, spec)
  rows <- integer(0); th <- 0; und <- FALSE; started <- is.null(start_max_cm)
  for (i in v$ord) {
    if (isTRUE(fr$bottom_gof[i] <= 0) || isTRUE(fr$buried[i] > 0L)) next
    # the run may start at the first such row with its top under
    # start_max_cm (D4: an Axh under an Ap or an M)
    if (!started) {
      if (isFALSE(m[i])) { if (isTRUE(fr$top_gof[i] >= start_max_cm)) break else next }
      if (!isTRUE(fr$top_gof[i] < start_max_cm)) break
      started <- TRUE
    }
    if (isFALSE(m[i])) break
    if (is.na(m[i])) und <- TRUE
    if (is.na(fr$top_gof[i]) || is.na(fr$bottom_gof[i])) und <- TRUE
    rows <- c(rows, i)
    th <- th + max(0, fr$bottom_gof[i] - max(0, fr$top_gof[i]), na.rm = TRUE)
  }
  list(rows = rows, thickness = th, undecided = und,
       at_bottom = length(rows) > 0L && .ka5_at_bottom(fr, rows[length(rows)]))
}

# The first rows of the view at or below the GOF (depth order).
.ka5_rows_from_gof <- function(v) v$ord[!(v$frame$bottom_gof[v$ord] <= 0) %in% TRUE & !(v$frame$buried[v$ord] > 0L) %in% TRUE]

# Is a row a horizon of these masters by its carrying code (decided rows
# only)?
.ka5_is <- function(v, i, masters) isTRUE(v$frame$master[i] %in% masters)


# --- generic predicates -------------------------------------------------------------

#' A KA5 horizon present within a depth (Tab. 51)
#'
#' TRUE when a row of the KA5 horizon frame whose carrying horizon matches
#' (\code{codes}, \code{pattern}, \code{masters}, \code{prefixes},
#' \code{with}, \code{fossil}) has its top less than \code{max_cm} under the
#' GOF (and at least \code{min_cm}) and reaches \code{reaching_cm}; NA when
#' only an undecided row could. Horizons by the last symbol (Hugo's decision
#' Q8).
#' @param pedon A PedonRecord.
#' @param codes,pattern,masters,prefixes,with,fossil The horizon.
#' @param max_cm,min_cm,reaching_cm Depths from the GOF, cm.
#' @param strict \code{max_cm} exclusive (default) or inclusive.
#' @return A list with \code{passed}, \code{layers}, \code{missing},
#'   \code{notes}.
#' @noRd
horizon_within_ka5 <- function(pedon, codes = NULL, pattern = NULL, masters = NULL, prefixes = NULL,
                               with = NULL, fossil = NULL, max_cm = NULL, min_cm = NULL,
                               reaching_cm = NULL, strict = TRUE) {
  v <- .ka5_view(pedon)
  spec <- list(codes = codes, pattern = pattern, masters = masters, prefixes = prefixes, with = with, fossil = fossil)
  w <- .ka5_within(v, spec, max_cm, min_cm, reaching_cm, strict)
  what <- paste(c(unlist(prefixes), unlist(codes), unlist(masters), pattern), collapse = "/")
  .ka5_p(w$passed, layers = w$rows, missing = .ka5_rows_missing(v, w$na_rows),
         notes = sprintf("%s%s%s%s: %s", what, if (!is.null(max_cm)) sprintf(" with its top under %s cm", max_cm) else "",
                         if (!is.null(min_cm)) sprintf(" at %s cm or deeper", min_cm) else "",
                         if (!is.null(reaching_cm)) sprintf(" reaching %s cm", reaching_cm) else "",
                         if (isTRUE(w$passed)) "present" else if (is.na(w$passed)) "undecided" else "absent"))
}

#' The thickness of the run of KA5 horizons from the GOF down
#'
#' The rows from the GOF down whose carrying horizon matches, summed: TRUE
#' when the run is at least \code{min_cm} (and under \code{max_cm}) thick.
#' @param min_cm,max_cm Thickness limits, cm.
#' @return A list with \code{passed}, \code{layers}, \code{missing}, \code{notes}.
#' @noRd
top_thickness_ka5 <- function(pedon, codes = NULL, pattern = NULL, masters = NULL, prefixes = NULL,
                              min_cm = 0, max_cm = Inf, start_max_cm = NULL) {
  v <- .ka5_view(pedon)
  spec <- list(codes = codes, pattern = pattern, masters = masters, prefixes = prefixes)
  r <- .ka5_top_run(v, spec, start_max_cm)
  ok <- length(r$rows) > 0L && r$thickness >= min_cm && r$thickness < max_cm
  p <- if (ok && !r$undecided) TRUE else if (ok || (r$undecided && r$thickness >= min_cm)) NA else FALSE
  # the run reaches the bottom of the description short of the minimum (D6)
  if (isFALSE(p) && length(r$rows) && r$thickness < min_cm && isTRUE(r$at_bottom)) p <- NA
  .ka5_p(p, layers = r$rows, missing = .ka5_rows_missing(v, r$rows),
         notes = sprintf("%s from the GOF: %s cm (%s to < %s)", paste(c(unlist(prefixes), unlist(codes), unlist(masters), pattern), collapse = "/"),
                         format(r$thickness), format(min_cm), format(max_cm)))
}


#' The summed thickness of the KA5 horizons of a kind
#'
#' Every row whose carrying horizon matches (\code{codes}, \code{pattern},
#' \code{masters}), or that holds a part in \code{with} (Go-Aa for an Aa),
#' summed: TRUE when at least \code{min_cm} and under \code{max_cm}.
#' @noRd
thickness_ka5 <- function(pedon, codes = NULL, pattern = NULL, masters = NULL, with = NULL,
                          min_cm = 0, max_cm = Inf) {
  v <- .ka5_view(pedon)
  fr <- v$frame
  a <- if (is.null(codes) && is.null(pattern) && is.null(masters)) rep(FALSE, nrow(fr))
       else .ka5_rows(v, list(codes = codes, pattern = pattern, masters = masters))
  b <- if (is.null(with)) rep(FALSE, nrow(fr)) else .ka5_rows(v, list(with = with))
  m <- mapply(function(p, q) .kidpr_or(p, q), a, b)
  th <- function(k) sum(pmax(0, fr$bottom_gof[k] - pmax(0, fr$top_gof[k])), na.rm = TRUE)
  sure <- th(which(m %in% TRUE)); maybe <- th(which(m %in% c(TRUE, NA)))
  p <- if (sure >= min_cm && maybe < max_cm) TRUE
       else if (maybe < min_cm || sure >= max_cm) FALSE else NA
  .ka5_p(p, layers = which(m %in% TRUE), missing = .ka5_rows_missing(v, which(is.na(m))),
         notes = sprintf("%s %s cm thick (%s to < %s)", paste(c(unlist(codes), unlist(masters), pattern, unlist(with)), collapse = "/"),
                         if (sure == maybe) format(sure) else sprintf("%s-%s", format(sure), format(maybe)), format(min_cm), format(max_cm)))
}


# --- divisions (p. 284; Q7) ---------------------------------------------------------

#' The Moore: H horizons 3 dm thick or more at the GOF or under a mineral
#' cover of less than 4 dm (Hugo's decision Q7; a cover of 2 to < 4 dm is
#' an overlay, named with the subtypes).
#' @param min_cm Thickness of the peat body.
#' @param cover_max_cm The mineral cover the Moor keeps.
#' @noRd
moor_ka5 <- function(pedon, min_cm = 30, cover_max_cm = 40) {
  v <- .ka5_view(pedon)
  fr <- v$frame
  # the peat body: the H rows from the first H, thin interbeds (10 cm or
  # less, a buried fF included) not breaking it (D7, p. 284)
  m <- .ka5_rows(v, list(masters = "H"), buried = TRUE)
  first <- v$ord[m[v$ord] %in% c(TRUE, NA)][1L]
  if (is.na(first)) return(.ka5_p(FALSE, notes = "no H horizon"))
  if (is.na(fr$top_gof[first])) return(.ka5_p(NA, missing = "top_cm", notes = "the depth of the first H not recorded"))
  if (!isTRUE(fr$top_gof[first] < cover_max_cm)) return(.ka5_p(FALSE, notes = sprintf("the peat begins at %s cm (4 dm or more)", format(fr$top_gof[first]))))
  th <- 0; und <- FALSE; rows <- integer(0); pend <- integer(0)
  lim <- .ka5_thr("moor_interbed_max_cm")
  seq_rows <- v$ord[which(v$ord == first):length(v$ord)]
  for (i in seq_rows) {
    d <- fr$bottom_gof[i] - fr$top_gof[i]
    if (isFALSE(m[i])) {
      if (isTRUE(d <= lim)) { pend <- c(pend, i); next }
      break
    }
    if (is.na(d)) und <- TRUE
    if (is.na(m[i])) und <- TRUE
    rows <- c(rows, i)
    th <- th + max(0, d, na.rm = TRUE)
  }
  cover <- fr$top_gof[first]
  p <- if (th >= min_cm) (if (und) NA else TRUE) else if (und || (length(rows) && .ka5_at_bottom(fr, rows[length(rows)]))) NA else FALSE
  .ka5_p(p, layers = rows, missing = .ka5_rows_missing(v, rows),
         notes = c(sprintf("peat %s cm thick from %s cm", format(th), format(cover)),
                   if (length(pend) && length(rows) > 1L) "thin interbeds of 10 cm or less do not break the peat body (D7)",
                   if (isTRUE(p %in% NA) && length(rows) && .ka5_at_bottom(fr, rows[length(rows)])) "the peat reaches the bottom of the description (D6)",
                   if (isTRUE(cover >= 20)) sprintf("a mineral cover of %s cm keeps the Moor (Q7): an overlay of %s", format(cover),
                                                     if (cover < 30) "\"flach ... \u00fcber\" (X\\HN)" else "\"... \u00fcber\" (X/HN)")))
}


#' The semisubhydric and subhydric soils: under water or in the tidal zone,
#' and a solum of F horizons (p. 284).
#' @param kind NULL (either), "inland" (class J) or "tidal" (class I).
#' @noRd
subhydric_ka5 <- function(pedon, kind = NULL) {
  v <- .ka5_view(pedon)
  fr <- v$frame
  first <- .ka5_rows_from_gof(v)[1L]
  if (is.na(first)) return(.ka5_p(FALSE))
  f <- .ka5_rows(v, list(masters = "F"))[first]
  if (isFALSE(f)) return(.ka5_p(FALSE, notes = "no F horizon at the GOF"))
  where <- .ka5_subaqueous(v$x)
  # a tidal or salt prefix (tm, tb, tp, z) on the F at the GOF is tidal
  # evidence; otherwise inland or tidal stays undecided (D14)
  tidal_pre <- any(fr$prefixes[[first]] %in% c("tm", "tb", "tp", "z"))
  w <- if (!is.na(where)) where else if (tidal_pre) "tidal" else NA_character_
  p <- f
  if (!is.null(kind)) p <- .kidpr_and(p, if (is.na(w)) NA else w == kind)
  .ka5_p(p, layers = first, missing = if (is.na(w)) c("subaqueous", "tidal_zone"),
         notes = sprintf("an F horizon at the GOF; %s", if (is.na(w)) "inland or tidal water undecided"
                         else if (w == "tidal") "in the tidal zone" else "under inland water"))
}

# Tidal: a tm, tb, tp or z prefix on a row of the present solum (top under
# 4 dm, not buried: D2, D14), or the site in the tidal zone.
.ka5_tidal <- function(v, max_cm = 40) {
  fr <- v$frame
  rows <- .ka5_rows_from_gof(v)
  rows <- rows[!is.na(fr$top_gof[rows]) & fr$top_gof[rows] < max_cm]
  pre <- any(vapply(rows, function(i) any(fr$prefixes[[i]] %in% c("tm", "tb", "tp", "z")), NA))
  if (pre) return(TRUE)
  tz <- .ka5_site_txt(v$x, "tidal_zone")
  if (isTRUE(tz %in% c("intertidal", "supratidal"))) return(TRUE)
  FALSE
}

# A floodplain soil: an aX horizon (prefix a), or the site in a floodplain.
.ka5_auen <- function(v) {
  fr <- v$frame
  any(vapply(fr$prefixes, function(p) "a" %in% p, NA)) ||
    (nrow(fr) > 0L && .ka5_floodplain(v$x, v$ord[1L]))
}

# The Auenboeden of the figure (Q5): no B, P, T, E or R horizon; an S only
# under an aM (v0.9.364: an aM anywhere above it, or an M part in it, as the
# printed Pseudogley-Vega aAh/aM/aSw/aSd and a(Sw-)Ah/aSw-M/a(M-)Sd write it,
# p. 241).
.ka5_auen_ok <- function(v) {
  fr <- v$frame
  bad <- .ka5_rows(v, list(masters = c("B", "P", "T", "E", "R")))
  s <- .ka5_rows(v, list(masters = "S"))
  am <- vapply(seq_len(nrow(fr)), function(j) (isTRUE(fr$carrying[j] == "M") && "a" %in% fr$prefixes[[j]]) ||
                 any(grepl("M", fr$with[[j]])), NA)
  s_ok <- vapply(which(s %in% c(TRUE, NA)), function(i) {
    above <- which(!is.na(fr$top_gof) & !is.na(fr$top_gof[i]) & fr$top_gof < fr$top_gof[i])
    any(grepl("M", fr$with[[i]])) || any(am[above])
  }, NA)
  ok <- .kidpr_and(!bad)
  .kidpr_and(ok, if (length(s_ok)) all(s_ok) else TRUE)
}

#' The semiterrestrial soils (p. 285; Q5, Q8): a G horizon (by its last
#' symbol) with its top under 4 dm; or tidal horizons (tm, tb, tp); or the
#' aX horizons of a floodplain with no B, P, T, E or R (an S only under an
#' aM).
#' @noRd
semiterrestrial_ka5 <- function(pedon, max_cm = 40) {
  v <- .ka5_view(pedon)
  g <- .ka5_within(v, list(masters = "G"), max_cm = max_cm)
  if (isTRUE(g$passed)) return(.ka5_p(TRUE, layers = g$rows, notes = "a G horizon under 4 dm"))
  if (.ka5_tidal(v)) return(.ka5_p(TRUE, notes = "tidal horizons (tm, tb, tp)"))
  if (.ka5_auen(v)) {
    ok <- .ka5_auen_ok(v)
    if (!isFALSE(ok)) return(.ka5_p(.kidpr_or(ok, g$passed), notes = "the aX horizons of a floodplain"))
    return(.ka5_p(g$passed, layers = g$rows, missing = .ka5_rows_missing(v, g$na_rows),
                  notes = "a floodplain soil with a B, P, T, E or R horizon: the terrestrial key, a variety Auen- (Q5)"))
  }
  .ka5_p(g$passed, layers = g$rows, missing = .ka5_rows_missing(v, g$na_rows),
         notes = if (is.na(g$passed)) "a G horizon under 4 dm undecided" else "no G horizon under 4 dm")
}


# --- the semiterrestrial classes and types (pp. 285-286) --------------------------------

#' The Strandboeden: tidal, a solum of Ai horizons (p. 285).
#' @noRd
strand_ka5 <- function(pedon) {
  v <- .ka5_view(pedon)
  if (!.ka5_tidal(v)) return(.ka5_p(FALSE, notes = "not tidal"))
  first <- .ka5_rows_from_gof(v)[1L]
  ai <- if (is.na(first)) FALSE else .ka5_rows(v, list(codes = "Ai"))[first]
  .ka5_p(ai, layers = first, notes = "tidal, the topsoil an Ai")
}

# The rows of the topsoil A run from the GOF.
.ka5_a_run <- function(v) .ka5_top_run(v, list(masters = "A"))

#' The Marschen: tidal, redoximorphic features to the surface or under the
#' Ah (p. 285).
#' @noRd
marsh_ka5 <- function(pedon) {
  v <- .ka5_view(pedon)
  if (!.ka5_tidal(v)) return(.ka5_p(FALSE, notes = "not tidal"))
  fr <- v$frame
  a <- .ka5_a_run(v)
  first <- .ka5_rows_from_gof(v)[1L]
  if (is.na(first)) return(.ka5_p(FALSE))
  # a G or S part in the topsoil, or a G or S row directly under it
  in_a <- any(vapply(a$rows, function(i) any(substr(sub("^[a-z]*", "", fr$with[[i]]), 1L, 1L) %in% c("G", "S")), NA))
  nxt <- if (length(a$rows)) .kidpr_next(v$x, a$rows[length(a$rows)]) else first
  under <- if (is.na(nxt)) FALSE else .ka5_rows(v, list(masters = c("G", "S")))[nxt]
  p <- .kidpr_or(in_a, under)
  .ka5_p(p, layers = c(a$rows, nxt), notes = "tidal; redoximorphic to the surface or under the Ah")
}

#' The Rohmarsch: Go features reaching into the Ah (Go-Ah), unless the site
#' is embanked or not tidally flooded (Hugo's decision Q20).
#' @noRd
rohmarsch_ka5 <- function(pedon) {
  v <- .ka5_view(pedon)
  fr <- v$frame
  first <- .ka5_rows_from_gof(v)[1L]
  if (is.na(first)) return(.ka5_p(FALSE))
  goah <- .ka5_rows(v, list(masters = "A", with = "Go"))[first]
  emb <- .ka5_site_lgl(v$x, "embanked"); ti <- .ka5_site_lgl(v$x, "tidal_inundation")
  if (isTRUE(goah) && (isTRUE(emb) || isFALSE(ti)))
    return(.ka5_p(FALSE, notes = "a Go-Ah behind a dyke or not tidally flooded: no Rohmarsch (Q20)"))
  .ka5_p(goah, layers = first, notes = "Go features reaching into the Ah (Go-Ah)")
}

# Is a row carbonatic (2 % or more): the prefix e or c, or the data.
.ka5_carbonatic <- function(v, i) {
  fr <- v$frame
  if (any(fr$prefixes[[i]] %in% c("e", "c")) || any(grepl("^[a-z]*[ec][a-z]*[A-Z]", fr$with[[i]]))) return(TRUE)
  if (identical(fr$route[i], "notation")) {
    r <- .ka5_carbonate(v$x, i)$range
    return(if (anyNA(r)) FALSE else isTRUE(.ka5_at_least(r, .ka5_thr("carbonate_rock_pct")[1L])))
  }
  .ka5_at_least(.ka5_carbonate(v$x, i)$range, .ka5_thr("carbonate_rock_pct")[1L])
}

#' Carbonatic at least below the Ah (the Kalkmarsch, p. 250): a carbonatic
#' row (prefix e or c, 2 % or more) with its top under 4 dm, the complement
#' of the Kleimarsch's decalcification to 4 dm (D8, Q6).
#' @noRd
carbonate_below_ah_ka5 <- function(pedon, max_cm = 40) {
  d <- decalcified_ka5(pedon, min_cm = max_cm)
  .ka5_p(.kidpr_not(d$passed), layers = d$layers, missing = d$missing,
         notes = sprintf("a carbonatic horizon with its top under %s cm: %s", max_cm,
                         if (is.na(d$passed)) "undecided" else if (isFALSE(d$passed)) "yes" else "no"))
}

#' Decalcified to a depth (the Marschen, p. 285): no carbonatic row (prefix e
#' or c, 2 % or more) with its top under \code{min_cm}.
#' @param min_cm The depth of decalcification, cm from the GOF.
#' @noRd
decalcified_ka5 <- function(pedon, min_cm = 40) {
  v <- .ka5_view(pedon)
  fr <- v$frame
  rows <- .ka5_rows_from_gof(v)
  rows <- rows[fr$top_gof[rows] < min_cm]
  if (!length(rows)) return(.ka5_p(NA))
  cb <- vapply(rows, function(i) .ka5_carbonatic(v, i), NA)
  deepest <- suppressWarnings(max(fr$bottom_gof, na.rm = TRUE))
  p <- .kidpr_and(!cb, if (is.finite(deepest) && deepest >= min_cm) TRUE else NA)
  .ka5_p(p, layers = rows, missing = if (anyNA(cb)) c("caco3_pct", "carbonate_class_ka5"),
         notes = sprintf("decalcified to %s cm: %s", min_cm, if (is.na(p)) "undecided" else if (p) "yes" else "no"))
}

#' The Organomarsch (p. 253; Q6): humus 8 % or more and clay 45 % or more in
#' the topsoil, or the prefix o written.
#' @noRd
organomarsch_ka5 <- function(pedon) {
  v <- .ka5_view(pedon)
  fr <- v$frame
  a <- .ka5_a_run(v)
  if (!length(a$rows)) return(.ka5_p(FALSE, notes = "no topsoil"))
  if (any(vapply(a$rows, function(i) "o" %in% fr$prefixes[[i]], NA))) return(.ka5_p(TRUE, layers = a$rows, notes = "the prefix o written"))
  # a marsh written in the notation without the o: the MOn always writes it
  # (Hugo, 2026-11-06)
  if (all(fr$route[a$rows] %in% c("notation", "both")))
    return(.ka5_p(FALSE, layers = a$rows, notes = "written in the notation without the prefix o: no Organomarsch (Hugo, 2026-11-06)"))
  i <- a$rows[1L]
  os <- .ka5_os(v$x, i)$range; cl <- .ka5_num(v$x, i, "clay_pct")
  p <- .kidpr_and(.ka5_at_least(os, .ka5_thr("mo_humus_min_pct")), if (is.na(cl)) NA else cl >= .ka5_thr("mo_clay_min_pct"))
  .ka5_p(p, layers = i, missing = c(if (anyNA(os)) "oc_pct", if (is.na(cl)) "clay_pct"),
         notes = sprintf("topsoil organic substance %s %%, clay %s %% (8 and 45, p. 253)", .ka5_range_txt(os), format(cl)))
}


#' The Dwogmarsch (p. 252): a buried humus or Fe horizon (Dwog) of a layer II
#' with its top under 4 dm (a fossil row, or one overprinted with the degree
#' sign).
#' @noRd
dwog_ka5 <- function(pedon, max_cm = 40) {
  v <- .ka5_view(pedon)
  fr <- v$frame
  rows <- which(fr$layer >= 2L & (fr$fossil | vapply(fr$with, function(w) any(startsWith(w, "f")), NA) |
                                    vapply(fr$features, function(f) any(grepl("\u00b0", f, fixed = TRUE)), NA)))
  rows <- rows[fr$top_gof[rows] < max_cm]
  .ka5_p(length(rows) > 0L, layers = rows, notes = "a buried Dwog horizon of a layer II under 4 dm")
}

#' The Auenboeden (p. 286; Q5): aX horizons of a floodplain, no B, P, T, E
#' or R, an S only under an aM.
#' @noRd
floodplain_soil_ka5 <- function(pedon) {
  v <- .ka5_view(pedon)
  if (!.ka5_auen(v)) return(.ka5_p(FALSE, notes = "no aX horizon"))
  .ka5_p(.ka5_auen_ok(v), notes = "a floodplain soil without B, P, T, E or R")
}

#' A topsoil of one kind over a C (the Auenboeden AO, AQ, AZ; p. 286): the
#' first row an \code{a_code} and the row under the topsoil a C with the rock
#' prefix \code{rock} (i, e) or the given code.
#' @param a_codes The topsoil codes.
#' @param rock The rock prefix of the C, or NULL.
#' @noRd
topsoil_over_c_ka5 <- function(pedon, a_codes = c("Ah", "Ap"), rock = NULL, no_m_below = FALSE) {
  v <- .ka5_view(pedon)
  first <- .ka5_rows_from_gof(v)[1L]
  if (is.na(first)) return(.ka5_p(FALSE))
  a <- .ka5_rows(v, list(codes = a_codes))[first]
  nx <- .kidpr_next(v$x, first)
  c_ok <- if (is.na(nx)) NA else .ka5_rows(v, list(masters = "C", prefixes = rock))[nx]
  # an aM band in the C (ailC+aM, aM+aelC, p. 239) is the C of the Paternia
  # (Hugo, 2026-11-06, D1)
  if (isTRUE(no_m_below) && isFALSE(c_ok) && grepl("+", v$frame$code[nx], fixed = TRUE)) {
    pt <- v$frame$with[[nx]]
    cpre <- if (is.null(rock)) "[a-z]*" else sprintf("[a-z]*[%s][a-z]*", paste(unlist(rock), collapse = ""))
    if (any(grepl(sprintf("^%sC", cpre), pt))) c_ok <- TRUE
  }
  # v0.9.364: the Paternia-Vega (aAh/(II)ailC/II(III)aM/..., p. 241) is a
  # Vega: an aM under the ilC takes the profile out of the Paternia (the gate
  # mended for the subtypes)
  # Hugo (2026-11-06, D1): only an aM (prefix a) of the present soil
  # beginning above 8 dm; aM bands in the ilC (ailC+aM) keep the Paternia,
  # as the Kalkpaternia's aM+aelC (p. 239)
  if (isTRUE(no_m_below) && !is.na(nx) && !isFALSE(c_ok)) {
    fr <- v$frame
    rows <- .ka5_rows_from_gof(v)
    under <- rows[!is.na(fr$top_gof[rows]) & fr$top_gof[rows] > (fr$top_gof[nx] %||% Inf) &
                    fr$top_gof[rows] < .ka5_thr("st_auen_g_min_cm")]
    m <- .ka5_rows(v, list(masters = "M", prefixes = "a"))[under]
    band <- vapply(under, function(i) any(grepl("^[a-z]*C", fr$with[[i]])), NA)
    if (any(m %in% TRUE & !band))
      return(.ka5_p(FALSE, layers = c(first, nx), notes = "an aM under the ilC: a Paternia-Vega, not a Paternia (p. 241)"))
  }
  .ka5_p(.kidpr_and(a, c_ok), layers = c(first, nx), missing = if (!is.na(nx)) unlist(v$frame$missing[nx]),
         notes = sprintf("%s over a %sC", paste(a_codes, collapse = "/"), paste(rock, collapse = "")))
}


# --- the terrestrial classes (pp. 286-288) -------------------------------------------

# The rows from the GOF down and their kinds, for the gates that allow only
# some horizons (F, O, R).
.ka5_only <- function(v, allow) {
  rows <- .ka5_rows_from_gof(v)
  if (!length(rows)) return(list(passed = FALSE, rows = integer(0)))
  r <- vapply(rows, function(i) .kidpr_or(vapply(allow, function(sp) .ka5_rows(v, sp)[i], NA)), NA)
  list(passed = .kidpr_and(r), rows = rows, bad = rows[r %in% FALSE])
}

#' The O/C-Boeden (p. 286): only O horizons and solid rock or coarse
#' skeleton; FF over solid rock (mC), FS in or over block debris (xC, xC+O).
#' @param rock NULL (class), "m" (FF) or "x" (FS).
#' @noRd
oc_soil_ka5 <- function(pedon, rock = NULL) {
  v <- .ka5_view(pedon)
  fr <- v$frame
  rows <- v$ord
  isO <- .ka5_rows(v, list(masters = c("O", "L")))
  wparts <- lapply(fr$with, function(w) sub("^[a-z]*", "", w))
  # the skeleton: an xC, or an O interfingered with an xC (xC+O)
  inskel <- vapply(seq_len(nrow(fr)), function(i) "C" %in% wparts[[i]] &&
                     any(grepl("^[a-z]*x[a-z]*C", fr$with[[i]])), NA)
  isO_pure <- isO & !inskel
  isRock <- .ka5_rows(v, list(masters = "C", prefixes = c("m", "x")))
  ok <- vapply(rows, function(i) .kidpr_or(isO[i], isRock[i]), NA)
  p <- .kidpr_and(.kidpr_and(ok), any(isO %in% TRUE), any(isRock %in% c(TRUE, NA)) || any(inskel))
  if (!is.null(rock)) {
    # the first row under the pure O layers: solid rock (FF), or block
    # debris with or without humus in its voids (FS)
    c1 <- rows[!(isO_pure[rows] %in% TRUE)][1L]
    kind <- if (is.na(c1)) NA else if (rock == "x") .kidpr_or(.ka5_rows(v, list(masters = "C", prefixes = "x"))[c1], inskel[c1])
            else .kidpr_and(!inskel[c1], .ka5_rows(v, list(masters = "C", prefixes = "m"))[c1])
    p <- .kidpr_and(p, kind)
  }
  .ka5_p(p, layers = rows, notes = sprintf("only O horizons over %s", if (is.null(rock)) "solid rock or skeleton" else if (rock == "m") "solid rock" else "block debris"))
}

#' The terrestrial Rohboeden (p. 287): only an Ai under 3 dm and rock; OO on
#' solid rock (mC), OL on loose rock (lC). An Aih is an Ah/C soil (Q18).
#' @param rock NULL (class), "m" (OO) or "l" (OL).
#' @noRd
raw_soil_ka5 <- function(pedon, rock = NULL, max_cm = 30) {
  v <- .ka5_view(pedon)
  a <- .ka5_top_run(v, list(masters = "A"))
  if (!length(a$rows)) return(.ka5_p(FALSE, notes = "no topsoil"))
  ai <- .kidpr_and(vapply(a$rows, function(i) .ka5_rows(v, list(codes = "Ai"))[i], NA))
  only <- .ka5_only(v, list(list(codes = "Ai"), list(masters = "C")))
  # an Ai on block debris (xC) is a Syrosem, as the Ranker takes imC and ixC
  # (Hugo, 2026-11-06)
  want <- if (identical(rock, "m")) c("m", "x") else rock
  rk <- if (is.null(rock)) TRUE else {
    nx <- .kidpr_next(v$x, a$rows[length(a$rows)])
    if (is.na(nx)) NA else .ka5_rows(v, list(masters = "C", prefixes = want))[nx]
  }
  p <- .kidpr_and(ai, a$thickness < max_cm, only$passed, rk)
  .ka5_p(p, layers = only$rows, notes = sprintf("only an Ai (%s cm) over rock", format(a$thickness)))
}

# The A rows of a chernozem: the Axh family by the carrying code.
.KA5_AXH <- "^Ac?x(h|p)$"

#' The Schwarzerden (p. 286; Q6): an Axh with its top under 4 dm, the Axh
#' run 4 dm thick or more; moist chroma under 3.5 and value 4 or less where
#' the colour is recorded (p. 211).
#' @noRd
chernozem_ka5 <- function(pedon, min_cm = 40, start_max_cm = 40) {
  v <- .ka5_view(pedon)
  # an Axh with its top under 4 dm and the Axh run from it 4 dm or more (D4,
  # pp. 211, 286): under an Ap or a thin M too
  r <- .ka5_top_run(v, list(pattern = .KA5_AXH), start_max_cm = start_max_cm)
  if (!length(r$rows)) return(.ka5_p(FALSE, notes = "no Axh with its top under 4 dm"))
  x <- v$x
  col <- vapply(r$rows, function(i) {
    cc <- .ka5_colour(x, i)
    if (is.na(cc$value) || is.na(cc$chroma)) NA else cc$chroma < 3.5 && cc$value <= 4
  }, NA)
  thick <- if (r$thickness >= min_cm) (if (r$undecided) NA else TRUE) else if (isTRUE(r$at_bottom)) NA else FALSE
  notes <- c(sprintf("Axh %s cm thick (4 dm or more)", format(r$thickness)),
             if (isTRUE(r$at_bottom) && r$thickness < min_cm) "the Axh reaches the bottom of the description (D6)",
             if (any(is.na(col))) "the Axh colour not recorded: not tested")
  # the Axh fails only on its colour: not a silent no (D5, Q17): undecided,
  # the notation and the data in conflict
  if (any(col %in% FALSE) && !isFALSE(thick))
    return(.ka5_p(NA, layers = r$rows, missing = "munsell_value_moist",
                  notes = c(notes, "the Axh is not black-grey (chroma under 3.5, value 4 or less, p. 211): a conflict of the designation and the data, undecided between the Schwarzerden and the Ah/C-Boeden (Q17)")))
  .ka5_p(thick, layers = r$rows, notes = notes)
}

#' Secondary carbonate in the whole solum (the Kalktschernosem, p. 212): every
#' A row an Acxh (or Acxp), or secondary carbonate recorded in each.
#' @noRd
carbonate_solum_ka5 <- function(pedon) {
  v <- .ka5_view(pedon)
  r <- .ka5_top_run(v, list(pattern = .KA5_AXH), start_max_cm = 40)
  if (!length(r$rows)) return(.ka5_p(FALSE))
  cc <- vapply(r$rows, function(i) .ka5_rows(v, list(pattern = "^Acx"))[i], NA)
  .ka5_p(.kidpr_and(cc), layers = r$rows, notes = "secondary carbonate in the whole Axh")
}

#' The Ah/C-Boeden (p. 287; Q17, Q18): only A horizons (Ah, Ahe, Ae, Aih,
#' Axh under 4 dm) with transitions to the rock (Bv-Ah, B-C, B+C) over rock,
#' the topsoil under 4 dm; S and G only from 4 dm. A fully developed Ah of 4
#' dm or more that is no Axh is undecided between the R type and the
#' Schwarzerden (Q17).
#' @noRd
ah_c_soil_ka5 <- function(pedon, max_cm = 40) {
  v <- .ka5_view(pedon)
  fr <- v$frame
  a <- .ka5_top_run(v, list(masters = "A"))
  if (!length(a$rows)) return(.ka5_p(FALSE, notes = "no topsoil"))
  # an Ai alone is a Rohboden (Q18: an Aih goes to R)
  if (all(vapply(a$rows, function(i) isTRUE(fr$carrying[i] == "Ai"), NA))) return(.ka5_p(FALSE, notes = "an Ai: the Rohboeden"))
  # only Ah horizons (Ah, Ap, Axh, Ahe, Ae, Aih and their marks, Q18): an Al,
  # Ael or Aa is no Ah/C soil
  if (any(vapply(a$rows, function(i) isTRUE(fr$carrying[i] %in% c("Al", "Ael", "Ahl", "Aa")), NA)))
    return(.ka5_p(FALSE, notes = "an Al, Ael or Aa in the topsoil"))
  rows <- .ka5_rows_from_gof(v)
  rest <- setdiff(rows, a$rows)
  # below the topsoil only rock (C, with B-C or B+C transitions), S and G
  # only from 4 dm
  isC <- .ka5_rows(v, list(masters = "C"))
  isSG <- .ka5_rows(v, list(masters = c("S", "G")))
  c_ok <- vapply(rest, function(i) .kidpr_or(isC[i], if (isTRUE(fr$top_gof[i] >= max_cm)) isSG[i] else FALSE), NA)
  hasC <- any(isC[rest] %in% c(TRUE, NA))
  p <- .kidpr_and(if (length(c_ok)) .kidpr_and(c_ok) else NA, hasC)
  notes <- sprintf("only A horizons (%s cm) over rock", format(a$thickness))
  if (a$thickness >= max_cm && !isFALSE(p)) {
    axh <- all(vapply(a$rows, function(i) isTRUE(grepl(.KA5_AXH, fr$carrying[i])), NA))
    if (!axh) return(.ka5_p(NA, layers = a$rows, notes = c(notes, "an Ah of 4 dm or more without Axh: undecided between the Ah/C-Boeden and the Schwarzerden (Q17)")))
    p <- FALSE
  }
  .ka5_p(p, layers = rows, missing = .ka5_rows_missing(v, rest), notes = notes)
}

#' The rock of an Ah/C-Boden (pp. 204-211; Q15, Q18): RN on silicate solid
#' rock or block debris (i with m or x) with the topsoil under 3 dm; RQ on
#' silicate loose rock (i, l) 3 dm or more with the Ah, or an Ah of 3 to < 4
#' dm on solid rock; RR on carbonate rock (c); RZ on marl (e).
#' @param type "RN", "RQ", "RR" or "RZ".
#' @noRd
ah_c_rock_ka5 <- function(pedon, type = c("RN", "RQ", "RR", "RZ")) {
  type <- match.arg(type)
  v <- .ka5_view(pedon)
  fr <- v$frame
  a <- .ka5_top_run(v, list(masters = "A"))
  rows <- .ka5_rows_from_gof(v)
  cr <- setdiff(rows, a$rows)
  cr <- cr[.ka5_rows(v, list(masters = "C"))[cr] %in% c(TRUE, NA)]
  if (!length(cr)) return(.ka5_p(NA, notes = "no rock recorded under the topsoil"))
  c1 <- cr[1L]
  pre <- function(p) .ka5_rows(v, list(masters = "C", prefixes = p))[c1]
  th <- a$thickness
  solid <- .kidpr_or(pre("m"), pre("x"))
  loose_th <- sum(pmax(0, fr$bottom_gof[cr] - fr$top_gof[cr]), na.rm = TRUE) + th
  # a thin loose layer over solid rock (Ah/ilCv/imC, pp. 204-205): the solid
  # rock that begins under 3 dm makes the Ranker, the loose material of the
  # Regosol is the part above it (v0.9.363)
  sol <- cr[vapply(cr, function(i) isTRUE(.kidpr_or(.ka5_rows(v, list(masters = "C", prefixes = "m"))[i],
                                                      .ka5_rows(v, list(masters = "C", prefixes = "x"))[i])), NA)]
  if (length(sol) && isFALSE(solid) && isTRUE(pre("l"))) {
    st <- fr$top_gof[sol[1L]]
    solid <- if (is.na(st)) NA else st < 30
    loose_th <- st
  }
  p <- switch(type,
    RN = .kidpr_and(pre("i"), solid, th < 30),
    RQ = .kidpr_and(pre("i"), .kidpr_or(.kidpr_and(pre("l"), loose_th >= 30), .kidpr_and(solid, th >= 30))),
    RR = pre("c"),
    RZ = pre("e"))
  .ka5_p(p, layers = c1, missing = unique(c(unlist(fr$missing[c1]), if (length(fr$prefixes_na[[c1]])) c("carbonate_whole_soil_pct", "caco3_pct"))),
         notes = sprintf("the rock %s under an Ah of %s cm", fr$code[c1] %||% "?", format(th)))
}

#' The Reduktosole (p. 287; Q20): a Y horizon with its top under 4 dm, unless
#' the gas is of natural wetness.
#' @noRd
reductosol_ka5 <- function(pedon, max_cm = 40) {
  v <- .ka5_view(pedon)
  if (isTRUE(.ka5_site_txt(v$x, "reductgas_source") == "natural_wetness"))
    return(.ka5_p(FALSE, notes = "the reducing gas of natural wetness: no Reduktosol (Q20)"))
  w <- .ka5_within(v, list(masters = "Y"), max_cm = max_cm)
  .ka5_p(w$passed, layers = w$rows, missing = .ka5_rows_missing(v, w$na_rows), notes = "a Y horizon under 4 dm")
}

#' The podzol sequence (p. 287): an Ae (or Ahe) over a Bh or Bs (B(h)s,
#' B(s)h, cemented or not), the podzol part reaching 1.5 dm when a B, M or E
#' follows.
#' @noRd
podzol_ka5 <- function(pedon, min_cm = 15) {
  v <- .ka5_view(pedon)
  fr <- v$frame
  ae <- .ka5_rows(v, list(codes = c("Ae", "Ahe", "Aeh")))
  bs <- .ka5_rows(v, list(pattern = "^B[hms]*(h|s)[hms]*$"))
  rows <- v$ord
  hit <- NA_integer_; und <- FALSE
  for (k in seq_along(rows)) {
    i <- rows[k]
    if (!(ae[i] %in% c(TRUE, NA))) next
    j <- .kidpr_next(v$x, i)
    while (!is.na(j) && ae[j] %in% c(TRUE, NA)) j <- .kidpr_next(v$x, j)
    if (is.na(j) || !(bs[j] %in% c(TRUE, NA))) next
    if (is.na(ae[i]) || is.na(bs[j])) und <- TRUE
    hit <- j
    break
  }
  if (is.na(hit)) return(.ka5_p(FALSE, notes = "no Ae over a Bh or Bs"))
  last <- hit
  nx <- .kidpr_next(v$x, last)
  while (!is.na(nx) && bs[nx] %in% c(TRUE, NA)) { last <- nx; nx <- .kidpr_next(v$x, nx) }
  follows <- !is.na(nx) && .ka5_is(v, nx, c("B", "M", "E"))
  deep <- isTRUE(fr$bottom_gof[last] >= min_cm)
  p <- if (follows && !deep) FALSE else if (und) NA else TRUE
  .ka5_p(p, layers = c(hit, last), notes = sprintf("an Ae over a Bh or Bs to %s cm%s", format(fr$bottom_gof[last]),
                                                    if (follows) " (a B, M or E follows: 1.5 dm or more)" else ""))
}

# The rows of an R horizon: the master R, or an R part joined by "-" or "+"
# (R-Ap, R+Bhs: the R body of the Rigosol and Treposol, p. 236).
.ka5_r_rows <- function(v, reaching_cm = NULL, max_cm = NULL) {
  fr <- v$frame
  m <- .ka5_rows(v, list(masters = "R"))
  bur <- if (is.null(fr$buried)) integer(nrow(fr)) else fr$buried
  part <- vapply(fr$features, function(f) any(f %in% c("R-", "R+")), NA) & !(bur > 0L)
  rows <- which(m %in% TRUE | part)
  if (!length(rows)) return(list(passed = if (any(is.na(m))) NA else FALSE, rows = integer(0), na_rows = which(is.na(m))))
  top <- min(fr$top_gof[rows], na.rm = TRUE); bot <- max(fr$bottom_gof[rows], na.rm = TRUE)
  # one strict bound for the Y class and the Rigosol and Treposol: deeper
  # than 4 dm (D9, pp. 107, 236); an R that reaches the bottom of the
  # description at or above it is undecided (D6)
  reach <- if (is.null(reaching_cm)) TRUE else if (bot > reaching_cm) TRUE else if (.ka5_at_bottom(fr, rows[which.max(fr$bottom_gof[rows])])) NA else FALSE
  ok <- .kidpr_and(is.null(max_cm) || top < max_cm, reach)
  list(passed = ok, rows = rows, na_rows = which(is.na(m)), bottom = bot,
       plus = any(vapply(fr$features[rows], function(f) "R+" %in% f, NA)))
}

#' The anthropogenic soils (p. 287): an M, E or R horizon with its top under
#' 4 dm reaching deeper, S and G only from 4 dm, and no podzol sequence of
#' 1.5 dm (it goes to the Podsole).
#' @noRd
anthropic_soil_ka5 <- function(pedon, max_cm = 40) {
  v <- .ka5_view(pedon)
  w <- .ka5_within(v, list(masters = c("M", "E")), max_cm = max_cm, reaching_cm = max_cm)
  r <- .ka5_r_rows(v, reaching_cm = max_cm, max_cm = max_cm)
  w$passed <- .kidpr_or(w$passed, r$passed); w$rows <- c(w$rows, if (isTRUE(r$passed)) r$rows)
  if (isFALSE(w$passed)) return(.ka5_p(FALSE, notes = "no M, E or R from under 4 dm to 4 dm"))
  sg <- .ka5_within(v, list(masters = c("S", "G")), max_cm = max_cm)
  pz <- podzol_ka5(pedon)$passed
  .ka5_p(.kidpr_and(w$passed, !sg$passed, !pz), layers = w$rows, missing = .ka5_rows_missing(v, c(w$na_rows, sg$na_rows)),
         notes = c("an M, E or R reaching 4 dm", if (isTRUE(pz)) "a podzol sequence of 1.5 dm: the Podsole"))
}

#' The deep cultivation of the Rigosol and Treposol (pp. 236-237; Q23): an R
#' horizon reaching deeper than 4 dm, the kind of cultivation recorded
#' (deep_cultivation_kind). Not recorded, the Rigosol is assigned first in
#' the key's order, provisionally, the Treposol a candidate.
#' @param kind "repeated_trenching" (YY) or "single_deep_ploughing" (YU).
#' @noRd
deep_cultivation_ka5 <- function(pedon, kind = c("repeated_trenching", "single_deep_ploughing"), min_cm = 40) {
  kind <- match.arg(kind)
  v <- .ka5_view(pedon)
  w <- .ka5_r_rows(v, reaching_cm = min_cm)
  if (isFALSE(w$passed)) return(.ka5_p(FALSE, notes = "no R horizon deeper than 4 dm"))
  rec <- .ka5_site_txt(v$x, "deep_cultivation_kind")
  plus <- isTRUE(w$plus)
  if (!is.na(rec)) return(.ka5_p(.kidpr_and(w$passed, rec == kind), layers = w$rows, notes = sprintf("deep cultivation recorded: %s", rec)))
  if (kind == "repeated_trenching") {
    if (plus) return(.ka5_p(FALSE, notes = "an R+ horizon: a single deep ploughing (Treposol)"))
    return(.ka5_p(w$passed, layers = w$rows, provisional = isTRUE(w$passed), missing = "deep_cultivation_kind",
                  notes = "the kind of deep cultivation not recorded: the Rigosol in the key's order, the Treposol a candidate (Q23)"))
  }
  .ka5_p(if (plus) w$passed else NA, layers = w$rows, missing = "deep_cultivation_kind",
         notes = if (plus) "an R+ horizon (R-Ap/R+...): the Treposol" else "the kind of deep cultivation not recorded")
}

#' The Stauwasserboeden types (p. 288): SG an Srw (Serw) under 4 dm and an
#' Srd; SS an Sw (Sew, Sdw) under 4 dm and an Sd.
#' @param type "SG" or "SS".
#' @noRd
stagnic_type_ka5 <- function(pedon, type = c("SS", "SG"), max_cm = 40) {
  type <- match.arg(type)
  v <- .ka5_view(pedon)
  # the Sw and Sd with secondary carbonate (Scw, Scd) are the Kalkpseudogley's
  # (p. 227; v0.9.363)
  up <- if (type == "SG") c("Srw", "Serw") else c("Sw", "Sew", "Sdw", "Srw", "Serw", "Scw", "Secw")
  lo <- if (type == "SG") "Srd" else c("Sd", "Srd", "Swd", "Sdg", "Scd")
  a <- .ka5_within(v, list(codes = up), max_cm = max_cm)
  b <- .ka5_within(v, list(codes = lo))
  .ka5_p(.kidpr_and(a$passed, b$passed), layers = c(a$rows, b$rows), missing = .ka5_rows_missing(v, c(a$na_rows, b$na_rows)),
         notes = sprintf("%s under 4 dm and an %s", paste(up, collapse = "/"), paste(lo, collapse = "/")))
}

#' The figure rule of Abb. 22 (Q5): a Bv or Bt over a T, P, Bu or Bj reaches
#' 1.5 dm; else the T, P or V class decides.
#' @param pattern The B of the class (Bv or Bt family).
#' @noRd
b_over_relict_ka5 <- function(pedon, pattern, min_cm = 15) {
  v <- .ka5_view(pedon)
  fr <- v$frame
  b <- .ka5_rows(v, list(pattern = pattern))
  rows <- which(b %in% TRUE)
  if (!length(rows)) return(.ka5_p(TRUE))
  below <- .ka5_rows(v, list(masters = c("T", "P"))) | .ka5_rows(v, list(codes = c("Bu", "Bj", "Bku")))
  last <- max(rows)
  follows <- any(below[v$ord[v$ord %in% which(fr$top_gof >= fr$bottom_gof[last])]] %in% TRUE)
  if (!follows) return(.ka5_p(TRUE))
  .ka5_p(isTRUE(fr$bottom_gof[last] >= min_cm), notes = sprintf("the B over a T, P, Bu or Bj reaches %s cm (1.5 dm, Abb. 22)", format(fr$bottom_gof[last])))
}

# The rows of the Fahlerde's clay difference (p. 220; v0.9.363): the Bt is
# the first row under the first Ael that holds a Bt (carrying it, or as a
# part: the Bt-Sd of SS-LF) and no Ael part (not the Ael+Bt); the Ael the
# last row above it with an Ael part that does not carry a Bt; the Ael+Bt
# rows between them are the interfingering.
.ka5_fahlerde_rows <- function(v) {
  fr <- v$frame
  rows <- .ka5_rows_from_gof(v)
  am <- .ka5_rows(v, list(codes = "Ael"))
  parts <- lapply(seq_len(nrow(fr)), function(i) unique(c(fr$carrying[i], sub("^[a-z]*", "", as.character(fr$with[[i]])))))
  has <- function(i, re) any(grepl(re, parts[[i]]))
  ael <- rows[am[rows] %in% c(TRUE, NA) | vapply(rows, has, NA, re = "^Ael$")]
  if (!length(ael)) return(NULL)
  below <- rows[fr$top_gof[rows] > fr$top_gof[ael[1L]] & !vapply(rows, has, NA, re = "^Ael$")]
  part_bt <- below[vapply(below, has, NA, re = "^B[bvh]?t$")]
  lo <- if (length(part_bt)) part_bt[1L] else NA_integer_
  if (is.na(lo)) return(list(ael = ael, lo = NA_integer_, up = ael[1L], between = integer(0)))
  cand <- ael[fr$top_gof[ael] < fr$top_gof[lo] & !grepl("^B[bvh]?t$", fr$carrying[ael])]
  up <- if (length(cand)) cand[length(cand)] else NA_integer_
  btw <- rows[fr$top_gof[rows] > fr$top_gof[up] & fr$top_gof[rows] < fr$top_gof[lo]]
  btw <- btw[vapply(btw, function(i) has(i, "^Ael$") && has(i, "^B[bvh]?t$"), NA)]
  list(ael = ael, lo = lo, up = up, between = btw, am = am)
}

#' The Fahlerde (pp. 219-220; C13): an Ael and the clay difference Ael to Bt
#' of 9 or 12 points (the Bt under the Ael+Bt, v0.9.363). Measured and short,
#' no Fahlerde and the conflict reported; not measured, the Fahlerde holds on
#' the designation, provisionally.
#' @noRd
fahlerde_ka5 <- function(pedon) {
  v <- .ka5_view(pedon)
  fr <- v$frame
  fz <- .ka5_fahlerde_rows(v)
  if (is.null(fz)) return(.ka5_p(FALSE, notes = "no Ael"))
  if (is.na(fz$lo) || is.na(fz$up)) return(.ka5_p(NA, notes = "an Ael with no Bt under it"))
  up <- fz$up; lo <- fz$lo
  inwith <- any(sub("^[a-z]*", "", fr$with[[up]]) == "Ael")
  # the Ael itself undecided on the data (Al or Ael, D11)
  und <- is.na(fz$am[up]) && !inwith
  cd <- .ka5_clay_diff(v$x, up, lo, "lf", between = fz$between)
  if (is.null(cd$value) || is.na(cd$value)) return(.ka5_p(if (und) NA else TRUE, layers = c(up, lo), provisional = !und, missing = "clay_pct",
                                     notes = "the clay of the Ael and the Bt not recorded: the Fahlerde on the designation, provisionally (C13)"))
  if (isFALSE(cd$passed)) return(.ka5_p(FALSE, layers = c(up, lo),
                                        notes = c(cd$note, "the Ael written, but the clay difference is short of the Fahlerde: a conflict of the notation and the data (C13)")))
  .ka5_p(if (und && isTRUE(cd$passed)) NA else cd$passed, layers = c(up, lo),
         missing = c(cd$missing, if (und) unlist(fr$missing[up])),
         notes = c(cd$note, if (und) "the Ael undecided on the data (its colour not recorded)"))
}

#' The Terrae calcis (p. 288): a T horizon with its top under 3 dm over a
#' carbonate rock (a C with the prefix c, or 75 % carbonate).
#' @noRd
terra_ka5 <- function(pedon, max_cm = 30) {
  v <- .ka5_view(pedon)
  fr <- v$frame
  w <- .ka5_within(v, list(masters = "T"), max_cm = max_cm)
  if (isFALSE(w$passed)) return(.ka5_p(FALSE, notes = "no T under 3 dm"))
  below <- v$ord[fr$top_gof[v$ord] > min(fr$top_gof[c(w$rows, w$na_rows)], na.rm = TRUE) & !(fr$buried[v$ord] > 0L)]
  # a C with the prefix c is limestone; an e (marl) or i excludes 75 % or
  # more (D18); else the carbonate recorded
  lime1 <- function(i) {
    if (isTRUE(fr$master[i] == "C") && any(fr$prefixes[[i]] %in% c("e", "i"))) return(FALSE)
    .kidpr_or(.ka5_rows(v, list(masters = "C", prefixes = "c"))[i], .ka5_at_least(.ka5_carbonate(v$x, i)$range, 75))
  }
  lime <- if (!length(below)) NA else .kidpr_or(vapply(below, lime1, NA))
  .ka5_p(.kidpr_and(w$passed, lime), layers = w$rows, missing = c(if (is.na(lime)) "carbonate_whole_soil_pct", .ka5_rows_missing(v, w$na_rows)),
         notes = "a T under 3 dm over a carbonate rock")
}

#' The palaeosols of class V (p. 288; Q19): a relict or fossil Bj (VV) or Bu
#' (VW) in the subsoil. The soil above decides when it has a class: the
#' palaeosol is then an overlay ("... ueber Fersiallit"), named with the
#' subtypes.
#' @param codes "Bj" (VV), "Bu" (VW), or both (the class).
#' @noRd
palaeosol_ka5 <- function(pedon, codes = c("Bj", "Bu", "Bku")) {
  v <- .ka5_view(pedon)
  w <- .ka5_within(v, list(codes = codes, fossil = TRUE), buried = TRUE)
  .ka5_p(w$passed, layers = w$rows, notes = sprintf("a relict or fossil %s in the subsoil", paste(codes, collapse = "/")))
}


# --- the Moore and the subhydric types (p. 284) ---------------------------------------

#' The Erd- und Mulmmoore (p. 284): an earthified or mulmified H (Hv, Hm,
#' Hvm, tilled or with carbonate) in the topsoil, 1 dm thick or more.
#' @noRd
earthified_moor_ka5 <- function(pedon, min_cm = 10, pattern = NULL, prefixes = NULL) {
  v <- .ka5_view(pedon)
  spec <- list(masters = "H", pattern = "^Hc?(v|vm|m)p?$")
  r <- .ka5_top_run(v, spec)
  # v0.9.364 (Hugo, 2026-11-06, R2): under a mineral cover of less than 4 dm
  # (Q7) the earthified topsoil is read from the top of the peat
  if (!length(r$rows)) {
    fr <- v$frame
    rows <- .ka5_rows_from_gof(v)
    h1 <- rows[.ka5_rows(v, list(masters = "H"))[rows] %in% TRUE][1L]
    if (!is.na(h1) && isTRUE(fr$top_gof[h1] > 0) && isTRUE(fr$top_gof[h1] < .ka5_thr("ov_moor_cm")[3L]) &&
        !any(fr$master[rows[seq_len(match(h1, rows) - 1L)]] %in% "H")) {
      r2 <- .ka5_top_run(v, spec, start_max_cm = fr$top_gof[h1] + 1e-9)
      if (length(r2$rows) && identical(r2$rows[1L], h1)) r <- r2
    }
  }
  p <- if (r$thickness >= min_cm) (if (r$undecided) NA else TRUE) else FALSE
  # the types read the earthified topsoil itself (D10, pp. 260-262, 284):
  # its first row of the kind and moor prefix asked
  if (!is.null(pattern) || !is.null(prefixes)) {
    k <- if (length(r$rows)) .ka5_rows(v, list(masters = "H", pattern = pattern, prefixes = prefixes))[r$rows[1L]] else FALSE
    p <- .kidpr_and(p, k)
  }
  .ka5_p(p, layers = r$rows, missing = .ka5_rows_missing(v, r$rows),
         notes = sprintf("earthified or mulmified H %s cm thick in the topsoil%s", format(r$thickness),
                         if (!is.null(pattern)) sprintf(", its first horizon %s", pattern) else ""))
}

#' A value of the KA5 frame's rows of one horizon against a limit: the rH of
#' an F (the Gyttja, Sapropel, Dy, p. 284), the pH (CaCl2) of the hH (the
#' Hochmoor). TRUE, FALSE, or NA when not measured.
#' @param attr The attribute (rh_value, ph_cacl2).
#' @param min,max The limits (min inclusive, max exclusive).
#' @noRd
value_of_horizon_ka5 <- function(pedon, codes = NULL, masters = NULL, prefixes = NULL, attr, min = NULL, max = NULL,
                                 prefer_codes = NULL) {
  v <- .ka5_view(pedon)
  w <- .ka5_within(v, list(codes = codes, masters = masters, prefixes = prefixes))
  if (isFALSE(w$passed)) return(.ka5_p(FALSE, notes = "no such horizon"))
  rows <- c(w$rows, w$na_rows)
  # v0.9.364: the rows of the codes asked first, when there are any (the
  # hH(w) of the Hochmoor, p. 260; the review's D5)
  if (!is.null(prefer_codes)) {
    pc <- rows[v$frame$carrying[rows] %in% unlist(prefer_codes)]
    if (length(pc)) rows <- pc
  }
  val <- vapply(rows, function(i) .ka5_num(v$x, i, attr), 0)
  ok <- vapply(val, function(z) if (is.na(z)) NA else (is.null(min) || z >= min) && (is.null(max) || z < max), NA)
  p <- .kidpr_and(w$passed, .kidpr_or(ok))
  .ka5_p(p, layers = rows, missing = if (all(is.na(val))) attr,
         notes = sprintf("%s of the %s: %s (%s%s)", attr, paste(c(unlist(prefixes), unlist(codes), unlist(masters)), collapse = "/"),
                         paste(format(val), collapse = ", "), if (!is.null(min)) sprintf("%s or more", min) else "",
                         if (!is.null(max)) sprintf("under %s", max) else ""))
}
