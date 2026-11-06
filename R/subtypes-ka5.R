# =============================================================================
# v0.9.363 -- the subtypes of the 28 terrestrial types of the German KA5
# (Bodenkundliche Kartieranleitung, 5th ed., Ad-hoc-AG Boden 2005), from the
# type texts (pp. 202-237): 138 subtypes of the 13 terrestrial classes, in
# inst/rules/ka5/subtypes/<class>.yaml.
#
# Each subtype is its printed horizon formula(s) (each alternative a string,
# as printed, read by .ka5_parse_formula() and matched with
# match_horizon_sequence() on the horizon frame of identify_horizons(pedon,
# "ka5")), its depth windows (the annotations under the formulas: the lower
# boundary of the underlined horizons) and its numbers (base-saturation
# marks, clay differences, Cp/Fep, slope ...). Hugo's decision Q21: inside a
# type the transitional subtypes (X-Y, a subtype of Y with the features of
# X) come first in printed order, then the deviation subtypes in printed
# order, the Norm subtype last as the fallback; the first that passes names
# the subtype, every one that passes is reported in the trace. Q22: the
# Treposol has no Norm subtype; the Rigosol and the Treposol take an open
# subtype "[X-YY]", "[X-YU]" for the type X still recognisable under the R.
# Three-valued: a gate the data leave open makes the subtype provisional,
# with the subtypes left undecided as candidates.
# =============================================================================

.ka5_sub_cache <- new.env(parent = emptyenv())

# Every subtype entry of the installed rules, by code (with the type it
# belongs to).
.ka5_subtype_entries <- function() {
  if (is.null(.ka5_sub_cache$entries)) {
    dir <- system.file("rules", "ka5", "subtypes", package = "soilKey")
    out <- list()
    if (nzchar(dir)) {
      for (f in sort(list.files(dir, pattern = "\\.yaml$", full.names = TRUE))) {
        bl <- yaml::read_yaml(f)$subtypes
        for (ty in names(bl)) for (e in bl[[ty]]) {
          e$type <- ty
          out[[e$code]] <- e
        }
      }
    }
    .ka5_sub_cache$entries <- out
  }
  .ka5_sub_cache$entries
}

.ka5_subtype_entry <- function(code) {
  e <- .ka5_subtype_entries()[[code]]
  if (is.null(e)) rlang::abort(sprintf("No KA5 subtype '%s'.", code))
  e
}

# The names of the KA5 types by code (for the open subtypes).
.ka5_type_entries <- function() {
  if (is.null(.ka5_sub_cache$types)) {
    dir <- system.file("rules", "ka5", "types", package = "soilKey")
    out <- list()
    for (f in sort(list.files(dir, pattern = "\\.yaml$", full.names = TRUE))) {
      bl <- yaml::read_yaml(f)$types
      for (cl in names(bl)) for (e in bl[[cl]]) out[[e$code]] <- e
    }
    .ka5_sub_cache$types <- out
  }
  .ka5_sub_cache$types
}

# The last item of a formula at depth 0 (after its last "/"), with the
# optional groups of horizons that open it ("(Bv/)" of "(Bv/)C") moved to
# the head.
.ka5_formula_tail <- function(s) {
  ch <- strsplit(s, "", fixed = TRUE)[[1L]]
  depth <- 0L; cut <- 0L
  for (k in seq_along(ch)) {
    if (ch[k] == "(") depth <- depth + 1L
    else if (ch[k] == ")") depth <- depth - 1L
    else if (ch[k] == "/" && depth == 0L) cut <- k
  }
  head <- substr(s, 1L, cut); tail <- substr(s, cut + 1L, nchar(s))
  repeat {
    if (!startsWith(tail, "(")) break
    tc <- strsplit(tail, "", fixed = TRUE)[[1L]]
    cl <- .ka5_close(tc, 1L)
    if (is.na(cl)) break
    grp <- substr(tail, 1L, cl)
    if (!grepl("/", grp, fixed = TRUE)) break
    head <- paste0(head, grp); tail <- substr(tail, cl + 1L, nchar(tail))
  }
  list(head = head, tail = tail)
}

# A formula of a subtype as the matcher reads it: the base-saturation marks
# are tested apart (on the data or the mark, Q14); a trailing Gr or G of a
# Gley-X formula that writes another G before it is optional, since it lies
# typically 13 dm or deeper; and a final C that names no kind of rock (C,
# eC, cC, i,eC, C(c), IIC ..., not lC, mC, xC) is satisfied by any deeper
# row (the review of 2026-11-06, D1: the formulas list only the
# characteristic horizons, p. 199): it is read as optional, and the matcher
# asks for a row under the formula (`terminal`). Parsed once.
.ka5_sub_spec <- function(f, typical_tail = FALSE) {
  key <- paste0(if (typical_tail) "t:" else "f:", f)
  if (!is.null(.ka5_sub_cache$specs[[key]])) return(.ka5_sub_cache$specs[[key]])
  s <- gsub("[<>]", "", f)
  if (typical_tail) {
    m <- regmatches(s, regexec("^(.*/)(\\(?[a-z]*G[a-z]*\\)?)$", s))[[1L]]
    if (length(m) && grepl("G", m[2L], fixed = TRUE) && !startsWith(m[3L], "("))
      s <- paste0(sub("/$", "", m[2L]), "(/", m[3L], ")")
  }
  terminal <- FALSE
  tl <- .ka5_formula_tail(s)
  if (nzchar(tl$tail) && nzchar(tl$head)) {
    m <- regmatches(tl$tail, regexec("^(\\(II\\)|II|III)?([a-z,()]*)C([a-z]*|\\([a-z,]+\\))*$", tl$tail))[[1L]]
    if (length(m) && !grepl("[lmx]", m[3L])) {
      terminal <- TRUE
      head <- tl$head
      s <- if (endsWith(head, "/")) paste0(sub("/$", "", head), "(/", tl$tail, ")")
           else paste0(head, "(", tl$tail, ")")
    }
  }
  if (grepl("(^|/)(II|III)(\\([a-z]+\\)|[a-z]*)\\.\\.\\.$", s)) terminal <- TRUE
  sp <- .ka5_parse_formula(s)
  sp$terminal <- terminal
  if (is.null(.ka5_sub_cache$specs)) .ka5_sub_cache$specs <- list()
  .ka5_sub_cache$specs[[key]] <- sp
  sp
}

# The parts of a decided row: its carrying code and its other parts, without
# their prefixes.
.ka5_row_parts <- function(fr, i) {
  unique(c(fr$carrying[i], sub("^[a-z]*", "", as.character(fr$with[[i]]))))
}

# Per row: TRUE when a part of a decided row matches the regular expression
# `parts` (or, with `carrying_only`, its carrying code), NA for an undecided
# row with such a candidate; rows of a buried soil FALSE.
.ka5_rows_parts <- function(v, parts) {
  fr <- v$frame
  vapply(seq_len(nrow(fr)), function(i) {
    if (isTRUE(fr$buried[i] > 0L)) return(FALSE)
    if (!is.na(fr$carrying[i])) return(any(grepl(parts, .ka5_row_parts(fr, i))))
    cc <- v$cand[[i]]
    if (!length(cc)) return(FALSE)
    if (any(vapply(cc, function(z) grepl(parts, z$code), NA))) NA else FALSE
  }, NA)
}

# The rows a window or check reads: `of` a spec on the carrying code
# (codes, pattern, masters, prefixes, with) or `parts` a regular expression
# on any part.
.ka5_sub_rows <- function(v, w) {
  if (!is.null(w$parts)) return(.ka5_rows_parts(v, w$parts))
  .ka5_rows(v, w$of %||% list())
}

# A depth against a window: side "range" [a, b), "min" (a or more), "max"
# (under a).
.ka5_in_window <- function(d, lim, side) {
  if (is.na(d)) return(NA)
  switch(side,
         range = d >= lim[1L] && d < lim[2L],
         min = d >= lim[1L],
         max = d < lim[1L])
}

.ka5_window_txt <- function(lim, side) {
  dm <- function(z) format(z / 10)
  switch(side, range = sprintf("%s to < %s dm", dm(lim[1L]), dm(lim[2L])),
         min = sprintf("%s dm or more", dm(lim[1L])), max = sprintf("under %s dm", dm(lim[1L])))
}

#' One window of a KA5 subtype, three-valued (plan 4.2)
#'
#' `w$kind`: "first_top" (the top of the first such horizon), "last_bottom"
#' (the bottom of the last one), "run_bottom" (the bottom of the run of such
#' horizons from the GOF down), "thickness" (the summed thickness of such
#' horizons). Depths from the GOF; the limits from thresholds.yaml (`thr`),
#' read by `side`. NA where a depth is missing, an undecided row could
#' change the answer, or the description ends above the window.
#' @noRd
.ka5_sub_window <- function(v, w) {
  fr <- v$frame
  lim <- .ka5_thr(w$thr)
  side <- w$side %||% "range"
  m <- .ka5_sub_rows(v, w)
  rows <- .ka5_rows_from_gof(v)
  what <- w$what %||% w$parts %||% paste(unlist(w$of), collapse = "/")
  deepest <- suppressWarnings(max(fr$bottom_gof[rows], na.rm = TRUE))
  res <- function(p, d, txt, miss = character(0))
    list(passed = p, missing = miss,
         note = sprintf("%s: %s (%s, %s)", what, txt, .ka5_window_txt(lim, side),
                        if (is.na(p)) "undecided" else if (p) "in the window" else "outside the window"))
  kind <- w$kind
  if (kind == "first_top") {
    und <- FALSE
    for (i in rows) {
      if (is.na(m[i])) { und <- TRUE; next }
      if (isTRUE(m[i])) {
        p <- .ka5_in_window(fr$top_gof[i], lim, side)
        if (und) p <- NA
        return(res(p, fr$top_gof[i], sprintf("begins at %s cm", format(fr$top_gof[i])),
                   if (und) .ka5_rows_missing(v, which(is.na(m)))))
      }
    }
    if (isTRUE(w$absent_ok)) return(res(TRUE, NA, "absent"))
    if (und) return(res(NA, NA, "undecided rows", .ka5_rows_missing(v, which(is.na(m)))))
    reach <- switch(side, range = lim[2L], min = Inf, max = lim[1L])
    p <- if (is.finite(deepest) && deepest >= reach) FALSE else if (side == "min") FALSE else NA
    return(res(p, NA, if (is.na(p)) sprintf("absent down to %s cm, where the description ends", format(deepest)) else "absent"))
  }
  if (kind == "last_bottom") {
    hit <- rows[m[rows] %in% TRUE]
    if (!length(hit)) return(res(if (any(is.na(m[rows]))) NA else FALSE, NA, "absent"))
    i <- hit[length(hit)]
    p <- .ka5_in_window(fr$bottom_gof[i], lim, side)
    return(res(p, fr$bottom_gof[i], sprintf("ends at %s cm", format(fr$bottom_gof[i]))))
  }
  if (kind == "run_bottom") {
    run <- integer(0); und <- FALSE
    for (i in rows) {
      if (isFALSE(m[i])) break
      if (is.na(m[i])) und <- TRUE
      run <- c(run, i)
    }
    if (!length(run)) return(res(FALSE, NA, "no such run from the GOF"))
    b <- fr$bottom_gof[run[length(run)]]
    p <- .ka5_in_window(b, lim, side)
    if (isFALSE(p) && side == "min" && .ka5_at_bottom(fr, run[length(run)])) p <- NA
    if (und && !isFALSE(p)) p <- NA
    return(res(p, b, sprintf("the run from the GOF ends at %s cm", format(b)),
               if (und) .ka5_rows_missing(v, run[is.na(m[run])])))
  }
  if (kind == "thickness") {
    th <- function(k) sum(pmax(0, fr$bottom_gof[k] - pmax(0, fr$top_gof[k])), na.rm = TRUE)
    sure <- th(rows[m[rows] %in% TRUE]); maybe <- th(rows[m[rows] %in% c(TRUE, NA)])
    a <- .ka5_in_window(sure, lim, side); b <- .ka5_in_window(maybe, lim, side)
    p <- if (identical(a, b)) a else NA
    return(res(p, sure, sprintf("%s cm thick", if (sure == maybe) format(sure) else sprintf("%s-%s", format(sure), format(maybe)))))
  }
  rlang::abort(sprintf("Unknown KA5 window kind '%s'.", kind))
}


# --- the numbers of the subtypes (checks) ---------------------------------------------

# A check's answer: passed, notes, missing, provisional.
.ka5_ck <- function(passed, note = character(0), missing = character(0), provisional = FALSE)
  list(passed = passed, note = note, missing = missing, provisional = provisional)

# The base saturation of the Ah of an Ah/C soil against a mark (Q14, the
# subtype scale: > 50 % or more, < under 50 %), read on every horizon of the
# topsoil run from the GOF (v0.9.363 review): each row by its measured value,
# else by the mark written on it, else undecided; TRUE when every row meets
# the mark, FALSE when one does not, else NA. A written mark the measured
# value contradicts goes to the notes (the data decide).
.ka5_ck_bs <- function(v, ck) {
  fr <- v$frame
  a <- .ka5_top_run(v, list(masters = "A"))
  if (!length(a$rows)) return(.ka5_ck(NA, "no topsoil"))
  lim <- .ka5_thr("st_bs_mark_pct")
  want_hi <- identical(ck$mark, ">")
  notes <- character(0)
  per <- vapply(a$rows, function(i) {
    b <- .ka5_num(v$x, i, "bs_pct")
    mk <- fr$bs_mark[i]
    if (!is.na(b)) {
      if (!is.na(mk) && ((mk == ">") != (b >= lim)))
        notes <<- c(notes, sprintf("the mark %s written on %s, but the base saturation measured is %s %%: the data decide", mk,
                                   fr$code[i], format(b)))
      notes <<- c(notes, sprintf("base saturation of the %s %s %%", fr$code[i], format(b)))
      return(if (want_hi) b >= lim else b < lim)
    }
    if (!is.na(mk)) {
      notes <<- c(notes, sprintf("the mark %s written on %s", mk, fr$code[i]))
      hi <- mk == ">"
      return(if (want_hi) hi else !hi)
    }
    NA
  }, NA)
  p <- .kidpr_and(per)
  .ka5_ck(p, c(notes, sprintf("every horizon of the topsoil %s %s %% (Q14)%s", ck$mark, format(lim),
                              if (is.na(p)) ": undecided (a horizon with no value and no mark)" else "")),
          if (is.na(p)) "bs_pct")
}

# The first row under `up` (depth order) whose parts match `parts`.
.ka5_next_with <- function(v, up, parts) {
  m <- .ka5_rows_parts(v, parts)
  rows <- .ka5_rows_from_gof(v)
  k <- rows[rows %in% which(m %in% TRUE) & v$frame$top_gof[rows] > v$frame$top_gof[up]]
  if (length(k)) k[1L] else NA_integer_
}

# The Fahlerde's clay difference in a transitional subtype LF-X (C13): the
# Ael against the Bt (not the Ael+Bt, p. 220; .ka5_fahlerde_rows()); measured
# and short of 9 or 12 points, FALSE and the conflict reported; not measured
# (or across a layer change), TRUE on the designation, provisionally.
.ka5_ck_fahlerde <- function(v, ck) {
  fz <- .ka5_fahlerde_rows(v)
  if (is.null(fz)) return(.ka5_ck(FALSE, "no Ael"))
  if (is.na(fz$lo) || is.na(fz$up)) {
    # the Ael and the Bt interfingered in one horizon (R+Ael+Bt): no
    # difference between two horizons to measure
    bt <- .ka5_rows_parts(v, "^B[bvh]?t[v]?$")
    if (any(bt[fz$ael] %in% TRUE))
      return(.ka5_ck(TRUE, "the Ael and the Bt in one horizon: the clay difference of the Fahlerde not measurable, on the designation, provisionally (C13)",
                     provisional = TRUE))
    return(.ka5_ck(NA, "an Ael with no Bt under it"))
  }
  cd <- .ka5_clay_diff(v$x, fz$up, fz$lo, "lf", between = fz$between)
  if (is.null(cd$value) || is.na(cd$value))
    return(.ka5_ck(TRUE, c(cd$note, "the clay difference of the Fahlerde not measured: on the designation, provisionally (C13)"),
                   provisional = TRUE))
  if (isFALSE(cd$passed))
    return(.ka5_ck(FALSE, c(cd$note, "the Ael written, but the clay difference is short of the Fahlerde: a conflict of the notation and the data (C13)")))
  .ka5_ck(cd$passed, cd$note, cd$missing)
}

# The clay band of the Parabraunerde-Braunerde (p. 217; C14): the row over
# the Btv against the Btv, at least 1, 2 or 3 points and under the
# Parabraunerde's 3, 5 or 8 by the texture of the Btv. Not measured, TRUE on
# the designation, provisionally (as the Fahlerde, C13).
.ka5_ck_clay_band <- function(v, ck) {
  fr <- v$frame
  rows <- .ka5_rows_from_gof(v)
  bt <- rows[.ka5_rows(v, list(pattern = "^B[v]?t[v]?$"))[rows] %in% TRUE]
  if (!length(bt)) return(.ka5_ck(FALSE, "no Btv"))
  lo <- bt[1L]
  k <- match(lo, rows)
  up <- if (k > 1L) rows[k - 1L] else NA_integer_
  if (is.na(up)) return(.ka5_ck(NA, "no horizon over the Btv"))
  x <- v$x
  cu <- .ka5_num(x, up, "clay_pct"); cl <- .ka5_num(x, lo, "clay_pct")
  if (is.na(cu) || is.na(cl) || !.ka5_same_layer(x, up, lo))
    return(.ka5_ck(TRUE, "the clay of the Al-Bv and the Btv not measured in one layer: the band on the designation, provisionally (C14)",
                   provisional = TRUE))
  d <- cl - cu
  tc <- .ka5_texture_classes(x, lo)
  mins <- .ka5_thr("st_llbb_band_min")[tc$cls]; maxs <- .ka5_thr("bt_clay_diff")[tc$cls]
  p <- if (d >= max(maxs)) FALSE else if (d < min(mins)) FALSE else if (d >= max(mins) && d < min(maxs)) TRUE else NA
  .ka5_ck(p, c(tc$note, sprintf("clay %s %% over %s %%: a difference of %s points against the band %s to < %s (p. 217)",
                                format(cl), format(cu), format(d), paste(unique(mins), collapse = "/"),
                                paste(unique(maxs), collapse = "/")),
               if (isTRUE(d >= max(maxs))) "the difference of a Parabraunerde: the Btv is a Bt (C14)"),
          if (is.na(p)) c("clay_pct", "silt_pct"))
}

# The slope of the Hangpseudogley (p. 227): 9 % or more, read on the data
# route only (in the notation the prefix s says it).
.ka5_ck_slope <- function(v, ck) {
  fr <- v$frame
  s <- which(.ka5_rows(v, list(masters = "S")) %in% TRUE)
  sl <- .ka5_site_num(v$x, "slope_pct")
  lim <- .ka5_thr("st_slope_min_pct")
  # in the notation an S written without the prefix s: no Hangpseudogley,
  # unless the slope recorded is 9 % or more, which leaves it undecided (the
  # book defines it by the slope, p. 227; Hugo, 2026-11-06, R2)
  if (length(s) && any(fr$route[s] %in% c("notation", "both"))) {
    if (isTRUE(sl >= lim))
      return(.ka5_ck(NA, sprintf("the S written without the prefix s on a slope of %s %% (%s %% or more): undecided (R2)", format(sl), format(lim))))
    return(.ka5_ck(FALSE, "the S horizons written without the prefix s: no Hangpseudogley"))
  }
  if (is.na(sl)) return(.ka5_ck(NA, "slope not recorded", "slope_pct"))
  .ka5_ck(sl >= lim, sprintf("slope %s %% (%s %% or more, p. 227)", format(sl), format(lim)))
}

# A measured ratio of the rows `codes`: Cp/Fep (c_pyro_pct / fe_pyro_pct)
# against a threshold; not measured, the designation stands.
.ka5_ck_cp_fep <- function(v, ck) {
  rows <- which(.ka5_rows(v, list(codes = ck$codes)) %in% TRUE)
  if (!length(rows)) return(.ka5_ck(FALSE, sprintf("no %s", paste(ck$codes, collapse = "/"))))
  r <- vapply(rows, function(i) {
    cp <- .ka5_num(v$x, i, "c_pyro_pct"); fe <- .ka5_num(v$x, i, "fe_pyro_pct")
    if (is.na(cp) || is.na(fe) || fe <= 0) NA_real_ else cp / fe
  }, 0)
  if (all(is.na(r))) return(.ka5_ck(TRUE, sprintf("Cp/Fep of the %s not measured: the designation stands", paste(ck$codes, collapse = "/"))))
  lim <- .ka5_thr(ck$thr)
  ok <- vapply(r[!is.na(r)], function(z) .ka5_in_window(z, lim, ck$side), NA)
  p <- all(ok)
  .ka5_ck(p, c(sprintf("Cp/Fep of the %s %s (%s)", paste(ck$codes, collapse = "/"),
                       paste(format(round(r[!is.na(r)], 2)), collapse = ", "),
                       switch(ck$side, max = sprintf("under %s", format(lim[1L])), min = sprintf("%s or more", format(lim[1L])),
                              range = sprintf("%s to < %s", format(lim[1L]), format(lim[2L])))),
               if (!p) "the designation and the measured Cp/Fep disagree: the data decide"))
}

# The Eisenpodsol's Bs: at least one page of hue redder than the horizon
# above or the one below (p. 222; Q12); colours not recorded, the
# designation stands.
.ka5_ck_hue <- function(v, ck) {
  rows <- which(.ka5_rows(v, list(codes = ck$codes)) %in% TRUE)
  if (!length(rows)) return(.ka5_ck(FALSE, sprintf("no %s", paste(ck$codes, collapse = "/"))))
  x <- v$x
  i <- rows[1L]
  pg <- .ka5_hue_page(.ka5_colour(x, i))
  nb <- c(.ka5_prev(x, i), .ka5_next(x, i))
  nb <- nb[!is.na(nb)]
  pn <- vapply(nb, function(j) .ka5_hue_page(.ka5_colour(x, j)), 0)
  if (is.na(pg) || all(is.na(pn))) return(.ka5_ck(TRUE, "the hue of the Bs and its neighbours not recorded: the designation stands"))
  lim <- .ka5_thr("st_ppe_hue_pages")
  p <- any(pn[!is.na(pn)] - pg >= lim - 1e-9)
  .ka5_ck(p, sprintf("the Bs %s page(s) redder than a neighbour: %s (p. 222)", format(lim), if (p) "yes" else "no"))
}

# A measured value of the rows `parts` in a window (clay_pct, terra
# material); not measured, the designation stands.
.ka5_ck_value <- function(v, ck) {
  rows <- which(.ka5_sub_rows(v, ck) %in% TRUE)
  what <- ck$what %||% ck$parts %||% paste(unlist(ck$of), collapse = "/")
  if (!length(rows)) return(.ka5_ck(FALSE, sprintf("no %s", what)))
  val <- vapply(rows, function(i) .ka5_num(v$x, i, ck$attr), 0)
  if (all(is.na(val))) return(.ka5_ck(TRUE, sprintf("%s not recorded: the designation stands", ck$attr)))
  lim <- .ka5_thr(ck$thr)
  ok <- vapply(val[!is.na(val)], function(z) .ka5_in_window(z, lim, ck$side %||% "range"), NA)
  p <- all(ok)
  .ka5_ck(p, c(sprintf("%s %s", ck$attr, paste(format(val[!is.na(val)]), collapse = ", ")),
               if (!p) "the designation and the data disagree: the data decide"))
}

# The Braunerde-Ranker's first form: the Bv-ilCv thinner than the Ah (p. 205).
.ka5_ck_thinner <- function(v, ck) {
  fr <- v$frame
  a <- .ka5_top_run(v, list(masters = "A"))
  rows <- which(.ka5_rows(v, list(masters = "C", with = "Bv")) %in% TRUE)
  if (!length(rows) || !length(a$rows)) return(.ka5_ck(FALSE, "no Bv-C under an Ah"))
  th <- sum(pmax(0, fr$bottom_gof[rows] - fr$top_gof[rows]))
  if (is.na(th)) return(.ka5_ck(NA, "a depth missing", "top_cm"))
  .ka5_ck(th < a$thickness, sprintf("the Bv-C %s cm, the Ah %s cm (thinner, p. 205)", format(th), format(a$thickness)))
}

# No present horizon holds such a part (LL-YU: no Ael in the R+...).
.ka5_ck_absent <- function(v, ck) {
  m <- .ka5_rows_parts(v, ck$parts)
  rows <- .ka5_rows_from_gof(v)
  p <- if (any(m[rows] %in% TRUE)) FALSE else if (any(is.na(m[rows]))) NA else TRUE
  .ka5_ck(p, sprintf("no %s part: %s", ck$parts, if (is.na(p)) "undecided" else if (p) "none" else "present"))
}

.ka5_sub_check <- function(v, ck) {
  switch(ck$check,
         absent = .ka5_ck_absent(v, ck),
         bs = .ka5_ck_bs(v, ck),
         fahlerde = .ka5_ck_fahlerde(v, ck),
         clay_band = .ka5_ck_clay_band(v, ck),
         slope = .ka5_ck_slope(v, ck),
         cp_fep = .ka5_ck_cp_fep(v, ck),
         hue = .ka5_ck_hue(v, ck),
         value = .ka5_ck_value(v, ck),
         thinner = .ka5_ck_thinner(v, ck),
         rlang::abort(sprintf("Unknown KA5 subtype check '%s'.", ck$check)))
}


# --- the formula ------------------------------------------------------------------------

# The horizon frame as the sequence engine reads it for the subtypes: the
# transitional parts are already in each row's code and features (the KA5
# reader), so no row is read as a transition of the engine's kind. Built
# once per view.
.ka5_sub_table <- function(v) {
  memo <- if (identical(.ka5_state$pedon, v$pedon)) .ka5_state$sub_memo else NULL
  if (!is.null(memo) && !is.null(memo[[".table"]])) return(memo[[".table"]])
  tab <- v$frame
  if (!is.null(tab$transitional)) tab$transitional <- FALSE
  if (!is.null(memo)) memo[[".table"]] <- tab
  tab
}

# The table with a transitional row and the plain rows of its carrying
# horizon under it read as one horizon (Bt-Go/Go, Bv-Tv/Tv): a slot written
# (X-)Y takes the run (the review of 2026-11-06, D2). NULL when no row
# merges.
.ka5_sub_table_merged <- function(tab) {
  n <- nrow(tab)
  if (!n) return(NULL)
  ord <- order(ifelse(is.na(tab$top_cm), Inf, tab$top_cm), seq_len(n))
  hit <- FALSE
  for (k in seq_along(ord)[-1L]) {
    i <- ord[k]; j <- ord[k - 1L]
    if (is.na(tab$carrying[i]) || is.na(tab$carrying[j]) || isTRUE(tab$buried[i] > 0L)) next
    if (length(tab$with[[i]]) || !length(tab$with[[j]]) || !identical(tab$carrying[i], tab$carrying[j])) next
    tab$code[i] <- tab$code[j]
    tab$overlay[[i]] <- unique(c(tab$overlay[[j]], tab$overlay[[i]]))
    tab$features[[i]] <- unique(c(tab$features[[j]], tab$features[[i]]))
    tab$with[[i]] <- tab$with[[j]]
    hit <- TRUE
  }
  if (hit) tab else NULL
}

# One reading of a formula on one table.
.ka5_sub_match1 <- function(v, tab, sp, f, closed_on) {
  voc <- if (length(closed_on)) unique(c(sp$codes, closed_on)) else NULL
  r <- tryCatch(match_horizon_sequence(tab, sp, vocabulary = voc), error = function(e) NULL)
  if (is.null(r)) return(.ka5_ck(NA, sprintf("%s: not matched (an error of the engine)", f)))
  p <- r$passed
  note <- sprintf("%s: %s", f, if (isTRUE(p)) "matches" else if (is.na(p)) "undecided" else
    sprintf("does not match (missing %s)", paste(r$missing, collapse = ", ")))
  mt <- r$matched
  has_mt <- is.data.frame(mt) && nrow(mt) > 0L
  lastb <- if (has_mt) max(mt$bottom_cm, na.rm = TRUE) else NA_real_
  # rows under the formula are free (D1): horizons left over under the last
  # one matched do not break it, unless the formula is closed on them
  if (isFALSE(p) && !length(r$missing) && length(r$extra) && has_mt) {
    ix <- suppressWarnings(as.integer(sub("^.*\\(([0-9]+)\\)$", "\\1", r$extra)))
    if (!anyNA(ix) && all(ix <= nrow(tab)) && all(tab$top_cm[ix] >= lastb) &&
        !any(tab$code[ix] %in% closed_on | tab$carrying[ix] %in% closed_on)) {
      p <- TRUE
      note <- sprintf("%s: matches, the horizons under it not written in the formula", f)
    }
  }
  # the final C (or II...) is satisfied by any deeper row; with none, the
  # description ends at the formula's last horizon: undecided (D1)
  if (isTRUE(p) && isTRUE(sp$terminal) && has_mt) {
    last <- length(sp$slots)
    deeper <- any(tab$top_cm >= lastb & !is.na(tab$top_cm), na.rm = TRUE)
    if (!(last %in% mt$slot) && !deeper) {
      p <- NA
      note <- sprintf("%s: the description ends at its last horizon, the C (or the soil) under it not described: undecided", f)
    }
  }
  # the description ends above the formula's last horizons: undecided
  if (isFALSE(p) && has_mt && !length(r$extra)) {
    req <- which(vapply(sp$slots, function(z) z$kind == "horizon" && !z$optional, NA))
    miss <- setdiff(req, mt$slot)
    deepest <- suppressWarnings(max(tab$bottom_cm, na.rm = TRUE))
    lb <- mt$bottom_cm[which.max(mt$slot)]
    if (length(miss) && all(miss > max(mt$slot)) && isTRUE(lb >= deepest)) {
      p <- NA
      note <- sprintf("%s: the description ends above its last horizons (%s): undecided", f, paste(r$missing, collapse = ", "))
    }
  }
  .ka5_ck(p, note, if (is.na(p)) unique(unlist(v$frame$missing)))
}

#' A subtype formula on the KA5 horizon frame, three-valued: the sequence
#' engine's verdict on the frame, and on the frame with a transitional row and
#' the plain rows of its horizon under it read as one (D2), the better of the
#' two; rows under the formula free, the final C satisfied by any deeper row
#' (D1); a formula that fails only because the description ends above its
#' last horizons is undecided, not failed.
#' @noRd
.ka5_sub_match <- function(v, f, typical_tail = FALSE, closed_on = NULL) {
  sp <- .ka5_sub_spec(f, typical_tail)
  tab <- .ka5_sub_table(v)
  a <- .ka5_sub_match1(v, tab, sp, f, closed_on)
  if (isTRUE(a$passed)) return(a)
  tab2 <- .ka5_sub_table_merged(tab)
  if (is.null(tab2)) return(a)
  b <- .ka5_sub_match1(v, tab2, sp, f, closed_on)
  if (isTRUE(b$passed)) { b$note <- paste(b$note, "(a transitional horizon and its horizon under it read as one)"); return(b) }
  if (is.na(b$passed) && isFALSE(a$passed)) return(b)
  a
}


# --- the predicates -------------------------------------------------------------------

#' A subtype of the KA5 (pp. 202-237; Q21)
#'
#' TRUE when one of the subtype's printed formulas matches the horizon frame
#' with its own windows and numbers, and the windows and numbers of the
#' subtype hold; NA when the data leave it open. The subtype is read from
#' inst/rules/ka5/subtypes/<class>.yaml by its code.
#' @param code The subtype code as printed (GG-BB, BBh ...).
#' @return A list with \code{passed}, \code{layers}, \code{missing},
#'   \code{notes}, \code{provisional}.
#' @noRd
subtype_ka5 <- function(pedon, code, norm = FALSE) {
  v <- .ka5_view(pedon)
  memo <- if (identical(.ka5_state$pedon, pedon)) .ka5_state$sub_memo else NULL
  if (!is.null(memo) && !is.null(memo[[code]])) return(memo[[code]])
  e <- .ka5_subtype_entry(code)
  notes <- character(0); missing <- character(0); prov <- FALSE
  add <- function(r) {
    notes <<- c(notes, r$note); missing <<- c(missing, r$missing)
    if (isTRUE(r$provisional) && isTRUE(r$passed)) prov <<- TRUE
    r$passed
  }
  tail <- isTRUE(e$typical_tail)
  forms <- lapply(e$formulas %||% list(), function(fm) if (is.character(fm)) list(formula = fm) else fm)
  # the podzol B written as the pair B(s)h/B(h)s stands for one horizon or
  # two: a profile with only one of them reads the formula too (soilKey's
  # reading)
  pair <- "B(s)h/B(h)s"
  forms <- unlist(lapply(forms, function(fm) {
    if (!grepl(pair, fm$formula, fixed = TRUE)) return(list(fm))
    one <- function(r) { z <- fm; z$formula <- sub(pair, r, fm$formula, fixed = TRUE); z }
    list(fm, one("(B(s)h/)B(h)s"), one("B(s)h(/B(h)s)"))
  }), recursive = FALSE)
  fp <- if (!length(forms)) TRUE else {
    vals <- vapply(forms, function(fm) {
      p <- add(.ka5_sub_match(v, fm$formula, tail, unlist(e$closed_on)))
      if (isFALSE(p)) return(FALSE)
      for (w in fm$windows %||% list()) p <- .kidpr_and(p, add(.ka5_sub_window(v, w)))
      for (ck in fm$checks %||% list()) p <- .kidpr_and(p, add(.ka5_sub_check(v, ck)))
      p
    }, NA)
    .kidpr_or(vals)
  }
  p <- fp
  if (!isFALSE(p)) {
    for (w in e$windows %||% list()) p <- .kidpr_and(p, add(.ka5_sub_window(v, w)))
    for (ck in e$checks %||% list()) p <- .kidpr_and(p, add(.ka5_sub_check(v, ck)))
  }
  out <- .ka5_p(p, missing = if (is.na(p)) missing else character(0), notes = notes,
                provisional = prov && isTRUE(p))
  # the Norm is the fallback (Q21); when its formula or the marks that
  # complement a deviation are not met, it holds provisionally, the
  # deviation a candidate (Hugo, 2026-11-06, R1, D6)
  if (isTRUE(norm) && !isTRUE(p)) {
    out <- .ka5_p(TRUE, notes = c(notes, sprintf("the Norm subtype as the fallback (Q21): its formula or marks %s, so it holds provisionally%s (R1)",
                                                 if (is.na(p)) "undecided" else "not met",
                                                 if (length(e$complement)) sprintf(", %s a candidate", paste(unlist(e$complement), collapse = ", ")) else "")),
                  provisional = TRUE)
    out$complement <- unlist(e$complement)
  }
  if (!is.null(memo)) memo[[code]] <- out
  out
}

# The type a Rigosol or Treposol formed from, by the horizons still
# recognisable under or in the R (Q22, pp. 236-237): the first of the list
# whose horizon a row of the present soil carries or holds as a part; the
# Fahlerde only with a Bt, a G or S only with its top in the window of the
# printed Gley-Treposol (4 to < 8 dm; the review of 2026-11-06, D7); a type
# whose X-Y is a printed subtype of the type (one that failed) is skipped.
.KA5_RELIC_TYPES <- list(c("^Ael$", "LF"), c("^B[bvh]?t[v]?$", "LL"), c("^B[hms]*(h|s)[hms]*$", "PP"),
                         c("^G", "GG"), c("^S", "SS"), c("^P", "DD"), c("^Tu", "CR"), c("^T", "CF"),
                         c("^Ac?x", "TT"), c("^M$", "YK"), c("^E$", "YE"), c("^Bc?f?v$", "BB"))

.ka5_relic_type <- function(v, type) {
  fr <- v$frame
  rows <- .ka5_rows_from_gof(v)
  printed <- names(Filter(function(e) identical(e$type, type), .ka5_subtype_entries()))
  bt_any <- any(.ka5_rows_parts(v, "^B[bvh]?t[v]?$")[rows] %in% TRUE)
  for (rt in .KA5_RELIC_TYPES) {
    if (paste0(rt[2L], "-", type) %in% printed) next
    if (rt[2L] == "LF" && !bt_any) next
    for (i in rows) {
      if (is.na(fr$carrying[i])) next
      pt <- .ka5_row_parts(fr, i)
      if (!any(grepl(rt[1L], pt))) next
      if (rt[2L] %in% c("GG", "SS")) {
        lim <- .ka5_thr(if (rt[2L] == "GG") "st_g_window_cm" else "st_s_window_cm")
        if (!isTRUE(.ka5_in_window(fr$top_gof[i], lim, "range"))) next
      }
      return(list(type = rt[2L], row = i))
    }
  }
  NULL
}

#' The open subtype of the Rigosol and the Treposol (Q22): "[X-YY]",
#' "[X-YU]" for the type X still recognisable under the R. TRUE when one is;
#' the type in \code{x_type}.
#' @param type "YY" or "YU".
#' @noRd
open_transition_ka5 <- function(pedon, type = c("YY", "YU")) {
  type <- match.arg(type)
  v <- .ka5_view(pedon)
  r <- .ka5_relic_type(v, type)
  und <- any(is.na(v$frame$carrying[.ka5_rows_from_gof(v)]))
  if (is.null(r)) return(.ka5_p(if (und) NA else FALSE, notes = "no type recognisable under the R"))
  out <- .ka5_p(TRUE, layers = r$row,
                notes = sprintf("the %s (%s) recognisable under the R: the open subtype [%s-%s] (Q22)",
                                v$frame$code[r$row], r$type, r$type, type))
  out$x_type <- r$type
  out
}


# --- the result ------------------------------------------------------------------------

# The subtype level of a KA5 result: the open subtype named, every subtype
# of the type that passes listed in the trace (Q21), the others noted.
.ka5_subtype_notes <- function(res, pedon, rules) {
  lv <- res$levels
  if (!is.data.frame(lv) || !nrow(lv)) return(res)
  kt <- which(lv$level == "types")
  if (!length(kt)) return(res)
  ty <- lv$code[kt]
  lst <- rules$subtypes[[ty]]
  if (!length(lst)) return(res)
  ev <- lapply(lst, function(e) evaluate_rsg_tests(pedon, e$tests))
  all <- data.frame(code = vapply(lst, function(e) as.character(e$code), ""),
                    name = vapply(lst, function(e) as.character(e$name), ""),
                    kind = vapply(lst, function(e) as.character(e$kind %||% NA_character_), ""),
                    passed = vapply(ev, function(r) r$passed, NA), stringsAsFactors = FALSE)
  ks <- which(lv$level == "subtypes")
  # the open subtype takes the name of the type it formed from (Q22)
  if (length(ks) && grepl("^\\[X-", lv$code[ks])) {
    k <- match(lv$code[ks], all$code)
    x <- unlist(lapply(ev[[k]]$evidence, function(z) z$x_type))[1L]
    te <- .ka5_type_entries()
    xe <- te[[x]]; ye <- te[[ty]]
    if (!is.null(xe) && !is.null(ye)) {
      lv$code[ks] <- sprintf("[%s-%s]", x, ty)
      lv$name[ks] <- if (ty == "YU") sprintf("[Treposol aus %s]", xe$name) else sprintf("[%s-%s]", xe$name, ye$name)
      if ("name_en" %in% names(lv)) lv$name_en[ks] <- sprintf("[%s from %s]", ye$name_en, tolower(xe$name_en))
      all$code[k] <- lv$code[ks]; all$name[k] <- lv$name[ks]
      res$levels <- lv
      res$name <- lv$name[ks]
    }
  }
  res$trace$subtypes_passing <- all
  # a Norm holding provisionally: the deviations it complements are the
  # candidates (R1, D6)
  if (length(ks)) {
    k <- match(lv$code[ks], all$code)
    cmp <- if (!is.na(k)) unlist(lapply(ev[[k]]$evidence, function(z) z$complement)) else NULL
    if (length(cmp)) res$provisional_because <- unique(c(res$provisional_because, paste0("subtypes:", cmp)))
  }
  if (length(ks)) {
    more <- all$code[all$passed %in% TRUE & !(all$kind %in% "norm") & all$code != lv$code[ks] & !startsWith(all$code, "[")]
    if (length(more))
      res$warnings <- c(res$warnings, sprintf(
        "Further subtypes of the %s pass: %s. The first in the order of Q21 names the subtype (transitions, then deviations, the Norm last); the others would join the name as a variety (Tab. 46 no. 17).",
        lv$name[kt], paste(more, collapse = ", ")))
  } else if (!any(all$passed %in% TRUE) && !any(is.na(all$passed))) {
    res$warnings <- c(res$warnings, sprintf("No subtype of the %s passed: the name stops at the type%s.", lv$name[kt],
                                            if (ty == "YU") " (the Treposol has no Norm subtype, Q22)" else ""))
  }
  res
}
