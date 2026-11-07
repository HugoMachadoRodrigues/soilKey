# =============================================================================
# v0.9.365 -- the varieties and subvarieties of the German KA5 (Bodenkundliche
# Kartieranleitung, 5th ed., Ad-hoc-AG Boden 2005, section 5.7.1.4, pp.
# 263-282), keyed after the subtype, in inst/rules/ka5/varieties.yaml:
#
#   * Tab. 46 (pp. 264-269): the 22 criteria; Tab. 49 (p. 273): the 38 codes;
#     Tab. 50 (pp. 274-282): the varieties of each subtype (read from the page
#     images); Tab. 47 (p. 269) and Tab. 48 (p. 270): the subvarieties of the
#     development depth and the base saturation; the symbol rules I-VII (pp.
#     270-272).
#   * Hugo's decision Q1 (2026-11-04): an additive level after the subtypes,
#     the subvarieties as digits on the variety letter (g4, p2, eu5); a
#     variety offered only where Tab. 50 marks the subtype, plus the six of its
#     note 1 (og, c, z, k, v, ku) everywhere; the humus forms only as recorded.
#     Q14: the base-saturation marks keep their two scales (the subtype scale
#     on the Ah of an Ah/C soil). Q21: the further subtypes that pass join the
#     name by the combination rules of Tab. 46 no. 17 and the rules II-IV.
#   * Each criterion is read on the horizon frame of identify_horizons(pedon,
#     "ka5"): on the notation route by the symbols written (Aeh, Sew, rGo, eGo,
#     Bms, Cbtv ...), on the data route by the recorded data, three-valued. A
#     variety the data leave open is not named; it is listed in the trace as
#     undecided with what would decide it (a variety need not exhaust its
#     criteria, p. 263; soilKey's reading, for Hugo to confirm).
#   * The overlays of Tab. 46 no. 18 (R/overlays-ka5.R) become a variety of the
#     subtype (rule V): the symbol X\Y or X/Y and the name "X ueber Y".
#
# The result: rows of level "variety" in `levels` (one per variety named, its
# code with the digit, the word of the name, the gloss; one for a combination
# and one for an overlay), the full name in `name`, the symbol and the
# details in trace$variety, every criterion in trace$varieties.
# =============================================================================

.ka5_var_cache <- new.env(parent = emptyenv())

# The rules of varieties.yaml, read once.
.ka5_var_rules <- function() {
  if (is.null(.ka5_var_cache$rules)) {
    f <- system.file("rules", "ka5", "varieties.yaml", package = "soilKey")
    .ka5_var_cache$rules <- if (nzchar(f)) yaml::read_yaml(f) else list()
  }
  .ka5_var_cache$rules
}

# A number of the varieties (thresholds of varieties.yaml).
.ka5_var_thr <- function(name) {
  v <- .ka5_var_rules()$thresholds[[name]]$value
  if (is.null(v)) rlang::abort(sprintf("No KA5 variety threshold '%s'.", name))
  as.numeric(unlist(v))
}

# The entries of Tab. 49 by code.
.ka5_var_entries <- function() {
  cs <- .ka5_var_rules()$codes
  stats::setNames(cs, vapply(cs, function(e) as.character(e$code), ""))
}

# The 38 codes in the column order of Tab. 50.
.ka5_var_columns <- function() as.character(unlist(.ka5_var_rules()$columns))


# --- what Tab. 50 offers ------------------------------------------------------------------

# The subtype of the rules a level code stands for: the open subtypes
# ("[BB-YY]" -> "[X-YY]").
.ka5_var_sub_key <- function(sub) {
  if (is.na(sub)) return(NA_character_)
  if (grepl("^\\[[A-Z\u00dc]+-YY\\]$", sub)) return("[X-YY]")
  if (grepl("^\\[[A-Z\u00dc]+-YU\\]$", sub)) return("[X-YU]")
  sub
}

# The tokens of a subtype's printed formulas: prefix and code of every
# horizon written.
.ka5_var_formula_tokens <- function(sub) {
  ents <- .ka5_subtype_entries()
  e <- ents[[sub]]
  if (is.null(e)) return(data.frame(prefix = character(0), code = character(0), stringsAsFactors = FALSE))
  fl <- e$formulas %||% list()
  # an open subtype ([X-YY], [X-YU]) writes no formula: those of its type's
  # printed subtypes
  if (!length(fl) && startsWith(sub, "["))
    fl <- unlist(lapply(Filter(function(z) identical(z$type, e$type) && !startsWith(z$code, "["), ents), function(z) z$formulas),
                 recursive = FALSE)
  fs <- vapply(fl, function(fm) if (is.character(fm)) fm else as.character(fm$formula), "")
  # an optional prefix ((e)Go, a(e)Ah) counts as written (Hugo, 2026-11-07);
  # an optional letter after the master (S(e)w, A(c)h) does not
  fs <- gsub("\\(([a-z]+)\\)(?=[A-Z\u00dc])", "\\1", fs, perl = TRUE)
  fs <- gsub("\\([a-z]+\\)", "", fs)
  tok <- unlist(strsplit(gsub("[()<>]", "", fs), "[/,+\u00b0-]"))
  tok <- sub("^(IV|III|II)", "", trimws(tok))
  tok <- tok[nzchar(tok) & !grepl("\\.", tok)]
  data.frame(prefix = gsub("t[mbp]", "", sub("[A-Z].*$", "", tok)), code = sub("^[a-z]*", "", tok),
             stringsAsFactors = FALSE)
}

#' Is a variety written in the subtype's own formula (and so no variety of
#' it)? The prefix e or z on a horizon other than a C, the prefix o on an A,
#' an M (not of a floodplain or the tidal zone), an Ap, an Akh or Aka, a
#' cemented B, G or H, an Sew, a Ctv or Cbtv, a G other than a Gr (g), an S
#' (s), an aG (a).
#' @noRd
.ka5_var_in_formula <- function(sub, code) {
  # the carbonate of the Kalkpaternia, Pararendzina, Rendzina and
  # Kalktschernosem is their type's own (Hugo, 2026-11-07)
  e <- .ka5_subtype_entries()[[sub]]
  if (identical(code, "c") && isTRUE(e$type %in% c("AZ", "RZ", "RR", "TC"))) return(TRUE)
  tk <- .ka5_var_formula_tokens(sub)
  if (!nrow(tk)) return(FALSE)
  ms <- substr(tk$code, 1L, 1L)
  switch(code,
         c = any(grepl("e", tk$prefix, fixed = TRUE) & ms != "C"),
         z = any(grepl("z", tk$prefix, fixed = TRUE) & ms != "C"),
         og = any(grepl("o", tk$prefix, fixed = TRUE) & ms == "A"),
         k = any(ms == "M" & !grepl("a", tk$prefix, fixed = TRUE) & !grepl("^t", sub("[A-Z].*$", "", tk$code))),
         v = any(tk$code == "Ap"),
         ku = any(grepl("^Ak|^Hk", tk$code)),
         vf = any(grepl("^B[a-z]*m|^G[km][sc]o|^H[km]cw", tk$code)),
         i = any(grepl("^Se", tk$code)),
         l = any(grepl("^C[^b]*t", tk$code)),
         d = any(grepl("^C[a-z]*b[a-z]*t", tk$code)),
         g = any(ms == "G" & !grepl("^Gr", tk$code)),
         s = any(ms == "S"),
         a = any(ms == "G" & grepl("a", tk$prefix, fixed = TRUE)),
         FALSE)
}

#' The varieties Tab. 50 offers a subtype (Q1): the codes marked on its row
#' (the Erdhochmoor's type row read for the KHn; the Rigosol's open subtypes
#' take the YYn's row), the six of note 1 everywhere except on the rows
#' printed empty and where the subtype's formula already writes the feature.
#' One row per code of Tab. 50: offered, source, page.
#' @noRd
.ka5_var_offer <- function(sub) {
  ru <- .ka5_var_rules()
  cols <- .ka5_var_columns()
  key <- .ka5_var_sub_key(sub)
  row <- if (identical(key, "[X-YY]")) ru$allowed[["YYn"]] else ru$allowed[[key]]
  marked <- as.character(unlist(row$codes))
  for (tr in names(ru$type_rows)) if (identical(ru$type_rows[[tr]]$to, key))
    marked <- union(marked, as.character(unlist(ru$type_rows[[tr]]$codes)))
  every <- as.character(unlist(ru$notes$everywhere$codes))
  empty <- key %in% unlist(ru$empty_rows)
  src <- ifelse(cols %in% marked, "Tab. 50", NA_character_)
  for (k in which(cols %in% every & is.na(src) & !empty)) src[k] <- "note 1"
  # what the subtype's own formula writes is no variety of it (soilKey's
  # reading): the Treposol's R-Ap, the Kolluvisol transitions' M ...
  for (k in which(!is.na(src))) if (.ka5_var_in_formula(key, cols[k])) src[k] <- "in the formula"
  data.frame(code = cols, offered = src %in% c("Tab. 50", "note 1"), source = src,
             page = if (is.null(row$page)) NA_integer_ else as.integer(row$page), stringsAsFactors = FALSE)
}


# --- the frame ---------------------------------------------------------------------------

# The parts of a decided row: prefixes (as one string, the tidal t* taken
# off), code and master; the carrying part last.
.ka5_var_parts <- function(fr, i) {
  car <- fr$carrying[i]
  if (is.na(car)) return(NULL)
  w <- as.character(fr$with[[i]])
  pre <- c(sub("[A-Z].*$", "", w), paste(fr$prefixes[[i]], collapse = ""))
  code <- c(sub("^[a-z]*", "", w), car)
  data.frame(prefix = gsub("t[mbp]", "", pre), code = code, master = substr(code, 1L, 1L),
             carrying = c(rep(FALSE, length(w)), TRUE), stringsAsFactors = FALSE)
}

# Does an undecided row hold a candidate matching a pattern?
.ka5_var_cand <- function(fr, i, pattern) {
  cc <- sub("^[a-z]*", "", unlist(fr$candidates[[i]]))
  length(cc) > 0L && any(grepl(pattern, cc[!is.na(cc)]))
}

# The rows of the present soil from the GOF down.
.ka5_var_rows <- function(v) .ka5_rows_from_gof(v)

# The depth the description reaches, and whether it ends on solid rock (a C
# with the prefix m, or rock not diggable).
.ka5_var_bottom <- function(v) {
  fr <- v$frame
  rows <- .ka5_var_rows(v)
  if (!length(rows)) return(list(depth = NA_real_, closed = FALSE))
  last <- rows[length(rows)]
  closed <- isTRUE(fr$master[last] %in% "C") && ("m" %in% fr$prefixes[[last]] || isFALSE(.ka5_lgl(v$x, last, "rock_diggable")))
  list(depth = suppressWarnings(max(fr$bottom_gof[rows], na.rm = TRUE)), closed = closed)
}

# A humus topsoil row (Ah, Ap, Axh, Aeh, Ahe, Akh, Aa ...; not Al, Ael, Ae,
# Ai).
.ka5_var_humus_a <- function(code) {
  !is.na(code) && startsWith(code, "A") && grepl("[hap]", substring(code, 2L)) && !code %in% c("Al", "Ael", "Ae", "Ai")
}

# The run of humus topsoil rows from the GOF.
.ka5_var_a_run <- function(v) {
  fr <- v$frame
  out <- integer(0)
  for (i in .ka5_var_rows(v)) {
    if (!.ka5_var_humus_a(fr$carrying[i])) break
    out <- c(out, i)
  }
  out
}

# One answer of a criterion.
.ka5_var_r <- function(passed, notes = character(0), missing = character(0), digit = NA_integer_, layers = integer(0),
                       target = NA_character_) {
  list(passed = if (length(passed) == 1L) as.logical(passed) else NA, notes = notes[nzchar(notes)],
       missing = if (isTRUE(passed) || isFALSE(passed)) character(0) else unique(as.character(missing)),
       digit = as.integer(digit), layers = as.integer(layers), target = target)
}

.ka5_var_cm <- function(z) format(round(z, 1))


# --- the criteria (Tab. 46) --------------------------------------------------------------

# 1 the humus forms, as recorded (Q1).
.ka5_var_humus_form <- function(v, e) {
  raw <- .ka5_site_txt(v$x, "humus_form")
  hf <- .ka5_humus_form(raw)
  if (is.na(hf)) {
    if (!is.na(raw) && nzchar(raw)) hf <- tolower(trimws(raw))
    else return(.ka5_var_r(FALSE, "no humus form recorded (the humus-form varieties only as recorded, Q1)"))
  }
  ok <- hf %in% unlist(e$values)
  .ka5_var_r(ok, sprintf("humus form recorded: %s", hf))
}

# 1 (note 3) an Aa or a peat thinner than 1 dm at the surface.
.ka5_var_thin_organic <- function(v, e) {
  fr <- v$frame
  rows <- .ka5_var_rows(v)
  if (!length(rows)) return(.ka5_var_r(FALSE))
  lim <- .ka5_var_thr("thin_organic_cm")
  first <- rows[1L]
  aa <- identical(e$code, "o")
  hit <- function(i) {
    pp <- .ka5_var_parts(fr, i)
    if (is.null(pp)) return(if (.ka5_var_cand(fr, i, if (aa) "^Aa$" else "^H")) NA else FALSE)
    if (aa) any(pp$code == "Aa") else any(pp$master == "H" & pp$carrying)
  }
  h1 <- hit(first)
  if (is.na(h1)) return(.ka5_var_r(NA, "the first horizon is undecided", unlist(fr$missing[first])))
  if (!h1) return(.ka5_var_r(FALSE))
  run <- integer(0)
  for (i in rows) { if (isTRUE(hit(i))) run <- c(run, i) else break }
  th <- sum(fr$bottom_gof[run] - pmax(0, fr$top_gof[run]))
  if (is.na(th)) return(.ka5_var_r(NA, "the thickness of the surface layer is not known", c("top_cm", "bottom_cm")))
  what <- if (aa) "Aa" else "peat"
  if (th >= lim) return(.ka5_var_r(FALSE, sprintf("the %s at the surface is %s cm thick (1 dm or more: no variety)", what, .ka5_var_cm(th))))
  if (aa) return(.ka5_var_r(TRUE, sprintf("an Aa of %s cm at the surface (under 1 dm, note 3)", .ka5_var_cm(th)), layers = run))
  kind <- substr(e$horizon, 1L, 1L)
  pre <- unlist(lapply(run, function(i) fr$prefixes[[i]]))
  pna <- unlist(lapply(run, function(i) fr$prefixes_na[[i]]))
  if (kind %in% pre) return(.ka5_var_r(TRUE, sprintf("an %s of %s cm at the surface (under 1 dm, note 3)", e$horizon, .ka5_var_cm(th)), layers = run))
  if (any(c("h", "n", "u") %in% pre)) return(.ka5_var_r(FALSE, sprintf("a peat of another kind (%s) at the surface", paste(intersect(c("h", "n", "u"), pre), collapse = ""))))
  .ka5_var_r(NA, sprintf("a peat of %s cm at the surface, its kind (nieder, uebergang, hoch) not known", .ka5_var_cm(th)),
             c("peat_moor_type", pna))
}

#' 2 the base saturation (Q14): the horizon under the Ah or Aa at 2 to 5 dm
#' (P, Bv, Al, Ael, T, Sw, Sg, Srw, Y, M, E, aM, G), in the Ah/C soils the
#' Ah. Measured: eu 50 % or more, m 20 to < 50 %, dy under 20 %, the digit the
#' class of Tab. 48 on the potential or the effective CEC by cec_method. Not
#' measured: the mark written (the variety scale > >= 50, < 20 to < 50, <<
#' under 20; on the Ah of an Ah/C soil the subtype scale, > 50 or more, <
#' under 50). One answer for the three codes.
#' @noRd
.ka5_var_bs <- function(v, ctx) {
  if (!is.null(ctx$memo$bs)) return(ctx$memo$bs)
  fr <- v$frame
  rows <- .ka5_var_rows(v)
  ac <- identical(ctx$class, "R")
  ref <- NA_integer_
  why <- "no horizon under the Ah to read the base saturation in"
  if (ac) {
    run <- .ka5_var_a_run(v)
    if (length(run)) ref <- run[1L]
  } else if (identical(ctx$class, "F") || identical(ctx$class, "O")) {
    # the O/C soils and the Rohboeden: the O horizon, else the first horizon
    # (review D3, Hugo 2026-11-07)
    all <- v$ord[!(fr$buried[v$ord] > 0L) %in% TRUE]
    o <- all[fr$master[all] %in% "O"]
    ref <- if (length(o)) o[1L] else if (length(all)) all[1L] else NA_integer_
    why <- "no horizon to read the base saturation in"
  } else {
    run <- .ka5_var_a_run(v)
    after <- rows[!rows %in% run & seq_along(rows) > length(run)]
    after <- after[!(fr$master[after] %in% "C")]
    win <- .ka5_var_thr("bs_horizon_cm")
    ov <- after[!is.na(fr$top_gof[after]) & !is.na(fr$bottom_gof[after]) & fr$top_gof[after] < win[2L] & fr$bottom_gof[after] > win[1L]]
    # only a horizon reaching into 2 to < 5 dm (Hugo, 2026-11-07)
    ref <- if (length(ov)) ov[1L] else NA_integer_
    if (is.na(ref)) why <- "no horizon under the Ah (not a C) reaches into 2 to < 5 dm: the base saturation is not read"
  }
  out <- list(code = NA_character_, digit = NA_integer_, row = ref, notes = character(0), missing = character(0),
              bs = NA_real_, class = NA_character_, column = NA_character_)
  if (is.na(ref)) {
    out$notes <- why
    out$code <- "none"
    ctx$memo$bs <- out
    return(out)
  }
  lim <- .ka5_var_thr("bs_pct")
  b <- .ka5_num(v$x, ref, "bs_pct")
  mk <- fr$bs_mark[ref]
  hz <- fr$code[ref] %||% fr$designation[ref]
  if (!is.na(b)) {
    out$bs <- b
    out$code <- if (b >= lim[2L]) "eu" else if (b >= lim[1L]) "m" else "dy"
    meth <- tolower(.ka5_txt(v$x, ref, "cec_method") %||% NA_character_)
    sv <- .ka5_var_rules()$subvarieties$base_saturation
    eff <- isTRUE(meth %in% unlist(sv$effective_methods))
    pot <- isTRUE(meth %in% unlist(sv$potential_methods))
    col <- if (eff) "effective" else "potential"
    out$column <- col
    for (cl in sv$classes) {
      r <- as.numeric(unlist(cl[[col]]))
      if (b >= r[1L] && b < r[2L]) { out$digit <- as.integer(cl$digit); out$class <- cl$class }
    }
    out$notes <- c(sprintf("base saturation of the %s %s %%: %s (Tab. 48 %s, on the %s CEC%s)", hz, format(b), out$code,
                           out$class, col, if (!eff && !pot) {
                             if (is.na(meth)) ", the method not recorded: read as the potential CEC (Q14)"
                             else sprintf(", %s read as the potential CEC (Q14)", meth)
                           } else ""),
                   if (!is.na(mk)) {
                     agree <- switch(mk, ">" = out$code == "eu", "<" = if (ac) out$code != "eu" else out$code == "m",
                                     "<<" = out$code == "dy", TRUE)
                     if (!agree) sprintf("the mark %s written on the %s disagrees: the data decide", mk, hz)
                   })
  } else if (!is.na(mk)) {
    if (ac && mk == "<") {
      out$code <- "m|dy"
      out$notes <- "the mark < on the Ah of the Ah/C soil: under 50 % on the subtype scale (Q14); m or dy not known"
      out$missing <- "bs_pct"
    } else {
      out$code <- switch(mk, ">" = "eu", "<" = "m", "<<" = "dy")
      out$notes <- sprintf("the mark %s written on the %s (%s scale, Q14)", mk, hz, if (ac) "subtype" else "variety")
    }
  } else {
    out$notes <- sprintf("the base saturation of the %s not recorded, no mark written", hz)
    out$missing <- "bs_pct"
  }
  ctx$memo$bs <- out
  out
}

.ka5_var_base_saturation <- function(v, e, ctx) {
  b <- .ka5_var_bs(v, ctx)
  if (identical(b$code, "none")) return(.ka5_var_r(NA, b$notes, "bs_pct"))
  if (is.na(b$code)) return(.ka5_var_r(NA, b$notes, b$missing, layers = b$row))
  if (identical(b$code, "m|dy")) {
    if (e$code == "eu") return(.ka5_var_r(FALSE, b$notes, layers = b$row))
    return(.ka5_var_r(NA, b$notes, b$missing, layers = b$row))
  }
  .ka5_var_r(b$code == e$code, b$notes, digit = if (b$code == e$code) b$digit else NA_integer_, layers = b$row)
}

#' 3, 4 the deep pseudogleying (s) and gleying (g): the first Sw or Go (or a
#' transitional one) 8 dm or deeper under the GOF; flach 8 to < 13 dm (digit
#' 2), tief 13 dm or more (digit 4); s also a very weak one above 8 dm written
#' as a transition Sw-... or Sd-... (no digit). A carrying S or G above 8 dm
#' belongs to the type or the subtype. The description ending above 8 dm
#' (not on solid rock) leaves it open.
#' @noRd
.ka5_var_depth_hydro <- function(v, e) {
  fr <- v$frame
  ms <- e$master
  lim <- .ka5_var_thr("deep_hydromorphy_cm")
  pat <- if (ms == "S") "^S" else "^G"
  good <- function(code, carrying) {
    if (ms == "S") grepl("^S", code) && (code != "Sd" || !carrying)
    else grepl("^G", code) && !grepl("^Gr", code)
  }
  und <- FALSE
  for (i in .ka5_var_rows(v)) {
    pp <- .ka5_var_parts(fr, i)
    if (is.null(pp)) { if (.ka5_var_cand(fr, i, pat)) und <- TRUE; next }
    k <- which(mapply(good, pp$code, pp$carrying))
    if (!length(k)) next
    t <- fr$top_gof[i]
    hz <- fr$code[i]
    if (is.na(t)) return(.ka5_var_r(NA, sprintf("the depth of the %s not known", hz), "top_cm"))
    if (t >= lim[1L]) {
      if (und) return(.ka5_var_r(NA, "an undecided horizon above it", unlist(fr$missing)))
      d <- if (t >= lim[2L]) 4L else 2L
      return(.ka5_var_r(TRUE, sprintf("the %s begins at %s cm: %s (%s 8 dm)", hz, .ka5_var_cm(t),
                                      if (d == 4L) "tief, 13 dm or more" else "flach, 8 to < 13 dm", "8 dm or deeper"),
                        digit = d, layers = i))
    }
    if (ms == "S" && !any(pp$carrying[k])) {
      return(.ka5_var_r(TRUE, sprintf("a very weak pseudogleying above 8 dm written as the transition %s at %s cm", hz, .ka5_var_cm(t)),
                        layers = i))
    }
    return(.ka5_var_r(FALSE, sprintf("the %s begins at %s cm, above 8 dm: part of the type or subtype", hz, .ka5_var_cm(t))))
  }
  if (und) return(.ka5_var_r(NA, "an undecided horizon may be one", unlist(fr$missing)))
  bt <- .ka5_var_bottom(v)
  if (!bt$closed && isTRUE(bt$depth < lim[1L]))
    return(.ka5_var_r(NA, sprintf("the description ends at %s cm, above 8 dm", .ka5_var_cm(bt$depth)), "bottom_cm"))
  .ka5_var_r(FALSE)
}

# 5 Auen: an aG deeper than 8 dm under a terrestrial soil.
.ka5_var_auen <- function(v, e) {
  fr <- v$frame
  lim <- .ka5_var_thr("auen_cm")
  for (i in .ka5_var_rows(v)) {
    pp <- .ka5_var_parts(fr, i)
    if (is.null(pp)) next
    k <- which(pp$master == "G" & grepl("a", pp$prefix, fixed = TRUE))
    if (!length(k)) next
    t <- fr$top_gof[i]
    if (is.na(t)) return(.ka5_var_r(NA, "the depth of the aG not known", "top_cm"))
    # "tiefer als 8 dm": strictly deeper (Hugo, 2026-11-07)
    if (t > lim) return(.ka5_var_r(TRUE, sprintf("the %s begins at %s cm (deeper than 8 dm)", fr$code[i], .ka5_var_cm(t)), layers = i))
    return(.ka5_var_r(FALSE, sprintf("the %s begins at %s cm, not deeper than 8 dm", fr$code[i], .ka5_var_cm(t))))
  }
  bt <- .ka5_var_bottom(v)
  if (!bt$closed && isTRUE(bt$depth <= lim))
    return(.ka5_var_r(NA, sprintf("the description ends at %s cm, not deeper than 8 dm", .ka5_var_cm(bt$depth)), "bottom_cm"))
  .ka5_var_r(FALSE)
}

#' 12 relict (drained): the prefix r on a G or S (rG, rSw, rSd), or a relict
#' redox feature recorded on a G or S row. The target of the r (rule VI): the
#' type when its own hydromorphy is relict, the transitional part when that
#' one's, the variety g or s when the deep one's.
#' @noRd
.ka5_var_relict <- function(v, e, ctx) {
  fr <- v$frame
  hits <- integer(0); mss <- character(0); notes <- character(0)
  for (i in .ka5_var_rows(v)) {
    pp <- .ka5_var_parts(fr, i)
    if (is.null(pp)) next
    k <- which(pp$master %in% c("G", "S") & grepl("r", pp$prefix, fixed = TRUE))
    if (length(k)) { hits <- c(hits, i); mss <- c(mss, pp$master[k[1L]]); next }
    if (pp$master[nrow(pp)] %in% c("G", "S") && isTRUE(.ka5_lgl(v$x, i, "redox_relict"))) {
      hits <- c(hits, i); mss <- c(mss, pp$master[nrow(pp)])
      notes <- c(notes, sprintf("relict redox features recorded on the %s", fr$code[i]))
    }
  }
  if (!length(hits)) return(.ka5_var_r(FALSE))
  hyd <- c("G", "S")
  tm <- substr(ctx$type, 1L, 1L)
  x <- ctx$x_type
  target <- if (tm %in% mss && tm %in% hyd) "T"
            else if (!is.na(x) && substr(x, 1L, 1L) %in% mss) "X"
            else if (any(mss == "G" & fr$top_gof[hits] >= .ka5_var_thr("deep_hydromorphy_cm")[1L], na.rm = TRUE)) "g"
            else if (any(mss == "S" & fr$top_gof[hits] >= .ka5_var_thr("deep_hydromorphy_cm")[1L], na.rm = TRUE)) "s"
            else "T"
  .ka5_var_r(TRUE, c(sprintf("relict hydromorphic horizon(s): %s", paste(fr$code[hits], collapse = ", ")), notes),
             layers = hits, target = target)
}

# 13 rewetted (note 4): the record of the site (not recorded: undecided).
.ka5_var_rewetted <- function(v, e) {
  w <- .ka5_site_lgl(v$x, "rewetted")
  # not recorded: absent (Hugo, 2026-11-07)
  if (is.na(w)) return(.ka5_var_r(FALSE, "rewetting not recorded: absent"))
  .ka5_var_r(w, sprintf("rewetted recorded %s", if (w) "TRUE" else "FALSE"))
}

#' 7 the colours (b, gb, gr): the Rendzina's bAh (on the data: its humus under
#' 8 %); the Plaggenesch's E by its prefix or its colour (7.5YR to 10YR: gr
#' 3/1-4/1, gb 3/2-4/2, b 3/3-5/4); the (Kalk)Paternia's gaAh or baAh (no
#' numbers: by the notation only).
#' @noRd
.ka5_var_colour <- function(v, e, ctx) {
  fr <- v$frame
  ty <- ctx$type
  code <- e$code
  want_pre <- switch(code, b = "b", gb = "gb", gr = "g")
  pre_kind <- function(pre) {
    pre <- gsub("[^bg]", "", pre)
    if (grepl("g", pre) && grepl("b", pre)) "gb" else if (grepl("g", pre)) "gr" else if (grepl("b", pre)) "b" else NA_character_
  }
  if (ty == "RR" || ty %in% c("AQ", "AZ")) {
    run <- .ka5_var_a_run(v)
    if (!length(run)) {
      rows <- .ka5_var_rows(v)
      if (length(rows) && is.na(fr$carrying[rows[1L]])) return(.ka5_var_r(NA, "the first horizon is undecided", unlist(fr$missing[rows[1L]])))
      return(.ka5_var_r(FALSE, "no Ah"))
    }
    i <- run[1L]
    pk <- pre_kind(paste(fr$prefixes[[i]], collapse = ""))
    if (!is.na(pk)) return(.ka5_var_r(pk == code, sprintf("the %s written", fr$designation[i] %||% fr$code[i]), layers = i))
    if (!identical(fr$route[i], "data")) return(.ka5_var_r(FALSE, "no colour prefix written on the Ah"))
    if (ty == "RR" && code == "b") {
      # on the data only a brown colour recorded names the bAh (a hue of the
      # YR pages, chroma 3 or more); its humus alone does not (review D4,
      # Hugo 2026-11-07): otherwise open
      col <- .ka5_colour(v$x, i)
      pos <- .rp_hue_pos(col)
      if (!is.na(pos) && !is.na(col$chroma) && pos >= 10 - 1e-9 && pos <= 20 + 1e-9 && col$chroma >= 3 - 1e-9)
        return(.ka5_var_r(TRUE, sprintf("the Ah %s: brown", .kidpr_colour_text(col)), layers = i))
      return(.ka5_var_r(NA, "no brown colour of the Ah recorded (the bAh by the notation, or a brown colour)",
                        c("designation", "munsell_hue_moist", "munsell_chroma_moist")))
    }
    return(.ka5_var_r(NA, "no numbers for the colour of the Paternia's Ah (by the notation only)", "designation"))
  }
  if (ty == "YE") {
    ee <- which(fr$master %in% "E")
    ee <- intersect(.ka5_var_rows(v), ee)
    if (!length(ee)) return(.ka5_var_r(FALSE, "no E"))
    i <- ee[1L]
    pk <- pre_kind(paste(fr$prefixes[[i]], collapse = ""))
    if (!is.na(pk)) return(.ka5_var_r(pk == code, sprintf("the %s written", fr$designation[i] %||% fr$code[i]), layers = i))
    col <- .ka5_colour(v$x, i)
    pos <- .rp_hue_pos(col)
    if (is.na(pos) || is.na(col$value) || is.na(col$chroma))
      return(.ka5_var_r(NA, "the colour of the E not recorded", c("munsell_hue_moist", "munsell_value_moist", "munsell_chroma_moist")))
    hue_ok <- pos >= 17.5 - 1e-9 && pos <= 20 + 1e-9
    vv <- col$value; cc <- col$chroma
    kind <- if (!hue_ok) NA_character_
            else if (vv >= 3 && vv <= 4 && abs(cc - 1) < 0.5) "gr"
            else if (vv >= 3 && vv <= 4 && abs(cc - 2) < 0.5) "gb"
            else if (vv >= 3 && vv <= 5 && cc >= 3 - 1e-9 && cc <= 4 + 1e-9) "b"
            else NA_character_
    return(.ka5_var_r(identical(kind, code), sprintf("the E %s", .kidpr_colour_text(col)), layers = i))
  }
  .ka5_var_r(FALSE, "Tab. 46 no. 7 gives this colour for the Rendzina, the Plaggenesch and the Kalkpaternia only")
}

# 7 reddish: the type's own B or T horizon (the Bt of the Lessives, the Bv
# of the Braunerde, the Bs of the Podsol, the T of the Terrae calcis; else
# the first of these) redder than 7.5YR, or 7.5YR with a chroma over 4, the
# numbers of the Parabraunerde's Bt (Hugo, 2026-11-07).
.KA5_VAR_RT_HORIZON <- list(L = "^B[a-z]*t", B = "^B[cf]?v$", P = "^B[a-z]*s", C = "^T")
.ka5_var_reddish <- function(v, e, ctx) {
  fr <- v$frame
  pat <- .KA5_VAR_RT_HORIZON[[substr(ctx$type, 1L, 1L)]] %||% "^B[a-z]*t|^B[cf]?v$|^B[a-z]*s|^T"
  for (i in .ka5_var_rows(v)) {
    pp <- .ka5_var_parts(fr, i)
    if (is.null(pp) || !any(grepl(pat, pp$code))) next
    col <- .ka5_colour(v$x, i)
    pos <- .rp_hue_pos(col)
    if (identical(col$family, "N")) return(.ka5_var_r(FALSE, sprintf("a neutral %s", fr$code[i])))
    if (is.na(pos) || (abs(pos - 17.5) < 1e-9 && is.na(col$chroma)))
      return(.ka5_var_r(NA, sprintf("the colour of the %s not recorded", fr$code[i]), c("munsell_hue_moist", "munsell_chroma_moist")))
    ok <- pos < 17.5 - 1e-9 || (abs(pos - 17.5) < 1e-9 && col$chroma > 4)
    return(.ka5_var_r(ok, sprintf("the %s %s", fr$code[i], .kidpr_colour_text(col)), layers = i))
  }
  .ka5_var_r(FALSE, "no B or T horizon of the type to read the reddish colour on")
}

# 8 iron-rich: a Bv of 7.5YR with a chroma of 4 or more, or redder.
.ka5_var_iron_rich <- function(v, e) {
  fr <- v$frame
  for (i in .ka5_var_rows(v)) {
    car <- fr$carrying[i]
    if (is.na(car) || !grepl("^B[cf]?v$", car)) next
    col <- .ka5_colour(v$x, i)
    pos <- .rp_hue_pos(col)
    if (identical(col$family, "N")) return(.ka5_var_r(FALSE, "a neutral Bv"))
    if (is.na(pos) || (abs(pos - 17.5) < 1e-9 && is.na(col$chroma)))
      return(.ka5_var_r(NA, sprintf("the colour of the %s not recorded", fr$code[i]), c("munsell_hue_moist", "munsell_chroma_moist")))
    ok <- pos < 17.5 - 1e-9 || (abs(pos - 17.5) < 1e-9 && col$chroma >= 4 - 1e-9)
    return(.ka5_var_r(ok, sprintf("the %s %s", fr$code[i], .kidpr_colour_text(col)), layers = i))
  }
  .ka5_var_r(FALSE, "no Bv")
}

# 8 sulfate-rich: jarosite (or schwertmannite) recorded.
.ka5_var_sulfate <- function(v, e) {
  rows <- .ka5_var_rows(v)
  j <- vapply(rows, function(i) isTRUE(.ka5_lgl(v$x, i, "jarosite_present")) || isTRUE(.ka5_lgl(v$x, i, "schwertmannite_present")), NA)
  if (any(j)) return(.ka5_var_r(TRUE, "jarosite or schwertmannite recorded", layers = rows[j]))
  .ka5_var_r(FALSE)
}

# 8 cemented: B(h)ms, Gkso, Gmso, Gkco, Gmco, Hkcw, Hmcw written, or a
# cemented B, G or H recorded.
.ka5_var_cemented <- function(v, e) {
  fr <- v$frame
  for (i in .ka5_var_rows(v)) {
    pp <- .ka5_var_parts(fr, i)
    if (is.null(pp)) next
    if (any(grepl("^B[a-z]*m", pp$code) | grepl("^G[km][sc]o", pp$code) | grepl("^H[km]cw", pp$code)))
      return(.ka5_var_r(TRUE, sprintf("the %s written", fr$code[i]), layers = i))
    cm <- tolower(.ka5_txt(v$x, i, "cementation_class") %||% NA_character_)
    if (fr$master[i] %in% c("B", "G", "H") && isTRUE(grepl("moderately|strongly|indurated", cm)))
      return(.ka5_var_r(TRUE, sprintf("the %s recorded %s", fr$code[i], cm), layers = i))
  }
  .ka5_var_r(FALSE)
}

#' 10, 11 a prefix on the solum: og the o on an A; c the e, z the z on a
#' horizon other than a C or a Gr (the solum, p. 316). The data stand in for
#' an unwritten prefix: c a geogenic carbonate of 2 % or more (no secondary
#' carbonate recorded), z an EC of 0.75 dS/m or more; on the data route a row
#' with no record leaves it open.
#' @noRd
.ka5_var_prefix <- function(v, e, ctx = list()) {
  fr <- v$frame
  p <- e$prefix
  und <- character(0); und_rows <- integer(0)
  for (i in .ka5_var_rows(v)) {
    pp <- .ka5_var_parts(fr, i)
    if (is.null(pp)) next
    sol <- if (p == "o") pp$master == "A" else !(pp$master == "C" | grepl("^Gr", pp$code))
    if (!any(sol)) next
    if (any(sol & grepl(p, pp$prefix, fixed = TRUE)))
      return(.ka5_var_r(TRUE, sprintf("the prefix %s on the %s", p, fr$code[i]), layers = i))
    data_row <- identical(fr$route[i], "data")
    # the data of a row stand in only for a row with no symbol written (the
    # data route) whose own horizon is of the solum (not a Bv+cCv, which is a
    # C); a Tschernosem's carbonate is not geogenic by default (Hugo,
    # 2026-11-07)
    if (p != "o" && (!sol[length(sol)] || !data_row)) next
    if (p == "e" && identical(substr(ctx$type %||% "", 1L, 1L), "T")) next
    if (p == "e") {
      cb <- .ka5_num(v$x, i, "caco3_pct"); sc <- .ka5_num(v$x, i, "secondary_carbonates_pct")
      if (isTRUE(cb >= 2) && !isTRUE(sc > 0))
        return(.ka5_var_r(TRUE, sprintf("the %s holds %s %% carbonate, none secondary recorded: geogenic (the data for the prefix e)", fr$code[i], format(cb)),
                          layers = i))
      if (is.na(cb) && is.na(.ka5_txt(v$x, i, "effervescence_class")) && data_row) { und <- c(und, "caco3_pct"); und_rows <- c(und_rows, i) }
    } else if (p == "z") {
      ec <- .ka5_num(v$x, i, "ec_dS_m")
      if (isTRUE(ec >= .ka5_thr("ec_z_dS_m")))
        return(.ka5_var_r(TRUE, sprintf("the %s: EC %s dS/m (the data for the prefix z)", fr$code[i], format(ec)), layers = i))
      if (is.na(ec) && data_row) { und <- c(und, "ec_dS_m"); und_rows <- c(und_rows, i) }
    } else if (p == "o" && data_row && "o" %in% fr$prefixes_na[[i]]) {
      und <- c(und, "oc_lithogenic_pct"); und_rows <- c(und_rows, i)
    }
  }
  if (length(und)) return(.ka5_var_r(NA, sprintf("not recorded on the data row(s) %s", paste(fr$code[unique(und_rows)], collapse = ", ")), und))
  .ka5_var_r(FALSE)
}

# 22 wet-bleached: an Sew.
.ka5_var_wet_bleached <- function(v, e) {
  fr <- v$frame
  und <- FALSE
  for (i in .ka5_var_rows(v)) {
    pp <- .ka5_var_parts(fr, i)
    if (is.null(pp)) { if (.ka5_var_cand(fr, i, "^Se")) und <- TRUE; next }
    if (any(grepl("^Se", pp$code))) return(.ka5_var_r(TRUE, sprintf("the %s", fr$code[i]), layers = i))
  }
  if (und) return(.ka5_var_r(NA, "an undecided horizon may be an Sew", unlist(fr$missing)))
  .ka5_var_r(FALSE)
}

# 9 humus-rich: the Ah or Ap with 8 % or more organic substance down to 1
# dm or deeper.
.ka5_var_humus_rich <- function(v, e) {
  fr <- v$frame
  run <- .ka5_var_a_run(v)
  run <- run[!fr$carrying[run] %in% "Aa"]
  if (!length(run)) {
    rows <- .ka5_var_rows(v)
    if (length(rows) && is.na(fr$carrying[rows[1L]]) && .ka5_var_cand(fr, rows[1L], "^A[a-z]*[hp]$"))
      return(.ka5_var_r(NA, "the first horizon is undecided (an Ah among its candidates)", unlist(fr$missing[rows[1L]])))
    return(.ka5_var_r(FALSE, "no Ah or Ap at the GOF"))
  }
  lim <- .ka5_var_thr("humus_rich_os_pct"); dep <- .ka5_var_thr("humus_rich_depth_cm")
  bot <- max(fr$bottom_gof[run])
  if (is.na(bot)) return(.ka5_var_r(NA, "the depth of the topsoil not known", "bottom_cm"))
  if (bot < dep) return(.ka5_var_r(FALSE, sprintf("the topsoil ends at %s cm, above 1 dm", .ka5_var_cm(bot))))
  use <- run[fr$top_gof[run] < dep]
  os <- vapply(use, function(i) .ka5_os(v$x, i)$range[1L], 0)
  if (any(os < lim, na.rm = TRUE))
    return(.ka5_var_r(FALSE, sprintf("organic substance %s %% in the top 1 dm", paste(format(signif(os, 3)), collapse = ", "))))
  if (anyNA(os)) return(.ka5_var_r(NA, "the organic substance of the topsoil not recorded", "oc_pct"))
  .ka5_var_r(TRUE, sprintf("organic substance %s %% down to 1 dm", paste(format(signif(os, 3)), collapse = ", ")), layers = use)
}

# 6 podzolic: O/Aeh (schwach, 2), O/Ahe (maessig, 3), O/Ahe+Ae (stark, 4).
.ka5_var_podzolic <- function(v, e) {
  fr <- v$frame
  d <- NA_integer_; rows <- integer(0); und <- FALSE
  for (i in .ka5_var_rows(v)) {
    pp <- .ka5_var_parts(fr, i)
    if (is.null(pp)) { if (.ka5_var_cand(fr, i, "^A(he|eh)$")) und <- TRUE; next }
    if (!all(pp$master == "A")) break
    k <- if ("Ahe" %in% pp$code && "Ae" %in% pp$code) 4L else if ("Ahe" %in% pp$code) 3L else if ("Aeh" %in% pp$code) 2L else NA_integer_
    # a separate Ae under an Ahe is the stark degree too (Hugo, 2026-11-07)
    if (is.na(k) && identical(pp$code[nrow(pp)], "Ae") && isTRUE(d >= 3L)) k <- 4L
    if (!is.na(k)) { d <- max(d, k, na.rm = TRUE); rows <- c(rows, i) }
  }
  if (!is.na(d)) return(.ka5_var_r(TRUE, sprintf("the %s: %s podsolig", paste(fr$code[rows], collapse = ", "),
                                                 c("", "schwach", "m\u00e4\u00dfig", "stark")[d]), digit = d, layers = rows))
  if (und) return(.ka5_var_r(NA, "an undecided topsoil may be an Aeh or Ahe", unlist(fr$missing)))
  .ka5_var_r(FALSE)
}

#' 20, 21 lessivated (l, a Ctv) and verfahlt (d, a Cbtv). On the data: the
#' clay bands of a C (bands_area_pct under 10 %, lamellae) give d, clay films
#' on a C give l (soilKey's reading).
#' @noRd
.ka5_var_illuvial_c <- function(v, e) {
  fr <- v$frame
  bands <- e$code == "d"
  for (i in .ka5_var_rows(v)) {
    pp <- .ka5_var_parts(fr, i)
    if (is.null(pp)) next
    k <- which(grepl("^C[a-z]*t", pp$code))
    if (length(k)) {
      hb <- any(grepl("b", pp$code[k], fixed = TRUE))
      if (hb == bands) return(.ka5_var_r(TRUE, sprintf("the %s", fr$code[i]), layers = i))
      next
    }
    if (!identical(fr$route[i], "data") || !(fr$master[i] %in% "C")) next
    if (bands) {
      ba <- .ka5_num(v$x, i, "bands_area_pct"); lm <- .ka5_num(v$x, i, "lamellae_cumulative_cm")
      if (isTRUE(ba > 0 && ba < .ka5_var_thr("bands_area_max_pct")) || (is.na(ba) && isTRUE(lm > 0)))
        return(.ka5_var_r(TRUE, sprintf("clay bands recorded in the %s", fr$code[i]), layers = i))
    } else {
      cf <- tolower(.ka5_txt(v$x, i, "clay_films_amount") %||% NA_character_)
      if (isTRUE(cf %in% c("few", "common", "many")))
        return(.ka5_var_r(TRUE, sprintf("clay films (%s) recorded in the %s", cf, fr$code[i]), layers = i))
    }
  }
  .ka5_var_r(FALSE)
}

# 16 colluvial: an M (not of a floodplain or the tidal zone) under the Ah,
# the Ah/M under 4 dm.
.ka5_var_colluvial <- function(v, e) {
  fr <- v$frame
  rows <- .ka5_var_rows(v)
  run <- .ka5_var_a_run(v)
  rest <- rows[!rows %in% run]
  m <- integer(0)
  for (i in rest) {
    pp <- .ka5_var_parts(fr, i)
    if (is.null(pp)) { if (.ka5_var_cand(fr, i, "^M$")) return(.ka5_var_r(NA, "an undecided horizon may be an M", unlist(fr$missing[i]))); break }
    if (identical(pp$code[nrow(pp)], "M") && !grepl("a", pp$prefix[nrow(pp)], fixed = TRUE) && !any(fr$prefixes[[i]] %in% c("tm", "tb", "tp"))) m <- c(m, i) else break
  }
  if (!length(m)) return(.ka5_var_r(FALSE))
  bot <- max(fr$bottom_gof[m])
  if (is.na(bot)) return(.ka5_var_r(NA, "the depth of the M not known", "bottom_cm"))
  lim <- .ka5_var_thr("colluvial_max_cm")
  .ka5_var_r(bot < lim, sprintf("the Ah/M reaches %s cm (%s 4 dm)", .ka5_var_cm(bot), if (bot < lim) "under" else "not under"),
             layers = m)
}

# 19 eroded: the degree of erosion Eg1 to Eg5 (Tab. 62); not recorded, an
# anthropic truncation recorded stands in, flagged.
.ka5_var_eroded <- function(v, e) {
  eg <- .ka5_site_txt(v$x, "erosion_degree")
  if (!is.na(eg)) {
    n <- suppressWarnings(as.integer(sub("^[Ee][Gg]?", "", trimws(eg))))
    if (!is.na(n)) return(.ka5_var_r(n >= 1L, sprintf("degree of erosion %s (Tab. 62)", eg)))
  }
  .ka5_var_r(FALSE)
}

# 14 arable: an Ap, else an arable land use recorded.
.ka5_var_arable <- function(v, e) {
  fr <- v$frame
  for (i in .ka5_var_rows(v)) {
    pp <- .ka5_var_parts(fr, i)
    if (!is.null(pp) && any(pp$code == "Ap")) return(.ka5_var_r(TRUE, sprintf("the %s", fr$code[i]), layers = i))
  }
  lu <- tolower(.ka5_site_txt(v$x, "land_use") %||% NA_character_)
  if (isTRUE(grepl("arable|crop|acker|tillage", lu))) return(.ka5_var_r(TRUE, sprintf("land use recorded: %s", lu)))
  .ka5_var_r(FALSE)
}

# 15 cultotrophic: no Ap, an Akh or Aka (or Hk) of 1 dm or more.
.ka5_var_cultotrophic <- function(v, e) {
  fr <- v$frame
  rows <- .ka5_var_rows(v)
  ap <- any(vapply(rows, function(i) { pp <- .ka5_var_parts(fr, i); !is.null(pp) && any(pp$code == "Ap") }, NA))
  if (ap) return(.ka5_var_r(FALSE, "an Ap (the variety v)"))
  k <- rows[vapply(rows, function(i) { pp <- .ka5_var_parts(fr, i); !is.null(pp) && any(grepl("^A[a-z]*k|^Hk", pp$code)) }, NA)]
  if (!length(k)) return(.ka5_var_r(FALSE))
  th <- sum(fr$bottom_gof[k] - pmax(0, fr$top_gof[k]))
  if (is.na(th)) return(.ka5_var_r(NA, "the thickness of the Akh not known", "bottom_cm"))
  .ka5_var_r(th >= .ka5_var_thr("cultotrophic_min_cm"), sprintf("the %s %s cm thick", paste(fr$code[k], collapse = ", "), .ka5_var_cm(th)),
             layers = k)
}

# One criterion of a code.
.ka5_var_eval <- function(v, code, ctx) {
  e <- .ka5_var_entries()[[code]]
  switch(e$reader,
         humus_form = .ka5_var_humus_form(v, e),
         thin_organic = .ka5_var_thin_organic(v, e),
         base_saturation = .ka5_var_base_saturation(v, e, ctx),
         depth_hydromorphy = .ka5_var_depth_hydro(v, e),
         auen = .ka5_var_auen(v, e),
         relict = .ka5_var_relict(v, e, ctx),
         rewetted = .ka5_var_rewetted(v, e),
         colour = .ka5_var_colour(v, e, ctx),
         reddish = .ka5_var_reddish(v, e, ctx),
         iron_rich = .ka5_var_iron_rich(v, e),
         sulfate = .ka5_var_sulfate(v, e),
         cemented = .ka5_var_cemented(v, e),
         prefix = .ka5_var_prefix(v, e, ctx),
         wet_bleached = .ka5_var_wet_bleached(v, e),
         humus_rich = .ka5_var_humus_rich(v, e),
         podzolic = .ka5_var_podzolic(v, e),
         illuvial_c = .ka5_var_illuvial_c(v, e),
         colluvial = .ka5_var_colluvial(v, e),
         eroded = .ka5_var_eroded(v, e),
         arable = .ka5_var_arable(v, e),
         cultotrophic = .ka5_var_cultotrophic(v, e),
         rlang::abort(sprintf("Unknown KA5 variety reader '%s'.", e$reader)))
}

#' Tab. 47 (p. 269): the development depth of a B soil or an Ah/C soil, the
#' top of the first C under the GOF (the solum, p. 316). NULL for other
#' types; NA where the description ends above the C.
#' @noRd
.ka5_var_development_depth <- function(v, type) {
  dd <- .ka5_var_rules()$subvarieties$development_depth
  grp <- if (type %in% unlist(dd$b_soils$types)) dd$b_soils else if (type %in% unlist(dd$ac_soils$types)) dd$ac_soils else NULL
  if (is.null(grp)) return(NULL)
  fr <- v$frame
  rows <- .ka5_var_rows(v)
  cr <- rows[fr$master[rows] %in% "C"]
  if (!length(cr)) return(list(depth_cm = NA_real_, digit = NA_integer_, name = NA_character_,
                               note = "the C not described: the development depth not known (Tab. 47)"))
  d <- fr$top_gof[cr[1L]]
  for (cl in grp$classes) {
    r <- vapply(cl$range, function(z) if (is.null(z)) Inf else as.numeric(z), 0)
    if (!is.na(d) && d >= r[1L] && d < r[2L])
      return(list(depth_cm = d, digit = as.integer(cl$digit), name = as.character(cl$name),
                  note = sprintf("development depth %s cm: %s (Tab. 47, p. 269)", .ka5_var_cm(d), cl$name)))
  }
  list(depth_cm = d, digit = NA_integer_, name = NA_character_,
       note = sprintf("development depth %s cm: beyond the classes of Tab. 47", .ka5_var_cm(d)))
}


# --- names and symbols (rules I-VII) -----------------------------------------------------

# The adverbs of the subvarieties, written as the book prints them: flach
# vergleyt, tiefvergleyt (pp. 266, 272), flach and tief pseudovergleyt (p.
# 265), schwach, maessig, stark podsolig (p. 266), sehr basenarm, sehr
# basenreich (Tab. 48); a trailing space where the adverb stands apart.
.KA5_VAR_ADVERB <- list(s = c(`2` = "flach ", `4` = "tief "), g = c(`2` = "flach ", `4` = "tief"),
                        p = c(`2` = "schwach ", `3` = "m\u00e4\u00dfig ", `4` = "stark "),
                        dy = c(`1` = "sehr "), eu = c(`5` = "sehr "))

# The gender of a type (der, die, das), for the endings of the adjectives.
.ka5_var_gender <- function(type) {
  g <- unname(.KA5_TYPE_GENDER[type])
  if (length(g) != 1L || is.na(g)) "f" else g
}

# An adjective variety declined (podsolige, vergleyter, basenreiches), with
# the adverb of its subvariety.
.ka5_var_adjective <- function(code, digit, gender, adverb = NULL) {
  e <- .ka5_var_entries()[[code]]
  end <- c(m = "er", f = "e", n = "es")[[gender]]
  adv <- adverb %||% (if (!is.na(digit)) .KA5_VAR_ADVERB[[code]][as.character(digit)] else NULL)
  adv <- adv[!is.na(adv)]
  paste0(paste(adv, collapse = ""), e$word, end)
}

# The word of a deviation subtype in its combinations: its name without the
# name of the type (Brauneisengley -> Brauneisen), else the Tab. 49 word of
# its letter.
.ka5_var_dev_word <- function(dev, type) {
  te <- .ka5_type_entries()[[type]]
  nm <- .ka5_subtype_entries()[[dev]]$name %||% dev
  tn <- te$name %||% ""
  if (nzchar(tn) && endsWith(tolower(nm), tolower(tn)) && nchar(nm) > nchar(tn))
    return(substr(nm, 1L, nchar(nm) - nchar(tn)))
  w <- .ka5_var_rules()$subtype_suffixes[[substring(dev, nchar(type) + 1L)]]
  if (is.null(w)) return(dev)
  sub("\\.\\.\\..*$", "", w)
}

#' The combination of the subtype with the further subtypes of its type that
#' pass (Q21; Tab. 46 no. 17, rules II-IV): two transitions X-T and Y-T give
#' X-Y-T when one of X and Y is a hydromorphic type and not the first (IV); a
#' deviation Tz with a transition X-T gives X-Tz (II), with a deviation Tz1
#' gives Tz-Tz1 (III). NULL when nothing combines; notes on what cannot.
#' @param sub The subtype (the first in the order of Q21).
#' @param others The further subtypes of the type that pass, in that order.
#' @noRd
.ka5_var_combine <- function(sub, others, type) {
  ents <- .ka5_subtype_entries()
  te <- .ka5_type_entries()
  hyd <- as.character(unlist(.ka5_var_rules()$hydromorphic_types))
  kind <- function(cd) ents[[cd]]$kind %||% NA_character_
  xpart <- function(cd) sub("-.*$", "", cd)
  tname <- function(t) te[[t]]$name %||% t
  others <- others[vapply(others, function(cd) kind(cd) %in% c("transition", "deviation"), NA)]
  notes <- character(0)
  # a further subtype whose formula writes nothing the subtype's does not
  # (the Auengley under a Paternia-Gley) adds nothing: not joined
  tok <- function(cd) { t <- .ka5_var_formula_tokens(cd); unique(paste0(t$prefix, t$code)) }
  t1 <- tok(sub)
  inside <- vapply(others, function(cd) { tt <- tok(cd); length(tt) > 0L && all(tt %in% t1) }, NA)
  if (any(inside)) notes <- c(notes, sprintf("%s not joined: its formula writes nothing %s does not", paste(others[inside], collapse = ", "), sub))
  others <- others[!inside]
  if (!length(others)) return(if (length(notes)) list(symbol = NA_character_, notes = notes) else NULL)
  k1 <- kind(sub)
  parts <- character(0)
  dev <- NA_character_
  devs_more <- character(0)
  if (identical(k1, "transition")) {
    x <- xpart(sub)
    parts <- x
    tr <- others[vapply(others, kind, "") == "transition"]
    if (length(tr)) {
      done <- FALSE
      for (y in xpart(tr)) {
        if (done) { notes <- c(notes, sprintf("%s-%s not combined: two transitions are joined only once (rule IV)", y, type)); next }
        hx <- x %in% hyd; hy <- y %in% hyd
        if (xor(hx, hy)) { parts <- if (hx) c(y, x) else c(x, y); done <- TRUE }
        # the hydromorphic type may stand last, as the type (BB-LL-SS; Hugo,
        # 2026-11-07)
        else if (!hx && !hy && type %in% hyd) { parts <- c(x, y); done <- TRUE }
        else notes <- c(notes, sprintf("%s-%s and %s-%s not combined: rule IV joins three types only with one hydromorphic type, not first (p. 271)",
                                       x, type, y, type))
      }
    }
    dv <- others[vapply(others, kind, "") == "deviation"]
    if (length(dv)) { dev <- dv[1L]; devs_more <- dv[-1L] }
  } else if (identical(k1, "deviation")) {
    dv <- others[vapply(others, kind, "") == "deviation"]
    tr <- others[vapply(others, kind, "") == "transition"]
    if (length(tr)) notes <- c(notes, sprintf("%s not combined with the deviation %s (Q21 tests the transitions first)", paste(tr, collapse = ", "), sub))
    if (!length(dv)) return(if (length(notes)) list(symbol = NA_character_, notes = notes) else NULL)
    sym <- paste(c(rev(dv), sub), collapse = "-")
    nm <- paste0(paste0("(", vapply(rev(dv), .ka5_var_dev_word, "", type = type), ")", collapse = ""), ents[[sub]]$name)
    en <- sprintf("%s with features of the %s", ents[[sub]]$name_en %||% sub, paste(vapply(dv, function(z) tolower(ents[[z]]$name_en %||% z), ""), collapse = " and "))
    return(list(symbol = sym, name = nm, name_en = en, rule = "III", joined = dv, notes = notes))
  } else {
    return(NULL)
  }
  if (length(parts) < 2L && is.na(dev)) return(if (length(notes)) list(symbol = NA_character_, notes = notes) else NULL)
  if (length(devs_more)) notes <- c(notes, sprintf("%s not combined: one deviation joins a transition (rule II)", paste(devs_more, collapse = ", ")))
  tail_sym <- if (is.na(dev)) type else dev
  sym <- paste(c(parts, tail_sym), collapse = "-")
  nm <- paste(c(vapply(parts, tname, ""), if (is.na(dev)) tname(type) else paste0("(", .ka5_var_dev_word(dev, type), ")", tname(type))),
              collapse = "-")
  en <- paste(c(vapply(parts, function(t) te[[t]]$name_en %||% t, ""), te[[type]]$name_en %||% type), collapse = "-")
  if (!is.na(dev)) en <- sprintf("%s (%s)", en, tolower(ents[[dev]]$name_en %||% dev))
  list(symbol = sym, name = nm, name_en = en, rule = if (length(parts) >= 2L && !is.na(dev)) "IV, II" else if (length(parts) >= 2L) "IV" else "II",
       joined = c(if (length(parts) >= 2L) paste0(parts[parts != xpart(sub)], "-", type), dev[!is.na(dev)]), notes = notes)
}

# Insert a letter before the last occurrence of a type code in the upper part
# of a symbol (before any \ or /) -- or, with lower = TRUE, in the lower
# part (rule VI).
.ka5_var_insert <- function(core, type, letter, lower = FALSE) {
  m <- regexpr("[\\\\/]", core)
  up <- if (m > 0) substr(core, 1L, m - 1L) else core
  rest <- if (m > 0) substring(core, m) else ""
  if (lower && m > 0) {
    lowp <- substring(core, m + 1L)
    pos <- gregexpr(type, lowp, fixed = TRUE)[[1L]]
    if (pos[1L] < 0) return(core)
    p <- max(pos)
    return(paste0(up, substr(core, m, m), substr(lowp, 1L, p - 1L), letter, substring(lowp, p)))
  }
  pos <- gregexpr(type, up, fixed = TRUE)[[1L]]
  if (pos[1L] < 0) return(paste0(letter, core))
  p <- max(pos)
  paste0(substr(up, 1L, p - 1L), letter, substring(up, p), rest)
}

#' The symbol and the name of a variety (rules I-VII): the adjective letters
#' in alphabetical order, a point between them, the noun letters directly
#' before the subtype symbol; r and e before the letter they qualify; the
#' digit of a subvariety after its letter. The name: the adjectives declined
#' by the gender of the type, the nouns in brackets before the deciding part.
#' @param named data.frame(code, digit, target) of the varieties named.
#' @param base list(symbol, name, name_en, type, head_type, norm, overlay
#'   (list(kind, symbol, upper, lower)) or NULL).
#' @noRd
.ka5_var_format <- function(named, base) {
  ents <- .ka5_var_entries()
  core <- base$symbol
  r_target <- if ("r" %in% named$code) named$target[named$code == "r"][1L] else NA_character_
  if ("r" %in% named$code && is.na(r_target)) r_target <- "T"
  lower_t <- isTRUE(base$overlay$kind == "moor_cover")
  if ("e" %in% named$code) core <- .ka5_var_insert(core, base$type, "e", lower = lower_t)
  if (!is.na(r_target) && r_target %in% c("T", "X")) {
    core <- .ka5_var_insert(core, if (r_target == "T") base$type else base$x_type %||% base$type, "r", lower = lower_t && r_target == "T")
  }
  kinds <- vapply(named$code, function(cd) ents[[cd]]$kind, "")
  inline <- named$code %in% c("e", if (!is.na(r_target) && r_target %in% c("T", "X")) "r")
  adj <- named[kinds == "adjective" & !inline & named$code != "r", , drop = FALSE]
  adj <- adj[order(adj$code), , drop = FALSE]
  nou <- named[kinds == "noun", , drop = FALSE]
  nou <- nou[order(nou$code), , drop = FALSE]
  lab <- function(d) paste0(d$code, ifelse(is.na(d$digit), "", d$digit))
  letters <- lab(adj)
  if (!is.na(r_target) && r_target %in% c("g", "s")) {
    k <- match(r_target, adj$code)
    letters <- if (is.na(k)) c("r", letters) else append(letters, "r", after = k - 1L)
  } else if (!is.na(r_target) && r_target == "soil") letters <- sort(c(letters, "r"))
  letters <- c(letters, lab(nou))
  # a point also before an r or e written at the head of the core (g.rGGn)
  symbol <- if (length(letters) && grepl("^[a-z]", core)) paste(c(letters, core), collapse = ".")
            else paste0(paste(letters, collapse = "."), core)
  # the name
  g <- .ka5_var_gender(base$head_type)
  words <- character(0)
  adj_all <- named[kinds == "adjective", , drop = FALSE]
  adj_all <- adj_all[order(adj_all$code), , drop = FALSE]
  for (k in seq_len(nrow(adj_all))) {
    cd <- adj_all$code[k]
    if (cd == "r" && !is.na(r_target) && r_target %in% c("g", "s")) next
    adv <- if (!is.na(r_target) && r_target %in% c("g", "s") && cd == r_target) {
      a <- .KA5_VAR_ADVERB[[cd]][as.character(adj_all$digit[k])]
      c("reliktisch ", a[!is.na(a)])
    } else NULL
    words <- c(words, .ka5_var_adjective(cd, adj_all$digit[k], g, adv))
  }
  nouns <- if (nrow(nou)) paste0("(", vapply(nou$code, function(cd) ents[[cd]]$word, ""), ")", collapse = "") else ""
  nm <- base$name
  if (nzchar(nouns)) {
    if (!is.null(base$overlay) && !lower_t) {
      # before the upper soil's name (flache (Acker)Braunerde ueber Terra fusca)
      nm <- sub(base$upper_name, paste0(nouns, base$upper_name), nm, fixed = TRUE)
    } else if (isTRUE(base$norm) && is.null(base$overlay) && startsWith(nm, "Norm")) {
      rest <- substring(nm, 5L)
      nm <- paste0(nouns, toupper(substr(rest, 1L, 1L)), substring(rest, 2L))
    } else {
      cut <- regexpr("-[^-]*$", nm)
      nm <- if (cut > 0) paste0(substr(nm, 1L, cut), nouns, substring(nm, cut + 1L)) else paste0(nouns, nm)
    }
  }
  name <- paste(c(words, nm), collapse = " ")
  en_words <- c(vapply(adj_all$code, function(cd) ents[[cd]]$name_en, ""), vapply(nou$code, function(cd) ents[[cd]]$name_en, ""))
  name_en <- paste(c(if (length(en_words)) paste(en_words, collapse = ", "), base$name_en), collapse = " ")
  list(symbol = symbol, name = name, name_en = name_en)
}


# --- the result --------------------------------------------------------------------------

#' The variety level of a KA5 result (Q1, Q14, Q21; Tab. 46-50, rules I-VII)
#'
#' After the subtype: every code Tab. 50 offers the subtype is tested; the
#' named varieties, the combination with the further subtypes that pass and
#' the overlay become rows of level "variety"; the full name in `name`; the
#' symbol and the details in trace$variety; every code in trace$varieties.
#' The levels above the variety are not changed.
#' @noRd
.ka5_variety_notes <- function(res, pedon, rules, view) {
  lv <- res$levels
  if (!is.data.frame(lv) || !nrow(lv)) return(res)
  code_of <- function(l) { k <- which(lv$level == l); if (length(k)) lv$code[k[1L]] else NA_character_ }
  ty <- code_of("types"); cls <- code_of("classes"); sub <- code_of("subtypes")
  if (is.na(ty)) return(res)
  ov <- res$trace$overlay
  has_ov <- !is.null(ov) && !is.na(ov$code %||% NA_character_)
  if (is.na(sub)) return(res)
  te <- .ka5_type_entries()
  v <- view
  key <- .ka5_var_sub_key(sub)
  ent <- if (!is.na(key)) .ka5_subtype_entries()[[key]] else NULL
  skind <- ent$kind %||% NA_character_
  x_type <- if (identical(skind, "transition")) sub("-.*$", "", gsub("[][]", "", sub)) else NA_character_
  ctx <- list(type = ty, class = cls, subtype = sub, kind = skind, x_type = x_type, memo = new.env(parent = emptyenv()))
  # every code: offered by Tab. 50 (and note 1), then tested
  tab <- if (!is.na(sub)) .ka5_var_offer(sub) else {
    cols <- .ka5_var_columns()
    data.frame(code = cols, offered = FALSE, source = NA_character_, page = NA_integer_, stringsAsFactors = FALSE)
  }
  ents <- .ka5_var_entries()
  tab$name <- vapply(tab$code, function(cd) as.character(ents[[cd]]$printed), "")
  tab$kind <- vapply(tab$code, function(cd) as.character(ents[[cd]]$kind), "")
  tab$passed <- NA; tab$digit <- NA_integer_; tab$status <- NA_character_; tab$target <- NA_character_
  tab$notes <- I(rep(list(character(0)), nrow(tab))); tab$missing <- I(rep(list(character(0)), nrow(tab)))
  tab$layers <- I(rep(list(integer(0)), nrow(tab)))
  # the r of a deep g or s (r.gLFn, rule VI, p. 272) where Tab. 50 does not
  # mark the r but offers the g or s (soilKey's reading)
  kr <- which(tab$code == "r")
  r_via <- !tab$offered[kr] && any(tab$offered[tab$code %in% c("g", "s")])
  if (r_via) { tab$offered[kr] <- TRUE; tab$source[kr] <- "rule VI (with g or s)" }
  for (k in seq_len(nrow(tab))) {
    if (!tab$offered[k]) {
      tab$status[k] <- if (identical(tab$source[k], "in the formula")) "in the formula" else "not offered"
      tab$passed[k] <- FALSE
      next
    }
    r <- tryCatch(.ka5_var_eval(v, tab$code[k], ctx),
                  error = function(e) .ka5_var_r(NA, paste("error:", conditionMessage(e))))
    tab$passed[k] <- r$passed; tab$digit[k] <- r$digit; tab$target[k] <- r$target
    tab$notes[[k]] <- r$notes; tab$missing[[k]] <- r$missing; tab$layers[[k]] <- r$layers
    tab$status[k] <- if (isTRUE(r$passed)) "named" else if (isFALSE(r$passed)) "not named" else "undecided"
  }
  if (r_via && isTRUE(tab$passed[kr])) {
    tg <- tab$target[kr]
    if (!(tg %in% c("g", "s") && isTRUE(tab$passed[tab$code == tg]))) {
      tab$passed[kr] <- FALSE; tab$status[kr] <- "not named"
      tab$notes[[kr]] <- c(tab$notes[[kr]], "Tab. 50 does not mark the r here: named only on a deep g or s (rule VI)")
    }
  } else if (r_via && is.na(tab$passed[kr])) tab$status[kr] <- "undecided"
  # a and g on the same deep aGo: only a (Hugo, 2026-11-07)
  ka <- which(tab$code == "a"); kg <- which(tab$code == "g")
  if (tab$status[ka] %in% "named" && tab$status[kg] %in% "named" && length(intersect(tab$layers[[ka]], tab$layers[[kg]]))) {
    tab$status[kg] <- "not named"; tab$passed[kg] <- FALSE
    tab$notes[[kg]] <- c(tab$notes[[kg]], "the deep G is the aG of the variety a: named once, as a")
  }
  # the r qualifies a g or s only when that one is named; else the type (review
  # D2, Hugo 2026-11-07)
  if (tab$status[kr] %in% "named" && tab$target[kr] %in% c("g", "s") && !(tab$status[tab$code == tab$target[kr]] %in% "named"))
    tab$target[kr] <- "T"
  named <- tab[tab$status %in% "named", c("code", "digit", "target"), drop = FALSE]
  # the base-saturation class goes to the trace only, not to the symbol or the
  # name (Hugo, 2026-11-07)
  named$digit[named$code %in% c("eu", "m", "dy")] <- NA_integer_
  und <- tab[tab$status %in% "undecided", , drop = FALSE]
  # the combination with the further subtypes (Q21)
  comb <- NULL
  sp <- res$trace$subtypes_passing
  if (!is.na(sub) && is.data.frame(sp) && nrow(sp) && !startsWith(sub, "[")) {
    others <- sp$code[sp$passed %in% TRUE & !(sp$kind %in% "norm") & sp$code != sub & !startsWith(sp$code, "[")]
    if (length(others)) comb <- .ka5_var_combine(sub, others, ty)
  }
  # the base of the name and the symbol
  norm <- identical(skind, "norm")
  sub_name <- if (!is.na(sub)) lv$name[which(lv$level == "subtypes")[1L]] else te[[ty]]$name %||% ty
  sub_en <- if (!is.na(sub) && "name_en" %in% names(lv)) lv$name_en[which(lv$level == "subtypes")[1L]] else te[[ty]]$name_en %||% ty
  base <- list(symbol = if (is.na(sub)) ty else sub, name = sub_name, name_en = sub_en, type = ty, head_type = ty,
               norm = norm, x_type = x_type)
  if (!is.null(comb) && !is.na(comb$symbol)) base <- utils::modifyList(base, list(symbol = comb$symbol, name = comb$name, name_en = comb$name_en, norm = FALSE))
  ov_row <- NULL
  if (has_ov) {
    lw <- .KA5_OVERLAY_LOWER[[ov$lower]]
    ln <- if (!is.null(lw)) lw[1L] else te[[ov$lower]]$name %||% ov$lower
    len <- if (!is.null(lw)) lw[2L] else tolower(te[[ov$lower]]$name_en %||% ov$lower)
    if (identical(ov$kind, "moor_cover")) {
      sym <- ov$code; un <- te[[ov$upper]]$name %||% ov$upper; nm <- ov$name; nen <- ov$name_en
    } else {
      upper_sym <- if (norm || is.na(sub)) ty else base$symbol
      un <- if (norm || is.na(sub)) te[[ty]]$name %||% ty else base$name
      sym <- paste0(upper_sym, ov$symbol, ov$lower)
      nm <- if (identical(ov$symbol, "\\")) {
        adj <- switch(.ka5_var_gender(ty), m = "flacher", f = "flache", n = "flaches")
        sprintf("%s %s \u00fcber %s", adj, un, ln)
      } else sprintf("%s \u00fcber %s", un, ln)
      nen <- sprintf("%s%s over %s", if (identical(ov$symbol, "\\")) "shallow " else "", if (norm || is.na(sub)) tolower(te[[ty]]$name_en %||% ty) else tolower(base$name_en), len)
    }
    # the overlay changes neither the levels, the name nor provisional_because
    # (Hugo's decision of 0.9.364, confirmed 2026-11-07): trace only
    ov_row <- list(code = sym, name = nm, name_en = nen, kind = ov$kind)
  }
  if (!nrow(named) && (is.null(comb) || is.na(comb$symbol))) {
    res$trace$varieties <- tab
    res$trace$variety <- list(symbol = NA_character_, name = NA_character_, named = character(0), undecided = und$code,
                              combination = comb, overlay = ov_row, base_saturation = ctx$memo$bs,
                              development_depth = .ka5_var_development_depth(v, ty))
    if (nrow(und)) res$warnings <- c(res$warnings, .ka5_var_open_note(und))
    if (!is.null(comb$notes) && length(comb$notes)) res$warnings <- c(res$warnings, paste0("Variety (Tab. 46 no. 17): ", comb$notes))
    return(res)
  }
  fm <- .ka5_var_format(named, base)
  # the rows of the level
  rows <- list()
  if (nrow(named)) for (k in seq_len(nrow(named))) {
    cd <- named$code[k]
    e <- ents[[cd]]
    word <- if (e$kind == "noun") sprintf("(%s)", e$word)
            else .ka5_var_adjective(cd, named$digit[k], .ka5_var_gender(base$head_type))
    rows[[length(rows) + 1L]] <- list(code = paste0(cd, if (!is.na(named$digit[k])) named$digit[k] else ""), name = word,
                                      name_en = as.character(e$name_en))
  }
  if (!is.null(comb) && !is.na(comb$symbol)) rows[[length(rows) + 1L]] <- list(code = comb$symbol, name = comb$name, name_en = comb$name_en)
  add <- lv[rep(NA_integer_, length(rows)), , drop = FALSE]
  add$level <- "variety"
  add$code <- vapply(rows, `[[`, "", "code")
  add$name <- vapply(rows, `[[`, "", "name")
  if ("name_en" %in% names(lv)) add$name_en <- vapply(rows, `[[`, "", "name_en")
  add$symbol <- fm$symbol
  if (!"symbol" %in% names(lv)) lv$symbol <- NA_character_
  rownames(add) <- NULL
  lv <- rbind(lv[lv$level != "variety", , drop = FALSE], add)
  rownames(lv) <- NULL
  res$levels <- lv
  res$name <- fm$name
  q <- res$qualifiers %||% list()
  q$variety <- add$name
  res$qualifiers <- q
  res$trace$varieties <- tab
  res$trace$variety <- list(symbol = fm$symbol, name = fm$name, name_en = fm$name_en, named = named$code,
                            undecided = und$code, combination = comb, overlay = ov_row,
                            base_saturation = ctx$memo$bs, development_depth = .ka5_var_development_depth(v, ty))
  res$warnings <- c(res$warnings, sprintf("Variety (Tab. 46, 49, 50; Q1): %s (%s).", fm$name, fm$symbol))
  if (!is.null(comb$notes) && length(comb$notes)) res$warnings <- c(res$warnings, paste0("Variety (Tab. 46 no. 17): ", comb$notes))
  if (nrow(und)) res$warnings <- c(res$warnings, .ka5_var_open_note(und))
  res
}

# The note on the varieties the data leave open.
.ka5_var_open_note <- function(und) {
  sprintf("Varieties left open by the data, not named (a variety need not exhaust its criteria, p. 263): %s.",
          paste(sprintf("%s (%s)", und$code, vapply(seq_len(nrow(und)), function(k) {
            m <- unique(unlist(und$missing[[k]]))
            if (length(m)) paste(m, collapse = ", ") else "undecided"
          }, "")), collapse = "; "))
}


#' Read a KA5 variety symbol (rules I-VII, pp. 270-272)
#'
#' Splits a symbol written as the KA5 writes it (pBBn, p.sBBn, g4.p2BB,
#' r.gLFn, BB-rGG, LF-eBB, vGGn, YK/BB, GG\\HN) into its variety letters with
#' their digits and the subtype (or combination, or overlay) symbol.
#' @param x A symbol.
#' @return A list: \code{varieties} (data.frame code, digit), \code{core} (the
#'   symbol after the letters, the r and e inside it taken out),
#'   \code{inline} (the r and e written inside the core), \code{known} (all
#'   the letters are codes of Tab. 49).
#' @noRd
.ka5_var_parse_symbol <- function(x) {
  x <- trimws(as.character(x)[1L])
  m <- regmatches(x, regexec("^((?:[a-z]+[0-9]?\\.)*[a-z]+[0-9]?)?([A-Z\u00dc].*)$", x, perl = TRUE))[[1L]]
  if (!length(m)) return(list(varieties = data.frame(code = character(0), digit = integer(0)), core = x, inline = character(0), known = FALSE))
  lead <- m[2L]; core <- m[3L]
  cols <- .ka5_var_columns()
  toks <- if (nzchar(lead)) strsplit(lead, ".", fixed = TRUE)[[1L]] else character(0)
  # the last token runs into the core: split the longest known code off its
  # end (pBB: p; p.moBB: mo)
  vc <- sub("[0-9]$", "", toks); dg <- suppressWarnings(as.integer(sub("^[a-z]+", "", toks)))
  # the letters stand apart (separated by points); the last runs into the
  # core (pBBn: p, p.moBB: mo)
  inline <- character(0)
  # r and e written inside the core, before a type (BB-rGG, LF-eBB)
  for (L in c("r", "e")) {
    pat <- sprintf("(^|-|[\\\\/])%s(?=[A-Z\u00dc])", L)
    if (grepl(pat, core, perl = TRUE)) {
      inline <- c(inline, L)
      core <- gsub(pat, "\\1", core, perl = TRUE)
    }
  }
  list(varieties = data.frame(code = vc, digit = dg, stringsAsFactors = FALSE), core = core, inline = inline,
       known = all(vc %in% cols))
}

#' The name of a subtype, combination or type symbol (rules II-IV): each part
#' a type (BB: Braunerde), a Norm subtype (BBn: Normbraunerde, alone), or a
#' deviation subtype whose word goes in brackets before its type
#' (BB-GGe: Braunerde-(Brauneisen)Gley; GGh-GGc: (Humus)Kalkgley, two
#' deviations of one type joined).
#' @return list(name, name_en, head_type, x_type, norm, symbol)
#' @noRd
.ka5_var_compose <- function(core) {
  ents <- .ka5_subtype_entries()
  te <- .ka5_type_entries()
  sym <- gsub("[][]", "", core)
  if (!is.null(ents[[core]])) {
    e <- ents[[core]]
    return(list(name = e$name, name_en = e$name_en %||% core, head_type = e$type, symbol = core,
                x_type = if (identical(e$kind, "transition")) sub("-.*$", "", sym) else NA_character_,
                norm = identical(e$kind, "norm")))
  }
  parts <- strsplit(sym, "-", fixed = TRUE)[[1L]]
  read <- lapply(parts, function(p) {
    if (!is.null(te[[p]])) return(list(type = p, word = NA_character_))
    e <- ents[[p]]
    if (!is.null(e) && identical(e$kind, "norm")) return(list(type = e$type, word = NA_character_))
    if (!is.null(e) && identical(e$kind, "deviation")) return(list(type = e$type, word = .ka5_var_dev_word(p, e$type), dev = p))
    list(type = p, word = NA_character_)
  })
  grp <- list()
  for (r in read) {
    n <- length(grp)
    if (n && !is.na(r$word) && identical(grp[[n]]$type, r$type) && length(grp[[n]]$words)) {
      grp[[n]]$words <- c(grp[[n]]$words, r$word)
      grp[[n]]$devs <- c(grp[[n]]$devs, r$dev)
    } else grp[[n + 1L]] <- list(type = r$type, words = if (is.na(r$word)) character(0) else r$word,
                                 devs = if (is.null(r$dev)) character(0) else r$dev)
  }
  nm <- vapply(grp, function(g) {
    tn <- te[[g$type]]$name %||% g$type
    if (!length(g$words)) return(tn)
    k <- length(g$devs)
    # the last deviation keeps its own name, the others in brackets before it
    if (k > 1L) paste0(paste0("(", g$words[-k], ")", collapse = ""), ents[[g$devs[k]]]$name)
    else paste0("(", g$words[k], ")", tn)
  }, "")
  en <- paste(vapply(grp, function(g) te[[g$type]]$name_en %||% g$type, ""), collapse = "-")
  list(name = paste(nm, collapse = "-"), name_en = en, head_type = grp[[length(grp)]]$type, symbol = core,
       x_type = if (length(grp) > 1L) grp[[1L]]$type else NA_character_, norm = FALSE)
}

#' Read a KA5 variety symbol and name it (rules I-VII): the variety letters
#' with their digits, the r and e written before a type (rule VI), the
#' subtype, combination or overlay after them. Gives the symbol back as
#' soilKey writes it and the name, so a recorded symbol reads as its unit.
#' @param x A symbol (pBBn, p.sBBn, g4.p2BB, r.gLFn, BB-rGG, LF-eBB, vGGn,
#'   GGh-GGc, PP-GG-BB, YK/BB, GG\\HN ...).
#' @return list(varieties (data.frame code, digit), core, symbol, name,
#'   name_en, known).
#' @noRd
.ka5_var_read_symbol <- function(x) {
  ps <- .ka5_var_parse_symbol(x)
  core <- ps$core
  te <- .ka5_type_entries()
  m <- regexpr("[\\\\/]", core)
  base <- NULL
  if (m > 0) {
    up <- substr(core, 1L, m - 1L); sy <- substr(core, m, m); low <- substring(core, m + 1L)
    cu <- .ka5_var_compose(up)
    lw <- .KA5_OVERLAY_LOWER[[low]]
    ln <- if (!is.null(lw)) lw[1L] else te[[low]]$name %||% low
    un <- cu$name
    nm <- if (sy == "\\") sprintf("%s %s \u00fcber %s", switch(.ka5_var_gender(cu$head_type), m = "flacher", f = "flache", n = "flaches"), un, ln)
          else sprintf("%s \u00fcber %s", un, ln)
    base <- list(symbol = core, name = nm, name_en = sprintf("%s over %s", cu$name_en, if (!is.null(lw)) lw[2L] else tolower(te[[low]]$name_en %||% low)),
                 type = cu$head_type, head_type = cu$head_type, norm = cu$norm, x_type = cu$x_type, upper_name = un,
                 overlay = list(kind = "overlay", symbol = sy, upper = cu$head_type, lower = low))
  } else {
    cu <- .ka5_var_compose(core)
    base <- list(symbol = core, name = cu$name, name_en = cu$name_en, type = cu$head_type, head_type = cu$head_type,
                 norm = cu$norm, x_type = cu$x_type)
  }
  v <- ps$varieties
  named <- data.frame(code = v$code, digit = v$digit, target = rep(NA_character_, nrow(v)), stringsAsFactors = FALSE)
  # an r written before a g or s qualifies it (r.gLFn); an r or e inside the
  # core qualifies the type or the transitional part (BB-rGG, LF-eBB)
  kr <- which(named$code == "r")
  if (length(kr) && kr[1L] < nrow(named) && named$code[kr[1L] + 1L] %in% c("g", "s")) named$target[kr[1L]] <- named$code[kr[1L] + 1L]
  # an r or e as the last letter stands directly before the type (rGGn,
  # g.rGGn, rule VI)
  nl <- nrow(named)
  if (nl && named$code[nl] %in% c("r", "e") && is.na(named$target[nl])) named$target[nl] <- "T"
  for (L in ps$inline) {
    tg <- if (L == "r") {
      pos_r <- regexpr(sprintf("(^|-)r(?=%s)", base$type), x, perl = TRUE)
      if (pos_r > 0) "T" else "X"
    } else NA_character_
    named <- rbind(named, data.frame(code = L, digit = NA_integer_, target = tg, stringsAsFactors = FALSE))
  }
  fm <- .ka5_var_format(named, base)
  list(varieties = v, core = core, symbol = fm$symbol, name = fm$name, name_en = fm$name_en, known = ps$known)
}


# --- the examples --------------------------------------------------------------------------

# The example profile of each variety code (make_ka5_example(variety = ...)):
# the subtype it is a variety of, and the profile (a printed formula of the
# subtype with the variety's horizon or record; NULL keeps the subtype's own
# example). soilKey's profiles, not real ones.
.KA5_VARIETY_PROFILES <- list(
  mu = list(base = "BBn", site = list(humus_form = "mull")),
  mo = list(base = "BBn", site = list(humus_form = "moder")),
  ro = list(base = "PPn", site = list(humus_form = "Rohhumus")),
  ta = list(base = "RRn", site = list(humus_form = "Tangel")),
  mf = list(base = "GNn", site = list(humus_form = "Feuchtmull")),
  md = list(base = "GNn", site = list(humus_form = "Feuchtmoder")),
  mr = list(base = "SSn", site = list(humus_form = "Feuchtrohhumus")),
  o = list(base = "GNn", spec = "Aa 0-6 / Go-Ah 6-25 / Gr 25-100"),
  n = list(base = "GNn", spec = "nH 0-6 / Go-Ah 6-25 / Gr 25-100"),
  u = list(base = "GNn", spec = "uH 0-6 / Go-Ah 6-25 / Gr 25-100"),
  h = list(base = "SGn", spec = "hH 0-6 / Sw-Ah 6-20 / Srw 20-40 / IISrd 40-100"),
  eu = list(base = "BBn", spec = "Ah 0-15 / Bv 15-50 / C 50-100", cols = list(bs_pct = c(NA, 60, NA))),
  m = list(base = "BBn", spec = "Ah 0-15 / Bv 15-50 / C 50-100", cols = list(bs_pct = c(NA, 35, NA))),
  dy = list(base = "BBn", spec = "Ah 0-15 / Bv 15-50 / C 50-100", cols = list(bs_pct = c(NA, 10, NA))),
  s = list(base = "BBn", spec = "Ah 0-15 / Bv 15-50 / C 50-90 / Sw 90-120 / IISd 120-150"),
  g = list(base = "BBn", spec = "Ah 0-15 / Bv 15-50 / C 50-140 / Go 140-170 / Gr 170-200"),
  a = list(base = "BBn", spec = "Ah 0-15 / Bv 15-50 / aC 50-90 / aGo 90-150"),
  r = list(base = "GGn", spec = "Ah 0-25 / rGo 25-60 / rGr 60-120"),
  w = list(base = "KVn", site = list(rewetted = TRUE)),
  b = list(base = "RRn", spec = "bAh> 0-20 / cmC 20-60", cols = list(bs_pct = c(90, NA))),
  gb = list(base = "YEn", spec = "Ah 0-30 / gbE 30-70 / IIfAh 70-80 / IIBhs 80-100"),
  gr = list(base = "YEn", spec = "Ah 0-30 / gE 30-70 / IIfAh 70-80 / IIBhs 80-100"),
  rt = list(base = "LLn", spec = "Ah 0-25 / Al 25-45 / Bt 45-90 / C 90-130",
            cols = list(munsell_hue_moist = c("10YR", "10YR", "5YR", "10YR"), munsell_chroma_moist = c(2, 4, 6, 4))),
  ei = list(base = "BBn", spec = "Ah 0-15 / Bv 15-50 / C 50-100",
            cols = list(munsell_hue_moist = c("10YR", "7.5YR", "10YR"), munsell_chroma_moist = c(2, 4, 4))),
  sf = list(base = "MOn", cols = list(jarosite_present = c(NA, TRUE, NA))),
  vf = list(base = "PPn", spec = "Ahe 0-10 / Ae 10-25 / Bsh 25-30 / Bhms 30-50 / C 50-100"),
  og = list(base = "GGn", spec = "oAh 0-25 / Go 25-60 / Gr 60-120"),
  c = list(base = "GGn", spec = "Ah 0-25 / eGo 25-60 / Gr 60-120"),
  z = list(base = "BBn", spec = "Ah 0-15 / zBv 15-50 / C 50-100", cols = list(ec_dS_m = c(0.3, 2, 0.3))),
  i = list(base = "SSn", spec = "Ah 0-20 / Sew 20-40 / IISd 40-100"),
  x = list(base = "RRn", spec = "Ah> 0-20 / cmC 20-60", cols = list(bs_pct = c(90, NA), oc_pct = c(6, NA))),
  p = list(base = "BBn", spec = "Aeh 0-10 / Bv 10-50 / C 50-100"),
  l = list(base = "BBh", spec = "Ah 0-20 / Ah-Bv 20-45 / Bv 45-70 / Ctv 70-100"),
  d = list(base = "BBn", spec = "Ah 0-15 / Bv 15-50 / Cbtv 50-100"),
  k = list(base = "BBn", spec = "Ah 0-15 / M 15-30 / Bv 30-60 / C 60-100"),
  e = list(base = "BBn", site = list(erosion_degree = "Eg2")),
  v = list(base = "BBn", spec = "Ap 0-25 / Bv 25-50 / C 50-100"),
  ku = list(base = "BBn", spec = "Akh 0-15 / Bv 15-50 / C 50-100")
)

#' The example profile of a KA5 variety: the profile of its subtype with the
#' variety's horizon or record.
#' @param code A variety code of Tab. 49 (p, g, eu, mo ...).
#' @noRd
.ka5_variety_example <- function(code, route = c("notation", "data")) {
  route <- match.arg(route)
  pr <- .KA5_VARIETY_PROFILES[[code]]
  if (is.null(pr)) rlang::abort(sprintf("No KA5 variety example '%s' (the codes of Tab. 49: %s).", code,
                                        paste(names(.KA5_VARIETY_PROFILES), collapse = ", ")))
  base <- pr$base
  spec <- pr$spec %||% (.KA5_SUBTYPE_PROFILES[[base]] %||% .KA5_SUBTYPE_PROFILES_WET[[base]])
  if (is.character(spec)) spec <- list(spec)
  for (k in names(pr$cols)) spec[[k]] <- pr$cols[[k]]
  if (length(pr$site)) spec$site <- utils::modifyList(spec$site %||% list(), pr$site)
  .ka5_subtype_example(base, route, spec = spec, id = paste0("variety-", code))
}

# The subtype each variety example keys to.
.ka5_variety_example_subtype <- function(code) .KA5_VARIETY_PROFILES[[code]]$base
