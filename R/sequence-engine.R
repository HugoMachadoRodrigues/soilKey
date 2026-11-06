# =============================================================================
# v0.9.334 -- profile formulas and a matcher of horizon sequences.
#
# Some classifications name a soil type by the sequence of its diagnostic
# horizons, not by a key: the Russian Field Guide of 2008 draws each type as a
# profile formula ("O-EL-BEL-BT-C", chapter 4). Two generic pieces, used by no
# system yet:
#
#   .parse_profile_formula() reads a formula string into slots: the
#     alternative codes of each horizon, whether it is optional (written in
#     parentheses), its features (required, or only typical), its role
#     (surface, middle, parent), buried profiles ([ABC], {ABC}), a turbated
#     composition (TUR[AU+SEL+ASN]), rock flags (CG, CQ, DG) and rock marks
#     (C~~, C", C^^, C"", C=, C..);
#   match_horizon_sequence() matches the identified horizons of a profile
#     against one formula, three-valued; .match_schemes() runs a list of
#     formulas, picks the most specific match (.pick_scheme()) and, when none
#     matches, ranks the nearest schemes by edit distance (.nearest_schemes()).
#
# The vocabulary of horizon codes and features is the caller's: a system
# passes its own masters, features and parent symbols.
# =============================================================================

# Rock marks after C or TT (lithology and genesis of the parent rock), the
# longest first so that a doubled quote is not read as two marks.
.FORMULA_MARKS <- c("\"\"", "~~", "^^", "..", "\"", "=")
# Feature symbols that are not letters: permafrost (U+22A5) and cryoturbation.
.FORMULA_SYMBOLS <- c("\u22a5", "@")
# Rock and parent symbols: their slots are tested only when flagged.
.FORMULA_PARENTS <- c("C", "D", "CLM", "R")
# The two buried-profile slots.
.FORMULA_BURIED <- c(square = "[ABC]", curly = "{ABC}")
# Roman numerals of a lithological layer, II to IX (V and X alone are horizon
# letters in some notations, so they are not read as numerals).
.FORMULA_ROMAN <- c("VIII", "VII", "III", "IX", "VI", "IV", "II")
# Codes of the matcher that stand for a class of codes, for an undecided
# layer: "a code in play that no slot names and no other layer has", "a code
# foreign to the match", and "no diagnostic horizon".
.MHS_REPS <- c("<p>", "<f>", "<none>")


# Cyrillic letters that look like Latin ones, as the book's own text and
# Russian databases mix them into the indices (TO, CRH, AO, ca, ek ...).
.FORMULA_CYRILLIC <- c(
  "\u0410" = "A", "\u0412" = "B", "\u0415" = "E", "\u041a" = "K", "\u041c" = "M",
  "\u041d" = "H", "\u041e" = "O", "\u0420" = "P", "\u0421" = "C", "\u0422" = "T",
  "\u0425" = "X", "\u0423" = "Y",
  "\u0430" = "a", "\u0435" = "e", "\u043e" = "o", "\u0440" = "p", "\u0441" = "c",
  "\u0443" = "y", "\u0445" = "x", "\u043a" = "k", "\u043c" = "m", "\u0442" = "t",
  "\u043d" = "h", "\u0432" = "b")

# Typographic variants folded before a formula is read: Cyrillic look-alikes
# to Latin; curly quotes, primes and doubled single quotes to a straight
# double quote (the ash mark), a single prime or right quote likewise; the
# Symbol-font perpendicular (U+F05E) to U+22A5; dashes and the minus sign to a
# hyphen; spaces removed.
.formula_normalise <- function(s) {
  s <- enc2utf8(as.character(s))
  s <- chartr(paste(names(.FORMULA_CYRILLIC), collapse = ""),
              paste(.FORMULA_CYRILLIC, collapse = ""), s)
  for (q in c("\u2032\u2032", "\u2019\u2019", "''", "\u201c", "\u201d", "\u201e",
              "\u2033", "\u02ba", "``", "\u2032", "\u2019"))
    s <- gsub(q, "\"", s, fixed = TRUE)
  s <- gsub("\uf05e", "\u22a5", s, fixed = TRUE)
  for (d in c("\u2010", "\u2011", "\u2012", "\u2013", "\u2014", "\u2212"))
    s <- gsub(d, "-", s, fixed = TRUE)
  gsub("[[:space:]]+", "", s)
}

.formula_abort <- function(fmt, ..., formula) {
  rlang::abort(sprintf(paste0(fmt, " in the profile formula '%s'."), ..., formula))
}

# Position of the bracket closing the one at position i of the character
# vector ch (any of ( [ {, counted together).
.formula_close <- function(ch, i, formula) {
  depth <- 0L
  for (k in seq.int(i, length(ch))) {
    if (ch[k] %in% c("(", "[", "{")) depth <- depth + 1L
    else if (ch[k] %in% c(")", "]", "}")) {
      depth <- depth - 1L
      if (depth == 0L) return(k)
    }
  }
  .formula_abort("Unbalanced brackets", formula = formula)
}

# Split a formula on "-" outside brackets.
.formula_items <- function(x, formula) {
  ch <- strsplit(x, "", fixed = TRUE)[[1L]]
  depth <- 0L
  cut <- integer(0)
  for (k in seq_along(ch)) {
    if (ch[k] %in% c("(", "[", "{")) depth <- depth + 1L
    else if (ch[k] %in% c(")", "]", "}")) depth <- depth - 1L
    if (depth < 0L) .formula_abort("Unbalanced brackets", formula = formula)
    if (ch[k] == "-" && depth == 0L) cut <- c(cut, k)
  }
  if (depth != 0L) .formula_abort("Unbalanced brackets", formula = formula)
  from <- c(1L, cut + 1L)
  to <- c(cut - 1L, length(ch))
  items <- mapply(function(a, b) if (b >= a) paste(ch[a:b], collapse = "") else "",
                  from, to, USE.NAMES = FALSE)
  if (any(!nzchar(items))) .formula_abort("Empty item", formula = formula)
  items
}

# Split a run of feature letters into known features, longest first (with
# backtracking); NULL when it cannot be split. The split of every tail of the
# run is computed once, from the end (v0.9.335: the recursive search took
# exponential time on a long run that cannot be split, such as BTrrr...rj).
.formula_decompose <- function(x, vocab) {
  n <- nchar(x)
  if (!n) return(character(0))
  maxlen <- max(c(1L, nchar(vocab)))
  # tail[[i]]: the split of the letters from i on, or NULL
  tail <- vector("list", n + 1L)
  ok <- c(logical(n), TRUE)
  tail[[n + 1L]] <- character(0)
  for (i in rev(seq_len(n))) {
    for (k in rev(seq_len(min(maxlen, n - i + 1L)))) {
      pre <- substr(x, i, i + k - 1L)
      if (ok[i + k] && pre %in% vocab) {
        tail[[i]] <- c(pre, tail[[i + k]])
        ok[i] <- TRUE
        break
      }
    }
  }
  if (ok[1L]) tail[[1L]] else NULL
}

# The features of a chunk ("ca,s", "g", "cs"): comma-separated; the symbols
# U+22A5 and @ stand alone; with a feature vocabulary a run without commas is
# split by longest match and an unknown feature is an error.
.formula_features <- function(chunk, voc) {
  ch <- strsplit(chunk, "", fixed = TRUE)[[1L]]
  runs <- character(0)
  cur <- character(0)
  flush <- function() {
    if (length(cur)) runs <<- c(runs, paste(cur, collapse = ""))
    cur <<- character(0)
  }
  for (c1 in ch) {
    if (c1 == ",") flush()
    else if (c1 %in% .FORMULA_SYMBOLS) { flush(); runs <- c(runs, c1) }
    else if (c1 %in% letters) cur <- c(cur, c1)
    else .formula_abort("Unknown token '%s'", chunk, formula = voc$formula)
  }
  flush()
  if (is.null(voc$features)) return(runs)
  out <- character(0)
  for (r in runs) {
    if (r %in% .FORMULA_SYMBOLS) { out <- c(out, r); next }
    d <- .formula_decompose(r, voc$features)
    if (is.null(d)) .formula_abort("Unknown feature '%s'", r, formula = voc$formula)
    out <- c(out, d)
  }
  out
}

# The horizon codes of a turbated composition ("SEL+ASN") or of a mixture
# ("AY,EL").
.formula_composition <- function(body, voc) {
  parts <- strsplit(body, "[+,]")[[1L]]
  parts <- parts[nzchar(parts)]
  for (p in parts) {
    ok <- grepl("^[A-Z]+$", p) && (is.null(voc$masters) || p %in% voc$masters)
    if (!ok) .formula_abort("Unknown horizon '%s'", p, formula = voc$formula)
  }
  parts
}

# One horizon as written ("BTg", "CGca(s)", "RY(RJ)", "C(ca)~~", "TUR[SEL+ASN]",
# "2BM", "O|T|H"): its form and the forms of its alternatives, flattened.
.formula_form <- function(x, voc) {
  ch <- strsplit(x, "", fixed = TRUE)[[1L]]
  n <- length(ch)
  p <- 1L
  digits <- as.character(0:9)
  lith <- NA_character_
  if (n >= 2L && ch[1L] %in% as.character(2:9) && ch[2L] %in% LETTERS) {
    lith <- ch[1L]; p <- 2L
  } else {
    for (r in .FORMULA_ROMAN) {
      k <- nchar(r)
      if (n > k && identical(paste(ch[seq_len(k)], collapse = ""), r) && ch[k + 1L] %in% LETTERS) {
        lith <- r; p <- k + 1L
        break
      }
    }
  }
  q <- p
  while (q <= n && ch[q] %in% LETTERS) q <- q + 1L
  if (q == p) .formula_abort("Unknown token '%s'", x, formula = voc$formula)
  run <- paste(ch[p:(q - 1L)], collapse = "")
  if (!is.null(voc$masters)) {
    pre <- voc$masters[startsWith(run, voc$masters)]
    if (!length(pre)) .formula_abort("Unknown horizon '%s'", run, formula = voc$formula)
    master <- pre[which.max(nchar(pre))]
  } else {
    master <- run
    if (nchar(run) == 2L && substr(run, 1L, 1L) %in% voc$flag_masters &&
        substr(run, 2L, 2L) %in% voc$rock_flags) master <- substr(run, 1L, 1L)
  }
  p <- p + nchar(master)
  rockflag <- NA_character_
  if (master %in% voc$flag_masters && p <= n && ch[p] %in% voc$rock_flags) {
    rockflag <- ch[p]; p <- p + 1L
  }
  if (p <= n && ch[p] %in% LETTERS) .formula_abort("Unknown horizon '%s'", run, formula = voc$formula)
  feats <- character(0); typ <- character(0); comp <- character(0)
  mark <- NA_character_; subdiv <- NA_character_
  alts <- list()
  while (p <= n) {
    c1 <- ch[p]
    if (c1 %in% digits) {
      q <- p
      while (q <= n && ch[q] %in% digits) q <- q + 1L
      subdiv <- paste(ch[p:(q - 1L)], collapse = "")
      p <- q
      next
    }
    if (c1 == ",") { p <- p + 1L; next }
    if (c1 == "[") {
      close <- .formula_close(ch, p, voc$formula)
      comp <- c(comp, .formula_composition(paste(ch[seq.int(p + 1L, length.out = close - p - 1L)],
                                                 collapse = ""), voc))
      p <- close + 1L
      next
    }
    if (c1 == "(") {
      close <- .formula_close(ch, p, voc$formula)
      body <- paste(ch[seq.int(p + 1L, length.out = close - p - 1L)], collapse = "")
      if (!nzchar(body)) .formula_abort("Empty parentheses", formula = voc$formula)
      first <- substr(body, 1L, 1L)
      if (first %in% c(LETTERS, digits)) alts <- c(alts, .formula_form(body, voc))
      else typ <- c(typ, .formula_features(body, voc))
      p <- close + 1L
      next
    }
    if (c1 == "|") {
      if (p == n) .formula_abort("Empty alternative", formula = voc$formula)
      alts <- c(alts, .formula_form(paste(ch[(p + 1L):n], collapse = ""), voc))
      p <- n + 1L
      next
    }
    mk <- NA_character_
    for (m in .FORMULA_MARKS) {
      k <- nchar(m)
      if (p + k - 1L <= n && identical(paste(ch[p:(p + k - 1L)], collapse = ""), m)) { mk <- m; break }
    }
    if (!is.na(mk)) { mark <- mk; p <- p + nchar(mk); next }
    if (c1 %in% c(letters, .FORMULA_SYMBOLS)) {
      q <- p + 1L
      if (c1 %in% letters) while (q <= n && ch[q] %in% letters) q <- q + 1L
      feats <- c(feats, .formula_features(paste(ch[p:(q - 1L)], collapse = ""), voc))
      p <- q
      next
    }
    .formula_abort("Unknown token '%s'", paste(ch[p:n], collapse = ""), formula = voc$formula)
  }
  main <- list(code = master, written = x, lith = lith, subdiv = subdiv,
               rockflag = rockflag, mark = mark, features = unique(feats),
               typical = unique(setdiff(typ, feats)), composition = unique(comp))
  # alternatives written after the main form ("CG~~(CQ~~)") keep their own
  # features, flags and marks
  c(list(main), alts)
}

# One item of a formula as a slot.
.formula_slot <- function(item, i, voc) {
  ch <- strsplit(item, "", fixed = TRUE)[[1L]]
  optional <- FALSE
  inner <- item
  if (ch[1L] == "(" && .formula_close(ch, 1L, voc$formula) == length(ch)) {
    optional <- TRUE
    inner <- substr(item, 2L, nchar(item) - 1L)
    if (!nzchar(inner)) .formula_abort("Empty parentheses", formula = voc$formula)
  }
  slot <- list(index = i, written = inner, kind = "horizon", role = NA_character_,
               optional = optional, tested = NA, forms = list(), codes = character(0),
               buried = NA_character_, small = character(0), composition = character(0))
  first <- substr(inner, 1L, 1L)
  if (inner %in% .FORMULA_BURIED) {
    slot$kind <- "buried"
    slot$buried <- names(.FORMULA_BURIED)[match(inner, .FORMULA_BURIED)]
    slot$forms <- list(list(code = inner, written = inner, lith = NA_character_, subdiv = NA_character_,
                            rockflag = NA_character_, mark = NA_character_, features = character(0),
                            typical = character(0), composition = character(0)))
  } else if (first == "[") {
    ich <- strsplit(inner, "", fixed = TRUE)[[1L]]
    close <- .formula_close(ich, 1L, voc$formula)
    body <- substr(inner, 2L, close - 1L)
    tail <- substring(inner, close + 1L)
    if (!nzchar(body)) .formula_abort("Empty brackets", formula = voc$formula)
    if (substr(body, 1L, 1L) %in% letters) {
      # a microprofile of small horizons as one feature: [e-hf], [hh], [akl]
      if (nzchar(tail)) .formula_abort("Unknown token '%s'", inner, formula = voc$formula)
      tok <- paste0("[", body, "]")
      if (!is.null(voc$features) && !(tok %in% voc$features)) {
        for (pc in strsplit(body, "-", fixed = TRUE)[[1L]]) .formula_features(pc, voc)
      }
      slot$kind <- "small"
      slot$small <- tok
    } else {
      # a turbated mixture written as one row: [AY, EL]tr
      slot$kind <- "mixture"
      slot$composition <- .formula_composition(body, voc)
      slot$small <- if (nzchar(tail)) .formula_features(tail, voc) else character(0)
    }
  } else if (first %in% c(letters, .FORMULA_SYMBOLS)) {
    # a small horizon written alone: e, ek, ao
    slot$kind <- "small"
    slot$small <- .formula_features(inner, voc)
  } else {
    slot$forms <- .formula_form(inner, voc)
  }
  slot$codes <- unique(vapply(slot$forms, function(f) f$code, ""))
  slot
}

#' Parse a profile formula
#'
#' Reads a type's profile formula, as the Russian Field Guide of 2008 writes
#' them (chapter 4: "O-EL-BELg-BTg-G-CG", "PU-(AU)-BEL-BT-C",
#' "TUR[AU+SEL+ASN]-BCAs,cs-Cca,s", "RY-[ABC]", "O-C~~"), into slots.
#'
#' Grammar: items split on "-" outside brackets. An item in parentheses is
#' optional ("(EL)", "(AY)"). A horizon is an optional lithological prefix
#' (2-9, II-IX), a master (the longest one of `masters`, else the run of
#' capitals), a rock flag (G or Q after C or D: CG, CQ, DG), sub-horizon
#' digits, a composition in brackets (TUR[SEL+ASN]), features in lower case
#' (comma-separated; the symbols U+22A5 and @ alone), optional features in
#' parentheses ("Gca(s)", "C(ca)"), a rock mark (~~, ", ^^, "", =, ..) and
#' alternatives: a master in parentheses after the horizon ("RY(RJ)",
#' "CG~~(CQ~~)") or soilKey's "|" ("O|T|H"). "[ABC]" and "{ABC}" are buried
#' profiles; "[e-hf]", "[hh]" and a lower-case item alone ("e", "ek") are
#' small horizons, which name features, not slots; "[AY, EL]tr" is a
#' turbated mixture written as one row.
#'
#' Roles: a slot whose master is in `parents`, or a buried profile, is a
#' parent slot, tested only when its code, its signature (code, rock flag and
#' mark: "R", "DG", "C~~") or its text is in `test_terminals` (TRUE tests
#' them all); the first horizon slot is the surface; the others are middle
#' slots. Features written on a tested parent slot are required (Rca, Rcs);
#' on other slots they are typical (expected, not tested), unless listed in
#' `required_features`; features in parentheses are always typical.
#'
#' @param s The formula, one string.
#' @param masters,features Optional vocabularies of master codes and feature
#'   codes; with them an unknown token is an error naming it.
#' @param parents Rock and parent symbols.
#' @param rock_flags,flag_masters The rock flags and the masters that take
#'   them.
#' @param test_terminals Parent slots to test (codes, signatures, texts), or
#'   TRUE.
#' @param optional Codes whose slots are optional although written without
#'   parentheses.
#' @param required_features Features required wherever they are written.
#' @param alternatives A named list of near-identity readings: a written code
#'   and the codes also accepted in its place (\code{list(BCA = "CAT")}).
#' @return A list of class \code{soilkey_formula}: \code{formula}, \code{slots}
#'   (each with \code{index}, \code{written}, \code{kind} (horizon, buried,
#'   small, mixture), \code{role}, \code{optional}, \code{tested},
#'   \code{forms} (code, rock flag, mark, required and typical features,
#'   composition, lithological prefix, sub-horizon digits), \code{codes},
#'   \code{buried}, \code{small}), \code{parents}, \code{codes} (the codes of
#'   the tested slots) and \code{specificity} (tested required slots; then
#'   their required features, flags, marks and components).
#' @noRd
.parse_profile_formula <- function(s, masters = NULL, features = NULL,
                                   parents = .FORMULA_PARENTS,
                                   rock_flags = c("G", "Q"), flag_masters = c("C", "D"),
                                   test_terminals = character(0), optional = character(0),
                                   required_features = character(0), alternatives = list()) {
  if (!is.character(s) || length(s) != 1L || is.na(s) || !nzchar(trimws(s)))
    rlang::abort("A profile formula must be one non-empty string.")
  voc <- list(masters = if (!is.null(masters)) unique(as.character(masters)),
              features = if (!is.null(features)) unique(as.character(features)),
              parents = as.character(parents), rock_flags = as.character(rock_flags),
              flag_masters = as.character(flag_masters), formula = s)
  items <- .formula_items(.formula_normalise(s), s)
  slots <- lapply(seq_along(items), function(i) .formula_slot(items[[i]], i, voc))
  kinds <- vapply(slots, function(z) z$kind, "")
  # the surface slot is the first row of the formula when it is a horizon (a
  # turbated mixture written first, [AY, EL]tr, leaves no surface slot)
  first <- which(kinds %in% c("horizon", "mixture"))[1L]
  if (!is.na(first) && kinds[first] != "horizon") first <- NA_integer_
  test_all <- isTRUE(test_terminals)
  tt <- if (is.character(test_terminals)) test_terminals else character(0)
  for (i in seq_along(slots)) {
    z <- slots[[i]]
    # near-identity readings: other codes accepted in place of a written one
    if (length(alternatives) && z$kind == "horizon") {
      extra <- list()
      for (f in z$forms) for (a in as.character(unlist(alternatives[[f$code]]))) {
        g <- f; g$code <- a; extra <- c(extra, list(g))
      }
      z$forms <- c(z$forms, extra)
      z$codes <- unique(vapply(z$forms, function(f) f$code, ""))
    }
    for (k in seq_along(z$forms)) {
      f <- z$forms[[k]]
      z$forms[[k]]$signature <- paste0(f$code, if (!is.na(f$rockflag)) f$rockflag,
                                       if (!is.na(f$mark)) f$mark)
    }
    sigs <- vapply(z$forms, function(f) f$signature, "")
    z$role <- if (z$kind %in% c("small", "mixture")) "feature"
              else if (z$kind == "buried" || any(z$codes %in% voc$parents)) "parent"
              else if (!is.na(first) && i == first) "surface" else "middle"
    z$tested <- switch(z$role,
      feature = FALSE,
      parent = test_all || any(c(z$codes, sigs, z$written) %in% tt),
      TRUE)
    if (any(z$codes %in% optional)) z$optional <- TRUE
    for (k in seq_along(z$forms)) {
      f <- z$forms[[k]]
      req <- if (z$role == "parent" && z$tested) f$features else intersect(f$features, required_features)
      z$forms[[k]]$required <- req
      z$forms[[k]]$typical <- unique(c(setdiff(f$features, req), f$typical))
    }
    slots[[i]] <- z
  }
  use <- Filter(function(z) z$tested && z$kind %in% c("horizon", "buried"), slots)
  req <- Filter(function(z) !z$optional, use)
  details <- sum(vapply(req, function(z) {
    f <- z$forms[[1L]]
    length(f$required) + !is.na(f$rockflag) + !is.na(f$mark) + length(f$composition)
  }, 0))
  structure(list(
    formula = s,
    slots = slots,
    parents = voc$parents,
    codes = unique(unlist(lapply(use, function(z) z$codes))),
    specificity = c(slots = length(req), details = details)
  ), class = "soilkey_formula")
}

# The slots of a parsed formula as a data frame, one row per slot.
.formula_slots_table <- function(spec) {
  sl <- spec$slots
  data.frame(
    index = vapply(sl, function(z) z$index, 0L),
    written = vapply(sl, function(z) z$written, ""),
    kind = vapply(sl, function(z) z$kind, ""),
    role = vapply(sl, function(z) z$role, ""),
    optional = vapply(sl, function(z) z$optional, NA),
    tested = vapply(sl, function(z) z$tested, NA),
    codes = vapply(sl, function(z) paste(z$codes, collapse = "|"), ""),
    required = vapply(sl, function(z) paste(unique(unlist(lapply(z$forms, function(f) f$required))), collapse = ","), ""),
    typical = vapply(sl, function(z) paste(unique(unlist(lapply(z$forms, function(f) f$typical))), collapse = ","), ""),
    stringsAsFactors = FALSE)
}

# The vocabulary of a list of schemes (a division): every code of a slot that
# is not a rock or parent symbol, and the buried profile when a scheme tests
# one. Closing a match on it makes a horizon of any scheme of the list count
# when present (Hugo's decision Q13, 2026-11-02).
.formula_vocabulary <- function(specs) {
  out <- character(0)
  for (sp in specs) {
    for (z in sp$slots) {
      if (z$kind == "horizon" && z$role != "parent") out <- c(out, z$codes)
      if (z$kind == "buried" && z$tested) out <- c(out, z$codes)
      if (z$kind == "horizon" && z$role == "parent" && z$tested) out <- c(out, z$codes)
    }
  }
  unique(out)
}


# --- the profile side ---------------------------------------------------------

# A table of identified horizons in the form the matcher reads: the frame of
# identify_horizons(), any data frame with a `code` column, or a character
# vector of codes (NA an undecided layer, "" a layer with no diagnostic
# horizon). Missing columns take their defaults; with `keep_extra` the other
# columns are kept. Rows are put in depth order only when every top is known:
# a row whose top is not recorded stays where it was written.
.as_horizon_table <- function(x, keep_extra = FALSE) {
  if (is.character(x) || (is.logical(x) && all(is.na(x)))) {
    v <- as.character(x)
    x <- data.frame(code = ifelse(!is.na(v) & !nzchar(trimws(v)), NA_character_, v),
                    passed = ifelse(is.na(v), NA, nzchar(trimws(v))),
                    stringsAsFactors = FALSE)
  }
  if (!is.data.frame(x))
    rlang::abort("'table' must be a data frame of identified horizons or a character vector of codes.")
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  if (!"code" %in% names(x))
    rlang::abort("The table of identified horizons needs a 'code' column.")
  n <- nrow(x)
  col <- function(k) x[[k]]
  chr <- function(v, d = NA_character_) {
    if (is.null(v)) return(rep(d, n))
    v <- as.character(v)
    v[!is.na(v) & !nzchar(trimws(v))] <- NA_character_
    v
  }
  num <- function(v) if (is.null(v)) rep(NA_real_, n) else suppressWarnings(as.numeric(v))
  lgl <- function(v, d) {
    if (is.null(v)) return(rep(d, n))
    v <- as.logical(v)
    v[is.na(v)] <- d
    v
  }
  lst <- function(v) {
    if (is.null(v)) return(rep(list(character(0)), n))
    if (!is.list(v)) v <- strsplit(ifelse(is.na(v), "", as.character(v)), "[,;]")
    lapply(v, function(e) {
      e <- trimws(as.character(unlist(e)))
      unique(e[!is.na(e) & nzchar(e)])
    })
  }
  code <- chr(col("code"))
  passed <- if (is.null(col("passed"))) ifelse(is.na(code), NA, TRUE) else as.logical(col("passed"))
  bur <- col("buried")
  bur <- if (is.null(bur)) rep(0L, n)
         else if (is.logical(bur)) ifelse(bur %in% TRUE, 1L, 0L)
         else { b <- suppressWarnings(as.integer(bur)); b[is.na(b)] <- 0L; b }
  out <- data.frame(
    horizon_idx = if (is.null(col("horizon_idx"))) seq_len(n) else as.integer(col("horizon_idx")),
    top_cm = num(col("top_cm")), bottom_cm = num(col("bottom_cm")),
    designation = chr(col("designation")), code = code, passed = passed,
    small = lgl(col("small"), FALSE), transitional = lgl(col("transitional"), FALSE),
    buried = bur, rockflag = chr(col("rockflag")), mark = chr(col("mark")),
    route = chr(col("route")), evidence = chr(col("evidence")),
    stringsAsFactors = FALSE)
  for (k in c("overlay", "features", "features_na", "composition", "missing", "candidates")) {
    out[[k]] <- if (n) lst(col(k)) else list()
  }
  # NA in a candidate list ("or no horizon") is kept as ""
  if (!is.null(col("candidates")) && is.list(col("candidates"))) {
    out$candidates <- lapply(col("candidates"), function(e) {
      e <- as.character(unlist(e))
      unique(ifelse(is.na(e), "", trimws(e)))
    })
  }
  if (keep_extra) for (k in setdiff(names(x), names(out))) out[[k]] <- x[[k]]
  if (n > 1L && !anyNA(out$top_cm) && is.unsorted(out$top_cm))
    out <- out[order(out$top_cm), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Three-valued AND.
.mhs_and3 <- function(a, b) {
  if (isFALSE(a) || isFALSE(b)) FALSE else if (is.na(a) || is.na(b)) NA else TRUE
}

# The context of one match: the slots matched, the codes in play, the surface
# codes and the allowances.
#
# The codes in play are the vocabulary's and the formula's tested slots'
# (rock and parent symbols included: a parent the division tests counts in
# every scheme of it). With closure_surface FALSE (the default), surface and
# agro codes the formula does not name are left out of the closure, so a
# natural remnant under the plough does not block an agro type. An untested
# slot whose code is in play is matched as an optional slot, by its code
# alone: it takes its horizon when present, and never fails the match.
.mhs_context <- function(tab, spec, vocabulary = NULL, surface = NULL, agro = NULL,
                         closure_surface = FALSE, max_cover_cm = 40, max_gap_cm = 0,
                         pair_gaps_cm = NULL, buried_within_cm = Inf, max_completions = 2000L,
                         agro_remnant = c("all", "thickest")) {
  agro_remnant <- match.arg(agro_remnant)
  agro <- as.character(agro)
  natural <- setdiff(as.character(surface), agro)
  tested <- Filter(function(z) isTRUE(z$tested) && z$kind %in% c("horizon", "buried"), spec$slots)
  tested_codes <- unique(unlist(lapply(tested, function(z) z$codes)))
  voc <- unique(as.character(vocabulary))
  # a formula whose first horizon is no surface horizon (SEL-ASN, BI-C, S-G,
  # TUR[...]) is closed on the surface horizons the other formulas of the
  # list write, as on their other horizons, and leaves out the ones foreign
  # to the list (Hugo, 2026-11-02: AU-SEL-BSN is no SEL-BSN, O-SEL-ASN is
  # SEL-ASN); a formula that writes a surface horizon leaves out the surface
  # horizons it does not write, under the thickest-horizon rule
  surf <- Filter(function(z) identical(z$role, "surface"), spec$slots)
  force_natural <- !length(surf) || any(surf[[1L]]$codes %in% c(agro, natural))
  if (!isTRUE(closure_surface) && force_natural) voc <- setdiff(voc, setdiff(c(agro, natural), tested_codes))
  play <- unique(c(voc, tested_codes))
  buried_play <- any(.FORMULA_BURIED %in% play)
  slots <- list()
  for (z in spec$slots) {
    if (!(z$kind %in% c("horizon", "buried"))) next
    if (isTRUE(z$tested)) { slots <- c(slots, list(z)); next }
    inplay <- if (z$kind == "buried") buried_play else any(z$codes %in% play)
    if (!inplay) next
    z$optional <- TRUE
    z$untested <- TRUE
    z$forms <- lapply(z$forms, function(f) {
      f$required <- character(0); f$rockflag <- NA_character_
      f$mark <- NA_character_; f$composition <- character(0)
      f
    })
    slots <- c(slots, list(z))
  }
  top0 <- suppressWarnings(min(tab$top_cm, na.rm = TRUE))
  # such a formula does not force the surface horizon of the profile into
  # the match (an agro horizon still counts): one foreign to the list is
  # left out (v0.9.339)
  list(slots = slots, spec = spec, force_natural = force_natural,
       spec_codes = unique(unlist(lapply(slots, function(z) z$codes))),
       play = play, agro = agro, natural = natural, buried_play = buried_play,
       max_cover_cm = max_cover_cm, max_gap_cm = max_gap_cm, pair_gaps_cm = pair_gaps_cm,
       buried_within_cm = buried_within_cm, max_completions = max_completions,
       agro_remnant = agro_remnant, top0 = if (is.finite(top0)) top0 else NA_real_)
}

.mhs_is_agro <- function(code, ctx) code %in% ctx$agro
.mhs_is_natural <- function(code, ctx) code %in% ctx$natural
.mhs_is_surface <- function(code, ctx) code %in% ctx$agro | code %in% ctx$natural

# The options of an undecided row: each candidate kept as itself when a slot
# names it, a decided row of the profile has it, or it is a surface or agro
# code in a row that could belong to the surface run (it may merge with a
# neighbour or change the thickest surface); otherwise a stand-in: "<p>" in
# play, "<f>" foreign; "" or NA "<none>".
.mhs_options <- function(cand, keep, ctx, surface_kept = TRUE) {
  unique(vapply(cand, function(c1) {
    if (is.na(c1) || !nzchar(c1)) return("<none>")
    if (c1 %in% keep || (surface_kept && .mhs_is_surface(c1, ctx))) return(c1)
    if (c1 %in% ctx$play) "<p>" else "<f>"
  }, "", USE.NAMES = FALSE))
}

# The rows of a completed profile collapsed top-down into tokens: consecutive
# rows with the same code merged (BT1, BT2 -> BT), the rows of one buried
# profile merged into one token.
.mhs_tokens <- function(rows, codes) {
  n <- nrow(rows)
  if (!n) return(NULL)
  key <- codes
  b <- rows$buried > 0L
  key[b] <- paste0("<buried", rows$buried[b], ">")
  start <- c(TRUE, key[-1L] != key[-n])
  grp <- split(seq_len(n), cumsum(start))
  code <- key[start]
  bur <- b[start]
  code[bur] <- "<buried>"
  first_chr <- function(v) { v <- v[!is.na(v)]; if (length(v)) v[1L] else NA_character_ }
  top <- vapply(grp, function(r) suppressWarnings(min(rows$top_cm[r])), 0)
  bottom <- vapply(grp, function(r) suppressWarnings(max(rows$bottom_cm[r])), 0)
  top[!is.finite(top)] <- NA_real_
  bottom[!is.finite(bottom)] <- NA_real_
  idx <- vapply(grp, function(r) {
    h <- rows$horizon_idx[r]
    if (length(h) == 1L) as.character(h) else paste0(h[1L], "-", h[length(h)])
  }, "")
  ul <- function(k) unname(lapply(grp, function(r) unique(unlist(rows[[k]][r]))))
  list(n = length(grp), code = unname(code), buried = unname(bur), idx = unname(idx),
       top = unname(top), bottom = unname(bottom),
       thick = unname(vapply(grp, function(r) sum(rows$bottom_cm[r] - rows$top_cm[r]), 0)),
       features = unname(lapply(grp, function(r) unique(c(unlist(rows$features[r]), unlist(rows$overlay[r]))))),
       features_na = ul("features_na"), overlay = ul("overlay"), composition = ul("composition"),
       rockflag = unname(vapply(grp, function(r) first_chr(rows$rockflag[r]), "")),
       mark = unname(vapply(grp, function(r) first_chr(rows$mark[r]), "")))
}

.mhs_label <- function(code, buried, idx) {
  lab <- ifelse(code %in% .MHS_REPS, "?", ifelse(buried, .FORMULA_BURIED[["square"]], code))
  paste0(lab, " (", idx, ")")
}

# Which tokens are in play: by code or by an overlay; a buried profile when the
# match counts one and it starts within buried_within_cm.
.mhs_play_mask <- function(tok, ctx) {
  ov <- vapply(tok$overlay, function(o) any(o %in% ctx$play), NA)
  p <- (tok$code %in% ctx$play | tok$code == "<p>" | ov) & !tok$buried
  if (ctx$buried_play) p <- p | (tok$buried & (is.na(tok$top) | tok$top <= ctx$buried_within_cm))
  p
}

.mhs_drop <- function(tok, drop) {
  keep <- setdiff(seq_len(tok$n), drop)
  out <- lapply(tok[setdiff(names(tok), "n")], function(v) v[keep])
  c(list(n = length(keep)), out)
}

# The surface horizon (Hugo's decision Q11, 2026-11-02): with `agro` and
# `surface` codes given, the run of surface horizons at the top of the profile
# gives the surface: the first agro horizon of the run, else the thickest
# natural one; the other horizons of the run above it, or all of them for a
# natural surface, are read as features and leave the sequence (v0.9.338: with
# agro_remnant "thickest", of the natural horizons of the run under the agro
# one only the thickest stays, as the remnant, and the others are features
# too). Above the run
# lies the cover: rows with no diagnostic horizon and horizons neither in play
# nor surface horizons (litter coded L); a cover of max_cover_cm or more means
# no surface run. Thicknesses that are not known, or equal, leave several
# choices, which the matcher follows. Each choice is the tokens left and the
# index of the surface token, which is always in play.
.mhs_surface <- function(tok, ctx) {
  none <- list(list(tok = tok, forced = NA_integer_, surface = NA_character_,
                    absorbed = character(0)))
  if (!length(ctx$agro) && !length(ctx$natural)) return(none)
  if (is.null(tok) || !tok$n) return(none)
  play <- .mhs_play_mask(tok, ctx)
  is_surf <- function(k) !tok$buried[k] && .mhs_is_surface(tok$code[k], ctx)
  i <- 1L
  while (i <= tok$n && !is_surf(i) && !tok$buried[i] && (tok$code[i] == "<none>" || !play[i])) i <- i + 1L
  if (i > tok$n || !is_surf(i)) return(none)
  cover <- tok$top[i] - ctx$top0
  if (!is.na(cover) && cover >= ctx$max_cover_cm) return(none)
  j <- i
  while (j < tok$n && is_surf(j + 1L)) j <- j + 1L
  run <- i:j
  choice <- function(k, absorbed) {
    list(tok = .mhs_drop(tok, absorbed), forced = k - sum(absorbed < k), surface = tok$code[k],
         absorbed = .mhs_label(tok$code[absorbed], tok$buried[absorbed], tok$idx[absorbed]))
  }
  # the thickest of the natural horizons `nat`: several when thicknesses are
  # not known or equal
  thickest <- function(nat) {
    th <- tok$thick[nat]
    mx <- suppressWarnings(max(th, na.rm = TRUE))
    nat[is.na(th) | (is.finite(mx) & th == mx)]
  }
  ag <- run[.mhs_is_agro(tok$code[run], ctx)]
  if (length(ag)) {
    above <- run[run < ag[1L]]
    below <- run[run > ag[1L] & !.mhs_is_agro(tok$code[run], ctx)]
    if (!identical(ctx$agro_remnant, "thickest") || length(below) < 2L)
      return(list(choice(ag[1L], above)))
    return(lapply(thickest(below), function(k) choice(ag[1L], c(above, setdiff(below, k)))))
  }
  if (length(run) == 1L) return(list(choice(i, integer(0))))
  lapply(thickest(run), function(k) choice(k, setdiff(run, k)))
}

# The horizons in play, consecutive ones with the same code merged when only
# horizons left out of the match lie between them (BT, V, BT with V foreign:
# one BT); a row with no diagnostic horizon between them keeps them apart.
.mhs_play_tokens <- function(tok, play) {
  pi <- which(play)
  if (!length(pi)) return(list(n = 0L, first = integer(0), last = integer(0)))
  grp <- list(pi[1L])
  for (k in pi[-1L]) {
    g <- grp[[length(grp)]]
    a <- g[length(g)]
    mid <- if (k - a > 1L) (a + 1L):(k - 1L) else integer(0)
    same <- !tok$buried[k] && !tok$buried[a] && tok$code[k] == tok$code[a] &&
      !(tok$code[k] %in% .MHS_REPS) && !any(tok$code[mid] == "<none>")
    if (same) grp[[length(grp)]] <- c(g, k) else grp <- c(grp, list(k))
  }
  first_chr <- function(v) { v <- v[!is.na(v)]; if (length(v)) v[1L] else NA_character_ }
  ul <- function(k) lapply(grp, function(g) unique(unlist(tok[[k]][g])))
  list(n = length(grp),
       first = vapply(grp, function(g) g[1L], 0L), last = vapply(grp, function(g) g[length(g)], 0L),
       code = vapply(grp, function(g) tok$code[g[1L]], ""),
       buried = vapply(grp, function(g) tok$buried[g[1L]], NA),
       label = vapply(grp, function(g) .mhs_label(tok$code[g[1L]], tok$buried[g[1L]],
                                                  paste(tok$idx[g], collapse = ", ")), ""),
       top = vapply(grp, function(g) tok$top[g[1L]], 0),
       bottom = vapply(grp, function(g) tok$bottom[g[length(g)]], 0),
       thick = vapply(grp, function(g) sum(tok$thick[g]), 0),
       features = ul("features"), features_na = ul("features_na"),
       overlay = ul("overlay"), composition = ul("composition"),
       rockflag = vapply(grp, function(g) first_chr(tok$rockflag[g]), ""),
       mark = vapply(grp, function(g) first_chr(tok$mark[g]), ""))
}

# Three-valued compatibility of a horizon in play with a slot (any of its
# forms). A horizon fills a slot by its code or by an overlay. A rock flag or
# mark the slot requires that the layer does not record is FALSE, NA when it is
# listed among the layer's undecided features (features_na: "G", "~~" ...).
.mhs_compat <- function(pt, i, slot, ctx) {
  best <- FALSE
  for (f in slot$forms) {
    if (pt$buried[i]) {
      if (!(f$code %in% .FORMULA_BURIED)) next
      v <- if (is.na(pt$top[i])) NA else pt$top[i] <= ctx$buried_within_cm
    } else {
      if (!identical(pt$code[i], f$code) && !(f$code %in% pt$overlay[[i]])) next
      v <- TRUE
      if (length(f$required)) {
        miss <- setdiff(f$required, pt$features[[i]])
        if (length(miss)) v <- if (all(miss %in% pt$features_na[[i]])) NA else FALSE
      }
      na_set <- pt$features_na[[i]]
      if (!is.na(f$rockflag) && !identical(pt$rockflag[i], f$rockflag))
        v <- .mhs_and3(v, if (is.na(pt$rockflag[i]) && f$rockflag %in% na_set) NA else FALSE)
      if (!is.na(f$mark) && !identical(pt$mark[i], f$mark))
        v <- .mhs_and3(v, if (is.na(pt$mark[i]) && f$mark %in% na_set) NA else FALSE)
      if (length(f$composition) && !isFALSE(v)) {
        tc <- pt$composition[[i]]
        v <- if (!length(tc)) .mhs_and3(v, NA) else .mhs_and3(v, all(f$composition %in% tc))
      }
    }
    if (isTRUE(v)) return(TRUE)
    if (is.na(v)) best <- NA
  }
  best
}

# Edit distance between the horizons in play and the slots (an optional slot
# is skipped at no cost, a required one at cost 1, an extra horizon costs 1,
# a mismatched pair 1), with the operations of one cheapest alignment.
.mhs_dp <- function(C, opt, trace = TRUE) {
  n <- nrow(C); m <- ncol(C)
  D <- matrix(0, n + 1L, m + 1L)
  for (j in seq_len(m)) D[1L, j + 1L] <- D[1L, j] + !opt[j]
  for (i in seq_len(n)) D[i + 1L, 1L] <- i
  for (i in seq_len(n)) for (j in seq_len(m)) {
    D[i + 1L, j + 1L] <- min(D[i + 1L, j] + !opt[j], D[i, j + 1L] + 1, D[i, j] + !C[i, j])
  }
  if (!trace) return(list(cost = D[n + 1L, m + 1L], ops = list()))
  ops <- list()
  i <- n; j <- m
  while (i > 0L || j > 0L) {
    if (i > 0L && j > 0L && D[i + 1L, j + 1L] == D[i, j] + !C[i, j]) {
      ops <- c(list(c(1L, i, j)), ops); i <- i - 1L; j <- j - 1L
    } else if (j > 0L && D[i + 1L, j + 1L] == D[i + 1L, j] + !opt[j]) {
      ops <- c(list(c(2L, 0L, j)), ops); j <- j - 1L
    } else {
      ops <- c(list(c(3L, i, 0L)), ops); i <- i - 1L
    }
  }
  list(cost = D[n + 1L, m + 1L], ops = ops)
}

# The code of a horizon in the keys of pair_gaps_cm: a buried profile is
# "[ABC]" ("RY>[ABC]").
.mhs_key_code <- function(code, buried) if (buried) .FORMULA_BURIED[["square"]] else code

# The allowance between two horizons in play.
.mhs_allow <- function(a, b, ctx) {
  key <- paste0(a, ">", b)
  if (!is.null(ctx$pair_gaps_cm) && key %in% names(ctx$pair_gaps_cm)) ctx$pair_gaps_cm[[key]]
  else ctx$max_gap_cm
}

# Match one choice of tokens (a completed profile with its surface chosen):
# the verdict and the distance, and with `evidence` the alignment read out.
.mhs_align <- function(ch, ctx, evidence = TRUE) {
  tok <- ch$tok
  slots <- ctx$slots
  m <- length(slots)
  n0 <- if (is.null(tok)) 0L else tok$n
  play <- if (n0) .mhs_play_mask(tok, ctx) else logical(0)
  if (n0 && !is.na(ch$forced) && (!isFALSE(ctx$force_natural) || .mhs_is_agro(tok$code[ch$forced], ctx)))
    play[ch$forced] <- TRUE
  pt <- if (n0) .mhs_play_tokens(tok, play) else list(n = 0L)
  n <- pt$n
  notes <- character(0)
  # "directly under": the rows with no diagnostic horizon between two horizons
  # in play may sum to max_gap_cm (or the allowance of the pair); transitional
  # rows and sub-horizons are dropped before, foreign horizons ignored
  gaps <- logical(0)
  if (n > 1L) {
    gaps <- vapply(seq_len(n - 1L), function(k) {
      a <- pt$last[k]; b <- pt$first[k + 1L]
      mid <- if (b - a > 1L) (a + 1L):(b - 1L) else integer(0)
      mid <- mid[tok$code[mid] == "<none>"]
      if (!length(mid)) return(TRUE)
      allow <- .mhs_allow(.mhs_key_code(pt$code[k], pt$buried[k]),
                          .mhs_key_code(pt$code[k + 1L], pt$buried[k + 1L]), ctx)
      th <- sum(tok$thick[mid])
      v <- if (allow <= 0) FALSE else if (is.na(th)) NA else th <= allow
      if (evidence && !isTRUE(v)) notes <<- c(notes, sprintf(
        "%s lies under %s across %s cm with no diagnostic horizon (allowed %s)",
        pt$label[k + 1L], pt$label[k],
        if (is.na(th)) "an unknown thickness" else format(th), format(allow)))
      v
    }, NA)
  }
  C3 <- matrix(FALSE, n, m)
  for (a in seq_len(n)) for (j in seq_len(m)) C3[a, j] <- .mhs_compat(pt, a, slots[[j]], ctx)
  opt <- vapply(slots, function(z) z$optional, NA)
  Copt <- C3; Copt[is.na(Copt)] <- TRUE
  Cpes <- C3; Cpes[is.na(Cpes)] <- FALSE
  dopt <- .mhs_dp(Copt, opt, trace = evidence)
  dpes <- if (anyNA(C3)) .mhs_dp(Cpes, opt, trace = FALSE) else dopt
  gap_bad <- sum(gaps %in% FALSE)
  value <- if (dpes$cost == 0 && all(gaps %in% TRUE)) TRUE
           else if (dopt$cost > 0 || gap_bad > 0) FALSE else NA
  if (!evidence) return(list(value = value, distance = dopt$cost + gap_bad))
  matched <- list(); missing <- character(0); extra <- character(0); absent <- character(0)
  for (op in dopt$ops) {
    if (op[1L] == 1L) {
      a <- op[2L]; z <- slots[[op[3L]]]
      if (Copt[a, op[3L]]) {
        matched[[length(matched) + 1L]] <- data.frame(
          slot = z$index, written = z$written, horizon = pt$label[a], code = pt$code[a],
          top_cm = pt$top[a], bottom_cm = pt$bottom[a],
          decided = !is.na(C3[a, op[3L]]), stringsAsFactors = FALSE)
        typ <- setdiff(unique(unlist(lapply(z$forms, function(f) f$typical))), pt$features[[a]])
        if (length(typ) && !pt$buried[a] && !isTRUE(z$untested))
          notes <- c(notes, sprintf("typical feature(s) %s of %s not recorded on %s",
                                    paste(typ, collapse = ", "), z$written, pt$label[a]))
      } else {
        missing <- c(missing, z$written)
        extra <- c(extra, pt$label[a])
        if (pt$code[a] %in% z$codes)
          notes <- c(notes, sprintf("%s does not have what %s requires", pt$label[a], z$written))
      }
    } else if (op[1L] == 2L) {
      z <- slots[[op[3L]]]
      if (z$optional) { if (!isTRUE(z$untested)) absent <- c(absent, z$written) }
      else missing <- c(missing, z$written)
    } else {
      extra <- c(extra, pt$label[op[2L]])
    }
  }
  ignored <- if (n0) .mhs_label(tok$code, tok$buried, tok$idx)[!play & tok$code != "<none>"] else character(0)
  matched <- if (length(matched)) do.call(rbind, matched)
             else data.frame(slot = integer(0), written = character(0), horizon = character(0),
                             code = character(0), top_cm = numeric(0), bottom_cm = numeric(0),
                             decided = logical(0), stringsAsFactors = FALSE)
  list(value = value, distance = dopt$cost + gap_bad, matched = matched,
       missing = missing, extra = extra, absent = absent, ignored = ignored,
       surface = ch$surface, absorbed = ch$absorbed,
       sequence = if (n) pt$code else character(0), notes = unique(notes))
}

# One completion of the undecided rows: its verdict over the surface choices
# (TRUE when every choice matches, FALSE when none does, else NA), the least
# distance, and whether the choices of the surface gave different verdicts.
.mhs_try <- function(rows, codes, ctx) {
  chs <- .mhs_surface(.mhs_tokens(rows, codes), ctx)
  rs <- lapply(chs, .mhs_align, ctx = ctx, evidence = FALSE)
  v <- vapply(rs, function(r) r$value, NA)
  list(value = if (anyNA(v)) NA else if (all(v)) TRUE else if (!any(v)) FALSE else NA,
       distance = min(vapply(rs, function(r) r$distance, 0)),
       tie = length(chs) > 1L && length(unique(v)) > 1L)
}

# The evidence of one completion: the alignment of the choice of surface whose
# verdict is `value` (else the closest one).
.mhs_evidence <- function(rows, codes, ctx, value) {
  chs <- .mhs_surface(.mhs_tokens(rows, codes), ctx)
  rs <- lapply(chs, .mhs_align, ctx = ctx, evidence = TRUE)
  v <- vapply(rs, function(r) r$value, NA)
  k <- which(vapply(v, identical, NA, value))
  rs[[if (length(k)) k[1L] else which.min(vapply(rs, function(r) r$distance, 0))]]
}

# A necessary condition for a match, before the completions of undecided
# layers are tried: the match relaxed so that every completion's alignment is
# one of its paths. An undecided layer may fill any slot one of its options
# names, or vanish when an option is foreign, no horizon, or a surface code in
# the top run. A decided horizon in play must fill a slot it is compatible
# with (optimistically: what is undecided counts as present, over the layers
# it could merge with), or may vanish as a feature of the surface (a surface
# code in the top run), or fill the same slot as the horizon before it (a
# merge). Two decided horizons in play with only decided rows between them,
# whose gap breaks its allowance, cost 1. Returns the cheapest relaxed edit
# distance (0 when a match is possible, else a lower bound of the distance of
# every completion) and a completion along the cheapest path: the witness.
.mhs_relaxed <- function(rows, base, und, opts, ctx) {
  slots <- ctx$slots
  m <- length(slots)
  req <- vapply(slots, function(z) !z$optional, NA)
  scodes <- lapply(slots, function(z) z$codes)
  surf_rule <- length(ctx$agro) || length(ctx$natural)
  vanish_reps <- c("<f>", "<none>")
  n <- nrow(rows)
  is_und <- seq_len(n) %in% und
  is_bur <- rows$buried > 0L
  inplay_dec <- vapply(seq_len(n), function(r) {
    !is_und[r] && !is_bur[r] && base[r] != "<none>" &&
      (base[r] %in% ctx$play || any(unlist(rows$overlay[r]) %in% ctx$play))
  }, NA)
  # the rows a decided horizon could merge with: the same code, undecided rows
  # and horizons left out of the match, up to a row with no horizon
  merge_group <- function(r) {
    c1 <- base[r]
    ok <- function(q) !is_bur[q] && (is_und[q] || base[q] == c1 ||
                                      (base[q] != "<none>" && !inplay_dec[q]))
    lo <- r; while (lo > 1L && ok(lo - 1L)) lo <- lo - 1L
    hi <- r; while (hi < n && ok(hi + 1L)) hi <- hi + 1L
    lo:hi
  }
  fits_decided <- function(r) {
    g <- merge_group(r)
    feats <- unique(c(unlist(rows$features[g]), unlist(rows$overlay[g]), unlist(rows$features_na[g])))
    flags <- unique(c(rows$rockflag[g], unlist(rows$features_na[g])))
    marks <- unique(c(rows$mark[g], unlist(rows$features_na[g])))
    comp <- unique(unlist(rows$composition[g]))
    codes <- c(base[r], unlist(rows$overlay[r]))
    vapply(slots, function(z) any(vapply(z$forms, function(f) {
      f$code %in% codes && all(f$required %in% feats) &&
        (is.na(f$rockflag) || f$rockflag %in% flags) && (is.na(f$mark) || f$mark %in% marks) &&
        (!length(f$composition) || !length(comp) || all(f$composition %in% comp))
    }, NA)), NA)
  }
  el <- list()
  top_run <- TRUE
  r <- 1L
  while (r <= n) {
    if (is_bur[r]) {
      b <- rows$buried[r]; r2 <- r
      while (r2 < n && rows$buried[r2 + 1L] == b) r2 <- r2 + 1L
      top <- suppressWarnings(min(rows$top_cm[r:r2]))
      if (ctx$buried_play && (!is.finite(top) || top <= ctx$buried_within_cm)) {
        el[[length(el) + 1L]] <- list(kind = "decided", row = r, vanish = FALSE,
          fits = vapply(slots, function(z) z$kind == "buried" &&
                          (!is.finite(top) || top <= ctx$buried_within_cm), NA))
      }
      top_run <- FALSE
      r <- r2 + 1L
      next
    }
    if (is_und[r]) {
      o <- opts[[match(r, und)]]
      ov <- unlist(rows$overlay[r])
      surf <- surf_rule && top_run && any(.mhs_is_surface(o, ctx))
      # an option leaves the row out of the match: no horizon, a stand-in for
      # a foreign code, or a real code not in play (unless an overlay is)
      free_opt <- o %in% vanish_reps | (!(o %in% .MHS_REPS) & !(o %in% ctx$play))
      el[[length(el) + 1L]] <- list(kind = "wild", row = r, k = match(r, und), surf = surf,
        free = o[free_opt & o != "<none>"],
        fits = vapply(scodes, function(cc) any(o %in% cc) || any(ov %in% cc), NA),
        vanish = (any(free_opt) && !any(ov %in% ctx$play)) || surf)
    } else if (base[r] != "<none>") {
      is_s <- .mhs_is_surface(base[r], ctx)
      if (inplay_dec[r]) {
        el[[length(el) + 1L]] <- list(kind = "decided", row = r,
                                      fits = fits_decided(r), vanish = surf_rule && top_run && is_s)
        if (!is_s) top_run <- FALSE
      }
    }
    r <- r + 1L
  }
  # decided gaps: two decided horizons in play that cannot vanish, with only
  # decided rows between them, among them a row with no horizon
  gap_cost <- 0
  dec <- Filter(function(e) e$kind == "decided" && !e$vanish && !is_bur[e$row], el)
  if (length(dec) > 1L) for (k in seq_len(length(dec) - 1L)) {
    a <- dec[[k]]$row; b <- dec[[k + 1L]]$row
    mid <- if (b - a > 1L) (a + 1L):(b - 1L) else integer(0)
    if (!length(mid) || any(is_und[mid]) || any(is_bur[mid])) next
    nn <- mid[base[mid] == "<none>"]
    if (!length(nn)) next
    allow <- .mhs_allow(base[a], base[b], ctx)
    th <- sum(rows$bottom_cm[nn] - rows$top_cm[nn])
    if (allow <= 0 || (!is.na(th) && th > allow)) gap_cost <- gap_cost + 1
  }
  # cost[j + 1, f]: slots 1..j resolved; f = 2 when the last element filled
  # slot j (so the next may fill it again, a merge)
  skip <- function(a, b) if (b > a + 1L) sum(req[(a + 1L):(b - 1L)]) else 0
  cost <- matrix(Inf, m + 1L, 2L)
  cost[1L, 1L] <- 0
  bp <- vector("list", length(el))
  for (e in seq_along(el)) {
    E <- el[[e]]
    nw <- matrix(Inf, m + 1L, 2L)
    pj <- matrix(NA_integer_, m + 1L, 2L); pf <- pj; act <- pj
    upd <- function(j2, f2, c, j, f, a) {
      if (c < nw[j2 + 1L, f2]) {
        nw[j2 + 1L, f2] <<- c; pj[j2 + 1L, f2] <<- j; pf[j2 + 1L, f2] <<- f; act[j2 + 1L, f2] <<- a
      }
    }
    for (j in 0:m) for (f in 1:2) {
      c0 <- cost[j + 1L, f]
      if (!is.finite(c0)) next
      if (E$vanish) upd(j, f, c0, j, f, 1L)
      if (f == 2L && j > 0L && E$fits[j]) upd(j, 2L, c0, j, f, 3L)
      if (j < m) for (j2 in (j + 1L):m) upd(j2, 2L, c0 + skip(j, j2) + !E$fits[j2], j, f, 4L)
      upd(j, f, c0 + 1, j, f, 2L)
    }
    bp[[e]] <- list(pj = pj, pf = pf, act = act)
    cost <- nw
  }
  fin <- vapply(0:m, function(j) min(cost[j + 1L, ]) + skip(j, m + 1L), 0)
  jb <- which.min(fin) - 1L
  total <- fin[jb + 1L] + gap_cost
  # the witness: the options along the cheapest path
  wit <- vapply(opts, function(o) o[1L], "")
  if (length(el)) {
    f <- which.min(cost[jb + 1L, ])
    j <- jb
    acts <- vector("list", length(el))
    for (e in rev(seq_along(el))) {
      a <- bp[[e]]$act[j + 1L, f]
      acts[[e]] <- c(a, j)
      j2 <- bp[[e]]$pj[j + 1L, f]; f2 <- bp[[e]]$pf[j + 1L, f]
      j <- j2; f <- f2
    }
    last_code <- rep(NA_character_, m)
    for (e in seq_along(el)) {
      E <- el[[e]]; a <- acts[[e]][1L]; j <- acts[[e]][2L]
      if (E$kind == "decided") {
        if (a %in% c(3L, 4L) && j > 0L) last_code[j] <- base[E$row]
        next
      }
      o <- opts[[E$k]]
      pick <- if (a == 1L) {
        c(E$free, if (E$surf) o[.mhs_is_surface(o, ctx)], intersect("<none>", o))[1L]
      } else if (a %in% c(3L, 4L) && j > 0L) {
        cand <- o[o %in% scodes[[j]]]
        if (!is.na(last_code[j]) && last_code[j] %in% cand) last_code[j] else cand[1L]
      } else NA_character_
      if (!is.na(pick)) {
        wit[E$k] <- pick
        if (a %in% c(3L, 4L) && j > 0L) last_code[j] <- pick
      }
    }
  }
  list(cost = total, witness = wit)
}

# The match of a normalised table against a parsed formula in a context.
.mhs_match <- function(tab, ctx) {
  spec <- ctx$spec
  rows <- tab[!(tab$small | tab$transitional), , drop = FALSE]
  und <- which(is.na(rows$code) & !(rows$passed %in% FALSE) & rows$buried == 0L)
  base <- ifelse(is.na(rows$code), "<none>", rows$code)
  keep <- unique(c(ctx$spec_codes, setdiff(base[setdiff(seq_len(nrow(rows)), und)], "<none>")))
  universe <- setdiff(unique(c(ctx$play, ctx$spec_codes, ctx$agro, ctx$natural)), .FORMULA_BURIED)
  free <- vapply(und, function(r) !length(rows$candidates[[r]]), NA)
  # a row under a decided horizon in play that is no surface horizon (or under
  # a buried profile) cannot be in the surface run
  stop_run <- (!(seq_len(nrow(rows)) %in% und) & base != "<none>" &
                 (base %in% ctx$play | vapply(rows$overlay, function(o) any(o %in% ctx$play), NA)) &
                 !.mhs_is_surface(base, ctx)) | rows$buried > 0L
  top_region <- cumsum(c(0, utils::head(stop_run, -1L))) == 0
  opts <- lapply(und, function(r) {
    cand <- rows$candidates[[r]]
    if (length(cand)) .mhs_options(cand, keep, ctx, top_region[r])
    else unique(c(.mhs_options(universe, keep, ctx, top_region[r]), "<f>", "<none>"))
  })
  notes <- character(0)
  result <- function(value, distance, exact, ev, undecided = character(0), more = character(0)) {
    list(passed = value, formula = spec$formula, specificity = spec$specificity,
         distance = distance, distance_exact = exact,
         matched = ev$matched, missing = ev$missing, extra = ev$extra,
         absent = ev$absent, ignored = ev$ignored,
         surface = ev$surface, absorbed = ev$absorbed, sequence = ev$sequence,
         undecided = undecided, notes = unique(c(ev$notes, more)))
  }
  undecided_list <- function() vapply(und, function(r) {
    cand <- rows$candidates[[r]]
    sprintf("%s (%s)", rows$horizon_idx[r],
            if (length(cand)) paste(ifelse(nzchar(cand), cand, "no horizon"), collapse = ", ") else "any")
  }, "")
  tie_note <- "the thickest surface horizon is not decided by the recorded depths"
  if (!length(und)) {
    chs <- .mhs_surface(.mhs_tokens(rows, base), ctx)
    rs <- lapply(chs, .mhs_align, ctx = ctx, evidence = TRUE)
    v <- vapply(rs, function(r) r$value, NA)
    value <- if (anyNA(v)) NA else if (all(v)) TRUE else if (!any(v)) FALSE else NA
    k <- which(vapply(v, identical, NA, value))
    ev <- rs[[if (length(k)) k[1L] else 1L]]
    tie <- length(chs) > 1L && length(unique(v)) > 1L
    return(result(value, min(vapply(rs, function(r) r$distance, 0)), TRUE, ev,
                  more = if (tie) tie_note))
  }
  rel <- .mhs_relaxed(rows, base, und, opts, ctx)
  wit <- base
  wit[und] <- rel$witness
  # no completion can match
  if (rel$cost > 0) {
    ev <- .mhs_evidence(rows, wit, ctx, FALSE)
    return(result(FALSE, rel$cost, FALSE, ev, more = sprintf(
      "no completion of the %d undecided layer(s) can match", length(und))))
  }
  # a match is possible: try the witness, then completions meant to fail,
  # then every completion while it can still change the answer
  vals <- logical(0); tried <- list(); ties <- FALSE
  try_codes <- function(codes) {
    key <- paste(codes[und], collapse = "\r")
    if (!is.null(tried[[key]])) return(invisible(NULL))
    t <- .mhs_try(rows, codes, ctx)
    tried[[key]] <<- codes
    vals <<- c(vals, t$value)
    if (t$tie) ties <<- TRUE
    invisible(t$value)
  }
  settled <- function() anyNA(vals) || (any(vals) && !all(vals))
  pick_ev <- function(value) {
    w <- which(vapply(vals, identical, NA, value))
    w <- if (length(w)) w[1L] else 1L
    .mhs_evidence(rows, tried[[w]], ctx, value)
  }
  # more than three layers that could be anything: undecided, not enumerated
  if (sum(free) > 3L) {
    ev <- .mhs_evidence(rows, wit, ctx, TRUE)
    return(result(NA, rel$cost, FALSE, ev, undecided_list(), sprintf(
      "%d undecided layer(s) with no candidates, so the match is left undecided", sum(free))))
  }
  try_codes(wit)
  if (!settled()) {
    # one completion meant to fail (an extra horizon in place of every
    # undecided layer), then the witness changed one layer at a time
    probes <- list()
    bad <- wit
    bad[und] <- vapply(seq_along(und), function(k) {
      o <- opts[[k]]
      alt <- c(intersect("<p>", o), setdiff(o[o %in% ctx$spec_codes], wit[und[k]]),
               setdiff(o, wit[und[k]]))
      if (length(alt)) alt[1L] else wit[und[k]]
    }, "")
    probes[[1L]] <- bad
    for (k in seq_along(und)) for (o in setdiff(opts[[k]], wit[und[k]])) {
      q <- wit; q[und[k]] <- o
      probes[[length(probes) + 1L]] <- q
    }
    for (q in utils::head(probes, 64L)) {
      try_codes(q)
      if (settled()) break
    }
  }
  capped <- FALSE
  if (!settled()) {
    total <- prod(as.numeric(lengths(opts)))
    if (total > ctx$max_completions) {
      capped <- TRUE
    } else {
      ix <- rep(1L, length(opts))
      repeat {
        codes <- base
        codes[und] <- vapply(seq_along(und), function(k) opts[[k]][ix[k]], "")
        try_codes(codes)
        if (settled()) break
        k <- length(ix)
        while (k > 0L) {
          if (ix[k] < length(opts[[k]])) { ix[k] <- ix[k] + 1L; break }
          ix[k] <- 1L; k <- k - 1L
        }
        if (k == 0L) break
      }
    }
  }
  value <- if (capped || settled()) NA else all(vals)
  more <- c(if (ties) tie_note,
            if (capped) sprintf("%d undecided layer(s): more than %d completions, so the match is left undecided",
                                length(und), as.integer(ctx$max_completions)))
  ev <- if (isTRUE(value)) pick_ev(TRUE) else if (isFALSE(value)) pick_ev(FALSE)
        else if (anyNA(vals)) pick_ev(NA) else pick_ev(TRUE)
  result(value, rel$cost, FALSE, ev, if (isTRUE(value) || isFALSE(value)) character(0) else undecided_list(), more)
}

# The parser's arguments, which a formula given as a string takes.
.PARSER_ARGS <- c("masters", "features", "parents", "rock_flags", "flag_masters",
                  "test_terminals", "optional", "required_features", "alternatives")

#' Match a profile against a profile formula
#'
#' Three-valued match of the identified horizons of a profile against the
#' profile formula of a type (the Russian Field Guide of 2008 names its types
#' by such formulas: "O-EL-BEL-BT-C"). Generic: the codes, the vocabulary and
#' the surface horizons are the caller's.
#'
#' The profile side is collapsed top-down: small horizons and transitional
#' rows are dropped; consecutive rows with the same code are merged
#' (sub-horizons: BT1, BT2 = BT), also across horizons left out of the match;
#' the rows of a buried profile become one buried token. A horizon fills a
#' slot by its code or by an overlay (an AU with an S overlay fills an S
#' slot). With `surface` and `agro` codes, the surface slot is read by the
#' thickest-horizon rule: the agro horizon if the surface run has one, else
#' the thickest natural horizon starting at the surface or under a cover
#' thinner than `max_cover_cm` (rows with no diagnostic horizon, and horizons
#' neither in play nor surface horizons, such as litter coded L); the other
#' horizons of that run are read as features and leave the sequence.
#'
#' The match is closed on `vocabulary` (the codes of every scheme of a
#' division): a horizon of the vocabulary, or of a tested slot of the
#' formula, present in the profile must be matched by a slot, in order; a
#' horizon foreign to both is ignored. Surface and agro codes the formula does
#' not name are left out of the closure unless `closure_surface` is TRUE.
#' Optional slots (in parentheses) may be absent. A rock or parent slot not
#' tested (C, D, R unless flagged) takes its horizon when that code is in play
#' and is left out otherwise. Two horizons matched one after the other must
#' lie directly one under the other: rows with no diagnostic horizon between
#' them may sum to `max_gap_cm`, or to the allowance of the pair in
#' `pair_gaps_cm`, named "UPPER>LOWER" by codes (a buried profile is
#' "[ABC]": \code{c("AU>BCA" = 15, "RY>[ABC]" = 10)}).
#'
#' A layer whose code is undecided (NA, with or without a list of
#' candidates) is completed in every way that matters: TRUE when every
#' completion matches, FALSE when none does, NA otherwise. A required
#' feature, rock flag or rock mark listed in \code{features_na}, a turbated
#' composition not recorded, or an unknown thickness, leave the result NA when
#' they decide it. More than three undecided layers with no candidates leave
#' the result NA unless no completion can match.
#'
#' @param table The identified horizons: the frame of
#'   \code{\link{identify_horizons}}, any data frame with a \code{code} column
#'   (and optionally \code{horizon_idx}, \code{top_cm}, \code{bottom_cm},
#'   \code{passed}, \code{candidates}, \code{features}, \code{features_na},
#'   \code{overlay}, \code{small}, \code{transitional}, \code{buried},
#'   \code{rockflag}, \code{mark}, \code{composition}), or a character vector
#'   of codes from the top (NA an undecided layer, "" no diagnostic horizon).
#'   Rows are read in depth order when every top is recorded, else as given.
#' @param spec The formula: a string, or one parsed by the package's parser.
#' @param vocabulary Codes the match is closed on; NULL closes it on the
#'   formula's own codes only.
#' @param surface,agro Natural and agro surface codes for the
#'   thickest-horizon rule; both NULL: no rule, the first horizon is the
#'   surface.
#' @param closure_surface Whether surface and agro codes of the vocabulary
#'   that the formula does not name count in the closure (default FALSE).
#' @param max_cover_cm Thickness of cover above which no surface run is read.
#' @param max_gap_cm,pair_gaps_cm The gap allowed between two consecutive
#'   horizons (cm), and named allowances for pairs.
#' @param buried_within_cm Depth within which a buried profile counts.
#' @param max_completions Most completions of undecided layers tried; beyond
#'   it the result is NA.
#' @param agro_remnant Under an agro surface horizon, \code{"all"} keeps
#'   every natural horizon of the surface run in the sequence (the default);
#'   \code{"thickest"} keeps only the thickest of them, as the remnant, and
#'   reads the others as features (the thickest-horizon rule of the Russian
#'   Field Guide, p. 37).
#' @param ... When `spec` is a string, the arguments of the parser:
#'   \code{test_terminals} (rock and parent slots to test, by code, by
#'   signature such as "C~~" or "DG", or by text such as "[ABC]"; TRUE for
#'   all), \code{optional} (codes whose slots are optional),
#'   \code{required_features} (features required wherever written),
#'   \code{alternatives} (a named list of codes accepted in place of a written
#'   one, \code{list(BCA = "CAT")}), \code{masters} and \code{features}
#'   (vocabularies: an unknown token is then an error), \code{parents},
#'   \code{rock_flags} and \code{flag_masters}. An error when `spec` is
#'   already parsed.
#' @return A list: \code{passed} (TRUE, FALSE or NA), \code{formula},
#'   \code{specificity}, \code{distance} and \code{distance_exact} (the edit
#'   distance of the closest reading, 0 for a match: exact when no layer is
#'   undecided, else a lower bound over the completions), \code{matched} (a
#'   data frame of slots and the horizons that fill them), \code{missing}
#'   (slots not filled), \code{extra} (horizons in play not matched),
#'   \code{absent} (optional slots not present), \code{ignored} (horizons left
#'   out of the match), \code{surface} and \code{absorbed} (the surface
#'   horizon and the ones read as its features), \code{sequence} (the codes in
#'   play), \code{undecided} (layers whose completion leaves it NA) and
#'   \code{notes}.
#' @keywords internal
#' @examples
#' # a profile as its sequence of horizon codes, from the top
#' match_horizon_sequence(c("AY", "EL", "BEL", "BT", "C"), "AY-EL-BEL-BT-C")$passed
#' # an optional remnant (in parentheses) absent; typical g features not needed
#' match_horizon_sequence(c("P", "BEL", "BT", "G", "C"), "P-(ELg)-BELg-BTg-G-CG")$passed
#' # closed on a vocabulary: a G horizon the formula does not name blocks it
#' match_horizon_sequence(c("O", "EL", "BEL", "BT", "G", "C"), "O-EL-BEL-BT-C",
#'                        vocabulary = c("O", "EL", "BEL", "BT", "G"))$passed
#' # an undecided layer that could be EL or another horizon
#' match_horizon_sequence(c("AY", NA, "BT", "C"), "AY-EL-BT-C")$passed
#' # a rock symbol tested where it names the type
#' match_horizon_sequence(c("O", "C", "R"), "O-(C)-R", test_terminals = "R")$passed
#' @export
match_horizon_sequence <- function(table, spec, vocabulary = NULL,
                                   surface = NULL, agro = NULL, closure_surface = FALSE,
                                   max_cover_cm = 40, max_gap_cm = 0, pair_gaps_cm = NULL,
                                   buried_within_cm = Inf, max_completions = 2000L,
                                   agro_remnant = c("all", "thickest"), ...) {
  dots <- list(...)
  if (length(dots) && (is.null(names(dots)) || any(!nzchar(names(dots)))))
    rlang::abort("Arguments of the parser must be named.")
  bad <- setdiff(names(dots), .PARSER_ARGS)
  if (length(bad)) rlang::abort(sprintf("Unknown argument(s): %s.", paste(bad, collapse = ", ")))
  if (is.character(spec)) spec <- do.call(.parse_profile_formula, c(list(spec), dots))
  else if (length(dots))
    rlang::abort(sprintf(
      "The parser's arguments (%s) apply to a formula given as a string; this one is already parsed.",
      paste(names(dots), collapse = ", ")))
  if (!inherits(spec, "soilkey_formula"))
    rlang::abort("'spec' must be a profile formula string or a parsed formula.")
  # a system's own preparation of the table for its formula (v0.9.360: the
  # KA5 runs of rows one slot takes); none for the others
  if (is.function(spec$prepare_table)) table <- spec$prepare_table(table, spec)
  tab <- .as_horizon_table(table)
  ctx <- .mhs_context(tab, spec, vocabulary = vocabulary, surface = surface, agro = agro,
                      closure_surface = closure_surface, max_cover_cm = max_cover_cm,
                      max_gap_cm = max_gap_cm, pair_gaps_cm = pair_gaps_cm,
                      buried_within_cm = buried_within_cm, max_completions = max_completions,
                      agro_remnant = agro_remnant)
  .mhs_match(tab, ctx)
}


# --- several schemes ----------------------------------------------------------

# The scheme a list of match results gives (Hugo's decision Q13, 2026-11-02):
# the most specific of the formulas that match (more required slots, then
# more required features, flags and marks), the list order breaking ties (the
# caller lists agro schemes before natural ones); the other matches are
# intergrades. A scheme left undecided that ranks before the one chosen makes
# it provisional. With no match the result is NA when some scheme is
# undecided (its candidates listed), else FALSE.
.pick_scheme <- function(results) {
  nm <- names(results) %||% vapply(results, function(r) r$formula, "")
  val <- vapply(results, function(r) r$passed, NA)
  sp <- vapply(results, function(r) as.numeric(r$specificity[1:2]), c(0, 0))
  ord <- order(-sp[1L, ], -sp[2L, ], seq_along(results))
  win <- ord[val[ord] %in% TRUE]
  if (length(win)) {
    w <- win[1L]
    before <- ord[seq_len(match(w, ord) - 1L)]
    return(list(code = nm[w], passed = TRUE,
                provisional_because = nm[before[is.na(val[before])]],
                intergrades = nm[win[-1L]], candidates = nm[w]))
  }
  und <- ord[is.na(val[ord])]
  list(code = NA_character_, passed = if (length(und)) NA else FALSE,
       provisional_because = character(0), intergrades = character(0),
       candidates = nm[und])
}

# The nearest schemes of a list of match results by edit distance (Hugo's
# decision Q15, 2026-11-02), then the more specific, then the list order. The
# results of one profile share one kind of distance (exact, or the bound when
# a layer is undecided).
.nearest_schemes <- function(results, n = 3L) {
  nm <- names(results) %||% vapply(results, function(r) r$formula, "")
  d <- vapply(results, function(r) r$distance, 0)
  sp <- vapply(results, function(r) as.numeric(r$specificity[1L]), 0)
  ord <- utils::head(order(d, -sp, seq_along(results)), n)
  data.frame(code = nm[ord], formula = vapply(results[ord], function(r) r$formula, ""),
             distance = d[ord],
             exact = vapply(results[ord], function(r) isTRUE(r$distance_exact), NA),
             missing = vapply(results[ord], function(r) paste(r$missing, collapse = ", "), ""),
             extra = vapply(results[ord], function(r) paste(r$extra, collapse = ", "), ""),
             stringsAsFactors = FALSE, row.names = NULL)
}

# Match a profile against a list of schemes (a division): named formulas,
# strings or parsed, in the order of trial. The parser's arguments go in
# `parse_args` or among `...` (they apply to the strings; an error with a
# parsed scheme); the others go to the matcher. The vocabulary defaults to the
# union of the schemes' codes; it may also be a list of vocabularies named by
# scheme (v0.9.338: a horizon that closes only some schemes), a scheme it does
# not name taking the union. The table is read once. Returns the pick
# (.pick_scheme()), the nearest schemes when none matched, and every result.
.match_schemes <- function(table, specs, vocabulary = NULL, parse_args = list(),
                           n_nearest = 3L, ...) {
  dots <- list(...)
  pa <- dots[names(dots) %in% .PARSER_ARGS]
  ma <- dots[!(names(dots) %in% .PARSER_ARGS)]
  parse_args <- utils::modifyList(as.list(parse_args), pa)
  if (is.character(specs)) specs <- as.list(specs)
  if (length(parse_args) && any(vapply(specs, inherits, NA, "soilkey_formula")))
    rlang::abort(sprintf(
      "The parser's arguments (%s) apply to schemes given as strings; some are already parsed.",
      paste(names(parse_args), collapse = ", ")))
  specs <- lapply(specs, function(s) if (is.character(s)) do.call(.parse_profile_formula, c(list(s), parse_args)) else s)
  if (is.null(names(specs))) names(specs) <- vapply(specs, function(s) s$formula, "")
  union <- .formula_vocabulary(specs)
  voc_of <- if (is.list(vocabulary)) function(k) vocabulary[[k]] %||% union
            else { vocabulary <- vocabulary %||% union; function(k) vocabulary }
  tab <- .as_horizon_table(table)
  res <- lapply(seq_along(specs), function(i) {
    tb <- if (is.function(specs[[i]]$prepare_table)) .as_horizon_table(specs[[i]]$prepare_table(table, specs[[i]])) else tab
    ctx <- do.call(.mhs_context, c(list(tb, specs[[i]], vocabulary = voc_of(names(specs)[i])), ma))
    .mhs_match(tb, ctx)
  })
  names(res) <- names(specs)
  pick <- .pick_scheme(res)
  pick$nearest <- if (!isTRUE(pick$passed)) .nearest_schemes(res, n_nearest) else NULL
  pick$results <- res
  pick
}
