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
# Codes of the matcher that stand for a class of codes: an undecided layer
# completed by "a code of the vocabulary not in the formula", "a code foreign
# to the vocabulary", "no diagnostic horizon"; "p"/"f" in play or free, "a"
# and "n" agro or natural surface codes.
.MHS_REPS <- c("<p>", "<pa>", "<pn>", "<f>", "<fa>", "<fn>", "<none>")


# Typographic variants folded before a formula is read: curly and double
# primes to a straight quote, the Symbol-font perpendicular (U+F05E) to U+22A5,
# dashes and the minus sign to a hyphen; spaces removed.
.formula_normalise <- function(s) {
  s <- enc2utf8(as.character(s))
  for (q in c("\u201c", "\u201d", "\u201e", "\u2033", "\u02ba", "``"))
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
# backtracking); NULL when it cannot be split.
.formula_decompose <- function(x, vocab) {
  if (!nzchar(x)) return(character(0))
  for (k in rev(seq_len(nchar(x)))) {
    pre <- substr(x, 1L, k)
    if (pre %in% vocab) {
      rest <- .formula_decompose(substring(x, k + 1L), vocab)
      if (!is.null(rest)) return(c(pre, rest))
    }
  }
  NULL
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
  first <- which(kinds == "horizon")[1L]
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
              else if (identical(i, first)) "surface" else "middle"
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
# horizon). Missing columns take their defaults; rows are put in depth order.
.as_horizon_table <- function(x) {
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
  code <- chr(x[["code"]])
  passed <- if (is.null(x[["passed"]])) ifelse(is.na(code), NA, TRUE) else as.logical(x[["passed"]])
  bur <- x[["buried"]]
  bur <- if (is.null(bur)) rep(0L, n)
         else if (is.logical(bur)) ifelse(bur %in% TRUE, 1L, 0L)
         else { b <- suppressWarnings(as.integer(bur)); b[is.na(b)] <- 0L; b }
  out <- data.frame(
    horizon_idx = if (is.null(x[["horizon_idx"]])) seq_len(n) else as.integer(x[["horizon_idx"]]),
    top_cm = num(x[["top_cm"]]), bottom_cm = num(x[["bottom_cm"]]),
    designation = chr(x[["designation"]]), code = code, passed = passed,
    small = lgl(x[["small"]], FALSE), transitional = lgl(x[["transitional"]], FALSE),
    buried = bur, rockflag = chr(x[["rockflag"]]), mark = chr(x[["mark"]]),
    route = chr(x[["route"]]), evidence = chr(x[["evidence"]]),
    stringsAsFactors = FALSE)
  for (k in c("overlay", "features", "features_na", "composition", "missing", "candidates")) {
    out[[k]] <- if (n) lst(x[[k]]) else list()
  }
  # NA in a candidate list ("or no horizon") is kept as ""
  if (!is.null(x[["candidates"]]) && is.list(x[["candidates"]])) {
    out$candidates <- lapply(x[["candidates"]], function(e) {
      e <- as.character(unlist(e))
      unique(ifelse(is.na(e), "", trimws(e)))
    })
  }
  if (n && any(!is.na(out$top_cm))) out <- out[order(out$top_cm, na.last = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# The class of a code for a given formula and vocabulary: the code itself when
# a slot names it, else one of .MHS_REPS.
.mhs_category <- function(code, ctx) {
  vapply(code, function(c1) {
    if (is.na(c1) || !nzchar(c1)) return("<none>")
    if (c1 %in% ctx$spec_codes) return(c1)
    play <- c1 %in% ctx$play
    kind <- if (c1 %in% ctx$agro) "a" else if (c1 %in% ctx$natural) "n" else ""
    paste0("<", if (play) "p" else "f", kind, ">")
  }, "", USE.NAMES = FALSE)
}

# Is a token in play (a code of the vocabulary or of a tested slot)?
.mhs_in_play <- function(code, ctx) {
  code %in% ctx$play | code %in% c("<p>", "<pa>", "<pn>")
}
.mhs_is_agro <- function(code, ctx) code %in% ctx$agro | code %in% c("<pa>", "<fa>")
.mhs_is_natural <- function(code, ctx) code %in% ctx$natural | code %in% c("<pn>", "<fn>")

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
  g <- cumsum(start)
  grp <- split(seq_len(n), g)
  code <- key[start]
  bur <- b[start]
  code[bur] <- "<buried>"
  first_chr <- function(v) { v <- v[!is.na(v)]; if (length(v)) v[1L] else NA_character_ }
  top <- vapply(grp, function(r) suppressWarnings(min(rows$top_cm[r])), 0)
  bottom <- vapply(grp, function(r) suppressWarnings(max(rows$bottom_cm[r])), 0)
  thick <- vapply(grp, function(r) sum(rows$bottom_cm[r] - rows$top_cm[r]), 0)
  top[!is.finite(top)] <- NA_real_
  bottom[!is.finite(bottom)] <- NA_real_
  idx <- vapply(grp, function(r) {
    h <- rows$horizon_idx[r]
    if (length(h) == 1L) as.character(h) else paste0(h[1L], "-", h[length(h)])
  }, "")
  lab <- ifelse(code %in% .MHS_REPS, "?", ifelse(bur, .FORMULA_BURIED[["square"]], code))
  list(n = length(grp), code = unname(code), buried = unname(bur),
       label = paste0(lab, " (", unname(idx), ")"),
       top = unname(top), bottom = unname(bottom), thick = unname(thick),
       features = unname(lapply(grp, function(r) unique(c(unlist(rows$features[r]), unlist(rows$overlay[r]))))),
       features_na = unname(lapply(grp, function(r) unique(unlist(rows$features_na[r])))),
       composition = unname(lapply(grp, function(r) unique(unlist(rows$composition[r])))),
       rockflag = unname(vapply(grp, function(r) first_chr(rows$rockflag[r]), "")),
       mark = unname(vapply(grp, function(r) first_chr(rows$mark[r]), "")))
}

.mhs_drop <- function(tok, drop) {
  keep <- setdiff(seq_len(tok$n), drop)
  out <- lapply(tok[setdiff(names(tok), "n")], function(v) v[keep])
  c(list(n = length(keep)), out)
}

# The surface horizon (Hugo's decision Q11, 2026-11-02): with `agro` and
# `surface` codes given, the run of surface horizons at the top of the profile
# (under no cover of `max_cover_cm` or more: leading rows with no diagnostic
# horizon, and the small and transitional rows dropped before) gives the
# surface: the first agro horizon of the run, else the thickest natural one;
# the other horizons of the run above it, or all of them for a natural
# surface, are read as features and leave the sequence. Thicknesses that are
# not known, or equal, leave several choices, which the matcher follows. Each
# choice is the tokens left and the index of the surface token (in play).
.mhs_surface <- function(tok, ctx) {
  none <- list(list(tok = tok, forced = NA_integer_, surface = NA_character_, absorbed = character(0)))
  if (!length(ctx$agro) && !length(ctx$natural)) return(none)
  if (is.null(tok) || !tok$n) return(none)
  i <- which(tok$code != "<none>")[1L]
  if (is.na(i)) return(none)
  is_surf <- function(k) !tok$buried[k] && (.mhs_is_agro(tok$code[k], ctx) || .mhs_is_natural(tok$code[k], ctx))
  if (!is_surf(i)) return(none)
  cover <- tok$top[i] - ctx$top0
  if (!is.na(cover) && cover >= ctx$max_cover_cm) return(none)
  j <- i
  while (j < tok$n && is_surf(j + 1L)) j <- j + 1L
  run <- i:j
  choice <- function(k, absorbed) {
    t2 <- .mhs_drop(tok, absorbed)
    list(tok = t2, forced = k - sum(absorbed < k), surface = tok$code[k],
         absorbed = tok$label[absorbed])
  }
  ag <- run[.mhs_is_agro(tok$code[run], ctx)]
  if (length(ag)) return(list(choice(ag[1L], run[run < ag[1L]])))
  if (length(run) == 1L) return(list(choice(i, integer(0))))
  th <- tok$thick[run]
  mx <- suppressWarnings(max(th, na.rm = TRUE))
  can <- run[is.na(th) | (is.finite(mx) & th == mx)]
  lapply(can, function(k) choice(k, setdiff(run, k)))
}

# Three-valued compatibility of a token with a slot (any of its forms).
.mhs_compat <- function(tok, i, slot, ctx) {
  best <- FALSE
  for (f in slot$forms) {
    if (tok$buried[i]) {
      if (!(f$code %in% .FORMULA_BURIED)) next
      v <- if (is.na(tok$top[i])) NA else tok$top[i] <= ctx$buried_within_cm
    } else {
      if (!identical(tok$code[i], f$code)) next
      v <- TRUE
      req <- f$required
      if (length(req)) {
        miss <- setdiff(req, tok$features[[i]])
        if (length(miss)) v <- if (all(miss %in% tok$features_na[[i]])) NA else FALSE
      }
      if (!is.na(f$rockflag) && !identical(tok$rockflag[i], f$rockflag)) v <- FALSE
      if (!is.na(f$mark) && !identical(tok$mark[i], f$mark)) v <- FALSE
      if (length(f$composition) && !isFALSE(v)) {
        tc <- tok$composition[[i]]
        if (!length(tc)) v <- if (isTRUE(v)) NA else v
        else if (!all(f$composition %in% tc)) v <- FALSE
      }
    }
    if (isTRUE(v)) return(TRUE)
    if (is.na(v)) best <- NA
  }
  best
}

# Edit distance between the tokens in play and the tested slots (an optional
# slot is skipped at no cost, a required one at cost 1, an extra token costs
# 1, a mismatched pair 1), with the operations of one cheapest alignment.
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

# Match one choice of tokens (a completed profile with its surface chosen):
# the verdict and the distance, and with `evidence` the alignment read out.
.mhs_align <- function(ch, ctx, evidence = TRUE) {
  tok <- ch$tok
  slots <- ctx$slots
  m <- length(slots)
  n0 <- if (is.null(tok)) 0L else tok$n
  play <- if (n0) {
    p <- .mhs_in_play(tok$code, ctx) & !tok$buried
    if (ctx$buried_play) p <- p | (tok$buried & (is.na(tok$top) | tok$top <= ctx$buried_within_cm))
    if (!is.na(ch$forced)) p[ch$forced] <- TRUE
    p
  } else logical(0)
  pi <- which(play)
  n <- length(pi)
  notes <- character(0)
  # "directly under": the rows with no diagnostic horizon between two horizons
  # in play may sum to max_gap_cm (or the allowance of the pair); transitional
  # rows and sub-horizons are skipped, foreign horizons ignored
  gaps <- logical(0)
  if (n > 1L) {
    gaps <- vapply(seq_len(n - 1L), function(k) {
      a <- pi[k]; b <- pi[k + 1L]
      mid <- if (b - a > 1L) (a + 1L):(b - 1L) else integer(0)
      mid <- mid[tok$code[mid] == "<none>"]
      if (!length(mid)) return(TRUE)
      key <- paste0(tok$code[a], ">", tok$code[b])
      allow <- if (!is.null(ctx$pair_gaps_cm) && key %in% names(ctx$pair_gaps_cm))
                 ctx$pair_gaps_cm[[key]] else ctx$max_gap_cm
      th <- sum(tok$thick[mid])
      v <- if (allow <= 0) FALSE else if (is.na(th)) NA else th <= allow
      if (evidence && !isTRUE(v)) notes <<- c(notes, sprintf("%s lies under %s across %s cm with no diagnostic horizon (allowed %s)",
                                                 tok$label[b], tok$label[a],
                                                 if (is.na(th)) "an unknown thickness" else format(th), format(allow)))
      v
    }, NA)
  }
  C3 <- matrix(FALSE, n, m)
  for (a in seq_len(n)) for (j in seq_len(m)) C3[a, j] <- .mhs_compat(tok, pi[a], slots[[j]], ctx)
  opt <- vapply(slots, function(z) z$optional, NA)
  Copt <- C3; Copt[is.na(Copt)] <- TRUE
  Cpes <- C3; Cpes[is.na(Cpes)] <- FALSE
  dopt <- .mhs_dp(Copt, opt, trace = evidence)
  dpes <- if (anyNA(C3)) .mhs_dp(Cpes, opt, trace = FALSE) else dopt
  gap_bad <- sum(gaps %in% FALSE)
  value <- if (dpes$cost == 0 && all(gaps %in% TRUE)) TRUE
           else if (dopt$cost > 0 || gap_bad > 0) FALSE else NA
  if (!evidence) return(list(value = value, distance = dopt$cost + gap_bad, surface = ch$surface))
  # the evidence of the cheapest alignment
  matched <- list(); missing <- character(0); extra <- character(0); absent <- character(0)
  for (op in dopt$ops) {
    if (op[1L] == 1L) {
      a <- pi[op[2L]]; z <- slots[[op[3L]]]
      if (Copt[op[2L], op[3L]]) {
        matched[[length(matched) + 1L]] <- data.frame(
          slot = z$index, written = z$written, horizon = tok$label[a], code = tok$code[a],
          top_cm = tok$top[a], bottom_cm = tok$bottom[a],
          decided = !is.na(C3[op[2L], op[3L]]), stringsAsFactors = FALSE)
        typ <- setdiff(unique(unlist(lapply(z$forms, function(f) f$typical))), tok$features[[a]])
        if (length(typ) && !tok$buried[a])
          notes <- c(notes, sprintf("typical feature(s) %s of %s not recorded on %s",
                                    paste(typ, collapse = ", "), z$written, tok$label[a]))
      } else {
        missing <- c(missing, z$written)
        extra <- c(extra, tok$label[a])
        if (tok$code[a] %in% z$codes)
          notes <- c(notes, sprintf("%s does not have what %s requires", tok$label[a], z$written))
      }
    } else if (op[1L] == 2L) {
      z <- slots[[op[3L]]]
      if (z$optional) absent <- c(absent, z$written) else missing <- c(missing, z$written)
    } else {
      extra <- c(extra, tok$label[pi[op[2L]]])
    }
  }
  ignored <- if (n0) tok$label[!play & tok$code != "<none>"] else character(0)
  matched <- if (length(matched)) do.call(rbind, matched)
             else data.frame(slot = integer(0), written = character(0), horizon = character(0),
                             code = character(0), top_cm = numeric(0), bottom_cm = numeric(0),
                             decided = logical(0), stringsAsFactors = FALSE)
  list(value = value, distance = dopt$cost + gap_bad, matched = matched,
       missing = missing, extra = extra, absent = absent, ignored = ignored,
       surface = ch$surface, absorbed = ch$absorbed,
       sequence = if (n) tok$code[pi] else character(0),
       notes = unique(notes))
}

# A necessary condition for a match, checked before the completions of
# undecided layers are tried: the match relaxed so that every completion's
# alignment is one of its paths. An undecided layer may fill any slot one of
# its options names, or vanish when an option is foreign or no horizon; a
# decided horizon in play must fill a slot, or may vanish when it could be
# merged with the horizon before it (same slot again) or read as a feature of
# the surface (a surface code in the top run); features, flags, gaps and the
# forced surface are ignored (they only take matches away). Returns the
# cheapest relaxed edit distance: 0 when a match is possible, else a lower
# bound of the distance of every completion.
.mhs_relaxed <- function(rows, base, und, opts, ctx) {
  slots <- ctx$slots
  m <- length(slots)
  req <- vapply(slots, function(z) !z$optional, NA)
  scodes <- lapply(slots, function(z) z$codes)
  surf_rule <- length(ctx$agro) || length(ctx$natural)
  vanish_reps <- c("<none>", "<f>", "<fa>", "<fn>")
  el <- list()
  top_run <- TRUE
  n <- nrow(rows)
  r <- 1L
  while (r <= n) {
    if (rows$buried[r] > 0L) {
      b <- rows$buried[r]; r2 <- r
      while (r2 < n && rows$buried[r2 + 1L] == b) r2 <- r2 + 1L
      top <- suppressWarnings(min(rows$top_cm[r:r2]))
      play <- ctx$buried_play && (!is.finite(top) || top <= ctx$buried_within_cm)
      if (play) el[[length(el) + 1L]] <- list(
        fits = vapply(scodes, function(cc) any(cc %in% .FORMULA_BURIED), NA), vanish = FALSE)
      top_run <- FALSE
      r <- r2 + 1L
      next
    }
    k <- match(r, und)
    if (!is.na(k)) {
      o <- opts[[k]]
      surf <- surf_rule && top_run && any(.mhs_is_agro(o, ctx) | .mhs_is_natural(o, ctx))
      el[[length(el) + 1L]] <- list(fits = vapply(scodes, function(cc) any(o %in% cc), NA),
                                    vanish = any(o %in% vanish_reps) || surf)
    } else if (base[r] != "<none>") {
      c1 <- base[r]
      is_surf <- .mhs_is_agro(c1, ctx) || .mhs_is_natural(c1, ctx)
      if (.mhs_in_play(c1, ctx)) {
        el[[length(el) + 1L]] <- list(fits = vapply(scodes, function(cc) c1 %in% cc, NA),
                                      vanish = surf_rule && top_run && is_surf)
      }
      if (!is_surf) top_run <- FALSE
    }
    r <- r + 1L
  }
  # cost[j + 1, f + 1]: slots 1..j resolved, f = the last element filled slot j
  cost <- matrix(Inf, m + 1L, 2L)
  cost[1L, 1L] <- 0
  skip <- function(a, b) if (b > a + 1L) sum(req[(a + 1L):(b - 1L)]) else 0
  for (e in el) {
    nw <- matrix(Inf, m + 1L, 2L)
    for (j in 0:m) for (f in 1:2) {
      c0 <- cost[j + 1L, f]
      if (!is.finite(c0)) next
      if (e$vanish) nw[j + 1L, f] <- min(nw[j + 1L, f], c0)
      nw[j + 1L, f] <- min(nw[j + 1L, f], c0 + 1)
      if (f == 2L && j > 0L && e$fits[j]) nw[j + 1L, 2L] <- min(nw[j + 1L, 2L], c0)
      if (j < m) for (j2 in (j + 1L):m) {
        nw[j2 + 1L, 2L] <- min(nw[j2 + 1L, 2L], c0 + skip(j, j2) + !e$fits[j2])
      }
    }
    cost <- nw
  }
  fin <- vapply(0:m, function(j) min(cost[j + 1L, ]) + skip(j, m + 1L), 0)
  min(fin)
}

#' Match a profile against a profile formula
#'
#' Three-valued match of the identified horizons of a profile against the
#' profile formula of a type (the Russian Field Guide of 2008 names its types
#' by such formulas: "O-EL-BEL-BT-C"). Generic: the codes, the vocabulary and
#' the surface horizons are the caller's.
#'
#' The profile side is collapsed top-down: small horizons and transitional
#' rows are dropped; consecutive rows with the same code are merged
#' (sub-horizons: BT1, BT2 = BT); the rows of a buried profile become one
#' buried token; overlays become features of their row. With `surface` and
#' `agro` codes, the surface slot is read by the thickest-horizon rule: the
#' agro horizon if the surface run has one, else the thickest natural horizon
#' starting at the surface or under a cover thinner than `max_cover_cm`; the
#' other horizons of that run are read as features and leave the sequence.
#'
#' The match is closed on `vocabulary` (the codes of every scheme of a
#' division): a horizon of the vocabulary, or of the formula, present in the
#' profile must be matched by a slot, in order; a horizon foreign to both is
#' ignored. Optional slots (in parentheses) may be absent; untested parent
#' slots (C, D, R unless flagged) are left out, and their horizons ignored.
#' Two horizons matched one after the other must lie directly one under the
#' other: rows with no diagnostic horizon between them may sum to
#' `max_gap_cm`, or to the allowance of the pair in `pair_gaps_cm`.
#'
#' A layer whose code is undecided (NA, with or without a list of
#' candidates) is completed in every way that matters: TRUE when every
#' completion matches, FALSE when none does, NA otherwise. A required
#' feature listed as undecided, a turbated composition not recorded, or an
#' unknown thickness, leave the result NA when they decide it.
#'
#' @param table The identified horizons: the frame of
#'   \code{\link{identify_horizons}}, any data frame with a \code{code} column
#'   (and optionally \code{horizon_idx}, \code{top_cm}, \code{bottom_cm},
#'   \code{passed}, \code{candidates}, \code{features}, \code{features_na},
#'   \code{overlay}, \code{small}, \code{transitional}, \code{buried},
#'   \code{rockflag}, \code{mark}, \code{composition}), or a character vector
#'   of codes from the top (NA an undecided layer, "" no diagnostic horizon).
#' @param spec The formula: a string, or one parsed by the package's parser.
#' @param vocabulary Codes the match is closed on; NULL closes it on the
#'   formula's own codes only.
#' @param surface,agro Natural and agro surface codes for the
#'   thickest-horizon rule; both NULL: no rule, the first horizon is the
#'   surface.
#' @param max_cover_cm Thickness of cover above which no surface run is read.
#' @param max_gap_cm,pair_gaps_cm The gap allowed between two consecutive
#'   horizons (cm), and named allowances for pairs (\code{c("AU>BCA" = 15)}).
#' @param buried_within_cm Depth within which a buried profile counts.
#' @param max_completions Most completions of undecided layers tried; beyond
#'   it the result is NA.
#' @param ... Passed to the parser when `spec` is a string (for example
#'   \code{test_terminals = "R"}, \code{optional = "BEL"}).
#' @return A list: \code{passed} (TRUE, FALSE or NA), \code{formula},
#'   \code{specificity}, \code{distance} (edit distance of the closest
#'   reading: 0 for a match), \code{matched} (a data frame of slots and the
#'   horizons that fill them), \code{missing} (slots not filled),
#'   \code{extra} (horizons in play not matched), \code{absent} (optional
#'   slots not present), \code{ignored} (horizons left out of the match),
#'   \code{surface} and \code{absorbed} (the surface horizon and the ones read
#'   as its features), \code{sequence} (the codes in play), \code{undecided}
#'   (layers whose completion was followed) and \code{notes}.
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
#' @export
match_horizon_sequence <- function(table, spec, vocabulary = NULL,
                                   surface = NULL, agro = NULL, max_cover_cm = 40,
                                   max_gap_cm = 0, pair_gaps_cm = NULL,
                                   buried_within_cm = Inf, max_completions = 2000L, ...) {
  if (is.character(spec)) spec <- .parse_profile_formula(spec, ...)
  if (!inherits(spec, "soilkey_formula"))
    rlang::abort("'spec' must be a profile formula string or a parsed formula.")
  tab <- .as_horizon_table(table)
  slots <- Filter(function(z) isTRUE(z$tested) && z$kind %in% c("horizon", "buried"), spec$slots)
  spec_codes <- unique(unlist(lapply(slots, function(z) z$codes)))
  play <- unique(c(setdiff(as.character(vocabulary), spec$parents), spec_codes))
  ctx <- list(slots = slots, spec_codes = spec_codes, play = play,
              agro = as.character(agro), natural = setdiff(as.character(surface), as.character(agro)),
              buried_play = any(.FORMULA_BURIED %in% play),
              max_cover_cm = max_cover_cm, max_gap_cm = max_gap_cm,
              pair_gaps_cm = pair_gaps_cm, buried_within_cm = buried_within_cm,
              top0 = suppressWarnings(min(tab$top_cm, na.rm = TRUE)))
  if (!is.finite(ctx$top0)) ctx$top0 <- NA_real_
  rows <- tab[!(tab$small | tab$transitional), , drop = FALSE]
  und <- which(is.na(rows$code) & !(rows$passed %in% FALSE) & rows$buried == 0L)
  # a decided row keeps its code; a row with no diagnostic horizon is "<none>"
  base <- ifelse(is.na(rows$code), "<none>", rows$code)
  universe <- unique(c(as.character(vocabulary), spec_codes, ctx$agro, ctx$natural))
  universe <- setdiff(universe, .FORMULA_BURIED)
  opts <- lapply(und, function(r) {
    cand <- rows$candidates[[r]]
    o <- if (length(cand)) .mhs_category(cand, ctx)
         else c(.mhs_category(universe, ctx), "<f>", "<none>")
    unique(o)
  })
  total <- if (length(opts)) prod(as.numeric(lengths(opts))) else 1
  # no completion can match: FALSE without trying them, the relaxed distance
  # as the distance, the evidence of one completion
  if (total > 1) {
    lb <- .mhs_relaxed(rows, base, und, opts, ctx)
    if (lb > 0) {
      codes <- base
      codes[und] <- vapply(opts, function(o) if ("<none>" %in% o) "<none>" else o[1L], "")
      best <- .mhs_align(.mhs_surface(.mhs_tokens(rows, codes), ctx)[[1L]], ctx)
      return(list(passed = FALSE, formula = spec$formula, specificity = spec$specificity,
                  distance = lb, matched = best$matched, missing = best$missing,
                  extra = best$extra, absent = best$absent, ignored = best$ignored,
                  surface = best$surface, absorbed = best$absorbed, sequence = best$sequence,
                  undecided = character(0),
                  notes = unique(c(best$notes, sprintf(
                    "no completion of the %d undecided layer(s) can match", length(und))))))
    }
  }
  ix <- rep(1L, length(opts))
  outs <- list()
  vals <- logical(0)
  where <- list()
  tried <- 0
  capped <- FALSE
  repeat {
    codes <- base
    if (length(und)) codes[und] <- vapply(seq_along(und), function(k) opts[[k]][ix[k]], "")
    tok <- .mhs_tokens(rows, codes)
    chs <- .mhs_surface(tok, ctx)
    for (ci in seq_along(chs)) {
      r <- .mhs_align(chs[[ci]], ctx, evidence = FALSE)
      outs[[length(outs) + 1L]] <- r
      where[[length(where) + 1L]] <- list(codes = codes, choice = ci)
      vals <- c(vals, r$value)
    }
    tried <- tried + 1
    if (anyNA(vals) || (any(vals) && !all(vals))) break
    if (tried >= total) break
    if (tried >= max_completions) { capped <- TRUE; break }
    k <- length(ix)
    while (k > 0L) {
      if (ix[k] < length(opts[[k]])) { ix[k] <- ix[k] + 1L; break }
      ix[k] <- 1L; k <- k - 1L
    }
  }
  value <- if (capped || anyNA(vals) || (any(vals) && !all(vals))) NA else all(vals)
  bi <- if (isTRUE(value)) 1L
        else if (isFALSE(value)) which.min(vapply(outs, function(o) o$distance, 0))
        else {
          w <- which(is.na(vals))
          if (length(w)) w[1L] else which(vals)[1L]
        }
  # the evidence of the representative completion, read out once
  best <- .mhs_align(.mhs_surface(.mhs_tokens(rows, where[[bi]]$codes), ctx)[[where[[bi]]$choice]], ctx)
  notes <- best$notes
  if (capped) notes <- c(notes, sprintf(
    "%d undecided layer(s): more than %d completions, so the match is left undecided",
    length(und), as.integer(max_completions)))
  undecided <- character(0)
  if (length(und) && !isTRUE(value) && !isFALSE(value)) {
    undecided <- vapply(und, function(r) {
      cand <- rows$candidates[[r]]
      sprintf("%s (%s)", rows$horizon_idx[r],
              if (length(cand)) paste(ifelse(nzchar(cand), cand, "no horizon"), collapse = ", ") else "any")
    }, "")
  }
  if (length(unique(vapply(outs, function(o) o$surface %||% NA_character_, ""))) > 1L)
    notes <- c(notes, "the thickest surface horizon is not decided by the recorded depths")
  list(passed = value, formula = spec$formula, specificity = spec$specificity,
       distance = min(vapply(outs, function(o) o$distance, 0)),
       matched = best$matched, missing = best$missing, extra = best$extra,
       absent = best$absent, ignored = best$ignored,
       surface = best$surface, absorbed = best$absorbed, sequence = best$sequence,
       undecided = undecided, notes = unique(notes))
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
# decision Q15, 2026-11-02), then the more specific, then the list order.
.nearest_schemes <- function(results, n = 3L) {
  nm <- names(results) %||% vapply(results, function(r) r$formula, "")
  d <- vapply(results, function(r) r$distance, 0)
  sp <- vapply(results, function(r) as.numeric(r$specificity[1L]), 0)
  ord <- order(d, -sp, seq_along(results))
  ord <- utils::head(ord, n)
  data.frame(code = nm[ord], formula = vapply(results[ord], function(r) r$formula, ""),
             distance = d[ord],
             missing = vapply(results[ord], function(r) paste(r$missing, collapse = ", "), ""),
             extra = vapply(results[ord], function(r) paste(r$extra, collapse = ", "), ""),
             stringsAsFactors = FALSE, row.names = NULL)
}

# Match a profile against a list of schemes (a division): named formulas,
# strings or parsed, in the order of trial. The vocabulary defaults to the
# union of the schemes' codes. Returns the pick (.pick_scheme()), the nearest
# schemes when none matched, and every result.
.match_schemes <- function(table, specs, vocabulary = NULL, parse_args = list(),
                           n_nearest = 3L, ...) {
  if (is.character(specs)) specs <- as.list(specs)
  specs <- lapply(specs, function(s) if (is.character(s)) do.call(.parse_profile_formula, c(list(s), parse_args)) else s)
  if (is.null(names(specs))) names(specs) <- vapply(specs, function(s) s$formula, "")
  vocabulary <- vocabulary %||% .formula_vocabulary(specs)
  res <- lapply(specs, function(sp) match_horizon_sequence(table, sp, vocabulary = vocabulary, ...))
  pick <- .pick_scheme(res)
  pick$nearest <- if (!isTRUE(pick$passed)) .nearest_schemes(res, n_nearest) else NULL
  pick$results <- res
  pick
}
