# =============================================================================
# v0.9.335 -- the horizon notation of the Russian soil classification (Field
# Guide of 2008, sections 3.2 and 3.3, pp. 59-75).
#
# A diagnostic horizon is written in Latin capitals (AY, BEL, BHF), its
# features in lower case to its right, separated by commas (BCAs,cs, Cca,s);
# a feature that forms a thin band is a small horizon written alone (O-e-BHF-
# C); three are bracketed ([hh], [e-hf], [akl]); BH and BF (of BHF) and AV (of
# V) are modifications written in capitals; G or Q after C or D is a gleyed
# or quasi-gleyed rock (CG, CQ, DG), not a G or Q horizon; ~~ " ^^ "" = .. after
# a rock name its genesis or texture; a leading Arabic (3.3) or Roman (5.3)
# numeral a lithological layer; trailing digits a sub-horizon (BT1, BT2).
#
# The notation gives the profile type only when the profile is written in it
# (Hugo's decision Q3, 2026-11-02): declared in site$designation_system, or
# recognised by a symbol only this notation has, once the Cyrillic letters
# that look like Latin ones are folded; never from the country, since Russian
# legacy data use the Soviet notation of 1977. FAO letters, and the Soviet
# ones, are not translated into Russian horizons: they give hints to the key
# on the data. Almost every lower-case letter means something else in the two
# notations (f, g, i, m, q, r, s, u, v, x, y), so the notation is decided for
# the whole profile before any letter is read.
#
# The vocabulary is the catalogue of inst/rules/kidpr2008 (horizons.yaml,
# features.yaml); the grammar is the one of .parse_profile_formula(), whose
# helpers read each designation. Nothing here stops with an error: a
# designation that cannot be read is returned unread, with a note.
# =============================================================================

# site$designation_system values that declare the notation, compared once
# lower-cased and stripped of everything but letters and digits ("Russian
# 2008" -> russian2008).
.KIDPR_DECLARED <- c("kidpr", "kidpr2008", "ru", "ru2004", "ru2008", "russian",
                     "russia", "rfg2008", "rsc2004", "rus", "russian2004", "russian2008")

# Values of site$designation_system that declare nothing.
.KIDPR_UNDECLARED <- c("na", "none", "unknown", "nd", "null", "missing", "notrecorded")

# Masters only the Russian notation writes, in three classes for the
# recognition of an undeclared profile:
#   strong: no other notation spells them, not even in capitals (no FAO
#     master starts with P, S, T or M), with the tokens of the chapter 6
#     formulas PY and AKL and the technogenic surface formations TPO;
#   ambiguous: an upper-cased FAO designation spells them as often as the
#     Russian notation does (Ah, Ak, Ao, Bh, Bf);
#   likely: the others (AY, EL, BEL, BHF, BCA ...).
# BT, BI and BM recognise nothing: other notations write them in capitals.
.KIDPR_STRONG <- c("SEL", "TUR", "PU", "PB", "PC", "PT", "PTR", "TO", "TE", "TJ",
                   "TT", "ML", "SS", "PY", "AKL", "TPO")
.KIDPR_AMBIGUOUS <- c("AH", "AK", "AO", "BH", "BF")
.KIDPR_LIKELY <- c("AY", "AJ", "AU", "AEL", "BEL", "EL", "BHF", "BFM", "BMK",
                   "BPL", "BAN", "BCA", "CAT", "ASN", "BSN", "CRM", "CRH", "RU",
                   "RY", "RJ", "CLM", "AV")
# The masters of the 2008 notation, for the record (all three classes).
.KIDPR_ONLY <- c(.KIDPR_LIKELY, .KIDPR_AMBIGUOUS, .KIDPR_STRONG)

# First letters of the Russian masters no FAO (2006) master shares.
.KIDPR_NON_FAO_INITIALS <- c("P", "S", "T", "M", "G", "Q", "F", "V", "X", "D")

# The numbered tokens of the Soviet notation of 1977 (A0, A1, A2, A1A2, A2B,
# B1, B2, the peaty AT, Ap and Apakh with a Cyrillic p).
.KIDPR_SOVIET_RE <- "^(A00|A[0-3]([^0-9]|$)|A0A1|A1A2|A2B|AT([^A-Z]|$)|B[1-3]([^0-9]|$)|A[\u043f\u041f])"

# Horizons the key identifies and then looks past for a second horizon in the
# same layer (positions 3, 4 and 23): written after a "+" ("AU+S").
.KIDPR_OVERLAYS <- c("S", "X", "V")

# Transitions written with the generic letters (AB, BC): not diagnostic.
.KIDPR_GENERIC_TRANSITIONS <- c("AB", "BA", "BC", "CB", "AC")

# Rock and parent symbols: the only indices a rock mark is written on.
.KIDPR_ROCKS <- c("C", "D", "R", "CLM", "TT")

# Designations that record nothing.
.KIDPR_MISSING <- c("na", "n/a", "nd", "n.d.", "-", "--", "?", "??", "...", "null")

.kidpr_cache <- new.env(parent = emptyenv())

# The catalogue of inst/rules/kidpr2008: horizons.yaml and features.yaml.
.kidpr_catalogue <- function() {
  if (is.null(.kidpr_cache$catalogue)) {
    dir <- system.file("rules", "kidpr2008", package = "soilKey")
    if (!nzchar(dir)) rlang::abort("The catalogue of the Russian notation is not installed.")
    .kidpr_cache$catalogue <- list(
      horizons = yaml::read_yaml(file.path(dir, "horizons.yaml")),
      features = yaml::read_yaml(file.path(dir, "features.yaml")))
  }
  .kidpr_cache$catalogue
}

# The vocabulary the reader works with, built once from the catalogue.
.kidpr_vocabulary <- function() {
  if (!is.null(.kidpr_cache$vocabulary)) return(.kidpr_cache$vocabulary)
  ct <- .kidpr_catalogue()
  h <- ct$horizons
  f <- ct$features
  code <- function(l) vapply(l, function(e) as.character(e$code), "")
  named <- function(l, field) {
    v <- vapply(l, function(e) as.character(e[[field]] %||% NA_character_)[1L], "")
    stats::setNames(v, code(l))
  }
  hz <- code(h$horizons)
  rocks <- code(h$rock_symbols)
  legacy <- code(h$legacy)
  group <- c(named(h$horizons, "group"), stats::setNames(rep("rock", length(rocks)), rocks))
  group[["TPO"]] <- "not_soil"
  lower <- Filter(function(e) !identical(e$written_as, "horizon"), f$features)
  falias <- unlist(lapply(f$features, function(e) {
    a <- as.character(unlist(e$aliases))
    stats::setNames(rep(as.character(e$code), length(a)), a)
  }))
  legacy_features <- lapply(h$legacy, function(e) as.character(unlist(e$features)))
  names(legacy_features) <- legacy
  legacy_candidates <- lapply(h$legacy, function(e) as.character(unlist(e$candidates)))
  names(legacy_candidates) <- legacy
  feats_all <- c(f$features, f$components)
  voc <- list(
    horizons = hz,
    group = group,
    agro = hz[vapply(h$horizons, function(e) isTRUE(e$agro), NA)],
    overlays = hz[vapply(h$horizons, function(e) isTRUE(e$overlay), NA)],
    modifications = named(h$modifications, "of"),
    master_aliases = named(h$aliases, "reads_as"),
    rocks = rocks,
    legacy = named(h$legacy, "reads_as"),
    legacy_features = legacy_features,
    legacy_candidates = legacy_candidates,
    feature_codes = code(f$features),
    feature_aliases = falias,
    components = code(f$components),
    small = code(Filter(function(e) isTRUE(e$small), feats_all)),
    ambiguous = code(Filter(function(e) isTRUE(e$ambiguous), feats_all)),
    flag_masters = c("C", "D"),
    rock_flags = vapply(h$rock_flags, function(e) as.character(e$code), ""),
    brackets = code(Filter(function(e) startsWith(as.character(e$code), "["), f$features)),
    bracket_parts = lapply(stats::setNames(
      Filter(function(e) startsWith(as.character(e$code), "["), f$features),
      code(Filter(function(e) startsWith(as.character(e$code), "["), f$features))),
      function(e) as.character(unlist(e$components %||% gsub("[][]", "", e$code))))
  )
  voc$masters <- unique(c(hz, names(voc$modifications), names(voc$master_aliases),
                          rocks, legacy))
  # the features the parser accepts: every code written in lower case or as a
  # symbol, the bracketed ones, their aliases and the parts of [e-hf]
  voc$features <- unique(c(code(lower), names(falias), voc$components))
  voc$ambiguous <- unique(c(voc$ambiguous, names(falias)[falias %in% voc$ambiguous]))
  voc$small <- unique(c(voc$small, names(falias)[falias %in% voc$small]))
  # host forms of the chapter 4 lists that do not read as written (Tote)
  hf <- Filter(function(e) !is.null(e$reads_as), f$host_forms)
  voc$rewrites <- stats::setNames(vapply(hf, function(e) as.character(e$reads_as), ""),
                                  vapply(hf, function(e) as.character(e$written), ""))
  .kidpr_cache$vocabulary <- voc
  voc
}

# The vocabulary as the helpers of .parse_profile_formula() take it.
.kidpr_engine_voc <- function(voc, s) {
  list(masters = voc$masters, features = voc$features, parents = .FORMULA_PARENTS,
       rock_flags = voc$rock_flags, flag_masters = voc$flag_masters, formula = s)
}


# --- site$designation_system ------------------------------------------------------

# The declared notation as a key ("Russian 2008" -> "russian2008"), or NA
# when the site declares none (no site, no value, or a value such as none,
# unknown or n/a). Never an error, whatever the site is.
.kidpr_declared_key <- function(site) {
  if (!is.list(site)) return(NA_character_)
  v <- tryCatch(site[["designation_system"]], error = function(e) NULL)
  v <- tryCatch(as.character(unlist(v)), error = function(e) character(0))
  if (!length(v) || is.na(v[1L])) return(NA_character_)
  v <- v[1L]
  if (!validUTF8(v)) return(NA_character_)
  k <- gsub("[^a-z0-9]", "", tolower(v))
  if (!nzchar(k) || k %in% .KIDPR_UNDECLARED || !grepl("[a-z]", k)) return(NA_character_)
  k
}

# Does a value of designation_system declare the Russian notation?
.kidpr_is_declared <- function(ds) {
  .kidpr_declared_key(list(designation_system = ds)) %in% .KIDPR_DECLARED
}


# --- cleaning a designation --------------------------------------------------------

# Characters folded before anything is read: zero-width characters and
# combining marks dropped (no index carries an accent; base R has no NFC),
# the no-break space to a space, full-width ASCII to ASCII, tilde look-alikes
# to ~, the Ukrainian I to I.
.kidpr_fold_codepoints <- function(x) {
  cp <- utf8ToInt(x)
  if (anyNA(cp)) return(x)
  drop <- (cp >= 0x200B & cp <= 0x200D) | cp == 0xFEFF | (cp >= 0x0300 & cp <= 0x036F)
  cp <- cp[!drop]
  fw <- cp >= 0xFF01 & cp <= 0xFF5E
  cp[fw] <- cp[fw] - 0xFEE0L
  cp[cp == 0x00A0] <- 0x20L
  cp[cp %in% c(0x02DC, 0x223C, 0x301C)] <- 0x7EL
  cp[cp == 0x0406] <- 0x49L
  cp[cp == 0x0456] <- 0x69L
  intToUtf8(cp)
}

# Is `w` a run of features (letters, commas, the symbols) of the vocabulary,
# or a bracketed feature?
.kidpr_feature_run <- function(w, voc) {
  if (grepl("^\\[[a-z][a-z,-]*\\]$", w)) return(TRUE)
  if (!grepl("^[a-z@\u22a5\uf05e,]+$", w, perl = TRUE)) return(FALSE)
  runs <- strsplit(w, ",", fixed = TRUE)[[1L]]
  runs <- runs[nzchar(runs)]
  length(runs) > 0L && all(vapply(runs, function(r) {
    r <- gsub("[@\u22a5\uf05e]", "", r, perl = TRUE)
    !nzchar(r) || !is.null(.formula_decompose(r, voc$features))
  }, NA))
}

# One designation cleaned for the reader: list(s, notes, cyrillic). Text that
# is not valid UTF-8 is read as Windows-1251 (the Cyrillic code page of most
# Russian files) when that gives valid text, else left unread. NA, N/A, ND,
# "-" and "?" record nothing. A numeral written apart is joined to its master
# ("II BT"); spaces inside brackets, around commas and around "+" and "/" are
# dropped ("[AY, EL]tr", "BCAs, cs"); a short feature run or a bracketed
# feature written after a space is joined to its index ("BCA s,cs",
# "AJ [akl]"); then a note after a space, or after a slash followed by a word
# ("BT/glossic"), and a trailing "?" are dropped, each noted. The Cyrillic
# look-alikes are folded to Latin letters, curly quotes and primes to a
# straight quote, the Symbol-font perpendicular to U+22A5 and dashes to a
# hyphen (.formula_normalise()); the Cyrillic TPO becomes TPO.
.kidpr_clean1 <- function(x, voc = .kidpr_vocabulary()) {
  out <- list(s = NA_character_, notes = character(0), cyrillic = FALSE)
  if (is.na(x)) return(out)
  x <- tryCatch(enc2utf8(x), error = function(e) x)
  if (!validUTF8(x)) {
    y <- suppressWarnings(iconv(x, "CP1251", "UTF-8"))
    if (is.na(y) || !validUTF8(y)) {
      out$notes <- "not readable text (invalid characters)"
      return(out)
    }
    x <- y
    out$notes <- "read as Windows-1251 (Cyrillic) text"
  }
  x <- .kidpr_fold_codepoints(x)
  x <- trimws(gsub("[[:space:]]+", " ", x, perl = TRUE))
  if (!nzchar(x)) return(out)
  if (tolower(x) %in% .KIDPR_MISSING) {
    out$notes <- c(out$notes, sprintf("'%s' records nothing", x))
    return(out)
  }
  x <- sub("^([IVX]+|[0-9]+) +(?=[A-Z\u0410-\u042f(\\[])", "\\1", x, perl = TRUE)
  repeat {
    x2 <- gsub("([\\[(][^\\])]*?) +", "\\1", x, perl = TRUE)
    if (identical(x2, x)) break
    x <- x2
  }
  x2 <- gsub(" *([,+/]) *", "\\1", x, perl = TRUE)
  if (!identical(x2, x)) {
    out$notes <- c(out$notes, "spaces around a comma, '+' or '/' dropped")
    x <- x2
  }
  sp <- regexpr(" ", x, fixed = TRUE)
  if (sp > 0L) {
    head <- substr(x, 1L, sp - 1L)
    rest <- substring(x, sp + 1L)
    w <- sub(" .*$", "", rest)
    wf <- .formula_normalise(w)
    join <- grepl("[A-Z0-9\u0410-\u042f)]$", head, perl = TRUE) &&
      (grepl("^\\[", wf) || grepl(",", wf, fixed = TRUE) || nchar(wf) <= 2L) &&
      .kidpr_feature_run(wf, voc)
    if (join) {
      out$notes <- c(out$notes, sprintf("'%s' after a space read as part of the designation", w))
      x <- paste0(head, w)
      rest <- sub("^[^ ]* ?", "", rest)
    } else {
      x <- head
    }
    if (nzchar(rest)) out$notes <- c(out$notes, sprintf("'%s' after a space read as a note", rest))
  }
  sl <- regexpr("/\\p{Ll}{3,}", x, perl = TRUE)
  if (sl > 0L) {
    out$notes <- c(out$notes, sprintf("'%s' read as a note", substring(x, sl + 1L)))
    x <- substr(x, 1L, sl - 1L)
  }
  if (grepl("\\?+$", x)) {
    out$notes <- c(out$notes, "a trailing '?' (an uncertain designation) dropped")
    x <- sub("\\?+$", "", x)
  }
  out$cyrillic <- grepl("[\u0400-\u04ff]", x, perl = TRUE)
  x <- .formula_normalise(x)
  x <- gsub("T\u041fO", "TPO", x, fixed = TRUE)
  # a single quote after a rock symbol: the ash mark (as chapter 6 prints it)
  q1 <- "^((?:[2-9]|II|III|IV|VI|VII|VIII|IX)?(?:C|D|R|CLM|TT)[GQ]?[a-z,]*)['`]$"
  if (grepl(q1, x, perl = TRUE)) {
    x <- sub(q1, "\\1\"", x, perl = TRUE)
    out$notes <- c(out$notes, "a single quote after the rock read as the ash mark")
  }
  if (nzchar(x) && !(tolower(x) %in% .KIDPR_MISSING)) out$s <- x
  out
}

# The designations of a profile cleaned (.kidpr_clean1), never an error.
.kidpr_clean <- function(desig) {
  d <- tryCatch(as.character(unlist(desig)), error = function(e) character(0))
  voc <- .kidpr_vocabulary()
  lapply(d, function(x) tryCatch(.kidpr_clean1(x, voc), error = function(e)
    list(s = NA_character_, notes = sprintf("not read: %s", conditionMessage(e)), cyrillic = FALSE)))
}

# The designations as the reader sees them (NA when nothing is recorded).
.kidpr_normalise <- function(d) {
  vapply(.kidpr_clean(d), function(z) z$s, "", USE.NAMES = FALSE)
}


# --- one designation ---------------------------------------------------------

# The fields of one row, at their defaults (single: an internal mark of two
# indices run together whose second has one letter, BTG).
.kidpr_row <- function(written = NA_character_) {
  list(written = written, index = NA_character_, modification = NA_character_,
       group = NA_character_, agro = FALSE, features = character(0),
       small = FALSE, bracket = NA_character_, composition = character(0),
       remnant = FALSE, buried = FALSE, lith = 1L, subdiv = NA_character_,
       rockflag = NA_character_, mark = NA_character_, transitional = FALSE,
       masters = character(0), overlay = character(0), legacy = FALSE,
       known = FALSE, ambiguous = NA_character_, notes = character(0),
       hint = character(0), hint_features = character(0), single = FALSE)
}

# Split s on `sep` outside brackets.
.kidpr_split_outside <- function(s, sep) {
  ch <- strsplit(s, "", fixed = TRUE)[[1L]]
  depth <- 0L
  cut <- integer(0)
  for (k in seq_along(ch)) {
    if (ch[k] %in% c("(", "[", "{")) depth <- depth + 1L
    else if (ch[k] %in% c(")", "]", "}")) depth <- depth - 1L
    else if (ch[k] == sep && depth == 0L) cut <- c(cut, k)
  }
  if (!length(cut)) return(s)
  from <- c(1L, cut + 1L)
  to <- c(cut - 1L, length(ch))
  mapply(function(a, b) if (b >= a) paste(ch[a:b], collapse = "") else "", from, to,
         USE.NAMES = FALSE)
}

# Is s wholly inside one pair of brackets of the kind `open`?
.kidpr_wrapped <- function(s, open = "(") {
  ch <- strsplit(s, "", fixed = TRUE)[[1L]]
  length(ch) >= 2L && ch[1L] == open &&
    isTRUE(tryCatch(.formula_close(ch, 1L, s) == length(ch), error = function(e) FALSE))
}

# A lithological prefix: an Arabic numeral 2-9 or a Roman one II-IX before a
# capital. Returns the layer number and the rest.
.kidpr_lith <- function(s) {
  m <- regmatches(s, regexpr("^([2-9]|VIII|VII|III|IX|VI|IV|II)(?=[A-Z])", s, perl = TRUE))
  if (!length(m) || !nzchar(m)) return(list(lith = 1L, rest = s))
  n <- if (grepl("^[0-9]$", m)) as.integer(m)
       else match(m, c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"))
  list(lith = n, rest = substring(s, nchar(m) + 1L))
}

# The longest master of the vocabulary that starts the run of capitals at the
# start of s, the run, and what follows the run.
.kidpr_master <- function(s, voc) {
  run <- regmatches(s, regexpr("^[A-Z]+", s))
  if (!length(run)) return(list(master = NA_character_, run = "", after = "", tail = s))
  pre <- voc$masters[startsWith(run, voc$masters)]
  master <- if (length(pre)) pre[which.max(nchar(pre))] else NA_character_
  after <- if (is.na(master)) run else substring(run, nchar(master) + 1L)
  list(master = master, run = run, after = after, tail = substring(s, nchar(run) + 1L))
}

# Features and sub-horizon digits written after an index, read as
# .formula_form() reads them; the index is replaced by a stand-in master.
.kidpr_tail_form <- function(tail, voc) {
  if (grepl("[A-Z\\[]", tail)) return(simpleError("capitals after the index"))
  s <- paste0("X", tail)
  tryCatch(.formula_form(s, .kidpr_engine_voc(voc, s))[[1L]], error = function(e) e)
}

# Fill a row from a form of .formula_form(): index, sub-horizon, rock flag,
# mark, features (written and in parentheses), composition.
.kidpr_from_form <- function(r, f) {
  r$index <- f$code
  r$rockflag <- f$rockflag
  r$mark <- f$mark
  r$subdiv <- f$subdiv
  r$features <- unique(c(r$features, f$features, f$typical))
  r$composition <- f$composition
  r$known <- TRUE
  r
}

# The features of a row in canonical form (tu -> tr, ko -> ek, ag -> agr,
# ael -> ae, as -> cs, hh -> [hh], akl -> [akl]), with a note per alias, and
# the ambiguity note for a feature written without commas that could be read
# as two (hi, gr, md, mr, rh, ur, yu, hh, ff, rr, rx, hf, agr).
.kidpr_canonical_features <- function(r, voc, commas) {
  fe <- r$features
  if (!length(fe)) return(r)
  amb <- if (!commas) intersect(fe, voc$ambiguous) else character(0)
  if (length(amb)) {
    r$ambiguous <- sprintf("%s written without commas, read by the longest match (%s)",
                           r$written, paste(amb, collapse = ", "))
  }
  al <- fe %in% names(voc$feature_aliases)
  if (any(al)) {
    r$notes <- c(r$notes, sprintf("feature %s read as %s", fe[al],
                                  voc$feature_aliases[fe[al]]))
    fe[al] <- unname(voc$feature_aliases[fe[al]])
  }
  r$features <- unique(fe)
  r
}

# Modifications, aliases and legacy tokens read as the horizon they stand for.
.kidpr_canonical_index <- function(r, voc) {
  ix <- r$index
  if (is.na(ix)) return(r)
  if (ix %in% names(voc$modifications)) {
    r$modification <- ix
    r$index <- unname(voc$modifications[[ix]])
  } else if (ix %in% names(voc$master_aliases)) {
    r$index <- unname(voc$master_aliases[[ix]])
    r$notes <- c(r$notes, sprintf("%s read as %s", ix, r$index))
  } else if (ix %in% names(voc$legacy)) {
    r$legacy <- TRUE
    to <- voc$legacy[[ix]]
    r$features <- unique(c(voc$legacy_features[[ix]], r$features))
    r$index <- if (is.na(to)) NA_character_ else unname(to)
    if (is.na(to)) {
      r$hint <- voc$legacy_candidates[[ix]]
      r$notes <- c(r$notes, sprintf("%s: a token of the chapter 6 formulas with no 2008 horizon of its own (candidates %s)",
                                    ix, paste(r$hint, collapse = ", ")))
    } else {
      r$notes <- c(r$notes, sprintf("%s: a token of the chapter 6 formulas, read as %s",
                                    ix, paste0(to, paste(voc$legacy_features[[ix]], collapse = ""))))
    }
  }
  if (!is.na(r$index)) {
    r$group <- unname(voc$group[r$index]) %||% NA_character_
    r$agro <- r$index %in% voc$agro
  }
  r
}

# The features of a bracket written in lower case ("[e-hf]", "[hh]", "[g]",
# "[e,hf]"): the catalogue's bracketed feature, or its parts read as features
# with a note (a bracket whose parts are those of a catalogue bracket in
# another order or with a comma is that bracket). NULL when unreadable.
.kidpr_bracket_features <- function(tok, voc) {
  if (tok %in% voc$features) return(list(features = tok, notes = character(0)))
  body <- gsub("^\\[|\\]$", "", tok)
  parts <- strsplit(body, "[-,]")[[1L]]
  parts <- parts[nzchar(parts)]
  if (!length(parts)) return(NULL)
  fe <- lapply(parts, function(p) .formula_decompose(p, voc$features))
  if (any(vapply(fe, is.null, NA))) return(NULL)
  fe <- unlist(fe)
  for (b in voc$brackets) {
    if (setequal(fe, voc$bracket_parts[[b]]) && length(fe) == length(voc$bracket_parts[[b]])) {
      return(list(features = b, notes = sprintf("%s read as %s", tok, b)))
    }
  }
  list(features = fe, notes = sprintf("%s is no bracketed feature of the Field Guide: read as %s",
                                      tok, paste(fe, collapse = ", ")))
}

# A row of features written alone: a small horizon, when every feature may
# form one (the catalogue's small flag); a feature that is only written on a
# horizon (ca, g, s, v) leaves the row unread.
.kidpr_small_row <- function(r, fe, voc) {
  canon <- ifelse(fe %in% names(voc$feature_aliases), voc$feature_aliases[fe], fe)
  not_small <- setdiff(canon, voc$small)
  if (length(not_small)) {
    r$notes <- c(r$notes, sprintf("%s is a feature written on a horizon, not a small horizon",
                                  paste(not_small, collapse = ", ")))
    return(r)
  }
  r$small <- TRUE
  r$features <- fe
  r$known <- TRUE
  r
}

# One designation with no "+" (overlay) or "/" (transition) outside
# brackets; never an error.
.kidpr_read_one <- function(s, voc, caps) {
  r <- tryCatch(.kidpr_read_one_(s, voc, caps), error = function(e) {
    z <- .kidpr_row(s)
    z$notes <- sprintf("not read: %s", conditionMessage(e))
    z
  })
  if (!r$known) r$lith <- 1L
  r
}

.kidpr_read_one_ <- function(s, voc, caps) {
  r <- .kidpr_row(s)
  if (is.na(s) || !nzchar(s)) return(r)
  commas <- grepl(",", s, fixed = TRUE)
  ch <- strsplit(s, "", fixed = TRUE)[[1L]]
  first <- ch[1L]
  # a remnant: the whole designation in parentheses
  if (.kidpr_wrapped(s, "(")) {
    r <- .kidpr_parse_str(substr(s, 2L, nchar(s) - 1L), voc, caps)
    r$written <- s
    r$remnant <- r$known
    return(r)
  }
  if (first %in% c("[", "{")) {
    close <- tryCatch(.formula_close(ch, 1L, s), error = function(e) NA_integer_)
    if (is.na(close)) {
      r$notes <- "unbalanced brackets"
      return(r)
    }
    body <- substr(s, 2L, close - 1L)
    tail <- substring(s, close + 1L)
    if (!nzchar(body)) {
      r$notes <- "empty brackets"
      return(r)
    }
    if (body == "ABC") {
      if (nzchar(tail)) {
        r$notes <- sprintf("'%s' after a buried profile", tail)
        return(r)
      }
      r$buried <- TRUE
      r$known <- TRUE
      r$notes <- "a buried profile"
      return(r)
    }
    if (first == "{") {
      r$notes <- "braces mark only a buried profile, {ABC}"
      return(r)
    }
    if (grepl("^[a-z]", body)) {
      # a bracketed feature written alone: a small horizon
      if (nzchar(tail)) {
        r$notes <- sprintf("'%s' after the bracketed feature %s", tail, substr(s, 1L, close))
        return(r)
      }
      bf <- .kidpr_bracket_features(s, voc)
      if (is.null(bf)) {
        r$notes <- sprintf("%s: no feature of the Field Guide", s)
        return(r)
      }
      r$notes <- bf$notes
      r <- .kidpr_small_row(r, bf$features, voc)
      if (r$known && length(bf$features) == 1L && startsWith(bf$features, "[")) r$bracket <- bf$features
      return(.kidpr_canonical_features(r, voc, commas = TRUE))
    }
    parts <- .kidpr_split_outside(body, ",")
    if (length(parts) == 1L) parts <- .kidpr_split_outside(body, "+")
    tf <- if (nzchar(tail)) .kidpr_tail_form(tail, voc) else NULL
    if (inherits(tf, "error")) {
      r$notes <- sprintf("'%s' after the brackets is no feature", tail)
      return(r)
    }
    if (length(parts) > 1L) {
      # a turbated mixture of horizons written as one row: [AY, EL]tr
      comp <- vapply(parts, function(p) {
        m <- .kidpr_master(p, voc)
        if (!is.na(m$master) && !nzchar(m$after) && !nzchar(m$tail)) m$master else NA_character_
      }, "", USE.NAMES = FALSE)
      if (anyNA(comp)) {
        r$notes <- sprintf("%s: a mixture of indices that are not all horizons", s)
        return(r)
      }
      r$composition <- comp
      r$features <- unique(c(tf$features, tf$typical))
      r$bracket <- s
      r$known <- TRUE
      r$notes <- sprintf("a turbated mixture of %s", paste(comp, collapse = ", "))
      return(.kidpr_canonical_features(r, voc, commas))
    }
    # one horizon in brackets, with its own features: a buried horizon
    inner <- .kidpr_read_one(body, voc, caps)
    if (!inner$known || is.na(inner$index) || inner$transitional || inner$buried) {
      r$notes <- sprintf("%s: no horizon in the brackets", s)
      return(r)
    }
    r <- inner
    r$written <- s
    r$buried <- TRUE
    if (!is.null(tf)) {
      r$features <- unique(c(r$features, tf$features, tf$typical))
      if (!is.na(tf$subdiv)) r$subdiv <- tf$subdiv
    }
    r$notes <- c(r$notes, "a buried horizon (written in brackets)")
    return(.kidpr_canonical_features(r, voc, commas))
  }
  if (first %in% c(letters, .FORMULA_SYMBOLS)) {
    # features written alone: a small horizon
    ev <- .kidpr_engine_voc(voc, s)
    fe <- tryCatch(.formula_features(s, ev), error = function(e) e)
    if (inherits(fe, "error")) {
      r$notes <- conditionMessage(fe)
      return(r)
    }
    r <- .kidpr_small_row(r, fe, voc)
    return(.kidpr_canonical_features(r, voc, commas))
  }
  lp <- .kidpr_lith(s)
  r$lith <- lp$lith
  body <- lp$rest
  # a bracketed feature written on its host (AJ[akl], EL[e-hf])
  bm <- regmatches(body, gregexpr("\\[[a-z][a-z,-]*\\]", body))[[1L]]
  if (length(bm)) {
    bf <- lapply(bm, .kidpr_bracket_features, voc = voc)
    if (any(vapply(bf, is.null, NA))) {
      r$notes <- sprintf("%s: a bracket that holds no feature of the Field Guide", s)
      return(r)
    }
    body2 <- gsub("\\[[a-z][a-z,-]*\\]", "", body)
    r <- .kidpr_read_one(paste0(substr(s, 1L, nchar(s) - nchar(body)), body2), voc, caps)
    r$written <- s
    r$features <- unique(c(r$features, unlist(lapply(bf, `[[`, "features"))))
    r$notes <- c(r$notes, unlist(lapply(bf, `[[`, "notes")))
    return(r)
  }
  mm <- .kidpr_master(body, voc)
  # a transition written with the generic letters (AB, BC, CB)
  if (mm$run %in% .KIDPR_GENERIC_TRANSITIONS) {
    tf <- .kidpr_tail_form(mm$tail, voc)
    if (!inherits(tf, "error")) {
      r$transitional <- TRUE
      r$masters <- strsplit(mm$run, "", fixed = TRUE)[[1L]]
      r$features <- unique(c(tf$features, tf$typical))
      r$subdiv <- tf$subdiv
      r$known <- TRUE
      r$notes <- sprintf("a transition (%s), not diagnostic", mm$run)
      return(.kidpr_canonical_features(r, voc, commas))
    }
  }
  if (is.na(mm$master)) {
    r$notes <- sprintf("'%s' is no index of the Russian notation", s)
    return(r)
  }
  after <- mm$after
  flag <- ""
  if (nzchar(after) && mm$master %in% voc$flag_masters && substr(after, 1L, 1L) %in% voc$rock_flags) {
    flag <- substr(after, 1L, 1L)
    after <- substring(after, 2L)
  }
  if (nzchar(after)) {
    # all capitals: the capitals after an index are its features (BTG = BT
    # + g), unless they name a rock (BTD, a transition to the rock D); after
    # C or D the first G or Q stays the rock flag (CCA = C + ca)
    m2 <- .kidpr_master(after, voc)
    to_rock <- !is.na(m2$master) && !nzchar(m2$after) && m2$master %in% .KIDPR_ROCKS
    lowered <- NULL
    if (caps && !to_rock) {
      tl <- gsub("([A-Z]+)(?![^\\[]*\\])", "\\L\\1", paste0(after, mm$tail), perl = TRUE)
      s2 <- paste0(mm$master, flag, tl)
      f2 <- tryCatch(.formula_form(s2, .kidpr_engine_voc(voc, s2)), error = function(e) e)
      if (!inherits(f2, "error")) {
        lowered <- s2
        r <- .kidpr_from_form(r, f2[[1L]])
        r$notes <- sprintf("all-capitals notation: %s read as %s", s, s2)
        r <- .kidpr_canonical_features(r, voc, commas)
        return(.kidpr_canonical_index(r, voc))
      }
    }
    # two indices run together: a transition (AUBM, BMC, AUB), not
    # diagnostic; one index twice (BTBT) is that horizon
    second <- if (!is.na(m2$master) && !nzchar(m2$after)) m2$master
              else if (after %in% c("A", "B")) after else NA_character_
    if (!is.na(second)) {
      tf <- .kidpr_tail_form(mm$tail, voc)
      if (!inherits(tf, "error")) {
        r$features <- unique(c(tf$features, tf$typical))
        r$subdiv <- tf$subdiv
        r$known <- TRUE
        if (nzchar(flag)) r$rockflag <- flag
        if (second == mm$master && !nzchar(flag)) {
          r$index <- mm$master
          r$notes <- sprintf("%s: one horizon written twice, read as %s", s, mm$master)
          r <- .kidpr_canonical_features(r, voc, commas)
          return(.kidpr_canonical_index(r, voc))
        }
        r$transitional <- TRUE
        r$masters <- c(mm$master, second)
        # one letter after the index, not a rock (BTG, AUB): a transition,
        # or an index with a feature written in capitals
        r$single <- nchar(second) == 1L && !(second %in% .KIDPR_ROCKS)
        r$notes <- sprintf("a transition (%s, %s), not diagnostic", mm$master, second)
        return(.kidpr_canonical_features(r, voc, commas))
      }
    }
    r$notes <- sprintf("'%s': capitals after the index %s that are no index", s, mm$master)
    return(r)
  }
  fm <- tryCatch(.formula_form(body, .kidpr_engine_voc(voc, body)), error = function(e) e)
  if (inherits(fm, "error")) {
    r$notes <- conditionMessage(fm)
    return(r)
  }
  r <- .kidpr_from_form(r, fm[[1L]])
  if (length(r$composition) && r$index != "TUR") {
    r <- .kidpr_row(s)
    r$notes <- sprintf("%s: only TUR is written with the horizons it is made of", s)
    return(r)
  }
  if (!is.na(r$mark) && !(r$index %in% .KIDPR_ROCKS)) {
    r$notes <- c(r$notes, sprintf("the mark %s is written on a rock (C, D, R, CLM, TT), not on %s",
                                  r$mark, r$index))
  }
  if (length(fm) > 1L) {
    r$notes <- c(r$notes, sprintf("alternatives written: %s",
                                  paste(vapply(fm[-1L], function(f) f$signature %||%
                                    paste0(f$code, if (!is.na(f$rockflag)) f$rockflag,
                                           if (!is.na(f$mark)) f$mark), ""), collapse = ", ")))
  }
  r <- .kidpr_canonical_features(r, voc, commas)
  .kidpr_canonical_index(r, voc)
}

# A cleaned designation, with its overlays ("+") and transitions ("/").
.kidpr_parse_str <- function(s, voc, caps) {
  if (is.na(s) || !nzchar(s)) return(.kidpr_row())
  if (s %in% names(voc$rewrites)) {
    r <- .kidpr_parse_str(voc$rewrites[[s]], voc, caps)
    r$written <- s
    r$notes <- c(sprintf("%s read as %s (chapter 4 list)", s, voc$rewrites[[s]]), r$notes)
    return(r)
  }
  if (.kidpr_wrapped(s, "(")) {
    r <- .kidpr_parse_str(substr(s, 2L, nchar(s) - 1L), voc, caps)
    r$written <- s
    r$remnant <- r$known
    return(r)
  }
  parts <- .kidpr_split_outside(s, "+")
  if (length(parts) > 1L) {
    # a horizon and an overlay in one layer (Hugo's default 10, 2026-11-02)
    empty <- !nzchar(parts)
    parts <- parts[!empty]
    if (!length(parts)) {
      r <- .kidpr_row(s)
      r$notes <- "nothing around the '+'"
      return(r)
    }
    rows <- lapply(parts, .kidpr_parse_str, voc = voc, caps = caps)
    ix <- vapply(rows, function(z) z$index %||% NA_character_, "")
    ov <- ix %in% voc$overlays
    host <- which(!ov & !is.na(ix))[1L]
    if (is.na(host)) host <- 1L
    r <- rows[[host]]
    r$written <- s
    r$overlay <- unique(ix[setdiff(which(ov), host)])
    r$known <- all(vapply(rows, function(z) z$known, NA)) &&
      length(r$overlay) == length(parts) - 1L
    r$notes <- c(r$notes, unlist(lapply(rows[-host], function(z) z$notes)))
    if (any(empty)) r$notes <- c(r$notes, "an empty part beside the '+' ignored")
    if (!r$known) r$notes <- c(r$notes, "only S, X or V can be written over another horizon")
    return(r)
  }
  parts <- .kidpr_split_outside(s, "/")
  if (length(parts) > 1L) {
    rows <- lapply(parts, .kidpr_read_one, voc = voc, caps = caps)
    ok <- all(vapply(rows, function(z) z$known && !z$small && !z$buried && !is.na(z$written) &&
                       (z$transitional || !is.na(z$index)), NA))
    r <- .kidpr_row(s)
    if (!ok) {
      r$notes <- sprintf("'%s' is no transition of two indices", s)
      return(r)
    }
    ix <- unlist(lapply(rows, function(z) if (z$transitional) z$masters else z$index))
    if (length(unique(ix)) == 1L) {
      # sub-horizons of one horizon (BT1/BT2): that horizon
      r <- rows[[1L]]
      r$written <- s
      r$features <- unique(unlist(lapply(rows, function(z) z$features)))
      r$subdiv <- NA_character_
      r$notes <- c(r$notes, sprintf("%s: parts of one horizon, read as %s", s, ix[1L]))
      return(r)
    }
    r$transitional <- TRUE
    r$masters <- ix
    r$known <- TRUE
    r$lith <- rows[[1L]]$lith
    r$features <- unique(unlist(lapply(rows, function(z) z$features)))
    r$notes <- sprintf("a transition (%s), not diagnostic", paste(ix, collapse = ", "))
    return(r)
  }
  .kidpr_read_one(s, voc, caps)
}

#' Read one designation in the Russian notation of 2008
#'
#' @param d A designation as recorded.
#' @param caps TRUE when the profile is written all in capitals (lower-case
#'   letters written as capitals): the capitals after an index are then read
#'   as its features (BTG = BT + g).
#' @return A list: \code{written} (the designation once cleaned),
#'   \code{index} (the canonical horizon or rock symbol; NA for a small
#'   horizon, a transition or an unread designation), \code{modification}
#'   (BH, BF, AV), \code{group} (the chapter 3 group), \code{agro},
#'   \code{features} (canonical codes), \code{small} (a feature written
#'   alone), \code{bracket} ([e-hf], [hh], [akl], or a turbated mixture),
#'   \code{composition} (the horizons of TUR[...] or of a mixture),
#'   \code{remnant} (written in parentheses), \code{buried}, \code{lith},
#'   \code{subdiv}, \code{rockflag} (G or Q after C or D), \code{mark},
#'   \code{transitional} and \code{masters} (two indices run together or
#'   joined by "/": not diagnostic, p. 36), \code{overlay} (S, X or V written
#'   after a "+"), \code{legacy} (a token of the chapter 6 formulas),
#'   \code{known}, \code{ambiguous} and \code{notes}.
#' @noRd
.kidpr_parse_one <- function(d, caps = FALSE) {
  voc <- .kidpr_vocabulary()
  cl <- .kidpr_clean(d)[[1L]] %||% list(s = NA_character_, notes = character(0), cyrillic = FALSE)
  .kidpr_parse_clean(cl, caps, voc)
}

# A designation already cleaned (.kidpr_clean1): read, with the notes of the
# cleaning.
.kidpr_parse_clean <- function(cl, caps, voc) {
  if (is.na(cl$s)) {
    r <- .kidpr_row()
    r$notes <- cl$notes
    return(r)
  }
  r <- tryCatch(.kidpr_parse_str(cl$s, voc, caps), error = function(e) {
    z <- .kidpr_row(cl$s)
    z$notes <- sprintf("not read: %s", conditionMessage(e))
    z
  })
  r$notes <- c(if (cl$cyrillic) "Cyrillic letters read as the Latin ones they look like",
               cl$notes, r$notes)
  r
}


# --- the notation of a profile -------------------------------------------------

# The master that starts a designation (outer parentheses or brackets, a
# lithological prefix and an overlay or transition after it aside), or NA.
.kidpr_first_master <- function(s, voc) {
  if (is.na(s)) return(NA_character_)
  s <- sub("^\\((.*)\\)$", "\\1", s)
  s <- .kidpr_split_outside(s, "+")[1L]
  s <- .kidpr_split_outside(s, "/")[1L]
  s <- sub("^\\[([A-Z0-9]+[^],+]*)\\].*$", "\\1", s)
  .kidpr_master(.kidpr_lith(s)$rest, voc)$master
}

# A sign of the Russian notation in the writing of one designation (not its
# index): the permafrost sign, a rock mark after C, D or TT, a comma between
# features, a bracketed feature or a turbated composition.
.kidpr_written_sign <- function(s) {
  if (is.na(s)) return(FALSE)
  grepl("\u22a5", s, fixed = TRUE) ||
    grepl("^(?:[2-9]|II|III|IV|VI|VII|VIII|IX)?(?:C|D|TT)[GQ]?[a-z,]*(?:\\([a-z,]+\\))?(?:~~|\\^\\^|\\.\\.|=|\"\"|\")", s, perl = TRUE) ||
    grepl("[a-z@\u22a5],[a-z@\u22a5]", s, perl = TRUE) ||
    grepl("\\[(hh|e-hf|akl)\\]|TUR\\[|\\[[A-Z]+(,[A-Z]+)+\\][a-z]", s, perl = TRUE)
}

# Does a designation the Russian notation does not read (or reads as two
# indices run together, BTG) read as an FAO one, once its letters after the
# master are in lower case when it is written in capitals?
.kidpr_fao_like <- function(s, r, caps_only) {
  if (is.na(s) || !grepl("^[0-9]*[A-Z][A-Za-z]", s)) return(FALSE)
  if (r$known && !isTRUE(r$single)) return(FALSE)
  low <- if (caps_only) sub("^([0-9]*[A-Z])(.*)$", "\\1\\L\\2", s, perl = TRUE) else s
  if (identical(low, s) && caps_only) return(FALSE)
  !is.null(tryCatch(.parse_designation_hscs1(low), error = function(e) NULL))
}

# The decision for a profile, from its cleaned designations and their
# reading in the Russian notation (normal mode).
.kidpr_decide <- function(s, rows, site) {
  voc <- .kidpr_vocabulary()
  present <- !is.na(s)
  sp <- s[present]
  rp <- rows[present]
  caps_only <- length(sp) > 0L && !any(grepl("\\p{Ll}", sp, perl = TRUE))
  fao <- vapply(seq_along(sp), function(i) .kidpr_fao_like(sp[i], rp[[i]], caps_only), NA)
  caps <- caps_only && any(fao)
  key <- .kidpr_declared_key(site)
  if (!is.na(key)) {
    raw <- as.character(unlist(site[["designation_system"]]))[1L]
    return(list(ok = key %in% .KIDPR_DECLARED, caps = caps,
                why = sprintf("site$designation_system = '%s'", raw)))
  }
  if (!length(sp)) return(list(ok = FALSE, caps = FALSE, why = "no designation"))
  m <- vapply(sp, .kidpr_first_master, "", voc = voc, USE.NAMES = FALSE)
  soviet <- grepl(.KIDPR_SOVIET_RE, sp, perl = TRUE)
  # a profile with Soviet numbered tokens: AO is the Soviet A0 typed with an O
  if (any(soviet)) m[m %in% "AO"] <- NA_character_
  sign <- vapply(sp, .kidpr_written_sign, NA, USE.NAMES = FALSE)
  strong <- unique(c(m[m %in% .KIDPR_STRONG], sp[sign]))
  likely <- m[m %in% .KIDPR_LIKELY]
  ambig <- unique(m[m %in% .KIDPR_AMBIGUOUS])
  non_fao <- any(substr(m[vapply(rp, function(z) z$known, NA)], 1L, 1L) %in% .KIDPR_NON_FAO_INITIALS, na.rm = TRUE)
  why <- function(codes, how) sprintf("%s: %s", paste(unique(codes), collapse = ", "), how)
  if (length(strong)) {
    return(list(ok = TRUE, caps = caps,
                why = why(c(m[m %in% c(.KIDPR_STRONG, .KIDPR_LIKELY)], sp[sign]),
                          "written only in the Russian notation")))
  }
  if (caps_only && sum(fao) > length(likely)) {
    return(list(ok = FALSE, caps = caps,
                why = if (any(soviet)) "the Soviet notation of 1977 (A0, A1, A2, B1 ...): read only as hints"
                      else sprintf("an FAO profile written in capitals (%s)", paste(sp[fao], collapse = ", "))))
  }
  if (length(likely)) {
    return(list(ok = TRUE, caps = caps, why = why(likely, sprintf("written only in the Russian notation%s",
                                                                  if (caps) " (an all-capitals profile)" else ""))))
  }
  # Ah, Ak, Ao, Bh, Bf written in capitals: Russian only where case is kept
  # (some lower-case letter in the profile) or another index no FAO
  # designation starts with (P, S, T, G ...) stands beside them; never
  # beside designations that read as FAO ones
  if (length(ambig) && !any(fao) && (!caps_only || non_fao)) {
    return(list(ok = TRUE, caps = caps, why = why(ambig, "Russian indices (the case of the letters is kept)")))
  }
  list(ok = FALSE, caps = caps,
       why = if (any(soviet)) "the Soviet notation of 1977 (A0, A1, A2, B1 ...): read only as hints"
             else if (length(ambig)) sprintf("%s alone may be an FAO designation written in capitals",
                                             paste(ambig, collapse = ", "))
             else "no index only the Russian notation writes (the country is not read)")
}

#' Is a profile written in the Russian notation of 2008?
#'
#' In order: site$designation_system decides when it names a notation (the
#' Russian names, or another notation). Otherwise the profile is Russian when
#' one designation starts with an index no other notation writes (SEL, TUR,
#' PU, PT, TO ...), carries a sign only this notation writes (the permafrost
#' sign, a rock mark, a comma between features, a bracketed feature), or
#' starts with an index only this notation writes in capitals (AY, EL, BEL,
#' BHF, BCA ...); in a profile written all in capitals those count only while
#' the designations that read as FAO ones (AP, BW, BTG) are not more
#' numerous. AH, AK, AO, BH and BF count only where the case of the letters
#' is kept or an index no FAO designation starts with stands beside them. A
#' profile with the numbered tokens of the Soviet notation (A1, A2, B1 ...)
#' reads AO as the Soviet A0. The country is never read.
#' @return A list: \code{ok}, \code{why}, \code{caps} (written all in
#'   capitals, with designations that read as FAO ones).
#' @noRd
.kidpr_notation <- function(desig, site) {
  voc <- .kidpr_vocabulary()
  cl <- .kidpr_clean(desig)
  s <- vapply(cl, function(z) z$s, "")
  rows <- lapply(cl, .kidpr_parse_clean, caps = FALSE, voc = voc)
  .kidpr_decide(s, rows, site)
}


# --- hints from other notations --------------------------------------------------

# Codes of a group of horizons, for the hints.
.KIDPR_HINT_GROUPS <- list(
  humus = c("AY", "AJ", "AU", "AH", "AK", "AO", "W"),
  eluvial = c("E", "EL", "AEL", "BEL", "SEL"),
  agro = c("P", "PU", "PB", "PT", "PTR"),
  organic = c("T", "H", "O"),
  peat = c("T", "TO", "TE", "TJ")
)

#' What an FAO (2006) or Soviet (1977) designation suggests to the key on the
#' data: candidate Russian horizons and features, never a horizon (Hugo's
#' decision Q3, 2026-11-02). Soviet: A00 fresh litter; A0 (also typed AO or
#' Ao) the litter or O; A1 a humus horizon, A2 an eluvial one, A1A2 AEL, A2B
#' BEL; Ap or Apakh (Cyrillic) the plough layer; AT a peaty horizon (T or AO);
#' T peat; G gley. FAO: Ap agro; Ah or A humus; Az S; E eluvial; Bt BT or BI;
#' Btn, Bn ASN or BSN; Bss (FAO i) V; Bs, Bh, and the iron B of the Soviet
#' and Canadian notations (Bf, Bhf, Bfe) BHF; Bk, Bca BCA or CAT; Bw BM, BFM or
#' BMK; r G; z on a B or C the feature s; k or ca on a C the feature ca; cs
#' the feature cs; g the feature g; Oi, L litter in a run of 5 cm or less at
#' the top, else T, H or O; Oe, Oa, H T, H or O by the
#' decomposition; @ the feature @; f on an A or C permafrost.
#' @noRd
.kidpr_hint <- function(d, caps = FALSE) {
  cl <- .kidpr_clean(d)[[1L]] %||% list(s = NA_character_, notes = character(0))
  .kidpr_hint_clean(cl, caps)
}

.kidpr_hint_clean <- function(cl, caps = FALSE) {
  r <- .kidpr_row(cl$s)
  r$notes <- cl$notes
  s <- cl$s
  if (is.na(s)) return(r)
  hint <- function(codes, features = character(0), note) {
    r$hint <<- unique(c(r$hint, codes))
    r$hint_features <<- unique(c(r$hint_features, features))
    r$notes <<- c(r$notes, note)
  }
  # the Soviet forms, before any letter is lowered
  if (grepl("^A(00|oo|OO)$", s)) {
    hint(character(0), note = "Soviet A00: fresh litter, not diagnostic"); return(r)
  }
  if (grepl("^A0A1", s)) {
    hint(c("AO", "O"), note = "Soviet A0A1: litter over humus, AO or O by the data"); return(r)
  }
  if (grepl("^A[0oO]$", s)) {
    hint("O", note = sprintf("%s: the Soviet A0, litter or the litter-peat O", s)); return(r)
  }
  if (grepl("^A[\u043f\u041f]", s)) {
    hint(.KIDPR_HINT_GROUPS$agro, note = "Soviet plough layer: an agro horizon"); return(r)
  }
  if (grepl("^A1A2", s)) {
    hint(c("AEL", .KIDPR_HINT_GROUPS$eluvial), note = "Soviet A1A2: humus-eluvial"); return(r)
  }
  if (grepl("^A2B", s)) {
    hint("BEL", note = "Soviet A2B: an eluvial-illuvial transition, BEL"); return(r)
  }
  if (grepl("^A1([^0-9A-Z]|$)", s)) {
    hint(.KIDPR_HINT_GROUPS$humus, note = "Soviet A1: a humus horizon"); return(r)
  }
  if (grepl("^A2([^0-9A-Z]|$)", s)) {
    hint(.KIDPR_HINT_GROUPS$eluvial, note = "Soviet A2: an eluvial horizon"); return(r)
  }
  if (grepl("^AT([^A-Z]|$)", s)) {
    hint(c("T", "AO"), note = "Soviet AT: a peaty horizon, T or AO by the data"); return(r)
  }
  if (grepl("^T([0-9]|$)", s)) {
    hint(.KIDPR_HINT_GROUPS$peat, note = sprintf("%s: peat", s)); return(r)
  }
  if (grepl("^G([0-9a-z]|$)", s)) {
    hint("G", note = sprintf("%s: gley", s)); return(r)
  }
  # in an all-capitals profile the letters after the master are suffixes,
  # unless they are a second master (AB, BC: a transition)
  if (caps && !grepl("^[0-9]*[HOLAEBCRIW]{2}[0-9]*$", s)) {
    s <- sub("^([0-9]*[A-Z])(.*)$", "\\1\\L\\2", s, perl = TRUE)
  }
  p <- tryCatch(.parse_designation_hscs1(s), error = function(e) NULL)
  if (is.null(p) && grepl("^[@\u22a5]+$", s)) {
    hint(character(0), if (grepl("@", s, fixed = TRUE)) "@" else "\u22a5",
         sprintf("%s: a sign written alone", s))
    return(r)
  }
  if (is.null(p)) {
    r$notes <- c(r$notes, sprintf("'%s': no hint", s))
    return(r)
  }
  m <- p$master
  l <- unique(c(p$strong, p$weak))
  low <- gsub("[^a-z]", "", s)
  if (grepl("cs", low, fixed = TRUE)) {
    hint(character(0), "cs", sprintf("%s: gypsum", s))
    l <- setdiff(l, c("c", "s"))
  }
  if (grepl("ca", low, fixed = TRUE)) l <- unique(c(setdiff(l, c("c", "a")), "k"))
  if (m == "L" || (m == "O" && "i" %in% l)) {
    hint(.KIDPR_HINT_GROUPS$organic,
         note = sprintf("%s: litter, not diagnostic, in a run of 5 cm or less at the top; else T, H or O by the decomposition", s))
  } else if (m == "H" || (m == "O" && any(c("e", "a") %in% l))) {
    hint(.KIDPR_HINT_GROUPS$organic, note = sprintf("%s: T, H or O by the decomposition", s))
  } else if (m == "O") {
    hint(.KIDPR_HINT_GROUPS$organic, note = sprintf("%s: an organic horizon", s))
  } else if (m == "A") {
    if ("p" %in% l) hint(.KIDPR_HINT_GROUPS$agro, note = sprintf("%s: an agro horizon", s))
    else hint(.KIDPR_HINT_GROUPS$humus, note = sprintf("%s: a humus horizon", s))
    if ("z" %in% l) hint("S", note = sprintf("%s: salts at the surface", s))
  } else if (m == "E") {
    hint(.KIDPR_HINT_GROUPS$eluvial, note = sprintf("%s: an eluvial horizon", s))
  } else if (m == "B") {
    if ("n" %in% l) hint(c("ASN", "BSN"), note = sprintf("%s: a solonetz horizon", s))
    else if ("t" %in% l) hint(c("BT", "BI"), note = sprintf("%s: clay illuviation", s))
    if (grepl("ss", low) || "i" %in% l) hint("V", note = sprintf("%s: slickensides", s))
    if (any(c("s", "h", "f") %in% l)) hint("BHF", note = sprintf("%s: an illuvial Al-Fe-humus horizon", s))
    if ("k" %in% l) hint(c("BCA", "CAT"), note = sprintf("%s: carbonates", s))
    if ("w" %in% l) hint(c("BM", "BFM", "BMK"), note = sprintf("%s: a structural or colour B", s))
    if ("z" %in% l) hint(character(0), "s", sprintf("%s: salts", s))
    if (!length(r$hint) && !length(r$hint_features)) r$notes <- c(r$notes, sprintf("%s: a middle horizon", s))
  } else if (m %in% c("C", "R")) {
    r$notes <- c(r$notes, sprintf("%s: parent rock", s))
    if ("k" %in% l) hint(character(0), "ca", sprintf("%s: carbonates", s))
    if ("z" %in% l) hint(character(0), "s", sprintf("%s: salts", s))
  }
  if ("r" %in% l && m %in% c("B", "C")) hint("G", note = sprintf("%s: strong reduction", s))
  if ("g" %in% l) hint(character(0), "g", sprintf("%s: stagnic or gleyic mottling", s))
  if (grepl("@", s, fixed = TRUE)) hint(character(0), "@", sprintf("%s: cryoturbation", s))
  if ("f" %in% l && m %in% c("A", "C")) hint(character(0), "\u22a5", sprintf("%s: frozen", s))
  r
}


# --- a whole profile -------------------------------------------------------------

#' All the designations of a profile, read in the Russian notation
#'
#' The profile's notation is decided first (.kidpr_notation()); in the
#' Russian notation every designation is read (.kidpr_parse_one()), and a row
#' it cannot read gets the hints of the FAO letters; in another notation no
#' letter is read as a Russian horizon and every row gets only hints. Each
#' designation is cleaned and read once; nothing stops with an error.
#' @return A list of vectors, one element per designation (as
#'   .cpcs_designations()): the fields of .kidpr_parse_one() (list columns for
#'   features, composition, masters, overlay, notes, hint and hint_features),
#'   then \code{kidpr} (the profile is in the Russian notation), \code{caps}
#'   (written all in capitals), \code{why} and \code{any_known}.
#' @noRd
.kidpr_designations <- function(desig, site) {
  voc <- .kidpr_vocabulary()
  cl <- .kidpr_clean(desig)
  s <- vapply(cl, function(z) z$s, "")
  rows <- lapply(cl, .kidpr_parse_clean, caps = FALSE, voc = voc)
  nt <- .kidpr_decide(s, rows, site)
  if (nt$ok) {
    if (nt$caps) rows <- lapply(cl, .kidpr_parse_clean, caps = TRUE, voc = voc)
    rows <- lapply(seq_along(rows), function(i) {
      z <- rows[[i]]
      if (!z$known && !is.na(z$written)) {
        h <- .kidpr_hint_clean(cl[[i]], caps = nt$caps)
        z$hint <- unique(c(z$hint, h$hint))
        z$hint_features <- h$hint_features
        z$notes <- unique(c(z$notes, h$notes))
      }
      z
    })
  } else {
    rows <- lapply(cl, .kidpr_hint_clean, caps = nt$caps)
  }
  fields <- setdiff(names(.kidpr_row()), "single")
  lists <- c("features", "composition", "masters", "overlay", "notes", "hint", "hint_features")
  out <- lapply(fields, function(f) {
    v <- lapply(rows, `[[`, f)
    if (f %in% lists) lapply(v, as.character)
    else if (f == "lith") vapply(v, function(z) as.integer(z %||% 1L)[1L], 1L)
    else if (is.logical(.kidpr_row()[[f]])) vapply(v, function(z) isTRUE(z), NA)
    else vapply(v, function(z) as.character(z %||% NA_character_)[1L], "")
  })
  names(out) <- fields
  out$kidpr <- nt$ok
  out$caps <- nt$caps
  out$why <- nt$why
  out$any_known <- any(out$known)
  out
}
