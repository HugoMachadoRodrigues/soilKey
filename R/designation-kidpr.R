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
# helpers read each designation.
# =============================================================================

# site$designation_system values that declare the notation.
.KIDPR_DECLARED <- c("kidpr", "kidpr2008", "ru", "ru2004", "ru2008", "russian",
                     "russia", "rfg2008", "rsc2004")

# Masters only the Russian notation writes: one of them recognises a profile.
# BT, BI and BM do not, since other notations write them in capitals too.
.KIDPR_ONLY <- c("AY", "AJ", "AU", "AO", "AH", "AK", "AEL", "BEL", "SEL", "EL",
                 "BHF", "BFM", "BMK", "BPL", "BAN", "BCA", "CAT", "ASN", "BSN",
                 "CRM", "CRH", "TUR", "PU", "PB", "PC", "PT", "PTR", "RU", "RY",
                 "RJ", "TO", "TE", "TJ", "TT", "ML", "SS", "CLM")

# In an all-capitals profile (AP, BTG: lower-case letters written as
# capitals) only the masters no FAO designation can spell once upper-cased
# recognise the notation: those starting with a letter that is no FAO master
# (P, S, T, M). AY, AU, EL, BEL, BCA ... could be an Ay, Au, El, BEl, BCa.
.KIDPR_ONLY_CAPS <- c("SEL", "TUR", "PU", "PB", "PC", "PT", "PTR", "TO", "TE",
                      "TJ", "TT", "ML", "SS")

# Horizons the key identifies and then looks past for a second horizon in the
# same layer (positions 3, 4 and 23): written after a "+" ("AU+S").
.KIDPR_OVERLAYS <- c("S", "X", "V")

# Transitions written with the generic letters (AB, BC): not diagnostic.
.KIDPR_GENERIC_TRANSITIONS <- c("AB", "BA", "BC", "CB", "AC")

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
    ambiguous = code(Filter(function(e) isTRUE(e$ambiguous), f$features)),
    flag_masters = c("C", "D"),
    rock_flags = vapply(h$rock_flags, function(e) as.character(e$code), "")
  )
  voc$masters <- unique(c(hz, names(voc$modifications), names(voc$master_aliases),
                          rocks, legacy))
  # the features the parser accepts: every code written in lower case or as a
  # symbol, the bracketed ones, their aliases and the parts of [e-hf]
  voc$features <- unique(c(code(lower), names(falias), voc$components))
  voc$ambiguous <- unique(c(voc$ambiguous, names(falias)[falias %in% voc$ambiguous]))
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


# --- normalisation -------------------------------------------------------------

# One designation as the reader sees it. Zero-width characters and combining
# marks are dropped (no index carries an accent; base R has no NFC); a
# numeral written apart is joined to its master ("II BT"); spaces inside
# brackets are dropped ("[AY, EL]tr"); a note after a space, or after a slash
# followed by a word ("BT/glossic"), and a trailing "?" are dropped; then the
# Cyrillic look-alikes are folded to Latin letters, curly quotes and primes to
# a straight quote, the Symbol-font perpendicular to U+22A5 and dashes to a
# hyphen (.formula_normalise()); the Cyrillic TPO (technogenic surface
# formations) becomes TPO.
.kidpr_normalise <- function(d) {
  d <- enc2utf8(as.character(d))
  vapply(d, function(s) {
    if (is.na(s)) return(NA_character_)
    s <- gsub("[\u200b-\u200d\ufeff\u0300-\u036f]", "", s, perl = TRUE)
    s <- gsub("[[:space:]\u00a0]+", " ", s, perl = TRUE)
    s <- trimws(s)
    if (!nzchar(s)) return(NA_character_)
    s <- sub("^([IVX]+|[0-9]+) +(?=[A-Z\u0410-\u042f(\\[])", "\\1", s, perl = TRUE)
    # spaces inside brackets
    repeat {
      s2 <- gsub("([\\[(][^\\])]*?) +", "\\1", s, perl = TRUE)
      if (identical(s2, s)) break
      s <- s2
    }
    s <- sub(" .*$", "", s)
    s <- sub("/[a-z]{3,}.*$", "", s)
    s <- sub("\\?+$", "", s)
    s <- .formula_normalise(s)
    s <- gsub("T\u041fO", "TPO", s, fixed = TRUE)
    if (nzchar(s)) s else NA_character_
  }, "", USE.NAMES = FALSE)
}


# --- one designation ---------------------------------------------------------

# The fields of one row, at their defaults.
.kidpr_row <- function(written = NA_character_) {
  list(written = written, index = NA_character_, modification = NA_character_,
       group = NA_character_, agro = FALSE, features = character(0),
       small = FALSE, bracket = NA_character_, composition = character(0),
       remnant = FALSE, buried = FALSE, lith = 1L, subdiv = NA_character_,
       rockflag = NA_character_, mark = NA_character_, transitional = FALSE,
       masters = character(0), overlay = character(0), legacy = FALSE,
       known = FALSE, ambiguous = NA_character_, notes = character(0),
       hint = character(0), hint_features = character(0), suspect = FALSE)
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

# Features written after an index, read as .formula_form() reads them; the
# index is replaced by a stand-in master.
.kidpr_tail_form <- function(tail, voc) {
  s <- paste0("X", tail)
  tryCatch(.formula_form(s, .kidpr_engine_voc(voc, s))[[1L]], error = function(e) e)
}

# Fill a row from a form of .formula_form(): index, lith prefix, sub-horizon,
# rock flag, mark, features (written and in parentheses), composition.
.kidpr_from_form <- function(r, f, voc) {
  r$index <- f$code
  r$rockflag <- f$rockflag
  r$mark <- f$mark
  r$subdiv <- f$subdiv
  r$features <- unique(c(f$features, f$typical))
  r$composition <- f$composition
  r$known <- TRUE
  r
}

# The features of a row in canonical form (tu -> tr, ko -> ek, ag -> agr,
# ael -> ae, as -> cs, hh -> [hh], akl -> [akl]), with a note per alias, and
# the ambiguity note for a two-letter feature written without commas.
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

# One designation with no "+" (overlay) or "/" (transition) outside brackets.
.kidpr_read_one <- function(s, voc, caps) {
  r <- .kidpr_row(s)
  if (is.na(s) || !nzchar(s)) return(r)
  ev <- .kidpr_engine_voc(voc, s)
  ch <- strsplit(s, "", fixed = TRUE)[[1L]]
  # a remnant: the whole designation in parentheses
  if (ch[1L] == "(" && tryCatch(.formula_close(ch, 1L, s) == length(ch), error = function(e) FALSE)) {
    r <- .kidpr_read_one(substr(s, 2L, nchar(s) - 1L), voc, caps)
    r$written <- s
    r$remnant <- TRUE
    return(r)
  }
  first <- ch[1L]
  if (first %in% c("[", "{", letters, .FORMULA_SYMBOLS)) {
    # a bracket, a buried profile, a small horizon or a turbated mixture
    sl <- tryCatch(.formula_slot(s, 1L, ev), error = function(e) e)
    if (inherits(sl, "error")) {
      r$notes <- conditionMessage(sl)
      return(r)
    }
    r$known <- TRUE
    if (sl$kind == "small") {
      r$small <- TRUE
      r$features <- sl$small
      if (first == "[") r$bracket <- s
    } else if (sl$kind == "buried") {
      r$buried <- TRUE
      r$notes <- "a buried profile"
    } else if (sl$kind == "mixture") {
      if (length(sl$composition) == 1L && !length(sl$small)) {
        # one horizon in brackets: a buried horizon
        r$index <- sl$composition
        r$buried <- TRUE
        r$notes <- "a buried horizon (written in brackets)"
      } else {
        r$composition <- sl$composition
        r$features <- sl$small
        r$bracket <- s
        r$notes <- sprintf("a turbated mixture of %s", paste(sl$composition, collapse = ", "))
      }
    }
    # a bracketed feature is one feature whatever its letters
    r <- .kidpr_canonical_features(r, voc, commas = first == "[" || grepl(",", s, fixed = TRUE))
    return(.kidpr_canonical_index(r, voc))
  }
  lp <- .kidpr_lith(s)
  r$lith <- lp$lith
  body <- lp$rest
  # a bracketed feature written on its host (AJ[akl], EL[e-hf])
  bf <- regmatches(body, gregexpr("\\[[a-z][a-z-]*\\]", body))[[1L]]
  if (length(bf) && all(bf %in% voc$features)) {
    body <- gsub("\\[[a-z][a-z-]*\\]", "", body)
    r <- .kidpr_read_one(body, voc, caps)
    r$written <- s
    r$lith <- lp$lith
    r$features <- unique(c(r$features, bf))
    return(r)
  }
  mm <- .kidpr_master(body, voc)
  if (is.na(mm$master)) {
    if (mm$run %in% .KIDPR_GENERIC_TRANSITIONS) {
      tf <- .kidpr_tail_form(mm$tail, voc)
      if (!inherits(tf, "error")) {
        r$transitional <- TRUE
        r$masters <- strsplit(mm$run, "", fixed = TRUE)[[1L]]
        r$features <- unique(c(tf$features, tf$typical))
        r$subdiv <- tf$subdiv
        r$known <- TRUE
        r$notes <- sprintf("a transition (%s), not diagnostic", mm$run)
        return(.kidpr_canonical_features(r, voc, grepl(",", s, fixed = TRUE)))
      }
    }
    # two capitals or more that make no index: a sign of capitals written
    # for lower-case letters (AP, BK)
    r$suspect <- nchar(mm$run) >= 2L
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
    # all capitals: the capitals after the index read as its features
    tl <- paste0(after, mm$tail)
    tl <- gsub("([A-Z]+)(?![^\\[]*\\])", "\\L\\1", tl, perl = TRUE)
    s2 <- paste0(mm$master, flag, tl)
    f2 <- tryCatch(.formula_form(s2, .kidpr_engine_voc(voc, s2)), error = function(e) e)
    if (caps) {
      if (!inherits(f2, "error")) {
        r <- .kidpr_from_form(r, f2[[1L]], voc)
        r$notes <- sprintf("all-capitals notation: %s read as %s", s, s2)
        r <- .kidpr_canonical_features(r, voc, grepl(",", s, fixed = TRUE))
        return(.kidpr_canonical_index(r, voc))
      }
    }
    # two indices run together: a transition (AUBM, BMC), not diagnostic
    m2 <- .kidpr_master(after, voc)
    if (!is.na(m2$master) && !nzchar(m2$after)) {
      tf <- .kidpr_tail_form(mm$tail, voc)
      if (!inherits(tf, "error")) {
        r$transitional <- TRUE
        r$masters <- c(mm$master, m2$master)
        r$features <- unique(c(tf$features, tf$typical))
        r$subdiv <- tf$subdiv
        r$known <- TRUE
        # BTG: a transition, or BT + g written in capitals
        r$suspect <- nchar(m2$master) == 1L && !inherits(f2, "error")
        r$notes <- sprintf("a transition (%s, %s), not diagnostic", mm$master, m2$master)
        return(.kidpr_canonical_features(r, voc, grepl(",", s, fixed = TRUE)))
      }
    }
    r$suspect <- TRUE
    r$notes <- sprintf("'%s': capitals after the index %s that are no index", s, mm$master)
    return(r)
  }
  fm <- tryCatch(.formula_form(body, .kidpr_engine_voc(voc, body)), error = function(e) e)
  if (inherits(fm, "error")) {
    r$suspect <- TRUE
    r$notes <- conditionMessage(fm)
    return(r)
  }
  r <- .kidpr_from_form(r, fm[[1L]], voc)
  if (length(fm) > 1L) {
    r$notes <- c(r$notes, sprintf("alternatives written: %s",
                                  paste(vapply(fm[-1L], function(f) f$code, ""), collapse = ", ")))
  }
  r <- .kidpr_canonical_features(r, voc, grepl(",", s, fixed = TRUE))
  .kidpr_canonical_index(r, voc)
}

#' Read one designation in the Russian notation of 2008
#'
#' @param d A designation as recorded.
#' @param caps TRUE when the profile is written all in capitals (lower-case
#'   letters written as capitals): the capitals after an index are then read
#'   as its features (BTG = BT + g).
#' @return A list: \code{written} (the designation once normalised),
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
  s <- .kidpr_normalise(d)
  folded <- !is.na(s) && !is.na(d) && grepl("[\u0400-\u04ff]", d, perl = TRUE)
  if (is.na(s)) return(.kidpr_row())
  if (s %in% names(voc$rewrites)) {
    r <- .kidpr_parse_one(voc$rewrites[[s]], caps = caps)
    r$written <- s
    r$notes <- c(sprintf("%s read as %s (chapter 4 list)", s, voc$rewrites[[s]]), r$notes)
    return(r)
  }
  parts <- .kidpr_split_outside(s, "+")
  if (length(parts) > 1L) {
    # a horizon and an overlay in one layer (Hugo's default 10, 2026-11-02)
    rows <- lapply(parts, .kidpr_parse_one, caps = caps)
    ix <- vapply(rows, function(z) z$index %||% NA_character_, "")
    ov <- ix %in% voc$overlays
    host <- which(!ov & !is.na(ix))[1L]
    if (is.na(host)) host <- 1L
    r <- rows[[host]]
    r$written <- s
    r$overlay <- unique(ix[setdiff(which(ov), host)])
    r$known <- all(vapply(rows, function(z) z$known, NA)) && length(r$overlay) == length(parts) - 1L
    r$notes <- c(r$notes, unlist(lapply(rows[-host], function(z) z$notes)))
    if (!r$known) r$notes <- c(r$notes, "only S, X or V can be written over another horizon")
    return(r)
  }
  parts <- .kidpr_split_outside(s, "/")
  if (length(parts) > 1L) {
    rows <- lapply(parts, function(p) .kidpr_read_one(p, voc, caps))
    ix <- unlist(lapply(rows, function(z) if (z$transitional) z$masters else z$index))
    r <- .kidpr_row(s)
    if (length(ix) >= 2L && !anyNA(ix) && all(vapply(rows, function(z) z$known, NA))) {
      r$transitional <- TRUE
      r$masters <- ix
      r$known <- TRUE
      r$lith <- rows[[1L]]$lith
      r$features <- unique(unlist(lapply(rows, function(z) z$features)))
      r$notes <- sprintf("a transition (%s), not diagnostic", paste(ix, collapse = ", "))
    } else {
      r$suspect <- TRUE
      r$notes <- sprintf("'%s' is no transition of two indices", s)
    }
    return(r)
  }
  r <- .kidpr_read_one(s, voc, caps)
  if (folded) r$notes <- c("Cyrillic letters read as the Latin ones they look like", r$notes)
  r
}


# --- the notation of a profile -------------------------------------------------

# The master that starts a designation (outer parentheses, a lithological
# prefix and an overlay or transition after it aside), or NA.
.kidpr_first_master <- function(s, voc) {
  if (is.na(s)) return(NA_character_)
  s <- sub("^\\((.*)\\)$", "\\1", s)
  s <- .kidpr_split_outside(s, "+")[1L]
  s <- .kidpr_split_outside(s, "/")[1L]
  .kidpr_master(.kidpr_lith(s)$rest, voc)$master
}

#' Is a profile written in the Russian notation of 2008?
#'
#' In order: site$designation_system decides when present (the Russian
#' aliases, or another notation); otherwise the profile is Russian when one
#' designation starts with a master only the Russian notation writes, when a
#' horizon carries a comma-separated feature list (BCAs,cs) or a bracketed
#' feature ([hh], [e-hf], [akl], TUR[...], [AY, EL]tr). The country is never
#' read. A profile written all in capitals (no lower-case letter anywhere and
#' a designation that only reads with its capitals as features, such as AP or
#' BTG) is Russian only by a master an upper-cased FAO designation cannot
#' spell; its capitals after an index are then read as features.
#' @return A list: \code{ok}, \code{why}, \code{caps}.
#' @noRd
.kidpr_notation <- function(desig, site) {
  voc <- .kidpr_vocabulary()
  s <- .kidpr_normalise(desig)
  s <- s[!is.na(s)]
  rows <- lapply(s, function(z) .kidpr_parse_one(z, caps = FALSE))
  caps <- length(s) > 0L && !any(grepl("[a-z]", s)) &&
    any(vapply(rows, function(z) isTRUE(z$suspect), NA))
  declared <- tolower(trimws(as.character((site %||% list())$designation_system %||% NA)[1L]))
  if (!is.na(declared) && nzchar(declared) && declared != "na") {
    return(list(ok = declared %in% .KIDPR_DECLARED, caps = caps,
                why = sprintf("site$designation_system = '%s'", declared)))
  }
  if (!length(s)) return(list(ok = FALSE, caps = FALSE, why = "no designation"))
  masters <- vapply(s, .kidpr_first_master, "", voc = voc, USE.NAMES = FALSE)
  only <- if (caps) .KIDPR_ONLY_CAPS else .KIDPR_ONLY
  hit <- unique(masters[masters %in% only])
  if (length(hit)) {
    return(list(ok = TRUE, caps = caps,
                why = sprintf("%s: written only in the Russian notation%s", paste(hit, collapse = ", "),
                              if (caps) " (an all-capitals profile)" else "")))
  }
  if (!caps) {
    comma <- grepl("[a-z@\u22a5],[a-z@\u22a5]", s, perl = TRUE)
    brack <- grepl("\\[(hh|e-hf|akl)\\]|TUR\\[|\\[[A-Z]+(,[A-Z]+)+\\][a-z]", s, perl = TRUE)
    if (any(comma | brack)) {
      return(list(ok = TRUE, caps = FALSE,
                  why = sprintf("%s: features written as the Russian notation writes them",
                                paste(unique(s[comma | brack]), collapse = ", "))))
    }
  }
  list(ok = FALSE, caps = caps,
       why = if (caps) "an all-capitals profile with no master an FAO designation cannot spell"
             else "no index only the Russian notation writes (the country is not read)")
}


# --- hints from other notations --------------------------------------------------

# Codes of a group of horizons, for the hints.
.KIDPR_HINT_GROUPS <- list(
  humus = c("AY", "AJ", "AU", "AH", "AK", "AO", "W"),
  eluvial = c("E", "EL", "AEL", "BEL", "SEL"),
  agro = c("P", "PU", "PB", "PT", "PTR"),
  organic = c("T", "H", "O")
)

#' What an FAO (2006) or Soviet (1977) designation suggests to the key on the
#' data: candidate Russian horizons and features, never a horizon (Hugo's
#' decision Q3, 2026-11-02). Ap: agro; Ah or A: humus; E: eluvial; Bt: BT or
#' BI; Bw: BM, BFM or BMK; Bs, Bhs, Bh: BHF; Bk: BCA or CAT; Btn, Bn: ASN or
#' BSN; Bss (FAO i): V; r: G; g: the feature g; z: S; Oi, L: litter; Oe, Oa,
#' H: T, H or O by the decomposition; @: @; f: permafrost. The Soviet A0 is the
#' litter or O, A1 the humus horizon, A2 the eluvial one, A1A2 AEL, Ap or
#' Apakh (Cyrillic) the plough layer, Bca BCA or CAT.
#' @noRd
.kidpr_hint <- function(d, caps = FALSE) {
  r <- .kidpr_row(d)
  s <- .kidpr_normalise(d)
  if (is.na(s)) return(r)
  r$written <- s
  # in an all-capitals profile the letters after the master are suffixes
  if (caps) s <- sub("^([0-9]*[A-Z])(.*)$", "\\1\\L\\2", s, perl = TRUE)
  hint <- function(codes, features = character(0), note) {
    r$hint <<- unique(c(r$hint, codes))
    r$hint_features <<- unique(c(r$hint_features, features))
    r$notes <<- c(r$notes, note)
  }
  # Soviet 1977 forms
  if (grepl("^A(00|oo)$", s)) {
    hint(character(0), note = "Soviet A00: fresh litter, not diagnostic"); return(r)
  }
  if (grepl("^A[0o]$", s)) {
    hint("O", note = "Soviet A0: litter or the litter-peat O"); return(r)
  }
  if (grepl("^A[\u043f\u041f]", s)) {
    hint(.KIDPR_HINT_GROUPS$agro, note = "Soviet plough layer: an agro horizon"); return(r)
  }
  if (grepl("^A1A2", s)) {
    hint(c("AEL", .KIDPR_HINT_GROUPS$eluvial), note = "Soviet A1A2: humus-eluvial"); return(r)
  }
  if (grepl("^A1([^0-9A-Z]|$)", s)) {
    hint(.KIDPR_HINT_GROUPS$humus, note = "Soviet A1: a humus horizon"); return(r)
  }
  if (grepl("^A2([^0-9A-Z]|$)", s)) {
    hint(.KIDPR_HINT_GROUPS$eluvial, note = "Soviet A2: an eluvial horizon"); return(r)
  }
  p <- .parse_designation_hscs1(s)
  if (is.null(p)) {
    r$notes <- sprintf("'%s': no hint", s)
    return(r)
  }
  m <- p$master
  l <- unique(c(p$strong, p$weak))
  low <- gsub("[^a-z]", "", s)
  if (grepl("ca", low) && m %in% c("B", "C")) l <- unique(c(l, "k"))
  if (m == "L" || (m == "O" && "i" %in% l)) {
    hint(character(0), note = sprintf("%s: litter, not diagnostic", s))
  } else if (m == "H" || (m == "O" && any(c("e", "a") %in% l))) {
    hint(.KIDPR_HINT_GROUPS$organic, note = sprintf("%s: T, H or O by the decomposition", s))
  } else if (m == "O") {
    hint(.KIDPR_HINT_GROUPS$organic, note = sprintf("%s: an organic horizon", s))
  } else if (m == "A") {
    if ("p" %in% l) hint(.KIDPR_HINT_GROUPS$agro, note = sprintf("%s: an agro horizon", s))
    else hint(.KIDPR_HINT_GROUPS$humus, note = sprintf("%s: a humus horizon", s))
  } else if (m == "E") {
    hint(.KIDPR_HINT_GROUPS$eluvial, note = sprintf("%s: an eluvial horizon", s))
  } else if (m == "B") {
    if ("n" %in% l) hint(c("ASN", "BSN"), note = sprintf("%s: a solonetz horizon", s))
    else if ("t" %in% l) hint(c("BT", "BI"), note = sprintf("%s: clay illuviation", s))
    if (grepl("ss", low) || "i" %in% l) hint("V", note = sprintf("%s: slickensides", s))
    if (any(c("s", "h") %in% l)) hint("BHF", note = sprintf("%s: an illuvial Al-Fe-humus horizon", s))
    if ("k" %in% l) hint(c("BCA", "CAT"), note = sprintf("%s: carbonates", s))
    if ("w" %in% l) hint(c("BM", "BFM", "BMK"), note = sprintf("%s: a structural or colour B", s))
    if (!length(r$hint)) r$notes <- c(r$notes, sprintf("%s: a middle horizon", s))
  } else if (m %in% c("C", "R")) {
    r$notes <- c(r$notes, sprintf("%s: parent rock", s))
  }
  if ("r" %in% l && m %in% c("B", "C")) hint("G", note = sprintf("%s: strong reduction", s))
  if ("g" %in% l) hint(character(0), "g", sprintf("%s: stagnic or gleyic mottling", s))
  if ("z" %in% l && m %in% c("A", "B")) hint("S", note = sprintf("%s: salts", s))
  if (grepl("@", s, fixed = TRUE)) hint(character(0), "@", sprintf("%s: cryoturbation", s))
  if ("f" %in% l && m %in% c("A", "B", "C")) {
    hint(character(0), "\u22a5", sprintf("%s: frozen", s))
  }
  r
}


# --- a whole profile -------------------------------------------------------------

#' All the designations of a profile, read in the Russian notation
#'
#' The profile's notation is decided first (.kidpr_notation()); in the
#' Russian notation every designation is read (.kidpr_parse_one()), and a row
#' it cannot read gets the hints of the FAO letters; in another notation no
#' letter is read as a Russian horizon and every row gets only hints.
#' @return A list of vectors, one element per designation (as
#'   .cpcs_designations()): the fields of .kidpr_parse_one() (list columns for
#'   features, composition, masters, overlay, notes, hint and hint_features),
#'   then \code{kidpr} (the profile is in the Russian notation), \code{caps}
#'   (written all in capitals), \code{why} and \code{any_known}.
#' @noRd
.kidpr_designations <- function(desig, site) {
  desig <- as.character(desig)
  nt <- .kidpr_notation(desig, site)
  rows <- if (nt$ok) lapply(desig, .kidpr_parse_one, caps = nt$caps)
          else lapply(desig, .kidpr_hint, caps = nt$caps)
  if (nt$ok) {
    rows <- lapply(seq_along(rows), function(i) {
      z <- rows[[i]]
      if (!z$known && !is.na(z$written)) {
        h <- .kidpr_hint(desig[i], caps = nt$caps)
        z$hint <- unique(c(z$hint, h$hint))
        z$hint_features <- h$hint_features
        z$notes <- c(z$notes, h$notes)
      }
      z
    })
  }
  fields <- setdiff(names(.kidpr_row()), "suspect")
  lists <- c("features", "composition", "masters", "overlay", "notes", "hint", "hint_features")
  out <- lapply(fields, function(f) {
    v <- lapply(rows, `[[`, f)
    if (f %in% lists) lapply(v, as.character)
    else if (f == "lith") vapply(v, function(z) as.integer(z), 1L)
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
