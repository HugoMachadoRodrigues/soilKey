# =============================================================================
# v0.9.355 -- the names of the diagnostic horizons and materials of the South
# African soil classification of 1991 (Soil Classification Working Group,
# 1991; the "Blue Book"), as surveys write them.
#
# South African profile descriptions name the diagnostic horizon beside the
# master horizon ("orthic A", "red apedal B", "soft plinthic B") and give the
# form and family as a code (Hu1200). The reader (Hugo's decision Q5,
# 2026-11-04):
#
#   * the horizon attribute diagnostic_za holds the name. It is read against
#     the controlled list of inst/rules/za1991/horizons.yaml: the 30 printed
#     names (5 topsoil horizons, 25 subsoil horizons and materials), their
#     variants and soilKey's tokens (OA, RAB ...), E and G alone, case,
#     spaces and hyphens aside. A name of the first edition (1977) is read as
#     its 1991 equivalent, flagged "1977 name" (ferrihumic B is the podzol B;
#     a gleycutanic B is the G horizon under an A or E and the unspecified
#     material with signs of wetness under a B). A negated name (not, no,
#     without, absent) is not read; a non-diagnostic one records no
#     diagnostic horizon; a name written with doubt (?, possibly, probably)
#     stays a candidate;
#   * the same names written inside designation ("Ap orthic A", "B1 red
#     apedal") are read too, when site$designation_system declares the
#     South African notation (za, za1991, sat, sat1991, south africa ...),
#     when the profile records a name in diagnostic_za, or when a designation
#     holds a name only this system writes (orthic A, red apedal, neocutanic,
#     dorbank ...). The country is never read: a South African site may be
#     described in the FAO notation;
#   * the FAO and USDA letters of a designation are hints only (Bt: a
#     pedocutanic, prismacutanic, red structured or neocutanic B), never the
#     name; the identification on the data decides among them.
#
# The written name gives the horizon, the recorded data its properties. The
# identification on the data comes later. Nothing here stops with an error: a
# name that cannot be read is returned unread, with a note.
# =============================================================================

# Values of site$designation_system that declare the notation, compared once
# lower-cased with everything but letters and digits dropped; and the prefix
# "southafrica" (South Africa, south_africa, South African 1991).
.ZA_DECLARED <- c("za", "za1991", "sat", "sat1991")
.ZA_DECLARED_PREFIX <- "^southafrica"

# Values of site$designation_system that declare nothing; SA and RSA (the
# country, not a notation) among them (Hugo, 2026-11-04).
.ZA_UNDECLARED <- c("na", "none", "unknown", "nd", "null", "missing", "notrecorded", "sa", "rsa")

# Declared notations whose letters are the FAO or USDA ones: their
# designations still give hints.
.ZA_FAO_NOTATIONS <- "^(fao|usda|wrb|ncss|nasis|soiltaxonomy)"

# Words of a negation and of doubt in diagnostic_za.
.ZA_NEGATION <- "(^| )(not|no|non|without|absent|none|nil)( |$)"
.ZA_DOUBT <- "(^| )(possibly|probably|perhaps|maybe|questionable|uncertain|doubtful)( |$)"

# Words of Afrikaans names, for the note of a name left unread.
.ZA_AFRIKAANS <- "(iese|horison|apedale|(^| )rooi( |$)|geelbruin|sagte|harde|grondneerslag|saproliet|rots( |$))"

# Words an FAO-letter parse would take for a designation (C + lay, R + ock).
.ZA_NOT_DESIGNATIONS <- c("clay", "rock", "gley", "grey", "gray", "hard", "ash", "bed", "bog",
                          "coal", "red", "ore", "hue", "and", "all", "age", "oak", "bar", "bay",
                          "cob", "rib", "rid", "gap", "cut", "hum", "car", "cay", "ahs")

.za_cache <- new.env(parent = emptyenv())

# The catalogue of inst/rules/za1991/horizons.yaml.
.za_catalogue <- function() {
  if (is.null(.za_cache$catalogue)) {
    path <- system.file("rules", "za1991", "horizons.yaml", package = "soilKey")
    if (!nzchar(path)) rlang::abort("The catalogue of the South African horizons is not installed.")
    .za_cache$catalogue <- yaml::read_yaml(path)
  }
  .za_cache$catalogue
}


# --- text to a key ------------------------------------------------------------

# One piece of text as valid UTF-8 (a Windows-1252 or Latin-1 file read as
# such), or NA.
.za_utf8 <- function(x) {
  if (length(x) != 1L || is.na(x)) return(NA_character_)
  x <- tryCatch(enc2utf8(as.character(x)), error = function(e) as.character(x))
  if (validUTF8(x)) return(x)
  y <- suppressWarnings(iconv(x, "CP1252", "UTF-8"))
  if (is.na(y) || !validUTF8(y)) y <- suppressWarnings(iconv(x, "latin1", "UTF-8"))
  if (is.na(y) || !validUTF8(y)) NA_character_ else y
}

# The key of a name: lower case, the accents folded, every run of characters
# that are not letters or digits one space (hyphens, dots, brackets ...).
# With drop_horizon, the word horizon is dropped (red apedal B horizon is
# red apedal B; E horizon is e).
.za_key <- function(x, drop_horizon = TRUE) {
  x <- .za_utf8(x)
  if (is.na(x)) return(NA_character_)
  x <- tolower(.rp_fold_accents(.rp_fold(x)))
  x <- trimws(gsub("[^a-z0-9]+", " ", x))
  if (drop_horizon) {
    x <- trimws(gsub("(^| )(horizons?|horison)( |$)", " ", x))
    x <- gsub(" +", " ", x)
  }
  x
}


# --- the vocabulary ---------------------------------------------------------------

# The keys the reader works with, built once from the catalogue: one row per
# written form, with its token, kind (name, variant, generic_variant, short,
# token, word, 1977) and detection (distinctive or generic); and the patterns
# searched inside a longer text (the written form with and without the word
# horizon, four letters or more, no short form or token).
.za_vocabulary <- function() {
  if (!is.null(.za_cache$vocabulary)) return(.za_cache$vocabulary)
  ct <- .za_catalogue()
  h <- ct$horizons
  fld <- function(l, f) vapply(l, function(e) as.character(e[[f]] %||% NA_character_)[1L], "")
  tokens <- fld(h, "token")
  info <- data.frame(token = tokens, name = fld(h, "name"), name_en = fld(h, "name_en"),
                     group = fld(h, "group"), kind = fld(h, "kind"), master = fld(h, "master"),
                     detection = fld(h, "detection"), in_1977 = fld(h, "in_1977"),
                     pages = fld(h, "pages"), stringsAsFactors = FALSE)
  rownames(info) <- tokens
  rows <- list()
  add <- function(written, token, kind, detection, idx = NA_integer_) {
    for (w in as.character(unlist(written))) {
      rows[[length(rows) + 1L]] <<- data.frame(written = w, key = .za_key(w),
                                               full = .za_key(w, drop_horizon = FALSE),
                                               token = token, kind = kind, detection = detection,
                                               idx = idx, stringsAsFactors = FALSE)
    }
  }
  for (e in h) {
    add(e$name, e$token, "name", e$detection)
    add(e$variants, e$token, "variant", e$detection)
    add(e$generic_variants, e$token, "generic_variant", "generic")
    add(e$short, e$token, "short", "generic")
    add(e$token, e$token, "token", "generic")
  }
  words <- ct$words
  for (i in seq_along(words)) {
    to <- as.character(unlist(words[[i]]$to))
    add(words[[i]]$written, if (length(to) == 1L) to else NA_character_, "word", "generic", i)
  }
  n77 <- ct$names_1977
  for (i in seq_along(n77)) {
    e <- n77[[i]]
    to <- as.character(unlist(e$to))
    # the name and the name without its final B (ferrihumic B, ferrihumic)
    add(unique(c(e$name, sub(" [Bb]$", "", e$name))),
        if (length(to) == 1L) to else NA_character_, "1977", "distinctive", i)
  }
  keys <- do.call(rbind, rows)
  # what a row reads as: its token, or the word or 1977 entry it stands for
  keys$reads <- ifelse(is.na(keys$token), paste0(keys$kind, ":", keys$idx), keys$token)
  keys <- keys[!duplicated(keys[c("key", "reads")]), , drop = FALSE]
  clash <- unique(keys$key[duplicated(keys$key)])
  if (length(clash)) {
    rlang::abort(sprintf("The South African catalogue reads %s as two horizons.",
                         paste(clash, collapse = ", ")))
  }
  rownames(keys) <- NULL
  # the patterns searched inside a text: each written form with and without
  # the word horizon
  srch <- keys[!(keys$kind %in% c("short", "token")), , drop = FALSE]
  pats <- rbind(transform(srch, pattern = srch$full), transform(srch, pattern = srch$key))
  pats <- pats[nchar(pats$pattern) >= 4L, , drop = FALSE]
  pats <- pats[!duplicated(pats[c("pattern", "reads")]), , drop = FALSE]
  clash <- unique(pats$pattern[duplicated(pats$pattern)])
  if (length(clash)) {
    rlang::abort(sprintf("The South African catalogue searches %s as two horizons.",
                         paste(clash, collapse = ", ")))
  }
  pats <- pats[order(-nchar(pats$pattern), pats$pattern), , drop = FALSE]
  rownames(pats) <- NULL
  voc <- list(info = info, keys = keys, patterns = pats, names_1977 = n77, words = words,
              no_diagnostic = vapply(ct$no_diagnostic, .za_key, ""),
              not_recorded = vapply(ct$not_recorded, .za_key, ""),
              fao_hints = ct$fao_hints)
  .za_cache$vocabulary <- voc
  voc
}

# The printed name of a token with the token in brackets: "Red apedal B [RAB]".
.za_label <- function(token, voc = .za_vocabulary()) {
  if (is.na(token)) return(NA_character_)
  if (!nzchar(token)) return("no diagnostic horizon")
  sprintf("%s [%s]", voc$info[token, "name"], token)
}


# --- site$designation_system ------------------------------------------------------

# The declared notation as a key ("South African 1991" -> southafrican1991),
# or NA when the site declares none (SA and RSA declare none). Never an
# error, whatever the site is.
.za_declared_key <- function(site) {
  if (!is.list(site)) return(NA_character_)
  v <- tryCatch(site[["designation_system"]], error = function(e) NULL)
  v <- tryCatch(as.character(unlist(v)), error = function(e) character(0))
  if (!length(v) || is.na(v[1L])) return(NA_character_)
  v <- .za_utf8(v[1L])
  if (is.na(v)) return(NA_character_)
  k <- gsub("[^a-z0-9]", "", .rp_fold_accents(tolower(v)))
  if (!nzchar(k) || k %in% .ZA_UNDECLARED || !grepl("[a-z]", k)) return(NA_character_)
  k
}

# Does a declared key name the South African notation?
.za_key_is_za <- function(key) {
  !is.na(key) && (key %in% .ZA_DECLARED || grepl(.ZA_DECLARED_PREFIX, key))
}

# Does a value of designation_system declare the South African notation?
.za_is_declared <- function(ds) {
  .za_key_is_za(.za_declared_key(list(designation_system = ds)))
}


# --- one name -----------------------------------------------------------------------

# The fields of one reading, at their defaults.
.za_reading <- function(written = NA_character_) {
  list(written = written, token = NA_character_, candidates = character(0),
       name = NA_character_, name_en = NA_character_, kind = NA_character_,
       detection = NA_character_, is_1977 = FALSE, name_1977 = NA_character_,
       by_position = NULL, recorded = FALSE, doubt = FALSE, notes = character(0))
}

# Fill a reading from a row of the vocabulary.
.za_fill <- function(r, row, voc) {
  r$kind <- row$kind
  r$detection <- row$detection
  if (row$kind == "1977") {
    e <- voc$names_1977[[row$idx]]
    r$is_1977 <- TRUE
    r$name_1977 <- e$name
    to <- as.character(unlist(e$to))
    if (length(to) == 1L) {
      r$token <- to
      r$notes <- c(r$notes, sprintf("1977 name: %s, read as the %s of 1991 (P)", e$name,
                                    voc$info[to, "name"]))
    } else {
      r$token <- NA_character_
      r$candidates <- to
      r$by_position <- e$by_position
      r$notes <- c(r$notes, sprintf("1977 name: %s, the %s by the horizon above (P)", e$name,
                                    paste(voc$info[to, "name"], collapse = " or the ")))
    }
  } else if (row$kind == "word") {
    to <- as.character(unlist(voc$words[[row$idx]]$to))
    if (length(to) == 1L) {
      r$token <- to
      r$notes <- c(r$notes, sprintf("'%s' read as the %s", row$written, voc$info[to, "name"]))
    } else {
      r$token <- NA_character_
      r$candidates <- to
      r$notes <- c(r$notes, sprintf("'%s': the %s, undecided between them (the data decide)",
                                    row$written, paste(voc$info[to, "name"], collapse = " or the ")))
    }
  } else {
    r$token <- row$token
    if (row$kind == "token") {
      r$notes <- c(r$notes, sprintf("'%s' read as soilKey's token of the %s", row$written,
                                    voc$info[row$token, "name"]))
    } else if (row$kind == "generic_variant") {
      r$notes <- c(r$notes, sprintf("'%s' read as the %s", row$written,
                                    voc$info[row$token, "name"]))
    }
  }
  if (!is.na(r$token)) {
    r$name <- voc$info[r$token, "name"]
    r$name_en <- voc$info[r$token, "name_en"]
  }
  r
}

# The names found inside a text, longest first and without overlap: a list
# of rows of the patterns, and the text left once they are taken out.
.za_find <- function(full, voc) {
  if (is.na(full) || !nzchar(full)) return(list(found = list(), rest = ""))
  s <- paste0(" ", full, " ")
  out <- list()
  k <- voc$patterns
  for (i in seq_len(nrow(k))) {
    pat <- paste0(" ", k$pattern[i], " ")
    if (grepl(pat, s, fixed = TRUE)) {
      out[[length(out) + 1L]] <- k[i, , drop = FALSE]
      s <- gsub(pat, " | ", s, fixed = TRUE)
    }
  }
  list(found = out, rest = trimws(gsub("[| ]+", " ", s)))
}

# Read the names found in a text into one reading: one name (or the same
# one written twice) gives it; two different ones leave it undecided.
.za_from_found <- function(r, found, voc) {
  if (length(unique(vapply(found, `[[`, "", "reads"))) == 1L) return(.za_fill(r, found[[1L]], voc))
  each <- lapply(found, function(f) .za_fill(.za_reading(r$written), f, voc))
  cand <- unique(unlist(lapply(each, function(z) c(z$token[!is.na(z$token)], z$candidates))))
  r$token <- NA_character_
  r$candidates <- cand
  r$kind <- "several"
  r$detection <- if (any(vapply(found, function(f) f$detection == "distinctive", NA)))
    "distinctive" else "generic"
  r$is_1977 <- any(vapply(each, `[[`, NA, "is_1977"))
  r$notes <- c(r$notes, unlist(lapply(each, `[[`, "notes")),
               sprintf("several names (%s): undecided between them",
                       paste(vapply(found, `[[`, "", "written"), collapse = ", ")))
  r
}

# A note when 'calcareous' is written with a name that is not a 1977
# calcareous B (red apedal B, calcareous; neocutanic B (calcareous)).
.za_calcareous_note <- function(r, key) {
  if (!r$is_1977 && !is.na(r$token) && nzchar(r$token) && r$token != "NCA" &&
      grepl("(^| )calcareous( |$)", key)) {
    r$notes <- c(r$notes, sprintf(paste("'calcareous' written with the %s: read as it; a",
                                        "calcareous apedal or neocutanic B is the neocarbonate",
                                        "B, which the data decide"), r$name))
  }
  r
}

#' Read one name of a South African diagnostic horizon or material
#'
#' @param x The text as recorded.
#' @param where \code{"diagnostic_za"} (the field of the name: the names,
#'   their variants, soilKey's tokens, E and G alone, a 1977 name; the words
#'   that record no diagnostic horizon; negations and doubt; text around a
#'   name) or \code{"designation"} (a designation that may hold a name: only
#'   the names and variants written inside it).
#' @return A list: \code{written}, \code{token} (the token, \code{""} for no
#'   diagnostic horizon, \code{NA} when not read or undecided),
#'   \code{candidates} (when undecided or doubtful), \code{name},
#'   \code{name_en}, \code{kind} (name, variant, generic_variant, short,
#'   token, word, 1977, several, none, negated, letter), \code{detection}
#'   (distinctive, generic), \code{is_1977}, \code{name_1977},
#'   \code{by_position} (a 1977 name decided by the horizon above),
#'   \code{recorded}, \code{doubt} and \code{notes}.
#' @noRd
.za_read_name <- function(x, where = c("diagnostic_za", "designation")) {
  where <- match.arg(where)
  voc <- .za_vocabulary()
  x <- tryCatch(as.character(unlist(x))[1L], error = function(e) NA_character_)
  r <- .za_reading(if (length(x)) x else NA_character_)
  if (!length(x) || is.na(x)) return(r)
  u <- .za_utf8(x)
  if (is.na(u)) {
    r$recorded <- TRUE
    r$notes <- "not readable text (invalid characters)"
    return(r)
  }
  if (!identical(u, x)) r$notes <- "read as Windows-1252 or Latin-1 text"
  key <- .za_key(u)
  full <- .za_key(u, drop_horizon = FALSE)
  if (!nzchar(key) || key %in% voc$not_recorded || grepl("^[?-]+$", trimws(u))) {
    if (nzchar(trimws(u))) r$notes <- c(r$notes, sprintf("'%s' records nothing", u))
    return(r)
  }
  r$recorded <- TRUE
  if (where == "designation") {
    fd <- .za_find(full, voc)
    if (!length(fd$found)) return(r)
    return(.za_calcareous_note(.za_from_found(r, fd$found, voc), key))
  }
  # doubt: ?, possibly, probably ... are taken out and the name kept as a
  # candidate
  doubt <- grepl("?", u, fixed = TRUE) || grepl(.ZA_DOUBT, key)
  if (doubt) {
    key <- trimws(gsub(" +", " ", gsub(.ZA_DOUBT, " ", key)))
    full <- trimws(gsub(" +", " ", gsub(.ZA_DOUBT, " ", full)))
    if (!nzchar(key)) {
      r$notes <- c(r$notes, sprintf("'%s' records nothing", u))
      r$recorded <- FALSE
      return(r)
    }
  }
  r <- .za_read_key(r, u, key, full, voc)
  r <- .za_calcareous_note(r, key)
  if (doubt) {
    r$doubt <- TRUE
    if (!is.na(r$token) && nzchar(r$token)) {
      r$candidates <- r$token
      r$token <- NA_character_
    }
    if (length(r$candidates) || identical(r$token, "")) {
      r$notes <- c(r$notes, sprintf("'%s' is written with doubt: a candidate, not the name", u))
    }
  }
  r
}

# The reading of a diagnostic_za key once doubt is taken out.
.za_read_key <- function(r, u, key, full, voc) {
  none <- function(r) {
    r$token <- ""
    r$kind <- "none"
    r$notes <- c(r$notes, sprintf("'%s': no diagnostic horizon", u))
    r
  }
  # the words of no diagnostic horizon (none, no subsoil, non-diagnostic
  # saprolite, saprolite (non-diagnostic) ...)
  if (key %in% voc$no_diagnostic || grepl("(^| )(non|not|no) ?diagnostic( |$)", key)) return(none(r))
  # the name as a whole, then the pieces of a text that holds two names, then
  # a name inside the text
  hit <- voc$keys[voc$keys$key == key, , drop = FALSE]
  if (nrow(hit)) return(.za_fill(r, hit[1L, , drop = FALSE], voc))
  parts <- trimws(strsplit(u, "[;|/+&]| or | and ", perl = TRUE)[[1L]])
  parts <- parts[nzchar(parts)]
  if (length(parts) > 1L) {
    pk <- vapply(parts, .za_key, "", USE.NAMES = FALSE)
    pk <- trimws(gsub(" +", " ", gsub(.ZA_DOUBT, " ", pk)))
    rows <- lapply(pk, function(k) voc$keys[voc$keys$key == k, , drop = FALSE][1L, , drop = FALSE])
    ok <- vapply(rows, function(z) !is.na(z$key), NA)
    if (all(ok)) return(.za_from_found(r, rows, voc))
  }
  fd <- .za_find(full, voc)
  negated <- grepl(.ZA_NEGATION, fd$rest)
  if (length(fd$found)) {
    if (negated) {
      r$kind <- "negated"
      r$notes <- c(r$notes, sprintf("'%s': a negated name (%s), not read", u,
                                    paste(vapply(fd$found, `[[`, "", "written"), collapse = ", ")))
      return(r)
    }
    r <- .za_from_found(r, fd$found, voc)
    r$notes <- c(r$notes, sprintf("'%s': the name read inside the text", u))
    return(r)
  }
  # none written with a remark (none (lithosol))
  if (grepl("^(none|nil)( |$)", key)) return(none(r))
  if (negated) {
    r$kind <- "negated"
    r$notes <- c(r$notes, sprintf("'%s': a negation, not read", u))
    return(r)
  }
  # a master letter alone: a hint, not the name
  p <- tryCatch(.za_letters(u), error = function(e) NULL)
  if (!is.null(p)) {
    h <- .za_hint(u)
    r$candidates <- h$hint
    r$kind <- "letter"
    r$notes <- c(r$notes, sprintf("'%s' is a master horizon letter, not a 1991 name%s", u,
                                  if (length(h$hint)) paste0(": ", paste(h$hint, collapse = ", ")) else ""))
    return(r)
  }
  r$notes <- c(r$notes, if (grepl(.ZA_AFRIKAANS, key))
    sprintf("'%s' looks Afrikaans: soilKey reads the English names only", u)
    else sprintf("'%s' is not a name of the 1991 diagnostic horizons", u))
  r
}


# --- hints of the FAO letters ------------------------------------------------------

# The master letter and the suffixes of a designation written in the FAO,
# USDA or the South African master notation (O, A, E, B, C, R and G; P): the
# first word, a lithological numeral before it dropped (2Bt, II C), ^ before
# the master (human-transported, ^C) kept as a suffix, the first master of a
# transition (AB, B/E). ss is one suffix (slickensides), as are jj, kk and
# ff. At most three suffixes and two digits in a run, and not an English
# word (Clay, Rock): NULL otherwise (Bleached, Calcareous, Hutton, Hu1200).
.za_letters <- function(s) {
  s <- .za_utf8(s)
  if (is.na(s)) return(NULL)
  w <- strsplit(trimws(.rp_fold(s)), "[[:space:]]+")[[1L]]
  if (!length(w) || !nzchar(w[1L])) return(NULL)
  w <- w[1L]
  caret <- startsWith(w, "^")
  w <- sub("^\\^", "", w)
  w <- sub("^([2-9]|I{1,3}|IV|V|VI{1,3}|IX|X)(?=\\^?[OHAEBCRG])", "", w, perl = TRUE)
  if (startsWith(w, "^")) {
    caret <- TRUE
    w <- substring(w, 2L)
  }
  if (tolower(gsub("[0-9]", "", w)) %in% .ZA_NOT_DESIGNATIONS) return(NULL)
  m <- regmatches(w, regexec(
    "^([OHAEBCRG])(/?[OHAEBCRG])?([0-9]{0,2})([a-z]*)(\\(([a-z]{1,3})\\))?([0-9]{0,2})$", w))[[1L]]
  if (!length(m)) return(NULL)
  suf <- paste0(m[5L], m[7L])
  units <- regmatches(suf, gregexpr("ss|jj|kk|ff|[a-z]", suf))[[1L]]
  if (length(units) > 3L) return(NULL)
  list(master = m[2L], letters = unique(c(units, if (caret) "^")))
}

#' What a designation's FAO or USDA letters suggest to the identification on
#' the data: candidate tokens, never the 1991 name (Hugo's decision Q5). The
#' rows of horizons.yaml (fao_hints) for the master letter whose suffixes are
#' all written, the most specific first; with none, the rows without
#' suffixes.
#' @return list(hint, hint_source, notes)
#' @noRd
.za_hint <- function(s) {
  out <- list(hint = character(0), hint_source = NA_character_, notes = character(0))
  p <- tryCatch(.za_letters(s), error = function(e) NULL)
  if (is.null(p)) return(out)
  rows <- Filter(function(r) identical(r$master, p$master), .za_vocabulary()$fao_hints)
  if (!length(rows)) return(out)
  w <- lapply(rows, function(r) as.character(unlist(r$with)))
  fit <- vapply(w, function(z) all(z %in% p$letters), NA)
  if (!any(fit)) return(out)
  size <- vapply(w, length, 1L)
  best <- fit & size == max(size[fit])
  tok <- unique(unlist(lapply(rows[best], function(r) as.character(unlist(r$tokens)))))
  out$hint <- tok
  out$hint_source <- "FAO letters"
  out$notes <- sprintf("%s: the letters suggest %s", s, paste(tok, collapse = ", "))
  out
}


# --- a whole profile -----------------------------------------------------------------

# The master letter of a designation as the 1991 master horizons read it, or
# NA.
.za_master_of <- function(d) {
  p <- tryCatch(.za_letters(d), error = function(e) NULL)
  if (is.null(p)) NA_character_ else p$master
}

# Does a reading name a horizon (a token, or candidates from names)?
.za_named <- function(z) {
  isTRUE(z$recorded) && ((!is.na(z$token) && nzchar(z$token)) ||
                           (length(z$candidates) > 0L && !identical(z$kind, "letter")))
}

#' The diagnostic horizons and materials of a profile, read from its
#' South African names
#'
#' diagnostic_za is read on every layer, whatever the notation. The names
#' written inside the designations are read when site$designation_system
#' declares the South African notation, when the profile records a name in
#' diagnostic_za, or when a designation holds a name only this system writes
#' (a distinctive name: orthic A, red apedal, neocutanic, dorbank ...); never
#' when the site declares another notation, never from the country. A layer
#' with a recorded name keeps it; its designation, read too, only adds a
#' note when it names another horizon. A 1977 gleycutanic B is the G horizon
#' under a topsoil or E and the unspecified material with signs of wetness
#' under an apedal, neocutanic or neocarbonate B, read on the nearest layer
#' above that holds a diagnostic horizon (its candidates or hints when it is
#' undecided); elsewhere it stays undecided between them. Every designation
#' also gets the hints of its FAO letters, unless the site declares a
#' notation whose letters are not the FAO or USDA ones.
#'
#' @param diagnostic_za The recorded names, one per layer (NULL when the
#'   attribute is absent).
#' @param designation The designations, one per layer.
#' @param site The site list (for designation_system).
#' @return A list of vectors, one element per layer: \code{token},
#'   \code{candidates} (list), \code{name}, \code{name_en}, \code{label}
#'   (the printed name with the token in brackets), \code{source}
#'   (diagnostic_za, designation or NA), \code{route} ("notation" when read),
#'   \code{written}, \code{designation}, \code{is_1977}, \code{doubt},
#'   \code{hint} (list), \code{hint_source}, \code{notes} (list); then
#'   \code{za} (TRUE when the designations are read as South African names),
#'   \code{status} (declared, diagnostic_za, recognised, other, none),
#'   \code{why} and \code{any_read}.
#' @noRd
.za_designations <- function(diagnostic_za = NULL, designation = NULL, site = NULL) {
  voc <- .za_vocabulary()
  as_chr <- function(v) tryCatch(as.character(unlist(v)), error = function(e) character(0))
  dz <- as_chr(diagnostic_za)
  ds <- as_chr(designation)
  n <- max(length(dz), length(ds))
  dz <- c(dz, rep(NA_character_, n - length(dz)))
  ds <- c(ds, rep(NA_character_, n - length(ds)))
  rz <- lapply(dz, function(x) tryCatch(.za_read_name(x, "diagnostic_za"), error = function(e)
    { r <- .za_reading(x); r$notes <- sprintf("not read: %s", conditionMessage(e)); r }))
  rd <- lapply(ds, function(x) tryCatch(.za_read_name(x, "designation"), error = function(e)
    .za_reading(x)))
  # the notation of the profile
  key <- .za_declared_key(site)
  any_dz <- any(vapply(rz, .za_named, NA))
  dist <- unique(unlist(lapply(rd, function(z)
    if (.za_named(z) && identical(z$detection, "distinctive")) z$written)))
  if (!is.na(key)) {
    raw <- as_chr(site[["designation_system"]])[1L]
    za <- .za_key_is_za(key)
    status <- if (za) "declared" else "other"
    why <- sprintf("site$designation_system = '%s'", raw)
  } else if (any_dz) {
    za <- TRUE
    status <- "diagnostic_za"
    why <- "the profile records a name in diagnostic_za"
  } else if (length(dist)) {
    za <- TRUE
    status <- "recognised"
    why <- sprintf("%s: a name only the South African system writes", paste(dist, collapse = ", "))
  } else {
    za <- FALSE
    status <- "none"
    why <- "no name only the South African system writes (the country is not read)"
  }
  hints_ok <- is.na(key) || za || grepl(.ZA_FAO_NOTATIONS, key)
  rows <- lapply(seq_len(n), function(i) {
    a <- rz[[i]]
    b <- rd[[i]]
    r <- a
    r$source <- NA_character_
    r$designation <- ds[i]
    named_b <- .za_named(b)
    r$route <- NA_character_
    if (a$recorded && (!is.na(a$token) || (length(a$candidates) && !identical(a$kind, "letter")))) {
      r$source <- "diagnostic_za"
      r$route <- "notation"
      if (named_b && za && !identical(b$token, a$token)) {
        r$notes <- c(r$notes, sprintf("the designation '%s' names the %s; diagnostic_za is kept", ds[i],
                                      if (is.na(b$token)) paste(b$candidates, collapse = " or ")
                                      else voc$info[b$token, "name"]))
      }
    } else if (named_b && za) {
      notes <- r$notes
      r <- b
      r$designation <- ds[i]
      r$source <- "designation"
      r$route <- "notation"
      r$notes <- c(notes, b$notes, sprintf("the name read in the designation '%s'", ds[i]))
    } else if (named_b) {
      r$notes <- c(r$notes, sprintf("'%s' holds a 1991 name, not read: %s", ds[i],
                                    if (status == "other") "the site declares another notation"
                                    else "the profile is not declared or recognised as South African"))
    }
    r$written <- dz[i]
    # the hints of the FAO letters
    h <- if (hints_ok) .za_hint(ds[i]) else .za_hint(NA_character_)
    r$hint <- h$hint
    r$hint_source <- h$hint_source
    if (identical(r$kind, "letter")) {
      # a master letter in diagnostic_za is a hint too, never the name
      r$hint <- unique(c(r$candidates, h$hint))
      r$hint_source <- "the master letter of diagnostic_za"
      r$candidates <- character(0)
    }
    if (is.na(r$token) && length(h$hint)) r$notes <- unique(c(r$notes, h$notes))
    r
  })
  # a 1977 name decided by the horizon above: the nearest layer above that
  # holds a diagnostic horizon (layers that hold none skipped); undecided or
  # unread, its candidates or hints decide when they all lead to one rule
  tok <- vapply(rows, function(z) z$token %||% NA_character_, "")
  for (i in seq_len(n)) {
    bp <- rows[[i]]$by_position
    if (is.null(bp)) next
    j <- if (i > 1L) rev(seq_len(i - 1L)) else integer(0)
    j <- j[is.na(tok[j]) | nzchar(tok[j])]
    up <- if (length(j)) rows[[j[1L]]] else NULL
    prev <- if (is.null(up)) character(0)
            else if (!is.na(tok[j[1L]])) tok[j[1L]]
            else if (length(up$candidates)) up$candidates
            else up$hint
    target <- unique(vapply(prev, function(p) {
      rule <- Filter(function(b) p %in% as.character(unlist(b$above)), bp)
      if (length(rule)) rule[[1L]]$to else NA_character_
    }, ""))
    where <- if (is.null(up)) "no horizon above it"
             else if (length(prev) == 1L) sprintf("under the %s", voc$info[prev, "name"])
             else if (length(prev)) sprintf("under a layer that may be the %s",
                                            paste(prev, collapse = ", "))
             else sprintf("under a layer not read (%s)", up$designation %||% "")
    if (length(target) == 1L && !is.na(target)) {
      rule <- Filter(function(b) identical(b$to, target), bp)[[1L]]
      rows[[i]]$token <- target
      rows[[i]]$candidates <- character(0)
      rows[[i]]$name <- voc$info[target, "name"]
      rows[[i]]$name_en <- voc$info[target, "name_en"]
      rows[[i]]$notes <- c(rows[[i]]$notes, sprintf("%s: %s", where, rule$note))
      tok[i] <- target
    } else {
      rows[[i]]$notes <- c(rows[[i]]$notes, sprintf("%s: undecided between %s", where,
                                                    paste(rows[[i]]$candidates, collapse = " and ")))
    }
  }
  # the master letter of the designation against the group of the name
  for (i in seq_len(n)) {
    t <- rows[[i]]$token
    if (is.na(t) || !nzchar(t) || is.na(rows[[i]]$source)) next
    m <- .za_master_of(ds[i])
    if (is.na(m)) next
    g <- voc$info[t, "group"]
    if ((g == "topsoil" && m %in% c("E", "B", "C", "R", "G")) ||
        (g == "subsoil" && m %in% c("A", "O", "H"))) {
      rows[[i]]$notes <- c(rows[[i]]$notes,
                           sprintf("the designation '%s' is a%s master horizon, the name a %s one; the name is kept",
                                   ds[i], if (m %in% c("A", "O", "H")) " topsoil" else " subsoil", g))
    }
  }
  chr <- function(f) vapply(rows, function(z) as.character(z[[f]] %||% NA_character_)[1L], "")
  lgl <- function(f) vapply(rows, function(z) isTRUE(z[[f]]), NA)
  lst <- function(f) lapply(rows, function(z) as.character(z[[f]]))
  token <- chr("token")
  list(
    token = token,
    candidates = lst("candidates"),
    name = chr("name"),
    name_en = chr("name_en"),
    label = vapply(token, .za_label, "", voc = voc, USE.NAMES = FALSE),
    source = chr("source"),
    route = chr("route"),
    written = dz,
    designation = ds,
    is_1977 = lgl("is_1977"),
    doubt = lgl("doubt"),
    hint = lst("hint"),
    hint_source = chr("hint_source"),
    notes = lapply(rows, function(z) unique(as.character(z$notes))),
    za = za,
    status = status,
    why = why,
    any_read = any(!is.na(token))
  )
}

#' The South African names of a pedon's layers: .za_designations() on its
#' horizons (diagnostic_za, designation) and site.
#' @noRd
.za_pedon_names <- function(pedon) {
  h <- tryCatch(pedon$horizons, error = function(e) NULL)
  site <- tryCatch(pedon$site, error = function(e) NULL)
  dz <- if (!is.null(h) && "diagnostic_za" %in% names(h)) h$diagnostic_za else NULL
  ds <- if (!is.null(h) && "designation" %in% names(h)) h$designation else NULL
  .za_designations(dz, ds, site)
}
