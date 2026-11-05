# =============================================================================
# v0.9.360 -- the horizon notation of the German soil systematics as the
# Bodenkundliche Kartieranleitung, 5th edition, writes it (Ad-hoc-AG Boden
# 2005, KA5: 5.6.3, pp. 81-108; Tab. 12; Liste 31, pp. 199-201), and the
# formula dialect of its soil types.
#
# A designation is a Roman numeral of a layer (II, III ...; I is never
# written, p. 108), then up to three parts joined by "-" (a transition), "+"
# (interfingering) or the degree sign (a horizon formed in a fossil or relict
# one: fAh<deg>Sd). A part is lower-case prefixes (geogenic or anthropogenic:
# lC, fAh, tmzeGo; tb, tm and tp are one prefix each), a capital master (F H L
# O A B C P T S G M E R Y), lower-case suffixes (pedogenic: Bhs, Gor), a
# base-saturation mark (>, <, <<) and sub-horizon digits. The last part
# carries the horizon, and in a part the last suffix carries it (p. 82): Ah-Bv
# is a Bv, Bv+Ah an Ah, fAh<deg>Sd an Sd in a buried Ah, Bsv a Bv with Bs
# features. A suffix letter means what its host master makes it mean (a, f,
# k, m, t, v, w; p. 85).
#
# The notation gives the horizons only when the profile is written in it
# (Hugo's decision Q3, 2026-11-04): declared in site$designation_system
# (ka5, ka4, ka3, kartieranleitung, bodensystematik, agboden, dbg; KA3 and
# KA4 read as KA5 with a note; ka6 read through the one-to-one KA6 symbols,
# Q4), or recognised by one strong sign or two likely ones of two kinds, with
# no symbol only another notation writes (FAO or USDA, the RP 2008, the
# Russian notation of 2008, the CPCS 1967); a profile with both is undecided.
# Never from the country. The RP, Russian, CPCS, CSSC, ASC and NZSC readers do
# not change.
#
# The formulas of Liste 31 and of the subtypes ("Ah/(Go/)Gso(/Gr)",
# "aAxh/(aM,alC/)aG", "Ah/M/II...") are read by .ka5_parse_formula() into the
# object of the sequence engine (soilkey_formula).
#
# The vocabulary is the catalogue inst/rules/ka5/horizons.yaml. Nothing here
# stops with an error: a designation that cannot be read is returned unread,
# with a note. ASCII only: the degree sign is written \u00b0.
# =============================================================================

.KA5_DEG <- "\u00b0"

# Values of site$designation_system that declare nothing.
.KA5_UNDECLARED <- c("na", "none", "unknown", "nd", "null", "missing", "notrecorded")

# Declared keys (once lower-cased, umlauts folded, all but letters and digits
# dropped) that name the KA5 notation, the older editions and the KA6: ka3 to
# ka5 (KA 5, ka5agboden2005, ka5auflage), the Kartieranleitung, the
# Bodensystematik, the AG Boden; dbg only alone or with a year (dbg-wrb is
# the WRB of the DBG, not the KA5). The KA6: ka6, or a name of the system
# followed by 6 or ka6 (Bodenkundliche Kartieranleitung 6, Bodensystematik
# (KA6)), unless the key starts with ka3 to ka5.
.KA5_DECLARED_RE <- "^(ka[345]|(bodenkundliche)?kartieranleitung|(deutsche)?bodensystematik|(adhoc)?agboden|dbg([0-9]{4})?$)"
.KA5_DECLARED_OLD_RE <- "^ka[34]"
.KA5_DECLARED_KA6_RE <- "^(ka6|((bodenkundliche)?kartieranleitung|(deutsche)?bodensystematik|(adhoc)?agboden)(ka)?6)"

# Designations that record nothing.
.KA5_MISSING <- c("NA", "na", "N/A", "n/a", "nd", "n.d.", "N.D.", "-", "--", "?", "??",
                  "...", "null", "NULL")

# Roman numerals of a layer, longest first (V and X alone are not read: V is
# a KA6 master).
.KA5_ROMAN <- c("VIII", "VII", "III", "IX", "VI", "IV", "II")

# The geogenic prefixes that, written in front of a designation of several
# parts, hold for the whole horizon (Hugo's decision on reading 1,
# 2026-11-04): floodplain, tidal, marl, saline, slope and spring water,
# organic. Not f and r, nor the rock prefixes c, i, j, l, m, x, y.
.KA5_GEOGENIC_FRONT <- c("a", "tb", "tm", "tp", "e", "z", "s", "q", "o")

.ka5_cache <- new.env(parent = emptyenv())

# The catalogue of inst/rules/ka5/horizons.yaml.
.ka5_catalogue <- function() {
  if (is.null(.ka5_cache$catalogue)) {
    path <- system.file("rules", "ka5", "horizons.yaml", package = "soilKey")
    if (!nzchar(path)) rlang::abort("The catalogue of the KA5 notation is not installed.")
    .ka5_cache$catalogue <- yaml::read_yaml(path)
  }
  .ka5_cache$catalogue
}

# The vocabulary the reader works with, built once from the catalogue.
.ka5_vocabulary <- function() {
  if (!is.null(.ka5_cache$vocabulary)) return(.ka5_cache$vocabulary)
  ct <- .ka5_catalogue()
  fld <- function(l, f) vapply(l, function(e) as.character(e[[f]] %||% NA_character_)[1L], "")
  masters <- fld(ct$masters, "code")
  # suffix letters each master takes, from the hosts of p. 85
  suf_letters <- stats::setNames(lapply(masters, function(m) character(0)), masters)
  for (s in ct$suffixes) {
    hosts <- if (!is.null(s$meanings)) unlist(lapply(s$meanings, function(z) z$hosts)) else s$hosts
    for (h in as.character(unlist(hosts))) suf_letters[[h]] <- unique(c(suf_letters[[h]], s$mark))
  }
  pre <- fld(ct$prefixes, "mark")
  pre_hosts <- stats::setNames(lapply(ct$prefixes, function(e) as.character(unlist(e$hosts))), pre)
  sy <- ct$symbols
  sym <- data.frame(code = fld(sy, "code"), master = fld(sy, "master"),
                    detection = fld(sy, "detection"), page = as.integer(fld(sy, "page")),
                    name = fld(sy, "name"), name_en = fld(sy, "name_en"),
                    stringsAsFactors = FALSE)
  named <- unique(unlist(lapply(ct$named, function(e) as.character(unlist(e$codes)))))
  comb <- lapply(ct$combinations, function(e) list(
    prefixes = as.character(unlist(e$prefixes)),
    prefixes_with_others = as.character(unlist(e$prefixes_with_others)),
    suffixes = as.character(unlist(e$suffixes)),
    with_others = as.character(unlist(e$with_others))))
  ka6 <- stats::setNames(fld(ct$ka6, "ka5"), fld(ct$ka6, "ka6"))
  voc <- list(
    masters = masters,
    prefixes = pre[order(-nchar(pre), pre)],
    prefix_hosts = pre_hosts,
    suffix_letters = suf_letters,
    symbols = sym,
    defined = unique(c(sym$code, named)),
    named = named,
    combinations = comb,
    strong = sym$code[sym$detection %in% "strong"],
    likely = sym$code[sym$detection %in% "likely"],
    ka6 = ka6,
    ka6_unread = ct$ka6_unread,
    field_codes = fld(ct$field_codes, "code")
  )
  .ka5_cache$vocabulary <- voc
  voc
}


# --- site$designation_system ----------------------------------------------------

# Fold the umlauts and the eszett a declaration may carry to ASCII.
.ka5_fold_umlauts <- function(x) {
  x <- gsub("\u00e4", "ae", x, fixed = TRUE)
  x <- gsub("\u00f6", "oe", x, fixed = TRUE)
  x <- gsub("\u00fc", "ue", x, fixed = TRUE)
  gsub("\u00df", "ss", x, fixed = TRUE)
}

# The declared notation as a key ("KA 5" -> "ka5", "Bodenkundliche
# Kartieranleitung" -> "bodenkundlichekartieranleitung"), or NA when the site
# declares none. Never an error, whatever the site is.
.ka5_declared_key <- function(site) {
  if (!is.list(site)) return(NA_character_)
  v <- tryCatch(site[["designation_system"]], error = function(e) NULL)
  v <- tryCatch(as.character(unlist(v)), error = function(e) character(0))
  if (!length(v) || is.na(v[1L])) return(NA_character_)
  v <- v[1L]
  if (!validUTF8(v)) {
    v <- suppressWarnings(iconv(v, "CP1252", "UTF-8"))
    if (is.na(v) || !validUTF8(v)) return(NA_character_)
  }
  k <- gsub("[^a-z0-9]", "", .ka5_fold_umlauts(tolower(v)))
  if (!nzchar(k) || k %in% .KA5_UNDECLARED || !grepl("[a-z]", k)) return(NA_character_)
  k
}

# Does a declared key name the KA5 notation, an older edition, or the KA6?
.ka5_key_is_ka5 <- function(key) !is.na(key) && grepl(.KA5_DECLARED_RE, key) && !.ka5_key_is_ka6(key)
.ka5_key_is_old <- function(key) !is.na(key) && grepl(.KA5_DECLARED_OLD_RE, key)
.ka5_key_is_ka6 <- function(key) !is.na(key) && grepl(.KA5_DECLARED_KA6_RE, key)

# Does a value of designation_system declare the KA5 notation (or the KA6,
# read as KA5)?
.ka5_is_declared <- function(ds) {
  k <- .ka5_declared_key(list(designation_system = ds))
  .ka5_key_is_ka5(k) || .ka5_key_is_ka6(k)
}


# --- cleaning --------------------------------------------------------------------

# Characters folded before anything is read: zero-width characters dropped,
# the no-break and thin spaces to a space, full-width ASCII to ASCII, the
# dashes and the minus sign to a hyphen, the masculine ordinal, the ring
# above and the ring operator (typed for the degree sign) to the degree sign.
.ka5_fold <- function(x) {
  cp <- utf8ToInt(x)
  if (anyNA(cp)) return(x)
  cp <- cp[!((cp >= 0x200B & cp <= 0x200D) | cp == 0xFEFF)]
  fw <- cp >= 0xFF01 & cp <= 0xFF5E
  cp[fw] <- cp[fw] - 0xFEE0L
  cp[cp %in% c(0x00A0, 0x2009, 0x202F, 0x2007, 0x2002, 0x2003)] <- 0x20L
  cp[cp %in% c(0x2010, 0x2011, 0x2012, 0x2013, 0x2014, 0x2212, 0x0096, 0x0097)] <- 0x2DL
  cp[cp %in% c(0x00BA, 0x02DA, 0x2218, 0x02DA)] <- 0x00B0L
  intToUtf8(cp)
}

# One designation cleaned: list(s, notes). Text that is not valid UTF-8 is
# read as Windows-1252, else Latin-1 (the degree sign is 0xB0 in both). NA,
# "-" and "?" record nothing. Spaces around a joiner and between a numeral and
# the symbol are dropped (II Bv, Ah - Bv); a note after a space and a trailing
# "?" are dropped, each noted.
.ka5_clean1 <- function(x) {
  out <- list(s = NA_character_, notes = character(0))
  if (length(x) != 1L || is.na(x)) return(out)
  x <- tryCatch(enc2utf8(as.character(x)), error = function(e) as.character(x))
  if (!validUTF8(x)) {
    y <- suppressWarnings(iconv(x, "CP1252", "UTF-8"))
    enc <- "Windows-1252"
    if (is.na(y) || !validUTF8(y)) {
      y <- suppressWarnings(iconv(x, "latin1", "UTF-8"))
      enc <- "Latin-1"
    }
    if (is.na(y) || !validUTF8(y)) {
      out$notes <- "not readable text (invalid characters)"
      return(out)
    }
    x <- y
    out$notes <- sprintf("read as %s text", enc)
  }
  x <- .ka5_fold(x)
  x <- trimws(gsub("[[:space:]]+", " ", x, perl = TRUE))
  if (!nzchar(x)) return(out)
  if (x %in% .KA5_MISSING) {
    out$notes <- c(out$notes, sprintf("'%s' records nothing", x))
    return(out)
  }
  x2 <- gsub(" *([-+\u00b0]) *", "\\1", x, perl = TRUE)
  x2 <- sub("^(VIII|VII|III|IX|VI|IV|II) +(?=[a-zA-Z(])", "\\1", x2, perl = TRUE)
  x2 <- sub("^([^ ]*[A-Za-z<>]) ([0-9]+)$", "\\1\\2", x2, perl = TRUE)
  # a suffix written apart from its master (B v, II C v)
  x2 <- sub("^((?:VIII|VII|III|IX|VI|IV|II)?[a-z]*[A-Z]) ([a-z]+[0-9]*)$", "\\1\\2", x2, perl = TRUE)
  if (!identical(x2, x)) {
    out$notes <- c(out$notes, "spaces inside the designation dropped")
    x <- x2
  }
  sp <- regexpr(" ", x, fixed = TRUE)
  if (sp > 0L) {
    out$notes <- c(out$notes, sprintf("'%s' after a space read as a note", substring(x, sp + 1L)))
    x <- substr(x, 1L, sp - 1L)
  }
  if (grepl("\\?+$", x)) {
    out$notes <- c(out$notes, "a trailing '?' (an uncertain designation) dropped")
    x <- sub("\\?+$", "", x)
  }
  # a whole designation in parentheses: an uncertain or discontinuous horizon
  if (grepl("^\\([^()]*[A-Z][^()]*\\)$", x) && !grepl("^\\((VIII|VII|III|IX|VI|IV|II|[AB][0-9]*)\\)$", x)) {
    out$notes <- c(out$notes, sprintf("%s: the parentheses around the designation dropped (an uncertain or discontinuous horizon)", x))
    x <- substr(x, 2L, nchar(x) - 1L)
  }
  if (nzchar(x) && !(x %in% .KA5_MISSING)) out$s <- x
  out
}


# --- one part ----------------------------------------------------------------------

# Split a run of lower-case letters into prefixes, tb, tm and tp first; NULL
# when a letter is no prefix.
.ka5_split_prefixes <- function(run, voc) {
  out <- character(0)
  p <- 1L
  n <- nchar(run)
  while (p <= n) {
    two <- substr(run, p, p + 1L)
    if (two %in% c("tb", "tm", "tp")) { out <- c(out, two); p <- p + 2L; next }
    one <- substr(run, p, p)
    if (!(one %in% voc$prefixes)) return(NULL)
    out <- c(out, one)
    p <- p + 1L
  }
  out
}

# An empty part reading.
.ka5_part0 <- function(written) {
  list(written = written, ok = FALSE, wildcard = FALSE, prefixes = character(0), master = NA_character_,
       suffix = "", code = NA_character_, bs_mark = NA_character_, digits = NA_character_,
       ka6 = NA_character_, defined = FALSE, permitted = NA, notes = character(0),
       unread = character(0))
}

# Is a part a combination the book permits (Tab. 12, or a symbol 5.6.3.3
# defines or names)? NA for a wildcard.
.ka5_part_permitted <- function(r, voc) {
  if (isTRUE(r$wildcard)) return(NA)
  cb <- voc$combinations[[r$master]]
  if (is.null(cb)) return(FALSE)
  suf_ok <- !nzchar(r$suffix) || r$suffix %in% cb$suffixes ||
    paste0(r$master, r$suffix) %in% voc$defined
  pre <- paste(r$prefixes, collapse = "")
  pre_ok <- !nzchar(pre) || pre %in% c(cb$prefixes, cb$prefixes_with_others) ||
    all(vapply(r$prefixes, function(p) r$master %in% voc$prefix_hosts[[p]], NA))
  suf_ok && pre_ok
}

# One part without joiners or parentheses ("ilCv", "tmzeGo", "Ah>", "Bv2",
# "..."): its prefixes, master, suffixes, base-saturation mark and digits.
# `fix_l`: read a capital I before C or after A as the lower-case l (the
# typing slips of KA5 data, IC and AI, p. 84 and default 12).
.ka5_read_part <- function(x, voc, fix_l = TRUE) {
  r <- .ka5_part0(x)
  if (identical(x, "...")) {
    r$ok <- TRUE
    r$wildcard <- TRUE
    r$code <- "..."
    r$notes <- "'...': any horizon"
    return(r)
  }
  y <- x
  dg <- regmatches(y, regexpr("[0-9]+$", y))
  if (length(dg)) { r$digits <- dg; y <- substr(y, 1L, nchar(y) - nchar(dg)) }
  bs <- regmatches(y, regexpr("(<<|<|>)$", y))
  if (length(bs)) { r$bs_mark <- bs; y <- substr(y, 1L, nchar(y) - nchar(bs)) }
  if (is.na(r$digits)) {
    dg <- regmatches(y, regexpr("[0-9]+$", y))
    if (length(dg)) { r$digits <- dg; y <- substr(y, 1L, nchar(y) - nchar(dg)) }
  }
  if (!nzchar(y)) { r$unread <- x; return(r) }
  # a capital I before C is the prefix l (IC, ICv, eIC); after A the suffix l (AI)
  if (fix_l && grepl("I(?=C)", y, perl = TRUE) && grepl("^[a-z]*IC", y)) {
    y <- sub("I(?=C)", "l", y, perl = TRUE)
    r$notes <- c(r$notes, sprintf("%s: the capital I before C read as the prefix l", x))
  }
  if (fix_l && grepl("^[a-z]*AI", y)) {
    y <- sub("AI", "Al", y, fixed = TRUE)
    r$notes <- c(r$notes, sprintf("%s: AI read as Al (Ai is written with a lower-case i)", x))
  }
  m <- regmatches(y, regexec("^([a-z]*)([A-Z])([a-z]*)$", y))[[1L]]
  if (!length(m)) { r$unread <- x; return(r) }
  run <- m[2L]; master <- m[3L]; suf <- m[4L]
  if (!(master %in% voc$masters)) { r$unread <- x; return(r) }
  pre <- .ka5_split_prefixes(run, voc)
  if (is.null(pre)) {
    r$unread <- x
    r$notes <- c(r$notes, sprintf("%s: '%s' is no prefix of the KA5", x, run))
    return(r)
  }
  bad <- setdiff(strsplit(suf, "", fixed = TRUE)[[1L]], voc$suffix_letters[[master]])
  if (length(bad)) {
    r$unread <- x
    r$notes <- c(r$notes, sprintf("%s: %s is no suffix of %s in the KA5", x,
                                  paste(bad, collapse = ", "), master))
    return(r)
  }
  r$ok <- TRUE
  r$prefixes <- pre
  r$master <- master
  r$suffix <- suf
  r$code <- paste0(master, suf)
  r$defined <- r$code %in% voc$defined
  off <- pre[!vapply(pre, function(p) master %in% voc$prefix_hosts[[p]], NA)]
  if (length(off)) {
    r$notes <- c(r$notes, sprintf("%s: the prefix %s is not written on %s (p. 84)", x,
                                  paste(off, collapse = ", "), master))
  }
  r$permitted <- .ka5_part_permitted(r, voc)
  if (!isTRUE(r$permitted)) {
    r$notes <- c(r$notes, sprintf("%s: not a KA5 combination (Tab. 12), read by the grammar", x))
  }
  r
}

# A part in the KA6 notation with a one-to-one KA5 reading (Hugo's decision
# Q4): NULL when there is none.
.ka5_read_ka6_part <- function(x, voc) {
  m <- regmatches(x, regexec("^([A-Z][a-z]*)([0-9]*)$", x))[[1L]]
  if (!length(m) || !(m[2L] %in% names(voc$ka6))) return(NULL)
  to <- voc$ka6[[m[2L]]]
  r <- .ka5_read_part(paste0(to, m[3L]), voc, fix_l = FALSE)
  if (!r$ok) return(NULL)
  r$written <- x
  r$ka6 <- m[2L]
  r$notes <- c(r$notes, sprintf("%s: a KA6 symbol, read as the KA5 %s", x, to))
  r
}

# Is a part a KA6 symbol or prefix with no KA5 reading?
.ka5_ka6_unread <- function(x, voc) {
  u <- voc$ka6_unread
  m <- regmatches(x, regexec("^([a-z]*)([A-Z])([a-z]*)[0-9]*$", x))[[1L]]
  if (!length(m)) return(FALSE)
  m[3L] %in% as.character(unlist(u$masters)) ||
    paste0(m[3L], m[4L]) %in% as.character(unlist(u$symbols)) ||
    (nzchar(m[2L]) && any(strsplit(m[2L], "", fixed = TRUE)[[1L]] %in% as.character(unlist(u$prefixes))))
}


# --- one designation ---------------------------------------------------------------

# Split a string on the joiners outside parentheses: list(parts, joiners).
# "-<deg>" is read as the degree sign (fGo-<deg>Sd, p. 201).
.ka5_split_joiners <- function(s) {
  ch <- strsplit(s, "", fixed = TRUE)[[1L]]
  parts <- character(0); joiners <- character(0)
  cur <- character(0); depth <- 0L; k <- 1L
  while (k <= length(ch)) {
    c1 <- ch[k]
    if (c1 == "(") depth <- depth + 1L
    if (c1 == ")") depth <- depth - 1L
    if (depth == 0L && c1 %in% c("-", "+", .KA5_DEG) && length(cur)) {
      j <- c1
      if (c1 == "-" && k < length(ch) && ch[k + 1L] == .KA5_DEG) { j <- "-\u00b0"; k <- k + 1L }
      parts <- c(parts, paste(cur, collapse = ""))
      joiners <- c(joiners, j)
      cur <- character(0)
      k <- k + 1L
      next
    }
    cur <- c(cur, c1)
    k <- k + 1L
  }
  parts <- c(parts, paste(cur, collapse = ""))
  list(parts = parts, joiners = joiners)
}

# The written variants of a string with optional pieces in parentheses: each
# group is present or absent, a group of lower-case letters with commas gives
# its alternatives (C(kc,c): C, Ckc, Cc); a numeral in parentheses after a
# numeral is an alternative numeral (II(III)aM: II or III). The first variant
# has every optional piece absent. a(Sw-)Ah is aAh or aSw-Ah, and the
# geogenic prefix written in front holds for the whole horizon (Hugo's
# decision on reading 1, 2026-11-04: .ka5_read_variant()).
.ka5_variants <- function(s) {
  m <- regexpr("\\([^()]*\\)", s)
  if (m < 0L) return(s)
  st <- as.integer(m); len <- attr(m, "match.length")
  before <- substr(s, 1L, st - 1L)
  body <- substr(s, st + 1L, st + len - 2L)
  after <- substring(s, st + len)
  rom <- paste0("^(", paste(.KA5_ROMAN, collapse = "|"), ")$")
  if (grepl(rom, body) && grepl(paste0("(", paste(.KA5_ROMAN, collapse = "|"), ")$"), before)) {
    b0 <- sub(paste0("(", paste(.KA5_ROMAN, collapse = "|"), ")$"), "", before)
    return(c(.ka5_variants(paste0(before, after)), .ka5_variants(paste0(b0, body, after))))
  }
  if (grepl("^[a-z]+(,[a-z]+)*$", body)) {
    alts <- c("", strsplit(body, ",", fixed = TRUE)[[1L]])
    return(unlist(lapply(alts, function(a) .ka5_variants(paste0(before, a, after)))))
  }
  c(.ka5_variants(paste0(before, after)), .ka5_variants(paste0(before, body, after)))
}

# A row of the reader, at its defaults.
.ka5_row <- function(written = NA_character_) {
  list(written = written, code = NA_character_, master = NA_character_, suffixes = character(0),
       prefixes = character(0), parts = character(0), part_prefixes = character(0),
       with = character(0), joiners = character(0), layer = 1L, layer_written = FALSE,
       layer_optional = FALSE, fossil = FALSE, relict = FALSE, overprint = FALSE,
       bs_mark = NA_character_, digits = NA_character_, options = character(0),
       defined = FALSE, permitted = NA, ka6 = character(0), wildcard = FALSE,
       prefixes_inherited = character(0),
       known = FALSE, unread = character(0), notes = character(0))
}

# Read one variant (no parentheses) of a designation: a numeral, parts and
# joiners. NULL-free: an unread variant has known FALSE.
.ka5_read_variant <- function(x, voc, ka6 = TRUE) {
  r <- .ka5_row(x)
  y <- x
  if (startsWith(y, "...") && nchar(y) > 3L && grepl("^\\.\\.\\.[a-z]*[A-Z]", y)) {
    # "(...fF/)": any layer numeral
    r$layer <- NA_integer_
    r$layer_written <- TRUE
    r$notes <- c(r$notes, "'...' before the symbol: any layer")
    y <- substring(y, 4L)
  } else {
    rom <- regmatches(y, regexec("^(VIII|VII|III|IX|VI|IV|II)(?=[a-z(]*[A-Z]|[a-z]*\\.\\.\\.|\\.\\.\\.|$)", y, perl = TRUE))[[1L]]
    if (length(rom) && nchar(rom[1L]) < nchar(y)) {
      # III before C may be II and a typed lC (IIIC): the longest numeral is read
      r$layer <- match(rom[2L], c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"))
      r$layer_written <- TRUE
      if (rom[2L] == "III" && grepl("^IIIC", y)) {
        r$notes <- c(r$notes, sprintf("%s: read as layer III and C (II and a typed lC is possible)", x))
      }
      y <- substring(y, nchar(rom[2L]) + 1L)
    }
  }
  if (!nzchar(y)) {
    r$notes <- c(r$notes, sprintf("'%s': no horizon written", x))
    return(r)
  }
  sp <- .ka5_split_joiners(y)
  parts <- sp$parts
  joiners <- sp$joiners
  if (any(joiners == "-\u00b0")) {
    r$notes <- c(r$notes, sprintf("%s: '-%s' read as '%s' (p. 201)", x, .KA5_DEG, .KA5_DEG))
    joiners[joiners == "-\u00b0"] <- .KA5_DEG
  }
  if (length(parts) > 3L) r$notes <- c(r$notes, sprintf("%s: more than three parts (p. 82 allows three)", x))
  if (any(!nzchar(parts))) {
    r$unread <- x
    r$notes <- c(r$notes, sprintf("'%s': an empty part", x))
    return(r)
  }
  reads <- lapply(parts, function(p) {
    z <- .ka5_read_part(p, voc)
    if (ka6 && (!z$ok || (!z$defined && !isTRUE(z$permitted) && p %in% names(voc$ka6)))) {
      k6 <- .ka5_read_ka6_part(p, voc)
      if (!is.null(k6)) return(k6)
      if (!z$ok && .ka5_ka6_unread(p, voc)) {
        z$notes <- c(z$notes, sprintf("%s: a KA6 symbol with no KA5 reading, not read", p))
      }
    }
    z
  })
  ok <- vapply(reads, function(z) z$ok, NA)
  r$notes <- c(r$notes, unlist(lapply(reads, function(z) z$notes)))
  r$parts <- vapply(reads, function(z) if (z$ok) z$code else z$written, "")
  r$part_prefixes <- vapply(reads, function(z) paste(z$prefixes, collapse = ""), "")
  r$joiners <- joiners
  r$ka6 <- unique(stats::na.omit(vapply(reads, function(z) z$ka6, "")))
  if (!all(ok)) {
    r$unread <- unique(unlist(lapply(reads[!ok], function(z) z$unread)))
    return(r)
  }
  last <- reads[[length(reads)]]
  r$code <- last$code
  r$master <- last$master
  r$suffixes <- if (nzchar(last$suffix)) strsplit(last$suffix, "", fixed = TRUE)[[1L]] else character(0)
  r$prefixes <- last$prefixes
  # a geogenic prefix written in front holds for the whole horizon (aSw-Ah is
  # an aAh, tmzeGo-Ah a tmzeAh); f and r (fossil, relict) only for their part
  # (Hugo's decision on reading 1, 2026-11-04)
  if (length(reads) > 1L) {
    front <- intersect(reads[[1L]]$prefixes, .KA5_GEOGENIC_FRONT)
    inh <- setdiff(front, last$prefixes)
    if (length(inh)) {
      r$prefixes <- c(inh, last$prefixes)
      r$prefixes_inherited <- inh
    }
  }
  r$wildcard <- isTRUE(last$wildcard)
  r$fossil <- "f" %in% last$prefixes
  r$relict <- "r" %in% last$prefixes
  r$overprint <- .KA5_DEG %in% joiners
  bs <- stats::na.omit(vapply(reads, function(z) z$bs_mark, ""))
  if (length(bs)) r$bs_mark <- bs[length(bs)]
  dg <- stats::na.omit(vapply(reads, function(z) z$digits, ""))
  if (length(dg)) r$digits <- dg[length(dg)]
  if (length(reads) > 1L) {
    r$with <- vapply(seq_len(length(reads) - 1L), function(i)
      paste0(r$part_prefixes[i], r$parts[i]), "")
  }
  n <- length(reads)
  whole <- if (n > 1L) paste0(paste0(r$part_prefixes[-n], r$parts[-n], joiners, collapse = ""), r$code) else r$code
  r$defined <- whole %in% voc$defined || (length(reads) == 1L && last$defined)
  perm <- vapply(reads, function(z) z$permitted, NA)
  r$permitted <- if (all(is.na(perm))) NA else all(perm, na.rm = TRUE)
  r$known <- TRUE
  r
}

# A part of the feature vocabulary the formulas and the profile frame share:
# the prefixes of the carrying part ("pre:tm"), the other parts with their
# joiner ("Bv-", "Ah<deg>", "Axh+") and each prefix of those parts ("Ah<deg>pre:f",
# "Go-pre:tm"), and the base-saturation mark ("bs:<"). A formula's part with
# fewer prefixes than a profile's (zeGo-Ah against tmzeGo-Ah) still fits.
.ka5_row_features <- function(r) {
  if (!isTRUE(r$known)) return(character(0))
  n <- length(r$parts)
  w <- character(0)
  if (n > 1L) {
    voc <- .ka5_vocabulary()
    for (i in seq_len(n - 1L)) {
      tok <- paste0(r$parts[i], r$joiners[i])
      pre <- .ka5_split_prefixes(r$part_prefixes[i], voc) %||% character(0)
      w <- c(w, tok, if (length(pre)) paste0(tok, "pre:", pre))
    }
  }
  c(if (length(r$prefixes)) paste0("pre:", r$prefixes), w,
    if (!is.na(r$bs_mark)) paste0("bs:", r$bs_mark))
}

#' Read one designation in the KA5 notation
#'
#' @param d A designation as recorded.
#' @param ka6 Read the one-to-one KA6 symbols (Ol, Ax, Ee, Ks ...) as their
#'   KA5 reading, with a note (Hugo's decision Q4).
#' @param typed_o Read an o or O between two parts as the degree sign (only
#'   for a designation of a profile in the KA5 notation).
#' @return A list: \code{written} (the designation once cleaned), \code{code}
#'   (the carrying part: master and suffixes as printed, Bv for Ah-Bv, Sd for
#'   fAh<deg>Sd; NA when unread), \code{master}, \code{suffixes},
#'   \code{prefixes} (of the carrying part: l, f, tm ...), \code{parts} (the
#'   codes of all parts, top to the carrying one), \code{part_prefixes},
#'   \code{with} (the other parts with their prefixes), \code{joiners},
#'   \code{layer} (1 with no numeral; NA for "..." as a numeral),
#'   \code{layer_written}, \code{layer_optional}, \code{fossil} and
#'   \code{relict} (the f and r prefixes of the carrying part),
#'   \code{overprint} (joined by the degree sign), \code{bs_mark},
#'   \code{digits} (sub-horizon), \code{options} (the codes of the readings a
#'   letter in parentheses gives: B(h)s = Bs or Bhs; the reading without the
#'   letter is the code), \code{defined} (a symbol 5.6.3.3 defines or names),
#'   \code{permitted} (Tab. 12), \code{ka6} (KA6 symbols read), \code{wildcard}
#'   ("..."), \code{known}, \code{unread} and \code{notes}.
#' @noRd
.ka5_parse_one <- function(d, ka6 = TRUE, typed_o = FALSE) {
  voc <- .ka5_vocabulary()
  cl <- .ka5_clean1(d)
  .ka5_read_clean(cl, voc, ka6 = ka6, typed_o = typed_o)
}

# Read a cleaned designation; never an error.
.ka5_read_clean <- function(cl, voc, ka6 = TRUE, typed_o = FALSE) {
  r <- tryCatch(.ka5_read_clean_(cl, voc, ka6, typed_o), error = function(e) {
    z <- .ka5_row(cl$s)
    z$notes <- c(cl$notes, sprintf("not read: %s", conditionMessage(e)))
    z
  })
  r
}

.ka5_read_clean_ <- function(cl, voc, ka6, typed_o = FALSE) {
  s <- cl$s
  r <- .ka5_row(s)
  r$notes <- cl$notes
  if (is.na(s)) return(r)
  # a field code of the description sheet is a feature, not a horizon (p. 116)
  core <- sub("^(VIII|VII|III|IX|VI|IV|II)", "", sub("[0-9]+$", "", s))
  if (core %in% voc$field_codes) {
    r$unread <- s
    r$notes <- c(r$notes, sprintf("%s: a KA5 field code (field 34, pp. 114-116), a feature, not a horizon", s))
    return(r)
  }
  vars <- .ka5_variants(s)
  rows <- lapply(vars, .ka5_read_variant, voc = voc, ka6 = ka6)
  z <- rows[[1L]]
  if (!z$known && typed_o) {
    # the degree sign typed as o or O between two parts (fAhoSd): only in a
    # profile declared or recognised as KA5, and only when the result reads
    # (p. 82); never a sign of the notation
    alt <- gsub("(?<=[a-z<>])[oO](?=[a-z]*[A-Z])", .KA5_DEG, s, perl = TRUE)
    if (!identical(alt, s)) {
      z2 <- .ka5_read_variant(alt, voc, ka6)
      if (z2$known) {
        z2$notes <- c(sprintf("%s: o read as the degree sign (%s)", s, alt), z2$notes)
        z <- z2
        rows <- list(z2)
      }
    }
  }
  z$written <- s
  # a symbol the book writes with a letter in parentheses (B(h)ms, mC+O(v)h)
  if (z$known && !z$defined) z$defined <- sub("^(VIII|VII|III|IX|VI|IV|II)", "", sub("[0-9]+$", "", s)) %in% voc$defined
  z$notes <- unique(c(cl$notes, z$notes))
  if (length(vars) > 1L) {
    z$options <- unique(vapply(rows, function(q) if (q$known) q$code else NA_character_, ""))
    z$options <- z$options[!is.na(z$options)]
    z$layer_optional <- grepl("^\\((VIII|VII|III|IX|VI|IV|II)\\)", s)
    if (length(z$options) > 1L) {
      z$notes <- c(z$notes, sprintf("%s: %s; read as %s until the data decide (default 12)", s,
                                    paste(z$options, collapse = " or "), z$code))
    }
  }
  z
}


# --- the notation of a profile ---------------------------------------------------------

# The signs of the notation in one read row: strong (TRUE or FALSE) and the
# kind of a likely sign (NA when none). A KA6 symbol read is neither.
.ka5_row_signs <- function(r, voc) {
  out <- list(strong = FALSE, likely = NA_character_)
  if (!isTRUE(r$known) || isTRUE(r$wildcard) || length(r$ka6)) return(out)
  if (.KA5_DEG %in% r$joiners) out$strong <- TRUE
  if (any(nzchar(r$part_prefixes))) out$strong <- TRUE
  if (!is.na(r$bs_mark)) out$strong <- TRUE
  if (any(r$parts %in% voc$strong)) out$strong <- TRUE
  lk <- r$parts[r$parts %in% voc$likely]
  if (length(lk)) out$likely <- lk[length(lk)]
  out
}

# Symbols only the FAO, the USDA or the CSSC write (Hugo's decision Q3): an
# Arabic layer number (2Bt), two masters run together (AB, BC, LFH), a slash
# inside a designation (E/B), or a B, C, A, E, H or O with suffix letters the
# KA5 does not write on it or writes only with others (Bw, Bk, Bg, Btg, Bss,
# Bf, Bm, Cr, Cg, Oi, Oa, Ag, Eg, Hi, He), the Canadian j with other letters
# (Btj, Bmj, Aej), and Ck and Oe written as designations (KA5 field codes,
# never horizons).
.KA5_FOREIGN_SINGLE <- list(A = c("k", "c", "b"), B = c("k", "f", "m", "ss", "kk", "c", "b", "cs"),
                            C = c("k", "kk"), O = character(0), E = character(0), H = character(0))
.ka5_fao_only <- function(s, voc = .ka5_vocabulary()) {
  if (is.na(s) || !nzchar(s)) return(FALSE)
  if (grepl("^[2-9][A-Z]", s)) return(TRUE)
  x <- sub("^(VIII|VII|III|IX|VI|IV|II)(?=[A-Z])", "", s, perl = TRUE)
  x <- sub("[0-9]+$", "", x)
  if (grepl("/", x, fixed = TRUE)) return(TRUE)
  if (grepl("[ABCEFHLMOPRSTGY][ABCEFHLMOPRSTGY]", x)) return(TRUE)
  # Ck and Oe as designations: FAO, USDA or CSSC (in the KA5 they are field
  # codes, never written as a horizon)
  if (x %in% c("Ck", "Oe")) return(TRUE)
  m <- regmatches(x, regexec("^([ABCEHO])([a-z]+)$", x))[[1L]]
  if (!length(m)) return(FALSE)
  # the Canadian j of a weakly expressed feature with other letters (Btj, Bmj,
  # Aej, Ckj); the KA5 Bj and Cj are fersiallitic and stand alone (Hugo,
  # 2026-11-04)
  if (grepl("j", m[3L], fixed = TRUE) && nchar(m[3L]) > 1L && !(x %in% voc$defined)) return(TRUE)
  letters_ok <- voc$suffix_letters[[m[2L]]]
  if (any(!(strsplit(m[3L], "", fixed = TRUE)[[1L]] %in% letters_ok))) return(TRUE)
  m[3L] %in% .KA5_FOREIGN_SINGLE[[m[2L]]] && !(x %in% voc$defined)
}

# The decision for a profile, from its cleaned designations and their
# reading.
.ka5_decide <- function(cl, rows, site, desig) {
  voc <- .ka5_vocabulary()
  s <- vapply(cl, function(z) z$s %||% NA_character_, "")
  key <- .ka5_declared_key(site)
  out <- list(ok = FALSE, status = "none", why = "", ka6 = FALSE, edition = NA_character_,
              strong = character(0), likely = character(0), foreign = character(0),
              rp = character(0), russian = FALSE, cpcs = character(0))
  if (!is.na(key)) {
    raw <- as.character(unlist(site[["designation_system"]]))[1L]
    out$why <- sprintf("site$designation_system = '%s'", raw)
    if (.ka5_key_is_ka6(key)) {
      out$ok <- TRUE
      out$ka6 <- TRUE
      out$status <- "declared_ka6"
      out$edition <- "KA6"
      out$why <- paste0(out$why, ": the KA6 notation read as KA5 (its one-to-one symbols only)")
    } else if (.ka5_key_is_ka5(key)) {
      out$ok <- TRUE
      out$status <- "declared"
      out$edition <- if (.ka5_key_is_old(key)) toupper(substr(key, 1L, 3L)) else "KA5"
      if (.ka5_key_is_old(key)) {
        out$why <- paste0(out$why, sprintf(": %s read as KA5 (the G horizons were redefined in 2005, p. 81)",
                                           out$edition))
      }
    } else {
      out$status <- "other"
    }
    return(out)
  }
  present <- !is.na(s) & nzchar(s)
  if (!any(present)) {
    out$why <- "no designation"
    return(out)
  }
  sg <- lapply(rows, .ka5_row_signs, voc = voc)
  strong <- vapply(sg, function(z) z$strong, NA) & present
  kind <- vapply(sg, function(z) z$likely, "")
  # E under an Ah over a layer II, M over a layer II: likely (Q3)
  for (i in seq_along(rows)) {
    r <- rows[[i]]
    if (!present[i] || !isTRUE(r$known)) next
    nxt <- if (i < length(rows)) rows[[i + 1L]] else NULL
    over_ii <- !is.null(nxt) && isTRUE(nxt$known) && isTRUE(nxt$layer_written) &&
      !is.na(nxt$layer) && nxt$layer > r$layer
    if (identical(r$code, "E") && over_ii && i > 1L && identical(rows[[i - 1L]]$master, "A")) kind[i] <- "E/II"
    if (identical(r$code, "M") && over_ii) kind[i] <- "M/II"
  }
  kinds <- unique(kind[present & !is.na(kind)])
  out$strong <- unique(s[strong])
  out$likely <- unique(s[present & !is.na(kind)])
  ka5_ev <- length(out$strong) >= 1L || length(kinds) >= 2L
  # what only other notations write
  nxt <- c(s[-1L], NA_character_)
  cp <- vapply(seq_along(s), function(i) present[i] && .rp_cpcs_only(s[i], nxt[i]), NA)
  out$cpcs <- unique(s[cp])
  rpn <- tryCatch(.rp_notation(desig, NULL), error = function(e) NULL)
  if (!is.null(rpn)) {
    # an RP code the KA5 does not define (Ach is both)
    rps <- as.character(rpn$strong)
    keep <- vapply(rps, function(x) {
      i <- match(x, s)
      r <- if (!is.na(i)) rows[[i]] else NULL
      is.null(r) || !isTRUE(r$known) || !(isTRUE(r$defined) || isTRUE(r$permitted))
    }, NA)
    out$rp <- rps[keep]
  }
  out$russian <- tryCatch(isTRUE(.kidpr_notation(desig, list())$ok), error = function(e) FALSE)
  # the FAO, USDA and CSSC symbols: not a KA6 symbol read, not an RP or CPCS
  # code, not two capitals of a Russian index (AY, BHF)
  fao <- vapply(seq_along(s), function(i) {
    if (!present[i] || length(rows[[i]]$ka6) || s[i] %in% c(out$rp, out$cpcs)) return(FALSE)
    w <- .ka5_fao_only(s[i], voc)
    if (w && out$russian && grepl("[A-Z][A-Z]", s[i]) && !grepl("^[2-9]", s[i])) return(FALSE)
    w
  }, NA)
  out$foreign <- unique(s[fao])
  foreign <- c(out$foreign, out$rp, out$cpcs)
  ev <- unique(c(out$strong, out$likely))
  if (ka5_ev && (length(foreign) || out$russian)) {
    out$ok <- NA
    out$status <- "mixed"
    out$why <- sprintf("undecided notation: %s written in the KA5 notation, and %s",
                       paste(ev, collapse = ", "),
                       paste(c(if (length(out$foreign)) sprintf("%s only in the FAO, USDA or CSSC notation",
                                                                paste(out$foreign, collapse = ", ")),
                               if (length(out$rp)) sprintf("%s only in the RP 2008 notation",
                                                           paste(out$rp, collapse = ", ")),
                               if (length(out$cpcs)) sprintf("%s only in the CPCS notation",
                                                             paste(out$cpcs, collapse = ", ")),
                               if (out$russian) "signs of the Russian notation"), collapse = " and "))
  } else if (ka5_ev) {
    out$ok <- TRUE
    out$status <- "recognised"
    out$why <- sprintf("%s: %s", paste(ev, collapse = ", "),
                       if (length(out$strong)) "written only in the KA5 notation"
                       else "two kinds of symbol the KA5 notation alone usually writes")
  } else {
    out$status <- if (length(out$rp)) "rp" else if (out$russian) "russian"
                  else if (length(out$cpcs)) "cpcs" else if (length(out$foreign)) "fao" else "none"
    out$why <- if (length(out$likely)) sprintf("%s: one kind of symbol the KA5 usually writes is not enough",
                                               paste(out$likely, collapse = ", "))
               else "no symbol only the KA5 notation writes (the country is not read)"
  }
  out
}

#' Is a profile written in the KA5 notation?
#'
#' In order: site$designation_system decides when it names a notation (a
#' value starting with ka3, ka4, ka5, kartieranleitung, bodensystematik,
#' agboden or dbg is the KA5, KA3 and KA4 read as KA5 with a note; ka6 the
#' KA6, read through its one-to-one symbols; any other notation is not the
#' KA5). Otherwise the profile is in the KA5 notation when one designation
#' has a strong sign (the degree sign, a lower-case prefix before a capital
#' master: lC, fAh, aM, tmGo; a base-saturation mark; Sw, Sd, Srw, Srd, Sew,
#' Sq, Sdg, Tv, Tu, Axh, Acxh, Ael, Ahe, Aeh, Aih, Bfv, Bsv, Bhsv, Btv, Bbt,
#' Bbms, Gor, Gro, Gso, Gkso, Gmso, Ex) or two have likely signs of two kinds
#' (Bv, Cv, Cn, Al, Aa, Hm, Hw, Hr, P, T, an E under an Ah over a layer II, an
#' M over a layer II), and no designation is a symbol only the FAO, USDA or
#' CSSC write (Bw, Bk, Bg, Oi, Oa, AB, 2Bt ...), an RP 2008 code the KA5 does
#' not define, a CPCS 1967 symbol (A00, (B), Bfe), and the profile is not in
#' the Russian notation. A profile with both is undecided (ok NA). The
#' country is never read (Hugo's decision Q3).
#' @return A list: \code{ok} (TRUE, FALSE or NA), \code{status} (declared,
#'   declared_ka6, recognised, mixed, other, fao, rp, russian, cpcs, none),
#'   \code{why}, \code{ka6}, \code{edition}, and the symbols found:
#'   \code{strong}, \code{likely}, \code{foreign}, \code{rp}, \code{russian},
#'   \code{cpcs}.
#' @noRd
.ka5_notation <- function(desig, site = NULL) {
  voc <- .ka5_vocabulary()
  d <- tryCatch(as.character(unlist(desig)), error = function(e) character(0))
  cl <- lapply(d, function(x) tryCatch(.ka5_clean1(x), error = function(e)
    list(s = NA_character_, notes = sprintf("not read: %s", conditionMessage(e)))))
  rows <- .ka5_iiic(lapply(cl, .ka5_read_clean, voc = voc, ka6 = TRUE), cl, voc)
  .ka5_decide(cl, rows, site, d)
}

# IIIC with no layer II above it is layer II and a typed lC (IIlC); with a
# layer II above, layer III and C; a note in both cases (Hugo's decision on
# reading 8, 2026-11-04).
.ka5_iiic <- function(rows, cl, voc, typed_o = FALSE) {
  seen_ii <- FALSE
  for (i in seq_along(rows)) {
    s <- cl[[i]]$s %||% NA_character_
    if (!is.na(s) && grepl("^IIIC", s)) {
      if (seen_ii) {
        rows[[i]]$notes <- c(setdiff(rows[[i]]$notes, grep("typed lC is possible", rows[[i]]$notes, value = TRUE)),
                             sprintf("%s: layer III and C (a layer II lies above)", s))
      } else {
        c2 <- cl[[i]]
        c2$s <- sub("^IIIC", "IIlC", s)
        z <- .ka5_read_clean(c2, voc, ka6 = TRUE, typed_o = typed_o)
        if (isTRUE(z$known)) {
          z$written <- s
          z$notes <- c(z$notes, sprintf("%s: layer II and a typed lC (IIlC), no layer II lying above", s))
          rows[[i]] <- z
        }
      }
    }
    if (isTRUE(rows[[i]]$known) && isTRUE(rows[[i]]$layer_written) && identical(rows[[i]]$layer, 2L)) seen_ii <- TRUE
  }
  rows
}

#' All the designations of a profile, read in the KA5 notation
#'
#' The profile's notation is decided first (.ka5_notation()). In the KA5
#' notation every designation is read (.ka5_parse_one()); in another
#' notation, or an undecided one, no designation is read as a KA5 symbol and
#' the KA5 reading is kept in \code{read_as_ka5} for the evidence. The KA6
#' one-to-one symbols are read only in a profile declared ka6 or recognised
#' as KA5 (Hugo's decision Q4). Nothing stops with an error.
#' @return A list of vectors, one element per designation: the fields of
#'   .ka5_parse_one() (list columns for the vectors), \code{features} (the
#'   feature vocabulary of the formulas), \code{read_as_ka5}, then \code{ka5}
#'   (TRUE, FALSE or NA), \code{status}, \code{why}, \code{ka6} and
#'   \code{edition}.
#' @noRd
.ka5_designations <- function(desig, site = NULL) {
  voc <- .ka5_vocabulary()
  d <- tryCatch(as.character(unlist(desig)), error = function(e) character(0))
  cl <- lapply(d, function(x) tryCatch(.ka5_clean1(x), error = function(e)
    list(s = NA_character_, notes = sprintf("not read: %s", conditionMessage(e)))))
  rows <- .ka5_iiic(lapply(cl, .ka5_read_clean, voc = voc, ka6 = TRUE), cl, voc)
  nt <- .ka5_decide(cl, rows, site, d)
  if (isTRUE(nt$ok)) {
    # in a KA5 profile, the degree sign typed o or O (fAhoSd) is read where
    # the designation does not read otherwise; it was no sign of the notation
    rows <- .ka5_iiic(lapply(cl, .ka5_read_clean, voc = voc, ka6 = TRUE, typed_o = TRUE), cl, voc, typed_o = TRUE)
  }
  rows <- lapply(seq_along(rows), function(i) {
    z <- rows[[i]]
    z$read_as_ka5 <- z$code
    if (!isTRUE(nt$ok)) {
      keep <- z
      z <- .ka5_row(cl[[i]]$s %||% NA_character_)
      z$read_as_ka5 <- keep$code
      z$notes <- c(cl[[i]]$notes,
                   if (is.na(nt$ok) && keep$known && !is.na(keep$code))
                     sprintf("read as %s in the KA5 notation, which the profile may not be in", keep$code))
    } else if (length(z$ka6) && !nt$ka6) {
      z$notes <- c(z$notes, "a KA6 symbol in a KA5 profile")
    }
    if (isTRUE(nt$ok) && !is.na(nt$edition) && nt$edition %in% c("KA3", "KA4") && identical(z$master, "G")) {
      z$notes <- c(z$notes, sprintf("%s: a G horizon of the %s, redefined in 2005 (p. 81)", z$code, nt$edition))
    }
    z$features <- .ka5_row_features(z)
    z
  })
  proto <- c(.ka5_row(), list(features = character(0), read_as_ka5 = NA_character_))
  lists <- c("suffixes", "prefixes", "parts", "part_prefixes", "with", "joiners", "options",
             "ka6", "unread", "notes", "features")
  out <- lapply(names(proto), function(f) {
    v <- lapply(rows, `[[`, f)
    if (f %in% lists) lapply(v, as.character)
    else if (f == "layer") vapply(v, function(z) as.integer(z %||% NA_integer_)[1L], 1L)
    else if (is.logical(proto[[f]])) vapply(v, function(z) isTRUE(z), NA)
    else vapply(v, function(z) as.character(z %||% NA_character_)[1L], "")
  })
  names(out) <- names(proto)
  out$permitted <- vapply(rows, function(z) as.logical(z$permitted %||% NA)[1L], NA)
  out$ka5 <- nt$ok
  out$status <- nt$status
  out$why <- nt$why
  out$ka6_declared <- nt$ka6
  out$edition <- nt$edition
  out
}

# The symbol of a read row without its numeral and digits, as the sequence
# engine's token code: the other parts with their prefixes and joiners, the
# prefixes and code of the carrying part, the base-saturation mark ("Bv-Ah",
# "ilCv", "fAh\u00b0Sd", "Ah<"). Two horizons with one carrying code (Bv-Ah over
# Ah, Ael+Bt over Bt) stay two horizons; sub-horizons (Bv1, Bv2) merge.
.ka5_row_symbol <- function(r) {
  if (!isTRUE(r$known) || is.na(r$code)) return(NA_character_)
  n <- length(r$parts)
  paste0(if (n > 1L) paste0(r$part_prefixes[-n], r$parts[-n], r$joiners, collapse = ""),
         paste(r$prefixes, collapse = ""), r$code, if (!is.na(r$bs_mark)) r$bs_mark)
}

#' The horizons of a profile in the KA5 notation as a frame
#'
#' One row per horizon with the horizon its designation names: the last part
#' carries it (Hugo's decision Q8): \code{carrying} (Bv for Ah-Bv, Sd for
#' fAh<deg>Sd), \code{master}, \code{layer}, \code{fossil}, \code{relict}, the
#' base-saturation mark, the other parts in \code{with}, \code{features} (the
#' prefixes, the other parts with their joiner and the mark, as the formulas
#' write them) and \code{options} (the readings a letter in parentheses gives).
#' The frame is what the sequence engine reads (match_horizon_sequence()): its
#' \code{code} is the symbol without numeral and digits (.ka5_row_symbol()), and
#' \code{overlay} holds the carrying code and the master, so that a formula
#' slot written with the carrying code (Ah) takes Bv-Ah, and a master written
#' without suffix (G, C) stands for its combinations (p. 199). Rows not read
#' (another notation, or unread) have code NA and route NA.
#' @param x A PedonRecord, or a character vector of designations.
#' @param site The site (taken from the PedonRecord when NULL).
#' @param top_cm,bottom_cm Depths, for a character vector.
#' @noRd
.ka5_horizon_frame <- function(x, site = NULL, top_cm = NULL, bottom_cm = NULL) {
  if (inherits(x, "PedonRecord")) {
    h <- x$horizons
    site <- site %||% x$site
    desig <- h$designation
    top_cm <- h$top_cm
    bottom_cm <- h$bottom_cm
  } else {
    desig <- as.character(x)
  }
  n <- length(desig)
  top_cm <- if (is.null(top_cm)) rep(NA_real_, n) else as.numeric(top_cm)
  bottom_cm <- if (is.null(bottom_cm)) rep(NA_real_, n) else as.numeric(bottom_cm)
  dd <- .ka5_designations(desig, site)
  ok <- isTRUE(dd$ka5)
  na <- rep(NA_character_, n)
  rows <- lapply(seq_len(n), function(i) list(known = dd$known[i], code = dd$code[i], parts = dd$parts[[i]],
                                              part_prefixes = dd$part_prefixes[[i]], joiners = dd$joiners[[i]],
                                              prefixes = dd$prefixes[[i]], bs_mark = dd$bs_mark[i]))
  sym <- if (ok) vapply(rows, .ka5_row_symbol, "") else na
  carrying <- if (ok) dd$code else na
  master <- if (ok) dd$master else na
  data.frame(
    horizon_idx = seq_len(n), top_cm = top_cm, bottom_cm = bottom_cm,
    designation = desig, code = sym, carrying = carrying, master = master,
    passed = !is.na(sym),
    layer = dd$layer, fossil = dd$fossil & ok, relict = dd$relict & ok,
    bs_mark = if (ok) dd$bs_mark else na,
    with = I(if (ok) dd$with else rep(list(character(0)), n)),
    features = I(if (ok) dd$features else rep(list(character(0)), n)),
    # Ap reads as Ah for the formulas' plain Ah (p. 199: Ah and Ap are used
    # alike for topsoils)
    overlay = I(lapply(seq_len(n), function(i)
      if (is.na(sym[i])) character(0)
      else setdiff(unique(c(carrying[i], master[i], if (identical(carrying[i], "Ap")) "Ah")), sym[i]))),
    options = I(if (ok) lapply(dd$options, function(o) if (length(o) > 1L) o else character(0))
                else rep(list(character(0)), n)),
    # the buried soil of II...: from the first fossil row of a layer II or
    # deeper downward (p. 235)
    buried = .ka5_buried_rows(ok, dd$layer, dd$fossil),
    route = ifelse(!is.na(sym), "notation", NA_character_),
    stringsAsFactors = FALSE)
}

# 1 from the first fossil (f) row of a layer II or deeper downward, else 0.
.ka5_buried_rows <- function(ok, layer, fossil) {
  n <- length(layer)
  out <- integer(n)
  if (!ok || !n) return(out)
  first <- which(fossil & !is.na(layer) & layer >= 2L)[1L]
  if (!is.na(first)) out[first:n] <- 1L
  out
}

# Does a row of a KA5 frame fit a form of a slot: by its code or an overlay,
# with the form's required features?
.ka5_row_fits <- function(code, overlay, features, f) {
  (identical(code, f$code) || f$code %in% overlay) && all(f$required %in% features)
}

#' The table of a KA5 frame prepared for one formula (the engine's
#' prepare_table hook, v0.9.360)
#'
#' A master written without suffix (C, G, aG, O) stands for one or several of
#' its combinations, and horizons separated by commas occur as alternatives
#' or in sequence (p. 199): a run of consecutive rows that fit such a slot
#' takes one code, so the engine reads it as one horizon (Ah/Bv/Cv/Cn is
#' Ah/Bv/C). A transitional row merges into the neighbour with the same
#' carrying code (Al+Bt into the Bt, Sw-Sd into the Sd), unless the formula
#' names the transition in a slot of its own (Ah/Ah-Go/(Go/)Gr). Rows of a
#' buried soil, unread rows and other tables are left alone. Byte-neutral for
#' every other system: only KA5 formulas carry the hook.
#' @noRd
.ka5_prepare_table <- function(table, spec) {
  if (!is.data.frame(table) || !nrow(table) || is.null(table$carrying)) return(table)
  n <- nrow(table)
  code <- as.character(table$code)
  ov <- if (is.null(table$overlay)) rep(list(character(0)), n) else lapply(table$overlay, as.character)
  fe <- if (is.null(table$features)) rep(list(character(0)), n) else lapply(table$features, as.character)
  car <- as.character(table$carrying)
  wi <- if (is.null(table$with)) rep(list(character(0)), n) else table$with
  bur <- if (is.null(table$buried)) integer(n) else as.integer(table$buried)
  slots <- Filter(function(z) z$kind == "horizon", spec$slots)
  if (!length(slots)) return(table)
  fits <- lapply(seq_len(n), function(i) {
    if (is.na(code[i]) || bur[i] > 0L) return(integer(0))
    which(vapply(slots, function(z) any(vapply(z$forms, function(f)
      .ka5_row_fits(code[i], ov[[i]], fe[[i]], f), NA)), NA))
  })
  # slots that name a row's transition: a form requiring one of its features
  # that are other parts
  names_tr <- function(i) {
    w <- fe[[i]][!startsWith(fe[[i]], "pre:") & !startsWith(fe[[i]], "bs:") & !grepl("pre:", fe[[i]], fixed = TRUE)]
    if (!length(w)) return(integer(0))
    which(vapply(slots, function(z) any(vapply(z$forms, function(f) any(w %in% f$required) &&
      .ka5_row_fits(code[i], ov[[i]], fe[[i]], f), NA)), NA))
  }
  # transitions into the neighbour with the same carrying code
  for (i in seq_len(n)) {
    if (!length(wi[[i]]) || is.na(car[i]) || bur[i] > 0L) next
    own <- names_tr(i)
    for (j in c(i + 1L, i - 1L)) {
      if (j < 1L || j > n || is.na(car[j]) || bur[j] > 0L || !identical(car[j], car[i]) || length(wi[[j]])) next
      # a formula that names the transition: merge only into a neighbour that
      # can only stand in the transition's own slot (Go-Bv into the Bv of
      # Ah/(Go-)Bv/..., not Sw-Axh into the Axh of Axh/(Sw-)Axh/...)
      if (length(own) && !(length(fits[[j]]) && all(fits[[j]] %in% own))) next
      code[i] <- code[j]
      ov[[j]] <- unique(c(ov[[j]], ov[[i]]))
      break
    }
  }
  # runs of rows one slot written as a bare master or with commas takes
  run_slot <- vapply(slots, function(z) isTRUE(z$run), NA)
  # a row joins a run only when the run's slot is the one slot it fits: a row
  # another slot may take is left to the engine's alignment
  elig <- vapply(fits, function(f) length(f) == 1L && run_slot[f], NA)
  i <- 1L
  while (i < n) {
    if (is.na(code[i]) || bur[i] > 0L || !elig[i]) { i <- i + 1L; next }
    j <- i
    shared <- intersect(fits[[i]], which(run_slot))
    while (j < n && length(shared) && !is.na(code[j + 1L]) && bur[j + 1L] == 0L && elig[j + 1L] &&
           length(intersect(shared, fits[[j + 1L]]))) {
      shared <- intersect(shared, fits[[j + 1L]])
      j <- j + 1L
    }
    if (j > i) code[i:j] <- code[i]
    i <- j + 1L
  }
  table$code <- code
  table
}


# --- the formulas of the types and subtypes ------------------------------------------------

# Split s on `sep` outside parentheses.
.ka5_split_outside <- function(s, sep) {
  ch <- strsplit(s, "", fixed = TRUE)[[1L]]
  if (!length(ch)) return("")
  depth <- 0L
  cut <- integer(0)
  for (k in seq_along(ch)) {
    if (ch[k] == "(") depth <- depth + 1L
    else if (ch[k] == ")") depth <- depth - 1L
    else if (ch[k] == sep && depth == 0L) cut <- c(cut, k)
  }
  if (!length(cut)) return(s)
  from <- c(1L, cut + 1L)
  to <- c(cut - 1L, length(ch))
  mapply(function(a, b) if (b >= a) paste(ch[a:b], collapse = "") else "", from, to,
         USE.NAMES = FALSE)
}

# Position of the parenthesis closing the one at position i of ch; NA when
# unbalanced.
.ka5_close <- function(ch, i) {
  depth <- 0L
  for (k in seq.int(i, length(ch))) {
    if (ch[k] == "(") depth <- depth + 1L
    else if (ch[k] == ")") { depth <- depth - 1L; if (depth == 0L) return(k) }
  }
  NA_integer_
}

# Does s contain `sep` at depth 0?
.ka5_has_outside <- function(s, sep) length(.ka5_split_outside(s, sep)) > 1L

# The lead of a designation: its numeral ("II", "..." or ""), its prefix
# tokens and the rest from the master on.
.ka5_lead <- function(x, voc) {
  rom <- regmatches(x, regexec("^(VIII|VII|III|IX|VI|IV|II|\\.\\.\\.)?([a-z]*)(.*)$", x))[[1L]]
  pre <- .ka5_split_prefixes(rom[3L], voc)
  list(roman = rom[2L], prefixes = pre %||% character(0), rest = rom[4L], ok = !is.null(pre))
}

# The alternatives of a formula item written with commas, completed where
# only the prefixes or the numeral alternate: "tp,tbAh" is tpAh or tbAh,
# "IIr,fBj" IIrBj or IIfBj, "i,eC" iC or eC, "II,IIIfF" IIfF or IIIfF,
# "tmo,tbo,tpoAh" tmoAh, tboAh or tpoAh (pp. 199, 201, 252).
.ka5_alternatives <- function(item, voc) {
  alts <- .ka5_split_outside(item, ",")
  if (length(alts) < 2L || any(!nzchar(alts))) return(alts)
  # a numeral written on the first alternative holds for the others
  # (IIfAe<deg>Go,fAe<deg>Gr: both in layer II; p. 254)
  rom1 <- regmatches(alts[1L], regexpr("^(VIII|VII|III|IX|VI|IV|II)(?=[a-z]*[A-Z(])", alts[1L], perl = TRUE))
  if (length(rom1)) {
    for (i in seq_along(alts)[-1L]) {
      if (!grepl("^(\\()?(VIII|VII|III|IX|VI|IV|II|\\.\\.\\.)", alts[i]) && grepl("[A-Z]", alts[i]))
        alts[i] <- paste0(rom1, alts[i])
    }
  }
  # a joined head written only on the first alternative holds for the others
  # (R+B(h)s,B(s)h: R+B(h)s or R+B(s)h; PP-YU, p. 237)
  j1 <- .ka5_split_joiners(alts[1L])
  if (length(j1$parts) > 1L && all(j1$joiners == "+")) {
    rest <- alts[-1L]
    if (all(nzchar(rest)) && all(vapply(rest, function(a) length(.ka5_split_joiners(a)$parts) == 1L, NA)) &&
        all(grepl("[A-Z]", rest))) {
      n1 <- length(j1$parts)
      head <- paste0(paste0(j1$parts[-n1], j1$joiners, collapse = ""))
      alts[-1L] <- paste0(head, rest)
    }
  }
  last <- .ka5_lead(alts[length(alts)], voc)
  bare <- vapply(alts[-length(alts)], function(a) !grepl("[A-Z(.]", sub("^(VIII|VII|III|IX|VI|IV|II)", "", a)),
                 NA, USE.NAMES = FALSE)
  if (!any(bare) || !nzchar(last$rest)) return(alts)
  k <- length(last$prefixes)
  head <- NULL
  out <- alts
  for (i in which(bare)) {
    a <- .ka5_lead(alts[i], voc)
    if (!a$ok) next
    if (!length(a$prefixes) && nzchar(a$roman)) {
      # the numerals alternate
      out[i] <- paste0(a$roman, paste(last$prefixes, collapse = ""), last$rest)
      next
    }
    n <- length(a$prefixes)
    if (n < k) next
    h <- paste0(a$roman, paste(a$prefixes[seq_len(n - k)], collapse = ""))
    head <- head %||% h
    out[i] <- paste0(h, paste(a$prefixes[(n - k + 1L):n], collapse = ""), last$rest)
  }
  if (!is.null(head) && nzchar(head) && !nzchar(last$roman)) out[length(out)] <- paste0(head, alts[length(alts)])
  out
}

# The forms of one alternative of a formula slot: each written variant read
# as a KA5 designation; list(forms, unknown, notes).
.ka5_formula_forms <- function(alt, voc, layer) {
  forms <- list(); unknown <- character(0); notes <- character(0)
  vars <- .ka5_variants(alt)
  for (v in vars) {
    r <- .ka5_read_variant(v, voc, ka6 = FALSE)
    if (!r$known) {
      unknown <- c(unknown, v)
      next
    }
    notes <- c(notes, r$notes[!grepl("not a KA5 combination|not written on", r$notes)])
    lith <- if (isTRUE(r$layer_written)) {
      if (is.na(r$layer)) "..." else c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX")[r$layer]
    } else NA_character_
    req <- .ka5_row_features(r)
    forms[[length(forms) + 1L]] <- list(
      code = r$code, written = v, lith = lith, layer = if (isTRUE(r$layer_written)) r$layer else layer,
      subdiv = NA_character_, rockflag = NA_character_, mark = NA_character_,
      features = req, typical = character(0), composition = character(0),
      required = req, signature = r$code, master = r$master, prefixes = r$prefixes,
      with = r$with, bs_mark = r$bs_mark, wildcard = isTRUE(r$wildcard),
      permitted = r$permitted)
  }
  list(forms = forms, unknown = unknown, notes = notes)
}

# The slots of a sequence ("Ah/(Go/)Gso(/Gr)"), each optional when
# `optional`. Returns list(slots, unknown, notes, layer).
.ka5_formula_seq <- function(s, voc, optional, layer) {
  slots <- list(); unknown <- character(0); notes <- character(0)
  items <- .ka5_split_outside(s, "/")
  if (length(items) > 1L && any(!nzchar(items))) {
    unknown <- c(unknown, s)
    notes <- c(notes, sprintf("'%s': an empty horizon between two '/'", s))
  }
  for (item in items) {
    if (!nzchar(item)) next
    ch <- strsplit(item, "", fixed = TRUE)[[1L]]
    buf <- ""
    k <- 1L
    pending <- list()
    while (k <= length(ch)) {
      if (ch[k] == "(") {
        cl <- .ka5_close(ch, k)
        if (is.na(cl)) { unknown <- c(unknown, item); break }
        body <- paste(ch[seq.int(k + 1L, length.out = cl - k - 1L)], collapse = "")
        if (.ka5_has_outside(body, "/")) {
          # an optional group of horizons: (Go/), (/Gr), (aM,alC/)
          if (nzchar(buf)) {
            if (grepl("^(\\()?(VIII|VII|III|IX|VI|IV|II|\\.\\.\\.)(\\))?$", buf) || grepl("^[a-z]+$", buf)) {
              pending$lead <- c(pending$lead, buf)
            } else {
              it <- .ka5_formula_item(buf, voc, optional, layer)
              slots <- c(slots, it$slots); unknown <- c(unknown, it$unknown); notes <- c(notes, it$notes)
              layer <- it$layer
            }
            buf <- ""
          }
          inner <- gsub("^/+|/+$", "", body)
          if (length(pending$lead)) {
            # a numeral before an optional group goes on its first horizon
            inner <- paste0(paste(gsub("[()]", "", pending$lead), collapse = ""), inner)
            if (any(grepl("^\\(", pending$lead))) inner <- paste0("(", sub("^((VIII|VII|III|IX|VI|IV|II))", "\\1)", inner))
            pending$carry <- pending$lead
            pending$lead <- NULL
          }
          g <- .ka5_formula_seq(inner, voc, optional = TRUE, layer = layer)
          slots <- c(slots, g$slots); unknown <- c(unknown, g$unknown); notes <- c(notes, g$notes)
          k <- cl + 1L
          next
        }
        buf <- paste0(buf, paste(ch[k:cl], collapse = ""))
        k <- cl + 1L
        next
      }
      buf <- paste0(buf, ch[k])
      k <- k + 1L
    }
    if (nzchar(buf)) {
      if (length(pending$carry) && !grepl("^(\\()?(VIII|VII|III|IX|VI|IV|II)", buf)) {
        # the numeral written before the optional group holds for what follows it
        buf <- paste0(paste(pending$carry, collapse = ""), buf)
      }
      it <- .ka5_formula_item(buf, voc, optional, layer)
      slots <- c(slots, it$slots); unknown <- c(unknown, it$unknown); notes <- c(notes, it$notes)
      layer <- it$layer
    }
  }
  list(slots = slots, unknown = unknown, notes = notes, layer = layer)
}

# One item of a formula without "/" at depth 0: an open tail or head
# ("..."), a buried soil ("II...", "III(f)..."), an optional horizon in
# parentheses ("(Gr)", "(Ahe)") or a horizon with its alternatives.
.ka5_formula_item <- function(buf, voc, optional, layer) {
  mk <- function(kind, forms, opt, written) {
    list(index = NA_integer_, written = written, kind = kind, role = NA_character_,
         optional = opt, tested = NA, forms = forms,
         codes = unique(vapply(forms, function(f) f$code, "")),
         buried = if (kind == "buried") "square" else NA_character_,
         small = character(0), composition = character(0))
  }
  form0 <- function(code, written, lith) list(
    code = code, written = written, lith = lith, layer = layer, subdiv = NA_character_,
    rockflag = NA_character_, mark = NA_character_, features = character(0), typical = character(0),
    composition = character(0), required = character(0), signature = code, master = NA_character_,
    prefixes = character(0), with = character(0), bs_mark = NA_character_, wildcard = TRUE,
    permitted = NA)
  if (identical(buf, "...")) {
    return(list(slots = list(mk("open", list(form0("...", buf, NA_character_)), TRUE, buf)),
                unknown = character(0), notes = character(0), layer = layer))
  }
  bm <- regmatches(buf, regexec("^(VIII|VII|III|IX|VI|IV|II)(\\(([a-z]+)\\)|[a-z]*)\\.\\.\\.$", buf))[[1L]]
  if (length(bm)) {
    # the horizons of a buried soil (p. 235): II...; III(f)... with an
    # optional fossil prefix
    lay <- match(bm[2L], c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"))
    f <- form0(.FORMULA_BURIED[["square"]], buf, bm[2L])
    f$layer <- lay
    f$typical <- if (nzchar(bm[3L])) paste0("pre:", gsub("[()]", "", bm[3L])) else character(0)
    return(list(slots = list(mk("buried", list(f), TRUE, buf)), unknown = character(0),
                notes = sprintf("%s: the horizons of a buried soil", buf), layer = lay))
  }
  opt <- optional
  inner <- buf
  ch <- strsplit(buf, "", fixed = TRUE)[[1L]]
  if (ch[1L] == "(" && identical(.ka5_close(ch, 1L), length(ch)) && grepl("[A-Z]|\\.\\.\\.", buf) &&
      !grepl("^\\((VIII|VII|III|IX|VI|IV|II)\\)$", buf)) {
    opt <- TRUE
    inner <- substr(buf, 2L, nchar(buf) - 1L)
  }
  forms <- list(); unknown <- character(0); notes <- character(0)
  alts <- .ka5_alternatives(inner, voc)
  for (a in alts) {
    if (!nzchar(a)) {
      unknown <- c(unknown, inner)
      notes <- c(notes, sprintf("'%s': an empty alternative", inner))
      next
    }
    f <- .ka5_formula_forms(a, voc, layer)
    forms <- c(forms, f$forms); unknown <- c(unknown, f$unknown); notes <- c(notes, f$notes)
  }
  if (!length(forms)) return(list(slots = list(), unknown = unique(c(unknown, buf)), notes = notes, layer = layer))
  lays <- vapply(forms, function(f) as.integer(f$layer %||% NA_integer_), 1L)
  lay <- if (all(is.na(lays))) layer else max(lays, na.rm = TRUE)
  sl <- mk("horizon", forms, opt, inner)
  # a slot written with commas, or as a master without suffix, takes a run of
  # consecutive horizons (p. 199)
  codes <- vapply(forms, function(f) f$code, "")
  sl$run <- length(alts) > 1L || all(codes %in% voc$masters)
  list(slots = list(sl), unknown = unknown, notes = unique(notes),
       layer = if (is.na(lay)) layer else lay)
}

#' Parse a formula of the KA5 types and subtypes
#'
#' The horizon formulas of Liste 31 (pp. 199-201) and of the subtypes
#' (5.7.1), as the book writes them (p. 199): horizons from top to bottom
#' separated by "/"; "," alternatives (also of prefixes, "tp,tbAh", and of
#' numerals, "II,IIIfF"); "( )" around horizons may be absent ("(Go/)",
#' "(/Gr)", "(Gr)", "(aM,alC/)"); "( )" inside a symbol around letters a set
#' of readings ("B(h)s" = Bs or Bhs, "C(kc,c)" = C, Ckc or Cc, "a(e)Ah",
#' "(Sw-)Ah", "(II)Sd", "II(III)aM"); "..." any continuation, "II..." the
#' horizons of a buried soil, "R+..." an R with any horizon, ".../" any upper
#' soil, "(...fF/)" an fF in any layer. Each designation is read by the KA5
#' reader; the last part carries the horizon (Hugo's decision Q8).
#'
#' The result is the sequence engine's formula object (as
#' .parse_profile_formula() returns, class soilkey_formula), so that
#' match_horizon_sequence() reads it: one slot per "/" item, the carrying
#' code as the slot code (a form per alternative and per reading of a letter
#' in parentheses), the prefixes of the carrying part ("pre:tm"), the other
#' parts with their joiner ("Bv-", "fAh<deg>") and the base-saturation mark
#' ("bs:<") as required features. A buried soil is a slot of kind buried
#' (optional, the engine's [ABC]); "..." a slot of kind open, which the
#' engine does not test. Depths and thicknesses are side tests
#' (.ka5_window()), not positions.
#' @param s The formula, one string.
#' @return A soilkey_formula with, besides the engine's fields, \code{system}
#'   ("ka5"), \code{open_head}, \code{open_tail}, \code{unknown} (the
#'   designations not read: none for every formula of the book) and
#'   \code{notes}. Never an error for a string.
#' @noRd
.ka5_parse_formula <- function(s) {
  if (!is.character(s) || length(s) != 1L || is.na(s) || !nzchar(trimws(s)))
    rlang::abort("A KA5 formula must be one non-empty string.")
  voc <- .ka5_vocabulary()
  x <- gsub("[[:space:]]+", "", .ka5_fold(enc2utf8(s)))
  ch <- strsplit(x, "", fixed = TRUE)[[1L]]
  unknown <- character(0)
  if (sum(ch == "(") != sum(ch == ")")) {
    unknown <- x
    sq <- list(slots = list(), unknown = character(0), notes = "unbalanced parentheses")
  } else {
    sq <- .ka5_formula_seq(x, voc, optional = FALSE, layer = 1L)
  }
  slots <- sq$slots
  kinds <- vapply(slots, function(z) z$kind, "")
  first <- which(kinds == "horizon")[1L]
  for (i in seq_along(slots)) {
    z <- slots[[i]]
    z$index <- i
    z$role <- if (z$kind == "open") "feature" else if (z$kind == "buried") "parent"
              else if (!is.na(first) && i == first) "surface" else "middle"
    z$tested <- z$kind %in% c("horizon", "buried")
    slots[[i]] <- z
  }
  use <- Filter(function(z) z$tested, slots)
  req <- Filter(function(z) !z$optional, use)
  details <- sum(vapply(req, function(z) length(z$forms[[1L]]$required), 0))
  structure(list(
    formula = s,
    slots = slots,
    parents = character(0),
    codes = unique(unlist(lapply(use, function(z) z$codes))),
    specificity = c(slots = length(req), details = details),
    system = "ka5",
    open_head = length(kinds) > 0L && kinds[1L] == "open",
    open_tail = length(kinds) > 0L && kinds[length(kinds)] %in% c("open", "buried"),
    unknown = unique(c(unknown, sq$unknown)),
    notes = unique(sq$notes),
    prepare_table = .ka5_prepare_table
  ), class = "soilkey_formula")
}

# A parsed KA5 formula written back: each slot's forms joined by ",",
# optional slots in parentheses, slots joined by "/". Reading it again gives
# the same slots (the round trip of the tests).
.ka5_format_formula <- function(spec) {
  paste(vapply(spec$slots, function(z) {
    w <- paste(unique(vapply(z$forms, function(f) f$written, "")), collapse = ",")
    if (z$optional && z$kind == "horizon") paste0("(", w, ")") else w
  }, ""), collapse = "/")
}

#' A depth window of the KA5 subtypes, three-valued
#'
#' Whether the first horizon of a KA5 frame whose carrying part matches
#' `codes` (a code, or a master standing for its combinations) has its top (or
#' bottom) in [from_cm, to_cm), the lower end inclusive and the upper end
#' exclusive (default 4 of the owner's rules): "G at 4 to < 8 dm" is
#' .ka5_window(frame, "G", 40, 80). Depths are read from `gof_cm` (the top of
#' the mineral soil, p. 79; the GOF helper comes with the horizon key). NA
#' when no such horizon is recorded and the description stops above to_cm,
#' or a depth is missing.
#' @noRd
.ka5_window <- function(frame, codes, from_cm = -Inf, to_cm = Inf, edge = c("top", "bottom"),
                        gof_cm = 0) {
  edge <- match.arg(edge)
  hit <- which(!is.na(frame$carrying) & (frame$carrying %in% codes | frame$master %in% codes))
  if (!length(hit)) {
    deepest <- suppressWarnings(max(frame$bottom_cm, na.rm = TRUE))
    if (!is.finite(deepest)) return(NA)
    return(if (deepest - gof_cm >= to_cm) FALSE else NA)
  }
  d <- frame[[paste0(edge, "_cm")]][hit[1L]] - gof_cm
  if (is.na(d)) return(NA)
  d >= from_cm && d < to_cm
}
