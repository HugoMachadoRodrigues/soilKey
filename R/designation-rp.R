# =============================================================================
# v0.9.341 -- the horizon notation of the French Referentiel pedologique 2008
# (AFES; Baize and Girard coord.; Quae, 2009), pp. 1-5, with the codes the
# definitions and chapters add.
#
# A designation is a code of a closed, case-sensitive vocabulary (FE is not
# Fe, SV is not Sv, G is not g), read by the longest match: Snd is the
# silandic S, not Sn + d; And the silandic A, not An + d; Sal the aluminic S,
# not Sa + l; Femp and Fem come before Fe; Kc, Xc, OXc and OXm are codes of
# their own. Before the code, a prefix that is an overlay of the horizon: L
# (ploughed: LA, LBT), Na and Sa before a capital (NaBT, SaS), pi (paleo:
# piRT) and J before a material (JZtc). After it, suffixes read as features,
# by the longest match (cry, cra, cri, sch, noz, nod, tp, ho, zo, cn, ca, do,
# vo, si, then the single letters), some only on the hosts the book lists
# (-a on E and G, -ca on C and the layers, -d on BT and S ...); then digits
# for a sub-horizon (BT1, BT2). A Roman numeral and a space before it mark a
# lithological discontinuity (II S; I is omitted); two codes joined by "-"
# a transition (A-S); by "&" juxtaposed volumes (E&BT).
#
# The notation gives the profile type only when the profile is written in it
# (Hugo's decision Q9, 2026-11-02): declared in site$designation_system
# (a value starting with rp, referentiel or afes), or recognised by one
# symbol only the RP writes or two kinds that it alone usually writes, with
# no symbol only the CPCS 1967, the FAO or the USDA writes and no sign of the
# Russian notation; a profile that mixes them is undecided, and one written
# all in capitals is the RP only when declared. Never from the country: French data use the CPCS, the
# FAO letters and the RP. The CPCS and FAO designations give hints, through
# Annex 6 and the FAO letters, to the identification on the data; never the
# RP code by themselves. The CPCS and Russian readers do not change.
#
# The vocabulary is the catalogue inst/rules/rp2008/horizons.yaml. Nothing
# here stops with an error: a designation that cannot be read is returned
# unread, with a note.
# =============================================================================

# site$designation_system declares the notation by a value that starts with
# rp, referentiel or afes, compared once lower-cased, the accents folded and
# everything but letters and digits dropped ("Referentiel pedologique
# francais" -> referentielpedologiquefrancais; "AFES 2009", "RP08"); the RP
# of 1995 by rp1995 or rp95 (also "RP 1995"), or 1995 after referentiel or
# afes (Hugo's decision 4, 2026-11-02).
.RP_DECLARED_PREFIX <- "^(rp|referentiel|afes)"
.RP_DECLARED_1995 <- "^(rp(19)?95|(referentiel|afes)[a-z]*1995)"

# Values of site$designation_system that declare nothing.
.RP_UNDECLARED <- c("na", "none", "unknown", "nd", "null", "missing", "notrecorded")

# Designations that record nothing, compared as written: ND and Na are codes.
.RP_MISSING <- c("NA", "na", "N/A", "n/a", "nd", "n.d.", "N.D.", "-", "--", "?", "??",
                 "...", "null", "NULL")

# Single letters that may start a transition written without a hyphen (the
# book's examples A-S, S-C, B-C, C-M, Z-S; ACk and AS on p. 127), and the
# families its second part may belong to.
.RP_TRANSITION_FIRST <- c("A", "B", "C", "E", "S")
.RP_TRANSITION_SECOND <- c("A", "B", "C", "E", "S", "M", "R", "D", "Z")

# Prefixes of an overlay, longest first (J, Js and Jp only before a material;
# L only before the families of the ploughed horizons p. 14 names).
.RP_PREFIXES <- c("Na", "Sa", "Js", "Jp", "L", "J")
.RP_L_FAMILIES <- c("A", "E", "S", "O", "H", "BT", "BP")

# The pi of "paleo" and the eszett of BT beta (written in the catalogue).
.RP_PI <- "\u03c0"

.rp_cache <- new.env(parent = emptyenv())

# The catalogue of inst/rules/rp2008/horizons.yaml.
.rp_catalogue <- function() {
  if (is.null(.rp_cache$catalogue)) {
    path <- system.file("rules", "rp2008", "horizons.yaml", package = "soilKey")
    if (!nzchar(path)) rlang::abort("The catalogue of the RP 2008 notation is not installed.")
    .rp_cache$catalogue <- yaml::read_yaml(path)
  }
  .rp_cache$catalogue
}

# The vocabulary the reader works with, built once from the catalogue.
.rp_vocabulary <- function() {
  if (!is.null(.rp_cache$vocabulary)) return(.rp_cache$vocabulary)
  ct <- .rp_catalogue()
  h <- ct$horizons
  fld <- function(l, f) vapply(l, function(e) as.character(e[[f]] %||% NA_character_)[1L], "")
  codes <- fld(h, "code")
  info <- data.frame(code = codes, family = fld(h, "family"), kind = fld(h, "kind"),
                     source = fld(h, "source"), variant_of = fld(h, "variant_of"),
                     reads_as = fld(h, "reads_as"), detection = fld(h, "detection"),
                     domain = fld(h, "domain"), name = fld(h, "name"),
                     name_en = fld(h, "name_en"), stringsAsFactors = FALSE)
  rownames(info) <- codes
  # written forms of a code (BT beta written with a Greek beta)
  written <- unlist(lapply(h, function(e) {
    w <- as.character(unlist(e$written))
    stats::setNames(rep(as.character(e$code), length(w)), w)
  }))
  as_code <- c(stats::setNames(codes, codes), written[!(names(written) %in% codes)])
  # masters for a designation read on its own: all but the transition letter
  masters <- names(as_code)[info[as_code, "source"] != "transition"]
  masters <- masters[order(-nchar(masters), masters)]
  sx <- ct$suffixes
  marks <- fld(sx, "mark")
  hosts <- stats::setNames(lapply(sx, function(e) as.character(unlist(e$hosts))), marks)
  meanings <- stats::setNames(lapply(sx, function(e) {
    m <- e$meanings
    if (is.null(m)) character(0) else stats::setNames(as.character(unlist(m)), names(m))
  }), marks)
  gloss <- stats::setNames(fld(sx, "name_en"), marks)
  wv <- ct$written_variants
  voc <- list(
    info = info,
    as_code = as_code,
    masters = masters,
    transition_masters = c(masters, names(as_code)[info[as_code, "source"] == "transition"]),
    marks = marks[order(-nchar(marks), marks)],
    hosts = hosts,
    meanings = meanings,
    gloss = gloss,
    rewrites = data.frame(from = fld(wv, "from"), to = fld(wv, "to"), note = fld(wv, "note"),
                          stringsAsFactors = FALSE),
    strong = codes[info$detection %in% "strong"],
    likely = codes[info$detection %in% "likely"],
    old = codes[info$source %in% "old"],
    annex6 = ct$annex6,
    fao_hints = ct$fao_hints,
    fao_features = unlist(ct$fao_features)
  )
  .rp_cache$vocabulary <- voc
  voc
}


# --- site$designation_system ------------------------------------------------------

# Fold the accented letters a declaration may carry (e acute and grave, a
# grave, c cedilla ...) to ASCII.
.rp_fold_accents <- function(x) {
  chartr("\u00e9\u00e8\u00ea\u00eb\u00e0\u00e2\u00e4\u00ee\u00ef\u00f4\u00f6\u00fb\u00fc\u00f9\u00e7",
         "eeeeaaaiioouuuc", x)
}

# The declared notation as a key ("Referentiel pedologique 2008" ->
# "referentielpedologique2008"), or NA when the site declares none. Never an
# error, whatever the site is.
.rp_declared_key <- function(site) {
  if (!is.list(site)) return(NA_character_)
  v <- tryCatch(site[["designation_system"]], error = function(e) NULL)
  v <- tryCatch(as.character(unlist(v)), error = function(e) character(0))
  if (!length(v) || is.na(v[1L])) return(NA_character_)
  v <- v[1L]
  if (!validUTF8(v)) {
    # a declaration from a Windows-1252 or Latin-1 file (R\xe9f\xe9rentiel)
    v <- suppressWarnings(iconv(v, "CP1252", "UTF-8"))
    if (is.na(v) || !validUTF8(v)) return(NA_character_)
  }
  k <- gsub("[^a-z0-9]", "", .rp_fold_accents(tolower(v)))
  if (!nzchar(k) || k %in% .RP_UNDECLARED || !grepl("[a-z]", k)) return(NA_character_)
  k
}

# Does a declared key name the RP notation (of 1995)?
.rp_key_is_rp <- function(key) !is.na(key) && grepl(.RP_DECLARED_PREFIX, key)
.rp_key_is_1995 <- function(key) !is.na(key) && grepl(.RP_DECLARED_1995, key)

# Does a value of designation_system declare the RP notation?
.rp_is_declared <- function(ds) {
  .rp_key_is_rp(.rp_declared_key(list(designation_system = ds)))
}


# --- cleaning a designation --------------------------------------------------------

# Characters folded before anything is read: zero-width characters dropped,
# the no-break, thin and narrow spaces to a space, full-width ASCII to ASCII,
# the dashes and the minus sign to a hyphen (Annex 6 prints the en dash: -g;
# the C1 codes of the Windows-1252 dashes too), the capital pi to pi and the
# capital eszett to the eszett.
.rp_fold <- function(x) {
  cp <- utf8ToInt(x)
  if (anyNA(cp)) return(x)
  cp <- cp[!((cp >= 0x200B & cp <= 0x200D) | cp == 0xFEFF)]
  fw <- cp >= 0xFF01 & cp <= 0xFF5E
  cp[fw] <- cp[fw] - 0xFEE0L
  cp[cp %in% c(0x00A0, 0x2009, 0x202F, 0x2007, 0x2002, 0x2003)] <- 0x20L
  cp[cp %in% c(0x2010, 0x2011, 0x2012, 0x2013, 0x2014, 0x2212, 0x0096, 0x0097)] <- 0x2DL
  cp[cp == 0x03A0] <- 0x03C0L
  cp[cp == 0x1E9E] <- 0x00DFL
  intToUtf8(cp)
}

# One designation cleaned: list(s, lith, notes). Text that is not valid
# UTF-8 is read as Windows-1252, else Latin-1 (the code pages of most French
# files: the eszett of BT beta, the en dash). NA, "-" and "?" record
# nothing. A Roman numeral written apart marks a lithological discontinuity
# (II S, p. 4), and one written alone a layer with no horizon written, but
# V and X, which are codes; a prefix written apart is joined to its master
# (Na BT, Sa A, p. 296); "pi" before a capital is the paleo pi; spaces
# around "-", "&", "+" and "/" are dropped (E & BT, A + E); sub-horizon
# digits written apart are joined (BT 1); then a note after a space and a
# trailing "?" are dropped, each noted.
.rp_clean1 <- function(x) {
  out <- list(s = NA_character_, lith = 1L, lith_written = FALSE, notes = character(0))
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
  x <- .rp_fold(x)
  x <- trimws(gsub("[[:space:]]+", " ", x, perl = TRUE))
  if (!nzchar(x)) return(out)
  if (x %in% .RP_MISSING) {
    out$notes <- c(out$notes, sprintf("'%s' records nothing", x))
    return(out)
  }
  rom <- c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X")
  m <- regmatches(x, regexec("^(X|IX|VIII|VII|VI|V|IV|III|II|I) +(.+)$", x))[[1L]]
  if (length(m)) {
    out$lith <- match(m[2L], rom)
    out$lith_written <- TRUE
    x <- m[3L]
  } else if (x %in% setdiff(rom[-1L], c("V", "X"))) {
    # a numeral written alone: a lithological layer with no horizon written
    out$lith <- match(x, rom)
    out$lith_written <- TRUE
    out$s <- ""
    out$notes <- c(out$notes, sprintf("%s: a lithological layer with no horizon written", x))
    return(out)
  }
  if (grepl("^pi(?=[A-Z])", x, perl = TRUE)) {
    x <- sub("^pi", "\u03c0", x)
    out$notes <- c(out$notes, "pi read as the paleo prefix")
  }
  x2 <- gsub(" *([-&+/]) *", "\\1", x, perl = TRUE)
  x2 <- sub("^(Na|Sa|L|\u03c0) +(?=[A-Z])", "\\1", x2, perl = TRUE)
  x2 <- sub("^([^ ]*[A-Za-z\u00df]) ([0-9]+)$", "\\1\\2", x2, perl = TRUE)
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
  if (nzchar(x) && !(x %in% .RP_MISSING)) out$s <- x
  out
}

# The designations of a profile cleaned (.rp_clean1), never an error.
.rp_clean <- function(desig) {
  d <- tryCatch(as.character(unlist(desig)), error = function(e) character(0))
  lapply(d, function(x) tryCatch(.rp_clean1(x), error = function(e)
    list(s = NA_character_, lith = 1L, lith_written = FALSE,
         notes = sprintf("not read: %s", conditionMessage(e)))))
}


# --- one designation ---------------------------------------------------------

# The fields of one row, at their defaults.
.rp_row <- function(written = NA_character_) {
  list(written = written, code = NA_character_, base = NA_character_,
       family = NA_character_, kind = NA_character_, overlay = character(0),
       features = character(0), meanings = character(0), lith = 1L,
       subdiv = NA_character_, transition = FALSE, juxtaposed = FALSE,
       parts = character(0), part_features = character(0), parts_strong = FALSE,
       mark_alone = FALSE, old = FALSE, old_codes = character(0), variant = FALSE,
       known = FALSE, ambiguous = NA_character_, notes = character(0))
}

# The families (and codes) a code answers to, for the hosts of a suffix.
.rp_host_keys <- function(code, voc) {
  i <- voc$info[code, ]
  unique(stats::na.omit(c(code, i$family, i$variant_of)))
}

# The suffixes allowed after a code (all of them when code is NA: a mark
# written alone). A host list names codes: the code itself or the listed
# code it is a variant of (OFr is an OF, Mma an M), as the complete lists of
# the book write them (St, Ct, FSt; BTd, Sd; Ea, Ga, ga; OFnoz, OHzo).
.rp_allowed_marks <- function(code, voc) {
  if (is.na(code)) return(voc$marks)
  i <- voc$info[code, ]
  keys <- unique(stats::na.omit(c(code, i$variant_of)))
  voc$marks[vapply(voc$marks, function(m) {
    h <- voc$hosts[[m]]
    !length(h) || any(keys %in% h)
  }, NA)]
}

# Codes whose own letters say indurated (Kc continuous, Km, OXc, OXm, Fem,
# Femp, Sim, Ym) or cobbly (Xc): -c and -m after them repeat the code.
.RP_INDURATED_CODES <- c("Kc", "Km", "OXc", "OXm", "Fem", "Femp", "Sim", "Ym", "Xc")

# Does a run of suffixes repeat a mark (Sgg, Rcaca) or a letter of the code
# (Kcc, OXmc)?
.rp_repeats <- function(code, dec) {
  anyDuplicated(dec) > 0L || (code %in% .RP_INDURATED_CODES && any(c("c", "m") %in% dec))
}

# Every way to write `rem` as a run of the marks `allowed`, the longest match
# first (marks are tried longest first at each position).
.rp_split_marks <- function(rem, allowed, limit = 20L) {
  if (!nzchar(rem)) return(list(character(0)))
  out <- list()
  walk <- function(pos, acc) {
    if (length(out) >= limit) return(invisible())
    if (pos > nchar(rem)) {
      out[[length(out) + 1L]] <<- acc
      return(invisible())
    }
    for (m in allowed) {
      n <- nchar(m)
      if (substr(rem, pos, pos + n - 1L) == m) walk(pos + n, c(acc, m))
    }
  }
  walk(1L, character(0))
  out
}

# The readings of a string with no lithological prefix, transition or
# juxtaposition: a master (with prefixes) and its suffixes. A list of
# list(prefix, master, features, alt), the chosen first.
.rp_core_readings <- function(x, voc, masters = voc$masters, allow_repeats = FALSE) {
  out <- list()
  direct <- function(y, pre) {
    res <- list()
    for (mm in masters[startsWith(y, masters)]) {
      rem <- substring(y, nchar(mm) + 1L)
      code <- voc$as_code[[mm]]
      for (dec in .rp_split_marks(rem, .rp_allowed_marks(code, voc))) {
        if (!allow_repeats && .rp_repeats(code, dec)) next
        res[[length(res) + 1L]] <- list(prefix = pre, master = mm, code = code, features = dec)
      }
    }
    res
  }
  pi <- startsWith(x, .RP_PI)
  y <- if (pi) substring(x, 2L) else x
  pre0 <- if (pi) .RP_PI else character(0)
  if (!nzchar(y)) return(out)
  out <- direct(y, pre0)
  for (p in .RP_PREFIXES) {
    n <- nchar(p)
    if (nchar(y) > n && startsWith(y, p) && grepl("^[A-Z]", substring(y, n + 1L))) {
      inner <- direct(substring(y, n + 1L), c(pre0, p))
      if (p %in% c("J", "Js", "Jp")) {
        inner <- Filter(function(r) voc$info[r$code, "family"] %in% "Z", inner)
      }
      if (p == "L") {
        # ploughed A, E, S, O, H, BT or BP (LA, LE, LS, LO, LH, LBT, LBP, p. 14)
        inner <- Filter(function(r) voc$info[r$code, "family"] %in% .RP_L_FAMILIES, inner)
      }
      out <- c(out, inner)
    }
  }
  if (!length(out)) return(out)
  # the longest master, then the fewest suffixes, then no prefix
  key <- vapply(out, function(r) sprintf("%03d%03d%03d", 100L - nchar(r$master),
                                         length(r$features), length(r$prefix)), "")
  out[order(key)]
}

# Fill a row from the chosen reading of .rp_core_readings().
.rp_fill <- function(r, rd, alts, voc) {
  code <- rd$code
  i <- voc$info[code, ]
  if (rd$master != code) {
    r$notes <- c(r$notes, sprintf("%s read as %s", rd$master, code))
  }
  if (i$source %in% "old") {
    r$old <- TRUE
    r$old_codes <- code
    r$notes <- c(r$notes, sprintf("%s: an old code, read as %s", code, i$reads_as))
    code <- i$reads_as
    i <- voc$info[code, ]
  }
  r$code <- code
  r$base <- if (!is.na(i$variant_of)) i$variant_of else code
  r$family <- i$family
  r$kind <- i$kind
  r$overlay <- rd$prefix
  r$features <- rd$features
  r$meanings <- vapply(rd$features, function(m) {
    mn <- voc$meanings[[m]]
    hit <- intersect(.rp_host_keys(code, voc), names(mn))
    if (length(hit)) unname(mn[[hit[1L]]]) else unname(voc$gloss[[m]])
  }, "", USE.NAMES = FALSE)
  if ("j" %in% rd$features && i$family %in% "FS") {
    r$notes <- c(r$notes, "-j is not written on FS (p. 180)")
  }
  if ("ho" %in% rd$features && i$family %in% "O") {
    r$notes <- c(r$notes, "-ho is not written on O horizons (Annex 4)")
  }
  if (length(alts) > 1L) {
    other <- vapply(alts[-1L], function(a) paste(c(a$prefix, a$master, a$features), collapse = " + "), "")
    r$ambiguous <- sprintf("%s also reads as %s; read by the longest match", r$written,
                           paste(unique(other), collapse = "; "))
  }
  r$known <- TRUE
  r
}

# A designation with no lithological prefix, read as one code; NULL when it
# does not read.
.rp_read_simple <- function(x, voc, masters = voc$masters) {
  r <- .rp_row(x)
  # sub-horizons are numbered from 1 (A1, BT2, S12): A0, A00 or BT0 are none
  sub <- regmatches(x, regexpr("[1-9][0-9]*$", x))
  core <- if (length(sub)) substr(x, 1L, nchar(x) - nchar(sub)) else x
  if (!nzchar(core)) return(NULL)
  rd <- .rp_core_readings(core, voc, masters)
  if (!length(rd)) return(NULL)
  r <- .rp_fill(r, rd[[1L]], rd, voc)
  if (length(sub)) r$subdiv <- sub
  r
}

# A transition (A-S) or a juxtaposition (E&BT): each part one code, the
# letter B allowed in a transition (B-C).
.rp_read_compound <- function(x, sep, voc) {
  parts <- strsplit(x, sep, fixed = TRUE)[[1L]]
  if (length(parts) < 2L || any(!nzchar(parts))) return(NULL)
  ms <- if (sep == "-") voc$transition_masters else voc$masters
  ms <- ms[order(-nchar(ms), ms)]
  rows <- lapply(parts, .rp_read_simple, voc = voc, masters = ms)
  if (any(vapply(rows, is.null, NA))) return(NULL)
  r <- .rp_row(x)
  r$parts <- vapply(rows, function(z) z$code, "")
  r$part_features <- vapply(rows, function(z) paste(z$features, collapse = ","), "")
  r$parts_strong <- any(vapply(rows, function(z) .rp_row_strength(z, voc, FALSE) %in% "strong", NA))
  r$features <- unique(unlist(lapply(rows, function(z) z$features)))
  r$meanings <- unname(vapply(r$features, function(m) voc$gloss[[m]], ""))
  r$overlay <- unique(unlist(lapply(rows, function(z) z$overlay)))
  r$old_codes <- unique(unlist(lapply(rows, function(z) z$old_codes)))
  r$old <- length(r$old_codes) > 0L
  sd <- unlist(lapply(rows, function(z) z$subdiv))
  sd <- sd[!is.na(sd)]
  if (length(sd)) r$subdiv <- sd[length(sd)]
  r$notes <- unlist(lapply(rows, function(z) z$notes))
  r$known <- TRUE
  if (sep == "-") {
    r$transition <- TRUE
    r$notes <- c(r$notes, sprintf("a transition (%s)", paste(r$parts, collapse = " to ")))
  } else {
    r$juxtaposed <- TRUE
    r$notes <- c(r$notes, sprintf("juxtaposed volumes of %s", paste(r$parts, collapse = " and ")))
  }
  r
}

# A transition written without the hyphen (BC, AS, ACk): a single letter A,
# B, C, E or S and a code of the families the book's transitions join.
.rp_read_glued <- function(x, voc) {
  first <- substr(x, 1L, 1L)
  if (!(first %in% .RP_TRANSITION_FIRST) || nchar(x) < 2L) return(NULL)
  rest <- substring(x, 2L)
  ms <- voc$transition_masters
  ms <- ms[order(-nchar(ms), ms)]
  second <- .rp_read_simple(rest, voc, masters = ms)
  if (is.null(second) || length(second$overlay) ||
      !(voc$info[second$code, "family"] %in% .RP_TRANSITION_SECOND)) return(NULL)
  r <- .rp_read_compound(paste0(first, "-", rest), "-", voc)
  if (is.null(r)) return(NULL)
  r$written <- x
  r$notes <- c(r$notes, "written without the hyphen")
  r
}

# Apply the accepted writings of the book (Lav, Vna, OFz ...).
.rp_rewrite <- function(x, voc) {
  notes <- character(0)
  rw <- voc$rewrites
  for (k in seq_len(nrow(rw))) {
    if (grepl(rw$from[k], x, perl = TRUE)) {
      y <- sub(rw$from[k], rw$to[k], x, perl = TRUE)
      notes <- c(notes, sprintf("%s read as %s: %s", x, y, rw$note[k]))
      x <- y
    }
  }
  list(s = x, notes = notes, changed = length(notes) > 0L)
}

# One cleaned designation (the string and its lithological layer); never an
# error.
.rp_read_clean <- function(cl, voc) {
  r <- tryCatch(.rp_read_clean_(cl, voc), error = function(e) {
    z <- .rp_row(cl$s)
    z$notes <- c(cl$notes, sprintf("not read: %s", conditionMessage(e)))
    z
  })
  if (!r$known && !isTRUE(cl$lith_written)) r$lith <- 1L
  r
}

.rp_read_clean_ <- function(cl, voc) {
  s <- cl$s
  r <- .rp_row(s)
  r$notes <- cl$notes
  r$lith <- as.integer(cl$lith %||% 1L)
  if (is.na(s)) return(r)
  if (!nzchar(s)) {
    # a lithological numeral written alone (Z/II, p. 91)
    r$written <- NA_character_
    r$known <- isTRUE(cl$lith_written)
    return(r)
  }
  rw <- .rp_rewrite(s, voc)
  x <- rw$s
  notes <- c(cl$notes, rw$notes)
  # a mark written alone (H and -*, p. 140)
  if (startsWith(x, "-")) {
    dec <- .rp_split_marks(substring(x, 2L), voc$marks)
    if (length(dec) && length(dec[[1L]])) {
      r$features <- dec[[1L]]
      r$meanings <- unname(vapply(r$features, function(m) voc$gloss[[m]], ""))
      r$mark_alone <- TRUE
      r$known <- TRUE
      r$notes <- c(notes, sprintf("%s: a mark written alone, on a horizon not named", s))
      return(r)
    }
    r$notes <- c(notes, sprintf("'%s' is no mark of the RP", s))
    return(r)
  }
  z <- NULL
  if (grepl("&", x, fixed = TRUE)) z <- .rp_read_compound(x, "&", voc)
  else if (grepl("+", x, fixed = TRUE)) {
    z <- .rp_read_compound(x, "+", voc)
    if (!is.null(z)) z$notes <- c(z$notes, "'+' read as juxtaposed volumes (E + A, p. 315)")
  }
  else if (grepl("-", x, fixed = TRUE)) z <- .rp_read_compound(x, "-", voc)
  else if (grepl("/", x, fixed = TRUE)) {
    parts <- strsplit(x, "/", fixed = TRUE)[[1L]]
    rows <- lapply(parts, function(p) if (nzchar(p)) .rp_read_simple(p, voc) else NULL)
    if (length(parts) > 1L && !any(vapply(rows, is.null, NA))) {
      codes <- vapply(rows, function(q) q$code, "")
      if (length(unique(codes)) == 1L) {
        z <- rows[[1L]]
        z$subdiv <- NA_character_
        z$features <- unique(unlist(lapply(rows, function(q) q$features)))
        z$notes <- c(z$notes, sprintf("%s: parts of one horizon, read as %s", x, codes[1L]))
      } else {
        z <- .rp_read_compound(gsub("/", "-", x, fixed = TRUE), "-", voc)
        if (!is.null(z)) z$notes <- c(z$notes, "written with '/'")
      }
    }
  } else {
    z <- .rp_read_simple(x, voc)
    if (is.null(z)) z <- .rp_read_glued(x, voc)
    if (is.null(z) && !isTRUE(cl$lith_written)) {
      # a numeral run into the code (IIRT, p. 234): II to IX but V and X,
      # which are codes; an Arabic numeral as the FAO writes it
      m <- regmatches(x, regexec("^(IX|VIII|VII|VI|IV|III|II|[2-9])([A-Z\u03c0].*)$", x))[[1L]]
      if (length(m)) {
        z <- .rp_read_simple(m[3L], voc)
        if (is.null(z)) z <- .rp_read_glued(m[3L], voc)
        if (!is.null(z)) {
          rom <- c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX")
          z$lith <- if (grepl("^[0-9]$", m[2L])) as.integer(m[2L]) else match(m[2L], rom)
          z$notes <- c(z$notes, sprintf("%s: the numeral %s written without a space%s", x, m[2L],
                                        if (grepl("^[0-9]$", m[2L])) " (an Arabic numeral, as the FAO writes it)" else ""))
        }
      }
    }
  }
  if (is.null(z)) {
    core <- sub("[1-9][0-9]*$", "", x)
    rep <- nzchar(core) && length(.rp_core_readings(core, voc, allow_repeats = TRUE)) > 0L
    r$notes <- c(notes, if (startsWith(x, "B") && grepl("^B[0-9]*$", x))
                          "B is written only in transitions (B-C)"
                        else if (rep) sprintf("'%s': a mark written twice, or one the code already says", s)
                        else sprintf("'%s' is no code of the RP notation", s))
    return(r)
  }
  if (is.na(z$lith) || z$lith == 1L) z$lith <- r$lith
  z$written <- s
  z$variant <- rw$changed
  z$notes <- c(notes, z$notes)
  z
}

#' Read one designation in the notation of the Referentiel pedologique 2008
#'
#' @param d A designation as recorded.
#' @param rp1995 TRUE in a profile written in the RP notation of 1995, where
#'   FE is the ferric horizon (now Fe).
#' @return A list: \code{written} (the designation once cleaned),
#'   \code{code} (the code read, a master with its variant letters: Sca, BTss,
#'   OXc, Hm, OFr; NA for a transition, a juxtaposition, a mark alone or an
#'   unread designation), \code{base} (the listed code of a variant: OF for
#'   OFr), \code{family} (A, BT, O, S, the layer or the material),
#'   \code{kind} (horizon, layer, material), \code{overlay} (L, Na, Sa, pi,
#'   J), \code{features} (the suffixes) and \code{meanings} (their reading on
#'   that host), \code{lith} (1 with no Roman numeral), \code{subdiv},
#'   \code{transition} and \code{juxtaposed} with \code{parts}, \code{mark_alone},
#'   \code{old} (an old code read as the 2008 one), \code{variant} (a writing
#'   of the book read as another code), \code{known}, \code{ambiguous} and
#'   \code{notes}.
#' @noRd
.rp_parse_one <- function(d, rp1995 = FALSE) {
  voc <- .rp_vocabulary()
  cl <- .rp_clean(d)[[1L]] %||% list(s = NA_character_, lith = 1L, lith_written = FALSE, notes = character(0))
  r <- .rp_read_clean(cl, voc)
  if (isTRUE(rp1995)) r <- .rp_apply_1995(r)
  r
}

# In the RP of 1995, FE was the ferric horizon (p. 1): read as Fe.
.rp_apply_1995 <- function(r) {
  fe <- function(code) if (identical(code, "FE")) "Fe" else code
  if (identical(r$code, "FE")) {
    r$code <- "Fe"
    r$base <- "Fe"
    r$family <- "Fe"
    r$old <- TRUE
    r$notes <- c(r$notes, "FE read as Fe, the ferric horizon of the RP of 1995")
  }
  if (length(r$parts) && "FE" %in% r$parts) {
    r$parts <- vapply(r$parts, fe, "", USE.NAMES = FALSE)
    r$notes <- c(r$notes, "FE read as Fe, the ferric horizon of the RP of 1995")
  }
  r
}


# --- a sequence of the book --------------------------------------------------

# Split s on `sep` outside parentheses.
.rp_split_outside <- function(s, sep) {
  ch <- strsplit(s, "", fixed = TRUE)[[1L]]
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

# Is s wholly inside one pair of parentheses?
.rp_wrapped <- function(s) {
  if (!grepl("^\\(.*\\)$", s)) return(FALSE)
  inner <- substr(s, 2L, nchar(s) - 1L)
  depth <- 0L
  for (ch in strsplit(inner, "", fixed = TRUE)[[1L]]) {
    if (ch == "(") depth <- depth + 1L
    if (ch == ")") { depth <- depth - 1L; if (depth < 0L) return(FALSE) }
  }
  depth == 0L
}

#' Read a sequence of reference horizons as the book writes it
#'
#' Horizons from top to bottom separated by "/" ("A/Sal/C|R"); alternatives
#' in one position separated by "|" (the book's "ou"); an optional or
#' discontinuous horizon in parentheses ("(OLn)/A", "(A|Ae|Anod)/ND"); "//" a
#' discontinuity between two horizons (O//A, p. 352). Every designation is
#' read in the RP notation; FE is read as Fe when the sequence holds FEm, FEmp
#' or Ahs.
#' @return A list: \code{slots} (each a list of \code{optional} and
#'   \code{alternatives}, rows of .rp_parse_one() with an \code{optional}
#'   flag of their own), \code{unknown} (the designations not read),
#'   \code{notes}.
#' @noRd
.rp_read_sequence <- function(s) {
  voc <- .rp_vocabulary()
  notes <- character(0)
  s <- .rp_fold(trimws(as.character(s)[1L]))
  if (grepl("//", s, fixed = TRUE)) {
    notes <- "a discontinuity (//) between two horizons"
    s <- gsub("/{2,}", "/", s)
  }
  slots <- lapply(.rp_split_outside(s, "/"), function(slot) {
    slot <- trimws(slot)
    opt <- .rp_wrapped(slot)
    if (opt) slot <- substr(slot, 2L, nchar(slot) - 1L)
    alts <- lapply(.rp_split_outside(slot, "|"), function(a) {
      a <- trimws(a)
      o <- .rp_wrapped(a)
      if (o) a <- substr(a, 2L, nchar(a) - 1L)
      r <- .rp_read_clean(.rp_clean1(a), voc)
      r$optional <- o
      r
    })
    list(optional = opt, alternatives = alts)
  })
  rows <- unlist(lapply(slots, `[[`, "alternatives"), recursive = FALSE)
  old <- vapply(rows, .rp_old_1995, NA)
  if (any(old)) {
    slots <- lapply(slots, function(sl) {
      sl$alternatives <- lapply(sl$alternatives, .rp_apply_1995)
      sl
    })
    rows <- unlist(lapply(slots, `[[`, "alternatives"), recursive = FALSE)
  }
  known <- vapply(rows, function(r) isTRUE(r$known), NA)
  unknown <- vapply(rows[!known], function(r) as.character(r$written %||% NA_character_)[1L], "")
  list(slots = slots, unknown = unknown, notes = notes)
}


# --- the notation of a profile -------------------------------------------------

# Old codes that are the RP of 1995 (Ahs, FEm, FEmp): signs of the notation
# that make FE the ferric Fe. Ap (formerly L) is neither.
.RP_OLD_1995 <- c("Ahs", "FEm", "FEmp")
.rp_old_1995 <- function(r) any(r$old_codes %in% .RP_OLD_1995)

# -ho is a sign of the notation only on the codes the book writes it on,
# with nothing after it (Aho, Acaho, Sho, Zho, THho, Xho); elsewhere it is
# likely (an FAO run such as Ahoc reads A + ho + c).
.RP_HO_HOSTS <- c("A", "Aca", "S", "Z", "TH", "X")

# The class of a row for the recognition of the notation: strong, likely,
# or NA (ambiguous, or not read). lower_elsewhere: another designation of the
# profile has a lower-case letter.
.rp_row_strength <- function(r, voc, lower_elsewhere) {
  if (!isTRUE(r$known)) return(NA_character_)
  if (isTRUE(r$juxtaposed) || isTRUE(r$transition)) {
    # E&BT (Ea&BT, Eg&BTg) or a part that is a sign itself; not L&F or F&H
    if (isTRUE(r$parts_strong)) return("strong")
    if (isTRUE(r$juxtaposed) && identical(r$parts, c("E", "BT"))) return("strong")
    return(NA_character_)
  }
  if (r$mark_alone) return(NA_character_)
  if (any(r$overlay %in% c(.RP_PI, "Na", "Sa", "J", "Js", "Jp"))) return("strong")
  if ("L" %in% r$overlay && !is.na(r$code)) return("strong")
  if (any(r$features %in% c("nod", "noz", "zo", "cra", "cri", "do", "sch", "si", "vo"))) return("strong")
  code <- r$code
  if (identical(r$features, "ho") && code %in% .RP_HO_HOSTS) return("strong")
  if (.rp_old_1995(r)) return("strong")
  if (is.na(code)) return(NA_character_)
  if (code %in% voc$strong) return("strong")
  if (!is.na(.rp_likely_kind(r, voc, lower_elsewhere))) return("likely")
  NA_character_
}

# The kind of a likely symbol, so that two likely symbols are two kinds
# (Hf and Hm, K and Cca), not one written twice or with other suffixes (Cca
# and II Cca, BTg and BTgd). A lone g (the redox horizon written alone),
# -ho beyond its own hosts and the frozen * are likely (Hugo's decisions,
# 2026-11-02).
.rp_likely_kind <- function(r, voc, lower_elsewhere) {
  code <- r$code
  if (!isTRUE(r$known) || is.na(code)) return(NA_character_)
  if (code %in% voc$likely) return(code)
  if (code == "g") return("g")
  if ("ho" %in% r$features) return("ho")
  if ("*" %in% r$features) return("*")
  if (code == "S" && any(c("g", "t", "k", "h") %in% r$features)) {
    return(paste0("S", intersect(c("g", "t", "k", "h"), r$features)[1L]))
  }
  if (code == "C" && "ca" %in% r$features) return("Cca")
  if (code == "BT" && length(r$features) && lower_elsewhere) return("BT with a suffix")
  NA_character_
}

# The CPCS-only symbols of a designation (the cleaned string), given the
# next designation of the profile: A00 and A0 (Aoo, Ao, with F or H), (A),
# (B), an iron fe, Can, and A2 used as an E (written with a suffix after the
# digit, A2g, or directly over a B of the CPCS: (B), B, B2, Bt ...). Cs is
# not one: C with -s is an RP writing.
.rp_cpcs_only <- function(s, next_s) {
  if (is.na(s)) return(FALSE)
  x <- sub("^([IVX]+|[0-9]+) ?(?=[A-Z(])", "", s, perl = TRUE)
  if (grepl("^(A00|Aoo|AOO|A0|Ao)(F|H)?[0-9]*$", x)) return(TRUE)
  if (grepl("^\\((A|B)\\)", x)) return(TRUE)
  if (grepl("[A-Z0-9)]fe", x)) return(TRUE)
  if (grepl("Can", x, fixed = TRUE)) return(TRUE)
  if (grepl("^A2[0-9]*[a-z]", x)) return(TRUE)
  if (grepl("^A2[0-9]*$", x) && !is.na(next_s) &&
      grepl("^([IVX]+ ?|[0-9])?(\\(B\\)|B([0-9]|[a-z]|$))", next_s)) return(TRUE)
  FALSE
}

# The FAO (2006) and USDA symbols the RP never writes (Hugo's decision 1,
# 2026-11-02): a B with a lower-case suffix (Bw, Bs, Bg, Bhs, Bt; the RP
# writes B only in transitions, BC), Oh, Oi, Oe, Oa, Cr, AB or BA (an A-B
# transition the RP does not write: BT and BP are its B horizons), and an
# Arabic numeral of a lithological discontinuity (2Cg, 3Bt).
.rp_fao_only <- function(s) {
  if (is.na(s) || !nzchar(s)) return(FALSE)
  if (grepl("^[2-9][A-Z]", s)) return(TRUE)
  x <- sub("^([IVX]+) ?(?=[A-Z])", "", s, perl = TRUE)
  grepl("^(B[a-z]|O[hiea]|Cr|AB([0-9a-z]|$)|BA([0-9a-z]|$))", x)
}

# The decision for a profile, from its cleaned designations and their reading.
.rp_decide <- function(cl, rows, site, desig) {
  voc <- .rp_vocabulary()
  s <- vapply(cl, function(z) z$s %||% NA_character_, "")
  key <- .rp_declared_key(site)
  rp1995 <- any(vapply(rows, .rp_old_1995, NA)) || .rp_key_is_1995(key)
  out <- list(ok = FALSE, status = "none", why = "", rp1995 = rp1995,
              strong = character(0), likely = character(0), cpcs = character(0),
              fao = character(0), russian = FALSE, capitals = FALSE)
  if (!is.na(key)) {
    raw <- as.character(unlist(site[["designation_system"]]))[1L]
    out$ok <- .rp_key_is_rp(key)
    out$status <- if (out$ok) "declared" else "other"
    out$why <- sprintf("site$designation_system = '%s'", raw)
    if (!out$ok) out$rp1995 <- FALSE
    return(out)
  }
  present <- !is.na(s) & nzchar(s)
  if (!any(present)) {
    out$why <- "no designation"
    out$rp1995 <- FALSE
    return(out)
  }
  low <- grepl("\\p{Ll}", s, perl = TRUE) & present
  st <- vapply(seq_along(rows), function(i) {
    if (!present[i]) return(NA_character_)
    .rp_row_strength(rows[[i]], voc, any(low[-i]))
  }, "")
  out$strong <- unique(s[st %in% "strong"])
  out$likely <- unique(s[st %in% "likely"])
  kinds <- unique(vapply(seq_along(rows), function(i) {
    if (!(st[i] %in% "likely")) return(NA_character_)
    .rp_likely_kind(rows[[i]], voc, any(low[-i]))
  }, ""))
  kinds <- kinds[!is.na(kinds)]
  nxt <- c(s[-1L], NA_character_)
  cp <- vapply(seq_along(s), function(i) .rp_cpcs_only(s[i], nxt[i]), NA)
  out$cpcs <- unique(s[cp])
  fo <- vapply(s, .rp_fao_only, NA, USE.NAMES = FALSE)
  out$fao <- unique(s[fo])
  ru <- tryCatch(isTRUE(.kidpr_notation(desig, list())$ok), error = function(e) FALSE)
  out$russian <- ru
  rp_ev <- length(out$strong) >= 1L || length(kinds) >= 2L
  ev <- c(out$strong, out$likely)
  # a table written all in capitals has lost the case of its letters: only a
  # declaration recognises the RP there (Hugo's decision 2, 2026-11-02)
  out$capitals <- !any(low)
  if (rp_ev && out$capitals) {
    out$status <- "capitals"
    out$why <- sprintf("%s: a profile written all in capitals, where only a declaration recognises the RP notation",
                       paste(ev, collapse = ", "))
  } else if (rp_ev && (length(out$cpcs) || ru || length(out$fao))) {
    out$ok <- NA
    out$status <- "mixed"
    out$why <- sprintf("undecided notation: %s written only in the RP notation, and %s",
                       paste(ev, collapse = ", "),
                       paste(c(if (length(out$cpcs)) sprintf("%s only in the CPCS notation",
                                                             paste(out$cpcs, collapse = ", ")),
                               if (length(out$fao)) sprintf("%s only in the FAO or USDA notation",
                                                            paste(out$fao, collapse = ", ")),
                               if (ru) "signs of the Russian notation"), collapse = " and "))
  } else if (rp_ev) {
    out$ok <- TRUE
    out$status <- "recognised"
    out$why <- sprintf("%s: %s", paste(ev, collapse = ", "),
                       if (length(out$strong)) "written only in the RP notation"
                       else "two symbols the RP notation alone usually writes")
  } else {
    out$status <- if (length(out$cpcs)) "cpcs" else if (ru) "russian" else "none"
    # the reason is the missing RP symbols, whatever else the profile holds
    out$why <- if (length(out$likely)) sprintf("%s: one kind of symbol the RP usually writes is not enough",
                                               paste(out$likely, collapse = ", "))
               else if (ru) "no symbol only the RP notation writes; signs of the Russian notation"
               else "no symbol only the RP notation writes (the country is not read)"
  }
  if (!isTRUE(out$ok)) out$rp1995 <- FALSE
  out
}

#' Is a profile written in the notation of the Referentiel pedologique 2008?
#'
#' In order: site$designation_system decides when it names a notation (a
#' value that starts with rp, referentiel or afes is the RP, rp1995 or rp95
#' the RP of 1995; any other notation is not the RP). Otherwise the profile is
#' in the RP notation when one designation is a symbol only the RP writes
#' (OL, OF, OH, BP, LA, SV, FS, OX, RT, FE, Sca, Sal, Aca, Ach, Aso, Kc, Km,
#' Js, Ys, Ztc, Mli, BT beta, a pi, Na or Sa before a master, E&BT, Aho,
#' Anod, Ccra, Rcri ...) or two kinds it alone usually writes (Fe, K, Si, Hf,
#' Hm, Hs, X, Y, U, V, Sn, ND, a lone g, Sg, St, Sk, Sh, Cca, -ho elsewhere,
#' the frozen *, a BT with a lower-case suffix where another designation has
#' lower case), and no designation is a symbol only the CPCS 1967 writes
#' (A00, A0, (A), (B), Bfe, Can, an A2 used as E) or only the FAO and USDA
#' write (a B with a lower-case suffix, Oh, Oi, Oe, Oa, Cr, AB, BA, an Arabic
#' numeral of discontinuity), and the profile is not in the Russian notation.
#' A profile with both is undecided (ok NA). A profile written all in
#' capitals is the RP only when declared. The country is never read. FE is
#' read as Fe in a profile declared rp1995 or written with FEm, FEmp or Ahs.
#' @return A list: \code{ok} (TRUE, FALSE or NA), \code{status} (declared,
#'   recognised, mixed, capitals, other, cpcs, russian, none), \code{why},
#'   \code{rp1995}, \code{strong}, \code{likely}, \code{cpcs}, \code{fao}
#'   (the symbols found), \code{russian} and \code{capitals}.
#' @noRd
.rp_notation <- function(desig, site = NULL) {
  voc <- .rp_vocabulary()
  cl <- .rp_clean(desig)
  rows <- lapply(cl, .rp_read_clean, voc = voc)
  .rp_decide(cl, rows, site, tryCatch(as.character(unlist(desig)), error = function(e) character(0)))
}


# --- hints from other notations --------------------------------------------------

#' What a CPCS (1967) or FAO (2006) designation suggests to the
#' identification on the data: candidate RP codes and features, never the RP
#' code (Hugo's decision Q9). The CPCS symbols through Annex 6 (p. 393), the
#' FAO letters through soilKey's table (horizons.yaml); cpcs_first when the
#' profile is in the CPCS notation, otherwise the FAO letters first; the
#' other table when the first gives nothing.
#' @return list(hint, hint_features, hint_source, notes)
#' @noRd
.rp_hint <- function(s, cpcs_first = FALSE) {
  voc <- .rp_vocabulary()
  out <- list(hint = character(0), hint_features = character(0), hint_source = NA_character_,
              notes = character(0))
  if (is.na(s) || !nzchar(s)) return(out)
  a6 <- function() {
    sym <- tryCatch(.cpcs_symbol(s), error = function(e) NA_character_)
    if (is.na(sym)) return(NULL)
    sym <- sub("^([IVX]+|[0-9]+)(?=[A-Z(])", "", sym, perl = TRUE)
    codes <- character(0); fe <- character(0); hit <- character(0)
    first <- TRUE
    for (row in voc$annex6) {
      all <- isTRUE(row$all)
      if (!all && !first) next
      if (grepl(row$match, sym, perl = TRUE)) {
        codes <- c(codes, as.character(unlist(row$rp)))
        fe <- c(fe, as.character(unlist(row$features)))
        hit <- c(hit, row$cpcs)
        if (!all) first <- FALSE
      }
    }
    if (!length(codes)) return(NULL)
    slips <- unlist(lapply(voc$annex6, function(row) if (row$cpcs %in% hit) row$slips))
    list(hint = unique(codes), hint_features = unique(fe), hint_source = "Annex 6 (CPCS 1967)",
         notes = c(sprintf("%s: Annex 6 gives %s", s, paste(unique(codes), collapse = ", ")),
                   if (length(slips)) sprintf("%s: a slip of Annex 6, not an RP code",
                                              paste(unique(slips), collapse = ", "))))
  }
  fao <- function() {
    p <- tryCatch(.parse_designation_hscs1(s), error = function(e) NULL)
    if (is.null(p)) return(NULL)
    lt <- unique(c(p$strong, p$weak))
    if (grepl("@", s, fixed = TRUE)) lt <- c(lt, "@")
    ms <- unique(c(p$master, if (nzchar(p$master2)) p$master2))
    codes <- character(0)
    for (m in ms) {
      rows <- Filter(function(r) identical(r$master, m), voc$fao_hints)
      spec <- Filter(function(r) length(r$with) && all(as.character(unlist(r$with)) %in% lt), rows)
      use <- if (length(spec)) spec else Filter(function(r) !length(r$with), rows)
      codes <- c(codes, unlist(lapply(use, function(r) as.character(unlist(r$rp)))))
    }
    fe <- unname(voc$fao_features[intersect(names(voc$fao_features), lt)])
    if (!length(codes) && !length(fe)) return(NULL)
    list(hint = unique(codes), hint_features = unique(fe), hint_source = "FAO letters",
         notes = sprintf("%s: the FAO letters suggest %s", s,
                         paste(c(unique(codes), if (length(fe)) paste0("-", fe)), collapse = ", ")))
  }
  if (.rp_cpcs_only(s, NA_character_)) cpcs_first <- TRUE
  res <- if (cpcs_first) a6() %||% fao() else fao() %||% a6()
  res %||% list(hint = character(0), hint_features = character(0), hint_source = NA_character_,
                notes = sprintf("'%s': no hint", s))
}


# --- a whole profile -------------------------------------------------------------

#' All the designations of a profile, read in the RP 2008 notation
#'
#' The profile's notation is decided first (.rp_notation()). In the RP
#' notation every designation is read (.rp_parse_one()), FE as Fe in an RP
#' 1995 profile, and a row it cannot read gets the hints of the FAO letters
#' or Annex 6. In another notation, or an undecided one, no designation is
#' read as an RP code: every row gets the hints, and the RP reading of the
#' designation is kept in \code{read_as_rp} for the evidence. Each
#' designation is cleaned and read once; nothing stops with an error.
#' @return A list of vectors, one element per designation (as
#'   .kidpr_designations()): the fields of .rp_parse_one() (list columns for
#'   overlay, features, meanings, parts and notes), \code{strength},
#'   \code{read_as_rp}, \code{hint}, \code{hint_features}, \code{hint_source},
#'   then \code{rp} (TRUE, FALSE or NA), \code{status}, \code{why},
#'   \code{rp1995} and \code{any_known}.
#' @noRd
.rp_designations <- function(desig, site = NULL) {
  voc <- .rp_vocabulary()
  d <- tryCatch(as.character(unlist(desig)), error = function(e) character(0))
  cl <- .rp_clean(d)
  rows <- lapply(cl, .rp_read_clean, voc = voc)
  nt <- .rp_decide(cl, rows, site, d)
  s <- vapply(cl, function(z) z$s %||% NA_character_, "")
  low <- grepl("\\p{Ll}", s, perl = TRUE) & !is.na(s)
  strength <- vapply(seq_along(rows), function(i)
    .rp_row_strength(rows[[i]], voc, any(low[-i])), "")
  cpcs <- tryCatch(isTRUE(.cpcs_notation(d, site)), error = function(e) FALSE)
  rows <- lapply(seq_along(rows), function(i) {
    z <- rows[[i]]
    z$strength <- strength[i]
    z$read_as_rp <- z$code
    z$hint <- character(0); z$hint_features <- character(0); z$hint_source <- NA_character_
    if (isTRUE(nt$ok)) {
      if (nt$rp1995) z <- .rp_apply_1995(z)
      if (!z$known && !is.na(z$written) && nzchar(z$written)) {
        h <- .rp_hint(z$written, cpcs_first = cpcs)
        z[c("hint", "hint_features", "hint_source")] <- h[c("hint", "hint_features", "hint_source")]
        z$notes <- unique(c(z$notes, h$notes))
      }
    } else {
      w <- cl[[i]]$s %||% NA_character_
      keep <- z
      z <- .rp_row(w)
      z$lith <- keep$lith
      z$read_as_rp <- keep$code
      z$strength <- strength[i]
      z$notes <- c(keep$notes[!grepl("is no code of the RP", keep$notes)],
                   if (is.na(nt$ok) && keep$known && !is.na(keep$code))
                     sprintf("read as %s in the RP notation, which the profile may not be in", keep$code))
      if (!is.na(w) && nzchar(w)) {
        h <- .rp_hint(w, cpcs_first = cpcs)
        z[c("hint", "hint_features", "hint_source")] <- h[c("hint", "hint_features", "hint_source")]
        z$notes <- unique(c(z$notes, h$notes))
      }
    }
    z
  })
  proto <- c(.rp_row(), list(strength = NA_character_, read_as_rp = NA_character_,
                             hint = character(0), hint_features = character(0),
                             hint_source = NA_character_))
  lists <- c("overlay", "features", "meanings", "parts", "notes", "hint", "hint_features")
  out <- lapply(names(proto), function(f) {
    v <- lapply(rows, `[[`, f)
    if (f %in% lists) lapply(v, as.character)
    else if (f == "lith") vapply(v, function(z) as.integer(z %||% 1L)[1L], 1L)
    else if (is.logical(proto[[f]])) vapply(v, function(z) isTRUE(z), NA)
    else vapply(v, function(z) as.character(z %||% NA_character_)[1L], "")
  })
  names(out) <- names(proto)
  out$rp <- nt$ok
  out$status <- nt$status
  out$why <- nt$why
  out$rp1995 <- nt$rp1995
  out$any_known <- any(out$known)
  out
}
