# =============================================================================
# v0.9.380 -- FAO horizon designations read on the KA5 data route.
#
# A profile that declares the FAO notation (site$designation_system "fao",
# "fao2006", "wrb", "wrb2022" ...; .cssc_fao_declared(), as
# isis_profile_to_pedon() sets it) is not in the KA5 notation, so its rows
# are read by soilKey's order of tests on the data (.ka5_data_layer()). The
# FAO letters (FAO Guidelines for soil description, 2006, repeated in WRB
# 2022, Annex 3, pp. 225-228) are not thrown away: where an FAO letter and a
# KA5 symbol mean the same (5.6.3.3, pp. 88-108), the letter
#   * bounds the horizons the row may be (a "family" of KA5 symbols): an FAO
#     E is an eluvial A (Al, Ael, Ae, Ahe), never an Ah or a C; an FAO B a
#     B, P or T; an FAO Bt a horizon with a Bt part; an FAO Bw one with a Bv
#     part; an FAO Bs, Bh, Bhs one with a Bs or Bh part; an FAO A an A that
#     is not eluvial (or the anthropogenic and colluvial E, R, M the FAO
#     notation writes as A); an FAO C a C (or an M); an FAO R a solid rock
#     (mC); an FAO O an L, Of or Oh; an FAO H an H; the redox letters g
#     (perched or ground water), l (capillary fringe) and r (strong
#     reduction) open the S and G horizons on any master;
#   * states the field observations its own definition names, where they
#     are not recorded: t the clay coatings or bridges of the Bt (p. 97), w
#     the colour change of the Bv against the horizon below (p. 95), p the
#     tillage of the Ap (p. 93), k the secondary carbonate (suffix c, p. 87).
# Measured data the KA5 asks are read first and always decide: a clay
# difference, a Munsell colour, recorded clay films.
#
# The row is then read three-valued within the family: the first test of the
# order that holds gives the horizon when no test before it in the family
# is undecided; undecided tests leave the row undecided between the
# family's candidates, unless all of them are one horizon, which is then
# taken as the FAO letter's translation (as a KA5 designation would be);
# when no horizon of the family holds or is undecided, the FAO letter and
# the measured data are in conflict and the row is undecided between the
# letter's translations and the data's reading. Rows with no designation, or
# one that does not parse, are read on the data as before; a profile in the
# KA5 notation is unchanged.
#
# Sub-horizons (Bt1, Bt2: the same FAO stem, numbered) are one horizon: the
# clay difference of a Bt sub-horizon is that of the run's first row
# against the eluvial horizon above the run (p. 97: the enrichment has
# penetrated the whole Bt).
# =============================================================================

.KA5_FAO_TRANSITIONAL <- c("AB", "AE", "AC", "EA", "EB", "BA", "BE", "BC", "CB", "CA", "CR", "BR")
.KA5_FAO_MASTERS <- c("H", "O", "A", "E", "B", "C", "R")

# One component of an FAO designation ("2Btg1", "AE", "Ah", "Cr"): the layer
# (Arabic number), the master letters, the suffix letters, the trailing
# digits and the stem (the designation without the trailing digits); NULL
# when it does not parse or has no KA5 family (I, W, the limnic L).
.ka5_fao_component <- function(s) {
  m <- regmatches(s, regexec("^([0-9]*)([A-Z]{1,2})([^0-9]*)([0-9]*)$", s))[[1L]]
  if (length(m) < 5L) return(NULL)
  ml <- m[3L]
  if (nchar(ml) == 2L && !ml %in% .KA5_FAO_TRANSITIONAL) return(NULL)
  if (nchar(ml) == 1L && !ml %in% .KA5_FAO_MASTERS) return(NULL)
  suf <- m[4L]
  # letters in parentheses (weakly expressed) are not read
  weak <- regmatches(suf, gregexpr("\\([^)]*\\)", suf))[[1L]]
  suf <- gsub("\\([^)]*\\)", "", suf)
  # ss (slickensides, as USDA writes them) and @ (cryoturbation) have no KA5
  # suffix: not read
  not_read <- character(0)
  if (grepl("ss", suf, fixed = TRUE)) { suf <- gsub("ss", "", suf, fixed = TRUE); not_read <- "ss" }
  if (grepl("@", suf, fixed = TRUE)) { suf <- gsub("@", "", suf, fixed = TRUE); not_read <- c(not_read, "@") }
  if (grepl("[^a-z]", suf)) return(NULL)
  list(layer = if (nzchar(m[2L])) as.integer(m[2L]) else 1L, masters = strsplit(ml, "")[[1L]],
       suffixes = if (nzchar(suf)) strsplit(suf, "")[[1L]] else character(0),
       weak = gsub("[()]", "", paste(weak, collapse = "")), not_read = not_read,
       digits = m[5L], stem = paste0(m[2L], ml, m[4L]))
}

# The KA5 family of an FAO designation (components joined by "/" are
# interfingered: both families): `allow`, regular expressions one of which
# the carrying part of a KA5 code (prefixes included) must match; `excl`,
# ones it must not match; `req`, a list of sets of expressions of which,
# for each set, some part of the code must match one; `defaults`, the KA5
# symbols the FAO letters translate to (the candidates of a conflict);
# `letters`, the suffixes written; `pair`, the translation in words.
.ka5_fao_rule <- function(comps, written) {
  masters <- unique(unlist(lapply(comps, `[[`, "masters")))
  suf <- unique(unlist(lapply(comps, `[[`, "suffixes")))
  allow <- character(0); excl <- character(0); req <- list(); def <- character(0)
  redox <- intersect(suf, c("g", "l", "r"))
  for (M in masters) {
    if (M == "H") { allow <- c(allow, "^[a-z]*H"); def <- c(def, "H") }
    if (M == "O") { allow <- c(allow, "^[a-z]*(L|O)"); def <- c(def, "L", "Of", "Oh") }
    if (M == "A") {
      allow <- c(allow, "^[a-z]*A", "^[a-z]*[MER]")
      def <- c(def, if ("p" %in% suf) "Ap" else "Ah")
    }
    if (M == "E") { allow <- c(allow, "^[a-z]*A(e|l|el|he|hl|eh|lh)$"); def <- c(def, "Al", "Ae") }
    if (M == "B") {
      allow <- c(allow, "^[a-z]*[BPT]")
      bdef <- c(if ("t" %in% suf) "Bt", if ("w" %in% suf) "Bv",
                if (all(c("h", "s") %in% suf)) "Bhs" else if ("s" %in% suf) "Bs" else if ("h" %in% suf) "Bh")
      def <- c(def, if (length(bdef)) bdef else "Bv")
    }
    if (M == "C") { allow <- c(allow, "^[a-z]*C", "^[a-z]*M"); def <- c(def, "lC") }
    if (M == "R") { allow <- c(allow, "^[a-z]*m[a-z]*C"); def <- c(def, "mC") }
  }
  # a plain FAO A is not eluvial (the FAO writes AE or E for that)
  if ("A" %in% masters && !"E" %in% masters) excl <- c(excl, "^[a-z]*A(e|l|el|he)$")
  # the B letters must show in a part of the KA5 code
  if ("B" %in% masters) {
    if ("t" %in% suf) req <- c(req, list("^[a-z]*B[a-z]*t"))
    if ("w" %in% suf) req <- c(req, list("^[a-z]*B[a-z]*v"))
    if ("s" %in% suf) req <- c(req, list("^[a-z]*B[a-z]*s"))
    if ("h" %in% suf) req <- c(req, list("^[a-z]*B[a-z]*h"))
  }
  # p on an A: the tillage of the Ap (or the Esch and Rigol the FAO writes Ap)
  if ("A" %in% masters && "p" %in% suf) req <- c(req, list(c("^[a-z]*A[a-z]*p$", "^[a-z]*[ER]")))
  # the redox letters open the perched-water and groundwater horizons
  if ("g" %in% redox) { allow <- c(allow, "^[a-z]*S", "^[a-z]*G"); def <- c(def, "Sw", "Sd", "Go") }
  if ("l" %in% redox) { allow <- c(allow, "^[a-z]*G"); def <- c(def, "Go") }
  if ("r" %in% redox) { allow <- c(allow, "^[a-z]*G"); def <- c(def, "Gr") }
  def <- unique(def)
  list(allow = unique(allow), excl = unique(excl), req = req, defaults = def, letters = suf,
       masters = masters, written = written,
       pair = sprintf("the FAO %s: KA5 %s", written, paste(def, collapse = " or ")))
}

# The FAO reading of every row of a profile that declares the FAO notation
# (NULL otherwise, and for a profile in the KA5 notation): per row NULL or
# list(comps, rule, stem, layer).
.ka5_fao_rows <- function(x) {
  if (isTRUE(x$ka5) || !x$n) return(NULL)
  ds <- x$site[["designation_system"]]
  if (is.null(ds) || !length(ds) || is.na(ds[[1L]]) || !.cssc_fao_declared(ds[[1L]])) return(NULL)
  lapply(seq_len(x$n), function(i) {
    d <- .ka5_des(x, i)
    if (is.na(d)) return(NULL)
    s <- sub("\\?+$", "", sub("[[:space:]].*$", "", enc2utf8(d)))
    comps <- lapply(strsplit(s, "/", fixed = TRUE)[[1L]], .ka5_fao_component)
    if (!length(comps) || any(vapply(comps, is.null, NA))) return(NULL)
    list(comps = comps, rule = .ka5_fao_rule(comps, d), stem = paste(vapply(comps, `[[`, "", "stem"), collapse = "/"),
         letters = unique(unlist(lapply(comps, `[[`, "suffixes"))),
         masters = unique(unlist(lapply(comps, `[[`, "masters"))))
  })
}

# The FAO reading of a row (NULL when none).
.ka5_fao_at <- function(x, i) {
  if (is.null(x$fao) || is.na(i) || i < 1L || i > length(x$fao)) return(NULL)
  x$fao[[i]]
}

# Does the FAO designation of a row write a suffix letter (on a master of
# `masters`, when given)?
.ka5_fao_has <- function(x, i, letter, masters = NULL) {
  f <- .ka5_fao_at(x, i)
  if (is.null(f)) return(FALSE)
  any(vapply(f$comps, function(cc) letter %in% cc$suffixes && (is.null(masters) || any(cc$masters %in% masters)), NA))
}

# Is a KA5 code within the FAO family of a row? TRUE, FALSE.
.ka5_fao_compatible <- function(rule, code) {
  if (is.null(rule)) return(TRUE)
  if (is.null(code) || is.na(code) || !nzchar(code)) return(FALSE)
  parts <- strsplit(code, "[-+\u00b0]")[[1L]]
  parts <- parts[nzchar(parts)]
  if (!length(parts)) return(FALSE)
  carry <- parts[length(parts)]
  if (!any(vapply(rule$allow, function(re) grepl(re, carry), NA))) return(FALSE)
  # the other parts: of the family too, or the humus of an Ah (Ah-Bv) or the
  # redox features of an S or G (Go-Bt), which the data measure
  for (p in parts[-length(parts)])
    if (!any(vapply(rule$allow, function(re) grepl(re, p), NA)) && !grepl("^[a-z]*(A[a-z]*h|[SG][a-z]*)$", p)) return(FALSE)
  if (length(rule$excl) && any(vapply(rule$excl, function(re) grepl(re, carry), NA))) return(FALSE)
  for (rq in rule$req)
    if (!any(vapply(rq, function(re) any(grepl(re, parts)), NA))) return(FALSE)
  TRUE
}

# Does the FAO designation of row j rule out a KA5 code (a row with no FAO
# reading rules out nothing)?
.ka5_fao_excludes <- function(x, j, code) {
  f <- .ka5_fao_at(x, j)
  !is.null(f) && !.ka5_fao_compatible(f$rule, code)
}

# A test on another row, read within that row's FAO family: FALSE (with a
# note) when the FAO designation rules the test's horizon out.
.ka5_fao_gate <- function(x, j, r, code = r$code) {
  if (is.na(j) || isFALSE(r$passed) || !.ka5_fao_excludes(x, j, code %||% "")) return(r)
  f <- .ka5_fao_at(x, j)
  .ka5_r(FALSE, code = r$code, note = sprintf("%s rules out a %s", f$rule$pair, code))
}

# The first row of the run of rows with the same FAO stem as row i (the
# sub-horizons Bt1, Bt2 of one Bt), in depth order: row i itself when the
# row above has another stem or no FAO reading.
.ka5_fao_run_top <- function(x, i) {
  f <- .ka5_fao_at(x, i)
  if (is.null(f)) return(i)
  j <- i
  repeat {
    p <- .ka5_prev(x, j)
    fp <- .ka5_fao_at(x, p)
    if (is.na(p) || is.null(fp) || !identical(fp$stem, f$stem) || !.ka5_same_layer(x, p, j)) break
    j <- p
  }
  j
}

# The last row of that run (the sub-horizon at its bottom).
.ka5_fao_run_bottom <- function(x, i) {
  f <- .ka5_fao_at(x, i)
  if (is.null(f)) return(i)
  j <- i
  repeat {
    nx <- .ka5_next(x, j)
    fn <- .ka5_fao_at(x, nx)
    if (is.na(nx) || is.null(fn) || !identical(fn$stem, f$stem) || !.ka5_same_layer(x, nx, j)) break
    j <- nx
  }
  j
}

# The row under an FAO eluvial horizon: below the run of its sub-horizons
# and below the transitional E-B rows (EB, BE, E/B) with no t; for any other
# row the next row.
.ka5_fao_under_eluvial <- function(x, i) {
  f <- .ka5_fao_at(x, i)
  if (is.null(f) || !"E" %in% f$masters) return(.ka5_next(x, i))
  j <- .ka5_next(x, .ka5_fao_run_bottom(x, i))
  repeat {
    fj <- .ka5_fao_at(x, j)
    if (is.na(j) || is.null(fj) || !"E" %in% fj$masters || "t" %in% fj$letters || !.ka5_same_layer(x, i, j)) break
    j <- .ka5_next(x, .ka5_fao_run_bottom(x, j))
  }
  j
}

# The reading of an FAO row within its family, from the answers of the order
# of tests (.ka5_data_layer()): list(code, dec, tr, cands, kind, note).
# kind: "both" (a test of the family holds), "fao" (the data leave it
# undecided, the family is one horizon), "undecided", "conflict".
.ka5_fao_combine <- function(x, i, ans, f, data_code, data_cands) {
  rule <- f$rule
  items <- list()
  for (s in names(ans)) for (r in ans[[s]]) {
    if (!length(r$passed) || isFALSE(r$passed)) next
    codes <- if (is.na(r$passed) && is.character(r$value) && length(r$value)) r$value else r$code %||% r$name
    for (cc in unique(codes[!is.na(codes)])) {
      tr <- .ka5_transition(x, i, cc, ans)
      full <- .ka5_suffix_c(x, i, .ka5_prefixes(x, i, tr$code))
      base <- .ka5_suffix_c(x, i, .ka5_prefixes(x, i, cc))
      if (.ka5_fao_compatible(rule, full)) {
        items[[length(items) + 1L]] <- list(code = cc, tr = tr, shown = full, passed = r$passed, r = r)
        next
      }
      if (.ka5_fao_compatible(rule, base)) {
        items[[length(items) + 1L]] <- list(code = cc, tr = list(code = cc, note = character(0)), shown = base,
                                            passed = r$passed, r = r)
        next
      }
      # a G or S with a B the data leave undecided (Bt-Sd when the Bt is
      # undecided): each such B part, undecided
      if (grepl("^[a-z]*[GS]", tr$code)) for (b in ans$subsoil) {
        if (!is.na(b$passed) || is.null(b$code)) next
        alt <- paste0(b$code, "-", tr$code)
        if (.ka5_fao_compatible(rule, alt))
          items[[length(items) + 1L]] <- list(code = cc, tr = list(code = alt, note = sprintf("the %s undecided: %s", b$code, alt)),
                                              shown = .ka5_suffix_c(x, i, .ka5_prefixes(x, i, alt)), passed = NA, r = r)
      }
    }
  }
  if (!length(items)) {
    dat <- unique(c(data_code[!is.na(data_code)], data_cands))
    return(list(code = NA_character_, dec = NULL, tr = NULL, kind = "conflict",
                cands = unique(c(rule$defaults, dat)),
                note = sprintf("%s; no horizon of that family holds on the measured data%s: the FAO designation and the data in conflict, undecided",
                               rule$pair, if (length(dat)) sprintf(" (the data read %s)", paste(dat, collapse = ", ")) else "")))
  }
  pas <- vapply(items, function(z) z$passed, NA)
  k <- which(pas %in% TRUE)[1L]
  if (identical(k, 1L))
    return(list(code = items[[1L]]$code, dec = items[[1L]]$r, tr = items[[1L]]$tr, kind = "both", cands = character(0),
                note = sprintf("%s; the data reach %s", rule$pair, items[[1L]]$shown)))
  pool <- if (is.na(k)) items else items[seq_len(k)]
  shown <- unique(vapply(pool, `[[`, "", "shown"))
  # one horizon of the family left undecided: the FAO designation reads it
  # when it is the letter's translation (Bt for Bt, Bv for Bw), as a KA5
  # designation would be read; a finer KA5 horizon the letter does not name
  # (an Axp for an FAO Ap) stays undecided beside the translation
  carry <- function(code) { pp <- strsplit(code, "[-+\u00b0]")[[1L]]; sub("^[a-z]*", "", pp[length(pp)]) }
  defs <- unique(vapply(rule$defaults, carry, ""))
  if (length(shown) == 1L && carry(shown) %in% defs) {
    z <- pool[[length(pool)]]
    return(list(code = z$code, dec = z$r, tr = z$tr, kind = "fao", cands = character(0),
                note = sprintf("%s; the data leave the %s undecided, the FAO designation reads it (as a KA5 designation would be read)",
                               rule$pair, z$shown)))
  }
  if (length(shown) == 1L)
    shown <- unique(c(shown, rule$defaults[vapply(rule$defaults, function(d) .ka5_fao_compatible(rule, d), NA)]))
  list(code = NA_character_, dec = NULL, tr = NULL, kind = "undecided", cands = shown,
       note = sprintf("%s; the data leave it undecided between %s", rule$pair, paste(shown, collapse = ", ")))
}
