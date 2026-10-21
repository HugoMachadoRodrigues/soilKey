# =============================================================================
# v0.9.267 -- horizon designations in the Canadian notation.
#
# The Canadian System of Soil Classification (Soil Classification Working
# Group 1998, Ch 2, pp. 12-22) writes horizons as an uppercase master (A, B, C,
# O, L, F, H, R, W, or a transition such as AB or BC) followed by lowercase
# suffixes, several of which differ from the FAO and USDA letters soilKey reads
# elsewhere: f is an illuvial Fe-Al-organic accumulation (not plinthite), m a
# slightly altered B (not cementation), v a vertic horizon, ss slickensides, y
# cryoturbation, z permafrost, and j marks the suffix just before it as weakly
# expressed (Btj, Bfjtj). Two letters can form one suffix (ca, cc, co, sa, ss),
# and hf and gf have their own meaning (Bhf, Bgf). Arabic numerals only number
# subdivisions (Bm1, Bm2) and Roman numerals mark a lithologic discontinuity
# (IIBt; IC is the reference parent material). Interfingered horizons are
# written "A and B" or "Ae/Bt".
#
# The Canadian keys read these designations, so the Canadian diagnostics parse
# them with .parse_designation_cssc() and never with the FAO/USDA helpers.
# =============================================================================

.CSSC_MASTERS <- c("A", "B", "C", "O", "L", "F", "H", "R", "W", "D")
.CSSC_PAIRED  <- c("co", "ca", "cc", "sa", "ss", "hf", "gf")
.CSSC_SINGLE  <- c("b", "c", "e", "f", "g", "h", "j", "k", "m", "n", "p", "s",
                   "t", "u", "v", "x", "y", "z")

# One designation component ("Bfjtj", "IIBt", "LFH"): Roman prefix, master,
# suffixes in order, and the suffixes marked weak by a following j.
.parse_cssc_component <- function(s) {
  none <- list(roman = "", master = NA_character_, suffixes = character(0),
               weak = character(0))
  s <- gsub("[0-9]+", "", trimws(s))
  if (!nzchar(s)) return(none)
  roman <- ""
  m <- regmatches(s, regexec("^(IX|IV|V?I{0,3}|X)(?=[A-Z])", s, perl = TRUE))[[1]]
  if (length(m) == 2L && nzchar(m[2]) &&
      !substr(s, nchar(m[2]) + 1L, nchar(m[2]) + 1L) %in% c("I", "V", "X")) {
    roman <- m[2]
    s <- substring(s, nchar(roman) + 1L)
  }
  up <- regmatches(s, regexec("^([A-Z]+)", s))[[1]]
  if (length(up) < 2L) return(none)
  master <- up[2]
  if (!all(strsplit(master, "")[[1]] %in% .CSSC_MASTERS)) return(none)
  rest <- substring(s, nchar(master) + 1L)
  tokens <- character(0)
  i <- 1L
  while (i <= nchar(rest)) {
    two <- substr(rest, i, i + 1L)
    if (nchar(two) == 2L && two %in% .CSSC_PAIRED) {
      tokens <- c(tokens, two); i <- i + 2L
    } else {
      one <- substr(rest, i, i)
      if (one %in% .CSSC_SINGLE) tokens <- c(tokens, one)
      i <- i + 1L
    }
  }
  # j marks the suffix written just before it as weakly expressed (Rule 7)
  weak <- character(0)
  keep <- character(0)
  for (tk in tokens) {
    if (identical(tk, "j")) {
      if (length(keep) > 0L) weak <- c(weak, keep[length(keep)])
    } else {
      keep <- c(keep, tk)
    }
  }
  # Bfh (2016 key) is the 1998 Bhf
  if (all(c("f", "h") %in% keep) && !"hf" %in% keep) {
    keep <- c(setdiff(keep, c("f", "h")), "hf")
  }
  list(roman = roman, master = master, suffixes = keep, weak = unique(weak))
}

#' Parse Canadian horizon designations
#'
#' @param x Character vector of designations.
#' @return A list with one element per designation, each a list of
#'   components (one, or several for "Ae and Bt" / "Ae/Bt"), each with
#'   \code{roman}, \code{master}, \code{suffixes} and \code{weak}.
#' @noRd
.parse_designation_cssc <- function(x) {
  lapply(as.character(x), function(s) {
    if (is.na(s) || !nzchar(trimws(s))) return(list())
    parts <- trimws(strsplit(s, "\\s+and\\s+|/", perl = TRUE)[[1]])
    comps <- lapply(parts[nzchar(parts)], .parse_cssc_component)
    comps[!vapply(comps, function(cmp) is.na(cmp$master), logical(1))]
  })
}

# The suffixes a component carries for the tests below: Bgf is a gleyed
# horizon with iron mottles (Ch 2, p. 13), so gf also counts as g; hf keeps
# its own meaning and counts as neither h nor f.
.cssc_suffix_set <- function(cmp) {
  s <- cmp$suffixes
  if ("gf" %in% s) s <- c(s, "g")
  s
}

# TRUE where a horizon has a component with the given master (exactly; a
# transition such as BC is not a B) and, if given, all the suffixes, each
# strong (weak = "no"), weak (weak = "yes") or either (weak = "any"). NA where
# the designation is missing or unreadable.
.cssc_is <- function(parsed, master, suffixes = character(0),
                     weak = c("no", "yes", "any")) {
  weak <- match.arg(weak)
  vapply(parsed, function(comps) {
    if (length(comps) == 0L) return(NA)
    any(vapply(comps, function(cmp) {
      if (!cmp$master %in% master) return(FALSE)
      if (!all(suffixes %in% .cssc_suffix_set(cmp))) return(FALSE)
      if (length(suffixes) == 0L || weak == "any") return(TRUE)
      w <- suffixes %in% cmp$weak
      if (weak == "no") !any(w) else all(w)
    }, logical(1)))
  }, logical(1))
}

# TRUE where a horizon is mineral by its designation (master A, B or C, or a
# transition of them); NA where the designation is missing.
.cssc_is_mineral <- function(parsed) {
  vapply(parsed, function(comps) {
    if (length(comps) == 0L) return(NA)
    any(vapply(comps, function(cmp)
      all(strsplit(cmp$master, "")[[1]] %in% c("A", "B", "C")), logical(1)))
  }, logical(1))
}

# TRUE where a horizon carries the suffix anywhere (any master), strong or
# weak as asked.
.cssc_has_suffix <- function(parsed, suffix, weak = c("no", "yes", "any")) {
  weak <- match.arg(weak)
  vapply(parsed, function(comps) {
    if (length(comps) == 0L) return(NA)
    any(vapply(comps, function(cmp) {
      if (!suffix %in% .cssc_suffix_set(cmp)) return(FALSE)
      switch(weak, any = TRUE, no = !suffix %in% cmp$weak,
             yes = suffix %in% cmp$weak)
    }, logical(1)))
  }, logical(1))
}

# Is a pedon's set of designations written in the Canadian notation? The
# FAO/USDA letters z (salts), m (cementation), f (frozen) and v (plinthite)
# mean something else in the Canadian notation, so the Canadian key must not
# read such designations. In order:
#   1. the site field designation_system decides when present ("cssc" or
#      another notation);
#   2. signs of the FAO/USDA notation turn the reading off, whatever the
#      country (pedons described in Canada by the USDA survey use it): an E
#      master; O horizons written Oi, Oe or Oa; the letters a, d, i, l, o, q,
#      r or w; or an Arabic number in front of the master (2Bt), where the
#      Canadian notation uses Roman numerals;
#   3. otherwise a Canadian site, or a site with no country, is read as
#      Canadian, and a site of another country is not (only its measured
#      properties are read).
.cssc_notation <- function(designation, site) {
  declared <- tolower(as.character(site$designation_system %||% NA))
  if (!is.na(declared) && nzchar(declared)) {
    ok <- declared %in% c("cssc", "cssc3", "canadian", "canada")
    return(list(ok = ok, why = sprintf("site$designation_system = '%s'", declared)))
  }
  d <- trimws(as.character(designation)); d <- d[!is.na(d) & nzchar(d)]
  # the lowercase letters left once the word "and" of a split horizon (Ae and
  # Bt) and the Canadian two-letter suffixes are taken out (Cca, Csa, Oco,
  # Bss, Bhf, Bgf are Canadian)
  lower <- gsub("ca|cc|co|sa|ss|hf|gf", "",
                gsub("[^a-z]", "", gsub("\\s+and\\s+", "/", d, perl = TRUE)))
  foreign <- grepl("^[0-9]+[A-Z]", d) | grepl("^E|[^A-Z]E", d) |
             grepl("^O[iea]", d) | grepl("[adilorqw]", lower)
  if (any(foreign)) {
    return(list(ok = FALSE, why = sprintf("FAO/USDA notation (%s)",
                                          paste(unique(d[foreign]), collapse = ", "))))
  }
  country <- toupper(as.character(site$country %||% NA))
  if (!is.na(country) && nzchar(country) &&
      !country %in% c("CA", "CAN", "CANADA")) {
    return(list(ok = FALSE, why = sprintf("site in %s, notation not declared", country)))
  }
  list(ok = TRUE, why = if (is.na(country) || !nzchar(country))
                          "no country and no sign of another notation"
                        else "Canadian site")
}
