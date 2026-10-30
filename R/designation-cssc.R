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

# Does one component carry a suffix, strong (weak = "no"), weak ("yes") or
# either ("any")? TRUE, FALSE, or NA where a designation translated from the
# FAO notation (.cssc_from_fao()) cannot say: a suffix it leaves open
# (cmp$open) is neither present nor absent, and a suffix whose weak form (j)
# the FAO notation cannot write (cmp$weak_open) is read in its full form but
# may be weak.
.cssc_cmp_has <- function(cmp, suffix, weak) {
  if (suffix %in% .cssc_suffix_set(cmp)) {
    w <- suffix %in% cmp$weak
    if (weak == "yes" && !w && suffix %in% cmp$weak_open) return(NA)
    return(switch(weak, any = TRUE, no = !w, yes = w))
  }
  if (suffix %in% cmp$open) NA else FALSE
}

# TRUE where a horizon has a component with the given master (exactly; a
# transition such as BC is not a B) and, if given, all the suffixes, each
# strong (weak = "no"), weak (weak = "yes") or either (weak = "any"). NA where
# the designation is missing or unreadable, or where a suffix asked for is
# left open by the translation of another notation.
.cssc_is <- function(parsed, master, suffixes = character(0),
                     weak = c("no", "yes", "any")) {
  weak <- match.arg(weak)
  vapply(parsed, function(comps) {
    if (length(comps) == 0L) return(NA)
    any(vapply(comps, function(cmp) {
      if (!cmp$master %in% master) return(FALSE)
      if (length(suffixes) == 0L) return(TRUE)
      has <- vapply(suffixes, function(s) .cssc_cmp_has(cmp, s, weak), logical(1))
      if (any(has %in% FALSE)) FALSE else if (all(has %in% TRUE)) TRUE else NA
    }, logical(1)))
  }, logical(1))
}

# TRUE where a horizon is an A whose h a translation leaves open (an FAO A
# written without h or p, which may be an Ah: .cssc_from_fao()).
.cssc_a_h_open <- function(parsed) {
  vapply(parsed, function(comps) {
    any(vapply(comps, function(cmp) identical(cmp$master, "A") && "h" %in% cmp$open,
               logical(1)))
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
    any(vapply(comps, function(cmp) .cssc_cmp_has(cmp, suffix, weak), logical(1)))
  }, logical(1))
}

# Is a pedon's set of designations written in the Canadian notation? The
# FAO/USDA letters z (salts), m (cementation), f (frozen) and v (plinthite)
# mean something else in the Canadian notation, so the Canadian key must not
# read such designations. In order:
#   1. the site field designation_system decides when present ("cssc" or
#      another notation); a declared FAO notation (.cssc_fao_declared()) is
#      read through its translation into the Canadian one (.cssc_from_fao()),
#      and $why lists what each designation was read as;
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
    if (.cssc_fao_declared(declared)) return(.cssc_fao_notation(designation, declared))
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


# =============================================================================
# v0.9.317 -- designations in the FAO notation, read in the Canadian one.
#
# A profile that declares the FAO notation (site$designation_system "fao",
# "fao2006", "wrb", "wrb2022" ...: the letters of the FAO Guidelines for soil
# description, 2006, which WRB 2022 repeats in Annex 3, Tables 10.1 and 10.2,
# pp. 225-228) is read by the Canadian key through a translation of its
# designations, letter by letter, where the FAO letter and the Canadian one
# (Ch 2, pp. 12-18) mean the same. The pairs are those of the book's own
# correlation of horizon designations (Table 6, p. 154, Canadian / U.S. / FAO
# columns), checked against both definitions:
#   masters: A = A; E = Ae (the Ae row); AE (or EA) = Ahe (the Ahe row, AE in
#     the U.S. column); EB = AB and BE = BA (the AB and BA rows); B, C, R and
#     W as written; H, the FAO peat layer, = O (the O row: Of, Om, Oh against
#     FAO H), its i, e and a (initial, intermediate and advanced
#     decomposition) = Of, Om and Oh, as the U.S. Oi, Oe and Oa are; O, an
#     FAO organic layer seldom saturated with water, = the folic L, F and H
#     (the L-F, L-H and F-H rows) by its i, e and a, and LFH (of unknown
#     kind) without one; I (ice) = Wz (segregated ice is a W layer, p. 12,
#     and z a frozen one); an Arabic number before the master = a Roman
#     numeral (2Bt = IIBt; the U.S. 2C against the Canadian IIC).
#   suffixes: b = b (buried); f (permafrost) = z ("A frozen layer", p. 16);
#     g, l (capillary fringe mottling) and r (strong reduction) = g (Table 6
#     pairs the Canadian g with the FAO g and r; all three mark reduction or
#     its mottling); h = h on an A or a B, and h with s on a B = hf (Bhs =
#     Bhf); i (slickensides; ss, as USDA writes them) = ss; k = k (an
#     accumulation of carbonates is a presence of carbonates, the Canadian k;
#     the ca that Table 6 pairs with k has limits a k does not state);
#     m (cementation) = c (Table 6) when it follows the letter of its
#     cementing agent (k, l, q, s, v, y or z: "only used if following another
#     suffix ... that indicates the cementing agent", Annex 3, p. 227; Bkm,
#     Bsm, Bqm, Bym), as the WRB reading of a cemented layer has it
#     (.wrb_cemented_limiting()); a bare m (Bm, Cm) is not read, since it is
#     what a Canadian Bm looks like; n = n on a B (Btn = Bnt); p = p on an A
#     or an H; s = f on a B (Bs = Bf); t = t on a B; w = m on a B (Bw = Bm);
#     x = x (fragipan); y (gypsum) and z (salts more soluble than gypsum) = s
#     ("salts, including gypsum", p. 15; Table 6 pairs z with s; never the
#     Canadian z, a frozen layer); @ (cryoturbation) = y; alpha (primary
#     carbonates, WRB 2022) = k.
# A WRB 2022 B that combines w with t, g or s writes them weakly expressed
# (Annex 3, rule 5, p. 228: "Bwt (weak illuvial accumulation of clay
# minerals"), which is the Canadian j (Bmtj, Bmgj, Bmfj); a letter in
# parentheses is read the same way. Otherwise an FAO letter is read as the
# Canadian suffix Table 6 pairs it with, in its full form; where the Canadian
# suffix has limits the FAO letter does not state (f, hf and h of a B, n),
# the key reads the measured properties first, as it does for any
# designation. The FAO notation has no j, so whether a Canadian describer
# would have written the e, f, hf, g, n or t weak cannot be read: the tests
# that ask for the weak form (Aej, Btj, Bfj, Bnj, gj) are left undecided
# ($weak_open), as they are for designations in another notation.
# Not read (listed in $not_read): a bare m (above), c (concretions or
# nodules, which need not be cemented, unlike the Canadian cc), d, e on a C
# (saprolite), j (jarosite; never the Canadian j), o, q, u (artefacts; never
# the Canadian turbic u), v
# (plinthite; never the Canadian vertic v), the other Greek letters, a letter
# marked relict (rho), and h, n, p, s, t and w where the Canadian notation
# does not use them (an h, s or t on a C, for instance).
# Canadian suffixes the FAO notation cannot write are left open ($open: NA,
# neither present nor absent): h on an A written without h or p (an FAO A may
# be an Ah, and is tested by the Ah definition); m on an uncemented B with
# none of w, t, g, l, r, s, h, n or o (a Bk, a plain B or a bare FAO Bm may be
# a Canadian Bm: rule 5 leaves the w out where k or y is strongly
# expressed); k where neither k nor alpha is
# written (primary carbonates have no FAO 2006 letter); v on a B or BC (the
# vertic horizon has no FAO letter); gf on a gleyed B (nor has the iron of
# the Bgf). The FAO limnic L, and a designation that does not parse, are not
# translated; they are read by their master letters only, as designations in
# another notation are.
# =============================================================================

.CSSC_FAO_SYSTEMS <- c("fao", "fao2006", "wrb", "wrb2006", "wrb2014", "wrb2015",
                       "wrb2022", "faowrb", "wrbfao")

# Does site$designation_system declare the FAO notation?
.cssc_fao_declared <- function(declared) {
  gsub("[^a-z0-9]", "", tolower(as.character(declared))[1L]) %in% .CSSC_FAO_SYSTEMS
}

# The order the Canadian notation writes its suffixes in (Ch 2, pp. 21-22,
# rules 6 and 7: Ahe, Bhf, Bnt, Btg, Ckg, Csk; a j after the others).
.CSSC_SUFFIX_ORDER <- c("h", "e", "hf", "f", "p", "m", "n", "t", "c", "s", "k", "ss",
                        "g", "gf", "x", "y", "z", "b")

.cssc_roman <- function(n) {
  n <- suppressWarnings(as.integer(n))
  if (is.na(n) || n <= 1L) "" else as.character(utils::as.roman(n))
}

# One FAO component ("2Btg1", "BCk", "Hi", "E") in the Canadian notation:
# $cmp, a component as .parse_cssc_component() returns it plus $open (the
# Canadian suffixes left open) and $weak_open (those whose weak form may be
# meant), or NULL when it does not translate; $canadian, the designation
# written in the Canadian notation; $not_read, the FAO letters left aside.
.cssc_fao_component <- function(s) {
  fail <- list(cmp = NULL, canadian = NA_character_, not_read = character(0))
  m <- regmatches(s, regexec("^([0-9]*)([AEBC][AEBC]|[HOAEBCRIWL])(.*)$", s))[[1]]
  if (length(m) < 4L || m[3L] == "L") return(fail)   # L: limnic, no Canadian master
  num <- m[2L]; fm <- m[3L]; rest <- m[4L]
  digits <- sub("^.*?([0-9]*)$", "\\1", rest)
  rest <- gsub("[0-9]", "", rest)
  # letters in parentheses are weakly expressed
  par <- regmatches(rest, gregexpr("\\(([^)]*)\\)", rest))[[1]]
  weak_l <- strsplit(gsub("[()]", "", paste(par, collapse = "")), "")[[1]]
  rest <- gsub("[()]", "", rest)
  # a letter marked relict (rho) is not read
  relict <- regmatches(rest, gregexpr(".\u03c1", rest))[[1]]
  rest <- gsub(".\u03c1", "", rest)
  rest <- gsub("ss", "i", rest, fixed = TRUE)   # slickensides, as USDA writes them
  if (grepl("[A-Z]", rest)) return(fail)
  # an m is a cementation only after the letter of its cementing agent
  cemented <- grepl("[klqsvyz]m", rest)
  ch <- strsplit(rest, "")[[1]]
  out <- character(0); open <- character(0); weak_can <- character(0)
  not_read <- substr(relict, 1L, 1L)
  skip <- function(l) not_read <<- c(not_read, l)
  if (fm %in% c("R", "W", "I")) {
    master <- if (fm == "I") "W" else fm
    if (fm == "I") out <- "z"
    for (l in ch) skip(l)
  } else if (fm %in% c("H", "O")) {
    dec <- ch[ch %in% c("i", "e", "a")][1L]
    if (fm == "H") {
      master <- "O"
      if (!is.na(dec)) out <- c(i = "f", e = "m", a = "h")[[dec]]
    } else {
      master <- if (is.na(dec)) "LFH" else c(i = "L", e = "F", a = "H")[[dec]]
    }
    for (k in seq_along(ch)) {
      l <- ch[k]
      if (l %in% c("i", "e", "a")) { if (k != match(dec, ch)) skip(l); next }
      can <- switch(l, b = "b", f = "z", k = "k", "\u03b1" = "k", y = "s", z = "s",
                    "@" = "y", p = if (fm == "H") "p" else NA_character_, NA_character_)
      if (is.na(can)) skip(l) else out <- c(out, can)
    }
    if (!"k" %in% out) open <- "k"
  } else {
    master <- gsub("E", "A", fm)
    if (master == "AA") master <- "A"
    plain_a <- fm == "A"
    on_b <- grepl("B", master)
    if (fm == "E") out <- "e"
    if (fm %in% c("AE", "EA")) out <- c("h", "e")
    for (l in ch) {
      can <- switch(l, b = "b", f = "z", g = "g", l = "g", r = "g", i = "ss", k = "k",
                    "\u03b1" = "k", m = if (cemented) "c" else NA_character_,
                    x = "x", y = "s", z = "s", "@" = "y",
                    h = if (plain_a || on_b) "h" else NA_character_,
                    n = if (on_b) "n" else NA_character_,
                    p = if (plain_a) "p" else NA_character_,
                    s = if (on_b) "f" else NA_character_,
                    t = if (on_b) "t" else NA_character_,
                    w = if (on_b) "m" else NA_character_,
                    NA_character_)
      if (is.na(can)) skip(l) else out <- c(out, can)
    }
    if (on_b && all(c("h", "f") %in% out)) out <- c(setdiff(out, c("h", "f")), "hf")
    # w with t, g or s writes them weakly expressed (rule 5); the Canadian
    # notation has no weak h
    if (on_b && "w" %in% ch) weak_can <- intersect(out, c("t", "g", "f", "hf"))
    wl <- intersect(weak_l, c("t", "g", "l", "r", "s", "n"))
    weak_can <- c(weak_can, intersect(out, unique(c(t = "t", g = "g", l = "g", r = "g",
                                                    s = if ("hf" %in% out) "hf" else "f",
                                                    n = "n")[wl])))
    if (on_b && (("w" %in% ch) || "h" %in% weak_l) && "h" %in% out) {
      out <- setdiff(out, "h"); skip("h")
    }
    if (plain_a && !any(c("h", "p") %in% out)) open <- c(open, "h")
    # (a cemented B is no Bm: "No cementation or induration", Ch 2, p. 14)
    if (master == "B" && !any(c("m", "c") %in% out) &&
        !any(ch %in% c("t", "g", "l", "r", "s", "h", "n", "o")))
      open <- c(open, "m")
    if (!"k" %in% out) open <- c(open, "k")
    if (master %in% c("B", "BC", "CB")) open <- c(open, "v")
    if (master == "B" && "g" %in% out) open <- c(open, "gf")
  }
  out <- unique(out); weak_can <- unique(weak_can)
  strong <- setdiff(out, weak_can)
  ordered <- function(v) v[order(match(v, .CSSC_SUFFIX_ORDER))]
  canadian <- paste0(.cssc_roman(num), master, paste(ordered(strong), collapse = ""),
                     if (length(weak_can)) paste0(ordered(weak_can), "j", collapse = "") else "",
                     digits)
  # the FAO notation has no j: a letter not marked weak may still be the
  # weak form of the Canadian suffix (in a mineral horizon; the f of an Of
  # is fibric material)
  weak_open <- if (master %in% c("O", "L", "F", "H", "LFH", "W", "R")) character(0)
               else setdiff(intersect(out, c("e", "f", "hf", "g", "n", "t")), weak_can)
  list(cmp = list(roman = .cssc_roman(num), master = master, suffixes = out,
                  weak = weak_can, weak_open = weak_open,
                  open = setdiff(unique(open), out)),
       canadian = canadian, not_read = unique(not_read))
}

#' FAO horizon designations in the Canadian notation
#'
#' @param designation Character vector of designations in the FAO notation.
#' @return A list: \code{comps}, one element per designation in the shape of
#'   \code{.parse_designation_cssc()} (components with \code{roman},
#'   \code{master}, \code{suffixes}, \code{weak}, \code{open}, the
#'   Canadian suffixes left open, and \code{weak_open}, those that may be
#'   weakly expressed); \code{table}, a data frame with the FAO
#'   designation, the Canadian one it is read as (NA when it does not
#'   translate), the FAO letters not read and the Canadian suffixes left
#'   open; \code{raw}, TRUE where a designation does not translate and is
#'   read by its master letters only.
#' @noRd
.cssc_from_fao <- function(designation) {
  d <- as.character(designation)
  one <- function(s) {
    if (is.na(s) || !nzchar(trimws(s)))
      return(list(comps = list(), canadian = NA_character_, not_read = "", open = ""))
    s1 <- sub("\\?+$", "", sub("[[:space:]].*$", "", trimws(enc2utf8(s))))
    s1 <- .cssc_other_case(s1)
    tr <- lapply(strsplit(s1, "/", fixed = TRUE)[[1]], .cssc_fao_component)
    ok <- !vapply(tr, function(t) is.null(t$cmp), logical(1))
    if (!any(ok))
      return(list(comps = list(), canadian = NA_character_, not_read = "", open = ""))
    list(comps = lapply(tr[ok], `[[`, "cmp"),
         canadian = paste(vapply(tr[ok], `[[`, "", "canadian"), collapse = "/"),
         not_read = paste(unique(unlist(lapply(tr, `[[`, "not_read"))), collapse = ", "),
         open = paste(unique(unlist(lapply(tr[ok], function(t) t$cmp$open))), collapse = ", "))
  }
  res <- lapply(d, one)
  comps <- lapply(res, `[[`, "comps")
  canadian <- vapply(res, `[[`, "", "canadian")
  list(comps = comps,
       table = data.frame(designation = d, canadian = canadian,
                          not_read = vapply(res, `[[`, "", "not_read"),
                          open = vapply(res, `[[`, "", "open"), stringsAsFactors = FALSE),
       raw = !is.na(d) & nzchar(trimws(d)) & is.na(canadian))
}

# The notation of a profile declared in the FAO notation: read as Canadian
# through .cssc_from_fao(), the translation listed in $why and $translation,
# and $raw the rows read by their master letters only.
.cssc_fao_notation <- function(designation, declared) {
  tr <- .cssc_from_fao(designation)
  t <- tr$table
  has <- !is.na(t$designation) & nzchar(trimws(t$designation))
  pairs <- ifelse(is.na(t$canadian), sprintf("%s (not translated)", t$designation),
                  sprintf("%s = %s%s", t$designation, t$canadian,
                          ifelse(nzchar(t$not_read), sprintf(" (%s not read)", t$not_read), "")))
  listed <- if (any(has)) paste0(": ", paste(unique(pairs[has]), collapse = "; ")) else ""
  list(ok = TRUE, translated = TRUE,
       why = sprintf(paste("site$designation_system = '%s': FAO designations read in the",
                           "Canadian notation (CSSC Table 6, p. 154)%s"), declared, listed),
       comps = tr$comps, translation = t, raw = tr$raw)
}
