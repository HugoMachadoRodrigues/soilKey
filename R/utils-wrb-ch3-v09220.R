# =============================================================================
# v0.9.220 -- shared readings of field descriptions for the WRB 2022 Chapter 3
# diagnostics audited in this version (nitic horizon, continuous rock, argic
# horizon criterion 2.a.i, panpaic horizon, retic properties, Lamellic).
# =============================================================================

#' The material number a horizon designation records (FAO 2006 Guidelines and
#' KST 13: "2Bt" lies in the second material of the profile, below a lithic
#' discontinuity; no number is the first). NA where there is no designation.
#' @noRd
.desg_material_number <- function(d) {
  d <- trimws(as.character(d))
  out <- ifelse(grepl("^[0-9]+", d), sub("^([0-9]+).*", "\\1", d), "1")
  out[is.na(d) | !nzchar(d)] <- NA_character_
  suppressWarnings(as.integer(out))
}

#' The designation of a horizon without the number of its material: "2Bt1",
#' and the "IIBt1" of the surveys that numbered the materials in Roman numerals,
#' are both "Bt1". It is what a test of the master horizon and of its suffixes
#' reads; the horizon data keep the designation as written. NA stays NA.
#'
#' v0.9.240: the "(B)" of the notation in use in Brazil until 1988, the B
#' incipiente, is read as the "Bi" that replaced it (Larach et al., 1988,
#' "Definicao e notacao de horizontes e camadas do solo", the reference of
#' SiBCS 2018 for the notation): "(B)", "(B)21", "(B1)" and "II(B)22" are
#' "Bi", "Bi21", "Bi1" and "Bi22". A parenthesis elsewhere in the designation
#' is left as written.
#'
#' v0.9.250: in a profile written in that older notation the "A2" is the
#' eluvial horizon, the E of today, and the "B3" is the horizon of transition
#' to the C, the BC of today: "A2", "A21" and "B3t" are read "E", "E1" and
#' "BCt". A profile is taken to be in the older notation when one of its
#' horizons is a B followed at once by a number ("B1", "B21t", "IIB3") or a
#' "(B)" (\code{.desg_old_notation()}); in the present notation the number
#' comes after the letters ("Bt1"), and "A1, A2" beside "Bt1, Bt2" are two
#' parts of an A. \code{profile} gives the designations of the whole profile
#' when \code{d} is only some of them.
#'
#' v0.9.256: the older notation is also known by an A or a C followed by two
#' figures ("A11", "A21", "C11") and by the suffixes "ir" in a B and "pl" in
#' a B or a C, so that "A1, A21, A22, Bhir" and "A1, A2, Bh, Bhir" have their
#' E read.
#'
#' v0.9.395: \code{site} is the site of the profile. A notation declared in
#' \code{site$designation_system} decides before the marks
#' (\code{.desg_declared_notation()}): a declared older notation has its A2,
#' B3 and "(B)" read, a declared present notation none of them, whatever
#' its designations look like (FAO 2006 writes "B1, B2, B3" for three parts
#' of a B and "(Bw)R" for a B in the cracks of a rock: there the
#' parentheses are dropped, "(Bw)R" is read "BwR"). With no declaration the
#' marks decide, as before.
#' @noRd
.desg_master <- function(d, profile = d, site = NULL) {
  d <- as.character(d)
  declared <- .desg_declared_notation(site)
  old <- .desg_old_notation(profile, site = site)
  # v0.9.400: a pure function of d, the declared notation and the reading of
  # the profile, kept in the session cache of R/memo.R
  .desg_memo("master", list(d, declared, if (old) "old" else "present"),
             function() .desg_master_read(d, declared, old))
}

# The reading of .desg_master() for designations d, a declared notation and
# whether the profile is in the older notation.
.desg_master_read <- function(d, declared, old) {
  d <- trimws(d)
  # "(B)" is the B incipiente (Bi) of the older notation; in a declared
  # present notation the parentheses are dropped and the B kept as written
  # ("(Bw)R" is "BwR", "(B)km" is "Bkm"; Hugo, 2026-11-10)
  d <- sub("^([0-9]*|[IVX]*)\\(\\s*B\\s*([0-9a-z]*)\\s*\\)\\s*",
           if (identical(declared, "present")) "\\1B\\2" else "\\1Bi\\2", d)
  out <- sub("^(?:[0-9]+(?=[A-Z])|[IVX]+(?=[A-HJ-UWYZ]))", "", d, perl = TRUE)
  if (old) {
    out <- sub("^A2", "E", out)
    out <- sub("^B3", "BC", out)
  }
  out
}

#' Whether the designations of a profile are in the notation in use until
#' 1988: a B followed at once by a number ("B1", "B21t", "IIB3"), or a "(B)".
#'
#' Since v0.9.256 also (decided on 2026-10-13):
#' \itemize{
#'   \item an A or a C followed at once by two figures ("A11", "A12", "A21",
#'         "C11"): the older notation numbers the parts of a horizon after
#'         the figure of the horizon, the present one counts them from 1
#'         after the letters;
#'   \item the suffix "ir" in a B ("Bir", "Bhir", "B2ir"), the illuvial iron
#'         that is written "s" today;
#'   \item the suffix "pl" in a B or a C ("Bpl", "B21pl", "Cpl"), the
#'         plinthite that is written "f" today.
#' }
#' The suffix "cn" of the concretions is not taken for a mark: it is found in
#' profiles otherwise written in the present notation ("A1, A2, AB, BA, Bw1,
#' Bw2, BC, C1, Ccn"), where the A2 is the second part of the A.
#'
#' Since v0.9.395 a notation declared in \code{site$designation_system}
#' decides before the marks (\code{.desg_declared_notation()}).
#' @noRd
.desg_old_notation <- function(d, site = NULL) {
  declared <- .desg_declared_notation(site)
  if (identical(declared, "old")) return(TRUE)
  if (identical(declared, "present")) return(FALSE)
  d <- as.character(d)
  .desg_memo("old", list(d), function() {
    d <- trimws(d)
    d <- d[!is.na(d)]
    if (length(d) == 0L) return(FALSE)
    m <- sub("^(?:[0-9]+(?=[A-Z(])|[IVX]+(?=[A-HJ-UWYZ(]))", "", d, perl = TRUE)
    any(grepl("^\\(\\s*B", m) | grepl("^B[0-9]", m) |
          grepl("^[AC][0-9][0-9]", m) |
          grepl("^B[^A-Z]*ir", m) | grepl("^[BC][^A-Z]*pl", m))
  })
}

#' The horizons with their designations read by .desg_master(), for the tests
#' that take the name of a column.
#' @noRd
.h_master <- function(h, site = NULL) {
  h <- data.table::copy(h)
  h$designation <- .desg_master(h$designation, site = site)
  h
}

# --- the notation declared in site$designation_system (v0.9.395) ---------------

# Declared values (lower-cased, accents folded, all but letters and digits
# dropped) of a notation in which the A2 is the eluvial horizon (the E of
# today), the B3 the transition to the C (the BC of today) and the "(B)" the
# B incipiente: the Australian Field Handbook of the ASC (A1, A2, A3, B1,
# B21, B3; "asc" with any edition number, .asc_key_is_asc()), the French
# CPCS of 1967 (A1, A2, B2, B3, (B)), the FAO-Unesco Legend of 1974 and the
# FAO Guidelines of 1977, the USDA Soil Survey Manual of 1951 and its
# supplement of 1962, and the Brazilian notation in use until 1988
# ("br_pre1988"; Larach et al., 1988, replaced it). Hugo's decisions Q1 and
# Q2, 2026-11-10; the USDA of 1975 and the Soviet notation of 1977 are not
# old here.
.DESG_OLD_DECLARED <- c("cpcs", "cpcs1967", "french", "francaise", "fr", "fao1974", "fao1977",
                        "usda1951", "ssm1951", "soilsurveymanual1951", "usda1962", "ssm1962",
                        "soilsurveymanual1962", "brpre1988")

# Declared values of a notation of today: the FAO Guidelines (and the WRB
# that repeats them), the USDA (Soil Survey Manual, KST, NASIS), the SiBCS
# (Santos et al., 2015, the notation of Larach et al., 1988), the CSSC, the
# NZSC, the SCSJ and CSCJ, the Chinese and Hungarian fields and the Soviet
# notation (a name, optionally followed by an edition or a year); the South
# African, German, Russian and French notations are read by their own
# readers' predicates (.desg_declared_notation()). A value that is none of
# these is not known: the marks decide. Hugo's decisions Q3 and Q4,
# 2026-11-10.
.DESG_PRESENT_DECLARED_RE <- paste0(
  "^((fao|faowrb|wrbfao|faoguidelines|guidelines|wrb)([0-9]{4})?|",
  "(usda|kst|soiltaxonomy|keystosoiltaxonomy|nasis|ncss|ssm|soilsurveymanual)([0-9]{1,4})?|",
  "(sibcs|santos)([0-9]{1,4})?|cssc[0-9]*|canadian|canada|nzsc[0-9]*|nz|newzealand|",
  "(scsj|cscj)[0-9]*|cst[0-9]*|hscs[0-9]*|(soviet|ussr)[0-9]*)$")

# Declared values that declare nothing, so that the marks decide: the
# missing-value words, SA and RSA (the country, not a notation), and
# "nz_legacy", the older New Zealand notation of the National Soils Database
# surveys (A11, A12, B21, B22, B3 beside Bw), written by read_nsd_pedons()
# so that each profile's own marks decide (Hugo's decision Q5, 2026-11-10).
.DESG_UNDECLARED <- c("na", "none", "unknown", "nd", "null", "missing", "notrecorded",
                      "sa", "rsa", "nzlegacy")

# A value of site$designation_system as a key: lower-cased, accents folded,
# all but letters and digits dropped ("NZ legacy" -> "nzlegacy"); a value
# that is not UTF-8 is read as CP1252, as the RP and KA5 readers do. NA when
# nothing is recorded or the value cannot be read.
.notation_key <- function(ds) {
  v <- tryCatch(as.character(unlist(ds)), error = function(e) character(0))
  if (!length(v) || is.na(v[1L])) return(NA_character_)
  v <- v[1L]
  if (!validUTF8(v)) v <- suppressWarnings(iconv(v, "CP1252", "UTF-8"))
  if (is.na(v) || !validUTF8(v)) return(NA_character_)
  k <- gsub("[^a-z0-9]", "", .rp_fold_accents(tolower(v)))
  if (!nzchar(k)) NA_character_ else k
}

# The declared value as a key, or NA when the site declares nothing.
.desg_declared_key <- function(site) {
  if (!is.list(site)) return(NA_character_)
  k <- .notation_key(tryCatch(site[["designation_system"]], error = function(e) NULL))
  if (is.na(k) || k %in% .DESG_UNDECLARED) return(NA_character_)
  k
}

#' What site$designation_system says of the older notation of the horizon
#' designations: "old" (the A2 is read E, the B3 BC and the "(B)" Bi),
#' "present" (none of them is), or NA (nothing declared, "nz_legacy", or a
#' value soilKey does not know: the marks of the designations decide, see
#' \code{.desg_old_notation()}; PedonRecord$validate() notes an unknown
#' value, Hugo's decision Q7, 2026-11-10). Never an error.
#' @noRd
.desg_declared_notation <- function(site) {
  # v0.9.400: read once per value (the session cache of R/memo.R)
  if (!is.list(site)) return(NA_character_)
  ds <- site[["designation_system"]]
  if (is.null(ds)) return(NA_character_)
  if (is.character(ds) && length(ds) == 1L)
    return(.desg_memo("declared", list(ds), function() .desg_declared_notation_read(site)))
  .desg_declared_notation_read(site)
}

.desg_declared_notation_read <- function(site) {
  k <- .desg_declared_key(site)
  if (is.na(k)) return(NA_character_)
  if (k %in% .DESG_OLD_DECLARED || .asc_key_is_asc(k)) return("old")
  if (grepl(.DESG_PRESENT_DECLARED_RE, k) || .za_key_is_za(k) || .ka5_key_is_ka5(k) ||
      .ka5_key_is_ka6(k) || .kidpr_key_is_ru(k) || .rp_key_is_rp(k)) return("present")
  NA_character_
}

#' The note on a value of site$designation_system that soilKey does not
#' know, or NA.
#' @noRd
.desg_notation_note <- function(site) {
  k <- .desg_declared_key(site)
  if (is.na(k) || !is.na(.desg_declared_notation(site))) return(NA_character_)
  sprintf(paste("site$designation_system = '%s' is not a notation soilKey knows:",
                "the older notation (A2 = E, B3 = BC, (B) = Bi) is judged from the",
                "designations"), as.character(unlist(site[["designation_system"]]))[1L])
}

#' Designations of continuous rock (FAO 2006 Guidelines and KST 13: R is hard
#' bedrock; RCr and R/Cr are mostly hard rock). Cr, weathered or soft bedrock
#' that can be dug with a spade and slakes in water, is not continuous rock in
#' WRB 2022 (Ch 3.2.5: it must "remain intact when an air-dried specimen ... is
#' submerged in water for 1 hour") nor a contato litico in SiBCS 2018 (Cap 1:
#' "rochas sas (camada R) ... ou ... majoritariamente por rocha dura (RCr ou
#' R/Cr)"). A material number may precede it (2R).
#' @noRd
.WRB_ROCK_DESIGNATION <- "^[0-9]*R($|[a-z0-9]|/?Cr)"

#' A term of a field description in lower case and without accents, so that a
#' pattern written in ASCII matches it whatever the language and the typing of
#' the source (the accented "Prismatica que se desfaz" of a Brazilian field
#' sheet is "prismatica que se desfaz").
#' Vectorised; NA stays NA.
#' @noRd
.field_term <- function(x) {
  x <- as.character(x)
  x <- gsub("[\u00C1\u00C0\u00C2\u00C3\u00E1\u00E0\u00E2\u00E3]", "a", x)
  x <- gsub("[\u00C9\u00CA\u00E9\u00EA]",                         "e", x)
  x <- gsub("[\u00CD\u00ED]",                                     "i", x)
  x <- gsub("[\u00D3\u00D4\u00D5\u00F3\u00F4\u00F5]",             "o", x)
  x <- gsub("[\u00DA\u00FA]",                                     "u", x)
  x <- gsub("[\u00C7\u00E7]",                                     "c", x)
  tolower(trimws(x))
}

#' Ordinal class of the clay films ("cerosidade") recorded for a layer: 0 none,
#' 1 very few, 2 few, 3 common, 4 many / abundant / dominant, NA unrecorded or
#' unreadable. English (KST, FAO) and Portuguese (Manual de descricao e coleta)
#' terms.
#' @noRd
.clay_films_class <- function(x) {
  x <- tolower(trimws(as.character(x)))
  out <- rep(NA_integer_, length(x))
  out[grepl("^(none|absent|no|nenhuma|ausente|sem)\\b", x)] <- 0L
  out[grepl("^(very few|muito pouc)", x)] <- 1L
  out[is.na(out) & grepl("^(few|pouc)", x)] <- 2L
  out[grepl("^(common|comum)", x)] <- 3L
  out[grepl("^(many|abundant|abundante|dominant|dominante|very many|muita|muito abundante)", x)] <- 4L
  out
}

#' Ordinal class of a grade of development, of structure or of clay films: 0
#' structureless / absent / ausente, 1 weak / fraca, 2 moderate / moderada, 3
#' strong / forte, NA unrecorded or unreadable. English and Portuguese terms.
#' A range is read by its lower end ("Fraca a moderada" is 1, "Moderada a
#' forte" 2): the grade is used where
#' SiBCS asks for a minimum ("grau moderado ou forte"), and a range meets a
#' minimum only if all of it does.
#' @noRd
.field_grade_class <- function(x) {
  x <- .field_term(x)
  out <- rep(NA_integer_, length(x))
  # v0.9.237: a grade recorded as absent is a grade, below weak; it used to be
  # read as not recorded, which passes a minimum.
  out[grepl("^(structureless|absent|ausente|sem estrutura)", x)] <- 0L
  out[grepl("^(very weak|weak|muito frac|frac)", x)] <- 1L
  out[grepl("^(moderate|moderad)", x)] <- 2L
  out[grepl("^(very strong|strong|muito fort|fort)", x)] <- 3L
  out
}

#' Whether an amount recorded on a field sheet (of clay films, of
#' slickensides) is one of `levels`, or at or above the lowest of them on the
#' scale of .clay_films_class() (few < common < many), in English or
#' Portuguese. A range is read by its lower end. A term the scale does not
#' know ("continuous") counts only when it is listed. Vectorised over `x`.
#' @noRd
.amount_at_least <- function(x, levels) {
  cls <- .clay_films_class(.field_term(x))
  thr <- suppressWarnings(min(.clay_films_class(levels), na.rm = TRUE))
  !is.na(x) & (x %in% levels | (is.finite(thr) & !is.na(cls) & cls >= thr))
}

#' Ordinal class of the size of the structural units: 0 ultrapequena, 1 very
#' fine / muito pequena, 2 fine / pequena, 3 medium / media, 4 coarse /
#' grande, 5 very coarse / muito grande; NA unrecorded or unreadable. English
#' and Portuguese terms. A range is read by its lower end ("Pequena a media"
#' is 2, "Media a grande" 3): the size is used where SiBCS asks for a minimum
#' ("blocos ... grandes ou medios"), and a range meets a minimum only if all
#' of it does.
#' @noRd
.structure_size_class <- function(x) {
  x <- .field_term(x)
  out <- rep(NA_integer_, length(x))
  out[grepl("^(ultra ?pequena|extremely fine)", x)] <- 0L
  out[is.na(out) & grepl("^(muito pequena|very fine)", x)] <- 1L
  out[is.na(out) & grepl("^(pequena|fine)", x)] <- 2L
  out[is.na(out) & grepl("^(media|medium)", x)] <- 3L
  out[is.na(out) & grepl("^(muito grande|very coarse|extremely coarse)", x)] <- 5L
  out[is.na(out) & grepl("^(grande|coarse|thick)", x)] <- 4L
  out
}

#' Ordinal class of the drainage of a site, in the eight classes of Anexo B of
#' SiBCS 2018 (p. 314-316): 1 excessivamente, 2 fortemente, 3 acentuadamente,
#' 4 bem, 5 moderadamente, 6 imperfeitamente, 7 mal, 8 muito mal drenado; NA
#' unrecorded or unreadable. English and Portuguese terms. A class written as
#' a range ("imperfeitamente a mal drenado") is read by the end asked for:
#' the drier one by default, as a range meets a minimum only if all of it
#' does, or the wetter one.
#' @noRd
.drainage_class <- function(x, end = c("drier", "wetter")) {
  end <- match.arg(end)
  t <- .field_term(x)
  if (length(t) != 1L || is.na(t) || !nzchar(t)) return(NA_integer_)
  one <- function(p) {
    p <- trimws(p)
    if (grepl("^(excessiv|excessively)", p)) return(1L)
    if (grepl("^(fortemente|somewhat excessively)", p)) return(2L)
    if (grepl("^acentuad", p)) return(3L)
    if (grepl("^(bem|well)", p)) return(4L)
    if (grepl("^(moderad|moderately)", p)) return(5L)
    if (grepl("^(imperfeit|imperfectly|somewhat poorly)", p)) return(6L)
    if (grepl("^(muito mal|very poorly)", p)) return(8L)
    if (grepl("^(mal|poorly)", p)) return(7L)
    NA_integer_
  }
  k <- vapply(strsplit(t, " a | to ")[[1]], one, 0L)
  k <- k[!is.na(k)]
  if (length(k) == 0L) return(NA_integer_)
  if (end == "wetter") max(k) else min(k)
}

#' The number and the family of a Munsell hue ("7.5YR" is 7.5 and "YR"), read
#' with a dot or a comma and whatever the spaces and the case. "N" is the
#' family "N" with no number. What is not a hue gives NA for both.
#' @noRd
.munsell_hue_parts <- function(hue) {
  none <- list(number = NA_real_, family = NA_character_)
  x <- toupper(gsub("[[:space:]]", "", as.character(hue)))
  if (length(x) != 1L || is.na(x) || !nzchar(x)) return(none)
  if (grepl("^N", x)) return(list(number = NA_real_, family = "N"))
  m <- regmatches(x, regexec("^([0-9]+(?:[.,][0-9]+)?)(YR|GY|BG|PB|RP|R|Y|G|B|P)$",
                              x, perl = TRUE))[[1]]
  if (length(m) < 3L) return(none)
  list(number = as.numeric(sub(",", ".", m[2], fixed = TRUE)), family = m[3])
}

#' A list of colours written as Munsell notations, separated by ";"
#' ("10YR 5/3; 2.5YR 4/6"), as the columns `variegated_colors` and
#' `mottle_colors` hold them. `hue`, `value` and `chroma` are vectors with
#' one element per colour; a colour with no hue is left out, and a missing
#' value or chroma leaves its place empty ("10YR 5/"). NA when no colour is
#' left.
#' @noRd
.munsell_notation_list <- function(hue, value, chroma) {
  txt <- function(x) { x <- trimws(as.character(x)); x[is.na(x)] <- ""; x }
  n <- max(length(hue), length(value), length(chroma))
  pad <- function(x) c(txt(x), rep("", n - length(x)))
  hue <- gsub("[[:space:]]+", "", pad(hue))
  hue <- toupper(sub("^([0-9]+),([0-9]+)", "\\1.\\2", hue))
  value <- sub(",", ".", pad(value), fixed = TRUE)
  chroma <- sub(",", ".", pad(chroma), fixed = TRUE)
  keep <- nzchar(hue)
  if (!any(keep)) return(NA_character_)
  paste(sprintf("%s %s/%s", hue[keep], value[keep], chroma[keep]), collapse = "; ")
}

#' The colours of such a list, one row per colour: the number and the family
#' of the hue (as `.munsell_hue_parts()` gives them), the value and the
#' chroma. What is not a notation gives a row of NA; NA or "" gives no row.
#' @noRd
.munsell_notations <- function(x) {
  none <- data.frame(number = numeric(0), family = character(0),
                     value = numeric(0), chroma = numeric(0),
                     stringsAsFactors = FALSE)
  if (length(x) != 1L || is.na(x) || !nzchar(trimws(x))) return(none)
  parts <- trimws(strsplit(as.character(x), ";", fixed = TRUE)[[1]])
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0L) return(none)
  num <- "([0-9]+(?:[.,][0-9]+)?)"
  pat <- paste0("^(N|[0-9]+(?:[.,][0-9]+)?[[:space:]]*[A-Za-z]{1,2})[[:space:]]*",
                num, "?[[:space:]]*(?:/[[:space:]]*", num, "?)?$")
  as_num <- function(v) if (length(v) != 1L || is.na(v) || !nzchar(v)) NA_real_ else
    as.numeric(sub(",", ".", v, fixed = TRUE))
  rows <- lapply(parts, function(p) {
    m <- regmatches(p, regexec(pat, p, perl = TRUE))[[1]]
    if (length(m) < 4L)
      return(data.frame(number = NA_real_, family = NA_character_,
                        value = NA_real_, chroma = NA_real_,
                        stringsAsFactors = FALSE))
    hue <- .munsell_hue_parts(m[2])
    data.frame(number = hue$number, family = hue$family,
               value = as_num(m[3]), chroma = as_num(m[4]),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

#' Tri-state AND: FALSE if any is FALSE, TRUE if all are TRUE, else NA.
#' @noRd
.and3 <- function(...) {
  x <- c(...)
  if (any(x %in% FALSE)) FALSE else if (length(x) && all(x %in% TRUE)) TRUE else NA
}
