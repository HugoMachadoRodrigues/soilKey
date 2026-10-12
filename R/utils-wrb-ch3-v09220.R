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
#' @noRd
.desg_master <- function(d) {
  sub("^(?:[0-9]+(?=[A-Z])|[IVX]+(?=[A-HJ-UWYZ]))", "", trimws(as.character(d)),
      perl = TRUE)
}

#' The horizons with their designations read by .desg_master(), for the tests
#' that take the name of a column.
#' @noRd
.h_master <- function(h) {
  h <- data.table::copy(h)
  h$designation <- .desg_master(h$designation)
  h
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

#' Ordinal class of a grade of development, of structure or of clay films: 1
#' weak / fraca, 2 moderate / moderada, 3 strong / forte, NA unrecorded or
#' unreadable. English and Portuguese terms. A range is read by its lower end
#' ("Fraca a moderada" is 1, "Moderada a forte" 2): the grade is used where
#' SiBCS asks for a minimum ("grau moderado ou forte"), and a range meets a
#' minimum only if all of it does.
#' @noRd
.grade_class <- function(x) {
  x <- .field_term(x)
  out <- rep(NA_integer_, length(x))
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

#' Tri-state AND: FALSE if any is FALSE, TRUE if all are TRUE, else NA.
#' @noRd
.and3 <- function(...) {
  x <- c(...)
  if (any(x %in% FALSE)) FALSE else if (length(x) && all(x %in% TRUE)) TRUE else NA
}
