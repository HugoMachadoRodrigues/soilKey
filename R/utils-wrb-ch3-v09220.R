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

#' Designations of continuous rock (FAO 2006 Guidelines and KST 13: R is hard
#' bedrock; RCr and R/Cr are mostly hard rock). Cr, weathered or soft bedrock
#' that can be dug with a spade and slakes in water, is not continuous rock in
#' WRB 2022 (Ch 3.2.5: it must "remain intact when an air-dried specimen ... is
#' submerged in water for 1 hour") nor a contato litico in SiBCS 2018 (Cap 1:
#' "rochas sas (camada R) ... ou ... majoritariamente por rocha dura (RCr ou
#' R/Cr)"). A material number may precede it (2R).
#' @noRd
.WRB_ROCK_DESIGNATION <- "^[0-9]*R($|[a-z0-9]|/?Cr)"

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

#' Tri-state AND: FALSE if any is FALSE, TRUE if all are TRUE, else NA.
#' @noRd
.and3 <- function(...) {
  x <- c(...)
  if (any(x %in% FALSE)) FALSE else if (length(x) && all(x %in% TRUE)) TRUE else NA
}
