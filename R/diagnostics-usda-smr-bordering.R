# Aridic moisture regime that borders on ustic or xeric (v0.9.429).
#
# The Xerolls and Ustolls (KST 13th ed., Mollisol suborders) take a Mollisol
# with a xeric or an ustic regime, or "an aridic soil moisture regime that
# borders on" xeric or ustic. KST 13, p. 66, leaves the phrase without a
# fixed definition: an aridic regime slightly too dry to be xeric or ustic,
# ustic when most rain falls in the warm season, xeric when in the cool one.
#
# Read from the moisture subclass of NASIS (site$soil_moisture_subclass),
# whose last word is the regime and first word the neighbour it borders on:
# "ustic aridic", "xeric aridic". Decided by the author (USDA audit N17,
# 2026-11-23): an aridic regime recorded without a subclass leaves both
# suborders undecided; it is never Udolls by default.

#' Aridic soil moisture regime bordering on ustic or xeric (USDA)
#'
#' TRUE when an aridic (torric) regime is recorded and the moisture subclass
#' names \code{side} as the neighbour ("ustic aridic"); FALSE when another
#' regime is recorded, or the subclass is typic or names the other
#' neighbour; NA when the regime is aridic and the subclass is not recorded,
#' or no regime is recorded.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param side "ustic" or "xeric".
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
smr_aridic_bordering_usda <- function(pedon, side = c("ustic", "xeric")) {
  side <- match.arg(side)
  raw <- pedon$site$soil_moisture_regime %||% NA_character_
  blank <- .usda_regime_blank(raw)
  smr <- if (blank) NA_character_ else trimws(.usda_smr_value(as.character(raw)[1L]))
  sub <- tolower(trimws(as.character(pedon$site$soil_moisture_subclass %||% NA_character_)[1L]))
  words <- if (is.na(sub) || !nzchar(sub)) character(0) else strsplit(sub, "[^a-z]+")[[1]]
  words <- words[nzchar(words)]
  missing <- character(0)
  passed <- if (blank) {
    missing <- "site$soil_moisture_regime"; NA
  } else if (!smr %in% c("aridic", "torric")) {
    FALSE
  } else if (length(words) < 2L || !words[length(words)] %in% c("aridic", "torric")) {
    missing <- "site$soil_moisture_subclass"; NA
  } else if (words[1L] == side) {
    TRUE
  } else if (words[1L] %in% c("typic", "ustic", "xeric")) {
    FALSE
  } else {
    missing <- "site$soil_moisture_subclass"; NA
  }
  DiagnosticResult$new(
    name = paste0("smr_aridic_bordering_", side, "_usda"), passed = passed,
    layers = integer(0),
    evidence = list(soil_moisture_regime = raw, soil_moisture_subclass = sub, side = side),
    missing = missing,
    reference = "Soil Survey Staff (2022), KST 13ed, p. 66; Mollisol suborders"
  )
}
