# Vertic subgroups of the USDA key: the linear extensibility read from the
# field record (v0.9.428).
#
# Every Vertic subgroup of the KST 13th edition (e.g. Vertic Hapludalfs,
# p. 92; Vertic Argiudolls, p. 260) passes on (1) cracks with slickensides or
# wedge-shaped peds, or (2) a linear extensibility of 6.0 cm or more. The
# linear extensibility needs COLE, a measurement that most profile records
# lack, so without COLE the test was undecided and the name took grade C.
#
# Reading accepted by the author (USDA audit, 2026-11-23): cracks and
# slickensides are field observations, absent when not recorded. A soil with
# a linear extensibility of 6 cm or more cracks and forms slickensides, so a
# profile that records neither has no Vertic subgroup: the test is FALSE. A
# COLE that is recorded still decides, and a profile that records any crack
# or slickenside keeps the test undecided until COLE is measured.

# TRUE when the horizons record any sign of shrink and swell: a crack of any
# width or depth, or slickensides that are not recorded as absent.
.usda_vertic_field_signs <- function(h) {
  if (is.null(h) || !nrow(h)) return(FALSE)
  num <- function(col) {
    v <- h[[col]]
    if (is.null(v)) rep(NA_real_, nrow(h)) else suppressWarnings(as.numeric(v))
  }
  cw <- num("cracks_width_cm"); cd <- num("cracks_depth_cm")
  cracks <- any((!is.na(cw) & cw > 0) | (!is.na(cd) & cd > 0))
  slick <- any(.ve_slickensides(h))
  cracks || slick
}

# Applies the reading to the result of a Vertic subgroup test: an undecided
# result becomes FALSE when the profile records no crack and no slickenside.
.usda_vertic_field_reading <- function(res, pedon) {
  if (!is.na(res$passed) || .usda_vertic_field_signs(pedon$horizons)) return(res)
  res$passed <- FALSE
  res$evidence <- c(res$evidence %||% list(),
                    list(linear_extensibility_from_field = paste(
                      "COLE not recorded; no crack and no slickenside recorded,",
                      "so no linear extensibility of 6 cm or more")))
  res$missing <- setdiff(res$missing %||% character(0), "cole_value")
  res
}
