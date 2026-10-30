# v0.9.317: the clay ratio of the Hungarian clay accumulation horizon (HSCS
# 2024, 8.2) passes over a transitional AB, BA, EB or BE horizon with no clay
# recorded and reads the row against the horizon above it (Hugo's reading of
# 2026-10-30). Made-up profiles.

.hu35 <- function(designation, clay, texture = NA_character_) {
  n <- length(designation)
  tops <- c(0, 10, 20, 32, 70)[seq_len(n)]
  bots <- c(10, 20, 32, 70, 120)[seq_len(n)]
  silt <- ifelse(is.na(clay), NA, 60)
  sand <- ifelse(is.na(clay), NA, 40 - clay)
  PedonRecord$new(site = list(id = "hu35"), horizons = ensure_horizon_schema(data.table::data.table(
    designation = designation, top_cm = tops, bottom_cm = bots,
    clay_pct = clay, silt_pct = silt, sand_pct = sand, field_texture = texture)))
}
.hu35_rows <- function(p) soilKey:::.hscs_clay_accum_rows(soilKey:::.hscs_horizons(p))

test_that("an EB with no clay is passed over: the Bt is read against the E", {
  skip_on_cran()
  p <- .hu35(c("A", "E", "EB", "Bt", "C"), c(20, 20, NA, 30, 18))
  expect_true(.hu35_rows(p)[4])
  expect_true(clay_illuvial_type_hscs(p)$passed)
  # the E has too much clay for the ratio: the Bt is no clay accumulation
  p <- .hu35(c("A", "E", "EB", "Bt", "C"), c(25, 25, NA, 30, 18))
  expect_false(.hu35_rows(p)[4])
})

test_that("AB, BA and BE are passed over the same way", {
  skip_on_cran()
  for (d in c("AB", "BA", "BE")) {
    p <- .hu35(c("A", "E", d, "Bt", "C"), c(20, 20, NA, 30, 18))
    expect_true(.hu35_rows(p)[4], info = d)
  }
})

test_that("a transitional horizon with a texture class, or a horizon that is not transitional, is not passed over", {
  skip_on_cran()
  # silt loam, 0 to 27 % clay: the 30 % of the Bt may or may not be 1.4 times it
  p <- .hu35(c("A", "E", "EB", "Bt", "C"), c(20, 20, NA, 30, 18), texture = c(NA, NA, "silt loam", NA, NA))
  expect_true(is.na(.hu35_rows(p)[4]))
  # a Bw with no clay stays a horizon of unknown clay above the Bt
  p <- .hu35(c("A", "E", "Bw", "Bt", "C"), c(20, 20, NA, 30, 18))
  expect_true(is.na(.hu35_rows(p)[4]))
})
