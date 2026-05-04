# Tests for v0.9.34 aqp interoperability (as_aqp + from_aqp).

skip_if_no_aqp <- function() {
  testthat::skip_if_not_installed("aqp")
}


# ---- as_aqp ---------------------------------------------------------------

test_that("as_aqp converts a single PedonRecord to a 1-profile SPC", {
  skip_if_no_aqp()
  p <- make_ferralsol_canonical()
  spc <- as_aqp(p)
  expect_s4_class(spc, "SoilProfileCollection")
  expect_equal(length(spc), 1L)
  # idcol default is "id"; profile id should match site$id.
  expect_equal(aqp::profile_id(spc)[[1]], as.character(p$site$id))
})

test_that("as_aqp converts a list of PedonRecords to multi-profile SPC", {
  skip_if_no_aqp()
  pedons <- list(make_ferralsol_canonical(),
                   make_luvisol_canonical(),
                   make_chernozem_canonical())
  spc <- as_aqp(pedons)
  expect_equal(length(spc), 3L)
  expect_setequal(aqp::profile_id(spc),
                    vapply(pedons, function(p) as.character(p$site$id),
                             character(1)))
})

test_that("as_aqp renames soilKey horizon columns to aqp canonical", {
  skip_if_no_aqp()
  p <- make_ferralsol_canonical()
  spc <- as_aqp(p)
  hz <- aqp::horizons(spc)
  expect_true("top" %in% names(hz))
  expect_true("bottom" %in% names(hz))
  expect_true("name" %in% names(hz))     # was "designation"
  expect_true("clay" %in% names(hz))     # was "clay_pct"
  expect_true("sand" %in% names(hz))
  expect_true("silt" %in% names(hz))
  # Unrenamed columns kept verbatim:
  if ("ph_h2o" %in% names(p$horizons))
    expect_true("ph_h2o" %in% names(hz))
})

test_that("as_aqp attaches site-level metadata", {
  skip_if_no_aqp()
  p <- make_ferralsol_canonical()
  spc <- as_aqp(p)
  st <- aqp::site(spc)
  expect_equal(nrow(st), 1L)
  if (!is.null(p$site$lat))
    expect_equal(st$lat[[1]], p$site$lat)
  if (!is.null(p$site$country))
    expect_equal(st$country[[1]], p$site$country)
})


# ---- from_aqp -------------------------------------------------------------

test_that("from_aqp converts SPC back to a list of PedonRecord", {
  skip_if_no_aqp()
  pedons <- list(make_ferralsol_canonical(), make_luvisol_canonical())
  spc <- as_aqp(pedons)
  back <- from_aqp(spc)
  expect_length(back, 2L)
  for (b in back) expect_s3_class(b, "PedonRecord")
})

test_that("from_aqp renames aqp columns back to soilKey conventions", {
  skip_if_no_aqp()
  p <- make_ferralsol_canonical()
  spc <- as_aqp(p)
  back <- from_aqp(spc)[[1]]
  hz <- back$horizons
  expect_true("top_cm"      %in% names(hz))
  expect_true("bottom_cm"   %in% names(hz))
  expect_true("designation" %in% names(hz))
  expect_true("clay_pct"    %in% names(hz))
  expect_true("sand_pct"    %in% names(hz))
  expect_true("silt_pct"    %in% names(hz))
})


# ---- round-trip property --------------------------------------------------

test_that("round-trip: from_aqp(as_aqp(pedon)) preserves horizon chemistry", {
  skip_if_no_aqp()
  p <- make_ferralsol_canonical()
  spc <- as_aqp(p)
  back <- from_aqp(spc)[[1]]
  # Numeric columns should round-trip exactly (modulo possible NA reorder).
  for (col in c("clay_pct", "sand_pct", "silt_pct", "ph_h2o", "oc_pct")) {
    if (col %in% names(p$horizons) && col %in% names(back$horizons)) {
      expect_equal(as.numeric(back$horizons[[col]]),
                     as.numeric(p$horizons[[col]]),
                     info = paste("column:", col))
    }
  }
})

test_that("round-trip preserves designation and depth columns", {
  skip_if_no_aqp()
  p <- make_luvisol_canonical()
  spc <- as_aqp(p)
  back <- from_aqp(spc)[[1]]
  expect_equal(back$horizons$designation, p$horizons$designation)
  expect_equal(back$horizons$top_cm,      p$horizons$top_cm)
  expect_equal(back$horizons$bottom_cm,   p$horizons$bottom_cm)
})

test_that("round-trip preserves the site id", {
  skip_if_no_aqp()
  p <- make_chernozem_canonical()
  spc <- as_aqp(p)
  back <- from_aqp(spc)[[1]]
  expect_equal(back$site$id, p$site$id)
})


# ---- error handling -------------------------------------------------------

test_that("as_aqp errors on non-PedonRecord input", {
  skip_if_no_aqp()
  expect_error(as_aqp(42), "PedonRecord")
  expect_error(as_aqp(list(1, 2, 3)), "PedonRecord")
})

test_that("from_aqp errors on non-SPC input", {
  skip_if_no_aqp()
  expect_error(from_aqp(data.frame(id = "a", top = 0, bottom = 10)),
                 "SoilProfileCollection")
  expect_error(from_aqp(list()), "SoilProfileCollection")
})


# ---- classification works on round-tripped pedons --------------------------

test_that("classify_wrb2022 returns same RSG before and after aqp round-trip", {
  skip_if_no_aqp()
  p <- make_ferralsol_canonical()
  res_before <- classify_wrb2022(p, on_missing = "silent")
  back <- from_aqp(as_aqp(p))[[1]]
  res_after <- classify_wrb2022(back, on_missing = "silent")
  expect_equal(res_before$rsg_or_order, res_after$rsg_or_order)
})

test_that("classify_usda returns same Order before and after aqp round-trip", {
  skip_if_no_aqp()
  p <- make_ferralsol_canonical()
  res_before <- classify_usda(p, on_missing = "silent")
  back <- from_aqp(as_aqp(p))[[1]]
  res_after <- classify_usda(back, on_missing = "silent")
  expect_equal(res_before$rsg_or_order, res_after$rsg_or_order)
})


# ---- multi-profile heterogeneous schema -----------------------------------

test_that("as_aqp pads missing columns across profiles with different schemas", {
  skip_if_no_aqp()
  # Build two pedons with intentionally different horizon columns.
  hz1 <- ensure_horizon_schema(data.table::data.table(
    top_cm = c(0, 30), bottom_cm = c(30, 60),
    designation = c("A", "Bw"),
    clay_pct = c(20, 25), oc_pct = c(2, 0.5)))
  hz2 <- ensure_horizon_schema(data.table::data.table(
    top_cm = 0, bottom_cm = 25,
    designation = "A",
    clay_pct = 10, sand_pct = 80))   # no oc_pct
  p1 <- PedonRecord$new(
    site = list(id = "p1", lat = 0, lon = 0, country = "TEST"),
    horizons = hz1)
  p2 <- PedonRecord$new(
    site = list(id = "p2", lat = 0, lon = 0, country = "TEST"),
    horizons = hz2)
  spc <- as_aqp(list(p1, p2))
  expect_equal(length(spc), 2L)
})
