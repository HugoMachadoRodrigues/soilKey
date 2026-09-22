# Tests for the v0.9.168 report redesign: branded logo header, a self-contained
# locator map, and multi-profile pagination (map on page 1, one page each).

.mk_pedon <- function(id, lat, lon) {
  p <- make_ferralsol_canonical()
  p$site$id <- id; p$site$lat <- lat; p$site$lon <- lon
  p
}

test_that(".report_pedon_points keeps only finite coordinates", {
  p1 <- .mk_pedon("A", -22.5, -43.7)
  p2 <- .mk_pedon("B", NA, NA)
  pts <- soilKey:::.report_pedon_points(list(p1, p2))
  expect_equal(nrow(pts), 1L)
  expect_equal(pts$id, "A")
})

test_that("the locator map is a self-contained PNG data URI (or empty)", {
  skip_if_not_installed("base64enc")
  uri <- soilKey:::.report_map_data_uri(list(.mk_pedon("A", -22.5, -43.7)))
  expect_true(startsWith(uri, "data:image/png;base64,"))
  expect_gt(nchar(uri), 500L)
  # no coordinates -> empty
  expect_identical(soilKey:::.report_map_data_uri(list(.mk_pedon("A", NA, NA))),
                   "")
})

test_that("single-profile HTML report has a branded header and a map, and stays self-contained", {
  p   <- .mk_pedon("FR-canonical-01", -22.5, -43.7)
  out <- tempfile(fileext = ".html")
  report_html(p, file = out, pedon = p)
  body <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(body, 'class="report-header"', fixed = TRUE)
  expect_match(body, "data:image/png;base64", fixed = TRUE)   # logo and/or map
  expect_match(body, 'class="map-card"', fixed = TRUE)
  # self-contained: no external stylesheet or remote image
  expect_false(grepl("<link[^>]*href", body, ignore.case = TRUE))
  expect_false(grepl('src="https?://', body, ignore.case = TRUE))
})

test_that("a list of PedonRecords produces a multi-profile report", {
  peds <- list(.mk_pedon("P01", -22.7, -43.6),
               .mk_pedon("P02", -12.4, -45.1),
               .mk_pedon("P03", -30.0, -51.2))
  expect_true(soilKey:::.report_multi_pedons(peds))
  out <- tempfile(fileext = ".html")
  report_html(peds, file = out)
  body <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(body, 'class="map-card"', fixed = TRUE)         # overview map
  # one page per profile
  n_pages <- length(gregexpr('class="profile-page"', body)[[1]])
  expect_equal(n_pages, length(peds))
  expect_true(all(vapply(c("P01", "P02", "P03"),
                         function(id) grepl(id, body), logical(1))))
})

test_that("a single ClassificationResult is NOT treated as multi-profile", {
  r <- classify_wrb2022(make_ferralsol_canonical())
  expect_false(soilKey:::.report_multi_pedons(r))
  expect_false(soilKey:::.report_multi_pedons(list(r)))  # list of results, not pedons
})
