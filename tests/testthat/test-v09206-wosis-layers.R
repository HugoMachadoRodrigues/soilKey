# v0.9.206: the WoSIS picker loads horizons, within ISRIC's terms and limits.
#
# Every response below is SYNTHETIC: invented numbers in the shape of ISRIC's
# GraphQL replies. soilKey ships no third-party data, and that includes test
# fixtures. Live checks against the real endpoint are at the end, behind
# SOILKEY_TEST_WOSIS_LIVE, so the suite never depends on ISRIC being up.
#
# Several tests pin defects found while building this release:
#   * the search asked ISRIC for 200 profiles at once; ISRIC accepts at most 100
#     and answered HTTP 400, so the picker never listed anything in the app;
#   * a profile with no layers crashed the whole search;
#   * the country typed by the user was pasted into the query text;
#   * an unknown licence was labelled "CC BY 3.0".

# ---- a fake ISRIC ----------------------------------------------------------

.v <- function(x) if (is.na(x)) list() else list(list(value = list(as.character(x)),
                                                     valueAvg = x, methodOptions = "m1"))

.fake_layers <- function() list(
  # deliberately out of depth order; one NonCommercial layer
  list(layerId = 3L, upperDepth = 30L, lowerDepth = 60L, layerName = "Bt",
       organicSurface = FALSE, datasetId = "XX-TEST",
       licence = "Attribution-NonCommercial 4.0 International (CC BY-NC 4.0), https://creativecommons.org/licenses/by-nc/4.0/",
       clayValues = .v(40), orgcValues = .v(5), tceqValues = .v(150),
       bdfi33lValues = .v(NA), bdfiodValues = .v(1.45), phaqValues = .v(6.1)),
  list(layerId = 1L, upperDepth = 0L, lowerDepth = 15L, layerName = "A",
       organicSurface = FALSE, datasetId = "XX-TEST",
       licence = "Attribution 4.0 International (CC BY 4.0), https://creativecommons.org/licenses/by/4.0/",
       clayValues = .v(20), orgcValues = .v(25), tceqValues = .v(NA),
       bdfi33lValues = .v(1.20), bdfiodValues = .v(1.30), phaqValues = .v(5.5)),
  list(layerId = 2L, upperDepth = 15L, lowerDepth = 30L, layerName = "AB",
       organicSurface = FALSE, datasetId = "XX-TEST",
       licence = "Attribution 4.0 International (CC BY 4.0), https://creativecommons.org/licenses/by/4.0/",
       # valueAvg missing: the mean of the reported values is used instead
       clayValues = list(list(value = list("30", "34"), valueAvg = NULL, methodOptions = "")),
       orgcValues = .v(12), tceqValues = .v(NA),
       bdfi33lValues = .v(NA), bdfiodValues = .v(NA), phaqValues = .v(5.8)))

# Routes a query to a synthetic answer and records every call.
.fake_isric <- function(layers = .fake_layers(), profiles = NULL, calls = NULL) {
  function(query, variables = NULL, timeout = 30) {
    if (!is.null(calls)) calls$log[[length(calls$log) + 1L]] <-
      list(query = query, variables = variables)
    n <- variables$n %||% 100L; o <- variables$o %||% 0L
    page <- function(x) x[seq_along(x) > o & seq_along(x) <= o + n]
    if (grepl("wosisLatestProfiles", query))
      return(list(data = list(wosisLatestProfiles = page(profiles)), error = NULL))
    if (grepl("Values\\(first", query))
      return(list(data = list(wosisLatestLayers = page(layers)), error = NULL))
    if (grepl("profileId lowerDepth upperDepth", query))
      return(list(data = list(wosisLatestLayers = list()), error = NULL))
    # the cheap count query
    list(data = list(wosisLatestLayers = page(lapply(layers, function(l)
      list(layerId = l$layerId)))), error = NULL)
  }
}


# ---- units, mapping and provenance ----------------------------------------

test_that("the layer map converts g/kg to % and maps only matching methods", {
  m <- soilKey:::.wosis_layer_map()
  expect_true(all(m$column %in% names(horizon_column_spec())))
  expect_equal(m$factor[m$code %in% c("ORGC", "NITKJD", "TCEQ")], c(0.1, 0.1, 0.1))
  # methods that do not mean what soilKey's column means are never mapped
  expect_false(any(c("CFGR", "ELCO20", "ELCO25", "ELCO50", "WV0033", "WV1500",
                     "CECPH8") %in% m$code))
})

test_that("layers arrive top to bottom, in soilKey units, with provenance", {
  local_mocked_bindings(.wosis_post = .fake_isric())
  h <- read_wosis_layers_graphql(1L)
  expect_equal(h$top_cm, c(0, 15, 30))
  expect_equal(h$designation, c("A", "AB", "Bt"))
  expect_equal(h$oc_pct, c(2.5, 1.2, 0.5))           # 25, 12, 5 g/kg
  expect_equal(h$caco3_pct, c(NA, NA, 15))           # 150 g/kg
  expect_equal(h$clay_pct, c(20, 32, 40))            # 32 = mean of "30", "34"
  pv <- attr(h, "provenance")
  expect_true(all(pv$source == "measured"))
  expect_true(any(grepl("^WoSIS ORGC \\(g/kg\\); method: m1", pv$notes)))
})

test_that("bulk density prefers BDFI33 and falls back to BDFIOD, saying which", {
  local_mocked_bindings(.wosis_post = .fake_isric())
  h <- read_wosis_layers_graphql(1L)
  expect_equal(h$bulk_density_g_cm3, c(1.20, NA, 1.45))
  pv <- attr(h, "provenance")
  bd <- pv[pv$attribute == "bulk_density_g_cm3", ]
  expect_match(bd$notes[bd$horizon_idx == 1], "BDFI33", fixed = TRUE)
  expect_match(bd$notes[bd$horizon_idx == 3], "BDFIOD", fixed = TRUE)
})

test_that("the most restrictive licence among the layers wins", {
  local_mocked_bindings(.wosis_post = .fake_isric())
  h <- read_wosis_layers_graphql(1L)
  expect_match(attr(h, "licence"), "NonCommercial", fixed = TRUE)
  expect_identical(attr(h, "dataset"), "XX-TEST")
})

test_that("depths above the mineral surface are shifted to start at 0 cm", {
  lay <- .fake_layers()
  lay[[2]]$upperDepth <- -5L; lay[[2]]$lowerDepth <- 0L   # an O layer, -5..0
  local_mocked_bindings(.wosis_post = .fake_isric(layers = lay))
  h <- read_wosis_layers_graphql(1L)
  expect_equal(h$top_cm[1], 0)
  expect_equal(attr(h, "depth_shift_cm"), 5)
  expect_true(all(h$bottom_cm > h$top_cm))
})


# ---- load on ISRIC --------------------------------------------------------

test_that("layers are read in pages, in a stable order", {
  many <- lapply(1:25, function(i) list(
    layerId = i, upperDepth = (i - 1L) * 4L, lowerDepth = i * 4L, layerName = paste0("L", i),
    organicSurface = FALSE, datasetId = "XX", licence = "Attribution 4.0 International (CC BY 4.0)",
    clayValues = .v(20)))
  calls <- new.env(); calls$log <- list()
  local_mocked_bindings(.wosis_post = .fake_isric(layers = many, calls = calls))
  h <- read_wosis_layers_graphql(1L, page_size = 10L)
  expect_equal(nrow(h), 25L)
  prop <- Filter(function(cl) grepl("Values\\(first", cl$query), calls$log)
  expect_equal(vapply(prop, function(cl) cl$variables$o, integer(1)), c(0L, 10L, 20L))
  expect_true(all(grepl("orderBy: \\[LAYER_ID_ASC\\]", vapply(calls$log, `[[`, "", "query"))))
})

test_that("a profile of fine depth increments is refused before the heavy query", {
  many <- lapply(1:50, function(i) list(layerId = i))
  calls <- new.env(); calls$log <- list()
  local_mocked_bindings(.wosis_post = .fake_isric(layers = many, calls = calls))
  h <- read_wosis_layers_graphql(1L)
  expect_equal(nrow(h), 0L)
  expect_match(attr(h, "wosis_error"), "50 layers", fixed = TRUE)
  expect_match(attr(h, "wosis_error"), "max_layers = 50", fixed = TRUE)
  # only the cheap count ran; the property query was never sent
  expect_false(any(grepl("Values\\(first", vapply(calls$log, `[[`, "", "query"))))
})

test_that("a network failure returns zero rows with the reason", {
  local_mocked_bindings(.wosis_post = function(...) list(data = NULL, error = "timeout"))
  h <- read_wosis_layers_graphql(1L)
  expect_equal(nrow(h), 0L)
  expect_identical(attr(h, "wosis_error"), "timeout")
})


# ---- the profile search ----------------------------------------------------

.fake_profiles <- function(k, licence = "Attribution 4.0 International (CC BY 4.0)",
                           with_layers = TRUE) {
  lapply(seq_len(k), function(i) list(
    profileId = 1000L + i, profileCode = paste0("P", i), countryName = "Testland",
    latitude = 1, longitude = 2, wrbReferenceSoilGroup = "Cambisol",
    usdaOrderName = NULL, datasetCode = "XX",
    layers = if (with_layers) list(list(licence = licence)) else list()))
}

test_that("the search never asks ISRIC for more than 100 profiles at once", {
  # Regression: it asked for 200 and ISRIC answered HTTP 400, so the picker
  # listed nothing in the app.
  calls <- new.env(); calls$log <- list()
  nc <- "Attribution-NonCommercial 4.0 International (CC BY-NC 4.0)"
  local_mocked_bindings(.wosis_post = .fake_isric(
    profiles = .fake_profiles(250, licence = nc), calls = calls))
  a <- read_wosis_profiles_graphql(n_max = 50L)
  ns <- vapply(Filter(function(cl) grepl("wosisLatestProfiles", cl$query), calls$log),
               function(cl) cl$variables$n, integer(1))
  expect_true(length(ns) > 1L)                 # it paged
  expect_true(all(ns <= 100L))
  expect_equal(nrow(a), 0L)                    # all NonCommercial
  expect_equal(attr(a, "wosis_excluded"), 250L)
})

test_that("the scan stops at max_scan", {
  calls <- new.env(); calls$log <- list()
  nc <- "Attribution-NonCommercial 4.0 International (CC BY-NC 4.0)"
  local_mocked_bindings(.wosis_post = .fake_isric(
    profiles = .fake_profiles(1000, licence = nc), calls = calls))
  read_wosis_profiles_graphql(n_max = 50L, max_scan = 300L)
  pages <- Filter(function(cl) grepl("wosisLatestProfiles", cl$query), calls$log)
  expect_equal(length(pages), 3L)
})

test_that("profiles without layers are left out and counted, not crashed on", {
  local_mocked_bindings(.wosis_post = .fake_isric(
    profiles = c(.fake_profiles(3), .fake_profiles(2, with_layers = FALSE))))
  a <- read_wosis_profiles_graphql(n_max = 10L)
  expect_equal(nrow(a), 3L)
  expect_equal(attr(a, "wosis_no_layers"), 2L)
  expect_true(all(c("n_layers", "depth_cm") %in% names(a)))
})

test_that("the country is sent as a variable, never pasted into the query", {
  calls <- new.env(); calls$log <- list()
  local_mocked_bindings(.wosis_post = .fake_isric(profiles = list(), calls = calls))
  evil <- 'X"}) { profileId } }#'
  read_wosis_profiles_graphql(country = evil, n_max = 5L)
  cl <- calls$log[[1]]
  expect_false(grepl(evil, cl$query, fixed = TRUE))
  expect_identical(cl$variables$f$countryName$equalTo, evil)
})

test_that("an unknown licence is labelled unknown, not permissive", {
  expect_true(is.na(soilKey:::.wosis_licence_short(NA)))
  expect_true(is.na(soilKey:::.wosis_licence_short(NA_character_)))
  expect_identical(soilKey:::.wosis_licence_short(c("Attribution 4.0 International (CC BY 4.0)", NA)),
                   c("CC BY 4.0", NA))
})


# ---- attribution -----------------------------------------------------------

test_that("the citation is the one ISRIC asks for, with the date", {
  cit <- wosis_citation(as.Date("2026-10-06"))
  expect_match(cit, "Batjes NH, Calisto L and de Sousa LM, 2024. WoSIS-latest", fixed = TRUE)
  expect_match(cit, "https://tinyurl.com/39xhaa9d", fixed = TRUE)
  expect_match(cit, "Date downloaded: 2026-10-06.", fixed = TRUE)
})

.row <- data.frame(profile_id = "1", profile_code = "P1", country = "Testland",
                   lat = 1, lon = 2, wrb_rsg = "Cambisol", usda_order = NA_character_,
                   dataset = "XX-TEST", licence = "Attribution 4.0 International (CC BY 4.0)",
                   licence_short = "CC BY 4.0", stringsAsFactors = FALSE)

test_that("a pedon from WoSIS has horizons, provenance and attribution", {
  local_mocked_bindings(.wosis_post = .fake_isric())
  p <- wosis_profile_to_pedon(.row)
  expect_s3_class(p, "PedonRecord")
  expect_equal(nrow(p$horizons), 3L)
  expect_true(nrow(p$provenance) > 0L)
  expect_match(p$site$citation, "WoSIS-latest", fixed = TRUE)
  expect_match(p$site$attribution, "ISRIC", fixed = TRUE)
  # the listing said CC BY, a layer said CC BY-NC: the stricter one is kept
  expect_match(p$site$licence, "NonCommercial", fixed = TRUE)
  expect_identical(p$site$licence_short, "CC BY-NC 4.0")
})

test_that("fetch_layers = FALSE makes no network call", {
  local_mocked_bindings(.wosis_post = function(...) stop("must not be called"))
  p <- wosis_profile_to_pedon(.row, fetch_layers = FALSE)
  expect_null(p$site$wosis_error)
  expect_match(p$site$citation, "WoSIS-latest", fixed = TRUE)
})

test_that("a profile that cannot be read says why, with no horizons", {
  local_mocked_bindings(.wosis_post = function(...) list(data = NULL, error = "down"))
  p <- wosis_profile_to_pedon(.row)
  expect_identical(p$site$wosis_error, "down")
  expect_true(is.null(p$horizons) || nrow(p$horizons) == 0L)
})

test_that("the report carries the source and the citation", {
  local_mocked_bindings(.wosis_post = .fake_isric())
  p <- wosis_profile_to_pedon(.row)
  html <- soilKey:::.html_site_header(p)
  expect_match(html, "WoSIS-latest", fixed = TRUE)
  expect_match(html, "tinyurl.com/39xhaa9d", fixed = TRUE)
  md <- soilKey:::.rmd_site_block(p)
  expect_match(md, "WoSIS-latest", fixed = TRUE)
})

test_that("the app shows where WoSIS horizons came from", {
  skip_on_cran()
  skip_if_not_installed("shiny"); skip_if_not_installed("bslib")
  skip_if_not_installed("DT");    skip_if_not_installed("shinyWidgets")
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d)) d <- file.path("inst", "shiny", "classify_app_pro")
  e <- new.env(parent = globalenv())
  for (f in list.files(file.path(d, "R"), pattern = "\\.R$", full.names = TRUE))
    sys.source(f, envir = e)
  h <- as.character(e$pedon_ui("p"))
  expect_true(grepl("p-data_source_note", h, fixed = TRUE))
})


# ---- live, against ISRIC ---------------------------------------------------

.live <- function() isTRUE(as.logical(Sys.getenv("SOILKEY_TEST_WOSIS_LIVE", "false")))

test_that("LIVE: the search works as the app calls it", {
  skip_on_cran()
  skip_if_not(.live(), "set SOILKEY_TEST_WOSIS_LIVE=true to query ISRIC")
  a <- read_wosis_profiles_graphql(country = "Argentina", n_max = 50L,
                                   licence_filter = "permissive")
  expect_null(attr(a, "wosis_error"))
  expect_gt(nrow(a), 0L)
  expect_true(all(is.finite(a$n_layers)))
})

test_that("LIVE: a profile's layers load and organic carbon is ORGC / 10", {
  skip_on_cran()
  skip_if_not(.live(), "set SOILKEY_TEST_WOSIS_LIVE=true to query ISRIC")
  a <- read_wosis_profiles_graphql(country = "Argentina", n_max = 10L)
  a <- a[a$n_layers >= 2 & a$n_layers <= 10, ][1, ]
  h <- read_wosis_layers_graphql(a$profile_id)
  expect_gt(nrow(h), 1L)
  expect_true(all(diff(h$top_cm) > 0))
  raw <- soilKey:::.wosis_post(
    "query($f: WosisLatestLayerFilter) { wosisLatestLayers(filter: $f, first: 50, orderBy: [UPPER_DEPTH_ASC]) { orgcValues(first: 3) { valueAvg } } }",
    list(f = list(profileId = list(equalTo = as.integer(a$profile_id)))))
  orgc <- vapply(raw$data$wosisLatestLayers, function(l)
    if (length(l$orgcValues)) as.numeric(l$orgcValues[[1]]$valueAvg) else NA_real_, numeric(1))
  ok <- is.finite(orgc) & is.finite(h$oc_pct)
  if (any(ok)) expect_equal(h$oc_pct[ok], orgc[ok] / 10)
})
