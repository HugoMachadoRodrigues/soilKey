# v0.9.396: the offline FAO-Unesco benchmark on WoSIS profiles
# (inst/benchmarks/fao_wosis.R) and the conversion step it shares with the
# live WoSIS reader (.wosis_layers_to_horizons()).
#
# Every layer and profile below is SYNTHETIC: invented numbers in the shape of
# ISRIC's GraphQL replies. soilKey ships no WoSIS data, test fixtures
# included.

.v396 <- function(x) if (is.na(x)) list() else list(list(value = list(as.character(x)),
                                                        valueAvg = x, methodOptions = "m1"))
.by396 <- "Attribution 4.0 International (CC BY 4.0), https://creativecommons.org/licenses/by/4.0/"
.nc396 <- "Attribution-NonCommercial 4.0 International (CC BY-NC 4.0), https://creativecommons.org/licenses/by-nc/4.0/"

.layer396 <- function(id, top, bot, name = NA, clay = NA, orgc = NA, tceq = NA, phaq = NA,
                      cec = NA, licence = .by396) {
  l <- list(layerId = id, upperDepth = top, lowerDepth = bot, organicSurface = FALSE,
            datasetId = "XX-TEST", licence = licence,
            clayValues = .v396(clay), siltValues = .v396(if (is.na(clay)) NA else 30),
            sandValues = .v396(if (is.na(clay)) NA else 70 - clay),
            orgcValues = .v396(orgc), tceqValues = .v396(tceq), phaqValues = .v396(phaq),
            cecph7Values = .v396(cec))
  if (!is.na(name)) l$layerName <- name
  l
}

.layers396 <- function(licence = .by396) list(
  .layer396(3L, 30L, 80L, "Bw", clay = 35, orgc = 4, phaq = 6.0, cec = 12, licence = licence),
  .layer396(1L, 0L, 15L, "A", clay = 20, orgc = 20, phaq = 5.6, cec = 15, licence = licence),
  .layer396(2L, 15L, 30L, "AB", clay = 28, orgc = 9, phaq = 5.8, cec = 13, licence = licence))

# A fake ISRIC for the live reader, serving `layers` (as in test-v09206).
.isric396 <- function(layers) {
  function(query, variables = NULL, timeout = 30) {
    n <- variables$n %||% 100L; o <- variables$o %||% 0L
    page <- function(x) x[seq_along(x) > o & seq_along(x) <= o + n]
    if (grepl("Values\\(first", query))
      return(list(data = list(wosisLatestLayers = page(layers)), error = NULL))
    list(data = list(wosisLatestLayers = page(lapply(layers, function(l) list(layerId = l$layerId)))),
         error = NULL)
  }
}

# the benchmark script: installed (R CMD check), else the source tree
.script396 <- function() {
  f <- system.file("benchmarks", "fao_wosis.R", package = "soilKey")
  if (!nzchar(f)) f <- testthat::test_path("..", "..", "inst", "benchmarks", "fao_wosis.R")
  skip_if_not(file.exists(f))
  e <- new.env()
  sys.source(f, envir = e)
  e
}

test_that("the live reader and the offline step give the same horizons", {
  skip_on_cran()
  for (lay in list(.layers396(), .layers396(.nc396))) {
    local_mocked_bindings(.wosis_post = .isric396(lay))
    live <- read_wosis_layers_graphql(1L)
    off <- soilKey:::.wosis_layers_to_horizons(lay)
    expect_identical(live, off)
    expect_identical(serialize(live, NULL), serialize(off, NULL))
  }
  h <- soilKey:::.wosis_layers_to_horizons(.layers396())
  expect_equal(h$top_cm, c(0, 15, 30))
  expect_equal(h$designation, c("A", "AB", "Bw"))
  expect_equal(h$oc_pct, c(2.0, 0.9, 0.4))
  expect_equal(h$cec_cmol, c(15, 13, 12))
  expect_true(all(is.na(h$caco3_pct)))
  expect_identical(attr(h, "n_layers"), 3L)
  expect_identical(attr(h, "dataset"), "XX-TEST")
  expect_identical(attr(h, "depth_shift_cm"), 0)
  expect_true(all(c("attribute", "notes") %in% names(attr(h, "provenance"))))
  # the most restrictive licence of the layers is kept
  lay <- .layers396(); lay[[1]]$licence <- .nc396
  expect_identical(attr(soilKey:::.wosis_layers_to_horizons(lay), "licence"), .nc396)
})

test_that("an organic surface above 0 cm is shifted, and a missing property is not measured", {
  skip_on_cran()
  lay <- list(.layer396(1L, -10L, 0L, "O", orgc = 300),
              .layer396(2L, 0L, 20L, "A", clay = 15, orgc = 15))
  h <- soilKey:::.wosis_layers_to_horizons(lay)
  expect_equal(h$top_cm, c(0, 10))
  expect_equal(h$bottom_cm, c(10, 30))
  expect_identical(attr(h, "depth_shift_cm"), 10)
  expect_true(is.na(h$clay_pct[1]))
  expect_true(all(is.na(h$ec_dS_m)))
  # n_layers is the caller's count when given
  expect_identical(attr(soilKey:::.wosis_layers_to_horizons(lay, n_layers = 7L), "n_layers"), 7L)
})

test_that("the benchmark script finds no cache, says so and downloads nothing", {
  skip_on_cran()
  env <- .script396()
  expect_true(is.function(env$fao_wosis_benchmark))
  expect_length(env$.fao_wosis_variants, 0L)
  expect_equal(env$.fao_wosis_wilson(0, 0), c(NA_real_, NA_real_))
  ci <- env$.fao_wosis_wilson(50, 100)
  expect_true(ci[1] < 0.5 && ci[2] > 0.5)
  # no cache anywhere: an empty user cache folder, no option, no variable
  withr::local_envvar(R_USER_CACHE_DIR = file.path(tempdir(), "empty-user-cache-396"),
                      SOILKEY_WOSIS_FAO_CACHE = "")
  withr::local_options(soilKey.wosis_fao_cache = NULL)
  expect_message(expect_null(env$fao_wosis_cache(NULL, must = FALSE)), "downloads nothing")
  expect_error(env$fao_wosis_cache(NULL), "downloads nothing")
  # an explicit path that holds no cache stops, even with must = FALSE
  nofile <- file.path(tempdir(), "no-such-wosis-cache-396")
  expect_error(env$fao_wosis_cache(nofile, must = FALSE), "does not exist")
})

test_that("weighted proportions, the undecided groupings and the baseline", {
  skip_on_cran()
  env <- .script396()
  p <- env$.fao_wosis_prop(c(TRUE, FALSE), c(3, 1))
  expect_equal(unname(p[c("n", "pct")]), c(2, 75))
  expect_equal(unname(p[c("lo", "hi")]), 100 * env$.fao_wosis_wilson(0.75 * 1.6, 1.6))
  expect_equal(unname(env$.fao_wosis_prop(c(TRUE, FALSE, TRUE))["pct"]), 200 / 3)
  # a provisional covered result names the groupings tested before it
  r <- list(levels = data.frame(level = "major_groups", code = "CM", stringsAsFactors = FALSE),
            provisional_because = c("major_groups:GY", "major_groups:VR"), system_id = "fao1988",
            trace = list(major_groups = list(CM = list(evidence = list()))))
  s <- env$.fao_wosis_result(r)
  expect_identical(s$fall_through, "no")
  expect_true(s$prov_group)
  expect_identical(s$candidates, "GY VR")
  r$levels <- data.frame(level = character(0), code = character(0))
  expect_identical(env$.fao_wosis_result(r)$fall_through, "undecided")
  # the baseline: fixed seed, the caller's random numbers untouched
  x <- data.frame(group_code = c("A", "B", "C", "A"), sk_group = c("A", NA, "B", NA),
                  candidates = c("", "B C", "", "A"), stringsAsFactors = FALSE)
  set.seed(1); u1 <- stats::runif(1)
  set.seed(1); b1 <- env$.fao_wosis_baseline(x, rep(1, 4), B = 50L); u2 <- stats::runif(1)
  expect_identical(u1, u2)
  expect_identical(b1, env$.fao_wosis_baseline(x, rep(1, 4), B = 50L))
  expect_true(all(b1 >= 0 & b1 <= 100))
  expect_equal(b1[["mean"]], b1[["mean_w"]])
})

test_that("the benchmark runs end to end on a synthetic cache", {
  skip_on_cran()
  env <- .script396()
  dir <- file.path(tempdir(), "wosis-fao-396")
  unlink(dir, recursive = TRUE)
  dir.create(file.path(dir, "raw"), recursive = TRUE)
  prof <- function(id, country, dataset, gc, g, uc, u, year, layers = TRUE)
    list(profileId = id, profileCode = paste0("XX-", id), datasetCode = dataset, countryName = country,
         latitude = 1, longitude = 2, faoMajorGroupCode = gc, faoMajorGroup = g,
         faoSoilUnitCode = uc, faoSoilUnit = u, faoPublicationYear = year,
         layers = if (layers) list(list(licence = .by396)) else list())
  listing <- list(
    prof(1L, "Testland", "XX-A", "CM", "Cambisol", "d", "Dystric", 1997L),
    prof(2L, "Testland", "XX-A", "CL", "Calcisol", "p", "Plinthic", 1997L),   # no unit of 1988
    prof(3L, "Mexico", "XX-B", "R", "Regosol", "c", "Chromic", 1974L),        # no unit of 1974
    prof(4L, "Mexico", "XX-B", "SC", "Solonchak", "o", "Orthic", 1974L),      # WoSIS codes Z as SC
    prof(5L, "Testland", "WD-ISIS", "B", "Cambisol", "d", "Dystric", 1974L),  # left out (ISIS)
    prof(6L, "Testland", "XX-A", "LV", "Luvisol", NULL, NULL, 1997L, FALSE),  # no layers
    prof(7L, "Testland", "XX-A", "B", "Cambisol", "e", "Eutric", 1974L),
    prof(8L, "Testland", "XX-A", "CM", "Cambisol", "e", "Eutric", 1997L),     # listed, not sampled
    prof(9L, "Testland", "XX-A", "CM", "Cambisol", "h", "Haplic", 1997L),     # listed, not sampled
    prof(10L, "Testland", "XX-A", "LV", "Luvisol", "h", "Haplic", 2006L))     # another year
  saveRDS(listing, file.path(dir, "listing.rds"))
  for (id in c(1L, 2L, 3L, 4L, 5L, 7L, 10L))
    saveRDS(.layers396(if (id %in% c(3L, 4L)) .nc396 else .by396), file.path(dir, "raw", paste0(id, ".rds")))
  man <- data.frame(profile_id = c(1:7, 10L), downloaded = "2026-11-10 12:00:00",
                    n_layers = c(3, 3, 3, 3, 3, 0, 3, 3),
                    status = c("ok", "ok", "ok", "ok", "ok", "no_layers", "ok", "ok"), licences = "")
  utils::write.csv(man, file.path(dir, "manifest.csv"), row.names = FALSE)
  out <- file.path(dir, "report.md")
  res <- env$fao_wosis_benchmark(cache = dir, out = out, verbose = FALSE, permutations = 20L)
  ac <- stats::setNames(res$accounting$n, res$accounting$item)
  expect_equal(unname(ac[c("profiles listed with an FAO-Unesco label",
                           "of which with a year other than 1974 or 1997 (not scored)",
                           "downloaded with layers", "no layers", "left out: dataset excluded",
                           "left out: year other than 1974 or 1997", "scored")]), c(10, 1, 7, 1, 1, 1, 5))
  r <- res$records
  expect_false(any(c("5", "10") %in% r$profile_id))
  expect_identical(r$edition[match(c("1", "3", "4"), r$profile_id)], c("1988", "1974", "1974"))
  # the Solonchak coded SC is a 1974 Solonchak, scored with the rest of 1974
  expect_identical(r$group_code[r$profile_id == "4"], "Z")
  expect_true(all(r$unit_outside[r$profile_id %in% c("2", "3")]))
  expect_identical(r$unit_code[r$profile_id == "1"], "CMd")
  expect_identical(r$cc_by[match(c("1", "3"), r$profile_id)], c(TRUE, FALSE))
  expect_setequal(res$unmatched$label, c("Plinthic Calcisol", "Chromic Regosol"))
  # the design: profile 1 stands for its stratum of 3 listed profiles
  expect_true(res$weighted)
  expect_equal(r$weight[match(c("1", "2", "3", "4", "7"), r$profile_id)], c(3, 1, 1, 1, 1))
  sc <- res$score
  expect_identical(sc$subset[sc$edition == "1974"], c("all", "CC BY only", "Mexico", "outside Mexico"))
  expect_equal(sc$n[sc$edition == "1974"], c(3, 1, 2, 1))
  expect_equal(sc$n[sc$edition == "1988"], c(2, 2))
  expect_true(all(sc$k_cov + sc$undecided + sc$not_classified == sc$n))
  expect_true(all(sc$k_agree <= sc$k_nc))
  expect_true(all(sc$k_cov - sc$k_agree == sc$covered_wrong))
  expect_true(all(c("w_agree", "w_nc", "base", "base_w", "agree_firm") %in% names(sc)))
  expect_true("Z" %in% res$by_group$group)
  expect_true("Z" %in% rownames(res$confusion[["1974"]]))
  md <- readLines(out)
  expect_true(any(grepl("Batjes NH, Calisto L and de Sousa LM, 2024", md, fixed = TRUE)))
  expect_true(any(grepl("Date downloaded: 2026-11-10", md, fixed = TRUE)))
  expect_true(any(grepl("Grouping, weighted (headline)", md, fixed = TRUE)))
  expect_true(any(grepl("Shuffled-label baseline", md, fixed = TRUE)))
  expect_true(any(grepl("Unmatched units", md, fixed = TRUE)))
  expect_true(all(!grepl("[^ -~]", md)))
  # without the layers field in the listing there is no design: unweighted
  listing <- lapply(listing, function(p) { p$layers <- NULL; p })
  saveRDS(listing, file.path(dir, "listing.rds"))
  res2 <- env$fao_wosis_benchmark(cache = dir, verbose = FALSE, permutations = 5L)
  expect_false(res2$weighted)
  expect_true(all(res2$records$weight == 1))
  expect_equal(res2$score$agree, res2$score$w_agree)
})
