# v0.9.295: classify_profiles() classifies many profiles from one horizon
# table, with site properties per profile and a map that renames and
# converts columns.

.two_profiles <- function() {
  rbind(
    cbind(profile_id = "p1", as.data.frame(make_ferralsol_canonical()$horizons)),
    cbind(profile_id = "p2", as.data.frame(make_phaeozem_canonical()$horizons)))
}

test_that("one row per profile, in order, with site properties", {
  skip_on_cran()
  h <- .two_profiles()
  s <- data.frame(profile_id = c("p2", "p1"),
                  soil_moisture_regime = c("ustic", NA))
  res <- classify_profiles(h, sites = s)
  expect_identical(res$profile_id, c("p1", "p2"))
  expect_true(all(c("wrb", "sibcs", "usda", "missing", "error") %in% names(res)))
  expect_identical(res$usda[2], "Typic Haplustolls")
  one <- classify_all(make_ferralsol_canonical(), on_missing = "silent")$summary
  expect_identical(res$wrb[1], one$wrb[1])
  expect_identical(res$usda[1], one$usda[1])
  expect_true(all(is.na(res$error)))
  expect_match(res$missing[1], "slickensides", fixed = TRUE)
  expect_named(attr(res, "results"), c("p1", "p2"))
  expect_s3_class(attr(res, "pedons")$p2, "PedonRecord")
  expect_identical(attr(res, "pedons")$p2$site$soil_moisture_regime, "ustic")
  expect_null(attr(res, "pedons")$p1$site$soil_moisture_regime)  # NA = not recorded
})

test_that("sites as a named list, a custom id column and shuffled rows", {
  skip_on_cran()
  h <- .two_profiles()
  names(h)[names(h) == "profile_id"] <- "code"
  h <- h[rev(seq_len(nrow(h))), ]
  res <- classify_profiles(h, sites = list(p2 = list(soil_moisture_regime = "ustic")),
                           id = "code", systems = "usda")
  expect_identical(res$code, c("p2", "p1"))
  expect_identical(res$usda[1], "Typic Haplustolls")
  # horizons are put back in depth order
  expect_true(!is.unsorted(attr(res, "pedons")$p1$horizons$top_cm))
})

test_that("map renames and converts columns", {
  skip_on_cran()
  h <- .two_profiles()
  ref <- classify_profiles(h, systems = "wrb2022")
  h2 <- h
  names(h2)[names(h2) == "oc_pct"] <- "ORGC"
  h2$ORGC <- h2$ORGC * 10
  res <- classify_profiles(h2, map = data.frame(from = "ORGC", to = "oc_pct",
                                                factor = 0.1),
                           systems = "wrb2022")
  expect_identical(res$wrb, ref$wrb)
  expect_equal(attr(res, "pedons")$p1$horizons$oc_pct,
               attr(ref, "pedons")$p1$horizons$oc_pct)
  expect_error(classify_profiles(h, map = data.frame(from = "clay_pct", to = "oc_pct")),
               "already has")
  expect_error(classify_profiles(h, map = data.frame(a = 1)), "from")
  expect_warning(classify_profiles(h, map = data.frame(from = "XYZ", to = "oc_pct"),
                                   systems = "usda"),
                 "not in 'horizons'")
})

test_that("one warning for the whole table, not one per profile", {
  skip_on_cran()
  h <- .two_profiles()
  h$foo_bar <- 1
  n <- 0L
  withCallingHandlers(
    classify_profiles(h, sites = list(p1 = list(moisture = "x")), systems = "usda"),
    soilKey_unrecognised_attributes = function(w) {
      n <<- n + 1L
      expect_match(conditionMessage(w), "foo_bar", fixed = TRUE)
      expect_match(conditionMessage(w), "moisture", fixed = TRUE)
      invokeRestart("muffleWarning")
    })
  expect_identical(n, 1L)
})

test_that("bad input stops; a failing profile does not stop the others", {
  skip_on_cran()
  h <- .two_profiles()
  expect_error(classify_profiles(h, id = "nope"), "no column 'nope'")
  expect_error(classify_profiles(as.list(h)), "data.frame")
  expect_error(classify_profiles(h, systems = "not-a-system"))
  h$profile_id[1] <- NA
  expect_warning(res <- classify_profiles(h, systems = "usda"), "without a profile id")
  expect_identical(nrow(res), 2L)
  local_mocked_bindings(classify_all = function(pedon, ...) {
    if (identical(pedon$site$id, "p1")) stop("boom")
    list(usda = NULL, summary = data.frame(usda = "x"))
  })
  res <- classify_profiles(.two_profiles(), systems = "usda")
  expect_identical(res$error[1], "boom")
  expect_true(is.na(res$usda[1]))
  expect_identical(res$usda[2], "x")
})
