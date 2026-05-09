# =============================================================================
# Tests for v0.9.74 -- USDA Soil Taxonomy <-> WRB Reference Soil Group
# cross-walk + KSSL/NASIS WRB benchmark plumbing.
# =============================================================================


test_that("v0.9.74: usda_to_wrb_rsg() handles the 12 USDA Orders at default", {
  expect_identical(usda_to_wrb_rsg("Histosols"),   "Histosol")
  expect_identical(usda_to_wrb_rsg("Andisols"),    "Andosol")
  expect_identical(usda_to_wrb_rsg("Gelisols"),    "Cryosol")
  expect_identical(usda_to_wrb_rsg("Spodosols"),   "Podzol")
  expect_identical(usda_to_wrb_rsg("Oxisols"),     "Ferralsol")
  expect_identical(usda_to_wrb_rsg("Vertisols"),   "Vertisol")
  expect_identical(usda_to_wrb_rsg("Aridisols"),   "Calcisol")     # default
  expect_identical(usda_to_wrb_rsg("Ultisols"),    "Acrisol")
  expect_identical(usda_to_wrb_rsg("Mollisols"),   "Phaeozem")     # default
  expect_identical(usda_to_wrb_rsg("Alfisols"),    "Luvisol")
  expect_identical(usda_to_wrb_rsg("Inceptisols"), "Cambisol")
  expect_identical(usda_to_wrb_rsg("Entisols"),    "Regosol")      # default
})


test_that("v0.9.74: case-insensitive + plural-stripped input handling", {
  expect_identical(usda_to_wrb_rsg("MOLLISOLS"),    "Phaeozem")
  expect_identical(usda_to_wrb_rsg("mollisols"),    "Phaeozem")
  expect_identical(usda_to_wrb_rsg("Mollisol"),     "Phaeozem")
  expect_identical(usda_to_wrb_rsg("  Mollisols "), "Phaeozem")
})


test_that("v0.9.74: Aridisol suborder refinement", {
  expect_identical(usda_to_wrb_rsg("Aridisols", "Salids"),  "Solonchak")
  expect_identical(usda_to_wrb_rsg("Aridisols", "Calcids"), "Calcisol")
  expect_identical(usda_to_wrb_rsg("Aridisols", "Gypsids"), "Gypsisol")
  expect_identical(usda_to_wrb_rsg("Aridisols", "Argids"),  "Solonetz")
  expect_identical(usda_to_wrb_rsg("Aridisols", "Durids"),  "Durisol")
})


test_that("v0.9.74: Mollisol suborder refinement", {
  expect_identical(usda_to_wrb_rsg("Mollisols", "Ustolls"),  "Kastanozem")
  expect_identical(usda_to_wrb_rsg("Mollisols", "Xerolls"),  "Kastanozem")
  expect_identical(usda_to_wrb_rsg("Mollisols", "Rendolls"), "Leptosol")
  expect_identical(usda_to_wrb_rsg("Mollisols", "Aquolls"),  "Phaeozem")
})


test_that("v0.9.74: Entisol + Inceptisol suborder refinement", {
  expect_identical(usda_to_wrb_rsg("Entisols", "Psamments"), "Arenosol")
  expect_identical(usda_to_wrb_rsg("Entisols", "Fluvents"),  "Fluvisol")
  expect_identical(usda_to_wrb_rsg("Entisols", "Aquents"),   "Fluvisol")
  expect_identical(usda_to_wrb_rsg("Inceptisols", "Aquepts"), "Gleysol")
})


test_that("v0.9.74: vectorised input preserved", {
  ords <- c("Mollisols", "Inceptisols", "Vertisols", "Aridisols")
  subs <- c("Ustolls",   "Aquepts",     NA,          "Salids")
  out <- usda_to_wrb_rsg(ords, subs)
  expect_equal(out, c("Kastanozem", "Gleysol", "Vertisol", "Solonchak"))
})


test_that("v0.9.74: unknown order returns NA", {
  expect_true(is.na(usda_to_wrb_rsg("FooBar")))
})


test_that("v0.9.74: annotate_wrb_from_usda() populates reference_wrb_from_usda", {
  hz <- data.table::data.table(
    top_cm = c(0, 30), bottom_cm = c(30, 90),
    designation = c("A", "Bt"), clay_pct = c(20, 40),
    silt_pct = c(20, 25), sand_pct = c(60, 35),
    cec_cmol = c(15, 20), oc_pct = c(2, 0.5), ph_h2o = c(7, 7.2)
  )
  hz <- ensure_horizon_schema(hz)
  pr <- PedonRecord$new(
    site = list(id = "test-mol",
                  reference_usda = "Mollisols",
                  reference_usda_suborder = "Ustolls"),
    horizons = hz
  )
  out <- annotate_wrb_from_usda(list(pr))
  expect_identical(out[[1]]$site$reference_wrb_from_usda, "Kastanozem")
})


test_that("v0.9.74: annotate_wrb_from_usda() preserves existing reference_wrb_from_usda", {
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 30, designation = "A",
    clay_pct = 20, silt_pct = 30, sand_pct = 50,
    oc_pct = 1, ph_h2o = 6
  )
  hz <- ensure_horizon_schema(hz)
  pr <- PedonRecord$new(
    site = list(id = "test", reference_usda = "Mollisols",
                  reference_wrb_from_usda = "Phaeozem"),  # already set
    horizons = hz
  )
  out <- annotate_wrb_from_usda(list(pr))
  expect_identical(out[[1]]$site$reference_wrb_from_usda, "Phaeozem")  # unchanged
})


# ---- Bundled KSSL sample tests ---------------------------------------------

test_that("v0.9.74: load_kssl_sample() returns 100 pedons with USDA + derived WRB", {
  testthat::skip_if_not(file.exists(file.path("inst", "extdata", "kssl_sample.rds"))
                          || nzchar(system.file("extdata", "kssl_sample.rds",
                                                package = "soilKey")),
                          "Bundled KSSL sample not present")
  s <- load_kssl_sample()
  expect_named(s, c("pedons", "pulled_on", "source", "cross_walk"),
                ignore.order = TRUE)
  # Loader skips pedons with require_b_horizon=TRUE; head=100 may
  # yield 99-100 valid pedons depending on the snapshot.
  expect_true(length(s$pedons) >= 95L && length(s$pedons) <= 100L)
  # Every pedon has USDA + derived WRB labels
  for (pr in s$pedons) {
    expect_true(!is.null(pr$site$reference_usda))
    expect_true(!is.null(pr$site$reference_wrb_from_usda))
  }
})


test_that("v0.9.74: KSSL sample USDA Orders all map to plausible WRB RSGs", {
  testthat::skip_if_not(file.exists(file.path("inst", "extdata", "kssl_sample.rds"))
                          || nzchar(system.file("extdata", "kssl_sample.rds",
                                                package = "soilKey")),
                          "Bundled KSSL sample not present")
  s <- load_kssl_sample()
  wrb_labs <- unique(vapply(s$pedons,
                              function(p) p$site$reference_wrb_from_usda,
                              character(1)))
  # Should not contain NAs (every Order is mapped) nor empty strings
  expect_false(any(is.na(wrb_labs)))
  expect_false(any(!nzchar(wrb_labs)))
  # And should be valid WRB RSGs (no random strings)
  valid_rsgs <- c("Acrisol","Albeluvisol","Alisol","Andosol","Anthrosol",
                    "Arenosol","Calcisol","Cambisol","Chernozem","Cryosol",
                    "Durisol","Ferralsol","Fluvisol","Gleysol","Gypsisol",
                    "Histosol","Kastanozem","Leptosol","Lixisol","Luvisol",
                    "Nitisol","Phaeozem","Planosol","Plinthosol","Podzol",
                    "Regosol","Solonchak","Solonetz","Stagnosol","Technosol",
                    "Umbrisol","Vertisol")
  expect_true(all(wrb_labs %in% valid_rsgs))
})


test_that("v0.9.74: benchmark_wrb_vs_usda() runs end-to-end on bundled KSSL sample", {
  testthat::skip_if_not(file.exists(file.path("inst", "extdata", "kssl_sample.rds"))
                          || nzchar(system.file("extdata", "kssl_sample.rds",
                                                package = "soilKey")),
                          "Bundled KSSL sample not present")
  s <- load_kssl_sample()
  # Run on first 20 pedons (full 100 takes longer)
  res <- benchmark_wrb_vs_usda(s$pedons[1:20], verbose = FALSE)
  expect_named(res, c("accuracy", "n_compared", "n_total",
                        "confusion", "per_class_recall", "refs", "preds"),
                ignore.order = TRUE)
  expect_true(res$n_total == 20L)
  expect_true(res$n_compared >= 1L)
  expect_true(is.numeric(res$accuracy))
})
