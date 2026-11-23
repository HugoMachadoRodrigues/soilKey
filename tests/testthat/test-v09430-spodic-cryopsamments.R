# v0.9.430 (Hugo, 2026-11-23; USDA audit N20): Spodic Cryopsamments (KST 13)
# take a spodic-like horizon 5 cm or more thick at any depth, unlike the
# Spodic Psammaquents, Quartzipsamments and Udipsamments, whose horizon must
# lie below an Ap or at 18 cm or more.

.v09430_pedon <- function() {
  hz <- data.table::data.table(
    top_cm = c(0, 5, 15, 60), bottom_cm = c(5, 15, 60, 150),
    designation = c("A", "Bs", "C1", "C2"),
    clay_pct = 3, sand_pct = 94, silt_pct = 3, oc_pct = c(1.5, 0.8, 0.2, 0.1),
    al_ox_pct = c(0.05, 0.40, 0.10, 0.05), fe_ox_pct = c(0.05, 0.30, 0.10, 0.05),
    odoe = c(0.02, 0.20, 0.05, 0.02))
  PedonRecord$new(site = list(id = "v09430"), horizons = ensure_horizon_schema(hz))
}

test_that("a shallow spodic-like horizon qualifies only where KST sets no depth", {
  testthat::skip_on_cran()
  p <- .v09430_pedon()
  expect_true(soilKey:::spodic_subgroup_usda(p, psamments = FALSE)$passed)
  expect_false(soilKey:::spodic_subgroup_usda(p, psamments = TRUE)$passed)
})

test_that("the Entisol rules give the depth rule to the right subgroups", {
  testthat::skip_on_cran()
  y <- yaml::read_yaml(system.file("rules", "usda", "subgroups", "entisols.yaml",
                                   package = "soilKey"))
  arg <- function(code) {
    for (g in y$subgroups %||% y) for (s in g) if (identical(s$code, code))
      return(s$tests$all_of[[length(s$tests$all_of)]]$spodic_subgroup_usda$psamments)
    NULL
  }
  expect_false(arg("LDAE"))   # Spodic Cryopsamments
  expect_true(arg("LBEC"))    # Spodic Psammaquents
  expect_true(arg("LDCL"))    # Spodic Quartzipsamments
  expect_true(arg("LDFD"))    # Spodic Udipsamments
})
