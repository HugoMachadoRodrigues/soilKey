# v0.9.392: Hugo's decisions of 2026-11-10 on the causes the FAO ISIS
# benchmark showed: the cracks of the ISIS reader, gypsum read from an
# upper bound of the ECe, and andic properties with the P retention not
# measured. Synthetic records only.

.cd392 <- function(item, description = NULL) list(item = item, description = description)
.isis392 <- function(...) {
  p <- list(profileCode = "XX392", countryCode = "XX", profileId = 1L,
            layers = list(list(layerNumber = 1L, upperDepth = 0L, lowerDepth = 30L, layerName = "A",
                               clay = list("50")),
                          list(layerNumber = 2L, upperDepth = 30L, lowerDepth = 120L, layerName = "Bss",
                               clay = list("55")),
                          list(layerNumber = 3L, upperDepth = 120L, lowerDepth = 150L, layerName = "C",
                               clay = list("40"))))
  utils::modifyList(p, list(...))
}
.ped392 <- function(...) {
  h <- data.table::data.table(top_cm = c(0, 30), bottom_cm = c(30, 80), designation = c("A", "Bw"), ...)
  PedonRecord$new(site = list(id = "t392"), horizons = ensure_horizon_schema(h))
}

test_that("ISIS cracks: large cracks and gilgai are 1 cm or more, small cracks 0.5 cm", {
  skip_on_cran()
  w <- function(p) as.data.frame(isis_profile_to_pedon(p)$horizons)$cracks_width_cm
  expect_identical(w(.isis392(cracking = .cd392("L", "large cracks"))), c(1, 1, NA))
  expect_identical(w(.isis392(cracking = .cd392("C", "(small) cracks"))), c(0.5, 0.5, NA))
  expect_identical(w(.isis392(microreliefKind = .cd392("G", "gilgai"))), c(1, 1, NA))
  # cracking wins over gilgai; no cracking and no gilgai, no width
  expect_identical(w(.isis392(cracking = .cd392("C"), microreliefKind = .cd392("G"))), c(0.5, 0.5, NA))
  expect_true(all(is.na(w(.isis392()))))
  pv <- isis_profile_to_pedon(.isis392(cracking = .cd392("L", "large cracks")))$provenance
  expect_true(any(grepl("large cracks): cracks of 1 cm or more", pv$notes, fixed = TRUE)))
})

test_that("gypsum: an ECe upper bound below 2 dS/m reads as no gypsum, flagged", {
  skip_on_cran()
  g <- function(...) soilKey:::.f88_gypsum(soilKey:::.f88_ctx(.ped392(...)))
  r <- g(ec_max_dS_m = c(1.5, 2.5))
  expect_identical(r$value, c(0, NA))
  expect_identical(r$flags, c("no_gypsum_at_ece_upper_bound_below_2", NA))
  # a measured ECe or gypsum decides first
  r <- g(ec_max_dS_m = c(1.5, 1.5), ec_dS_m = c(3, NA), caso4_pct = c(NA, 7))
  expect_identical(r$value, c(NA, 7))
  r <- g(ec_dS_m = c(1, 3))
  expect_identical(r$flags, c("no_gypsum_at_ece_below_2", NA))
})

test_that("andic: item 1 met but for the P retention stays undecided", {
  skip_on_cran()
  a <- function(...) soilKey:::.f88_andic_layer(soilKey:::.f88_ctx(.ped392(...)))
  # Alox + 1/2 Feox 2 % or more and a bulk density of 0.9 or less, no P retention
  r <- a(al_ox_pct = 2.2, fe_ox_pct = 1, bulk_density_g_cm3 = 0.8)
  expect_true(all(is.na(r$value)))
  expect_true(all(grepl("andic_undecided_p_retention_not_measured", r$flags)))
  # with the P retention measured the layer is decided
  expect_identical(a(al_ox_pct = 2.2, fe_ox_pct = 1, bulk_density_g_cm3 = 0.8,
                     phosphate_retention_pct = 90)$value, c(TRUE, TRUE))
  # fewer criteria met: absent unless volcanic material is recorded (Hugo, 2026-11-09)
  r <- a(al_ox_pct = 2.2, fe_ox_pct = 1, bulk_density_g_cm3 = 1.2)
  expect_identical(r$value, c(FALSE, FALSE))
  r <- a(al_ox_pct = 2.2, fe_ox_pct = 1)
  expect_identical(r$value, c(FALSE, FALSE))
  expect_true(all(grepl("andic_absent_no_volcanic_material", r$flags)))
})
