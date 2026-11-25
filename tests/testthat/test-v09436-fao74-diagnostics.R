# v0.9.436: the diagnostics of the 1974 Legend after the audit of the FAO
# legends (Hugo, 2026-11-24): A2 oxic B on contiguous sub-runs, A3 histic H
# on H layers, A4 calcic horizon against the C horizon, A5 stand-in layers
# held to the undecided 1974 layers, A6 hydromorphic items 3 and 4 off H and
# O layers, R2 cambic B not measured against an argillic or natric B, R3
# branch 4.5 skips a dark A.

.p436 <- function(..., site = list(id = "p")) {
  cols <- list(...)
  n <- max(lengths(cols))
  h <- make_empty_horizons(n)
  for (k in names(cols)) data.table::set(h, j = k, value = rep_len(cols[[k]], n))
  suppressWarnings(PedonRecord$new(site = site, horizons = h))
}

test_that("A2: the oxic B is tested on contiguous sub-runs of its candidate layers", {
  skip_on_cran()
  ox <- function(top, bot, bnd) {
    n <- length(top)
    .p436(top_cm = top, bottom_cm = bot, designation = c("A", paste0("Bo", seq_len(n - 1L))),
          boundary_distinctness = bnd, clay_pct = 40, silt_pct = 10, sand_pct = 50,
          oc_pct = c(1.5, rep(0.5, n - 1L)), cec_cmol = c(8, rep(6, n - 1L)),
          ca_cmol = 3.9 / 2, mg_cmol = 3.9 / 4, k_cmol = 3.9 / 8, na_cmol = 3.9 / 8,
          al_kcl_cmol = 0, weatherable_minerals_pct = 1, bs_pct = 50)
  }
  # Bo1 15-50 passes alone; with Bo2 and Bo3 below it across two clear
  # boundaries the whole run fails (0.9.435 read only the whole run)
  p <- ox(c(0, 15, 50, 70), c(15, 50, 70, 85), c("clear", "clear", "clear", NA))
  r <- oxic_b_fao74(p)
  expect_true(r$passed)
  expect_identical(as.integer(r$layers), 2L)
  # three layers of 20 cm across abrupt boundaries: no sub-run passes
  p <- ox(c(0, 15, 35, 55), c(15, 35, 55, 75), c("clear", "abrupt", "abrupt", NA))
  expect_false(oxic_b_fao74(p)$passed)
  # the same layers across gradual boundaries pass as one horizon
  p <- ox(c(0, 15, 35, 55), c(15, 35, 55, 75), c("clear", "gradual", "gradual", NA))
  r <- oxic_b_fao74(p)
  expect_true(r$passed)
  expect_identical(as.integer(r$layers), 2:4)
})

test_that("A3: the thin surface and plough layer paths of the histic H read H layers, not O", {
  skip_on_cran()
  thin <- function(d) .p436(top_cm = c(0, 10, 40), bottom_cm = c(10, 40, 120),
                            designation = c(d, "A", "Bw"), oc_pct = c(45, 3, 0.5),
                            clay_pct = 20, silt_pct = 40, sand_pct = 40,
                            site = list(id = "p", drainage_class = "well drained"))
  # a well-drained forest floor (O, or O with a suffix of decomposition on a
  # well-drained site) is no histic H; a saturated H is
  expect_false(histic_h_fao74(thin("O"))$passed)
  expect_false(histic_h_fao74(thin("Oa"))$passed)
  expect_true(histic_h_fao74(thin("H"))$passed)
  expect_false(isTRUE(hydromorphic_properties_fao74(thin("O"))$passed))
  # 1988 keeps reading the organic layers (its own PRs)
  expect_true(histic_h_fao88(thin("O"))$passed)
  plough <- function(d) .p436(top_cm = c(0, 30), bottom_cm = c(30, 120), designation = c(d, "Bw"),
                              oc_pct = c(12, 0.5), clay_pct = 20, silt_pct = 40, sand_pct = 40,
                              site = list(id = "p", drainage_class = "well drained"))
  expect_true(histic_h_fao74(plough("Ap"))$passed)
  expect_false(histic_h_fao74(plough("Op"))$passed)
})

test_that("A4: the calcic horizon of 1974 has 5 % more carbonate than the C horizon", {
  skip_on_cran()
  # Bk 18 % over a C of 14 % and a 2C of 5 %: 1988 (a deeper horizon)
  # passes, 1974 (the C horizon) does not
  p <- .p436(top_cm = c(0, 30, 60, 100), bottom_cm = c(30, 60, 100, 150),
             designation = c("A", "Bk", "C", "2C"), caco3_pct = c(2, 18, 14, 5),
             clay_pct = 20, silt_pct = 40, sand_pct = 40)
  expect_false(calcic_fao74(p)$passed)
  expect_true(calcic_fao88(p)$passed)
  # no C designated: the layer right below, flagged
  p <- .p436(top_cm = c(0, 30, 60), bottom_cm = c(30, 60, 100), designation = c("A", "Bk1", "Bk2"),
             caco3_pct = c(2, 20, 10), clay_pct = 20, silt_pct = 40, sand_pct = 40)
  r <- calcic_fao74(p)
  expect_true(r$passed)
  expect_identical(as.integer(r$layers), 2L)
  expect_true("no_c_horizon_layer_below_compared" %in% r$evidence$flags)
  expect_false("no_c_horizon_layer_below_compared" %in% calcic_fao88(p)$evidence$flags)
})

test_that("A5: a stand-in counts only on the layers the 1974 reader left undecided", {
  skip_on_cran()
  p <- .p436(top_cm = c(0, 30, 60, 100), bottom_cm = c(30, 60, 100, 150),
             designation = c("A", "Bk", "C", "2C"), caco3_pct = c(2, 18, 14, NA),
             clay_pct = 20, silt_pct = 40, sand_pct = 40)
  x <- soilKey:::.f74_ctx(p)
  r <- list(units = list(list(layers = 4L, top = 100, bottom = 150, thickness = 50, status = NA)),
            missing = "caco3_pct")
  si <- function(layers, passed = TRUE) function(pedon)
    DiagnosticResult$new(name = "si", passed = passed, layers = layers)
  # the stand-in passes on Bk (decided not calcic by 1974) and on 2C
  # (undecided): only 2C is kept
  local_mocked_bindings(calcic_horizon_usda = si(x$idx[c(2L, 4L)]), calcic = si(integer(0), NA),
                        .package = "soilKey")
  res <- soilKey:::.f74_result(x, "calcic", r)
  expect_true(res$passed)
  expect_identical(as.integer(res$layers), as.integer(x$idx[4L]))
  expect_true("calcic_horizon_usda" %in% res$evidence$stand_in)
  # passing only on the decided Bk, the stand-in is not taken; the WRB one
  # is undecided, so the answer stays undecided
  local_mocked_bindings(calcic_horizon_usda = si(x$idx[2L]), .package = "soilKey")
  res <- soilKey:::.f74_result(x, "calcic", r)
  expect_true(is.na(res$passed))
  expect_length(res$evidence$stand_in, 0L)
  # an undecided answer without layers decided none: the stand-in is kept
  r0 <- list(units = list(), undecided = TRUE)
  res <- soilKey:::.f74_result(x, "calcic", r0)
  expect_true(res$passed)
  expect_identical(as.integer(res$layers), as.integer(x$idx[2L]))
})

test_that("A6: hydromorphic items 3 and 4 are not read on H and O layers", {
  skip_on_cran()
  # 2 cm of litter of hue N over a brown mineral soil
  p <- .p436(top_cm = c(0, 2, 20, 60), bottom_cm = c(2, 20, 60, 120),
             designation = c("O", "A", "Bw", "C"), oc_pct = c(40, 1.5, 0.5, 0.2),
             clay_pct = 20, silt_pct = 40, sand_pct = 40,
             munsell_hue_moist = c("N", "10YR", "7.5YR", "7.5YR"),
             munsell_value_moist = c(2, 4, 4, 5), munsell_chroma_moist = c(0, 4, 6, 6), bs_pct = 60)
  expect_false(hydromorphic_properties_fao74(p)$passed)
  it <- soilKey:::.f74_hydro_v(soilKey:::.f74_ctx(p))$evidence$items
  expect_false(it$hue[1L])
  expect_false(it$colours[1L])
  # the same hue N in a mineral layer still counts
  p2 <- .p436(top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 120), designation = c("A", "Bg", "C"),
              oc_pct = c(1.5, 0.5, 0.2), clay_pct = 20, silt_pct = 40, sand_pct = 40,
              munsell_hue_moist = c("10YR", "N", "7.5YR"), munsell_value_moist = c(4, 5, 5),
              munsell_chroma_moist = c(4, 0, 6), bs_pct = 60)
  expect_true(hydromorphic_properties_fao74(p2)$passed)
})

test_that("R2: the cambic B is not measured against an argillic or natric B below", {
  skip_on_cran()
  cb <- function(films) .p436(top_cm = c(0, 20, 40, 100), bottom_cm = c(20, 40, 100, 150),
                              designation = c("A", "BA", if (is.na(films)) "Bw" else "Bt", "C"),
                              clay_pct = c(20, 22, 32, 25), silt_pct = 30, sand_pct = c(50, 48, 38, 45),
                              oc_pct = c(1.5, 0.6, 0.4, 0.2), cec_cmol = c(10, 10, 9, 7), bs_pct = 60,
                              munsell_hue_moist = c("10YR", "5YR", "7.5YR", "7.5YR"),
                              munsell_value_moist = c(4, 4, 4, 5), munsell_chroma_moist = c(3, 6, 4, 4),
                              munsell_value_dry = c(6, 5, 5, 6), munsell_chroma_dry = c(3, 6, 4, 4),
                              clay_films_amount = c(NA, NA, films, NA),
                              structure_type = c("granular", "subangular blocky", "subangular blocky", "massive"),
                              structure_grade = c("weak", "moderate", "moderate", NA),
                              weatherable_minerals_pct = 10)
  # a BA redder than the Bt of an argillic B shows no alteration
  p <- cb("common")
  expect_true(argillic_b_fao74(p)$passed)
  expect_false(cambic_b_fao74(p)$passed)
  crit <- soilKey:::.f74_hz_cambic(soilKey:::.f74_ctx(p))$evidence$criteria
  expect_false(crit$alteration[2L])
  # the same BA over a Bw that is no argillic B is altered
  p <- cb(NA)
  expect_false(argillic_b_fao74(p)$passed)
  r <- cambic_b_fao74(p)
  expect_true(r$passed)
  expect_true(2L %in% as.integer(r$layers))
})

test_that("R3: branch 4.5 of the hydromorphic colours skips an A of moist value below 3.5", {
  skip_on_cran()
  hy <- function(vm) .p436(top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 120),
                           designation = c("A", "Bw", "C"), oc_pct = c(2, 0.5, 0.2),
                           clay_pct = 20, silt_pct = 40, sand_pct = 40, munsell_hue_moist = "10YR",
                           munsell_value_moist = c(vm, 5, 5), munsell_chroma_moist = c(2, 4, 4),
                           munsell_value_dry = c(vm + 1, 6, 6), munsell_chroma_dry = c(2, 4, 4),
                           mottle_colors = c("7.5YR 5/6", NA, NA),
                           redoximorphic_features_pct = c(3, 0, 0), bs_pct = 30, cec_cmol = 12,
                           structure_type = c("granular", "subangular blocky", "massive"),
                           structure_grade = c("moderate", "moderate", NA), ph_h2o = 5)
  # a humus-dark mottled A of chroma 2 (other soils): not read
  p <- hy(3)
  expect_identical(soilKey:::.f74_hydro_v(soilKey:::.f74_ctx(p))$evidence$kind_of_soil, "other")
  expect_false(hydromorphic_properties_fao74(p)$passed)
  # the same A with a moist value of 4 is read and meets branch 4.5
  expect_true(hydromorphic_properties_fao74(hy(4))$passed)
})

test_that("the readings of this part are recorded for the 1974 legend", {
  skip_on_cran()
  rd <- yaml::read_yaml(system.file("rules", "fao1974", "readings.yaml", package = "soilKey"))$readings
  r436 <- Filter(function(e) identical(e$version, "0.9.436"), rd)
  expect_length(r436, 3L)
  expect_true(all(vapply(r436, function(e) grepl("(Hugo, 2026-11-24)", e$reading, fixed = TRUE), NA)))
  expect_true(all(grepl("FAO audit", vapply(r436, `[[`, "", "reading"))))
})
