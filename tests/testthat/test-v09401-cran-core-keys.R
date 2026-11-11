# v0.9.401 -- one profile through each key, on CRAN too.
#
# Most tests are skipped on CRAN to keep the check within its time limit.
# These stay: each classifies one example through its key, so every system
# of the registry is exercised by the CRAN check. The WRB 2022 and USDA keys
# keep their own fast fixture tests (test-key-wrb-end-to-end.R,
# test-key-usda.R), the CSSC and ASC keys theirs (test-v09267-cssc3-orders.R,
# test-v09277-asc3-orders.R).

test_that("SiBCS: the Podzol fixture keys to Espodossolos", {
  r <- classify_sibcs(make_podzol_canonical(), on_missing = "silent")
  expect_identical(r$rsg_or_order, "Espodossolos")
})

test_that("NZSC: the organic example keys to Organic Soils", {
  r <- classify_nzsc4(make_nzsc4_example("organic"), on_missing = "silent")
  expect_identical(r$system_id, "nzsc4")
  expect_identical(r$rsg_or_order, "Organic Soils")
})

test_that("HSCS: the peat example keys to the peat soils", {
  r <- classify_hscs2024(make_hscs2024_example("peat"), on_missing = "silent")
  expect_identical(r$system_id, "hscs2024")
  expect_identical(r$rsg_or_order, "L\u00e1ptalaj")
})

test_that("SCSJ: the man-made example keys to Man-made soils", {
  r <- classify_scsj2017(make_scsj2017_example("man_made"), on_missing = "silent")
  expect_identical(r$system_id, "scsj2017")
  expect_identical(r$rsg_or_order, "Man-made soils")
})

test_that("CSCJ: the man-made example keys to Man-made soils", {
  r <- classify_cscj2011(make_cscj2011_example("man_made"), on_missing = "silent")
  expect_identical(r$system_id, "cscj2011")
  expect_identical(r$rsg_or_order, "Man-made soils")
})

test_that("CPCS: the raw mineral example keys to its class", {
  r <- classify_cpcs1967(make_cpcs1967_example("raw_mineral"), on_missing = "silent")
  expect_identical(r$system_id, "cpcs1967")
  expect_identical(r$rsg_or_order, "Sols min\u00e9raux bruts")
})

test_that("Russia 2008: the oligotrophic peat example keys to Organogenic soils", {
  r <- classify_kidpr2008(make_kidpr2008_example("AA"), on_missing = "silent")
  expect_identical(r$system_id, "kidpr2008")
  expect_identical(r$rsg_or_order, "Organogenic soils")
})

test_that("RP 2008: the thalassosol example keys to Thalassosols", {
  r <- classify_rp2008(make_rp2008_example("thalassosols_bruts"), on_missing = "silent")
  expect_identical(r$system_id, "rp2008")
  expect_identical(r$rsg_or_order, "Thalassosols")
})

test_that("KA5: the KV example keys to the Moore", {
  r <- classify_ka5(make_ka5_example("KV"), on_missing = "silent")
  expect_identical(r$system_id, "ka5")
  expect_identical(r$rsg_or_order, "Moore")
})

test_that("ZA 1991: the Hutton example keys to the Hutton form", {
  r <- classify_za1991(make_za1991_example("hutton"), on_missing = "silent")
  expect_identical(r$system_id, "za1991")
  expect_identical(r$rsg_or_order, "Hutton")
})

test_that("CST 2001: the first example keys to Histosols", {
  r <- classify_cst2001(make_cst2001_example(), on_missing = "silent")
  expect_identical(r$system_id, "cst2001")
  expect_identical(r$rsg_or_order, "Histosols")
})

test_that("FAO 1988: the HS example keys to Histosols", {
  r <- classify_fao1988(make_fao1988_example("HS"), on_missing = "silent")
  expect_identical(r$system_id, "fao1988")
  expect_identical(r$rsg_or_order, "Histosols")
})

test_that("FAO 1974: the O example keys to Histosols", {
  r <- classify_fao1974(make_fao1974_example("O"), on_missing = "silent")
  expect_identical(r$system_id, "fao1974")
  expect_identical(r$rsg_or_order, "Histosols")
})
