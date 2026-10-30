# v0.9.315: the limiting layer of WRB 2022 Ch 2.1, rule 8 (p. 23) reads the
# "other cemented layers" (a cemented Bsm, a layer recorded as at least
# moderately cemented), and texture_class_from_pct() follows every boundary
# of the USDA texture triangle (WRB 2022 Annex 1, Table 8.39, p. 182; Soil
# Science Division Staff 2017). Small made-up pedons; no third-party data.

.k22_ped <- function(..., site = list(id = "k22")) {
  hz <- ensure_horizon_schema(data.table::as.data.table(data.frame(..., stringsAsFactors = FALSE)))
  PedonRecord$new(site = site, horizons = hz)
}


# ---- the limiting layer (Ch 2.1, rule 8, p. 23) ---------------------------------

test_that("v0.9.315 cemented layers: the class decides, else an m after the cementing agent", {
  skip_on_cran()
  h <- data.frame(
    designation = c("Bsm", "Bhsm", "Bkm1", "2Bqm", "Bsm/Btsm", "Bm", "Cm", "Bk/Bkm",
                    "Bt", NA, NA, NA, NA, "Bsm", "Bsm", "Btx", "Bw"),
    cementation_class = c(NA, NA, NA, NA, NA, NA, NA, NA,
                          NA, "moderately", "strongly", "indurated", "weakly", "weakly", "none",
                          "strongly", "very strongly"),
    pan_type = c(rep(NA, 15), "fragipan", NA), stringsAsFactors = FALSE)
  expect_identical(.wrb_cemented_limiting(h),
                   c(TRUE, TRUE, TRUE, TRUE, TRUE,     # m after s, k, q; both parts of Bsm/Btsm
                     FALSE, FALSE, FALSE,              # a bare m (Canadian Bm); Bk/Bkm mostly not cemented
                     FALSE, TRUE, TRUE, TRUE,          # no m; the class when recorded
                     FALSE, FALSE, FALSE,              # weakly or none, whatever the designation
                     FALSE, TRUE))                     # a fragipan is not continuous
  # nothing recorded: undecided, and not read as a limiting layer
  expect_true(is.na(.wrb_cemented_limiting(data.frame(designation = NA_character_,
                                                      cementation_class = NA_character_))))
})

test_that("v0.9.315 limiting layer: a cemented Bsm or a moderately cemented layer is one", {
  skip_on_cran()
  mk <- function(des, cc = rep(NA_character_, 4))
    .k22_ped(top_cm = c(0, 20, 45, 52), bottom_cm = c(20, 45, 52, 120),
             designation = des, cementation_class = cc,
             clay_pct = c(4, 3, 5, 4), silt_pct = c(8, 6, 7, 6), sand_pct = c(88, 91, 88, 90),
             oc_pct = c(1.5, 0.3, 2.5, 0.2))
  expect_equal(.barrier_top_cm(mk(c("A", "E", "Bsm", "C"))), 45)
  expect_equal(.barrier_top_cm(mk(c("A", "E", "Bs", "C"), c(NA, NA, "strongly", NA))), 45)
  # weakly cemented, or a Bs without m, is not a limiting layer
  expect_true(is.na(.barrier_top_cm(mk(c("A", "E", "Bsm", "C"), c(NA, NA, "weakly", NA)))))
  expect_true(is.na(.barrier_top_cm(mk(c("A", "E", "Bs", "C")))))
  # the shallowest of continuous rock and a cemented layer
  expect_equal(.barrier_top_cm(mk(c("A", "Bsm", "Bs", "R"))), 20)
  expect_equal(.wrb_limiting_layers(mk(c("A", "Bsm", "Bs", "R"))), c(2L, 4L))
  # the USDA kandic horizon keeps its contact as before (rock here)
  expect_equal(.barrier_top_cm(mk(c("A", "Bsm", "Bs", "R")), other_cemented = FALSE), 52)
})

test_that("v0.9.315 Humic and Skeletic count to the cemented layer", {
  skip_on_cran()
  # Humic (Ch 5, p. 136): the depth range below a limiting layer contributes 0
  hu <- function(des) .k22_ped(top_cm = c(0, 20, 30, 50), bottom_cm = c(20, 30, 50, 100),
                               designation = des, oc_pct = c(2, 0.5, 3, 0.3),
                               clay_pct = 10, silt_pct = 20, sand_pct = 70)
  r0 <- qual_humic(hu(c("A", "E", "Bs", "C")))
  r1 <- qual_humic(hu(c("A", "E", "Bsm", "C")))
  expect_true(r0$passed)                         # (2*20 + 0.5*10 + 3*20) / 50 = 2.1
  expect_false(r1$passed)                        # (2*20 + 0.5*10) / 50 = 0.9
  expect_equal(r1$evidence$limiting_layer_top_cm, 30)
  # Skeletic (Ch 5, p. 146): averaged to the limiting layer when shallower
  sk <- function(cc) .k22_ped(top_cm = c(0, 30, 40), bottom_cm = c(30, 40, 100),
                              designation = c("A", "Bk", "C"), cementation_class = cc,
                              coarse_fragments_pct = c(60, 0, 10),
                              clay_pct = 10, silt_pct = 20, sand_pct = 70)
  expect_false(qual_skeletic(sk(c(NA, "weakly", NA)))$passed)    # 24% over 100 cm
  s1 <- qual_skeletic(sk(c(NA, "strongly", NA)))
  expect_true(s1$passed)                                          # 60% over 0-30 cm
  expect_equal(s1$evidence$window_cm, c(0, 30))
})

test_that("v0.9.315 Alcalic reads the whole limiting layer, not only rock and technic hard material", {
  skip_on_cran()
  p <- .k22_ped(top_cm = c(0, 30, 45), bottom_cm = c(30, 45, 100),
                designation = c("A", "Bkm", "C"), ph_h2o = c(8.8, 8.6, 8),
                clay_pct = 20, silt_pct = 30, sand_pct = 50)
  expect_equal(qual_alcalic(p)$evidence$window_cm, c(0, 30))
  q <- p$clone(deep = TRUE)
  q$horizons$designation <- c("A", "Bk", "C")
  expect_equal(qual_alcalic(q)$evidence$window_cm, c(0, 50))
})


# ---- texture_class_from_pct() (WRB 2022 Annex 1, Table 8.39, p. 182) ------------

test_that("v0.9.315 texture triangle: the four boundaries that were wrong", {
  skip_on_cran()
  tc <- function(clay, silt, sand) texture_class_from_pct(clay, silt, sand)
  # sandy clay is > 45% sand: clay 40 with 45% sand is clay (was sandy clay)
  expect_equal(tc(40, 15, 45), "C")
  expect_equal(tc(40, 14, 46), "SC")
  # clay 35 to < 40 with > 45% sand is sandy clay (was sandy clay loam)
  expect_equal(tc(35, 19, 46), "SC")
  expect_equal(tc(39, 10, 51), "SC")
  expect_equal(tc(34.9, 19.1, 46), "SCL")
  expect_equal(tc(35, 20, 45), "CL")
  # silt 50 to < 80 with clay < 12 is silt loam (was silt)
  expect_equal(tc(5, 60, 35), "SIL")
  expect_equal(tc(11.9, 79.9, 8.2), "SIL")
  expect_equal(tc(11.9, 80, 8.1), "SI")
  expect_equal(tc(12, 80, 8), "SIL")
  # clay < 7, silt < 50, sand > 43 is sandy loam (was loam)
  expect_equal(tc(5, 45, 50), "SL")
  expect_equal(tc(6.9, 49.9, 43.2), "SL")
  expect_equal(tc(7, 49.9, 43.1), "L")
})

test_that("v0.9.315 texture triangle: the other boundaries stay where they were", {
  skip_on_cran()
  tc <- function(clay, silt, sand) texture_class_from_pct(clay, silt, sand)
  expect_equal(tc(c(4, 4), c(8.9, 9), c(87.1, 87)), c("S", "LS"))        # silt + 1.5 clay = 15
  expect_equal(tc(c(10, 10), c(9.9, 10), c(80.1, 80)), c("LS", "SL"))     # silt + 2 clay = 30
  expect_equal(tc(c(15, 15), c(33, 32.9), c(52, 52.1)), c("L", "SL"))     # 52% sand
  expect_equal(tc(c(20, 20), c(28, 27.9), c(52, 52.1)), c("L", "SCL"))    # 28% silt
  expect_equal(tc(c(20, 20), c(49.9, 50), c(30.1, 30)), c("L", "SIL"))    # 50% silt
  expect_equal(tc(c(26.9, 27), c(40, 40), c(33.1, 33)), c("L", "CL"))     # 27% clay
  expect_equal(tc(c(26.9, 27), c(53.1, 53), c(20, 20)), c("SIL", "SICL")) # 27% clay, silty side
  expect_equal(tc(c(30, 30), c(50, 49.9), c(20, 20.1)), c("SICL", "CL"))  # 20% sand
  expect_equal(tc(c(39.9, 40), c(30, 30), c(30.1, 30)), c("CL", "C"))     # 40% clay
  expect_equal(tc(c(40, 40), c(39.9, 40), c(20.1, 20)), c("C", "SIC"))    # 40% silt
  expect_equal(tc(c(19.9, 20), c(25, 25), c(55.1, 55)), c("SL", "SCL"))   # 20% clay
  expect_equal(tc(c(30, 30), c(25, 24.9), c(45, 45.1)), c("CL", "SCL"))   # 45% sand
})

test_that("v0.9.315 texture triangle: every point of a 1% grid as the Soil Survey Manual classes", {
  skip_on_cran()
  # the definitions of Table 8.39 (Soil Science Division Staff 2017), one by one
  ssm <- function(cl, si, sa) {
    if (si + 1.5 * cl < 15) return("S")
    if (si + 2 * cl < 30) return("LS")
    if (cl >= 40 && si >= 40) return("SIC")
    if (cl >= 35 && sa > 45) return("SC")
    if (cl >= 40) return("C")
    if (cl >= 27 && sa <= 20) return("SICL")
    if (cl >= 27 && sa <= 45) return("CL")
    if (cl >= 20 && si < 28 && sa > 45) return("SCL")
    if (si >= 80 && cl < 12) return("SI")
    if (si >= 50) return("SIL")
    if (cl >= 7 && si >= 28 && sa <= 52) return("L")
    "SL"
  }
  g <- expand.grid(cl = 0:100, sa = 0:100)
  g$si <- 100 - g$cl - g$sa
  g <- g[g$si >= 0, ]
  expect_identical(texture_class_from_pct(g$cl, g$si, g$sa),
                   unname(mapply(ssm, g$cl, g$si, g$sa)))
  skip_if_not_installed("aqp")
  expect_identical(texture_class_from_pct(g$cl, g$si, g$sa),
                   toupper(aqp::ssc_to_texcl(sand = g$sa, clay = g$cl, as.is = TRUE)))
})

test_that("v0.9.315 WRB texture classes: sandy clay without the old patch", {
  skip_on_cran()
  h <- data.frame(clay_pct = c(36, 36, 40), silt_pct = c(NA, 14, NA), sand_pct = c(50, NA, 45))
  expect_equal(.wrb_texture_class(h), c("SC", "SC", "C"))
})
