# v0.9.322: the limiting layer of WRB 2022 Ch 2.1, rule 8 (p. 23) reads the
# "other cemented layers" (a cemented Bsm, a layer recorded as at least
# moderately cemented), and texture_class_from_pct() follows every boundary
# of the USDA texture triangle (WRB 2022 Annex 1, Table 8.39, p. 182; Soil
# Science Division Staff 2017). Small made-up pedons; no third-party data.

.k22_ped <- function(..., site = list(id = "k22")) {
  hz <- ensure_horizon_schema(data.table::as.data.table(data.frame(..., stringsAsFactors = FALSE)))
  PedonRecord$new(site = site, horizons = hz)
}


# ---- the limiting layer (Ch 2.1, rule 8, p. 23) ---------------------------------

test_that("v0.9.322 cemented layers: the class decides, else an m after the cementing agent", {
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

test_that("v0.9.322 limiting layer: a cemented Bsm or a moderately cemented layer is one", {
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

test_that("v0.9.322 Humic and Skeletic count to the cemented layer", {
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

test_that("v0.9.322 Alcalic reads the whole limiting layer, not only rock and technic hard material", {
  skip_on_cran()
  p <- .k22_ped(top_cm = c(0, 30, 45), bottom_cm = c(30, 45, 100),
                designation = c("A", "Bkm", "C"), ph_h2o = c(8.8, 8.6, 8),
                clay_pct = 20, silt_pct = 30, sand_pct = 50)
  expect_equal(qual_alcalic(p)$evidence$window_cm, c(0, 30))
  q <- p$clone(deep = TRUE)
  q$horizons$designation <- c("A", "Bk", "C")
  expect_equal(qual_alcalic(q)$evidence$window_cm, c(0, 50))
})
