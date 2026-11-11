# v0.9.397: the South African soil classification of 1991 on the AfSP data.
# B1s (Q17 revised): with no plasticity index and no diagnostic slickensides,
# vertic A (ii) is undecided where the vertic A could hold (strong structure,
# 30 % clay or more); elsewhere the slickensides decide (Q17). MP1: mottles
# recorded only as present leave the soft plinthic B and the signs of
# wetness undecided. The AfSP loader reads Mottling, ClyMinera and Sticknss
# (synthetic rows, no AfSP data).

.za_L <- function(top, bottom, ...) {
  d <- list(top_cm = top, bottom_cm = bottom, munsell_hue_moist = "10YR", munsell_value_moist = 5,
            munsell_chroma_moist = 4, munsell_hue_dry = "10YR", munsell_value_dry = 6,
            munsell_chroma_dry = 4, structure_type = "massive", structure_grade = "structureless",
            clay_pct = 20, silt_pct = 20, sand_pct = 60, oc_pct = 0.3, effervescence_class = "none",
            clay_films_amount = "none")
  a <- list(...)
  d[names(a)] <- a
  d
}
.za_P <- function(..., site = list()) {
  s <- list(id = "za", lat = -29, lon = 30)
  s[names(site)] <- site
  ls <- list(...)
  nm <- unique(unlist(lapply(ls, names)))
  h <- as.data.frame(lapply(stats::setNames(nm, nm), function(k)
    unlist(lapply(ls, function(l) if (is.null(l[[k]])) NA else l[[k]]))), stringsAsFactors = FALSE)
  old <- options(soilKey.warn_unrecognised = FALSE)
  on.exit(options(old))
  PedonRecord$new(site = s, horizons = h)
}
.za_t <- function(f, p, i) f(soilKey:::.za_x(p), i)
.has_tag <- function(r, t) any(grepl(t, r$tag, fixed = TRUE))

test_that("B1s: no plasticity index leaves vertic A (ii) undecided only where the vertic A could hold", {
  skip_on_cran()
  v <- function(...) .za_P(.za_L(0, 40, designation = "A", structure_type = "angular blocky",
                                 structure_grade = "strong", clay_pct = 30, ...), .za_L(40, 80))
  # strong structure, 30 % clay, no slickensides recorded, no PI: undecided
  r <- .za_t(soilKey:::.za_t_VA, v(), 1)
  expect_true(is.na(r$passed)); expect_true(.has_tag(r, "plasticity_index_undecided"))
  expect_true("plasticity_index" %in% r$missing)
  # the melanic A (ii)-(iii) is undecided with it
  m <- .za_t(soilKey:::.za_ma_not_vertic, v(), 1)
  expect_true(is.na(m$passed))
  # clay not recorded: undecided too
  expect_true(is.na(.za_t(soilKey:::.za_t_VA, v(clay_pct = NA), 1)$passed))
  # structure grade not recorded and 30 % clay: (ii) undecided (the vertic A
  # is undecided on (i) too)
  r <- .za_t(soilKey:::.za_vertic_ii, v(structure_grade = NA), 1)
  expect_true(is.na(r$passed)); expect_true(.has_tag(r, "plasticity_index_undecided"))
  expect_true(is.na(.za_t(soilKey:::.za_ma_not_vertic, v(structure_grade = NA), 1)$passed))
  # 29.9 % clay: Q17 refuses
  r <- .za_t(soilKey:::.za_vertic_ii, v(clay_pct = 29.9), 1)
  expect_false(r$passed); expect_true(.has_tag(r, "slickensides_decide"))
  # moderate structure: Q17, the slickensides decide (not vertic), flagged
  r <- .za_t(soilKey:::.za_vertic_ii, v(structure_grade = "moderate"), 1)
  expect_false(r$passed); expect_true(.has_tag(r, "slickensides_decide"))
  m <- .za_t(soilKey:::.za_ma_not_vertic, v(structure_grade = "moderate"), 1)
  expect_true(m$passed); expect_true(.has_tag(m, "slickensides_decide"))
  # 25 % clay: Q17 refuses
  r <- .za_t(soilKey:::.za_t_VA, v(clay_pct = 25), 1)
  expect_false(r$passed); expect_true(.has_tag(r, "slickensides_decide"))
  # a plasticity index recorded decides
  expect_true(.za_t(soilKey:::.za_t_VA, v(plasticity_index = 40), 1)$passed)
  expect_false(.za_t(soilKey:::.za_t_VA, v(plasticity_index = 20), 1)$passed)
  expect_true(.za_t(soilKey:::.za_ma_not_vertic, v(plasticity_index = 20), 1)$passed)
  # common slickensides still decide
  expect_true(.za_t(soilKey:::.za_t_VA, v(slickensides = "common"), 1)$passed)
  # the threshold is flagged as a stand-in
  expect_true(isTRUE(soilKey:::.za_th("vertic_pi_clay")$stand_in))
  expect_identical(soilKey:::.za_thv("vertic_pi_clay"), 30L)
})

test_that("MP1: mottles recorded only as present leave the soft plinthic B and wetness undecided", {
  skip_on_cran()
  b <- function(...) .za_P(.za_L(0, 25, designation = "A"),
                           .za_L(25, 80, designation = "B", munsell_hue_moist = "7.5YR",
                                 munsell_chroma_moist = 6, ...),
                           .za_L(80, 120, designation = "C", munsell_hue_moist = "10YR",
                                 munsell_value_moist = 6, munsell_chroma_moist = 1))
  f <- .za_t(soilKey:::.za_few_mottles, b(mottles_present = TRUE), 2)
  expect_true(is.na(f$passed))
  expect_true(.za_t(soilKey:::.za_few_mottles, b(mottles_present = FALSE), 2)$passed)
  expect_true(.za_t(soilKey:::.za_few_mottles, b(), 2)$passed)
  # a recorded amount decides over the presence
  expect_false(.za_t(soilKey:::.za_few_mottles, b(mottles_present = TRUE, redoximorphic_features_pct = 20), 2)$passed)
  expect_true(is.na(.za_t(soilKey:::.za_t_SPL, b(mottles_present = TRUE), 2)$passed))
  expect_false(.za_t(soilKey:::.za_t_SPL, b(mottles_present = FALSE), 2)$passed)
  expect_true(is.na(.za_t(soilKey:::.za_wetness, b(mottles_present = TRUE), 2)$passed))
  expect_false(.za_t(soilKey:::.za_wetness, b(mottles_present = FALSE), 2)$passed)
  # with mottles present, plinthite and ferric nodules recorded as 0 do not
  # decide: only the amount of the mottles does
  expect_true(is.na(.za_t(soilKey:::.za_few_mottles,
                          b(mottles_present = TRUE, plinthite_pct = 0, ferric_nodules_pct = 0), 2)$passed))
  expect_true(.za_t(soilKey:::.za_few_mottles,
                    b(mottles_present = TRUE, plinthite_pct = 0, redoximorphic_features_pct = 5), 2)$passed)
  expect_true(.za_t(soilKey:::.za_few_mottles, b(plinthite_pct = 0, ferric_nodules_pct = 0), 2)$passed)
  # a soft carbonate horizon with mottles recorded only as present: its
  # signs of wetness (FC 15) are undecided, not absent
  sch <- function(...) .za_P(.za_L(0, 25, designation = "A"),
                             .za_L(25, 80, designation = "Ck", munsell_value_moist = 7, munsell_chroma_moist = 3,
                                   munsell_value_dry = 8, munsell_chroma_dry = 3, caco3_pct = 25,
                                   effervescence_class = "strong", ...))
  expect_true(is.na(.za_t(soilKey:::.za_sch_wetness, sch(mottles_present = TRUE), 2)$passed))
  expect_false(.za_t(soilKey:::.za_sch_wetness, sch(mottles_present = FALSE), 2)$passed)
  expect_false(.za_t(soilKey:::.za_sch_wetness, sch(), 2)$passed)
})

test_that("the AfSP loader reads Mottling, ClyMinera and Sticknss", {
  skip_on_cran()
  L <- data.frame(ProfileID = "X1", UpDpth = c(30, 0), LowDpth = c(80, 30), HorDes = c("Bv", "A"),
                  ColorM = c("10YR5/6", "10YR3/2"), ColorD = NA, Mottling = c("Y", "N"),
                  StrGrade = c("W", "S"), StrSize = NA, StrType = c("S", "A"),
                  Sticknss = c("VST", "ST"), ClyMinera = c("SE", "MO"), Clay = c(30, 40),
                  stringsAsFactors = FALSE)
  for (f in c("Silt", "Sand", "CfPc", "PHH2O", "PHKCl", "PHCaCl2", "EC", "OrgC", "TotalN", "CecSoil", "Ecec",
              "Bsat", "ExCa", "ExMg", "ExK", "ExNa", "ExAl", "CaCO3", "CaSO4", "BlkDens")) L[[f]] <- -9999
  P <- data.frame(ProfileID = "X1", Country = "ZA", WRB06rg = "VR", WRB06 = "", FAO88 = "", USDA = "",
                  Drain = "I", ParMat = "SO1", LndForm = "LV", stringsAsFactors = FALSE)
  p <- soilKey:::.afsp_to_pedon(P, L)
  h <- p$horizons
  # the layers are sorted by depth and each code follows its layer
  expect_equal(h$top_cm, c(0, 30))
  expect_identical(h$mottles_present, c(FALSE, TRUE))
  expect_identical(h$clay_mineralogy, c("smectite", NA))
  expect_identical(h$stickiness, c("moderately sticky", "very sticky"))
  # the site fields: test-v09399-afsp-site-fields.R
  pr <- as.data.frame(p$provenance)
  expect_true(any(pr$attribute == "clay_mineralogy" & pr$horizon_idx == 2 &
                    grepl("sesquioxidic", pr$notes)))
  # ClyMinera is an estimate: inferred_prior, confidence under 1, grade C
  cmp <- pr[pr$attribute == "clay_mineralogy" & pr$horizon_idx == 1, ]
  expect_identical(cmp$source, "inferred_prior")
  expect_true(cmp$confidence < 1)
  expect_identical(soilKey:::compute_evidence_grade(p, NULL), "C")
  L2 <- L; L2$ClyMinera <- c("GI", "SM")
  h2 <- soilKey:::.afsp_to_pedon(P, L2)$horizons
  expect_identical(h2$clay_mineralogy, c("smectite", NA))
  expect_true(any(pr$attribute == "mottles_present" & pr$horizon_idx == 2 & grepl("present", pr$notes)))
  # fields absent: nothing read
  p2 <- soilKey:::.afsp_to_pedon(P[, 1:6], L[, setdiff(names(L), c("Mottling", "ClyMinera", "Sticknss"))])
  expect_true(all(is.na(p2$horizons$mottles_present)))
  expect_true(all(is.na(p2$horizons$clay_mineralogy)))
  # the code tables
  expect_identical(unname(soilKey:::.AFSP_MOTTLING[c("Y", "N")]), c(TRUE, FALSE))
})
