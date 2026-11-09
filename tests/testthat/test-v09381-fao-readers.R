# v0.9.381: the FAO-Unesco legend (Revised Legend 1988, Legend 1974), part 1:
# the new attributes and the shared readers of R/fao-readers.R, on hand
# profiles. The readers are internal; the diagnostics and keys that call
# them follow in later parts.

.fao_h <- function(...) {
  cols <- list(...)
  n <- length(cols[[1L]])
  h <- make_empty_horizons(n)
  for (k in names(cols)) data.table::set(h, j = k, value = cols[[k]])
  h
}

# A sandy loam over a sandy clay loam B: Ap, E, Bt, C.
.fao_luvic <- function() {
  .fao_h(top_cm = c(0, 20, 45, 90), bottom_cm = c(20, 45, 90, 130),
         designation = c("Ap", "E", "Bt", "C"),
         clay_pct = c(12, 13, 25, 22), silt_pct = c(18, 19, 25, 23),
         sand_pct = c(70, 68, 50, 55), oc_pct = c(2, 1, 0.5, 0.2),
         cec_cmol = c(10, 6, 8, 7), ecec_cmol = c(5, 4, NA, NA),
         bs_pct = c(60, NA, 40, NA),
         ca_cmol = c(4, 2, 2, 2), mg_cmol = c(1, 1, 1, 1),
         k_cmol = c(0.2, 0.1, 0.1, 0.1), na_cmol = c(0.1, 0.1, 0.1, 0.1),
         munsell_hue_moist = c("10YR", "10YR", "5YR", "N"),
         munsell_value_moist = c(3, 5, 4, 6), munsell_chroma_moist = c(2, 3, 6, 0))
}

test_that("the new attributes are in the schema, documented and recognised", {
  skip_on_cran()
  hz <- c(p2o5_citric_mg_kg = "numeric", extractable_acidity_ph82_cmol = "numeric",
          cec_ph82_cmol = "numeric", al_dcb_pct = "numeric",
          exch_acidity_kcl_cmol = "numeric", ph_h2o_1to1 = "numeric",
          ph_h2o_1to5 = "numeric", rock_structure_pct = "numeric",
          smeary_consistence = "logical", bleached_ped_coatings = "logical",
          ignition_redder = "logical", spodic_pellets = "logical",
          skeletans_pct = "numeric", skeletan_thickness_mm = "numeric",
          carbonate_coatings_faces_pct = "numeric", nodule_size_max_cm = "numeric",
          dta_low_endotherm = "logical", munsell_hue_rubbed = "character",
          munsell_value_rubbed = "numeric", munsell_chroma_rubbed = "numeric",
          pitted_quartz_pct = "numeric", palygorskite_pct = "numeric",
          aeolian_crack_infill = "logical")
  spec <- horizon_column_spec()
  expect_identical(unlist(spec[names(hz)]), hz)
  a <- horizon_attribute_spec()
  rows <- a[match(names(hz), a$name), ]
  expect_true(all(rows$section == "FAO-Unesco legend (1988 and 1974)"))
  expect_true(all(nzchar(rows$description) & nzchar(rows$unit)))
  expect_true(all(grepl("p\\. [0-9]", rows$description)))
  expect_true(all(names(hz) %in% names(make_empty_horizons(1))))
  s <- site_attribute_spec()
  st <- s[match(c("flooding_days_growing_period", "rudic_recorded", "reference_fao74"),
                s$name), ]
  expect_identical(st$type, c("numeric", "logical", "character"))
  expect_false(anyNA(st$name))
  h <- data.frame(top_cm = c(0, 20), bottom_cm = c(20, 60), ph_h2o_1to1 = c(4, 3),
                  smeary_consistence = c(TRUE, FALSE), munsell_hue_rubbed = c("5YR", "2.5YR"))
  tab <- unrecognised_attributes(h, site = list(id = "p", rudic_recorded = TRUE,
                                                flooding_days_growing_period = 12,
                                                reference_fao74 = "Lc"))
  expect_identical(nrow(tab), 0L)
})

test_that("edition, depths, windows and 'within' read the plan's conventions", {
  skip_on_cran()
  expect_identical(soilKey:::.fao_edition(88), "1988")
  expect_identical(soilKey:::.fao_edition("fao1974"), "1974")
  expect_error(soilKey:::.fao_edition("2006"), "Unknown edition")
  # Q9: the top of the first recorded horizon is 0, an O written above 0 too
  h <- .fao_h(top_cm = c(-5, 0, 30), bottom_cm = c(0, 30, 80),
              designation = c("Oa", "A", "Bw"), clay_pct = c(NA, 10, 20))
  d <- soilKey:::.fao_depths(h)
  expect_identical(d$top, c(0, 5, 35)); expect_identical(d$bottom, c(5, 35, 85))
  p <- .fao_luvic()
  bs <- soilKey:::.fao_bs(p)$value
  # throughout 20-50: E (53 %) and Bt (40 %): fails on the Bt
  r <- soilKey:::.fao_in_window(p, bs >= 50, 20, 50, "throughout")
  expect_false(r$value); expect_identical(r$layers, 3L)
  expect_true(soilKey:::.fao_in_window(p, bs >= 50, 0, 40, "throughout")$value)
  expect_true(soilKey:::.fao_in_window(p, bs < 50, 20, 125, "some")$value)
  # a window below the description is undecided, a window below rock fails
  short <- .fao_h(top_cm = c(0, 20), bottom_cm = c(20, 40), designation = c("A", "Bw"))
  r <- soilKey:::.fao_in_window(short, c(TRUE, TRUE), 20, 50, "throughout")
  expect_true(is.na(r$value)); expect_identical(r$missing, c("top_cm", "bottom_cm"))
  rocky <- .fao_h(top_cm = c(0, 15), bottom_cm = c(15, 40), designation = c("A", "R"))
  expect_false(soilKey:::.fao_in_window(rocky, c(TRUE, NA), 20, 50, "throughout")$value)
  # subset: throughout the B to 125 cm
  p2 <- p; p2$bs_pct <- c(60, 55, 70, NA)
  isb <- grepl("^B", p2$designation)
  expect_true(soilKey:::.fao_in_window(p2, soilKey:::.fao_bs(p2)$value >= 50, 0, 125,
                                       "throughout", subset = isb)$value)
  # within: the top of the feature at or above the depth
  expect_true(soilKey:::.fao_within(p, p$clay_pct > 20, 45)$value)
  expect_false(soilKey:::.fao_within(p, p$clay_pct > 20, 44)$value)
  expect_true(is.na(soilKey:::.fao_within(short, c(FALSE, FALSE), 50)$value))
  expect_false(soilKey:::.fao_within(rocky, c(FALSE, NA), 50)$value)
})

test_that("weighted means, mixing and the strongly humic zero below rock", {
  skip_on_cran()
  p <- .fao_luvic()
  # OC 0-100: (2 x 20 + 1 x 25 + 0.5 x 45 + 0.2 x 10) / 100
  expect_equal(soilKey:::.fao_weighted(p, "oc_pct", 0, 100)$value, 0.895)
  expect_equal(soilKey:::.fao_mixed(p, "oc_pct", 18)$value, 2)
  expect_equal(soilKey:::.fao_mixed(p, "oc_pct", 25)$value, (2 * 20 + 1 * 5) / 25)
  p$oc_pct[3] <- NA
  r <- soilKey:::.fao_weighted(p, "oc_pct", 0, 100)
  expect_true(is.na(r$value)); expect_identical(r$missing, "oc_pct")
  shallow <- .fao_h(top_cm = c(0, 30, 60), bottom_cm = c(30, 60, 70),
                    designation = c("A", "Bw", "R"), oc_pct = c(3, 1.5, NA))
  # 1988 p. 40: the depth below the soil counts as zero
  expect_equal(soilKey:::.fao_weighted(shallow, "oc_pct", 0, 100,
                                       zero_below_rock = TRUE)$value, (90 + 45) / 100)
  expect_equal(soilKey:::.fao_weighted(shallow, "oc_pct", 0, 100)$value, (90 + 45) / 60)
  open <- .fao_h(top_cm = c(0, 30), bottom_cm = c(30, 60), designation = c("A", "Bw"),
                 oc_pct = c(3, 1.5))
  expect_true(is.na(soilKey:::.fao_weighted(open, "oc_pct", 0, 100)$value))
})

test_that("organic matter, pH, CEC, ECEC and base saturation by method", {
  skip_on_cran()
  om <- soilKey:::.fao_om(c(1, 0.58), "1974")
  expect_equal(om$value, c(1.724, 0.99992))
  expect_identical(om$flags, "om_from_oc_x1.724")
  h <- .fao_h(top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 100),
              ph_h2o = c(5, 4, 8.8), ph_h2o_1to1 = c(NA, 3.2, NA), ph_h2o_1to5 = c(5.6, NA, NA),
              clay_pct = c(20, 40, 50), cec_cmol = c(8, 6, 30),
              cec_method = c(NA, "nh4cl", "cobaltihexammine"), ecec_cmol = c(3, NA, NA),
              ca_cmol = c(1, 0.5, 20), mg_cmol = c(0.5, 0.2, 5), k_cmol = c(0.2, 0.1, 1),
              na_cmol = c(0.1, 0.1, 2), al_kcl_cmol = c(1, 2, NA),
              exch_acidity_kcl_cmol = c(NA, 2.5, NA), bs_pct = c(NA, 20, 80))
  ph <- soilKey:::.fao_ph(h, "1to1")
  expect_identical(ph$value, c(5, 3.2, 8.8))
  expect_identical(ph$flags, c("ph_h2o_for_1to1", NA, "ph_h2o_for_1to1"))
  expect_identical(soilKey:::.fao_ph(h, "1to5")$value, c(5.6, 4, 8.8))
  a <- soilKey:::.fao_cec_clay(h, "nh4oac")
  expect_equal(a$value, c(40, 15, 60))
  expect_identical(a$flags, c(NA, "cec_by_nh4cl", "cec_by_cobaltihexammine"))
  b <- soilKey:::.fao_cec_clay(h, "nh4cl")
  expect_equal(b$value, c(15, 15, NA))
  expect_identical(b$flags, c("ecec_for_nh4cl_cec", NA, NA))
  expect_equal(soilKey:::.fao_cec_clay(h, "nh4oac", per = "soil")$value, c(8, 6, 30))
  e88 <- soilKey:::.fao_ecec_clay(h, "1988")
  # bases + KCl acidity; al_kcl_cmol flagged where the acidity is missing
  expect_equal(e88$value, c((1.8 + 1) * 5, (0.9 + 2.5) * 2.5, NA))
  expect_identical(e88$flags, c("al_kcl_for_kcl_acidity", NA, NA))
  e74 <- soilKey:::.fao_ecec_clay(h, "1974")
  expect_equal(e74$value, c((1.8 + 1) * 5, (0.9 + 2) * 2.5, NA))
  h$ca_cmol[1] <- NA
  expect_identical(soilKey:::.fao_ecec_clay(h, "1988")$flags[1], "ecec_as_recorded")
  bs <- soilKey:::.fao_bs(.fao_h(bs_pct = c(NA, 20, 80, NA), cec_cmol = c(10, 6, 30, NA),
                                 cec_method = c(NA, "nh4cl", NA, NA),
                                 ca_cmol = c(4, 1, 1, 1), mg_cmol = c(1, 1, 1, 1),
                                 k_cmol = c(0, 0, 0, 0), na_cmol = c(0, 0, 0, 0)))
  expect_equal(bs$value, c(50, 20, 80, NA))
  expect_identical(bs$flags, c("bs_computed", "bs_by_cec_nh4cl", NA, NA))
  # defect 8: both flags when the computed value rests on another CEC method
  bs2 <- soilKey:::.fao_bs(.fao_h(bs_pct = NA_real_, cec_cmol = 10, cec_method = "kcl",
                                  ca_cmol = 4, mg_cmol = 1, k_cmol = 0, na_cmol = 0))
  expect_identical(bs2$flags, "bs_computed; bs_by_cec_kcl")
})

test_that("the texture classes of both editions and the texture groups", {
  skip_on_cran()
  cls <- function(clay, sand, ed) soilKey:::.fao_texture_class_pct(clay, sand, ed)
  # 1988 p. 107: coarse < 15 % clay and > 70 % sand; fine > 35 % clay
  expect_identical(cls(c(14, 14, 15, 14, 35, 36, 36, NA, 10),
                       c(71, 70, 80, 60, 30, NA, 10, 50, NA), "1988"),
                   c("coarse", "medium", "medium", "medium", "medium", "fine", "fine",
                     NA, NA))
  # 1974 p. 5: coarse < 18 % clay and > 65 % sand
  expect_identical(cls(c(17, 17, 18, 16, 35, 40), c(66, 65, 70, 72, 40, 20), "1974"),
                   c("coarse", "medium", "medium", "coarse", "medium", "fine"))
  expect_identical(cls(16, 68, "1988"), "medium")
  expect_identical(cls(16, 68, "1974"), "coarse")
  p <- .fao_luvic()
  expect_identical(soilKey:::.fao_texture_class(p, "1974"),
                   c("coarse", "coarse", "medium", "medium"))
  # upper 30 cm: clay (12 x 20 + 13 x 10) / 30, sand (70 x 20 + 68 x 10) / 30
  u <- soilKey:::.fao_texture_upper30(p, "1988")
  expect_equal(u$clay, 370 / 30); expect_equal(u$sand, 2080 / 30)
  expect_identical(u$value, "medium")
  expect_identical(soilKey:::.fao_texture_upper30(p, "1974")$value, "coarse")
  expect_identical(soilKey:::.fao_depth_by_texture(p)$value, 125)
  fine <- .fao_h(top_cm = c(0, 30), bottom_cm = c(30, 80), clay_pct = c(45, 50),
                 silt_pct = c(30, 30), sand_pct = c(25, 20))
  expect_identical(soilKey:::.fao_depth_by_texture(fine)$value, 75)
  expect_true(is.na(soilKey:::.fao_depth_by_texture(
    .fao_h(top_cm = 0, bottom_cm = 20, clay_pct = 20))$value))
  # sandy loam or finer: silt + 2 x clay >= 30 (the USDA boundary)
  sl <- .fao_h(clay_pct = c(5, 8, NA), silt_pct = c(15, 14, 20), sand_pct = c(80, 78, NA))
  expect_identical(soilKey:::.fao_sandy_loam_or_finer(sl), c(FALSE, TRUE, NA))
  expect_identical(soilKey:::.fao_coarser_than_sandy_loam(sl), c(TRUE, FALSE, NA))
  tg <- .fao_h(clay_pct = c(50, 30, 15, 5), silt_pct = c(25, 35, 40, 5),
               sand_pct = c(25, 35, 45, 90))
  expect_identical(soilKey:::.fao_tonguing_texture(tg),
                   c("fine", "moderately fine", "medium or coarser", "medium or coarser"))
})

test_that("the solum and buried horizons", {
  skip_on_cran()
  p <- .fao_luvic()
  s <- soilKey:::.fao_solum(p)
  expect_identical(s$value, 90); expect_false(s$open)
  # a description that ends in the B gives a lower bound
  s2 <- soilKey:::.fao_solum(p[1:3, ])
  expect_identical(s2$value, 90); expect_true(s2$open)
  # AC counts, CB does not; H and O do not
  q <- .fao_h(top_cm = c(0, 10, 40, 70), bottom_cm = c(10, 40, 70, 100),
              designation = c("Oe", "A", "AC", "CB"))
  expect_identical(soilKey:::.fao_solum(q)$value, 70)
  # decision E (2026-11-09): an undesignated layer between the last A, E or B
  # and the first C is undecided; one above the last B does not matter
  r <- .fao_h(top_cm = c(0, 25, 60, 80), bottom_cm = c(25, 60, 80, 100),
              designation = c("A", "Bw", NA, "C"))
  s3 <- soilKey:::.fao_solum(r)
  expect_true(is.na(s3$value)); expect_identical(s3$missing, "designation")
  r2 <- .fao_h(top_cm = c(0, 25, 60, 80), bottom_cm = c(25, 60, 80, 100),
               designation = c("A", NA, "Bw", "C"))
  expect_identical(soilKey:::.fao_solum(r2)$value, 80)
  r3 <- .fao_h(top_cm = c(0, 25, 60, 80), bottom_cm = c(25, 60, 80, 100),
               designation = c("A", "Bw", "C", NA))
  expect_identical(soilKey:::.fao_solum(r3)$value, 60)
  # the Q9 datum: an O on top counts in the depth of the solum
  o <- .fao_h(top_cm = c(0, 10, 30), bottom_cm = c(10, 30, 60), designation = c("Oe", "A", "C"))
  expect_identical(soilKey:::.fao_solum(o)$value, 30)
  expect_true(is.na(soilKey:::.fao_solum(.fao_h(top_cm = 0, bottom_cm = 20))$value))
  # buried by 50 cm or more of new material (1988 p. 42; 74 p. 32)
  b <- .fao_h(top_cm = c(0, 30, 55, 80), bottom_cm = c(30, 55, 80, 120),
              designation = c("A", "C", "2Ab", "2Btb"))
  expect_identical(soilKey:::.fao_buried(b, "1988")$value, c(FALSE, FALSE, TRUE, TRUE))
  b40 <- .fao_h(top_cm = c(0, 40, 70), bottom_cm = c(40, 70, 100),
                designation = c("A", "Ab", "Bwb"))
  expect_identical(soilKey:::.fao_buried(b40, "1974")$value, c(FALSE, FALSE, FALSE))
  expect_identical(soilKey:::.fao_buried(b40, "1988", mantle_andic = TRUE)$value,
                   c(FALSE, TRUE, TRUE))
  expect_identical(soilKey:::.fao_buried(b40, "1988", mantle_andic = FALSE)$value,
                   c(FALSE, FALSE, FALSE))
  und <- soilKey:::.fao_buried(b40, "1988")
  expect_identical(und$value, c(FALSE, NA, NA))
  expect_identical(soilKey:::.fao_buried(p)$value, rep(FALSE, 4))
})

test_that(".fao_clay_increase equals the usda branch of test_clay_increase_argic on a grid", {
  skip_on_cran()
  # whole percents and the half percents beside the two breaks
  up <- sort(c(0:60, 14.5, 15.5, 39.5, 40.5))
  grid <- expand.grid(upper = up, lower = sort(c(0:80, 17.5, 18.5, 47.5, 48.5)))
  usda <- vapply(seq_len(nrow(grid)), function(k) {
    h <- .fao_h(top_cm = c(0, 20), bottom_cm = c(20, 50), designation = c("A", "B"),
                clay_pct = c(grid$upper[k], grid$lower[k]))
    isTRUE(soilKey:::test_clay_increase_argic(h, system = "usda")$passed)
  }, logical(1))
  pair <- soilKey:::.fao_clay_increase_pair(grid$upper, grid$lower)
  expect_identical(pair, usda)
  prof <- vapply(seq_len(nrow(grid)), function(k) {
    h <- .fao_h(top_cm = c(0, 20), bottom_cm = c(20, 50), designation = c("A", "B"),
                clay_pct = c(grid$upper[k], grid$lower[k]))
    soilKey:::.fao_clay_increase(h)$value[2]
  }, logical(1))
  expect_identical(prof, usda)
  # the printed examples (1988 p. 28): 17 vs 14, 36 vs 30, 50 vs 42
  expect_identical(soilKey:::.fao_clay_increase_pair(c(14, 30, 42, 14, 30, 42),
                                                     c(17, 36, 50, 16.9, 35.9, 49.9)),
                   c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_true(is.na(soilKey:::.fao_clay_increase_pair(NA, 20)))
})

test_that("the clay increase in a profile: distance and discontinuities", {
  skip_on_cran()
  p <- .fao_luvic()
  r <- soilKey:::.fao_clay_increase(p)
  expect_identical(r$value, c(FALSE, FALSE, TRUE, FALSE))
  expect_identical(r$upper[3], 1L)   # the Ap, 25 cm above the Bt top: within 30 cm
  # 15 cm without clay skins: only the E (directly above) is compared
  r15 <- soilKey:::.fao_clay_increase(p, distance = 15)
  expect_identical(r15$value[3], TRUE); expect_identical(r15$upper[3], 2L)
  # the increase is reached over a transition thicker than the distance
  t <- .fao_h(top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 100),
              designation = c("A", "BA", "Bt"), clay_pct = c(10, 11, 20))
  expect_identical(soilKey:::.fao_clay_increase(t, distance = 30)$value[3], TRUE)
  t$clay_pct[2] <- 18
  expect_identical(soilKey:::.fao_clay_increase(t, distance = 30)$value[3], FALSE)
  t$clay_pct[2] <- NA
  expect_true(is.na(soilKey:::.fao_clay_increase(t, distance = 30)$value[3]))
  # a lithological discontinuity: undecided, not failed
  ld <- .fao_h(top_cm = c(0, 30), bottom_cm = c(30, 70), designation = c("A", "2Bt"),
               clay_pct = c(10, 25))
  expect_true(is.na(soilKey:::.fao_clay_increase(ld)$value[2]))
  # other numbers as arguments (the WRB 2022 bands, for instance)
  expect_identical(soilKey:::.fao_clay_increase_pair(10, 15, steps = c(6, 1.4, 20),
                                                     breaks = c(15, 50)), FALSE)
})

test_that("hue rank, hue comparisons and rubbed colours", {
  skip_on_cran()
  expect_identical(soilKey:::.fao_hue_rank(c("10R", "2.5YR", "5YR", "7.5YR", "10YR",
                                             "2.5Y", "5Y", "N", "x", NA)),
                   c(10, 12.5, 15, 17.5, 20, 22.5, 25, NA, NA, NA))
  hues <- c("5YR", "7.5YR", "10YR", "2.5Y", "N", NA)
  expect_identical(soilKey:::.fao_hue_cmp(hues, "7.5YR", "redder"),
                   c(TRUE, FALSE, FALSE, FALSE, FALSE, NA))
  expect_identical(soilKey:::.fao_hue_cmp(hues, "5YR", "as_red"),
                   c(TRUE, FALSE, FALSE, FALSE, FALSE, NA))
  expect_identical(soilKey:::.fao_hue_cmp(hues, "2.5Y", "as_yellow"),
                   c(FALSE, FALSE, FALSE, TRUE, FALSE, NA))
  h <- .fao_h(munsell_hue_moist = c("5YR", "10YR", NA),
              munsell_value_moist = c(3, 4, NA), munsell_chroma_moist = c(4, 6, NA),
              munsell_hue_rubbed = c("2.5YR", NA, NA),
              munsell_value_rubbed = c(3, NA, NA), munsell_chroma_rubbed = c(6, NA, NA))
  col <- soilKey:::.fao_colour_rubbed(h)
  expect_identical(col$hue, c("2.5YR", "10YR", NA))
  expect_identical(col$chroma, c(6, 6, NA))
  expect_identical(col$flags, c(NA, "moist_colour_for_rubbed", NA))
  # "less than 3.5" on the recorded chip: 3 passes, 3.5 fails (Q24)
  expect_identical(c(3, 3.5, 4) < 3.5, c(TRUE, FALSE, FALSE))
})

test_that("the new attributes are read with their fallbacks", {
  skip_on_cran()
  h <- .fao_h(top_cm = c(0, 20, 50, 80), bottom_cm = c(20, 50, 80, 120),
              designation = c("A", "Bw", "BC", "Cr"),
              smeary_consistence = c(TRUE, NA, NA, NA), thixotropic_index = c(NA, 60, 20, NA),
              ignition_redder = c(NA, TRUE, NA, NA), fe_dcb_pct = c(0.3, NA, 0.8, NA),
              rock_structure_pct = c(NA, 10, NA, NA), saprolite_pct = c(NA, NA, NA, NA),
              palygorskite_pct = c(3, NA, NA, NA),
              clay_mineralogy = c(NA, "palygorskite", "smectite", NA),
              spodic_pellets = c(NA, TRUE, NA, NA))
  sm <- soilKey:::.fao_smeary(h)
  expect_identical(sm$value, c(TRUE, TRUE, FALSE, FALSE))
  expect_identical(sm$flags, c(NA, "thixotropic_index_for_smeary",
                               "thixotropic_index_for_smeary",
                               "smeary_consistence_not_recorded_absent"))
  ig <- soilKey:::.fao_ignition_redder(h)
  # decision C (2026-11-09): Fe >= 0.5 % leaves the test undecided
  expect_identical(ig$value, c(FALSE, TRUE, NA, NA))
  expect_identical(ig$flags, c("free_iron_for_ignition", NA, NA, NA))
  rs <- soilKey:::.fao_rock_structure(h)
  expect_identical(rs$value, c(0, 10, NA, NA))
  expect_identical(rs$flags, c("rock_structure_not_recorded_absent", NA,
                               "rock_structure_from_designation",
                               "rock_structure_from_designation"))
  # defect 9: a rock layer is 100 % rock structure, not absent
  rr <- .fao_h(top_cm = c(0, 30), bottom_cm = c(30, 60), designation = c("A", "R"))
  expect_identical(soilKey:::.fao_rock_structure(rr)$value, c(0, 100))
  h$saprolite_pct[4] <- 70
  expect_identical(soilKey:::.fao_rock_structure(h)$value[4], 70)
  pa <- soilKey:::.fao_palygorskite(h)
  expect_identical(pa$value, c(3, 100, 0, 0))
  fo <- soilKey:::.fao_field_obs(h, "spodic_pellets")
  expect_identical(fo$value, c(FALSE, TRUE, FALSE, FALSE))
  expect_identical(fo$flags[2], NA_character_)
})

test_that("the readers' defaults are the numbers of the threshold tables", {
  skip_on_cran()
  root <- testthat::test_path("..", "..", "data-raw")
  f88 <- file.path(root, "fao_thresholds_1988.yaml")
  f74 <- file.path(root, "fao_thresholds_1974.yaml")
  fr <- file.path(root, "fao_readings.yaml")
  skip_if_not(file.exists(f88) && file.exists(f74) && file.exists(fr),
              "package sources not available")
  t88 <- yaml::read_yaml(f88); t74 <- yaml::read_yaml(f74)
  inc <- formals(soilKey:::.fao_clay_increase_pair)
  expect_identical(eval(inc$steps), as.numeric(unlist(t88$horizons$argic_b$clay_increase$steps)))
  expect_identical(eval(inc$breaks), as.numeric(unlist(t88$horizons$argic_b$clay_increase$breaks_pct)))
  expect_identical(t74$horizons$argillic_b$clay_increase$steps, t88$horizons$argic_b$clay_increase$steps)
  expect_equal(formals(soilKey:::.fao_clay_increase)$distance,
               t88$horizons$argic_b$distance_with_skins_cm)
  expect_identical(formals(soilKey:::.fao_om)$factor, t74$units$om_from_oc_factor$value)
  expect_equal(t88$general$buried_new_material_cm$value, formals(soilKey:::.fao_buried)$min_cm)
  expect_equal(t88$general$buried_new_material_andic_cm$value,
               formals(soilKey:::.fao_buried)$min_andic_cm)
  expect_equal(eval(formals(soilKey:::.fao_depth_by_texture)$depths),
               unlist(t74$texture_classes$depth_by_class_cm$value))
  expect_identical(t88$texture_classes$coarse, list(clay_lt = 15L, sand_gt = 70L))
  expect_identical(t74$texture_classes$coarse, list(clay_lt = 18L, sand_gt = 65L))
  expect_identical(formals(soilKey:::.fao_ignition_redder)$fe_min_pct, 0.5)
  # every diagnostic horizon and property carries its page
  for (t in list(t88, t74)) {
    for (sec in c("horizons", "properties")) {
      for (k in names(t[[sec]])) expect_false(is.null(t[[sec]][[k]]$page), info = k)
    }
  }
  r <- yaml::read_yaml(fr)$readings
  expect_true(length(r) >= 10L)
  expect_true(all(vapply(r, function(x) all(c("topic", "edition", "page", "reading") %in% names(x)),
                         logical(1))))
  expect_true(all(grepl("\\((Hugo|soilKey's reading, for Hugo to confirm), 2026-11-0[89]\\)\\.$",
                        vapply(r, `[[`, "", "reading"))))
})

test_that("the review of 2026-11-09: gaps, overlaps, coverage and vector answers", {
  skip_on_cran()
  # defect 1: a gap in the depths (A 0-20, C 40-80) leaves the window open
  g <- .fao_h(top_cm = c(0, 40), bottom_cm = c(20, 80), designation = c("A", "C"),
              clay_pct = c(10, 30))
  r <- soilKey:::.fao_weighted(g, "clay_pct", 0, 60)
  expect_true(is.na(r$value)); expect_identical(r$missing, c("top_cm", "bottom_cm"))
  expect_true(is.na(soilKey:::.fao_in_window(g, c(TRUE, TRUE), 0, 60)$value))
  expect_true(is.na(soilKey:::.fao_within(g, c(FALSE, FALSE), 50)$value))
  expect_false(soilKey:::.fao_in_window(g, c(TRUE, FALSE), 0, 60)$value)
  expect_equal(soilKey:::.fao_weighted(g, "clay_pct", 0, 20)$value, 10)
  # a layer without depths
  n <- .fao_h(top_cm = c(0, NA, 40), bottom_cm = c(20, 40, 80),
              designation = c("A", "Bw", "C"), clay_pct = c(10, 20, 30))
  expect_true(is.na(soilKey:::.fao_weighted(n, "clay_pct", 0, 60)$value))
  # defect 10: overlapping layers are flagged and undecided
  o <- .fao_h(top_cm = c(0, 10), bottom_cm = c(30, 50), designation = c("A", "B"),
              clay_pct = c(10, 30))
  ro <- soilKey:::.fao_weighted(o, "clay_pct", 0, 30)
  expect_true(is.na(ro$value)); expect_identical(ro$flags, "overlapping_layers")
  # defect 7: 'within 40' fails only when the description goes below 40,
  # or reaches rock at 40
  s40 <- .fao_h(top_cm = c(0, 20), bottom_cm = c(20, 40), designation = c("A", "B"))
  expect_true(is.na(soilKey:::.fao_within(s40, c(FALSE, FALSE), 40)$value))
  expect_false(soilKey:::.fao_within(s40, c(FALSE, FALSE), 39)$value)
  r40 <- .fao_h(top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 60), designation = c("A", "B", "R"))
  expect_false(soilKey:::.fao_within(r40, c(FALSE, FALSE, NA), 40)$value)
  # defect 2: a vector answer with NA keeps what would decide it
  expect_identical(soilKey:::.fao_om(c(1, NA))$missing, "oc_pct")
  t <- .fao_h(top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 100),
              designation = c("A", "BA", "Bt"), clay_pct = c(10, NA, 20))
  expect_identical(soilKey:::.fao_clay_increase(t)$missing, "clay_pct")
  b40 <- .fao_h(top_cm = c(0, 40), bottom_cm = c(40, 70), designation = c("A", "Ab"))
  expect_identical(soilKey:::.fao_buried(b40, "1988")$missing, "andic properties of the mantle")
  expect_identical(soilKey:::.fao_om(1)$missing, character(0))
  # defect 6: buried suffixes after figures (Bt2b, Ap2b)
  b <- .fao_h(top_cm = c(0, 50, 70), bottom_cm = c(50, 70, 100),
              designation = c("C", "Ap2b", "Bt2b"))
  expect_identical(soilKey:::.fao_buried(b)$value, c(FALSE, TRUE, TRUE))
})

test_that("the decisions of 2026-11-09: rock, strongly humic, organic surface", {
  skip_on_cran()
  # D: bedrock_pct >= 90 is rock only with a hard rupture resistance
  h <- .fao_h(top_cm = c(0, 30), bottom_cm = c(30, 60), designation = c("A", "C"),
              bedrock_pct = c(NA, 95), rupture_resistance = c(NA, "soft"))
  expect_identical(soilKey:::.fao_is_rock(h), c(FALSE, FALSE))
  h$rupture_resistance[2] <- "very hard"
  expect_identical(soilKey:::.fao_is_rock(h), c(FALSE, TRUE))
  h$bedrock_pct[2] <- 80
  expect_identical(soilKey:::.fao_is_rock(h), c(FALSE, FALSE))
  expect_identical(soilKey:::.fao_is_rock(.fao_h(designation = c("A", "2R", "R/Cr"))),
                   c(FALSE, TRUE, TRUE))
  # B: strongly humic in a soil shallower than 50 cm: divided by 100, flagged
  sh <- .fao_h(top_cm = c(0, 30), bottom_cm = c(30, 40), designation = c("A", "R"),
               oc_pct = c(3, NA))
  r <- soilKey:::.fao_weighted(sh, "oc_pct", 0, 100, zero_below_rock = TRUE)
  expect_equal(r$value, 0.9)
  expect_identical(r$flags, "zero_below_rock_soil_shallower_than_50cm")
  deep <- .fao_h(top_cm = c(0, 60), bottom_cm = c(60, 80), designation = c("A", "R"),
                 oc_pct = c(2, NA))
  r2 <- soilKey:::.fao_weighted(deep, "oc_pct", 0, 100, zero_below_rock = TRUE)
  expect_equal(r2$value, 1.2); expect_identical(r2$flags, character(0))
  # A: the upper 30 cm under an organic surface start at the first mineral horizon
  org <- .fao_h(top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 90),
                designation = c("Oa", "A", "Bw"), oc_pct = c(30, 2, 1),
                clay_pct = c(NA, 10, 20), silt_pct = c(NA, 15, 30), sand_pct = c(NA, 75, 50))
  u <- soilKey:::.fao_texture_upper30(org, "1988")
  # the A alone (20-50 cm), not the O and the top of the A
  expect_equal(u$clay, 10); expect_identical(u$value, "coarse")
  expect_true("texture_from_mineral_surface" %in% u$flags)
  expect_identical(soilKey:::.fao_texture_upper30(org[2:3, ], "1988")$flags, character(0))
  und <- .fao_h(top_cm = c(0, 20), bottom_cm = c(20, 60), oc_pct = c(25, 1),
                clay_pct = c(NA, 10), sand_pct = c(NA, 75))
  expect_identical(soilKey:::.fao_texture_upper30(und, "1988")$value, "coarse")
  # the 1974 D nodules: a diameter of 2 to 30 cm
  root <- testthat::test_path("..", "..", "data-raw")
  skip_if_not(file.exists(file.path(root, "fao_thresholds_1974.yaml")))
  t74 <- yaml::read_yaml(file.path(root, "fao_thresholds_1974.yaml"))
  expect_identical(unlist(t74$key$D$nodule_diameter_cm), c(2L, 30L))
  expect_null(t74$key$D$nodules_within_cm)
  expect_identical(t74$key$X$depth_by_class_cm, t74$key$C$depth_by_class_cm)
  expect_identical(t74$general$two_b_horizons_within_cm$page, 32L)
})
