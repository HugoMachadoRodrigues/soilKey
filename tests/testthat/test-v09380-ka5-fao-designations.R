# v0.9.380: a profile that declares the FAO notation (the ISIS profiles) is
# read on the KA5 data route with its FAO letters bounding the horizons a row
# may be, and stating the field observations their definitions name (t
# coatings, w browning, p tillage, k carbonate, g and l mottling); measured
# data first; three-valued. Made-up profiles only.

.fmk <- function(h, site = list()) {
  n <- max(lengths(h))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  PedonRecord$new(site = utils::modifyList(list(id = "ka5-fao-test", country = "DE", designation_system = "fao2006"), site),
                  horizons = ensure_horizon_schema(data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}
.fidf <- function(p) soilKey:::.ka5_identify(p)

# a Parabraunerde in the FAO notation
.lu <- function(...) {
  h <- list(designation = c("Ap", "E", "Bt1", "Bt2", "C"), top_cm = c(0, 25, 45, 70, 100),
            bottom_cm = c(25, 45, 70, 100, 140), oc_pct = c(1.5, 0.4, 0.3, 0.2, 0.1),
            clay_pct = c(12, 11, 22, 20, 15), silt_pct = 75, sand_pct = c(13, 14, 3, 5, 10),
            munsell_hue_moist = "10YR", munsell_value_moist = c(3, 6, 4, 4, 5),
            munsell_chroma_moist = c(2, 3, 4, 4, 4), caco3_pct = c(0, 0, 0, 0, 0), bs_pct = 40)
  utils::modifyList(h, list(...))
}

test_that("FAO designations parse into components and KA5 families", {
  skip_on_cran()
  cp <- soilKey:::.ka5_fao_component
  b <- cp("2Btg1")
  expect_identical(b$layer, 2L)
  expect_identical(b$masters, "B")
  expect_identical(b$suffixes, c("t", "g"))
  expect_identical(b$stem, "2Btg")
  expect_identical(cp("AE")$masters, c("A", "E"))
  expect_null(cp("Ab1C"))
  expect_null(cp("I"))            # ice: no KA5 family
  expect_null(cp("XY"))
  expect_identical(cp("Bss")$suffixes, character(0))
  rule <- function(d) soilKey:::.ka5_fao_rule(list(cp(d)), d)
  ok <- function(d, code) soilKey:::.ka5_fao_compatible(rule(d), code)
  # E: an eluvial A, never an Ah or a C
  expect_true(ok("E", "Al")); expect_true(ok("E", "Ael")); expect_true(ok("E", "Ae"))
  expect_false(ok("E", "Ah")); expect_false(ok("E", "ilC")); expect_false(ok("E", "Sw"))
  expect_true(ok("Eg", "Sw")); expect_true(ok("Eg", "Ah-Sw")); expect_false(ok("Eg", "lC-Sw"))
  # A: not eluvial; the Ap needs a p-reading
  expect_true(ok("Ah", "Ah")); expect_true(ok("Ah", "Axh")); expect_false(ok("Ah", "Al"))
  expect_false(ok("Ah", "Ah-Bv")); expect_true(ok("Ap", "Ap")); expect_true(ok("Ap", "Axp"))
  expect_false(ok("Ap", "Ah"))
  # B: the suffix letters must show in a part
  expect_true(ok("Bt", "Bt")); expect_true(ok("Bt", "Btv")); expect_false(ok("Bt", "Bv"))
  expect_false(ok("Bt", "Bt-Go")); expect_true(ok("Btg", "Bt-Go")); expect_true(ok("Btg", "Bt-Sd"))
  expect_false(ok("Btg", "Sd")); expect_true(ok("Bw", "Bv")); expect_true(ok("Bw", "Ah-Bv"))
  expect_false(ok("Bw", "Bt")); expect_true(ok("Bhs", "Bhs")); expect_false(ok("Bs", "Bh"))
  expect_true(ok("B", "Bv")); expect_false(ok("B", "ilC"))
  # C and R: loose and solid rock
  expect_true(ok("C", "ilC")); expect_true(ok("C", "Go-ilC")); expect_false(ok("C", "Bv"))
  expect_true(ok("R", "imC")); expect_false(ok("R", "ilC"))
  expect_true(ok("Cr", "Gr")); expect_true(ok("O", "Of")); expect_false(ok("O", "Ah"))
  # transitional: both families
  expect_true(ok("EB", "Al")); expect_true(ok("EB", "Bv")); expect_true(ok("BC", "ilC"))
  expect_identical(rule("Btg")$defaults, c("Bt", "Sw", "Sd", "Go"))
})

test_that("only a profile that declares the FAO notation is read so", {
  skip_on_cran()
  h <- .lu()
  x <- soilKey:::.ka5_profile(.fmk(h))
  expect_length(x$fao, 5L)
  expect_null(soilKey:::.ka5_profile(.fmk(h, list(designation_system = NULL)))$fao)
  h2 <- utils::modifyList(h, list(designation = c("Ap", "Al", "Bt", "Bt", "ilC")))
  x2 <- soilKey:::.ka5_profile(.fmk(h2, list(designation_system = "ka5")))
  expect_null(x2$fao)
  # a profile in the KA5 notation keeps its notation route
  f2 <- .fidf(.fmk(h2, list(designation_system = "ka5")))
  expect_true(all(f2$route %in% c("notation", "both")))
  # a row with no designation is read on the data alone
  h3 <- utils::modifyList(h, list(designation = c("Ap", "E", "Bt1", "Bt2", NA)))
  expect_null(soilKey:::.ka5_profile(.fmk(h3))$fao[[5]])
})

test_that("an FAO Parabraunerde reads Ap/Al/Bt/Bt/C and keys LL", {
  skip_on_cran()
  p <- .fmk(.lu())
  f <- .fidf(p)
  expect_identical(f$code, c("Ap", "Al", "Bt", "Bt", "ilC"))
  expect_identical(f$route, rep("data", 5L))
  expect_match(f$evidence[2], "the FAO E: KA5 Al or Ae", fixed = TRUE)
  # the Bt2 has the clay difference of the Bt run's top (22 against 11), not
  # the one against the Bt1 above it (20 against 22)
  expect_match(f$evidence[4], "sub-horizon of the FAO Bt", fixed = TRUE)
  r <- classify_ka5(p, on_missing = "silent")
  expect_identical(r$levels$code[r$levels$level == "classes"], "L")
  expect_identical(r$levels$code[r$levels$level == "types"], "LL")
  # without the FAO declaration the same data read the E and the Bt1 so too,
  # but the Bt2 not (its clay difference to the Bt1 is negative)
  f0 <- .fidf(.fmk(.lu(), list(designation_system = NULL)))
  expect_false(identical(f0$code[4], "Bt"))
})

test_that("the FAO t states the clay coatings only where none are recorded", {
  skip_on_cran()
  # not recorded: the FAO t states them, the clay difference measured decides
  f <- .fidf(.fmk(.lu()))
  expect_identical(f$code[3], "Bt")
  expect_match(f$evidence[3], "the FAO t states them", fixed = TRUE)
  # recorded few (not the pronounced coatings, p. 97): no Bt; no horizon of
  # the FAO family holds, a conflict, undecided, never a silent pick
  f2 <- .fidf(.fmk(.lu(clay_films_amount = c(NA, NA, "few", NA, NA),
                       munsell_value_moist = c(3, 6, 4, 4, 4))))
  expect_true(is.na(f2$code[3]))
  expect_true("Bt" %in% f2$candidates[[3]])
  expect_match(f2$evidence[3], "in conflict", fixed = TRUE)
  # the clay difference measured too small (p. 97): no Bt either
  f3 <- .fidf(.fmk(.lu(clay_pct = c(12, 11, 13, 13, 12))))
  expect_true(is.na(f3$code[3]))
  # clay not recorded in the E: the Bt undecided on the data, the FAO
  # designation reads it
  f4 <- .fidf(.fmk(.lu(clay_pct = c(12, NA, 22, 20, 15))))
  expect_identical(f4$code[3], "Bt")
  expect_match(f4$evidence[3], "the FAO designation reads it", fixed = TRUE)
})

test_that("an FAO E is an Al over a Bt, an Ae over a Bs, and undecided against the colours", {
  skip_on_cran()
  # E1/E2/EB over the Bt: the Al looks under the E run and the E-B transition
  h <- list(designation = c("Ap", "E1", "E2", "EB", "Bt", "C"), top_cm = c(0, 25, 35, 45, 55, 90),
            bottom_cm = c(25, 35, 45, 55, 90, 130), oc_pct = c(1.5, 0.3, 0.3, 0.3, 0.2, 0.1),
            clay_pct = c(12, 9, 9, 10, 24, 15), silt_pct = 80, sand_pct = c(8, 11, 11, 10, 0, 5),
            munsell_hue_moist = "10YR", munsell_value_moist = c(3, 6, 6, 6, 4, 5),
            munsell_chroma_moist = c(2, 2, 2, 2, 4, 4), caco3_pct = 0, bs_pct = 60)
  f <- .fidf(.fmk(h))
  expect_identical(f$code[2:5], c("Al", "Al", "Al", "Bt"))
  # an E darker than the Bt below: not the lighter Al (p. 94), a conflict
  h$munsell_value_moist <- c(3, 4, 4, 4, 5, 5)
  f2 <- .fidf(.fmk(h))
  expect_true(is.na(f2$code[2]))
  expect_true(all(c("Al", "Ae") %in% f2$candidates[[2]]))
  # the E's colour not recorded: Al or Ael (D11)
  h$munsell_value_moist <- c(3, NA, NA, NA, 4, 5); h$munsell_chroma_moist <- c(2, NA, NA, NA, 4, 4)
  f3 <- .fidf(.fmk(h))
  expect_true(is.na(f3$code[2]))
  expect_setequal(f3$candidates[[2]], c("Al", "Ael"))
})

test_that("the FAO w reads the Bv; sub-horizons are compared with the horizon under them", {
  skip_on_cran()
  h <- list(designation = c("Ah", "Bw1", "Bw2", "C"), top_cm = c(0, 10, 30, 70), bottom_cm = c(10, 30, 70, 100),
            oc_pct = c(4, 1, 0.4, 0.2), clay_pct = 20, silt_pct = 50, sand_pct = 30,
            munsell_hue_moist = c("10YR", "7.5YR", "7.5YR", "2.5Y"), munsell_value_moist = c(3, 4, 5, 5),
            munsell_chroma_moist = c(2, 6, 6, 4), caco3_pct = c(0, 0, 0, 0), bs_pct = 20)
  p <- .fmk(h)
  f <- .fidf(p)
  expect_identical(f$carrying[2:3], c("Bv", "Bv"))
  expect_identical(f$code[4], "ilC")
  r <- classify_ka5(p, on_missing = "silent")
  expect_identical(r$levels$code[r$levels$level == "types"], "BB")
  # no colours recorded: the browning stated by the FAO w
  h$munsell_hue_moist <- NA; h$munsell_value_moist <- NA; h$munsell_chroma_moist <- NA
  f2 <- .fidf(.fmk(h))
  expect_identical(f2$carrying[2:3], c("Bv", "Bv"))
  expect_match(f2$evidence[3], "the browning stated by the FAO w", fixed = TRUE)
  # colours recorded and not browner than the C: no Bv, a conflict
  h$munsell_hue_moist <- "2.5Y"; h$munsell_value_moist <- c(3, 5, 5, 5); h$munsell_chroma_moist <- c(2, 4, 4, 4)
  f3 <- .fidf(.fmk(h))
  expect_true(is.na(f3$code[3]))
  expect_true("Bv" %in% f3$candidates[[3]])
  # an FAO Ah under the first row is no B with the Ah minimum
  h2 <- list(designation = c("Ah1", "Ah2", "Bw", "C"), top_cm = c(0, 10, 25, 60), bottom_cm = c(10, 25, 60, 90),
             oc_pct = c(4, 2, 0.4, 0.2), clay_pct = 20, silt_pct = 50, sand_pct = 30,
             munsell_hue_moist = c("10YR", "10YR", "7.5YR", "2.5Y"), munsell_value_moist = c(3, 3, 5, 5),
             munsell_chroma_moist = c(2, 3, 6, 4), caco3_pct = 0, bs_pct = 20)
  expect_identical(.fidf(.fmk(h2))$code[2], "Ah")
})

test_that("an FAO Btg is a Bt with S or G features, by the water regime the KA5 asks", {
  skip_on_cran()
  h <- list(designation = c("Ap", "Eg", "Btg1", "Btg2", "Cg"), top_cm = c(0, 25, 40, 60, 90),
            bottom_cm = c(25, 40, 60, 90, 120), oc_pct = c(1.5, 0.3, 0.2, 0.2, 0.1),
            clay_pct = c(13, 12, 24, 22, 18), silt_pct = 75, sand_pct = c(12, 13, 1, 3, 7),
            clay_films_amount = c(NA, NA, "common", "common", NA),
            redoximorphic_features_pct = c(0, 5, 30, 30, 20),
            munsell_hue_moist = "10YR", munsell_value_moist = c(3, 6, 5, 5, 5),
            munsell_chroma_moist = c(2, 2, 4, 4, 3), caco3_pct = 0, bs_pct = 60)
  # the water not recorded: Bt with S or with G features undecided, never a
  # silent pick
  f <- .fidf(.fmk(h))
  expect_true(is.na(f$code[3]))
  expect_true(all(grepl("Bt", f$candidates[[3]])))
  expect_true("Bt-Go" %in% f$candidates[[3]])
  # perched water recorded (Q13): S features 10 to < 80 %, the Bt-Sw (p. 102)
  fp <- .fidf(.fmk(h, list(groundwater_kind = "perched")))
  expect_identical(fp$code[3], "Bt-Sw")
  # groundwater recorded: the Bt-Go
  fg <- .fidf(.fmk(h, list(groundwater_kind = "observed", groundwater_depth_cm = 70)))
  expect_identical(fg$code[3], "Bt-Go")
  # the FAO g states mottling where none is recorded: rust present, its
  # share open (no silent absent)
  h$redoximorphic_features_pct <- NA
  x <- soilKey:::.ka5_profile(.fmk(h))
  sh <- soilKey:::.ka5_shares(x, 3)
  expect_identical(sh$rust, c(1, 100))
  expect_identical(soilKey:::.ka5_shares(x, 1)$rust, c(0, 0))
})

test_that("FAO O, R, k and p are read where they mean what the KA5 symbol means", {
  skip_on_cran()
  h <- list(designation = c("O", "Ah", "R"), top_cm = c(0, 5, 15), bottom_cm = c(5, 15, 40),
            oc_pct = c(40, 4, NA), clay_pct = c(NA, 15, NA), silt_pct = c(NA, 40, NA), sand_pct = c(NA, 45, NA))
  f <- .fidf(.fmk(h))
  expect_identical(f$code[3], "mC")
  expect_identical(f$top_gof[2], 0)
  # k: the secondary carbonate c where none is recorded
  h2 <- list(designation = c("Ah", "Bk", "Ck"), top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 90),
             oc_pct = c(3, 0.5, 0.2), clay_pct = 20, silt_pct = 60, sand_pct = 20, caco3_pct = c(0, 0, 15),
             munsell_hue_moist = c("10YR", "7.5YR", "10YR"), munsell_value_moist = c(3, 4, 6),
             munsell_chroma_moist = c(2, 6, 3))
  f2 <- .fidf(.fmk(h2))
  expect_match(f2$code[3], "C[a-z]*c$")
  # p: the Ap of the FAO, on any row of the topsoil
  h3 <- .lu(designation = c("Ap1", "Ap2", "Bt1", "Bt2", "C"), munsell_value_moist = c(3, 3, 4, 4, 5),
            oc_pct = c(1.5, 1.4, 0.3, 0.2, 0.1))
  expect_identical(.fidf(.fmk(h3))$code[1:2], c("Ap", "Ap"))
  # base saturation 50 % or more, bioturbation not recorded: the Axp the
  # FAO letter does not name stays open beside the Ap
  f4 <- .fidf(.fmk(.lu(bs_pct = 80)))
  expect_true(is.na(f4$code[1]))
  expect_setequal(f4$candidates[[1]], c("Axp", "Ap"))
})
