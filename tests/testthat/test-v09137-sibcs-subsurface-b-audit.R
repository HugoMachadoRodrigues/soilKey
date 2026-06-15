# v0.9.137 -- SiBCS subsurface-B diagnostic-HORIZON audit slice (Embrapa 2018
# Cap 2, verbatim, p.59-74). Confirmed divergences fixed; each refine-when-
# present so data lacking the field stays byte-identical.
#
# Verbatim grounding:
#   B nitico        p.62 (a) thickness >=30 (>=15 if lithic contact <=50cm);
#                   (c) structure GRADE moderate/strong + cerosidade quantity
#                   >=common AND grade moderate/strong; (d) low-activity OR
#                   (high-activity AND aluminic) -- NO ferric path.
#   B incipiente    p.60 (a) must NOT have duripa/petrocalcico/fragipa/plintita/glei.
#   vertico         p.73 cracks ">= 1 cm" (SiBCS) vs WRB/USDA 0.5 cm.
#   sulfurico       p.72-73 pH<=3.5 + >=15cm + (jarosite OR sulfidic OR sulfate).

mkh <- function(df) ensure_horizon_schema(data.table::as.data.table(df))
prh <- function(hz) PedonRecord$new(site = list(id = "t"), horizons = hz)


# ---- B_nitico structure-grade + cerosidade-grade --------------------------

test_that("v0.9.137: B_nitico requires structure GRADE moderate/strong", {
  base <- data.frame(top_cm = c(0, 20), bottom_cm = c(20, 60),
                     designation = c("A", "Bt"), clay_pct = c(40, 50),
                     structure_type = c("granular", "blocks"),
                     clay_films_amount = c(NA, "common"),
                     clay_films_strength = c(NA, "moderate"))
  wk <- base; wk$structure_grade <- c(NA, "weak")
  expect_false(isTRUE(B_nitico(prh(mkh(wk)))$passed))
  mo <- base; mo$structure_grade <- c(NA, "moderate")
  expect_true(isTRUE(B_nitico(prh(mkh(mo)))$passed))
  na <- base; na$structure_grade <- NA_character_
  expect_true(isTRUE(B_nitico(prh(mkh(na)))$passed))     # byte-identical
})

test_that("v0.9.137: B_nitico requires cerosidade GRADE moderate/strong", {
  base <- data.frame(top_cm = c(0, 20), bottom_cm = c(20, 60),
                     designation = c("A", "Bt"), clay_pct = c(40, 50),
                     structure_type = c("granular", "blocks"),
                     structure_grade = c(NA, "moderate"),
                     clay_films_amount = c(NA, "common"))
  wk <- base; wk$clay_films_strength <- c(NA, "weak")
  expect_false(isTRUE(B_nitico(prh(mkh(wk)))$passed))
  mo <- base; mo$clay_films_strength <- c(NA, "moderate")
  expect_true(isTRUE(B_nitico(prh(mkh(mo)))$passed))
})

test_that("v0.9.137: B_nitico honours the thickness exception (>=15cm over rock <=50cm)", {
  short <- data.frame(top_cm = c(0, 20, 38), bottom_cm = c(20, 38, 60),
                      designation = c("A", "Bt", "R"), clay_pct = c(40, 50, NA),
                      structure_type = c("granular", "blocks", NA),
                      structure_grade = c(NA, "moderate", NA),
                      clay_films_amount = c(NA, "common", NA),
                      clay_films_strength = c(NA, "moderate", NA))
  # the 18 cm Bt would fail the flat 30 cm rule, but the lithic contact at 38 cm
  # relaxes the minimum to 15 cm.
  expect_true(isTRUE(B_nitico(prh(mkh(short)))$passed))
})

test_that("v0.9.137: B_nitico drops the non-verbatim ferric short-circuit", {
  # high-activity clay (Ta = cec_cmol*100/clay >= 27), high Fe-DCB, NOT aluminic:
  # before v0.9.137 the ferri path rescued it; now criterion (d) rejects it.
  fer <- data.frame(top_cm = c(0, 20), bottom_cm = c(20, 60),
                    designation = c("A", "Bt"), clay_pct = c(40, 50),
                    cec_cmol = c(20, 22), al_cmolc_kg = c(0.1, 0.1), bs_pct = c(80, 80),
                    structure_type = c("granular", "blocks"),
                    structure_grade = c(NA, "moderate"),
                    clay_films_amount = c(NA, "common"),
                    clay_films_strength = c(NA, "moderate"), fe_dcb_pct = c(NA, 10))
  expect_true(isTRUE(atividade_argila_alta(prh(mkh(fer)))$passed))   # Ta clay
  expect_false(isTRUE(carater_alitico(prh(mkh(fer)))$passed))        # not aluminic
  expect_false(isTRUE(B_nitico(prh(mkh(fer)))$passed))               # so NOT nitico
})

test_that("v0.9.137: the canonical Nitossolo still classifies as Nitossolos", {
  expect_true(isTRUE(B_nitico(make_nitossolo_canonical())$passed))
  expect_equal(classify_sibcs(make_nitossolo_canonical(),
                              on_missing = "silent")$rsg_or_order, "Nitossolos")
})


# ---- B_incipiente exclusions (duripa/petrocalcico/fragipa/plintita/glei) ----

test_that("v0.9.137: B_incipiente excludes a gleyed layer", {
  gl <- data.frame(top_cm = c(0, 20), bottom_cm = c(20, 60),
                   designation = c("A", "Bg"), clay_pct = c(20, 25),
                   munsell_hue_moist = c("10YR", "10Y"),
                   munsell_value_moist = c(4, 5), munsell_chroma_moist = c(3, 1),
                   structure_type = c("granular", "blocks"))
  expect_true(isTRUE(gleyic_properties(prh(mkh(gl)))$passed))
  expect_false(isTRUE(B_incipiente(prh(mkh(gl)))$passed))   # excluded
})

test_that("v0.9.137: B_incipiente is byte-identical on a plain cambic Bw", {
  bw <- data.frame(top_cm = c(0, 20), bottom_cm = c(20, 60),
                   designation = c("A", "Bw"), clay_pct = c(20, 25),
                   munsell_hue_moist = c("10YR", "7.5YR"),
                   munsell_value_moist = c(4, 4), munsell_chroma_moist = c(3, 6),
                   structure_type = c("granular", "blocks"))
  expect_true(isTRUE(B_incipiente(prh(mkh(bw)))$passed))
})


# ---- horizonte_vertico crack >= 1 cm (SiBCS strict) -----------------------

test_that("v0.9.137: SiBCS vertico needs cracks >= 1cm; WRB stays at 0.5cm", {
  vt <- data.frame(top_cm = 0, bottom_cm = 40, designation = "Bv",
                   clay_pct = 50, slickensides = "common", cracks_width_cm = 0.7)
  expect_false(isTRUE(horizonte_vertico(prh(mkh(vt)))$passed))  # 0.7 < 1.0
  expect_true(isTRUE(vertic_horizon(prh(mkh(vt)))$passed))      # WRB 0.5 byte-id
  vt$cracks_width_cm <- 1.3
  expect_true(isTRUE(horizonte_vertico(prh(mkh(vt)))$passed))
})


# ---- horizonte_sulfurico jarosite OR-path ---------------------------------

test_that("v0.9.137: sulfurico fires via the jarosite path (no sulfidic-S)", {
  sf <- data.frame(top_cm = 0, bottom_cm = 20, designation = "Bj",
                   ph_h2o = 3.2, jarosite_present = TRUE)
  expect_true(isTRUE(horizonte_sulfurico(prh(mkh(sf)))$passed))
  # without jarosite AND without sulfidic-S -> still FALSE (byte-identical)
  sf$jarosite_present <- NA
  expect_false(isTRUE(horizonte_sulfurico(prh(mkh(sf)))$passed))
})
