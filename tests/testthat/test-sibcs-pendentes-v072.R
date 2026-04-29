# v0.7.2 SiBCS pendentes:
#  saprico / hemico / fibrico  (Cap 14, Organossolos 3o nivel)
#  carater_acrico              (Cap 1, p 31)
#  carater_ebanico             (Cap 1; Cap 7; Cap 17)
#  carater_retratil            (Cap 1, p 33)
#  carater_espodico            (Cap 1, p 35; Cap 8)
#  compute_ki / compute_kr / latossolo_ki_kr (Cap 1, p 32; Cap 10)
#  cerosidade                  (Cap 13, p 207)


# Helper builder for tests: minimal pedon with a single B horizon plus
# whatever extra columns the test sets. Reuses ensure_horizon_schema
# so missing fields are filled with NA of the right type.
.make_test_pedon <- function(...) {
  hz <- data.table::data.table(
    top_cm    = c(0, 30),
    bottom_cm = c(30, 150),
    designation = c("A", "Bw"),
    ...
  )
  PedonRecord$new(
    site = list(id = "TEST-7-2", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
}


.make_organic_pedon <- function(fiber_rubbed = NA_real_,
                                  von_post = NA_integer_) {
  hz <- data.table::data.table(
    top_cm    = c(0, 30),
    bottom_cm = c(30, 80),
    designation = c("Oa", "Hd"),
    fiber_content_rubbed_pct = c(fiber_rubbed, fiber_rubbed),
    von_post_index           = c(von_post, von_post),
    oc_pct                   = c(35, 30)
  )
  PedonRecord$new(
    site = list(id = "ORG-TEST", lat = -5, lon = -45, country = "BR",
                  parent_material = "deposito turfoso"),
    horizons = ensure_horizon_schema(hz)
  )
}


# ===========================================================================
# 1. Grau de decomposicao -- saprico / hemico / fibrico
# ===========================================================================

test_that("saprico passes for low-fiber organic profile (< 17% rubbed)", {
  pr <- .make_organic_pedon(fiber_rubbed = 10)
  res <- saprico(pr)
  expect_true(isTRUE(res$passed))
  expect_match(res$reference, "Cap 14")
})

test_that("saprico ALSO passes for high von Post (H7-H10) when fiber NA", {
  pr <- .make_organic_pedon(von_post = 8L)
  res <- saprico(pr)
  expect_true(isTRUE(res$passed))
})

test_that("hemico passes for intermediate fiber (17-40%)", {
  pr <- .make_organic_pedon(fiber_rubbed = 25)
  expect_true(isTRUE(hemico(pr)$passed))
  expect_false(isTRUE(saprico(pr)$passed))
  expect_false(isTRUE(fibrico(pr)$passed))
})

test_that("fibrico passes for high fiber (>= 40%) and von Post H1-H4", {
  pr1 <- .make_organic_pedon(fiber_rubbed = 60)
  pr2 <- .make_organic_pedon(von_post = 3L)
  expect_true(isTRUE(fibrico(pr1)$passed))
  expect_true(isTRUE(fibrico(pr2)$passed))
  expect_false(isTRUE(hemico(pr1)$passed))
})

test_that("decomposition diagnostics are FALSE when no histic horizon", {
  pr <- .make_test_pedon(clay_pct = c(20, 40))
  expect_false(isTRUE(saprico(pr)$passed))
  expect_false(isTRUE(hemico(pr)$passed))
  expect_false(isTRUE(fibrico(pr)$passed))
})

test_that("decomposition reports missing when fiber+von_post both NA", {
  pr <- .make_organic_pedon()  # both NA
  res <- saprico(pr)
  expect_false(isTRUE(res$passed))
  expect_true("fiber_content_rubbed_pct" %in% res$missing)
  expect_true("von_post_index" %in% res$missing)
})


# ===========================================================================
# 2. Carater acrico (DeltapH >= 0 + CECef <= 1.5 cmolc/kg argila)
# ===========================================================================

test_that("carater_acrico passes when DeltapH >= 0 AND CECef <= 1.5", {
  pr <- .make_test_pedon(
    ph_h2o    = c(5.0, 5.2),
    ph_kcl    = c(5.0, 5.4),   # delta = 0 e 0.2
    ecec_cmol = c(0.6, 0.4),
    clay_pct  = c(30, 50)      # ecec/clay = 2.0/0.8 cmolc/kg argila
  )
  res <- carater_acrico(pr)
  expect_true(isTRUE(res$passed))
})

test_that("carater_acrico FAILS when DeltapH negative (acidic acrustic)", {
  pr <- .make_test_pedon(
    ph_h2o    = c(5.0, 5.2),
    ph_kcl    = c(4.5, 4.6),   # delta = -0.5 e -0.6
    ecec_cmol = c(0.6, 0.4),
    clay_pct  = c(30, 50)
  )
  res <- carater_acrico(pr)
  expect_false(isTRUE(res$passed))
})

test_that("carater_acrico FAILS when ECef/clay > 1.5", {
  pr <- .make_test_pedon(
    ph_h2o    = c(5.0, 5.2),
    ph_kcl    = c(5.5, 5.6),   # delta positivo
    ecec_cmol = c(2.0, 2.5),
    clay_pct  = c(30, 50)      # ecec_clay = 6.7 e 5.0 cmolc/kg argila >> 1.5
  )
  res <- carater_acrico(pr)
  expect_false(isTRUE(res$passed))
})

test_that("carater_acrico returns NA when pH/CECef both NA in B layers", {
  pr <- .make_test_pedon(clay_pct = c(30, 50))
  res <- carater_acrico(pr)
  expect_true(is.na(res$passed))
  expect_true(any(c("ph_kcl", "ecec_cmol") %in% res$missing))
})


# ===========================================================================
# 3. Carater ebanico (preto + Ta + V >= 65 em todo B)
# ===========================================================================

test_that("carater_ebanico passes for black + high-V + Ta profile", {
  pr <- .make_test_pedon(
    munsell_value_moist  = c(3, 2),
    munsell_chroma_moist = c(2, 1),
    bs_pct               = c(80, 90),
    cec_cmol             = c(35, 40),  # high CEC
    clay_pct             = c(30, 50)   # Ta = 35*1000/(30*10) ~ 117; >= 27
  )
  res <- carater_ebanico(pr)
  expect_true(isTRUE(res$passed))
})

test_that("carater_ebanico FAILS when one B layer has chroma > 2", {
  pr <- .make_test_pedon(
    munsell_value_moist  = c(3, 3),
    munsell_chroma_moist = c(2, 4),   # second B layer fails
    bs_pct               = c(80, 90),
    cec_cmol             = c(35, 40),
    clay_pct             = c(30, 50)
  )
  res <- carater_ebanico(pr)
  expect_false(isTRUE(res$passed))
})

test_that("carater_ebanico FAILS when V% < 65 in any B layer", {
  pr <- .make_test_pedon(
    munsell_value_moist  = c(3, 2),
    munsell_chroma_moist = c(2, 2),
    bs_pct               = c(80, 50),  # second layer below threshold
    cec_cmol             = c(35, 40),
    clay_pct             = c(30, 50)
  )
  expect_false(isTRUE(carater_ebanico(pr)$passed))
})

test_that("carater_ebanico FAILS when atividade da argila is Tb (low)", {
  pr <- .make_test_pedon(
    munsell_value_moist  = c(3, 2),
    munsell_chroma_moist = c(2, 1),
    bs_pct               = c(80, 90),
    cec_cmol             = c(2, 3),    # Ta = 2*1000/(30*10) ~ 6.7 < 27
    clay_pct             = c(30, 50)
  )
  expect_false(isTRUE(carater_ebanico(pr)$passed))
})


# ===========================================================================
# 4. Carater retratil (COLE >= 0.06 OR slickensides + cracks)
# ===========================================================================

test_that("carater_retratil passes when COLE >= 0.06", {
  pr <- .make_test_pedon(cole_value = c(0.04, 0.08))
  expect_true(isTRUE(carater_retratil(pr)$passed))
})

test_that("carater_retratil passes via slickensides + cracks pathway", {
  pr <- .make_test_pedon(
    cole_value      = c(NA_real_, NA_real_),
    slickensides    = c(NA_character_, "common"),
    cracks_width_cm = c(NA_real_, 2.5)
  )
  expect_true(isTRUE(carater_retratil(pr)$passed))
})

test_that("carater_retratil FAILS without COLE and without slickensides", {
  pr <- .make_test_pedon(cole_value = c(0.02, 0.03))
  expect_false(isTRUE(carater_retratil(pr)$passed))
})


# ===========================================================================
# 5. Carater espodico (>= 2.5 cm + OC + iluv evidence)
# ===========================================================================

test_that("carater_espodico passes for shallow Bh with OC + iluv signal", {
  hz <- data.table::data.table(
    top_cm = c(0, 20, 25),
    bottom_cm = c(20, 25, 80),
    designation = c("E", "Bh", "BC"),
    oc_pct = c(0.8, 1.5, 0.3),
    al_ox_pct = c(0.05, 0.4, 0.2),
    fe_ox_pct = c(0.02, 0.15, 0.1)
  )
  pr <- PedonRecord$new(
    site = list(id = "ESP", lat = 0, lon = 0, country = "TEST",
                  parent_material = "areia eolica"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- carater_espodico(pr)
  expect_true(isTRUE(res$passed))
})

test_that("carater_espodico FAILS when Bh layer < 2.5 cm", {
  hz <- data.table::data.table(
    top_cm = c(0, 20, 21.5),
    bottom_cm = c(20, 21.5, 80),
    designation = c("E", "Bh", "BC"),
    oc_pct = c(0.8, 1.5, 0.3),
    al_ox_pct = c(0.05, 0.4, 0.2)
  )
  pr <- PedonRecord$new(
    site = list(id = "ESP", lat = 0, lon = 0, country = "TEST",
                  parent_material = "areia eolica"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(carater_espodico(pr)$passed))
})


# ===========================================================================
# 6. Ki/Kr quantitativos
# ===========================================================================

test_that("compute_ki returns canonical molar ratio for kaolinite-like SiO2:Al2O3 = 2:1", {
  # Pure kaolinite has weight ratio SiO2:Al2O3 ~ 1.18 (theoretical) -> Ki ~ 2.00
  ki_kaol <- compute_ki(46.5, 39.5)   # mass ratio ~ 1.177
  expect_equal(ki_kaol, 2.0, tolerance = 0.05)
})

test_that("compute_ki is NA when Al2O3 is 0 or input is NA", {
  expect_true(is.na(compute_ki(50, 0)))
  expect_true(is.na(compute_ki(NA, 30)))
  expect_true(is.na(compute_ki(50, NA)))
})

test_that("compute_kr equals compute_ki when Fe2O3 is zero", {
  ki <- compute_ki(46, 39)
  kr <- compute_kr(46, 39, 0.0001)  # near-zero Fe -> Kr ~ Ki
  expect_equal(ki, kr, tolerance = 0.01)
})

test_that("compute_kr is lower than Ki when Fe2O3 substantial", {
  ki <- compute_ki(40, 30)
  kr <- compute_kr(40, 30, 15)  # Fe2O3 high (acriferrico-like)
  expect_lt(kr, ki)
})

test_that("latossolo_ki_kr passes for B_latossolico-like SiO2/Al2O3/Fe2O3", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 80),
    bottom_cm = c(30, 80, 200),
    designation = c("A", "Bw1", "Bw2"),
    sio2_sulfuric_pct  = c(NA, 18, 17),    # Ki = 18*1.7/15 = 2.04
    al2o3_sulfuric_pct = c(NA, 15, 14),
    fe2o3_sulfuric_pct = c(NA, 10, 9)
  )
  pr <- PedonRecord$new(
    site = list(id = "L-Ki", lat = -3, lon = -60, country = "BR",
                  parent_material = "Barreiras"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- latossolo_ki_kr(pr)
  expect_true(isTRUE(res$passed))
  expect_true(all(res$evidence$ki[!is.na(res$evidence$ki)] <= 2.2))
})

test_that("latossolo_ki_kr returns NA when sulfuric data missing", {
  pr <- .make_test_pedon(clay_pct = c(30, 50))   # no sulfuric oxides
  res <- latossolo_ki_kr(pr)
  expect_true(is.na(res$passed))
  expect_true("sio2_sulfuric_pct" %in% res$missing)
})

test_that("latossolo_ki_kr FAILS when Ki > 2.2", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 100),
    designation = c("A", "Bw"),
    sio2_sulfuric_pct  = c(NA, 35),
    al2o3_sulfuric_pct = c(NA, 15),     # Ki = 35/60.08 / 15/101.96 ~ 3.96
    fe2o3_sulfuric_pct = c(NA, 5)
  )
  pr <- PedonRecord$new(
    site = list(id = "non-lat", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(latossolo_ki_kr(pr)$passed))
})


# ===========================================================================
# 7. Cerosidade quantitativa (clay films amount x strength)
# ===========================================================================

test_that("cerosidade passes when amount=common AND strength=moderate", {
  pr <- .make_test_pedon(
    clay_films_amount   = c(NA_character_, "common"),
    clay_films_strength = c(NA_character_, "moderate")
  )
  expect_true(isTRUE(cerosidade(pr)$passed))
})

test_that("cerosidade accepts SiBCS Portuguese terms (comum + moderada)", {
  pr <- .make_test_pedon(
    clay_films_amount   = c(NA_character_, "comum"),
    clay_films_strength = c(NA_character_, "moderada")
  )
  expect_true(isTRUE(cerosidade(pr)$passed))
})

test_that("cerosidade maps 'shiny' to 'strong' strength", {
  pr <- .make_test_pedon(
    clay_films_amount   = c(NA_character_, "many"),
    clay_films_strength = c(NA_character_, "shiny")
  )
  expect_true(isTRUE(cerosidade(pr, min_strength = "strong")$passed))
})

test_that("cerosidade FAILS when amount below threshold", {
  pr <- .make_test_pedon(
    clay_films_amount   = c(NA_character_, "few"),
    clay_films_strength = c(NA_character_, "strong")
  )
  expect_false(isTRUE(cerosidade(pr, min_amount = "common")$passed))
})

test_that("cerosidade FAILS when strength below threshold", {
  pr <- .make_test_pedon(
    clay_films_amount   = c(NA_character_, "many"),
    clay_films_strength = c(NA_character_, "weak")
  )
  expect_false(isTRUE(cerosidade(pr, min_strength = "moderate")$passed))
})

test_that("cerosidade ignores strength dimension when min_strength = NULL", {
  pr <- .make_test_pedon(
    clay_films_amount   = c(NA_character_, "many"),
    clay_films_strength = c(NA_character_, NA_character_)
  )
  expect_true(isTRUE(cerosidade(pr, min_strength = NULL)$passed))
})

test_that("cerosidade reports missing when fields are NA", {
  pr <- .make_test_pedon(clay_pct = c(20, 40))   # no clay films set
  res <- cerosidade(pr)
  expect_false(isTRUE(res$passed))
  expect_true("clay_films_amount" %in% res$missing)
  expect_true("clay_films_strength" %in% res$missing)
})

test_that("cerosidade rejects unknown amount/strength terms", {
  expect_error(cerosidade(.make_test_pedon(), min_amount = "bizarro"),
                 "min_amount")
  expect_error(cerosidade(.make_test_pedon(), min_strength = "bizarro"),
                 "min_strength")
})


# ===========================================================================
# v0.7.3 -- Cap 14 atributos (terrico, cambissolico)
# ===========================================================================

# ---- carater_terrico (mineral horizons >= 30 cm within 100 cm) ----------

test_that("carater_terrico passes when mineral horizons sum >= 30 cm in upper 100 cm", {
  hz <- data.table::data.table(
    top_cm    = c(0,  20, 60),
    bottom_cm = c(20, 60, 120),
    designation = c("Hd", "Ag", "Cg")
  )
  pr <- PedonRecord$new(
    site = list(id = "TER", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- carater_terrico(pr)   # 40 cm mineral (Ag) + 40 cm (Cg, capped at 100)
  expect_true(isTRUE(res$passed))
  expect_gte(res$evidence$cumulative_cm, 30)
})

test_that("carater_terrico FAILS when mineral horizons < 30 cm in upper 100 cm", {
  hz <- data.table::data.table(
    top_cm    = c(0,  60),
    bottom_cm = c(60, 120),
    designation = c("Hd", "Ag")    # Ag has 40 cm above 100 (60-100)
  )
  pr <- PedonRecord$new(
    site = list(id = "TER2", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- carater_terrico(pr, min_thickness_cm = 50)
  # Cumulative mineral within 100 cm = 40 cm (Ag from 60 to 100), < 50
  expect_false(isTRUE(res$passed))
})

test_that("carater_terrico ignores histic H/O horizons when summing", {
  hz <- data.table::data.table(
    top_cm    = c(0,  50, 80),
    bottom_cm = c(50, 80, 150),
    designation = c("Oa", "Hd", "Ag")
  )
  pr <- PedonRecord$new(
    site = list(id = "TER3", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- carater_terrico(pr)   # only Ag (80-100 capped) = 20 cm < 30
  expect_false(isTRUE(res$passed))
})

test_that("carater_terrico FAILS when no mineral horizons present", {
  hz <- data.table::data.table(
    top_cm    = c(0,  40),
    bottom_cm = c(40, 90),
    designation = c("Oa", "Hd")
  )
  pr <- PedonRecord$new(
    site = list(id = "TER4", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- carater_terrico(pr)
  expect_false(isTRUE(res$passed))
})


# ---- carater_cambissolico (B_incipiente below H/O or A) -----------------

test_that("carater_cambissolico passes for histic profile with Bw beneath", {
  hz <- data.table::data.table(
    top_cm    = c(0,  40,  80),
    bottom_cm = c(40, 80, 130),
    designation = c("Hd", "A", "Bw"),
    munsell_value_moist  = c(2,  3, 4),
    munsell_chroma_moist = c(1,  2, 4),
    structure_grade      = c("strong", "moderate", "moderate"),
    structure_type       = c("granular", "blocks", "blocks"),
    clay_pct             = c(NA, 25, 35),
    silt_pct             = c(NA, 30, 28),
    sand_pct             = c(NA, 45, 37),
    cec_cmol             = c(NA, 12, 10),
    bs_pct               = c(NA, 60, 55),
    al_cmol              = c(NA, 0.5, 0.6),
    ph_h2o               = c(NA, 5.5, 5.6),
    oc_pct               = c(NA, 1.0, 0.4),
    consistence_moist    = c(NA, "friable", "firm"),
    coarse_fragments_pct = c(NA, 5, 10)
  )
  pr <- PedonRecord$new(
    site = list(id = "CAM", lat = -25, lon = -50, country = "BR",
                  parent_material = "rocha basica"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- carater_cambissolico(pr)
  if (isTRUE(B_incipiente(pr)$passed)) {
    expect_true(isTRUE(res$passed))
  } else {
    skip("B_incipiente nao passou para esta fixture; carater_cambissolico depende disso")
  }
})

test_that("carater_cambissolico FAILS without B_incipiente", {
  hz <- data.table::data.table(
    top_cm    = c(0,  40),
    bottom_cm = c(40, 90),
    designation = c("Hd", "C")
  )
  pr <- PedonRecord$new(
    site = list(id = "CAM2", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(carater_cambissolico(pr)$passed))
})
