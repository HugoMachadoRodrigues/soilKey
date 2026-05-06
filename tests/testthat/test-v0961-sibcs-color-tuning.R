# =============================================================================
# Tests for v0.9.61 -- SiBCS color tuning (dominant-color-in-B post-processor).
# =============================================================================


# ---- helpers --------------------------------------------------------------

.make_color_tuning_pedon <- function(id = "ct1",
                                       ordem_first = "ARGISSOLO",
                                       hues   = c("10YR", "7.5YR", "5YR", "5YR"),
                                       values = c(4,      4,       4,     4),
                                       chromas = c(3,     4,       6,     6),
                                       tops   = c(0,    20,   55,   115),
                                       bots   = c(20,   55,   115,  170),
                                       desg   = c("A",  "AB", "Bt1", "Bt2"),
                                       clay   = c(20, 28, 45, 42)) {
  hz <- data.table::data.table(
    top_cm    = tops,
    bottom_cm = bots,
    designation          = desg,
    munsell_hue_moist    = hues,
    munsell_value_moist  = values,
    munsell_chroma_moist = chromas,
    structure_grade      = c("moderate","moderate","strong","strong"),
    structure_type       = c("granular","subangular blocky",
                                "subangular blocky","subangular blocky"),
    clay_films_amount    = c(NA, "few", "common", "common"),
    clay_pct             = clay,
    silt_pct             = c(30, 25, 20, 22),
    sand_pct             = 100 - clay - c(30, 25, 20, 22),
    ph_h2o               = c(5.5, 5.3, 5.0, 5.0),
    oc_pct               = c(1.5, 0.6, 0.3, 0.2),
    cec_cmol             = c(8, 6, 5.5, 4.5),
    bs_pct               = c(35, 25, 20, 18),
    al_cmol              = c(0.5, 0.8, 1.2, 1.5)
  )
  PedonRecord$new(
    site = list(
      id                = id,
      lat               = -22.86, lon = -43.78, country = "BR",
      reference_sibcs   = sprintf("%s tipico", ordem_first),
      reference_nivel_1 = ordem_first,
      reference_source  = "synthetic"
    ),
    horizons = ensure_horizon_schema(hz)
  )
}


# ---- .classify_b_color ----------------------------------------------------

test_that(".classify_b_color identifies VERMELHO from red hues", {
  expect_equal(soilKey:::.classify_b_color("2.5YR", 4, 6), "VERMELHO")
  expect_equal(soilKey:::.classify_b_color("10R",   3, 6), "VERMELHO")
  expect_equal(soilKey:::.classify_b_color("7.5R",  4, 5), "VERMELHO")
})

test_that(".classify_b_color identifies VERMELHO_AMARELO at 5YR", {
  expect_equal(soilKey:::.classify_b_color("5YR", 5, 6), "VERMELHO_AMARELO")
})

test_that(".classify_b_color identifies AMARELO at 7.5YR/10YR with chroma >= 4", {
  expect_equal(soilKey:::.classify_b_color("7.5YR", 5, 6), "AMARELO")
  expect_equal(soilKey:::.classify_b_color("10YR",  5, 4), "AMARELO")
})

test_that(".classify_b_color identifies BRUNO_ACINZENTADO when dark + non-red", {
  expect_equal(soilKey:::.classify_b_color("5YR",   3, 3), "BRUNO_ACINZENTADO")
  expect_equal(soilKey:::.classify_b_color("7.5YR", 4, 3), "BRUNO_ACINZENTADO")
  expect_equal(soilKey:::.classify_b_color("10YR",  4, 4), "BRUNO_ACINZENTADO")
})

test_that(".classify_b_color identifies ACINZENTADO at pale yellow side", {
  expect_equal(soilKey:::.classify_b_color("10YR", 6, 2), "ACINZENTADO")
  expect_equal(soilKey:::.classify_b_color("2.5Y", 5, 3), "ACINZENTADO")
})

test_that(".classify_b_color returns NA on missing inputs", {
  expect_true(is.na(soilKey:::.classify_b_color(NA,    4, 6)))
  expect_true(is.na(soilKey:::.classify_b_color("5YR", NA, 6)))
  expect_true(is.na(soilKey:::.classify_b_color("5YR", 4, NA)))
})


# ---- .dominant_b_color ----------------------------------------------------

test_that(".dominant_b_color picks the thicker B color category", {
  # Bt1 (35 cm) AMARELO (10YR 5/6) vs Bt2 (55 cm) VERMELHO (2.5YR 4/6)
  ped <- .make_color_tuning_pedon(
    hues    = c("10YR", "7.5YR", "10YR",  "2.5YR"),
    values  = c(4,      4,       5,       4),
    chromas = c(3,      4,       6,       6),
    tops    = c(0,    20,   55,   115),
    bots    = c(20,   55,   115,  170)
  )
  d <- soilKey:::.dominant_b_color(ped)
  # Bt1 thickness 60 (AMARELO), Bt2 thickness 55 (VERMELHO)
  expect_equal(d$dominant, "AMARELO")
  expect_true(d$n_b_layers >= 1L)
  expect_true(d$n_classified >= 1L)
})


test_that(".dominant_b_color reports NA when no B color is measured", {
  ped <- .make_color_tuning_pedon(
    hues    = c("10YR", "7.5YR", NA, NA),
    values  = c(4,      4,       NA, NA),
    chromas = c(3,      4,       NA, NA)
  )
  d <- soilKey:::.dominant_b_color(ped)
  expect_true(is.na(d$dominant))
  expect_equal(d$n_classified, 0L)
})


# ---- .dominant_b_color_subordem ------------------------------------------

test_that(".dominant_b_color_subordem maps Argissolos by dominant color", {
  ped_v <- .make_color_tuning_pedon(
    hues    = c("10YR", "7.5YR", "2.5YR", "2.5YR"),
    values  = c(4,      4,       4,       4),
    chromas = c(3,      4,       6,       6)
  )
  expect_equal(soilKey:::.dominant_b_color_subordem(ped_v, "P")$code, "PV")

  ped_a <- .make_color_tuning_pedon(
    hues    = c("10YR", "7.5YR", "10YR",  "10YR"),
    values  = c(4,      4,       5,       5),
    chromas = c(3,      4,       6,       6)
  )
  expect_equal(soilKey:::.dominant_b_color_subordem(ped_a, "P")$code, "PA")
})


test_that(".dominant_b_color_subordem maps Latossolos to LV/LA/LB/LVA", {
  ped <- .make_color_tuning_pedon(
    hues    = c("10YR", "7.5YR", "2.5YR", "2.5YR"),
    values  = c(4,      4,       4,       4),
    chromas = c(3,      4,       6,       6)
  )
  expect_equal(soilKey:::.dominant_b_color_subordem(ped, "L")$code, "LV")
})


test_that(".dominant_b_color_subordem returns NA for non-color Ordens", {
  ped <- .make_color_tuning_pedon()
  expect_true(is.na(soilKey:::.dominant_b_color_subordem(ped, "C")$code))
  expect_true(is.na(soilKey:::.dominant_b_color_subordem(ped, "M")$code))
  expect_true(is.na(soilKey:::.dominant_b_color_subordem(ped, "T")$code))
})


# ---- .apply_color_dominant_override --------------------------------------

test_that(".apply_color_dominant_override flips PA -> PV when red dominates", {
  rules <- soilKey::load_rules("sibcs5")
  ped <- .make_color_tuning_pedon(
    hues    = c("10YR", "7.5YR", "2.5YR", "2.5YR"),
    values  = c(4,      4,       4,       4),
    chromas = c(3,      4,       6,       6)
  )
  # Force a "PA" first-match-wins assignment by faking the YAML entry.
  fake_pa <- list(code = "PA", name = "Argissolos Amarelos",
                   tests = list(all_of = list(list(argissolo_amarelo = list()))))
  out <- soilKey:::.apply_color_dominant_override(fake_pa, ped, "P", rules)
  expect_equal(out$subordem$code, "PV")
  expect_equal(out$override$from_code, "PA")
  expect_equal(out$override$to_code,   "PV")
  expect_true(grepl("dominante", out$override$reason))
})


test_that(".apply_color_dominant_override is a no-op when codes match", {
  rules <- soilKey::load_rules("sibcs5")
  ped <- .make_color_tuning_pedon(
    hues    = c("10YR", "7.5YR", "2.5YR", "2.5YR"),
    values  = c(4,      4,       4,       4),
    chromas = c(3,      4,       6,       6)
  )
  fake_pv <- list(code = "PV", name = "Argissolos Vermelhos",
                   tests = list(all_of = list(list(argissolo_vermelho = list()))))
  out <- soilKey:::.apply_color_dominant_override(fake_pv, ped, "P", rules)
  expect_null(out$override)
  expect_equal(out$subordem$code, "PV")
})


test_that(".apply_color_dominant_override is a no-op for non-color Ordens", {
  rules <- soilKey::load_rules("sibcs5")
  ped <- .make_color_tuning_pedon()
  fake_cx <- list(code = "CX", name = "Cambissolos Haplicos",
                   tests = list(default = TRUE))
  out <- soilKey:::.apply_color_dominant_override(fake_cx, ped, "C", rules)
  expect_null(out$override)
  expect_equal(out$subordem$code, "CX")
})


test_that(".apply_color_dominant_override is a no-op when no Munsell B", {
  rules <- soilKey::load_rules("sibcs5")
  ped <- .make_color_tuning_pedon(
    hues    = c(NA, NA, NA, NA),
    values  = c(NA, NA, NA, NA),
    chromas = c(NA, NA, NA, NA)
  )
  fake_pva <- list(code = "PVA", name = "Argissolos Vermelho-Amarelos",
                    tests = list(default = TRUE))
  out <- soilKey:::.apply_color_dominant_override(fake_pva, ped, "P", rules)
  expect_null(out$override)
})


# ---- end-to-end: classify_sibcs() trace exposes override ------------------

test_that("classify_sibcs records color_dominant_override in trace", {
  ped <- .make_color_tuning_pedon(
    hues    = c("10YR", "7.5YR", "2.5YR", "2.5YR"),
    values  = c(4,      4,       4,       4),
    chromas = c(3,      4,       6,       6)
  )
  res <- classify_sibcs(ped, on_missing = "silent")
  expect_true("color_dominant_override" %in% names(res$trace))
  # The pedon has a Bt1 (10YR ~ AMARELO 35 cm) + Bt2 (2.5YR ~ VERMELHO 55 cm)
  # plus AB (7.5YR 4/4 BRUNO_ACINZENTADO 35 cm). Overall the THICKEST single
  # category is VERMELHO (55 cm) -- override should fire and the result
  # should carry "Argissolos Vermelhos".
  expect_match(res$name, "Argissolos Vermelhos|Argissolos")
})


test_that("classify_sibcs leaves Cambissolos untouched (no override)", {
  ped <- .make_color_tuning_pedon(
    ordem_first = "CAMBISSOLO",
    hues    = c("10YR", "7.5YR", "10YR",  "10YR"),
    values  = c(4,      4,       5,       5),
    chromas = c(3,      4,       4,       4),
    desg    = c("A",  "AB", "Bw1", "Bw2"),
    clay    = c(20, 28, 30, 32)
  )
  res <- classify_sibcs(ped, on_missing = "silent")
  expect_null(res$trace$color_dominant_override)
})
