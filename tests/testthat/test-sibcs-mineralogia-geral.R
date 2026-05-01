# Tests for familia_mineralogia_argila_geral() -- SiBCS Cap 18
# extension that covers Argissolos / Cambissolos / Plintossolos /
# Vertissolos / Luvissolos / Nitossolos / Chernossolos / Planossolos
# / Gleissolos. Adds the four mineralogia da argila classes the
# Latossolo-only function did not address: esmectitica, caulinitica,
# oxidica, mista.


make_pedon_with_sulfuric <- function(ta = NA_real_, ki = NA_real_,
                                          kr = NA_real_) {
  # Synthesise SiO2 / Al2O3 / Fe2O3 sulfuric values that satisfy the
  # given ki and kr (when supplied). Choose Al2O3 = 100 g/100g for
  # convenient arithmetic.
  al2o3 <- if (is.na(ki) && is.na(kr)) NA_real_ else 30
  sio2  <- if (is.na(ki)) NA_real_ else ki * (al2o3 / 101.96) * 60.08
  fe2o3 <- if (is.na(kr) || is.na(sio2) || is.na(al2o3)) NA_real_ else {
    # kr = (SiO2 / 60.08) / (Al2O3/101.96 + Fe2O3/159.69)
    # solve for fe2o3 given ki, kr, al2o3
    rhs <- (sio2 / 60.08) / kr - (al2o3 / 101.96)
    if (rhs <= 0) NA_real_ else rhs * 159.69
  }
  # CEC + clay tuned so T_argila = ta when supplied.
  cec  <- if (is.na(ta)) NA_real_ else ta * 0.30  # clay_pct = 30, cec = ta * 0.3
  clay <- if (is.na(ta)) NA_real_ else 30
  hz <- data.table::data.table(
    top_cm    = c(0,    20),
    bottom_cm = c(20,  100),
    clay_pct                  = c(clay, clay),
    cec_cmol                  = c(cec, cec),
    sio2_sulfuric_pct         = c(sio2, sio2),
    al2o3_sulfuric_pct        = c(al2o3, al2o3),
    fe2o3_sulfuric_pct        = c(fe2o3, fe2o3)
  )
  PedonRecord$new(site = list(id = "sibcs-min-test"),
                    horizons = ensure_horizon_schema(hz))
}


test_that("familia_mineralogia_argila_geral assigns 'esmectitica' when T_argila >= 27", {
  pr <- make_pedon_with_sulfuric(ta = 35)
  res <- familia_mineralogia_argila_geral(pr)
  expect_equal(res$value, "esmectitica")
})


test_that("familia_mineralogia_argila_geral assigns 'oxidica' when Kr < 0.75", {
  pr <- make_pedon_with_sulfuric(ta = 10, ki = 0.5, kr = 0.4)
  res <- familia_mineralogia_argila_geral(pr)
  expect_equal(res$value, "oxidica")
})


test_that("familia_mineralogia_argila_geral assigns 'caulinitica' when Ki/Kr both >= 0.75 and T low", {
  pr <- make_pedon_with_sulfuric(ta = 12, ki = 1.5, kr = 1.0)
  res <- familia_mineralogia_argila_geral(pr)
  expect_equal(res$value, "caulinitica")
})


test_that("familia_mineralogia_argila_geral assigns 'mista' when no gate closes", {
  # Ki below the caulinitica threshold but Kr above the oxidica one;
  # T low. Falls into the catch-all 'mista' bucket.
  pr <- make_pedon_with_sulfuric(ta = 12, ki = 0.6, kr = 1.0)
  res <- familia_mineralogia_argila_geral(pr)
  expect_equal(res$value, "mista")
})


test_that("familia_mineralogia_argila_geral returns NULL + lists missing fields when nothing measured", {
  hz <- data.table::data.table(top_cm = c(0,30), bottom_cm = c(30,100),
                                  clay_pct = c(NA_real_, NA_real_))
  pr <- PedonRecord$new(site = list(id = "min-empty"),
                         horizons = ensure_horizon_schema(hz))
  res <- familia_mineralogia_argila_geral(pr)
  expect_null(res$value)
  expect_true(length(res$missing) >= 1L)
})
