# =============================================================================
# Tests for v0.9.45 -- SiBCS Argissolo/Latossolo/Nitossolo/Luvissolo
# graceful "cor a determinar" fallback when Munsell hue is missing in B.
#
# When the catch-all subordem code (PVA, LVA, NX, TX) is the assigned
# subordem AND at least one earlier predicate failed with missing =
# "munsell_hue_moist", classify_sibcs() should:
#   - Stop the descent at the Ordem level (no GG / SG)
#   - Surface "munsell_hue_moist_horizon_B" in missing_data
#   - Downgrade evidence_grade to at most "C"
#   - Append a Portuguese warning explaining the fallback
#   - Set trace$color_undetermined to a structured record
# =============================================================================

# ---- Fixtures specific to v0.9.45 ----------------------------------------

.make_argissolo_no_hue <- function() {
  pr <- make_argissolo_canonical()
  pr$horizons$munsell_hue_moist <- NA_character_
  pr
}

.make_argissolo_red <- function() {
  # Argissolo canonico ja tem matiz "2.5YR" / "10R" (vermelho) -- usado
  # como controle: classificacao DEVE descer normalmente, sem fallback.
  make_argissolo_canonical()
}


# ---- Argissolo without hue triggers the fallback -------------------------

test_that("classify_sibcs flags 'cor a determinar' when Argissolo hue is NA", {
  pr <- .make_argissolo_no_hue()
  res <- classify_sibcs(pr, on_missing = "silent")

  expect_s3_class(res, "ClassificationResult")
  expect_equal(res$rsg_or_order, "Argissolos")
  expect_match(res$name, "cor a determinar", ignore.case = TRUE)
  expect_true("munsell_hue_moist_horizon_B" %in% res$missing_data)
  expect_equal(res$evidence_grade, "C")
  expect_true(any(grepl("matiz Munsell", res$warnings, ignore.case = TRUE)))

  fb <- res$trace$color_undetermined
  expect_true(isTRUE(fb$detected))
  expect_equal(fb$missing_attribute, "munsell_hue_moist_horizon_B")
  expect_equal(fb$horizon_target, "B")
  expect_equal(fb$fallback_subordem$code, "PVA")
  expect_s3_class(fb$rejected_alternatives, "data.frame")
  expect_true(nrow(fb$rejected_alternatives) >= 1L)
})


# ---- Argissolo WITH hue keeps the regular descent ------------------------

test_that("classify_sibcs does NOT flag fallback when hue is present", {
  pr <- .make_argissolo_red()
  res <- classify_sibcs(pr, on_missing = "silent")

  expect_equal(res$rsg_or_order, "Argissolos")
  expect_false(grepl("cor a determinar", res$name, ignore.case = TRUE))
  expect_null(res$trace$color_undetermined)
})


# ---- Fallback stops the descent: no GG and no SG -------------------------

test_that("color-undetermined fallback stops descent at Ordem level", {
  pr <- .make_argissolo_no_hue()
  res <- classify_sibcs(pr, on_missing = "silent")

  expect_null(res$trace$grande_grupo_assigned)
  expect_null(res$trace$subgrupo_assigned)
})


# ---- Latossolo without hue triggers the same fallback --------------------

test_that("Latossolo also flags 'cor a determinar' when hue is NA", {
  pr <- make_ferralsol_canonical()
  pr$horizons$munsell_hue_moist <- NA_character_
  res <- classify_sibcs(pr, on_missing = "silent")

  if (!identical(res$rsg_or_order, "Latossolos")) {
    skip("Pedon was not classified as Latossolos -- skip color test")
  }
  expect_match(res$name, "cor a determinar", ignore.case = TRUE)
  expect_equal(res$evidence_grade, "C")
  expect_equal(res$trace$color_undetermined$fallback_subordem$code, "LVA")
})


# ---- Pedon whose subordem is NOT a color catch-all is unaffected --------

test_that("Non-color-catch-all subordens are unaffected by v0.9.45", {
  pr <- make_ferralsol_canonical()  # full hue present -- LV, not LVA
  res <- classify_sibcs(pr, on_missing = "silent")
  expect_null(res$trace$color_undetermined)
})


# ---- Internal helper exposes the canonical catch-all list ---------------

test_that("color catch-all codes constant covers PVA/LVA/NX/TX", {
  expect_setequal(soilKey:::.SIBCS_COLOR_CATCH_ALL_CODES,
                   c("PVA", "LVA", "NX", "TX"))
})


# ---- Helper returns NULL when subordem is NULL --------------------------

test_that(".detect_color_undetermined_fallback returns NULL on NULL subordem", {
  out <- soilKey:::.detect_color_undetermined_fallback(
    list(trace = list()), NULL
  )
  expect_null(out)
})


# ---- Helper returns NULL when no earlier predicate is hue-blocked -------

test_that(".detect_color_undetermined_fallback returns NULL when no hue-block", {
  fake_trace <- list(
    PV = list(code = "PV", name = "Argissolos Vermelhos",
                passed = FALSE, missing = "atividade_argila"),
    PVA = list(code = "PVA", name = "Argissolos Vermelho-Amarelos",
                passed = TRUE)
  )
  fake_sub <- list(trace = fake_trace)
  fake_assigned <- list(code = "PVA", name = "Argissolos Vermelho-Amarelos")
  out <- soilKey:::.detect_color_undetermined_fallback(fake_sub, fake_assigned)
  expect_null(out)
})


# ---- Warning is appended to ClassificationResult$warnings ---------------

test_that("Color-undetermined fallback appends a Portuguese warning", {
  pr <- .make_argissolo_no_hue()
  res <- classify_sibcs(pr, on_missing = "silent")
  expect_true(length(res$warnings) >= 1L)
  expect_true(any(grepl("matiz", res$warnings, ignore.case = TRUE)))
  expect_true(any(grepl("Vermelho", res$warnings)))
})
