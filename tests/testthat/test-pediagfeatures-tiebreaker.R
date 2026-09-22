# v0.9.21 NASIS pediagfeatures tie-breaker tests.
#
# Verifies the contract:
# (1) When canonical gate returns TRUE -- tie-breaker does not
#     override.
# (2) When canonical gate returns FALSE -- tie-breaker does not
#     override.
# (3) When canonical gate returns NA AND surveyor identifies the
#     diagnostic -- tie-breaker flips to TRUE with provenance.
# (4) When canonical gate returns NA AND surveyor does NOT identify
#     the diagnostic -- result stays NA.


test_that(".has_nasis_feature returns FALSE when slot is missing", {
  pr <- make_ferralsol_canonical()
  expect_false(soilKey:::.has_nasis_feature(pr, "Argillic"))
})


test_that(".has_nasis_feature matches case-insensitively when slot present", {
  pr <- make_ferralsol_canonical()
  pr$site$nasis_diagnostic_features <- c("Mollic epipedon",
                                              "Argillic horizon")
  expect_true(soilKey:::.has_nasis_feature(pr, "argillic"))
  expect_true(soilKey:::.has_nasis_feature(pr, "^Mollic"))
  expect_false(soilKey:::.has_nasis_feature(pr, "^Spodic"))
})


test_that(".apply_nasis_tiebreaker leaves TRUE results unchanged", {
  pr <- make_ferralsol_canonical()
  pr$site$nasis_diagnostic_features <- "Argillic horizon"
  res <- DiagnosticResult$new(name = "x", passed = TRUE,
                                 layers = 1L, evidence = list(),
                                 missing = character(0),
                                 reference = "test")
  out <- soilKey:::.apply_nasis_tiebreaker(res, pr,
                                              pattern       = "Argillic",
                                              feature_label = "Argillic")
  expect_identical(out$passed, TRUE)
  # No tie-breaker entry recorded.
  expect_null(out$evidence$nasis_tiebreaker)
})


test_that(".apply_nasis_tiebreaker leaves FALSE results unchanged", {
  pr <- make_ferralsol_canonical()
  pr$site$nasis_diagnostic_features <- "Argillic horizon"
  res <- DiagnosticResult$new(name = "x", passed = FALSE,
                                 layers = integer(0), evidence = list(),
                                 missing = character(0),
                                 reference = "test")
  out <- soilKey:::.apply_nasis_tiebreaker(res, pr,
                                              pattern       = "Argillic",
                                              feature_label = "Argillic")
  expect_identical(out$passed, FALSE)
  expect_null(out$evidence$nasis_tiebreaker)
})


test_that(".apply_nasis_tiebreaker flips NA to TRUE when surveyor confirms", {
  pr <- make_ferralsol_canonical()
  pr$site$nasis_diagnostic_features <- "Mollic epipedon"
  res <- DiagnosticResult$new(name = "x", passed = NA,
                                 layers = integer(0), evidence = list(),
                                 missing = "oc_pct",
                                 reference = "test")
  out <- soilKey:::.apply_nasis_tiebreaker(res, pr,
                                              pattern       = "Mollic",
                                              feature_label = "Mollic epipedon")
  expect_identical(out$passed, TRUE)
  expect_true(isTRUE(out$evidence$nasis_tiebreaker$triggered))
  expect_match(out$evidence$nasis_tiebreaker$source,
                 "v0\\.9\\.21|tiebreaker|NASIS|pediagfeatures",
                 ignore.case = TRUE)
})


test_that(".apply_nasis_tiebreaker leaves NA unchanged when surveyor silent", {
  pr <- make_ferralsol_canonical()
  pr$site$nasis_diagnostic_features <- "Cambic horizon"
  res <- DiagnosticResult$new(name = "x", passed = NA,
                                 layers = integer(0), evidence = list(),
                                 missing = "oc_pct",
                                 reference = "test")
  out <- soilKey:::.apply_nasis_tiebreaker(res, pr,
                                              pattern       = "^Mollic",
                                              feature_label = "Mollic")
  expect_true(is.na(out$passed))
  expect_null(out$evidence$nasis_tiebreaker)
})
