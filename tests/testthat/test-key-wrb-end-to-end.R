# End-to-end key tests: every canonical fixture should classify to its
# intended RSG when run through classify_wrb2022 against the v0.2
# wired key.yaml.

expect_classifies_as <- function(fixture, expected_rsg) {
  res <- classify_wrb2022(fixture, on_missing = "silent")
  if (!identical(res$rsg_or_order, expected_rsg)) {
    fail(sprintf("Expected %s, got %s. Trace tail: %s",
                  expected_rsg, res$rsg_or_order,
                  res$trace[[length(res$trace)]]$code))
  } else {
    succeed()
  }
}

test_that("Ferralsol fixture -> Ferralsols", {
  expect_classifies_as(make_ferralsol_canonical(), "Ferralsols")
})

test_that("Vertisol fixture -> Vertisols", {
  expect_classifies_as(make_vertisol_canonical(), "Vertisols")
})

test_that("Solonchak fixture -> Solonchaks", {
  expect_classifies_as(make_solonchak_canonical(), "Solonchaks")
})

test_that("Gleysol fixture -> Gleysols", {
  expect_classifies_as(make_gleysol_canonical(), "Gleysols")
})

test_that("Podzol fixture -> Podzols", {
  expect_classifies_as(make_podzol_canonical(), "Podzols")
})

test_that("Plinthosol fixture -> Plinthosols", {
  expect_classifies_as(make_plinthosol_canonical(), "Plinthosols")
})

test_that("Chernozem fixture -> Chernozems", {
  expect_classifies_as(make_chernozem_canonical(), "Chernozems")
})

test_that("Kastanozem fixture -> Kastanozems", {
  expect_classifies_as(make_kastanozem_canonical(), "Kastanozems")
})

test_that("Phaeozem fixture -> Phaeozems", {
  expect_classifies_as(make_phaeozem_canonical(), "Phaeozems")
})

test_that("Gypsisol fixture -> Gypsisols", {
  expect_classifies_as(make_gypsisol_canonical(), "Gypsisols")
})

test_that("Calcisol fixture -> Calcisols", {
  expect_classifies_as(make_calcisol_canonical(), "Calcisols")
})

test_that("Acrisol fixture -> Acrisols", {
  expect_classifies_as(make_acrisol_canonical(), "Acrisols")
})

test_that("Lixisol fixture -> Lixisols", {
  expect_classifies_as(make_lixisol_canonical(), "Lixisols")
})

test_that("Alisol fixture -> Alisols", {
  expect_classifies_as(make_alisol_canonical(), "Alisols")
})

test_that("Luvisol fixture -> Luvisols", {
  expect_classifies_as(make_luvisol_canonical(), "Luvisols")
})

test_that("Cambisol fixture -> Cambisols", {
  expect_classifies_as(make_cambisol_canonical(), "Cambisols")
})

test_that("Evidence grade is A for all canonical fixtures (no provenance log)", {
  fixtures <- list(
    make_ferralsol_canonical(),  make_luvisol_canonical(),
    make_chernozem_canonical(),  make_calcisol_canonical(),
    make_gypsisol_canonical(),   make_solonchak_canonical(),
    make_cambisol_canonical(),   make_plinthosol_canonical(),
    make_podzol_canonical(),     make_gleysol_canonical(),
    make_vertisol_canonical(),   make_acrisol_canonical(),
    make_lixisol_canonical(),    make_alisol_canonical(),
    make_kastanozem_canonical(), make_phaeozem_canonical()
  )
  for (f in fixtures) {
    res <- classify_wrb2022(f, on_missing = "silent")
    expect_equal(res$evidence_grade, "A")
  }
})

test_that("Trace length grows with how deep the assignment is in the key", {
  # Vertisols (VR) is position 7 in the canonical key
  vr <- classify_wrb2022(make_vertisol_canonical(), on_missing = "silent")
  expect_equal(vr$trace[[length(vr$trace)]]$code, "VR")

  # Cambisols (CM) is position 29
  cm <- classify_wrb2022(make_cambisol_canonical(), on_missing = "silent")
  expect_equal(cm$trace[[length(cm$trace)]]$code, "CM")

  expect_gt(length(cm$trace), length(vr$trace))
})
