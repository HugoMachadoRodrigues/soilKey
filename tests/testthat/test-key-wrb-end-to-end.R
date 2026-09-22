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

# ----- v0.3 fixtures end-to-end -----

test_that("Histosol fixture -> Histosols", {
  expect_classifies_as(make_histosol_canonical(), "Histosols")
})

test_that("Anthrosol fixture -> Anthrosols", {
  expect_classifies_as(make_anthrosol_canonical(), "Anthrosols")
})

test_that("Technosol fixture -> Technosols", {
  expect_classifies_as(make_technosol_canonical(), "Technosols")
})

test_that("Cryosol fixture -> Cryosols", {
  expect_classifies_as(make_cryosol_canonical(), "Cryosols")
})

test_that("Leptosol fixture -> Leptosols", {
  expect_classifies_as(make_leptosol_canonical(), "Leptosols")
})

test_that("Solonetz fixture -> Solonetz", {
  expect_classifies_as(make_solonetz_canonical(), "Solonetz")
})

test_that("Andosol fixture -> Andosols", {
  expect_classifies_as(make_andosol_canonical(), "Andosols")
})

test_that("Nitisol fixture -> Nitisols", {
  expect_classifies_as(make_nitisol_canonical(), "Nitisols")
})

test_that("Planosol fixture -> Planosols", {
  expect_classifies_as(make_planosol_canonical(), "Planosols")
})

test_that("Stagnosol fixture -> Stagnosols", {
  expect_classifies_as(make_stagnosol_canonical(), "Stagnosols")
})

test_that("Umbrisol fixture -> Umbrisols", {
  expect_classifies_as(make_umbrisol_canonical(), "Umbrisols")
})

test_that("Durisol fixture -> Durisols", {
  expect_classifies_as(make_durisol_canonical(), "Durisols")
})

test_that("Retisol fixture -> Retisols", {
  expect_classifies_as(make_retisol_canonical(), "Retisols")
})

test_that("Arenosol fixture -> Arenosols", {
  expect_classifies_as(make_arenosol_canonical(), "Arenosols")
})

test_that("Fluvisol fixture -> Fluvisols", {
  expect_classifies_as(make_fluvisol_canonical(), "Fluvisols")
})

test_that("All 30 canonical fixtures classify to their intended RSG", {
  fixture_to_rsg <- list(
    list(make_ferralsol_canonical, "Ferralsols"),
    list(make_luvisol_canonical,   "Luvisols"),
    list(make_chernozem_canonical, "Chernozems"),
    list(make_calcisol_canonical,  "Calcisols"),
    list(make_gypsisol_canonical,  "Gypsisols"),
    list(make_solonchak_canonical, "Solonchaks"),
    list(make_cambisol_canonical,  "Cambisols"),
    list(make_plinthosol_canonical,"Plinthosols"),
    list(make_podzol_canonical,    "Podzols"),
    list(make_gleysol_canonical,   "Gleysols"),
    list(make_vertisol_canonical,  "Vertisols"),
    list(make_acrisol_canonical,   "Acrisols"),
    list(make_lixisol_canonical,   "Lixisols"),
    list(make_alisol_canonical,    "Alisols"),
    list(make_kastanozem_canonical,"Kastanozems"),
    list(make_phaeozem_canonical,  "Phaeozems"),
    list(make_histosol_canonical,  "Histosols"),
    list(make_anthrosol_canonical, "Anthrosols"),
    list(make_technosol_canonical, "Technosols"),
    list(make_cryosol_canonical,   "Cryosols"),
    list(make_leptosol_canonical,  "Leptosols"),
    list(make_solonetz_canonical,  "Solonetz"),
    list(make_andosol_canonical,   "Andosols"),
    list(make_nitisol_canonical,   "Nitisols"),
    list(make_planosol_canonical,  "Planosols"),
    list(make_stagnosol_canonical, "Stagnosols"),
    list(make_umbrisol_canonical,  "Umbrisols"),
    list(make_durisol_canonical,   "Durisols"),
    list(make_retisol_canonical,   "Retisols"),
    list(make_arenosol_canonical,  "Arenosols"),
    list(make_fluvisol_canonical,  "Fluvisols")
  )

  for (entry in fixture_to_rsg) {
    fix <- entry[[1]]()
    expected <- entry[[2]]
    res <- classify_wrb2022(fix, on_missing = "silent")
    if (!identical(res$rsg_or_order, expected)) {
      fail(sprintf("Fixture for %s landed at %s instead",
                    expected, res$rsg_or_order))
    } else {
      succeed()
    }
  }
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
