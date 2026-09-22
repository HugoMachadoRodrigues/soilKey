# Tests for the v0.9.105 automatic WRB depth-specifier attachment.
# Epi-/Endo-/Bathy-/Amphi-/Panto-/Kato- are computed from the feature's
# actual layers and prefixed to depth-anchored qualifiers, opt-in via
# classify_wrb2022(specifiers = TRUE). The default path stays byte-identical.

.spc_pedon <- function(top, bottom, designation = NULL) {
  hz <- data.frame(top_cm = top, bottom_cm = bottom)
  if (!is.null(designation)) hz$designation <- designation
  soilKey::PedonRecord$new(site = list(id = "spc"), horizons = hz)
}


test_that(".compute_depth_specifier maps a feature's bands to the right prefix", {
  skip_on_cran()
  cds <- soilKey:::.compute_depth_specifier
  expect_equal(cds(.spc_pedon(0,   40),  1L), "Epi")
  expect_equal(cds(.spc_pedon(60,  95),  1L), "Endo")
  expect_equal(cds(.spc_pedon(120, 160), 1L), "Bathy")
  # all three bands -> Panto
  expect_equal(cds(.spc_pedon(c(0, 60, 110), c(60, 110, 160)), 1:3), "Panto")
  # contiguous across 50 cm -> no specifier (canonical bare name)
  expect_equal(cds(.spc_pedon(c(0, 40), c(40, 80)), 1:2), "")
  # epi + endo with a mid gap -> Amphi
  expect_equal(cds(.spc_pedon(c(0, 60), c(30, 90)), 1:2), "Amphi")
  # endo + bathy -> Kato (lower part)
  expect_equal(cds(.spc_pedon(c(60, 110), c(100, 150)), 1:2), "Kato")
  # no layers -> ""
  expect_equal(cds(.spc_pedon(0, 40), integer(0)), "")
})


test_that(".apply_depth_specifiers renames only depth-specifiable qualifiers", {
  skip_on_cran()
  apply_sp <- soilKey:::.apply_depth_specifiers
  pr <- .spc_pedon(c(0, 30, 60), c(30, 60, 95))
  # Gleyic feature in layer 3 (60-95 -> Endo); Rhodic is not depth-specifiable.
  out <- apply_sp(pr, c("Gleyic", "Rhodic"),
                  list(Gleyic = 3L, Rhodic = 1L))
  expect_equal(out, c("Endogleyic", "Rhodic"))

  # A contiguous 0-80 feature gets no prefix.
  pr2 <- .spc_pedon(c(0, 40), c(40, 80))
  out2 <- apply_sp(pr2, "Calcic", list(Calcic = 1:2))
  expect_equal(out2, "Calcic")
})


test_that("epipedon / surface qualifiers are excluded from specifiers", {
  skip_on_cran()
  spc <- soilKey:::.WRB_DEPTH_SPECIFIABLE
  expect_true("Gleyic" %in% spc)
  expect_true("Calcic" %in% spc)
  expect_true("Spodic" %in% spc)
  for (epi in c("Mollic", "Umbric", "Chernic", "Histic", "Takyric", "Cryic"))
    expect_false(epi %in% spc)
})


test_that("resolve_wrb_qualifiers accepts specifiers and stays canonical by default", {
  skip_on_cran()
  skip_if_not_installed("yaml")
  pr <- make_ferralsol_canonical()
  base <- resolve_wrb_qualifiers(pr, "FR")
  off  <- resolve_wrb_qualifiers(pr, "FR", specifiers = FALSE)
  on   <- resolve_wrb_qualifiers(pr, "FR", specifiers = TRUE)
  # default == specifiers=FALSE, byte-identical name sets
  expect_identical(base$principal, off$principal)
  expect_identical(base$supplementary, off$supplementary)
  # specifiers=TRUE returns a valid (possibly identical) structure
  expect_type(on$principal, "character")
  expect_true(length(on$principal) >= 1L)
})


test_that("classify_wrb2022 default name is byte-identical across canonical fixtures", {
  skip_on_cran()
  fx <- grep("^make_.*_canonical$", ls(asNamespace("soilKey")), value = TRUE)
  expect_gt(length(fx), 20L)
  for (f in fx) {
    pr <- tryCatch(get(f, asNamespace("soilKey"))(), error = function(e) NULL)
    if (is.null(pr)) next
    n_default <- classify_wrb2022(pr)$name
    n_off     <- classify_wrb2022(pr, specifiers = FALSE)$name
    expect_identical(n_default, n_off)
  }
})
