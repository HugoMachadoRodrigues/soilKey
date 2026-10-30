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
  # v0.9.321: depths from the mineral soil surface, the top of the first
  # layer, so these pedons start at 0 cm and the feature is given by its rows
  expect_equal(cds(.spc_pedon(c(0, 60), c(60, 95)), 2L), "Endo")
  # v0.9.321: WRB 2022 Ch 2.3.1 rule 2 (p. 27). A layer starting >= 50 cm is
  # Endo- at any depth (the text's Endospodic starts >= 50 and <= 200 cm); it
  # was "Bathy", which rule 6 keeps for criteria met only below 100 cm.
  expect_equal(cds(.spc_pedon(c(0, 120), c(120, 160)), 2L), "Endo")
  # from the surface to >= 100 cm -> Panto
  expect_equal(cds(.spc_pedon(c(0, 60, 110), c(60, 110, 160)), 1:3), "Panto")
  # from the surface to > 50 and < 100 cm -> Ano- (was "", no Ano- before)
  expect_equal(cds(.spc_pedon(c(0, 40), c(40, 80)), 1:2), "Ano")
  # two layers with a gap between -> Poly- (was "Amphi"; rule 2 Amphi- is one
  # layer from > 0 to between 50 and 100 cm)
  expect_equal(cds(.spc_pedon(c(0, 60), c(30, 90)), 1:2), "Poly")
  expect_equal(cds(.spc_pedon(c(0, 10, 40), c(10, 40, 70)), 2:3), "Amphi")
  # starting >= 50 cm with a deeper layer -> Endo- (was "Kato"; rule 2 Kato- is
  # one layer from > 0 and < 50 cm to >= 100 cm)
  expect_equal(cds(.spc_pedon(c(0, 60, 100, 110), c(60, 100, 110, 150)), c(2L, 4L)), "Endo")
  expect_equal(cds(.spc_pedon(c(0, 10, 60), c(10, 60, 120)), 2:3), "Kato")
  # no layers -> ""
  expect_equal(cds(.spc_pedon(0, 40), integer(0)), "")
})


test_that(".apply_depth_specifiers renames only depth-specifiable qualifiers", {
  skip_on_cran()
  apply_sp <- soilKey:::.apply_depth_specifiers
  # v0.9.321: described to 100 cm (Ch 2.3.1, p. 26: depth subqualifiers "are
  # only used if the relevant soil characteristics are reported until
  # >= 100 cm"); this pedon stopped at 95 cm before.
  pr <- .spc_pedon(c(0, 30, 60), c(30, 60, 100))
  # Gleyic feature in layer 3 (60-100 -> Endo); Rhodic is not depth-specifiable.
  out <- apply_sp(pr, c("Gleyic", "Rhodic"),
                  list(Gleyic = 3L, Rhodic = 1L))
  expect_equal(out, c("Endogleyic", "Rhodic"))

  # A 0-80 feature in a soil described to 80 cm only gets no prefix (it would
  # be Ano- described to 100 cm).
  pr2 <- .spc_pedon(c(0, 40), c(40, 80))
  out2 <- apply_sp(pr2, "Calcic", list(Calcic = 1:2))
  expect_equal(out2, "Calcic")
  # Cambic and Retic take no specifier: no figure in Chapter 5 (p. 127)
  expect_equal(apply_sp(pr, "Cambic", list(Cambic = 3L)), "Cambic")
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
