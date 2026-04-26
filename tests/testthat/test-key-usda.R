test_that("load_rules reads the USDA rule set", {
  rules <- load_rules("usda")
  expect_equal(length(rules$orders), 12L)
  codes <- vapply(rules$orders, function(o) o$code, character(1))
  expect_equal(codes[5], "OX")             # Oxisols (5th order tested)
  expect_equal(codes[length(codes)], "EN") # Entisols catch-all
})

test_that("classify_usda assigns Oxisols to canonical Ferralsol fixture", {
  pr <- make_ferralsol_canonical()
  res <- classify_usda(pr, on_missing = "silent")
  expect_s3_class(res, "ClassificationResult")
  expect_equal(res$rsg_or_order, "Oxisols")
  expect_equal(res$system, "USDA Soil Taxonomy")
})

test_that("classify_usda assigns Entisols (catch-all) to non-oxic fixtures", {
  for (fix_fn in list(make_luvisol_canonical, make_chernozem_canonical,
                        make_solonchak_canonical, make_cambisol_canonical)) {
    res <- classify_usda(fix_fn(), on_missing = "silent")
    expect_equal(res$rsg_or_order, "Entisols")
    # Catch-all warning should mention v0.8
    expect_true(any(grepl("v0\\.8", res$warnings)))
  }
})

test_that("oxic_usda delegates faithfully to ferralic", {
  pr <- make_ferralsol_canonical()
  fer <- ferralic(pr)
  oxi <- oxic_usda(pr)
  expect_identical(fer$passed, oxi$passed)
  expect_identical(fer$layers, oxi$layers)
  expect_match(oxi$reference, "Soil Survey Staff")
})

test_that("argillic_usda delegates faithfully to argic", {
  pr <- make_luvisol_canonical()
  arg  <- argic(pr)
  argl <- argillic_usda(pr)
  expect_identical(arg$passed, argl$passed)
  expect_identical(arg$layers, argl$layers)
})
