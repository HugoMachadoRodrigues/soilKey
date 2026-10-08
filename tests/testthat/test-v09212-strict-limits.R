# v0.9.212: CEC-per-clay limits are strict where the books write them strict.
#
# WRB 2022: ferralic horizon "< 16 cmolc kg-1 clay" (Ch 3.1.10, criterion 3);
# cohesic horizon, sideralic properties and the Acrisol / Lixisol argic
# "< 24", Alisol / Luvisol ">= 24". SiBCS 2018 (Cap 2): B latossolico "menor
# que 17 cmolc kg-1 de argila", B incipiente "17 ... ou maior". KST 13 (Ch 3):
# oxic horizon "16 cmol(+) or less per kg clay". soilKey accepted a value equal
# to the upper limit everywhere, so a horizon at exactly 16 was ferralic and
# one at exactly 24 was low-activity (Acrisol) and high-activity (Luvisol) at
# once.

# Set every horizon's CEC to `k` cmolc per kg clay.
.cec_at <- function(p, k) {
  h <- p$horizons
  h$cec_cmol <- h$clay_pct * k / 100
  PedonRecord$new(site = p$site, horizons = h)
}


test_that("the CEC-per-clay test passes below the limit, not at it", {
  h <- data.table::data.table(top_cm = c(0, 50, 100), bottom_cm = c(50, 100, 150),
                              clay_pct = 60, cec_cmol = 60 * c(15.99, 16, 16.01) / 100)
  r <- test_cec_per_clay(h, max_cmol_per_kg_clay = 16)
  expect_identical(r$layers, 1L)
  r <- test_cec_per_clay(h, max_cmol_per_kg_clay = 16, inclusive = TRUE)
  expect_identical(r$layers, 1:2)
  # floating-point noise in cec * 100 / clay does not cross the limit:
  # 2.32 * 100 / 14.5 is 15.999999999999998 and 1.12 * 100 / 7 is
  # 16.000000000000004 in double precision; both are 16.
  noisy <- data.table::data.table(top_cm = c(0, 50), bottom_cm = c(50, 100),
                                  clay_pct = c(14.5, 7), cec_cmol = c(2.32, 1.12))
  expect_length(test_cec_per_clay(noisy, 16)$layers, 0L)
  expect_identical(test_cec_per_clay(noisy, 16, inclusive = TRUE)$layers, 1:2)
  expect_identical(test_cec_per_clay_above(noisy, 16)$layers, 1:2)
})

test_that("a horizon at exactly 16 is oxic (USDA) but not ferralic (WRB)", {
  f <- make_ferralsol_canonical()
  below <- .cec_at(f, 15.9)
  at    <- .cec_at(f, 16)
  expect_true(ferralic(below)$passed)
  expect_true(oxic_usda(below)$passed)
  expect_false(ferralic(at)$passed)
  expect_true(oxic_usda(at)$passed)
  expect_false(oxic_usda(.cec_at(f, 16.1))$passed)
  expect_identical(classify_wrb2022(below, on_missing = "silent")$rsg_or_order, "Ferralsols")
  expect_false(identical(classify_wrb2022(at, on_missing = "silent")$rsg_or_order, "Ferralsols"))
  expect_identical(classify_usda(at, on_missing = "silent")$rsg_or_order, "Oxisols")
  # the argument can still be set by hand
  expect_true(ferralic(at, cec_inclusive = TRUE)$passed)
  expect_false(oxic_usda(at, cec_inclusive = FALSE)$passed)
})

test_that("SiBCS: 17 cmolc per kg clay is B incipiente territory, not latossolic", {
  f <- make_ferralsol_canonical()
  expect_true(B_latossolico(.cec_at(f, 16.9))$passed)
  expect_false(B_latossolico(.cec_at(f, 17))$passed)
  expect_identical(classify_sibcs(.cec_at(f, 16.9), on_missing = "silent")$rsg_or_order,
                   "Latossolos")
  expect_false(identical(classify_sibcs(.cec_at(f, 17), on_missing = "silent")$rsg_or_order,
                         "Latossolos"))
})

test_that("at exactly 24 an argic horizon is high-activity only", {
  a <- make_acrisol_canonical()
  at <- .cec_at(a, 24)
  expect_true(acrisol(.cec_at(a, 23.9))$passed)
  expect_false(acrisol(at)$passed)
  expect_false(lixisol(at)$passed)
  low  <- test_cec_per_clay(at$horizons, 24)
  high <- test_cec_per_clay_above(at$horizons, 24)
  expect_length(intersect(low$layers, high$layers), 0L)
  expect_false(identical(classify_wrb2022(at, on_missing = "silent")$rsg_or_order,
                         "Acrisols"))
})

test_that("sideralic properties use the strict 24 too", {
  p <- make_cambisol_canonical()
  ev <- function(k) sideralic_properties(.cec_at(p, k))$evidence$cec_per_clay
  expect_length(ev(24)$layers, 0L)
  expect_gt(length(ev(23.9)$layers), 0L)
})


# --- the Map's class colours -------------------------------------------------

test_that("the Map gives every class its own colour, and keeps Set3 up to 12", {
  skip_if_not_installed("leaflet")
  skip_if_not_installed("RColorBrewer")
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d)) d <- file.path("inst", "shiny", "classify_app_pro")
  e <- new.env(parent = globalenv())
  for (f in list.files(file.path(d, "R"), pattern = "\\.R$", full.names = TRUE))
    sys.source(f, envir = e)
  # 19 classes, as the SoilGrids overlay of Rio de Janeiro shows
  ids <- 1:19
  pal <- expect_no_warning(e$sk_class_pal(ids))
  cols <- pal(ids)
  expect_length(unique(cols), 19L)
  # up to 12 classes: the colours leaflet's "Set3" gave before
  ids <- 1:7
  expect_identical(e$sk_class_pal(ids)(ids),
                   leaflet::colorFactor("Set3", domain = ids)(ids))
  expect_identical(e$sk_class_pal(c("a", NA))(NA), "transparent")
})
