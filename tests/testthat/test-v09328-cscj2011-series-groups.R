# v0.9.328: the 381 soil series groups of the Comprehensive Soil
# Classification System of Japan, First Approximation (CSCJ 2011, chapter
# VI). Made-up profiles; no Japanese data.

.jp <- function(h, site = list()) {
  n <- max(lengths(h))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  base <- list(id = "t", country = "JP", designation_system = "scsj", drainage_class = "well drained",
               land_use = "forest")
  PedonRecord$new(site = utils::modifyList(base, site), horizons = ensure_horizon_schema(
    data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}
.se <- function(p) {
  r <- classify_cscj2011(p, on_missing = "silent")
  c(code = r$levels$code[4], prov = paste(r$provisional_because, collapse = ","))
}

test_that("every example keys to a series group, decided; 381 series groups", {
  skip_on_cran()
  want <- c(organic = "B1LMTY", podzol = "C1HATY", andosol = "D6HAHH", dark_red = "E1ARTY", lowland = "F4HAME",
            red_yellow = "G1REFI", stagnic = "H2HAFI", brown_forest = "I1HAFI", regosol = "J2HATY")
  for (g in names(want)) {
    r <- classify_cscj2011(make_cscj2011_example(g), on_missing = "silent")
    expect_identical(r$levels$code[4], want[[g]], info = g)
    expect_length(r$provisional_because, 0)
  }
  # the Man-made soils have no series groups
  expect_identical(nrow(classify_cscj2011(make_cscj2011_example("man_made"), on_missing = "silent")$levels), 3L)
  r <- classify_cscj2011(make_cscj2011_example("red_yellow"), on_missing = "silent")
  expect_identical(r$name, "fine-textured Reddish Argic Red-Yellow soils")
  expect_identical(r$levels$name_native[4], "\u7d30\u7c92\u8cea\u8d64\u8272\u7c98\u571f\u96c6\u7a4d\u8d64\u9ec4\u8272\u571f")
  rules <- load_rules("cscj2011")
  expect_identical(sum(lengths(rules$series_groups)), 381L)
  codes <- unlist(lapply(rules$series_groups, function(g) vapply(g, `[[`, "", "code")))
  expect_false(anyDuplicated(codes) > 0)
})

test_that("lithic, skeletal (with or without rock), petoric", {
  skip_on_cran()
  b <- function() make_cscj2011_example("brown_forest")
  p <- b(); p$horizons$designation <- c("A", "B1", "B2", "R"); p$horizons$top_cm <- c(0, 15, 30, 50)
  p$horizons$bottom_cm <- c(15, 30, 50, 80)
  expect_identical(.se(p)[["code"]], "I1HALI")
  p <- b(); p$horizons$gravel_pct <- c(0, 0, 30, 0)
  expect_identical(.se(p)[["code"]], "I1HASK")
  # gravel not recorded: no gravel layer
  expect_identical(.se(b())[["code"]], "I1HAFI")
  # the Lowland lists: a gravel layer or the bedrock
  l <- make_cscj2011_example("lowland"); l$horizons$designation <- c("A", "C1", "R", "R")
  l$horizons$texture_class_jp <- c("SiL", "FSL", NA, NA)
  expect_identical(.se(l)[["code"]], "F4HASK")
  # pan layer: hardness 29 mm or more, 10 cm or more within 60 cm
  a <- make_cscj2011_example("andosol"); a$horizons$hardness_yamanaka_mm <- c(15, 18, 30, 25)
  r <- classify_cscj2011(a, on_missing = "silent")
  expect_true(petoric_cscj(a)$passed)
  a$horizons$hardness_yamanaka_mm <- NA
  expect_false(petoric_cscj(a)$passed)
})

test_that("texture of the subsurface: the class, clay alone, the representative horizon", {
  skip_on_cran()
  l <- function(tc, clay = c(16, 10, 6, 3)) { p <- make_cscj2011_example("lowland"); p$horizons$texture_class_jp <- tc
    p$horizons$clay_pct <- clay; .se(p) }
  expect_identical(l(c("SiL", "LiC", "LS", "S"))[["code"]], "F4HAFI")
  expect_identical(l(c("SiL", "LS", "LS", "S"))[["code"]], "F4HACO")
  expect_identical(l(c("SiL", "\u58cc\u571f", "LS", "S"))[["code"]], "F4HAME")       # the Japanese word
  # no class: clay 15 % or more is fine; less is medium or coarse, undecided
  expect_identical(l(NA, clay = c(16, 30, 6, 3))[["code"]], "F4HAFI")
  g <- l(NA)
  expect_match(g[["prov"]], "F4HACO")
  # Red-Yellow lists (medium-coarse, then fine): clay alone decides
  r <- make_cscj2011_example("red_yellow"); r$horizons$clay_pct <- c(10, 12, 12, 10)
  expect_identical(texture_cscj(r, "coarse_medium")$passed, TRUE)
})

test_that("Andosol, peat and Regosol terms; names lower case before the subgroup", {
  skip_on_cran()
  # peat: a mineral layer at the surface, fibric peat
  o <- make_cscj2011_example("organic")
  o$horizons$fiber_content_rubbed_pct <- c(80, 80, 80, NA)
  expect_identical(.se(o)[["code"]], "B1LMFB")
  o2 <- make_cscj2011_example("organic")
  o2$horizons$designation <- c("C", "H2", "H3", "G"); o2$horizons$oc_pct <- c(1, 30, 28, 2)
  expect_true(epimineralic_cscj(o2)$passed)
  # mollic: a base-rich dark surface horizon on a Calcaric Terrestrial Regosol
  h <- list(designation = c("A", "C"), top_cm = c(0, 25), bottom_cm = c(25, 100), oc_pct = c(2, 0.3),
            munsell_hue_moist = "10YR", munsell_value_moist = c(2, 5), munsell_chroma_moist = c(2, 3),
            bs_pct = c(80, 90), caco3_pct = c(5, 20), ph_h2o = 7.8, clay_pct = 15, field_texture = "loam",
            layer_origin = "colluvium", redoximorphic_features_pct = 0, structure_grade = c("weak", "structureless"))
  p <- .jp(h)
  expect_true(mollic_cscj(p)$passed)
  p$site$acid_neutralising_amendment <- TRUE
  expect_true(is.na(mollic_cscj(p)$passed))
  # the name: the term, lower case, before the subgroup's
  r <- classify_cscj2011(make_cscj2011_example("podzol"), on_missing = "silent")
  expect_identical(r$name, "typic Haplic Podzols")
})
