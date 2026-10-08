# v0.9.216: no qualifier that WRB 2022 does not have.
#
# soilKey's WRB 2022 qualifier lists carried ten names that do not occur
# anywhere in the WRB 2022 text (IUSS Working Group WRB 2022, 4th edition, as
# corrected 18 December 2022). They are WRB 2014 qualifiers: Vetic (replaced by
# Geric, ECEC < 6 cmolc kg-1 clay), Melanic, Cumulic, Hyperalbic,
# Hyperskeletic, Hypersodic, Hypocalcic, Hypogypsic, Hyposalic and Hyposodic.
# A profile could be named with a qualifier the system it claims to follow
# does not define. They are removed from every list.

.not_wrb2022 <- c("Vetic", "Melanic", "Cumulic", "Hyperalbic", "Hyperskeletic",
                  "Hypersodic", "Hypocalcic", "Hypogypsic", "Hyposalic",
                  "Hyposodic")

test_that("no WRB 2022 list names a qualifier WRB 2022 does not have", {
  qfile <- system.file("rules/wrb2022/qualifiers.yaml", package = "soilKey")
  if (!nzchar(qfile)) qfile <- "inst/rules/wrb2022/qualifiers.yaml"
  q <- yaml::read_yaml(qfile)$rsg_qualifiers
  expect_length(q, 32L)
  used <- unique(unlist(q))
  expect_length(intersect(used, .not_wrb2022), 0L)
})

test_that("no example profile is named with one of them", {
  fx <- grep("^make_.*_canonical$", ls(asNamespace("soilKey")), value = TRUE)
  names_ <- vapply(fx, function(f) {
    p <- get(f, envir = asNamespace("soilKey"))()
    suppressWarnings(classify_wrb2022(p, on_missing = "silent")$name)
  }, character(1))
  for (n in .not_wrb2022)
    expect_false(any(grepl(paste0("\\b", n, "\\b"), names_)), info = n)
})

test_that("the seven example profiles that carried them keep their RSG", {
  # Their full names are pinned in test-v09217-wrb2022-ch4-names.R, after the
  # realignment of v0.9.217 changed them again.
  rsg <- function(f) suppressWarnings(classify_wrb2022(f(), on_missing = "silent")$rsg_or_order)
  expect_identical(rsg(make_andosol_canonical), "Andosols")
  expect_identical(rsg(make_chernozem_canonical), "Chernozems")
  expect_identical(rsg(make_gypsisol_canonical), "Gypsisols")
  expect_identical(rsg(make_kastanozem_canonical), "Kastanozems")
  expect_identical(rsg(make_solonchak_canonical), "Solonchaks")
  expect_identical(rsg(make_solonetz_canonical), "Solonetz")
})


# ---- the Assistant is told the units and the direction of each limit -------

test_that("the Assistant's evidence gives units and whether a limit is inclusive", {
  skip_if_not_installed("shiny")
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d)) d <- file.path("inst", "shiny", "classify_app_pro")
  e <- new.env(parent = globalenv())
  for (f in list.files(file.path(d, "R"), pattern = "\\.R$", full.names = TRUE))
    sys.source(f, envir = e)
  ctx <- e$.chat_pedon_context(make_ferralsol_canonical(), NULL)
  ev <- strsplit(paste(unlist(ctx[c("text", "evidence")]), collapse = "\n"), "\n")[[1]]
  cpc <- grep("cec_per_clay: ", ev, value = TRUE)
  # the model wrote "cmolc/kg per % clay" when the unit was missing
  expect_true(all(grepl("cmolc/kg clay", cpc)))
  # WRB's ferralic "< 16" and USDA's oxic "16 or less" no longer read alike
  expect_true(any(grepl("must be below 16", cpc)))
  expect_true(any(grepl("must be 16 or less", cpc)))
  expect_true(any(grepl("thickness: .* cm \\(limit 30\\)", ev)))
})
