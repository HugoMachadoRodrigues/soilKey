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

test_that("the seven example profiles that carried them are named without", {
  nm <- function(f) suppressWarnings(classify_wrb2022(f(), on_missing = "silent")$name)
  expect_identical(nm(make_andosol_canonical),
    "Vitric Silandic Hydric Andic Umbric Cambic Andosol (Loamic, Humic, Eutric, Brunic, Hydric)")
  expect_identical(nm(make_chernozem_canonical),
    "Vermic Pachic Chernic Protocalcic Cambic Chernozem (Loamic, Siltic, Humic, Hypereutric, Pachic, Calcaric)")
  expect_identical(nm(make_gypsisol_canonical),
    "Gypsic Protocalcic Cambic Gypsisol (Loamic, Ochric, Eutric, Calcaric)")
  expect_identical(nm(make_kastanozem_canonical),
    "Protocalcic Cambic Kastanozem (Loamic, Humic, Hypereutric, Calcaric)")
  expect_identical(nm(make_solonchak_canonical),
    "Sodic Solonchak (Loamic, Ochric, Hypereutric)")
  expect_identical(nm(make_solonetz_canonical),
    "Albic Solonetz (Loamic, Ochric, Hypereutric)")
})
