# v0.9.213: a profile with no soil property has no evidence grade.
#
# Horizons with only depths and designations ran through every key and ended
# at its catch-all (Haplic Regosol, Neossolos Regoliticos, Typic Udorthents)
# by elimination, nothing in the class verified, and each result said
# "evidence grade A" because a pedon with no provenance recorded is read as
# measured. The grade is now NA, the result says why, and the app and reports
# show "no measured data" instead of a letter.

.depth_only <- function() {
  h <- data.table::data.table(top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 150),
                              designation = c("A", "Bt1", "Bt2"))
  PedonRecord$new(site = list(id = "depth-only", lat = -22.5, lon = -43.7,
                              country = "BR"), horizons = h)
}


test_that("a profile with only depths and designations gets no grade", {
  p <- .depth_only()
  for (r in list(classify_wrb2022(p, on_missing = "silent"),
                 classify_sibcs(p, on_missing = "silent"),
                 classify_usda(p, on_missing = "silent"))) {
    expect_true(is.na(r$evidence_grade), info = r$system)
    expect_true(any(grepl("No horizon carries a soil property", r$warnings)),
                info = r$system)
  }
  # still classified: designations and depths are kept as they are
  expect_identical(classify_wrb2022(p, on_missing = "silent")$rsg_or_order, "Regosols")
  # and the print says so instead of staying silent
  out <- cli::cli_fmt(print(classify_usda(p, on_missing = "silent")))
  expect_true(any(grepl("Evidence grade: none", out)))
})

test_that("one described or measured property is enough for a grade", {
  base <- .depth_only()
  for (col in list(list("munsell_hue_moist", "10YR"), list("clay_pct", 30),
                   list("structure_type", "granular"))) {
    h <- base$horizons
    h[[col[[1]]]][1] <- col[[2]]
    p <- PedonRecord$new(site = base$site, horizons = h)
    expect_identical(classify_wrb2022(p, on_missing = "silent")$evidence_grade, "A",
                     info = col[[1]])
  }
  # an empty string is not a property
  h <- base$horizons
  h$structure_type <- "  "
  p <- PedonRecord$new(site = base$site, horizons = h)
  expect_true(is.na(classify_wrb2022(p, on_missing = "silent")$evidence_grade))
})

test_that("profiles with properties keep their grades", {
  for (f in c("make_ferralsol_canonical", "make_luvisol_canonical",
              "make_acrisol_canonical")) {
    p <- get(f)()
    res <- classify_all(p, on_missing = "silent")
    for (s in c("wrb", "sibcs", "usda"))
      expect_false(is.na(res[[s]]$evidence_grade), info = paste(f, s))
  }
})

test_that("the HTML report prints 'none' for the missing grade", {
  r <- classify_wrb2022(.depth_only(), on_missing = "silent")
  f <- tempfile(fileext = ".html")
  on.exit(unlink(f), add = TRUE)
  report_html(list(r), file = f, pedon = .depth_only())
  html <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_match(html, "none (no soil property measured)", fixed = TRUE)
  expect_false(grepl(">NA<", html, fixed = TRUE))
})

test_that("the Pro app shows 'No measured data' and explains it", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d)) d <- file.path("inst", "shiny", "classify_app_pro")
  e <- new.env(parent = globalenv())
  for (f in list.files(file.path(d, "R"), pattern = "\\.R$", full.names = TRUE))
    sys.source(f, envir = e)
  expect_match(as.character(e$pro_grade_badge(NA)), "No measured data")
  expect_match(as.character(e$pro_grade_badge("B")), "Evidence B")
  r <- classify_wrb2022(.depth_only(), on_missing = "silent")
  card <- as.character(e$pro_result_card(r, "WRB 2022"))
  expect_match(card, "reached by elimination")
})
