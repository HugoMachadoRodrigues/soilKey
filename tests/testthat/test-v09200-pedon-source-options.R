# The Pedon tab's source selector.
#
# v0.9.199 removed the bundled WoSIS profiles; v0.9.200 then hid the WoSIS
# button when none were present, because an option that opened an empty table
# was worse than no option. v0.9.202 makes the picker query ISRIC live, so it no
# longer depends on a bundled corpus at all and the button is always offered --
# what is conditional now is which PROFILES appear, and that is decided by
# licence, not by whether a file shipped. See test-v09202.

.src_env <- function() {
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d)) d <- file.path("inst", "shiny", "classify_app_pro")
  e <- new.env(parent = globalenv())
  for (f in list.files(file.path(d, "R"), pattern = "\\.R$", full.names = TRUE))
    sys.source(f, envir = e)
  e
}


test_that("all four sources are offered, including the live WoSIS picker", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  s <- .src_env()$.sk_pedon_sources()
  expect_setequal(unname(s), c("fixture", "wosis", "upload", "blank"))
})

test_that("labels stay aligned with the ids they name", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  # A mis-built selector would relabel every button while the app still ran,
  # which is the kind of bug that survives review.
  s <- .src_env()$.sk_pedon_sources()
  expect_equal(length(names(s)), length(s))
  expect_true(all(nzchar(names(s))))
  expect_false(any(duplicated(names(s))))
  expect_false(any(duplicated(unname(s))))
})

test_that("fixture remains available, since the selector opens on it", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  # The radio group defaults to "fixture"; if it stopped being offered the app
  # would open on a selection that does not exist.
  expect_true("fixture" %in% .src_env()$.sk_pedon_sources())
})

test_that("the WoSIS panel exposes the licence switch, not a bare table", {
  skip_on_cran()
  skip_if_not_installed("shiny"); skip_if_not_installed("bslib")
  skip_if_not_installed("DT");    skip_if_not_installed("shinyWidgets")
  # The filter is the reason the picker is allowed to exist at all, so its
  # control has to be present in the UI, not just in the R function.
  h <- as.character(.src_env()$pedon_ui("p"))
  for (id in c("p-wosis_country", "p-wosis_permissive", "p-wosis_search",
               "p-wosis_table"))
    expect_true(grepl(id, h, fixed = TRUE))
})
