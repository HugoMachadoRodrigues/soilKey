# Tests for the v0.9.180 Uncertainty "group of points" mode: a per-point
# analysis over the batch of profiles shared from the Map tab via rv$batch_pedons.

.ung_app_dir <- function() {
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d)) d <- file.path("inst", "shiny", "classify_app_pro")
  d
}
.ung_source_modules <- function() {
  env <- new.env(parent = globalenv())
  for (f in list.files(file.path(.ung_app_dir(), "R"), pattern = "\\.R$",
                       full.names = TRUE))
    sys.source(f, envir = env)
  env
}


test_that("uncertainty group mode runs a per-point analysis over rv$batch_pedons", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  env               <- .ung_source_modules()
  uncertainty_server <- get("uncertainty_server", envir = env)
  # a small group of real fixtures (the Map batch shares these via rv)
  peds <- get(".batch_demo_pedons", envir = env)(4L)
  skip_if(!length(peds), "no demo pedons")

  rv <- shiny::reactiveValues(pedon = peds[[1]], batch_pedons = peds)
  settings <- shiny::reactive(list(on_missing = "silent"))

  shiny::testServer(uncertainty_server, args = list(rv = rv, settings = settings), {
    session$setInputs(source = "group", system = "wrb2022", level = "rsg",
                      n = 30, sensitivity = FALSE)
    expect_equal(n_group(), length(peds))
    session$setInputs(run = 1)
    g <- group_unc()
    expect_false(inherits(g, "error"))
    expect_equal(nrow(g), length(peds))               # one row per point
    expect_true(all(c("id", "top1", "prob", "entropy") %in% names(g)))
    ok <- g[is.finite(g$prob), , drop = FALSE]
    expect_true(nrow(ok) >= 1L)
    expect_true(all(ok$prob >= 0 & ok$prob <= 1))     # posterior is a probability
    expect_error(output$group_table, NA)              # the DT renders
  })
})


test_that("uncertainty group mode reports an empty group cleanly", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  env               <- .ung_source_modules()
  uncertainty_server <- get("uncertainty_server", envir = env)
  rv <- shiny::reactiveValues(pedon = NULL, batch_pedons = NULL)
  settings <- shiny::reactive(list(on_missing = "silent"))

  shiny::testServer(uncertainty_server, args = list(rv = rv, settings = settings), {
    session$setInputs(source = "group", system = "wrb2022", level = "rsg", n = 30)
    expect_equal(n_group(), 0L)
    expect_error(output$group_note, NA)               # the "no group" note renders
    session$setInputs(run = 1)
    expect_true(inherits(group_unc(), "error"))       # nothing to analyse
  })
})
