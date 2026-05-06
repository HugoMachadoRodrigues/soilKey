# =============================================================================
# Tests for v0.9.65 -- agent_app() Shiny launcher + app.R parseability.
# =============================================================================


# ---- Launcher availability + dependency-check error path -----------------

test_that("run_agent_app is exported", {
  expect_true(exists("run_agent_app", envir = asNamespace("soilKey"),
                       inherits = FALSE))
  expect_true("run_agent_app" %in% ls("package:soilKey"))
})


test_that("run_agent_app errors clearly when Shiny / bslib / DT absent", {
  # We can't actually unload Shiny mid-session; we check the error message
  # template is correct by inspecting the function body.
  fn_body <- deparse(body(run_agent_app))
  expect_true(any(grepl("Packages required for run_agent_app", fn_body)))
  expect_true(any(grepl("bslib", fn_body)))
  expect_true(any(grepl("bsicons", fn_body)))
  expect_true(any(grepl("DT", fn_body)))
})


# ---- App.R syntactic + structural sanity ---------------------------------

test_that("agent_app/app.R is syntactically parseable", {
  app_dir <- system.file("shiny", "agent_app", package = "soilKey")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    app_dir <- file.path("inst", "shiny", "agent_app")
  }
  app_file <- file.path(app_dir, "app.R")
  skip_if_not(file.exists(app_file), "agent_app/app.R missing -- pre-install state.")
  expect_silent(parse(app_file))
})


test_that("agent_app/app.R wires the canonical UI sections", {
  app_dir <- system.file("shiny", "agent_app", package = "soilKey")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    app_dir <- file.path("inst", "shiny", "agent_app")
  }
  app_file <- file.path(app_dir, "app.R")
  skip_if_not(file.exists(app_file), "agent_app/app.R missing.")
  txt <- paste(readLines(app_file, warn = FALSE), collapse = "\n")
  # Must include all 8 nav_panels we documented.
  for (label in c("Foto Munsell", "PDF / Texto", "Ficha de Campo",
                    "Espectros", "Tabela de horizontes", "Classificar",
                    "Trace", "Pergunte ao Pedometrista")) {
    expect_true(grepl(label, txt, fixed = TRUE),
                  info = sprintf("Missing nav_panel '%s' in agent_app/app.R", label))
  }
  # And the bslib + bsicons + soilKey libraries.
  expect_true(grepl("library(bslib)", txt, fixed = TRUE))
  expect_true(grepl("library(bsicons)", txt, fixed = TRUE))
  expect_true(grepl("library(soilKey)", txt, fixed = TRUE))
  # Setup button + classify button + chat handler exist.
  expect_true(grepl("setup_vlm",       txt, fixed = TRUE))
  expect_true(grepl("classify",        txt, fixed = TRUE))
  expect_true(grepl("chat_send",       txt, fixed = TRUE))
})


# ---- Dependency wiring: pedologist persona is callable from app -----------

test_that("agent_app references pedologist_system_prompt for persona", {
  app_dir <- system.file("shiny", "agent_app", package = "soilKey")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    app_dir <- file.path("inst", "shiny", "agent_app")
  }
  app_file <- file.path(app_dir, "app.R")
  skip_if_not(file.exists(app_file), "agent_app/app.R missing.")
  txt <- paste(readLines(app_file, warn = FALSE), collapse = "\n")
  expect_true(grepl("pedologist_system_prompt", txt, fixed = TRUE))
})
