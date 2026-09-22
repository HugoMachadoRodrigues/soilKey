# Tests for the v0.9.176 "Talk to soilKey Pro" chat module (mod_chat.R).
#
# The live Groq path needs a network + key, so it is never exercised here; we
# test the deterministic parts: key resolution, the grounded context, the
# scripted (no-key) fallback, and the photo -> Munsell fold-in via the offline
# MockVLMProvider.

.chat_app_dir <- function() {
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d)) d <- file.path("inst", "shiny", "classify_app_pro")
  d
}
.chat_source_modules <- function() {
  env <- new.env(parent = globalenv())
  for (f in list.files(file.path(.chat_app_dir(), "R"), pattern = "\\.R$",
                       full.names = TRUE))
    sys.source(f, envir = env)
  env
}
.chat_demo_pedon <- function() {
  soilKey::PedonRecord$new(
    site = list(id = "chat-test", lat = -22.5, lon = -43.7, crs = 4326),
    horizons = data.frame(
      designation = c("A", "Bo", "BC"),
      top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 120),
      clay_pct = c(55, 62, 60), cec_cmol = c(4, 3, 3), bs_pct = c(40, 35, 30)))
}


test_that("mod_chat.R ships and parses", {
  skip_on_cran()
  expect_true(file.exists(file.path(.chat_app_dir(), "R", "mod_chat.R")))
  expect_silent(parse(file.path(.chat_app_dir(), "R", "mod_chat.R")))
})


test_that(".chat_groq_key() prefers the field, else GROQ_API_KEY", {
  skip_on_cran()
  env <- .chat_source_modules()
  key_fn <- get(".chat_groq_key", envir = env)
  withr::with_envvar(c(GROQ_API_KEY = "from-env"), {
    expect_identical(key_fn("from-field"), "from-field")
    expect_identical(key_fn(""), "from-env")
    expect_identical(key_fn(NULL), "from-env")
  })
  withr::with_envvar(c(GROQ_API_KEY = ""), {
    expect_identical(key_fn(""), "")
    expect_identical(key_fn("x"), "x")
  })
})


test_that(".chat_make_groq() returns NULL without a key (no network)", {
  skip_on_cran()
  env <- .chat_source_modules()
  mk <- get(".chat_make_groq", envir = env)
  expect_null(mk("", "llama-3.3-70b-versatile", "sys"))
})


test_that(".chat_pedon_context() grounds in the deterministic classification", {
  skip_on_cran()
  env <- .chat_source_modules()
  ctx_fn <- get(".chat_pedon_context", envir = env)

  expect_null(ctx_fn(NULL))
  ctx <- ctx_fn(.chat_demo_pedon(), NULL)
  expect_true(is.list(ctx))
  expect_true(nzchar(ctx$text))
  expect_true(grepl("WRB", ctx$text))               # names the WRB result
  expect_true(grepl("Horizons", ctx$text))          # lists the horizons
  expect_true(!is.null(ctx$results$wrb))            # carries classify_all output
})


test_that(".chat_scripted_reply() answers from the pedon context", {
  skip_on_cran()
  env <- .chat_source_modules()
  ctx_fn    <- get(".chat_pedon_context", envir = env)
  reply_fn  <- get(".chat_scripted_reply", envir = env)
  # i18n() is used inside the scripted replies; source needs the app helper
  i18n <- get("i18n", envir = env)

  # no pedon -> a helpful "build a profile first" message, not a crash
  expect_true(nzchar(reply_fn("hi", NULL)))

  ctx <- ctx_fn(.chat_demo_pedon(), NULL)
  wrb_ans <- reply_fn("what is the WRB class?", ctx)
  expect_true(grepl("WRB", wrb_ans))
  # a colour question routes to the photo hint
  expect_true(nzchar(reply_fn("what munsell colour?", ctx)))
})


test_that("chat_server: no-key send yields a scripted, grounded reply", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  env         <- .chat_source_modules()
  chat_server <- get("chat_server", envir = env)
  rv          <- shiny::reactiveValues(pedon = .chat_demo_pedon())
  settings    <- shiny::reactive(list(on_missing = "silent"))

  withr::with_envvar(c(GROQ_API_KEY = ""), {
    shiny::testServer(chat_server, args = list(rv = rv, settings = settings), {
      session$setInputs(groq_key = "")
      session$setInputs(msg = "Tell me about the WRB classification.")
      session$setInputs(send = 1)
      h <- history()
      expect_equal(length(h), 2L)                 # user + assistant
      expect_identical(h[[1]]$role, "user")
      expect_identical(h[[2]]$role, "assistant")
      expect_true(grepl("WRB|Ferralsol|Reference", h[[2]]$text))
    })
  })
})


test_that("chat_ui builds the drawer content (no key field, no photo upload)", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  env <- .chat_source_modules()
  ui  <- get("chat_ui", envir = env)("chat")
  html <- as.character(ui)
  # the transcript + composer are present...
  expect_true(grepl("chat-log", html))
  expect_true(grepl("chat-msg|chat-composer|-msg", html))
  # ...but the removed controls are gone (v0.9.179: drawer, no key/photo)
  expect_false(grepl("groq_key", html))
  expect_false(grepl("chat-photo|read_munsell", html))
})
