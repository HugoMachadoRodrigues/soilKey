# v0.9.207: the Assistant explains from the evidence the keys used.
#
# Until v0.9.206 the live model was given the three class names and a list of
# horizon depths, nothing else. Asked why a profile is a Ferralsol and not an
# Acrisol, models made up "redoximorphic features" the profile did not have and
# "higher-activity clays" for Acrisols. The answer was in the key all along:
# Ferralsols come before Acrisols, so Acrisols are never tested. These tests pin
# what the model is now given (the horizon data, each key's trace, the criteria
# with their values and limits, the qualifier rules) and how a question is
# handed on when a model is at Groq's per-minute limit.

.grounding_env <- function() {
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d)) d <- file.path("inst", "shiny", "classify_app_pro")
  e <- new.env(parent = globalenv())
  for (f in list.files(file.path(d, "R"), pattern = "\\.R$", full.names = TRUE))
    sys.source(f, envir = e)
  e
}

# the evidence lines that contain `pattern`
.ev_lines <- function(ctx, pattern) {
  l <- strsplit(ctx$evidence, "\n", fixed = TRUE)[[1]]
  l[grepl(pattern, l, fixed = TRUE)]
}


test_that("the Ferralsol's evidence answers 'why not an Acrisol'", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  e <- .grounding_env()
  ctx <- e$.chat_pedon_context(make_ferralsol_canonical(), NULL)
  ev <- strsplit(ctx$evidence, "\n", fixed = TRUE)[[1]]

  # the WRB key as walked: Ferralsols assigned 16th, Acrisols never reached
  expect_true("  16. Ferralsols: met" %in% ev)
  never <- .ev_lines(ctx, "Classes after Ferralsols in this key, therefore never tested")
  expect_length(never, 1L)
  expect_match(never, "Acrisols", fixed = TRUE)
  # the ferralic criteria with the values and the limit applied
  expect_match(.ev_lines(ctx, "- cec_per_clay:")[1],
               "values #1=16, #2=12.5, #3=10, #4=8.33, #5=8 (limit 16)", fixed = TRUE)
  expect_match(.ev_lines(ctx, "- argic: not met")[1], "clay-increase", fixed = TRUE)
})

test_that("qualifiers carry the rule soilKey applied (Eutric is not BS at pH 7)", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  e <- .grounding_env()
  ctx <- e$.chat_pedon_context(make_ferralsol_canonical(), NULL)
  eu <- .ev_lines(ctx, "- Eutric: met")
  expect_length(eu, 1L)
  expect_match(eu, "exchangeable Al vs exchangeable bases", fixed = TRUE)
  expect_match(eu, "Not base saturation", fixed = TRUE)
  # SiBCS: eutrófico was tested and failed on V% -- the reason for Distróficos
  expect_match(.ev_lines(ctx, "- eutrofico: not met")[1],
               "bs_pct values #3=14, #4=13, #5=13 (limit 50)", fixed = TRUE)
  expect_match(.ev_lines(ctx, "Classes after Latossolos")[1], "Argissolos", fixed = TRUE)
  expect_match(.ev_lines(ctx, "Classes after Oxisols")[1], "Ultisols", fixed = TRUE)
})

test_that("a class tested before the one assigned shows as tested and not met", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  e <- .grounding_env()
  ctx <- e$.chat_pedon_context(make_luvisol_canonical(), NULL)
  ev <- strsplit(ctx$evidence, "\n", fixed = TRUE)[[1]]
  al <- grep("^  [0-9]+\\. Alisols: not met", ev)
  lv <- grep("^  [0-9]+\\. Luvisols: met", ev)
  expect_length(al, 1L)
  expect_length(lv, 1L)
  expect_lt(al, lv)                              # gpt-oss had this backwards
  expect_match(.ev_lines(ctx, "Classes after Luvisols")[1], "Cambisols", fixed = TRUE)
})

test_that("every horizon value is labelled, so columns cannot be swapped", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  e <- .grounding_env()
  ctx <- e$.chat_pedon_context(make_ferralsol_canonical(), NULL)
  h1 <- .ev_lines(ctx, "#1 A 0-15 cm:")
  expect_length(h1, 1L)
  # given a pipe table, a model read Al saturation (26) as base saturation (24)
  expect_match(h1, "BS 24%", fixed = TRUE)
  expect_match(h1, "Al sat 26%", fixed = TRUE)
  expect_match(h1, "exch Ca 1.2 Mg 0.5 K 0.15 Na 0.05 Al 0.7", fixed = TRUE)
  expect_false(grepl(" | ", ctx$evidence, fixed = TRUE))
})

test_that("a long profile is cut to the budget, and says so", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  e <- .grounding_env()
  n <- 40L
  h <- data.frame(top_cm = seq(0, by = 5, length.out = n),
                  bottom_cm = seq(5, by = 5, length.out = n),
                  designation = paste0("L", seq_len(n)),
                  structure_grade = "moderate", structure_type = "subangular blocky",
                  clay_pct = 30, silt_pct = 30, sand_pct = 40, ph_h2o = 6,
                  oc_pct = 1, cec_cmol = 10, bs_pct = 60, n_total_pct = 0.1)
  out <- e$.chat_horizon_lines(h, budget = 1500L)
  expect_lte(sum(nchar(out[-1])), 1500L + 60L)
  expect_match(out[length(out)], "deeper horizons not shown", fixed = TRUE)
  expect_false(any(grepl("structure", out)))     # field description dropped first
  expect_true(any(grepl("^  #1 L1 0-5 cm:", out)))
})

test_that("the scripted reply keeps the short summary; only the model gets it all", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  e <- .grounding_env()
  ctx <- e$.chat_pedon_context(make_ferralsol_canonical(), NULL)
  expect_false(grepl("Criteria behind", ctx$text, fixed = TRUE))
  expect_lt(nchar(ctx$text), 1000L)
  sys <- e$.chat_system_prompt(ctx, "en")
  expect_match(sys, "### Evidence used by the keys", fixed = TRUE)
  expect_match(sys, "Do not fill the gap from memory", fixed = TRUE)
  # not said twice: the one-line horizon list and missing-data list go
  expect_false(grepl("\nHorizons (5):", sys, fixed = TRUE))
  expect_false(grepl("\nMissing data:", sys, fixed = TRUE))
  expect_match(e$.chat_system_prompt(ctx, "pt"), "Não complete a lacuna de memória",
               fixed = TRUE)
  # what a question costs, in characters; about 2.5 per token. Qwen on Groq's
  # free tier takes 7,000 input tokens a minute.
  expect_lt(nchar(sys), 10000L)
})

test_that("the site record is passed on, and an empty session is said to be empty", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  e <- .grounding_env()
  p <- make_ferralsol_canonical()
  p$site$parent_material <- "basalt"
  p$site$citation <- strrep("long citation text ", 20)
  ctx <- e$.chat_pedon_context(p, NULL)
  expect_match(ctx$text, "parent_material basalt", fixed = TRUE)
  expect_false(grepl("long citation", ctx$text, fixed = TRUE))
  # asked about "this profile" with none loaded, the model is told so
  # (it answered "the evidence was not provided in your message")
  expect_match(e$.chat_system_prompt(NULL, "en"), "No profile has been loaded",
               fixed = TRUE)
})

test_that("the evidence reader handles R6 results, lists and odd names", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  e <- .grounding_env()
  dr <- DiagnosticResult$new(name = "x", passed = TRUE, layers = 2:3,
                             reference = "Ref p. 1")
  expect_true(e$.chat_get(dr, "passed"))
  expect_identical(e$.chat_get(list(passed = FALSE), "passed"), FALSE)
  expect_null(e$.chat_get("atomic", "passed"))
  expect_null(e$.chat_get(list(passed_extra = TRUE), "passed"))   # no partial match
  expect_match(e$.chat_criteria(dr, "x"), "x: met in horizons #2-#3 [Ref p. 1]",
               fixed = TRUE)
  expect_identical(e$.chat_runs(c(1, 2, 3, 5, 7, 8)), "#1-#3, #5, #7-#8")
  w <- e$.chat_common_words(c("Latossolos Vermelhos Ácricos", "Latossolos Vermelhos Distróficos"))
  expect_identical(w$common, "Latossolos Vermelhos ...")
  expect_identical(w$rest, c("Ácricos", "Distróficos"))
  w <- e$.chat_common_words(c("Lithic Hapludox", "Rhodic Hapludox"))
  expect_identical(w$common, "... Hapludox")
  expect_null(e$.chat_common_words(c("Acrudox", "Hapludox"))$common)
})


# ---- a model at its per-minute limit hands the question on -------------------
# (v0.9.209: the calls are asynchronous, so the helpers return promises.)

# Wait for a promise in a test.
.await <- function(p, timeout = 5) {
  out <- NULL; done <- FALSE
  promises::then(p, function(v) { out <<- v; done <<- TRUE },
                 function(e) { out <<- e; done <<- TRUE })
  t0 <- Sys.time()
  while (!done && difftime(Sys.time(), t0, units = "secs") < timeout)
    later::run_now(0.05)
  out
}

# A stand-in for an ellmer chat: answers, or fails as Groq does at its limit or
# after retiring the model.
.fake_chat <- function(model, state) {
  turns <- list()
  list(model = model,
       chat_async = function(msg) {
         if (model %in% state$gone)
           return(promises::promise_reject(simpleError(paste0(
             "HTTP 404 Not Found. The model `", model, "` does not exist"))))
         if (model %in% state$busy)
           return(promises::promise_reject(simpleError(paste0(
             "HTTP 429 Too Many Requests. Rate limit reached for model `", model,
             "` on input tokens per minute (ITPM): Limit 7000"))))
         turns <<- c(turns, list(msg, paste("answer from", model)))
         promises::promise_resolve(paste("answer from", model))
       },
       get_turns = function() turns,
       set_turns = function(x) turns <<- x)
}

.fake_backend <- function(e, state, models = c("qwen/qwen3.8-27b", "openai/gpt-oss-120b",
                                              "openai/gpt-oss-20b")) {
  e$.groq_available_models <- function(key, ttl = 3600) setdiff(models, state$gone)
  e$.chat_make_groq <- function(key, model, system_prompt) .fake_chat(model, state)
  main <- .fake_chat("qwen/qwen3.8-27b", state)
  main$set_turns(list("earlier question", "earlier answer"))
  list(chat = main, model = "qwen/qwen3.8-27b", key = "k")
}

test_that("the next model answers, with the conversation, and says so", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  e <- .grounding_env()
  state <- new.env(); state$busy <- "qwen/qwen3.8-27b"; state$gone <- character(0)
  b <- .fake_backend(e, state)

  out <- .await(e$.chat_converse_async(b, "why?", "sys"))
  expect_identical(out$reply, "answer from openai/gpt-oss-120b")
  expect_identical(out$msg, "why?")
  expect_match(out$note, "openai/gpt-oss-120b", fixed = TRUE)
  expect_match(out$note, "qwen/qwen3.8-27b", fixed = TRUE)
  # the stand-in saw the earlier turns, and its answer is in the main history
  expect_identical(b$chat$get_turns(), list("earlier question", "earlier answer",
                                            "why?", "answer from openai/gpt-oss-120b"))

  # with every model at its limit, the user is told to wait, not handed a summary
  state$busy <- c("qwen/qwen3.8-27b", "openai/gpt-oss-120b", "openai/gpt-oss-20b")
  out <- .await(e$.chat_converse_async(b, "and?", "sys"))
  expect_identical(out$reply, "rate_limited")
  expect_null(out$note)
})

test_that("a retired model is replaced and the conversation carried over", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  e <- .grounding_env()
  state <- new.env(); state$busy <- character(0)
  state$gone <- "qwen/qwen3.8-27b"
  b <- .fake_backend(e, state)
  e$.groq_forget_models()
  out <- .await(e$.chat_converse_async(b, "why?", "sys"))
  expect_identical(out$reply, "answer from openai/gpt-oss-120b")
  expect_identical(out$backend$model, "openai/gpt-oss-120b")    # kept for next time
  expect_identical(out$backend$chat$get_turns()[1:2],
                   list("earlier question", "earlier answer"))
})

test_that("other failures are passed through, not mistaken for a limit", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  e <- .grounding_env()
  made <- 0L
  e$.chat_make_groq <- function(...) { made <<- made + 1L; NULL }
  chat <- list(chat_async = function(msg)
    promises::promise_reject(simpleError("HTTP 500 Internal Server Error")),
    get_turns = function() list(), set_turns = function(x) NULL)
  out <- .await(e$.chat_converse_async(list(chat = chat, model = "m", key = "k"),
                                       "why?", "sys"))
  expect_s3_class(out$reply, "error")
  expect_match(conditionMessage(out$reply), "500")
  expect_equal(made, 0L)
})

test_that("Groq calls fail fast instead of waiting out Retry-After", {
  skip_on_cran()
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d)) d <- file.path("inst", "shiny", "classify_app_pro")
  # ellmer's default retries a 429 after the wait Groq asks for; a synchronous
  # call held the R process meanwhile, and the Assistant hands the question on.
  expect_true(any(grepl("options(ellmer_max_tries = 1L)",
                        readLines(file.path(d, "app.R")), fixed = TRUE)))
})
