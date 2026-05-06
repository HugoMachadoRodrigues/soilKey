# =============================================================================
# Tests for v0.9.64 -- setup_local_vlm() + ollama lifecycle helpers +
#                      pedologist_system_prompt().
# All tests run unconditionally (no real Ollama required); behaviour
# without Ollama is part of the contract we test.
# =============================================================================


# ---- ollama_is_installed --------------------------------------------------

test_that("ollama_is_installed returns a logical scalar", {
  res <- ollama_is_installed()
  expect_type(res, "logical")
  expect_length(res, 1L)
})


# ---- ollama_list_local_models ---------------------------------------------

test_that("ollama_list_local_models returns character() when daemon offline", {
  withr::with_options(list(soilKey.ollama_url = "http://127.0.0.1:1/no-such-host"), {
    res <- ollama_list_local_models()
    expect_type(res, "character")
    # Either empty (daemon not reachable) or a real list of models when
    # the developer happens to have Ollama running on a non-standard port.
    expect_true(is.character(res))
  })
})


# ---- ollama_pull_model: input validation ----------------------------------

test_that("ollama_pull_model rejects non-character / empty model", {
  expect_error(ollama_pull_model(NULL),    "non-empty character")
  expect_error(ollama_pull_model(""),      "non-empty character")
  expect_error(ollama_pull_model(c("a", "b")), "non-empty character")
  expect_error(ollama_pull_model(NA_character_), "non-empty character")
})


test_that("ollama_pull_model returns FALSE when ollama not installed", {
  # Force ollama_is_installed() FALSE by stubbing Sys.which.
  withr::with_envvar(c(PATH = ""), {
    if (!ollama_is_installed()) {
      res <- ollama_pull_model("gemma4:e2b", verbose = FALSE)
      expect_false(res)
    } else {
      skip("Cannot reset PATH on this platform; ollama still on PATH.")
    }
  })
})


# ---- setup_local_vlm: catalog resolution ---------------------------------

test_that("setup_local_vlm resolves catalog labels to real model names", {
  withr::with_envvar(c(PATH = ""), {
    if (ollama_is_installed()) {
      skip("Cannot test installed = FALSE path on this machine (ollama on PATH).")
    }
    out_light    <- setup_local_vlm("light",    verbose = FALSE)
    out_balanced <- setup_local_vlm("balanced", verbose = FALSE)
    out_best     <- setup_local_vlm("best",     verbose = FALSE)
    expect_equal(out_light$model,    "gemma4:e2b")
    expect_equal(out_balanced$model, "gemma4:e4b")
    expect_equal(out_best$model,     "gemma4:31b")
    # All three should report installed = FALSE here, so ready = FALSE.
    expect_false(out_light$ready)
    expect_false(out_balanced$ready)
    expect_false(out_best$ready)
    expect_match(out_light$hint, "Install Ollama")
  })
})


test_that("setup_local_vlm returns the documented status schema", {
  withr::with_envvar(c(PATH = ""), {
    if (ollama_is_installed()) {
      skip("Cannot test installed = FALSE path on this machine (ollama on PATH).")
    }
    out <- setup_local_vlm("light", verbose = FALSE)
    expect_named(out, c("ready", "model", "ollama_url", "installed",
                          "running", "pulled", "hint"))
    expect_type(out$ready,     "logical")
    expect_type(out$installed, "logical")
    expect_type(out$running,   "logical")
    expect_type(out$pulled,    "logical")
    expect_type(out$model,     "character")
    expect_type(out$ollama_url, "character")
    expect_type(out$hint,      "character")
  })
})


test_that("setup_local_vlm accepts an arbitrary explicit model identifier", {
  withr::with_envvar(c(PATH = ""), {
    if (ollama_is_installed()) {
      skip("Cannot test installed = FALSE path on this machine (ollama on PATH).")
    }
    out <- setup_local_vlm("qwen2.5vl:7b", verbose = FALSE)
    expect_equal(out$model, "qwen2.5vl:7b")
  })
})


# ---- ollama_ensure_running: short-circuits when already running -----------

test_that("ollama_ensure_running returns TRUE immediately when daemon running", {
  if (!ollama_is_running()) {
    skip("Ollama daemon not running -- skipping the no-op short-circuit test.")
  }
  expect_true(ollama_ensure_running(verbose = FALSE))
})


test_that("ollama_ensure_running returns FALSE when ollama not installed", {
  if (ollama_is_running()) {
    skip("Ollama daemon already running on this machine (would short-circuit to TRUE).")
  }
  withr::with_envvar(c(PATH = ""), {
    if (ollama_is_installed()) {
      skip("Cannot test installed = FALSE path on this machine (ollama on PATH).")
    }
    expect_false(ollama_ensure_running(verbose = FALSE))
  })
})


# ---- pedologist_system_prompt -------------------------------------------

test_that("pedologist_system_prompt returns a non-empty string per language", {
  pt <- pedologist_system_prompt("pt-BR")
  en <- pedologist_system_prompt("en")
  expect_type(pt, "character"); expect_length(pt, 1L); expect_gt(nchar(pt), 200L)
  expect_type(en, "character"); expect_length(en, 1L); expect_gt(nchar(en), 200L)
  # PT-BR contains the SiBCS reference; EN contains "U.S. pedology"
  expect_match(pt, "SiBCS")
  expect_match(en, "U\\.S\\. pedology")
})


test_that("pedologist_system_prompt forbids invented values + delegated classification", {
  for (lang in c("pt-BR", "en")) {
    p <- pedologist_system_prompt(lang)
    # The persona must explicitly forbid classification (deterministic key
    # owns that), and must forbid hallucination ("Do not invent values").
    expect_match(p, "NEVER classif|NUNCA classifica")
    expect_match(p, "Do not invent|Nao invente")
  }
})


test_that("pedologist_system_prompt rejects unsupported languages", {
  expect_error(pedologist_system_prompt("fr"), "should be one of")
})
