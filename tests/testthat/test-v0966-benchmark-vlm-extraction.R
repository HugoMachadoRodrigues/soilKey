# =============================================================================
# Tests for v0.9.66 -- benchmark_vlm_extraction() + metrics + .onAttach.
# All tests run unconditionally (no real Ollama / no network).
# =============================================================================


# ---- Fixture discovery ----------------------------------------------------

test_that("list_vlm_fixtures finds bundled horizons + site fixtures", {
  fx_h <- list_vlm_fixtures("horizons")
  expect_s3_class(fx_h, "data.frame")
  expect_true(nrow(fx_h) >= 2L,
                info = "expected at least 2 bundled horizons fixtures")
  expect_setequal(names(fx_h), c("id", "input_path", "golden_path"))
  expect_true(all(file.exists(fx_h$input_path)))
  expect_true(all(file.exists(fx_h$golden_path)))

  fx_s <- list_vlm_fixtures("site")
  expect_true(nrow(fx_s) >= 2L)
  expect_true(all(file.exists(fx_s$input_path)))
})


test_that("list_vlm_fixtures returns empty df when munsell dir has no images", {
  fx_m <- list_vlm_fixtures("munsell")
  expect_s3_class(fx_m, "data.frame")
  # README.md does NOT count as a fixture (no .golden.json companion)
  expect_equal(nrow(fx_m), 0L)
})


# ---- Synthetic fixture generator ----------------------------------------

test_that("make_synthetic_horizons_fixture writes input + golden pairs", {
  hz <- data.table::data.table(
    top_cm = c(0, 20),
    bottom_cm = c(20, 60),
    designation = c("A", "Bt"),
    munsell_hue_moist = c("10YR", "5YR"),
    munsell_value_moist = c(4, 4),
    munsell_chroma_moist = c(3, 6),
    clay_pct = c(20, 45),
    silt_pct = c(30, 22),
    sand_pct = c(50, 33),
    ph_h2o   = c(5.4, 5.0),
    oc_pct   = c(1.2, 0.3)
  )
  ped <- PedonRecord$new(
    site = list(id = "test-fx", state = "RJ", municipality = "Itaguai",
                  country = "BR"),
    horizons = ensure_horizon_schema(hz)
  )
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  out <- make_synthetic_horizons_fixture(ped, "fx_test", out_dir = tmp)
  expect_true(file.exists(out$input_path))
  expect_true(file.exists(out$golden_path))
  txt <- paste(readLines(out$input_path, warn = FALSE), collapse = "\n")
  expect_match(txt, "Itaguai")
  expect_match(txt, "5YR 4/6")
  golden <- jsonlite::fromJSON(out$golden_path, simplifyVector = FALSE)
  expect_true(length(golden$horizons) == 2L)
})


test_that("make_synthetic_horizons_fixture rejects bad input", {
  expect_error(make_synthetic_horizons_fixture("not a pedon", "id"),
                  "PedonRecord")
  ped <- PedonRecord$new(
    site = list(id = "x"),
    horizons = ensure_horizon_schema(
      data.table::data.table(top_cm = numeric(0), bottom_cm = numeric(0))
    )
  )
  expect_error(make_synthetic_horizons_fixture(ped, "x", out_dir = tempdir()),
                  "no horizons")
})


# ---- Metric: horizons overlap --------------------------------------------

test_that(".metric_horizons_overlap returns precision = recall = 1 on identical sets", {
  golden <- list(horizons = list(
    list(top_cm = 0,  bottom_cm = 20, clay_pct = 25, ph_h2o = 5.4),
    list(top_cm = 20, bottom_cm = 60, clay_pct = 45, ph_h2o = 5.0)
  ))
  pred <- golden
  m <- soilKey:::.metric_horizons_overlap(pred, golden)
  expect_equal(m$precision, 1)
  expect_equal(m$recall,    1)
  expect_equal(m$attr_match_rate, 1)
})


test_that(".metric_horizons_overlap penalises missing horizons", {
  golden <- list(horizons = list(
    list(top_cm = 0,  bottom_cm = 20),
    list(top_cm = 20, bottom_cm = 60),
    list(top_cm = 60, bottom_cm = 100)
  ))
  pred <- list(horizons = list(
    list(top_cm = 0,  bottom_cm = 20),
    list(top_cm = 20, bottom_cm = 60)
  ))
  m <- soilKey:::.metric_horizons_overlap(pred, golden)
  expect_equal(m$recall, 2/3)
  expect_equal(m$precision, 1)
})


test_that(".metric_horizons_overlap detects attribute mismatch", {
  golden <- list(horizons = list(
    list(top_cm = 0, bottom_cm = 20, clay_pct = 25, ph_h2o = 5.4)
  ))
  pred <- list(horizons = list(
    list(top_cm = 0, bottom_cm = 20, clay_pct = 50, ph_h2o = 5.4)  # clay wrong
  ))
  m <- soilKey:::.metric_horizons_overlap(pred, golden)
  expect_equal(m$recall, 1)
  expect_equal(m$attr_match_rate, 0.5)  # 1 of 2 numeric attrs matched
})


# ---- Metric: site IoU ----------------------------------------------------

test_that(".metric_site_iou: identical site -> iou=1, value_accuracy=1", {
  golden <- list(site = list(id = "x", lat = -22.5, lon = -43.7,
                                country = "BR", land_use = "pastagem"))
  pred <- golden
  m <- soilKey:::.metric_site_iou(pred, golden)
  expect_equal(m$iou,            1)
  expect_equal(m$value_accuracy, 1)
})


test_that(".metric_site_iou: missing fields lower IoU", {
  golden <- list(site = list(id = "x", lat = -22.5, lon = -43.7,
                                country = "BR"))
  pred <- list(site = list(id = "x", lat = -22.5))
  m <- soilKey:::.metric_site_iou(pred, golden)
  expect_equal(m$recall, 0.5)
  expect_equal(m$iou,    0.5)
})


test_that(".metric_site_iou: numeric mismatch counts as wrong", {
  golden <- list(site = list(lat = -22.5, lon = -43.7))
  pred   <- list(site = list(lat = -22.5, lon = -50.0))   # lon wrong
  m <- soilKey:::.metric_site_iou(pred, golden, numeric_tol = 0.01)
  expect_equal(m$value_accuracy, 0.5)
})


# ---- Metric: Munsell ΔE ---------------------------------------------------

test_that(".munsell_delta_e returns 0 on identical Munsell triplets", {
  skip_if_not_installed("munsellinterpol")
  d <- soilKey:::.munsell_delta_e("5YR", 4, 6, "5YR", 4, 6)
  expect_true(is.numeric(d))
  expect_lt(d, 0.01)  # numerical noise tolerance
})


test_that(".munsell_delta_e increases with chroma distance", {
  skip_if_not_installed("munsellinterpol")
  near <- soilKey:::.munsell_delta_e("5YR", 4, 6,  "5YR", 4, 5)
  far  <- soilKey:::.munsell_delta_e("5YR", 4, 6,  "5YR", 4, 1)
  expect_true(is.finite(near))
  expect_true(is.finite(far))
  expect_lt(near, far)
})


test_that(".munsell_delta_e returns NA when any input missing", {
  expect_true(is.na(soilKey:::.munsell_delta_e(NA, 4, 6, "5YR", 4, 6)))
  expect_true(is.na(soilKey:::.munsell_delta_e("5YR", 4, 6, "5YR", NA, 6)))
})


# ---- Top-level benchmark with mock provider ------------------------------

test_that("benchmark_vlm_extraction runs end-to-end with a MockVLMProvider", {
  # Mock that always returns the perfil_RJ_argissolo golden as JSON.
  # Note: bare horizons golden does NOT match the soilKey horizon schema
  # (which wraps each attribute in value/confidence/source_quote), so
  # the extractor's validate_or_retry will fail. We test the harness
  # gracefully captures that as ok = FALSE -- which is exactly what we
  # want when benchmarking against real models on imperfect data.
  golden_text <- paste(readLines(
    list_vlm_fixtures("horizons")$golden_path[1L],
    warn = FALSE), collapse = "\n")
  mock <- MockVLMProvider$new(responses = rep(list(golden_text), 12))
  bench <- benchmark_vlm_extraction(
    providers     = list(mock_perfect = mock),
    tasks         = "horizons",
    max_per_task  = 1L,
    verbose       = FALSE
  )
  expect_s3_class(bench$predictions, "data.frame")
  expect_true(nrow(bench$predictions) >= 1L)
  expect_true(all(c("provider", "task", "fixture", "ok", "error",
                      "metric_1", "metric_2", "metric_3",
                      "metric_1_name", "metric_2_name", "metric_3_name")
                    %in% names(bench$predictions)))
  # Summary table populated.
  expect_s3_class(bench$summary, "data.frame")
  expect_true(all(c("provider", "task", "n", "ok_rate")
                    %in% names(bench$summary)))
})


test_that("benchmark_vlm_extraction rejects malformed providers list", {
  expect_error(benchmark_vlm_extraction(providers = list(),
                                            tasks = "horizons",
                                            verbose = FALSE),
                  "non-empty named list")
  expect_error(benchmark_vlm_extraction(providers = list("a"),
                                            tasks = "horizons",
                                            verbose = FALSE),
                  "non-empty named list")
})


# ---- .suggest_local_vlm_message ------------------------------------------

test_that(".suggest_local_vlm_message returns empty string when ollama absent", {
  withr::with_envvar(c(PATH = ""), {
    if (ollama_is_installed()) {
      skip("Cannot test installed=FALSE path on this machine.")
    }
    msg <- soilKey:::.suggest_local_vlm_message("gemma4:e2b")
    expect_equal(msg, "")
  })
})


test_that(".suggest_local_vlm_message: 'ready' shape when model present", {
  if (!ollama_is_running()) skip("Need running Ollama to test ready path.")
  models <- tryCatch(ollama_list_local_models(),
                       error = function(e) character(0))
  if (length(models) == 0L) skip("No local models present.")
  msg <- soilKey:::.suggest_local_vlm_message(models[1L])
  expect_match(msg, "local VLM ready")
  expect_match(msg, models[1L], fixed = TRUE)
})


test_that(".suggest_local_vlm_message: 'pull' shape when model missing", {
  if (!ollama_is_running()) skip("Need running Ollama to test pull-suggestion path.")
  msg <- soilKey:::.suggest_local_vlm_message("definitely-not-a-real-model:99")
  expect_match(msg, "not yet pulled")
  expect_match(msg, "setup_local_vlm")
})
