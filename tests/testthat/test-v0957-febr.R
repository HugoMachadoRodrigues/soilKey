# =============================================================================
# Tests for v0.9.57 -- FEBR loader (read_febr_pedons + febr_index_munsell).
#
# Munsell parser tests + column-detection tests + index-cache tests run
# unconditionally (no network, no febr package needed). Live FEBR test
# is gated on febr availability + SOILKEY_NETWORK_TESTS.
# =============================================================================


# ---- .parse_febr_munsell -----------------------------------------------

test_that(".parse_febr_munsell handles canonical PT-BR Munsell strings", {
  out <- soilKey:::.parse_febr_munsell("2,5YR 3/6")
  expect_equal(out$hue, "2.5YR")
  expect_equal(out$value, 3)
  expect_equal(out$chroma, 6)
})


test_that(".parse_febr_munsell handles fractional value/chroma", {
  out <- soilKey:::.parse_febr_munsell("10YR 5,5/3,5")
  expect_equal(out$hue, "10YR")
  expect_equal(out$value, 5.5)
  expect_equal(out$chroma, 3.5)
})


test_that(".parse_febr_munsell returns NA on garbage / empty input", {
  for (bad in list(NA, "", "   ", "lixo", NULL)) {
    out <- soilKey:::.parse_febr_munsell(bad)
    expect_true(is.na(out$hue))
    expect_true(is.na(out$value))
    expect_true(is.na(out$chroma))
  }
})


test_that(".parse_febr_munsell_vec vectorises over a character vector", {
  v <- c("10YR 4/3", NA, "2,5YR 3/6", "")
  df <- soilKey:::.parse_febr_munsell_vec(v)
  expect_equal(nrow(df), 4L)
  expect_equal(df$hue,    c("10YR", NA, "2.5YR", NA))
  expect_equal(df$value,  c(4, NA, 3, NA))
  expect_equal(df$chroma, c(3, NA, 6, NA))
})


# ---- .detect_febr_munsell_columns --------------------------------------

test_that(".detect_febr_munsell_columns picks parsed columns when available", {
  cols <- c("camada_id", "profund_sup", "profund_inf",
              "cor_munsell_umida_matiz", "cor_munsell_umida_valor",
              "cor_munsell_umida_croma", "cor_munsell_umida_nome",
              "ph_h2o", "argila")
  out <- soilKey:::.detect_febr_munsell_columns(cols)
  expect_equal(out$moist_hue,    "cor_munsell_umida_matiz")
  expect_equal(out$moist_value,  "cor_munsell_umida_valor")
  expect_equal(out$moist_chroma, "cor_munsell_umida_croma")
  expect_equal(out$moist_string, "cor_munsell_umida_nome")
})


test_that(".detect_febr_munsell_columns falls back to string column when parsed are absent", {
  cols <- c("profund_sup", "profund_inf", "cor_cod_munsell_umida", "argila")
  out <- soilKey:::.detect_febr_munsell_columns(cols)
  expect_true(is.na(out$moist_hue))
  expect_equal(out$moist_string, "cor_cod_munsell_umida")
})


test_that(".detect_febr_munsell_columns picks ctb0005 variants", {
  cols <- c("cor_cod_munsell_umida_1", "cor_nome_munsell_umida_1",
              "cor_cod_munsell_seca_1")
  out <- soilKey:::.detect_febr_munsell_columns(cols)
  expect_equal(out$moist_string, "cor_cod_munsell_umida_1")
  expect_equal(out$dry_string,   "cor_cod_munsell_seca_1")
})


# ---- .febr_match_layer_columns -----------------------------------------

test_that(".febr_match_layer_columns maps FEBR layer names to soilKey", {
  cols <- c("camada_nome", "profund_sup", "profund_inf",
              "ph_h2o", "carbono", "argila", "silte", "areia",
              "ca_troc", "mg_troc", "k_troc", "al_troc",
              "ctc", "v_pct", "p_assim",
              "cor_munsell_umida_matiz")
  out <- soilKey:::.febr_match_layer_columns(cols)
  expect_equal(out$designation, "camada_nome")
  expect_equal(out$top_cm,      "profund_sup")
  expect_equal(out$bottom_cm,   "profund_inf")
  expect_equal(out$ph_h2o,      "ph_h2o")
  expect_equal(out$oc_pct,      "carbono")
  expect_equal(out$clay_pct,    "argila")
  expect_equal(out$silt_pct,    "silte")
  expect_equal(out$sand_pct,    "areia")
  expect_equal(out$ca_cmol,     "ca_troc")
  expect_equal(out$bs_pct,      "v_pct")
  expect_equal(out$cec_cmol,    "ctc")
})


# ---- .FEBR_MUNSELL_INDEX bundled cache --------------------------------

test_that(".FEBR_MUNSELL_INDEX bundled cache has the expected shape", {
  idx <- get(".FEBR_MUNSELL_INDEX", envir = asNamespace("soilKey"))
  expect_s3_class(idx, "data.frame")
  expect_named(idx, c("dataset_id", "n_horizons", "n_finite_munsell",
                       "coverage", "column_pattern"))
  # Result of the May 2026 scan: 200 datasets
  expect_equal(nrow(idx), 200L)
  # Top dataset is ctb0032 (10,577 horizons with Munsell)
  expect_equal(idx$dataset_id[1L], "ctb0032")
  expect_equal(idx$n_finite_munsell[1L], 10577L)
})


test_that("febr_index_munsell uses the cache by default", {
  idx <- febr_index_munsell(min_coverage = 0, verbose = FALSE)
  expect_s3_class(idx, "data.frame")
  expect_equal(nrow(idx), 200L)

  # Filter by coverage
  idx_high <- febr_index_munsell(min_coverage = 0.9, verbose = FALSE)
  expect_true(nrow(idx_high) <= nrow(idx))
  expect_true(all(idx_high$coverage >= 0.9))
})


test_that("febr_index_munsell sorts descending by n_finite_munsell", {
  idx <- febr_index_munsell(min_coverage = 0, verbose = FALSE)
  expect_equal(idx$n_finite_munsell, sort(idx$n_finite_munsell, decreasing = TRUE))
})


# ---- read_febr_pedons error paths --------------------------------------

test_that("read_febr_pedons errors clearly when febr package is missing", {
  if (requireNamespace("febr", quietly = TRUE)) {
    skip("febr installed -- cannot exercise the missing-pkg path")
  }
  expect_error(read_febr_pedons("ctb0039"), "febr")
})


# ---- Live FEBR test (opt-in) -------------------------------------------

test_that("read_febr_pedons retrieves a real FEBR dataset when network is enabled", {
  testthat::skip_if_not_installed("febr")
  if (!nzchar(Sys.getenv("SOILKEY_NETWORK_TESTS"))) {
    skip("Live FEBR test gated by SOILKEY_NETWORK_TESTS env var")
  }
  pedons <- read_febr_pedons("ctb0039", verbose = FALSE)
  expect_type(pedons, "list")
  expect_true(length(pedons) >= 1L)
  expect_s3_class(pedons[[1L]], "PedonRecord")
  # ctb0039 is 100% Munsell -- at least one horizon has hue
  has_munsell <- vapply(pedons, function(p) {
    any(!is.na(p$horizons$munsell_hue_moist))
  }, logical(1L))
  expect_true(any(has_munsell))
})
