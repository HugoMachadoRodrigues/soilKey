# Tests for the v0.9.100 provenance-weighted uncertainty MC:
# get_perturbation_scale(), classify_with_uncertainty(), and the
# classification_robustness() provenance_aware extension.

# ---- get_perturbation_scale -----------------------------------------------

test_that("perturbation scales grow monotonically from grade A to E", {
  skip_on_cran()
  pcts <- vapply(c("A", "B", "C", "D", "E"),
                 function(g) get_perturbation_scale(g)$pct, numeric(1))
  expect_equal(unname(pcts), sort(unname(pcts)))
  expect_lt(get_perturbation_scale("A")$pct, get_perturbation_scale("E")$pct)
  expect_named(get_perturbation_scale("A"),
               c("pct", "ph_abs", "munsell_abs"))
})

test_that("get_perturbation_scale rejects an unknown grade", {
  skip_on_cran()
  expect_error(get_perturbation_scale("Z"), "arg")
})

# ---- classify_with_uncertainty: structure ---------------------------------

test_that("classify_with_uncertainty returns a well-formed posterior", {
  skip_on_cran()
  u <- classify_with_uncertainty(make_ferralsol_canonical(),
                                 n = 40, system = "wrb2022", seed = 1)
  expect_s3_class(u, "soilkey_uncertainty")
  expect_true(is.numeric(u$posterior))
  expect_equal(sum(u$posterior), 1, tolerance = 1e-8)
  expect_true(!is.null(names(u$posterior)))
  expect_identical(u$top1, names(u$posterior)[1])
  expect_gte(u$entropy, 0)
  expect_equal(u$n_runs, 40L)
})

test_that("a canonical Ferralsol is a robust classification", {
  skip_on_cran()
  u <- classify_with_uncertainty(make_ferralsol_canonical(),
                                 n = 60, system = "wrb2022", seed = 1)
  expect_gt(as.numeric(u$posterior["Ferralsols"]), 0.85)
  expect_lt(u$entropy, 0.5)
})

test_that("the print method runs without error", {
  skip_on_cran()
  u <- classify_with_uncertainty(make_ferralsol_canonical(),
                                 n = 30, system = "wrb2022",
                                 sensitivity = FALSE, seed = 1)
  expect_output(print(u), "soilkey_uncertainty")
})

# ---- provenance weighting actually changes the posterior ------------------

test_that("downgrading provenance to user-assumed widens the posterior", {
  skip_on_cran()
  measured <- classify_with_uncertainty(
    make_ferralsol_canonical(), n = 60, system = "wrb2022",
    sensitivity = FALSE, seed = 1)

  # Same profile, but every clay value is now a bare assumption (grade E):
  # its perturbation half-width jumps from 3 % to 30 %.
  p_assumed <- make_ferralsol_canonical()
  for (i in seq_len(nrow(p_assumed$horizons))) {
    p_assumed$add_measurement(i, "clay_pct",
                              p_assumed$horizons$clay_pct[i],
                              "user_assumed", confidence = 0.2,
                              overwrite = TRUE)
  }
  assumed <- classify_with_uncertainty(
    p_assumed, n = 60, system = "wrb2022",
    sensitivity = FALSE, seed = 1)

  expect_gt(assumed$entropy, measured$entropy)
})

# ---- sensitivity ----------------------------------------------------------

test_that("the sensitivity table ranks perturbable attributes", {
  skip_on_cran()
  u <- classify_with_uncertainty(make_acrisol_canonical(),
                                 n = 40, system = "wrb2022", seed = 3)
  expect_s3_class(u$sensitivity, "data.table")
  expect_identical(names(u$sensitivity), c("attribute", "importance"))
  expect_gt(nrow(u$sensitivity), 0L)
  # sorted descending by importance
  imp <- u$sensitivity$importance
  expect_equal(imp, sort(imp, decreasing = TRUE))
})

test_that("sensitivity = FALSE skips the leave-one-out pass", {
  skip_on_cran()
  u <- classify_with_uncertainty(make_ferralsol_canonical(),
                                 n = 30, system = "wrb2022",
                                 sensitivity = FALSE, seed = 1)
  expect_null(u$sensitivity)
})

# ---- edge cases -----------------------------------------------------------

test_that("n = 1 yields a degenerate but valid posterior", {
  skip_on_cran()
  u <- classify_with_uncertainty(make_ferralsol_canonical(),
                                 n = 1, system = "wrb2022",
                                 sensitivity = FALSE, seed = 1)
  expect_equal(u$n_runs, 1L)
  expect_equal(sum(u$posterior), 1, tolerance = 1e-8)
})

test_that("a pedon with no perturbable attributes is handled gracefully", {
  skip_on_cran()
  h <- make_empty_horizons(2L)
  h$top_cm    <- c(0, 30)
  h$bottom_cm <- c(30, 80)
  h$designation <- c("A", "Bw")
  p <- PedonRecord$new(site = list(id = "bare"), horizons = h)
  expect_warning(
    u <- classify_with_uncertainty(p, n = 10, system = "wrb2022"),
    "perturbable")
  expect_equal(u$n_runs, 10L)
})

# ---- classification_robustness backward compatibility ---------------------

test_that("provenance_aware = FALSE reproduces the v0.9.42 robustness", {
  skip_on_cran()
  p <- make_ferralsol_canonical()
  default_call <- classification_robustness(p, system = "wrb2022",
                                            n = 40, seed = 7)
  explicit     <- classification_robustness(p, system = "wrb2022",
                                            n = 40, seed = 7,
                                            provenance_aware = FALSE)
  expect_identical(default_call, explicit)
})

test_that("provenance_aware = TRUE runs and returns a robustness fraction", {
  skip_on_cran()
  p <- make_ferralsol_canonical()
  r <- classification_robustness(p, system = "wrb2022", n = 40,
                                 seed = 7, provenance_aware = TRUE)
  expect_true(is.numeric(r$robustness))
  expect_gte(r$robustness, 0)
  expect_lte(r$robustness, 1)
})
