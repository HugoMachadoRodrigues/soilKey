# Tests for the v0.9.98 WRB Tier-3 RSG-gate strict mode.
#
# Strict mode is opt-in: with strict = FALSE (the default) every gate
# behaves exactly as in v0.9.97. With strict = TRUE a per-RSG numerical
# threshold is tightened. These tests verify both halves: backward
# compatibility AND that strict mode actually changes borderline outcomes.

mk <- function(fixture) {
  get(paste0("make_", fixture, "_canonical"), envir = asNamespace("soilKey"))()
}

affected <- c("vertisol", "andosol", "gleysol", "planosol",
              "ferralsol", "chernozem", "kastanozem")

# ---- backward compatibility ------------------------------------------------

test_that("strict = FALSE reproduces the v0.9.97 default classification", {
  for (nm in affected) {
    p <- mk(nm)
    default_arg <- classify_wrb2022(p, on_missing = "silent")$rsg_or_order
    explicit    <- classify_wrb2022(p, on_missing = "silent",
                                    strict = FALSE)$rsg_or_order
    expect_identical(default_arg, explicit,
                     info = paste("fixture", nm))
  }
})

test_that("canonical fixtures classify identically under strict mode", {
  # Strict mode strengthens borderline gates; a textbook canonical
  # profile must still key to its own RSG.
  for (nm in affected) {
    p <- mk(nm)
    base   <- classify_wrb2022(p, on_missing = "silent", strict = FALSE)
    strict <- classify_wrb2022(p, on_missing = "silent", strict = TRUE)
    expect_identical(base$rsg_or_order, strict$rsg_or_order,
                     info = paste("fixture", nm))
  }
})

# ---- every gate accepts `strict` and records it ---------------------------

test_that("all seven RSG gates accept strict and record strict_mode", {
  gates <- list(
    vertisol         = mk("vertisol"),
    andosol          = mk("andosol"),
    gleysol          = mk("gleysol"),
    planosol         = mk("planosol"),
    ferralsol        = mk("ferralsol"),
    chernozem_strict = mk("chernozem"),
    kastanozem_strict = mk("kastanozem")
  )
  for (fn in names(gates)) {
    f <- get(fn, envir = asNamespace("soilKey"))
    res_f <- f(gates[[fn]], strict = FALSE)
    res_t <- f(gates[[fn]], strict = TRUE)
    expect_s3_class(res_f, "DiagnosticResult")
    expect_false(isTRUE(res_f$evidence$strict_mode), info = fn)
    expect_true(isTRUE(res_t$evidence$strict_mode),  info = fn)
  }
})

# ---- borderline profiles flip under strict --------------------------------

test_that("Vertisol gate: 32% clay passes default, fails strict", {
  p <- mk("vertisol")
  p$horizons$clay_pct <- rep(32, nrow(p$horizons))
  expect_true(isTRUE(vertisol(p, strict = FALSE)$passed))
  expect_false(isTRUE(vertisol(p, strict = TRUE)$passed))
})

test_that("Chernozem gate: BS 65% passes default, fails strict", {
  p <- mk("chernozem")
  p$horizons$bs_pct <- c(65, 65, 65, 97, 95)
  expect_true(isTRUE(chernozem_strict(p, strict = FALSE)$passed))
  expect_false(isTRUE(chernozem_strict(p, strict = TRUE)$passed))
})

test_that("Kastanozem gate: BS 60% passes default, fails strict", {
  p <- mk("kastanozem")
  p$horizons$bs_pct <- c(60, 60, 97, 99)
  expect_true(isTRUE(kastanozem_strict(p, strict = FALSE)$passed))
  expect_false(isTRUE(kastanozem_strict(p, strict = TRUE)$passed))
})

test_that("Gleysol gate: gleying from 30 cm passes default, fails strict", {
  p <- mk("gleysol")
  p$horizons$top_cm    <- c(0, 30, 60, 110)
  p$horizons$bottom_cm <- c(30, 60, 110, 150)
  expect_true(isTRUE(gleysol(p, strict = FALSE)$passed))
  expect_false(isTRUE(gleysol(p, strict = TRUE)$passed))
})

test_that("Ferralsol gate strict requires two argic exception paths", {
  # Canonical Ferralsol has no argic above the ferralic, so the gate
  # passes in both modes; the strict tightening is recorded in evidence.
  p <- mk("ferralsol")
  expect_true(isTRUE(ferralsol(p, strict = FALSE)$passed))
  expect_true(isTRUE(ferralsol(p, strict = TRUE)$passed))
})

test_that("a borderline Vertisol flips RSG through classify_wrb2022", {
  p <- mk("vertisol")
  p$horizons$clay_pct <- rep(32, nrow(p$horizons))
  default_rsg <- classify_wrb2022(p, on_missing = "silent",
                                  strict = FALSE)$rsg_or_order
  strict_rsg  <- classify_wrb2022(p, on_missing = "silent",
                                  strict = TRUE)$rsg_or_order
  expect_identical(default_rsg, "Vertisols")
  expect_false(identical(strict_rsg, "Vertisols"))
})

# ---- the global option pathway --------------------------------------------

test_that("getOption('soilKey.rsg_strict') drives the gates", {
  p <- mk("vertisol")
  p$horizons$clay_pct <- rep(32, nrow(p$horizons))
  old <- getOption("soilKey.rsg_strict")
  on.exit(options(soilKey.rsg_strict = old), add = TRUE)

  options(soilKey.rsg_strict = TRUE)
  expect_identical(vertisol(p)$passed, vertisol(p, strict = TRUE)$passed)

  options(soilKey.rsg_strict = FALSE)
  expect_identical(vertisol(p)$passed, vertisol(p, strict = FALSE)$passed)
})

test_that("classify_wrb2022 restores the option after the call", {
  p <- mk("vertisol")
  old <- getOption("soilKey.rsg_strict")
  on.exit(options(soilKey.rsg_strict = old), add = TRUE)

  options(soilKey.rsg_strict = FALSE)
  classify_wrb2022(p, on_missing = "silent", strict = TRUE)
  expect_false(isTRUE(getOption("soilKey.rsg_strict")))
})
