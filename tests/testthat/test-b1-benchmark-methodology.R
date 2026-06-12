# Tests for the v0.9.110 "benchmark methodology" front (B1). All offline and
# synthetic -- no soil_data/ (gitignored, absent on CI). Covers the sampling
# filter-then-cap fix, the confusion-matrix metrics, the reproducible bootstrap
# CIs, and the widened report renderer.

# ---- helpers ---------------------------------------------------------------

.b1_mk_pedon <- function(id, ref_field = NULL, value = "X") {
  hz   <- data.frame(top_cm = 0, bottom_cm = 20, designation = "A")
  site <- list(id = id)
  if (!is.null(ref_field)) site[[ref_field]] <- value
  soilKey::PedonRecord$new(site = site, horizons = hz)
}

.b1_cm <- function(m, labs) {
  as.table(as.matrix(matrix(m, length(labs), length(labs), byrow = TRUE,
                            dimnames = list(reference = labs, predicted = labs))))
}

# ---- Step 1: filter-then-cap ----------------------------------------------

test_that(".benchmark_has_reference detects only a usable label", {
  has <- soilKey:::.benchmark_has_reference
  expect_true(has(.b1_mk_pedon("p", "reference_usda", "Hapludox"), "usda"))
  expect_false(has(.b1_mk_pedon("p"), "usda"))                       # no field
  expect_false(has(.b1_mk_pedon("p", "reference_usda", NA), "usda")) # NA
  expect_false(has(.b1_mk_pedon("p", "reference_usda", "  "), "usda")) # blank
  # right field per system
  expect_true(has(.b1_mk_pedon("p", "reference_wrb", "Ferralsol"), "wrb2022"))
  expect_false(has(.b1_mk_pedon("p", "reference_wrb", "Ferralsol"), "usda"))
})

test_that(".benchmark_filter_then_cap filters to the label THEN caps", {
  fcap <- soilKey:::.benchmark_filter_then_cap
  peds <- c(lapply(1:18, function(i) .b1_mk_pedon(paste0("p", i),
                                                  "reference_usda", "Hapludox")),
            lapply(19:20, function(i) .b1_mk_pedon(paste0("p", i))))
  expect_length(fcap(peds, "usda", 50),   18L)   # cap above n -> all labelled
  expect_length(fcap(peds, "usda", 10),   10L)   # cap below n
  expect_length(fcap(peds, "usda", NULL), 18L)   # no cap
  expect_length(fcap(peds, "wrb2022", 50), 0L)   # none carry a WRB label
})

test_that(".benchmark_normalise_febr_ref reduces WRB/USDA labels to order level", {
  nrm <- soilKey:::.benchmark_normalise_febr_ref
  # raw FEBR WRB/USDA labels are full names that never match the predicted RSG
  p_wrb <- .b1_mk_pedon("p1", "reference_wrb", "HAPLIC ACRISOL (ALUMIC, CHROMIC)")
  expect_equal(nrm(list(p_wrb), "wrb2022")[[1]]$site$reference_wrb, "Acrisols")
  p_usda <- .b1_mk_pedon("p2", "reference_usda", "XANTHIC KANDIUDOX")
  expect_equal(nrm(list(p_usda), "usda")[[1]]$site$reference_usda, "Oxisols")
  # SiBCS is a no-op here (benchmark_run_classification canonicalises it)
  p_sib <- .b1_mk_pedon("p3", "reference_sibcs", "LATOSSOLO VERMELHO")
  expect_equal(nrm(list(p_sib), "sibcs")[[1]]$site$reference_sibcs,
               "LATOSSOLO VERMELHO")
})

test_that("filter-then-cap is reproducible and RNG-state-preserving", {
  fcap <- soilKey:::.benchmark_filter_then_cap
  peds <- lapply(1:50, function(i) .b1_mk_pedon(paste0("p", i),
                                                "reference_sibcs", "Latossolos"))
  a <- vapply(fcap(peds, "sibcs", 10), function(p) p$site$id, character(1))
  b <- vapply(fcap(peds, "sibcs", 10), function(p) p$site$id, character(1))
  expect_identical(a, b)                      # same internal seed -> same draw
  expect_length(a, 10L)
  # RNG-state-preserving: calling fcap must not advance the caller's stream.
  set.seed(7); s0 <- get(".Random.seed", envir = .GlobalEnv)
  invisible(fcap(peds, "sibcs", 10))
  s1 <- get(".Random.seed", envir = .GlobalEnv)
  expect_identical(s0, s1)
})

# ---- Step 2: metrics from confusion ---------------------------------------

test_that(".benchmark_metrics_from_confusion is exact on a perfect diagonal", {
  mfc <- soilKey:::.benchmark_metrics_from_confusion
  cm  <- .b1_cm(c(5,0,0, 0,5,0, 0,0,5), c("A","B","C"))
  m   <- mfc(cm)
  expect_equal(m$accuracy, 1)
  expect_equal(m$kappa, 1)
  expect_equal(m$balanced_accuracy, 1)
  expect_equal(m$macro_f1, 1)
  expect_equal(m$nir, 1/3)
  expect_equal(m$n, 15L)
})

test_that("metrics are exact on an imbalanced 2-class confusion", {
  mfc <- soilKey:::.benchmark_metrics_from_confusion
  # ref A = 8 (6 correct, 2 -> B); ref B = 2 (1 correct, 1 -> A)
  cm  <- .b1_cm(c(6,2, 1,1), c("A","B"))
  m   <- mfc(cm)
  expect_equal(m$accuracy, 0.7)                       # 7/10
  expect_equal(m$nir, 0.8)                            # majority ref = A (8/10)
  pcA <- m$per_class[m$per_class$class == "A", ]
  pcB <- m$per_class[m$per_class$class == "B", ]
  expect_equal(pcA$precision, 6/7); expect_equal(pcA$recall, 6/8)
  expect_equal(pcB$precision, 1/3); expect_equal(pcB$recall, 1/2)
  expect_equal(m$balanced_accuracy, mean(c(6/8, 1/2)))
})

test_that("metrics are NaN-safe for zero-support and zero-prediction classes", {
  mfc <- soilKey:::.benchmark_metrics_from_confusion
  # class C is predicted (col) but never referenced (row) -> zero support
  cm <- as.table(as.matrix(matrix(c(4,0,1, 0,3,0), 2, 3, byrow = TRUE,
            dimnames = list(reference = c("A","B"),
                            predicted = c("A","B","C")))))
  m  <- mfc(cm)
  expect_false(any(is.nan(unlist(m$per_class[c("precision","recall","f1")]))))
  # macro-F1 / balanced-acc average only over reference-present classes (A,B)
  expect_equal(m$macro_f1, mean(c(8/9, 1)))   # F1(A)=2*.8*1/(1.8)=0.888.., F1(B)=1
  pcC <- m$per_class[m$per_class$class == "C", ]
  expect_equal(pcC$support, 0L); expect_equal(pcC$precision, 0)
})

test_that("metrics handle NULL / empty input", {
  mfc <- soilKey:::.benchmark_metrics_from_confusion
  expect_null(mfc(NULL))
})

# ---- Step 2: reproducible bootstrap CIs -----------------------------------

test_that("bootstrap CIs bracket the point, are reproducible and RNG-safe", {
  mfc  <- soilKey:::.benchmark_metrics_from_confusion
  boot <- soilKey:::.benchmark_bootstrap_metrics
  cm   <- .b1_cm(c(40,5,5, 3,42,5, 6,4,40), c("A","B","C"))
  pt   <- mfc(cm)
  ci1  <- boot(cm, B = 400L)
  ci2  <- boot(cm, B = 400L)
  expect_identical(ci1, ci2)                          # reproducible (seed 42)
  expect_true(ci1$accuracy[1] <= pt$accuracy &&
                pt$accuracy <= ci1$accuracy[2])
  expect_true(ci1$macro_f1[1] <= pt$macro_f1 &&
                pt$macro_f1 <= ci1$macro_f1[2])
  # RNG-state-preserving: bootstrap must not advance the caller's stream.
  set.seed(99); s0 <- get(".Random.seed", envir = .GlobalEnv)
  invisible(boot(cm, B = 50L))
  s1 <- get(".Random.seed", envir = .GlobalEnv)
  expect_identical(s0, s1)
})

test_that("bootstrap returns NA CIs on degenerate input", {
  boot <- soilKey:::.benchmark_bootstrap_metrics
  expect_true(all(is.na(boot(NULL)$accuracy)))
  one <- .b1_cm(c(3,0, 0,0), c("A","B"))              # single reference class
  expect_true(all(is.na(boot(one)$accuracy)))
})

# ---- Step 3: report writer (flags + new columns) --------------------------

test_that(".suite_report_md renders the new metric columns and flags", {
  srow <- soilKey:::.suite_row
  smd  <- soilKey:::.suite_report_md
  cm   <- .b1_cm(c(40,5,5, 3,42,5, 6,4,40), c("A","B","C"))
  small <- .b1_cm(c(3,1, 0,1), c("A","B"))
  rows <- rbind(
    srow("febr", "wrb2022", 200, 0.81, confusion = cm),
    srow("febr", "usda", 5, 0.80, confusion = small),       # n<30
    srow("lucas_esdb", "wrb2022", 200, 0.03, confusion = NULL),
    srow("canonical", "all", 132, 1.0, confusion = NULL))
  # back-compat: the four original columns still present
  expect_true(all(c("dataset","system","n_compared","accuracy") %in% names(rows)))
  md <- smd(rows, weak = list(),
            config = list(soilKey_version = "0.9.110", max_n = 200,
                          level = "order"))
  expect_true(any(grepl("benchmark suite", md)))      # title kept (compat)
  expect_true(any(grepl("Accuracy by dataset", md)))  # section kept (compat)
  expect_true(any(grepl("Macro-F1", md)))
  expect_true(any(grepl("Kappa", md)))
  expect_true(any(grepl("NIR", md)))
  expect_true(any(grepl("n<30", md)))                 # small-n flag
  expect_true(any(grepl("lower-bound", md)))          # LUCAS flag
})

test_that(".suite_row keeps the displayed accuracy consistent with its CI", {
  srow <- soilKey:::.suite_row
  cm   <- .b1_cm(c(40,5,5, 3,42,5, 6,4,40), c("A","B","C"))
  # accuracy passed deliberately wrong (0.10) -> overridden by cm-derived 0.81
  r    <- srow("febr", "wrb2022", 200, 0.10, confusion = cm)
  expect_equal(round(r$accuracy, 3), round(122 / 150, 3))
  expect_true(r$acc_lo <= r$accuracy && r$accuracy <= r$acc_hi)
})
