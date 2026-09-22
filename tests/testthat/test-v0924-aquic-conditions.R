# Tests for v0.9.24 aquic + oxyaquic tightening
# (KST 13ed Ch 3 pp 41-44 + Ch 14)

build_pedon_v0924 <- function(...) {
  hz <- data.table::data.table(...)
  hz <- ensure_horizon_schema(hz)
  PedonRecord$new(
    site = list(id = "v0924-test", lat = 0, lon = 0, country = "TEST",
                 parent_material = "test"),
    horizons = hz
  )
}


# ---- aquic_conditions_usda (KST 13ed Ch 3, pp 41-44) -----------------------
# v0.9.24 tightening: requires BOTH reduction evidence (chroma <= 2 OR
# 'g' designation) AND redox evidence (redox >= min OR chroma+g combo).

test_that("aquic_conditions: chroma + redox passes (canonical case)", {
  p <- build_pedon_v0924(
    top_cm                       = c(0,  20),
    bottom_cm                    = c(20, 50),
    designation                  = c("A", "Bg"),
    munsell_chroma_moist         = c(3,   2),
    redoximorphic_features_pct   = c(0,   10)
  )
  res <- aquic_conditions_usda(p, max_top_cm = 50, min_redox_pct = 5)
  expect_true(isTRUE(res$passed))
})

test_that("aquic_conditions: chroma <= 2 alone is NOT sufficient (no redox)", {
  # Pre-v0.9.24 logic accepted this. v0.9.24 requires redox evidence too.
  p <- build_pedon_v0924(
    top_cm                       = c(0,  20),
    bottom_cm                    = c(20, 50),
    designation                  = c("A",  "Bw"),  # no 'g' suffix
    munsell_chroma_moist         = c(3,    2),     # chroma OK
    redoximorphic_features_pct   = c(0,    0)      # NO redox features
  )
  res <- aquic_conditions_usda(p, max_top_cm = 50, min_redox_pct = 5)
  expect_false(isTRUE(res$passed))
})

test_that("aquic_conditions: redox >= 5pct alone is NOT sufficient (no reduction)", {
  # Pre-v0.9.24 logic accepted this. v0.9.24 requires reduction evidence.
  p <- build_pedon_v0924(
    top_cm                       = c(0,  20),
    bottom_cm                    = c(20, 50),
    designation                  = c("A",  "Bw"),  # no 'g' suffix
    munsell_chroma_moist         = c(4,    4),     # chroma too HIGH (no reduction)
    redoximorphic_features_pct   = c(0,    10)     # redox OK
  )
  res <- aquic_conditions_usda(p, max_top_cm = 50, min_redox_pct = 5)
  expect_false(isTRUE(res$passed))
})

test_that("aquic_conditions: g-designation + chroma <= 2 passes (combo evidence)", {
  # The 'g' suffix + chroma=2 combo serves as both reduction AND redox evidence.
  p <- build_pedon_v0924(
    top_cm                       = c(0,  20),
    bottom_cm                    = c(20, 50),
    designation                  = c("A",  "Bg"),  # 'g' suffix
    munsell_chroma_moist         = c(4,    2),     # chroma OK
    redoximorphic_features_pct   = c(0,    NA)     # redox MISSING
  )
  res <- aquic_conditions_usda(p, max_top_cm = 50, min_redox_pct = 5)
  expect_true(isTRUE(res$passed))
})

test_that("aquic_conditions: missing chroma + missing redox returns NA-style", {
  p <- build_pedon_v0924(
    top_cm                       = c(0,  20),
    bottom_cm                    = c(20, 50),
    designation                  = c("A",  "Bw"),
    munsell_chroma_moist         = c(NA_real_, NA_real_),
    redoximorphic_features_pct   = c(NA_real_, NA_real_)
  )
  res <- aquic_conditions_usda(p, max_top_cm = 50, min_redox_pct = 5)
  expect_false(isTRUE(res$passed))
  expect_true("munsell_chroma_moist" %in% res$missing ||
                "redoximorphic_features_pct" %in% res$missing)
})


# ---- oxyaquic_subgroup_usda (KST 13ed Ch 14) -------------------------------
# v0.9.24 tightening: requires (redox >= 2 AND chroma <= 4) OR
# (g-designation AND chroma <= 3). Single low-evidence trigger no longer fires.

test_that("oxyaquic: redox >= 2 + chroma <= 4 passes (clause a)", {
  p <- build_pedon_v0924(
    top_cm                       = c(0,  30),
    bottom_cm                    = c(30, 60),
    designation                  = c("A", "Bw"),
    munsell_chroma_moist         = c(4,   3),
    redoximorphic_features_pct   = c(0,   5)
  )
  res <- oxyaquic_subgroup_usda(p)
  expect_true(isTRUE(res$passed))
})

test_that("oxyaquic: g-designation + chroma <= 3 passes (clause b)", {
  p <- build_pedon_v0924(
    top_cm                       = c(0,  30),
    bottom_cm                    = c(30, 60),
    designation                  = c("A", "Bg"),
    munsell_chroma_moist         = c(4,   3),
    redoximorphic_features_pct   = c(0,   0)
  )
  res <- oxyaquic_subgroup_usda(p)
  expect_true(isTRUE(res$passed))
})

test_that("oxyaquic: chroma <= 2 alone is NOT sufficient (pre-v0.9.24 false-positive)", {
  p <- build_pedon_v0924(
    top_cm                       = c(0,  30),
    bottom_cm                    = c(30, 60),
    designation                  = c("A", "Bw"),  # no 'g'
    munsell_chroma_moist         = c(4,   2),     # chroma <= 2
    redoximorphic_features_pct   = c(0,   0)      # NO redox
  )
  res <- oxyaquic_subgroup_usda(p)
  expect_false(isTRUE(res$passed))
})

test_that("oxyaquic: redox >= 2 alone is NOT sufficient (pre-v0.9.24 false-positive)", {
  p <- build_pedon_v0924(
    top_cm                       = c(0,  30),
    bottom_cm                    = c(30, 60),
    designation                  = c("A", "Bw"),  # no 'g'
    munsell_chroma_moist         = c(4,   5),     # chroma > 4
    redoximorphic_features_pct   = c(0,   3)      # redox OK on its own
  )
  res <- oxyaquic_subgroup_usda(p)
  expect_false(isTRUE(res$passed))
})

test_that("oxyaquic: no candidate layers in upper 100 cm returns FALSE-not-NA", {
  p <- build_pedon_v0924(
    top_cm                       = c(120, 150),
    bottom_cm                    = c(150, 180),
    designation                  = c("BC", "C"),
    munsell_chroma_moist         = c(4,   3),
    redoximorphic_features_pct   = c(0,   5)
  )
  res <- oxyaquic_subgroup_usda(p)
  expect_false(isTRUE(res$passed))
  expect_true(grepl("no candidate", res$evidence$reason %||% ""))
})
