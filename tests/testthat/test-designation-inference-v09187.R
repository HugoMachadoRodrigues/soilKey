# =============================================================================
# v0.9.187 -- horizon-designation -> diagnostic morphological inference.
#
# Legacy archives record the field pedologist's horizon designation (Bt, Bw,
# Bi, ...) far more often than the quantitative lab attributes a diagnostic
# needs. When enabled (opt-in), the key uses the designation as morphological
# evidence so a well-designated but lab-sparse profile still reaches the right
# class instead of the catch-all. Default OFF => byte-identical classification.
# =============================================================================

mk <- function(h) PedonRecord$new(site = list(id = "t"), horizons = h)

# --- 1. the subordinate-letter mapping --------------------------------------
test_that(".designation_indicates maps subordinate letters to diagnostics", {
  di <- soilKey:::.designation_indicates
  expect_equal(di(c("Bt", "Bt2", "BA")),      c("argic", "argic", NA))
  expect_equal(di(c("Bw", "Bwo", "Bo")),      c("ferralic", "ferralic", "ferralic"))
  expect_equal(di("Bi"),                       "cambic")
  expect_equal(di(c("Bh", "Bs", "Bhs")),      c("spodic", "spodic", "spodic"))
  expect_equal(di(c("Bn", "Btn")),            c("natric", "natric"))   # sodium wins over t
  expect_equal(di(c("Bss", "Bv")),            c("vertic", "vertic"))   # slickensides
  # order-defining "t" beats a merely modifying plinthite/gleying letter
  expect_equal(di(c("Btf", "Btg")),           c("argic", "argic"))
  expect_equal(di(c("Bf", "Bg")),             c("plinthic", "gleyic")) # bare -> those
  # non-subsurface / transitional / undesignated -> NA
  expect_equal(di(c("Ap", "AB", "C", "Cr", "R", NA)),
               rep(NA_character_, 6))
})

# --- 2. default is OFF: no behavioural change --------------------------------
test_that("morphological inference is OFF by default", {
  expect_false(isTRUE(getOption("soilKey.morphological_inference", FALSE)))
})

test_that("with inference OFF, a Bt with no measured clay increase is not argic", {
  old <- options(soilKey.morphological_inference = FALSE); on.exit(options(old))
  p <- mk(data.frame(designation = c("Ap", "Bt"), top_cm = c(0, 20),
                     bottom_cm = c(20, 70), clay_pct = c(30, 34),
                     stringsAsFactors = FALSE))
  expect_false(isTRUE(argic(p)$passed))
})

# --- 3. inference ON: the designation carries the diagnostic -----------------
test_that("with inference ON, a Bt is accepted as argic by morphology", {
  old <- options(soilKey.morphological_inference = TRUE); on.exit(options(old))
  p <- mk(data.frame(designation = c("Ap", "Bt"), top_cm = c(0, 20),
                     bottom_cm = c(20, 70), clay_pct = c(30, 34),
                     stringsAsFactors = FALSE))
  r <- argic(p)
  expect_true(isTRUE(r$passed))
  expect_equal(r$evidence$morphological_inference$provenance,
               "morphological_designation")
})

test_that("a Bw with borderline CEC/clay is ferralic only when inference is ON", {
  # CEC/clay = 11/0.60 = 18.3 > 16 (soilkey ferralic ceiling) -> quantitative fails
  p <- mk(data.frame(designation = c("Ap", "Bw"), top_cm = c(0, 25),
                     bottom_cm = c(25, 90), clay_pct = c(45, 60),
                     cec_cmol = c(9, 11), stringsAsFactors = FALSE))
  old <- options(soilKey.morphological_inference = FALSE)
  expect_false(isTRUE(ferralic(p, engine = "soilkey")$passed))   # OFF: fails
  options(soilKey.morphological_inference = TRUE); on.exit(options(old))
  r <- ferralic(p, engine = "soilkey")
  expect_true(isTRUE(r$passed))                                   # ON: morphology
  expect_equal(r$evidence$morphological_inference$diagnostic, "ferralic")
})

# --- 4. it does not over-fire ------------------------------------------------
test_that("inference does not fire without a diagnostic B/E designation", {
  old <- options(soilKey.morphological_inference = TRUE); on.exit(options(old))
  # A over C, no diagnostic subsurface designation -> nothing to infer
  p <- mk(data.frame(designation = c("A", "C"), top_cm = c(0, 20),
                     bottom_cm = c(20, 80), clay_pct = c(12, 10),
                     stringsAsFactors = FALSE))
  expect_false(isTRUE(argic(p)$passed))
  expect_false(isTRUE(ferralic(p, engine = "soilkey")$passed))
})
