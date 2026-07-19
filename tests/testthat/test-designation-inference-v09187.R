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
  # v0.9.188: Bw/Bo is region-aware -- cambic by default (temperate/unknown),
  # latossolic-ferralic only when the pedon is tropical.
  expect_equal(di(c("Bw", "Bwo", "Bo")),      c("cambic", "cambic", "cambic"))
  expect_equal(di(c("Bw", "Bwo", "Bo"), tropical = TRUE),
               c("ferralic", "ferralic", "ferralic"))
  expect_equal(di(c("Bw", "Bwo", "Bo"), tropical = FALSE),
               c("cambic", "cambic", "cambic"))
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

test_that("a tropical Bw with borderline CEC/clay is ferralic only when inference is ON", {
  # CEC/clay = 11/0.60 = 18.3 > 16 (soilkey ferralic ceiling) -> quantitative fails
  # v0.9.188: ferralic reading of a Bw requires a tropical regime -> country = BR.
  hz <- data.frame(designation = c("Ap", "Bw"), top_cm = c(0, 25),
                   bottom_cm = c(25, 90), clay_pct = c(45, 60),
                   cec_cmol = c(9, 11), stringsAsFactors = FALSE)
  p <- PedonRecord$new(site = list(id = "t", country = "BR"), horizons = hz)
  old <- options(soilKey.morphological_inference = FALSE)
  expect_false(isTRUE(ferralic(p, engine = "soilkey")$passed))   # OFF: fails
  options(soilKey.morphological_inference = TRUE); on.exit(options(old))
  r <- ferralic(p, engine = "soilkey")
  expect_true(isTRUE(r$passed))                                   # ON: morphology
  expect_equal(r$evidence$morphological_inference$diagnostic, "ferralic")
})

test_that("a temperate Bw is NOT read as ferralic even with inference ON (region-aware)", {
  # v0.9.188: identical borderline Bw, but an Austrian (temperate) site -> the
  # Bw is the cambic B, so the ferralic morphological inference must NOT fire.
  hz <- data.frame(designation = c("Ap", "Bw"), top_cm = c(0, 25),
                   bottom_cm = c(25, 90), clay_pct = c(45, 60),
                   cec_cmol = c(9, 11), stringsAsFactors = FALSE)
  p <- PedonRecord$new(site = list(id = "t", country = "AT", lat = 47.3),
                       horizons = hz)
  old <- options(soilKey.morphological_inference = TRUE); on.exit(options(old))
  expect_false(isTRUE(ferralic(p, engine = "soilkey")$passed))
  # and the canonical temperate Cambisol keys to Cambisol under inference ON
  expect_equal(classify_wrb2022(make_cambisol_canonical())$rsg_or_order,
               "Cambisols")
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

# --- 5. the same mechanism reaches spodic / gleyic / plinthic ----------------
test_that("inference ON reaches spodic (Bh), gleyic (Bg) and plinthic (Bf)", {
  old <- options(soilKey.morphological_inference = TRUE); on.exit(options(old))

  sp <- mk(data.frame(designation = c("E", "Bh"), top_cm = c(0, 20),
                      bottom_cm = c(20, 60), clay_pct = c(8, 10),
                      stringsAsFactors = FALSE))
  expect_true(isTRUE(spodic(sp)$passed))

  gl <- mk(data.frame(designation = c("Ap", "Bg"), top_cm = c(0, 20),
                      bottom_cm = c(20, 70), clay_pct = c(20, 22),
                      stringsAsFactors = FALSE))
  expect_true(isTRUE(gleyic_properties(gl)$passed))

  pl <- mk(data.frame(designation = c("Ap", "Bf"), top_cm = c(0, 20),
                      bottom_cm = c(20, 80), clay_pct = c(25, 30),
                      stringsAsFactors = FALSE))
  expect_true(isTRUE(plinthic(pl)$passed))
})

test_that("those diagnostics stay OFF by default (byte-identical)", {
  old <- options(soilKey.morphological_inference = FALSE); on.exit(options(old))
  sp <- mk(data.frame(designation = c("E", "Bh"), top_cm = c(0, 20),
                      bottom_cm = c(20, 60), clay_pct = c(8, 10),
                      stringsAsFactors = FALSE))
  expect_false(isTRUE(spodic(sp)$passed))
})
