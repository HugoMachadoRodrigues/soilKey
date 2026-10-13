# v0.9.247: the "ir" of the older surveys is a B espodico in the
# morphological mode.
#
# The surveys that predate the present notation write the illuvial iron of a
# B horizon "ir" (Bir, B2ir, B22irx, Bhir), where the present notation writes
# "s". The morphological mode read "h" and "s" as the B espodico; a B with
# "ir" and no "h" was not read. In the national BDsolos export 26 profiles
# carry "ir" in a designation, all of them with a reference in Espodossolos.

.ir_morph <- function(on) options(soilKey.morphological_inference = on)

.ir_profile <- function(designation, top = c(0, 15, 60, 90), bottom = c(15, 60, 90, 150)) {
  n <- length(designation)
  hz <- data.table::data.table(
    designation = designation, top_cm = top[seq_len(n)], bottom_cm = bottom[seq_len(n)],
    clay_pct = c(3, 1, 4, 3)[seq_len(n)], silt_pct = 3,
    sand_pct = 97 - c(3, 1, 4, 3)[seq_len(n)])
  PedonRecord$new(site = list(id = "ir", country = "BR"),
                  horizons = ensure_horizon_schema(hz))
}

test_that("a B with 'ir' is a B espodico in the morphological mode", {
  old <- .ir_morph(TRUE); on.exit(options(old), add = TRUE)
  p <- .ir_profile(c("A1", "A2", "B2ir", "C"))
  r <- B_espodico(p)
  expect_true(isTRUE(r$passed))
  expect_equal(r$layers, 3L)
  expect_equal(r$evidence$morphological_inference$provenance, "morphological_designation")
  expect_equal(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Espodossolos")
})

test_that("the designations of the export are read", {
  old <- .ir_morph(TRUE); on.exit(options(old), add = TRUE)
  for (d in c("Bir", "B2ir", "B21ir", "B22irx", "B21irh", "IIB23ir", "B'ir"))
    expect_true(isTRUE(B_espodico(.ir_profile(c("A1", "A2", d, "C")))$passed), info = d)
  # more than one layer
  r <- B_espodico(.ir_profile(c("A1", "A2", "B21ir", "B22ir")))
  expect_equal(r$layers, 3:4)
})

test_that("a B incipiente, a Bw and a C with the letters are not", {
  old <- .ir_morph(TRUE); on.exit(options(old), add = TRUE)
  for (d in c("Bi", "Bi2", "Bw", "Cir"))
    expect_false(isTRUE(B_espodico(.ir_profile(c("A1", "A2", d, "C")))$passed), info = d)
})

test_that("the default mode does not read the letters", {
  old <- .ir_morph(FALSE); on.exit(options(old), add = TRUE)
  p <- .ir_profile(c("A1", "A2", "B2ir", "C"))
  expect_false(isTRUE(B_espodico(p)$passed))
  expect_false(identical(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Espodossolos"))
})

test_that("the letters 'h' and 's' are read as before, and the canonical Espodossolo keeps its class", {
  old <- .ir_morph(TRUE); on.exit(options(old), add = TRUE)
  for (d in c("Bh", "Bs", "Bhs", "Bhir"))
    expect_true(isTRUE(B_espodico(.ir_profile(c("A1", "E", d, "C")))$passed), info = d)
  .ir_morph(FALSE)
  expect_true(isTRUE(B_espodico(make_espodossolo_canonical())$passed))
  expect_equal(classify_sibcs(make_espodossolo_canonical(), on_missing = "silent")$rsg_or_order,
               "Espodossolos")
})
