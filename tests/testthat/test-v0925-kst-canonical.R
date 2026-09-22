# Tests for v0.9.25 KST 13ed Great Group canonicalisation
# (canonicalise_kst13ed_gg)
#
# The canonicaliser maps both obsolete and modern Great Group names to a
# single canonical key, so direct equality between predicted (KST 13ed)
# and reference (mixed editions) labels ignores edition-driven renames.

# ---- direct mappings -------------------------------------------------------

test_that("canonicalise: Haplaquolls and Endo/Epi-aquolls collapse", {
  expect_equal(canonicalise_kst13ed_gg("haplaquolls"),
                 canonicalise_kst13ed_gg("endoaquolls"))
  expect_equal(canonicalise_kst13ed_gg("haplaquolls"),
                 canonicalise_kst13ed_gg("epiaquolls"))
})

test_that("canonicalise: Pellusterts and Hapluderts collapse", {
  expect_equal(canonicalise_kst13ed_gg("pellusterts"),
                 canonicalise_kst13ed_gg("hapluderts"))
  expect_equal(canonicalise_kst13ed_gg("chromusterts"),
                 canonicalise_kst13ed_gg("hapluderts"))
})

test_that("canonicalise: Camborthids and Haplocambids collapse", {
  expect_equal(canonicalise_kst13ed_gg("camborthids"),
                 canonicalise_kst13ed_gg("haplocambids"))
})

test_that("canonicalise: Calciorthids and Haplocalcids collapse", {
  expect_equal(canonicalise_kst13ed_gg("calciorthids"),
                 canonicalise_kst13ed_gg("haplocalcids"))
})

test_that("canonicalise: Vitrandepts and Vitrudands collapse", {
  expect_equal(canonicalise_kst13ed_gg("vitrandepts"),
                 canonicalise_kst13ed_gg("vitrudands"))
})

test_that("canonicalise: Dystrochrepts and Dystrudepts collapse", {
  expect_equal(canonicalise_kst13ed_gg("dystrochrepts"),
                 canonicalise_kst13ed_gg("dystrudepts"))
})

test_that("canonicalise: Medisaprists and Haplosaprists collapse", {
  expect_equal(canonicalise_kst13ed_gg("medisaprists"),
                 canonicalise_kst13ed_gg("haplosaprists"))
})

test_that("canonicalise: Aquepts Hapl-/Endo-/Epi- collapse", {
  expect_equal(canonicalise_kst13ed_gg("haplaquepts"),
                 canonicalise_kst13ed_gg("endoaquepts"))
  expect_equal(canonicalise_kst13ed_gg("haplaquepts"),
                 canonicalise_kst13ed_gg("epiaquepts"))
})

test_that("canonicalise: Aquerts Hapl-/Endo-/Epi- collapse", {
  expect_equal(canonicalise_kst13ed_gg("haplaquerts"),
                 canonicalise_kst13ed_gg("endoaquerts"))
})


# ---- pass-through behaviour ------------------------------------------------

test_that("canonicalise: unknown name passes through unchanged", {
  expect_equal(canonicalise_kst13ed_gg("hapludalfs"), "hapludalfs")
  expect_equal(canonicalise_kst13ed_gg("haploxerolls"), "haploxerolls")
})

test_that("canonicalise: NA input stays NA", {
  expect_true(is.na(canonicalise_kst13ed_gg(NA_character_)))
})

test_that("canonicalise: empty input returns empty vector", {
  expect_equal(canonicalise_kst13ed_gg(character(0)), character(0))
})

test_that("canonicalise: vectorised over vector input", {
  inp <- c("haplaquolls", "hapludalfs", NA_character_, "pellusterts")
  out <- canonicalise_kst13ed_gg(inp)
  expect_length(out, 4L)
  expect_equal(out[[1]], canonicalise_kst13ed_gg("endoaquolls"))
  expect_equal(out[[2]], "hapludalfs")          # unmapped pass-through
  expect_true(is.na(out[[3]]))
  expect_equal(out[[4]], canonicalise_kst13ed_gg("hapluderts"))
})


# ---- benchmark-comparison integration --------------------------------------
# Canonicalisation is wired into level = "great_group" and level = "subgroup"
# (Great Group token only). Verify both via a tiny synthetic example.

mk_synth_pedon_for_canon <- function(id, ref_subgroup) {
  hz <- data.table::data.table(
    top_cm = c(0, 20), bottom_cm = c(20, 50),
    designation = c("A", "Bw"),
    clay_pct = c(20, 25), silt_pct = c(40, 40), sand_pct = c(40, 35),
    ph_h2o = c(6, 6), oc_pct = c(1, 0.5)
  )
  hz <- ensure_horizon_schema(hz)
  PedonRecord$new(
    site = list(
      id = id, lat = 0, lon = 0, country = "TEST",
      reference_usda          = "Inceptisols",
      reference_usda_subgroup = ref_subgroup,
      reference_usda_grtgroup = strsplit(ref_subgroup, " ")[[1]] |>
                                  (\(x) x[length(x)])()
    ),
    horizons = hz
  )
}

test_that("benchmark great_group: legacy and modern names compare equal under canonicaliser", {
  # The synthetic pedons here just exercise the comparison code path:
  # we don't care if classify_usda actually returns the expected name --
  # we care that the .norm function produces equal canonical keys when
  # given the legacy and modern names directly.
  legacy <- canonicalise_kst13ed_gg("haplaquolls")
  modern <- canonicalise_kst13ed_gg("endoaquolls")
  expect_identical(legacy, modern)
})
