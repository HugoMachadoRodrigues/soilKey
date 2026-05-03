# Tests for v0.9.24 multi-level USDA benchmark infrastructure
# (great_group + suborder levels in benchmark_run_classification)

# ---- .gg_to_suborder helper ------------------------------------------------
# Maps Great Group -> Suborder by canonical KST 13ed Ch 4 suffix.

test_that(".gg_to_suborder resolves canonical Mollisols Great Groups", {
  expect_equal(soilKey:::.gg_to_suborder("hapludolls"),    "udolls")
  expect_equal(soilKey:::.gg_to_suborder("calciaquolls"),  "aquolls")
  expect_equal(soilKey:::.gg_to_suborder("haplocryolls"),  "cryolls")
  expect_equal(soilKey:::.gg_to_suborder("argiustolls"),   "ustolls")
})

test_that(".gg_to_suborder resolves canonical Alfisols Great Groups", {
  expect_equal(soilKey:::.gg_to_suborder("hapludalfs"),    "udalfs")
  expect_equal(soilKey:::.gg_to_suborder("natraqualfs"),   "aqualfs")
  expect_equal(soilKey:::.gg_to_suborder("glossustalfs"),  "ustalfs")
})

test_that(".gg_to_suborder resolves Inceptisols + Entisols + Spodosols", {
  expect_equal(soilKey:::.gg_to_suborder("dystrudepts"), "udepts")
  expect_equal(soilKey:::.gg_to_suborder("eutrudepts"),  "udepts")
  expect_equal(soilKey:::.gg_to_suborder("fluvaquents"), "aquents")
  expect_equal(soilKey:::.gg_to_suborder("haplorthods"), "orthods")
})

test_that(".gg_to_suborder is vectorised", {
  res <- soilKey:::.gg_to_suborder(c("hapludalfs", "fluvaquents",
                                       "haplorthods", "haplustox"))
  expect_equal(res, c("udalfs", "aquents", "orthods", "ustox"))
})

test_that(".gg_to_suborder returns NA for unrecognised input", {
  expect_true(is.na(soilKey:::.gg_to_suborder("nonexistentgroup")))
  expect_true(is.na(soilKey:::.gg_to_suborder(NA_character_)))
})

test_that(".gg_to_suborder handles empty input", {
  expect_equal(soilKey:::.gg_to_suborder(character(0)), character(0))
})


# ---- benchmark_run_classification: great_group + suborder levels -----------
# Smoke tests: ensure the new levels accept their inputs and return the
# expected list shape. Uses synthetic minimal pedons -- not testing the
# classifier itself, only the level-comparison machinery.

mk_synth_pedon_with_subgroup_ref <- function(id, ref_subgroup) {
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
                                  (\(x) x[length(x)])(),
      reference_usda_suborder = soilKey:::.gg_to_suborder(
        strsplit(ref_subgroup, " ")[[1]] |>
          (\(x) x[length(x)])()
      )
    ),
    horizons = hz
  )
}

test_that("benchmark_run_classification accepts level='great_group'", {
  peds <- list(
    mk_synth_pedon_with_subgroup_ref("p1", "typic dystrudepts"),
    mk_synth_pedon_with_subgroup_ref("p2", "aquic eutrudepts")
  )
  res <- benchmark_run_classification(peds, system = "usda",
                                         level = "great_group",
                                         boot_n = 50L)
  expect_true("accuracy_top1" %in% names(res))
  expect_true("accuracy_ci"   %in% names(res))
  expect_true("confusion"     %in% names(res))
  expect_true("per_pedon"     %in% names(res))
  expect_equal(res$level, "great_group")
})

test_that("benchmark_run_classification accepts level='suborder'", {
  peds <- list(
    mk_synth_pedon_with_subgroup_ref("p1", "typic dystrudepts"),
    mk_synth_pedon_with_subgroup_ref("p2", "aquic eutrudepts")
  )
  res <- benchmark_run_classification(peds, system = "usda",
                                         level = "suborder",
                                         boot_n = 50L)
  expect_true("accuracy_top1" %in% names(res))
  expect_equal(res$level, "suborder")
})

test_that("benchmark_run_classification rejects invalid level", {
  peds <- list(mk_synth_pedon_with_subgroup_ref("p1", "typic dystrudepts"))
  expect_error(
    benchmark_run_classification(peds, system = "usda", level = "bogus_level",
                                    boot_n = 50L),
    regexp = "should be one of"
  )
})
