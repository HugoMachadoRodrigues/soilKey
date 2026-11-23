# v0.9.147 — USDA subgroup coverage slice (+35 criteria-exact intergrades)
#
# 35 missing subgroups whose every modifier maps to an EXISTING strict predicate
# (verified per-subgroup against ST_criteria_13th), generated append-before-default
# + first-match so they only ever refine a former Typic. KSSL-gated (0 worsened).

test_that("v0.9.147: USDA subgroup coverage (running total)", {
  skip_if_no_kst13()
  skip_on_cran()
  cov <- coverage_report("usda_subgroup")
  # 2003 + 35 (v0.9.147) + 11 (v0.9.149) + 105 (v0.9.422: the Alfisol lists
  # rebuilt from the KST, all 356 Alfisol subgroups registered)
  # + 77 (v0.9.423)
  # + 119 (v0.9.424)
  # + 169 (v0.9.425)
  # + 196 (v0.9.426)
  expect_equal(cov$overall$covered_n, 2715L)
  expect_equal(cov$overall$canonical_n, 2715L)
  expect_gt(cov$overall$pct, 75)
})

test_that("v0.9.147: the 35 added subgroups are registered", {
  skip_on_cran()
  reg <- soilKey:::.coverage_registered_usda_subgroups()
  added <- tolower(c(
    "Argic Petrocalcids", "Aridic Leptic Haplusterts",
    "Fragiaquic Palexeralfs", "Fragiaquic Haploxeralfs", "Fragiaquic Paleudalfs",
    "Fragiaquic Glossudalfs", "Fragiaquic Hapludalfs", "Fragiaquic Dystroxerepts",
    "Fragiaquic Eutrudepts", "Fragiaquic Dystrudepts", "Fragiaquic Kanhapludults",
    "Fragiaquic Paleudults", "Fragiaquic Hapludults",
    "Gypsic Calciustepts", "Gypsic Haplustepts", "Gypsic Haploxerepts",
    "Gypsic Calciustolls", "Gypsic Haplusterts",
    "Humic Durustands", "Humic Fragiaquepts", "Humic Densiaquepts",
    "Humic Gelaquepts", "Humic Cryaquepts", "Humic Sombriustox", "Humic Kandiustox",
    "Humic Sombriperox", "Humic Kandiperox", "Humic Sombriudox", "Humic Acrudox",
    "Humic Eutrudox", "Humic Kandiudox",
    "Plinthic Quartzipsamments",
    "Spodic Humicryepts", "Spodic Dystrocryepts", "Spodic Dystrudepts"))
  expect_true(all(added %in% reg))
  expect_length(added, 35L)
})

test_that("v0.9.147: USDA rule base still loads cleanly with the additions", {
  skip_on_cran()
  expect_silent(suppressMessages(load_rules("usda")))
})
