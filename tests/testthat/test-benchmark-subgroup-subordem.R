# v0.9.22 -- benchmark_run_classification level="subgroup" / "subordem"
# tests. The runner needs to:
#  - case-insensitively normalise both ref and pred
#  - strip qualifier parens
#  - take first 2 tokens for SiBCS subordem
#  - read reference_usda_subgroup when level="subgroup" + system="usda"


make_test_pedons <- function(refs_subgroup,
                                refs_order = NULL,
                                preds      = NULL) {
  if (is.null(refs_order))
    refs_order <- vapply(refs_subgroup, function(x) {
      toks <- strsplit(x, " ")[[1]]
      tolower(toks[length(toks)])
    }, character(1))
  out <- vector("list", length(refs_subgroup))
  for (i in seq_along(refs_subgroup)) {
    p <- make_ferralsol_canonical()
    p$site$reference_usda          <- refs_order[i]
    p$site$reference_usda_subgroup <- refs_subgroup[i]
    out[[i]] <- p
  }
  out
}


test_that("benchmark level='subgroup' uses reference_usda_subgroup", {
  # Make 4 fake pedons. classify_usda on the canonical Ferralsol
  # always returns "Rhodic Hapludox" / "Oxisols". Three references
  # match (after lowercasing); one doesn't.
  peds <- make_test_pedons(
    refs_subgroup = c("Rhodic Hapludox",
                       "rhodic hapludox",
                       "RHODIC HAPLUDOX",
                       "Typic Hapludalfs"))
  res <- benchmark_run_classification(peds, system = "usda",
                                          level = "subgroup",
                                          boot_n = 50L)
  # 3/4 should match after case normalisation.
  expect_equal(res$n_evaluated, 4L)
  expect_equal(res$accuracy_top1, 0.75)
})


test_that("benchmark level='subordem' compares first 2 tokens (SiBCS)", {
  # When the surveyor labels at SiBCS Subordem level
  # ("Latossolos Vermelhos") and the classifier emits the full
  # 4-token name ("Latossolos Vermelhos Distroficos tipicos"), the
  # subordem benchmark takes the first 2 tokens of the prediction
  # and matches.
  peds <- make_test_pedons(
    refs_subgroup = c("Latossolos Vermelhos", "Latossolos Amarelos"))
  for (p in peds) p$site$reference_sibcs <- p$site$reference_usda_subgroup
  # Hack: replace classify_sibcs return via the pedon site so the
  # benchmark sees a reproducible pred.
  # Use canonical Ferralsol fixture; classify_sibcs returns
  # "Latossolos Vermelhos Distroficos tipicos".
  res <- benchmark_run_classification(peds, system = "sibcs",
                                          level = "subordem",
                                          boot_n = 50L)
  expect_equal(res$n_evaluated, 2L)
  # Pedon 1: ref "Latossolos Vermelhos" matches first-2-tokens
  # "latossolos vermelhos" of the predicted full name -> hit.
  # Pedon 2: ref "Latossolos Amarelos" mismatches -> miss.
  expect_equal(res$accuracy_top1, 0.5)
})


test_that("benchmark level='order' comparison still works (no regression)", {
  peds <- make_test_pedons(
    refs_subgroup = c("Rhodic Hapludox"))
  peds[[1]]$site$reference_usda <- "Oxisols"
  res <- benchmark_run_classification(peds, system = "usda",
                                          level = "order",
                                          boot_n = 50L)
  expect_equal(res$accuracy_top1, 1.0)
})


test_that("normalise_kssl_subgroup is idempotent and handles whitespace", {
  expect_equal(normalise_kssl_subgroup("Typic Hapludalfs"),
                 "typic hapludalfs")
  expect_equal(normalise_kssl_subgroup("  TYPIC  HAPLUDALFS  "),
                 "typic hapludalfs")
  expect_equal(normalise_kssl_subgroup(c("Aquic Argiudolls", NA, "")),
                 c("aquic argiudolls", NA, NA))
})
