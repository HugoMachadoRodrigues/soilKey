# v0.9.151: every USDA subgroup `code` must be unique across the rule base.
#
# The coverage-slice generators (v0.9.113/121/123/147) minted codes without
# intra-batch reservation, so 47 duplicate codes accumulated across 5 order
# files (e.g. KFGN for both Spodic and Fragiaquic Dystrudepts). Codes are
# internal ids (comparison is by NAME, never by code), so this never affected
# classification or coverage -- but a duplicate id is a latent wart. This guard
# keeps them unique.

test_that("USDA subgroup codes are globally unique across the rule base", {
  dir <- system.file("rules", "usda", "subgroups", package = "soilKey")
  if (!nzchar(dir) || !dir.exists(dir))
    dir <- file.path("inst", "rules", "usda", "subgroups")
  skip_if_not(dir.exists(dir))

  codes <- unlist(lapply(list.files(dir, pattern = "\\.yaml$", full.names = TRUE),
    function(f) {
      sg <- yaml::read_yaml(f)$subgroups
      unlist(lapply(sg, function(block) vapply(block, `[[`, character(1), "code")),
             use.names = FALSE)
    }), use.names = FALSE)

  expect_gt(length(codes), 2000L)
  dups <- unique(codes[duplicated(codes)])
  expect_identical(dups, character(0))            # no duplicate code anywhere
  expect_true(all(nchar(codes) == 4L))            # all canonical 4-char ids
})
