# Tests for v0.9.113 honest taxonomic-completeness measurement
# (coverage_report) and the 4 new byte-identical WRB qualifiers
# (Aeolic / Fragic / Limonic / Tsitelic).

test_that("coverage_report(usda_subgroup) measures by name against KST 13ed", {
  skip_on_cran()
  cov <- coverage_report("usda_subgroup")
  expect_type(cov, "list")
  expect_named(cov, c("overall", "by_group", "missing", "extra"))
  o <- cov$overall
  expect_equal(o$system, "usda")
  expect_equal(o$level, "subgroup")
  # canonical set is the full 13th-edition subgroup count
  expect_gt(o$canonical_n, 2700)
  # registered after the v0.9.113 generator is well above the pre-existing ~47%
  expect_gt(o$pct, 70)
  expect_equal(o$covered_n + o$missing_n, o$canonical_n)
  # per-order rows sum back to the overall canonical count
  expect_equal(sum(cov$by_group$canonical_n), o$canonical_n)
  # the three already-complete orders stay at 100%
  done <- cov$by_group[cov$by_group$group %in% c("Gelisols", "Histosols", "Spodosols"), ]
  expect_true(all(done$pct == 100))
})

test_that("coverage_report(wrb_qualifiers) counts only genuine implementations", {
  skip_on_cran()
  cov <- coverage_report("wrb_qualifiers")
  expect_equal(cov$overall$system, "wrb2022")
  expect_equal(cov$overall$level, "qualifier")
  # the 4 added qualifiers are genuinely implemented, so none remain missing
  expect_false(any(c("Aeolic", "Fragic", "Limonic", "Tsitelic") %in% cov$missing))
  expect_gt(cov$overall$pct, 95)
  # v0.9.122: Fibric/Hemic/Sapric delegate to a real helper (.qual_decomp); the
  # detector follows one level of delegation, so they are covered, not stubs.
  expect_false(any(c("Fibric", "Hemic", "Sapric") %in% cov$stubs))
  expect_length(cov$stubs, 0L)
  # v0.9.145: +4 over the v0.9.122 baseline of 229. Petrosalic (vendored as the
  # upstream-corrupted "etrosalic") is now normalised to its complete
  # qual_petrosalic; Sideralic/Panpaic/Claric gained thin wrappers over their
  # existing diagnostics. Only Novic remains (schema-blocked: deposition age).
  expect_equal(cov$overall$covered_n, 233L)
  expect_false(any(c("etrosalic", "Petrosalic", "Sideralic", "Panpaic",
                     "Claric") %in% cov$missing))
  expect_true("Novic" %in% cov$missing)
  expect_true(all(cov$stubs %in% cov$missing))     # (vacuously true: no stubs)
  expect_equal(cov$overall$covered_n + cov$overall$missing_n, cov$overall$canonical_n)
  # specifier-derived qualifiers (Endo-/Epi-/...) count as covered
  expect_gt(cov$overall$specifier_derived_n, 0L)
})

test_that("coverage_report extends to USDA great-group / suborder levels", {
  skip_on_cran()
  gg <- coverage_report("usda_great_group")
  expect_equal(gg$overall$level, "great_group")
  expect_equal(gg$overall$pct, 100)               # all 339 great groups registered
  so <- coverage_report("usda_suborder")
  expect_equal(so$overall$pct, 100)               # all 68 suborders registered
})

test_that("coverage_report(sibcs) honestly reports registered counts (no canonical)", {
  skip_on_cran()
  s <- coverage_report("sibcs")
  expect_equal(s$overall$system, "sibcs")
  expect_true(is.na(s$overall$pct))               # no external canonical to diff
  expect_gt(s$overall$registered_n, 100L)
  expect_true("subgroup" %in% s$by_group$group)
})

test_that("the 4 new qualifiers wrap their diagnostics and gate on depth", {
  skip_on_cran()
  for (q in c("qual_aeolic", "qual_fragic", "qual_limonic", "qual_tsitelic")) {
    expect_true(exists(q, where = asNamespace("soilKey")), info = q)
  }
  # each returns a DiagnosticResult with the canonical display name
  expect_equal(qual_fragic(make_andosol_canonical())$name, "Fragic")
  expect_equal(qual_aeolic(make_andosol_canonical())$name, "Aeolic")
  expect_equal(qual_limonic(make_andosol_canonical())$name, "Limonic")
  expect_equal(qual_tsitelic(make_andosol_canonical())$name, "Tsitelic")
})

test_that("v0.9.113 subgroup refinements: 4 validated within-GG changes", {
  skip_on_cran()
  # Each is a Typic -> specific refinement that the KSSL n=2895 gate cleared
  # (0 was-correct -> now-wrong), firing on genuine multi-condition evidence.
  sg <- function(f) classify_usda(f(), on_missing = "silent")$name
  expect_equal(sg(make_andosol_canonical),    "Thaptic Hydrudands")
  expect_equal(sg(make_argissolo_canonical),  "Rhodic Kandiudults")
  expect_equal(sg(make_calcisol_canonical),   "Petronodic Haplocalcids")
  expect_equal(sg(make_planossolo_canonical), "Umbric Albaqualfs")
})

test_that("the generator never changes a fixture's great group", {
  skip_on_cran()
  # append-before-default guarantees new specifics can only steal pedons that
  # were falling through to Typic; the great group is invariant. Check the 4
  # changed fixtures keep their GG (the trailing token of the subgroup name).
  gg <- function(f) {
    n <- classify_usda(f(), on_missing = "silent")$name
    sub(".*\\s", "", n)
  }
  expect_equal(gg(make_andosol_canonical),    "Hydrudands")
  expect_equal(gg(make_argissolo_canonical),  "Kandiudults")
  expect_equal(gg(make_calcisol_canonical),   "Haplocalcids")
  expect_equal(gg(make_planossolo_canonical), "Albaqualfs")
})

test_that("the 4 qualifiers are wired into qualifiers.yaml per WRB Ch.4", {
  skip_on_cran()
  q <- yaml::read_yaml(system.file("rules", "wrb2022", "qualifiers.yaml",
                                   package = "soilKey"))$rsg_qualifiers
  # spot-check the canonical RSG memberships
  expect_true("Tsitelic" %in% q$CM$principal)      # Cambisols
  expect_true("Tsitelic" %in% q$AR$principal)      # Arenosols
  expect_true("Fragic"   %in% q$LV$principal)      # Luvisols
  expect_true("Aeolic"   %in% q$AN$principal)      # Andosols
  expect_true("Limonic"  %in% q$GL$supplementary)  # Gleysols
})
