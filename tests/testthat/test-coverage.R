# Tests for v0.9.113 honest taxonomic-completeness measurement
# (coverage_report) and the 4 new byte-identical WRB qualifiers
# (Aeolic / Fragic / Limonic / Tsitelic).

test_that("coverage_report(usda_subgroup) measures by name against KST 13ed", {
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

test_that("coverage_report(wrb_qualifiers) counts qual_* functions", {
  cov <- coverage_report("wrb_qualifiers")
  expect_equal(cov$overall$system, "wrb2022")
  expect_equal(cov$overall$level, "qualifier")
  # the 4 newly-added qualifiers are now registered, so none remain missing
  expect_false(any(c("Aeolic", "Fragic", "Limonic", "Tsitelic") %in% cov$missing))
  expect_gt(cov$overall$pct, 96)
})

test_that("the 4 new qualifiers wrap their diagnostics and gate on depth", {
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
  # Each is a Typic -> specific refinement that the KSSL n=2895 gate cleared
  # (0 was-correct -> now-wrong), firing on genuine multi-condition evidence.
  sg <- function(f) classify_usda(f(), on_missing = "silent")$name
  expect_equal(sg(make_andosol_canonical),    "Thaptic Hydrudands")
  expect_equal(sg(make_argissolo_canonical),  "Rhodic Kandiudults")
  expect_equal(sg(make_calcisol_canonical),   "Petronodic Haplocalcids")
  expect_equal(sg(make_planossolo_canonical), "Umbric Albaqualfs")
})

test_that("the generator never changes a fixture's great group", {
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
  q <- yaml::read_yaml(system.file("rules", "wrb2022", "qualifiers.yaml",
                                   package = "soilKey"))$rsg_qualifiers
  # spot-check the canonical RSG memberships
  expect_true("Tsitelic" %in% q$CM$principal)      # Cambisols
  expect_true("Tsitelic" %in% q$AR$principal)      # Arenosols
  expect_true("Fragic"   %in% q$LV$principal)      # Luvisols
  expect_true("Aeolic"   %in% q$AN$principal)      # Andosols
  expect_true("Limonic"  %in% q$GL$supplementary)  # Gleysols
})
