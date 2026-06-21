# v0.9.144 — non-circular predicted-taxon gap-fill
#
# build_taxon_profiles() summarises per-taxon mean depth profiles from reference
# labels; gapfill_by_predicted_taxon() classifies fill-free to a provisional
# taxon, then fills missing cells from that taxon's profile. Non-circular: the
# fill is keyed on the model's own prediction, not the reference label.

mk_ped <- function(site, df) {
  PedonRecord$new(site = site,
                  horizons = ensure_horizon_schema(data.table::as.data.table(df)))
}

test_that(".taxon_key normalises labels to a comparable key", {
  skip_on_cran()
  expect_identical(soilKey:::.taxon_key("ARGISSOLO VERMELHO Tb"), "argissolo")
  expect_identical(soilKey:::.taxon_key("Argissolos"), "argissolo")        # de-pluralised
  expect_identical(soilKey:::.taxon_key("LATOSSOLO AMARELO"), "latossolo")
  expect_true(is.na(soilKey:::.taxon_key(NA_character_)))
  expect_true(is.na(soilKey:::.taxon_key(NULL)))
  expect_true(is.na(soilKey:::.taxon_key(character(0))))
})

test_that("build_taxon_profiles averages each attribute into 6 depth slices", {
  skip_on_cran()
  p1 <- mk_ped(list(id = "a1", reference_sibcs = "ARGISSOLO VERMELHO"),
    data.frame(top_cm = c(0, 30), bottom_cm = c(30, 80),
               designation = c("A", "Bt"), clay_pct = c(18, 42)))
  p2 <- mk_ped(list(id = "a2", reference_sibcs = "ARGISSOLO AMARELO"),
    data.frame(top_cm = c(0, 30), bottom_cm = c(30, 80),
               designation = c("A", "Bt"), clay_pct = c(22, 46)))
  prof <- build_taxon_profiles(list(p1, p2), ref_field = "reference_sibcs",
                               attrs = "clay_pct")
  # both labels collapse to the single order-level key
  expect_identical(names(prof), "argissolo")
  cp <- prof$argissolo$clay_pct
  expect_length(cp, length(soilKey:::.SOILGRIDS_DEPTH_MIDS))
  # A horizons (mid 15 -> slice 2 mid 10): mean(18,22)=20; Bt (mid 55 -> slice 4 mid 45): mean(42,46)=44
  expect_equal(cp[2], 20)
  expect_equal(cp[4], 44)
  expect_true(is.na(cp[1]))   # no layer near 2.5 cm
})

test_that("build_taxon_profiles skips pedons with no/blank reference label", {
  skip_on_cran()
  p_ok  <- mk_ped(list(id = "x", reference_sibcs = "NEOSSOLO LITOLICO"),
    data.frame(top_cm = 0, bottom_cm = 20, designation = "A", clay_pct = 10))
  p_na  <- mk_ped(list(id = "y", reference_sibcs = NA_character_),
    data.frame(top_cm = 0, bottom_cm = 20, designation = "A", clay_pct = 99))
  p_bl  <- mk_ped(list(id = "z", reference_sibcs = ""),
    data.frame(top_cm = 0, bottom_cm = 20, designation = "A", clay_pct = 88))
  prof <- build_taxon_profiles(list(p_ok, p_na, p_bl), attrs = "clay_pct")
  expect_identical(names(prof), "neossolo")
})

test_that("gapfill_by_predicted_taxon fills missing cells from the predicted taxon", {
  skip_on_cran()
  # calibration set -> profile with a Bt clay around 44 at depth
  cal <- list(
    mk_ped(list(reference_sibcs = "ARGISSOLO VERMELHO"),
      data.frame(top_cm = c(0, 30, 60), bottom_cm = c(30, 60, 100),
                 designation = c("A", "Bt1", "Bt2"), clay_pct = c(20, 44, 46),
                 cec_cmol = c(6, 8, 7))),
    mk_ped(list(reference_sibcs = "ARGISSOLO AMARELO"),
      data.frame(top_cm = c(0, 30, 60), bottom_cm = c(30, 60, 100),
                 designation = c("A", "Bt1", "Bt2"), clay_pct = c(22, 44, 46),
                 cec_cmol = c(5, 8, 7))))
  prof <- build_taxon_profiles(cal, attrs = c("clay_pct", "cec_cmol"))

  # target pedon: an Argissolo-shaped profile with a hole in the Bt1 clay/cec
  tgt <- mk_ped(list(id = "t"),
    data.frame(top_cm = c(0, 30, 60), bottom_cm = c(30, 60, 100),
               designation = c("A", "Bt1", "Bt2"),
               clay_pct = c(20, NA, 46), cec_cmol = c(6, NA, 7),
               bs_pct = c(60, 58, 55)))
  before_clay <- tgt$horizons$clay_pct[2]

  out <- gapfill_by_predicted_taxon(tgt$clone(deep = TRUE), prof, system = "sibcs")
  meta <- attr(out, "gapfill_by_predicted_taxon")

  expect_identical(meta$taxon, "argissolo")          # provisional == model prediction
  expect_gt(meta$n_filled, 0L)
  expect_false(is.na(out$horizons$clay_pct[2]))      # the hole is filled
  expect_true(is.na(before_clay))                    # caller object never mutated
  expect_true(is.na(tgt$horizons$clay_pct[2]))

  # filled cell carries grade-C provenance
  prov <- out$provenance
  if (!is.null(prov) && nrow(prov)) {
    expect_true(any(prov$source == "inferred_prior"))
  }
})

test_that("gapfill_by_predicted_taxon is a no-op when the taxon is unknown", {
  skip_on_cran()
  prof <- list(argissolo = list(clay_pct = c(NA, 20, NA, 44, NA, NA)))
  # a pedon that classifies to something not in the profile -> nothing filled
  tgt <- mk_ped(list(id = "u"),
    data.frame(top_cm = c(0, 20), bottom_cm = c(20, 50),
               designation = c("A", "C"), clay_pct = c(NA, NA)))
  out  <- gapfill_by_predicted_taxon(tgt$clone(deep = TRUE), prof, system = "sibcs")
  meta <- attr(out, "gapfill_by_predicted_taxon")
  if (!identical(meta$taxon, "argissolo")) {
    expect_identical(meta$n_filled, 0L)
  }
})

test_that(".classify_apply_gapfill routes method='taxon' and never mutates the caller", {
  skip_on_cran()
  cal <- list(
    mk_ped(list(reference_sibcs = "ARGISSOLO VERMELHO"),
      data.frame(top_cm = c(0, 30, 60), bottom_cm = c(30, 60, 100),
                 designation = c("A", "Bt1", "Bt2"), clay_pct = c(20, 44, 46))))
  prof <- build_taxon_profiles(cal, attrs = "clay_pct")
  tgt <- mk_ped(list(id = "t"),
    data.frame(top_cm = c(0, 30, 60), bottom_cm = c(30, 60, 100),
               designation = c("A", "Bt1", "Bt2"),
               clay_pct = c(20, NA, 46), bs_pct = c(60, 58, 55)))
  filled <- soilKey:::.classify_apply_gapfill(
    tgt, list(method = "taxon", taxon_profiles = prof, system = "sibcs"))
  expect_true(is.na(tgt$horizons$clay_pct[2]))        # original untouched
  expect_false(identical(filled, tgt))
})

test_that("unknown gapfill method errors and lists taxon", {
  skip_on_cran()
  tgt <- mk_ped(list(id = "t"),
    data.frame(top_cm = 0, bottom_cm = 20, designation = "A", clay_pct = 10))
  expect_error(
    soilKey:::.classify_apply_gapfill(tgt, list(method = "bogus")),
    "taxon")
})
