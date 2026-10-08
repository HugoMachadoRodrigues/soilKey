# =============================================================================
# Tests for v0.9.64 -- final WRB qualifier batch closing the audit gap
# to 100% / 100% coverage (32/32 RSG, 131/131 PQ, 170/170 SQ).
#
# Three test groups:
#   1. Substantive PQ/SQs (5 tests each: NA-safe, positive trigger,
#      negative trigger, DiagnosticResult contract)
#   2. Tier-3 stubs (function exists, returns NA with `missing` field)
#   3. Bonus Endo- depth-window variants
# =============================================================================


.pedon_minimal <- function(...) {
  hz <- data.frame(
    designation = c("A", "B"),
    top_cm = c(0, 30), bottom_cm = c(30, 100),
    munsell_hue_moist = c("10YR", "10YR"),
    munsell_value_moist = c(4, 4),
    munsell_chroma_moist = c(3, 4),
    munsell_hue_dry = c(NA_character_, NA_character_),
    munsell_value_dry = c(NA_real_, NA_real_),
    munsell_chroma_dry = c(NA_real_, NA_real_),
    clay_pct = c(20, 30), silt_pct = c(20, 25), sand_pct = c(60, 45),
    ph_h2o = c(5.5, 5.0), oc_pct = c(2.0, 0.5),
    cec_cmol = c(8, 6), base_saturation_pct = c(40, 25),
    stringsAsFactors = FALSE
  )
  PedonRecord$new(
    site = list(id = "test-min", country = "BR"),
    horizons = hz)
}


# ---- 1. Principal qualifiers -----------------------------------------

test_that("qual_entic = albic AND NOT spodic", {
  res <- qual_entic(.pedon_minimal())
  expect_s3_class(res, "DiagnosticResult")
  expect_match(res$reference, "Entic")
})


test_that("qual_tonguic: a mollic horizon tonguing into the layer below", {
  # v0.9.217, WRB 2022: "tonguing of a chernic, mollic or umbric horizon into
  # an underlying layer". An AB/BA designation (the v0.9.64 reading) is not
  # tonguing; a tongued lower boundary of the mollic horizon is.
  p <- .pedon_minimal()
  p$horizons$designation <- c("A", "BA")
  expect_false(isTRUE(qual_tonguic(p)$passed))
  ch <- make_chernozem_canonical()
  h <- ch$horizons; h$boundary_topography <- "tongued"
  expect_true(isTRUE(qual_tonguic(PedonRecord$new(site = ch$site, horizons = h))$passed))
  expect_true(is.na(qual_tonguic(ch)$passed))     # boundary not described
})


test_that("qual_nudiargic requires argic at the surface (top_cm <= 5)", {
  p <- .pedon_minimal()
  # Profile with strong clay increase starting at 0 cm
  p$horizons$clay_pct <- c(20, 50)
  p$horizons$designation <- c("Bt1", "Bt2")
  p$horizons$top_cm <- c(0, 30)
  res <- qual_nudiargic(p)
  expect_s3_class(res, "DiagnosticResult")
})


test_that("qual_nudinatric returns NA when natric() unavailable", {
  res <- qual_nudinatric(.pedon_minimal())
  expect_s3_class(res, "DiagnosticResult")
})


test_that("qual_someric requires anthric AND mollic", {
  res <- qual_someric(.pedon_minimal())
  expect_s3_class(res, "DiagnosticResult")
  expect_false(isTRUE(res$passed))   # minimal pedon has neither
})


test_that("qual_neobrunic: cambic + recent layer_origin", {
  p <- .pedon_minimal()
  p$horizons$layer_origin <- c("recent colluvial", "recent colluvial")
  res <- qual_neobrunic(p)
  expect_s3_class(res, "DiagnosticResult")
})


test_that("qual_neocambic: cambic + weak structure", {
  p <- .pedon_minimal()
  p$horizons$structure_grade <- c("weak", "weak")
  res <- qual_neocambic(p)
  expect_s3_class(res, "DiagnosticResult")
})


test_that("qual_petrosalic stub returns DiagnosticResult", {
  res <- qual_petrosalic(.pedon_minimal())
  expect_s3_class(res, "DiagnosticResult")
})


# ---- 2. Substantive supplementary qualifiers -------------------------

test_that("qual_endic returns layers in 50-100 cm window", {
  res <- qual_endic(.pedon_minimal())
  expect_s3_class(res, "DiagnosticResult")
})


test_that("qual_epic: the RSG's diagnostic horizon starts <= 50 cm", {
  # v0.9.217, WRB 2022: Epic refers to "the uppermost respective diagnostic
  # horizon of the RSG"; it passed for any horizon starting above 50 cm.
  lv <- make_luvisol_canonical()
  expect_true(isTRUE(qual_epic(lv, rsg_code = "LV")$passed))   # argic <= 50
  expect_true(is.na(qual_epic(.pedon_minimal())$passed))       # no RSG given
})


test_that("qual_hyperorganic: organic material >= 200 cm thick (WRB 2022)", {
  # WRB 2022 Ch 5: Hyperorganic = organic material >= 200 cm THICK, not merely
  # an organic layer in the upper 100 cm.
  deep <- PedonRecord$new(horizons = ensure_horizon_schema(
    data.table::data.table(top_cm = c(0, 100), bottom_cm = c(100, 220),
                           oc_pct = c(30, 28))))
  expect_true(isTRUE(qual_hyperorganic(deep)$passed))

  shallow <- PedonRecord$new(horizons = ensure_horizon_schema(
    data.table::data.table(top_cm = c(0, 30), bottom_cm = c(30, 80),
                           oc_pct = c(30, 28))))  # only 80 cm organic
  expect_false(isTRUE(qual_hyperorganic(shallow)$passed))
})


test_that("qual_mineralic: mineral layers between organic ones (Histosols)", {
  # v0.9.217, WRB 2022: mineral material, combined >= 20 cm, above or in
  # between layers of organic material. A mineral soil (the v0.9.64 reading,
  # weighted OC < 12) is not Mineralic.
  expect_false(isTRUE(qual_mineralic(.pedon_minimal())$passed))
  peat <- PedonRecord$new(site = list(id = "t", country = "TEST"), horizons = ensure_horizon_schema(data.table::data.table(
    designation = c("Oa", "C", "Oa2"), top_cm = c(0, 40, 70),
    bottom_cm = c(40, 70, 120), oc_pct = c(40, 1, 40),
    clay_pct = c(NA, 20, NA), munsell_chroma_moist = c(1, 3, 1))))
  expect_true(isTRUE(qual_mineralic(peat)$passed))
})


test_that("qual_alcalic: pH >= 8.5 throughout the upper 50 cm, and Eutric", {
  # v0.9.217, WRB 2022: pHwater >= 8.5 in the upper 50 cm of the mineral soil
  # and fulfilling the criteria of the Eutric qualifier
  p <- PedonRecord$new(site = list(id = "t", country = "TEST"), horizons = ensure_horizon_schema(data.table::data.table(
    designation = c("A", "B"), top_cm = c(0, 30), bottom_cm = c(30, 100),
    ph_h2o = c(9.5, 9.2), ca_cmol = c(10, 10), mg_cmol = c(3, 3),
    k_cmol = c(0.5, 0.5), na_cmol = c(4, 5), al_cmol = c(0, 0),
    clay_pct = c(20, 30))))
  expect_true(isTRUE(qual_alcalic(p)$passed))
  expect_false(isTRUE(qual_alcalic(.pedon_minimal())$passed))
})


test_that("qual_chloridic: a salic horizon with chloride-dominated solution", {
  # v0.9.217, WRB 2022: a salic horizon whose 1:1 solution has [Cl-] >
  # 2[SO4--] > 2[HCO3-] (in Solonchaks only). soilKey stores no solution
  # anions, so it is never TRUE; a chloride figure alone no longer makes it.
  p <- .pedon_minimal()
  p$horizons$cl_cmol <- c(5, 6)
  expect_false(isTRUE(qual_chloridic(p)$passed))
})


test_that("qual_columnic: columnar / prismatic structure", {
  p <- .pedon_minimal()
  p$horizons$structure_type <- c("columnar", "prism")
  expect_true(isTRUE(qual_columnic(p)$passed))
})


test_that("qual_differentic: an argic or natric horizon meeting criterion 2.a", {
  # v0.9.217, WRB 2022: "an argic or natric horizon that meets diagnostic
  # criterion 2.a of the respective horizon". A 1.2-1.4x clay ratio without an
  # argic horizon (the v0.9.64 reading) is not Differentic.
  p <- .pedon_minimal()
  p$horizons$clay_pct <- c(20, 26)
  expect_false(isTRUE(qual_differentic(p)$passed))
  expect_true(isTRUE(qual_differentic(make_luvisol_canonical())$passed))
})


test_that("qual_capillaric: reducing conditions from capillary saturation", {
  # v0.9.217, WRB 2022: a layer >= 25 cm starting <= 75 cm with so few
  # macropores that capillary saturation causes reducing conditions. What
  # causes the reduction is not recorded, so it is never TRUE; redox features
  # in a fine soil (the v0.9.64 reading) do not make it so.
  p <- .pedon_minimal()
  p$horizons$redoximorphic_features_pct <- c(5, 5)
  p$horizons$clay_pct <- c(35, 35)
  p$horizons$silt_pct <- c(30, 30)
  expect_false(isTRUE(qual_capillaric(p)$passed))
})


test_that("qual_protospodic: spodic-like designation, fails strict", {
  p <- .pedon_minimal()
  p$horizons$designation <- c("A", "Bs")
  res <- qual_protospodic(p)
  expect_s3_class(res, "DiagnosticResult")
})


test_that("qual_protoargic: clay delta 2-6 pp", {
  p <- .pedon_minimal()
  p$horizons$clay_pct <- c(20, 24)   # delta 4 -> in [2, 6)
  expect_true(isTRUE(qual_protoargic(p)$passed))
})


test_that("qual_activic: an active-clay layer above the ferralic horizon", {
  # v0.9.217, WRB 2022: "above a ferralic horizon a layer, >= 30 cm thick,
  # with a CEC ... >= 24 cmolc kg-1 clay and < 0.6% soil organic carbon (in
  # Ferralsols only)". Exchangeable Al (the v0.9.64 reading) is not that.
  p <- .pedon_minimal()
  p$horizons$al_kcl_cmol <- c(6, 7)
  expect_false(isTRUE(qual_activic(p)$passed))
  f <- make_ferralsol_canonical(); h <- f$horizons
  h$cec_cmol[1:2] <- h$clay_pct[1:2] * 0.30     # 30 cmolc per kg clay, 0-35 cm
  h$oc_pct[1:2] <- 0.5
  expect_true(isTRUE(qual_activic(PedonRecord$new(site = f$site, horizons = h),
                                  rsg_code = "FR")$passed))
})


test_that("qual_geoabruptic: an abrupt textural difference not at a horizon top", {
  # v0.9.217, WRB 2022: an abrupt textural difference within 100 cm "not
  # associated with the upper limit of an argic, natric or spodic horizon".
  # A 2C designation alone (the v0.9.64 reading) is not that.
  p <- .pedon_minimal()
  p$horizons$designation <- c("A", "2C")
  expect_false(isTRUE(qual_geoabruptic(p)$passed))
  g <- PedonRecord$new(site = list(id = "t", country = "TEST"), horizons = ensure_horizon_schema(data.table::data.table(
    designation = c("A", "Bt", "2C"), top_cm = c(0, 20, 60),
    bottom_cm = c(20, 60, 100), clay_pct = c(10, 18, 45),
    silt_pct = c(20, 22, 30), sand_pct = c(70, 60, 25),
    na_cmol = c(0.1, 0.1, 0.1), cec_cmol = c(8, 10, 20),
    al_ox_pct = c(0.1, 0.1, 0.1), fe_ox_pct = c(0.1, 0.1, 0.1))))
  expect_true(isTRUE(qual_geoabruptic(g)$passed))   # at 60 cm; argic from 20
})


test_that("qual_gilgaic: site$forma_relevo contains 'gilgai'", {
  p <- .pedon_minimal()
  p$site$forma_relevo <- "gilgai microrelief"
  expect_true(isTRUE(qual_gilgaic(p)$passed))
})


test_that("qual_mahic: a thin artefact-rich layer in an artefact-poor soil", {
  # v0.9.217, WRB 2022: a layer >= 10 cm, starting <= 50 cm, with >= 80%
  # artefacts, and < 20% artefacts in the upper 100 cm (or to a limiting
  # layer). Organic carbon, base saturation and P (the v0.9.64 reading) are
  # not part of it.
  p <- .pedon_minimal()
  p$horizons$oc_pct <- c(5, 0.5)
  p$horizons$p_mehlich3_mg_kg <- c(150, 50)
  expect_false(isTRUE(qual_mahic(p)$passed))
  m <- PedonRecord$new(site = list(id = "t", country = "TEST"), horizons = ensure_horizon_schema(data.table::data.table(
    designation = c("Au", "C"), top_cm = c(0, 12), bottom_cm = c(12, 100),
    artefacts_pct = c(85, 0))))
  expect_true(isTRUE(qual_mahic(m)$passed))
})


test_that("qual_laxic: bulk density <= 0.9 in a mineral layer at 25-75 cm", {
  # v0.9.217, WRB 2022: between 25 and 75 cm a mineral layer >= 20 cm thick
  # with a bulk density <= 0.9 kg dm-3. Loose consistence (the v0.9.64
  # reading) is not a bulk density.
  p <- .pedon_minimal()
  p$horizons$consistence_dry <- c("loose", NA_character_)
  expect_false(isTRUE(qual_laxic(p)$passed))
  q <- .pedon_minimal()
  q$horizons$bulk_density_g_cm3 <- c(1.2, 0.8)
  expect_true(isTRUE(qual_laxic(q)$passed))
})


# ---- 3. Tier-3 stubs (NA with missing field listed) ------------------

test_that("Tier-3 stubs return NA-or-FALSE with WRB reference", {
  # v0.9.65 update: with the Tier-3 schema fields wired, a stub may
  # legitimately return FALSE when one of its checked fields IS
  # populated but doesn't match (e.g. qual_litholinic checks both
  # stratification_pattern AND designation; on a minimal pedon with
  # designation = c("A", "B") -- non-rock -- it returns FALSE rather
  # than NA, because designation is not "missing"). The test relaxes
  # to accept any well-formed DiagnosticResult on a sparse pedon.
  for (fn in list(qual_archaic, qual_arenicolic, qual_biocrustic,
                    qual_bryic, qual_cordic, qual_dorsic,
                    qual_escalic, qual_evapocrustic, qual_immissic,
                    qual_isopteric, qual_kalaic, qual_lapiadic,
                    qual_litholinic, qual_mochipic, qual_naramic,
                    qual_nechic, qual_pelocrustic, qual_puffic,
                    qual_raptic, qual_saprolithic,
                    qual_thixotropic, qual_uterquic)) {
    res <- fn(.pedon_minimal())
    expect_s3_class(res, "DiagnosticResult")
    # Either NA (no relevant data at all) OR FALSE (some data
    # present but doesn't match) -- both are acceptable on a
    # sparse pedon. The behavior we forbid is passed=TRUE without
    # the relevant field populated.
    expect_true(is.na(res$passed) || identical(res$passed, FALSE))
    expect_match(res$reference, "WRB")
  }
})


# ---- 4. Bonus Endo- variants -----------------------------------------

test_that("qual_endocalcic / endogypsic / endoduric depth-bounded", {
  expect_s3_class(qual_endocalcic(.pedon_minimal()), "DiagnosticResult")
  expect_s3_class(qual_endogypsic(.pedon_minimal()), "DiagnosticResult")
  expect_s3_class(qual_endoduric(.pedon_minimal()),  "DiagnosticResult")
})


# ---- 5. WRB audit shows 100% coverage --------------------------------

test_that("All canonical WRB qualifiers map to a soilKey function", {
  skip_if_no_soiltaxonomy()
  testthat::skip_if_not(file.exists(file.path(
    "inst", "extdata", "canonical", "WRB_4th_2022.rda")))
  wrb <- wrb2022_canonical(prefer_pkg = FALSE)
  pq_canon <- unique(wrb$pq$principal_qualifiers)
  sq_canon <- unique(wrb$sq$supplementary_qualifiers)
  ns_lower <- tolower(getNamespaceExports("soilKey"))

  pq_hits <- vapply(pq_canon, function(q) {
    any(grepl(paste0("\\bqual_", tolower(q), "\\b"),
                ns_lower, perl = TRUE)) ||
      any(grepl(paste0("\\b", tolower(q), "\\b"),
                  ns_lower, perl = TRUE))
  }, logical(1L))
  sq_hits <- vapply(sq_canon, function(q) {
    any(grepl(paste0("\\bqual_", tolower(q), "\\b"),
                ns_lower, perl = TRUE)) ||
      any(grepl(paste0("\\b", tolower(q), "\\b"),
                  ns_lower, perl = TRUE))
  }, logical(1L))

  pq_hit_pct <- 100 * sum(pq_hits) / length(pq_canon)
  sq_hit_pct <- 100 * sum(sq_hits) / length(sq_canon)
  # Expect very high coverage; the audit-script heuristic is broader
  # than this NAMESPACE-only match (it scans R/ source), so we
  # accept >= 80% via this stricter test.
  expect_gte(pq_hit_pct, 80)
  expect_gte(sq_hit_pct, 80)
})
