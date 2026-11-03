# v0.9.346: the French Referentiel pedologique 2008, part 4 -- the registry
# entry rp2008, classify_rp2008(), the attachment (degrees, tolerances of p.
# 64, precedence, doubles, "[non rattache]") and the 29 references of the
# first nine GER (inst/rules/rp2008/references/*.yaml, R/diagnostics-rp.R,
# R/key-rp2008.R). Hugo's decisions Q1, Q3-Q7, Q14-Q16 and the defaults of
# 2026-11-02; soilKey's readings of 2026-11-03 in readings.yaml.

.rp46_refs <- c("alocrisols_typiques", "alocrisols_humiques", "vitrandosols", "silandosols_eutriques",
                "silandosols_dystriques", "silandosols_perhydriques", "aluandosols_hapliques",
                "aluandosols_perhydriques", "anthroposols_transformes", "anthroposols_artificiels",
                "anthroposols_reconstitues", "anthroposols_construits", "anthroposols_archeologiques",
                "arenosols", "brunisols_eutriques", "brunisols_dystriques", "rendosols", "calcosols",
                "dolomitosols", "rendisols", "calcisols", "magnesisols", "calcarisols",
                "chernosols_hapliques", "chernosols_typiques", "chernosols_melanoluviques",
                "colluviosols", "cryosols_histiques", "cryosols_mineraux")

# A pedon in the RP notation, or on the data (rp = FALSE).
.rp46_mk <- function(h, site = list(), rp = TRUE) {
  n <- max(lengths(h))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  base <- list(id = "t", country = "FR", drainage_class = "well drained")
  base <- if (rp) c(base, designation_system = "rp2008") else c(base, lat = 46, lon = 3, elevation_m = 300)
  PedonRecord$new(site = utils::modifyList(base, site),
                  horizons = ensure_horizon_schema(data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}

# A pedon with some horizon values (rows) or site values changed.
.rp46_edit <- function(p, rows = NULL, ..., site = list()) {
  h <- as.data.frame(p$horizons)
  ch <- list(...)
  for (k in names(ch)) {
    if (!k %in% names(h)) h[[k]] <- NA
    h[[k]][rows] <- ch[[k]]
  }
  s <- p$site
  for (k in names(site)) s[[k]] <- site[[k]]
  PedonRecord$new(site = s, horizons = ensure_horizon_schema(data.table::as.data.table(h)))
}

# The degree of one reference on a pedon (its tests and imperfect cases).
.rp46_deg <- function(p, code) {
  rules <- .rp_rules()
  fr <- .rp_frame_codes(.rp_identify(p))
  .rp_with_state(p, fr, .rp_profile(p),
                 .rp_eval_reference(rules$ref_by_code[[code]], p, .tolerance_table(rules$tolerances)))
}

.rp46_cl <- function(p) classify_rp2008(p, on_missing = "silent")
.rp46_ex <- make_rp2008_example


# --- the registry and the rules ------------------------------------------------------

test_that("the registry has the RP 2008, experimental and opt-in, beside the CPCS", {
  skip_on_cran()
  s <- soilkey_systems()
  r <- s[s$id == "rp2008", ]
  expect_identical(nrow(r), 1L)
  expect_identical(r$classifier, "classify_rp2008")
  expect_identical(r$levels, "references")
  expect_identical(r$aliases, "rp, referentiel")
  expect_identical(r$status, "experimental")
  expect_false(r$default)
  sys <- .system_get("rp2008")
  expect_identical(sys$namespace, "rp")
  expect_identical(sys$order, 12L)
  for (a in c("rp", "referentiel")) expect_identical(.system_resolve(a), "rp2008")
  # france, french and fr stay with the CPCS (Q2)
  expect_identical(.system_resolve("france"), "cpcs1967")
  expect_false("rp2008" %in% .system_ids(default_only = TRUE))
  # identify_horizons() now reaches the RP identification
  expect_identical(identify_horizons(.rp46_ex("calcosols"), "rp2008")$code, c("Aca", "Sca", "C"))
  # classify_all() runs it only when asked
  p <- .rp46_ex("brunisols_eutriques")
  expect_false("rp2008" %in% names(classify_all(p, on_missing = "silent")))
  res <- classify_all(p, systems = "rp", on_missing = "silent")
  expect_identical(res$rp2008$name, "brunisol eutrique")
})

test_that("the rule validator passes the RP rules", {
  skip_on_cran()
  v <- .validate_rules("rp2008")
  expect_gt(v$n, 15L)
  expect_identical(v$missing, character(0))
  expect_identical(v$unsuffixed, character(0))
  expect_identical(v$mixed, character(0))
  expect_identical(v$malformed, character(0))
  expect_identical(v$codes, character(0))
})

test_that("the catalogue has 34 GER and 110 references, 29 of them implemented", {
  skip_on_cran()
  rules <- .rp_rules()
  expect_length(rules$ger, 34L)
  impl <- Filter(function(g) isTRUE(g$implemented), rules$ger)
  expect_identical(vapply(impl, `[[`, "", "code"),
                   c("alocrisols", "andosols", "anthroposols", "arenosols", "brunisols", "calcium_magnesium",
                     "chernosols", "colluviosols", "cryosols"))
  n_ref <- sum(vapply(rules$ger, function(g) length(g$references), 0L))
  expect_identical(n_ref, 110L)
  expect_identical(names(rules$ref_by_code), .rp46_refs)
  expect_identical(unlist(lapply(impl, function(g) unlist(g$references))), .rp46_refs)
  for (r in rules$references) {
    expect_true(all(c("code", "ger", "name", "name_singular", "name_en", "name_en_singular", "page", "tests") %in% names(r)),
                info = r$code)
    slug <- gsub("[^a-z]+", "_", chartr("\u00e9\u00e8\u00ea\u00e0\u00e2\u00f9\u00fb\u00f4\u00ee\u00ef\u00e7",
                                        "eeeaauuoiic", gsub("\u00e6", "ae", r$name)))
    expect_identical(r$code, slug, info = r$code)
  }
  # every unimplemented GER has a signpost
  later <- Filter(function(g) !isTRUE(g$implemented), rules$ger)
  expect_length(later, 25L)
  expect_true(all(vapply(later, function(g) !is.null(g$signpost), NA)))
  # the tolerance table of p. 64
  tab <- .tolerance_table(rules$tolerances)
  expect_identical(.tolerance_of(tab, "depth", 120), 20)
  expect_identical(.tolerance_of(tab, "depth", 40), 0)
  expect_identical(.tolerance_of(tab, "organic_carbon", 30), 5)
  expect_identical(.tolerance_of(tab, "bs", 90), 5)
})

test_that("the rapid key reaches every GER, the grisols only by soilKey's 7c", {
  skip_on_cran()
  ck <- .rp_rapid_key_check()
  expect_identical(nrow(ck), 34L)
  expect_true(all(ck$reached))
  expect_identical(ck$ger[!ck$by_the_book], "grisols")
  expect_identical(ck$entries[ck$ger == "grisols"], "7c")
  rules <- .rp_rules()
  outs <- unique(unlist(lapply(rules$rapid_key, `[[`, "outcomes")))
  expect_true(all(outs %in% ck$ger))
  # the order: entries 1 to 10, then 11 and 12; anthroposols (2) before
  # brunisols (11a) before the calcium GER (12)
  ord <- .rp_order(rules)
  expect_lt(match("anthroposols_transformes", ord), match("brunisols_eutriques", ord))
  expect_lt(match("brunisols_dystriques", ord), match("alocrisols_typiques", ord))
  expect_lt(match("alocrisols_humiques", ord), match("rendosols", ord))
  expect_lt(match("grisols", ord), match("podzosols", ord))
})


# --- every reference: a simple fixture, an imperfect one, a failing twin ---------------

test_that("each example attaches simply to its own reference, in the name", {
  skip_on_cran()
  for (cd in .rp46_refs) {
    r <- .rp46_cl(.rp46_ex(cd))
    expect_identical(r$levels$code[1], cd, info = cd)
    expect_identical(r$candidates$degree[r$candidates$code == cd], "simple", info = cd)
    expect_identical(r$system_id, "rp2008")
    expect_identical(r$levels$level[1], "references")
  }
})

test_that("a value inside a tolerance band attaches imperfectly; a failing twin fails", {
  skip_on_cran()
  ex <- .rp46_ex
  e <- .rp46_edit
  cases <- list(
    alocrisols_typiques = list(e(ex("alocrisols_typiques"), 2:3, top_cm = c(8, 15), bottom_cm = c(15, 100)),
                               e(ex("alocrisols_typiques"), 2:3, top_cm = c(8, 11), bottom_cm = c(11, 100))),
    alocrisols_humiques = list(e(ex("alocrisols_humiques"), 1, oc_pct = 7), e(ex("alocrisols_humiques"), 1, oc_pct = 4)),
    vitrandosols = list(e(ex("vitrandosols"), 1:2, top_cm = c(0, 27), bottom_cm = c(27, 100)),
                        e(ex("vitrandosols"), 1:2, top_cm = c(0, 30), bottom_cm = c(30, 100), designation = c("Avi", "E"))),
    silandosols_eutriques = list(e(ex("silandosols_dystriques"), 1:3, bs_pct = 47), ex("silandosols_dystriques")),
    silandosols_dystriques = list(e(ex("silandosols_dystriques"), 1:3, bs_pct = 52),
                                  e(ex("silandosols_dystriques"), 1:3, bs_pct = 70)),
    silandosols_perhydriques = list(e(ex("silandosols_perhydriques"), 2:3, top_cm = c(45, 77), bottom_cm = c(77, 130)),
                                    ex("silandosols_dystriques")),
    aluandosols_hapliques = list(e(ex("aluandosols_hapliques"), 1:3, top_cm = c(0, 15, 27), bottom_cm = c(15, 27, 130)),
                                 e(ex("aluandosols_hapliques"), 1:2, top_cm = c(0, 35), bottom_cm = c(35, 90),
                                   designation = c("Alu", "BT"))),
    aluandosols_perhydriques = list(e(ex("aluandosols_perhydriques"), 1:3, top_cm = c(0, 45, 78), bottom_cm = c(45, 78, 130),
                                      water_content_1500kpa_undried = c(80, 140, 40)), ex("aluandosols_hapliques")),
    anthroposols_transformes = list(e(ex("anthroposols_transformes"), site = list(mechanical_mixing_depth_cm = 45)),
                                    e(ex("anthroposols_transformes"), site = list(rp_anthropic_history = NA))),
    anthroposols_artificiels = list(e(ex("anthroposols_artificiels"), 2:3, top_cm = c(10, 45), bottom_cm = c(45, 110)),
                                    e(ex("anthroposols_artificiels"), 2:3, top_cm = c(10, 20), bottom_cm = c(20, 110))),
    anthroposols_reconstitues = list(e(ex("anthroposols_reconstitues"), 1:2, top_cm = c(0, 45), bottom_cm = c(45, 100)),
                                     e(ex("anthroposols_reconstitues"), 1:2, top_cm = c(0, 20), bottom_cm = c(20, 100))),
    anthroposols_construits = list(e(ex("anthroposols_construits"), 2, bottom_cm = 45),
                                   e(ex("anthroposols_construits"), 2, bottom_cm = 20)),
    anthroposols_archeologiques = list(e(ex("anthroposols_archeologiques"), 1:2, top_cm = c(0, 45), bottom_cm = c(45, 90)),
                                       e(ex("anthroposols_archeologiques"), 1, anthropic_material_pct = 10)),
    arenosols = list(.rp46_mk(list(designation = c("A", "C", "Cg"), top_cm = c(0, 20, 70), bottom_cm = c(20, 70, 150),
                                   oc_pct = c(1.2, 0.1, 0.1), sand_pct = c(88, 93, 93), clay_pct = c(4, 3, 3))),
                     .rp46_mk(list(designation = c("A", "C", "Cg"), top_cm = c(0, 20, 30), bottom_cm = c(20, 30, 150),
                                   oc_pct = c(1.2, 0.1, 0.1), sand_pct = c(88, 93, 93), clay_pct = c(4, 3, 3)))),
    brunisols_eutriques = list(e(ex("brunisols_eutriques"), 2, bs_pct = 47), e(ex("brunisols_eutriques"), 2, bs_pct = 30)),
    brunisols_dystriques = list(e(ex("brunisols_dystriques"), 2, bs_pct = 53), e(ex("brunisols_dystriques"), 2, bs_pct = 75)),
    rendosols = list(e(ex("rendosols"), 1:2, top_cm = c(0, 38), bottom_cm = c(38, 60)),
                     e(ex("rendosols"), 2, designation = "Sca")),
    calcosols = list(e(ex("calcosols"), 2:3, top_cm = c(20, 32), bottom_cm = c(32, 100)), e(ex("calcosols"), 2, designation = "Sci")),
    dolomitosols = list(e(ex("dolomitosols"), 2:3, top_cm = c(20, 32), bottom_cm = c(32, 80)),
                        e(ex("dolomitosols"), 1:2, designation = c("Aca", "Sca"))),
    rendisols = list(e(ex("rendisols"), 1:2, top_cm = c(0, 38), bottom_cm = c(38, 60)), e(ex("rendisols"), 1, mg_cmol = 17)),
    calcisols = list(e(ex("calcisols"), 2:3, top_cm = c(20, 32), bottom_cm = c(32, 100)), e(ex("calcisols"), 2, bs_pct = 70)),
    magnesisols = list(e(ex("magnesisols"), 2:3, top_cm = c(20, 32), bottom_cm = c(32, 100)),
                       e(ex("magnesisols"), 1:2, designation = c("Aci", "Sci"))),
    calcarisols = list(e(ex("calcarisols"), 1:3, top_cm = c(0, 38, 58), bottom_cm = c(38, 58, 90)),
                       e(ex("calcarisols"), 1:3, top_cm = c(0, 45, 65), bottom_cm = c(45, 65, 90))),
    chernosols_hapliques = list(e(ex("chernosols_hapliques"), 2:3, top_cm = c(30, 90), bottom_cm = c(90, 130)),
                                e(ex("chernosols_hapliques"), 2:3, top_cm = c(30, 35), bottom_cm = c(35, 120))),
    chernosols_typiques = list(e(ex("chernosols_typiques"), 1, bs_pct = 82), e(ex("chernosols_typiques"), 2, clay_pct = 45)),
    chernosols_melanoluviques = list(e(ex("chernosols_melanoluviques"), 2:3, top_cm = c(45, 170), bottom_cm = c(170, 200)),
                                     e(ex("chernosols_melanoluviques"), 2, clay_pct = 27)),
    colluviosols = list(e(ex("colluviosols"), 2:3, top_cm = c(25, 45), bottom_cm = c(45, 120)),
                        e(ex("colluviosols"), 1:2, layer_origin = NA)),
    cryosols_histiques = list(e(ex("cryosols_histiques"), 2:3, designation = c("Hf", "C"), top_cm = c(3, 30), bottom_cm = c(30, 90),
                                oc_pct = c(42, 1)),
                              e(ex("cryosols_histiques"), 3, designation = "H", site = list(frost_regime = "none"))),
    cryosols_mineraux = list(e(ex("cryosols_mineraux"), 1:2, top_cm = c(0, 33), bottom_cm = c(33, 40)),
                             e(ex("cryosols_mineraux"), 1:2, designation = c("H", "Js"), cryoturbation = NA))
  )
  expect_identical(sort(names(cases)), sort(.rp46_refs))
  for (cd in names(cases)) {
    a <- .rp46_deg(cases[[cd]][[1]], cd)
    b <- .rp46_deg(cases[[cd]][[2]], cd)
    expect_identical(a$degree, "imperfect", info = cd)
    expect_gt(length(a$imperfect_by), 0L)
    expect_identical(b$degree, "fail", info = cd)
    expect_gt(length(b$failed), 0L)
  }
  # an imperfect case of the chapter names its qualifier in the name
  r <- .rp46_cl(.rp46_edit(ex("calcosols"), 1:3, top_cm = c(0, 10, 20), bottom_cm = c(10, 20, 100)))
  expect_identical(r$name, "calcosol leptique")
  expect_identical(r$levels$attachment, "imperfect")
  expect_match(r$candidates$imperfect_by[r$candidates$code == "calcosols"], "leptique (p. 111)", fixed = TRUE)
  # a value within the tolerance gives imperfect, never simple (Q4): the
  # brunisol at S/CEC 47 % is a dystric one, the eutric imperfect beside it
  r <- .rp46_cl(.rp46_edit(ex("brunisols_eutriques"), 2, bs_pct = 47))
  expect_identical(r$name, "brunisol dystrique")
  expect_identical(r$candidates$degree[r$candidates$code == "brunisols_eutriques"], "imperfect")
})


# --- the data route -------------------------------------------------------------------

test_that("a profile with no designations attaches through the horizons on the data, one per GER", {
  skip_on_cran()
  d <- function(h, site = list()) .rp46_mk(h, site, rp = FALSE)
  P <- list(
    alocrisols_typiques = d(list(top_cm = c(0, 8, 55), bottom_cm = c(8, 55, 100), oc_pct = c(5, 1.2, 0.3), clay_pct = c(18, 20, 12),
      sand_pct = c(45, 45, 60), ph_h2o = c(4.4, 4.6, 4.8), al_kcl_cmol = c(5, 4.5, 3), cec_cmol = c(14, 11, 8), bs_pct = c(15, 12, 15),
      ca_cmol = c(1, 0.7, 0.6), mg_cmol = c(0.6, 0.4, 0.4), k_cmol = 0.2, na_cmol = 0.05,
      structure_type = c("granular", "subangular blocky", "massive"), structure_grade = c("moderate", "moderate", "weak"),
      munsell_hue_moist = "10YR", munsell_value_moist = c(3, 5, 6), munsell_chroma_moist = c(2, 6, 4)), list(parent_material = "granite")),
    silandosols_eutriques = d(list(top_cm = c(0, 45, 90), bottom_cm = c(45, 90, 130), oc_pct = c(5, 1.5, 0.5), clay_pct = c(25, 22, 15),
      al_ox_pct = c(4, 4.5, 1), fe_ox_pct = c(1.5, 1.6, 0.5), si_ox_pct = c(1.2, 1.4, 0.3), al_pyro_pct = c(0.8, 0.6, 0.1),
      bulk_density_g_cm3 = c(0.6, 0.7, 1.1), phosphate_retention_pct = c(92, 95, 40), ph_naf = c(10.5, 10.8, 9),
      water_content_1500kpa_undried = c(60, 60, 30), bs_pct = c(65, 70, 70), cec_cmol = c(30, 25, 15), ca_cmol = 12, mg_cmol = 4,
      k_cmol = 1, na_cmol = 0.5, al_kcl_cmol = 0.3, ph_h2o = c(5.6, 5.8, 6), structure_type = c("granular", "subangular blocky", "massive"),
      structure_grade = c("strong", "moderate", "weak"), munsell_hue_moist = "10YR", munsell_value_moist = c(3, 5, 5),
      munsell_chroma_moist = c(3, 4, 4)), list(parent_material = "volcanic ash")),
    anthroposols_artificiels = d(list(top_cm = c(0, 10, 70), bottom_cm = c(10, 70, 110), oc_pct = c(2, 0.5, 0.3), artefacts_pct = c(60, 75, 0),
      clay_pct = c(15, 12, 20), ph_h2o = 7.8, structure_type = "massive", munsell_hue_moist = "10YR",
      munsell_value_moist = c(4, 5, 5), munsell_chroma_moist = 3)),
    arenosols = d(list(top_cm = c(0, 20), bottom_cm = c(20, 150), oc_pct = c(1.2, 0.1), sand_pct = c(88, 93), clay_pct = c(4, 3),
      silt_pct = c(8, 4), ph_h2o = c(5.0, 5.4), bs_pct = c(30, 40), cec_cmol = c(4, 2), ca_cmol = c(0.8, 0.5), mg_cmol = c(0.3, 0.2),
      structure_type = c("granular", "single grain"), structure_grade = c("weak", NA), redoximorphic_features_pct = 0,
      munsell_hue_moist = "10YR", munsell_value_moist = c(4, 6), munsell_chroma_moist = c(2, 4))),
    brunisols_eutriques = d(list(top_cm = c(0, 20, 70), bottom_cm = c(20, 70, 110), oc_pct = c(3, 0.8, 0.3), clay_pct = c(22, 25, 20),
      sand_pct = 40, ph_h2o = c(6.2, 6.4, 6.6), cec_cmol = c(16, 13, 10), bs_pct = c(60, 62, 65), ca_cmol = c(7, 6, 5),
      mg_cmol = c(1.5, 1.4, 1.2), k_cmol = 0.3, na_cmol = 0.1, al_kcl_cmol = c(0.2, 0.1, 0.1), caco3_pct = 0,
      structure_type = c("granular", "subangular blocky", "massive"), structure_grade = c("strong", "strong", "weak"),
      structure_size = c("fine", "fine", NA), munsell_hue_moist = "10YR", munsell_value_moist = c(4, 4, 5),
      munsell_chroma_moist = c(3, 4, 4), redoximorphic_features_pct = 0)),
    calcosols = d(list(top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 100), oc_pct = c(3, 0.8, 0.3), caco3_pct = c(15, 25, 40),
      clay_pct = c(30, 32, 25), ph_h2o = c(8.0, 8.2, 8.3), effervescence_class = "strong", effervescence_extent = "continuous",
      bs_pct = 100, ca_cmol = 25, mg_cmol = 1, structure_type = c("granular", "subangular blocky", "massive"),
      structure_grade = c("strong", "moderate", "weak"), worm_holes_pct = c(5, 1, 0), munsell_hue_moist = "10YR",
      munsell_value_moist = c(3, 5, 6), munsell_chroma_moist = c(2, 3, 3), redoximorphic_features_pct = 0), list(parent_material = "marl")),
    chernosols_hapliques = d(list(top_cm = c(0, 65), bottom_cm = c(65, 120), oc_pct = c(3.5, 0.4), clay_pct = c(30, 25), caco3_pct = c(0, 12),
      bs_pct = c(98, 100), cec_cmol = c(30, 20), ph_h2o = c(7.4, 8.2), ca_cmol = c(25, 18), mg_cmol = c(3, 2),
      structure_type = c("granular", "massive"), structure_grade = c("strong", "weak"), worm_holes_pct = c(10, 2),
      secondary_carbonates_pct = c(0, 10), secondary_carbonates_form = c(NA, "pseudomycelia"), effervescence_class = c("none", "strong"),
      boundary_distinctness = c("gradual", NA), munsell_hue_moist = "10YR", munsell_value_moist = c(2, 5), munsell_chroma_moist = c(1, 3),
      redoximorphic_features_pct = 0), list(land_use = "grassland")),
    colluviosols = d(list(top_cm = c(0, 25, 75), bottom_cm = c(25, 75, 120), oc_pct = c(2.5, 1, 0.4), clay_pct = c(28, 30, 25),
      sand_pct = c(30, 28, 35), ph_h2o = 6.0, bs_pct = c(60, 55, 55), cec_cmol = 15, ca_cmol = 6, mg_cmol = 2,
      structure_type = c("granular", "massive", "massive"), structure_grade = c("weak", NA, NA), munsell_hue_moist = "10YR",
      munsell_value_moist = c(4, 5, 5), munsell_chroma_moist = c(3, 4, 4), layer_origin = c("colluvic", "colluvic", NA),
      redoximorphic_features_pct = 0)),
    cryosols_histiques = d(list(top_cm = c(0, 40), bottom_cm = c(40, 90), oc_pct = c(42, 40), loi_pct = c(85, 80),
      fiber_content_rubbed_pct = c(60, 30), permafrost_temp_C = c(NA, -1), ice_pct = c(NA, 30)),
      list(frost_regime = "permafrost", drainage_class = "very poorly drained", lat = 68, elevation_m = 400))
  )
  for (cd in names(P)) {
    r <- .rp46_cl(P[[cd]])
    expect_identical(r$levels$code[1], cd, info = cd)
    expect_true(all(r$trace$horizons$route == "data"), info = cd)
  }
  expect_identical(length(unique(vapply(names(P), function(cd) .rp_rules()$ref_by_code[[cd]]$ger, ""))), 9L)
})


# --- precedence, doubles, not attached, not implemented -------------------------------

test_that("calcisol against brunisol eutrique: a positive proof of carbonate (Q16)", {
  skip_on_cran()
  # the Cca and a limestone: the calcisol, the brunisol excluded
  r <- .rp46_cl(.rp46_ex("calcisols"))
  expect_identical(r$name, "calcisol")
  b <- r$candidates[r$candidates$code == "brunisols_eutriques", ]
  expect_identical(b$excluded_by, "calcisols")
  expect_match(b$precedence, "excluded by calcisols", fixed = TRUE)
  # no reserve recorded: the brunisol eutrique sature takes the name, the
  # calcisol stays a candidate with the reason
  p <- .rp46_edit(.rp46_ex("calcisols"), 3, designation = "C", caco3_pct = 0, site = list(parent_material = "loess"))
  expect_false(carbonate_reserve_rp(p)$passed)
  r <- .rp46_cl(p)
  expect_identical(r$name, "brunisol eutrique")
  ca <- r$candidates[r$candidates$code == "calcisols", ]
  expect_identical(ca$degree, "simple")
  expect_false(ca$in_name)
  expect_match(ca$precedence, "no carbonate reserve recorded", fixed = TRUE)
})

test_that("the explicit precedence of the chapters", {
  skip_on_cran()
  # a calcarisol before the rendosol that attaches too (p. 122)
  r <- .rp46_cl(.rp46_ex("calcarisols"))
  expect_identical(r$name, "calcarisol")
  expect_identical(r$candidates$degree[r$candidates$code == "rendosols"], "simple")
  # perhydric first (p. 81): the dystric silandosol attaches too
  r <- .rp46_cl(.rp46_ex("silandosols_perhydriques"))
  expect_identical(r$name, "silandosol perhydrique")
  expect_true("silandosols_dystriques" %in% r$candidates$code[r$candidates$degree == "simple"])
  # a colluviosol only when no other reference attaches (p. 132)
  p <- .rp46_edit(.rp46_ex("brunisols_eutriques"), 1:2, layer_origin = "colluvic")
  r <- .rp46_cl(p)
  expect_identical(r$name, "brunisol eutrique")
  co <- r$candidates[r$candidates$code == "colluviosols", ]
  expect_identical(co$degree, "simple")
  expect_match(co$precedence, "brunisols_eutriques first", fixed = TRUE)
  # the artificiels before the construits, unless the construction is recorded
  r <- .rp46_cl(.rp46_ex("anthroposols_construits"))
  expect_identical(r$name, "anthroposol construit")
  r <- .rp46_cl(.rp46_edit(.rp46_ex("anthroposols_construits"), site = list(rp_anthropic_history = NA)))
  expect_identical(r$name, "anthroposol artificiel")
  # the magnesisol before the brunisol eutrique
  expect_identical(.rp46_cl(.rp46_ex("magnesisols"))$name, "magn\u00e9sisol")
})

test_that("the doubles of these chapters, in the book's order", {
  skip_on_cran()
  # calcisol-calcosol: [Aci + Sci] and Sca of the same order (p. 113)
  p <- .rp46_mk(list(designation = c("Aci", "Sci", "Sca", "Cca"), top_cm = c(0, 20, 45, 90), bottom_cm = c(20, 45, 90, 130),
                     oc_pct = c(3, 1, 0.5, 0.2), bs_pct = c(95, 98, 100, 100), ca_cmol = c(18, 17, 20, 20), mg_cmol = 2,
                     caco3_pct = c(0, 0, 15, 30)))
  r <- .rp46_cl(p)
  expect_identical(r$name, "calcisol-calcosol")
  expect_identical(r$levels$code, c("calcisols", "calcosols"))
  expect_identical(r$levels$attachment, c("double", "double"))
  # clearly thinner: the calcosol alone (decarbonated at the surface)
  r <- .rp46_cl(.rp46_edit(p, 1:4, top_cm = c(0, 10, 20, 90), bottom_cm = c(10, 20, 90, 130)))
  expect_identical(r$name, "calcosol")
  # clearly thicker: the calcisol alone (bathycarbonate)
  r <- .rp46_cl(.rp46_edit(p, 1:4, top_cm = c(0, 25, 70, 90), bottom_cm = c(25, 70, 90, 130)))
  expect_identical(r$name, "calcisol")
  # anthroposol transforme-brunisol eutrique (p. 91)
  r <- .rp46_cl(.rp46_ex("anthroposols_transformes"))
  expect_identical(r$name, "anthroposol transform\u00e9-brunisol eutrique")
  expect_identical(r$levels$attachment, c("double", "double"))
  # calcosol-anthroposol transforme (p. 91)
  p <- .rp46_edit(.rp46_ex("calcosols"), site = list(rp_anthropic_history = "terrassettes", mechanical_mixing_depth_cm = 60))
  r <- .rp46_cl(p)
  expect_identical(r$name, "calcosol-anthroposol transform\u00e9")
  # two references outside a listed pair: one in the name, the other a candidate
  r <- .rp46_cl(.rp46_ex("calcarisols"))
  expect_false(any(r$candidates$double_with[r$candidates$code == "rendosols"] != ""))
  # the pairs with references of later releases wait for them
  later <- Filter(function(d) identical(d$status, "later"), .rp_rules()$doubles)
  expect_gt(length(later), 5L)
})

test_that("nothing attaches: [non rattach\u00e9] with the three nearest references", {
  skip_on_cran()
  p <- .rp46_mk(list(designation = c("A", "E", "BT", "C"), top_cm = c(0, 20, 45, 100), bottom_cm = c(20, 45, 100, 140),
                     oc_pct = c(2, 0.5, 0.4, 0.2), clay_pct = c(15, 12, 30, 22), bs_pct = c(60, 55, 70, 80), ph_h2o = 6),
                site = list(lat = 46, elevation_m = 200))
  r <- .rp46_cl(p)
  expect_identical(r$name, "[non rattach\u00e9]")
  expect_null(r$levels)
  expect_true(is.na(r$rsg_or_order))
  near <- r$candidates[r$candidates$nearest, ]
  expect_identical(nrow(near), 3L)
  expect_true(all(near$degree == "fail" & nzchar(near$failed)))
  expect_true(all(diff(near$n_failed) >= 0))
  # the luvisols are not implemented yet: an undecided candidate, said so
  lu <- r$candidates[r$candidates$code == "luvisols", ]
  expect_identical(lu$degree, "undecided")
  expect_false(lu$implemented)
  expect_match(lu$undecided_by, "not implemented yet", fixed = TRUE)
  expect_true("references:luvisols" %in% r$provisional_because)
  expect_true(any(grepl("not implemented yet", r$warnings, fixed = TRUE)))
  expect_true(any(grepl("intergrade", r$warnings, fixed = TRUE)))
})

test_that("a GER of a later release makes the name provisional only when it ranks first", {
  skip_on_cran()
  # a BP under a brunisol: the podzosols (rapid key 8) rank before the
  # brunisols (11a), and the name is provisional
  p <- .rp46_mk(list(designation = c("A", "S", "BP", "C"), top_cm = c(0, 20, 50, 80), bottom_cm = c(20, 50, 80, 120),
                     oc_pct = c(3, 0.8, 1, 0.3), bs_pct = c(70, 75, 60, 60), ph_h2o = 6))
  r <- .rp46_cl(p)
  expect_true("podzosols" %in% r$candidates$code)
  # the brunisols forbid a BP: nothing of these GER attaches
  expect_identical(r$name, "[non rattach\u00e9]")
  # a chernosol with a BTh: the luvisols (11b) rank after the chernosols (7c)
  r <- .rp46_cl(.rp46_ex("chernosols_melanoluviques"))
  expect_true("luvisols" %in% r$candidates$code)
  expect_identical(r$provisional_because, character(0))
  # g within 50 cm under a brunisol: the redoxisols signpost, before it
  r <- .rp46_cl(.rp46_edit(.rp46_ex("brunisols_eutriques"), 2, designation = "Sg"))
  expect_identical(r$name, "brunisol eutrique")
  expect_true("references:reductisols_redoxisols" %in% r$provisional_because)
})


# --- the result and the predicates ---------------------------------------------------

test_that("the result carries the levels, the candidates, the trace and prints", {
  skip_on_cran()
  r <- .rp46_cl(.rp46_ex("chernosols_typiques"))
  expect_identical(r$system, "RP 2008")
  expect_identical(r$rsg_or_order, "Chernosols")
  expect_identical(names(r$levels), c("level", "code", "name", "name_native", "name_en", "ger", "attachment", "page"))
  expect_identical(r$levels$name_en, "typical chernosol")
  expect_true(all(c("code", "name", "group", "degree", "rank", "in_name", "double_with", "failed", "imperfect_by",
                    "undecided_by", "excluded_by", "nearest", "name_en", "implemented", "precedence") %in% names(r$candidates)))
  expect_length(r$trace$references, 29L + sum(!r$candidates$implemented))
  expect_s3_class(r$trace$horizons, "data.frame")
  expect_identical(r$trace$rapid_key$attached[r$trace$rapid_key$entry == "7c"], TRUE)
  expect_true(any(grepl("convention of soilKey", r$warnings, fixed = TRUE)))
  out <- c(capture.output(msg <- testthat::capture_messages(print(r))), msg)
  expect_true(any(grepl("Candidates:", out, fixed = TRUE)))
  r2 <- .rp46_cl(.rp46_ex("calcosols"))
  expect_identical(r2$rsg_or_order, "[calcium-magn\u00e9sium]")
  # the report sections of the national systems read the levels
  html <- withr::with_options(list(soilKey.report_lang = "en"), .html_national_sections(r))
  expect_match(html, "chernosol typique", fixed = TRUE)
  expect_match(html, "Reference", fixed = TRUE)
})

test_that("the side tests read the tolerances of p. 64 in the tolerant pass only", {
  skip_on_cran()
  tab <- .tolerance_table(.rp_rules()$tolerances)
  p <- .rp46_ex("brunisols_eutriques")
  # a BT at 110 cm: within 120, and in the tolerant pass looked for only
  # within 100 cm (none_of direction)
  q <- .rp46_edit(p, 3, designation = "BT", top_cm = 110, bottom_cm = 140)
  q <- .rp46_edit(q, 2, bottom_cm = 110)
  expect_false(no_horizon_within_rp(q, "BT", 120)$passed)
  expect_true(.with_rule_ctx(TRUE, tab, no_horizon_within_rp(q, "BT", 120), "rp2008")$passed)
  # another system's tolerant pass leaves the RP thresholds as printed
  expect_false(.with_rule_ctx(TRUE, tab, no_horizon_within_rp(q, "BT", 120), "wrb2022")$passed)
  # a thickness of 32 cm against 35: strict FALSE, tolerant TRUE
  expect_false(thickness_rp(p, "S", min = 52)$passed)
  expect_true(.with_rule_ctx(TRUE, tab, thickness_rp(p, "S", min = 50), "rp2008")$passed)
  # 40 cm is not in the table: strict in both passes (Q5)
  expect_false(.with_rule_ctx(TRUE, tab, thickness_rp(.rp46_edit(p, 2, bottom_cm = 58), "S", min = 40), "rp2008")$passed)
  # three-valued: an undecided layer that could be the code
  d <- .rp46_mk(list(top_cm = c(0, 20), bottom_cm = c(20, 60), oc_pct = c(2, 0.5)), rp = FALSE)
  expect_true(is.na(horizon_within_rp(d, "S")$passed))
  # the codes of the files: a family head, a feature, an overlay
  expect_true(all(c("Sca", "Sci", "Sal") %in% .rp_spec("S")$codes))
  expect_identical(.rp_spec("Sca")$codes, "Sca")
  expect_identical(.rp_spec("Ck")$features, "k")
  expect_identical(.rp_spec("-g")$feature_only, "g")
  expect_identical(.rp_spec("Sa+")$overlay, "Sa")
  expect_error(.rp_spec("Qx"), "no code")
  # the predicates of the chapters
  expect_true(permafrost_rp(.rp46_ex("cryosols_histiques"))$passed)
  expect_false(permafrost_rp(.rp46_edit(p, site = list(koppen_climate = "Cfb")))$passed)
  expect_true(is.na(permafrost_rp(p)$passed))
  expect_true(carbonate_reserve_rp(.rp46_ex("calcosols"))$passed)
  expect_true(colluvial_rp(.rp46_ex("colluviosols"))$passed)
  expect_false(colluvial_rp(p)$passed)
  expect_true(is.na(anthropic_history_rp(.rp46_edit(p, site = list(rp_anthropic_history = "sablage")))$passed))
})

test_that("the readings of this release wait for Hugo's word", {
  skip_on_cran()
  rd <- yaml::read_yaml(system.file("rules", "rp2008", "readings.yaml", package = "soilKey"))$readings
  txt <- vapply(rd, `[[`, "", "reading")
  mine <- grepl("2026-11-03)", txt, fixed = TRUE)
  expect_gt(sum(mine), 10L)
  expect_true(all(grepl("for Hugo to confirm, 2026-11-03\\)\\.?$", txt[mine])))
})
