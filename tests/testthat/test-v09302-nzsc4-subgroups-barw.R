# v0.9.302: the keys to subgroups of the New Zealand Soil Classification,
# 4th ed., for the Brown, Anthropic, Recent and Raw Soils (109 subgroups).
# One profile per subgroup, built from the order examples, keys to its group
# and its subgroup; the groups with no subgroups keep the group name; and
# the readings where the text is silent (inst/rules/nzsc4/readings.yaml).

.nzb_mod <- function(order, h = list(), site = list()) {
  p <- make_nzsc4_example(order)
  hz <- as.data.frame(p$horizons, stringsAsFactors = FALSE)
  for (nm in names(h)) {
    v <- h[[nm]]
    if (is.list(v)) { if (!nm %in% names(hz)) hz[[nm]] <- NA; for (k in names(v)) hz[[nm]][as.integer(k)] <- v[[k]] }
    else hz[[nm]] <- if (length(v) == 1L) rep(v, nrow(hz)) else v
  }
  PedonRecord$new(site = utils::modifyList(p$site, site), horizons = ensure_horizon_schema(data.table::as.data.table(hz)))
}
# edits over a base: a list of row edits applies to the base's vector
.nzb_h <- function(base, ...) {
  new <- list(...)
  for (nm in names(new)) {
    v <- new[[nm]]; b <- base[[nm]]
    base[[nm]] <- if (is.list(v) && !is.null(b) && !is.list(b)) { for (k in names(v)) b[as.integer(k)] <- v[[k]]; b }
                  else if (is.list(v) && is.list(b)) utils::modifyList(b, v) else v
  }
  base
}
.nzb_g <- function(order, base, site = list()) function(..., site2 = list()) .nzb_mod(order, h = .nzb_h(base, ...), site = utils::modifyList(site, site2))
.nzb_mot <- list(designation = c("Ah", "Bw(g)1", "Bw2", "C"))
.nzb_wf <- list(coarse_fragments_pct = 10, weathered_fragments_pct = 80)
# ---- B
.nzb_bl <- .nzb_g("brown", list(phosphate_retention_pct = c(60, 90, 90, 40)))
.nzb_bs <- .nzb_g("brown", list(field_texture = "loamy sand", clay_pct = 4, silt_pct = 8, sand_pct = 88))
.nzb_bx <- .nzb_g("brown", list(munsell_value_moist = c(3, 4, 4, 6), consistence_moist = "friable",
                                structure_type = c("granular", "polyhedral", "polyhedral", "massive"),
                                structure_grade = c("moderate", "moderate", "moderate", "structureless"), ped_size_max_mm = c(5, 8, 8, NA)))
.nzb_bm <- .nzb_g("brown", list(munsell_value_moist = c(3, 4, 4, 6), consistence_moist = "firm",
                                structure_type = c("granular", "polyhedral", "polyhedral", "massive"),
                                structure_grade = c("moderate", "moderate", "moderate", "structureless"), ped_size_max_mm = c(5, 15, 15, NA),
                                ca_cmol = 10, mg_cmol = 3, rock_origin = "basalt"))
.nzb_ba <- .nzb_g("brown", list(ph_h2o = 4.6))
.nzb_bf <- .nzb_g("brown", list(structure_grade = c("moderate", "weak", "structureless", "structureless"),
                                structure_type = c("granular", "blocky", "massive", "massive"), strength_moist = list(`3` = "firm")))
.nzb_bo <- .nzb_g("brown", list())
# ---- A
.nzb_at <- .nzb_g("anthropic", list(designation = c("eA", "eC1", "Bw1", "Bw2", "C"), top_cm = c(0, 10, 25, 70, 90),
                                    bottom_cm = c(10, 25, 70, 90, 120), phosphate_retention_pct = 30,
                                    layer_origin = c("fill", "fill", NA, NA, NA)), site = list(anthropic_truncation = TRUE))
.nzb_ao <- .nzb_g("anthropic", list(maori_cultural_layer = list(`1` = TRUE, `2` = TRUE)))
.nzb_ar <- .nzb_g("anthropic", list(layer_origin = c("refuse", "refuse", "fill", NA, NA)), site = list(methane_odour = TRUE))
.nzb_am <- .nzb_g("anthropic", list(), site = list(mechanical_mixing_depth_cm = 45))
.nzb_af <- .nzb_g("anthropic", list(bulk_density_g_cm3 = c(1.3, 1.3, 1.3, NA, NA)))
# ---- R
.nzb_rx <- .nzb_g("recent", list(designation = c("Ah", "C1", "R"), top_cm = c(0, 15, 20), bottom_cm = c(15, 20, 40)))
.nzb_rx2 <- list(top_cm = c(0, 25, 28), bottom_cm = c(25, 28, 50), ph_h2o = c(5.0, 5.0, NA))
.nzb_rs <- .nzb_g("recent", list(field_texture = c("silt loam", "loamy sand", "sand"), layer_origin = "dune sand"), site = list(landform = "dune"))
.nzb_rf <- .nzb_g("recent", list())
.nzb_rt <- .nzb_g("recent", list(layer_origin = "tephra", field_texture = c("silt loam", "sandy loam", "sandy loam")))
.nzb_ro <- .nzb_g("recent", list(layer_origin = "loess"), site = list(landform = "hill slope"))
.nzb_rm <- c("Ah", "C(g)1", "C2")
.nzb_sulfuric <- list(ph_h2o = list(`2` = 3.5), jarosite_present = list(`2` = TRUE))
# ---- W
.nzb_ww <- .nzb_g("raw", list(), site = list(surface_water = "most of the time"))
.nzb_wg <- .nzb_g("raw", list(childs_test = c("positive", NA)))
.nzb_ws <- .nzb_g("raw", list())
.nzb_wsilt <- list(field_texture = "silt loam", clay_pct = 15, silt_pct = 60, sand_pct = 25)
.nzb_cases <- function() list(
  # ---- B
  BLM  = do.call(.nzb_bl, .nzb_mot),
  BLP  = .nzb_bl(designation = c("Ah", "Bw1", "Bfm", "C")),
  BLA  = .nzb_bl(structure_grade = list(`2` = "weak"), bulk_density_g_cm3 = list(`2` = 1.0), ph_h2o = list(`2` = 5.2)),
  BLF  = .nzb_bl(structure_grade = c("moderate", "weak", "structureless", "structureless"),
                 structure_type = c("granular", "blocky", "massive", "massive"), strength_moist = list(`3` = "firm"),
                 bulk_density_g_cm3 = list(`2` = 1.0)),
  BLT  = .nzb_bl(structure_grade = list(`2` = "weak"), bulk_density_g_cm3 = list(`2` = 1.0)),
  BLAD = .nzb_bl(ph_h2o = list(`2` = 5.2)),
  BLD  = .nzb_bl(),
  BSMP = .nzb_bs(designation = c("Ah", "Bw(g)1", "Bfm", "C")),
  BSM  = do.call(.nzb_bs, .nzb_mot),
  BSA  = .nzb_bs(ph_h2o = list(`2` = 5.2)),
  BSP  = .nzb_bs(designation = c("Ah", "E", "Bw2", "C")),
  BSX  = .nzb_bs(designation = c("Ah", "Bw1", "Bsm", "C")),
  BST  = .nzb_bs(),
  BXM  = do.call(.nzb_bx, .nzb_mot),
  BXT  = .nzb_bx(),
  BMMG = do.call(.nzb_bm, c(.nzb_mot, list(ca_cmol = list(`2` = 0.5), mg_cmol = list(`2` = 5)))),
  BMG  = .nzb_bm(ca_cmol = list(`2` = 0.5), mg_cmol = list(`2` = 5)),
  BMM  = do.call(.nzb_bm, .nzb_mot),
  BMA  = .nzb_bm(ph_h2o = list(`2` = 5.2)),
  BMT  = .nzb_bm(),
  BAO  = .nzb_ba(field_texture = list(`1` = "peaty silt loam"), site2 = list(drainage_class = "poorly drained")),
  BAMP = .nzb_ba(designation = c("Ah", "Bw(g)1", "Bfm", "C")),
  BAM  = do.call(.nzb_ba, .nzb_mot),
  BAC  = .nzb_ba(cementation_class = list(`3` = "weakly")),
  BAP  = .nzb_ba(designation = c("Ah", "Bw1", "Bfm", "C")),
  BAT  = .nzb_ba(),
  BFMW = do.call(.nzb_bf, c(.nzb_mot, .nzb_wf)),
  BFMC = do.call(.nzb_bf, c(.nzb_mot, list(cementation_class = list(`3` = "weakly")))),
  BFMP = do.call(.nzb_bf, c(.nzb_mot, list(munsell_hue_moist = list(`2` = "2.5Y")))),
  BFMA = do.call(.nzb_bf, c(.nzb_mot, list(ph_h2o = list(`2` = 5.2)))),
  BFM  = do.call(.nzb_bf, .nzb_mot),
  BFC  = .nzb_bf(cementation_class = list(`3` = "weakly")),
  BFF  = .nzb_bf(designation = c("Ah", "Bw1", "Bw2", "Bfm"), top_cm = c(0, 20, 50, 92), bottom_cm = c(20, 50, 92, 120)),
  BFL  = .nzb_bf(top_cm = c(0, 20, 62, 90), bottom_cm = c(20, 62, 90, 120), phosphate_retention_pct = list(`3` = 90)),
  BFW  = do.call(.nzb_bf, .nzb_wf),
  BFP  = .nzb_bf(munsell_hue_moist = list(`2` = "2.5Y")),
  BFA  = .nzb_bf(ph_h2o = list(`2` = 5.2)),
  BFT  = .nzb_bf(),
  BOMW = do.call(.nzb_bo, c(.nzb_mot, .nzb_wf)),
  BOMP = do.call(.nzb_bo, c(.nzb_mot, list(munsell_hue_moist = list(`2` = "2.5Y")))),
  BOMA = do.call(.nzb_bo, c(.nzb_mot, list(ph_h2o = list(`2` = 5.2)))),
  BOM  = do.call(.nzb_bo, .nzb_mot),
  BOH  = .nzb_bo(munsell_value_moist = list(`2` = 4, `3` = 4), clay_pct = 8),
  BOI  = .nzb_bo(designation = c("Ah", "Bw1", "bAh", "C")),
  BOW  = do.call(.nzb_bo, .nzb_wf),
  BOP  = .nzb_bo(munsell_hue_moist = list(`2` = "2.5Y")),
  BOA  = .nzb_bo(ph_h2o = list(`2` = 5.2)),
  BOC  = .nzb_bo(effervescence_class = list(`2` = "strong")),
  BOT  = make_nzsc4_example("brown"),
  # ---- A
  ATX  = .nzb_at(designation = c("eA", "eC1", "Bw1", "R", "R"), top_cm = c(0, 10, 25, 50, 90), bottom_cm = c(10, 25, 50, 90, 120)),
  ATT  = .nzb_at(),
  AOA  = .nzb_ao(),
  AOO  = .nzb_ao(artefacts_pct = list(`1` = 0, `2` = 0), layer_origin = list(`1` = "shell midden")),
  AOF  = .nzb_ao(artefacts_pct = list(`1` = 0, `2` = 0)),
  ARB  = .nzb_ar(layer_origin = c("fill", "fill", "fill", "refuse", "refuse")),
  ART  = .nzb_ar(),
  AMI  = .nzb_am(site2 = list(mechanical_mixing_purpose = "tephra flipping")),
  AMZ  = .nzb_am(site2 = list(mechanical_mixing_purpose = "breaking impeding horizons")),
  AMT  = .nzb_am(),
  AFU  = .nzb_af(sulfidic_s_pct = 0.5, site2 = list(acid_neutralising_amendment = TRUE)),
  AFC  = .nzb_af(bulk_density_g_cm3 = list(`2` = 1.6)),
  AFW  = .nzb_af(redoximorphic_features_pct = list(`2` = 5)),
  AFX  = .nzb_af(coarse_fragments_pct = c(10, 70, 70, 0, 0)),
  AFT  = .nzb_af(),
  # ---- R
  RXOA = do.call(.nzb_rx, c(.nzb_rx2, list(field_texture = list(`1` = "peaty silt loam"), site2 = list(drainage_class = "poorly drained")))),
  RXM  = .nzb_rx(designation = c("Ah", "C(g)1", "R")),
  RXA  = do.call(.nzb_rx, .nzb_rx2),
  RXT  = .nzb_rx(),
  RSR  = .nzb_rs(top_cm = c(0, 15, 65), bottom_cm = c(15, 65, 120), ph_h2o = list(`3` = 3.5), jarosite_present = list(`3` = TRUE)),
  RSU  = .nzb_rs(sulfidic_s_pct = list(`2` = 0.5)),
  RSM  = .nzb_rs(designation = .nzb_rm),
  RSA  = .nzb_rs(ph_h2o = list(`2` = 5.0)),
  RSK  = .nzb_rs(layer_origin = "tephric dune sand"),
  RST  = .nzb_rs(),
  RFR  = .nzb_rf(top_cm = c(0, 15, 65), bottom_cm = c(15, 65, 120), ph_h2o = list(`3` = 3.5), jarosite_present = list(`3` = TRUE)),
  RFU  = .nzb_rf(sulfidic_s_pct = list(`2` = 0.5)),
  RFMA = .nzb_rf(designation = .nzb_rm, ph_h2o = list(`2` = 5.0)),
  RFMQ = .nzb_rf(designation = .nzb_rm, ec_1to5_dS_m = list(`2` = 1.5)),
  RFQ  = .nzb_rf(ec_1to5_dS_m = list(`2` = 1.5)),
  RFMW = .nzb_rf(designation = c("Ah", "Bw(g)", "C2"), phosphate_retention_pct = list(`2` = 30)),
  RFM  = .nzb_rf(designation = .nzb_rm),
  RFAW = .nzb_rf(designation = c("Ah", "Bw", "C2"), ph_h2o = list(`2` = 5.0), phosphate_retention_pct = list(`2` = 30)),
  RFW  = .nzb_rf(designation = c("Ah", "Bw", "C2"), phosphate_retention_pct = list(`2` = 30)),
  RFA  = .nzb_rf(ph_h2o = list(`2` = 5.0)),
  RFT  = make_nzsc4_example("recent"),
  RTM  = .nzb_rt(designation = .nzb_rm),
  RTS  = .nzb_rt(coarse_fragments_pct = 60, layer_origin = "basaltic scoria"),
  RTSP = .nzb_rt(designation = c("Ah", "C1", "bBw"), coarse_fragments_pct = c(60, 60, 0),
                 layer_origin = c("basaltic scoria", "basaltic scoria", "tephra"), volcanic_glass_pct = list(`3` = 50)),
  RTBP = .nzb_rt(designation = c("Ah", "C1", "bBw"), coarse_fragments_pct = 0, volcanic_glass_pct = list(`3` = 50)),
  RTBL = .nzb_rt(designation = c("Ah", "C1", "bBw"), coarse_fragments_pct = 0, phosphate_retention_pct = list(`3` = 90)),
  RTT  = .nzb_rt(),
  ROMP = .nzb_ro(designation = c("Ah", "Bx(g)", "C2"), top_cm = c(0, 15, 28), bottom_cm = c(15, 28, 120),
                 munsell_hue_moist = list(`2` = "10YR"), munsell_chroma_moist = list(`2` = 6), phosphate_retention_pct = list(`2` = 30)),
  ROM  = .nzb_ro(designation = .nzb_rm),
  ROAW = .nzb_ro(designation = c("Ah", "Bw", "C2"), ph_h2o = list(`2` = 5.0), phosphate_retention_pct = list(`2` = 30)),
  ROW  = .nzb_ro(designation = c("Ah", "Bw", "C2"), phosphate_retention_pct = list(`2` = 30)),
  ROA  = .nzb_ro(ph_h2o = list(`2` = 5.0)),
  ROT  = .nzb_ro(),
  # ---- W
  WWU  = .nzb_ww(sulfidic_s_pct = list(`1` = 0.5)),
  WWT  = .nzb_ww(),
  WGR  = do.call(.nzb_wg, .nzb_sulfuric),
  WGFU = .nzb_wg(fluidity_class = list(`1` = "very fluid"), sulfidic_s_pct = list(`2` = 0.5)),
  WGU  = .nzb_wg(sulfidic_s_pct = list(`2` = 0.5)),
  WGFQ = .nzb_wg(fluidity_class = list(`1` = "very fluid"), ec_1to5_dS_m = list(`1` = 1.5)),
  WGQ  = .nzb_wg(ec_1to5_dS_m = list(`1` = 1.5)),
  WGF  = .nzb_wg(fluidity_class = list(`1` = "very fluid")),
  WGS  = .nzb_wg(),
  WGT  = do.call(.nzb_wg, .nzb_wsilt),
  WSK  = .nzb_ws(layer_origin = "tephric dune sand"),
  WST  = make_nzsc4_example("raw")
)

test_that("every subgroup of the Brown, Anthropic, Recent and Raw Soils is reached by its profile", {
  skip_on_cran()
  cases <- .nzb_cases()
  expect_length(cases, 109L)
  res <- lapply(cases, classify_nzsc4, on_missing = "silent")
  got <- vapply(res, function(r) { v <- r$levels$code[r$levels$level == "subgroups"]; if (length(v)) v else NA_character_ }, "")
  expect_identical(unname(got), names(cases))
  # and each is decided: no subgroup before it is left open
  open <- vapply(res, function(r) any(grepl("^subgroups:", r$provisional_because)), NA)
  expect_identical(names(cases)[open], character(0))
  expect_identical(res$ATX$name, "Rocky Truncated Anthropic Soils")
})

test_that("the subgroup keys of the four orders are registered, and seven groups have none", {
  skip_on_cran()
  r <- load_rules("nzsc4")
  g <- c("BL", "BS", "BX", "BM", "BA", "BF", "BO", "AT", "AO", "AR", "AM", "AF", "RX", "RS", "RF", "RT", "RO", "WW", "WG", "WS")
  expect_true(all(g %in% names(r$subgroups)))
  expect_identical(sum(lengths(r$subgroups[g])), 109L)
  # the whole classification: 336 subgroups
  expect_identical(sum(lengths(r$subgroups)), 336L)
  v <- soilKey:::.validate_rules("nzsc4")
  expect_true(all(lengths(v[c("missing", "mixed", "unsuffixed", "malformed", "codes")]) == 0))
  expect_true(all(vapply(r$subgroups[g], function(l) isTRUE(l[[length(l)]]$tests$default), NA)))
  # "No subgroups have been defined": RH, WH, WC, WX, WF, WT, WO keep the group name
  expect_false(any(c("RH", "WH", "WC", "WX", "WF", "WT", "WO") %in% names(r$subgroups)))
  res <- classify_nzsc4(.nzb_mod("raw", h = list(designation = c("C1", "R"), top_cm = c(0, 20), bottom_cm = c(20, 60))),
                        on_missing = "silent")
  expect_identical(res$name, "Rocky Raw Soils")
  expect_false("subgroups" %in% res$levels$level)
  # BLT has criteria and BLD is the default
  bl <- r$subgroups$BL
  expect_identical(vapply(bl, `[[`, "", "code"), c("BLM", "BLP", "BLA", "BLF", "BLT", "BLAD", "BLD"))
  expect_false(isTRUE(bl[[5]]$tests$default))
})

test_that("the Anthropic subgroups read Hugo's decisions", {
  skip_on_cran()
  # Maori subgroups from the existing fields
  expect_true(soilKey:::maori_subgroup_nzsc(.nzb_ao(artefacts_pct = list(`1` = 0, `2` = 0),
                                                    layer_origin = list(`1` = "pa terrace earthworks")), kind = "artefact")$passed)
  expect_false(soilKey:::maori_subgroup_nzsc(.nzb_ao(artefacts_pct = list(`1` = 0, `2` = 0)), kind = "organic")$passed)
  # the mixing purpose and the amendments are recorded when met
  expect_false(soilKey:::mixing_purpose_subgroup_nzsc(.nzb_am(), purpose = "tephra")$passed)
  expect_false(soilKey:::treated_sulfidic_nzsc(.nzb_af(sulfidic_s_pct = 0.5))$passed)
  spec <- soilKey:::site_attribute_spec()
  expect_true(all(c("mechanical_mixing_purpose", "acid_neutralising_amendment") %in% spec$name))
  # a coarse fill layer: 80 % or more coarse fragments and artefacts, more than 30 cm thick
  expect_false(soilKey:::coarse_fill_nzsc(.nzb_af(coarse_fragments_pct = c(10, 60, 60, 0, 0)))$passed)
  p <- .nzb_af(coarse_fragments_pct = c(10, 70, 0, 0, 0))
  expect_false(soilKey:::coarse_fill_nzsc(p)$passed)
  # a compacted fill: one fill horizon of 1.5 or more; not measured leaves it open
  expect_true(is.na(soilKey:::compacted_fill_nzsc(make_nzsc4_example("anthropic"))$passed))
  expect_false(soilKey:::compacted_fill_nzsc(.nzb_af(bulk_density_g_cm3 = list(`4` = 1.7)))$passed)
  # buried refuse known only by its odour leaves the depth open
  expect_true(is.na(soilKey:::buried_refuse_nzsc(.nzb_mod("anthropic", site = list(methane_odour = TRUE)))$passed))
  # rock within 60 cm of the soil surface (ATX), deeper does not count
  expect_false(soilKey:::contact_subgroup_nzsc(.nzb_at(designation = c("eA", "eC1", "Bw1", "Bw2", "R")),
                                               depth = 60, from = "soil_surface")$passed)
})

test_that("the Brown subgroups read their criteria", {
  skip_on_cran()
  # BOH (Hugo's decision): 10 % or less clay in every horizon to 90 cm
  p <- .nzb_bo(munsell_value_moist = list(`2` = 4, `3` = 4), clay_pct = c(8, 8, 12, 8))
  expect_false(soilKey:::humose_brown_nzsc(p)$passed)
  p <- .nzb_bo(munsell_value_moist = list(`2` = 4, `3` = 4), clay_pct = NA, field_texture = "sand")
  expect_true(soilKey:::humose_brown_nzsc(p)$passed)
  # BAC: a placic horizon is not a cemented horizon more than 10 mm thick
  expect_false(soilKey:::cemented_subgroup_nzsc(.nzb_ba(designation = c("Ah", "Bw1", "Bfm", "C")), min_mm = 10)$passed)
  expect_true(soilKey:::cemented_subgroup_nzsc(.nzb_ba(designation = c("Ah", "Bw1", "Bfm", "C")))$passed)
  # pallic colours: 10YR with chroma 4 needs one of the three; 2.5Y alone passes
  p <- .nzb_bo(munsell_chroma_moist = list(`2` = 4), phosphate_retention_pct = list(`2` = 40), naf_test = list(`2` = "strong"))
  expect_false(soilKey:::pallic_colour_subgroup_nzsc(p)$passed)
  p <- .nzb_bo(munsell_chroma_moist = list(`2` = 4), phosphate_retention_pct = list(`2` = 20))
  expect_true(soilKey:::pallic_colour_subgroup_nzsc(p)$passed)
  # BMG: Ca:Mg 0.2 or less with no Mg minimum
  expect_true(soilKey:::magnesic_brown_nzsc(.nzb_bm(ca_cmol = list(`2` = 0.2), mg_cmol = list(`2` = 1.2)))$passed)
  # BOI: a buried A deeper than 120 cm does not count
  p <- .nzb_bo(designation = c("Ah", "Bw1", "Bw2", "bAh"), top_cm = c(0, 20, 50, 125), bottom_cm = c(20, 50, 125, 150))
  expect_false(isTRUE(soilKey:::immature_brown_nzsc(p)$evidence$buried_a))
  # BFA: acid above the firm horizon only
  expect_false(soilKey:::firm_acid_brown_nzsc(.nzb_bf(ph_h2o = list(`4` = 5.0)))$passed)
})

test_that("the Recent and Raw subgroups read their windows", {
  skip_on_cran()
  # the Pallic 2(b) helper, shared with the order key
  expect_true(soilKey:::pallic_soils_nzsc(make_nzsc4_example("pallic"))$passed)
  expect_true(soilKey:::.nzsc_pallic_2b(soilKey:::.nzsc_horizons(make_nzsc4_example("pallic"))))
  expect_false(soilKey:::.nzsc_pallic_2b(soilKey:::.nzsc_horizons(make_nzsc4_example("recent"))))
  # RSA: an acid horizon only below 60 cm does not count
  expect_false(soilKey:::acid_window_subgroup_nzsc(.nzb_rs(ph_h2o = list(`3` = 5.0), top_cm = c(0, 15, 65),
                                                           bottom_cm = c(15, 65, 120)), from = 20, to = 60)$passed)
  # RFA: the surface A is not a subsurface horizon
  expect_false(soilKey:::acid_window_subgroup_nzsc(.nzb_rf(ph_h2o = list(`1` = 5.0)), to = 60, subsurface = TRUE)$passed)
  # a weathered-B horizon ending above 30 cm does not reach it
  p <- .nzb_rf(designation = c("Ah", "Bw", "C2"), top_cm = c(0, 15, 25), bottom_cm = c(15, 25, 120))
  expect_false(soilKey:::weathered_b_deep_nzsc(p)$passed)
  # a buried soil deeper than 60 cm does not count
  p <- .nzb_rt(designation = c("Ah", "C1", "bBw"), top_cm = c(0, 15, 65), bottom_cm = c(15, 65, 120),
               volcanic_glass_pct = list(`3` = 50))
  expect_false(soilKey:::buried_soil_subgroup_nzsc(p, material = "vitric")$passed)
  # WGFU reads 30 cm or less; WGF within 30 cm
  p <- .nzb_wg(fluidity_class = list(`2` = "very fluid"), top_cm = c(0, 30), bottom_cm = c(30, 100))
  expect_true(soilKey:::fluid_subgroup_nzsc(p, depth = 30, limit = "at_or_less")$passed)
  expect_false(soilKey:::fluid_subgroup_nzsc(p, depth = 30)$passed)
  # the shared gates keep their old windows
  expect_true(soilKey:::fluid_subgroup_nzsc(p)$passed)
})

test_that("the findings of the two reviews stay fixed", {
  skip_on_cran()
  r <- load_rules("nzsc4")
  expect_identical(vapply(r$subgroups$AO, `[[`, "", "name"),
                   c("Artefact Māori Anthropic Soils", "Organic Māori Anthropic Soils", "Fill Māori Anthropic Soils"))
  # BFC (Hugo's decision): a placic horizon is not the cemented horizon
  expect_false(soilKey:::cemented_subgroup_nzsc(.nzb_bf(designation = c("Ah", "Bw1", "Bfm", "C")), placic = FALSE)$passed)
  # a fragipan recorded as a pan type is not cemented
  expect_false(soilKey:::cemented_subgroup_nzsc(.nzb_bf(pan_type = list(`3` = "fragipan")))$passed)
  # RTS: scoria that stops above 60 cm does not extend to it
  p <- .nzb_rt(coarse_fragments_pct = c(80, 80, 0), layer_origin = c("basaltic scoria", "basaltic scoria", "tephra"),
               top_cm = c(0, 15, 40), bottom_cm = c(15, 40, 120))
  expect_false(soilKey:::scoriaceous_pumice_nzsc(p, depth = 60, reach = TRUE)$passed)
  # AFX: natural gravel under the fill is not a coarse fill layer
  p <- .nzb_af(coarse_fragments_pct = c(10, 10, 10, 90, 90), layer_origin = c("fill", "fill", "fill", "alluvial gravel", "alluvial gravel"))
  expect_false(soilKey:::coarse_fill_nzsc(p)$passed)
  # AOO: whole words only
  for (o in c("inorganic sand and gravel", "humus-rich topsoil", "volcanic ash"))
    expect_false(soilKey:::maori_subgroup_nzsc(.nzb_ao(artefacts_pct = list(`1` = 0, `2` = 0), layer_origin = list(`1` = o)),
                                               kind = "organic")$passed)
  expect_true(soilKey:::maori_subgroup_nzsc(.nzb_ao(artefacts_pct = list(`1` = 0, `2` = 0), layer_origin = list(`1` = "wood ash")),
                                            kind = "organic")$passed)
  # rock is neither acid nor sulfuric
  p <- .nzb_rf(designation = c("Ah", "C1", "R"), top_cm = c(0, 15, 45), bottom_cm = c(15, 45, 120), ph_h2o = c(6, 6, NA))
  expect_false(soilKey:::sulfuric_subgroup_nzsc(p)$passed)
  expect_false(soilKey:::acid_window_subgroup_nzsc(p, to = 60, subsurface = TRUE)$passed)
  expect_false(soilKey:::acidic_subgroup_nzsc(p)$passed)
  # the firm horizon of BFA is a B or BC subhorizon (not a firm A)
  p <- .nzb_bf(structure_grade = c("structureless", "weak", "structureless", "structureless"),
               structure_type = c("massive", "blocky", "massive", "massive"), strength_moist = list(`1` = "firm", `3` = "firm"),
               ph_h2o = list(`2` = 5.2))
  expect_true(soilKey:::firm_acid_brown_nzsc(p)$passed)
  # BOI: an undesignated row next to a thin pale Bw may or may not be Bw
  p <- .nzb_bo(designation = c("Ah", "Bw1", NA, "C"), top_cm = c(0, 20, 35, 85), bottom_cm = c(20, 35, 85, 120),
               munsell_chroma_moist = c(3, 4, 7, 4), structure_grade = c("moderate", "structureless", "structureless", "structureless"),
               structure_type = c("granular", "massive", "massive", "massive"), strength_moist = list(`2` = "weak"))
  expect_true(is.na(soilKey:::immature_brown_nzsc(p)$evidence$thin_b))
  # only the uppermost buried soil counts
  p <- .nzb_rt(designation = c("Ah", "bAh", "bC2"), volcanic_glass_pct = list(`3` = 50), top_cm = c(0, 40, 60),
               bottom_cm = c(40, 60, 120))
  expect_false(soilKey:::buried_soil_subgroup_nzsc(p, material = "vitric")$passed)
})
