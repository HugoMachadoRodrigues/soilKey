# v0.9.299: the keys to subgroups of the New Zealand Soil Classification,
# 4th ed., for the Organic, Gley and Ultic Soils (70 subgroups). One profile
# per subgroup, built from the order examples, keys to its group and its
# subgroup; and the readings where the text is silent
# (inst/rules/nzsc4/readings.yaml).

.nzs_mod <- function(order, h = list(), site = list()) {
  p <- make_nzsc4_example(order)
  hz <- as.data.frame(p$horizons, stringsAsFactors = FALSE)
  for (nm in names(h)) {
    v <- h[[nm]]
    if (is.list(v)) { if (!nm %in% names(hz)) hz[[nm]] <- NA; for (k in names(v)) hz[[nm]][as.integer(k)] <- v[[k]] }
    else hz[[nm]] <- if (length(v) == 1L) rep(v, nrow(hz)) else v
  }
  PedonRecord$new(site = utils::modifyList(p$site, site), horizons = ensure_horizon_schema(data.table::as.data.table(hz)))
}
# the order examples with the measures that decide the earlier subgroups
.nzs_o <- function(...) { h <- list(...); .nzs_mod("organic", h = utils::modifyList(list(sulfidic_s_pct = 0), h)) }
.nzs_g <- function(...) { h <- list(...); .nzs_mod("gley", h = utils::modifyList(list(sulfidic_s_pct = 0), h)) }
.nzs_u <- function(...) { h <- list(...); .nzs_mod("ultic", h = utils::modifyList(list(ca_cmol = 5, mg_cmol = 2), h)) }
.nzs_litter <- function(des) .nzs_mod("organic", h = list(designation = des, top_cm = c(0, 20, 40, 60), bottom_cm = c(20, 40, 60, 110),
                                       oc_pct = c(45, 40, 3, 1), field_texture = c(NA, NA, "sandy loam", "sandy loam")),
                                       site = list(drainage_class = "well drained"))
.nzs_of <- function(...) .nzs_o(designation = c("Of", "Of", "Oh", "C"), ...)
.nzs_oh <- function(...) .nzs_o(designation = c("Oh", "Oh", "Om", "C"), ...)
.nzs_gu <- function(...) .nzs_g(ph_h2o = c(5.6, 3.6, 3.5, 6.2, 6.3), jarosite_present = list(`2` = TRUE, `3` = TRUE), ...)
.nzs_gt <- function(...) .nzs_g(layer_origin = "tephra", ...)
.nzs_gs <- function(ft = c("loamy sand", "sand", "sand", "sand", "sand"), ...) .nzs_g(field_texture = ft, clay_pct = 3, silt_pct = 5, sand_pct = 92, ...)
.nzs_gx <- function(des = c("Ap", "Bg1", "Bog2", "Bog3", "Cg"), ...) .nzs_g(designation = des, bottom_cm = c(20, 45, 75, 100, 130), ...)
.nzs_gr <- function(...) .nzs_g(designation = c("Ap", "Cg1", "Cg2", "Cg3", "Cg4"), stratification_pattern = list(`2` = "continuous"), ...)
.nzs_ga <- function(...) .nzs_g(ph_h2o = list(`1` = 5.0, `2` = 4.6, `3` = 4.6, `4` = 5.0, `5` = 5.2), ...)
.nzs_ud <- function(des = c("Ah", "Ed", "Bt1", "Bt2", "BC"), ...) .nzs_u(designation = des, ...)
.nzs_ue <- function(des = c("Ah", "Eg", "Bt1", "Bt2", "BC"), ...)
  .nzs_u(designation = des, structure_type = c("granular", "platy", "prismatic", "prismatic", "blocky"), ...)
.nzs_up <- function(...) .nzs_u(designation = c("Ah", "E", "Btg1", "Bt2", "BC"), redoximorphic_features_pct = c(0, 10, 20, 5, 0),
                                munsell_value_moist = c(4, 6, 6, 5, 6), munsell_chroma_moist = c(2, 1, 2, 6, 4), ...)
.nzs_us <- function(...) .nzs_u(sand_pct = c(40, 32, 62, 64, 34), clay_pct = c(20, 18, 36, 35, 36), silt_pct = c(40, 50, 2, 1, 30),
                                field_texture = c("loam", "silt loam", "sandy clay", "sandy clay", "clay loam"), ...)
.nzs_peaty <- list(`1` = "peaty silt loam")
.nzs_cases <- function() list(
  # ---- O
  OLBZ = .nzs_litter(c("Lm", "Lh", "Bh", "Bs")),
  OLBG = .nzs_litter(c("Lm", "Lh", "Bg", "Cg")),
  OLO  = .nzs_litter(c("Lm", "Lh", "Bw", "C")),
  OFS  = .nzs_of(sphagnum_fibres_pct = c(90, 80, NA, NA)),
  OFU  = .nzs_of(sulfidic_s_pct = list(`3` = 0.5)),
  OFA  = .nzs_of(ph_h2o = c(4.2, 4.3, 4.8, 5.5)),
  OFM  = .nzs_of(),
  OMU  = .nzs_o(sulfidic_s_pct = list(`3` = 0.5)),
  OMA  = .nzs_o(ph_h2o = c(4.2, 4.3, 4.4, 5.5)),
  OMM  = .nzs_o(),
  OHU  = .nzs_oh(sulfidic_s_pct = list(`3` = 0.5)),
  OHA  = .nzs_oh(ph_h2o = c(4.2, 4.3, 4.4, 5.5)),
  OHM  = .nzs_oh(),
  # ---- G
  GUFQ = .nzs_gu(fluidity_class = list(`3` = "very fluid"), ec_1to5_dS_m = list(`2` = 1.5)),
  GUF  = .nzs_gu(fluidity_class = list(`3` = "moderately fluid")),
  GUO  = .nzs_gu(field_texture = .nzs_peaty),
  GUSQ = .nzs_gu(field_texture = c("loamy sand", "sand", "sand", "sand", "sand"), clay_pct = 3, silt_pct = 5, sand_pct = 92,
                 ec_1to5_dS_m = list(`2` = 1.5)),
  GUT  = .nzs_gu(),
  GTO  = .nzs_gt(field_texture = .nzs_peaty),
  GTA  = .nzs_gt(ph_h2o = list(`2` = 5.2)),
  GTT  = .nzs_gt(),
  GSOU = .nzs_gs(ft = c("peaty sand", "sand", "sand", "sand", "sand"), sulfidic_s_pct = list(`3` = 0.5)),
  GSU  = .nzs_gs(sulfidic_s_pct = list(`3` = 0.5)),
  GSO  = .nzs_gs(ft = c("peaty sand", "sand", "sand", "sand", "sand")),
  GSQ  = .nzs_gs(ec_1to5_dS_m = list(`2` = 1.5)),
  GSC  = .nzs_gs(ferric_nodules_pct = list(`2` = 5)),
  GSA  = .nzs_gs(ph_h2o = list(`2` = 5.0)),
  GST  = .nzs_gs(),
  GXN  = .nzs_gx(c("Ap", "Bg1", "Boig2", "Boig3", "Cg")),
  GXT  = .nzs_gx(),
  GROU = .nzs_gr(field_texture = .nzs_peaty, sulfidic_s_pct = list(`3` = 0.5)),
  GRU  = .nzs_gr(sulfidic_s_pct = list(`3` = 0.5)),
  GRO  = .nzs_gr(field_texture = .nzs_peaty),
  GRF  = .nzs_gr(fluidity_class = list(`3` = "moderately fluid")),
  GRQ  = .nzs_gr(ec_1to5_dS_m = list(`2` = 1.5)),
  GRC  = .nzs_gr(shelly_layer = list(`3` = TRUE)),
  GRA  = .nzs_gr(ph_h2o = list(`2` = 5.0)),
  GRT  = .nzs_gr(),
  GAO  = .nzs_ga(field_texture = .nzs_peaty),
  GAG  = .nzs_ga(designation = c("Ap", "Btg1", "Btg2", "BCg", "Cg"), clay_pct = c(22, 40, 42, 24, 20),
                 structure_grade = list(`2` = "moderate", `3` = "moderate"), structure_type = list(`2` = "polyhedral", `3` = "polyhedral"),
                 ped_size_max_mm = list(`2` = 10, `3` = 10)),
  GAY  = .nzs_ga(clay_pct = c(22, 45, 45, 24, 20), silt_pct = c(60, 30, 30, 62, 64), sand_pct = c(18, 25, 25, 14, 16),
                 clay_films_amount = list(`2` = "common", `3` = "common")),
  GAPH = .nzs_ga(designation = c("Ap", "Bg1", "Bgfm", "BCg", "Cg"), clay_films_amount = list(`2` = "many"), coatings_value_moist = list(`2` = 3)),
  GAH  = .nzs_ga(clay_films_amount = list(`2` = "many"), coatings_value_moist = list(`2` = 3)),
  GAT  = .nzs_ga(),
  GOOU = .nzs_g(field_texture = .nzs_peaty, sulfidic_s_pct = list(`3` = 0.5)),
  GOU  = .nzs_g(sulfidic_s_pct = list(`3` = 0.5)),
  GOO  = .nzs_g(field_texture = .nzs_peaty),
  GOQ  = .nzs_g(ec_1to5_dS_m = list(`2` = 1.5)),
  GOC  = .nzs_g(effervescence_class = list(`3` = "strong")),
  GOI  = .nzs_g(designation = c("Ap", "Bg1", "Bg2", "Bglm", "Cg")),
  GOE  = .nzs_g(structure_grade = list(`2` = "moderate", `3` = "moderate"), stickiness = list(`2` = "moderately sticky", `3` = "moderately sticky"),
                ph_h2o = list(`2` = 6.0, `3` = 6.0)),
  GOJ  = .nzs_g(designation = c("Ap", "Btg1", "Btg2", "BCg", "Cg")),
  GOA  = .nzs_g(ph_h2o = list(`2` = 5.2)),
  GOT  = .nzs_g(),
  # ---- U
  UDP  = .nzs_ud(c("Ah", "Ed", "Bt1", "Btg2", "BC")),
  UDM  = .nzs_ud(c("Ah", "Ed", "Bt(g)1", "Bt2", "BC")),
  UEP  = .nzs_ue(c("Ah", "Eg", "Bt1", "Btg2", "BC")),
  UEM  = .nzs_ue(c("Ah", "Eg", "Bt(g)1", "Bt2", "BC")),
  UEY  = .nzs_ue(),
  UPS  = .nzs_up(sand_pct = c(40, 32, 65, 65, 34), clay_pct = c(20, 18, 30, 30, 36), silt_pct = c(40, 50, 5, 5, 30)),
  UPT  = .nzs_up(),
  USE  = .nzs_us(designation = c("Ah", "Ea", "Bt1", "Bt2", "BC")),
  USM  = .nzs_us(designation = c("Ah", "E", "Bt(g)1", "Bt2", "BC")),
  UST  = .nzs_us(),
  UYG  = .nzs_u(ca_cmol = list(`3` = 0.5), mg_cmol = list(`3` = 4)),
  UYMZ = .nzs_u(designation = c("Ah", "E", "Bt(g)1", "Bt2", "BC"), ph_h2o = list(`2` = 4.6), coatings_value_moist = list(`3` = 3)),
  UYM  = .nzs_u(designation = c("Ah", "E", "Bt(g)1", "Bt2", "BC")),
  UYZ  = .nzs_u(ph_h2o = list(`2` = 4.6), coatings_value_moist = list(`3` = 3)),
  UYBG = .nzs_u(field_texture = c("silt loam", "silt loam", "silt loam", "clay", "clay loam"), munsell_value_moist = list(`4` = 4)),
  UYT  = .nzs_u()
)

test_that("every subgroup of the Organic, Gley and Ultic Soils is reached by its profile", {
  skip_on_cran()
  cases <- .nzs_cases()
  expect_length(cases, 70L)
  res <- lapply(cases, classify_nzsc4, on_missing = "silent")
  got <- vapply(res, function(r) { v <- r$levels$code[r$levels$level == "subgroups"]; if (length(v)) v else NA_character_ }, "")
  expect_identical(unname(got), names(cases))
  # and each is decided: no subgroup before it is left open
  open <- vapply(res, function(r) any(grepl("^subgroups:", r$provisional_because)), NA)
  expect_identical(names(cases)[open], character(0))
  # the name is the subgroup's
  expect_identical(res$GOE$name, "Melanic Orthic Gley Soils")
  expect_identical(res$UYT$name, "Typic Yellow Ultic Soils")
})

test_that("the subgroup keys are registered and validate", {
  skip_on_cran()
  r <- load_rules("nzsc4")
  ogu <- c("OL", "OF", "OM", "OH", "GU", "GT", "GS", "GX", "GR", "GA", "GO", "UD", "UE", "UP", "US", "UY")
  expect_true(all(ogu %in% names(r$subgroups)))
  expect_identical(sum(lengths(r$subgroups[ogu])), 70L)
  v <- soilKey:::.validate_rules("nzsc4")
  expect_true(all(lengths(v[c("missing", "mixed", "unsuffixed", "malformed", "codes")]) == 0L))
  # a default closes each list, except under UD and UP (the text gives none)
  last_default <- vapply(r$subgroups[ogu], function(l) isTRUE(l[[length(l)]]$tests$default), NA)
  expect_identical(names(last_default)[!last_default], c("UD", "UP"))
})

test_that("a Densipan Ultic Soil that passes no subgroup keeps the group name", {
  skip_on_cran()
  r <- classify_nzsc4(.nzs_ud(), on_missing = "silent")
  expect_identical(tail(r$levels$code, 1L), "UD")
  expect_identical(r$name, "Densipan Ultic Soils")
  expect_true(any(grepl("No class of the subgroups list under UD passed", r$warnings, fixed = TRUE)))
})

test_that("sulfidic soil material not measured is read by its field look", {
  skip_on_cran()
  x <- function(p) soilKey:::.nzsc_horizons(p)
  # the gley example: reductimorphic 5Y horizons, not measured -> undecided
  g <- make_nzsc4_example("gley")
  s <- soilKey:::.nzsc_sulfidic_rows(x(g))
  expect_true(all(is.na(s[2:5])))
  expect_false(s[1])                          # the Ap: not reductimorphic
  expect_true(any(grepl("subgroups:GOU", classify_nzsc4(g, on_missing = "silent")$provisional_because)))
  # the same horizons at 10YR cannot look sulfidic
  g2 <- .nzs_mod("gley", h = list(munsell_hue_moist = "10YR"))
  expect_false(any(soilKey:::.nzsc_sulfidic_rows(x(g2))))
  # measured decides
  g3 <- .nzs_mod("gley", h = list(sulfidic_s_pct = c(0, 0, 0.02, 0, 0)))
  expect_identical(soilKey:::.nzsc_sulfidic_rows(x(g3)), c(FALSE, FALSE, TRUE, FALSE, FALSE))
  # a black peat not measured: undecided; a brown one: not sulfidic
  o <- soilKey:::.nzsc_sulfidic_rows(x(make_nzsc4_example("organic")))
  expect_identical(o[c(1, 2)], c(FALSE, FALSE))
  expect_true(is.na(o[3]))
})

test_that("saline soil material: EC 1:5, the z suffix, and no z on designated horizons", {
  skip_on_cran()
  x <- function(p) soilKey:::.nzsc_horizons(p)
  g <- make_nzsc4_example("gley")
  expect_false(any(soilKey:::.nzsc_saline_rows(x(g))))
  gz <- .nzs_mod("gley", h = list(designation = c("Ap", "Bgz1", "Bg2", "BCg", "Cg")))
  expect_identical(soilKey:::.nzsc_saline_rows(x(gz))[1:3], c(FALSE, TRUE, FALSE))
  ge <- .nzs_mod("gley", h = list(ec_1to5_dS_m = c(0.2, 0.9, 0.5, NA, NA)))
  expect_identical(soilKey:::.nzsc_saline_rows(x(ge))[1:3], c(FALSE, TRUE, FALSE))
  gn <- .nzs_mod("gley", h = list(designation = NA_character_))
  expect_true(all(is.na(soilKey:::.nzsc_saline_rows(x(gn)))))
  # 0.8 dS/m itself is not above the limit
  expect_false(soilKey:::.nzsc_saline_rows(x(.nzs_mod("gley", h = list(ec_1to5_dS_m = 0.8))))[2])
})

test_that("the Sphagnum share and a shelly layer are recorded when met", {
  skip_on_cran()
  of <- classify_nzsc4(.nzs_of(), on_missing = "silent")
  expect_false(any(grepl("subgroups:OFS", of$provisional_because)))
  # the share is weighted over the organic material to 60 cm
  half <- classify_nzsc4(.nzs_of(sphagnum_fibres_pct = c(100, 60, NA, NA)), on_missing = "silent")
  expect_identical(tail(half$levels$code, 1L), "OFS")            # (20 x 100 + 40 x 60) / 60 = 73
  low <- classify_nzsc4(.nzs_of(sphagnum_fibres_pct = c(100, 50, NA, NA)), on_missing = "silent")
  expect_false(identical(tail(low$levels$code, 1L), "OFS"))     # 67
  gr <- classify_nzsc4(.nzs_gr(), on_missing = "silent")
  expect_identical(tail(gr$levels$code, 1L), "GRT")
})

test_that("a peaty topsoil buried under new material counts within 60 cm", {
  skip_on_cran()
  bur <- .nzs_g(designation = c("Ap", "Bg1", "Ab", "BCg", "Cg"), field_texture = list(`3` = "peat"),
                oc_pct = list(`3` = 12), clay_pct = list(`3` = 10))
  r <- classify_nzsc4(bur, on_missing = "silent")
  expect_identical(tail(r$levels$code, 1L), "GOO")
  # buried below 60 cm of the soil surface it does not
  deep <- .nzs_g(top_cm = c(0, 62, 75, 90, 110), bottom_cm = c(62, 75, 90, 110, 130),
                 designation = c("Ap", "Ab", "Bg2", "BCg", "Cg"), field_texture = list(`2` = "peat"))
  p <- soilKey:::.nzsc_peaty_or_buried(deep, soilKey:::.nzsc_horizons(deep), 0)
  expect_false(isTRUE(p))
})

test_that("the mottled profile form reads its two routes", {
  skip_on_cran()
  x <- function(...) soilKey:::.nzsc_horizons(.nzs_u(...))
  expect_true(soilKey:::.nzsc_mottled_profile_form(x(designation = c("Ah", "E", "Bt(g)1", "Bt2", "BC"))))
  # a reductimorphic horizon from 30 to 60 cm: here Bt2 starts at 60, so no
  expect_false(soilKey:::.nzsc_mottled_profile_form(x(designation = c("Ah", "E", "Bt1", "Btg2", "BC"))))
  expect_true(soilKey:::.nzsc_mottled_profile_form(x(designation = c("Ah", "E", "Bt1", "Btg2", "BC"),
                                                      top_cm = c(0, 10, 25, 40, 115), bottom_cm = c(10, 25, 40, 115, 140))))
  # one starting higher up (a gley profile form) is not the second route
  expect_false(soilKey:::.nzsc_mottled_profile_form(x(designation = c("Ah", "E", "Btg1", "Btg2", "BC"),
                                                       redoximorphic_features_pct = 0)))
})

test_that("the findings of the text review stay fixed", {
  skip_on_cran()
  last <- function(p) { r <- classify_nzsc4(p, on_missing = "silent"); list(code = tail(r$levels$code, 1L), prov = r$provisional_because) }
  # (f) horizons, and segregations without (g) on a designated horizon, are
  # not the mottled profile form (the 4th edition's Changelog)
  expect_identical(last(.nzs_u(designation = c("Ah", "E", "Bt(f)1", "Bt2", "BC")))$code, "UYT")
  expect_identical(last(.nzs_u(redoximorphic_features_pct = c(0, 0, 5, 0, 0)))$code, "UYT")
  expect_identical(last(.nzs_ud(c("Ah", "Ed", "Bt(f)1", "Bt2", "BC")))$code, "UD")
  # the organic window runs to 60 cm from the soil surface, over the
  # organic material only
  om <- .nzs_o(designation = c("A", "Om", "Om", "C"), top_cm = c(0, 30, 50, 90), bottom_cm = c(30, 50, 90, 130),
               field_texture = c("silt loam", "peat", "peat", "silt loam"), oc_pct = c(5, 40, 40, 2), ph_h2o = c(5, 4.3, 4.9, 5.5))
  expect_identical(last(om)$code, "OMA")
  # a buried podzol under the litter, written with the buried prefix
  bz <- .nzs_mod("organic", h = list(designation = c("Lm", "Lh", "bE", "bBh"), top_cm = c(0, 20, 40, 60), bottom_cm = c(20, 40, 60, 110),
                                     oc_pct = c(45, 40, 1, 3), field_texture = c(NA, NA, "sandy loam", "sandy loam"),
                                     munsell_value_moist = c(2, 2, 6, 3), munsell_chroma_moist = c(2, 2, 2, 3), sulfidic_s_pct = 0),
                 site = list(drainage_class = "well drained"))
  expect_identical(last(bz)$code, "OLBZ")
  # GAG: a B horizon that ends above 60 cm is read whole
  thin <- .nzs_ga(designation = c("Ap", "Btg1", "Cg1", "Cg2", "Cg3"), top_cm = c(0, 20, 35, 75, 100), bottom_cm = c(20, 35, 75, 100, 130),
                  clay_pct = c(22, 40, 42, 24, 20), structure_grade = c("moderate", "moderate", rep("structureless", 3)),
                  structure_type = c("granular", "polyhedral", rep("massive", 3)), ped_size_max_mm = c(5, 10, NA, NA, NA))
  expect_identical(last(thin)$code, "GAG")
  # GAG: recorded dark coatings make a cutanic horizon without (h)
  cut <- .nzs_ga(clay_pct = c(38, 40, 42, 24, 20), silt_pct = c(40, 35, 35, 62, 64), sand_pct = c(22, 25, 23, 14, 16),
                 field_texture = c("clay loam", "clay", "clay", "silt loam", "silt loam"),
                 structure_grade = c("moderate", "moderate", "moderate", "structureless", "structureless"),
                 structure_type = c("granular", "polyhedral", "polyhedral", "massive", "massive"), ped_size_max_mm = c(5, 10, 10, NA, NA),
                 clay_films_amount = c(NA, "few", "few", NA, NA), coatings_value_moist = c(NA, 3, 3, NA, NA))
  expect_identical(last(cut)$code, "GAG")
  # a recorded mineral texture with no carbon is not peat
  r <- last(.nzs_g(oc_pct = c(4.5, NA, NA, NA, NA)))
  expect_identical(r$code, "GOT")
  expect_false(any(grepl("subgroups:GOO", r$prov)))
  # UYZ admits coatings of value 4 or less only: value 5 fails
  r <- last(.nzs_u(ph_h2o = c(4.8, 4.6, 5, 5.1, 5.2), coatings_value_moist = c(NA, NA, 5, 5, NA)))
  expect_identical(r$code, "UYT")
  expect_false(any(grepl("subgroups:UYZ", r$prov)))
  # a reductimorphic horizon of a redder hue with its value not recorded may
  # still be black
  x <- soilKey:::.nzsc_horizons(.nzs_mod("gley", h = list(munsell_hue_moist = "10YR", munsell_value_moist = NA)))
  expect_true(all(is.na(soilKey:::.nzsc_sulfidic_rows(x)[2:5])))
})

test_that("the findings of the logic review stay fixed", {
  skip_on_cran()
  nz <- function(h, site = list()) PedonRecord$new(
    site = utils::modifyList(list(id = "r", country = "NZ", designation_system = "nzsc", drainage_class = "poorly drained"), site),
    horizons = ensure_horizon_schema(data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
  pass <- function(f, p, ...) f(p, ...)$passed
  # a buried peat under a peaty row that is not a topsoil (here a C)
  bur <- nz(list(designation = c("Ap", "C", "Ob", "Cg"), top_cm = c(0, 15, 25, 50), bottom_cm = c(15, 25, 50, 100),
                 oc_pct = c(3, NA, 40, 1), field_texture = c("silt loam", "peaty sand", "peat", "silt loam"), clay_pct = c(20, 5, NA, 20)),
            site = list(drainage_class = "very poorly drained"))
  expect_true(pass(peaty_subgroup_nzsc, bur, "soil_surface"))
  # ... and under an undesignated row it holds either way
  und <- nz(list(designation = c("Ap", NA, "Ob", "Cg"), top_cm = c(0, 10, 20, 50), bottom_cm = c(10, 20, 50, 100),
                 oc_pct = c(3, NA, 40, 1), field_texture = c("silt loam", NA, "peat", "silt loam")),
            site = list(drainage_class = "very poorly drained"))
  expect_true(pass(peaty_subgroup_nzsc, und, "soil_surface"))
  # an undesignated row under the A may be A: the acidic window is open
  ac <- function(d2) nz(list(designation = c("Ah", d2, "Bg", "Cg"), top_cm = c(0, 15, 25, 60), bottom_cm = c(15, 25, 60, 100),
                             oc_pct = c(4, 3, 0.5, 0.2), ph_h2o = c(6.0, 5.0, 6.0, 6.2)))
  expect_false(pass(acidic_subgroup_nzsc, ac("A2")))
  expect_true(pass(acidic_subgroup_nzsc, ac("Bw")))
  expect_true(is.na(pass(acidic_subgroup_nzsc, ac(NA))))
  # nothing described under the litter: OLBZ and OLBG undecided
  lit <- nz(list(designation = c("Lf", "Lm", "Lh"), top_cm = c(0, 10, 20), bottom_cm = c(10, 20, 50), oc_pct = 40),
            site = list(drainage_class = "well drained"))
  expect_true(is.na(pass(buried_podzol_litter_nzsc, lit)))
  expect_true(is.na(pass(gley_profile_subgroup_nzsc, lit)))
  # sulfidic and saline: a window the description does not reach is open;
  # features recorded when met stay absent
  sh <- nz(list(designation = c("Ap", "Bg"), top_cm = c(0, 20), bottom_cm = c(20, 40), oc_pct = 1, munsell_hue_moist = "5Y",
                munsell_value_moist = 5, munsell_chroma_moist = 1, sulfidic_s_pct = 0, ec_1to5_dS_m = 0.2))
  expect_true(is.na(pass(sulfidic_subgroup_nzsc, sh)))
  expect_true(is.na(pass(saline_subgroup_nzsc, sh)))
  expect_false(pass(fluid_subgroup_nzsc, sh))
  expect_false(pass(concretionary_subgroup_nzsc, sh))
  # UYBG: a gap above the clay leaves the contrast open
  ub <- function(t3) nz(list(designation = c("Ah", "Bw", "Bt", "BC"), top_cm = c(0, 15, t3, 70), bottom_cm = c(15, 40, 70, 100),
                             field_texture = c("silt loam", "silt loam", "clay", "clay loam"), munsell_value_moist = c(3, 5, 4, 5),
                             oc_pct = c(5, 1, 1, 1)))
  expect_true(pass(buried_granular_ultic_nzsc, ub(40)))
  expect_true(is.na(pass(buried_granular_ultic_nzsc, ub(41))))
  # a densipan with no top depth leaves UDP open
  d <- .nzs_mod("ultic", h = list(designation = c("Ah", "Ed", "Btg1", "Bt2", "BC"), top_cm = c(0, NA, 25, 60, 115)))
  expect_true(is.na(pass(below_layer_subgroup_nzsc, d, layer = "densipan", feature = "reductimorphic")))
  # one decisive lab value settles less silt than clay
  cl <- nz(list(designation = c("Ap", "Bg", "Cg"), top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 100), clay_pct = c(20, 60, 20)))
  expect_true(soilKey:::.nzsc_less_silt_than_clay(soilKey:::.nzsc_horizons(cl))[2])
  # a pan is not an argillic horizon (GOJ)
  pan <- .nzs_g(designation = c("Ap", "Bg1", "Bg2", "Bglm", "Cg"), clay_pct = c(22, 30, 32, 40, 20), clay_films_amount = list(`4` = "few"))
  expect_false(pass(argillic_subgroup_nzsc, pan))
})
