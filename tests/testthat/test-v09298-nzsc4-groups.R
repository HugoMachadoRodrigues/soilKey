# v0.9.298: the keys to groups of the New Zealand Soil Classification, 4th
# ed. (77 groups of the 15 orders). One profile per group, built from the
# order examples, keys to its order and its group; and the readings where
# the text is silent (inst/rules/nzsc4/readings.yaml).

.nzg_mod <- function(order, h = list(), site = list(), drop = NULL, add = NULL) {
  p <- make_nzsc4_example(order)
  hz <- as.data.frame(p$horizons, stringsAsFactors = FALSE)
  if (!is.null(drop)) hz <- hz[-drop, , drop = FALSE]
  if (!is.null(add)) { for (nm in setdiff(names(add), names(hz))) hz[[nm]] <- NA; a <- hz[rep(1, nrow(as.data.frame(add))), ]; a[] <- NA
    for (nm in names(add)) a[[nm]] <- add[[nm]]; hz <- rbind(hz, a); hz <- hz[order(hz$top_cm), ] }
  for (nm in names(h)) {
    v <- h[[nm]]
    if (is.list(v)) { if (!nm %in% names(hz)) hz[[nm]] <- NA; for (k in names(v)) hz[[nm]][as.integer(k)] <- v[[k]] }
    else hz[[nm]] <- if (length(v) == 1L) rep(v, nrow(hz)) else v
  }
  PedonRecord$new(site = utils::modifyList(p$site, site), horizons = ensure_horizon_schema(data.table::as.data.table(hz)))
}
.nzg_cases <- function() list(
  # ---- O
  OL = .nzg_mod("organic", h = list(designation = c("Lm", "Lh", "Lh", "C"), field_texture = NA), site = list(drainage_class = "well drained")),
  OF = .nzg_mod("organic", h = list(designation = c("Of", "Of", "Oh", "C"))),
  OM = .nzg_mod("organic", h = list(designation = c("Of", "Om", "Om", "C"))),
  OH = .nzg_mod("organic", h = list(designation = c("Oh", "Oh", "Om", "C"))),
  # ---- G
  GU = .nzg_mod("gley", h = list(ph_h2o = c(5.6, 3.6, 3.5, 6.2, 6.3), jarosite_present = list(`2` = TRUE, `3` = TRUE))),
  GT = .nzg_mod("gley", h = list(layer_origin = "tephra")),
  GS = .nzg_mod("gley", h = list(field_texture = c("loamy sand", "sand", "sand", "sand", "sand"), clay_pct = 3, silt_pct = 5, sand_pct = 92)),
  GX = .nzg_mod("gley", h = list(designation = c("Ap", "Bg1", "Bog2", "Bog3", "Cg"), bottom_cm = c(20, 45, 75, 100, 130))),
  GR = .nzg_mod("gley", h = list(designation = c("Ap", "Cg1", "Cg2", "Cg3", "Cg4"), stratification_pattern = list(`2` = "continuous"))),
  GA = .nzg_mod("gley", h = list(ph_h2o = c(5.0, 4.6, 4.6, 5.0, 5.2))),
  GO = make_nzsc4_example("gley"),
  # ---- U
  UD = .nzg_mod("ultic", h = list(designation = c("Ah", "Ed", "Bt1", "Bt2", "BC"))),
  UE = .nzg_mod("ultic", h = list(designation = c("Ah", "Eg", "Bt1", "Bt2", "BC"), structure_type = c("granular", "platy", "prismatic", "prismatic", "blocky"))),
  UP = .nzg_mod("ultic", h = list(designation = c("Ah", "E", "Btg1", "Bt2", "BC"), redoximorphic_features_pct = c(0, 10, 20, 5, 0), munsell_value_moist = c(4, 6, 6, 5, 6), munsell_chroma_moist = c(2, 1, 2, 6, 4))),
  US = .nzg_mod("ultic", h = list(sand_pct = c(40, 32, 62, 64, 34), clay_pct = c(20, 18, 36, 35, 36), silt_pct = c(40, 50, 2, 1, 30), field_texture = c("loam", "silt loam", "sandy clay", "sandy clay", "clay loam"))),
  UY = make_nzsc4_example("ultic"),
  # ---- Z
  ZD = .nzg_mod("podzol", h = list(designation = c("Lm", "Ah", "Ed", "Bh", "Bs", "C"))),
  ZP = .nzg_mod("podzol", h = list(munsell_chroma_moist = c(2, 1, 1, 3, 6, 4), packing_degree = list(`6` = "very high"), field_texture = c(NA, "sandy loam", "loamy sand", "sandy loam", "sandy loam", "silt loam"))),
  ZG = .nzg_mod("podzol", h = list(munsell_chroma_moist = c(2, 1, 1, 3, 6, 1), designation = c("Lm", "Ah", "E", "Bh", "Bs", "Cg"), bottom_cm = c(0, 8, 30, 35, 60, 120))),
  ZX = .nzg_mod("podzol", h = list(structure_grade = list(`5` = "structureless"), structure_type = list(`5` = "massive"), strength_moist = list(`5` = "firm"))),
  ZO = .nzg_mod("podzol", h = list(structure_grade = list(`4` = "weak", `5` = "weak"), structure_type = list(`4` = "blocky", `5` = "blocky"))),
  # ---- L
  LP = .nzg_mod("allophanic", h = list(designation = c("Ah", "Bg1", "Bw2", "C"), redoximorphic_features_pct = c(0, 15, 2, 0))),
  LG = .nzg_mod("allophanic", h = list(designation = c("Ah", "Bg1", "Bg2", "C"), redoximorphic_features_pct = c(0, 15, 15, 15))),
  LI = .nzg_mod("allophanic", h = list(designation = c("Ah", "Bw1", "Bx", "C"))),
  LO = make_nzsc4_example("allophanic"),
  # ---- M
  MP = .nzg_mod("pumice", h = list(designation = c("Ap", "Bg", "Bw2", "C"), redoximorphic_features_pct = c(0, 10, 0, 0), coarse_fragments_pct = 0)),
  MI = .nzg_mod("pumice", h = list(ksat_mm_h = list(`3` = 1))),
  MO = make_nzsc4_example("pumice"),
  # ---- E
  EV = .nzg_mod("melanic", h = list(cracks_width_cm = list(`2` = 1), cracks_depth_cm = list(`2` = 50))),
  EP = .nzg_mod("melanic", h = list(designation = c("Ah", "Bg", "Bk", "R"), redoximorphic_features_pct = c(0, 10, 0, NA), munsell_value_moist = c(2, 5, 5, NA), munsell_chroma_moist = c(1, 2, 3, NA))),
  ER = .nzg_mod("melanic", h = list(top_cm = c(0, 22, 40, 55), bottom_cm = c(22, 40, 55, 90))),
  EM = .nzg_mod("melanic", h = list(rock_origin = c(NA, NA, NA, "basalt"), munsell_value_moist = c(2, 3, 4, NA), munsell_chroma_moist = c(1, 2, 3, NA))),
  EO = .nzg_mod("melanic", h = list(rock_origin = c(NA, NA, "loess", "basalt"), ca_cmol = c(20, 15, 15, NA), mg_cmol = c(3, 3, 3, NA))),
  # ---- S
  SA = .nzg_mod("semiarid", h = list(designation = c("Ap", "Bt", "Bk", "C"), munsell_hue_moist = c("10YR", "7.5YR", "10YR", "2.5Y"))),
  SZ = .nzg_mod("semiarid", h = list(designation = c("Ap", "Bt", "Bk", "C"), structure_type = c("granular", "prismatic", "blocky", "massive"), clay_films_amount = list(`2` = "many"), coatings_value_moist = list(`2` = 3), ph_h2o = c(6.8, 8.8, 8.2, 8.3))),
  SJ = .nzg_mod("semiarid", h = list(designation = c("Ap", "Bt", "Bk", "C"))),
  SI = make_nzsc4_example("semiarid"),
  # ---- X
  XP = .nzg_mod("oxidic", h = list(designation = c("Ah", "Bog1", "Bo2", "BC"), redoximorphic_features_pct = c(0, 10, 0, 0), munsell_value_moist = c(3, 5, 3, 4), munsell_chroma_moist = c(3, 2, 6, 6))),
  XN = .nzg_mod("oxidic", h = list(designation = c("Ah", "Boi1", "Bo2", "BC"))),
  XO = make_nzsc4_example("oxidic"),
  # ---- N
  NP = .nzg_mod("granular", h = list(designation = c("Ah", "Bg(h)1", "Bw(h)2", "BC"), redoximorphic_features_pct = c(0, 10, 0, 0), munsell_value_moist = c(3, 5, 5, 5), munsell_chroma_moist = c(2, 2, 6, 6))),
  NE = .nzg_mod("granular", h = list(ph_h2o = c(6.0, 6.1, 6.2, 6.2))),
  NX = .nzg_mod("granular", h = list(ped_size_max_mm = c(5, 10, 5, 5), stickiness = "very sticky", plasticity = "very plastic", structure_type = c("granular", "polyhedral", "polyhedral", "polyhedral"))),
  NO = make_nzsc4_example("granular"),
  # ---- P
  PP = .nzg_mod("pallic", h = list(designation = c("Ap", "Bg", "Bx", "C"), redoximorphic_features_pct = c(0, 15, 2, 0), munsell_value_moist = c(4, 6, 6, 6), munsell_chroma_moist = c(2, 2, 4, 4))),
  PU = .nzg_mod("pallic", h = list(designation = c("Ap", "Bw", "Bqm", "C"))),
  PX = make_nzsc4_example("pallic"),
  PL = .nzg_mod("pallic", h = list(designation = c("Ap", "Bw", "Bt(x) lamellae", "C"))),
  PJ = .nzg_mod("pallic", h = list(designation = c("Ap", "Bt", "BC", "C"))),
  PI = .nzg_mod("pallic", h = list(designation = c("Ap", "Bw", "BC", "C"))),
  # ---- B
  BL = .nzg_mod("brown", h = list(phosphate_retention_pct = c(60, 90, 90, 40))),
  BS = .nzg_mod("brown", h = list(field_texture = "loamy sand", clay_pct = 4, silt_pct = 8, sand_pct = 88)),
  BX = .nzg_mod("brown", h = list(munsell_value_moist = c(3, 4, 4, 6), consistence_moist = "friable", structure_type = c("granular", "polyhedral", "polyhedral", "massive"), structure_grade = c("moderate", "moderate", "moderate", "structureless"), ped_size_max_mm = c(5, 8, 8, NA))),
  BM = .nzg_mod("brown", h = list(munsell_value_moist = c(3, 4, 4, 6), consistence_moist = "firm", structure_type = c("granular", "polyhedral", "polyhedral", "massive"), structure_grade = c("moderate", "moderate", "moderate", "structureless"), ped_size_max_mm = c(5, 15, 15, NA))),
  BA = .nzg_mod("brown", h = list(ph_h2o = 4.6)),
  BF = .nzg_mod("brown", h = list(structure_grade = c("moderate", "weak", "structureless", "structureless"), structure_type = c("granular", "blocky", "massive", "massive"), strength_moist = list(`3` = "firm"))),
  BO = make_nzsc4_example("brown"),
  # ---- A
  AT = .nzg_mod("anthropic", h = list(designation = c("eA", "eC1", "Bw1", "Bw2", "C"), top_cm = c(0, 10, 25, 70, 90), bottom_cm = c(10, 25, 70, 90, 120), phosphate_retention_pct = 30, layer_origin = c("fill", "fill", NA, NA, NA)), site = list(anthropic_truncation = TRUE)),
  AO = .nzg_mod("anthropic", h = list(maori_cultural_layer = list(`1` = TRUE, `2` = TRUE))),
  AR = .nzg_mod("anthropic", site = list(methane_odour = TRUE)),
  AM = .nzg_mod("anthropic", site = list(mechanical_mixing_depth_cm = 45)),
  AF = make_nzsc4_example("anthropic"),
  # ---- R
  RH = .nzg_mod("recent", site = list(soil_temp_30cm_C = 20, mean_annual_air_temp_C = 12)),
  RX = .nzg_mod("recent", h = list(designation = c("Ah", "C1", "R"), top_cm = c(0, 15, 20), bottom_cm = c(15, 20, 40))),
  RS = .nzg_mod("recent", h = list(field_texture = c("silt loam", "loamy sand", "sand"), layer_origin = "dune sand"), site = list(landform = "dune")),
  RF = make_nzsc4_example("recent"),
  RT = .nzg_mod("recent", h = list(layer_origin = "tephra", field_texture = c("silt loam", "sandy loam", "sandy loam"))),
  RO = .nzg_mod("recent", h = list(layer_origin = "loess"), site = list(landform = "hill slope")),
  # ---- W
  WW = .nzg_mod("raw", site = list(surface_water = "most of the time")),
  WG = .nzg_mod("raw", h = list(childs_test = c("positive", NA))),
  WH = .nzg_mod("raw", site = list(soil_temp_30cm_C = 25, mean_annual_air_temp_C = 12)),
  WC = .nzg_mod("raw", h = list(permafrost_temp_C = -2)),
  WX = .nzg_mod("raw", h = list(designation = c("C1", "R"), top_cm = c(0, 20), bottom_cm = c(20, 60))),
  WS = make_nzsc4_example("raw"),
  WF = .nzg_mod("raw", h = list(field_texture = "silt loam", clay_pct = 15, silt_pct = 60, sand_pct = 25, layer_origin = "alluvium")),
  WT = .nzg_mod("raw", h = list(field_texture = "loam", clay_pct = 10, silt_pct = 40, sand_pct = 50, coarse_fragments_pct = 70, layer_origin = "tephra", rock_origin = "basaltic scoria")),
  WO = .nzg_mod("raw", h = list(field_texture = "silt loam", clay_pct = 15, silt_pct = 60, sand_pct = 25))
)

test_that("every group of the 15 orders is reached by its profile", {
  skip_on_cran()
  cases <- .nzg_cases()
  expect_length(cases, 77L)
  # the group level (since 0.9.299 the name may go on to a subgroup)
  got <- vapply(cases, function(p) { lv <- classify_nzsc4(p, on_missing = "silent")$levels
                                     lv$code[lv$level == "groups"] }, "")
  expect_identical(unname(got), names(cases))
})

test_that("the group keys are registered and validate", {
  skip_on_cran()
  r <- load_rules("nzsc4")
  expect_length(r$groups, 15L)
  expect_identical(sum(lengths(r$groups)), 77L)
  expect_length(soilKey:::.validate_rules("nzsc4")$missing_predicates %||% character(0), 0L)
  res <- classify_nzsc4(make_nzsc4_example("brown"), on_missing = "silent")
  expect_identical(res$levels$code, c("B", "BO"))
  expect_identical(res$name, "Orthic Brown Soils")
})

test_that("hydrothermal: neither temperature reads as not; one alone is open", {
  skip_on_cran()
  p <- make_nzsc4_example("recent")
  expect_false(soilKey:::.nzsc_hydrothermal(p))
  q <- p$clone(deep = TRUE); q$site$soil_temp_30cm_C <- 18
  expect_true(is.na(soilKey:::.nzsc_hydrothermal(q)))
  q$site$mean_annual_air_temp_C <- 16.5
  expect_false(soilKey:::.nzsc_hydrothermal(q))
  q$site$mean_annual_air_temp_C <- 15
  expect_true(soilKey:::.nzsc_hydrothermal(q))
})

test_that("slowly permeable layer: Ksat decides, then packing, and the named pans count", {
  skip_on_cran()
  x <- soilKey:::.nzsc_horizons(make_nzsc4_example("pallic"))
  sp <- soilKey:::.nzsc_slowly_permeable_rows(x)
  expect_identical(sp, c(FALSE, FALSE, TRUE, FALSE))       # the Bx fragipan
  p <- .nzg_mod("pallic", h = list(ksat_mm_h = list(`3` = 20), packing_degree = list(`2` = "high")))
  sp <- soilKey:::.nzsc_slowly_permeable_rows(soilKey:::.nzsc_horizons(p))
  expect_identical(sp[2:3], c(TRUE, FALSE))                # packed Bw; measured Ksat over the pan
})

test_that("tephric material is open when nothing is recorded and not tephric for another origin", {
  skip_on_cran()
  p <- .nzg_mod("brown", h = list(rock_origin = NA, volcanic_glass_pct = NA))
  t <- soilKey:::.nzsc_tephric_rows(p, soilKey:::.nzsc_horizons(p))
  expect_true(all(is.na(t[1:3])))
  p <- .nzg_mod("brown", h = list(layer_origin = "loess"))
  expect_false(any(soilKey:::.nzsc_tephric_rows(p, soilKey:::.nzsc_horizons(p))))
  p <- .nzg_mod("brown", h = list(volcanic_glass_pct = 45))
  expect_true(all(soilKey:::.nzsc_tephric_rows(p, soilKey:::.nzsc_horizons(p))))
})

test_that("a sandy window with no horizon in it is undecided", {
  skip_on_cran()
  p <- .nzg_mod("recent", h = list(designation = c("Ap1", "Ap2", "C"), bottom_cm = c(15, 70, 120), top_cm = c(0, 15, 70)))
  expect_true(is.na(sandy_group_nzsc(p, from = "a_base")$passed))
})

test_that("peaty topsoil: saturation by days or drainage, waived when artificially drained", {
  skip_on_cran()
  p <- .nzg_mod("gley", h = list(field_texture = c("peaty silt loam", "silty clay loam", "silty clay loam", "silt loam", "silt loam")))
  x <- soilKey:::.nzsc_horizons(p)
  expect_true(soilKey:::.nzsc_peaty_topsoil(p, x))         # poorly drained
  q <- p$clone(deep = TRUE); q$site$drainage_class <- "well drained"
  expect_false(soilKey:::.nzsc_peaty_topsoil(q, x))
  q$site$artificially_drained <- TRUE
  expect_true(soilKey:::.nzsc_peaty_topsoil(q, x))
})

test_that("perch-gley features: a mottled horizon over a less mottled one", {
  skip_on_cran()
  p <- .nzg_mod("pallic", h = list(redoximorphic_features_pct = c(0, 10, 2, 0)))
  expect_true(soilKey:::.nzsc_perch_gley(soilKey:::.nzsc_horizons(p)))
  p <- .nzg_mod("pallic", h = list(redoximorphic_features_pct = c(0, 0, 0, 0)))
  expect_false(soilKey:::.nzsc_perch_gley(soilKey:::.nzsc_horizons(p)))
})

# ---- regressions from the two reviews of 2026-10-28 ---------------------------

.nzg_pedon <- function(h, site = list()) {
  n <- max(lengths(h)); h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  PedonRecord$new(site = utils::modifyList(list(id = "t", country = "NZ", designation_system = "nzsc",
                                                drainage_class = "well drained"), site),
                  horizons = ensure_horizon_schema(data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}

test_that("a pan is a subhorizon of the B (Pan Podzol Soils)", {
  skip_on_cran()
  p <- .nzg_pedon(list(designation = c("Ah", "E", "Bh", "Bsm", "C"), top_cm = c(0, 8, 30, 35, 60),
                       bottom_cm = c(8, 30, 35, 60, 110), structure_grade = "structureless", structure_type = "massive",
                       strength_moist = c(NA, NA, NA, "very firm", NA)))
  expect_true(pan_podzol_nzsc(p)$passed)
})

test_that("a horizon whose top lies above the window does not start there (GX)", {
  skip_on_cran()
  p <- .nzg_pedon(list(designation = c("Ah", "Bo1", "Bo2", "C"), top_cm = c(0, 10, 30, 90), bottom_cm = c(10, 30, 90, 120)))
  expect_false(oxidic_gley_nzsc(p)$passed)
})

test_that("an in situ AB is not truncated; a B ending at 60 cm is read at its base", {
  skip_on_cran()
  p <- .nzg_pedon(list(designation = c("AB", "Bw", "C"), top_cm = c(0, 15, 50), bottom_cm = c(15, 50, 100)),
                  list(anthropic_truncation = TRUE))
  expect_false(truncated_anthropic_nzsc(p)$passed)
  p <- .nzg_pedon(list(designation = c("Ah", "Bw", "C"), top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 100),
                       munsell_value_moist = c(2, 3, 5), munsell_chroma_moist = c(1, 4, 4)))
  expect_true(mafic_group_nzsc(p, colour = "melanic")$passed)
})

test_that("fresh litter is not organic soil material; other organic material keeps the group open", {
  skip_on_cran()
  p <- .nzg_pedon(list(designation = c("Lf", "Lm", "Lh", "C"), top_cm = c(-20, -15, -8, 0), bottom_cm = c(-15, -8, 0, 40),
                       oc_pct = c(45, 40, 35, 2)))
  expect_true(litter_organic_nzsc(p)$passed)
  p <- .nzg_pedon(list(designation = c("Ah", "Bw"), top_cm = c(0, 40), bottom_cm = c(40, 90), oc_pct = c(25, 1)),
                  list(drainage_class = "very poorly drained"))
  r <- classify_nzsc4(p, on_missing = "silent")
  expect_identical(r$levels$code[1], "O")
  expect_true(length(grep("^groups:", r$provisional_because)) > 0L)
})

test_that("Gley Raw reads from the mineral soil surface down; a buried floodplain is not fluvial", {
  skip_on_cran()
  p <- .nzg_pedon(list(designation = c("Oh", "C1", "C2"), top_cm = c(0, 5, 40), bottom_cm = c(5, 40, 100),
                       oc_pct = c(30, 0.2, 0.1), childs_test = "negative"))
  expect_false(gley_raw_nzsc(p)$passed)
  p <- .nzg_pedon(list(designation = c("A", "C1", "C2", "C3"), top_cm = c(0, 20, 60, 90), bottom_cm = c(20, 60, 90, 130),
                       layer_origin = c("tephra", "tephra", "alluvium", "alluvium")), list(landform = "floodplain"))
  expect_false(fluvial_group_nzsc(p, from = "mineral_surface")$passed)
})

test_that("clay lamellae make an argillic horizon only at 15 cm in all", {
  skip_on_cran()
  thin <- .nzg_pedon(list(designation = c("A", "Bw lamellae", "C"), top_cm = c(0, 30, 33), bottom_cm = c(30, 33, 100)))
  expect_false(soilKey:::.nzsc_argillic_rows(soilKey:::.nzsc_horizons(thin))[2])
  thick <- .nzg_pedon(list(designation = c("A", "Bw lamellae", "C"), top_cm = c(0, 30, 50), bottom_cm = c(30, 50, 100)))
  expect_true(soilKey:::.nzsc_argillic_rows(soilKey:::.nzsc_horizons(thick))[2])
})

test_that("site fields of length 0 or 2 do not stop a gate", {
  skip_on_cran()
  q <- make_nzsc4_example("melanic"); q$site$parent_material <- character(0)
  expect_no_error(mafic_group_nzsc(q, colour = "melanic"))
  q$site$parent_material <- c("basalt", "loess")
  expect_no_error(classify_nzsc4(q, on_missing = "silent"))
})
