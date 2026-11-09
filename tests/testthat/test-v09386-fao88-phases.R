# v0.9.386: the phases of the FAO-Unesco Revised Legend (1988), ch. VIII,
# pp. 67-71, and the texture and slope classes of the map symbol (Appendix 2,
# p. 107), given in $phases of classify_fao() beside the unit (Q25). Each
# phase present, absent or undecided with near misses at the printed numbers;
# the "not shown for" rules; the unit never changed by the phases.

.f386 <- function(p) classify_fao(p, on_missing = "silent")
.ph386 <- function(p) .f386(p)$phases
.src386 <- function(...) testthat::test_path("..", "..", ...)
# the criterion (TRUE / FALSE / NA) and the status of one phase
.cr386 <- function(p, id) { ph <- .ph386(p); ph$criterion[ph$kind == "phase" & ph$code %in% id] }
.st386 <- function(p, id) { ph <- .ph386(p); ph$status[ph$kind == "phase" & ph$code %in% id] }
.fl386 <- function(p, id) { ph <- .ph386(p); ph$flags[ph$kind == "phase" & ph$code %in% id] }
# the Eutric Cambisol of make_fao_example() (A 0-20, Bw 20-60, C 60-130),
# with horizon columns and site entries changed
.cm386 <- function(rows = NULL, ..., site = list()) {
  p <- make_fao_example("CMe")
  h <- as.data.frame(p$horizons)
  if (!is.null(rows)) h <- h[rows, , drop = FALSE]
  v <- list(...)
  for (k in names(v)) h[[k]] <- v[[k]]
  s <- utils::modifyList(p$site, site)
  suppressWarnings(PedonRecord$new(site = s, horizons = ensure_horizon_schema(data.table::as.data.table(h))))
}
# the same with one more layer below (top, bottom and columns)
.cm386_add <- function(top, bottom, ..., site = list()) {
  p <- make_fao_example("CMe")
  h <- as.data.frame(p$horizons)
  h <- h[c(1, 2, 3, 3), , drop = FALSE]
  h$top_cm <- c(0, 20, 60, top); h$bottom_cm <- c(20, 60, top, bottom)
  v <- list(...)
  for (k in names(v)) { if (!k %in% names(h)) h[[k]] <- NA; h[[k]][4] <- v[[k]] }
  s <- utils::modifyList(p$site, site)
  suppressWarnings(PedonRecord$new(site = s, horizons = ensure_horizon_schema(data.table::as.data.table(h))))
}
.IDS386 <- c("anthraquic", "duripan", "fragipan", "gelundic", "gilgai", "inundic", "lithic",
             "petroferric", "phreatic", "placic", "rudic", "salic", "skeletic", "sodic",
             "takyric", "yermic")

test_that("the 16 phases and the two map classes are given beside the unit, as printed", {
  skip_on_cran()
  r <- .f386(make_fao_example("CMe"))
  ph <- r$phases
  expect_s3_class(ph, "data.frame")
  expect_identical(ph$code[ph$kind == "phase"], .IDS386)
  expect_identical(ph$kind[17:18], c("texture_class", "slope_class"))
  expect_true(all(c("kind", "code", "name", "name_native", "name_pt", "status", "present",
                    "criterion", "page", "basis", "flags", "missing", "layers") %in% names(ph)))
  expect_true(all(ph$status %in% c("present", "absent", "undecided", "not shown")))
  expect_identical(ph$name[ph$code %in% "lithic"], "Lithic phase")
  expect_identical(ph$name_pt[ph$code %in% "lithic"], "Fase lítica")
  expect_identical(ph$page[ph$code %in% "phreatic"], "69")
  # the yaml numbers are those of the thresholds table
  y <- yaml::read_yaml(system.file("rules", "fao1988", "phases.yaml", package = "soilKey"))
  th <- yaml::read_yaml(system.file("rules", "fao1988", "thresholds.yaml", package = "soilKey"))$phases
  g <- function(id) Find(function(e) identical(e$id, id), y$phases)
  expect_equal(g("lithic")$within_cm, th$lithic_rock_within_cm$value)
  expect_equal(g("salic")$ece_gt_ds_m, th$salic_ece_gt_ds_m$value)
  expect_equal(g("salic")$within_cm, th$salic_within_cm$value)
  expect_equal(g("sodic")$esp_gt_pct, th$sodic_esp_gt_pct$value)
  expect_equal(g("skeletic")$coarse_min_pct_vol, th$skeletic_coarse_min_pct_vol$value)
  expect_equal(g("skeletic")$thickness_min_cm, th$skeletic_thickness_min_cm$value)
  expect_equal(g("skeletic")$within_cm, th$skeletic_within_cm$value)
  expect_equal(g("inundic")$days_gt, th$inundic_days_gt$value)
  expect_equal(g("phreatic")$within_cm, th$phreatic_water_table_within_cm$value)
  expect_equal(g("anthraquic")$stagnic_within_cm, th$anthraquic_stagnic_within_cm$value)
  expect_equal(g("yermic")$oc_lt_pct, th$yermic_oc_lt_pct$value)
  expect_equal(g("yermic")$oc_lt_pct_coarse, th$yermic_oc_lt_pct_coarse$value)
  expect_equal(g("yermic")$pitted_quartz_min_pct, th$yermic_pitted_quartz_min_pct$value)
  expect_equal(g("yermic")$palygorskite_min_pct, th$yermic_palygorskite_min_pct$value)
  expect_equal(g("yermic")$palygorskite_within_cm, th$yermic_palygorskite_within_cm$value)
  for (id in c("duripan", "fragipan", "petroferric", "placic"))
    expect_equal(g(id)$within_cm, th[[paste0(id, "_within_cm")]]$value)
  expect_true(all(vapply(y$phases, function(e) nzchar(e$name_pt %||% ""), NA)))
})

test_that("the unit and grouping are never changed by the phases", {
  skip_on_cran()
  for (k in c("CMe", "LPq", "SCn", "SNh", "FLs", "GLe", "LVj", "VRk", "HSs", "ARh")) {
    p <- make_fao_example(k)
    a <- .f386(p)
    b <- soilKey:::.classify_fao1988_impl(p, on_missing = "silent")
    expect_identical(a$name, b$name)
    expect_identical(a$levels, b$levels)
    expect_identical(a$provisional_because, b$provisional_because)
    expect_null(b$phases)
    # site records that only phases read leave the unit as it is
    p2 <- p$clone(deep = TRUE)
    p2$site <- utils::modifyList(p2$site, list(slope_pct = 45, flooding_days_growing_period = 30,
      rudic_recorded = TRUE, patterned_ground = "earth polygons", gilgai_presence = TRUE,
      groundwater_depth_cm = 300))
    c2 <- .f386(p2)
    expect_identical(c2$name, a$name)
    expect_identical(c2$levels, a$levels)
    expect_identical(c2$phases$status[c2$phases$code %in% "rudic"], "present")
  }
})

test_that("lithic phase: continuous hard rock within 50 cm", {
  skip_on_cran()
  rock <- function(t) .cm386(rows = c(1, 2, 3), top_cm = c(0, 20, t), bottom_cm = c(20, t, t + 50),
                             designation = c("A", "Bw", "R"))
  expect_true(.cr386(rock(50), "lithic"))
  expect_false(.cr386(rock(51), "lithic"))
  expect_false(.cr386(make_fao_example("CMe"), "lithic"))
  # shown for the Leptosols too (no exclusion in 1988)
  expect_identical(.st386(make_fao_example("LPq"), "lithic"), "present")
})

test_that("salic phase: ECe above 4 dS/m within 100 cm; not shown for Solonchaks", {
  skip_on_cran()
  expect_true(.cr386(.cm386(ec_dS_m = c(0.4, 4.1, 0.4)), "salic"))
  expect_false(.cr386(.cm386(ec_dS_m = c(0.4, 4, 0.4)), "salic"))
  expect_true(is.na(.cr386(.cm386(ec_dS_m = NA_real_), "salic")))
  # an upper bound of 4 or less rules a layer out
  expect_false(.cr386(.cm386(ec_dS_m = NA_real_, ec_max_dS_m = 4), "salic"))
  expect_true(is.na(.cr386(.cm386(ec_dS_m = NA_real_, ec_max_dS_m = 4.5), "salic")))
  # depth: a layer starting at 100 counts, one at 101 does not
  expect_true(.cr386(.cm386_add(100, 140, ec_dS_m = 5), "salic"))
  expect_false(.cr386(.cm386_add(101, 140, ec_dS_m = 5), "salic"))
  ph <- .ph386(.cm386(ec_dS_m = c(0.4, 4.1, 0.4)))
  expect_identical(ph$missing[ph$code %in% "salic"], "")
  expect_true(grepl("ec_dS_m", .ph386(.cm386(ec_dS_m = NA_real_))$missing[12]))
  # Solonchaks: not shown, whatever the unit; the Salic Fluvisols keep it
  expect_identical(.st386(make_fao_example("SCh"), "salic"), "not shown")
  expect_identical(.st386(make_fao_example("SCn"), "salic"), "not shown")
  expect_identical(.st386(make_fao_example("FLs"), "salic"), "present")
})

test_that("sodic phase: ESP above 6 within 100 cm; not shown for Solonetz and Sodic Solonchaks", {
  skip_on_cran()
  expect_true(.cr386(.cm386(esp_pct = c(2, 6.1, 2)), "sodic"))
  expect_false(.cr386(.cm386(esp_pct = c(2, 6, 2)), "sodic"))
  # from Na and the CEC, flagged
  r <- .ph386(.cm386(esp_pct = NA_real_, na_cmol = c(0.1, 0.9, 0.1), cec_cmol = 12))
  expect_true(r$criterion[r$code %in% "sodic"])
  expect_true(grepl("esp_computed", r$flags[r$code %in% "sodic"]))
  expect_true(is.na(.cr386(.cm386(esp_pct = NA_real_, na_cmol = NA_real_), "sodic")))
  expect_true(.cr386(.cm386_add(100, 140, esp_pct = 7), "sodic"))
  expect_false(.cr386(.cm386_add(101, 140, esp_pct = 7), "sodic"))
  expect_identical(.st386(make_fao_example("SNh"), "sodic"), "not shown")
  expect_identical(.st386(make_fao_example("SCn"), "sodic"), "not shown")
  # a Solonchak of another unit with ESP above 6 shows it
  p <- make_fao_example("SCh")
  h <- as.data.frame(p$horizons); h$esp_pct <- c(8, 8, 8)
  p2 <- suppressWarnings(PedonRecord$new(site = p$site, horizons = ensure_horizon_schema(data.table::as.data.table(h))))
  r2 <- .f386(p2)
  if (identical(r2$levels$code[2], "SCh")) expect_identical(.st386(p2, "sodic"), "present")
})

test_that("skeletic phase: 40 % coarse fragments, 25 cm thick, top within 50 cm", {
  skip_on_cran()
  sk <- function(cf, top, bot, cem = NA_character_) {
    .cm386(rows = c(1, 2, 2, 3), top_cm = c(0, 20, top, bot), bottom_cm = c(20, top, bot, 130),
           designation = c("A", "Bw1", "Bw2", "C"), coarse_fragments_pct = c(5, 5, cf, 5),
           cementation_class = c(NA, NA, cem, NA))
  }
  expect_true(.cr386(sk(40, 40, 65), "skeletic"))
  expect_false(.cr386(sk(39, 40, 65), "skeletic"))
  expect_false(.cr386(sk(40, 40, 64), "skeletic"))
  expect_true(.cr386(sk(40, 50, 75), "skeletic"))
  expect_false(.cr386(sk(40, 51, 76), "skeletic"))
  expect_false(.cr386(sk(40, 40, 65, cem = "indurated"), "skeletic"))
  expect_true(is.na(.cr386(.cm386(coarse_fragments_pct = NA_real_), "skeletic")))
  # a run thinner than 25 cm at the base of the description is undecided
  p <- .cm386(rows = c(1, 2), top_cm = c(0, 20), bottom_cm = c(20, 40), designation = c("A", "Bw"),
              coarse_fragments_pct = c(5, 60))
  expect_true(is.na(.cr386(p, "skeletic")))
})

test_that("inundic and phreatic phases: days of flooding and the water table", {
  skip_on_cran()
  expect_true(.cr386(.cm386(site = list(flooding_days_growing_period = 11)), "inundic"))
  expect_false(.cr386(.cm386(site = list(flooding_days_growing_period = 10)), "inundic"))
  expect_true(is.na(.cr386(.cm386(), "inundic")))
  expect_true(.cr386(.cm386(site = list(groundwater_depth_cm = 500)), "phreatic"))
  expect_false(.cr386(.cm386(site = list(groundwater_depth_cm = 501)), "phreatic"))
  expect_true(is.na(.cr386(.cm386(), "phreatic")))
  expect_identical(.st386(.cm386(site = list(groundwater_depth_cm = 300)), "phreatic"), "present")
  # gleyic properties: the water table shows in the morphology
  p <- make_fao_example("GLe")
  p$site$groundwater_depth_cm <- 300
  expect_false(.cr386(p, "phreatic"))
  # not shown for Fluvisols and Gleysols
  f <- make_fao_example("FLe"); f$site$groundwater_depth_cm <- 400
  expect_true(.cr386(f, "phreatic"))
  expect_identical(.st386(f, "phreatic"), "not shown")
})

test_that("field-only phases: rudic, gelundic, gilgai; not recorded, absent", {
  skip_on_cran()
  expect_identical(.st386(.cm386(site = list(rudic_recorded = TRUE)), "rudic"), "present")
  expect_identical(.st386(.cm386(site = list(rudic_recorded = FALSE)), "rudic"), "absent")
  expect_identical(.st386(.cm386(), "rudic"), "absent")
  expect_identical(.fl386(.cm386(), "rudic"), "rudic_not_recorded_absent")
  # a stony surface alone is no rudic phase (no percentage stands in)
  expect_false(.cr386(.cm386(coarse_fragments_pct = c(80, 5, 5)), "rudic"))
  expect_true(.cr386(.cm386(site = list(patterned_ground = "earth polygons")), "gelundic"))
  expect_false(.cr386(.cm386(site = list(patterned_ground = "stripes")), "gelundic"))
  expect_false(.cr386(.cm386(), "gelundic"))
  expect_true(.cr386(.cm386(site = list(gilgai_presence = TRUE)), "gilgai"))
  expect_false(.cr386(.cm386(site = list(gilgai_presence = FALSE)), "gilgai"))
  expect_true(.cr386(.cm386(site = list(microrelief_form = "linear gilgai")), "gilgai"))
  expect_false(.cr386(.cm386(site = list(microrelief_form = "hummocky")), "gilgai"))
  expect_identical(.fl386(.cm386(), "gilgai"), "gilgai_not_recorded_absent")
})

test_that("anthraquic phase: stagnic properties within 50 cm under irrigation", {
  skip_on_cran()
  p <- make_fao_example("LVj")
  expect_false(.cr386(p, "anthraquic"))
  q <- p$clone(deep = TRUE); q$site$flood_irrigated <- TRUE
  expect_identical(.st386(q, "anthraquic"), "present")
  q <- p$clone(deep = TRUE); q$site$land_use <- "paddy rice"
  expect_true(.cr386(q, "anthraquic"))
  q <- p$clone(deep = TRUE); q$site$paddy_years_frac <- 0.8
  expect_true(.cr386(q, "anthraquic"))
  q <- p$clone(deep = TRUE); q$site$irrigated <- TRUE
  expect_true(is.na(.cr386(q, "anthraquic")))
  expect_identical(.ph386(q)$missing[1], "flood_irrigated")
  q <- p$clone(deep = TRUE); q$site$irrigated <- FALSE
  expect_false(.cr386(q, "anthraquic"))
  # irrigation without stagnic properties within 50 cm
  expect_false(.cr386(.cm386(site = list(flood_irrigated = TRUE)), "anthraquic"))
})

test_that("duripan, fragipan, petroferric and placic phases: within 100 cm", {
  skip_on_cran()
  # the pan kind recorded in the field, at 100 and 101 cm
  pan <- function(t, ...) .cm386_add(t, t + 30, ...)
  expect_true(.cr386(pan(100, pan_type = "duripan"), "duripan"))
  expect_false(.cr386(pan(101, pan_type = "duripan"), "duripan"))
  expect_true(grepl("pan_type_duripan", .fl386(pan(100, pan_type = "duripan"), "duripan")))
  expect_true(.cr386(pan(100, pan_type = "fragipan"), "fragipan"))
  expect_false(.cr386(pan(101, pan_type = "fragipan"), "fragipan"))
  expect_true(.cr386(pan(100, pan_type = "ferricrete"), "petroferric"))
  expect_false(.cr386(pan(101, pan_type = "ferricrete"), "petroferric"))
  expect_true(.cr386(pan(100, pan_type = "thin ironpan"), "placic"))
  expect_false(.cr386(pan(101, pan_type = "thin ironpan"), "placic"))
  # the USDA predicates
  expect_true(.cr386(pan(90, cementation_class = "indurated", duripan_pct = 60), "duripan"))
  expect_false(.cr386(pan(90, cementation_class = "indurated", duripan_pct = 40), "duripan"))
  expect_true(.cr386(pan(80, rupture_resistance = "very firm"), "fragipan"))
  expect_true(.cr386(pan(90, cementation_class = "indurated", plinthite_pct = 60), "petroferric"))
  expect_false(.cr386(pan(90, cementation_class = "indurated", plinthite_pct = 40,
                          coarse_fragments_pct = 10), "petroferric"))
  pl <- .cm386(rows = c(1, 2, 2, 3), top_cm = c(0, 20, 40, 40.5), bottom_cm = c(20, 40, 40.5, 130),
               designation = c("A", "Bw", "Bsm", "C"), cementation_class = c(NA, NA, "strongly", NA))
  expect_true(.cr386(pl, "placic"))
  # designations in the FAO notation, flagged
  expect_true(.cr386(pan(60, designation = "Bqm"), "duripan"))
  expect_true(grepl("duripan_from_designation", .fl386(pan(60, designation = "Bqm"), "duripan")))
  expect_true(.cr386(pan(60, designation = "Btx"), "fragipan"))
  # the WRB fragic horizon stands in only where the rupture resistance is not
  # recorded, flagged
  fr <- function(rr) pan(80, bulk_density_g_cm3 = 1.7, structure_grade = "massive",
                         rupture_resistance = rr)
  expect_true(.cr386(fr(NA_character_), "fragipan"))
  expect_true(grepl("wrb_fragic_stand_in", .fl386(fr(NA_character_), "fragipan")))
  expect_false(.cr386(fr("friable"), "fragipan"))
  # nothing recorded: absent, flagged
  expect_false(.cr386(.cm386(), "duripan"))
  expect_identical(.fl386(.cm386(), "fragipan"), "fragipan_not_recorded_absent")
})

test_that("takyric phase: heavy texture, polygonal cracks and a platy or massive crust", {
  skip_on_cran()
  tk <- function(clay, sp = 15, st = "platy") .cm386(clay_pct = c(clay, 22, 20), sand_pct = c(20, 38, 40),
                                                   polygonal_cracks_spacing_cm = c(sp, NA, NA),
                                                   structure_type = c(st, "blocky", "massive"))
  expect_true(.cr386(tk(36), "takyric"))
  expect_false(.cr386(tk(35), "takyric"))
  expect_true(.cr386(tk(40, st = "massive"), "takyric"))
  expect_false(.cr386(tk(40, st = "granular"), "takyric"))
  expect_false(.cr386(tk(40, sp = NA), "takyric"))
  expect_true(is.na(.cr386(tk(NA), "takyric")))
})

test_that("yermic phase: organic carbon of the surface 18 cm and the desert features", {
  skip_on_cran()
  ye <- function(oc, clay = 20, sand = 40, varnish = 10, ...) {
    .cm386(oc_pct = c(oc, 0.3, 0.2), clay_pct = c(clay, 22, 20), sand_pct = c(sand, 38, 40),
           silt_pct = c(100 - clay - sand, 40, 40), varnish_pct = c(varnish, NA, NA), ...)
  }
  expect_true(.cr386(ye(0.59), "yermic"))
  expect_false(.cr386(ye(0.6), "yermic"))
  expect_false(.cr386(ye(0.5, varnish = NA), "yermic"))
  expect_true(is.na(.cr386(ye(NA), "yermic")))
  # coarser than sandy loam: 0.20 %
  expect_true(.cr386(ye(0.19, clay = 3, sand = 92), "yermic"))
  expect_false(.cr386(ye(0.2, clay = 3, sand = 92), "yermic"))
  # the carbon condition waived by the salic phase
  expect_true(.cr386(ye(1.5, ec_dS_m = c(5, 0.4, 0.4)), "yermic"))
  expect_true(grepl("organic_carbon_waived_salic", .fl386(ye(1.5, ec_dS_m = c(5, 0.4, 0.4)), "yermic")))
  # each feature, with its near miss
  expect_true(.cr386(ye(0.3, varnish = NA, pitted_quartz_pct = c(10, NA, NA)), "yermic"))
  expect_false(.cr386(ye(0.3, varnish = NA, pitted_quartz_pct = c(9.9, NA, NA)), "yermic"))
  expect_true(.cr386(ye(0.3, varnish = NA, palygorskite_pct = c(NA, 2, NA)), "yermic"))
  expect_false(.cr386(ye(0.3, varnish = NA, palygorskite_pct = c(NA, 1.9, NA)), "yermic"))
  expect_false(.cr386(ye(0.3, varnish = NA, palygorskite_pct = c(NA, NA, 5)), "yermic"))
  expect_true(.cr386(ye(0.3, varnish = NA, aeolian_crack_infill = c(TRUE, NA, NA)), "yermic"))
  expect_true(.cr386(ye(0.3, varnish = NA, structure_type = c("platy", "blocky", "massive")), "yermic"))
  expect_true(.cr386(ye(0.3, varnish = NA, aeolian_morphology = c("sandsheet", NA, NA)), "yermic"))
  expect_true(.cr386(ye(0.3, varnish = NA, ventifact_pct = c(5, NA, NA)), "yermic"))
})

test_that("map symbol: the texture class of the upper 30 cm and the slope class", {
  skip_on_cran()
  cls <- function(p, kind) { ph <- .ph386(p); ph$code[ph$kind == kind] }
  tx <- function(clay, sand) .cm386(clay_pct = clay, sand_pct = sand, silt_pct = 100 - clay - sand)
  expect_identical(cls(tx(14, 71), "texture_class"), "1")
  expect_identical(cls(tx(15, 71), "texture_class"), "2")
  expect_identical(cls(tx(14, 70), "texture_class"), "2")
  expect_identical(cls(tx(35, 30), "texture_class"), "2")
  expect_identical(cls(tx(36, 30), "texture_class"), "3")
  expect_true(is.na(cls(.cm386(clay_pct = NA_real_), "texture_class")))
  sl <- function(s) .cm386(site = list(slope_pct = s))
  expect_identical(cls(sl(8), "slope_class"), "a")
  expect_identical(cls(sl(8.1), "slope_class"), "b")
  expect_identical(cls(sl(30), "slope_class"), "b")
  expect_identical(cls(sl(30.1), "slope_class"), "c")
  expect_true(is.na(cls(.cm386(), "slope_class")))
  ph <- .ph386(.cm386())
  expect_identical(ph$status[ph$kind == "slope_class"], "undecided")
  expect_identical(ph$missing[ph$kind == "slope_class"], "slope_pct")
})

test_that("not shown for a candidate: a met criterion is undecided", {
  skip_on_cran()
  rule <- list(groupings = list("SC"))
  ex <- soilKey:::.f88ph_excluded
  expect_true(ex(rule, list(grouping = "SC", unit = "SCh", cand_groupings = character(0), cand_units = character(0))))
  expect_true(is.na(ex(rule, list(grouping = "GL", unit = "GLe", cand_groupings = "SC", cand_units = character(0)))))
  expect_false(ex(rule, list(grouping = "GL", unit = "GLe", cand_groupings = "AN", cand_units = character(0))))
  expect_true(is.na(ex(list(units = list("SCn")),
                       list(grouping = "SC", unit = "SCh", cand_groupings = character(0), cand_units = "SCn"))))
  expect_false(ex(NULL, list(grouping = "SC", unit = "SCh", cand_groupings = character(0), cand_units = character(0))))
})

test_that("print and the reports show the phases; other systems unchanged", {
  skip_on_cran()
  p <- make_fao_example("LPq"); p$site$slope_pct <- 12
  r <- .f386(p)
  out <- paste(cli::cli_fmt(print(r)), collapse = "\n")
  expect_match(out, "Phases: Lithic phase")
  expect_match(out, "Phases undecided: Inundic phase; Phreatic phase")
  expect_match(out, "Map symbol: texture 2 \\(medium textured\\); slope b \\(rolling to hilly\\)")
  ln <- soilKey:::.fao_phase_lines(r)
  expect_true(any(grepl("^Lithic phase: present, p. 68\\.$", ln)))
  expect_false(any(grepl("^Rudic", ln)))
  expect_true(any(grepl("^Slope class: b Rolling to hilly, p. 107\\.$", ln)))
  old <- options(soilKey.report_lang = "pt"); on.exit(options(old))
  lp <- soilKey:::.fao_phase_lines(r)
  expect_true(any(grepl("^Fase lítica: presente, p. 68\\.$", lp)))
  options(old)
  html <- soilKey:::.html_national_sections(r)
  expect_match(html, "Phases and map symbol")
  md <- soilKey:::.rmd_national_sections(r)
  expect_match(md, "### Phases and map symbol")
  # a result without three-valued phases: no FAO lines
  w <- classify_wrb2022(make_fao_example("CMe"), on_missing = "silent")
  expect_null(soilKey:::.fao_phase_lines(w))
  cat <- yaml::read_yaml(system.file("i18n", "report_translations.yaml", package = "soilKey"))
  for (k in c("report.fao_phases", "report.fao_phases_desc", "report.fao_phase_present",
              "report.fao_phase_undecided", "report.fao_phase_not_shown",
              "report.fao_map_texture", "report.fao_map_slope")) {
    expect_true(nzchar(cat$en[[k]] %||% ""))
    expect_true(nzchar(cat$pt[[k]] %||% ""))
  }
})

test_that("the readings of this part are recorded for Hugo; the R source is ASCII", {
  skip_on_cran()
  rd <- yaml::read_yaml(system.file("rules", "fao1988", "readings.yaml", package = "soilKey"))$readings
  mine <- Filter(function(e) identical(e$version, "0.9.386"), rd)
  expect_true(length(mine) >= 10L)
  expect_true(all(vapply(mine, function(e) nzchar(e$page %||% ""), NA)))
  expect_true(all(grepl("\\((Hugo|soilKey's reading, for Hugo to confirm), 2026-11-0[89]\\)\\.$",
                        vapply(mine, `[[`, "", "reading"))))
  f <- .src386("data-raw", "fao_readings.yaml")
  if (file.exists(f)) {
    dr <- Filter(function(e) identical(e$version, "0.9.386"), yaml::read_yaml(f)$readings)
    expect_identical(dr, mine)
  }
  src <- .src386("R", "fao-phases.R")
  skip_if_not(file.exists(src))
  txt <- readLines(src, warn = FALSE)
  expect_false(any(grepl("[^\\x01-\\x7F]", txt, perl = TRUE)))
})
