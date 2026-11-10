# v0.9.390: the 12 phases of the FAO-Unesco Legend of 1974 (pp. 5-7) and the
# texture and slope classes of its map symbol (pp. 4-5), given in $phases of
# classify_fao(edition = "1974") beside the unit, three-valued, with the
# "not shown for" rules; the unit never changes.

.f390 <- function(p) classify_fao(p, edition = "1974", on_missing = "silent")
.ex390 <- function(code, nm = FALSE) make_fao_example(code, edition = "1974", near_miss = nm)
.IDS390 <- c("stony", "lithic", "petric", "petrocalcic", "petrogypsic", "petroferric", "phreatic",
             "fragipan", "duripan", "saline", "sodic", "cerrado")
.edit390 <- function(p, ..., site = list()) {
  h <- as.data.frame(p$horizons)
  v <- list(...)
  for (k in names(v)) h[[k]] <- if (length(v[[k]]) == 1L) rep(v[[k]], nrow(h)) else v[[k]]
  PedonRecord$new(site = utils::modifyList(p$site, site),
                  horizons = ensure_horizon_schema(data.table::as.data.table(h)))
}
.ph390 <- function(r, id) r$phases[r$phases$code %in% id & r$phases$kind == "phase", ]
.src390 <- function(...) {
  f <- testthat::test_path("..", "..", ...)
  if (file.exists(f)) f else NA_character_
}

test_that("the phases file: 12 phases in the printed order, numbers as printed", {
  skip_on_cran()
  y <- yaml::read_yaml(system.file("rules", "fao1974", "phases.yaml", package = "soilKey"))
  expect_identical(vapply(y$phases, `[[`, "", "id"), .IDS390)
  expect_true(all(vapply(y$phases, function(p) nzchar(p$name_pt %||% ""), NA)))
  ph <- function(id) Find(function(p) identical(p$id, id), y$phases)
  th <- yaml::read_yaml(system.file("rules", "fao1974", "thresholds.yaml", package = "soilKey"))$phases
  expect_equal(ph("lithic")$within_cm, th$lithic_rock_within_cm$value)
  expect_equal(ph("petric")$coarse_min_pct_vol, th$petric_coarse_min_pct_vol$value)
  expect_equal(ph("petric")$thickness_min_cm, th$petric_thickness_min_cm$value)
  expect_equal(ph("petric")$within_cm, th$petric_within_cm$value)
  expect_equal(ph("petrocalcic")$within_cm, th$petrocalcic_within_cm$value)
  expect_equal(ph("petrogypsic")$within_cm, th$petrogypsic_within_cm$value)
  expect_equal(ph("petroferric")$within_cm, th$petroferric_within_cm$value)
  expect_equal(c(ph("phreatic")$min_cm, ph("phreatic")$max_cm), unlist(th$phreatic_water_table_cm$value))
  expect_equal(ph("fragipan")$within_cm, th$fragipan_within_cm$value)
  expect_equal(ph("duripan")$within_cm, th$duripan_within_cm$value)
  expect_equal(ph("saline")$ece_gt_ds_m, th$saline_ece_gt_mmhos$value)
  expect_equal(ph("saline")$within_cm, th$saline_within_cm$value)
  expect_equal(ph("sodic")$esp_gt_pct, th$sodic_esp_gt_pct$value)
  expect_equal(ph("stony")$gravel_max_cm, th$stony_gravel_max_cm$value)
  # not shown: lithic for Lithosols, saline for Solonchaks, sodic for Solonetz
  expect_identical(ph("lithic")$not_shown_for$groupings, "I")
  expect_identical(ph("saline")$not_shown_for$groupings, "Z")
  expect_identical(ph("sodic")$not_shown_for$groupings, "S")
  expect_null(ph("phreatic")$not_shown_for)
  # the 1974 texture classes of the map symbol
  expect_identical(vapply(y$map_symbol$texture$classes, `[[`, "", "code"), c("1", "2", "3"))
  expect_identical(vapply(y$map_symbol$slope$classes, `[[`, "", "code"), c("a", "b", "c"))
})

test_that("every result of 1974 carries the 12 phases and the map symbol, the unit unchanged", {
  skip_on_cran()
  for (g in soilKey:::.F74_GROUPS$code) {
    r <- .f390(.ex390(g))
    expect_identical(r$phases$code[r$phases$kind == "phase"], .IDS390, info = g)
    expect_identical(r$phases$kind[13:14], c("texture_class", "slope_class"))
    expect_true(all(r$phases$status %in% c("present", "absent", "undecided", "not shown")))
    expect_identical(r$levels$code[1L], g, info = g)
  }
  # the phases never change the unit
  p <- .edit390(.ex390("Lc"), ec_dS_m = 6, esp_pct = 10, site = list(groundwater_depth_cm = 400))
  expect_identical(.f390(p)$name, "Chromic Luvisols")
  expect_identical(.ph390(.f390(p), c("saline", "sodic", "phreatic"))$status, rep("present", 3))
})

test_that("each phase reads its printed number", {
  skip_on_cran()
  E <- .edit390
  b <- .ex390("B")                                    # A 0-20, Bw 20-60, C 60-130
  st <- function(p, id) .ph390(.f390(p), id)$status
  # lithic: rock within 50 cm
  expect_identical(st(.edit390(b, top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 80), designation = c("A", "Bw", "R")), "lithic"),
                   "present")
  expect_identical(st(.edit390(b, top_cm = c(0, 20, 51), bottom_cm = c(20, 51, 80), designation = c("A", "Bw", "R")), "lithic"),
                   "absent")
  # saline: ECe above 4 within 100 cm
  expect_identical(st(E(b, ec_dS_m = c(1, 1, 4.5)), "saline"), "present")
  expect_identical(st(E(b, ec_dS_m = c(1, 1, 4)), "saline"), "absent")
  expect_identical(st(E(b, ec_dS_m = NA), "saline"), "undecided")
  # sodic: more than 6 % exchangeable sodium within 100 cm
  expect_identical(st(E(b, esp_pct = c(2, 7, 3)), "sodic"), "present")
  expect_identical(st(E(b, esp_pct = c(2, 6, 3)), "sodic"), "absent")
  # phreatic: a water table from 300 to 500 cm
  for (d in c(300, 500)) expect_identical(st(E(b, site = list(groundwater_depth_cm = d)), "phreatic"), "present")
  for (d in c(299, 501)) expect_identical(st(E(b, site = list(groundwater_depth_cm = d)), "phreatic"), "absent")
  expect_identical(st(b, "phreatic"), "undecided")
  # petric: 40 % coarse fragments, 25 cm thick, the top within 100 cm
  expect_identical(st(E(b, coarse_fragments_pct = c(0, 0, 40)), "petric"), "present")
  expect_identical(st(E(b, coarse_fragments_pct = c(0, 0, 39)), "petric"), "absent")
  expect_identical(st(E(b, coarse_fragments_pct = c(0, 0, 40), top_cm = c(0, 20, 101), bottom_cm = c(20, 101, 140)),
                      "petric"), "absent")
  # petrocalcic: a cemented calcic horizon within 100 cm
  pc <- E(b, designation = c("A", "Bk", "Bkm"), caco3_pct = c(2, 20, 60),
          cementation_class = c(NA, NA, "strongly"))
  expect_identical(st(pc, "petrocalcic"), "present")
  expect_identical(st(E(pc, top_cm = c(0, 20, 101), bottom_cm = c(20, 101, 140)), "petrocalcic"), "absent")
  # petrogypsic: a pan recorded as gypcrete, flagged
  pg <- E(b, pan_type = c(NA, NA, "gypcrete"))
  r <- .ph390(.f390(pg), "petrogypsic")
  expect_identical(r$status, "present")
  expect_match(r$flags, "pan_type_petrogypsic")
  # fragipan and duripan by the pan recorded, within 100 cm
  expect_identical(st(E(b, pan_type = c(NA, NA, "fragipan")), "fragipan"), "present")
  expect_identical(st(E(b, pan_type = c(NA, NA, "duripan")), "duripan"), "present")
  expect_identical(st(E(b, pan_type = c(NA, NA, "duripan"), top_cm = c(0, 20, 101), bottom_cm = c(20, 101, 140)),
                      "duripan"), "absent")
  # petroferric: an ironstone pan within 100 cm
  expect_identical(st(E(b, pan_type = c(NA, NA, "ironstone"), plinthite_pct = c(0, 0, 60),
                        cementation_class = c(NA, NA, "indurated")), "petroferric"), "present")
})

test_that("field phases: absent when not recorded", {
  skip_on_cran()
  b <- .ex390("B")
  r <- .f390(b)
  expect_identical(.ph390(r, "stony")$status, "absent")
  expect_match(.ph390(r, "stony")$flags, "stony_not_recorded_absent")
  expect_identical(.ph390(r, "cerrado")$status, "absent")
  r2 <- .f390(.edit390(b, site = list(rudic_recorded = TRUE, vegetation = "Cerrado sensu stricto")))
  expect_identical(.ph390(r2, c("stony", "cerrado"))$status, c("present", "present"))
  r3 <- .f390(.edit390(b, site = list(rudic_recorded = FALSE, vegetation = "tropical forest")))
  expect_identical(.ph390(r3, c("stony", "cerrado"))$status, c("absent", "absent"))
})

test_that("the not shown for rules of 1974", {
  skip_on_cran()
  # lithic not for the Lithosols (rock at 8 cm)
  r <- .f390(.ex390("I"))
  expect_identical(.ph390(r, "lithic")$status, "not shown")
  expect_true(.ph390(r, "lithic")$criterion)
  # lithic for a Regosol on rock at 11 cm
  expect_identical(.ph390(.f390(.ex390("I", TRUE)), "lithic")$status, "present")
  # saline not for the Solonchaks
  r <- .f390(.ex390("Z"))
  expect_identical(.ph390(r, "saline")$status, "not shown")
  # sodic not for the Solonetz
  r <- .f390(.ex390("S"))
  expect_identical(.ph390(r, "sodic")$status, "not shown")
  expect_false(.ph390(r, "sodic")$present)
  # a sodic Planosol shows it
  r <- .f390(.ex390("Ws"))
  expect_identical(.ph390(r, "sodic")$status, "present")
  # the group code of a candidate unit is its first letter (1974)
  asg <- list(grouping = "B", unit = "Be", cand_groupings = character(0), cand_units = "Zg")
  ex <- soilKey:::.f88ph_excluded(list(groupings = "Z"), asg, glen = 1L)
  expect_true(is.na(ex))
  expect_identical(attr(ex, "candidates"), "units:Zg")
})

test_that("the classes of the map symbol: the 1974 texture classes", {
  skip_on_cran()
  E <- .edit390
  b <- .ex390("B")
  cls <- function(p, k) { ph <- .f390(p)$phases; ph$code[ph$kind == k] }
  expect_identical(cls(b, "texture_class"), "2")
  # clay 16 %, sand 70 %: coarse in 1974 (below 18 and above 65), medium in 1988
  co <- E(b, clay_pct = 16, sand_pct = 70, silt_pct = 14)
  expect_identical(cls(co, "texture_class"), "1")
  ph88 <- classify_fao(co, on_missing = "silent")$phases
  expect_identical(ph88$code[ph88$kind == "texture_class"], "2")
  expect_identical(cls(E(b, clay_pct = 40, sand_pct = 20, silt_pct = 40), "texture_class"), "3")
  for (s in list(c(8, "a"), c(8.5, "b"), c(30, "b"), c(31, "c")))
    expect_identical(cls(E(b, site = list(slope_pct = as.numeric(s[1]))), "slope_class"), s[2])
  expect_true(is.na(cls(b, "slope_class")))
})

test_that("print and the report give the phases of 1974", {
  skip_on_cran()
  r <- .f390(.ex390("Z"))
  out <- paste(cli::cli_fmt(print(r)), collapse = "\n")
  expect_match(out, "Not shown for this unit: Saline phase", fixed = TRUE)
  expect_match(out, "Map symbol: texture 2", fixed = TRUE)
  lines <- soilKey:::.fao_phase_lines(r)
  expect_true(any(grepl("^Saline phase", lines)))
  expect_true(any(grepl("p\\. 4-5", lines)))
})

test_that("the readings of this part are recorded for Hugo; the R source is ASCII", {
  skip_on_cran()
  rd <- yaml::read_yaml(system.file("rules", "fao1974", "readings.yaml", package = "soilKey"))$readings
  r390 <- Filter(function(e) identical(e$version, "0.9.390"), rd)
  expect_gte(length(r390), 3L)
  f <- .src390("data-raw", "fao_readings.yaml")
  skip_if_not(!is.na(f))
  d390 <- Filter(function(e) identical(e$version, "0.9.390"), yaml::read_yaml(f)$readings)
  expect_identical(vapply(d390, `[[`, "", "topic"), vapply(r390, `[[`, "", "topic"))
  src <- .src390("R", "fao-phases.R")
  skip_if_not(!is.na(src))
  expect_false(any(grepl("[^\\x01-\\x7F]", readLines(src, warn = FALSE), perl = TRUE)))
})
