# v0.9.389: the FAO-Unesco Legend of 1974 registered as fao1974, keyed to its
# 26 major soil groups and 106 soil units (classify_fao(edition = "1974"),
# classify_fao1974()), with one example per group and per unit, a near miss
# per group and per unit gate with a printed number (make_fao_example(code,
# edition = "1974")), the edition pairs of plan section 11, the fall-through
# of Q5, the rule folder, the i18n keys and the app examples.

.f389 <- function(p, ...) classify_fao(p, edition = "1974", on_missing = "silent", ...)
.CODES389 <- c("O", "I", "V", "J", "Z", "G", "T", "Q", "R", "U", "E", "P", "F", "W", "S", "M",
               "C", "K", "H", "D", "X", "Y", "N", "A", "L", "B")
.NAMES389 <- c("Histosols", "Lithosols", "Vertisols", "Fluvisols", "Solonchaks", "Gleysols",
               "Andosols", "Arenosols", "Regosols", "Rankers", "Rendzinas", "Podzols",
               "Ferralsols", "Planosols", "Solonetz", "Greyzems", "Chernozems", "Kastanozems",
               "Phaeozems", "Podzoluvisols", "Xerosols", "Yermosols", "Nitosols", "Acrisols",
               "Luvisols", "Cambisols")
.src389 <- function(...) {
  f <- testthat::test_path("..", "..", ...)
  if (file.exists(f)) f else NA_character_
}
.ex389 <- function(code, nm = FALSE) make_fao_example(code, edition = "1974", near_miss = nm)
.last389 <- function(r) r$levels$code[nrow(r$levels)]
.edit389 <- function(p, rows = NULL, ..., site = list()) {
  h <- as.data.frame(p$horizons)
  if (!is.null(rows)) h <- h[rows, , drop = FALSE]
  v <- list(...)
  for (k in names(v)) h[[k]] <- if (length(v[[k]]) == 1L) rep(v[[k]], nrow(h)) else v[[k]]
  PedonRecord$new(site = utils::modifyList(p$site, site),
                  horizons = ensure_horizon_schema(data.table::as.data.table(h)))
}

test_that("the registry entry of fao1974", {
  skip_on_cran()
  s <- soilkey_systems()
  r <- s[s$id == "fao1974", ]
  expect_equal(nrow(r), 1L)
  expect_identical(r$classifier, "classify_fao1974")
  sys <- soilKey:::.system_get("fao1974")
  expect_equal(sys$order, 17)
  expect_identical(sys$status, "experimental")
  expect_false(sys$default)
  expect_false(sys$legacy)
  expect_identical(sys$namespace, "fao74")
  expect_identical(sys$levels, c("major_groups", "units"))
  expect_identical(sys$top_level, "major_groups")
  for (a in c("fao74", "fao_unesco_1974", "fao1974"))
    expect_identical(soilKey:::.system_resolve(a), "fao1974")
  # "fao" stays the Revised Legend (Q2)
  expect_identical(soilKey:::.system_resolve("fao"), "fao1988")
  expect_false("fao1974" %in% soilKey:::.system_ids(default_only = TRUE))
  p <- .ex389("L")
  ca <- classify_all(p, systems = c("wrb", "fao74"), on_missing = "silent")
  expect_identical(ca$fao1974$rsg_or_order, "Luvisols")
  expect_false("fao1974" %in% names(classify_all(p, on_missing = "silent")))
  # the thin wrapper and the user function agree
  expect_identical(classify_fao1974(p, on_missing = "silent")$name, .f389(p)$name)
  expect_identical(.f389(p)$system_id, "fao1974")
  expect_identical(soilKey:::make_fao1974_example("Lc")$site$id, "fao1974-Lc")
})

test_that("the key: 26 groups in the printed key order, every predicate defined", {
  skip_on_cran()
  k <- load_rules("fao1974")
  expect_identical(vapply(k$major_groups, `[[`, "", "code"), .CODES389)
  expect_identical(vapply(k$major_groups, `[[`, "", "name"), .NAMES389)
  expect_identical(soilKey:::.F74_GROUPS$code, .CODES389)
  expect_identical(soilKey:::.F74_GROUPS$name, .NAMES389)
  expect_false(any(vapply(k$major_groups, function(e) isTRUE(e$tests$default), NA)))
  expect_true(all(vapply(k$major_groups, function(e) nzchar(e$name_pt %||% ""), NA)))
  res <- soilKey:::.validate_rules("fao1974")
  expect_equal(res$missing, character(0))
  expect_identical(res$unsuffixed, character(0))
  expect_identical(res$codes, character(0))
  # the units: 103 in 23 lists, the Lithosols, Rankers and Rendzinas single
  expect_setequal(names(k$units), setdiff(.CODES389, c("I", "U", "E")))
  expect_equal(sum(lengths(k$units)), 103L)
  u <- soilKey:::.f74_unit_codes()
  expect_length(u, 106L)
  expect_false(anyDuplicated(u) > 0L)
  for (g in names(k$units)) {
    lst <- k$units[[g]]
    expect_true(isTRUE(lst[[length(lst)]]$tests$default), info = g)
    expect_false(any(vapply(lst[-length(lst)], function(e) isTRUE(e$tests$default), NA)), info = g)
    expect_true(all(startsWith(vapply(lst, `[[`, "", "code"), g)), info = g)
    expect_true(all(vapply(lst, function(e) nzchar(e$name_pt %||% ""), NA)), info = g)
  }
  csv <- .src389("data-raw", "fao1974_units.csv")
  skip_if_not(!is.na(csv))
  t <- utils::read.csv(csv, stringsAsFactors = FALSE)
  g <- t[t$level == "group", ]
  g <- g[order(as.numeric(g$key_position)), ]
  expect_identical(g$code, .CODES389)
  ut <- t[t$level == "unit", ]
  expect_identical(ut$code, u)
  nm <- unlist(lapply(.CODES389, function(c0) {
    if (c0 %in% c("I", "U", "E")) return(.NAMES389[.CODES389 == c0])
    vapply(k$units[[c0]], `[[`, "", "name")
  }))
  expect_identical(ut$name, nm)
})

test_that("every group example keys to its own group, not provisionally", {
  skip_on_cran()
  for (g in .CODES389) {
    r <- .f389(.ex389(g))
    expect_identical(r$levels$code[1L], g, info = g)
    expect_identical(r$rsg_or_order, .NAMES389[.CODES389 == g], info = g)
    expect_length(r$provisional_because, 0L)
    expect_identical(.ex389(g)$site$id, paste0("fao1974-", g))
  }
})

test_that("every group near miss keys elsewhere", {
  skip_on_cran()
  for (g in .CODES389) {
    r <- .f389(.ex389(g, TRUE))
    expect_false(identical(r$levels$code[1L], g), info = g)
  }
  # the decisive numbers
  expect_identical(.f389(.ex389("I", TRUE))$levels$code[1L], "R")    # rock at 11 cm
  expect_identical(.f389(.ex389("C", TRUE))$levels$code[1L], "H")    # calcic at 91 cm, medium texture
  expect_identical(.f389(.ex389("K", TRUE))$levels$code[1L], "H")
  expect_identical(.f389(.ex389("X", TRUE))$levels$code[1L], "Y")    # very weak ochric A
  expect_identical(.f389(.ex389("Y", TRUE))$levels$code[1L], "X")    # weak ochric A
  expect_identical(.f389(.ex389("A", TRUE))$levels$code[1L], "L")    # BS 55 throughout the B
  expect_identical(.f389(.ex389("L", TRUE))$levels$code[1L], "A")    # BS 45 in the lower B
  expect_identical(.f389(.ex389("U", TRUE))$levels$code[1L], "B")    # umbric A 26 cm: Humic Cambisols
  expect_identical(.f389(.ex389("U", TRUE))$name, "Humic Cambisols")
})

test_that("every unit example keys to its own unit, not provisionally", {
  skip_on_cran()
  for (u in soilKey:::.f74_unit_codes()) {
    r <- .f389(.ex389(u))
    expect_identical(.last389(r), u, info = u)
    expect_length(r$provisional_because, 0L)
  }
  # a single-unit group has one level, its name the group's
  r <- .f389(.ex389("I"))
  expect_identical(nrow(r$levels), 1L)
  expect_identical(r$name, "Lithosols")
})

test_that("every unit near miss crosses its number and keys to another unit", {
  skip_on_cran()
  nm <- soilKey:::.F74_UNIT_NEAR_MISS
  expect_length(nm, 52L)
  expect_true(all(nm %in% soilKey:::.f74_unit_codes()))
  for (u in nm) {
    r <- .f389(.ex389(u, TRUE))
    expect_false(identical(.last389(r), u), info = u)
  }
  expect_identical(.last389(.f389(.ex389("Ox", TRUE))), "Od")   # permafrost at 201 cm
  expect_identical(.last389(.f389(.ex389("Od", TRUE))), "Oe")   # pH 5.5
  expect_identical(.last389(.f389(.ex389("Fa", TRUE))), "Fr")   # 1.64 me per 100 g clay
  expect_identical(.last389(.f389(.ex389("Fh", TRUE))), "Fr")   # base saturation 50 % in the B to 100 cm
  expect_identical(.last389(.f389(.ex389("To", TRUE))), "Tv")   # sandy loam to 100 cm, not smeary
  expect_identical(.last389(.f389(.ex389("Lc", TRUE))), "Lo")   # 7.5YR chroma 4
  expect_identical(.last389(.f389(.ex389("Bf", TRUE))), "Bc")   # 24.1 me per 100 g clay
  expect_error(make_fao_example("Oe", edition = "1974", near_miss = TRUE), "no printed number")
  expect_error(make_fao_example("LV", edition = "1974"), "Legend of 1974")
})

test_that("the edition pairs of plan section 11 differ as the texts do", {
  skip_on_cran()
  E <- .edit389
  both <- function(p) c(classify_fao(p, on_missing = "silent")$name, .f389(p)$name)
  # a Lixisol (1988) that is an Orthic Luvisol (1974): no CEC split in 1974
  lx <- E(.ex389("L"), cec_cmol = c(8, 7, 16 * 0.32, 16 * 0.34, 16 * 0.2))
  expect_identical(both(lx), c("Haplic Lixisols", "Orthic Luvisols"))
  # a Calcisol (1988) that is a Calcic Cambisol, or a Calcic Xerosol in an aridic regime
  cl <- E(make_fao_example("CL"), ecec_cmol = c(14, 12, 10), site = list(soil_moisture_regime = "udic"))
  expect_identical(both(cl), c("Haplic Calcisols", "Calcic Cambisols"))
  expect_identical(both(E(cl, site = list(soil_moisture_regime = "aridic"))),
                   c("Haplic Calcisols", "Calcic Xerosols"))
  # gleyic properties at 70 cm: Gleyic Luvisols in 1988 (100 cm), Orthic in 1974 (50 cm)
  lg <- soilKey:::.f88x_gley(.ex389("L"), 4)
  expect_identical(both(lg), c("Gleyic Luvisols", "Orthic Luvisols"))
  # a Nitisol with a mollic A: the Phaeozems of 1988 ask a base saturation of
  # 50 % to 125 cm, those of 1974 only the mollic A
  nt <- E(make_fao_example("NT"), bs_pct = c(60, 55, 30, 30, 30), munsell_value_moist = c(2, 3, 4, 4, 4),
          munsell_chroma_moist = c(2, 2, 6, 6, 6), munsell_value_dry = c(4, 4, 5, 5, 5),
          oc_pct = c(3, 2, 0.6, 0.4, 0.2), ph_h2o = c(6, 6, 5.2, 5.2, 5.2),
          structure_grade = c("strong", "strong", "strong", "strong", "moderate"),
          ecec_cmol = c(12, 12, 12, 12, 11), site = list(soil_moisture_regime = "udic"))
  expect_identical(both(nt), c("Haplic Nitisols", "Luvic Phaeozems"))
})

test_that("no residual group after the Cambisols (Q5)", {
  skip_on_cran()
  # an ochric A over a calcic C, outside an aridic regime: no cambic B
  cl <- .edit389(.ex389("X"), designation = c("A", "Ck1", "Ck2"), site = list(soil_moisture_regime = "udic"))
  r <- .f389(cl)
  expect_identical(r$name, "[Not classified in the FAO-Unesco Legend (1974)]")
  expect_equal(nrow(r$levels), 0L)
  expect_true(any(grepl("none of the 26 gates", r$warnings, fixed = TRUE)))
  # the same with no moisture regime recorded: the Xerosols are the candidate
  r2 <- .f389(.edit389(cl, site = list(soil_moisture_regime = NA)))
  expect_match(r2$name, "^\\[Undecided in the FAO-Unesco Legend \\(1974\\): X Xerosols")
  expect_true("major_groups:X" %in% r2$provisional_because)
  # an albic E over C (the Podzol near miss)
  expect_identical(.f389(.ex389("P", TRUE))$name, "[Not classified in the FAO-Unesco Legend (1974)]")
})

test_that("the gates read their printed numbers", {
  skip_on_cran()
  E <- .edit389
  # Histosols: 45 cm of H, 35 cm fails
  expect_true(soilKey:::histosols_gate_fao74(.ex389("O"))$passed)
  expect_false(soilKey:::histosols_gate_fao74(.ex389("O", TRUE))$passed)
  # Lithosols: rock at 8 cm passes, at 11 cm fails
  expect_true(soilKey:::continuous_rock_fao74(.ex389("I"))$passed)
  expect_false(soilKey:::continuous_rock_fao74(.ex389("I", TRUE))$passed)
  # Vertisols mix the upper 20 cm (1988: 18 cm): 25 % clay over 0-18 cm and
  # 75 % over 18-25 cm mix to 30 % over 20 cm (passes) but 25 % over 18 cm
  v <- .ex389("V")
  vm <- .edit389(v, rows = c(1, 1, 2, 3, 4), designation = c("A1", "A2", "Bss1", "Bss2", "C"),
                 top_cm = c(0, 18, 25, 60, 110), bottom_cm = c(18, 25, 60, 110, 150),
                 clay_pct = c(25, 75, 50, 52, 48), sand_pct = c(45, 5, 22, 20, 22),
                 silt_pct = c(30, 20, 28, 28, 30))
  g74 <- soilKey:::vertisols_gate_fao74(vm)
  expect_equal(g74$evidence$clay_mixed_20cm, 30)
  expect_true(g74$passed)
  g88 <- soilKey:::vertisols_gate_fao88(vm)
  expect_equal(g88$evidence$clay_mixed_18cm, 25)
  expect_false(g88$passed)
  # Rankers: 20 cm passes, 26 cm fails
  expect_true(soilKey:::umbric_thickness_fao74(.ex389("U"), max = 25)$passed)
  expect_false(soilKey:::umbric_thickness_fao74(.ex389("U", TRUE), max = 25)$passed)
  expect_true(soilKey:::umbric_thickness_fao74(.ex389("U", TRUE), gt = 25)$passed)
  # Rendzinas: CaCO3 equivalent above 40 %
  expect_true(soilKey:::rendzinas_gate_fao74(.ex389("E"))$passed)
  expect_false(soilKey:::rendzinas_gate_fao74(.ex389("E", TRUE))$passed)
  # the 125 / 90 / 75 cm rule: a medium texture reads 90 cm
  r <- soilKey:::lime_by_texture_fao74(.ex389("C"))
  expect_true(r$passed)
  expect_equal(r$evidence$depth_cm, 90)
  expect_false(soilKey:::lime_by_texture_fao74(.ex389("C", TRUE))$passed)
  # Kastanozems: the complement of the Chernozems (a chroma above 2 in any layer)
  ch <- .ex389("C")
  mixed <- E(ch, top_cm = c(0, 10, 50, 120), bottom_cm = c(10, 50, 120, 150),
             munsell_chroma_moist = c(2, 3, 3, 3))
  expect_false(soilKey:::mollic_chroma_fao74(mixed, max = 2)$passed)
  expect_true(soilKey:::mollic_chroma_fao74(mixed, gt = 2)$passed)
  # Gypsic and Calcic Xerosols: the shallower horizon decides
  xy <- .ex389("Xy")
  expect_true(soilKey:::aridic_lime_fao74(xy, kind = "gypsic")$passed)
  expect_identical(.last389(.f389(.ex389("Xy", TRUE))), "Xh")
  # the uppermost B decides: a cambic B above an argillic B is no Luvisol
  l <- .ex389("L")
  expect_true(soilKey:::upper_b_fao74(l, kind = "argillic_b")$passed)
  cb <- .edit389(.ex389("Lo"), designation = c("A", "Bw", "Bt1", "Bt2", "BC"),
                 munsell_hue_moist = c("10YR", "7.5YR", "10YR", "10YR", "10YR"),
                 munsell_chroma_moist = c(3, 6, 4, 4, 4), clay_pct = c(15, 15, 32, 34, 20),
                 sand_pct = c(50, 50, 38, 38, 48), silt_pct = c(35, 35, 30, 28, 32),
                 clay_films_amount = c(NA, NA, "common", "common", "few"))
  expect_true(soilKey:::argillic_b_fao74(cb)$passed)
  expect_true(soilKey:::cambic_b_fao74(cb)$passed)
  expect_false(soilKey:::upper_b_fao74(cb, kind = "argillic_b")$passed)
  expect_true(soilKey:::upper_b_fao74(cb, kind = "cambic_b")$passed)
  rcb <- .f389(cb)
  expect_identical(rcb$rsg_or_order, "Cambisols")
  expect_false("L" %in% rcb$levels$code)
  # Rendzinas: the lime waiver of the mollic A (74 p. 24) waives the dry
  # value, its contrast with the C included: A 10YR 5/2 (dry 7) with 3 % OC
  # and 50 % CaCO3 over a Ck of 60 % (dry 7)
  e2 <- .edit389(.ex389("E"), munsell_value_moist = c(5, 6), munsell_value_dry = c(7, 7),
                 caco3_pct = c(50, 60))
  expect_true(soilKey:::mollic_a_fao74(e2)$passed)
  expect_identical(.f389(e2)$name, "Rendzinas")
  # without the lime the dry contrast is asked: no mollic A
  expect_false(isTRUE(soilKey:::mollic_a_fao74(.edit389(e2, caco3_pct = c(10, 60)))$passed))
  # Nitosols: a clay fall below 0.8 x the maximum
  expect_true(soilKey:::nitosols_gate_fao74(.ex389("N"))$passed)
  expect_false(soilKey:::nitosols_gate_fao74(.ex389("N", TRUE))$passed)
  # Arenosols: the coarse class of 74 p. 5 (clay below 18 %, sand above 65 %)
  expect_true(soilKey:::coarse_texture_fao74(.ex389("Q"))$passed)
  expect_false(soilKey:::coarse_texture_fao74(.ex389("Q", TRUE))$passed)
  # Planosols: the slowly permeable horizon under the albic E
  w <- soilKey:::planosols_gate_fao74(.ex389("W"))
  expect_true(w$passed)
  expect_false(isTRUE(soilKey:::planosols_gate_fao74(.ex389("W", TRUE))$passed))
})

test_that("the results carry the codes, pages and glosses", {
  skip_on_cran()
  r <- .f389(.ex389("Lc"))
  expect_identical(r$name, "Chromic Luvisols")
  expect_identical(r$rsg_or_order, "Luvisols")
  expect_identical(r$levels$code, c("L", "Lc"))
  expect_identical(r$levels$page, c("52", "52"))
  expect_true(all(nzchar(r$levels$name_pt)))
  expect_identical(r$system_id, "fao1974")
  # the gates name the 1974 key in their results
  tr <- r$trace$major_groups$L
  expect_false(is.null(tr))
})

test_that("the rule folder: thresholds and readings", {
  skip_on_cran()
  f <- system.file("rules", "fao1974", "thresholds.yaml", package = "soilKey")
  th <- yaml::read_yaml(f)
  expect_true(all(c("key", "unit_key", "phases", "texture_classes") %in% names(th)))
  expect_equal(th$unit_key$acric_cec_nh4cl_max_me_100g_clay$value, 1.5)
  dr <- .src389("data-raw", "fao_thresholds_1974.yaml")
  if (!is.na(dr)) expect_identical(yaml::read_yaml(dr), th)
  rd <- yaml::read_yaml(system.file("rules", "fao1974", "readings.yaml", package = "soilKey"))$readings
  expect_true(all(vapply(rd, function(e) "1974" %in% as.character(unlist(e$edition)), NA)))
  r389 <- Filter(function(e) identical(e$version, "0.9.389"), rd)
  expect_gte(length(r389), 15L)
  expect_true(all(vapply(r389, function(e) grepl("(Hugo, 2026-11-08)|for Hugo to confirm", e$reading), NA)))
  f2 <- .src389("data-raw", "fao_readings.yaml")
  skip_if_not(!is.na(f2))
  d389 <- Filter(function(e) identical(e$version, "0.9.389"), yaml::read_yaml(f2)$readings)
  expect_identical(vapply(d389, `[[`, "", "topic"), vapply(r389, `[[`, "", "topic"))
})

test_that("i18n, the app examples and the R source", {
  skip_on_cran()
  p <- system.file("i18n", "translations.yaml", package = "soilKey")
  y <- yaml::read_yaml(p)
  for (lang in names(y)) {
    expect_true(nzchar(y[[lang]][["classify.sys_fao1974"]] %||% ""), info = lang)
    expect_true(nzchar(y[[lang]][["classify.edition_fao1974"]] %||% ""), info = lang)
  }
  ui <- system.file("shiny", "classify_app_pro", "R", "utils_ui.R", package = "soilKey")
  skip_if_not(nzchar(ui) && file.exists(ui))
  env <- new.env()
  for (e in parse(ui)) if (is.call(e) && identical(e[[1]], as.name("<-")) &&
                           identical(e[[2]], as.name(".PRO_NATIONAL_EXAMPLES"))) eval(e, env)
  ex <- env$.PRO_NATIONAL_EXAMPLES$fao1974
  expect_identical(unname(ex), .CODES389)
  expect_identical(names(ex), .NAMES389)
  src <- .src389("R", "key-fao74.R")
  skip_if_not(!is.na(src))
  expect_false(any(grepl("[^\\x01-\\x7F]", readLines(src, warn = FALSE), perl = TRUE)))
})
