# v0.9.373: the Chinese Soil Taxonomy (2001, English edition) registered as
# cst2001, keyed to its 14 orders and 39 suborders (classify_cst2001()), with
# one example per suborder and a near miss per order
# (make_cst2001_example()), the correlation of Tables 4.2 and 4.3 as a note,
# and the i18n keys.

.c373 <- function(p, ...) classify_cst2001(p, on_missing = "silent", ...)
# v0.9.375: provisional above the subgroups (the subgroups of an example of
# this part may rest on data it does not carry)
.prov373 <- function(r) {
  any(!grepl("^subgroups:", r$provisional_because)) ||
    any(grepl("^Provisional: \\S+ .* rests on a reading", r$warnings) &
          !grepl("^Provisional: \\[?[A-N][0-9]+\\.[0-9]+\\.", r$warnings))
}
.CODES373 <- c("A1", "A2", "B1", "B2", "C1", "C2", "D1", "D2", "D3", "E1", "F1", "F2", "F3",
               "G1", "G2", "H1", "H2", "I1", "I2", "I3", "J1", "J2", "J3", "K1", "K2", "K3",
               "L1", "L2", "L3", "L4", "M1", "M2", "M3", "M4", "M5", "N1", "N2", "N3", "N4")

test_that("the registry entry of cst2001", {
  skip_on_cran()
  s <- soilkey_systems()
  r <- s[s$id == "cst2001", ]
  expect_equal(nrow(r), 1L)
  expect_identical(r$classifier, "classify_cst2001")
  sys <- soilKey:::.system_get("cst2001")
  expect_equal(sys$order, 15)
  expect_identical(sys$status, "experimental")
  expect_false(sys$default)
  expect_false(sys$legacy)
  expect_identical(sys$namespace, "cst")
  expect_identical(sys$levels, c("orders", "suborders", "groups", "subgroups"))
  for (a in c("cst", "china", "chinese", "cst2001"))
    expect_identical(soilKey:::.system_resolve(a), "cst2001")
  expect_false("cst2001" %in% soilKey:::.system_ids(default_only = TRUE))
  # identify_horizons reaches the CST panel
  fr <- identify_horizons(make_cst2001_example("K3"), "cst2001")
  expect_true(all(c("diagnostics", "characteristics") %in% names(fr)))
  expect_true("LAC-ferric horizon" %in% unlist(fr$diagnostics))
})

test_that("the key: 14 orders and 39 suborders in the book's order, every predicate defined", {
  skip_on_cran()
  k <- load_rules("cst2001")
  expect_identical(vapply(k$orders, `[[`, "", "code"), LETTERS[1:14])
  expect_identical(vapply(k$orders, `[[`, "", "name"),
                   c("Histosols", "Anthrosols", "Spodosols", "Andosols", "Ferralosols", "Vertosols",
                     "Aridosols", "Halosols", "Gleyosols", "Isohumosols", "Ferrosols", "Argosols",
                     "Cambosols", "Primosols"))
  expect_identical(names(k$suborders), LETTERS[1:14])
  sub <- unlist(lapply(k$suborders, function(l) vapply(l, `[[`, "", "code")), use.names = FALSE)
  expect_identical(setdiff(sub, "[E2]"), .CODES373)
  expect_equal(length(sub), 40L)
  # every list but E ends with its residual; E with soilKey's bracketed one
  for (o in names(k$suborders)) {
    l <- k$suborders[[o]]
    expect_true(isTRUE(l[[length(l)]]$tests$default), info = o)
  }
  expect_identical(k$suborders$E[[2]]$name, "[Other Ferralosols]")
  expect_true(all(vapply(unlist(k$suborders, recursive = FALSE), function(e) !is.null(e$name_pt), NA)))
  res <- soilKey:::.validate_rules("cst2001")
  expect_equal(res$missing, character(0))
  expect_identical(vapply(k$orders, function(e) isTRUE(e$tests$default), NA), c(rep(FALSE, 13), TRUE))
})

test_that("every example keys to its own suborder, not provisionally", {
  skip_on_cran()
  for (cd in .CODES373) {
    r <- .c373(make_cst2001_example(cd))
    expect_identical(r$levels$code[1:2], c(substr(cd, 1, 1), cd), info = cd)
    expect_false(.prov373(r), info = cd)
    expect_identical(r$system_id, "cst2001")
    expect_identical(r$rsg_or_order, r$levels$name[1])
  }
})

test_that("the fourteen near misses land where the one change sends them", {
  skip_on_cran()
  want <- c(A1 = "A2", B2 = "N4", C1 = "C2", D1 = "D3", E1 = "K3", F1 = "M2", G1 = "M5", H1 = "L2",
            I1 = "I3", J2 = "M3", K1 = "L2", L1 = "L4", M3 = "M5", N2 = "N4")
  expect_equal(length(want), 14L)
  expect_identical(sort(unique(substr(names(want), 1, 1))), LETTERS[1:14])
  for (cd in names(want)) {
    r <- .c373(make_cst2001_example(cd, near_miss = TRUE))
    expect_identical(r$levels$code[2], want[[cd]], info = cd)
    # v0.9.374: decided down to the suborder (a near miss is a device of the
    # suborders; its group may rest on data it does not carry)
    expect_false(any(grepl("^(orders|suborders):", r$provisional_because)), info = cd)
    expect_false(any(grepl("^Provisional: [A-N][0-9]? [A-Z]", r$warnings)), info = cd)
  }
  expect_error(make_cst2001_example("A2", near_miss = TRUE), "No near miss")
})

test_that("the residual named for the udic regime carries a note, the class unchanged", {
  skip_on_cran()
  r <- .c373(make_cst2001_example("M3", near_miss = TRUE))
  expect_identical(r$levels$name[2], "Udic Cambosols")
  expect_true(any(grepl("residual of its key, named for the udic regime; the moisture regime read is aridic", r$warnings)))
  # a udic soil has no note
  r <- .c373(make_cst2001_example("M5"))
  expect_false(any(grepl("residual of its key", r$warnings)))
})

test_that("an undecided gate makes the name provisional; a gate on no data leaves the level", {
  skip_on_cran()
  # the moisture regime not recorded: the Ustic and Perudic Cambosols
  # undecided, the residual provisional
  p <- make_cst2001_example("M3")
  p$site$soil_moisture_regime <- NULL
  r <- .c373(p)
  expect_identical(r$levels$name[2], "Udic Cambosols")
  expect_true(all(c("suborders:M3", "suborders:M4") %in% r$provisional_because))
  # salts not measured: the Halosols undecided before the Argosols
  p <- make_cst2001_example("L2")
  p$horizons$soluble_salts_pct <- NA_real_
  r <- .c373(p)
  expect_identical(r$levels$code[1], "L")
  expect_true("orders:H" %in% r$provisional_because)
  # an acid Ferrosol: salts not measured under pH 5.5 are too few (Hugo,
  # 2026-11-06), the name stays decided
  p <- make_cst2001_example("K3")
  p$horizons$soluble_salts_pct <- NA_real_
  r <- .c373(p)
  expect_identical(r$levels$code[1:2], c("K", "K3"))
  expect_false("orders:H" %in% r$provisional_because)
  # no moisture regime recorded on a poorly drained Gleyosol: the stagnic
  # regime undecided
  p <- make_cst2001_example("I3")
  p$site$soil_moisture_regime <- NULL
  r <- .c373(p)
  expect_identical(r$levels$name[2], "Orthic Gleyosols")
  expect_true("suborders:I2" %in% r$provisional_because)
  # the Ferralosols under an ustic regime: soilKey's residual [E2], the name
  # stops there
  p <- make_cst2001_example("E1")
  p$site$soil_moisture_regime <- "ustic"
  r <- .c373(p)
  expect_identical(r$levels$code[1:2], c("E", "[E2]"))
  expect_identical(r$levels$name[2], "[Other Ferralosols]")
  # gley colours without a test: Gleyosols, with the warning of a provisional reading
  p <- make_cst2001_example("I3")
  p$horizons$childs_test <- NA_character_
  r <- .c373(p)
  expect_identical(r$levels$code[1], "I")
  expect_true(any(grepl("^Provisional: I Gleyosols rests on a reading", r$warnings)))
})

test_that("the gates of the key read their windows", {
  skip_on_cran()
  # andic share: the Andosols at 60 % or more, the Histosols and Spodosols under it
  p <- make_cst2001_example("D1")
  expect_true(soilKey:::andic_share_cst(p, min = 0.6, datum = "andic")$passed)
  expect_false(soilKey:::andic_share_cst(p, below = 0.6, datum = "surface")$passed)
  p$horizons$al_ox_pct[2] <- 0.5; p$horizons$fe_ox_pct[2] <- 0.2
  p$horizons$phosphate_retention_pct[2] <- 40
  expect_false(soilKey:::andic_share_cst(p, min = 0.6, datum = "andic")$passed)
  # Histosols: 40 cm of sapric peat saturated, 35 cm not enough
  p <- make_cst2001_example("A2")
  r <- soilKey:::histosol_materials_cst(p)
  expect_true(r$passed); expect_true(isTRUE(r$evidence$items[["d"]]))
  h <- p$horizons; h <- h[c(1, 3), ]; h$bottom_cm[1] <- 35; h$top_cm[2] <- 35
  p2 <- PedonRecord$new(site = p$site, horizons = h)
  expect_false(soilKey:::histosol_materials_cst(p2)$passed)
  # one horizon against a total thickness of redoxic features
  p <- make_cst2001_example("F1")
  expect_true(soilKey:::layer_run_cst(p, "redoxic_features", min_cm = 10, to_cm = 50, total = TRUE)$passed)
  expect_false(soilKey:::layer_run_cst(p, "redoxic_features", min_cm = 10, to_cm = 50, datum = "surface",
                                      from_cm = 45)$passed)
  # the Vitric texture: loamy sand passes, silt loam fails
  expect_true(soilKey:::vitric_texture_cst(make_cst2001_example("D2"))$passed)
  expect_false(soilKey:::vitric_texture_cst(make_cst2001_example("D3"))$passed)
  # crack days: recorded days decide; irrigated land passes
  p <- make_cst2001_example("F2")
  expect_true(soilKey:::crack_days_cst(p)$passed)
  p$site$cracks_open_days <- 60
  expect_false(soilKey:::crack_days_cst(p)$passed)
  p$site$irrigated <- TRUE
  expect_true(soilKey:::crack_days_cst(p)$passed)
  p$site$irrigated <- FALSE; p$site$cracks_open_days <- NULL
  r <- soilKey:::crack_days_cst(p)
  expect_true(r$passed); expect_true(r$evidence$provisional)
  # the humic spodic subhorizons
  expect_true(soilKey:::humic_spodic_cst(make_cst2001_example("C1"))$passed)
  expect_false(soilKey:::humic_spodic_cst(make_cst2001_example("C2"))$passed)
  # the base saturation window of the Isohumosols: to 180 cm; below a
  # calcareous base read as saturated, otherwise cut at the description and
  # provisional (Hugo, 2026-11-06)
  p <- make_cst2001_example("J2")
  expect_true(soilKey:::isohumosol_base_saturation_cst(p)$passed)
  h <- p$horizons[1:3, ]
  r <- soilKey:::isohumosol_base_saturation_cst(PedonRecord$new(site = p$site, horizons = h))
  expect_true(r$passed); expect_false(r$evidence$provisional)
  h$caco3_pct[3] <- 0; h$secondary_carbonates_pct[3] <- 0
  r <- soilKey:::isohumosol_base_saturation_cst(PedonRecord$new(site = p$site, horizons = h))
  expect_true(r$passed); expect_true(r$evidence$provisional)
  # no saturation regime under a well-drained record; undecided without it
  p <- make_cst2001_example("M3")
  expect_false(soilKey:::moisture_regime_cst(p, "aquic")$passed)
  p$site$drainage_class <- NULL
  expect_true(is.na(soilKey:::moisture_regime_cst(p, "aquic")$passed))
})

test_that("the result: Portuguese glosses, the book's marks, the correlation, the panel", {
  skip_on_cran()
  r <- .c373(make_cst2001_example("K3"))
  expect_identical(r$levels$name_pt[1:2], c("Ferrossolos (Ferrosols)", "Ferrossolos \u00fadicos"))
  expect_identical(r$levels$book_provisional[1:2], c(FALSE, FALSE))
  cr <- r$qualifiers$correlation_cst
  expect_identical(cr$order, "K")
  expect_identical(cr$us_st_1999$class[cr$us_st_1999$degree == "mostly"], "Ultisols")
  expect_true("Nitisols" %in% cr$wrb_1998$class)
  ln <- soilKey:::.cst_correlation_lines(r)
  expect_length(ln, 2L)
  expect_match(ln[1], "Table 4.2: mostly Ultisols; partly Alfisols, Inceptisols")
  expect_true(any(grepl("^Correlation \\(a note, never a class\\)", r$warnings)))
  # the correlation is no qualifier in the printed result
  out <- c(utils::capture.output(print(r)), utils::capture.output(print(r), type = "message"))
  expect_true(any(grepl("Levels:", out)))
  expect_false(any(grepl("Qualifiers:.*Ultisols", out)))
  # every order has its two tables
  cc <- soilKey:::.cst_correlation()$correlation
  expect_identical(vapply(cc, function(z) as.character(z$order), ""), LETTERS[1:14])
  # the panel in the trace, one entry per layer
  expect_equal(length(r$trace$horizons), nrow(make_cst2001_example("K3")$horizons))
  # a book mark, as a later part will write it
  k <- load_rules("cst2001")
  k$suborders$K[[3]]$book_provisional <- TRUE
  r <- classify_cst2001(make_cst2001_example("K3"), rules = k, on_missing = "silent")
  expect_identical(r$levels$book_provisional[1:2], c(FALSE, TRUE))
  expect_true(any(grepl("K3 Udic Ferrosols is marked provisional in CST 2001", r$warnings)))
})

test_that("the i18n keys and the app examples of China", {
  skip_on_cran()
  tr <- yaml::read_yaml(system.file("i18n", "translations.yaml", package = "soilKey"))
  for (l in c("en", "pt")) {
    for (k in c("classify.sys_cst2001", "classify.edition_cst2001", "classify.level_orders",
                "classify.level_suborders", "classify.cst_correlation", "classify.cst_book_provisional"))
      expect_true(nzchar(tr[[l]][[k]] %||% ""), info = paste(l, k))
  }
  rt <- yaml::read_yaml(system.file("i18n", "report_translations.yaml", package = "soilKey"))
  for (l in c("en", "pt")) {
    for (k in c("report.cst_correlation", "report.cst_correlation_desc", "report.level_orders",
                "report.level_suborders"))
      expect_true(nzchar(rt[[l]][[k]] %||% ""), info = paste(l, k))
  }
  app <- system.file("shiny", "classify_app_pro", "R", "utils_ui.R", package = "soilKey")
  env <- new.env()
  exprs <- parse(app)
  for (e in exprs) if (is.call(e) && identical(e[[1]], as.name("<-")) &&
                       identical(e[[2]], as.name(".PRO_NATIONAL_EXAMPLES"))) eval(e, env)
  ex <- env$.PRO_NATIONAL_EXAMPLES$cst2001
  expect_identical(unname(ex), .CODES373)
  for (i in c(1, 20, 39)) {
    r <- .c373(make_cst2001_example(ex[[i]]))
    expect_identical(r$levels$name[2], names(ex)[i])
  }
})

test_that("the report carries the correlation of a Chinese order", {
  skip_on_cran()
  p <- make_cst2001_example("J2")
  f <- tempfile(fileext = ".html")
  on.exit(unlink(f))
  report(p, file = f, pedon = p, systems = c("wrb2022", "cst2001"))
  html <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_match(html, "Ustic Isohumosols", fixed = TRUE)
  expect_match(html, "Tables 4.2 and 4.3", fixed = TRUE)
  expect_match(html, "wholly Mollisols", fixed = TRUE)
})


# --- the review (Hugo, 2026-11-06) ---------------------------------------------------------------

.P373 <- function(h, site = list()) {
  n <- max(lengths(h)); h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  PedonRecord$new(site = utils::modifyList(list(country = "CN"), site),
                  horizons = ensure_horizon_schema(data.table::as.data.table(as.data.frame(h, stringsAsFactors = FALSE))))
}
.ns373 <- function(f) get(f, envir = asNamespace("soilKey"))

test_that("D1: red acid B horizons are spodic only with a sign or under an E", {
  skip_on_cran()
  red_bt <- list(designation = c("A", "Bt1", "Bt2"), top_cm = c(0, 15, 50), bottom_cm = c(15, 50, 150),
    clay_pct = c(20, 34, 36), silt_pct = 40, sand_pct = c(40, 26, 24), oc_pct = c(1.8, 0.7, 0.4), ph_h2o = 5.2,
    munsell_hue_moist = c("5YR", "2.5YR", "2.5YR"), munsell_value_moist = 4, munsell_chroma_moist = 6,
    clay_films_strength = c(NA, "distinct", "distinct"), clay_films_amount = c(NA, "common", "common"),
    al_ox_pct = 0.1, fe_ox_pct = 0.3, volcanic_glass_pct = 0, cec_cmol = 12, bs_pct = 40)
  site <- list(soil_moisture_regime = "udic", soil_temperature_regime = "thermic", drainage_class = "well drained")
  p <- .P373(red_bt, site)
  expect_false(.ns373("spodic_horizon_cst")(p)$passed)
  expect_true(.ns373("spodic_horizon_cst")(p, grade = "evidence")$passed)
  r <- .c373(p)
  expect_identical(r$levels$code[1:2], c("L", "L4"))
  # no oxalate data: undecided, not spodic
  h <- red_bt; h$al_ox_pct <- NULL; h$fe_ox_pct <- NULL
  expect_true(is.na(.ns373("spodic_horizon_cst")(.P373(h, site))$passed))
  # the Hainan ferralic Bo and the Jiangxi Ferrosol Bt
  bo <- list(designation = c("A", "Bo1", "Bo2", "Bo3"), top_cm = c(0, 15, 50, 110), bottom_cm = c(15, 50, 110, 200),
    clay_pct = c(42, 52, 55, 55), sand_pct = c(28, 20, 18, 18), silt_pct = c(30, 28, 27, 27), oc_pct = c(2.2, 0.8, 0.4, 0.25),
    cec_cmol = c(9, 6.5, 6, 6), ca_cmol = 0.6, mg_cmol = 0.3, k_cmol = 0.1, na_cmol = 0.05, al_kcl_cmol = 1.0,
    bs_pct = c(20, 15, 15, 15), ph_h2o = c(4.7, 4.9, 5.0, 5.1), fe_dcb_pct = c(5, 7, 7, 7), fe_total_pct = c(8, 10, 10, 10),
    weatherable_minerals_pct = c(NA, 2, 2, 2), k_total_pct = c(NA, 0.2, 0.2, 0.2), al_ox_pct = 0.1, fe_ox_pct = 0.3,
    volcanic_glass_pct = 0, soluble_salts_pct = 0.02,
    structure_type = c("granular", "subangular blocky", "subangular blocky", "subangular blocky"),
    munsell_hue_moist = c("5YR", "2.5YR", "2.5YR", "2.5YR"), munsell_value_moist = c(3, 3, 4, 4),
    munsell_chroma_moist = c(4, 6, 6, 6))
  r <- .c373(.P373(bo, list(soil_moisture_regime = "udic", soil_temperature_regime = "hyperthermic")))
  expect_identical(r$levels$code[1:2], c("E", "E1"))
  expect_false(isTRUE(.ns373("spodic_horizon_cst")(.P373(bo))$passed))
  bt <- list(designation = c("A", "Bt1", "Bt2", "BC"), top_cm = c(0, 18, 50, 110), bottom_cm = c(18, 50, 110, 160),
    clay_pct = c(28, 40, 44, 38), sand_pct = c(35, 28, 26, 30), silt_pct = c(37, 32, 30, 32), oc_pct = c(1.8, 0.7, 0.4, 0.2),
    cec_cmol = c(9, 8.5, 9, 8.5), ca_cmol = 1.0, mg_cmol = 0.5, k_cmol = 0.15, na_cmol = 0.05, al_kcl_cmol = 2.0,
    bs_pct = 25, ph_h2o = c(4.8, 4.9, 5.0, 5.1), fe_dcb_pct = c(2.0, 3.0, 3.2, 3.0), fe_total_pct = c(4, 5, 5, 5),
    clay_films_strength = c(NA, "distinct", "distinct", NA), clay_films_amount = c(NA, "common", "common", NA),
    al_ox_pct = 0.1, fe_ox_pct = 0.3, volcanic_glass_pct = 0,
    structure_type = c("granular", "subangular blocky", "subangular blocky", "massive"),
    munsell_hue_moist = c("5YR", "2.5YR", "2.5YR", "5YR"), munsell_value_moist = c(4, 4, 4, 5),
    munsell_chroma_moist = c(4, 6, 6, 6))
  r <- .c373(.P373(bt, list(soil_moisture_regime = "udic", soil_temperature_regime = "thermic")))
  expect_identical(r$levels$code[1:2], c("K", "K3"))
  expect_false(any(c("orders:C") %in% r$provisional_because))
  # the colour alone still makes spodic materials directly below an E
  e <- list(designation = c("A", "E", "Bs", "C"), top_cm = c(0, 10, 20, 50), bottom_cm = c(10, 20, 50, 90),
    oc_pct = c(3, 0.4, 1.0, 0.2), ph_h2o = 4.6, clay_pct = 6, sand_pct = 84, silt_pct = 10,
    munsell_hue_moist = c("10YR", "10YR", "5YR", "10YR"), munsell_value_moist = c(3, 6, 4, 6),
    munsell_chroma_moist = c(2, 2, 6, 4), munsell_value_dry = c(5, 7, 5, 7))
  expect_true(.ns373("spodic_horizon_cst")(.P373(e))$passed)
})

test_that("D2: the Isohumosols described above 180 cm", {
  skip_on_cran()
  chz <- list(designation = c("Ah1", "Ah2", "AB", "Bk", "Ck"), top_cm = c(0, 20, 45, 70, 100), bottom_cm = c(20, 45, 70, 100, 150),
    clay_pct = c(24, 25, 25, 23, 20), silt_pct = c(50, 50, 50, 52, 55), sand_pct = c(26, 25, 25, 25, 25),
    oc_pct = c(2.4, 1.6, 0.9, 0.4, 0.2), caco3_pct = c(0, 0, 1, 14, 9), secondary_carbonates_pct = c(0, 0, 0, 8, 2),
    ph_h2o = c(7.3, 7.6, 7.9, 8.3, 8.4), bs_pct = 100, cec_cmol = c(28, 26, 22, 16, 14),
    bulk_density_g_cm3 = c(1.15, 1.25, 1.3, 1.35, 1.4), structure_type = c("granular", "granular", "blocky", "blocky", "massive"),
    munsell_hue_moist = "10YR", munsell_value_moist = c(2, 3, 3, 5, 6), munsell_chroma_moist = c(2, 2, 2, 3, 3),
    munsell_value_dry = c(4, 4, 5, 7, 7))
  r <- .c373(.P373(chz, list(soil_moisture_regime = "ustic", soil_temperature_regime = "frigid")))
  expect_identical(r$levels$code[1:2], c("J", "J2"))
  expect_false(any(grepl("^Provisional: J", r$warnings)))
  blk <- list(designation = c("Ah1", "Ah2", "AB", "Bw", "C"), top_cm = c(0, 25, 50, 75, 120), bottom_cm = c(25, 50, 75, 120, 170),
    clay_pct = c(28, 29, 30, 31, 28), silt_pct = c(50, 50, 48, 48, 50), sand_pct = c(22, 21, 22, 21, 22),
    oc_pct = c(3.0, 2.0, 1.0, 0.5, 0.3), ph_h2o = c(6.3, 6.4, 6.6, 6.8, 7.0), bs_pct = c(85, 85, 88, 90, 92),
    cec_cmol = c(32, 30, 27, 25, 22), bulk_density_g_cm3 = c(1.1, 1.2, 1.3, 1.35, 1.4),
    structure_type = c("granular", "granular", "blocky", "blocky", "massive"),
    munsell_hue_moist = "10YR", munsell_value_moist = c(2, 2, 3, 4, 5), munsell_chroma_moist = c(1, 2, 2, 3, 3),
    munsell_value_dry = c(3, 3, 4, 6, 6))
  r <- .c373(.P373(blk, list(soil_moisture_regime = "udic", soil_temperature_regime = "frigid")))
  expect_identical(r$levels$code[1:2], c("J", "J3"))
  expect_true(any(grepl("^Provisional: J Isohumosols", r$warnings)))
})

test_that("D3: rock and organic layers carry no gleyic or redoxic run", {
  skip_on_cran()
  pur <- list(designation = c("Ap", "C", "R"), top_cm = c(0, 18, 40), bottom_cm = c(18, 40, 60),
    clay_pct = c(22, 20, NA), silt_pct = c(40, 42, NA), sand_pct = c(38, 38, NA), oc_pct = c(0.8, 0.3, NA), ph_h2o = 7.9,
    caco3_pct = c(4, 6, NA), structure_type = c("granular", "massive", NA),
    munsell_hue_moist = c("10RP", "10RP", NA), munsell_value_moist = c(4, 4, NA), munsell_chroma_moist = c(3, 3, NA))
  p <- .P373(pur, list(soil_moisture_regime = "udic", soil_temperature_regime = "thermic", parent_material = "purplish shale"))
  expect_false(.ns373("layer_run_cst")(p, "gleyic_features", 10, to_cm = 50)$passed)
  expect_false("orders:I" %in% .c373(p)$provisional_because)
})

test_that("D4: the permafrost of M 4, the contacts of F 2 and the anthroturbic layer from the mineral surface", {
  skip_on_cran()
  pf <- list(designation = c("Oe", "A", "Bw", "Cf"), top_cm = c(0, 15, 25, 55), bottom_cm = c(15, 25, 55, 90),
    oc_pct = c(25, 3, 0.8, 0.4), clay_pct = c(NA, 12, 12, 12), silt_pct = c(NA, 40, 40, 40), sand_pct = c(NA, 48, 48, 48),
    ph_h2o = 6.5, permafrost_temp_C = c(NA, NA, NA, -1), structure_type = c(NA, "granular", "massive", "massive"),
    munsell_hue_moist = "10YR", munsell_value_moist = c(2, 3, 4, 5), munsell_chroma_moist = c(1, 2, 3, 3))
  p <- .P373(pf, list(soil_temperature_regime = "cryic", soil_moisture_regime = "stagnic"))
  expect_false(.ns373("permafrost_layer_cst")(p, top_max = 50)$passed)
  expect_true(.ns373("permafrost_layer_cst")(p, top_max = 50, datum = "mineral")$passed)
  vt <- list(designation = c("Oa", "A", "Bss", "R"), top_cm = c(0, 10, 25, 55), bottom_cm = c(10, 25, 55, 70))
  pv <- .P373(vt)
  expect_false(.ns373("lithic_contact_cst")(pv, top_max = 50)$passed)
  expect_true(.ns373("lithic_contact_cst")(pv, top_max = 50, datum = "mineral")$passed)
  at <- list(designation = c("Oe", "Ap", "Au", "C"), top_cm = c(0, 15, 35, 70), bottom_cm = c(15, 35, 70, 120),
             foreign_fragments_pct = c(NA, NA, 5, NA))
  pa <- .P373(at)
  expect_true(.ns373("anthroturbic_layer_cst")(pa, top_max = 20, datum = "mineral")$passed)
  expect_false(.ns373("anthroturbic_layer_cst")(pa, top_max = 20)$passed)
  k <- load_rules("cst2001")
  expect_identical(k$orders[[13]]$tests$any_of[[9]]$all_of[[1]]$permafrost_layer_cst$datum, "mineral")
  expect_identical(k$suborders$N[[1]]$tests$any_of[[1]]$anthroturbic_layer_cst$datum, "mineral")
})

test_that("D5: a plain A whose core items fail is no anthrostagnic epipedon", {
  skip_on_cran()
  h <- list(designation = c("A", "B", "C"), top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 120), clay_pct = 20, silt_pct = 50,
    sand_pct = 30, oc_pct = c(1.2, 0.4, 0.2), ph_h2o = 6.5, munsell_hue_moist = "10YR", munsell_value_moist = c(4, 5, 5),
    munsell_chroma_moist = c(3, 4, 4))
  p <- .P373(h, list(soil_moisture_regime = "udic", soil_temperature_regime = "thermic", drainage_class = "well drained"))
  expect_false(.ns373("anthrostagnic_epipedon_cst")(p)$passed)
  expect_false("orders:B" %in% .c373(p)$provisional_because)
})

test_that("R1-R4: the decisions on the doubtful readings", {
  skip_on_cran()
  # R1: the n value not measured is under 0.7 in a layer not saturated
  p <- make_cst2001_example("M3")
  expect_true(.ns373("low_n_or_clay_cst")(p)$passed)
  p$site$drainage_class <- NULL
  expect_true(is.na(.ns373("low_n_or_clay_cst")(p)$passed))
  r <- yaml::read_yaml(system.file("rules", "cst2001", "readings.yaml", package = "soilKey"))$readings
  rd <- vapply(r, `[[`, "", "reading")
  expect_true(any(grepl("KST criterion has 8 % clay or more", rd, fixed = TRUE)))
  # the readings of the parts up to 0.9.373 are all decided (the groups of
  # 0.9.374 and the subgroups of 0.9.375 bring their own, for Hugo's word)
  tp <- vapply(r, `[[`, "", "topic")
  expect_false(any(grepl("for Hugo to confirm", rd[!grepl("^(Groups|Subgroups):", tp)])))
  # R2: the Changbai Andosol (silty clay loam) is not Vitric; loam is not coarse
  andi <- list(designation = c("Ah", "Bw1", "Bw2", "C"), top_cm = c(0, 25, 60, 100), bottom_cm = c(25, 60, 100, 140),
    oc_pct = c(7, 3, 1.5, 0.6), clay_pct = c(25, 28, 25, 20), silt_pct = c(50, 47, 50, 50), sand_pct = c(25, 25, 25, 30),
    al_ox_pct = c(2.4, 2.8, 2.5, 1.8), fe_ox_pct = c(1.2, 1.4, 1.2, 1.0), bulk_density_g_cm3 = c(0.65, 0.7, 0.75, 0.85),
    phosphate_retention_pct = c(92, 95, 93, 88), ph_h2o = 5.6, bs_pct = 25, cec_cmol = c(35, 28, 25, 20),
    structure_type = c("granular", "blocky", "blocky", "massive"), munsell_hue_moist = "10YR",
    munsell_value_moist = c(2, 3, 4, 4), munsell_chroma_moist = c(2, 3, 4, 4))
  r <- .c373(.P373(andi, list(soil_moisture_regime = "udic", soil_temperature_regime = "frigid")))
  expect_identical(r$levels$code[1:2], c("D", "D3"))
  expect_identical(unlist(soilKey:::.cst_thr("key", "vitric_andosols", "coarser_than_silt_loam")), c("S", "LS", "SL"))
  # R3: Histosol item b, literal: 40 cm of organic materials within 50 cm
  hb <- list(designation = c("Oa", "C"), top_cm = c(0, 45), bottom_cm = c(45, 90), oc_pct = c(30, 0.5),
             clay_pct = c(NA, 20), water_saturation_days = c(60, 60), fiber_content_rubbed_pct = c(10, NA))
  r <- .ns373("histosol_materials_cst")(.P373(hb, list(soil_moisture_regime = "udic")))
  expect_true(isTRUE(r$evidence$items[["b"]]))
  # R4: a non-volcanic parent material rules out andic; acid soils have no
  # salic horizon without salts; an aridic soil without a crust record
  h <- list(designation = c("A", "Bw", "C"), top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 120), oc_pct = c(1.5, 0.5, 0.2),
            clay_pct = 20, silt_pct = 60, sand_pct = 20)
  expect_true(is.na(.ns373("andic_property_cst")(.P373(h))$passed))
  r <- .ns373("andic_property_cst")(.P373(h, list(parent_material = "loess")))
  expect_false(r$passed)
  h2 <- c(h, list(ph_h2o = 4.6))
  expect_false(.ns373("salic_horizon_cst")(.P373(h2), top_max = 30, datum = "mineral")$passed)
  expect_false(.ns373("salic_horizon_cst")(.P373(h, list(soil_moisture_regime = "perudic")), top_max = 30, datum = "mineral")$passed)
  expect_true(is.na(.ns373("salic_horizon_cst")(.P373(h), top_max = 30, datum = "mineral")$passed))
  gyp <- list(designation = c("Av", "Bw", "By1", "By2", "C"), top_cm = c(0, 6, 20, 45, 80), bottom_cm = c(6, 20, 45, 80, 120),
    clay_pct = c(8, 12, 10, 9, 8), silt_pct = c(30, 34, 30, 28, 25), sand_pct = c(62, 54, 60, 63, 67),
    oc_pct = c(0.25, 0.2, 0.1, 0.08, 0.05), caco3_pct = c(8, 9, 7, 6, 6), caso4_pct = c(1, 2, 25, 18, 4),
    gypsum_visible_pct = c(0, 0, 10, 6, 0), soluble_salts_pct = c(0.3, 0.4, 0.8, 0.6, 0.4), ph_h2o = 8.3,
    desert_pavement_pct = c(50, NA, NA, NA, NA), structure_type = c("platy", "blocky", "massive", "massive", "single grain"),
    munsell_hue_moist = "7.5YR", munsell_value_moist = 6, munsell_chroma_moist = 3)
  p <- .P373(gyp, list(soil_moisture_regime = "aridic", soil_temperature_regime = "mesic"))
  expect_true(is.na(.ns373("aridic_epipedon_cst")(p)$passed))
  expect_true("orders:G" %in% .c373(p)$provisional_because)
  p$site$surface_crust_thickness_cm <- 1
  expect_identical(.c373(p)$levels$code[1:2], c("G", "G2"))
})

test_that("a class on a provisional reading gives the alternative the key would reach (Hugo, 2026-11-06)", {
  skip_on_cran()
  aq <- list(designation = c("Ap", "Bw", "Br", "Cr"), top_cm = c(0, 20, 40, 80), bottom_cm = c(20, 40, 80, 130),
    clay_pct = c(20, 22, 22, 18), silt_pct = c(55, 55, 55, 58), sand_pct = c(25, 23, 23, 24), oc_pct = c(1.3, 0.5, 0.3, 0.2),
    ph_h2o = 7.8, bs_pct = 100, cec_cmol = 14, caco3_pct = c(3, 3, 3, 3),
    mottle_colors = c(NA, "7.5YR 5/6", "7.5YR 5/8", "7.5YR 5/8"), structure_type = c("granular", "blocky", "blocky", "massive"),
    munsell_hue_moist = c("10YR", "10YR", "2.5Y", "2.5Y"), munsell_value_moist = c(4, 5, 5, 5),
    munsell_chroma_moist = c(3, 3, 3, 2), munsell_value_dry = c(6, 7, 7, 7))
  site <- list(soil_moisture_regime = "aquic", soil_temperature_regime = "mesic")
  r <- .c373(.P373(aq, site))
  expect_identical(r$levels$code[1:2], c("I", "I3"))
  expect_true(any(grepl("^Provisional: I Gleyosols rests on a reading", r$warnings)))
  expect_identical(r$candidates$kind, "alternative")
  expect_identical(r$candidates$code, "M2.4.3")  # v0.9.376: the alternative reaches its subgroup
  expect_identical(r$candidates$instead_of, "I")
  expect_false("orders:M" %in% r$provisional_because)
  expect_true(any(grepl("Alternative if that reading fails: M2.4.3 Calcaric Ochri-Aquic Cambosols", r$warnings, fixed = TRUE)))
  tt <- soilKey:::.flatten_key_trace(r$trace)
  expect_true(any(tt$phase == "alternative" & tt$code == "M2.4.3" & tt$status == "info"))
  out <- c(utils::capture.output(print(r)), utils::capture.output(print(r), type = "message"))
  expect_true(any(grepl("Alternative \\(if the provisional reading of I fails\\): M2.4.3 Calcaric", out)))
  expect_false(any(grepl("an attachment, not a key", out)))
  # a negative dipyridyl test: Aquic Cambosols, no alternative
  aq$childs_test <- "negative"
  r <- .c373(.P373(aq, site))
  expect_identical(r$levels$code[1:2], c("M", "M2"))
  expect_null(r$candidates)
  # the examples carry none
  expect_null(.c373(make_cst2001_example("I3"))$candidates)
})
