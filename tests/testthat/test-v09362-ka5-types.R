# v0.9.362: the German KA5 registered (ka5, experimental and opt-in) and keyed
# to the type by Tab. 51 (pp. 283-288): 4 divisions, 21 classes, 56 types, one
# example per type in the KA5 notation with its near miss, the data route,
# provisional names, the figures as consistency flags and the horizon panel.

.ka5_types <- function() eval(formals(make_ka5_example)$type)
.ka5_code <- function(t) if (identical(t, "UEA")) "\u00dcA" else t
.ka5_cls <- function(p) classify_ka5(p, on_missing = "silent")
.ka5_type_of <- function(r) { lv <- r$levels; k <- which(lv$level == "types"); if (length(k)) lv$code[k] else NA_character_ }
.mk6 <- function(h, site = list()) {
  n <- max(lengths(h))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  PedonRecord$new(site = utils::modifyList(list(id = "ka5-t", country = "DE", designation_system = "ka5"), site),
                  horizons = ensure_horizon_schema(data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}

test_that("the registry has the KA5, experimental and opt-in", {
  skip_on_cran()
  s <- soilkey_systems()
  r <- s[s$id == "ka5", ]
  expect_identical(nrow(r), 1L)
  expect_identical(r$classifier, "classify_ka5")
  # v0.9.363: the subtypes level (the terrestrial subtypes of the type texts)
  expect_identical(r$levels, "divisions, classes, types, subtypes")
  expect_identical(r$aliases, "germany, kartieranleitung, bodensystematik")
  expect_identical(r$status, "experimental")
  expect_false(r$default)
  sys <- soilKey:::.system_get("ka5")
  expect_identical(sys$namespace, "ka5")
  expect_identical(as.integer(sys$order), 13L)
  for (a in c("ka5", "germany", "kartieranleitung", "bodensystematik"))
    expect_identical(soilKey:::.system_resolve(a), "ka5")
  # the rules are clean and every predicate carries the suffix
  v <- soilKey:::.validate_rules("ka5")
  expect_identical(v$missing, character(0))
  expect_identical(v$unsuffixed, character(0))
  expect_identical(v$mixed, character(0))
  expect_identical(v$malformed, character(0))
  expect_identical(v$codes, character(0))
  # 4 divisions, 21 classes, 56 types (Liste 31, p. 199)
  rl <- load_rules("ka5")
  expect_length(rl$divisions, 4L)
  expect_identical(sum(lengths(rl$classes)), 21L)
  expect_identical(sum(lengths(rl$types)), 56L)
  tcodes <- unlist(lapply(rl$types, function(l) vapply(l, `[[`, "", "code")))
  expect_setequal(unname(tcodes), unname(vapply(.ka5_types(), .ka5_code, "")))
  # the order of Tab. 51 (Q5)
  expect_identical(vapply(rl$divisions, `[[`, "", "code"), c("[moore]", "[subhydrisch]", "[semiterrestrisch]", "[terrestrisch]"))
  expect_identical(vapply(rl$classes[["[terrestrisch]"]], `[[`, "", "code"),
                   c("F", "O", "T", "R", "X", "Y", "S", "P", "L", "B", "C", "D", "V"))
  expect_identical(vapply(rl$classes[["[semiterrestrisch]"]], `[[`, "", "code"), c("\u00dc", "M", "G", "A"))
  # identify_horizons() reaches the KA5 identification
  f <- identify_horizons(make_ka5_example("BB"), "ka5")
  expect_identical(f$code, c("Ah", "Bv", "C"))
  expect_true(all(c("carrying", "top_gof", "route") %in% names(f)))
})

test_that("each of the 56 type examples keys to its type by the notation; its near miss does not", {
  skip_on_cran()
  for (t in .ka5_types()) {
    r <- .ka5_cls(make_ka5_example(t))
    expect_identical(.ka5_type_of(r), .ka5_code(t), info = t)
    expect_length(r$provisional_because, 0L)
    nm <- .ka5_cls(make_ka5_example(t, near_miss = TRUE))
    expect_false(identical(.ka5_type_of(nm), .ka5_code(t)), info = paste(t, "near miss"))
  }
  # the near misses of the plan's table land where it says
  nm <- function(t) .ka5_type_of(.ka5_cls(make_ka5_example(t, near_miss = TRUE)))
  expect_identical(nm("KV"), "HN")
  expect_identical(nm("HN"), "GH")
  expect_identical(nm("JG"), NA_character_)
  expect_identical(nm("IW"), "IA")
  expect_identical(nm("MR"), "MC")
  expect_identical(nm("MC"), "MN")
  expect_identical(nm("MO"), "MN")
  expect_identical(nm("GH"), "GN")
  expect_identical(nm("GN"), "GG")
  expect_identical(nm("GG"), "BB")
  expect_identical(nm("AQ"), "AZ")
  expect_identical(nm("AB"), "BB")
  expect_identical(nm("TT"), "RZ")
  expect_identical(nm("RN"), "RQ")
  expect_identical(nm("SS"), "BB")
  expect_identical(nm("LF"), "LL")
  expect_identical(nm("LL"), "BB")
  expect_identical(nm("BB"), "CF")
  expect_identical(nm("CR"), "CF")
  expect_identical(nm("VV"), "BB")
  expect_identical(nm("YY"), "YU")
})

test_that("the examples key to their type on the data where the data route reaches it", {
  skip_on_cran()
  for (t in soilKey:::.KA5_DATA_ROUTE_TYPES) {
    p <- make_ka5_example(t, route = "data")
    expect_true(all(is.na(p$horizons$designation)))
    r <- .ka5_cls(p)
    expect_identical(.ka5_type_of(r), .ka5_code(t), info = t)
    expect_true(all(r$trace$horizons$route == "data"), info = t)
  }
  # the others need the notation: transitions and relict or buried horizons
  expect_setequal(setdiff(.ka5_types(), soilKey:::.KA5_DATA_ROUTE_TYPES), c("MK", "MD", "MH", "VV", "VW"))
})

test_that("names as printed, glosses, and the result's levels", {
  skip_on_cran()
  r <- .ka5_cls(make_ka5_example("MH"))
  # the subtype level of the wet types since v0.9.364
  expect_identical(r$name, "Normhaftn\u00e4ssemarsch")
  expect_identical(r$levels$level, c("divisions", "classes", "types", "subtypes"))
  expect_identical(r$levels$name[1:3], c("Semiterrestrische B\u00f6den", "Marschen", "Haftn\u00e4ssemarsch"))
  expect_identical(r$levels$name_en[3], "Adhesive-water marsh")
  expect_identical(r$rsg_or_order, "Semiterrestrische B\u00f6den")
  expect_identical(r$system_id, "ka5")
  expect_identical(.ka5_cls(make_ka5_example("UEA"))$levels$name[3], "Strand")
  expect_identical(.ka5_cls(make_ka5_example("\u00dcA"))$levels$code[3], "\u00dcA")
  expect_identical(.ka5_cls(make_ka5_example("VV"))$name, "... \u00fcber Fersiallit")
})

test_that("a gate the data leave open makes the name provisional", {
  skip_on_cran()
  # mottles with no record of the water: the semiterrestrial gate undecided
  p <- .mk6(list(designation = NA_character_, top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 90), oc_pct = c(3, 0.3, 0.1),
                 clay_pct = 20, redoximorphic_features_pct = c(0, 30, 0), bleached_pct = c(0, 60, 0),
                 rock_diggable = c(NA, NA, TRUE)), list(designation_system = NULL))
  r <- .ka5_cls(p)
  expect_true("divisions:[semiterrestrisch]" %in% r$provisional_because)
  expect_true(any(grepl("Provisional", r$warnings, fixed = TRUE)))
  # the Fahlerde on the designation, its clay not recorded (C13)
  p <- make_ka5_example("LF")
  p$horizons$clay_pct <- NA_real_
  r <- .ka5_cls(p)
  expect_identical(.ka5_type_of(r), "LF")
  expect_true("types:LF" %in% r$provisional_because)
  # the Rigosol in the key's order, the kind of cultivation not recorded (Q23)
  p <- make_ka5_example("YY")
  p$site$deep_cultivation_kind <- NULL
  r <- .ka5_cls(p)
  expect_identical(.ka5_type_of(r), "YY")
  expect_true("types:YY" %in% r$provisional_because)
  expect_true(any(grepl("Treposol a candidate", r$warnings, fixed = TRUE)))
  # an Ah of 4 dm or more without Axh: undecided between R and T (Q17)
  r <- .ka5_cls(.mk6(list(designation = c("Ah", "ilC"), top_cm = c(0, 45), bottom_cm = c(45, 100))))
  expect_true("classes:R" %in% r$provisional_because)
  expect_true(any(grepl("Schwarzerden (Q17)", r$warnings, fixed = TRUE)))
  # an earthified H no type takes: the class K without a type (Q22)
  r <- .ka5_cls(.mk6(list(designation = c("Hv", "Hw", "Hr"), top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 120))))
  expect_identical(r$levels$code, c("[moore]", "K"))
  expect_true("types:K" %in% r$provisional_because)
  expect_true(any(grepl("type level is provisional", r$warnings, fixed = TRUE)))
  # a rock prefix the data leave open: the R type undecided
  r <- .ka5_cls(.mk6(list(designation = NA_character_, top_cm = c(0, 20), bottom_cm = c(20, 100), oc_pct = c(3, NA),
                          clay_pct = c(15, NA), rock_diggable = c(NA, TRUE)), list(designation_system = NULL)))
  expect_identical(r$levels$code, c("[terrestrisch]", "R"))
  expect_true(any(startsWith(r$provisional_because, "types:R")))
})

test_that("the Moor under a mineral cover (Q7) and the figures as consistency flags", {
  skip_on_cran()
  r <- .ka5_cls(.mk6(list(designation = c("Ah", "nHw", "nHr"), top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 150))))
  expect_identical(r$levels$code[1], "[moore]")
  expect_true(any(grepl("an overlay of \"flach", r$trace$divisions[["[moore]"]]$evidence[[1]]$notes, fixed = TRUE)))
  r <- .ka5_cls(.mk6(list(designation = c("Ah", "nHw", "nHr"), top_cm = c(0, 45, 80), bottom_cm = c(45, 80, 150))))
  expect_false(identical(r$levels$code[1], "[moore]"))
  # an S at exactly 4 dm: Tab. 51 and the type texts read it as no S (Q6); Abb. 22 would
  r <- .ka5_cls(.mk6(list(designation = c("Ah", "Bv", "Sw", "IISd"), top_cm = c(0, 25, 40, 60), bottom_cm = c(25, 40, 60, 100))))
  expect_identical(.ka5_type_of(r), "BB")
  expect_identical(r$trace$figures$figure, "Abb. 22")
  expect_true(any(grepl("Consistency with the figures", r$warnings, fixed = TRUE)))
  # a Bv over a T that does not reach 1.5 dm goes on to the Terrae calcis (Abb. 22, Q5)
  expect_identical(.ka5_type_of(.ka5_cls(make_ka5_example("BB", near_miss = TRUE))), "CF")
})

test_that("the horizon panel in the result", {
  skip_on_cran()
  r <- .ka5_cls(make_ka5_example("GG", route = "data"))
  pn <- r$trace$horizon_panel
  expect_s3_class(pn, "data.frame")
  expect_identical(pn$horizon, c("Ah", "Go", "Gr"))
  expect_identical(pn$route, rep("data", 3L))
  expect_true(is.data.frame(r$trace$horizons))
  expect_true(any(grepl("soilKey's order of tests on the recorded data", r$warnings, fixed = TRUE)))
  r <- .ka5_cls(make_ka5_example("GG"))
  expect_identical(r$trace$horizon_panel$route, rep("both", 3L))
  expect_length(r$qualifiers, 0L)
  # the print method reads the trace with the frame in it
  expect_no_error(suppressMessages(utils::capture.output(print(r), type = "message")))
})

test_that("classify_all() runs the KA5 when asked, never by default", {
  skip_on_cran()
  p <- make_ka5_example("PP")
  a <- classify_all(p, systems = "ka5", on_missing = "silent")
  expect_identical(a$ka5$levels$code[3], "PP")
  expect_false("ka5" %in% soilKey:::.system_ids(default_only = TRUE))
})

test_that("the app offers one example per type, each keying to it; the catalogue names the system", {
  skip_on_cran()
  ui <- testthat::test_path("..", "..", "inst", "shiny", "classify_app_pro", "R", "utils_ui.R")
  if (!file.exists(ui)) ui <- system.file("shiny", "classify_app_pro", "R", "utils_ui.R", package = "soilKey")
  skip_if_not(nzchar(ui) && file.exists(ui), "app sources not available")
  env <- new.env()
  sys.source(ui, envir = env)
  ex <- env$.PRO_NATIONAL_EXAMPLES$ka5
  expect_length(ex, 56L)
  expect_setequal(unname(ex), .ka5_types())
  for (k in seq_along(ex)) {
    r <- .ka5_cls(make_ka5_example(ex[[k]]))
    # v0.9.363: the name is the subtype's where the type has subtypes; the
    # catalogue names the type
    expect_identical(r$levels$name[r$levels$level == "types"], names(ex)[k], info = ex[[k]])
  }
  p <- system.file("i18n", "translations.yaml", package = "soilKey")
  if (!nzchar(p)) p <- testthat::test_path("..", "..", "inst", "i18n", "translations.yaml")
  cat <- yaml::read_yaml(p)
  expect_identical(cat$en[["classify.edition_ka5"]], soilKey:::.system_get("ka5")$edition)
  expect_identical(cat$en[["classify.sys_ka5"]], "Germany (KA5)")
  expect_true(nzchar(cat$pt[["classify.sys_ka5"]] %||% ""))
})

test_that("the KA5 key sources are ASCII", {
  skip_on_cran()
  src <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(src), "package sources not available")
  for (fn in c("diagnostics-ka5.R", "key-ka5.R")) {
    txt <- readLines(file.path(src, fn), warn = FALSE)
    expect_false(any(grepl("[^\\x01-\\x7F]", txt, perl = TRUE)), info = fn)
  }
})

# ---- the review of 2026-11-06 (Hugo: "Corrigir os 19") ------------------------------------

.k6 <- function(des, top, bot, ..., site = list()) .ka5_cls(.mk6(c(list(designation = des, top_cm = top, bottom_cm = bot), list(...)), site))
.tp <- function(r) paste(r$levels$code, collapse = ">")

test_that("D1: a buried soil does not decide the present soil (p. 283)", {
  skip_on_cran()
  expect_identical(.ka5_type_of(.k6(c("Ah", "Bv", "IIfAe", "IIfBhs", "IIC"), c(0, 10, 50, 65, 80), c(10, 50, 65, 80, 120))), "BB")
  expect_identical(.ka5_type_of(.k6(c("Ah", "M", "IIfAe", "IIfBhs", "IIC"), c(0, 25, 60, 75, 90), c(25, 60, 75, 90, 120))), "YK")
  expect_identical(.ka5_type_of(.k6(c("Ap", "E", "IIfAe", "IIfBs", "IIC"), c(0, 30, 70, 80, 100), c(30, 70, 80, 100, 130))), "YE")
  r <- .k6(c("Ah", "Bv", "IIfGo"), c(0, 10, 30), c(10, 30, 100))
  expect_false(identical(r$levels$code[1], "[semiterrestrisch]"))
  expect_identical(.ka5_type_of(r), "BB")
})

test_that("D2: tidal prefixes only in the present solum; a semiterrestrial division with no class falls through", {
  skip_on_cran()
  r <- .k6(c("Ah", "Bv", "IItmGo", "IItmGr"), c(0, 10, 80, 110), c(10, 80, 110, 150))
  expect_identical(.tp(r), "[terrestrisch]>B>BB>BBn")
})

test_that("D3: a prefix the notation leaves out is filled from the data, or left open (Q15)", {
  skip_on_cran()
  expect_identical(.ka5_type_of(.k6(c("Ah", "mC"), c(0, 20), c(20, 60), carbonate_whole_soil_pct = c(NA, 95))), "RR")
  expect_identical(.ka5_type_of(.k6(c("Ah", "lC"), c(0, 20), c(20, 150), caco3_pct = c(0, 0))), "RQ")
  expect_identical(.ka5_type_of(.k6(c("Ah", "mC"), c(0, 20), c(20, 60), caco3_pct = c(0, 0))), "RN")
  expect_identical(.ka5_type_of(.k6(c("aAh", "alC", "aG"), c(0, 20, 90), c(20, 90, 150), caco3_pct = c(0, 0.2, 0.2))), "AQ")
  expect_identical(.ka5_type_of(.k6(c("Hw", "Hr"), c(0, 40), c(40, 150), peat_moor_type = "low-moor", oc_pct = 45)), "HN")
  expect_identical(.ka5_type_of(.k6(c("Hv", "Ht", "Hr"), c(0, 20, 50), c(20, 50, 150), peat_moor_type = "low-moor", oc_pct = 45)), "KV")
  r <- .k6(c("Axh", "lCc"), c(0, 35), c(35, 100))
  expect_true(nrow(r$levels) >= 2L)
  expect_true(length(r$provisional_because) > 0L)
  # the Vega is not reached while the Paternia is undecided
  r <- .k6(c("aAh", "alC", "aG"), c(0, 20, 90), c(20, 90, 150))
  expect_true(is.na(.ka5_type_of(r)))
  expect_true("types:A" %in% r$provisional_because)
})

test_that("D4, D5: the Axh window and the colour conflict (pp. 211, 286; Q17)", {
  skip_on_cran()
  expect_true(.ka5_type_of(.k6(c("Ap", "Axh", "elCc"), c(0, 30, 70), c(30, 70, 150))) %in% c("TC", "TT"))
  expect_identical(.k6(c("M", "Axh", "lCc"), c(0, 15, 70), c(15, 70, 120))$levels$code[2], "T")
  r <- .k6(c("Axh", "lCc"), c(0, 50), c(50, 100), munsell_hue_moist = "10YR", munsell_value_moist = c(5, 6), munsell_chroma_moist = c(3, 4))
  expect_true("classes:T" %in% r$provisional_because)
  expect_true(any(grepl("conflict of the designation and the data", unlist(r$trace$classes$T$evidence[[1]]$notes), fixed = TRUE)))
})

test_that("D6: a run that reaches the bottom of the description is undecided, not a silent no", {
  skip_on_cran()
  expect_true("divisions:[moore]" %in% .k6(c("nHv", "nHw"), c(0, 10), c(10, 25))$provisional_because)
  expect_true("classes:Y" %in% .k6(c("Ah", "M"), c(0, 20), c(20, 35))$provisional_because)
  expect_true("classes:T" %in% .k6("Axh", 0, 35)$provisional_because)
  expect_true("classes:Y" %in% .k6(c("Ap", "Ex"), c(0, 25), c(25, 38))$provisional_because)
})

test_that("D7, D8, D9, D10: the Moor body, the Kalkmarsch, the R bound, the Moore types", {
  skip_on_cran()
  expect_identical(.tp(.k6(c("nHw", "IIfF", "IIInHr"), c(0, 20, 25), c(20, 25, 120))), "[moore]>H>HN>HNn")
  expect_identical(.ka5_type_of(.k6(c("tmAh", "tmGo", "tmeGo", "tmzeGr"), c(0, 20, 30, 60), c(20, 30, 60, 120))), "MC")
  r <- .k6(c("R-Ap", "R"), c(0, 25), c(25, 40))
  expect_true("classes:Y" %in% r$provisional_because)
  r2 <- .k6(c("R-Ap", "R"), c(0, 25), c(25, 40), site = list(deep_cultivation_kind = "repeated_trenching"))
  expect_true("classes:Y" %in% r2$provisional_because)
  expect_identical(.ka5_type_of(.k6(c("hHv", "hHw", "IInHv"), c(0, 15, 80), c(15, 80, 150))), "KH")
  expect_false(identical(.ka5_type_of(.k6(c("hH", "hHw"), c(0, 15), c(15, 150))), "KH"))
})

test_that("D11, D12, D13: the Ael undecided, the depth of the groundwater, the stand-ins in the result", {
  skip_on_cran()
  dd <- function(h, site = list()) .ka5_cls(.mk6(h, utils::modifyList(list(designation_system = NULL), site)))
  r <- dd(list(designation = NA_character_, top_cm = c(0, 10, 45, 100), bottom_cm = c(10, 45, 100, 130), oc_pct = c(2, 0.3, 0.2, 0.1),
               clay_pct = c(10, 6, 26, 18), silt_pct = 60, sand_pct = c(30, 34, 14, 22),
               munsell_hue_moist = c("10YR", NA, "7.5YR", "10YR"), munsell_value_moist = c(3, NA, 4, 5),
               munsell_chroma_moist = c(2, NA, 4, 4), clay_films_amount = c(NA, NA, "common", NA), caco3_pct = c(0, 0, 0, 12)))
  expect_identical(r$levels$code[2], "L")
  expect_true("types:LF" %in% r$provisional_because)
  ps <- list(designation = NA_character_, top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 100), oc_pct = c(3, 0.5, 0.3),
             clay_pct = c(15, 15, 30), silt_pct = 60, sand_pct = c(25, 25, 10), redoximorphic_features_pct = c(0, 30, 40),
             bleached_pct = c(0, 55, 30), ksat_mm_h = c(NA, 20, 1), air_capacity_pct = c(NA, 8, 2), rh_value = c(NA, 17, NA))
  expect_identical(.ka5_type_of(dd(ps, list(groundwater_depth_cm = 250))), "SS")
  g <- list(designation = NA_character_, top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 120), oc_pct = c(3, 0.5, 0.3), clay_pct = 20,
            silt_pct = 40, sand_pct = 40, redoximorphic_features_pct = c(0, 20, 1), reduction_colours_pct = c(0, 5, 99),
            rh_value = c(NA, NA, 12), munsell_hue_moist = c("10YR", "2.5Y", "5GY"), munsell_value_moist = c(3, 5, 5),
            munsell_chroma_moist = c(2, 3, 1))
  r <- dd(g)
  expect_identical(.ka5_type_of(r), "GG")
  expect_true(any(startsWith(r$provisional_because, "horizons:")))
  expect_true(any(grepl("gleyic_properties()", r$warnings, fixed = TRUE)))
})

test_that("D14: z on the F at the GOF is tidal; otherwise inland or tidal undecided", {
  skip_on_cran()
  expect_identical(.ka5_type_of(.k6(c("zeFo", "zeFr"), c(0, 15), c(15, 80), rh_value = c(15, 10))), "IW")
  expect_identical(.ka5_type_of(.k6(c("zeAi", "zelC", "zeGo"), c(0, 5, 50), c(5, 50, 100))), "\u00dcA")
  r <- .k6(c("Fo", "Fr"), c(0, 15), c(15, 80), rh_value = c(15, 10))
  expect_identical(r$levels$code[1], "[subhydrisch]")
  expect_true(any(startsWith(r$provisional_because, "classes:")))
  expect_true(.ka5_type_of(.k6(c("eAh", "eGo", "zeGr"), c(0, 20, 60), c(20, 60, 120))) %in% c("GG", "MC", "MN"))
})

test_that("D16, D17, D18, D19 and Hugo's decisions", {
  skip_on_cran()
  # a class with no type: provisional at the type level
  r <- .k6(c("Ah", "Bv", "Sd"), c(0, 10, 30), c(10, 30, 100))
  expect_identical(r$levels$code[2], "S")
  expect_true("types:S" %in% r$provisional_because)
  # the no-differentiation note only when every class failed
  r <- .k6(c("Axh", "lCc"), c(0, 35), c(35, 100))
  expect_false(any(grepl("no horizon differentiation", r$warnings, fixed = TRUE)))
  # the Organomarsch: written without o, no MO (Hugo); data short of its clay
  r <- .ka5_cls(make_ka5_example("MO", near_miss = TRUE))
  expect_false(identical(.ka5_type_of(r), "MO"))
  mo <- soilKey:::organomarsch_ka5(make_ka5_example("MO", route = "data"))
  expect_true(mo$passed)
  expect_length(mo$missing, 0L)
  # a marl C is no limestone for the Terrae calcis
  r <- .k6(c("Ah", "Tv", "emC"), c(0, 10, 40), c(10, 40, 60))
  expect_false(identical(r$levels$code[2], "C"))
  # no topsoil under 30 cm or more of peat
  p <- .mk6(list(designation = c("H1", "H2", "Cr"), top_cm = c(0, 40, 90), bottom_cm = c(40, 90, 150), oc_pct = c(40, 45, 1)),
            list(designation_system = NULL))
  f <- identify_horizons(p, "ka5")
  expect_false(isTRUE(substr(f$carrying[3], 1, 1) == "A"))
  # an Ai on block debris is a Syrosem (Hugo)
  expect_identical(.ka5_type_of(.k6(c("Ai", "xC"), c(0, 3), c(3, 60))), "OO")
})
