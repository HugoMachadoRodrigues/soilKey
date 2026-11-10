# v0.9.385: the 153 soil units of the FAO-Unesco Revised Legend (1988), the
# second level of fao1988 under its 28 major soil groupings: the units in the
# printed key order, one example per unit and one near miss per unit gate with
# a printed number (make_fao_example()), the provisional units, the names of
# the final list, the CSV of data-raw against the rule files, the i18n keys,
# the thresholds and the readings.

.f385 <- function(p, ...) classify_fao(p, on_missing = "silent", ...)
.src385 <- function(...) testthat::test_path("..", "..", ...)
# the 153 units in the order the key prints them (pp. 75-89)
.UNITS385 <- c(
  "HSi", "HSt", "HSl", "HSf", "HSs", "ATa", "ATf", "ATc", "ATu", "LPq", "LPi", "LPk", "LPm", "LPu",
  "LPd", "LPe", "VRy", "VRk", "VRd", "VRe", "FLt", "FLs", "FLm", "FLc", "FLu", "FLd", "FLe", "SCi",
  "SCg", "SCm", "SCy", "SCk", "SCn", "SCh", "GLi", "GLt", "GLa", "GLm", "GLu", "GLk", "GLd", "GLe",
  "ANi", "ANg", "ANz", "ANm", "ANu", "ANh", "ARg", "ARa", "ARc", "ARl", "ARo", "ARb", "ARh", "RGi",
  "RGu", "RGy", "RGc", "RGd", "RGe", "PZi", "PZg", "PZc", "PZf", "PZb", "PZh", "PTa", "PTu", "PTd",
  "PTe", "FRp", "FRg", "FRu", "FRr", "FRx", "FRh", "PLi", "PLm", "PLu", "PLd", "PLe", "SNg", "SNj",
  "SNm", "SNy", "SNk", "SNh", "GRg", "GRh", "CHg", "CHl", "CHw", "CHk", "CHh", "KSy", "KSl", "KSk",
  "KSh", "PHg", "PHj", "PHl", "PHc", "PHh", "PDi", "PDg", "PDj", "PDd", "PDe", "GYp", "GYk", "GYl",
  "GYh", "CLp", "CLl", "CLh", "NTu", "NTr", "NTh", "ALp", "ALg", "ALj", "ALu", "ALf", "ALh", "ACp",
  "ACg", "ACu", "ACf", "ACh", "LVg", "LVj", "LVa", "LVv", "LVk", "LVf", "LVx", "LVh", "LXp", "LXg",
  "LXj", "LXa", "LXf", "LXh", "CMi", "CMg", "CMv", "CMu", "CMc", "CMo", "CMd", "CMx", "CMe")
.GROUPS385 <- c("HS", "AT", "LP", "VR", "FL", "SC", "GL", "AN", "AR", "RG", "PZ", "PT", "FR", "PL",
                "SN", "GR", "CH", "KS", "PH", "PD", "GY", "CL", "NT", "AL", "AC", "LV", "LX", "CM")
.edit385 <- function(p, rows = NULL, ...) {
  h <- as.data.frame(p$horizons)
  if (!is.null(rows)) h <- h[rows, , drop = FALSE]
  v <- list(...)
  for (k in names(v)) h[[k]] <- v[[k]]
  PedonRecord$new(site = p$site, horizons = ensure_horizon_schema(data.table::as.data.table(h)))
}

test_that("the second level is registered: 153 units under the 28 groupings", {
  skip_on_cran()
  sys <- soilKey:::.system_get("fao1988")
  expect_identical(sys$levels, c("major_groups", "units"))
  k <- load_rules("fao1988")
  expect_identical(sort(names(k$units)), sort(.GROUPS385))
  expect_equal(sum(lengths(k$units)), 153L)
  expect_identical(vapply(.GROUPS385, function(g) length(k$units[[g]]), 0L, USE.NAMES = FALSE),
                   c(5L, 4L, 7L, 4L, 7L, 7L, 8L, 6L, 7L, 6L, 6L, 4L, 6L, 5L, 6L, 2L, 5L, 4L, 5L,
                     5L, 4L, 3L, 3L, 6L, 5L, 8L, 6L, 9L))
  expect_equal(soilKey:::.validate_rules("fao1988")$missing, character(0))
  expect_identical(soilKey:::.f88_unit_codes(), .UNITS385)
})

test_that("the key order is the printed order, each grouping closed by its residual", {
  skip_on_cran()
  k <- load_rules("fao1988")
  codes <- unlist(lapply(.GROUPS385, function(g) vapply(k$units[[g]], `[[`, "", "code")))
  expect_identical(codes, .UNITS385)
  for (g in .GROUPS385) {
    u <- k$units[[g]]
    expect_true(all(startsWith(vapply(u, `[[`, "", "code"), g)), info = g)
    dflt <- vapply(u, function(e) isTRUE(e$tests$default), NA)
    # the last unit, and only it, is the residual
    expect_identical(dflt, c(rep(FALSE, length(u) - 1L), TRUE), info = g)
    last <- u[[length(u)]]$name
    expect_true(grepl("^(Haplic|Eutric|Terric|Urbic) ", last), info = g)
    for (e in u) {
      expect_true(nzchar(e$name_pt %||% ""), info = e$code)
      expect_true(grepl(e$name, e$name_pt, fixed = TRUE), info = e$code)
      expect_true(as.integer(e$page) %in% 75:89, info = e$code)
      expect_true(nzchar(e$notes %||% ""), info = e$code)
    }
  }
  # Haplic in 17 groupings, Eutric in 9, the two described residuals
  res <- vapply(.GROUPS385, function(g) { u <- k$units[[g]]; sub(" .*", "", u[[length(u)]]$name) }, "")
  expect_equal(sum(res == "Haplic"), 17L)
  expect_equal(sum(res == "Eutric"), 9L)
  expect_identical(unname(res[c("HS", "AT")]), c("Terric", "Urbic"))
  expect_true(nzchar(k$units$HS[[5]]$description))
  expect_true(nzchar(k$units$AT[[4]]$description))
})

test_that("the CSV of data-raw equals the rule files", {
  skip_on_cran()
  f <- .src385("data-raw", "fao1988_units.csv")
  skip_if_not(file.exists(f))
  csv <- utils::read.csv(f, stringsAsFactors = FALSE)
  u <- csv[csv$level == "unit", ]
  u <- u[order(match(u$parent, .GROUPS385), as.numeric(sub("^[0-9]+[.]", "", u$key_position))), ]
  k <- load_rules("fao1988")
  ents <- unlist(lapply(.GROUPS385, function(g) k$units[[g]]), recursive = FALSE)
  expect_identical(u$code, vapply(ents, `[[`, "", "code"))
  expect_identical(u$name, vapply(ents, `[[`, "", "name"))
  expect_identical(u$parent, substr(u$code, 1L, 2L))
  expect_identical(as.character(u$page_key), vapply(ents, function(e) as.character(e$page), ""))
  expect_identical(u$code, .UNITS385)
})

test_that("every name of the final list resolves to exactly one unit", {
  skip_on_cran()
  f <- .src385("data-raw", "fao1988_units.csv")
  skip_if_not(file.exists(f))
  csv <- utils::read.csv(f, stringsAsFactors = FALSE)
  u <- csv[csv$level == "unit", ]
  # the final list (pp. 137-140) prints Gypsic Regosols (Q7: Gypsiric, Gypsic its alias)
  listed <- ifelse(u$code == "RGy", "Gypsic Regosols", u$name)
  got <- soilKey:::.f88_unit_lookup(listed)
  expect_identical(got, u$code)
  expect_identical(soilKey:::.f88_unit_lookup(c("Gypsiric Regosols", "  haplic luvisols ", "Gelic Luvisols")),
                   c("RGy", "LVh", NA))
  # names are unique across the legend
  k <- load_rules("fao1988")
  nm <- unlist(lapply(k$units, function(l) vapply(l, `[[`, "", "name")))
  expect_false(anyDuplicated(nm) > 0L)
})

test_that("every unit example keys to its own unit, not provisionally", {
  skip_on_cran()
  for (cd in .UNITS385) {
    r <- .f385(make_fao_example(cd))
    expect_identical(r$levels$code, c(substr(cd, 1L, 2L), cd), info = cd)
    expect_length(r$provisional_because, 0L)
    expect_identical(r$levels$level, c("major_groups", "units"))
    expect_identical(r$name, r$levels$name[2])
    expect_identical(r$rsg_or_order, r$levels$name[1])
    expect_true(all(nzchar(r$levels$name_pt)), info = cd)
    expect_true(!anyNA(r$levels$page), info = cd)
  }
  expect_identical(.f385(make_fao_example("LVx"))$name, "Chromic Luvisols")
  expect_identical(.f385(make_fao_example("RGy"))$name, "Gypsiric Regosols")
  expect_identical(.f385(make_fao_example("HSs"))$levels$name_pt[2],
                   "Histossolos Térricos (Terric Histosols)")
})

test_that("one near miss per unit gate with a printed number, each failing that gate decided", {
  skip_on_cran()
  nmc <- soilKey:::.F88_UNIT_NEAR_MISS
  expect_equal(length(nmc), 87L)
  expect_true(all(nmc %in% .UNITS385))
  k <- load_rules("fao1988")
  for (cd in nmc) {
    e <- Find(function(z) identical(z$code, cd), k$units[[substr(cd, 1L, 2L)]])
    p <- make_fao_example(cd); q <- make_fao_example(cd, near_miss = TRUE)
    expect_true(isTRUE(soilKey:::evaluate_rsg_tests(p, e$tests)$passed), info = cd)
    expect_true(isFALSE(soilKey:::evaluate_rsg_tests(q, e$tests)$passed), info = cd)
    r <- .f385(q)
    expect_false(identical(r$levels$code[2], cd), info = cd)
    expect_length(r$provisional_because, 0L)
  }
  # the near misses that stay in their grouping land on the next unit that passes
  want <- c(LPq = "LPe", VRd = "VRe", FLd = "FLe", GLd = "GLe", RGd = "RGe", PTd = "PTe",
            FRr = "FRh", FRx = "FRh", PLd = "PLe", PDd = "PDe", ATc = "ATu", LVx = "LVh",
            CMd = "CMx", CMx = "CMe", ARa = "ARh", HSf = "HSs", LPk = "LPm", ANz = "ANu")
  for (cd in names(want))
    expect_identical(.f385(make_fao_example(cd, near_miss = TRUE))$levels$code[2], want[[cd]], info = cd)
  expect_error(make_fao_example("CHw", near_miss = TRUE), "no printed number")
  expect_error(make_fao_example("XXq"), "not a code")
})

test_that("the printed numbers at their edges", {
  skip_on_cran()
  # within X cm includes a feature starting at X: gleyic at 100, calcic at
  # 125, permafrost at 200 pass; one cm deeper fails
  expect_true(soilKey:::gleyic_properties_fao88(make_fao_example("CMg"), max_top_cm = 100)$passed)
  expect_false(soilKey:::gleyic_properties_fao88(make_fao_example("CMg", near_miss = TRUE), max_top_cm = 100)$passed)
  expect_true(soilKey:::calcic_fao88(make_fao_example("LVk"), max_top_cm = 125)$passed)
  expect_true(soilKey:::permafrost_fao88(make_fao_example("CMi"), max_top_cm = 200)$passed)
  expect_false(soilKey:::permafrost_fao88(make_fao_example("CMi", near_miss = TRUE), max_top_cm = 200)$passed)
  # Lithic Leptosols: rock at 10 cm passes, at 11 fails
  expect_true(soilKey:::lithic_fao88(make_fao_example("LPq"))$passed)
  expect_false(soilKey:::lithic_fao88(make_fao_example("LPq", near_miss = TRUE))$passed)
  # Rendzic: above 40 % CaCO3 equivalent; exactly 40 fails
  expect_true(soilKey:::rendzic_fao88(make_fao_example("LPk"))$passed)
  expect_false(soilKey:::rendzic_fao88(make_fao_example("LPk", near_miss = TRUE))$passed)
  # strongly humic is above 1.4 % over 100 cm: 1.40 exactly fails (Humic Plinthosols)
  sh <- soilKey:::strongly_humic_fao88(make_fao_example("PTu", near_miss = TRUE))
  expect_false(sh$passed)
  expect_equal(sh$evidence$oc_weighted_0_100, 1.368, tolerance = 1e-9)
  # Dystric: below 50 % throughout 20-50 cm; 50 fails
  expect_true(soilKey:::bs_below_fao88(make_fao_example("GLd"))$passed)
  expect_false(soilKey:::bs_below_fao88(make_fao_example("GLd", near_miss = TRUE))$passed)
  # Ferric Podzols: Fe / C of 6 in every subhorizon of the B
  f <- soilKey:::spodic_fe_oc_fao88(make_fao_example("PZf"))
  expect_true(f$passed)
  expect_equal(min(f$evidence$ratio), 6)
  # Cambic Podzols: an albic E of 2 cm, broken; 3 cm fails
  expect_true(soilKey:::podzol_cambic_fao88(make_fao_example("PZb"))$passed)
  expect_false(soilKey:::podzol_cambic_fao88(make_fao_example("PZb", near_miss = TRUE))$passed)
  # Albic Arenosols: 50 cm of albic E; 49 fails
  expect_true(soilKey:::albic_e_thick_fao88(make_fao_example("ARa"))$passed)
  expect_false(soilKey:::albic_e_thick_fao88(make_fao_example("ARa", near_miss = TRUE))$passed)
  # Luvic Arenosols: 3 points more clay; 2 fails
  expect_true(soilKey:::clay_increase_or_lamellae_fao88(make_fao_example("ARl"))$passed)
  expect_false(soilKey:::clay_increase_or_lamellae_fao88(make_fao_example("ARl", near_miss = TRUE))$passed)
  # Geric Ferralsols: bases + KCl acidity of 1.5 cmol(+)/kg clay or less
  expect_true(soilKey:::property_in_b_fao88(make_fao_example("FRg"), "geric", "ferralic_b")$passed)
  expect_false(soilKey:::property_in_b_fao88(make_fao_example("FRg", near_miss = TRUE), "geric", "ferralic_b")$passed)
  # Ferralic Cambisols: CEC below 24 per kg clay in the cambic B
  expect_true(soilKey:::property_in_b_fao88(make_fao_example("CMo"), "ferralic", "cambic_b")$passed)
  expect_false(soilKey:::property_in_b_fao88(make_fao_example("CMo", near_miss = TRUE), "ferralic", "cambic_b")$passed)
  # colours: rhodic dry value one unit above the moist value, two fail
  expect_true(soilKey:::b_colour_fao88(make_fao_example("FRr"), "rhodic", "ferralic_b")$passed)
  expect_false(soilKey:::b_colour_fao88(make_fao_example("FRr", near_miss = TRUE), "rhodic", "ferralic_b")$passed)
  # chromic: 7.5YR needs a chroma above 4; 5YR passes at any chroma
  expect_true(soilKey:::b_colour_fao88(make_fao_example("CMx"), "chromic", "cambic_b")$passed)
  expect_false(soilKey:::b_colour_fao88(make_fao_example("CMx", near_miss = TRUE), "chromic", "cambic_b")$passed)
  # Cumulic Anthrosols: more than 50 cm of irrigation sediments; 50 fails (and the soil is Urbic)
  expect_true(soilKey:::cumulic_fao88(make_fao_example("ATc"))$passed)
  expect_false(soilKey:::cumulic_fao88(make_fao_example("ATc", near_miss = TRUE))$passed)
})

test_that("an undecided unit before the assigned one makes the unit provisional", {
  skip_on_cran()
  # the grouping examples: sulfur not measured leaves the Thionic Fluvisols undecided
  r <- .f385(make_fao_example("FL"))
  expect_identical(r$levels$code, c("FL", "FLe"))
  expect_true("units:FLt" %in% r$provisional_because)
  expect_false(any(grepl("^major_groups:", r$provisional_because)))
  expect_true("s_total_pct" %in% r$trace$units$FLt$missing)
  expect_true(any(grepl("^Provisional", r$warnings)))
  # drainage not recorded: the Folic and Fibric Histosols (a fibric peat) undecided
  p <- make_fao_example("HSf")
  p$site$drainage_class <- NA
  r2 <- .f385(p)
  expect_identical(r2$levels$code[2], "HSs")
  expect_true(all(c("units:HSl", "units:HSf") %in% r2$provisional_because))
  # the trace holds both levels
  expect_identical(names(r2$trace), c("major_groups", "units"))
  expect_identical(names(r2$trace$units), c("HSi", "HSt", "HSl", "HSf", "HSs"))
})

test_that("Hugo's decisions of 2026-11-09 on the readings of this part", {
  skip_on_cran()
  E <- soilKey:::.f88x_edit
  # A. the fimic A needs a sign of manuring; artefacts neither make nor exclude it
  expect_identical(.f385(make_anthrosol_canonical())$levels$code, c("AT", "ATf"))
  ac <- make_anthrosol_canonical(); ac$horizons$artefacts_pct <- 0
  expect_identical(.f385(ac)$levels$code, c("AT", "ATf"))
  expect_identical(.f385(make_fao_example("AT"))$levels$code[2], "ATu")
  fm <- soilKey:::fimic_a_fao88(make_fao_example("AT"))
  expect_false(fm$passed)
  expect_true("anthropic_layers_without_manuring_not_fimic" %in% fm$evidence$flags)
  expect_true(soilKey:::fimic_a_fao88(make_fao_example("ATf"))$passed)
  expect_false(exists("fimic_not_manured_fao88", envir = asNamespace("soilKey")))
  # B. Rhodic: a dry value not recorded leaves it undecided, flagged
  fr <- E(make_fao_example("FRr"), munsell_value_dry = NA_real_)
  rc <- soilKey:::b_colour_fao88(fr, "rhodic", "ferralic_b")
  expect_true(is.na(rc$passed))
  expect_true("dry_value_not_recorded" %in% rc$evidence$flags)
  r <- .f385(fr)
  expect_identical(r$levels$code[1], "FR")
  expect_true("units:FRr" %in% r$provisional_because)
  # C. Vitric: smeary not recorded is undecided on silt loam, absent on loam
  an <- make_fao_example("ANu")
  v <- soilKey:::vitric_fao88(an)
  expect_false(v$passed)
  expect_identical(v$evidence$texture_class_0_100, "SIL")
  expect_true(is.na(soilKey:::vitric_fao88(E(an, smeary_consistence = NA))$passed))
  expect_true("units:ANz" %in% .f385(E(an, smeary_consistence = NA))$provisional_because)
  expect_true(soilKey:::vitric_fao88(E(an, smeary_consistence = NA, silt_pct = 40, sand_pct = 40))$passed)
  expect_identical(.f385(make_fao_example("AN"))$levels$code[2], "ANz")
  # D. Rendzic: a mollic A directly on limestone
  lp <- make_fao_example("LPk")
  q <- E(lp, rows = c(1, 3), designation = c("Ah", "R"), top_cm = c(0, 20), bottom_cm = c(20, 60),
         caco3_pct = c(20, NA))
  expect_false(soilKey:::rendzic_fao88(q)$passed)
  q$site$parent_material <- "limestone"
  rz <- soilKey:::rendzic_fao88(q)
  expect_true(rz$passed)
  expect_true("calcareous_rock_from_record" %in% rz$evidence$flags)
  expect_identical(.f385(q)$levels$code, c("LP", "LPk"))
  # E. the colour of the B is read in every layer of it
  lv <- make_fao_example("LVx")
  expect_false(soilKey:::b_colour_fao88(E(lv, munsell_hue_moist = c("10YR", "10YR", "5YR", "10YR", "5YR")),
                                         "chromic", "argic_b")$passed)
})

test_that("review fixes: the shared anthropic vocabulary, drainage ranges, the upper B, 0/0", {
  skip_on_cran()
  E <- soilKey:::.f88x_edit
  # 2. Cumulic Anthrosols from plain sediment terms or the silting test alone
  atc <- make_fao_example("ATc")
  for (lo in c("irrigation sediment", "irragric", "terric", "raised surface (man-made)"))
    expect_identical(.f385(E(atc, layer_origin = c(lo, lo, NA, NA)))$levels$code, c("AT", "ATc"), info = lo)
  expect_identical(.f385(E(atc, layer_origin = NA_character_, silting_sieve_test = c("positive", "positive", NA, NA)))$levels$code,
                   c("AT", "ATc"))
  x <- soilKey:::.f88_ctx(E(atc, layer_origin = c("mineral soil", "mineral fill", "mine spoil", NA)))
  expect_identical(soilKey:::.f88_anthropic(x)$value, c(FALSE, TRUE, TRUE, FALSE))
  # 3. drainage: both ends of a range; codes, Portuguese, undrained
  hs <- make_fao_example("HSf")
  dr <- function(d, cl) { p <- hs; p$site$drainage_class <- d; soilKey:::drainage_fao88(p, cl)$passed }
  expect_true(is.na(dr("imperfectly to very poorly drained", "very_poor")))
  expect_false(dr("imperfectly to very poorly drained", "well"))
  expect_true(dr("VP", "very_poor")); expect_false(dr("P", "very_poor")); expect_true(dr("W", "well"))
  expect_true(dr("muito mal drenado", "very_poor")); expect_true(dr("bem drenado", "well"))
  expect_true(dr("undrained", "very_poor")); expect_true(dr("not drained", "very_poor"))
  expect_true(is.na(dr("well to moderately well drained", "well")))
  p <- hs; p$site$drainage_class <- "imperfectly to very poorly drained"
  expect_true("units:HSf" %in% .f385(p)$provisional_because)
  # 4. an undecided upper B decides when the answers differ
  x <- soilKey:::.f88_ctx(make_fao_example("CMd"))
  testthat::local_mocked_bindings(.f88_b_units = function(x, kind) list(
    list(top = 20, layers = 2L, status = NA), list(top = 60, layers = 3L, status = TRUE)), .package = "soilKey")
  expect_true(is.na(soilKey:::.f88_b_eval(x, "cambic_b", 125, function(L) L[1] == 3L)$value))
  expect_true(soilKey:::.f88_b_eval(x, "cambic_b", 125, function(L) TRUE)$value)
  expect_false(soilKey:::.f88_b_eval(x, "cambic_b", 125, function(L) FALSE)$value)
})

test_that("review fixes: Gelic Leptosols near miss at 201 cm, Fe/C of 0/0 fails", {
  skip_on_cran()
  expect_true(soilKey:::permafrost_fao88(make_fao_example("LPi"), max_top_cm = 200)$passed)
  q <- make_fao_example("LPi", near_miss = TRUE)
  expect_false(soilKey:::permafrost_fao88(q, max_top_cm = 200)$passed)
  expect_identical(.f385(q)$levels$code[1], "LP")
  pz <- soilKey:::.f88x_edit(make_fao_example("PZf"), fe_dcb_pct = c(0.2, 0.1, 0, 0, 0.2), oc_pct = c(2, 0.4, 0, 0, 0.2))
  expect_false(soilKey:::spodic_fe_oc_fao88(pz)$passed)
})

test_that("the grouping level of the 56 examples of 0.9.384 is unchanged", {
  skip_on_cran()
  want <- c(HS = "GL", AT = "CM", LP = "RG", VR = "LV", FL = "RG", SC = "CM", GL = "CM",
            AN = "RG", AR = "RG", RG = "CL", PZ = "AR", PT = "AC", FR = "AC", PL = "LV",
            SN = "LV", GR = "PH", CH = "KS", KS = "PH", PH = "CM", PD = "LV", GY = "CM",
            CL = "CM", NT = "AC", AL = "AC", AC = "LX", LV = "AL", LX = "LV", CM = "RG")
  for (g in .GROUPS385) {
    expect_identical(.f385(make_fao_example(g))$levels$code[1], g, info = g)
    expect_identical(.f385(make_fao_example(g, near_miss = TRUE))$levels$code[1], want[[g]], info = g)
  }
})

test_that("i18n, thresholds, readings and the ASCII of the new R file", {
  skip_on_cran()
  p <- system.file("i18n", "translations.yaml", package = "soilKey")
  if (!nzchar(p)) p <- .src385("inst", "i18n", "translations.yaml")
  cat <- yaml::read_yaml(p)
  expect_identical(cat$en[["classify.level_units"]], "Soil unit")
  expect_identical(cat$pt[["classify.level_units"]], "Unidade de solo")
  rp <- system.file("i18n", "report_translations.yaml", package = "soilKey")
  if (!nzchar(rp)) rp <- .src385("inst", "i18n", "report_translations.yaml")
  rc <- yaml::read_yaml(rp)
  expect_identical(rc$en[["report.level_units"]], "Soil unit")
  expect_identical(rc$pt[["report.level_units"]], "Unidade de solo")
  withr::with_options(list(soilKey.report_lang = "pt"),
                      expect_identical(soilKey:::.report_level_label("units"), "Unidade de solo"))
  th <- yaml::read_yaml(system.file("rules", "fao1988", "thresholds.yaml", package = "soilKey"))
  expect_equal(th$unit_key$gelic$permafrost_within_cm, 200)
  expect_equal(th$unit_key$LP$lithic_within_cm, 10)
  expect_equal(th$unit_key$PZ$ferric_fe_oc_min, 6)
  expect_identical(eval(formals(soilKey:::lithic_fao88)$depth), 10)
  expect_identical(eval(formals(soilKey:::spodic_fe_oc_fao88)$min), 6)
  src <- .src385("data-raw", "fao_thresholds_1988.yaml")
  if (file.exists(src))
    expect_identical(readLines(src), readLines(system.file("rules", "fao1988", "thresholds.yaml", package = "soilKey")))
  rd <- yaml::read_yaml(system.file("rules", "fao1988", "readings.yaml", package = "soilKey"))$readings
  mine <- Filter(function(e) identical(e$version, "0.9.385"), rd)
  expect_true(length(mine) >= 12L)
  expect_true(all(vapply(mine, function(e) nzchar(e$page %||% ""), NA)))
  expect_true(all(vapply(mine, function(e) grepl("\\(Hugo, 2026-11-0[89]\\)", e$reading), NA)))
  f <- .src385("R", "key-fao-units.R")
  skip_if_not(file.exists(f))
  txt <- readLines(f, warn = FALSE)
  expect_false(any(grepl("[^\\x01-\\x7F]", txt, perl = TRUE)))
})
