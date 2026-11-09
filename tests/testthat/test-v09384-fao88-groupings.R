# v0.9.384: the FAO-Unesco Revised Legend (1988) registered as fao1988,
# keyed to its 28 major soil groupings (classify_fao(), classify_fao1988()),
# with one example per grouping and one near miss per grouping
# (make_fao_example()), the fall-through of Q5, the i18n keys and the app
# examples.

.f384 <- function(p, ...) classify_fao(p, on_missing = "silent", ...)
.CODES384 <- c("HS", "AT", "LP", "VR", "FL", "SC", "GL", "AN", "AR", "RG", "PZ", "PT", "FR", "PL",
               "SN", "GR", "CH", "KS", "PH", "PD", "GY", "CL", "NT", "AL", "AC", "LV", "LX", "CM")
.NAMES384 <- c("Histosols", "Anthrosols", "Leptosols", "Vertisols", "Fluvisols", "Solonchaks",
               "Gleysols", "Andosols", "Arenosols", "Regosols", "Podzols", "Plinthosols",
               "Ferralsols", "Planosols", "Solonetz", "Greyzems", "Chernozems", "Kastanozems",
               "Phaeozems", "Podzoluvisols", "Gypsisols", "Calcisols", "Nitisols", "Alisols",
               "Acrisols", "Luvisols", "Lixisols", "Cambisols")
.src384 <- function(...) {
  f <- testthat::test_path("..", "..", ...)
  if (file.exists(f)) f else NA_character_
}
# a pedon with its horizons edited (rows subset with a comma)
.edit384 <- function(p, rows = NULL, ...) {
  h <- as.data.frame(p$horizons)
  if (!is.null(rows)) h <- h[rows, , drop = FALSE]
  v <- list(...)
  for (k in names(v)) h[[k]] <- v[[k]]
  PedonRecord$new(site = p$site, horizons = ensure_horizon_schema(data.table::as.data.table(h)))
}

test_that("the registry entry of fao1988", {
  skip_on_cran()
  s <- soilkey_systems()
  r <- s[s$id == "fao1988", ]
  expect_equal(nrow(r), 1L)
  expect_identical(r$classifier, "classify_fao1988")
  sys <- soilKey:::.system_get("fao1988")
  expect_equal(sys$order, 16)
  expect_identical(sys$status, "experimental")
  expect_false(sys$default)
  expect_false(sys$legacy)
  expect_identical(sys$namespace, "fao88")
  expect_identical(sys$levels, "major_groups")
  expect_identical(sys$top_level, "major_groups")
  for (a in c("fao", "fao88", "revised_legend", "fao1988"))
    expect_identical(soilKey:::.system_resolve(a), "fao1988")
  expect_false("fao1988" %in% soilKey:::.system_ids(default_only = TRUE))
  # classify_all() reaches it by its alias, and only when asked
  p <- make_fao_example("LV")
  ca <- classify_all(p, systems = c("wrb", "fao"), on_missing = "silent")
  expect_identical(ca$fao1988$name, "Luvisols")
  expect_false("fao1988" %in% names(classify_all(p, on_missing = "silent")))
})

test_that("the key: 28 major soil groupings in the printed key order, every predicate defined", {
  skip_on_cran()
  k <- load_rules("fao1988")
  expect_identical(vapply(k$major_groups, `[[`, "", "code"), .CODES384)
  expect_identical(vapply(k$major_groups, `[[`, "", "name"), .NAMES384)
  expect_identical(soilKey:::.F88_GROUPINGS$code, .CODES384)
  expect_identical(soilKey:::.F88_GROUPINGS$name, .NAMES384)
  # no residual grouping (Q5), every grouping with a gloss and a page
  expect_false(any(vapply(k$major_groups, function(e) isTRUE(e$tests$default), NA)))
  expect_true(all(vapply(k$major_groups, function(e) nzchar(e$name_pt %||% ""), NA)))
  expect_identical(vapply(k$major_groups, function(e) as.integer(e$page), 0L),
                   c(75L, 75L, 76L, 77L, 77L, 78L, 78L, 79L, 80L, 80L, 81L, 81L, 82L, 82L,
                     83L, 83L, 84L, 84L, 84L, 85L, 85L, 86L, 86L, 87L, 87L, 88L, 88L, 89L))
  res <- soilKey:::.validate_rules("fao1988")
  expect_equal(res$missing, character(0))
  # the order of the key, not that of the lists of chapters III and VI
  csv <- .src384("data-raw", "fao1988_units.csv")
  if (!is.na(csv)) {
    u <- utils::read.csv(csv, stringsAsFactors = FALSE)
    g <- u[u$level == "grouping", ]
    g <- g[order(as.numeric(g$key_position)), ]
    expect_identical(g$code, .CODES384)
    expect_identical(tools::toTitleCase(tolower(g$name)), .NAMES384)
    expect_equal(sum(u$level == "unit"), 153L)
  }
})

test_that("every example keys to its own grouping, not provisionally", {
  skip_on_cran()
  for (cd in .CODES384) {
    r <- .f384(make_fao_example(cd))
    expect_identical(r$levels$code, cd, info = cd)
    expect_identical(r$name, .NAMES384[match(cd, .CODES384)], info = cd)
    expect_identical(r$rsg_or_order, r$name)
    expect_length(r$provisional_because, 0L)
    expect_identical(r$system_id, "fao1988")
    expect_identical(r$system, "FAO 1988")
    expect_true(nzchar(r$levels$name_pt))
    expect_identical(r$levels$page, as.character(soilKey:::.F88_GROUPINGS$page[match(cd, .CODES384)]))
  }
  # the wrapper is the same classification
  p <- make_fao_example("CM")
  expect_identical(classify_fao1988(p, on_missing = "silent")$name, .f384(p)$name)
  expect_identical(classify_fao(p, edition = "88", on_missing = "silent")$name, "Cambisols")
})

test_that("the 28 near misses land where the one change sends them", {
  skip_on_cran()
  want <- c(HS = "GL", AT = "CM", LP = "RG", VR = "LV", FL = "RG", SC = "CM", GL = "CM",
            AN = "RG", AR = "RG", RG = "CL", PZ = "AR", PT = "AC", FR = "AC", PL = "LV",
            SN = "LV", GR = "PH", CH = "KS", KS = "PH", PH = "CM", PD = "LV", GY = "CM",
            CL = "CM", NT = "AC", AL = "AC", AC = "LX", LV = "AL", LX = "LV", CM = "RG")
  expect_identical(names(want), .CODES384)
  for (cd in names(want)) {
    r <- .f384(make_fao_example(cd, near_miss = TRUE))
    expect_identical(r$levels$code, want[[cd]], info = cd)
    expect_false(identical(r$levels$code, cd))
    expect_length(r$provisional_because, 0L)
    # the gate of the grouping fails on the crossed number, decided (when
    # the soil lands after it; before it, the gate is not reached)
    g <- r$trace$major_groups[[cd]]
    if (!is.null(g)) expect_true(isFALSE(g$passed), info = cd)
  }
})

test_that("an undecided gate before the assigned one makes the result provisional", {
  skip_on_cran()
  # no conductivity: the salic properties, and the Solonchaks gate, undecided
  p <- .edit384(make_fao_example("CM"), ec_dS_m = NA_real_)
  r <- .f384(p)
  expect_identical(r$name, "Cambisols")
  expect_true("major_groups:SC" %in% r$provisional_because)
  expect_true(is.na(r$trace$major_groups$SC$passed))
  expect_true("ec_dS_m" %in% r$trace$major_groups$SC$missing)
  expect_true(any(grepl("^Provisional", r$warnings)))
  # a gate that fails on another criterion is decided all the same
  r2 <- .f384(.edit384(make_fao_example("LV"), ec_dS_m = NA_real_))
  expect_identical(r2$name, "Luvisols")
  expect_false("major_groups:SC" %in% r2$provisional_because)
  # the 1988 names stop at the grouping
  expect_identical(r$levels$level, "major_groups")
})

test_that("no residual grouping: the fall-through of Q5", {
  skip_on_cran()
  # a mollic A directly over C with a base saturation below 50 % in the C
  p <- .edit384(make_fao_example("PH"), rows = c(1, 2, 4),
                designation = c("A1", "A2", "C"), top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 140),
                bs_pct = c(70, 65, 30), ph_h2o = c(6, 6, 5))
  r <- .f384(p)
  expect_identical(r$name, "[Not classified in the FAO-Unesco Revised Legend (1988)]")
  expect_true(is.na(r$rsg_or_order))
  expect_equal(nrow(r$levels), 0L)
  expect_length(r$provisional_because, 0L)
  expect_true(all(vapply(r$trace$major_groups, function(e) isFALSE(e$passed), NA)))
  expect_identical(names(r$trace$major_groups), .CODES384)
  w <- r$warnings[grepl("^Not classified", r$warnings)]
  expect_length(w, 1L)
  expect_match(w, "none of the 28 gates", fixed = TRUE)
  expect_match(w, "Q5", fixed = TRUE)
  expect_false(any(grepl("^No class of the major_groups key", r$warnings)))
  # the same soil with the base saturation of the C not measured: the
  # Phaeozems undecided, named as the candidate
  q <- .edit384(p, bs_pct = c(70, 65, NA))
  r2 <- .f384(q)
  expect_identical(r2$name, "[Undecided in the FAO-Unesco Revised Legend (1988): PH Phaeozems, or not classified]")
  expect_identical(r2$provisional_because, "major_groups:PH")
  expect_true(any(grepl("leave PH undecided", r2$warnings, fixed = TRUE)))
  expect_false(any(grepl("^Provisional: [0-9]", r2$warnings)))
})

test_that("the edition switch: 1974 is not yet available", {
  skip_on_cran()
  p <- make_fao_example("LV")
  expect_error(classify_fao(p, edition = "1974"), "not yet available")
  expect_error(classify_fao(p, edition = "1999"))
  expect_error(make_fao_example("LV", edition = "1974"), "1974")
  expect_identical(classify_fao(p, on_missing = "silent")$system_id, "fao1988")
})

test_that("the gates read their printed numbers", {
  skip_on_cran()
  mk <- function(...) {
    h <- data.frame(..., stringsAsFactors = FALSE)
    PedonRecord$new(site = list(id = "t384", drainage_class = "very poorly drained"),
                    horizons = ensure_horizon_schema(data.table::as.data.table(h)))
  }
  hs <- function(p) soilKey:::histosols_gate_fao88(p)$passed
  # 40 cm of peat from the surface; 35 cm fails; 45 cm of light sphagnum peat fails (60)
  expect_true(hs(mk(designation = c("H", "C"), top_cm = c(0, 40), bottom_cm = c(40, 100),
                    oc_pct = c(30, 1), clay_pct = c(NA, 20))))
  expect_false(hs(mk(designation = c("H", "C"), top_cm = c(0, 35), bottom_cm = c(35, 100),
                     oc_pct = c(30, 1), clay_pct = c(NA, 20))))
  expect_false(hs(mk(designation = c("H", "C"), top_cm = c(0, 45), bottom_cm = c(45, 100),
                     oc_pct = c(30, 1), clay_pct = c(NA, 20), sphagnum_fibres_pct = c(90, NA))))
  # cumulative within 80 cm: 25 + 20 cm of peat
  expect_true(hs(mk(designation = c("H1", "C1", "H2", "C2"), top_cm = c(0, 25, 50, 70),
                    bottom_cm = c(25, 50, 70, 100), oc_pct = c(30, 1, 30, 1), clay_pct = c(NA, 20, NA, 20))))
  # thinner on rock
  r <- soilKey:::histosols_gate_fao88(mk(designation = c("H", "R"), top_cm = c(0, 15),
                                         bottom_cm = c(15, 50), oc_pct = c(30, NA)))
  expect_true(r$passed)
  expect_true("organic_layers_on_rock_thinner" %in% r$evidence$flags)
  # Leptosols: rock at 30 cm passes, at 31 fails; fine earth below 20 % over 75 cm
  lp <- function(p) soilKey:::leptosols_gate_fao88(p)$passed
  rk <- function(d) mk(designation = c("A", "C", "R"), top_cm = c(0, 10, d), bottom_cm = c(10, d, 80),
                       oc_pct = c(2, 0.5, NA), clay_pct = c(20, 20, NA), coarse_fragments_pct = c(0, 0, NA),
                       caco3_pct = c(0, 0, NA))
  expect_true(lp(rk(30)))
  expect_false(lp(rk(31)))
  fe <- function(cf) mk(designation = c("A", "C"), top_cm = c(0, 10), bottom_cm = c(10, 100),
                        oc_pct = c(2, 0.5), clay_pct = c(20, 20), coarse_fragments_pct = c(cf, cf),
                        caco3_pct = c(0, 0))
  expect_true(lp(fe(81)))
  expect_false(lp(fe(80)))
  # Vertisols: 30 % clay after mixing the upper 18 cm
  vr <- make_fao_example("VR")
  expect_true(soilKey:::vertisols_gate_fao88(vr)$passed)
  expect_false(soilKey:::vertisols_gate_fao88(.edit384(vr, clay_pct = c(29, 29, 52, 48)))$passed)
  # cracks not recorded are absent; an irrigated site waives them
  nc <- .edit384(vr, cracks_width_cm = NA_real_, cracks_depth_cm = NA_real_)
  expect_false(soilKey:::vertisols_gate_fao88(nc)$passed)
  nc$site$irrigated <- TRUE
  expect_true(soilKey:::vertisols_gate_fao88(nc)$passed)
  # the CEC of the argic B is one thickness-weighted value: 24 is the line
  lv <- make_fao_example("LV")
  cec <- function(p) soilKey:::argic_cec_clay_fao88(p, min = 24)
  expect_equal(cec(lv)$evidence$cec_per_kg_clay, 30)
  expect_true(cec(.edit384(lv, cec_cmol = c(8, 6, 0.24 * 32, 0.24 * 34, 0.24 * 28)))$passed)
  expect_false(cec(.edit384(lv, cec_cmol = c(8, 6, 0.23 * 32, 0.23 * 34, 0.23 * 28)))$passed)
  expect_true(is.na(cec(.edit384(lv, cec_cmol = c(8, 6, NA, 0.30 * 34, 9)))$passed))
  # base saturation of the argic B: below 50 in one layer / 50 or more in all
  bs <- .edit384(lv, bs_pct = c(60, 60, 60, 49, 49))
  expect_true(soilKey:::argic_bs_fao88(bs, lt = 50)$passed)
  expect_false(soilKey:::argic_bs_fao88(bs, min = 50)$passed)
  # the chroma of the mollic A to 15 cm
  ch <- make_fao_example("CH")
  expect_true(soilKey:::mollic_chroma_fao88(ch, max = 2)$passed)
  expect_false(soilKey:::mollic_chroma_fao88(ch, gt = 2)$passed)
  # Arenosols: coarser than sandy loam to 100 cm; not on rock above it
  ar <- make_fao_example("AR")
  expect_true(soilKey:::coarse_texture_fao88(ar)$passed)
  expect_false(soilKey:::coarse_texture_fao88(.edit384(ar, designation = c("A", "C1", "R")))$passed)
  expect_false(soilKey:::coarse_fragments_fao88(.edit384(ar, coarse_fragments_pct = c(0, 35, 0)))$passed)
  expect_true(soilKey:::coarse_fragments_fao88(.edit384(ar, coarse_fragments_pct = c(0, 34, 0)))$passed)
})

test_that("no diagnostic horizons other than: present, undecided, and the argic B of the Calcisols", {
  skip_on_cran()
  only <- function(p, a) soilKey:::only_horizons_fao88(p, allowed = a)
  rg <- make_fao_example("RG")
  expect_true(only(rg, c("ochric_a", "umbric_a"))$passed)
  cm <- make_fao_example("CM")
  r <- only(cm, c("ochric_a", "umbric_a"))
  expect_false(r$passed)
  expect_true("cambic_b" %in% r$evidence$present)
  expect_true(only(cm, c("a_horizon", "cambic_b"))$passed)
  # an argic B allowed only when every layer is calcareous
  lv <- make_fao_example("LV")
  allowed <- c("ochric_a", "cambic_b", "argic_b_calcareous", "calcic", "petrocalcic", "albic_e")
  expect_false(only(lv, allowed)$passed)
  calc <- .edit384(lv, caco3_pct = c(0, 0, 5, 5, 5))
  expect_true(only(calc, allowed)$passed)
  part <- .edit384(lv, caco3_pct = c(0, 0, 5, 0, 5))
  expect_false(only(part, allowed)$passed)
})

test_that("the i18n keys and the app examples of the Revised Legend", {
  skip_on_cran()
  p <- system.file("i18n", "translations.yaml", package = "soilKey")
  if (!nzchar(p)) p <- testthat::test_path("..", "..", "inst", "i18n", "translations.yaml")
  cat <- yaml::read_yaml(p)
  for (l in c("en", "pt")) {
    for (k in c("classify.sys_fao1988", "classify.edition_fao1988", "classify.level_major_groups"))
      expect_true(nzchar(cat[[l]][[k]] %||% ""), info = paste(l, k))
  }
  expect_identical(cat$en[["classify.edition_fao1988"]], soilKey:::.system_get("fao1988")$edition)
  rp <- system.file("i18n", "report_translations.yaml", package = "soilKey")
  if (!nzchar(rp)) rp <- testthat::test_path("..", "..", "inst", "i18n", "report_translations.yaml")
  rc <- yaml::read_yaml(rp)
  expect_identical(rc$en[["report.level_major_groups"]], "Major soil grouping")
  expect_identical(rc$pt[["report.level_major_groups"]], "Grupamento principal de solos")
  withr::with_options(list(soilKey.report_lang = "pt"),
                      expect_identical(soilKey:::.report_level_label("major_groups"),
                                       "Grupamento principal de solos"))
  # the app: one example per grouping, each keying to it
  for (pk in c("shiny", "bslib", "shinyWidgets", "DT", "plotly")) testthat::skip_if_not_installed(pk)
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d)) d <- testthat::test_path("..", "..", "inst", "shiny", "classify_app_pro")
  env <- new.env(parent = globalenv())
  for (f in list.files(file.path(d, "R"), pattern = "[.]R$", full.names = TRUE)) sys.source(f, envir = env)
  env$.sk_i18n_env$cat <- cat
  expect_true("fao1988" %in% env$.sk_national_systems()$id)
  grp <- env$pro_fixture_catalog()[[env$.sk_system_label("fao1988")]]
  expect_equal(length(grp), 28L)
  expect_identical(unname(names(grp)), .NAMES384)
  for (k in seq_along(grp)) {
    expect_identical(env$pro_fixture_system(grp[[k]]), "fao1988")
    q <- env$pro_load_fixture(grp[[k]])
    expect_true(inherits(q, "PedonRecord"))
    expect_identical(classify_fao1988(q, on_missing = "silent")$levels$name[1], names(grp)[k])
  }
})

test_that("the rule folder: thresholds and readings", {
  skip_on_cran()
  th <- system.file("rules", "fao1988", "thresholds.yaml", package = "soilKey")
  expect_true(nzchar(th))
  t <- yaml::read_yaml(th)
  expect_equal(t$key$LV$cec_clay_min, 24)
  expect_equal(t$key$HS$h_min_cm, 40)
  expect_equal(t$key$LP$fine_earth_lt_pct, 20)
  src <- .src384("data-raw", "fao_thresholds_1988.yaml")
  if (!is.na(src)) expect_identical(readLines(src), readLines(th))
  rd <- yaml::read_yaml(system.file("rules", "fao1988", "readings.yaml", package = "soilKey"))$readings
  expect_true(length(rd) > 50L)
  expect_true(all(vapply(rd, function(e) "1988" %in% as.character(e$edition), NA)))
  expect_true(all(vapply(rd, function(e) nzchar(e$page %||% ""), NA)))
  mine <- Filter(function(e) identical(e$version, "0.9.384"), rd)
  expect_true(length(mine) >= 15L)
  expect_true(all(vapply(mine, function(e) grepl("for Hugo to confirm", e$reading, fixed = TRUE), NA)))
})
