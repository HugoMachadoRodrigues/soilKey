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
  rules <- load_rules("fao1988")
  for (cd in names(want)) {
    r <- .f384(make_fao_example(cd, near_miss = TRUE))
    expect_identical(r$levels$code, want[[cd]], info = cd)
    expect_false(identical(r$levels$code, cd))
    expect_length(r$provisional_because, 0L)
    # the gate of the grouping itself fails on the crossed number, decided,
    # evaluated directly (the key may stop before it)
    g <- Find(function(e) identical(e$code, cd), rules$major_groups)
    ev <- soilKey:::evaluate_rsg_tests(make_fao_example(cd, near_miss = TRUE), g$tests)
    expect_true(isFALSE(ev$passed), info = cd)
    # and passes, decided, on the example
    expect_true(isTRUE(soilKey:::evaluate_rsg_tests(make_fao_example(cd), g$tests)$passed), info = cd)
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
  r <- soilKey:::histosols_gate_fao88(mk(designation = c("H", "R"), top_cm = c(0, 25),
                                         bottom_cm = c(25, 50), oc_pct = c(30, NA)))
  expect_true(r$passed)
  expect_true("organic_layers_on_rock_thinner" %in% r$evidence$flags)
  # more than 20 cm, the minimum of the histic H (Hugo, 2026-11-09)
  expect_false(hs(mk(designation = c("H", "R"), top_cm = c(0, 20), bottom_cm = c(20, 50), oc_pct = c(30, NA))))
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
  expect_equal(cec(lv)$evidence$cec_per_kg_clay, c(30, 30, 30))
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
  expect_true(all(vapply(mine, function(e) grepl("(Hugo, 2026-11-09)", e$reading, fixed = TRUE), NA)))
})

test_that("review fixes: plinthite runs, cracks of one record, the Planosols trace, the NT example", {
  skip_on_cran()
  mkp <- function(h) {
    base <- list(ec_dS_m = 0.4, caso4_pct = 0, caco3_pct = 0, coarse_fragments_pct = 0,
                 munsell_hue_moist = "10YR", structure_grade = "moderate",
                 structure_type = "subangular blocky", boundary_distinctness = "clear",
                 ph_h2o = 5, na_cmol = 0.1)
    h <- utils::modifyList(base, h); n <- max(lengths(h))
    h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
    PedonRecord$new(site = list(id = "r384", drainage_class = "well drained", parent_material = "granite"),
                    horizons = ensure_horizon_schema(data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
  }
  # an albic E 10-60 over 20 cm of 35 % plinthite at 60-80, plinthite not
  # recorded at 80-100: the sure part of the run is element-wise TRUE
  pt <- mkp(list(designation = c("A", "E", "Bv1", "Bv2", "C"), top_cm = c(0, 10, 60, 80, 100),
                 bottom_cm = c(10, 60, 80, 100, 150), clay_pct = c(10, 8, 35, 38, 35),
                 silt_pct = c(20, 20, 20, 18, 20), sand_pct = c(70, 72, 45, 44, 45),
                 oc_pct = c(1, 0.3, 0.3, 0.2, 0.1), plinthite_pct = c(0, 0, 35, NA, 0),
                 munsell_value_moist = c(4, 7, 5, 5, 5), munsell_chroma_moist = c(2, 1, 6, 6, 6),
                 munsell_value_dry = c(5, 8, 6, 6, 6), munsell_chroma_dry = c(2, 1, 6, 6, 6)))
  g <- soilKey:::plinthosols_gate_fao88(pt)
  expect_true(g$passed)
  expect_true(g$evidence$within_125_under_e_stagnic_or_gleyic)
  # Vertisols: the 1 cm width and the 50 cm depth of the same crack
  vr <- make_fao_example("VR")
  vg <- function(p) soilKey:::vertisols_gate_fao88(p)$evidence$cracks
  expect_true(vg(vr))
  expect_false(vg(.edit384(vr, cracks_width_cm = c(2, 0.3, NA, NA), cracks_depth_cm = c(10, 70, NA, NA))))
  expect_true(is.na(vg(.edit384(vr, cracks_width_cm = NA_real_))))
  expect_true(is.na(vg(.edit384(vr, cracks_width_cm = c(2, NA, NA, NA), cracks_depth_cm = NA_real_))))
  expect_true(vg(.edit384(vr, cracks_width_cm = c(NA, 1.5, NA, NA), cracks_depth_cm = NA_real_)))
  # Planosols: what is missing comes from the readers left undecided
  pl <- make_fao_example("PL")
  q <- .edit384(pl, clay_pct = c(15, 12, NA, 30), rh_value = NA_real_)
  r <- soilKey:::planosols_gate_fao88(q)
  expect_true(is.na(r$passed))
  expect_true("clay_pct" %in% r$missing)
  expect_false("mottle_colors" %in% r$missing && !("mottle_colors" %in% soilKey:::.f88_stagnic_v(soilKey:::.f88_ctx(q))$missing))
  expect_length(soilKey:::planosols_gate_fao88(pl)$missing, 0L)
  # the NT example records only attributes soilKey reads
  expect_no_warning(nt <- make_fao_example("NT"))
  expect_true(soilKey:::nitic_properties_fao88(nt, max_top_cm = 125)$passed)
  expect_identical(.f384(nt)$levels$code, "NT")
})

test_that("Hugo's decisions of 2026-11-09 on the readings of this part", {
  skip_on_cran()
  # A. the uppermost diagnostic B decides: an argic B over a ferralic B is
  # no Ferralsol, a cambic B over an argic B no Luvisol
  fr <- .edit384(make_fao_example("FR"), designation = c("A", "Bt", "Bo2", "Bo3"),
                 clay_pct = c(20, 45, 58, 58), sand_pct = c(72, 48, 36, 36),
                 cec_cmol = c(8, 12, 5.5, 5), ca_cmol = c(1, 5, 0.4, 0.3), mg_cmol = c(0.4, 2, 0.2, 0.2),
                 exch_acidity_kcl_cmol = c(1, 3, 0.6, 0.5), water_dispersible_clay_pct = c(20, 20, 1, 1),
                 clay_films_amount = c(NA, "common", NA, NA), boundary_distinctness = c("clear", "clear", "diffuse", NA))
  expect_true(soilKey:::ferralic_b_fao88(fr)$passed)
  expect_true(soilKey:::argic_b_fao88(fr)$passed)
  expect_false(soilKey:::upper_b_fao88(fr, "ferralic_b")$passed)
  r <- .f384(fr)
  expect_false(isTRUE(r$trace$major_groups$FR$passed))
  expect_identical(r$levels$code, "AL")
  cm <- .edit384(make_fao_example("LV"), rows = c(1, 3, 4, 5), designation = c("A", "Bw", "Bt", "C"),
                 top_cm = c(0, 20, 60, 110), bottom_cm = c(20, 60, 110, 150), clay_pct = c(18, 20, 34, 30),
                 silt_pct = c(40, 40, 30, 30), sand_pct = c(42, 40, 36, 40), cec_cmol = c(10, 8, 10, 9),
                 bs_pct = 70, ph_h2o = 6.5, clay_films_amount = c(NA, NA, "common", NA),
                 munsell_hue_moist = c("10YR", "7.5YR", "7.5YR", "10YR"), munsell_chroma_moist = c(3, 6, 6, 4),
                 boundary_distinctness = c("clear", "clear", "gradual", NA))
  expect_true(soilKey:::cambic_b_fao88(cm)$passed)
  expect_true(soilKey:::argic_b_fao88(cm)$passed)
  expect_false(soilKey:::upper_b_fao88(cm, "argic_b")$passed)
  expect_identical(.f384(cm)$levels$code, "CM")
  # B. the CEC of the argic B per layer: 32, 20, 20 -> Lixisols
  lx <- .edit384(make_fao_example("LV"), cec_cmol = c(8, 6, 0.32 * 32, 0.20 * 34, 0.20 * 28))
  expect_false(soilKey:::argic_cec_clay_fao88(lx, min = 24)$passed)
  expect_true(soilKey:::argic_cec_clay_fao88(lx, lt = 24)$passed)
  expect_identical(.f384(lx)$levels$code, "LX")
  # D. a 45 cm peat without bulk density: 40 cm, flagged
  hs <- .edit384(make_fao_example("HS"), bulk_density_g_cm3 = NA_real_)
  g <- soilKey:::histosols_gate_fao88(hs)
  expect_true(g$passed)
  expect_true("bulk_density_not_measured_read_0.1_or_more" %in% g$evidence$flags)
  expect_false("bulk_density_not_measured_read_0.1_or_more" %in% soilKey:::histosols_gate_fao88(make_fao_example("HS"))$evidence$flags)
  # E. gleyic properties of the plinthite itself do not make the deep path
  pt <- make_fao_example("PT")
  deep <- .edit384(pt, top_cm = c(0, 60, 80, 110), bottom_cm = c(60, 80, 110, 150),
                   munsell_hue_moist = c("10YR", "5GY", "5GY", "5Y"), munsell_value_moist = c(4, 5, 5, 5),
                   munsell_chroma_moist = c(3, 1, 1, 1), mottle_colors = NA_character_,
                   redoximorphic_features_pct = 0)
  expect_false(soilKey:::plinthosols_gate_fao88(deep)$passed)
  above <- .edit384(deep, munsell_hue_moist = c("5GY", "5GY", "5GY", "5Y"), munsell_value_moist = c(5, 5, 5, 5),
                    munsell_chroma_moist = c(1, 1, 1, 1), designation = c("Ag", "Bv1", "Bv2", "C"))
  expect_true(soilKey:::plinthosols_gate_fao88(above)$passed)
  # F. andic depth from the mineral surface under an O
  an <- make_fao_example("AN")
  h <- as.data.frame(an$horizons)
  h <- rbind(h[1, , drop = FALSE], h)
  h$designation[1] <- "Oi"; h$oc_pct[1] <- 40; h$clay_pct[1] <- NA
  h$top_cm <- c(0, 10, 35, 70); h$bottom_cm <- c(10, 35, 70, 140)
  ano <- PedonRecord$new(site = an$site, horizons = ensure_horizon_schema(data.table::as.data.table(h)))
  a35 <- soilKey:::andic_to_35_fao88(ano)
  expect_true(a35$passed)
  expect_true("andic_depth_from_mineral_surface" %in% a35$evidence$flags)
  # G. Kastanozems: any layer of the upper 15 cm with chroma above 2
  ch <- .edit384(make_fao_example("CH"), rows = c(1, 1, 2, 3, 4), designation = c("Ah1", "Ah2", "Ah3", "Bk", "Ck"),
                 top_cm = c(0, 10, 25, 50, 90), bottom_cm = c(10, 25, 50, 90, 150),
                 munsell_chroma_moist = c(2, 3, 3, 3, 3))
  expect_false(soilKey:::mollic_chroma_fao88(ch, max = 2)$passed)
  expect_true(soilKey:::mollic_chroma_fao88(ch, gt = 2)$passed)
  expect_identical(.f384(ch)$levels$code, "KS")
  # H. Nitisols: only the A-to-B boundary; a clear boundary deeper does not matter
  nt <- make_fao_example("NT")
  expect_identical(.f384(.edit384(nt, boundary_distinctness = c("gradual", "gradual", "clear", "clear", NA)))$levels$code, "NT")
  expect_false(soilKey:::nitisols_gate_fao88(.edit384(nt, boundary_distinctness = c("gradual", "clear", "diffuse", "diffuse", NA)))$passed)
  # I. cracks with a depth but no width: undecided, the Vertisols provisional
  v2 <- .edit384(make_fao_example("VR"), cracks_width_cm = NA_real_)
  expect_true(is.na(soilKey:::vertisols_gate_fao88(v2)$passed))
  expect_true("major_groups:VR" %in% .f384(v2)$provisional_because)
})
