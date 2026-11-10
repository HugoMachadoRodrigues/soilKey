# v0.9.358: the South African soil classification of 1991, part 4: the
# registry, the 73 soil forms (key.yaml, in the order of van Huyssteen 2020)
# with their closure lists, the profile view (1500 mm, burial, truncation),
# the 500 mm gates of the form pages, classify_za1991() and
# make_za1991_example(): every example keys to its own form by the data and
# by the notation.

.za_L <- function(top, bottom, ...) {
  d <- list(top_cm = top, bottom_cm = bottom, munsell_hue_moist = "10YR", munsell_value_moist = 5,
            munsell_chroma_moist = 4, munsell_hue_dry = "10YR", munsell_value_dry = 6,
            munsell_chroma_dry = 4, structure_type = "massive", structure_grade = "structureless",
            clay_pct = 20, silt_pct = 20, sand_pct = 60, oc_pct = 0.3, effervescence_class = "none",
            clay_films_amount = "none")
  a <- list(...)
  d[names(a)] <- a
  d
}
.za_P <- function(...) {
  ls <- list(...)
  nm <- unique(unlist(lapply(ls, names)))
  h <- as.data.frame(lapply(stats::setNames(nm, nm), function(k)
    unlist(lapply(ls, function(l) if (is.null(l[[k]])) NA else l[[k]]))), stringsAsFactors = FALSE)
  old <- options(soilKey.warn_unrecognised = FALSE)
  on.exit(options(old))
  PedonRecord$new(site = list(id = "za", lat = -29, lon = 30), horizons = h)
}
.za_A <- function(...) .za_L(0, 25, designation = "A", structure_type = "subangular blocky",
                             structure_grade = "weak", oc_pct = 1, munsell_value_moist = 4,
                             munsell_chroma_moist = 3, munsell_value_dry = 5, munsell_chroma_dry = 3,
                             clay_pct = 15, ca_cmol = 1, mg_cmol = 0.5, k_cmol = 0.2, na_cmol = 0.1, ...)
.rab <- function(top, bottom, ...) .za_L(top, bottom, designation = "B", munsell_hue_moist = "2.5YR",
                                         munsell_value_moist = 3, munsell_chroma_moist = 6, munsell_hue_dry = "2.5YR",
                                         munsell_value_dry = 4, munsell_chroma_dry = 6, clay_pct = 25, ...)
.yab <- function(top, bottom, ...) .za_L(top, bottom, designation = "B", munsell_value_moist = 5,
                                         munsell_chroma_moist = 6, munsell_value_dry = 6, munsell_chroma_dry = 6,
                                         clay_pct = 25, ...)
# v0.9.359: the name now carries the family (Q23); these tests read the form
.form <- function(p) {
  r <- classify_za1991(p, on_missing = "silent")
  if (nrow(r$levels)) r$name <- r$levels$name[1L]
  r
}


test_that("the registry has the South African system of 1991, experimental and opt-in", {
  skip_on_cran()
  s <- soilkey_systems()
  r <- s[s$id == "za1991", ]
  expect_identical(nrow(r), 1L)
  expect_identical(r$classifier, "classify_za1991")
  expect_identical(r$levels, "forms, families")
  expect_identical(r$aliases, "za, sat, sat1991, south_africa")
  expect_identical(r$status, "experimental")
  expect_false(r$default)
  sys <- soilKey:::.system_get("za1991")
  expect_identical(sys$namespace, "za")
  for (a in c("za", "sat", "sat1991", "south_africa")) expect_identical(soilKey:::.system_resolve(a), "za1991")
  expect_false("za1991" %in% soilKey:::.system_ids(default_only = TRUE))
  res <- soilKey:::.validate_rules("za1991")
  expect_identical(res$missing, character(0))
  expect_identical(res$unsuffixed, character(0))
  expect_identical(res$codes, character(0))
  # identify_horizons() dispatches to .za_identify()
  f <- identify_horizons(make_za1991_example("hutton"), "za1991")
  expect_identical(f$code, c("OA", "RAB"))
})

test_that("key.yaml: 73 forms and Lichtenburg, in the order of V, with closure lists", {
  skip_on_cran()
  fs <- yaml::read_yaml(system.file("rules", "za1991", "key.yaml", package = "soilKey"))$forms
  expect_identical(length(fs), 74L)
  codes <- vapply(fs, `[[`, "", "code")
  expect_false(anyDuplicated(codes) > 0L)
  expect_identical(sum(!vapply(fs, function(f) isTRUE(f$outside_1991_key), NA)), 73L)
  expect_identical(codes[c(1, 47, 74)], c("Ch", "Hu", "Wb"))
  expect_true(all(c("[Ia]", "[Py]", "No") %in% codes))
  seqs <- vapply(fs, `[[`, "", "sequence")
  expect_false(anyDuplicated(seqs) > 0L)
  toks <- soilKey:::.za_tokens()
  expect_true(all(unlist(strsplit(seqs, "-")) %in% toks))
  by <- stats::setNames(fs, codes)
  expect_setequal(unlist(by$Cv$closed_on), c("SPL", "HPL", "USW", "RAB", "SCH", "HCH"))
  expect_setequal(unlist(by$Hu$closed_on), c("SPL", "HPL", "USW", "SCH", "HCH", "DOR"))
  expect_setequal(unlist(by$Ik$closed_on), c("G", "PCU", "SCH", "HCH", "LCU", "HRK"))
  expect_identical(length(by$Fw$closed_on), 25L)
  expect_identical(unlist(by$Ch$closed_on), NULL)
  # the glosses in the catalogue, en and pt
  tr <- yaml::read_yaml(system.file("i18n", "translations.yaml", package = "soilKey"))
  for (k in codes) {
    expect_true(nzchar(tr$en[[paste0("za1991.form_", k)]] %||% ""), info = k)
    expect_true(nzchar(tr$pt[[paste0("za1991.form_", k)]] %||% ""), info = k)
  }
  expect_identical(tr$en[["classify.sys_za1991"]], "South Africa (SA 1991)")
  expect_identical(tr$pt[["classify.level_forms"]], "Forma")
})

test_that("each of the 73 examples keys to its own form, by the data and by the notation", {
  skip_on_cran()
  fs <- soilKey:::.za_forms()
  fs <- fs[!vapply(fs, function(f) f$outside, NA)]
  expect_identical(length(fs), 73L)
  for (f in fs) {
    k <- tolower(f$name)
    r1 <- .form(make_za1991_example(k))
    expect_identical(r1$name, f$name, info = k)
    expect_identical(r1$levels$code[1L], f$code, info = k)
    expect_identical(r1$provisional_because, character(0), info = k)
    p2 <- make_za1991_example(k, notation = TRUE)
    expect_true("diagnostic_za" %in% names(p2$horizons), info = k)
    r2 <- .form(p2)
    expect_identical(r2$name, f$name, info = k)
    fr <- identify_horizons(p2, "za1991")
    expect_true(all(fr$route[!is.na(fr$code)] %in% c("notation", "both")), info = k)
  }
})

test_that("the closure: Clovelly over hard rock is Clovelly, over soft plinthic B Avalon", {
  skip_on_cran()
  ex <- make_za1991_example("clovelly")
  h <- ex$horizons
  p1 <- .za_P(.za_A(), .yab(25, 90), .za_L(90, 110, designation = "R"))
  expect_identical(.form(p1)$name, "Clovelly")
  spl <- .za_L(90, 150, designation = "B", munsell_chroma_moist = 6, munsell_value_dry = 7, munsell_chroma_dry = 6,
               redoximorphic_features_pct = 25, mottle_colors = "10YR 6/1; 7.5YR 5/8")
  expect_identical(.form(.za_P(.za_A(), .yab(25, 90), spl))$name, "Avalon")
  # Hutton over hard rock; over a hard plinthic B, Lichtenburg, provisional (Q1)
  expect_identical(.form(.za_P(.za_A(), .rab(25, 90), .za_L(90, 110, designation = "R")))$name, "Hutton")
  lc <- .form(make_za1991_example("lichtenburg"))
  expect_identical(lc$name, "Lichtenburg")
  expect_identical(lc$provisional_because, "forms:Lc")
  expect_true(any(grepl("Q1", lc$warnings, fixed = TRUE)))
  # an E between the orthic A and a red apedal B is no Hutton
  e <- .za_L(25, 50, designation = "E", munsell_value_dry = 7, munsell_chroma_dry = 2, munsell_chroma_moist = 2, clay_pct = 8)
  r <- .form(.za_P(.za_A(), e, .rab(50, 150)))
  expect_true(is.na(r$name))
  expect_true(any(grepl("No form of 1991", r$warnings)))
  expect_true(is.data.frame(r$qualifiers$za_nearest))
})

test_that("a description that stops above 1500 mm: the shorter form provisional", {
  skip_on_cran()
  r <- .form(.za_P(.za_A(), .rab(25, 90)))
  expect_identical(r$name, "Hutton")
  expect_true(all(c("forms:Bv", "forms:Ky", "forms:Gr") %in% r$provisional_because))
  expect_true(any(grepl("stops at 90 cm", r$warnings)))
  # Valsrivier: the material below the pedocutanic B not described
  pcu <- .za_L(25, 70, designation = "Bt", structure_type = "angular blocky", structure_grade = "moderate",
               clay_films_amount = "many", clay_pct = 35)
  r2 <- .form(.za_P(.za_A(), pcu))
  expect_true(is.na(r2$name))
  expect_true(all(c("forms:Se", "forms:Va", "forms:Sw") %in% r2$provisional_because))
  # the B reaches 1500 mm: the material below read as unconsolidated (Q4)
  pcu2 <- pcu; pcu2$bottom_cm <- 160
  r3 <- .form(.za_P(.za_A(), pcu2))
  expect_identical(r3$name, "Valsrivier")
  expect_true(any(grepl("reaches 1500 mm", r3$warnings)))
  # hard rock directly under the pedocutanic B: Swartland (Q4)
  r4 <- .form(.za_P(.za_A(), pcu, .za_L(70, 90, designation = "R")))
  expect_identical(r4$name, "Swartland")
})

test_that("the 500 mm rules of the form pages", {
  skip_on_cran()
  pcu <- .za_L(25, 70, designation = "Bt", structure_type = "angular blocky", structure_grade = "moderate",
               clay_films_amount = "many", clay_pct = 35)
  sap <- function(top) .za_L(top, top + 40, designation = "C", weathering_stage = "saprolite")
  # saprolite within 500 mm of the B: Swartland
  expect_identical(.form(.za_P(.za_A(), pcu, sap(70)))$name, "Swartland")
  # 600 mm of unconsolidated material, then saprolite: Valsrivier
  expect_identical(.form(.za_P(.za_A(), pcu, .za_L(70, 130, designation = "C"), sap(130)))$name, "Valsrivier")
  # 300 mm of it, then saprolite: Swartland (the saprolite within 500 mm, D1)
  r <- .form(.za_P(.za_A(), pcu, .za_L(70, 100, designation = "C"), sap(100)))
  expect_identical(r$name, "Swartland")
  # Witbank over a buried soil needs more than 500 mm of A and deposit
  mmd <- function(b) .za_L(25, b, designation = "C", anthropic_material = "earthy")
  expect_identical(.form(.za_P(.za_A(), mmd(150)))$name, "Witbank")
  r2 <- .form(.za_P(.za_A(), mmd(40), .rab(40, 150, designation = "2Bb")))
  expect_false(identical(r2$name, "Witbank"))
})

test_that("the general rules: 1500 mm, burial, truncation, the land class", {
  skip_on_cran()
  # 1500 mm: a soft plinthic B starting at 149 cm counts, at 150 cm it does not
  spl <- function(top) .za_L(top, top + 30, designation = "B", munsell_chroma_moist = 6, munsell_value_dry = 7,
                             munsell_chroma_dry = 6, redoximorphic_features_pct = 25, mottle_colors = "10YR 6/1; 7.5YR 5/8")
  expect_identical(.form(.za_P(.za_A(), .rab(25, 149), spl(149)))$name, "Bainsvlei")
  expect_identical(.form(.za_P(.za_A(), .rab(25, 150), spl(150)))$name, "Hutton")
  # burial: 490 mm of aeolian sand over a Hutton classifies the buried soil;
  # 510 mm classifies the sand (Namib)
  sand <- function(b) .za_L(0, b, designation = "C", sand_pct = 92, silt_pct = 5, clay_pct = 3, layer_origin = "aeolic",
                            munsell_hue_moist = "7.5YR", munsell_value_moist = 5, munsell_chroma_moist = 6,
                            munsell_hue_dry = "7.5YR", munsell_value_dry = 6, munsell_chroma_dry = 6)
  buried <- function(b) .za_P(sand(b), .za_A(top_cm = b, bottom_cm = b + 20, designation = "2Ab"),
                              .rab(b + 20, 170, designation = "2Bb"))
  r49 <- .form(buried(49))
  expect_identical(r49$name, "Hutton")
  expect_true(any(grepl("buried soil is classified", r49$warnings)))
  r51 <- .form(buried(51))
  expect_true(any(grepl("the deposit is classified", r51$warnings)))
  expect_false(identical(r51$name, "Hutton"))
  # R4: a thin deposit over rock is classified
  thin <- .za_P(.za_A(layer_origin = "aeolic", bottom_cm = 20), .za_L(20, 40, designation = "R"))
  r4 <- .form(thin)
  expect_identical(r4$name, "Mispah")
  expect_true(any(grepl("R4", r4$warnings, fixed = TRUE)))
  # the special examples
  b <- .form(make_za1991_example("buried"))
  expect_identical(b$name, "Valsrivier")
  t <- .form(make_za1991_example("truncated"))
  expect_identical(t$name, "Garies")
  expect_true(t$qualifiers$za_truncated)
  expect_true(any(grepl("truncated", t$warnings)))
  l <- .form(make_za1991_example("lithosol"))
  expect_identical(l$qualifiers$za_land_class, "rock and lithosols")
  expect_identical(nrow(l$levels), 0L)
})

test_that("provisional and undecided results", {
  skip_on_cran()
  u <- .form(make_za1991_example("undecided"))
  expect_true(is.na(u$name))
  expect_setequal(u$provisional_because, c("forms:Cv", "forms:Hu"))
  expect_true("munsell_hue_moist" %in% u$missing_data)
  # the Kroonstad and Estcourt switch at 500 mm under the E
  mk <- function(eb) .za_P(.za_A(bottom_cm = 20, boundary_distinctness = "abrupt"),
                           .za_L(20, eb, designation = "E", munsell_value_dry = 7, munsell_chroma_dry = 2,
                                 munsell_chroma_moist = 2, clay_pct = 8, boundary_distinctness = "abrupt"),
                           .za_L(eb, 150, designation = "Btg", structure_type = "prismatic", structure_grade = "strong",
                                 munsell_value_moist = 5, munsell_chroma_moist = 1, munsell_value_dry = 6,
                                 munsell_chroma_dry = 1, clay_pct = 45, coatings_value_moist = 2))
  expect_identical(.form(mk(70))$name, "Kroonstad")
  expect_identical(.form(mk(40))$name, "Estcourt")
  r <- .form(mk(50))
  expect_true(is.na(r$name))
  expect_setequal(r$provisional_because, c("forms:Kd", "forms:Es"))
  # stand-ins named in the warnings
  expect_true(any(grepl("stand-ins for numbers of the 1991 book", .form(make_za1991_example("lamotte"))$warnings)))
})

test_that("classify_all runs it only when asked", {
  skip_on_cran()
  p <- make_za1991_example("hutton")
  a <- classify_all(p, systems = c("wrb2022", "za1991"), on_missing = "silent")
  expect_true("za1991" %in% names(a))
  expect_identical(a$za1991$rsg_or_order, "Hutton")
  expect_false("za1991" %in% names(classify_all(p, on_missing = "silent")))
})


# --- the review of 2026-11-06: the repros of D1-D9 and R1-R9 ----------------------

.L0 <- function(top, bottom, ...) { d <- list(top_cm = top, bottom_cm = bottom); a <- list(...); d[names(a)] <- a; d }
.E <- function(t, b, ...) .za_L(t, b, designation = "E", munsell_value_moist = 5, munsell_chroma_moist = 2,
                                munsell_value_dry = 7, munsell_chroma_dry = 2, clay_pct = 8, ...)
.G <- function(t, b, ...) .za_L(t, b, designation = "G", munsell_value_moist = 5, munsell_chroma_moist = 1,
                                munsell_value_dry = 6, munsell_chroma_dry = 1, structure_type = "angular blocky",
                                structure_grade = "moderate", clay_pct = 40, ...)
.pcu <- function(t, b, ...) .za_L(t, b, designation = "Bt", munsell_value_moist = 4, munsell_chroma_moist = 4,
                                  structure_type = "angular blocky", structure_grade = "moderate",
                                  clay_films_amount = "many", clay_pct = 35, ...)
.sap <- function(t, b, ...) .za_L(t, b, designation = "C", weathering_stage = "saprolite", saprolite_pct = 80, ...)
.ma <- function(t, b, ...) .za_L(t, b, designation = "A", munsell_value_moist = 2, munsell_chroma_moist = 1,
                                 munsell_value_dry = 3, munsell_chroma_dry = 1, structure_type = "subangular blocky",
                                 structure_grade = "moderate", slickensides = "none", clay_pct = 35, silt_pct = 25,
                                 sand_pct = 40, oc_pct = 2, ca_cmol = 15, mg_cmol = 5, k_cmol = 0.5, na_cmol = 0.3, ...)
.pzb <- function(t, b, ...) .za_L(t, b, designation = "Bhs", munsell_hue_moist = "7.5YR", munsell_value_moist = 3,
                                  munsell_chroma_moist = 4, munsell_hue_dry = "7.5YR", munsell_value_dry = 4,
                                  munsell_chroma_dry = 4, oc_pct = 1.5, ph_h2o = 4.5, al_ox_pct = 0.4, fe_ox_pct = 0.6,
                                  clay_pct = 5, silt_pct = 5, sand_pct = 90, ...)
.As <- function(b) .za_A(bottom_cm = b, clay_pct = 5, sand_pct = 85, silt_pct = 10)
.Es <- function(t, b) .E(t, b, clay_pct = 3, munsell_chroma_dry = 1)
.nm <- function(...) .form(.za_P(...))$name

test_that("D1: saprolite within 500 mm under the B (Swartland, Houwhoek)", {
  skip_on_cran()
  expect_identical(.nm(.za_A(), .pcu(25, 70), .za_L(70, 90, designation = "C"), .za_L(90, 130, designation = "Cr")), "Swartland")
  expect_identical(.nm(.As(10), .Es(10, 40), .pzb(40, 70), .za_L(70, 90, designation = "C"), .za_L(90, 130, designation = "Cr")),
                   "Houwhoek")
  expect_identical(.nm(.za_A(), .pcu(25, 70), .za_L(70, 110, designation = "C"), .L0(110, 130, designation = "R")), "Swartland")
  # 600 mm of material before the saprolite stays Valsrivier
  expect_identical(.nm(.za_A(), .pcu(25, 70), .za_L(70, 130, designation = "C"), .sap(130, 160)), "Valsrivier")
})

test_that("D2: the B horizons by position", {
  skip_on_cran()
  expect_identical(.nm(.za_A(), .pcu(25, 80), .za_L(80, 100, designation = "BC"), .za_L(100, 160, designation = "C")), "Valsrivier")
  expect_identical(.nm(.As(15), .pzb(15, 50), .yab(50, 150, designation = "B2", clay_pct = 6, sand_pct = 88, silt_pct = 6)),
                   "Pinegrove")
  expect_identical(.nm(.As(10), .Es(10, 40), .pzb(40, 70), .rab(70, 150, designation = "BC", clay_pct = 6, sand_pct = 88,
                                                                silt_pct = 6)), "Concordia")
})

test_that("D3: an eluvial grey layer is the E, not a G", {
  skip_on_cran()
  expect_identical(.nm(.za_A(designation = "Ap"), .E(25, 45, munsell_value_dry = 6, munsell_chroma_dry = 3), .pcu(45, 110),
                       .za_L(110, 160, designation = "C")), "Klapmuts")
  expect_identical(.nm(.za_A(clay_pct = 8), .E(25, 60, clay_pct = 9), .G(60, 150)), "Kroonstad")
})

test_that("D4: one pass over the horizons, however many undecided layers", {
  skip_on_cran()
  nocol <- function(t, b) .za_L(t, b, designation = "C", munsell_value_moist = NA, munsell_chroma_moist = NA,
                                munsell_hue_moist = NA, munsell_value_dry = NA, munsell_chroma_dry = NA, munsell_hue_dry = NA)
  l11 <- c(list(.za_A(), .pcu(25, 60), .za_L(60, 70, designation = "C")), lapply(0:10, function(k) nocol(70 + k * 7, 77 + k * 7)))
  l5 <- c(list(.za_A(), .pcu(25, 60), .za_L(60, 70, designation = "C")), lapply(0:4, function(k) nocol(70 + k * 16, 86 + k * 16)))
  expect_identical(.form(do.call(.za_P, l11))$name, "Valsrivier")
  expect_identical(.form(do.call(.za_P, l5))$name, "Valsrivier")
  b5 <- c(list(.za_A(bottom_cm = 10)), lapply(1:5, function(k) .za_L(k * 10, (k + 1) * 10, designation = "B", munsell_hue_moist = NA,
                                                                    munsell_hue_dry = NA, structure_type = NA, structure_grade = NA)))
  r <- .form(do.call(.za_P, b5))
  expect_true(is.na(r$name))
  pv <- sub("^forms:", "", r$provisional_because)
  # the candidates are the forms of an orthic A, the B undecided
  expect_true(all(c("Hu", "Cv", "Sd") %in% pv))
  expect_false(any(c("Ch", "Ar", "Ik", "Kp", "Wo") %in% pv))
})

test_that("D5: the podzol B needs its organic carbon on the oxalate route", {
  skip_on_cran()
  expect_false(identical(.nm(.As(10), .Es(10, 40), .pzb(40, 70, oc_pct = 0.1), .za_L(70, 150, designation = "C")), "Concordia"))
})

test_that("D6: saprolite under the topsoil or an E is the lithocutanic B", {
  skip_on_cran()
  expect_identical(.nm(.za_A(bottom_cm = 20), .za_L(20, 60, designation = "Cr"), .L0(60, 80, designation = "R")), "Glenrosa")
  expect_identical(.nm(.za_A(bottom_cm = 20), .E(20, 45), .za_L(45, 80, designation = "Cr")), "Cartref")
})

test_that("D7: a red apedal surface layer with little carbon over rock or a dorbank", {
  skip_on_cran()
  r <- .form(.za_P(.rab(0, 30, designation = "A", oc_pct = 0.2), .L0(30, 50, designation = "R")))
  expect_identical(r$name, "Hutton")
  expect_true(r$qualifiers$za_truncated)
  dor <- .za_L(30, 60, designation = "Bqm", cementation_class = "strongly", pan_type = "duripan")
  expect_identical(.nm(.rab(0, 30, designation = NA, oc_pct = 0.2), dor), "Garies")
  # B1 (0.9.394): 100 mm or less over hard rock is the orthic A, Mispah
  expect_identical(.form(.za_P(.rab(0, 10, designation = "A", oc_pct = 0.2), .L0(10, 30, designation = "R")))$name, "Mispah")
  expect_identical(.form(.za_P(.yab(0, 10, designation = "A", oc_pct = 0.2), .L0(10, 30, designation = "R")))$name, "Mispah")
  expect_identical(.form(.za_P(.yab(0, 15, designation = "A", oc_pct = 0.2), .L0(15, 30, designation = "R")))$name, "Clovelly")
})

test_that("D8: the topsoil over a deposit is part of the deposit", {
  skip_on_cran()
  r <- .form(.za_P(.za_A(bottom_cm = 10), .za_L(10, 35, designation = "C", anthropic_material = "mine spoil"), .rab(35, 150)))
  expect_identical(r$name, "Hutton")
  expect_true(any(grepl("buried soil is classified", r$warnings)))
  expect_true(r$qualifiers$za_truncated)
  r2 <- .form(.za_P(.za_A(bottom_cm = 10), .za_L(10, 40, designation = "C", stratification_pattern = "continuous",
                                                layer_origin = "fluvic"), .rab(40, 150)))
  expect_identical(r2$name, "Hutton")
})

test_that("D9 and R5: the topsoil of several layers", {
  skip_on_cran()
  expect_identical(.nm(.za_L(0, 8, designation = "A1", oc_pct = 3, munsell_value_dry = NA, munsell_chroma_dry = NA,
                             munsell_value_moist = 2, munsell_chroma_moist = 1, clay_pct = 30),
                       .za_L(8, 40, designation = "A2", oc_pct = 2.5, munsell_value_moist = 3, munsell_chroma_moist = 2,
                             munsell_value_dry = 4, munsell_chroma_dry = 2, clay_pct = 30, ca_cmol = 0.5, mg_cmol = 0.2,
                             k_cmol = 0.1, na_cmol = 0.05), .rab(40, 160)), "Inanda")
  expect_identical(.nm(.ma(0, 20, designation = "A1"),
                       .za_L(20, 50, designation = "A2", munsell_value_dry = 4, munsell_chroma_dry = 2, munsell_value_moist = 3,
                             munsell_chroma_moist = 2, oc_pct = 1.2, clay_pct = 35, ca_cmol = 15, mg_cmol = 5, k_cmol = 0.5,
                             na_cmol = 0.3, structure_type = "subangular blocky", structure_grade = "moderate"),
                       .pcu(50, 100), .za_L(100, 160, designation = "C")), "Bonheim")
})

test_that("R1, R2, R3: grey pedocutanic B, the E under a melanic A, the Avalon B1", {
  skip_on_cran()
  grey <- function(t, b) .pcu(t, b, munsell_value_moist = 4, munsell_chroma_moist = 2, munsell_value_dry = 5, munsell_chroma_dry = 2)
  expect_identical(.nm(.za_A(), grey(25, 90), .za_L(90, 160, designation = "C")), "Valsrivier")
  expect_identical(.nm(.ma(0, 30), grey(30, 90), .za_L(90, 160, designation = "C")), "Bonheim")
  expect_false(identical(.nm(.ma(0, 25), .E(25, 60), .G(60, 150)), "Inhoek"))
  spl <- .za_L(60, 150, designation = "B2", munsell_value_moist = 6, munsell_chroma_moist = 6, munsell_value_dry = 7,
               munsell_chroma_dry = 6, redoximorphic_features_pct = 25, mottle_colors = "10YR 6/1; 7.5YR 5/8", clay_pct = 30)
  expect_identical(.nm(.za_A(), .yab(25, 60, designation = "B1", redoximorphic_features_pct = 12, mottle_colors = "7.5YR 5/8"), spl),
                   "Avalon")
})

test_that("R6, R7, R9: colours of a B, the layer below 1500 mm, a surface C", {
  skip_on_cran()
  # a thin yellow-brown layer over a red apedal B: one red B
  r <- .form(.za_P(.za_A(), .yab(25, 35), .rab(35, 150)))
  expect_identical(r$name, "Hutton")
  expect_true(any(grepl("R6", r$warnings, fixed = TRUE)))
  # each 20 cm or more: two horizons
  expect_identical(.nm(.za_A(), .yab(25, 60), .rab(60, 150)), "Griffin")
  # R7: the gleyed C below 1500 mm is read
  r7 <- .form(.za_P(.za_A(), .pcu(25, 150), .G(150, 200, designation = "C")))
  expect_identical(r7$name, "Sepane")
  expect_true(any(grepl("R7", r7$warnings, fixed = TRUE)))
  # R9: a surface C is the orthic A
  expect_identical(.nm(.za_L(0, 30, designation = "C"), .L0(30, 50, designation = "R")), "Mispah")
})
