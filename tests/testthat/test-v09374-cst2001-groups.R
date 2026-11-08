# v0.9.374: the groups of the Chinese Soil Taxonomy (2001, English edition):
# the keys to groups of key 5.2 (inst/rules/cst2001/groups/{A..N}.yaml), the
# criteria they define (R/key-cst-groups.R), soilKey's bracketed residuals
# [A1.4] and [A2.5] (Q4), and one example per group.

.c374 <- function(p, ...) classify_cst2001(p, on_missing = "silent", ...)
# v0.9.375: provisional above the subgroups
.prov374 <- function(r) {
  any(!grepl("^subgroups:", r$provisional_because)) ||
    any(grepl("^Provisional: \\S+ .* rests on a reading", r$warnings) &
          !grepl("^Provisional: \\[?[A-N][0-9]+\\.[0-9]+\\.", r$warnings))
}
.SUB374 <- c(A1 = "A1.3", A2 = "A2.3", B1 = "B1.3", B2 = "B2.4", C1 = "C1.1", C2 = "C2.1", D1 = "D1.2",
             D2 = "D2.1", D3 = "D3.2", E1 = "E1.1", F1 = "F1.2", F2 = "F2.1", F3 = "F3.3", G1 = "G1.1",
             G2 = "G2.3", H1 = "H1.3", H2 = "H2.1", I1 = "I1.2", I2 = "I2.2", I3 = "I3.2", J1 = "J1.1",
             J2 = "J2.3", J3 = "J3.2", K1 = "K1.1", K2 = "K2.2", K3 = "K3.4", L1 = "L1.3", L2 = "L2.2",
             L3 = "L3.2", L4 = "L4.6", M1 = "M1.6", M2 = "M2.4", M3 = "M3.5", M4 = "M4.5", M5 = "M5.3",
             N1 = "N1.1", N2 = "N2.3", N3 = "N3.4", N4 = "N4.1")

test_that("the registry reaches the groups", {
  skip_on_cran()
  sys <- soilKey:::.system_get("cst2001")
  expect_identical(sys$levels, c("orders", "suborders", "groups", "subgroups"))
  expect_true(file.exists(system.file("rules", "cst2001", "groups", "M.yaml", package = "soilKey")))
})

test_that("the keys to groups: 138 groups of the book and two bracketed residuals, in the book's order", {
  skip_on_cran()
  k <- load_rules("cst2001")
  subs <- unlist(lapply(k$suborders, function(l) vapply(l, `[[`, "", "code")), use.names = FALSE)
  expect_identical(names(k$groups), setdiff(subs, "[E2]"))
  all <- unname(unlist(k$groups, recursive = FALSE))
  codes <- vapply(all, `[[`, "", "code", USE.NAMES = FALSE)
  expect_equal(length(codes), 140L)
  expect_identical(codes[grepl("^\\[", codes)], c("[A1.4]", "[A2.5]"))
  expect_true(all(vapply(all[grepl("^\\[", codes)], function(e) isTRUE(e$bracketed), NA)))
  expect_identical(vapply(all[grepl("^\\[", codes)], `[[`, "", "name"),
                   c("[Other Permagelic Histosols]", "[Other Orthic Histosols]"))
  # the names and their order are those of Appendix 1 (data-raw/cst2001_taxa.csv)
  f <- testthat::test_path("..", "..", "data-raw", "cst2001_taxa.csv")
  if (file.exists(f)) {
    csv <- utils::read.csv(f, stringsAsFactors = FALSE)
    g <- csv[csv$level == "group", ]
    expect_identical(codes[!grepl("^\\[", codes)], g$code)
    expect_identical(vapply(all, `[[`, "", "name")[!grepl("^\\[", codes)], g$name)
    # each list sits under its suborder
    for (s in names(k$groups))
      expect_true(all(g$parent[g$code %in% vapply(k$groups[[s]], `[[`, "", "code")] == s), info = s)
  }
  # every list ends with its residual
  for (s in names(k$groups)) {
    l <- k$groups[[s]]
    expect_true(isTRUE(l[[length(l)]]$tests$default), info = s)
    expect_equal(sum(vapply(l, function(e) isTRUE(e$tests$default), NA)), 1L, info = s)
  }
  expect_true(all(vapply(all, function(e) !is.null(e$name_pt) && nzchar(e$name_pt), NA)))
  expect_true(all(vapply(all[!grepl("^\\[", codes)], function(e) !is.null(e$page), NA)))
  # the book's provisional marks of the group level
  bp <- codes[vapply(all, function(e) isTRUE(e$book_provisional), NA)]
  expect_identical(bp, c("C1.1", "C2.1"))
  # the spellings of Q2 kept as aliases
  d22 <- Find(function(e) e$code == "D2.2", all)
  expect_identical(d22$name, "Udi-Vitric Andosols")
  expect_true("Udic-Vitric Andosols" %in% d22$aliases)
  m43 <- Find(function(e) e$code == "M4.3", all)
  expect_true("Calci-Perudic Cambosols" %in% m43$aliases)
  res <- soilKey:::.validate_rules("cst2001")
  expect_equal(res$missing, character(0))
})

test_that("every group example keys to its own group, not provisionally", {
  skip_on_cran()
  codes <- soilKey:::.CST_GROUP_EXAMPLE_CODES
  expect_equal(length(codes), 140L)
  k <- load_rules("cst2001")
  expect_setequal(codes, unlist(lapply(k$groups, function(l) vapply(l, `[[`, "", "code")), use.names = FALSE))
  for (cd in codes) {
    p <- make_cst2001_example(cd)
    expect_identical(p$site$id, paste0("cst2001-", cd))
    r <- .c374(p)
    expect_identical(r$levels$code[3], cd, info = cd)
    expect_identical(r$levels$level[1:3], c("orders", "suborders", "groups"), info = cd)
    expect_false(.prov374(r), info = cd)
    expect_true(nzchar(r$levels$name_pt[3]), info = cd)
  }
  expect_error(make_cst2001_example("K3.2", near_miss = TRUE), "No near miss")
})

test_that("the 39 suborder examples key to their suborder and a group, not provisionally", {
  skip_on_cran()
  for (cd in names(.SUB374)) {
    r <- .c374(make_cst2001_example(cd))
    expect_identical(r$levels$code[1:3], c(substr(cd, 1, 1), cd, .SUB374[[cd]]), info = cd)
    expect_false(.prov374(r), info = cd)
  }
  # the group example of these groups is the suborder example itself
  p <- make_cst2001_example("K3.4"); q <- make_cst2001_example("K3")
  expect_identical(as.data.frame(p$horizons), as.data.frame(q$horizons))
})

test_that("the book's provisional groups and soilKey's bracketed residuals carry their notes", {
  skip_on_cran()
  r <- .c374(make_cst2001_example("C1"))
  expect_identical(r$levels$book_provisional[1:3], c(FALSE, FALSE, TRUE))
  expect_true(any(grepl("C1.1 Hapli-Humic Spodosols is marked provisional in CST 2001", r$warnings, fixed = TRUE)))
  expect_false(.prov374(r))
  # a sapric Permagelic Histosol: no printed group, soilKey's residual
  r <- .c374(make_cst2001_example("[A1.4]"))
  expect_identical(r$name, "[Other Permagelic Histosols]")
  expect_identical(r$levels$name_pt[3], "[Outros Organossolos permagélicos]")
  expect_true(any(grepl("\\[A1.4\\] \\[Other Permagelic Histosols\\] is soilKey's residual", r$warnings)))
  # [E2] has no groups: the name stops at the suborder, with the note
  p <- make_cst2001_example("E1"); p$site$soil_moisture_regime <- "ustic"
  r <- .c374(p)
  expect_identical(r$levels$code, c("E", "[E2]"))
  expect_true(any(grepl("\\[E2\\] \\[Other Ferralosols\\] is soilKey's residual", r$warnings)))
  # a group of the book carries no such note
  expect_false(any(grepl("soilKey's residual", .c374(make_cst2001_example("E1"))$warnings)))
})

test_that("an undecided group makes the name provisional; a provisional reading gives the alternative", {
  skip_on_cran()
  # the allitic property not measured in a Udic Ferrosol: K3.3 undecided
  p <- make_cst2001_example("K3")
  for (a in c("sio2_al2o3_clay", "sio2_al2o3_naoh", "sio2_sulfuric_pct", "al2o3_sulfuric_pct")) p$horizons[[a]] <- NA_real_
  r <- .c374(p)
  expect_identical(r$levels$code[3], "K3.4")
  expect_true("groups:K3.3" %in% r$provisional_because)
  # gley colours without a test in a paddy soil: Gleyi-Stagnic, with the
  # alternative the key reaches without that reading
  p <- make_cst2001_example("B1.1"); p$horizons$childs_test <- NA_character_
  r <- .c374(p)
  expect_identical(r$levels$code[3], "B1.1")
  expect_true(any(grepl("^Provisional: B1.1 Gleyi-Stagnic Anthrosols rests on a reading", r$warnings)))
  expect_match(r$candidates$code, "^B1\\.3(\\.[0-9]+)?$")
  expect_identical(r$candidates$instead_of, "B1.1")
  # the regime not recorded: the climate groups of the Primosols undecided,
  # the Udi- residual provisional
  p <- make_cst2001_example("N2.5"); p$site$soil_moisture_regime <- NULL
  r <- .c374(p)
  expect_identical(r$levels$code[1], "N")
  expect_true(.prov374(r))
})

test_that("the residual named for a regime reached under another one carries a note", {
  skip_on_cran()
  r <- .c374(make_cst2001_example("H2.2"))
  expect_identical(r$levels$name[3], "Aqui-Orthic Halosols")  # v0.9.376: a subgroup follows
  expect_true(any(grepl("H2.2 Aqui-Orthic Halosols is the residual of its key, named for the aquic regime", r$warnings,
                        fixed = TRUE)))
  # under an aquic regime, no note
  p <- make_cst2001_example("H2.2"); p$site$soil_moisture_regime <- "aquic"; p$site$climatic_moisture_regime <- "ustic"
  p$site$drainage_class <- "poorly drained"
  r <- .c374(p)
  expect_identical(r$levels$name[3], "Aqui-Orthic Halosols")  # v0.9.376: a subgroup follows
  expect_false(any(grepl("named for the aquic regime", r$warnings)))
  # a Udi- residual reached under the udic regime: no note
  expect_false(any(grepl("residual of its key", .c374(make_cst2001_example("N2.5"))$warnings)))
})

test_that("the criteria of the group keys read their edges", {
  skip_on_cran()
  # Foli-: a few days of saturation (under 30); 30 days fails; nothing
  # recorded and no drainage class, undecided
  p <- make_cst2001_example("A2.1")
  expect_true(soilKey:::foli_histosol_cst(p)$passed)
  p$horizons$water_saturation_days[1] <- 29
  expect_true(soilKey:::foli_histosol_cst(p)$passed)
  p$horizons$water_saturation_days[1] <- 30
  expect_false(soilKey:::foli_histosol_cst(p)$passed)
  p$horizons$water_saturation_days[1] <- NA; p$site$drainage_class <- NULL
  expect_true(is.na(soilKey:::foli_histosol_cst(p)$passed))
  # the kind of the surface tier: fibre not measured, undecided
  p <- make_cst2001_example("A2.4")
  expect_true(soilKey:::histosol_tier_kind_cst(p, "sapric")$passed)
  expect_false(soilKey:::histosol_tier_kind_cst(p, "fibric")$passed)
  p$horizons$fiber_content_rubbed_pct <- NA_real_
  expect_true(is.na(soilKey:::histosol_tier_kind_cst(p, "sapric")$passed))
  # iron accumulation: 1.5 times passes, 1.4 times fails, not measured undecided
  p <- make_cst2001_example("B1")
  r <- soilKey:::fe_accumulation_cst(p)
  expect_true(r$passed)
  p$horizons$fe_dcb_pct[3] <- 1.8
  expect_true(soilKey:::fe_accumulation_cst(p)$passed)
  p$horizons$fe_dcb_pct[3] <- 1.68
  expect_false(soilKey:::fe_accumulation_cst(p)$passed)
  p$horizons$fe_dcb_pct[3] <- NA
  expect_true(is.na(soilKey:::fe_accumulation_cst(p)$passed))
  # the rhodic colours in half of the window (Q22, by thickness)
  p <- make_cst2001_example("E1")
  expect_true(soilKey:::colour_share_cst(p, "rhodic")$passed)
  p$horizons$munsell_hue_moist <- c("2.5YR", "2.5YR", "5YR")      # 0-80 of 0-125: 64 %
  expect_true(soilKey:::colour_share_cst(p, "rhodic")$passed)
  p$horizons$munsell_hue_moist <- c("2.5YR", "5YR", "2.5YR")      # 0-15 and 80-125: 48 %
  expect_false(soilKey:::colour_share_cst(p, "rhodic")$passed)
  p$horizons$munsell_hue_moist <- "2.5YR"; p$horizons$munsell_value_dry <- c(4, NA, 4)  # 48-100 %
  expect_true(is.na(soilKey:::colour_share_cst(p, "rhodic")$passed))
  expect_true(is.na(soilKey:::udic_perudic_border_cst(make_cst2001_example("E1"))$passed))
  # a calcic horizon with its top between 50 and 125 cm
  p <- make_cst2001_example("L2")
  expect_true(soilKey:::horizon_top_between_cst(p, "calcic_horizon", 50, 125)$passed)
  p$horizons$top_cm[3] <- 45; p$horizons$bottom_cm[2] <- 45
  expect_false(soilKey:::horizon_top_between_cst(p, "calcic_horizon", 50, 125)$passed)
  # a mollic epipedon 50 cm thick
  expect_true(soilKey:::epipedon_thick_cst(make_cst2001_example("J2.3"))$passed)
  expect_false(soilKey:::epipedon_thick_cst(make_cst2001_example("J2.4"))$passed)
  # the takyric surface: cracks 1 cm wide through 1-4 cm
  p <- make_cst2001_example("H1.1")
  expect_true(soilKey:::takyric_surface_cst(p)$passed)
  p$horizons$cracks_depth_cm[1] <- 5
  expect_false(soilKey:::takyric_surface_cst(p)$passed)
  # shajiang, with hard carbonate concretions standing in, flagged
  p <- make_cst2001_example("M2")
  expect_false(soilKey:::shajiang_cst(p)$passed)
  p$horizons$carbonate_hard_pct <- c(NA, 12, 2, NA)
  r <- soilKey:::shajiang_cst(p)
  expect_true(r$passed); expect_true("shajiang_from_hard_carbonates" %in% r$evidence$flags)
  # throughout the B: every B layer; a value not measured, undecided
  p <- make_cst2001_example("M4.5")
  expect_true(soilKey:::b_throughout_cst(p, "acid_or")$passed)
  p$horizons$bs_pct[2] <- NA; p$horizons$ph_h2o[2] <- NA
  expect_true(is.na(soilKey:::b_throughout_cst(p, "acid_or")$passed))
  expect_true(soilKey:::b_throughout_cst(make_cst2001_example("L3"), "alic")$passed)
  expect_false(soilKey:::b_throughout_cst(make_cst2001_example("L3.3"), "alic")$passed)
  # some subhorizon of the B 10 cm thick (the Hiweatheri layer is 20 cm)
  p <- make_cst2001_example("K3.2")
  expect_true(soilKey:::b_subhorizon_cst(p, "hiweatheri")$passed)
  expect_false(soilKey:::b_subhorizon_cst(p, "hiweatheri", min_cm = 25)$passed)
  # an albic horizon above the claypan
  expect_true(soilKey:::albic_above_claypan_cst(make_cst2001_example("L1.1"))$passed)
  expect_false(soilKey:::albic_above_claypan_cst(make_cst2001_example("L1"))$passed)
  # folic histic evidence: litter at the surface
  expect_true(soilKey:::histic_littery_cst(make_cst2001_example("M2.1"))$passed)
  expect_false(soilKey:::histic_littery_cst(make_cst2001_example("M2"))$passed)
  # coral sands within 100 cm; carbonate rocks in more than half of the window
  expect_true(soilKey:::lc_within_cst(make_cst2001_example("J1"), "coral_sands", 100)$passed)
  expect_true(soilKey:::lc_carbonate_share_cst(make_cst2001_example("M5.2"))$passed)
  expect_false(soilKey:::lc_carbonate_share_cst(make_cst2001_example("M5"))$passed)
  # the hydromorphic cumulic epipedon
  expect_true(soilKey:::mud_cumulic_cst(make_cst2001_example("B2.3"))$passed)
  expect_false(soilKey:::mud_cumulic_cst(make_cst2001_example("B2"))$passed)
})

test_that("the readings of this part are listed", {
  skip_on_cran()
  rd <- yaml::read_yaml(system.file("rules", "cst2001", "readings.yaml", package = "soilKey"))$readings
  topics <- vapply(rd, `[[`, "", "topic")
  expect_true(sum(grepl("^Groups:", topics)) >= 8L)
})

# ---- the review of 2026-11-07 ----------------------------------------------------------------------

.mk374 <- function(code, f) {
  p <- make_cst2001_example(code); h <- as.data.frame(p$horizons); r <- f(h, p$site)
  PedonRecord$new(site = r$s, horizons = ensure_horizon_schema(data.table::as.data.table(r$h)))
}

test_that("D1: the depth below the description is undecided in the colour and coral shares", {
  skip_on_cran()
  # a red Ferralosol described to 50 cm: E1.1 on the part described, provisionally,
  # with the alternative
  p <- .mk374("E1", function(h, s) { h <- h[1:2, ]; h$bottom_cm[2] <- 50; list(h = h, s = s) })
  cs <- soilKey:::colour_share_cst(p, "rhodic")
  expect_true(cs$passed)
  expect_true("window_cut_at_description_base" %in% cs$evidence$flags)
  expect_true(cs$evidence$provisional)
  r <- .c374(p)
  expect_identical(r$levels$code[3], "E1.1")
  expect_true(any(grepl("^Provisional: E1.1 Rhodi-Udic Ferralosols rests on a reading", r$warnings)))
  expect_identical(r$candidates$instead_of, "E1.1")
  # yellow in the 50 cm described, the border recorded FALSE: E1.1 undecided
  p <- .mk374("E1", function(h, s) { h <- h[1:2, ]; h$bottom_cm[2] <- 50; h$munsell_hue_moist <- "7.5YR"
    s$udic_perudic_border <- FALSE; list(h = h, s = s) })
  expect_true(is.na(soilKey:::colour_share_cst(p, "rhodic")$passed))
  r <- .c374(p)
  expect_identical(r$levels$code[3], "E1.3")
  expect_true("groups:E1.1" %in% r$provisional_because)
  # described to 125 cm: decided, no flag
  cs <- soilKey:::colour_share_cst(make_cst2001_example("E1"), "rhodic")
  expect_true(cs$passed); expect_false("window_cut_at_description_base" %in% cs$evidence$flags)
  # coral sands in every layer of a profile described to 60 cm: more than half of 0-100 for sure;
  # described to 45 cm: on the part described only, provisionally
  coral <- function(bot) .mk374("J1", function(h, s) { h <- h[1:2, ]; h$caco3_pct <- 60; h$sand_pct <- 85
    h$silt_pct <- 10; h$clay_pct <- 5; h$top_cm <- c(0, 30); h$bottom_cm <- c(30, bot); list(h = h, s = s) })
  r <- soilKey:::lc_carbonate_share_cst(coral(60))
  expect_true(r$passed); expect_false("window_cut_at_description_base" %in% r$evidence$flags)
  r <- soilKey:::lc_carbonate_share_cst(coral(45))
  expect_true(r$passed); expect_true("window_cut_at_description_base" %in% r$evidence$flags)
  expect_true(r$evidence$provisional)
})

test_that("D2: a possible continuation of the mollic epipedon leaves the Pachi- group undecided", {
  skip_on_cran()
  p <- .mk374("J2.3", function(h, s) { h$munsell_value_moist[2] <- NA; list(h = h, s = s) })
  expect_true(is.na(soilKey:::epipedon_thick_cst(p)$passed))
  r <- .c374(p)
  expect_identical(r$levels$code[3], "J2.4")
  expect_true("groups:J2.3" %in% r$provisional_because)
  expect_true(soilKey:::epipedon_thick_cst(make_cst2001_example("J2.3"))$passed)
  expect_false(soilKey:::epipedon_thick_cst(make_cst2001_example("J2.4"))$passed)
})

test_that("D3: gleyic evidence counts in the Stagni-Perudic Cambosols", {
  skip_on_cran()
  p <- .mk374("M4.6", function(h, s) { h$munsell_hue_moist <- c("10YR", "5GY", "10YR"); h$munsell_chroma_moist <- c(3, 3, 4)
    h$eh_mv <- c(NA, 348, NA); s$soil_moisture_regime <- "stagnic"; s$climatic_moisture_regime <- "perudic"
    s$drainage_class <- "imperfectly drained"; list(h = h, s = s) })
  expect_false(soilKey:::layer_run_cst(p, "gleyic_features", min_cm = 10, to_cm = 50)$passed)
  expect_true(soilKey:::layer_run_cst(p, "gleyic_features", min_cm = 10, to_cm = 50, grade = "at_least_evidence")$passed)
  r <- .c374(p)
  expect_identical(r$levels$code[3], "M4.2")
  expect_false(.prov374(r))
})

test_that("D4 and the decisions of 2026-11-07 are in the readings", {
  skip_on_cran()
  rd <- yaml::read_yaml(system.file("rules", "cst2001", "readings.yaml", package = "soilKey"))$readings
  tp <- vapply(rd, `[[`, "", "topic"); tx <- vapply(rd, `[[`, "", "reading")
  g <- tx[grepl("^Groups:", tp)]
  expect_false(any(grepl("for Hugo to confirm", g)))
  b12 <- g[grepl("^B1.2", g)]
  expect_match(b12, "chroma 2 or less, as the print reads", fixed = TRUE)
  expect_false(grepl("less than 2", b12, fixed = TRUE))
  expect_true(any(grepl("a hypercalcic horizon does not count", g, fixed = TRUE)))
  expect_true(any(grepl("read on cracks_depth_cm first", g, fixed = TRUE)))
})

test_that("decisions: the drainage class and the crust thickness are flagged stand-ins", {
  skip_on_cran()
  # Foli-: no days recorded, a well-drained record passes, flagged and named
  p <- make_cst2001_example("A2.1"); p$horizons$water_saturation_days <- NA_real_
  p$site$drainage_class <- "well drained"
  f <- soilKey:::foli_histosol_cst(p)
  expect_true(f$passed); expect_true("stand_in:drainage_class" %in% f$evidence$flags)
  r <- .c374(p)
  expect_identical(r$levels$code[3], "A2.1")
  expect_true(any(grepl("^Stand-in: A2.1 Foli-Orthic Histosols rests on a drainage class", r$warnings)))
  # the days recorded: no stand-in
  expect_false(any(grepl("^Stand-in", .c374(make_cst2001_example("A2.1"))$warnings)))
  # Takyri-: the crack depth recorded decides; not recorded, the crust stands in, flagged
  p <- make_cst2001_example("H1.1")
  expect_false(any(grepl("^Stand-in", .c374(p)$warnings)))
  p$horizons$cracks_depth_cm <- NA_real_
  tk <- soilKey:::takyric_surface_cst(p)
  expect_true(tk$passed); expect_true("stand_in:crust_thickness" %in% tk$evidence$flags)
  r <- .c374(p)
  expect_identical(r$levels$code[3], "H1.1")
  expect_true(any(grepl("^Stand-in: H1.1 Takyri-Alkalic Halosols rests on the thickness of the surface crust", r$warnings)))
  p$site$surface_crust_thickness_cm <- 6
  expect_false(soilKey:::takyric_surface_cst(p)$passed)
  p$site$surface_crust_thickness_cm <- NULL
  expect_false(soilKey:::takyric_surface_cst(p)$passed)
})

test_that("decisions: the calcic horizon alone does not take a hypercalcic horizon", {
  skip_on_cran()
  p <- make_cst2001_example("G1")
  expect_true(soilKey:::calcic_only_cst(p, top_max = 100)$passed)
  p$horizons$caco3_pct[3] <- 50; p$horizons$secondary_carbonates_pct[3] <- 55
  expect_true(soilKey:::hypercalcic_horizon_cst(p)$passed)
  expect_false(soilKey:::calcic_only_cst(p, top_max = 100)$passed)
  expect_false(soilKey:::horizon_top_between_cst(p, "calcic_horizon", 0, 125)$passed)
  k <- load_rules("cst2001")
  ids <- function(code) { e <- Find(function(z) z$code == code, unlist(k$groups, recursive = FALSE))
    unlist(lapply(e$tests[[1]], names)) }
  for (cd in c("F1.1", "F3.2", "G1.1", "J2.4")) {
    expect_true("calcic_only_cst" %in% ids(cd), info = cd)
    expect_false("calcic_horizon_cst" %in% ids(cd), info = cd)
  }
  for (cd in c("F2.1", "G2.1")) expect_true("hypercalcic_horizon_cst" %in% ids(cd), info = cd)
})
