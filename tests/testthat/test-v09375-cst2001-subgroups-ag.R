# v0.9.375: the subgroups of orders A to G of the Chinese Soil Taxonomy (2001,
# English edition): the keys to subgroups of key 5.2
# (inst/rules/cst2001/subgroups/{A..G}.yaml), the presets they call
# (presets.yaml, expanded at load time by R/rule-presets.R), the criteria of
# R/key-cst-subgroups.R, [A1.3.x] of Appendix 1 (Q3), one example per
# subgroup, and the groups of orders H to N stopping at the group.

.c375 <- function(p, ...) classify_cst2001(p, on_missing = "silent", ...)
.prov375 <- function(r) length(r$provisional_because) > 0L || any(grepl("^Provisional", r$warnings))
.cp375 <- function(h, site = list()) soilKey:::.cst_ex_build(h, c(list(id = "cst375"), site))
.f375 <- function(fn, p, ...) get(fn, envir = asNamespace("soilKey"))(p, ...)
.sg375 <- function(k) unname(unlist(k$subgroups, recursive = FALSE))

test_that("the registry reaches the subgroups and the presets are expanded at load time", {
  skip_on_cran()
  sys <- soilKey:::.system_get("cst2001")
  expect_identical(sys$levels, c("orders", "suborders", "groups", "subgroups"))
  expect_true("presets" %in% unlist(sys$rule_files))
  k <- load_rules("cst2001")
  expect_null(k$presets)
  pre <- attr(k, "presets")
  expect_true(all(c("lithic", "mottlic", "gleyic", "parasalic", "sodic", "sulfic", "dystric", "eutrophic",
                    "trunic", "xanthic", "hue_run", "hue_share", "dark_surface", "takyric", "crack_days",
                    "ustic_region", "calcic_or_evidence", "paragypsic", "anthrostagnic", "dark_epipedon") %in% names(pre)))
  expect_identical(attr(k, "preset_problems"), character(0))
  # no preset name is left in the expanded rules
  preds <- unique(soilKey:::.collect_rule_predicates(k))
  expect_false(any(preds %in% names(pre)))
  res <- soilKey:::.validate_rules("cst2001")
  expect_identical(res$missing, character(0))
  expect_identical(res$unsuffixed, character(0))
  expect_identical(res$mixed, character(0))
  expect_identical(res$malformed, character(0))
  expect_identical(res$codes, character(0))
  expect_identical(res$presets, character(0))
  # a lithic row: the preset call became the predicate with its numbers
  a111 <- k$subgroups[["A1.1"]][[1]]
  expect_identical(names(a111$tests$all_of[[1]]), "contact_sg_cst")
  expect_identical(a111$tests$all_of[[1]]$contact_sg_cst$top_max, 100L)
  expect_identical(a111$tests$all_of[[1]]$contact_sg_cst$kind, "lithic_or_paralithic")
  expect_null(a111$tests$all_of[[1]]$contact_sg_cst$top_min)
  # the other systems have no presets and read as before
  w <- load_rules("wrb2022")
  expect_null(attr(w, "presets"))
  expect_identical(soilKey:::.validate_rules("usda")$presets, character(0))
})

test_that("the preset expander fills, drops, nests and reports", {
  skip_on_cran()
  rules <- list(
    presets = list(
      one = list(params = list(d = "required", k = "a", m = NULL),
                 template = list(f_cst = list(x = "$d", y = "$k", z = "$m"))),
      two = list(params = list(d = 5), template = list(all_of = list(list(one = list(d = "$d")), list(g_cst = list())))),
      bad = list(params = list(), template = list(f_cst = list(x = "$nope")))),
    orders = list(list(code = "X", name = "X", tests = list(all_of = list(
      list(one = list(d = 3)), list(one = list(d = 4, k = "b", m = 7)), list(two = list()),
      list(any_of = list(list(one = list(k = "c")), list(one = list(d = 1, w = 2)), list(bad = list()))))))))
  out <- soilKey:::.expand_presets(rules)
  t <- out$orders[[1]]$tests$all_of
  expect_identical(t[[1]], list(f_cst = list(x = 3, y = "a")))
  expect_identical(t[[2]], list(f_cst = list(x = 4, y = "b", z = 7)))
  expect_identical(t[[3]], list(all_of = list(list(f_cst = list(x = 5, y = "a")), list(g_cst = list()))))
  expect_null(out$presets)
  pr <- attr(out, "preset_problems")
  expect_true(any(grepl("preset one needs d", pr)))
  expect_true(any(grepl("preset one has no parameter w", pr)))
  expect_true(any(grepl("preset bad names nope, not a parameter", pr)))
  expect_true(any(grepl("presets/bad: the template names nope", pr, fixed = TRUE)))
  # a rule set without presets is returned untouched
  r0 <- list(orders = list(list(code = "X", name = "X", tests = list(default = TRUE))))
  expect_identical(soilKey:::.expand_presets(r0), r0)
})

test_that("the keys to subgroups of A to G: 176 rows in the book's order, Typic last", {
  skip_on_cran()
  k <- load_rules("cst2001")
  ag <- unlist(lapply(k$groups[substr(names(k$groups), 1, 1) %in% LETTERS[1:7]], function(l) vapply(l, `[[`, "", "code")))
  # every group of A-G of the book has its list; soilKey's residuals and H-N none
  expect_setequal(names(k$subgroups), setdiff(ag, c("[A1.4]", "[A2.5]")))
  hn <- unlist(lapply(k$groups[substr(names(k$groups), 1, 1) %in% LETTERS[8:14]], function(l) vapply(l, `[[`, "", "code")))
  expect_false(any(hn %in% names(k$subgroups)))
  all <- .sg375(k)
  codes <- vapply(all, `[[`, "", "code")
  expect_equal(length(codes), 176L)
  expect_equal(as.vector(table(substr(gsub("^\\[", "", codes), 1, 1))), c(32L, 40L, 3L, 19L, 16L, 20L, 46L))
  f <- testthat::test_path("..", "..", "data-raw", "cst2001_taxa.csv")
  if (file.exists(f)) {
    csv <- utils::read.csv(f, stringsAsFactors = FALSE)
    sg <- csv[csv$level == "subgroup" & substr(gsub("^\\[", "", csv$code), 1, 1) %in% LETTERS[1:7], ]
    expect_identical(codes, sg$code)
    expect_identical(vapply(all, `[[`, "", "name"), sg$name)
    for (g in names(k$subgroups))
      expect_true(all(sg$parent[sg$code %in% vapply(k$subgroups[[g]], `[[`, "", "code")] == g), info = g)
  }
  for (g in names(k$subgroups)) {
    l <- k$subgroups[[g]]
    expect_true(isTRUE(l[[length(l)]]$tests$default), info = g)
    expect_equal(sum(vapply(l, function(e) isTRUE(e$tests$default), NA)), 1L, info = g)
    expect_identical(l[[length(l)]]$adjective, "Typic", info = g)
  }
  expect_true(all(vapply(all, function(e) nzchar(e$name_pt %||% "") && !is.null(e$page) && nzchar(e$notes %||% ""), NA)))
  # the book's provisional marks of the subgroup level
  expect_identical(codes[vapply(all, function(e) isTRUE(e$book_provisional), NA)], c("C1.1.1", "D1.1.1"))
  # [A1.3.x] at its Appendix position, with the test of A1.2.3
  a13 <- vapply(k$subgroups[["A1.3"]], `[[`, "", "code")
  expect_identical(a13, c("A1.3.1", "A1.3.2", "[A1.3.x]", "A1.3.3", "A1.3.4"))
  ax <- k$subgroups[["A1.3"]][[3]]
  expect_true(isTRUE(ax$appendix_only))
  expect_identical(ax$name, "Lithic Hemi-Permagelic Histosols")
  expect_identical(ax$tests, k$subgroups[["A1.2"]][[3]]$tests)
  # the spellings of Q2: G2.5.2 Mottlic with the key's Redoxic as an alias
  g252 <- Find(function(e) e$code == "G2.5.2", all)
  expect_identical(g252$name, "Mottlic Hapli-Orthic Aridosols")
  expect_true("Redoxic Hapli-Orthic Aridosols" %in% g252$aliases)
  expect_true("Parasalic Hemic-Orthic Histosols" %in% Find(function(e) e$code == "A2.3.5", all)$aliases)
})

test_that("complex subgroups are both of their parts and keyed before them", {
  skip_on_cran()
  k <- load_rules("cst2001")
  at <- function(g, code) { l <- k$subgroups[[g]]; i <- which(vapply(l, `[[`, "", "code") == code); list(i = i, e = l[[i]]) }
  pairs <- list(c("B2.1", "B2.1.2", "B2.1.3", "B2.1.4"), c("F2.2", "F2.2.1", "F2.2.2", "F2.2.3"),
                c("G2.2", "G2.2.3", "G2.2.4", "G2.2.5"), c("G2.5", "G2.5.3", "G2.5.4", "G2.5.5"))
  for (p in pairs) {
    cx <- at(p[1], p[2]); a <- at(p[1], p[3]); b <- at(p[1], p[4])
    expect_true(cx$i < a$i && cx$i < b$i, info = p[2])
    expect_length(cx$e$tests$all_of, 2L)
  }
  # the parts are the simple rows' tests
  expect_identical(at("F2.2", "F2.2.1")$e$tests$all_of, c(at("F2.2", "F2.2.2")$e$tests$all_of, at("F2.2", "F2.2.3")$e$tests$all_of))
  expect_identical(at("G2.5", "G2.5.3")$e$tests$all_of, c(at("G2.5", "G2.5.4")$e$tests$all_of, at("G2.5", "G2.5.5")$e$tests$all_of))
  expect_identical(at("B2.1", "B2.1.2")$e$tests$all_of, c(at("B2.1", "B2.1.4")$e$tests$all_of, at("B2.1", "B2.1.3")$e$tests$all_of))
})

test_that("every Appendix 1 name of orders A to G resolves to exactly one subgroup row", {
  skip_on_cran()
  k <- load_rules("cst2001")
  all <- .sg375(k)
  resolve <- function(nm) sum(vapply(all, function(e) identical(e$name, nm) || nm %in% unlist(e$aliases), NA))
  f <- testthat::test_path("..", "..", "data-raw", "cst2001_taxa.csv")
  skip_if_not(file.exists(f))
  csv <- utils::read.csv(f, stringsAsFactors = FALSE)
  sg <- csv[csv$level == "subgroup" & substr(gsub("^\\[", "", csv$code), 1, 1) %in% LETTERS[1:7], ]
  expect_equal(nrow(sg), 176L)
  for (i in seq_len(nrow(sg))) {
    expect_equal(resolve(sg$name[i]), 1L, info = sg$name[i])
    for (a in trimws(strsplit(sg$aliases[i], ";")[[1]])) if (nzchar(a)) expect_equal(resolve(a), 1L, info = a)
  }
  # names and aliases are unique across the rows
  every <- unlist(lapply(all, function(e) c(e$name, unlist(e$aliases))))
  expect_false(any(duplicated(every)))
})

test_that("every subgroup example keys to its own subgroup, not provisionally", {
  skip_on_cran()
  codes <- soilKey:::.CST_SUBGROUP_EXAMPLE_CODES
  expect_equal(length(codes), 176L)
  k <- load_rules("cst2001")
  expect_identical(codes, vapply(.sg375(k), `[[`, "", "code"))
  for (cd in codes) {
    p <- make_cst2001_example(cd)
    expect_identical(p$site$id, paste0("cst2001-", cd))
    r <- .c375(p, rules = k)
    expect_identical(r$levels$code[4], cd, info = cd)
    expect_identical(r$levels$level, c("orders", "suborders", "groups", "subgroups"), info = cd)
    expect_false(.prov375(r), info = cd)
    expect_identical(r$name, r$levels$name[4], info = cd)
    expect_true(nzchar(r$levels$name_pt[4]), info = cd)
  }
  expect_error(make_cst2001_example("E1.3.4", near_miss = TRUE), "No near miss")
})

test_that("the group and suborder examples keep their order, suborder and group", {
  skip_on_cran()
  k <- load_rules("cst2001")
  for (cd in soilKey:::.CST_GROUP_EXAMPLE_CODES) {
    r <- .c375(make_cst2001_example(cd), rules = k)
    expect_identical(r$levels$code[3], cd, info = cd)
    expect_false(any(grepl("^(orders|suborders|groups):", r$provisional_because)), info = cd)
    # orders A to G reach a subgroup; H to N and soilKey's residuals stop at the group
    hn <- substr(cd, 1, 1) %in% LETTERS[8:14] || grepl("^\\[", cd)
    expect_identical(nrow(r$levels), if (hn) 3L else 4L, info = cd)
  }
})

test_that("the notes: H to N stop at the group, [A1.3.x], the book's provisional subgroups", {
  skip_on_cran()
  r <- .c375(make_cst2001_example("K3.2"))
  expect_identical(r$levels$code, c("K", "K3", "K3.2"))
  expect_true(any(grepl("The subgroups of K3.2 .* are not keyed in this release", r$warnings)))
  # a bracketed residual keeps its own note only
  r <- .c375(make_cst2001_example("[A2.5]"))
  expect_false(any(grepl("are not keyed in this release", r$warnings)))
  expect_true(any(grepl("is soilKey's residual", r$warnings)))
  # an A-G group reaches a subgroup and carries no such note
  r <- .c375(make_cst2001_example("E1.3.4"))
  expect_identical(r$name, "Xanthic Hapli-Udic Ferralosols")
  expect_identical(r$levels$name_pt[4], "Ferralossolos údicos háplicos xânticos")
  expect_false(any(grepl("are not keyed in this release", r$warnings)))
  r <- .c375(make_cst2001_example("[A1.3.x]"))
  expect_identical(r$name, "Lithic Hemi-Permagelic Histosols")
  expect_true(any(grepl("\\[A1.3.x\\] Lithic Hemi-Permagelic Histosols is listed in Appendix 1 only", r$warnings)))
  r <- .c375(make_cst2001_example("D1.1.1"))
  expect_identical(r$levels$book_provisional, c(FALSE, FALSE, FALSE, TRUE))
  expect_true(any(grepl("D1.1.1 Typic Geli-Cryic Andosols is marked provisional in CST 2001", r$warnings, fixed = TRUE)))
})

test_that("an undecided subgroup makes the Typic provisional; a provisional reading gives the alternative", {
  skip_on_cran()
  # the region not recorded: Ustic Calci-Aquic undecided, the Typic provisional
  p <- make_cst2001_example("F1.1.3"); p$site$climatic_moisture_regime <- NULL
  r <- .c375(p)
  expect_identical(r$levels$code[4], "F1.1.3")
  expect_true("subgroups:F1.1.1" %in% r$provisional_because)
  # sodium not measured in an Orthic Aridosol: Sodic undecided
  p <- make_cst2001_example("G2.4.3"); p$horizons$na_cmol <- NA_real_
  r <- .c375(p)
  expect_identical(r$levels$code[4], "G2.4.3")
  expect_true("subgroups:G2.4.1" %in% r$provisional_because)
  # gley colours without a test (Q11): Endogleyic provisionally, with the alternative
  p <- make_cst2001_example("B1.4.5"); p$horizons$childs_test[4] <- NA_character_
  r <- .c375(p)
  expect_identical(r$levels$code[4], "B1.4.5")
  expect_true(any(grepl("^Provisional: B1.4.5 Endogleyic Hapli-Stagnic Anthrosols rests on a reading", r$warnings)))
  expect_identical(r$candidates$code, "B1.4.6")
  expect_identical(r$candidates$instead_of, "B1.4.5")
  # a share cut by the base of the description (Eutrophic, 100 of 125 cm
  # described): provisional on the described part, with the alternative
  p <- make_cst2001_example("E1.3.7"); p$horizons$bottom_cm[3] <- 100; p$horizons$bs_pct <- c(20, 40, 40)
  r <- .c375(p)
  expect_identical(r$levels$code[4], "E1.3.7")
  expect_true(any(grepl("^Provisional: E1.3.7 Eutrophic Hapli-Udic Ferralosols rests on a reading", r$warnings)))
  expect_identical(r$candidates$code, "E1.3.8")
})

# --- the presets at their edges -------------------------------------------------------------------

test_that("lithic: depth, kind, between depths, fragments, extent", {
  skip_on_cran()
  mk <- function(rtop, des = "R", site = list(), cf = NA) .cp375(list(designation = c("A", "Bw", des), top_cm = c(0, 20, rtop),
    bottom_cm = c(20, rtop, rtop + 20), clay_pct = 20, silt_pct = 40, sand_pct = 40, oc_pct = c(1, 0.4, NA),
    coarse_fragments_pct = cf), site)
  f <- function(p, ...) .f375("contact_sg_cst", p, ...)$passed
  expect_true(f(mk(50), top_max = 50, kind = "lithic"))
  expect_false(f(mk(51), top_max = 50, kind = "lithic"))
  # a paralithic contact (Cr with a contact recorded) passes lithic_or_paralithic only
  cr <- mk(40, des = "Cr", site = list(lithic_contact_cm = 40))
  expect_identical(f(cr, top_max = 50, kind = "lithic_or_paralithic"), paralithic_contact_cst(cr, top_max = 50)$passed)
  # between 50 and 100 (F3.3.1)
  expect_false(f(mk(45), top_max = 100, top_min = 50))
  expect_true(f(mk(60), top_max = 100, top_min = 50))
  # the fragments alternative of the later orders
  fr <- .cp375(list(designation = c("A", "Bw", "C"), top_cm = c(0, 20, 30), bottom_cm = c(20, 30, 80),
                    coarse_fragments_pct = c(10, 80, 80), clay_pct = 20, silt_pct = 40, sand_pct = 40))
  expect_false(f(fr, top_max = 50))
  expect_true(f(fr, top_max = 50, fragments = "more_than_half"))
  fr$horizons$coarse_fragments_pct <- c(10, 80, 10); fr$horizons$top_cm <- c(0, 25, 50); fr$horizons$bottom_cm <- c(25, 50, 80)
  expect_true(f(fr, top_max = 50, fragments = "half_or_more"))
  expect_false(f(fr, top_max = 50, fragments = "more_than_half"))
  # the extent under the plane: recorded over half passes, half fails, not recorded provisional
  expect_true(f(mk(40, site = list(lithic_contact_extent_pct = 80)), top_max = 50, plane_extent = TRUE))
  expect_false(f(mk(40, site = list(lithic_contact_extent_pct = 50)), top_max = 50, plane_extent = TRUE))
  r <- .f375("contact_sg_cst", mk(40), top_max = 50, plane_extent = TRUE)
  expect_true(r$passed); expect_true(isTRUE(r$evidence$provisional))
})

test_that("a feature in a window: one run, layers totalling, anywhere", {
  skip_on_cran()
  mk <- function(mot) .cp375(list(designation = c("A", "B1", "B2", "B3", "C"), top_cm = c(0, 20, 50, 56, 62),
    bottom_cm = c(20, 50, 56, 62, 120), clay_pct = 25, silt_pct = 40, sand_pct = 35, oc_pct = c(1.2, 0.5, 0.4, 0.3, 0.2),
    munsell_hue_moist = "10YR", munsell_value_moist = 5, munsell_chroma_moist = 4, mottle_colors = mot),
    list(drainage_class = "moderately well drained"))
  f <- function(p, ...) .f375("feature_run_cst", p, features = "redoxic_features", ...)$passed
  # mottles in B2 and C (not contiguous): 6 + 38 cm inside 50-100
  p <- mk(c(NA, NA, "7.5YR 5/6", NA, "7.5YR 5/6"))
  expect_true(f(p, min_cm = 10, from_cm = 50, to_cm = 100))
  expect_true(f(p, min_cm = 10, from_cm = 50, to_cm = 100, total = TRUE))
  # B2 alone: 6 cm, under 10 for a run; B2 and B3 apart from each other by nothing: a run of 12
  p <- mk(c(NA, NA, "7.5YR 5/6", NA, NA))
  expect_false(f(p, min_cm = 10, from_cm = 50, to_cm = 100))
  expect_true(f(p, min_cm = 0, from_cm = 50, to_cm = 100))
  expect_false(f(p, min_cm = 0, from_cm = 60, to_cm = 100))
  p <- mk(c(NA, NA, "7.5YR 5/6", "7.5YR 5/6", NA))
  expect_true(f(p, min_cm = 10, from_cm = 50, to_cm = 100))
  # two separate horizons of 6 cm: no run of 10, but a total of 12
  p <- mk(c(NA, NA, "7.5YR 5/6", NA, NA)); p2 <- mk(c(NA, "7.5YR 5/6", "7.5YR 5/6", NA, NA))
  expect_false(f(p, min_cm = 10, from_cm = 50, to_cm = 100, total = TRUE))
  expect_true(f(p2, min_cm = 10, from_cm = 45, to_cm = 100, total = TRUE))
  # rock carries no feature; nothing in the window is FALSE
  expect_false(f(mk(rep(NA, 5)), min_cm = 10, from_cm = 50, to_cm = 100))
  # several features, each on its own run
  r <- .f375("feature_run_cst", mk(c(NA, NA, "7.5YR 5/6", "7.5YR 5/6", NA)),
             features = c("gleyic_features", "redoxic_features"), min_cm = 10, to_cm = 100, grade = "at_least_evidence")
  expect_true(r$passed)
})

test_that("the Histosol criteria: Buric, Terric, the tiers and the control section", {
  skip_on_cran()
  ex <- function(cd) make_cst2001_example(cd)
  expect_true(.f375("buric_cst", ex("A2.2.1"))$passed)
  expect_false(.f375("buric_cst", ex("A2.2.6"))$passed)
  # a mineral cover of 40 cm is not under 40
  p <- ex("A2.2.1"); p$horizons$bottom_cm[1] <- 40; p$horizons$top_cm[2] <- 40
  expect_false(.f375("buric_cst", p)$passed)
  expect_true(.f375("terric_cst", ex("A2.2.2"))$passed)
  # the mineral layer 29 cm thick: not Terric
  p <- ex("A2.2.2"); p$horizons$bottom_cm[3] <- 79; p$horizons$top_cm[4] <- 79
  expect_false(.f375("terric_cst", p)$passed)
  expect_true(.f375("histosol_tier_layers_cst", ex("A2.2.4"), kind = "hemic", min_cm = 25)$passed)
  expect_false(.f375("histosol_tier_layers_cst", ex("A2.2.6"), kind = "hemic", min_cm = 25)$passed)
  expect_true(.f375("histosol_tier_layers_cst", ex("A2.2.5"), kind = "sapric", min_cm = 12.5)$passed)
  # the kind not known in the tiers: undecided
  p <- ex("A2.2.6"); p$horizons$fiber_content_rubbed_pct[3] <- NA
  expect_true(is.na(.f375("histosol_tier_layers_cst", p, kind = "sapric", min_cm = 12.5)$passed))
  expect_true(.f375("histosol_lithic_cs_cst", ex("A2.2.3"))$passed)
  expect_false(.f375("histosol_lithic_cs_cst", ex("A2.2.6"))$passed)
})

test_that("Recalcaric, the salts of B2.2.2 and Dystric", {
  skip_on_cran()
  mk <- function(cc) .cp375(list(designation = c("Ap1", "Ap2", "Bg", "C"), top_cm = c(0, 15, 22, 60),
                                 bottom_cm = c(15, 22, 60, 100), caco3_pct = cc, clay_pct = 25, silt_pct = 45, sand_pct = 30))
  f <- function(cc) .f375("recalcaric_cst", mk(cc), within = 60)$passed
  expect_true(f(c(6, 5, 3, 1)))
  expect_false(f(c(4.5, 4, 3, 1)))      # 45 g/kg is not exceeded
  expect_false(f(c(6, 7, 3, 1)))        # the maximum is not in the surface horizon
  expect_true(is.na(f(c(6, NA, NA, 1)))) # one layer measured within 60 cm
  expect_true(is.na(f(c(NA, 5, 3, 1))))
  # the salts: arid band 5-20 g/kg, other 1.5-10, in the cultivated horizon
  sal <- function(s, site) .f375("soluble_salt_band_cst", .cp375(list(designation = c("Ap", "C"), top_cm = c(0, 20),
    bottom_cm = c(20, 100), soluble_salts_pct = c(s, 0.05), clay_pct = 20, silt_pct = 50, sand_pct = 30), site))$passed
  expect_true(sal(1.5, list(climatic_moisture_regime = "aridic")))
  expect_false(sal(1.5, list(climatic_moisture_regime = "udic")))
  expect_true(sal(0.2, list(climatic_moisture_regime = "udic")))
  expect_false(sal(0.2, list(climatic_moisture_regime = "aridic")))
  expect_true(sal(0.7, list()))            # in both bands
  expect_true(is.na(sal(1.5, list())))     # arid band only, the region unknown
  expect_false(sal(2.5, list()))
  # Dystric: every horizon touching 25-50 cm
  dy <- function(bs, ph, bot = 120) .f375("dystric_cst", .cp375(list(designation = c("Ap", "B1", "B2"), top_cm = c(0, 20, 40),
    bottom_cm = c(20, 40, bot), bs_pct = bs, ph_h2o = ph, clay_pct = 20, silt_pct = 50, sand_pct = 30)), from_cm = 25, to_cm = 50)
  expect_true(dy(c(80, 40, 45), c(6.5, 6.0, 6.0))$passed)
  expect_true(dy(c(80, 60, 60), c(6.5, 5.2, 5.0))$passed)
  expect_false(dy(c(80, 40, 60), c(6.5, 6.0, 6.0))$passed)
  expect_true(is.na(dy(c(80, 40, NA), c(6.5, 6.0, NA))$passed))
  r <- dy(c(80, 40, 45), c(6.5, 6.0, 6.0), bot = 45)
  expect_true(r$passed); expect_true(isTRUE(r$evidence$provisional))
})

test_that("Trunic, Xanthic, the hue run and Eutrophic", {
  skip_on_cran()
  ex <- function(cd) make_cst2001_example(cd)
  expect_true(.f375("trunic_cst", ex("E1.3.1"), horizon = "ferralic_horizon")$passed)
  expect_false(.f375("trunic_cst", ex("E1.3.8"), horizon = "ferralic_horizon")$passed)
  p <- ex("E1.3.8"); p$site$anthropic_truncation <- TRUE
  expect_true(.f375("trunic_cst", p, horizon = "ferralic_horizon")$passed)
  expect_true(.f375("trunic_cst", ex("F2.2.3"), horizon = "b")$passed)
  expect_false(.f375("trunic_cst", ex("F2.2.4"), horizon = "b")$passed)
  p <- ex("F2.2.3"); p$horizons$designation <- NA_character_
  expect_true(is.na(.f375("trunic_cst", p, horizon = "b")$passed))
  # the upper 10 cm of the B: 7.5YR passes, 5YR not; the neutral hue is yellower
  expect_true(.f375("b_upper_hue_cst", ex("E1.3.4"))$passed)
  expect_false(.f375("b_upper_hue_cst", ex("E1.3.8"))$passed)
  p <- ex("E1.3.8"); p$horizons$munsell_hue_moist <- c("5YR", "N", "5YR")
  expect_true(.f375("b_upper_hue_cst", p)$passed)
  # a B whose upper 10 cm mixes 7.5YR and 5YR fails
  p <- ex("E1.3.4"); p$horizons$bottom_cm[2] <- 20; p$horizons$top_cm[3] <- 20
  expect_false(.f375("b_upper_hue_cst", p)$passed)
  expect_true(.f375("hue_run_cst", ex("E1.3.5"), hue = "7.5YR", rule = "yellower", to_cm = 100)$passed)
  expect_false(.f375("hue_run_cst", ex("E1.3.8"), hue = "7.5YR", rule = "yellower", to_cm = 100)$passed)
  # the hue share of the later orders: 5YR or redder in more than half
  expect_true(.f375("hue_share_cst", ex("E1.3.8"), hue = "5YR", rule = "redder", share = 0.5, strict = TRUE)$passed)
  expect_false(.f375("hue_share_cst", ex("E1.3.8"), hue = "5YR", rule = "redder_than", share = 0.5, strict = TRUE)$passed)
  # Eutrophic: 35 % in 80 % of 0-125 cm
  eu <- function(bs) { p <- ex("E1.3.8"); p$horizons$bs_pct <- bs; .f375("bs_share_cst", p, min_pct = 35, share = 0.8)$passed }
  expect_true(eu(c(40, 40, 40)))
  expect_true(eu(c(20, 40, 40)))     # 15 of 125 cm under 35: 88 %
  expect_false(eu(c(40, 20, 40)))    # 65 cm under 35
  expect_true(is.na(eu(c(40, NA, 40))))
})

test_that("Dark, the argic horizon with lime, takyric and the cracks", {
  skip_on_cran()
  ex <- function(cd) make_cst2001_example(cd)
  expect_true(.f375("dark_surface_cst", ex("G2.1.8"))$passed)
  expect_false(.f375("dark_surface_cst", ex("G2.1.9"))$passed)
  dk <- function(vm, vd) { p <- ex("G2.1.8"); p$horizons$munsell_value_moist <- vm; p$horizons$munsell_value_dry <- vd
    .f375("dark_surface_cst", p)$passed }
  # the upper 18 cm: 8 cm of Avk and 10 of Bw
  expect_true(dk(c(3.5, 3.5, 6, 6), c(5.5, 5.5, 7, 7)))
  expect_false(dk(c(3.5, 3.6, 6, 6), c(5.5, 5.5, 7, 7)))
  expect_false(dk(c(3, 3, 6, 6), c(5.5, 6, 7, 7)))
  expect_true(is.na(dk(c(3, 3, 6, 6), c(NA, 5, 7, 7))))
  expect_true(.f375("argic_lime_cst", ex("G1.1.2"))$passed)
  p <- ex("G1.1.2"); p$horizons$caco3_pct[2] <- 0; p$horizons$secondary_carbonates_pct[2] <- 0
  expect_false(.f375("argic_lime_cst", p)$passed)
  p$horizons$caco3_pct[2] <- NA
  expect_true(is.na(.f375("argic_lime_cst", p)$passed))
  expect_false(.f375("argic_lime_cst", ex("G1.1.4"))$passed)
  expect_true(.f375("takyric_surface_cst", ex("G2.5.4"))$passed)
  expect_false(.f375("takyric_surface_cst", ex("G2.5.8"))$passed)
  # the cracks: 210 days recorded, or observed under an aridic regime only
  expect_true(.f375("crack_days_cst", ex("F2.2.2"), days = 210)$passed)
  expect_false(.f375("crack_days_cst", ex("F2.2.4"), days = 210)$passed)
  p <- ex("F2.2.4"); p$site$cracks_open_days <- NULL
  expect_true(is.na(.f375("crack_days_cst", p, days = 210)$passed))
  p$site$soil_moisture_regime <- "aridic"
  expect_true(.f375("crack_days_cst", p, days = 210)$passed)
})

test_that("B1.1.5: the grey subhorizon above the gleyic horizon; shajiang in a window", {
  skip_on_cran()
  ex <- function(cd) make_cst2001_example(cd)
  expect_true(.f375("fe_percoleached_cst", ex("B1.1.5"), above_gleyic = TRUE)$passed)
  p <- ex("B1.1.5"); p$horizons$childs_test[4] <- "negative"
  expect_true(.f375("fe_percoleached_cst", p)$passed)
  expect_false(.f375("fe_percoleached_cst", p, above_gleyic = TRUE)$passed)
  expect_true(.f375("shajiang_cst", ex("F1.1.2"), from_cm = 50, within = 100, total = TRUE)$passed)
  expect_false(.f375("shajiang_cst", ex("F1.1.3"), from_cm = 50, within = 100, total = TRUE)$passed)
  # the group reading (a run within 125 cm) unchanged
  expect_identical(.f375("shajiang_cst", ex("F1.1.2"))$passed, TRUE)
})
