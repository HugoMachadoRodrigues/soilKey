# v0.9.376: the subgroups of orders H to N of the Chinese Soil Taxonomy (2001,
# English edition): the keys to subgroups of key 5.2
# (inst/rules/cst2001/subgroups/{H..N}.yaml), the new presets
# (presets.yaml), the criteria of R/key-cst-subgroups-hn.R, one example per
# reachable subgroup, the eight subgroups no profile reaches under the
# readings in force, and the 588 names of Appendix 1.

.c376 <- function(p, ...) classify_cst2001(p, on_missing = "silent", ...)
.prov376 <- function(r) length(r$provisional_because) > 0L || any(grepl("^Provisional", r$warnings))
.cp376 <- function(h, site = list()) soilKey:::.cst_ex_build(h, c(list(id = "cst376"), site))
.f376 <- function(fn, p, ...) get(fn, envir = asNamespace("soilKey"))(p, ...)
.hn376 <- function(codes) substr(gsub("^\\[", "", codes), 1, 1) %in% LETTERS[8:14]
.sg376 <- function(k) unname(unlist(k$subgroups, recursive = FALSE))
.ONCE376 <- c("J2.4.3", "J2.4.4", "J2.5.2", "J2.5.3", "M2.2.1", "M2.3.1", "M2.4.1", "N2.4.4")

test_that("the new presets are expanded and the rules validate", {
  skip_on_cran()
  k <- load_rules("cst2001")
  pre <- attr(k, "presets")
  expect_true(all(c("albic", "calcic", "eutric", "leachic", "ferric", "b_subhorizon", "feature_share", "feature_every",
                    "parasalic_duration", "para_alkalic", "stagnic_perched", "xanthic_sub", "permic", "gelic_frost") %in% names(pre)))
  expect_identical(attr(k, "preset_problems"), character(0))
  expect_false(any(unique(soilKey:::.collect_rule_predicates(k)) %in% names(pre)))
  res <- soilKey:::.validate_rules("cst2001")
  for (f in c("missing", "unsuffixed", "mixed", "malformed", "codes", "presets")) expect_identical(res[[f]], character(0), info = f)
  # the presets of 0.9.375 expand as before (E1.3.4 Xanthic: no part)
  e134 <- Find(function(e) e$code == "E1.3.4", k$subgroups[["E1.3"]])
  expect_null(e134$tests$all_of[[1]]$all_of[[2]]$b_upper_hue_cst$part)
  # a new preset with its numbers: L1.2.2 calcic between 50 and 125 cm
  l122 <- Find(function(e) e$code == "L1.2.2", k$subgroups[["L1.2"]])
  cs <- l122$tests$all_of[[1]]$calcic_sg_cst
  expect_identical(c(cs$top_min, cs$top_max), c(50L, 125L))
  expect_identical(unlist(cs$kinds), "calcic_only")
  # M5.4.4: the upper half of the B
  m544 <- Find(function(e) e$code == "M5.4.4", k$subgroups[["M5.4"]])
  expect_identical(m544$tests$all_of[[1]]$all_of[[2]]$b_upper_hue_cst$part, "upper_half")
  # the registry: subgroups for every group, no stop note
  sys <- soilKey:::.system_get("cst2001")
  expect_identical(sys$levels, c("orders", "suborders", "groups", "subgroups"))
})

test_that("the keys to subgroups of H to N: 412 rows in the book's order, Typic last", {
  skip_on_cran()
  k <- load_rules("cst2001")
  allg <- unlist(lapply(k$groups, function(l) vapply(l, `[[`, "", "code")))
  # every group of the book has its list, soilKey's residuals none
  expect_setequal(names(k$subgroups), setdiff(allg, c("[A1.4]", "[A2.5]")))
  all <- .sg376(k)
  codes <- vapply(all, `[[`, "", "code")
  expect_equal(length(codes), 588L)
  hn <- all[.hn376(codes)]
  hc <- vapply(hn, `[[`, "", "code")
  expect_equal(length(hc), 412L)
  expect_equal(as.vector(table(substr(hc, 1, 1))), c(18L, 25L, 35L, 62L, 71L, 128L, 73L))
  f <- testthat::test_path("..", "..", "data-raw", "cst2001_taxa.csv")
  if (file.exists(f)) {
    csv <- utils::read.csv(f, stringsAsFactors = FALSE)
    sg <- csv[csv$level == "subgroup" & .hn376(csv$code), ]
    expect_identical(hc, sg$code)
    expect_identical(vapply(hn, `[[`, "", "name"), sg$name)
    expect_identical(vapply(hn, function(e) as.character(e$page), ""), as.character(sg$key_page))
  }
  for (g in names(k$subgroups)[.hn376(names(k$subgroups))]) {
    l <- k$subgroups[[g]]
    expect_true(isTRUE(l[[length(l)]]$tests$default), info = g)
    expect_equal(sum(vapply(l, function(e) isTRUE(e$tests$default), NA)), 1L, info = g)
    expect_identical(l[[length(l)]]$adjective, "Typic", info = g)
    expect_true(all(startsWith(vapply(l, `[[`, "", "code"), paste0(g, "."))), info = g)
  }
  expect_true(all(vapply(hn, function(e) nzchar(e$name_pt %||% "") && !is.null(e$page) && nzchar(e$notes %||% "") &&
                           startsWith(e$name, e$adjective), NA)))
  # the book's provisional marks of the subgroup level
  expect_identical(hc[vapply(hn, function(e) isTRUE(e$book_provisional), NA)], c("J1.2.1", "K1.1.1", "M3.5.1"))
  expect_identical(Find(function(e) e$code == "M5.4.4", hn)$name_pt, "Cambissolos údicos álicos xânticos")
})

test_that("complex subgroups are both of their parts and keyed before them", {
  skip_on_cran()
  k <- load_rules("cst2001")
  at <- function(code) {
    g <- sub("\\.[0-9]+$", "", code); l <- k$subgroups[[g]]
    i <- which(vapply(l, `[[`, "", "code") == code); list(i = i, e = l[[i]])
  }
  tt <- function(code) at(code)$e$tests$all_of
  # Mottli-argic, Mottli-calcic and Calci-argic of the Pachi-Ustic Isohumosols
  expect_identical(tt("J2.3.1"), c(tt("J2.4.1"), tt("J2.3.4")))
  expect_identical(tt("J2.3.2"), c(tt("J2.4.1"), tt("J2.3.5")))
  expect_identical(tt("J2.3.3"), c(tt("J2.3.5"), tt("J2.3.4")))
  for (p in list(c("J2.3.1", "J2.3.4"), c("J2.3.2", "J2.3.5"), c("J2.3.3", "J2.3.4"), c("L1.3.3", "L1.3.4"),
                 c("L4.2.1", "L4.2.2"), c("H2.1.2", "H2.1.3"), c("N4.7.4", "N4.7.5"), c("J2.4.3", "J2.4.4"), c("J2.5.2", "J2.5.3")))
    expect_true(at(p[1])$i < at(p[2])$i, info = p[1])
  # Glossi-para-alkalic: the glossic part and the Para-alkalic row
  expect_identical(tt("J2.4.3")[2], tt("J2.4.4"))
  expect_identical(tt("J2.5.2"), c(tt("J2.4.3")[1], tt("J2.5.3")))
  # Mottli-albic and Humi-brown: the first part is the simple row
  expect_identical(tt("L1.3.3")[1], tt("L1.3.4"))
  expect_identical(tt("L4.2.1")[1], tt("L4.2.2"))
  expect_identical(tt("H2.1.2")[1], tt("H2.1.3"))
  # Verti-lithic reads 25 cm, Lithic 50
  expect_identical(tt("N4.7.4")[[1]]$contact_sg_cst$top_max, 25L)
  expect_identical(tt("N4.7.5")[[1]]$contact_sg_cst$top_max, 50L)
})

test_that("every Appendix 1 name resolves to exactly one subgroup row (588)", {
  skip_on_cran()
  k <- load_rules("cst2001")
  all <- .sg376(k)
  idx <- new.env()
  for (e in all) for (nm in unique(c(e$name, unlist(e$aliases)))) assign(nm, c(get0(nm, idx, inherits = FALSE), e$code), envir = idx)
  f <- testthat::test_path("..", "..", "data-raw", "cst2001_taxa.csv")
  skip_if_not(file.exists(f))
  csv <- utils::read.csv(f, stringsAsFactors = FALSE)
  sg <- csv[csv$level == "subgroup", ]
  expect_equal(nrow(sg), 588L)
  for (i in seq_len(nrow(sg))) {
    expect_identical(get0(sg$name[i], idx, inherits = FALSE), sg$code[i], info = sg$name[i])
    for (a in trimws(strsplit(sg$aliases[i], ";")[[1]])) if (nzchar(a))
      expect_identical(get0(a, idx, inherits = FALSE), sg$code[i], info = a)
  }
  every <- unlist(lapply(all, function(e) c(e$name, unlist(e$aliases))))
  expect_false(any(duplicated(every)))
})

test_that("every subgroup example of H to N keys to its own subgroup, not provisionally", {
  skip_on_cran()
  k <- load_rules("cst2001")
  codes <- soilKey:::.CST_SUBGROUP_EXAMPLE_CODES
  expect_equal(length(codes), 588L)
  hc <- codes[.hn376(codes)]
  expect_equal(length(hc), 412L)
  keyed <- vapply(.sg376(k), `[[`, "", "code")
  expect_identical(hc, keyed[.hn376(keyed)])
  for (cd in hc) {
    p <- make_cst2001_example(cd)
    expect_identical(p$site$id, paste0("cst2001-", cd))
    r <- .c376(p, rules = k)
    expect_identical(r$levels$code[4], cd, info = cd)
    expect_false(.prov376(r), info = cd)
    expect_identical(r$name, r$levels$name[4], info = cd)
  }
})

test_that("the subgroups the first reading left unreachable are reached (Hugo, 2026-11-08)", {
  skip_on_cran()
  expect_null(get0(".CST_SUBGROUP_UNREACHABLE", envir = asNamespace("soilKey"), inherits = FALSE))
  for (cd in .ONCE376) expect_identical(.c376(make_cst2001_example(cd))$levels$code[4], cd, info = cd)
  # alkalic evidence on its printed items: an ESP of 10 in a B that is not argic
  p <- make_cst2001_example("J2.5.3")
  expect_true(alkalic_horizon_cst(p, grade = "at_least_evidence", top_max = 50, datum = "mineral")$passed)
  expect_false(argic_horizon_cst(p, top_max = 50, datum = "mineral")$passed)
  # anthrostagnic evidence on a short paddy history beside the aquic regime
  p <- make_cst2001_example("M2.4.1")
  expect_true(moisture_regime_cst(p, "aquic")$passed)
  expect_true(anthrostagnic_epipedon_cst(p, grade = "at_least_evidence")$passed)
  expect_false(anthrostagnic_epipedon_cst(p)$passed)
  # the litter of N2.4.4 is not parent material: the sandy deposits hold
  expect_true(lc_sandy_cst(make_cst2001_example("N2.4.4"))$passed)
  # L4.3.1: the claypan exposed by its argillans (item 4)
  p <- make_cst2001_example("L4.3.1")
  x <- soilKey:::.cst_ctx(p)
  expect_true(soilKey:::.cst_hz(x, "claypan")$full[1])
  expect_true(.f376("trunic_cst", p, horizon = "argic_or_claypan")$passed)
  # M3.3.2: alkalic evidence in a Cambosol
  expect_identical(.c376(make_cst2001_example("M3.3.2"))$levels$code[4], "M3.3.2")
})

test_that("the group and suborder examples of H to N reach a subgroup and keep their classes", {
  skip_on_cran()
  k <- load_rules("cst2001")
  for (cd in grep("^[H-N]", soilKey:::.CST_GROUP_EXAMPLE_CODES, value = TRUE)) {
    r <- .c376(make_cst2001_example(cd), rules = k)
    expect_identical(r$levels$code[3], cd, info = cd)
    expect_identical(nrow(r$levels), 4L, info = cd)
    expect_false(any(grepl("^(orders|suborders|groups):", r$provisional_because)), info = cd)
    expect_false(any(grepl("are not keyed in this release", r$warnings)), info = cd)
  }
  for (cd in grep("^[H-N]", soilKey:::.CST_EXAMPLE_CODES, value = TRUE))
    expect_identical(.c376(make_cst2001_example(cd), rules = k)$levels$code[2], cd, info = cd)
  # the book's own provisional mark on a subgroup
  r <- .c376(make_cst2001_example("M3.5.1"))
  expect_true(any(grepl("M3.5.1 Typic Hapli-Ustic Cambosols is marked provisional in CST 2001", r$warnings, fixed = TRUE)))
})

test_that("undecided subgroups make the name provisional; provisional readings give the alternative", {
  skip_on_cran()
  # coarse fragments not measured: Lithic undecided, the Typic provisional
  p <- make_cst2001_example("K3.5.10"); p$horizons$coarse_fragments_pct <- NA_real_
  r <- .c376(p)
  expect_identical(r$levels$code[4], "K3.5.10")
  expect_true("subgroups:K3.5.1" %in% r$provisional_because)
  # perched water not recorded: Stagnic undecided, Mottlic (gleyic evidence) after it
  p <- make_cst2001_example("K2.2.5"); p$site$groundwater_kind <- NULL
  r <- .c376(p)
  expect_identical(r$levels$code[4], "K2.2.6")
  expect_true("subgroups:K2.2.5" %in% r$provisional_because)
  # the salic duration not recorded: Parasalic provisionally, with the alternative
  p <- make_cst2001_example("M2.2.3"); p$site$salic_months_per_year <- NULL
  r <- .c376(p)
  expect_identical(r$levels$code[4], "M2.2.3")
  expect_true(any(grepl("^Provisional: M2.2.3 Parasalic Shajiang-Aquic Cambosols rests on a reading", r$warnings)))
  expect_identical(r$candidates$code, "M2.2.6")
  # the udic regime bordering on the perudic one not recorded: Xanthic undecided
  p <- make_cst2001_example("L4.4.5"); p$site$udic_perudic_border <- NULL
  r <- .c376(p)
  expect_true("subgroups:L4.4.4" %in% r$provisional_because)
})

# --- the criteria at their edges ------------------------------------------------------------------

test_that("Albic: anywhere, within a depth, below the mollic epipedon, above the argic horizon", {
  skip_on_cran()
  ex <- function(cd) make_cst2001_example(cd)
  f <- function(p, ...) .f376("albic_sg_cst", p, ...)$passed
  expect_true(f(ex("L4.7.3"), position = "above_argic"))
  expect_false(f(ex("L4.7.5"), position = "above_argic"))
  expect_true(f(ex("J3.1.1"), features = "albic", position = "below_mollic"))
  # the E within the mollic epipedon's depth is not below it
  p <- ex("J3.1.1"); p$horizons$munsell_value_moist[2] <- 6
  expect_true(f(p, features = "albic"))
  expect_true(f(ex("M5.7.1"), within = 125))
  expect_false(f(ex("M5.7.1"), within = 10))
  # M4.2.1: the albic horizon alone within 50 cm; a glossic-only profile fails it
  expect_true(f(ex("M4.2.1"), features = "albic", within = 50))
  p <- ex("M5.7.4"); p$horizons$tongue_depth_cm <- c(NA, 8, NA, NA); p$horizons$tongue_width_mm <- c(NA, 5, NA, NA)
  p$horizons$tongue_vol_pct <- c(NA, 20, NA, NA)
  expect_true(f(p, features = "glossic"))
  expect_false(f(p, features = "albic"))
  # no argic horizon: above_argic fails
  expect_false(f(ex("M5.7.1"), position = "above_argic"))
})

test_that("Calcic: the calcic horizon alone, with soft lime or the calcipan, within or between depths", {
  skip_on_cran()
  ex <- function(cd) make_cst2001_example(cd)
  f <- function(p, ...) .f376("calcic_sg_cst", p, ...)$passed
  expect_true(f(ex("M3.4.1"), top_max = 100))
  expect_false(f(ex("M3.4.2"), top_max = 100))
  # between 50 and 125: the Ck from 80 passes, a calcic horizon from 45 does not
  expect_true(f(ex("L1.2.2"), top_min = 50, top_max = 125))
  p <- ex("L1.2.2"); p$horizons$top_cm[4] <- 45; p$horizons$bottom_cm[3] <- 45
  expect_false(f(p, top_min = 50, top_max = 125))
  # soft powdery carbonates recorded: soft lime, flagged
  p <- ex("J2.3.6"); p$horizons$secondary_carbonates_form <- c(NA, NA, "powdery", NA)
  expect_false(f(p, top_max = 100))
  r <- .f376("calcic_sg_cst", p, kinds = c("calcic_only", "soft_lime"), top_max = 100)
  expect_true(r$passed); expect_true("soft_lime_from_powdery_carbonates" %in% r$evidence$flags)
  # the window below the base of the description is undecided
  p <- ex("M3.4.2"); p$horizons$bottom_cm[3] <- 90
  expect_true(is.na(f(p, top_max = 100)))
})

test_that("per-layer tests: Dystric, Eutric, pH 7, Leachic, Ferric", {
  skip_on_cran()
  ex <- function(cd) make_cst2001_example(cd)
  lt <- function(p, ...) .f376("layer_test_cst", p, ...)$passed
  # throughout the claypan / the argic horizon or claypan
  expect_true(.f376("dystric_cst", ex("L1.1.4"), scope = "claypan")$passed)
  expect_false(.f376("dystric_cst", ex("L1.1.5"), scope = "claypan")$passed)
  expect_true(.f376("dystric_cst", ex("L4.3.4"), scope = "argic_or_claypan")$passed)
  p <- ex("L4.3.4"); p$horizons$bs_pct[3] <- 60
  expect_false(.f376("dystric_cst", p, scope = "argic_or_claypan")$passed)
  p$horizons$bs_pct[3] <- NA; p$horizons$ph_h2o[3] <- NA
  expect_true(is.na(.f376("dystric_cst", p, scope = "argic_or_claypan")$passed))
  # Eutric of N4.3.2: every horizon within 25 cm; a mixed profile is Typic
  expect_true(lt(ex("N4.3.2"), "eutric_or", to_cm = 25))
  p <- ex("N4.3.3"); p$horizons$ph_h2o <- c(6.0, 5.0); p$horizons$bs_pct <- c(60, 40)
  expect_false(lt(p, "eutric_or", to_cm = 25))
  expect_identical(.c376(p)$levels$code[4], "N4.3.3")
  # pH 7.0 throughout the argic horizon or claypan
  expect_true(lt(ex("L4.3.3"), "ph_7", scope = "argic_or_claypan"))
  p <- ex("L4.3.3"); p$horizons$ph_h2o[3] <- 6.9
  expect_false(lt(p, "ph_7", scope = "argic_or_claypan"))
  # Leachic: more than half of the window to the contact
  expect_true(lt(ex("K3.1.3"), "leachic", mode = "share", share = 0.5, strict = TRUE))
  expect_false(lt(ex("K3.1.6"), "leachic", mode = "share", share = 0.5, strict = TRUE))
  p <- ex("K3.1.3"); p$horizons$ph_h2o <- c(6.0, 6.6, NA); p$horizons$top_cm <- c(0, 55, 110)
  p$horizons$bottom_cm <- c(55, 110, 130)
  expect_false(lt(p, "leachic", mode = "share", share = 0.5, strict = TRUE))   # 55 of 110 cm: not more than half
  # Ferric throughout the argic horizon or claypan, in a B subhorizon
  expect_true(lt(ex("L4.1.2"), "ferric", scope = "argic_or_claypan"))
  expect_false(lt(ex("L4.1.3"), "ferric", scope = "argic_or_claypan"))
  expect_true(.f376("b_subhorizon_cst", ex("M4.5.4"), test = "ferric")$passed)
  expect_false(.f376("b_subhorizon_cst", ex("M4.5.5"), test = "ferric")$passed)
  expect_true(.f376("b_subhorizon_cst", ex("M4.1.2"), test = "spodic")$passed)
  # Alic in any B layer (no thickness): one alic layer of the B
  expect_true(.f376("b_subhorizon_cst", ex("L4.5.1"), test = "alic", min_cm = 0)$passed)
  expect_false(.f376("b_subhorizon_cst", ex("L4.5.5"), test = "alic", min_cm = 0)$passed)
})

test_that("features in a share, in every layer, below the claypan", {
  skip_on_cran()
  ex <- function(cd) make_cst2001_example(cd)
  expect_true(.f376("feature_share_cst", ex("M5.2.1"), features = "phosphic_property", within = 100)$passed)
  p <- ex("M5.2.1"); p$horizons$p_total_pct <- c(1.0, 0.8, 0.1, 0.1)   # 60 of 100 cm
  expect_true(.f376("feature_share_cst", p, features = "phosphic_property", within = 100)$passed)
  p$horizons$p_total_pct <- c(1.0, 0.1, 0.1, 0.1)                        # 15 of 100 cm
  expect_false(.f376("feature_share_cst", p, features = "phosphic_property", within = 100)$passed)
  # gleyic evidence in every layer between 20 and 50 cm
  ev <- function(p) .f376("feature_every_cst", p, features = "gleyic_features", from_cm = 20, to_cm = 50)$passed
  expect_true(ev(ex("N2.1.2")))
  p <- ex("N2.1.2"); p$horizons$reduced_matrix_pct <- c(40, NA); p$horizons$childs_test <- c("positive", "negative")
  expect_false(ev(p))
  # below the top of the claypan
  expect_true(.f376("feature_below_cst", ex("L1.1.2"), features = "gleyic_features")$passed)
  expect_false(.f376("feature_below_cst", ex("L1.1.5"), features = "gleyic_features")$passed)
})

test_that("salic duration, perched water, vertic evidence within 25 cm", {
  skip_on_cran()
  ex <- function(cd) make_cst2001_example(cd)
  sd <- function(p) .f376("salic_duration_cst", p, within = 25, min_cm = 0)
  expect_true(sd(ex("M2.2.3"))$passed)
  p <- ex("M2.2.3"); p$site$salic_months_per_year <- 0.5
  expect_false(sd(p)$passed)
  p$site$salic_months_per_year <- NULL
  r <- sd(p); expect_true(r$passed); expect_true(isTRUE(r$evidence$provisional))
  expect_false(sd(ex("M2.2.6"))$passed)
  st <- function(p) .f376("stagnic_perched_cst", p)$passed
  expect_true(st(ex("K2.2.5")))
  p <- ex("K2.2.5"); p$site$groundwater_kind <- "apparent"
  expect_false(st(p))
  p$site$groundwater_kind <- NULL
  expect_true(is.na(st(p)))
  expect_false(st(ex("K2.2.9")))
  # Verti-lithic: the contact and the vertic features within 25 cm
  expect_true(.f376("vertic_within_cst", ex("N4.7.4"), within = 25)$passed)
  expect_false(.f376("vertic_within_cst", ex("N4.7.6"), within = 25)$passed)
  expect_true(.f376("contact_sg_cst", ex("N4.7.4"), top_max = 25, kind = "lithic_or_paralithic")$passed)
})

test_that("the histic kind, Concretic, Cinderic and Grayic", {
  skip_on_cran()
  ex <- function(cd) make_cst2001_example(cd)
  hk <- function(p, k) .f376("histic_kind_cst", p, kind = k)$passed
  expect_true(hk(ex("I3.1.1"), "fibric")); expect_false(hk(ex("I3.1.1"), "sapric"))
  expect_true(hk(ex("I3.1.2"), "sapric")); expect_false(hk(ex("I3.1.3"), "fibric"))
  p <- ex("I3.1.1"); p$horizons$fiber_content_rubbed_pct <- NA_real_
  expect_true(is.na(hk(p, "fibric")))
  # Concretic: 30 % nodules in an indurated subhorizon 5 cm thick below the albic
  cc <- function(p) .f376("concretic_cst", p)
  expect_true(cc(ex("L4.1.1"))$passed)
  p <- ex("L4.1.1"); p$horizons$fe_mn_nodules_pct[3] <- 29
  expect_false(cc(p)$passed)
  p$horizons$fe_mn_nodules_pct[3] <- NA; p$horizons$ferric_nodules_pct <- c(NA, NA, 20, NA, NA)
  p$horizons$manganiferous_nodules_pct <- c(NA, NA, 12, NA, NA)
  r <- cc(p); expect_true(r$passed); expect_true("fe_mn_nodules_from_sum" %in% r$evidence$flags)
  p <- ex("L4.1.1"); p$horizons$cementation_class <- NA_character_
  expect_false(cc(p)$passed)
  # Cinderic: the weighted mean of 0-25 cm
  py <- function(p) .f376("pyroclastic_cst", p)
  expect_true(py(ex("N4.4.2"))$passed)
  p <- ex("N4.4.2"); p$horizons$pyroclastic_fragments_pct <- c(35, 0)   # 15 cm at 35, 10 at 0
  expect_false(py(p)$passed)
  p$horizons$pyroclastic_fragments_pct <- c(NA, 40); p$horizons$layer_origin <- c("tephric", NA)
  r <- py(p); expect_true(r$passed); expect_true("pyroclastic_from_tephric_origin" %in% r$evidence$flags)
  expect_false(py(ex("N4.4.5"))$passed)
  # Grayic: 6 g/kg over 0-20 cm
  og <- function(oc) { p <- ex("N4.1.2"); p$horizons$oc_pct <- oc; .f376("oc_weighted_cst", p)$passed }
  expect_true(og(c(0.6, 0.6))); expect_false(og(c(0.6, 0.59))); expect_true(is.na(og(c(0.8, NA))))
})

test_that("Dilluvic and Marinic", {
  skip_on_cran()
  ex <- function(cd) make_cst2001_example(cd)
  dl <- function(p) .f376("dilluvic_cst", p)$passed
  expect_true(dl(ex("H2.1.1")))
  p <- ex("H2.1.1"); p$site$parent_material <- NULL
  expect_false(dl(p))                                   # the origin is a field record
  p <- ex("H2.1.1"); p$horizons$soluble_salts_pct <- c(2.8, 1.5, 0.6)
  expect_true(is.na(dl(p)))                             # regular to 110 cm, 200 not described (2026-11-08)
  p$horizons$bottom_cm[3] <- 210
  expect_false(dl(p))                                   # decreasing with depth to 210 cm: regular
  p$horizons$soluble_salts_pct <- c(2.8, NA, 0.6)
  expect_true(is.na(dl(p)))
  mr <- function(p) .f376("marinic_cst", p)$passed
  expect_true(mr(ex("H2.2.3")))
  p <- ex("H2.2.3"); p$horizons$soluble_salts_chloride_pct <- c(40, 40, 40); p$horizons$soluble_salts_sulfate_pct <- c(50, 50, 50)
  expect_false(mr(p))
  p <- ex("H2.2.3"); p$horizons$soluble_salts_sodium_pct <- c(40, 40, 40)
  expect_true(is.na(mr(p)))
  p$horizons$soluble_salts_sodium_pct <- c(20, 20, 20)
  expect_false(mr(p))
  p <- ex("H2.2.3"); p$horizons$stratification_pattern <- NA_character_
  expect_false(mr(p))
})

test_that("Xanthic of M5.4.4 reads a subhorizon in the upper half of the B", {
  skip_on_cran()
  ex <- function(cd) make_cst2001_example(cd)
  bu <- function(p, ...) .f376("b_upper_hue_cst", p, ...)$passed
  p <- ex("M5.4.8")   # Bw 15-60 and C 60-130 within 125: the B is 15-60, its upper half 15-37.5
  expect_true(bu(p, part = "upper_half"))
  p$horizons$munsell_hue_moist <- c("10YR", "5YR", "10YR")
  expect_false(bu(p, part = "upper_half"))
  # a 7.5YR run of 12 cm starting at 20 cm inside the upper half passes, the
  # upper 10 cm of the B (the other Xanthic subgroups) does not
  p <- ex("M5.4.8")
  p$horizons <- p$horizons[c(1, 2, 2, 3), ]
  p$horizons$top_cm <- c(0, 15, 20, 60); p$horizons$bottom_cm <- c(15, 20, 60, 130)
  p$horizons$munsell_hue_moist <- c("10YR", "5YR", "7.5YR", "10YR")
  expect_true(bu(p, part = "upper_half"))
  expect_false(bu(p))
  # the regime: border recorded FALSE fails the subgroup
  expect_false(isTRUE(.c376(ex("M5.4.8"))$levels$code[4] == "M5.4.4"))
})


# --- the decisions of the review (Hugo, 2026-11-08) -------------------------------------------------

.mod376 <- function(code, rows = NULL, set = list(), site = list()) {
  p <- make_cst2001_example(code); h <- as.data.frame(p$horizons)
  if (!is.null(rows)) h <- h[rows, , drop = FALSE]
  for (k in names(set)) h[[k]] <- rep_len(set[[k]], nrow(h))
  s <- p$site; for (k in names(site)) s[k] <- list(site[[k]])
  soilKey:::.cst_ex_build(h, s)
}

test_that("review 1: the nine defects fixed", {
  skip_on_cran()
  # 1. shajiang in a run: a description ending at 60 cm leaves 50-100 open
  q <- .mod376("L4.3.5", rows = 1:2)
  expect_true(is.na(.f376("shajiang_cst", q, min_pct = 10, min_cm = 10, from_cm = 50, within = 100)$passed))
  # 2. phosphic evidence of N4.7.1 from the mineral soil surface
  p0 <- make_cst2001_example("N4.7.1"); h <- as.data.frame(p0$horizons)
  h$top_cm <- c(0, 20); h$bottom_cm <- c(20, 100); h$p_total_pct <- c(0.01, 0.5)
  o <- h[1, ]; o[] <- NA; o$top_cm <- 0; o$bottom_cm <- 10; o$designation <- "Oi"; o$oc_pct <- 30
  h2 <- h; h2$top_cm <- h2$top_cm + 10; h2$bottom_cm <- h2$bottom_cm + 10
  p <- soilKey:::.cst_ex_build(rbind(o, h2), p0$site)
  expect_false(isTRUE(phosphic_property_cst(p, "at_least_evidence", top_max = 25)$passed))
  expect_true(phosphic_property_cst(p, "at_least_evidence", top_max = 25, datum = "mineral")$passed)
  # 3. the upper half of the B: capped at 125 cm, open when the B goes on below the description
  mk <- function(d, top, bot, hue) soilKey:::.cst_ex_build(list(designation = d, top_cm = top, bottom_cm = bot, munsell_hue_moist = hue,
                                                                  clay_pct = 25, coarse_fragments_pct = 0), make_cst2001_example("M5.4.4")$site)
  bu <- function(p) .f376("b_upper_hue_cst", p, part = "upper_half")$passed
  expect_true(is.na(bu(mk(c("A", "Bw1"), c(0, 15), c(15, 50), c("10YR", "5YR")))))
  expect_true(bu(mk(c("A", "Bw1", "Bw2", "Bw3"), c(0, 15, 50, 90), c(15, 50, 90, 200), c("10YR", "5YR", "7.5YR", "5YR"))))
  expect_false(bu(mk(c("A", "Bw1", "Bw2", "Bw3"), c(0, 15, 75, 90), c(15, 75, 90, 200), c("10YR", "5YR", "7.5YR", "5YR"))))
  # 4. a B subhorizon measured within 125 cm, a description ending in the B open
  p <- mk(c("A", "Bw1"), c(0, 15), c(15, 70), c("10YR", "10YR")); p$horizons$fe_dcb_pct <- 0.8; p$horizons$fe_total_pct <- 3
  expect_true(is.na(.f376("b_subhorizon_cst", p, test = "ferric")$passed))
  p <- mk(c("A", "Bw1", "Bw2"), c(0, 15, 120), c(15, 120, 160), "10YR")
  p$horizons$fe_dcb_pct <- c(0.8, 0.8, 2.0); p$horizons$fe_total_pct <- 3
  expect_false(.f376("b_subhorizon_cst", p, test = "ferric")$passed)            # 5 cm inside 125 cm
  p <- .mod376("M5.1.2", set = list(bottom_cm = c(15, 60, 100, 130)))
  expect_true(.f376("dystric_cst", p, scope = "b")$passed)                       # ends on rock at 100
  # 5. Dilluvic: a layer not measured leaves a regular profile open
  p <- .mod376("H2.1.1", rows = c(1, 2, 3, 3), set = list(top_cm = c(0, 25, 60, 110), bottom_cm = c(25, 60, 110, 210),
                                                          soluble_salts_pct = c(2.8, NA, 1.5, 0.6)))
  expect_true(is.na(.f376("dilluvic_cst", p)$passed))
  # 6. Marinic, the carbonate not measured
  mr <- function(cl, so) .f376("marinic_cst", .mod376("H2.2.3", set = list(soluble_salts_chloride_pct = cl, soluble_salts_sulfate_pct = so)))$passed
  expect_true(is.na(mr(40, 15))); expect_true(mr(40, 20)); expect_false(mr(30, 40))
  # 7. M1.6.2: a calcic horizon starting at 100 cm
  p <- .mod376("M1.6.2", set = list(top_cm = c(0, 8, 100), bottom_cm = c(8, 100, 130)))
  expect_identical(.c376(p)$levels$code[4], "M1.6.2")
  # 8. Verti-lithic: a feature layer starting at 25 cm is not within 25 cm
  p <- .mod376("N4.7.4", rows = c(1, 3), set = list(top_cm = c(0, 25), bottom_cm = c(25, 45), slickensides = c(NA, NA),
                                                    cracks_width_cm = c(NA, NA)))
  expect_false(.f376("vertic_within_cst", p, within = 25)$passed)
  # 9. a contact or a horizon below the base of the description: undecided
  q <- .mod376("K3.5.10", rows = 1:2, set = list(bottom_cm = c(15, 40)))
  expect_true(is.na(.f376("contact_sg_cst", q, top_max = 50, kind = "lithic")$passed))
  q <- .mod376("G1.1.4", rows = 1:2)
  expect_true(is.na(.f376("contact_sg_cst", q, top_max = 50, datum = "surface")$passed))   # an A-G Lithic row
  q <- .mod376("H2.1.4", rows = 1:2, set = list(bottom_cm = c(25, 60)))
  expect_true(is.na(.f376("upper_within_cst", q, horizon = "gypsic_only", top_max = 100)$passed))
  expect_true(is.na(.f376("upper_within_cst", q, horizon = "salipan", top_max = 100)$passed))
  q <- .mod376("H2.2.8", rows = 1:2, set = list(bottom_cm = c(25, 50)))
  expect_true(is.na(.f376("upper_within_cst", q, horizon = "sulfuric_horizon", top_max = 100)$passed))
  q <- .mod376("H2.2.8", rows = 1, set = list(bottom_cm = 30))
  expect_true(is.na(.f376("upper_within_cst", q, horizon = "alkalic_horizon", top_max = 50, grade = "at_least_evidence")$passed))
  expect_false(.f376("upper_within_cst", .mod376("J2.1.3"), horizon = "argic_horizon", top_max = 50)$passed)
})

test_that("review 2-6: the readings decided", {
  skip_on_cran()
  # the saturation axis: a short paddy history gives no anthrostagnic regime
  p <- .mod376("M2.4.5", site = list(soil_moisture_regime = NULL, land_use = "paddy rice", paddy_years_frac = 0.4))
  expect_false(identical(soilKey:::.cst_moisture_regime(p)$saturation$candidates, "anthrostagnic"))
  p$site$paddy_years_frac <- NULL
  expect_identical(soilKey:::.cst_moisture_regime(p)$saturation$candidates, "anthrostagnic")
  # Stagnic: a flooded record leaves it undecided
  expect_true(is.na(.f376("stagnic_perched_cst", .mod376("K2.2.5", site = list(groundwater_kind = "flooded")))$passed))
  # Concretic: strongly cemented is not indurated; very hard neither
  expect_false(.f376("concretic_cst", .mod376("L4.1.1", set = list(cementation_class = c(NA, NA, "strongly", NA, NA))))$passed)
  expect_true(.f376("concretic_cst", .mod376("L4.1.1", set = list(cementation_class = NA, rupture_resistance = c(NA, NA, "extremely hard", NA, NA))))$passed)
  # Recalcaric: a description ending above 100 cm is open
  q <- .mod376("L2.4.3", rows = 1:2, set = list(bottom_cm = c(20, 40)))
  expect_true(is.na(.f376("recalcaric_cst", q, within = 100, datum = "mineral")$passed))
  # L1.1.4 / L4.3.4 read no window
  k <- load_rules("cst2001")
  l114 <- Find(function(e) e$code == "L1.1.4", k$subgroups[["L1.1"]])$tests$all_of[[1]]$dystric_cst
  expect_identical(l114$scope, "claypan")
  # the remedies named in the provisional warning
  p <- make_cst2001_example("M2.2.3"); p$site$salic_months_per_year <- NULL
  expect_true(any(grepl("the months a year with salic evidence would confirm it", .c376(p)$warnings)))
  p <- make_cst2001_example("M5.2.2"); p$site$lithic_contact_extent_pct <- NULL
  expect_true(any(grepl("the extent of the contact under the plane would confirm it", .c376(p)$warnings)))
  p <- make_cst2001_example("E1.3.7"); p$horizons$bottom_cm[3] <- 100; p$horizons$bs_pct <- c(20, 40, 40)
  expect_true(any(grepl("a deeper description would confirm it", .c376(p)$warnings)))
  # the readings of H to N are all decided
  rd <- yaml::read_yaml(system.file("rules", "cst2001", "readings.yaml", package = "soilKey"))$readings
  hn <- Filter(function(r) grepl("^Subgroups: H to N", r$topic), rd)
  expect_gte(length(hn), 14L)
  expect_false(any(vapply(hn, function(r) grepl("for Hugo to confirm", r$reading), NA)))
})
