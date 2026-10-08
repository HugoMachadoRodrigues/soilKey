# v0.9.217: WRB 2022 names from WRB 2022 Chapter 4 lists and Chapter 2.2 rules.
#
# The qualifier lists were, for most RSGs, the longer WRB 2014 lists, and the
# name was built in list order. inst/rules/wrb2022/qualifiers.yaml is now
# generated from Chapter 4 (data-raw/wrb2022_ch4_lists.py,
# data-raw/wrb2022_qualifiers_yaml.py) and resolve_wrb_qualifiers() applies the
# rules for naming soils of Chapter 2.2.

.q_lists <- function() {
  qfile <- system.file("rules/wrb2022/qualifiers.yaml", package = "soilKey")
  if (!nzchar(qfile)) qfile <- "inst/rules/wrb2022/qualifiers.yaml"
  yaml::read_yaml(qfile)$rsg_qualifiers
}

test_that("the lists are Chapter 4's (spot checks against the printed text)", {
  q <- .q_lists()
  expect_length(q, 32L)
  # Ferralsols, WRB 2022 p. 110
  expect_identical(unlist(q$FR$principal),
    c("Ferritic", "Gibbsic", "Rhodic/Xanthic", "Geric", "Nitic", "Pretic",
      "Gleyic", "Stagnic", "Profundihumic", "Mollic/Umbric", "Acric/Lixic",
      "Skeletic", "Haplic"))
  expect_true("Ferric" %in% unlist(q$FR$supplementary))
  expect_true("Humic/Ochric" %in% unlist(q$FR$supplementary))
  # Luvisols, p. 122: the list the Chapter 2.2 example is named from
  expect_identical(unlist(q$LV$principal)[1:7],
    c("Abruptic", "Fragic", "Petrocalcic", "Leptic",
      "Hydragric/Anthraquic/Irragric/Pretic/Terric", "Gleyic", "Stagnic"))
  # Haplic only where Chapter 4 lists it, always last
  has_haplic <- vapply(q, function(r) "Haplic" %in% unlist(r$principal), logical(1))
  expect_equal(sum(has_haplic), 16L)
  expect_false(has_haplic[["CM"]])
  expect_true(all(vapply(q[has_haplic], function(r) tail(unlist(r$principal), 1) == "Haplic",
                         logical(1))))
  # every name in every list has a function (Novic was the last missing)
  qs <- setdiff(unique(unlist(strsplit(unlist(q), "/", fixed = TRUE))), "Haplic")
  ns <- asNamespace("soilKey")
  expect_true(all(vapply(qs, function(n) exists(paste0("qual_", tolower(n)), envir = ns),
                         logical(1))))
})

test_that("principal qualifiers are written right to left, the first in the list next to the RSG", {
  r <- resolve_wrb_qualifiers(make_ferralsol_canonical(), "FR")
  # Chapter 4 ranks Rhodic/Xanthic (3rd) above Geric (4th): "Geric Rhodic Ferralsol"
  expect_identical(r$principal, c("Geric", "Rhodic"))
  nm <- classify_wrb2022(make_ferralsol_canonical(), on_missing = "silent")$name
  expect_match(nm, "^Geric Rhodic Ferralsol")
})

test_that("supplementary qualifiers: texture first, then alphabetical by qualifier", {
  r <- resolve_wrb_qualifiers(make_ferralsol_canonical(), "FR")
  s <- r$supplementary
  expect_identical(s[1], "Clayic")
  rest <- s[-1]
  expect_identical(rest, sort(rest))
  # alphabetical by the qualifier, not the subqualifier: Hypereutric sorts as Eutric
  expect_identical(soilKey:::.wrb_base_qualifier("Hypereutric", c("Eutric", "Dystric")), "Eutric")
  expect_identical(soilKey:::.wrb_base_qualifier("Epic", c("Eutric")), "Epic")
  expect_identical(soilKey:::.wrb_base_qualifier("Protic", c("Eutric")), "Protic")
})

test_that("a slash group gives at most one qualifier: Humic/Ochric", {
  for (f in c("make_ferralsol_canonical", "make_luvisol_canonical", "make_acrisol_canonical")) {
    p <- get(f)()
    nm <- classify_wrb2022(p, on_missing = "silent")$name
    expect_false(grepl("Humic", nm) && grepl("Ochric", nm), info = f)
  }
})

test_that("Eutric is not added when Calcaric applies (Chapter 2.2)", {
  expect_identical(soilKey:::.drop_redundant_qualifiers(c("Calcaric", "Eutric"),
                                                         c("Calcaric", "Eutric")),
                   "Calcaric")
  expect_identical(soilKey:::.drop_redundant_qualifiers(c("Eutric"), c("Eutric", "Dolomitic")),
                   character(0))
  expect_identical(soilKey:::.drop_redundant_qualifiers(c("Eutric"), "Eutric"), "Eutric")
})

test_that("Epic, Endic, Dorsic read the RSG's own diagnostic horizon", {
  f <- make_ferralsol_canonical()
  top <- min(f$horizons$top_cm[ferralic(f)$layers])
  ep <- soilKey:::qual_epic(f, rsg_code = "FR")
  ed <- soilKey:::qual_endic(f, rsg_code = "FR")
  ds <- soilKey:::qual_dorsic(f, rsg_code = "FR")
  expect_identical(isTRUE(ep$passed), top <= 50)
  expect_identical(isTRUE(ed$passed), top > 50 && top <= 100)
  expect_false(isTRUE(ds$passed))
  # without the RSG they cannot be told
  expect_true(is.na(soilKey:::qual_epic(f)$passed))
  # Dorsic only in Cryosols, Ferralsols and Podzols
  expect_true(is.na(soilKey:::qual_dorsic(f, rsg_code = "LV")$passed))
  # a deep horizon: shift the profile down 120 cm
  h <- f$horizons; h$top_cm <- h$top_cm + 120; h$bottom_cm <- h$bottom_cm + 120
  h <- rbind(data.table::data.table(top_cm = 0, bottom_cm = 120, designation = "C",
                                    clay_pct = 5, cec_cmol = 8), h, fill = TRUE)
  deep <- PedonRecord$new(site = f$site, horizons = h)
  if (isTRUE(ferralic(deep)$passed))
    expect_true(soilKey:::qual_dorsic(deep, rsg_code = "FR")$passed)
})

test_that("a qualifier's own function is tried before reading it as specifier + qualifier", {
  # Epic was read as Epi- + "c" and never evaluated
  ev <- soilKey:::.evaluate_qualifier(make_ferralsol_canonical(), "Epic", "FR")
  expect_false(is.null(ev$passed))
  expect_null(ev$trace_entry$specifier)
})

test_that("Novic: a 5-50 cm layer over a buried soil", {
  h <- data.table::data.table(top_cm = c(0, 30, 60), bottom_cm = c(30, 60, 120),
                              designation = c("C", "Ab", "Bwb"))
  p <- PedonRecord$new(site = list(id = "n"), horizons = h)
  expect_true(soilKey:::qual_novic(p)$passed)
  h$top_cm <- c(0, 60, 90); h$bottom_cm <- c(60, 90, 120)
  expect_false(soilKey:::qual_novic(PedonRecord$new(site = list(id = "n"), horizons = h))$passed)
  h$designation <- c("A", "Bw", "C")
  expect_false(soilKey:::qual_novic(PedonRecord$new(site = list(id = "n"), horizons = h))$passed)
  h$designation <- NA_character_
  expect_true(is.na(soilKey:::qual_novic(PedonRecord$new(site = list(id = "n"), horizons = h))$passed))
})

test_that("a hydragric horizon needs an anthraquic horizon above it", {
  for (f in c("make_gleysol_canonical", "make_stagnosol_canonical")) {
    p <- get(f)()
    expect_false(isTRUE(hydragric(p)$passed), info = f)
    expect_false(grepl("Hydragric", classify_wrb2022(p, on_missing = "silent")$name), info = f)
  }
  h <- data.table::data.table(top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 100),
                              designation = c("Apg", "Bg", "C"))
  paddy <- PedonRecord$new(site = list(id = "p"), horizons = h)
  expect_true(isTRUE(hydragric(paddy)$passed))
})

test_that("Toxic needs a recorded contamination, not salts or acidity", {
  sc <- make_solonchak_canonical()
  expect_false(isTRUE(soilKey:::qual_toxic(sc)$passed))
  expect_false(grepl("Toxic", classify_wrb2022(sc, on_missing = "silent")$name))
  h <- sc$horizons; h$contamination_type <- c("heavy metals (Pb)", rep(NA, nrow(h) - 1))
  expect_true(soilKey:::qual_toxic(PedonRecord$new(site = sc$site, horizons = h))$passed)
})

test_that("a soil with no principal qualifier keeps its supplementary ones", {
  # 16 RSGs list no Haplic; the name is still formatted, not the bare key name
  nm <- classify_wrb2022(make_technosol_canonical(), on_missing = "silent")$name
  expect_false(identical(nm, "Technosols"))
  expect_match(nm, "Technosol")
})


# ---- every example profile's WRB name, pinned -------------------------------
#
# The 44 example profiles named under the Chapter 4 lists and Chapter 2.2 rules
# (v0.9.217), after the Chapter 5 check of the qualifiers that reach names for
# the first time. Any change to a list, a rule or a qualifier shows up here.
# v0.9.220 (Chapter 3 diagnostics): Albic leaves the Acrisol, Alisol, Lixisol,
# Luvisol and Cryosol (their E is 6/3 dry, not claric material, which needs the
# dry and the moist colour); Nitic leaves the Plinthosols (a nitic horizon is
# not part of a plinthic one); the Retisol's argic horizon is no longer voided
# by its glossic designation.
test_that("the 44 example profiles keep their WRB 2022 names", {
  expected <- c(
    acrisol_canonical = "Chromic Acrisol (Loamic, Cutanic, Differentic, Epic, Geric, Ochric, Profondic)",
    alisol_canonical = "Haplic Alisol (Loamic, Hyperalic, Cutanic, Differentic, Epic, Ochric, Profondic)",
    andosol_canonical = "Eutric Umbric Hydric Vitric Silandic Andosol (Loamic, Humic, Mulmic)",
    anthrosol_canonical = "Hortic Anthrosol (Loamic)",
    arenosol_canonical = "Eutric Sideralic Arenosol (Claric, Ochric)",
    argissolo_canonical = "Rhodic Ferralic Acrisol (Clayic, Cutanic, Differentic, Epic, Ochric, Profondic)",
    calcisol_canonical = "Cambic Calcisol (Loamic, Epic, Ochric)",
    cambisol_canonical = "Eutric Cambisol (Loamic, Ochric)",
    cambissolo_canonical = "Eutric Cambisol (Loamic, Ochric)",
    chernossolo_canonical = "Vermic Cambic Chernozem (Loamic, Humic, Pachic)",
    chernozem_canonical = "Vermic Cambic Chernozem (Loamic, Humic, Pachic)",
    cryosol_canonical = "Skeletic Cambic Cryosol (Loamic, Epic, Humic)",
    durisol_canonical = "Eutric Skeletic Durisol (Loamic, Epic, Ochric)",
    espodossolo_canonical = "Albic Podzol (Arenic, Epic)",
    ferralsol_canonical = "Geric Rhodic Ferralsol (Clayic, Epic, Eutric, Ferric, Humic)",
    fluvisol_canonical = "Pantofluvic Fluvisol (Loamic, Humic)",
    gleissolo_canonical = "Eutric Oxygleyic Gleysol (Loamic, Ochric)",
    gleysol_canonical = "Eutric Oxygleyic Gleysol (Loamic, Ochric)",
    gypsisol_canonical = "Calcaric Cambic Gypsisol (Loamic, Epic, Ochric)",
    histosol_canonical = "Ombric Drainic Sapric Floatic Folic Histosol (Dystric, Mulmic)",
    kastanozem_canonical = "Cambic Kastanozem (Loamic, Humic)",
    latossolo_canonical = "Geric Rhodic Ferralsol (Clayic, Epic, Eutric, Ferric, Humic)",
    leptosol_canonical = "Skeletic Lithic Leptosol (Ochric)",
    lixisol_canonical = "Chromic Lixisol (Loamic, Cutanic, Differentic, Epic, Hypereutric, Ochric, Profondic)",
    luvisol_canonical = "Haplic Luvisol (Loamic, Cutanic, Differentic, Epic, Hypereutric, Ochric)",
    luvissolo_canonical = "Haplic Luvisol (Clayic, Cutanic, Differentic, Epic, Hypereutric, Ochric, Profondic)",
    neossolo_canonical = "Umbric Leptosol",
    nitisol_canonical = "Eutric Luvic Ferritic Nitisol (Epic, Ferric, Humic)",
    nitossolo_canonical = "Rhodic Ferralsol (Clayic, Epic, Ferric, Humic)",
    organossolo_canonical = "Ombric Folic Histosol (Mulmic)",
    phaeozem_canonical = "Cambic Phaeozem (Loamic, Humic, Pachic)",
    planosol_canonical = "Eutric Luvic Albic Planosol (Loamic, Ochric)",
    planossolo_canonical = "Oxygleyic Umbric Gleysol (Clayic, Abruptic, Luvic)",
    plinthosol_canonical = "Haplic Plinthosol (Loamic, Epic, Eutric, Ochric)",
    plintossolo_canonical = "Haplic Plinthosol (Loamic, Epic, Eutric, Ochric)",
    podzol_canonical = "Albic Podzol (Arenic, Epic)",
    retisol_canonical = "Eutric Albic Retisol (Loamic, Cutanic, Differentic, Epic, Ochric, Profondic)",
    solonchak_canonical = "Sodic Solonchak (Loamic, Ochric)",
    solonetz_canonical = "Albic Solonetz (Loamic, Columnic, Cutanic, Differentic, Epic, Hypernatric, Ochric)",
    stagnosol_canonical = "Albic Stagnosol (Loamic, Cambic, Ochric)",
    technosol_canonical = "Technosol (Loamic, Humic, Irragric, Mollic, Terric)",
    umbrisol_canonical = "Cambic Umbrisol (Loamic, Eutric, Humic)",
    vertisol_canonical = "Haplic Vertisol (Calcaric, Epic, Ochric)",
    vertissolo_canonical = "Haplic Vertisol (Calcaric, Epic, Ochric)"
  )
  for (f in names(expected)) {
    p <- get(paste0("make_", f), envir = asNamespace("soilKey"))()
    expect_identical(suppressWarnings(classify_wrb2022(p, on_missing = "silent")$name),
                     expected[[f]], info = f)
  }
})
