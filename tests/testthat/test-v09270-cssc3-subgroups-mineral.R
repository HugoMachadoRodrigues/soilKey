# v0.9.270: the subgroups of the Podzolic, Gleysolic, Brunisolic and
# Regosolic orders of the Canadian System of Soil Classification, 3rd ed.
# (Key to Soil Subgroups, Ch 3, pp. 40-51).

.sm <- function(h, site = list()) {
  site <- utils::modifyList(list(id = "t", country = "CA"), site)
  n <- max(lengths(h))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  p <- PedonRecord$new(site = site, horizons = ensure_horizon_schema(
    data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
  classify_cssc3(p, on_missing = "silent")
}

test_that("the rules list 67 subgroups under the twelve great groups of D, E, I, J", {
  r <- load_rules("cssc3")
  gg <- c("DA", "DB", "DC", "EA", "EB", "EC", "IA", "IB", "IC", "ID", "JA", "JB")
  expect_true(all(gg %in% names(r$subgroups)))
  expect_identical(sum(lengths(r$subgroups[gg])), 67L)
  # the Sombric Brunisols get unique codes although the book prints ICB twice
  expect_identical(vapply(r$subgroups$IC, `[[`, "", "code"), c("ICA", "ICB", "ICC", "ICD", "ICE"))
})

test_that("the example pedons of the four orders reach a subgroup", {
  skip_on_cran()
  want <- c(podzolic = "Orthic Humo-Ferric Podzol", gleysolic = "Orthic Humic Gleysol",
            brunisolic = "Orthic Eutric Brunisol", regosolic = "Orthic Regosol")
  for (o in names(want)) {
    r <- classify_cssc3(make_cssc3_example(o), on_missing = "silent")
    expect_identical(r$name, want[[o]], info = o)
    expect_length(r$provisional_because, 0L)
  }
})

podzol <- function(designation = c("LFH", "Ae", "Bf", "BC", "C"), ...) {
  h <- list(designation = designation, top_cm = c(0, 5, 12, 35, 60), bottom_cm = c(5, 12, 35, 60, 100),
            oc_pct = c(40, 0.6, 1.5, 0.4, 0.2), clay_pct = c(NA, 5, 6, 5, 5),
            sand_pct = c(NA, 78, 76, 80, 82), fe_pyro_pct = c(NA, 0.05, 0.5, 0.1, 0.05),
            al_pyro_pct = c(NA, 0.04, 0.4, 0.1, 0.05),
            munsell_hue_moist = c(NA, "10YR", "5YR", "7.5YR", "10YR"),
            munsell_value_moist = c(NA, 6, 4, 4, 5), munsell_chroma_moist = c(NA, 2, 6, 4, 3))
  utils::modifyList(h, list(...))
}

test_that("Podzolic subgroups: cemented horizons, fragipan, mottles", {
  skip_on_cran()
  expect_identical(.sm(podzol())$name, "Orthic Humo-Ferric Podzol")
  expect_identical(.sm(podzol(c("LFH", "Ae", "Bfc", "BC", "C")))$name, "Ortstein Humo-Ferric Podzol")
  expect_identical(.sm(podzol(cementation_class = c(NA, "none", "strongly", "none", "none")))$name,
                   "Ortstein Humo-Ferric Podzol")
  expect_identical(.sm(podzol(c("LFH", "Ae", "Bfc", "BCgj", "C"),
                              mottle_contrast = c(NA, NA, NA, "distinct", NA)))$name,
                   "Gleyed Ortstein Humo-Ferric Podzol")
  expect_identical(.sm(podzol(c("LFH", "Ae", "Bf", "BCc", "C")))$name, "Duric Humo-Ferric Podzol")
  expect_identical(.sm(podzol(c("LFH", "Ae", "Bf", "BCx", "C")))$name, "Fragic Humo-Ferric Podzol")
  expect_identical(.sm(podzol(c("LFH", "Ae", "Bf", "BC", "Cgj")))$name, "Gleyed Humo-Ferric Podzol")
  # a 1 cm cemented Bfc under the Bf is a placic horizon
  pl <- list(designation = c("LFH", "Ae", "Bf", "Bfc", "BC", "C"), top_cm = c(0, 5, 12, 34, 35, 60),
             bottom_cm = c(5, 12, 34, 35, 60, 100), oc_pct = c(40, 0.6, 1.5, 1.5, 0.4, 0.2),
             clay_pct = c(NA, 5, 6, 6, 5, 5), sand_pct = c(NA, 78, 76, 76, 80, 82),
             fe_pyro_pct = c(NA, 0.05, 0.5, 0.5, 0.1, 0.05), al_pyro_pct = c(NA, 0.04, 0.4, 0.4, 0.1, 0.05),
             munsell_hue_moist = c(NA, "10YR", "5YR", "5YR", "7.5YR", "10YR"),
             munsell_value_moist = c(NA, 6, 4, 4, 4, 5), munsell_chroma_moist = c(NA, 2, 6, 6, 4, 3))
  expect_identical(.sm(pl)$name, "Placic Humo-Ferric Podzol")
  # Sombric: an Ah of 12 cm over the Bf
  expect_identical(.sm(list(designation = c("Ah", "Bf", "BC", "C"), top_cm = c(0, 12, 35, 60),
                            bottom_cm = c(12, 35, 60, 100), oc_pct = c(5, 1.5, 0.4, 0.2),
                            clay_pct = c(8, 6, 5, 5), sand_pct = c(70, 76, 80, 82),
                            fe_pyro_pct = c(0.1, 0.5, 0.1, 0.05), al_pyro_pct = c(0.1, 0.4, 0.1, 0.05),
                            munsell_hue_moist = c("10YR", "5YR", "7.5YR", "10YR"),
                            munsell_value_moist = c(2, 4, 4, 5), munsell_chroma_moist = c(1, 6, 4, 3)))$name,
                   "Sombric Humo-Ferric Podzol")
  # without designations the cementation is unknown: Ortstein undecided
  r <- .sm(podzol(designation = NA_character_))
  expect_identical(r$rsg_or_order, "Podzolic")
  expect_true("subgroups:DCA" %in% r$provisional_because)
})

test_that("Gleysolic subgroups", {
  skip_on_cran()
  gl <- function(designation, ...) utils::modifyList(list(designation = designation,
    top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 100), munsell_hue_moist = c("10YR", "2.5Y", "5Y"),
    munsell_value_moist = c(2, 5, 5), munsell_chroma_moist = c(1, 1, 2), clay_pct = c(20, 22, 20),
    oc_pct = c(5, 0.8, 0.4)), list(...))
  expect_identical(.sm(gl(c("Ah", "Bg", "Ckg")))$name, "Orthic Humic Gleysol")
  expect_identical(.sm(gl(c("Ah", "Bgss", "Ckg")))$name, "Vertic Humic Gleysol")
  expect_identical(.sm(gl(c("Ah", "Bgf", "Ckg")))$name, "Fera Humic Gleysol")
  expect_identical(.sm(gl(c("Ah", "Bg", "Ckg"), fe_dcb_pct = c(1, 2.6, 1.2)))$name, "Fera Humic Gleysol")
  expect_identical(.sm(gl(c("Ah", "Cg", "Ckg")))$name, "Rego Humic Gleysol")
  expect_identical(.sm(list(designation = c("Ah", "Bng", "Csg"), top_cm = c(0, 5, 35), bottom_cm = c(5, 35, 80),
                            oc_pct = c(3, 0.8, 0.3), munsell_hue_moist = c("10YR", "2.5Y", "5Y"),
                            munsell_value_moist = c(3, 5, 5), munsell_chroma_moist = c(2, 1, 1),
                            ca_cmol = c(12, 6, 8), na_cmol = c(0.5, 4, 6),
                            structure_type = c("granular", "columnar", "massive"),
                            rupture_resistance = c("soft", "very hard", "hard"),
                            clay_pct = c(18, 32, 28)))$name, "Solonetzic Gleysol")
  lg <- function(designation) list(designation = designation, top_cm = c(0, 15, 50, 80),
    bottom_cm = c(15, 50, 80, 110), clay_pct = c(12, 28, 26, 24),
    clay_films_amount = c("none", "common", NA, NA), munsell_hue_moist = c("10YR", "2.5Y", "2.5Y", "5Y"),
    munsell_value_moist = 5, munsell_chroma_moist = 1, oc_pct = c(1, 0.5, 0.3, 0.2))
  expect_identical(.sm(lg(c("Aeg", "Btg", "BCg", "Ckg")))$name, "Orthic Luvic Gleysol")
  expect_identical(.sm(lg(c("Aeg", "Btg", "Btgx", "Ckg")))$name, "Fragic Luvic Gleysol")
  expect_identical(.sm(lg(c("Aeg", "Btgf", "BCg", "Ckg")))$name, "Fera Luvic Gleysol")
})

test_that("Brunisolic subgroups", {
  skip_on_cran()
  br <- function(designation = c("Ah", "Bm", "C"), top = c(0, 8, 40), bottom = c(8, 40, 90),
                 ph = c(5.6, 5.9, 6.4)[seq_along(designation)], ...)
    utils::modifyList(list(designation = designation, top_cm = top, bottom_cm = bottom,
      munsell_hue_moist = "10YR", munsell_value_moist = c(3, 4, 5, 5)[seq_along(designation)],
      munsell_chroma_moist = c(3, 4, 3, 3)[seq_along(designation)], clay_pct = 14,
      oc_pct = c(3, 0.8, 0.3, 0.3)[seq_along(designation)], ph_cacl2 = ph), list(...))
  expect_identical(.sm(br())$name, "Orthic Eutric Brunisol")
  expect_identical(.sm(br(c("Ah", "Aej", "Bm", "C"), c(0, 6, 9, 40), c(6, 9, 40, 90),
                          ph = c(5.6, 5.7, 5.9, 6.4)))$name, "Eluviated Eutric Brunisol")
  expect_identical(.sm(br(c("Ah", "Bmgj", "C")))$name, "Gleyed Eutric Brunisol")
  expect_identical(.sm(br(c("Ah", "Bm", "BC", "Cg"), c(0, 8, 40, 60), c(8, 40, 60, 100),
                          ph = c(5.6, 5.9, 6.2, 6.4)))$name, "Gleyed Eutric Brunisol")
  expect_identical(.sm(br(c("Ah", "Bm", "BCc", "C"), c(0, 6, 30, 60), c(6, 30, 60, 100),
                          ph = c(4.5, 4.8, 5, 5)))$name, "Duric Dystric Brunisol")
  expect_identical(.sm(br(c("Ah", "Ae", "Bm", "C"), c(0, 12, 15, 40), c(12, 15, 40, 90),
                          ph = c(4.5, 4.6, 4.8, 5)))$name, "Eluviated Sombric Brunisol")
})

test_that("Regosolic subgroups: cumulic layering and mottles", {
  skip_on_cran()
  rg <- function(designation, value, oc) list(designation = designation,
    top_cm = c(0, 15, 35, 60), bottom_cm = c(15, 35, 60, 100), munsell_hue_moist = "10YR",
    munsell_value_moist = value, munsell_chroma_moist = 2, clay_pct = 10, oc_pct = oc)
  expect_identical(.sm(rg(c("Ah", "C1", "C2", "C3"), c(3, 5, 5, 5), c(3, 0.4, 0.3, 0.3)))$name,
                   "Orthic Humic Regosol")
  expect_identical(.sm(rg(c("Ah", "C", "Ahb", "C"), c(3, 5, 3.5, 5), c(3, 0.4, 1.8, 0.3)))$name,
                   "Cumulic Humic Regosol")
  expect_identical(.sm(rg(c("Ah", "Cgj", "Ahb", "Cgj"), c(3, 5, 3.5, 5), c(3, 0.4, 1.8, 0.3)))$name,
                   "Gleyed Cumulic Humic Regosol")
  expect_identical(.sm(rg(c("Ah", "Cgj", "C", "C"), c(3, 5, 5, 5), c(3, 0.4, 0.3, 0.3)))$name,
                   "Gleyed Humic Regosol")
  expect_identical(.sm(list(designation = c("Ah", "C"), top_cm = c(0, 6), bottom_cm = c(6, 100),
                            munsell_value_moist = c(3, 5), oc_pct = c(2, 0.3), clay_pct = 10))$name,
                   "Orthic Regosol")
})

test_that("review fixes: control section, podzolic status, Btgf, slickensides, cj", {
  skip_on_cran()
  # a fragipan, a duric and a placic layer below the control section (C at
  # 55 cm: the section ends at 1 m) do not count
  deep <- function(d5) list(designation = c("LFH", "Ae", "Bf", "BC", "C", d5),
    top_cm = c(0, 5, 12, 35, 55, 105), bottom_cm = c(5, 12, 35, 55, 105, 130),
    oc_pct = c(40, 0.6, 1.5, 0.4, 0.2, 0.2), clay_pct = c(NA, 5, 6, 5, 5, 5),
    sand_pct = c(NA, 78, 76, 80, 82, 82), fe_pyro_pct = c(NA, 0.05, 0.5, 0.1, 0.05, 0.05),
    al_pyro_pct = c(NA, 0.04, 0.4, 0.1, 0.05, 0.05),
    munsell_hue_moist = c(NA, "10YR", "5YR", "7.5YR", "10YR", "10YR"),
    munsell_value_moist = c(NA, 6, 4, 4, 5, 5), munsell_chroma_moist = c(NA, 2, 6, 4, 3, 3))
  expect_identical(.sm(deep("Cx"))$name, "Orthic Humo-Ferric Podzol")
  expect_identical(.sm(deep("Cc"))$name, "Orthic Humo-Ferric Podzol")
  # a weak cj is not strongly cemented
  expect_identical(.sm(podzol(c("LFH", "Ae", "Bfcj", "BC", "C")))$name, "Orthic Humo-Ferric Podzol")
  # a cemented layer of unknown podzolic status: ortstein or duric undecided
  r <- .sm(podzol(designation = NA_character_,
                  cementation_class = c(NA, "none", NA, "strongly", "none"),
                  fe_pyro_pct = c(NA, 0.05, 0.5, NA, 0.05), al_pyro_pct = c(NA, 0.04, 0.4, NA, 0.05)))
  expect_true(is.na(r$trace$subgroups$DCD$passed) || "subgroups:DCD" %in% r$provisional_because ||
              any(c("subgroups:DCA", "subgroups:DCB") %in% r$provisional_because))
  expect_false(identical(r$name, "Duric Humo-Ferric Podzol") && length(r$provisional_because) == 0L)
  # a Btgf whose measured Fe is not 1 point above the parent material
  lg <- list(designation = c("Aeg", "Btgf", "BCg", "Ckg"), top_cm = c(0, 15, 50, 80),
             bottom_cm = c(15, 50, 80, 110), clay_pct = c(12, 28, 26, 24),
             clay_films_amount = c("none", "common", NA, NA),
             munsell_hue_moist = c("10YR", "2.5Y", "2.5Y", "5Y"), munsell_value_moist = 5,
             munsell_chroma_moist = 1, oc_pct = c(1, 0.5, 0.3, 0.2), fe_dcb_pct = c(1, 1.4, 1.3, 1.2))
  expect_identical(.sm(lg)$name, "Orthic Luvic Gleysol")
  # slickensides recorded only as present are not a slickenside horizon
  gl <- list(designation = c("Ah", "Bg", "Ckg"), top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 100),
             munsell_hue_moist = c("10YR", "2.5Y", "5Y"), munsell_value_moist = c(2, 5, 5),
             munsell_chroma_moist = c(1, 1, 2), clay_pct = c(20, 22, 20), oc_pct = c(5, 0.8, 0.4),
             slickensides = c(NA, "present", NA))
  expect_identical(.sm(gl)$name, "Orthic Humic Gleysol")
  # one layer below the Ah: nothing to vary, not cumulic
  r <- .sm(list(designation = NA_character_, top_cm = c(0, 15), bottom_cm = c(15, 100),
                munsell_value_moist = c(3, 5), oc_pct = c(3, 0.3), clay_pct = 10))
  expect_false("subgroups:JAB" %in% r$provisional_because)
})

test_that("second review fixes: cementation words, organic layers, uncertain C, buried A", {
  skip_on_cran()
  # "extremely weakly cemented" is the weakest class, not a strong one
  expect_identical(.sm(podzol(cementation_class = c(NA, "none", "extremely weakly cemented", "none", "none")))$name,
                   "Orthic Humo-Ferric Podzol")
  # no designations, every mineral horizon recorded not cemented: the blank
  # LFH is not a possible ortstein
  nd <- podzol(designation = NA_character_, cementation_class = c(NA, "none", "none", "none", "none"))
  r <- .sm(nd)
  expect_false(any(c("subgroups:DCA", "subgroups:DCB", "subgroups:DCC") %in% r$provisional_because))
  # no C designated in a fully designated profile: the C lies below 130 cm,
  # so a BCx at 101 cm is inside the control section
  full <- list(designation = c("LFH", "Ae", "Bf", "BC", "BCx"), top_cm = c(0, 5, 12, 35, 101),
               bottom_cm = c(5, 12, 35, 101, 130), oc_pct = c(40, 0.6, 1.5, 0.4, 0.2),
               clay_pct = c(NA, 5, 6, 5, 5), sand_pct = c(NA, 78, 76, 80, 82),
               fe_pyro_pct = c(NA, 0.05, 0.5, 0.1, 0.05), al_pyro_pct = c(NA, 0.04, 0.4, 0.1, 0.05),
               munsell_hue_moist = c(NA, "10YR", "5YR", "7.5YR", "10YR"),
               munsell_value_moist = c(NA, 6, 4, 4, 5), munsell_chroma_moist = c(NA, 2, 6, 4, 3))
  expect_identical(.sm(full)$name, "Fragic Humo-Ferric Podzol")
  # an undesignated horizon above the C may be the C: the Cx at 105 cm is undecided
  unc <- full
  unc$designation <- c("LFH", "Ae", "Bf", "BC", NA, "Cx")
  unc$top_cm <- c(0, 5, 12, 35, 60, 105); unc$bottom_cm <- c(5, 12, 35, 60, 105, 140)
  for (k in c("oc_pct", "clay_pct", "sand_pct", "fe_pyro_pct", "al_pyro_pct",
              "munsell_hue_moist", "munsell_value_moist", "munsell_chroma_moist"))
    unc[[k]] <- c(unc[[k]], unc[[k]][5])
  expect_true("subgroups:DCE" %in% .sm(unc)$provisional_because)
  # a buried Ahb is not part of the surface A, and makes the soil cumulic
  r <- .sm(list(designation = c("Ah", "Ahb", "C"), top_cm = c(0, 6, 20), bottom_cm = c(6, 20, 100),
                munsell_value_moist = c(3, 3, 5), oc_pct = c(3, 2.5, 0.3), clay_pct = 10))
  expect_identical(r$levels$name[2], "Regosol")
  expect_identical(r$name, "Cumulic Regosol")
})
