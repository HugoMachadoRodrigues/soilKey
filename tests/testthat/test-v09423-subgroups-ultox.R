# v0.9.423: the subgroups of the Ultisols and Oxisols rebuilt from the Keys
# to Soil Taxonomy, 13th ed. (Oxisols pp. 295-309, Ultisols pp. 321-341;
# audit items R3 to R6, O7, F16, F17, G7). Small pedons built here; no
# third-party data.

# The KST lists, per great group: the great group and its subgroups, in KST
# order (the name is "<subgroup> <great group>").
.kst13_oxisol_subgroups <- list(
  EAA = list("Acraquox", c("Plinthic", "Aeric", "Typic")),
  EAB = list("Plinthaquox", c("Aeric", "Typic")),
  EAC = list("Eutraquox", c("Histic", "Plinthic", "Aeric", "Humic", "Typic")),
  EAD = list("Haplaquox", c("Histic", "Plinthic", "Aeric", "Humic", "Typic")),
  EBA = list("Acrotorrox", c("Petroferric", "Lithic", "Typic")),
  EBB = list("Eutrotorrox", c("Petroferric", "Lithic", "Typic")),
  EBC = list("Haplotorrox", c("Petroferric", "Lithic", "Typic")),
  ECA = list("Sombriustox", c("Petroferric", "Lithic", "Humic", "Typic")),
  ECB = list("Acrustox", c(
    "Aquic Petroferric", "Petroferric", "Aquic Lithic", "Lithic",
    "Anionic Aquic", "Anionic", "Plinthic", "Aquic", "Eutric", "Humic Rhodic",
    "Humic Xanthic", "Humic", "Rhodic", "Xanthic", "Typic")),
  ECC = list("Eutrustox", c(
    "Aquic Petroferric", "Petroferric", "Aquic Lithic", "Lithic", "Plinthaquic",
    "Plinthic", "Aquic", "Kandiustalfic", "Humic Inceptic", "Inceptic",
    "Humic Rhodic", "Humic Xanthic", "Humic", "Rhodic", "Xanthic", "Typic")),
  ECD = list("Kandiustox", c(
    "Aquic Petroferric", "Petroferric", "Aquic Lithic", "Lithic", "Plinthaquic",
    "Plinthic", "Aquic", "Humic Rhodic", "Humic Xanthic", "Humic", "Rhodic",
    "Xanthic", "Typic")),
  ECE = list("Haplustox", c(
    "Aquic Petroferric", "Petroferric", "Aquic Lithic", "Lithic", "Plinthaquic",
    "Plinthic", "Aqueptic", "Aquic", "Oxyaquic", "Inceptic", "Humic Rhodic",
    "Humic Xanthic", "Humic", "Rhodic", "Xanthic", "Typic")),
  EDA = list("Sombriperox", c("Petroferric", "Lithic", "Humic", "Typic")),
  EDB = list("Acroperox", c(
    "Aquic Petroferric", "Petroferric", "Aquic Lithic", "Lithic", "Anionic",
    "Plinthic", "Aquic", "Humic Rhodic", "Humic Xanthic", "Humic", "Rhodic",
    "Xanthic", "Typic")),
  EDC = list("Eutroperox", c(
    "Aquic Petroferric", "Petroferric", "Aquic Lithic", "Lithic", "Plinthaquic",
    "Plinthic", "Aquic", "Kandiudalfic", "Humic Inceptic", "Inceptic",
    "Humic Rhodic", "Humic Xanthic", "Humic", "Rhodic", "Xanthic", "Typic")),
  EDD = list("Kandiperox", c(
    "Aquic Petroferric", "Petroferric", "Aquic Lithic", "Lithic", "Plinthaquic",
    "Plinthic", "Aquic", "Andic", "Humic Rhodic", "Humic Xanthic", "Humic",
    "Rhodic", "Xanthic", "Typic")),
  EDE = list("Haploperox", c(
    "Aquic Petroferric", "Petroferric", "Aquic Lithic", "Lithic", "Plinthaquic",
    "Plinthic", "Aquic", "Andic", "Humic Rhodic", "Humic Xanthic", "Humic",
    "Rhodic", "Xanthic", "Typic")),
  EEA = list("Sombriudox", c("Petroferric", "Lithic", "Humic", "Typic")),
  EEB = list("Acrudox", c(
    "Aquic Petroferric", "Petroferric", "Aquic Lithic", "Lithic",
    "Anionic Aquic", "Anionic", "Plinthic", "Aquic", "Eutric", "Humic Rhodic",
    "Humic Xanthic", "Humic", "Rhodic", "Xanthic", "Typic")),
  EEC = list("Eutrudox", c(
    "Aquic Petroferric", "Petroferric", "Aquic Lithic", "Lithic", "Plinthaquic",
    "Plinthic", "Aquic", "Kandiudalfic", "Humic Inceptic", "Inceptic",
    "Humic Rhodic", "Humic Xanthic", "Humic", "Rhodic", "Xanthic", "Typic")),
  EED = list("Kandiudox", c(
    "Aquic Petroferric", "Petroferric", "Aquic Lithic", "Lithic", "Plinthaquic",
    "Plinthic", "Aquic", "Andic", "Humic Rhodic", "Humic Xanthic", "Humic",
    "Rhodic", "Xanthic", "Typic")),
  EEE = list("Hapludox", c(
    "Aquic Petroferric", "Petroferric", "Aquic Lithic", "Lithic", "Plinthaquic",
    "Plinthic", "Aquic", "Inceptic", "Andic", "Humic Rhodic", "Humic Xanthic",
    "Humic", "Rhodic", "Xanthic", "Typic"))
)

.kst13_ultisol_subgroups <- list(
  HAA = list("Plinthaquults", c("Kandic", "Typic")),
  HAB = list("Fragiaquults", c("Aeric", "Plinthic", "Umbric", "Typic")),
  HAC = list("Albaquults", c("Vertic", "Kandic", "Aeric", "Typic")),
  HAD = list("Kandiaquults", c(
    "Acraquoxic", "Arenic Plinthic", "Arenic Umbric", "Arenic", "Grossarenic",
    "Plinthic", "Aeric", "Umbric", "Typic")),
  HAE = list("Kanhaplaquults", c(
    "Aquandic", "Plinthic", "Aeric Umbric", "Aeric", "Umbric", "Typic")),
  HAF = list("Paleaquults", c(
    "Vertic", "Arenic Plinthic", "Arenic Umbric", "Arenic", "Grossarenic",
    "Plinthic", "Aeric", "Umbric", "Typic")),
  HAG = list("Umbraquults", c("Plinthic", "Typic")),
  HAH = list("Epiaquults", c(
    "Vertic", "Aeric Fragic", "Arenic", "Grossarenic", "Fragic", "Aeric",
    "Typic")),
  HAI = list("Endoaquults", c("Arenic", "Grossarenic", "Aeric", "Typic")),
  HBA = list("Sombrihumults", c("Typic")),
  HBB = list("Plinthohumults", c("Typic")),
  HBC = list("Kandihumults", c(
    "Andic Ombroaquic", "Ustandic", "Andic", "Aquic", "Ombroaquic", "Plinthic",
    "Ustic", "Xeric", "Anthropic", "Typic")),
  HBD = list("Kanhaplohumults", c(
    "Lithic", "Ustandic", "Andic", "Aquic", "Ombroaquic", "Ustic", "Xeric",
    "Anthropic", "Typic")),
  HBE = list("Palehumults", c(
    "Aquandic", "Andic", "Aquic", "Plinthic", "Oxyaquic", "Ustic", "Xeric",
    "Typic")),
  HBF = list("Haplohumults", c(
    "Lithic", "Aquandic", "Aquic", "Andic", "Plinthic", "Oxyaquic", "Ustic",
    "Xeric", "Typic")),
  HCA = list("Plinthudults", c("Typic")),
  HCB = list("Fragiudults", c(
    "Arenic", "Plinthaquic", "Glossaquic", "Aquic", "Plinthic", "Glossic",
    "Humic", "Typic")),
  HCC = list("Kandiudults", c(
    "Arenic Plinthaquic", "Aquic Arenic", "Arenic Plinthic", "Arenic Rhodic",
    "Arenic", "Grossarenic Plinthic", "Grossarenic", "Acrudoxic Plinthic",
    "Acrudoxic", "Plinthaquic", "Aquandic", "Andic", "Aquic", "Plinthic",
    "Ombroaquic", "Oxyaquic", "Sombric", "Rhodic", "Typic")),
  HCD = list("Kanhapludults", c(
    "Lithic", "Plinthaquic", "Arenic Plinthic", "Arenic", "Acrudoxic",
    "Fragiaquic", "Andic", "Aquic", "Ombroaquic", "Oxyaquic", "Plinthic",
    "Fragic", "Rhodic", "Typic")),
  HCE = list("Paleudults", c(
    "Vertic", "Spodic", "Arenic Plinthaquic", "Aquic Arenic", "Anthraquic",
    "Plinthaquic", "Fragiaquic", "Aquic", "Oxyaquic", "Lamellic",
    "Arenic Plinthic", "Psammentic", "Grossarenic Plinthic", "Plinthic",
    "Arenic Rhodic", "Arenic", "Grossarenic", "Fragic", "Rhodic", "Typic")),
  HCF = list("Rhodudults", c("Lithic", "Psammentic", "Typic")),
  HCG = list("Hapludults", c(
    "Lithic-Ruptic-Entic", "Lithic", "Vertic", "Fragiaquic", "Aquic Arenic",
    "Aquic", "Fragic", "Oxyaquic", "Lamellic", "Psammentic", "Arenic",
    "Grossarenic", "Inceptic", "Humic", "Typic")),
  HDA = list("Plinthustults", c("Haplic", "Typic")),
  HDB = list("Kandiustults", c(
    "Acrustoxic", "Aquic", "Arenic Plinthic", "Arenic", "Udandic", "Andic",
    "Plinthic", "Aridic", "Udic", "Rhodic", "Typic")),
  HDC = list("Kanhaplustults", c(
    "Lithic", "Acrustoxic", "Aquic", "Arenic", "Udandic", "Andic", "Plinthic",
    "Ombroaquic", "Aridic", "Udic", "Rhodic", "Typic")),
  HDD = list("Paleustults", c("Typic")),
  HDE = list("Rhodustults", c("Lithic", "Psammentic", "Typic")),
  HDF = list("Haplustults", c(
    "Lithic", "Petroferric", "Aquic", "Arenic", "Ombroaquic", "Plinthic",
    "Kanhaplic", "Typic")),
  HEA = list("Palexerults", c("Aquandic", "Aquic", "Andic", "Typic")),
  HEB = list("Haploxerults", c(
    "Lithic Ruptic-Inceptic", "Lithic", "Aquic", "Andic", "Lamellic",
    "Psammentic", "Arenic", "Grossarenic", "Typic"))
)

.v09423_yaml <- function(order) {
  f <- system.file("rules", "usda", "subgroups", paste0(order, ".yaml"), package = "soilKey")
  yaml::read_yaml(f)$subgroups
}

test_that("each Oxisol and Ultisol great group lists the KST subgroups, in KST order", {
  for (o in list(list("oxisols", .kst13_oxisol_subgroups),
                 list("ultisols", .kst13_ultisol_subgroups))) {
    y <- .v09423_yaml(o[[1]])
    kst <- o[[2]]
    expect_setequal(names(y), names(kst))
    for (gg in names(kst)) {
      want <- paste(kst[[gg]][[2]], kst[[gg]][[1]])
      got <- vapply(y[[gg]], `[[`, "", "name")
      expect_identical(got, want, label = paste(o[[1]], gg))
      # Typic last, as the default; codes are the KST codes (great group + letter)
      last <- y[[gg]][[length(y[[gg]])]]
      expect_true(isTRUE(last$tests$default), label = paste(gg, "Typic default"))
      codes <- vapply(y[[gg]], `[[`, "", "code")
      expect_identical(codes, paste0(gg, LETTERS[seq_along(codes)]), label = paste(gg, "codes"))
    }
  }
  expect_equal(sum(lengths(lapply(.kst13_oxisol_subgroups, `[[`, 2))), 213L)
  expect_equal(sum(lengths(lapply(.kst13_ultisol_subgroups, `[[`, 2))), 215L)
})

test_that("a compound subgroup comes before its parts, so it can be reached", {
  for (o in c("oxisols", "ultisols")) for (g in .v09423_yaml(o)) {
    nm <- vapply(g, `[[`, "", "name")
    gg <- sub(".* ", "", nm[1])
    adj <- sub(paste0(" ", gg, "$"), "", nm)
    for (i in seq_along(adj)) {
      parts <- strsplit(adj[i], " ", fixed = TRUE)[[1]]
      if (length(parts) < 2L) next
      for (pt in parts) {
        j <- match(pt, adj)
        if (!is.na(j)) expect_lt(i, j, label = paste(nm[i], "before", nm[j]))
      }
    }
  }
})

test_that("every test the two lists name exists in soilKey", {
  fns <- character(0)
  for (o in c("oxisols", "ultisols")) for (g in .v09423_yaml(o)) for (s in g)
    for (t in s$tests$all_of) fns <- c(fns, names(t))
  fns <- unique(fns)
  for (f in fns) expect_true(exists(f, envir = asNamespace("soilKey"), mode = "function"), label = f)
})

# ---- Ultisols -----------------------------------------------------------------

# A Hapludult: sandy A and E over a Bt at `top_arg` cm, clay falling by more
# than 20 % at 140 cm (not Pale-), a CEC too high for a kandic horizon.
.v09423_hapludult <- function(top_arg = 60, sand_top = 90, clay_top = 4) {
  PedonRecord$new(site = list(id = "v09423", soil_moisture_regime = "udic"),
    horizons = data.frame(designation = c("A", "E", "Bt1", "Bt2", "BC"),
      top_cm = c(0, 20, top_arg, top_arg + 40, 140),
      bottom_cm = c(20, top_arg, top_arg + 40, 140, 200),
      clay_pct = c(clay_top, clay_top, 30, 32, 20),
      sand_pct = c(sand_top, sand_top, 50, 48, 60),
      silt_pct = c(100 - sand_top - clay_top, 100 - sand_top - clay_top, 20, 20, 20),
      munsell_hue_moist = c("10YR", "10YR", "5YR", "5YR", "5YR"),
      munsell_value_moist = c(3, 5, 5, 5, 5), munsell_chroma_moist = c(2, 3, 6, 6, 6),
      munsell_value_dry = c(5, 7, 6, 6, 6),
      clay_films_amount = c(NA, NA, "common", "common", NA),
      cec_cmol = c(3, 2, 12, 12, 8), bs_pct = c(20, 15, 10, 10, 10),
      oc_pct = c(1, 0.3, 0.2, 0.1, 0.1), ph_h2o = c(5, 5, 4.8, 4.8, 4.8),
      stringsAsFactors = FALSE))
}

test_that("Arenic and Grossarenic Ultisols read the Ultisol definition (no spodic horizon)", {
  skip_on_cran()
  p <- .v09423_hapludult(top_arg = 60)
  expect_true(soilKey:::argillic_usda(p)$passed)
  expect_true(soilKey:::sg_ultisol_arenic(p)$passed)                         # 50-100 cm
  expect_false(soilKey:::sg_ultisol_arenic(p, min_cm = 100, max_cm = Inf)$passed)
  expect_false(soilKey:::sg_ultisol_arenic(.v09423_hapludult(top_arg = 30))$passed)
  expect_true(soilKey:::sg_ultisol_arenic(.v09423_hapludult(top_arg = 110), min_cm = 100, max_cm = Inf)$passed)
  # a loamy E: not sandy throughout
  q <- p; h <- q$horizons; h$sand_pct[2] <- 40; h$silt_pct[2] <- 56
  expect_false(soilKey:::sg_ultisol_arenic(PedonRecord$new(site = q$site, horizons = h))$passed)
  # texture not recorded above the Bt: undecided
  h <- p$horizons; h$sand_pct[1:2] <- NA; h$silt_pct[1:2] <- NA
  r <- soilKey:::sg_ultisol_arenic(PedonRecord$new(site = p$site, horizons = h))
  expect_true(is.na(r$passed)); expect_true(length(r$missing) > 0)
  # the old helper needed a spodic horizon and never passed outside the Spodosols
  expect_false(isTRUE(soilKey:::arenic_subgroup_usda(p)$passed))
})

test_that("an Arenic Hapludult is named so, with the undecided Vertic and Oxyaquic listed", {
  skip_on_cran()
  r <- classify_usda(.v09423_hapludult(), on_missing = "silent")
  expect_identical(r$name, "Arenic Hapludults")
  tr <- r$trace$subgroups
  expect_true(is.na(tr$HCGC$passed))   # Vertic: no COLE recorded
  expect_true(is.na(tr$HCGH$passed))   # Oxyaquic: no days of saturation
  expect_false(tr$HCGE$passed)         # Aquic Arenic: no redox features recorded
  expect_identical(r$evidence_grade, "C")
})

test_that("Vertic Ultisols: slickensides decide, the linear extensibility is a measurement", {
  skip_on_cran()
  p <- .v09423_hapludult()
  expect_true(is.na(soilKey:::sg_ultisol_vertic(p)$passed))
  h <- p$horizons; h$cole_value <- c(0.01, 0.01, 0.02, 0.02, 0.02)
  expect_false(soilKey:::sg_ultisol_vertic(PedonRecord$new(site = p$site, horizons = h))$passed)
  h$cole_value <- c(0.05, 0.05, 0.09, 0.09, 0.09)                        # LE >= 6 cm
  expect_true(soilKey:::sg_ultisol_vertic(PedonRecord$new(site = p$site, horizons = h))$passed)
  h <- p$horizons; h$slickensides <- c(NA, NA, "common", "common", NA)    # 80 cm within 125
  expect_true(soilKey:::sg_ultisol_vertic(PedonRecord$new(site = p$site, horizons = h))$passed)
})

test_that("Aquic Ultisols need recorded redox depletions in the zone of the great group", {
  skip_on_cran()
  p <- .v09423_hapludult()
  expect_false(soilKey:::sg_ultisol_aquic(p, zone = "75")$passed)        # no redox features
  h <- p$horizons
  h$redoximorphic_features_pct <- c(0, 0, 10, 0, 0)
  h$mottle_colors <- c(NA, NA, "10YR 6/2; 5YR 5/8", NA, NA)
  q <- PedonRecord$new(site = p$site, horizons = h)
  expect_true(soilKey:::sg_ultisol_aquic(q, zone = "75")$passed)         # Bt1 at 60-100 cm
  expect_true(soilKey:::sg_ultisol_aquic(q, zone = "arg25", horizon = "argillic")$passed)
  expect_false(soilKey:::sg_ultisol_aquic(q, zone = "40")$passed)
  # mottles recorded without a colour, the matrix colour not recorded: undecided
  h$mottle_colors <- NA; h$munsell_chroma_moist[3] <- NA
  expect_true(is.na(soilKey:::sg_ultisol_aquic(PedonRecord$new(site = p$site, horizons = h), zone = "75")$passed))
})

test_that("Aeric Aquults read chroma 3 or more between the A (or 25 cm) and 75 cm", {
  skip_on_cran()
  p <- .v09423_hapludult()
  expect_true(soilKey:::sg_ultisol_aeric(p)$passed)                      # E chroma 3, Bt chroma 6
  h <- p$horizons; h$munsell_chroma_moist <- c(2, 1, 2, 2, 2)
  expect_false(soilKey:::sg_ultisol_aeric(PedonRecord$new(site = p$site, horizons = h))$passed)
  h$munsell_chroma_moist[2] <- NA
  expect_true(is.na(soilKey:::sg_ultisol_aeric(PedonRecord$new(site = p$site, horizons = h))$passed))
})

test_that("Kanhaplic Haplustults: CEC under 24 cmol(+)/kg clay in half the argillic horizon", {
  skip_on_cran()
  p <- .v09423_hapludult()                                               # 12 / 30 = 40
  expect_false(soilKey:::sg_ultisol_kandic(p, "cec")$passed)
  h <- p$horizons; h$cec_cmol[3:4] <- c(6, 6)                            # 20
  expect_true(soilKey:::sg_ultisol_kandic(PedonRecord$new(site = p$site, horizons = h), "cec")$passed)
  h$cec_cmol[3:4] <- NA
  expect_true(is.na(soilKey:::sg_ultisol_kandic(PedonRecord$new(site = p$site, horizons = h), "cec")$passed))
})

test_that("Humic Hapludults read the dry value, inferred only for a moist value of 2 or less", {
  skip_on_cran()
  p <- .v09423_hapludult()
  h <- p$horizons; h$designation[1] <- "Ap"; h$munsell_value_moist[1:2] <- c(3, 3)
  h$munsell_value_dry[1:2] <- c(5, 5)
  expect_true(soilKey:::sg_ultisol_humic(PedonRecord$new(site = p$site, horizons = h))$passed)
  h$munsell_value_dry[1:2] <- NA
  expect_true(is.na(soilKey:::sg_ultisol_humic(PedonRecord$new(site = p$site, horizons = h))$passed))
  h$munsell_value_moist[1:2] <- c(2, 2)
  expect_true(soilKey:::sg_ultisol_humic(PedonRecord$new(site = p$site, horizons = h))$passed)
  h$munsell_value_moist[1:2] <- c(4, 4)
  expect_false(soilKey:::sg_ultisol_humic(PedonRecord$new(site = p$site, horizons = h))$passed)
})

test_that("the udic and aridic sides of the ustic regime are read from the subclass", {
  skip_on_cran()
  p <- .v09423_hapludult()
  expect_true(is.na(soilKey:::sg_ultisol_ustic_intergrade(p, "udic")$passed))
  s <- p$site; s$soil_moisture_regime <- "ustic"; s$soil_moisture_subclass <- "udic ustic"
  q <- PedonRecord$new(site = s, horizons = p$horizons)
  expect_true(soilKey:::sg_ultisol_ustic_intergrade(q, "udic")$passed)
  expect_false(soilKey:::sg_ultisol_ustic_intergrade(q, "aridic")$passed)
})

test_that("Haplic Plinthustults are the negation of the Pale- core test", {
  skip_on_cran()
  p <- .v09423_hapludult()                                               # clay falls 32 -> 20
  expect_true(soilKey:::sg_ultisol_haplic(p)$passed)
  h <- p$horizons; h$clay_pct[5] <- 31
  expect_false(soilKey:::sg_ultisol_haplic(PedonRecord$new(site = p$site, horizons = h))$passed)
})

# ---- Oxisols -----------------------------------------------------------------

test_that("Aquic Oxisols: depletions of value 4 or more and chroma 2 or less within 125 cm", {
  skip_on_cran()
  f <- make_ferralsol_canonical()
  expect_false(soilKey:::sg_oxisol_aquic(f)$passed)
  h <- f$horizons; h$redoximorphic_features_pct <- c(0, 0, 0, 15, 0)
  h$mottle_colors <- c(NA, NA, NA, "10YR 6/1", NA)
  expect_true(soilKey:::sg_oxisol_aquic(PedonRecord$new(site = f$site, horizons = h))$passed)
  h$mottle_colors[4] <- "10YR 3/1"                                       # too dark: value 3
  expect_false(soilKey:::sg_oxisol_aquic(PedonRecord$new(site = f$site, horizons = h))$passed)
  h$mottle_colors[4] <- NA; h$munsell_chroma_moist[4] <- NA              # undecided
  expect_true(is.na(soilKey:::sg_oxisol_aquic(PedonRecord$new(site = f$site, horizons = h))$passed))
})

test_that("Xanthic Oxisols: half the 25-125 cm thickness yellow and light", {
  skip_on_cran()
  f <- make_ferralsol_canonical()
  expect_false(soilKey:::sg_oxisol_xanthic(f)$passed)                   # 2.5YR
  h <- f$horizons; h$munsell_hue_moist <- "10YR"; h$munsell_value_moist <- c(4, 5, 6, 6, 6)
  expect_true(soilKey:::sg_oxisol_xanthic(PedonRecord$new(site = f$site, horizons = h))$passed)
  h$munsell_value_moist[4] <- 5                                          # 30 of 100 cm
  expect_false(soilKey:::sg_oxisol_xanthic(PedonRecord$new(site = f$site, horizons = h))$passed)
  h$munsell_value_moist[4] <- NA
  expect_true(is.na(soilKey:::sg_oxisol_xanthic(PedonRecord$new(site = f$site, horizons = h))$passed))
})

test_that("Anionic Oxisols: delta pH 0 or more through 18 cm within 125 cm", {
  skip_on_cran()
  f <- make_ferralsol_canonical()
  h <- f$horizons; h$ph_h2o <- 5.0; h$ph_kcl <- c(4.2, 4.3, 4.6, 5.2, 5.3)
  expect_true(soilKey:::sg_oxisol_anionic(PedonRecord$new(site = f$site, horizons = h))$passed)
  h$ph_kcl <- c(4.2, 4.3, 4.6, 4.8, 5.3)                                 # only below 130 cm
  expect_false(soilKey:::sg_oxisol_anionic(PedonRecord$new(site = f$site, horizons = h))$passed)
  h$ph_kcl[4] <- NA
  expect_true(is.na(soilKey:::sg_oxisol_anionic(PedonRecord$new(site = f$site, horizons = h))$passed))
})

test_that("Inceptic Oxisols: the oxic horizon ends within 125 cm", {
  skip_on_cran()
  f <- make_ferralsol_canonical()
  expect_false(soilKey:::sg_oxisol_inceptic(f)$passed)                   # oxic to 200 cm
  h <- as.data.frame(f$horizons)
  h <- h[1:4, ]; h$bottom_cm[4] <- 110
  expect_true(is.na(soilKey:::sg_oxisol_inceptic(PedonRecord$new(site = f$site, horizons = h))$passed))
  h2 <- rbind(h, h[4, ]); h2$designation[5] <- "R"; h2$top_cm[5] <- 110; h2$bottom_cm[5] <- 130
  h2[5, setdiff(names(h2), c("designation", "top_cm", "bottom_cm"))] <- NA
  expect_true(soilKey:::sg_oxisol_inceptic(PedonRecord$new(site = f$site, horizons = h2))$passed)
})

test_that("Eutric Oxisols need 35 % base saturation in every horizon to 125 cm", {
  skip_on_cran()
  f <- make_ferralsol_canonical()
  h <- f$horizons; h$bs_pct <- c(60, 50, 40, 36, 35)
  expect_true(soilKey:::sg_oxisol_eutric(PedonRecord$new(site = f$site, horizons = h))$passed)
  h$bs_pct[3] <- 20
  expect_false(soilKey:::sg_oxisol_eutric(PedonRecord$new(site = f$site, horizons = h))$passed)
  h$bs_pct[3] <- NA
  expect_true(is.na(soilKey:::sg_oxisol_eutric(PedonRecord$new(site = f$site, horizons = h))$passed))
})

test_that("the canonical Oxisol fixtures keep their names", {
  skip_on_cran()
  expect_identical(classify_usda(make_ferralsol_canonical(), on_missing = "silent")$name, "Typic Hapludox")
  expect_identical(classify_usda(make_latossolo_canonical(), on_missing = "silent")$name, "Typic Hapludox")
  expect_identical(classify_usda(make_nitossolo_canonical(), on_missing = "silent")$name, "Rhodic Kandiudox")
})
