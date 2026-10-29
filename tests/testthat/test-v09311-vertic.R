# v0.9.311: the vertic horizon and the Vertisols gate by the WRB 2022 text.
#   * vertic_horizon(), Ch 3.1.40 (p. 70): wedge-shaped aggregates or
#     slickensides; 25 cm of adjoining layers; the COLE is "additional
#     information" only, so the KST 13 LE path stays in reading = "usda";
#     a criterion is undecided only where it could make up the horizon.
#   * vertisol(), Ch 4 (p. 101): a vertic horizon starting <= 100 cm from the
#     mineral soil surface; >= 30 % clay above it throughout; cracks that start
#     at the surface, at the base of a plough layer or below a self-mulching
#     surface and extend to it. Missing cracks leave the gate undecided.

.v09311_pedon <- function(...) {
  PedonRecord$new(site = list(id = "v09311", lat = 0, lon = 0, country = "TEST"),
                  horizons = ensure_horizon_schema(data.table::data.table(...)))
}

# A clayey profile whose B has the given structure and slickensides
.v09311_wedge <- function(structure = "wedge-shaped angular blocky",
                          slick = "absent", clay2 = 55) {
  .v09311_pedon(
    top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 120),
    designation = c("A", "Bi", "C"),
    clay_pct = c(45, clay2, 50),
    slickensides = c("absent", slick, "absent"),
    structure_type = c("subangular blocky", structure, "massive"),
    cracks_width_cm = c(0.6, 0.6, NA))
}


# ---- vertic_horizon(), WRB 2022 Ch 3.1.40 -----------------------------------

test_that("wedge-shaped aggregates make a vertic horizon without slickensides", {
  skip_on_cran()
  p <- .v09311_wedge()
  vh <- vertic_horizon(p)
  expect_true(isTRUE(vh$passed))
  expect_equal(vh$layers, 2L)
  # the reading kept for USDA asks for slickensides
  expect_false(isTRUE(vertic_horizon(p, reading = "usda")$passed))
  # neither wedge-shaped aggregates nor slickensides: not a vertic horizon
  expect_identical(vertic_horizon(.v09311_wedge("prismatic"))$passed, FALSE)
  # neither recorded: undecided, and the missing attributes say why
  und <- vertic_horizon(.v09311_wedge(NA_character_, NA_character_))
  expect_true(is.na(und$passed))
  expect_true(all(c("slickensides", "structure_type") %in% und$missing))
})

test_that("the 25 cm of the vertic horizon are those of adjoining layers", {
  skip_on_cran()
  mk <- function(clay2 = 50) .v09311_pedon(
    top_cm = c(0, 20, 35, 50), bottom_cm = c(20, 35, 50, 60),
    designation = c("A", "Bi1", "Bi2", "Bi3"),
    clay_pct = c(45, 50, clay2, 50),
    slickensides = c("absent", "common", "many", "common"),
    structure_type = rep("angular blocky", 4),
    cracks_width_cm = rep(0.6, 4))
  vh <- vertic_horizon(mk())
  expect_true(isTRUE(vh$passed))
  expect_equal(vh$layers, 2:4)
  expect_equal(vh$evidence$thickness$runs[[1]]$thickness, 40)
  # one layer of 15 cm at most: the reading kept for USDA does not pass
  expect_false(isTRUE(vertic_horizon(mk(), reading = "usda")$passed))
  # the middle layer below 30 % clay splits them into 15 and 10 cm
  expect_identical(vertic_horizon(mk(clay2 = 20))$passed, FALSE)
})

test_that("the COLE path is the KST 13 one: reading = 'usda' only", {
  skip_on_cran()
  p <- .v09311_pedon(
    top_cm = c(0, 30, 60), bottom_cm = c(30, 60, 100),
    designation = c("A", "Bw", "C"), clay_pct = c(45, 55, 50),
    slickensides = rep("absent", 3), structure_type = rep("angular blocky", 3),
    cole_value = c(0.09, 0.10, 0.10))
  expect_identical(vertic_horizon(p)$passed, FALSE)
  expect_false("cole_value" %in% vertic_horizon(p)$missing)
  u <- vertic_horizon(p, reading = "usda")
  expect_true(isTRUE(u$passed))
  expect_true(isTRUE(u$evidence$cole_le_path$passed))
  expect_true(isTRUE(soilKey:::vertisol_qualifying_usda(p)$passed))
})

test_that("a vertic horizon is undecided only where it could be made up", {
  skip_on_cran()
  # clay not recorded in a 40 cm layer with slickensides: undecided
  p <- .v09311_wedge("prismatic", "many", clay2 = NA_real_)
  vh <- vertic_horizon(p)
  expect_true(is.na(vh$passed))
  expect_identical(vh$missing, "clay_pct")
  # the same in a 10 cm layer between decided ones: not a vertic horizon
  q <- .v09311_pedon(
    top_cm = c(0, 20, 30), bottom_cm = c(20, 30, 120),
    designation = c("A", "Bi", "C"), clay_pct = c(45, NA, 50),
    slickensides = c("absent", "many", "absent"),
    structure_type = c("subangular blocky", "prismatic", "massive"),
    cracks_width_cm = c(0.6, 0.6, NA))
  expect_identical(vertic_horizon(q)$passed, FALSE)
  # continuous rock with nothing recorded is not mineral material
  r <- .v09311_pedon(
    top_cm = c(0, 30), bottom_cm = c(30, 80), designation = c("A", "R"),
    clay_pct = c(45, NA), slickensides = c("absent", NA),
    structure_type = c("subangular blocky", NA))
  expect_identical(vertic_horizon(r)$passed, FALSE)
})


# ---- vertisol(), WRB 2022 Ch 4, p. 101 --------------------------------------

test_that("Vertisols ask for a vertic horizon starting <= 100 cm", {
  skip_on_cran()
  expect_true(isTRUE(vertisol(make_vertisol_canonical())$passed))
  # a vertic horizon from 105 cm: present, but not a Vertisol
  deep <- .v09311_pedon(
    top_cm = c(0, 30, 105), bottom_cm = c(30, 105, 160),
    designation = c("A", "Bw", "Bi"), clay_pct = c(45, 50, 55),
    slickensides = c("absent", "absent", "many"),
    structure_type = c("subangular blocky", "prismatic", "prismatic"),
    cracks_width_cm = c(0.6, 0.6, 0.6))
  expect_true(isTRUE(vertic_horizon(deep)$passed))
  expect_identical(vertisol(deep)$passed, FALSE)
  # an undescribed row below 100 cm leaves the horizon open, not the gate
  open <- .v09311_pedon(
    top_cm = c(0, 30, 110), bottom_cm = c(30, 110, 200),
    designation = c("A", "Bw", NA), clay_pct = c(45, 50, 50),
    slickensides = c("absent", "absent", NA),
    structure_type = c("subangular blocky", "prismatic", NA),
    cracks_width_cm = c(0.6, 0.6, NA))
  expect_true(is.na(vertic_horizon(open)$passed))
  expect_identical(vertisol(open)$passed, FALSE)
})

test_that("Vertisols ask for >= 30 % clay above the vertic horizon throughout", {
  skip_on_cran()
  p <- make_vertisol_canonical()
  p$horizons$clay_pct[1] <- 25
  expect_identical(vertisol(p)$passed, FALSE)
  p$horizons$clay_pct[1] <- NA
  r <- vertisol(p)
  expect_true(is.na(r$passed))
  expect_true("clay_pct" %in% r$missing)
})

test_that("cracks start at the surface, below a plough layer or a self-mulching surface", {
  skip_on_cran()
  # the canonical fixture: Aw 0-25 (strong medium subangular blocky) over the
  # vertic Bss from 25 cm
  base <- function(desig = "Aw", width = NA_real_, grade = "strong",
                   type = "subangular blocky", size = "medium") {
    p <- make_vertisol_canonical()
    h <- p$horizons
    h$designation[1] <- desig; h$cracks_width_cm[1] <- width
    h$structure_grade[1] <- grade; h$structure_type[1] <- type
    h$structure_size[1] <- size
    PedonRecord$new(site = p$site, horizons = h)
  }
  # (a) recorded at the surface, or narrower than 0.5 cm there
  expect_true(isTRUE(vertisol(base(width = 0.8))$passed))
  expect_identical(vertisol(base(width = 0.2))$passed, FALSE)
  # not recorded at the surface: undecided, not absent
  und <- vertisol(base())
  expect_true(is.na(und$passed))
  expect_true("cracks_width_cm" %in% und$missing)
  # (b) at the base of a plough layer, the top of the vertic horizon
  ap <- vertisol(base("Ap"))
  expect_true(isTRUE(ap$passed))
  expect_true(isTRUE(ap$evidence$cracks$starts$plough_layer_base))
  # (c) below a self-mulching surface: strong fine granular, or strong
  # blocky of <= 1 cm; a weak granular surface is not one
  expect_true(isTRUE(vertisol(base(type = "granular", size = "fine"))$passed))
  expect_true(isTRUE(vertisol(base(size = "fine"))$passed))
  expect_true(is.na(vertisol(base(grade = "weak", type = "granular",
                                  size = "fine"))$passed))
})

test_that("every layer between the start and the vertic horizon has cracks", {
  skip_on_cran()
  p <- .v09311_pedon(
    top_cm = c(0, 15, 40), bottom_cm = c(15, 40, 120),
    designation = c("A", "AB", "Bss"), clay_pct = c(45, 48, 55),
    slickensides = c("absent", "absent", "many"),
    structure_type = c("subangular blocky", "angular blocky", "wedge-shaped"),
    structure_grade = rep("moderate", 3),
    cracks_width_cm = c(0.6, 0.6, 0.8))
  expect_true(isTRUE(vertisol(p)$passed))
  h <- p$horizons; h$cracks_width_cm[2] <- 0
  expect_identical(vertisol(PedonRecord$new(site = p$site, horizons = h))$passed,
                   FALSE)
  h$cracks_width_cm[2] <- NA
  expect_true(is.na(vertisol(PedonRecord$new(site = p$site, horizons = h))$passed))
})
