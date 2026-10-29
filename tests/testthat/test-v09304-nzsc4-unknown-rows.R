# v0.9.304: two readings of the New Zealand Soil Classification keys where
# a mineral row of unknown role (no designation) was read one way only.
#   * The base of the A horizon: an undesignated row directly under the A
#     may be A too, so a test measured from the base of the A is read at
#     every base the A may have (sandy groups, Ultic part 1, Granular part
#     2, Pallic parts 1 and 2(a), Rocky Recent Soils, UPT).
#   * Pallic part 2(b): an undesignated row may be part of the B or BC, so a
#     B thinner than 10 cm over such a row no longer reads as failing.

.nz304_mod <- function(order, h = list(), site = list()) {
  p <- make_nzsc4_example(order)
  hz <- as.data.frame(p$horizons, stringsAsFactors = FALSE)
  if (!is.null(h$top_cm)) {
    n <- length(h$top_cm)
    hz <- hz[rep_len(seq_len(nrow(hz)), n), , drop = FALSE]
  }
  for (nm in names(h)) hz[[nm]] <- if (length(h[[nm]]) == 1L) rep(h[[nm]], nrow(hz)) else h[[nm]]
  PedonRecord$new(site = utils::modifyList(p$site, site),
                  horizons = ensure_horizon_schema(data.table::as.data.table(hz)))
}

# Recent-Soils base: Ah over a row 15-30 cm, over sand to 120 cm
.nz304_sandy <- function(des2) {
  .nz304_mod("recent", list(designation = c("Ah", des2, "C2"), top_cm = c(0, 15, 30), bottom_cm = c(15, 30, 120),
                            field_texture = c("silt loam", "loam", "sand"), clay_pct = c(15, 18, 3),
                            silt_pct = c(60, 40, 2), sand_pct = c(25, 42, 95)))
}

test_that("the base of the A runs through undesignated rows directly under it", {
  skip_on_cran()
  x <- .nzsc_horizons(.nz304_sandy(NA_character_))
  expect_identical(.nzsc_a_base_options(x), c(15, 30))
  x2 <- .nzsc_horizons(.nz304_mod("recent", list(designation = c("Ah", NA, NA), top_cm = c(0, 15, 30),
                                                 bottom_cm = c(15, 30, NA))))
  expect_identical(.nzsc_a_base_options(x2), c(15, 30, Inf))
  # a designated row stops the run
  expect_identical(.nzsc_a_base_options(.nzsc_horizons(.nz304_sandy("C1"))), 15)
  expect_identical(.nzsc_a_base_options(.nzsc_horizons(.nz304_sandy("Ah2"))), 30)
  # the verdict holds only when every base gives it
  expect_true(.nzsc_at_a_bases(x, function(z) TRUE))
  expect_true(is.na(.nzsc_at_a_bases(x, function(z) z > 20)))
  expect_false(.nzsc_at_a_bases(x, function(z) z > 40))
})

test_that("sandy groups: an undesignated loamy row under the A leaves the test open", {
  skip_on_cran()
  # before v0.9.304 the loam row was read as below the A, so not sandy
  expect_true(is.na(sandy_group_nzsc(.nz304_sandy(NA_character_))$passed))
  expect_false(sandy_group_nzsc(.nz304_sandy("C1"))$passed)
  expect_true(sandy_group_nzsc(.nz304_sandy("Ah2"))$passed)
  # from the soil surface (WS) the base of the A plays no part
  expect_false(sandy_group_nzsc(.nz304_sandy(NA_character_), from = "soil_surface")$passed)
})

test_that("Rocky Recent Soils: the gap to the contact is read at every base", {
  skip_on_cran()
  rx <- function(des2) .nz304_mod("recent", list(designation = c("Ah", des2, "R"), top_cm = c(0, 12, 25),
                                                 bottom_cm = c(12, 25, 40)))
  expect_true(is.na(rocky_recent_nzsc(rx(NA_character_))$passed))
  expect_false(rocky_recent_nzsc(rx("C1"))$passed)
  expect_true(rocky_recent_nzsc(rx("Ah2"))$passed)
})

test_that("Ultic part 1: pH from the base of the A is read at every base", {
  skip_on_cran()
  # Ah 0-10, a row 10-40 at pH 5.0, Bt 40-60 and C at pH 5.8: from 10 cm the
  # acid row is 30 of the 50 cm to 60 cm (major part); from 40 cm it is out
  acid <- function(des2) {
    p <- .nz304_mod("ultic", list(designation = c("Ah", des2, "Bt", "C"), top_cm = c(0, 10, 40, 60),
                                  bottom_cm = c(10, 40, 60, 100), ph_h2o = c(5.8, 5.0, 5.8, 5.8)))
    ultic_soils_nzsc(p)$evidence$acid
  }
  expect_true(is.na(acid(NA_character_)))
  expect_true(acid("Bw"))
  expect_false(acid("Ah2"))
})

# Pallic: Ap, a fragipan 8 cm thick, then a row 28-60 cm, then C
.nz304_pallic <- function(des3) {
  .nz304_mod("pallic", list(designation = c("Ap", "Bx", des3, "C"), top_cm = c(0, 20, 28, 60),
                            bottom_cm = c(20, 28, 60, 120)))
}

test_that("Pallic part 2(b): a thin B over an undesignated row is no longer read as failing", {
  skip_on_cran()
  # before v0.9.304: FALSE (8 cm of B); the row may be B, making 40 cm with
  # a fragipan in it
  expect_true(is.na(.nzsc_pallic_2b(.nzsc_horizons(.nz304_pallic(NA_character_)))))
  expect_false(.nzsc_pallic_2b(.nzsc_horizons(.nz304_pallic("C1"))))
  expect_true(.nzsc_pallic_2b(.nzsc_horizons(.nz304_pallic("Bw"))))
  # the designated example is unchanged
  expect_true(.nzsc_pallic_2b(.nzsc_horizons(make_nzsc4_example("pallic"))))
  # an undesignated row with no criterion of its own still lets the thin B fail
  # when it cannot add thickness (0 cm)
  p0 <- .nz304_mod("pallic", list(designation = c("Ap", "Bx", NA, "C"), top_cm = c(0, 20, 28, 28),
                                  bottom_cm = c(20, 28, 28, 120)))
  expect_false(.nzsc_pallic_2b(.nzsc_horizons(p0)))
})

test_that("the order and group examples key as before", {
  skip_on_cran()
  for (o in c("organic", "gley", "ultic", "podzol", "allophanic", "pumice", "melanic", "semiarid",
              "oxidic", "granular", "pallic", "brown", "anthropic", "recent", "raw")) {
    r <- classify_nzsc4(make_nzsc4_example(o), on_missing = "silent")
    expect_identical(r$levels$code[r$levels$level == "orders"], toupper(substr(o, 1, 1)) |>
                       (\(z) c(organic = "O", gley = "G", ultic = "U", podzol = "Z", allophanic = "L", pumice = "M",
                               melanic = "E", semiarid = "S", oxidic = "X", granular = "N", pallic = "P", brown = "B",
                               anthropic = "A", recent = "R", raw = "W")[[o]])(), info = o)
  }
})
