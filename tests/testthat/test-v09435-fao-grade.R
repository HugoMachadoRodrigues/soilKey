# v0.9.435: the evidence grade and the ambiguities of the FAO-Unesco legends
# (1974 and 1988), Hugo's decision G1 (a) of the FAO audit (2026-11-24): the
# rule of the USDA key since 0.9.415. A grouping or unit tested before the
# one assigned and left undecided joins $ambiguities with its level, and a
# name reached past it has grade C at most; so does a name resting on a USDA
# or WRB reader standing in for an undecided FAO criterion. The 1974 key
# carries the stand-ins of the horizons it reads through its key-level
# predicates (A1). No name changes.

.f435 <- function(p, ed = "1988") classify_fao(p, edition = ed, on_missing = "silent")
.lv435 <- function(r) vapply(r$ambiguities, function(a) a$level %||% NA_character_, "")
.cd435 <- function(r) vapply(r$ambiguities, function(a) a$rsg_code %||% NA_character_, "")
.edit435 <- function(p, ...) {
  h <- as.data.frame(p$horizons)
  v <- list(...)
  for (k in names(v)) h[[k]] <- v[[k]]
  PedonRecord$new(site = p$site, horizons = ensure_horizon_schema(data.table::as.data.table(h)))
}

test_that("a name keyed without an undecided test or a stand-in keeps grade A", {
  skip_on_cran()
  for (ed in c("1988", "1974")) {
    r <- .f435(make_fao_example(if (ed == "1988") "LVx" else "Lc", edition = ed), ed)
    expect_identical(r$evidence_grade, "A", info = ed)
    expect_length(r$ambiguities, 0L)
    expect_false(any(grepl("^The name was reached past|^The name rests on", r$warnings)), info = ed)
  }
})

test_that("an undecided grouping before the assigned one: grade C, listed with its level (1988)", {
  skip_on_cran()
  firm <- .f435(make_fao_example("CM"))
  p <- .edit435(make_fao_example("CM"), ec_dS_m = NA_real_)
  r <- .f435(p)
  expect_identical(r$name, firm$name)
  expect_identical(r$evidence_grade, "C")
  expect_true("SC" %in% .cd435(r))
  expect_identical(unname(.lv435(r)[.cd435(r) == "SC"]), "major_groups")
  w <- grep("^The name was reached past", r$warnings, value = TRUE)
  expect_length(w, 1L)
  expect_match(w, "major_groups:SC", fixed = TRUE)
  expect_match(w, "at most C", fixed = TRUE)
  expect_false(any(grepl("^Provisional: [0-9]+ class", r$warnings)))
  # every undecided test before the name is listed, at its level
  pb <- r$provisional_because
  expect_setequal(paste0(.lv435(r), ":", .cd435(r)), pb)
})

test_that("an undecided unit before the assigned one: grade C, level units (both editions)", {
  skip_on_cran()
  r <- .f435(make_histosol_canonical(), "1988")
  expect_identical(r$name, "Terric Histosols")
  expect_identical(r$evidence_grade, "C")
  expect_true(length(r$ambiguities) >= 2L)
  expect_true(all(.lv435(r) == "units"))
  expect_setequal(paste0("units:", .cd435(r)), r$provisional_because)
  expect_match(grep("^The name was reached past", r$warnings, value = TRUE), "units:", fixed = TRUE)
  r74 <- .f435(make_luvisol_canonical(), "1974")
  expect_identical(r74$name, "Orthic Luvisols")
  expect_identical(r74$evidence_grade, "C")
  expect_true(length(r74$ambiguities) >= 1L)
  expect_true(all(.lv435(r74) == "units"))
  # up to 0.9.434 a unit undecided before the assigned one never reached
  # $ambiguities; now each one of provisional_because is there
  expect_setequal(paste0(.lv435(r74), ":", .cd435(r74)), r74$provisional_because)
})

test_that("a name resting on a stand-in has grade C at most, with a warning naming it", {
  skip_on_cran()
  for (ed in c("1974", "1988")) {
    r <- .f435(make_podzol_canonical(), ed)
    expect_identical(r$rsg_or_order, "Podzols", info = ed)
    expect_identical(r$evidence_grade, "C", info = ed)
    w <- grep("^The name rests on a reader of another system", r$warnings, value = TRUE)
    expect_length(w, 1L)
    expect_match(w, "spodic_horizon_usda()", fixed = TRUE)
    expect_match(w, sprintf("the %s legend", ed), fixed = TRUE)
  }
})

test_that("which results a name rests on (.fao_rests_on)", {
  skip_on_cran()
  rr <- soilKey:::.fao_rests_on
  leaf <- function(passed, si = character(0), name = "t") list(test_name = name, passed = passed,
    evidence = list(stand_in = si, lower_grade = length(si) > 0L))
  # all_of: each test is needed
  expect_identical(rr(list(leaf(TRUE, "x_usda"), leaf(TRUE)), "all_of", TRUE), "x_usda()")
  # any_of: a firm passing test is enough, so the stand-in does not count
  expect_length(rr(list(leaf(TRUE, "x_usda"), leaf(TRUE)), "any_of", TRUE), 0L)
  expect_identical(rr(list(leaf(TRUE, "x_usda"), leaf(FALSE)), "any_of", TRUE), "x_usda()")
  # a failed test of an any_of that passed is not on the path
  expect_length(rr(list(leaf(TRUE), leaf(FALSE, "y")), "any_of", TRUE), 0L)
  # none_of: a test found absent by a stand-in counts
  expect_identical(rr(list(leaf(FALSE, "y"), leaf(FALSE)), "none_of", TRUE), "y()")
  # nested blocks are followed
  nest <- list(test_name = "any_of", passed = FALSE, evidence = list(leaf(FALSE, "z"), leaf(FALSE)))
  expect_identical(rr(list(leaf(TRUE), nest), "all_of", TRUE), "z()")
  # lower_grade without a name
  lg <- list(test_name = "q_fao74", passed = TRUE, evidence = list(lower_grade = TRUE))
  expect_identical(rr(list(lg), "all_of", TRUE), "q_fao74() (lower grade)")
  expect_length(rr(list(leaf(TRUE, "x")), "all_of", NA), 0L)
})

test_that("1974: only_horizons_fao74() keeps the stand-in that decided a horizon (A1)", {
  skip_on_cran()
  # the canonical Anthrosol: its 1974 cambic B undecided, decided by the WRB
  # cambic(); the Regosols gate (only an ochric A) fails on it
  p <- make_anthrosol_canonical()
  cb <- cambic_b_fao74(p)
  expect_true(isTRUE(cb$passed))
  expect_true("cambic" %in% cb$evidence$stand_in)
  oh <- only_horizons_fao74(p, allowed = "ochric_a")
  expect_false(oh$passed)
  expect_identical(oh$evidence$stand_in, "cambic")
  expect_true(oh$evidence$lower_grade)
  # a horizon present without a stand-in decides the failure alone
  ex <- make_fao_example("Lc", edition = "1974")
  ah <- only_horizons_fao74(ex, allowed = "ochric_a")
  expect_false(ah$passed)
  expect_length(ah$evidence$stand_in, 0L)
  expect_false(ah$evidence$lower_grade)
})

test_that("1974: a horizon found absent by a stand-in reaches the gate that passes", {
  skip_on_cran()
  p <- make_fao_example("Lc", edition = "1974")
  absent <- argillic_b_fao74(p)$clone()
  absent$passed <- FALSE
  absent$layers <- integer(0)
  absent$evidence$stand_in <- "argillic_usda"
  absent$evidence$lower_grade <- TRUE
  testthat::local_mocked_bindings(argillic_b_fao74 = function(pedon, ...) absent)
  # the argillic B is the example's only diagnostic horizon besides the
  # ochric A: found absent by the stand-in, the gate passes on it
  oh <- only_horizons_fao74(p, allowed = "ochric_a")
  expect_true(oh$passed)
  expect_identical(oh$evidence$stand_in, "argillic_usda")
  expect_true(oh$evidence$lower_grade)
})

test_that("1974: the mollic and umbric callers carry the stand-in of the A (A1)", {
  skip_on_cran()
  p <- make_fao_example("C", edition = "1974")
  mo <- mollic_a_fao74(p)
  expect_true(isTRUE(mo$passed))
  firm <- mollic_chroma_fao74(p, max = 2)
  expect_length(firm$evidence$stand_in, 0L)
  si <- mo$clone()
  si$evidence$stand_in <- "mollic_epipedon_usda"
  si$evidence$lower_grade <- TRUE
  testthat::local_mocked_bindings(mollic_a_fao74 = function(pedon) si)
  mc <- mollic_chroma_fao74(p, max = 2)
  expect_true(mc$passed)
  expect_identical(mc$passed, firm$passed)
  expect_identical(mc$evidence$stand_in, "mollic_epipedon_usda")
  expect_true(mc$evidence$lower_grade)
  # the Rendzinas gate fails on the lime, not on the A: no stand-in
  rz <- rendzinas_gate_fao74(p)
  expect_false(rz$passed)
  expect_false(rz$evidence$calcareous_gt)
  expect_length(rz$evidence$stand_in, 0L)
  e <- make_fao_example("E", edition = "1974")
  me <- mollic_a_fao74(e)$clone()
  me$evidence$stand_in <- "mollic_epipedon_usda"
  me$evidence$lower_grade <- TRUE
  testthat::local_mocked_bindings(mollic_a_fao74 = function(pedon) me)
  rz2 <- rendzinas_gate_fao74(e)
  expect_true(rz2$passed)
  expect_identical(rz2$evidence$stand_in, "mollic_epipedon_usda")
  # the A found absent by a stand-in: the gate fails on it, and says so
  no <- mo$clone()
  no$passed <- FALSE
  no$evidence$stand_in <- "mollic"
  no$evidence$lower_grade <- TRUE
  testthat::local_mocked_bindings(mollic_a_fao74 = function(pedon) no)
  mc2 <- mollic_chroma_fao74(p, max = 2)
  expect_false(mc2$passed)
  expect_identical(mc2$evidence$stand_in, "mollic")
})

test_that("1974: the umbric thickness and the argillic B of the Nitosols carry their stand-ins", {
  skip_on_cran()
  p <- make_fao_example("U", edition = "1974")
  um <- umbric_a_fao74(p)
  expect_true(isTRUE(um$passed))
  si <- um$clone()
  si$evidence$stand_in <- "umbric_epipedon_usda"
  si$evidence$lower_grade <- TRUE
  testthat::local_mocked_bindings(umbric_a_fao74 = function(pedon) si)
  ut <- umbric_thickness_fao74(p, max = 25)
  expect_identical(ut$passed, TRUE)
  expect_identical(ut$evidence$stand_in, "umbric_epipedon_usda")
  q <- make_fao_example("N", edition = "1974")
  ar <- argillic_b_fao74(q)$clone()
  ar$evidence$stand_in <- "argillic_usda"
  ar$evidence$lower_grade <- TRUE
  testthat::local_mocked_bindings(argillic_b_fao74 = function(pedon, ...) ar)
  ng <- nitosols_gate_fao74(q)
  expect_identical(ng$evidence$stand_in, "argillic_usda")
  pz <- podzoluvisols_gate_fao74(q)
  expect_identical(pz$evidence$stand_in, "argillic_usda")
})

test_that("a fall-through keeps its grade and lists its candidates with their level", {
  skip_on_cran()
  r <- .f435(make_durisol_canonical(), "1974")
  expect_match(r$name, "^\\[Undecided in the FAO-Unesco Legend \\(1974\\)")
  expect_identical(r$evidence_grade, "A")
  expect_true(all(.lv435(r) == "major_groups"))
  expect_setequal(.cd435(r), c("Z", "R"))
  expect_false(any(grepl("^The name was reached past", r$warnings)))
})

test_that("no name changes: the canonical fixtures keep their FAO names", {
  skip_on_cran()
  want <- list(
    andosol = c("Humic Cambisols", "Humic Alisols"),
    chernozem = c("Haplic Phaeozems", "Haplic Phaeozems"),
    kastanozem = c("Haplic Xerosols", "Eutric Cambisols"),
    # 0.9.438 (R9 (a)): the mottled E takes the colour route, a Planosol in 1988
    planosol = c("Eutric Planosols", "Eutric Planosols"),
    podzol = c("Orthic Podzols", "Haplic Podzols"),
    ferralsol = c("Humic Ferralsols", "Haplic Ferralsols"),
    vertisol = c("Eutric Cambisols", "Vertic Cambisols"))
  for (f in names(want)) {
    p <- get(paste0("make_", f, "_canonical"))()
    expect_identical(.f435(p, "1974")$name, want[[f]][1L], info = f)
    expect_identical(.f435(p, "1988")$name, want[[f]][2L], info = f)
  }
  # the Ferralsol keys firmly in both editions: grade A
  expect_identical(.f435(make_ferralsol_canonical(), "1974")$evidence_grade, "A")
  expect_identical(.f435(make_ferralsol_canonical(), "1988")$evidence_grade, "A")
})
