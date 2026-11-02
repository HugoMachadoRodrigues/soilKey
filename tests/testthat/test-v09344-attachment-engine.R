# v0.9.344: the generic engine pieces of attachment, for systems that are not
# keys (the French Referentiel pedologique 2008 first): within_tolerance(),
# the tolerance table, the tolerant mode of the rule engine with the flip
# inside none_of, the degree of one class, the combiner, the candidates slot
# of the result and the additive level with no typical class. Synthetic
# profiles and rule sets; the tolerance table is a test table shaped like the
# Referentiel's p. 64 (the system's own file comes with the system).

.TOL <- list(
  list(kind = "depth", thresholds = c(10, 20, 30, 35), tol = 5),
  list(kind = "depth", thresholds = c(50, 60), tol = 10),
  list(kind = "depth", threshold = 80, tol = 15),
  list(kind = "depth", threshold = 120, tol = 20),
  list(kind = "depth", threshold = 200, tol = 30),
  list(kind = "ph", tol = 0.2),
  list(kind = "cec", tol = 1),
  list(kind = "base_saturation", tol = 5),
  list(kind = "organic_carbon", thresholds = c(8, 30), tols = c(2, 5)))
.tab <- function() .tolerance_table(.TOL)
# a profile: horizons from designations, depths and columns
.ped <- function(designation, top, bottom, ...) {
  list(horizons = data.frame(designation = designation, top_cm = top, bottom_cm = bottom, ...,
                             stringsAsFactors = FALSE))
}
# a test of the generic tolerance-aware predicate
.at <- function(...) list(.attribute_threshold = list(...))
.oc8 <- .at(attribute = "oc", threshold = 8, direction = "min", kind = "organic_carbon")
.ph55 <- .at(attribute = "ph", threshold = 5.5, direction = "min", kind = "ph")
.bt120 <- .at(attribute = "top_cm", threshold = 120, direction = "max", kind = "depth", designation = "^BT")


# --- within_tolerance() ---------------------------------------------------------

test_that("within_tolerance: minima and maxima, band edges included", {
  skip_on_cran()
  # organic carbon 8 +- 2 (the book's own example, 6.5 against 8)
  expect_identical(within_tolerance(c(8, 9, 6.5, 6, 5.9), 8, "min", 2),
                   c("pass", "pass", "imperfect", "imperfect", "fail"))
  # pH 5.5 +- 0.2: 5.3 is at the edge despite binary decimals
  expect_identical(within_tolerance(c(5.5, 5.3, 5.29), 5.5, "min", 0.2),
                   c("pass", "imperfect", "fail"))
  expect_identical(within_tolerance(c(6.5, 6.7, 6.71), 6.5, "max", 0.2),
                   c("pass", "imperfect", "fail"))
  # a depth "within 120 cm", 120 +- 20
  expect_identical(within_tolerance(c(100, 120, 130, 140, 141), 120, "max", 20),
                   c("pass", "pass", "imperfect", "imperfect", "fail"))
  # S/CEC 50 +- 5 and CEC 8 +- 1 per kg of soil
  expect_identical(within_tolerance(c(47, 44), 50, "min", 5), c("imperfect", "fail"))
  expect_identical(within_tolerance(c(7.2, 6.9), 8, "min", 1), c("imperfect", "fail"))
})

test_that("within_tolerance: a strict threshold, NA values, recycling and errors", {
  skip_on_cran()
  expect_identical(within_tolerance(c(40, 39.9), 40, "min", 0), c("pass", "fail"))
  expect_identical(within_tolerance(39.9, 40, "min", NA), "fail")
  expect_identical(within_tolerance(39.9, 40, "min", NULL), "fail")
  expect_identical(within_tolerance(c(NA, 7), 8, "min", 2), c(NA, "imperfect"))
  expect_identical(within_tolerance(numeric(0), 8, "min", 2), character(0))
  expect_identical(within_tolerance(c(7, 7), c(8, 10), "min", 2), c("imperfect", "fail"))
  expect_error(within_tolerance(7, 8, "min", -1), "negative")
  expect_error(within_tolerance(7, NA, "min", 1), "threshold")
  expect_error(within_tolerance(7, 8, "above", 1))
})


# --- the tolerance table ------------------------------------------------------

test_that("the tolerance table: per threshold, for every threshold of a kind, else strict", {
  skip_on_cran()
  tab <- .tab()
  expect_s3_class(tab, "data.frame")
  expect_identical(names(tab), c("kind", "threshold", "tol"))
  expect_identical(.tolerance_of(tab, "depth", 120), 20)
  expect_identical(.tolerance_of(tab, "depth", 35), 5)
  expect_identical(.tolerance_of(tab, "depth", 200), 30)
  # a depth the table does not list is strict (Q5)
  expect_identical(.tolerance_of(tab, "depth", 40), 0)
  expect_identical(.tolerance_of(tab, "depth", 100), 0)
  # every pH, CEC and S/CEC threshold
  expect_identical(.tolerance_of(tab, "ph", 4.2), 0.2)
  expect_identical(.tolerance_of(tab, "base_saturation", 90), 5)
  expect_identical(.tolerance_of(tab, "cec", 25), 1)
  # organic carbon 8 and 30 only
  expect_identical(.tolerance_of(tab, "organic_carbon", 8), 2)
  expect_identical(.tolerance_of(tab, "organic_carbon", 30), 5)
  expect_identical(.tolerance_of(tab, "organic_carbon", 15), 0)
  expect_identical(.tolerance_of(tab, "clay", 40), 0)
  expect_identical(.tolerance_of(NULL, "depth", 120), 0)
  # the same table from a data.frame and from a YAML-shaped list
  expect_identical(.tolerance_table(tab), tab)
  expect_identical(.tolerance_table(list(tolerances = .TOL)), tab)
  expect_error(.tolerance_table(list(list(kind = "ph", tol = 0.2), list(kind = "ph", tol = 0.3))),
               "twice")
  expect_error(.tolerance_table(list(list(tol = 1))), "no kind")
  expect_error(.tolerance_table(list(list(kind = "ph"))), "no tolerance")
  expect_error(.tolerance_table(list(list(kind = "x", thresholds = c(1, 2), tols = c(1, 2, 3)))),
               "one per threshold")
})

test_that("thresholds move only in the tolerant pass, the other way inside none_of", {
  skip_on_cran()
  tab <- .tab()
  # off by default: every system reads its thresholds as printed
  expect_false(.rule_ctx$tolerant)
  expect_identical(.rule_threshold(120, "depth", "max"), 120)
  expect_identical(.rule_threshold(8, "organic_carbon", "min"), 8)
  expect_true(is.na(.rule_distance(5, 8, "organic_carbon", "min")))
  expect_identical(.with_rule_ctx(TRUE, tab, .rule_threshold(120, "depth", "max")), 140)
  expect_identical(.with_rule_ctx(TRUE, tab, .rule_threshold(8, "organic_carbon", "min")), 6)
  expect_identical(.with_rule_ctx(TRUE, tab, .rule_threshold(40, "depth", "max")), 40)
  # inside a none_of block "within 120 cm" is widened to 100 cm, not 140
  neg <- .with_rule_ctx(TRUE, tab, {
    .rule_ctx$negated <- TRUE
    .rule_threshold(120, "depth", "max")
  })
  expect_identical(neg, 100)
  # the mode is restored afterwards, also after an error
  expect_false(.rule_ctx$tolerant)
  expect_false(.rule_ctx$negated)
  expect_null(.rule_ctx$table)
  expect_error(.with_rule_ctx(TRUE, tab, stop("boom")), "boom")
  expect_false(.rule_ctx$tolerant)
  expect_null(.rule_ctx$table)
  # distances in tolerance units
  expect_identical(.with_rule_ctx(TRUE, tab, .rule_distance(5, 8, "organic_carbon", "min")), 1.5)
  expect_identical(.with_rule_ctx(TRUE, tab, .rule_distance(c(5, 7), 8, "organic_carbon", "min", "any")), 0.5)
  expect_identical(.with_rule_ctx(TRUE, tab, .rule_distance(c(5, 7), 8, "organic_carbon", "min", "all")), 1.5)
  expect_true(is.na(.with_rule_ctx(TRUE, tab, .rule_distance(30, 40, "clay", "min"))))
})

test_that("a strict evaluation of none_of leaves the mode untouched", {
  skip_on_cran()
  p <- .ped(c("A", "BT"), c(0, 30), c(30, 80))
  r <- evaluate_rsg_tests(p, list(none_of = list(.bt120)))
  expect_false(r$passed)
  expect_false(.rule_ctx$negated)
  expect_false(.rule_ctx$tolerant)
})


# --- the degree of one class ----------------------------------------------------

test_that("the degree: simple, imperfect, fail and undecided on a minimum", {
  skip_on_cran()
  tab <- .tab()
  tests <- list(all_of = list(.oc8))
  d <- .evaluate_degree(.ped("A", 0, 20, oc = 9), tests, tab)
  expect_identical(d$degree, "simple")
  expect_identical(d$imperfect_by, character(0))
  d <- .evaluate_degree(.ped("A", 0, 20, oc = 6.5), tests, tab)
  expect_identical(d$degree, "imperfect")
  expect_false(d$strict)
  expect_true(d$tolerant)
  expect_identical(d$imperfect_by, .test_label(.oc8))
  expect_match(d$imperfect_by, "^\\.attribute_threshold\\(attribute=oc, threshold=8")
  d <- .evaluate_degree(.ped("A", 0, 20, oc = 5), tests, tab)
  expect_identical(d$degree, "fail")
  expect_identical(d$failed, .test_label(.oc8))
  expect_identical(d$distance, 1.5)
  d <- .evaluate_degree(.ped("A", 0, 20, oc = NA_real_), tests, tab)
  expect_identical(d$degree, "undecided")
  expect_identical(d$undecided_by, .test_label(.oc8))
  expect_true("oc" %in% d$missing)
  # with no tolerance table the tolerant pass is the strict one
  d <- .evaluate_degree(.ped("A", 0, 20, oc = 6.5), tests, NULL)
  expect_identical(d$degree, "fail")
  expect_true(is.na(d$distance))
  # a threshold the table does not list stays strict
  tests40 <- list(all_of = list(.at(attribute = "clay", threshold = 40, direction = "min", kind = "clay")))
  expect_identical(.evaluate_degree(.ped("B", 20, 60, clay = 38), tests40, tab)$degree, "fail")
})

test_that("the degree: inside none_of the tolerance flips (no BT within 120 cm reads 100)", {
  skip_on_cran()
  tab <- .tab()
  tests <- list(none_of = list(.bt120))
  # BT at 110 cm: strictly within 120 (fails), not within 100 (imperfect)
  d <- .evaluate_degree(.ped(c("A", "BT"), c(0, 110), c(110, 150)), tests, tab)
  expect_identical(d$degree, "imperfect")
  expect_false(d$strict)
  expect_identical(d$imperfect_by, .test_label(.bt120))
  # BT at 100 cm: at the edge of the band, which is inside it (imperfect);
  # at 99 cm beyond it
  expect_identical(.evaluate_degree(.ped(c("A", "BT"), c(0, 100), c(100, 150)), tests, tab)$degree, "imperfect")
  expect_identical(.evaluate_degree(.ped(c("A", "BT"), c(0, 99), c(99, 150)), tests, tab)$degree, "fail")
  # BT at 90 cm fails even within tolerance; had the tolerance not flipped
  # (140) the BT at 130 cm would also fail
  d <- .evaluate_degree(.ped(c("A", "BT"), c(0, 90), c(90, 150)), tests, tab)
  expect_identical(d$degree, "fail")
  expect_identical(d$failed, .test_label(.bt120))
  expect_identical(d$distance, 1.5)
  expect_identical(.evaluate_degree(.ped(c("A", "BT"), c(0, 130), c(130, 150)), tests, tab)$degree, "simple")
  # no BT at all
  expect_identical(.evaluate_degree(.ped(c("A", "C"), c(0, 30), c(30, 150)), tests, tab)$degree, "simple")
  # a none_of nested in a none_of flips back
  tests2 <- list(none_of = list(list(none_of = list(.bt120))))
  expect_identical(.evaluate_degree(.ped(c("A", "BT"), c(0, 130), c(130, 150)), tests2, tab)$degree, "imperfect")
  expect_identical(.evaluate_degree(.ped(c("A", "BT"), c(0, 110), c(110, 150)), tests2, tab)$degree, "simple")
})

test_that("the degree: simple_or_imperfect, any_of and nested blocks", {
  skip_on_cran()
  tab <- .tab()
  # one alternative undecided, the other met only within tolerance
  tests <- list(any_of = list(.oc8, .ph55))
  d <- .evaluate_degree(.ped("A", 0, 20, oc = NA_real_, ph = 5.4), tests, tab)
  expect_identical(d$degree, "simple_or_imperfect")
  expect_true(is.na(d$strict))
  expect_identical(d$imperfect_by, .test_label(.ph55))
  expect_identical(d$undecided_by, .test_label(.oc8))
  # one alternative met strictly: nothing imperfect
  d <- .evaluate_degree(.ped("A", 0, 20, oc = 9, ph = 5.4), tests, tab)
  expect_identical(d$degree, "simple")
  expect_identical(d$imperfect_by, character(0))
  # every alternative fails: one criterion, the nearest distance
  d <- .evaluate_degree(.ped("A", 0, 20, oc = 4, ph = 5.0), tests, tab)
  expect_identical(d$degree, "fail")
  expect_length(d$failed, 1L)
  expect_match(d$failed, "^one of \\{")
  expect_equal(d$distance, 2)
  # all_of with a nested any_of and a none_of
  nested <- list(all_of = list(.oc8, list(any_of = list(.ph55, .at(attribute = "cec", threshold = 10, kind = "cec"))),
                               list(none_of = list(.bt120))))
  p <- .ped(c("A", "BT"), c(0, 110), c(110, 150), oc = c(7, 2), ph = c(6, 6), cec = c(12, 12))
  d <- .evaluate_degree(p, nested, tab)
  expect_identical(d$degree, "imperfect")
  expect_setequal(d$imperfect_by, c(.test_label(.oc8), .test_label(.bt120)))
  p$horizons$oc <- c(5, 2)
  d <- .evaluate_degree(p, nested, tab)
  expect_identical(d$degree, "fail")
  expect_identical(d$failed, .test_label(.oc8))
  # default and stubbed blocks
  expect_identical(.evaluate_degree(p, list(default = TRUE), tab)$degree, "simple")
  d <- .evaluate_degree(p, list(not_implemented_v01 = "x"), tab)
  expect_identical(d$degree, "undecided")
  expect_identical(d$undecided_by, "diagnostic_x")
  # a predicate that does not read the tolerances gives the same verdict twice
  expect_identical(.evaluate_degree(p, list(all_of = list(list(no_such_predicate_xyz = list()))), tab)$degree,
                   "undecided")
  expect_false(.rule_ctx$tolerant)
})


# --- the combiner -------------------------------------------------------------

.e <- function(code, degree, ...) list(code = code, name = paste0(code, "s"), label = tolower(code),
                                       degree = degree, ...)

test_that("the combiner: the degree before the order, unlisted pairs stay candidates", {
  skip_on_cran()
  a <- .attach_combine(list(.e("A", "imperfect", imperfect_by = "oc"), .e("B", "simple"), .e("C", "fail")),
                       order = c("A", "B", "C"))
  expect_identical(a$name, "b")
  expect_identical(a$codes, "B")
  expect_identical(a$attachment, "simple")
  expect_identical(a$degree, "simple")
  cd <- a$candidates
  expect_identical(cd$code, c("B", "A"))
  expect_identical(cd$rank, 1:2)
  expect_identical(cd$in_name, c(TRUE, FALSE))
  expect_identical(cd$double_with, c("", ""))
  expect_identical(cd$imperfect_by, c("", "oc"))
  expect_identical(a$provisional_because, character(0))
  # two simple classes, no listed group: one in the name by the order
  a <- .attach_combine(list(.e("A", "simple"), .e("B", "simple")), order = c("B", "A"))
  expect_identical(a$name, "b")
  expect_identical(a$candidates$in_name, c(TRUE, FALSE))
  # with no order, the order given
  expect_identical(.attach_combine(list(.e("A", "simple"), .e("B", "simple")))$name, "a")
  # only imperfect
  a <- .attach_combine(list(.e("A", "imperfect")))
  expect_identical(a$attachment, "imperfect")
  expect_identical(names(a$candidates),
                   c("code", "name", "group", "degree", "rank", "in_name", "double_with", "n_failed",
                     "distance", "failed", "imperfect_by", "undecided_by", "excluded_by", "nearest"))
})

test_that("the combiner: explicit precedence before the degree; cycles are broken", {
  skip_on_cran()
  ents <- list(.e("A", "imperfect"), .e("B", "simple"))
  a <- .attach_combine(ents, precedence = data.frame(first = "A", second = "B"))
  expect_identical(a$name, "a")
  # a pair that does not apply, or whose class failed, changes nothing
  expect_identical(.attach_combine(ents, precedence = list(list(first = "A", second = "B", when = FALSE)))$name, "b")
  expect_identical(.attach_combine(list(.e("A", "fail"), .e("B", "imperfect")),
                                   precedence = data.frame(first = "A", second = "B"))$name, "b")
  # transitive through a third class
  ents3 <- list(.e("A", "simple"), .e("B", "simple"), .e("C", "imperfect"))
  pr <- data.frame(first = c("C", "B"), second = c("B", "A"))
  a <- .attach_combine(ents3, precedence = pr)
  expect_identical(a$candidates$code, c("C", "B", "A"))
  # a cycle
  a <- .attach_combine(ents3, precedence = data.frame(first = c("A", "B"), second = c("B", "A")))
  expect_match(a$notes, "cycle")
  expect_identical(a$name, "a")
  expect_identical(a$candidates$code, c("A", "B", "C"))
  # a pair off the cycle still applies (C before A; B first by its degree)
  a <- .attach_combine(ents3, precedence = data.frame(first = c("A", "B", "C"), second = c("B", "A", "A")))
  expect_identical(a$candidates$code, c("B", "C", "A"))
  # a pair left undecided is not applied and makes the name provisional
  a <- .attach_combine(ents, precedence = list(list(first = "A", second = "B", when = NA)))
  expect_identical(a$name, "b")
  expect_identical(a$provisional_because, "A")
  expect_match(a$notes, "undecided")
})

test_that("the combiner: listed doubles and multiples, in their own order", {
  skip_on_cran()
  ents <- list(.e("A", "simple"), .e("B", "imperfect"), .e("C", "simple"))
  a <- .attach_combine(ents, groups = list(c("B", "A")), order = c("A", "B", "C"))
  expect_identical(a$name, "b-a")
  expect_identical(a$codes, c("B", "A"))
  expect_identical(a$attachment, "double")
  expect_identical(a$degree, "imperfect")
  cd <- a$candidates
  expect_identical(cd$in_name[cd$code %in% c("A", "B")], c(TRUE, TRUE))
  expect_identical(cd$double_with[cd$code == "A"], "B")
  expect_false(cd$in_name[cd$code == "C"])
  # a group whose partner does not attach is not written
  a <- .attach_combine(list(.e("A", "simple"), .e("B", "fail")), groups = list(c("B", "A")))
  expect_identical(a$name, "a")
  # a group without the main class is not written
  a <- .attach_combine(ents, groups = list(c("B", "C")), order = c("A", "B", "C"))
  expect_identical(a$name, "a")
  # a partner left undecided makes the name provisional
  a <- .attach_combine(list(.e("A", "simple"), .e("B", "undecided")), groups = list(c("A", "B")),
                       order = c("A", "B"))
  expect_identical(a$name, "a")
  expect_identical(a$provisional_because, "B")
  # a listed triple, before the double it contains
  a <- .attach_combine(ents, groups = list(c("A", "B"), list(codes = c("C", "A", "B"), note = "p. 0")),
                       order = c("A", "B", "C"), sep = " - ")
  expect_identical(a$name, "c - a - b")
  expect_identical(a$attachment, "multiple")
  # a condition left undecided: not written, provisional
  a <- .attach_combine(ents, groups = list(list(codes = c("A", "B"), when = NA)), order = c("A", "B", "C"))
  expect_identical(a$name, "a")
  expect_identical(a$provisional_because, "B")
})

test_that("the combiner: undecided classes make the name provisional only when they could take it", {
  skip_on_cran()
  ents <- list(.e("U", "undecided", undecided_by = "ph"), .e("A", "simple"), .e("V", "undecided"))
  a <- .attach_combine(ents, order = c("U", "A", "V"))
  expect_identical(a$name, "a")
  expect_identical(a$provisional_because, "U")
  expect_identical(a$candidates$code, c("A", "U", "V"))
  expect_identical(a$candidates$undecided_by, c("", "ph", ""))
  # a simple_or_imperfect main class, an imperfect class before it in the order
  a <- .attach_combine(list(.e("X", "imperfect"), .e("M", "simple_or_imperfect")), order = c("X", "M"))
  expect_identical(a$name, "m")
  expect_identical(a$attachment, "simple_or_imperfect")
  expect_identical(a$provisional_because, "X")
  # a simple_or_imperfect class after a simple one in the degree but before it in the order
  a <- .attach_combine(list(.e("Z", "simple_or_imperfect"), .e("M", "simple")), order = c("Z", "M"))
  expect_identical(a$name, "m")
  expect_identical(a$provisional_because, "Z")
  # an undecided class with an explicit pair over the main class
  a <- .attach_combine(list(.e("A", "simple"), .e("U", "undecided")), order = c("A", "U"),
                       precedence = data.frame(first = "U", second = "A"))
  expect_identical(a$name, "a")
  expect_identical(a$provisional_because, "U")
  expect_identical(a$candidates$code, c("U", "A"))
})

test_that("the combiner: exclusions", {
  skip_on_cran()
  ents <- list(.e("A", "simple"), .e("B", "simple"), .e("C", "simple", excluded_by = "a horizon H within 50 cm"))
  a <- .attach_combine(ents, exclusions = data.frame(code = "A", by = "B", note = "p. 102"))
  expect_identical(a$name, "b")
  cd <- a$candidates
  expect_identical(cd$code, c("B", "A", "C"))
  expect_identical(cd$rank, c(1L, NA, NA))
  expect_identical(cd$excluded_by, c("", "B", "a horizon H within 50 cm"))
  # by an undecided class: kept, marked, and the name provisional
  a <- .attach_combine(list(.e("A", "simple"), .e("B", "undecided")),
                       exclusions = data.frame(code = "A", by = "B"), order = c("A", "B"))
  expect_identical(a$name, "a")
  expect_identical(a$candidates$excluded_by[1L], "B (undecided)")
  expect_identical(a$provisional_because, "B")
  # by a class that fails: no exclusion
  a <- .attach_combine(list(.e("A", "simple"), .e("B", "fail")), exclusions = data.frame(code = "A", by = "B"))
  expect_identical(a$candidates$excluded_by, "")
})

test_that("the combiner: nothing attaches, the three nearest", {
  skip_on_cran()
  ents <- list(.e("A", "fail", failed = c("x", "y", "z")), .e("B", "fail", failed = "x", distance = 3),
               .e("C", "fail", failed = "y", distance = 1.2), .e("D", "fail", failed = c("x", "y")),
               .e("E", "fail", failed = "w"))
  a <- .attach_combine(ents, none_label = "[none]")
  expect_identical(a$name, "[none]")
  expect_identical(a$attachment, "not_attached")
  expect_identical(a$codes, character(0))
  expect_true(is.na(a$degree))
  cd <- a$candidates
  expect_identical(cd$code, c("C", "B", "E"))
  expect_true(all(cd$nearest))
  expect_true(all(is.na(cd$rank)))
  expect_identical(cd$n_failed, c(1L, 1L, 1L))
  expect_identical(cd$failed, c("y", "x", "w"))
  # with an undecided class too: it is a candidate, the name provisional
  a <- .attach_combine(c(ents, list(.e("U", "undecided"))), n_nearest = 1L)
  expect_identical(a$candidates$code, c("U", "C"))
  expect_identical(a$candidates$nearest, c(FALSE, TRUE))
  expect_identical(a$provisional_because, "U")
  # nothing at all
  a <- .attach_combine(list())
  expect_identical(a$attachment, "not_attached")
  expect_identical(nrow(a$candidates), 0L)
})

test_that("the combiner: checks its input; a data.frame of entries works", {
  skip_on_cran()
  expect_error(.attach_combine(list(.e("A", "maybe"))), "degree must be one of")
  expect_error(.attach_combine(list(.e("A", "simple"), .e("A", "fail"))), "repeat")
  expect_error(.attach_combine(list(list(degree = "simple"))), "no code")
  df <- data.frame(code = c("A", "B"), degree = c("imperfect", "simple"), stringsAsFactors = FALSE)
  a <- .attach_combine(df)
  expect_identical(a$name, "B")
  expect_identical(a$candidates$name, c("B", "A"))
})

test_that("degrees and combiner together on small rule sets", {
  skip_on_cran()
  tab <- .tab()
  refs <- list(
    list(code = "humic", tests = list(all_of = list(.oc8))),
    list(code = "acid", tests = list(all_of = list(.at(attribute = "ph", threshold = 5, direction = "max", kind = "ph")))),
    list(code = "no_bt", tests = list(none_of = list(.bt120))),
    list(code = "clayey", tests = list(all_of = list(.at(attribute = "clay", threshold = 40, kind = "clay")))))
  p <- .ped(c("A", "BT"), c(0, 110), c(110, 150), oc = c(7, 1), ph = c(5.1, 5.6), clay = c(20, 30))
  ents <- lapply(refs, function(r) c(list(code = r$code, name = r$code), .evaluate_degree(p, r$tests, tab)))
  expect_identical(vapply(ents, `[[`, "", "degree"), c("imperfect", "imperfect", "imperfect", "fail"))
  a <- .attach_combine(ents, order = c("acid", "humic", "no_bt", "clayey"), groups = list(c("humic", "acid")))
  expect_identical(a$name, "humic-acid")
  expect_identical(a$attachment, "double")
  cd <- a$candidates
  expect_identical(cd$code, c("acid", "humic", "no_bt"))
  expect_identical(cd$in_name, c(TRUE, TRUE, FALSE))
  expect_match(cd$imperfect_by[cd$code == "no_bt"], "top_cm")
  # data for humic raised: simple; the double keeps its degree from acid
  p$horizons$oc <- c(9, 1)
  ents <- lapply(refs, function(r) c(list(code = r$code, name = r$code), .evaluate_degree(p, r$tests, tab)))
  a <- .attach_combine(ents, order = c("acid", "humic", "no_bt", "clayey"))
  expect_identical(a$name, "humic")
})


# --- the result slot and the additive level -------------------------------------

test_that("the candidates slot is empty and invisible for the other systems", {
  skip_on_cran()
  res <- classify_cssc3(make_cssc3_example("podzolic"), on_missing = "silent")
  expect_null(res$candidates)
  expect_false("n_candidates" %in% names(res$summary()))
  out <- paste(cli::cli_fmt(print(res)), collapse = "\n")
  expect_false(grepl("Candidates", out))
  # filled, it is printed and counted
  res2 <- res$clone(deep = TRUE)
  res2$candidates <- .attach_combine(list(.e("A", "simple"), .e("B", "imperfect")))$candidates
  expect_identical(res2$summary()$n_candidates, 2L)
  out2 <- paste(cli::cli_fmt(print(res2)), collapse = "\n")
  expect_match(out2, "Candidates: A \\(simple, in the name\\); B \\(imperfect\\)")
  # an empty table prints nothing
  res2$candidates <- res2$candidates[0, ]
  expect_false(grepl("Candidates", paste(cli::cli_fmt(print(res2)), collapse = "\n")))
})

test_that("the additive level with no typical class", {
  skip_on_cran()
  res <- ClassificationResult$new(
    system = "test", name = "luvisol",
    levels = data.frame(level = "references", code = "luvisols", name = "luvisol", stringsAsFactors = FALSE),
    system_id = "test")
  f <- list(list(id = "g", name = "glossique", passed = FALSE), list(id = "d", name = "drain\u00e9", passed = FALSE))
  r <- .add_additive_level(res, NULL, f, level = "qualifiers", after = "references", typical = NULL)
  expect_identical(r$name, "luvisol")
  expect_identical(r$levels$level, "references")
  expect_identical(r$qualifiers$qualifiers, character(0))
  expect_identical(names(r$trace$qualifiers), c("g", "d"))
  f[[1L]]$passed <- TRUE
  r <- .add_additive_level(res, NULL, f, level = "qualifiers", after = "references", typical = NULL)
  expect_identical(r$name, "luvisol glossique")
  expect_identical(r$levels$code, c("luvisols", "g"))
  # undecided entries keep the name provisional, still with no typical row
  f[[1L]]$passed <- NA
  r <- .add_additive_level(res, NULL, f, level = "qualifiers", after = "references", typical = NULL)
  expect_identical(r$name, "luvisol")
  expect_identical(r$provisional_because, "qualifiers:g")
  # the default keeps the typical class
  f[[1L]]$passed <- FALSE
  r <- .add_additive_level(res, NULL, f, level = "qualifiers", after = "references")
  expect_identical(r$name, "luvisol [typical]")
})
