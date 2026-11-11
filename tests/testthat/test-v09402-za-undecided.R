# v0.9.402 (Hugo, 2026-11-11): two decisions.
# 1. When no class of the first level of an ordered key passes but some are
#    undecided, the result names the candidates, in the order of the key,
#    provisional (classify_hierarchical(), as the FAO-Unesco legends do);
#    "not classified" only when every class of the first level fails.
# 2. South Africa 1991: with no plasticity index, a strong granular (or
#    crumb) structure refuses the vertic A; only a blocky, prismatic or
#    columnar structure, or one whose type is not recorded, leaves vertic A
#    (ii) undecided (B1s).

.za402_cl <- function(p) classify_za1991(p, on_missing = "silent")
.za402_set <- function(p, rows, ...) {
  q <- p$clone(deep = TRUE)
  a <- list(...)
  for (k in names(a)) q$horizons[[k]][rows] <- a[[k]]
  q
}

test_that("the canonical Ferralsol keys to Inanda, decided (granular A: humic, not vertic)", {
  skip_on_cran()
  r <- .za402_cl(make_ferralsol_canonical())
  expect_identical(r$levels$code[1L], "[Ia]")
  expect_identical(r$rsg_or_order, "Inanda")
  expect_match(r$name, "^Inanda, ")
  expect_false(any(startsWith(r$provisional_because, "forms:")))
  expect_identical(r$qualifiers$za_sequence, "HA - RAB")
  expect_false(any(grepl("not classified", r$warnings)))
})

test_that("forms undecided and none passed: the candidates are named, provisional", {
  skip_on_cran()
  # the same A with a strong angular blocky structure, 50 % clay and no
  # plasticity index: vertic or humic A undecided
  p <- .za402_set(make_ferralsol_canonical(), 1:2, structure_type = "angular blocky")
  expect_true(is.na(p$horizons$plasticity_index[1L]))
  r <- .za402_cl(p)
  expect_identical(nrow(r$levels), 0L)
  expect_true(is.na(r$rsg_or_order))
  expect_match(r$name, "^\\[Undecided in the South African soil classification \\(1991\\): ")
  expect_match(r$name, ", or not classified\\]$")
  expect_match(r$name, "Inanda ([Ia])", fixed = TRUE)
  expect_match(r$name, "Arcadia (Ar)", fixed = TRUE)
  # in the order of the key, as provisional_because lists them
  und <- sub("^forms:", "", grep("^forms:", r$provisional_because, value = TRUE))
  expect_true(length(und) >= 2L)
  pos <- vapply(sprintf("(%s)", und), function(z) regexpr(z, r$name, fixed = TRUE)[1L], 0L)
  expect_true(all(pos > 0L))
  expect_false(is.unsorted(pos))
  # one warning, which says the key could not decide; no contradiction
  expect_true(any(grepl("cannot decide between these candidates", r$warnings, fixed = TRUE)))
  expect_false(any(grepl("so the profile is not classified", r$warnings, fixed = TRUE)))
  expect_false(any(grepl("^Provisional: [0-9]+ class", r$warnings)))
  # the example "undecided" (an orthic A over a B with no hue): Hutton or Clovelly
  u <- .za402_cl(make_za1991_example("undecided"))
  expect_match(u$name, "Hutton (Hu)", fixed = TRUE)
  expect_match(u$name, "Clovelly (Cv)", fixed = TRUE)
})

test_that("not classified only when every form fails", {
  skip_on_cran()
  # rules whose forms all fail on this profile (the sequence of Hutton is
  # not this one's)
  rules <- load_rules("za1991")
  rules$forms <- Filter(function(f) identical(f$code, "Wb"), rules$forms)
  r <- .za402_cl(make_za1991_example("hutton"))
  expect_false(is.na(r$name))
  r <- classify_za1991(make_za1991_example("hutton"), rules = rules, on_missing = "silent")
  expect_true(is.na(r$name))
  expect_identical(nrow(r$levels), 0L)
  expect_true(any(grepl("so the profile is not classified", r$warnings, fixed = TRUE)))
  expect_false(any(grepl("Undecided", r$warnings, fixed = TRUE)))
  # the generic engine: every class of the top fails, name NA
  rr <- list(orders = list(list(code = "A", name = "Class A", tests = list(all_of = list(list(gelisol_usda = list()))))))
  g <- soilKey:::classify_hierarchical("usda", make_ferralsol_canonical(), rules = rr, on_missing = "silent")
  expect_true(is.na(g$name))
  # one class undecided at the top: named
  rr$orders[[2L]] <- list(code = "B", name = "Class B",
                          tests = list(all_of = list(list(no_such_diagnostic_v09402 = list()))))
  g <- soilKey:::classify_hierarchical("usda", make_ferralsol_canonical(), rules = rr, on_missing = "silent")
  expect_identical(g$name, "[Undecided in the USDA Soil Taxonomy: Class B (B), or not classified]")
  expect_identical(g$provisional_because, "orders:B")
})

test_that("CPCS 1967: classes undecided and none passed are named too", {
  skip_on_cran()
  p <- make_cpcs1967_example("brunified")
  expect_identical(classify_cpcs1967(p, on_missing = "silent")$name, "Sols bruns acides (oligotrophes)")
  # no designations: the B or A2 of the brunified soils is undecided
  p$horizons$designation <- NA_character_
  r <- classify_cpcs1967(p, on_missing = "silent")
  expect_identical(nrow(r$levels), 0L)
  expect_identical(r$name, paste0("[Undecided in the French soil classification CPCS (1967): ",
                                  "Sols peu \u00e9volu\u00e9s (II), Sols brunifi\u00e9s (VII), or not classified]"))
  expect_identical(r$provisional_because, c("classes:II", "classes:VII"))
  expect_true(any(grepl("cannot decide between these candidates", r$warnings, fixed = TRUE)))
})

test_that("FAO 1988 keeps its own wording for the same case", {
  skip_on_cran()
  r <- classify_fao(make_ferralsol_canonical(), on_missing = "silent")
  expect_false(any(grepl("cannot decide between these candidates", r$warnings, fixed = TRUE)))
})

test_that("vertic A (ii) with no plasticity index: granular refuses, blocky or no type leaves it undecided", {
  skip_on_cran()
  base <- make_ferralsol_canonical()
  va <- function(type) {
    p <- .za402_set(base, 1:2, structure_type = type)
    vertic_a_za(p, layers = 1L)$passed
  }
  expect_false(va("granular"))
  expect_false(va("crumb"))
  expect_true(is.na(va("angular blocky")))
  expect_true(is.na(va("subangular blocky")))
  expect_true(is.na(va("prismatic")))
  expect_true(is.na(va("columnar")))
  expect_true(is.na(va(NA_character_)))
  # a plasticity index recorded still decides, granular or not
  p <- .za402_set(base, 1:2, structure_type = "granular", plasticity_index = 40)
  expect_true(vertic_a_za(p, layers = 1L)$passed)
  # common slickensides make it vertic whatever the type
  p <- .za402_set(base, 1:2, structure_type = "granular", slickensides = "common")
  expect_true(vertic_a_za(p, layers = 1L)$passed)
})
