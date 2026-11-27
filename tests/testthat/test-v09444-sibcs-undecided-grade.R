# v0.9.444: the first change of the SiBCS 2018 audit (decision G1, Hugo,
# 2026-11-27). Undecided tests are read at every level of the key, as in the
# USDA key and the FAO legends: listed in $ambiguities with their level, and a
# name reached past one gets grade C at most. A class is assigned only when
# its test passes; only the residuals of the text ("Outros solos") take what
# is left. The Neossolos, Luvissolos and Chernossolos steps are three-valued;
# the B espodico and the horizonte vertico are undecided only where the text
# leaves them so.

.g1 <- function(designation, top, bottom, ...) {
  hz <- data.table::data.table(designation = designation, top_cm = top,
                               bottom_cm = bottom, ...)
  PedonRecord$new(site = list(id = "g1", country = "BR"),
                  horizons = ensure_horizon_schema(hz))
}

test_that("a name reached past an undecided test has grade C and lists it with its level", {
  skip_on_cran()
  r <- classify_sibcs(make_acrisol_canonical(), on_missing = "silent")
  expect_false(is.na(r$rsg_or_order))
  expect_equal(r$evidence_grade, "C")
  lv <- vapply(r$ambiguities, function(a) a$level, "")
  expect_true(length(lv) > 0L)
  expect_true(all(lv %in% c("ordens", "subordens", "grandes_grupos", "subgrupos")))
  expect_true(any(grepl("undecided test", r$warnings, fixed = TRUE)))
})

test_that("with no order passing, the undecided orders are the candidates", {
  skip_on_cran()
  # A and C with no clay recorded: the B textural cannot be decided, so the
  # Neossolos step is undecided, and no later order passes
  p <- .g1(c("A", "B", "C"), c(0, 20, 60), c(20, 60, 120),
           munsell_hue_moist = c("10YR", "7.5YR", "7.5YR"),
           munsell_value_moist = c(3, 5, 6), munsell_chroma_moist = c(2, 6, 6))
  r <- classify_sibcs(p, on_missing = "silent")
  expect_true(is.na(r$rsg_or_order))
  expect_match(r$name, "^\\[Ordem indeterminada no SiBCS: .*Neossolos")
  expect_match(r$name, "ou n\u00e3o classificado\\]$")
  expect_equal(r$evidence_grade, "C")
  expect_true("ordens" %in% vapply(r$ambiguities, function(a) a$level, ""))
})

test_that("the Tb great groups test Tb, and no list ends with an invented residual", {
  skip_on_cran()
  rules <- load_rules("sibcs5")
  has_ta <- function(tests) any(grepl("atividade_argila_alta",
                                      capture.output(str(tests))))
  for (code in names(rules$grandes_grupos)) {
    for (t in rules$grandes_grupos[[code]]) {
      if (grepl(" Tb ", t$name, fixed = TRUE))
        expect_true(has_ta(t$tests$all_of), info = t$code)
    }
  }
  last <- function(code) { l <- rules$grandes_grupos[[code]]; l[[length(l)]] }
  for (code in c("SX", "FT", "FX", "FF"))
    expect_null(last(code)$tests$default, info = code)
})

test_that("a Gleissolo with clay activity undecided stops at the subordem", {
  skip_on_cran()
  # no CEC: Ta is undecided, so neither a Ta nor a Tb great group is named
  p <- make_gleysol_canonical()
  p$horizons$cec_cmol <- NA_real_
  r <- classify_sibcs(p, on_missing = "silent")
  if (identical(r$rsg_or_order, "Gleissolos")) {
    expect_false(grepl(" T[ab] ", r$name))
  }
})

test_that("the B espodico is undecided for lack of oxalate only where a B has the colours of item (b)", {
  skip_on_cran()
  # a yellowish B, 10YR 6/4: none of the colours of item (b)
  p <- .g1(c("A", "Bw", "C"), c(0, 20, 80), c(20, 80, 150),
           clay_pct = c(20, 30, 30), munsell_hue_moist = c("10YR", "10YR", "10YR"),
           munsell_value_moist = c(4, 6, 6), munsell_chroma_moist = c(2, 4, 4))
  expect_false(B_espodico(p)$passed)
  # a red B, 2.5YR 4/6: the oxalate of item (b)(3) is asked
  q <- .g1(c("A", "Bw", "C"), c(0, 20, 80), c(20, 80, 150),
           clay_pct = c(20, 30, 30), munsell_hue_moist = c("10YR", "2.5YR", "2.5YR"),
           munsell_value_moist = c(4, 4, 5), munsell_chroma_moist = c(2, 6, 6))
  expect_true(is.na(B_espodico(q)$passed))
  # a B with no colour recorded: undecided
  s <- .g1(c("A", "Bw", "C"), c(0, 20, 80), c(20, 80, 150), clay_pct = c(20, 30, 30))
  expect_true(is.na(B_espodico(s)$passed))
})

test_that("the horizonte vertico does not ask for cracks where its features are recorded", {
  skip_on_cran()
  v <- function(cracks = NA_real_, clay = c(40, 50, 50))
    .g1(c("A", "Biv", "Cv"), c(0, 20, 60), c(20, 60, 120), clay_pct = clay,
        slickensides = c(NA, "Comum", "Comum"), cracks_width_cm = cracks)
  expect_true(soilKey:::horizonte_vertico(v())$passed)
  # cracks recorded under the 1 cm of p. 73
  expect_false(soilKey:::horizonte_vertico(v(cracks = 0.5))$passed)
  # the features with no clay recorded: undecided
  r <- soilKey:::horizonte_vertico(v(clay = c(40, NA, NA)))
  expect_true(is.na(r$passed))
  expect_true("clay_pct" %in% r$missing)
  # no feature recorded: absent, whatever the COLE
  n <- .g1(c("A", "B", "C"), c(0, 20, 60), c(20, 60, 120), clay_pct = c(40, 50, 50))
  expect_false(soilKey:::horizonte_vertico(n)$passed)
})

test_that("the Luvissolos step is undecided when Ta or V cannot be decided", {
  skip_on_cran()
  p <- make_luvisol_canonical()
  p$horizons$bs_pct <- NA_real_
  p$horizons$ca_cmol <- NA_real_
  p$horizons$mg_cmol <- NA_real_
  p$horizons$k_cmol <- NA_real_
  p$horizons$na_cmol <- NA_real_
  if (isTRUE(B_textural(p)$passed) && isTRUE(soilKey:::atividade_argila_alta(p)$passed))
    expect_true(is.na(soilKey:::luvissolo(p)$passed))
})

test_that("the BDsolos benchmark scores a result with no order as a miss", {
  skip_on_cran()
  p <- .g1(c("A", "B", "C"), c(0, 20, 60), c(20, 60, 120),
           munsell_hue_moist = c("10YR", "7.5YR", "7.5YR"),
           munsell_value_moist = c(3, 5, 6), munsell_chroma_moist = c(2, 6, 6))
  p$site$reference_sibcs <- "NEOSSOLO REGOLITICO"
  r <- suppressMessages(benchmark_bdsolos(list(p), systems = "sibcs", verbose = FALSE,
                                          sampling_type = NULL))$per_system$sibcs
  expect_equal(r$n_compared, 1L)
  expect_equal(r$n_correct, 0L)
})
