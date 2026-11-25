# v0.9.440: the units, the readings text and the crosswalk of the Revised
# Legend (1988) as the audit of the FAO legends read them (Hugo, 2026-11-24,
# option (a) of R10 and R17, with the defects D3 to D5): the Cambic Podzols
# allow a thin or a discontinuous albic E (R10); the deleted Solodic Planosols
# and Placic Podzols carry bracketed alternatives in the crosswalk (R17); the
# Q24 text on the Chromic colour and the Gc to GLk citation (D4, D5). With
# Hugo's decision of 2026-11-25: carbonate recorded only in a form that is not
# soft (hard, crystals) is no soft powdery lime. Every pedon and ISIS record
# below is synthetic.

# The PZb example with its E (layer 2) given a thickness and a boundary.
.pzb440 <- function(e_cm, topo) {
  p <- make_fao_example("PZb")
  soilKey:::.f88x_edit(p, top_cm = c(0, 10, 10 + e_cm, 45, 80),
                       bottom_cm = c(10, 10 + e_cm, 45, 80, 130),
                       boundary_topography = c(NA, topo, NA, NA, NA))
}

# ---- R10: Cambic Podzols, thin or discontinuous ------------------------------

test_that("R10: the albic E of the Cambic Podzols is thin or discontinuous", {
  skip_on_cran()
  f <- function(p) soilKey:::podzol_cambic_fao88(p)$passed
  expect_true(f(.pzb440(2, "broken")))     # thin and discontinuous
  expect_true(f(.pzb440(2, "smooth")))     # thin, continuous
  expect_true(f(.pzb440(2, NA)))           # thin, boundary not recorded
  expect_true(f(.pzb440(3, "broken")))     # discontinuous, thicker than 2 cm
  expect_false(f(.pzb440(3, "smooth")))    # continuous and thicker: Haplic
  expect_false(f(.pzb440(3, NA)))          # not recorded reads continuous
  ev <- soilKey:::podzol_cambic_fao88(.pzb440(3, "broken"))$evidence
  expect_true(ev$albic_thin_or_discontinuous)
})

test_that("R10: a broken albic E of 3 cm keys PZb, a continuous one PZh", {
  skip_on_cran()
  k <- function(p) classify_fao(p, on_missing = "silent")$levels$code
  expect_identical(k(.pzb440(3, "broken")), c("PZ", "PZb"))
  expect_identical(k(.pzb440(3, "smooth")), c("PZ", "PZh"))
})

test_that("R10: the PZb example keys to itself and its near miss misses again", {
  skip_on_cran()
  nm <- make_fao_example("PZb", near_miss = TRUE)
  h <- nm$horizons
  expect_equal(h$bottom_cm[2] - h$top_cm[2], 3)
  expect_identical(h$boundary_topography[2], "smooth")
  expect_false(soilKey:::podzol_cambic_fao88(nm)$passed)
  expect_identical(classify_fao(nm, on_missing = "silent")$levels$code, c("PZ", "PZh"))
  r <- classify_fao(make_fao_example("PZb"), on_missing = "silent")
  expect_identical(r$levels$code, c("PZ", "PZb"))
  expect_length(r$provisional_because, 0L)
})

test_that("R10: the unit file states the reading", {
  skip_on_cran()
  u <- load_rules("fao1988")$units$PZ
  pzb <- Filter(function(e) identical(e$code, "PZb"), u)[[1]]
  expect_match(pzb$notes, "2 cm or less) or discontinuous", fixed = TRUE)
})

# ---- R17: the deleted Ws and Pp in the crosswalk -----------------------------

test_that("R17: Ws and Pp stay deleted and carry bracketed alternatives", {
  skip_on_cran()
  cw <- fao_crosswalk(c("Ws", "Pp"))
  ws <- cw[cw$code_1974 == "Ws", ]
  pp <- cw[cw$code_1974 == "Pp", ]
  expect_identical(ws$relation, "deleted")
  expect_identical(ws$code_1988, "")
  expect_identical(ws$also, "SN, PL")
  expect_identical(ws$also_when, "SN: a natric B; PL: a sodic phase")
  expect_false(ws$bracketed)
  expect_identical(pp$relation, "deleted")
  expect_identical(pp$code_1988, "")
  expect_identical(pp$also, "PZ")
  expect_identical(pp$also_when, "a placic phase")
  expect_false(pp$bracketed)
  # the reverse lookup finds them through the alternatives
  expect_true("Ws" %in% fao_crosswalk("SN", from = "1988")$code_1974)
  expect_true("Ws" %in% fao_crosswalk("PL", from = "1988")$code_1974)
  expect_true("Pp" %in% fao_crosswalk("PZ", from = "1988")$code_1974)
})

test_that("R17: the edition lines show the alternatives in brackets, as for Gp", {
  skip_on_cran()
  line <- function(id, codes) soilKey:::.fao_edition_lines(list(system_id = id, levels = data.frame(code = codes)))
  expect_identical(line("fao1974", c("W", "Ws")),
                   "Revised Legend (1988): no class of 1988 (deleted, App. 4 p. 123) [or SN Solonetz: a natric B; PL Planosols: a sodic phase].")
  expect_identical(line("fao1974", c("P", "Pp")),
                   "Revised Legend (1988): no class of 1988 (deleted, App. 4 p. 124) [or PZ Podzols: a placic phase].")
  expect_identical(line("fao1974", c("G", "Gp")),
                   "Revised Legend (1988): no class of 1988 (deleted, App. 4 p. 116) [or PT Plinthosols: plinthite within 125 cm].")
  # a 1988 result that stops at the grouping names the deleted unit
  expect_match(line("fao1988", "SN"), "[some Ws Solodic Planosols, a natric B]", fixed = TRUE)
  expect_match(line("fao1988", "PZ"), "[some Pp Placic Podzols, a placic phase]", fixed = TRUE)
})

test_that("R17: the new conditions have their Portuguese", {
  skip_on_cran()
  pt <- yaml::read_yaml(system.file("i18n", "fao_crosswalk_pt.yaml", package = "soilKey"))
  for (k in c("a natric B", "a sodic phase", "a placic phase")) expect_false(is.null(pt[[k]]), info = k)
})

# ---- D4, D5: the text of the readings and notes ------------------------------

test_that("D4: Q24 says the Chromic units read the rubbed colour, in both editions", {
  skip_on_cran()
  for (e in c("fao1974", "fao1988")) {
    rd <- yaml::read_yaml(system.file("rules", e, "readings.yaml", package = "soilKey"))$readings
    q24 <- Filter(function(z) identical(z$topic, "Rubbed colours and the limits between Munsell chips"), rd)
    expect_length(q24, 1L)
    expect_false(grepl("Chromic reads the moist colour", q24[[1]]$reading, fixed = TRUE), info = e)
    expect_match(q24[[1]]$reading, "the Chromic units read it too", fixed = TRUE, info = e)
  }
})

test_that("D5: the Gc to GLk note cites p. 79 for calcic and ch. VI p. 44 for gypsic", {
  skip_on_cran()
  gc <- fao_crosswalk("Gc")
  expect_identical(gc$code_1988, "GLk")
  expect_false(grepl("calcic or gypsic horizon, p. 79", gc$note, fixed = TRUE))
  expect_match(gc$note, "a calcic horizon in the key, p. 79", fixed = TRUE)
  expect_match(gc$note, "ch. VI p. 44", fixed = TRUE)
})

test_that("R10 and R17 are recorded in the readings of 1988", {
  skip_on_cran()
  rd <- yaml::read_yaml(system.file("rules", "fao1988", "readings.yaml", package = "soilKey"))$readings
  v <- Filter(function(z) identical(z$version, "0.9.440") && grepl("2026-11-24", z$reading, fixed = TRUE), rd)
  expect_length(v, 2L)
  txt <- vapply(v, function(z) z$reading, "")
  expect_true(all(grepl("option a) (Hugo, 2026-11-24)", txt, fixed = TRUE)))
  for (k in c("R10", "R17")) expect_true(any(grepl(k, txt, fixed = TRUE)), info = k)
})

# ---- 2026-11-25: carbonate recorded only in a form that is not soft ----------

.h440 <- function(...) {
  cols <- list(...)
  n <- max(lengths(cols))
  h <- make_empty_horizons(n)
  for (k in names(cols)) data.table::set(h, j = k, value = rep_len(cols[[k]], n))
  h
}
.it440 <- function(x) list(item = x, description = NULL)
.isis440 <- function(layers) list(profileCode = "XX440", countryCode = NULL, layers = layers,
                                  wrbClassifications = list(), usdaClassifications = list())

test_that("a form that is not soft makes the soft powdery lime absent; no form keeps R13", {
  skip_on_cran()
  h <- .h440(top_cm = c(0, 20, 40, 60, 80), bottom_cm = c(20, 40, 60, 80, 100),
             secondary_carbonates_pct = c(NA, 10, 10, 10, 10),
             carbonate_form_not_soft = c(NA, "hard", "crystals", NA, "hard"),
             carbonate_soft_pct = c(NA, NA, NA, NA, 6))
  p <- suppressWarnings(PedonRecord$new(site = list(id = "p440"), horizons = h))
  x <- soilKey:::.f88_ctx(p)
  v <- soilKey:::.f88_spl_v(x)
  # hard and crystals: absent; no form: undecided (R13); soft nodules decide
  expect_identical(v$value, c(FALSE, FALSE, FALSE, NA, TRUE))
  expect_identical(v$flags[2:4], c("secondary_carbonate_form_not_soft", "secondary_carbonate_form_not_soft",
                                   "secondary_carbonate_without_form"))
  # a soft or coating form recorded still rules over the column
  h2 <- .h440(top_cm = 0, bottom_cm = 50, secondary_carbonates_pct = 10,
              carbonate_form_not_soft = "hard", secondary_carbonates_form = "soft concretions")
  p2 <- suppressWarnings(PedonRecord$new(site = list(id = "p440b"), horizons = h2))
  expect_true(is.na(soilKey:::.f88_spl_v(soilKey:::.f88_ctx(p2))$value))
})

test_that("the ISIS reader carries hard and crystal calcareous inclusions, nothing else", {
  skip_on_cran()
  K <- .it440("K")
  lay <- function(top, bot, ...) c(list(upperDepth = top, lowerDepth = bot, layerName = "Bk",
                                        inclusionsComposition = K, mineralConcAbundance = .it440("C")), list(...))
  L <- list(
    list(upperDepth = 0, lowerDepth = 20, layerName = "A"),
    lay(20, 40, concretionsMineralHardness = .it440("H"), inclusionsType = .it440("C")),   # hard
    lay(40, 60, concretionsMineralHardness = .it440("S"), inclusionsType = .it440("T")),   # soft crystals
    lay(60, 80, inclusionsType = .it440("C")),                                            # no hardness
    lay(80, 100, concretionsMineralHardness = .it440("B"), inclusionsType = .it440("N")),  # both: no
    lay(100, 120, concretionsMineralHardness = .it440("S"), inclusionsType = .it440("N")), # soft (R14)
    # a hard inclusion beside one with no hardness: the form is not known
    lay(120, 140, concretionsMineralHardness = .it440("H"), inclusionsType = .it440("C"),
        inclusionsComposition2 = K, mineralConcAbundance2 = .it440("F"), inclusionsType2 = .it440("N")),
    # hard and crystals together
    lay(140, 160, concretionsMineralHardness = .it440("H"), inclusionsType = .it440("N"),
        inclusionsComposition2 = K, mineralConcAbundance2 = .it440("F"), inclusionsType2 = .it440("T")))
  p <- suppressWarnings(isis_profile_to_pedon(.isis440(L)))
  h <- as.data.frame(p$horizons)
  expect_identical(h$carbonate_form_not_soft,
                   c(NA, "hard", "crystals", NA, NA, NA, NA, "crystals; hard"))
  # the columns the other systems read are as in 0.9.439
  expect_true(all(is.na(h$secondary_carbonates_form)))
  expect_true(all(is.na(h$carbonate_hard_pct)))
  expect_equal(h$carbonate_soft_pct, c(NA, NA, NA, NA, NA, 10, NA, NA))
  pv <- as.data.frame(p$provenance)
  expect_true(any(pv$attribute == "carbonate_form_not_soft" & grepl("2026-11-25", pv$notes)))
  spl <- soilKey:::.f88_spl_v(soilKey:::.f88_ctx(p))$value
  expect_identical(spl[2:8], c(FALSE, FALSE, NA, NA, TRUE, NA, FALSE))
})

test_that("no other system reads carbonate_form_not_soft", {
  skip_on_cran()
  rfiles <- list.files(test_path("..", "..", "R"), pattern = "[.]R$", full.names = TRUE)
  skip_if(!length(rfiles), "package sources not available")
  hit <- vapply(rfiles, function(f) any(grepl("carbonate_form_not_soft", readLines(f, warn = FALSE), fixed = TRUE)), NA)
  expect_setequal(basename(rfiles[hit]), c("diagnostics-fao88-properties.R", "isis-graphql.R", "utils.R"))
})

test_that("the decision of 2026-11-25 is recorded in the readings of both editions", {
  skip_on_cran()
  for (e in c("fao1974", "fao1988")) {
    rd <- yaml::read_yaml(system.file("rules", e, "readings.yaml", package = "soilKey"))$readings
    v <- Filter(function(z) identical(z$version, "0.9.440") && grepl("(Hugo, 2026-11-25)", z$reading, fixed = TRUE), rd)
    expect_length(v, 1L)
    expect_match(v[[1]]$reading, "carbonate_form_not_soft", fixed = TRUE, info = e)
  }
})
