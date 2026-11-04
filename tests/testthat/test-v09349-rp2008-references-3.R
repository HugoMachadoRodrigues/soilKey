# v0.9.349: the French Referentiel pedologique 2008, part 6 -- the 18
# references of the histosols (5), leptismectisols (1), lithosols (1),
# luvisols (6), nitosols (1) and organosols (4) (pp. 201-242;
# inst/rules/rp2008/references/*.yaml, R/diagnostics-rp.R, R/key-rp2008.R),
# on the attachment of part 4 (0.9.346) and the references of part 5
# (0.9.348). Hugo's decisions Q5, Q6, Q7, Q8, Q17, Q19 and the defaults of
# map 3; the nine readings of this part wait for his word in readings.yaml.

.rp49_refs <- c("histosols_fibriques", "histosols_mesiques", "histosols_sapriques", "histosols_composites",
                "histosols_leptiques", "leptismectisols", "lithosols", "neoluvisols", "luvisols_typiques",
                "luvisols_degrades", "luvisols_derniques", "luvisols_tronques", "quasi_luvisols", "nitosols",
                "organosols_holorganiques", "organosols_calcaires", "organosols_satures", "organosols_insatures")
.rp49_readings <- c("Lithosols: the obstacle at 10 cm or less",
                    "A cuirasse or carapace within 10 cm in the tropical chapters",
                    "Histosols: the solum under the H, the windows and the kinds", "Luvisols: the texture contrast",
                    "Luvisols: the sequences", "Luvisols tronqu\u00e9s and quasi-luvisols",
                    "Leptismectisols: the saturation, the smectites and the V",
                    "Organosols: the Aho, the thin horizons and the drainage", "The doubles of part 6",
                    "Lithosols: one horizon over the obstacle (decision 2)",
                    "Organosols and alocrisols humiques: the Sal (decision 5)",
                    "Luvisols against the fersialsols \u00e9luviques: the BT (decision 6)",
                    "Luvisols: an h on the E or the BT (decision 7)",
                    "Luvisols tronqu\u00e9s before the typiques (decision 8)",
                    "Organosols against the brunisols with no A (decision 9)")

# A pedon in the RP notation (France, Koppen Cfb, unless the site says
# otherwise), or on the data (des = NULL, rp = FALSE).
.rp49_N <- function(des, top, bot, ..., site = list(), rp = TRUE) {
  h <- data.frame(top_cm = top, bottom_cm = bot, stringsAsFactors = FALSE)
  if (!is.null(des)) h$designation <- des
  a <- list(...)
  for (k in names(a)) h[[k]] <- rep_len(a[[k]], nrow(h))
  base <- list(id = "t49", country = "FR", lat = 46, koppen_climate = "Cfb")
  if (rp) base$designation_system <- "rp2008"
  PedonRecord$new(site = utils::modifyList(base, site),
                  horizons = ensure_horizon_schema(data.table::as.data.table(h)))
}
# a peat: very poorly drained
.rp49_H <- function(des, top, bot, ..., site = list())
  .rp49_N(des, top, bot, ..., site = utils::modifyList(list(drainage_class = "very poorly drained"), site))
# a pedon with some horizon values (rows) or site values changed
.rp49_edit <- function(p, rows = NULL, ..., site = list()) {
  h <- as.data.frame(p$horizons)
  ch <- list(...)
  for (k in names(ch)) {
    if (!k %in% names(h)) h[[k]] <- NA
    h[[k]][rows] <- ch[[k]]
  }
  s <- p$site
  for (k in names(site)) s[[k]] <- site[[k]]
  PedonRecord$new(site = s, horizons = ensure_horizon_schema(data.table::as.data.table(h)))
}
.rp49_deg <- function(p, code) {
  rules <- .rp_rules()
  fr <- .rp_frame_codes(.rp_identify(p))
  .rp_with_state(p, fr, .rp_profile(p),
                 .rp_eval_reference(rules$ref_by_code[[code]], p, .tolerance_table(rules$tolerances)))
}
.rp49_cl <- function(p) classify_rp2008(p, on_missing = "silent")
.rp49_ex <- make_rp2008_example
.rp49_deg_of <- function(r, code) r$candidates$degree[r$candidates$code == code]


# --- the catalogue and the rules -----------------------------------------------------

test_that("twenty-one GER and 71 references are implemented; the six of part 6 lose their signposts", {
  skip_on_cran()
  rules <- .rp_rules()
  impl <- Filter(function(g) isTRUE(g$implemented), rules$ger)
  expect_length(impl, 21L)
  six <- c("histosols", "leptismectisols", "lithosols", "luvisols", "nitosols", "organosols")
  expect_identical(vapply(impl, `[[`, "", "code")[16:21], six)
  expect_identical(unlist(lapply(impl[16:21], function(g) unlist(g$references))), .rp49_refs)
  expect_length(rules$references, 71L)
  expect_identical(names(rules$ref_by_code)[54:71], .rp49_refs)
  for (g in impl[16:21]) expect_null(g$signpost, info = g$code)
  for (r in rules$references[54:71]) {
    expect_true(all(c("code", "ger", "name", "name_singular", "name_en", "name_en_singular", "page", "tests",
                      "wrb2006") %in% names(r)), info = r$code)
  }
  later <- Filter(function(g) !isTRUE(g$implemented), rules$ger)
  expect_length(later, 13L)
  expect_identical(sum(vapply(later, function(g) length(g$references), 0L)), 39L)
  # the nitosols are a first version of the book (p. VI, Q18), their climate
  # and the habitat notes of the histosols are reported, not tested
  expect_true(isTRUE(rules$ger_by_code$nitosols$first_draft))
  expect_null(rules$ger_by_code$luvisols$first_draft)
  expect_match(rules$ger_by_code$nitosols$result_note, "not tested", fixed = TRUE)
  expect_match(rules$ger_by_code$histosols$result_note, "D18", fixed = TRUE)
  # the validator passes; the rapid key reaches the six by the book
  v <- .validate_rules("rp2008")
  expect_identical(c(v$missing, v$unsuffixed, v$mixed, v$malformed, v$codes), character(0))
  ck <- .rp_rapid_key_check()
  expect_true(all(ck$by_the_book[ck$ger %in% six]))
  expect_true(all(ck$implemented[ck$ger %in% six]))
  ord <- .rp_order(rules)
  # lithosols (1) before histosols (4c) before leptismectisols (6d) before
  # nitosols (6f) before organosols (7a) before brunisols (11a) before
  # luvisols (11b)
  expect_true(all(diff(match(c("lithosols", "histosols_fibriques", "leptismectisols", "nitosols",
                               "organosols_holorganiques", "brunisols_eutriques", "neoluvisols"), ord)) > 0))
  # the doubles: luvisol-cryosol (p. 141) and organosol insature-alocrisol
  # humique (p. 240) written; those with the redoxisols and reductisols wait
  dd <- rules$doubles
  act <- Filter(function(d) !identical(d$status, "later") && any(unlist(d$codes) %in% .rp49_refs), dd)
  expect_length(act, 7L)
  lat <- Filter(function(d) identical(d$status, "later") &&
                  (any(unlist(d$codes) %in% .rp49_refs) || "histosols" %in% unlist(d$codes)), dd)
  expect_length(lat, 4L)
  expect_false(any(vapply(dd, function(d) identical(unlist(d$codes), c("cryosols_mineraux", "luvisols")), NA)))
})

test_that("the readings of part 6 are Hugo's decisions of 2026-11-04; none waits for his word", {
  skip_on_cran()
  rd <- yaml::read_yaml(system.file("rules", "rp2008", "readings.yaml", package = "soilKey"))$readings
  tp <- vapply(rd, `[[`, "", "topic")
  txt <- vapply(rd, `[[`, "", "reading")
  expect_true(all(.rp49_readings %in% tp))
  expect_true(all(grepl("(Hugo, 2026-11-04).", txt[match(.rp49_readings, tp)], fixed = TRUE)))
  expect_false(any(grepl("to confirm", txt, fixed = TRUE)))
  expect_true(all(nzchar(vapply(rd[match(.rp49_readings, tp)], `[[`, "", "page"))))
})


# --- every reference: a simple fixture, an imperfect one, a failing twin ---------------

test_that("each example attaches simply to its own reference, in the name, and is not provisional", {
  skip_on_cran()
  for (cd in .rp49_refs) {
    r <- .rp49_cl(.rp49_ex(cd))
    expect_identical(r$levels$code, cd, info = cd)
    expect_identical(.rp49_deg_of(r, cd), "simple", info = cd)
    expect_identical(r$provisional_because, character(0), info = cd)
  }
  # the Pro app offers them
  ui <- readLines(system.file("shiny", "classify_app_pro", "R", "utils_ui.R", package = "soilKey"), encoding = "UTF-8")
  for (cd in .rp49_refs) expect_true(any(grepl(sprintf("\"%s\"", cd), ui, fixed = TRUE)), info = cd)
})

test_that("a value inside a tolerance band attaches imperfectly; a failing twin fails", {
  skip_on_cran()
  H <- .rp49_H
  N <- .rp49_N
  ex <- .rp49_ex
  e <- .rp49_edit
  cases <- list(
    # Hf 55 cm in all: within the 60 +/- 10 of the fibriques
    histosols_fibriques = list(H(c("Hf", "Hm", "M"), c(0, 55, 65), c(55, 65, 100)),
                               H(c("Hf", "Hm", "M"), c(0, 45, 55), c(45, 55, 100))),
    # H 45 cm in all: within the 50 +/- 10 of the figure of p. 209; the 40 of
    # the mesiques and sapriques is strict
    histosols_mesiques = list(H(c("Hm", "M"), c(0, 45), c(45, 100)), H(c("Hm", "M"), c(0, 38), c(38, 100))),
    histosols_sapriques = list(H(c("Hs", "M"), c(0, 45), c(45, 100)), H(c("Hs", "M"), c(0, 38), c(38, 100))),
    histosols_composites = list(H(c("Hf", "Hm", "M"), c(0, 20, 45), c(20, 45, 100)), H(c("Hf", "M"), c(0, 90), c(90, 130))),
    # H 55 cm over R: within the 50 +/- 10 of the leptiques
    histosols_leptiques = list(H(c("Hs", "R"), c(0, 55), c(55, 100)), H(c("Hs", "R"), c(0, 65), c(65, 100))),
    # (Ca + Mg) / CEC 88 %: within the 90 +/- 5 (default 9)
    leptismectisols = list(e(ex("leptismectisols"), 1:2, ca_cmol = c(34.5, 36)), e(ex("leptismectisols"), 1:2, ca_cmol = c(30, 30))),
    # the R at 12 cm: within the 10 +/- 5
    lithosols = list(e(ex("lithosols"), 1:2, top_cm = c(0, 12), bottom_cm = c(12, 60)),
                     e(ex("lithosols"), 1:2, top_cm = c(0, 20), bottom_cm = c(20, 60))),
    # an E of 12 cm between the A and the BT: within the 10 +/- 5
    luvisols_tronques = list(N(c("A", "E", "BT", "C"), c(0, 10, 22, 80), c(10, 22, 80, 120), clay_pct = c(25, 22, 30, 22)),
                             N(c("A", "E", "BT", "C"), c(0, 10, 30, 80), c(10, 30, 80, 120), clay_pct = c(25, 22, 30, 22))),
    # [OF + OH] 8 cm: within the 10 +/- 5 of the organosols
    organosols_holorganiques = list(e(ex("organosols_holorganiques"), 2:4, designation = c("OF", "OH", "R"),
                                      top_cm = c(3, 8, 11), bottom_cm = c(8, 11, 60)),
                                    e(ex("organosols_holorganiques"), 2:4, designation = c("OF", "A", "R"))),
    organosols_calcaires = list(N(c("Acaho", "C"), c(0, 8), c(8, 60), oc_pct = c(12, 1), caco3_pct = c(15, 40)),
                                e(ex("organosols_calcaires"), 3, designation = "Aca")),
    organosols_satures = list(e(ex("organosols_satures"), 2, bs_pct = 77), e(ex("organosols_satures"), 2, bs_pct = 60)),
    organosols_insatures = list(e(ex("organosols_insatures"), 3, bs_pct = 83), e(ex("organosols_insatures"), 3, bs_pct = 92))
  )
  for (cd in names(cases)) {
    a <- .rp49_deg(cases[[cd]][[1]], cd)
    b <- .rp49_deg(cases[[cd]][[2]], cd)
    expect_identical(a$degree, "imperfect", info = cd)
    expect_gt(length(a$imperfect_by), 0L)
    expect_identical(b$degree, "fail", info = cd)
    expect_gt(length(b$failed), 0L)
  }
  # the references with no threshold of p. 64 attach simply or not at all:
  # their failing twins
  fails <- list(
    neoluvisols = e(ex("neoluvisols"), 3, clay_pct = 32),
    luvisols_typiques = e(ex("luvisols_typiques"), 3, clay_pct = 20),
    luvisols_degrades = e(ex("luvisols_degrades"), 3, designation = "BTg"),
    luvisols_derniques = e(ex("luvisols_derniques"), 4, clay_pct = 18),
    quasi_luvisols = e(ex("quasi_luvisols"), 2:3, designation = c("BT", "C")),
    nitosols = e(ex("nitosols"), 2, designation = "S")
  )
  expect_identical(sort(c(names(cases), names(fails))), sort(.rp49_refs))
  for (cd in names(fails)) expect_identical(.rp49_deg(fails[[cd]], cd)$degree, "fail", info = cd)
  # the example of p. 239: an S of 10 cm under [OF + OH] of 40 cm is
  # tolerated; of 15 cm the solum goes to another reference (Hugo's
  # decision 9 on the review, 2026-11-04: an S over 10 cm fails, strict)
  S <- function(s) N(c("OF", "OH", "S", "C"), c(0, 20, 40, 40 + s), c(20, 40, 40 + s, 90), oc_pct = c(40, 35, 2, 0.5),
                     bs_pct = c(NA, NA, 70, 70), ph_h2o = 6, clay_pct = c(NA, NA, 20, 15))
  expect_identical(.rp49_deg(S(10), "organosols_holorganiques")$degree, "simple")
  expect_identical(.rp49_deg(S(15), "organosols_holorganiques")$degree, "fail")
  expect_identical(.rp49_deg(S(25), "organosols_holorganiques")$degree, "fail")
})


# --- the histosols --------------------------------------------------------------------

test_that("the histosols: the predominant H between 40 and 120 cm, its thickness in the solum (D1, D3)", {
  skip_on_cran()
  H <- .rp49_H
  nm <- function(p) .rp49_cl(p)$name
  # Hm thicker in all, Hf predominant in the window
  expect_identical(nm(H(c("Hm", "Hf", "M"), c(0, 35, 130), c(35, 130, 160))), "histosol fibrique")
  # equal thicknesses in the window: no kind truly predominant
  expect_identical(nm(H(c("Hf", "Hm", "M"), c(0, 80, 140), c(80, 140, 180))), "histosol composite")
  # Hm predominant but 40 cm in all: not more than 40, composite
  expect_identical(nm(H(c("Hf", "Hm", "Hs", "M"), c(0, 30, 70, 110), c(30, 70, 110, 150))), "histosol composite")
  # a drained Ha predominant counts for no kind (D3)
  expect_identical(nm(H(c("Ha", "Hm", "M"), c(0, 90, 110), c(90, 110, 150))), "histosol composite")
  # the window 0-50 when an M, D or R starts within 50 cm
  v <- .rp_with_state(p <- H(c("Hf", "Hs", "R"), c(0, 10, 45), c(10, 45, 80)), .rp_frame_codes(.rp_identify(p)),
                      .rp_profile(p), histic_kind_rp(p, "Hs"))
  expect_true(v$passed)
  expect_identical(v$evidence$window, "0-50")
  # the solum: only G, M, D, R and thin materials under the H; a cover of 50
  # cm at most (recouvert)
  expect_identical(nm(H(c("A", "Hm", "M"), c(0, 30, 100), c(30, 100, 140))), "histosol m\u00e9sique")
  expect_identical(.rp49_deg(H(c("A", "Hm", "M"), c(0, 70, 140), c(70, 140, 180)), "histosols_mesiques")$degree, "fail")
  expect_identical(.rp49_deg(H(c("Hm", "S", "C"), c(0, 80, 110), c(80, 110, 140)), "histosols_mesiques")$failed,
                   "histic_solum_rp")
  expect_identical(nm(H(c("Hm", "Mt", "Hm", "G"), c(0, 50, 70, 130), c(50, 70, 130, 160))), "histosol m\u00e9sique")
})

test_that("the histosols against the lithosols, r\u00e9gosols, r\u00e9ductisols and cryosols (pp. 207-211)", {
  skip_on_cran()
  H <- .rp49_H
  # H of 10 cm or less over R: a lithosol epihistique; over 10 cm, a histosol leptique
  r <- .rp49_cl(H(c("Hs", "R"), c(0, 8), c(8, 60)))
  expect_identical(r$name, "lithosol")
  expect_identical(.rp49_deg_of(r, "histosols_leptiques"), "imperfect")
  expect_identical(.rp49_cl(H(c("Hs", "R"), c(0, 12), c(12, 60)))$name, "histosol leptique")
  # over M: the regosols of a later release make the name provisional
  r <- .rp49_cl(H(c("Hs", "M"), c(0, 8), c(8, 60)))
  expect_true("references:regosols" %in% r$provisional_because)
  # H under 50 cm over G: no histosol leptique; the reductisols epihistiques
  # (a later release) make the name provisional
  r <- .rp49_cl(H(c("Hs", "G"), c(0, 40), c(40, 100)))
  expect_identical(.rp49_deg_of(r, "histosols_leptiques"), character(0))
  expect_true("references:reductisols_redoxisols" %in% r$provisional_because)
  # H with a permafrost within 2 m: the cryosol histique (p. 211)
  r <- .rp49_cl(.rp49_edit(.rp49_ex("histosols_fibriques"), site = list(frost_regime = "permafrost")))
  expect_identical(r$name, "cryosol histique")
  expect_match(r$candidates$precedence[r$candidates$code == "histosols_fibriques"], "cryosols_histiques first", fixed = TRUE)
})

test_that("the H kinds on the data: rubbed fibres, then von Post; H against O by the water (D4, Q19)", {
  skip_on_cran()
  d <- function(..., site = list()) .rp49_N(NULL, ..., site = site, rp = FALSE)
  p <- d(c(0, 90, 120), c(90, 120, 160), site = list(drainage_class = "very poorly drained", elevation_m = 800),
         oc_pct = c(45, 40, NA), loi_pct = c(90, 85, NA), fiber_content_rubbed_pct = c(60, 25, NA),
         rupture_resistance = c(NA, NA, "extremely hard"), redoximorphic_features_pct = 0)
  r <- .rp49_cl(p)
  expect_identical(r$name, "histosol fibrique")
  expect_identical(r$trace$horizons$code, c("Hf", "Hm", "R"))
  expect_true(all(r$trace$horizons$route == "data"))
  # von Post 5 lies between Hf and Hm: the kind open, the histosols undecided
  # and the name provisional, never a silent FALSE
  p2 <- .rp49_edit(p, 1:2, fiber_content_rubbed_pct = c(NA, NA), von_post_index = c(5, 5))
  r <- .rp49_cl(p2)
  expect_identical(.rp49_deg_of(r, "histosols_fibriques"), "undecided")
  expect_true(any(grepl("^references:histosols_", r$provisional_because)))
  # an H written alone in the RP notation reads the kind of the data
  p3 <- .rp49_H(c("H", "M"), c(0, 90), c(90, 130), fiber_content_rubbed_pct = c(60, NA), oc_pct = c(45, NA))
  expect_identical(.rp49_cl(p3)$name, "histosol fibrique")
  expect_identical(.rp49_deg(.rp49_H(c("H", "M"), c(0, 90), c(90, 130)), "histosols_fibriques")$degree, "undecided")
  # an organic layer with no water recorded: H or O undecided, both GER (Q19)
  p4 <- d(c(0, 60), c(60, 100), oc_pct = c(40, NA), rupture_resistance = c(NA, "extremely hard"))
  r <- .rp49_cl(p4)
  expect_identical(.rp49_deg_of(r, "histosols_leptiques"), "undecided")
  expect_identical(.rp49_deg_of(r, "organosols_holorganiques"), "undecided")
})


# --- the leptismectisols and the lithosols ----------------------------------------------

test_that("the leptismectisols: no V, [Av + SV] 50 cm at most, saturated by Ca and Mg (pp. 217-218)", {
  skip_on_cran()
  ex <- .rp49_ex("leptismectisols")
  e <- .rp49_edit
  # with no V, before the signpost of the vertisols (rapid key 6c needs a V)
  r <- .rp49_cl(ex)
  expect_true("vertisols" %in% r$candidates$code)
  expect_match(r$candidates$precedence[r$candidates$code == "vertisols"], "leptismectisols first", fixed = TRUE)
  # a V: a vertisol of a later release
  r <- .rp49_cl(e(ex, 2, designation = "V"))
  expect_identical(r$name, "[non rattach\u00e9]")
  expect_true("references:vertisols" %in% r$provisional_because)
  # [Av + SV] over 60 cm fails; a recorded kaolinite fails; the saturation not
  # measured leaves it undecided
  expect_identical(.rp49_deg(e(ex, 2:3, bottom_cm = c(70, 100), top_cm = c(18, 70)), "leptismectisols")$degree, "fail")
  expect_identical(.rp49_deg(e(ex, 1:2, clay_mineralogy = c("kaolinite", "kaolinite")), "leptismectisols")$degree, "fail")
  expect_identical(.rp49_deg(e(ex, 1:2, ca_cmol = c(NA, NA)), "leptismectisols")$degree, "undecided")
  # the lower limit: R, Kc, Km or C
  expect_identical(.rp49_deg(e(ex, 3, designation = "Kc"), "leptismectisols")$degree, "simple")
  expect_identical(.rp49_deg(e(ex, 3, designation = "M"), "leptismectisols")$degree, "fail")
})

test_that("the lithosols: an obstacle within 10 cm, the OL not counted (p. 219)", {
  skip_on_cran()
  N <- .rp49_N
  for (o in c("R", "Km", "Fem", "Sim")) expect_identical(.rp49_cl(N(c("A", o), c(0, 6), c(6, 60)))$name, "lithosol", info = o)
  # the OL is not counted
  expect_identical(.rp49_cl(N(c("OL", "A", "R"), c(0, 5, 13), c(5, 13, 60)))$name, "lithosol")
  # bare rock
  expect_identical(.rp49_cl(N("R", 0, 60))$name, "lithosol")
  # a loose M is a regosol's (a later release)
  r <- .rp49_cl(N(c("A", "M"), c(0, 6), c(6, 60)))
  expect_identical(.rp49_deg(N(c("A", "M"), c(0, 6), c(6, 60)), "lithosols")$degree, "fail")
  expect_true("regosols" %in% r$candidates$code)
  # the cuirasse of the tropical chapters with their horizons under it: their
  # reference, not the lithosol (A optional, p. 175; Q17)
  r <- .rp49_cl(N(c("OXm", "Ea", "BTg", "C"), c(0, 30, 60, 120), c(30, 60, 120, 150), clay_pct = 25, sand_pct = 60,
                  site = list(country = "BF", lat = 11.2, koppen_climate = "Aw")))
  expect_identical(r$name, "ferruginosol p\u00e9troxydique")
  expect_identical(.rp49_deg_of(r, "lithosols"), "simple")
  # the OXm is a horizon of the intertropical domain (Q18): no obstacle in France
  expect_identical(.rp49_deg(N(c("A", "OXm", "C"), c(0, 5, 40), c(5, 40, 90)), "lithosols")$degree, "fail")
})


# --- the luvisols ----------------------------------------------------------------------

test_that("the luvisols: the texture contrast of the n\u00e9oluvisols and typiques by the bands of pp. 225-226", {
  skip_on_cran()
  L <- function(e, b) .rp49_N(c("A", "E", "BT", "C"), c(0, 15, 40, 100), c(15, 40, 100, 140),
                              clay_pct = c(e, e, b, b - 2), clay_films_amount = c(NA, NA, "many", NA))
  nm <- function(e, b) .rp49_cl(L(e, b))$name
  # E 10 to 30 %: the index 1.3 to 1.8 neo, over 1.8 typique
  expect_identical(nm(15, 19.5), "n\u00e9oluvisol")
  expect_identical(nm(15, 27), "n\u00e9oluvisol")
  expect_identical(nm(15, 27.1), "luvisol typique")
  # E under 10 %: 3 points more neo, 8 typique
  expect_identical(nm(8, 11), "n\u00e9oluvisol")
  expect_identical(nm(8, 16), "luvisol typique")
  # E over 30 %: 9 points more neo, 24 typique
  expect_identical(nm(35, 44), "n\u00e9oluvisol")
  expect_identical(nm(35, 59), "luvisol typique")
  # under the neoluvisols' minimum: no luvisol (and the brunisols forbid the BT)
  expect_identical(nm(15, 18), "[non rattach\u00e9]")
  v <- .rp_with_state(p <- L(15, 27), .rp_frame_codes(.rp_identify(p)), .rp_profile(p), luvic_contrast_rp(p, "neo"))
  expect_equal(v$evidence$idt, 1.8)
  # the clay not recorded: undecided, the name provisional
  r <- .rp49_cl(.rp49_edit(L(15, 30), 2:3, clay_pct = c(NA, NA)))
  expect_identical(.rp49_deg_of(r, "luvisols_typiques"), "undecided")
  # under crops with no E the ploughed horizon stands for it (LA-BT, D12)
  P <- function(b) .rp49_N(c("LA", "BT", "C"), c(0, 30, 90), c(30, 90, 130), clay_pct = c(15, b, 20),
                           clay_films_amount = c(NA, "many", NA))
  expect_identical(.rp49_cl(P(24))$name, "n\u00e9oluvisol")
  expect_identical(.rp49_cl(P(30))$name, "luvisol typique")
  # no contrast left under the plough: a luvisol tronque
  expect_identical(.rp49_cl(P(17))$name, "luvisol tronqu\u00e9")
})

test_that("the luvisols d\u00e9grad\u00e9s, derniques, tronqu\u00e9s and quasi-luvisols (pp. 226-228)", {
  skip_on_cran()
  ex <- .rp49_ex
  e <- .rp49_edit
  # a dernique also meets the degrades: the Ea on the BTd names it (D9)
  r <- .rp49_cl(ex("luvisols_derniques"))
  expect_identical(.rp49_deg_of(r, "luvisols_degrades"), "simple")
  expect_match(r$candidates$precedence[r$candidates$code == "luvisols_degrades"], "luvisols_derniques first", fixed = TRUE)
  # an index under 1.8 is no dernique: a degrade
  expect_identical(.rp49_cl(e(ex("luvisols_derniques"), 4, clay_pct = 18))$name, "luvisol d\u00e9grad\u00e9")
  # a BTd makes no neoluvisol or typique
  expect_identical(.rp49_deg(e(ex("luvisols_typiques"), 3, designation = "BTd"), "luvisols_typiques")$degree, "fail")
  # under forest a tronque has its BT directly under the A: an S between is
  # a chernosol's sequence, not a truncated luvisol
  expect_identical(.rp49_deg(.rp49_N(c("Ach", "Sh", "BT", "Ck"), c(0, 50, 90, 150), c(50, 90, 150, 190),
                                     oc_pct = c(3.5, 1, 0.3, 0.2), clay_pct = c(30, 32, 45, 28), bs_pct = 90),
                             "luvisols_tronques")$degree, "fail")
  # an LBT at the surface: tronque under crops
  expect_identical(.rp49_cl(.rp49_N(c("LBT", "BT", "C"), c(0, 25, 80), c(25, 80, 120), clay_pct = c(28, 30, 22)))$name,
                   "luvisol tronqu\u00e9")
  # the quasi-luvisols need the discontinuity written; the others read the
  # first material only
  expect_identical(.rp49_deg(ex("quasi_luvisols"), "luvisols_typiques")$degree, "fail")
  expect_identical(.rp49_cl(e(ex("quasi_luvisols"), 2:3, designation = c("BT", "C")))$name, "luvisol typique")
  # FFE over total iron of the BT over 0.20, measured: no luvisol (p. 230)
  p <- e(ex("luvisols_typiques"), 3, fe_ffe_pct = 2, fe_total_pct = 6)
  expect_identical(.rp49_deg(p, "luvisols_typiques")$degree, "fail")
  expect_identical(.rp49_deg(e(p, 3, fe_ffe_pct = 0.5), "luvisols_typiques")$degree, "simple")
})

test_that("the luvisols on the data, and their doubles with the cryosols and r\u00e9doxisols", {
  skip_on_cran()
  d <- .rp49_N(NULL, c(0, 15, 50, 110), c(15, 50, 110, 150), site = list(land_use = "deciduous forest"), rp = FALSE,
               clay_pct = c(14, 13, 28, 22), sand_pct = 10, silt_pct = c(76, 77, 62, 68), oc_pct = c(2, 0.5, 0.3, 0.1),
               ph_h2o = c(5.5, 5.6, 6, 6.5), caco3_pct = 0, munsell_hue_moist = "10YR", munsell_value_moist = c(4, 6, 5, 5),
               munsell_chroma_moist = c(2, 3, 5, 4), structure_type = c("granular", "platy", "angular blocky", "massive"),
               structure_grade = c("moderate", "weak", "strong", NA), clay_films_amount = c(NA, "none", "many", "none"),
               redoximorphic_features_pct = 0)
  r <- .rp49_cl(d)
  expect_identical(r$name, "luvisol typique")
  expect_identical(r$trace$horizons$code[2:3], c("E", "BT"))
  # cryoturbated, with a permafrost: luvisol-cryosol (p. 141)
  r <- .rp49_cl(.rp49_edit(.rp49_ex("luvisols_typiques"), 2:3, cryoturbation = c("present", "present"),
                           site = list(frost_regime = "permafrost")))
  expect_identical(r$name, "luvisol typique-cryosol min\u00e9ral")
  expect_identical(r$levels$code, c("luvisols_typiques", "cryosols_mineraux"))
  # a -g within 50 cm: the redoxisols of a later release (Q8), provisional
  r <- .rp49_cl(.rp49_edit(.rp49_ex("luvisols_typiques"), 2, designation = "Eg"))
  expect_identical(r$name, "luvisol typique")
  expect_true("references:reductisols_redoxisols" %in% r$provisional_because)
})


# --- the nitosols and the organosols -----------------------------------------------------

test_that("the nitosols: the Sn and its halloysite, before the brunisols (pp. 233-236)", {
  skip_on_cran()
  ex <- .rp49_ex("nitosols")
  e <- .rp49_edit
  r <- .rp49_cl(ex)
  expect_true(any(grepl("first version of the book", r$warnings, fixed = TRUE)))
  expect_true(any(grepl("not tested", r$warnings, fixed = TRUE)))
  # the brunisols read the Sn as an S: the nitosol first (p. 236)
  r <- .rp49_cl(e(ex, 1:3, bs_pct = c(60, 55, 60)))
  expect_identical(r$name, "nitosol")
  expect_identical(.rp49_deg_of(r, "brunisols_eutriques"), "simple")
  expect_match(r$candidates$precedence[r$candidates$code == "brunisols_eutriques"], "nitosols first", fixed = TRUE)
  # an F with the Sn: the ferrallitisol meuble polylithique of p. 157
  r <- .rp49_cl(.rp49_N(c("A", "Sn", "F", "II Sn", "II F", "II RT", "C", "R"), c(0, 10, 40, 90, 130, 200, 300, 400),
                        c(10, 40, 90, 130, 200, 300, 400, 420), site = list(country = "CI", lat = 6.8, koppen_climate = "Aw")))
  expect_identical(r$name, "ferrallitisol meuble")
  expect_identical(.rp49_deg_of(r, "nitosols"), "simple")
  # on the data: halloysite 50 % or less rules the Sn out; no mineralogy, Ki
  # and Kr; neither, soilKey's nitic_horizon() stands in, flagged (D14)
  d <- .rp49_N(NULL, c(0, 20, 120), c(20, 120, 200), site = list(country = "GP", lat = 16.2, koppen_climate = "Af"), rp = FALSE,
               clay_pct = c(45, 65, 40), sand_pct = c(20, 10, 30), silt_pct = c(35, 25, 30), oc_pct = c(4, 0.9, 0.3),
               halloysite_pct = c(NA, 70, NA), al_ox_pct = c(0.4, 0.3, NA), fe_ox_pct = c(0.8, 0.6, NA),
               pressure_faces = c(NA, "common", NA), clay_films_amount = c(NA, "few", NA), cec_cmol = c(25, 18, 12),
               ph_h2o = c(5.6, 5.8, 6.0), munsell_hue_moist = c("10YR", "5YR", "5YR"), munsell_value_moist = c(3, 4, 5),
               munsell_chroma_moist = c(2, 4, 4), structure_type = c("granular", "angular blocky", "massive"),
               structure_grade = c("moderate", "strong", NA), structure_size = c("fine", "coarse", NA),
               redoximorphic_features_pct = 0)
  expect_identical(.rp49_cl(d)$name, "nitosol")
  expect_false(identical(.rp_identify(e(d, 2, halloysite_pct = 40))$code[2], "Sn"))
  k <- e(d, 2, halloysite_pct = NA, sio2_sulfuric_pct = 25, al2o3_sulfuric_pct = 22, fe2o3_sulfuric_pct = 12)
  expect_identical(.rp_identify(k)$code[2], "Sn")
  fr <- .rp_identify(e(d, 2, halloysite_pct = NA))
  expect_true("nitic_horizon" %in% unlist(fr$stand_ins[2]) || is.na(fr$code[2]))
})

test_that("the organosols: the Aho as a whole, satur\u00e9s and insatur\u00e9s, holorganiques and calcaires (pp. 238-241)", {
  skip_on_cran()
  ex <- .rp49_ex
  e <- .rp49_edit
  N <- .rp49_N
  # the S/CEC of the Aho as a whole: the mean weighted by thickness
  p <- N(c("OF", "Aho", "Aho", "C"), c(0, 5, 15, 45), c(5, 15, 45, 80), oc_pct = c(40, 12, 9, 1),
         bs_pct = c(NA, 70, 90, 95), caco3_pct = c(NA, 0, 0, 0), ph_h2o = c(6.2, 6.4, 6.6, 6.8), clay_pct = c(NA, 30, 32, 30))
  expect_identical(.rp49_cl(p)$name, "organosol satur\u00e9")
  expect_identical(.rp49_cl(e(p, 3, bs_pct = 75))$name, "organosol insatur\u00e9")
  # the S/CEC not measured: undecided, never a silent FALSE
  r <- .rp49_cl(e(ex("organosols_satures"), 2, bs_pct = NA))
  expect_identical(.rp49_deg_of(r, "organosols_satures"), "undecided")
  expect_identical(.rp49_deg_of(r, "organosols_insatures"), "undecided")
  # the insatures are well drained to 50 cm
  expect_identical(.rp49_deg(e(ex("organosols_insatures"), site = list(drainage_class = "poorly drained")),
                             "organosols_insatures")$degree, "fail")
  # an Acaho with OF and OH over 40 cm: the organosol calcaire before the rendosol (p. 241)
  r <- .rp49_cl(ex("organosols_calcaires"))
  expect_identical(.rp49_deg_of(r, "rendosols"), "simple")
  expect_match(r$candidates$precedence[r$candidates$code == "rendosols"], "organosols_calcaires first", fixed = TRUE)
  # a Sal of 10 cm under the Aho: the double organosol insature-alocrisol
  # humique of p. 240 (decision 5 on the review)
  p <- N(c("OF", "Aho", "Sal", "C"), c(0, 5, 30, 40), c(5, 30, 40, 80), oc_pct = c(40, 12, 2.5, 0.5),
         bs_pct = c(NA, 30, 15, 20), ph_h2o = c(4.2, 4.5, 4.6, 4.8), al_kcl_cmol = c(NA, 4, 5, 3), cec_cmol = c(NA, 20, 12, 8),
         clay_pct = c(NA, 18, 18, 12), munsell_hue_moist = "10YR", munsell_value_moist = c(2, 2, 5, 6),
         munsell_chroma_moist = c(1, 2, 5, 4))
  r <- .rp49_cl(p)
  expect_identical(r$name, "organosol insatur\u00e9-alocrisol humique")
  expect_identical(r$levels$code, c("organosols_insatures", "alocrisols_humiques"))
  # no H in an organosol; an OF over R under 10 cm is a lithosol
  expect_identical(.rp49_deg(e(ex("organosols_holorganiques"), 3, designation = "Hm"), "organosols_holorganiques")$degree, "fail")
  expect_identical(.rp49_cl(N(c("OL", "OF", "R"), c(0, 3, 9), c(3, 9, 60), oc_pct = c(45, 40, NA)))$name, "lithosol")
})


# --- the data route, one profile per GER -------------------------------------------------

test_that("profiles with no designations attach through the horizons on the data, one per GER", {
  skip_on_cran()
  d <- function(..., site = list()) .rp49_N(NULL, ..., site = site, rp = FALSE)
  P <- list(
    histosols_fibriques = d(c(0, 90, 120), c(90, 120, 160), site = list(drainage_class = "very poorly drained", elevation_m = 800),
      oc_pct = c(45, 40, NA), loi_pct = c(90, 85, NA), fiber_content_rubbed_pct = c(60, 25, NA),
      rupture_resistance = c(NA, NA, "extremely hard"), redoximorphic_features_pct = 0),
    leptismectisols = d(c(0, 18, 40), c(18, 40, 80), site = list(lat = 48.3),
      clay_pct = c(55, 58, NA), sand_pct = c(10, 8, NA), silt_pct = c(35, 34, NA), oc_pct = c(2.2, 1.0, NA),
      cec_cmol = c(42, 44, NA), ca_cmol = c(38, 40, NA), mg_cmol = c(2.5, 2.5, NA), bs_pct = c(98, 98, NA), ph_h2o = c(7.6, 7.8, NA),
      caco3_pct = c(0, 0, NA), structure_type = c("angular blocky", "prismatic", NA), structure_grade = c("strong", "strong", NA),
      structure_size = c("fine", "coarse", NA), slickensides = c("few", "few", NA), cracks_width_cm = c(2, 3, NA),
      rupture_resistance = c(NA, NA, "extremely hard"), redoximorphic_features_pct = 0),
    lithosols = d(c(0, 6), c(6, 60), oc_pct = c(4, NA), clay_pct = c(25, NA), sand_pct = c(30, NA),
      structure_type = c("granular", NA), structure_grade = c("moderate", NA), rupture_resistance = c(NA, "extremely hard"),
      redoximorphic_features_pct = 0),
    neoluvisols = d(c(0, 20, 45, 100), c(20, 45, 100, 150), site = list(land_use = "deciduous forest"),
      clay_pct = c(15, 14, 22, 20), sand_pct = 10, silt_pct = c(75, 76, 68, 70), oc_pct = c(2, 0.5, 0.3, 0.1),
      ph_h2o = c(5.5, 5.6, 6, 6.5), caco3_pct = 0, munsell_hue_moist = "10YR", munsell_value_moist = c(4, 6, 5, 5),
      munsell_chroma_moist = c(2, 3, 5, 4), structure_type = c("granular", "platy", "angular blocky", "massive"),
      structure_grade = c("moderate", "weak", "strong", NA), clay_films_amount = c(NA, "none", "common", "none"),
      redoximorphic_features_pct = 0),
    nitosols = d(c(0, 20, 120), c(20, 120, 200), site = list(country = "GP", lat = 16.2, koppen_climate = "Af"),
      clay_pct = c(45, 65, 40), sand_pct = c(20, 10, 30), silt_pct = c(35, 25, 30), oc_pct = c(4, 0.9, 0.3),
      halloysite_pct = c(NA, 70, NA), al_ox_pct = c(0.4, 0.3, NA), fe_ox_pct = c(0.8, 0.6, NA),
      pressure_faces = c(NA, "common", NA), clay_films_amount = c(NA, "few", NA), cec_cmol = c(25, 18, 12),
      ph_h2o = c(5.6, 5.8, 6.0), munsell_hue_moist = c("10YR", "5YR", "5YR"), munsell_value_moist = c(3, 4, 5),
      munsell_chroma_moist = c(2, 4, 4), structure_type = c("granular", "angular blocky", "massive"),
      structure_grade = c("moderate", "strong", NA), structure_size = c("fine", "coarse", NA), redoximorphic_features_pct = 0),
    organosols_insatures = d(c(0, 6, 40), c(6, 40, 90), site = list(lat = 48, elevation_m = 1200, drainage_class = "well drained"),
      oc_pct = c(40, 12, 1), fine_organic_matter_pct = c(50, NA, NA), clay_pct = c(NA, 18, 12), sand_pct = c(NA, 40, 50),
      bs_pct = c(NA, 35, 40), ph_h2o = c(4.4, 4.8, 5.0), structure_type = c(NA, "granular", "massive"),
      structure_grade = c(NA, "moderate", NA), munsell_hue_moist = "10YR", munsell_value_moist = c(2, 2, 5),
      munsell_chroma_moist = c(1, 2, 4), redoximorphic_features_pct = 0)
  )
  for (cd in names(P)) {
    r <- .rp49_cl(P[[cd]])
    expect_identical(r$levels$code[1], cd, info = cd)
    expect_true(all(r$trace$horizons$route == "data"), info = cd)
  }
  expect_identical(length(unique(vapply(names(P), function(cd) .rp_rules()$ref_by_code[[cd]]$ger, ""))), 6L)
})


# --- Hugo's decisions on the review of part 6 (2026-11-04) -------------------------------

.rp49_CI <- list(country = "CI", lat = 6.8, koppen_climate = "Aw")

test_that("decisions 1 and 2: the lithosol obstacle at 10 cm or less, one horizon over it (pp. 207, 219, 238)", {
  skip_on_cran()
  N <- .rp49_N
  expect_identical(.rp49_deg(N(c("A", "R"), c(0, 10), c(10, 60)), "lithosols")$degree, "simple")
  expect_identical(.rp49_deg(N(c("A", "R"), c(0, 13), c(13, 60)), "lithosols")$degree, "imperfect")
  expect_identical(.rp49_deg(N(c("A", "R"), c(0, 16), c(16, 60)), "lithosols")$degree, "fail")
  # two horizons over the R at 12 cm: no lithosol, the alocrisol typique imperfect
  p <- N(c("A", "Sal", "R"), c(0, 5, 12), c(5, 12, 60), oc_pct = c(5, 1.2, NA))
  expect_identical(.rp49_deg(p, "lithosols")$degree, "fail")
  r <- .rp49_cl(p)
  expect_identical(r$name, "alocrisol typique")
  expect_identical(r$levels$attachment, "imperfect")
  # the holorganic O over the R count as one horizon ([OF + OH], p. 241)
  expect_identical(.rp49_cl(N(c("OL", "OF", "OH", "R"), c(0, 2, 6, 11), c(2, 6, 11, 60), oc_pct = c(45, 40, 35, NA)))$name,
                   "lithosol")
})

test_that("decision 3: a cuirasse in the tropics is the chapter's only with its horizons under it (pp. 159-161, 219-220)", {
  skip_on_cran()
  N <- .rp49_N
  CI <- .rp49_CI
  for (p in list(N(c("A", "OXm", "R"), c(0, 6, 60), c(6, 60, 100), site = CI), N("OXm", 0, 60, site = CI),
                 N(c("A", "OXc", "C"), c(0, 6, 60), c(6, 60, 100), site = CI))) {
    r <- .rp49_cl(p)
    expect_identical(r$name, "lithosol")
    expect_true(any(r$candidates$group == "ferrallitisols_oxydisols" & r$candidates$degree == "simple"))
  }
  # with an RT, an OX, an F or an Ea and a BT under it: the tropical reference
  expect_identical(.rp49_cl(N(c("A", "OXm", "RT"), c(0, 6, 60), c(6, 60, 150), site = CI))$name,
                   "ferrallitisol p\u00e9troxydique")
  expect_identical(.rp49_cl(N(c("OXm", "Ea", "BTg", "C"), c(0, 30, 60, 120), c(30, 60, 120, 150), clay_pct = 25, sand_pct = 60,
                              site = list(country = "BF", lat = 11.2, koppen_climate = "Aw")))$name,
                   "ferruginosol p\u00e9troxydique")
  # part 5's confirmed cuirasse examples attach as before
  for (cd in c("ferrallitisols_duroxydiques", "oxydisols_petroxydiques", "ferruginosols_petroxydiques",
               "ferrallitisols_petroxydiques")) expect_identical(.rp49_cl(.rp49_ex(cd))$levels$code, cd, info = cd)
})

test_that("decision 4: the index of the lexicon in the middle band and for the derniques (pp. 225, 227, 401)", {
  skip_on_cran()
  L <- function(des, cl) .rp49_N(des, seq(0, by = 20, length.out = length(des)), seq(20, by = 20, length.out = length(des)),
                                 clay_pct = cl, clay_films_amount = ifelse(grepl("BT", des), "many", NA))
  nm <- function(p) .rp49_cl(p)$name
  expect_identical(nm(L(c("Ae", "E", "BT", "C"), c(11, 16, 24, 20))), "luvisol typique")
  expect_identical(nm(L(c("A", "E", "E", "BT", "C"), c(15, 18, 12, 25, 20))), "luvisol typique")
  expect_identical(nm(L(c("A", "E", "Ea", "BTd", "C"), c(20, 25, 15, 40, 30))), "luvisol dernique")
  expect_identical(nm(L(c("A", "E", "Ea", "BTd", "C"), c(6, 5, 5, 12, 10))), "luvisol dernique")
  # under 10 % and over 30 % the bands on the mean of the E stay
  expect_identical(nm(L(c("A", "E", "BT", "C"), c(8, 8, 11, 10))), "n\u00e9oluvisol")
  v <- .rp_with_state(p <- L(c("Ae", "E", "BT", "C"), c(11, 16, 24, 20)), .rp_frame_codes(.rp_identify(p)), .rp_profile(p),
                      luvic_contrast_rp(p, "typique"))
  expect_equal(v$evidence$e_min, 11)
})

test_that("decision 5: a Sal over 10 cm an alocrisol humique, under 10 an organosol insatur\u00e9, 10 the double (p. 241)", {
  skip_on_cran()
  o <- function(sal) .rp49_N(c("OF", "OH", "Aho", "Sal", "C"), c(0, 3, 8, 30, 30 + sal), c(3, 8, 30, 30 + sal, 70),
                             oc_pct = c(40, 35, 12, 2, NA), bs_pct = c(NA, NA, 20, 10, NA))
  r <- .rp49_cl(o(12))
  expect_identical(r$name, "alocrisol humique")
  expect_true("organosols_insatures" %in% r$candidates$code)
  r <- .rp49_cl(o(7))
  expect_identical(r$name, "organosol insatur\u00e9")
  expect_true("alocrisols_humiques" %in% r$candidates$code)
  expect_identical(.rp49_cl(o(10))$name, "organosol insatur\u00e9-alocrisol humique")
})

test_that("decision 6: a BT of 5YR or redder, or micro-polyhedral, recorded, is no luvisol's (p. 230)", {
  skip_on_cran()
  p <- .rp49_ex("luvisols_typiques")
  e <- .rp49_edit
  expect_identical(.rp49_deg(e(p, 3, munsell_hue_moist = "5YR", munsell_value_moist = 4, munsell_chroma_moist = 6),
                             "luvisols_typiques")$degree, "fail")
  expect_identical(.rp49_deg(e(p, 3, munsell_hue_moist = "7.5YR", munsell_value_moist = 5, munsell_chroma_moist = 6),
                             "luvisols_typiques")$degree, "simple")
  expect_identical(.rp49_deg(e(p, 3, structure_type = "angular blocky", structure_grade = "strong", structure_size = "fine"),
                             "luvisols_typiques")$degree, "fail")
  expect_identical(.rp49_deg(e(p, 3, structure_type = "subangular blocky", structure_grade = "moderate",
                               structure_size = "medium"), "luvisols_typiques")$degree, "simple")
})

test_that("decision 7: an h on the E or the BT bars the luvisols (p. 194)", {
  skip_on_cran()
  N <- .rp49_N
  for (des in list(c("A", "Eh", "BT", "C"), c("A", "E", "BTh", "C"))) {
    p <- N(des, c(0, 15, 50, 110), c(15, 50, 110, 150), clay_pct = c(14, 13, 28, 22))
    for (cd in c("neoluvisols", "luvisols_typiques", "luvisols_tronques"))
      expect_identical(.rp49_deg(p, cd)$degree, "fail", info = paste(des[2], cd))
  }
})

test_that("decision 8: under forest with an E of 10 cm or less the tronqu\u00e9 names, the typique a candidate (p. 227)", {
  skip_on_cran()
  r <- .rp49_cl(.rp49_N(c("A", "E", "BT", "C"), c(0, 10, 18, 80), c(10, 18, 80, 120), clay_pct = c(15, 13, 28, 22)))
  expect_identical(r$name, "luvisol tronqu\u00e9")
  expect_identical(.rp49_deg_of(r, "luvisols_typiques"), "simple")
  expect_match(r$candidates$precedence[r$candidates$code == "luvisols_typiques"], "luvisols_tronques first", fixed = TRUE)
})

test_that("decision 9: an S over 10 cm under the O is no organosol; the brunisol, imperfect with no A (p. 239)", {
  skip_on_cran()
  p <- .rp49_N(c("OF", "OH", "S", "C"), c(0, 10, 40, 55), c(10, 40, 55, 90), oc_pct = c(40, 35, 2, NA), bs_pct = c(NA, NA, 40, NA),
               site = list(elevation_m = 1300, drainage_class = "well drained"))
  r <- .rp49_cl(p)
  expect_identical(.rp49_deg(p, "organosols_holorganiques")$degree, "fail")
  expect_identical(r$name, "brunisol dystrique")
  expect_identical(r$levels$attachment, "imperfect")
})

test_that("decision 10: the holorganiques, the r\u00e9gosols over C, the insatur\u00e9s, OHta, an unread notation", {
  skip_on_cran()
  N <- .rp49_N
  # OF and/or OH and/or OHta (p. 239)
  expect_identical(.rp49_cl(N(c("OL", "OHta", "Rca"), c(0, 3, 25), c(3, 25, 60), oc_pct = c(45, 35, NA),
                              site = list(drainage_class = "well drained")))$name, "organosol holorganique")
  expect_identical(.rp49_cl(N(c("OH", "C"), c(0, 25), c(25, 60), oc_pct = c(35, NA),
                              site = list(drainage_class = "well drained", elevation_m = 1300)))$name, "organosol holorganique")
  # an H of 8 cm over a C read as the M: the regosols epihistiques (a later release)
  r <- .rp49_cl(.rp49_H(c("Hm", "C"), c(0, 8), c(8, 100)))
  expect_true("references:regosols" %in% r$provisional_because)
  # the insatures exclude a carbonated Acaho
  p <- N(c("OF", "Acaho", "C"), c(0, 5, 35), c(5, 35, 70), oc_pct = c(40, 12, 1), caco3_pct = c(NA, 10, 30), bs_pct = c(NA, 60, NA),
         site = list(drainage_class = "well drained"))
  expect_identical(.rp49_deg(p, "organosols_insatures")$degree, "fail")
  expect_true("OHta" %in% .RP_KINDS$O)
  # an unread mineral notation puts no histosol or organosol holorganique among the candidates
  r <- .rp49_cl(N(c("ASa", "SSa", "C"), c(0, 20, 60), c(20, 60, 100), bs_pct = 100))
  expect_false(any(r$candidates$group == "histosols"))
  expect_false("organosols_holorganiques" %in% r$candidates$code)
})
