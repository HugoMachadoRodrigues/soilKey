# v0.9.348: the French Referentiel pedologique 2008, part 5 -- the 24
# references of the ferrallitisols et oxydisols (7), ferruginosols (4),
# fersialsols (4), fluviosols (4), grisols (3) and gypsosols (2) (pp.
# 146-200; inst/rules/rp2008/references/*.yaml, R/diagnostics-rp.R,
# R/key-rp2008.R), on the attachment of part 4 (0.9.346). Hugo's decisions
# Q6, Q7, Q8, Q14, Q17, Q18 and default 21 (map 2); the readings of
# readings.yaml confirmed by Hugo on 2026-11-03, with his decisions on the
# review of this part (D3, R1, R2 and the fixes D1, D2, R3, R4).

.rp48_refs <- c("ferrallitisols_meubles", "ferrallitisols_nodulaires", "ferrallitisols_petroxydiques",
                "ferrallitisols_duroxydiques", "oxydisols_meubles", "oxydisols_nodulaires", "oxydisols_petroxydiques",
                "ferruginosols_meubles", "ferruginosols_semiluviques", "ferruginosols_luviques",
                "ferruginosols_petroxydiques", "fersialsols_carbonates", "fersialsols_calciques",
                "fersialsols_insatures", "fersialsols_eluviques", "fluviosols_bruts", "fluviosols_juveniles",
                "fluviosols_typiques", "fluviosols_brunifies", "grisols_eluviques", "grisols_degrades",
                "grisols_hapliques", "gypsosols_hapliques", "gypsosols_petrogypsiques")

# A pedon in the RP notation (in the tropics unless the site says otherwise),
# or on the data (rp = FALSE).
.rp48_N <- function(des, top, bot, ..., site = list(), rp = TRUE) {
  h <- data.frame(top_cm = top, bottom_cm = bot, stringsAsFactors = FALSE)
  if (!is.null(des)) h$designation <- des
  a <- list(...)
  for (k in names(a)) h[[k]] <- rep_len(a[[k]], nrow(h))
  base <- list(id = "t48", country = "CI", lat = 6.8, koppen_climate = "Aw")
  if (rp) base$designation_system <- "rp2008"
  PedonRecord$new(site = utils::modifyList(base, site),
                  horizons = ensure_horizon_schema(data.table::as.data.table(h)))
}
# a pedon with some horizon values (rows) or site values changed
.rp48_edit <- function(p, rows = NULL, ..., site = list()) {
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
.rp48_deg <- function(p, code) {
  rules <- .rp_rules()
  fr <- .rp_frame_codes(.rp_identify(p))
  .rp_with_state(p, fr, .rp_profile(p),
                 .rp_eval_reference(rules$ref_by_code[[code]], p, .tolerance_table(rules$tolerances)))
}
.rp48_cl <- function(p) classify_rp2008(p, on_missing = "silent")
.rp48_ex <- make_rp2008_example
.rp48_deg_of <- function(r, code) r$candidates$degree[r$candidates$code == code]


# --- the catalogue and the rules -----------------------------------------------------

test_that("fifteen GER and 53 references are implemented; the six of part 5 lose their signposts", {
  skip_on_cran()
  rules <- .rp_rules()
  impl <- Filter(function(g) isTRUE(g$implemented), rules$ger)
  # 0.9.349: six more GER after these (test-v09349)
  expect_length(impl, 21L)
  six <- c("ferrallitisols_oxydisols", "ferruginosols", "fersialsols", "fluviosols", "grisols", "gypsosols")
  expect_identical(vapply(impl, `[[`, "", "code")[10:15], six)
  expect_identical(unlist(lapply(impl[10:15], function(g) unlist(g$references))), .rp48_refs)
  expect_length(rules$references, 71L)
  expect_identical(names(rules$ref_by_code)[30:53], .rp48_refs)
  for (g in impl[10:15]) expect_null(g$signpost, info = g$code)
  # the tropical chapters are a first version of the book (p. VI, Q18)
  expect_true(isTRUE(rules$ger_by_code$ferrallitisols_oxydisols$first_draft))
  expect_true(isTRUE(rules$ger_by_code$ferruginosols$first_draft))
  expect_true(isTRUE(rules$ger_by_code$nitosols$first_draft))
  expect_null(rules$ger_by_code$fersialsols$first_draft)
  # the validator passes, the rapid key reaches the grisols only by 7c
  v <- .validate_rules("rp2008")
  expect_identical(c(v$missing, v$unsuffixed, v$mixed, v$malformed, v$codes), character(0))
  ck <- .rp_rapid_key_check()
  expect_identical(ck$entries[ck$ger == "grisols"], "7c")
  expect_identical(ck$ger[!ck$by_the_book], "grisols")
  expect_true(all(ck$implemented[ck$ger %in% six]))
  ord <- .rp_order(rules)
  # the rapid key: fluviosols (5a) before gypsosols (6h) before grisols (7c)
  # before fersialsols (9) before ferrallitisols (10) before brunisols (11a)
  expect_true(all(diff(match(c("fluviosols_bruts", "gypsosols_hapliques", "chernosols_hapliques", "grisols_eluviques",
                               "fersialsols_carbonates", "ferrallitisols_meubles", "ferruginosols_meubles",
                               "brunisols_eutriques"), ord)) > 0))
  # the doubles of these chapters wait for the redoxisols and peyrosols
  later <- Filter(function(d) identical(d$status, "later") && any(grepl("^fluviosols", unlist(d$codes))), rules$doubles)
  expect_length(later, 4L)
})


# --- every reference: a simple fixture, an imperfect one, a failing twin ---------------

test_that("each example attaches simply to its own reference, in the name, and is not provisional", {
  skip_on_cran()
  for (cd in .rp48_refs) {
    r <- .rp48_cl(.rp48_ex(cd))
    expect_identical(r$levels$code, cd, info = cd)
    expect_identical(.rp48_deg_of(r, cd), "simple", info = cd)
    expect_identical(r$provisional_because, character(0), info = cd)
  }
})

test_that("a value inside a tolerance band attaches imperfectly; a failing twin fails", {
  skip_on_cran()
  N <- .rp48_N
  ex <- .rp48_ex
  e <- .rp48_edit
  cl45 <- function(...) N(..., clay_pct = 45, sand_pct = 30)
  cases <- list(
    # an ND at 110 cm: within the tolerance of "no ND within 120" (100)
    ferrallitisols_meubles = list(cl45(c("A", "F", "ND"), c(0, 15, 110), c(15, 110, 200)),
                                  cl45(c("A", "F", "ND"), c(0, 15, 60), c(15, 60, 200))),
    ferrallitisols_nodulaires = list(cl45(c("A", "F", "ND"), c(0, 10, 130), c(10, 130, 200)),
                                     cl45(c("A", "F"), c(0, 10), c(10, 200))),
    ferrallitisols_petroxydiques = list(cl45(c("A", "F", "OXm"), c(0, 15, 130), c(15, 130, 180)),
                                        e(ex("ferrallitisols_petroxydiques"), 3, designation = "OXc")),
    ferrallitisols_duroxydiques = list(cl45(c("A", "F", "OXc"), c(0, 10, 125), c(10, 125, 160)),
                                       e(ex("ferrallitisols_duroxydiques"), 2, designation = "OXm")),
    oxydisols_meubles = list(cl45(c("A", "OX", "ND"), c(0, 10, 110), c(10, 110, 150)),
                             e(ex("oxydisols_meubles"), 2, designation = "F")),
    oxydisols_nodulaires = list(cl45(c("A", "OX", "ND"), c(0, 10, 130), c(10, 130, 150)),
                                e(ex("oxydisols_nodulaires"), 2, designation = "F")),
    oxydisols_petroxydiques = list(cl45(c("A", "OX", "OXm"), c(0, 10, 130), c(10, 130, 160)),
                                   e(ex("oxydisols_petroxydiques"), 3, designation = "F")),
    # the imperfect case of p. 173: no S under the FE
    ferruginosols_meubles = list(e(ex("ferruginosols_meubles"), 3, designation = "C"),
                                 e(ex("ferruginosols_meubles"), 3, designation = "BT")),
    fersialsols_calciques = list(e(ex("fersialsols_calciques"), 2, bs_pct = 77),
                                 e(ex("fersialsols_calciques"), 2, bs_pct = 60)),
    fersialsols_insatures = list(e(ex("fersialsols_insatures"), 2, bs_pct = 83),
                                 e(ex("fersialsols_insatures"), 2, bs_pct = 92)),
    # the imperfect case tronque: an FSt with no E, the index over 1.3 (R1)
    fersialsols_eluviques = list(e(ex("fersialsols_eluviques"), 2, designation = "A"),
                                 e(ex("fersialsols_eluviques"), 3, designation = "FS")),
    fluviosols_juveniles = list(e(ex("fluviosols_juveniles"), 1:2, top_cm = c(0, 33), bottom_cm = c(33, 100)),
                                e(ex("fluviosols_juveniles"), 1:2, top_cm = c(0, 40), bottom_cm = c(40, 100))),
    # the imperfect case leptique: [A + Jp] under 30 cm
    fluviosols_typiques = list(e(ex("fluviosols_typiques"), 1:3, top_cm = c(0, 10, 25), bottom_cm = c(10, 25, 120)),
                               e(ex("fluviosols_typiques"), 2, designation = "S")),
    grisols_eluviques = list(e(ex("grisols_eluviques"), 1:2, bottom_cm = c(33, 40), top_cm = c(0, 33)),
                             e(ex("grisols_eluviques"), 2, designation = "S")),
    # the Aso sombric by the chapter only (4/2: value under 4.5, not under 4)
    grisols_degrades = list(e(ex("grisols_degrades"), 1, munsell_value_moist = 4),
                            e(ex("grisols_degrades"), 3, designation = "BTh")),
    grisols_hapliques = list(e(ex("grisols_hapliques"), 1, bs_pct = 83),
                             e(ex("grisols_hapliques"), 2, clay_pct = 40)),
    gypsosols_hapliques = list(N(c("A", "Ys", "Cy"), c(0, 13, 40), c(13, 40, 90), site = list(country = "TN", lat = 33.9)),
                               N(c("A", "Ys", "Cy"), c(0, 25, 50), c(25, 50, 90), site = list(country = "TN", lat = 33.9))),
    gypsosols_petrogypsiques = list(N(c("A", "Ym", "Ys", "Cy"), c(0, 12, 20, 50), c(12, 20, 50, 90),
                                      site = list(country = "TN", lat = 33.9)),
                                    N(c("A", "Ym", "Ys", "Cy"), c(0, 30, 40, 70), c(30, 40, 70, 90),
                                      site = list(country = "TN", lat = 33.9)))
  )
  for (cd in names(cases)) {
    a <- .rp48_deg(cases[[cd]][[1]], cd)
    b <- .rp48_deg(cases[[cd]][[2]], cd)
    expect_identical(a$degree, "imperfect", info = cd)
    expect_gt(length(a$imperfect_by), 0L)
    expect_identical(b$degree, "fail", info = cd)
    expect_gt(length(b$failed), 0L)
  }
  # the references with no threshold of p. 64 and no imperfect case of their
  # chapter attach simply or not at all: their failing twins
  fails <- list(
    ferruginosols_semiluviques = e(ex("ferruginosols_semiluviques"), 3, designation = "S"),
    ferruginosols_luviques = e(ex("ferruginosols_luviques"), 3, designation = "S"),
    ferruginosols_petroxydiques = e(ex("ferruginosols_petroxydiques"), 3:4, designation = c("C", "C")),
    fersialsols_carbonates = e(ex("fersialsols_carbonates"), 2, caco3_pct = 0, effervescence_class = "none"),
    fluviosols_bruts = e(ex("fluviosols_bruts"), site = list(valley_position = "major bed")),
    fluviosols_brunifies = e(ex("fluviosols_brunifies"), 3:4, top_cm = c(80, 90), bottom_cm = c(90, 160))
  )
  expect_identical(sort(c(names(cases), names(fails))), sort(.rp48_refs))
  for (cd in names(fails)) {
    b <- .rp48_deg(fails[[cd]], cd)
    expect_identical(b$degree, "fail", info = cd)
  }
  # the imperfect cases of the chapters name their qualifier
  expect_identical(.rp48_cl(cases$ferruginosols_meubles[[1]])$name, "ferruginosol meuble sans horizon S")
  expect_identical(.rp48_cl(cases$fluviosols_typiques[[1]])$name, "fluviosol typique leptique")
  # D13: a saturated, non-effervescent FS with Ca/Mg 5 or less is calcique,
  # imperfect, calcimagnesique or magnesique
  r <- .rp48_cl(e(ex("fersialsols_calciques"), 1:2, mg_cmol = c(6, 7)))
  expect_identical(r$name, "fersialsol calcique calcimagn\u00e9sique")
  expect_identical(r$levels$attachment, "imperfect")
  r <- .rp48_cl(e(ex("fersialsols_calciques"), 1:2, mg_cmol = c(15, 16)))
  expect_identical(r$name, "fersialsol calcique magn\u00e9sique")
})


# --- the ferrallitisols and oxydisols ---------------------------------------------------

test_that("the tropical gate: no ferrallitisol, oxydisol or ferruginosol outside the tropics (Q18)", {
  skip_on_cran()
  N <- .rp48_N
  e <- .rp48_edit
  p <- .rp48_ex("ferrallitisols_meubles")
  r <- .rp48_cl(e(p, site = list(country = "FR", lat = 46, koppen_climate = "Cfb")))
  expect_false(r$name == "ferrallitisol meuble")
  expect_identical(.rp48_deg(e(p, site = list(lat = 46, koppen_climate = "Cfb")), "ferrallitisols_meubles")$failed,
                   "tropical_rp")
  expect_identical(.rp48_deg(e(p, site = list(intertropical = FALSE)), "ferrallitisols_meubles")$degree, "fail")
  expect_identical(.rp48_deg(e(.rp48_ex("ferruginosols_luviques"), site = list(lat = 46, koppen_climate = "Cfb")),
                             "ferruginosols_luviques")$degree, "fail")
  # declared in the domain, or a Koppen A, or a latitude between the tropics
  expect_true(tropical_rp(e(p, site = list(lat = 46, koppen_climate = NA, intertropical = TRUE)))$passed)
  expect_true(tropical_rp(e(p, site = list(lat = NA, koppen_climate = "Af")))$passed)
  # neither recorded: the horizons written in the RP notation place the
  # profile in the domain (soilKey's reading); on the data, undecided
  q <- e(p, site = list(lat = NA, koppen_climate = NA))
  expect_true(tropical_rp(q)$passed)
  expect_identical(.rp48_cl(q)$name, "ferrallitisol meuble")
  d <- N(NULL, c(0, 15, 80), c(15, 80, 200), rp = FALSE, site = list(lat = NA, koppen_climate = NA),
         munsell_hue_moist = c("5YR", "2.5YR", "2.5YR"), munsell_value_moist = c(3, 4, 4), munsell_chroma_moist = c(3, 6, 8),
         clay_pct = c(40, 50, 52), sand_pct = c(45, 38, 36), silt_pct = c(15, 12, 12), oc_pct = c(2.2, 0.6, 0.3),
         ph_h2o = c(4.6, 4.8, 5.0), ph_kcl = c(NA, 4.2, 4.5), sio2_sulfuric_pct = 18, al2o3_sulfuric_pct = c(17, 17, 18),
         cec_cmol = c(6, 3.5, 3), bs_pct = c(10, 6, 6), structure_type = c("granular", "subangular blocky", "massive"),
         structure_grade = c("moderate", "weak", NA), clay_films_amount = "none")
  expect_true(is.na(tropical_rp(d)$passed))
  r <- .rp48_cl(d)
  expect_identical(.rp48_deg_of(r, "ferrallitisols_meubles"), "undecided")
  expect_true("references:ferrallitisols_meubles" %in% r$provisional_because)
  # the results of the tropical chapters say the book calls them a first version
  r <- .rp48_cl(p)
  expect_true(any(grepl("first version of the book", r$warnings, fixed = TRUE)))
  r <- .rp48_cl(.rp48_ex("ferruginosols_meubles"))
  expect_true(any(grepl("first version of the book", r$warnings, fixed = TRUE)))
  expect_true(any(grepl("is reported, not tested", r$warnings, fixed = TRUE)))
  expect_false(any(grepl("first version", .rp48_cl(.rp48_ex("fersialsols_insatures"))$warnings, fixed = TRUE)))
})

test_that("the hardness precedence OXm > OXc > ND > meuble (D3)", {
  skip_on_cran()
  N <- function(...) .rp48_N(..., clay_pct = 45, sand_pct = 30)
  nm <- function(p) .rp48_cl(p)$name
  # an OXm and an ND within 120 cm: petroxydique
  r <- .rp48_cl(N(c("A", "ND", "F", "OXm", "RT"), c(0, 10, 40, 90, 130), c(10, 40, 90, 130, 200)))
  expect_identical(r$name, "ferrallitisol p\u00e9troxydique")
  expect_identical(.rp48_deg_of(r, "ferrallitisols_nodulaires"), character(0))
  # an OXc and an ND: duroxydique; an OXm and an OXc: petroxydique
  expect_identical(nm(N(c("A", "ND", "F", "OXc"), c(0, 10, 40, 90), c(10, 40, 90, 150))), "ferrallitisol duroxydique")
  expect_identical(nm(N(c("A", "OXc", "F", "OXm"), c(0, 10, 40, 90), c(10, 40, 90, 150))), "ferrallitisol p\u00e9troxydique")
  # an OXm beyond 120 cm, within its tolerance: the petroxydique imperfect,
  # the simple nodulaire keeps the name (the degree decides)
  r <- .rp48_cl(N(c("A", "ND", "F", "OXm"), c(0, 30, 60, 130), c(30, 60, 130, 180)))
  expect_identical(r$name, "ferrallitisol nodulaire")
  expect_identical(.rp48_deg_of(r, "ferrallitisols_petroxydiques"), "imperfect")
  # the precedence rows of the chapter, with their pages
  rules <- .rp_rules()
  p <- N(c("A", "ND", "F", "OXm"), c(0, 10, 40, 90), c(10, 40, 90, 150))
  fr <- .rp_frame_codes(.rp_identify(p))
  pr <- .rp_with_state(p, fr, .rp_profile(p), .rp_precedence_rows(rules, p, c("ferrallitisols_petroxydiques",
                                                                           "ferrallitisols_nodulaires")))
  expect_identical(pr[[1]]$first, "ferrallitisols_petroxydiques")
  expect_true(pr[[1]]$when)
  expect_match(pr[[1]]$note, "p. 157-161", fixed = TRUE)
})

test_that("F against OX: the thicker loose horizon within 120 cm; neither, a ferrallitisol (D4, D5)", {
  skip_on_cran()
  N <- function(...) .rp48_N(..., clay_pct = 45, sand_pct = 30)
  nm <- function(p) .rp48_cl(p)$name
  expect_identical(nm(N(c("A", "F", "OX"), c(0, 10, 70), c(10, 70, 100))), "ferrallitisol meuble")
  expect_identical(nm(N(c("A", "F", "OX"), c(0, 10, 30), c(10, 30, 150))), "oxydisol meuble")
  # the OX under 120 cm does not count: within 120, F 80 cm, OX 30 cm
  expect_identical(nm(N(c("A", "F", "OX"), c(0, 10, 90), c(10, 90, 300))), "ferrallitisol meuble")
  # equal thicknesses: a ferrallitisol (soilKey's reading)
  expect_identical(nm(N(c("A", "F", "OX"), c(0, 10, 60), c(10, 60, 110))), "ferrallitisol meuble")
  # neither F nor OX: a ferrallitisol (the book's A-OXc-RT, p. 159)
  expect_identical(nm(N(c("A", "OXc", "RT"), c(0, 10, 50), c(10, 50, 150))), "ferrallitisol duroxydique")
  # D5: an OX with an OXc, no OXm or ND: a ferrallitisol duroxydique
  expect_identical(nm(N(c("A", "OX", "OXc"), c(0, 10, 80), c(10, 80, 150))), "ferrallitisol duroxydique")
  # with an ND as well: still a ferrallitisol duroxydique, the hardness
  # precedence OXc before ND (Hugo's decision R2, pp. 159-160)
  expect_identical(nm(N(c("A", "OX", "OXc", "ND"), c(0, 10, 80, 100), c(10, 80, 100, 150))), "ferrallitisol duroxydique")
  # an OXm with the OX thicker: an oxydisol petroxydique
  expect_identical(nm(N(c("A", "OXm", "OX"), c(0, 10, 40), c(10, 40, 150))), "oxydisol p\u00e9troxydique")
  p <- N(c("A", "F", "OX"), c(0, 10, 30), c(10, 30, 150))
  expect_false(loose_rp(p, "F")$passed)
  expect_true(loose_rp(p, "OX")$passed)
  expect_identical(loose_rp(p, "OX")$evidence$OX, 90)
  # the nodular forms count with their code
  expect_true(loose_rp(N(c("A", "Fnod", "OX"), c(0, 10, 90), c(10, 90, 130)), "F")$passed)
})

test_that("the proximal and distal types and the band of 60 to 80 cm (Q17, D2)", {
  skip_on_cran()
  N <- function(...) .rp48_N(..., clay_pct = 45, sand_pct = 30)
  pos <- function(top) {
    r <- .rp48_cl(N(c("A", "F", "ND", "F"), c(0, 10, top, top + 20), c(10, top, top + 20, 200)))
    expect_identical(r$levels$code, "ferrallitisols_nodulaires")
    r$trace$position
  }
  expect_identical(pos(45)$position, "proximal")
  # 0-60 cm, the chapter (p. 45 says 40, Q17)
  expect_identical(pos(55)$position, "proximal")
  # 60 to 80 cm: neither, the reference kept, the depth reported
  p70 <- pos(70)
  expect_identical(p70$position, "neither (60-80 cm)")
  expect_identical(p70$top_cm, 70)
  expect_identical(pos(90)$position, "distal")
  r <- .rp48_cl(N(c("A", "F", "ND", "F"), c(0, 10, 70, 90), c(10, 70, 90, 200)))
  expect_true(any(grepl("the ND starts at 70 cm, neither proximal nor distal", r$warnings, fixed = TRUE)))
  # the examples
  expect_identical(.rp48_cl(.rp48_ex("ferrallitisols_petroxydiques"))$trace$position$position, "distal")
  expect_identical(.rp48_cl(.rp48_ex("oxydisols_petroxydiques"))$trace$position$position, "proximal")
  # no type for the meubles
  expect_null(.rp48_cl(.rp48_ex("ferrallitisols_meubles"))$trace$position)
})


# --- ferruginosols, fersialsols ---------------------------------------------------------

test_that("the ferruginosols: an FE, or an E or BT under the cuirasse or carapace (D10, D11)", {
  skip_on_cran()
  N <- function(...) .rp48_N(..., clay_pct = 25, sand_pct = 60, site = list(country = "BF", lat = 11.2))
  nm <- function(p) .rp48_cl(p)$name
  # an OXc under the A over an E: a ferruginosol petroxydique (D11)
  expect_identical(nm(N(c("A", "OXc", "Ea", "C"), c(0, 10, 50, 90), c(10, 50, 90, 150))), "ferruginosol p\u00e9troxydique")
  # truncated, no A (p. 175)
  expect_identical(nm(N(c("OXm", "Ea", "BTg", "C"), c(0, 30, 60, 120), c(30, 60, 120, 150))), "ferruginosol p\u00e9troxydique")
  # an FE over a cuirasse is a meuble (the type petroxydique, p. 173)
  expect_identical(nm(N(c("A", "FE", "Sg", "OXm"), c(0, 15, 60, 100), c(15, 60, 100, 140))), "ferruginosol meuble")
  # no E or BT under the cuirasse: a ferrallitisol
  expect_identical(nm(N(c("A", "OXm", "RT"), c(0, 10, 60), c(10, 60, 150))), "ferrallitisol p\u00e9troxydique")
  # a ferrallitisol has no FE, E or BT (p. 162)
  r <- .rp48_cl(.rp48_ex("ferruginosols_luviques"))
  expect_false(any(r$candidates$group[r$candidates$degree != "fail"] == "ferrallitisols_oxydisols"))
})

test_that("the fersialsols: the FS, its iron and its carbonates; against the brunisols (p. 182)", {
  skip_on_cran()
  e <- .rp48_edit
  ex <- .rp48_ex
  # an FS forbids the brunisols, the calcosols and calcisols
  r <- .rp48_cl(ex("fersialsols_insatures"))
  expect_identical(.rp48_deg_of(r, "brunisols_dystriques"), character(0))
  # the same profile with an S: a brunisol, no fersialsol
  r <- .rp48_cl(e(ex("fersialsols_insatures"), 2, designation = "S"))
  expect_identical(r$name, "brunisol eutrique")
  expect_false(any(grepl("^fersialsols", r$candidates$code[r$candidates$degree != "fail"])))
  r <- .rp48_cl(ex("fersialsols_carbonates"))
  expect_identical(.rp48_deg_of(r, "calcosols"), character(0))
  # the iron: FFE over total iron 0.20 or less fails (a luvisol, p. 182), on
  # one basis (Fe against Fe2O3)
  p <- e(ex("fersialsols_insatures"), 2, fe_ffe_pct = 0.6, fe_total_pct = 6)
  expect_false(iron_release_rp(p)$passed)
  expect_identical(.rp48_deg(p, "fersialsols_insatures")$degree, "fail")
  expect_true(iron_release_rp(e(p, 2, fe_ffe_pct = 1.2))$passed)
  # the free iron alone is evidence
  expect_true(iron_release_rp(e(ex("fersialsols_insatures"), 2, fe_dcb_pct = 1, fe_total_pct = 6))$passed)
  # carbonated in the fine earth, or only locally
  expect_true(carbonated_rp(ex("fersialsols_carbonates"), "FS")$passed)
  expect_true(carbonated_rp(ex("fersialsols_calciques"), "FS", carbonated = FALSE)$passed)
  q <- e(ex("fersialsols_calciques"), 2, caco3_pct = 3, effervescence_extent = "local")
  expect_true(carbonated_rp(q, "FS", carbonated = FALSE)$passed)
  # an FSt forbids the calciques and insatures; with no E it is an eluvique
  # tronque (index over 1.3) or an insature luvique (R1)
  r <- .rp48_cl(e(ex("fersialsols_insatures"), 1:3, designation = c("A", "FSt", "C"), clay_pct = c(20, 40, 30)))
  expect_identical(r$name, "fersialsol \u00e9luvique tronqu\u00e9")
  r <- .rp48_cl(e(ex("fersialsols_insatures"), 1:3, designation = c("A", "FSt", "C"), clay_pct = c(30, 35, 30)))
  expect_identical(r$name, "fersialsol insatur\u00e9 luvique")
})


# --- fluviosols -------------------------------------------------------------------------

test_that("the fluviosols: the alluvial origin required, the site read when recorded (Q14)", {
  skip_on_cran()
  e <- .rp48_edit
  p <- .rp48_ex("fluviosols_typiques")
  # no site data: the fluviosol in the name, provisional, said so
  q <- e(p, site = list(valley_position = NA, flood_frequency = NA))
  r <- .rp48_cl(q)
  expect_identical(r$name, "fluviosol typique")
  expect_identical(r$levels$attachment, "simple")
  expect_identical(r$provisional_because, "references:fluviosols_typiques")
  expect_true(any(grepl("attaches on its alluvial origin", r$warnings, fixed = TRUE)))
  expect_true(all(c("valley_position", "flood_frequency") %in% r$missing_data))
  expect_true(alluvial_rp(q)$passed)
  expect_true(is.na(alluvial_rp(q, site = "required")$passed))
  # recorded: a terrace or an alluvial fan, or neither flooding nor a water
  # table (both recorded as none), fail; "none observed" is no record (D1)
  for (s in list(list(valley_position = "terrace"), list(valley_position = "alluvial fan"),
                 list(flood_frequency = "none", groundwater_kind = "none")))
    expect_identical(.rp48_deg(e(p, site = s), "fluviosols_typiques")$degree, "fail", info = names(s)[1])
  r <- .rp48_cl(e(p, site = list(flood_frequency = "none", groundwater_kind = "none observed")))
  expect_identical(r$name, "fluviosol typique")
  expect_identical(r$provisional_because, "references:fluviosols_typiques")
  # a water table recorded with no position: provisional
  r <- .rp48_cl(e(p, site = list(valley_position = NA, flood_frequency = NA, groundwater_depth_cm = 90)))
  expect_identical(r$provisional_because, "references:fluviosols_typiques")
  # no origin recorded: no fluviosol (the origin is required)
  q <- e(p, 1:3, layer_origin = NA, site = list(parent_material = "loess"))
  expect_false(alluvial_rp(q)$passed)
  expect_identical(.rp48_deg(q, "fluviosols_typiques")$failed, "alluvial_rp")
  # an alluvial parent material is enough
  r <- .rp48_cl(e(p, 1:3, layer_origin = NA))
  expect_identical(r$name, "fluviosol typique")
  # the bruts: the minor bed when recorded
  expect_identical(.rp48_deg(e(.rp48_ex("fluviosols_bruts"), site = list(valley_position = NA)), "fluviosols_bruts")$degree,
                   "simple")
  expect_identical(.rp48_cl(e(.rp48_ex("fluviosols_bruts"), site = list(valley_position = NA)))$provisional_because,
                   "references:fluviosols_bruts")
})

test_that("the fluviosols: forbidden horizons, the hydromorphy (Q8) and the other references", {
  skip_on_cran()
  e <- .rp48_edit
  ex <- .rp48_ex
  p <- ex("fluviosols_typiques")
  # an E, a G or an H within 50 cm excludes; a G deeper does not
  expect_identical(.rp48_deg(e(p, 2, designation = "E"), "fluviosols_typiques")$degree, "fail")
  expect_identical(.rp48_deg(e(p, 2, designation = "G"), "fluviosols_typiques")$degree, "fail")
  expect_identical(.rp48_deg(e(p, 1, designation = "Hs"), "fluviosols_typiques")$degree, "fail")
  q <- .rp48_N(c("A", "Jp", "M", "G"), c(0, 20, 50, 90), c(20, 50, 90, 130), layer_origin = "fluvic",
               site = list(country = "FR", lat = 47.4, valley_position = "major bed", flood_frequency = "annual"))
  expect_identical(.rp48_cl(q)$name, "fluviosol typique")
  # a g within 50 cm: the fluviosol in the name, provisional by the redoxisols
  # (a later release: Q8, the redoxisol fluvique will take it)
  r <- .rp48_cl(e(p, 2, designation = "Jpg"))
  expect_identical(r$name, "fluviosol typique")
  expect_true("references:reductisols_redoxisols" %in% r$provisional_because)
  # against the colluviosols: colluvium over alluvium, the fluviosol in the
  # name, the colluviosol a candidate (p. 132)
  r <- .rp48_cl(e(ex("fluviosols_brunifies"), 1:2, layer_origin = "colluvic"))
  expect_identical(r$name, "fluviosol brunifi\u00e9")
  co <- r$candidates[r$candidates$code == "colluviosols", ]
  expect_identical(co$degree, "simple")
  expect_match(co$precedence, "fluviosols_brunifies first", fixed = TRUE)
  # against the brunisols and calcosols: the rapid key (5a before 11a and 12)
  r <- .rp48_cl(ex("fluviosols_brunifies"))
  expect_identical(.rp48_deg_of(r, "brunisols_eutriques"), "simple")
  r <- .rp48_cl(e(ex("fluviosols_brunifies"), 1:3, designation = c("Aca", "Sca", "M"), caco3_pct = c(10, 15, 20)))
  expect_identical(r$name, "fluviosol brunifi\u00e9")
  expect_identical(.rp48_deg_of(r, "calcosols"), "simple")
  # in an alluvial setting the bruts, not the regosols (p. 188)
  r <- .rp48_cl(ex("fluviosols_bruts"))
  expect_true("regosols" %in% r$candidates$code)
  expect_match(r$candidates$precedence[r$candidates$code == "regosols"], "fluviosols_bruts first", fixed = TRUE)
})


# --- grisols, gypsosols -----------------------------------------------------------------

test_that("the grisols: the Aso of the chapter, [7c], before the chernosols and phaeosols (Q6, Q17, p. 194)", {
  skip_on_cran()
  e <- .rp48_edit
  ex <- .rp48_ex
  # the phaeosols signpost (an Aso) ranks in 7c before the grisols: the
  # precedence of p. 194 keeps the name firm
  r <- .rp48_cl(ex("grisols_hapliques"))
  expect_true("phaeosols" %in% r$candidates$code)
  expect_identical(r$provisional_because, character(0))
  expect_match(r$candidates$precedence[r$candidates$code == "phaeosols"], "grisols_hapliques first", fixed = TRUE)
  rules <- .rp_rules()
  p <- ex("grisols_eluviques")
  fr <- .rp_frame_codes(.rp_identify(p))
  pr <- .rp_with_state(p, fr, .rp_profile(p), .rp_precedence_rows(rules, p, c("grisols_eluviques", "chernosols_melanoluviques")))
  expect_identical(c(pr[[1]]$first, pr[[1]]$second), c("grisols_eluviques", "chernosols_melanoluviques"))
  expect_match(pr[[1]]$note, "p. 194", fixed = TRUE)
  # a chernosol has an Ach, a grisol an Aso: they never attach together
  r <- .rp48_cl(ex("chernosols_melanoluviques"))
  expect_false(any(grepl("^grisols", r$candidates$code[r$candidates$degree != "fail"])))
  # the Aso by the chapter (D15): 4/3 fails, 4/2 is imperfect; a calcareous
  # or saturated one fails under permanent vegetation
  expect_identical(.rp48_deg(e(p, 1, munsell_value_moist = 4, munsell_chroma_moist = 3), "grisols_eluviques")$degree, "fail")
  expect_identical(.rp48_deg(e(p, 1, munsell_value_moist = 4), "grisols_eluviques")$degree, "imperfect")
  expect_identical(.rp48_deg(e(p, 1, bs_pct = 92), "grisols_eluviques")$degree, "fail")
  # cultivated (D19): an LAh, the complex not tested
  q <- e(p, 1, designation = "LAh", bs_pct = 92)
  expect_identical(.rp48_cl(q)$name, "grisol \u00e9luvique")
  expect_identical(.rp48_cl(e(p, 1, bs_pct = 92, site = list(land_use = "cropland")))$name, "grisol \u00e9luvique")
  # an LEh over the BTh under crops
  expect_identical(.rp48_deg(.rp48_N(c("LEh", "BTh", "BT", "C"), c(0, 25, 70, 110), c(25, 70, 110, 150),
                                     clay_pct = c(18, 32, 30, 24), oc_pct = c(1.5, 0.9, 0.4, 0.2), bs_pct = c(80, 80, 85, 90),
                                     site = list(country = "RO", lat = 46.6, land_use = "cropland")),
                             "grisols_eluviques")$degree, "simple")
  # the index: 1.3 itself hapliques (S19)
  h <- e(ex("grisols_hapliques"), 1:3, clay_pct = c(20, 26, 26))
  expect_identical(.rp48_deg(h, "grisols_hapliques")$degree, "simple")
  expect_identical(.rp48_deg(e(ex("grisols_degrades"), 2:4, clay_pct = c(20, 26, 26)), "grisols_degrades")$degree, "fail")
})

test_that("the gypsosols: a Ys from the surface or under a Ym (D18); a Yp makes none", {
  skip_on_cran()
  N <- function(...) .rp48_N(..., site = list(country = "TN", lat = 33.9, koppen_climate = "BWh"))
  nm <- function(p) .rp48_cl(p)$name
  expect_identical(nm(N(c("Ys", "Cy", "My"), c(0, 30, 80), c(30, 80, 120))), "gypsosol haplique")
  expect_identical(nm(N(c("Ym", "Ys", "Cy"), c(0, 10, 40), c(10, 40, 90))), "gypsosol p\u00e9trogypsique")
  # a Ym 12 cm thick: the Ys under it within the tolerance, still the petrogypsique
  r <- .rp48_cl(N(c("Ym", "Ys", "Cy"), c(0, 12, 40), c(12, 40, 90)))
  expect_identical(r$name, "gypsosol p\u00e9trogypsique")
  # a deep Ys or a Yp makes no gypsosol
  r <- .rp48_cl(N(c("A", "S", "Yp", "C"), c(0, 20, 60, 100), c(20, 60, 100, 140), bs_pct = 90))
  expect_false(any(grepl("^gypsosols", r$candidates$code[r$candidates$degree != "fail"])))
  # the calcium GER forbids the Ys within 10 cm
  r <- .rp48_cl(N(c("Ys", "Sca", "Cca"), c(0, 30, 60), c(30, 60, 100), caco3_pct = c(15, 25, 30)))
  expect_identical(r$name, "gypsosol haplique")
  expect_identical(.rp48_deg_of(r, "calcosols"), character(0))
  expect_true(any(grepl("is reported, not tested", r$warnings, fixed = TRUE)))
})


# --- the data route, one reference per GER ------------------------------------------------

test_that("profiles with no designations attach through the horizons on the data, one per GER", {
  skip_on_cran()
  d <- function(..., site = list()) .rp48_N(NULL, ..., site = site, rp = FALSE)
  P <- list(
    ferrallitisols_meubles = d(c(0, 15, 80), c(15, 80, 200), site = list(country = "GF", lat = 4.9, koppen_climate = "Af"),
      munsell_hue_moist = c("5YR", "2.5YR", "2.5YR"), munsell_value_moist = c(3, 4, 4), munsell_chroma_moist = c(3, 6, 8),
      clay_pct = c(40, 50, 52), sand_pct = c(45, 38, 36), silt_pct = c(15, 12, 12), oc_pct = c(2.2, 0.6, 0.3),
      ph_h2o = c(4.6, 4.8, 5.0), ph_kcl = c(NA, 4.2, 4.5), sio2_sulfuric_pct = 18, al2o3_sulfuric_pct = c(17, 17, 18),
      cec_cmol = c(6, 3.5, 3), bs_pct = c(10, 6, 6), structure_type = c("granular", "subangular blocky", "massive"),
      structure_grade = c("moderate", "weak", NA), clay_films_amount = "none", redoximorphic_features_pct = 0),
    ferruginosols_meubles = d(c(0, 12, 70), c(12, 70, 110), site = list(country = "BF", lat = 11.2, koppen_climate = "Aw"),
      munsell_hue_moist = c("7.5YR", "5YR", "5YR"), munsell_value_moist = c(5, 5, 5), munsell_chroma_moist = c(4, 6, 6),
      clay_pct = c(8, 25, 30), sand_pct = c(70, 60, 55), silt_pct = c(22, 15, 15), oc_pct = c(0.6, 0.2, 0.1),
      ph_h2o = c(6.2, 6.2, 6.0), fe_total_pct = c(NA, 5, NA), cec_cmol = c(3, 4, 4), ca_cmol = c(1.5, 1, 1),
      mg_cmol = c(0.4, 0.4, 0.4), k_cmol = 0.1, na_cmol = 0.05, structure_type = c("massive", "subangular blocky", "massive"),
      structure_grade = c("weak", "weak", NA), clay_films_amount = c(NA, "few", NA), nodules_wt_pct = c(NA, NA, 75),
      ferric_nodules_pct = c(NA, NA, 55), redoximorphic_features_pct = 0),
    fersialsols_eluviques = d(c(0, 10, 25, 70), c(10, 25, 70, 100), site = list(country = "FR", lat = 43.5, koppen_climate = "Csa"),
      munsell_hue_moist = c("5YR", "7.5YR", "2.5YR", NA), munsell_value_moist = c(3, 5, 4, NA), munsell_chroma_moist = c(3, 4, 6, NA),
      clay_pct = c(18, 15, 45, NA), sand_pct = c(42, 43, 25, NA), silt_pct = c(40, 42, 30, NA), oc_pct = c(2.5, 0.6, 0.4, NA),
      ph_h2o = c(6.5, 6.4, 6.8, NA), fe_dcb_pct = c(NA, NA, 4, NA), fe_total_pct = c(NA, NA, 6, NA),
      cec_cmol = c(16, 9, 22, NA), ca_cmol = c(11, 6, 15, NA), mg_cmol = c(1.5, 1, 2, NA), k_cmol = 0.3, na_cmol = 0.1,
      structure_type = c("granular", "subangular blocky", "angular blocky", NA), structure_grade = c("moderate", "weak", "strong", NA),
      clay_films_amount = c(NA, "none", "common", NA), rupture_resistance = c(NA, NA, NA, "extremely hard"),
      redoximorphic_features_pct = 0),
    fluviosols_brunifies = d(c(0, 25, 110), c(25, 110, 150),
      site = list(country = "FR", lat = 47.4, koppen_climate = "Cfb", valley_position = "major bed", flood_frequency = "occasional"),
      munsell_hue_moist = "10YR", munsell_value_moist = c(3, 4, NA), munsell_chroma_moist = c(2, 3, NA),
      clay_pct = c(28, 30, NA), sand_pct = c(25, 22, NA), silt_pct = c(47, 48, NA), oc_pct = c(2.5, 1.0, 0.02),
      ph_h2o = c(7.0, 7.2, NA), bs_pct = c(90, 92, NA), caco3_pct = c(0, 0, NA), coarse_fragments_pct = c(0, 0, 85),
      structure_type = c("granular", "subangular blocky", "single grain"), structure_grade = c("strong", "moderate", NA),
      worm_holes_pct = c(5, 2, 0), roots_abundance = c("many", "common", "none"), layer_origin = "fluvic",
      redoximorphic_features_pct = 0),
    # a dark Eh still rich in carbon stays undecided against an A on the
    # data: the haplique, with no Eh
    grisols_hapliques = d(c(0, 25, 80, 120), c(25, 80, 120, 160),
      site = list(country = "RO", lat = 46.6, koppen_climate = "Dfb", land_use = "deciduous forest"),
      munsell_hue_moist = "10YR", munsell_value_moist = c(3, 3, 4, 5), munsell_chroma_moist = c(2, 3, 4, 4),
      clay_pct = c(22, 27, 28, 22), sand_pct = 20, silt_pct = c(58, 53, 52, 58), oc_pct = c(3, 0.9, 0.4, 0.2),
      ph_h2o = c(6.0, 6.2, 6.4, 6.6), bs_pct = c(70, 80, 85, 90), caco3_pct = 0, cec_cmol = c(25, 24, 22, 18),
      structure_type = c("granular", "angular blocky", "angular blocky", "massive"),
      structure_grade = c("strong", "strong", "moderate", NA), structure_size = c("fine", "fine", "medium", NA),
      worm_holes_pct = c(5, 1, 1, 0), clay_films_amount = c(NA, "many", "common", "none"),
      degradation_form = c(NA, "interdigitations", NA, NA), redoximorphic_features_pct = 0),
    gypsosols_hapliques = d(c(0, 30), c(30, 100), site = list(country = "TN", lat = 33.9, koppen_climate = "BWh"),
      caso4_pct = c(60, 10), caco3_pct = c(8, 12), oc_pct = c(0.4, 0.1), clay_pct = c(10, 15), sand_pct = c(70, 60),
      structure_type = c("massive", "massive"), munsell_hue_moist = "10YR", munsell_value_moist = c(7, 6),
      munsell_chroma_moist = c(2, 3), redoximorphic_features_pct = 0)
  )
  for (cd in names(P)) {
    r <- .rp48_cl(P[[cd]])
    expect_identical(r$levels$code[1], cd, info = cd)
    expect_true(all(r$trace$horizons$route == "data"), info = cd)
  }
  expect_identical(length(unique(vapply(names(P), function(cd) .rp_rules()$ref_by_code[[cd]]$ger, ""))), 6L)
  # the data FS reads its iron on one basis (Q23): free Fe 4 % against Fe2O3 6 %
  fr <- .rp_identify(P$fersialsols_eluviques)
  expect_identical(fr$code[3], "FS")
})


# --- the readings and the fix of the parent material -------------------------------------

test_that("the parent material written last matches a description through a C into an R", {
  skip_on_cran()
  N <- function(des, ...) .rp48_N(des, seq(0, by = 25, length.out = length(des)), seq(25, by = 25, length.out = length(des)),
                                  ..., site = list(country = "FR", lat = 46, koppen_climate = "Cfb"))
  expect_identical(.rp48_cl(N(c("A", "S", "C", "R"), bs_pct = 70, clay_pct = 25))$name, "brunisol eutrique")
  expect_identical(.rp48_cl(N(c("Aca", "Sca", "C", "R"), clay_pct = 25))$name, "calcosol")
  expect_identical(.rp48_cl(N(c("A", "S", "M", "R"), bs_pct = 70, clay_pct = 25))$name, "brunisol eutrique")
  # the C, M and D in any order, the R last; a slot of one code otherwise
  f <- .rp_formula("A-S-(C|M|R)")
  expect_identical(f$formula, "A-S-(C|M)-(C|M)-(R)")
})

test_that("the readings of part 5 are Hugo's decisions; none waits for his word", {
  skip_on_cran()
  rd <- yaml::read_yaml(system.file("rules", "rp2008", "readings.yaml", package = "soilKey"))$readings
  tp <- vapply(rd, `[[`, "", "topic")
  txt <- vapply(rd, `[[`, "", "reading")
  # none waits for Hugo's word (0.9.349 confirmed on 2026-11-04, test-v09349)
  expect_false(any(grepl("to confirm", txt, fixed = TRUE)))
  part5 <- c("The intertropical domain of a profile written in the RP notation",
             "Ferrallitisols and oxydisols: F against OX, the hardness and the types",
             "Ferruginosols: the sequences", "Fersialsols: the A, the FSt, the carbonates and the iron",
             "Fluviosols: the origin, the site and the sequences",
             "Grisols: the Aso, the Eh, the BTh and BThd, the index", "Gypsosols: the crust",
             "The parent material written last")
  expect_true(all(part5 %in% tp))
  expect_true(all(grepl("(Hugo, 2026-11-03).", txt[match(part5, tp)], fixed = TRUE)))
  # the decisions of 2026-11-03 are recorded
  expect_true(all(c("Fersialsols carbonat\u00e9s: the larger part of the FS (D3)",
                    "Fersialsols: an FSt with no E (R1)",
                    "Ferrallitisols: an OX with an ND over an OXc (R2)") %in% tp))
})


# --- the review of part 5 (2026-11-03): Hugo's decisions and the fixes ----------------

test_that("D1: the water table fails only when flooding and groundwater are both recorded as none", {
  skip_on_cran()
  N <- function(site) .rp48_N(c("A", "S", "D"), c(0, 25, 100), c(25, 100, 160), layer_origin = "fluvic", bs_pct = 85,
                               site = utils::modifyList(list(country = "FR", lat = 47.4, koppen_climate = "Cfb"), site))
  # embanked (no flooding recorded) or no water seen in the pit: the
  # fluviosol, provisional (pp. 183, 186, 190)
  for (s in list(list(valley_position = "major bed", flood_frequency = "none"),
                 list(valley_position = "major bed", groundwater_kind = "none observed"),
                 list(valley_position = "major bed", flood_frequency = "none", groundwater_kind = "none observed"))) {
    r <- .rp48_cl(N(s))
    expect_identical(r$name, "fluviosol brunifi\u00e9", info = paste(unlist(s), collapse = " "))
    expect_identical(r$provisional_because, "references:fluviosols_brunifies")
  }
  # both recorded as none: no fluviosol
  r <- .rp48_cl(N(list(valley_position = "major bed", flood_frequency = "none", groundwater_kind = "none")))
  expect_identical(r$name, "brunisol eutrique")
  expect_false(alluvial_rp(N(list(flood_frequency = "none", groundwater_kind = "none")))$passed)
  # a groundwater depth recorded is a water table
  expect_true(alluvial_rp(N(list(valley_position = "major bed", flood_frequency = "none", groundwater_depth_cm = 150)),
                          site = "required")$passed)
})

test_that("the valley position: French words read, an unknown value undecided", {
  skip_on_cran()
  N <- function(vp) .rp48_N(c("A", "S", "D"), c(0, 25, 100), c(25, 100, 160), layer_origin = "fluvic", bs_pct = 85,
                            site = list(country = "FR", lat = 47.4, koppen_climate = "Cfb", valley_position = vp,
                                        flood_frequency = "occasional"))
  expect_true(alluvial_rp(N("lit majeur"), site = "required")$passed)
  expect_true(alluvial_rp(N("floodplain"), site = "required")$passed)
  expect_true(alluvial_rp(N("lit mineur"), bed = "minor", site = "required")$passed)
  expect_false(alluvial_rp(N("lit majeur"), bed = "minor", site = "required")$passed)
  expect_false(alluvial_rp(N("terrasse"), site = "required")$passed)
  expect_false(alluvial_rp(N("c\u00f4ne de d\u00e9jection"), site = "required")$passed)
  expect_true(alluvial_rp(N("marais ass\u00e9ch\u00e9"), site = "required")$passed)
  # not a value soilKey reads: undecided, the fluviosol provisional
  expect_true(is.na(alluvial_rp(N("valley bottom"), site = "required")$passed))
  r <- .rp48_cl(N("valley bottom"))
  expect_identical(r$name, "fluviosol brunifi\u00e9")
  expect_identical(r$provisional_because, "references:fluviosols_brunifies")
  expect_identical(.rp48_cl(N("lit majeur"))$provisional_because, character(0))
})

test_that("D2: a structureless surface layer is no Js; the fluviosols bruts on the data", {
  skip_on_cran()
  d <- function(oc, st, gr = NA) .rp48_N(NULL, c(0, 30), c(30, 100), layer_origin = "fluvic", oc_pct = c(oc, 0.05),
                                clay_pct = c(3, 2), sand_pct = c(85, 60), coarse_fragments_pct = c(10, 80),
                                structure_type = c(st, "single grain"), structure_grade = c(gr, NA), rp = FALSE,
                                site = list(country = "FR", lat = 46, koppen_climate = "Cfb",
                                            valley_position = "minor bed", flood_frequency = "frequent"))
  for (oc in c(0.05, 0.3)) for (st in c("single grain", "massive")) {
    p <- d(oc, st)
    fr <- .rp_identify(p)
    expect_identical(fr$code, c("C", "D"), info = paste(oc, st))
    r <- .rp48_cl(p)
    expect_identical(r$name, "fluviosol brut", info = paste(oc, st))
    expect_identical(r$levels$attachment, "simple")
  }
  # a weak structure is still a Js: the juveniles
  p <- d(0.3, "granular", "weak")
  expect_identical(.rp_identify(p)$code[1], "Js")
  expect_identical(.rp48_cl(p)$name, "fluviosol juv\u00e9nile")
  # no Jp under the raw alluvium (a Jp is under a Js or an A, p. 25): two
  # structureless sandy layers over the gravel, the bruts
  p <- .rp48_N(NULL, c(0, 10, 50), c(10, 50, 100), layer_origin = "fluvic", oc_pct = c(0.3, 0.2, 0.02),
               clay_pct = c(8, 6, NA), sand_pct = c(52, 64, NA), coarse_fragments_pct = c(0, 0, 85),
               structure_type = "single grain", rp = FALSE,
               site = list(country = "FR", lat = 47, koppen_climate = "Cfb", valley_position = "minor bed",
                           flood_frequency = "frequent"))
  expect_identical(.rp_identify(p)$code, c("C", "C", "D"))
  expect_identical(.rp48_cl(p)$name, "fluviosol brut")
  # with no carbon recorded the structureless layer stays undecided between
  # an A and the raw alluvium (C), the bruts undecided, not failed
  q <- .rp48_N(NULL, c(0, 30), c(30, 100), layer_origin = "fluvic", clay_pct = c(3, 2), sand_pct = c(85, 60),
               coarse_fragments_pct = c(10, 80), structure_type = "single grain", rp = FALSE,
               site = list(country = "FR", lat = 46, koppen_climate = "Cfb", valley_position = "minor bed",
                           flood_frequency = "frequent"))
  expect_true("C" %in% .rp_identify(q)$candidates[[1]])
  expect_identical(.rp48_deg_of(.rp48_cl(q), "fluviosols_bruts"), "undecided")
})

test_that("C read as M in the fluviosols typiques: A-Jp-C attaches", {
  skip_on_cran()
  N <- function(des, top, bot) .rp48_N(des, top, bot, layer_origin = "fluvic",
                                       site = list(country = "FR", lat = 47.4, koppen_climate = "Cfb",
                                                   valley_position = "major bed", flood_frequency = "occasional"))
  expect_identical(.rp48_cl(N(c("A", "Jp", "C"), c(0, 20, 60), c(20, 60, 120)))$name, "fluviosol typique")
  expect_identical(.rp48_cl(N(c("A", "Jp", "C", "D"), c(0, 20, 60, 110), c(20, 60, 110, 150)))$name, "fluviosol typique")
  # the C reads as the M would: the same names
  for (tb in list(list(c(0, 10, 25), c(10, 25, 120)), list(c(0, 10, 25), c(10, 25, 60))))
    expect_identical(.rp48_cl(N(c("A", "Jp", "C"), tb[[1]], tb[[2]]))$name,
                     .rp48_cl(N(c("A", "Jp", "M"), tb[[1]], tb[[2]]))$name)
})

test_that("D3: the fersialsols carbonates by the larger part of the FS; else the calciques imperfect", {
  skip_on_cran()
  N <- function(cc, ef) .rp48_N(c("A", "FS", "FS", "R"), c(0, 20, 50, 80), c(20, 50, 80, 100), caco3_pct = c(0, cc, NA),
                                effervescence_class = c("none", ef, NA), bs_pct = c(90, 95, 100, NA),
                                ca_cmol = c(20, 22, 25, NA), mg_cmol = 2,
                                site = list(country = "FR", lat = 43.5, koppen_climate = "Csa"))
  # half the FS carbonated, the deeper half: calcique bathycarbonate (never
  # [non rattache])
  p <- N(c(0, 6), c("none", "strong"))
  r <- .rp48_cl(p)
  expect_identical(r$name, "fersialsol calcique bathycarbonat\u00e9")
  expect_identical(r$levels$attachment, "imperfect")
  expect_false(carbonated_rp(p, "FS")$passed)
  expect_true(carbonated_rp(p, "FS", partial = TRUE, where = "below")$passed)
  # the upper half: recarbonate
  expect_identical(.rp48_cl(N(c(6, 0), c("strong", "none")))$name, "fersialsol calcique recarbonat\u00e9")
  # the larger part: carbonates
  q <- .rp48_N(c("A", "FS", "FS", "R"), c(0, 20, 40, 80), c(20, 40, 80, 100), caco3_pct = c(0, 0, 6, NA),
               effervescence_class = c("none", "none", "strong", NA), bs_pct = c(90, 95, 100, NA),
               ca_cmol = c(20, 22, 25, NA), mg_cmol = 2, site = list(country = "FR", lat = 43.5, koppen_climate = "Csa"))
  expect_true(carbonated_rp(q, "FS")$passed)
  expect_identical(.rp48_cl(q)$name, "fersialsol carbonat\u00e9")
  # every FS carbonated: carbonates, simple
  expect_identical(.rp48_cl(N(c(6, 6), c("strong", "strong")))$name, "fersialsol carbonat\u00e9")
})

test_that("a local effervescence is not the larger part carbonated: the calcique firm", {
  skip_on_cran()
  p <- .rp48_N(c("Aci", "FS", "R"), c(0, 15, 70), c(15, 70, 100), effervescence_class = c("none", "slight", NA),
               effervescence_extent = c(NA, "local", NA), bs_pct = c(92, 95, NA), ca_cmol = c(20, 22, NA),
               mg_cmol = c(2.5, 2.8, NA), site = list(country = "FR", lat = 43.5, koppen_climate = "Csa"))
  expect_false(carbonated_rp(p, "FS")$passed)
  r <- .rp48_cl(p)
  expect_identical(r$name, "fersialsol calcique")
  expect_identical(r$provisional_because, character(0))
  expect_identical(.rp48_deg(p, "fersialsols_carbonates")$degree, "fail")
})

test_that("R1: few clay films make no FSt on the data; an FSt with no E attaches imperfectly", {
  skip_on_cran()
  d <- function(films, mg = 2.5, clay = c(30, 40, NA)) .rp48_N(NULL, c(0, 15, 70), c(15, 70, 100), rp = FALSE,
    site = list(country = "FR", lat = 43.5, koppen_climate = "Csa", parent_material = "limestone"),
    munsell_hue_moist = c("5YR", "2.5YR", "2.5YR"), munsell_value_moist = c(3, 4, 4), munsell_chroma_moist = c(3, 6, 6),
    structure_type = c("granular", "angular blocky", "angular blocky"), structure_grade = c("moderate", "strong", "strong"),
    oc_pct = c(2.5, 0.5, 0.5), clay_pct = clay, sand_pct = 20, silt_pct = c(50, 40, 40), redoximorphic_features_pct = 0,
    caco3_pct = c(0, 0, NA), effervescence_class = c("none", "none", NA), bs_pct = c(92, 95, NA), ca_cmol = c(20, 22, NA),
    mg_cmol = c(mg, mg, NA), ph_h2o = c(7.0, 7.2, NA), cec_cmol = 25, rupture_resistance = c(NA, NA, "extremely hard"),
    coarse_fragments_pct = c(5, 5, 100), clay_films_amount = c(NA, films, NA))
  # few films: an FS, no t; the calcique
  p <- d("few")
  expect_identical(.rp_identify(p)$code[2], "FS")
  expect_false("t" %in% .rp_identify(p)$features[[2]])
  expect_identical(.rp48_cl(p)$name, "fersialsol calcique")
  # common films: an FSt; no E, the index 40/30 over 1.3: eluvique tronque
  p <- d("common")
  expect_true("t" %in% .rp_frame_codes(.rp_identify(p))$features[[2]])
  r <- .rp48_cl(p)
  expect_identical(r$name, "fersialsol \u00e9luvique tronqu\u00e9")
  expect_identical(r$levels$attachment, "imperfect")
  # the index 1.3 or less: the calcique luvique (saturated), even with Ca/Mg
  # under 5, never [non rattache]
  r <- .rp48_cl(d("common", clay = c(36, 40, NA)))
  expect_identical(r$name, "fersialsol calcique luvique")
  r <- .rp48_cl(d("common", mg = 8, clay = c(36, 40, NA)))
  expect_identical(r$name, "fersialsol calcique luvique")
  # the notation: an FSt with no E, unsaturated, a low index: insature luvique
  q <- .rp48_N(c("A", "FSt", "C"), c(0, 15, 80), c(15, 80, 120), clay_pct = c(30, 35, 30), caco3_pct = 0, bs_pct = 60,
               site = list(country = "FR", lat = 43.5, koppen_climate = "Csa"))
  expect_identical(.rp48_cl(q)$name, "fersialsol insatur\u00e9 luvique")
})

test_that("R2: an OXc with an ND and an OX is a ferrallitisol duroxydique (pp. 159-160)", {
  skip_on_cran()
  N <- function(...) .rp48_N(..., clay_pct = 45, sand_pct = 30, site = list(country = "NC", lat = -21.6, koppen_climate = "Aw"))
  r <- .rp48_cl(N(c("A", "OX", "ND", "OXc"), c(0, 10, 40, 90), c(10, 40, 90, 150)))
  expect_identical(r$name, "ferrallitisol duroxydique")
  expect_match(r$candidates$precedence[r$candidates$code == "oxydisols_nodulaires"], "ferrallitisols_duroxydiques first",
               fixed = TRUE)
  # the nodulaires with no OXc still attach as the book says
  expect_identical(.rp48_cl(N(c("A", "OX", "ND"), c(0, 10, 90), c(10, 90, 150)))$name, "oxydisol nodulaire")
  expect_identical(.rp48_cl(N(c("A", "F", "ND", "RT"), c(0, 10, 90, 110), c(10, 90, 110, 150)))$name,
                   "ferrallitisol nodulaire")
  expect_identical(.rp48_cl(N(c("A", "ND", "OX"), c(0, 10, 40), c(10, 40, 150)))$name, "oxydisol nodulaire")
  # an OXm still before the OXc
  expect_identical(.rp48_cl(N(c("A", "OX", "OXc", "OXm"), c(0, 10, 40, 90), c(10, 40, 90, 150)))$name,
                   "oxydisol p\u00e9troxydique")
})

test_that("R3: an Eh undecided against an A keeps its h possible; under an Aso the data read it", {
  skip_on_cran()
  g <- function(dry) .rp48_N(NULL, c(0, 25, 40, 90, 130), c(25, 40, 90, 130, 170), rp = FALSE,
    site = list(country = "RO", lat = 46.6, koppen_climate = "Dfb", land_use = "deciduous forest"),
    munsell_hue_moist = "10YR", munsell_value_moist = c(3, 4, 3, 4, 5), munsell_chroma_moist = c(2, 2, 2, 3, 4),
    munsell_value_dry = dry, clay_pct = c(20, 16, 32, 30, 24), sand_pct = 20, silt_pct = c(60, 64, 48, 50, 56),
    oc_pct = c(3.5, 1.2, 0.9, 0.4, 0.2), ph_h2o = c(6.0, 6.0, 6.2, 6.4, 6.6), bs_pct = c(70, 72, 80, 85, 90),
    caco3_pct = 0, cec_cmol = c(25, 15, 24, 22, 18),
    structure_type = c("granular", "platy", "angular blocky", "angular blocky", "massive"),
    structure_grade = c("strong", "weak", "strong", "moderate", NA), structure_size = c("fine", "fine", "fine", "medium", NA),
    worm_holes_pct = c(5, 1, 1, 1, 0), clay_films_amount = c(NA, "none", "many", "common", "none"),
    redoximorphic_features_pct = 0)
  # the dry colour lighter than above and below: the Eh, the grisol eluvique
  p <- g(c(4, 6, 5, 6, 6))
  fr <- .rp_frame_codes(.rp_identify(p))
  expect_identical(fr$code[2], "E")
  expect_true("h" %in% fr$features[[2]])
  expect_identical(.rp48_cl(p)$name, "grisol \u00e9luvique")
  # the dry colour not lighter: undecided between A and E, the h possible
  q <- g(c(4, 5, 5, 6, 6))
  fr <- .rp_frame_codes(.rp_identify(q))
  expect_true(is.na(fr$code[2]))
  expect_true("h" %in% fr$features_na[[2]])
  expect_true(is.na(.rp_with_state(q, fr, .rp_profile(q), horizon_within_rp(q, "Eh"))$passed))
  r <- .rp48_cl(q)
  expect_identical(.rp48_deg_of(r, "grisols_eluviques"), "undecided")
})

test_that("R4: a written Aso with no colour still reads its recorded S/CEC, pH and carbonates", {
  skip_on_cran()
  N <- function(...) .rp48_N(c("Aso", "Eh", "BTh", "C"), c(0, 25, 40, 100), c(25, 40, 100, 140),
                             oc_pct = c(3.5, 1.2, 0.9, 0.2), clay_pct = c(20, 18, 32, 24), ...,
                             site = list(country = "RO", lat = 46.6, koppen_climate = "Dfb", land_use = "deciduous forest"))
  p <- N(bs_pct = c(90, 90, 90, 95), ph_h2o = 7)
  expect_false(sombric_rp(p)$passed)
  expect_false(any(grepl("^grisols", .rp48_cl(p)$candidates$code[.rp48_cl(p)$candidates$degree != "fail"])))
  # calcareous: no Aso
  expect_false(sombric_rp(N(bs_pct = c(70, 75, 80, 90), ph_h2o = 6, caco3_pct = c(5, 0, 0, 0)))$passed)
  # recorded and in the band: the Aso, the grisol eluvique
  p <- N(bs_pct = c(70, 75, 80, 90), ph_h2o = 6)
  expect_true(sombric_rp(p)$passed)
  expect_identical(.rp48_cl(p)$name, "grisol \u00e9luvique")
  # nothing recorded: the author's reading, as before
  expect_true(sombric_rp(N())$passed)
})

test_that("the grisols: S/CEC under 95 % in the Eh, BTh and BThd, not in a deep non-humic BT", {
  skip_on_cran()
  N <- function(bs) .rp48_N(c("Aso", "Eh", "BTh", "BT", "C"), c(0, 25, 40, 90, 130), c(25, 40, 90, 130, 170),
                            oc_pct = c(3.5, 1.2, 0.9, 0.4, 0.2), clay_pct = c(20, 18, 32, 30, 24), bs_pct = bs, ph_h2o = 6,
                            munsell_hue_moist = "10YR", munsell_value_moist = c(3, 4, 3, 4, 5), munsell_chroma_moist = 2,
                            site = list(country = "RO", lat = 46.6, koppen_climate = "Dfb", land_use = "deciduous forest"))
  r <- .rp48_cl(N(c(70, 75, 85, 99, 100)))
  expect_identical(r$name, "grisol \u00e9luvique")
  expect_identical(r$levels$attachment, "simple")
  # the BTh at 97 or 99 %: within the tolerance of p. 64 (+/- 5), imperfect
  expect_identical(.rp48_deg(N(c(70, 75, 97, 99, 100)), "grisols_eluviques")$degree, "imperfect")
  expect_identical(.rp48_deg(N(c(70, 75, 99, 85, 100)), "grisols_eluviques")$degree, "imperfect")
  # a BT written with no h but starting within 50 cm is read as the BTh
  q <- .rp48_N(c("Aso", "Eh", "BT", "C"), c(0, 25, 40, 130), c(25, 40, 130, 170), oc_pct = c(3.5, 1.2, 0.9, 0.2),
               clay_pct = c(20, 18, 32, 24), bs_pct = c(70, 75, 99, 100), ph_h2o = 6, munsell_hue_moist = "10YR",
               munsell_value_moist = c(3, 4, 3, 5), munsell_chroma_moist = 2,
               site = list(country = "RO", lat = 46.6, koppen_climate = "Dfb", land_use = "deciduous forest"))
  expect_identical(.rp48_deg(q, "grisols_eluviques")$degree, "imperfect")
})

test_that("the parent material written last: an M or a D over a C matches", {
  skip_on_cran()
  s <- function(des, ...) .rp48_N(des, seq(0, by = 25, length.out = length(des)), seq(25, by = 25, length.out = length(des)),
                                  ..., site = list(country = "FR", lat = 46, koppen_climate = "Cfb"))
  expect_identical(.rp48_cl(s(c("A", "S", "M", "C"), bs_pct = 70))$name, "brunisol eutrique")
  expect_identical(.rp48_cl(s(c("A", "S", "C", "M"), bs_pct = 70))$name, "brunisol eutrique")
  expect_identical(.rp48_cl(.rp48_N(c("A", "FE", "S", "M", "C"), c(0, 20, 70, 100, 150), c(20, 70, 100, 150, 200),
                                    site = list(country = "BF", lat = 11.2, koppen_climate = "Aw")))$name,
                   "ferruginosol meuble")
  expect_identical(.rp48_cl(.rp48_N(c("Ys", "M", "C"), c(0, 35, 80), c(35, 80, 120),
                                    site = list(country = "TN", lat = 33.9, koppen_climate = "BWh")))$name,
                   "gypsosol haplique")
  expect_identical(.rp_formula("Aca-(C|M|R|D)")$formula, "ACA-(C|M|D)-(C|M|D)-(C|M|D)-(R)")
})
