# v0.9.351: the French Referentiel pedologique 2008, part 7 -- the 21
# references of the pelosols (3), peyrosols (1), phaeosols (2), planosols
# (3), podzosols (8), rankosols (1) and reductisols et redoxisols (3) (pp.
# 243-290; inst/rules/rp2008/references/*.yaml, R/diagnostics-rp.R,
# R/key-rp2008.R), on the attachment of part 4 (0.9.346) and the references
# of parts 5 and 6; the hydromorphic doubles that waited for the
# reductisols and redoxisols. Hugo's decisions Q5, Q6, Q7, Q8, Q11, Q13,
# Q14, Q17 and the defaults of map 4; the readings of this part confirmed
# with his decisions 1-9 on its review (2026-11-04).

.rp51_refs <- c("pelosols_typiques", "pelosols_brunifies", "pelosols_differencies", "peyrosols",
                "phaeosols_hapliques", "phaeosols_melanoluviques", "planosols_typiques", "planosols_distaux",
                "planosols_structuraux", "podzosols_ocriques", "podzosols_meubles", "podzosols_duriques",
                "podzosols_humiques", "podzosols_humo_duriques", "podzosols_eluviques", "podzosols_placiques",
                "post_podzosols", "rankosols", "reductisols_typiques", "reductisols_stagniques", "redoxisols")
.rp51_pend <- c("Hydromorphy: the 50 cm of functional features (0.9.351)",
                "R\u00e9ductisols typiques and stagniques: the base and the water (0.9.351)",
                "Doubles with the r\u00e9doxisols and r\u00e9ductisols: the pairs the book names (0.9.351)",
                "Brunisol-r\u00e9doxisol: the aerated S (0.9.351)",
                "The doubles of the podzosols: the GER named (0.9.351)",
                "Histosol-r\u00e9ductisol: no double (0.9.351)",
                "Planosols: the abrupt change within 8 cm (0.9.351)",
                "Planosols typiques tronqu\u00e9s: an imperfect attachment (0.9.351)",
                "P\u00e9losols: the pH, the 40 cm and the LE (0.9.351)",
                "Podzosols: the humus and the post-podzosols (0.9.351)",
                "Rankosols: the Sal, the BP and the peyric A (0.9.351)",
                "Ph\u00e6osols: the Sh and BTh on the data and the index (0.9.351)")

# A pedon in the RP notation (France, Koppen Cfb, unless the site says
# otherwise), or on the data (des = NULL, rp = FALSE).
.rp51_N <- function(des, top, bot, ..., site = list(), rp = TRUE) {
  h <- data.frame(top_cm = top, bottom_cm = bot, stringsAsFactors = FALSE)
  if (!is.null(des)) h$designation <- des
  a <- list(...)
  for (k in names(a)) h[[k]] <- rep_len(a[[k]], nrow(h))
  base <- list(id = "t51", country = "FR", lat = 46, koppen_climate = "Cfb")
  if (rp) base$designation_system <- "rp2008"
  PedonRecord$new(site = utils::modifyList(base, site),
                  horizons = ensure_horizon_schema(data.table::as.data.table(h)))
}
.rp51_edit <- function(p, rows = NULL, ..., site = list()) {
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
.rp51_deg <- function(p, code) {
  rules <- .rp_rules()
  fr <- .rp_frame_codes(.rp_identify(p))
  .rp_with_state(p, fr, .rp_profile(p),
                 .rp_eval_reference(rules$ref_by_code[[code]], p, .tolerance_table(rules$tolerances)))
}
.rp51_pred <- function(p, f, ...) {
  fr <- .rp_frame_codes(.rp_identify(p))
  .rp_with_state(p, fr, .rp_profile(p), f(p, ...))
}
.rp51_cl <- function(p) suppressWarnings(classify_rp2008(p, on_missing = "silent"))
.rp51_ex <- make_rp2008_example
.rp51_deg_of <- function(r, code) r$candidates$degree[r$candidates$code == code]


# --- the catalogue and the rules -----------------------------------------------------

test_that("twenty-eight GER and 92 references are implemented; the seven of part 7 lose their signposts", {
  skip_on_cran()
  rules <- .rp_rules()
  impl <- Filter(function(g) isTRUE(g$implemented), rules$ger)
  expect_length(impl, 28L)
  seven <- c("pelosols", "peyrosols", "phaeosols", "planosols", "podzosols", "rankosols", "reductisols_redoxisols")
  expect_identical(vapply(impl, `[[`, "", "code")[22:28], seven)
  expect_identical(unlist(lapply(impl[22:28], function(g) unlist(g$references))), .rp51_refs)
  expect_length(rules$references, 92L)
  expect_identical(names(rules$ref_by_code)[72:92], .rp51_refs)
  for (g in impl[22:28]) expect_null(g$signpost, info = g$code)
  for (r in rules$references[72:92])
    expect_true(all(c("code", "ger", "name", "name_singular", "name_en", "name_en_singular", "page", "tests",
                      "wrb2006") %in% names(r)), info = r$code)
  later <- Filter(function(g) !isTRUE(g$implemented), rules$ger)
  expect_identical(vapply(later, `[[`, "", "code"),
                   c("regosols", "salisols_sodisols", "thalassosols", "thiosols_sulfatosols", "veracrisols", "vertisols"))
  expect_identical(sum(vapply(later, function(g) length(g$references), 0L)), 18L)
  # the phaeosols report their climate, the reductisols the GEPPA classes
  expect_match(rules$ger_by_code$phaeosols$result_note, "D10", fixed = TRUE)
  expect_match(rules$ger_by_code$reductisols_redoxisols$result_note, "Q1", fixed = TRUE)
  v <- .validate_rules("rp2008")
  expect_identical(c(v$missing, v$unsuffixed, v$mixed, v$malformed, v$codes), character(0))
  ck <- .rp_rapid_key_check()
  expect_true(all(ck$by_the_book[ck$ger %in% seven]))
  expect_true(all(ck$implemented[ck$ger %in% seven]))
  ord <- .rp_order(rules)
  # reductisols and planosols (4a) before peyrosols (6a) before pelosols (6e)
  # before rankosols (7a) before phaeosols (7c) before podzosols (8)
  expect_true(all(diff(match(c("redoxisols", "planosols_typiques", "peyrosols", "pelosols_typiques", "rankosols",
                               "phaeosols_hapliques", "podzosols_ocriques"), ord)) > 0))
})

test_that("the later doubles are real now, but those of the GER still missing; the fluviosol pairs stay candidates", {
  skip_on_cran()
  dd <- .rp_rules()$doubles
  st <- vapply(dd, function(d) d$status %||% "", "")
  impl <- c(names(.rp_rules()$ref_by_code))
  # a later pair only with a GER still missing
  for (d in dd[st == "later"]) expect_false(all(unlist(d$codes) %in% impl), info = paste(unlist(d$codes), collapse = "-"))
  expect_identical(lapply(dd[st == "later"], function(d) unlist(d$codes)), list(c("thalassosols_polderises", "redoxisols")))
  # every written pair names implemented references
  for (d in dd[st == ""]) expect_true(all(unlist(d$codes) %in% impl), info = paste(unlist(d$codes), collapse = "-"))
  # the pairs the earlier parts left waiting
  pairs <- vapply(dd[st == ""], function(d) paste(unlist(d$codes), collapse = "-"), "")
  for (p in c("luvisols_typiques-redoxisols", "luvisols_degrades-redoxisols", "luvisols_derniques-redoxisols",
              "peyrosols-alocrisols_typiques", "peyrosols-colluviosols",
              "colluviosols-redoxisols", "brunisols_eutriques-redoxisols", "cryosols_mineraux-redoxisols",
              "cryosols_mineraux-podzosols_meubles", "peyrosols-fluviosols_bruts", "vitrandosols-peyrosols"))
    expect_true(p %in% pairs, info = p)
  expect_identical(vapply(dd[st == "candidate"], function(d) unlist(d$codes)[1L], ""),
                   c("fluviosols_juveniles", "fluviosols_typiques", "fluviosols_brunifies"))
  # no precedence row names a code that is no reference nor GER
  rules <- .rp_rules()
  known <- c(names(rules$ref_by_code), names(rules$ger_by_code), "*",
             unlist(lapply(rules$ger, function(g) if (!isTRUE(g$implemented)) vapply(g$references, `[[`, "", "code"))))
  for (r in rules$precedence) expect_true(all(c(unlist(r$first), unlist(r$second), unlist(r$except)) %in% known))
})

test_that("the readings of part 7 are Hugo's decisions of 2026-11-04; none waits for his word", {
  skip_on_cran()
  rd <- yaml::read_yaml(system.file("rules", "rp2008", "readings.yaml", package = "soilKey"))$readings
  tp <- vapply(rd, `[[`, "", "topic")
  txt <- vapply(rd, `[[`, "", "reading")
  mine <- tp[grepl("(0.9.351)", tp, fixed = TRUE)]
  expect_length(mine, 12L)
  expect_setequal(mine, .rp51_pend)
  dec <- tp[grepl("\\(decision [1-9]\\)$", tp) & seq_along(tp) > max(match(mine, tp))]
  expect_length(dec, 9L)
  expect_true(all(grepl("(Hugo, 2026-11-04).", txt[match(c(mine, dec), tp)], fixed = TRUE)))
  expect_length(tp[grepl("to confirm", txt, fixed = TRUE)], 0L)
  expect_true(all(nzchar(vapply(rd[match(c(mine, dec), tp)], `[[`, "", "page"))))
})


# --- every reference: a simple fixture, an imperfect one, a failing twin ---------------

test_that("each example attaches simply to its own reference, in the name, and is not provisional", {
  skip_on_cran()
  for (cd in .rp51_refs) {
    r <- .rp51_cl(.rp51_ex(cd))
    expect_identical(r$levels$code, cd, info = cd)
    expect_identical(.rp51_deg_of(r, cd), "simple", info = cd)
    expect_identical(r$provisional_because, character(0), info = cd)
  }
  ui <- readLines(system.file("shiny", "classify_app_pro", "R", "utils_ui.R", package = "soilKey"), encoding = "UTF-8")
  for (cd in .rp51_refs) expect_true(any(grepl(sprintf("\"%s\"", cd), ui, fixed = TRUE)), info = cd)
})

test_that("a value inside a tolerance band attaches imperfectly; a failing twin fails", {
  skip_on_cran()
  N <- .rp51_N
  ex <- .rp51_ex
  e <- .rp51_edit
  cases <- list(
    # an Sp of 27 cm: within the 30 +/- 5; of 20 cm, none
    pelosols_typiques = list(e(ex("pelosols_typiques"), 2:3, bottom_cm = c(42, 130), top_cm = c(15, 42)),
                             e(ex("pelosols_typiques"), 2:3, bottom_cm = c(35, 130), top_cm = c(15, 35))),
    # the pH of the Sp 4.9: within the 5.0 +/- 0.2; 4.5 is acid
    pelosols_brunifies = list(e(ex("pelosols_brunifies"), 3, ph_h2o = 4.9), e(ex("pelosols_brunifies"), 3, ph_h2o = 4.5)),
    # [Ae + E] of 33 cm: within the 30 +/- 5; of 40 cm a planosol's
    pelosols_differencies = list(e(ex("pelosols_differencies"), 2:3, bottom_cm = c(33, 85), top_cm = c(8, 33)),
                                 e(ex("pelosols_differencies"), 2:3, bottom_cm = c(40, 85), top_cm = c(8, 40))),
    # peyric horizons of 45 cm from the surface over a C: within the 50 +/- 10
    peyrosols = list(e(ex("peyrosols"), 2:3, bottom_cm = c(45, 120), top_cm = c(30, 45)),
                     e(ex("peyrosols"), 2:3, bottom_cm = c(35, 120), top_cm = c(30, 35))),
    # an Aso of 27 cm: within the 30 +/- 5
    phaeosols_hapliques = list(e(ex("phaeosols_hapliques"), 1:2, bottom_cm = c(27, 85), top_cm = c(0, 27)),
                               e(ex("phaeosols_hapliques"), 1:2, bottom_cm = c(20, 85), top_cm = c(0, 20))),
    # the S/CEC of the Aso 47 %: within the 50 +/- 5; 40 % the dystrique
    phaeosols_melanoluviques = list(e(ex("phaeosols_melanoluviques"), 1, bs_pct = 47),
                                    e(ex("phaeosols_melanoluviques"), 1, clay_pct = 34)),
    # the Eg at 55 cm: within the 50 +/- 10 of the typiques; at 65, none
    planosols_typiques = list(N(c("Ae", "E", "Eg", "Sg"), c(0, 10, 55, 80), c(10, 55, 80, 130), clay_pct = c(10, 9, 10, 40),
                                boundary_distinctness = c("clear", "clear", "abrupt", NA)),
                              N(c("Ae", "E", "Eg", "Sg"), c(0, 10, 75, 90), c(10, 75, 90, 130), clay_pct = c(10, 9, 10, 40),
                                boundary_distinctness = c("clear", "clear", "abrupt", NA))),
    # the floor at 130 cm: within the 120 +/- 20; at 145, none
    planosols_distaux = list(e(ex("planosols_distaux"), 3:5, top_cm = c(65, 130, 160), bottom_cm = c(130, 160, 190)),
                             e(ex("planosols_distaux"), 3:5, top_cm = c(65, 145, 170), bottom_cm = c(145, 170, 200))),
    # the Eg at 55 cm over the grep: within the 50 +/- 10; at 65, none
    planosols_structuraux = list(e(ex("planosols_structuraux"), 1:4, top_cm = c(0, 55, 70, 85), bottom_cm = c(55, 70, 85, 110)),
                                 e(ex("planosols_structuraux"), 1:4, top_cm = c(0, 65, 80, 95), bottom_cm = c(65, 80, 95, 120))),
    # the thickness [O + A] of 33 to 38 cm: within the 35 +/- 5; 45, none
    rankosols = list(e(ex("rankosols"), 1:2, bottom_cm = c(38, 60), top_cm = c(0, 38)),
                     e(ex("rankosols"), 1:2, bottom_cm = c(45, 60), top_cm = c(0, 45))),
    # the G at 55 cm: within the 50 +/- 10
    reductisols_typiques = list(N(c("A", "Gr"), c(0, 55), c(55, 140)), N(c("A", "Gr"), c(0, 70), c(70, 140))),
    reductisols_stagniques = list(N(c("A", "Gr", "C"), c(0, 15, 60), c(15, 60, 100)), N(c("A", "Gr", "C"), c(0, 15, 45), c(15, 45, 100))),
    # redox features over 45 cm: within the 50 +/- 10
    redoxisols = list(N(c("A", "g", "C"), c(0, 20, 65), c(20, 65, 120)), N(c("A", "g", "C"), c(0, 20, 50), c(20, 50, 120)))
  )
  for (cd in names(cases)) {
    a <- .rp51_deg(cases[[cd]][[1]], cd)
    b <- .rp51_deg(cases[[cd]][[2]], cd)
    expect_identical(a$degree, "imperfect", info = cd)
    expect_gt(length(a$imperfect_by), 0L)
    expect_identical(b$degree, "fail", info = cd)
  }
  # the podzosols have no threshold of p. 64 in their tests: failing twins
  fails <- list(
    podzosols_ocriques = e(ex("podzosols_ocriques"), 3, designation = "E"),
    podzosols_meubles = e(ex("podzosols_meubles"), 4, designation = "Eh"),
    podzosols_duriques = e(ex("podzosols_duriques"), 5:6, cementation_class = c("none", "none")),
    podzosols_humiques = e(ex("podzosols_humiques"), 3, designation = "E"),
    podzosols_humo_duriques = e(ex("podzosols_humo_duriques"), 4, cementation_class = "none"),
    podzosols_eluviques = e(ex("podzosols_eluviques"), site = list(lateral_bp_downslope = FALSE)),
    podzosols_placiques = e(ex("podzosols_placiques"), 5, designation = "Fe"),
    post_podzosols = e(ex("post_podzosols"), 1, designation = "A", site = list(land_use = "coniferous forest"))
  )
  expect_identical(sort(c(names(cases), names(fails))), sort(.rp51_refs))
  for (cd in names(fails)) expect_identical(.rp51_deg(fails[[cd]], cd)$degree, "fail", info = cd)
  # the explicit imperfect cases of the chapters name their qualifier
  d <- .rp51_deg(e(ex("phaeosols_hapliques"), 1, bs_pct = 38), "phaeosols_hapliques")
  expect_identical(d$degree, "imperfect")
  expect_match(d$imperfect_by, "dystrique", fixed = TRUE)
  expect_identical(d$alt$qualifier, "dystrique")
  r <- .rp51_cl(N(c("LE", "Sg", "C"), c(0, 28, 100), c(28, 100, 130), clay_pct = c(16, 42, 38),
                  boundary_distinctness = c("abrupt", NA, NA), site = list(land_use = "cropland")))
  expect_identical(r$name, "planosol typique tronqu\u00e9")
  r <- .rp51_deg(.rp51_N(c("OF", "A", "BPh", "C"), c(0, 3, 8, 30), c(3, 8, 30, 80), cementation_class = c(NA, NA, "strongly", NA)),
                 "podzosols_ocriques")
  expect_identical(r$degree, "imperfect")
  expect_match(r$imperfect_by, "D11", fixed = TRUE)
})


# --- the pelosols, peyrosols and phaeosols ----------------------------------------------

test_that("the pelosols: the Sp within 40 cm, never acid; against the vertisols and planosols (pp. 244-247)", {
  skip_on_cran()
  ex <- .rp51_ex
  e <- .rp51_edit
  # an Sp starting at 45 cm: no pelosol (the qualifier of the non-pelosols)
  expect_identical(.rp51_deg(e(ex("pelosols_brunifies"), 2:3, bottom_cm = c(45, 80), top_cm = c(10, 45)),
                             "pelosols_brunifies")$degree, "fail")
  # the pH not recorded: undecided, the name provisional, never a silent FALSE
  r <- .rp51_cl(e(ex("pelosols_typiques"), 1:3, ph_h2o = c(NA, NA, NA)))
  expect_identical(.rp51_deg_of(r, "pelosols_typiques"), "undecided")
  expect_true("references:pelosols_typiques" %in% r$provisional_because)
  # a V: a vertisol of a later release, not a pelosol
  expect_identical(.rp51_deg(e(ex("pelosols_typiques"), 2, designation = "V"), "pelosols_typiques")$degree, "fail")
  # the differencies take the name before a planosol, and include their redox
  # horizons: no double (p. 285)
  p <- e(ex("pelosols_differencies"), 2:3, designation = c("Eg", "Spg"), boundary_distinctness = c("abrupt", NA))
  r <- .rp51_cl(p)
  expect_identical(r$name, "p\u00e9losol diff\u00e9renci\u00e9")
  # a typique with redox features from under 50 cm: the double of p. 285
  p <- e(ex("pelosols_typiques"), 2:3, designation = c("Spg", "Cg"))
  r <- .rp51_cl(p)
  expect_identical(r$name, "p\u00e9losol typique-r\u00e9doxisol")
  expect_identical(r$levels$attachment, c("double", "double"))
  # the Sp on the data: undecided without the structure (map 4, D7)
  d <- .rp51_N(NULL, c(0, 15, 75), c(15, 75, 130), rp = FALSE, clay_pct = c(48, 56, 52), oc_pct = c(3.5, 0.8, 0.3),
               ph_h2o = c(6.6, 7, 7.8), caco3_pct = c(0, 0, 12), munsell_value_moist = c(3, 4, 5), munsell_chroma_moist = 3,
               munsell_hue_moist = "2.5Y")
  expect_true(is.na(.rp51_cl(d)$trace$horizons$code[2]))
  # a coarse angular structure (a prismatic one is read SV on the data, the
  # reading of 0.9.343)
  d2 <- .rp51_edit(d, 1:3, structure_type = c("subangular blocky", "angular blocky", "massive"),
                   structure_grade = c("moderate", "strong", NA), structure_size = c("fine", "coarse", NA))
  r <- .rp51_cl(d2)
  expect_identical(r$trace$horizons$code[2], "Sp")
  expect_identical(r$name, "p\u00e9losol typique")
})

test_that("the peyrosols: peyric horizons from the surface, by weight (Q11), whatever the context (p. 253)", {
  skip_on_cran()
  N <- .rp51_N
  # leptique: 10 to 50 cm of peyric horizons directly on R
  expect_identical(.rp51_cl(N(c("Xp", "R"), c(0, 30), c(30, 60)))$name, "peyrosol")
  expect_identical(.rp51_deg(N(c("Xp", "C", "R"), c(0, 30, 40), c(30, 40, 60)), "peyrosols")$degree, "fail")
  # whatever the context: a peyrosol before a calcosol, a double with the
  # podzosols and with an alocrisol typique (pp. 73, 253)
  r <- .rp51_cl(N(c("OF", "Ae", "E", "BPs", "C"), c(0, 3, 6, 20, 60), c(3, 6, 20, 60, 100),
                  coarse_fragments_wt_pct = c(NA, 70, 75, 80, 60), oc_pct = c(40, 4, 1, 1.5, 0.3)))
  expect_true(all(r$trace$horizons$code[2:4] %in% c("Ae", "E", "BPs")))
  p <- .rp51_N(NULL, c(0, 30, 70), c(30, 70, 120), rp = FALSE, oc_pct = c(3, 1, 0.2), clay_pct = 20, caco3_pct = c(20, 30, 40),
               effervescence_class = "strong", coarse_fragments_wt_pct = c(70, 75, 80), roots_abundance = c("common", "few", NA),
               structure_type = c("granular", "subangular blocky", NA), structure_grade = c("moderate", "weak", NA))
  r <- .rp51_cl(p)
  expect_true(all(r$trace$horizons$code[1:2] %in% c("X", "Xgr", "Xc", "Xp")))
  expect_identical(r$name, "peyrosol")
  # the volume converted, flagged (Q11): 50 % by volume with the bulk density
  pv <- .rp51_edit(p, 1:3, coarse_fragments_wt_pct = c(NA, NA, NA), coarse_fragments_pct = c(50, 50, 50),
                   bulk_density_g_cm3 = c(1.3, 1.4, 1.5))
  expect_true(any(grepl("weight_from_volume", unlist(.rp51_cl(pv)$trace$horizons$fallbacks))))
  expect_identical(.rp51_cl(pv)$name, "peyrosol")
})

test_that("the phaeosols: the Aso of 30 cm, the Sh or BTh, the index; against the grisols and chernosols (pp. 255-258)", {
  skip_on_cran()
  ex <- .rp51_ex
  e <- .rp51_edit
  # an E over 5 cm under the Aso: no phaeosol (a grisol's, a planosol's)
  p <- .rp51_N(c("Aso", "E", "BTh", "C"), c(0, 35, 45, 90), c(35, 45, 90, 130), clay_pct = c(22, 18, 36, 28),
               bs_pct = c(66, 70, 86, 95), oc_pct = c(2.8, 1, 0.9, 0.2))
  expect_identical(.rp51_deg(p, "phaeosols_melanoluviques")$degree, "fail")
  # an E of 3 cm is allowed (planosolisation, p. 256)
  p3 <- e(p, 2:3, top_cm = c(35, 38), bottom_cm = c(38, 90))
  expect_identical(.rp51_deg(p3, "phaeosols_melanoluviques")$degree, "simple")
  # the Sh on the data: carbon over 0.6 % and S/CEC over 80 %; at 70 % no Sh
  h <- e(ex("phaeosols_hapliques"), 2, designation = "S")
  expect_identical(.rp51_deg(h, "phaeosols_hapliques")$degree, "simple")
  expect_identical(.rp51_deg(e(h, 2, bs_pct = 70), "phaeosols_hapliques")$degree, "fail")
  # the index: 2.5 with a gradual transition, melanoluvique; abrupt, none;
  # the boundary not recorded, undecided
  m <- e(ex("phaeosols_melanoluviques"), 2:3, clay_pct = c(55, 40))
  expect_identical(.rp51_deg(e(m, 1, boundary_distinctness = "gradual"), "phaeosols_melanoluviques")$degree, "simple")
  expect_identical(.rp51_deg(e(m, 1, boundary_distinctness = "abrupt"), "phaeosols_melanoluviques")$degree, "fail")
  expect_identical(.rp51_deg(m, "phaeosols_melanoluviques")$degree, "undecided")
  # a deeper non-humic BT does not count in the index of the hapliques
  hb <- .rp51_edit(ex("phaeosols_hapliques"), 3, designation = "BT", clay_pct = 45)
  expect_identical(.rp51_deg(hb, "phaeosols_hapliques")$degree, "simple")
  # the Aso over 80 % under permanent vegetation: undecided against the
  # chernosols (D9); cultivated, not tested
  expect_identical(.rp51_deg(e(ex("phaeosols_hapliques"), 1, bs_pct = 92), "phaeosols_hapliques")$degree, "undecided")
  expect_identical(.rp51_deg(e(ex("phaeosols_hapliques"), 1, bs_pct = 92, site = list(land_use = "cropland")),
                             "phaeosols_hapliques")$degree, "simple")
  # the grisols take an Aso over an Eh and BTh (p. 194)
  g <- .rp51_cl(.rp51_ex("grisols_eluviques"))
  expect_identical(g$levels$code, "grisols_eluviques")
})


# --- the planosols ------------------------------------------------------------------------

test_that("the planosols: the four characters, the 20 % rule, the floor, the Eg (pp. 259-263; D1-D4)", {
  skip_on_cran()
  N <- .rp51_N
  P <- function(ce, cf, bd = "abrupt", eg = 12, ...) N(c("Ae", "Eg", "Sg", "C"), c(0, eg, 40, 100), c(eg, 40, 100, 140),
                                                    clay_pct = c(ce, ce, cf, cf - 2), boundary_distinctness = c("clear", bd, NA, NA), ...)
  pt <- function(p) .rp51_deg(p, "planosols_typiques")$degree
  # E under 20 %: more than the double; 18 to 36 is not more
  expect_identical(pt(P(15, 36)), "simple")
  expect_identical(pt(P(18, 36)), "fail")
  # the clay difference of 20 points or more (D1): 18 to 37 is 19
  expect_identical(pt(P(18, 37)), "fail")
  # E at 20 % and over: 20 points more (D2: 20 in the second rule)
  expect_identical(pt(P(20, 40)), "simple")
  expect_identical(pt(P(25, 44)), "fail")
  # the least clayey E over 30 %: no planosol
  expect_identical(pt(P(32, 55)), "fail")
  # within 8 cm: a clear boundary yes, diffuse no, gradual or not recorded undecided
  expect_identical(pt(P(15, 40, "clear")), "simple")
  expect_identical(pt(P(15, 40, "diffuse")), "fail")
  expect_identical(pt(P(15, 40, "gradual")), "undecided")
  expect_identical(pt(P(15, 40, NA)), "undecided")
  # tongues in the floor: not subhorizontal
  expect_identical(pt(.rp51_edit(P(15, 40), 3, degradation_form = "tongues")), "fail")
  # [Ae + E + Eg] under 15 cm
  expect_identical(pt(N(c("Ae", "Eg", "Sg"), c(0, 5, 12), c(5, 12, 80), clay_pct = c(10, 10, 40),
                        boundary_distinctness = c(NA, "abrupt", NA))), "fail")
  # a transition horizon of 15 to 20 cm: no abrupt change (p. 263)
  expect_identical(pt(N(c("Ae", "Eg", "Sg", "Sg"), c(0, 12, 40, 58), c(12, 40, 58, 120), clay_pct = c(12, 12, 22, 42),
                        boundary_distinctness = c(NA, "abrupt", NA, NA))), "fail")
  # Eg over a BP: a podzosol's
  expect_identical(pt(N(c("Ae", "Eg", "BPs"), c(0, 12, 40), c(12, 40, 100), clay_pct = c(10, 10, 30),
                        boundary_distinctness = c(NA, "abrupt", NA))), "fail")
  # the planosols take the name before the redoxisols, with no double (p. 263)
  r <- .rp51_cl(.rp51_ex("planosols_typiques"))
  expect_identical(.rp51_deg_of(r, "redoxisols"), "simple")
  expect_match(r$candidates$precedence[r$candidates$code == "redoxisols"], "planosols_typiques first", fixed = TRUE)
  # a luvisol degrade with the planosolic characters: the planosol (p. 227)
  ld <- N(c("Ae", "Eg", "BTgd", "C"), c(0, 10, 45, 100), c(10, 45, 100, 140), clay_pct = c(12, 11, 34, 26),
          boundary_distinctness = c(NA, "abrupt", NA, NA), degradation_form = c(NA, NA, "interdigitations", NA))
  r <- .rp51_cl(ld)
  expect_identical(r$name, "planosol typique")
  expect_identical(.rp51_deg_of(r, "luvisols_degrades"), "simple")
  # textural before structural
  r <- .rp51_cl(.rp51_edit(.rp51_ex("planosols_typiques"), 3, pan_type = "fragipan"))
  expect_identical(r$name, "planosol typique")
})


# --- the podzosols and rankosols ----------------------------------------------------------

test_that("the podzosols: the BP kinds, the order of D11, the humus, the eluviques (pp. 265-274; Q14)", {
  skip_on_cran()
  ex <- .rp51_ex
  e <- .rp51_edit
  # only the podzosols have BP; a placic Femp takes the name before the meuble
  pl <- .rp51_N(c("OF", "Ae", "E", "BPh", "Femp", "BPs", "C"), c(0, 3, 6, 25, 35, 36, 70), c(3, 6, 25, 35, 36, 70, 110))
  r <- .rp51_cl(pl)
  expect_identical(r$name, "podzosol placique")
  expect_identical(.rp51_deg_of(r, "podzosols_meubles"), "simple")
  expect_match(r$candidates$precedence[r$candidates$code == "podzosols_meubles"], "podzosols_placiques first", fixed = TRUE)
  # a written BPh with c is an alios
  expect_identical(.rp51_cl(e(ex("podzosols_meubles"), 5, designation = "BPhc"))$name, "podzosol durique")
  # a mull recorded excludes the podzosols, unless cultivated; nothing
  # recorded, no mull
  m <- e(ex("podzosols_meubles"), site = list(humus_form = "mull"))
  expect_identical(.rp51_deg(m, "podzosols_meubles")$degree, "fail")
  nm <- e(ex("podzosols_meubles"), 1:2, designation = c("A", "A"), oc_pct = c(6, 5), ph_h2o = c(NA, NA))
  expect_identical(.rp51_deg(nm, "podzosols_meubles")$degree, "simple")
  expect_identical(.rp51_deg(e(nm, 1, ph_h2o = 6.2), "podzosols_meubles")$degree, "undecided")
  # the eluviques: the BP downslope not recorded, a candidate (Q14)
  r <- .rp51_cl(e(ex("podzosols_eluviques"), site = list(lateral_bp_downslope = NA)))
  expect_identical(.rp51_deg_of(r, "podzosols_eluviques"), "undecided")
  expect_true("references:podzosols_eluviques" %in% r$provisional_because)
  # an E under an OXm is no eluvial podzosol's E (the E under the A or O)
  expect_identical(.rp51_deg(.rp51_N(c("A", "OXm", "Ea", "C"), c(0, 10, 60, 90), c(10, 60, 90, 200),
                                     site = list(lat = 11.2, koppen_climate = "Aw")), "podzosols_eluviques")$degree, "fail")
  # the post-podzosols: a ploughed layer on the BP; LA over E over BP is a
  # cultivated meuble
  expect_identical(.rp51_cl(.rp51_N(c("LA", "E", "BPs", "C"), c(0, 25, 40, 80), c(25, 40, 80, 120),
                                    site = list(land_use = "cropland")))$name, "podzosol meuble")
  expect_identical(.rp51_cl(.rp51_N(c("LE", "E", "BPs", "C"), c(0, 25, 40, 80), c(25, 40, 80, 120),
                                    site = list(land_use = "cropland")))$name, "post-podzosol")
  # the BP on the data: soilKey's spodic() stands in, flagged (D13)
  d <- .rp51_N(NULL, c(0, 5, 10, 35, 50), c(5, 10, 35, 50, 90), rp = FALSE,
               designation = c("Oe", "A", "E", "Bhs", "C"), oc_pct = c(40, 5, 0.5, 3, 0.3), ph_h2o = c(3.8, 4.0, 4.3, 4.5, 4.8),
               al_ox_pct = c(NA, 0.1, 0.05, 0.9, 0.3), fe_ox_pct = c(NA, 0.1, 0.05, 0.8, 0.2),
               munsell_hue_moist = c(NA, "10YR", "10YR", "5YR", "10YR"), munsell_value_moist = c(NA, 3, 6, 3, 5),
               munsell_chroma_moist = c(NA, 1, 1, 4, 4), sand_pct = 85, clay_pct = c(NA, 5, 3, 6, 4))
  r <- .rp51_cl(d)
  expect_true(any(r$trace$horizons$code %in% c("BP", "BPh", "BPs")))
  expect_true(r$levels$code %in% c("podzosols_meubles", "podzosols_humiques"))
})

test_that("the rankosols: 10 to 35 cm on R, coarse 25 %, no S over 5 cm; the doubles with the podzosols and peyrosols", {
  skip_on_cran()
  ex <- .rp51_ex("rankosols")
  e <- .rp51_edit
  N <- .rp51_N
  # 10 cm or less: a lithosol
  expect_identical(.rp51_cl(e(ex, 1:2, bottom_cm = c(8, 60), top_cm = c(0, 8)))$name, "lithosol")
  # the coarse elements not recorded leave the rankosol undecided (Hugo's
  # decision 7); 22 % by volume with no bulk density: undecided by weight
  expect_identical(.rp51_deg(e(ex, 1, coarse_fragments_wt_pct = NA), "rankosols")$degree, "undecided")
  expect_identical(.rp51_deg(e(ex, 1, coarse_fragments_wt_pct = NA, coarse_fragments_pct = 22), "rankosols")$degree, "undecided")
  # an S of 8 cm: a brunisol dystrique lithique's; a Sal of 8 cm is allowed (p. 73)
  base <- function(code, th) N(c("A", code, "R"), c(0, 15, 15 + th), c(15, 15 + th, 60), oc_pct = c(4, 1, NA),
                               coarse_fragments_wt_pct = c(40, 40, NA), ph_h2o = 5)
  expect_identical(.rp51_deg(base("S", 8), "rankosols")$degree, "fail")
  expect_identical(.rp51_deg(base("Sal", 8), "rankosols")$degree, "simple")
  # a calcareous R, or a calcic Aci ("ni calcaires ni calciques", p. 276): no rankosol
  expect_identical(.rp51_deg(e(ex, 2, designation = "Rca"), "rankosols")$degree, "fail")
  expect_identical(.rp51_deg(e(ex, 1, designation = "Aci"), "rankosols")$degree, "fail")
  expect_identical(.rp51_cl(.rp51_ex("rendisols"))$levels$code, "rendisols")
  # a BP of 3 cm ignored; of 7 cm the double rankosol-podzosol ocrique; of 12 a podzosol
  bp <- function(th) N(c("OF", "A", "BPs", "R"), c(0, 3, 15, 15 + th), c(3, 15, 15 + th, 60), oc_pct = c(40, 5, 2, NA),
                       coarse_fragments_wt_pct = c(NA, 40, 40, NA), ph_h2o = 4.5)
  expect_identical(.rp51_cl(bp(3))$name, "rankosol")
  r <- .rp51_cl(bp(7))
  expect_identical(r$name, "rankosol-podzosol ocrique")
  expect_identical(r$levels$attachment, c("double", "double"))
  expect_identical(.rp51_cl(bp(12))$name, "podzosol ocrique")
  # coarse elements over 60 %: the peyrosol, and the double of p. 277
  r <- .rp51_cl(N(c("Xc", "R"), c(0, 25), c(25, 60), oc_pct = c(4, NA), coarse_fragments_wt_pct = c(70, NA)))
  expect_identical(r$name, "rankosol-peyrosol")
  r <- .rp51_cl(N(c("A", "R"), c(0, 25), c(25, 60), oc_pct = c(4, NA), coarse_fragments_wt_pct = c(65, NA)))
  expect_identical(r$name, "rankosol-peyrosol")
})


# --- the reductisols and redoxisols -------------------------------------------------------

test_that("the reductisols and redoxisols: G or g within 50 cm over 50 cm, functional (pp. 279-284; Q13, Q17)", {
  skip_on_cran()
  N <- .rp51_N
  nm <- function(p) .rp51_cl(p)$name
  # a G within 50 cm: a reductisol whatever lies above, a g above it too (D22)
  expect_identical(nm(N(c("A", "g", "Gr"), c(0, 20, 40), c(20, 40, 120))), "r\u00e9ductisol typique")
  # the base: G to the bottom typique, over a C stagnique; a perched water
  # table recorded decides when the base is not seen (D23)
  expect_identical(nm(N(c("H", "Gr"), c(0, 30), c(30, 120))), "r\u00e9ductisol typique")
  expect_identical(nm(N(c("H", "Gr", "C"), c(0, 30, 90), c(30, 90, 120))), "r\u00e9ductisol stagnique")
  expect_identical(nm(N(c("A", "Gr"), c(0, 20), c(20, 120), site = list(groundwater_kind = "perched"))), "r\u00e9ductisol stagnique")
  # the redox features: over 5 % (Q17); 4 % is peu tachete only; relict (Q13)
  d <- function(pct, relict = NA) N(NULL, c(0, 20, 100), c(20, 100, 140), rp = FALSE, designation = c("A", "Bg", "C"),
                                    clay_pct = 35, oc_pct = c(3, 0.6, 0.3), structure_type = c("granular", "subangular blocky", NA),
                                    structure_grade = c("moderate", "moderate", NA), munsell_hue_moist = "10YR",
                                    munsell_value_moist = c(3, 5, 5), munsell_chroma_moist = c(2, 4, 4), bs_pct = c(40, 30, 30),
                                    redoximorphic_features_pct = c(0, pct, 0), redox_relict = c(NA, relict, NA))
  expect_identical(.rp51_cl(d(25))$levels$code, "redoxisols")
  expect_false("redoxisols" %in% .rp51_cl(d(4))$levels$code)
  expect_identical(.rp51_deg(d(4), "redoxisols")$degree, "fail")
  expect_identical(.rp51_deg(d(25, TRUE), "redoxisols")$degree, "fail")
  # a written g recorded relict is not functional either
  w <- N(c("A", "g", "C"), c(0, 20, 95), c(20, 95, 140), redox_relict = c(NA, TRUE, NA))
  expect_identical(.rp51_deg(w, "redoxisols")$degree, "fail")
  # a drained site keeps the attachment, noted (drain\u00e9)
  r <- .rp51_cl(N(c("A", "g", "C"), c(0, 20, 95), c(20, 95, 140), site = list(artificially_drained = TRUE)))
  expect_identical(r$name, "r\u00e9doxisol")
  ev <- r$trace$references[[which(vapply(r$trace$references, `[[`, "", "code") == "redoxisols")]]$evidence
  expect_true(any(grepl("drain", unlist(ev), fixed = TRUE)))
  # the run reaching the bottom of the description short of 50 cm: undecided
  expect_identical(.rp51_deg(N(c("A", "g"), c(0, 20), c(20, 55)), "redoxisols")$degree, "undecided")
  # a run broken by a layer with no redox features: not 50 cm
  expect_identical(.rp51_deg(N(c("A", "g", "S", "g"), c(0, 20, 50, 70), c(20, 50, 70, 130)), "redoxisols")$degree, "fail")
  # an undecided layer (no designation, a mottle colour but no share) leaves
  # it undecided, flagged by the stagnic properties
  u <- N(NULL, c(0, 20, 100), c(20, 100, 140), rp = FALSE, designation = c("A", "B", "C"), clay_pct = 35,
         oc_pct = c(3, 0.6, 0.3), mottle_colors = c(NA, "7.5YR 5/8", NA))
  expect_true(.rp51_deg(u, "redoxisols")$degree %in% c("undecided", "simple", "fail"))
})

test_that("the depth classes of the hydromorphy are in the trace (pp. 282, 284, 289)", {
  skip_on_cran()
  N <- .rp51_N
  cls <- function(p) .rp51_cl(p)$trace$hydromorphy
  h <- cls(N(c("A", "g", "C"), c(0, 15, 90), c(15, 90, 130)))
  expect_match(h$class[h$kind == "g"], "surr\u00e9doxique", fixed = TRUE)
  h <- cls(N(c("A", "S", "Sg", "C"), c(0, 20, 60, 100), c(20, 60, 100, 130)))
  expect_match(h$class[h$kind == "g"], "r\u00e9doxique (50 to 80 cm)", fixed = TRUE)
  h <- cls(N(c("A", "S", "G"), c(0, 20, 90), c(20, 90, 130)))
  expect_match(h$class[h$kind == "G"], "r\u00e9ductique de profondeur", fixed = TRUE)
  h <- cls(N(c("A", "g", "Go"), c(0, 25, 60), c(25, 60, 130)))
  expect_identical(h$kind, c("g", "G"))
  expect_match(h$class[h$kind == "G"], "r\u00e9ductique (50 to 80 cm)", fixed = TRUE)
  expect_null(cls(.rp51_ex("brunisols_eutriques")))
})

test_that("the hydromorphic doubles of the earlier parts are written now (Q8, Q13)", {
  skip_on_cran()
  N <- .rp51_N
  e <- .rp51_edit
  nm <- function(p) .rp51_cl(p)$name
  # the luvisols typiques, degrades and derniques with redox from under 50 cm
  lt <- e(.rp51_ex("luvisols_typiques"), 2:4, designation = c("Eg", "BTg", "Cg"))
  r <- .rp51_cl(lt)
  expect_identical(r$name, "luvisol typique-r\u00e9doxisol")
  expect_identical(r$levels$attachment, c("double", "double"))
  ld <- e(.rp51_ex("luvisols_degrades"), 2:4, designation = c("Eg", "BTgd", "Cg"))
  expect_identical(nm(ld), "luvisol d\u00e9grad\u00e9-r\u00e9doxisol")
  lr <- e(.rp51_ex("luvisols_derniques"), 2:5, designation = c("Eg", "Eag", "BTdg", "Cg"))
  expect_identical(nm(lr), "luvisol dernique-r\u00e9doxisol")
  # the example degrade: its -g from 55 cm, the redoxisol only within
  # tolerance: no double (the double needs the redoxisol strictly)
  r <- .rp51_cl(.rp51_ex("luvisols_degrades"))
  expect_identical(r$levels$code, "luvisols_degrades")
  # a neoluvisol with redox features: no double the book names; the
  # neoluvisol takes the name, the redoxisol a candidate
  r <- .rp51_cl(e(.rp51_ex("neoluvisols"), 2:4, designation = c("Eg", "BTg", "Cg")))
  expect_identical(r$name, "n\u00e9oluvisol")
  expect_identical(.rp51_deg_of(r, "redoxisols"), "simple")
  # H over a G: under 50 cm the reductisol epihistique (pp. 208-209); over
  # 50 cm the histosol, no double (Hugo's decision 2)
  H <- function(th) N(c("Hs", "Gr"), c(0, th), c(th, 140), site = list(drainage_class = "very poorly drained"))
  r <- .rp51_cl(H(40))
  expect_identical(r$name, "r\u00e9ductisol typique")
  # the podzosol meuble-reductisol of p. 285 (A/E/BP/G)
  expect_identical(nm(N(c("OF", "Ae", "E", "BPs", "Go", "Gr"), c(0, 3, 6, 20, 35, 60), c(3, 6, 20, 35, 60, 120))),
                   "podzosol meuble-r\u00e9ductisol typique")
  # the cryosol mineral-redoxisol (p. 142) and -podzosol (p. 141)
  r <- .rp51_cl(N(c("A", "g", "Cg"), c(0, 15, 60), c(15, 60, 120), cryoturbation = c(NA, "present", "present"),
                  site = list(frost_regime = "permafrost", lat = 68, koppen_climate = "ET")))
  expect_identical(r$name, "cryosol min\u00e9ral-r\u00e9doxisol")
  # the brunisol-redoxisol: an aerated S over an Sg (p. 107); A/S/g or A/Sg the redoxisol
  b <- function(des) N(des, c(0, 20, 45, 110), c(20, 45, 110, 140), bs_pct = 70)
  expect_identical(nm(b(c("A", "S", "Sg", "C"))), "brunisol eutrique-r\u00e9doxisol")
  expect_identical(nm(b(c("A", "S", "g", "C"))), "r\u00e9doxisol")
  expect_identical(nm(N(c("A", "Sg", "C"), c(0, 20, 90), c(20, 90, 130), bs_pct = 70)), "r\u00e9doxisol")
  # the colluviosol-redoxisol from 20 to 50 cm; from under 20 the redoxisol colluvial
  co <- function(t) N(c("A", "Cg"), c(0, t), c(t, 150), layer_origin = "colluvic")
  expect_identical(nm(co(30)), "colluviosol-r\u00e9doxisol")
  # with a brunisol attaching too, the brunisol takes the colluvium (p. 133)
  # and its own double of p. 107
  expect_identical(nm(N(c("A", "S", "Sg", "C"), c(0, 15, 30, 120), c(15, 30, 120, 150), layer_origin = "colluvic", bs_pct = 60)),
                   "brunisol eutrique-r\u00e9doxisol")
  expect_identical(nm(N(c("A", "Sg", "C"), c(0, 15, 100), c(15, 100, 150), layer_origin = "colluvic", bs_pct = 60)), "r\u00e9doxisol")
  # the fluviosols: the redoxisol fluvique in the name, the double among the
  # candidates (Q8)
  fl <- list(valley_position = "major bed", flood_frequency = "frequent", groundwater_kind = "observed")
  r <- .rp51_cl(N(c("A", "Sg", "D"), c(0, 25, 100), c(25, 100, 160), layer_origin = "fluvic", bs_pct = 85, site = fl))
  expect_identical(r$name, "r\u00e9doxisol")
  k <- r$candidates$code == "fluviosols_brunifies"
  expect_true(r$candidates$double_possible[k])
  expect_match(r$candidates$precedence[k], "among the candidates", fixed = TRUE)
  # the redoxisol only within tolerance does not take the alluvial solum
  r <- .rp51_cl(N(c("A", "Jpg", "D"), c(0, 20, 60), c(20, 60, 120), layer_origin = "fluvic", site = fl))
  expect_identical(r$name, "fluviosol typique")
})


# --- notation and data routes; missing data ------------------------------------------------

test_that("the references on the data route, and missing data leave the names provisional", {
  skip_on_cran()
  N <- .rp51_N
  # a gley on the data: a bluish matrix, the base reached
  g <- N(NULL, c(0, 20, 50), c(20, 50, 130), rp = FALSE, designation = c("A", "Bg", "Cg"), clay_pct = 40,
         oc_pct = c(4, 0.8, 0.4), munsell_hue_moist = c("10YR", "5GY", "5BG"), munsell_value_moist = c(3, 5, 5),
         munsell_chroma_moist = c(2, 1, 1), redoximorphic_features_pct = c(0, 15, 2),
         redox_segregations_location = c(NA, "pores", NA), structure_type = c("granular", "massive", "massive"))
  r <- .rp51_cl(g)
  expect_true(all(r$trace$horizons$code[2:3] %in% c("Go", "Gr", "G")))
  expect_identical(r$levels$code, "reductisols_typiques")
  # the notation and the data agree on a podzol of the notation
  r <- .rp51_cl(.rp51_ex("podzosols_meubles"))
  expect_true(all(r$trace$horizons$route %in% c("notation", "both")))
  # the phaeosols: the S/CEC of a written Aso not recorded is the author's
  # reading; on the data it is needed
  p <- .rp51_edit(.rp51_ex("phaeosols_hapliques"), 1, bs_pct = NA)
  expect_identical(.rp51_deg(p, "phaeosols_hapliques")$degree, "simple")
})


# --- Hugo's decisions on the review of part 7 (2026-11-04) -------------------------------

test_that("decision 1: a prismatic over-structure alone over an Sp is no SV (pp. 244, 246)", {
  skip_on_cran()
  M <- list(land_use = "deciduous forest", parent_material = "Keuper marls", lat = 48.9)
  d <- function(...) .rp51_N(NULL, c(0, 15, 75), c(15, 75, 130), rp = FALSE, site = M, designation = c("A", "Bw", "C"),
                             clay_pct = c(48, 56, 52), oc_pct = c(3.5, NA, NA), ph_h2o = c(6.6, 7, NA), caco3_pct = c(NA, 0, 12),
                             structure_type = c("subangular blocky", "angular blocky", "massive"),
                             structure_type_secondary = c(NA, "prismatic", NA), ...)
  r <- .rp51_cl(d())
  expect_identical(r$trace$horizons$code[2], "Sp")
  expect_identical(r$name, "p\u00e9losol typique")
  # with slickensides recorded, or wedge-shaped aggregates, or wide cracks: an SV
  expect_identical(.rp51_cl(d(slickensides = c(NA, "few", NA)))$trace$horizons$code[2], "SV")
  expect_identical(.rp51_cl(d(cracks_width_cm = c(NA, 2, NA)))$trace$horizons$code[2], "SV")
  w <- .rp51_edit(d(), 2, structure_type_secondary = "wedge-shaped")
  expect_false(identical(.rp51_cl(w)$trace$horizons$code[2], "Sp"))
})

test_that("decision 2: no double histosol-reductisol; the histosol over 50 cm of H (p. 209)", {
  skip_on_cran()
  N <- .rp51_N
  W <- list(land_use = "meadow")
  for (th in c(55, 60)) {
    r <- .rp51_cl(N(c("Hm", "Gr"), c(0, th), c(th, 130), oc_pct = c(30, 1), site = W))
    expect_identical(r$name, "histosol m\u00e9sique", info = th)
    expect_true("reductisols_typiques" %in% r$candidates$code, info = th)
  }
  expect_false(any(vapply(.rp_rules()$doubles, function(d) any(grepl("^histosols", unlist(d$codes))) &&
                            any(grepl("^reductisols", unlist(d$codes))), NA)))
})

test_that("decision 3: a mor directly on the BP, no A or E, is a podzosol ocrique, imperfect (pp. 267-268)", {
  skip_on_cran()
  r <- .rp51_cl(.rp51_N(c("OF", "OH", "BPs", "C"), c(0, 3, 8, 50), c(3, 8, 50, 90), site = list(land_use = "coniferous forest")))
  expect_identical(r$name, "podzosol ocrique")
  expect_identical(r$levels$attachment, "imperfect")
  expect_match(.rp51_deg(.rp51_N(c("OF", "OH", "BPs", "C"), c(0, 3, 8, 50), c(3, 8, 50, 90)), "podzosols_ocriques")$imperfect_by,
               "decision 3", fixed = TRUE)
})

test_that("decision 4: the phaeosols before the brunisols with an Aso of 30 cm or more (p. 257)", {
  skip_on_cran()
  G <- list(land_use = "natural grassland", country = "UY", lat = -33)
  p <- function(bs) .rp51_N(c("Aso", "Sh", "Kc"), c(0, 40, 85), c(40, 85, 130), clay_pct = c(25, 28, 26),
                            oc_pct = c(2.6, 0.9, 0.3), bs_pct = c(bs, 88, 100), site = G)
  for (b in c(45, 47)) {
    r <- .rp51_cl(p(b))
    expect_match(r$name, "^ph\u00e6osol haplique", info = b)
    expect_identical(r$levels$attachment, "imperfect", info = b)
    expect_match(r$candidates$precedence[r$candidates$code == "brunisols_eutriques"], "phaeosols_hapliques first", fixed = TRUE)
  }
})

test_that("decision 5: reductisols typiques and stagniques by the substrate and the water (p. 282)", {
  skip_on_cran()
  N <- .rp51_N
  W <- list(land_use = "meadow")
  nm <- function(p) .rp51_cl(p)$name
  ph <- c(W, groundwater_kind = "phreatic")
  # an R under the G: stagnique, whatever the water
  expect_identical(nm(N(c("A", "Go", "Gr", "R"), c(0, 20, 50, 100), c(20, 50, 100, 130), site = ph)), "r\u00e9ductisol stagnique")
  # a C under the G with a phreatic water table: typique; with no water recorded, stagnique
  expect_identical(nm(N(c("A", "Go", "Gr", "C"), c(0, 20, 50, 100), c(20, 50, 100, 130), site = ph)), "r\u00e9ductisol typique")
  expect_identical(nm(N(c("A", "Go", "Gr", "C"), c(0, 20, 50, 100), c(20, 50, 100, 130), site = W)), "r\u00e9ductisol stagnique")
  expect_identical(nm(N(c("A", "Go", "Gr", "C"), c(0, 20, 50, 100), c(20, 50, 100, 130),
                        site = c(W, groundwater_kind = "perched"))), "r\u00e9ductisol stagnique")
})

test_that("decision 6: a peyrosol with a G within 50 cm is the double peyrosol-reductisol (p. 253)", {
  skip_on_cran()
  dd <- vapply(.rp_rules()$doubles, function(d) paste(unlist(d$codes), collapse = "-"), "")
  expect_true(all(c("peyrosols-reductisols_typiques", "peyrosols-reductisols_stagniques") %in% dd))
  # on the data: a reduced peyric layer from 20 cm (its G a candidate)
  p <- .rp51_N(NULL, c(0, 20, 60), c(20, 60, 140), rp = FALSE, designation = c("A", "Bg", "Cg"), oc_pct = c(3, 0.6, 0.3),
               roots_abundance = c("common", "few", NA), coarse_fragments_wt_pct = c(70, 75, 80),
               structure_type = c("granular", "subangular blocky", NA), structure_grade = c("moderate", "weak", NA),
               munsell_hue_moist = c("10YR", "5GY", "5GY"), munsell_value_moist = c(3, 5, 5), munsell_chroma_moist = c(2, 1, 1),
               redoximorphic_features_pct = c(0, 2, 0), clay_pct = 20)
  r <- .rp51_cl(p)
  expect_identical(r$name, "peyrosol-r\u00e9ductisol typique")
  expect_identical(r$levels$attachment, c("double", "double"))
  # the precedence: the peyrosol before the reductisols whatever the context
  pr <- .rp_with_state(p, .rp_frame_codes(.rp_identify(p)), .rp_profile(p),
                       .rp_precedence_rows(.rp_rules(), p, c("peyrosols", "reductisols_typiques")))
  expect_true(any(vapply(pr, function(z) identical(c(z$first, z$second), c("peyrosols", "reductisols_typiques")), NA)))
})

test_that("decision 7: the rankosols with the coarse elements not recorded are undecided; under 60 % in the A", {
  skip_on_cran()
  N <- .rp51_N
  V <- list(land_use = "heath", elevation_m = 900, parent_material = "granite")
  r <- .rp51_cl(N(c("A", "R"), c(0, 22), c(22, 60), oc_pct = c(4.5, NA), site = V))
  expect_identical(.rp51_deg_of(r, "rankosols"), "undecided")
  expect_true("references:rankosols" %in% r$provisional_because)
  r <- .rp51_cl(N(c("A", "Sal", "R"), c(0, 15, 18), c(15, 18, 60), oc_pct = c(5, 1.2, NA)))
  expect_identical(.rp51_deg_of(r, "rankosols"), "undecided")
  # A/C/R is no rankosol (the sequence (O)/A/R, p. 276)
  expect_identical(.rp51_deg(N(c("A", "C", "R"), c(0, 15, 25), c(15, 25, 60), oc_pct = c(4.5, 0.5, NA),
                               coarse_fragments_wt_pct = c(40, 50, NA), site = V), "rankosols")$degree, "fail")
  # Q11 still holds for the peyrosols: coarse elements not recorded, none
  expect_identical(.rp51_deg(N(c("A", "C"), c(0, 30), c(30, 100)), "peyrosols")$degree, "fail")
})

test_that("decision 8: E and BP over 10 cm under the Aho make any podzosol, the organosol a candidate (p. 241)", {
  skip_on_cran()
  p <- .rp51_N(c("OF", "Aho", "E", "BPs", "C"), c(0, 5, 25, 30, 36), c(5, 25, 30, 36, 90),
               oc_pct = c(40, 12, 1, 2, 0.3), ph_h2o = c(3.9, 4.1, 4.3, 4.5, 4.8), bs_pct = c(NA, 20, 15, 15, 20),
               site = list(land_use = "coniferous forest", elevation_m = 1200))
  r <- .rp51_cl(p)
  expect_identical(r$name, "podzosol meuble")
  oi <- r$candidates$code == "organosols_insatures"
  if (any(oi)) expect_match(r$candidates$precedence[oi], "podzosols_meubles first", fixed = TRUE)
})

test_that("decision 9: the E over the BP, the peyric podzosols, the alios on the data, the humus, the placic", {
  skip_on_cran()
  N <- .rp51_N
  F <- list(land_use = "coniferous forest")
  nm <- function(p) .rp51_cl(p)$name
  # some E with the BP directly under it: the luvisol typique-podzosol meuble
  # leptique ([A + E + BP] 26 cm) and the micropodzol under 20 cm
  expect_identical(nm(N(c("A", "E", "BPs", "E", "BT", "C"), c(0, 5, 15, 26, 40, 100), c(5, 15, 26, 40, 100, 130),
                        clay_pct = c(12, 10, 12, 12, 28, 25), site = F)), "luvisol typique-podzosol meuble")
  expect_identical(nm(N(c("A", "E", "BPs", "E", "BT", "C"), c(0, 4, 10, 16, 40, 100), c(4, 10, 16, 40, 100, 130),
                        clay_pct = c(12, 10, 12, 12, 28, 25), site = F)), "luvisol typique")
  # peyric podzols: the data (an X that holds an E and a BP) and the notation
  # (coarse elements over 60 % by weight), the peyrosol-podzosol meuble of p. 252
  Fm <- list(land_use = "coniferous forest", elevation_m = 1500, parent_material = "granite moraine")
  y9 <- .rp51_N(NULL, c(0, 4, 20, 45), c(4, 20, 45, 100), rp = FALSE, site = Fm, designation = c("Oe", "E", "Bs", "C"),
                oc_pct = c(35, 0.8, 1.5, NA), coarse_fragments_wt_pct = c(NA, 70, 72, 80), ph_h2o = c(NA, 4.2, 4.6, NA),
                munsell_hue_moist = c(NA, "10YR", "7.5YR", NA), munsell_value_moist = c(NA, 6, 4, NA),
                munsell_chroma_moist = c(NA, 1, 6, NA), al_ox_pct = c(NA, NA, 1.2, 0.3), fe_ox_pct = c(NA, NA, 1.1, 0.2))
  expect_identical(nm(y9), "peyrosol-podzosol meuble")
  expect_identical(nm(N(c("OF", "E", "BPs", "C"), c(0, 4, 20, 45), c(4, 20, 45, 100), coarse_fragments_wt_pct = c(NA, 70, 72, 80),
                        site = Fm)), "peyrosol-podzosol meuble")
  # an alios written Bhsm on the data: a durique
  b <- function(...) .rp51_N(NULL, c(0, 5, 40, 60), c(5, 40, 60, 150), rp = FALSE, site = list(land_use = "coniferous forest"),
                             designation = c("Oa", "E", "Bhsm", "C"), oc_pct = c(30, 0.5, 3, 0.1), ph_h2o = c(NA, 4.3, 4.4, NA),
                             munsell_hue_moist = c(NA, "10YR", "5YR", NA), munsell_value_moist = c(NA, 6, 2, NA),
                             munsell_chroma_moist = c(NA, 1, 2, NA), al_ox_pct = c(NA, NA, 0.8, 0.1), fe_ox_pct = c(NA, NA, 0.2, 0.05), ...)
  expect_identical(nm(b()), "podzosol durique")
  # an A at pH 5.5 with no O or humus form recorded: the humus undecided
  r <- .rp51_cl(N(c("A", "E", "BPh", "BPs", "C"), c(0, 10, 30, 40, 70), c(10, 30, 40, 70, 120), ph_h2o = c(5.5, 5, 4.8, 5, 5), site = F))
  expect_identical(.rp51_deg_of(r, "podzosols_meubles"), "undecided")
  # a ploughed layer over E over BP on the data: post-podzosol undecided
  d <- .rp51_N(NULL, c(0, 28, 40, 70), c(28, 40, 70, 120), rp = FALSE, site = list(land_use = "cropland"),
               designation = c("Ap", "E", "Bhs", "C"), oc_pct = c(2, 0.5, 2.5, 0.2), ph_h2o = c(5, 4.6, 4.7, 5),
               munsell_hue_moist = c("10YR", "10YR", "5YR", "10YR"), munsell_value_moist = c(4, 6, 3, 5),
               munsell_chroma_moist = c(2, 1, 4, 4), al_ox_pct = c(0.1, 0.05, 0.9, 0.2), fe_ox_pct = c(0.1, 0.05, 0.8, 0.2))
  expect_identical(.rp51_deg(d, "post_podzosols")$degree, "undecided")
  # a Femp under the BP alone is no placique's
  expect_identical(.rp51_deg(N(c("OF", "Ae", "E", "BPs", "Femp", "C"), c(0, 3, 6, 20, 40, 41), c(3, 6, 20, 40, 41, 110), site = F),
                             "podzosols_placiques")$degree, "fail")
  # the book's gaps in the trace: Aso/Eg of 8 cm/BThg; an IDT of 2.5 with an abrupt transition
  G <- list(land_use = "natural grassland", country = "UY", lat = -33)
  r <- .rp51_cl(N(c("Aso", "Eg", "BThg", "Kc"), c(0, 30, 38, 80), c(30, 38, 80, 150), clay_pct = c(22, 18, 46, 28),
                  bs_pct = c(66, 60, 86, 100), boundary_distinctness = c("clear", "abrupt", NA, NA), site = G))
  expect_true(any(grepl("places no reference", r$trace$book_gaps, fixed = TRUE)))
  r <- .rp51_cl(N(c("Aso", "BTh", "Kc"), c(0, 35, 80), c(35, 80, 150), clay_pct = c(18, 45, 28), bs_pct = c(66, 86, 100),
                  boundary_distinctness = c("abrupt", NA, NA), site = G))
  expect_true(any(grepl("abrupt Aso/BTh", r$trace$book_gaps, fixed = TRUE)))
})
