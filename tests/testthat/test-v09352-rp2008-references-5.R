# v0.9.352: the French Referentiel pedologique 2008, part 8 -- the 18
# references of the regosols (1), salisols et sodisols (7), thalassosols (3),
# thiosols et sulfatosols (2), veracrisols (1) and vertisols (4) (pp.
# 291-326; inst/rules/rp2008/references/*.yaml, R/diagnostics-rp.R,
# R/key-rp2008.R), on the attachment of part 4 (0.9.346) and the references
# of parts 5 to 7: the 110 references are in, no signpost and no later
# double or precedence is left, and the cross-GER precedence and doubles are
# checked on the full set of examples. Hugo's decisions Q5, Q7, Q8, Q12,
# Q14, Q17, Q20 and the defaults of map 5; the readings of this part wait
# for his word (readings.yaml, 2026-11-05).

.rp52_refs <- c("regosols", "salisols_chloruro_sulfates", "salisols_carbonates", "sodisols_indifferencies",
                "sodisols_solonetziques", "sodisols_solodises", "sodisalisols", "salisodisols",
                "thalassosols_bruts", "thalassosols_juveniles", "thalassosols_polderises", "thiosols",
                "sulfatosols", "veracrisols", "lithovertisols", "topovertisols", "paravertisols_hapliques",
                "paravertisols_planosoliques")
.rp52_pend <- c("R\u00e9gosols: the obstacle and the young deposits (0.9.352)",
                "Salisols: the series without the pH of the paste (0.9.352)",
                "Sodisols: the three references and the superposed ones (0.9.352)",
                "Salsodic doubles with the r\u00e9ductisols and r\u00e9doxisols (0.9.352)",
                "Thalassosols: the marine origin, the closed list and the site (0.9.352)",
                "Thalassosols and the hydromorphy: the doubles and their order (0.9.352)",
                "Thiosols and sulfatosols: the solum characters and the TH under the U (0.9.352)",
                "Veracrisols: the Ah on the data, heath and culture (0.9.352)",
                "Vertisols: litho against topo (0.9.352)",
                "Vertisols: the paravertisols and the other GER (0.9.352)",
                "The rows of part 7 with the new GER (0.9.352)")

# A pedon in the RP notation (France, Koppen Cfb, unless the site says
# otherwise), or on the data (des = NULL, rp = FALSE).
.rp52_N <- function(des, top, bot, ..., site = list(), rp = TRUE) {
  h <- data.frame(top_cm = top, bottom_cm = bot, stringsAsFactors = FALSE)
  if (!is.null(des)) h$designation <- des
  a <- list(...)
  for (k in names(a)) h[[k]] <- rep_len(a[[k]], nrow(h))
  base <- list(id = "t52", country = "FR", lat = 46, koppen_climate = "Cfb")
  if (rp) base$designation_system <- "rp2008"
  PedonRecord$new(site = utils::modifyList(base, site),
                  horizons = ensure_horizon_schema(data.table::as.data.table(h)))
}
.rp52_edit <- function(p, rows = NULL, ..., site = list()) {
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
# the same profile with its designations removed: the data route
.rp52_data <- function(p) {
  h <- as.data.frame(p$horizons)
  h$designation <- NA_character_
  s <- p$site
  s$designation_system <- NULL
  PedonRecord$new(site = s, horizons = ensure_horizon_schema(data.table::as.data.table(h)))
}
.rp52_deg <- function(p, code) {
  rules <- .rp_rules()
  fr <- .rp_frame_codes(.rp_identify(p))
  .rp_with_state(p, fr, .rp_profile(p),
                 .rp_eval_reference(rules$ref_by_code[[code]], p, .tolerance_table(rules$tolerances)))
}
.rp52_pred <- function(p, f, ...) {
  fr <- .rp_frame_codes(.rp_identify(p))
  .rp_with_state(p, fr, .rp_profile(p), f(p, ...))
}
.rp52_cl <- function(p) suppressWarnings(classify_rp2008(p, on_missing = "silent"))
.rp52_ex <- make_rp2008_example
.rp52_deg_of <- function(r, code) r$candidates$degree[r$candidates$code == code]


# --- the catalogue and the rules -----------------------------------------------------

test_that("the 34 GER and the 110 references are implemented; no signpost is left", {
  skip_on_cran()
  rules <- .rp_rules()
  impl <- Filter(function(g) isTRUE(g$implemented), rules$ger)
  expect_length(impl, 34L)
  expect_length(rules$ger, 34L)
  six <- c("regosols", "salisols_sodisols", "thalassosols", "thiosols_sulfatosols", "veracrisols", "vertisols")
  expect_identical(vapply(impl, `[[`, "", "code")[29:34], six)
  expect_identical(unlist(lapply(impl[29:34], function(g) unlist(g$references))), .rp52_refs)
  expect_length(rules$references, 110L)
  expect_identical(names(rules$ref_by_code)[93:110], .rp52_refs)
  expect_identical(sort(unlist(lapply(rules$ger, function(g) unlist(g$references)))), sort(names(rules$ref_by_code)))
  for (g in rules$ger) expect_null(g$signpost, info = g$code)
  for (r in rules$references[93:110])
    expect_true(all(c("code", "ger", "name", "name_singular", "name_en", "name_en_singular", "page", "tests",
                      "wrb2006") %in% names(r)), info = r$code)
  # the veracrisols and vertisols report their climates
  expect_match(rules$ger_by_code$veracrisols$result_note, "D17", fixed = TRUE)
  expect_match(rules$ger_by_code$vertisols$result_note, "reported", fixed = TRUE)
  v <- .validate_rules("rp2008")
  expect_identical(c(v$missing, v$unsuffixed, v$mixed, v$malformed, v$codes), character(0))
  ck <- .rp_rapid_key_check()
  expect_true(all(ck$implemented))
  expect_true(all(ck$by_the_book[ck$ger %in% six]))
  ord <- .rp_order(rules)
  # regosols (1) before reductisols (4a) before thalassosols (5a) before
  # thiosols (5b) before vertisols (6c) before salisols (6i) before
  # veracrisols (7b) before the chernosols (7c)
  expect_true(all(diff(match(c("regosols", "reductisols_typiques", "thalassosols_bruts", "thiosols", "lithovertisols",
                               "salisols_chloruro_sulfates", "veracrisols", "chernosols_hapliques"), ord)) > 0))
  expect_length(ord, 110L)
})

test_that("no later double or precedence is left; every row names references", {
  skip_on_cran()
  rules <- .rp_rules()
  st <- vapply(rules$doubles, function(d) d$status %||% "", "")
  expect_false(any(st == "later"))
  expect_true(all(st %in% c("", "candidate")))
  refs <- names(rules$ref_by_code)
  for (d in rules$doubles) expect_true(all(unlist(d$codes) %in% refs), info = paste(unlist(d$codes), collapse = "-"))
  for (r in rules$precedence) {
    expect_null(r$status)
    expect_true(all(c(unlist(r$first), unlist(r$second), unlist(r$except)) %in% c(refs, "*")),
                info = paste(unlist(r$first), collapse = ","))
  }
  for (x in rules$exclusions) expect_true(all(c(x$code, x$by) %in% refs))
  # the files say so too
  for (f in c("precedence.yaml", "doubles.yaml")) {
    y <- yaml::read_yaml(system.file("rules", "rp2008", f, package = "soilKey"))
    expect_false(any(grepl("status: later|\"later\"", yaml::as.yaml(y))), info = f)
  }
  # the pairs of this part, written
  pairs <- vapply(rules$doubles[st == ""], function(d) paste(unlist(d$codes), collapse = "-"), "")
  for (p in c("redoxisols-thalassosols_polderises", "reductisols_typiques-thalassosols_bruts",
              "reductisols_typiques-thalassosols_polderises", "salisols_chloruro_sulfates-reductisols_typiques",
              "sodisols_solonetziques-redoxisols", "salisodisols-reductisols_stagniques", "lithovertisols-redoxisols",
              "paravertisols_planosoliques-reductisols_typiques", "alocrisols_humiques-veracrisols"))
    expect_true(p %in% pairs, info = p)
  expect_false("thalassosols_polderises-redoxisols" %in% pairs)
  # the doubles of the hydromorphy: 3 + 1 thalassosols (the reductisols
  # typiques only, decision 4), 21 salsodic, 12 vertisols
  hyd <- Filter(function(d) any(unlist(d$codes) %in% c("reductisols_typiques", "reductisols_stagniques", "redoxisols")) &&
                  any(unlist(d$codes) %in% .rp52_refs), rules$doubles)
  expect_length(hyd, 37L)
})

test_that("the readings of part 8 are Hugo's decisions of 2026-11-04; none waits for his word", {
  skip_on_cran()
  rd <- yaml::read_yaml(system.file("rules", "rp2008", "readings.yaml", package = "soilKey"))$readings
  tp <- vapply(rd, `[[`, "", "topic")
  txt <- vapply(rd, `[[`, "", "reading")
  mine <- tp[grepl("(0.9.352)", tp, fixed = TRUE)]
  expect_setequal(mine, .rp52_pend)
  dec <- tp[grepl("\\(decision [1-7]\\)$", tp) & seq_along(tp) > max(match(mine, tp))]
  expect_length(dec, 7L)
  expect_true(all(grepl("(Hugo, 2026-11-04).", txt[match(c(mine, dec), tp)], fixed = TRUE)))
  expect_length(tp[grepl("to confirm", txt, fixed = TRUE)], 0L)
  expect_true(all(nzchar(vapply(rd[match(c(mine, dec), tp)], `[[`, "", "page"))))
  expect_true(all(grepl("\\(Hugo, 2026-11-0[234]\\)\\.?$", txt)))
})


# --- every reference: a simple fixture, an imperfect one, a failing twin ---------------

test_that("each example of part 8 attaches simply to its own reference; the Pro app offers them", {
  skip_on_cran()
  for (cd in .rp52_refs) {
    r <- .rp52_cl(.rp52_ex(cd))
    expect_identical(r$levels$code, cd, info = cd)
    expect_identical(.rp52_deg_of(r, cd), "simple", info = cd)
    expect_identical(r$provisional_because, character(0), info = cd)
  }
  ui <- readLines(system.file("shiny", "classify_app_pro", "R", "utils_ui.R", package = "soilKey"), encoding = "UTF-8")
  for (cd in .rp52_refs) expect_true(any(grepl(sprintf("\"%s\"", cd), ui, fixed = TRUE)), info = cd)
  expect_length(eval(formals(make_rp2008_example)$reference), 110L)
})

test_that("a value inside a tolerance band attaches imperfectly; a failing twin fails", {
  skip_on_cran()
  N <- .rp52_N
  ex <- .rp52_ex
  e <- .rp52_edit
  cases <- list(
    # the M at 13 cm: within the 10 +/- 5; at 20, none
    regosols = list(e(ex("regosols"), 1:2, bottom_cm = c(13, 80), top_cm = c(0, 13)),
                    e(ex("regosols"), 1:2, bottom_cm = c(20, 80), top_cm = c(0, 20))),
    # the salic horizon starting at 65 cm: within the 60 +/- 10; at 75, none
    salisols_chloruro_sulfates = list(e(ex("salisols_chloruro_sulfates"), 1:3, designation = c("A", "S", "SaC"), top_cm = c(0, 15, 65)),
                                      e(ex("salisols_chloruro_sulfates"), 1:3, designation = c("A", "S", "SaC"), top_cm = c(0, 15, 75))),
    # the pH of the salic horizon 8.4, its anions not recorded: within the
    # 8.5 +/- 0.2 of the alkaline series; 8.0, the neutral series
    salisols_carbonates = list(e(ex("salisols_carbonates"), 1:2, ph_h2o = c(8.4, 8.4), soluble_salts_carbonate_pct = NA),
                               e(ex("salisols_carbonates"), 1:2, ph_h2o = c(8.0, 8.0), soluble_salts_carbonate_pct = NA)),
    # the sodic horizon starting at 65 cm under an A: within the 60 +/- 10
    sodisols_indifferencies = list(e(ex("sodisols_indifferencies"), 1:3, designation = c("A", "A", "NaC"), bottom_cm = c(15, 65, 100), top_cm = c(0, 15, 65)),
                                   e(ex("sodisols_indifferencies"), 1:3, designation = c("A", "A", "NaC"), bottom_cm = c(15, 75, 100), top_cm = c(0, 15, 75))),
    # the NaBT at 65 cm: within the 60 +/- 10
    sodisols_solonetziques = list(e(ex("sodisols_solonetziques"), 2:4, top_cm = c(12, 65, 90), bottom_cm = c(65, 90, 110)),
                                  e(ex("sodisols_solonetziques"), 2:4, top_cm = c(12, 75, 95), bottom_cm = c(75, 95, 110))),
    # the pH of the E 5.1: within the 5.0 +/- 0.2 of the solodises; 6.0, none
    sodisols_solodises = list(e(ex("sodisols_solodises"), 2, designation = "E", ph_h2o = 5.1),
                              e(ex("sodisols_solodises"), 2, designation = "E", ph_h2o = 6.0)),
    # the salic horizon at 65 cm under the sodic one: within the 60 +/- 10
    sodisalisols = list(e(ex("sodisalisols"), 1:3, designation = c("NaA", "NaS", "SaC"), top_cm = c(0, 20, 65)),
                        e(ex("sodisalisols"), 1:3, designation = c("NaA", "NaS", "SaC"), top_cm = c(0, 20, 75))),
    salisodisols = list(e(ex("salisodisols"), 1:3, designation = c("SaA", "S", "NaC"), top_cm = c(0, 20, 65)),
                        e(ex("salisodisols"), 1:3, designation = c("SaA", "S", "NaC"), top_cm = c(0, 20, 75))),
    # the Js under an O of 12 cm: within the 10 +/- 5
    thalassosols_juveniles = list(N(c("OF", "Js", "M"), c(0, 12, 20), c(12, 20, 100), layer_origin = "marine",
                                    oc_pct = c(40, 0.4, 0.3), site = list(tidal_zone = "supratidal")),
                                  N(c("OF", "Js", "M"), c(0, 20, 30), c(20, 30, 100), layer_origin = "marine",
                                    oc_pct = c(40, 0.4, 0.3), site = list(tidal_zone = "supratidal"))),
    # the A under an O of 12 cm: within the 10 +/- 5
    thalassosols_polderises = list(N(c("OF", "A", "Jp", "M"), c(0, 12, 35, 70), c(12, 35, 70, 130), layer_origin = "marine",
                                     oc_pct = c(40, 3, 0.8, 0.5), site = list(poldered = TRUE)),
                                   N(c("OF", "A", "Jp", "M"), c(0, 20, 35, 70), c(20, 35, 70, 130), layer_origin = "marine",
                                     oc_pct = c(40, 3, 0.8, 0.5), site = list(poldered = TRUE))),
    # the TH starting at 55 cm: within the 50 +/- 10; at 65, none
    thiosols = list(e(ex("thiosols"), 1:3, top_cm = c(0, 55, 80), bottom_cm = c(55, 80, 120)),
                    e(ex("thiosols"), 1:3, top_cm = c(0, 65, 90), bottom_cm = c(65, 90, 120))),
    # the U starting at 55 cm: within the 50 +/- 10
    sulfatosols = list(e(ex("sulfatosols"), 1:3, top_cm = c(0, 55, 80), bottom_cm = c(55, 80, 120)),
                       e(ex("sulfatosols"), 1:3, top_cm = c(0, 65, 90), bottom_cm = c(65, 90, 120))),
    # Ah of 45 cm under heath: within the 50 +/- 10 (and the leptique); of 20, none
    veracrisols = list(e(ex("veracrisols"), 2:3, bottom_cm = c(45, 120), top_cm = c(30, 45)),
                       e(ex("veracrisols"), 1:3, bottom_cm = c(10, 20, 120), top_cm = c(0, 10, 20))),
    # the Av under an O of 12 cm: within the 10 +/- 5; the SV of a
    # topovertisol at pH 6.85, with no site recorded: within the 7 +/- 0.2
    lithovertisols = list(N(c("OF", "Av", "SV", "V", "C"), c(0, 12, 25, 50, 110), c(12, 25, 50, 110, 150), clay_pct = c(NA, 50, 55, 58, 50),
                            oc_pct = c(40, 3, 1, 0.5, 0.3), bs_pct = c(NA, 85, 88, 95, 100), site = list(slope_pct = 5)),
                          N(c("OF", "Av", "SV", "V", "C"), c(0, 20, 30, 50, 110), c(20, 30, 50, 110, 150), clay_pct = c(NA, 50, 55, 58, 50),
                            oc_pct = c(40, 3, 1, 0.5, 0.3), bs_pct = c(NA, 85, 88, 95, 100), site = list(slope_pct = 5))),
    topovertisols = list(e(ex("topovertisols"), 2, ph_h2o = 6.85, site = list(slope_pct = NA, landform = NA)),
                         e(ex("topovertisols"), 2, ph_h2o = 6.5, site = list(slope_pct = NA, landform = NA))),
    # the V starting at 100 cm is the last cm: strict (Q5); the A 40 %
    # clay or more: no paravertisol
    paravertisols_hapliques = list(NULL, e(ex("paravertisols_hapliques"), 1, clay_pct = 42)),
    paravertisols_planosoliques = list(NULL, e(ex("paravertisols_planosoliques"), 2, boundary_distinctness = "gradual"))
  )
  # the imperfect thalassosol: a sulfidic material at 55 cm, within the 60
  # +/- 10 the thalassosols forbid (the thiosol is imperfect too and comes
  # first, p. 310); at 45, none
  cases$thalassosols_bruts <- list(N(c("M", "TH"), c(0, 55), c(55, 120), layer_origin = "marine", s_total_pct = c(NA, 2),
                                     site = list(tidal_zone = "intertidal")),
                                   N(c("M", "TH"), c(0, 45), c(45, 120), layer_origin = "marine", s_total_pct = c(NA, 2),
                                     site = list(tidal_zone = "intertidal")))
  expect_identical(sort(names(cases)), sort(.rp52_refs))
  for (cd in names(cases)) {
    if (!is.null(cases[[cd]][[1]])) {
      a <- .rp52_deg(cases[[cd]][[1]], cd)
      expect_identical(a$degree, "imperfect", info = cd)
      expect_gt(length(a$imperfect_by), 0L)
    }
    expect_identical(.rp52_deg(cases[[cd]][[2]], cd)$degree, "fail", info = cd)
  }
  # the leptique of the veracrisols names the imperfect attachment (p. 315);
  # over a BT the luvisol tronque, simple, would come first by the degree
  r <- .rp52_cl(e(ex("veracrisols"), 2:4, designation = c("Ah", "C", "C"), bottom_cm = c(45, 120, 160), top_cm = c(30, 45, 120)))
  expect_identical(r$name, "veracrisol leptique")
  expect_identical(r$levels$attachment, "imperfect")
  # the thalassosol imperfect, the thiosol imperfect first (p. 310)
  r <- .rp52_cl(cases$thalassosols_bruts[[1]])
  expect_identical(r$levels$code, "thiosols")
  expect_identical(.rp52_deg_of(r, "thalassosols_bruts"), "imperfect")
})


# --- the regosols, salisols and sodisols ------------------------------------------------

test_that("the regosols: within 10 cm an M, D or C under one horizon; against the lithosols, histosols and alluvium (pp. 188, 209, 291-292)", {
  skip_on_cran()
  N <- .rp52_N
  # an R: the lithosol, not the regosol
  r <- .rp52_cl(N(c("A", "R"), c(0, 6), c(6, 60), oc_pct = c(3, NA)))
  expect_identical(r$name, "lithosol")
  expect_identical(.rp52_deg_of(r, "regosols"), character(0))
  # two horizons over the C, or a horizon neither organo-mineral nor
  # holorganic (a Ym): none
  expect_identical(.rp52_deg(N(c("A", "S", "C"), c(0, 4, 8), c(4, 8, 60)), "regosols")$degree, "fail")
  expect_identical(.rp52_deg(N(c("Ym", "C"), c(0, 10), c(10, 100)), "regosols")$degree, "fail")
  # an A over 10 cm from the surface: the arenosol, the regosol within the
  # tolerance a candidate (pp. 101, 292)
  r <- .rp52_cl(N(c("A", "C", "R"), c(0, 15, 30), c(15, 30, 60), sand_pct = c(90, 92, NA), clay_pct = c(4, 3, NA)))
  expect_identical(r$levels$code, "arenosols")
  expect_identical(.rp52_deg_of(r, "regosols"), "imperfect")
  expect_match(r$candidates$precedence[r$candidates$code == "regosols"], "arenosols first", fixed = TRUE)
  # H of 8 cm over M: the regosol epihistique, not a histosol leptique (p. 209)
  r <- .rp52_cl(N(c("Hs", "M"), c(0, 8), c(8, 60), oc_pct = c(40, NA)))
  expect_identical(r$name, "r\u00e9gosol")
  expect_match(r$candidates$precedence[r$candidates$code == "histosols_leptiques"], "regosols first", fixed = TRUE)
  # a young alluvium: the fluviosol juvenile; a recorded recent alluvium is
  # no regosol (p. 291; the review's D8)
  r <- .rp52_cl(N(c("Js", "M"), c(0, 8), c(8, 80), oc_pct = c(0.3, NA), structure_type = c("subangular blocky", NA),
                  structure_grade = c("weak", NA), layer_origin = "fluvic",
                  site = list(parent_material = "Loire alluvium", valley_position = "major bed", flood_frequency = "occasional")))
  expect_identical(r$name, "fluviosol juv\u00e9nile")
  expect_identical(.rp52_deg_of(r, "regosols"), character(0))
  # a G within 50 cm: the reductisol whatever lies above, the regosol a candidate
  r <- .rp52_cl(N(c("A", "C", "G"), c(0, 8, 30), c(8, 30, 120), oc_pct = c(3, 0.3, 0.2)))
  expect_identical(r$name, "r\u00e9ductisol typique")
  expect_identical(.rp52_deg_of(r, "regosols"), "simple")
  # on the data: a soft chalk with no fine earth under a thin A is the M
  p <- .rp52_N(NULL, c(0, 6), c(6, 80), oc_pct = c(2.5, NA), clay_pct = c(20, NA), caco3_pct = c(25, 85),
               structure_type = c("granular", NA), rupture_resistance = c(NA, "slightly hard"), rp = FALSE,
               site = list(parent_material = "chalk"))
  expect_identical(.rp52_cl(p)$name, "r\u00e9gosol")
  # the hardness not recorded (an FAO Cr): undecided against the lithosols,
  # never a silent FALSE
  p <- .rp52_N(c("A", "Cr"), c(0, 6), c(6, 80), oc_pct = c(2.5, NA), clay_pct = c(20, NA), rp = FALSE)
  r <- .rp52_cl(p)
  expect_identical(.rp52_deg_of(r, "regosols"), "undecided")
  expect_true("references:regosols" %in% r$provisional_because)
  # an undecided layer deeper than the window is no obstacle: no provisional
  # regosol on the data (the salisol of the sebkha)
  r <- .rp52_cl(.rp52_data(.rp52_ex("salisols_chloruro_sulfates")))
  expect_identical(r$name, "salisol chloruro-sulfat\u00e9")
  expect_false("references:regosols" %in% r$provisional_because)
})

test_that("the salisols and sodisols: the series, the order of the horizons, the doubles (pp. 295-299)", {
  skip_on_cran()
  N <- .rp52_N
  ex <- .rp52_ex
  e <- .rp52_edit
  # the anions decide the series; with none, the pH in water stands in
  r <- .rp52_cl(e(ex("salisols_chloruro_sulfates"), 1:3, soluble_salts_carbonate_pct = 70, soluble_salts_chloride_pct = 15,
                  soluble_salts_sulfate_pct = 10))
  expect_identical(r$name, "salisol carbonat\u00e9")
  d <- .rp52_pred(e(ex("salisols_carbonates"), 1:3, soluble_salts_carbonate_pct = NA), salic_series_rp, series = "alkaline")
  expect_true(d$passed)
  expect_match(d$notes, "stands for the pH of the paste extract", fixed = TRUE)
  # neither recorded: both undecided, the name provisional
  r <- .rp52_cl(e(ex("salisols_chloruro_sulfates"), 1:3, ph_h2o = NA, soluble_salts_carbonate_pct = NA,
                  soluble_salts_chloride_pct = NA, soluble_salts_sulfate_pct = NA))
  expect_identical(.rp52_deg_of(r, "salisols_chloruro_sulfates"), "undecided")
  expect_identical(.rp52_deg_of(r, "salisols_carbonates"), "undecided")
  expect_true("references:salisols_chloruro_sulfates" %in% r$provisional_because)
  # an EC under 8 dS/m: no salic horizon
  expect_identical(.rp52_deg(e(ex("salisols_chloruro_sulfates"), 1:3, designation = c("A", "S", "C"), ec_dS_m = 5),
                             "salisols_chloruro_sulfates")$degree, "fail")
  # the order of superposition: the sodisalisol turned over is a salisodisol
  r <- .rp52_cl(e(ex("sodisalisols"), 1:3, designation = c("SaA", "NaS", "NaC"), ec_dS_m = c(20, 3, 3),
                  structure_type = c("granular", "prismatic", "massive")))
  expect_identical(r$name, "salisodisol")
  # the sodic horizon written NaBT: the solonetzique; an Ea, the solodise
  expect_identical(.rp52_cl(e(ex("sodisols_solonetziques"), 2, designation = "Ea", ph_h2o = 4.6))$name, "sodisol solodis\u00e9")
  expect_identical(.rp52_deg(e(ex("sodisols_solonetziques"), 3, designation = "BT"), "sodisols_solonetziques")$degree, "fail")
  # an E: no sodisol indifferencie
  expect_identical(.rp52_deg(e(ex("sodisols_indifferencies"), 1, designation = "E"), "sodisols_indifferencies")$degree, "fail")
  # a columnar structure with the sodium not recorded: undecided
  r <- .rp52_cl(.rp52_data(e(ex("sodisols_indifferencies"), 1:3, na_cmol = NA, structure_type = "columnar")))
  expect_true("undecided" %in% r$candidates$degree[r$candidates$code == "sodisols_indifferencies"] ||
                identical(r$name, "[non rattach\u00e9]"))
  # on the data: the salic and sodic overlays read from the EC and the sodium
  expect_identical(.rp52_cl(.rp52_data(ex("salisols_chloruro_sulfates")))$levels$code, "salisols_chloruro_sulfates")
  expect_identical(.rp52_cl(.rp52_data(ex("sodisols_indifferencies")))$levels$code, "sodisols_indifferencies")
  expect_identical(.rp52_cl(.rp52_data(ex("sodisalisols")))$levels$code, "sodisalisols")
  # a G within 50 cm: the double salisol-reductisol (pp. 298-299)
  r <- .rp52_cl(e(ex("salisols_chloruro_sulfates"), 2:3, designation = c("SaG", "SaG")))
  expect_identical(r$name, "salisol chloruro-sulfat\u00e9-r\u00e9ductisol typique")
  expect_identical(r$levels$attachment, c("double", "double"))
  # redox features from under 50 cm over 50 cm with an E: the double
  # sodisol solonetzique-redoxisol
  r <- .rp52_cl(e(ex("sodisols_solonetziques"), 2:4, designation = c("Eg", "NaBTg", "NaCg"),
                  redoximorphic_features_pct = c(10, 20, 15)))
  expect_identical(r$name, "sodisol solonetzique-r\u00e9doxisol")
  # too sandy for a saturated paste: an arenosol salin (pp. 102, 298)
  r <- .rp52_cl(N(c("SaA", "SaC"), c(0, 15), c(15, 130), oc_pct = c(1, 0.2), sand_pct = 85, clay_pct = 5, ec_dS_m = c(20, 18),
                  ph_h2o = 8, soluble_salts_chloride_pct = 70, soluble_salts_sulfate_pct = 20, soluble_salts_carbonate_pct = 5,
                  site = list(country = "TN", lat = 34, koppen_climate = "BWh")))
  expect_identical(r$name, "ar\u00e9nosol")
  expect_match(r$candidates$excluded_by[r$candidates$code == "salisols_chloruro_sulfates"], "arenosols", fixed = TRUE)
})


# --- the thalassosols, thiosols and sulfatosols ------------------------------------------

test_that("the thalassosols: a marine material, the closed list, the horizons and the site (pp. 303-306)", {
  skip_on_cran()
  N <- .rp52_N
  ex <- .rp52_ex
  e <- .rp52_edit
  # no marine origin recorded: no thalassosol (Q14)
  expect_identical(.rp52_deg(e(ex("thalassosols_polderises"), 1:3, layer_origin = NA, site = list(parent_material = "silts")),
                             "thalassosols_polderises")$degree, "fail")
  # the site not recorded: provisional; a polder fails the bruts
  r <- .rp52_cl(e(ex("thalassosols_bruts"), site = list(tidal_zone = NULL, land_use = NULL)))
  expect_identical(r$name, "thalassosol brut")
  expect_true("references:thalassosols_bruts" %in% r$provisional_because)
  expect_identical(.rp52_deg(e(ex("thalassosols_bruts"), site = list(poldered = TRUE)), "thalassosols_bruts")$degree, "fail")
  # poldered not recorded: an artificial drainage stands in; nothing, provisional
  r <- .rp52_cl(e(ex("thalassosols_polderises"), site = list(poldered = NULL, artificially_drained = TRUE)))
  expect_identical(r$name, "thalassosol pold\u00e9ris\u00e9")
  expect_identical(r$provisional_because, character(0))
  r <- .rp52_cl(e(ex("thalassosols_polderises"), site = list(poldered = NULL)))
  expect_true("references:thalassosols_polderises" %in% r$provisional_because)
  expect_identical(.rp52_deg(e(ex("thalassosols_polderises"), site = list(poldered = FALSE)), "thalassosols_polderises")$degree, "fail")
  # the closed list: an S or a BT fails them
  expect_identical(.rp52_deg(e(ex("thalassosols_polderises"), 2, designation = "S"), "thalassosols_polderises")$degree, "fail")
  # a salic horizon within 60 cm: the salisol's
  expect_identical(.rp52_deg(e(ex("thalassosols_polderises"), 2, designation = "SaJp"), "thalassosols_polderises")$degree, "fail")
  # a G within 50 cm: the double reductisol-thalassosol (p. 305); a g, the
  # double redoxisol-thalassosol polderise (pp. 285, 305; Q8)
  r <- .rp52_cl(N(c("A", "Jp", "Gr"), c(0, 20, 40), c(20, 40, 130), layer_origin = "marine", oc_pct = c(3, 0.8, 0.5),
                  site = list(poldered = TRUE)))
  expect_identical(r$name, "r\u00e9ductisol typique-thalassosol pold\u00e9ris\u00e9")
  r <- .rp52_cl(N(c("A", "Jpg", "Mg"), c(0, 25, 70), c(25, 70, 130), layer_origin = "marine", oc_pct = c(3, 0.8, 0.5),
                  redoximorphic_features_pct = c(NA, 15, 20), site = list(poldered = TRUE)))
  expect_identical(r$name, "r\u00e9doxisol-thalassosol pold\u00e9ris\u00e9")
  expect_identical(r$levels$code, c("redoxisols", "thalassosols_polderises"))
  # a marine material is no alluvium of the fluviosols; a fluviatile one is
  r <- .rp52_cl(e(ex("thalassosols_polderises"), 1:3, layer_origin = "fluvic",
                  site = list(poldered = NULL, parent_material = "Loire alluvium", valley_position = "major bed",
                              flood_frequency = "occasional")))
  expect_identical(r$name, "fluviosol typique")
  expect_false(isTRUE(.rp52_pred(e(ex("fluviosols_typiques"), 1:3, layer_origin = "marine",
                                   site = list(parent_material = "marine silts")), alluvial_rp)$passed))
})

test_that("the thiosols and sulfatosols: the TH and the U within 50 cm, first in every case (pp. 308-310)", {
  skip_on_cran()
  ex <- .rp52_ex
  e <- .rp52_edit
  # a consistency recorded under 1.4 in the TH, a pH after drying over 3.5: no thiosol
  expect_identical(.rp52_deg(e(ex("thiosols"), 2:3, n_value = c(1.0, 1.0)), "thiosols")$degree, "fail")
  expect_identical(.rp52_deg(e(ex("thiosols"), 2:3, incubation_ph = c(4.5, 4.6)), "thiosols")$degree, "fail")
  # neither recorded: the TH stands for them, noted
  d <- .rp52_deg(e(ex("thiosols"), 2:3, n_value = NA, incubation_ph = NA), "thiosols")
  expect_identical(d$degree, "simple")
  # a U within 50 cm: the sulfatosol
  expect_identical(.rp52_deg(ex("sulfatosols"), "thiosols")$degree, "fail")
  # the description ending at the U: simple or imperfect; a C under it:
  # imperfect (decision 3)
  p <- .rp52_N(c("A", "U"), c(0, 15), c(15, 45), ph_h2o = c(4.5, 3.2), jarosite_present = c(NA, TRUE),
               s_total_pct = c(NA, 1.5), site = list(country = "SN", lat = 12.6, koppen_climate = "Aw"))
  r <- .rp52_cl(p)
  expect_identical(.rp52_deg_of(r, "sulfatosols"), "simple_or_imperfect")
  expect_identical(r$levels$code, "sulfatosols")
  expect_identical(.rp52_deg(e(ex("sulfatosols"), 3, designation = "C"), "sulfatosols")$degree, "imperfect")
  # a salic U: the sulfatosol (salique), the salisol a candidate (pp. 299, 310)
  r <- .rp52_cl(e(ex("sulfatosols"), 2, designation = "SaU", ec_dS_m = 20, soluble_salts_chloride_pct = 60,
                  soluble_salts_sulfate_pct = 30, soluble_salts_carbonate_pct = 5))
  expect_identical(r$name, "sulfatosol")
  expect_identical(.rp52_deg_of(r, "salisols_chloruro_sulfates"), "simple")
  expect_match(r$candidates$precedence[r$candidates$code == "salisols_chloruro_sulfates"], "sulfatosols first", fixed = TRUE)
  # on the data: a waterlogged material with sulfides, its pH falling on drying
  expect_identical(.rp52_cl(.rp52_data(ex("thiosols")))$name, "thiosol")
})


# --- the veracrisols and vertisols --------------------------------------------------------

test_that("the veracrisols: the vermihumic Ah under heath or culture (pp. 313-316)", {
  skip_on_cran()
  ex <- .rp52_ex
  e <- .rp52_edit
  # under culture: an Ah under the LAh with its base deeper than 50 cm, the
  # complex not tested
  r <- .rp52_cl(e(ex("veracrisols"), 1:2, designation = c("LAh", "Ah"), bs_pct = c(70, 40), site = list(land_use = "cropland")))
  expect_identical(r$name, "veracrisol")
  # under heath the complex must be acid and aluminic
  expect_identical(.rp52_deg(e(ex("veracrisols"), 1:2, bs_pct = 50), "veracrisols")$degree, "fail")
  expect_identical(.rp52_deg(e(ex("veracrisols"), 1:2, al_kcl_cmol = 0.5), "veracrisols")$degree, "fail")
  # not dark, or not biomacrostructured: none
  expect_identical(.rp52_deg(e(ex("veracrisols"), 1, munsell_chroma_moist = 4), "veracrisols")$degree, "fail")
  expect_identical(.rp52_deg(e(ex("veracrisols"), 2, biogenic_aggregates_pct = 10), "veracrisols")$degree, "fail")
  # a BP: no veracrisol (no podzolic character)
  expect_identical(.rp52_deg(e(ex("veracrisols"), 3, designation = "BPs"), "veracrisols")$degree, "fail")
  # the colour not recorded on the data: undecided
  p <- .rp52_data(e(ex("veracrisols"), 1:2, munsell_value_moist = NA, munsell_chroma_moist = NA))
  expect_identical(.rp52_deg(p, "veracrisols")$degree, "undecided")
  # on the data: the dark A layers of the surface run
  expect_identical(.rp52_cl(.rp52_data(ex("veracrisols")))$name, "veracrisol")
  # with a Sal: the double alocrisol humique-veracrisol (p. 316)
  r <- .rp52_cl(e(ex("veracrisols"), 1:4, designation = c("Ah", "Ah", "Salh", "C"), oc_pct = c(9, 4, 2.2, 0.3),
                  ph_h2o = c(4.4, 4.6, 4.7, 4.8), cec_cmol = c(12, 10, 8, 6)))
  expect_identical(r$name, "alocrisol humique-veracrisol")
})

test_that("the vertisols: SV and V, litho against topo, the paravertisols, the other GER (pp. 317-326)", {
  skip_on_cran()
  ex <- .rp52_ex
  e <- .rp52_edit
  # the site decides litho against topo; else the SV (Q20); an unsaturated
  # SV makes the lithovertisol (decision 2)
  expect_identical(.rp52_cl(e(ex("lithovertisols"), 2, bs_pct = 100, site = list(slope_pct = 1)))$name, "topovertisol")
  expect_identical(.rp52_cl(e(ex("lithovertisols"), 2, bs_pct = 100, ph_h2o = 7.5,
                              site = list(slope_pct = NULL, landform = NULL)))$name, "topovertisol")
  expect_identical(.rp52_cl(e(ex("lithovertisols"), site = list(slope_pct = NULL, landform = NULL)))$name, "lithovertisol")
  r <- .rp52_cl(e(ex("lithovertisols"), 2, bs_pct = NA, site = list(slope_pct = NULL, landform = NULL)))
  expect_identical(.rp52_deg_of(r, "lithovertisols"), "undecided")
  expect_identical(.rp52_deg_of(r, "topovertisols"), "undecided")
  expect_true(all(c("references:lithovertisols", "references:topovertisols") %in% r$provisional_because))
  # no SV, under 40 % clay, a V deeper than 100 cm: no vertisol
  expect_identical(.rp52_deg(e(ex("lithovertisols"), 2, designation = "S"), "lithovertisols")$degree, "fail")
  expect_identical(.rp52_deg(e(ex("lithovertisols"), 2, clay_pct = 38), "lithovertisols")$degree, "fail")
  expect_identical(.rp52_deg(e(ex("lithovertisols"), 2:4, top_cm = c(15, 101, 130), bottom_cm = c(101, 130, 150)),
                             "lithovertisols")$degree, "fail")
  # a recorded kaolinite fails the smectitic SV and V
  expect_identical(.rp52_deg(e(ex("lithovertisols"), 2:3, clay_mineralogy = "kaolinite"), "lithovertisols")$degree, "fail")
  # the abrupt boundary of the E not recorded: the planosolique undecided
  r <- .rp52_cl(e(ex("paravertisols_planosoliques"), 2, boundary_distinctness = NA))
  expect_identical(.rp52_deg_of(r, "paravertisols_planosoliques"), "undecided")
  # a VSa: the vertisol, the salisol a candidate (Q20)
  r <- .rp52_cl(e(ex("lithovertisols"), 3, designation = "SaV", ec_dS_m = 20, soluble_salts_chloride_pct = 60,
                  soluble_salts_sulfate_pct = 30, soluble_salts_carbonate_pct = 5))
  expect_identical(r$name, "lithovertisol")
  expect_match(r$candidates$precedence[r$candidates$code == "salisols_chloruro_sulfates"], "lithovertisols first", fixed = TRUE)
  # a prolonged waterlogging: the doubles of p. 325
  r <- .rp52_cl(e(ex("lithovertisols"), 2:3, designation = c("SVg", "Vg"), redoximorphic_features_pct = c(15, 20)))
  expect_identical(r$name, "lithovertisol-r\u00e9doxisol")
  r <- .rp52_cl(e(ex("paravertisols_planosoliques"), 2:4, designation = c("Eg", "SVg", "Vg"), redoximorphic_features_pct = c(15, 20, 20)))
  expect_identical(r$name, "paravertisol planosolique-r\u00e9doxisol")
  # no V: the leptismectisol, not a vertisol
  expect_identical(.rp52_cl(.rp52_ex("leptismectisols"))$name, "leptismectisol")
  # on the data: the vertic characters of the Av read on the surface A
  p <- .rp52_data(e(ex("lithovertisols"), 1:4, oc_pct = c(2.5, 1, 0.6, 0.3), munsell_hue_moist = "10YR",
                    munsell_value_moist = c(4, 4, 4, 5), munsell_chroma_moist = c(2, 2, 2, 3),
                    structure_grade = c("strong", "strong", "strong", NA), na_cmol = 0.3, ca_cmol = c(25, 25, 28, 30),
                    mg_cmol = 5, k_cmol = 0.5))
  expect_identical(.rp52_cl(p)$name, "lithovertisol")
})


# --- the review of part 8 and Hugo's decisions (2026-11-04) -------------------------------

test_that("the review: VSa and VNa, the vertic surface, a dark SV, the sodic horizon by the ESP (D1-D4)", {
  skip_on_cran()
  N <- .rp52_N
  # D1: VSa and VNa are the salic and sodic horizons too (p. 318)
  expect_true("Sa" %in% .rp_parse_one("VSa")$overlay)
  expect_true("Na" %in% .rp_parse_one("VNa")$overlay)
  expect_identical(.rp_parse_one("VSa")$features, "Sa")
  p <- N(c("Av", "SV", "VSa", "C"), c(0, 20, 50, 110), c(20, 50, 110, 150), clay_pct = c(50, 55, 58, 50), ec_dS_m = c(4, 6, 20, 15),
         ph_h2o = 8, site = list(slope_pct = 1))
  r <- .rp52_cl(p)
  expect_identical(r$name, "topovertisol")
  expect_identical(.rp52_deg_of(r, "salisols_chloruro_sulfates"), "simple")
  expect_match(r$candidates$precedence[r$candidates$code == "salisols_chloruro_sulfates"], "topovertisols first", fixed = TRUE)
  r <- .rp52_cl(N(c("Av", "SV", "VNa", "C"), c(0, 20, 50, 110), c(20, 50, 110, 150), clay_pct = c(50, 55, 58, 50),
                  site = list(slope_pct = 1)))
  expect_identical(.rp52_deg_of(r, "sodisols_indifferencies"), "simple")
  # on the data a V with Na over 15 % of the CEC is a VNa
  p <- .rp52_N(NULL, c(0, 20, 50), c(20, 50, 110), clay_pct = c(50, 55, 58), oc_pct = c(2, 1, 0.5), esp_pct = c(NA, NA, 20),
               slickensides = c(NA, NA, "many"), rp = FALSE)
  fr <- .rp_identify(p)
  expect_identical(fr$code[3], "V")
  expect_true("Na" %in% fr$overlay[[3]])
  # D2: a written A, LA or SaA over SV and V: the vertic surface read on the
  # data, undecided when not recorded, never a silent FALSE
  for (a in c("A", "LA", "SaA")) {
    p <- N(c(a, "SV", "V", "C"), c(0, 25, 60, 120), c(25, 60, 120, 160), clay_pct = c(50, 55, 58, 50), site = list(slope_pct = 6))
    expect_identical(.rp52_deg(p, "lithovertisols")$degree, "undecided", info = a)
    p <- .rp52_edit(p, 1, cracks_width_cm = 2)
    expect_identical(.rp52_deg(p, "lithovertisols")$degree, "simple", info = a)
  }
  # D3: a dark, humic SV under the A run stays an SV candidate
  p <- .rp52_N(NULL, c(0, 20, 60, 120), c(20, 60, 120, 150), clay_pct = c(60, 62, 62, 50), oc_pct = c(3, 1.5, 0.6, NA),
               munsell_hue_moist = "10YR", munsell_value_moist = c(2, 3, 3, 5), munsell_chroma_moist = c(1, 1, 2, 3),
               cracks_width_cm = c(3, 2, NA, NA), structure_type = c("granular", "prismatic", "wedge-shaped", NA),
               structure_type_secondary = c(NA, "angular blocky", NA, NA), slickensides = c(NA, "few", "many", NA),
               bs_pct = c(100, 100, 100, NA), ph_h2o = c(7.2, 7.5, 7.8, NA), rp = FALSE,
               site = list(country = "UY", lat = -32, koppen_climate = "Cfa", landform = "depression", slope_pct = 1))
  expect_true("SV" %in% .rp_identify(p)$candidates[[2]])
  expect_identical(.rp52_deg(p, "topovertisols")$degree, "undecided")
  # D4: a massive sodic horizon known by its ESP only
  p <- .rp52_N(NULL, c(0, 15, 60), c(15, 60, 110), clay_pct = c(30, 32, 30), oc_pct = c(1.5, 0.5, NA), ph_h2o = 9.3,
               structure_type = "massive", esp_pct = c(35, 40, 38), ec_dS_m = c(2, 3, 3), rp = FALSE)
  expect_identical(.rp52_cl(p)$name, "sodisol indiff\u00e9renci\u00e9")
})

test_that("the review: the solodises, the NaBT under an A, one horizon, the alluvium (D5-D8)", {
  skip_on_cran()
  N <- .rp52_N
  # D5: a written E with no pH is no solodise; on the data the pH missing
  # leaves it undecided
  r <- .rp52_cl(N(c("A", "E", "NaBT", "C"), c(0, 10, 25, 70), c(10, 25, 70, 110)))
  expect_identical(r$name, "sodisol solonetzique")
  p <- .rp52_data(.rp52_edit(.rp52_ex("sodisols_solodises"), 1:2, ph_h2o = NA))
  expect_identical(.rp52_deg(p, "sodisols_solodises")$degree, "undecided")
  # D6: an A directly on the NaBT is a sodisol solonetzique, imperfect,
  # before a luvisol tronque
  r <- .rp52_cl(N(c("A", "NaBT", "NaC"), c(0, 15, 60), c(15, 60, 120), ph_h2o = c(7.5, 9.0, 9.2)))
  expect_identical(r$levels$code, "sodisols_solonetziques")
  expect_identical(r$levels$attachment, "imperfect")
  # D7: adjacent layers of one code are one horizon
  expect_identical(.rp52_cl(N(c("A", "A", "C"), c(0, 4, 9), c(4, 9, 100)))$name, "r\u00e9gosol")
  expect_identical(.rp52_cl(N(c("A1", "A2", "C"), c(0, 4, 9), c(4, 9, 100)))$name, "r\u00e9gosol")
  expect_identical(.rp52_cl(N(c("A", "A", "R"), c(0, 4, 9), c(4, 9, 100)))$name, "lithosol")
  # D8: a recorded recent alluvium is no regosol
  p <- N(c("M", "D"), c(0, 30), c(30, 100), layer_origin = "fluvic", site = list(valley_position = "major bed", flood_frequency = "frequent"))
  expect_identical(.rp52_deg(p, "regosols")$degree, "fail")
  expect_identical(.rp52_deg(N(c("A", "C"), c(0, 8), c(8, 100), layer_origin = "fluvial"), "regosols")$degree, "fail")
  # the minor point: a BT deeper than 60 cm does not bar the indifferencies
  expect_identical(.rp52_cl(N(c("NaA", "NaS", "BT"), c(0, 15, 90), c(15, 90, 130), ph_h2o = 9))$name,
                   "sodisol indiff\u00e9renci\u00e9")
})

test_that("the review: the thalassosols in the solum and their site, the veracrisol ending in the Ah (D9-D11)", {
  skip_on_cran()
  N <- .rp52_N
  bri <- list(parent_material = "bri (old marine clay)")
  # D9: an R at 150 cm or a D at 110 cm is under the solum
  expect_identical(.rp52_cl(N(c("A", "Jp", "M", "R"), c(0, 25, 70, 150), c(25, 70, 150, 170),
                              site = c(bri, list(poldered = TRUE))))$name, "thalassosol pold\u00e9ris\u00e9")
  expect_identical(.rp52_cl(N(c("A", "Jp", "M", "D"), c(0, 25, 70, 110), c(25, 70, 110, 150),
                              site = c(bri, list(poldered = TRUE))))$name, "thalassosol pold\u00e9ris\u00e9")
  # D10: a polder, reclaimed or former tidal land is read before the tidal words
  for (s in list(list(land_use = "grassland on a reclaimed salt marsh (polder)"), list(landform = "former tidal flat, embanked 1850"))) {
    r <- .rp52_cl(N(c("A", "Jp", "M"), c(0, 25, 70), c(25, 70, 130), site = c(bri, s)))
    expect_identical(r$name, "thalassosol pold\u00e9ris\u00e9")
    expect_identical(r$provisional_because, character(0))
  }
  expect_false(isTRUE(.rp52_pred(N(c("A", "Jp", "M"), c(0, 25, 70), c(25, 70, 130), site = list(parent_material = "submarine fan")),
                                 marine_rp)$passed))
  expect_false(isTRUE(.rp52_pred(N(c("A", "Jp", "M"), c(0, 25, 70), c(25, 70, 130), site = list(parent_material = "non-tidal lagoon clay")),
                                 marine_rp)$passed))
  expect_true(.rp52_pred(N(c("A", "Jp", "M"), c(0, 25, 70), c(25, 70, 130), site = list(parent_material = "fluvio-marine silts")),
                         marine_rp)$passed)
  # D11: a description ending in the Ah leaves the veracrisol undecided
  p <- .rp52_edit(.rp52_ex("veracrisols"), 1:2, top_cm = c(0, 60), bottom_cm = c(60, 120))
  h <- as.data.frame(p$horizons)[1:2, ]
  p <- PedonRecord$new(site = p$site, horizons = ensure_horizon_schema(data.table::as.data.table(h)))
  expect_identical(.rp52_deg(p, "veracrisols")$degree, "undecided")
})

test_that("Hugo's decisions 1-7 on the review of part 8", {
  skip_on_cran()
  N <- .rp52_N
  ex <- .rp52_ex
  e <- .rp52_edit
  # 1: a vermihumic Ah of 30 cm or more, the veracrisol leptique before a
  # luvisol tronque
  r <- .rp52_cl(e(ex("veracrisols"), 2:3, bottom_cm = c(45, 120), top_cm = c(30, 45)))
  expect_identical(r$name, "veracrisol leptique")
  expect_identical(.rp52_deg_of(r, "luvisols_tronques"), "simple")
  # 2: an unsaturated SV makes the lithovertisol in a low position; with no
  # site recorded the name is provisional
  r <- .rp52_cl(e(ex("lithovertisols"), site = list(slope_pct = 1, landform = "valley bottom")))
  expect_identical(r$name, "lithovertisol")
  r <- .rp52_cl(e(ex("lithovertisols"), site = list(slope_pct = NULL, landform = NULL)))
  expect_identical(r$name, "lithovertisol")
  expect_true("references:lithovertisols" %in% r$provisional_because)
  expect_true(any(grepl("decision 2", r$warnings, fixed = TRUE)))
  r <- .rp52_cl(N(c("LAv", "SV", "V", "C"), c(0, 25, 60, 130), c(25, 60, 130, 170), clay_pct = c(52, 58, 60, 50),
                  bs_pct = c(90, 85, 95, 100), ph_h2o = c(6.8, 6.9, 7.4, 7.8),
                  site = list(country = "BG", lat = 42.5, koppen_climate = "Cfa", slope_pct = 0.5, landform = "former marsh, drained")))
  expect_identical(r$name, "lithovertisol")
  # 3: a U with no TH under it: the sulfatosol imperfect, before the reductisol
  mg <- list(country = "SN", lat = 12.6, koppen_climate = "Aw", land_use = "mangrove")
  r <- .rp52_cl(N(c("A", "U", "G"), c(0, 20, 45), c(20, 45, 120), site = mg))
  expect_identical(r$levels$code, "sulfatosols")
  expect_identical(r$levels$attachment, "imperfect")
  # 4: no double reductisol stagnique-thalassosol
  dd <- vapply(.rp_rules()$doubles, function(d) paste(unlist(d$codes), collapse = "-"), "")
  expect_false(any(grepl("reductisols_stagniques-thalassosols", dd, fixed = TRUE)))
  # 5: a reductisol within the tolerance makes no salsodic double
  r <- .rp52_cl(N(c("SaA", "SaS", "SaG"), c(0, 20, 55), c(20, 55, 130), ec_dS_m = c(25, 20, 18), ph_h2o = 8,
                  soluble_salts_chloride_pct = 70, soluble_salts_sulfate_pct = 20, soluble_salts_carbonate_pct = 2))
  expect_identical(r$name, "salisol chlorur\u006f-sulfat\u00e9")
  expect_identical(.rp52_deg_of(r, "reductisols_typiques"), "imperfect")
  # 6: a peyric X over a TH within 50 cm: the thiosol, not the peyrosol
  r <- .rp52_cl(N(c("X", "TH"), c(0, 55), c(55, 120), coarse_fragments_pct = c(70, NA), site = mg))
  expect_identical(r$levels$code, "thiosols")
  expect_match(r$candidates$precedence[r$candidates$code == "peyrosols"], "thiosols first", fixed = TRUE)
  # 7: a written SV and V with no clay recorded carry it
  r <- .rp52_cl(N(c("Av", "SV", "V", "C"), c(0, 25, 60, 120), c(25, 60, 120, 160), site = list(slope_pct = 5)))
  expect_identical(r$name, "lithovertisol")
})


# --- the full matrix ---------------------------------------------------------------------

test_that("the full matrix: each of the 110 examples gives its own reference, simple, with its candidates", {
  skip_on_cran()
  # every candidate besides the reference of the name, with its degree: a
  # change of the cross-GER precedence or doubles must update this
  # deliberately
  expected <- list(
    alocrisols_typiques = character(0),
    alocrisols_humiques = character(0),
    vitrandosols = character(0),
    silandosols_eutriques = character(0),
    silandosols_dystriques = character(0),
    silandosols_perhydriques = c("silandosols_dystriques:simple"),
    aluandosols_hapliques = character(0),
    aluandosols_perhydriques = c("aluandosols_hapliques:simple"),
    anthroposols_transformes = c("brunisols_eutriques:simple"),
    anthroposols_artificiels = character(0),
    anthroposols_reconstitues = character(0),
    anthroposols_construits = c("anthroposols_artificiels:simple"),
    anthroposols_archeologiques = character(0),
    arenosols = character(0),
    brunisols_eutriques = character(0),
    brunisols_dystriques = character(0),
    rendosols = character(0),
    calcosols = character(0),
    dolomitosols = character(0),
    rendisols = character(0),
    calcisols = c("brunisols_eutriques:simple"),
    magnesisols = c("brunisols_eutriques:simple"),
    calcarisols = c("rendosols:simple"),
    chernosols_hapliques = character(0),
    chernosols_typiques = character(0),
    chernosols_melanoluviques = character(0),
    colluviosols = character(0),
    cryosols_histiques = c("histosols_mesiques:simple"),
    cryosols_mineraux = character(0),
    ferrallitisols_meubles = character(0),
    ferrallitisols_nodulaires = character(0),
    ferrallitisols_petroxydiques = character(0),
    ferrallitisols_duroxydiques = c("lithosols:simple"),
    oxydisols_meubles = character(0),
    oxydisols_nodulaires = character(0),
    oxydisols_petroxydiques = c("lithosols:simple"),
    ferruginosols_meubles = c("brunisols_eutriques:undecided", "brunisols_dystriques:undecided"),
    ferruginosols_semiluviques = character(0),
    ferruginosols_luviques = c("luvisols_typiques:simple"),
    ferruginosols_petroxydiques = c("lithosols:simple", "luvisols_typiques:simple"),
    fersialsols_carbonates = character(0),
    fersialsols_calciques = character(0),
    fersialsols_insatures = character(0),
    fersialsols_eluviques = character(0),
    # 0.9.352, the review D8: a recorded recent alluvium is no regosol
    fluviosols_bruts = character(0),
    fluviosols_juveniles = character(0),
    fluviosols_typiques = character(0),
    fluviosols_brunifies = c("brunisols_eutriques:simple", "phaeosols_hapliques:undecided"),
    grisols_eluviques = character(0),
    grisols_degrades = character(0),
    grisols_hapliques = character(0),
    gypsosols_hapliques = character(0),
    gypsosols_petrogypsiques = character(0),
    histosols_fibriques = character(0),
    histosols_mesiques = character(0),
    histosols_sapriques = character(0),
    histosols_composites = character(0),
    histosols_leptiques = character(0),
    leptismectisols = character(0),
    lithosols = character(0),
    neoluvisols = character(0),
    luvisols_typiques = character(0),
    luvisols_degrades = c("redoxisols:imperfect"),
    luvisols_derniques = c("luvisols_degrades:simple"),
    luvisols_tronques = character(0),
    quasi_luvisols = character(0),
    nitosols = c("brunisols_eutriques:undecided", "brunisols_dystriques:undecided"),
    organosols_holorganiques = character(0),
    organosols_calcaires = c("rendosols:simple"),
    organosols_satures = character(0),
    organosols_insatures = character(0),
    pelosols_typiques = character(0),
    pelosols_brunifies = character(0),
    pelosols_differencies = character(0),
    peyrosols = character(0),
    phaeosols_hapliques = c("brunisols_eutriques:simple"),
    phaeosols_melanoluviques = character(0),
    planosols_typiques = c("redoxisols:simple"),
    planosols_distaux = c("arenosols:undecided"),
    planosols_structuraux = character(0),
    podzosols_ocriques = character(0),
    podzosols_meubles = character(0),
    podzosols_duriques = character(0),
    podzosols_humiques = character(0),
    podzosols_humo_duriques = character(0),
    podzosols_eluviques = character(0),
    podzosols_placiques = character(0),
    post_podzosols = c("podzosols_ocriques:simple"),
    rankosols = c("arenosols:undecided"),
    reductisols_typiques = character(0),
    reductisols_stagniques = character(0),
    redoxisols = character(0),
    regosols = character(0),
    salisols_chloruro_sulfates = c("brunisols_eutriques:undecided", "brunisols_dystriques:undecided"),
    salisols_carbonates = c("brunisols_eutriques:undecided", "brunisols_dystriques:undecided"),
    sodisols_indifferencies = c("brunisols_eutriques:undecided", "brunisols_dystriques:undecided"),
    sodisols_solonetziques = c("luvisols_typiques:simple", "luvisols_tronques:imperfect"),
    sodisols_solodises = c("luvisols_typiques:simple", "luvisols_tronques:imperfect"),
    sodisalisols = c("brunisols_eutriques:undecided", "brunisols_dystriques:undecided"),
    salisodisols = c("brunisols_eutriques:undecided", "brunisols_dystriques:undecided"),
    thalassosols_bruts = c("regosols:simple"),
    thalassosols_juveniles = c("regosols:simple"),
    thalassosols_polderises = character(0),
    thiosols = character(0),
    sulfatosols = character(0),
    veracrisols = c("luvisols_tronques:simple"),
    lithovertisols = character(0),
    topovertisols = character(0),
    paravertisols_hapliques = character(0),
    paravertisols_planosoliques = character(0) 
  )
  refs <- names(.rp_rules()$ref_by_code)
  expect_identical(names(expected), refs)
  for (cd in refs) {
    r <- .rp52_cl(.rp52_ex(cd))
    expect_identical(r$levels$code, cd, info = cd)
    expect_identical(r$levels$attachment, "simple", info = cd)
    expect_identical(r$provisional_because, character(0), info = cd)
    c <- r$candidates[r$candidates$code != cd, ]
    expect_identical(if (nrow(c)) paste0(c$code, ":", c$degree) else character(0), expected[[cd]], info = cd)
  }
})
