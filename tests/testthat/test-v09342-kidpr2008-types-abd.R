# v0.9.342: the types of trunks A, B and D of the Russian soil classification
# (Field Guide of 2008, chapter 4: AA, AB, BA, BB, BC and [DA]), matched as
# the types of trunk C are (R/types-kidpr.R), and the name of the chemozems
# (CU), which have no types: their pollutants and the soil they were, the
# profile keyed again with the X horizon masked. Profiles are written in the
# Russian notation, except the undesignated twins, which go through the key
# to horizons on the data.

# --- builders ------------------------------------------------------------------------

.ab_ped <- function(tokens, site = list(), ...) {
  d <- sub(":[0-9.]+$", "", tokens)
  th <- as.numeric(sub("^.*:", "", tokens))
  bot <- cumsum(th)
  h <- data.frame(designation = d, top_cm = c(0, bot[-length(bot)]), bottom_cm = bot, stringsAsFactors = FALSE)
  h$designation[h$designation == "NA"] <- NA_character_
  extra <- list(...)
  for (k in names(extra)) h[[k]] <- extra[[k]]
  PedonRecord$new(site = utils::modifyList(list(id = "ru-type", designation_system = "kidpr"), site),
                  horizons = ensure_horizon_schema(data.table::as.data.table(h)))
}
.ab_class <- function(p, ...) classify_kidpr2008(p, on_missing = "silent", ...)
.ab_of <- function(tokens, ...) .ab_class(.ab_ped(tokens, ...))
.ab_type <- function(r) if (nrow(r$levels) >= 3L) r$levels$code[3L] else NA_character_
.ab_codes <- function(tokens, ...) paste(.ab_of(tokens, ...)$levels$code, collapse = "/")
.ab_fp <- list(landform = "floodplain")
.ab_counts <- c(AA = 5L, AB = 4L, BA = 18L, BB = 4L, BC = 10L, "[DA]" = 17L)
.ab_fixture <- function() {
  path <- testthat::test_path("fixtures", "kidpr2008-formulas.tsv")
  fx <- utils::read.delim(path, comment.char = "#", quote = "", stringsAsFactors = FALSE,
                          encoding = "UTF-8", colClasses = "character")
  fx <- fx[grepl("^ch4 (A[AB]|B[ABC]|DA)", fx$source), ]
  stats::setNames(fx$formula, sub("^ch4 ", "", fx$source))
}
# A formula as a profile in the Russian notation, with plausible thicknesses:
# a peat body of more than 50 cm in trunk A, the natural remnant thinner than
# the plough layer, the stratified body over 40 cm, the buried soil [ABC] a
# buried AU over BM, the buried profiles {ABC} of the volcanic soils a
# buried AO over BAN; the alternatives in parentheses (RY(RJ), CG~~ (CQ~~))
# left out.
.ab_th <- c(O = 5, W = 4, S = 10, TO = 40, TE = 40, TJ = 40, TT = 100, T = 30, H = 20, PT = 25, PTR = 25,
            AY = 15, AU = 30, AO = 8, P = 25, PU = 30, G = 30, Q = 30, F = 20, V = 30, ML = 30, BH = 15, BAN = 40,
            E = 10, RY = 50, RU = 50, RJ = 50, C = 40, D = 30, R = 30)
.ab_tokens <- function(formula) {
  out <- character(0)
  prev <- ""
  for (x in soilKey:::.kidpr_split_outside(gsub(" ", "", formula), "-")) {
    if (x == "[ABC]") { out <- c(out, "[AU]:20", "[BM]:30"); next }
    if (x == "{ABC}") { out <- c(out, "[AO]:5", "[BAN]:20"); next }
    x <- sub("\\((RJ|CQ~~)\\)$", "", x)
    m <- sub("^([A-Z]+).*$", "\\1", x)
    if (m %in% c("CG", "CQ")) m <- "C"
    t <- .ab_th[[m]]
    if (prev %in% c("PTR", "P", "PU") && m %in% c("AY", "AU", "T")) t <- 10
    if (prev %in% c("P", "PU") && m %in% c("RY", "RU")) t <- 30
    out <- c(out, sprintf("%s:%s", x, t))
    prev <- m
  }
  out
}

# Undesignated layers for the data route (designation_system unset), as in
# the tests of the key to horizons.
.ab_L <- function(top, bottom, ...) {
  d <- list(top_cm = top, bottom_cm = bottom, munsell_hue_dry = "10YR", munsell_value_dry = 6,
            munsell_chroma_dry = 4, structure_type = "massive", structure_grade = "structureless",
            clay_pct = 20, silt_pct = 40, sand_pct = 40, oc_pct = 0.1, ph_h2o = 6.0,
            clay_films_amount = "none", effervescence_class = "none", secondary_carbonates_form = "none")
  a <- list(...)
  d[names(a)] <- a
  d
}
.ab_P <- function(..., site = list()) {
  s <- list(frost_regime = "none", land_use = "forest")
  s[names(site)] <- site
  ls <- list(...)
  nm <- unique(unlist(lapply(ls, names)))
  h <- as.data.frame(lapply(stats::setNames(nm, nm), function(k)
    unlist(lapply(ls, function(l) if (is.null(l[[k]])) NA else l[[k]]))), stringsAsFactors = FALSE)
  old <- options(soilKey.warn_unrecognised = FALSE)
  on.exit(options(old))
  PedonRecord$new(site = s, horizons = h)
}
.ab_org <- function(top, bottom, ...) .ab_L(top, bottom, munsell_hue_dry = "7.5YR", munsell_value_dry = 3, munsell_chroma_dry = 3,
                                            plant_residues_pct = 80, peat_decomposition_pct = 30, oc_pct = 40, ...)
.ab_AY <- function(top, bottom, ...) .ab_L(top, bottom, munsell_value_dry = 5, munsell_chroma_dry = 2,
                                           structure_type = "granular", structure_grade = "moderate", ph_h2o = 5.0,
                                           oc_pct = 2, clay_pct = 15, silt_pct = 50, sand_pct = 35, ...)
.ab_W <- function() .ab_L(0, 3, munsell_value_dry = 5, munsell_chroma_dry = 3, roots_abundance = "common")
.ab_und <- function(r, cands, div) {
  expect_identical(r$levels$code[2L], div)
  expect_identical(nrow(r$levels), 2L)
  expect_true(all(paste0("types:", cands) %in% r$provisional_because))
  w <- grep("The type is undecided", r$warnings, value = TRUE)
  expect_length(w, 1L)
  for (k in cands) expect_match(w, k, fixed = TRUE)
}
.ab_q15 <- function(r, div, near = character(0)) {
  expect_identical(r$levels$code[2L], div)
  expect_identical(nrow(r$levels), 2L)
  expect_length(r$provisional_because, 0L)
  w <- grep("Combination not provided by the Field Guide of 2008", r$warnings, value = TRUE)
  expect_length(w, 1L)
  expect_true(all(near %in% names(r$trace$types_nearest)))
}


# --- the rules --------------------------------------------------------------------------

test_that("the 58 types of trunks A, B and D are clean and as the fixture writes them", {
  skip_on_cran()
  r <- load_rules("kidpr2008")
  v <- .validate_rules("kidpr2008")
  expect_length(v$missing, 0L)
  expect_length(v$malformed, 0L)
  expect_length(v$codes, 0L)
  expect_true(all(names(.ab_counts) %in% names(r$types)))
  expect_identical(lengths(r$types)[names(.ab_counts)], .ab_counts)
  expect_identical(sum(.ab_counts), 58L)
  expect_identical(sum(lengths(r$types)), 275L)
  # the chemozems have no types of their own
  expect_false("CU" %in% names(r$types))
  fx <- .ab_fixture()
  expect_length(fx, 58L)
  agro <- c("P", "PU", "PT", "PTR")
  for (d in names(.ab_counts)) {
    l <- r$types[[d]]
    code <- vapply(l, `[[`, "", "code")
    expect_identical(code, sprintf("[%s%02d]", gsub("[][]", "", d), seq_along(l)))
    expect_false(is.unsorted(vapply(l, function(e) sub("-.*$", "", e$formula) %in% agro, NA)), info = d)
    for (e in l) {
      k <- gsub("[][]", "", e$code)
      expect_identical(e$formula, fx[[k]], info = k)
      expect_true(nzchar(e$name) && !grepl("[\u0400-\u04ff]", e$name), info = k)
      expect_true(grepl("^[\u0400-\u04ff]", e$name_native), info = k)
      expect_true(is.numeric(e$page) && e$page >= 124 && e$page <= 133, info = k)
      expect_identical(e$tests$all_of[[1L]]$type_formula_kidpr$type, e$code)
      expect_length(e$optional, 0L)
    }
  }
  all <- unlist(unname(r$types[names(.ab_counts)]), recursive = FALSE)
  codes_with <- function(field) vapply(Filter(function(e) length(e[[field]]), all), `[[`, "", "code")
  # Q14: the rock of AA05, the buried soil of the stratozems, the substrate
  # of trunk D are tested; the C of the solonchaks and the marks of BA and BB
  # are not
  expect_identical(codes_with("test_terminals"), c("[AA05]", sprintf("[BC%02d]", c(4:6, 9:10)),
                                                   sprintf("[DA%02d]", setdiff(1:17, 11))))
  tt <- vapply(r$types[["[DA]"]][-11L], function(e) unlist(e$test_terminals), "")
  expect_identical(unname(tt), c("C=", "C=", "C..", "C..", rep("R", 6L), "C~~", "C\"", "C^^", "C~~", "C\"", "C^^"))
  # the BH of the ochric types is a BHF of either kind; the rock under the
  # dry peat is R or loose material C, which closes no other formula of AA
  # (Hugo, 2026-11-02)
  expect_identical(codes_with("near_identity"), c("[AA05]", "[BB01]", "[BB03]"))
  expect_identical(r$types$AA[[5L]]$near_identity, list(R = "C"))
  expect_identical(codes_with("ignore"), sprintf("[AA%02d]", 1:4))
  for (e in r$types$AA[1:4]) expect_identical(unlist(e$ignore), "C")
  # Hugo's decisions on the readings of this release (2026-11-02)
  rd <- yaml::read_yaml(system.file("rules", "kidpr2008", "readings.yaml", package = "soilKey"))$readings
  top <- vapply(rd, `[[`, "", "topic")
  mine <- c("The peat of the types of trunk A", "The substrate of the weakly developed soils",
            "The illuvial-humus BH of the ochric soils", "The name of a chemozem",
            "Profiles of trunks A, B and D the literal formulas leave without a type", "The rock under the dry peat")
  expect_true(all(mine %in% top))
  for (x in rd[top %in% mine]) expect_match(x$reading, "\\(Hugo, 2026-11-02\\)\\.$", info = x$topic)
})


# --- one profile per type -------------------------------------------------------------

test_that("every formula of trunks A, B and D, as a profile in the Russian notation, keys to its division and type", {
  skip_on_cran()
  fx <- .ab_fixture()
  r0 <- load_rules("kidpr2008")
  names_of <- unlist(lapply(unname(r0$types[names(.ab_counts)]), function(l)
    stats::setNames(vapply(l, `[[`, "", "name_native"), vapply(l, `[[`, "", "code"))))
  # the doubles: carbonate or gypsum rock names the type, R is its intergrade
  inter <- c(DA07 = "[DA05]", DA08 = "[DA06]", DA09 = "[DA05]", DA10 = "[DA06]")
  for (k in names(fx)) {
    d <- substr(k, 1L, 2L)
    tk <- .ab_tokens(fx[[k]])
    r <- .ab_of(tk, site = if (d == "BA") .ab_fp else list())
    info <- paste(k, paste(tk, collapse = " "))
    expect_identical(r$levels$code, c(substr(k, 1L, 1L), if (d == "DA") "[DA]" else d, sprintf("[%s]", k)), info = info)
    expect_length(r$provisional_because, 0L)
    expect_false(any(grepl("not provided|undecided", r$warnings)), info = info)
    ev <- r$trace$types[[sprintf("[%s]", k)]]$evidence[[1L]]$evidence
    expect_identical(as.character(ev$intergrades), if (k %in% names(inter)) inter[[k]] else character(0), info = info)
    expect_identical(r$levels$name_native[3L], names_of[[sprintf("[%s]", k)]], info = info)
    expect_identical(r$name, r$levels$name[3L])
  }
})

test_that("the example of each division of trunks A, B and D reaches its type", {
  skip_on_cran()
  ex <- c(AA = "A/AA/[AA01]", AB = "A/AB/[AB02]", BA = "B/BA/[BA02]", BB = "B/BB/[BB01]", BC = "B/BC/[BC02]",
          DA = "D/[DA]/[DA06]")
  for (d in names(ex)) {
    r <- .ab_class(make_kidpr2008_example(d))
    expect_identical(paste(r$levels$code, collapse = "/"), ex[[d]], info = d)
    expect_length(r$provisional_because, 0L)
  }
})


# --- Q14: what a formula tests ---------------------------------------------------------

test_that("Q14: the buried soil within 100 cm, the substrate of trunk D, the rock of the dry peat", {
  skip_on_cran()
  # the buried soil names the stratozems on a buried soil within 100 cm
  expect_identical(.ab_codes(c("RY:60", "[AU]:20", "[BM]:30")), "B/BC/[BC04]")
  expect_identical(.ab_codes(c("RY:60", "C:50", "[AU]:20")), "B/BC/[BC01]")
  expect_identical(.ab_codes(c("RU:60", "C:30", "[AU]:20")), "B/BC/[BC05]")
  expect_identical(.ab_codes(c("P:25", "RJ:30", "[AU]:20", "[BM]:30")), "B/BC/[BC09]")
  expect_identical(.ab_codes(c("P:25", "RJ:30", "D:30")), "B/BC/[BC07]")
  # the substrate and its mark name the types of trunk D
  for (k in list(c("C=", "[DA02]"), c("C..", "[DA04]"), c("C~~", "[DA15]"), c("C\"", "[DA16]"), c("C^^", "[DA17]"),
                 c("R", "[DA06]"), c("Rca", "[DA08]"), c("Rcs", "[DA10]")))
    expect_identical(.ab_codes(c("W:4", paste0(k[1L], ":40"))), paste0("D/[DA]/", k[2L]), info = k[1L])
  # the legend's aeolian mark is the ^^ of the diagrams (soilKey's reading)
  expect_identical(.ab_codes(c("O:5", "C\"\":40")), "D/[DA]/[DA14]")
  # the C of the solonchaks is not tested
  expect_identical(.ab_codes(c("S:10", "C:40")), "D/[DA]/[DA11]")
  # the rock under the dry peat is tested: R or loose material C (p. 43;
  # Hugo, 2026-11-02); with neither described, no type
  expect_identical(.ab_codes(c("TJ:40", "TT:30", "R:30")), "A/AA/[AA05]")
  r <- .ab_of(c("TJ:40", "TT:30", "C:30"))
  expect_identical(r$levels$code, c("A", "AA", "[AA05]"))
  expect_length(r$provisional_because, 0L)
  expect_identical(.ab_codes(c("TJ:40", "TT:30", "C~~:30")), "A/AA/[AA05]")
  .ab_q15(.ab_of(c("TJ:40", "TT:60")), "AA", "[AA05]")
  # loose rock under the peat closes no other formula
  expect_identical(.ab_codes(c("TO:40", "TT:100", "C:30")), "A/AA/[AA01]")
  expect_identical(.ab_codes(c("TE:40", "TT:100", "G:30", "C:30")), "A/AA/[AA04]")
  # the marks of the alluvial and volcanic parent material are not tested
  expect_identical(.ab_codes(c("AU:30", "C:40"), site = .ab_fp, layer_origin = c(NA, "alluvium")), "B/BA/[BA02]")
  expect_identical(.ab_codes(c("H:20", "BAN:40", "C:40")), "B/BB/[BB02]")
  expect_identical(.ab_codes(c("AO:8", "BH:15", "BAN:40", "C\":40")), "B/BB/[BB01]")
  # the buried humus horizons of the alluvial soils are not in the formula
  expect_identical(.ab_codes(c("AU:30", "C~~:20", "[AU]:15", "C~~:30"), site = .ab_fp), "B/BA/[BA02]")
})


# --- Q13: closed on the division ---------------------------------------------------------

test_that("Q13: a horizon of the division outside the formula blocks it; a foreign one is ignored", {
  skip_on_cran()
  # G, Q, V, F, ML in the alluvial formulas
  expect_identical(.ab_codes(c("AU:30", "G:30", "CG~~:30"), site = .ab_fp), "B/BA/[BA03]")
  expect_identical(.ab_codes(c("AU:30", "ML:30", "CQ~~:30"), site = .ab_fp), "B/BA/[BA09]")
  expect_identical(.ab_codes(c("PU:30", "AU:10", "V:30", "C~~:30"), site = .ab_fp), "B/BA/[BA16]")
  # a horizon foreign to the division is ignored: a BM in the alluvial soils
  expect_identical(.ab_codes(c("AU:30", "BM:30", "C~~:30"), site = .ab_fp), "B/BA/[BA02]")
  # E closes the ochric soils without E; G closes the peat soils without G
  expect_identical(.ab_codes(c("AO:8", "E:10", "BH:15", "BAN:40", "C\":40")), "B/BB/[BB03]")
  expect_identical(.ab_codes(c("TE:40", "TT:100", "G:30")), "A/AA/[AA04]")
  .ab_q15(.ab_of(c("TJ:40", "TT:30", "G:30")), "AA")
  # the buried soil closes the stratozems that do not write it
  expect_false(isTRUE(.ab_of(c("RY:60", "[AU]:20", "[BM]:30"))$trace$types[["[BC01]"]]$passed))
  # a natural remnant under the plough: written in the formula, or left out
  expect_identical(.ab_codes(c("PT:25", "T:15", "TT:100")), "A/AB/[AB01]")
  expect_identical(.ab_codes(c("P:25", "AY:10", "G:30", "CG~~:30"), site = .ab_fp), "B/BA/[BA15]")
})

test_that("agro before natural, most specific first, in the order classify_kidpr2008() walks", {
  skip_on_cran()
  r <- load_rules("kidpr2008")
  for (d in names(.ab_counts)) {
    sch <- .kidpr_ty_parse(r$types[[d]])
    sp <- vapply(sch$specs[sch$trial], function(s) as.numeric(s$specificity[1:2]), c(0, 0))
    ag <- sch$agro[sch$trial]
    expect_false(is.unsorted(-(sp[1L, ] * 100 + sp[2L, ])), info = d)
    key <- paste(sp[1L, ], sp[2L, ])
    for (s in unique(key)) expect_false(is.unsorted(-ag[key == s]), info = d)
  }
})


# --- Q15: no type --------------------------------------------------------------------------

test_that("Q15: a combination no formula provides for stops at the division, with the nearest formulas", {
  skip_on_cran()
  cases <- list(
    list(c("AO:8", "BAN:40", "C\":40"), list(), "BB", "[BB01]"),                        # no BH
    list(c("RU:50", "C~~:30"), .ab_fp, "BA", c("[BA01]", "[BA02]")),                    # RU on the floodplain
    list(c("AY:15", "Q:30", "CQ~~:30"), .ab_fp, "BA", "[BA04]"),                        # AY over Q
    list(c("H:60", "TT:60"), list(), "AA", "[AA01]"),                                   # muck over the peat
    list(c("O:5", "D:40"), list(), "[DA]", "[DA01]")                                    # D is not C
  )
  for (k in cases) .ab_q15(.ab_of(k[[1L]], site = k[[2L]]), k[[3L]], k[[4L]])
  # a proluvial body has no type (soilKey's reading)
  .ab_q15(.ab_of(c("W:4", "C:40"), layer_origin = c(NA, "proluvial fan")), "[DA]")
})


# --- Hugo's decisions on the readings of this release (2026-11-02) --------------------------

test_that("the peat of trunk A: TT below the upper 50 cm of the body; a T is TO or TE by its botany", {
  skip_on_cran()
  # one layer of TO across the line is TO above it and TT below
  r <- .ab_of(c("TO:120"))
  expect_identical(r$levels$code, c("A", "AA", "[AA01]"))
  expect_length(r$provisional_because, 0L)
  expect_identical(.ab_codes(c("TE:30", "TE:90", "G:20")), "A/AA/[AA04]")
  # under PT the peat T starts within 50 cm: TT below the line
  expect_identical(.ab_codes(c("PT:25", "T:100")), "A/AB/[AB01]")
  expect_identical(.ab_codes(c("PTR:25", "TE:100", "G:20")), "A/AB/[AB04]")
  # within the upper 50 cm the thicker peat names the type
  expect_identical(.ab_codes(c("TO:30", "TE:30", "TT:100")), "A/AA/[AA01]")
  # a T of the notation: TO or TE by the moor type, Sphagnum or the pH
  expect_identical(.ab_codes(c("T:60", "TT:60"), peat_moor_type = c("high-moor", NA)), "A/AA/[AA01]")
  expect_identical(.ab_codes(c("T:60", "TT:60"), peat_moor_type = c("low-moor", NA)), "A/AA/[AA03]")
  expect_identical(.ab_codes(c("T:60", "TT:60"), ph_h2o = c(6.8, NA)), "A/AA/[AA03]")
  r <- .ab_of(c("T:80", "G:30"))
  .ab_und(r, c("[AA02]", "[AA04]"), "AA")
  expect_true("peat_moor_type" %in% r$missing_data)
})

test_that("the substrate of trunk D: no horizon is loose rock, the mark from the data, the rock R on the data", {
  skip_on_cran()
  # a C whose mark is not written: from the origin, else the texture
  expect_identical(.ab_codes(c("W:4", "C:40"), texture_class_ru = c(NA, "loamy sand")), "D/[DA]/[DA04]")
  expect_identical(.ab_codes(c("W:4", "C:40"), phys_clay_pct = c(NA, 35)), "D/[DA]/[DA02]")
  expect_identical(.ab_codes(c("O:5", "C:40"), layer_origin = c(NA, "alluvium")), "D/[DA]/[DA12]")
  expect_identical(.ab_codes(c("O:5", "C:40"), layer_origin = c(NA, "volcanic ash")), "D/[DA]/[DA13]")
  expect_identical(.ab_codes(c("O:5", "C:40"), layer_origin = c(NA, "aeolian sand")), "D/[DA]/[DA14]")
  # loess is no stratified aeolian body: the texture names the type
  expect_identical(.ab_codes(c("O:5", "C:40"), layer_origin = c(NA, "loess"), phys_clay_pct = c(NA, 30)), "D/[DA]/[DA01]")
  # no texture recorded: pelozem or psammozem, undecided
  r <- .ab_of(c("W:4", "C:40"))
  .ab_und(r, c("[DA02]", "[DA04]"), "[DA]")
  expect_true("phys_clay_pct" %in% r$missing_data)
  # the data route: a W over a sand, a loam, an alluvial layer
  p <- function(...) .ab_P(.ab_W(), .ab_L(3, 50, designation = "C", ...))
  expect_identical(.ab_class(p(clay_pct = 3, silt_pct = 5, sand_pct = 92))$levels$code, c("D", "[DA]", "[DA04]"))
  expect_identical(.ab_class(p(clay_pct = 35, silt_pct = 40, sand_pct = 25))$levels$code, c("D", "[DA]", "[DA02]"))
  expect_identical(.ab_class(p(layer_origin = "alluvium"))$levels$code, c("D", "[DA]", "[DA15]"))
  # the rock R on the data: carbonate, gypsum, granite, not named
  O <- function() .ab_org(0, 5, peat_decomposition_pct = 20)
  rk <- function(...) .ab_class(.ab_P(O(), .ab_L(5, 40, designation = "R", ...)))
  expect_identical(rk(rock_origin = "limestone")$levels$code[3L], "[DA07]")
  expect_identical(rk(rock_origin = "gypsum")$levels$code[3L], "[DA09]")
  r <- rk(rock_origin = "granite")
  expect_identical(r$levels$code[3L], "[DA05]")
  expect_length(r$provisional_because, 0L)
  r <- rk()
  expect_identical(r$levels$code[3L], "[DA05]")
  expect_true("types:[DA09]" %in% r$provisional_because)
  expect_true("rock_origin" %in% r$missing_data)
})


# --- the data route -------------------------------------------------------------------------

test_that("undesignated twins reach their division and type through the key to horizons", {
  skip_on_cran()
  L <- .ab_L
  twins <- list(
    "A/AA/[AA01]" = .ab_P(.ab_org(0, 40, designation = "Oi", peat_moor_type = "high-moor"),
                          .ab_org(40, 150, designation = "Oe", peat_moor_type = "high-moor"), L(150, 180, designation = "C")),
    "A/AB/[AB01]" = .ab_P(.ab_org(0, 25, designation = "Op", boundary_distinctness = "abrupt", boundary_topography = "smooth"),
                          .ab_org(25, 150, peat_moor_type = "low-moor"), L(150, 180, designation = "C"),
                          site = list(land_use = "arable")),
    "B/BA/[BA02]" = .ab_P(.ab_AY(0, 30, designation = "Ah", munsell_value_dry = 3, munsell_chroma_dry = 1, ph_h2o = 6.8,
                                 oc_pct = 3.5, layer_origin = "alluvium"),
                          L(30, 70, designation = "C1", layer_origin = "alluvium", stratification_pattern = "continuous"),
                          L(70, 120, designation = "C2", layer_origin = "alluvium"), site = .ab_fp),
    "B/BB/[BB01]" = .ab_P(L(0, 15, designation = "Ah", plant_residues_pct = 20, oc_pct = 12, munsell_hue_dry = "7.5YR",
                            munsell_value_dry = 3, munsell_chroma_dry = 3),
                          L(15, 25, designation = "Bh", munsell_hue_dry = "7.5YR", munsell_value_dry = 3, munsell_chroma_dry = 3,
                            oc_pct = 4, uncoated_grains_pct = 30, clay_pct = 8, sand_pct = 70, silt_pct = 22, al_ox_pct = 1,
                            fe_ox_pct = 1),
                          L(25, 60, designation = "Bw", munsell_hue_dry = "7.5YR", munsell_value_dry = 6, munsell_chroma_dry = 8,
                            structure_type = "granular", structure_grade = "strong", uncoated_grains_pct = 40, al_ox_pct = 3,
                            fe_ox_pct = 2, bulk_density_g_cm3 = 0.7, phosphate_retention_pct = 90, ph_h2o = 5.6, oc_pct = 2),
                          L(60, 100, designation = "C", layer_origin = "volcanic ash")),
    "B/BC/[BC05]" = .ab_P(L(0, 60, layer_origin = "fluvic", munsell_value_dry = 3, munsell_chroma_dry = 1, oc_pct = 3),
                          .ab_AY(60, 80, designation = "Ab"), L(80, 110, designation = "C"), site = list(landform = "slope")),
    "D/[DA]/[DA04]" = .ab_P(.ab_W(), L(3, 50, designation = "C", clay_pct = 3, silt_pct = 5, sand_pct = 92))
  )
  for (k in names(twins)) {
    p <- twins[[k]]
    expect_null(p$site$designation_system)
    expect_true(all(identify_horizons(p, "kidpr2008")$route == "data"), info = k)
    r <- .ab_class(p)
    expect_identical(paste(r$levels$code, collapse = "/"), k, info = k)
    expect_length(r$provisional_because, 0L)
    expect_true(any(grepl("key 2.1 run on", r$warnings)), info = k)
  }
})


# --- the chemozems --------------------------------------------------------------------------

test_that("a chemozem is named by its pollutants and the soil it was, keyed with the X masked", {
  skip_on_cran()
  # the example: Cu, Ni, Co on a sod-podzolic soil (p. 134)
  r <- .ab_class(make_kidpr2008_example("CU"))
  expect_identical(r$levels$level, c("trunks", "divisions", "types"))
  expect_identical(r$levels$code, c("C", "CU", "[CU:CA04]"))
  expect_identical(r$name, "Chemozems polluted with Cu, Ni, Co; original soil: Sod-podzolic")
  expect_identical(r$levels$name_native[3L], paste0("\u0425\u0435\u043c\u043e\u0437\u0435\u043c\u044b, ",
    "\u0437\u0430\u0433\u0440\u044f\u0437\u043d\u0435\u043d\u043d\u044b\u0435 Cu, Ni, Co, ",
    "\u043f\u043e \u0438\u0441\u0445\u043e\u0434\u043d\u043e\u0439 \u043f\u043e\u0447\u0432\u0435: ",
    "\u0414\u0435\u0440\u043d\u043e\u0432\u043e-\u043f\u043e\u0434\u0437\u043e\u043b\u0438\u0441\u0442\u044b\u0435"))
  expect_length(r$provisional_because, 0L)
  expect_identical(paste(r$trace$chemozem$original$levels$code, collapse = "/"), "C/CA/[CA04]")
  expect_identical(r$trace$chemozem$pollutants, c("Cu", "Ni", "Co"))
  expect_true(any(grepl("^Chemozem .*C/CA/\\[CA04\\]", r$warnings)))
  # pollutants of several layers, split on commas and semicolons
  hz <- c("extremely hazardous", "extremely hazardous", NA, NA)
  r <- .ab_of(c("AY+X:10", "EL+X:15", "BT:40", "C:30"), contamination_hazard = hz, pollutants = c("oil products; Pb", "Pb, Cd", NA, NA))
  expect_identical(r$name, "Chemozems polluted with oil products, Pb, Cd; original soil: Sod-podzolic")
  # pollutants not recorded: named so, and asked
  r <- .ab_of(c("AY+X:15", "EL:15", "BT:40", "C:30"), contamination_hazard = c("extremely hazardous", NA, NA, NA))
  expect_identical(r$levels$code[3L], "[CU:CA04]")
  expect_identical(r$name, "Chemozems, pollutants not recorded; original soil: Sod-podzolic")
  expect_true("pollutants" %in% r$missing_data)
  expect_length(r$provisional_because, 0L)
  # the masked soil undecided at its type: the name provisional
  r <- .ab_of(c("O:10", "AY+X:10", "EL:15", "BT:40", "C:30"), pollutants = c(NA, "Cd", NA, NA, NA),
              contamination_hazard = c(NA, "extremely hazardous", NA, NA, NA))
  expect_identical(r$levels$code, c("C", "CU", "[CU:CA]"))
  expect_identical(r$name, "Chemozems polluted with Cd; original soil: Texture-differentiated soils, type undecided")
  expect_true(all(c("original_soil:types:[CA01]", "original_soil:types:[CA04]") %in% r$provisional_because))
  expect_true(any(grepl("^Original soil: The type is undecided", r$warnings)))
  # the masked soil has a division and no type, or no division
  r <- .ab_of(c("AU+X:30", "BEL:20", "BI:30", "C:30"), pollutants = c("Hg", NA, NA, NA))
  expect_identical(r$levels$code[3L], "[CU:C]")
  expect_true(any(grepl("^Original soil: This combination of horizons is not provided", r$warnings)))
  # the data route: extremely hazardous pollutants in the humus horizon
  p <- .ab_P(.ab_AY(0, 20, designation = "Ah", contamination_hazard = "extremely hazardous", pollutants = "Pb; Zn"),
             .ab_L(20, 80, designation = "C", contamination_hazard = NA, pollutants = NA),
             site = list(contaminated = TRUE))
  r <- .ab_class(p)
  expect_identical(r$levels$code, c("C", "CU", "[CU:CN01]"))
  expect_match(r$name, "^Chemozems polluted with Pb, Zn; original soil: ")
  # the masking does not touch the pedon
  expect_identical(p$horizons$contamination_hazard[1L], "extremely hazardous")
  expect_true(isTRUE(p$site$contaminated))
})
