# v0.9.334: the generic engine pieces for systems that name a type by its
# profile formula (the Russian Field Guide of 2008 first): the formula parser,
# match_horizon_sequence() and the scheme helpers, identify_horizons() and the
# key walker, and the additive level. Synthetic profiles; the formula strings
# are real ones of the Field Guide's chapter 4.

.pf <- function(s, ...) .parse_profile_formula(s, ...)
.slots <- function(s, ...) .formula_slots_table(.pf(s, ...))
.mhs <- match_horizon_sequence
# a table of horizons from codes and depths
.tab <- function(code, top, bottom, ...) {
  data.frame(code = code, top_cm = top, bottom_cm = bottom, ..., stringsAsFactors = FALSE)
}
.NAT <- c("O", "T", "H", "AO", "AY", "AU", "AH", "AJ", "AK", "W")
.AGRO <- c("P", "PU", "PB", "PT", "PTR")
.CA <- c(CA01 = "O-EL-BEL-BT-C", CA02 = "O-EL-BELg-BTg-G-CG", CA04 = "AY-EL-BEL-BT-C",
         CA05 = "AY-EL-BELg-BTg-G-CG", CA06 = "AY-AEL-BEL-BT-C", CA11 = "AY-BEL-BT-C")


# --- the parser ---------------------------------------------------------------

test_that("the parser reads slots, roles, optional horizons and typical features", {
  skip_on_cran()
  s <- .pf("O-EL-BELg-BTg-G-CG")
  expect_s3_class(s, "soilkey_formula")
  st <- .formula_slots_table(s)
  expect_identical(st$written, c("O", "EL", "BELg", "BTg", "G", "CG"))
  expect_identical(st$codes, c("O", "EL", "BEL", "BT", "G", "C"))
  expect_identical(st$role, c("surface", "middle", "middle", "middle", "middle", "parent"))
  expect_identical(st$tested, c(rep(TRUE, 5), FALSE))
  expect_identical(st$typical[3:4], c("g", "g"))
  expect_identical(st$required[3:4], c("", ""))
  expect_identical(s$slots[[6]]$forms[[1]]$rockflag, "G")
  expect_identical(unname(s$specificity), c(5, 0))
  st <- .slots("P-(EL)-BEL-BT-C")
  expect_identical(st$optional, c(FALSE, TRUE, FALSE, FALSE, FALSE))
  expect_identical(st$written[2], "EL")
  st <- .slots("(AY)-BM-C")
  expect_identical(st$role[1], "surface")
  expect_true(st$optional[1])
  # a code read as optional although written plainly (BEL where not diagnostic)
  expect_identical(.slots("O-EL-BEL-BT-C", optional = "BEL")$optional, c(FALSE, FALSE, TRUE, FALSE, FALSE))
  # a lithological prefix and sub-horizon digits
  f <- .pf("AY-2BM-BT2-C")$slots
  expect_identical(f[[2]]$forms[[1]]$lith, "2")
  expect_identical(f[[2]]$codes, "BM")
  expect_identical(f[[3]]$forms[[1]]$subdiv, "2")
  expect_identical(.pf("AY-IIBT-C")$slots[[2]]$forms[[1]]$lith, "II")
})

test_that("the parser reads alternatives: a master in parentheses, | and near-identity readings", {
  skip_on_cran()
  expect_identical(.slots("P-RY(RJ)-[ABC]")$codes[2], "RY|RJ")
  s <- .pf("AU-ML-CG~~ (CQ~~)")
  z <- s$slots[[3]]
  expect_length(z$forms, 2L)
  expect_identical(vapply(z$forms, function(f) f$rockflag, ""), c("G", "Q"))
  expect_identical(vapply(z$forms, function(f) f$mark, ""), c("~~", "~~"))
  expect_identical(vapply(z$forms, function(f) f$signature, ""), c("CG~~", "CQ~~"))
  expect_identical(.slots("O|T|H-G-CRM-C(g)")$codes[1], "O|T|H")
  st <- .slots("AJ-BCA-Cca", alternatives = list(BCA = "CAT"))
  expect_identical(st$codes[2], "BCA|CAT")
})

test_that("features: typical by default, required on tested terminals and when listed, optional in parentheses", {
  skip_on_cran()
  st <- .slots("AY-EL-BTg-BCAg-Gca(s)-CGca(s)")
  expect_identical(st$typical[5], "ca,s")
  expect_identical(st$required[5], "")
  st <- .slots("AY-EL-BTg-BCAg-Gca(s)-CGca(s)", required_features = "ca")
  expect_identical(st$required[5], "ca")
  expect_identical(st$typical[5], "s")
  st <- .slots("O-Rca", test_terminals = "R")
  expect_true(st$tested[2])
  expect_identical(st$required[2], "ca")
  st <- .slots("BPL-C(ca)", test_terminals = "C")
  expect_identical(st$required[2], "")
  expect_identical(st$typical[2], "ca")
  st <- .slots("AJ-SEL-BSN-BCAs,cs-Cca,s")
  expect_identical(st$typical[4:5], c("s,cs", "ca,s"))
})

test_that("buried profiles, turbated compositions, small horizons and mixtures", {
  skip_on_cran()
  s <- .pf("RY-[ABC]")
  expect_identical(s$slots[[2]]$kind, "buried")
  expect_identical(s$slots[[2]]$buried, "square")
  expect_identical(s$slots[[2]]$role, "parent")
  expect_false(s$slots[[2]]$tested)
  expect_true(.pf("RY-[ABC]", test_terminals = "[ABC]")$slots[[2]]$tested)
  s <- .pf("AO-BH-BAN-C\"-{ABC}-{ABC}", test_terminals = "[ABC]")
  expect_identical(vapply(s$slots[5:6], function(z) z$buried, ""), c("curly", "curly"))
  expect_identical(vapply(s$slots[5:6], function(z) z$tested, NA), c(FALSE, FALSE))
  expect_identical(s$slots[[4]]$forms[[1]]$mark, "\"")
  s <- .pf("TUR[AU+SEL+ASN]-BCAs,cs-Cca,s")
  expect_identical(s$slots[[1]]$codes, "TUR")
  expect_identical(s$slots[[1]]$forms[[1]]$composition, c("AU", "SEL", "ASN"))
  expect_identical(s$slots[[1]]$role, "surface")
  expect_identical(.pf("PU-TUR[SEL+ASN]-BCAs,cs-Cca,s")$slots[[2]]$forms[[1]]$composition, c("SEL", "ASN"))
  st <- .slots("O-[e-hf]-EL-BEL-BT-C")
  expect_identical(st$kind[2], "small")
  expect_identical(st$role[2], "feature")
  expect_false(st$tested[2])
  expect_identical(.pf("O-[e-hf]-EL-BEL-BT-C")$slots[[2]]$small, "[e-hf]")
  expect_identical(.pf("O-e-BHF-C")$slots[[2]]$small, "e")
  expect_identical(st$role[3], "middle")
  m <- .pf("[AY, EL]tr-BT-C")$slots[[1]]
  expect_identical(m$kind, "mixture")
  expect_identical(m$composition, c("AY", "EL"))
  expect_identical(m$small, "tr")
})

test_that("rock marks, rock flags and the permafrost symbol", {
  skip_on_cran()
  mk <- function(s) .pf(s)$slots[[2]]$forms[[1]]$mark
  expect_identical(c(mk("O-C~~"), mk("W-C.."), mk("W-C="), mk("O-C^^"), mk("O-C\"\""), mk("O-C\"")),
                   c("~~", "..", "=", "^^", "\"\"", "\""))
  # typographic quotes and the Symbol-font perpendicular are folded
  expect_identical(mk("O-C\u201d"), "\"")
  expect_identical(.pf("O-CR-C\u22a5")$slots[[3]]$forms[[1]]$typical, "\u22a5")
  expect_identical(.pf("O-CR-C\uf05e")$slots[[3]]$forms[[1]]$typical, "\u22a5")
  expect_identical(.pf("O\u2013EL\u2013BHF\u2013C")$slots[[3]]$codes, "BHF")
  s <- .pf("AY-EL-DG", test_terminals = "DG")
  expect_identical(s$slots[[3]]$forms[[1]]$rockflag, "G")
  expect_true(s$slots[[3]]$tested)
  expect_false(.pf("AY-EL-D", test_terminals = "DG")$slots[[3]]$tested)
  # a terminal is flagged by its code, its signature or its text
  expect_true(.pf("O-C~~", test_terminals = "C~~")$slots[[2]]$tested)
  expect_true(.pf("O-C~~", test_terminals = "C")$slots[[2]]$tested)
  expect_false(.pf("O-C..", test_terminals = "C~~")$slots[[2]]$tested)
  expect_true(all(.slots("PTR-T-G-CG~~", test_terminals = TRUE)$tested))
  # CQca,s: C with the rock flag Q and the features ca and s
  f <- .pf("AU-Q-CQca,s")$slots[[3]]$forms[[1]]
  expect_identical(c(f$code, f$rockflag), c("C", "Q"))
  expect_identical(f$typical, c("ca", "s"))
})

test_that("an unknown token stops the parser with its name, when a vocabulary is given", {
  skip_on_cran()
  ma <- c("O", "AY", "AU", "EL", "BEL", "BT", "C", "G", "BCA", "TUR", "SEL", "ASN")
  expect_error(.pf("O-XYZ-C", masters = ma), "Unknown horizon 'XYZ'")
  expect_error(.pf("O-BTG-C", masters = ma), "Unknown horizon 'BTG'")
  expect_error(.pf("TUR[AU+SXL]-C", masters = ma), "Unknown horizon 'SXL'")
  expect_error(.pf("O-ELq-C", masters = ma, features = c("g", "f")), "Unknown feature 'q'")
  expect_error(.pf("O-[e-hx]-EL-C", masters = ma, features = c("e", "hf")), "Unknown feature 'hx'")
  expect_error(.pf("O-EL-#-C"), "Unknown token '#'")
  expect_error(.pf("O--C"), "Empty item")
  expect_error(.pf("O-(EL-C"), "Unbalanced brackets")
  expect_error(.pf(""), "one non-empty string")
  # with a feature vocabulary a run written without commas is split, longest first
  f <- .pf("AU-BCA-Ccas", masters = ma, features = c("ca", "cs", "s"))$slots[[3]]$forms[[1]]
  expect_identical(f$typical, c("ca", "s"))
  expect_silent(.pf("O-EL-BELg-BTg-G-CG", masters = ma, features = "g"))
})


# --- the matcher --------------------------------------------------------------

test_that("the matcher reads the sequence in order, with optional slots and alternatives", {
  skip_on_cran()
  expect_true(.mhs(c("AY", "EL", "BEL", "BT", "C"), "AY-EL-BEL-BT-C")$passed)
  r <- .mhs(c("AY", "EL", "BEL", "BT", "C"), "AY-AEL-BEL-BT-C")
  expect_false(r$passed)
  expect_identical(r$distance, 1)
  r <- .mhs(c("P", "BEL", "BT", "C"), "P-(EL)-BEL-BT-C")
  expect_true(r$passed)
  expect_identical(r$absent, "EL")
  expect_true(.mhs(c("P", "EL", "BEL", "BT", "C"), "P-(EL)-BEL-BT-C")$passed)
  r <- .mhs(c("P", "BT", "C"), "P-(EL)-BEL-BT-C")
  expect_false(r$passed)
  expect_identical(r$missing, "BEL")
  # out of order
  expect_false(.mhs(c("AY", "BT", "EL", "C"), "AY-EL-BT-C")$passed)
  # alternatives
  expect_true(.mhs(c("P", "RJ", "D"), "P-RY(RJ)-D")$passed)
  expect_true(.mhs(c("H", "G", "CRM", "C"), "O|T|H-G-CRM-C(g)")$passed)
  expect_true(.mhs(c("AJ", "CAT", "C"), "AJ-BCA-Cca", alternatives = list(BCA = "CAT"))$passed)
  expect_false(.mhs(c("AJ", "CAT", "C"), "AJ-BCA-Cca")$passed)
  # a parsed formula is accepted as it is
  expect_true(.mhs(c("AY", "BM", "C"), .pf("(AY)-BM-C"))$passed)
  expect_true(.mhs(c("BM", "C"), "(AY)-BM-C")$passed)
  # the evidence: slots matched
  r <- .mhs(.tab(c("AY", "EL", "BT", "C"), c(0, 20, 35, 90), c(20, 35, 90, 130)), "AY-EL-BTg-C")
  expect_identical(r$matched$written, c("AY", "EL", "BTg"))
  expect_identical(r$matched$top_cm, c(0, 20, 35))
  expect_match(r$notes, "typical feature\\(s\\) g of BTg", all = FALSE)
})

test_that("sub-horizons merge; small horizons and transitional rows drop", {
  skip_on_cran()
  p <- .tab(c("AY", "EL", "BEL", "BT", "BT", "C"), c(0, 15, 30, 45, 70, 120), c(15, 30, 45, 70, 120, 150),
            designation = c("AY", "EL", "BEL", "BT1", "BT2", "C"))
  r <- .mhs(p, "AY-EL-BEL-BT-C")
  expect_true(r$passed)
  expect_identical(r$matched$horizon[4], "BT (4-5)")
  expect_identical(r$matched$bottom_cm[4], 120)
  p <- .tab(c("O", NA, "BHF", "C"), c(0, 5, 6, 40), c(5, 6, 40, 90), small = c(FALSE, TRUE, FALSE, FALSE))
  expect_true(.mhs(p, "O-BHF-C")$passed)
  p <- .tab(c("AY", "EL", NA, "BT", "C"), c(0, 15, 30, 40, 90), c(15, 30, 40, 90, 120),
            transitional = c(FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_true(.mhs(p, "AY-EL-BT-C")$passed)
  # overlays become features of their row
  p <- .tab(c("AU", "G", "C"), c(0, 30, 80), c(30, 80, 120))
  p$overlay <- list(character(0), "S", character(0))
  expect_true(.mhs(p, "AU-G-C", required_features = "S")$passed)
})

test_that("an undecided layer gives NA when it could change the match, and only then", {
  skip_on_cran()
  r <- .mhs(c("AY", NA, "BT", "C"), "AY-EL-BT-C")
  expect_identical(r$passed, NA)
  expect_identical(r$undecided, "2 (any)")
  # candidates: EL or BEL, closed on both
  p <- data.frame(code = c("AY", NA, "BT", "C"), stringsAsFactors = FALSE)
  p$candidates <- list(character(0), c("EL", "BEL"), character(0), character(0))
  expect_identical(.mhs(p, "AY-EL-BT-C", vocabulary = c("AY", "EL", "BEL", "BT"))$passed, NA)
  # every candidate gives the same answer: decided
  p <- data.frame(code = c("AY", "EL", "BEL", NA, "BT", "C"), stringsAsFactors = FALSE)
  p$candidates <- list(character(0), character(0), character(0), c("BEL", "BT"), character(0), character(0))
  expect_true(.mhs(p, "AY-EL-BEL-BT-C")$passed)
  p <- data.frame(code = c("AY", "EL", "BT", NA), stringsAsFactors = FALSE)
  p$candidates <- list(character(0), character(0), character(0), c("C", NA))
  r <- .mhs(p, "AY-EL-BT-C")
  expect_true(r$passed)
  expect_identical(r$undecided, character(0))
  # no completion can match: FALSE, without trying them all
  r <- .mhs(c("AY", NA, NA, "C"), "O-EL-BEL-BT-C", vocabulary = c("O", "AY", "EL", "BEL", "BT"))
  expect_false(r$passed)
  expect_match(r$notes, "no completion", all = FALSE)
  expect_true(r$distance >= 1)
  # a required feature left undecided
  p <- .tab(c("O", "R"), c(0, 10), c(10, 40))
  p$features_na <- list(character(0), "ca")
  expect_identical(.mhs(p, "O-Rca", test_terminals = "R")$passed, NA)
  # a layer with no diagnostic horizon ("") is not undecided
  expect_true(.mhs(c("AY", "BT", ""), "AY-BT-C")$passed)
})

test_that("closed on a vocabulary: its horizons must be matched, foreign ones are ignored", {
  skip_on_cran()
  voc <- .formula_vocabulary(lapply(.CA, .pf))
  expect_setequal(voc, c("O", "EL", "BEL", "BT", "G", "AY", "AEL"))
  p <- c("O", "EL", "BEL", "BT", "G", "C")
  r <- .mhs(p, .CA[["CA01"]], vocabulary = voc)
  expect_false(r$passed)
  expect_identical(r$extra, "G (5)")
  # closed on its own codes only, the G is ignored
  r <- .mhs(p, .CA[["CA01"]])
  expect_true(r$passed)
  expect_identical(r$ignored, c("G (5)", "C (6)"))
  # a horizon foreign to the division (V) is ignored
  r <- .mhs(c("O", "EL", "BEL", "V", "BT", "C"), .CA[["CA01"]], vocabulary = voc)
  expect_true(r$passed)
  expect_identical(r$ignored, c("V (4)", "C (6)"))
  expect_true(.mhs(p, .CA[["CA02"]], vocabulary = voc)$passed)
})

test_that("directly under: transitional rows skipped, a gap only within its allowance", {
  skip_on_cran()
  p <- .tab(c("AU", NA, "BCA", "C"), c(0, 40, 52, 90), c(40, 52, 90, 130), passed = c(TRUE, FALSE, TRUE, TRUE))
  r <- .mhs(p, "AU-BCA-Cca")
  expect_false(r$passed)
  expect_match(r$notes, "across 12 cm", all = FALSE)
  expect_true(.mhs(p, "AU-BCA-Cca", pair_gaps_cm = c("AU>BCA" = 15))$passed)
  expect_true(.mhs(p, "AU-BCA-Cca", max_gap_cm = 15)$passed)
  expect_false(.mhs(p, "AU-BCA-Cca", max_gap_cm = 10)$passed)
  p2 <- .tab(c("AU", NA, "BCA", "C"), c(0, 40, 60, 90), c(40, 60, 90, 130), passed = c(TRUE, FALSE, TRUE, TRUE))
  expect_false(.mhs(p2, "AU-BCA-Cca", pair_gaps_cm = c("AU>BCA" = 15))$passed)
  # a transition between them is not a gap
  p3 <- .tab(c("AU", NA, "BCA", "C"), c(0, 40, 60, 90), c(40, 60, 90, 130),
             transitional = c(FALSE, TRUE, FALSE, FALSE))
  expect_true(.mhs(p3, "AU-BCA-Cca")$passed)
  # a gap of unknown thickness, allowed some: undecided
  p4 <- .tab(c("AU", NA, "BCA", "C"), c(0, 40, NA, 90), c(40, NA, 90, 130), passed = c(TRUE, FALSE, TRUE, TRUE))
  expect_identical(.mhs(p4, "AU-BCA-Cca", pair_gaps_cm = c("AU>BCA" = 15))$passed, NA)
})

test_that("terminal symbols are tested only when flagged", {
  skip_on_cran()
  p <- .tab(c("O", "C"), c(0, 10), c(10, 100))
  expect_true(.mhs(p, "O-C~~")$passed)
  expect_false(.mhs(p, "O-C~~", test_terminals = "C~~")$passed)
  p$mark <- c(NA, "~~")
  expect_true(.mhs(p, "O-C~~", test_terminals = "C~~")$passed)
  expect_false(.mhs(p, "O-C..", test_terminals = "C..")$passed)
  # hard rock under loose rock: R tested, the (C) left out
  expect_true(.mhs(c("AY", "C", "R"), "AY-(C)-R", test_terminals = "R")$passed)
  expect_false(.mhs(c("AY", "C"), "AY-(C)-R", test_terminals = "R")$passed)
  expect_true(.mhs(c("AY", "C"), "AY-(C)-R")$passed)
  # a feature that names the type
  p <- .tab(c("O", "R"), c(0, 10), c(10, 40))
  expect_false(.mhs(p, "O-Rca", test_terminals = "R")$passed)
  p$features <- list(character(0), "ca")
  expect_true(.mhs(p, "O-Rca", test_terminals = "R")$passed)
  # a gleyed substrate (DG) against a plain one
  p <- .tab(c("AY", "EL", "D"), c(0, 20, 40), c(20, 40, 100))
  expect_false(.mhs(p, "AY-EL-DG", test_terminals = "DG")$passed)
  p$rockflag <- c(NA, NA, "G")
  expect_true(.mhs(p, "AY-EL-DG", test_terminals = "DG")$passed)
  expect_true(.mhs(p, "AY-EL-D")$passed)
})

test_that("the surface slot: the agro horizon, else the thickest natural one", {
  skip_on_cran()
  p <- .tab(c("O", "AY", "EL", "BEL", "BT", "C"), c(0, 5, 25, 40, 60, 120), c(5, 25, 40, 60, 120, 150))
  r <- .mhs(p, "AY-EL-BEL-BT-C", surface = .NAT, agro = .AGRO)
  expect_true(r$passed)
  expect_identical(r$surface, "AY")
  expect_identical(r$absorbed, "O (1)")
  expect_false(.mhs(p, "O-EL-BEL-BT-C", surface = .NAT, agro = .AGRO)$passed)
  # without the rule the O is the first horizon
  expect_false(.mhs(p, "AY-EL-BEL-BT-C", vocabulary = c("O", "AY", "EL", "BEL", "BT"))$passed)
  p$bottom_cm[1] <- 30; p$top_cm[2] <- 30; p$bottom_cm[2] <- 35; p$top_cm[3] <- 35
  expect_true(.mhs(p, "O-EL-BEL-BT-C", surface = .NAT, agro = .AGRO)$passed)
  # equal thicknesses: undecided where the choice matters
  p <- .tab(c("O", "AY", "EL", "BT", "C"), c(0, 10, 20, 40, 90), c(10, 20, 40, 90, 120))
  r <- .mhs(p, "AY-EL-BT-C", surface = .NAT, agro = .AGRO)
  expect_identical(r$passed, NA)
  expect_match(r$notes, "thickest surface horizon", all = FALSE)
  # an agro horizon is the surface whatever its thickness; the natural horizon
  # under it stays as a remnant
  p <- .tab(c("P", "AY", "AEL", "BEL", "BT", "C"), c(0, 20, 50, 60, 75, 120), c(20, 50, 60, 75, 120, 150))
  r <- .mhs(p, "P-(AY)-AEL-BEL-BT-C", surface = .NAT, agro = .AGRO)
  expect_true(r$passed)
  expect_identical(r$surface, "P")
  # a surface horizon foreign to the formula is not ignored
  p <- .tab(c("AK", "BM", "C"), c(0, 20, 60), c(20, 60, 100))
  expect_true(.mhs(p, "(AY)-BM-C")$passed)
  expect_false(.mhs(p, "(AY)-BM-C", surface = .NAT, agro = .AGRO)$passed)
})

test_that("buried profiles and turbated compositions", {
  skip_on_cran()
  p <- .tab(c("RY", "AU", "BT", "C"), c(0, 45, 70, 110), c(45, 70, 110, 150), buried = c(0, 1, 1, 1))
  expect_true(.mhs(p, "RY-[ABC]", test_terminals = "[ABC]")$passed)
  expect_true(.mhs(p, "RY-C")$passed)
  expect_false(.mhs(p, "RY-C", vocabulary = c("RY", "[ABC]"))$passed)
  expect_false(.mhs(p, "RY-[ABC]", test_terminals = "[ABC]", buried_within_cm = 40)$passed)
  expect_true(.mhs(p, "RY-C", vocabulary = c("RY", "[ABC]"), buried_within_cm = 40)$passed)
  sch <- c(BC01 = "RY-D", BC04 = "RY-[ABC]")
  x <- .match_schemes(p, sch, parse_args = list(test_terminals = "[ABC]"), buried_within_cm = 100)
  expect_identical(x$code, "BC04")
  p <- .tab(c("PU", "TUR", "BCA", "C"), c(0, 25, 50, 90), c(25, 50, 90, 130))
  expect_identical(.mhs(p, "PU-TUR[SEL+ASN]-BCAs,cs-Cca,s")$passed, NA)
  expect_true(.mhs(p, "PU-TUR-BCA-Cca")$passed)
  p$composition <- list(character(0), c("SEL", "ASN"), character(0), character(0))
  expect_true(.mhs(p, "PU-TUR[SEL+ASN]-BCAs,cs-Cca,s")$passed)
  expect_false(.mhs(p, "P-TUR[SEL+BSN]-BCAs,cs-Cca,s")$passed)
})


# --- several schemes ----------------------------------------------------------

test_that("the nearest schemes by edit distance when none matches", {
  skip_on_cran()
  p <- .tab(c("AY", "AEL", "BT", "C"), c(0, 20, 35, 90), c(20, 35, 90, 130))
  x <- .match_schemes(p, .CA, surface = .NAT, agro = .AGRO)
  expect_false(x$passed)
  expect_identical(x$code, NA_character_)
  expect_identical(x$nearest$code[1], "CA06")
  expect_identical(x$nearest$distance[1], 1)
  expect_identical(x$nearest$missing[1], "BEL")
  expect_identical(nrow(x$nearest), 3L)
  expect_true(all(diff(x$nearest$distance) >= 0))
  expect_identical(.nearest_schemes(x$results, 1L)$code, "CA06")
})

test_that("the most specific of several matches wins; the other is an intergrade", {
  skip_on_cran()
  p <- .tab(c("O", "R"), c(0, 10), c(10, 30))
  p$features <- list(character(0), "ca")
  x <- .match_schemes(p, list(R = "O-R", Rca = "O-Rca", Rcs = "O-Rcs"), parse_args = list(test_terminals = "R"))
  expect_identical(x$code, "Rca")
  expect_identical(x$intergrades, "R")
  expect_null(x$nearest)
  p <- .tab(c("AY", "EL", "BEL", "BT", "G", "C"), c(0, 20, 35, 50, 90, 130), c(20, 35, 50, 90, 130, 160))
  x <- .match_schemes(p, .CA, surface = .NAT, agro = .AGRO)
  expect_identical(x$code, "CA05")
  expect_identical(x$intergrades, character(0))
  # the picker: a more specific scheme left undecided makes the choice provisional
  res <- list(A = list(passed = NA, specificity = c(slots = 5, details = 0)),
              B = list(passed = TRUE, specificity = c(slots = 4, details = 0)),
              C = list(passed = TRUE, specificity = c(slots = 3, details = 0)),
              D = list(passed = NA, specificity = c(slots = 2, details = 0)))
  pk <- .pick_scheme(res)
  expect_identical(pk$code, "B")
  expect_identical(pk$provisional_because, "A")
  expect_identical(pk$intergrades, "C")
  # equal specificity: the list order (agro before natural) decides
  res <- list(PB = list(passed = TRUE, specificity = c(3, 0)), AU = list(passed = TRUE, specificity = c(3, 0)))
  expect_identical(.pick_scheme(res)$code, "PB")
  pk <- .pick_scheme(list(A = list(passed = FALSE, specificity = c(3, 0)), B = list(passed = NA, specificity = c(2, 0))))
  expect_identical(pk$passed, NA)
  expect_identical(pk$candidates, "B")
})


# --- identify_horizons() and the key walker -----------------------------------

test_that("identify_horizons() dispatches to the system's identification, or says it has none", {
  skip_on_cran()
  p <- make_hscs2024_example("steppe")
  expect_error(identify_horizons(p, "hscs2024"), "has no horizon identification")
  expect_error(identify_horizons(list(), "hscs2024"), "PedonRecord")
  fr <- .horizon_frame(p)
  expect_identical(nrow(fr), nrow(p$horizons))
  expect_true(all(c("horizon_idx", "code", "overlay", "features", "small", "route", "passed",
                    "evidence", "missing", "candidates") %in% names(fr)))
  expect_true(is.list(fr$candidates) && is.list(fr$overlay))
  local_mocked_bindings(.horizon_identifier = function(ns) {
    if (!identical(ns, "hscs")) return(NULL)
    function(pedon, ...) {
      f <- .horizon_frame(pedon)
      f$code <- c("AU", NA, "BCA", "C")
      f$passed <- c(TRUE, NA, TRUE, TRUE)
      f$candidates[[2]] <- c("AU", "BCA")
      f$route <- "data"
      f
    }
  })
  h <- identify_horizons(p, "hscs")
  expect_identical(h$code, c("AU", NA, "BCA", "C"))
  expect_identical(h$candidates[[2]], c("AU", "BCA"))
  # the frame feeds the matcher: both candidates merge with a neighbour
  expect_true(match_horizon_sequence(h, "AU-BCA-Cca")$passed)
  expect_error(identify_horizons(p, "cpcs1967"), "has no horizon identification")
})

test_that("the key walker gives one outcome when the answers decide it", {
  skip_on_cran()
  tree <- list(
    k1 = list(test = function(l, ctx) l$organic, yes = "O", no = "k2"),
    k2 = list(test = function(l, ctx) l$clay_ratio >= 1.4, yes = "k3", no = "BM"),
    k3 = list(test = function(l, ctx) ctx$eluvial_above, yes = "BT", no = "BI"))
  w <- .walk_key(tree, list(organic = FALSE, clay_ratio = 1.6), context = list(eluvial_above = TRUE))
  expect_identical(w$outcome, "BT")
  expect_true(w$passed)
  expect_identical(w$path, c("k1:no", "k2:yes", "k3:yes"))
  expect_identical(w$candidates, "BT")
  expect_false(w$dead_end)
  expect_identical(.walk_key(tree, list(organic = TRUE))$outcome, "O")
  # explicit outcomes and a multi-way position
  tree2 <- list(
    a = list(branches = list(
      list(test = function(l, ctx) l$x > 10, to = list(outcome = "a"), label = "1"),
      list(test = function(l, ctx) l$x > 5, to = "b", label = "2"),
      list(test = TRUE, to = list(outcome = "c"), label = "3"))),
    b = list(test = function(l, ctx) l$y, yes = "B1", no = "B2"))
  expect_identical(.walk_key(tree2, list(x = 7, y = FALSE))$outcome, "B2")
  expect_identical(.walk_key(tree2, list(x = 2, y = FALSE))$path, "a:3")
})

test_that("an undecided question follows every branch and leaves the candidates", {
  skip_on_cran()
  tree <- list(
    k1 = list(test = function(l, ctx) l$organic, yes = "O", no = "k2"),
    k2 = list(test = function(l, ctx) l$clay_ratio >= 1.4, yes = "k3", no = "BM"),
    k3 = list(test = function(l, ctx) list(passed = l$films, missing = "clay_films_amount"), yes = "BT", no = "BI"))
  w <- .walk_key(tree, list(organic = FALSE, clay_ratio = 1.6, films = NA))
  expect_identical(w$outcome, NA_character_)
  expect_identical(w$passed, NA)
  expect_identical(w$candidates, c("BT", "BI"))
  expect_identical(w$undecided, "k3")
  expect_identical(w$missing, "clay_films_amount")
  expect_identical(w$paths$BI, c("k1:no", "k2:yes", "k3:no"))
  w <- .walk_key(tree, list(organic = FALSE, clay_ratio = NA, films = NA))
  expect_identical(w$candidates, c("BT", "BI", "BM"))
  # both branches of an undecided question end at the same horizon: decided
  tree3 <- list(q = list(test = function(l, ctx) NA, yes = "BT", no = "r"),
                r = list(test = function(l, ctx) TRUE, yes = "BT", no = "BI"))
  expect_identical(.walk_key(tree3, list())$outcome, "BT")
  # a position with no exit: undecided, a dead end
  tree4 <- list(q = list(branches = list(list(test = function(l, ctx) FALSE, to = "BT"),
                                         list(test = function(l, ctx) NA, to = "BI"))))
  w <- .walk_key(tree4, list())
  expect_true(w$dead_end)
  expect_identical(w$outcome, NA_character_)
  expect_identical(w$candidates, "BI")
  # an error in a test is an undecided answer, with its message
  tree5 <- list(q = list(test = function(l, ctx) stop("no data"), yes = "BT", no = "BI"))
  w <- .walk_key(tree5, list())
  expect_identical(w$candidates, c("BT", "BI"))
  expect_match(w$notes, "no data")
  expect_error(.walk_key(list(a = list(test = function(l, ctx) TRUE, yes = "a", no = "B")), list()),
               "loops back")
  expect_error(.walk_key(list(list(test = TRUE)), list()), "unique names")
})


# --- the additive level -------------------------------------------------------

.res <- function(type = "CA01") {
  ClassificationResult$new(
    system = "test", name = "Podzolic",
    levels = data.frame(level = c("divisions", "types"), code = c("CA", type),
                        name = c("Texture-differentiated", "Podzolic"), stringsAsFactors = FALSE),
    system_id = "test")
}
.feat <- function(id, name, passed) list(id = id, name = name, name_en = toupper(name), passed = passed)

test_that("the additive level: typical when every feature is FALSE", {
  skip_on_cran()
  r <- .add_additive_level(.res(), NULL, list(.feat("g", "gleyic", FALSE), .feat("y", "tongued", FALSE)))
  expect_identical(r$name, "Podzolic [typical]")
  expect_identical(utils::tail(r$levels$level, 1L), "subtypes")
  expect_identical(utils::tail(r$levels$code, 1L), NA_character_)
  expect_identical(r$qualifiers$subtypes, "[typical]")
  expect_identical(r$provisional_because, character(0))
  expect_identical(names(r$trace$subtypes), c("g", "y"))
  r <- .add_additive_level(.res(), NULL, list(), typical = "[\u0442\u0438\u043f\u0438\u0447\u043d\u044b\u0435]")
  expect_identical(r$name, "Podzolic [\u0442\u0438\u043f\u0438\u0447\u043d\u044b\u0435]")
})

test_that("the additive level: every feature that passes, in list order; provisional when some are NA", {
  skip_on_cran()
  f <- list(.feat("y", "tongued", TRUE), .feat("ca", "residual-calcareous", FALSE), .feat("g", "gleyic", TRUE))
  r <- .add_additive_level(.res(), NULL, f)
  expect_identical(r$name, "Podzolic tongued, gleyic")
  expect_identical(utils::tail(r$levels$code, 2L), c("y", "g"))
  expect_identical(utils::tail(r$levels$name_en, 2L), c("TONGUED", "GLEYIC"))
  f <- list(.feat("g", "gleyic", TRUE),
            list(id = "q", name = "quasigleyic", test = function(pedon, ctx) list(passed = NA, missing = "munsell_chroma_moist")),
            .feat("y", "tongued", FALSE))
  r <- .add_additive_level(.res(), NULL, f)
  expect_identical(r$name, "Podzolic gleyic")
  expect_identical(r$provisional_because, "subtypes:q")
  expect_match(r$warnings, "Provisional subtypes", all = FALSE)
  expect_true("munsell_chroma_moist" %in% r$missing_data)
  # only undecided ones: no typical, the name stays at the type
  r <- .add_additive_level(.res(), NULL, list(list(id = "q", name = "quasigleyic", passed = NA)))
  expect_identical(r$name, "Podzolic")
  expect_identical(r$qualifiers$subtypes, character(0))
  expect_identical(nrow(r$levels), 2L)
})

test_that("the additive level: a feature written in the type's formula is not named again", {
  skip_on_cran()
  f <- list(.feat("g", "gleyic", TRUE), .feat("y", "tongued", TRUE))
  r <- .add_additive_level(.res("CA02"), NULL, f, already = "g")
  expect_identical(r$name, "Podzolic tongued")
  expect_true(isTRUE(r$trace$subtypes$g$skipped))
  r <- .add_additive_level(.res("CA02"), NULL, list(.feat("g", "gleyic", TRUE)), already = "g")
  expect_identical(r$name, "Podzolic [typical]")
  # the tests see the type and the context; an error is an undecided test
  f <- list(list(id = "a", name = "agro", test = function(pedon, ctx) ctx$type == "CA17" && ctx$extra),
            list(id = "b", name = "broken", test = function(pedon, ctx) stop("boom")))
  r <- .add_additive_level(.res("CA17"), NULL, f, context = list(extra = TRUE))
  expect_identical(r$name, "Podzolic agro")
  expect_identical(r$provisional_because, "subtypes:b")
  expect_match(r$trace$subtypes$b$notes, "boom")
  # no type assigned: unchanged
  r0 <- .res(NA_character_)
  r <- .add_additive_level(r0, NULL, list(.feat("g", "gleyic", TRUE)))
  expect_identical(r$name, "Podzolic")
  expect_identical(nrow(r$levels), 2L)
  expect_error(.add_additive_level(.res(), NULL, list(list(name = "x", passed = TRUE))), "no id")
})
