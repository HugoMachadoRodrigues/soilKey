# v0.9.335: the horizon notation of the Russian Field Guide of 2008 (the
# notation reader R/designation-kidpr.R and the catalogues of
# inst/rules/kidpr2008), Hugo's decisions of 2026-11-02 (Q3 and the notation
# defaults). Every profile formula of chapters 4 and 6 is in
# fixtures/kidpr2008-formulas.tsv.

.voc <- function() .kidpr_vocabulary()
.kd <- function(d, site = list()) .kidpr_designations(d, site)
.one <- function(d, caps = FALSE) .kidpr_parse_one(d, caps = caps)
.pff <- function(s) {
  v <- .voc()
  .parse_profile_formula(s, masters = v$masters, features = v$features)
}
.formulas <- function() {
  path <- testthat::test_path("fixtures", "kidpr2008-formulas.tsv")
  utils::read.delim(path, comment.char = "#", quote = "", stringsAsFactors = FALSE,
                    encoding = "UTF-8", colClasses = "character")
}


# --- the catalogues ------------------------------------------------------------

test_that("the horizon catalogue has the 50 horizons and the other symbols", {
  skip_on_cran()
  h <- .kidpr_catalogue()$horizons
  code <- function(l) vapply(l, function(e) e$code, "")
  grp <- vapply(h$horizons, function(e) e$group, "")
  expect_length(h$horizons, 50L)
  expect_false(anyDuplicated(code(h$horizons)) > 0L)
  expect_identical(as.integer(table(factor(grp, c("organic", "humus", "eluvial", "middle",
                                                  "hydrogenic", "halomorphic", "anthropogenic")))),
                   c(6L, 10L, 5L, 17L, 4L, 2L, 6L))
  expect_true(all(c("AY", "BHF", "TUR", "CRH", "H", "X", "PTR") %in% code(h$horizons)))
  expect_identical(code(h$modifications), c("AV", "BH", "BF"))
  expect_identical(code(h$aliases), "PC")
  expect_identical(code(h$rock_symbols), c("C", "D", "CLM", "R", "TT", "TPO"))
  expect_identical(code(h$legacy), c("PY", "TR", "AKL", "AM"))
  expect_length(h$marks, 6L)
  for (e in h$horizons) {
    expect_true(nzchar(e$name_native) && nzchar(e$name) && nzchar(e$key) && is.numeric(e$page),
                info = e$code)
  }
  v <- .voc()
  expect_identical(unname(v$agro), c("P", "PU", "PB", "PT", "PTR"))
  expect_setequal(v$overlays, c("V", "S", "X"))
})

test_that("the feature catalogue has the 64 features of 3.2, three of the lists, aliases", {
  skip_on_cran()
  f <- .kidpr_catalogue()$features
  code <- vapply(f$features, function(e) e$code, "")
  grp <- vapply(f$features, function(e) e$group, "")
  expect_length(f$features, 67L)
  expect_false(anyDuplicated(code) > 0L)
  expect_identical(as.integer(table(factor(grp, c("transitional", "process", "turbation", "evolutionary",
                                                  "parent_rock", "substrate", "list_only")))),
                   c(23L, 14L, 5L, 10L, 4L, 8L, 3L))
  expect_true(all(c("y", "\u22a5", "@", "[e-hf]", "[hh]", "[akl]", "TT~~", "TT\"") %in% code))
  v <- .voc()
  expect_identical(v$feature_aliases[c("tu", "ko", "ag", "ael", "as", "hh", "akl")],
                   c(tu = "tr", ko = "ek", ag = "agr", ael = "ae", as = "cs", hh = "[hh]", akl = "[akl]"))
  expect_setequal(intersect(v$ambiguous, c("hi", "gr", "md", "mr", "rh", "ur", "yu", "hh", "ff", "rr")),
                  c("hi", "gr", "md", "mr", "rh", "ur", "yu", "hh", "ff", "rr"))
  for (e in f$features) {
    expect_true(nzchar(e$name_native) && nzchar(e$name) && is.numeric(e$page), info = e$code)
  }
})

test_that("the host forms of the chapter 4 lists read as their host and features", {
  skip_on_cran()
  for (e in .kidpr_catalogue()$features$host_forms) {
    w <- e$written
    host <- unlist(e$host)
    parts <- if (length(host) > 1L) strsplit(w, "-", fixed = TRUE)[[1L]] else w
    x <- .kd(parts, list(designation_system = "kidpr"))
    expect_identical(x$index, host, info = w)
    expect_setequal(unlist(x$features), unlist(e$features))
  }
  expect_match(.one("Tote")$notes[1L], "TOte", fixed = TRUE)
})


# --- every formula of the book ----------------------------------------------------

test_that("every formula of chapters 4 and 6 parses with zero unknown tokens", {
  skip_on_cran()
  fx <- .formulas()
  expect_identical(nrow(fx), 543L)
  expect_identical(sum(startsWith(fx$source, "ch4")), 275L)
  expect_identical(sum(startsWith(fx$source, "ch6")), 268L)
  v <- .voc()
  codes <- character(0)
  feats <- character(0)
  for (i in seq_len(nrow(fx))) {
    p <- tryCatch(.pff(fx$formula[i]), error = function(e) conditionMessage(e))
    expect_s3_class(p, "soilkey_formula")
    if (!inherits(p, "soilkey_formula")) next
    for (z in p$slots) {
      codes <- c(codes, z$codes, z$composition)
      feats <- c(feats, z$small)
      for (fm in z$forms) codes <- c(codes, fm$code, fm$composition)
      for (fm in z$forms) feats <- c(feats, fm$required, fm$typical)
    }
    # round trip: the slots give back the formula as normalised
    wr <- vapply(p$slots, function(z) if (z$optional) paste0("(", z$written, ")") else z$written, "")
    expect_identical(paste(wr, collapse = "-"), .formula_normalise(fx$formula[i]), info = fx$formula[i])
  }
  # every code used in the formulas is in the catalogues
  codes <- setdiff(unique(codes), c("[ABC]", "{ABC}"))
  expect_true(all(codes %in% v$masters), info = paste(setdiff(codes, v$masters), collapse = " "))
  feats <- unique(feats)
  expect_true(all(feats %in% c(v$features, "@", "\u22a5")),
              info = paste(setdiff(feats, v$features), collapse = " "))
  # the formulas use most of the vocabulary
  expect_true(all(c("AY", "TUR", "BAN", "CLM", "TT", "PB", "PC", "SS", "AKL", "AM", "PY", "TR") %in% codes))
})

test_that("an unknown token is refused, with its name", {
  skip_on_cran()
  expect_error(.pff("SS-GsC-Gss"), "Unknown token")
  expect_error(.pff("O-EL-BTj-C"), "Unknown feature 'j'")
  expect_error(.pff("O-ZZ-C"), "Unknown horizon 'ZZ'")
  # the printing slips of chapter 6 are corrected in the fixture, with a note
  fx <- .formulas()
  # (v0.9.347: the two rows added then carry a note too, no slip)
  expect_identical(sum(nzchar(fx$note) & !grepl("added in 0.9.347", fx$note, fixed = TRUE)), 9L)
  expect_true("TJ-E-BHF-C" %in% fx$formula)
  expect_error(.pff("J-E-BHF-C"), "Unknown horizon 'J'")
})


# --- normalisation ----------------------------------------------------------------

test_that("Cyrillic look-alikes, quotes and the permafrost sign are folded", {
  skip_on_cran()
  x <- .kd(c("\u0410Y", "\u0421AT", "\u041d", "\u0425", "\u0412EL", "\u0422\u041f\u041e", "\u0435\u043a"),
           list(designation_system = "ru"))
  expect_identical(x$index, c("AY", "CAT", "H", "X", "BEL", "TPO", NA))
  expect_identical(x$features[[7L]], "ek")
  expect_true(x$small[7L])
  expect_match(x$notes[[1L]][1L], "Cyrillic", fixed = TRUE)
  expect_identical(x$group[6L], "not_soil")
  # formulas: the book's own homoglyph forms
  st <- .formula_slots_table(.pff("\u0422-ELg-BELg-BTg-G-Cg"))
  expect_identical(st$codes, c("T", "EL", "BEL", "BT", "G", "C"))
  st <- .formula_slots_table(.pff("\u0420Y-(AEL)-BEL-BT-C"))
  expect_identical(st$codes[1:2], c("PY", "AEL"))
  st <- .formula_slots_table(.pff("\u041d-CRM-C"))
  expect_identical(st$codes[1L], "H")
  # the permafrost sign, as U+22A5 or in the Symbol font (U+F05E)
  expect_identical(.one("C\uf05e")$features, "\u22a5")
  expect_identical(.one("C\u22a5")$features, "\u22a5")
  expect_identical(.one("C\u201d")$mark, "\"")
  expect_identical(.one("C``")$mark, "\"")
})

test_that("notes, lithological prefixes and sub-horizons", {
  skip_on_cran()
  x <- .kd(c("2BM", "3D", "IIBT", "II BT", "IVC", "BT1", "BT2", "(EL)", "BTg (sampled)", "BT/glossic", "BT?"),
           list(designation_system = "kidpr2008"))
  expect_identical(x$index, c("BM", "D", "BT", "BT", "C", "BT", "BT", "EL", "BT", "BT", "BT"))
  expect_identical(x$lith, c(2L, 3L, 2L, 2L, 4L, rep(1L, 6L)))
  expect_identical(x$subdiv[6:7], c("1", "2"))
  expect_identical(x$remnant, c(rep(FALSE, 7L), TRUE, rep(FALSE, 3L)))
  expect_identical(x$features[[9L]], "g")
})


# --- the notation of a profile (Hugo's decision Q3) --------------------------------

test_that("the notation is declared, or recognised by a Russian-only index", {
  skip_on_cran()
  for (a in c("kidpr", "kidpr2008", "ru", "ru2004", "ru2008", "russian", "russia", "rfg2008", "rsc2004")) {
    expect_true(.kd(c("O", "G", "CG"), list(designation_system = a))$kidpr, info = a)
  }
  # declared as another notation: never Russian, whatever the letters
  x <- .kd(c("AY", "EL", "BEL", "BT", "C"), list(designation_system = "fao"))
  expect_false(x$kidpr)
  expect_true(all(is.na(x$index)))
  # recognised
  x <- .kd(c("AY", "EL", "BEL", "BTg,ox", "C"))
  expect_true(x$kidpr)
  expect_match(x$why, "AY, EL, BEL", fixed = TRUE)
  expect_identical(x$index, c("AY", "EL", "BEL", "BT", "C"))
  expect_identical(x$features[[4L]], c("g", "ox"))
  # by a comma-separated feature list, by a bracketed feature
  expect_true(.kd(c("A", "B", "Cca,s"))$kidpr)
  expect_true(.kd(c("O", "[e-hf]", "C"))$kidpr)
  expect_true(.kd(c("TUR[SEL+ASN]", "C"))$kidpr)
  # O, G and C are shared by other notations: not recognised alone
  expect_false(.kd(c("O", "G", "CG"))$kidpr)
})

test_that("an FAO profile is not read as Russian; its letters give hints only", {
  skip_on_cran()
  x <- .kd(c("Ap", "Bt", "Bk", "C"))
  expect_false(x$kidpr)
  expect_true(all(is.na(x$index)))
  expect_false(any(x$known))
  expect_identical(x$hint[[1L]], c("P", "PU", "PB", "PT", "PTR"))
  expect_identical(x$hint[[2L]], c("BT", "BI"))
  expect_identical(x$hint[[3L]], c("BCA", "CAT"))
  # the country never decides
  expect_false(.kd(c("Ap", "Bt", "Bk", "C"), list(country = "RU"))$kidpr)
  expect_false(.kd(c("Ap", "Bt", "Bk", "C"), list(country = "Russia"))$kidpr)
  h <- .kd(c("Oi", "Oe", "Ah", "E", "Bhs", "Btn", "Bss", "Bw", "Cr", "Bg", "Az", "Cf"),
           list(designation_system = "fao2006"))
  expect_identical(h$hint[[1L]], c("T", "H", "O"))
  expect_identical(h$hint[[2L]], c("T", "H", "O"))
  expect_identical(h$hint[[4L]], c("E", "EL", "AEL", "BEL", "SEL"))
  expect_identical(h$hint[[5L]], "BHF")
  expect_identical(h$hint[[6L]], c("ASN", "BSN"))
  expect_identical(h$hint[[7L]], "V")
  expect_identical(h$hint[[8L]], c("BM", "BFM", "BMK"))
  expect_identical(h$hint[[9L]], "G")
  expect_identical(h$hint_features[[10L]], "g")
  expect_true("S" %in% h$hint[[11L]])
  expect_identical(h$hint_features[[12L]], "\u22a5")
})

test_that("a Soviet 1977 profile is not read as Russian 2008; it gives the same hints", {
  skip_on_cran()
  for (site in list(list(), list(country = "RU"))) {
    x <- .kd(c("A0", "A1", "A2", "B", "Bca"), site)
    expect_false(x$kidpr)
    expect_true(all(is.na(x$index)))
    expect_identical(x$hint[[1L]], "O")
    expect_identical(x$hint[[2L]], c("AY", "AJ", "AU", "AH", "AK", "AO", "W"))
    expect_identical(x$hint[[3L]], c("E", "EL", "AEL", "BEL", "SEL"))
    expect_identical(x$hint[[5L]], c("BCA", "CAT"))
  }
  expect_identical(.kd(c("A1A2", "\u0410\u043f\u0430\u0445"))$hint[[2L]], c("P", "PU", "PB", "PT", "PTR"))
})

test_that("an all-capitals profile is Russian only by an index FAO cannot spell", {
  skip_on_cran()
  x <- .kd(c("AP", "BTG", "BK", "C"))
  expect_true(x$caps)
  expect_false(x$kidpr)
  expect_identical(x$hint[[1L]], c("P", "PU", "PB", "PT", "PTR"))
  expect_identical(x$hint_features[[2L]], "g")
  # one row written as FAO capitals does not outweigh three Russian indices
  x <- .kd(c("AY", "EL", "BEL", "BTG", "C"))
  expect_true(x$caps)
  expect_true(x$kidpr)
  expect_identical(x$features[[4L]], "g")
  # but AY, EL alone do not outweigh more FAO rows
  expect_false(.kd(c("AY", "AP", "BW", "BK", "C"))$kidpr)
  # PU cannot: the capitals after an index are then its features
  x <- .kd(c("PU", "AU", "BCA", "CCA"))
  expect_true(x$caps && x$kidpr)
  expect_identical(x$index, c("PU", "AU", "BCA", "C"))
  expect_identical(x$features[[4L]], "ca")
  expect_match(x$notes[[4L]], "all-capitals", fixed = TRUE)
  # declared: BTG = BT + g
  x <- .kd(c("AY", "EL", "BTG", "C"), list(designation_system = "ru"))
  expect_true(x$caps && x$kidpr)
  expect_identical(x$index[3L], "BT")
  expect_identical(x$features[[3L]], "g")
  # capitals only, but nothing written as a capital for a lower-case letter
  x <- .kd(c("AY", "EL", "BEL", "BT", "C"))
  expect_false(x$caps)
  expect_true(x$kidpr)
})


# --- rows -------------------------------------------------------------------------

test_that("features with and without commas; ambiguous strings are noted", {
  skip_on_cran()
  x <- .kd(c("BCAs,cs", "BCAscs", "BTg,ox", "BTgox", "Cgca", "Ohi", "Oh,i", "TEmr", "Cqca,s"),
           list(designation_system = "ru"))
  expect_identical(x$features[1:5], list(c("s", "cs"), c("s", "cs"), c("g", "ox"), c("g", "ox"), c("g", "ca")))
  expect_identical(x$features[[6L]], "hi")
  expect_match(x$ambiguous[6L], "hi", fixed = TRUE)
  expect_identical(x$features[[7L]], c("h", "i"))
  expect_true(is.na(x$ambiguous[7L]))
  expect_match(x$ambiguous[8L], "mr", fixed = TRUE)
  # a lower-case q is the feature q (quasi-gleyic), a capital Q the rock flag
  expect_true(is.na(x$rockflag[9L]))
  expect_identical(x$features[[9L]], c("q", "ca", "s"))
  expect_identical(.one("CQca,s")$rockflag, "Q")
})

test_that("a layer holds a horizon and an overlay S, X or V", {
  skip_on_cran()
  x <- .kd(c("AU+S", "BT+X", "S+BT", "G+V", "AU+BT"), list(designation_system = "ru"))
  expect_identical(x$index, c("AU", "BT", "BT", "G", "AU"))
  expect_identical(x$overlay[1:4], list("S", "X", "S", "V"))
  expect_true(all(x$known[1:4]))
  expect_false(x$known[5L])
})

test_that("small horizons are marked rows, not diagnostic horizons", {
  skip_on_cran()
  x <- .kd(c("O", "e", "ek", "ko", "ao", "[e-hf]", "[hh]", "hh", "rh", "pir", "BHF", "C"),
           list(designation_system = "ru"))
  expect_identical(x$small, c(FALSE, rep(TRUE, 9L), FALSE, FALSE))
  expect_true(all(is.na(x$index[2:10])))
  expect_identical(unlist(x$features[2:10]), c("e", "ek", "ek", "ao", "[e-hf]", "[hh]", "[hh]", "rh", "pir"))
  expect_identical(x$bracket[6:7], c("[e-hf]", "[hh]"))
  expect_true(is.na(x$ambiguous[7L]))
  expect_match(x$ambiguous[8L], "hh", fixed = TRUE)
  expect_match(x$ambiguous[9L], "rh", fixed = TRUE)
  # a turbated mixture, a buried profile, a buried horizon
  y <- .kd(c("[AY, EL]tr", "[ABC]", "[AU]"), list(designation_system = "ru"))
  expect_identical(y$composition[[1L]], c("AY", "EL"))
  expect_identical(y$features[[1L]], "tr")
  expect_identical(y$buried, c(FALSE, TRUE, TRUE))
  expect_identical(y$index[3L], "AU")
})

test_that("transitions are not diagnostic", {
  skip_on_cran()
  x <- .kd(c("AU", "AUBM", "AU/BM", "BMC", "BC", "AB", "BM"), list(designation_system = "ru"))
  expect_identical(x$transitional, c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE))
  expect_true(all(is.na(x$index[2:6])))
  expect_identical(x$masters[2:4], list(c("AU", "BM"), c("AU", "BM"), c("BM", "C")))
  expect_false(x$caps)
})

test_that("aliases, modifications and the tokens of chapter 6", {
  skip_on_cran()
  x <- .kd(c("Tote", "PC", "PB(PC)", "BF", "BH", "AV", "Ptu", "Pag", "RYael", "BCAas", "Pw", "AKL", "PY", "TR", "AM"),
           list(designation_system = "ru"))
  expect_identical(x$index, c("TO", "PB", "PB", "BHF", "BHF", "V", "P", "P", "RY", "BCA", "P", "AJ", "P", "T", NA))
  expect_identical(x$modification[4:6], c("BF", "BH", "AV"))
  expect_identical(x$features[c(1, 7:12)],
                   list("te", "tr", "agr", "ae", "cs", "w", "[akl]"))
  expect_identical(x$legacy, c(rep(FALSE, 11L), rep(TRUE, 4L)))
  expect_identical(x$hint[[15L]], c("BMK", "BM"))
  expect_identical(x$group[c(2, 4, 13)], c("anthropogenic", "middle", "anthropogenic"))
  expect_true(all(x$agro[c(2, 3, 7, 8, 11, 13)]))
})

test_that("rock flags and marks belong to the rock, not to a G or Q horizon", {
  skip_on_cran()
  x <- .kd(c("CG", "CQca,s", "DG", "C~~", "C\"", "C^^", "C\"\"", "C=", "C..", "CG~~", "G"),
           list(designation_system = "ru"))
  expect_identical(x$index, c("C", "C", "D", rep("C", 7L), "G"))
  expect_identical(x$rockflag, c("G", "Q", "G", rep(NA, 6L), "G", NA))
  expect_identical(x$mark, c(NA, NA, NA, "~~", "\"", "^^", "\"\"", "=", "..", "~~", NA))
  expect_identical(x$group, c(rep("rock", 10L), "hydrogenic"))
})


# --- the fixes of the review (2026-11-02) -------------------------------------------

.ru <- list(designation_system = "ru")
.has_note <- function(notes, pattern) any(grepl(pattern, notes, fixed = TRUE))

test_that("H1: text that is not valid UTF-8 never stops the reader", {
  skip_on_cran()
  cp <- iconv("\u0410\u0423", "UTF-8", "CP1251")
  x <- .kd(c("AY", cp), list())
  expect_identical(x$index, c("AY", "AY"))
  expect_true(.has_note(x$notes[[2L]], "Windows-1251"))
  lat <- "BT\xb4"
  Encoding(lat) <- "latin1"
  for (bad in list("BT\xff", "BT\xff\xfe", rawToChar(as.raw(c(0x42, 0x54, 0xc3))), lat)) {
    x <- expect_no_error(.kd(c("AY", "EL", bad), list()))
    expect_true(x$kidpr)
    expect_identical(x$index[1:2], c("AY", "EL"))
    expect_false(x$known[3L])
  }
  expect_no_error(.kd(c(NA, "", " "), list()))
})

test_that("H2: a Soviet A0 typed with the letter O is not the Russian AO", {
  skip_on_cran()
  x <- .kd(c("AO", "A1", "A2", "B1", "B2", "C"))
  expect_false(x$kidpr)
  expect_match(x$why, "Soviet", fixed = TRUE)
  expect_identical(x$hint[[1L]], "O")
  cyr <- c("\u0410\u041e", paste0("\u0410", "1"), paste0("\u0410", "2"), "\u0412", "\u0421")
  x <- .kd(cyr)
  expect_false(x$kidpr)
  expect_identical(x$hint[[1L]], "O")
  # with lower case too, and an old FAO AO
  expect_false(.kd(c("AO", "A1", "A2", "Bt", "Cca"))$kidpr)
  x <- .kd(c("AO", "Bw", "C"))
  expect_false(x$kidpr)
  expect_identical(x$hint[[1L]], "O")
  # beside a Russian-only index other than AO, the profile is Russian
  x <- .kd(c("O", "AO", "BHF", "C"))
  expect_true(x$kidpr)
  expect_identical(x$index[2L], "AO")
})

test_that("H3: one row does not flip a Russian profile into FAO capitals", {
  skip_on_cran()
  base <- c("AY", "EL", "BEL", "BT", "C")
  for (miss in c("NA", "N/A", "ND", "-", "?", "n/a")) {
    x <- .kd(c(base, miss))
    expect_true(x$kidpr, info = miss)
    expect_false(x$caps, info = miss)
    expect_true(is.na(x$index[6L]) && !x$known[6L], info = miss)
    expect_true(.has_note(x$notes[[6L]], "records nothing"), info = miss)
  }
  lat <- "BT\xb4"
  Encoding(lat) <- "latin1"
  x <- .kd(c("AY", "EL", "BEL", lat, "C"))
  expect_true(x$kidpr)
  expect_false(x$caps)
  # a Cyrillic lower-case letter is lower case
  x <- .kd(c("AY", "EL", "BT\u0433", "C"))
  expect_true(x$kidpr)
  expect_false(x$caps)
  # Russian transitions of two indices, and a junk row
  expect_true(.kd(c("AU", "AUB", "BM", "BC", "C"))$kidpr)
  x <- .kd(c("AY", "EL", "BEL", "BT", "BTD", "D"))
  expect_true(x$kidpr)
  expect_identical(x$masters[[5L]], c("BT", "D"))
  x <- .kd(c("AY", "EL", "BEL", "BT", "CB", "C"))
  expect_true(x$kidpr)
  expect_true(x$transitional[5L])
  expect_true(.kd(c("AY", "EL", "BEL", "BTG", "G"))$kidpr)
  expect_true(.kd(c(base, "QQZ"))$kidpr)
  expect_true(.kd(c(base, "BTCA"))$kidpr)
  # AH, AK, AO written in capitals beside shared letters only: no decision
  for (p in list(c("AH", "C"), c("AH", "AC", "C"), c("AH", "BC", "C"), c("AH", "BW", "C"),
                 c("AO", "C"), c("AK", "C"))) {
    expect_false(.kd(p)$kidpr, info = paste(p, collapse = "-"))
  }
  # ... but where the case is kept, or beside an index FAO has no master for
  expect_true(.kd(c("AH", "Cca"))$kidpr)
  expect_true(.kd(c("P", "BH", "G", "CG"))$kidpr)
  # the limit left: AU over C alone reads as Russian (an FAO Au is rare)
  expect_true(.kd(c("AU", "C"))$kidpr)
})

test_that("M1: spaces inside a feature list do not drop features", {
  skip_on_cran()
  x <- .kd(c("BCAs, cs", "BCA s,cs", "AJ [akl]", "BT grey", "BT g"), .ru)
  expect_identical(x$features[1:3], list(c("s", "cs"), c("s", "cs"), "[akl]"))
  expect_identical(x$features[[4L]], character(0))
  expect_identical(x$features[[5L]], "g")
  expect_true(.has_note(x$notes[[1L]], "spaces around a comma"))
  expect_true(.has_note(x$notes[[2L]], "after a space read as part of the designation"))
  expect_true(.has_note(x$notes[[4L]], "'grey' after a space read as a note"))
})

test_that("M2: a bracket that is no feature of the catalogue is read by its parts, noted", {
  skip_on_cran()
  x <- .kd(c("[g]", "[e]", "[ek]", "[gox]", "[e,hf]", "[hf-e]", "AJ[g]", "[abc]"), .ru)
  expect_identical(x$known, c(FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE))
  expect_identical(x$features[[2L]], "e")
  expect_true(.has_note(x$notes[[2L]], "no bracketed feature of the Field Guide"))
  expect_identical(x$features[5:6], list("[e-hf]", "[e-hf]"))
  expect_true(.has_note(x$notes[[6L]], "[hf-e] read as [e-hf]"))
  expect_identical(x$index[7L], "AJ")
  expect_identical(x$features[[7L]], "g")
  expect_true(all(is.na(x$bracket[c(2L, 3L)])))
})

test_that("M3: one horizon in brackets is buried, with its features", {
  skip_on_cran()
  x <- .kd(c("[AU]", "[AUg]", "[BT1]", "[2BT]", "[AU]g", "[AU,EL]", "[AY+EL]tr"), .ru)
  expect_identical(x$index, c("AU", "AU", "BT", "BT", "AU", NA, NA))
  expect_identical(x$buried, c(rep(TRUE, 5L), FALSE, FALSE))
  expect_identical(x$features[c(2L, 5L)], list("g", "g"))
  expect_identical(x$subdiv[3L], "1")
  expect_identical(x$lith[4L], 2L)
  expect_identical(x$composition[6:7], list(c("AU", "EL"), c("AY", "EL")))
  expect_identical(x$features[[7L]], "tr")
})

test_that("M4: the hints of the Soviet and FAO letters", {
  skip_on_cran()
  h <- function(d, caps = FALSE) .kidpr_hint(d, caps)
  for (d in c("Bf", "Bhf", "Bfe")) {
    expect_identical(h(d)$hint, "BHF", info = d)
    expect_identical(h(d)$hint_features, character(0), info = d)
  }
  expect_identical(h("Bcs")$hint, character(0))
  expect_identical(h("Bcs")$hint_features, "cs")
  expect_identical(h("T")$hint, c("T", "TO", "TE", "TJ"))
  expect_identical(h("AT")$hint, c("T", "AO"))
  expect_identical(h("G")$hint, "G")
  expect_identical(h("A2B")$hint, "BEL")
  expect_identical(h("A1A2", caps = TRUE)$hint[1L], "AEL")
  expect_identical(h("Ck")$hint_features, "ca")
  expect_identical(h("Cca")$hint_features, "ca")
  expect_identical(h("Bz")$hint, character(0))
  expect_identical(h("Bz")$hint_features, "s")
  expect_true("S" %in% h("Az")$hint)
  expect_identical(h("Cf")$hint_features, "\u22a5")
})

test_that("M5: every formula with a sign only the Russian notation writes is recognised", {
  skip_on_cran()
  fx <- .formulas()
  rec <- vapply(fx$formula, function(f) {
    rows <- .formula_items(.formula_normalise(f), f)
    .kd(rows, list())$kidpr
  }, NA)
  # v0.9.347: 480 with the two formulas of chapter 6 the text layer had
  # hidden (AY-AEL-BM-C, P-TUR-C)
  expect_identical(sum(rec), 480L)
  # the 63 left are written only with letters other notations share (O, T,
  # H, G, C, R, D, P, S, W, E, BT, BI, BM), or with AH, AK, AO beside them
  shared <- c("O", "T", "H", "G", "Q", "C", "R", "D", "P", "S", "W", "E", "BT", "BI", "BM", "CR",
              "AH", "AK", "AO", "[ABC]")
  v <- .voc()
  left <- fx$formula[!rec]
  codes <- unique(unlist(lapply(left, function(f)
    unlist(lapply(.pff(f)$slots, function(z) c(z$codes, z$small))))))
  expect_true(all(codes %in% shared), info = paste(setdiff(codes, shared), collapse = " "))
  for (f in c("O-CR-C\u22a5", "O-C~~", "W-C^^", "H-G-CG~~", "P-BH-G-CG", "PY-E-BF-C", "O-C..", "W-C=")) {
    expect_true(.kd(.formula_items(.formula_normalise(f), f), list())$kidpr, info = f)
  }
  expect_true(.kd(c("\u0422\u041f\u041e", "C"))$kidpr)
  expect_true(.kd(c("O", "C\uf05e"))$kidpr)
})

test_that("M6: a long feature run that cannot be split is refused at once", {
  skip_on_cran()
  v <- .voc()
  t0 <- proc.time()[["elapsed"]]
  expect_null(.formula_decompose(paste0(strrep("r", 40), "j"), v$features))
  x <- .kd(c("AY", paste0("BT", strrep("r", 40), "j")), list())
  expect_lt(proc.time()[["elapsed"]] - t0, 2)
  expect_false(x$known[2L])
  # the split itself is unchanged: longest first, with backtracking
  expect_identical(.formula_decompose("scs", v$features), c("s", "cs"))
  expect_identical(.formula_decompose("gox", v$features), c("g", "ox"))
  expect_identical(.formula_decompose("hhi", v$features), c("hh", "i"))
})

test_that("M7: the psammozems of chapter 6 take the sand mark", {
  skip_on_cran()
  fx <- .formulas()
  ps <- fx[fx$source %in% c("ch6 p159", "ch6 p160") & grepl("^[OW]-C\\.\\.$", fx$formula), ]
  expect_identical(ps$formula, c("O-C..", "W-C.."))
  expect_true(all(grepl("sand mark", ps$note, fixed = TRUE)))
  marks <- .kidpr_catalogue()$horizons$marks
  ash <- Filter(function(e) e$code == "\"", marks)[[1L]]
  expect_match(ash$note, "psammozems", fixed = TRUE)
})

test_that("M8: in the Russian notation f is not frozen and the permafrost sign is", {
  skip_on_cran()
  for (ds in c("ru", "kidpr", "Russian 2008")) {
    pat <- .frozen_designation_pattern(list(designation_system = ds))
    expect_false(grepl(pat, "Cf"), info = ds)
    expect_false(grepl(pat, "ELf"), info = ds)
    expect_true(grepl(pat, "C\u22a5"), info = ds)
    expect_true(grepl(pat, "BT\uf05e"), info = ds)
  }
  expect_true(grepl(.frozen_designation_pattern(list()), "Cf"))
  expect_true(grepl(.frozen_designation_pattern(list(designation_system = "fao")), "Bf"))
})

test_that("L2, L3: Russian texture words; the declared value is normalised", {
  skip_on_cran()
  a <- horizon_attribute_spec()
  allowed <- strsplit(a$allowed[a$name == "texture_class_ru"], ", ", fixed = TRUE)[[1L]]
  expect_true(all(c("sand", "heavy clay", "\u043f\u0435\u0441\u043e\u043a",
                    "\u0441\u0443\u043f\u0435\u0441\u044c") %in% allowed))
  for (ds in c(" RU ", "Russian 2008", "rus", "KIDPR-2008", "Russian")) {
    expect_true(.kd(c("O", "G", "CG"), list(designation_system = ds))$kidpr, info = ds)
  }
  for (ds in c("unknown", "none", "n/a", "NA", "", "1")) {
    x <- .kd(c("AY", "EL", "BT"), list(designation_system = ds))
    expect_true(x$kidpr, info = ds)
    expect_false(grepl("designation_system", x$why), info = ds)
  }
  expect_no_error(x <- .kd(c("AY", "BT"), "ru"))
  expect_true(x$kidpr)
  expect_false(.kd(c("AY", "BT"), data.frame(designation_system = "fao"))$kidpr)
  expect_true(.kd(c("O", "G"), list(designation_system = list("ru")))$kidpr)
})

test_that("L4: transitions, rock flags, brackets, marks and folded characters", {
  skip_on_cran()
  x <- .kd(c("CB", "CGG", "BT1/BT2", "BTBT", "IIIIBT", "AU+", "(AU+S)", "BT[AU]", "BT~~",
             "C\u02dc\u02dc", "C\u223c\u223c", "C\uff5e\uff5e", "C'", "\uff21\uff39",
             "\u0406\u0406BT", "AU/BM+S"), .ru)
  expect_true(x$transitional[1L])
  expect_identical(x$masters[[1L]], c("C", "B"))
  expect_identical(x$rockflag[2L], "G")
  expect_identical(x$index[3:4], c("BT", "BT"))
  expect_false(any(x$transitional[3:4]))
  expect_false(x$known[5L])
  expect_identical(x$lith[5L], 1L)
  expect_identical(x$index[6L], "AU")
  expect_true(.has_note(x$notes[[6L]], "empty part"))
  expect_identical(x$index[7L], "AU")
  expect_identical(x$overlay[[7L]], "S")
  expect_true(x$remnant[7L])
  expect_false(x$known[8L])
  expect_true(.has_note(x$notes[[8L]], "only TUR"))
  expect_true(.has_note(x$notes[[9L]], "written on a rock"))
  expect_identical(x$mark[10:12], rep("~~", 3L))
  expect_identical(x$mark[13L], "\"")
  expect_identical(x$index[14L], "AY")
  expect_identical(x$lith[15L], 2L)
  expect_identical(x$masters[[16L]], c("AU", "BM"))
  expect_identical(x$overlay[[16L]], "S")
})

test_that("L5, L6: small horizons by the catalogue; more ambiguous runs", {
  skip_on_cran()
  x <- .kd(c("ca", "g", "s", "v", "te", "cf", "Pagr", "Crx", "BHFhf"), .ru)
  expect_identical(x$known[1:4], rep(FALSE, 4L))
  expect_true(.has_note(x$notes[[1L]], "not a small horizon"))
  expect_identical(x$small[5:6], c(TRUE, TRUE))
  expect_false(any(is.na(x$ambiguous[7:9])))
  f <- .kidpr_catalogue()$features$features
  host <- function(code) unlist(Filter(function(e) e$code == code, f)[[1L]]$hosts)
  expect_true(all(c("AY", "AU", "T") %in% host("h")))
  expect_identical(host("cf"), "G")
  expect_true(all(c("P", "PU", "BT", "BM") %in% host("w")))
  expect_true("AU" %in% host("el"))
})

test_that("L7: notes name what was read", {
  skip_on_cran()
  x <- .kd(c("CG~~(CQ~~)", "BT/\u0433\u043b\u0435\u0435\u0432\u044b\u0439", "BT?", "\u0412T"), .ru)
  expect_true(.has_note(x$notes[[1L]], "CQ~~"))
  expect_false(.has_note(x$notes[[2L]], "Cyrillic"))
  expect_true(.has_note(x$notes[[3L]], "trailing '?'"))
  expect_true(.has_note(x$notes[[4L]], "Cyrillic"))
})
