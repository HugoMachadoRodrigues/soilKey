# v0.9.360: the horizon notation of the German KA5 (Bodenkundliche
# Kartieranleitung, 5th ed., 2005): the catalogue (inst/rules/ka5/horizons.yaml),
# the reader (R/designation-ka5.R), the recognition of the notation (Hugo's
# decisions Q3, Q4), the formula dialect of Liste 31 and the subtypes, the new
# attributes and the readers of the KA5 field codes. Nothing is classified yet.

.k1 <- function(d, ...) soilKey:::.ka5_parse_one(d, ...)
.kn <- function(d, site = NULL) soilKey:::.ka5_notation(d, site)
.deg <- "\u00b0"
.has_note <- function(notes, pat) any(grepl(pat, notes, fixed = TRUE))

test_that("the catalogue: 15 masters, 23 prefixes, 166 defined symbols, Tab. 12", {
  skip_on_cran()
  ct <- soilKey:::.ka5_catalogue()
  v <- soilKey:::.ka5_vocabulary()
  expect_setequal(v$masters, c("F", "H", "L", "O", "A", "B", "C", "P", "T", "S", "G", "M", "E", "R", "Y"))
  expect_length(ct$prefixes, 23L)
  expect_true(all(c("tb", "tm", "tp") %in% v$prefixes))
  expect_identical(nrow(v$symbols), 166L)
  expect_false(anyDuplicated(v$symbols$code) > 0L)
  per <- table(v$symbols$master)
  expect_identical(as.integer(per[c("F", "H", "L", "O", "A", "B", "C", "P", "T", "S", "G", "M", "E", "R", "Y")]),
                   c(6L, 27L, 1L, 4L, 26L, 35L, 12L, 4L, 4L, 17L, 21L, 2L, 2L, 2L, 3L))
  expect_true(all(v$symbols$page >= 88L & v$symbols$page <= 108L))
  expect_setequal(names(ct$combinations), v$masters)
  # no YAML 1.1 boolean slipped in (Y, N, y, n unquoted)
  lg <- function(x) if (is.list(x)) unlist(lapply(x, lg)) else is.logical(x)
  expect_false(any(lg(ct)))
  # every symbol 5.6.3.3 defines or names reads, and the defined ones are known as such
  for (cd in v$symbols$code) {
    r <- .k1(cd)
    expect_true(r$known, info = cd)
    expect_true(r$defined, info = cd)
  }
  for (cd in v$named) expect_true(.k1(cd)$known, info = cd)
  # the suffix letters by host (p. 85)
  expect_setequal(v$suffix_letters$S, c("c", "d", "e", "g", "q", "r", "s", "w"))
  expect_setequal(v$suffix_letters$G, c("c", "h", "k", "m", "o", "r", "s", "w", "z"))
  expect_identical(v$suffix_letters$P, character(0))
})

test_that("the last part carries the horizon: the parse examples of the plan", {
  skip_on_cran()
  r <- .k1(paste0("fAh", .deg, "Sd"))
  expect_identical(r$code, "Sd")
  expect_identical(r$with, "fAh")
  expect_true(r$overprint)
  expect_false(r$fossil)
  r <- .k1("tmzeGo-Ah")
  expect_identical(r$code, "Ah")
  expect_identical(r$with, "tmzeGo")
  expect_identical(r$joiners, "-")
  expect_identical(.k1("Bbt+Bv")$code, "Bv")
  expect_identical(.k1("Bv+Ah")$code, "Ah")
  expect_identical(.k1("Ah-Bv")$code, "Bv")
  expect_identical(.k1("Bsv")$code, "Bsv")
  r <- .k1("B(h)s")
  expect_identical(r$code, "Bs")
  expect_identical(r$options, c("Bs", "Bhs"))
  r <- .k1("Ah<")
  expect_identical(r$code, "Ah")
  expect_identical(r$bs_mark, "<")
  r <- .k1(paste0("IIfAh", .deg, "Sd"))
  expect_identical(r$layer, 2L)
  expect_identical(r$code, "Sd")
  r <- .k1("aelC")
  expect_identical(r$code, "C")
  expect_identical(r$prefixes, c("a", "e", "l"))
  r <- .k1("Acxh+elCc")
  expect_identical(r$code, "Cc")
  expect_identical(r$prefixes, c("e", "l"))
  expect_identical(r$with, "Acxh")
  r <- .k1("xC+O(v)h")
  expect_identical(r$code, "Oh")
  expect_identical(r$options, c("Oh", "Ovh"))
  r <- .k1("fAh")
  expect_true(r$fossil)
  expect_true(.k1("rGo")$relict)
  expect_identical(.k1("Bv2")$digits, "2")
  expect_identical(.k1("Bv2")$code, "Bv")
})

test_that("prefixes before the master, suffixes after it, by host", {
  skip_on_cran()
  # cC carbonate rock vs Cc secondary carbonate; xC stony vs Axh; lC vs Al
  expect_identical(.k1("cC")$prefixes, "c")
  expect_identical(.k1("Cc")$suffixes, "c")
  expect_identical(.k1("xC")$prefixes, "x")
  expect_identical(.k1("Axh")$suffixes, c("x", "h"))
  expect_identical(.k1("oG")$prefixes, "o")
  expect_identical(.k1("Go")$code, "Go")
  expect_identical(.k1("aM")$prefixes, "a")
  expect_identical(.k1("Aa")$code, "Aa")
  expect_identical(.k1("tbGr")$prefixes, "tb")
  # a suffix the master does not take is not read
  expect_false(.k1("Bw")$known)
  expect_false(.k1("Pv", ka6 = FALSE)$known)
  # a prefix on a master its list does not name: read, with a note
  r <- .k1("hG")
  expect_true(r$known)
  expect_true(.has_note(r$notes, "is not written on G"))
  # a combination outside Tab. 12: read by the grammar, with a note
  r <- .k1("Bhst")
  expect_true(r$known)
  expect_false(r$permitted)
  expect_true(.has_note(r$notes, "not a KA5 combination"))
  expect_true(.k1("Gkso")$permitted)
})

test_that("joiners, numerals and the typing slips (defaults 11-13)", {
  skip_on_cran()
  r <- .k1(paste0("fGo-", .deg, "Sd"))
  expect_identical(r$joiners, .deg)
  expect_true(.has_note(r$notes, "read as"))
  # the degree sign typed o, only when the result reads
  r <- .k1("fAhoSd", typed_o = TRUE)
  expect_identical(r$code, "Sd")
  expect_true(r$overprint)
  expect_identical(.k1("Gro")$code, "Gro")
  # the masculine ordinal and the ring for the degree sign
  expect_identical(.k1("fAh\u00baSd")$code, "Sd")
  # a capital I before C is the prefix l; AI is Al
  expect_identical(.k1("ICv")$prefixes, "l")
  expect_identical(.k1("ICv")$code, "Cv")
  expect_identical(.k1("AI")$code, "Al")
  expect_identical(.k1("IIC")$layer, 2L)
  r <- .k1("IIIC")
  expect_identical(r$layer, 3L)
  expect_true(.has_note(r$notes, "typed lC"))
  expect_identical(.k1("II Bv")$layer, 2L)
  # field codes are features, never horizons
  for (fc in c("Ck", "Oe", "Ost")) {
    r <- .k1(fc)
    expect_false(r$known, info = fc)
    expect_true(.has_note(r$notes, "field code"), info = fc)
  }
  for (x in c(NA, "", "-", "?")) expect_false(.k1(x)$known)
})

test_that("KA6 symbols: the one-to-one ones read with a flag, the others not (Q4)", {
  skip_on_cran()
  ka6 <- c(Ol = "L", Ax = "Axh", Ee = "Ae", El = "Al", Eal = "Ael", Ks = "Bs", Kh = "Bh", Kt = "Bt",
           Khs = "Bhs", Dj = "M", De = "E", Pv = "P")
  for (k in names(ka6)) {
    r <- .k1(k)
    expect_identical(r$code, ka6[[k]], info = k)
    expect_identical(r$ka6, k, info = k)
    expect_true(.has_note(r$notes, "a KA6 symbol"), info = k)
  }
  r <- .k1("Mm")
  expect_identical(r$code, "M")
  expect_identical(r$prefixes, "a")
  for (k in c("Nv", "Vf", "Uw", "Wt", "Hn", "Au")) {
    r <- .k1(k)
    expect_false(r$known, info = k)
  }
  expect_true(.has_note(.k1("Hn")$notes, "no KA5 reading"))
  # a profile declared ka6
  n <- .kn(c("Ol", "Ah", "Ks", "Cv"), list(designation_system = "KA6"))
  expect_true(n$ok)
  expect_identical(n$status, "declared_ka6")
  d <- soilKey:::.ka5_designations(c("Ol", "Ah", "Ks", "Cv"), list(designation_system = "ka6"))
  expect_identical(d$code, c("L", "Ah", "Bs", "Cv"))
})

test_that("declared notations: ka5, ka4, ka3, kartieranleitung, bodensystematik, agboden, dbg", {
  skip_on_cran()
  for (ds in c("ka5", "KA5", "KA 5", "ka4", "KA3", "Kartieranleitung", "Bodenkundliche Kartieranleitung",
               "Bodensystematik", "AG Boden", "Ad-hoc-AG Boden", "DBG")) {
    expect_true(.kn(c("Ah", "Go"), list(designation_system = ds))$ok, info = ds)
    expect_true(soilKey:::.ka5_is_declared(ds), info = ds)
  }
  n <- .kn(c("Ah", "Go", "Gr"), list(designation_system = "ka4"))
  expect_identical(n$edition, "KA4")
  expect_match(n$why, "redefined in 2005")
  d <- soilKey:::.ka5_designations(c("Ah", "Go", "Gr"), list(designation_system = "ka4"))
  expect_true(.has_note(d$notes[[2]], "redefined in 2005"))
  for (ds in c("wrb", "fao", "rp2008", "kidpr", "cpcs", "cssc", "asc")) {
    n <- .kn(c("Ah", "Sw", "Sd"), list(designation_system = ds))
    expect_false(n$ok, info = ds)
    expect_identical(n$status, "other", info = ds)
  }
  expect_false(soilKey:::.ka5_is_declared("kidpr"))
  expect_identical(.kn(c("Ah", "Sw"), list(designation_system = "NA"))$status, "recognised")
})

test_that("an undeclared profile: one strong sign or two likely kinds (Q3)", {
  skip_on_cran()
  expect_true(.kn(c("Ah", "Sw", "IISd"))$ok)
  expect_true(.kn(c("Ahe", "Ae", "Bhs", "Bs", "ilC"))$ok)
  expect_true(.kn(c("Ah<", "imC"))$ok)
  expect_true(.kn(c("Ah", paste0("IIfAh", .deg, "Sd")))$ok)
  expect_true(.kn(c("nHv", "nHt", "nHw", "nHr"))$ok)
  expect_true(.kn(c("Ah", "Bv", "Cv"))$ok)
  n <- .kn(c("Ah", "Bv", "C"))
  expect_false(n$ok)
  expect_match(n$why, "one kind")
  # E under an Ah over II, M over II: likely
  expect_true(.kn(c("Ah", "E", "IIfAh", "IIBv"))$ok)
  expect_true(.kn(c("Ah", "M", "IIBv"))$ok)
  # shared symbols are no sign; a profile in capitals has lost its case
  expect_false(.kn(c("Ah", "Go", "Gr"))$ok)
  expect_false(.kn(c("AH", "BV", "CV"))$ok)
  expect_false(.kn(c("Ah", "Bt", "C"))$ok)
  expect_false(.kn(character(0))$ok)
})

test_that("FAO, RP, CPCS, Russian and Canadian profiles are not read as KA5", {
  skip_on_cran()
  cases <- list(fao = list(c("Ap", "Bw1", "Bw2", "C"), "fao"),
                fao2 = list(c("A", "E", "Bt", "2Bt", "C"), "fao"),
                fao3 = list(c("Ah", "AB", "Bt", "BC", "C"), "fao"),
                usda = list(c("Oi", "Oe", "A", "Bk", "Ck"), "fao"),
                rp = list(c("LA", "S", "C"), "rp"),
                rp2 = list(c("OL", "OF", "OH", "A", "Sal", "C"), "rp"),
                cpcs = list(c("A00", "A1", "(B)", "C"), "cpcs"),
                cpcs2 = list(c("A1", "A2", "B2fe", "C"), "cpcs"),
                ru = list(c("AY", "EL", "BEL", "BT", "C"), "russian"),
                ru2 = list(c("PU", "BMK", "C"), "russian"))
  for (k in names(cases)) {
    n <- .kn(cases[[k]][[1]])
    expect_false(n$ok, info = k)
    expect_identical(n$status, cases[[k]][[2]], info = k)
    d <- soilKey:::.ka5_designations(cases[[k]][[1]])
    expect_true(all(is.na(d$code)), info = k)
  }
  # the other readers still read their own profiles, not KA5 ones
  expect_true(soilKey:::.rp_notation(c("LA", "S", "C"))$ok)
  expect_true(soilKey:::.kidpr_notation(c("AY", "EL", "BEL", "BT", "C"), list())$ok)
  expect_false(isTRUE(soilKey:::.rp_notation(c("Ah", "Sw", "IISd"))$ok))
  expect_false(soilKey:::.kidpr_notation(c("Ah", "Sw", "IISd"), list())$ok)
})

test_that("mixed profiles are undecided: nothing read as KA5", {
  skip_on_cran()
  mixed <- list(fao = c("Ah", "Sw", "Bw", "C"), rp = c("OL", "Ah", "Sw", "Sd"),
                cpcs = c("A00", "Ah", "Bv", "lC"), ru = c("AY", "Bv", "fAh"),
                cssc = c("LFH", "Ahe", "Ae", "Bf", "BC", "C"))
  for (k in names(mixed)) {
    n <- .kn(mixed[[k]])
    expect_true(is.na(n$ok), info = k)
    expect_identical(n$status, "mixed", info = k)
    expect_match(n$why, "undecided notation", info = k)
    d <- soilKey:::.ka5_designations(mixed[[k]])
    expect_true(is.na(d$ka5), info = k)
    expect_true(all(is.na(d$code)), info = k)
    expect_true(any(!is.na(d$read_as_ka5)), info = k)
  }
  expect_true(.has_note(soilKey:::.ka5_designations(mixed$fao)$notes[[2]], "may not be in"))
  # Ach is both an RP code and a KA5 symbol: no veto
  expect_true(.kn(c("Ach", "Sw", "Sd"))$ok)
  # a KA6 symbol is neither a sign nor a veto
  expect_false(.kn(c("Ol", "Ah", "Ks", "Kt", "Cv"))$ok)
  expect_identical(.kn(c("Ol", "Ah", "Ks", "Kt", "Cv"))$status, "none")
  d <- soilKey:::.ka5_designations(c("Ah", "Ks", "Sw", "Sd"))
  expect_true(d$ka5)
  expect_identical(d$code[2], "Bs")
  expect_true(.has_note(d$notes[[2]], "a KA6 symbol in a KA5 profile"))
})

test_that("every formula of Liste 31 and of the 223 subtypes parses with no unknown token", {
  skip_on_cran()
  fx <- utils::read.delim(testthat::test_path("fixtures", "ka5-formulas.tsv"), comment.char = "#",
                          quote = "", colClasses = "character", encoding = "UTF-8")
  expect_named(fx, c("class", "type", "unit", "kind", "page", "formula", "note"))
  expect_identical(length(unique(fx$unit[fx$kind == "type"])), 56L)
  # the Dwogmarsch twice: Liste 31 and its type text (p. 252)
  expect_setequal(fx$formula[fx$unit == "MD" & fx$kind == "type"],
                  c(paste0("Ah/Go-Sw/IIfAh", .deg, "Sd/fGo-", .deg, "Sd/Go/Gr"),
                    paste0("Ah/Sw/IIfAh", .deg, "Sd/fGo", .deg, "Sd/Go/Gr")))
  expect_identical(length(unique(fx$unit[fx$kind == "subtype"])), 223L)
  expect_identical(length(unique(fx$class[fx$kind == "type"])), 21L)
  st <- fx[fx$kind == "subtype" & !duplicated(fx$unit), ]
  expect_identical(nrow(st[st$class %in% c("F", "O", "R", "T", "D", "B", "L", "P", "C", "V", "S", "X", "Y"), ]), 138L)
  expect_identical(nrow(st[!(st$class %in% c("F", "O", "R", "T", "D", "B", "L", "P", "C", "V", "S", "X", "Y")), ]), 85L)
  expect_true(all(as.integer(fx$page) >= 199L & as.integer(fx$page) <= 262L))
  bad <- character(0)
  for (k in seq_len(nrow(fx))) {
    sp <- soilKey:::.ka5_parse_formula(fx$formula[k])
    expect_s3_class(sp, "soilkey_formula")
    if (length(sp$unknown)) bad <- c(bad, sprintf("%s %s: %s", fx$unit[k], fx$formula[k],
                                                  paste(sp$unknown, collapse = ", ")))
  }
  expect_identical(bad, character(0))
})

test_that("the formula object: slots, alternatives, optional groups, open ends", {
  skip_on_cran()
  p <- soilKey:::.ka5_parse_formula
  sp <- p("Ah/(Go/)Gso(/Gr)")
  expect_identical(vapply(sp$slots, function(z) z$codes, ""), c("Ah", "Go", "Gso", "Gr"))
  expect_identical(vapply(sp$slots, function(z) z$optional, NA), c(FALSE, TRUE, FALSE, TRUE))
  expect_identical(unname(sp$specificity), c(2, 0))
  sp <- p("aAxh/(aM,alC/)aG")
  expect_identical(sp$slots[[2]]$codes, c("M", "C"))
  expect_true(sp$slots[[2]]$optional)
  expect_identical(sp$slots[[2]]$forms[[2]]$required, c("pre:a", "pre:l"))
  # transitional parts and prefixes are required features of the carrying code
  sp <- p("Axh/Axh+lC(c)/C(kc,c)")
  expect_identical(sp$slots[[2]]$codes, c("C", "Cc"))
  expect_identical(sp$slots[[2]]$forms[[1]]$required, c("pre:l", "Axh+"))
  expect_identical(sp$slots[[3]]$codes, c("C", "Ckc", "Cc"))
  # alternatives of prefixes and numerals
  sp <- p(paste0("tp,tbAh/tp,tbSw/IIftp,tbAh", .deg, "Sd/ftp,tbGo", .deg, "Sd/tp,tbGo/tp,tbGr"))
  expect_identical(vapply(sp$slots[[3]]$forms, function(f) f$written, ""),
                   paste0(c("IIftpAh", "IIftbAh"), .deg, "Sd"))
  expect_identical(vapply(sp$slots[[3]]$forms, function(f) f$lith, ""), c("II", "II"))
  sp <- p(".../IIr,fBj/Cj/Cv")
  expect_true(sp$open_head)
  expect_identical(sp$slots[[1]]$kind, "open")
  expect_identical(vapply(sp$slots[[2]]$forms, function(f) f$written, ""), c("IIrBj", "IIfBj"))
  sp <- p("hH/(IInH/)(II,IIIfF/)...")
  expect_identical(vapply(sp$slots[[3]]$forms, function(f) f$lith, ""), c("II", "III"))
  expect_true(sp$open_tail)
  sp <- p("Ah/M/II...")
  expect_identical(sp$slots[[3]]$kind, "buried")
  expect_identical(sp$slots[[3]]$codes, "[ABC]")
  expect_true(sp$slots[[3]]$optional)
  sp <- p("Ah/M/(Sw-M/)(M-)Sw/II(Sw/)Sd")
  expect_identical(vapply(sp$slots, function(z) z$forms[[1]]$written, ""),
                   c("Ah", "M", "Sw-M", "Sw", "IISw", "IISd"))
  sp <- p("aAh/(II)ailC/II(III)aM/(ailC/)aG")
  expect_identical(vapply(sp$slots[[3]]$forms, function(f) f$lith, ""), c("II", "III"))
  sp <- p("a(Sw-)Ah/aSw-M/a(M-)Sd/aG")
  expect_identical(sp$slots[[1]]$forms[[2]]$required, c("pre:a", "Sw-", "Sw-pre:a"))
  sp <- p("R-Ap/R+.../...")
  expect_identical(sp$slots[[2]]$codes, "...")
  expect_identical(sp$slots[[2]]$forms[[1]]$required, "R+")
  # the one-to-one reading of a slot table
  tab <- soilKey:::.formula_slots_table(p("(P-)Ah/P/i,eC"))
  expect_identical(tab$codes, c("Ah", "P", "C"))
  expect_identical(tab$role, c("surface", "middle", "middle"))
  expect_error(p(""))
  expect_length(p("Ah/(Go")$unknown, 1L)
})

test_that("round trip: every formula written back reads the same, and its own profile matches it", {
  skip_on_cran()
  fx <- utils::read.delim(testthat::test_path("fixtures", "ka5-formulas.tsv"), comment.char = "#",
                          quote = "", colClasses = "character", encoding = "UTF-8")
  sig <- function(sp) lapply(sp$slots, function(z) list(z$kind, z$optional, sort(vapply(z$forms, function(f)
    paste(f$code, paste(sort(f$required), collapse = ","), f$lith), ""))))
  bad_rt <- bad_match <- character(0)
  n_buried <- 0L
  for (k in seq_len(nrow(fx))) {
    sp <- soilKey:::.ka5_parse_formula(fx$formula[k])
    sp2 <- soilKey:::.ka5_parse_formula(soilKey:::.ka5_format_formula(sp))
    if (!identical(sig(sp), sig(sp2))) bad_rt <- c(bad_rt, fx$formula[k])
    sl <- Filter(function(z) z$kind == "horizon" && !z$optional, sp$slots)
    d <- character(0)
    for (z in sl) {
      w <- vapply(z$forms, function(f) f$written, "")
      d <- c(d, if (length(d) && w[1] == d[length(d)] && length(w) > 1L) w[2] else w[1])
    }
    # the optional buried soil of II... built too (the review's D3)
    bu <- Filter(function(z) z$kind == "buried", sp$slots)
    if (length(bu)) {
      lith <- bu[[1]]$forms[[1]]$lith
      d <- c(d, paste0(lith, "fAh"), paste0(lith, "Bv"))
    }
    n <- length(d)
    fr <- soilKey:::.ka5_horizon_frame(d, site = list(designation_system = "ka5"),
                                       top_cm = (seq_len(n) - 1) * 20, bottom_cm = seq_len(n) * 20)
    m <- match_horizon_sequence(fr, sp)
    if (!isTRUE(m$passed)) bad_match <- c(bad_match, fx$formula[k])
    if (length(bu) && isTRUE(m$passed)) n_buried <- n_buried + 1L
  }
  expect_identical(bad_rt, character(0))
  expect_identical(bad_match, character(0))
  expect_gte(n_buried, 8L)
})

test_that("the frame: carrying code, overlay, features; matching and windows", {
  skip_on_cran()
  fr <- soilKey:::.ka5_horizon_frame(c("Ah", "Bv-Ah", "Bv", "Go-Bv", "IIGo", "IIGr"),
                                     site = list(designation_system = "ka5"),
                                     top_cm = c(0, 10, 20, 40, 55, 90), bottom_cm = c(10, 20, 40, 55, 90, 150))
  expect_identical(fr$carrying, c("Ah", "Ah", "Bv", "Bv", "Go", "Gr"))
  expect_identical(fr$code, c("Ah", "Bv-Ah", "Bv", "Go-Bv", "Go", "Gr"))
  expect_identical(fr$overlay[[2]], c("Ah", "A"))
  expect_identical(fr$features[[4]], "Go-")
  expect_identical(fr$layer, c(1L, 1L, 1L, 1L, 2L, 2L))
  # the Gley-Braunerde: Ah/(Go-)Bv/Bv-Go,Go/Gr, the Go at 4 to < 8 dm
  gb <- soilKey:::.ka5_horizon_frame(c("Ah", "Go-Bv", "Go", "Gr"), site = list(designation_system = "ka5"),
                                     top_cm = c(0, 20, 45, 90), bottom_cm = c(20, 45, 90, 150))
  sp <- soilKey:::.ka5_parse_formula("Ah/(Go-)Bv/Bv-Go,Go/Gr")
  expect_true(match_horizon_sequence(gb, sp)$passed)
  expect_true(soilKey:::.ka5_window(gb, "G", 40, 80))
  # a master written without suffix stands for its combinations (p. 199)
  expect_true(match_horizon_sequence(gb, soilKey:::.ka5_parse_formula("Ah/Bv/G/Gr"))$passed)
  # Bv-Ah over Ah stays a horizon of its own: Ah/Bv-Ah,Bv+Ah/... is not Ah/Bv/...
  expect_false(isTRUE(match_horizon_sequence(gb, soilKey:::.ka5_parse_formula("Ah/Bv-Ah/Go/Gr"))$passed))
  # the last symbol carries the horizon (Q8): Go-Bv is a Bv, not a G
  expect_true(soilKey:::.ka5_window(fr, "G", 40, 80))
  expect_false(soilKey:::.ka5_window(fr, "Gr", 40, 80))
  expect_true(is.na(soilKey:::.ka5_window(fr[1:3, ], "G", 40, 80)))
  expect_false(soilKey:::.ka5_window(fr, "Sd", 40, 80))
  # in another notation nothing is read
  fr2 <- soilKey:::.ka5_horizon_frame(c("Ap", "Bw", "C"))
  expect_true(all(is.na(fr2$code)))
})

test_that("the KA5 field codes read into ranges (Q24)", {
  skip_on_cran()
  expect_identical(soilKey:::.ka5_humus_class("h4"), c(4, 8))
  expect_identical(soilKey:::.ka5_humus_class("H7"), c(30, 100))
  expect_true(all(is.na(soilKey:::.ka5_humus_class("h9"))))
  expect_identical(soilKey:::.ka5_carbonate_class("c3.3"), c(4, 7))
  expect_identical(soilKey:::.ka5_carbonate_class("c7"), c(75, 100))
  h <- soilKey:::.ka5_hydromorphic_features("ed,fl,g3,f4;rb,f6;rg,f2")
  expect_identical(h$rust, c(5, 10))
  expect_identical(h$bleached, c(30, 50))
  expect_identical(h$reduction, c(1, 2))
  h <- soilKey:::.ka5_hydromorphic_features("ed+eh,fl,f5;eo,f4;mr,g2")
  expect_identical(h$rust, c(15, 40))
  h <- soilKey:::.ka5_hydromorphic_features("rb")
  expect_identical(h$bleached, c(0, 100))
  expect_true(.has_note(h$notes, "no area class"))
  expect_identical(soilKey:::.ka5_texture_class("Tu4"), "T")
  expect_identical(soilKey:::.ka5_texture_class("Slu"), "L")
  expect_identical(soilKey:::.ka5_texture_class("Ss"), "S")
  expect_identical(soilKey:::.ka5_texture_class("Lu"), "U")
  expect_identical(soilKey:::.ka5_texture_class("Lt3"), "T")
  expect_identical(soilKey:::.ka5_texture_class("Ts4"), "L")
  expect_true(is.na(soilKey:::.ka5_texture_class("SiL")))
  cw <- soilKey:::.ka5_colour_words("roligrbn")
  expect_identical(cw$words, c("roli", "gr", "bn"))
  expect_identical(cw$main, "bn")
  expect_identical(soilKey:::.ka5_colour_words("dbn")$main, "bn")
  expect_null(soilKey:::.ka5_colour_words("7.5YR"))
  expect_identical(soilKey:::.ka5_humus_form("Rohhumus"), "mor")
  expect_identical(soilKey:::.ka5_humus_form("Feuchtmull"), "hydromull")
  expect_identical(soilKey:::.ka5_humus_form("F-Mull"), "f-mull")
  expect_true(is.na(soilKey:::.ka5_humus_form("Rendzina")))
})

test_that("the new attributes are documented in the schemas, naming the KA5", {
  skip_on_cran()
  a <- horizon_attribute_spec()
  hz <- c("bleached_pct", "reduction_colours_pct", "rh_value", "air_capacity_pct", "total_porosity_pct",
          "muscovite_pct", "clay_coatings_thin_section_pct", "clay_bridges", "bands_area_pct", "kultotroph",
          "carbonate_whole_soil_pct", "rock_diggable", "parent_rock_ka5", "caco3_lithogenic",
          "oc_lithogenic_pct", "knick_degree", "sand_ffs_pct", "soil_gas_ch4_co2_pct", "horizon_continuity",
          "terra_material_pct", "ec_soil_solution_dS_m", "humus_class_ka5", "carbonate_class_ka5",
          "texture_class_ka5", "colour_ka5", "hydromorphic_features_ka5")
  expect_length(hz, 26L)
  expect_true(all(hz %in% a$name))
  expect_true(all(grepl("KA5", a$description[a$name %in% hz])))
  expect_identical(a$type[a$name == "clay_bridges"], "logical")
  expect_match(a$description[a$name == "layer_origin"], "tidal-brackish")
  s <- site_attribute_spec()
  st <- c("particle_size_scheme", "embanked", "groundwater_source", "groundwater_oxygen_rich",
          "groundwater_mhgw_cm", "groundwater_mgw_cm", "groundwater_mngw_cm", "groundwater_step",
          "vegetation_cover", "erosion_degree", "rewetted", "reductgas_source", "deep_cultivation_kind")
  expect_length(st, 13L)
  expect_true(all(st %in% s$name))
  ds <- s$allowed[s$name == "designation_system"]
  for (v in c("ka5", "ka4", "ka3", "ka6", "kartieranleitung", "bodensystematik", "agboden", "dbg"))
    expect_match(ds, v, fixed = TRUE)
  expect_match(s$allowed[s$name == "humus_form"], "f-mull", fixed = TRUE)
  rd <- yaml::read_yaml(system.file("rules", "ka5", "readings.yaml", package = "soilKey"))$readings
  expect_true(length(rd) >= 10L)
  expect_true(any(grepl("(Hugo, 2026-11-04)", vapply(rd, function(e) e$reading, ""))))
})

test_that("the KA5 sources are ASCII", {
  skip_on_cran()
  src <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(src), "package sources not available")
  for (f in c("designation-ka5.R", "ka5-field-codes.R")) {
    x <- readLines(file.path(src, f), warn = FALSE, encoding = "UTF-8")
    expect_false(any(grepl("[^\\x01-\\x7F]", x, perl = TRUE)), info = f)
  }
})


# --- the review of 2026-11-04 and Hugo's decisions ---------------------------------

.mt <- function(d, f, ds = "ka5") {
  n <- length(d)
  fr <- soilKey:::.ka5_horizon_frame(d, site = list(designation_system = ds),
                                     top_cm = (seq_len(n) - 1) * 20, bottom_cm = seq_len(n) * 20)
  match_horizon_sequence(fr, soilKey:::.ka5_parse_formula(f))$passed
}

test_that("reading 1: a geogenic prefix written in front holds for the whole horizon", {
  skip_on_cran()
  r <- .k1("aSw-Ah")
  expect_identical(r$code, "Ah")
  expect_identical(r$prefixes, "a")
  expect_identical(r$prefixes_inherited, "a")
  expect_identical(.k1("tmzeGo-Ah")$prefixes, c("tm", "z", "e"))
  expect_identical(.k1("aM-Sd")$prefixes, "a")
  # f and r and the rock prefixes do not pass
  r <- .k1(paste0("fAh", .deg, "Sd"))
  expect_identical(r$prefixes, character(0))
  expect_false(r$fossil)
  expect_identical(.k1("rGo-Bv")$prefixes, character(0))
  expect_identical(.k1("ilCv-P")$prefixes, character(0))
  expect_identical(.k1("elCv-P")$prefixes, "e")
  # the Pseudogley-Vega reads by its own formula
  expect_true(.mt(c("aSw-Ah", "aSw-M", "aM-Sd", "aG"), "a(Sw-)Ah/aSw-M/a(M-)Sd/aG"))
  expect_true(.mt(c("aAh", "aSw-M", "aSd", "aG"), "a(Sw-)Ah/aSw-M/a(M-)Sd/aG"))
  expect_true(.mt(c("tmzeGo-Ah", "tmzeGo", "tmzeGr"), "tm(z)eGo-Ah/(tmzeGo/)tmzeGr"))
  sp <- soilKey:::.ka5_parse_formula("a(Sw-)Ah/aSw-M/a(M-)Sd/aG")
  expect_identical(vapply(sp$slots[[1]]$forms, function(f) f$written, ""), c("aAh", "aSw-Ah"))
})

test_that("the strong signs are exactly the decision card's (Q3)", {
  skip_on_cran()
  v <- soilKey:::.ka5_vocabulary()
  expect_setequal(v$strong, c("Sw", "Sd", "Srw", "Sew", "Sq", "Tv", "Tu", "Axh", "Ael", "Ahe", "Bfv",
                              "Bsv", "Bbt", "Gor", "Gso", "Ex"))
  for (x in c("Btv", "Acxh", "Srd", "Sdg", "Gro", "Gkso", "Gmso", "Bbms", "Bhsv", "Aih", "Aeh")) {
    expect_false(x %in% v$strong, info = x)
  }
  expect_false(.kn(c("Ap", "Bt", "Btv", "C"))$ok)
  expect_false(.kn(c("Ap", "E", "Bt1", "Bt2", "Btv1", "Btv2", "C"))$ok)
  expect_true(.kn(c("Ap", "Bt", "Bbt", "C"))$ok)
  rd <- yaml::read_yaml(system.file("rules", "ka5", "readings.yaml", package = "soilKey"))$readings
  card <- vapply(rd, function(e) e$reading, "")[vapply(rd, function(e) e$topic, "") == "Recognising the notation"]
  expect_match(card, "Sw, Sd, Srw, Sew, Sq, Tv, Tu, Axh, Ael, Ahe, Bfv, Bsv, Bbt, Gor, Gso, Ex)", fixed = TRUE)
})

test_that("the Canadian j forms and Ck, Oe written as designations veto the KA5", {
  skip_on_cran()
  for (x in c("Btj", "Bmj", "Aej", "Ckj", "Btgj", "Ck", "Oe")) {
    expect_true(soilKey:::.ka5_fao_only(x), info = x)
  }
  for (x in c("Bj", "Cj", "Bv", "Sw")) expect_false(soilKey:::.ka5_fao_only(x), info = x)
  expect_false(isTRUE(.kn(c("Ahe", "Ae", "Bt", "Ck"))$ok))
  expect_false(isTRUE(.kn(c("Ap", "Ahe", "Ae", "Btj", "Bt", "Ck"))$ok))
  expect_true(is.na(.kn(c("Ahe", "Ae", "Bt", "Ck"))$ok))
  # Ahe stays a strong sign
  expect_true(.kn(c("Ahe", "Ae", "Bhs", "Bs", "C"))$ok)
  # in a declared KA5 profile Ck is a field code, not read
  d <- soilKey:::.ka5_designations(c("Ah", "Bv", "Ck"), list(designation_system = "ka5"))
  expect_true(is.na(d$code[3]))
})

test_that("reading 8: IIIC is II and lC with no layer II above, III and C with one", {
  skip_on_cran()
  d <- soilKey:::.ka5_designations(c("Ah", "Bv", "IIIC"), list(designation_system = "ka5"))
  expect_identical(d$layer[3], 2L)
  expect_identical(d$prefixes[[3]], "l")
  expect_true(.has_note(d$notes[[3]], "no layer II lying above"))
  d <- soilKey:::.ka5_designations(c("Ah", "IIBv", "IIIC"), list(designation_system = "ka5"))
  expect_identical(d$layer[3], 3L)
  expect_identical(d$prefixes[[3]], character(0))
  expect_true(.has_note(d$notes[[3]], "a layer II lies above"))
})

test_that("D1: a bare master or a comma slot takes a run; transitions merge into their neighbour", {
  skip_on_cran()
  repro <- list(c("Ah/Bv/Cv/Cn", "Ah/Bv/C"), c("Ah/cCv/cCn", "Ah/cC"), c("Ah/Tv/cCv/cCn", "Ah/Tv/cC"),
                c("Ah/Al/Bt/Cv/Cn", "Ah/Al/Bt/C"), c("Ah/Al/Al+Bt/Bt/C", "Ah/Al/Bt/C"),
                c("Ah/Sw/Sw-Sd/Sd", "Ah/S(e)w/(II)Sd"), c("Ah/Bv/Go-Bv/Go/Gr", "Ah/(Go-)Bv/Bv-Go,Go/Gr"),
                c("aAh/aM/IIalC/IIaGo/IIaGr", "aAh/aM/(IIalC/)(II)aG"), c("L/Of/Oh/mC", "O/mC"),
                c("Ap/Ex/Ex-C/C", "Ap/Ex/(Ex-)C"),
                c("Ahe/Ae/Bsh/Bhs/Bs/Bv/Cv", "Ahe/Ae/Bsh,Bhs,Bs/Bv/Cv"))
  for (r in repro) expect_true(.mt(strsplit(r[1], "/")[[1]], r[2]), info = paste(r, collapse = " ~ "))
  # a transition the formula names stays a horizon of its own
  expect_true(.mt(c("Ah", "Ah-Go", "Go", "Gr"), "Ah/Ah-Go/(Go/)Gr"))
  expect_true(.mt(c("Ah", "Ael", "Ael+Bt", "Bt", "C"), "Ah/Ael/Ael+Bt/Bt/C"))
  expect_false(isTRUE(.mt(c("Ah", "Ael", "Bt", "C"), "Ah/Ael/Ael+Bt/Bt/C")))
  # a row another slot may take is not swallowed by a run
  expect_true(.mt(c("Ah", "Bv+cCv", "cC"), "Ah/Bv+cCv,Bv-cCv/cC"))
  expect_true(.mt(c("Ah", "Go", "Gr"), "Ah/Bv/G/Gr") %in% c(FALSE, NA))
  # the hook is the KA5 formulas' only: a Russian formula has none
  expect_null(soilKey:::.parse_profile_formula("AY-EL-BT-C")$prepare_table)
})

test_that("D2: an Ap takes a plain Ah slot (p. 199)", {
  skip_on_cran()
  expect_true(.mt(c("Ap", "Bv", "C"), "Ah/Bv/C"))
  expect_true(.mt(c("Ap", "Al", "Al+Bt", "Bt", "Bv", "elCc"), "Ah/Al/Bt/C"))
  expect_true(.mt(c("Ap<", "ilC"), "Ah</ilC"))
  expect_false(isTRUE(.mt(c("Ap", "Ae", "Bh", "Bs", "C"), "Ahe/Ae/B(s)h/B(h)s/C")))
})

test_that("D3: the buried soil of II... from the first fossil row of a layer II", {
  skip_on_cran()
  fr <- soilKey:::.ka5_horizon_frame(c("Ah", "M", "IIfAh", "IIBv"), site = list(designation_system = "ka5"),
                                     top_cm = c(0, 20, 50, 70), bottom_cm = c(20, 50, 70, 100))
  expect_identical(fr$buried, c(0L, 0L, 1L, 1L))
  expect_true(.mt(c("Ah", "M", "IIfAh", "IIBv"), "Ah/M/II..."))
  expect_true(.mt(c("Ah", "E", "IIfAh", "IIfBv"), "Ah/E/II..."))
  expect_true(.mt(c("Ah", "M"), "Ah/M/II..."))
  # a present horizon formed in a fossil one is no buried soil
  fr <- soilKey:::.ka5_horizon_frame(c("Ah", "Sw", paste0("IIfAh", .deg, "Sd"), "Gr"), site = list(designation_system = "ka5"))
  expect_identical(fr$buried, c(0L, 0L, 0L, 0L))
})

test_that("R6: a joined head written on the first alternative holds for the others", {
  skip_on_cran()
  sp <- soilKey:::.ka5_parse_formula("R-Ap/R+B(h)s,B(s)h/C")
  expect_identical(vapply(sp$slots[[2]]$forms, function(f) f$written, ""), c("R+Bs", "R+Bhs", "R+Bh", "R+Bsh"))
  expect_true(.mt(c("R-Ap", "R+Bsh", "C"), "R-Ap/R+B(h)s,B(s)h/C"))
  # not a transition head: Bv-Go,Go keeps its plain Go
  sp <- soilKey:::.ka5_parse_formula("Ah/(Go-)Bv/Bv-Go,Go/Gr")
  expect_identical(vapply(sp$slots[[3]]$forms, function(f) f$written, ""), c("Bv-Go", "Go"))
})

test_that("the smaller points of the review", {
  skip_on_cran()
  # declarations
  expect_identical(.kn(c("Ah", "Bv"), list(designation_system = "Bodenkundliche Kartieranleitung 6"))$status, "declared_ka6")
  expect_identical(.kn(c("Ah", "Bv"), list(designation_system = "Bodensystematik (KA6)"))$status, "declared_ka6")
  expect_identical(.kn(c("Ah", "Bv"), list(designation_system = "KA 6"))$status, "declared_ka6")
  expect_identical(.kn(c("Ah", "Bv"), list(designation_system = "dbg-wrb"))$status, "other")
  expect_identical(.kn(c("Ah", "Bv"), list(designation_system = "dbg 1998"))$status, "declared")
  expect_identical(.kn(c("Ah", "Bv"), list(designation_system = "KA5 (AG Boden 2005)"))$status, "declared")
  expect_identical(.kn(c("Ah", "Bv"), list(designation_system = "kanadisch"))$status, "other")
  # the typed o is read only in a KA5 profile and is never a sign
  expect_false(.k1("fAhoSd")$known)
  expect_false(.kn(c("fAhoSd", "Bv"))$ok)
  d <- soilKey:::.ka5_designations(c("Ah", "fAhoSd", "Bv"), list(designation_system = "ka5"))
  expect_identical(d$code[2], "Sd")
  d <- soilKey:::.ka5_designations(c("Ah", "Sw", "fAhoSd"))
  expect_true(d$ka5)
  expect_identical(d$code[3], "Sd")
  # (Bv) and B v
  r <- .k1("(Bv)")
  expect_identical(r$code, "Bv")
  expect_true(.has_note(r$notes, "parentheses"))
  expect_identical(.k1("B v")$code, "Bv")
  expect_false(.k1("(B)")$known)
  # pages, the forest humus classes, the reductive r
  ct <- soilKey:::.ka5_catalogue()
  pg <- vapply(ct$field_codes, function(e) as.integer(e$page), 1L)
  names(pg) <- vapply(ct$field_codes, function(e) e$code, "")
  expect_identical(unname(pg[c("Hu", "Vw", "Vr")]), c(114L, 114L, 114L))
  expect_true("Bt+Ael" %in% unlist(lapply(Filter(function(e) e$under == "Al", ct$named), function(e) e$codes)))
  expect_identical(soilKey:::.ka5_humus_class("h4", forest = TRUE), c(5, 10))
  expect_identical(soilKey:::.ka5_humus_class("h4"), c(4, 8))
  expect_identical(soilKey:::.ka5_hydromorphic_features("r,f5")$reduction, c(10, 30))
  # empty alternatives and horizons are unknown tokens
  expect_length(soilKey:::.ka5_parse_formula("Ah/Bv,,Cv")$unknown, 1L)
  expect_length(soilKey:::.ka5_parse_formula("Ah//Bv")$unknown, 1L)
  expect_length(soilKey:::.ka5_parse_formula("Ah,/Bv")$unknown, 1L)
})
