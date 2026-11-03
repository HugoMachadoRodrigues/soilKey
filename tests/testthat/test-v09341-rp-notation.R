# v0.9.341: the notation of the French Referentiel pedologique 2008 (pp. 1-5):
# the reader (R/designation-rp.R), the catalogue (inst/rules/rp2008/horizons.yaml),
# the recognition of the notation (Hugo's decision Q9), the hints of the CPCS
# and FAO designations, and the new attributes. Nothing is classified yet.

.rp1 <- function(d, ...) soilKey:::.rp_parse_one(d, ...)
.rpn <- function(d, site = NULL) soilKey:::.rp_notation(d, site)
.has_note <- function(notes, pat) any(grepl(pat, notes, fixed = TRUE))
.pi <- "\u03c0"
.ss <- "\u00df"

test_that("the catalogue: 75 listed codes, 4 layers, 8 materials, variants, marks", {
  skip_on_cran()
  v <- soilKey:::.rp_vocabulary()
  i <- v$info
  expect_false(anyDuplicated(i$code) > 0L)
  expect_identical(sum(i$source == "list" & i$kind == "horizon"), 75L)
  expect_setequal(i$code[i$source == "list" & i$kind == "layer"], c("R", "M", "D", "P"))
  expect_setequal(i$code[i$source == "list" & i$kind == "material"],
                  c("Mli", "Mt", "TH", "Z", "Zar", "ZO", "Ztc", "Ztr"))
  expect_setequal(i$code[i$source == "annex1"], c("OLn", "OLv", "OLt", "OFr", "OFm", "OHr", "OHf"))
  expect_setequal(i$code[i$source == "old"], c("Ahs", "FEm", "FEmp", "Ap"))
  expect_true(all(c("Fox", "Fhy", "OXal", "OXfl", "OXfr", "OXba", "OXti", "Gr", "Go", "Mma", "Marg")
                  %in% i$code[i$source == "variant"]))
  expect_true(all(i$detection %in% c("strong", "likely", "ambiguous")))
  expect_true(all(is.na(i$variant_of) | i$variant_of %in% i$code))
  expect_true(all(is.na(i$reads_as) | i$reads_as %in% i$code))
  expect_true(all(c(paste0("BT", .ss), "SV", "FE", "Fe", "OHta", "Kc") %in% i$code))
  expect_true(all(c("cry", "noz", "nod", "tp", "ho", "zo", "cn", "ca", "cra", "cri", "sch", "do",
                    "vo", "si", "a", "b", "c", "d", "g", "h", "j", "k", "m", "s", "t", "v", "x",
                    "y", "z", "*", "Na", "Sa") %in% v$marks))
  ct <- soilKey:::.rp_catalogue()
  for (p in ct$prefixes) for (ex in unlist(p$examples)) {
    r <- .rp1(ex)
    expect_true(r$known, info = ex)
    expect_true(p$mark %in% r$overlay, info = ex)
  }
})

test_that("every code of Annex 6 and of the FAO table reads, but the slips", {
  skip_on_cran()
  ct <- soilKey:::.rp_catalogue()
  slips <- unlist(lapply(ct$annex6, function(r) r$slips))
  expect_setequal(unique(slips), c("Ha", "BTfe", "FSj"))
  codes <- unique(c(unlist(lapply(ct$annex6, function(r) r$rp)),
                    unlist(lapply(ct$fao_hints, function(r) r$rp))))
  for (cd in setdiff(codes, "BTfe")) expect_true(.rp1(cd)$known, info = cd)
  expect_false(.rp1("BTfe")$known)
  expect_true(.has_note(.rp1("FSj")$notes, "not written on FS"))
})

test_that("every sequence of the 34 chapters and Annex 1 reads with no unknown code", {
  skip_on_cran()
  f <- testthat::test_path("fixtures", "rp2008-sequences.tsv")
  seqs <- utils::read.delim(f, quote = "", colClasses = "character", encoding = "UTF-8")
  expect_named(seqs, c("ger", "reference", "kind", "page", "sequence", "note"))
  expect_identical(nrow(seqs), 359L)
  expect_identical(length(unique(seqs$sequence)), 308L)
  ger <- c("alocrisols", "andosols", "anthroposols", "arenosols", "brunisols", "calcium_magnesium",
           "chernosols", "colluviosols", "cryosols", "ferrallitisols_oxydisols", "ferruginosols",
           "fersialsols", "fluviosols", "grisols", "gypsosols", "histosols", "leptismectisols",
           "lithosols", "luvisols", "nitosols", "organosols", "pelosols", "peyrosols", "phaeosols",
           "planosols", "podzosols", "rankosols", "reductisols_redoxisols", "regosols",
           "salisols_sodisols", "thalassosols", "thiosols_sulfatosols", "veracrisols", "vertisols")
  expect_length(ger, 34L)
  expect_true(all(seqs$ger %in% c(ger, "annex1")))
  # the colluviosols, peyrosols and regosols give their sequence only in words
  expect_setequal(setdiff(ger, seqs$ger), c("colluviosols", "peyrosols", "regosols"))
  expect_true("annex1" %in% seqs$ger)
  expect_true(all(as.integer(seqs$page) >= 69L & as.integer(seqs$page) <= 356L))
  bad <- character(0)
  for (k in seq_len(nrow(seqs))) {
    rd <- soilKey:::.rp_read_sequence(seqs$sequence[k])
    if (length(rd$unknown)) bad <- c(bad, sprintf("p. %s %s: %s", seqs$page[k], seqs$sequence[k],
                                                  paste(rd$unknown, collapse = ", ")))
  }
  expect_identical(bad, character(0))
  # the structure of a sequence: slots, alternatives, optional horizons
  rd <- soilKey:::.rp_read_sequence("(A|Ae|Anod)/ND/II F|II Fnod/(RT)")
  expect_length(rd$slots, 4L)
  expect_true(rd$slots[[1L]]$optional)
  expect_identical(vapply(rd$slots[[1L]]$alternatives, `[[`, "", "code"), c("A", "Ae", "A"))
  expect_identical(rd$slots[[1L]]$alternatives[[3L]]$features, "nod")
  expect_identical(vapply(rd$slots[[3L]]$alternatives, `[[`, 1L, "lith"), c(2L, 2L))
  expect_true(rd$slots[[4L]]$optional)
  rd <- soilKey:::.rp_read_sequence("OL/OFzo/OHzo|(OHzo)")
  expect_true(rd$slots[[3L]]$alternatives[[2L]]$optional)
  rd <- soilKey:::.rp_read_sequence("O//A")
  expect_length(rd$slots, 2L)
  expect_true(.has_note(rd$notes, "discontinuity"))
  rd <- soilKey:::.rp_read_sequence("H/-*")
  expect_true(rd$slots[[2L]]$alternatives[[1L]]$mark_alone)
  expect_identical(rd$slots[[2L]]$alternatives[[1L]]$features, "*")
  rd <- soilKey:::.rp_read_sequence("C|M|D/R|Km|FEm|Sim|OXc|OXm")
  expect_identical(rd$slots[[2L]]$alternatives[[3L]]$code, "Fem")
})

test_that("the parse examples of the plan", {
  skip_on_cran()
  r <- .rp1("LAch")
  expect_identical(c(r$code, r$overlay), c("Ach", "L"))
  expect_length(r$features, 0L)
  r <- .rp1("NaBTg2")
  expect_identical(c(r$code, r$overlay, r$features, r$subdiv), c("BT", "Na", "g", "2"))
  r <- .rp1("II Scag")
  expect_identical(c(r$code, r$features), c("Sca", "g"))
  expect_identical(r$lith, 2L)
  r <- .rp1("BTgd")
  expect_identical(c(r$code, r$features), c("BT", "g", "d"))
  r <- .rp1("E&BT")
  expect_true(r$juxtaposed)
  expect_true(is.na(r$code))
  expect_identical(r$parts, c("E", "BT"))
  r <- .rp1(paste0(.pi, "RT"))
  expect_identical(c(r$code, r$overlay), c("RT", .pi))
  r <- .rp1("OFrzo")
  expect_identical(c(r$code, r$base, r$family, r$features), c("OFr", "OF", "O", "zo"))
  r <- .rp1("OHnozc")
  expect_identical(c(r$code, r$features), c("OH", "noz", "c"))
  expect_match(r$meanings[2L], "mycogenic")
  r <- .rp1("Acaho")
  expect_identical(c(r$code, r$features), c("Aca", "ho"))
  r <- .rp1("JZtc")
  expect_identical(c(r$code, r$overlay, r$kind), c("Ztc", "J", "material"))
  r <- .rp1("A-S")
  expect_true(r$transition)
  expect_identical(r$parts, c("A", "S"))
})

test_that("the longest match and the codes with letters of their own", {
  skip_on_cran()
  code <- function(d) .rp1(d)$code
  feats <- function(d) .rp1(d)$features
  expect_identical(code("Snd"), "Snd")
  # -d is written on BT and S only (BTd, Sd): Snd has no other reading
  expect_true(is.na(.rp1("Snd")$ambiguous))
  expect_match(.rp1("Kc")$ambiguous, "K + c", fixed = TRUE)
  expect_identical(code("And"), "And")
  expect_identical(code("Sdo"), "Sdo")
  expect_identical(code("Sal"), "Sal")
  expect_identical(c(code("SaS"), .rp1("SaS")$overlay), c("S", "Sa"))
  expect_identical(code("Femp"), "Femp")
  expect_identical(code("Fem"), "Fem")
  expect_identical(c(code("Kc"), length(feats("Kc"))), c("Kc", "0"))
  expect_identical(code("OXc"), "OXc")
  expect_identical(code("OXm"), "OXm")
  expect_identical(c(code("Xc"), length(feats("Xc"))), c("Xc", "0"))
  expect_identical(c(code("Dx"), feats("Dx")), c("D", "x"))
  expect_match(.rp1("Dx")$meanings, "mixed rocks")
  expect_match(.rp1("BTx")$meanings, "fragic")
  expect_identical(c(code("Anod"), feats("Anod")), c("A", "nod"))
  expect_identical(c(code("Sng"), feats("Sng")), c("Sn", "g"))
  expect_identical(c(code("Sdg"), feats("Sdg")), c("S", "d", "g"))
  expect_identical(c(code("Ccry"), feats("Ccry")), c("C", "cry"))
  expect_identical(c(code("Rcri"), feats("Rcri")), c("R", "cri"))
  expect_identical(c(code("Msi"), feats("Msi")), c("M", "si"))
  expect_identical(code("Mli"), "Mli")
  expect_identical(c(code("Sc"), feats("Sc")), c("S", "c"))
  expect_match(.rp1("Sc")$meanings, "indurated")
  expect_identical(c(code("SVg"), feats("SVg")), c("SV", "g"))
  expect_identical(c(code("Sv"), feats("Sv")), c("S", "v"))
  expect_identical(c(code("VNa"), feats("VNa")), c("V", "Na"))
  expect_identical(c(code("Gr"), .rp1("Gr")$base), c("Gr", "G"))
  expect_identical(c(code("Ga"), feats("Ga")), c("G", "a"))
  expect_identical(c(code("ga"), feats("ga")), c("g", "a"))
  expect_identical(c(code("BT2"), .rp1("BT2")$subdiv), c("BT", "2"))
  expect_identical(code(paste0("BT", .ss)), paste0("BT", .ss))
  expect_identical(code("BT\u03b2"), paste0("BT", .ss))
  expect_identical(c(code("LBT"), .rp1("LBT")$overlay), c("BT", "L"))
  expect_identical(c(code("Lz"), feats("Lz")), c("L", "z"))
  expect_identical(c(code("Ltp"), feats("Ltp")), c("L", "tp"))
  # hosts the book lists completely: -a on E and G, -ca on C and layers, -t on S, C, FS
  expect_false(.rp1("Aa")$known)
  expect_false(.rp1("Aca2a")$known)
  expect_false(.rp1("BTt")$known)
  expect_identical(c(code("FSt"), feats("FSt")), c("FS", "t"))
  # transitions without the hyphen; B only in transitions
  r <- .rp1("BC")
  expect_true(r$transition)
  expect_identical(r$parts, c("B", "C"))
  expect_identical(.rp1("ACk")$parts, c("A", "C"))
  expect_false(.rp1("B")$known)
  expect_true(.has_note(.rp1("B")$notes, "only in transitions"))
  expect_false(.rp1("BTG")$known)
  # Roman numerals
  expect_identical(.rp1("IIRT")$lith, 2L)
  expect_identical(c(code("VSa"), .rp1("VSa")$lith), c("V", "1"))
  expect_identical(.rp1("III R")$lith, 3L)
  r <- .rp1("II")
  expect_true(r$known)
  expect_identical(r$lith, 2L)
})

test_that("the writings of the book and the old codes", {
  skip_on_cran()
  r <- .rp1("Lav")
  expect_identical(c(r$code, r$overlay, r$features), c("A", "L", "v"))
  expect_true(r$variant)
  expect_identical(.rp1("Vna")$features, "Na")
  expect_identical(.rp1("Vsa")$features, "Sa")
  expect_identical(c(.rp1("OFz")$code, .rp1("OFz")$features), c("OF", "zo"))
  expect_identical(c(.rp1("OHz")$code, .rp1("OHz")$features), c("OH", "zo"))
  expect_identical(c(.rp1("OHmzo")$code, .rp1("OHmzo")$features), c("OHf", "zo"))
  expect_identical(c(.rp1("Ypm")$code, .rp1("Ypm")$features), c("Yp", "m"))
  expect_identical(c(.rp1("Snh")$code, .rp1("Snh")$features), c("Sn", "h"))
  expect_identical(c(.rp1("JZ")$code, .rp1("JZ")$overlay), c("Z", "J"))
  for (oc in list(c("Ahs", "Aso"), c("FEm", "Fem"), c("FEmp", "Femp"), c("Ap", "L"))) {
    r <- .rp1(oc[1L])
    expect_identical(r$code, oc[2L], info = oc[1L])
    expect_true(r$old, info = oc[1L])
  }
})

test_that("the notation is declared, recognised, refused or undecided (Q9)", {
  skip_on_cran()
  for (ds in c("rp", "RP 2008", "rp2008", "referentiel", "R\u00e9f\u00e9rentiel p\u00e9dologique",
               "AFES", "rp1995")) {
    expect_true(.rpn(c("A", "S", "C"), list(designation_system = ds))$ok, info = ds)
  }
  for (ds in c("french", "fr", "cpcs", "fao", "kidpr")) {
    x <- .rpn(c("OL", "OF", "Sca"), list(designation_system = ds))
    expect_false(x$ok, info = ds)
    expect_identical(x$status, "other", info = ds)
  }
  for (ds in c("unknown", "none", "", NA)) {
    expect_true(.rpn(c("OL", "A", "Sca"), list(designation_system = ds))$ok, info = ds)
  }
  x <- .rpn(c("OL", "OF", "Ah", "S", "C"))
  expect_true(x$ok)
  expect_identical(x$status, "recognised")
  expect_true(.rpn(c("LA", "Eg", "BT", "C"))$ok)
  expect_true(.rpn(c("A", "Sp", "C"))$ok)
  expect_true(.rpn(c("Ah", "E&BT", "C"))$ok)
  expect_true(.rpn(c("A", "Aho", "C"))$ok)
  expect_true(.rpn(c("NaA", "NaS", "NaC"))$ok)
  # two likely symbols, not one
  expect_true(.rpn(c("A", "K", "Cca"))$ok)
  x <- .rpn(c("A", "S", "Cca"))
  expect_false(x$ok)
  expect_match(x$why, "one kind of symbol")
  expect_false(.rpn(c("A", "Cca", "Cca"))$ok)
  expect_false(.rpn(c("Ah", "Bm", "Cca", "IICcak"))$ok)
  expect_false(.rpn(c("A", "BTg", "BTgd", "C", "x"))$ok)
  expect_true(.rpn(c("A", "Hf", "Hm"))$ok)
  # ND (also "not determined") is likely, not strong
  expect_false(.rpn(c("A", "ND", "C"))$ok)
  expect_true(.rpn(c("Ah", "ND", "RT"))$ok)
  # BT with a lower-case suffix is likely only beside lower case elsewhere
  expect_false(.rpn(c("A", "BTg", "C"))$ok)
  expect_true(.rpn(c("Ae", "BTg", "Sg"))$ok)
  # a CPCS profile (A1, A2, (B), Cca) and the CPCS examples are not RP
  x <- .rpn(c("A1", "A2", "(B)", "Cca"))
  expect_false(x$ok)
  expect_identical(x$status, "cpcs")
  expect_true("(B)" %in% x$cpcs)
  expect_false(.rpn(c("A00", "A1", "A2g", "Bt", "C"))$ok)
  for (cl in eval(formals(make_cpcs1967_example)$class)) {
    p <- make_cpcs1967_example(cl)
    expect_false(isTRUE(.rpn(p$horizons$designation, p$site)$ok), info = cl)
    expect_false(isTRUE(.rpn(p$horizons$designation)$ok), info = cl)
  }
  # a Russian profile and the Russian examples are not RP
  x <- .rpn(c("O", "AY", "EL", "BEL", "BT", "C"))
  expect_false(x$ok)
  expect_identical(x$status, "russian")
  for (dv in eval(formals(make_kidpr2008_example)$division)) {
    p <- make_kidpr2008_example(dv)
    expect_false(isTRUE(.rpn(p$horizons$designation)$ok), info = dv)
  }
  # an FAO profile written all in capitals
  x <- .rpn(c("AP", "BT", "BTG", "BC", "C"))
  expect_false(x$ok)
  expect_identical(x$status, "none")
  # all in capitals, the case is lost: only a declaration recognises the RP
  x <- .rpn(c("OL", "OF", "A", "S", "C"))
  expect_false(x$ok)
  expect_identical(x$status, "capitals")
  expect_true(.rpn(c("OL", "OF", "A", "S", "C"), list(designation_system = "rp"))$ok)
  expect_false(.rpn(c("Ap", "Bt", "Btg", "BC", "C"))$ok)
  expect_false(.rpn(c("Oi", "Ah", "Bw", "Bk", "Ck"))$ok)
  # a mixed profile is undecided
  x <- .rpn(c("OL", "OFzo", "A1", "A2", "(B)", "C"))
  expect_true(is.na(x$ok))
  expect_identical(x$status, "mixed")
  x <- .rpn(c("OL", "Sca", "AY", "BEL"))
  expect_true(is.na(x$ok))
  # never by the country
  expect_false(.rpn(c("A", "S", "C"), list(country = "FR"))$ok)
  expect_false(.rpn(c("A", "S", "C"), list(country = "France"))$ok)
  expect_false(.rpn(character(0))$ok)
  expect_false(.rpn(c(NA, ""))$ok)
})

test_that("the traps of the notation: Cca, A2, G, FE, Ha, -cn", {
  skip_on_cran()
  rp <- list(designation_system = "rp")
  cpcs <- list(designation_system = "cpcs")
  # Cca: a calcareous C in the RP, a carbonate accumulation in the CPCS
  r <- soilKey:::.rp_designations(c("A", "S", "Cca"), rp)
  expect_identical(c(r$code[3L], r$features[[3L]]), c("C", "ca"))
  expect_match(r$meanings[[3L]], "calcareous")
  r <- soilKey:::.rp_designations(c("A1", "(B)", "Cca"), cpcs)
  expect_false(r$rp)
  expect_true(is.na(r$code[3L]))
  expect_setequal(r$hint[[3L]], c("Ck", "K", "Kc", "Km"))
  expect_identical(r$hint_source[3L], "Annex 6 (CPCS 1967)")
  expect_identical(r$read_as_rp[3L], "C")
  # A2: a sub-horizon of A in the RP, an E in the CPCS
  r <- soilKey:::.rp_designations(c("A1", "A2", "S", "C"), rp)
  expect_identical(c(r$code[2L], r$subdiv[2L]), c("A", "2"))
  r <- soilKey:::.rp_designations(c("A1", "A2", "B", "C"), cpcs)
  expect_setequal(r$hint[[2L]], c("E", "Ea"))
  expect_true(soilKey:::.rp_cpcs_only("A2", "Bt"))
  expect_true(soilKey:::.rp_cpcs_only("A2g", NA))
  expect_false(soilKey:::.rp_cpcs_only("A2", "BT"))
  expect_false(soilKey:::.rp_cpcs_only("A2", "S"))
  # G, Go, Gr: shared, no sign of either notation for the RP; the CPCS reader unchanged
  expect_false(.rpn(c("A", "Go", "Gr"))$ok)
  expect_true(soilKey:::.cpcs_notation(c("A1", "G"), list()))
  expect_true(soilKey:::.cpcs_notation(c("A", "Go", "Gr"), list()))
  r <- soilKey:::.rp_designations(c("OL", "A", "Go", "Gr"))
  expect_true(r$rp)
  expect_identical(r$code[3:4], c("Go", "Gr"))
  expect_identical(r$base[3:4], c("G", "G"))
  r <- soilKey:::.rp_designations(c("A1", "G"), cpcs)
  expect_setequal(r$hint[[2L]], c("Go", "Gr"))
  # FE, the ferruginous horizon of 2008, against Fe; FE ferric in the RP of 1995
  r <- soilKey:::.rp_designations(c("A", "FE", "Fe"), rp)
  expect_identical(r$code[2:3], c("FE", "Fe"))
  expect_false(r$rp1995)
  r <- soilKey:::.rp_designations(c("A", "FE", "C"), list(designation_system = "rp1995"))
  expect_identical(r$code[2L], "Fe")
  expect_true(r$rp1995)
  expect_true(.has_note(r$notes[[2L]], "RP of 1995"))
  r <- soilKey:::.rp_designations(c("A", "FE", "FEm"))
  expect_true(r$rp)
  expect_true(r$rp1995)
  expect_identical(r$code[2:3], c("Fe", "Fem"))
  r <- soilKey:::.rp_designations(c("Ahs", "FE", "C"))
  expect_identical(r$code[1:2], c("Aso", "Fe"))
  expect_identical(.rp1("FE", rp1995 = TRUE)$code, "Fe")
  # Ha: the drained histic horizon of the RP, the highly decomposed H of the FAO
  r <- soilKey:::.rp_designations(c("Ha", "Mli"), rp)
  expect_identical(c(r$code[1L], r$base[1L], r$family[1L]), c("Ha", "Ha", "H"))
  r <- soilKey:::.rp_designations(c("Oi", "Ha", "C"), list(designation_system = "fao"))
  expect_true(is.na(r$code[2L]))
  expect_identical(r$hint[[2L]], "Hs")
  expect_identical(r$hint_source[2L], "FAO letters")
  # -cn: the sesquioxide suffix, not c and n
  expect_identical(.rp1("BTcn")$features, "cn")
  expect_identical(c(.rp1("FEcn")$code, .rp1("FEcn")$features), c("FE", "cn"))
})

test_that("the profile reader: hints in other notations, RP codes in the RP", {
  skip_on_cran()
  r <- soilKey:::.rp_designations(c("OL", "OFzo", "A", "Ckm", "C"))
  expect_true(r$rp)
  expect_identical(r$code[1:3], c("OL", "OF", "A"))
  expect_false(r$known[4L])
  expect_setequal(r$hint[[4L]], c("Ck", "K", "Km"))
  expect_identical(r$strength[1:2], c("strong", "strong"))
  expect_true(r$any_known)
  r <- soilKey:::.rp_designations(c("Ap", "Bw", "Btg", "Ck"))
  expect_false(r$rp)
  expect_true(all(is.na(r$code)))
  expect_setequal(r$hint[[1L]], c("L", "LA", "LE", "LS"))
  expect_true("S" %in% r$hint[[2L]])
  expect_identical(r$hint_features[[3L]], "g")
  expect_setequal(r$hint[[4L]], c("Ck", "K"))
  r <- soilKey:::.rp_designations(c("OL", "OF", "A1g", "A2", "(B)", "C"))
  expect_true(is.na(r$rp))
  expect_true(all(is.na(r$code)))
  expect_identical(r$read_as_rp[1:2], c("OL", "OF"))
  expect_true(.has_note(r$notes[[1L]], "may not be in"))
  expect_true(length(r$hint[[5L]]) > 0L)
  fields <- c("written", "code", "base", "family", "kind", "overlay", "features", "meanings",
              "lith", "subdiv", "transition", "juxtaposed", "parts", "part_features",
              "parts_strong", "mark_alone", "old", "old_codes", "variant", "known",
              "ambiguous", "notes", "strength", "read_as_rp", "hint",
              "hint_features", "hint_source", "rp", "status", "why", "rp1995", "any_known")
  expect_named(r, fields)
})

test_that("nothing stops with an error", {
  skip_on_cran()
  odd <- list(NA, "", " ", "?", "NA", "ND", "Na", "123", "BT?", "A  1", "\u00a0LA\u00a0",
              "A1 (glossic)", "\u2013g", "((A)", ")", "|", "&", "A-", "-", "--x", "II  ",
              "\uff2c\uff21", rawToChar(as.raw(c(0x42, 0x54, 0xdf))), "B\xff\xfe")
  for (d in odd) expect_no_error(.rp1(d))
  expect_true(is.na(.rp1(NA)$written))
  expect_false(.rp1("?")$known)
  expect_identical(.rp1("ND")$code, "ND")
  expect_identical(.rp1("Na")$code, "Na")
  expect_identical(.rp1(rawToChar(as.raw(c(0x42, 0x54, 0xdf))))$code, paste0("BT", .ss))
  expect_identical(.rp1("\uff2c\uff21")$code, "A")
  expect_identical(.rp1("A1 (glossic)")$code, "A")
  expect_identical(.rp1("\u2013g")$features, "g")
  for (x in list(NULL, 1:3, factor(c("A", "S")), list("OL", 2), data.frame(a = 1))) {
    expect_no_error(soilKey:::.rp_designations(x))
    expect_no_error(.rpn(x, list(designation_system = list("rp"))))
  }
  expect_no_error(.rpn(c("A", "S"), "rp"))
  expect_no_error(soilKey:::.rp_read_sequence("A/(B/C"))
})

test_that("the new attributes are documented, naming the RP", {
  skip_on_cran()
  a <- horizon_attribute_spec()
  hz <- c("coarse_fragments_wt_pct", "gravel_wt_pct", "cobbles_wt_pct", "stones_wt_pct",
          "boulders_wt_pct", "cec_method", "redox_relict", "anthropic_material",
          "anthropic_material_pct", "o_zoogenic", "fine_organic_matter_pct",
          "biogenic_aggregates_pct", "biogenic_aggregate_size", "effervescence_hot", "mgco3_pct",
          "fe_total_pct", "fe_ffe_pct", "weatherable_minerals_pct", "oxides_triacid_pct",
          "clay_mineralogy", "halloysite_pct", "kaolinite_pct", "smectite_pct",
          "degradation_form", "tongue_vol_pct", "tongue_width_mm", "lamellae_cumulative_cm",
          "c_pyro_pct", "pyrophosphate_index", "s_total_pct", "nodules_wt_pct")
  expect_length(hz, 31L)
  expect_true(all(hz %in% a$name))
  expect_true(all(a$systems[a$name %in% hz] == "rp2008"))
  expect_true(all(grepl("R\u00e9f\u00e9rentiel p\u00e9dologique 2008", a$description[a$name %in% hz])))
  expect_identical(a$type[a$name == "redox_relict"], "logical")
  expect_identical(a$allowed[a$name == "o_zoogenic"], "zo, noz")
  expect_match(a$description[a$name == "cole_value"], "33 kPa")
  expect_match(a$description[a$name == "layer_origin"], "colluvic")
  s <- site_attribute_spec()
  st <- c("valley_position", "flood_frequency", "poldered", "lateral_bp_downslope",
          "rp_anthropic_history", "rp_qualifiers_recorded", "parent_material_relation")
  expect_true(all(st %in% s$name))
  expect_true(all(!nzchar(s$systems[s$name %in% st])))
  ds <- strsplit(s$allowed[s$name == "designation_system"], ", ", fixed = TRUE)[[1L]]
  expect_true(all(c("rp", "rp2008", "rp1995", "referentiel", "afes", "french", "cpcs") %in% ds))
  hf <- strsplit(s$allowed[s$name == "humus_form"], ", ", fixed = TRUE)[[1L]]
  expect_true(all(c("mull", "eumull", "dysmoder", "amphimus", "tangel", "agri-mull") %in% hf))
  expect_true(all(hz %in% names(pedon_json_schema()$properties$horizons$items$properties)))
  rd <- yaml::read_yaml(system.file("rules", "rp2008", "readings.yaml", package = "soilKey"))$readings
  # every reading is Hugo's decision, of 2026-11-02 or of the later releases
  # (0.9.346: 2026-11-03); none waits for confirmation
  expect_true(all(grepl("\\(Hugo, 2026-11-0[23]\\)\\.?\"?$",
                        vapply(rd, `[[`, "", "reading"))))
})

test_that("the fixes of the review (D1-D7 and the technical points)", {
  skip_on_cran()
  code <- function(d) .rp1(d)$code
  # D1: a lone V or X is a code, not a Roman numeral (SV/V, O/X, OL/OFnoz/X)
  for (d in c("V", "X")) {
    expect_identical(code(d), d)
    expect_identical(.rp1(d)$lith, 1L)
  }
  expect_identical(.rp1("III")$lith, 3L)
  for (sq in c("SV/V", "A|LA/SV/V/C|M|R|K|Yp", "O/X", "OL/OFnoz/X", "OL/OFnoz/OHnoz/X")) {
    rd <- soilKey:::.rp_read_sequence(sq)
    last <- rd$slots[[if (grepl("X$", sq)) length(rd$slots) else
                      which(vapply(rd$slots, function(z) z$alternatives[[1L]]$code %in% "V", NA))]]
    expect_true(last$alternatives[[1L]]$code %in% c("V", "X"), info = sq)
    expect_identical(last$alternatives[[1L]]$lith, 1L, info = sq)
  }
  # D2: Ap inside a transition is no sign and does not make the profile RP 1995
  r <- .rp1("Ap/C")
  expect_identical(r$old_codes, "Ap")
  x <- .rpn(c("Ap/C", "Bw"))
  expect_false(isTRUE(x$ok))
  expect_false(x$rp1995)
  expect_false(.rpn(c("Ap-C", "Bw", "C"))$rp1995)
  expect_true(.rpn(c("A", "FE", "FEm-S"))$rp1995)
  # D3: a juxtaposition is a sign only as E&BT or with a part that is one
  expect_false(.rpn(c("L&F", "Ah", "Bw1", "C"))$ok %in% TRUE)
  expect_false(.rpn(c("L", "F&H", "Ah", "Bs"))$ok %in% TRUE)
  expect_true(.rpn(c("Ah", "Ea&BTg", "C"))$ok)
  expect_true(.rpn(c("Ah", "OF&OH", "C"))$ok)
  expect_true(.rp1("Ea&BTg")$juxtaposed)
  # D4: L only before the A, E, S, O, H, BT and BP families
  for (d in c("LF", "LC", "LR", "LM", "LD", "LF&H")) expect_false(.rp1(d)$known, info = d)
  for (d in c("LA", "LE", "LS", "LSp", "LO", "LH", "LBT", "LBP")) expect_true("L" %in% .rp1(d)$overlay, info = d)
  # D5: -ho is a sign on its own hosts with nothing after it; * is likely
  expect_true(.rpn(c("Aho", "S", "C"))$ok)
  expect_true(.rpn(c("Acaho", "Sca", "C"))$ok)
  x <- .rpn(c("Ahoc", "Bw", "C"))
  expect_false(isTRUE(x$ok))
  expect_false(.rpn(c("A*", "Bt", "C"))$ok %in% TRUE)
  expect_identical(soilKey:::.rp_designations(c("Ahoc", "S", "A*"))$strength, c("likely", NA, "likely"))
  # D6: a prefix written apart joins its master (p. 296)
  for (d in c("Na BT", "Sa A", "Na A", "Sa S", "L A", "\u03c0 BT")) {
    r <- .rp1(d)
    expect_true(r$known, info = d)
    expect_length(r$overlay, 1L)
  }
  expect_identical(c(code("Na BT"), .rp1("Na BT")$overlay), c("BT", "Na"))
  rd <- soilKey:::.rp_read_sequence("Na A/Na S/Na C")
  expect_identical(vapply(rd$slots, function(z) z$alternatives[[1L]]$code, ""), c("A", "S", "C"))
  # D7: Annex 6 (B)Ca
  expect_identical(soilKey:::.rp_hint("(B)Ca", TRUE)$hint, "Sk")
  # sub-horizons from 1; A00 declared stays unread with its Annex 6 hint
  for (d in c("A0", "A00", "BT0", "S01")) expect_false(.rp1(d)$known, info = d)
  expect_identical(.rp1("A10")$subdiv, "10")
  r <- soilKey:::.rp_designations(c("A00", "A1", "S"), list(designation_system = "rp"))
  expect_false(r$known[1L])
  expect_identical(r$hint[[1L]], "OL")
  # Cs is C with -s, not a symbol of the CPCS only
  expect_false(soilKey:::.rp_cpcs_only("Cs", NA))
  expect_identical(c(code("Cs"), .rp1("Cs")$features), c("C", "s"))
  # the Australian A2e is not called CPCS in the explanation
  x <- .rpn(c("A1", "A2e", "B21", "C"))
  expect_false(x$ok)
  expect_false(grepl("CPCS", x$why))
  # repeated marks, and marks the code already says
  for (d in c("Sgg", "Rcaca", "Kcc", "OXmc", "Femc", "Xcc")) {
    r <- .rp1(d)
    expect_false(r$known, info = d)
    expect_true(.has_note(r$notes, "written twice"), info = d)
  }
  # the hosts of the complete lists
  for (d in c("St", "Ct", "FSt", "BTd", "Sd", "Ea", "Ga", "ga", "OFnoz", "OHzo", "OFrzo"))
    expect_true(.rp1(d)$known, info = d)
  for (d in c("Scat", "Et", "Sndd", "Aa", "OLzo", "OHtanoz")) expect_false(.rp1(d)$known, info = d)
  # transitions keep their digits and the features of each part
  r <- .rp1("ACk2")
  expect_identical(c(r$subdiv, r$part_features), c("2", "", "k"))
  # "+" written between two horizons: juxtaposed volumes (pp. 229, 315)
  for (d in c("E+A", "A + E")) expect_true(.rp1(d)$juxtaposed, info = d)
  # folded characters: thin spaces, the Windows-1252 en dash, capital pi, pi, capital eszett
  expect_true(.rp1(rawToChar(as.raw(c(0x41, 0x96, 0x53))))$transition)
  expect_true(.rp1("A\u2009-\u202fS")$transition)
  expect_identical(.rp1("\u03a0RT")$overlay, .pi)
  expect_identical(.rp1("piBT")$overlay, .pi)
  expect_identical(code("BT\u1e9e"), paste0("BT", .ss))
  # the pages corrected; A1g takes the A1 row of Annex 6
  i <- soilKey:::.rp_vocabulary()$info
  expect_identical(c(i["Amg", "code"], i["Alu", "code"], i["OXfr", "code"]), c("Amg", "Alu", "OXfr"))
  pg <- vapply(soilKey:::.rp_catalogue()$horizons, function(e) as.integer(e$page), 1L)
  names(pg) <- vapply(soilKey:::.rp_catalogue()$horizons, `[[`, "", "code")
  expect_identical(unname(pg[c("Amg", "Alu", "OXfr")]), c(12L, 78L, 151L))
  for (d in c("A1g", "A11g")) expect_true("Aso" %in% soilKey:::.rp_hint(d, TRUE)$hint, info = d)
  # the iron units and the two attributes of the plan
  a <- horizon_attribute_spec()
  expect_identical(a$unit[a$name == "fe_total_pct"], "% Fe2O3")
  expect_identical(a$unit[a$name == "fe_dcb_pct"], "% Fe")
  expect_identical(a$unit[a$name == "fe_ffe_pct"], "% Fe")
  expect_match(a$description[a$name == "fe_total_pct"], "0.6994", fixed = TRUE)
  expect_true("lateral_cover_pct" %in% a$name)
  expect_true("rp_paleo" %in% site_attribute_spec()$name)
})

test_that("Hugo's decisions on the review (2026-11-02)", {
  skip_on_cran()
  # 1. FAO and USDA symbols beside an RP symbol leave the notation undecided
  for (p in list(c("OL", "OF", "Ah", "Bt"), c("LA", "Sca", "Bw", "C"), c("OF", "Ag", "2Oh", "2Cgh"),
                 c("AG", "2Cg1", "OH", "3Cr"), c("Oi", "OFzo", "Ah"), c("LA", "Sca", "AB", "C"),
                 c("LA", "Sca", "Cr"))) {
    x <- .rpn(p)
    expect_true(is.na(x$ok), info = paste(p, collapse = "/"))
    expect_identical(x$status, "mixed", info = paste(p, collapse = "/"))
    expect_true(length(x$fao) > 0L, info = paste(p, collapse = "/"))
  }
  # ... the RP transitions are no FAO symbols (A-S, AS, BC)
  expect_true(.rpn(c("LA", "Sca", "BC", "C"))$ok)
  expect_true(.rpn(c("LA", "AS", "Sca", "C"))$ok)
  expect_true(.rpn(c("Aca", "II Sca", "II Cca"))$ok)
  # 2 (refined). In a profile all in capitals the designations an upper-cased
  # FAO, USDA or Canadian table writes are no signs; the RP-only codes still are
  x <- .rpn(c("OL", "OF", "OH", "A", "BT", "C"))
  expect_false(x$ok)
  expect_identical(x$status, "capitals")
  expect_true(.rpn(c("OL", "OF", "OH", "A", "BT", "C"), list(designation_system = "AFES"))$ok)
  x <- .rpn(c("OH", "AH", "E", "BT", "C"))
  expect_false(x$ok)
  expect_identical(x$status, "capitals")
  for (p in list(c("LA", "BT"), c("SV", "V"), c("A", "FE", "C"), c("ND", "OX"), c("A", "ND", "F", "RT"),
                 c("LE", "BT1", "BT2", "C"), c("A", "E", "BP"), c("U", "TH"), c("II A", "II FS")))
    expect_true(.rpn(p)$ok, info = paste(p, collapse = "/"))
  up <- function(d) soilKey:::.rp_upper_fao(d)
  for (d in c("OL", "OF", "OH", "OI", "OE", "OA", "OM", "AH", "AP", "AE", "AG", "BT", "BTG", "BTD",
              "BW", "BS", "BG", "BK", "BH", "BF", "BC", "CK", "CG", "CR", "2CG", "II BT", "A1", "LFH", "HF"))
    expect_true(up(d), info = d)
  for (d in c("LA", "LE", "LS", "LO", "LBT", "LBP", "SV", "FS", "OX", "RT", "FE", "BP", "BPH", "ND",
              "TH", "SCA", "KC", "HM", "ZO"))
    expect_false(up(d), info = d)
  # 3. a lone g is likely
  expect_identical(soilKey:::.rp_designations(c("Ah", "g", "C"))$strength[2L], "likely")
  expect_false(.rpn(c("Ah", "g", "Cg"))$ok)
  expect_true(.rpn(c("Ah", "g", "Sg"))$ok)
  # 4. the declaration by prefix, for the RP and for the Russian reader
  for (ds in c("AFES 2009", "RP08", "referentiel 2008", "R\u00e9f\u00e9rentiel p\u00e9dologique fran\u00e7ais",
               "RP-2008", "rp")) {
    x <- .rpn(c("A", "FE", "C"), list(designation_system = ds))
    expect_true(x$ok, info = ds)
    expect_false(x$rp1995, info = ds)
  }
  expect_true(.rpn(c("A", "S"), list(designation_system = iconv("R\u00e9f\u00e9rentiel", "UTF-8", "latin1")))$ok)
  for (ds in c("rp 1995", "RP95", "rp1995", "Referentiel pedologique 1995")) {
    x <- .rpn(c("A", "FE", "C"), list(designation_system = ds))
    expect_true(x$ok, info = ds)
    expect_true(x$rp1995, info = ds)
  }
  for (ds in c("french", "fr", "cpcs", "fao")) expect_false(.rpn(c("OL", "Sca"), list(designation_system = ds))$ok, info = ds)
  for (ds in c("Russian 2008", "RU-2008", "kidpr 2008", "rfg2008", "kidpr", "ru"))
    expect_true(soilKey:::.kidpr_is_declared(ds), info = ds)
  for (ds in c("fao", "rp", "cpcs", "french")) expect_false(soilKey:::.kidpr_is_declared(ds), info = ds)
})
