# v0.9.339: the types of the divisions CK to CT of the Russian soil
# classification (Field Guide of 2008, chapter 4): the profile formula of each
# type (inst/rules/kidpr2008/types/CK.yaml ... CT.yaml), matched as for CA to
# CJ (R/types-kidpr.R) under Hugo's decisions of 2026-11-02 (Q13 closed
# formulas, Q14 what a formula tests, Q15 no type, Q20 the fragments of TUR).
# Profiles are written in the Russian notation, except the undesignated
# twins, which go through the key to horizons on the data.

# --- builders ------------------------------------------------------------------------

.ck_ped <- function(tokens, site = list(), ...) {
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
.ck_class <- function(p, ...) classify_kidpr2008(p, on_missing = "silent", ...)
.ck_of <- function(tokens, ...) .ck_class(.ck_ped(tokens, ...))
.ck_type <- function(r) if (nrow(r$levels) >= 3L) r$levels$code[3L] else NA_character_
.ck_codes <- function(tokens, ...) paste(.ck_of(tokens, ...)$levels$code, collapse = "/")
.ck_counts <- c(CK = 12L, CL = 3L, CM = 4L, CN = 9L, CO = 11L, CP = 13L, CQ = 10L, CR = 13L, CS = 19L, CT = 17L)
.ck_fixture <- function() {
  path <- testthat::test_path("fixtures", "kidpr2008-formulas.tsv")
  fx <- utils::read.delim(path, comment.char = "#", quote = "", stringsAsFactors = FALSE,
                          encoding = "UTF-8", colClasses = "character")
  fx <- fx[grepl("^ch4 C[K-T]", fx$source), ]
  stats::setNames(fx$formula, sub("^ch4 ", "", fx$source))
}
# A formula as a profile in the Russian notation, with plausible thicknesses:
# the marks as written (BCAs,cs, Qs, CQca,s, Rca, DG, TUR[SEL+ASN]), the
# optional features in parentheses left out, the optional horizons written
# or left out, the horizons in `drop` left out; the [ABC] of CL03 a formed
# profile; a lithozem over its rock within 30 cm; a TUR of 50 cm (a
# turbozem) or, with its fragments under P or PU, of 30 cm (a solonetzic
# agrozem).
.ck_th <- c(O = 6, T = 30, TJ = 20, H = 20, AY = 15, AU = 30, AJ = 15, AO = 8, AK = 10, AH = 20, P = 25,
            PU = 30, PT = 25, PTR = 25, PB = 25, E = 10, EL = 15, SEL = 10, ASN = 30, BSN = 30, BT = 40, BHF = 30,
            BH = 30, BFM = 35, BM = 40, CRM = 30, BPL = 30, BCA = 35, CAT = 35, BI = 35, G = 30, Q = 30, S = 8,
            SS = 8, TUR = 50, C = 30, D = 30, R = 20)
.ck_lith <- c(T = 15, TJ = 15, H = 15, AY = 15, AU = 15, AJ = 15, AO = 8, AK = 10, AH = 15, P = 25, PU = 25, C = 5, R = 20)
.ck_tokens <- function(formula, drop_optional = FALSE, drop = character(0)) {
  out <- character(0)
  prev <- ""
  lith <- grepl("(^|-)R", formula)
  for (x in soilKey:::.kidpr_split_outside(formula, "-")) {
    if (x == "[ABC]") {
      out <- c(out, "AJ:15", "BCA:30", "Cca:30")
      next
    }
    if (grepl("^\\(.*\\)$", x)) {
      if (drop_optional) next
      x <- sub("^\\((.*)\\)$", "\\1", x)
    }
    x <- gsub("\\((ca|s|g)\\)", "", x)
    m <- sub("^([A-Z]+).*$", "\\1", x)
    if (m %in% c("CG", "CQ")) m <- "C"
    if (m == "DG") m <- "D"
    if (m %in% drop) next
    t <- if (lith) .ck_lith[[m]] else .ck_th[[m]]
    if (m == "T" && prev == "PT") t <- 15
    if (m == "TUR" && prev %in% c("P", "PU") && grepl("[", x, fixed = TRUE)) t <- 30
    out <- c(out, sprintf("%s:%s", x, t))
    prev <- m
  }
  out
}

# Undesignated layers for the data route (designation_system unset; FAO
# designations as hints), as in the tests of the key to horizons.
.ck_L <- function(top, bottom, ...) {
  d <- list(top_cm = top, bottom_cm = bottom, munsell_hue_dry = "10YR", munsell_value_dry = 6,
            munsell_chroma_dry = 4, structure_type = "massive", structure_grade = "structureless",
            clay_pct = 20, silt_pct = 40, sand_pct = 40, oc_pct = 0.1, ph_h2o = 6.0,
            clay_films_amount = "none", effervescence_class = "none", secondary_carbonates_form = "none")
  a <- list(...)
  d[names(a)] <- a
  d
}
.ck_P <- function(..., site = list()) {
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
.ck_AY <- function(top = 0, bottom = 15, ...) .ck_L(top, bottom, munsell_value_dry = 5, munsell_chroma_dry = 2,
                                                   structure_type = "granular", structure_grade = "moderate",
                                                   ph_h2o = 5.0, oc_pct = 2, clay_pct = 15, silt_pct = 50,
                                                   sand_pct = 35, ...)
.ck_AU <- function(top, bottom, ...) .ck_AY(top, bottom, munsell_value_dry = 3, munsell_chroma_dry = 1,
                                            ph_h2o = 7.2, oc_pct = 4, ...)
.ck_B <- function(top, bottom, ...) .ck_L(top, bottom, munsell_hue_dry = "7.5YR", munsell_value_dry = 5,
                                          munsell_chroma_dry = 6, structure_type = "blocky",
                                          structure_grade = "moderate", clay_pct = 25, ...)
.ck_moist <- function(top, bottom, hue, v, c, ...) .ck_L(top, bottom, munsell_hue_moist = hue,
                                                         munsell_value_moist = v, munsell_chroma_moist = c,
                                                         munsell_hue_dry = NA, munsell_value_dry = NA,
                                                         munsell_chroma_dry = NA, ...)
.ck_E <- function(top, bottom, ...) .ck_L(top, bottom, munsell_value_dry = 7, munsell_chroma_dry = 2,
                                          structure_type = "platy", structure_grade = "weak", clay_pct = 10,
                                          silt_pct = 55, sand_pct = 35, ...)
.ck_BT <- function(top, bottom, ...) .ck_B(top, bottom, clay_pct = 30, silt_pct = 40, sand_pct = 30,
                                           structure_type = "prismatic", clay_films_amount = "common", ...)
.ck_und <- function(r, cands) {
  expect_identical(nrow(r$levels), 2L)
  expect_true(all(paste0("types:", cands) %in% r$provisional_because))
  w <- grep("The type is undecided", r$warnings, value = TRUE)
  expect_length(w, 1L)
  for (k in cands) expect_match(w, k, fixed = TRUE)
}
.ck_q15 <- function(r, div, near = character(0)) {
  expect_identical(r$levels$code, c("C", div))
  expect_length(r$provisional_because, 0L)
  w <- grep("Combination not provided by the Field Guide of 2008", r$warnings, value = TRUE)
  expect_length(w, 1L)
  expect_true(all(near %in% names(r$trace$types_nearest)))
}


# --- the rules --------------------------------------------------------------------------

test_that("the 111 types of CK to CT are clean and as the fixture writes them", {
  skip_on_cran()
  r <- load_rules("kidpr2008")
  v <- .validate_rules("kidpr2008")
  expect_length(v$missing, 0L)
  expect_length(v$malformed, 0L)
  expect_length(v$codes, 0L)
  expect_true(all(names(.ck_counts) %in% names(r$types)))
  expect_identical(lengths(r$types)[names(.ck_counts)], .ck_counts)
  expect_identical(sum(.ck_counts), 111L)
  expect_identical(sum(lengths(r$types)), 217L)
  # the chemozems have no types
  expect_false("CU" %in% names(r$types))
  fx <- .ck_fixture()
  expect_length(fx, 111L)
  for (d in names(.ck_counts)) {
    l <- r$types[[d]]
    code <- vapply(l, `[[`, "", "code")
    expect_identical(code, sprintf("[%s%02d]", d, seq_along(l)))
    for (e in l) {
      k <- gsub("[][]", "", e$code)
      expect_identical(e$formula, fx[[k]], info = k)
      expect_true(nzchar(e$name) && !grepl("[\u0400-\u04ff]", e$name), info = k)
      expect_true(grepl("^[\u0400-\u04ff]", e$name_native), info = k)
      expect_true(is.numeric(e$page) && e$page >= 100 && e$page <= 123, info = k)
      expect_identical(e$tests$all_of[[1L]]$type_formula_kidpr$type, e$code)
      expect_true(all(unlist(e$ignore) %in% character(0)), info = k)
    }
  }
  all <- unlist(unname(r$types[names(.ck_counts)]), recursive = FALSE)
  codes_with <- function(field) vapply(Filter(function(e) length(e[[field]]), all), `[[`, "", "code")
  # Q14: the BCA under the solonetz horizon is not tested
  expect_identical(codes_with("optional"), c(sprintf("[CK%02d]", 1:12), "[CQ09]", "[CQ10]", sprintf("[CS%02d]", 16:19),
                                             sprintf("[CT%02d]", 14:17)))
  for (e in Filter(function(e) length(e$optional), all)) expect_identical(unlist(e$optional), "BCA")
  # Q14: the rock R and the gleyed substrate DG are tested where they name the type
  expect_identical(codes_with("test_terminals"), c(sprintf("[CO%02d]", c(2, 4, 5, 8, 10, 11)), sprintf("[CP%02d]", 1:13)))
  for (e in Filter(function(e) length(e$test_terminals), all))
    expect_identical(unlist(e$test_terminals), if (startsWith(e$code, "[CO")) "DG" else "R", info = e$code)
  # Q15: S over Q, BT exposed or under PB, the BH of the gley Al-Fe-humus soils
  expect_identical(codes_with("near_identity"), c("[CL01]", "[CQ01]", "[CR04]", "[CR05]", "[CR07]", "[CS10]"))
  expect_identical(r$types$CL[[1L]]$near_identity, list(G = "Q"))
  expect_identical(r$types$CR[[7L]]$near_identity, list(BH = "BHF"))
  for (k in c(1L, 4L, 5L)) expect_identical((if (k == 1L) r$types$CQ else r$types$CR)[[k]]$near_identity, list(BI = "BT"))
  # the turbozems: the agro types first, the post-solonetzic ones (TUR at the
  # surface) last, as chapter 4 draws them
  expect_identical(vapply(r$types$CT, function(e) sub("[-[].*$", "", e$formula), "")[14:17], rep("TUR", 4L))
  # Hugo's decisions on the readings of this release (2026-11-02)
  rd <- yaml::read_yaml(system.file("rules", "kidpr2008", "readings.yaml", package = "soilKey"))$readings
  top <- vapply(rd, `[[`, "", "topic")
  mine <- c("The secondary solonchaks and an S laid over a horizon", "The fragments of TUR with no record",
            "Doubles among the formulas of CK to CT", "Profiles of CK to CT with a division and no type")
  expect_true(all(mine %in% top))
  for (x in rd[top %in% mine]) expect_match(x$reading, "\\(Hugo, 2026-11-02\\)\\.$", info = x$topic)
})


# --- one profile per type -------------------------------------------------------------

test_that("every formula of CK to CT, as a profile in the Russian notation, keys to its division and its type", {
  skip_on_cran()
  fx <- .ck_fixture()
  names_of <- unlist(lapply(unname(load_rules("kidpr2008")$types[names(.ck_counts)]), function(l)
    stats::setNames(vapply(l, `[[`, "", "name_native"), vapply(l, `[[`, "", "code"))))
  inter <- c(CO02 = "[CO01]", CO04 = "[CO03]", CO10 = "[CO09]", CP10 = "[CP09]", CP11 = "[CP06]")
  off <- character(0)
  for (k in names(fx)) {
    f <- fx[[k]]
    variants <- list(full = .ck_tokens(f))
    if (grepl("\\([A-Z]", f)) variants$no_optional <- .ck_tokens(f, drop_optional = TRUE)
    if (grepl("ASN|BSN", f) && grepl("BCA", f)) variants$no_BCA <- .ck_tokens(f, drop = "BCA")
    for (vn in names(variants)) {
      r <- .ck_of(variants[[vn]])
      info <- paste(k, vn, paste(variants[[vn]], collapse = " "))
      if (!identical(.ck_type(r), sprintf("[%s]", k))) {
        off <- c(off, sprintf("%s %s: %s", k, vn, paste(r$levels$code, collapse = "/")))
        next
      }
      expect_identical(r$levels$code[1:2], c("C", substr(k, 1L, 2L)), info = info)
      expect_length(r$provisional_because, 0L)
      expect_false(any(grepl("not provided|undecided", r$warnings)), info = info)
      # the doubles: the gleyed substrate and the carbonate rock name the
      # type, the formula without them is an intergrade
      ev <- r$trace$types[[sprintf("[%s]", k)]]$evidence[[1L]]$evidence
      expect_identical(as.character(ev$intergrades), if (k %in% names(inter)) inter[[k]] else character(0), info = info)
      if (vn == "full") expect_identical(r$levels$name_native[3L], names_of[[sprintf("[%s]", k)]], info = info)
    }
  }
  # without its BCA, a thin TUR of solonetz fragments under P or PU over the
  # parent material is not directly over a middle horizon (C.6, the first branch): the key to
  # divisions has no exit for it (the division takes it before the types)
  expect_identical(off, c("CS16 no_BCA: C", "CS17 no_BCA: C"))
})

test_that("the example of each division CK to CT reaches its type; the chemozems stop at the division", {
  skip_on_cran()
  ex <- c(CK = "[CK05]", CL = "[CL01]", CM = "[CM01]", CN = "[CN01]", CO = "[CO03]", CP = "[CP06]", CQ = "[CQ04]",
          CR = "[CR10]", CS = "[CS07]", CT = "[CT01]")
  for (d in names(ex)) {
    r <- .ck_class(make_kidpr2008_example(d))
    expect_identical(r$levels$level, c("trunks", "divisions", "types"), info = d)
    expect_identical(r$levels$code[2:3], c(d, ex[[d]]), info = d)
    expect_identical(r$name, r$levels$name[3L])
  }
  r <- .ck_class(make_kidpr2008_example("CU"))
  expect_identical(r$levels$level, c("trunks", "divisions"))
  expect_false(any(grepl("not provided", r$warnings)))
})


# --- Q14: what a formula tests ---------------------------------------------------------

test_that("Q14: the BCA of the solonetzes, the rock R and Rca, the gleyed substrate DG", {
  skip_on_cran()
  # the BCA under the solonetz horizon is optional, and its marks expected
  expect_identical(.ck_codes(c("SEL:10", "BSN:30", "C:30")), "C/CK/[CK02]")
  expect_identical(.ck_codes(c("SEL:10", "BSN:30", "BCA:30", "C:30")), "C/CK/[CK02]")
  expect_identical(.ck_codes(c("ASN:30", "Cca:30")), "C/CQ/[CQ10]")
  # the quasigley horizon is a horizon of the division: it names the type
  expect_identical(.ck_codes(c("SEL:10", "ASN:30", "BCA:30", "Q:30", "C:30")), "C/CK/[CK03]")
  expect_identical(.ck_codes(c("PU:30", "ASN:30", "Q:30", "CQ:30")), "C/CK/[CK11]")
  # the rock is tested; carbonate rock names the carbolithozems, and the
  # formula with R is their intergrade
  r <- .ck_of(c("AU:15", "Cca:5", "Rca:10"))
  expect_identical(.ck_type(r), "[CP11]")
  expect_match(grep("^Intergrade", r$warnings, value = TRUE), "[CP06] AU-(C)-R", fixed = TRUE)
  expect_identical(.ck_codes(c("AU:15", "C:5", "R:10")), "C/CP/[CP06]")
  expect_false(isTRUE(.ck_of(c("AU:15", "C:5", "R:10"))$trace$types[["[CP11]"]]$passed))
  # R(ca): the carbonate mark expected, not required; AY has no carbolithozem
  expect_identical(.ck_codes(c("PU:25", "Rca:10")), "C/CP/[CP12]")
  expect_identical(.ck_codes(c("PU:25", "R:10")), "C/CP/[CP12]")
  expect_identical(.ck_codes(c("AY:15", "Rca:10")), "C/CP/[CP05]")
  # the gleyed substrate DG names the gley eluvozems; D is not tested
  r <- .ck_of(c("O:6", "EL:15", "DG:30"))
  expect_identical(.ck_type(r), "[CO02]")
  expect_identical(r$trace$types[["[CO02]"]]$evidence[[1L]]$evidence$intergrades, "[CO01]")
  expect_identical(.ck_codes(c("O:6", "EL:15", "D:30")), "C/CO/[CO01]")
  expect_identical(.ck_codes(c("O:6", "EL:15", "C:30")), "C/CO/[CO01]")
  expect_identical(.ck_codes(c("O:6", "E:10", "DG:30")), "C/CO/[CO06]")
  # PT with or without its remnant T
  expect_identical(.ck_codes(c("PT:25", "EL:15", "DG:30")), "C/CO/[CO11]")
})

test_that("Q14 and Q20: the fragments of TUR, from the notation, the data, or the ESP", {
  skip_on_cran()
  expect_identical(.ck_codes(c("PU:30", "TUR[SEL+ASN]:30", "BCA:30", "Cca:30")), "C/CS/[CS16]")
  expect_identical(.ck_codes(c("TUR[AJ+SEL+BSN]:50", "BCA:30", "Cca:30")), "C/CT/[CT15]")
  # other fragments, or fragments short of the formula's: not the type
  .ck_q15(.ck_of(c("PU:30", "TUR[SEL+BSN]:30", "BCA:30", "Cca:30")), "CS", "[CS16]")
  .ck_q15(.ck_of(c("TUR[SEL+ASN]:50", "BCA:30", "Cca:30")), "CT", "[CT14]")
  # a TUR written with no fragments in the notation: not a solonetzic type
  .ck_q15(.ck_of(c("PU:30", "TUR:30", "BCA:30", "Cca:30")), "CS", "[CS16]")
  # the fragments from the column turbated_fragments
  r <- .ck_of(c("PU:30", "TUR:30", "BCA:30", "Cca:30"), turbated_fragments = c(NA, "SEL; ASN", NA, NA))
  expect_identical(.ck_type(r), "[CS16]")
  expect_length(r$provisional_because, 0L)
  # with neither, a high ESP leaves the solonetzic type undecided; a low one
  # does not
  r <- .ck_of(c("PU:30", "TUR:30", "BCA:30", "Cca:30"), na_cmol = c(NA, 6, NA, NA), cec_cmol = c(NA, 25, NA, NA))
  .ck_und(r, "[CS16]")
  .ck_q15(.ck_of(c("PU:30", "TUR:30", "BCA:30", "Cca:30"), esp_pct = c(NA, 5, NA, NA)), "CS")
  r <- .ck_of(c("TUR:50", "BCA:30", "Qs:30", "Cca:30"), esp_pct = c(30, NA, NA, NA))
  .ck_und(r, c("[CT16]", "[CT17]"))
})


# --- Q13: closed on the division ---------------------------------------------------------

test_that("Q13: a horizon of the division outside the formula blocks it; a foreign one is ignored", {
  skip_on_cran()
  # a TUR or a G in the division's formulas blocks the formulas without it
  expect_identical(.ck_codes(c("PU:30", "TUR:50", "BCA:30", "Cca:30")), "C/CT/[CT10]")
  expect_identical(.ck_codes(c("PB:25", "BM:40", "BCA:30", "Cca:30")), "C/CR/[CR09]")
  expect_false(isTRUE(.ck_of(c("PB:25", "BM:40", "BCA:30", "Cca:30"))$trace$types[["[CR08]"]]$passed))
  expect_identical(.ck_codes(c("PB:25", "BCA:35", "Q:30", "CQ:30")), "C/CR/[CR11]")
  # a horizon foreign to the division is ignored (it may name a subtype): a
  # BM under the lithozem's humus horizon, a BFM or a BMK under the plough,
  # a BCA between S and Q
  expect_identical(.ck_codes(c("AY:15", "BM:10", "R:10")), "C/CP/[CP05]")
  expect_identical(.ck_codes(c("PB:22", "BFM:40", "C:30")), "C/CR/[CR01]")
  expect_identical(.ck_codes(c("P:25", "BMK:30", "CAT:30", "C:30")), "C/CS/[CS15]")
  expect_identical(.ck_codes(c("S:8", "BCA:30", "Q:30", "CQ:30")), "C/CL/[CL01]")
  # a natural remnant under the plough: written in the formula, or ignored
  expect_identical(.ck_codes(c("PU:30", "AU:15", "Q:30", "CQ:30")), "C/CM/[CM03]")
  expect_identical(.ck_codes(c("PU:30", "H:15", "Q:30", "CQ:30")), "C/CM/[CM04]")
  expect_identical(.ck_codes(c("P:25", "AY:10", "C:30")), "C/CN/[CN08]")
  expect_identical(.ck_codes(c("PU:30", "AU:15", "C:30")), "C/CN/[CN09]")
  # an organic layer over a thicker humus horizon is left out (p. 37)
  expect_identical(.ck_codes(c("O:3", "AY:15", "C:40")), "C/CN/[CN01]")
})

test_that("agro before natural, most specific first, in the order classify_kidpr2008() walks", {
  skip_on_cran()
  r <- load_rules("kidpr2008")
  for (d in names(.ck_counts)) {
    sch <- .kidpr_ty_parse(r$types[[d]])
    sp <- vapply(sch$specs[sch$trial], function(s) as.numeric(s$specificity[1:2]), c(0, 0))
    ag <- sch$agro[sch$trial]
    # more required slots first, then more required marks (Rca before R),
    # then the agro types
    expect_false(is.unsorted(-(sp[1L, ] * 100 + sp[2L, ])), info = d)
    key <- paste(sp[1L, ], sp[2L, ])
    for (s in unique(key)) expect_false(is.unsorted(-ag[key == s]), info = d)
  }
  rr <- .ck_of(c("PU:30", "ASN:30", "BCA:30", "Cca:30"))
  expect_identical(.ck_type(rr), "[CK09]")
  expect_identical(names(rr$trace$types)[1L], .kidpr_ty_parse(r$types$CK)$trial[1L])
})


# --- Q15: near-identity readings and no type ---------------------------------------------

test_that("Q15: the near-identity readings", {
  skip_on_cran()
  # S over Q is a gley solonchak
  expect_identical(.ck_codes(c("S:8", "Q:30", "CQ:30")), "C/CL/[CL01]")
  expect_identical(.ck_codes(c("S:8", "Gs:30", "CGs:30")), "C/CL/[CL01]")
  # a BT under PB is read as BI
  expect_identical(.ck_codes(c("PB:25", "BT:30", "C:30")), "C/CR/[CR04]")
  expect_identical(.ck_codes(c("PB:25", "BT:30", "G:30", "CG:30")), "C/CR/[CR05]")
  # the BH of the gley Al-Fe-humus types is a BHF of either kind
  for (b in c("BH", "BF", "BHF")) {
    expect_identical(.ck_codes(c("PB:25", paste0(b, ":30"), "G:30", "CG:30")), "C/CR/[CR07]", info = b)
    expect_identical(.ck_codes(c("P:25", paste0(b, ":30"), "G:30", "CG:30")), "C/CS/[CS10]", info = b)
  }
  expect_identical(.ck_codes(c("PB:25", "BH:30", "C:30")), "C/CR/[CR06]")
})

test_that("Q15: a combination no formula provides for stops at the division, with the nearest formulas", {
  skip_on_cran()
  cases <- list(
    list(c("AU:30", "SEL:10", "BSN:30", "BCA:30", "Cca:30"), "CK", c("[CK05]", "[CK06]")),  # AU-SEL-BSN
    list(c("AU:30", "ASN:30", "BCA:30", "Cca:30"), "CK", "[CK05]"),                          # no SEL under AU
    list(c("PB:25", "BSN:30", "BCA:30", "Cca:30"), "CK", "[CK10]"),                          # PB over BSN
    list(c("SS:8", "Q:30", "CQ:30"), "CL", "[CL02]"),                                         # SS over Q
    list(c("O:8", "Q:30", "CQ:30"), "CM", c("[CM01]", "[CM02]")),                            # O over Q (C.14)
    list(c("TJ:20", "C:30"), "CN", "[CN01]"),                                                 # TJ (C.19, the third branch)
    list(c("V:30", "C:30"), "CQ", "[CQ01]"),                                                  # V at the surface
    list(c("P:25", "BI:30", "C:30"), "CS", "[CS07]"),                                         # P over BI
    list(c("PT:25", "Q:30", "CQ:30"), "CS", "[CS04]"),                                        # PT over Q
    list(c("TUR:50", "BCA:30", "Cca:30"), "CT", c("[CT09]", "[CT10]"))                       # TUR, no fragments
  )
  for (k in cases) .ck_q15(.ck_of(k[[1L]]), k[[2L]], k[[3L]])
  # an S over a formed profile of its own layer is a secondary solonchak
  expect_identical(.ck_codes(c("S:8", "AU:20", "BCA:30", "Cca:30")), "C/CL/[CL03]")
})


# --- Hugo's decisions on the readings (2026-11-02) ---------------------------------------

test_that("in the types of CL an S laid over another horizon is the S of the formula", {
  skip_on_cran()
  # the key to divisions sends these profiles to CL, as before (C.3)
  r <- .ck_of(c("AY+S:8", "C:40"))
  expect_identical(r$levels$code, c("C", "CL", "[CL03]"))
  expect_length(r$provisional_because, 0L)
  expect_false(any(grepl("not provided|undecided", r$warnings)))
  ev <- r$trace$types[["[CL03]"]]$evidence[[1L]]$evidence
  expect_identical(ev$matched$code, "S")
  expect_identical(.ck_codes(c("AU+S:8", "G:30", "CG:30")), "C/CL/[CL01]")
  expect_identical(.ck_codes(c("AU+S:8", "Q:30", "CQ:30")), "C/CL/[CL01]")
  expect_identical(.ck_codes(c("G+S:10", "CG:30")), "C/CL/[CL01]")
  expect_identical(.ck_codes(c("AY+S:15", "BT:30", "C:30")), "C/CL/[CL03]")
  # outside CL the overlay stays a feature of its layer
  expect_identical(.ck_codes(c("AU:30", "BCA+S:30", "C:30")), "C/CI/[CI01]")
  v <- .kidpr_dv_pedon(.ck_ped(c("AY+S:8", "C:40")))
  expect_false("S" %in% .kidpr_ty_table(v)$code)
  expect_identical(.kidpr_ty_table(v, "CL")$code[1:2], c("S", "AY"))
})

test_that("an exposed BT counts as BI at C.20: an abrazem, CQ01", {
  skip_on_cran()
  r <- .ck_of(c("BT:30", "C:30"))
  expect_identical(r$levels$code, c("C", "CQ", "[CQ01]"))
  expect_length(r$provisional_because, 0L)
  # G is foreign to the abrazems (a subtype)
  expect_identical(.ck_codes(c("BT:30", "G:30", "CG:30")), "C/CQ/[CQ01]")
  # not under a humus or eluvial horizon (C.4), nor when the description
  # starts below 0 cm (Q19)
  expect_identical(.ck_codes(c("AY:15", "BT:30", "C:30")), "C")
  expect_identical(.ck_codes(c("EL:10", "BT:30", "C:30")), "C/CA")
  p <- .ck_ped(c("BT:30", "C:30"))
  p$horizons$top_cm <- p$horizons$top_cm + 20
  p$horizons$bottom_cm <- p$horizons$bottom_cm + 20
  r <- .ck_class(p)
  expect_false(identical(r$levels$code[2L], "CQ"))
  expect_true("divisions:CQ" %in% r$provisional_because)
})


# --- undecided layers ---------------------------------------------------------------------

test_that("an undecided layer leaves the type undecided between its candidates", {
  skip_on_cran()
  .ck_und(.ck_of(c("SEL:10", "ASN:30", "BCA:30", "NA:30", "C:30")), c("[CK03]", "[CK01]"))
  .ck_und(.ck_of(c("S:8", "NA:30", "C:30")), c("[CL01]", "[CL03]"))
  .ck_und(.ck_of(c("PU:30", "TUR:50", "NA:30", "C:30")), c("[CT03]", "[CT04]", "[CT06]", "[CT10]", "[CT02]"))
  r <- .ck_of(c("AU:15", "NA:5", "R:10"))
  .ck_und(r, "[CP06]")
  expect_true("designation" %in% r$missing_data)
  # on the data: a bluish layer whose moist value is not recorded, under the
  # BT of an agroabrazem: G (CR05) or no horizon (CR04)
  p <- .ck_P(.ck_L(0, 25, designation = "Ap", structure_type = "cloddy", boundary_distinctness = "abrupt",
                   boundary_topography = "smooth"),
             .ck_BT(25, 80, designation = "Bt"), .ck_moist(80, 120, "5GY", NA, 1, designation = "Bg"),
             .ck_L(120, 150, designation = "C"), site = list(land_use = "arable"))
  f <- identify_horizons(p, "kidpr2008")
  expect_identical(f$code[1:2], c("PB", "BT"))
  expect_true(is.na(f$passed[3L]))
  r <- .ck_class(p)
  expect_identical(r$levels$code, c("C", "CR"))
  .ck_und(r, c("[CR05]", "[CR04]"))
  expect_true("munsell_value_moist" %in% r$missing_data)
})


# --- the data route -------------------------------------------------------------------------

test_that("undesignated twins reach their division and type through the key to horizons", {
  skip_on_cran()
  ar <- list(land_use = "arable")
  ap <- function(...) .ck_AY(0, 25, designation = "Ap", boundary_distinctness = "abrupt", boundary_topography = "smooth", ...)
  twins <- list(
    CK = list("[CK02]", .ck_P(.ck_L(0, 10, designation = "E", munsell_value_dry = 6, munsell_chroma_dry = 2, structure_type = "platy",
                                    structure_grade = "weak", ph_h2o = 7.3, oc_pct = 1, clay_pct = 14, clay_films_amount = NA),
                              .ck_L(10, 32, designation = "Btn", munsell_value_dry = 4, munsell_chroma_dry = 3, structure_type = "columnar",
                                    structure_grade = "strong", clay_pct = 42, clay_films_amount = "many", ph_h2o = 8.6, na_cmol = 6,
                                    cec_cmol = 25),
                              .ck_L(32, 70, designation = "Bk", munsell_value_dry = 7, munsell_chroma_dry = 3, structure_type = "prismatic",
                                    structure_grade = "weak", clay_pct = 34, caco3_pct = 9, secondary_carbonates_form = "pseudomycelia",
                                    ph_h2o = 8.8, clay_films_amount = NA),
                              .ck_L(70, 140, designation = "Ck", munsell_value_dry = 7, clay_pct = 30, caco3_pct = 10, ph_h2o = 8.9,
                                    clay_films_amount = NA, secondary_carbonates_form = NA), site = list(land_use = "pasture"))),
    CL = list("[CL01]", .ck_P(.ck_L(0, 10, designation = "Az", soluble_salts_pct = 3, ph_h2o = 8.4),
                              .ck_moist(10, 40, "5GY", 6, 1, designation = "Bgz", soluble_salts_pct = 1.5),
                              .ck_moist(40, 80, "5GY", 5, 1, designation = "Cg"), site = list(drainage_class = "poorly drained"))),
    CM = list("[CM01]", .ck_P(.ck_AU(0, 30, designation = "Ah"), .ck_moist(30, 70, "5Y", 5, 3, designation = "Bg"),
                              .ck_moist(70, 120, "5Y", 5, 3, designation = "Cg"))),
    CN = list("[CN01]", .ck_P(.ck_AY(0, 20, designation = "Ah"), .ck_L(20, 80, designation = "C"))),
    CO = list("[CO07]", .ck_P(.ck_AY(0, 15, designation = "Ah"), .ck_E(15, 35, designation = "E", oc_pct = 0.4),
                              .ck_L(35, 100, designation = "2C"))),
    CP = list("[CP05]", .ck_P(.ck_AY(0, 18, designation = "Ah"), .ck_L(18, 40, designation = "R", rock_origin = "granite"))),
    CQ = list("[CQ04]", .ck_P(.ck_B(0, 40, designation = "Bw", munsell_hue_dry = "10YR", munsell_chroma_dry = 4, oc_pct = 0.4),
                              .ck_L(40, 100, designation = "C"), site = list(anthropic_truncation = TRUE))),
    CR = list("[CR04]", .ck_P(.ck_L(0, 25, designation = "Ap", structure_type = "cloddy", boundary_distinctness = "abrupt",
                                    boundary_topography = "smooth"),
                              .ck_BT(25, 80, designation = "Bt"), .ck_L(80, 130, designation = "C"), site = ar)),
    CS = list("[CS07]", .ck_P(ap(), .ck_BT(25, 80, designation = "Bt"), .ck_L(80, 130, designation = "C"), site = ar)),
    CT = list("[CT01]", .ck_P(ap(), .ck_L(25, 80, designation = "Bp", foreign_fragments_pct = 90, structure_type = "blocky"),
                              .ck_L(80, 130, designation = "C"), site = ar))
  )
  for (d in names(twins)) {
    p <- twins[[d]][[2L]]
    expect_null(p$site$designation_system)
    expect_true(all(identify_horizons(p, "kidpr2008")$route == "data"), info = d)
    r <- .ck_class(p)
    expect_identical(r$levels$code, c("C", d, twins[[d]][[1L]]), info = d)
    expect_length(r$provisional_because, 0L)
    expect_true(any(grepl("key 2.1 run on", r$warnings)), info = d)
  }
  # a TUR on the data with no record of its fragments and no sodium: not a
  # post-solonetzic type
  p <- twins$CT[[2L]]
  expect_false(isTRUE(.ck_class(p)$trace$types[["[CT14]"]]$passed))
})
