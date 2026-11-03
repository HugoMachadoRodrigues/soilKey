# v0.9.338: the types of the divisions CA to CJ of the Russian soil
# classification (Field Guide of 2008, chapter 4): the profile formula of each
# type (inst/rules/kidpr2008/types/), matched on the diagnostic horizons the
# divisions read (R/types-kidpr.R), Hugo's decisions of 2026-11-02 (Q13 closed
# formulas, Q14 what a formula tests, Q15 no type). Profiles are written in the
# Russian notation, except the undesignated twins, which go through the key to
# horizons on the data.

# --- builders ------------------------------------------------------------------------

# A pedon in the Russian notation from tokens "DESIGNATION:thickness" (top-down
# from `top`), with site fields and horizon columns (one value per layer).
.ty_ped <- function(tokens, site = list(), top = 0, ...) {
  d <- sub(":[0-9.]+$", "", tokens)
  th <- as.numeric(sub("^.*:", "", tokens))
  bot <- top + cumsum(th)
  h <- data.frame(designation = d, top_cm = c(top, bot[-length(bot)]), bottom_cm = bot,
                  stringsAsFactors = FALSE)
  h$designation[h$designation == "NA"] <- NA_character_
  extra <- list(...)
  for (k in names(extra)) h[[k]] <- extra[[k]]
  PedonRecord$new(site = utils::modifyList(list(id = "ru-type", designation_system = "kidpr"), site),
                  horizons = ensure_horizon_schema(data.table::as.data.table(h)))
}
.ty_class <- function(p, ...) classify_kidpr2008(p, on_missing = "silent", ...)
.ty_type <- function(r) if (nrow(r$levels) >= 3L) r$levels$code[3L] else NA_character_
.ty_div <- function(r) if (nrow(r$levels) >= 2L) r$levels$code[2L] else NA_character_
.ty_of <- function(tokens, ...) .ty_class(.ty_ped(tokens, ...))
.ty_counts <- c(CA = 27L, CB = 17L, CC = 3L, CD = 13L, CE = 7L, CF = 4L, CG = 3L, CH = 9L, CI = 20L, CJ = 3L)
.ty_agro <- c("P", "PU", "PB", "PT", "PTR")
# the formulas of chapter 4 for CA to CJ, from the fixture of the formulas
.ty_fixture <- function() {
  path <- testthat::test_path("fixtures", "kidpr2008-formulas.tsv")
  fx <- utils::read.delim(path, comment.char = "#", quote = "", stringsAsFactors = FALSE,
                          encoding = "UTF-8", colClasses = "character")
  fx <- fx[grepl("^ch4 C[A-J]", fx$source), ]
  stats::setNames(fx$formula, sub("^ch4 ", "", fx$source))
}
# A formula as a profile in the Russian notation, with plausible thicknesses:
# the marks as written (BTg, Gca, Cca,s, C with the permafrost mark), the
# optional features in parentheses left out, the optional horizons written
# or left out, the horizons in `drop` left out, a BT or BM of 40 cm written as
# two sub-horizons.
.ty_th <- c(O = 6, T = 30, TJ = 20, H = 20, AY = 15, AU = 35, AJ = 15, AO = 8, AK = 10, AH = 20, P = 25,
            PU = 30, PT = 25, E = 10, EL = 15, AEL = 12, BEL = 15, BT = 40, BHF = 30, BF = 30, BFM = 35,
            BM = 40, CRM = 30, BPL = 30, CR = 30, BCA = 35, CAT = 35, BI = 35, V = 40, CRH = 30, BMK = 25,
            G = 30, Q = 30, F = 10, C = 30, CLM = 30)
.ty_tokens <- function(formula, drop_optional = FALSE, drop = character(0)) {
  out <- character(0)
  prev <- ""
  for (x in soilKey:::.kidpr_split_outside(formula, "-")) {
    if (grepl("^\\(.*\\)$", x)) {
      if (drop_optional) next
      x <- sub("^\\((.*)\\)$", "\\1", x)
    }
    x <- gsub("\\((ca|s|g)\\)", "", x)
    m <- sub("^([A-Z]+).*$", "\\1", x)
    if (m %in% c("CG", "CQ")) m <- "C"
    if (m %in% drop) next
    t <- .ty_th[[m]]
    if (m == "T" && prev == "PT") t <- 15
    out <- c(out, if (x %in% c("BT", "BM")) c(paste0(x, "1:", t / 2), paste0(x, "2:", t / 2))
                  else sprintf("%s:%s", x, t))
    prev <- m
  }
  out
}

# Undesignated layers for the data route (designation_system unset; FAO
# designations as hints), as in the tests of the key to horizons.
.ty_L <- function(top, bottom, ...) {
  d <- list(top_cm = top, bottom_cm = bottom, munsell_hue_dry = "10YR", munsell_value_dry = 6,
            munsell_chroma_dry = 4, structure_type = "massive", structure_grade = "structureless",
            clay_pct = 20, silt_pct = 40, sand_pct = 40, oc_pct = 0.1, ph_h2o = 6.0,
            clay_films_amount = "none", effervescence_class = "none", secondary_carbonates_form = "none")
  a <- list(...)
  d[names(a)] <- a
  d
}
.ty_P <- function(..., site = list()) {
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
.ty_AY <- function(top = 0, bottom = 15, ...) .ty_L(top, bottom, munsell_value_dry = 5, munsell_chroma_dry = 2,
                                                   structure_type = "granular", structure_grade = "moderate",
                                                   ph_h2o = 5.0, oc_pct = 2, clay_pct = 15, silt_pct = 50,
                                                   sand_pct = 35, ...)
.ty_AU <- function(top, bottom, ...) .ty_AY(top, bottom, munsell_value_dry = 3, munsell_chroma_dry = 1,
                                            ph_h2o = 7.2, oc_pct = 4, ...)
.ty_AJ <- function(top, bottom, ...) .ty_AY(top, bottom, munsell_value_dry = 6, ph_h2o = 7.6,
                                            structure_type = "crumb", structure_grade = "weak", ...)
.ty_B <- function(top, bottom, ...) .ty_L(top, bottom, munsell_hue_dry = "7.5YR", munsell_value_dry = 5,
                                          munsell_chroma_dry = 6, structure_type = "blocky",
                                          structure_grade = "moderate", clay_pct = 25, ...)
.ty_O <- function(top, bottom, ...) .ty_L(top, bottom, munsell_hue_dry = "7.5YR", munsell_value_dry = 3,
                                          munsell_chroma_dry = 3, plant_residues_pct = 80,
                                          peat_decomposition_pct = 20, oc_pct = 40, ...)
.ty_sand <- function(top, bottom, ...) .ty_L(top, bottom, clay_pct = 3, silt_pct = 5, sand_pct = 92, ...)
.ty_moist <- function(top, bottom, hue, v, c, ...) .ty_L(top, bottom, munsell_hue_moist = hue,
                                                         munsell_value_moist = v, munsell_chroma_moist = c,
                                                         munsell_hue_dry = NA, munsell_value_dry = NA,
                                                         munsell_chroma_dry = NA, ...)
.ty_E <- function(top, bottom, ...) .ty_L(top, bottom, munsell_value_dry = 7, munsell_chroma_dry = 2,
                                          structure_type = "platy", structure_grade = "weak", clay_pct = 10,
                                          silt_pct = 55, sand_pct = 35, ...)
.ty_BT <- function(top, bottom, ...) .ty_B(top, bottom, clay_pct = 30, silt_pct = 40, sand_pct = 30,
                                           structure_type = "prismatic", clay_films_amount = "common", ...)
.ty_BCA <- function(top, bottom, ...) .ty_L(top, bottom, munsell_value_dry = 7, munsell_chroma_dry = 3,
                                            structure_type = "prismatic", structure_grade = "weak", caco3_pct = 15,
                                            secondary_carbonates_form = "pseudomycelia", ph_h2o = 8.2, ...)


# --- the registry and the rules ------------------------------------------------------

test_that("the registry has the type level; the 106 types are clean and as the fixture writes them", {
  skip_on_cran()
  expect_identical(.system_get("kidpr2008")$levels, c("trunks", "divisions", "types"))
  v <- .validate_rules("kidpr2008")
  expect_length(v$missing, 0L)
  expect_length(v$mixed, 0L)
  expect_length(v$unsuffixed, 0L)
  expect_length(v$malformed, 0L)
  expect_length(v$codes, 0L)
  expect_true("type_formula_kidpr" %in% .collect_rule_predicates(load_rules("kidpr2008")$types))
  r <- load_rules("kidpr2008")
  # the divisions CA to CJ (CK to CT: test-v09339-kidpr2008-types-ck-ct.R)
  expect_true(all(names(.ty_counts) %in% names(r$types)))
  r$types <- r$types[names(.ty_counts)]
  expect_identical(lengths(r$types)[names(.ty_counts)], .ty_counts)
  expect_identical(sum(lengths(r$types)), 106L)
  fx <- .ty_fixture()
  expect_length(fx, 106L)
  for (d in names(r$types)) {
    l <- r$types[[d]]
    code <- vapply(l, `[[`, "", "code")
    # soilKey's codes, in brackets, in the order of chapter 4
    expect_identical(code, sprintf("[%s%02d]", d, seq_along(l)))
    agro <- vapply(l, function(e) sub("-.*$", "", e$formula) %in% .ty_agro, NA)
    # the natural types first, then the agro types
    expect_false(is.unsorted(agro), info = d)
    for (e in l) {
      k <- gsub("[][]", "", e$code)
      expect_identical(e$formula, fx[[k]], info = k)
      expect_true(nzchar(e$name) && !grepl("[Ѐ-ӿ]", e$name), info = k)
      expect_true(grepl("^[Ѐ-ӿ]", e$name_native), info = k)
      expect_true(is.numeric(e$page) && e$page >= 76 && e$page <= 100, info = k)
      expect_identical(e$tests$all_of[[1L]]$type_formula_kidpr$type, e$code)
    }
  }
  # BEL optional where it is not the diagnostic eluvial horizon (Q14)
  all <- unlist(unname(r$types), recursive = FALSE)
  opt <- vapply(Filter(function(e) length(e$optional), all), `[[`, "", "code")
  expect_identical(opt, sprintf("[CA%02d]", c(1:6, 16:19, 22)))
  for (e in all) if (length(e$optional)) expect_identical(unlist(e$optional), "BEL")
  # BEL closes only the formulas where it is diagnostic, and BCA does not
  # close the formulas of CD that do not write it (Hugo, 2026-11-02)
  ign_of <- function(h) vapply(Filter(function(e) h %in% unlist(e$ignore), all), `[[`, "", "code")
  ign <- ign_of("BEL")
  expect_identical(ign, c(sprintf("[CA%02d]", c(10, 12:15, 23:27)), sprintf("[CD%02d]", c(1:8, 10:13))))
  expect_identical(ign_of("BCA"), sprintf("[CD%02d]", c(1:7, 9:12)))
  for (e in all) expect_true(all(unlist(e$ignore) %in% c("BEL", "BCA")), info = e$code)
  # in CA and CD, a formula writes BEL or ignores it
  for (e in all) if (substr(e$code, 2L, 3L) %in% c("CA", "CD"))
    expect_true(xor(grepl("BEL", e$formula), e$code %in% ign), info = e$code)
  sch <- .kidpr_ty_parse(r$types$CA)
  expect_false("BEL" %in% sch$vocabularies[["[CA10]"]])
  expect_true(all(c("BEL", "EL", "BT", "G") %in% sch$vocabularies[["[CA08]"]]))
  # the near-identity readings of Q15, on the types they concern, and the EL
  # that counts with E in CB and CE (Hugo, 2026-11-02)
  ni <- Filter(function(e) length(e$near_identity), all)
  expect_identical(vapply(ni, `[[`, "", "code"),
                   c("[CB05]", sprintf("[CB%02d]", 7:17), sprintf("[CE%02d]", 5:7), "[CH02]", "[CJ01]"))
  expect_identical(r$types$CJ[[1L]]$near_identity, list(BCA = "CAT"))
  expect_identical(r$types$CE[[6L]]$near_identity, list(BF = "BHF", E = "EL"))
  for (e in ni) if (grepl("^\\[C[BE]", e$code) && e$code != "[CB05]") expect_identical(e$near_identity$E, "EL", info = e$code)
  # Hugo's decisions on the types (2026-11-02), with his answers on the
  # readings of this release
  rd <- yaml::read_yaml(system.file("rules", "kidpr2008", "readings.yaml", package = "soilKey"))$readings
  top <- vapply(rd, `[[`, "", "topic")
  mine <- c("The types: formulas closed on the division", "BEL closes only the formulas where it is diagnostic",
            "The peat T of the type formulas", "Surface horizons a formula does not write",
            "Overlays in the type formulas", "The order of trial of the types")
  expect_true(all(mine %in% top))
  for (x in rd[top %in% mine]) expect_match(x$reading, "\\(Hugo, 2026-11-02\\)\\.$", info = x$topic)
})


# --- one profile per type -------------------------------------------------------------

test_that("every formula of CA to CJ, as a profile in the Russian notation, keys to its division and its type", {
  skip_on_cran()
  fx <- .ty_fixture()
  bel_opt <- sprintf("CA%02d", c(1:6, 16:19, 22))
  names_of <- unlist(lapply(unname(load_rules("kidpr2008")$types), function(l)
    stats::setNames(vapply(l, `[[`, "", "name_native"), vapply(l, `[[`, "", "code"))))
  off <- character(0)
  for (k in names(fx)) {
    f <- fx[[k]]
    variants <- list(full = .ty_tokens(f))
    # the optional horizons left out, and BEL where Q14 makes it optional
    if (grepl("\\([A-Z]", f)) variants$no_optional <- .ty_tokens(f, drop_optional = TRUE)
    if (k %in% bel_opt) variants$no_BEL <- .ty_tokens(f, drop = "BEL")
    for (vn in names(variants)) {
      r <- .ty_of(variants[[vn]])
      info <- paste(k, vn, paste(variants[[vn]], collapse = " "))
      if (!identical(.ty_type(r), sprintf("[%s]", k))) {
        off <- c(off, sprintf("%s %s: %s", k, vn, paste(r$levels$code, collapse = "/")))
        next
      }
      expect_identical(r$levels$code[1:2], c("C", substr(k, 1L, 2L)), info = info)
      expect_length(r$provisional_because, 0L)
      expect_false(any(grepl("not provided|undecided between", r$warnings)), info = info)
      if (vn == "full") expect_identical(r$levels$name_native[3L], names_of[[sprintf("[%s]", k)]], info = info)
    }
  }
  # left without its optional EL, CA24 (P-(EL)-BT-BCA-Cca) is P directly over
  # BT, an agrozem by C.6a (the division takes it before the types): CS07,
  # the BCA under BT ignored (Hugo, 2026-11-02)
  expect_identical(off, "CA24 no_optional: C/CS/[CS07]")
})

test_that("the example of each division CA to CJ reaches its type", {
  skip_on_cran()
  ex <- c(CA = "[CA04]", CB = "[CB07]", CC = "[CC02]", CD = "[CD01]", CE = "[CE01]", CF = "[CF01]",
          CG = "[CG01]", CH = "[CH01]", CI = "[CI01]", CJ = "[CJ02]")
  for (d in names(ex)) {
    r <- .ty_class(make_kidpr2008_example(d))
    expect_identical(r$levels$level, c("trunks", "divisions", "types"), info = d)
    expect_identical(r$levels$code[2:3], c(d, ex[[d]]), info = d)
    expect_identical(r$name, r$levels$name[3L])
    expect_true("types" %in% names(r$trace))
  }
  # a division with no types keeps the division as the name
  r <- .ty_class(make_kidpr2008_example("CU"))
  expect_identical(r$levels$level, c("trunks", "divisions"))
  expect_false(any(grepl("not provided", r$warnings)))
})


# --- Q14: what a formula tests ---------------------------------------------------------

test_that("Q14: BEL optional or required, parent material not tested, g and q expected, directly under", {
  skip_on_cran()
  # BEL is the only eluvial horizon of CA11, optional in CA04
  expect_identical(.ty_type(.ty_of(c("AY:15", "BEL:15", "BT:40", "C:30"))), "[CA11]")
  expect_identical(.ty_type(.ty_of(c("AY:15", "EL:15", "BT:40", "C:30"))), "[CA04]")
  expect_identical(.ty_type(.ty_of(c("AU:30", "EL:15", "BT:40", "C:30"))), "[CA09]")
  expect_identical(.ty_type(.ty_of(c("PU:30", "AU:10", "BEL:15", "BT:40", "C:30"))), "[CA20]")
  # CD09 needs its BEL; its weathering crust CLM is not tested
  expect_identical(.ty_type(.ty_of(c("AY:15", "BEL:15", "BM:40", "CLM:40"))), "[CD09]")
  expect_identical(.ty_type(.ty_of(c("AY:15", "BEL:15", "BM:40", "C:40"))), "[CD09]")
  expect_identical(.ty_type(.ty_of(c("AY:15", "BM:40", "CLM:40"))), "[CD01]")
  # the parent material: C with no permafrost mark, no carbonate, not gleyed,
  # or no parent layer at all
  expect_identical(.ty_type(.ty_of(c("O:5", "CR:30", "C:30"))), "[CG01]")
  expect_identical(.ty_type(.ty_of(c("AU:35", "BCA:40", "C:40"))), "[CI01]")
  expect_identical(.ty_type(.ty_of(c("O:8", "G:40", "C:30"))), "[CH01]")
  expect_identical(.ty_type(.ty_of(c("AY:15", "EL:15", "BEL:15", "BT:40"))), "[CA04]")
  # the g and q marks are expected, not required: the G or Q makes the type;
  # a mark with no G is no gley type
  expect_identical(.ty_type(.ty_of(c("AY:15", "EL:15", "BEL:15", "BT:40", "G:30", "C:30"))), "[CA05]")
  expect_identical(.ty_type(.ty_of(c("AH:20", "EL:15", "BT:40", "BCA:30", "Q:30", "C:30"))), "[CA15]")
  expect_identical(.ty_type(.ty_of(c("AY:15", "EL:15", "BELg:15", "BTg:40", "C:30"))), "[CA04]")
  # directly under: across a transition and sub-horizons; the typical marks
  # not recorded are noted, not required
  r <- .ty_of(c("AU1:20", "AU2:15", "AUBCA:10", "BCA1:20", "BCA2:20", "Cca:30"))
  expect_identical(.ty_type(r), "[CI01]")
  ev <- r$trace$types[["[CI01]"]]$evidence[[1L]]
  expect_identical(ev$evidence$matched$code, c("AU", "BCA"))
  expect_identical(ev$layers, c(1L, 2L, 4L, 5L))
  r <- .ty_of(c("O:5", "EL:15", "BEL:15", "BT:40", "G:30", "CG:30"))
  expect_identical(.ty_type(r), "[CA02]")
  expect_true(any(grepl("typical feature\\(s\\) g of BTg", r$trace$types[["[CA02]"]]$evidence[[1L]]$notes)))
})

test_that("Q14 on the data: up to 15 cm with no horizon between AU and BCA", {
  skip_on_cran()
  gap <- function(th) .ty_P(.ty_AU(0, 35, designation = "Ah"), .ty_L(35, 35 + th, designation = "Bw", ph_h2o = 7.4),
                            .ty_BCA(35 + th, 90, designation = "Bk"), .ty_L(90, 150, designation = "Ck", caco3_pct = 10))
  f <- identify_horizons(gap(10), "kidpr2008")
  expect_true(is.na(f$code[2]) && isFALSE(f$passed[2]))
  r <- .ty_class(gap(10))
  expect_identical(r$levels$code, c("C", "CI", "[CI01]"))
  # 20 cm: AU is not directly over BCA, and the key to divisions has no exit
  expect_identical(.ty_class(gap(20))$levels$code, "C")
})


# --- Q13: closed on the division ---------------------------------------------------------

test_that("Q13: a horizon of the division outside the formula blocks it; a foreign one is ignored", {
  skip_on_cran()
  # a grey soil with G: no formula of CA has AY-AEL-BEL-BT-G (Q15)
  r <- .ty_of(c("AY:15", "AEL:12", "BEL:15", "BT:40", "G:30", "CG:30"))
  expect_identical(r$levels$code, c("C", "CA"))
  expect_identical(r$name, r$levels$name[2L])
  expect_length(r$provisional_because, 0L)
  expect_true(all(vapply(r$trace$types, function(t) isFALSE(t$passed), NA)))
  expect_length(r$trace$types, 27L)
  w <- grep("Combination not provided by the Field Guide of 2008", r$warnings, value = TRUE)
  expect_length(w, 1L)
  expect_match(w, "AY-AEL-BEL-BT-G-C", fixed = TRUE)
  expect_match(w, "[CA05] AY-EL-BELg-BTg-G-CG, distance 1", fixed = TRUE)
  expect_match(w, "[CA06] AY-AEL-BEL-BT-C, distance 1", fixed = TRUE)
  expect_false(any(grepl("No class of the types list", r$warnings)))
  near <- r$trace$types_nearest
  expect_identical(names(near)[1:2], c("[CA05]", "[CA06]"))
  expect_true(all(vapply(near, function(t) isFALSE(t$passed) && t$distance == 1, NA)[1:2]))
  tt <- key_trace_table(r)
  expect_identical(sum(tt$phase == "types_nearest"), 3L)
  # a Q in a sod-podzolic profile: CA04 does not write it, and no formula of
  # CA writes AY-EL-BT-Q
  r <- .ty_of(c("AY:15", "EL:15", "BEL:15", "BT:40", "Q:30", "C:30"))
  expect_identical(.ty_div(r), "CA")
  expect_true(is.na(.ty_type(r)))
  expect_true(any(grepl("Combination not provided", r$warnings)))
  # a horizon foreign to the division is ignored: BM or V under the BT of a
  # sod-podzolic soil, ML under a chernozem
  for (x in list(c("AY:15", "EL:15", "BEL:15", "BT:40", "BM:30", "C:30"),
                 c("AY:15", "EL:15", "BEL:15", "BT:40", "V:30", "C:30"))) {
    r <- .ty_of(x)
    expect_identical(.ty_type(r), "[CA04]", info = paste(x, collapse = " "))
    ev <- r$trace$types[["[CA04]"]]$evidence[[1L]]$evidence
    expect_true(any(grepl("^(BM|V) \\(5\\)$", ev$ignored)))
  }
  expect_identical(.ty_type(.ty_of(c("AU:35", "BCA:40", "ML:20", "Cca:30"))), "[CI01]")
  # an overlay is a feature of its layer: a vertic BCA under AU is a chernozem,
  # a V of its own the dark slitic soil
  expect_identical(.ty_type(.ty_of(c("AU:35", "BCA+V:40", "Cca:30"))), "[CI01]")
  expect_identical(.ty_type(.ty_of(c("AU:35", "V:40", "C:30"))), "[CI04]")
})

test_that("BEL closes only the formulas where it is diagnostic (Hugo, 2026-11-02)", {
  skip_on_cran()
  # a BEL in a formula that does not write it is part of the eluvial passage
  for (k in list(list(c("AU:30", "EL:15", "BEL:15", "BT:40", "G:30", "CG:30"), "[CA10]"),
                 list(c("AY:15", "EL:15", "BEL:15", "BT:40", "BCA:30", "Cca:30"), "[CA12]"),
                 list(c("PU:30", "EL:15", "BEL:15", "BT:40", "G:30", "CG:30"), "[CA23]"),
                 list(c("AY:15", "EL:15", "BEL:15", "BM:40", "C:30"), "[CD07]"),
                 list(c("P:25", "BEL:15", "BT:40", "BCA:30", "Cca:30"), "[CA24]"))) {
    r <- .ty_of(k[[1L]])
    info <- paste(k[[1L]], collapse = " ")
    expect_identical(.ty_type(r), k[[2L]], info = info)
    expect_length(r$provisional_because, 0L)
    expect_false(any(grepl("Intergrade|not provided", r$warnings)), info = info)
    ev <- r$trace$types[[k[[2L]]]]$evidence[[1L]]$evidence
    expect_true(any(startsWith(ev$ignored, "BEL (")), info = info)
  }
  # the formulas that write BEL win where they match
  expect_identical(.ty_type(.ty_of(c("AY:15", "EL:15", "BEL:15", "BT:40", "C:30"))), "[CA04]")
  expect_identical(.ty_type(.ty_of(c("AU:30", "BEL:15", "BT:40", "C:30"))), "[CA07]")
  expect_identical(.ty_type(.ty_of(c("AY:15", "BEL:15", "BT:40", "C:30"))), "[CA11]")
  expect_identical(.ty_type(.ty_of(c("AU:30", "BEL:15", "BT:40", "G:30", "CG:30"))), "[CA08]")
  expect_identical(.ty_type(.ty_of(c("PU:30", "BEL:15", "BT:40", "G:30", "CG:30"))), "[CA21]")
  # where BEL is diagnostic it still blocks: an EL keeps AU-EL-BEL-BT from CA07
  r <- .ty_of(c("AU:30", "EL:15", "BEL:15", "BT:40", "C:30"))
  expect_identical(.ty_type(r), "[CA09]")
  expect_false(isTRUE(r$trace$types[["[CA07]"]]$passed))
  # AY-BEL-BM also matches CD01, which ignores the BEL: CD09, more specific,
  # names the type and CD01 is its intergrade
  r <- .ty_of(c("AY:15", "BEL:15", "BM:40", "CLM:30"))
  expect_identical(.ty_type(r), "[CD09]")
  expect_identical(r$trace$types[["[CD09]"]]$evidence[[1L]]$evidence$intergrades, "[CD01]")
  expect_match(grep("^Intergrade", r$warnings, value = TRUE), "[CD01] AY-BM-C", fixed = TRUE)
  # p. 44: a BEL separates the dark grey soils from the clay-illuvial
  # chernozems; the key to divisions does it: a BT under an eluvial horizon is
  # CA (C.8, C.9), a chernozem needs AU directly over BI (C.13)
  expect_identical(.ty_of(c("AU:30", "BEL:15", "BT:40", "C:30"))$levels$code, c("C", "CA", "[CA07]"))
  expect_identical(.ty_of(c("AU:30", "BI:40", "C:30"))$levels$code, c("C", "CI", "[CI02]"))
  expect_identical(.ty_of(c("AU:30", "BEL:15", "BI:40", "C:30"))$levels$code, "C")
  # the matcher closes each scheme of a list on its own vocabulary
  m <- .match_schemes(c("AY", "EL", "BEL", "BT"), list(a = "AY-EL-BT", b = "AY-EL-BT-BCA"),
                      vocabulary = list(a = c("EL", "BT", "BCA")))
  expect_true(m$results$a$passed)
  expect_false(m$results$b$passed)
})

test_that("agro before natural, and the remnant under the plough", {
  skip_on_cran()
  r <- load_rules("kidpr2008")
  for (d in names(.ty_counts)) {
    sch <- .kidpr_ty_parse(r$types[[d]])
    sp <- vapply(sch$specs[sch$trial], function(s) s$specificity[["slots"]], 0)
    ag <- sch$agro[sch$trial]
    # most specific first; at equal specificity the agro types first
    expect_false(is.unsorted(-sp), info = d)
    for (s in unique(sp)) expect_false(is.unsorted(-ag[sp == s]), info = d)
  }
  # classify_kidpr2008() walks the types in that order
  rr <- .ty_of(c("P:25", "EL:15", "BEL:15", "BT:40", "C:30"))
  expect_identical(.ty_type(rr), "[CA17]")
  expect_identical(names(rr$trace$types)[1L], .kidpr_ty_parse(r$types$CA)$trial[1L])
  # a natural remnant the agro formula does not write is ignored; one it
  # writes in parentheses is taken
  expect_identical(.ty_type(.ty_of(c("P:25", "AY:10", "EL:15", "BEL:15", "BT:40", "C:30"))), "[CA17]")
  expect_identical(.ty_type(.ty_of(c("P:25", "AY:10", "AEL:12", "BEL:15", "BT:40", "C:30"))), "[CA19]")
  expect_identical(.ty_type(.ty_of(c("AY:15", "EL:15", "BEL:15", "BT:40", "C:30"))), "[CA04]")
  # the thickest natural horizon under the plough is the remnant (p. 37)
  expect_identical(.ty_type(.ty_of(c("PU:30", "AU:15", "H:10", "G:30", "CG:30"))), "[CH08]")
  expect_identical(.ty_type(.ty_of(c("PU:30", "AU:5", "H:15", "G:30", "CG:30"))), "[CH09]")
  expect_identical(.ty_type(.ty_of(c("PT:25", "T:15", "G:30", "CG:30"))), "[CH07]")
})


# --- Q15: near-identity readings and no type ---------------------------------------------

test_that("Q15: the near-identity readings", {
  skip_on_cran()
  # the BF of CB05, CE06 and CE07: a BHF of either kind
  for (b in c("BF", "BH", "BHF")) expect_identical(.ty_type(.ty_of(c("AY:15", paste0(b, ":30"), "C:30"))), "[CB05]")
  expect_identical(.ty_type(.ty_of(c("O:5", "E:10", "BH:20", "CRM:30", "C:30"))), "[CE06]")
  expect_identical(.ty_type(.ty_of(c("O:5", "E:10", "BHF:20", "CRM:30", "BT:30", "C:30"))), "[CE07]")
  # the BH of the gley Al-Fe-humus soils
  expect_identical(.ty_type(.ty_of(c("O:5", "BH:25", "G:30", "CG:30"))), "[CB02]")
  expect_identical(.ty_type(.ty_of(c("T:30", "E:10", "BHg:25", "G:30", "CG:30"))), "[CB13]")
  # CJ01 takes BCA or CAT
  expect_identical(.ty_type(.ty_of(c("AJ:15", "CAT:35", "Cca:30"))), "[CJ01]")
  expect_identical(.ty_type(.ty_of(c("AJ:15", "BCA:35", "Cca:30"))), "[CJ01]")
  # CH02 takes O, T or H over G-CRM (and T includes TO and TE)
  for (s in c("O:8", "T:30", "H:20", "TE:30"))
    expect_identical(.ty_type(.ty_of(c(s, "G:30", "CRM:30", "C:30"))), "[CH02]", info = s)
  expect_identical(.ty_type(.ty_of(c("TO:30", "ELg:15", "BELg:15", "BTg:40", "G:30", "CG:30"))), "[CA03]")
})

test_that("Q15: a combination no formula provides for stops at the division, with the nearest formulas", {
  skip_on_cran()
  cases <- list(
    list(c("AU:30", "G:30", "CRM:30", "C:30"), "CH", c("[CH06]", "[CH02]", "[CH04]")),  # AU over G-CRM (D24)
    list(c("AY:15", "G:30", "CG:30"), "CH", "[CH01]"),                                # AY over G (Q10)
    list(c("AU:35", "BCA:40", "G:30", "CG:30"), "CI", "[CI06]"),                      # no formula AU-BCA-G
    list(c("AJ:15", "BMK:25", "BM:35", "BCA:35", "Cca:30"), "CJ", "[CJ03]"),          # BMK-BM over BCA
    list(c("T:30", "BHF:30", "C:30"), "CB", "[CB12]"),                                # peaty podbur, no G
    list(c("AY:15", "BPL:30", "BCA:35", "Cca:30"), "CF", "[CF01]"),                   # AY over BPL
    list(c("O:5", "BM:40", "C:30"), "CD", "[CD06]")                                    # O over BM
  )
  for (k in cases) {
    info <- paste(k[[1L]], collapse = " ")
    r <- .ty_of(k[[1L]])
    expect_identical(r$levels$code, c("C", k[[2L]]), info = info)
    expect_length(r$provisional_because, 0L)
    w <- grep("Combination not provided by the Field Guide of 2008", r$warnings, value = TRUE)
    expect_length(w, 1L)
    expect_true(all(k[[3L]] %in% names(r$trace$types_nearest)), info = info)
  }
})


# --- undecided layers ---------------------------------------------------------------------

test_that("an undecided layer leaves the type undecided between its candidates, never FALSE", {
  skip_on_cran()
  # a layer with no designation under the BT of a sod-podzolic soil: G, BCA
  # (CA12 ignores the BEL) or neither
  r <- .ty_of(c("AY:15", "EL:15", "BEL:15", "BT:40", "NA:30", "C:30"))
  expect_identical(r$levels$code, c("C", "CA"))
  expect_true(all(c("types:[CA05]", "types:[CA12]", "types:[CA04]") %in% r$provisional_because))
  w <- grep("The type is undecided between", r$warnings, value = TRUE)
  expect_length(w, 1L)
  expect_match(w, "[CA05] AY-EL-BELg-BTg-G-CG; [CA12] AY-EL-BT-BCA-Cca; [CA04] AY-EL-BEL-BT-C", fixed = TRUE)
  expect_true(is.na(r$trace$types[["[CA05]"]]$passed))
  expect_true(length(r$trace$types[["[CA05]"]]$missing) > 0L)
  # the same on the data: a bluish layer whose moist value is not recorded
  p <- .ty_P(.ty_AY(), .ty_E(15, 35), .ty_BT(35, 90), .ty_moist(90, 130, "5GY", NA, 1), .ty_L(130, 160))
  f <- identify_horizons(p, "kidpr2008")
  expect_identical(f$code[1:3], c("AY", "EL", "BT"))
  expect_true(is.na(f$passed[4]))
  r <- .ty_class(p)
  expect_identical(r$levels$code, c("C", "CA"))
  expect_true(all(c("types:[CA05]", "types:[CA04]") %in% r$provisional_because))
  expect_true("munsell_value_moist" %in% r$missing_data)
  # a division left undecided before the one assigned makes the type
  # provisional too
  r <- .ty_of(c("AY:15", "EL:15", "BEL:15", "BT:40", "C:30"), site = list(contaminated = TRUE))
  expect_identical(.ty_type(r), "[CA04]")
  expect_true("divisions:CU" %in% r$provisional_because)
})

test_that("an undecided remnant under the plough is read by the thickest-horizon rule", {
  skip_on_cran()
  # PU over a layer that is AU or AY, over H, over G: as AU (the thicker) it
  # is the remnant of CH08; as AY it is a remnant no formula writes
  fr <- function(th) {
    x <- data.frame(top_cm = c(0, 30, 30 + th, 45 + th, 75 + th), bottom_cm = c(30, 30 + th, 45 + th, 75 + th, 105 + th),
                    code = c("PU", NA, "H", "G", "C"), passed = c(TRUE, NA, TRUE, TRUE, TRUE),
                    stringsAsFactors = FALSE)
    x$candidates <- list(character(0), c("AU", "AY"), character(0), character(0), character(0))
    x$missing <- list(character(0), "munsell_value_dry", character(0), character(0), character(0))
    x
  }
  rules <- load_rules("kidpr2008")
  v <- .kidpr_dv(fr(20))
  rules <- .kidpr_types_attach(rules, v)
  t <- .run_level(v, rules$types$CH, "types")
  expect_null(t$assigned)
  expect_identical(t$undecided, "[CH08]")
  expect_false(t$trace[["[CH09]"]]$passed)
  expect_true("munsell_value_dry" %in% t$trace[["[CH08]"]]$missing)
  # thinner than H, either reading leaves H the remnant: CH09, decided
  v <- .kidpr_dv(fr(8))
  rules <- .kidpr_types_attach(load_rules("kidpr2008"), v)
  t <- .run_level(v, rules$types$CH, "types")
  expect_identical(t$assigned$code, "[CH09]")
  expect_length(t$undecided, 0L)
  expect_identical(type_formula_kidpr(v, "[CH09]")$passed, TRUE)
})

test_that("a type list with a second match: the intergrade in the trace, and a provisional type", {
  skip_on_cran()
  # two schemes of soilKey's own, added to CA only for this test: a twin of
  # CA04, and CA04 with an optional G
  extra <- function(code, formula) list(code = code, name = "Test", name_native = "Тест",
                                        formula = formula, page = 78,
                                        tests = list(all_of = list(list(type_formula_kidpr = list(type = code)))))
  rules <- load_rules("kidpr2008")
  rules$types$CA <- c(rules$types$CA, list(extra("[CA90]", "AY-EL-(BEL)-BT-C"), extra("[CA91]", "AY-EL-(BEL)-BT-(G)-C")))
  r <- .ty_class(.ty_ped(c("AY:15", "EL:15", "BEL:15", "BT:40", "C:30")), rules = rules)
  expect_identical(.ty_type(r), "[CA04]")
  ev <- r$trace$types[["[CA04]"]]$evidence[[1L]]
  expect_setequal(ev$evidence$intergrades, c("[CA90]", "[CA91]"))
  expect_true(any(grepl("intergrade", ev$notes)))
  w <- grep("^Intergrade", r$warnings, value = TRUE)
  expect_length(w, 1L)
  expect_match(w, "[CA90] AY-EL-(BEL)-BT-C", fixed = TRUE)
  expect_true(isFALSE(type_formula_kidpr(.kidpr_dv_pedon(.ty_ped(c("AY:15", "EL:15", "BEL:15", "BT:40", "C:30"))),
                                         "[CA05]")$passed))
  # a bluish layer whose moist value is not recorded, G or no horizon: CA91
  # matches either way, but CA05, CA04 and CA90, tried before it, are
  # undecided, so it is provisional
  p <- .ty_P(.ty_AY(), .ty_E(15, 35), .ty_BT(35, 90), .ty_moist(90, 130, "5GY", NA, 1), .ty_L(130, 160))
  r <- .ty_class(p, rules = rules)
  expect_identical(.ty_type(r), "[CA91]")
  expect_true(all(c("types:[CA05]", "types:[CA04]", "types:[CA90]") %in% r$provisional_because))
  expect_true(any(grepl("^Provisional", r$warnings)))
})


# --- the fixes of the review, Hugo's answers (2026-11-02) -------------------------------

.ty_tab <- function(d, top, bot, site = list()) {
  h <- data.frame(designation = d, top_cm = top, bottom_cm = bot, stringsAsFactors = FALSE)
  PedonRecord$new(site = utils::modifyList(list(id = "ru-type", designation_system = "kidpr"), site),
                  horizons = ensure_horizon_schema(data.table::as.data.table(h)))
}
.ty_und <- function(r, cands) {
  expect_identical(nrow(r$levels), 2L)
  expect_true(all(paste0("types:", cands) %in% r$provisional_because))
  w <- grep("The type is undecided between", r$warnings, value = TRUE)
  expect_length(w, 1L)
  for (k in cands) expect_match(w, k, fixed = TRUE)
}

test_that("D1: a tie or a missing thickness at the surface leaves the type undecided", {
  skip_on_cran()
  .ty_und(.ty_class(.ty_tab(c("O", "AY", "EL", "BEL", "BT", "C"), c(0, 10, 20, 35, 50, 90), c(10, 20, 35, 50, 90, 120))),
          c("[CA01]", "[CA04]"))
  r <- .ty_class(.ty_tab(c("O", "AY", "EL", "BEL", "BT", "C"), c(0, NA, 25, 40, 55, 95), c(10, 25, 40, 55, 95, 120)))
  .ty_und(r, c("[CA01]", "[CA04]"))
  expect_true(all(c("top_cm", "bottom_cm") %in% r$missing_data))
  .ty_und(.ty_class(.ty_tab(c("O", "AU", "G", "CG"), c(0, 15, 30, 60), c(15, 30, 60, 90))), c("[CH01]", "[CH04]"))
  # under the plough: two natural horizons of equal thickness
  .ty_und(.ty_class(.ty_tab(c("PU", "AU", "H", "G", "CG"), c(0, 30, 40, 50, 80), c(30, 40, 50, 80, 110))),
          c("[CH08]", "[CH09]"))
  # a layer that could thicken a horizon the rule drops: AY 8 + (AY 8) > O 10
  .ty_und(.ty_of(c("O:10", "AY:8", "NA:8", "EL:10", "BT:40", "C:30")), c("[CA01]", "[CA04]"))
  # the thicker one decided: still decided
  expect_identical(.ty_type(.ty_of(c("O:5", "AY:15", "EL:15", "BEL:15", "BT:40", "C:30"))), "[CA04]")
  expect_identical(.ty_type(.ty_of(c("O:20", "AY:10", "EL:15", "BEL:15", "BT:40", "C:30"))), "[CA01]")
  # a W of 5 cm or less over another horizon is still a feature
  expect_identical(.ty_type(.ty_of(c("W:4", "O:6", "BHF:30", "C:30"))), "[CB01]")
  # on the data: litter-free organic and humus layers of 5 cm each
  p <- .ty_P(.ty_O(0, 5, designation = "Oe"), .ty_AY(5, 10, designation = "Ah"), .ty_E(10, 30, designation = "E"),
             .ty_BT(30, 90, designation = "Bt"), .ty_L(90, 150, designation = "C"))
  r <- .ty_class(p)
  expect_identical(r$levels$code[2L], "CA")
  .ty_und(r, c("[CA01]", "[CA04]"))
})

test_that("D2 and R1: transitions and loose rock on the data; a layer with no horizon between", {
  skip_on_cran()
  dr <- list(drainage_class = "poorly drained")
  gley <- function(mid) .ty_P(.ty_AY(0, 15, designation = "Ah"), .ty_E(15, 32, designation = "Eg"),
                              .ty_BT(32, 80, designation = "Btg"), mid,
                              .ty_moist(90, 130, "5GY", 6, 1, designation = "Bg"), site = dr)
  # an FAO transition (BC) or loose rock (Cg1) between Bt and Bg
  for (d in c("BC", "Cg1")) {
    r <- .ty_class(gley(.ty_L(80, 90, designation = d)))
    expect_identical(r$levels$code, c("C", "CA", "[CA05]"), info = d)
    expect_length(r$provisional_because, 0L)
  }
  # a layer the key reads as no horizon: it may be a transition or loose rock
  # (Hugo, 2026-11-02), so the type is undecided, not FALSE
  p <- gley(.ty_L(80, 90))
  f <- identify_horizons(p, "kidpr2008")
  expect_true(is.na(f$code[4]) && isFALSE(f$passed[4]))
  r <- .ty_class(p)
  expect_identical(r$levels$code, c("C", "CA"))
  expect_true("types:[CA05]" %in% r$provisional_because)
  expect_match(grep("undecided", r$warnings, value = TRUE), "the formula of [CA05] AY-EL-BELg-BTg-G-CG may match", fixed = TRUE)
  expect_true("designation" %in% r$missing_data)
  # the allowance of 15 cm between AU and BCA still takes it, decided
  p <- .ty_P(.ty_AU(0, 35, designation = "Ah"), .ty_L(35, 45, ph_h2o = 7.4), .ty_BCA(45, 90, designation = "Bk"),
             .ty_L(90, 150, designation = "Ck", caco3_pct = 10))
  r <- .ty_class(p)
  expect_identical(r$levels$code, c("C", "CI", "[CI01]"))
  expect_length(r$provisional_because, 0L)
})

test_that("M1: a gap that blocks the nearest formula is named in the note", {
  skip_on_cran()
  m <- soilKey:::.kidpr_ty_describe(list(formula = "AU-BCA-Cca", distance = 1, missing = character(0), extra = character(0),
                                         notes = "BCA (3) lies under AU (1) across 20 cm with no diagnostic horizon (allowed 15)"))
  expect_match(m, "AU-BCA-Cca, distance 1 (BCA (3) lies under AU (1) across 20 cm", fixed = TRUE)
})

test_that("D4: an agro horizon under RY is no part of the stratified body", {
  skip_on_cran()
  expect_identical(.ty_of(c("RY:12", "P:25", "EL:15", "BEL:15", "BT:40", "C:30"))$levels$code, c("C", "CA", "[CA17]"))
  expect_identical(.ty_of(c("RY:12", "PU:30", "AU:10", "BCA:40", "Cca:30"))$levels$code, c("C", "CI", "[CI11]"))
  # over the stratified horizon it still counts (P-RY-D, PU-RU-D)
  expect_identical(.ty_of(c("P:25", "RY:25", "D:40"))$levels$code[1:2], c("B", "BC"))
  expect_identical(.ty_of(c("PU:30", "RY:12", "AU:10", "BCA:40", "Cca:30"))$levels$code[1:2], c("B", "BC"))
})

test_that("R2: in CB and CE an EL counts with E", {
  skip_on_cran()
  expect_identical(.ty_type(.ty_of(c("O:5", "EL:12", "BHF:30", "C:30"))), "[CB07]")
  expect_identical(.ty_type(.ty_of(c("T:30", "EL:12", "BHF:30", "C:30"))), "[CB12]")
  expect_identical(.ty_type(.ty_of(c("O:5", "EL:12", "CRM:30", "C:30"))), "[CE05]")
  # not a podbur, nor a plain cryometamorphic soil
  r <- .ty_of(c("O:5", "EL:12", "BHF:30", "C:30"))
  expect_false(isTRUE(r$trace$types[["[CB01]"]]$passed))
  # no formula with E for TJ over G: the division stays (Q15)
  r <- .ty_of(c("TJ:20", "EL:10", "BHF:30", "G:30", "CG:30"))
  expect_identical(r$levels$code, c("C", "CB"))
  expect_true(any(grepl("Combination not provided", r$warnings)))
  expect_identical(.ty_type(.ty_of(c("O:5", "BHF:30", "C:30"))), "[CB01]")
  expect_identical(.ty_type(.ty_of(c("O:5", "CRM:30", "C:30"))), "[CE01]")
})

test_that("R3: in CD a BCA neither blocks nor is required where the formula does not write it", {
  skip_on_cran()
  expect_identical(.ty_type(.ty_of(c("AY:15", "BM:40", "BCA:30", "Cca:30"))), "[CD01]")
  expect_identical(.ty_type(.ty_of(c("AO:8", "BM:40", "BCA:30", "Cca:30"))), "[CD02]")
  expect_identical(.ty_type(.ty_of(c("PU:30", "AU:10", "BM:40", "BCA:30", "Cca:30"))), "[CD13]")
  # CD08 wins where it matches; CD03, which ignores the BCA, is its intergrade
  r <- .ty_of(c("AU:30", "BM:40", "BCA:30", "Cca:30"))
  expect_identical(.ty_type(r), "[CD08]")
  expect_identical(r$trace$types[["[CD08]"]]$evidence[[1L]]$evidence$intergrades, "[CD03]")
})

test_that("R4: a V laid over another horizon is a feature at the division key too", {
  skip_on_cran()
  r <- .ty_of(c("AU:30", "G+V:40", "CG:30"))
  expect_identical(r$levels$code, c("C", "CH", "[CH04]"))
  expect_identical(.ty_of(c("AU:30", "BCA+V:40", "Cca:30"))$levels$code, c("C", "CI", "[CI01]"))
  expect_identical(.ty_of(c("AU:30", "V:40", "C:30"))$levels$code, c("C", "CI", "[CI04]"))
  # the predicates of the key do not read the V overlay as V
  v <- .kidpr_dv_pedon(.ty_ped(c("AU:30", "G+V:40", "CG:30")))
  expect_false(has_horizon_kidpr(v, "V")$passed)
  expect_true(has_horizon_kidpr(v, "G")$passed)
  # an S overlay still counts (C.3)
  expect_identical(.ty_of(c("AY+S:8", "C:40"))$levels$code[2L], "CL")
})


# --- the data route -------------------------------------------------------------------------

test_that("undesignated twins reach their division and type through the key to horizons", {
  skip_on_cran()
  fr <- list(frost_regime = "long-seasonal")
  twins <- list(
    CA = list("[CA04]", .ty_P(.ty_AY(0, 15, designation = "Ah"), .ty_E(15, 35, designation = "E"),
                              .ty_BT(35, 90, designation = "Bt"), .ty_L(90, 150, designation = "C"))),
    CB = list("[CB07]", .ty_P(.ty_O(0, 8, designation = "Oe"),
                              .ty_sand(8, 20, designation = "E", munsell_value_dry = 7, munsell_chroma_dry = 1,
                                       structure_type = "single grain"),
                              .ty_sand(20, 50, designation = "Bhs", munsell_hue_dry = "7.5YR", munsell_value_dry = 4,
                                       munsell_chroma_dry = 6, uncoated_grains_pct = 30),
                              .ty_sand(50, 80, designation = "Bs", munsell_value_dry = 6, munsell_chroma_dry = 6,
                                       uncoated_grains_pct = 50),
                              .ty_sand(80, 120, designation = "C", uncoated_grains_pct = 95))),
    CC = list("[CC01]", .ty_P(.ty_AY(0, 15, designation = "Ah"),
                              .ty_sand(15, 40, designation = "Bw1", munsell_hue_dry = "5YR", munsell_value_dry = 5,
                                       munsell_chroma_dry = 8, uncoated_grains_pct = 40, structure_type = "granular",
                                       structure_grade = "weak"),
                              .ty_sand(40, 70, designation = "Bw2", munsell_hue_dry = "5YR", munsell_value_dry = 5,
                                       munsell_chroma_dry = 8, uncoated_grains_pct = 40),
                              .ty_sand(70, 100, designation = "C", uncoated_grains_pct = 95))),
    CD = list("[CD01]", .ty_P(.ty_AY(0, 15, designation = "Ah"),
                              .ty_B(15, 50, designation = "Bw", munsell_hue_dry = "10YR", munsell_chroma_dry = 4),
                              .ty_L(50, 100, designation = "C"))),
    CE = list("[CE04]", .ty_P(.ty_AY(0, 15, designation = "Ah"),
                              .ty_L(15, 45, designation = "Bw", munsell_value_dry = 4, munsell_chroma_dry = 2,
                                    structure_type = "lenticular", structure_grade = "moderate"),
                              .ty_L(45, 100, designation = "C"), site = fr)),
    CF = list("[CF01]", .ty_P(.ty_AJ(0, 15, designation = "Ah"),
                              .ty_L(15, 45, designation = "Bw", munsell_value_dry = 7, munsell_chroma_dry = 3,
                                    structure_type = "blocky", structure_grade = "weak"),
                              .ty_L(45, 80, designation = "Bk", munsell_value_dry = 7, munsell_chroma_dry = 3,
                                    caco3_pct = 12, secondary_carbonates_form = "soft concretions"),
                              .ty_L(80, 120, designation = "Ck", caco3_pct = 8), site = fr)),
    CG = list("[CG01]", .ty_P(.ty_O(0, 6, designation = "Oe"),
                              .ty_L(6, 40, designation = "Bw", munsell_value_dry = 4, munsell_chroma_dry = 2),
                              .ty_L(40, 100, designation = "Cf", permafrost_temp_C = -1),
                              site = list(frost_regime = "permafrost"))),
    CH = list("[CH01]", .ty_P(.ty_O(0, 8, designation = "Oe"), .ty_moist(8, 60, "5GY", 6, 1, designation = "Bg"),
                              .ty_L(60, 100, designation = "Cg"), site = list(drainage_class = "poorly drained"))),
    CI = list("[CI01]", .ty_P(.ty_AU(0, 40, designation = "Ah"), .ty_BCA(40, 90, designation = "Bk"),
                              .ty_L(90, 150, designation = "Ck", caco3_pct = 10))),
    CJ = list("[CJ01]", .ty_P(.ty_AJ(0, 15, designation = "Ah"),
                              .ty_BCA(15, 60, designation = "Bk", secondary_carbonates_form = "soft concretions"),
                              .ty_L(60, 120, designation = "Ck", caco3_pct = 10)))
  )
  for (d in names(twins)) {
    p <- twins[[d]][[2L]]
    expect_null(p$site$designation_system)
    expect_false(isTRUE(soilKey:::.kidpr_designations(p$horizons$designation, p$site)$kidpr), info = d)
    expect_true(all(identify_horizons(p, "kidpr2008")$route == "data"), info = d)
    r <- .ty_class(p)
    expect_identical(r$levels$code, c("C", d, twins[[d]][[1L]]), info = d)
    expect_length(r$provisional_because, 0L)
    expect_true(any(grepl("key 2.1 run on", r$warnings)), info = d)
  }
})
