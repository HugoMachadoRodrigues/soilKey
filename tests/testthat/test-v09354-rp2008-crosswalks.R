# =============================================================================
# soilKey 0.9.354 -- the correspondence tables of the Referentiel pedologique
# 2008 (Annexes 5, 6 and 7) and the labels of the RP 1995 (Hugo's decision
# Q1; the readings of 2026-11-04). Data for display and benchmarks: every row
# resolves to existing codes, and no classification reads them.
# =============================================================================

.cw54_yaml <- function(name) {
  yaml::read_yaml(system.file("rules", "rp2008", paste0(name, ".yaml"), package = "soilKey"))
}

# The 110 references of the list of pp. 402-403 and the 34 GER, from key.yaml
# (whatever the parts already implemented).
.cw54_refs <- function() .rp_reference_catalogue()$code
.cw54_gers <- function() vapply(.rp_rules()$ger, `[[`, "", "code")

# Every code of the cpcs1967 rules: classes, subclasses, groups, subgroups.
.cw54_cpcs_codes <- function() {
  root <- system.file("rules", "cpcs1967", package = "soilKey")
  codes <- vapply(yaml::read_yaml(file.path(root, "key.yaml"))$classes, `[[`, "", "code")
  for (d in c("subclasses", "groups", "subgroups"))
    for (f in list.files(file.path(root, d), pattern = "\\.yaml$", full.names = TRUE)) {
      txt <- readLines(f, encoding = "UTF-8", warn = FALSE)
      m <- regmatches(txt, regexpr("^\\s*- code: \"[^\"]+\"", txt))
      codes <- c(codes, sub("^\\s*- code: \"([^\"]+)\"$", "\\1", m))
    }
  unique(codes)
}

# An RP code the notation reader reads as a code of its vocabulary.
.cw54_rp_code_ok <- function(x) !is.na(.rp_parse_one(x)$code)


# --- the catalogue ---------------------------------------------------------------

test_that("the catalogue of the references holds the 110 references of the 34 GER", {
  skip_on_cran()
  cat <- .rp_reference_catalogue()
  expect_identical(nrow(cat), 110L)
  expect_false(anyDuplicated(cat$code) > 0L)
  expect_identical(length(.cw54_gers()), 34L)
  expect_true(all(cat$ger %in% .cw54_gers()))
  expect_false(anyNA(cat$name))
})


# --- Annex 5 ---------------------------------------------------------------------

test_that("Annex 5 gives every one of the 110 references a WRB 2006 row, and no other code", {
  skip_on_cran()
  w <- rp2008_crosswalk("wrb2006")
  expect_setequal(unique(w$code), .cw54_refs())
  expect_identical(nrow(w), 112L)
  # the brunisols eutriques by their saturation, the lithosols stricts (p. 380, 385)
  expect_identical(w$wrb2006[w$code == "brunisols_eutriques"], c("Cambisols (Hypereutric)", "Cambisols (Eutric)"))
  expect_identical(w$wrb2006[w$rp == "lithosols stricts"], "Nudilithic Leptosols")
  expect_true(all(w$page >= 379L & w$page <= 390L))
  expect_false(anyNA(w$ger))
  # the WRB does not take two anthroposols into account (p. 379)
  expect_setequal(w$code[is.na(w$wrb2006)], c("anthroposols_reconstitues", "anthroposols_archeologiques"))
  expect_true(all(!is.na(w$note[is.na(w$wrb2006)])))
  # the book's doubts kept as a flag (pp. 382, 384, 387)
  expect_setequal(w$code[w$doubtful], c("cryosols_histiques", "gypsosols_petrogypsiques", "pelosols_typiques",
                                        "pelosols_brunifies", "pelosols_differencies"))
  expect_identical(w$wrb2006[w$code == "gypsosols_petrogypsiques"], "Epipetric (?) Gypsisols")
  # the book's French "ou" written "or"
  expect_false(any(grepl("\\bou\\b", w$wrb2006)))
  # a few rows read on the page images
  expect_identical(w$wrb2006[w$code == "ferrallitisols_nodulaires"], "Ferralic Hyperferric Plinthosols")
  expect_identical(w$wrb2006[w$code == "podzosols_humo_duriques"], "Umbric-Ortsteinic Podzols")
  expect_identical(w$wrb2006[w$code == "regosols"], "(non Lithic) Leptosols")
  expect_identical(w$wrb2006[w$code == "sulfatosols"], "Fluvisols or Gleysols (Hyperthionic or Orthothionic)")
  expect_identical(w$wrb2006[w$code == "luvisols_tronques"], "(truncated) Luvisols")
})

test_that("the WRB 2006 names of the references files agree with Annex 5", {
  skip_on_cran()
  # the 71 (and later more) references files give a wrb2006 of their own, in
  # soilKey's words; the WRB names of each must be those of the annex
  w <- rp2008_crosswalk("wrb2006")
  caps <- function(x) unique(tolower(regmatches(x, gregexpr("\\b[A-Z][a-z]+(-[A-Z][a-z]+)?", x))[[1]]))
  words <- function(x) unique(tolower(regmatches(x, gregexpr("[A-Za-z][a-z]+(-[A-Z][a-z]+)?", x))[[1]]))
  dir <- system.file("rules", "rp2008", "references", package = "soilKey")
  n <- 0L
  for (f in list.files(dir, pattern = "\\.yaml$", full.names = TRUE))
    for (r in yaml::read_yaml(f)$references) {
      if (is.null(r$wrb2006) || grepl("^not taken", r$wrb2006)) next
      n <- n + 1L
      mine <- paste(w$wrb2006[w$code == r$code], collapse = " ")
      expect_identical(setdiff(caps(r$wrb2006), words(mine)), character(0), info = r$code)
      expect_identical(setdiff(caps(mine), words(r$wrb2006)), character(0), info = r$code)
    }
  expect_gte(n, 60L)
})

test_that("the horizons, qualifiers and faux amis of Annex 5 are codes and names of the RP", {
  skip_on_cran()
  h <- rp2008_crosswalk("wrb2006_horizons")
  expect_setequal(h$rp, c("K", "Kc", "Km", "F", "OX", "RT", "OXm", "OXc", "ND", "Hf", "Hm", "Hs"))
  expect_true(all(vapply(h$rp, .cw54_rp_code_ok, NA)))
  expect_identical(h$wrb2006[h$rp == "Hs"], "Ha")
  q <- rp2008_crosswalk("wrb2006_qualifiers")
  expect_identical(nrow(q), 65L)
  expect_identical(sum(q$page %in% c(391L, 392L)), 49L)
  expect_identical(sum(q$scope %in% "histosols" & q$page == 385L), 9L)
  expect_identical(q$wrb2006[q$rp == "p\u00e9trosilicique"], "Petroduric")
  expect_identical(q$wrb2006[q$rp == "\u00e0 duripan"], "Petrosilicique")
  expect_false(is.na(q$note[q$rp == "\u00e0 duripan"]))
  fa <- rp2008_crosswalk("wrb2006_faux_amis")
  expect_identical(nrow(fa), 15L)
  expect_true(all(c("Andic", "Calcaric", "Vertic", "Luvic") %in% fa$wrb2006))
})


# --- Annex 6 ---------------------------------------------------------------------

test_that("Annex 6: every CPCS symbol is read by the CPCS reader, every RP code by the RP reader", {
  skip_on_cran()
  a6 <- rp2008_crosswalk("cpcs_horizons")
  syms <- unique(a6$cpcs)
  expect_identical(length(syms), 28L)
  for (s in setdiff(syms, "-g")) {
    p <- .cpcs_parse_one(s, cpcs = TRUE)
    expect_false(p$role %in% c("other", NA), info = s)
  }
  # every RP code a code of the vocabulary, except the slips and the -g mark
  ok <- vapply(a6$rp, function(x) x == "-g" || .cw54_rp_code_ok(x), NA)
  expect_setequal(unique(a6$rp[!ok]), "BTfe")
  expect_setequal(unique(a6$rp[a6$slip]), c("Ha", "BTfe", "FSj"))
  expect_true(all(a6$slip[!ok]))
  # the intertropical codes of row B, the symbols after 1967
  expect_setequal(a6$rp[a6$intertropical], c("FE", "OX", "F"))
  expect_setequal(unique(a6$cpcs[a6$after_1967]), c("Bs", "Btgd", "Bw", "(B)v"))
  expect_identical(a6$rp[a6$cpcs == "Cca"], c("Ck", "K", "Kc", "Km"))
  expect_true(all(a6$page == 393L))
})

test_that("Annex 6 as data and the reader's copy in horizons.yaml give the same codes", {
  skip_on_cran()
  a6 <- rp2008_crosswalk("cpcs_horizons")
  hz <- .cw54_yaml("horizons")$annex6
  hz <- Filter(function(r) !isTRUE(r$soilkey), hz)
  expect_setequal(vapply(hz, `[[`, "", "cpcs"), unique(a6$cpcs))
  for (r in hz) {
    mine <- a6$rp[a6$cpcs == r$cpcs]
    if (identical(r$cpcs, "-g")) mine <- setdiff(mine, "-g")
    expect_setequal(as.character(unlist(r$rp)), mine)
    expect_setequal(as.character(unlist(r$slips)), a6$rp[a6$cpcs == r$cpcs & a6$slip])
  }
})


# --- Annex 7 ---------------------------------------------------------------------

test_that("Annex 7: every row is keyed by a cpcs1967 code and resolves to RP codes", {
  skip_on_cran()
  rows <- .cw54_yaml("crosswalk_cpcs_classes")$rows
  keys <- vapply(rows, `[[`, "", "cpcs")
  expect_false(anyDuplicated(keys) > 0L)
  expect_true(all(keys %in% .cw54_cpcs_codes()), info = paste(setdiff(keys, .cw54_cpcs_codes()), collapse = " "))
  # every class of the CPCS has a row on it or under it
  cls <- sub("[-/].*$", "", keys)
  expect_setequal(unique(cls), c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", "XI", "XII"))
  refs <- .cw54_refs(); gers <- .cw54_gers()
  for (r in rows) {
    if (isTRUE(r$none)) { expect_null(r$rp, info = r$cpcs); next }
    expect_gte(length(r$rp), 1L)
    expect_true(r$page >= 394L && r$page <= 398L, info = r$cpcs)
    for (it in r$rp) {
      rr <- as.character(unlist(it$references))
      expect_true(all(rr %in% refs), info = paste(r$cpcs, it$name))
      if (!is.null(it$ger)) expect_true(it$ger %in% gers, info = paste(r$cpcs, it$name))
      expect_true(length(rr) > 0L || !is.null(it$ger) || isTRUE(it$any_reference), info = paste(r$cpcs, it$name))
      if (isTRUE(it$double)) expect_true(length(rr) + (!is.null(it$ger)) >= 2L, info = paste(r$cpcs, it$name))
    }
  }
  # the "???" of the book (pp. 394, 396)
  d <- rp2008_crosswalk("cpcs_classes")
  expect_setequal(unique(d$cpcs[d$none]), c("II/31", "VI/22", "VI/23", "VI/31", "VI/32", "VI/41", "VII/31"))
  expect_setequal(unique(d$cpcs[d$doubtful]), c("IV/21", "IV/22", "V/115"))
  expect_identical(unique(d$cpcs_level[d$cpcs %in% c("I", "X")]), "classes")
  expect_identical(d$cpcs_level[d$cpcs == "V/111"], "subgroups")
  # the decisions of 2026-11-04: fersialsols calcaires and satures
  expect_identical(d$references[d$cpcs == "IX/21"], c("fersialsols_carbonates", "fersialsols_calciques"))
  # the categories new in the RP (p. 398)
  nw <- rp2008_crosswalk("cpcs_new_in_rp")
  expect_identical(nrow(nw), 14L)
  expect_true(all(unlist(strsplit(stats::na.omit(nw$references), ", ")) %in% refs))
  expect_true(all(!is.na(nw$references) | !is.na(nw$ger)))
  expect_true(all(is.na(nw$ger) | nw$ger %in% gers))
})


# --- the labels of the RP 1995 ---------------------------------------------------

test_that("the 13 references removed in 2008 resolve to references of 2008", {
  skip_on_cran()
  lg <- .cw54_yaml("legacy_rp1995")$labels
  expect_setequal(vapply(lg, `[[`, "", "label"), c(
    "pseudo-luvisols", "brunisols satur\u00e9s", "brunisols m\u00e9sosatur\u00e9s", "brunisols oligo-satur\u00e9s",
    "brunisols resatur\u00e9s", "peyrosols cailloutiques", "peyrosols pierriques", "histosols recouverts",
    "histosols flottants", "r\u00e9ductisols dupliques", "organosols tangeliques", "aluandosols humiques",
    "silandosols humiques"))
  refs <- .cw54_refs(); gers <- .cw54_gers()
  for (r in lg) for (it in r$rp2008) {
    rr <- as.character(unlist(it$references))
    expect_true(all(rr %in% refs), info = r$label)
    if (!is.null(it$ger)) expect_true(it$ger %in% gers, info = r$label)
    expect_true(length(rr) > 0L || !is.null(it$ger), info = r$label)
  }
  d <- rp2008_crosswalk("legacy_rp1995")
  expect_setequal(unique(d$label[d$reading]), c("brunisols oligo-satur\u00e9s", "histosols recouverts",
                                                "aluandosols humiques", "silandosols humiques"))
  # none of the labels of 1995 is a name of 2008
  cat <- .rp_reference_catalogue()
  expect_false(any(.rp_label_fold(d$label) %in% .rp_label_fold(cat$name)))
})

test_that("rp2008_from_label reads references, types, doubles, groups and labels of 1995", {
  skip_on_cran()
  x <- rp2008_from_label(c("Brunisol eutrique resatur\u00e9, limoneux", "BRUNISOL SATURE",
                           "luvisol typique-r\u00e9doxisol", "Sol brun lessiv\u00e9", "planosol distal",
                           "PHAEOSOL", "Ph\u00e6osol haplique", "r\u00e9ductisol \u00e0 anmoor",
                           "brunisol oligosatur\u00e9", "pseudo-luvisol", "post-podzosol",
                           "aren\u00f3sol", NA, ""))
  expect_identical(x$references[1:3], c("brunisols_eutriques", "brunisols_eutriques", "luvisols_typiques"))
  expect_identical(x$source[1:3], c("rp2008", "rp1995", "rp2008"))
  expect_identical(x$double_with[3], "redoxisols")
  expect_true(is.na(x$references[4]))
  expect_identical(x$references[5], "planosols_distaux")
  expect_identical(x$source[6], "group")
  expect_identical(x$ger[6], "phaeosols")
  expect_identical(x$references[7], "phaeosols_hapliques")
  expect_identical(x$references[8], "reductisols_typiques, reductisols_stagniques")
  expect_identical(x$references[9], "brunisols_dystriques")
  expect_identical(x$references[10], "brunisols_eutriques, brunisols_dystriques")
  expect_identical(x$references[11], "post_podzosols")
  expect_true(all(is.na(x$references[12:14])))
  # a label of 1995 that names a group reads as every reference of it (the review)
  y <- rp2008_from_label("Histosol recouvert par colluvions")
  expect_identical(y$references, paste(c("histosols_fibriques", "histosols_mesiques", "histosols_sapriques",
                                         "histosols_composites", "histosols_leptiques"), collapse = ", "))
  expect_identical(y$source, "rp1995")
  # every reference of 2008 reads as itself, in the plural and in the singular
  cat <- .rp_reference_catalogue()
  expect_identical(rp2008_from_label(cat$name)$references, cat$code)
  expect_identical(rp2008_from_label(toupper(cat$name))$references, cat$code)
})


# --- display only ----------------------------------------------------------------

test_that("an RP result shows its WRB 2006 names, a CPCS result its RP equivalents", {
  skip_on_cran()
  r <- classify_rp2008(make_rp2008_example("brunisols_eutriques"), on_missing = "silent")
  cw <- .rp_crosswalk_lines(r)
  expect_identical(cw$kind, "wrb2006")
  expect_length(cw$lines, 2L)
  expect_match(cw$lines[1], "Cambisols (Hypereutric)", fixed = TRUE)
  # every example of the RP gets its lines
  for (k in eval(formals(make_rp2008_example)[[1]])) {
    res <- classify_rp2008(make_rp2008_example(k), on_missing = "silent")
    if (is.data.frame(res$levels) && nrow(res$levels))
      expect_gte(length(.rp_crosswalk_lines(res)$lines), 1L)
  }
  # the most specific row of the chain (the decision of 2026-11-04)
  for (k in eval(formals(make_cpcs1967_example)[[1]])) {
    res <- classify_cpcs1967(make_cpcs1967_example(k), on_missing = "silent")
    cw <- .rp_crosswalk_lines(res)
    expect_identical(cw$kind, "cpcs", info = k)
    expect_length(cw$lines, 1L)
  }
  res <- classify_cpcs1967(make_cpcs1967_example("calcimagnesic"), on_missing = "silent")
  expect_identical(.rp_crosswalk_lines(res)$lines, "Rendzines modales (V/113, p. 395): rendosols.")
  res <- classify_cpcs1967(make_cpcs1967_example("ferrallitic"), on_missing = "silent")
  expect_match(.rp_crosswalk_lines(res)$lines, "(X, p. 397): ferrallitisols et oxydisols.", fixed = TRUE)
  # a "???" row, and a chain with no row
  fake <- list(system_id = "cpcs1967", levels = data.frame(level = c("classes", "subclasses", "groups"),
                                                           code = c("VI", "VI-2", "VI/22")))
  expect_match(.rp_crosswalk_lines(fake)$lines, "no equivalent in the RP 2008", fixed = TRUE)
  fake$levels <- data.frame(level = c("classes", "subclasses", "groups"), code = c("I", "I-1", "I/16"))
  expect_match(.rp_crosswalk_lines(fake)$lines, "Annex 7 has no row for I/16", fixed = TRUE)
  # a level left NA is skipped; a doubt printed in the cell is not marked twice;
  # a row ending in etc. says so (the review)
  fake$levels <- data.frame(level = c("classes", "subclasses", "groups", "subgroups"),
                            code = c("VIII", "VIII-2", "VIII/21", NA))
  expect_match(.rp_crosswalk_lines(fake)$lines, "(VIII-2, p. 397): podzosols arctiques, alpins", fixed = TRUE)
  expect_match(.rp_crosswalk_lines(fake)$lines, ", etc.$")
  fake$levels <- data.frame(level = "classes", code = NA_character_)
  expect_null(.rp_crosswalk_lines(fake))
  fake <- list(system_id = "rp2008", levels = data.frame(level = "references", code = "gypsosols_petrogypsiques"))
  expect_identical(.rp_crosswalk_lines(fake)$lines,
                   "gypsosols p\u00e9trogypsiques: Epipetric (?) Gypsisols (p. 384)")
  # nothing for the other systems
  expect_null(.rp_crosswalk_lines(classify_wrb2022(make_ferralsol_canonical(), on_missing = "silent")))
})

test_that("the report shows the correspondences as notes, in both languages", {
  skip_on_cran()
  r <- classify_rp2008(make_rp2008_example("alocrisols_typiques"), on_missing = "silent")
  h <- .html_national_sections(r)
  expect_match(h, "Annex 5 of the RP 2008", fixed = TRUE)
  expect_match(h, "Cambisols (Hyperdystric)", fixed = TRUE)
  expect_match(.rmd_national_sections(r), "Cambisols (Hyperdystric)", fixed = TRUE)
  c7 <- classify_cpcs1967(make_cpcs1967_example("podzolised"), on_missing = "silent")
  expect_match(.html_national_sections(c7), "(Annex 7)", fixed = TRUE)
  expect_match(.rmd_national_sections(c7), "podzosols meubles", fixed = TRUE)
  for (lang in c("en", "pt")) {
    tr <- yaml::read_yaml(system.file("i18n", "report_translations.yaml", package = "soilKey"))[[lang]]
    for (k in c("report.rp_crosswalk_wrb2006", "report.rp_crosswalk_wrb2006_desc",
                "report.rp_crosswalk_cpcs", "report.rp_crosswalk_cpcs_desc"))
      expect_true(nzchar(tr[[k]] %||% ""), info = paste(lang, k))
    ap <- yaml::read_yaml(system.file("i18n", "translations.yaml", package = "soilKey"))[[lang]]
    for (k in c("classify.rp_wrb2006", "classify.rp_cpcs_equivalents"))
      expect_true(nzchar(ap[[k]] %||% ""), info = paste(lang, k))
  }
})

test_that("the correspondences change no classification", {
  skip_on_cran()
  # only the accessors, the report and the app read the four files
  ns <- asNamespace("soilKey")
  readers <- Filter(function(f) {
    fn <- get(f, envir = ns)
    is.function(fn) && any(grepl("\\.rp_cw_read|\\.rp_crosswalk_lines|rp2008_crosswalk\\(|\\.rp_label_dictionary",
                                 deparse(fn)))
  }, ls(ns, all.names = TRUE))
  expect_setequal(readers, c("rp2008_crosswalk", "rp2008_from_label", ".rp_label_dictionary",
                             ".rp_crosswalk_lines", ".html_national_sections", ".rmd_national_sections"))
  r <- classify_rp2008(make_rp2008_example("calcosols"), on_missing = "silent")
  expect_null(r$qualifiers$wrb2006)
  expect_false(any(grepl("WRB 2006", r$warnings, fixed = TRUE)))
})


# --- the readings -----------------------------------------------------------------

test_that("the readings of the crosswalks are Hugo's decisions", {
  skip_on_cran()
  rd <- .cw54_yaml("readings")$readings
  tp <- vapply(rd, `[[`, "", "topic")
  txt <- vapply(rd, `[[`, "", "reading")
  mine <- c("Annex 5: what is shipped", "Annex 6: the table as data and the reader's copy",
            "Annex 7: the row of a CPCS result", "Annex 7: the entries that name no reference",
            "The labels of the RP 1995")
  expect_true(all(mine %in% tp))
  expect_true(all(grepl("(Hugo, 2026-11-04).", txt[match(mine, tp)], fixed = TRUE)))
})
