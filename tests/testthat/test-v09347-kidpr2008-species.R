# v0.9.347 -- the species of the Russian Field Guide of 2008 (chapter 5.1,
# the twelve tables of thickness and depth classes) after the subtypes, and
# the 1977 analogue of chapter 6 as a note, never used to classify (Hugo's
# decision Q1).

# A pedon in the Russian notation from tokens "DESIGNATION:thickness".
.sp_ped <- function(tokens, site = list(), ...) {
  d <- sub(":[0-9.]+$", "", tokens)
  th <- as.numeric(sub("^.*:", "", tokens))
  bot <- cumsum(th)
  h <- data.frame(designation = d, top_cm = c(0, bot[-length(bot)]), bottom_cm = bot, stringsAsFactors = FALSE)
  ex <- list(...)
  for (k in names(ex)) h[[k]] <- ex[[k]]
  PedonRecord$new(site = utils::modifyList(list(id = "ru-species", designation_system = "kidpr"), site),
                  horizons = ensure_horizon_schema(data.table::as.data.table(h)))
}
.sp_of <- function(tokens, ...) classify_kidpr2008(.sp_ped(tokens, ...), on_missing = "silent")
.sp_codes <- function(r) r$levels$code[r$levels$level == "species"]
.sp_type <- function(r) r$levels$code[r$levels$level == "types"]
.sp_tab <- function(id) Filter(function(t) identical(t$id, id), soilKey:::.kidpr_sp_rules()$tables)[[1L]]
.sp_cl <- function(id, v) soilKey:::.kidpr_sp_class_of(v, .sp_tab(id)$classes)


# --- the tables -------------------------------------------------------------------

test_that("species.yaml holds the twelve tables of 5.1 with numeric limits", {
  skip_on_cran()
  tabs <- soilKey:::.kidpr_sp_rules()$tables
  ids <- vapply(tabs, `[[`, "", "id")
  expect_identical(ids, c("humus_light", "humus_dark", "peat_horizon", "plough", "gley_place", "bleached",
                          "sel", "carbonates", "salts", "peat_decomposition", "botanical", "peat_body"))
  expect_true(all(vapply(tabs, function(t) t$page %in% 135:137, NA)))
  for (t in tabs) {
    if (t$id == "botanical") {
      expect_length(t$plants, 9L)
      next
    }
    for (cl in t$classes) {
      expect_true(nzchar(cl$name) && nzchar(cl$name_en), info = t$id)
      lim <- unlist(cl[c("from", "over", "under", "to")])
      expect_true(is.numeric(lim) || is.null(lim), info = paste(t$id, cl$id))
    }
  }
  expect_length(.sp_tab("humus_dark")$classes, 5L)
  expect_length(.sp_tab("bleached")$classes, 5L)
})

test_that("a value at a limit is read by the lower end for a minimum and the upper end for a maximum", {
  skip_on_cran()
  # dark humus: < 30, 30-50, 50-80, 80-120, > 120
  expect_identical(vapply(c(29.9, 30, 50, 80, 120, 121), function(v) .sp_cl("humus_dark", v), ""),
                   c("shallow", "thin", "medium", "thick", "thick", "very_thick"))
  # grey and light humus: < 10, 10-20, 20-30; over 30 no class
  expect_identical(vapply(c(9, 10, 20, 30, 31), function(v) .sp_cl("humus_light", v), ""),
                   c("thin", "medium", "thick", "thick", ""))
  expect_identical(vapply(c(9, 10, 20, 50, 51), function(v) .sp_cl("peat_horizon", v), ""),
                   c("", "slightly_peaty", "peaty", "peat", ""))
  expect_identical(vapply(c(19, 20, 30, 40, 41), function(v) .sp_cl("plough", v), ""),
                   c("shallow", "medium", "deep", "deep", ""))
  expect_identical(vapply(c(9, 10, 30, 45, 46), function(v) .sp_cl("bleached", v), ""),
                   c("surface", "shallow", "deep", "deep", "very_deep"))
  expect_identical(vapply(c(4, 5, 10, 20, 21), function(v) .sp_cl("sel", v), ""),
                   c("crusty", "shallow", "medium", "medium", "deep"))
  # carbonates: the last class printed "-120" is 80-120
  expect_identical(vapply(c(29, 30, 50, 80, 120, 121), function(v) .sp_cl("carbonates", v), ""),
                   c("calcareous", "slightly_leached", "medium_leached", "strongly_leached", "strongly_leached", ""))
  expect_identical(vapply(c(0, 30, 150, 151), function(v) .sp_cl("salts", v), ""),
                   c("solonchakous", "slightly_solonchakous", "deep_solonchakous", "deep_saline"))
  expect_identical(vapply(c(24, 25, 50, 75, 76), function(v) .sp_cl("peat_decomposition", v), ""),
                   c("peat", "muck_peat", "peat_muck", "peat_muck", "muck"))
  expect_identical(vapply(c(49, 50, 100, 200, 201), function(v) .sp_cl("peat_body", v), ""),
                   c("", "thin", "medium", "medium", "thick"))
  # an interval is decided only inside one class
  ci <- soilKey:::.kidpr_sp_class_interval(100, Inf, .sp_tab("peat_body")$classes)
  expect_false(ci$decided)
  expect_true(soilKey:::.kidpr_sp_class_interval(52, 79, .sp_tab("humus_dark")$classes)$decided)
})


# --- the tables on soils ----------------------------------------------------------------

test_that("humus, bleached and agro tables: the classes of each", {
  skip_on_cran()
  r <- .sp_of(c("AY:15", "EL:15", "BEL:15", "BT:40", "C:30"))
  expect_identical(.sp_type(r), "[CA04]")
  expect_identical(.sp_codes(r), c("humus_light:medium", "bleached:deep"))
  expect_identical(.sp_codes(.sp_of(c("AY:8", "EL:15", "BEL:15", "BT:40", "C:30"))),
                   c("humus_light:thin", "bleached:moderate"))
  # the dark-humus horizon, sub-horizons merged
  r <- .sp_of(c("AU:25", "AU:25", "BCA:50", "Cca:50"))
  expect_identical(.sp_type(r), "[CI01]")
  expect_identical(.sp_codes(r), c("humus_dark:medium", "carbonates:medium_leached"))
  expect_identical(.sp_codes(.sp_of(c("AU:60", "AU:61", "BCA:30", "Cca:30"))), "humus_dark:very_thick")
  expect_identical(.sp_codes(.sp_of(c("AU:10", "AU:10", "BCA:50", "Cca:50"))), c("humus_dark:shallow", "carbonates:calcareous"))
  # PU over AU counts with it; the agro horizon has its own table
  r <- .sp_of(c("PU:30", "AU:30", "BCA:40", "Cca:30"))
  expect_identical(.sp_type(r), "[CI11]")
  expect_identical(.sp_codes(r), c("humus_dark:medium", "plough:deep", "carbonates:medium_leached"))
  expect_identical(.sp_codes(.sp_of(c("P:25", "BT:55", "C:40"))), "plough:medium")
  expect_identical(.sp_codes(.sp_of(c("P:15", "BT:55", "C:40"))), "plough:shallow")
  # a plough horizon over 40 cm is in no class
  r <- .sp_of(c("P:45", "BT:55", "C:40"))
  expect_length(.sp_codes(r), 0L)
  expect_identical(r$trace$species$plough$status, "no class of the table")
  # the peat horizon of a mineral soil
  r <- .sp_of(c("T:25", "ELg:15", "BELg:15", "BTg:30", "G:20", "CG:20"))
  expect_identical(.sp_type(r), "[CA03]")
  # the lower boundary of EL at 40 cm is 15 cm under the T: shallow
  expect_identical(.sp_codes(r), c("peat_horizon:peaty", "gley_place:profile", "bleached:shallow"))
})

test_that("the bleached species takes the term of its horizon (p. 136)", {
  skip_on_cran()
  # E of a podzol: podzolic
  r <- .sp_of(c("O:5", "E:10", "BHF:25", "C:60"))
  expect_identical(.sp_codes(r), "bleached:shallow")
  expect_identical(r$qualifiers$species_native, "\u043c\u0435\u043b\u043a\u043e\u043f\u043e\u0434\u0437\u043e\u043b\u0438\u0441\u0442\u044b\u0435")
  expect_identical(r$qualifiers$species, "shallow-podzolic")
  # EL: eluvial
  r <- .sp_of(c("AY:15", "EL:15", "BEL:15", "BT:40", "C:30"))
  expect_identical(r$qualifiers$species_native[2L], "\u0433\u043b\u0443\u0431\u043e\u043a\u043e\u044d\u043b\u044e\u0432\u0438\u0430\u043b\u044c\u043d\u044b\u0435")
  # the EL of a solod: solodised
  r <- .sp_of(c("AY:10", "EL:15", "BT:30", "BCA:30", "Cca:30"))
  expect_identical(.sp_type(r), "[CA12]")
  expect_true("bleached:moderate" %in% .sp_codes(r))
  expect_true("\u043d\u0435\u0433\u043b\u0443\u0431\u043e\u043a\u043e\u043e\u0441\u043e\u043b\u043e\u0434\u0435\u043b\u044b\u0435" %in%
                r$qualifiers$species_native)
})

test_that("the depth and place of gleying", {
  skip_on_cran()
  # gleyed only from 105 cm: deep-gleyed (and the gleyic subtype)
  r <- .sp_of(c("AY:15", "EL:15", "BEL:20", "BT:55", "BTg:30", "C:20"))
  expect_true("gley_place:deep" %in% .sp_codes(r))
  # gleyed at 15 cm: surface-gleyed
  r <- .sp_of(c("AY:15", "ELg:15", "BEL:20", "BT:50", "C:30"))
  expect_true("gley_place:surface" %in% .sp_codes(r))
  # gleyed from 60 cm only: no class
  r <- .sp_of(c("AY:15", "EL:15", "BEL:20", "BT:10", "BTg:30", "C:30"))
  expect_false(any(startsWith(.sp_codes(r), "gley_place")))
  expect_identical(r$trace$species$gley_place$status, "no class of the table")
  # every mineral layer gleyed: profile-gleyed
  r <- classify_kidpr2008(make_kidpr2008_example("CH"), on_missing = "silent")
  expect_identical(.sp_codes(r), "gley_place:profile")
})

test_that("the solonetz, carbonate and salt tables", {
  skip_on_cran()
  r <- classify_kidpr2008(make_kidpr2008_example("CK"), on_missing = "silent")
  # AU 12 cm (shallow), ASN from 20 cm (medium), carbonates and salts in the
  # BCAs,cs from 45 cm
  expect_identical(.sp_codes(r), c("humus_dark:shallow", "sel:medium", "carbonates:slightly_leached",
                                   "salts:slightly_solonchakous"))
  expect_true("sel:crusty" %in% .sp_codes(.sp_of(c("SEL:3", "ASN:25", "BCAs,cs:35", "Cca,s:40"))))
  r <- .sp_of(c("AU:15", "SEL:10", "ASN:25", "BCAs,cs:35", "Cca,s:40"))
  expect_identical(.sp_type(r), "[CK05]")
  expect_true("sel:deep" %in% .sp_codes(r))
  # the solonchaks take no salt species: their formula starts with S
  r <- classify_kidpr2008(make_kidpr2008_example("CL"), on_missing = "silent")
  expect_false(any(startsWith(.sp_codes(r), "salts")))
  expect_null(r$trace$species$salts)
  # carbonates in a soil whose formula writes none: read where the profile has them
  r <- .sp_of(c("AY:15", "EL:15", "BEL:15", "BT:40", "C:30"), effervescence_class = c(NA, NA, NA, NA, "strong"))
  expect_true("carbonates:strongly_leached" %in% .sp_codes(r))
})

test_that("the peat of the organogenic trunk: decomposition, botanical composition, thickness", {
  skip_on_cran()
  r <- .sp_of(c("TO:40", "TT:60", "G:20"), peat_decomposition_pct = c(20, 30, NA),
              peat_botanical_composition = c("wood 45; sedge 45; sphagnum 10", "wood 45; sedge 45; sphagnum 10", NA))
  expect_identical(.sp_type(r), "[AA02]")
  # (40 x 20 + 60 x 30) / 100 = 26 %: muck-peat; wood and sedge over 40 %:
  # the double name of p. 136; 100 cm of peat: medium-thick
  expect_identical(.sp_codes(r), c("peat_decomposition:muck_peat", "botanical:wood+sedge", "peat_body:medium"))
  expect_identical(r$qualifiers$species_native[2L], "\u0434\u0440\u0435\u0432\u0435\u0441\u043d\u043e-\u043e\u0441\u043e\u043a\u043e\u0432\u044b\u0435")
  expect_identical(r$qualifiers$species[2L], "wood-sedge peat")
  # names without shares; no plant over 40 %
  r <- .sp_of(c("TO:30", "TT:40", "G:20"), peat_botanical_composition = c("sedge", "\u043e\u0441\u043e\u043a\u0430", NA))
  expect_true(all(c("botanical:sedge", "peat_body:thin") %in% .sp_codes(r)))
  r <- .sp_of(c("TO:50", "TT:160", "G:20"), peat_botanical_composition = "wood 30; sedge 30; sphagnum 40")
  expect_false(any(startsWith(.sp_codes(r), "botanical")))
  expect_true("peat_body:thick" %in% .sp_codes(r))
  # the agro-peat PT and the peat under it
  r <- classify_kidpr2008(make_kidpr2008_example("AB"), on_missing = "silent")
  expect_true(all(c("plough:medium", "peat_body:medium") %in% .sp_codes(r)))
})


# --- undecided -------------------------------------------------------------------------

test_that("a measure not recorded, or a horizon reaching the bottom, leaves the species undecided", {
  skip_on_cran()
  # the peat reaches the bottom of the description at 180 cm: 100-200 or
  # over 200; decomposition and botanical composition not recorded
  r <- classify_kidpr2008(make_kidpr2008_example("AA"), on_missing = "silent")
  expect_identical(r$qualifiers$species_undecided, c("peat_decomposition", "botanical", "peat_body"))
  expect_length(.sp_codes(r), 0L)
  expect_identical(r$name, "Oligotrophic peat soils [typical]")
  expect_false(any(startsWith(r$provisional_because, "species")))
  expect_true(any(startsWith(r$warnings, "Species undecided")))
  expect_true(is.na(r$trace$species$peat_body$passed))
  expect_true("peat_decomposition_pct" %in% r$trace$species$peat_decomposition$missing)
  # one peat layer recorded of three: the weighted mean stays an interval
  r <- .sp_of(c("TO:50", "TT:160", "G:20"), peat_decomposition_pct = c(80, NA, NA))
  expect_true("peat_decomposition" %in% r$qualifiers$species_undecided)
  # a depth not recorded
  p <- .sp_ped(c("AY:15", "EL:15", "BEL:15", "BT:40", "C:30"))
  p$horizons$bottom_cm[1L] <- NA
  r <- classify_kidpr2008(p, on_missing = "silent")
  if (identical(.sp_type(r), "[CA04]")) expect_true("humus_light" %in% r$qualifiers$species_undecided)
})


# --- the name ----------------------------------------------------------------------------

test_that("the species follow the subtypes in the name, in English and in Russian", {
  skip_on_cran()
  r <- classify_kidpr2008(make_kidpr2008_example("CA"), on_missing = "silent")
  expect_identical(r$name, "Sod-podzolic [typical], medium-thick, deep-eluvial")
  expect_identical(r$qualifiers$name_native, paste0(
    "\u0414\u0435\u0440\u043d\u043e\u0432\u043e-\u043f\u043e\u0434\u0437\u043e\u043b\u0438\u0441\u0442\u044b\u0435 ",
    "[\u0442\u0438\u043f\u0438\u0447\u043d\u044b\u0435], ",
    "\u0441\u0440\u0435\u0434\u043d\u0435\u043c\u043e\u0449\u043d\u044b\u0435, ",
    "\u0433\u043b\u0443\u0431\u043e\u043a\u043e\u044d\u043b\u044e\u0432\u0438\u0430\u043b\u044c\u043d\u044b\u0435"))
  lv <- r$levels
  expect_identical(lv$level, c("trunks", "divisions", "types", "subtype", "species", "species"))
  expect_identical(lv$name_native[lv$level == "species"], r$qualifiers$species_native)
  # the sod-pale-podzolic gleyic soil of p. 60, with its species
  r <- .sp_of(c("AY:15", "ELf:15", "BEL:15", "BTg:40", "C:30"))
  expect_identical(r$name, "Sod-podzolic pale-podzolic, gleyic, medium-thick, surface-gleyed, deep-eluvial")
  # species = FALSE, and subtypes = FALSE, stop before them
  r0 <- classify_kidpr2008(make_kidpr2008_example("CA"), on_missing = "silent", species = FALSE)
  expect_identical(r0$name, "Sod-podzolic [typical]")
  expect_false("species" %in% r0$levels$level)
  r1 <- classify_kidpr2008(make_kidpr2008_example("CA"), on_missing = "silent", subtypes = FALSE)
  expect_identical(r1$name, "Sod-podzolic")
  # the species never change the classes above them
  expect_identical(as.list(lv[1:4, ]), as.list(r0$levels))
  # a chemozem, and a soil whose type is unassigned, get none
  r <- classify_kidpr2008(make_kidpr2008_example("CU"), on_missing = "silent")
  expect_false("species" %in% r$levels$level)
  r <- .sp_of(c("AU:3", "ASN:25", "BCAs,cs:35", "Cca,s:40"))
  expect_true(is.na(.sp_type(r)[1L]) || !length(.sp_type(r)))
  expect_null(r$trace$species)
})


# --- the 1977 analogue ---------------------------------------------------------------------

test_that("every row of chapter 6 resolves to a type of 2008, in the order of the fixture", {
  skip_on_cran()
  rows <- soilKey:::.kidpr_c77_rules()
  expect_length(rows, 268L)
  expect_identical(vapply(rows, function(r) as.integer(r$row), 0L), seq_len(268L))
  fx <- utils::read.delim(testthat::test_path("fixtures", "kidpr2008-formulas.tsv"), comment.char = "#", quote = "",
                          stringsAsFactors = FALSE, encoding = "UTF-8", colClasses = "character")
  ch6 <- fx[startsWith(fx$source, "ch6 "), ]
  expect_identical(vapply(rows, function(r) as.character(r$formula_2004), ""), ch6$formula)
  expect_identical(vapply(rows, function(r) as.integer(r$page), 0L), as.integer(sub("^ch6 p", "", ch6$source)))
  rules <- load_rules("kidpr2008")
  known <- unlist(lapply(rules$types, function(l) vapply(l, function(t) as.character(t$code), "")))
  codes <- unlist(lapply(rows, function(r) r$type))
  expect_length(codes, 267L)
  expect_true(all(codes %in% known))
  expect_false(anyDuplicated(codes) > 0L)
  # the one 2004 type the Field Guide of 2008 does not keep
  none <- Filter(function(r) is.null(r$type), rows)
  expect_length(none, 1L)
  expect_identical(none[[1L]]$formula_2004, "H-AU-G-CG(\u0441\u0430)")
  # the types of 2008 with no row
  expect_identical(sort(setdiff(known, codes)),
                   c("[CB12]", "[CB14]", "[CD02]", "[CD05]", "[CI16]", "[CI17]", "[CI18]", "[DA08]"))
  # the fields
  nd <- vapply(rows, function(r) isFALSE(r$distinguished), NA)
  expect_identical(sum(nd), 144L)
  for (r in rows) {
    expect_true(is.logical(r$distinguished) && nzchar(r$name_2004) && nzchar(r$en), info = r$row)
    for (a in r$analogues) {
      expect_true(a$level %in% c("type", "subtype", "species", "genus", "group", "literature"), info = r$row)
      expect_true(a$extent %in% c("full", "approximately", "partly", "mostly"), info = r$row)
      expect_true(nzchar(a$name), info = r$row)
    }
    if (!isFALSE(r$distinguished)) expect_true(length(r$analogues) > 0L, info = r$row)
  }
})

test_that("the 1977 analogue is a note and a field, never a class", {
  skip_on_cran()
  # the sod-podzolic soils: the sod-podzolic subtype of the podzolic soils (p. 140)
  r <- classify_kidpr2008(make_kidpr2008_example("CA"), on_missing = "silent")
  a <- r$qualifiers$analogue_1977
  expect_identical(a$type, "[CA04]")
  expect_identical(a$status, "given")
  expect_identical(a$rows[[1L]]$page, 140L)
  expect_identical(a$rows[[1L]]$analogues$level, "subtype")
  expect_identical(a$rows[[1L]]$analogues$name,
                   "\u0434\u0435\u0440\u043d\u043e\u0432\u043e-\u043f\u043e\u0434\u0437\u043e\u043b\u0438\u0441\u0442\u044b\u0435")
  w <- grep("^1977 analogue", r$warnings, value = TRUE)
  expect_length(w, 1L)
  expect_match(w, "chapter 6, p. 140; a note only, never used to classify", fixed = TRUE)
  # chernozems: several 1977 subtypes, partly and mostly (p. 146)
  r <- classify_kidpr2008(make_kidpr2008_example("CI"), on_missing = "silent")
  an <- r$qualifiers$analogue_1977$rows[[1L]]$analogues
  expect_true(all(c("full", "partly", "mostly") %in% an$extent))
  # alluvial dark-humus soils (p. 157); oligotrophic peat soils (p. 160)
  expect_identical(classify_kidpr2008(make_kidpr2008_example("BA"), on_missing = "silent")$qualifiers$analogue_1977$rows[[1L]]$page, 157L)
  expect_identical(classify_kidpr2008(make_kidpr2008_example("AA"), on_missing = "silent")$qualifiers$analogue_1977$rows[[1L]]$page, 160L)
  # not distinguished in 1977: the cryometamorphic soils (p. 144)
  r <- classify_kidpr2008(make_kidpr2008_example("CE"), on_missing = "silent")
  expect_identical(r$qualifiers$analogue_1977$status, "not_distinguished")
  expect_false(r$qualifiers$analogue_1977$rows[[1L]]$distinguished)
  expect_match(grep("^1977 analogue", r$warnings, value = TRUE), "not distinguished in the 1977 classification", fixed = TRUE)
  # not distinguished, with what they were part of: the coarse-humus rzhavozems (p. 143)
  r <- classify_kidpr2008(make_kidpr2008_example("CC"), on_missing = "silent")
  expect_match(grep("^1977 analogue", r$warnings, value = TRUE), "formerly within:", fixed = TRUE)
  # no row in chapter 6: the coarse-humus burozems
  r <- .sp_of(c("AO:10", "BM:40", "C:40"))
  expect_identical(.sp_type(r), "[CD02]")
  expect_identical(r$qualifiers$analogue_1977$status, "no_row")
  expect_match(grep("^1977 analogue", r$warnings, value = TRUE), "has no row for the type [CD02]", fixed = TRUE)
  # the type unassigned, and the chemozems: no note
  r <- .sp_of(c("AU:3", "ASN:25", "BCAs,cs:35", "Cca,s:40"))
  expect_null(r$qualifiers$analogue_1977)
  expect_false(any(startsWith(r$warnings, "1977 analogue")))
  r <- classify_kidpr2008(make_kidpr2008_example("CU"), on_missing = "silent")
  expect_null(r$qualifiers$analogue_1977)
  # the report shows it
  r <- classify_kidpr2008(make_kidpr2008_example("CA"), on_missing = "silent")
  html <- withr::with_options(list(soilKey.report_lang = "en"), soilKey:::.html_national_sections(r))
  expect_match(html, "Name in the 1977 USSR classification", fixed = TRUE)
  expect_match(html, "AY-EL-BEL-BT-C", fixed = TRUE)
  md <- withr::with_options(list(soilKey.report_lang = "pt"), soilKey:::.rmd_national_sections(r))
  expect_match(md, "Nome na classifica", fixed = TRUE)
  expect_null(soilKey:::.kidpr_1977_lines(classify_cssc3(make_ferralsol_canonical(), on_missing = "silent")))
})


# --- the review of 2026-11-03 and Hugo's decisions ----------------------------------------

test_that("depths are measured from the mineral surface, under O and T (Hugo, 2026-11-03)", {
  skip_on_cran()
  # EL from 8 to 23 cm under an O of 8 cm: the lower boundary at 15 cm
  r <- .sp_of(c("O:8", "EL:15", "BEL:20", "BT:50", "C:30"))
  expect_identical(.sp_type(r), "[CA01]")
  expect_identical(.sp_codes(r), "bleached:shallow")
  # E from 8 to 17 cm under O: 9 cm, surface-podzolic
  expect_identical(.sp_codes(.sp_of(c("O:8", "E:9", "BHF:30", "C:40"))), "bleached:surface")
  # the peat itself is still measured as peat
  r <- .sp_of(c("TO:50", "TT:50", "G:20"))
  expect_true("peat_body:medium" %in% .sp_codes(r))
})

test_that("gleying by its continuity (Hugo, 2026-11-03)", {
  skip_on_cran()
  # a humus or agro horizon that is not gleyed is allowed over profile gleying
  expect_true("gley_place:profile" %in% .sp_codes(.sp_of(c("AU:20", "G:40", "CG:40"))))
  expect_true("gley_place:profile" %in% .sp_codes(.sp_of(c("PU:30", "G:40", "CG:40"))))
  # gleying from 49 cm to the bottom under the dark-humus horizon
  expect_true("gley_place:profile" %in% .sp_codes(.sp_of(c("AU:49", "BCAg:31", "Cg:40"))))
  # gleying at 15-30 cm only: surface
  expect_true("gley_place:surface" %in% .sp_codes(.sp_of(c("AY:15", "ELg:15", "BEL:20", "BT:50", "C:30"))))
  # gleying from 25 cm to the bottom under an EL that is not gleyed: neither
  r <- .sp_of(c("O:5", "EL:20", "BEL:15", "BTg:40", "G:40", "Cg:30"))
  expect_false(any(startsWith(.sp_codes(r), "gley_place")))
  # gleying starting at 50-100 cm: no class
  r <- .sp_of(c("AU:30", "AU:30", "BCAg:30", "Cg:40"))
  expect_false(any(startsWith(.sp_codes(r), "gley_place")))
  # the class reader on its own: an undecided layer read both ways
  tab <- .sp_tab("gley_place")
  expect_identical(soilKey:::.kidpr_sp_gley_class(c(FALSE, TRUE, TRUE), c(0, 20, 60), c(TRUE, FALSE, FALSE), tab), "profile")
  expect_identical(soilKey:::.kidpr_sp_gley_class(c(FALSE, TRUE, FALSE), c(0, 20, 60), c(TRUE, FALSE, FALSE), tab), "surface")
  expect_identical(soilKey:::.kidpr_sp_gley_class(c(FALSE, FALSE, TRUE), c(0, 60, 110), c(TRUE, FALSE, FALSE), tab), "deep")
})

test_that("cotton-grass is no herb, in any spelling; a double name puts the larger share last", {
  skip_on_cran()
  pl <- .sp_tab("botanical")$plants
  for (w in c("cotton-grass 60", "cottongrass 60", "cotton grass 60", "Eriophorum 60", "\u043f\u0443\u0448\u0438\u0446\u0430 60"))
    expect_identical(names(soilKey:::.kidpr_sp_plants_of(w, pl)), "cottongrass", info = w)
  expect_identical(names(soilKey:::.kidpr_sp_plants_of("grass 60", pl)), "herb")
  # the spellings of one plant add up, in a layer and across layers
  expect_equal(soilKey:::.kidpr_sp_plants_of("cotton-grass 30; Eriophorum 30; sphagnum 40", pl),
               c(cottongrass = 60, sphagnum = 40))
  r <- .sp_of(c("TO:50", "TT:50", "G:20"), peat_botanical_composition = c("cotton-grass 60; sphagnum 40", "Eriophorum 60; sphagnum 40", NA))
  expect_true("botanical:cottongrass" %in% .sp_codes(r))
  expect_true("\u043f\u0443\u0448\u0438\u0446\u0435\u0432\u044b\u0435" %in% r$qualifiers$species_native)
  # wood 45 with sedge 50: drevesno-osokovye (the larger share last)
  r <- .sp_of(c("TO:50", "TT:50", "G:20"), peat_botanical_composition = c("wood 45; sedge 50; sphagnum 5", "wood 45; sedge 50; sphagnum 5", NA))
  expect_true("botanical:wood+sedge" %in% .sp_codes(r))
  expect_true("\u0434\u0440\u0435\u0432\u0435\u0441\u043d\u043e-\u043e\u0441\u043e\u043a\u043e\u0432\u044b\u0435" %in% r$qualifiers$species_native)
  r <- .sp_of(c("TO:50", "TT:50", "G:20"), peat_botanical_composition = c("wood 50; sedge 45; sphagnum 5", "wood 50; sedge 45; sphagnum 5", NA))
  expect_true("botanical:sedge+wood" %in% .sp_codes(r))
  expect_true("\u043e\u0441\u043e\u043a\u043e\u0432\u043e-\u0434\u0440\u0435\u0432\u0435\u0441\u043d\u044b\u0435" %in% r$qualifiers$species_native)
})

test_that("a transition to a carbonate horizon has carbonates, and stays out of the humus thickness", {
  skip_on_cran()
  r <- .sp_of(c("AU:40", "AUBCA:30", "BCA:40", "Cca:40"))
  expect_identical(.sp_type(r), "[CI01]")
  # carbonates from 40 cm (the AUBCA); AU 40 cm thin, the AUBCA not counted
  expect_identical(.sp_codes(r), c("humus_dark:thin", "carbonates:slightly_leached"))
})

test_that("a lower boundary needs only its depth; a repeated adjective is named once", {
  skip_on_cran()
  # the top of EL not recorded: its lower boundary still decides
  p <- .sp_ped(c("AY:15", "EL:15", "BEL:20", "BT:50", "C:30"))
  p$horizons$top_cm[2L] <- NA
  r <- classify_kidpr2008(p, on_missing = "silent")
  expect_true("bleached:deep" %in% .sp_codes(r))
  expect_false("bleached" %in% r$qualifiers$species_undecided)
  # melkie twice (an AU of 4 cm, the solonetzic horizon from 8 cm): once in the name
  r <- .sp_of(c("AU:4", "SEL:4", "ASN:25", "BCAs,cs:35", "Cca,s:40"))
  expect_true(all(c("humus_dark:shallow", "sel:shallow") %in% .sp_codes(r)))
  mel <- "\u043c\u0435\u043b\u043a\u0438\u0435"
  expect_identical(lengths(regmatches(r$qualifiers$name_native, gregexpr(mel, r$qualifiers$name_native))), 1L)
  expect_identical(lengths(regmatches(r$name, gregexpr("shallow", r$name))), 1L)
})

test_that("the 1977 note keeps what the table adds to a unit, and the Rmd report escapes the marks", {
  skip_on_cran()
  rows <- soilKey:::.kidpr_c77_rules()
  sm <- function(n) soilKey:::.kidpr_c77_sentence(rows[[n]])
  expect_match(sm(58), "varieties", fixed = TRUE)
  expect_match(sm(149), "\u0440\u0438\u0445\u043a\u043e\u0432\u044b\u0435", fixed = TRUE)
  expect_match(sm(188), "eroded", fixed = TRUE)
  expect_match(sm(180), "\u0442\u043e\u0440\u0444\u044f\u043d\u044b\u0435 \u0431\u043e\u043b\u043e\u0442\u043d\u044b\u0435 \u0432\u0435\u0440\u0445\u043e\u0432\u044b\u0435", fixed = TRUE)
  expect_match(sm(180), "reclaimed", fixed = TRUE)
  # the alluvial dark-humus soils, AU-C~~: escaped in the Markdown of the PDF
  r <- classify_kidpr2008(make_kidpr2008_example("BA"), on_missing = "silent")
  md <- withr::with_options(list(soilKey.report_lang = "en"), soilKey:::.rmd_national_sections(r))
  expect_match(md, "AU-C(ca)\\~\\~", fixed = TRUE)
  expect_false(grepl("[^\\\\]~~", md))
  expect_identical(soilKey:::.md_escape_marks("a`b"), "a\\`b")
})
