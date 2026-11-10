# v0.9.391: the correspondence between the FAO-Unesco Legend of 1974 and the
# Revised Legend of 1988 (Appendix 4 of 1988, pp. 113-126, with three
# bracketed rows), fao_crosswalk() and the edition line of a result. Display
# only: it never changes a classification.

.cw391 <- function() yaml::read_yaml(system.file("rules", "fao1988", "crosswalk.yaml", package = "soilKey"))
.split391 <- function(x) unlist(strsplit(x[nzchar(x)], ", ", fixed = TRUE))

test_that("every 1974 group and unit maps to a 1988 class or is deleted", {
  skip_on_cran()
  tab <- fao_crosswalk()
  u74 <- soilKey:::.f74_unit_codes()
  expect_length(u74, 106L)
  un <- tab[tab$level == "unit", ]
  expect_setequal(un$code_1974, u74)
  expect_false(any(duplicated(un$code_1974)))
  gr <- tab[tab$level == "group", ]
  expect_setequal(gr$code_1974, soilKey:::.F74_GROUPS$code)
  # a target, or the relation "deleted" with no target
  ok <- (nzchar(tab$code_1988) & tab$relation != "deleted") |
    (!nzchar(tab$code_1988) & tab$relation == "deleted")
  expect_true(all(ok))
  expect_true(all(tab$relation %in% c("same", "renamed", "amended", "renamed_amended",
                                       "merged", "split", "deleted")))
  expect_true(all(!is.na(tab$name_1974)))
  expect_true(all(tab$page >= 115L & tab$page <= 126L))
})

test_that("the 1988 side resolves to real codes, and every 1988 unit is reached or new", {
  skip_on_cran()
  u88 <- soilKey:::.f88_unit_codes()
  g88 <- soilKey:::.F88_GROUPINGS$code
  tab <- fao_crosswalk()
  un <- tab[tab$level == "unit", ]
  expect_true(all(.split391(un$code_1988) %in% u88))
  expect_true(all(.split391(tab$code_1988[tab$level == "group"]) %in% g88))
  expect_true(all(.split391(tab$also) %in% c(u88, g88)))
  new <- fao_crosswalk(from = "1988")
  new <- new[new$relation == "new", ]
  expect_true(all(new$code_1988[new$level == "unit"] %in% u88))
  expect_true(all(new$code_1988[new$level == "group"] %in% g88))
  # every 1988 unit is the target of a 1974 unit or new, not both
  tgt <- unique(.split391(un$code_1988))
  expect_setequal(c(tgt, new$code_1988[new$level == "unit"]), u88)
  expect_length(intersect(tgt, new$code_1988), 0L)
  expect_setequal(c(.split391(tab$code_1988[tab$level == "group"]), new$code_1988[new$level == "group"]), g88)
  expect_true(all(!is.na(fao_crosswalk(from = "1988")$name_1988)))
})

test_that("the rows of Appendix 4 and the three bracketed rows", {
  skip_on_cran()
  cw <- fao_crosswalk()
  r <- function(cd) cw[cw$code_1974 == cd, ]
  expect_identical(r("Lo")$code_1988, "LVh")
  expect_identical(r("Lo")$relation, "renamed_amended")
  expect_identical(r("Lo")$also, "LXh")
  expect_identical(r("Gh")$code_1988, "GLu")
  expect_identical(r("Fa")$code_1988, "FRg")
  expect_identical(r("Pl")$code_1988, "PZb")
  expect_identical(r("Cg")$code_1988, "CHw")
  expect_identical(r("Vp")$code_1988, "VRe, VRd, VRk, VRy")
  expect_identical(r("Vp")$relation, "split")
  expect_identical(r("Od")$code_1988, "HSl, HSs, HSf")
  expect_identical(r("Ne")$code_1988, "NTh")
  expect_identical(r("Nd")$relation, "merged")
  expect_identical(r("I")$code_1988[r("I")$level == "unit"], "LPq")
  expect_identical(r("E")$code_1988[r("E")$level == "unit"], "LPk")
  expect_identical(r("U")$code_1988[r("U")$level == "unit"], "LPu")
  for (cd in c("Gp", "Zt", "Ws", "Pp")) {
    expect_identical(r(cd)$relation, "deleted")
    expect_false(r(cd)$bracketed)
  }
  # the three changes Appendix 4 does not flag
  expect_identical(r("Gc")$code_1988, "GLk")
  expect_true(r("Gc")$bracketed)
  expect_identical(r("Lp")$relation, "deleted")
  expect_true(r("Lp")$bracketed)
  hst <- fao_crosswalk("HSt", from = "1988")
  expect_identical(hst$relation, "new")
  expect_true(hst$bracketed)
  # the deleted groups of 1974
  expect_identical(r("X")$relation, "deleted")
  expect_identical(r("Y")$relation, "deleted")
  # reverse lookup
  rv <- fao_crosswalk("NTh", from = "1988")
  expect_setequal(rv$code_1974, c("Ne", "Nd"))
  rv <- fao_crosswalk("LXh", from = "1988")
  expect_true(all(c("Lo", "Lc", NA) %in% rv$code_1974))
  expect_identical(nrow(fao_crosswalk("ZZZ")), 0L)
})

test_that("the edition line: 1974 to 1988 and 1988 to 1974, display only", {
  skip_on_cran()
  r74 <- classify_fao(make_fao_example("Lo", edition = "1974"), edition = "1974", on_missing = "silent")
  l <- soilKey:::.fao_edition_lines(r74)
  expect_match(l, "^Revised Legend \\(1988\\): LVh Haplic Luvisols")
  expect_match(l, "LXh Haplic Lixisols", fixed = TRUE)
  r88 <- classify_fao(make_fao_example("NTh"), on_missing = "silent")
  l <- soilKey:::.fao_edition_lines(r88)
  expect_match(l, "^Legend of 1974: Ne Eutric Nitosols")
  expect_match(l, "Nd Dystric Nitosols", fixed = TRUE)
  l <- soilKey:::.fao_edition_lines(classify_fao(make_fao_example("HSt"), on_missing = "silent"))
  expect_match(l, "new in 1988", fixed = TRUE)
  l <- soilKey:::.fao_edition_lines(classify_fao(make_fao_example("Yk", edition = "1974"), edition = "1974",
                                                 on_missing = "silent"))
  expect_match(l, "no class of 1988", fixed = TRUE)
  # single-unit group of 1974: one level
  l <- soilKey:::.fao_edition_lines(classify_fao(make_fao_example("I", edition = "1974"), edition = "1974",
                                                 on_missing = "silent"))
  expect_match(l, "LPq Lithic Leptosols", fixed = TRUE)
  # other systems and unclassified results get no line
  expect_null(soilKey:::.fao_edition_lines(list(system_id = "wrb2022", levels = data.frame(code = "LV"))))
  expect_null(soilKey:::.fao_edition_lines(list(system_id = "fao1988", levels = NULL)))
  # print shows it; the result object is unchanged by it
  out <- paste(capture.output(print(r88), type = "message"), collapse = "\n")
  expect_match(out, "Other edition: Legend of 1974", fixed = TRUE)
})

test_that("the edition line on constructed results and a sample of examples", {
  skip_on_cran()
  mk <- function(id, codes) list(system_id = id, levels = data.frame(
    level = c("major_groups", "units")[seq_along(codes)], code = codes, stringsAsFactors = FALSE))
  el <- function(id, codes) soilKey:::.fao_edition_lines(mk(id, codes))
  # 1974 -> 1988: the unit row of a single-unit group; per-target conditions
  expect_match(el("fao1974", "I"), "LPq Lithic Leptosols (renamed", fixed = TRUE)
  l <- el("fao1974", c("L", "Lk"))
  expect_match(l, "LXh Haplic Lixisols: an argic B", fixed = TRUE)
  expect_match(l, "CLl Luvic Calcisols: a calcic", fixed = TRUE)
  expect_match(el("fao1974", c("B", "Bk")), "CMc Calcaric Cambisols (split", fixed = TRUE)
  expect_match(el("fao1974", c("N", "Ne")), "[or NTr Rhodic Nitisols", fixed = TRUE)
  # 1988 -> 1974: group-level alternatives only from the group rows, or
  # from unit rows when the result stops at the grouping
  l <- el("fao1988", c("CM", "CMc"))
  expect_match(l, "Bk Calcic Cambisols (split", fixed = TRUE)
  expect_false(grepl("Xh|Yh", l))
  l <- el("fao1988", c("FL", "FLe"))
  expect_match(l, "G Gleysols, alluvial deposits with fluvic properties]", fixed = TRUE)
  expect_false(grepl("plinthite|coarse", l))
  l <- el("fao1988", c("LX", "LXh"))
  expect_match(l, "Lo Orthic Luvisols, Lc Chromic Luvisols, Lk Calcic Luvisols, Lv Vertic Luvisols", fixed = TRUE)
  expect_false(grepl("\\bL Luvisols", l))
  expect_match(el("fao1988", "CM"), "Xh Haplic Xerosols", fixed = TRUE)
  expect_match(el("fao1988", c("NT", "NTr")), "[not flagged in App. 4]", fixed = TRUE)
  expect_match(el("fao1988", c("HS", "HSt")), "[not flagged in App. 4]", fixed = TRUE)
  expect_null(el("fao1988", character(0)))
  # a sample of classified examples keys as before and gets a line
  for (cd in c("LVh", "CMc", "FLe", "HSi", "ATa")) {
    r <- classify_fao(make_fao_example(cd), on_missing = "silent")
    expect_true(cd %in% r$levels$code, info = cd)
    expect_false(is.null(soilKey:::.fao_edition_lines(r)), info = cd)
  }
  for (cd in c("Lo", "U", "Yk", "Fa")) {
    r <- classify_fao(make_fao_example(cd, edition = "1974"), edition = "1974", on_missing = "silent")
    expect_true(cd %in% r$levels$code, info = cd)
    expect_false(is.null(soilKey:::.fao_edition_lines(r)), info = cd)
  }
})

test_that("the relations the review settled", {
  skip_on_cran()
  cw <- fao_crosswalk()
  r <- function(cd, lv = "unit") cw[cw$code_1974 == cd & cw$level == lv, ]
  for (cd in c("I", "E", "U")) expect_identical(r(cd)$relation, "renamed")
  for (cd in c("V", "O")) expect_identical(r(cd, "group")$relation, "same")
  expect_identical(r("Dd")$relation, "amended")
  expect_identical(r("Zm")$relation, "amended")
  expect_identical(r("Bk")$relation, "split")
  expect_identical(r("Lk")$also, "LXh, CLl")
  expect_identical(r("Ne")$also, "NTr")
  expect_true(fao_crosswalk("NTr", from = "1988")$bracketed[fao_crosswalk("NTr", from = "1988")$relation == "new"])
  expect_match(r("G", "group")$also_when, "FL: alluvial deposits with fluvic properties", fixed = TRUE)
})

test_that("the reports carry the edition section in both languages", {
  skip_on_cran()
  r <- classify_fao(make_fao_example("LVh"), on_missing = "silent")
  html <- soilKey:::.html_national_sections(r)
  expect_match(html, "The other edition of the FAO-Unesco legend", fixed = TRUE)
  expect_match(html, "Lo Orthic Luvisols", fixed = TRUE)
  md <- soilKey:::.rmd_national_sections(r)
  expect_match(md, "### The other edition of the FAO-Unesco legend", fixed = TRUE)
  i18n <- yaml::read_yaml(system.file("i18n", "report_translations.yaml", package = "soilKey"))
  for (lg in names(i18n)) {
    expect_true(nzchar(i18n[[lg]][["report.fao_crosswalk"]] %||% ""), info = lg)
    expect_true(nzchar(i18n[[lg]][["report.fao_crosswalk_desc"]] %||% ""), info = lg)
  }
  w <- soilKey:::.html_national_sections(classify_fao(make_fao_example("Lc", edition = "1974"),
                                                     edition = "1974", on_missing = "silent"))
  expect_match(w, "Revised Legend (1988): LVx Chromic Luvisols", fixed = TRUE)
})
