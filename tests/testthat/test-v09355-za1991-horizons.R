# v0.9.355: the South African soil classification of 1991, part 1: the
# catalogue of the 30 diagnostic horizons and materials
# (inst/rules/za1991/horizons.yaml), the reader of their names in
# diagnostic_za and in designation (R/designation-za.R), the declared
# notation, the hints of the FAO letters, and the new attributes. Nothing is
# classified yet.

.zr <- function(x, where = "diagnostic_za") soilKey:::.za_read_name(x, where)
.zd <- function(dz = NULL, ds = NULL, site = list()) soilKey:::.za_designations(dz, ds, site)
.has_note <- function(notes, pat) any(grepl(pat, unlist(notes), fixed = TRUE))

test_that("the catalogue: 5 topsoil and 25 subsoil horizons and materials", {
  skip_on_cran()
  v <- soilKey:::.za_vocabulary()
  i <- v$info
  expect_identical(nrow(i), 30L)
  expect_false(anyDuplicated(i$token) > 0L)
  expect_setequal(i$token[i$group == "topsoil"], c("OO", "HA", "VA", "MA", "OA"))
  expect_identical(sum(i$group == "subsoil"), 25L)
  expect_setequal(i$token, c("OO", "HA", "VA", "MA", "OA", "E", "G", "RAB", "YAB", "RSB", "NCU",
                             "NCA", "PCU", "PRC", "LCU", "PZB", "PLP", "SPL", "HPL", "UMW", "USW",
                             "SCH", "HCH", "DOR", "SAP", "HRK", "SAL", "RSA", "MMD", "UMN"))
  expect_true(all(grepl("^[A-Z]+$", i$token)))
  expect_true(all(i$detection %in% c("distinctive", "generic")))
  expect_true(all(i$kind %in% c("horizon", "material")))
  expect_true(all(nzchar(i$name) & nzchar(i$name_en) & nzchar(i$pages) & nzchar(i$in_1977)))
  expect_true(all(i$master %in% c("O", "A", "E", "B", "G", "C", "R")))
  # every key reads one horizon; every hint and 1977 target is a token
  expect_false(anyDuplicated(v$keys$key) > 0L)
  hint <- unique(unlist(lapply(v$fao_hints, `[[`, "tokens")))
  expect_true(all(hint %in% i$token))
  to <- unique(unlist(lapply(v$names_1977, `[[`, "to")))
  expect_true(all(to %in% i$token))
  to <- unique(unlist(lapply(v$words, `[[`, "to")))
  expect_true(all(to %in% i$token))
  expect_false(anyDuplicated(v$patterns$pattern) > 0L)
  # the HPL of 1977 and the dorbank pages (review D6)
  expect_match(i["HPL", "in_1977"], "Glencoe, Wasbank in 1977")
  expect_match(i["DOR", "pages"], "K 45, 61, 66, 70")
  ct <- soilKey:::.za_catalogue()
  expect_identical(ct$edition_id, "za1991")
})

test_that("every printed name and variant reads to its token", {
  skip_on_cran()
  ct <- soilKey:::.za_catalogue()
  for (e in ct$horizons) {
    for (w in c(e$name, unlist(e$variants), unlist(e$generic_variants), unlist(e$short), e$token)) {
      r <- .zr(w)
      expect_identical(r$token, e$token, info = w)
      expect_identical(r$name, e$name, info = w)
      expect_false(r$is_1977, info = w)
      for (ww in c(toupper(w), tolower(w), gsub(" ", "-", w), paste0("  ", w, " "))) {
        expect_identical(.zr(ww)$token, e$token, info = ww)
      }
    }
  }
  expect_identical(.zr("Orthic A horizon")$token, "OA")
  expect_identical(.zr("Soft plinthic B horizon")$token, "SPL")
  expect_identical(.zr("Neocarbonate horizon")$token, "NCA")
  expect_identical(.zr("yellow brown apedal")$token, "YAB")
  expect_identical(.zr("Man made soil deposit")$token, "MMD")
  expect_identical(.zr("Unconsolidated material without signs of wetness")$token, "UMN")
  expect_identical(.zr("unconsolidated material with signs of wetness")$token, "UMW")
  expect_identical(.zr("UNSPECIFIED MATERIAL WITH SIGNS OF WETNESS")$token, "USW")
  expect_identical(.zr("red apedal B")$name_en,
                   "red apedal B horizon (uniformly red, structure weaker than moderate)")
  expect_identical(soilKey:::.za_label("RAB"), "Red apedal B [RAB]")
  expect_identical(soilKey:::.za_label(""), "no diagnostic horizon")
})

test_that("the short forms are not read; the tokens, E and G alone are (reading 1)", {
  skip_on_cran()
  for (k in c("ot", "re", "ye", "sp", "hp", "ne", "pd", "pr", "li", "gh", "do", "SP", "Do")) {
    r <- .zr(k)
    expect_true(is.na(r$token), info = k)
    expect_length(r$candidates, 0L)
  }
  expect_identical(.zr("E")$token, "E")
  expect_identical(.zr("G")$token, "G")
  for (t in c("OA", "RAB", "SPL", "rab", "Hch")) {
    r <- .zr(t)
    expect_identical(r$token, toupper(t), info = t)
    expect_identical(r$kind, "token", info = t)
  }
  # neither tokens nor short forms inside a designation
  for (k in c("Ap ot", "B1 re", "B2 RAB", "C SAP")) {
    expect_true(is.na(.zr(k, "designation")$token), info = k)
  }
  d <- .zd(c("OA", "RAB", NA), c("A", "B1", "C"))
  expect_identical(d$token, c("OA", "RAB", NA))
})

test_that("the 1977 names read to their 1991 equivalent, flagged", {
  skip_on_cran()
  r <- .zr("Ferrihumic B")
  expect_identical(r$token, "PZB")
  expect_true(r$is_1977)
  expect_identical(r$name_1977, "Ferrihumic B")
  expect_true(.has_note(r$notes, "1977 name"))
  expect_identical(.zr("ferrihumic")$token, "PZB")
  for (x in c("Calcareous red apedal B", "calcareous yellow-brown apedal B", "Calcareous neocutanic B")) {
    expect_identical(.zr(x)$token, "NCA", info = x)
    expect_true(.zr(x)$is_1977, info = x)
  }
  g <- .zr("gleycutanic B")
  expect_true(is.na(g$token))
  expect_setequal(g$candidates, c("G", "USW"))
  expect_true(g$is_1977)
  # by the horizon above: G under an A or E, USW under an apedal B
  d <- .zd(c("orthic A", "E horizon", "gleycutanic B"), c("A", "E", "B"))
  expect_identical(d$token, c("OA", "E", "G"))
  expect_true(d$is_1977[3])
  d <- .zd(c("orthic A", "yellow-brown apedal B", "gleycutanic B"))
  expect_identical(d$token, c("OA", "YAB", "USW"))
  d <- .zd(c("orthic A", "none", "gleycutanic B"))
  expect_identical(d$token, c("OA", "", "G"))
  d <- .zd(c("gleycutanic B"))
  expect_true(is.na(d$token))
  expect_setequal(d$candidates[[1]], c("G", "USW"))
  expect_true(.has_note(d$notes[[1]], "undecided"))
  d <- .zd(c("orthic A", "pedocutanic B", "gleycutanic B"))
  expect_true(is.na(d$token[3]))
  # a 1977 name in a designation recognises the notation
  d <- .zd(NULL, c("Ap", "B2 ferrihumic", "C"))
  expect_identical(d$status, "recognised")
  expect_identical(d$token, c(NA, "PZB", NA))
})

test_that("no diagnostic horizon, nothing recorded, two names, unknown words", {
  skip_on_cran()
  for (x in c("none", "None", "no subsoil", "non-diagnostic saprolite", "not diagnostic",
              "no diagnostic horizon")) {
    r <- .zr(x)
    expect_identical(r$token, "", info = x)
    expect_identical(r$kind, "none", info = x)
  }
  for (x in list(NA, NA_character_, "", "  ", "-", "?", "NA", "nd", NULL, character(0))) {
    r <- .zr(x)
    expect_true(is.na(r$token))
    expect_false(r$recorded)
  }
  r <- .zr("red apedal B / soft plinthic B")
  expect_true(is.na(r$token))
  expect_setequal(r$candidates, c("RAB", "SPL"))
  expect_identical(.zr("red apedal B or yellow-brown apedal B")$kind, "several")
  r <- .zr("B1 red apedal")
  expect_identical(r$token, "RAB")
  expect_true(.has_note(r$notes, "read inside the text"))
  expect_identical(.zr("calcareous stratified alluvium")$token, "SAL")
  r <- .zr("rooi apedale B")
  expect_true(is.na(r$token))
  expect_true(r$recorded)
  expect_true(.has_note(r$notes, "Afrikaans"))
  expect_true(.has_note(.zr("ortiese A-horison")$notes, "Afrikaans"))
  expect_true(.has_note(.zr("xyz")$notes, "not a name"))
  # a master letter is a hint, never the name (E and G are names)
  r <- .zr("Bt")
  expect_true(is.na(r$token))
  expect_identical(r$kind, "letter")
  d <- .zd(c("A", "Bt"), c("A1", "B2"))
  expect_true(all(is.na(d$token)))
  expect_true(all(is.na(d$route)))
  expect_identical(d$hint[[1]], c("OA", "HA", "VA", "MA"))
  expect_identical(d$hint[[2]], c("PCU", "PRC", "RSB", "NCU"))
  expect_identical(lengths(d$candidates), c(0L, 0L))
  # never an error
  for (x in list(1:3, factor("orthic A"), list("orthic A", 2), "\xff\xfe", "red \u2013 apedal B")) {
    expect_no_error(.zr(x))
    expect_no_error(.zr(x, "designation"))
  }
  expect_identical(.zr("Red\u2013apedal B")$token, "RAB")
  expect_no_error(.zd(1:3, list("A", 2), "not a site"))
  expect_no_error(.zd(NULL, NULL, NULL))
})

test_that("the names written in designation, by declaration or recognition", {
  skip_on_cran()
  ds <- c("Ap orthic A", "B1 red apedal", "B2 red apedal B", "C saprolite")
  d <- .zd(NULL, ds)
  expect_true(d$za)
  expect_identical(d$status, "recognised")
  expect_identical(d$token, c("OA", "RAB", "RAB", "SAP"))
  expect_identical(d$source, rep("designation", 4))
  expect_identical(d$route, rep("notation", 4))
  expect_identical(d$label, c("Orthic A [OA]", "Red apedal B [RAB]", "Red apedal B [RAB]", "Saprolite [SAP]"))
  # generic names alone do not recognise the profile
  d <- .zd(NULL, c("A1", "Bt", "Cr (saprolite)", "R hard rock"))
  expect_false(d$za)
  expect_identical(d$status, "none")
  expect_true(all(is.na(d$token)))
  expect_true(.has_note(d$notes[[3]], "not declared or recognised"))
  expect_identical(d$hint[[3]], c("SAP", "UMW", "USW", "G", "LCU"))
  expect_identical(d$hint[[4]], "HRK")
  # declared: the generic names are read too
  for (sys in c("za", "ZA1991", "sat", "SAT 1991", "south african", "South African 1991",
                "South Africa", "south_africa", "southafrican")) {
    d <- .zd(NULL, c("A1", "Bt", "Cr saprolite", "R hard rock"), list(designation_system = sys))
    expect_identical(d$status, "declared", info = sys)
    expect_identical(d$token, c(NA, NA, "SAP", "HRK"), info = sys)
  }
  expect_true(soilKey:::.za_is_declared("sat1991"))
  expect_false(soilKey:::.za_is_declared("fao"))
  expect_false(soilKey:::.za_is_declared("russian"))
  expect_false(soilKey:::.za_is_declared(NA))
  # another notation declared: no designation is read as a name
  for (sys in c("fao", "rp", "kidpr", "cssc", "usda")) {
    d <- .zd(NULL, ds, list(designation_system = sys))
    expect_false(d$za, info = sys)
    expect_identical(d$status, "other", info = sys)
    expect_true(all(is.na(d$token)), info = sys)
    expect_true(.has_note(d$notes[[1]], "declares another notation"), info = sys)
  }
  # diagnostic_za is read whatever the site declares, and makes the profile's
  # designations readable
  d <- .zd(c("orthic A", NA), c("A", "B1 red apedal"), list(designation_system = "fao"))
  expect_identical(d$token, c("OA", NA))
  d <- .zd(c("orthic A", NA, NA), c("A", "B1 red apedal", "C saprolite"))
  expect_identical(d$status, "diagnostic_za")
  expect_identical(d$token, c("OA", "RAB", "SAP"))
  expect_identical(d$source, c("diagnostic_za", "designation", "designation"))
  # the country is never read
  d <- .zd(NULL, c("A1", "Bt", "C"), list(country = "ZA"))
  expect_false(d$za)
  d <- .zd(NULL, c("A1", "Bt", "C"), list(country = "South Africa"))
  expect_false(d$za)
})

test_that("a recorded name is kept against the designation, with notes", {
  skip_on_cran()
  d <- .zd(c("yellow-brown apedal B"), c("B1 red apedal"))
  expect_identical(d$token, "YAB")
  expect_identical(d$source, "diagnostic_za")
  expect_true(.has_note(d$notes[[1]], "names the Red apedal B"))
  d <- .zd(c("red apedal B", "orthic A"), c("A1", "B2"))
  expect_identical(d$token, c("RAB", "OA"))
  expect_true(.has_note(d$notes[[1]], "the name is kept"))
  expect_true(.has_note(d$notes[[2]], "the name is kept"))
  d <- .zd(c("xyz"), c("B1 red apedal"))
  expect_identical(d$token, "RAB")
  expect_identical(d$source, "designation")
})

test_that("the FAO letters give hints only", {
  skip_on_cran()
  h <- function(x) soilKey:::.za_hint(x)$hint
  expect_identical(h("E"), "E")
  expect_identical(h("Ap"), c("OA", "HA", "VA", "MA"))
  expect_identical(h("Bt1"), c("PCU", "PRC", "RSB", "NCU"))
  expect_identical(h("B21t"), c("PCU", "PRC", "RSB", "NCU"))
  expect_identical(h("2Bt"), c("PCU", "PRC", "RSB", "NCU"))
  expect_identical(h("Bw"), c("RAB", "YAB", "NCU"))
  expect_identical(h("Bk"), c("NCA", "SCH", "HCH"))
  expect_identical(h("Bkm"), "HCH")
  expect_identical(h("Bhs"), "PZB")
  expect_identical(h("Bs"), "PZB")
  expect_identical(h("Bsm"), c("SPL", "HPL", "PLP"))
  expect_identical(h("Bv"), c("SPL", "HPL", "PLP"))
  expect_identical(h("Bqm"), "DOR")
  expect_identical(h("Bg"), c("G", "USW", "UMW"))
  expect_identical(h("Cg"), c("G", "USW", "UMW"))
  expect_identical(h("G"), c("G", "USW", "UMW"))
  expect_identical(h("C"), c("UMN", "SAP", "SAL", "RSA"))
  expect_identical(h("Ck"), c("SCH", "HCH"))
  expect_identical(h("C(k)"), c("SCH", "HCH"))
  expect_identical(h("Cr"), c("SAP", "UMW", "USW", "G", "LCU"))
  expect_identical(h("Cqm"), "DOR")
  expect_identical(h("Cu"), "MMD")
  expect_identical(h("^C"), "MMD")
  expect_identical(h("2^C"), "MMD")
  expect_identical(h("Bc"), c("SPL", "HPL"))
  expect_setequal(h("Bcs"), c("SPL", "HPL", "PZB"))
  # ss is one suffix with no hint (review D5)
  expect_length(h("Bss"), 0L)
  expect_identical(h("Bkss"), c("NCA", "SCH", "HCH"))
  expect_identical(h("Btss"), c("PCU", "PRC", "RSB", "NCU"))
  # words give no hint
  for (w in c("Bleached", "Ochric", "Calcareous", "Bedrock", "Clay", "Clay loam", "Rock", "Hutton",
              "Hu1200", "Humus", "Brown", "Gravel", "Gleyed", "Organic", "Ahorizon")) {
    expect_length(h(w), 0L)
  }
  expect_identical(h("R"), "HRK")
  expect_identical(h("Oa"), "OO")
  expect_identical(h("H"), "OO")
  expect_length(h("B"), 0L)
  expect_length(h("xyz"), 0L)
  expect_length(h(NA), 0L)
  expect_setequal(h("Btg"), c("PCU", "PRC", "RSB", "NCU", "G", "USW", "UMW"))
  # a declared profile with letters only: hints, no name
  d <- .zd(NULL, c("Ap", "Bt", "C"), list(designation_system = "za"))
  expect_true(all(is.na(d$token)))
  expect_identical(d$hint_source, rep("FAO letters", 3))
  # no hints when the site declares a notation whose letters are not FAO's
  for (sys in c("kidpr", "rp", "cpcs", "cssc")) {
    d <- .zd(NULL, c("Ap", "Bt", "C"), list(designation_system = sys))
    expect_identical(lengths(d$hint), c(0L, 0L, 0L), info = sys)
  }
  for (sys in c("fao", "usda", "FAO 2006")) {
    d <- .zd(NULL, c("Ap", "Bt", "C"), list(designation_system = sys))
    expect_identical(d$hint[[2]], c("PCU", "PRC", "RSB", "NCU"), info = sys)
  }
})

test_that("FAO, Russian and RP profiles are not read as South African, nor the reverse", {
  skip_on_cran()
  fao <- c("Ap", "Bt1", "Bt2", "BC", "C")
  ru <- c("AY", "BEL", "BT", "C")
  rp <- c("LA", "E", "BT", "C")
  for (ds in list(fao, ru, rp)) {
    d <- .zd(NULL, ds)
    expect_false(d$za)
    expect_identical(d$status, "none")
    expect_true(all(is.na(d$token)))
  }
  za <- c("Ap orthic A", "B1 red apedal", "B2 soft plinthic B", "C")
  expect_false(isTRUE(soilKey:::.rp_notation(za, list())$ok))
  expect_false(isTRUE(soilKey:::.kidpr_notation(za, list())$ok))
  expect_false(isTRUE(soilKey:::.rp_notation(c("A", "B", "C"), list(designation_system = "za"))$ok))
  expect_false(isTRUE(soilKey:::.kidpr_notation(c("A", "B", "C"), list(designation_system = "sat"))$ok))
})

test_that("a pedon's layers are read from its horizons and site", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 25, 70), bottom_cm = c(25, 70, 140),
                  designation = c("Ap", "B1", "B2"),
                  diagnostic_za = c("orthic A", "red apedal B", "soft plinthic B"),
                  clay_pct = c(12, 25, 30), stringsAsFactors = FALSE)
  p <- PedonRecord$new(site = list(id = "za-1", country = "ZA"), horizons = h)
  expect_identical(p$horizons$diagnostic_za, c("orthic A", "red apedal B", "soft plinthic B"))
  d <- soilKey:::.za_pedon_names(p)
  expect_identical(d$token, c("OA", "RAB", "SPL"))
  expect_identical(d$status, "diagnostic_za")
  p2 <- PedonRecord$new(site = list(id = "za-2"), horizons = h[, -4])
  d2 <- soilKey:::.za_pedon_names(p2)
  expect_true(all(is.na(d2$token)))
  expect_identical(d2$status, "none")
})

test_that("the new attributes are documented, naming za1991", {
  skip_on_cran()
  a <- horizon_attribute_spec()
  hz <- c("diagnostic_za", "plasticity_index", "liquid_limit_method", "bedrock_pct",
          "coatings_hue_moist", "coatings_chroma_moist", "coatings_continuity",
          "recent_deposit", "cross_bedding", "sand_grade")
  expect_true(all(hz %in% a$name))
  expect_true(all(a$systems[a$name %in% hz] == "za1991"))
  expect_identical(a$unit[a$name == "coatings_chroma_moist"], "Munsell")
  expect_identical(a$unit[a$name == "coatings_hue_moist"], "Munsell")
  expect_true(all(a$section[a$name %in% hz] == "South African system (1991)"))
  expect_true(all(grepl("South African soil classification of 1991", a$description[a$name %in% hz])))
  expect_identical(a$type[a$name == "recent_deposit"], "logical")
  expect_identical(a$type[a$name == "cross_bedding"], "logical")
  expect_identical(a$type[a$name == "plasticity_index"], "numeric")
  expect_identical(a$allowed[a$name == "liquid_limit_method"], "casagrande_cup, bs_cone")
  expect_identical(a$allowed[a$name == "coatings_continuity"], "continuous, discontinuous, patchy")
  expect_identical(a$allowed[a$name == "sand_grade"], "fine, medium, coarse")
  expect_true(all(hz %in% names(pedon_json_schema()$properties$horizons$items$properties)))
  s <- site_attribute_spec()
  st <- c("reference_za_form", "reference_za_family", "reference_za_raw", "reference_za_edition")
  expect_true(all(st %in% s$name))
  expect_true(all(!nzchar(s$systems[s$name %in% st])))
  expect_identical(s$allowed[s$name == "reference_za_edition"], "1977, 1991, 2018")
  ds <- strsplit(s$allowed[s$name == "designation_system"], ", ", fixed = TRUE)[[1L]]
  expect_true(all(c("za", "za1991", "sat", "sat1991", "south africa", "south african",
                    "south_africa", "southafrican") %in% ds))
  for (v in ds[grepl("za|sat|south", ds)]) expect_true(soilKey:::.za_is_declared(v), info = v)
  for (v in c("za", "za1991", "sat", "sat1991", "south african")) {
    expect_true(soilKey:::.za_is_declared(v), info = v)
  }
  # stone_line and stripped_grains are not added (default 21)
  expect_false(any(c("stone_line", "stripped_grains") %in% a$name))
})

test_that("the readings: Hugo's decisions and soilKey's readings for him", {
  skip_on_cran()
  rd <- yaml::read_yaml(system.file("rules", "za1991", "readings.yaml", package = "soilKey"))$readings
  r <- vapply(rd, `[[`, "", "reading")
  # the 13 readings of this part are Hugo's decisions of 2026-11-04; a later
  # part adds its own, decided by Hugo or waiting for his word (0.9.357)
  expect_true(all(grepl("\\(Hugo, 2026-11-04\\)\\.?$", r[1:13])))
  expect_true(all(grepl("\\((Hugo|soilKey's reading, for Hugo to confirm), 2026-11-(0[4-9]|1[0-9])\\)\\.?$", r)))
  expect_true(length(r) >= 13L)
  expect_true(all(nzchar(vapply(rd, `[[`, "", "topic"))))
})

test_that("the reader's R code is ASCII", {
  skip_on_cran()
  src <- testthat::test_path("..", "..", "R", "designation-za.R")
  skip_if_not(file.exists(src), "package sources not available")
  txt <- readLines(src, warn = FALSE, encoding = "UTF-8")
  expect_false(any(grepl("[^\\x01-\\x7F]", txt, perl = TRUE)))
})

test_that("negations, non-diagnostic names and doubt in diagnostic_za (review D1)", {
  skip_on_cran()
  for (x in c("not red apedal B", "no E horizon", "not an E horizon", "E horizon absent",
              "without E horizon", "none; hard rock", "not E")) {
    r <- .zr(x)
    expect_true(is.na(r$token), info = x)
    expect_length(r$candidates, 0L)
    expect_identical(r$kind, "negated", info = x)
    expect_true(.has_note(r$notes, "not read"), info = x)
  }
  for (x in c("saprolite (non-diagnostic)", "Non diagnostic saprolite", "none (lithosol)",
              "No diagnostic subsoil", "-- none --", "hard rock, not diagnostic")) {
    expect_identical(.zr(x)$token, "", info = x)
  }
  # a name that holds 'without' is no negation
  expect_identical(.zr("Unconsolidated material without signs of wetness")$token, "UMN")
  expect_identical(.zr("unconsolidated material without signs of wetness (Concordia)")$token, "UMN")
  for (x in c("E?", "possibly E", "red apedal B (?)", "probably red apedal B", "red apedal B?")) {
    r <- .zr(x)
    expect_true(is.na(r$token), info = x)
    expect_length(r$candidates, 1L)
    expect_true(r$doubt, info = x)
    expect_true(.has_note(r$notes, "written with doubt"), info = x)
  }
  expect_identical(.zr("possibly E")$candidates, "E")
  expect_setequal(.zr("possibly red apedal or yellow-brown apedal B")$candidates, c("RAB", "YAB"))
  d <- .zd(c("orthic A", "red apedal B?"), c("A", "B"))
  expect_identical(d$token, c("OA", NA))
  expect_identical(d$candidates[[2]], "RAB")
  expect_identical(d$route, c("notation", "notation"))
  expect_identical(d$doubt, c(FALSE, TRUE))
  # a junk diagnostic_za does not switch the designations on
  d <- .zd(c("xyz", NA), c("A humic", "Bw"))
  expect_identical(d$status, "none")
  expect_true(all(is.na(d$token)))
  d <- .zd(c("not red apedal B", NA), c("A humic", "Bw"))
  expect_identical(d$status, "none")
})

test_that("variants inside longer text, and calcareous after a name (review D2, R4)", {
  skip_on_cran()
  for (x in c("Soft carbonate B", "soft carbonate B horizon", "B2 soft carbonate", "soft carbonate")) {
    expect_identical(.zr(x)$token, "SCH", info = x)
  }
  for (x in c("Hardpan carbonate B", "hardpan carbonate B horizon", "B3 hardpan carbonate")) {
    expect_identical(.zr(x)$token, "HCH", info = x)
  }
  expect_identical(.zr("neocarbonate B2")$token, "NCA")
  expect_identical(.zr("B2 soft carbonate", "designation")$token, "SCH")
  expect_identical(.zr("Bk neocarbonate B2", "designation")$token, "NCA")
  for (x in c("red apedal B, calcareous", "Neocutanic B (calcareous)")) {
    r <- .zr(x)
    expect_false(r$is_1977, info = x)
    expect_true(.has_note(r$notes, "'calcareous' written with"), info = x)
  }
  expect_identical(.zr("red apedal B, calcareous")$token, "RAB")
  expect_false(.has_note(.zr("calcareous red apedal B")$notes, "'calcareous' written with"))
  expect_false(.has_note(.zr("calcareous, neocarbonate")$notes, "'calcareous' written with"))
})

test_that("calcrete, ferricrete, ouklip and silcrete by their adjective (reading 4)", {
  skip_on_cran()
  r <- .zr("calcrete")
  expect_true(is.na(r$token))
  expect_identical(r$candidates, c("SCH", "HCH"))
  expect_identical(.zr("hardpan calcrete")$token, "HCH")
  expect_identical(.zr("powder calcrete")$token, "SCH")
  expect_identical(.zr("Nodular calcrete")$token, "SCH")
  expect_identical(.zr("honeycomb calcrete")$candidates, c("SCH", "HCH"))
  r <- .zr("nodular ferricrete")
  expect_true(is.na(r$token))
  expect_identical(r$candidates, c("SPL", "HPL"))
  for (x in c("ouklip", "Ouklip", "ferricrete", "hardpan ferricrete", "hard plinthite")) {
    expect_identical(.zr(x)$token, "HPL", info = x)
  }
  expect_identical(.zr("silcrete")$token, "HRK")
  # generic: they never recognise a profile
  d <- .zd(NULL, c("A", "Bkm calcrete", "R silcrete"))
  expect_identical(d$status, "none")
  d <- .zd(NULL, c("A", "Bkm hardpan calcrete", "R silcrete"), list(designation_system = "za"))
  expect_identical(d$token, c(NA, "HCH", "HRK"))
})

test_that("the gleycutanic B under an undecided or unread layer (review D3)", {
  skip_on_cran()
  d <- .zd(c("orthic A", "red apedal or yellow-brown apedal B", "gleycutanic B"))
  expect_identical(d$token[3], "USW")
  d <- .zd(c("orthic A", NA, "gleycutanic B"), c("A", "Bw", "B2"))
  expect_identical(d$token[3], "USW")
  d <- .zd(c("orthic A", NA, "gleycutanic B"), c("A", "B1", "B2"))
  expect_true(is.na(d$token[3]))
  expect_true(.has_note(d$notes[[3]], "not read"))
  d <- .zd(c("orthic A", NA, "gleycutanic B"), c("A", "Bt", "B2"))
  expect_true(is.na(d$token[3]))
  d <- .zd(c("orthic A", "red apedal B / E horizon", "gleycutanic B"))
  expect_true(is.na(d$token[3]))
  # only the layers that hold no diagnostic horizon are skipped
  d <- .zd(c("orthic A", "none", "gleycutanic B"))
  expect_identical(d$token, c("OA", "", "G"))
  d <- .zd(c("yellow-brown apedal B", "xyz", "gleycutanic B"), c("B1", "B2", "B3"))
  expect_true(is.na(d$token[3]))
})

test_that("SA and RSA declare nothing; the master letter of a designation name (review D4, R3)", {
  skip_on_cran()
  for (sys in c("SA", "RSA", "sa")) {
    d <- .zd(NULL, c("Ap orthic A", "B1 red apedal"), list(designation_system = sys))
    expect_identical(d$status, "recognised", info = sys)
    expect_identical(d$token, c("OA", "RAB"), info = sys)
    d <- .zd(NULL, c("A1", "Bt"), list(designation_system = sys))
    expect_identical(d$status, "none", info = sys)
    expect_identical(d$hint[[2]], c("PCU", "PRC", "RSB", "NCU"), info = sys)
  }
  expect_false(soilKey:::.za_is_declared("SA"))
  expect_false(soilKey:::.za_is_declared("RSA"))
  d <- .zd(NULL, c("A vertic A", "Bss vertic"), list(designation_system = "za"))
  expect_identical(d$token, c("VA", "VA"))
  expect_false(.has_note(d$notes[[1]], "the name is kept"))
  expect_true(.has_note(d$notes[[2]], "the name is kept"))
})
