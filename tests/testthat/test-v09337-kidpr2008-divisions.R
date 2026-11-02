# v0.9.337: the Russian soil classification (Field Guide of 2008) in the
# registry, its trunks (key 2.2) and its 27 divisions (key 2.3), the keys
# flattened into first-match lists (inst/rules/kidpr2008/key.yaml and
# divisions/), Hugo's decisions of 2026-11-02 (Q1, Q2, Q9-Q12, Q19-Q23).
# Profiles are written in the Russian notation, which gives the horizons; the
# oracle (helper-kidpr-oracle.R) is a literal walker of the two keys.

# A pedon in the Russian notation from tokens "CODE:thickness" (top-down from
# `top`), with site fields and horizon columns (one value per layer).
.ru_ped <- function(tokens, site = list(), top = 0, ...) {
  d <- sub(":[0-9.]+$", "", tokens)
  th <- as.numeric(sub("^.*:", "", tokens))
  bot <- top + cumsum(th)
  h <- data.frame(designation = d, top_cm = c(top, bot[-length(bot)]), bottom_cm = bot,
                  stringsAsFactors = FALSE)
  extra <- list(...)
  for (k in names(extra)) h[[k]] <- extra[[k]]
  PedonRecord$new(site = utils::modifyList(list(id = "ru-test", designation_system = "kidpr"), site),
                  horizons = ensure_horizon_schema(data.table::as.data.table(h)))
}
.ru_class <- function(p) classify_kidpr2008(p, on_missing = "silent")
.ru_label <- function(r) {
  lv <- r$levels
  paste0(if (nrow(lv)) lv$code[1L] else NA, "/", if (nrow(lv) > 1L) lv$code[2L] else "-")
}
.ru_divisions <- c("AA", "AB", "BA", "BB", "BC", "DA", "CA", "CB", "CC", "CD", "CE", "CF", "CG", "CH",
                   "CI", "CJ", "CK", "CL", "CM", "CN", "CO", "CP", "CQ", "CR", "CS", "CT", "CU")


# --- the registry and the rules ------------------------------------------------------

test_that("the registry has the Russian system, experimental and opt-in", {
  skip_on_cran()
  s <- soilkey_systems()
  r <- s[s$id == "kidpr2008", ]
  expect_identical(nrow(r), 1L)
  expect_identical(r$classifier, "classify_kidpr2008")
  expect_identical(r$levels, "trunks, divisions")
  expect_identical(r$aliases, "kidpr, russia, ru2008")
  expect_identical(r$status, "experimental")
  expect_false(r$default)
  sys <- .system_get("kidpr2008")
  expect_identical(sys$namespace, "kidpr")
  expect_identical(sys$top_level, "trunks")
  expect_true(grepl("2008", sys$edition) && nzchar(sys$display$native))
  for (a in c("kidpr", "russia", "ru2008")) expect_identical(.system_resolve(a), "kidpr2008")
  expect_false("kidpr2008" %in% .system_ids(default_only = TRUE))
  # identify_horizons() now reaches the Russian identification
  f <- identify_horizons(make_kidpr2008_example("CN"), "kidpr2008")
  expect_identical(f$code, c("AY", "C"))
  # classify_all() runs it only when asked
  p <- make_kidpr2008_example("CI")
  expect_false("kidpr2008" %in% names(classify_all(p, on_missing = "silent")))
  res <- classify_all(p, systems = "kidpr2008", on_missing = "silent")
  expect_identical(res$kidpr2008$levels$code, c("C", "CI"))
})

test_that("the rules are clean: predicates, suffixes, codes, names and paths", {
  skip_on_cran()
  v <- .validate_rules("kidpr2008")
  expect_length(v$missing, 0L)
  expect_length(v$mixed, 0L)
  expect_length(v$unsuffixed, 0L)
  expect_length(v$malformed, 0L)
  expect_length(v$codes, 0L)
  expect_true(v$n >= 15L)
  r <- load_rules("kidpr2008")
  code <- function(l) vapply(l, `[[`, "", "code")
  expect_identical(code(r$trunks), c("E", "D", "A", "B", "C"))
  expect_identical(names(r$divisions), c("A", "B", "C", "D"))
  expect_identical(code(r$divisions$A), c("AB", "AA"))
  expect_identical(code(r$divisions$B), c("BA", "BB", "BC"))
  expect_identical(code(r$divisions$D), "[DA]")
  expect_identical(code(r$divisions$C), c("CP", "CL", "CU", "CQ", "CO", "CN", "CT", "CR", "CS", "CC", "CF",
                                          "CG", "CH", "CE", "CJ", "CK", "CB", "CA", "CD", "CI", "CM"))
  all <- c(r$trunks, unlist(unname(r$divisions), recursive = FALSE))
  expect_length(all, 5L + 27L)
  for (e in all) {
    expect_true(nzchar(e$name) && nzchar(e$name_native) && nzchar(e$path) && nzchar(e$notes), info = e$code)
    # the Russian name in Cyrillic, the gloss in English
    expect_true(grepl("[\u0400-\u04ff]", e$name_native), info = e$code)
    expect_false(grepl("[\u0400-\u04ff]", e$name), info = e$code)
  }
  # the names of the chapter 4 headings, the forms of the index and the key
  # as synonyms (Q12)
  bc <- r$divisions$B[[3L]]
  expect_identical(bc$name_native, "\u0421\u0442\u0440\u0430\u0442\u043e\u0437\u0435\u043c\u044b")
  expect_true("\u0421\u0442\u0440\u0430\u0442\u0438\u0444\u0438\u0446\u0438\u0440\u043e\u0432\u0430\u043d\u043d\u044b\u0435" %in% bc$synonyms_native)
  ck <- r$divisions$C[[16L]]
  expect_true(any(grepl("\u0442\u0435\u043a\u0441\u0442\u0443\u0440\u043d\u043e", ck$synonyms_native)))
  # trunk B takes only what one of its divisions takes (Q21): its gate is
  # the union of theirs
  b <- r$trunks[[4L]]
  expect_identical(b$tests$any_of, lapply(r$divisions$B, `[[`, "tests"))
  # the readings, each a decision or a reading in soilKey's words
  rd <- yaml::read_yaml(system.file("rules", "kidpr2008", "readings.yaml", package = "soilKey"))$readings
  expect_true(length(rd) >= 10L)
  # each a decision of the owner, or a reading of soilKey's for him to confirm
  for (x in rd) expect_true(nzchar(x$topic) && grepl("\\((Hugo|soilKey's reading, for Hugo to confirm), 2026-11-02\\)\\.$",
                                                     x$reading), info = x$topic)
  expect_true(any(grepl("for Hugo to confirm", vapply(rd, `[[`, "", "reading"))))
})


# --- one profile per division ----------------------------------------------------------

test_that("each example keys to its division, decided, in the Russian notation", {
  skip_on_cran()
  r <- load_rules("kidpr2008")
  names_of <- unlist(lapply(unname(r$divisions), function(l)
    stats::setNames(vapply(l, `[[`, "", "name"), vapply(l, `[[`, "", "code"))))
  for (d in .ru_divisions) {
    code <- if (d == "DA") "[DA]" else d
    p <- make_kidpr2008_example(d)
    expect_true(soilKey:::.kidpr_designations(p$horizons$designation, p$site)$kidpr, info = d)
    res <- .ru_class(p)
    expect_identical(res$system_id, "kidpr2008")
    expect_identical(res$levels$code[2L], code, info = d)
    expect_identical(res$levels$code[1L], substr(d, 1L, 1L), info = d)
    expect_identical(res$name, names_of[[code]], info = d)
    expect_true(all(nzchar(res$levels$name_native)), info = d)
    expect_length(res$provisional_because, 0L)
    expect_false(any(grepl("not provided|not a soil", res$warnings)), info = d)
  }
  # the view of the profile is dropped after the classification
  expect_null(soilKey:::.kidpr_dv_state$view)
})

test_that("trunk E, a thin layer of loose rock and a combination not provided", {
  skip_on_cran()
  # no soil horizon within about 1 m: a rock outcrop or a technogenic formation
  for (tk in list(c("C:60", "R:40"), c("\u0422\u041f\u041e:80", "C:40"))) {
    r <- .ru_class(.ru_ped(tk))
    expect_identical(.ru_label(r), "E/-")
    expect_true(any(grepl("not a soil", r$warnings)))
  }
  # a deposit over 40 cm, not humified, over a buried soil (2.2, step 4b)
  r <- .ru_class(.ru_ped(c("C~~:50", "[AU]:20", "[BM]:30", "C:30")))
  expect_identical(.ru_label(r), "E/-")
  # 40 cm or less: the buried soil under it is the soil keyed
  r <- .ru_class(.ru_ped(c("C~~:30", "[AU]:20", "[BM]:30", "C:30")))
  expect_identical(.ru_label(r), "C/CD")
  # hard rock within 30 cm and none of the horizons of C.2
  r <- .ru_class(.ru_ped(c("EL:10", "R:20")))
  expect_identical(.ru_label(r), "C/-")
  expect_true(any(grepl("thin layer of loose rock", r$warnings)))
  # a textural horizon with no eluvial horizon above it (C.9b)
  r <- .ru_class(.ru_ped(c("AY:15", "BT:40", "C:30")))
  expect_identical(.ru_label(r), "C/-")
  expect_true(any(grepl("not provided by the Field Guide of 2008", r$warnings)))
  expect_length(r$provisional_because, 0L)
})


# --- the repaired cases --------------------------------------------------------------------

test_that("the repairs of Q9, Q10, Q20 and Q21 and soilKey's readings key as decided", {
  skip_on_cran()
  # tokens, floodplain, the division by the decisions, the literal key
  cases <- list(
    list(c("P:25", "G:30", "CRM:30", "C:30"), FALSE, "C/CH", "C/CS"),        # Q9: C.6b over C.6a (CH06)
    list(c("P:25", "BFM:40", "C:30"), FALSE, "C/CS", "C/CC"),                # Q9: P on BFM, an agrozem
    list(c("P:25", "BMK:30", "CAT:30", "C:30"), FALSE, "C/CS", "C/CJ"),      # Q9: P on BMK, an agrozem
    list(c("PU:30", "AU:20", "C:40"), FALSE, "C/CN", "C/CS"),                # Q9: C.19v before C.19a (CN09)
    list(c("P:25", "C~~:60"), TRUE, "C/CS", "B/BA"),                         # Q9: no remnant on alluvium
    list(c("AY:10", "C:10", "R:20"), FALSE, "C/CP", "C/-"),                   # Q10: C.2 + AY (CP05)
    list(c("AK:10", "R:30"), FALSE, "C/CP", "C/-"),                           # Q10: C.2 + AK (CP08)
    list(c("PT:25", "C:40"), FALSE, "C/CS", "C/-"),                           # Q10: C.19a + PT (CS05)
    list(c("AK:10", "C:40"), FALSE, "C/CN", "C/-"),                           # Q10: C.19v + AK (CN05)
    list(c("P:25", "TUR:50", "C:30"), FALSE, "C/CT", "C/-"),                  # Q10: TUR > 40 at C.6 (CT01)
    list(c("TUR:50", "BCA:30", "C:30"), FALSE, "C/CT", "C/-"),                # Q10: TUR at the surface (CT14)
    list(c("O:5", "W:4", "C:50"), FALSE, "D/[DA]", "C/-"),                    # Q10: O with W
    list(c("S:10", "C:50"), FALSE, "D/[DA]", "C/CL"),                         # Q10: S alone over C (DA11)
    list(c("AY:15", "G:30", "CG:30"), FALSE, "C/CH", "C/-"),                  # Q10: AY over G
    list(c("P:25", "TUR:20", "BCA:30", "C:30"), FALSE, "C/CS", "C/CT"),       # Q20: thin TUR (CS16-CS19)
    list(c("AY:15", "BM:40", "C~~:40"), FALSE, "C/CD", "B/-"),                # Q21: other layering stays in C
    list(c("RY:30", "D:40"), FALSE, "C/-", "B/BC"),                           # Q21: R* under 40 cm
    list(c("AU:30", "V:40", "C:30"), FALSE, "C/CI", "C/-"),                   # reading: V at C.5 (CI04)
    list(c("AU:30", "CRH:30", "C:30"), FALSE, "C/CI", "C/-"),                 # reading: CRH at C.5 (CI05)
    list(c("PU:30", "ASN:20", "BCA:30", "C:30"), FALSE, "C/CK", "C/-")        # reading: C.6 no exit (CK09)
  )
  for (k in cases) {
    site <- if (k[[2L]]) list(landform = "floodplain") else list()
    r <- .ru_class(.ru_ped(k[[1L]], site))
    info <- paste(k[[1L]], collapse = " ")
    expect_identical(.ru_label(r), k[[3L]], info = info)
    pr <- .kor_profile(k[[1L]], floodplain = k[[2L]])
    expect_identical(.kor_label(.kor_walk(pr, FALSE)), k[[4L]], info = info)
    rep <- .kor_walk(pr, TRUE)
    expect_identical(.kor_label(rep), k[[3L]], info = info)
    expect_true(length(rep$used) > 0L, info = info)
  }
})

test_that("the readings shared by both walks: surface, truncation, peat, chemozems", {
  skip_on_cran()
  # Q11: the thickest natural horizon is the surface one (O becomes a feature)
  expect_identical(.ru_label(.ru_class(.ru_ped(c("O:3", "AY:15", "BM:40", "C:30")))), "C/CD")
  expect_identical(.ru_label(.ru_class(.ru_ped(c("O:3", "AY:15", "C:40")))), "C/CN")
  # Q19: a middle horizon at the surface is an abrazem when the description
  # starts at 0 cm, undecided otherwise
  expect_identical(.ru_label(.ru_class(.ru_ped(c("BM:30", "C:40")))), "C/CQ")
  r <- .ru_class(.ru_ped(c("BM:30", "C:40"), top = 10))
  expect_identical(.ru_label(r), "C/-")
  expect_true("divisions:CQ" %in% r$provisional_because)
  # Q22: the agro-peat counts in the peat body; a description that ends in
  # peat before 51 cm leaves trunk A undecided
  expect_identical(.ru_label(.ru_class(.ru_ped(c("PT:25", "TT:40")))), "A/AB")
  expect_identical(.ru_label(.ru_class(.ru_ped(c("TE:30", "TT:30", "G:20")))), "A/AA")
  r <- .ru_class(.ru_ped(c("T:40")))
  expect_identical(r$levels$code[1L], "C")
  expect_true("trunks:A" %in% r$provisional_because)
  # Q23: the chemozems after the lithozems, only in trunk C; X from the
  # extremely hazardous category within 30 cm
  hz <- c("extremely hazardous", NA, NA, NA)
  expect_identical(.ru_label(.ru_class(.ru_ped(c("AY:15", "EL:15", "BT:40", "C:30"),
                                               contamination_hazard = hz))), "C/CU")
  expect_identical(.ru_label(.ru_class(.ru_ped(c("AY+X:10", "C:10", "R:20")))), "C/CP")
  expect_identical(.ru_label(.ru_class(.ru_ped(c("TO+X:40", "TT:60")))), "A/AA")
  r <- .ru_class(.ru_ped(c("AY:15", "EL:15", "BT:40", "C:30"),
                         contamination_hazard = c("moderately hazardous", NA, NA, NA)))
  expect_identical(.ru_label(r), "C/CA")
  # contamination without its category leaves X undecided
  r <- .ru_class(.ru_ped(c("AY:15", "EL:15", "BT:40", "C:30"), site = list(contaminated = TRUE)))
  expect_identical(.ru_label(r), "C/CA")
  expect_true("divisions:CU" %in% r$provisional_because)
})

test_that("trunk B: a floodplain on alluvium, volcanic ash, a stratified body", {
  skip_on_cran()
  fp <- list(landform = "floodplain")
  expect_identical(.ru_label(.ru_class(.ru_ped(c("AU:30", "G:30", "CG~~:40"), fp))), "B/BA")
  expect_identical(.ru_label(.ru_class(.ru_ped(c("P:25", "AY:15", "C~~:40"), fp))), "B/BA")
  # the floodplain recorded, the origin of the layers instead of the mark
  expect_identical(.ru_label(.ru_class(.ru_ped(c("AY:15", "C:40"), fp, layer_origin = c(NA, "alluvium")))), "B/BA")
  # a terrace is no floodplain; marine material is no alluvium
  expect_identical(.ru_label(.ru_class(.ru_ped(c("AY:15", "C~~:40"), list(landform = "river terrace")))), "C/CN")
  expect_identical(.ru_label(.ru_class(.ru_ped(c("AY:15", "C:40"), fp, layer_origin = c(NA, "marine")))), "C/CN")
  # alluvial material with no landform recorded: undecided
  r <- .ru_class(.ru_ped(c("AY:15", "C~~:40")))
  expect_identical(.ru_label(r), "C/CN")
  expect_true("trunks:B" %in% r$provisional_because)
  # BAN in volcanic ash; BAN with no ash recorded is not keyed in B
  expect_identical(.ru_label(.ru_class(.ru_ped(c("H:15", "BAN:40", "C\":30")))), "B/BB")
  expect_identical(.ru_label(.ru_class(.ru_ped(c("H:15", "BAN:40", "C:30"),
                                               list(parent_material = "volcanic ash")))), "B/BB")
  expect_identical(.ru_label(.ru_class(.ru_ped(c("H:15", "BAN:40", "C:30")))), "C/-")
  # stratozems: the agro horizon counted with the stratified horizon; a
  # buried soil under the deposit
  expect_identical(.ru_label(.ru_class(.ru_ped(c("P:25", "RY:25", "D:40")))), "B/BC")
  expect_identical(.ru_label(.ru_class(.ru_ped(c("RU:50", "[AU]:20", "[BM]:30")))), "B/BC")
  expect_identical(.ru_label(.ru_class(.ru_ped(c("RY:30", "C~~:20", "[AU]:20", "[BM]:30")))), "B/BC")
})

test_that("an undecided layer leaves the division undecided between its candidates", {
  skip_on_cran()
  # AY over EL over a layer the data leave between BT and BM
  fr <- data.frame(top_cm = c(0, 15, 30, 80), bottom_cm = c(15, 30, 80, 110),
                   code = c("AY", "EL", NA, "C"), passed = c(TRUE, TRUE, NA, TRUE),
                   stringsAsFactors = FALSE)
  fr$candidates <- list(character(0), character(0), c("BT", "BM"), character(0))
  fr$missing <- list(character(0), character(0), "clay_films_amount", character(0))
  v <- .kidpr_dv(fr)
  r <- load_rules("kidpr2008")
  t <- .run_level(v, r$trunks, "trunks")
  expect_identical(t$assigned$code, "C")
  d <- .run_level(v, r$divisions$C, "divisions")
  expect_null(d$assigned)
  expect_true(all(c("CA", "CD") %in% d$undecided))
  expect_true("clay_films_amount" %in% d$trace$CA$missing)
  # decided as BT, the same view keys to CA
  fr$code[3L] <- "BT"; fr$passed[3L] <- TRUE; fr$candidates[[3L]] <- character(0)
  expect_identical(.run_level(.kidpr_dv(fr), r$divisions$C, "divisions")$assigned$code, "CA")
})


# --- the oracle: a literal walker of keys 2.2 and 2.3 -----------------------------------------

# The horizon-presence space: every surface (organic, humus, agro, their
# pairs, a thin O over AY, a TUR), with or without an eluvial horizon, every
# middle horizon alone or none, with or without a gley horizon under it, over
# loose rock; and families for the rock, the salts and the contamination, the
# pairs of middle horizons, a gley horizon above CRM, and the trunks.
.ru_space <- function() {
  surf <- list(character(0), "O:5", "W:3", "AY:15", "AU:30", "AJ:15", "AO:8", "AK:10", "AH:20", "T:30", "TJ:20",
               "H:20", "P:25", "PU:30", "PT:25", "PTR:25", "PB:25", c("P:25", "AY:10"), c("PU:30", "AU:15"),
               c("PU:30", "H:10"), c("PT:25", "T:15"), c("O:3", "AY:15"), "TUR:50", c("P:25", "TUR:20"),
               c("P:25", "TUR:50"))
  mids <- c("BT", "BI", "BHF", "BFM", "BM", "BMK", "BPL", "CRM", "CR", "CRH", "BCA", "CAT", "ASN", "BSN", "V", "BAN", "F")
  out <- list()
  add <- function(tk, fp = FALSE) out[[length(out) + 1L]] <<- list(tokens = tk, floodplain = fp)
  for (s in surf) for (e in list(character(0), "EL:15")) for (m in c(list(character(0)), as.list(paste0(mids, ":40"))))
    for (h in list(character(0), "G:30")) add(c(s, e, m, h, "C:30"))
  # the other eluvial horizons and Q
  for (s in surf[c(1, 2, 4, 5, 6, 13, 14, 17)]) for (e in c("E:10", "BEL:15", "SEL:8"))
    for (m in list(character(0), "BT:40", "BHF:40", "BM:40", "ASN:40", "BCA:40")) for (h in list(character(0), "Q:30"))
      add(c(s, e, m, h, "C:30"))
  # pairs of middle horizons, a gley horizon above CRM or another middle one
  pairs <- list(c("BHF", "BT"), c("BT", "BCA"), c("BM", "BCA"), c("BI", "BCA"), c("ASN", "BCA"), c("BFM", "BT"),
                c("BMK", "BM"), c("BPL", "BCA"), c("CRM", "BT"), c("BM", "CRM"))
  for (s in surf[c(2, 4, 5, 6, 13, 14, 17, 22)]) for (pp in pairs) {
    add(c(s, paste0(pp, ":30"), "C:30"))
    add(c(s, "G:20", paste0(pp, ":30"), "C:30"))
  }
  # hard rock within 30 cm, or deeper
  for (s in list(character(0), "O:5", "W:3", "AY:10", "AU:15", "AJ:10", "AO:8", "AK:10", "AH:15", "T:15",
                 "TJ:15", "H:15", "P:20", "PU:20", "PT:20", "PTR:20", "PB:20", "EL:10", c("P:15", "AY:5")))
    for (r in list(c("C:5", "R:20"), c("C:40", "R:30"))) add(c(s, r))
  # salts and contamination
  for (s in list("S:10", "SS:10", "AY+S:15", "AU+S:30", "X:10", "AY+X:15", "TO+X:40"))
    for (rest in list("C:40", c("G:30", "C:30"), c("EL:15", "BT:40", "C:30"), c("C:10", "R:20"), "TT:60"))
      add(c(s, rest))
  # the trunks: no soil, deposits, weakly developed, peat, floodplains, ash,
  # stratified bodies
  fam <- list(c("C:60", "R:40"), c("TPO:80", "C:40"), c("C~~:50", "[AU]:20", "[BM]:30"),
              c("C~~:30", "[AU]:20", "[BM]:30", "C:30"), c("r:10", "AY:15", "BM:40", "C:30"),
              c("rh:10", "AU:30", "BCA:40", "C:30"), c("O:5", "C=:50"), c("W:3", "C..:50"), c("O:5", "W:4", "C:50"),
              c("W:3", "O:2", "C:50"), c("W:3", "R:30"), c("S:10", "C:50"), c("S:10", "[AU]:20", "[BM]:30"),
              c("TO:40", "TT:60"), c("TE:30", "TT:60", "G:20"), c("TJ:30", "TT:40", "R:30"), c("PT:25", "TT:40"),
              c("PTR:25", "TT:40", "G:20"), c("T:30", "C:40"), c("RY:50", "D:40"), c("RU:60", "[AU]:20", "[BM]:30"),
              c("RY:30", "D:40"), c("P:25", "RY:25", "D:40"), c("PU:30", "RU:20", "[AU]:20"),
              c("RY:30", "C~~:20", "[AU]:20", "[BM]:30"), c("AO:6", "BHF:10", "BAN:40", "C\":30"),
              c("H:15", "BAN:40", "C:30"), c("P:25", "BAN:40", "C\":30"), c("AY:15", "BM:40", "C~~:40"),
              c("AY:15", "BM:40", "C^^:40"), c("AY:15", "EL:15", "BT:40", "D\"\":40"))
  for (tk in fam) add(tk)
  alluvial <- list(c("AY:15", "C~~:40"), c("AU:30", "G:30", "CG~~:40"), c("AU:30", "Q:30", "C~~:40"),
                   c("H:20", "G:30", "C~~:40"), c("T:30", "G:30", "C~~:40"), c("P:25", "AY:15", "C~~:40"),
                   c("PU:30", "AU:15", "G:20", "C~~:40"), c("PTR:25", "T:15", "G:30", "C~~:40"),
                   c("P:25", "C~~:60"), c("PU:30", "G:30", "C~~:40"), c("AY:15", "BM:40", "C~~:40"),
                   c("AU:30", "V:40", "C~~:40"), c("AY:15", "C:40"))
  for (tk in alluvial) { add(tk, TRUE); add(tk, FALSE) }
  out
}

test_that("the flattened key agrees with the walker of the book's keys, repairs applied", {
  skip_on_cran()
  rules <- load_rules("kidpr2008")
  sp <- .ru_space()
  expect_true(length(sp) > 2000L)
  res <- lapply(sp, function(x) {
    pr <- .kor_profile(x$tokens, x$floodplain)
    l <- .kor_walk(pr, FALSE); r <- .kor_walk(pr, TRUE); f <- .kor_flat(pr, rules)
    list(p = paste(c(x$tokens, if (x$floodplain) "(floodplain)"), collapse = " "), lit = .kor_label(l),
         rep = .kor_label(r), flat = .kor_label(f), undecided = f$undecided, used = r$used)
  })
  lab <- function(k) vapply(res, `[[`, "", k)
  # the flattened lists give what the walker gives, on every combination
  bad <- which(lab("flat") != lab("rep"))
  expect_length(bad, 0L)
  if (length(bad)) print(head(data.frame(p = lab("p"), flat = lab("flat"), rep = lab("rep"))[bad, ], 20))
  # undecided only where the floodplain is: the alluvial mark with no
  # landform recorded, or a floodplain with no alluvial material recorded
  # (Q21); the walker reads both as no floodplain
  und <- vapply(res, `[[`, NA, "undecided")
  expect_true(all(xor(grepl("~~", lab("p")[und]), grepl("floodplain", lab("p")[und]))))
  expect_true(any(und))
  # every difference from the literal key is one of the repairs or readings
  diff <- lab("lit") != lab("rep")
  used <- lapply(res, `[[`, "used")
  expect_true(all(lengths(used[diff]) > 0L))
  expected <- c("Q9: C.6b over C.6a with BFM, BPL, CRM, CR, CRH or BMK",
                "Q9: an agro horizon on BFM or BMK is an agrozem",
                "Q9: C.19v before C.19a",
                "Q9: an agro horizon with no natural remnant on alluvium is an agrozem",
                "Q10: C.2 takes AY and AK",
                "Q10: C.19a takes PT and PTR",
                "Q10: C.19v takes AK",
                "Q10: a TUR over 40 cm is tested at C.6",
                "Q10: a TUR at the surface counts at C.4",
                "Q10: trunk D takes O with W",
                "Q10: trunk D takes S alone over C",
                "Q10: AY or AO directly over G is a gley soil",
                "Q20: a TUR of 40 cm or less goes on to C.16",
                "Q21: step 7 only for what a division of trunk B takes",
                "reading: V and CRH are middle horizons at C.5",
                "reading: C.6 with no exit goes on to C.7")
  seen <- unique(unlist(used[diff]))
  expect_setequal(seen, expected)
  # the space reaches every trunk and division
  rep <- unique(lab("rep"))
  expect_true(all(c("E/-", "D/[DA]", "A/AA", "A/AB", "B/BA", "B/BB", "B/BC",
                    paste0("C/", setdiff(.ru_divisions, c("AA", "AB", "BA", "BB", "BC", "DA")))) %in% rep))
})

test_that("every profile formula of chapter 4 reaches its own division", {
  skip_on_cran()
  path <- testthat::test_path("fixtures", "kidpr2008-formulas.tsv")
  fx <- utils::read.delim(path, comment.char = "#", quote = "", stringsAsFactors = FALSE,
                          encoding = "UTF-8", colClasses = "character")
  fx <- fx[startsWith(fx$source, "ch4 "), ]
  expect_identical(nrow(fx), 275L)
  type <- sub("^ch4 ", "", fx$source)
  div <- ifelse(substr(type, 1L, 2L) == "DA", "[DA]", substr(type, 1L, 2L))
  rules <- load_rules("kidpr2008")
  off <- character(0)
  for (i in seq_along(type)) {
    items <- .kidpr_split_outside(sub(" \\(.*\\)$", "", fx$formula[i]), "-")
    variants <- if (any(grepl("^\\(.*\\)$", items))) c(TRUE, FALSE) else TRUE
    for (opt in variants) {
      tk <- .kor_formula_tokens(fx$formula[i], substr(type[i], 1L, 2L), optional = opt)
      pr <- .kor_profile(tk, floodplain = startsWith(type[i], "BA"))
      f <- .kor_flat(pr, rules)
      expect_identical(.kor_label(f), .kor_label(.kor_walk(pr, TRUE)), info = type[i])
      if (opt) expect_identical(f$division, div[i], info = paste(type[i], fx$formula[i]))
      else if (!identical(f$division, div[i])) off <- c(off, sprintf("%s:%s", type[i], f$division))
    }
  }
  # left without its optional EL, CA24 (P-(EL)-BT-BCA-Cca) is P directly over
  # BT, an agrozem by C.6a (the type CS07)
  expect_identical(off, "CA24:CS")
})


# --- the app and the translations -------------------------------------------------------------

test_that("the level names and the system have their translations", {
  skip_on_cran()
  tr <- yaml::read_yaml(system.file("i18n", "translations.yaml", package = "soilKey"))
  rp <- yaml::read_yaml(system.file("i18n", "report_translations.yaml", package = "soilKey"))
  for (lang in c("en", "pt")) {
    for (k in c("classify.sys_kidpr2008", "classify.edition_kidpr2008", "classify.level_trunks",
                "classify.level_divisions", "classify.level_types"))
      expect_true(nzchar(tr[[lang]][[k]] %||% ""), info = paste(lang, k))
    for (k in c("report.level_trunks", "report.level_divisions"))
      expect_true(nzchar(rp[[lang]][[k]] %||% ""), info = paste(lang, k))
  }
  expect_identical(tr$en[["classify.edition_kidpr2008"]], .system_get("kidpr2008")$edition)
  expect_identical(tr$pt[["classify.level_trunks"]], "Tronco")
  expect_identical(rp$pt[["report.level_divisions"]], "Divis\u00e3o")
})
