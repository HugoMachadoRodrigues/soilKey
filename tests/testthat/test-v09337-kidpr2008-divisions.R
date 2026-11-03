# v0.9.337: the Russian soil classification (Field Guide of 2008) in the
# registry, its trunks (key 2.2) and its 27 divisions (key 2.3), the keys
# flattened into first-match lists (inst/rules/kidpr2008/key.yaml and
# divisions/), Hugo's decisions of 2026-11-02 (Q1, Q2, Q9-Q12, Q19-Q23, and
# those after the review). Most profiles are written in the Russian notation,
# which gives the horizons; some go by the data route (FAO designations and
# the data key 2.1 reads). The oracle (helper-kidpr-oracle.R) is a literal
# walker of the two keys.

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
# A pedon from designations and depths (NA allowed).
.ru_mk <- function(des, top, bottom, site = list(), ...) {
  h <- data.frame(designation = des, top_cm = top, bottom_cm = bottom, stringsAsFactors = FALSE)
  extra <- list(...)
  for (k in names(extra)) h[[k]] <- extra[[k]]
  PedonRecord$new(site = utils::modifyList(list(id = "ru-na", designation_system = "kidpr"), site),
                  horizons = ensure_horizon_schema(data.table::as.data.table(h)))
}
.ru_class <- function(p) classify_kidpr2008(p, on_missing = "silent", subtypes = FALSE)
.ru_label <- function(r) {
  lv <- r$levels
  paste0(if (nrow(lv)) lv$code[1L] else NA, "/", if (nrow(lv) > 1L) lv$code[2L] else "-")
}
.ru_divisions <- c("AA", "AB", "BA", "BB", "BC", "DA", "CA", "CB", "CC", "CD", "CE", "CF", "CG", "CH",
                   "CI", "CJ", "CK", "CL", "CM", "CN", "CO", "CP", "CQ", "CR", "CS", "CT", "CU")

# The data route: a layer of data (FAO designations, no Russian notation),
# and a pedon of such layers.
.ru_L <- function(top, bottom, ...) {
  d <- list(top_cm = top, bottom_cm = bottom, munsell_hue_dry = "10YR", munsell_value_dry = 6,
            munsell_chroma_dry = 4, structure_type = "massive", structure_grade = "structureless",
            clay_pct = 20, silt_pct = 40, sand_pct = 40, oc_pct = 0.1, ph_h2o = 6.0,
            clay_films_amount = "none", effervescence_class = "none", secondary_carbonates_form = "none")
  a <- list(...); d[names(a)] <- a; d
}
.ru_P <- function(..., site = list()) {
  s <- list(id = "data", frost_regime = "none", land_use = "forest")
  s[names(site)] <- site
  ls <- list(...)
  nm <- unique(unlist(lapply(ls, names)))
  h <- as.data.frame(lapply(stats::setNames(nm, nm), function(k)
    unlist(lapply(ls, function(l) if (is.null(l[[k]])) NA else l[[k]]))), stringsAsFactors = FALSE)
  old <- options(soilKey.warn_unrecognised = FALSE); on.exit(options(old))
  PedonRecord$new(site = s, horizons = h)
}
.ru_AY <- function(top = 0, bottom = 15, ...) .ru_L(top, bottom, munsell_value_dry = 5, munsell_chroma_dry = 2,
  structure_type = "granular", structure_grade = "moderate", ph_h2o = 5.0, oc_pct = 2, clay_pct = 15,
  silt_pct = 50, sand_pct = 35, ...)
.ru_B <- function(top, bottom, ...) .ru_L(top, bottom, munsell_hue_dry = "7.5YR", munsell_value_dry = 5,
  munsell_chroma_dry = 6, structure_type = "blocky", structure_grade = "moderate", clay_pct = 25, ...)
.ru_org <- function(top, bottom, ...) .ru_L(top, bottom, munsell_hue_dry = "7.5YR", munsell_value_dry = 3,
  munsell_chroma_dry = 3, plant_residues_pct = 80, peat_decomposition_pct = 30, oc_pct = 40, ...)
.ru_sand <- function(top, bottom, ...) .ru_L(top, bottom, clay_pct = 3, silt_pct = 5, sand_pct = 92, ...)
.ru_moist <- function(top, bottom, hue, v, c, ...) .ru_L(top, bottom, munsell_hue_moist = hue,
  munsell_value_moist = v, munsell_chroma_moist = c, munsell_hue_dry = NA, munsell_value_dry = NA,
  munsell_chroma_dry = NA, ...)
# A chernozem of the data route: Ah, a middle layer, Bk, Ck.
.ru_chernozem <- function(mid = NULL) {
  ah_bot <- if (is.null(mid)) 50 else mid$top_cm
  ls <- list(.ru_AY(0, ah_bot, designation = "Ah", munsell_value_dry = 3, munsell_chroma_dry = 1, ph_h2o = 7.2,
                    oc_pct = 4, clay_pct = 35, silt_pct = 45, sand_pct = 20))
  if (!is.null(mid)) ls <- c(ls, list(mid))
  b_top <- if (is.null(mid)) 50 else mid$bottom_cm
  ls <- c(ls, list(.ru_L(b_top, 120, designation = "Bk", munsell_value_dry = 7, munsell_chroma_dry = 3,
                         structure_type = "prismatic", structure_grade = "weak", caco3_pct = 15,
                         secondary_carbonates_form = "pseudomycelia", ph_h2o = 8.2, effervescence_class = "strong"),
                   .ru_L(120, 160, designation = "Ck", caco3_pct = 10, ph_h2o = 8.3, effervescence_class = "strong")))
  do.call(.ru_P, c(ls, list(site = list(land_use = "grassland"))))
}


# --- the registry and the rules ------------------------------------------------------

test_that("the registry has the Russian system, experimental and opt-in", {
  skip_on_cran()
  s <- soilkey_systems()
  r <- s[s$id == "kidpr2008", ]
  expect_identical(nrow(r), 1L)
  expect_identical(r$classifier, "classify_kidpr2008")
  expect_identical(r$levels, "trunks, divisions, types")
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
  # v0.9.345: the type is followed by its subtypes (here the typical one)
  expect_identical(res$kidpr2008$levels$code, c("C", "CI", "[CI01]", NA))
})

test_that("the rules are clean: predicates, suffixes, codes, names and paths", {
  skip_on_cran()
  v <- .validate_rules("kidpr2008")
  expect_length(v$missing, 0L)
  expect_length(v$mixed, 0L)
  expect_length(v$unsuffixed, 0L)
  expect_length(v$malformed, 0L)
  expect_length(v$codes, 0L)
  expect_true(v$n >= 14L)
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
  # the volcanic soils need BAN only (Hugo, 2026-11-02)
  expect_identical(r$divisions$B[[2L]]$tests, list(all_of = list(list(has_horizon_kidpr = list(codes = "BAN")))))
  # every reading is a decision of the owner (Hugo, 2026-11-02); none is left
  # for him to confirm
  rd <- yaml::read_yaml(system.file("rules", "kidpr2008", "readings.yaml", package = "soilKey"))$readings
  expect_true(length(rd) >= 10L)
  for (x in rd) expect_true(nzchar(x$topic) && grepl("\\(Hugo, 2026-11-02\\)\\.$", x$reading), info = x$topic)
  expect_false(any(grepl("for Hugo to confirm", vapply(rd, `[[`, "", "reading"))))
  # no text of the book: the readings are in English, Cyrillic only for names
  # and terms
  expect_false(any(grepl("[\u0400-\u04ff]{5,} [\u0400-\u04ff]{5,} [\u0400-\u04ff]{5,}",
                         vapply(rd, `[[`, "", "reading"))))
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
    expect_identical(res$levels$name[2L], names_of[[code]], info = d)
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

test_that("the repairs and Hugo's decisions key as decided, the literal key otherwise", {
  skip_on_cran()
  # tokens, floodplain, the division by the decisions, the literal key
  cases <- list(
    list(c("P:25", "G:30", "CRM:30", "C:30"), FALSE, "C/CH", "C/CS"),        # Q9: C.6b over C.6a (CH06)
    list(c("PU:30", "AU:20", "C:40"), FALSE, "C/CN", "C/CS"),                # Q9: C.19v before C.19a (CN09)
    list(c("P:25", "C~~:60"), TRUE, "C/CS", "B/BA"),                         # Q9: no remnant on alluvium
    list(c("AY:10", "C:10", "R:20"), FALSE, "C/CP", "C/-"),                   # Q10: C.2 + AY (CP05)
    list(c("AK:10", "R:30"), FALSE, "C/CP", "C/-"),                           # Q10: C.2 + AK (CP08)
    list(c("PT:25", "C:40"), FALSE, "C/CS", "C/-"),                           # Q10: C.19a + PT (CS05)
    list(c("AK:10", "C:40"), FALSE, "C/CN", "C/-"),                           # Q10: C.19v + AK (CN05)
    list(c("P:25", "TUR:50", "C:30"), FALSE, "C/CT", "C/-"),                  # Q10: TUR > 40 at C.6 (CT01)
    list(c("TUR:50", "BCA:30", "C:30"), FALSE, "C/CT", "C/-"),                # Q10: TUR at the surface (CT14)
    list(c("S:10", "C:50"), FALSE, "D/[DA]", "C/CL"),                         # Q10: S alone over C (DA11)
    list(c("AY:15", "G:30", "CG:30"), FALSE, "C/CH", "C/-"),                  # Q10: AY over G
    list(c("P:25", "TUR:20", "BCA:30", "C:30"), FALSE, "C/CS", "C/CT"),       # Q20: thin TUR (CS16-CS19)
    list(c("AY:15", "BM:40", "C~~:40"), FALSE, "C/CD", "B/-"),                # Q21: other layering stays in C
    list(c("RY:30", "D:40"), FALSE, "C/-", "B/BC"),                           # Q21: R* under 40 cm
    list(c("AU:30", "V:40", "C:30"), FALSE, "C/CI", "C/-"),                   # reading: V at C.5 (CI04)
    list(c("AU:30", "CRH:30", "C:30"), FALSE, "C/CI", "C/-"),                 # reading: CRH at C.5 (CI05)
    list(c("PU:30", "ASN:20", "BCA:30", "C:30"), FALSE, "C/CK", "C/-"),       # reading: C.6 no exit (CK09)
    # after the review (Hugo, 2026-11-02)
    list(c("P:25", "BFM:40", "C:30"), FALSE, "C/CS", "C/CC"),                # a ploughed CC soil: an agrozem
    list(c("P:25", "AY:8", "BFM:40", "C:30"), FALSE, "C/CS", "C/CC"),        # with its remnant AY
    list(c("PB:22", "BFM:40", "C:30"), FALSE, "C/CR", "C/CC"),               # PB: an agroabrazem
    list(c("P:25", "BMK:30", "CAT:30", "C:30"), FALSE, "C/CS", "C/CJ"),      # a ploughed CJ soil
    list(c("P:25", "AJ:8", "BCA:40", "C:30"), FALSE, "C/CS", "C/-"),         # AJ-BCA ploughed
    list(c("P:25", "AJ:8", "BM:30", "BCA:40", "C:30"), FALSE, "C/CS", "C/CD"), # AJ-BM-BCA (CJ02) ploughed
    list(c("H:15", "BAN:40", "C:30"), FALSE, "B/BB", "C/-"),                  # BAN is formed in ash
    list(c("BT:40", "C:30"), FALSE, "C/CQ", "C/-")                            # an exposed BT is BI (CQ01)
  )
  for (k in cases) {
    site <- if (k[[2L]]) list(landform = "floodplain") else list()
    r <- .ru_class(.ru_ped(k[[1L]], site))
    info <- paste(k[[1L]], collapse = " ")
    expect_identical(.ru_label(r), k[[3L]], info = info)
    # decided, but the alluvial mark with no landform leaves trunk B open
    if (k[[2L]] || !any(grepl("~~", k[[1L]]))) expect_length(r$provisional_because, 0L)
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
  # peat at 50 cm or less leaves trunk A undecided
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

test_that("trunk B: a floodplain on alluvium, the ochre horizon BAN, a stratified body", {
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
  # BAN, formed in volcanic ash by its definition, makes the volcanic soils,
  # an ash recorded or not (Hugo, 2026-11-02)
  expect_identical(.ru_label(.ru_class(.ru_ped(c("H:15", "BAN:40", "C\":30")))), "B/BB")
  expect_identical(.ru_label(.ru_class(.ru_ped(c("H:15", "BAN:40", "C:30"),
                                               list(parent_material = "volcanic ash")))), "B/BB")
  r <- .ru_class(.ru_ped(c("H:15", "BAN:40", "C:30")))
  expect_identical(.ru_label(r), "B/BB")
  expect_length(r$provisional_because, 0L)
  expect_identical(.ru_label(.ru_class(.ru_ped(c("AO:6", "BAN:40", "C:30")))), "B/BB")
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


# --- after the review (Hugo, 2026-11-02) ------------------------------------------------------

test_that("the deposit of step 2 holds TPO and the deposit of key 2.1 position 38b", {
  skip_on_cran()
  # TPO over 40 cm on a soil, buried or not: not a soil (2.2, step 4b)
  for (tk in list(c("TPO:60", "[AY]:20", "[BT]:40"), c("TPO:60", "AY:20", "BT:40", "C:30"),
                  c("\u0422\u041f\u041e:50", "[AU]:20", "[BM]:30"))) {
    r <- .ru_class(.ru_ped(tk))
    expect_identical(.ru_label(r), "E/-", info = paste(tk, collapse = " "))
    expect_length(r$provisional_because, 0L)
    expect_true(any(grepl("not a soil", r$warnings)))
  }
  v <- soilKey:::.kidpr_dv_pedon(.ru_ped(c("TPO:60", "AY:20", "BT:40", "C:30")))
  expect_identical(v$dep$rows, 1L)
  expect_identical(v$dep$thick, 60)
  expect_false(v$dep$humified)
  # 40 cm or less: the soil under it is keyed, TPO over loose rock too
  expect_identical(.ru_label(.ru_class(.ru_ped(c("TPO:30", "AY:20", "BM:40", "C:30")))), "C/CD")
  expect_identical(.ru_label(.ru_class(.ru_ped(c("TPO:20", "C:30", "[AY]:20", "[BM]:30")))), "E/-")
  expect_identical(.ru_label(.ru_class(.ru_ped(c("TPO:20", "C:10", "[AY]:20", "[BM]:30")))), "C/CD")
  # the data route: 60 cm of fluvial material over a buried Ab (the key to
  # horizons reads a deposit, position 38b)
  p <- .ru_P(.ru_L(0, 60, layer_origin = "fluvic"), .ru_AY(60, 80, designation = "Ab"),
             .ru_L(80, 110, designation = "C"))
  f <- identify_horizons(p, "kidpr2008")
  expect_true(grepl("_deposit$", f$key_path[1L]))
  r <- .ru_class(p)
  expect_identical(.ru_label(r), "E/-")
  expect_length(r$provisional_because, 0L)
  # the walker agrees: TPO and the deposit of 38b belong to the deposit
  for (tk in list(c("TPO:60", "[AY]:20", "[BT]:40"), c("_dep:60", "[AY]:20", "C:30"),
                  c("_dep:30", "[AY]:20", "[BM]:30", "C:30"))) {
    pr <- .kor_profile(tk)
    expect_identical(.kor_label(.kor_flat(pr)), .kor_label(.kor_walk(pr, TRUE)), info = paste(tk, collapse = " "))
  }
  expect_identical(.kor_label(.kor_walk(.kor_profile(c("_dep:60", "[AY]:20", "C:30")), FALSE)), "E/-")
})

test_that("the peat body: layers undecided between peats count, more than 50 cm", {
  skip_on_cran()
  # peat with no botany and no decomposition recorded: each layer undecided
  # between the peats and the muck, a peat body all the same
  p <- .ru_P(.ru_L(0, 50, designation = "Oa", oc_pct = 35, munsell_value_dry = 3, munsell_chroma_dry = 1),
             .ru_L(50, 100, designation = "Oe", oc_pct = 28, munsell_value_dry = 4, munsell_chroma_dry = 2),
             .ru_L(100, 150, designation = "C", oc_pct = 0.5))
  f <- identify_horizons(p, "kidpr2008")
  expect_true(all(is.na(f$passed[1:2])))
  expect_true(all(unlist(f$candidates[1:2]) %in% soilKey:::.KIDPR_DV_PEAT))
  r <- .ru_class(p)
  expect_identical(.ru_label(r), "A/AA")
  # the trunk and the division decided (v0.9.342: the type is undecided
  # between AA02 and AA04, the peat below 50 cm being TT)
  expect_false(any(grepl("^(trunks|divisions):", r$provisional_because)))
  # step 6 does not ask what it does not need (the undecided type may)
  expect_false(any(c("peat_decomposition_pct", "von_post_index") %in% peat_body_kidpr(p)$missing))
  expect_false(any(c("peat_decomposition_pct", "von_post_index") %in%
                     collect_missing_attributes(do.call(c, unname(r$trace[c("trunks", "divisions")])))))
  expect_true(isTRUE(peat_body_kidpr(p)$passed))
  expect_identical(.ru_label(.ru_class(make_histosol_canonical())), "A/AA")
  # a layer that may be peat or not stops the body undecided
  fr <- data.frame(top_cm = c(0, 40, 70), bottom_cm = c(40, 70, 100), code = c("T", NA, "G"),
                   passed = c(TRUE, NA, TRUE), stringsAsFactors = FALSE)
  fr$candidates <- list(character(0), c("T", "G"), character(0))
  fr$missing <- list(character(0), "oc_pct", character(0))
  pb <- peat_body_kidpr(.kidpr_dv(fr))
  expect_true(is.na(pb$passed))
  expect_identical(pb$missing, "oc_pct")
  fr$candidates[[2L]] <- c("T", "TE")
  expect_true(isTRUE(peat_body_kidpr(.kidpr_dv(fr))$passed))
  # more than 50 cm, as the book prints it (51 cm or more on whole numbers)
  expect_identical(.ru_label(.ru_class(.ru_ped(c("TO:30", "TT:20.5", "G:30")))), "A/AA")
  r <- .ru_class(.ru_ped(c("TO:30", "TT:20", "G:30")))
  expect_identical(r$levels$code[1L], "C")
  expect_length(r$provisional_because, 0L)
  expect_identical(load_rules("kidpr2008")$trunks[[3L]]$tests$all_of[[2L]], list(peat_body_kidpr = list(min_cm = 50L)))
})

test_that("step 1: undecided soil horizons are soil horizons; position 66 proves nothing", {
  skip_on_cran()
  # no land use and no designations: the layers are undecided between soil
  # horizons (P or AY ...), so the soil is no trunk E
  p <- .ru_P(.ru_AY(0, 18), .ru_L(18, 35, munsell_value_dry = 7, munsell_chroma_dry = 2, structure_type = "platy",
                                   structure_grade = "weak", clay_pct = 10, silt_pct = 55, sand_pct = 35),
             .ru_B(35, 90, clay_pct = 30, structure_type = "prismatic", clay_films_amount = "common"),
             .ru_L(90, 150), site = list(land_use = NA))
  expect_true(is.na(identify_horizons(p, "kidpr2008")$passed[1L]))
  expect_true(isTRUE(soil_horizon_within_kidpr(p)$passed))
  r <- .ru_class(p)
  expect_false(any(startsWith(r$provisional_because, "trunks:")))
  # a layer with no diagnostic horizon (key 2.1, position 66) may be a
  # transition: with no other layer to decide, step 1 is undecided
  for (fx in list(make_plinthosol_canonical(), make_plintossolo_canonical())) {
    r <- .ru_class(fx)
    expect_identical(r$levels$code[1L], "C")
    expect_true("trunks:E" %in% r$provisional_because)
  }
  # rock, TPO and the deposit of position 38b decide it
  fr <- data.frame(top_cm = c(0, 30), bottom_cm = c(30, 120), code = c(NA, "R"), passed = c(FALSE, TRUE),
                   route = "data", key_path = c("66\u0430: _none", NA), stringsAsFactors = FALSE)
  expect_true(is.na(soil_horizon_within_kidpr(.kidpr_dv(fr))$passed))
  fr$key_path[1L] <- "38\u0431: _deposit"
  expect_false(soil_horizon_within_kidpr(.kidpr_dv(fr))$passed)
  fr$key_path[1L] <- "66\u0430: _none"; fr$designation <- c("C", "R")
  expect_false(soil_horizon_within_kidpr(.kidpr_dv(fr))$passed)
  # of the 44 canonical fixtures, only the two whose every layer is
  # position 66 leave trunk E undecided
  ns <- asNamespace("soilKey")
  fx <- sort(ls(ns, pattern = "^make_.*_canonical$"))
  expect_identical(length(fx), 44L)
  e_open <- fx[vapply(fx, function(f) "trunks:E" %in% .ru_class(get(f, ns)())$provisional_because, NA)]
  expect_identical(e_open, c("make_plinthosol_canonical", "make_plintossolo_canonical"))
})

test_that("transitions on both routes: FAO AB, BC or E/B with no horizon, and gaps of 15 cm", {
  skip_on_cran()
  # the notation skips its transition AB; the data route skips the FAO AB in
  # which the key to horizons finds no diagnostic horizon (Q14, one rule)
  expect_identical(.ru_label(.ru_class(.ru_ped(c("AU:50", "AB:25", "BCA:40", "Cca:30")))), "C/CI")
  p <- .ru_chernozem(.ru_L(50, 75, designation = "AB", munsell_value_dry = 4, munsell_chroma_dry = 2,
                           structure_type = "blocky", ph_h2o = 7.5, oc_pct = 1.5, clay_pct = 35))
  f <- identify_horizons(p, "kidpr2008")
  expect_true(is.na(f$code[2L]) && isFALSE(f$passed[2L]))
  v <- soilKey:::.kidpr_dv_pedon(p)
  expect_identical(v$kind[2L], "trans")
  r <- .ru_class(p)
  expect_identical(.ru_label(r), "C/CI")
  expect_length(r$provisional_because, 0L)
  expect_identical(.ru_label(.ru_class(.ru_chernozem())), "C/CI")
  # the designations read as transitions, and those that are not
  tr <- soilKey:::.kidpr_dv_fao_transition(c("AB", "BA", "BC", "CB", "AE", "EB", "ABg", "2BC", "A/B", "E/Bt",
                                             "B/C", "Bt", "BCA", "AA", "Ap", "C", NA))
  expect_identical(tr, c(rep(TRUE, 11L), rep(FALSE, 6L)))
  # the 15 cm with no diagnostic horizon between AU and BCA (p. 94), the
  # data route's layers of position 66 in the walker's frames
  for (g in c(10, 15)) {
    pr <- .kor_profile(c("AU:40", sprintf("_none:%d", g), "BCA:40", "C:30"))
    expect_identical(.kor_label(.kor_flat(pr)), "C/CI")
    expect_identical(.kor_label(.kor_walk(pr, FALSE)), "C/CI")
  }
  pr <- .kor_profile(c("AU:40", "_none:20", "BCA:40", "C:30"))
  expect_identical(.kor_label(.kor_flat(pr)), "C/-")
  pr <- .kor_profile(c("AU:40", "_none:10", "BI:40", "C:30"))
  expect_identical(.kor_label(.kor_flat(pr)), "C/-")
})

test_that("missing depths leave the steps undecided, with no R warning", {
  skip_on_cran()
  w <- character(0)
  cls <- function(p) withCallingHandlers(.ru_class(p), warning = function(x) {
    w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning")
  })
  # hard rock of unknown depth under 15 cm of AY: CP or CN
  r <- cls(.ru_mk(c("AY", "R"), c(0, NA), c(15, NA)))
  expect_identical(.ru_label(r), "C/-")
  expect_true(all(c("divisions:CP", "divisions:CN") %in% r$provisional_because))
  expect_true(is.na(rock_within_kidpr(.ru_mk(c("AY", "R"), c(0, NA), c(15, NA)))$passed))
  # a salt overlay of unknown depth below BCA: not decided CL
  r <- cls(.ru_mk(c("AY", "BCA", "BSN+S"), c(0, 20, NA), c(20, 60, NA)))
  expect_false(identical(r$levels$code[2L], "CL"))
  expect_true("divisions:CL" %in% r$provisional_because)
  # extremely hazardous pollutants in a layer of unknown depth: not decided CU
  r <- cls(.ru_mk(c("AY", "EL", "BT"), c(0, 15, NA), c(15, 30, NA),
                  contamination_hazard = c(NA, NA, "extremely hazardous")))
  expect_false(identical(r$levels$code[2L], "CU"))
  expect_true("divisions:CU" %in% r$provisional_because)
  # a W of unknown thickness over BM: the feature w, or W at the surface
  r <- cls(.ru_mk(c("W", "BM", "C"), c(0, NA, 40), c(NA, 40, 80)))
  expect_false(identical(r$levels$code[2L], "CQ"))
  expect_true("divisions:CQ" %in% r$provisional_because)
  # an O of unknown thickness over AY: the thickest is not decided
  v <- soilKey:::.kidpr_dv_pedon(.ru_mk(c("O", "AY", "BM", "C"), c(0, NA, 15, 60), c(NA, 15, 60, 90)))
  expect_length(v$var, 2L)
  expect_true(is.na(surface_horizon_kidpr(v, "AY")$passed))
  expect_true(isTRUE(has_horizon_kidpr(v, "BM")$passed))
  expect_length(w, 0L)
})

test_that("step 5 after the thickest-horizon rule; C.14 directly under; BMK out of C.5", {
  skip_on_cran()
  # O over a thinner humus horizon is only O (trunk D), over loose rock or
  # over hard rock; O with W is one of them
  for (tk in list(c("O:8", "AY:6", "C:50"), c("O:9", "AY:7", "R:20"), c("O:5", "AY:3", "C:50"),
                  c("O:5", "W:4", "C:50"), c("W:3", "O:2", "C:50"))) {
    r <- .ru_class(.ru_ped(tk))
    expect_identical(.ru_label(r), "D/[DA]", info = paste(tk, collapse = " "))
    expect_false(any(grepl("loose rock|not provided", r$warnings)))
    pr <- .kor_profile(tk)
    expect_identical(.kor_label(.kor_walk(pr, FALSE)), "D/[DA]")
  }
  # a thicker humus horizon is the surface one, the O a feature
  expect_identical(.ru_label(.ru_class(.ru_ped(c("O:6", "AY:12", "C:50")))), "C/CN")
  # C.14 reads "directly under", as C.13: the gley division text (p. 92)
  # puts G directly under the organic or humus horizon, and another mineral
  # horizon leaves the soil out of the gley soils (Hugo, 2026-11-02)
  for (tk in list(c("O:8", "BI:30", "G:40", "CG:30"), c("T:30", "BCA:30", "Q:40", "CQ:30"),
                  c("O:5", "BI:40", "C:30"))) {
    r <- .ru_class(.ru_ped(tk))
    expect_identical(.ru_label(r), "C/-", info = paste(tk, collapse = " "))
    expect_true(any(grepl("not provided by the Field Guide of 2008", r$warnings)))
    expect_length(r$provisional_because, 0L)
    expect_identical(.kor_label(.kor_walk(.kor_profile(tk), FALSE)), "C/-")
  }
  for (tk in list(c("O:8", "G:40", "CG:30"), c("T:30", "Q:40", "CQ:30"), c("PT:25", "T:15", "G:30", "CG:30"))) {
    r <- .ru_class(.ru_ped(tk))
    expect_identical(.ru_label(r), if (grepl("^Q:", tk[2L])) "C/CM" else "C/CH", info = paste(tk, collapse = " "))
    expect_identical(.kor_label(.kor_walk(.kor_profile(tk), FALSE)), .ru_label(r))
  }
  # BMK stays out of C.5 (Q10: only the gaps the types prove are repaired)
  r <- .ru_class(.ru_ped(c("AJ:15", "BMK:20", "C:30")))
  expect_identical(.ru_label(r), "C/-")
  expect_true(any(grepl("not provided by the Field Guide of 2008", r$warnings)))
  expect_length(r$provisional_because, 0L)
  expect_identical(.ru_label(.ru_class(.ru_ped(c("AJ:15", "BMK:20", "BCA:40", "C:30")))), "C/CJ")
})

test_that("every ploughed soil of CC or CJ is an agrozem (pp. 83, 99)", {
  skip_on_cran()
  cases <- list(c("P:25", "AJ:8", "BCA:40", "Cca:30") ,         # AJ-BCA (C.12a) ploughed
                c("P:25", "AJ:8", "BM:30", "BCA:40", "Cca:30"),  # AJ-BM-BCA, CJ02
                c("PU:30", "AJ:8", "CAT:30", "Cca:30"),
                c("P:25", "AY:8", "BFM:40", "C:30"), c("P:25", "BFM:40", "C:30"),
                c("PT:25", "BFM:40", "C:30"), c("P:25", "BCA:30", "BMK:30", "C:30"),
                c("P:25", "AJ:8", "BMK:20", "BCA:40", "C:30"))
  for (tk in cases) expect_identical(.ru_label(.ru_class(.ru_ped(tk))), "C/CS", info = paste(tk, collapse = " "))
  for (tk in list(c("PB:22", "BFM:40", "C:30"), c("PB:22", "BMK:20", "BM:30", "CAT:30", "Cca:30")))
    expect_identical(.ru_label(.ru_class(.ru_ped(tk))), "C/CR", info = paste(tk, collapse = " "))
  # the natural soils keep their divisions; the other ploughed soils too
  keep <- list(list(c("AY:15", "BFM:40", "C:30"), "C/CC"), list(c("AJ:15", "BM:30", "BCA:40", "Cca:30"), "C/CJ"),
               list(c("PU:30", "AU:15", "BM:30", "BCA:40", "Cca:30"), "C/CD"),   # CD13
               list(c("P:25", "BPL:40", "BCA:40", "Cca:30"), "C/CF"),             # CF04
               list(c("P:25", "G:30", "CRM:30", "C:30"), "C/CH"),                 # CH06
               list(c("P:25", "BSN:30", "BCA:40", "Cca:30"), "C/CK"),             # CK10
               list(c("P:25", "AY:8", "BM:30", "C:30"), "C/CD"),
               list(c("P:25", "AJ:8", "C:40"), "C/CS"))
  for (k in keep) expect_identical(.ru_label(.ru_class(.ru_ped(k[[1L]]))), k[[2L]], info = paste(k[[1L]], collapse = " "))
  # the walker reads it its own way: the profile under the plough, the
  # remnant (or the natural counterpart) at the surface, keys to CC or CJ
  for (tk in cases) {
    pr <- .kor_profile(tk)
    expect_true(.kor_walk_c7(.kor_natural(pr), TRUE, character(0))$division %in% c("CC", "CJ"))
    expect_identical(.kor_label(.kor_walk(pr, TRUE)), "C/CS")
  }
})

test_that("the text of the site is read as words", {
  skip_on_cran()
  fp <- function(...) floodplain_kidpr(.ru_ped(c("AY:15", "C:40"), ...))$passed
  expect_true(fp(list(landform = "floodplain"), layer_origin = c(NA, "alluvium")))
  expect_true(fp(list(landform = "flood plain of the Oka"), layer_origin = c(NA, "alluvial loam")))
  expect_true(fp(list(landform = "\u043f\u043e\u0439\u043c\u0430"), layer_origin = c(NA, "alluvium")))
  # a terrace, above the floodplain or not, is no floodplain
  expect_false(fp(list(landform = "terrace above the floodplain"), layer_origin = c(NA, "alluvium")))
  expect_false(fp(list(landform = "second river terrace"), layer_origin = c(NA, "alluvium")))
  expect_false(fp(list(landform = "\u043d\u0430\u0434\u043f\u043e\u0439\u043c\u0435\u043d\u043d\u0430\u044f \u0442\u0435\u0440\u0440\u0430\u0441\u0430"),
                  layer_origin = c(NA, "alluvium")))
  # glaciofluvial material and outwash are no alluvium
  for (o in c("fluvio-glacial sands", "fluvioglacial", "glaciofluvial", "glacial outwash", "moraine", "marine"))
    expect_false(fp(list(landform = "floodplain"), layer_origin = c(NA, o)), info = o)
  expect_identical(.ru_label(.ru_class(.ru_ped(c("AY:15", "C:40"), list(landform = "terrace above the floodplain"),
                                               layer_origin = c(NA, "alluvium")))), "C/CN")
  # a valley leaves it undecided; "flake" is no lake
  expect_true(is.na(fp(list(landform = "river valley"), layer_origin = c(NA, "alluvium"))))
  expect_false(fp(list(landform = "floodplain"), layer_origin = c(NA, "flake till")))
  # volcanic ash: outwash or washed sand is none
  ash <- function(pm) volcanic_ash_kidpr(.ru_ped(c("H:15", "BAN:40", "C:30"), list(parent_material = pm)))$passed
  expect_true(ash("volcanic ash"))
  expect_true(ash("basaltic tephra"))
  expect_true(ash("\u0432\u0443\u043b\u043a\u0430\u043d\u0438\u0447\u0435\u0441\u043a\u0438\u0439 \u043f\u0435\u043f\u0435\u043b"))
  for (pm in c("glacial outwash", "washed sand", "ashy grey loam", "cashew orchard soil")) expect_false(ash(pm), info = pm)
})

test_that("fresh litter at the top counts as 0 cm for a truncated soil (Q19)", {
  skip_on_cran()
  p <- .ru_P(.ru_L(0, 2, designation = "Oi"),
             .ru_B(2, 40, designation = "Bw", munsell_hue_dry = "10YR", munsell_chroma_dry = 4, oc_pct = 0.4),
             .ru_L(40, 100, designation = "C"))
  f <- identify_horizons(p, "kidpr2008")
  expect_true(grepl("litter", f$evidence[1L]))
  v <- soilKey:::.kidpr_dv_pedon(p)
  expect_identical(v$start, 2L)
  expect_identical(v$desc_top, 0)
  r <- .ru_class(p)
  expect_identical(.ru_label(r), "C/CQ")
  expect_length(r$provisional_because, 0L)
  # a description that starts below 0 cm stays undecided
  expect_true(is.na(starts_at_surface_kidpr(.ru_ped(c("BM:30", "C:40"), top = 2), "BM", from_ground_zero = TRUE)$passed))
})

test_that("classify_kidpr2008() needs a PedonRecord", {
  skip_on_cran()
  expect_error(classify_kidpr2008(data.frame(top_cm = 0, bottom_cm = 10)), "PedonRecord")
  expect_error(classify_kidpr2008(list(horizons = data.frame(top_cm = 0, bottom_cm = 10))), "PedonRecord")
})


# --- the data route end to end ---------------------------------------------------------------

test_that("common soils of Russia by the data route key to their divisions", {
  skip_on_cran()
  L <- .ru_L; AY <- .ru_AY; B <- .ru_B; O <- .ru_org; SA <- .ru_sand; M <- .ru_moist
  prof <- list(
    "C/CA" = .ru_P(AY(0, 18, designation = "Ah"),
                   L(18, 35, designation = "E", munsell_value_dry = 7, munsell_chroma_dry = 2, structure_type = "platy",
                     structure_grade = "weak", clay_pct = 10, silt_pct = 55, sand_pct = 35, ph_h2o = 5.0, oc_pct = 0.4),
                   B(35, 90, designation = "Bt", clay_pct = 30, silt_pct = 40, sand_pct = 30, structure_type = "prismatic",
                     clay_films_amount = "common", ph_h2o = 5.3),
                   L(90, 150, designation = "C", ph_h2o = 6)),
    "C/CJ" = .ru_P(AY(0, 15, designation = "Ah", munsell_value_dry = 6, ph_h2o = 7.8, structure_type = "crumb", structure_grade = "weak"),
                   B(15, 45, designation = "Bw", munsell_hue_dry = "5YR", munsell_value_dry = 5, munsell_chroma_dry = 6,
                     structure_type = "prismatic", structure_size = "fine", structure_type_secondary = "blocky", caco3_pct = 3, ph_h2o = 8),
                   L(45, 90, designation = "Bk", munsell_value_dry = 7, munsell_chroma_dry = 3, structure_type = "blocky", caco3_pct = 15,
                     secondary_carbonates_form = "soft concretions", ph_h2o = 8.4, effervescence_class = "strong"),
                   L(90, 140, designation = "Ck", caco3_pct = 10, ph_h2o = 8.4, effervescence_class = "strong"),
                   site = list(land_use = "grassland")),
    "C/CH" = .ru_P(O(0, 8, designation = "Oe", peat_decomposition_pct = 20),
                   M(8, 50, "5GY", 6, 1, designation = "Bg", structure_type = "massive"),
                   M(50, 100, "5GY", 5, 1, designation = "Cg"), site = list(drainage_class = "poorly drained")),
    "C/CB" = .ru_P(O(0, 8, designation = "O", peat_decomposition_pct = 20),
                   SA(8, 20, designation = "E", munsell_value_dry = 7, munsell_chroma_dry = 1, structure_type = "single grain"),
                   SA(20, 50, designation = "Bhs", munsell_hue_dry = "7.5YR", munsell_value_dry = 4, munsell_chroma_dry = 6,
                      uncoated_grains_pct = 30),
                   SA(50, 80, designation = "Bs", munsell_value_dry = 6, munsell_chroma_dry = 6, uncoated_grains_pct = 50),
                   SA(80, 120, designation = "C", uncoated_grains_pct = 95)),
    "C/CA" = .ru_P(AY(0, 20, designation = "Ah", munsell_value_dry = 4, munsell_chroma_dry = 2, ph_h2o = 5.8),
                   L(20, 32, designation = "AE", munsell_value_dry = 6, munsell_chroma_dry = 2, structure_type = "crumb",
                     leached_spots = TRUE, clay_pct = 15),
                   L(32, 45, designation = "EB", munsell_value_dry = 6, munsell_chroma_dry = 3, structure_type = "blocky",
                     structure_grade = "weak", variegated_colors = "10YR 7/2; 7.5YR 5/6", clay_pct = 20),
                   B(45, 100, designation = "Bt", clay_films_amount = "common", structure_type = "prismatic", clay_pct = 32),
                   L(100, 150, designation = "C")),
    "C/CD" = .ru_P(AY(0, 15, designation = "Ah"), B(15, 60, designation = "Bw", munsell_hue_dry = "10YR", munsell_chroma_dry = 4),
                   L(60, 120, designation = "C")),
    "B/BA" = .ru_P(AY(0, 30, designation = "Ah", munsell_value_dry = 3, munsell_chroma_dry = 1, ph_h2o = 6.8, oc_pct = 3.5,
                      layer_origin = "alluvium"),
                   L(30, 70, designation = "C1", layer_origin = "alluvium", stratification_pattern = "continuous"),
                   L(70, 120, designation = "C2", layer_origin = "alluvium"), site = list(landform = "floodplain")),
    "C/CP" = .ru_P(AY(0, 18, designation = "Ah"), L(18, 40, designation = "R", rock_origin = "granite")),
    "C/CM" = .ru_P(AY(0, 30, designation = "Ah", munsell_value_dry = 3, munsell_chroma_dry = 1, ph_h2o = 7, oc_pct = 4),
                   M(30, 70, "5Y", 5, 3, designation = "Bg"), M(70, 120, "5Y", 5, 3, designation = "Cg")),
    "C/CS" = .ru_P(AY(0, 30, designation = "Ap", munsell_value_dry = 3, munsell_chroma_dry = 1, ph_h2o = 7.2, oc_pct = 3.5,
                      boundary_distinctness = "abrupt", boundary_topography = "smooth"),
                   L(30, 90, designation = "Bk", munsell_value_dry = 7, munsell_chroma_dry = 3, structure_type = "prismatic",
                     structure_grade = "weak", caco3_pct = 15, secondary_carbonates_form = "pseudomycelia", ph_h2o = 8.2,
                     effervescence_class = "strong"),
                   L(90, 150, designation = "Ck", caco3_pct = 10, ph_h2o = 8.3, effervescence_class = "strong"),
                   site = list(land_use = "arable")),
    "C/CL" = .ru_P(L(0, 10, designation = "Az", soluble_salts_pct = 3, ph_h2o = 8.4),
                   M(10, 40, "5GY", 6, 1, designation = "Bgz", soluble_salts_pct = 1.5),
                   M(40, 80, "5GY", 5, 1, designation = "Cg"), site = list(drainage_class = "poorly drained")),
    "C/CQ" = .ru_P(B(0, 40, designation = "Bw", munsell_hue_dry = "10YR", munsell_chroma_dry = 4, oc_pct = 0.4),
                   L(40, 100, designation = "C"), site = list(anthropic_truncation = TRUE)),
    # the ochre horizon BAN by the andic stand-in, no ash recorded
    "B/BB" = .ru_P(L(0, 15, designation = "Ah", munsell_value_dry = 3, munsell_chroma_dry = 2, structure_type = "granular",
                     structure_grade = "moderate", ph_h2o = 5.4, oc_pct = 10, plant_residues_pct = 20),
                   L(15, 55, designation = "Bw", munsell_hue_dry = "7.5YR", munsell_value_dry = 6, munsell_chroma_dry = 8,
                     structure_type = "granular", structure_grade = "strong", uncoated_grains_pct = 40, al_ox_pct = 3,
                     fe_ox_pct = 2, bulk_density_g_cm3 = 0.7, phosphate_retention_pct = 90, ph_h2o = 5.6, oc_pct = 2),
                   L(55, 100, designation = "C"))
  )
  for (k in seq_along(prof)) {
    p <- prof[[k]]
    expect_true(all(identify_horizons(p, "kidpr2008")$route == "data"))
    r <- .ru_class(p)
    expect_identical(.ru_label(r), names(prof)[k], info = paste(k, names(prof)[k]))
    expect_length(r$provisional_because, 0L)
    expect_true(any(grepl("key 2.1 run on", r$warnings)))
  }
  # undecided layers: no designation and no land use leave the agro reading
  # open, and the division with it (the land use is asked)
  p <- .ru_P(AY(0, 18), L(18, 35, munsell_value_dry = 7, munsell_chroma_dry = 2, structure_type = "platy",
                            structure_grade = "weak", clay_pct = 10, silt_pct = 55, sand_pct = 35),
             B(35, 90, clay_pct = 30, silt_pct = 40, sand_pct = 30, structure_type = "prismatic", clay_films_amount = "common"),
             L(90, 150), site = list(land_use = NA))
  r <- .ru_class(p)
  expect_identical(r$levels$code[1L], "C")
  expect_true("divisions:CA" %in% r$provisional_because)
  expect_true("land_use" %in% r$missing_data)
})


# --- the oracle: a literal walker of keys 2.2 and 2.3 -----------------------------------------

# The horizon-presence space: every surface (organic, humus, agro, their
# pairs, a thin O over AY, an O over a thinner AY, a TUR), with or without an
# eluvial horizon, every middle horizon alone or none, with or without a gley
# horizon under it, over loose rock; and families for the rock, the salts and
# the contamination, the pairs of middle horizons, a gley horizon above CRM,
# the trunks, TPO and the data route's deposit, transitions and layers with
# no diagnostic horizon.
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
  # an O over a thinner humus horizon, a ploughed AJ, a peat over AY: every
  # middle horizon alone or none
  for (s in list(c("O:8", "AY:6"), c("P:25", "AJ:8"), c("T:30", "AY:10")))
    for (m in c(list(character(0)), as.list(paste0(mids, ":40")))) add(c(s, m, "C:30"))
  # the other eluvial horizons and Q
  for (s in surf[c(1, 2, 4, 5, 6, 13, 14, 17)]) for (e in c("E:10", "BEL:15", "SEL:8"))
    for (m in list(character(0), "BT:40", "BHF:40", "BM:40", "ASN:40", "BCA:40")) for (h in list(character(0), "Q:30"))
      add(c(s, e, m, h, "C:30"))
  # pairs of middle horizons, a gley horizon above CRM or another middle one
  pairs <- list(c("BHF", "BT"), c("BT", "BCA"), c("BM", "BCA"), c("BI", "BCA"), c("ASN", "BCA"), c("BFM", "BT"),
                c("BMK", "BM"), c("BPL", "BCA"), c("CRM", "BT"), c("BM", "CRM"), c("BCA", "BMK"), c("BI", "G"))
  for (s in c(surf[c(2, 4, 5, 6, 13, 14, 17, 22)], list(c("P:25", "AJ:8")))) for (pp in pairs) {
    add(c(s, paste0(pp, ":30"), "C:30"))
    add(c(s, "G:20", paste0(pp, ":30"), "C:30"))
  }
  # hard rock within 30 cm, or deeper
  for (s in list(character(0), "O:5", "W:3", "AY:10", "AU:15", "AJ:10", "AO:8", "AK:10", "AH:15", "T:15",
                 "TJ:15", "H:15", "P:20", "PU:20", "PT:20", "PTR:20", "PB:20", "EL:10", c("P:15", "AY:5"),
                 c("O:9", "AY:7"), c("O:6", "W:4")))
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
              c("AY:15", "BM:40", "C^^:40"), c("AY:15", "EL:15", "BT:40", "D\"\":40"),
              c("TO:30", "TT:20", "G:30"), c("TO:30", "TT:21", "G:30"), c("T:51", "C:30"))
  for (tk in fam) add(tk)
  alluvial <- list(c("AY:15", "C~~:40"), c("AU:30", "G:30", "CG~~:40"), c("AU:30", "Q:30", "C~~:40"),
                   c("H:20", "G:30", "C~~:40"), c("T:30", "G:30", "C~~:40"), c("P:25", "AY:15", "C~~:40"),
                   c("PU:30", "AU:15", "G:20", "C~~:40"), c("PTR:25", "T:15", "G:30", "C~~:40"),
                   c("P:25", "C~~:60"), c("PU:30", "G:30", "C~~:40"), c("AY:15", "BM:40", "C~~:40"),
                   c("AU:30", "V:40", "C~~:40"), c("AY:15", "C:40"), c("H:15", "BAN:40", "C~~:30"))
  for (tk in alluvial) { add(tk, TRUE); add(tk, FALSE) }
  # after the review: TPO over a soil and the data route's deposit (38b)
  for (tk in list(c("TPO:60", "AY:20", "BT:40", "C:30"), c("TPO:60", "[AY]:20", "[BT]:40"),
                  c("TPO:30", "AY:20", "BM:40", "C:30"), c("TPO:40", "AU:30", "BCA:40", "C:30"),
                  c("TPO:50", "[AU]:20", "[BM]:30"), c("TPO:20", "C:30", "[AY]:20", "[BM]:30"),
                  c("TPO:20", "C:10", "[AY]:20", "[BM]:30"), c("C:20", "TPO:30", "[AU]:20", "[BM]:30"),
                  c("TPO:30", "RY:20", "[AU]:20"), c("TPO:30", "O:5", "C:40"), c("_dep:60", "[AY]:20", "C:30"),
                  c("_dep:60", "AY:20", "BM:40", "C:30"), c("_dep:30", "[AY]:20", "[BM]:30", "C:30"),
                  c("RY:20", "_dep:30", "[AU]:20", "[BM]:30"), c("_dep:50", "RU:10", "[AU]:20")))
    add(tk)
  # O over a thinner humus horizon, and the reverse
  for (tk in list(c("O:8", "AY:6", "C:50"), c("O:9", "AY:7", "R:20"), c("O:10", "AY:5", "BM:40", "C:30"),
                  c("O:8", "AO:6", "BHF:20", "C:30"), c("O:12", "AU:10", "BCA:40", "C:30"), c("O:5", "AY:5", "C:40"),
                  c("O:6", "AJ:4", "BCA:40", "C:30"), c("O:9", "AY:7", "EL:15", "BT:40", "C:30"),
                  c("O:8", "AU:6", "G:30", "CG:30"), c("W:8", "O:3", "C:40"), c("O:4", "W:6", "BM:30", "C:30")))
    add(tk)
  # transitions of the notation between the surface and the middle horizons
  for (s in surf[c(4, 5, 6, 13, 14, 17, 22)]) for (m in c("BT", "BI", "BM", "BCA", "CAT", "G", "Q", "BFM", "BHF"))
    add(c(s, "AB:15", paste0(m, ":40"), "C:30"))
  for (tk in list(c("AU:40", "BC:25", "BCA:40", "C:30"), c("AY:15", "EL:15", "BEL:15", "BT:40", "BC:20", "C:30"),
                  c("AU:40", "AB:30", "BCA:40", "C:30"), c("P:25", "AB:10", "BT:40", "C:30"), c("BM:30", "BC:20", "C:30")))
    add(tk)
  # layers with no diagnostic horizon (key 2.1, position 66) between the
  # dark-humus horizon and the middle ones
  for (g in c(5, 10, 15, 20)) for (m in c("BCA", "BI", "G", "Q", "CAT")) for (s in list("AU:40", c("PU:30", "AU:15")))
    add(c(s, sprintf("_none:%d", g), paste0(m, ":40"), "C:30"))
  for (tk in list(c("AU:40", "_none:8", "_none:7", "BCA:40", "C:30"), c("AU:40", "_none:10", "_none:10", "BCA:40", "C:30"),
                  c("AY:15", "_none:10", "BM:40", "C:30"), c("P:25", "_none:10", "BT:40", "C:30")))
    add(tk)
  # FAO and WRB transitions of the data route in which the key finds no
  # horizon, at any thickness; an FAO layer that names one master is no
  # transition
  for (s in surf[c(4, 5, 6, 13, 14, 22)]) for (m in c("BT", "BI", "BCA", "G", "Q"))
    add(c(s, "~AB:25", paste0(m, ":40"), "~C:30"))
  for (tk in list(c("AU:40", "~BC:30", "BCA:40", "C:30"), c("AY:15", "EL:15", "~E/B:15", "BT:40", "C:30"),
                  c("AU:40", "~Bw:20", "BCA:40", "C:30"), c("AU:40", "~Bw:10", "BCA:40", "C:30"),
                  c("O:5", "~EB:20", "G:30", "~Cg:30"), c("AY:15", "~AB:20", "~C:60")))
    add(tk)
  out
}

test_that("the flattened key agrees with the walker of the book's keys, repairs applied", {
  skip_on_cran()
  rules <- load_rules("kidpr2008")
  sp <- .ru_space()
  expect_true(length(sp) > 2500L)
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
  # every difference from the literal key is one of the repairs or decisions
  diff <- lab("lit") != lab("rep")
  used <- lapply(res, `[[`, "used")
  expect_true(all(lengths(used[diff]) > 0L))
  expected <- c("Q9: C.6b over C.6a with BFM, BPL, CRM, CR, CRH or BMK",
                "Q9: C.19v before C.19a",
                "Q9: an agro horizon with no natural remnant on alluvium is an agrozem",
                "Q10: C.2 takes AY and AK",
                "Q10: C.19a takes PT and PTR",
                "Q10: C.19v takes AK",
                "Q10: a TUR over 40 cm is tested at C.6",
                "Q10: a TUR at the surface counts at C.4",
                "Q10: trunk D takes S alone over C",
                "Q10: AY or AO directly over G is a gley soil",
                "Q20: a TUR of 40 cm or less goes on to C.16",
                "Q21: step 7 only for what a division of trunk B takes",
                "reading: V and CRH are middle horizons at C.5",
                "reading: C.6 with no exit goes on to C.7",
                "decision: a ploughed CC or CJ soil is an agrozem",
                "decision: BAN is formed in volcanic ash",
                "Q15: an exposed BT counts as BI at C.20")
  seen <- unique(unlist(used[diff]))
  expect_setequal(seen, expected)
  # the space reaches every trunk and division
  rep <- unique(lab("rep"))
  expect_true(all(c("E/-", "D/[DA]", "A/AA", "A/AB", "B/BA", "B/BB", "B/BC",
                    paste0("C/", setdiff(.ru_divisions, c("AA", "AB", "BA", "BB", "BC", "DA")))) %in% rep))
  # end to end: a sample of the space written as PedonRecords in the Russian
  # notation (the notation reader, identify_horizons() and the view of the
  # pedon) keys as the flattened key on the walker's frames
  notation <- !grepl("_none|_dep|~", lab("p"))
  first <- which(notation & !duplicated(paste(lab("rep"), notation)))
  pick <- sort(unique(c(first, which(notation)[seq(1L, sum(notation), by = 200L)])))
  expect_true(length(pick) > 35L)
  for (i in pick) {
    e <- .kor_e2e(sp[[i]]$tokens, sp[[i]]$floodplain)
    expect_identical(.kor_label(e), lab("flat")[i], info = lab("p")[i])
    expect_identical(e$undecided, res[[i]]$undecided, info = lab("p")[i])
  }
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
    items <- strsplit(sub(" \\(.*\\)$", "", fx$formula[i]), "-(?![^\\[]*\\])", perl = TRUE)[[1L]]
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
  # end to end, with the formula's own designations (features, marks and
  # brackets kept): one type of each division
  one <- !duplicated(div)
  for (i in which(one)) {
    items <- strsplit(sub(" \\(.*\\)$", "", fx$formula[i]), "-(?![^\\[]*\\])", perl = TRUE)[[1L]]
    tk <- .kor_formula_tokens(fx$formula[i], substr(type[i], 1L, 2L))
    des <- sub("^\\((.*)\\)$", "\\1", items)
    des <- unlist(lapply(des, function(d) if (d %in% c("[ABC]", "{ABC}")) c("[AU]", "[BM]") else d))
    des <- gsub("\\((ca|s|g)\\)", "", sub("\\(RJ\\)", "", des))
    th <- as.numeric(sub("^.*:", "", tk))
    site <- if (startsWith(type[i], "BA")) list(landform = "floodplain") else list()
    if (substr(type[i], 1L, 2L) %in% c("CE", "CG")) site$frost_regime <- "permafrost"
    r <- .ru_class(.ru_ped(paste0(des, ":", th), site))
    expect_identical(r$levels$code[2L] %||% NA_character_, div[i], info = paste(type[i], fx$formula[i]))
    expect_length(r$provisional_because, 0L)
  }
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
