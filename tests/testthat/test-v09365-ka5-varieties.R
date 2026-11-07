# v0.9.365 -- the varieties and subvarieties of the German KA5 (Tab. 46-50,
# pp. 263-282; Hugo's decisions Q1, Q14, Q21): Tab. 50 read from the page
# images, the 38 codes and their criteria on the horizons, the subvariety
# digits, the symbol rules I-VII, the combinations of the subtypes, the
# overlays as varieties, the printed examples as name fixtures, and the
# levels above the variety of every KA5 example unchanged.

.vcls <- function(p) classify_ka5(p, on_missing = "silent")
.vsub <- function(r) { k <- which(r$levels$level == "subtypes"); if (length(k)) r$levels$code[k] else NA_character_ }
.vtype <- function(r) { k <- which(r$levels$level == "types"); if (length(k)) r$levels$code[k] else NA_character_ }
.vsym <- function(r) r$trace$variety$symbol %||% NA_character_
.vnamed <- function(r) r$trace$variety$named %||% character(0)
.vstat <- function(r, cd) { t <- r$trace$varieties; t$status[t$code == cd] }
.vex <- function(base, spec) soilKey:::.ka5_subtype_example(base, "notation", spec = spec, id = "test")
# a symbol compared without the digits of the subvarieties and the n of the
# Norm subtype (the printed examples of rule I write neither)
.vsym_plain <- function(s) sub("n$", "", gsub("[0-9]", "", s))

test_that("varieties.yaml: the 38 codes of Tab. 49, the criteria of Tab. 46, the rows of Tab. 50", {
  skip_on_cran()
  y <- soilKey:::.ka5_var_rules()
  cols <- unlist(y$columns)
  expect_length(cols, 38L)
  expect_false(anyDuplicated(cols) > 0L)
  codes <- vapply(y$codes, function(e) e$code, "")
  expect_identical(codes, cols)
  for (e in y$codes) {
    expect_true(e$kind %in% c("adjective", "noun"), info = e$code)
    expect_true(all(nzchar(c(e$printed, e$word, e$name_en, e$name_pt, e$page, e$what, e$reader))), info = e$code)
    expect_true(e$criterion %in% 1:22, info = e$code)
  }
  # every Tab. 46 criterion but 17 and 18 (combinations, overlays) has a code
  expect_setequal(unique(vapply(y$codes, function(e) as.integer(e$criterion), 0L)), setdiff(1:22, 17:18))
  expect_setequal(unlist(y$notes$everywhere$codes), c("og", "c", "z", "k", "v", "ku"))
  expect_setequal(unlist(y$notes$subvarieties$codes), c("eu", "m", "dy", "s", "g", "p"))
  expect_length(y$subtype_suffixes, 21L)
  # Tab. 50: a row for every printed subtype (223), on pp. 274-282, with codes
  # of the columns only
  ent <- soilKey:::.ka5_subtype_entries()
  printed <- names(ent)[!startsWith(names(ent), "[")]
  expect_length(printed, 223L)
  expect_setequal(names(y$allowed), printed)
  for (cd in names(y$allowed)) {
    a <- y$allowed[[cd]]
    expect_true(a$page >= 274 && a$page <= 282, info = cd)
    ac <- as.character(unlist(a$codes))
    expect_true(all(ac %in% cols), info = cd)
    expect_identical(ac, cols[cols %in% ac], info = cd)
  }
  # the terrestrial rows are on pp. 274-279, the wet ones on pp. 279-282
  expect_identical(y$allowed[["FFn"]]$page, 274L)
  expect_identical(y$allowed[["GG-YU"]]$page, 279L)
  expect_identical(y$allowed[["AOn"]]$page, 279L)
  expect_identical(y$allowed[["KHn"]]$page, 282L)
  expect_identical(unlist(y$type_rows$KH$codes), c("mo", "ro", "md", "mr", "u"))
  # the printed examples
  expect_length(y$examples, 28L)
})

test_that("Tab. 50: terrestrial rows as read from the page images", {
  skip_on_cran()
  a <- soilKey:::.ka5_var_rules()$allowed
  g <- function(cd) as.character(unlist(a[[cd]]$codes))
  expect_identical(g("FFn"), c("mo", "ro", "ta", "md", "mr", "eu", "m", "dy", "c", "k"))
  expect_identical(g("RNn"), c("mo", "ro", "ta", "m", "dy", "x", "p", "k", "e", "v", "ku"))
  expect_identical(g("RRn"), c("mu", "mo", "ta", "eu", "m", "s", "g", "b", "og", "x", "k", "v", "ku"))
  expect_identical(g("TTn"), c("mu", "s", "g", "a", "z", "l", "k", "e", "v", "ku"))
  expect_identical(g("BBn"), c("mu", "mo", "ro", "eu", "m", "dy", "s", "g", "a", "rt", "ei", "og", "z", "x", "p", "l", "d", "k", "e", "v", "ku"))
  expect_identical(g("LF-BB"), c("ro", "m", "dy", "s", "g", "z", "p", "k", "e", "v", "ku"))
  expect_identical(g("LFn"), c("mu", "mo", "ro", "eu", "m", "dy", "s", "g", "a", "z", "p", "k", "e", "v", "ku"))
  expect_identical(g("PPn"), c("mo", "ro", "ta", "s", "g", "a", "rt", "vf", "z", "k", "e", "v", "ku"))
  expect_identical(g("SG-PP"), c("md", "mr", "o", "u", "h", "m", "dy", "g", "r", "w", "vf", "z", "i", "k", "e", "ku"))
  expect_identical(g("CRn"), c("mu", "mo", "eu", "m", "s", "c", "e"))
  expect_identical(g("SSn"), c("mu", "mo", "ro", "mf", "md", "mr", "o", "u", "h", "eu", "m", "dy", "g", "a", "r", "w", "og", "c", "z", "i", "x", "p", "k", "e", "ku"))
  expect_identical(g("HH-SG"), c("h", "dy", "r", "ei"))
  expect_identical(g("XXu"), c("mu", "mo", "ro", "eu", "m", "dy", "s", "g", "w", "og", "c", "z", "i", "k", "e", "v", "ku"))
  expect_identical(g("YKn"), c("mu", "mo", "eu", "m", "dy", "s", "g", "c", "z", "x", "p", "l", "d", "e", "v", "ku"))
  expect_identical(g("YEn"), c("mu", "mo", "eu", "m", "dy", "s", "g", "a", "b", "gb", "gr", "z", "x", "p", "e"))
  expect_identical(g("YOn"), c("mu", "mo", "eu", "m", "s", "g", "a", "r", "c", "z", "p", "l", "d", "e"))
  # the wet rows (the map of 2026-11-04)
  expect_identical(g("GNn"), c("mf", "md", "o", "n", "u", "eu", "m", "dy", "a", "r", "og", "c", "z", "x", "k", "e", "ku"))
  expect_length(g("MRn"), 0L)
  expect_identical(g("MOn"), c("r", "ei", "sf", "ku"))
  expect_identical(g("KHn"), c("mo", "ro", "md", "mr", "dy", "g", "r", "z", "e", "v"))
})

test_that("the offer of a subtype (Q1): Tab. 50, note 1, the rows printed empty, the formula", {
  skip_on_cran()
  o <- function(cd) { t <- soilKey:::.ka5_var_offer(cd); stats::setNames(t$source, t$code) }
  b <- o("BBn")
  expect_identical(unname(b[c("ei", "p", "s")]), rep("Tab. 50", 3L))
  expect_identical(unname(b["c"]), "note 1")
  expect_true(is.na(b["ta"]))
  # the Rohmarsch rows are printed empty: nothing, not even note 1
  expect_true(all(is.na(o("MRn"))))
  # what the formula writes is no variety: the Kalkmarsch's tme, the
  # Treposol's R-Ap, the Kolluvisol transitions' M, the Auen Norms' aG
  expect_identical(unname(o("MCn")["c"]), "in the formula")
  expect_identical(unname(o("PP-YU")["v"]), "in the formula")
  expect_identical(unname(o("YK-SS")["k"]), "in the formula")
  expect_identical(unname(o("ABn")["g"]), "in the formula")
  expect_identical(unname(o("[X-YU]")["v"]), "in the formula")
  # the Erdhochmoor's type row is read for the KHn; [X-YY] takes the YYn's row
  expect_identical(unname(o("KHn")["u"]), "Tab. 50")
  expect_identical(o("[BB-YY]")[["eu"]], "Tab. 50")
})

test_that("an example profile for each of the 38 variety codes: the variety named, the subtype kept", {
  skip_on_cran()
  ent <- soilKey:::.ka5_subtype_entries()
  cols <- soilKey:::.ka5_var_columns()
  expect_setequal(names(soilKey:::.KA5_VARIETY_PROFILES), cols)
  for (cd in cols) {
    p <- make_ka5_example(variety = cd)
    expect_identical(p$site$designation_system, "ka5")
    r <- .vcls(p)
    base <- soilKey:::.ka5_variety_example_subtype(cd)
    expect_identical(.vsub(r), base, info = cd)
    expect_identical(.vtype(r), ent[[base]]$type, info = cd)
    expect_true(cd %in% .vnamed(r), info = cd)
    expect_length(r$provisional_because, 0L)
    lv <- r$levels
    w <- which(lv$level == "variety")
    expect_true(length(w) > 0L, info = cd)
    expect_true(all(w > which(lv$level == "subtypes")), info = cd)
    expect_true(startsWith(lv$code[w[1L]], cd) || any(startsWith(lv$code[w], cd)), info = cd)
    expect_true(all(lv$symbol[w] == .vsym(r)), info = cd)
    expect_true(grepl(cd, .vsym(r), fixed = TRUE), info = cd)
    expect_identical(r$name, r$trace$variety$name, info = cd)
    expect_true(any(grepl("Variety (Tab. 46, 49, 50; Q1)", r$warnings, fixed = TRUE)), info = cd)
  }
  expect_error(make_ka5_example(variety = "zz"), "variety example")
  # the names and symbols of a few
  s <- function(cd) { r <- .vcls(make_ka5_example(variety = cd)); c(.vsym(r), r$name) }
  expect_identical(s("o"), c("oGNn", "(Anmoor)Nassgley"))
  expect_identical(s("n"), c("nGNn", "(Niedermoor)Nassgley"))
  expect_identical(s("u"), c("uGNn", "(\u00dcbergangsmoor)Nassgley"))
  expect_identical(s("h"), c("hSGn", "(Hochmoor)Stagnogley"))
  expect_identical(s("r"), c("rGGn", "reliktischer Normgley"))
  expect_identical(s("w"), c("wKVn", "aufgestautes Normerdniedermoor"))
  expect_identical(s("rt"), c("rtLLn", "r\u00f6tliche Normparabraunerde"))
  expect_identical(s("gb"), c("gbYEn", "graubrauner Normplaggenesch"))
  expect_identical(s("vf"), c("rt.vfPPn", "r\u00f6tlicher verfestigter Normpodsol"))
  expect_identical(s("og"), c("ogGGn", "(Organo)Gley"))
  expect_identical(s("i"), c("iSSn", "nassgebleichter Normpseudogley"))
  expect_identical(s("l"), c("lBBh", "lessivierte Humusbraunerde"))
  expect_identical(s("ei"), c("eiBBn", "eisenreiche Normbraunerde"))
  expect_identical(s("p"), c("p2BBn", "schwach podsolige Normbraunerde"))
  # the base-saturation class in the trace only (Hugo, 2026-11-07)
  r <- .vcls(make_ka5_example(variety = "eu"))
  expect_identical(.vsym(r), "euBBn")
  expect_identical(r$trace$variety$base_saturation$class, "BS4")
  # the Bsh of 5YR of the Podsol example is reddish (rt on the Bs, Hugo 2026-11-07)
  expect_identical(s("ro"), c("rt.roPPn", "r\u00f6tlicher (Rohhumus)Podsol"))
})

test_that("the variety examples on the data route where the data carry the criterion", {
  skip_on_cran()
  for (cd in c("mu", "eu", "m", "dy", "e", "rt", "ei")) {
    r <- .vcls(make_ka5_example(variety = cd, route = "data"))
    expect_identical(.vsub(r), soilKey:::.ka5_variety_example_subtype(cd), info = cd)
    expect_true(cd %in% .vnamed(r), info = cd)
  }
  # an Ah left undecided on the data leaves x open
  r <- .vcls(make_ka5_example(variety = "x", route = "data"))
  expect_identical(.vstat(r, "x"), "undecided")
})

test_that("the printed examples (pp. 268-272) read from their symbols to their names", {
  skip_on_cran()
  for (e in soilKey:::.ka5_var_rules()$examples) {
    r <- soilKey:::.ka5_var_read_symbol(e$symbol)
    expect_true(r$known, info = e$symbol)
    expect_identical(r$symbol, e$symbol, info = e$symbol)
    expect_identical(r$name, e$name_soilkey %||% e$name, info = e$symbol)
  }
  # the parts of a symbol
  ps <- soilKey:::.ka5_var_parse_symbol("g4.p2BB")
  expect_identical(ps$varieties$code, c("g", "p"))
  expect_identical(ps$varieties$digit, c(4L, 2L))
  expect_identical(ps$core, "BB")
  ps <- soilKey:::.ka5_var_parse_symbol("LF-eBB")
  expect_identical(ps$inline, "e")
  expect_identical(ps$core, "LF-BB")
  expect_identical(soilKey:::.ka5_var_read_symbol("p.moBB")$name, "podsolige (Moder)Braunerde")
  expect_identical(soilKey:::.ka5_var_read_symbol("eu5.m3BBn")$symbol, "eu5.m3BBn")
})

test_that("the printed examples keyed on profiles: the symbol of the book", {
  skip_on_cran()
  # a Bv of 10YR 5/4 (not iron-rich), so that only the printed varieties are named
  bv <- function(spec, n) list(spec, munsell_hue_moist = rep("10YR", n), munsell_value_moist = rep(5, n), munsell_chroma_moist = rep(4, n))
  chk <- function(r, printed, sub) {
    expect_identical(.vsub(r), sub, info = printed)
    expect_identical(.vsym_plain(.vsym(r)), .vsym_plain(printed), info = printed)
    expect_length(r$provisional_because, 0L)
  }
  r <- .vcls(.vex("BBn", bv("Aeh 0-10 / Bv 10-50 / C 50-100", 3)))
  chk(r, "pBBn", "BBn")
  expect_identical(r$name, "schwach podsolige Normbraunerde")
  r <- .vcls(.vex("BBn", bv("Aeh 0-10 / Bv 10-50 / C 50-90 / Sw 90-120 / IISd 120-150", 5)))
  chk(r, "p.sBBn", "BBn")
  expect_identical(.vsym(r), "p2.s2BBn")
  r <- .vcls(.vex("BBn", bv("Aeh 0-10 / Bv 10-50 / C 50-140 / Go 140-170 / Gr 170-200", 5)))
  chk(r, "g4.p2BB", "BBn")
  expect_identical(r$name, "tiefvergleyte schwach podsolige Normbraunerde")
  r <- .vcls(.vex("BBn", bv("Ah 0-15 / Bv 15-50 / C 50-140 / Go 140-170 / Gr 170-200", 5)))
  chk(r, "g4BB", "BBn")
  r <- .vcls(.vex("BBn", bv("Ap 0-25 / Bv 25-50 / C 50-120 / Go 120-150", 4)))
  chk(r, "g.vBB", "BBn")
  expect_identical(r$name, "flach vergleyte (Acker)Braunerde")
  r <- .vcls(.vex("BBn", c(bv("Aeh 0-10 / Bv 10-50 / C 50-100", 3), list(site = list(humus_form = "Moder")))))
  chk(r, "p.moBB", "BBn")
  expect_identical(r$name, "schwach podsolige (Moder)Braunerde")
  r <- .vcls(.vex("GGn", "Ap 0-25 / Go 25-60 / Gr 60-120"))
  chk(r, "vGGn", "GGn")
  expect_identical(r$name, "(Acker)Gley")
  r <- .vcls(.vex("SSg", list("Ap 0-20 / sSw 20-40 / IIsSd 40-100", site = list(slope_pct = 12))))
  chk(r, "vSSg", "SSg")
  expect_identical(r$name, "(Acker)Hangpseudogley")
  r <- .vcls(.vex("GG-BB", bv("Aeh 0-20 / Go-Bv 20-45 / Go 45-90 / Gr 90-150", 4)))
  chk(r, "pGG-BB", "GG-BB")
  # the Fahlerde-Braunerde of the subtype examples, with a Go at 15 dm
  lf <- make_ka5_example(subtype = "LF-BB")
  h <- data.table::rbindlist(list(lf$horizons, data.table::data.table(designation = "Go", top_cm = 150, bottom_cm = 180)), fill = TRUE)
  r <- .vcls(PedonRecord$new(site = lf$site, horizons = ensure_horizon_schema(h)))
  chk(r, "gLF-BB", "LF-BB")
  expect_identical(r$name, "tiefvergleyte Fahlerde-Braunerde")
  r <- .vcls(.vex("BBh", "Ah 0-20 / Ah-Bv 20-45 / Bv 45-70 / Ctv 70-100"))
  chk(r, "lBBh", "BBh")
  # p2RQ (p. 272): Tab. 50 marks no p for the Normregosol, so the profile
  # keeps the Normregosol without it (a slip of the source, readings)
  r <- .vcls(.vex("RQn", "Aeh< 0-20 / ilC 20-100"))
  expect_identical(.vsub(r), "RQn")
  expect_identical(.vstat(r, "p"), "not offered")
  # rule VI: r and e before the letter they qualify
  r <- .vcls(.vex("BB-GG", "Ah 0-20 / Bv-rGo 20-45 / rGo 45-90 / Gr 90-150"))
  chk(r, "BB-rGG", "BB-GG")
  expect_identical(r$name, "reliktischer Braunerde-Gley")
  r <- .vcls(.vex("LFn", "Ah 0-20 / Ael 20-45 / Ael+Bt 45-60 / Bt 60-100 / C 100-120 / rGo 120-150"))
  chk(r, "r.gLFn", "LFn")
  expect_identical(r$name, "reliktisch flach vergleyte Normfahlerde")
  lf$site$erosion_degree <- "Eg2"
  r <- .vcls(lf)
  chk(r, "LF-eBB", "LF-BB")
  expect_identical(r$name, "erodierte Fahlerde-Braunerde")
  # rule IV: three Norm subtypes, the hydromorphic type not first
  r <- .vcls(.vex("BBn", "Ahe 0-5 / Ae 5-8 / Bhs 8-12 / Bv 12-45 / Go 45-90 / Gr 90-150"))
  chk(r, "PP-GG-BB", "PP-BB")
  expect_identical(r$name, "Podsol-Gley-Braunerde")
  expect_true("PP-GG-BB" %in% r$levels$code[r$levels$level == "variety"])
  # rule V: the overlays (Tab. 46 no. 18)
  # the overlays stay out of the levels and the name (Hugo's decision of
  # 0.9.364, confirmed 2026-11-07): trace$overlay and trace$variety only
  r <- .vcls(.vex("YKn", "Ah 0-20 / M 20-50 / IIfAh 50-70 / IIBv 70-100 / IIC 100-130"))
  expect_identical(.vsub(r), "YKn")
  expect_false(any(grepl("/", r$levels$code[r$levels$level == "variety"], fixed = TRUE)))
  expect_identical(r$trace$variety$overlay$code, "YK/BB")
  expect_identical(r$trace$variety$overlay$name, "Kolluvisol \u00fcber Braunerde")
  expect_false(grepl("\u00fcber", r$name, fixed = TRUE))
  r <- .vcls(.vex("BBn", bv("Ah 0-10 / Bv 10-25 / IIfTv 25-60 / IIcC 60-80", 4)))
  expect_identical(r$trace$variety$overlay$code, "BB\\CF")
  expect_identical(r$trace$variety$overlay$name, "flache Braunerde \u00fcber Terra fusca")
  expect_identical(r$name, "Normbraunerde")
  r <- .vcls(.vex("BBn", bv("Ah 0-10 / Bv 10-40 / IIfTv 40-70 / IIcC 70-90", 4)))
  expect_identical(r$trace$variety$overlay$code, "BB/CF")
})

test_that("the combinations of subtypes (Tab. 46 no. 17; rules II-IV; Q21)", {
  skip_on_cran()
  cb <- function(sub, others, ty) soilKey:::.ka5_var_combine(sub, others, ty)
  r <- cb("PP-BB", "GG-BB", "BB")
  expect_identical(r$symbol, "PP-GG-BB"); expect_identical(r$name, "Podsol-Gley-Braunerde"); expect_identical(r$rule, "IV")
  r <- cb("GG-BB", "PP-BB", "BB")
  expect_identical(r$symbol, "PP-GG-BB")
  r <- cb("LL-TT", "SS-TT", "TT")
  expect_identical(r$symbol, "LL-SS-TT"); expect_identical(r$name, "Parabraunerde-Pseudogley-Tschernosem")
  # two transitions neither or both hydromorphic: not combined
  r <- cb("DD-BB", "LL-BB", "BB")
  expect_true(is.na(r$symbol)); expect_true(any(grepl("rule IV", r$notes)))
  # a hydromorphic type may stand last (Hugo, 2026-11-07)
  r <- cb("BB-SS", "LL-SS", "SS")
  expect_identical(r$symbol, "BB-LL-SS"); expect_identical(r$name, "Braunerde-Parabraunerde-Pseudogley")
  r <- cb("SS-BB", "GG-BB", "BB")
  expect_true(is.na(r$symbol))
  # rule II: a transition and a deviation; rule III: two deviations
  r <- cb("BB-GG", "GGe", "GG")
  expect_identical(r$symbol, "BB-GGe"); expect_identical(r$name, "Braunerde-(Brauneisen)Gley"); expect_identical(r$rule, "II")
  r <- cb("GGc", "GGh", "GG")
  expect_identical(r$symbol, "GGh-GGc"); expect_identical(r$name, "(Humus)Kalkgley"); expect_identical(r$rule, "III")
  r <- cb("GGx", "GGg", "GG")
  expect_identical(r$name, "(Hang)Oxigley")
  # a further subtype whose formula adds nothing is not joined
  r <- cb("AQ-GG", "GGa", "GG")
  expect_true(is.na(r$symbol))
  expect_true(any(grepl("not joined", r$notes, fixed = TRUE)))
  expect_null(cb("BBn", character(0), "BB"))
  # the composer of a written combination
  expect_identical(soilKey:::.ka5_var_compose("GGi-RQ")$name, "(Bleich)Gley-Regosol")
  expect_identical(soilKey:::.ka5_var_compose("BBh-GGe")$name, "(Humus)Braunerde-(Brauneisen)Gley")
})

test_that("the subvarieties: depth classes, podzolic degrees, base-saturation classes (Tab. 47, 48)", {
  skip_on_cran()
  d <- function(spec, cd) { r <- .vcls(.vex("BBn", spec)); t <- r$trace$varieties; list(st = t$status[t$code == cd], dg = t$digit[t$code == cd]) }
  # s and g: 8 dm and 13 dm, the lower end inclusive
  g <- function(z) d(sprintf("Ah 0-15 / Bv 15-50 / C 50-%d / Go %d-%d", z, z, z + 40), "g")
  # a Go at 79 cm is the Gley-Braunerde (4 to < 8 dm), no variety of it
  expect_identical(.vsub(.vcls(.vex("BBn", "Ah 0-15 / Bv 15-50 / C 50-79 / Go 79-119"))), "GG-BB")
  expect_identical(g(79)$st, "not offered")
  expect_identical(g(80)$dg, 2L)
  expect_identical(g(129)$dg, 2L)
  expect_identical(g(130)$dg, 4L)
  s <- function(z) d(sprintf("Ah 0-15 / Bv 15-50 / C 50-%d / Sw %d-%d / IISd %d-%d", z, z, z + 20, z + 20, z + 50), "s")
  expect_identical(s(80)$dg, 2L)
  expect_identical(s(135)$dg, 4L)
  # a very weak pseudogleying above 8 dm written as a transition: s, no digit
  w <- d("Ah 0-15 / Bv 15-40 / Sw-Bv 40-60 / C 60-100", "s")
  expect_identical(w$st, "named"); expect_true(is.na(w$dg))
  # the description ends above 8 dm: open; on solid rock: not named
  expect_identical(d("Ah 0-15 / Bv 15-50 / C 50-70", "g")$st, "undecided")
  expect_identical(.vstat(.vcls(make_ka5_example(subtype = "RNn")), "g"), "not offered")
  r <- .vcls(.vex("BBn", "Ah 0-15 / Bv 15-50 / mC 50-70"))
  expect_identical(.vstat(r, "g"), "not named")
  expect_identical(.vstat(r, "s"), "not named")
  # p: Aeh 2, Ahe 3, Ahe+Ae 4
  expect_identical(d("Aeh 0-10 / Bv 10-50 / C 50-100", "p")$dg, 2L)
  expect_identical(d("Ahe 0-10 / Bv 10-50 / C 50-100", "p")$dg, 3L)
  expect_identical(d("Ahe 0-5 / Ahe+Ae 5-10 / Bv 10-50 / C 50-100", "p")$dg, 4L)
  # eu, m, dy: the classes of Tab. 48 on the potential or effective CEC
  bs <- function(v, m = NA) {
    r <- .vcls(.vex("BBn", list("Ah 0-15 / Bv 15-50 / C 50-100", bs_pct = c(NA, v, NA), cec_method = c(NA, m, NA))))
    t <- r$trace$varieties; k <- t$status %in% "named" & t$code %in% c("eu", "m", "dy")
    paste0(t$code[k], t$digit[k])
  }
  expect_identical(bs(4), "dy1"); expect_identical(bs(5), "dy2"); expect_identical(bs(19), "dy2")
  expect_identical(bs(20), "m3"); expect_identical(bs(49), "m3"); expect_identical(bs(50), "eu4")
  expect_identical(bs(79), "eu4"); expect_identical(bs(80), "eu5")
  expect_identical(bs(6, "nh4cl"), "dy1"); expect_identical(bs(82, "nh4cl"), "eu4"); expect_identical(bs(85, "kcl"), "eu5")
  expect_identical(bs(82, "bacl2"), "eu5")
  # the development depth (Tab. 47), in the trace only
  r <- .vcls(make_ka5_example(subtype = "BBn"))
  expect_identical(r$trace$variety$development_depth$name, "mittel")
  expect_identical(.vcls(make_ka5_example(subtype = "RNn"))$trace$variety$development_depth$name, "mittel")
})

test_that("the base-saturation marks (Q14) and three-valued criteria", {
  skip_on_cran()
  nm <- function(spec, base = "BBn") .vnamed(.vcls(.vex(base, spec)))
  expect_true("eu" %in% nm("Ah 0-15 / Bv> 15-50 / C 50-100"))
  expect_true("m" %in% nm("Ah 0-15 / Bv< 15-50 / C 50-100"))
  expect_true("dy" %in% nm("Ah 0-15 / Bv<< 15-50 / C 50-100"))
  # the subtype scale on the Ah of an Ah/C soil: Ah< leaves m and dy open
  r <- .vcls(.vex("RNn", "Ah< 0-20 / imC 20-60"))
  expect_identical(.vsub(r), "RNn")
  expect_identical(.vstat(r, "m"), "undecided")
  expect_identical(.vstat(r, "dy"), "undecided")
  # not measured and no mark: open, listed, not named, the result not provisional
  r <- .vcls(.vex("BBn", "Ah 0-15 / Bv 15-50 / C 50-100"))
  expect_identical(.vstat(r, "eu"), "undecided")
  expect_true("eu" %in% r$trace$variety$undecided)
  expect_length(r$provisional_because, 0L)
  expect_true(any(grepl("Varieties left open by the data", r$warnings, fixed = TRUE)))
  expect_true("bs_pct" %in% unlist(r$trace$varieties$missing[r$trace$varieties$code == "eu"]))
  # the measured value beats the mark, with a note
  r <- .vcls(.vex("BBn", list("Ah 0-15 / Bv> 15-50 / C 50-100", bs_pct = c(NA, 30, NA))))
  expect_true("m" %in% .vnamed(r))
  expect_true(any(grepl("the data decide", unlist(r$trace$varieties$notes), fixed = TRUE)))
  # w: rewetting not recorded is absent (Hugo, 2026-11-07)
  r <- .vcls(make_ka5_example(subtype = "KVn"))
  expect_identical(.vstat(r, "w"), "not named")
  # the humus forms only as recorded
  expect_identical(.vstat(.vcls(make_ka5_example(subtype = "BBn")), "mu"), "not named")
  # c, z: the data for an unwritten prefix; on the data a missing record is open
  # on the notation route the data do not stand in for an unwritten e
  # (Hugo, 2026-11-07); on the data route they do
  expect_false("c" %in% nm(list("Ah 0-25 / Go 25-60 / Gr 60-120", caco3_pct = c(0, 5, 5)), "GGn"))
  r <- .vcls(soilKey:::.ka5_subtype_example("GGn", "data", spec = list("Ah 0-25 / Go 25-60 / Gr 60-120", caco3_pct = c(0, 5, 5))))
  expect_true("c" %in% .vnamed(r))
  r <- .vcls(soilKey:::.ka5_subtype_example("BBn", "data", spec = list("Ah 0-15 / Bv 15-50 / C 50-100", ec_dS_m = NA_real_)))
  expect_identical(.vstat(r, "z"), "undecided")
  # a cC under a Bv+cCv is no solum: no c
  expect_false("c" %in% .vnamed(.vcls(make_ka5_example(subtype = "BB-RR"))))
  # e: the degree of erosion; Eg0 is none
  expect_false("e" %in% nm(list("Ah 0-15 / Bv 15-50 / C 50-100", site = list(erosion_degree = "Eg0"))))
  # no anthropic_truncation stand-in (Hugo, 2026-11-07)
  expect_false("e" %in% nm(list("Ah 0-15 / Bv 15-50 / C 50-100", site = list(anthropic_truncation = TRUE))))
  # thin organic covers: 1 dm or more is no variety
  expect_false("n" %in% nm("nH 0-10 / Go-Ah 10-25 / Gr 25-100", "GNn"))
  # k: an Ah/M of 4 dm or more is the Kolluvisol, no variety
  r <- .vcls(.vex("BBn", "Ah 0-15 / M 15-30 / Bv 30-60 / C 60-100"))
  expect_true("k" %in% .vnamed(r))
})

test_that("every KA5 example keeps its levels above the variety", {
  skip_on_cran()
  allowed <- c("divisions", "classes", "types", "subtypes", "variety")
  chk <- function(r, info) {
    lv <- r$levels$level
    expect_true(all(lv %in% allowed), info = info)
    w <- which(lv == "variety")
    if (length(w)) expect_true(all(w > max(which(lv != "variety"))), info = info)
    if (length(w)) expect_identical(r$name, r$trace$variety$name, info = info)
  }
  for (t in eval(formals(make_ka5_example)$type)) {
    r <- .vcls(make_ka5_example(t))
    chk(r, t)
    expect_identical(.vtype(r), if (t == "UEA") "\u00dcA" else t, info = t)
  }
  ent <- soilKey:::.ka5_subtype_entries()
  for (cd in names(ent)[!startsWith(names(ent), "[X-")]) {
    r <- .vcls(make_ka5_example(subtype = cd))
    chk(r, cd)
    expect_identical(.vtype(r), ent[[cd]]$type, info = cd)
    expect_identical(.vsub(r), cd, info = cd)
  }
})

test_that("the variety level in the result, the trace and the registry", {
  skip_on_cran()
  r <- .vcls(make_ka5_example(variety = "p"))
  lv <- r$levels
  w <- which(lv$level == "variety")
  expect_true(all(c("code", "name", "name_en", "symbol") %in% names(lv)))
  expect_identical(lv$code[w], "p2")
  expect_identical(lv$name[w], "schwach podsolige")
  expect_identical(r$qualifiers$variety, lv$name[w])
  expect_true(is.data.frame(r$trace$varieties))
  expect_identical(nrow(r$trace$varieties), 38L)
  expect_setequal(unique(r$trace$varieties$status), c("named", "not named", "undecided", "not offered", "in the formula")[
    c("named", "not named", "undecided", "not offered", "in the formula") %in% r$trace$varieties$status])
  # the registry keeps its levels; the variety is an additive level
  sys <- soilkey_systems()
  expect_true("ka5" %in% sys$id)
  # the readings of this part are in readings.yaml
  rd <- yaml::read_yaml(system.file("rules", "ka5", "readings.yaml", package = "soilKey"))$readings
  expect_true(any(grepl("Tab. 50 read from the page images", vapply(rd, `[[`, "", "topic"), fixed = TRUE)))
  # the variety source is ASCII
  f <- system.file("..", "R", "varieties-ka5.R", package = "soilKey")
  src <- if (file.exists(f)) f else testthat::test_path("..", "..", "R", "varieties-ka5.R")
  if (file.exists(src)) expect_false(any(grepl("[^\\x01-\\x7f]", readLines(src, warn = FALSE), perl = TRUE)))
})

test_that("the review of 2026-11-07: Hugo's decisions and the defects", {
  skip_on_cran()
  # D2: the relict r qualifies the type when no g or s is named
  r <- .vcls(.vex("MCn", "tmeAh 0-20 / tmeGo 20-50 / tmzeGr 50-85 / tmerGo 85-120"))
  expect_identical(.vsub(r), "MCn")
  expect_true("r" %in% .vnamed(r))
  expect_false("g" %in% .vnamed(r))
  expect_identical(.vsym(r), "rMCn")
  expect_true(startsWith(r$name, "reliktische "))
  r <- .vcls(.vex("ABn", "aAh 0-20 / aM 20-90 / arGo 90-150"))
  expect_identical(.vsub(r), "ABn")
  if ("r" %in% .vnamed(r)) {
    expect_false(grepl("^r\\.", .vsym(r)))
    expect_true(grepl("rAB", .vsym(r), fixed = TRUE))
  }
  # D3: the O/C soils read the O horizon; nothing to read is open, never FALSE
  r <- .vcls(.vex("FFn", list("O 0-8 / mC 8-30", bs_pct = c(70, NA))))
  expect_identical(.vsub(r), "FFn")
  expect_true("eu" %in% .vnamed(r))
  r <- .vcls(make_ka5_example(subtype = "FFn"))
  expect_identical(.vstat(r, "eu"), "undecided")
  # 6: no horizon outside the C reaching into 2 to < 5 dm: open, with a note
  r <- .vcls(.vex("BBn", list("Ah 0-15 / Bv 15-19 / C 19-100", bs_pct = c(NA, 60, NA))))
  expect_identical(.vstat(r, "eu"), "undecided")
  expect_true(any(grepl("2 to < 5 dm", unlist(r$trace$varieties$notes[r$trace$varieties$code == "eu"]), fixed = TRUE)))
  # D4: b on the data route only with a brown colour; the twins agree
  rn <- .vcls(make_ka5_example(subtype = "RRn"))
  rd <- .vcls(make_ka5_example(subtype = "RRn", route = "data"))
  expect_false("b" %in% .vnamed(rn)); expect_false("b" %in% .vnamed(rd))
  rb <- .vcls(soilKey:::.ka5_subtype_example("RRn", "data", spec = list("Ah> 0-20 / cmC 20-60", bs_pct = c(90, NA), oc_pct = c(3, NA),
                                                                        munsell_hue_moist = c("7.5YR", NA), munsell_chroma_moist = c(4, NA))))
  if (identical(.vsub(rb), "RRn")) expect_true("b" %in% .vnamed(rb))
  # 5(1): no note-1 c on the carbonate types; an optional (e) counts as written
  expect_identical(soilKey:::.ka5_var_offer("RZn")$source[soilKey:::.ka5_var_columns() == "c"], "in the formula")
  expect_identical(soilKey:::.ka5_var_offer("TCn")$source[soilKey:::.ka5_var_columns() == "c"], "in the formula")
  expect_identical(soilKey:::.ka5_var_offer("RRn")$source[soilKey:::.ka5_var_columns() == "c"], "in the formula")
  expect_identical(soilKey:::.ka5_var_offer("GG-AZ")$source[soilKey:::.ka5_var_columns() == "c"], "in the formula")
  expect_true(soilKey:::.ka5_var_in_formula("AZ-GG", "c"))
  # 5(3): a separate Ae under an Ahe is stark
  r <- .vcls(.vex("BBn", "Ahe 0-5 / Ae 5-8 / Bv 8-50 / C 50-100"))
  if (identical(.vsub(r), "BBn")) expect_identical(r$trace$varieties$digit[r$trace$varieties$code == "p"], 4L)
  # 5(7): a and g on the same deep aGo: only a
  r <- .vcls(.vex("BBn", "Ah 0-15 / Bv 15-50 / aC 50-90 / aGo 90-150"))
  expect_true("a" %in% .vnamed(r)); expect_false("g" %in% .vnamed(r))
  # minor: an aG at 80 cm is not deeper than 8 dm
  r <- .vcls(.vex("BBn", "Ah 0-15 / Bv 15-50 / aC 50-80 / aGo 80-150"))
  expect_false("a" %in% .vnamed(r))
  # minor: the Auen reader open where the description ends above 8 dm
  expect_identical(.vstat(.vcls(.vex("BBn", "Ah 0-15 / Bv 15-50 / C 50-70")), "a"), "undecided")
  # minor: schwertmannite
  r <- .vcls(.vex("MOn", list("tmoAh 0-20 / tmoGo 20-50 / tmoGr 50-120", oc_pct = c(6, 1, 0.5), clay_pct = 50,
                              schwertmannite_present = c(NA, TRUE, NA))))
  expect_true("sf" %in% .vnamed(r))
  # 4: rt on the type's own B or T: the Braunerde's Bv, the Podsol's Bs, the Terra fusca's Tv
  expect_true("rt" %in% .vnamed(.vcls(.vex("BBn", list("Ah 0-15 / Bv 15-50 / C 50-100", munsell_hue_moist = c("10YR", "5YR", "10YR"))))))
  r <- .vcls(.vex("CFn", list("Ah 0-10 / Tv 10-45 / cC 45-80", munsell_hue_moist = c("10YR", "5YR", NA))))
  if (identical(.vsub(r), "CFn")) expect_true("rt" %in% .vnamed(r))
  # the default Bv of the examples is not iron-rich
  expect_false("ei" %in% .vnamed(.vcls(make_ka5_example("BB"))))
  expect_false("ei" %in% .vnamed(.vcls(make_ka5_example(subtype = "BBn"))))
})
