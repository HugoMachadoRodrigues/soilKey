# v0.9.363: the 138 subtypes of the 28 terrestrial types of the German KA5
# (pp. 202-237), keyed after the type (Hugo's decisions Q21, Q22, Q14): one
# example per subtype in the KA5 notation, the window edges, transitional
# against Norm, provisional subtypes with missing data, the Fahlerde conflict
# (C13), the open subtypes of the Rigosol and Treposol, the data-route twins.

.st_cls <- function(p) classify_ka5(p, on_missing = "silent")
.st_of <- function(r) { lv <- r$levels; k <- which(lv$level == "subtypes"); if (length(k)) lv$code[k] else NA_character_ }
.st_type <- function(r) { lv <- r$levels; k <- which(lv$level == "types"); if (length(k)) lv$code[k] else NA_character_ }
.st_mk <- function(prof, ..., site = list()) {
  p <- soilKey:::.ka5_parse_profile_string(prof)
  n <- length(p$des)
  h <- c(list(designation = p$des, top_cm = p$top, bottom_cm = p$bot), list(...))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  PedonRecord$new(site = utils::modifyList(list(id = "ka5-st", country = "DE", designation_system = "ka5"), site),
                  horizons = ensure_horizon_schema(data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}
.st_codes <- function() names(soilKey:::.KA5_SUBTYPE_PROFILES)

# v0.9.365: the varieties are an additive level that joins the name; the
# name of the subtype is the deepest level above the variety
.lv_name <- function(r) { lv <- r$levels; k <- which(lv$level != "variety"); lv$name[k[length(k)]] }

test_that("the registry lists the subtypes; 138 printed terrestrial subtypes, in the order of Q21", {
  skip_on_cran()
  s <- soilkey_systems()
  expect_identical(s$levels[s$id == "ka5"], "divisions, classes, types, subtypes")
  v <- soilKey:::.validate_rules("ka5")
  for (k in c("missing", "unsuffixed", "mixed", "malformed", "codes")) expect_identical(v[[k]], character(0), info = k)
  rl <- load_rules("ka5")
  # the terrestrial types (the wet subtypes of v0.9.364 are tested in test-v09364)
  rl$subtypes <- rl$subtypes[names(rl$subtypes) %in% c("FF", "FS", "OO", "OL", "RN", "RQ", "RR", "RZ", "TT", "TC", "DD",
                                                        "BB", "LL", "LF", "PP", "CF", "CR", "SS", "SH", "SG", "XX", "YK",
                                                        "YE", "YO", "YY", "YU")]
  ent <- unlist(rl$subtypes, recursive = FALSE)
  codes <- vapply(ent, `[[`, "", "code")
  printed <- codes[!startsWith(codes, "[")]
  expect_length(printed, 138L)
  expect_setequal(setdiff(codes, printed), c("[X-YY]", "[X-YU]"))
  expect_false(anyDuplicated(codes) > 0L)
  # every subtype of the formulas transcribed from the images (fixtures), and no other
  fx <- utils::read.delim(testthat::test_path("fixtures", "ka5-formulas.tsv"), comment.char = "#",
                          quote = "", colClasses = "character", encoding = "UTF-8")
  fx <- fx[fx$kind == "subtype" & fx$class %in% strsplit("FORTDBLPCVSXY", "")[[1]], ]
  expect_setequal(printed, unique(fx$unit))
  # per class (the reading map's count, no total printed: F 2, O 3, R 27, T 11, D 8, B 10, L 15, P 11, C 6,
  # V 0, S 27, X 4, Y 14)
  cls <- c(FF = "F", FS = "F", OO = "O", OL = "O", RN = "R", RQ = "R", RR = "R", RZ = "R", TT = "T", TC = "T",
           DD = "D", BB = "B", LL = "L", LF = "L", PP = "P", CF = "C", CR = "C", SS = "S", SH = "S", SG = "S",
           XX = "X", YK = "Y", YE = "Y", YO = "Y", YY = "Y", YU = "Y")
  n <- vapply(names(rl$subtypes), function(t) sum(!startsWith(vapply(rl$subtypes[[t]], `[[`, "", "code"), "[")), 0L)
  per <- tapply(n, cls[names(n)], sum)
  expect_identical(as.integer(per[c("F", "O", "R", "T", "D", "B", "L", "P", "C", "S", "X", "Y")]),
                   c(2L, 3L, 27L, 11L, 8L, 10L, 15L, 11L, 6L, 27L, 4L, 14L))
  expect_null(rl$subtypes[["VV"]]); expect_null(rl$subtypes[["VW"]])
  # each entry: name as printed, glosses, kind, page; formulas that read
  kinds <- c(transition = 1L, deviation = 2L, norm = 3L)
  for (t in names(rl$subtypes)) {
    l <- rl$subtypes[[t]]
    k <- kinds[vapply(l, `[[`, "", "kind")]
    expect_false(is.unsorted(k), info = t)
    for (e in l) {
      expect_true(all(nzchar(c(e$name, e$name_en, e$name_pt, e$page))), info = e$code)
      if (!startsWith(e$code, "[") && e$kind == "transition") expect_true(endsWith(e$code, paste0("-", t)), info = e$code)
      for (f in e$formulas) {
        fm <- if (is.list(f)) f$formula else f
        expect_length(soilKey:::.ka5_parse_formula(gsub("[<>]", "", fm))$unknown, 0L)
      }
      for (w in c(e$windows, unlist(lapply(e$formulas, function(f) if (is.list(f)) f$windows), recursive = FALSE)))
        expect_true(is.numeric(soilKey:::.ka5_thr(w$thr)), info = e$code)
    }
  }
  # the Norm last in every type but the Treposol (Q22)
  for (t in setdiff(names(rl$subtypes), "YU")) {
    l <- rl$subtypes[[t]]
    expect_identical(l[[length(l)]]$kind, "norm", info = t)
    # the Norm tests its formula and marks, holding provisionally when they fail (R1)
    expect_true(isTRUE(l[[length(l)]]$tests$all_of[[1]]$subtype_ka5$norm), info = t)
  }
  expect_false(any(vapply(rl$subtypes[["YU"]], `[[`, "", "kind") == "norm"))
  expect_identical(vapply(rl$subtypes[["BB"]], `[[`, "", "code"),
                   c("DD-BB", "LL-BB", "LF-BB", "PP-BB", "SS-BB", "GG-BB", "BBc", "BBh", "BBl", "BBn"))
  expect_identical(rl$subtypes[["YY"]][[1]]$code, "[X-YY]")
})

test_that("each of the 138 subtype examples keys to its type and subtype by the notation", {
  skip_on_cran()
  ent <- soilKey:::.ka5_subtype_entries()
  expect_length(.st_codes(), 138L)
  for (cd in .st_codes()) {
    p <- make_ka5_example(subtype = cd)
    expect_identical(p$site$designation_system, "ka5")
    r <- .st_cls(p)
    expect_identical(.st_type(r), ent[[cd]]$type, info = cd)
    expect_identical(.st_of(r), cd, info = cd)
    expect_identical(.lv_name(r), ent[[cd]]$name, info = cd)
    # provisional only where the Fahlerde's clay difference cannot be measured (C13)
    if (identical(cd, "LF-YU")) expect_identical(r$provisional_because, "subtypes:LF-YU")
    else expect_length(r$provisional_because, 0L)
    expect_true(is.data.frame(r$trace$subtypes_passing), info = cd)
    expect_true(cd %in% r$trace$subtypes_passing$code[r$trace$subtypes_passing$passed %in% TRUE], info = cd)
  }
  expect_error(make_ka5_example(subtype = "ZZn"), "subtype example")
})

test_that("the type examples key to their Norm subtype; the levels and glosses", {
  skip_on_cran()
  terr <- c("FF", "FS", "OO", "OL", "TC", "TT", "RN", "RQ", "RR", "RZ", "XX", "YK", "YE", "YY", "YO", "SG", "SH",
            "SS", "PP", "LF", "LL", "BB", "CR", "CF", "DD")
  for (t in terr) {
    r <- .st_cls(make_ka5_example(t))
    expect_identical(.st_of(r), paste0(t, "n"), info = t)
    expect_length(r$provisional_because, 0L)
  }
  # the Treposol has no Norm subtype (Q22): its example is a printed example subtype
  expect_identical(.st_of(.st_cls(make_ka5_example("YU"))), "PP-YU")
  expect_identical(.lv_name(.st_cls(make_ka5_example("YU"))), "Treposol aus Podsol")
  # the palaeosols and the subhydric types have no subtypes (the other divisions' since v0.9.364)
  for (t in c("VV", "VW", "JG")) expect_true(is.na(.st_of(.st_cls(make_ka5_example(t)))), info = t)
  r <- .st_cls(make_ka5_example(subtype = "GG-BB"))
  expect_identical(r$levels$level, c("divisions", "classes", "types", "subtypes"))
  expect_identical(r$levels$name, c("Terrestrische Böden", "Braunerden", "Braunerde", "Gley-Braunerde"))
  expect_identical(r$levels$name_en[4], "Gley-brown earth")
  expect_identical(.lv_name(.st_cls(make_ka5_example(subtype = "LLd"))), "Bänderparabraunerde")
  # the i18n of the level
  p <- system.file("i18n", "translations.yaml", package = "soilKey")
  if (!nzchar(p)) p <- testthat::test_path("..", "..", "inst", "i18n", "translations.yaml")
  tr <- yaml::read_yaml(p)
  expect_identical(tr$en[["classify.level_subtypes"]], "Subtype")
  expect_identical(tr$pt[["classify.level_subtypes"]], "Subtipo")
  p <- system.file("i18n", "report_translations.yaml", package = "soilKey")
  if (!nzchar(p)) p <- testthat::test_path("..", "..", "inst", "i18n", "report_translations.yaml")
  tr <- yaml::read_yaml(p)
  expect_identical(tr$en[["report.level_subtypes"]], "Subtype")
  expect_identical(tr$pt[["report.level_subtypes"]], "Subtipo")
})

test_that("the window edges: 4 and 8 dm, 1.5 dm, the humic 4 dm, 1.5 and 3 dm, 1 dm", {
  skip_on_cran()
  # the Go of the Gley-Braunerde at 4 to < 8 dm (p. 217); at 39 cm the soil is semiterrestrial
  gb <- function(z) .st_cls(.st_mk(sprintf("Ah 0-20 / Bv 20-%d / Go %d-120 / Gr 120-150", z, z)))
  expect_identical(.st_of(gb(40)), "GG-BB")
  expect_identical(.st_of(gb(79)), "GG-BB")
  expect_identical(.st_of(gb(80)), "BBn")
  expect_false(identical(gb(39)$levels$code[1], "[terrestrisch]"))
  # the transitional Go counts by its last symbol (Q8): Bv-Go at 45 is the Go; Go-Bv is a Bv
  expect_identical(.st_of(.st_cls(.st_mk("Ah 0-20 / Bv 20-45 / Bv-Go 45-90 / Gr 90-150"))), "GG-BB")
  expect_identical(.st_of(.st_cls(.st_mk("Ah 0-20 / Go-Bv 20-90 / C 90-150"))), "BBn")
  # the S of the Pseudogley-Braunerde at 4 to < 8 dm
  sb <- function(z) .st_cls(.st_mk(sprintf("Ah 0-20 / Bv 20-%d / Sw %d-%d / IISd %d-130", z, z, z + 15, z + 15),
                                   site = list(groundwater_kind = "perched")))
  expect_identical(.st_of(sb(40)), "SS-BB")
  expect_identical(.st_of(sb(79)), "SS-BB")
  expect_identical(.st_of(sb(80)), "BBn")
  expect_identical(.st_type(sb(39)), "SS")
  # the podzol part of the Podsol-Braunerde ends above 1.5 dm; at 1.5 dm the soil is a Podsol (BB-PP)
  pb <- function(z) .st_cls(.st_mk(sprintf("Ahe 0-5 / Ae 5-8 / Bhs 8-%d / Bv %d-50 / C 50-100", z, z)))
  expect_identical(.st_of(pb(14)), "PP-BB")
  expect_identical(.st_type(pb(15)), "PP")
  expect_identical(.st_of(pb(15)), "BB-PP")
  # one podzol B for the pair B(s)h/B(h)s (soilKey's reading), or both
  expect_identical(.st_of(.st_cls(.st_mk("Ahe 0-5 / Ae 5-8 / Bsh 8-12 / Bhs 12-20 / Bv 20-50 / C 50-100"))), "BB-PP")
  # the B(h)s of the Braunerde-Podsol ends at 1.5 to < 7 dm
  bp <- function(z) .st_cls(.st_mk(sprintf("Ahe 0-5 / Ae 5-10 / Bhs 10-%d / Bv %d-%d / C %d-150", z, z, z + 20, z + 20)))
  expect_identical(.st_of(bp(69)), "BB-PP")
  expect_identical(.st_of(bp(70)), "PPn")
  # the humic part of the Humusbraunerde reaches 4 dm
  bh <- function(z) .st_cls(.st_mk(sprintf("Ah 0-20 / Ah-Bv 20-%d / Bv %d-80 / C 80-120", z, z)))
  expect_identical(.st_of(bh(40)), "BBh")
  expect_identical(.st_of(bh(39)), "BBn")
  # the Ah/Bv of the Pelosol-Braunerde ends at 1.5 to < 3 dm
  db <- function(z) .st_cls(.st_mk(sprintf("Ah 0-10 / Bv 10-%d / IIBv-P %d-60 / IIeC 60-100", z, z)))
  expect_identical(.st_of(db(15)), "DD-BB")
  expect_identical(.st_of(db(29)), "DD-BB")
  expect_false(identical(.st_of(db(30)), "DD-BB"))
  # the Yr of the Normreduktosol under 4 dm, of the Ockerreduktosol 4 dm or deeper
  xx <- function(z) .st_of(.st_cls(.st_mk(sprintf("Ah 0-15 / Yo 15-%d / Yr %d-100", z, z),
                                          site = list(reductgas_source = "refuse_sludge_harbour_mud"))))
  expect_identical(xx(39), "XXn")
  expect_identical(xx(40), "XXx")
  # the Sw-Aa of the Anmoorpseudogley 1 dm or more
  sm <- function(z) .st_of(.st_cls(.st_mk(sprintf("Sw-Aa 0-%d / Sw %d-35 / IISd 35-100", z, z), site = list(groundwater_kind = "perched"))))
  expect_identical(sm(10), "SSm")
  expect_identical(sm(9), "SSn")
})

test_that("transitional before deviation before Norm; every passing subtype reported (Q21)", {
  skip_on_cran()
  # a humic Braunerde with its Go at 5 dm: GG-BB names it, BBh passes too
  r <- .st_cls(.st_mk("Ah 0-20 / Ah-Bv 20-45 / Go 45-90 / Gr 90-130 / C 130-150"))
  expect_identical(.st_of(r), "GG-BB")
  sp <- r$trace$subtypes_passing
  expect_identical(sp$code[sp$passed %in% TRUE], c("GG-BB", "BBh", "BBn"))
  expect_true(any(grepl("Further subtypes of the Braunerde pass: BBh", r$warnings, fixed = TRUE)))
  # the Norm only when nothing else passes
  r <- .st_cls(make_ka5_example("BB"))
  expect_identical(r$trace$subtypes_passing$code[r$trace$subtypes_passing$passed %in% TRUE], "BBn")
  # the Kalkpseudogley is reachable (the Scw of the Pseudogley gate)
  expect_identical(.st_of(.st_cls(make_ka5_example(subtype = "SSc"))), "SSc")
})

test_that("a gate the data leave open makes the subtype provisional, with candidates", {
  skip_on_cran()
  # the base saturation of the Ranker's Ah not recorded: RNn, provisionally, RNr a candidate
  r <- .st_cls(.st_mk("Ah 0-20 / imC 20-60"))
  expect_identical(.st_of(r), "RNn")
  expect_setequal(r$provisional_because, c("subtypes:RNr", "subtypes:RNn"))
  # the mark written decides when nothing is measured (Q14); the data decide over the mark
  expect_identical(.st_of(.st_cls(.st_mk("Ah> 0-20 / imC 20-60"))), "RNr")
  r <- .st_cls(.st_mk("Ah> 0-20 / imC 20-60", bs_pct = c(30, NA)))
  expect_identical(.st_of(r), "RNn")
  expect_true(any(grepl("the data decide", unlist(lapply(r$trace$subtypes$RNr$evidence, `[[`, "notes")), fixed = TRUE)))
  expect_identical(.st_of(.st_cls(.st_mk("Ah 0-20 / imC 20-60", bs_pct = c(55, NA)))), "RNr")
  expect_identical(.st_of(.st_cls(.st_mk("Ah 0-20 / imC 20-60", bs_pct = c(50, NA)))), "RNr")
  expect_identical(.st_of(.st_cls(.st_mk("Ah 0-20 / imC 20-60", bs_pct = c(49.9, NA)))), "RNn")
  # a Braunerde described only to 5 dm: the Gley- and Pseudogley-Braunerde undecided
  r <- .st_cls(.st_mk("Ah 0-20 / Bv 20-50"))
  expect_identical(.st_of(r), "BBn")
  expect_true(all(c("subtypes:SS-BB", "subtypes:GG-BB") %in% r$provisional_because))
  # the description ends above the formula's last horizons: undecided, not failed
  r <- .st_cls(.st_mk("Ahe 0-5 / Ae 5-8 / Bhs 8-12 / Bv 12-35"))
  expect_true("subtypes:PP-BB" %in% r$provisional_because)
  # the Gley-Braunerde without its Gr (typically 13 dm or deeper) holds
  expect_identical(.st_of(.st_cls(.st_mk("Ah 0-20 / Bv 20-45 / Go 45-100"))), "GG-BB")
  # the Hangpseudogley on the data with the slope not recorded
  r <- .st_cls(make_ka5_example("SS", route = "data"))
  expect_true("subtypes:SSg" %in% r$provisional_because)
  r <- .st_cls(make_ka5_example(subtype = "SSg", route = "data"))
  expect_identical(.st_of(r), "SSg")
  # in the notation an S written without the prefix s is no Hangpseudogley, unless a slope of 9 % or
  # more is recorded: then undecided, SSg a candidate (R2, Hugo 2026-11-06)
  r <- .st_cls(.st_mk("Ah 0-20 / Sw 20-40 / IISd 40-100", site = list(slope_pct = 15, groundwater_kind = "perched")))
  expect_identical(.st_of(r), "SSn")
  expect_identical(r$provisional_because, "subtypes:SSg")
  r <- .st_cls(.st_mk("Ah 0-20 / Sw 20-40 / IISd 40-100", site = list(slope_pct = 4, groundwater_kind = "perched")))
  expect_identical(.st_of(r), "SSn")
  expect_length(r$provisional_because, 0L)
})

test_that("the Fahlerde conflict (C13) in the subtypes", {
  skip_on_cran()
  base <- "Ah 0-15 / Bv 15-40 / Ael 40-80 / Ael+Bt 80-100 / Bt 100-130 / C 130-150"
  # measured: 14 points (12 or more) holds
  expect_identical(.st_of(.st_cls(.st_mk(base, clay_pct = c(12, 13, 12, 26, 26, 20), silt_pct = 60))), "LF-BB")
  # measured and short (6 points): no Fahlerde-Braunerde, the conflict reported
  r <- .st_cls(.st_mk(base, clay_pct = c(12, 13, 18, 24, 24, 20), silt_pct = 60))
  expect_false(identical(.st_of(r), "LF-BB"))
  expect_true(any(grepl("C13", unlist(lapply(r$trace$subtypes[["LF-BB"]]$evidence, `[[`, "notes")), fixed = TRUE)))
  # not measured: on the designation, provisionally
  r <- .st_cls(.st_mk(base))
  expect_identical(.st_of(r), "LF-BB")
  expect_identical(r$provisional_because, "subtypes:LF-BB")
  # the type: the Fahlerde with its clay not recorded keys LFn, the type provisional (C13)
  r <- .st_cls(.st_mk("Ah 0-20 / Ael 20-45 / Ael+Bt 45-60 / Bt 60-100 / C 100-130"))
  expect_identical(.st_type(r), "LF")
  expect_identical(.st_of(r), "LFn")
  expect_true("types:LF" %in% r$provisional_because)
  # the LL-BB band (C14): measured inside it, measured above it (a Bt), not measured
  lb <- "Ah 0-20 / Al-Bv 20-35 / Btv 35-90 / C 90-130"
  expect_identical(.st_of(.st_cls(.st_mk(lb, clay_pct = c(15, 15, 18, 16), silt_pct = 60))), "LL-BB")
  expect_false(identical(.st_of(.st_cls(.st_mk(lb, clay_pct = c(15, 15, 15.5, 16), silt_pct = 60))), "LL-BB"))
  r <- .st_cls(.st_mk(lb))
  expect_identical(.st_of(r), "LL-BB")
  expect_identical(r$provisional_because, "subtypes:LL-BB")
})

test_that("the open subtypes of the Rigosol and the Treposol (Q22)", {
  skip_on_cran()
  yy <- list(deep_cultivation_kind = "repeated_trenching", mechanical_mixing_depth_cm = 70)
  yu <- list(deep_cultivation_kind = "single_deep_ploughing", mechanical_mixing_depth_cm = 60)
  r <- .st_cls(.st_mk("R-Ap 0-30 / R 30-70 / Bv 70-100 / C 100-130", site = yy))
  expect_identical(.st_type(r), "YY")
  expect_identical(.st_of(r), "[BB-YY]")
  expect_identical(.lv_name(r), "[Braunerde-Rigosol]")
  expect_identical(r$levels$name_en[4], "[Rigosol (trenched soil) from brown earth]")
  expect_identical(.st_of(.st_cls(.st_mk("R-Ap 0-30 / R 30-70 / Sw 70-90 / Sd 90-130", site = yy))), "[SS-YY]")
  # a G beginning at 8 dm or deeper names nothing (soilKey's reading)
  expect_identical(.st_of(.st_cls(.st_mk("R-Ap 0-30 / R 30-80 / Go 80-130", site = yy))), "YYn")
  expect_identical(.st_of(.st_cls(make_ka5_example("YY"))), "YYn")
  # the Treposol: the four printed examples first, then the open subtype
  for (cd in c("PP-YU", "LL-YU", "LF-YU", "GG-YU"))
    expect_identical(.st_of(.st_cls(make_ka5_example(subtype = cd))), cd)
  r <- .st_cls(.st_mk("R-Ap 0-30 / R+Bv 30-60 / C 60-100", site = yu))
  expect_identical(.st_of(r), "[BB-YU]")
  expect_identical(.lv_name(r), "[Treposol aus Braunerde]")
  # nothing recognisable: no Norm, the name stops at the type
  r <- .st_cls(.st_mk("R-Ap 0-30 / R 30-60 / C 60-100", site = yu))
  expect_identical(.st_type(r), "YU")
  expect_true(is.na(.st_of(r)))
  expect_identical(r$name, "Treposol")
  expect_true(any(grepl("no Norm subtype", r$warnings, fixed = TRUE)))
})

test_that("the data-route twins key to their subtype where the data route reaches it", {
  skip_on_cran()
  dr <- soilKey:::.KA5_DATA_ROUTE_SUBTYPES
  expect_true(length(dr) >= 40L)
  expect_true(all(dr %in% .st_codes()))
  for (cd in dr) {
    p <- make_ka5_example(subtype = cd, route = "data")
    expect_true(all(is.na(p$horizons$designation)))
    r <- .st_cls(p)
    expect_identical(.st_of(r), cd, info = cd)
    expect_true(all(r$trace$horizons$route == "data"), info = cd)
  }
})

test_that("the KA5 subtype sources are ASCII", {
  skip_on_cran()
  src <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(src), "package sources not available")
  for (fn in c("subtypes-ka5.R", "examples-ka5-subtypes.R")) {
    txt <- readLines(file.path(src, fn), warn = FALSE)
    expect_false(any(grepl("[^\\x01-\\x7F]", txt, perl = TRUE)), info = fn)
  }
})


# ---- the review of 2026-11-06 (Hugo: 8 defects, decisions R1, R2) ---------------------------

test_that("D1: rows under a formula are free, its final C met by any deeper row, else undecided", {
  skip_on_cran()
  expect_identical(.st_of(.st_cls(.st_mk("Ah 0-20 / Ah-Bv 20-45 / Bv 45-85 / Go 85-110 / Gr 110-150"))), "BBh")
  expect_identical(.st_of(.st_cls(.st_mk("Ahe 0-5 / Ae 5-15 / Bsh 15-20 / Bhs 20-35 / Bv 35-60 / Go 60-110"))), "BB-PP")
  expect_identical(.st_of(.st_cls(.st_mk("Ahe 0-5 / Ae 5-15 / Bsh 15-20 / Bhs 20-35 / Al 35-50 / Bt 50-80 / Sd 80-120"))), "LL-PP")
  expect_identical(.st_of(.st_cls(.st_mk("Ah 0-15 / Bcv 15-50 / Go 85-120"))), "BBc")
  expect_identical(.st_of(.st_cls(.st_mk("Ah 0-25 / Al 25-45 / Bv+Bbt 45-80 / Sd 85-130"))), "LLd")
  # nothing described under the Bv: the C of the Humusbraunerde undecided, not failed
  r <- .st_cls(.st_mk("Ah 0-20 / Ah-Bv 20-45 / Bv 45-100"))
  expect_identical(.st_of(r), "BBn")
  expect_true("subtypes:BBh" %in% r$provisional_because)
  expect_true(is.na(r$trace$subtypes_passing$passed[r$trace$subtypes_passing$code == "BBh"]))
})

test_that("D2: a slot (X-)Y takes a run of X-Y then Y; the Bv-Tv of the Braunerde-Terra fusca", {
  skip_on_cran()
  expect_identical(.st_of(.st_cls(.st_mk("Ah 0-20 / Al 20-40 / Bt 40-60 / Bt-Go 60-75 / Go 75-100 / Gr 100-150"))), "GG-LL")
  expect_identical(.st_of(.st_cls(.st_mk("Ah 0-20 / Ael 20-40 / Bt 40-60 / Bt-Go 60-75 / Go 75-100 / Gr 100-150"))), "GG-LF")
  bc <- "Ah 0-10 / Bv-Tv 10-30 / Tv 30-45 / cC 45-80"
  expect_identical(.st_of(.st_cls(.st_mk(bc, clay_pct = c(40, 55, 70, NA)))), "BB-CF")
  # the clay check reads the Bv-Tv only, not the Tv (p. 225)
  expect_false(identical(.st_of(.st_cls(.st_mk(bc, clay_pct = c(40, 68, 70, NA)))), "BB-CF"))
})

test_that("D3: the first forms of SS-LL and SS-LF reach the Lessives", {
  skip_on_cran()
  r <- .st_cls(.st_mk("Ah 0-20 / Al 20-45 / Al-Sw 45-60 / Bt-Sd 60-90 / C 90-120"))
  expect_identical(.st_type(r), "LL")
  expect_identical(.st_of(r), "SS-LL")
  r <- .st_cls(.st_mk("Ah 0-20 / Ael 20-45 / Ael-Sw 45-60 / Bt-Sd 60-90 / C 90-120", clay_pct = c(12, 12, 12, 28, 20), silt_pct = 60))
  expect_identical(.st_type(r), "LF")
  expect_identical(.st_of(r), "SS-LF")
  expect_length(r$provisional_because, 0L)
  # a Bt part on a Terra (Bt-Tv of the Parabraunerde-Terra fusca) does not open the Lessives
  expect_identical(.st_of(.st_cls(make_ka5_example(subtype = "LL-CF"))), "LL-CF")
})

test_that("D4: the Fahlerde's clay difference is the Ael against the Bt, not the Ael+Bt (p. 220)", {
  skip_on_cran()
  r <- .st_cls(.st_mk("Ah 0-20 / Ael 20-45 / Ael+Bt 45-60 / Bt 60-100 / C 100-130", clay_pct = c(12, 12, 18, 26, 20), silt_pct = 60))
  expect_identical(.st_type(r), "LF")
  expect_identical(.st_of(r), "LFn")
  expect_length(r$provisional_because, 0L)
  # the LF-BB example: the Ael+Bt between the two
  p <- make_ka5_example(subtype = "LF-BB")
  expect_identical(p$horizons$clay_pct[4], 18)
  expect_identical(.st_of(.st_cls(p)), "LF-BB")
})

test_that("D5, D6, R1: the Euranker needs no Bv; the Norm tests its marks, provisionally when they fail", {
  skip_on_cran()
  r <- .st_cls(.st_mk("Ah> 0-15 / Bv-Ah 15-20 / imC 20-50"))
  expect_identical(.st_of(r), "RNn")
  expect_setequal(r$provisional_because, c("subtypes:RNn", "subtypes:RNr"))
  for (cd in c("RNr", "RQr")) {
    r <- .st_cls(make_ka5_example(subtype = cd, route = "data"))
    expect_identical(.st_of(r), paste0(substr(cd, 1, 2), "n"), info = cd)
    expect_true(paste0("subtypes:", cd) %in% r$provisional_because, info = cd)
  }
  # the Normrendzina with a measured base saturation under 50 %: the Sauerrendzina's formula fails, the Norm
  # holds provisionally
  r <- .st_cls(.st_mk("Ah 0-20 / Bv+cCv 20-25 / cC 25-60", bs_pct = c(30, NA, NA)))
  expect_identical(.st_type(r), "RR")
  # the Normpodsol with a measured Cp/Fep of a Humuspodsol in its Bhs
  r <- .st_cls(.st_mk("Ahe 0-10 / Ae 10-25 / Bsh 25-30 / Bhs 30-50 / C 50-100", c_pyro_pct = c(NA, NA, 1.5, 3, NA),
                      fe_pyro_pct = c(NA, NA, 0.3, 0.2, NA)))
  expect_identical(.st_of(r), "PPn")
  expect_true(all(c("subtypes:PPn", "subtypes:PPe", "subtypes:PPh") %in% r$provisional_because))
  # the Norm examples stay clean
  for (cd in c("RNn", "RQn", "RRn", "RZn", "PPn", "XXn", "DDn"))
    expect_length(.st_cls(make_ka5_example(subtype = cd))$provisional_because, 0L)
})

test_that("D7: the open subtypes: LF needs a Bt, the G in its window, never a printed code", {
  skip_on_cran()
  yu <- list(deep_cultivation_kind = "single_deep_ploughing")
  r <- .st_cls(.st_mk("R-Ap 0-30 / R+Ael 30-60 / C 60-100", site = yu))
  expect_true(is.na(.st_of(r)))
  r <- .st_cls(.st_mk("R-Ap 0-30 / R 30-50 / Bt 50-80 / C 80-100", site = yu))
  expect_true(is.na(.st_of(r)))
  expect_false(any(grepl("\\[LL-YU\\]", r$trace$subtypes_passing$code)))
  yy <- list(deep_cultivation_kind = "repeated_trenching")
  expect_identical(.st_of(.st_cls(.st_mk("R-Ap 0-30 / R 30-70 / Go 70-110", site = yy))), "[GG-YY]")
  expect_identical(.st_of(.st_cls(.st_mk("R-Ap 0-30 / R 30-80 / Go 80-130", site = yy))), "YYn")
  r <- .st_cls(.st_mk("R-Ap 0-30 / R 30-70 / Bv 70-100 / C 100-130", site = yy))
  expect_false(any(grepl("Further subtypes", r$warnings, fixed = TRUE)))
})

test_that("D8: a missing rock top leaves the Ranker undecided; the base saturation of every topsoil horizon", {
  skip_on_cran()
  p <- .st_mk("Ah 0-20 / ilCv 20-25 / imC 25-60")
  p$horizons$top_cm[3] <- NA
  r <- .st_cls(p)
  expect_true("types:RN" %in% r$provisional_because)
  # two Ah horizons, 70 and 30 %: not every one meets Ah>, the Euranker fails
  r <- .st_cls(.st_mk("Ah 0-10 / Ah 10-20 / imC 20-50", bs_pct = c(70, 30, NA)))
  expect_identical(.st_of(r), "RNn")
  expect_false(isTRUE(r$trace$subtypes_passing$passed[r$trace$subtypes_passing$code == "RNr"]))
  # one measured, one not: undecided
  r <- .st_cls(.st_mk("Ah 0-10 / Ah 10-20 / imC 20-50", bs_pct = c(70, NA, NA)))
  expect_true(is.na(r$trace$subtypes_passing$passed[r$trace$subtypes_passing$code == "RNr"]))
  expect_identical(.st_of(.st_cls(.st_mk("Ah 0-10 / Ah 10-20 / imC 20-50", bs_pct = c(70, 60, NA)))), "RNr")
})
