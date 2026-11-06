# v0.9.364: the 85 subtypes of the semiterrestrial, subhydric and Moore types of
# the German KA5 (pp. 238-262), keyed after the type (Hugo's decisions Q21, Q22,
# Q16, Q20), the Moore covers (Q7), the palaeosols (Q19) and the overlay
# splitter (Tab. 46 no. 18): one example per subtype in the KA5 notation, the
# window and cover edges, the Oxigley against the Normgley, the Rohmarsch and its
# flooding, the overlays, provisional subtypes, the data-route twins.

.wt_cls <- function(p) classify_ka5(p, on_missing = "silent")
.wt_of <- function(r) { lv <- r$levels; k <- which(lv$level == "subtypes"); if (length(k)) lv$code[k] else NA_character_ }
.wt_type <- function(r) { lv <- r$levels; k <- which(lv$level == "types"); if (length(k)) lv$code[k] else NA_character_ }
.wt_mk <- function(prof, ..., site = list()) {
  p <- soilKey:::.ka5_parse_profile_string(prof)
  n <- length(p$des)
  h <- c(list(designation = p$des, top_cm = p$top, bottom_cm = p$bot), list(...))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  PedonRecord$new(site = utils::modifyList(list(id = "ka5-wt", country = "DE", designation_system = "ka5"), site),
                  horizons = ensure_horizon_schema(data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}
.wt_codes <- function() names(soilKey:::.KA5_SUBTYPE_PROFILES_WET)
.wt_ov <- function(r) r$trace$overlay
.wt_passing <- function(r, cd) r$trace$subtypes_passing$passed[r$trace$subtypes_passing$code == cd]
.WT_TYPES <- c(AO = "A", AQ = "A", AZ = "A", AT = "A", AB = "A", GG = "G", GN = "G", GM = "G", GH = "G",
               MR = "M", MC = "M", MO = "M", MK = "M", MD = "M", MH = "M", MN = "M", "ÜA" = "Ü",
               IA = "I", IW = "I", JP = "J", JG = "J", JS = "J", JD = "J", HH = "H", HN = "H", KV = "K", KM = "K",
               KH = "K")

test_that("85 printed wet subtypes, by class, in the order of Q21; two open subtypes of Q22", {
  skip_on_cran()
  rl <- load_rules("ka5")
  v <- soilKey:::.validate_rules("ka5")
  for (k in c("missing", "unsuffixed", "mixed", "malformed", "codes")) expect_identical(v[[k]], character(0), info = k)
  wet <- rl$subtypes[intersect(names(rl$subtypes), names(.WT_TYPES))]
  codes <- unlist(lapply(wet, function(l) vapply(l, `[[`, "", "code")))
  printed <- codes[!startsWith(codes, "[")]
  expect_length(printed, 85L)
  expect_setequal(setdiff(codes, printed), c("[GHu]", "[KHm]"))
  expect_false(anyDuplicated(codes) > 0L)
  # every wet subtype of the formulas transcribed from the images, and no other
  fx <- utils::read.delim(testthat::test_path("fixtures", "ka5-formulas.tsv"), comment.char = "#",
                          quote = "", colClasses = "character", encoding = "UTF-8")
  fx <- fx[fx$kind == "subtype" & fx$class %in% c("A", "G", "M", "Ü", "I", "J", "H", "K"), ]
  expect_setequal(printed, unique(fx$unit))
  # per class (the reading map's count, p. 199 and the type texts): A 12, G 40, M 15, Ue 3, I 3, J 0, H 4,
  # K 7 and the unit KV-KM
  n <- vapply(names(wet), function(t) sum(!startsWith(vapply(wet[[t]], `[[`, "", "code"), "[")), 0L)
  per <- tapply(n, .WT_TYPES[names(n)], sum)
  expect_identical(as.integer(per[c("A", "G", "M", "Ü", "I", "H", "K")]), c(12L, 40L, 15L, 3L, 3L, 4L, 8L))
  for (t in c("IA", "JP", "JG", "JS", "JD")) expect_null(rl$subtypes[[t]], info = t)
  kinds <- c(transition = 1L, deviation = 2L, norm = 3L)
  for (t in names(wet)) {
    l <- wet[[t]]
    expect_false(is.unsorted(kinds[vapply(l, `[[`, "", "kind")]), info = t)
    for (e in l) {
      expect_true(all(nzchar(c(e$name, e$name_en, e$name_pt, e$page))), info = e$code)
      if (!startsWith(e$code, "[") && e$kind == "transition") expect_true(endsWith(e$code, paste0("-", t)), info = e$code)
      for (f in e$formulas) {
        fm <- if (is.list(f)) f$formula else f
        expect_length(soilKey:::.ka5_parse_formula(fm)$unknown, 0L)
      }
      for (w in c(e$windows, unlist(lapply(e$formulas, function(f) if (is.list(f)) f$windows), recursive = FALSE)))
        expect_true(is.numeric(soilKey:::.ka5_thr(w$thr)), info = e$code)
    }
    # the Norm last, but in the Moorgley (n is Nieder- there, Tab. 49: no Norm)
    if (t == "GH") expect_false(any(vapply(l, `[[`, "", "kind") == "norm"))
    else expect_identical(l[[length(l)]]$kind, "norm", info = t)
  }
  expect_identical(vapply(rl$subtypes[["GH"]], `[[`, "", "code"), c("GHg", "GHq", "GHh", "[GHu]", "GHn"))
  expect_identical(vapply(rl$subtypes[["KM"]], `[[`, "", "code"), c("KV-KM", "KMc", "KMu", "KMn"))
  expect_identical(vapply(rl$subtypes[["AB"]], `[[`, "", "code"), c("AQ-AB", "SS-AB", "GG-AB", "ABn"))
  gg <- vapply(rl$subtypes[["GG"]], `[[`, "", "code")
  expect_identical(gg[1:2], c("RQ-GG", "RR-GG"))
  expect_length(gg, 27L)
  expect_identical(gg[18:27], c("GGx", "GGe", "GGi", "GGw", "GGc", "GGh", "GGg", "GGq", "GGa", "GGn"))
  # the KMc's nHcw printed twice is read once (slip 36)
  kmc <- Filter(function(e) e$code == "KMc", rl$subtypes[["KM"]])[[1]]
  expect_identical(unlist(kmc$formulas), "nHcm/nHca/(nHcw/)(nHcr/)(IIfF/)...")
})

test_that("each wet subtype example keys to its type and subtype by the notation", {
  skip_on_cran()
  ent <- soilKey:::.ka5_subtype_entries()
  expect_length(.wt_codes(), 87L)
  for (cd in .wt_codes()) {
    p <- make_ka5_example(subtype = cd)
    expect_identical(p$site$designation_system, "ka5")
    r <- .wt_cls(p)
    expect_identical(.wt_type(r), ent[[cd]]$type, info = cd)
    expect_identical(.wt_of(r), cd, info = cd)
    expect_identical(r$name, ent[[cd]]$name, info = cd)
    expect_length(r$provisional_because, 0L)
    expect_true(cd %in% r$trace$subtypes_passing$code[r$trace$subtypes_passing$passed %in% TRUE], info = cd)
  }
  # the Strand's subtypes also by an ASCII code
  expect_identical(.wt_of(.wt_cls(make_ka5_example(subtype = "UEAb"))), "ÜAb")
  expect_identical(.wt_cls(make_ka5_example(subtype = "KV-KM"))$name, "Erdniedermoor-Mulmniedermoor")
  expect_identical(.wt_cls(make_ka5_example(subtype = "[GHu]"))$name, "[Übergangsmoorgley]")
})

test_that("the wet type examples key to their Norm subtype; the Moorgley to GHn; no subtype in J and IA", {
  skip_on_cran()
  for (t in c("KV", "KM", "KH", "HH", "HN", "IW", "MR", "MC", "MO", "MK", "MD", "MH", "MN", "GM", "GN", "GG",
              "AO", "AT", "AQ", "AZ", "AB")) {
    r <- .wt_cls(make_ka5_example(t))
    expect_identical(.wt_of(r), paste0(t, "n"), info = t)
    expect_length(r$provisional_because, 0L)
  }
  expect_identical(.wt_of(.wt_cls(make_ka5_example("UEA"))), "ÜAn")
  expect_identical(.wt_of(.wt_cls(make_ka5_example("GH"))), "GHn")
  for (t in c("JP", "JG", "JS", "JD", "IA")) {
    r <- .wt_cls(make_ka5_example(t))
    expect_true(is.na(.wt_of(r)), info = t)
    expect_false(any(grepl("No subtype of", r$warnings, fixed = TRUE)), info = t)
  }
  r <- .wt_cls(make_ka5_example(subtype = "BB-GG"))
  expect_identical(r$levels$name[4], "Braunerde-Gley")
  expect_identical(r$levels$name_en[4], "Brown earth-gley")
})

test_that("the window edges: the aGo at 4 and 8 dm, the aG of the Norm, the humic 4 dm, the S of the Pseudogley-Vega", {
  skip_on_cran()
  ga <- function(z) .wt_cls(.wt_mk(sprintf("aAi 0-3 / alC 3-%d / aGo %d-120 / aG 120-150", z, z), site = list(landform = "floodplain")))
  expect_identical(.wt_of(ga(40)), "GG-AO")
  expect_identical(.wt_of(ga(79)), "GG-AO")
  expect_identical(.wt_of(ga(80)), "AOn")
  expect_identical(.wt_type(ga(39)), "GG")
  # the aG of the Normrambla 8 dm or deeper: absent from a description reaching 8 dm counts, a shallower
  # description leaves it undecided
  r <- .wt_cls(.wt_mk("aAi 0-3 / alC 3-100", site = list(landform = "floodplain")))
  expect_identical(.wt_of(r), "AOn")
  expect_length(r$provisional_because, 0L)
  r <- .wt_cls(.wt_mk("aAi 0-3 / alC 3-60", site = list(landform = "floodplain")))
  expect_identical(.wt_of(r), "AOn")
  expect_true(all(c("subtypes:GG-AO", "subtypes:AOn") %in% r$provisional_because))
  # the humic part of the Humusgley reaches 4 dm
  gh <- function(z) .wt_of(.wt_cls(.wt_mk(sprintf("Ah 0-20 / Ah-Go 20-%d / Go %d-70 / Gr 70-120", z, z))))
  expect_identical(gh(40), "GGh")
  expect_identical(gh(39), "GGn")
  # the Pseudogley-Vega: the aSw at 4 to < 8 dm, the aG at 8 dm or deeper
  sv <- function(z, g = 120) .wt_of(.wt_cls(.wt_mk(sprintf("aAh 0-20 / aM 20-%d / aSw %d-%d / aSd %d-%d / aG %d-150", z, z, z + 10, z + 10, g, g),
                                                   site = list(landform = "floodplain"))))
  expect_identical(sv(40), "SS-AB")
  expect_identical(sv(69, 85), "SS-AB")
  expect_identical(sv(80, 95), "ABn")
  expect_false(identical(sv(40, 75), "SS-AB"))
  # the second form: the a(M-)Sd at 4 to < 8 dm
  expect_identical(.wt_of(.wt_cls(.wt_mk("aSw-Ah 0-20 / aSw-M 20-50 / aM-Sd 50-80 / aG 90-150", site = list(landform = "floodplain")))), "SS-AB")
  # the Gley-Tschernitza: the aGo of the second form inside the Axh
  expect_identical(.wt_of(.wt_cls(.wt_mk("aAxh 0-45 / aGo-Axh 45-60 / aG 60-150", site = list(landform = "floodplain")))), "GG-AT")
})

test_that("the Moore covers (Q7): 19/20, 29/30, 39/40 cm", {
  skip_on_cran()
  cv <- function(z) .wt_cls(.wt_mk(sprintf("Ah 0-%d / Go %d-%d / IIrnHw %d-90 / nHr 90-150", z %/% 2, z %/% 2, z, z),
                                   caco3_pct = 0))
  r <- cv(19)
  expect_identical(.wt_type(r), "HN")
  expect_identical(.wt_ov(r)$kind, "moor_cover")
  expect_true(is.na(.wt_ov(r)$code))
  expect_true(any(grepl("substrate only", r$warnings, fixed = TRUE)))
  for (z in c(20, 29)) {
    r <- cv(z)
    expect_identical(.wt_type(r), "HN")
    expect_identical(.wt_of(r), "HNn")
    expect_identical(.wt_ov(r)$code, "GG\\HN")
    expect_identical(.wt_ov(r)$name, "flacher Gley über Niedermoor")
    expect_identical(.wt_ov(r)$cover_cm, z)
  }
  for (z in c(30, 39)) {
    r <- cv(z)
    expect_identical(.wt_ov(r)$code, "GG/HN")
    expect_identical(.wt_ov(r)$name, "Gley über Niedermoor")
    expect_true(any(grepl("GG/HN", r$warnings, fixed = TRUE)))
  }
  r <- cv(40)
  expect_identical(.wt_type(r), "GG")
  expect_identical(.wt_ov(r)$kind, "peat_below")
  expect_true(is.na(.wt_ov(r)$code))
  # an Organomarsch over a Niedermoor
  mo <- function(z) .wt_cls(.wt_mk(sprintf("tmoAh 0-%d / tmoGo %d-%d / IIrnHw %d-90 / IIIfF 90-110", z - 10, z - 10, z, z),
                                   oc_pct = c(6, 1, 45, 3), clay_pct = c(50, 50, NA, 20), caco3_pct = 0))
  expect_identical(.wt_ov(mo(25))$code, "MO\\HN")
  expect_identical(.wt_ov(mo(25))$name, "flache Organomarsch über Niedermoor")
  expect_identical(.wt_ov(mo(35))$code, "MO/HN")
  # the overlay does not change the levels, the name or provisional_because
  r <- cv(25)
  expect_identical(r$name, "Normniedermoor")
  expect_length(r$provisional_because, 0L)
})

test_that("the palaeosols (Q19) and the buried soils (Tab. 46 no. 18)", {
  skip_on_cran()
  pv <- function(z, b = "Bj") .wt_cls(.wt_mk(sprintf("Ah 0-10 / Bv 10-%d / IIr%s %d-%d / IICj %d-150", z, b, z, z + 30, z + 30)))
  r <- pv(25)
  expect_identical(.wt_type(r), "BB")
  expect_identical(.wt_ov(r)$code, "BB\\VV")
  expect_identical(.wt_ov(r)$name, "flache Braunerde über Fersiallit")
  expect_identical(.wt_ov(pv(30, "Bu"))$code, "BB/VW")
  expect_identical(.wt_ov(pv(69))$code, "BB/VV")
  r <- pv(70)
  expect_true(is.na(.wt_ov(r)$code))
  expect_false(any(grepl("Overlay", r$warnings, fixed = TRUE)))
  # the palaeosol type examples stay the V types; a Cj alone is ignored
  expect_null(.wt_ov(.wt_cls(make_ka5_example("VV"))))
  expect_null(.wt_ov(.wt_cls(.wt_mk("Ah 0-10 / Bv 10-40 / IICj 40-100"))))
  expect_identical(.wt_ov(.wt_cls(make_ka5_example("VV", near_miss = TRUE)))$code, "BB/VV")
  # a Kolluvisol over a buried Braunerde (YK/BB, p. 272)
  r <- .wt_cls(.wt_mk("Ah 0-20 / M 20-50 / IIfAh 50-70 / IIBv 70-100 / IIC 100-130"))
  expect_identical(.wt_type(r), "YK")
  expect_identical(.wt_ov(r)$code, "YK/BB")
  expect_identical(.wt_ov(r)$name, "Kolluvisol über Braunerde")
  expect_true(is.na(.wt_ov(.wt_cls(make_ka5_example("YK")))$code))
  # the Organomarsch over a Podsol: 1.5 to < 3 and 3 to < 7 dm (p. 254)
  mp <- function(z) .wt_cls(.wt_mk(sprintf("tmoAh 0-%d / tmoGo %d-%d / IIfAe°Go %d-%d / IIfBsh°Go %d-%d / IIGr %d-150",
                                           z %/% 2, z %/% 2, z, z, z + 15, z + 15, z + 30, z + 30),
                                   oc_pct = c(6, 1, 0.3, 2, 0.3), clay_pct = c(50, 50, 3, 3, 10), caco3_pct = 0))
  expect_identical(.wt_type(mp(20)), "MO")
  expect_identical(.wt_ov(mp(20))$code, "MO\\PP")
  expect_identical(.wt_ov(mp(15))$code, "MO\\PP")
  expect_identical(.wt_ov(mp(45))$code, "MO/PP")
  expect_true(is.na(.wt_ov(mp(14))$code))
  # a Hochmoor over a Podsol (p. 268)
  r <- .wt_cls(.wt_mk("hHw 0-50 / hHr 50-60 / IIfAe 60-70 / IIfBhs 70-90 / IIC 90-120", ph_cacl2 = c(3.4, 3.4, NA, NA, NA)))
  expect_identical(.wt_ov(r)$code, "HH/PP")
  # the Dwogmarsch is no overlay; the F under the Niedermoor is no buried soil
  expect_null(.wt_ov(.wt_cls(make_ka5_example("MD"))))
  expect_null(.wt_ov(.wt_cls(make_ka5_example("HN"))))
})

test_that("GGx and GGn (Q16)", {
  skip_on_cran()
  expect_identical(.wt_of(.wt_cls(.wt_mk("Ah 0-25 / Go 25-60 / Gr 60-120"))), "GGn")
  expect_identical(.wt_of(.wt_cls(.wt_mk("Ah 0-25 / Go 25-130"))), "GGx")
  # described to under 13 dm with no Gr: undecided between the two, GGn provisionally, GGx a candidate
  r <- .wt_cls(.wt_mk("Ah 0-25 / Go 25-129"))
  expect_identical(.wt_of(r), "GGn")
  expect_true(all(c("subtypes:GGx", "subtypes:GGn") %in% r$provisional_because))
  expect_true(is.na(.wt_passing(r, "GGx")))
  # the oxygen-rich groundwater recorded decides
  expect_identical(.wt_of(.wt_cls(.wt_mk("Ah 0-25 / Go 25-100", site = list(groundwater_oxygen_rich = TRUE)))), "GGx")
  r <- .wt_cls(.wt_mk("Ah 0-25 / Go 25-140", site = list(groundwater_oxygen_rich = FALSE)))
  expect_identical(.wt_of(r), "GGn")
  expect_false(isTRUE(.wt_passing(r, "GGx")))
  # a Gor is no Gr: the Wechselgley
  expect_identical(.wt_of(.wt_cls(.wt_mk("Ah 0-25 / Go 25-90 / Gor 90-150"))), "GGw")
  # a peat under the Go does not count as the described mineral profile
  r <- .wt_cls(.wt_mk("Ah 0-20 / Go 20-60 / IInHr 60-150", oc_pct = c(3, 0.5, 45)))
  expect_identical(.wt_of(r), "GGn")
  expect_true("subtypes:GGx" %in% r$provisional_because)
})

test_that("the Rohmarsch and its flooding (Q20)", {
  skip_on_cran()
  mr <- "tmzeGo-Ah 0-10 / tmzeGo 10-30 / tmzeGr 30-100"
  r <- .wt_cls(.wt_mk(mr))
  expect_identical(.wt_type(r), "MR")
  expect_identical(.wt_of(r), "MRn")
  expect_length(r$provisional_because, 0L)
  # behind a dyke, or not tidally flooded: no Rohmarsch, a Kalkmarsch by its carbonate
  for (s in list(list(embanked = TRUE), list(tidal_inundation = FALSE))) {
    r <- .wt_cls(.wt_mk(mr, site = s))
    expect_identical(.wt_type(r), "MC")
    expect_false(identical(substr(.wt_of(r), 1, 2), "MR"))
  }
  expect_identical(.wt_of(.wt_cls(.wt_mk(mr, site = list(tidal_inundation = TRUE)))), "MRn")
  expect_identical(.wt_of(.wt_cls(.wt_mk("tbGo-Ah 0-10 / tbGo 10-30 / tbGr 30-100"))), "MRb")
  # the type example's near miss (embanked) keys a Kalkmarsch subtype
  expect_identical(.wt_of(.wt_cls(make_ka5_example("MR", near_miss = TRUE))), "MCn")
})

test_that("provisional wet subtypes and gates mended for the subtypes", {
  skip_on_cran()
  # a G written without the prefix s on a slope of 9 % or more: undecided (R2), GGg a candidate
  r <- .wt_cls(.wt_mk("Ah 0-25 / Go 25-60 / Gr 60-120", site = list(slope_pct = 12)))
  expect_identical(.wt_of(r), "GGn")
  expect_identical(r$provisional_because, "subtypes:GGg")
  r <- .wt_cls(.wt_mk("Ah 0-25 / Go 25-60 / Gr 60-120", site = list(groundwater_source = "spring_water")))
  expect_identical(r$provisional_because, "subtypes:GGq")
  # on the data with the source of the water not recorded: the Hang- and Quellengley candidates
  r <- .wt_cls(make_ka5_example("GG", route = "data"))
  expect_identical(.wt_of(r), "GGn")
  expect_true(all(c("subtypes:GGg", "subtypes:GGq") %in% r$provisional_because))
  # the pH of the Normniedermoor (4 to < 6) measured at 6.5: the Norm provisionally, HNc and HNu candidates
  r <- .wt_cls(.wt_mk("nHw 0-50 / nHr 50-120 / IIfF 120-140", ph_cacl2 = c(6.5, 6.5, NA)))
  expect_identical(.wt_of(r), "HNn")
  expect_true(all(c("subtypes:HNn", "subtypes:HNc", "subtypes:HNu") %in% r$provisional_because))
  # the Kalkniedermoor's subsoil pH under 6: no HNc
  r <- .wt_cls(.wt_mk("nHcw 0-40 / nHcr 40-120 / IIfF 120-140", ph_cacl2 = c(6.5, 5.5, NA)))
  expect_false(isTRUE(.wt_passing(r, "HNc")))
  expect_identical(.wt_of(.wt_cls(.wt_mk("nHcw 0-40 / nHcr 40-120 / IIfF 120-140", ph_cacl2 = c(5.8, 6.5, NA)))), "HNc")
  # the Paternia-Vega is a Vega (the Paternia gate mended, p. 241)
  r <- .wt_cls(make_ka5_example(subtype = "AQ-AB"))
  expect_identical(.wt_type(r), "AB")
  expect_identical(.wt_type(.wt_cls(make_ka5_example("AQ"))), "AQ")
  # the terrestrial X-Gley transitions do not take floodplain horizons (p. 246)
  r <- .wt_cls(make_ka5_example(subtype = "AQ-GG"))
  expect_false(isTRUE(.wt_passing(r, "RQ-GG")))
  # a Hang-Niedermoorgley: the Hangmoorgley names it, the Niedermoorgley passes too (R1)
  r <- .wt_cls(.wt_mk("nH 0-20 / IIsGr 20-100", oc_pct = c(45, 0.3), site = list(slope_pct = 12)))
  expect_identical(.wt_of(r), "GHg")
  expect_true(isTRUE(.wt_passing(r, "GHn")))
  # the vermulmt Erdhochmoor (Q22)
  expect_identical(.wt_of(.wt_cls(.wt_mk("hHvm 0-15 / hHw 15-60 / hHr 60-150"))), "[KHm]")
})

# ---- the review of 2026-11-06 (Hugo: D1-D6, R1-R4) ---------------------------------------------

test_that("D1: only an aM above 8 dm takes the profile out of the Paternia; aM bands stay", {
  skip_on_cran()
  fp <- list(landform = "floodplain")
  expect_identical(.wt_type(.wt_cls(.wt_mk("aAh 0-20 / ailC 20-40 / IIM 40-90", site = fp))), "AQ")
  expect_identical(.wt_type(.wt_cls(.wt_mk("aAh 0-20 / ailC 20-150 / IIaM 150-170", site = fp))), "AQ")
  expect_identical(.wt_type(.wt_cls(.wt_mk("aAh 0-20 / ailC+aM 20-60 / aG 90-150", site = fp))), "AQ")
  expect_identical(.wt_type(.wt_cls(.wt_mk("aAh 0-20 / ailC 20-35 / IIaM 35-79 / IIaG 90-150", site = fp))), "AB")
  expect_identical(.wt_of(.wt_cls(make_ka5_example(subtype = "AQ-AB"))), "AQ-AB")
})

test_that("D2, D3: the Gley-Paternia's aG under the description; the Wechselgley takes no q", {
  skip_on_cran()
  fp <- list(landform = "floodplain")
  expect_identical(.wt_of(.wt_cls(.wt_mk("aAh 0-20 / ailC 20-50 / aGo 50-120", site = fp))), "GG-AQ")
  expect_identical(.wt_of(.wt_cls(.wt_mk("aAi 0-3 / alC 3-50 / aGo 50-120", site = fp))), "GG-AO")
  expect_identical(.wt_of(.wt_cls(.wt_mk("Ah 0-20 / qGo 20-60 / qGor 60-120"))), "GGq")
  expect_identical(.wt_of(.wt_cls(.wt_mk("Ah 0-20 / Go 20-60 / Gor 60-120"))), "GGw")
})

test_that("D4, R1: the Moorgley's peat of no known kind; the water source names the subtype", {
  skip_on_cran()
  r <- .wt_cls(.wt_mk("H 0-20 / IIGr 20-100"))
  expect_identical(.wt_type(r), "GH")
  expect_true(is.na(.wt_of(r)))
  expect_true(all(c("subtypes:GHn", "subtypes:GHh", "subtypes:[GHu]") %in% r$provisional_because))
  p <- make_ka5_example(subtype = "GHn", route = "data")
  p$horizons$peat_moor_type <- NA
  r <- .wt_cls(p)
  expect_true(is.na(.wt_of(r)))
  expect_true(all(c("subtypes:GHn", "subtypes:GHh", "subtypes:[GHu]") %in% r$provisional_because))
  expect_identical(.wt_of(.wt_cls(.wt_mk("hH 0-20 / IIqGr 20-100", oc_pct = c(45, 0.3)))), "GHq")
  expect_identical(.wt_of(.wt_cls(.wt_mk("H 0-20 / IIsGr 20-100", oc_pct = c(45, 0.3)))), "GHg")
  expect_identical(.wt_of(.wt_cls(make_ka5_example("GH"))), "GHn")
})

test_that("D5: the Normhochmoor's pH in the hH(w)", {
  skip_on_cran()
  r <- .wt_cls(.wt_mk("hHw 0-60 / hHr 60-150", ph_cacl2 = c(3.6, 4.2)))
  expect_identical(.wt_of(r), "HHn")
  expect_length(r$provisional_because, 0L)
  r <- .wt_cls(.wt_mk("hHw 0-60 / hHr 60-150", ph_cacl2 = c(4.2, 3.6)))
  expect_false(identical(.wt_type(r), "HH"))
  expect_identical(.wt_type(.wt_cls(.wt_mk("hHr 0-150", ph_cacl2 = 3.6))), "HH")
})

test_that("D6: on the data route a horizon among the candidates, or one the data key does not build, is undecided", {
  skip_on_cran()
  dt <- function(cd) .wt_cls(make_ka5_example(subtype = cd, route = "data"))
  expect_true("subtypes:BB-GG" %in% dt("BB-GG")$provisional_because)
  expect_true("subtypes:AB-GG" %in% dt("AB-GG")$provisional_because)
  expect_true("subtypes:GNh" %in% dt("GNh")$provisional_because)
  expect_true("subtypes:KV-KM" %in% dt("KV-KM")$provisional_because)
  expect_true("subtypes:GGe" %in% dt("GGe")$provisional_because)
  p <- make_ka5_example(subtype = "GGn", route = "data")
  p$horizons$caco3_pct <- c(0, 15, 15)
  r <- .wt_cls(p)
  expect_identical(.wt_of(r), "GGn")
  expect_true("subtypes:GGc" %in% r$provisional_because)
  # in the notation the same profiles fail cleanly
  expect_length(.wt_cls(make_ka5_example(subtype = "GGn"))$provisional_because, 0L)
})

test_that("R2, R3, R4: a covered Erdniedermoor; the Normniedermoor's pH; Marsch prefixes", {
  skip_on_cran()
  r <- .wt_cls(.wt_mk("Ah 0-15 / Go 15-35 / IInHv 35-60 / nHt 60-80 / nHw 80-150", caco3_pct = 0))
  expect_identical(.wt_type(r), "KV")
  expect_identical(.wt_ov(r)$code, "GG/KV")
  expect_identical(.wt_of(.wt_cls(.wt_mk("nHw 0-50 / nHr 50-120", ph_cacl2 = c(5.9, 5.9)))), "HNn")
  expect_true("subtypes:HNn" %in% .wt_cls(.wt_mk("nHw 0-50 / nHr 50-120", ph_cacl2 = c(6, 6)))$provisional_because)
  r <- .wt_cls(.wt_mk("tmAh 0-20 / tmGo 20-60 / tmGr 60-120", caco3_pct = 0))
  expect_identical(.wt_of(r), "MNn")
  expect_true("subtypes:MNn" %in% r$provisional_because)
})

test_that("the data-route twins key to their subtype where the data route reaches it", {
  skip_on_cran()
  dr <- soilKey:::.KA5_DATA_ROUTE_SUBTYPES_WET
  expect_true(length(dr) >= 55L)
  expect_true(all(dr %in% .wt_codes()))
  for (cd in dr) {
    p <- make_ka5_example(subtype = cd, route = "data")
    expect_true(all(is.na(p$horizons$designation)))
    r <- .wt_cls(p)
    expect_identical(.wt_of(r), cd, info = cd)
    expect_true(all(r$trace$horizons$route == "data"), info = cd)
  }
})

test_that("the KA5 wet subtype and overlay sources are ASCII", {
  skip_on_cran()
  src <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(src), "package sources not available")
  for (fn in c("subtypes-ka5.R", "examples-ka5-subtypes.R", "overlays-ka5.R")) {
    txt <- readLines(file.path(src, fn), warn = FALSE)
    expect_false(any(grepl("[^\\x01-\\x7F]", txt, perl = TRUE)), info = fn)
  }
})
