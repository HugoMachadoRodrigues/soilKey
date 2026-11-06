# v0.9.359: the South African soil classification of 1991, part 5: the
# families. families.yaml (404 families of the 73 forms, written by
# data-raw/za1991_families.R), the 19 family criteria (R/families-za.R), the
# partial code of an undecided family, the name of Q23, and the examples of
# every family (make_za1991_example(family = )).

.ex <- function(form, family = NULL, notation = FALSE) make_za1991_example(form, family = family, notation = notation)
.cl <- function(p) classify_za1991(p, on_missing = "silent")
# a pedon with some attributes of one layer changed
.mod <- function(p, i, ...) {
  h <- as.data.frame(p$horizons)
  a <- list(...)
  for (k in names(a)) {
    if (!k %in% names(h)) h[[k]] <- NA
    h[[k]][i] <- a[[k]]
  }
  old <- options(soilKey.warn_unrecognised = FALSE)
  on.exit(options(old))
  PedonRecord$new(site = p$site, horizons = ensure_horizon_schema(data.table::as.data.table(h)))
}
# the record of a criterion in a pedon's view
.fc <- function(p, criterion, part, horizon) {
  soilKey:::.za_family_class(soilKey:::.za_view(p), criterion, part, horizon)
}
.fam <- function() soilKey:::.za_families_file()


test_that("families.yaml: 404 families of the 73 forms, codes of the form and four digits", {
  skip_on_cran()
  f <- .fam()
  expect_identical(length(f$family_criteria), 19L)
  expect_identical(vapply(f$family_criteria, function(c) as.integer(c$number), 0L), 1:19)
  fs <- soilKey:::.za_forms()
  in_key <- names(fs)[!vapply(fs, function(z) z$outside, NA)]
  expect_setequal(names(f$families), in_key)
  expect_false("Lc" %in% names(f$families))
  expect_identical(sum(lengths(f$families)), 404L)
  n <- lengths(f$families)
  expect_identical(as.integer(table(n)[c("2", "4", "6", "8", "12", "16")]), c(22L, 20L, 8L, 16L, 2L, 5L))
  for (code in names(f$families)) {
    fam <- f$families[[code]]
    codes <- vapply(fam, `[[`, "", "code")
    expect_true(all(grepl(paste0("^", gsub("([][])", "\\\\\\1", code), "[0-3]{4}$"), codes)), info = code)
    expect_false(anyDuplicated(codes) > 0L, info = code)
    expect_true(all(nzchar(vapply(fam, `[[`, "", "name"))), info = code)
    # every digit of every family has its class in family_digits, and the
    # family tests are the conditions of those classes
    dg <- f$family_digits[[code]]
    for (e in fam) {
      d <- as.integer(strsplit(e$digits, "")[[1]])
      conds <- unlist(lapply(seq_along(dg), function(p) dg[[p]]$classes[[d[p]]]), recursive = FALSE)
      expect_identical(lapply(e$tests$all_of, `[[`, "family_class_za"), conds, info = e$code)
      expect_true(all(d[-seq_along(dg)] == 0L), info = e$code)
    }
  }
  # the rule set: families under the forms, predicates of the namespace
  res <- soilKey:::.validate_rules("za1991")
  expect_identical(res$missing, character(0))
  expect_identical(res$unsuffixed, character(0))
  expect_identical(res$codes, character(0))
  expect_identical(res$malformed, character(0))
  r <- load_rules("za1991")
  expect_identical(length(r$families), 73L)
  expect_identical(soilKey:::.system_get("za1991")$levels, c("forms", "families"))
})

test_that("every one of the 404 leaves is reachable from a synthetic class vector", {
  skip_on_cran()
  f <- .fam()
  rules <- load_rules("za1991")
  p <- .ex("hutton")
  key <- function(c) sprintf("%d|%s|%s", as.integer(c$criterion), c$part, c$horizon)
  rec <- function(c, class) list(criterion = as.integer(c$criterion), name = "x", horizon = c$horizon, part = c$part, what = "synthetic",
                                 layers = integer(0), class = class, candidates = if (is.na(class)) soilKey:::.za_fc_classes(c$criterion) else character(0),
                                 missing = if (is.na(class)) "synthetic" else character(0), note = character(0), tag = character(0))
  st <- soilKey:::.za_state
  old <- list(view = st$view, pedon = st$pedon)
  on.exit({ st$view <- old$view; st$pedon <- old$pedon }, add = TRUE)
  reached <- 0L
  for (code in names(f$families)) {
    taxa <- rules$families[[code]]
    all_conds <- unlist(lapply(f$family_digits[[code]], function(d) unlist(d$classes, recursive = FALSE)), recursive = FALSE)
    for (e in taxa) {
      env <- new.env(parent = emptyenv())
      for (c in all_conds) assign(key(c), rec(c, "synthetic_none"), envir = env)
      mine <- lapply(e$tests$all_of, `[[`, "family_class_za")
      for (c in mine) assign(key(c), rec(c, as.character(unlist(c$class))[1L]), envir = env)
      st$view <- list(fc = env)
      st$pedon <- p
      r <- soilKey:::.run_level(p, taxa, "families")
      expect_identical(r$assigned$code, e$code, info = e$code)
      if (identical(r$assigned$code, e$code)) reached <- reached + 1L
      # the first criterion of the family undecided: no family, this one
      # among the undecided ones
      c1 <- mine[[1L]]
      assign(key(c1), rec(c1, NA_character_), envir = env)
      r2 <- soilKey:::.run_level(p, taxa, "families")
      expect_null(r2$assigned, info = e$code)
      expect_true(e$code %in% r2$undecided, info = e$code)
    }
  }
  expect_identical(reached, 404L)
})

test_that("every family keys from its example profile, and each criterion reads its class there", {
  skip_on_cran()
  f <- .fam()
  fs <- soilKey:::.za_forms()
  rules <- load_rules("za1991")
  seen <- character(0)
  n <- 0L
  for (code in names(f$families)) {
    for (e in f$families[[code]]) {
      p <- .ex(tolower(fs[[code]]$name), family = e$code)
      r <- classify_za1991(p, rules = rules, on_missing = "silent")
      expect_identical(r$rsg_or_order, fs[[code]]$name, info = e$code)
      expect_identical(r$levels$code, c(code, e$code), info = e$code)
      expect_identical(r$provisional_because, character(0), info = e$code)
      if (identical(r$levels$code, c(code, e$code))) n <- n + 1L
      v <- soilKey:::.za_view(p)
      for (cnd in lapply(e$tests$all_of, `[[`, "family_class_za")) {
        rc <- soilKey:::.za_family_class(v, cnd$criterion, cnd$part, cnd$horizon)
        expect_true(rc$class %in% as.character(unlist(cnd$class)), info = paste(e$code, cnd$criterion))
        seen <- c(seen, paste(cnd$criterion, rc$class))
      }
    }
  }
  expect_identical(n, 404L)
  # every class of every criterion is read on an example
  all_cls <- unlist(lapply(f$family_criteria, function(c) paste(c$number, unlist(c$classes))))
  expect_setequal(unique(seen), all_cls)
})

test_that("the families by the notation: a family of each form", {
  skip_on_cran()
  f <- .fam()
  fs <- soilKey:::.za_forms()
  for (code in names(f$families)) {
    e <- f$families[[code]][[length(f$families[[code]])]]
    r <- .cl(.ex(tolower(fs[[code]]$name), family = e$code, notation = TRUE))
    expect_identical(r$levels$code, c(code, e$code), info = e$code)
  }
})

test_that("the name of Q23: form, family code and name, texture of the A", {
  skip_on_cran()
  r <- .cl(.ex("hutton", "Hu1200"))
  expect_identical(r$name, "Hutton, Hu1200 Kelvin, sandy loam")
  expect_identical(r$rsg_or_order, "Hutton")
  expect_identical(r$levels$level, c("forms", "families"))
  expect_identical(r$levels$name, c("Hutton", "Kelvin"))
  expect_identical(r$qualifiers$za_family$code, "Hu1200")
  expect_identical(r$qualifiers$za_family$texture, "sandy loam")
  expect_true(is.data.frame(r$qualifiers$za_family$criteria))
  expect_setequal(r$qualifiers$za_family$criteria$criterion, c(8L, 10L))
  # the sand grade only with sand_grade
  p <- .mod(.ex("hutton", "Hu1200"), 1, sand_grade = "medium")
  expect_identical(.cl(p)$name, "Hutton, Hu1200 Kelvin, medium sandy loam")
  # no texture without clay, silt and sand
  p2 <- .mod(.ex("hutton", "Hu1200"), 1, silt_pct = NA)
  r2 <- .cl(p2)
  expect_identical(r2$name, "Hutton, Hu1200 Kelvin")
  expect_true(any(grepl("texture of the A is not in the name", r2$warnings)))
  # a proper name: Inanda's code in soilKey's brackets
  expect_match(.cl(.ex("inanda"))$name, "^Inanda, \\[Ia\\]1100 Himeville")
})

test_that("partial codes: an x for each undecided digit", {
  skip_on_cran()
  # the cations of the B1 not recorded: the base status undecided
  p <- .mod(.ex("hutton", "Hu2200"), 2, ca_cmol = NA, mg_cmol = NA, k_cmol = NA, na_cmol = NA)
  r <- .cl(p)
  expect_identical(r$name, "Hutton, [Hux200], sandy loam")
  expect_identical(r$qualifiers$za_family$partial, "Hux200")
  expect_true(is.na(r$qualifiers$za_family$code))
  expect_identical(nrow(r$levels), 1L)
  expect_setequal(r$provisional_because, c("families:Hu1200", "families:Hu2200", "families:Hu3200"))
  expect_true(any(grepl("The family is undecided (Hux200", r$warnings, fixed = TRUE)))
  cr <- r$qualifiers$za_family$criteria
  expect_true(is.na(cr$class[cr$criterion == 8L]))
  expect_match(cr$missing[cr$criterion == 8L], "ca_cmol")
  # two digits undecided
  p2 <- .mod(p, 2, clay_pct = NA)
  expect_identical(.cl(p2)$qualifiers$za_family$partial, "Huxx00")
  # a four-digit family
  p3 <- .mod(.ex("valsrivier", "Va2122"), 2, structure_size = NA)
  expect_identical(.cl(p3)$qualifiers$za_family$partial, "Va21x2")
  # an A whose dry colour is not recorded may be melanic: no form, no family
  p4 <- .mod(.ex("valsrivier", "Va2122"), 1, munsell_value_dry = NA, munsell_chroma_dry = NA)
  expect_null(.cl(p4)$qualifiers$za_family)
  # Lichtenburg has no family
  lc <- .cl(.ex("lichtenburg"))
  expect_null(lc$qualifiers$za_family)
  expect_identical(lc$name, "Lichtenburg")
})

test_that("the slips of the family table", {
  skip_on_cran()
  f <- .fam()
  py <- vapply(f$families[["[Py]"]], `[[`, "", "code")
  expect_identical(py, c("[Py]1000", "[Py]2000"))
  expect_match(f$families[["[Py]"]][[1L]]$note, "slip 1")
  # the Shortlands 3000 families read the luvic criterion on the B1
  sd3 <- Filter(function(e) startsWith(e$digits, "3"), f$families$Sd)
  expect_identical(length(sd3), 4L)
  for (e in sd3) {
    c10 <- Filter(function(c) c$criterion == 10L, lapply(e$tests$all_of, `[[`, "family_class_za"))
    expect_identical(c10[[1L]]$part, "B1")
    c16 <- Filter(function(c) c$criterion == 16L, lapply(e$tests$all_of, `[[`, "family_class_za"))
    expect_identical(c16[[1L]]$class, "calcareous")
    expect_match(e$note, "slip 6")
  }
  # the "carbonate horizon" of Etosha, Addo and Brandvlei is the soft
  # carbonate horizon, as in Molopo and Kimberly
  for (code in c("Et", "Ad", "Br", "Mp", "Ky")) {
    conds <- unlist(lapply(f$families[[code]], function(e) lapply(e$tests$all_of, `[[`, "family_class_za")), recursive = FALSE)
    c15 <- unique(vapply(Filter(function(c) c$criterion == 15L, conds), function(c) c$horizon, ""))
    expect_identical(c15, "SCH", info = code)
  }
  # the 1110 headers: the families of Valsrivier keep their printed codes
  va <- vapply(f$families$Va, `[[`, "", "code")
  expect_identical(length(va), 16L)
  expect_true(all(c("Va1111", "Va2222") %in% va))
  expect_false(any(c("Va1110", "Va1100") %in% va))
  expect_true("Sw2111" %in% vapply(f$families$Sw, `[[`, "", "code"))
  # Griffin: mesotrophic or eutrophic is one class
  g2 <- Filter(function(e) startsWith(e$digits, "2"), f$families$Gf)[[1L]]
  expect_setequal(unlist(g2$tests$all_of[[1L]]$family_class_za$class), c("mesotrophic", "eutrophic"))
  # Kimberly and Vilafontes as printed
  expect_true(all(vapply(f$families$Ky, function(e) startsWith(e$code, "Ky"), NA)))
})

test_that("FC 8: the base status of the B1, 5 and 15 mesotrophic, calcareous no class", {
  skip_on_cran()
  base <- .ex("hutton", "Hu1100")
  clay <- as.numeric(base$horizons$clay_pct[2])
  at <- function(v) .mod(base, 2, ca_cmol = v * clay / 100, mg_cmol = 0, k_cmol = 0, na_cmol = 0)
  expect_identical(.fc(at(4.9), 8, "B1", "RAB")$class, "dystrophic")
  expect_identical(.fc(at(5), 8, "B1", "RAB")$class, "mesotrophic")
  expect_identical(.fc(at(15), 8, "B1", "RAB")$class, "mesotrophic")
  expect_identical(.fc(at(15.1), 8, "B1", "RAB")$class, "eutrophic")
  # a cation not recorded: the others a lower end
  p <- .mod(at(30), 2, na_cmol = NA)
  expect_identical(.fc(p, 8, "B1", "RAB")$class, "eutrophic")
  expect_true("bases_partial" %in% .fc(p, 8, "B1", "RAB")$tag)
  expect_true(is.na(.fc(.mod(at(10), 2, na_cmol = NA), 8, "B1", "RAB")$class))
  # a high EC flags the S-value
  expect_true("ec_high" %in% .fc(.mod(at(10), 2, ec_dS_m = 6), 8, "B1", "RAB")$tag)
  # a calcareous B1 has no base status (a calcareous apedal B is a
  # neocarbonate B, so it is met in the red structured B of Shortlands,
  # whose calcareous families take it)
  sd <- .ex("shortlands", "Sd2110")
  expect_identical(.fc(sd, 8, "B1", "RSB")$class, "eutrophic")
  pc <- .mod(sd, 2, effervescence_class = "slight", caco3_pct = 2)
  expect_identical(.fc(pc, 8, "B1", "RSB")$class, "calcareous")
  expect_identical(.cl(pc)$levels$code, c("Sd", "Sd3110"))
  # a humic A neither thick nor thin: no family, the name stops at the form
  kp <- .ex("kranskop", "Kp1100")
  h <- as.data.frame(kp$horizons)
  a2 <- h[1, ]; a2$top_cm <- 20; a2$bottom_cm <- 30; a2$oc_pct <- 0.8
  h$bottom_cm[1] <- 20; h$oc_pct[1] <- 2.2
  kn <- PedonRecord$new(site = kp$site, horizons = ensure_horizon_schema(data.table::as.data.table(rbind(h[1, ], a2, h[-1, ]))))
  r <- .cl(kn)
  expect_identical(r$rsg_or_order, "Kranskop")
  expect_identical(.fc(kn, 2, "whole", "HA")$class, "neither")
  expect_identical(nrow(r$levels), 1L)
  expect_true(any(grepl("No class of the families list under Kp passed", r$warnings, fixed = TRUE)))
  expect_true(any(grepl("No family of Kranskop holds", r$warnings, fixed = TRUE)))
  # the B1 is the layer designated B1, else the first B layer (Q20)
  h <- as.data.frame(base$horizons)
  b2 <- h[2, ]; b2$top_cm <- 60; b2$bottom_cm <- 150; b2$clay_pct <- 40
  h1 <- h; h1$bottom_cm[2] <- 60
  two <- PedonRecord$new(site = base$site, horizons = ensure_horizon_schema(data.table::as.data.table(rbind(h1, b2))))
  r2 <- .fc(two, 10, "B1", "RAB")
  expect_identical(r2$class, "non_luvic")
  expect_match(r2$what, "25-60 cm")
  # R5: the upper layer of the first B, whatever its designation (a BA over
  # a Bt1: the BA)
  two_b <- .mod(.mod(two, 2, designation = "BA"), 3, designation = "Bt1")
  expect_identical(.fc(two_b, 10, "B1", "RAB")$class, "non_luvic")
  expect_match(.fc(two_b, 10, "B1", "RAB")$what, "25-60 cm")
})

test_that("FC 10: the luvic B1 against the A or E directly above (Q21)", {
  skip_on_cran()
  base <- .ex("hutton", "Hu1100")
  lv <- function(a, b) .fc(.mod(.mod(base, 1, clay_pct = a), 2, clay_pct = b), 10, "B1", "RAB")$class
  # A 15 % or less: 5 points more
  expect_identical(lv(10, 15), "luvic")
  expect_identical(lv(10, 14.9), "non_luvic")
  # A over 15 %: 1.3 times
  expect_identical(lv(20, 26), "luvic")
  expect_identical(lv(20, 25.9), "non_luvic")
  expect_identical(lv(30, 35), "non_luvic")
  # clay of the B1 not recorded: undecided
  r <- .fc(.mod(base, 2, clay_pct = NA), 10, "B1", "RAB")
  expect_true(is.na(r$class))
  expect_true("clay_pct" %in% r$missing)
  # the E directly above (Vilafontes)
  vf <- .ex("vilafontes", "Vf1120")
  expect_identical(.fc(vf, 10, "B1", "NCU")$class, "luvic")
})

test_that("FC 2, 3, 4, 5, 6: the humic A, dark and bleached A, the colours", {
  skip_on_cran()
  kp <- .ex("kranskop", "Kp2100")
  expect_identical(.fc(kp, 2, "whole", "HA")$class, "thick")
  expect_identical(.fc(.ex("kranskop", "Kp1100"), 2, "whole", "HA")$class, "thin")
  # organic carbon of the A not recorded: undecided
  expect_true(is.na(.fc(.mod(kp, 1, oc_pct = NA), 2, "whole", "HA")$class))
  # FC 3 (Fernwood)
  expect_identical(.fc(.ex("fernwood", "Fw2110"), 3, "whole", "OA")$class, "dark")
  expect_identical(.fc(.ex("fernwood", "Fw1110"), 3, "whole", "OA")$class, "light")
  # FC 4: grey dry (Q9); no dry colour and a low moist chroma, undecided
  expect_identical(.fc(.ex("dresden", "Dr2000"), 4, "whole", "OA")$class, "bleached")
  expect_identical(.fc(.ex("dresden", "Dr1000"), 4, "whole", "OA")$class, "not_bleached")
  nd <- .mod(.ex("dresden", "Dr2000"), 1, munsell_value_dry = NA, munsell_chroma_dry = NA, munsell_chroma_moist = 2)
  expect_true(is.na(.fc(nd, 4, "whole", "OA")$class))
  # FC 5: dark first; 10YR with chroma 2 is dark, 5YR chroma 2 is not
  ar <- .ex("arcadia", "Ar1100")
  expect_identical(.fc(ar, 5, "whole", "VA")$class, "dark")
  expect_identical(.fc(.mod(ar, 1, munsell_hue_moist = "10YR", munsell_value_moist = 3, munsell_chroma_moist = 2), 5, "whole", "VA")$class, "dark")
  expect_identical(.fc(.mod(ar, 1, munsell_hue_moist = "5YR", munsell_value_moist = 3, munsell_chroma_moist = 2), 5, "whole", "VA")$class, "red")
  expect_identical(.fc(.ex("arcadia", "Ar3100"), 5, "whole", "VA")$class, "other")
  expect_identical(.fc(.ex("bonheim", "Bo2110"), 5, "whole", "PCU")$class, "red")
  # FC 6: yellow moist (5YR or yellower), grey moist
  kd <- .ex("kroonstad", "Kd2000")
  expect_identical(.fc(kd, 6, "whole", "E")$class, "yellow")
  expect_identical(.fc(.mod(kd, 2, munsell_hue_moist = "5YR"), 6, "whole", "E")$class, "yellow")
  # R1: a redder E with a higher chroma is grey
  expect_identical(.fc(.mod(kd, 2, munsell_hue_moist = "2.5YR"), 6, "whole", "E")$class, "grey")
  expect_identical(.fc(.ex("kroonstad", "Kd1000"), 6, "whole", "E")$class, "grey")
  # FC 9: no hue recorded, undecided
  r9 <- .fc(.mod(.ex("oakleaf", "Oa1210"), 2, munsell_hue_moist = NA, munsell_hue_dry = NA), 9, "whole", "NCU")
  expect_true(is.na(r9$class))
  expect_true("munsell_hue_moist" %in% r9$missing)
})

test_that("FC 1, 7, 11, 12, 13, 14: organic O, lamellae, structure, cutans, ortstein, hardness", {
  skip_on_cran()
  ch <- .ex("champagne", "Ch1100")
  expect_identical(.fc(ch, 1, "whole", "OO")$class, "fibrous")
  expect_identical(.fc(.mod(ch, 1, fiber_content_unrubbed_pct = NA, von_post_index = "H3"), 1, "whole", "OO")$class, "fibrous")
  expect_identical(.fc(.mod(ch, 1, fiber_content_unrubbed_pct = NA, von_post_index = "H7"), 1, "whole", "OO")$class, "humified")
  expect_true(is.na(.fc(.mod(ch, 1, fiber_content_unrubbed_pct = NA), 1, "whole", "OO")$class))
  # FC 7: lamellae not recorded, absent
  expect_identical(.fc(.ex("fernwood", "Fw1110"), 7, "whole", "E")$class, "absent")
  expect_identical(.fc(.ex("fernwood", "Fw1120"), 7, "whole", "E")$class, "present")
  # FC 11: subangular any size; angular by size; a range by its lower end
  sd <- .ex("shortlands", "Sd1120")
  expect_identical(.fc(sd, 11, "whole", "RSB")$class, "medium_coarse")
  expect_identical(.fc(.mod(sd, 2, structure_size = "fine to medium"), 11, "whole", "RSB")$class, "subangular_fine")
  expect_true(is.na(.fc(.mod(sd, 2, structure_size = NA), 11, "whole", "RSB")$class))
  expect_identical(.fc(.mod(sd, 2, structure_type = "subangular blocky", structure_size = "coarse"), 11, "whole", "RSB")$class,
                   "subangular_fine")
  # FC 12: continuous black cutans; continuity from clay films, flagged
  es <- .ex("estcourt", "Es1200")
  expect_identical(.fc(es, 12, "whole", "PRC")$class, "with")
  expect_identical(.fc(.ex("estcourt", "Es1100"), 12, "whole", "PRC")$class, "without")
  i_prc <- 3L
  cf <- .fc(.mod(es, i_prc, coatings_continuity = NA, clay_films_amount = "many"), 12, "whole", "PRC")
  expect_identical(cf$class, "with")
  expect_true("continuity_from_clay_films" %in% cf$tag)
  expect_true(is.na(.fc(.mod(es, i_prc, coatings_chroma_moist = NA), 12, "whole", "PRC")$class))
  # FC 13: ortstein
  expect_identical(.fc(.ex("lamotte", "Lt2100"), 13, "whole", "PZB")$class, "hardened")
  expect_identical(.fc(.ex("lamotte", "Lt1100"), 13, "whole", "PZB")$class, "not_hardened")
  # FC 14 (Q19): bedrock over 70 %; fragments with rupture resistance
  gs <- .ex("glenrosa", "Gs1211")
  expect_identical(.fc(gs, 14, "B1", "LCU")$class, "hard")
  expect_identical(.fc(.mod(gs, 2, bedrock_pct = 70), 14, "B1", "LCU")$class, "not_hard")
  hh <- .ex("houwhoek", "Hh2100")
  r14 <- .fc(hh, 14, "upper", "SAP")
  expect_identical(r14$class, "hard")
  expect_true("hard_from_fragments" %in% r14$tag)
  j <- which(as.character(hh$horizons$designation) == "C" & !is.na(hh$horizons$saprolite_pct))[1L]
  expect_true(is.na(.fc(.mod(hh, j, rupture_resistance = NA), 14, "upper", "SAP")$class))
})

test_that("FC 15, 16, 17, 18, 19: wetness, carbonates, podzolic character, the C, under the O", {
  skip_on_cran()
  # FC 15 on the layer immediately beneath the placic pan
  expect_identical(.fc(.ex("tsitsikamma", "Ts2000"), 15, "below", "PLP")$class, "present")
  expect_identical(.fc(.ex("tsitsikamma", "Ts1000"), 15, "below", "PLP")$class, "absent")
  # nothing described under the pan: undecided
  ts <- .ex("tsitsikamma", "Ts1000")
  h <- as.data.frame(ts$horizons)
  ts2 <- PedonRecord$new(site = ts$site, horizons = ensure_horizon_schema(data.table::as.data.table(h[-nrow(h), ])))
  expect_true(is.na(.fc(ts2, 15, "below", "PLP")$class))
  # FC 15 on the soft carbonate horizon, Dundee on the profile
  expect_identical(.fc(.ex("brandvlei", "Br2000"), 15, "whole", "SCH")$class, "present")
  expect_identical(.fc(.ex("dundee", "Du1210"), 15, "profile", "-")$class, "present")
  # FC 16 (Q22): effervescence, else CaCO3, relict nodules undecided
  ms <- .ex("mispah", "Ms1200")
  expect_identical(.fc(ms, 16, "whole", "OA")$class, "calcareous")
  expect_identical(.fc(.ex("mispah", "Ms1100"), 16, "whole", "OA")$class, "non_calcareous")
  rel <- .mod(.ex("mispah", "Ms1100"), 1, effervescence_class = NA, caco3_pct = 3, carbonate_hard_pct = 5)
  expect_true(is.na(.fc(rel, 16, "whole", "OA")$class))
  expect_identical(.fc(.ex("valsrivier", "Va1112"), 16, "with_next", "PCU")$class, "calcareous")
  expect_identical(.fc(.ex("witbank", "Wb2000"), 16, "profile", "-")$class, "calcareous")
  # FC 17: a podzolic layer under the yellow-brown apedal B (Constantia)
  ct <- .ex("constantia", "Ct1200")
  expect_identical(.cl(ct)$rsg_or_order, "Constantia")
  expect_identical(.fc(ct, 17, "below_all", "YAB")$class, "present")
  expect_identical(.fc(.ex("constantia", "Ct1100"), 17, "below_all", "YAB")$class, "absent")
  # FC 18: moist consistence; not recorded, undecided
  expect_identical(.fc(.ex("lamotte", "Lt1200"), 18, "upper", "UMW")$class, "firm")
  pg <- .ex("pinegrove", "Pg1000")
  expect_identical(.fc(pg, 18, "upper", "UMN")$class, "friable")
  k <- nrow(pg$horizons)
  expect_true(is.na(.fc(.mod(pg, k, consistence_moist = NA), 18, "upper", "UMN")$class))
  expect_identical(.fc(.mod(pg, k, consistence_moist = "slightly firm"), 18, "upper", "UMN")$class, "friable")
  # FC 19: rock within 500 mm under the O
  expect_identical(.fc(.ex("champagne", "Ch1100"), 19, "below", "OO")$class, "rock_or_saprolite")
  expect_identical(.fc(.ex("champagne", "Ch1200"), 19, "below", "OO")$class, "unconsolidated")
  o <- .ex("champagne", "Ch1100")
  h <- as.data.frame(o$horizons)
  mid <- h[1, ]; mid$top_cm <- 40; mid$bottom_cm <- 100; mid$designation <- "C"; mid$oc_pct <- 0.3
  mid$fiber_content_unrubbed_pct <- NA; mid$water_saturation_days <- NA; mid$munsell_value_moist <- 5; mid$munsell_chroma_moist <- 4
  rk <- h[2, ]; rk$top_cm <- 100; rk$bottom_cm <- 120
  far <- PedonRecord$new(site = o$site, horizons = ensure_horizon_schema(data.table::as.data.table(rbind(h[1, ], mid, rk))))
  expect_identical(.fc(far, 19, "below", "OO")$class, "unconsolidated")
  rk2 <- rk; rk2$top_cm <- 85; mid2 <- mid; mid2$bottom_cm <- 85
  near <- PedonRecord$new(site = o$site, horizons = ensure_horizon_schema(data.table::as.data.table(rbind(h[1, ], mid2, rk2))))
  expect_identical(.fc(near, 19, "below", "OO")$class, "rock_or_saprolite")
  # the trace names the criterion, the layers read and the class
  r <- .cl(.ex("hutton", "Hu3200"))
  tr <- r$trace$families[["Hu3200"]]
  expect_true(isTRUE(tr$passed))
})

test_that("the families in the app and report catalogues, en and pt", {
  skip_on_cran()
  tr <- yaml::read_yaml(system.file("i18n", "translations.yaml", package = "soilKey"))
  rp <- yaml::read_yaml(system.file("i18n", "report_translations.yaml", package = "soilKey"))
  expect_identical(tr$en[["classify.level_families"]], "Family")
  expect_identical(tr$pt[["classify.level_families"]], "Fam\u00edlia")
  expect_identical(rp$pt[["report.level_families"]], "Fam\u00edlia")
  cls <- unique(unlist(lapply(.fam()$family_criteria, function(c) unlist(c$classes))))
  for (l in c("en", "pt")) {
    for (k in sprintf("za1991.fc_%02d", 1:19)) expect_true(nzchar(tr[[l]][[k]] %||% ""), info = paste(l, k))
    for (k in paste0("za1991.fc_class_", cls)) expect_true(nzchar(tr[[l]][[k]] %||% ""), info = paste(l, k))
  }
  expect_identical(tr$en[["za1991.fc_01"]], .fam()$family_criteria[[1L]]$name)
})


# --- the review of 2026-11-06: the repros of D1-D8 and Hugo's decisions R1-R6 ------

# a pedon whose layer i is split into pieces (top, bottom and attributes each)
.split <- function(p, i, pieces) {
  h <- as.data.frame(p$horizons)
  rows <- lapply(pieces, function(pc) { r <- h[i, ]; for (k in names(pc)) { if (!k %in% names(r)) r[[k]] <- NA; r[[k]] <- pc[[k]] }; r })
  nm <- unique(c(names(h), unlist(lapply(rows, names))))
  fill <- function(d) { for (k in setdiff(nm, names(d))) d[[k]] <- NA; d[, nm] }
  h2 <- do.call(rbind, c(list(fill(h[seq_len(i - 1L), , drop = FALSE])), lapply(rows, fill), list(fill(h[-seq_len(i), , drop = FALSE]))))
  old <- options(soilKey.warn_unrecognised = FALSE)
  on.exit(options(old))
  PedonRecord$new(site = p$site, horizons = ensure_horizon_schema(data.table::as.data.table(h2)))
}
# a pedon with only some layers, the last one ending at `bottom`
.cut <- function(p, keep, bottom = NULL) {
  h <- as.data.frame(p$horizons)[keep, ]
  if (!is.null(bottom)) h$bottom_cm[nrow(h)] <- bottom
  PedonRecord$new(site = p$site, horizons = ensure_horizon_schema(data.table::as.data.table(h)))
}

test_that("D1, D6: an organic O of unrecorded lower limit does not crash, no -Inf", {
  skip_on_cran()
  old <- options(soilKey.warn_unrecognised = FALSE); on.exit(options(old))
  p <- PedonRecord$new(site = list(id = "d1"), horizons = data.frame(top_cm = c(0, 60), bottom_cm = c(NA, 150),
                       designation = c("O", "C"), diagnostic_za = c("Organic O", ""), stringsAsFactors = FALSE))
  expect_no_warning(r <- .cl(p))
  expect_identical(r$rsg_or_order, "Champagne")
  cr <- r$qualifiers$za_family$criteria
  expect_true(is.na(cr$class[cr$criterion == 19L]))
  expect_match(cr$missing[cr$criterion == 19L], "bottom_cm")
  expect_false(any(grepl("Inf", cr$read)))
  expect_true(any(grepl("?", cr$read, fixed = TRUE)))
})

test_that("D2: absence on an open description, or with nothing under the A, is undecided", {
  skip_on_cran()
  du <- .cut(.ex("dundee", "Du1110"), 1:2, bottom = 80)
  r <- .cl(du)
  expect_identical(r$rsg_or_order, "Dundee")
  expect_identical(r$qualifiers$za_family$partial, "Du1xx0")
  # a positive finding still decides
  du2 <- .mod(du, 2, effervescence_class = "slight", caco3_pct = 2)
  expect_identical(.fc(du2, 16, "profile", "-")$class, "calcareous")
  nb <- .cut(.ex("namib", "Nb1100"), 1:2, bottom = 80)
  expect_true(is.na(.fc(nb, 16, "profile", "-")$class))
  wb <- .cut(.ex("witbank", "Wb1000"), 1:2, bottom = 80)
  expect_true(is.na(.fc(wb, 16, "profile", "-")$class))
  # the complete descriptions still decide
  expect_identical(.cl(.ex("dundee", "Du1110"))$levels$code[2L], "Du1110")
  # Inhoek: a melanic A with nothing under it
  ik <- .cut(.ex("inhoek", "Ik1100"), 1, bottom = 60)
  ri <- .cl(ik)
  expect_identical(ri$rsg_or_order, "Inhoek")
  expect_true(is.na(.fc(ik, 15, "with_below", "MA")$class))
  expect_true(is.na(.fc(ik, 16, "with_below", "MA")$class))
  expect_identical(ri$qualifiers$za_family$partial, "Ikxx00")
})

test_that("D3: a pale carbonate matrix is no sign of wetness", {
  skip_on_cran()
  for (f in c("molopo", "kimberly", "brandvlei")) {
    p <- .ex(f)
    j <- which(as.character(p$horizons$designation) == "Ck")
    for (mv in list(c(7, 2), c(8, 1))) {
      pale <- .mod(p, j, munsell_value_moist = mv[1], munsell_chroma_moist = mv[2])
      rc <- .fc(pale, 15, "whole", "SCH")
      expect_identical(rc$class, "absent", info = f)
      expect_true("sch_mottles_only" %in% rc$tag, info = f)
    }
    expect_identical(.fc(.mod(p, j, mottle_colors = "7.5YR 5/8", redoximorphic_features_pct = 6), 15, "whole", "SCH")$class, "present")
    expect_identical(.fc(.mod(p, j, munsell_hue_moist = "5GY"), 15, "whole", "SCH")$class, "present")
  }
})

test_that("D4: the words of hardness follow the USDA rupture resistance", {
  skip_on_cran()
  hh <- .ex("houwhoek", "Hh2100")
  j <- which(!is.na(hh$horizons$saprolite_pct))[1L]
  rd <- function(w) .fc(.mod(hh, j, rupture_resistance = w), 14, "upper", "SAP")$class
  for (w in c("hard", "very hard", "slightly rigid", "rigid", "very firm", "extremely firm", "moderately cemented", "indurated"))
    expect_identical(rd(w), "hard", info = w)
  for (w in c("slightly hard", "moderately hard", "firm", "friable", "weakly cemented", "soft"))
    expect_identical(rd(w), "not_hard", info = w)
  expect_true(is.na(rd("crumbly")))
})

test_that("D5: the 500 mm under the organic O is the book's number", {
  skip_on_cran()
  t <- soilKey:::.za_th("organic_rock_depth")
  expect_identical(t$source, "K 17")
  expect_false(isTRUE(t$stand_in))
  rc <- .fc(.ex("champagne", "Ch1100"), 19, "below", "OO")
  expect_false(any(grepl("organic_rock_depth", rc$tag)))
})

test_that("D7: one pair of brackets; no partial code when no class of a digit holds", {
  skip_on_cran()
  py <- .ex("plooysburg", "[Py]1000")
  j <- which(as.character(py$horizons$designation) == "B")[1L]
  r <- .cl(.mod(py, j, clay_pct = NA))
  expect_identical(r$qualifiers$za_family$partial, "Pyx000")
  expect_match(r$name, "^Plooysburg, \\[Pyx000\\]")
  expect_false(grepl("[[", r$name, fixed = TRUE))
  # a humic A neither thick nor thin: no family, no partial code
  kp <- .ex("kranskop", "Kp1100")
  rk <- .cl(.split(kp, 1, list(list(bottom_cm = 20, oc_pct = 2.2), list(top_cm = 20, bottom_cm = 30, oc_pct = 0.8))))
  expect_true(is.na(rk$qualifiers$za_family$partial))
  expect_false(grepl("[", rk$name, fixed = TRUE))
  expect_true(any(grepl("No family of Kranskop holds", rk$warnings, fixed = TRUE)))
  expect_false(any(grepl("x", rk$qualifiers$za_family$partial %||% "")))
})

test_that("D8: a partial sum of 5 or more leaves mesotrophic or eutrophic", {
  skip_on_cran()
  base <- .ex("hutton", "Hu1100")
  clay <- as.numeric(base$horizons$clay_pct[2])
  r <- .fc(.mod(base, 2, ca_cmol = 10 * clay / 100, mg_cmol = NA, k_cmol = 0, na_cmol = 0), 8, "B1", "RAB")
  expect_true(is.na(r$class))
  expect_setequal(r$candidates, c("mesotrophic", "eutrophic"))
  r2 <- .fc(.mod(base, 2, ca_cmol = 2 * clay / 100, mg_cmol = NA, k_cmol = 0, na_cmol = 0), 8, "B1", "RAB")
  expect_setequal(r2$candidates, c("dystrophic", "mesotrophic", "eutrophic"))
})

test_that("R2: Dundee reads the signs of wetness from the alluvium down, not in the A", {
  skip_on_cran()
  du <- .ex("dundee", "Du1110")
  wetA <- .mod(du, 1, mottle_colors = "10YR 6/2", redoximorphic_features_pct = 10)
  expect_identical(.fc(wetA, 15, "from", "SAL")$class, "absent")
  expect_identical(.cl(wetA)$levels$code[2L], "Du1110")
  expect_identical(.cl(.ex("dundee", "Du1210"))$levels$code[2L], "Du1210")
})

test_that("R3: Shortlands, a calcareous middle layer of the B gives Sd3xxx", {
  skip_on_cran()
  for (nt in c(FALSE, TRUE)) {
  sd <- .ex("shortlands", "Sd1110", notation = nt)
  j <- which(as.character(sd$horizons$designation) == "B")[1L]
  t0 <- as.numeric(sd$horizons$top_cm[j])
  p <- .split(sd, j, list(list(bottom_cm = t0 + 35), list(top_cm = t0 + 35, bottom_cm = t0 + 75, effervescence_class = "slight", caco3_pct = 2),
                          list(top_cm = t0 + 75)))
  r <- .cl(p)
  expect_identical(r$rsg_or_order, "Shortlands", info = nt)
  expect_identical(r$levels$code[2L], "Sd3110", info = nt)
  }
})

test_that("a diagnostic horizon of several layers on the data: every layer read as it", {
  skip_on_cran()
  cases <- list(list("shortlands", "Sd1220", "B", 3L, "RSB"), list("valsrivier", "Va2121", "Bt", 2L, "PCU"),
                list("clovelly", "Cv2200", "B", 2L, "YAB"), list("hutton", "Hu3200", "B", 3L, "RAB"),
                list("glenrosa", "Gs2111", "BC", 2L, "LCU"), list("estcourt", "Es2200", "Btn", 2L, "PRC"),
                list("oakleaf", "Oa2220", "B", 2L, "NCU"), list("augrabies", "Ag1210", "Bk", 2L, "NCA"),
                list("kroonstad", "Kd2000", "E", 2L, "E"), list("katspruit", "Ka2000", "G", 2L, "G"))
  for (cs in cases) {
    p <- .ex(cs[[1]], cs[[2]])
    j <- which(as.character(p$horizons$designation) == cs[[3]])[1L]
    t0 <- as.numeric(p$horizons$top_cm[j]); b0 <- as.numeric(p$horizons$bottom_cm[j])
    cut <- round(seq(t0, b0, length.out = cs[[4]] + 1L))
    ps <- .split(p, j, lapply(seq_len(cs[[4]]), function(k) list(top_cm = cut[k], bottom_cm = cut[k + 1L])))
    fr <- identify_horizons(ps, "za1991")
    expect_identical(fr$code[j + seq_len(cs[[4]]) - 1L], rep(cs[[5]], cs[[4]]), info = cs[[2]])
    r <- .cl(ps)
    expect_identical(r$levels$code[2L], cs[[2]], info = cs[[2]])
    expect_identical(r$provisional_because, character(0), info = cs[[2]])
  }
  # under a lithocutanic B a layer with the rock of a saprolite is saprolite
  gs <- .ex("glenrosa", "Gs1111")
  j <- which(as.character(gs$horizons$designation) == "BC")[1L]
  t0 <- as.numeric(gs$horizons$top_cm[j]); b0 <- as.numeric(gs$horizons$bottom_cm[j])
  ps <- .split(gs, j, list(list(bottom_cm = t0 + 15), list(top_cm = t0 + 15, bedrock_pct = 60)))
  expect_identical(identify_horizons(ps, "za1991")$code[j + 0:1], c("LCU", "SAP"))
})

test_that("R4: fibrous is one contiguous run of 200 mm", {
  skip_on_cran()
  ch <- .ex("champagne", "Ch1100")
  p <- .split(ch, 1, list(list(bottom_cm = 15, fiber_content_unrubbed_pct = 80),
                          list(top_cm = 15, bottom_cm = 25, fiber_content_unrubbed_pct = 20),
                          list(top_cm = 25, bottom_cm = 40, fiber_content_unrubbed_pct = 80)))
  expect_identical(.fc(p, 1, "whole", "OO")$class, "humified")
  p2 <- .split(ch, 1, list(list(bottom_cm = 15, fiber_content_unrubbed_pct = 80),
                           list(top_cm = 15, bottom_cm = 40, fiber_content_unrubbed_pct = 80)))
  expect_identical(.fc(p2, 1, "whole", "OO")$class, "fibrous")
})

test_that("R6: FC 5 on a B of several layers, the larger share of dark or red", {
  skip_on_cran()
  bo <- .ex("bonheim", "Bo1110")
  j <- which(as.character(bo$horizons$designation) == "Bt")[1L]
  t0 <- as.numeric(bo$horizons$top_cm[j]); b0 <- as.numeric(bo$horizons$bottom_cm[j])
  dark <- list(munsell_hue_moist = "10YR", munsell_value_moist = 3, munsell_chroma_moist = 1)
  red <- list(munsell_hue_moist = "2.5YR", munsell_value_moist = 3, munsell_chroma_moist = 4, munsell_hue_dry = "2.5YR",
              coatings_hue_moist = "10YR", coatings_value_moist = 4, coatings_chroma_moist = 3)
  other <- list(munsell_hue_moist = "10YR", munsell_value_moist = 4, munsell_chroma_moist = 3)
  th <- b0 - t0
  mk <- function(a, b, c, fa, fb) .split(bo, j, list(c(list(bottom_cm = t0 + fa * th), a),
                                                       c(list(top_cm = t0 + fa * th, bottom_cm = t0 + fb * th), b),
                                                       c(list(top_cm = t0 + fb * th), c)))
  expect_identical(.fc(mk(dark, red, other, 0.4, 0.75), 5, "whole", "PCU")$class, "dark")
  expect_identical(.fc(mk(dark, red, other, 0.3, 0.7), 5, "whole", "PCU")$class, "red")
  expect_identical(.fc(mk(dark, red, other, 0.35, 0.7), 5, "whole", "PCU")$class, "dark")
})
