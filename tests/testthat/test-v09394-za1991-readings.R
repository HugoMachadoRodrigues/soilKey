# v0.9.394: four readings of the South African classification of 1991 after
# the AfSP benchmark (Hugo's decisions of 2026-11-10): B1, a carbon-poor
# surface layer over hard rock is a B at the surface only when more than
# 100 mm thick (thickness not recorded: undecided); C1, clay films not
# recorded and soilKey's argic horizon not holding leave the cutans of the
# pedocutanic B undecided; A1, the second figure of the AfSP benchmark reads
# the humic A of 1977; M1, no dry colour and a moist value over 3 refuse
# the melanic colour. Synthetic profiles only; no AfSP profile or label is
# shipped.

.L394 <- function(top, bottom, ...) {
  d <- list(top_cm = top, bottom_cm = bottom, munsell_hue_moist = "10YR", munsell_value_moist = 5,
            munsell_chroma_moist = 4, munsell_hue_dry = "10YR", munsell_value_dry = 6,
            munsell_chroma_dry = 4, structure_type = "massive", structure_grade = "structureless",
            clay_pct = 20, silt_pct = 20, sand_pct = 60, oc_pct = 0.3, effervescence_class = "none",
            clay_films_amount = "none")
  a <- list(...)
  d[names(a)] <- a
  d
}
.P394 <- function(...) {
  ls <- list(...)
  nm <- unique(unlist(lapply(ls, names)))
  h <- as.data.frame(lapply(stats::setNames(nm, nm), function(k)
    unlist(lapply(ls, function(l) if (is.null(l[[k]])) NA else l[[k]]))), stringsAsFactors = FALSE)
  old <- options(soilKey.warn_unrecognised = FALSE)
  on.exit(options(old))
  PedonRecord$new(site = list(id = "za394", lat = -29, lon = 30), horizons = h)
}
.cl394 <- function(p) classify_za1991(p, on_missing = "silent")
.form394 <- function(p) {
  lv <- .cl394(p)$levels
  if (nrow(lv)) lv$code[lv$level == "forms"][1] else NA_character_
}
# a dark, base-rich melanic A (dry colour recorded) over a strong blocky,
# non-red B with no clay increase
.melA <- function(...) .L394(0, 30, designation = "A", munsell_value_moist = 2, munsell_chroma_moist = 1,
                             munsell_value_dry = 3, munsell_chroma_dry = 1, structure_type = "subangular blocky",
                             structure_grade = "moderate", slickensides = "none", clay_pct = 45, oc_pct = 2,
                             ca_cmol = 15, mg_cmol = 8, k_cmol = 0.5, na_cmol = 0.3, ...)
.blkB <- function(top, bottom, ...) .L394(top, bottom, designation = "B", munsell_hue_moist = "7.5YR",
                                          munsell_value_moist = 4, munsell_chroma_moist = 4,
                                          munsell_hue_dry = "7.5YR", munsell_value_dry = 5, munsell_chroma_dry = 4,
                                          structure_type = "angular blocky", structure_grade = "strong",
                                          clay_pct = 46, ...)

test_that("B1: a carbon-poor surface layer over hard rock is a B only when more than 100 mm thick", {
  skip_on_cran()
  th <- soilKey:::.za_thresholds()$surface_b_thick
  expect_identical(as.numeric(th$value), 100)
  expect_identical(th$op, ">")
  expect_true(th$stand_in)
  yb <- function(b) .P394(.L394(0, b, designation = "A", munsell_value_moist = 5, munsell_chroma_moist = 6,
                                munsell_value_dry = 6, munsell_chroma_dry = 6, oc_pct = 0.2),
                          .L394(b, b + 20, designation = "R"))
  # 50 and 100 mm: the orthic A on hard rock, Mispah
  for (b in c(5, 10)) {
    f <- identify_horizons(yb(b), "za1991")
    expect_identical(f$code, c("OA", "HRK"), info = b)
    expect_identical(.form394(yb(b)), "Ms", info = b)
  }
  # 110 mm: a yellow-brown apedal B at the surface, Clovelly, flagged
  f <- identify_horizons(yb(11), "za1991")
  expect_identical(f$code, c("YAB", "HRK"))
  expect_true(all(c("surface_b_low_oc", "stand_in:surface_b_oc", "stand_in:surface_b_thick") %in% f$tags[[1]]))
  r <- .cl394(yb(11))
  expect_identical(r$levels$code[1], "Cv")
  expect_true(r$qualifiers$za_truncated)
  # D: the thickness not recorded leaves it undecided, the orthic A or the B
  pd <- .P394(.L394(0, NA, designation = "A", munsell_value_moist = 5, munsell_chroma_moist = 6,
                    munsell_value_dry = 6, munsell_chroma_dry = 6, oc_pct = 0.2),
              .L394(NA, 30, designation = "R"))
  fd <- identify_horizons(pd, "za1991")
  expect_true(is.na(fd$code[1]))
  expect_setequal(fd$candidates[[1]], c("OA", "YAB"))
  expect_true(any(grepl("thickness not recorded", fd$notes[[1]], fixed = TRUE)))
  expect_true(is.na(.form394(pd)))
  # with more carbon it is not read as a B, whatever the thickness
  p <- .P394(.L394(0, 30, designation = "A", munsell_value_moist = 5, munsell_chroma_moist = 6,
                   munsell_value_dry = 6, munsell_chroma_dry = 6, oc_pct = 1), .L394(30, 50, designation = "R"))
  expect_identical(identify_horizons(p, "za1991")$code[1], "OA")
})

test_that("C1: clay films not recorded and argic not holding leave the pedocutanic B undecided", {
  skip_on_cran()
  pcu <- function(p, i) soilKey:::.za_t_PCU(soilKey:::.za_x(p), i)
  # no clay increase: argic does not hold
  p <- .P394(.melA(), .blkB(30, 150, clay_films_amount = NA))
  expect_false(isTRUE(argic(p)$passed))
  r <- pcu(p, 2)
  expect_true(is.na(r$passed))
  expect_true("clay_films_amount" %in% r$missing)
  expect_true(any(grepl("argic", r$tag, fixed = TRUE)))
  expect_true(any(grepl("needs no clay increase", r$note, fixed = TRUE)))
  # the key: Bonheim stays a candidate, Inhoek is not given
  res <- .cl394(p)
  expect_false(identical(res$levels$code[1], "Ik"))
  expect_true("forms:Bo" %in% res$provisional_because)
  # clay films recorded decide as before
  expect_true(isTRUE(pcu(.P394(.melA(), .blkB(30, 150, clay_films_amount = "many")), 2)$passed))
  expect_false(pcu(.P394(.melA(), .blkB(30, 150, clay_films_amount = "few")), 2)$passed)
  expect_identical(.form394(.P394(.melA(), .blkB(30, 150, clay_films_amount = "many"))), "Bo")
  # a clear clay increase: argic holds and still stands in for the cutans
  p2 <- .P394(.melA(clay_pct = 20, sand_pct = 60), .blkB(30, 150, clay_films_amount = NA, clay_pct = 45))
  expect_true(isTRUE(argic(p2)$passed))
  r2 <- pcu(p2, 2)
  expect_true(isTRUE(r2$passed))
  expect_true(any(grepl("argic", r2$tag, fixed = TRUE)))
  # the other refusals of the cutans reader stand: carbonate neoformations
  p3 <- .P394(.melA(), .blkB(30, 150, clay_films_amount = NA, secondary_carbonates_form = "pseudomycelium"))
  expect_true(any(grepl("carbonate neoformations", soilKey:::.kidpr_cutans(soilKey:::.za_x(p3), 2)$note)))
  expect_false(pcu(p3, 2)$passed)
  # and outside the zone of the clay maximum of an argic horizon
  p4 <- .P394(.melA(clay_pct = 15, sand_pct = 65), .blkB(30, 60, clay_films_amount = NA, clay_pct = 45),
              .blkB(60, 150, clay_films_amount = NA, clay_pct = 30))
  k4 <- soilKey:::.kidpr_cutans(soilKey:::.za_x(p4), 3)
  expect_false(k4$passed)
  expect_true(any(grepl("outside the zone of the clay maximum", k4$note)))
  expect_false(pcu(p4, 3)$passed)
  # a weak structure is no pedocutanic B, recorded or not
  expect_false(pcu(.P394(.melA(), .blkB(30, 150, clay_films_amount = NA, structure_grade = "weak")), 2)$passed)
  # the shared reader of cutans is unchanged: argic not holding is still no cutans there
  x <- soilKey:::.za_x(p)
  expect_false(soilKey:::.kidpr_cutans(x, 2)$passed)
})

test_that("A1: the humic A of 1977 reads a thin humic form as its orthic counterpart", {
  skip_on_cran()
  map <- soilKey:::.ZA_HUMIC_ORTHIC
  expect_identical(unname(map[c("Kp", "Ma", "[Ia]", "Sr", "No")]), c("Gf", "Cv", "Hu", "Oa", "Gs"))
  for (k in c("inanda", "magwa", "kranskop", "sweetwater", "nomanci")) {
    p <- make_za1991_example(k)
    r <- .cl394(p)
    h <- soilKey:::.za_humic_1977(r, p)
    expect_identical(h$sk_form, r$levels$code[1], info = k)
    expect_true(h$mapped, info = k)
    expect_identical(h$form, unname(map[[h$sk_form]]), info = k)
    expect_true(h$humic_mm <= 450, info = k)
  }
  # a thick humic A (over 450 mm) is the humic A of 1977 too
  p <- make_za1991_example("inanda")
  hz <- as.data.frame(p$horizons)
  d <- 50 - (hz$bottom_cm[1] - hz$top_cm[1])
  hz$bottom_cm[1] <- hz$bottom_cm[1] + d
  hz$top_cm[-1] <- hz$top_cm[-1] + d
  hz$bottom_cm[-1] <- hz$bottom_cm[-1] + d
  old <- options(soilKey.warn_unrecognised = FALSE)
  on.exit(options(old), add = TRUE)
  p2 <- PedonRecord$new(site = p$site, horizons = ensure_horizon_schema(data.table::as.data.table(hz)))
  r2 <- .cl394(p2)
  skip_if_not(identical(r2$levels$code[1], "[Ia]"))
  h2 <- soilKey:::.za_humic_1977(r2, p2)
  expect_false(h2$mapped)
  expect_identical(h2$form, "[Ia]")
  expect_equal(h2$humic_mm, 500)
  # not a humic form, or no form: unchanged
  ph <- make_za1991_example("hutton")
  h3 <- soilKey:::.za_humic_1977(.cl394(ph), ph)
  expect_identical(c(h3$form, h3$sk_form), c("Hu", "Hu"))
  expect_false(h3$mapped)
  pu <- make_za1991_example("undecided")
  expect_true(is.na(soilKey:::.za_humic_1977(.cl394(pu), pu)$form))
})

test_that("A1: the AfSP benchmark gives the second figure, the main figure unchanged", {
  skip_on_cran()
  f <- system.file("benchmarks", "za1991_afsp.R", package = "soilKey")
  if (!nzchar(f)) f <- testthat::test_path("..", "..", "inst", "benchmarks", "za1991_afsp.R")
  skip_if_not(file.exists(f))
  env <- new.env()
  sys.source(f, envir = env)
  ex <- c("inanda", "inanda", "magwa", "hutton", "kranskop", "inanda")
  peds <- lapply(seq_along(ex), function(i) {
    p <- make_za1991_example(ex[i])
    p$site$id <- paste0("H", i)
    p
  })
  labels <- data.frame(id = paste0("H", 1:6),
                       label = c("Hutton Xseries", "Hutton Form, Lillieburn Family", "Clovelly Xseries", "Hutton Xseries",
                                 "Kranskop Xseries", "Hutton Xseries"),
                       year = c(1975L, 1999L, 1980L, 1980L, 1985L, 1999L), stringsAsFactors = FALSE)
  out <- tempfile(fileext = ".md")
  b <- env$za1991_afsp_benchmark(pedons = peds, labels = labels, out = out, verbose = FALSE)
  a <- b$score[b$score$group == "All", ]
  # main figure: the Hutton and the Kranskop examples agree
  expect_identical(c(a$n, a$agree, a$decided), c(6L, 2L, 6L))
  h <- b$humic_1977
  # H1 (1975) and H3 (1980) are read as Hutton and Clovelly and agree; H6 is
  # surveyed in 1999 but written as a 1977 binomial, read so and agrees; H2
  # is neither (a 1991 label of 1999) and is not read so; H5 (1985) Kranskop
  # is read as Griffin and no longer agrees with its Kranskop label
  expect_identical(c(h$mapped, h$gained, h$lost), c(4L, 3L, 1L))
  expect_identical(c(h$score$n, h$score$agree, h$score$decided), c(6L, 4L, 6L))
  rec <- b$records
  expect_identical(rec$form_1977[rec$id == "H2"], "[Ia]")
  expect_identical(rec$form_1977[rec$id == "H5"], "Gf")
  expect_identical(rec$form_1977[rec$id == "H6"], "Hu")
  expect_false(rec$agree_1977[rec$id == "H5"])
  md <- readLines(out)
  expect_true(any(grepl("^## Second figure: the humic A of 1977 \\(flagged\\)", md)))
  expect_true(any(grepl("^Profiles read so: 4; agreement gained 3, lost 1\\.", md)))
  expect_false(any(grepl("[^\\x01-\\x7F]", md, perl = TRUE)))
})

test_that("M1: no dry colour and a moist value over 3 refuse the melanic colour, flagged", {
  skip_on_cran()
  ma <- function(p) soilKey:::.za_t_MA(soilKey:::.za_x(p), 1)
  one <- function(...) .P394(.melA(...), .blkB(30, 150, clay_films_amount = "many"))
  nodry <- list(munsell_hue_dry = NA, munsell_value_dry = NA, munsell_chroma_dry = NA)
  # moist value 4, no dry colour: refused, flagged
  r <- ma(do.call(one, c(nodry, list(munsell_value_moist = 4, munsell_chroma_moist = 2))))
  expect_false(r$passed)
  expect_true(any(grepl("dry_value_from_moist", r$tag, fixed = TRUE)))
  expect_true(any(grepl("never darker", r$note, fixed = TRUE)))
  # moist value 3, no dry colour: undecided (default 9)
  r <- ma(do.call(one, c(nodry, list(munsell_value_moist = 3, munsell_chroma_moist = 2))))
  expect_true(is.na(r$passed))
  expect_true(any(grepl("dry_from_moist_evidence", r$tag, fixed = TRUE)))
  expect_false(any(grepl("dry_value_from_moist", r$tag, fixed = TRUE)))
  # a dry colour recorded rules, whatever the moist colour
  r <- ma(one(munsell_value_moist = 5, munsell_value_dry = 3, munsell_chroma_dry = 1))
  expect_true(isTRUE(r$passed))
  expect_false(any(grepl("dry_value_from_moist", r$tag, fixed = TRUE)))
  expect_false(ma(one(munsell_value_moist = 2, munsell_value_dry = 4, munsell_chroma_dry = 1))$passed)
  # the key: a thin, carbon-poor brown A on hard rock is Mispah
  p <- .P394(.L394(0, 5, designation = "A", munsell_hue_dry = NA, munsell_value_dry = NA, munsell_chroma_dry = NA,
                   munsell_value_moist = 4, munsell_chroma_moist = 4, oc_pct = 0.1),
             .L394(5, 30, designation = "R"))
  expect_identical(.form394(p), "Ms")
})

test_that("the readings of 0.9.394 are in readings.yaml, with Hugo's date", {
  skip_on_cran()
  f <- system.file("rules", "za1991", "readings.yaml", package = "soilKey")
  skip_if_not(file.exists(f))
  rd <- yaml::read_yaml(f)$readings
  tp <- vapply(rd, `[[`, "", "topic")
  for (t in c("A red or yellow B at the surface: its thickness (B1)", "Cutans not recorded in a structured B (C1)",
              "The 1977 humic A in the AfSP benchmark (A1)", "No dry colour and a light moist colour: not melanic (M1)")) {
    j <- match(t, tp)
    expect_false(is.na(j), info = t)
    expect_true(grepl("\\(Hugo, 2026-11-10\\)\\.$", rd[[j]]$reading), info = t)
  }
})
