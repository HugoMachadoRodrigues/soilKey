# v0.9.377: two order-level fixes of the Chinese Soil Taxonomy (2001):
# gley colours without a test in a well-drained profile (G-1 to G-3) and the
# fine clay of argic item 2b (A-1 to A-3), decided by Hugo on 2026-11-08.

.c377 <- function(p) classify_cst2001(p, on_missing = "silent")
.f377 <- function(fn, p, ...) get(fn, envir = asNamespace("soilKey"))(p, ...)
.wd377 <- function(dc) soilKey:::.cst_well_drained(if (is.na(dc)) list() else list(drainage_class = dc))
.alt377 <- function(r) if (is.data.frame(r$candidates) && nrow(r$candidates)) r$candidates$name[1] else NA_character_

# A over a brown B and C; only the A colour and the site vary.
.gp377 <- function(v = 4, c = 2, site = list(drainage_class = "well drained"), extra = list()) {
  h <- data.frame(top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 120), designation = c("A", "Bw", "C"),
                  munsell_hue_moist = c("10YR", "7.5YR", "7.5YR"), munsell_value_moist = c(v, 5, 5),
                  munsell_chroma_moist = c(c, 6, 6), clay_pct = c(20, 22, 20), sand_pct = c(40, 38, 40),
                  silt_pct = c(40, 40, 40), oc_pct = c(2, 0.5, 0.2), ph_h2o = c(6, 6, 6),
                  cec_cmol = c(15, 12, 10), bs_pct = c(60, 60, 60), stringsAsFactors = FALSE)
  for (k in names(extra)) h[[k]] <- extra[[k]]
  PedonRecord$new(site = c(list(id = "cst377"), site), horizons = h)
}

# The M5 example (A over a Bw, no clay films) with the clay changed.
.ap377 <- function(a, b, fine = c(NA, NA), films = NULL) {
  p <- make_cst2001_example("M5")$clone(deep = TRUE)
  h <- p$horizons
  h$clay_pct <- c(a, b, b, NA)
  h$fine_clay_pct <- c(fine, NA, NA)
  if (!is.null(films)) {
    h$clay_films_amount <- c(NA, films[1], NA, NA)
    h$clay_films_strength <- c(NA, films[2], NA, NA)
  }
  p$horizons <- h
  p
}

test_that("the drainage reader takes the excessively well drained wordings (G-3)", {
  skip_on_cran()
  for (dc in c("well drained", "excessively drained", "excessively well drained", "somewhat excessively drained",
               "somewhat excessively well drained", "Somewhat Excessively Well Drained", "rapidly drained",
               "bem drenado", "acentuadamente drenado"))
    expect_true(.wd377(dc), info = dc)
  for (dc in c("moderately well drained", "somewhat poorly drained", "poorly drained", "very poorly drained", NA))
    expect_false(.wd377(dc), info = dc)
  # the saturation axis reads it: no saturation regime
  sat <- soilKey:::.cst_moisture_regime(.gp377(site = list(drainage_class = "excessively well drained")))$saturation
  expect_identical(sat$candidates, "none")
})

test_that("gley colours alone are not gleyic in a well-drained profile, any layer (G-1)", {
  skip_on_cran()
  g <- function(p) .f377("gleyic_features_cst", p)
  # well drained or better: FALSE (three-valued: decided, not undecided)
  for (dc in c("well drained", "excessively well drained", "somewhat excessively well drained")) {
    r <- g(.gp377(site = list(drainage_class = dc)))
    expect_false(r$passed, info = dc)
  }
  # moderately well drained, poorly drained, or not recorded: provisional (Q11)
  for (s in list(list(drainage_class = "moderately well drained"), list(drainage_class = "poorly drained"), list())) {
    r <- g(.gp377(site = s))
    expect_true(r$passed)
    expect_true(isTRUE(r$evidence$provisional))
  }
  # well drained with redox features or a recorded aquic regime: provisional, as before
  r <- g(.gp377(extra = list(mottle_colors = c("7.5YR 5/8", NA, NA))))
  expect_true(r$passed)
  r <- g(.gp377(site = list(drainage_class = "well drained", soil_moisture_regime = "aquic")))
  expect_true(r$passed)
  # a positive ferrous-iron test is the book's proof: firm under any drainage
  r <- g(.gp377(extra = list(ferricyanide_test = c("positive", NA, NA))))
  expect_true(r$passed)
  expect_false(isTRUE(r$evidence$provisional))
  # a value of 3 was never a gley colour (item 1d)
  expect_false(g(.gp377(v = 3, site = list()))$passed)
})

test_that("a humus-grey A keys as Cambosols when well drained, Gleyosols provisionally otherwise (G-1, G-2)", {
  skip_on_cran()
  for (dc in c("well drained", "excessively well drained")) {
    r <- .c377(.gp377(site = list(drainage_class = dc)))
    expect_identical(r$rsg_or_order, "Cambosols", info = dc)
    expect_false(any(grepl("Gleyosols", c(r$warnings, .alt377(r)))))
  }
  for (v_c in list(c(4, 1), c(5, 1))) {
    r <- .c377(.gp377(v = v_c[1], c = v_c[2]))
    expect_identical(r$rsg_or_order, "Cambosols")
  }
  for (s in list(list(), list(drainage_class = "poorly drained"), list(drainage_class = "moderately well drained"))) {
    r <- .c377(.gp377(site = s))
    expect_identical(r$rsg_or_order, "Gleyosols")
    expect_true(any(grepl("^Provisional: I Gleyosols .*ferrous-iron test", r$warnings)))
    expect_match(.alt377(r), "Cambosols$")
  }
  # the H2.2 example (ustic, well drained) loses its colour-only Gleyic subgroup
  r <- .c377(make_cst2001_example("H2.2"))
  expect_identical(r$name, "Typic Aqui-Orthic Halosols")
  expect_false(any(grepl("ferrous-iron", r$warnings)))
})

test_that("argic item 2b asks more fine clay where it is measured (A-1)", {
  skip_on_cran()
  arg <- function(p) .f377("argic_horizon_cst", p)
  # 18 to 21.6 % (1.2 times), no fine clay: argic provisionally
  r <- arg(.ap377(18, 21.6))
  expect_true(r$passed)
  expect_true(isTRUE(r$evidence$provisional))
  expect_true("argic_increase_without_fine_clay" %in% r$evidence$flags)
  # fine clay in both, not higher: no argic horizon (decided)
  expect_false(arg(.ap377(18, 21.6, fine = c(8, 8)))$passed)
  expect_false(arg(.ap377(18, 21.6, fine = c(9, 8)))$passed)
  # fine clay higher (only higher: the third more of p. 36 is "normally", A-3)
  r <- arg(.ap377(18, 21.6, fine = c(8, 8.5)))
  expect_true(r$passed)
  expect_false(isTRUE(r$evidence$provisional))
  # fine clay in one layer only: as not measured
  r <- arg(.ap377(18, 21.6, fine = c(NA, 10)))
  expect_true(r$passed)
  expect_true(isTRUE(r$evidence$provisional))
  # no total-clay increase: fails whatever the fine clay
  expect_false(arg(.ap377(22, 25))$passed)
  expect_false(arg(.ap377(22, 25, fine = c(5, 15)))$passed)
  # under 15 % (3 points) and 40-60 % (8 points) read the fine clay the same way
  expect_true(isTRUE(arg(.ap377(10, 13))$evidence$provisional))
  expect_false(arg(.ap377(10, 13, fine = c(4, 3)))$passed)
  expect_false(isTRUE(arg(.ap377(45, 53, fine = c(20, 30)))$evidence$provisional))
  expect_true(arg(.ap377(45, 53, fine = c(20, 30)))$passed)
  # 60 % or more: the 8 points of fine clay, as before (undecided without it)
  expect_true(is.na(arg(.ap377(62, 70))$passed))
  expect_true(arg(.ap377(62, 70, fine = c(30, 40)))$passed)
  # clay films (item 2a, the printed "or", A-2) make it firm without fine clay
  r <- arg(.ap377(18, 21.6, films = c("common", "distinct")))
  expect_true(r$passed)
  expect_false(isTRUE(r$evidence$provisional))
})

test_that("a 1.2 times increase without fine clay gives Argosols provisionally, Cambosols the alternative", {
  skip_on_cran()
  r <- .c377(.ap377(18, 21.6))
  expect_identical(r$rsg_or_order, "Argosols")
  expect_true(any(grepl("^Provisional: L Argosols .*fine clay", r$warnings)))
  expect_match(.alt377(r), "Cambosols$")
  expect_identical(r$candidates$instead_of[1], "L")
  # fine clay measured, not higher: Cambosols, nothing provisional about it
  r <- .c377(.ap377(18, 21.6, fine = c(8, 8)))
  expect_identical(r$rsg_or_order, "Cambosols")
  expect_true(is.na(.alt377(r)))
  # fine clay higher: Argosols, firm
  r <- .c377(.ap377(18, 21.6, fine = c(8, 10)))
  expect_identical(r$rsg_or_order, "Argosols")
  expect_false(any(grepl("rests on a reading", r$warnings)))
  expect_true(is.na(.alt377(r)))
  # the example itself (22 to 25 %) stays Cambosols
  expect_identical(.c377(make_cst2001_example("M5"))$name, "Typic Purpli-Udic Cambosols")
})

test_that("the argic and alkalic examples keep their names, firm on their fine clay (A-1)", {
  skip_on_cran()
  nm <- c(H1.1.1 = "Parasalic Takyri-Alkalic Halosols", H1.2.2 = "Typic Aqui-Alkalic Halosols",
          H1.3.2 = "Typic Hapli-Alkalic Halosols", K1.1.1 = "Typic Argi-Ustic Ferrosols",
          K2.2.8 = "Argic Alliti-Perudic Ferrosols", G2.3.5 = "Argic Gypsi-Orthic Aridosols")
  for (k in names(nm)) {
    p <- make_cst2001_example(k)
    r <- .c377(p)
    expect_identical(r$name, nm[[k]], info = k)
    expect_false(any(grepl("rests on a reading", r$warnings)), info = k)
    # the same profile without its fine clay: the same name, provisionally
    h <- p$horizons; h$fine_clay_pct <- NA_real_; p$horizons <- h
    r <- .c377(p)
    expect_identical(r$name, nm[[k]], info = k)
    expect_true(any(grepl("fine clay", r$warnings)), info = k)
    expect_false(is.na(.alt377(r)), info = k)
  }
  # K1.1.1 without fine clay: the alternative is the Hapli- group
  p <- make_cst2001_example("K1.1.1"); h <- p$horizons; h$fine_clay_pct <- NA_real_; p$horizons <- h
  expect_identical(.alt377(.c377(p)), "Typic Hapli-Ustic Ferrosols")
  # an any_of gate with a firm alternative is firm: the salic horizon of the
  # Sali-Orthic Aridosols carries the order whatever the argic reading
  p <- make_cst2001_example("G2.2"); h <- p$horizons; h$fine_clay_pct <- NA_real_; p$horizons <- h
  r <- .c377(p)
  expect_identical(r$name, "Typic Sali-Orthic Aridosols")
  expect_false(any(grepl("^Provisional: G Aridosols", r$warnings)))
  # para-alkalic evidence, read on its own items, stays firm (M3.3.2)
  r <- .c377(make_cst2001_example("M3.3.2"))
  expect_false(any(grepl("fine clay", r$warnings)))
})

# --- the review of 0.9.377 (Hugo, 2026-11-08) ---------------------------------------------------

test_that("drainage ranges take their worst class; hyphens and underscores are spaces (F4)", {
  skip_on_cran()
  for (dc in c("Well-drained", "well_drained", "Somewhat-excessively-drained", "excessively  well drained"))
    expect_true(.wd377(dc), info = dc)
  for (dc in c("well drained to moderately well drained", "well drained (somewhat poorly in places)",
               "well-drained to imperfectly drained", "bem drenado a moderadamente drenado"))
    expect_false(.wd377(dc), info = dc)
  # a range keeps the colour-only gleyic reading, provisionally
  r <- .f377("gleyic_features_cst", .gp377(site = list(drainage_class = "well drained to moderately well drained")))
  expect_true(r$passed)
  expect_false(.f377("gleyic_features_cst", .gp377(site = list(drainage_class = "Well-drained")))$passed)
})

test_that("redox features are read per layer: none values, g and r suffixes (F5)", {
  skip_on_cran()
  g <- function(p) .f377("gleyic_features_cst", p)
  # a mottle recorded as none is no redox feature: the guard holds
  for (v in c("none", "absent", "nil", "-", "0", " None "))
    expect_false(g(.gp377(extra = list(mottle_colors = c(v, NA, NA))))$passed, info = v)
  expect_false(soilKey:::.cst_mottles_recorded("none"))
  expect_true(soilKey:::.cst_mottles_recorded("7.5YR 5/8"))
  # and beside a shallow water table it is no redox sign either
  h <- data.frame(designation = c("A", "Bw"), top_cm = c(0, 30), bottom_cm = c(30, 80), mottle_colors = c("none", NA))
  p <- PedonRecord$new(site = list(id = "r"), horizons = h)
  expect_false(isTRUE(soilKey:::.cst_redox_above(p, 50)))
  h$mottle_colors <- c("10YR 5/6", NA)
  p <- PedonRecord$new(site = list(id = "r"), horizons = h)
  expect_true(soilKey:::.cst_redox_above(p, 50))
  # a g or r suffix is a recorded redox feature of that layer: provisional
  expect_identical(soilKey:::.cst_redox_suffix(c("Bg", "Btg1", "2Cg", "Cr", "Apg", "Ap", "Bw", "A2", "R", NA)),
                   c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))
  p <- .gp377(extra = list(designation = c("Ag", "Bw", "C")))
  r <- g(p)
  expect_true(r$passed)
  expect_true(isTRUE(r$evidence$provisional))
  # per layer: mottles in the C do not protect the A
  r <- g(.gp377(extra = list(mottle_colors = c(NA, NA, "7.5YR 5/8"))))
  expect_false(r$passed)
})

test_that("the fine clay is compared with the eluvial layers the total clay increases over (F6)", {
  skip_on_cran()
  p <- make_cst2001_example("M5")$clone(deep = TRUE)
  h <- as.data.frame(p$horizons)[c(1, 1, 2, 3), ]
  h$designation <- c("A1", "A2", "Bw", "C"); h$top_cm <- c(0, 10, 20, 60); h$bottom_cm <- c(10, 20, 60, 100)
  h$clay_pct <- c(18, 19, 23, 20); h$fine_clay_pct <- c(NA, 8, 8, NA)
  p$horizons <- h
  expect_false(.f377("argic_horizon_cst", p)$passed)
  # higher than the one measured: firm
  h$fine_clay_pct <- c(NA, 8, 9, NA); p$horizons <- h
  r <- .f377("argic_horizon_cst", p)
  expect_true(r$passed)
  expect_false(isTRUE(r$evidence$provisional))
  # the only layer measured is not one the total clay increases over (20 to 23: 1.15 times): provisional
  h$clay_pct <- c(18, 20, 23, 20); h$fine_clay_pct <- c(NA, 8, 8, NA); p$horizons <- h
  r <- .f377("argic_horizon_cst", p)
  expect_true(r$passed)
  expect_true(isTRUE(r$evidence$provisional))
})

test_that("the fine-clay alternative comes from the whole key run again (F1, F2, F3, F8)", {
  skip_on_cran()
  # F1: an Aridosol whose argic horizon is provisional
  g <- make_cst2001_example("G2"); h <- g$horizons
  h$caso4_pct <- 0.5; h$gypsum_visible_pct <- 0; h$caco3_pct <- 1; h$secondary_carbonates_pct <- 0; h$fine_clay_pct <- NA_real_
  p <- g$clone(deep = TRUE); p$horizons <- h
  r <- .c377(p)
  expect_identical(r$name, "Typic Argi-Orthic Aridosols")
  expect_identical(.alt377(r), "Typic Hapli-Orthic Aridosols")
  expect_identical(r$candidates$instead_of[1], "G2.4")
  expect_true(any(grepl("^Provisional: G2.4 Argi-Orthic Aridosols .*fine clay", r$warnings)))
  expect_false(any(grepl("^Provisional: G Aridosols", r$warnings)))
  h$fine_clay_pct <- 4; p$horizons <- h
  expect_identical(.c377(p)$name, "Typic Hapli-Orthic Aridosols")
  # F2: a provisional argic horizon blocks the Spodosols of a profile keyed firmly as Gleyosols
  p0 <- make_cst2001_example("C2")
  h <- as.data.frame(p0$horizons)[c(1, 2, 2, 3), ]
  h$designation <- c("E", "Bt", "Bs", "C"); h$top_cm <- c(0, 12, 30, 60); h$bottom_cm <- c(12, 30, 60, 100)
  h$clay_pct <- c(4, 7.5, 6, 3); h$fe_ox_pct[2] <- 0.1; h$al_ox_pct[2] <- 0.1; h$oc_pct[2] <- 0.5
  h$ferricyanide_test <- c("positive", NA, NA, NA)
  mk <- function(h) PedonRecord$new(site = utils::modifyList(p0$site, list(id = "e4")), horizons = h)
  r <- .c377(mk(h))
  expect_identical(r$rsg_or_order, "Gleyosols")
  expect_true(any(grepl("^Provisional: I Gleyosols .*fine clay", r$warnings)))
  expect_match(.alt377(r), "Spodosols$")
  h$fine_clay_pct <- c(2, 2, 2, 1)
  r <- .c377(mk(h))
  expect_identical(r$rsg_or_order, "Spodosols")
  expect_false(any(grepl("fine clay", r$warnings)))
  # F3: argic with lime infiltrates passes the flags of the argic horizon
  p <- make_cst2001_example("G1.1.2")$clone(deep = TRUE); h <- as.data.frame(p$horizons)
  h$fine_clay_pct <- NA_real_; h$clay_films_amount <- NA; h$clay_films_strength <- NA
  p$horizons <- h
  a <- .f377("argic_lime_cst", p)
  expect_true(a$passed)
  expect_true(isTRUE(a$evidence$provisional))
  expect_true("argic_increase_without_fine_clay" %in% a$evidence$flags)
  r <- .c377(p)
  expect_identical(r$name, "Argic Calci-Cryic Aridosols")
  expect_identical(.alt377(r), "Typic Calci-Cryic Aridosols")
  h$fine_clay_pct <- 5; p$horizons <- h
  expect_identical(.c377(p)$name, "Typic Calci-Cryic Aridosols")
  # F8: a class provisional only by gley colour gets no fine-clay rerun or remedy
  r <- .c377(.gp377(site = list()))
  expect_false(any(grepl("fine clay", r$warnings)))
  expect_true(any(grepl("ferrous-iron test", r$warnings)))
})
