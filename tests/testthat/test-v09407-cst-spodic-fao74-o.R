# v0.9.407 -- the CST spodic horizon reviewed on ISIS (the cementation sign,
# the colours below an E layer) and the O layers of the 1974 H horizon
# (Hugo, 2026-11-13). Every profile below is synthetic.

.p407 <- function(..., site = list(id = "p407")) {
  cols <- list(...)
  n <- max(lengths(cols))
  h <- make_empty_horizons(n)
  for (k in names(cols)) data.table::set(h, j = k, value = rep_len(cols[[k]], n))
  suppressWarnings(PedonRecord$new(site = site, horizons = h))
}

test_that("a hard dry consistence is no cementation sign of the spodic horizon", {
  skip_on_cran()
  mk <- function(rr = NA_character_, cem = NA_character_)
    .p407(designation = c("Ah", "Bt1", "Bt2"), top_cm = c(0, 15, 40), bottom_cm = c(15, 40, 80),
          clay_pct = c(15, 25, 27), ph_h2o = 5.5, oc_pct = c(1.8, 0.7, 0.4),
          munsell_hue_moist = c("10YR", "7.5YR", "7.5YR"), munsell_value_moist = c(4, 4, 5),
          munsell_chroma_moist = c(3, 4, 6), rupture_resistance = c("hard", rr, rr),
          cementation_class = c(NA, cem, cem))
  sp <- function(p) soilKey:::.cst_hz(soilKey:::.cst_ctx(p), "spodic_horizon")$full[2]
  # very hard when dry: not a sign, so the red-brown Bt below an A stays
  # undecided (no oxalate data), no longer spodic
  expect_false(isTRUE(sp(mk("very hard"))))
  expect_false(isTRUE(sp(mk("extremely hard"))))
  # a cementation class still is
  expect_true(sp(mk(cem = "moderately")))
  # the other CST readers keep the dry rupture resistance
  x <- soilKey:::.cst_ctx(mk("very hard"))
  expect_true(soilKey:::.cst_cemented(x)$value[2])
  expect_false(soilKey:::.cst_cemented(x, rupture = FALSE)$value[2])
})

test_that("the colours alone count below an E layer only with an albic horizon in its run", {
  skip_on_cran()
  mk <- function(e_hue, e_v, e_c, e_vd, eb = FALSE) {
    d <- c("Ah", "E", if (eb) "EB", "Bs", "C")
    n <- length(d)
    .p407(designation = d, top_cm = c(0, 10, if (eb) 25, 30, 60)[seq_len(n)],
          bottom_cm = c(10, if (eb) 25 else 30, if (eb) 30, 60, 100)[seq_len(n)],
          ph_h2o = 4.8, oc_pct = c(3, 0.4, if (eb) 0.3, 0.8, 0.2), clay_pct = 8,
          munsell_hue_moist = c("10YR", e_hue, if (eb) "7.5YR", "5YR", "10YR"),
          munsell_value_moist = c(3, e_v, if (eb) 5, 5, 6),
          munsell_chroma_moist = c(2, e_c, if (eb) 6, 6, 4),
          munsell_value_dry = c(5, e_vd, if (eb) 6, 6, 7))
  }
  sp <- function(p) spodic_horizon_cst(p)$passed
  # an E whose recorded colours are not albic (10YR 5/6): no longer spodic
  expect_false(isTRUE(sp(mk("10YR", 5, 6, 6))))
  # a bleached, albic E: the colours alone decide, as before
  expect_true(sp(mk("10YR", 6, 2, 7)))
  # an albic E, then an EB that is not albic, then the Bs (a Humic Podzol)
  expect_true(sp(mk("10YR", 6, 2, 7, eb = TRUE)))
  # an E with no colours recorded leaves the albic horizon undecided: it counts
  expect_true(sp(mk(NA_character_, NA_real_, NA_real_, NA_real_)))
})

test_that("an O with a suffix of decomposition takes the drainage of the site in 1974", {
  skip_on_cran()
  sat <- function(desg, drainage = NULL) {
    site <- c(list(id = "o"), if (!is.null(drainage)) list(drainage_class = drainage))
    p <- .p407(designation = c(desg, "C"), top_cm = c(0, 50), bottom_cm = c(50, 100),
               oc_pct = c(30, 0.5), clay_pct = c(NA, 25), site = site)
    soilKey:::.f74_saturated(soilKey:::.f74_ctx(p))[1]
  }
  for (d in c("Oa", "Oe", "Oi", "Of", "Om", "Oh", "2Oa")) {
    expect_true(sat(d, "very poorly drained"), info = d)
    expect_false(sat(d, "well drained"), info = d)
    expect_true(is.na(sat(d)), info = d)
  }
  # an O without a suffix is the O of p. 21: not saturated whatever the site
  for (d in c("O", "O1", "O2")) expect_false(sat(d, "very poorly drained"), info = d)
  expect_true(sat("H", "well drained"))
  # the canonical Histosol (Oa, Oe, very poorly drained) keys as a Histosol
  r <- suppressWarnings(classify_fao(make_histosol_canonical(), edition = "1974", on_missing = "silent"))
  expect_identical(r$name, "Dystric Histosols")
})

test_that("the readings of 0.9.407 are recorded with Hugo's date", {
  skip_on_cran()
  for (sys in c("cst2001", "fao1974")) {
    rd <- yaml::read_yaml(system.file("rules", sys, "readings.yaml", package = "soilKey"))$readings
    r407 <- Filter(function(r) identical(r$version, "0.9.407"), rd)
    expect_length(r407, if (sys == "cst2001") 2L else 1L)
    for (r in r407) expect_match(r$reading, "\\(Hugo, 2026-11-13\\)\\.$")
  }
})
