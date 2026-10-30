# v0.9.317: a profile that declares the FAO notation is read by the Canadian
# key through a translation of its designations into the Canadian notation
# where the letters mean the same (CSSC Table 6, p. 154). Made-up profiles
# only.

.fao_pedon <- function(designation, ..., site = list()) {
  n <- length(designation)
  top <- c(0, cumsum(rep(15, n - 1L)))
  h <- data.table::as.data.table(data.frame(designation = designation, top_cm = top,
                                            bottom_cm = top + 15, ..., stringsAsFactors = FALSE))
  site <- utils::modifyList(list(id = "fao-test", country = "CA",
                                 designation_system = "fao2006"), site)
  PedonRecord$new(site = site, horizons = ensure_horizon_schema(h))
}

.fao_as <- function(d) {
  t <- soilKey:::.cssc_from_fao(d)$table
  stats::setNames(t$canadian, t$designation)
}

test_that("FAO designations are translated where the letters mean the same", {
  skip_on_cran()
  tr <- .fao_as(c("E", "AE", "EB", "BE", "Bw", "Bt", "Btn", "Bs", "Bhs", "Bh", "Bg", "Bl",
                  "Cr", "Ck", "Ckz", "Cy", "Cf", "Bi", "Bss", "Bkm", "Bsm", "Bx", "Hi", "He",
                  "Ha", "H", "O", "Oi", "Oe", "Oa", "I", "R", "2Bt1", "3C", "Bwt", "Bwg",
                  "B(t)", "Ah@", "Ap", "BCg"))
  expect_identical(unname(tr), c("Ae", "Ahe", "AB", "BA", "Bm", "Bt", "Bnt", "Bf", "Bhf", "Bh",
                                 "Bg", "Bg", "Cg", "Ck", "Csk", "Cs", "Cz", "Bss", "Bss", "Bck",
                                 "Bfc", "Bx", "Of", "Om", "Oh", "O", "LFH", "L", "F", "H", "Wz",
                                 "R", "IIBt1", "IIIC", "Bmtj", "Bmgj", "Btj", "Ahy", "Ap", "BCg"))
  # the letters with no Canadian equivalent are not read
  t <- soilKey:::.cssc_from_fao(c("Cgj", "Bv", "Bu", "Bo", "Ch", "Bkc", "L", "LFH"))$table
  expect_identical(t$canadian[1:6], c("Cg", "B", "B", "B", "C", "Bk"))
  expect_identical(t$not_read[1:6], c("j", "v", "u", "o", "h", "c"))
  # the limnic L (and anything that does not parse) is not translated
  expect_true(all(is.na(t$canadian[7:8])))
  expect_identical(soilKey:::.cssc_from_fao(c("Bt", "L", NA))$raw, c(FALSE, TRUE, FALSE))
  expect_true(soilKey:::.cssc_fao_declared("WRB 2022"))
  expect_false(soilKey:::.cssc_fao_declared("usda"))
})

test_that("an m is a cementation only after its cementing agent", {
  skip_on_cran()
  t <- soilKey:::.cssc_from_fao(c("Bm", "Cm", "Bkm", "Bsm", "Bqm", "Bym", "Bhsm"))
  expect_identical(t$table$canadian, c("B", "C", "Bck", "Bfc", "Bc", "Bcs", "Bhfc"))
  expect_identical(t$table$not_read[1:2], c("m", "m"))
  p <- t$comps
  # a bare m is not read: the Canadian c is absent and the Canadian m undecided
  expect_identical(soilKey:::.cssc_has_suffix(p, "c", "no"), c(FALSE, FALSE, rep(TRUE, 5)))
  expect_identical(soilKey:::.cssc_is(p, "B", "m", "any")[c(1, 3, 4)], c(NA, FALSE, FALSE))
  # the evidence says so
  bm <- .fao_pedon(c("Ap", "Bm", "Ck"))
  expect_match(soilKey:::.cssc_horizons(bm)$notation$why, "Bm = B (m not read)", fixed = TRUE)
  expect_true(is.na(soilKey:::brunisolic_b_cssc(bm)$passed))
  # a cemented B (Bsm) is read as cemented
  ort <- .fao_pedon(c("Ap", "E", "Bs", "Bsm", "C"))
  expect_true(soilKey:::.cssc_cemented(soilKey:::.cssc_horizons(ort))[4])
})

test_that("the FAO z, j, u and v never become the Canadian z, j, u and v", {
  skip_on_cran()
  cmp <- function(d) soilKey:::.cssc_from_fao(d)$comps[[1]][[1]]
  expect_false("z" %in% cmp("Ckz")$suffixes)
  expect_true("s" %in% cmp("Ckz")$suffixes)
  expect_length(cmp("Cgj")$weak, 0L)
  expect_false(any(c("u", "v") %in% c(cmp("Bu")$suffixes, cmp("Bv")$suffixes)))
  # salts (FAO z) are no permafrost; FAO f is
  salty <- .fao_pedon(c("Ah", "Btn", "Ckz1", "Ckz2"))
  frozen <- .fao_pedon(c("Ah", "Bw", "Cf1", "Cf2"))
  expect_false(soilKey:::cryosolic_cssc(salty)$passed)
  expect_true(soilKey:::cryosolic_cssc(frozen)$passed)
  expect_identical(classify_cssc3(frozen, on_missing = "silent")$rsg_or_order, "Cryosolic")
})

test_that("what the FAO notation cannot write is left open, not absent", {
  skip_on_cran()
  p <- soilKey:::.cssc_from_fao(c("A", "Ap", "B", "Bk", "Bt", "C", "Bwt", "Bg"))$comps
  expect_identical(soilKey:::.cssc_is(p, "A", "h", "any"), c(NA, FALSE, FALSE, FALSE, FALSE,
                                                             FALSE, FALSE, FALSE))
  expect_identical(soilKey:::.cssc_is(p, "A", "p", "any")[2], TRUE)
  expect_identical(soilKey:::.cssc_is(p, "B", "m", "any")[3:5], c(NA, NA, FALSE))
  expect_identical(soilKey:::.cssc_has_suffix(p, "k", "any")[c(4, 6)], c(TRUE, NA))
  expect_identical(soilKey:::.cssc_has_suffix(p, "v", "no")[c(3, 6)], c(NA, FALSE))
  # a t is read in its full form; whether it is a tj cannot be read, unless
  # rule 5 (w with t) says it is weak
  expect_identical(soilKey:::.cssc_is(p, "B", "t", "no")[c(5, 7)], c(TRUE, FALSE))
  expect_identical(soilKey:::.cssc_is(p, "B", "t", "yes")[c(5, 7)], c(NA, TRUE))
  expect_identical(soilKey:::.cssc_is(p, "B", "gf", "any")[8], NA)
  # a plain FAO B may be a Bm: the Brunisolic B is undecided, a Bw decides it
  expect_true(is.na(soilKey:::brunisolic_b_cssc(.fao_pedon(c("Ah", "B", "C")))$passed))
  expect_true(soilKey:::brunisolic_b_cssc(.fao_pedon(c("Ah", "Bw", "C")))$passed)
  # a fragipan is no Bm: the m "lacks a brittle consistence when moist"
  # (Ch 2, p. 14) and a fragipan is brittle when moist
  expect_identical(soilKey:::.cssc_is(soilKey:::.cssc_from_fao("Bx")$comps, "B", "m", "any"), FALSE)
  # the vertic horizon has no FAO letter
  vp <- .fao_pedon(c("Ah", "Bi", "Bik", "Ck"), clay_pct = c(60, 62, 64, 60),
                   slickensides = c(NA, "many", "many", NA))
  expect_true(is.na(soilKey:::vertisolic_cssc(vp)$passed))
  expect_true("orders:C" %in% classify_cssc3(vp, on_missing = "silent")$provisional_because)
})

test_that("an FAO A without h is tested by the Ah definition", {
  skip_on_cran()
  dark <- .fao_pedon(c("A", "Bw", "C"), munsell_value_moist = c(3, 5, 5),
                     oc_pct = c(2.5, 0.6, 0.3))
  pale <- .fao_pedon(c("A", "Bw", "C"), munsell_value_moist = c(5, 5, 5),
                     oc_pct = c(0.3, 0.3, 0.3))
  a1 <- soilKey:::.cssc_surface_a(soilKey:::.cssc_horizons(dark))
  a2 <- soilKey:::.cssc_surface_a(soilKey:::.cssc_horizons(pale))
  expect_identical(a1$rows, 1L)
  expect_identical(a2$rows, integer(0))
  # it is an A all the same, not a horizon tested for gleying
  gr <- soilKey:::.cssc_gley_rows(dark)
  expect_false(gr$eligible[1])
})

test_that("the weak forms the FAO notation cannot write stay undecided", {
  skip_on_cran()
  # the Ae conditions of the Chernozems: an E thicker than the Ah above it
  ae <- function(dv) .fao_pedon(c("Ah", "E", "Bw", "Ck"), munsell_value_dry = c(3, dv, 5, 6))
  thin <- .fao_pedon(c("Ah", "Ah", "E", "Bw"))
  expect_true(is.na(soilKey:::chernozemic_ae_cssc(ae(4))$passed))
  expect_false(soilKey:::chernozemic_ae_cssc(ae(6))$passed)
  expect_true(soilKey:::chernozemic_ae_cssc(thin)$passed)
  # an FAO Btn may be a weak solonetzic B (Btnj) of the Chernozems
  expect_true(is.na(soilKey:::weak_solonetzic_b_cssc(.fao_pedon(c("Ah", "Btn", "Ck")))$passed))
  expect_false(soilKey:::weak_solonetzic_b_cssc(.fao_pedon(c("Ah", "Bt", "Ck")))$passed)
})

test_that("a profile in the FAO notation reaches the classes of its Canadian twin", {
  skip_on_cran()
  fao <- list(cryosolic = c("Ah@", "Bw@", "Cf"), organic = c("Hi", "He", "Ha", "C"),
              podzolic = c("O", "E", "Bs", "BC", "C"), gleysolic = c("Ah", "Bg", "Ckg"),
              solonetzic = c("Ah", "Btn", "Ckz"), chernozemic = c("Ah", "Bw", "Ck"),
              luvisolic = c("O", "E", "Bt", "Ck"), brunisolic = c("Ah", "Bw", "C"),
              regosolic = c("Ah", "C"))
  for (o in names(fao)) {
    p <- make_cssc3_example(o)
    can <- classify_cssc3(p, on_missing = "silent")$name
    p$site$designation_system <- "fao2006"
    h <- data.table::copy(p$horizons); h$designation <- fao[[o]]; p$horizons <- h
    expect_identical(classify_cssc3(p, on_missing = "silent")$name, can, info = o)
  }
})

test_that("the designations are kept and the evidence lists their translation", {
  skip_on_cran()
  p <- .fao_pedon(c("O", "E", "Bt", "L", "Ck"), clay_pct = c(NA, 10, 25, NA, 22))
  d0 <- p$horizons$designation
  r <- classify_cssc3(p, on_missing = "silent")
  expect_identical(p$horizons$designation, d0)
  why <- soilKey:::bt_horizon_cssc(p)$evidence$notation
  expect_match(why, "fao2006", fixed = TRUE)
  expect_match(why, "E = Ae", fixed = TRUE)
  expect_match(why, "L (not translated)", fixed = TRUE)
  # the limnic L is still read by its master letter
  expect_true(soilKey:::.cssc_organic_column(p)$limnic[4])
  # without the declaration the same designations are not translated, and
  # only their master letters are read
  q <- .fao_pedon(c("Ah", "E", "Bw", "C"), site = list(designation_system = NULL))
  x <- soilKey:::.cssc_horizons(q)
  expect_false(x$notation$ok)
  expect_null(x$notation$translated)
  expect_true(is.na(soilKey:::brunisolic_b_cssc(q)$passed))
})
