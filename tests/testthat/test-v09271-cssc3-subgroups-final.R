# v0.9.271: the subgroups of the Solonetzic, Chernozemic and Luvisolic
# orders of the Canadian System of Soil Classification, 3rd ed. (Key to Soil
# Subgroups, Ch 3, pp. 42-49), the last 101 of the 231.

.sf <- function(h, site = list()) {
  site <- utils::modifyList(list(id = "t", country = "CA"), site)
  n <- max(lengths(h))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  p <- PedonRecord$new(site = site, horizons = ensure_horizon_schema(
    data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
  classify_cssc3(p, on_missing = "silent")
}
.sf_pedon <- function(h, site = list()) {
  site <- utils::modifyList(list(id = "t", country = "CA"), site)
  PedonRecord$new(site = site, horizons = ensure_horizon_schema(
    data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}

test_that("the rules list all 231 subgroups, 101 of them under F, G and H", {
  r <- load_rules("cssc3")
  n <- lengths(r$subgroups)
  expect_identical(sum(n), 231L)
  expect_setequal(names(r$subgroups), unlist(lapply(r$great_groups, function(g)
    vapply(g, `[[`, "", "code"))))
  want <- c(FA = 6L, FB = 10L, FC = 10L, FD = 7L, GA = 12L, GB = 12L, GC = 12L,
            GD = 10L, HA = 8L, HB = 14L)
  expect_identical(n[names(want)], want)
  codes <- unlist(lapply(r$subgroups, function(l) vapply(l, `[[`, "", "code")))
  expect_false(anyDuplicated(codes) > 0L)
  # the Solonetzic great groups have no last 'other' subgroup
  for (g in c("FA", "FB", "FC", "FD"))
    expect_false(any(vapply(r$subgroups[[g]], function(e) isTRUE(e$tests$default), TRUE)), info = g)
  # the Dark Gray Chernozems have no Eluviated subgroups
  expect_false(any(grepl("Eluviated", vapply(r$subgroups$GD, `[[`, "", "name"))))
  expect_length(yaml::read_yaml(system.file("rules", "cssc3", "readings.yaml",
                                            package = "soilKey"))$readings, 58L)  # 58 since v0.9.274
})

test_that("the example pedons of the three orders reach a subgroup", {
  skip_on_cran()
  want <- c(solonetzic = "Dark Brown Solonetz", chernozemic = "Orthic Dark Brown Chernozem",
            luvisolic = "Orthic Gray Luvisol")
  for (o in names(want)) {
    r <- classify_cssc3(make_cssc3_example(o), on_missing = "silent")
    expect_identical(r$name, want[[o]], info = o)
    expect_identical(r$levels$level, c("orders", "great_groups", "subgroups"), info = o)
    expect_length(r$provisional_because, 0L)
  }
})

solonetz <- function(designation = c("Ah", "Bnt", "Csk"), top_cm = c(0, 12, 35),
                     bottom_cm = c(12, 35, 90), vd = 5, cd = 2, ...) {
  n <- length(designation)
  h <- list(designation = designation, top_cm = top_cm, bottom_cm = bottom_cm,
            munsell_value_dry = c(vd, rep(5, n - 1)), munsell_chroma_dry = c(cd, rep(3, n - 1)),
            munsell_value_moist = c(3, rep(4, n - 1)), oc_pct = c(3, rep(0.6, n - 1)),
            ph_h2o = c(7, rep(8.2, n - 1)))
  utils::modifyList(h, list(...))
}
solod <- function(...) solonetz(designation = c("Ah", "Ae", "AB", "Bnt", "Csk"),
                                top_cm = c(0, 10, 14, 22, 40), bottom_cm = c(10, 14, 22, 40, 90), ...)
sub_humid <- list(soil_moisture_subclass = "subhumid")

test_that("Solonetzic subgroups follow the dry colour of the A", {
  skip_on_cran()
  expect_identical(.sf(solonetz())$name, "Brown Solonetz")
  expect_identical(.sf(solonetz(vd = 4))$name, "Dark Brown Solonetz")
  expect_identical(.sf(solonetz(vd = 3))$name, "Black Solonetz")
  # the Solonetz has no Gray subgroups: a low chroma stays Brown
  expect_identical(.sf(solonetz(cd = 1))$name, "Brown Solonetz")
  expect_identical(.sf(solod())$name, "Brown Solod")
  expect_identical(.sf(solod(vd = 4, cd = 1), sub_humid)$name, "Dark Gray Solod")
  expect_identical(.sf(solod(vd = 5, cd = 1), sub_humid)$name, "Gray Solod")
  expect_identical(.sf(solod(vd = 3, cd = 1), sub_humid)$name, "Black Solod")
  # a low chroma under a climate that is not subhumid stays Dark Brown
  expect_identical(.sf(solod(vd = 4, cd = 1), list(soil_moisture_subclass = "semiarid"))$name,
                   "Dark Brown Solod")
  # with no climate recorded it is undecided
  r <- .sf(solod(vd = 4, cd = 1))
  expect_identical(r$name, "Solod")
  expect_true(all(c("subgroups:FBD", "subgroups:FBH") %in% r$provisional_because))
  ssz <- function(...) solonetz(designation = c("Ah", "Ae", "Bnt", "Csk"),
                                top_cm = c(0, 10, 14, 40), bottom_cm = c(10, 14, 40, 90), ...)
  expect_identical(.sf(ssz())$name, "Brown Solodized Solonetz")
  expect_identical(.sf(ssz(vd = 4, cd = 1.5), sub_humid)$name, "Dark Gray Solodized Solonetz")
  vs <- function(...) solonetz(designation = c("Ah", "Bnt", "Bnss", "Csk"),
                               top_cm = c(0, 12, 35, 70), bottom_cm = c(12, 35, 70, 100), ...)
  expect_identical(.sf(vs())$name, "Brown Vertic Solonetz")
  expect_identical(.sf(vs(vd = 3))$name, "Black Vertic Solonetz")
  expect_identical(.sf(vs(vd = 5, cd = 1), sub_humid)$name, "Brown Vertic Solonetz")
})

test_that("Solonetzic subgroups: alkaline A, mottles, no A", {
  skip_on_cran()
  expect_identical(.sf(solonetz(ph_h2o = c(9, 9, 9)))$name, "Alkaline Solonetz")
  # the key's 'above 8.5'
  expect_identical(.sf(solonetz(ph_h2o = c(8.5, 9, 9)))$name, "Brown Solonetz")
  expect_identical(.sf(solonetz(designation = c("Ah", "Bnt", "Ckgj")))$name, "Gleyed Brown Solonetz")
  expect_identical(.sf(solonetz(mottle_contrast = c(NA, "faint", NA)))$name, "Gleyed Brown Solonetz")
  expect_identical(.sf(solonetz(vd = 3, mottle_contrast = c(NA, "distinct", NA)))$name,
                   "Gleyed Black Solonetz")
  # prominent mottles are not 'faint to distinct'
  expect_identical(.sf(solonetz(mottle_contrast = c(NA, "prominent", NA)))$name, "Brown Solonetz")
  expect_identical(.sf(solod(vd = 5, cd = 1, mottle_contrast = c(NA, "faint", NA, NA, NA)),
                       sub_humid)$name, "Gleyed Gray Solod")
  # eroded down to the solonetzic B: no colour class, no residual subgroup
  r <- .sf(list(designation = c("Bnt", "Csk"), top_cm = c(0, 25), bottom_cm = c(25, 90),
                munsell_value_dry = c(5, 6), oc_pct = c(0.8, 0.4)))
  expect_identical(r$rsg_or_order, "Solonetzic")
  expect_identical(r$name, "Solonetz")
  expect_length(r$provisional_because, 0L)
})

chernozem <- function(designation = c("Ah", "Bm", "Cca"), top_cm = c(0, 18, 40),
                      bottom_cm = c(18, 40, 90), vd = 4, ...) {
  n <- length(designation)
  h <- list(designation = designation, top_cm = top_cm, bottom_cm = bottom_cm,
            munsell_hue_moist = rep("10YR", n),
            munsell_value_moist = c(2, rep(3, n - 2), 5), munsell_chroma_moist = c(2, rep(3, n - 1)),
            munsell_value_dry = c(vd, rep(5, n - 2), 7), munsell_chroma_dry = c(2, rep(3, n - 1)),
            oc_pct = c(3, rep(1, n - 2), 0.4), n_total_pct = c(0.25, rep(0.09, n - 2), NA),
            bs_pct = c(95, rep(98, n - 2), 100), ca_cmol = rep(18, n), mg_cmol = rep(4, n),
            k_cmol = rep(1, n), na_cmol = rep(0.1, n), clay_pct = rep(25, n))
  utils::modifyList(h, list(...))
}
chern_site <- list(mean_annual_soil_temp_C = 4, soil_moisture_subclass = "subhumid")

test_that("Chernozemic subgroups", {
  skip_on_cran()
  .c <- function(...) .sf(chernozem(...), chern_site)$name
  expect_identical(.c(), "Orthic Dark Brown Chernozem")
  expect_identical(.c(vd = 5), "Orthic Brown Chernozem")
  expect_identical(.c(vd = 3), "Orthic Black Chernozem")
  expect_identical(.c(designation = c("Ah", "Bmss", "Cca")), "Vertic Dark Brown Chernozem")
  expect_identical(.c(designation = c("Ah", "Bmss", "Ckgj")), "Gleyed Vertic Dark Brown Chernozem")
  expect_identical(.c(designation = c("Ah", "Bnjtj", "Cca")), "Solonetzic Dark Brown Chernozem")
  expect_identical(.c(designation = c("Ah", "Bnjtj", "Ccagj")), "Gleyed Solonetzic Dark Brown Chernozem")
  expect_identical(.c(designation = c("Ah", "Ae", "Btj", "Cca"), top_cm = c(0, 12, 15, 40),
                      bottom_cm = c(12, 15, 40, 90)), "Eluviated Dark Brown Chernozem")
  expect_identical(.c(designation = c("Ah", "Bmk", "Cca")), "Calcareous Dark Brown Chernozem")
  expect_identical(.c(designation = c("Ah", "AC", "Cca"), top_cm = c(0, 18, 22),
                      bottom_cm = c(18, 22, 90)), "Rego Dark Brown Chernozem")
  expect_identical(.c(designation = c("Ah", "AC", "Ccagj"), top_cm = c(0, 18, 22),
                      bottom_cm = c(18, 22, 90)), "Gleyed Rego Dark Brown Chernozem")
  expect_identical(.c(designation = c("Ah", "Bmgj", "Cca")), "Gleyed Dark Brown Chernozem")
  expect_identical(.c(designation = c("Ahe", "Bmk", "Cca"), munsell_chroma_dry = c(1, 3, 3)),
                   "Calcareous Dark Gray Chernozem")
  # no Eluviated Dark Gray Chernozem: an Ae leaves it Orthic
  expect_identical(.c(designation = c("Ahe", "Ae", "Bt", "Cca"), top_cm = c(0, 15, 20, 40),
                      bottom_cm = c(15, 20, 40, 90), munsell_chroma_dry = c(1, 2, 3, 3)),
                   "Orthic Dark Gray Chernozem")
})

test_that("Calcareous Chernozems read measured carbonates first", {
  skip_on_cran()
  .c <- function(...) .sf(chernozem(...), chern_site)$name
  expect_identical(.c(caco3_pct = c(0, 2, 10)), "Calcareous Dark Brown Chernozem")
  # a trace in a Bm designated without k
  expect_identical(.c(caco3_pct = c(0, 0.2, 10)), "Orthic Dark Brown Chernozem")
  # none measured in a Bmk
  expect_identical(.c(designation = c("Ah", "Bmk", "Cca"), caco3_pct = c(0, 0, 10)),
                   "Orthic Dark Brown Chernozem")
  # 4 cm of Bmk is not enough
  expect_identical(.c(designation = c("Ah", "Bmk", "Bm", "Cca"), top_cm = c(0, 18, 22, 40),
                      bottom_cm = c(18, 22, 40, 90)), "Orthic Dark Brown Chernozem")
})

luvisol <- function(designation = c("LFH", "Ae", "Bt", "Ck"), top_cm = c(0, 4, 20, 60),
                    bottom_cm = c(4, 20, 60, 100), ...) {
  h <- list(designation = designation, top_cm = top_cm, bottom_cm = bottom_cm,
            oc_pct = c(35, 0.5, 0.4, 0.3), clay_pct = c(NA, 12, 28, 26),
            clay_films_amount = c(NA, "none", "common", NA),
            munsell_hue_moist = c(NA, "10YR", "10YR", "2.5Y"),
            munsell_value_moist = c(NA, 6, 4, 5), munsell_chroma_moist = c(NA, 2, 3, 3))
  utils::modifyList(h, list(...))
}
podzolic_luvisol <- function(...) {
  h <- list(designation = c("LFH", "Ae", "Bf", "Ae2", "Bt", "Ck"),
            top_cm = c(0, 4, 10, 22, 30, 60), bottom_cm = c(4, 10, 22, 30, 60, 100),
            oc_pct = c(35, 0.5, 1.2, 0.4, 0.4, 0.3), clay_pct = c(NA, 12, 13, 12, 28, 26),
            clay_films_amount = c(NA, "none", "none", "none", "common", NA),
            munsell_hue_moist = c(NA, "10YR", "7.5YR", "10YR", "10YR", "2.5Y"),
            munsell_value_moist = c(NA, 6, 4, 6, 4, 5), munsell_chroma_moist = c(NA, 2, 6, 2, 3, 3),
            fe_pyro_pct = c(NA, 0.05, 0.6, 0.05, 0.1, 0.05),
            al_pyro_pct = c(NA, 0.05, 0.4, 0.05, 0.1, 0.05))
  utils::modifyList(h, list(...))
}
brunisolic_luvisol <- function(...) {
  h <- list(designation = c("LFH", "Bm", "Ae", "Bt", "Ck"), top_cm = c(0, 4, 12, 20, 60),
            bottom_cm = c(4, 12, 20, 60, 100), oc_pct = c(35, 0.8, 0.5, 0.4, 0.3),
            clay_pct = c(NA, 13, 12, 28, 26), clay_films_amount = c(NA, "none", "none", "common", NA),
            munsell_hue_moist = c(NA, "10YR", "10YR", "10YR", "2.5Y"),
            munsell_value_moist = c(NA, 4, 6, 4, 5), munsell_chroma_moist = c(NA, 4, 2, 3, 3))
  utils::modifyList(h, list(...))
}
warm <- list(mean_annual_soil_temp_C = 9)

test_that("Gray Luvisol subgroups", {
  skip_on_cran()
  expect_identical(.sf(luvisol())$name, "Orthic Gray Luvisol")
  expect_identical(.sf(luvisol(c("LFH", "Ae", "Btss", "Ck")))$name, "Vertic Gray Luvisol")
  expect_identical(.sf(luvisol(c("LFH", "Ae", "Btss", "Ck"),
                               mottle_contrast = c(NA, NA, "faint", NA)))$name,
                   "Gleyed Vertic Gray Luvisol")
  expect_identical(.sf(luvisol(c("LFH", "Ae", "Bt", "BCx")))$name, "Fragic Gray Luvisol")
  expect_identical(.sf(luvisol(c("LFH", "Ae", "Btgj", "BCx")))$name, "Gleyed Fragic Gray Luvisol")
  expect_identical(.sf(podzolic_luvisol())$name, "Podzolic Gray Luvisol")
  expect_identical(.sf(podzolic_luvisol(designation = c("LFH", "Ae", "Bf", "Ae2", "Btgj", "Ck")))$name,
                   "Gleyed Podzolic Gray Luvisol")
  dg <- luvisol(c("Ah", "Ae", "Bt", "Ck"), top_cm = c(0, 6, 20, 60), bottom_cm = c(6, 20, 60, 100),
                oc_pct = c(4, 0.5, 0.4, 0.3), munsell_value_moist = c(3, 6, 4, 5))
  expect_identical(.sf(dg)$name, "Dark Gray Luvisol")
  dg$designation[3] <- "Btgj"
  expect_identical(.sf(dg)$name, "Gleyed Dark Gray Luvisol")
  # an Ah of 4 cm is not enough
  thin <- luvisol(c("Ah", "Ae", "Bt", "Ck"), oc_pct = c(4, 0.5, 0.4, 0.3),
                  munsell_value_moist = c(3, 6, 4, 5))
  expect_identical(.sf(thin)$name, "Orthic Gray Luvisol")
  expect_identical(.sf(luvisol(c("LFH", "Ae", "Btnj", "Ck")))$name, "Solonetzic Gray Luvisol")
  expect_identical(.sf(luvisol(c("LFH", "Ae", "Btnjgj", "Ck")))$name, "Gleyed Solonetzic Gray Luvisol")
  expect_identical(.sf(brunisolic_luvisol())$name, "Brunisolic Gray Luvisol")
  expect_identical(.sf(brunisolic_luvisol(mottle_contrast = c(NA, NA, NA, "distinct", NA)))$name,
                   "Gleyed Brunisolic Gray Luvisol")
  # a Bm of chroma 2 is not Brunisolic
  expect_identical(.sf(brunisolic_luvisol(munsell_chroma_moist = c(NA, 2, 2, 3, 3)))$name,
                   "Orthic Gray Luvisol")
  # a Bf of 8 cm ending at 14 cm is the Brunisolic 'thin Bf'
  expect_identical(.sf(podzolic_luvisol(top_cm = c(0, 4, 6, 14, 30, 60),
                                        bottom_cm = c(4, 6, 14, 30, 60, 100)))$name,
                   "Brunisolic Gray Luvisol")
  expect_identical(.sf(luvisol(c("LFH", "Ae", "Btgj", "Ck")))$name, "Gleyed Gray Luvisol")
  # prominent mottles at 50-100 cm; faint ones within 50 cm are not enough
  expect_identical(.sf(luvisol(mottle_contrast = c(NA, NA, NA, "prominent")))$name, "Gleyed Gray Luvisol")
  expect_identical(.sf(luvisol(mottle_contrast = c(NA, NA, "faint", NA)))$name, "Orthic Gray Luvisol")
})

test_that("Gray Brown Luvisol subgroups", {
  skip_on_cran()
  gbl <- function(...) luvisol(c("Ah", "Ae", "Bt", "Ck"), top_cm = c(0, 8, 20, 60),
                               bottom_cm = c(8, 20, 60, 100), oc_pct = c(4, 0.5, 0.4, 0.3),
                               munsell_value_moist = c(3, 6, 4, 5), ...)
  expect_identical(.sf(gbl(), warm)$name, "Orthic Gray Brown Luvisol")
  expect_identical(.sf(gbl(mottle_contrast = c(NA, "distinct", NA, NA)), warm)$name,
                   "Gleyed Gray Brown Luvisol")
  expect_identical(.sf(gbl(designation = c("Ah", "Ae", "Btss", "Ck")), warm)$name,
                   "Vertic Gray Brown Luvisol")
  expect_identical(.sf(gbl(designation = c("Ah", "Ae", "Btssgj", "Ck")), warm)$name,
                   "Gleyed Vertic Gray Brown Luvisol")
  pz <- podzolic_luvisol(designation = c("Ah", "Ae", "Bf", "Ae2", "Bt", "Ck"),
                         top_cm = c(0, 6, 10, 22, 30, 60), bottom_cm = c(6, 10, 22, 30, 60, 100),
                         oc_pct = c(4, 0.5, 1.2, 0.4, 0.4, 0.3), munsell_value_moist = c(3, 6, 4, 6, 4, 5))
  expect_identical(.sf(pz, warm)$name, "Podzolic Gray Brown Luvisol")
  pz$designation[5] <- "Btgj"
  expect_identical(.sf(pz, warm)$name, "Gleyed Podzolic Gray Brown Luvisol")
  bl <- brunisolic_luvisol(designation = c("Ah", "Bm", "Ae", "Bt", "Ck"),
                           top_cm = c(0, 6, 12, 20, 60), bottom_cm = c(6, 12, 20, 60, 100),
                           oc_pct = c(4, 0.8, 0.5, 0.4, 0.3), munsell_value_moist = c(3, 4, 6, 4, 5))
  expect_identical(.sf(bl, warm)$name, "Brunisolic Gray Brown Luvisol")
  bl$designation[4] <- "Btgj"
  expect_identical(.sf(bl, warm)$name, "Gleyed Brunisolic Gray Brown Luvisol")
})

test_that("weak solonetzic B and carbonates in other notations", {
  skip_on_cran()
  h <- chernozem(designation = c("A", "Bw", "Bk"), caco3_pct = c(0, 0, 8))
  p <- .sf_pedon(h, chern_site)
  # USDA notation: Bw and Bk have no n, so no weak solonetzic B
  expect_false(soilKey:::weak_solonetzic_b_cssc(p)$passed)
  # a B carrying an n has no weak form there: undecided
  p2 <- .sf_pedon(utils::modifyList(h, list(designation = c("A", "Bwn", "Bk"))), chern_site)
  expect_true(is.na(soilKey:::weak_solonetzic_b_cssc(p2)$passed))
  # a Bk may or may not be a Bmk in that notation
  expect_true(is.na(soilKey:::calcareous_b_cssc(p)$passed))
  # Canadian notation: a Btn is strong, a Btnj is weak
  p3 <- .sf_pedon(utils::modifyList(h, list(designation = c("Ah", "Btn", "Ck"))), chern_site)
  expect_false(soilKey:::weak_solonetzic_b_cssc(p3)$passed)
  p4 <- .sf_pedon(utils::modifyList(h, list(designation = c("Ah", "Btnj", "Ck"))), chern_site)
  expect_true(soilKey:::weak_solonetzic_b_cssc(p4, t = TRUE)$passed)
  p5 <- .sf_pedon(utils::modifyList(h, list(designation = c("Ah", "Bnj", "Ck"))), chern_site)
  expect_false(soilKey:::weak_solonetzic_b_cssc(p5, t = TRUE)$passed)
  expect_true(soilKey:::weak_solonetzic_b_cssc(p5)$passed)
})

test_that("review fixes: open bottoms, climate words, partial colours", {
  skip_on_cran()
  # a Bmk with no bottom depth may go on: Calcareous and Rego are undecided
  h <- chernozem(designation = c("Ah", "Bmk"), top_cm = c(0, 18), bottom_cm = c(18, NA))
  r <- .sf(h, chern_site)
  expect_identical(r$name, "Orthic Dark Brown Chernozem")
  expect_true(all(c("subgroups:GBH", "subgroups:GBJ") %in% r$provisional_because))
  # a B of 4 cm over one with no bottom: the Rego Gleysol is undecided
  expect_true(is.na(soilKey:::no_thick_b_cssc(.sf_pedon(list(
    designation = c("Ah", "Bg", "Bg"), top_cm = c(0, 10, 14), bottom_cm = c(10, 14, NA))))$passed))
  # the subhumid climate: other subclasses of the scale fail, aquic ones and
  # unknown words leave it undecided
  sh <- function(w) soilKey:::.cssc_subhumid(list(soil_moisture_subclass = w))
  expect_true(sh("Sub-humid"))
  expect_false(sh("semiarid"))
  expect_false(sh("humid"))
  expect_true(is.na(sh("subaquic")))
  expect_true(is.na(sh("")))
  expect_true(is.na(sh(c(NA, "x"))))
  expect_identical(.sf(solod(vd = 4, cd = 1), list(soil_moisture_subclass = "aquic"))$name, "Solod")
  # the colour of the whole A: 22 of 25 cm without a dry value
  r <- .sf(solonetz(designation = c("Ah", "Ahe", "Bnt", "Csk"), top_cm = c(0, 3, 25, 45),
                    bottom_cm = c(3, 25, 45, 80), munsell_value_dry = c(3, NA, 5, 6),
                    munsell_chroma_dry = c(2, NA, 2, 3)))
  expect_identical(r$name, "Solonetz")
  expect_true("subgroups:FDG" %in% r$provisional_because)
  # an unknown value that cannot change the class does not matter
  expect_identical(.sf(solonetz(designation = c("Ah", "Ahe", "Bnt", "Csk"), top_cm = c(0, 10, 11, 45),
                                bottom_cm = c(10, 11, 45, 80), munsell_value_dry = c(2.5, NA, 5, 6),
                                munsell_chroma_dry = c(2, NA, 2, 3)))$name, "Black Solonetz")
  # the gray split reads the moist chroma when the dry one is missing
  ssz <- solonetz(designation = c("Ahe", "Ae", "Bnt", "Csk"), top_cm = c(0, 10, 14, 40),
                  bottom_cm = c(10, 14, 40, 90), vd = 4, munsell_chroma_dry = c(NA, 3, 3, 3),
                  munsell_chroma_moist = c(1, 2, 3, 3))
  expect_identical(.sf(ssz, sub_humid)$name, "Dark Gray Solodized Solonetz")
})

test_that("review fixes: forest Gray Solods, alkaline and eluvial horizons", {
  skip_on_cran()
  # LFH over an Ae: the Gray subgroups read the Ae (Ch 12)
  fs <- list(designation = c("LFH", "Ae", "AB", "Bnt", "Csk"), top_cm = c(0, 5, 17, 27, 55),
             bottom_cm = c(5, 17, 27, 55, 90), munsell_value_dry = c(NA, 6, 5, 5, 6),
             munsell_chroma_dry = c(NA, 1, 2, 3, 3), oc_pct = c(40, 0.5, 0.5, 0.5, 0.3))
  expect_identical(.sf(fs, sub_humid)$name, "Gray Solod")
  # a dark Ae is no class: the key and Ch 12 give the Dark Gray subgroups an Ah, Ahe or Ap
  fs$munsell_value_dry <- c(NA, 4, 5, 5, 6)
  expect_identical(.sf(fs, sub_humid)$name, "Solod")
  # an undesignated horizon that a longer surface A may take in
  p <- .sf_pedon(list(designation = c("Ah", NA, "Bnt", "Csk"), top_cm = c(0, 30, 45, 70),
                      bottom_cm = c(30, 45, 70, 100), ph_h2o = c(7.5, 9, 8.4, 8.6),
                      munsell_value_dry = c(4, NA, 5, 6), munsell_chroma_dry = c(2, NA, 2, 3)))
  expect_true(is.na(soilKey:::alkaline_a_cssc(p)$passed))
  # a buried Ahb is not the A of the soil; an E in another notation is an A
  p <- .sf_pedon(list(designation = c("Ah", "Bnt", "Ahb", "Csk"), top_cm = c(0, 12, 35, 50),
                      bottom_cm = c(12, 35, 50, 90), ph_h2o = c(7, 8, 9.2, 8.6)))
  expect_false(soilKey:::alkaline_a_cssc(p)$passed)
  p <- .sf_pedon(list(designation = c("A", "E", "Btn", "Bk"), top_cm = c(0, 10, 20, 50),
                      bottom_cm = c(10, 20, 50, 90), ph_h2o = c(7, 9, 8, 8)))
  expect_true(soilKey:::alkaline_a_cssc(p)$passed)
  # a buried Aeb is not an eluvial horizon of the soil
  h <- chernozem(designation = c("Ah", "Bm", "Aeb", "Cca"), top_cm = c(0, 18, 40, 45),
                 bottom_cm = c(18, 40, 45, 90))
  expect_false(soilKey:::eluvial_horizon_cssc(.sf_pedon(h, chern_site))$passed)
  expect_false(soilKey:::ae_horizon_cssc(.sf_pedon(h, chern_site))$passed)
  # carbonates in the A do not make a Bmk
  p <- .sf_pedon(list(designation = rep(NA_character_, 3), top_cm = c(0, 15, 40),
                      bottom_cm = c(15, 40, 90), caco3_pct = c(5, 0, 0),
                      munsell_value_moist = c(2, 4, 5), oc_pct = c(3, 0.8, 0.3)))
  expect_false(soilKey:::calcareous_b_cssc(p)$passed)
})

test_that("review fixes: Btnj, podzolic B above the Bt, thin Bf", {
  skip_on_cran()
  w <- function(d) soilKey:::weak_solonetzic_b_cssc(.sf_pedon(list(
    designation = c("Ae", d, "C"), top_cm = c(0, 10, 40), bottom_cm = c(10, 40, 80))), t = TRUE)$passed
  expect_true(w("Btnj"))
  expect_false(w("Bnjtj"))
  expect_false(w("Bnj/Bt"))
  # a deeper, thicker IIBf does not hide the Bf above the Bt
  pz <- podzolic_luvisol(designation = c("LFH", "Ae", "Bf", "Ae2", "Bt", "Ck"))
  pz <- lapply(pz, function(v) c(v, v[6]))
  pz$designation[6:7] <- c("IIBf", "IIC"); pz$top_cm[6:7] <- c(60, 95); pz$bottom_cm[6:7] <- c(95, 120)
  pz$fe_pyro_pct[6] <- 0.8; pz$al_pyro_pct[6] <- 0.5
  pz$munsell_hue_moist[6] <- "7.5YR"; pz$munsell_chroma_moist[6] <- 6; pz$munsell_value_moist[6] <- 4
  pz$clay_films_amount[6:7] <- NA
  expect_identical(.sf(pz)$name, "Podzolic Gray Luvisol")
  # an open Bf does not stop the test
  expect_no_error(soilKey:::brunisolic_luvisol_cssc(.sf_pedon(list(
    designation = c("Ae", "Bf"), top_cm = c(0, 10), bottom_cm = c(10, NA)))))
})

test_that("second review fixes: possible Ae horizons, buried and open rows", {
  skip_on_cran()
  # a light undesignated horizon under the Ah may be an Ae: undecided
  p <- .sf_pedon(list(designation = c("Ah", NA, "Bn"), top_cm = c(0, 10, 14),
                      bottom_cm = c(10, 14, 40), munsell_value_moist = c(2, 5, 3.5),
                      oc_pct = c(4, 0.5, 0.6), ph_h2o = c(7, 9, 7)))
  expect_true(is.na(soilKey:::alkaline_a_cssc(p)$passed))
  # below the B it is not an A
  p <- .sf_pedon(list(designation = c("Ah", "Bn", NA), top_cm = c(0, 10, 40),
                      bottom_cm = c(10, 40, 80), ph_h2o = c(7, 8, 9)))
  expect_false(soilKey:::alkaline_a_cssc(p)$passed)
  # an undesignated horizon at the mineral surface may be the Ae: undecided
  fs <- list(designation = c("LFH", NA, "Ae", "AB", "Bnt", "Csk"), top_cm = c(0, 5, 7, 17, 27, 55),
             bottom_cm = c(5, 7, 17, 27, 55, 90), munsell_value_dry = c(NA, 6, 6, 5, 5, 6),
             munsell_chroma_dry = c(NA, 1, 1, 3, 3, 4), oc_pct = c(35, 0.5, 0.5, 0.6, 0.6, 0.3))
  expect_true(is.na(soilKey:::solonetzic_a_colour_cssc(.sf_pedon(fs, sub_humid), "gray")$passed))
  # a buried Aeb under the surface Ae is not read
  fs$designation <- c("LFH", "Ae", "Aeb", "AB", "Bnt", "Csk")
  fs$top_cm <- c(0, 5, 10, 17, 27, 55); fs$bottom_cm <- c(5, 10, 17, 27, 55, 90)
  fs$munsell_value_dry <- c(NA, 6, 3, 5, 5, 6)
  expect_true(soilKey:::solonetzic_a_colour_cssc(.sf_pedon(fs, sub_humid), "gray")$passed)
  # a Bf with no bottom may or may not be thin
  bf <- function(bottom) soilKey:::brunisolic_luvisol_cssc(.sf_pedon(list(
    designation = c("Ae", "Bf"), top_cm = c(0, 10), bottom_cm = c(10, bottom))))$passed
  expect_true(is.na(bf(NA)))
  expect_true(bf(14))
  # a buried Btnjb is not the Btnj of the soil
  expect_false(soilKey:::weak_solonetzic_b_cssc(.sf_pedon(list(
    designation = c("Ae", "Bt", "Btnjb"), top_cm = c(0, 10, 40), bottom_cm = c(10, 40, 80))), t = TRUE)$passed)
})
