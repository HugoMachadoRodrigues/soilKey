# v0.9.350: the French Referentiel pedologique 2008 -- the FE horizon of the
# ferruginosols on the data (pp. 17, 170-171, 179). In the intertropical
# domain the illuviation cutans separate the BT from the FE (p. 170): with
# the clay films not recorded soilKey's argic() no longer makes a BT of a
# layer that holds FE (no BT at 5YR or redder; undecided at 7.5YR, within
# the tolerance or with the FE itself undecided), a BT mark of another
# notation kept (D1, D2); a CEC of 16 cmol+/kg of clay or less with no clay
# mineralogy recorded is no FS (D3). Hugo's decisions on the review
# (2026-11-04): the field route of the FS needs a strong angular blocky
# structure (decision 3), and a BT under an FE needs more clay than the FE
# (decision 4). Every reading confirmed in readings.yaml.

# A profile on the data, in Burkina Faso (Koppen Aw) unless the site says
# otherwise; the exchangeable Mg, K and Na of a sandy savanna soil.
.rp50_D <- function(top, bot, hue, v, c, clay, sand, oc, ph, cec, ca, st, gr, films = NA, ..., site = list()) {
  h <- data.frame(top_cm = top, bottom_cm = bot, stringsAsFactors = FALSE)
  a <- list(munsell_hue_moist = hue, munsell_value_moist = v, munsell_chroma_moist = c, clay_pct = clay,
            sand_pct = sand, silt_pct = 100 - clay - sand, oc_pct = oc, ph_h2o = ph, cec_cmol = cec, ca_cmol = ca,
            mg_cmol = 0.4, k_cmol = 0.1, na_cmol = 0.05, structure_type = st, structure_grade = gr,
            clay_films_amount = films, redoximorphic_features_pct = 0, ...)
  for (k in names(a)) h[[k]] <- rep_len(a[[k]], nrow(h))
  s <- utils::modifyList(list(id = "t50", country = "BF", lat = 11.2, lon = -4.3, koppen_climate = "Aw"), site)
  old <- options(soilKey.warn_unrecognised = FALSE)
  on.exit(options(old))
  PedonRecord$new(site = s, horizons = ensure_horizon_schema(data.table::as.data.table(h)))
}
.rp50_cl <- function(p) suppressWarnings(classify_rp2008(p, on_missing = "silent"))
.rp50_codes <- function(p) .rp_identify(p)$code
.rp50_cands <- function(p, k) .rp_identify(p)$candidates[[k]]
.rp50_FR <- list(country = "FR", lat = 46, lon = 2, koppen_climate = "Cfb")
.rp50_readings <- c("FE against BT on the data: the clay films (0.9.350)",
                    "FS in the intertropical domain: a kaolinitic clay (0.9.350)",
                    "FS on the field route: a strong angular structure (0.9.350, decision 3)",
                    "The clay increase of a BT under an FE (0.9.350, decision 4)")
# The layers of the reviewer's repros: A / B / C in Burkina Faso, the B
# 2.5YR 4/6, clay 15 over 6 %, carbon 0.15 %, bases 1.45, CEC 4, no films.
.rp50_abc <- function(des = NULL, ph = 6.3, hue = "2.5YR", site = list()) {
  p <- .rp50_D(c(0, 15, 100), c(15, 100, 200), hue = c("7.5YR", hue, "10YR"), v = c(5, 4, 6), c = c(4, 6, 4),
               clay = c(6, 15, 12), sand = c(76, 66, 60), oc = c(0.5, 0.15, 0.05), ph = c(6.1, ph, 6.0),
               cec = c(2.5, 4, 3), ca = c(1, 0.9, 0.8), st = c("massive", "subangular blocky", "massive"),
               gr = c("weak", "weak", NA), site = site)
  if (!is.null(des)) p$horizons$designation <- des
  p
}

# The book's sequences of pp. 173-174 written as data: the A of p. 171
# (7.5YR 5/4, carbon 0.6 %, little clay), the FE of pp. 170-171 (5YR 5/6,
# carbon under 0.3 %, pH 6.0-6.5, bases under 5 cmol+/kg, S/CEC under 50 %),
# the BT of the ferruginosols (7.5YR 5/5 or 10YR 5/5, kaolinitic, clay
# films common), the Ea (10YR 7/4, sandy).
.rp50_meuble <- function(site = list())
  .rp50_D(c(0, 15, 90, 160), c(15, 90, 160, 220), hue = c("7.5YR", "5YR", "7.5YR", "10YR"), v = c(5, 5, 5, 6),
          c = c(4, 6, 4, 4), clay = c(8, 14, 16, 12), sand = c(72, 66, 62, 60), oc = c(0.6, 0.2, 0.1, 0.05),
          ph = c(6.2, 6.2, 6.0, 6.0), cec = c(3, 4, 3.5, 3), ca = c(1.2, 1, 1, 0.8),
          st = c("massive", "subangular blocky", "subangular blocky", "massive"), gr = c("weak", "weak", "moderate", NA),
          films = c(NA, NA, "none", NA), fe_total_pct = c(3.5, 6, 5, 4), saprolite_pct = c(NA, NA, NA, 60), site = site)
.rp50_semiluvique <- function(site = list())
  .rp50_D(c(0, 15, 60, 130, 180), c(15, 60, 130, 180, 200), hue = c("7.5YR", "5YR", "7.5YR", "10YR", NA),
          v = c(5, 5, 5, 6, NA), c = c(4, 6, 5, 4, NA), clay = c(8, 15, 35, 20, NA), sand = c(72, 65, 50, 60, NA),
          oc = c(0.6, 0.2, 0.1, 0.05, NA), ph = c(6.2, 6.2, 6.0, 6.0, NA), cec = c(3, 4, 4.5, 3, NA), ca = c(1.2, 1, 1.5, 1, NA),
          st = c("massive", "subangular blocky", "subangular blocky", "massive", NA), gr = c("weak", "weak", "moderate", NA, NA),
          films = c(NA, NA, "common", NA, NA), saprolite_pct = c(NA, NA, NA, 60, NA),
          rupture_resistance = c(NA, NA, NA, NA, "extremely hard"), coarse_fragments_pct = c(5, 5, 5, 20, 100), site = site)
.rp50_luvique <- function(site = list())
  .rp50_D(c(0, 12, 50, 80, 140), c(12, 50, 80, 140, 200), hue = c("7.5YR", "5YR", "10YR", "10YR", "10YR"),
          v = c(5, 5, 7, 5, 6), c = c(4, 6, 4, 5, 4), clay = c(10, 18, 8, 32, 20), sand = c(70, 62, 80, 50, 60),
          oc = c(0.6, 0.2, 0.1, 0.1, 0.05), ph = c(6.2, 6.2, 6.2, 6.0, 6.0), cec = c(3, 4, 2, 4.5, 3), ca = c(1.5, 1, 0.5, 1.5, 1),
          st = c("massive", "subangular blocky", "massive", "subangular blocky", "massive"), gr = c("weak", "weak", "weak", "moderate", NA),
          films = c(NA, NA, "none", "common", NA), site = site)
# A / FE / C with no clay film recorded anywhere; the FE of hue `hue`.
.rp50_nofilms <- function(hue = "2.5YR", ph = 6.3, films = NA, cec = 4, ca = 0.9, site = list())
  .rp50_D(c(0, 15, 100), c(15, 100, 200), hue = c("7.5YR", hue, "10YR"), v = c(5, if (hue == "2.5YR") 4 else 5, 6),
          c = c(4, 6, 4), clay = c(6, 15, 12), sand = c(76, 66, 60), oc = c(0.5, 0.15, 0.05), ph = c(6.1, ph, 6.0),
          cec = c(2.5, cec, 3), ca = c(1, ca, 0.8), st = c("massive", "subangular blocky", "massive"), gr = c("weak", "weak", NA),
          films = c(NA, films, NA), saprolite_pct = c(NA, NA, 60), site = site)


# --- the probes of part 6 (Burkina Faso, data only) --------------------------------------

test_that("the ferruginosols of the probes attach on the data: their FE is no longer a BT", {
  skip_on_cran()
  G <- function(clay = c(8, 25, 30, 20), films = NA)
    .rp50_D(c(0, 12, 70, 140), c(12, 70, 140, 200), hue = c("7.5YR", "5YR", "5YR", "10YR"), v = c(5, 5, 5, 6),
            c = c(4, 6, 6, 4), clay = clay, sand = c(70, 60, 55, 60), oc = c(0.6, 0.2, 0.1, 0.05),
            ph = c(6.2, 6.2, 6.0, 6.0), cec = c(3, 4, 4, 4), ca = c(1.5, 1, 1, 1),
            st = c("massive", "subangular blocky", "subangular blocky", "massive"), gr = c("weak", "weak", "moderate", NA),
            films = films, fe_total_pct = c(NA, 5, NA, NA))
  # d_gm: no clay film recorded; the 5YR FE over the A depleted of clay
  gm <- G()
  # the 5YR layer under it holds FE too: no BT, the fixed order goes on to
  # the F tested before the FE
  expect_identical(.rp50_codes(gm), c("A", "FE", "F", "C"))
  r <- .rp50_cl(gm)
  expect_identical(r$levels$code, "ferruginosols_meubles")
  expect_identical(r$name, "ferruginosol meuble sans horizon S")
  # d_gs: FE over a BT with common clay films, kaolinitic (CEC 11 per kg of
  # clay): the BT stays a BT, not an FSt
  gs <- G(clay = c(8, 15, 35, 20), films = c(NA, NA, "common", NA))
  expect_identical(.rp50_codes(gs), c("A", "FE", "BT", "C"))
  expect_identical(.rp50_cl(gs)$name, "ferruginosol semiluvique")
  # d_gs2: few clay films in the FE make no BT (the cutans, p. 170)
  gs2 <- G(clay = c(8, 15, 35, 20), films = c(NA, "few", "common", NA))
  expect_identical(.rp50_codes(gs2), c("A", "FE", "BT", "C"))
  expect_identical(.rp50_cl(gs2)$name, "ferruginosol semiluvique")
})


# --- the book's sequences as data (pp. 173-174) ------------------------------------------

test_that("the sequences of pp. 173-174 written as data attach to their references", {
  skip_on_cran()
  m <- .rp50_meuble()
  expect_identical(.rp50_codes(m), c("A", "FE", "S", "C"))
  r <- .rp50_cl(m)
  expect_identical(r$levels$code, "ferruginosols_meubles")
  expect_identical(r$name, "ferruginosol meuble")
  s <- .rp50_semiluvique()
  expect_identical(.rp50_codes(s), c("A", "FE", "BT", "F", "R"))
  expect_identical(.rp50_cl(s)$name, "ferruginosol semiluvique")
  l <- .rp50_luvique()
  expect_identical(.rp50_codes(l), c("A", "FE", "E", "BT", "F"))
  expect_identical(.rp50_cl(l)$name, "ferruginosol luvique")
  # A / FE / C, the FE 2.5YR 4/6, no clay film recorded: the imperfect case
  # of p. 173
  n <- .rp50_nofilms("2.5YR")
  expect_identical(.rp50_codes(n), c("A", "FE", "C"))
  r <- .rp50_cl(n)
  expect_identical(r$name, "ferruginosol meuble sans horizon S")
  expect_identical(r$levels$attachment, "imperfect")
})

test_that("the clay films recorded decide; not recorded, a 7.5YR or an undecided FE leaves FE or BT", {
  skip_on_cran()
  # 7.5YR 5/6: the colour of an FE and near the BT of the ferruginosols
  u <- .rp50_nofilms("7.5YR")
  expect_true(is.na(.rp50_codes(u)[2]))
  expect_true(all(c("BT", "FE") %in% .rp50_cands(u, 2)))
  r <- .rp50_cl(u)
  expect_length(r$levels$code, 0L)
  expect_identical(r$candidates$degree[r$candidates$code == "ferruginosols_meubles"], "undecided")
  # the pH of the FE not recorded: FE undecided, so FE or BT
  u2 <- .rp50_nofilms("5YR", ph = NA)
  expect_true(is.na(.rp50_codes(u2)[2]))
  expect_true(all(c("BT", "FE") %in% .rp50_cands(u2, 2)))
  # few clay films recorded: no BT, the FE
  expect_identical(.rp50_codes(.rp50_nofilms("7.5YR", films = "few"))[2], "FE")
  # common clay films recorded: the cutans of an illuviation, a BT (p. 170),
  # kaolinitic (CEC 14 per kg of clay) so not an FS; no FE, no ferruginosol
  b <- .rp50_nofilms("5YR", films = "common", cec = 2.5, ca = 0.7)
  expect_identical(.rp50_codes(b)[2], "BT")
  expect_true("FE" %in% .rp50_cands(b, 2))
  expect_false(any(grepl("^ferruginosols", .rp50_cl(b)$levels$code)))
})


# --- the twins: outside the domain, and the fersialsols, luvisols, ferrallitisols --------

test_that("outside the intertropical domain nothing changes: no FE, the BT read as before", {
  skip_on_cran()
  expect_identical(.rp50_codes(.rp50_semiluvique(site = .rp50_FR))[1:3], c("A", "BT", "BT"))
  expect_identical(.rp50_codes(.rp50_nofilms("2.5YR", site = .rp50_FR)), c("A", "BT", "C"))
  for (p in list(.rp50_semiluvique(site = .rp50_FR), .rp50_nofilms("2.5YR", site = .rp50_FR), .rp50_meuble(site = .rp50_FR)))
    expect_false(any(grepl("^ferruginosols", .rp50_cl(p)$levels$code)))
})

test_that("the twins stay fersialsols, luvisols and ferrallitisols", {
  skip_on_cran()
  BR <- list(country = "BR", lat = -21, lon = -47, koppen_climate = "Aw", parent_material = "basalt")
  fs <- function(cec, site = BR)
    .rp50_D(c(0, 20, 90), c(20, 90, 150), hue = c("5YR", "2.5YR", "5YR"), v = c(3, 3, 4), c = c(3, 6, 6),
            clay = c(40, 50, 45), sand = c(20, 15, 20), oc = c(2.0, 0.5, 0.2), ph = c(6.6, 6.8, 7.0), cec = cec,
            ca = c(14, 13, 13), st = c("granular", "angular blocky", "angular blocky"), gr = c("moderate", "strong", "strong"),
            bs_pct = c(65, 75, 75), caco3_pct = 0, site = site)
  # a tropical fersialsol on basalt, 2:1 clays (CEC 40 per kg of clay),
  # saturated: no FE, its FS kept
  r <- fs(c(25, 20, 20))
  expect_identical(.rp50_codes(r), c("A", "FS", "FS"))
  expect_identical(.rp50_cl(r)$name, "fersialsol insatur\u00e9")
  # the same FS with a kaolinitic clay (CEC 12 per kg of clay) is no FS in
  # the domain; in Provence it stays one
  k <- fs(c(25, 6, 6))
  expect_false("FS" %in% .rp50_codes(k))
  expect_true(any(grepl("kaolinitic", .rp_t_FS(.rp_view(k), 2L)$note)))
  expect_identical(.rp50_codes(fs(c(25, 6, 6), site = list(country = "FR", lat = 43.5, lon = 5, koppen_climate = "Csa"))),
                   c("A", "FS", "FS"))
  # a tropical luvisol: its BT 7.5YR with common films, saturated (pH 6.9):
  # no FE
  lu <- .rp50_D(c(0, 15, 40, 110), c(15, 40, 110, 160), hue = c("10YR", "10YR", "7.5YR", "10YR"), v = c(4, 6, 5, 6),
                c = c(3, 3, 6, 4), clay = c(15, 12, 30, 22), sand = c(55, 60, 45, 50), oc = c(1.2, 0.4, 0.3, 0.1),
                ph = c(6.6, 6.6, 6.9, 7.0), cec = c(10, 6, 14, 10), ca = c(7, 4, 10, 7),
                st = c("granular", "platy", "subangular blocky", "massive"), gr = c("moderate", "weak", "moderate", NA),
                films = c(NA, "none", "common", NA), bs_pct = c(80, 80, 85, 85),
                site = list(country = "SN", lat = 13, lon = -15, koppen_climate = "Aw"))
  expect_identical(.rp50_codes(lu)[2:3], c("E", "BT"))
  expect_identical(.rp50_cl(lu)$name, "luvisol typique")
  # a ferrallitisol: acid (pH 4.9), desaturated, its clay films none
  fo <- .rp50_D(c(0, 15, 80), c(15, 80, 200), hue = c("5YR", "2.5YR", "2.5YR"), v = c(3, 4, 4), c = c(3, 6, 6),
                clay = c(40, 50, 50), sand = c(45, 38, 38), oc = c(2.2, 0.4, 0.4), ph = c(4.6, 4.9, 4.9), cec = c(6, 3.5, 3.5),
                ca = c(0.3, 0.15, 0.15), st = c("granular", "subangular blocky", "subangular blocky"), gr = c("moderate", "weak", "weak"),
                films = "none", sio2_sulfuric_pct = 18, al2o3_sulfuric_pct = 17, ph_kcl = c(4.0, 4.3, 4.3), bs_pct = c(10, 6, 6),
                site = list(country = "CI", lat = 6.8, lon = -5.3, koppen_climate = "Aw"))
  expect_identical(.rp50_codes(fo), c("A", "F", "F"))
  expect_identical(.rp50_cl(fo)$name, "ferrallitisol meuble")
})


# --- the readings ------------------------------------------------------------------------

test_that("D1: a BT mark of another notation keeps the BT", {
  skip_on_cran()
  # A / B2t / C: the B2t holds the FE on its data, no films recorded
  b2t <- .rp50_abc(c("A", "B2t", "C"))
  expect_identical(.rp50_codes(b2t), c("A", "BT", "C"))
  expect_true("FE" %in% .rp50_cands(b2t, 2))
  expect_identical(.rp50_codes(.rp50_abc(c("A1", "B1-Bt", "C")))[2], "BT")
  # the same layers with no designation, or a Bw: the FE
  expect_identical(.rp50_codes(.rp50_abc()), c("A", "FE", "C"))
  expect_identical(.rp50_codes(.rp50_abc(c("Ap", "Bw", "C")))[2], "FE")
})

test_that("D2: an FE held only within the tolerance leaves FE or BT undecided", {
  skip_on_cran()
  # pH 5.9 (6.0 - 0.2): the W10 of the review is no brunisol dystrique
  w10 <- .rp50_abc(ph = 5.9, hue = "5YR")
  expect_true(is.na(.rp50_codes(w10)[2]))
  expect_true(all(c("BT", "FE") %in% .rp50_cands(w10, 2)))
  r <- .rp50_cl(w10)
  expect_length(r$levels$code, 0L)
  expect_false(identical(r$name, "brunisol dystrique"))
  # pH 6.3 at the same hue: the FE, simply
  expect_identical(.rp50_codes(.rp50_abc(hue = "5YR"))[2], "FE")
})

test_that("D3: a recorded clay mineralogy wins over the CEC of the clay", {
  skip_on_cran()
  BR <- list(country = "BR", lat = -21, lon = -47, koppen_climate = "Aw", parent_material = "basalt")
  x2 <- function(cm) .rp50_D(c(0, 20, 90), c(20, 90, 150), hue = c("5YR", "2.5YR", "5YR"), v = c(3, 3, 4), c = c(3, 6, 6),
                             clay = c(40, 50, 45), sand = c(20, 15, 20), oc = c(2, 0.5, 0.2), ph = c(7.2, 7.4, 7.5),
                             cec = c(25, 7, 6.3), ca = c(14, 6, 5.5), st = c("granular", "angular blocky", "angular blocky"),
                             gr = c("moderate", "strong", "moderate"), bs_pct = c(90, 95, 95), caco3_pct = 0,
                             clay_mineralogy = cm, site = BR)
  # smectite recorded, CEC 14 per kg of clay: the FS stays
  expect_identical(.rp50_codes(x2(c(NA, "smectite", "smectite")))[2], "FS")
  expect_true(grepl("^fersialsol", .rp50_cl(x2(c(NA, "smectite", "smectite")))$name))
  # no mineralogy recorded: the CEC reads a kaolinitic clay, no FS
  expect_false("FS" %in% .rp50_codes(x2(NA)))
})

test_that("decision 3: the field route of the FS needs a strong angular blocky structure", {
  skip_on_cran()
  MED <- list(country = "FR", lat = 43.5, lon = 5, koppen_climate = "Csa", parent_material = "limestone")
  fs <- function(st, gr, ...) .rp50_D(c(0, 20, 70), c(20, 70, 100), hue = c("5YR", "2.5YR", "2.5YR"), v = c(3, 4, 4),
                                      c = c(3, 6, 6), clay = c(30, 40, 40), sand = 20, oc = c(2.5, 0.5, 0.5),
                                      ph = c(7.0, 7.2, 7.2), cec = 25, ca = c(20, 22, 22), st = c("granular", st, st),
                                      gr = c("moderate", gr, gr), bs_pct = c(92, 95, 95), caco3_pct = 0, site = MED, ...)
  expect_identical(.rp50_codes(fs("angular blocky", "strong"))[2:3], c("FS", "FS"))
  expect_identical(.rp50_cl(fs("angular blocky", "strong"))$name, "fersialsol calcique")
  # moderate angular: the FS imperfect, the fersialsol attaches as imperfect
  m <- fs("angular blocky", "moderate")
  expect_identical(.rp50_codes(m)[2:3], c("FS", "FS"))
  expect_true(all(.rp_identify(m)$imperfect[2:3]))
  r <- .rp50_cl(m)
  expect_identical(r$levels$code, "fersialsols_calciques")
  expect_identical(r$levels$attachment, "imperfect")
  # weak angular, subangular, prismatic alone: no FS by the field route
  for (a in list(c("angular blocky", "weak"), c("subangular blocky", "strong"), c("prismatic", "strong")))
    expect_false("FS" %in% .rp50_codes(fs(a[1], a[2])), info = paste(a, collapse = " "))
  # the grade not recorded: undecided
  expect_true(is.na(.rp_t_FS(.rp_view(fs("angular blocky", NA)), 2L)$passed))
  # the FFE measured: the structure read as before (moderate passes, simply)
  f2 <- fs("angular blocky", "moderate", fe_total_pct = 6, fe_ffe_pct = 1.5)
  expect_identical(.rp50_codes(f2)[2], "FS")
  expect_false(.rp_identify(f2)$imperfect[2])
  # the reviewer's X3: a BT of CEC 17 to 24 per kg of clay under an FE, its
  # structure moderate subangular: no FSt, a ferruginosol semiluvique (p. 171)
  for (cec in c(6, 7.7, 8.5)) {
    p <- .rp50_D(c(0, 12, 70, 140), c(12, 70, 140, 200), hue = c("7.5YR", "5YR", "5YR", "10YR"), v = c(5, 5, 5, 6),
                 c = c(4, 6, 6, 4), clay = c(8, 15, 35, 20), sand = c(70, 60, 55, 60), oc = c(0.6, 0.2, 0.1, 0.05),
                 ph = c(6.2, 6.2, 6.0, 6.0), cec = c(3, 4, cec, 4), ca = c(1.5, 1, 1, 1),
                 st = c("massive", "subangular blocky", "subangular blocky", "massive"), gr = c("weak", "weak", "moderate", NA),
                 films = c(NA, NA, "common", NA))
    expect_identical(.rp50_codes(p), c("A", "FE", "BT", "C"), info = cec)
    expect_identical(.rp50_cl(p)$name, "ferruginosol semiluvique", info = cec)
  }
})

test_that("decision 3: the Indian red soils of the review, moderate angular FS imperfect", {
  skip_on_cran()
  IN <- list(country = "IN", lat = 13.0, lon = 77.6, koppen_climate = "Aw", parent_material = "granite gneiss")
  red <- function(ph, cec, ca, gr3, bs, mg) .rp50_D(c(0, 15, 40, 100), c(15, 40, 100, 150), hue = c("5YR", "5YR", "2.5YR", "2.5YR"),
                                     v = 4, c = c(4, 4, 6, 6), clay = c(12, 15, 35, 30), sand = c(68, 65, 45, 50),
                                     oc = c(0.5, 0.35, 0.25, 0.15), ph = ph, cec = cec, ca = ca,
                                     st = c("granular", "subangular blocky", "angular blocky", "subangular blocky"),
                                     gr = c("weak", "moderate", gr3, "weak"), bs_pct = bs, mg_cmol = mg, k_cmol = 0.2, na_cmol = 0.1, site = IN)
  # I1: saturated 65 %, CEC 25 per kg of clay, the FS moderate angular:
  # a fersialsol insature, imperfect
  i1 <- red(c(6.3, 6.3, 6.4, 6.5), c(5, 5, 8.75, 7.5), c(2.5, 2.5, 4.4, 3.8), "moderate", c(70, 65, 65, 65), 1)
  expect_identical(.rp50_codes(i1), c("A", "S", "FS", "BT"))
  r <- .rp50_cl(i1)
  expect_identical(r$name, "fersialsol insatur\u00e9")
  expect_identical(r$levels$attachment, "imperfect")
  # I2: S/CEC 42 % and bases 3.7 cmol+/kg, pH 6.2: the layer holds FE,
  # tested before FS in the fixed order (the FS a candidate)
  i2 <- red(c(6.2, 6.2, 6.2, 6.3), c(5, 5, 8.75, 7.5), c(2, 2, 2.6, 2.3), "moderate", c(50, 45, 45, 45), 0.8)
  expect_identical(.rp50_codes(i2)[3], "FE")
  expect_true("FS" %in% .rp50_cands(i2, 3))
  # I3: strong angular but CEC 13 per kg of clay, no mineralogy: kaolinitic,
  # no FS (reading 2), a BT
  i3 <- red(c(6.4, 6.4, 6.6, 6.6), c(5, 4, 4.5, 4), c(2.5, 2.2, 2.4, 2.2), "strong", 70, 0.5)
  expect_identical(.rp50_codes(i3)[3], "BT")
  expect_false("FS" %in% .rp50_codes(i3))
})

test_that("decision 4: under an FE the BT needs more clay than the FE", {
  skip_on_cran()
  # the reviewer's W1: a 7.5YR 5/4 layer of 16 % clay under an FE of 14 %:
  # no BT, an S, and the ferruginosol meuble
  w1 <- .rp50_D(c(0, 15, 90, 160), c(15, 90, 160, 220), hue = c("7.5YR", "5YR", "7.5YR", "10YR"), v = c(5, 5, 5, 6),
                c = c(4, 6, 4, 4), clay = c(8, 14, 16, 12), sand = c(72, 66, 62, 60), oc = c(0.6, 0.2, 0.1, 0.05),
                ph = c(6.2, 6.2, 6.0, 6.0), cec = c(3, 4, 3.5, 3), ca = c(1.2, 1, 1, 0.8),
                st = c("massive", "subangular blocky", "subangular blocky", "massive"), gr = c("weak", "weak", "moderate", NA))
  expect_identical(.rp50_codes(w1), c("A", "FE", "S", "C"))
  expect_identical(.rp50_cl(w1)$name, "ferruginosol meuble")
  # 22 % clay under the same FE (1.2 times and more): a BT by argic()
  w1b <- .rp50_D(c(0, 15, 90, 160), c(15, 90, 160, 220), hue = c("7.5YR", "5YR", "7.5YR", "10YR"), v = c(5, 5, 5, 6),
                 c = c(4, 6, 4, 4), clay = c(8, 14, 22, 12), sand = c(72, 66, 56, 60), oc = c(0.6, 0.2, 0.1, 0.05),
                 ph = c(6.2, 6.2, 6.0, 6.0), cec = c(3, 4, 4, 3), ca = c(1.2, 1, 1, 0.8),
                 st = c("massive", "subangular blocky", "subangular blocky", "massive"), gr = c("weak", "weak", "moderate", NA))
  expect_identical(.rp50_codes(w1b)[3], "BT")
  # the BTss reads the BT above it through the same rule
  expect_true(any(grepl(".rp_bt_fe(x, pv", deparse(.rp_t_BT), fixed = TRUE)))
})


# --- the readings ------------------------------------------------------------------------

test_that("the readings of this release are Hugo's decisions of 2026-11-04", {
  skip_on_cran()
  rd <- yaml::read_yaml(system.file("rules", "rp2008", "readings.yaml", package = "soilKey"))$readings
  tp <- vapply(rd, `[[`, "", "topic")
  txt <- vapply(rd, `[[`, "", "reading")
  expect_true(all(.rp50_readings %in% tp))
  expect_true(all(grepl("(Hugo, 2026-11-04).", txt[match(.rp50_readings, tp)], fixed = TRUE)))
  # 0.9.351 adds its readings, confirmed on 2026-11-04 (test-v09351)
  # none waits for Hugo's word (0.9.352 confirmed on 2026-11-04, test-v09352)
  expect_length(tp[grepl("to confirm", txt, fixed = TRUE)], 0L)
  expect_true(all(nzchar(vapply(rd[match(.rp50_readings, tp)], `[[`, "", "page"))))
})
