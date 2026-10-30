# v0.9.316: HSCS 2024 subtype properties, the chemical group (Micheli et al.
# 2024, 8.5, p. 29-32; carbonatic materials 8.4, p. 28): carbonate degrees,
# base status, gypsum, magnesium, salts, carbonate coatings, leached horizon.
# Made-up profiles only.

# an example with some columns replaced (whole columns, or row edits as a
# list of row = value)
.ch_mod <- function(type, h = list(), site = list()) {
  p <- make_hscs2024_example(type)
  hz <- as.data.frame(p$horizons, stringsAsFactors = FALSE)
  for (nm in names(h)) {
    v <- h[[nm]]
    if (is.list(v)) { if (!nm %in% names(hz)) hz[[nm]] <- NA; for (k in names(v)) hz[[nm]][as.integer(k)] <- v[[k]] }
    else hz[[nm]] <- if (length(v) == 1L) rep(v, nrow(hz)) else v
  }
  PedonRecord$new(site = utils::modifyList(p$site, site), horizons = ensure_horizon_schema(data.table::as.data.table(hz)))
}
.ch_pedon <- function(h) {
  n <- max(lengths(h))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  PedonRecord$new(site = list(id = "chem", country = "HU"),
                  horizons = ensure_horizon_schema(data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}
.ch_x <- function(p) soilKey:::.hscs_horizons(soilKey:::.cssc_prepare_pedon(p))
.ch_prop <- function(p, id) soilKey:::.hscs_eval_property(id, .ch_x(p))
.ch_status <- function(p, code) soilKey:::.hscs_type_properties(soilKey:::.cssc_prepare_pedon(p), code)$status
.ch_label <- function(p, code, id) { st <- .ch_status(p, code); st$label[st$id == id] }
# the full name holds `parts` in order (other groups' properties may come
# between them), the first of the subtypes with its capital letter
.ch_name <- function(p, type_name, parts, level = "subtypes") {
  r <- classify_hscs2024(p, on_missing = "silent")
  expect_identical(r$levels$name[1], type_name)
  got <- r$qualifiers[[level]]
  expect_identical(got[got %in% parts], parts, info = r$name)
  if (level == "subtypes") {
    first <- paste0(toupper(substr(parts[1], 1, 1)), substr(parts[1], 2, nchar(parts[1])))
    expect_true(startsWith(r$name, paste0(first, if (length(got) > 1) "," else " ")), info = r$name)
  } else expect_true(grepl(paste0("(", parts[1]), r$name, fixed = TRUE), info = r$name)
  invisible(r$name)
}

.ch_eros <- "er\u0151sen"; .ch_gyenge <- "gyeng\u00e9n"
.ch_surface <- "felsz\u00ednt\u0151l"; .ch_shallow <- "sek\u00e9lyen"; .ch_middle <- "k\u00f6z\u00e9pm\u00e9lyen"

test_that("the carbonatic materials of 8.4 per row: CaCO3 first, then the reaction", {
  skip_on_cran()
  p <- .ch_pedon(list(
    designation = c(rep("C", 12), "Ck", "Bk", "C", "C", "C", "C"), top_cm = seq(0, 170, 10),
    bottom_cm = seq(10, 180, 10),
    caco3_pct = c(1, 3, 3, 3, 20, 20, 1, 0, 8, NA, NA, NA, NA, NA, NA, 20, 1, NA),
    effervescence_class = c("slight", "slight", "moderate", "strong", "very strong", "very strong",
                            NA, NA, NA, "slight", "moderate", "strong", "very strong", NA, NA, NA,
                            "none", "very strong"),
    secondary_carbonates_pct = c(0, 0, 0, 0, 2, rep(0, 13)), carbonate_hard_pct = c(rep(NA, 15), 3, NA, NA)))
  x <- .ch_x(p)
  m <- function(d) soilKey:::.hscs_carbonate_material_rows(x, d)
  # a measured CaCO3 decides the band, whatever the reaction
  expect_identical(m("nyomokban")[c(1, 2, 17)], c(TRUE, FALSE, TRUE))
  expect_identical(m("gyengen")[1:4], c(FALSE, TRUE, TRUE, TRUE))       # a strong reaction with 3 %
  expect_identical(m("kozepesen")[4:6], c(FALSE, FALSE, TRUE))          # 20 % without secondary carbonates
  expect_identical(m("eros")[5:6], c(TRUE, FALSE))
  expect_identical(m("nyomokban")[7:8], c(TRUE, FALSE))                 # 0 % has no carbonate
  expect_identical(c(m("gyengen")[9], m("kozepesen")[9]), c(FALSE, TRUE))
  # no CaCO3: the reaction; slight leaves the very weak and the weak class open
  expect_identical(c(m("nyomokban")[10], m("gyengen")[10], m("kozepesen")[10]), c(NA, NA, FALSE))
  expect_identical(m("gyengen")[11], TRUE)
  expect_identical(c(m("kozepesen")[12], m("eros")[12]), c(TRUE, FALSE))
  expect_identical(c(m("kozepesen")[13], m("eros")[13]), c(FALSE, TRUE))  # the k shows secondary carbonates
  expect_identical(c(m("kozepesen")[18], m("eros")[18]), c(TRUE, FALSE))  # +++ without them
  # neither: a k shows 2 % or more, not the degree
  expect_identical(c(m("nyomokban")[14], m("gyengen")[14], m("kozepesen")[14], m("eros")[14]),
                   c(FALSE, NA, NA, NA))
  expect_identical(c(m("gyengen")[15], m("eros")[15]), c(NA, FALSE))
  # hard secondary carbonates count for the strong degree
  expect_identical(c(m("eros")[16], m("kozepesen")[16]), c(TRUE, FALSE))
})

test_that("the carbonate degrees: 15 cm within 100 cm, the strongest named, the depth modifier", {
  skip_on_cran()
  # brown earth: A 0-25, Bw 25-60, C 60-120
  p <- .ch_mod("brown_earth", list(caco3_pct = c(0, 3, 8)))
  g <- .ch_prop(p, "gyengen_karbonatos"); k <- .ch_prop(p, "kozepesen_karbonatos")
  expect_true(g$passed); expect_identical(g$top, 25)
  expect_true(k$passed); expect_identical(k$top, 60)
  expect_false(.ch_prop(p, "eros_karbonatos")$passed)
  expect_false(.ch_prop(p, "nyomokban_karbonatos")$passed)
  st <- .ch_status(p, "BAR")
  expect_identical(st$label[st$named & st$id == "kozepesen_karbonatos"], paste(.ch_middle, "k\u00f6zepesen karbon\u00e1tos"))
  expect_false(any(st$named & st$id == "gyengen_karbonatos"))
  # none calcareous
  p0 <- .ch_mod("brown_earth", list(caco3_pct = 0))
  for (id in c("eros_karbonatos", "kozepesen_karbonatos", "gyengen_karbonatos", "nyomokban_karbonatos"))
    expect_false(.ch_prop(p0, id)$passed, info = id)
  # a stronger degree undecided leaves the row unnamed
  pn <- .ch_mod("brown_earth", list(caco3_pct = c(0, 3, NA)))
  expect_true(.ch_prop(pn, "gyengen_karbonatos")$passed)
  expect_true(is.na(.ch_prop(pn, "kozepesen_karbonatos")$passed))
  expect_true("caco3_pct" %in% .ch_prop(pn, "kozepesen_karbonatos")$missing)
  stn <- .ch_status(pn, "BAR")
  expect_false(any(stn$named & grepl("karbonatos", stn$id)))
  # a layer thinner than 15 cm does not count
  pt <- .ch_pedon(list(designation = c("A", "Bw", "C"), top_cm = c(0, 25, 35), bottom_cm = c(25, 35, 120),
                       caco3_pct = c(0, 3, 0)))
  expect_false(.ch_prop(pt, "gyengen_karbonatos")$passed)
  # the trace degree from the surface
  pf <- .ch_mod("weakly_developed", list(caco3_pct = c(1, 1)))
  expect_identical(.ch_label(pf, "FOL", "nyomokban_karbonatos"), paste(.ch_surface, "nyomokban karbon\u00e1tos"))
})

test_that("erosen karbonatos needs secondary carbonates, recorded when met", {
  skip_on_cran()
  # steppe: Ck 90-130 with 15 % CaCO3 and secondary carbonates
  p <- make_hscs2024_example("steppe")
  e <- .ch_prop(p, "eros_karbonatos")
  expect_true(e$passed); expect_identical(e$top, 90)
  expect_identical(.ch_label(p, "MEZ", "eros_karbonatos"), paste(.ch_middle, "er\u0151sen karbon\u00e1tos"))
  # without them (and without the k), the strong degree fails
  p2 <- .ch_mod("steppe", list(designation = c("A", "AB", "B", "C"), secondary_carbonates_pct = 0))
  expect_false(.ch_prop(p2, "eros_karbonatos")$passed)
  # a description ending above 100 cm: the part below holds no secondary
  # carbonates, so the strong degree fails while the others stay open
  p3 <- .ch_pedon(list(designation = c("A", "C"), top_cm = c(0, 30), bottom_cm = c(30, 80), caco3_pct = c(3, 20)))
  expect_false(.ch_prop(p3, "eros_karbonatos")$passed)
  k3 <- .ch_prop(p3, "kozepesen_karbonatos")    # 20 % without secondary carbonates
  expect_true(k3$passed); expect_identical(k3$top, 30)
  expect_true(.ch_prop(p3, "gyengen_karbonatos")$passed)
  st3 <- .ch_status(p3, "BAR")
  expect_identical(st3$id[st3$named & grepl("karbonatos", st3$id)], "kozepesen_karbonatos")
})

test_that("the base status: telitett, telitetlen in the major part of 20-100 cm", {
  skip_on_cran()
  # clay-illuvial: A 0-20, E 20-35, Bt 35-80, C 80-120; base saturation 55-75
  p <- make_hscs2024_example("clay_illuvial")
  expect_true(.ch_prop(p, "telitetlen")$passed)
  expect_false(.ch_prop(p, "telitett")$passed)
  # the major part: 60 of the 80 cm above 80 %
  p2 <- .ch_mod("clay_illuvial", list(bs_pct = c(60, 90, 90, 60)))
  expect_true(.ch_prop(p2, "telitett")$passed)
  expect_false(.ch_prop(p2, "telitetlen")$passed)
  # exactly half is not the major part
  p3 <- .ch_pedon(list(designation = c("A", "B", "C"), top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 120),
                       bs_pct = c(90, 90, 70)))
  expect_false(.ch_prop(p3, "telitett")$passed)
  expect_false(.ch_prop(p3, "telitetlen")$passed)
  # undecided without base saturation; a calcareous row reads as saturated
  pn <- .ch_mod("clay_illuvial", list(bs_pct = NA_real_))
  expect_true(is.na(.ch_prop(pn, "telitett")$passed))
  expect_true("bs_pct" %in% .ch_prop(pn, "telitett")$missing)
  pc <- .ch_mod("clay_illuvial", list(bs_pct = NA_real_, caco3_pct = 5))
  expect_true(.ch_prop(pc, "telitett")$passed)
  expect_false(.ch_prop(pc, "telitetlen")$passed)
  # no modifier for a whole-soil property
  expect_true(is.na(.ch_prop(p, "telitetlen")$top))
})

test_that("the base status between 20 cm and rock or a cemented layer, and in shallow soils", {
  skip_on_cran()
  h <- list(designation = c("A", "Bw", "C"), top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 120), bs_pct = c(90, 60, NA))
  # with a C below 60 cm of unknown base saturation the window to 100 cm is open
  expect_true(is.na(.ch_prop(.ch_pedon(h), "telitetlen")$passed))
  # hard rock at 60 cm closes the window: 20-60 cm
  h$designation <- c("A", "Bw", "R")
  expect_true(.ch_prop(.ch_pedon(h), "telitetlen")$passed)
  # so does a cemented layer
  h$designation <- c("A", "Bw", "Ckm")
  expect_true(.ch_prop(.ch_pedon(h), "telitetlen")$passed)
  # a soil shallower than 20 cm: a layer 5 cm or more thick
  ps <- .ch_mod("rock_influenced", list(bs_pct = c(60, NA), caco3_pct = c(0, NA)))
  expect_true(.ch_prop(ps, "telitetlen")$passed)
  expect_false(.ch_prop(ps, "telitett")$passed)
  ps2 <- .ch_pedon(list(designation = c("A1", "A2", "R"), top_cm = c(0, 3, 18), bottom_cm = c(3, 18, 40),
                        bs_pct = c(90, 60, NA)))
  expect_false(.ch_prop(ps2, "telitett")$passed)   # 3 cm only
  expect_true(.ch_prop(ps2, "telitetlen")$passed)
  # a description that ends (every bottom recorded) above 100 cm is read to its end
  pe <- .ch_pedon(list(designation = c("A", "Bw"), top_cm = c(0, 25), bottom_cm = c(25, 60), bs_pct = 90))
  expect_true(.ch_prop(pe, "telitett")$passed)
})

test_that("savanyu: pH(H2O) 5.5 or less, in the window of the base status", {
  skip_on_cran()
  p <- .ch_mod("brown_earth", list(bs_pct = 40, ph_h2o = c(4.8, 5.2, 5.5)))
  expect_true(.ch_prop(p, "savanyu")$passed)
  expect_false(.ch_prop(p, "telitetlen")$passed)
  .ch_name(p, "Barnaf\u00f6ld", "savany\u00fa")
  # the surface horizon alone does not decide it
  p2 <- .ch_mod("brown_earth", list(bs_pct = 40, ph_h2o = c(4.8, 6, 6.2)))
  expect_false(.ch_prop(p2, "savanyu")$passed)
  p3 <- .ch_mod("brown_earth", list(bs_pct = 40, ph_h2o = NA_real_))
  expect_true(is.na(.ch_prop(p3, "savanyu")$passed))
  expect_identical(.ch_prop(p3, "savanyu")$missing, "ph_h2o")
  # a stronger base status takes the row
  p4 <- .ch_mod("brown_earth", list(bs_pct = 70, ph_h2o = 5))
  st <- .ch_status(p4, "BAR")
  expect_identical(st$id[st$named & st$id %in% c("telitett", "telitetlen", "savanyu")], "telitetlen")
})

test_that("gipszes and erosen gipszes", {
  skip_on_cran()
  # meadow: A 0-30, Bl 30-60, Cr 60-120
  p <- .ch_mod("meadow", list(caso4_pct = c(0, 8, 0)))
  g <- .ch_prop(p, "gipszes")
  expect_true(g$passed); expect_identical(g$top, 30); expect_null(g$variant)
  expect_identical(.ch_label(p, "RET", "gipszes"), paste(.ch_shallow, "gipszes"))
  p2 <- .ch_mod("meadow", list(caso4_pct = c(0, 20, 0)))
  expect_identical(.ch_prop(p2, "gipszes")$variant, "eros")
  expect_identical(.ch_label(p2, "RET", "gipszes"), paste(.ch_shallow, .ch_eros, "gipszes"))
  expect_false(.ch_prop(.ch_mod("meadow", list(caso4_pct = 0)), "gipszes")$passed)
  expect_true(is.na(.ch_prop(.ch_mod("meadow", list(caso4_pct = c(0, NA, 0))), "gipszes")$passed))
  # the strong form undecided leaves the property undecided, with a note
  pn <- .ch_mod("meadow", list(caso4_pct = c(8, NA, 0)))
  gn <- .ch_prop(pn, "gipszes")
  expect_true(is.na(gn$passed)); expect_match(gn$note, "holds")
  # visible gypsum at the threshold shows it when no measure is given
  pv <- .ch_mod("meadow", list(caso4_pct = c(0, NA, 0), gypsum_visible_pct = c(0, 20, 0)))
  expect_identical(.ch_prop(pv, "gipszes")$variant, "eros")
  pv2 <- .ch_mod("meadow", list(caso4_pct = c(0, NA, 0), gypsum_visible_pct = c(0, 6, 0)))
  expect_true(is.na(.ch_prop(pv2, "gipszes")$passed))   # 15 % or more not excluded
})

test_that("magneziumos: exchangeable Ca / Mg below 1 in 30 cm within 100 cm", {
  skip_on_cran()
  p <- .ch_mod("meadow", list(ca_cmol = c(10, 3, 3), mg_cmol = c(3, 5, 5)))
  m <- .ch_prop(p, "magneziumos")
  expect_true(m$passed); expect_identical(m$top, 30)
  expect_identical(.ch_label(p, "RET", "magneziumos"), paste(.ch_shallow, "magn\u00e9ziumos"))
  expect_false(.ch_prop(.ch_mod("meadow", list(ca_cmol = 10, mg_cmol = 3)), "magneziumos")$passed)
  # 20 cm is too thin
  pt <- .ch_pedon(list(designation = c("A", "B", "C"), top_cm = c(0, 30, 50), bottom_cm = c(30, 50, 120),
                       ca_cmol = c(10, 3, 10), mg_cmol = c(3, 5, 3)))
  expect_false(.ch_prop(pt, "magneziumos")$passed)
  pn <- .ch_mod("meadow", list(ca_cmol = c(10, NA, 10), mg_cmol = c(3, 5, 5)))
  expect_true(is.na(.ch_prop(pn, "magneziumos")$passed))
  expect_true("ca_cmol" %in% .ch_prop(pn, "magneziumos")$missing)
})

test_that("szoloncsakos and gyengen szoloncsakos: the plain property wins", {
  skip_on_cran()
  p <- .ch_mod("meadow", list(soluble_salts_pct = c(0.02, 0.2, 0.02)))
  s <- .ch_prop(p, "szoloncsakos")
  expect_true(s$passed); expect_null(s$variant); expect_identical(s$top, 30)
  expect_identical(.ch_label(p, "RET", "szoloncsakos"), paste(.ch_shallow, "szoloncs\u00e1kos"))
  pw <- .ch_mod("meadow", list(soluble_salts_pct = c(0.02, 0.08, 0.02)))
  expect_identical(.ch_prop(pw, "szoloncsakos")$variant, "gyenge")
  expect_identical(.ch_label(pw, "RET", "szoloncsakos"), paste(.ch_shallow, .ch_gyenge, "szoloncs\u00e1kos"))
  expect_false(.ch_prop(make_hscs2024_example("meadow"), "szoloncsakos")$passed)
  # undecided plain property: the weak form is not named
  pn <- .ch_mod("meadow", list(soluble_salts_pct = c(0.08, NA, 0.02)))
  expect_true(is.na(.ch_prop(pn, "szoloncsakos")$passed))
  expect_identical(.ch_prop(pn, "szoloncsakos")$missing, "soluble_salts_pct")
  # erosen is only for Szoloncsak soils, whose table does not list it
  pe <- .ch_mod("meadow", list(soluble_salts_pct = c(0.02, 0.8, 0.02), ph_h2o = 9))
  expect_null(.ch_prop(pe, "szoloncsakos")$variant)
})

test_that("szolonyeces and its weak and strong forms", {
  skip_on_cran()
  # meadow, CEC 20: Bl with Na 10 % and Na + Mg 20 %
  p <- .ch_mod("meadow", list(na_cmol = c(0.2, 2, 0.2), mg_cmol = c(3, 2, 3)))
  s <- .ch_prop(p, "szolonyeces")
  expect_true(s$passed); expect_null(s$variant); expect_identical(s$top, 30)
  expect_identical(.ch_label(p, "RET", "szolonyeces"), paste(.ch_shallow, "szolonyeces"))
  pe <- .ch_mod("meadow", list(na_cmol = c(0.2, 2, 0.2), mg_cmol = c(3, 5, 3)))
  expect_identical(.ch_prop(pe, "szolonyeces")$variant, "eros")
  expect_identical(.ch_label(pe, "RET", "szolonyeces"), paste(.ch_shallow, .ch_eros, "szolonyeces"))
  pw <- .ch_mod("meadow", list(na_cmol = c(0.2, 2, 0.2), mg_cmol = c(3, 0.5, 3)))
  expect_identical(.ch_prop(pw, "szolonyeces")$variant, "gyenge")
  expect_identical(.ch_label(pw, "RET", "szolonyeces"), paste(.ch_shallow, .ch_gyenge, "szolonyeces"))
  expect_false(.ch_prop(make_hscs2024_example("meadow"), "szolonyeces")$passed)
  # ESP alone: the weak form holds, the stronger ones are undecided
  pn <- .ch_mod("meadow", list(na_cmol = NA_real_, esp_pct = c(1, 10, 1)))
  sn <- .ch_prop(pn, "szolonyeces")
  expect_true(is.na(sn$passed)); expect_true("mg_cmol" %in% sn$missing); expect_match(sn$note, "holds")
})

test_that("karbonat-szulfatos and karbonat-kloridos: shares of the salts beside 20 % carbonate", {
  skip_on_cran()
  # solonchak: Az 0-20, Cz 20-60, Czl 60-100
  p <- .ch_mod("solonchak", list(soluble_salts_sulfate_pct = c(60, NA, NA), soluble_salts_chloride_pct = c(20, NA, NA),
                                 soluble_salts_carbonate_pct = c(20, 10, 10)))
  expect_true(.ch_prop(p, "karbonat_szulfatos")$passed)
  expect_false(.ch_prop(p, "karbonat_kloridos")$passed)   # 20 % chloride; below 20 % carbonate in the rest
  p2 <- .ch_mod("solonchak", list(soluble_salts_chloride_pct = 35, soluble_salts_carbonate_pct = c(10, 25, 10)))
  expect_true(.ch_prop(p2, "karbonat_kloridos")$passed)
  # less than 20 % carbonate in the salts; the CaCO3 of the soil does not count
  p3 <- .ch_mod("solonchak", list(caco3_pct = 30, soluble_salts_sulfate_pct = 60, soluble_salts_carbonate_pct = 15))
  expect_false(.ch_prop(p3, "karbonat_szulfatos")$passed)
  p4 <- .ch_mod("solonchak", list(soluble_salts_sulfate_pct = 40, soluble_salts_carbonate_pct = 30))
  expect_false(.ch_prop(p4, "karbonat_szulfatos")$passed)
  pn <- .ch_mod("solonchak", list(caco3_pct = 30, soluble_salts_sulfate_pct = 60))
  expect_true(is.na(.ch_prop(pn, "karbonat_szulfatos")$passed))
  expect_true("soluble_salts_carbonate_pct" %in% .ch_prop(pn, "karbonat_szulfatos")$missing)
  pn2 <- .ch_mod("solonchak", list(soluble_salts_carbonate_pct = 25))
  expect_true("soluble_salts_sulfate_pct" %in% .ch_prop(pn2, "karbonat_szulfatos")$missing)
  expect_true(is.na(.ch_prop(p, "karbonat_szulfatos")$top))
})

test_that("meszlepedekes: soft secondary carbonates on ped faces", {
  skip_on_cran()
  # steppe: A 0-40, AB 40-60, Bk 60-90, Ck 90-130
  p <- .ch_mod("steppe", list(carbonate_soft_pct = c(0, 0, 5, 0)))
  m <- .ch_prop(p, "meszlepedekes")
  expect_true(m$passed); expect_identical(m$top, 60)
  expect_identical(.ch_label(p, "MEZ", "meszlepedekes"), paste(.ch_middle, "m\u00e9szleped\u00e9kes"))
  # secondary carbonates of unrecorded form: undecided
  pn <- make_hscs2024_example("steppe")
  expect_true(is.na(.ch_prop(pn, "meszlepedekes")$passed))
  expect_identical(.ch_prop(pn, "meszlepedekes")$missing, c("secondary_carbonates_form", "carbonate_soft_pct"))
  # none recorded: absent
  p0 <- .ch_mod("steppe", list(designation = c("A", "AB", "B", "C"), secondary_carbonates_pct = 0))
  expect_false(.ch_prop(p0, "meszlepedekes")$passed)
  # no ped faces in a massive horizon
  pm <- .ch_mod("steppe", list(carbonate_soft_pct = c(0, 0, 0, 5)))
  expect_false(.ch_prop(pm, "meszlepedekes")$passed)
  # the form of the secondary carbonates decides where it is recorded
  pf <- .ch_mod("steppe", list(secondary_carbonates_form = c(NA, NA, "pseudomycelia", "masses")))
  mf <- .ch_prop(pf, "meszlepedekes")
  expect_true(mf$passed); expect_identical(mf$top, 60)
  pc <- .ch_mod("steppe", list(secondary_carbonates_form = c(NA, NA, "hard concretions", "coatings")))
  expect_identical(.ch_prop(pc, "meszlepedekes")$top, 90)
  ph <- .ch_mod("steppe", list(secondary_carbonates_form = c(NA, NA, "hard concretions", "soft concretions"),
                               carbonate_soft_pct = c(0, 0, 5, 5)))
  expect_false(.ch_prop(ph, "meszlepedekes")$passed)     # another form recorded: no
})

test_that("szologyos: a leached horizon within 50 cm, pale when dry, pH 5 or less", {
  skip_on_cran()
  # solonetz: A 0-12, E 12-20, Btn 20-45, Bkn 45-80, Ck 80-120
  dry <- list(munsell_value_dry = c(4, 7, 4, 5, 6), munsell_chroma_dry = c(2, 2, 3, 3, 3))
  p <- .ch_mod("solonetz", c(dry, list(ph_h2o = c(6, 4.8, 7, 8, 8.5))))
  s <- .ch_prop(p, "szologyos")
  expect_true(s$passed); expect_identical(s$top, 12)
  expect_identical(.ch_label(p, "SZN", "szologyos"), paste(.ch_shallow, "szologyos"))
  expect_false(.ch_prop(.ch_mod("solonetz", c(dry, list(ph_h2o = c(6, 5.5, 7, 8, 8.5)))), "szologyos")$passed)
  # a dark dry colour fails
  expect_false(.ch_prop(.ch_mod("solonetz", list(munsell_value_dry = 4, munsell_chroma_dry = 2,
                                                 ph_h2o = c(6, 4.8, 7, 8, 8.5))), "szologyos")$passed)
  pn <- .ch_mod("solonetz", list(ph_h2o = c(6, 4.8, 7, 8, 8.5)))
  expect_true(is.na(.ch_prop(pn, "szologyos")$passed))
  expect_true("munsell_value_dry" %in% .ch_prop(pn, "szologyos")$missing)
  # thinner than 5 cm
  pt <- .ch_pedon(list(designation = c("A", "E", "Btn"), top_cm = c(0, 10, 13), bottom_cm = c(10, 13, 60),
                       munsell_value_dry = c(4, 7, 4), munsell_chroma_dry = 2, ph_h2o = c(6, 4.8, 7)))
  expect_false(.ch_prop(pt, "szologyos")$passed)
})

test_that("full names of the types with the chemical properties", {
  skip_on_cran()
  kz <- "k\u00f6zepesen karbon\u00e1tos"; gy <- "gyeng\u00e9n karbon\u00e1tos"; ny <- "nyomokban karbon\u00e1tos"
  er <- "er\u0151sen karbon\u00e1tos"
  tt <- "tel\u00edtett"; tn <- "tel\u00edtetlen"
  # LAP: the properties are varieties
  .ch_name(.ch_mod("peat", list(caco3_pct = c(0, 0, 8), bs_pct = c(90, 90, 95))), "L\u00e1ptalaj",
           c(paste(.ch_middle, kz), tt), level = "varieties")
  .ch_name(.ch_mod("anthropogenic", list(caco3_pct = c(3, 3, 0), bs_pct = 90)), "Antropog\u00e9n talaj",
           c(paste(.ch_surface, gy), tt))
  .ch_name(.ch_mod("rocky_skeletal", list(bs_pct = c(90, NA))), "K\u00f6ves-szikl\u00e1s v\u00e1ztalaj", tt)
  .ch_name(make_hscs2024_example("rock_influenced"), "K\u0151zethat\u00e1s\u00fa talaj", c(paste(.ch_surface, kz), tt))
  .ch_name(.ch_mod("solonetz", list(munsell_value_dry = c(4, 7, 4, 5, 6), munsell_chroma_dry = c(2, 2, 3, 3, 3),
                                    ph_h2o = c(6, 4.8, 7, 8, 8.5), soluble_salts_pct = c(0.02, 0.02, 0.2, 0.3, 0.3),
                                    ca_cmol = c(10, 8, 5, 5, 5), mg_cmol = c(3, 3, 8, 9, 8))),
           "Szolonyec talaj",
           c(paste(.ch_shallow, kz), tt, paste(.ch_shallow, "magn\u00e9ziumos"), paste(.ch_shallow, "szologyos"),
             paste(.ch_shallow, "szoloncs\u00e1kos")))
  .ch_name(.ch_mod("solonchak", list(soluble_salts_sulfate_pct = c(60, NA, NA), soluble_salts_carbonate_pct = 20,
                                     na_cmol = c(3, 3, 0.2))),
           "Szoloncs\u00e1k talaj",
           c(paste(.ch_surface, kz), tt, "karbon\u00e1t-szulf\u00e1tos", paste(.ch_surface, .ch_eros, "szolonyeces")))
  .ch_name(.ch_mod("swelling_clay", list(caco3_pct = c(0, 0, 8), bs_pct = 90, caso4_pct = c(0, 6, 0))),
           "Duzzad\u00f3 agyagtalaj", c(paste(.ch_middle, kz), tt, paste(.ch_shallow, "gipszes")))
  .ch_name(.ch_mod("meadow", list(caco3_pct = c(0, 0, 3), bs_pct = 70, soluble_salts_pct = c(0.02, 0.08, 0.02))),
           "R\u00e9ti talaj", c(paste(.ch_middle, gy), tn, paste(.ch_shallow, .ch_gyenge, "szoloncs\u00e1kos")))
  .ch_name(.ch_mod("steppe", list(carbonate_soft_pct = c(0, 0, 5, 0), bs_pct = c(70, 70, 70, NA))),
           "Mez\u0151s\u00e9gi talaj", c(paste(.ch_middle, er), tn, paste(.ch_middle, "m\u00e9szleped\u00e9kes")))
  .ch_name(.ch_mod("clay_illuvial", list(caco3_pct = c(0, 0, 0, 3))), "Agyagbemos\u00f3d\u00e1sos talaj",
           c(paste(.ch_middle, gy), tn))
  .ch_name(.ch_mod("sandy", list(caco3_pct = c(0, 1, 1), bs_pct = 90)), "Homoktalaj", c(paste(.ch_shallow, ny), tt))
  .ch_name(.ch_mod("brown_earth", list(bs_pct = 40, ph_h2o = 5)), "Barnaf\u00f6ld", "savany\u00fa")
  .ch_name(.ch_mod("deposit", list(caco3_pct = 6, na_cmol = c(0.2, 0.2, 2))), "Hordal\u00e9ktalaj",
           c(paste(.ch_surface, kz), tt, paste(.ch_shallow, "szolonyeces")))
  .ch_name(.ch_mod("weakly_developed", list(caco3_pct = 1, bs_pct = 90)), "F\u00f6ldeskop\u00e1r", c(paste(.ch_surface, ny), tt))
})
