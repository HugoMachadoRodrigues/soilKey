# v0.9.316: HSCS 2024 (Micheli et al. 2024) subtype and variety properties of
# human influence, tillage, layering, burial, coarse fragments and texture
# (8.5, 8.6). Made-up profiles only, built from make_hscs2024_example().

.hu_mine <- c("mutermekes", "fedett", "vizzaro", "hortikus", "terrikus", "bolygatott", "szennyezett",
              "muvelt", "rigolozott", "teraszolt", "arasztott", "eketalpas", "elporosodott", "hantos",
              "tomodott", "digozott", "tobbretegu", "lejtohordalekos", "ontes", "eltemetett", "kavicsos",
              "kozettormelekes", "textura", "textura_feku")

# The properties of the other groups are stubbed as undecided, so that the
# full names below show the properties of this file alone.
.hu_only_mine <- function(env = parent.frame()) {
  ns <- asNamespace("soilKey")
  ids <- setdiff(names(soilKey:::.hscs_properties_rules()$properties), .hu_mine)
  fns <- paste0(".hscs_p_", ids)
  fns <- fns[vapply(fns, exists, NA, envir = ns, inherits = FALSE)]
  if (!length(fns)) return(invisible(NULL))
  stub <- function(x) soilKey:::.hscs_prop(NA, note = "stubbed")
  do.call(testthat::local_mocked_bindings,
          c(stats::setNames(rep(list(stub), length(fns)), fns), list(.package = "soilKey", .env = env)))
}

# An example with columns replaced (whole columns, or row edits as a list of
# row = value) and site fields added.
.hu_mod <- function(type, h = list(), site = list()) {
  p <- make_hscs2024_example(type)
  hz <- as.data.frame(p$horizons, stringsAsFactors = FALSE)
  for (nm in names(h)) {
    v <- h[[nm]]
    if (is.list(v)) { if (!nm %in% names(hz)) hz[[nm]] <- NA; for (k in names(v)) hz[[nm]][as.integer(k)] <- v[[k]] }
    else hz[[nm]] <- if (length(v) == 1L) rep(v, nrow(hz)) else v
  }
  PedonRecord$new(site = utils::modifyList(p$site, site), horizons = ensure_horizon_schema(data.table::as.data.table(hz)))
}

# A profile from columns, with the base values of the examples.
.hu_mk <- function(h, site = list()) {
  base <- list(soluble_salts_pct = 0.02, na_cmol = 0.2, mg_cmol = 3, cec_cmol = 20,
               redoximorphic_features_pct = 0, coarse_fragments_pct = 0, caco3_pct = 0)
  h <- utils::modifyList(base, h)
  n <- max(lengths(h))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  PedonRecord$new(site = utils::modifyList(list(id = "hu-test", country = "HU", designation_system = "fao2006"), site),
                  horizons = ensure_horizon_schema(data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}

.hu_p <- function(id, p) {
  f <- get(paste0(".hscs_p_", id), envir = asNamespace("soilKey"))
  f(soilKey:::.hscs_horizons(p))
}
.hu_name <- function(p) classify_hscs2024(p, on_missing = "silent")$name

# a buried brown earth under 60 cm of new material
.hu_buried <- function(top = 60, des = c("A", "C", "2Ab", "2Bwb", "2C")) {
  .hu_mk(list(designation = des, top_cm = c(0, 20, top, top + 20, top + 50),
              bottom_cm = c(20, top, top + 20, top + 50, top + 90),
              clay_pct = c(18, 8, 20, 22, 20), silt_pct = c(62, 30, 42, 40, 42), sand_pct = c(20, 62, 38, 38, 38),
              oc_pct = c(1.2, 0.3, 1.5, 0.5, 0.2),
              structure_grade = c("weak", "structureless", "moderate", "moderate", "structureless"),
              structure_type = c("granular", "massive", "granular", "subangular blocky", "massive"),
              munsell_hue_moist = c("10YR", "10YR", "10YR", "7.5YR", "10YR"),
              munsell_value_moist = c(4, 5, 3, 4, 5), munsell_chroma_moist = c(2, 3, 3, 4, 4)))
}

test_that("every property of this group is read, and none fails on the examples", {
  skip_on_cran()
  ns <- asNamespace("soilKey")
  expect_true(all(vapply(paste0(".hscs_p_", .hu_mine), exists, NA, envir = ns, inherits = FALSE)))
  for (t in c("peat", "anthropogenic", "rocky_skeletal", "rock_influenced", "solonetz", "solonchak",
              "swelling_clay", "meadow", "carbonate", "steppe", "clay_illuvial", "sandy", "brown_earth",
              "deposit", "weakly_developed")) {
    st <- classify_hscs2024(make_hscs2024_example(t), on_missing = "silent")$qualifiers$status
    st <- st[st$id %in% .hu_mine, ]
    expect_false(any(grepl("^error|not read", st$note)), info = t)
  }
})

test_that("textura names the dominant class within 50 cm, in Hungarian", {
  skip_on_cran()
  .hu_only_mine()
  expect_identical(.hu_name(make_hscs2024_example("brown_earth")), "Barnaf\u00f6ld (v\u00e1lyog)")
  p <- .hu_p("textura", make_hscs2024_example("brown_earth"))
  expect_true(p$passed); expect_identical(p$label, "v\u00e1lyog")
  # 25 cm of silt loam against 25 cm of sandy loam: the class met first
  expect_identical(.hu_p("textura", make_hscs2024_example("deposit"))$label, "iszapos v\u00e1lyog")
  # the thicker class wins
  expect_identical(.hu_p("textura", make_hscs2024_example("solonetz"))$label, "agyag")
  # each class of the triangle (p. 36) keeps its Hungarian name
  hu <- c("homok", "v\u00e1lyogos homok", "homokos v\u00e1lyog", "v\u00e1lyog", "iszapos v\u00e1lyog", "iszap",
          "homokos agyagos v\u00e1lyog", "agyagos v\u00e1lyog", "iszapos agyagos v\u00e1lyog", "homokos agyag",
          "iszapos agyag", "agyag")
  for (nm in hu) {
    q <- .hu_mod("brown_earth", list(field_texture = nm))
    expect_identical(.hu_p("textura", q)$label, nm, info = nm)
  }
  # half the window without texture: undecided
  q <- .hu_mod("brown_earth", list(clay_pct = list(`2` = NA), silt_pct = list(`2` = NA), sand_pct = list(`2` = NA)))
  expect_true(is.na(.hu_p("textura", q)$passed))
  # rock at the surface: no soil to take a texture from
  q <- .hu_mk(list(designation = c("R"), top_cm = 0, bottom_cm = 30))
  expect_false(.hu_p("textura", q)$passed)
})

test_that("textura_feku names the mineral substratum of the peat", {
  skip_on_cran()
  .hu_only_mine()
  p <- .hu_mod("peat", list(clay_pct = list(`3` = 50), silt_pct = list(`3` = 30), sand_pct = list(`3` = 20)))
  r <- .hu_p("textura_feku", p)
  expect_true(r$passed); expect_identical(r$label, "agyag fek\u00fc")
  expect_identical(.hu_name(p), "L\u00e1ptalaj (agyag fek\u00fc)")
  # the substratum is described with no texture: undecided
  expect_true(is.na(.hu_p("textura_feku", make_hscs2024_example("peat"))$passed))
  # the description ends in peat
  q <- .hu_mod("peat", list(designation = c("H1", "H2", "H3"), oc_pct = c(35, 30, 30)))
  expect_true(is.na(.hu_p("textura_feku", q)$passed))
  # peat on rock
  q <- .hu_mod("peat", list(designation = c("H1", "H2", "R")))
  expect_false(.hu_p("textura_feku", q)$passed)
})

test_that("kavicsos and kozettormelekes read the shape and the share of coarse fragments", {
  skip_on_cran()
  .hu_only_mine()
  p <- .hu_mod("brown_earth", list(coarse_fragments_pct = c(10, 50, 50), coarse_fragments_shape = "angular"))
  r <- .hu_p("kozettormelekes", p)
  expect_true(r$passed); expect_equal(r$top, 25); expect_null(r$variant)
  expect_false(.hu_p("kavicsos", p)$passed)
  # the naming example of p. 7
  expect_identical(.hu_name(p), "Barnaf\u00f6ld (sek\u00e9lyen k\u0151zett\u00f6rmel\u00e9kes, v\u00e1lyog)")
  p <- .hu_mod("brown_earth", list(coarse_fragments_pct = c(10, 50, 50), coarse_fragments_shape = "rounded"))
  expect_true(.hu_p("kavicsos", p)$passed); expect_false(.hu_p("kozettormelekes", p)$passed)
  # erosen: 80 % or more above rock at 20 cm
  p <- .hu_mod("rock_influenced", list(coarse_fragments_pct = c(85, NA), coarse_fragments_shape = c("rounded", NA)))
  r <- .hu_p("kavicsos", p)
  expect_true(r$passed); expect_identical(r$variant, "eros"); expect_equal(r$top, 0)
  expect_identical(.hu_name(p), "K\u0151zethat\u00e1s\u00fa talaj (felsz\u00ednt\u0151l er\u0151sen kavicsos, v\u00e1lyog)")
  # 60 % rounded: kavicsos, erosen not
  p <- .hu_mod("rock_influenced", list(coarse_fragments_pct = c(60, NA), coarse_fragments_shape = c("rounded", NA)))
  expect_null(.hu_p("kavicsos", p)$variant)
  # a mixed shape, or none recorded: undecided
  p <- .hu_mod("brown_earth", list(coarse_fragments_pct = 60, coarse_fragments_shape = "mixed"))
  expect_true(is.na(.hu_p("kavicsos", p)$passed)); expect_true(is.na(.hu_p("kozettormelekes", p)$passed))
  p <- .hu_mod("brown_earth", list(coarse_fragments_pct = 60))
  expect_true(is.na(.hu_p("kavicsos", p)$passed))
  # not recorded reads as none
  p <- .hu_mod("brown_earth", list(coarse_fragments_pct = NA))
  expect_false(.hu_p("kavicsos", p)$passed)
})

test_that("tobbretegu reads a lithologic discontinuity, a buried soil or another origin", {
  skip_on_cran()
  .hu_only_mine()
  r <- .hu_p("tobbretegu", make_hscs2024_example("anthropogenic"))
  expect_true(r$passed); expect_equal(r$top, 30)
  expect_false(.hu_p("tobbretegu", make_hscs2024_example("brown_earth"))$passed)
  # a buried C at 60 cm: layered, and a buried soil (of a C alone)
  p <- .hu_mod("brown_earth", list(designation = c("A", "Bw", "Cb")))
  r <- .hu_p("tobbretegu", p)
  expect_true(r$passed); expect_equal(r$top, 60)
  expect_identical(.hu_p("eltemetett", p)$label, "eltemetett F\u00f6ldeskop\u00e1r")
  # another origin recorded at 60 cm
  p <- .hu_mod("brown_earth", list(layer_origin = c("loess", "loess", "aeolian sand")))
  expect_equal(.hu_p("tobbretegu", p)$top, 60)
  expect_identical(.hu_name(p), "Barnaf\u00f6ld (k\u00f6z\u00e9pm\u00e9lyen t\u00f6bbr\u00e9teg\u0171, v\u00e1lyog)")
  # a row with no designation, or a description that ends above 100 cm
  expect_true(is.na(.hu_p("tobbretegu", .hu_mod("brown_earth", list(designation = c("A", NA, "C"))))$passed))
  expect_true(is.na(.hu_p("tobbretegu", .hu_mod("brown_earth", list(bottom_cm = c(25, 60, 90))))$passed))
})

test_that("ontes and lejtohordalekos read stratified material of their origin, 25 cm within 100 cm", {
  skip_on_cran()
  .hu_only_mine()
  p <- .hu_mod("deposit", list(rock_origin = "fluviatile"))
  r <- .hu_p("ontes", p)
  expect_true(r$passed); expect_equal(r$top, 0)
  expect_false(.hu_p("lejtohordalekos", p)$passed)
  expect_identical(.hu_name(p), "Felsz\u00ednt\u0151l \u00f6nt\u00e9s Hordal\u00e9ktalaj (iszapos v\u00e1lyog)")
  # colluvium over alluvium: the layer's own origin before the rock's
  p <- .hu_mod("deposit", list(layer_origin = c("colluvial", "colluvial", "fluvial"), rock_origin = "fluviatile"))
  expect_equal(.hu_p("lejtohordalekos", p)$top, 0)
  expect_equal(.hu_p("ontes", p)$top, 45)
  expect_identical(.hu_name(p), paste("Sek\u00e9lyen \u00f6nt\u00e9s, felsz\u00ednt\u0151l lejt\u0151hordal\u00e9kos",
                                      "Hordal\u00e9ktalaj (iszapos v\u00e1lyog)"))
  # a colluvial layer thinner than 25 cm, and a marine origin
  p <- .hu_mod("deposit", list(layer_origin = c("colluvial", "aeolian", "aeolian"), rock_origin = "marine"))
  expect_false(.hu_p("lejtohordalekos", p)$passed); expect_false(.hu_p("ontes", p)$passed)
  # no origin recorded: undecided
  expect_true(is.na(.hu_p("ontes", make_hscs2024_example("deposit"))$passed))
  # no stratified material within 100 cm: no
  expect_false(.hu_p("ontes", .hu_mod("sandy", list(rock_origin = "fluviatile")))$passed)
})

test_that("eltemetett keys the buried soil and names its type", {
  skip_on_cran()
  .hu_only_mine()
  p <- .hu_buried()
  r <- .hu_p("eltemetett", p)
  expect_true(r$passed); expect_identical(r$label, "eltemetett Barnaf\u00f6ld")
  expect_identical(.hu_name(p), "Hordal\u00e9ktalaj (homokos v\u00e1lyog, eltemetett Barnaf\u00f6ld)")
  expect_false(isTRUE(soilKey:::.hscs_state$buried))
  # buried under less than 50 cm: not eltemetett, but layered
  p <- .hu_buried(top = 40)
  expect_false(.hu_p("eltemetett", p)$passed)
  expect_equal(.hu_p("tobbretegu", p)$top, 40)
  # no buried horizon recorded: no; no designations below 50 cm: undecided
  expect_false(.hu_p("eltemetett", make_hscs2024_example("brown_earth"))$passed)
  expect_true(is.na(.hu_p("eltemetett", .hu_buried(des = c("A", "C", NA, NA, NA)))$passed))
  # the designations of the buried part, as at the surface
  expect_identical(soilKey:::.hscs_unbury_designation(c("2Ab", "2Bwb1", "2C", "3Ck", "Btb", "C(b)", NA), "2"),
                   c("A", "Bw1", "C", "2Ck", "Bt", "C", NA))
  # a buried soil is not asked for its own buried soil
  st <- soilKey:::.hscs_state
  st$buried <- TRUE
  r <- .hu_p("eltemetett", .hu_buried())
  st$buried <- FALSE
  expect_true(is.na(r$passed))
})

test_that("the anthropogenic subtypes", {
  skip_on_cran()
  .hu_only_mine()
  p <- make_hscs2024_example("anthropogenic")
  expect_identical(.hu_name(p), "M\u0171term\u00e9kes, terrikus Antropog\u00e9n talaj (sek\u00e9lyen t\u00f6bbr\u00e9teg\u0171, v\u00e1lyog)")
  # mutermekes: 9 % artefacts (the soil is anthropogenic by its fill)
  q <- .hu_mod("anthropogenic", list(artefacts_pct = c(10, 10, 0)))
  expect_false(.hu_p("mutermekes", q)$passed)
  expect_identical(classify_hscs2024(q, on_missing = "silent")$levels$code[1], "ANT")
  # terrikus: the raised surface and the original one alike, or another
  # origin recorded at the surface, or no origin at all
  q <- .hu_mod("anthropogenic", list(designation = c("Au", "Cu", "C"), munsell_value_moist = 4, munsell_chroma_moist = 3))
  expect_false(.hu_p("terrikus", q)$passed)
  q <- .hu_mod("anthropogenic", list(layer_origin = c("in situ", "fill", NA)))
  expect_false(.hu_p("terrikus", q)$passed)
  q <- .hu_mod("anthropogenic", list(layer_origin = NA))
  expect_true(is.na(.hu_p("terrikus", q)$passed))
  # fedett and vizzaro
  q <- .hu_mod("anthropogenic", site = list(surface_condition = "asphalt"))
  expect_true(.hu_p("fedett", q)$passed)
  expect_equal(.hu_p("vizzaro", q)$top, 0)
  q <- .hu_mod("anthropogenic", list(geomembrane_present = c(FALSE, TRUE, FALSE)))
  expect_false(.hu_p("fedett", q)$passed)
  expect_equal(.hu_p("vizzaro", q)$top, 30)
  expect_match(.hu_name(q), "terrikus Antropog\u00e9n talaj", fixed = TRUE)
  expect_match(.hu_name(q), "M\u0171term\u00e9kes, sek\u00e9lyen v\u00edzz\u00e1r\u00f3, terrikus", fixed = TRUE)
  q <- .hu_mod("anthropogenic", list(geomembrane_present = c(TRUE, FALSE, FALSE)))
  expect_true(is.na(.hu_p("fedett", q)$passed))
  # hortikus: AL-P2O5 of the upper 20 cm
  expect_true(.hu_p("hortikus", .hu_mod("anthropogenic", list(p2o5_al_mg_kg = c(650, 200, 50))))$passed)
  expect_false(.hu_p("hortikus", .hu_mod("anthropogenic", list(p2o5_al_mg_kg = c(300, 200, 50))))$passed)
  expect_true(is.na(.hu_p("hortikus", p)$passed))
  # bolygatott, from the site
  q <- .hu_mod("anthropogenic", site = list(mechanical_mixing_depth_cm = 60))
  expect_true(.hu_p("bolygatott", q)$passed)
  expect_true(.hu_p("rigolozott", q)$passed); expect_true(.hu_p("muvelt", q)$passed)
  expect_identical(.hu_name(q), paste("M\u0171term\u00e9kes, terrikus, bolygatott Antropog\u00e9n talaj",
                                      "(rigolozott, sek\u00e9lyen t\u00f6bbr\u00e9teg\u0171, v\u00e1lyog)"))
  expect_false(.hu_p("bolygatott", .hu_mod("anthropogenic", site = list(mechanical_mixing_depth_cm = 0)))$passed)
  expect_true(is.na(.hu_p("bolygatott", p)$passed))
  # szennyezett, from the site (no depth recorded, no modifier)
  q <- .hu_mod("anthropogenic", site = list(contaminated = TRUE))
  expect_true(.hu_p("szennyezett", q)$passed)
  expect_match(.hu_name(q), "^Szennyezett, m\u0171term\u00e9kes")
  expect_false(.hu_p("szennyezett", .hu_mod("anthropogenic", site = list(contaminated = "no")))$passed)
  expect_true(is.na(.hu_p("szennyezett", p)$passed))
})

test_that("tillage: muvelt, rigolozott, teraszolt, arasztott", {
  skip_on_cran()
  .hu_only_mine()
  p <- .hu_mod("brown_earth", list(designation = c("Ap", "Bw", "C")))
  expect_true(.hu_p("muvelt", p)$passed); expect_false(.hu_p("rigolozott", p)$passed)
  expect_identical(.hu_name(p), "Barnaf\u00f6ld (m\u0171velt, v\u00e1lyog)")
  # a land use naming tillage, with no p horizon
  expect_true(.hu_p("muvelt", .hu_mod("brown_earth", site = list(land_use = "arable land")))$passed)
  # ploughed to 8 cm only; not ploughed; nothing recorded
  expect_false(.hu_p("muvelt", .hu_mk(list(designation = c("Ap", "A2", "Bw", "C"), top_cm = c(0, 8, 25, 60),
                                           bottom_cm = c(8, 25, 60, 120), clay_pct = 20, silt_pct = 40, sand_pct = 40))
                     )$passed)
  expect_false(.hu_p("muvelt", make_hscs2024_example("brown_earth"))$passed)
  expect_true(is.na(.hu_p("muvelt", .hu_mod("brown_earth", list(designation = NA)))$passed))
  # the plough layer ends the description above 50 cm: rigolozott undecided
  q <- .hu_mk(list(designation = "Ap", top_cm = 0, bottom_cm = 30, clay_pct = 20, silt_pct = 40, sand_pct = 40))
  expect_true(is.na(.hu_p("rigolozott", q)$passed)); expect_true(.hu_p("muvelt", q)$passed)
  # teraszolt and arasztott from the site
  q <- .hu_mod("brown_earth", site = list(terraced = TRUE))
  expect_identical(.hu_name(q), "Barnaf\u00f6ld (teraszolt, v\u00e1lyog)")
  expect_true(.hu_p("teraszolt", .hu_mod("brown_earth", site = list(land_use = "terraced vineyard")))$passed)
  expect_false(.hu_p("teraszolt", .hu_mod("brown_earth", site = list(terraced = FALSE)))$passed)
  expect_true(is.na(.hu_p("teraszolt", make_hscs2024_example("brown_earth"))$passed))
  q <- .hu_mod("meadow", site = list(flood_irrigated = TRUE))
  expect_identical(.hu_name(q), "R\u00e9ti talaj (\u00e1rasztott, iszapos v\u00e1lyog)")
  expect_true(.hu_p("arasztott", .hu_mod("meadow", site = list(land_use = "rice paddy")))$passed)
  expect_true(is.na(.hu_p("arasztott", make_hscs2024_example("meadow"))$passed))
})

test_that("plough pan, compaction, clods and a pulverised surface", {
  skip_on_cran()
  .hu_only_mine()
  # a plough pan below the Ap: bulk density 1.70 in a loam (limit 1.63)
  p <- .hu_mod("brown_earth", list(designation = c("Ap", "Bw", "C"), bulk_density_g_cm3 = c(1.35, 1.70, 1.50)))
  r <- .hu_p("eketalpas", p)
  expect_true(r$passed); expect_equal(r$top, 25)
  # not 1.80 (the limit that restricts roots in a loam): not tomodott there
  expect_false(.hu_p("tomodott", p)$passed)
  expect_identical(.hu_name(p), "Barnaf\u00f6ld (m\u0171velt, sek\u00e9lyen eketalpas, v\u00e1lyog)")
  # a d suffix (dense layer) passes both
  q <- .hu_mod("brown_earth", list(designation = c("Ap", "Bwd", "C")))
  expect_true(.hu_p("eketalpas", q)$passed); expect_equal(.hu_p("tomodott", q)$top, 25)
  expect_identical(.hu_name(q), paste("Barnaf\u00f6ld (m\u0171velt, sek\u00e9lyen eketalpas,",
                                      "sek\u00e9lyen t\u00f6m\u0151d\u00f6tt, v\u00e1lyog)"))
  # a pan that is not compacted; one with nothing measured; a soil not ploughed
  expect_false(.hu_p("eketalpas", .hu_mod("brown_earth", list(designation = c("Ap", "Bw", "C"),
                                                              bulk_density_g_cm3 = 1.4)))$passed)
  expect_true(is.na(.hu_p("eketalpas", .hu_mod("brown_earth", list(designation = c("Ap", "Bw", "C"))))$passed))
  # compacted from the surface down: no pan distinct from the tilled layer
  expect_false(.hu_p("eketalpas", .hu_mod("brown_earth", list(designation = c("Ap", "Bw", "C"),
                                                              bulk_density_g_cm3 = c(1.70, 1.70, 1.50))))$passed)
  expect_false(.hu_p("eketalpas", make_hscs2024_example("brown_earth"))$passed)
  # tomodott: 3500 kPa in the C at 60 cm
  q <- .hu_mod("brown_earth", list(penetration_resistance_kpa = c(800, 1200, 3500)))
  r <- .hu_p("tomodott", q)
  expect_true(r$passed); expect_equal(r$top, 60)
  expect_identical(.hu_name(q), "Barnaf\u00f6ld (k\u00f6z\u00e9pm\u00e9lyen t\u00f6m\u0151d\u00f6tt, v\u00e1lyog)")
  expect_false(.hu_p("tomodott", .hu_mod("brown_earth", list(bulk_density_g_cm3 = 1.3)))$passed)
  expect_true(is.na(.hu_p("tomodott", make_hscs2024_example("brown_earth"))$passed))
  # hantos: very coarse, very hard clods in the upper 25 cm
  q <- .hu_mod("brown_earth", list(designation = c("Ap", "Bw", "C"), structure_type = list(`1` = "clods"),
                                   structure_size = list(`1` = "very coarse"), rupture_resistance = list(`1` = "very hard")))
  expect_true(.hu_p("hantos", q)$passed)
  expect_identical(.hu_name(q), "Barnaf\u00f6ld (m\u0171velt, hantos, v\u00e1lyog)")
  q2 <- .hu_mod("brown_earth", list(designation = c("Ap", "Bw", "C"), structure_type = list(`1` = "clods"),
                                    rupture_resistance = list(`1` = "very hard")))
  expect_true(is.na(.hu_p("hantos", q2)$passed))
  expect_false(.hu_p("hantos", make_hscs2024_example("brown_earth"))$passed)
  # elporosodott: a ploughed loam in powder; a sand in single grains is not;
  # structure not recorded; not tilled
  q <- .hu_mod("brown_earth", list(designation = c("Ap", "Bw", "C"), structure_type = list(`1` = "powder"),
                                   structure_grade = list(`1` = "structureless")))
  expect_true(.hu_p("elporosodott", q)$passed)
  expect_identical(.hu_name(q), "Barnaf\u00f6ld (m\u0171velt, elporosodott, v\u00e1lyog)")
  q <- .hu_mod("sandy", list(designation = c("Ap", "C1", "C2"), structure_type = list(`1` = "single grain")))
  expect_false(.hu_p("elporosodott", q)$passed)
  q <- .hu_mod("meadow", list(designation = c("Ap", "Bl", "Cr")))
  expect_true(is.na(.hu_p("elporosodott", q)$passed))
  q <- .hu_mod("brown_earth", list(structure_type = list(`1` = "powder")))
  expect_false(.hu_p("elporosodott", q)$passed)
})

test_that("digozott: digo earth in 5 % of the matrix within 50 cm", {
  skip_on_cran()
  .hu_only_mine()
  p <- .hu_mod("solonetz", list(digo_pct = c(0, 8, 0, 0, 0)))
  r <- .hu_p("digozott", p)
  expect_true(r$passed); expect_equal(r$top, 12)
  expect_identical(.hu_name(p), "Szolonyec talaj (sek\u00e9lyen dig\u00f3zott, agyag)")
  expect_false(.hu_p("digozott", .hu_mod("solonetz", list(digo_pct = 0)))$passed)
  expect_true(is.na(.hu_p("digozott", make_hscs2024_example("solonetz"))$passed))
})
