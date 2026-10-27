# v0.9.292: the families of the Australian Soil Classification (3rd ed.):
# the criteria of each order, the readings of readings.yaml (topic
# "Families"), the NatSoil reader's new fields and the family benchmark.

.fm <- function(h, site = list()) {
  n <- max(lengths(h))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  site <- utils::modifyList(list(id = "fam", country = "AU", designation_system = "asc",
                                 drainage_class = "well drained"), site)
  PedonRecord$new(site = site, horizons = ensure_horizon_schema(
    data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}
.fcodes <- function(f) vapply(f, function(a) a$evidence$code, "")
.fcode <- function(p, id, order = "CH", ...) .fcodes(classify_asc3_family(p, order, ...))[[id]]

# a duplex profile: A1 0-12, A2e 12-35, B21t, B22t, C
.fduplex <- function(...) {
  h <- list(designation = c("A1", "A2e", "B21t", "B22t", "C"), top_cm = c(0, 12, 35, 60, 90),
            bottom_cm = c(12, 35, 60, 90, 130), field_texture = c("SL", "SL", "LMC", "MC", "SCL"))
  .fm(utils::modifyList(h, list(...)))
}

test_that("the example profiles get the family classes of their order", {
  skip_on_cran()
  fam_of <- function(o) classify_asc3(make_asc3_example(o), on_missing = "silent", include_family = TRUE)
  r <- fam_of("chromosols")
  expect_identical(r$name, "Bleached, Eutrophic, Red Chromosol; thick, non-gravelly, loamy/clayey, deep")
  expect_identical(r$trace$family_code, "C E L O W YY")       # water repellence not tested
  expect_identical(r$trace$family_label, "thick, non-gravelly, loamy/clayey, deep")
  expect_s3_class(r$trace$family$thickness, "FamilyAttribute")
  expect_identical(fam_of("dermosols")$trace$family_code, "B E M O W")       # no water repellence criterion
  expect_identical(fam_of("kandosols")$trace$family_code, "B E L M W YY")    # the A1 (15 cm), not the whole A
  expect_identical(fam_of("vertosols")$trace$family_code, "E R R X")         # 60 % clay is medium fine
  expect_identical(fam_of("organosols")$trace$family_code, "D E OC O X")
  expect_identical(fam_of("rudosols")$trace$family_code, "H K U YY")         # gravel 20 %, rock at 0.35 m
  expect_identical(fam_of("calcarosols")$trace$family_code, "B E L M W YY")  # the Bk at 0.12 m
  expect_identical(fam_of("hydrosols")$trace$family_label, "medium, non-gravelly, clay loamy/clayey, moderately deep")
  a <- fam_of("anthroposols")
  expect_identical(a$name, "Spolic Anthroposol")
  expect_length(a$trace$family, 0L)
  # without include_family the result is as before
  r0 <- classify_asc3(make_asc3_example("chromosols"), on_missing = "silent")
  expect_identical(r0$name, "Bleached, Eutrophic, Red Chromosol")
  expect_null(r0$trace$family)
  # classify_all forwards include_family
  ca <- classify_all(make_asc3_example("chromosols"), systems = "asc3", on_missing = "silent",
                     include_family = TRUE)
  expect_identical(ca$asc3$name, r$name)
  # the family alone, the soil classified first
  expect_identical(.fcodes(classify_asc3_family(make_asc3_example("chromosols"))),
                   c(thickness = "C", gravel = "E", a1_texture = "L", b_texture = "O", depth = "W",
                     water_repellence = "YY"))
})

test_that("thickness: the whole A in the texture-contrast orders, the A1 in the others", {
  skip_on_cran()
  p <- .fduplex()
  expect_identical(.fcode(p, "thickness", "CH"), "C")          # A1 + A2 = 35 cm
  expect_identical(.fcode(p, "thickness", "SO"), "C")
  expect_identical(.fcode(p, "thickness", "KU"), "C")
  expect_identical(.fcode(p, "thickness", "DE"), "B")          # the A1, 12 cm
  expect_identical(.fcode(p, "thickness", "KA"), "B")
  # the Hydrosols by their great group: Kurosolic, Sodosolic, Chromosolic the whole A
  f <- classify_asc3_family(p, "HY", suborder = "ED", great_group = "BG")
  expect_identical(f$thickness$evidence$code, "C")
  f <- classify_asc3_family(p, "HY", suborder = "ED", great_group = "CY")
  expect_identical(f$thickness$evidence$code, "B")
  expect_identical(f$thickness$value, "medium")                # the Hydrosols' name of class B
  expect_identical(classify_asc3_family(p, "HY", suborder = "ED")$thickness$evidence$code, "YY")
  # organic horizons over the A1 count, and the A1 texture is then peaty
  q <- .fm(list(designation = c("O2", "A1", "B2"), top_cm = c(0, 4, 12), bottom_cm = c(4, 12, 60),
                field_texture = c(NA, "L", "LC"), oc_pct = c(20, 2, 0.5)))
  expect_identical(.fcode(q, "thickness", "DE"), "B")          # 4 + 8 = 12 cm
  expect_identical(.fcode(q, "a1_texture", "DE"), "J")
  expect_identical(.fcode(q, "a1_texture", "SO"), "ZZ")        # the Sodosols list no peaty class
  # a description ending in the A1 leaves its thickness open
  r <- .fm(list(designation = "A1", top_cm = 0, bottom_cm = 20, field_texture = "L"))
  expect_identical(.fcode(r, "thickness", "DE"), "YY")
  # a horizon of unknown role in the A: decided only when both readings agree
  s <- .fm(list(designation = c("A1", NA, "B2"), top_cm = c(0, 15, 25), bottom_cm = c(15, 25, 80),
                field_texture = c("L", "L", "LC")))
  expect_identical(.fcode(s, "thickness", "DE"), "B")          # 15 or 25 cm: both moderately thick
  s2 <- .fm(list(designation = c("A1", NA, "B2"), top_cm = c(0, 25, 40), bottom_cm = c(25, 40, 80),
                 field_texture = c("L", "L", "LC")))
  expect_identical(.fcode(s2, "thickness", "DE"), "YY")        # 25 or 40 cm
})

test_that("gravel: the larger of the ground surface and the A1, gravel-sized fragments only", {
  skip_on_cran()
  p <- .fduplex(coarse_fragments_pct = c(5, 5, 0, 0, 0))
  expect_identical(.fcode(p, "gravel"), "F")                   # the A1 alone
  p$site$surface_gravel_pct <- 30
  expect_identical(.fcode(p, "gravel"), "H")                   # the surface is more gravelly
  p$site$surface_gravel_pct <- 0
  expect_identical(.fcode(p, "gravel"), "F")                   # the A1 is more gravelly
  p$site$surface_gravel_pct <- NA; p$site$surface_coarse_fragments_pct <- 60
  expect_identical(.fcode(p, "gravel"), "I")                   # all sizes, when gravel is not given
  # gravel_pct (2-60 mm) before coarse_fragments_pct (all sizes)
  q <- .fduplex(coarse_fragments_pct = c(40, 0, 0, 0, 0), gravel_pct = c(3, NA, NA, NA, NA))
  expect_identical(.fcode(q, "gravel"), "F")
  # no record: none
  expect_identical(.fcode(.fduplex(), "gravel"), "E")
  # the Rudosols: the surface soil only, not the ground surface
  r <- .fduplex(coarse_fragments_pct = c(5, 5, 0, 0, 0)); r$site$surface_gravel_pct <- 30
  expect_identical(.fcode(r, "gravel", "RU"), "F")
  # limits: 2 % slightly gravelly, 50 % moderately gravelly
  expect_identical(.fcode(.fduplex(coarse_fragments_pct = c(2, 0, 0, 0, 0)), "gravel"), "F")
  expect_identical(.fcode(.fduplex(coarse_fragments_pct = c(50, 0, 0, 0, 0)), "gravel"), "H")
})

test_that("textures: the major part of the A1, the most clayey class of the B", {
  skip_on_cran()
  a1 <- function(tex, tops = c(0, 5, 20), ...) .fm(list(designation = c("A11", "A12", "B2"), top_cm = tops,
                                                       bottom_cm = c(tops[-1], 80), field_texture = tex, ...))
  expect_identical(.fcode(a1(c("SL", "SCL", "LC")), "a1_texture", "DE"), "M")       # 15 of 20 cm
  expect_identical(.fcode(a1(c("SL", "SCL", "LC"), tops = c(0, 10, 20)), "a1_texture", "DE"), "YY")  # a tie
  expect_identical(.fcode(a1(c("SL", NA, "LC")), "a1_texture", "DE"), "YY")         # unknown major part
  expect_identical(.fcode(a1(c("SL+", "FSL", "LC")), "a1_texture", "DE"), "L")
  # lab clay where no field texture is recorded, undecided near the limits
  lab <- function(clay, silt = NA) .fm(list(designation = c("A1", "B2"), top_cm = c(0, 10), bottom_cm = c(10, 60),
                                            field_texture = c(NA, "LC"), clay_pct = c(clay, 40), silt_pct = c(silt, 20)))
  expect_identical(.fcode(lab(40), "a1_texture", "DE"), "O")
  expect_identical(.fcode(lab(36), "a1_texture", "DE"), "YY")
  expect_identical(.fcode(lab(30, 40), "a1_texture", "DE"), "N")
  expect_identical(.fcode(lab(30, 10), "a1_texture", "DE"), "M")
  expect_identical(.fcode(lab(30), "a1_texture", "DE"), "YY")                      # silt not given
  expect_identical(.fcode(lab(5), "a1_texture", "DE"), "K")
  # the B: the most clayey class
  b <- function(t1, t2) .fm(list(designation = c("A1", "B21", "B22"), top_cm = c(0, 10, 40),
                                 bottom_cm = c(10, 40, 80), field_texture = c("L", t1, t2)))
  expect_identical(.fcode(b("LC", "MC"), "b_texture", "DE"), "O")
  expect_identical(.fcode(b("SCL", "CL"), "b_texture", "DE"), "M")
  expect_identical(.fcode(b("SCL", "ZCL"), "b_texture", "DE"), "N")                # ZCL is more clayey
  expect_identical(.fcode(b("CL", "ZCL"), "b_texture", "DE"), "YY")                # a tie
  expect_identical(.fcode(b("SCL", NA), "b_texture", "DE"), "YY")                  # may be more clayey
  expect_identical(.fcode(b("MC", NA), "b_texture", "DE"), "O")                    # nothing is
  expect_identical(.fcode(b("S", "LS"), "b_texture", "CH"), "ZZ")                  # not in the Chromosol list
  # no B: the criterion does not apply
  nob <- .fm(list(designation = c("A1", "C"), top_cm = c(0, 10), bottom_cm = c(10, 120), field_texture = c("S", "S")))
  expect_false("b_texture" %in% names(classify_asc3_family(nob, "TE")))
  # the Tenosols' B may include a C/B
  cb <- .fm(list(designation = c("A1", "B2", "C/B"), top_cm = c(0, 10, 40), bottom_cm = c(10, 40, 80),
                 field_texture = c("S", "LS", "SL")))
  expect_identical(.fcode(cb, "b_texture", "TE"), "L")
  expect_identical(.fcode(cb, "b_texture", "DE"), "K")
  # the label leaves an undecided texture out of the pair
  lab_f <- .asc_family_label(classify_asc3_family(b("SCL", NA), "DE"))
  expect_false(grepl("/", lab_f, fixed = TRUE))
})

test_that("soil depth: rock or a cemented pan, else the C under the B, else the depth described", {
  skip_on_cran()
  d <- function(des, tops, bots, ..., order = "DE") .fcode(.fm(list(designation = des, top_cm = tops, bottom_cm = bots,
                                                                    field_texture = "L", ...)), "depth", order)
  expect_identical(d(c("A1", "B2", "C"), c(0, 10, 40), c(10, 40, 200)), "U")       # the C at 0.4 m
  expect_identical(d(c("A1", "B2", "D"), c(0, 10, 40), c(10, 40, 200)), "X")       # a D is soil
  expect_identical(d(c("A1", "C"), c(0, 10), c(10, 200), order = "RU"), "X")       # no B: the C counts
  expect_identical(d(c("A1", "C", "R"), c(0, 10, 35), c(10, 35, 60), order = "RU"), "U")
  expect_identical(d(c("A1", "B2", "B3"), c(0, 10, 60), c(10, 60, 150)), "X")      # 1.5 m: very deep
  expect_identical(d(c("A1", "B2", "B3"), c(0, 10, 60), c(10, 60, 149)), "W")
  # a thin ironpan does not end the soil; a cemented pan does
  expect_identical(d(c("A1", "B2", "B3"), c(0, 10, 40), c(10, 40, 120),
                     pan_type = c(NA, "thin ironpan", NA), cementation_class = c(NA, "strongly", NA)), "W")
  expect_identical(d(c("A1", "B2", "B3"), c(0, 10, 40), c(10, 40, 120),
                     pan_type = c(NA, NA, "ferricrete"), cementation_class = c(NA, NA, "strongly")), "U")
  # saprolite named as such
  expect_identical(d(c("A1", "B2", "C"), c(0, 10, 60), c(10, 60, 200), layer_origin = c(NA, NA, "saprolite"),
                     order = "DE"), "V")
  # a horizon of unknown role below the B may be a C: a range
  expect_identical(d(c("A1", "B2", NA, "B3"), c(0, 10, 60, 70), c(10, 60, 70, 140)), "YY")  # 0.6 or 1.4 m
  expect_identical(d(c("A1", "B2", NA, "B3"), c(0, 10, 60, 70), c(10, 60, 70, 90)), "V")    # 0.6 or 0.9 m
  # the Arenosols list only 1.0 m or more
  expect_identical(d(c("A1", "C"), c(0, 20), c(20, 80), order = "RE"), "ZZ")
  # only an O1 designated, the rest outside the description: undecided, no error
  expect_identical(d(c("O1", NA, NA), c(0, 1, 10), c(1, 10, 40), oc_pct = c(30, 5, 1)), "YY")
  # the Organosols measure from the top of their organic horizons
  expect_identical(d(c("O1", "P", "C", "R"), c(0, 5, 30, 48), c(5, 30, 48, 60), order = "OR",
                     oc_pct = c(40, 30, 1, NA)), "U")
})

test_that("water repellence of the surface soil", {
  skip_on_cran()
  w <- function(v) .fcode(.fduplex(water_repellence = c(v, NA, NA, NA, NA)), "water_repellence")
  expect_identical(w("non-repellent"), "NR")
  expect_identical(w("repellent"), "WR")
  expect_identical(w("strongly repellent"), "SR")
  expect_identical(w("S"), "SR")
  expect_identical(w(NA), "YY")
  expect_false("water_repellence" %in% names(classify_asc3_family(.fduplex(), "DE")))  # not a Dermosol criterion
})

test_that("Calcarosols, Vertosols, Organosols, Arenosols and Hydrosols", {
  skip_on_cran()
  # Calcarosols: the depth of the Bk, only when there is one
  ca <- .fm(list(designation = c("A1", "B21", "B22k"), top_cm = c(0, 15, 45), bottom_cm = c(15, 45, 90),
                 field_texture = c("SL", "SCL", "SCL")))
  expect_identical(.fcode(ca, "bk_depth", "CA"), "C")          # 0.45 m
  noc <- .fm(list(designation = c("A1", "B2"), top_cm = c(0, 15), bottom_cm = c(15, 90), field_texture = "SL"))
  expect_false("bk_depth" %in% names(classify_asc3_family(noc, "CA")))
  # Vertosols: lab clay of the upper 0.1 m below a crusty surface horizon, and of the B
  ve <- function(clay) .fm(list(designation = c("A11", "A12", "B21", "B22"), top_cm = c(0, 2, 10, 50),
                                bottom_cm = c(2, 10, 50, 100), field_texture = "MC", clay_pct = clay,
                                structure_grade = c("structureless", "strong", "strong", "strong"),
                                structure_type = c("massive", "granular", "lenticular", "lenticular")))
  expect_identical(.fcode(ve(c(30, 50, 50, 65)), "upper_clay", "VE"), "R")   # the 2 cm crust left out
  expect_identical(.fcode(ve(c(30, 50, 50, 65)), "b_clay", "VE"), "S")
  expect_identical(.fcode(ve(c(30, 50, NA, 50)), "upper_clay", "VE"), "YY")  # part of the window not measured
  expect_identical(.fcode(ve(c(30, 50, 50, NA)), "b_clay", "VE"), "YY")
  expect_identical(.fcode(ve(c(30, 50, 62, NA)), "b_clay", "VE"), "S")
  # Organosols: altered materials only when present; organic thickness classes
  or <- .fm(list(designation = c("P1", "P2", "C"), top_cm = c(0, 25, 70), bottom_cm = c(25, 70, 120),
                 field_texture = c("GP", "AP", "LS"), oc_pct = c(35, 30, 1)),
            list(drainage_class = "very poorly drained"))
  f <- classify_asc3_family(or, "OR")
  expect_identical(.fcodes(f)[["alteration"]], "P")            # a granular peat, 25 cm
  expect_identical(.fcodes(f)[["organic_type"]], "OC")         # sapric 45 cm, the granular one of unknown kind
  expect_identical(.fcodes(f)[["underlying_texture"]], "K")
  expect_false("alteration" %in% names(classify_asc3_family(make_asc3_example("organosols"), "OR")))
  thin <- .fm(list(designation = c("P", "C", "R"), top_cm = c(0, 8, 40), bottom_cm = c(8, 40, 50),
                   field_texture = c("AP", "L", NA), oc_pct = c(30, 1, NA)))
  expect_identical(.fcode(thin, "organic_thickness", "OR"), "ZZ")  # below 0.1 m: no class
  # Arenosols: maximum texture below the surface soil within 1.0 m
  re <- .fm(list(designation = c("A1", "C1", "C2"), top_cm = c(0, 10, 60), bottom_cm = c(10, 60, 160),
                 field_texture = c("S", "LS", "SL")))
  f <- classify_asc3_family(re, "RE")
  expect_identical(.fcodes(f), c(gravel = "E", surface_texture = "K", subsoil_texture = "L", depth = "X",
                                 water_repellence = "YY"))
  # Hydrosols: full, partial or no criteria by suborder
  hy <- .fduplex()
  expect_length(classify_asc3_family(hy, "HY", suborder = "IU"), 0L)
  expect_length(classify_asc3_family(hy, "HY"), 0L)
  expect_named(classify_asc3_family(hy, "HY", suborder = "EW"),
               c("gravel", "surface_texture", "subsoil_texture", "depth", "water_repellence"))
  expect_named(classify_asc3_family(hy, "HY", suborder = "EG", great_group = "BG"),
               c("thickness", "gravel", "a1_texture", "b_texture", "depth", "water_repellence"))
  expect_length(classify_asc3_family(hy, "AN"), 0L)
})

test_that("families.yaml is consistent with the key", {
  skip_on_cran()
  fr <- soilKey:::.asc_family_rules()
  orders <- vapply(load_rules("asc3")$orders, function(t) t$code, "")
  expect_setequal(setdiff(orders, "AN"), setdiff(names(fr$orders), "HY-partial"))
  for (o in names(fr$orders)) for (cr in fr$orders[[o]]) {
    expect_true(cr$kind %in% names(fr$classes), info = paste(o, cr$id))
    codes <- vapply(fr$classes[[cr$kind]], function(k) k$code, "")
    if (!is.null(cr$classes)) expect_true(all(unlist(cr$classes) %in% codes), info = paste(o, cr$id))
  }
  hy <- vapply(load_rules("asc3")$suborders$HY, function(t) t$code, "")
  expect_true(all(unlist(fr$hydrosol_suborders) %in% hy))
})

test_that("NatSoil: gravel, water repellence, surface fragments and the surveyors' family", {
  skip_on_cran()
  skip_if_not_installed("jsonlite")
  hz <- function(master, subdiv, top, bottom, texture, extra = list())
    c(list(h_desig_master = master, h_desig_subdiv = subdiv, h_depth_upper = top, h_depth_lower = bottom,
           h_texture = texture, z_colours = list(list(col_moisture_stat = "M", col_hue = "10YR", col_value = 4,
                                                      col_chroma = 3))), extra)
  site <- list(
    agency_code = "999", proj_code = "FAM", s_id = "1",
    usage_license = "https://creativecommons.org/licenses/by/4.0/", citation = "Synthetic",
    z_observations = list(list(
      o_asc_ord = "CH", o_asc_subord = "AB", o_asc_gg = "AH", o_asc_subg = "BS",
      o_asc_fam1 = "C", o_asc_fam2 = "F", o_asc_fam3 = "L", o_asc_fam4 = "O", o_asc_fam5 = "W",
      z_surface_coarse_frags = list(list(scf_abun = "3", scf_size = "2"), list(scf_abun = "2", scf_size = "4")),
      z_horizons = list(
        hz("A", "1", 0, 0.12, "SL", list(h_water_repellence = "R",
                                         z_coarse_frags = list(list(cf_abun = "2", cf_size = "1"),
                                                               list(cf_abun = "3", cf_size = "5"),
                                                               list(cf_abun = "1")))),
        hz("B", "2", 0.12, 0.6, "MC")))))
  dir <- withr::local_tempdir(); f <- file.path(dir, "s.json.gz")
  con <- gzfile(f, "w"); writeLines(jsonlite::toJSON(list(site), auto_unbox = TRUE, null = "null"), con); close(con)
  p <- load_natsoil_pedons(f, verbose = FALSE)[[1]]
  h <- as.data.frame(p$horizons)
  expect_equal(h$coarse_fragments_pct[1], 6 + 15 + 1)          # all sizes
  expect_equal(h$gravel_pct[1], 6 + 1)                          # gravel and the record of no size
  expect_identical(h$water_repellence[1], "repellent")
  expect_true(is.na(h$gravel_pct[2]))
  expect_equal(p$site$surface_coarse_fragments_pct, 21)
  expect_equal(p$site$surface_gravel_pct, 15)                   # cobbles left out
  expect_identical(p$site$reference_asc_family, "C;F;L;O;W")
})

test_that("the NatSoil benchmark compares each family criterion", {
  skip_on_cran()
  p <- make_asc3_example("chromosols")
  r <- classify_asc3(p, on_missing = "silent")
  lv <- r$levels$code
  p$site$reference_asc_order <- lv[1]; p$site$reference_asc_suborder <- lv[2]
  p$site$reference_asc_great_group <- lv[3]; p$site$reference_asc_subgroup <- lv[4]
  p$site$reference_asc_family <- "C;E;M;O;W"
  b <- benchmark_natsoil(list(p), verbose = FALSE, family = TRUE)
  by <- b$families$by_criterion
  expect_identical(by$criterion, c("thickness", "gravel", "a1_texture", "b_texture", "depth"))
  expect_identical(by$agreement, c(100, 100, 0, 100, 100))
  expect_null(benchmark_natsoil(list(p), verbose = FALSE)$families)
})

test_that("review cases: unknown data stay undecided, Organosols with a mineral surface", {
  skip_on_cran()
  # an Organosol whose organic materials lie under a mineral A1
  p <- .fm(list(designation = c("A1", "P1", "P2", "C"), top_cm = c(0, 15, 50, 120), bottom_cm = c(15, 50, 120, 160),
                field_texture = c("L", "AP", "AP", "S"), oc_pct = c(3, 40, 40, 0.5)),
           list(drainage_class = "very poorly drained"))
  f <- classify_asc3_family(p, "OR")
  expect_identical(.fcodes(f)[["organic_thickness"]], "D")
  expect_identical(.fcodes(f)[["organic_type"]], "OC")
  # the description ends in peat: the texture under it is unknown, not absent
  q <- .fm(list(designation = c("P1", "P2"), top_cm = c(0, 40), bottom_cm = c(40, 150),
                field_texture = c("AP", "AP"), oc_pct = c(40, 40)))
  expect_identical(.fcode(q, "underlying_texture", "OR"), "YY")
  # a gravel record of unknown abundance (NaN) leaves the gravel unknown
  g <- .fduplex(gravel_pct = c(NaN, NA, NA, NA, NA))
  expect_identical(.fcode(g, "gravel"), "YY")
  g$site$surface_gravel_pct <- 60                                  # the surface decides it anyway
  expect_identical(.fcode(g, "gravel"), "I")
  h <- .fduplex(); h$site$surface_gravel_pct <- NaN
  expect_identical(.fcode(h, "gravel"), "YY")
  # rows that may belong to the A1: every A1 they allow
  r <- .fm(list(designation = c("A1", NA, NA, "B2"), top_cm = c(0, 10, 20, 30), bottom_cm = c(10, 20, 30, 120),
                field_texture = "L", gravel_pct = c(2, 40, 2, 0)))
  expect_identical(.fcode(r, "gravel", "DE"), "YY")                # 2 %, 21 % or 14.7 %
  # a CB ends the soil like a C/B; the Tenosols' B may hold one
  d <- function(x) .fcode(.fm(list(designation = c("A1", "B2", x, "C"), top_cm = c(0, 10, 40, 70),
                                   bottom_cm = c(10, 40, 70, 200), field_texture = "L")), "depth", "DE")
  expect_identical(d("C/B"), "U"); expect_identical(d("CB"), "U")
  # a horizon of unknown depth
  na_top <- .fm(list(designation = c("A1", "B2", "C"), top_cm = c(0, NA, 50), bottom_cm = c(10, 50, 120),
                     field_texture = "L"))
  expect_identical(.fcode(na_top, "depth", "DE"), "YY")
  bk_na <- .fm(list(designation = c("A1", "B21", "B22k"), top_cm = c(0, 15, NA), bottom_cm = c(15, 45, 90),
                    field_texture = "SL"))
  expect_identical(.fcode(bk_na, "bk_depth", "CA"), "YY")
  # the subsoil: a row that may belong to the surface soil may be subsoil
  s <- .fm(list(designation = c("A1", NA, "C"), top_cm = c(0, 10, 20), bottom_cm = c(10, 20, 150),
                field_texture = c("S", "SL", "S")))
  expect_identical(.fcode(s, "subsoil_texture", "RE"), "YY")
  # an undecided A1 texture takes its pair out of the name
  u <- .fm(list(designation = c("A11", "A12", "B2"), top_cm = c(0, 10, 20), bottom_cm = c(10, 20, 80),
                field_texture = c("SL", "SCL", "LC")))
  lab_u <- .asc_family_label(classify_asc3_family(u, "DE"))
  expect_false(grepl("clayey", lab_u, fixed = TRUE))
})

test_that("the family benchmark finds the codes by their kind", {
  skip_on_cran()
  p <- make_asc3_example("rudosols")
  r <- classify_asc3(p, on_missing = "silent")
  lv <- r$levels$code
  p$site$reference_asc_order <- lv[1]; p$site$reference_asc_suborder <- lv[2]
  p$site$reference_asc_family <- "B;H;K;K;U"        # an older layout with a thickness and two textures
  by <- benchmark_natsoil(list(p), verbose = FALSE, family = TRUE)$families$by_criterion
  expect_identical(by$criterion, c("gravel", "surface_texture", "depth"))
  expect_identical(by$agreement, c(100, 100, 100))
})

test_that("second review: litter over a mineral cover, BA horizons, blank slots", {
  skip_on_cran()
  # an O1 litter over a mineral cover over the peat: the peat is read
  p <- .fm(list(designation = c("O1", "A1", "P", "C"), top_cm = c(0, 3, 15, 90), bottom_cm = c(3, 15, 90, 130),
                field_texture = c(NA, "L", "AP", "S"), oc_pct = c(45, 3, 40, 0.5)))
  f <- classify_asc3_family(p, "OR")
  expect_identical(.fcodes(f)[["organic_thickness"]], "D")
  expect_identical(.fcodes(f)[["organic_type"]], "OC")
  # an O1 directly over the peat counts in its thickness
  q <- .fm(list(designation = c("O1", "P", "C"), top_cm = c(0, 5, 30), bottom_cm = c(5, 30, 80),
                field_texture = c(NA, "AP", "S"), oc_pct = c(45, 40, 0.5)))
  expect_identical(.fcode(q, "organic_thickness", "OR"), "C")      # 30 cm
  # a BA marked k is the Bk
  ba <- .fm(list(designation = c("A1", "BAk", "B2k"), top_cm = c(0, 8, 12), bottom_cm = c(8, 12, 60),
                 field_texture = "SL"))
  expect_identical(.fcode(ba, "bk_depth", "CA"), "A")              # 8 cm
  # a blank slot of a record kept by position is a criterion not given
  ch <- make_asc3_example("chromosols")
  lv <- classify_asc3(ch, on_missing = "silent")$levels$code
  ch$site$reference_asc_order <- lv[1]; ch$site$reference_asc_suborder <- lv[2]
  ch$site$reference_asc_family <- "C;E;;O;W"
  by <- benchmark_natsoil(list(ch), verbose = FALSE, family = TRUE)$families$by_criterion
  expect_identical(by$criterion, c("thickness", "gravel", "b_texture", "depth"))
  expect_identical(by$agreement, c(100, 100, 100, 100))
})
