# v0.9.320: Queensland SALI read from its site report (a PDF, read from the
# coordinates of its words) and its soil chemistry API (OData Samples). Every
# word table and JSON record here is made up, in the shape pdftools::pdf_data()
# and the API return; no network and no SALI data.

# The words of one line: each cell is list(x, "text"), its words laid out
# from x (4 px per character, 3 px between words), as pdf_data() gives them.
.sw <- function(y, ...) {
  do.call(rbind, lapply(list(...), function(cl) {
    words <- strsplit(cl[[2]], " ", fixed = TRUE)[[1]]
    w <- 4 * nchar(words)
    data.frame(width = w, height = 7, x = cl[[1]] + cumsum(c(0, utils::head(w + 3, -1))), y = y,
               space = TRUE, text = words, stringsAsFactors = FALSE)
  }))
}

# The page header of the report (removed by the reader).
.sali_hdr <- function(page, obs = 1L) {
  rbind(.sw(20, list(12, "Date: 01/01/2026 00:00:00"), list(241, "Queensland Government"), list(555, paste("Page:", page))),
        .sw(34, list(240, "Soil and Land Information")),
        .sw(48, list(12, sprintf("Project TST Site 1 Observation %d", obs)), list(256, "Site Listing Report"),
            list(539, "SALI4320")))
}

# The morphology header (the depths' "depth" and "(m)" on the lines below).
.sali_morph_hdr <- function(y) {
  rbind(.sw(y, list(12, "No"), list(28, "Name"), list(57, "Upper"), list(85, "Lower"), list(114, "Colour"),
            list(194, "Mottles"), list(247, "Textures"), list(304, "Structures"), list(361, "Coarse"),
            list(389, "fragments"), list(439, "Segregations"), list(496, "Strengths"), list(553, "Bounds")),
        .sw(y + 8, list(57, "depth"), list(85, "depth")),
        .sw(y + 16, list(57, "(m)"), list(85, "(m)")))
}

# A two-page report: site characteristics, the classification, a morphology
# table of three horizons whose cells wrap over lines and whose last row is
# on page 2 under a repeated header, field tests and a lab table of two
# samples (one a MIR prediction).
.sali_pages <- function() {
  p1 <- rbind(
    .sali_hdr(1),
    .sw(70, list(250, "Site characteristics")),
    .sw(82, list(12, "Date described:"), list(100, "16/12/1981"), list(330, "Observation type:"), list(420, "Soil pit")),
    .sw(94, list(12, "Drainage:"), list(100, "Poorly drained"), list(330, "Observation class:"),
        list(420, "Class IIIb (core analytical suite)")),
    .sw(106, list(12, "Surface condition:"), list(100, "Self-mulching"), list(330, "Landform element:"),
        list(420, "Inter-tidal flat")),
    .sw(118, list(12, "Surface coarse"), list(100, "Few (2-10%) fine gravel"), list(330, "Landform pattern:"),
        list(420, "Tidal flat")),
    .sw(126, list(12, "fragments:")),
    .sw(140, list(250, "Soil classification")),
    .sw(152, list(12, "Australian Soil Classification (ASC)"), list(180, "Confidence"),
        list(300, "ASC Technical Reference"), list(470, "Buried"), list(500, "GSG"), list(530, "PPF")),
    .sw(164, list(12, "Black Vertosol"), list(180, "All necessary analytical data"), list(300, "Isbell and NCST (2021) The"),
        list(470, "N/A"), list(500, "Grey clay"), list(530, "Ug5.16")),
    .sw(172, list(180, "are available"), list(300, "Australian Soil Classification")),
    .sw(180, list(300, "Third Edition")),
    .sw(200, list(255, "Profile morphology")),
    .sali_morph_hdr(214),
    # horizon 1: the colour starts 3 px above the row number
    .sw(239, list(114, "Black (10YR 2/1)")),
    .sw(242, list(12, "1"), list(28, "A1"), list(57, "0"), list(85, "0.1"), list(247, "Light medium"),
        list(304, "Strong <2 mm"), list(361, "No coarse"), list(496, "Moderately"), list(553, "Clear")),
    .sw(247, list(114, "moist")),
    .sw(250, list(247, "clay"), list(304, "granular"), list(361, "fragments"), list(496, "moist, weak")),
    .sw(254, list(114, "______")),
    .sw(258, list(304, "structure"), list(496, "strength")),
    # horizon 2
    .sw(269, list(114, "Dark grey")),
    .sw(272, list(12, "2"), list(28, "B21"), list(57, "0.1"), list(85, "0.5"), list(194, "Few (2-10%)"),
        list(247, "Medium"), list(304, "Moderate 20-50"), list(439, "Few (2-10%)"), list(496, "Dry, firm"),
        list(553, "Gradual")),
    .sw(277, list(114, "(10YR 4/1) moist")),
    .sw(280, list(194, "distinct"), list(247, "heavy clay"), list(304, "mm lenticular"), list(439, "calcareous soft"),
        list(496, "strength")),
    .sw(288, list(194, "mottles"), list(304, "structure"), list(439, "segregations")))
  p2 <- rbind(
    .sali_hdr(2),
    .sali_morph_hdr(60),
    # horizon 3, continued on page 2 below the repeated header
    .sw(87, list(114, "Grey (N 5/)")),
    .sw(90, list(12, "3"), list(28, "B22"), list(57, "0.5"), list(85, "1.2"), list(194, "Few (2-10%)"),
        list(247, "Sandy clay"), list(304, "Moderate blocky"), list(496, "Firm strength")),
    .sw(98, list(194, "biological"), list(304, "structure")),
    .sw(106, list(194, "mixing")),
    .sw(117, list(13, "Field test"), list(157, "Field test")),
    .sw(130, list(13, "pH by Raupach and Tucker method"), list(157, "Carbonate effervescence")),
    .sw(142, list(13, "Depth (m)"), list(83, "Value"), list(157, "Depth (m)"), list(227, "Value")),
    .sw(154, list(13, "0.05"), list(83, "5.5"), list(157, "1.0"), list(227, "M")),
    .sw(166, list(13, "0.3"), list(83, "8.5")),
    .sw(222, list(248, "Laboratory test results")),
    .sw(235, list(470, "Sample"), list(549, "1"), list(589, "2")),
    .sw(247, list(455, "Upper depth (m)"), list(549, "0"), list(589, "0.3")),
    .sw(259, list(455, "Lower depth (m)"), list(549, "0.1"), list(589, "0.5")),
    .sw(271, list(12, "Group"), list(99, "Method"), list(273, "Code"), list(360, "Units"), list(457, "Bulked Sample"),
        list(549, "N"), list(589, "Y")),
    .sw(283, list(99, "pH - 1:5 water"), list(273, "4A1"), list(549, "6.5"), list(589, "8.0")),
    .sw(295, list(99, "Clay"), list(273, "2Z2_Clay"), list(360, "%"), list(549, "12"), list(589, "45")),
    .sw(307, list(99, "CEC"), list(273, "15C1_CEC"), list(360, "cmol_c/kg"), list(549, "-"), list(589, "30")),
    .sw(319, list(99, "Exch Na"), list(273, "15C1_Na_MC2"), list(360, "cmol_c/kg"), list(549, "0.4")),
    .sw(331, list(69, "Based on or contains data provided by the State of Queensland")))
  list(p1, p2)
}

.sali_layer_row <- function(label = "Vertosol Black Self-Mulching Epicalcareous-Endohypersodic") {
  data.frame(project_code = "TST", site_id = 1L, australian_soil_classification = label,
             latitude = -27.4, longitude = 153.1, stringsAsFactors = FALSE)
}

.res <- function(code, value, valid = "Y") list(LabMethodCode = code, FormattedValue = value, ValidFlag = valid)
.smp <- function(no, top, bot, results, obs = 1L, bulk = "N")
  list(ProjectCode = "TST", SiteId = 1L, ObservationNumber = obs, SampleNumber = no, UpperDepth = top,
       LowerDepth = bot, BulkFlag = bulk, SiteLabMethodResults = results)

test_that("the report's word coordinates give the site, the class and a table wrapped over two pages", {
  skip_on_cran()
  pw <- .sali_pdf_words(.sali_pages())
  expect_identical(unique(pw$obs), 1L)
  expect_false(any(pw$words$text == "SALI4320"))                       # page headers removed
  x <- .sali_pdf_parse(pw)
  expect_identical(x$site[["Date described"]], "16/12/1981")
  expect_identical(x$site[["Observation type"]], "Soil pit")
  expect_identical(x$site[["Drainage"]], "Poorly drained")
  expect_identical(x$site[["Surface coarse fragments"]], "Few (2-10%) fine gravel")   # label over two lines
  expect_identical(x$site[["Landform element"]], "Inter-tidal flat")
  expect_identical(x$asc$asc, "Black Vertosol")
  expect_identical(x$asc$confidence, "All necessary analytical data are available")
  expect_identical(.sali_edition(x$asc$reference), "3rd (2021)")
  tb <- x$table
  expect_identical(nrow(tb), 3L)                                        # row 3 on page 2, header repeated
  expect_identical(tb$Name, c("A1", "B21", "B22"))
  expect_identical(tb$Upper, c("0", "0.1", "0.5")); expect_identical(tb$Lower, c("0.1", "0.5", "1.2"))
  expect_identical(tb$Colour[1], "Black (10YR 2/1) moist ______")         # cell begun above the row number
  expect_identical(tb$Textures[2], "Medium heavy clay")
  expect_identical(tb$Structures[2], "Moderate 20-50 mm lenticular structure")
  expect_identical(tb$Mottles[3], "Few (2-10%) biological mixing")
  expect_identical(nrow(x$ftests), 3L)
  expect_identical(x$ftests$test[1], "pH by Raupach and Tucker method")
  expect_identical(x$ftests$value[x$ftests$test == "Carbonate effervescence"], "M")
  lab <- .sali_pdf_lab(pw)
  expect_setequal(lab$code, c("4A1", "2Z2_Clay", "15C1_CEC", "15C1_Na_MC2"))
  expect_identical(lab$value[lab$sample == "2" & lab$code == "15C1_CEC"], "30")
  expect_false(any(lab$sample == "1" & lab$code == "15C1_CEC"))         # "-" dropped
  expect_identical(lab$upper_m[lab$sample == "2"][1], 0.3)
  expect_identical(lab$bulked[lab$sample == "2"][1], "Y")
})

test_that("a later observation is not read, and a report without the table gives no pedon", {
  skip_on_cran()
  pg <- .sali_pages()
  pg3 <- rbind(.sali_hdr(3, obs = 2L), .sw(70, list(255, "Profile morphology")), .sali_morph_hdr(84),
               .sw(112, list(12, "1"), list(28, "C"), list(57, "0"), list(85, "2")))
  x <- .sali_pdf_parse(.sali_pdf_words(c(pg, list(pg3))))
  expect_identical(x$obs, c(1L, 2L))
  expect_identical(nrow(x$table), 3L)
  expect_match(x$problems, "observation 1 read")
  bare <- list(rbind(.sali_hdr(1), .sw(70, list(250, "Site characteristics"))))
  y <- .sali_pdf_parse(.sali_pdf_words(bare))
  expect_null(y$table)
  expect_null(.sali_build_pedon(y, list(), .sali_layer_row(), .sali_asc_dict(load_rules("asc3"))))
})

test_that("the rows become horizons read by the NatSoil conventions", {
  skip_on_cran()
  pw <- .sali_pdf_words(.sali_pages())
  dict <- .sali_asc_dict(load_rules("asc3"))
  js <- list(.smp(1, 0, 0.1, list(.res("4A1", "6.6"), .res("2Z2_Clay", "40"))))
  p <- .sali_build_pedon(.sali_pdf_parse(pw), js, .sali_layer_row(), dict)
  h <- as.data.frame(p$horizons)
  expect_identical(h$designation, c("A1", "B21", "B22"))
  expect_identical(h$top_cm, c(0, 10, 50)); expect_identical(h$bottom_cm, c(10, 50, 120))
  expect_identical(h$field_texture, c("LMC", "MHC", "SC"))               # sandy clay: grade not given
  expect_identical(h$structure_grade, c("strong", "moderate", "moderate"))
  expect_identical(h$structure_type, c("granular", "lenticular", NA))     # "blocky" alone: no type
  expect_identical(h$ped_size_max_mm[2], 50)
  expect_identical(h$munsell_hue_moist, c("10YR", "10YR", "N"))           # no moisture given: moist
  expect_identical(h$munsell_chroma_moist[3], 0)
  expect_identical(h$coarse_fragments_pct[1], 0)                         # recorded absent
  expect_true(is.na(h$coarse_fragments_pct[2]))                           # empty cell: not recorded
  expect_identical(h$redoximorphic_features_pct[2], 6)
  expect_identical(h$mottle_contrast[2], "distinct")
  expect_true(is.na(h$redoximorphic_features_pct[3]))                     # biological mixing: not mottles
  expect_identical(h$carbonate_soft_pct[2], round((2 + 10) / 2, 3))
  expect_identical(h$strength_moist[1], "weak"); expect_identical(h$strength_dry[2], "firm")
  expect_true(is.na(h$strength_dry[3]) && is.na(h$strength_moist[3]))    # no moisture: not read
  expect_identical(h$boundary_distinctness[1:2], c("clear", "gradual"))
  expect_identical(h$effervescence_class[3], "moderate")
  expect_equal(h$ph_h2o, c(6.6, 8.5, NA))                                # lab pH, else the field pH
  expect_equal(h$clay_pct[1], 40)
  s <- p$site
  expect_identical(s$id, "SALI_TST_1")
  expect_identical(c(s$reference_asc_order, s$reference_asc_suborder, s$reference_asc_great_group,
                     s$reference_asc_subgroup), c("VE", "AE", "EI", "GB"))
  expect_identical(s$reference_asc_confidence, "1")
  expect_identical(s$sali_asc_edition, "3rd (2021)")
  expect_identical(s$drainage_class, "poorly drained")
  expect_identical(s$landform_element, "intertidal flat")                 # NatSoil's spelling
  expect_identical(s$surface_gravel_pct, 6)
  expect_identical(s$licence, "https://creativecommons.org/licenses/by/4.0/")
  expect_match(s$citation, "State of Queensland")
  expect_identical(s$sali_lab_source, "api")
  pv <- p$provenance
  expect_match(pv$notes[pv$horizon_idx == 2 & pv$attribute == "ph_h2o"], "field test: pH by Raupach")
  expect_match(pv$notes[pv$horizon_idx == 1 & pv$attribute == "clay_pct"], "soil chemistry API.*2Z2_C")
  expect_true(all(pv$source %in% valid_provenance_sources()))
})

test_that("MIR predictions fill only what nothing measured gives, marked as predictions", {
  skip_on_cran()
  pw <- .sali_pdf_words(.sali_pages())
  dict <- .sali_asc_dict(load_rules("asc3"))
  js <- list(
    .smp(1, 0, 0.1, list(.res("15C1_CEC", "20"), .res("15C1_CEC_MC2", "35"), .res("15C1_Ca_MC2", "9"),
                         .res("15C1_Ca_MQ01", "11"), .res("15C1_Na", "<0.2"), .res("15C1_K", "<0.2"),
                         .res("15C1_Mg", "4", valid = "N"))),
    .smp(2, 0.2, 0.4, list(.res("2Z2_Clay_MC91", "55"), .res("4A1_MC2", "7.9"))),
    .smp(1, 0, 0.1, list(.res("15C1_CEC", "99")), obs = 2L))              # another observation
  p <- .sali_build_pedon(.sali_pdf_parse(pw), js, .sali_layer_row(), dict)
  h <- as.data.frame(p$horizons); pv <- as.data.frame(p$provenance)
  src <- function(k, a) pv$source[pv$horizon_idx == k & pv$attribute == a]
  expect_equal(h$cec_cmol[1], 20)                                         # measured wins over MIR 35
  expect_identical(src(1, "cec_cmol"), "measured")
  expect_equal(h$ca_cmol[1], 10)                                          # two MIR calibrations, averaged
  expect_identical(src(1, "ca_cmol"), "predicted_spectra")
  expect_match(pv$notes[pv$horizon_idx == 1 & pv$attribute == "ca_cmol"], "15C1_Ca_MC2, 15C1_Ca_MQ01")
  expect_match(pv$notes[pv$horizon_idx == 1 & pv$attribute == "ca_cmol"], "^predicted: MIR prediction")
  expect_equal(h$k_cmol[1], 0.1)                                          # "<0.2": half the limit
  expect_true(is.na(h$na_cmol[1]))                                        # a limit above 0.18: unknown
  expect_true(is.na(h$mg_cmol[1]))                                        # flagged invalid
  expect_equal(h$clay_pct[2], 55); expect_identical(src(2, "clay_pct"), "predicted_spectra")
  expect_equal(h$ph_h2o[2], 8.5)                                          # the field pH is a measurement
  expect_identical(src(2, "ph_h2o"), "measured")
  expect_identical(p$site$sali_n_predicted, 2L)
  expect_identical(p$site$sali_n_lab_samples, 2L)
  expect_identical(compute_evidence_grade(p), "B")
})

test_that("the report's lab table is read only when the API holds nothing", {
  skip_on_cran()
  pw <- .sali_pdf_words(.sali_pages())
  dict <- .sali_asc_dict(load_rules("asc3"))
  lab_pdf <- .sali_pdf_lab(pw)
  p <- .sali_build_pedon(.sali_pdf_parse(pw), list(), .sali_layer_row(), dict, lab_pdf = lab_pdf)
  h <- as.data.frame(p$horizons); pv <- as.data.frame(p$provenance)
  expect_identical(p$site$sali_lab_source, "report table")
  expect_equal(h$ph_h2o[1:2], c(6.5, 8.0)); expect_equal(h$clay_pct[1:2], c(12, 45)); expect_equal(h$cec_cmol[2], 30)
  expect_match(pv$notes[pv$horizon_idx == 2 & pv$attribute == "cec_cmol"], "Laboratory test results table, method 15C1_CEC")
  expect_equal(h$na_cmol[1], 0.4)                                         # MIR in the table, nothing measured
  expect_match(pv$notes[pv$horizon_idx == 1 & pv$attribute == "na_cmol"], "15C1_Na_MC2.*Laboratory test results")
  # the API holds a sample: the table is not read
  js <- list(.smp(1, 0, 0.1, list(.res("4A1", "6.6"))))
  q <- .sali_build_pedon(.sali_pdf_parse(pw), js, .sali_layer_row(), dict, lab_pdf = lab_pdf)
  hq <- as.data.frame(q$horizons)
  expect_identical(q$site$sali_lab_source, "api")
  expect_true(all(is.na(hq$clay_pct)))
  expect_equal(hq$ph_h2o[1], 6.6)
  # a site with no samples at all
  r <- .sali_build_pedon(.sali_pdf_parse(pw), list(), .sali_layer_row(), dict)
  expect_identical(r$site$sali_lab_source, "none")
})

test_that("labels are read by position and matched by name to the 3rd edition", {
  skip_on_cran()
  d <- .sali_asc_dict(load_rules("asc3"))
  lab <- function(x) { z <- .sali_parse_label(x, d); c(z$order, z$suborder, z$great_group, z$subgroup) }
  expect_identical(lab("Vertosol"), c("VE", NA, NA, NA))
  expect_identical(lab("Vertosol Black"), c("VE", "AE", NA, NA))
  expect_identical(lab("VERTOSOL black Self-Mulching Epicalcareous-Endohypersodic"), c("VE", "AE", "EI", "GB"))
  expect_identical(lab("Dermosol Brown No Available Class"), c("DE", "AB", "ZZ", NA))
  expect_identical(lab("Dermosol Brown Class Undetermined"), c("DE", "AB", "YY", NA))
  z <- .sali_parse_label("Tenosol Purple", d)
  expect_identical(c(z$order, z$suborder), c("TE", NA))                   # a name not in the 3rd edition
  expect_identical(z$unmapped, "suborder:Purple")
  expect_true(is.na(.sali_parse_label("Unclassified", d)$order))
  expect_identical(.sali_order_code(c("Kandosol Red", "Unclassified")), c("KA", NA))
  expect_identical(.sali_confidence("Analytical data are incomplete but reasonable confidence"), "2")
  expect_identical(.sali_confidence("Other classification used with rapid check of key attributes"), "C")
  expect_true(is.na(.sali_confidence("")))
})

test_that("texture and the other Field Handbook readings", {
  skip_on_cran()
  expect_identical(unname(.sali_texture("Loam, fine sandy")), c("L", NA))
  expect_identical(unname(.sali_texture("Silty clay")), c("ZC", NA))
  expect_identical(unname(.sali_texture("Heavy sandy loam")), c("SL", "+"))
  expect_identical(unname(.sali_texture("Light clay")), c("LC", NA))
  expect_identical(unname(.sali_texture("Sandy light medium clay")), c("SLMC", NA))
  expect_true(is.na(.sali_texture("Not recorded")[["code"]]))
  expect_true(is.na(.sali_rec("Not recorded (historical)")))
  expect_identical(.sali_lab_code(c("6B2b", "2Z2_Clay", "15C1_Ca")), c("6B2", "2Z2_C", "15C1_CA"))
  expect_identical(.sali_mottles("Few (2-10%) jarosite mottles")[[1]]$mott_type, "J")
  expect_identical(.sali_mottles("No mottles")[[1]]$mott_abun, "0")
  expect_identical(.sali_segregations("No segregations")[[1]]$seg_abun, "0")
  expect_identical(length(.sali_structures("Strong 5-10 mm angular blocky; Weak platy")), 2L)
  expect_identical(.sali_landform("Supra-tidal flat"), "supratidal flat")
})

test_that("read_sali_pedons() reads the folder layout and leaves out what it cannot read", {
  skip_on_cran()
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("pdftools")
  dir <- withr::local_tempdir()
  for (d in c("layer", "pdf", "lab")) dir.create(file.path(dir, d))
  rec <- function(pc, id, lab) list(attributes = list(objectid = id, project_code = pc, site_id = id,
                                                      australian_soil_classification = lab, latitude = -20,
                                                      longitude = 145))
  layer <- list(features = list(rec("TST", 1, "Vertosol Black"), rec("TST", 2, "Unclassified"),
                                rec("TST", 3, "Dermosol Red"), rec("TST", 4, "Kandosol Red")))
  writeLines(jsonlite::toJSON(layer, auto_unbox = TRUE), file.path(dir, "layer", "layer101_off000000.json"))
  for (s in 1:4) writeLines("%PDF made up", file.path(dir, "pdf", sprintf("TST_%d.pdf", s)))
  file.remove(file.path(dir, "pdf", "TST_3.pdf")); writeLines("over", file.path(dir, "pdf", "TST_3.pdf.oversize"))
  for (s in 1:3) writeLines(jsonlite::toJSON(list(.smp(1, 0, 0.1, list(.res("4A1", "6.6")))), auto_unbox = TRUE),
                            file.path(dir, "lab", sprintf("TST_%d_samples.json", s)))
  pages <- .sali_pages()
  testthat::local_mocked_bindings(pdf_data = function(pdf, ...) pages, .package = "pdftools")
  peds <- read_sali_pedons(dir, verbose = FALSE)
  expect_named(peds, "SALI_TST_1")
  st <- attr(peds, "sali_status")
  expect_identical(st$status[match(c("TST_1", "TST_2", "TST_3", "TST_4"), st$stem)],
                   c("read", "label names no ASC order", "report over the download cap", "no laboratory file"))
  expect_identical(peds[[1]]$site$reference_asc_suborder, "AE")
  expect_equal(as.data.frame(peds[[1]]$horizons)$ph_h2o[1], 6.6)
  expect_length(read_sali_pedons(dir, sites = "TST_4", verbose = FALSE), 0L)
})

test_that("fetch_sali() caps the reports, marks the oversize ones and resumes", {
  skip_on_cran()
  skip_if_not_installed("httr")
  skip_if_not_installed("jsonlite")
  expect_identical(formals(fetch_sali)$cap_bytes, 150e3)
  expect_true(.sali_host_ok("https://resources.information.qld.gov.au/soils/reports/sites?project=A&site=1"))
  expect_true(.sali_host_ok("https://soil-chem.information.qld.gov.au/odata/Samples"))
  expect_false(.sali_host_ok("https://example.org/soils/reports/sites"))
  expect_error(.sali_guard(401L, "text/html", "", "u"), class = "sali_stop")
  expect_error(.sali_guard(200L, "text/html", "<p>Please sign in</p>", "u"), class = "sali_stop")
  expect_error(.sali_guard(200L, "text/html", "Request Rejected", "u"), class = "sali_stop")
  expect_true(.sali_guard(200L, "application/pdf", "", "u"))
  dir <- withr::local_tempdir()
  layer <- list(features = list(list(attributes = list(objectid = 1, project_code = "TST", site_id = 1,
                                                        australian_soil_classification = "Vertosol Black")),
                                list(attributes = list(objectid = 2, project_code = "TST", site_id = 2,
                                                        australian_soil_classification = "Kandosol Red")),
                                list(attributes = list(objectid = 3, project_code = "TST", site_id = 3,
                                                        australian_soil_classification = "Unclassified"))))
  calls <- new.env(); calls$pdf <- character(0); calls$lab <- 0L
  testthat::local_mocked_bindings(
    .sali_get = function(url, query = NULL, ...) {
      if (isTRUE(query$returnCountOnly == "true")) return('{"count": 3}')
      if (is.null(query$resultOffset) && is.null(query$`$filter`)) return('{"maxRecordCount": 2000}')
      if (!is.null(query$resultOffset)) return(as.character(jsonlite::toJSON(layer, auto_unbox = TRUE)))
      calls$lab <- calls$lab + 1L
      "[]"
    },
    .sali_get_capped = function(url, cap, ...) {
      calls$pdf <- c(calls$pdf, url)
      expect_identical(cap, 150e3)
      if (grepl("site=2", url, fixed = TRUE)) list(status = "oversize", bytes = cap + 1, data = NULL)
      else list(status = "ok", bytes = 4, data = charToRaw("%PDF"))
    })
  s <- fetch_sali(dir, pause = 0, verbose = FALSE)
  expect_identical(s$stem, c("TST_1", "TST_2"))                          # Unclassified not fetched
  expect_identical(s$pdf, c("ok", "oversize")); expect_identical(s$lab, c("ok", "ok"))
  expect_true(file.exists(file.path(dir, "pdf", "TST_1.pdf")))
  expect_true(file.exists(file.path(dir, "pdf", "TST_2.pdf.oversize")))
  expect_identical(calls$pdf, c("https://resources.information.qld.gov.au/soils/reports/sites?project=TST&site=1",
                                "https://resources.information.qld.gov.au/soils/reports/sites?project=TST&site=2"))
  fetch_sali(dir, pause = 0, verbose = FALSE)                             # resumed: nothing fetched again
  expect_identical(length(calls$pdf), 2L); expect_identical(calls$lab, 2L)
})

test_that("benchmark_sali() gives benchmark_natsoil()'s figures and a table by order", {
  skip_on_cran()
  pw <- .sali_pdf_words(.sali_pages())
  dict <- .sali_asc_dict(load_rules("asc3"))
  js <- list(.smp(1, 0, 0.1, list(.res("4A1", "6.6"))))
  peds <- list()
  for (lb in c("Vertosol Black", "Dermosol Brown", "Vertosol Grey")) {
    p <- .sali_build_pedon(.sali_pdf_parse(pw), js, .sali_layer_row(lb), dict)
    p$site$id <- paste("SALI", lb); peds[[p$site$id]] <- p
  }
  b <- benchmark_sali(peds, verbose = FALSE)
  n <- benchmark_natsoil(peds, verbose = FALSE)
  expect_identical(b$n, 3L); expect_identical(b$accuracy, n$accuracy); expect_identical(b$results, n$results)
  expect_identical(b$by_order$order, c("VE", "DE"))
  expect_identical(b$by_order$n, c(2L, 1L))
  expect_identical(b$by_edition$edition, "3rd (2021)")
  expect_identical(sum(b$by_order$n), b$n)
})

test_that("sali_citation() gives the CC BY 4.0 attribution", {
  skip_on_cran()
  x <- sali_citation(accessed = "2026-10-30")
  expect_match(x, "State of Queensland \\(Department of Environment, Tourism, Science and Innovation\\), 2026")
  expect_match(x, "CC BY 4.0")
  expect_match(x, "downloaded on 2026-10-30")
})
