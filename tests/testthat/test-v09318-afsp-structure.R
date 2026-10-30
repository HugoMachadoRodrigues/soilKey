# v0.9.312: the AfSP reader reads the structure codes of its dictionary
# (AfSP012Qry_DictioClassValues). Made-up rows in the AfSP shape; no AfSP data
# is stored in the package.

.v09318_layers <- function(type = c("G", "M"), grade = c("M", "N"), size = c("F", NA)) {
  n <- length(type)
  data.frame(
    UpDpth = c(0, 25)[seq_len(n)], LowDpth = c(25, 60)[seq_len(n)],
    HorDes = c("Ah", "AB")[seq_len(n)], ColorM = "10YR2/1", ColorD = "10YR3/2",
    StrType = type, StrGrade = grade, StrSize = ifelse(is.na(size), "-9999", size),
    Clay = 20, Silt = 30, Sand = 50, CfPc = 0, PHH2O = 5.0, PHKCl = 4.2,
    PHCaCl2 = -9999, EC = -9999, OrgC = 25, TotalN = 2, CecSoil = 15, Ecec = -9999,
    Bsat = 30, ExCa = 3, ExMg = 1, ExK = 0.3, ExNa = 0.1, ExAl = -9999,
    CaCO3 = -9999, CaSO4 = -9999, BlkDens = 1.2, stringsAsFactors = FALSE)
}
.v09318_profile <- function() {
  data.frame(ProfileID = "made-up-1", Country = "ZZ", WRB06rg = "UM",
             WRB06 = "Haplic Umbrisol", FAO88 = NA, USDA = NA,
             stringsAsFactors = FALSE)
}

test_that("the structure codes are read by the AfSP dictionary", {
  skip_on_cran()
  hz <- data.frame(structure_type  = c("S", "M", "N", "BL", "massive", NA),
                   structure_grade = c("M", "WM", "N", "V", "weak", NA),
                   structure_size  = c("F", "FM", "S", "CV", "fine", NA),
                   stringsAsFactors = FALSE)
  d <- soilKey:::.afsp_decode_structure(hz)
  expect_identical(d$horizons$structure_type,
                   c("subangular blocky", "massive", "single grain", "blocky", "massive", NA))
  # a range of grades by its lower end; V is not in the dictionary
  expect_identical(d$horizons$structure_grade,
                   c("moderate", "weak", "structureless", NA, "weak", NA))
  # a range of sizes keeps both ends; S is not in the dictionary
  expect_identical(d$horizons$structure_size,
                   c("fine", "fine and medium", NA, "coarse and very coarse", "fine", NA))
  expect_identical(soilKey:::.wrb_structure_size_class(d$horizons$structure_size)[2], 3L)
  # every code goes to the provenance, the undefined ones as not recorded
  expect_equal(nrow(d$provenance), 12L)
  expect_true(all(grepl("^AfSP Str(Type|Grade|Size) code ", d$provenance$notes)))
  undefined <- d$provenance[grepl("not in the AfSP dictionary", d$provenance$notes), ]
  expect_setequal(paste(undefined$attribute, undefined$horizon_idx),
                  c("structure_grade 4", "structure_size 3"))
  # words are left as they are, so a second pass changes nothing
  expect_identical(soilKey:::.afsp_decode_structure(d$horizons)$horizons, d$horizons)
})

test_that("a massive AfSP layer fails the mollic structure criterion", {
  skip_on_cran()
  p <- soilKey:::.afsp_to_pedon(.v09318_profile(), .v09318_layers())
  h <- p$horizons
  expect_identical(h$structure_type, c("granular", "massive"))
  expect_identical(h$structure_grade, c("moderate", "structureless"))
  expect_true(any(grepl("AfSP StrType code M", p$provenance$notes)))
  expect_identical(soilKey:::.wrb_aggregate_structure_le_10cm(h), c(TRUE, FALSE))
  # all massive: criterion 1 fails; until v0.9.311 the code M read as not
  # recorded and, by the reading of v0.9.311, did not block it
  m <- soilKey:::.afsp_to_pedon(.v09318_profile(),
                                .v09318_layers(type = c("M", "M"), grade = c("N", "N"), size = c(NA, NA)))
  expect_false(soilKey:::.wrb_mollic_structure(m$horizons, 1:2)$passed)
  raw <- m$horizons
  raw$structure_type <- c("M", "M"); raw$structure_grade <- c("N", "N")
  expect_true(soilKey:::.wrb_mollic_structure(raw, 1:2)$passed)
})

test_that("profiles built before v0.9.312 are read when benchmarked", {
  skip_on_cran()
  p <- soilKey:::.afsp_to_pedon(.v09318_profile(), .v09318_layers())
  h <- p$horizons
  h$structure_type <- c("G", "M"); h$structure_grade <- c("M", "N"); h$structure_size <- c("F", NA)
  old <- PedonRecord$new(site = p$site, horizons = h)
  new <- soilKey:::.afsp_as_pedons(list(pedons = list(old)))[[1]]
  expect_identical(new$horizons$structure_type, c("granular", "massive"))
  expect_identical(new$horizons$structure_size, c("fine", NA))
  # a profile with no code is returned as it is
  expect_identical(soilKey:::.afsp_decode_pedon(new), new)
})
