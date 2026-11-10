# v0.9.399: the AfSP loader reads the site fields Drain (drainage_class),
# ParMat (parent_material, with the words of its ancestors) and LndForm
# (landform, only the forms that say something of the origin), by the words
# of the AfSP dictionary; and the clay-rock words of KA5 and the ejecta words
# of SCSJ and NZSC no longer read limestone or a pyroclastic breccia.
# Synthetic rows and a synthetic dictionary, no AfSP data.

.afsp_rows <- function() {
  L <- data.frame(ProfileID = "X1", UpDpth = c(30, 0), LowDpth = c(80, 30), HorDes = c("B", "A"),
                  ColorM = c("10YR5/6", "10YR3/2"), ColorD = NA, StrGrade = c("W", "S"), StrSize = NA,
                  StrType = c("S", "A"), Clay = c(30, 40), stringsAsFactors = FALSE)
  for (f in c("Silt", "Sand", "CfPc", "PHH2O", "PHKCl", "PHCaCl2", "EC", "OrgC", "TotalN", "CecSoil", "Ecec",
              "Bsat", "ExCa", "ExMg", "ExK", "ExNa", "ExAl", "CaCO3", "CaSO4", "BlkDens")) L[[f]] <- -9999
  P <- data.frame(ProfileID = "X1", Country = "ZA", WRB06rg = "VR", WRB06 = "", FAO88 = "", USDA = "",
                  Drain = "I", ParMat = "SO1", LndForm = "LV", stringsAsFactors = FALSE)
  D <- data.frame(ProprtyCod = c("ParMat", "ParMat", "ParMat", "LndForm", "LndForm"),
                  ValueCode = c("S", "SO", "SO1", "LV", "LP"),
                  ValueDescr = c("S  sedimentary rocks (consolidated)", "SO calcareous rocks",
                                 "SO1 limestone, chalk, dolomite and other carbonate rocks", "valley floor", "plain"),
                  stringsAsFactors = FALSE)
  list(L = L, P = P, D = D)
}

test_that("the AfSP loader reads Drain, ParMat and LndForm", {
  skip_on_cran()
  r <- .afsp_rows()
  p <- soilKey:::.afsp_to_pedon(r$P, r$L, dict = r$D)
  expect_identical(p$site$drainage_class, "imperfectly drained")
  expect_identical(p$site$parent_material,
                   "sedimentary rocks (consolidated), calcareous rocks, limestone, chalk, dolomite and other carbonate rocks")
  expect_identical(p$site$landform, "valley floor")
  # a general relief class is not read
  P3 <- r$P; P3$LndForm <- "LP"
  expect_true(is.na(soilKey:::.afsp_to_pedon(P3, r$L, dict = r$D)$site$landform))
  # every drainage code to a drainage_class word
  expect_identical(unname(soilKey:::.AFSP_DRAINAGE[c("E", "S", "W", "M", "I", "P", "V")]),
                   c("excessively drained", "somewhat excessively drained", "well drained",
                     "moderately well drained", "imperfectly drained", "poorly drained", "very poorly drained"))
  # no dictionary: ParMat and LndForm not read; an unknown or missing code: not read
  p <- soilKey:::.afsp_to_pedon(r$P, r$L)
  expect_true(is.na(p$site$parent_material)); expect_true(is.na(p$site$landform))
  P2 <- r$P; P2$Drain <- "X"; P2$ParMat <- "-9999"
  p <- soilKey:::.afsp_to_pedon(P2, r$L, dict = r$D)
  expect_true(is.na(p$site$drainage_class)); expect_true(is.na(p$site$parent_material))
  p <- soilKey:::.afsp_to_pedon(r$P[, 1:6], r$L, dict = r$D)
  expect_true(is.na(p$site$drainage_class)); expect_true(is.na(p$site$landform))
})

test_that("ParMat: ancestors composed, the unconsolidated deposits decoded by the pattern", {
  skip_on_cran()
  D <- data.frame(ProprtyCod = "ParMat",
                  ValueCode = c("V", "VA", "VA1", "M", "MA", "MA1", "U", "U0", "U000", "U00CF", "UY", "UY2F", "UY1C", "UT", "UQ0M", "UA", "UAN"),
                  ValueDescr = c("V  volcanic rocks", "VA  acid", "VA1 rhyolite", "M  metamorphic rocks", "MA acid metamorphic rocks",
                                 "MA1 quartzite", "U unconsolidated deposits", "U0 group unknown", "U000 subgroup and type unknown",
                                 "U00CF subgroup unknown collo-fluvial", "UY clayey", "UY2F fluvial, calcareous",
                                 "UY1C colluvial, not calcareous", "UT silty, loamy", "UQ0M marine and estuarine, unspecified",
                                 "UA anthropogenic/ technogenic", "UAN redeposited natural materials"),
                  stringsAsFactors = FALSE)
  w <- function(k) soilKey:::.afsp_parmat_words(D, k)
  expect_identical(w("VA"), "volcanic rocks, acid")
  expect_identical(w("VA1"), "volcanic rocks, acid, rhyolite")
  expect_identical(w("MA1"), "acid metamorphic rocks, quartzite")
  expect_identical(w("UY2F"), "clayey, fluvial, calcareous")
  expect_identical(w("U00CF"), "collo-fluvial")
  expect_identical(w("UAN"), "anthropogenic/ technogenic, redeposited natural materials")
  # codes the dictionary does not list, by its pattern
  expect_identical(w("U00C"), "colluvial")
  expect_identical(w("U00M"), "marine and estuarine")
  expect_identical(w("U00FC"), "fluvial and colluvial")   # F and C given apart, read one by one
  expect_true(is.na(w("U00GX")))                           # G not given in this dictionary
  expect_identical(w("UT00"), "silty, loamy")
  expect_identical(w("UY2"), "clayey, calcareous")
  expect_identical(w("Ux0C"), "colluvial")
  # nothing to read
  expect_true(is.na(w("U000"))); expect_true(is.na(w("A"))); expect_true(is.na(w("UQUC"))); expect_true(is.na(w(NA)))
})

test_that("KA5 reads clay stone by whole words; SCSJ and NZSC read no pyroclastic breccia as tephra", {
  skip_on_cran()
  re <- soilKey:::.KA5_CLAY_ROCK_RE
  yes <- c("Ton", "Tonstein", "Tonschiefer", "Geschiebemergel", "claystone", "clayey, fluvial", "shale", "pelite or lutite", "marl")
  no <- c("limestone", "sandstone", "siltstone", "ironstone", "tonalite", "limestone, chalk, dolomite and other carbonate rocks")
  for (t in yes) expect_true(grepl(re, t, ignore.case = TRUE), info = t)
  for (t in no) expect_false(grepl(re, t, ignore.case = TRUE), info = t)
  expect_true(grepl(soilKey:::.SCSJ_NOT_EJECTA_RE, "volcanic rocks, pyroclastic rocks (tephra), pyroclastic-breccia", perl = TRUE))
  expect_false(grepl(soilKey:::.SCSJ_NOT_EJECTA_RE, "volcanic rocks, pyroclastic rocks (tephra), volcanic ash", perl = TRUE))
  pd <- function(pm) {
    old <- options(soilKey.warn_unrecognised = FALSE); on.exit(options(old))
    PedonRecord$new(site = list(id = "t", parent_material = pm),
                    horizons = data.frame(top_cm = c(0, 20), bottom_cm = c(20, 60), designation = c("A", "Bw"),
                                          clay_pct = c(20, 20), stringsAsFactors = FALSE))
  }
  tr <- function(pm) { p <- pd(pm); soilKey:::.nzsc_tephric_rows(p, soilKey:::.nzsc_horizons(p)) }
  expect_true(all(tr("volcanic rocks, pyroclastic rocks (tephra), volcanic ash") %in% TRUE))
  expect_false(any(tr("volcanic rocks, pyroclastic rocks (tephra), pyroclastic-breccia") %in% TRUE))
})
