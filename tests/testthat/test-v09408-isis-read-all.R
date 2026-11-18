# v0.9.408 -- questions from ISRIC on the ISIS reader (Hugo, 2026-11-18): the
# whole collection, codes with a small letter, the alternative fields of a
# layer and the edition of ISRIC's name. Every profile below is synthetic.

.p408 <- function(code = "XX001", layers = NULL, wrb = NULL) {
  it <- function(x) list(item = x, description = NULL)
  layers <- layers %||% list(
    list(layerNumber = 1L, upperDepth = 0, lowerDepth = 20, layerName = "A",
         clay = "20", colourMoistHue = it("10YR"), colourMoistValue = it("3"),
         colourMoistChroma = it("2")),
    list(layerNumber = 2L, upperDepth = 20, lowerDepth = 60, layerName = "Bt",
         clay = "35", colourMoistHue = it("5YR"), colourMoistValue = it("4"),
         colourMoistChroma = it("6")))
  list(profileCode = code, countryCode = NULL, layers = layers,
       wrbClassifications = wrb %||% list(
         list(publicationYear = 2006L, referenceSoilGroup = "Luvisol", qualifier = list()),
         list(publicationYear = 2022L, referenceSoilGroup = "Lixisol", qualifier = list())))
}
.it408 <- function(x) list(item = x, description = NULL)

test_that("read_isis(all = TRUE) reads every profile and names those with no horizons", {
  skip_on_cran()
  pool <- c(lapply(sprintf("XX%03d", 1:6), .p408), list(.p408("BT001a"), .p408("ZZ001", layers = list())))
  calls <- list()
  fake <- function(query, variables = NULL, timeout = 60) {
    calls[[length(calls) + 1L]] <<- list(query = query, variables = variables)
    off <- as.integer(sub(".*offset: ([0-9]+).*", "\\1", query))
    first <- as.integer(sub(".*first: ([0-9]+), offset.*", "\\1", query))
    list(data = list(profiles = pool[seq_along(pool) > off & seq_along(pool) <= off + first]), error = NULL)
  }
  local_mocked_bindings(.isis_post = fake)
  expect_warning(out <- read_isis(all = TRUE), "1 of the 8 ISIS profiles read have no horizons to classify: ZZ001")
  expect_length(out, 8L)
  expect_true(all(c("BT001a", "ZZ001") %in% names(out)))
  expect_null(calls[[1]]$variables)
  expect_false(grepl("filter", calls[[1]]$query))
  expect_error(read_isis("XX001", all = TRUE), "whole collection")
  expect_error(read_isis(country = "BR", all = TRUE), "whole collection")
})

test_that("codes keep their small letter and a short max_profiles is reported", {
  skip_on_cran()
  expect_identical(soilKey:::.isis_normalise_code(c("bt001a", " BT001A ", "ke009", "ID049a")),
                   c("BT001a", "BT001a", "KE009", "ID049a"))
  fake <- function(query, variables = NULL, timeout = 60) {
    off <- as.integer(sub(".*offset: ([0-9]+).*", "\\1", query))
    first <- as.integer(sub(".*first: ([0-9]+), offset.*", "\\1", query))
    codes <- unlist(variables$codes)
    list(data = list(profiles = lapply(codes[seq_along(codes) > off & seq_along(codes) <= off + first], .p408)), error = NULL)
  }
  local_mocked_bindings(.isis_post = fake)
  expect_named(read_isis("bt001a"), "BT001a")
  expect_warning(out <- read_isis(sprintf("XX%03d", 1:4), max_profiles = 2L), "first 2 of 4 profile codes")
  expect_length(out, 2L)
})

test_that("a colour written whole stands in for an empty hue, value and chroma", {
  skip_on_cran()
  expect_identical(soilKey:::.isis_parse_matrix("7.5YR 3/2"), list(hue = "7.5YR", value = 3, chroma = 2))
  expect_identical(soilKey:::.isis_parse_matrix("N 5/"), list(hue = "N", value = 5, chroma = NA_real_))
  expect_true(is.na(soilKey:::.isis_parse_matrix("N4/N5")$hue))
  L <- list(
    list(upperDepth = 0, lowerDepth = 20, layerName = "A", colourMoistMatrix = .it408("7.5YR 3/2"),
         colourDryMatrix = .it408("N4/N5"), colourDryMatrix2 = .it408("10YR 5/3")),
    list(upperDepth = 20, lowerDepth = 40, layerName = "B", colourMoistHue = .it408("5YR"),
         colourMoistValue = .it408("4"), colourMoistChroma = .it408("6"),
         colourMoistMatrix = .it408("10YR 8/1")))
  p <- isis_profile_to_pedon(.p408(layers = L))
  h <- p$horizons
  expect_identical(h$munsell_hue_moist, c("7.5YR", "5YR"))        # the first field wins
  expect_identical(h$munsell_value_moist, c(3, 4))
  expect_identical(h$munsell_chroma_moist, c(2, 6))
  expect_identical(h$munsell_hue_dry[1], "10YR")                  # the second entry, the first unreadable
  pv <- p$provenance[p$provenance$attribute == "munsell_hue_moist" & p$provenance$horizon_idx == 1L, ]
  expect_match(pv$notes, "colourMoistMatrix")
  # a layer with only the colour written whole is a described layer
  expect_true(soilKey:::.isis_described(list(colourDryMatrix = .it408("10YR 5/3"))))
})

test_that("the second structure entry stands in where the first is empty", {
  skip_on_cran()
  L <- list(
    list(upperDepth = 0, lowerDepth = 20, layerName = "A", structureGrade2 = .it408("MO"),
         structureType2 = .it408("SB"), structureSize2 = .it408("FI")),
    list(upperDepth = 20, lowerDepth = 40, layerName = "B", structureGrade1 = .it408("ST"),
         structureType = .it408("AB"), structureSize = .it408("ME"), structureGrade2 = .it408("WE"),
         structureType2 = .it408("SB"), structureSize2 = .it408("FI")))
  h <- isis_profile_to_pedon(.p408(layers = L))$horizons
  expect_identical(h$structure_type, c("subangular blocky", "angular blocky"))
  expect_identical(h$structure_type_secondary, c(NA_character_, "subangular blocky"))
  expect_identical(h$structure_grade[1], unname(soilKey:::.ISIS_GRADE[["MO"]]))
  expect_identical(h$structure_grade[2], unname(soilKey:::.ISIS_GRADE[["ST"]]))
  expect_identical(h$structure_size[1], unname(soilKey:::.ISIS_SIZE[["FI"]]))
})

test_that("coatings of clay are clay films only where no cutans are recorded", {
  skip_on_cran()
  mk <- function(nature, abundance = NULL, contrast = NULL, extra = list())
    c(list(upperDepth = 0, lowerDepth = 20, layerName = "Bt", coatingsNature = .it408(nature)),
      if (!is.null(abundance)) list(coatingsAbundance = .it408(abundance)),
      if (!is.null(contrast)) list(coatingsContrast = .it408(contrast)), extra)
  h <- function(L) isis_profile_to_pedon(.p408(layers = list(L)))$horizons
  x <- h(mk("C", "M", "P")); expect_identical(x$clay_films_amount, "many"); expect_identical(x$clay_films_strength, "prominent")
  x <- h(mk("CS", "V", "F")); expect_identical(x$clay_films_amount, "few"); expect_identical(x$clay_films_strength, "faint")
  expect_identical(h(mk("CH"))$clay_films_amount, "few")
  x <- h(mk("C", "N", "D")); expect_identical(x$clay_films_amount, "none"); expect_true(is.na(x$clay_films_strength))
  expect_true(is.na(h(mk("H", "M", "P"))$clay_films_amount))      # humus coatings are not clay films
  x <- h(mk("C", "M", "P", list(cutansKind = .it408("C"), cutansAbundance = .it408("P"), cutansThickness = .it408("f"))))
  expect_identical(x$clay_films_amount, "few")                    # the cutans win
  expect_identical(x$clay_films_strength, "faint")
})

test_that("the site gives ISRIC's name with its year and every edition", {
  skip_on_cran()
  q <- list(list(position = "principal", place = 1, qualifier = "Luvic"))
  w <- list(list(publicationYear = 2006L, referenceSoilGroup = "Phaeozem", qualifier = q),
            list(publicationYear = 2022L, referenceSoilGroup = "Luvisol", qualifier = list()),
            list(publicationYear = 1998L, referenceSoilGroup = "Cambisol", qualifier = list()))
  s <- isis_profile_to_pedon(.p408(wrb = w))$site
  expect_identical(s$reference_wrb_full, "Luvisol")               # the name stays the name
  expect_identical(s$reference_wrb_edition, "2022")
  expect_identical(s$reference_wrb_cited, "Luvisol; publication year: 2022")
  expect_identical(s$reference_wrb_all, "2022: Luvisol | 2006: Luvic Phaeozem | 1998: Cambisol")
})

test_that("benchmark_isis scores only names of the 2022 edition", {
  skip_on_cran()
  old <- list(list(publicationYear = 2006L, referenceSoilGroup = "Luvisol", qualifier = list()))
  b <- benchmark_isis(list(isis_profile_to_pedon(.p408("XX001")), isis_profile_to_pedon(.p408("XX002", wrb = old))))
  expect_identical(b$isric_wrb_edition, c("2022", "2006"))
  expect_true(is.na(b$wrb_agree[2]))
  expect_false(is.na(b$wrb_agree[1]))
})

test_that("the second entries of mottles and inclusions stand in for an empty first", {
  skip_on_cran()
  L <- list(list(upperDepth = 0, lowerDepth = 20, layerName = "Bv", mottlesContrast2 = .it408("P"),
                 mottlesSizeclass2 = .it408("M"), inclusionsComposition = .it408("F"),
                 mineralConcAbundance = .it408("C"), concretionsMineralHardness2 = .it408("H"),
                 inclusionsSize2 = .it408("C")),
            list(upperDepth = 20, lowerDepth = 40, layerName = "B", mottlesContrast = .it408("F"),
                 mottlesContrast2 = .it408("P"), mottlesSizeclass = .it408("F"), mottlesSizeclass2 = .it408("M")))
  p <- isis_profile_to_pedon(.p408(layers = L)); h <- p$horizons
  expect_identical(h$mottle_contrast, c("prominent", "faint"))
  expect_identical(h$mottle_size_class, c("medium", "fine"))
  expect_identical(h$ferric_nodules_pct[1], 10)
  expect_true(any(grepl("mottlesContrast2", p$provenance$notes)))
})

test_that("the organic matter is not read as organic carbon", {
  skip_on_cran()
  L <- list(list(upperDepth = 0, lowerDepth = 20, layerName = "A", orgm = list("59")))
  expect_true(is.na(isis_profile_to_pedon(.p408(layers = L))$horizons$oc_pct))
})

test_that("a failed request is tried again, and a rejected query is not", {
  skip_on_cran()
  n <- 0L
  local_mocked_bindings(.isis_post = function(query, variables = NULL, timeout = 60) {
    n <<- n + 1L
    if (n < 3L) list(data = NULL, error = "timeout") else list(data = list(profiles = list(.p408())), error = NULL)
  })
  expect_named(read_isis("XX001"), "XX001")
  expect_identical(n, 3L)
  n <- 0L
  local_mocked_bindings(.isis_post = function(query, variables = NULL, timeout = 60) {
    n <<- n + 1L; list(data = NULL, error = "ISRIC returned HTTP 400: bad query")
  })
  expect_warning(read_isis("XX001"), "HTTP 400")
  expect_identical(n, 1L)
  n <- 0L
  local_mocked_bindings(.isis_post = function(query, variables = NULL, timeout = 60) {
    n <<- n + 1L; list(data = NULL, error = "timeout")
  })
  expect_warning(read_isis("XX001"), "after 3 attempts")
  expect_identical(n, 3L)
})

test_that("benchmark_isis says what it scored, by edition", {
  skip_on_cran()
  old <- list(list(publicationYear = 2006L, referenceSoilGroup = "Luvisol", qualifier = list()))
  pd <- list(isis_profile_to_pedon(.p408("XX001")), isis_profile_to_pedon(.p408("XX002", wrb = old)))
  expect_message(b <- benchmark_isis(pd), "1 of 2 profiles scored.*2006 \\(1\\)")
  e <- attr(b, "wrb_editions")
  expect_identical(e$edition, c("2006", "2022"))
  expect_identical(e$profiles, c(1L, 1L))
  expect_identical(e$scored, c(FALSE, TRUE))
  expect_silent(benchmark_isis(pd, quiet = TRUE))
})
