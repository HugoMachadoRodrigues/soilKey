# v0.9.441: three follow-ups of the audit of the FAO-Unesco legends. (1) The
# ISIS reader keeps ISRIC's FAO-Unesco labels (site$reference_fao74 and
# site$reference_fao88), and the ISIS benchmark script takes the
# PedonRecords of read_isis(). (2) A result with no major soil group(ing) and
# candidates left undecided has evidence grade C at most (G1). (3) A test
# with alternative routes lists the attributes of its undecided routes only.
# Every pedon and ISIS record below is synthetic.

.f441 <- function(p, ed = "1988") classify_fao(p, edition = ed, on_missing = "silent")
.edit441 <- function(p, rows = NULL, ..., site = list()) {
  h <- as.data.frame(p$horizons)
  if (!is.null(rows)) h <- h[rows, , drop = FALSE]
  v <- list(...)
  for (k in names(v)) h[[k]] <- if (length(v[[k]]) == 1L) rep(v[[k]], nrow(h)) else v[[k]]
  PedonRecord$new(site = utils::modifyList(p$site, site),
                  horizons = ensure_horizon_schema(data.table::as.data.table(h)))
}
# the benchmark script: installed (R CMD check), else the source tree
.script441 <- function() {
  f <- system.file("benchmarks", "fao_isis.R", package = "soilKey")
  if (!nzchar(f)) f <- testthat::test_path("..", "..", "inst", "benchmarks", "fao_isis.R")
  skip_if_not(file.exists(f))
  e <- new.env()
  sys.source(f, envir = e)
  e
}
# A synthetic raw ISIS profile, as the GraphQL server returns one.
.raw441 <- function(code, labels, layers = NULL) {
  lay <- function(top, bot, nm, clay) list(upperDepth = top, lowerDepth = bot, layerName = nm,
                                           clay = list(as.character(clay)))
  list(profileCode = code, countryCode = "XX", profileId = 1L,
       layers = layers %||% list(lay(0L, 20L, "A", 20), lay(20L, 100L, "B", 30)),
       faoClassifications = lapply(labels, function(l)
         list(publicationYear = l[1], faoClassification = l[2])))
}

# ---- (1) the FAO-Unesco labels of ISIS -----------------------------------------

test_that("the ISIS query asks for the FAO-Unesco classifications", {
  skip_on_cran()
  expect_match(soilKey:::.isis_profile_fields(),
               "faoClassifications(first: 20) { publicationYear faoClassification }", fixed = TRUE)
})

test_that("isis_profile_to_pedon() keeps the raw labels of both editions", {
  skip_on_cran()
  p <- isis_profile_to_pedon(.raw441("XX001", list(c("1974", "Orthic Luvisol"),
                                                   c("1997", "Ferri-Haplic Luvisol"))))
  expect_identical(p$site$reference_fao74, "Orthic Luvisol")
  expect_identical(p$site$reference_fao88, "Ferri-Haplic Luvisol")
  # one edition only; the year of the revised label is 1997, as ISIS stores it
  p <- isis_profile_to_pedon(.raw441("XX002", list(c("1997", "Eutric Cambisol skeletic"))))
  expect_null(p$site$reference_fao74)
  expect_identical(p$site$reference_fao88, "Eutric Cambisol skeletic")
  # no label, an empty label or another year: nothing is set
  p <- isis_profile_to_pedon(.raw441("XX003", list(c("1974", ""), c("1990", "Haplic Nitisol"))))
  expect_null(p$site$reference_fao74)
  expect_null(p$site$reference_fao88)
  p <- isis_profile_to_pedon(.raw441("XX004", list()))
  expect_null(p$site$reference_fao74)
  # the attributes are recognised site properties
  s <- site_attribute_spec()
  expect_true(all(c("reference_fao74", "reference_fao88") %in% s$name))
})

test_that("the benchmark gives the same tables from PedonRecords as from the raw cache", {
  skip_on_cran()
  env <- .script441()
  raw <- list(.raw441("XX001", list(c("1974", "Orthic Luvisol"), c("1997", "Ferri-Haplic Luvisol"))),
              # ISIS lists the labels of a profile in either order
              .raw441("XX002", list(c("1997", "Eutric Cambisol skeletic"), c("1974", "Eutric Cambisol petric"))),
              .raw441("XX003", list(c("1997", "Haplic Nitisol"))),
              .raw441("XX004", list(c("1974", "Dystric Regosol")), layers = list()),
              .raw441("XX005", list()))
  f <- tempfile(fileext = ".rds")
  on.exit(unlink(f), add = TRUE)
  saveRDS(raw, f)
  a <- env$fao_isis_benchmark(cache = f, verbose = FALSE)
  peds <- lapply(raw, isis_profile_to_pedon)
  names(peds) <- vapply(raw, `[[`, "", "profileCode")
  b <- env$fao_isis_benchmark(pedons = peds, verbose = FALSE)
  for (k in c("records", "score", "status", "phases", "confusion", "undecided", "n_profiles"))
    expect_identical(b[[k]], a[[k]], info = k)
  expect_identical(a$records$year[a$records$profile == "XX002"], c("1974", "1997"))
  expect_true(is.na(b$cache))
  expect_match(env$.fao_isis_report(b)[3], "read_isis", fixed = TRUE)
  expect_error(env$fao_isis_benchmark(pedons = list(make_fao_example("LV")), verbose = FALSE),
               "No pedon carries")
  expect_error(env$fao_isis_benchmark(pedons = list(1), verbose = FALSE), "PedonRecords")
  expect_error(env$fao_isis_benchmark(cache = f, pedons = peds, verbose = FALSE), "not both")
})

# ---- (2) the grade of a result with no group -------------------------------------

test_that("no grouping passed and some undecided: grade C at most (1988)", {
  skip_on_cran()
  r <- .f441(make_durisol_canonical())
  expect_match(r$name, "^\\[Undecided in the FAO-Unesco Revised Legend \\(1988\\)")
  expect_equal(nrow(r$levels), 0L)
  expect_true(length(r$provisional_because) > 0L)
  expect_identical(r$evidence_grade, "C")
  expect_length(grep("^The evidence grade is at most C: no gate passed", r$warnings), 1L)
  amb <- vapply(r$ambiguities, function(a) a$rsg_code, "")
  expect_setequal(amb, sub("^major_groups:", "", r$provisional_because))
})

test_that("no group passed and some undecided: grade C at most (1974)", {
  skip_on_cran()
  cl <- .edit441(make_fao_example("X", edition = "1974"), designation = c("A", "Ck1", "Ck2"),
                 site = list(soil_moisture_regime = NA))
  r <- .f441(cl, "1974")
  expect_match(r$name, "^\\[Undecided in the FAO-Unesco Legend \\(1974\\): X Xerosols")
  expect_identical(r$evidence_grade, "C")
  expect_length(grep("^The evidence grade is at most C: no gate passed", r$warnings), 1L)
})

test_that("not classified, every gate failed: the grade of the data stands", {
  skip_on_cran()
  p <- .edit441(make_fao_example("PH"), rows = c(1, 2, 4),
                designation = c("A1", "A2", "C"), top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 140),
                bs_pct = c(70, 65, 30), ph_h2o = c(6, 6, 5))
  r <- .f441(p)
  expect_identical(r$name, "[Not classified in the FAO-Unesco Revised Legend (1988)]")
  expect_identical(r$evidence_grade, "A")
  expect_false(any(grepl("^The evidence grade is at most C", r$warnings)))
  cl <- .edit441(make_fao_example("X", edition = "1974"), designation = c("A", "Ck1", "Ck2"),
                 site = list(soil_moisture_regime = "udic"))
  r <- .f441(cl, "1974")
  expect_identical(r$name, "[Not classified in the FAO-Unesco Legend (1974)]")
  expect_identical(r$evidence_grade, "A")
})

# ---- (3) the missing attributes of the undecided routes -------------------------

test_that(".fao_route_missing() keeps the routes undecided where the test is", {
  skip_on_cran()
  rm <- soilKey:::.fao_route_missing
  a <- list(value = c(FALSE, FALSE, NA), missing = "a")
  b <- list(value = c(NA, FALSE, NA), missing = "b")
  expect_identical(rm(c(NA, FALSE, NA), a, b), c("a", "b"))
  expect_identical(rm(c(FALSE, FALSE, NA), list(value = c(FALSE, FALSE, FALSE), missing = "a"),
                      list(value = c(NA, FALSE, NA), missing = "b")), "b")
  # a route undecided only where the test is decided adds nothing
  expect_identical(rm(c(TRUE, NA), list(value = c(NA, FALSE), missing = "a"),
                      list(value = c(TRUE, NA), missing = "b")), "b")
  # one answer for the profile
  expect_identical(rm(NA, list(value = FALSE, missing = "a"), list(value = NA, missing = "b")), "b")
  expect_identical(rm(TRUE, list(value = NA, missing = "a")), character(0))
})

# The canonical Luvisol with the base saturation of its Bt lowered to 35 %,
# the exchangeable bases scaled with it.
.luv441 <- function() {
  p <- make_luvisol_canonical()
  h <- as.data.frame(p$horizons)
  bt <- grepl("^Bt", h$designation)
  f <- 35 / h$bs_pct[bt]
  for (k in c("ca_cmol", "mg_cmol", "k_cmol", "na_cmol")) h[[k]][bt] <- h[[k]][bt] * f
  h$bs_pct[bt] <- 35
  PedonRecord$new(site = p$site, horizons = ensure_horizon_schema(data.table::as.data.table(h)))
}

test_that("Ferric Acrisols (1974): the failed mottle route adds nothing to the missing data", {
  skip_on_cran()
  r <- .f441(.luv441(), "1974")
  expect_identical(r$levels$code, c("A", "Ao"))
  af <- r$trace$units$Af
  expect_true(is.na(af$passed))
  expect_setequal(af$missing, c("cec_cmol (cec_method nh4cl)", "ecec_cmol", "clay_pct"))
  mottle <- c("redoximorphic_features_pct", "mottle_size_class", "mottle_colors")
  expect_false(any(mottle %in% af$missing))
  expect_false(any(mottle %in% r$missing_data))
  expect_true("units:Af" %in% r$provisional_because)
  # with the mottles of the ferric properties in the Bt, both routes are
  # open only where the CEC is missing: the mottle attributes stay out
  fe <- soilKey:::ferric_properties_fao74(.luv441())
  expect_true(is.na(fe$passed))
  expect_false(any(mottle %in% fe$missing))
})

test_that("the routes of the other FAO tests: geric, salic, stagnic, high salinity", {
  skip_on_cran()
  base <- make_fao_example("LV")
  n <- nrow(base$horizons)
  # geric (1988): the delta pH decides no layer, the ECEC route is open
  p <- .edit441(base, ph_kcl = NA_real_, ph_h2o = 6, ecec_cmol = NA_real_, cec_cmol = NA_real_,
                ca_cmol = NA_real_, mg_cmol = NA_real_, k_cmol = NA_real_, na_cmol = NA_real_,
                al_cmol = NA_real_, exch_acidity_kcl_cmol = NA_real_)
  g <- soilKey:::geric_properties_fao88(p)
  expect_true(is.na(g$passed))
  expect_true(all(c("ph_kcl", "ph_h2o", "ca_cmol") %in% g$missing))
  # pH KCl 5 below pH H2O 6: the delta pH route fails in every layer
  g2 <- soilKey:::geric_properties_fao88(.edit441(p, ph_kcl = 5))
  expect_true(is.na(g2$passed))
  expect_false(any(c("ph_kcl", "ph_h2o") %in% g2$missing))
  expect_true("ca_cmol" %in% g2$missing)
  # salic (1988): an EC of 10 dS/m with no pH leaves only the pH open
  s <- soilKey:::salic_properties_fao88(.edit441(base, ec_dS_m = 10, ph_h2o = NA_real_))
  expect_true(is.na(s$passed))
  expect_false("ec_dS_m" %in% s$missing)
  # an EC of 2 decides it; no EC leaves the EC open
  expect_false(soilKey:::salic_properties_fao88(.edit441(base, ec_dS_m = 2, ph_h2o = NA_real_))$passed)
  s3 <- soilKey:::salic_properties_fao88(.edit441(base, ec_dS_m = NA_real_))
  expect_true(is.na(s3$passed))
  expect_true("ec_dS_m" %in% s3$missing)
})

test_that("the readings of 0.9.441 are recorded in both editions", {
  skip_on_cran()
  for (ed in c("fao1974", "fao1988")) {
    rd <- yaml::read_yaml(system.file("rules", ed, "readings.yaml", package = "soilKey"))$readings
    mine <- Filter(function(e) identical(e$version, "0.9.441"), rd)
    expect_setequal(vapply(mine, `[[`, "", "topic"),
                    c("The evidence grade of a result with no major soil group",
                      "The missing data of a test with alternative routes"))
  }
})
