# v0.9.221: the BDsolos and FEBR loaders keep dithionite Fe, oxalate Fe and
# the Fe2O3 of the sulfuric attack apart, in %.
#
# Until v0.9.220 both loaders filled fe_dcb_pct (dithionite Fe, which the WRB
# Fe-dith criteria read) with the sulfuric-attack Fe2O3 (total Fe, as the oxide,
# which the SiBCS ferrico classes read), unconverted from g/kg: a BDsolos
# horizon with 43 g/kg Fe2O3 had fe_dcb_pct = 43, so every horizon with a
# sulfuric attack passed the WRB Fe-dith limits. The BDsolos CDB column, which
# comes after the sulfuric one in the export, was dropped, and
# fe2o3_sulfuric_pct stayed empty.

test_that("BDsolos: CDB Fe to fe_dcb_pct, sulfuric Fe2O3 to fe2o3_sulfuric_pct, in %", {
  dir <- tempfile("bdsolos_v09221_"); dir.create(dir)
  csv <- file.path(dir, "bdsolos_fe.csv")
  hdr <- paste("id_perfil", "horizonte", "limite_sup", "limite_inf",
               "argila", "silte", "areia", "c_org",
               "Ataque sulfúrico - Fe2O3",
               "Ataque sulfúrico - Al2O3 / Fe2O3",
               "CDB - Ferro (g/kg)", "classificacao", sep = ",")
  rows <- c(
    paste("MG-7", "A",  "0",  "15", "550", "200", "250", "20", "152", "1.6", "79.3",
          "LATOSSOLO VERMELHO", sep = ","),
    paste("MG-7", "Bw", "30", "120", "600", "180", "220", "5", "190", "1.5", "96.5",
          "LATOSSOLO VERMELHO", sep = ","),
    paste("RJ-1", "A",  "0",  "20", "180", "300", "520", "15", "43", "4.1", "",
          "ARGISSOLO", sep = ","),
    paste("RJ-1", "Bt", "20", "90", "450", "200", "350", "3", "61", "3.9", "",
          "ARGISSOLO", sep = ","))
  writeLines(c(hdr, rows), csv)
  peds <- suppressMessages(load_bdsolos_csv(csv, verbose = FALSE))
  h <- setNames(lapply(peds, function(p) p$horizons), vapply(peds, function(p) p$site$id, ""))
  expect_equal(h[["MG-7"]]$fe_dcb_pct, c(7.93, 9.65))
  expect_equal(h[["MG-7"]]$fe2o3_sulfuric_pct, c(15.2, 19.0))
  expect_true(all(is.na(h[["RJ-1"]]$fe_dcb_pct)))         # no CDB, no dithionite Fe
  expect_equal(h[["RJ-1"]]$fe2o3_sulfuric_pct, c(4.3, 6.1))
})

test_that("BDsolos column names map to the right iron determination", {
  m <- function(x) soilKey:::.bdsolos_match_column(x)
  expect_identical(m("CDB - Ferro (g/kg)"), "fe_dcb_pct")
  expect_identical(m("Ataque sulfúrico - Fe2O3"), "fe2o3_sulfuric_pct")
  expect_identical(m("Oxalato de Amônio - Ferro"), "fe_ox_pct")
  expect_true(is.na(m("Ataque sulfúrico - Al2O3 / Fe2O3")))   # a ratio
  expect_true(is.na(m("Microelementos - Ferro")))                  # Mehlich Fe
})

test_that("FEBR: dictionary codes map by determination and convert to %", {
  cols <- c("camada_nome", "profund_sup", "profund_inf", "argila",
            "fe2o3_aquaregia_icpoes", "fe2o3_sulfurico_eaa",
            "ferro_ditionito_eaa", "fe2o3_oxalato_eaa")
  sk <- soilKey:::.febr_match_layer_columns(cols)
  expect_identical(sk$fe_dcb_pct, "ferro_ditionito_eaa")
  expect_identical(sk$fe_ox_pct, "fe2o3_oxalato_eaa")
  expect_identical(sk$fe2o3_sulfuric_pct, "fe2o3_sulfurico_eaa")
  rows <- data.frame(camada_nome = c("A", "Bw"), profund_sup = c(0, 20),
                     profund_inf = c(20, 100), argila = c("400", "550"),
                     fe2o3_aquaregia_icpoes = c("200", "210"),
                     fe2o3_sulfurico_eaa = c("152", "190"),
                     ferro_ditionito_eaa = c("79,3", "96,5"),
                     fe2o3_oxalato_eaa = c("10", "12"),
                     stringsAsFactors = FALSE)
  mc <- soilKey:::.detect_febr_munsell_columns(names(rows))
  hz <- soilKey:::.febr_rows_to_horizons(rows, sk, mc)
  expect_equal(hz$fe_dcb_pct, c(7.93, 9.65))
  expect_equal(hz$fe2o3_sulfuric_pct, c(15.2, 19.0))
  fe_per_fe2o3 <- 2 * 55.845 / 159.69
  expect_equal(hz$fe_ox_pct, c(1.0, 1.2) * fe_per_fe2o3)   # Fe2O3 -> Fe
  # sulfuric Fe reported as the element is converted to Fe2O3
  sk2 <- soilKey:::.febr_match_layer_columns(c("camada_nome", "profund_sup",
                                               "profund_inf", "ferro_sulfurico_xxx"))
  rows2 <- data.frame(camada_nome = "Bw", profund_sup = 0, profund_inf = 50,
                      ferro_sulfurico_xxx = "70", stringsAsFactors = FALSE)
  hz2 <- soilKey:::.febr_rows_to_horizons(rows2, sk2,
                                          soilKey:::.detect_febr_munsell_columns(names(rows2)))
  expect_equal(hz2$fe2o3_sulfuric_pct, 7.0 / fe_per_fe2o3)
})
