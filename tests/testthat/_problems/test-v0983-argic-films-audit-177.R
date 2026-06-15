# Extracted from test-v0983-argic-films-audit.R:177

# prequel ----------------------------------------------------------------------
.fix_with_films <- function(films) {
  hz <- data.table::data.table(
    top_cm    = c(0,    20,   55,   115),
    bottom_cm = c(20,   55,   115,  170),
    designation = c("A", "AB", "Bt1", "Bt2"),
    munsell_hue_moist    = c("10YR","7.5YR","5YR","2.5YR"),
    munsell_value_moist  = c(4, 4, 4, 3),
    munsell_chroma_moist = c(3, 5, 6, 6),
    structure_grade  = c("moderate","moderate","strong","strong"),
    structure_size   = c("medium","medium","medium","medium"),
    structure_type   = c("granular","subangular","subangular","subangular"),
    consistence_moist = c("friable","friable","firm","firm"),
    clay_pct = c(15, 25, 40, 45),
    sand_pct = c(60, 50, 30, 25),
    silt_pct = c(25, 25, 30, 30),
    cec_cmolc_kg = c(8, 6, 5, 4),
    bs_pct  = c(60, 55, 50, 45),
    oc_pct  = c(2.0, 1.0, 0.5, 0.3),
    ph_h2o  = c(5.0, 5.5, 5.8, 6.0),
    bulk_density_g_cm3 = c(1.3, 1.4, 1.5, 1.5),
    al_cmolc_kg = c(0.3, 0.2, 0.1, 0.0),
    coarse_fragments_pct = c(0, 0, 0, 0),
    clay_films_amount  = films
  )
  hz <- ensure_horizon_schema(hz)
  PedonRecord$new(site = list(id = "fix"), horizons = hz)
}

# test -------------------------------------------------------------------------
RJ <- "/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/soilKey/soil_data/embrapa_bdsolos/BD_solos/RJ.csv"
skip_if_not(file.exists(RJ), "BDsolos RJ.csv not available")
peds <- suppressMessages(suppressWarnings(load_bdsolos_csv(RJ, verbose = FALSE)))
res <- suppressMessages(suppressWarnings(
    benchmark_bdsolos(peds, systems = "sibcs", verbose = FALSE)))
conf <- res$per_system$sibcs$confusion
expect_equal(conf["Latossolos","Latossolos"], 17L)
expect_equal(conf["Latossolos","Argissolos"], 17L)
expect_equal(conf["Latossolos","Cambissolos"], 42L)
expect_equal(conf["Latossolos","Neossolos"], 38L)
expect_equal(conf["Argissolos","Latossolos"], 5L)
expect_equal(conf["Argissolos","Argissolos"], 166L)
expect_equal(conf["Argissolos","Cambissolos"], 1L)
expect_equal(conf["Argissolos","Neossolos"], 60L)
