# Extracted from test-v0989-texture-engine-fallback.R:92

# prequel ----------------------------------------------------------------------
.fix_no_texture_with_Bw <- function() {
  hz <- data.table::data.table(
    top_cm    = c(0,    25,   60,   100),
    bottom_cm = c(25,   60,  100,   200),
    designation = c("Ap","BA","Bw","Bo"),
    clay_pct = rep(NA_real_, 4),  # texture missing on every layer
    sand_pct = rep(NA_real_, 4),
    silt_pct = rep(NA_real_, 4),
    cec_cmol = c(8, 6, 5, 4),
    bs_pct  = c(35, 30, 25, 20),
    oc_pct  = c(2.0, 0.8, 0.4, 0.2),
    ph_h2o  = c(5.0, 5.2, 5.3, 5.4),
    munsell_value_moist = c(3, 4, 4, 5),
    munsell_chroma_moist = c(2, 3, 4, 5),
    structure_grade = rep("moderate", 4),
    structure_size  = rep("medium", 4),
    structure_type  = rep("granular", 4),
    consistence_moist = rep("friable", 4),
    bulk_density_g_cm3 = rep(1.2, 4),
    coarse_fragments_pct = rep(0, 4)
  )
  hz <- ensure_horizon_schema(hz)
  PedonRecord$new(site = list(id = "Bw-no-texture"), horizons = hz)
}

# test -------------------------------------------------------------------------
RJ <- "/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/soilKey/soil_data/embrapa_bdsolos/BD_solos/RJ.csv"
skip_if_not(file.exists(RJ), "BDsolos RJ.csv not available")
peds <- suppressMessages(suppressWarnings(load_bdsolos_csv(RJ, verbose = FALSE)))
res_aqp <- withr::with_options(list(soilKey.diagnostic_engine = "aqp"), {
    suppressMessages(suppressWarnings(
      benchmark_bdsolos(peds, systems = "sibcs", verbose = FALSE)))
  })
cf_aqp <- res_aqp$per_system$sibcs$confusion
expect_gte(cf_aqp["Latossolos","Latossolos"], 33L)
