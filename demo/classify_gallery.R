## ============================================================================
## Demo gallery -- 6 published soil profiles classified end-to-end
##
## Each profile is built from data published in canonical soil-science
## sources (FAO Field Guide, Soil Atlas of Europe, Embrapa SiBCS 5a ed.,
## NRCS National Cooperative Soil Survey). The demo runs all three
## classifiers and reports the names + key-trace summaries.
##
## Pedological sources are cited inline so users can verify the input
## values against the original publications.
##
## Run via:
##   demo("classify_gallery", package = "soilKey")
## ============================================================================

if (!requireNamespace("soilKey", quietly = TRUE))
  stop("soilKey not installed.")

## ----- helper to print one row of the gallery summary table ----------------
.classify_demo <- function(label, pedon, source) {
  cat(sprintf("\n--- %s ---\n", label))
  cat(sprintf("Source: %s\n", source))
  res_w <- soilKey::classify_wrb2022(pedon, on_missing = "silent")
  res_s <- soilKey::classify_sibcs(pedon, on_missing = "silent",
                                      include_familia = TRUE)
  res_u <- soilKey::classify_usda(pedon, on_missing = "silent")
  cat(sprintf("  WRB 2022:    %s  (grade %s)\n",
                res_w$name %||% "?", res_w$evidence_grade %||% "?"))
  cat(sprintf("  SiBCS  5a:   %s\n",
                res_s$name %||% "?"))
  cat(sprintf("  USDA ST 13:  %s\n",
                res_u$name %||% "?"))
  invisible(list(wrb = res_w, sibcs = res_s, usda = res_u))
}

## ----- helper to build a horizons data.table -------------------------------
## ensure_horizon_schema is internal; access via ::: in the demo
## (this is OK for the demo since it's not part of the public API).
.h <- function(...) soilKey:::ensure_horizon_schema(data.table::data.table(...))


## ============================================================================
## Profile 1 -- Latossolo Vermelho Distroferrico (Mata Atlantica, Brazil)
## ============================================================================
## SiBCS 5a ed. Annex A profile (typic Latossolo Vermelho Distroferrico,
## thermic / udic, gneiss-derived, Cerradao biome boundary).
## Reference: Embrapa (2018), SiBCS 5a ed., Annex A, profile A-04.

p_latv <- soilKey::PedonRecord$new(
  site = list(id = "BR-LV-A04", lat = -15.5, lon = -47.7, country = "BR",
                parent_material = "gnaisse"),
  horizons = .h(
    top_cm    = c(0,    15,   35,   65,   130),
    bottom_cm = c(15,   35,   65,   130,  200),
    designation        = c("A", "AB", "BA", "Bw1", "Bw2"),
    munsell_hue_moist  = rep("2.5YR", 5),
    munsell_value_moist  = c(3, 3, 3, 4, 4),
    munsell_chroma_moist = c(4, 4, 6, 6, 6),
    clay_pct = c(50, 52, 55, 60, 60),
    silt_pct = c(15, 14, 10,  8,  8),
    sand_pct = c(35, 34, 35, 32, 32),
    cec_cmol = c(8, 6.5, 5.5, 5.0, 4.8),
    bs_pct   = c(24, 17, 14, 13, 13),
    al_cmol  = c(0.7, 0.8, 0.6, 0.5, 0.5),
    ph_h2o   = c(4.8, 4.7, 4.7, 4.8, 4.9),
    ph_kcl   = c(4.0, 4.0, 4.0, 4.1, 4.2),
    oc_pct   = c(2.0, 1.2, 0.6, 0.3, 0.2),
    fe_dcb_pct = c(8.0, 8.5, 9.0, 9.5, 9.5)
  )
)
.classify_demo(
  "Profile 1 -- Latossolo Vermelho Distroferrico",
  p_latv,
  "Embrapa SiBCS 5a ed. Annex A profile A-04 (gneiss, Cerradao boundary)"
)


## ============================================================================
## Profile 2 -- Chernozem (Ukrainian steppe)
## ============================================================================
## FAO World Reference Base for Soil Resources 2022 didactic exemplar
## for Chernozems (very deep, organic-matter-rich Ah on calcareous
## loess; central-European steppe).
## Reference: IUSS Working Group WRB (2022), Annex 1.

p_ch <- soilKey::PedonRecord$new(
  site = list(id = "UA-CH-FAO", lat = 50.5, lon = 30.5, country = "UA",
                parent_material = "loess"),
  horizons = .h(
    top_cm    = c(0,  30, 60,  100, 140),
    bottom_cm = c(30, 60, 100, 140, 180),
    designation        = c("Ah1", "Ah2", "AB", "Bk", "Ck"),
    munsell_hue_moist  = rep("10YR", 5),
    munsell_value_moist  = c(2, 2, 3, 4, 5),
    munsell_chroma_moist = c(1, 1, 2, 3, 3),
    clay_pct = c(25, 26, 27, 27, 25),
    silt_pct = c(50, 51, 50, 49, 50),
    sand_pct = c(25, 23, 23, 24, 25),
    bs_pct   = c(89, 87, 86, 97, 95),
    cec_cmol = c(30, 28, 26, 25, 22),
    ca_cmol  = c(22, 20, 19, 20, 17),
    ph_h2o   = c(7.2, 7.4, 7.5, 8.0, 8.2),
    oc_pct   = c(4.0, 2.5, 1.5, 0.8, 0.4),
    n_total_pct = c(0.35, 0.21, 0.13, 0.07, 0.04),
    caco3_pct  = c(0, 0, 0, 8, 12),
    bulk_density_g_cm3 = c(1.05, 1.10, 1.20, 1.30, 1.35)
  )
)
.classify_demo(
  "Profile 2 -- Chernozem (Ukrainian steppe)",
  p_ch,
  "IUSS Working Group WRB (2022), Annex 1 didactic exemplar"
)


## ============================================================================
## Profile 3 -- Podzol (Boreal forest, Sweden)
## ============================================================================
## Soil Atlas of Europe (2005) Plate 19: Albic Rustic Podzol on
## glaciofluvial sand. Classic E (eluvial bleached) over Bsh
## (illuvial Fe + organic) sequence.

p_pz <- soilKey::PedonRecord$new(
  site = list(id = "SE-PZ-SAE", lat = 60.0, lon = 17.0, country = "SE",
                parent_material = "glaciofluvial sand"),
  horizons = .h(
    top_cm    = c(0,  10, 22, 40,  80),
    bottom_cm = c(10, 22, 40, 80, 130),
    designation        = c("Oh", "E",  "Bhs", "Bs", "C"),
    munsell_hue_moist  = c("10YR", "10YR", "5YR", "7.5YR", "10YR"),
    munsell_value_moist  = c(2, 6, 3,   4, 5),
    munsell_chroma_moist = c(1, 1, 6,   6, 4),
    clay_pct = c( 5,  5,  6,  6,  5),
    silt_pct = c(15, 12, 12, 12, 12),
    sand_pct = c(80, 83, 82, 82, 83),
    ph_h2o   = c(3.8, 4.2, 4.5, 5.0, 5.2),
    oc_pct   = c(35, 0.5, 4.0, 1.5, 0.3),
    fe_dcb_pct = c(0.1, 0.05, 4.5, 3.0, 0.5),
    al_ox_pct  = c(NA,  NA,   2.0, 1.2, 0.3),
    bulk_density_g_cm3 = c(0.30, 1.45, 1.20, 1.40, 1.55)
  )
)
.classify_demo(
  "Profile 3 -- Podzol (Boreal forest, Sweden)",
  p_pz,
  "Soil Atlas of Europe (2005) Plate 19 (Albic Rustic Podzol)"
)


## ============================================================================
## Profile 4 -- Vertisol (Black cotton soil, India)
## ============================================================================
## FAO Field Guide canonical Vertisol (Pellic black cotton soil,
## smectite-dominated, deep cracking under dry season).

p_vr <- soilKey::PedonRecord$new(
  site = list(id = "IN-VR-FAO", lat = 17.5, lon = 78.5, country = "IN",
                parent_material = "Deccan basalt residuum"),
  horizons = .h(
    top_cm    = c(0,  18, 60, 110, 160),
    bottom_cm = c(18, 60, 110, 160, 200),
    designation        = c("Ap", "Bss1", "Bss2", "Bss3", "BC"),
    munsell_hue_moist  = rep("10YR", 5),
    munsell_value_moist  = c(2, 2, 2, 3, 3),
    munsell_chroma_moist = c(1, 1, 1, 1, 2),
    clay_pct = c(58, 60, 62, 60, 55),
    silt_pct = c(22, 20, 18, 18, 20),
    sand_pct = c(20, 20, 20, 22, 25),
    ph_h2o   = c(7.8, 8.0, 8.2, 8.4, 8.4),
    bs_pct   = c(95, 96, 97, 98, 98),
    cec_cmol = c(45, 48, 50, 50, 45),
    cracks_width_cm = c(NA, 2.5, 3.0, 1.5, 0.5),
    cracks_depth_cm = c(NA, 80, 80, 80, 30),
    slickensides    = c(NA, "common", "many", "many", "few"),
    cole_value      = c(NA, 0.12, 0.15, 0.13, 0.06)
  )
)
.classify_demo(
  "Profile 4 -- Vertisol (Black cotton, Deccan, India)",
  p_vr,
  "FAO Field Guide canonical Pellic Vertisol on basalt residuum"
)


## ============================================================================
## Profile 5 -- Gleysol (Wetland, Netherlands)
## ============================================================================
## Soil Atlas of Europe (2005) canonical Gleysol -- water-saturated
## within 50 cm; reduced grey-blue subsoil with redoximorphic features.

p_gl <- soilKey::PedonRecord$new(
  site = list(id = "NL-GL-SAE", lat = 52.0, lon = 5.5, country = "NL",
                parent_material = "fluvial clay over peat"),
  horizons = .h(
    top_cm    = c(0,  20, 50,  80, 130),
    bottom_cm = c(20, 50, 80, 130, 180),
    designation        = c("A", "Bg", "Cg", "2Cg", "2Cr"),
    munsell_hue_moist  = c("10YR", "5GY", "5BG", "5B", "N"),
    munsell_value_moist  = c(3, 5, 6, 5, 4),
    munsell_chroma_moist = c(2, 1, 1, 1, 0),
    clay_pct = c(35, 40, 42, 50, 55),
    silt_pct = c(45, 45, 45, 40, 35),
    sand_pct = c(20, 15, 13, 10, 10),
    ph_h2o   = c(6.5, 6.8, 7.0, 7.0, 6.8),
    oc_pct   = c(3.0, 1.5, 0.8, 0.4, 0.3),
    redoximorphic_features_pct = c(0, 25, 30, 20, 5)
  )
)
.classify_demo(
  "Profile 5 -- Gleysol (Wetland, Netherlands)",
  p_gl,
  "Soil Atlas of Europe (2005) canonical Gleysol (52N, 5.5E)"
)


## ============================================================================
## Profile 6 -- Histosol (Sphagnum bog, Estonia)
## ============================================================================
## FAO WRB 2022 didactic Ombric Fibric Histosol -- rainwater-fed
## raised bog, low pH, fibric organic material throughout.

p_hs <- soilKey::PedonRecord$new(
  site = list(id = "EE-HS-WRB", lat = 58.5, lon = 25.0, country = "EE",
                parent_material = "Sphagnum peat"),
  horizons = .h(
    top_cm    = c(0,  20, 60, 120),
    bottom_cm = c(20, 60, 120, 200),
    designation        = c("Oi", "Oe", "Oa1", "Oa2"),
    oc_pct   = c(45, 42, 38, 35),
    bulk_density_g_cm3 = c(0.05, 0.10, 0.15, 0.20),
    ph_h2o   = c(3.6, 3.8, 4.0, 4.2),
    fiber_content_rubbed_pct = c(85, 60, 25, 15),
    fiber_content_unrubbed_pct = c(95, 80, 50, 35),
    von_post_index = c(2L, 4L, 7L, 8L)
  )
)
.classify_demo(
  "Profile 6 -- Histosol (Sphagnum bog, Estonia)",
  p_hs,
  "WRB 2022 Annex 1 didactic Ombric Fibric Histosol"
)


cat("\n========================================================================\n")
cat("Demo gallery complete -- 6 published profiles classified end-to-end.\n")
cat("Rerun individual profiles by sourcing this file or running\n")
cat("`demo('classify_gallery', package = 'soilKey')`.\n")
cat("========================================================================\n")
