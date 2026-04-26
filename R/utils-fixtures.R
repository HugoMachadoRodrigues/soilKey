# ================================================================
# Canonical fixtures used by tests, vignettes, and end users.
#
# The same builder functions are reused by the data-raw script that
# materialises the .rds files in inst/extdata/.
# ================================================================


#' Build the canonical Ferralsol fixture
#'
#' Synthetic but realistic Brazilian Latossolo Vermelho (Ferralsol per
#' WRB 2022): deeply weathered, kaolinitic / oxidic, with the canonical
#' "low-activity clay" signature. Diagnostic outcomes are deterministic
#' by construction:
#' \itemize{
#'   \item \code{\link{ferralic}}: PASSES on horizons Bw1 and Bw2
#'         (CEC/clay = 8.3 cmol_c/kg clay; ECEC/clay = 3.6 cmol_c/kg
#'         clay; texture sandy clay / clay; thickness >= 30 cm).
#'   \item \code{\link{argic}}: FAILS (gradual clay increase, all
#'         pairwise ratios < 1.2; absolute increment too small for the
#'         >= 40\% rule).
#'   \item \code{\link{mollic}}: FAILS (chroma > 3, BS < 50\%, A
#'         horizon < 20 cm thick).
#' }
#'
#' @return A \code{\link{PedonRecord}}.
#' @export
make_ferralsol_canonical <- function() {
  hz <- data.table::data.table(
    top_cm                = c(0,    15,   35,   65,   130),
    bottom_cm             = c(15,   35,   65,   130,  200),
    designation           = c("A",  "AB", "BA", "Bw1","Bw2"),
    munsell_hue_moist     = c("2.5YR","2.5YR","2.5YR","2.5YR","2.5YR"),
    munsell_value_moist   = c(3,    3,    3,    4,    4),
    munsell_chroma_moist  = c(4,    4,    6,    6,    6),
    munsell_value_dry     = c(4,    4,    4,    5,    5),
    munsell_chroma_dry    = c(6,    6,    6,    6,    6),
    structure_grade       = c("strong","strong","moderate","weak","weak"),
    structure_size        = c("fine","fine","fine","fine","fine"),
    structure_type        = c("granular","granular","subangular blocky",
                                "subangular blocky","subangular blocky"),
    consistence_moist     = c("friable","friable","friable","friable","friable"),
    clay_films            = c(NA_character_, NA_character_, NA_character_,
                                "absent","absent"),
    coarse_fragments_pct  = c(0,    0,    0,    0,    0),
    clay_pct              = c(50,   52,   55,   60,   60),
    silt_pct              = c(15,   14,   10,   8,    8),
    sand_pct              = c(35,   34,   35,   32,   32),
    ph_h2o                = c(4.8,  4.7,  4.7,  4.8,  4.9),
    ph_kcl                = c(4.0,  4.0,  4.0,  4.1,  4.2),
    oc_pct                = c(2.0,  1.2,  0.6,  0.3,  0.2),
    n_total_pct           = c(0.18, 0.10, 0.05, 0.03, 0.02),
    cec_cmol              = c(8.0,  6.5,  5.5,  5.0,  4.8),
    ca_cmol               = c(1.20, 0.70, 0.50, 0.40, 0.40),
    mg_cmol               = c(0.50, 0.30, 0.20, 0.20, 0.15),
    k_cmol                = c(0.15, 0.08, 0.05, 0.05, 0.05),
    na_cmol               = c(0.05, 0.04, 0.03, 0.03, 0.03),
    al_cmol               = c(0.70, 0.80, 0.60, 0.50, 0.50),
    bs_pct                = c(24,   17,   14,   13,   13),
    al_sat_pct            = c(26,   38,   43,   41,   43),
    fe_dcb_pct            = c(8.0,  8.5,  9.0,  9.5,  9.5),
    bulk_density_g_cm3    = c(1.20, 1.25, 1.20, 1.10, 1.10)
  )

  hz <- ensure_horizon_schema(hz)

  PedonRecord$new(
    site = list(
      id              = "FR-canonical-01",
      lat             = -22.5,
      lon             = -43.7,
      crs             = 4326,
      date            = as.Date("2024-03-10"),
      country         = "BR",
      parent_material = "gneiss",
      elevation_m     = 180,
      slope_pct       = 8,
      land_use        = "secondary forest",
      vegetation      = "Mata Atlantica regrowth",
      drainage_class  = "well drained"
    ),
    horizons = hz
  )
}


#' Build the canonical Luvisol fixture
#'
#' Synthetic temperate-zone Luvisol on loess: clear textural
#' differentiation, Bt with clay coatings, high base saturation, high-
#' activity clay. By construction:
#' \itemize{
#'   \item \code{\link{argic}}: PASSES on horizon Bt1 (clay increase
#'         from E (18\%) to Bt1 (35\%) gives ratio 1.94 in the 15-40\%
#'         band; thickness 25 cm; texture clay loam; no glossic
#'         features).
#'   \item \code{\link{ferralic}}: FAILS (CEC/clay ~ 45 cmol_c/kg clay
#'         in the Bt -- well above the 16 cmol_c/kg threshold).
#'   \item \code{\link{mollic}}: FAILS (A horizon: moist value 4 > 3,
#'         thickness 10 cm < 20 cm).
#' }
#'
#' @return A \code{\link{PedonRecord}}.
#' @export
make_luvisol_canonical <- function() {
  hz <- data.table::data.table(
    top_cm                = c(0,    10,   25,   50,   90),
    bottom_cm             = c(10,   25,   50,   90,   130),
    designation           = c("A",  "E",  "Bt1","Bt2","C"),
    munsell_hue_moist     = c("10YR","10YR","7.5YR","7.5YR","10YR"),
    munsell_value_moist   = c(4,    5,    4,    4,    5),
    munsell_chroma_moist  = c(3,    3,    4,    4,    3),
    munsell_value_dry     = c(5,    6,    5,    5,    6),
    munsell_chroma_dry    = c(3,    3,    4,    4,    3),
    structure_grade       = c("moderate","weak","strong","strong","weak"),
    structure_size        = c("fine","fine","medium","coarse","fine"),
    structure_type        = c("granular","platy","subangular blocky",
                                "subangular blocky","massive"),
    consistence_moist     = c("friable","friable","firm","firm","firm"),
    clay_films            = c(NA_character_, NA_character_, "common", "many",
                                NA_character_),
    coarse_fragments_pct  = c(2,    2,    5,    8,    15),
    clay_pct              = c(22,   18,   35,   38,   25),
    silt_pct              = c(30,   28,   25,   22,   28),
    sand_pct              = c(48,   54,   40,   40,   47),
    ph_h2o                = c(6.0,  5.8,  6.2,  6.5,  6.6),
    ph_kcl                = c(5.2,  5.0,  5.5,  5.8,  5.9),
    oc_pct                = c(2.0,  0.8,  0.5,  0.3,  0.2),
    n_total_pct           = c(0.18, 0.07, 0.05, 0.03, 0.02),
    cec_cmol              = c(15,   11,   16,   17,   13),
    ca_cmol               = c(6.5,  4.0,  8.0,  9.0,  6.5),
    mg_cmol               = c(2.0,  1.5,  2.5,  2.8,  2.0),
    k_cmol                = c(0.50, 0.30, 0.40, 0.40, 0.30),
    na_cmol               = c(0.10, 0.10, 0.10, 0.10, 0.10),
    al_cmol               = c(0,    0,    0,    0,    0),
    bs_pct                = c(60,   54,   69,   72,   68),
    al_sat_pct            = c(0,    0,    0,    0,    0),
    bulk_density_g_cm3    = c(1.30, 1.45, 1.45, 1.50, 1.55)
  )

  hz <- ensure_horizon_schema(hz)

  PedonRecord$new(
    site = list(
      id              = "LV-canonical-01",
      lat             = 47.5,
      lon             = 8.5,
      crs             = 4326,
      date            = as.Date("2023-09-15"),
      country         = "DE",
      parent_material = "loess over morainic deposits",
      elevation_m     = 420,
      slope_pct       = 3,
      land_use        = "arable",
      vegetation      = "deciduous mixed forest (historic)",
      drainage_class  = "well drained"
    ),
    horizons = hz
  )
}


#' Build the canonical Calcisol fixture
#'
#' Synthetic semi-arid Calcisol on calcareous loess: A horizon with
#' modest secondary carbonate; a thick Bk1 with the diagnostic calcic
#' horizon (35\% CaCO3 over 40 cm); deepening accumulation in Bk2.
#' By construction:
#' \itemize{
#'   \item \code{\link{calcic}}: PASSES on Bk1 and Bk2.
#'   \item \code{\link{gypsic}}, \code{\link{salic}}: FAIL.
#'   \item \code{\link{argic}}, \code{\link{ferralic}},
#'         \code{\link{mollic}}: FAIL.
#' }
#'
#' @return A \code{\link{PedonRecord}}.
#' @export
make_calcisol_canonical <- function() {
  hz <- data.table::data.table(
    top_cm                = c(0,    20,   60,   100),
    bottom_cm             = c(20,   60,   100,  150),
    designation           = c("A",  "Bk1","Bk2","C"),
    munsell_hue_moist     = c("10YR","10YR","10YR","10YR"),
    munsell_value_moist   = c(4,    5,    6,    7),
    munsell_chroma_moist  = c(3,    4,    4,    4),
    munsell_value_dry     = c(6,    7,    7,    8),
    munsell_chroma_dry    = c(3,    3,    3,    3),
    structure_grade       = c("moderate","moderate","weak","weak"),
    structure_size        = c("medium","medium","medium","medium"),
    structure_type        = c("subangular blocky","subangular blocky",
                                "subangular blocky","massive"),
    consistence_moist     = c("friable","friable","firm","firm"),
    coarse_fragments_pct  = c(5,    5,    8,    10),
    clay_pct              = c(22,   25,   25,   22),
    silt_pct              = c(35,   35,   35,   35),
    sand_pct              = c(43,   40,   40,   43),
    ph_h2o                = c(7.6,  8.0,  8.2,  8.4),
    ph_kcl                = c(7.0,  7.5,  7.7,  7.9),
    oc_pct                = c(0.8,  0.4,  0.2,  0.1),
    cec_cmol              = c(20,   18,   15,   12),
    ca_cmol               = c(17,   16,   14,   11),
    mg_cmol               = c(2,    1.5,  1,    0.8),
    k_cmol                = c(0.5,  0.3,  0.2,  0.1),
    na_cmol               = c(0.1,  0.1,  0.1,  0.1),
    al_cmol               = c(0,    0,    0,    0),
    bs_pct                = c(98,   100,  100,  100),
    al_sat_pct            = c(0,    0,    0,    0),
    caco3_pct             = c(5,    35,   40,   30),
    bulk_density_g_cm3    = c(1.30, 1.35, 1.40, 1.45)
  )

  hz <- ensure_horizon_schema(hz)

  PedonRecord$new(
    site = list(
      id              = "CL-canonical-01",
      lat             = 36.5,
      lon             = -4.5,
      crs             = 4326,
      date            = as.Date("2023-05-12"),
      country         = "ES",
      parent_material = "calcareous loess",
      elevation_m     = 280,
      slope_pct       = 5,
      land_use        = "olive grove",
      vegetation      = "Mediterranean shrubland (degraded)",
      drainage_class  = "well drained"
    ),
    horizons = hz
  )
}


#' Build the canonical Gypsisol fixture
#'
#' Synthetic Gypsisol on gypsiferous parent material: shallow A; gypsum
#' accumulation rising sharply in the By1 horizon (35\% gypsum over 50
#' cm) -- the diagnostic gypsic horizon. By construction:
#' \itemize{
#'   \item \code{\link{gypsic}}: PASSES on By1 and By2.
#'   \item \code{\link{calcic}}, \code{\link{salic}}: FAIL.
#'   \item \code{\link{argic}}, \code{\link{ferralic}},
#'         \code{\link{mollic}}: FAIL.
#' }
#'
#' @return A \code{\link{PedonRecord}}.
#' @export
make_gypsisol_canonical <- function() {
  hz <- data.table::data.table(
    top_cm                = c(0,    15,   50,   100),
    bottom_cm             = c(15,   50,   100,  150),
    designation           = c("A",  "AyB","By1","By2"),
    munsell_hue_moist     = c("10YR","10YR","10YR","10YR"),
    munsell_value_moist   = c(5,    6,    7,    7),
    munsell_chroma_moist  = c(3,    4,    3,    3),
    munsell_value_dry     = c(6,    7,    8,    8),
    munsell_chroma_dry    = c(3,    3,    2,    2),
    structure_grade       = c("weak","weak","weak","massive"),
    structure_size        = c("medium","medium","fine","fine"),
    structure_type        = c("subangular blocky","subangular blocky",
                                "platy","massive"),
    consistence_moist     = c("friable","firm","firm","firm"),
    coarse_fragments_pct  = c(5,    8,    15,   20),
    clay_pct              = c(20,   25,   25,   22),
    silt_pct              = c(35,   35,   35,   33),
    sand_pct              = c(45,   40,   40,   45),
    ph_h2o                = c(7.8,  7.6,  7.5,  7.5),
    ph_kcl                = c(7.2,  7.0,  6.9,  6.9),
    oc_pct                = c(0.5,  0.3,  0.2,  0.1),
    cec_cmol              = c(15,   13,   10,   8),
    ca_cmol               = c(13,   11,   9,    7),
    mg_cmol               = c(1.5,  1.2,  0.8,  0.6),
    k_cmol                = c(0.3,  0.2,  0.1,  0.1),
    na_cmol               = c(0.2,  0.2,  0.1,  0.1),
    al_cmol               = c(0,    0,    0,    0),
    bs_pct                = c(100,  100,  100,  100),
    caco3_pct             = c(5,    8,    5,    3),
    caso4_pct             = c(0.5,  8,    35,   25),
    bulk_density_g_cm3    = c(1.35, 1.45, 1.55, 1.60)
  )

  hz <- ensure_horizon_schema(hz)

  PedonRecord$new(
    site = list(
      id              = "GY-canonical-01",
      lat             = 41.2,
      lon             = -1.5,
      crs             = 4326,
      date            = as.Date("2023-07-08"),
      country         = "ES",
      parent_material = "gypsiferous lutites (Tertiary)",
      elevation_m     = 480,
      slope_pct       = 4,
      land_use        = "extensive grazing",
      vegetation      = "halophilic / gypsophilic shrubs",
      drainage_class  = "somewhat excessively drained"
    ),
    horizons = hz
  )
}


#' Build the canonical Solonchak fixture
#'
#' Synthetic Solonchak from a coastal-arid setting: surface salt
#' accumulation gives the diagnostic salic horizon (EC 25 dS/m over 25
#' cm); EC declines but stays elevated in the Bz; non-saline C below.
#' By construction:
#' \itemize{
#'   \item \code{\link{salic}}: PASSES on Az.
#'   \item \code{\link{gypsic}}, \code{\link{calcic}}: FAIL.
#'   \item \code{\link{argic}}, \code{\link{ferralic}},
#'         \code{\link{mollic}}: FAIL.
#' }
#'
#' @return A \code{\link{PedonRecord}}.
#' @export
make_solonchak_canonical <- function() {
  hz <- data.table::data.table(
    top_cm                = c(0,    25,   60),
    bottom_cm             = c(25,   60,   150),
    designation           = c("Az", "Bz", "C"),
    munsell_hue_moist     = c("10YR","10YR","10YR"),
    munsell_value_moist   = c(4,    5,    6),
    munsell_chroma_moist  = c(2,    3,    3),
    munsell_value_dry     = c(6,    7,    7),
    munsell_chroma_dry    = c(2,    2,    3),
    structure_grade       = c("weak","weak","massive"),
    structure_size        = c("medium","medium","fine"),
    structure_type        = c("subangular blocky","subangular blocky","massive"),
    consistence_moist     = c("friable","firm","firm"),
    coarse_fragments_pct  = c(0,    0,    2),
    clay_pct              = c(30,   35,   35),
    silt_pct              = c(45,   45,   40),
    sand_pct              = c(25,   20,   25),
    ph_h2o                = c(8.4,  8.6,  8.0),
    ph_kcl                = c(7.8,  8.0,  7.5),
    oc_pct                = c(0.6,  0.3,  0.2),
    cec_cmol              = c(28,   30,   25),
    ca_cmol               = c(15,   12,   18),
    mg_cmol               = c(5,    6,    4),
    k_cmol                = c(0.8,  0.9,  0.4),
    na_cmol               = c(7.0,  10.0, 2.0),
    al_cmol               = c(0,    0,    0),
    bs_pct                = c(99,   97,   98),
    ec_dS_m               = c(25,   18,   4),
    bulk_density_g_cm3    = c(1.20, 1.30, 1.40)
  )

  hz <- ensure_horizon_schema(hz)

  PedonRecord$new(
    site = list(
      id              = "SC-canonical-01",
      lat             = 38.0,
      lon             = -0.7,
      crs             = 4326,
      date            = as.Date("2023-08-20"),
      country         = "ES",
      parent_material = "alluvium with marine influence",
      elevation_m     = 5,
      slope_pct       = 0,
      land_use        = "salt-marsh fringe",
      vegetation      = "Salicornia / Sarcocornia community",
      drainage_class  = "imperfectly drained"
    ),
    horizons = hz
  )
}


#' Build the canonical Chernozem fixture
#'
#' Synthetic Ukrainian / Russian steppe Chernozem on loess: thick dark
#' Ah, granular structure, secondary carbonates accumulating in the Bk.
#' By construction:
#' \itemize{
#'   \item \code{\link{mollic}}: PASSES on horizon Ah1 (moist value 2,
#'         chroma 1, dry value 3; SOC 4\%; BS 89\%; thickness 30 cm;
#'         strong granular structure).
#'   \item \code{\link{argic}}: FAILS (essentially no clay
#'         differentiation; ratios all close to 1).
#'   \item \code{\link{ferralic}}: FAILS (CEC/clay ~ 120 cmol_c/kg
#'         clay -- high-activity 2:1 clay).
#' }
#'
#' @return A \code{\link{PedonRecord}}.
#' @export
make_chernozem_canonical <- function() {
  hz <- data.table::data.table(
    top_cm                = c(0,    30,   60,   100,  140),
    bottom_cm             = c(30,   60,   100,  140,  180),
    designation           = c("Ah1","Ah2","AB","Bk","Ck"),
    munsell_hue_moist     = c("10YR","10YR","10YR","10YR","10YR"),
    munsell_value_moist   = c(2,    2,    3,    4,    5),
    munsell_chroma_moist  = c(1,    1,    2,    3,    3),
    munsell_value_dry     = c(3,    3,    4,    6,    7),
    munsell_chroma_dry    = c(2,    2,    3,    3,    3),
    structure_grade       = c("strong","strong","moderate","weak","weak"),
    structure_size        = c("medium","medium","medium","medium","medium"),
    structure_type        = c("granular","granular","subangular blocky",
                                "subangular blocky","massive"),
    consistence_moist     = c("friable","friable","friable","friable","firm"),
    clay_films            = c(NA_character_, NA_character_, NA_character_,
                                NA_character_, NA_character_),
    coarse_fragments_pct  = c(0,    0,    0,    0,    0),
    clay_pct              = c(25,   26,   27,   27,   25),
    silt_pct              = c(50,   51,   50,   49,   50),
    sand_pct              = c(25,   23,   23,   24,   25),
    ph_h2o                = c(7.2,  7.4,  7.5,  8.0,  8.2),
    ph_kcl                = c(6.8,  7.0,  7.0,  7.3,  7.5),
    oc_pct                = c(4.0,  2.5,  1.5,  0.8,  0.4),
    n_total_pct           = c(0.35, 0.21, 0.13, 0.07, 0.04),
    cec_cmol              = c(30,   28,   26,   25,   22),
    ca_cmol               = c(22,   20,   19,   20,   17),
    mg_cmol               = c(4,    4,    4,    4,    3.5),
    k_cmol                = c(0.6,  0.4,  0.3,  0.2,  0.2),
    na_cmol               = c(0.1,  0.1,  0.1,  0.1,  0.1),
    al_cmol               = c(0,    0,    0,    0,    0),
    bs_pct                = c(89,   87,   86,   97,   95),
    al_sat_pct            = c(0,    0,    0,    0,    0),
    caco3_pct             = c(0,    0,    0,    8,    12),
    bulk_density_g_cm3    = c(1.05, 1.10, 1.20, 1.30, 1.35)
  )

  hz <- ensure_horizon_schema(hz)

  PedonRecord$new(
    site = list(
      id              = "CH-canonical-01",
      lat             = 47.5,
      lon             = 30.5,
      crs             = 4326,
      date            = as.Date("2022-06-12"),
      country         = "UA",
      parent_material = "loess",
      elevation_m     = 220,
      slope_pct       = 1,
      land_use        = "wheat-sunflower rotation",
      vegetation      = "former feather-grass steppe",
      drainage_class  = "well drained"
    ),
    horizons = hz
  )
}
