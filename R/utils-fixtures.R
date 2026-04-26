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


#' Build the canonical Cambisol fixture
#'
#' Synthetic temperate-zone Cambisol on weathered colluvium: modest
#' subsurface alteration in Bw without meeting argic clay-increase or
#' ferralic CEC criteria. By construction:
#' \itemize{
#'   \item \code{\link{cambic}}: PASSES on Bw (thickness 35 cm, sandy
#'         clay loam, no argic / no ferralic).
#'   \item \code{\link{argic}}, \code{\link{ferralic}},
#'         \code{\link{mollic}}, \code{\link{calcic}},
#'         \code{\link{gypsic}}, \code{\link{salic}}: FAIL.
#' }
#'
#' @return A \code{\link{PedonRecord}}.
#' @export
make_cambisol_canonical <- function() {
  hz <- data.table::data.table(
    top_cm                     = c(0,    15,   50,   100),
    bottom_cm                  = c(15,   50,   100,  150),
    designation                = c("A",  "Bw", "BC", "C"),
    munsell_hue_moist          = c("10YR","7.5YR","7.5YR","7.5YR"),
    munsell_value_moist        = c(4,    4,    5,    5),
    munsell_chroma_moist       = c(3,    4,    4,    3),
    munsell_value_dry          = c(5,    5,    6,    6),
    munsell_chroma_dry         = c(3,    4,    4,    3),
    structure_grade            = c("moderate","moderate","weak","weak"),
    structure_size             = c("medium","medium","medium","medium"),
    structure_type             = c("subangular blocky","subangular blocky",
                                     "subangular blocky","massive"),
    consistence_moist          = c("friable","friable","firm","firm"),
    coarse_fragments_pct       = c(5,    8,    12,   15),
    clay_pct                   = c(25,   27,   25,   22),
    silt_pct                   = c(35,   33,   33,   33),
    sand_pct                   = c(40,   40,   42,   45),
    ph_h2o                     = c(6.5,  6.7,  6.8,  6.8),
    ph_kcl                     = c(5.8,  6.0,  6.1,  6.1),
    oc_pct                     = c(1.5,  0.5,  0.3,  0.2),
    cec_cmol                   = c(18,   17,   16,   14),
    ca_cmol                    = c(8,    8,    8,    7),
    mg_cmol                    = c(2.5,  2.5,  2.0,  1.8),
    k_cmol                     = c(0.4,  0.3,  0.2,  0.2),
    na_cmol                    = c(0.1,  0.1,  0.1,  0.1),
    al_cmol                    = c(0.1,  0.1,  0.1,  0.1),
    bs_pct                     = c(60,   65,   65,   65),
    al_sat_pct                 = c(1,    1,    1,    1),
    plinthite_pct              = c(0,    0,    0,    0),
    redoximorphic_features_pct = c(0,    0,    0,    0),
    slickensides               = c("absent","absent","absent","absent"),
    bulk_density_g_cm3         = c(1.30, 1.40, 1.50, 1.55)
  )

  hz <- ensure_horizon_schema(hz)

  PedonRecord$new(
    site = list(
      id              = "CM-canonical-01",
      lat             = 47.3,
      lon             = 11.5,
      crs             = 4326,
      date            = as.Date("2023-08-22"),
      country         = "AT",
      parent_material = "weathered colluvium over schist",
      elevation_m     = 920,
      slope_pct       = 18,
      land_use        = "alpine pasture",
      vegetation      = "subalpine grassland",
      drainage_class  = "well drained"
    ),
    horizons = hz
  )
}


#' Build the canonical Plinthosol fixture
#'
#' Synthetic seasonally-saturated tropical Plinthosol: A horizon with
#' typical Cerrado SOC; Btv with diagnostic plinthite (25\% by volume
#' over 60 cm); persistent plinthite at depth. By construction:
#' \itemize{
#'   \item \code{\link{plinthic}}: PASSES on Btv and Cv.
#'   \item \code{\link{argic}}, \code{\link{ferralic}},
#'         \code{\link{mollic}}, \code{\link{spodic}},
#'         \code{\link{calcic}}, \code{\link{gypsic}},
#'         \code{\link{salic}}: FAIL.
#' }
#'
#' @return A \code{\link{PedonRecord}}.
#' @export
make_plinthosol_canonical <- function() {
  hz <- data.table::data.table(
    top_cm                     = c(0,    20,   80),
    bottom_cm                  = c(20,   80,   150),
    designation                = c("A",  "Btv","Cv"),
    munsell_hue_moist          = c("7.5YR","5YR","5YR"),
    munsell_value_moist        = c(4,    4,    5),
    munsell_chroma_moist       = c(3,    6,    6),
    munsell_value_dry          = c(5,    5,    6),
    munsell_chroma_dry         = c(4,    6,    6),
    structure_grade            = c("moderate","strong","moderate"),
    structure_size             = c("fine","medium","medium"),
    structure_type             = c("granular","subangular blocky",
                                     "subangular blocky"),
    consistence_moist          = c("friable","firm","firm"),
    coarse_fragments_pct       = c(2,    8,    20),
    clay_pct                   = c(35,   38,   35),
    silt_pct                   = c(20,   22,   25),
    sand_pct                   = c(45,   40,   40),
    ph_h2o                     = c(5.0,  5.2,  5.3),
    ph_kcl                     = c(4.4,  4.5,  4.6),
    oc_pct                     = c(2.0,  0.5,  0.2),
    cec_cmol                   = c(14,   12,   10),
    ca_cmol                    = c(2.5,  2.0,  1.8),
    mg_cmol                    = c(1.0,  0.8,  0.7),
    k_cmol                     = c(0.2,  0.1,  0.1),
    na_cmol                    = c(0.1,  0.1,  0.1),
    al_cmol                    = c(1.0,  1.2,  1.0),
    bs_pct                     = c(28,   25,   27),
    al_sat_pct                 = c(20,   28,   25),
    plinthite_pct              = c(0,    25,   18),
    # Plinthite (hardening Fe nodules) is conceptually distinct from
    # gleyic redoximorphic features (softer reduction mottles); the
    # canonical fixture separates them so the WRB-key order
    # (GL @ #9 before PT @ #12) does not misclassify the Plinthosol.
    redoximorphic_features_pct = c(0,    0,    0),
    slickensides               = c("absent","absent","absent"),
    bulk_density_g_cm3         = c(1.30, 1.45, 1.55)
  )

  hz <- ensure_horizon_schema(hz)

  PedonRecord$new(
    site = list(
      id              = "PT-canonical-01",
      lat             = -15.5,
      lon             = -47.8,
      crs             = 4326,
      date            = as.Date("2024-04-05"),
      country         = "BR",
      parent_material = "Tertiary plinthitic sediments",
      elevation_m     = 920,
      slope_pct       = 3,
      land_use        = "Cerrado native",
      vegetation      = "campo cerrado",
      drainage_class  = "imperfectly drained (seasonal saturation)"
    ),
    horizons = hz
  )
}


#' Build the canonical Podzol fixture
#'
#' Synthetic boreal / temperate-coniferous Podzol: bleached E (low
#' clay, low CEC), illuvial Bs with diagnostic Al/Fe oxalate
#' accumulation, weathered C. By construction:
#' \itemize{
#'   \item \code{\link{spodic}}: PASSES on Bs (Al_ox + 0.5*Fe_ox = 0.6,
#'         pH 4.5, 40 cm thick).
#'   \item \code{\link{argic}}, \code{\link{ferralic}},
#'         \code{\link{mollic}}, \code{\link{cambic}},
#'         \code{\link{plinthic}}, \code{\link{calcic}},
#'         \code{\link{gypsic}}, \code{\link{salic}}: FAIL.
#' }
#'
#' E horizon Munsell is set to chroma 3 (rather than canonical 1-2 of a
#' true albic) to keep \code{gleyic_properties} clearly negative under
#' the conservative v0.2 criterion.
#'
#' @return A \code{\link{PedonRecord}}.
#' @export
make_podzol_canonical <- function() {
  hz <- data.table::data.table(
    top_cm                     = c(0,    5,    30,   70),
    bottom_cm                  = c(5,    30,   70,   150),
    designation                = c("Oa", "E",  "Bs", "BC"),
    munsell_hue_moist          = c("10YR","10YR","7.5YR","10YR"),
    munsell_value_moist        = c(2,    6,    3,    5),
    munsell_chroma_moist       = c(1,    3,    4,    3),
    munsell_value_dry          = c(3,    7,    4,    6),
    munsell_chroma_dry         = c(1,    3,    4,    3),
    structure_grade            = c("weak","weak","strong","weak"),
    structure_size             = c("fine","fine","medium","fine"),
    structure_type             = c("granular","platy","subangular blocky","massive"),
    consistence_moist          = c("loose","loose","friable","firm"),
    coarse_fragments_pct       = c(0,    5,    10,   20),
    clay_pct                   = c(8,    5,    8,    7),
    silt_pct                   = c(20,   10,   12,   13),
    sand_pct                   = c(72,   85,   80,   80),
    ph_h2o                     = c(4.2,  4.0,  4.5,  4.8),
    ph_kcl                     = c(3.5,  3.4,  3.8,  4.0),
    oc_pct                     = c(35,   0.5,  1.5,  0.4),
    cec_cmol                   = c(60,   3,    5,    4),
    ca_cmol                    = c(2,    0.1,  0.2,  0.2),
    mg_cmol                    = c(1,    0.05, 0.1,  0.1),
    k_cmol                     = c(0.5,  0.05, 0.05, 0.05),
    na_cmol                    = c(0.1,  0.05, 0.05, 0.05),
    al_cmol                    = c(3,    1,    2,    1),
    bs_pct                     = c(6,    8,    8,    10),
    al_sat_pct                 = c(45,   80,   83,   70),
    al_ox_pct                  = c(0.05, 0.05, 0.40, 0.10),
    fe_ox_pct                  = c(0.05, 0.05, 0.40, 0.10),
    plinthite_pct              = c(0,    0,    0,    0),
    redoximorphic_features_pct = c(0,    0,    0,    0),
    slickensides               = c("absent","absent","absent","absent"),
    bulk_density_g_cm3         = c(0.40, 1.40, 1.30, 1.50)
  )

  hz <- ensure_horizon_schema(hz)

  PedonRecord$new(
    site = list(
      id              = "PZ-canonical-01",
      lat             = 60.5,
      lon             = 17.5,
      crs             = 4326,
      date            = as.Date("2023-09-30"),
      country         = "SE",
      parent_material = "glacial sandy till",
      elevation_m     = 95,
      slope_pct       = 5,
      land_use        = "boreal forest (managed)",
      vegetation      = "Pinus sylvestris / Vaccinium",
      drainage_class  = "well drained"
    ),
    horizons = hz
  )
}


#' Build the canonical Gleysol fixture
#'
#' Synthetic Gleysol from a high-water-table floodplain: A with low
#' chroma but no explicit redox features (so gleyic test is anchored on
#' Bg); Bg with diagnostic redoximorphic features (35\% by volume) within
#' the upper 50 cm. By construction:
#' \itemize{
#'   \item \code{\link{gleyic_properties}}: PASSES on Bg.
#'   \item \code{\link{argic}}, \code{\link{ferralic}},
#'         \code{\link{mollic}}, \code{\link{cambic}},
#'         \code{\link{plinthic}}, \code{\link{spodic}},
#'         \code{\link{calcic}}, \code{\link{gypsic}},
#'         \code{\link{salic}}: FAIL.
#' }
#'
#' @return A \code{\link{PedonRecord}}.
#' @export
make_gleysol_canonical <- function() {
  hz <- data.table::data.table(
    top_cm                     = c(0,    15,   45,   100),
    bottom_cm                  = c(15,   45,   100,  150),
    designation                = c("A",  "Bg1","Bg2","Cg"),
    munsell_hue_moist          = c("10YR","2.5Y","2.5Y","5GY"),
    munsell_value_moist        = c(3,    5,    5,    5),
    munsell_chroma_moist       = c(2,    1,    1,    1),
    munsell_value_dry          = c(4,    7,    6,    6),
    munsell_chroma_dry         = c(2,    2,    1,    1),
    structure_grade            = c("moderate","weak","weak","massive"),
    structure_size             = c("fine","medium","medium","fine"),
    structure_type             = c("granular","subangular blocky",
                                     "subangular blocky","massive"),
    consistence_moist          = c("friable","firm","firm","sticky"),
    coarse_fragments_pct       = c(2,    2,    5,    8),
    clay_pct                   = c(28,   30,   32,   30),
    silt_pct                   = c(45,   45,   42,   40),
    sand_pct                   = c(27,   25,   26,   30),
    ph_h2o                     = c(5.5,  5.8,  6.0,  6.2),
    ph_kcl                     = c(4.8,  5.1,  5.3,  5.5),
    oc_pct                     = c(3.0,  0.8,  0.4,  0.2),
    cec_cmol                   = c(22,   18,   16,   14),
    ca_cmol                    = c(7,    8,    8,    8),
    mg_cmol                    = c(2,    2,    2,    2),
    k_cmol                     = c(0.4,  0.3,  0.3,  0.3),
    na_cmol                    = c(0.2,  0.2,  0.2,  0.2),
    al_cmol                    = c(0.5,  0.4,  0.3,  0.2),
    bs_pct                     = c(43,   58,   65,   75),
    al_sat_pct                 = c(5,    4,    3,    2),
    plinthite_pct              = c(0,    0,    0,    0),
    redoximorphic_features_pct = c(2,    35,   40,   30),
    slickensides               = c("absent","absent","absent","absent"),
    bulk_density_g_cm3         = c(1.15, 1.40, 1.50, 1.55)
  )

  hz <- ensure_horizon_schema(hz)

  PedonRecord$new(
    site = list(
      id              = "GL-canonical-01",
      lat             = 52.0,
      lon             = 5.5,
      crs             = 4326,
      date            = as.Date("2023-06-18"),
      country         = "NL",
      parent_material = "Holocene fluvial clay",
      elevation_m     = 1,
      slope_pct       = 0,
      land_use        = "intensive grassland",
      vegetation      = "Lolium perenne / Trifolium",
      drainage_class  = "poorly drained (high groundwater)"
    ),
    horizons = hz
  )
}


#' Build the canonical Vertisol fixture
#'
#' Synthetic Vertisol from a smectite-rich plain: deep clay (50-55\%)
#' with strong slickensides in the Bss horizon. Surface chroma 4
#' (above the mollic cap) so that vertic_properties is the only v0.2
#' diagnostic that passes. By construction:
#' \itemize{
#'   \item \code{\link{vertic_properties}}: PASSES on Bss and BC.
#'   \item \code{\link{argic}}, \code{\link{ferralic}},
#'         \code{\link{mollic}}, \code{\link{cambic}},
#'         \code{\link{plinthic}}, \code{\link{spodic}},
#'         \code{\link{calcic}}, \code{\link{gypsic}},
#'         \code{\link{salic}}: FAIL.
#' }
#'
#' @return A \code{\link{PedonRecord}}.
#' @export
make_vertisol_canonical <- function() {
  hz <- data.table::data.table(
    top_cm                     = c(0,    25,   80),
    bottom_cm                  = c(25,   80,   150),
    designation                = c("Aw", "Bss","BCss"),
    munsell_hue_moist          = c("10YR","10YR","10YR"),
    munsell_value_moist        = c(3,    4,    4),
    munsell_chroma_moist       = c(4,    3,    3),
    munsell_value_dry          = c(5,    5,    5),
    munsell_chroma_dry         = c(3,    3,    3),
    structure_grade            = c("strong","strong","moderate"),
    structure_size             = c("medium","coarse","coarse"),
    structure_type             = c("subangular blocky","wedge-shaped",
                                     "wedge-shaped"),
    consistence_moist          = c("firm","very firm","very firm"),
    coarse_fragments_pct       = c(0,    0,    2),
    clay_pct                   = c(50,   55,   52),
    silt_pct                   = c(30,   28,   30),
    sand_pct                   = c(20,   17,   18),
    ph_h2o                     = c(7.0,  7.2,  7.5),
    ph_kcl                     = c(6.4,  6.6,  6.9),
    oc_pct                     = c(1.5,  0.6,  0.3),
    cec_cmol                   = c(45,   48,   45),
    ca_cmol                    = c(28,   30,   28),
    mg_cmol                    = c(7,    8,    8),
    k_cmol                     = c(0.6,  0.4,  0.3),
    na_cmol                    = c(0.4,  0.5,  0.6),
    al_cmol                    = c(0,    0,    0),
    bs_pct                     = c(80,   81,   82),
    al_sat_pct                 = c(0,    0,    0),
    caco3_pct                  = c(0,    1,    3),
    plinthite_pct              = c(0,    0,    0),
    redoximorphic_features_pct = c(0,    0,    0),
    slickensides               = c("absent","common","many"),
    bulk_density_g_cm3         = c(1.40, 1.55, 1.60)
  )

  hz <- ensure_horizon_schema(hz)

  PedonRecord$new(
    site = list(
      id              = "VR-canonical-01",
      lat             = -18.5,
      lon             = 35.0,
      crs             = 4326,
      date            = as.Date("2023-11-12"),
      country         = "MZ",
      parent_material = "alluvium over basalt",
      elevation_m     = 80,
      slope_pct       = 1,
      land_use        = "extensive cropping",
      vegetation      = "savanna fallow",
      drainage_class  = "imperfectly drained"
    ),
    horizons = hz
  )
}


#' Build the canonical Acrisol fixture
#'
#' Synthetic tropical-humid Acrisol on weathered gneiss: argic horizon
#' at Bt1 with low-activity clay (CEC/clay ~ 17 cmol_c/kg clay) and low
#' base saturation (BS ~ 25\%). By construction:
#' \itemize{
#'   \item \code{\link{argic}}: PASSES on Bt1.
#'   \item \code{\link{acrisol}}: PASSES (CEC low, BS low).
#'   \item \code{\link{lixisol}}, \code{\link{alisol}},
#'         \code{\link{luvisol}}: FAIL.
#'   \item Other diagnostics: FAIL.
#' }
#'
#' @return A \code{\link{PedonRecord}}.
#' @export
make_acrisol_canonical <- function() {
  hz <- data.table::data.table(
    top_cm                     = c(0,    15,   30,   70),
    bottom_cm                  = c(15,   30,   70,   150),
    designation                = c("A",  "E",  "Bt1","Bt2"),
    munsell_hue_moist          = c("10YR","10YR","5YR","5YR"),
    munsell_value_moist        = c(4,    5,    4,    4),
    munsell_chroma_moist       = c(3,    3,    6,    6),
    munsell_value_dry          = c(5,    6,    5,    5),
    munsell_chroma_dry         = c(3,    3,    6,    6),
    structure_grade            = c("moderate","weak","strong","strong"),
    structure_size             = c("fine","fine","medium","medium"),
    structure_type             = c("granular","platy","subangular blocky",
                                     "subangular blocky"),
    consistence_moist          = c("friable","friable","firm","firm"),
    clay_films                 = c(NA_character_, NA_character_, "common", "common"),
    coarse_fragments_pct       = c(2,    2,    5,    8),
    clay_pct                   = c(22,   18,   35,   38),
    silt_pct                   = c(30,   28,   25,   22),
    sand_pct                   = c(48,   54,   40,   40),
    ph_h2o                     = c(4.5,  4.6,  4.7,  4.8),
    ph_kcl                     = c(3.9,  4.0,  4.1,  4.2),
    oc_pct                     = c(1.5,  0.5,  0.3,  0.2),
    cec_cmol                   = c(8,    6,    6,    6.5),
    ca_cmol                    = c(0.8,  0.5,  0.6,  0.6),
    mg_cmol                    = c(0.4,  0.3,  0.4,  0.4),
    k_cmol                     = c(0.15, 0.10, 0.10, 0.10),
    na_cmol                    = c(0.05, 0.05, 0.05, 0.05),
    al_cmol                    = c(0.4,  0.5,  0.4,  0.4),
    bs_pct                     = c(18,   16,   19,   18),
    al_sat_pct                 = c(22,   33,   25,   26),
    plinthite_pct              = c(0,    0,    0,    0),
    redoximorphic_features_pct = c(0,    0,    0,    0),
    slickensides               = c("absent","absent","absent","absent"),
    bulk_density_g_cm3         = c(1.30, 1.45, 1.45, 1.50)
  )

  hz <- ensure_horizon_schema(hz)

  PedonRecord$new(
    site = list(
      id              = "AC-canonical-01",
      lat             = -20.0,
      lon             = -44.0,
      crs             = 4326,
      date            = as.Date("2024-01-15"),
      country         = "BR",
      parent_material = "weathered gneiss",
      elevation_m     = 750,
      slope_pct       = 12,
      land_use        = "Eucalyptus plantation",
      vegetation      = "Mata Atlantica residual",
      drainage_class  = "well drained"
    ),
    horizons = hz
  )
}


#' Build the canonical Lixisol fixture
#'
#' Synthetic Mediterranean / sub-tropical Lixisol on weathered
#' calcareous parent material: argic horizon at Bt1 with low-activity
#' clay (CEC/clay ~ 20) but high base saturation (BS ~ 70\%) thanks to
#' carbonate-buffered weathering. By construction:
#' \itemize{
#'   \item \code{\link{argic}}: PASSES on Bt1.
#'   \item \code{\link{lixisol}}: PASSES (CEC low, BS high).
#'   \item \code{\link{acrisol}}, \code{\link{alisol}},
#'         \code{\link{luvisol}}: FAIL.
#' }
#'
#' @return A \code{\link{PedonRecord}}.
#' @export
make_lixisol_canonical <- function() {
  hz <- data.table::data.table(
    top_cm                     = c(0,    15,   30,   70),
    bottom_cm                  = c(15,   30,   70,   150),
    designation                = c("A",  "E",  "Bt1","Bt2"),
    munsell_hue_moist          = c("7.5YR","7.5YR","5YR","5YR"),
    munsell_value_moist        = c(4,    5,    4,    4),
    munsell_chroma_moist       = c(3,    3,    6,    6),
    munsell_value_dry          = c(5,    6,    5,    5),
    munsell_chroma_dry         = c(3,    3,    6,    6),
    structure_grade            = c("moderate","weak","strong","strong"),
    structure_size             = c("fine","fine","medium","medium"),
    structure_type             = c("granular","platy","subangular blocky",
                                     "subangular blocky"),
    consistence_moist          = c("friable","friable","firm","firm"),
    clay_films                 = c(NA_character_, NA_character_, "common", "many"),
    coarse_fragments_pct       = c(5,    5,    8,    10),
    clay_pct                   = c(22,   18,   35,   38),
    silt_pct                   = c(30,   28,   25,   22),
    sand_pct                   = c(48,   54,   40,   40),
    ph_h2o                     = c(6.0,  6.1,  6.2,  6.4),
    ph_kcl                     = c(5.4,  5.5,  5.6,  5.8),
    oc_pct                     = c(1.5,  0.5,  0.3,  0.2),
    cec_cmol                   = c(9,    7,    7,    8),
    ca_cmol                    = c(5.0,  4.0,  4.0,  5.0),
    mg_cmol                    = c(1.5,  1.0,  1.0,  1.0),
    k_cmol                     = c(0.4,  0.3,  0.3,  0.3),
    na_cmol                    = c(0.1,  0.1,  0.1,  0.1),
    al_cmol                    = c(0,    0,    0,    0),
    bs_pct                     = c(78,   77,   77,   80),
    al_sat_pct                 = c(0,    0,    0,    0),
    plinthite_pct              = c(0,    0,    0,    0),
    redoximorphic_features_pct = c(0,    0,    0,    0),
    slickensides               = c("absent","absent","absent","absent"),
    bulk_density_g_cm3         = c(1.30, 1.45, 1.50, 1.55)
  )

  hz <- ensure_horizon_schema(hz)

  PedonRecord$new(
    site = list(
      id              = "LX-canonical-01",
      lat             = 35.0,
      lon             = -7.5,
      crs             = 4326,
      date            = as.Date("2023-10-08"),
      country         = "PT",
      parent_material = "weathered marl over limestone",
      elevation_m     = 280,
      slope_pct       = 6,
      land_use        = "vineyard",
      vegetation      = "Mediterranean (degraded)",
      drainage_class  = "well drained"
    ),
    horizons = hz
  )
}


#' Build the canonical Alisol fixture
#'
#' Synthetic humid-tropical Alisol on weathered shale: argic horizon
#' at Bt1 with high-activity clay (CEC/clay ~ 34) AND high Al
#' saturation (Al sat ~ 70\%); the canonical "young weathering on a 2:1
#' clay parent that has not yet released enough Al into the
#' precipitate-stabilised pool". By construction:
#' \itemize{
#'   \item \code{\link{argic}}: PASSES on Bt1.
#'   \item \code{\link{alisol}}: PASSES (CEC high, Al sat high).
#'   \item \code{\link{acrisol}}, \code{\link{lixisol}},
#'         \code{\link{luvisol}}: FAIL.
#' }
#'
#' @return A \code{\link{PedonRecord}}.
#' @export
make_alisol_canonical <- function() {
  hz <- data.table::data.table(
    top_cm                     = c(0,    15,   30,   70),
    bottom_cm                  = c(15,   30,   70,   150),
    designation                = c("A",  "E",  "Bt1","Bt2"),
    munsell_hue_moist          = c("10YR","10YR","7.5YR","7.5YR"),
    munsell_value_moist        = c(4,    5,    4,    4),
    munsell_chroma_moist       = c(3,    3,    4,    4),
    munsell_value_dry          = c(5,    6,    5,    5),
    munsell_chroma_dry         = c(3,    3,    4,    4),
    structure_grade            = c("moderate","weak","strong","strong"),
    structure_size             = c("fine","fine","medium","medium"),
    structure_type             = c("granular","platy","subangular blocky",
                                     "subangular blocky"),
    consistence_moist          = c("friable","friable","firm","firm"),
    clay_films                 = c(NA_character_, NA_character_, "common", "common"),
    coarse_fragments_pct       = c(3,    3,    6,    8),
    clay_pct                   = c(22,   18,   35,   38),
    silt_pct                   = c(30,   28,   25,   22),
    sand_pct                   = c(48,   54,   40,   40),
    ph_h2o                     = c(4.2,  4.3,  4.3,  4.4),
    ph_kcl                     = c(3.6,  3.7,  3.7,  3.8),
    oc_pct                     = c(2.0,  0.7,  0.5,  0.3),
    cec_cmol                   = c(14,   11,   12,   13),
    ca_cmol                    = c(0.8,  0.6,  0.8,  0.8),
    mg_cmol                    = c(0.4,  0.3,  0.4,  0.4),
    k_cmol                     = c(0.15, 0.10, 0.10, 0.10),
    na_cmol                    = c(0.05, 0.05, 0.05, 0.05),
    al_cmol                    = c(3.5,  3.0,  4.0,  4.5),
    bs_pct                     = c(10,   9,    11,   10),
    al_sat_pct                 = c(72,   74,   75,   77),
    plinthite_pct              = c(0,    0,    0,    0),
    redoximorphic_features_pct = c(0,    0,    0,    0),
    slickensides               = c("absent","absent","absent","absent"),
    bulk_density_g_cm3         = c(1.25, 1.40, 1.45, 1.50)
  )

  hz <- ensure_horizon_schema(hz)

  PedonRecord$new(
    site = list(
      id              = "AL-canonical-01",
      lat             = -8.0,
      lon             = -78.0,
      crs             = 4326,
      date            = as.Date("2023-12-03"),
      country         = "PE",
      parent_material = "weathered Cretaceous shale",
      elevation_m     = 1850,
      slope_pct       = 22,
      land_use        = "subsistence cropping",
      vegetation      = "montane forest residual",
      drainage_class  = "well drained"
    ),
    horizons = hz
  )
}


#' Build the canonical Kastanozem fixture
#'
#' Synthetic continental-semiarid Kastanozem on loess-like substrate:
#' mollic surface (chroma 3, value 3) -- dark enough for mollic but
#' not dark enough for Chernozem (chroma 3 > 2 in the upper 20 cm);
#' secondary carbonates accumulating in the Bk. By construction:
#' \itemize{
#'   \item \code{\link{mollic}}: PASSES.
#'   \item \code{\link{kastanozem}}: PASSES.
#'   \item \code{\link{chernozem}}, \code{\link{phaeozem}}: FAIL.
#' }
#'
#' @return A \code{\link{PedonRecord}}.
#' @export
make_kastanozem_canonical <- function() {
  hz <- data.table::data.table(
    top_cm                     = c(0,    25,   60,   100),
    bottom_cm                  = c(25,   60,   100,  150),
    designation                = c("A",  "AB", "Bk", "Ck"),
    munsell_hue_moist          = c("10YR","10YR","10YR","10YR"),
    munsell_value_moist        = c(3,    4,    5,    5),
    munsell_chroma_moist       = c(3,    3,    3,    3),
    munsell_value_dry          = c(5,    6,    7,    7),
    munsell_chroma_dry         = c(3,    3,    3,    3),
    structure_grade            = c("strong","moderate","weak","weak"),
    structure_size             = c("medium","medium","medium","medium"),
    structure_type             = c("granular","subangular blocky",
                                     "subangular blocky","massive"),
    consistence_moist          = c("friable","friable","firm","firm"),
    coarse_fragments_pct       = c(0,    0,    0,    2),
    clay_pct                   = c(22,   23,   23,   22),
    silt_pct                   = c(48,   48,   48,   48),
    sand_pct                   = c(30,   29,   29,   30),
    ph_h2o                     = c(7.0,  7.4,  7.9,  8.2),
    ph_kcl                     = c(6.5,  6.9,  7.3,  7.5),
    oc_pct                     = c(2.0,  1.0,  0.5,  0.2),
    cec_cmol                   = c(22,   20,   18,   16),
    ca_cmol                    = c(15,   14,   14,   13),
    mg_cmol                    = c(3,    3,    3,    2.5),
    k_cmol                     = c(0.4,  0.3,  0.2,  0.2),
    na_cmol                    = c(0.1,  0.1,  0.1,  0.1),
    al_cmol                    = c(0,    0,    0,    0),
    bs_pct                     = c(84,   87,   97,   99),
    al_sat_pct                 = c(0,    0,    0,    0),
    caco3_pct                  = c(0,    0,    7,    14),
    plinthite_pct              = c(0,    0,    0,    0),
    redoximorphic_features_pct = c(0,    0,    0,    0),
    slickensides               = c("absent","absent","absent","absent"),
    bulk_density_g_cm3         = c(1.20, 1.30, 1.40, 1.45)
  )

  hz <- ensure_horizon_schema(hz)

  PedonRecord$new(
    site = list(
      id              = "KS-canonical-01",
      lat             = 50.5,
      lon             = 53.5,
      crs             = 4326,
      date            = as.Date("2023-07-01"),
      country         = "KZ",
      parent_material = "calcareous loess",
      elevation_m     = 250,
      slope_pct       = 1,
      land_use        = "extensive grain cropping",
      vegetation      = "former dry steppe",
      drainage_class  = "well drained"
    ),
    horizons = hz
  )
}


#' Build the canonical Phaeozem fixture
#'
#' Synthetic humid-temperate Phaeozem on non-calcareous loess: mollic
#' (chroma 2, value 2-3) and high BS, but no secondary carbonates
#' anywhere -- typical of more leached / less-arid steppe-forest
#' transition. By construction:
#' \itemize{
#'   \item \code{\link{mollic}}: PASSES.
#'   \item \code{\link{phaeozem}}: PASSES.
#'   \item \code{\link{chernozem}}, \code{\link{kastanozem}}: FAIL
#'         (no carbonates).
#' }
#'
#' @return A \code{\link{PedonRecord}}.
#' @export
make_phaeozem_canonical <- function() {
  hz <- data.table::data.table(
    top_cm                     = c(0,    30,   60,   100),
    bottom_cm                  = c(30,   60,   100,  150),
    designation                = c("Ah", "AB", "Bw", "C"),
    munsell_hue_moist          = c("10YR","10YR","10YR","10YR"),
    munsell_value_moist        = c(2,    3,    4,    5),
    munsell_chroma_moist       = c(2,    2,    3,    4),
    munsell_value_dry          = c(3,    4,    5,    6),
    munsell_chroma_dry         = c(2,    2,    3,    4),
    structure_grade            = c("strong","moderate","weak","weak"),
    structure_size             = c("medium","medium","medium","medium"),
    structure_type             = c("granular","subangular blocky",
                                     "subangular blocky","massive"),
    consistence_moist          = c("friable","friable","firm","firm"),
    coarse_fragments_pct       = c(0,    0,    2,    5),
    clay_pct                   = c(25,   26,   27,   25),
    silt_pct                   = c(50,   50,   48,   48),
    sand_pct                   = c(25,   24,   25,   27),
    ph_h2o                     = c(6.5,  6.7,  6.8,  6.9),
    ph_kcl                     = c(5.9,  6.1,  6.2,  6.3),
    oc_pct                     = c(4.0,  2.0,  0.8,  0.3),
    cec_cmol                   = c(28,   25,   22,   20),
    ca_cmol                    = c(18,   16,   15,   14),
    mg_cmol                    = c(4,    4,    4,    3.5),
    k_cmol                     = c(0.5,  0.4,  0.3,  0.2),
    na_cmol                    = c(0.1,  0.1,  0.1,  0.1),
    al_cmol                    = c(0,    0,    0,    0),
    bs_pct                     = c(81,   82,   88,   89),
    al_sat_pct                 = c(0,    0,    0,    0),
    caco3_pct                  = c(0,    0,    0,    0),
    plinthite_pct              = c(0,    0,    0,    0),
    redoximorphic_features_pct = c(0,    0,    0,    0),
    slickensides               = c("absent","absent","absent","absent"),
    bulk_density_g_cm3         = c(1.10, 1.20, 1.30, 1.40)
  )

  hz <- ensure_horizon_schema(hz)

  PedonRecord$new(
    site = list(
      id              = "PH-canonical-01",
      lat             = 39.5,
      lon             = -88.0,
      crs             = 4326,
      date            = as.Date("2023-09-22"),
      country         = "US",
      parent_material = "leached loess",
      elevation_m     = 220,
      slope_pct       = 2,
      land_use        = "corn-soybean rotation",
      vegetation      = "former tall-grass prairie / oak savanna",
      drainage_class  = "well drained"
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
