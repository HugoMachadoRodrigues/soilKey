# ============================================================================
# SiBCS 5a edicao -- 13 fixtures canonicas (um perfil por ordem)
#
# Cada perfil e construido para satisfazer o gate canonico do 1o nivel
# da classe alvo, e somente esse gate -- isto verifica que (a) a chave
# atribui corretamente e (b) a ordem na chave nao ha "captura" cruzada.
# ============================================================================


.build_sibcs_pedon <- function(id, lat, lon, parent_material, hz) {
  hz <- ensure_horizon_schema(hz)
  PedonRecord$new(
    site = list(
      id = id, lat = lat, lon = lon, country = "BR",
      crs = 4326, parent_material = parent_material
    ),
    horizons = hz
  )
}


# ---- Organossolos ----------------------------------------------------------

#' Perfil canonico de Organossolo (SiBCS 5a ed., Cap 14)
#'
#' Solo organico saturado, com horizonte H histico >= 60 cm e SOC
#' alto. Tipico de varzea / brejo.
#' @export
make_organossolo_canonical <- function() {
  hz <- data.table::data.table(
    top_cm    = c(0,    20,   50,   80),
    bottom_cm = c(20,   50,   80,   150),
    designation = c("H1", "H2", "H3", "Cg"),
    munsell_hue_moist    = c("10YR","10YR","10YR","2.5Y"),
    munsell_value_moist  = c(2,    2,    3,    5),
    munsell_chroma_moist = c(1,    1,    2,    1),
    structure_grade      = c("weak","weak","massive","massive"),
    structure_type       = c("granular","granular","massive","massive"),
    consistence_moist    = c("friable","friable","firm","firm"),
    clay_pct             = c(15,   15,   10,   10),
    silt_pct             = c(35,   35,   30,   30),
    sand_pct             = c(50,   50,   60,   60),
    ph_h2o               = c(4.5,  4.6,  4.8,  5.0),
    oc_pct               = c(35,   28,   18,   2),    # 350, 280, 180 g/kg -> histico
    cec_cmol             = c(45,   40,   25,   8),
    bs_pct               = c(25,   22,   30,   40),
    redoximorphic_features_pct = c(0, 0, 5, 30)
  )
  .build_sibcs_pedon("O-canonical-01", -22.5, -43.5, "alluvium / peat", hz)
}


# ---- Neossolos -------------------------------------------------------------

#' Perfil canonico de Neossolo Litolico (SiBCS 5a ed., Cap 12)
#'
#' Solo raso sobre rocha continua dura. Sem horizonte B diagnostico.
#' @export
make_neossolo_canonical <- function() {
  hz <- data.table::data.table(
    top_cm    = c(0,    25),
    bottom_cm = c(25,   80),
    designation = c("A", "R"),
    munsell_hue_moist    = c("10YR","10YR"),
    munsell_value_moist  = c(3,    NA),
    munsell_chroma_moist = c(2,    NA),
    structure_grade      = c("weak", NA),
    clay_pct             = c(20,   NA),
    silt_pct             = c(40,   NA),
    sand_pct             = c(40,   NA),
    ph_h2o               = c(5.5,  NA),
    oc_pct               = c(2.0,  NA),
    cec_cmol             = c(8,    NA),
    bs_pct               = c(45,   NA)
  )
  .build_sibcs_pedon("R-canonical-01", -10.0, -45.0, "granito", hz)
}


# ---- Vertissolos -----------------------------------------------------------

#' Perfil canonico de Vertissolo (SiBCS 5a ed., Cap 17)
#'
#' Solo argiloso (>= 30% argila desde superficie) com horizonte vertico
#' (slickensides + fendas + clay alto) iniciando dentro de 100 cm.
#' Reusa structure / fixture do WRB Vertisol.
#' @export
make_vertissolo_canonical <- function() {
  pr <- make_vertisol_canonical()
  pr$site$id <- "V-canonical-01"
  pr$site$country <- "BR"
  pr
}


# ---- Espodossolos ----------------------------------------------------------

#' Perfil canonico de Espodossolo (SiBCS 5a ed., Cap 8)
#'
#' Reusa fixture WRB Podzol -- B espodico imediatamente abaixo de E.
#' @export
make_espodossolo_canonical <- function() {
  pr <- make_podzol_canonical()
  pr$site$id <- "E-canonical-01"
  pr$site$country <- "BR"
  pr
}


# ---- Planossolos -----------------------------------------------------------

#' Perfil canonico de Planossolo (SiBCS 5a ed., Cap 15)
#'
#' Solo com horizonte E sobrejacente a B planico (mudanca textural
#' abrupta + cores neutras + cromas baixos).
#' @export
make_planossolo_canonical <- function() {
  hz <- data.table::data.table(
    top_cm    = c(0,    25,   50,   100),
    bottom_cm = c(25,   50,   100,  180),
    designation = c("A", "E", "Btn", "C"),
    munsell_hue_moist    = c("10YR","10YR","10YR","10YR"),
    munsell_value_moist  = c(3,    5,    5,    6),
    munsell_chroma_moist = c(2,    2,    2,    2),    # cromas baixos
    munsell_hue_dry      = c("10YR","10YR","10YR","10YR"),
    munsell_value_dry    = c(5,    7,    6,    7),
    munsell_chroma_dry   = c(2,    1,    1,    2),
    structure_grade      = c("weak","weak","strong","moderate"),
    structure_type       = c("granular","platy","prismatic","massive"),
    boundary_distinctness = c("clear","abrupt","gradual","clear"),
    clay_pct             = c(15,   15,   45,   40),    # mudanca textural abrupta
    silt_pct             = c(45,   45,   25,   25),
    sand_pct             = c(40,   40,   30,   35),
    ph_h2o               = c(5.0,  5.2,  5.5,  6.0),
    oc_pct               = c(1.2,  0.5,  0.3,  0.2),
    cec_cmol             = c(8,    5,    18,   15),
    bs_pct               = c(40,   38,   55,   60),
    redoximorphic_features_pct = c(0, 5, 10, 5)
  )
  .build_sibcs_pedon("S-canonical-01", -7.5, -39.5, "sedimentos / xisto", hz)
}


# ---- Gleissolos ------------------------------------------------------------

#' Perfil canonico de Gleissolo (SiBCS 5a ed., Cap 9)
#'
#' Reusa fixture WRB Gleysol -- horizonte glei dentro de 50 cm.
#' @export
make_gleissolo_canonical <- function() {
  pr <- make_gleysol_canonical()
  pr$site$id <- "G-canonical-01"
  pr$site$country <- "BR"
  pr
}


# ---- Latossolos ------------------------------------------------------------

#' Perfil canonico de Latossolo (SiBCS 5a ed., Cap 10)
#'
#' Reusa fixture WRB Ferralsol -- B latossolico imediatamente abaixo
#' de A, sem horizonte argilico acima.
#' @export
make_latossolo_canonical <- function() {
  pr <- make_ferralsol_canonical()
  pr$site$id <- "L-canonical-01"
  pr$site$country <- "BR"
  pr
}


# ---- Chernossolos ----------------------------------------------------------

#' Perfil canonico de Chernossolo (SiBCS 5a ed., Cap 7)
#'
#' Reusa fixture WRB Chernozem -- A chernozemico + Bk com argila Ta +
#' V alta. SiBCS strictos exigem (a) Bi/Bt + Ta + V alta, OR (b)
#' calcico/petrocalcico/carbonatico + A chernozemico.
#' @export
make_chernossolo_canonical <- function() {
  pr <- make_chernozem_canonical()
  pr$site$id <- "M-canonical-01"
  pr$site$country <- "BR"
  # SiBCS Chernossolo path B requer Ck cálcico/petrocálcico/carbonático
  # (CaCO3 >= 15%). Enriquece o ultimo horizonte para alcancar o gate.
  n <- nrow(pr$horizons)
  pr$horizons$caco3_pct[n] <- 18    # Ck carbonatico
  pr
}


# ---- Cambissolos -----------------------------------------------------------

#' Perfil canonico de Cambissolo (SiBCS 5a ed., Cap 6)
#'
#' Reusa fixture WRB Cambisol -- B incipiente sem ser plintico,
#' vertico, planico, etc.
#' @export
make_cambissolo_canonical <- function() {
  pr <- make_cambisol_canonical()
  pr$site$id <- "C-canonical-01"
  pr$site$country <- "BR"
  pr
}


# ---- Plintossolos ----------------------------------------------------------

#' Perfil canonico de Plintossolo (SiBCS 5a ed., Cap 16)
#'
#' Reusa fixture WRB Plinthosol -- horizonte plintico iniciando
#' dentro de 40 cm.
#' @export
make_plintossolo_canonical <- function() {
  pr <- make_plinthosol_canonical()
  pr$site$id <- "F-canonical-01"
  pr$site$country <- "BR"
  pr
}


# ---- Luvissolos ------------------------------------------------------------

#' Perfil canonico de Luvissolo (SiBCS 5a ed., Cap 11)
#'
#' Solo com B textural argila Ta + V alta. Tipico do semiarido com
#' rocha basica.
#' @export
make_luvissolo_canonical <- function() {
  hz <- data.table::data.table(
    top_cm    = c(0,    20,   50,   100),
    bottom_cm = c(20,   50,   100,  180),
    designation = c("A", "BA", "Bt", "C"),
    munsell_hue_moist    = c("10YR","7.5YR","5YR","10YR"),
    # A com cor moderada (NAO chernozemico): value 4 chroma 4
    munsell_value_moist  = c(4,    4,    3,    5),
    munsell_chroma_moist = c(4,    4,    4,    4),
    structure_grade      = c("moderate","strong","strong","moderate"),
    structure_type       = c("granular","subangular blocky",
                              "prismatic","massive"),
    clay_films_amount    = c(NA,   "common", "many",   NA),
    clay_pct             = c(20,   30,   45,   40),
    silt_pct             = c(40,   35,   30,   30),
    sand_pct             = c(40,   35,   25,   30),
    ph_h2o               = c(6.5,  6.8,  7.0,  7.2),
    # OC moderado (~0.8% = 8 g/kg < 25 g/kg do A chernozemico-com-CaCO3,
    # mas o gate do A chernozemico tambem exige value/chroma)
    oc_pct               = c(0.8,  0.4,  0.2,  0.1),
    cec_cmol             = c(20,   22,   25,   20),     # CTC alta -> Ta
    ca_cmol              = c(13,   14,   16,   13),
    mg_cmol              = c(4,    5,    6,    5),
    k_cmol               = c(0.6,  0.4,  0.3,  0.2),
    na_cmol              = c(0.4,  0.6,  0.7,  0.8),
    al_cmol              = c(0,    0,    0,    0),
    bs_pct               = c(90,   91,   93,   95),     # V alta
    boundary_distinctness = c("clear","gradual","gradual","diffuse")
  )
  .build_sibcs_pedon("T-canonical-01", -8.5, -39.0,
                       "rocha sedimentar carbonatica", hz)
}


# ---- Nitossolos ------------------------------------------------------------

#' Perfil canonico de Nitossolo Vermelho (SiBCS 5a ed., Cap 13)
#'
#' Solo argiloso (>= 35% argila desde superficie) com B nitico
#' (estrutura forte em blocos + cerosidade), gradiente textural
#' baixo (B/A <= 1.5).
#' @export
make_nitossolo_canonical <- function() {
  hz <- data.table::data.table(
    top_cm    = c(0,    20,   60,   130),
    bottom_cm = c(20,   60,   130,  200),
    designation = c("A", "BA", "Bt", "BC"),
    munsell_hue_moist    = c("2.5YR","2.5YR","2.5YR","2.5YR"),
    munsell_value_moist  = c(3,    3,    3,    4),
    munsell_chroma_moist = c(4,    6,    6,    6),
    structure_grade      = c("strong","strong","strong","moderate"),
    structure_type       = c("granular","subangular blocky",
                              "subangular blocky","subangular blocky"),
    clay_films_amount    = c(NA,   "common", "many",   "common"),
    clay_pct             = c(45,   55,   60,   55),     # argila >= 35% desde A
    silt_pct             = c(25,   23,   20,   22),
    sand_pct             = c(30,   22,   20,   23),
    ph_h2o               = c(5.0,  5.2,  5.4,  5.5),
    oc_pct               = c(2.5,  0.8,  0.4,  0.2),
    cec_cmol             = c(8,    7,    6,    5),     # baixa atividade
    bs_pct               = c(30,   25,   20,   18),
    al_cmol              = c(0.2,  0.3,  0.4,  0.5),
    fe_dcb_pct           = c(15,   17,   18,   16),
    boundary_distinctness = c("gradual","diffuse","diffuse","gradual")
  )
  .build_sibcs_pedon("N-canonical-01", -23.5, -47.0,
                       "rocha basica (basalto)", hz)
}


# ---- Argissolos (catch-all) -----------------------------------------------

#' Perfil canonico de Argissolo (SiBCS 5a ed., Cap 5)
#'
#' B textural com gradiente significativo, argila ativ baixa ou
#' alta + V baixa. Catch-all final na chave -- tipica do Brasil
#' tropical.
#' @export
make_argissolo_canonical <- function() {
  hz <- data.table::data.table(
    top_cm    = c(0,    20,   50,   120),
    bottom_cm = c(20,   50,   120,  200),
    designation = c("A", "BA", "Bt", "BC"),
    munsell_hue_moist    = c("7.5YR","5YR","2.5YR","10R"),
    munsell_value_moist  = c(4,    4,    3,    3),
    munsell_chroma_moist = c(3,    5,    6,    6),
    structure_grade      = c("moderate","moderate","strong","moderate"),
    structure_type       = c("granular","subangular blocky",
                              "subangular blocky","subangular blocky"),
    clay_films_amount    = c(NA,   "few",  "common", "common"),
    clay_pct             = c(18,   28,   45,   42),     # gradiente >> Bt
    silt_pct             = c(30,   25,   20,   22),
    sand_pct             = c(52,   47,   35,   36),
    ph_h2o               = c(5.5,  5.3,  5.0,  5.0),
    oc_pct               = c(1.5,  0.6,  0.3,  0.2),
    cec_cmol             = c(6,    5,    5,    4),     # Tb (baixa)
    bs_pct               = c(35,   25,   20,   18),    # V baixa -> distrofico
    al_cmol              = c(0.5,  0.8,  1.0,  1.2),
    boundary_distinctness = c("clear","clear","gradual","diffuse")
  )
  .build_sibcs_pedon("P-canonical-01", -22.0, -43.0,
                       "sedimento argiloso", hz)
}
