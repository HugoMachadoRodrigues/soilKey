#' Default-value-for-NULL operator
#'
#' Returns the left-hand side if it is non-NULL, otherwise the right-hand side.
#' Re-exported so that downstream code can use the same idiom soilKey itself
#' uses internally.
#'
#' @param a The candidate value.
#' @param b The fallback used when \code{a} is NULL.
#' @return Either \code{a} or \code{b}.
#' @name grapes-or-or-grapes
#' @export
`%||%` <- function(a, b) if (is.null(a)) b else a

#' Canonical horizon column specification
#'
#' Returns the schema for the \code{horizons} \code{data.table} carried by a
#' \code{\link{PedonRecord}}: an ordered named list mapping column names to
#' their R type (\code{"numeric"} or \code{"character"}). Adding a new
#' attribute means editing this single function.
#'
#' @return Named list of column types in canonical order.
#' @keywords internal
horizon_column_spec <- function() {
  list(
    # ---- geometry & boundaries ----
    top_cm                  = "numeric",
    bottom_cm               = "numeric",
    designation             = "character",
    boundary_distinctness   = "character",
    boundary_topography     = "character",
    # ---- color (Munsell) ----
    munsell_hue_moist       = "character",
    munsell_value_moist     = "numeric",
    munsell_chroma_moist    = "numeric",
    munsell_hue_dry         = "character",
    munsell_value_dry       = "numeric",
    munsell_chroma_dry      = "numeric",
    # ---- structure & consistence ----
    structure_grade         = "character",
    structure_size          = "character",
    structure_type          = "character",
    consistence_moist       = "character",
    consistence_wet         = "character",
    clay_films_amount       = "character",
    clay_films_strength     = "character",
    coarse_fragments_pct    = "numeric",
    # ---- texture ----
    clay_pct                = "numeric",
    silt_pct                = "numeric",
    sand_pct                = "numeric",
    # ---- acidity ----
    ph_h2o                  = "numeric",
    ph_kcl                  = "numeric",
    ph_cacl2                = "numeric",
    # ---- organics ----
    oc_pct                  = "numeric",
    n_total_pct             = "numeric",
    # ---- exchange complex ----
    cec_cmol                = "numeric",
    ecec_cmol               = "numeric",
    bs_pct                  = "numeric",
    al_sat_pct              = "numeric",
    ca_cmol                 = "numeric",
    mg_cmol                 = "numeric",
    k_cmol                  = "numeric",
    na_cmol                 = "numeric",
    al_cmol                 = "numeric",
    # ---- carbonates / sulphates ----
    caco3_pct               = "numeric",
    caso4_pct               = "numeric",
    # ---- iron / aluminum oxides ----
    fe_dcb_pct              = "numeric",
    fe_ox_pct               = "numeric",
    al_ox_pct               = "numeric",
    si_ox_pct               = "numeric",
    # ---- physical ----
    bulk_density_g_cm3      = "numeric",
    water_content_33kpa     = "numeric",
    water_content_1500kpa   = "numeric",
    # ---- v0.2 additions: salinity, redoximorphism, vertic ----
    ec_dS_m                       = "numeric",   # electrical conductivity (saturated paste, 25C)
    plinthite_pct                 = "numeric",   # volume % of plinthite (Fe-rich nodules / mottles)
    redoximorphic_features_pct    = "numeric",   # volume % of Fe/Mn redox features
    slickensides                  = "character", # absent / few / common / many / continuous
    # ---- v0.3 additions: technic, duric ----
    artefacts_pct                 = "numeric",   # volume % of human artefacts (for Technosols)
    duripan_pct                   = "numeric",   # volume % of Si-cemented duripan (for Durisols)
    # ---- v0.3.3 additions: completing WRB Ch 3.1 / 3.2 / 3.3 coverage ----
    cementation_class             = "character", # 'none' / 'weakly' / 'moderately' / 'strongly' / 'indurated' (for petric variants)
    p_mehlich3_mg_kg              = "numeric",   # plant-available P (anthric / hortic / pretic / ornithogenic)
    worm_holes_pct                = "numeric",   # volume % of worm holes / casts / coprolites (chernic / vermic / arenicolic)
    water_dispersible_clay_pct    = "numeric",   # WDC / total clay (Ferralsols 'activic' check)
    sulfidic_s_pct                = "numeric",   # inorganic sulfidic S (hypersulfidic / hyposulfidic / thionic)
    volcanic_glass_pct            = "numeric",   # % volcanic glass in 0.02-2 mm fraction (vitric / tephric)
    phosphate_retention_pct       = "numeric",   # P retention (vitric / andic threshold)
    artefacts_industrial_pct      = "numeric",   # subset of artefacts: industrial-process glasses, slag, ash (organotechnic)
    artefacts_urbic_pct           = "numeric",   # subset of artefacts: rubble / refuse from human settlements (Technosols urbic)
    rock_origin                   = "character", # 'fluviatile' / 'marine' / 'lacustrine' / 'aeolian' / 'colluvial' / 'pyroclastic' / NA
    permafrost_temp_C             = "numeric",   # mean annual soil temp at this depth (gelic / cryic)
    cracks_width_cm               = "numeric",   # width of shrink-swell cracks when soil dry (vertic horizon / shrink_swell_cracks)
    cracks_depth_cm               = "numeric",   # depth to which the crack extends from the surface
    polygonal_cracks_spacing_cm   = "numeric",   # avg horizontal spacing of polygonal cracks (takyric properties)
    desert_pavement_pct           = "numeric",   # % surface coverage by coarse fragments (yermic properties)
    varnish_pct                   = "numeric",   # % of coarse fragments with desert varnish (yermic)
    ventifact_pct                 = "numeric",   # % of coarse fragments wind-shaped (yermic)
    vesicular_pores               = "character", # 'absent' / 'few' / 'common' / 'many' (yermic)
    rupture_resistance            = "character", # 'loose' / 'soft' / 'slightly hard' / 'hard' / 'very hard' / 'extremely hard'
    plasticity                    = "character", # 'non-plastic' / 'slightly plastic' / 'moderately plastic' / 'very plastic'
    al_kcl_cmol                   = "numeric",   # KCl-extractable Al (Alisols criterion)
    layer_origin                  = "character", # 'aeolic' / 'fluvic' / 'solimovic' / 'tephric' / 'organic' etc (for material gating)
    # ---- v0.7.2 additions: SiBCS pendentes (von Post, Ki/Kr, COLE, sulfuric attack) ----
    fiber_content_rubbed_pct      = "numeric",   # SiBCS Cap 14: % fibras apos esfregamento (Saprico < 17, Hemico 17-40, Fibrico >= 40)
    fiber_content_unrubbed_pct    = "numeric",   # SiBCS Cap 14: % fibras antes do esfregamento (auxiliar)
    von_post_index                = "integer",   # Indice de decomposicao von Post 1924 (H1-H10): H1-H4 Fibrico / H5-H6 Hemico / H7-H10 Saprico
    cole_value                    = "numeric",   # Coefficient of Linear Extensibility (1500 kPa moist -> oven dry); SiBCS retratil >= 0.06
    sio2_sulfuric_pct             = "numeric",   # SiO2 por ataque sulfurico-NaOH (Embrapa Manual de Metodos), para Ki/Kr
    al2o3_sulfuric_pct            = "numeric",   # Al2O3 por ataque sulfurico, para Ki = (SiO2/60.08)/(Al2O3/101.96) molar
    fe2o3_sulfuric_pct            = "numeric",   # Fe2O3 por ataque sulfurico, para Kr = SiO2/(Al2O3+Fe2O3) molar (Latossolos Acriferricos)
    # ---- v0.7.14.C additions: SiBCS Cap 18 mineralogia da fracao areia ----
    sand_mica_pct                 = "numeric",   # SiBCS Cap 18 p 286: % volume de micas na fracao areia (>= 15% -> Familia "micacea")
    sand_amphibole_pct            = "numeric",   # SiBCS Cap 18 p 286: % volume de anfibolios (>= 15% -> Familia "anfibolitica")
    sand_feldspar_pct             = "numeric",   # SiBCS Cap 18 p 286: % volume de feldspatos (>= 15% -> Familia "feldspatica")
    sand_mineralogy               = "character", # SiBCS Cap 18 p 286 fallback: 'micacea' / 'anfibolitica' / 'feldspatica' / 'quartzosa' / NA (atalho qualitativo)
    # ---- v0.7.14.D additions: SiBCS Cap 18 Organossolos -----------------
    woody_fragments_pct           = "numeric"    # SiBCS Cap 18 p 288: % volume de galhos/troncos >= 2 cm em horizontes organicos (Organossolos lenhosos / muito lenhosos / extremamente lenhosos)
  )
}

#' Build an empty horizons data.table with the canonical schema
#'
#' @param n Number of rows (default 0).
#' @return A \code{data.table} with all canonical horizon columns filled
#'         with NAs of the correct type.
#' @export
#' @examples
#' h <- make_empty_horizons(3)
#' nrow(h)
make_empty_horizons <- function(n = 0L) {
  n <- as.integer(n)
  spec <- horizon_column_spec()
  cols <- lapply(spec, function(type) {
    switch(type,
      numeric   = rep(NA_real_,      n),
      character = rep(NA_character_, n),
      integer   = rep(NA_integer_,   n)
    )
  })
  data.table::as.data.table(cols)
}

#' Coerce a horizons-like data.frame to the canonical schema
#'
#' Adds any missing canonical columns as NAs of the right type and reorders
#' canonical columns first. Extra user-supplied columns are preserved at the
#' end. Coerces character values to numeric where the schema requires it.
#'
#' @param h Input data.frame or data.table.
#' @return A \code{data.table}.
#' @keywords internal
ensure_horizon_schema <- function(h) {
  if (is.null(h)) return(make_empty_horizons(0L))
  if (!data.table::is.data.table(h)) h <- data.table::as.data.table(h)
  spec <- horizon_column_spec()
  n <- nrow(h)
  for (col in names(spec)) {
    if (!col %in% names(h)) {
      h[[col]] <- switch(spec[[col]],
        numeric   = rep(NA_real_,      n),
        character = rep(NA_character_, n),
        integer   = rep(NA_integer_,   n)
      )
    } else if (spec[[col]] == "numeric" && !is.numeric(h[[col]])) {
      h[[col]] <- suppressWarnings(as.numeric(h[[col]]))
    } else if (spec[[col]] == "character" && !is.character(h[[col]])) {
      h[[col]] <- as.character(h[[col]])
    }
  }
  data.table::setcolorder(h, intersect(c(names(spec), names(h)), names(h)))
  h
}

#' Empty provenance table
#'
#' @keywords internal
make_empty_provenance <- function() {
  data.table::data.table(
    horizon_idx = integer(),
    attribute   = character(),
    source      = character(),
    confidence  = numeric(),
    notes       = character()
  )
}

#' Format a numeric value with suffix, returning "NA" for NA/NULL
#'
#' @keywords internal
fmt_num <- function(x, suffix = "", digits = 1) {
  if (is.null(x)) return("NA")
  if (length(x) == 1 && is.na(x)) return("NA")
  paste0(formatC(x, format = "f", digits = digits), suffix)
}

#' Valid provenance source codes
#'
#' @keywords internal
valid_provenance_sources <- function() {
  c("measured", "extracted_vlm", "predicted_spectra",
    "inferred_prior", "user_assumed")
}

#' Authority order for provenance sources
#'
#' Higher value = more authoritative. Used when reconciling values from
#' multiple sources (e.g. measured beats predicted_spectra beats
#' extracted_vlm beats inferred_prior beats user_assumed).
#'
#' @keywords internal
provenance_authority <- function(source) {
  authority <- c(
    measured          = 5L,
    predicted_spectra = 4L,
    extracted_vlm     = 3L,
    inferred_prior    = 2L,
    user_assumed      = 1L
  )
  authority[source]
}
