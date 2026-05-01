# =============================================================================
# KSSL / NCSS GeoPackage loader (v0.9.18)
#
# The NCSS / KSSL Lab Data Mart is distributed as a GeoPackage
# (`ncss_labdata.gpkg`, ~5 GB) with the following layers:
#
#   lab_combine_nasis_ncss   -- one row per pedon, classification fields
#                                  (samp_taxorder / samp_taxsubgrp / ...)
#   lab_site                 -- one row per site, lat/lon
#   lab_layer                -- one row per layer, hzn_top/hzn_bot/hzn_desgn
#                                  + the layer_key <-> pedon_key link
#   lab_chemical_properties  -- per-layer chemistry (CEC, BS, OC, pH, ...)
#   lab_physical_properties  -- per-layer texture (clay, silt, sand, BD)
#
# This loader joins the five layers into a list of PedonRecord objects
# with `site$reference_usda` set from `samp_taxorder` (Title-Cased and
# pluralised to match `classify_usda()$rsg_or_order`).
#
# Designed for SCALE: the gpkg has ~36 000 classified pedons, ~417 000
# layers. A `head` argument limits to the first N pedons for parser
# validation; the full run takes minutes. Reads via sf::read_sf with
# the relevant gpkg layer queried as a whole (no spatial filter).
# =============================================================================


#' Load KSSL / NCSS pedons from the ncss_labdata GeoPackage
#'
#' Reads the `lab_combine_nasis_ncss` / `lab_site` / `lab_layer` /
#' `lab_chemical_properties` / `lab_physical_properties` views from
#' the NCSS Lab Data Mart GeoPackage and assembles a list of
#' \code{\link{PedonRecord}} objects. Each pedon has its USDA Soil
#' Taxonomy Order attached as \code{site$reference_usda}, normalised
#' to match `classify_usda()` output ("Mollisols", "Alfisols", ...).
#'
#' @param gpkg Path to \code{ncss_labdata.gpkg}.
#' @param head Optional integer; load only the first N classified
#'        pedons. Useful for parser validation.
#' @param require_b_horizon If \code{TRUE} (default), drops pedons
#'        whose deepest horizon's bottom_cm < 30. Most non-Entisol
#'        Order gates need a B horizon.
#' @param verbose If \code{TRUE} (default), emits progress messages.
#' @return A list of \code{\link{PedonRecord}} objects.
#' @export
load_kssl_pedons_gpkg <- function(gpkg,
                                     head              = NULL,
                                     require_b_horizon = TRUE,
                                     verbose           = TRUE) {
  if (!requireNamespace("sf", quietly = TRUE))
    stop("install.packages('sf') required to read GeoPackage")
  if (!file.exists(gpkg)) stop(sprintf("gpkg not found: %s", gpkg))

  if (verbose) cli::cli_alert_info("Reading lab_combine_nasis_ncss ...")
  combine <- data.table::as.data.table(suppressWarnings(
    sf::st_drop_geometry(sf::read_sf(gpkg, layer = "lab_combine_nasis_ncss"))
  ))
  combine <- combine[!is.na(combine$samp_taxorder) &
                       nzchar(combine$samp_taxorder), ]
  if (!is.null(head)) combine <- utils::head(combine, n = head)
  pkeys <- combine$pedon_key

  if (verbose) cli::cli_alert_info("Reading lab_site ...")
  sites <- data.table::as.data.table(suppressWarnings(
    sf::st_drop_geometry(sf::read_sf(gpkg, layer = "lab_site"))
  ))
  data.table::setnames(sites, "site_key", "site_key")
  sites <- sites[, c("site_key",
                     "latitude_std_decimal_degrees",
                     "longitude_std_decimal_degrees")]

  if (verbose) cli::cli_alert_info("Reading lab_layer ...")
  layers <- data.table::as.data.table(suppressWarnings(
    sf::st_drop_geometry(sf::read_sf(gpkg, layer = "lab_layer"))
  ))
  layers <- layers[layers$pedon_key %in% pkeys, ]
  layers <- layers[, c("layer_key","pedon_key","layer_sequence",
                       "hzn_top","hzn_bot","hzn_desgn")]

  if (verbose) cli::cli_alert_info("Reading lab_physical_properties ...")
  phys <- data.table::as.data.table(suppressWarnings(
    sf::st_drop_geometry(sf::read_sf(gpkg, layer = "lab_physical_properties"))
  ))
  phys <- phys[phys$layer_key %in% layers$layer_key, ]
  keep_phys <- intersect(c("layer_key","clay_total","silt_total","sand_total",
                              "bulk_density_third_bar","bulk_density_oven_dry",
                              # v0.9.19: COLE for vertic LE-based detection.
                              "cole_whole_soil"),
                            names(phys))
  phys <- phys[, keep_phys, with = FALSE]

  if (verbose) cli::cli_alert_info("Reading lab_chemical_properties ...")
  chem <- data.table::as.data.table(suppressWarnings(
    sf::st_drop_geometry(sf::read_sf(gpkg, layer = "lab_chemical_properties"))
  ))
  chem <- chem[chem$layer_key %in% layers$layer_key, ]
  keep_chem <- intersect(
    c("layer_key","cec_nh4_ph_7","ca_nh4_ph_7","mg_nh4_ph_7","na_nh4_ph_7",
      "k_nh4_ph_7","aluminum_kcl_extractable",
      "ph_h2o","ph_kcl","ph_cacl2",
      "total_carbon_ncs","organic_carbon_walkley_black",
      "base_sat_nh4oac_ph_7","caco3_lt_2_mm",
      "aluminum_saturation",
      "aluminum_dithionite_citrate",
      # v0.9.19: oxalate + pyrophosphate fields needed for the
      # spodic / andic / vitric horizon tests.
      "aluminum_ammonium_oxalate",
      "fe_ammoniumoxalate_extractable",
      "silica_ammonium_oxalate",
      "phosphorus_ammonium_oxalate",
      "aluminum_na_pyro_phosphate",
      "iron_sodium_pyro_phosphate",
      "carbon_sodium_pyro_phosphate"),
    names(chem))
  chem <- chem[, keep_chem, with = FALSE]

  layers_full <- merge(layers, phys, by = "layer_key", all.x = TRUE)
  layers_full <- merge(layers_full, chem, by = "layer_key", all.x = TRUE)
  data.table::setkeyv(layers_full, c("pedon_key", "layer_sequence"))
  combine <- merge(combine, sites, by = "site_key", all.x = TRUE)

  if (verbose)
    cli::cli_alert_info("Assembling {.val {length(pkeys)}} PedonRecord objects ...")
  out <- vector("list", length(pkeys))
  for (i in seq_along(pkeys)) {
    pk <- pkeys[i]
    p_layers <- layers_full[layers_full$pedon_key == pk, ]
    if (nrow(p_layers) == 0L) next
    p_layers <- p_layers[order(p_layers$hzn_top), ]
    if (require_b_horizon) {
      max_bot <- max(p_layers$hzn_bot, na.rm = TRUE)
      if (!is.finite(max_bot) || max_bot < 30) next
    }
    rowi <- combine[combine$pedon_key == pk, ][1, ]

    pull <- function(col) {
      if (col %in% names(p_layers)) as.numeric(p_layers[[col]])
      else rep(NA_real_, nrow(p_layers))
    }
    hz <- data.table::data.table(
      top_cm      = as.numeric(p_layers$hzn_top),
      bottom_cm   = as.numeric(p_layers$hzn_bot),
      designation = as.character(p_layers$hzn_desgn),
      clay_pct    = pull("clay_total"),
      silt_pct    = pull("silt_total"),
      sand_pct    = pull("sand_total"),
      ph_h2o      = pull("ph_h2o"),
      ph_kcl      = pull("ph_kcl"),
      ph_cacl2    = pull("ph_cacl2"),
      oc_pct      = .pick_first_non_na(pull("organic_carbon_walkley_black"),
                                          pull("total_carbon_ncs")),
      cec_cmol    = pull("cec_nh4_ph_7"),
      bs_pct      = pull("base_sat_nh4oac_ph_7"),
      ca_cmol     = pull("ca_nh4_ph_7"),
      mg_cmol     = pull("mg_nh4_ph_7"),
      k_cmol      = pull("k_nh4_ph_7"),
      na_cmol     = pull("na_nh4_ph_7"),
      al_cmol     = pull("aluminum_kcl_extractable"),
      al_sat_pct  = pull("aluminum_saturation"),
      caco3_pct   = pull("caco3_lt_2_mm"),
      al_ox_pct   = pull("aluminum_ammonium_oxalate"),
      fe_ox_pct   = pull("fe_ammoniumoxalate_extractable"),
      si_ox_pct   = pull("silica_ammonium_oxalate"),
      cole_value  = pull("cole_whole_soil"),
      bulk_density_g_cm3 = .pick_first_non_na(
        pull("bulk_density_third_bar"),
        pull("bulk_density_oven_dry"))
    )
    hz <- ensure_horizon_schema(hz)

    out[[i]] <- PedonRecord$new(
      site = list(
        id              = as.character(rowi$pedon_key),
        lat             = as.numeric(rowi$latitude_std_decimal_degrees),
        lon             = as.numeric(rowi$longitude_std_decimal_degrees),
        country         = "US",
        reference_usda   = .normalise_kssl_taxorder(rowi$samp_taxorder),
        reference_source = "KSSL / NCSS Lab Data Mart"
      ),
      horizons = hz
    )
  }
  out <- out[!vapply(out, is.null, logical(1))]
  if (verbose)
    cli::cli_alert_success("KSSL: loaded {.val {length(out)}} pedons (require_b_horizon = {.field {require_b_horizon}})")
  out
}


# Internal: normalise KSSL Order labels (lower-case, capitalise) to
# soilKey output format.
.normalise_kssl_taxorder <- function(x) {
  if (is.na(x) || !nzchar(x)) return(NA_character_)
  s <- tolower(as.character(x))
  s <- gsub("s$", "", s)
  paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)), "s")
}


# Internal: element-wise pick first non-NA across two vectors.
.pick_first_non_na <- function(a, b) {
  a <- as.numeric(a); b <- as.numeric(b)
  ifelse(is.na(a), b, a)
}
