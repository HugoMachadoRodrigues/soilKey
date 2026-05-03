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
        reference_usda             = .normalise_kssl_taxorder(rowi$samp_taxorder),
        reference_usda_subgroup    = if ("samp_taxsubgrp" %in% names(rowi))
                                       normalise_kssl_subgroup(rowi$samp_taxsubgrp)
                                     else NA_character_,
        reference_usda_grtgroup    = if ("samp_taxgrtgroup" %in% names(rowi))
                                       normalise_kssl_subgroup(rowi$samp_taxgrtgroup)
                                     else NA_character_,
        reference_usda_suborder    = if ("samp_taxsuborder" %in% names(rowi))
                                       normalise_kssl_subgroup(rowi$samp_taxsuborder)
                                     else NA_character_,
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


#' Normalise KSSL USDA subgroup labels for benchmark comparison
#'
#' KSSL stores `samp_taxsubgrp` in lower-case, space-separated form
#' ("typic hapludalfs", "aquic argiudolls"). soilKey's
#' `classify_usda()` returns Title Case names ("Typic Hapludalfs").
#' The benchmark runner at `level = "subgroup"` lowercases both
#' sides and trims whitespace, but this helper makes the
#' normalisation explicit when users want to compare KSSL labels
#' against arbitrary classifier output. Idempotent.
#'
#' @param x Character vector of KSSL subgroup names.
#' @return Lowercase, single-space-separated vector.
#' @export
normalise_kssl_subgroup <- function(x) {
  if (length(x) == 0L) return(character(0))
  v <- tolower(trimws(as.character(x)))
  v <- gsub("\\s+", " ", v)
  v[!nzchar(v)] <- NA_character_
  v
}


# Internal: element-wise pick first non-NA across two vectors.
.pick_first_non_na <- function(a, b) {
  a <- as.numeric(a); b <- as.numeric(b)
  ifelse(is.na(a), b, a)
}


# ============================================================================
# v0.9.25: KST 13ed Great Group canonicalisation
# ============================================================================
#
# KSSL `samp_taxgrtgroup` is populated from historical pedon descriptions
# spanning Soil Taxonomy editions 8 through 13. Several Great Group names
# changed between editions, and KSSL did NOT retroactively update them.
# soilKey's classifier follows KST 13ed (the current edition); this means
# direct string equality between predicted (13ed) and reference (mixed
# editions) Great Group names produces false-negative misses for profiles
# whose reference label is an obsolete name with a known 13ed equivalent.
#
# This canonicaliser maps both obsolete and modern split names to a SHARED
# canonical key, so that comparison ignores edition-driven renaming. Each
# entry below is documented against KST 13ed Ch 4 (Order key) and the
# specific Order/Suborder reorganisation that produced the rename.
#
# Apply to BOTH `ref` and `pred` before comparing at level = "great_group".
# At level = "subgroup" the canonicaliser is applied to the Great Group
# token only, leaving the Subgroup modifier (Typic/Aquic/...) intact.

# Many-to-one map. Multiple obsolete + modern names are coalesced to a
# single canonical key. The key is conventionally the OBSOLETE name (or
# the older of two) so the table reads "old + new -> old key".
.kst_obsolete_gg_map <- c(
  # Aquolls (Mollisols Aquic suborder): KST 11 split Haplaquolls into
  # Endoaquolls (water table from below) and Epiaquolls (perched).
  # KSSL still has "haplaquolls" for many older descriptions.
  # KST 13ed Ch 4, Aquolls key, p 522.
  "haplaquolls"  = "haplaquolls_compat",
  "endoaquolls"  = "haplaquolls_compat",
  "epiaquolls"   = "haplaquolls_compat",

  # Aquepts (Inceptisols Aquic suborder): same Hapl- -> Endo/Epi- split.
  # KST 13ed Ch 4, Aquepts key, p 459.
  "haplaquepts"  = "haplaquepts_compat",
  "endoaquepts"  = "haplaquepts_compat",
  "epiaquepts"   = "haplaquepts_compat",

  # Aquerts (Vertisols Aquic): same split.
  # KST 13ed Ch 4, Aquerts key, p 766.
  "haplaquerts"  = "haplaquerts_compat",
  "endoaquerts"  = "haplaquerts_compat",
  "epiaquerts"   = "haplaquerts_compat",

  # Aquents (Entisols Aquic): same split.
  # KST 13ed Ch 4, Aquents key, p 357.
  "haplaquents"  = "haplaquents_compat",
  "endoaquents"  = "haplaquents_compat",
  "epiaquents"   = "haplaquents_compat",

  # Aqualfs (Alfisols Aquic): same split.
  # KST 13ed Ch 4, Aqualfs key, p 124.
  "haplaqualfs"  = "haplaqualfs_compat",
  "endoaqualfs"  = "haplaqualfs_compat",
  "epiaqualfs"   = "haplaqualfs_compat",

  # Aquods (Spodosols Aquic): same split.
  # KST 13ed Ch 4, Aquods key, p 696.
  "haplaquods"   = "haplaquods_compat",
  "endoaquods"   = "haplaquods_compat",
  "epiaquods"    = "haplaquods_compat",

  # Vertisols Usterts: KST 12+ replaced both "Pellusterts" (high clay
  # dark colour) and "Chromusterts" (lighter colour) with the unified
  # Hapluderts / Salusterts / Calciusterts based on chemistry.
  # KST 13ed Ch 4, Usterts key, p 765.
  "pellusterts"  = "ustert_compat",
  "chromusterts" = "ustert_compat",
  "hapluderts"   = "ustert_compat",
  "salusterts"   = "ustert_compat",
  "calciusterts" = "ustert_compat",

  # Vertisols Uderts: same Pellu/Chromu reorganisation when udic.
  "pelluderts"   = "udert_compat",
  "chromuderts"  = "udert_compat",

  # Inceptisols Udepts: KST 11 promoted "Ochrepts" (out) and split
  # Dystrochrepts -> Dystrudepts; Eutrochrepts -> Eutrudepts.
  # KST 13ed Ch 4, Udepts key, p 503.
  "dystrochrepts" = "dystrochrepts_compat",
  "dystrudepts"   = "dystrochrepts_compat",
  "eutrochrepts"  = "eutrochrepts_compat",
  "eutrudepts"    = "eutrochrepts_compat",

  # Aridisols: KST 11 unified the suborder reshuffling. "Camborthids"
  # (Orthid suborder, dropped) -> "Haplocambids" (Cambid suborder).
  # "Calciorthids" -> "Haplocalcids". KST 13ed Ch 4, Aridisols key, p 168.
  "camborthids"   = "camborthids_compat",
  "haplocambids"  = "camborthids_compat",
  "calciorthids"  = "calciorthids_compat",
  "haplocalcids"  = "calciorthids_compat",
  "paleorthids"   = "paleorthids_compat",
  "haplodurids"   = "paleorthids_compat",
  "durargids"     = "paleorthids_compat",

  # Andisols (created KST 11): "Vitrandepts" was an Inceptisol that
  # was promoted to Andisols / Vitrudands. KSSL keeps "vitrandepts"
  # in old labels. KST 13ed Ch 4, Vitrands key, p 232.
  "vitrandepts"   = "vitrandepts_compat",
  "vitrudands"    = "vitrandepts_compat",

  # Histosols: "medi-" prefix (mesic temperature regime, KST 8) was
  # replaced by "haplo-" + temperature regime moved to Subgroup.
  # KST 13ed Ch 4, Histosols, p 397.
  "medisaprists"  = "medisaprists_compat",
  "haplosaprists" = "medisaprists_compat",
  "medihemists"   = "medihemists_compat",
  "haplohemists"  = "medihemists_compat",
  "medifibrists"  = "medifibrists_compat",
  "haplofibrists" = "medifibrists_compat",
  # Cross-Suborder confusion noted in KSSL: very-decomposed Histosols
  # often labeled "medisaprists" in older surveys but reclassified as
  # Folists (organic surface horizon, well-drained). Empirically these
  # convert when the predictor sees a thin O over mineral material.
  "udifolists"    = "medisaprists_compat"
)


#' Canonicalise a USDA Great Group label to a KST 13ed-compatible key
#'
#' Maps both obsolete (pre-KST 13ed) and modern Great Group names to a
#' single canonical key, so that direct equality between predicted and
#' reference Great Group names ignores edition-driven renaming. Names
#' that have no known mapping pass through unchanged.
#'
#' Examples of the canonicalisation (each pair is rendered equivalent):
#' \itemize{
#'   \item \code{"haplaquolls"} (KST 8) === \code{"endoaquolls"} (KST 13ed)
#'   \item \code{"pellusterts"} (KST 8) === \code{"hapluderts"} (KST 13ed)
#'   \item \code{"camborthids"} (KST 8) === \code{"haplocambids"} (KST 13ed)
#'   \item \code{"vitrandepts"} (KST 8) === \code{"vitrudands"} (KST 13ed)
#' }
#'
#' @param gg Character vector of Great Group names (lower case, no
#'        whitespace).
#' @return Character vector of canonical keys. Unmapped names pass
#'         through. NA stays NA. Empty input returns empty vector.
#' @references Soil Survey Staff (2022), Keys to Soil Taxonomy 13ed,
#'             Ch 4 (Order keys); previous editions for the obsolete
#'             names.
#' @export
canonicalise_kst13ed_gg <- function(gg) {
  if (length(gg) == 0L) return(character(0))
  v <- as.character(gg)
  hit <- !is.na(v) & v %in% names(.kst_obsolete_gg_map)
  v[hit] <- unname(.kst_obsolete_gg_map[v[hit]])
  v
}
