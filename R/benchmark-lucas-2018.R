# =============================================================================
# v0.9.49 -- LUCAS Soil 2018 Topsoil benchmark loader + WRB cross-check.
#
# The "EU-LUCAS / WRB benchmark Route B" was open since the v0.9.27 roadmap.
# v0.9.44 closed the raster-lookup half (lookup_esdb()); the chemistry half
# was waiting for an ESDAC download. With LUCAS-SOIL-2018.csv now in
# soil_data/eu_lucas/, this module ships the loader + benchmark to close
# Route B end-to-end.
#
# Pipeline:
#
#   load_lucas_soil_2018(path)  ----+
#                                    +--> benchmark_lucas_2018(...)
#   lookup_esdb(coords, "WRBLV1") --+
#         |
#         '--- predicted RSG (classify_wrb2022 on each pedon)
#                vs. reference RSG (canonical 1km map) -> confusion matrix
#
# LUCAS Soil 2018 ships only topsoil (0-20 cm) chemistry: pH (H2O / CaCl2),
# EC (mS/m), OC (g/kg), CaCO3 (g/kg), P (mg/kg), N (g/kg), K (mg/kg),
# Ox_Al (g/kg), Ox_Fe (g/kg). NO texture, NO Munsell, NO Vis-NIR. Texture
# can be filled from SoilGrids 250m via lookup_soilgrids() (v0.9.48); spectra
# can be filled with the v0.9.46 OSSL pretrained models if available.
# =============================================================================


# ---- WRB 2-letter code -> full RSG name (LUCAS WRBLV1 vs classify_wrb2022) ----

#' WRB Reference Soil Group code-to-name table
#'
#' The ESDB \code{WRBLV1.tif} raster encodes RSGs as 2-letter codes
#' (e.g. \code{"FL"} for Fluvisols). \code{\link{classify_wrb2022}}
#' returns the English plural name (e.g. \code{"Fluvisols"}). This
#' table maps between the two. Codes follow IUSS Working Group WRB
#' (2022); the legacy \code{"AB"} (Albeluvisols, WRB 2006) is mapped
#' to \code{NA} as it does not exist in WRB 2022.
#'
#' @keywords internal
.WRB_LV1_NAME_BY_CODE <- c(
  AB = NA_character_,    # Albeluvisols -- legacy WRB 2006, dropped in 2014/2022
  AC = "Acrisols",
  AN = "Andosols",
  AR = "Arenosols",
  AT = "Anthrosols",
  CH = "Chernozems",
  CL = "Calcisols",
  CM = "Cambisols",
  CR = "Cryosols",
  DU = "Durisols",
  FL = "Fluvisols",
  FR = "Ferralsols",
  GL = "Gleysols",
  GY = "Gypsisols",
  HS = "Histosols",
  KS = "Kastanozems",
  LP = "Leptosols",
  LV = "Luvisols",
  LX = "Lixisols",
  NT = "Nitisols",
  PH = "Phaeozems",
  PL = "Planosols",
  PT = "Plinthosols",
  PZ = "Podzols",
  RG = "Regosols",
  SC = "Solonchaks",
  SN = "Solonetz",
  ST = "Stagnosols",
  TC = "Technosols",
  UM = "Umbrisols",
  VR = "Vertisols"
)


# ---- Internal helpers ---------------------------------------------------

#' Coerce a LUCAS character cell to numeric, treating "< LOD" / "" as NA
#' @keywords internal
.lucas_numeric <- function(x) {
  s <- trimws(as.character(x))
  s[s %in% c("", "< LOD", "<LOD", "NA", "n.d.", "ND")] <- NA_character_
  suppressWarnings(as.numeric(s))
}


#' Build a single PedonRecord from one LUCAS chemistry row + optional BD row
#' @keywords internal
.build_lucas_pedon_2018 <- function(chem_row, bd_row = NULL) {
  # Unit conversions:
  #   OC, N, CaCO3, Ox_Al, Ox_Fe : g/kg     -> %        (* 0.1)
  #   EC                          : mS/m    -> dS/m     (* 0.01)
  #   P, K                        : mg/kg   -> mg/kg    (* 1)
  #   pH                          : unitless
  oc_top    <- .lucas_numeric(chem_row[["OC"]])             * 0.1
  oc_sub    <- .lucas_numeric(chem_row[["OC (20-30 cm)"]])  * 0.1
  caco3_top <- .lucas_numeric(chem_row[["CaCO3"]])          * 0.1
  caco3_sub <- .lucas_numeric(chem_row[["CaCO3 (20-30 cm)"]]) * 0.1
  n_pct     <- .lucas_numeric(chem_row[["N"]])              * 0.1
  ec_dS     <- .lucas_numeric(chem_row[["EC"]])             * 0.01
  fe_pct    <- .lucas_numeric(chem_row[["Ox_Fe"]])          * 0.1
  al_pct    <- .lucas_numeric(chem_row[["Ox_Al"]])          * 0.1

  top <- data.table::data.table(
    top_cm           = 0,
    bottom_cm        = 20,
    designation      = "Ap",
    ph_h2o           = .lucas_numeric(chem_row[["pH_H2O"]]),
    ph_cacl2         = .lucas_numeric(chem_row[["pH_CaCl2"]]),
    oc_pct           = oc_top,
    n_total_pct      = n_pct,
    p_mehlich3_mg_kg = .lucas_numeric(chem_row[["P"]]),
    caco3_pct        = caco3_top,
    ec_dS_m          = ec_dS,
    fe_ox_pct        = fe_pct,
    al_ox_pct        = al_pct
  )

  has_sub <- isTRUE(is.finite(oc_sub)) || isTRUE(is.finite(caco3_sub))
  hz <- if (has_sub) {
    sub <- data.table::data.table(
      top_cm      = 20,
      bottom_cm   = 30,
      designation = "B",
      oc_pct      = oc_sub,
      caco3_pct   = caco3_sub
    )
    data.table::rbindlist(list(top, sub), fill = TRUE)
  } else {
    top
  }

  if (!is.null(bd_row) && is.data.frame(bd_row) && nrow(bd_row) > 0L) {
    bd_top <- suppressWarnings(as.numeric(bd_row[["BD 0-20"]][1]))
    if (isTRUE(is.finite(bd_top))) hz$bulk_density_g_cm3[1L] <- bd_top
    if (nrow(hz) >= 2L) {
      bd_sub <- suppressWarnings(as.numeric(bd_row[["BD 20-30"]][1]))
      if (isTRUE(is.finite(bd_sub))) hz$bulk_density_g_cm3[2L] <- bd_sub
    }
  }

  hz <- ensure_horizon_schema(hz)

  PedonRecord$new(
    site = list(
      id               = as.character(chem_row[["POINTID"]]),
      lat              = .lucas_numeric(chem_row[["TH_LAT"]]),
      lon              = .lucas_numeric(chem_row[["TH_LONG"]]),
      country          = as.character(chem_row[["NUTS_0"]]),
      survey_date      = as.character(chem_row[["SURVEY_DATE"]]),
      land_cover       = as.character(chem_row[["LC0_Desc"]]),
      land_use         = as.character(chem_row[["LU1_Desc"]]),
      elevation_m      = .lucas_numeric(chem_row[["Elev"]]),
      reference_source = "LUCAS Soil 2018 Topsoil"
    ),
    horizons = hz
  )
}


# ---- Loader -------------------------------------------------------------

#' Load the LUCAS Soil 2018 Topsoil release as a list of PedonRecord objects
#'
#' Reads the canonical European Soil Data Centre (ESDAC) release of
#' LUCAS Soil 2018 Topsoil chemistry as published in the JRC report
#' (ESDAC dataset
#' \url{https://esdac.jrc.ec.europa.eu/content/lucas-2018-topsoil-data}).
#' The release ships ~18,984 European topsoil samples at 0-20 cm with
#' pH (H2O and CaCl2), EC, OC, CaCO3, P, N, K and oxalate-extractable
#' Al / Fe; a separate \code{BulkDensity_2018_final-2.csv} carries
#' bulk density at 0-10 / 10-20 / 20-30 / 0-20 cm for ~6,272 of those
#' points and is joined automatically when present.
#'
#' What's NOT in the release (and how to fill it):
#'
#' \itemize{
#'   \item \strong{Texture (clay / sand / silt)} -- not in this CSV.
#'         Pass \code{benchmark_lucas_2018(..., fill_texture_from =
#'         "soilgrids")} to fill from ISRIC SoilGrids 250m via
#'         \code{\link{lookup_soilgrids}}.
#'   \item \strong{Munsell colors} -- not collected by LUCAS Soil 2018.
#'         If the user has Vis-NIR spectra (release separate ~83 GB),
#'         use \code{\link{predict_munsell_from_spectra}} (v0.9.47).
#'   \item \strong{Vis-NIR spectra} -- distributed separately by ESDAC.
#'         Once downloaded and attached to the pedon's \code{$spectra},
#'         \code{\link{predict_from_spectra}} (v0.9.46) fills clay /
#'         sand / silt / pH / OC / CEC.
#'   \item \strong{Taxonomic reference} -- not in the LUCAS release;
#'         \code{\link{benchmark_lucas_2018}} attaches the canonical
#'         WRB Reference Soil Group via \code{\link{lookup_esdb}}
#'         (v0.9.44) at the pedon's coordinates.
#' }
#'
#' Unit conversions applied (LUCAS -> soilKey schema):
#'
#' \itemize{
#'   \item OC, N, CaCO3, Ox_Al, Ox_Fe: g/kg -> %  (* 0.1)
#'   \item EC: mS/m -> dS/m (* 0.01)
#'   \item P, K: mg/kg unchanged
#'   \item pH: unitless
#' }
#'
#' Special LUCAS string values \code{"< LOD"}, \code{"<LOD"}, empty
#' cells and \code{"n.d."} / \code{"ND"} are converted to \code{NA}
#' before numeric coercion.
#'
#' @param path Folder containing \code{LUCAS-SOIL-2018.csv} (typically
#'        \code{<root>/LUCAS-SOIL-2018-data-report-readme-v2/LUCAS-SOIL-2018-v2/}).
#' @param attach_bulk_density If \code{TRUE} (default), joins the
#'        \code{BulkDensity_2018_final-2.csv} sister file on
#'        \code{POINTID} when present.
#' @param countries Optional character vector of NUTS_0 codes
#'        (e.g. \code{c("ES", "FR")}) to filter pedons. Default
#'        \code{NULL} (all countries).
#' @param max_n Optional integer cap on the number of pedons returned
#'        (after country filter). Useful for development.
#' @param verbose If \code{TRUE} (default), prints a summary line.
#' @return A list of \code{\link{PedonRecord}} objects (one per LUCAS
#'         point). Each pedon has a \code{site$id} matching the LUCAS
#'         \code{POINTID}, \code{site$lat} / \code{site$lon} in WGS84,
#'         and either one or two horizons (the second being 20-30 cm
#'         when the subsoil OC / CaCO3 columns are populated).
#'         Provenance entries from the loader use
#'         \code{source = "measured"}.
#'
#' @seealso \code{\link{benchmark_lucas_2018}},
#'          \code{\link{lookup_esdb}},
#'          \code{\link{lookup_soilgrids}}.
#' @examples
#' \dontrun{
#' path <- "soil_data/eu_lucas/LUCAS-SOIL-2018-data-report-readme-v2/LUCAS-SOIL-2018-v2"
#' pedons <- load_lucas_soil_2018(path, countries = c("ES", "PT"),
#'                                  max_n = 100)
#' length(pedons)
#' pedons[[1]]
#' }
#' @export
load_lucas_soil_2018 <- function(path,
                                   attach_bulk_density = TRUE,
                                   countries           = NULL,
                                   max_n               = NULL,
                                   verbose             = TRUE) {
  if (!dir.exists(path) && !file.exists(path)) {
    stop(sprintf("load_lucas_soil_2018(): path does not exist: %s", path))
  }
  csv <- if (file.info(path)$isdir) {
    direct <- file.path(path, "LUCAS-SOIL-2018.csv")
    if (file.exists(direct)) {
      direct
    } else {
      hit <- list.files(path, pattern = "^LUCAS-SOIL-2018\\.csv$",
                          recursive = TRUE, full.names = TRUE)
      if (length(hit) == 0L) {
        stop(sprintf("LUCAS-SOIL-2018.csv not found under: %s", path))
      }
      hit[1L]
    }
  } else {
    path
  }
  d <- data.table::fread(csv)

  bd <- NULL
  if (isTRUE(attach_bulk_density)) {
    bd_csv <- file.path(dirname(csv), "BulkDensity_2018_final-2.csv")
    if (file.exists(bd_csv)) {
      bd <- data.table::fread(bd_csv)
    }
  }

  if (!is.null(countries)) {
    d <- d[d$NUTS_0 %in% countries, ]
  }
  if (!is.null(max_n) && nrow(d) > max_n) {
    d <- utils::head(d, n = as.integer(max_n))
  }

  out <- vector("list", nrow(d))
  bd_attached <- 0L
  for (i in seq_len(nrow(d))) {
    r <- d[i, ]
    bd_row <- NULL
    if (!is.null(bd)) {
      hit <- bd[bd$POINT_ID == r$POINTID, ]
      if (nrow(hit) >= 1L) {
        bd_row <- hit
        bd_attached <- bd_attached + 1L
      }
    }
    out[[i]] <- .build_lucas_pedon_2018(r, bd_row)
  }

  if (isTRUE(verbose)) {
    cli::cli_alert_success(sprintf(
      "load_lucas_soil_2018(): %d pedons loaded (BD attached: %d)",
      length(out), bd_attached
    ))
  }
  out
}


# ---- v0.9.50: comprehensive SoilGrids fill + Vis-NIR wire-up -----------

#' Mapping of SoilGrids 250m property names to soilKey horizon columns
#'
#' SoilGrids stores nine soil properties at six standard depths;
#' \code{\link{lookup_soilgrids}} returns them in conventional units
#' after the published per-property scale factor. This table records
#' the corresponding soilKey horizon column plus an optional secondary
#' multiplier needed to align with soilKey unit conventions.
#'
#' @keywords internal
.SOILGRIDS_TO_HORIZON_MAP <- list(
  clay     = list(col = "clay_pct",            scale_secondary = 1.0),
  sand     = list(col = "sand_pct",            scale_secondary = 1.0),
  silt     = list(col = "silt_pct",            scale_secondary = 1.0),
  phh2o    = list(col = "ph_h2o",              scale_secondary = 1.0),
  soc      = list(col = "oc_pct",              scale_secondary = 0.1),  # g/kg -> %
  cec      = list(col = "cec_cmol",            scale_secondary = 1.0),
  bdod     = list(col = "bulk_density_g_cm3",  scale_secondary = 1.0),
  nitrogen = list(col = "n_total_pct",         scale_secondary = 0.1),  # g/kg -> %
  cfvo     = list(col = "coarse_fragments_pct", scale_secondary = 1.0)
)


#' Fill a horizon (or synthesise a new one) from SoilGrids 250m
#'
#' Internal helper used by \code{\link{benchmark_lucas_2018}}. For
#' each requested property, calls \code{lookup_fn} (default
#' \code{\link{lookup_soilgrids}}) at \code{soilgrids_depth},
#' converts to the soilKey unit and writes onto the pedon's horizon
#' \code{horizon_idx} via \code{add_measurement(...,
#' source = "inferred_prior")}. Synthesises the horizon if it does
#' not exist yet (geometry from \code{horizon_top_cm} /
#' \code{horizon_bottom_cm}).
#'
#' Test injection: pass \code{lookup_fn = function(...) value} to
#' bypass the network when unit-testing.
#'
#' @keywords internal
.fill_horizon_from_soilgrids <- function(pedon,
                                            horizon_idx,
                                            properties,
                                            soilgrids_depth = "0-5cm",
                                            horizon_top_cm    = 0,
                                            horizon_bottom_cm = 20,
                                            horizon_designation = "Ap",
                                            lookup_fn = lookup_soilgrids) {
  if (is.na(pedon$site$lon %||% NA_real_) ||
        is.na(pedon$site$lat %||% NA_real_)) {
    return(invisible(0L))
  }
  coord <- c(pedon$site$lon, pedon$site$lat)
  if (horizon_idx > nrow(pedon$horizons)) {
    new_hz <- data.table::data.table(
      top_cm      = horizon_top_cm,
      bottom_cm   = horizon_bottom_cm,
      designation = horizon_designation
    )
    pedon$horizons <- ensure_horizon_schema(
      data.table::rbindlist(list(pedon$horizons, new_hz), fill = TRUE)
    )
  }
  written <- 0L
  h <- pedon$horizons
  for (prop in properties) {
    spec <- .SOILGRIDS_TO_HORIZON_MAP[[prop]]
    if (is.null(spec)) next
    col <- spec$col
    if (isTRUE(is.finite(h[[col]][horizon_idx]))) next
    raw <- tryCatch(
      lookup_fn(coord, property = prop,
                  depth = soilgrids_depth, quantile = "mean"),
      error   = function(e) NA_real_,
      warning = function(w) NA_real_
    )
    if (!isTRUE(is.finite(raw))) next
    val <- as.numeric(raw) * spec$scale_secondary
    pedon$add_measurement(
      horizon_idx = horizon_idx,
      attribute   = col,
      value       = val,
      source      = "inferred_prior",
      confidence  = 0.6,
      notes       = sprintf("SoilGrids 250m mean, %s", soilgrids_depth),
      overwrite   = FALSE
    )
    written <- written + 1L
  }
  invisible(written)
}


#' Attach LUCAS 2018 Vis-NIR spectra to a list of PedonRecord objects
#'
#' Joins the LUCAS Soil 2018 Spectral Library (separate ESDAC release,
#' ~83 GB) onto the pedons returned by
#' \code{\link{load_lucas_soil_2018}}, by matching the LUCAS
#' \code{POINT_ID} of the spectra against \code{pedon$site$id}. Each
#' matched pedon gets \code{$spectra$vnir} populated as a numeric
#' matrix (rows = horizons, cols = wavelengths).
#'
#' Two input shapes are accepted:
#'
#' \itemize{
#'   \item A wide \code{data.frame} keyed by an integer
#'         \code{POINT_ID} column with one column per wavelength
#'         (column names parseable as numeric nm). One row per
#'         LUCAS point.
#'   \item A long \code{data.frame} with columns \code{POINT_ID},
#'         \code{wavelength_nm}, \code{reflectance}.
#' }
#'
#' Spectra are attached only to the topsoil horizon (row 1); the
#' subsoil horizon (if any) is left without spectra. After this call,
#' \code{benchmark_lucas_2018(..., fill_topsoil_from = "spectra",
#' ossl_models = ...)} feeds the spectra through
#' \code{\link{predict_from_spectra}} (v0.9.46) to fill any
#' chemistry / texture gap not already populated by SoilGrids.
#'
#' @param pedons List of \code{\link{PedonRecord}} objects.
#' @param spectra A wide or long \code{data.frame} as described
#'        above.
#' @param point_id_col Name of the LUCAS point-id column in
#'        \code{spectra}. Default \code{"POINT_ID"}.
#' @param verbose If \code{TRUE} (default), reports the join hit
#'        rate.
#' @return The list of pedons (mutated in place; returned invisibly).
#' @seealso \code{\link{predict_from_spectra}},
#'          \code{\link{predict_munsell_from_spectra}},
#'          \code{\link{load_lucas_soil_2018}}.
#' @export
attach_lucas_spectra <- function(pedons,
                                   spectra,
                                   point_id_col = "POINT_ID",
                                   verbose      = TRUE) {
  if (!is.list(pedons) || length(pedons) == 0L) {
    stop("attach_lucas_spectra(): 'pedons' must be a non-empty list of PedonRecord.")
  }
  if (!is.data.frame(spectra)) {
    stop("attach_lucas_spectra(): 'spectra' must be a data.frame.")
  }
  if (!point_id_col %in% names(spectra)) {
    stop(sprintf("attach_lucas_spectra(): '%s' not in spectra columns.",
                  point_id_col))
  }
  is_long <- all(c("wavelength_nm", "reflectance") %in% names(spectra))
  if (is_long) {
    # Pivot long -> wide
    wl <- sort(unique(as.numeric(spectra$wavelength_nm)))
    pids <- unique(spectra[[point_id_col]])
    mat <- matrix(NA_real_, nrow = length(pids), ncol = length(wl),
                    dimnames = list(as.character(pids), as.character(wl)))
    for (j in seq_len(nrow(spectra))) {
      r <- spectra[j, ]
      i_row <- match(r[[point_id_col]], pids)
      i_col <- match(as.numeric(r$wavelength_nm), wl)
      mat[i_row, i_col] <- as.numeric(r$reflectance)
    }
    wide_ids <- pids
  } else {
    wide_ids <- spectra[[point_id_col]]
    wl_cols <- setdiff(names(spectra), point_id_col)
    wl <- suppressWarnings(as.numeric(wl_cols))
    keep <- !is.na(wl)
    wl <- wl[keep]
    wl_cols <- wl_cols[keep]
    mat <- as.matrix(spectra[, wl_cols, drop = FALSE])
    storage.mode(mat) <- "double"
    rownames(mat) <- as.character(wide_ids)
    colnames(mat) <- as.character(wl)
  }
  hits <- 0L
  for (p in pedons) {
    pid <- p$site$id %||% NA_character_
    if (is.na(pid)) next
    row_idx <- match(as.character(pid), rownames(mat))
    if (is.na(row_idx)) next
    spec_row <- mat[row_idx, , drop = FALSE]
    n_hz <- nrow(p$horizons)
    full <- matrix(NA_real_, nrow = n_hz, ncol = ncol(mat),
                     dimnames = list(NULL, colnames(mat)))
    full[1L, ] <- spec_row
    p$spectra <- list(vnir = full)
    hits <- hits + 1L
  }
  if (isTRUE(verbose)) {
    cli::cli_alert_success(sprintf(
      "attach_lucas_spectra(): joined spectra to %d / %d pedons",
      hits, length(pedons)
    ))
  }
  invisible(pedons)
}


# ---- Benchmark ----------------------------------------------------------

#' Run the LUCAS Soil 2018 / ESDB WRB benchmark
#'
#' For each pedon in \code{pedons}, attaches the canonical Reference
#' Soil Group at its coordinate via \code{\link{lookup_esdb}}, runs
#' \code{\link{classify_wrb2022}} (or \code{\link{classify_sibcs}}),
#' and tabulates predicted vs reference. Optionally fills missing
#' texture from ISRIC SoilGrids 250m before classifying so that
#' WRB diagnostic horizons that depend on clay (argic, ferralic,
#' nitic) are reachable.
#'
#' This closes Route B of the v0.9.27 EU-LUCAS roadmap end-to-end:
#' v0.9.44 \code{\link{lookup_esdb}} provides the reference label;
#' v0.9.49 (this) provides the loader and the comparison loop;
#' v0.9.48 \code{\link{lookup_soilgrids}} fills texture; v0.9.46
#' \code{\link{predict_from_spectra}} and v0.9.47
#' \code{\link{predict_munsell_from_spectra}} can fill the
#' chemistry / Munsell gaps when Vis-NIR is available.
#'
#' @param pedons List of \code{\link{PedonRecord}} objects, typically
#'        from \code{\link{load_lucas_soil_2018}}.
#' @param esdb_root Path to the unpacked ESDB raster directory
#'        (containing the \code{WRBLV1/} sub-folder).
#' @param attribute ESDB attribute to use as reference. Default
#'        \code{"WRBLV1"} (Reference Soil Group, 31 codes). Other
#'        sensible choices: \code{"FAO90LV1"} (legacy FAO 1990).
#' @param fill_texture_from Deprecated alias for
#'        \code{fill_topsoil_from} (v0.9.49 signature). When
#'        \code{"soilgrids"}, treated as
#'        \code{fill_topsoil_from = "soilgrids"} with
#'        \code{fill_properties = c("clay", "sand", "silt")} and
#'        \code{fill_subsoil_from = "none"}.
#' @param fill_topsoil_from One of \code{"none"} (default),
#'        \code{"soilgrids"} (fills topsoil 0-20 cm from SoilGrids
#'        250m at 0-5 cm), or \code{"spectra"} (runs
#'        \code{\link{predict_from_spectra}} with the supplied
#'        \code{ossl_models}; pedons must have
#'        \code{$spectra$vnir} attached, e.g. via
#'        \code{\link{attach_lucas_spectra}}).
#' @param fill_subsoil_from One of \code{"none"} (default) or
#'        \code{"soilgrids"} (synthesises a 30-60 cm B horizon from
#'        SoilGrids 250m). Unlocks WRB diagnostic horizons that
#'        depend on subsoil features (cambic, argic, mollic).
#' @param fill_properties Character vector of SoilGrids properties
#'        to fill when \code{fill_topsoil_from = "soilgrids"} or
#'        \code{fill_subsoil_from = "soilgrids"}. Default uses all
#'        9 properties: clay, sand, silt, phh2o, soc, cec, bdod,
#'        nitrogen, cfvo. Set to \code{c("clay", "sand", "silt")}
#'        to recover the v0.9.49 behaviour. \code{cfvo} is mapped
#'        to \code{coarse_fragments_pct}, which drives the
#'        Leptosols diagnostic (>= 90 within 25 cm).
#' @param ossl_models Required when \code{fill_topsoil_from =
#'        "spectra"}. A list of \code{soilKey_pls_model} objects
#'        from \code{\link{train_pls_from_ossl}} (v0.9.46).
#' @param classify_with One of \code{"wrb2022"} (default) or
#'        \code{"sibcs"}.
#' @param max_n Optional integer cap on the number of pedons
#'        benchmarked. Useful for quick development runs.
#' @param soilgrids_lookup_fn Internal: SoilGrids lookup function
#'        (defaults to \code{\link{lookup_soilgrids}}). Override
#'        for unit tests to inject a deterministic stub.
#' @param verbose If \code{TRUE} (default), prints progress.
#' @return A list with elements:
#'   \describe{
#'     \item{\code{predictions}}{data.frame with one row per pedon:
#'           \code{point_id, lon, lat, country, predicted,
#'           reference_code, reference_name, agree}.}
#'     \item{\code{confusion}}{Confusion table (predicted vs
#'           reference) over in-scope rows.}
#'     \item{\code{accuracy}}{Overall fraction of correct
#'           classifications among in-scope rows.}
#'     \item{\code{per_rsg}}{Per-RSG recall data.frame.}
#'     \item{\code{n_in_scope}}{Number of pedons with both
#'           predicted and reference set.}
#'     \item{\code{n_total}}{Total pedons benchmarked.}
#'     \item{\code{n_errors}}{Number of pedons where the classifier
#'           errored out.}
#'     \item{\code{errors}}{List of \code{(i, id, error)} tuples for
#'           classifier errors.}
#'     \item{\code{config}}{Recap of arguments used.}
#'   }
#'
#' @examples
#' \dontrun{
#' pedons <- load_lucas_soil_2018(
#'   "soil_data/eu_lucas/LUCAS-SOIL-2018-data-report-readme-v2/LUCAS-SOIL-2018-v2",
#'   countries = c("ES"), max_n = 50)
#' bench <- benchmark_lucas_2018(
#'   pedons,
#'   esdb_root = "soil_data/eu_lucas/ESDB-Raster-Library-1k-GeoTIFF-20240507",
#'   fill_texture_from = "soilgrids")
#' bench$accuracy
#' bench$per_rsg
#' }
#' @seealso \code{\link{load_lucas_soil_2018}},
#'          \code{\link{lookup_esdb}},
#'          \code{\link{lookup_soilgrids}}.
#' @export
benchmark_lucas_2018 <- function(pedons,
                                   esdb_root,
                                   attribute          = "WRBLV1",
                                   fill_texture_from  = NULL,
                                   fill_topsoil_from  = c("none", "soilgrids", "spectra"),
                                   fill_subsoil_from  = c("none", "soilgrids"),
                                   fill_properties    = c("clay", "sand", "silt",
                                                            "phh2o", "soc", "cec",
                                                            "bdod", "nitrogen",
                                                            "cfvo"),
                                   ossl_models        = NULL,
                                   classify_with      = c("wrb2022", "sibcs"),
                                   max_n              = NULL,
                                   soilgrids_lookup_fn = lookup_soilgrids,
                                   verbose            = TRUE) {
  # Backward compatibility: v0.9.49 fill_texture_from = "soilgrids" maps
  # to fill_topsoil_from = "soilgrids" + fill_properties = clay/sand/silt
  if (!is.null(fill_texture_from)) {
    fill_texture_from <- match.arg(fill_texture_from,
                                      choices = c("none", "soilgrids"))
    if (fill_texture_from == "soilgrids") {
      fill_topsoil_from <- "soilgrids"
      fill_properties   <- c("clay", "sand", "silt")
      fill_subsoil_from <- "none"
    }
  }
  fill_topsoil_from <- match.arg(fill_topsoil_from)
  fill_subsoil_from <- match.arg(fill_subsoil_from)
  classify_with     <- match.arg(classify_with)
  unknown_props <- setdiff(fill_properties,
                              names(.SOILGRIDS_TO_HORIZON_MAP))
  if (length(unknown_props) > 0L) {
    stop(sprintf(
      "benchmark_lucas_2018(): unknown SoilGrids properties: %s",
      paste(unknown_props, collapse = ", ")
    ))
  }
  if (fill_topsoil_from == "spectra" &&
        (is.null(ossl_models) || length(ossl_models) == 0L)) {
    stop("benchmark_lucas_2018(): fill_topsoil_from = 'spectra' requires ",
         "'ossl_models' (output of train_pls_from_ossl).")
  }

  if (!is.list(pedons) || length(pedons) == 0L) {
    stop("benchmark_lucas_2018(): 'pedons' must be a non-empty list of PedonRecord.")
  }
  if (!all(vapply(pedons, inherits, logical(1L), "PedonRecord"))) {
    stop("benchmark_lucas_2018(): every element of 'pedons' must be a PedonRecord.")
  }
  if (!is.null(max_n) && length(pedons) > max_n) {
    pedons <- pedons[seq_len(as.integer(max_n))]
  }

  # 1. Reference labels via lookup_esdb
  coords <- t(vapply(pedons, function(p) {
    c(p$site$lon %||% NA_real_, p$site$lat %||% NA_real_)
  }, FUN.VALUE = numeric(2L)))
  has_coords <- is.finite(coords[, 1L]) & is.finite(coords[, 2L])
  ref_codes <- rep(NA_character_, length(pedons))
  if (any(has_coords)) {
    if (isTRUE(verbose)) {
      cli::cli_alert_info(sprintf(
        "Looking up ESDB %s for %d coordinates...",
        attribute, sum(has_coords)
      ))
    }
    rc <- tryCatch(
      lookup_esdb(coords[has_coords, , drop = FALSE],
                   attribute = attribute, raster_root = esdb_root),
      error = function(e) NULL
    )
    if (!is.null(rc)) {
      ref_codes[has_coords] <- as.character(rc)
    }
  }
  ref_names <- vapply(ref_codes, function(code) {
    if (is.na(code)) return(NA_character_)
    nm <- .WRB_LV1_NAME_BY_CODE[code]
    if (is.null(nm) || is.na(nm)) NA_character_ else as.character(nm)
  }, FUN.VALUE = character(1L))

  # 2a. Topsoil fill from SoilGrids 0-5cm.
  if (fill_topsoil_from == "soilgrids") {
    if (isTRUE(verbose)) {
      cli::cli_alert_info(sprintf(
        "Filling topsoil from SoilGrids 250m at 0-5cm: %s",
        paste(fill_properties, collapse = ", ")
      ))
    }
    for (i in seq_along(pedons)) {
      if (!has_coords[i]) next
      .fill_horizon_from_soilgrids(
        pedons[[i]],
        horizon_idx       = 1L,
        properties        = fill_properties,
        soilgrids_depth   = "0-5cm",
        lookup_fn         = soilgrids_lookup_fn
      )
    }
  }

  # 2b. Subsoil fill from SoilGrids 30-60cm. Synthesises a B horizon
  # if absent. Unlocks WRB cambic / argic / mollic diagnostics that
  # the LUCAS topsoil-only data cannot satisfy alone.
  if (fill_subsoil_from == "soilgrids") {
    if (isTRUE(verbose)) {
      cli::cli_alert_info(sprintf(
        "Filling subsoil from SoilGrids 250m at 30-60cm: %s",
        paste(fill_properties, collapse = ", ")
      ))
    }
    for (i in seq_along(pedons)) {
      if (!has_coords[i]) next
      n_hz <- nrow(pedons[[i]]$horizons)
      sub_idx <- if (n_hz >= 2L) 2L else (n_hz + 1L)
      .fill_horizon_from_soilgrids(
        pedons[[i]],
        horizon_idx         = sub_idx,
        properties          = fill_properties,
        soilgrids_depth     = "30-60cm",
        horizon_top_cm      = 30,
        horizon_bottom_cm   = 60,
        horizon_designation = "B",
        lookup_fn           = soilgrids_lookup_fn
      )
    }
  }

  # 2c. Topsoil fill from Vis-NIR spectra (v0.9.46 OSSL pretrained).
  if (fill_topsoil_from == "spectra") {
    if (isTRUE(verbose)) {
      cli::cli_alert_info(sprintf(
        "Filling topsoil from Vis-NIR via OSSL pretrained models (%d properties)",
        length(ossl_models)
      ))
    }
    n_filled <- 0L
    for (i in seq_along(pedons)) {
      p <- pedons[[i]]
      if (is.null(p$spectra) || is.null(p$spectra$vnir)) next
      tryCatch({
        predict_from_spectra(p, models = ossl_models,
                              overwrite = FALSE, verbose = FALSE)
        n_filled <- n_filled + 1L
      }, error = function(e) {
        # Spectra path is best-effort; record but don't abort.
        invisible(NULL)
      })
    }
    if (isTRUE(verbose)) {
      cli::cli_alert_info(sprintf(
        "  ... %d / %d pedons had spectra and were filled",
        n_filled, length(pedons)
      ))
    }
  }

  # 3. Classify
  classify_fn <- switch(classify_with,
                          wrb2022 = classify_wrb2022,
                          sibcs   = classify_sibcs)
  if (isTRUE(verbose)) {
    cli::cli_alert_info(sprintf("Running classify_%s on %d pedons...",
                                  classify_with, length(pedons)))
  }
  predicted <- character(length(pedons))
  errors <- list()
  for (i in seq_along(pedons)) {
    res <- tryCatch(
      classify_fn(pedons[[i]], on_missing = "silent"),
      error = function(e) {
        errors[[length(errors) + 1L]] <<- list(
          i = i,
          id = as.character(pedons[[i]]$site$id %||% i),
          error = conditionMessage(e)
        )
        NULL
      }
    )
    predicted[i] <- if (is.null(res)) NA_character_ else as.character(res$rsg_or_order %||% NA_character_)
  }

  # 4. Build comparison data.frame
  ids <- vapply(pedons, function(p) as.character(p$site$id %||% NA_character_),
                  FUN.VALUE = character(1L))
  countries <- vapply(pedons, function(p) {
    as.character(p$site$country %||% NA_character_)
  }, FUN.VALUE = character(1L))
  comparison <- data.frame(
    point_id       = ids,
    lon            = coords[, 1L],
    lat            = coords[, 2L],
    country        = countries,
    predicted      = predicted,
    reference_code = ref_codes,
    reference_name = ref_names,
    agree          = !is.na(predicted) & !is.na(ref_names) & predicted == ref_names,
    stringsAsFactors = FALSE
  )

  in_scope <- !is.na(comparison$predicted) & !is.na(comparison$reference_name)
  conf <- if (any(in_scope)) {
    table(
      Predicted = comparison$predicted[in_scope],
      Reference = comparison$reference_name[in_scope]
    )
  } else {
    NULL
  }
  per_rsg <- if (any(in_scope)) {
    sub_in <- comparison[in_scope, ]
    refs <- sort(unique(sub_in$reference_name))
    do.call(rbind, lapply(refs, function(rsg) {
      sub <- sub_in[sub_in$reference_name == rsg, ]
      data.frame(
        reference_rsg = rsg,
        n             = nrow(sub),
        n_correct     = sum(sub$agree),
        recall        = mean(sub$agree),
        stringsAsFactors = FALSE
      )
    }))
  } else {
    NULL
  }
  accuracy <- if (any(in_scope)) {
    mean(comparison$agree[in_scope])
  } else {
    NA_real_
  }

  if (isTRUE(verbose)) {
    if (sum(in_scope) > 0L) {
      cli::cli_alert_success(sprintf(
        "benchmark_lucas_2018(): accuracy = %.1f%% over %d in-scope points",
        accuracy * 100, sum(in_scope)
      ))
    } else {
      cli::cli_alert_warning("benchmark_lucas_2018(): 0 in-scope points (no reference + prediction overlap).")
    }
  }

  list(
    predictions = comparison,
    confusion   = conf,
    accuracy    = accuracy,
    per_rsg     = per_rsg,
    n_in_scope  = sum(in_scope),
    n_total     = nrow(comparison),
    n_errors    = length(errors),
    errors      = errors,
    config = list(
      esdb_attribute    = attribute,
      fill_topsoil_from = fill_topsoil_from,
      fill_subsoil_from = fill_subsoil_from,
      fill_properties   = fill_properties,
      classify_with     = classify_with
    )
  )
}
