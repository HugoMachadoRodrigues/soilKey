# =============================================================================
# soilKey -- user-facing spatial classification aid.
#
# `soil_classes_at_location(lat, lon, ...)` answers the question:
#
#   "I'm at this location -- before I have any pedon data, what soil
#    classes are most likely here?"
#
# This is the *guide* counterpart to `spatial_prior_soilgrids()`,
# which is a *check* applied AFTER classification. The user-facing
# helper does NOT require a `PedonRecord`; it consumes coordinates
# directly and returns a ranked list of likely classes plus the
# canonical attribute ranges per class so the user knows what data
# to collect to confirm.
#
# The architectural invariant still holds: this function does not
# classify a pedon. It tells the user what classes to *expect*; the
# deterministic key remains the only thing that *assigns* a class
# from horizon data.
# =============================================================================


#' Likely soil classes at a geographic location (spatial classification aid)
#'
#' Returns a ranked list of the soil Reference Soil Groups (or
#' SiBCS ordens, or USDA orders) most likely to occur at the given
#' point, based on a global or regional dominant-soil raster
#' (SoilGrids 2.0 by default). This is the **before-you-have-a-pedon
#' helper**: a pedologist arriving in the field can call it with the
#' GPS coordinates of the planned profile pit and see which classes
#' are expected, plus what attributes typically distinguish them.
#'
#' This function does \strong{not} classify a profile. The
#' deterministic key in \code{\link{classify_wrb2022}} /
#' \code{\link{classify_sibcs}} / \code{\link{classify_usda}} remains
#' the only thing that assigns a class from horizon data. The output
#' here is purely informational -- a "shopping list" of what to
#' confirm.
#'
#' @section Data source:
#' For real use, point \code{source_url} at a regional SoilGrids
#' "MostProbable WRB" GeoTIFF / COG (one of the cuts at
#' \url{https://files.isric.org/soilgrids/latest/data/wrb/}). For
#' tests, \code{options(soilKey.test_raster = "/tmp/syn.tif")} is
#' honoured. When no source is given, the function emits a
#' \code{cli_alert_warning()} and returns an empty result -- it does
#' \strong{not} pretend to know.
#'
#' @section Output:
#' A list with three elements:
#' \describe{
#'   \item{\code{distribution}}{A \code{data.table} with columns
#'         \code{rsg_code}, \code{rsg_name}, \code{probability},
#'         sorted by descending probability.}
#'   \item{\code{typical_attributes}}{A \code{data.table} keyed by
#'         \code{rsg_code} with the canonical attribute ranges that
#'         distinguish each class (clay range, CEC range, BS range,
#'         etc.). The values come from the WRB 2022 / SiBCS 5 /
#'         KST 13ed canonical thresholds, NOT from the raster.}
#'   \item{\code{site}}{The site list passed in, plus the buffer
#'         radius and the source URL.}
#' }
#'
#' @param lat,lon Numeric WGS-84 coordinates.
#' @param system Classification system. One of \code{"wrb2022"}
#'        (default), \code{"sibcs"}, \code{"usda"}.
#' @param buffer_m Radius in metres around the point used to gather
#'        raster pixels (default 1000 m, i.e. roughly 4 SoilGrids
#'        pixels).
#' @param source_url Path / URL of the dominant-soil raster.
#' @param top_n Keep the top N classes by probability (default 5).
#' @param verbose Emit a \code{cli} summary.
#' @return A list as described under \strong{Output}.
#'
#' @examples
#' \dontrun{
#' # Mata Atlântica, Rio de Janeiro state.
#' res <- soil_classes_at_location(
#'   lat        = -22.7,
#'   lon        = -43.7,
#'   system     = "wrb2022",
#'   source_url = "https://files.isric.org/soilgrids/latest/data/wrb/MostProbable.vrt"
#' )
#' res$distribution         # ranked list of likely RSGs
#' res$typical_attributes   # canonical thresholds per RSG to confirm
#' }
#' @seealso \code{\link{spatial_prior_soilgrids}} for the
#'          post-classification consistency check.
#' @export
soil_classes_at_location <- function(lat,
                                       lon,
                                       system     = c("wrb2022", "sibcs",
                                                      "usda"),
                                       buffer_m   = 1000,
                                       source_url = NULL,
                                       top_n      = 5,
                                       verbose    = TRUE) {
  system <- match.arg(system)
  if (!is.numeric(lat) || !is.numeric(lon) ||
        is.na(lat) || is.na(lon))
    stop("`lat` and `lon` must be numeric WGS-84 coordinates.")
  if (lat < -90 || lat > 90 || lon < -180 || lon > 180)
    stop("Coordinates out of WGS-84 range.")
  if (!requireNamespace("terra", quietly = TRUE))
    stop("Package 'terra' is required for soil_classes_at_location().")

  # Build a tiny PedonRecord just to reuse spatial_prior_soilgrids().
  ghost <- PedonRecord$new(
    site = list(id = "soil_classes_at_location-helper",
                lat = lat, lon = lon, crs = 4326),
    horizons = data.frame(top_cm = 0, bottom_cm = 5)
  )

  # Resolve the raster source (explicit > test option > NULL with warning).
  if (is.null(source_url))
    source_url <- getOption("soilKey.test_raster", default = NULL)
  if (is.null(source_url) || !nzchar(source_url)) {
    if (verbose) {
      cli::cli_alert_warning(c(
        "No SoilGrids raster source given. Pass {.arg source_url=} ",
        "or set {.code options(soilKey.test_raster = '...')}.",
        " Returning empty distribution."
      ))
    }
    return(list(
      distribution        = data.table::data.table(rsg_code = character(0),
                                                       rsg_name = character(0),
                                                       probability = numeric(0)),
      typical_attributes  = .typical_attribute_table(system, character(0)),
      site = list(lat = lat, lon = lon, crs = 4326,
                  buffer_m = buffer_m, source_url = NA_character_)
    ))
  }

  if (system == "wrb2022") {
    sg_system <- "wrb2022"
    lut       <- soilgrids_wrb_lut()
  } else if (system == "usda") {
    sg_system <- "usda"
    lut       <- soilgrids_usda_lut()
  } else {
    # SiBCS doesn't have a global SoilGrids layer; we use the WRB
    # raster + Schad (2023) Annex Table 1 to translate.
    sg_system <- "wrb2022"
    lut       <- soilgrids_wrb_lut()
  }

  dist <- tryCatch(
    spatial_prior_soilgrids(ghost,
                              system     = sg_system,
                              buffer_m   = buffer_m,
                              source_url = source_url,
                              n_classes_top = top_n,
                              lut        = lut),
    error = function(e) {
      cli::cli_alert_warning(
        "spatial_prior_soilgrids() failed: {.val {conditionMessage(e)}}"
      )
      data.table::data.table(rsg_code = character(0),
                               probability = numeric(0))
    }
  )

  # Translate WRB -> SiBCS via Schad correspondence when system='sibcs'.
  if (system == "sibcs" && nrow(dist) > 0L) {
    dist <- .wrb_to_sibcs_distribution(dist)
  }

  # Add the human-readable name column.
  if (nrow(dist) > 0L) {
    dist$rsg_name <- vapply(dist$rsg_code, .rsg_name_for_system,
                              FUN.VALUE = character(1),
                              system = system)
  } else {
    dist$rsg_name <- character(0)
  }

  # Pull the canonical attribute table for the listed classes.
  attrs <- .typical_attribute_table(system, dist$rsg_code)

  if (verbose && nrow(dist) > 0L) {
    top1 <- dist[1, ]
    cli::cli_alert_success(
      "{.field {nrow(dist)}} candidate {system} class{?es} at ({.val {lat}}, {.val {lon}}); top: {.strong {top1$rsg_name}} ({sprintf('%.0f%%', 100 * top1$probability)})"
    )
  }

  list(
    distribution        = dist,
    typical_attributes  = attrs,
    site = list(lat = lat, lon = lon, crs = 4326,
                buffer_m = buffer_m, source_url = source_url)
  )
}


# ---- internals -------------------------------------------------------------


#' Map an RSG code to a human-readable class name in the requested
#' classification system.
#' @keywords internal
.rsg_name_for_system <- function(code, system) {
  if (system == "wrb2022") {
    .wrb_rsg_full_names()[[code]] %||% code
  } else if (system == "sibcs") {
    .sibcs_ordem_full_names()[[code]] %||% code
  } else {
    .usda_order_full_names()[[code]] %||% code
  }
}

#' @keywords internal
.wrb_rsg_full_names <- function() {
  c(HS = "Histosols",  AT = "Anthrosols", TC = "Technosols",
    CR = "Cryosols",   LP = "Leptosols",  SN = "Solonetz",
    VR = "Vertisols",  SC = "Solonchaks", GL = "Gleysols",
    AN = "Andosols",   PZ = "Podzols",    PT = "Plinthosols",
    PL = "Planosols",  ST = "Stagnosols", NT = "Nitisols",
    FR = "Ferralsols", CH = "Chernozems", KS = "Kastanozems",
    PH = "Phaeozems",  UM = "Umbrisols",  DU = "Durisols",
    GY = "Gypsisols",  CL = "Calcisols",  RT = "Retisols",
    AC = "Acrisols",   LX = "Lixisols",   AL = "Alisols",
    LV = "Luvisols",   CM = "Cambisols",  AR = "Arenosols",
    FL = "Fluvisols",  RG = "Regosols")
}

#' @keywords internal
.sibcs_ordem_full_names <- function() {
  c(O = "Organossolos",  R = "Neossolos",   V = "Vertissolos",
    E = "Espodossolos",  S = "Planossolos", G = "Gleissolos",
    L = "Latossolos",    M = "Chernossolos",C = "Cambissolos",
    F = "Plintossolos",  T = "Luvissolos",  N = "Nitossolos",
    P = "Argissolos")
}

#' @keywords internal
.usda_order_full_names <- function() {
  c(GE = "Gelisols",   HI = "Histosols",   SP = "Spodosols",
    AD = "Andisols",   OX = "Oxisols",     VE = "Vertisols",
    AS = "Aridisols",  UT = "Ultisols",    MO = "Mollisols",
    AF = "Alfisols",   IN = "Inceptisols", EN = "Entisols")
}


#' Translate a WRB-RSG probability distribution to SiBCS-Ordem
#' probabilities via Schad (2023) Annex Table 1 / SiBCS 5ª ed. Annex A.
#' Many-to-many: a single WRB RSG may map to multiple SiBCS ordens
#' (we split the probability evenly).
#'
#' @keywords internal
.wrb_to_sibcs_distribution <- function(dist) {
  map <- list(
    HS = "O", AT = c("P", "C"), TC = "C", CR = "C", LP = "R",
    SN = c("P", "T"), VR = "V", SC = "P", GL = "G", AN = "C",
    PZ = "E", PT = "F", PL = "S", ST = "C", NT = "N", FR = "L",
    CH = "M", KS = "M", PH = "M", UM = "C", DU = "C", GY = "C",
    CL = "C", RT = "P", AC = "P", LX = "P", AL = "P", LV = "T",
    CM = "C", AR = "R", FL = "R", RG = "R"
  )
  rows <- list()
  for (i in seq_len(nrow(dist))) {
    wrb <- dist$rsg_code[i]
    p   <- dist$probability[i]
    sibcs <- map[[wrb]] %||% character(0)
    if (length(sibcs) == 0L) next
    share <- p / length(sibcs)
    for (s in sibcs) {
      rows[[length(rows) + 1L]] <- data.table::data.table(
        rsg_code = s, probability = share)
    }
  }
  if (length(rows) == 0L)
    return(data.table::data.table(rsg_code = character(0),
                                    probability = numeric(0)))
  out <- data.table::rbindlist(rows)
  # Aggregate per rsg_code without using `:=` (cedta-friendly).
  agg <- aggregate(probability ~ rsg_code, data = as.data.frame(out),
                     FUN = sum)
  agg <- agg[order(-agg$probability), ]
  data.table::as.data.table(agg)
}


#' Canonical attribute ranges per class, used as the
#' "what-to-confirm" appendix.
#' @keywords internal
.typical_attribute_table <- function(system, codes) {
  if (length(codes) == 0L)
    return(data.table::data.table(rsg_code = character(0),
                                    attribute = character(0),
                                    range = character(0),
                                    rationale = character(0)))
  rows <- list()
  add <- function(code, attribute, range, rationale) {
    rows[[length(rows) + 1L]] <<- data.table::data.table(
      rsg_code = code, attribute = attribute,
      range = range, rationale = rationale)
  }

  # Thresholds drawn from WRB 2022 Ch 3 / 4 + Schad 2023 keys.
  for (code in codes) {
    if (system == "wrb2022") {
      if (code == "FR") {
        add(code, "clay_pct", ">= 30 %",
            "ferralic threshold (Ch 3.1)")
        add(code, "cec_per_clay", "<= 16 cmolc/kg clay",
            "ferralic low-CEC clay")
        add(code, "delta_pH",   ">= 0 (Geric variant)",
            "differentiates Geric Ferralsol")
      } else if (code %in% c("AC", "LX", "AL", "LV")) {
        add(code, "clay_increase",
            ">= 1.4x ratio OR >= 8 %abs to Bt",
            "argic horizon")
        add(code, "bs_pct",
            if (code == "AC") "< 50 % in argic"
            else if (code == "LX") ">= 50 % in argic + low CEC"
            else if (code == "AL") "< 50 %, Al-sat >= 50 %"
            else ">= 50 % in argic + high CEC",
            "discriminator")
      } else if (code == "VR") {
        add(code, "clay_pct", ">= 30 %", "vertic horizon")
        add(code, "cracks_width_cm", ">= 0.5 cm", "vertic horizon")
        add(code, "slickensides", "common+", "vertic horizon")
      } else if (code == "PZ") {
        add(code, "spodic", "Bh / Bs / Bhs designation", "spodic horizon")
        add(code, "fe_dcb_pct or oc_pct",
            "OC + Fe accumulation in B", "spodic illuvial")
      } else if (code == "AN") {
        add(code, "phosphate_retention_pct", ">= 85 %", "andic property")
        add(code, "al_ox + 0.5*fe_ox", ">= 2 %", "andic property")
        add(code, "bulk_density_g_cm3", "<= 0.9 g/cm3", "andic property")
      } else if (code == "GL") {
        add(code, "redoximorphic_features_pct", ">= 5 % shallow",
            "gleyic properties")
      } else if (code == "PT") {
        add(code, "plinthite_pct", ">= 15 %", "plinthic horizon")
      } else {
        add(code, "(consult Ch 3-4)", "see WRB 2022",
            "canonical RSG criteria")
      }
    } else if (system == "sibcs") {
      add(code, "(consult Cap 4 + diagnoses)", "see SiBCS 5",
          "canonical Ordem criteria")
    } else if (system == "usda") {
      add(code, "(consult KST 13ed Ch 4)", "see Soil Survey Staff 2022",
          "canonical Order criteria")
    }
  }
  data.table::rbindlist(rows)
}
