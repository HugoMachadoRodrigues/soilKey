#' PedonRecord: structured representation of a single pedon
#'
#' The central data carrier in soilKey. A PedonRecord bundles everything we
#' know about one soil profile: site metadata, the horizons table (with a
#' fixed canonical schema — see \code{\link{horizon_column_spec}}),
#' optional Vis-NIR/MIR spectra, profile photographs, source documents, and
#' a provenance log that records, per (horizon, attribute) pair, where each
#' value came from (\code{measured}, \code{extracted_vlm},
#' \code{predicted_spectra}, \code{inferred_prior}, \code{user_assumed}).
#'
#' All diagnostic functions (\code{\link{argic}}, \code{\link{ferralic}},
#' \code{\link{mollic}}, ...) consume a PedonRecord directly. The
#' provenance log is what allows the final
#' \code{\link{ClassificationResult}} to assign a meaningful evidence
#' grade.
#'
#' @field site       List. Site-level metadata: \code{lat}, \code{lon},
#'                   \code{crs} (default 4326), \code{date},
#'                   \code{country}, \code{elevation_m}, \code{slope_pct},
#'                   \code{aspect_deg}, \code{landform},
#'                   \code{parent_material}, \code{land_use},
#'                   \code{vegetation}, \code{drainage_class},
#'                   plus an arbitrary \code{id}.
#' @field horizons   data.table with the canonical horizon schema.
#' @field spectra    List with optional \code{vnir} matrix (rows =
#'                   horizons, cols = wavelengths in nm), \code{mir}
#'                   matrix, and \code{metadata} list.
#' @field images     List of named lists describing profile photographs.
#' @field documents  List of named lists describing source documents.
#' @field provenance data.table with columns \code{horizon_idx},
#'                   \code{attribute}, \code{source}, \code{confidence},
#'                   \code{notes}.
#'
#' @export
PedonRecord <- R6::R6Class("PedonRecord",
  public = list(

    site       = NULL,
    horizons   = NULL,
    spectra    = NULL,
    images     = NULL,
    documents  = NULL,
    provenance = NULL,

    #' @description Construct a PedonRecord.
    #' @param site List of site-level metadata.
    #' @param horizons data.frame/data.table of horizons.
    #' @param spectra Optional list with \code{vnir}, \code{mir},
    #'                \code{metadata}.
    #' @param images Optional list of image descriptors.
    #' @param documents Optional list of document descriptors.
    #' @param provenance Optional provenance data.table; if NULL, an empty
    #'                   one is created.
    initialize = function(site       = NULL,
                          horizons   = NULL,
                          spectra    = NULL,
                          images     = NULL,
                          documents  = NULL,
                          provenance = NULL) {
      self$site       <- site
      self$horizons   <- ensure_horizon_schema(horizons)
      self$spectra    <- spectra
      self$images     <- images
      self$documents  <- documents
      self$provenance <- if (is.null(provenance)) make_empty_provenance() else provenance
    },

    #' @description Validate the record against soil-physical sanity rules.
    #'
    #' Checks: top < bottom for every horizon; no overlapping depths;
    #' clay+silt+sand sum to 100 ± 2 where all three are reported; pH
    #' values plausible (1..12); CEC >= sum of exchangeable bases (Ca, Mg,
    #' K, Na); Munsell value/chroma in plausible ranges; coarse fragments
    #' percent in [0, 100]; OC% reasonable (<60); site lat/lon within
    #' geographic ranges. Returns a list with \code{valid}, \code{errors},
    #' \code{warnings}, \code{n_horizons}.
    #'
    #' @param strict If \code{TRUE}, throws on errors instead of returning.
    #' @param verbose If \code{TRUE}, prints messages via cli.
    #' @return Invisibly, a list summarising the validation outcome.
    validate = function(strict = FALSE, verbose = TRUE) {
      errors   <- character()
      warnings <- character()
      h        <- self$horizons

      if (is.null(h) || nrow(h) == 0) {
        errors <- c(errors, "No horizons defined")
      } else {

        # 1. top < bottom
        bad <- which(!is.na(h$top_cm) & !is.na(h$bottom_cm) &
                       h$top_cm >= h$bottom_cm)
        if (length(bad) > 0) {
          errors <- c(errors, sprintf(
            "Horizons %s have top_cm >= bottom_cm",
            paste(bad, collapse = ", ")
          ))
        }

        # 2. No overlap (sorted by depth)
        if (nrow(h) > 1L && !any(is.na(h$top_cm)) && !any(is.na(h$bottom_cm))) {
          ord <- order(h$top_cm)
          h_sorted <- h[ord, ]
          gaps <- h_sorted$top_cm[-1] - h_sorted$bottom_cm[-nrow(h_sorted)]
          overlaps <- which(gaps < -0.01)
          if (length(overlaps) > 0) {
            warnings <- c(warnings, sprintf(
              "Horizons may overlap at sorted indices %s",
              paste(ord[overlaps + 1L], collapse = ", ")
            ))
          }
        }

        # 3. Texture sums to ~100
        has_texture <- !is.na(h$clay_pct) & !is.na(h$silt_pct) & !is.na(h$sand_pct)
        if (any(has_texture)) {
          sums <- h$clay_pct[has_texture] + h$silt_pct[has_texture] +
                  h$sand_pct[has_texture]
          bad <- which(abs(sums - 100) > 2)
          if (length(bad) > 0) {
            idx <- which(has_texture)[bad]
            errors <- c(errors, sprintf(
              "clay+silt+sand != 100 +/- 2 in horizons %s (sums: %s)",
              paste(idx, collapse = ", "),
              paste(round(sums[bad], 1), collapse = ", ")
            ))
          }
        }

        # 4. pH plausibility
        for (ph_col in c("ph_h2o", "ph_kcl", "ph_cacl2")) {
          vals <- h[[ph_col]]
          bad <- which(!is.na(vals) & (vals < 1 | vals > 12))
          if (length(bad) > 0) {
            errors <- c(errors, sprintf(
              "Implausible %s values at horizons %s",
              ph_col, paste(bad, collapse = ", ")
            ))
          }
        }

        # 5. CEC >= sum of exchangeable bases
        has_bases <- !is.na(h$cec_cmol) & !is.na(h$ca_cmol) &
                     !is.na(h$mg_cmol) & !is.na(h$k_cmol) & !is.na(h$na_cmol)
        if (any(has_bases)) {
          sum_bases <- h$ca_cmol + h$mg_cmol + h$k_cmol + h$na_cmol
          bad <- which(has_bases & sum_bases > h$cec_cmol + 0.5)
          if (length(bad) > 0) {
            warnings <- c(warnings, sprintf(
              "Sum of bases > CEC at horizons %s (measurement inconsistency)",
              paste(bad, collapse = ", ")
            ))
          }
        }

        # 6. Munsell ranges
        for (val_col in c("munsell_value_moist", "munsell_value_dry")) {
          vals <- h[[val_col]]
          bad <- which(!is.na(vals) & (vals < 0 | vals > 10))
          if (length(bad) > 0) {
            errors <- c(errors, sprintf(
              "Implausible %s at horizons %s",
              val_col, paste(bad, collapse = ", ")
            ))
          }
        }
        for (chroma_col in c("munsell_chroma_moist", "munsell_chroma_dry")) {
          vals <- h[[chroma_col]]
          bad <- which(!is.na(vals) & (vals < 0 | vals > 8))
          if (length(bad) > 0) {
            errors <- c(errors, sprintf(
              "Implausible %s at horizons %s",
              chroma_col, paste(bad, collapse = ", ")
            ))
          }
        }

        # 7. Coarse fragments
        bad <- which(!is.na(h$coarse_fragments_pct) &
                       (h$coarse_fragments_pct < 0 |
                          h$coarse_fragments_pct > 100))
        if (length(bad) > 0) {
          errors <- c(errors, sprintf(
            "Implausible coarse_fragments_pct at horizons %s",
            paste(bad, collapse = ", ")
          ))
        }

        # 8. OC sanity
        bad <- which(!is.na(h$oc_pct) & (h$oc_pct < 0 | h$oc_pct > 60))
        if (length(bad) > 0) {
          warnings <- c(warnings, sprintf(
            "Implausible oc_pct (>60) at horizons %s",
            paste(bad, collapse = ", ")
          ))
        }

        # 9. BS percent
        bad <- which(!is.na(h$bs_pct) & (h$bs_pct < 0 | h$bs_pct > 100))
        if (length(bad) > 0) {
          errors <- c(errors, sprintf(
            "Implausible bs_pct (outside [0,100]) at horizons %s",
            paste(bad, collapse = ", ")
          ))
        }

        # 10. EC, plinthite_pct, redoximorphic_features_pct (v0.2 additions)
        bad <- which(!is.na(h$ec_dS_m) & h$ec_dS_m < 0)
        if (length(bad) > 0) {
          errors <- c(errors, sprintf(
            "Implausible ec_dS_m (< 0) at horizons %s",
            paste(bad, collapse = ", ")
          ))
        }
        for (vol_col in c("plinthite_pct", "redoximorphic_features_pct")) {
          vals <- h[[vol_col]]
          bad <- which(!is.na(vals) & (vals < 0 | vals > 100))
          if (length(bad) > 0) {
            errors <- c(errors, sprintf(
              "Implausible %s (outside [0,100]) at horizons %s",
              vol_col, paste(bad, collapse = ", ")
            ))
          }
        }
      }

      # 10. Site coordinates
      if (!is.null(self$site)) {
        if (!is.null(self$site$lat) && length(self$site$lat) == 1 &&
            (self$site$lat < -90 || self$site$lat > 90)) {
          errors <- c(errors, sprintf("Implausible latitude: %s", self$site$lat))
        }
        if (!is.null(self$site$lon) && length(self$site$lon) == 1 &&
            (self$site$lon < -180 || self$site$lon > 180)) {
          errors <- c(errors, sprintf("Implausible longitude: %s", self$site$lon))
        }
      }

      result <- list(
        valid      = length(errors) == 0,
        errors     = errors,
        warnings   = warnings,
        n_horizons = if (is.null(h)) 0L else nrow(h)
      )

      if (verbose) {
        for (e in errors)   cli::cli_alert_danger(e)
        for (w in warnings) cli::cli_alert_warning(w)
        if (length(errors) == 0L && length(warnings) == 0L) {
          cli::cli_alert_success(
            sprintf("PedonRecord validates: %d horizons OK", result$n_horizons)
          )
        }
      }

      if (strict && length(errors) > 0L) {
        rlang::abort(sprintf(
          "PedonRecord validation failed: %d errors", length(errors)
        ))
      }

      invisible(result)
    },

    #' @description Coerce to an aqp \code{SoilProfileCollection}.
    #' @return A \code{SoilProfileCollection}. Requires the \code{aqp}
    #'         package.
    to_aqp = function() {
      if (!requireNamespace("aqp", quietly = TRUE)) {
        rlang::abort(
          "Package 'aqp' is required for to_aqp() -- install with install.packages('aqp')"
        )
      }
      if (is.null(self$horizons) || nrow(self$horizons) == 0L) {
        rlang::abort("Cannot convert empty PedonRecord to SoilProfileCollection")
      }

      id <- self$site$id %||% "P1"
      hz <- as.data.frame(data.table::copy(self$horizons))
      hz$profile_id <- id

      spc <- hz
      aqp::depths(spc) <- profile_id ~ top_cm + bottom_cm

      if (!is.null(self$site)) {
        site_df <- data.frame(profile_id = id, stringsAsFactors = FALSE)
        for (k in setdiff(names(self$site), "id")) {
          val <- self$site[[k]]
          if (length(val) == 1L && !is.list(val)) site_df[[k]] <- val
        }
        suppressWarnings(aqp::site(spc) <- site_df)
      }

      spc
    },

    #' @description Populate this record from an aqp
    #'              \code{SoilProfileCollection}.
    #' @param spc A \code{SoilProfileCollection}.
    #' @param top_col Name of the top-depth column in \code{spc} (mapped to
    #'                \code{top_cm}).
    #' @param bottom_col Name of the bottom-depth column (mapped to
    #'                   \code{bottom_cm}).
    #' @return Invisibly self (mutated in place).
    from_aqp = function(spc, top_col = "top_cm", bottom_col = "bottom_cm") {
      if (!requireNamespace("aqp", quietly = TRUE)) {
        rlang::abort("Package 'aqp' is required for from_aqp()")
      }
      hz <- data.table::as.data.table(aqp::horizons(spc))
      site_df <- aqp::site(spc)

      if (top_col != "top_cm" && top_col %in% names(hz)) {
        data.table::setnames(hz, top_col, "top_cm")
      }
      if (bottom_col != "bottom_cm" && bottom_col %in% names(hz)) {
        data.table::setnames(hz, bottom_col, "bottom_cm")
      }

      id_col <- aqp::idname(spc)
      if (id_col %in% names(hz)) hz[, (id_col) := NULL]

      self$horizons <- ensure_horizon_schema(hz)

      if (nrow(site_df) >= 1L) {
        self$site <- as.list(site_df[1L, , drop = FALSE])
      }

      invisible(self)
    },

    #' @description Add a measurement (or extracted/predicted value) and
    #'              record its provenance.
    #' @param horizon_idx Integer horizon index (1-based).
    #' @param attribute Name of the horizon column to set.
    #' @param value New value for that cell.
    #' @param source One of "measured", "extracted_vlm",
    #'               "predicted_spectra", "inferred_prior", "user_assumed".
    #' @param confidence Numeric in [0, 1].
    #' @param notes Optional free-text note.
    #' @param overwrite If \code{FALSE} (default) and the cell already has
    #'                  a value from a more authoritative source, leave it
    #'                  alone. If \code{TRUE}, overwrite.
    #' @return Invisibly self.
    add_measurement = function(horizon_idx,
                                attribute,
                                value,
                                source     = "measured",
                                confidence = 1.0,
                                notes      = NA_character_,
                                overwrite  = FALSE) {

      if (!source %in% valid_provenance_sources()) {
        rlang::abort(sprintf(
          "source must be one of: %s",
          paste(valid_provenance_sources(), collapse = ", ")
        ))
      }
      if (!attribute %in% names(self$horizons)) {
        rlang::abort(sprintf(
          "attribute '%s' not in horizon schema", attribute
        ))
      }
      horizon_idx <- as.integer(horizon_idx)
      if (length(horizon_idx) != 1L ||
          horizon_idx < 1L ||
          horizon_idx > nrow(self$horizons)) {
        rlang::abort(sprintf(
          "horizon_idx %s out of range (1..%d)",
          horizon_idx, nrow(self$horizons)
        ))
      }

      # Authority check: if the cell already has a value with a higher-
      # authority source, keep it unless overwrite = TRUE.
      if (!overwrite) {
        prior <- self$provenance[
          self$provenance$horizon_idx == horizon_idx &
          self$provenance$attribute   == attribute, ]
        if (nrow(prior) > 0L) {
          best <- max(provenance_authority(prior$source), na.rm = TRUE)
          if (provenance_authority(source) < best) {
            return(invisible(self))
          }
        }
      }

      self$horizons[[attribute]][horizon_idx] <- value

      new_prov <- data.table::data.table(
        horizon_idx = horizon_idx,
        attribute   = attribute,
        source      = source,
        confidence  = as.numeric(confidence),
        notes       = as.character(notes)
      )
      self$provenance <- if (is.null(self$provenance) ||
                             nrow(self$provenance) == 0L) {
        new_prov
      } else {
        data.table::rbindlist(list(self$provenance, new_prov), fill = TRUE)
      }

      invisible(self)
    },

    #' @description Compact summary list (for serialization or testing).
    summary = function(...) {
      list(
        n_horizons   = if (is.null(self$horizons)) 0L else nrow(self$horizons),
        depth_range  = if (is.null(self$horizons) || nrow(self$horizons) == 0L) {
                          c(NA_real_, NA_real_)
                       } else {
                          c(min(self$horizons$top_cm,    na.rm = TRUE),
                            max(self$horizons$bottom_cm, na.rm = TRUE))
                       },
        has_site         = !is.null(self$site),
        has_spectra      = !is.null(self$spectra),
        n_images         = length(self$images %||% list()),
        n_documents      = length(self$documents %||% list()),
        provenance_rows  = if (is.null(self$provenance)) 0L else nrow(self$provenance)
      )
    },

    #' @description Pretty-print the record.
    print = function(...) {
      cli::cli_h2("PedonRecord")

      if (!is.null(self$site)) {
        pieces <- character()
        if (!is.null(self$site$id))      pieces <- c(pieces, paste0("id=", self$site$id))
        if (!is.null(self$site$lat) && !is.null(self$site$lon)) {
          pieces <- c(pieces, sprintf("(%.4f, %.4f)",
                                       self$site$lat, self$site$lon))
        }
        if (!is.null(self$site$country)) pieces <- c(pieces, self$site$country)
        if (!is.null(self$site$date))    pieces <- c(pieces, as.character(self$site$date))
        if (!is.null(self$site$parent_material)) {
          pieces <- c(pieces, paste0("on ", self$site$parent_material))
        }
        cli::cli_text("Site: {paste(pieces, collapse = ' | ')}")
      } else {
        cli::cli_text("Site: <none>")
      }

      h <- self$horizons
      if (!is.null(h) && nrow(h) > 0L) {
        cli::cli_text("Horizons ({nrow(h)}):")
        for (i in seq_len(nrow(h))) {
          designation <- h$designation[i] %||% "-"
          if (is.na(designation)) designation <- "-"
          cli::cli_text(sprintf(
            "  %2d) %-5s %5.0f-%-5.0f cm  clay=%-5s silt=%-5s sand=%-5s  CEC=%-5s pH=%-4s OC=%-4s",
            i,
            designation,
            h$top_cm[i] %||% NA_real_,
            h$bottom_cm[i] %||% NA_real_,
            fmt_num(h$clay_pct[i],  ""),
            fmt_num(h$silt_pct[i],  ""),
            fmt_num(h$sand_pct[i],  ""),
            fmt_num(h$cec_cmol[i],  ""),
            fmt_num(h$ph_h2o[i],    ""),
            fmt_num(h$oc_pct[i],    "")
          ))
        }
      } else {
        cli::cli_text("Horizons: <none>")
      }

      if (!is.null(self$spectra)) {
        cli::cli_text("Spectra: {paste(names(self$spectra), collapse = ', ')}")
      }
      if (!is.null(self$images) && length(self$images) > 0L) {
        cli::cli_text("Images: {length(self$images)}")
      }
      if (!is.null(self$documents) && length(self$documents) > 0L) {
        cli::cli_text("Documents: {length(self$documents)}")
      }
      if (!is.null(self$provenance) && nrow(self$provenance) > 0L) {
        cli::cli_text("Provenance entries: {nrow(self$provenance)}")
      }

      invisible(self)
    }
  )
)
