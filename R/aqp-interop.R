# =============================================================================
# v0.9.34 -- Interoperability with the {aqp} (Algorithms for Quantitative
# Pedology) package.
#
# {aqp}'s SoilProfileCollection (SPC) is the canonical R representation for
# pedological data. Wirin g `as_aqp()` and `from_aqp()` lets every aqp user
# adopt soilKey without rewriting their pipelines.
#
# Conventions:
#
#   soilKey                       aqp
#   ---------------------------   ---------------------------------------
#   pedon$site$id                 idcol (default: "id")
#   pedon$horizons$top_cm         "top"     (depthcols[1])
#   pedon$horizons$bottom_cm      "bottom"  (depthcols[2])
#   pedon$horizons$designation    "name"    (horizon-name slot)
#   pedon$horizons$clay_pct       "clay"
#   pedon$horizons$sand_pct       "sand"
#   pedon$horizons$silt_pct       "silt"
#   pedon$horizons$ph_h2o         "ph_h2o"  (kept verbatim; aqp tolerant)
#   pedon$horizons$oc_pct         "oc_pct"  (kept verbatim)
#   ... (other soilKey-specific columns are passed through unchanged) ...
#
# Round-trip property: from_aqp(as_aqp(pedon)) == pedon (modulo column-order
# canonicalisation).
# =============================================================================


# Canonical column-name map used by both as_aqp and from_aqp.
# Keys = soilKey names; values = aqp names.
.SOILKEY_TO_AQP <- c(
  top_cm    = "top",
  bottom_cm = "bottom",
  designation = "name",
  clay_pct  = "clay",
  sand_pct  = "sand",
  silt_pct  = "silt"
)
.AQP_TO_SOILKEY <- setNames(names(.SOILKEY_TO_AQP), unname(.SOILKEY_TO_AQP))


#' Convert one or more PedonRecord objects to an aqp SoilProfileCollection
#'
#' Builds a \code{aqp::SoilProfileCollection} from one \code{PedonRecord}
#' or a list of them. Standard soilKey columns (\code{top_cm},
#' \code{bottom_cm}, \code{designation}, \code{clay_pct}, \code{sand_pct},
#' \code{silt_pct}) are renamed to aqp's canonical convention (\code{top},
#' \code{bottom}, \code{name}, \code{clay}, \code{sand}, \code{silt}).
#' All other columns are passed through unchanged. Site-level slots
#' (\code{lat}, \code{lon}, \code{country}, \code{parent_material},
#' \code{reference_*}, \code{nasis_diagnostic_features}, etc.) are
#' attached to the SPC's site table.
#'
#' Requires the \code{aqp} package, listed in Suggests; the function
#' raises a clear error if aqp is not installed.
#'
#' @param x A \code{\link{PedonRecord}} or a list of them.
#' @return A \code{aqp::SoilProfileCollection}.
#' @seealso \code{\link{from_aqp}}, the inverse conversion.
#' @examples
#' \dontrun{
#' library(soilKey)
#' library(aqp)
#'
#' pedons <- list(make_ferralsol_canonical(), make_luvisol_canonical())
#' spc <- as_aqp(pedons)
#' length(spc)         # 2 profiles
#' aqp::horizons(spc)  # one row per horizon, aqp-named columns
#' }
#' @export
as_aqp <- function(x) {
  if (!requireNamespace("aqp", quietly = TRUE))
    stop("Package 'aqp' is required for as_aqp(). Install it with ",
         "`install.packages(\"aqp\")`.")

  # Coerce single pedon to a list of one.
  pedons <- if (inherits(x, "PedonRecord")) list(x)
            else if (is.list(x) && length(x) > 0L &&
                       all(vapply(x, inherits, logical(1), "PedonRecord")))
              x
            else
              stop("`x` must be a PedonRecord or a list of PedonRecord objects.")

  # Build per-profile horizon and site rows.
  hzn_rows  <- list()
  site_rows <- list()
  for (i in seq_along(pedons)) {
    p <- pedons[[i]]
    pid <- p$site$id %||% paste0("pedon-", i)
    h <- as.data.frame(p$horizons)
    if (nrow(h) == 0L) next
    h$id <- as.character(pid)
    # Rename soilKey columns to aqp canonical names.
    for (k in names(.SOILKEY_TO_AQP)) {
      if (k %in% names(h)) {
        names(h)[names(h) == k] <- .SOILKEY_TO_AQP[[k]]
      }
    }
    hzn_rows[[i]] <- h

    # Site-level row. Drop list columns (e.g. nasis_diagnostic_features
    # is a vector, kept verbatim; provenance / images are dropped).
    sl <- p$site
    sl_flat <- sl[vapply(sl, function(v) is.atomic(v) && length(v) <= 1L,
                            logical(1))]
    sl_df <- as.data.frame(sl_flat, stringsAsFactors = FALSE)
    sl_df$id <- as.character(pid)
    site_rows[[i]] <- sl_df
  }

  hzns <- .bind_rows_pad(hzn_rows)
  if (is.null(hzns) || nrow(hzns) == 0L)
    stop("No horizons in input -- cannot build SoilProfileCollection.")

  aqp::depths(hzns) <- id ~ top + bottom
  spc <- hzns

  # Attach site-level data.
  sites <- .bind_rows_pad(site_rows)
  if (!is.null(sites) && nrow(sites) > 0L)
    aqp::site(spc) <- sites

  spc
}


#' Convert an aqp SoilProfileCollection back to a list of PedonRecord
#'
#' Inverse of \code{\link{as_aqp}}. Walks each profile in the SPC,
#' renames aqp's canonical horizon column names back to soilKey's
#' (\code{top} -> \code{top_cm}, \code{name} -> \code{designation},
#' \code{clay} -> \code{clay_pct}, ...), assembles a
#' \code{\link{PedonRecord}} per profile, and returns the list.
#'
#' Round-trip property: \code{from_aqp(as_aqp(pedon))} reproduces
#' \code{pedon} modulo column ordering.
#'
#' @param spc A \code{aqp::SoilProfileCollection}.
#' @return A list of \code{\link{PedonRecord}} objects (length =
#'         \code{length(spc)}).
#' @seealso \code{\link{as_aqp}}, the forward conversion.
#' @examples
#' \dontrun{
#' pedons <- list(make_ferralsol_canonical(), make_luvisol_canonical())
#' spc <- as_aqp(pedons)
#' pedons2 <- from_aqp(spc)
#' identical(pedons[[1]]$horizons$clay_pct, pedons2[[1]]$horizons$clay_pct)
#' #> [1] TRUE
#' }
#' @export
from_aqp <- function(spc) {
  if (!requireNamespace("aqp", quietly = TRUE))
    stop("Package 'aqp' is required for from_aqp(). Install it with ",
         "`install.packages(\"aqp\")`.")
  if (!inherits(spc, "SoilProfileCollection"))
    stop("`spc` must be a aqp::SoilProfileCollection.")

  hzns_all  <- aqp::horizons(spc)
  sites_all <- aqp::site(spc)
  idcol     <- aqp::idname(spc)
  ids       <- aqp::profile_id(spc)

  # Rename aqp columns back to soilKey conventions.
  rename_back <- function(df) {
    for (k in names(.AQP_TO_SOILKEY)) {
      if (k %in% names(df)) {
        names(df)[names(df) == k] <- .AQP_TO_SOILKEY[[k]]
      }
    }
    df
  }
  hzns_all <- rename_back(hzns_all)

  out <- vector("list", length(ids))
  for (i in seq_along(ids)) {
    pid <- ids[[i]]
    h_rows <- hzns_all[hzns_all[[idcol]] == pid, , drop = FALSE]
    # Drop the id column and the auto-generated hzID before storing
    # in the PedonRecord (kept implicit in soilKey via row order).
    h_rows[[idcol]] <- NULL
    if ("hzID" %in% names(h_rows)) h_rows$hzID <- NULL

    s_row <- sites_all[sites_all[[idcol]] == pid, , drop = FALSE]
    s_list <- if (nrow(s_row) > 0L) {
      l <- as.list(s_row[1, , drop = FALSE])
      l[[idcol]] <- NULL
      l$id <- pid
      l
    } else {
      list(id = pid)
    }

    out[[i]] <- PedonRecord$new(
      site     = s_list,
      horizons = ensure_horizon_schema(data.table::as.data.table(h_rows))
    )
  }
  out
}


# Helper: rbind a list of data.frames where columns may differ between
# rows. Pads missing columns with NA. Returns a single data.frame.
# Uses data.table::rbindlist with fill=TRUE -- robust to schema
# mismatches and avoids the matrix-coercion gotcha that base rbind
# triggers when type-promoting heterogeneous columns.
.bind_rows_pad <- function(lst) {
  lst <- lst[!vapply(lst, is.null, logical(1))]
  if (length(lst) == 0L) return(NULL)
  lst_dt <- lapply(lst, data.table::as.data.table)
  out <- data.table::rbindlist(lst_dt, fill = TRUE)
  as.data.frame(out, stringsAsFactors = FALSE)
}
