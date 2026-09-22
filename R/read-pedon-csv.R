# =============================================================================
# The simple way in: a horizon spreadsheet (CSV/TSV) -> PedonRecord -> result.
# For users who do not want to type a PedonRecord by hand.
# =============================================================================


#' Read a horizon spreadsheet (CSV/TSV) into a PedonRecord
#'
#' The everyday entry point for anyone who has a soil profile in a spreadsheet:
#' one row per horizon, one column per attribute, using soilKey's canonical
#' column names (\code{top_cm}, \code{bottom_cm}, \code{designation},
#' \code{clay_pct}, \code{ph_h2o}, \code{munsell_hue_moist}, ...). The full list
#' is \code{names(horizon_column_spec())}; the quickest start is the bundled
#' template
#' \code{system.file("extdata", "perfil_exemplo.csv", package = "soilKey")}.
#'
#' Only recognised columns are used (any extras are carried through untouched),
#' so a messy export still works - soilKey simply uses the columns it
#' understands. Site metadata (id, lat/lon, soil moisture / temperature regime)
#' is optional and passed via \code{site}; without it the profile still
#' classifies, just with less specificity where site data would have refined the
#' name.
#'
#' @param file Path to a \code{.csv} (comma) or \code{.tsv} (tab) file, one row
#'   per horizon.
#' @param site Optional named list of site metadata (see \code{\link{PedonRecord}}).
#'   Defaults to \code{list(id = <file base name>)}.
#' @param sep Field separator. \code{"auto"} (default) uses a tab for
#'   \code{.tsv} files and a comma otherwise.
#' @return A \code{\link{PedonRecord}}.
#' @seealso \code{\link{classify_csv}} to go straight from a file to the three
#'   classifications; \code{\link{classify_all}}; \code{\link{PedonRecord}}.
#' @examples
#' f <- system.file("extdata", "perfil_exemplo.csv", package = "soilKey")
#' pedon <- read_pedon_csv(f)
#' classify_all(pedon)$summary
#' @export
read_pedon_csv <- function(file, site = NULL, sep = "auto") {
  if (!is.character(file) || length(file) != 1L || !nzchar(file) ||
        !file.exists(file)) {
    stop("read_pedon_csv(): 'file' must be a path to an existing .csv/.tsv file.")
  }
  if (identical(sep, "auto")) {
    sep <- if (grepl("\\.tsv$", file, ignore.case = TRUE)) "\t" else ","
  }
  df <- utils::read.csv(file, sep = sep, stringsAsFactors = FALSE)
  spec <- names(horizon_column_spec())
  keep <- intersect(spec, names(df))
  if (length(keep) == 0L) {
    stop("read_pedon_csv(): no recognised horizon columns found in '", file,
         "'.\n  Expected canonical names such as top_cm, bottom_cm, clay_pct, ",
         "ph_h2o, ...\n  See names(horizon_column_spec()), or copy the template ",
         "system.file('extdata', 'perfil_exemplo.csv', package = 'soilKey').")
  }
  extra <- setdiff(names(df), spec)
  df <- df[, c(keep, extra), drop = FALSE]
  if (is.null(site)) {
    site <- list(id = tools::file_path_sans_ext(basename(file)))
  }
  PedonRecord$new(site = site, horizons = df)
}


#' Classify a horizon spreadsheet in all three systems - one file, one line
#'
#' The shortest path from a spreadsheet to an answer. Reads \code{file} with
#' \code{\link{read_pedon_csv}} and returns the WRB 2022 / SiBCS / USDA names as
#' a one-row \code{data.frame}. Missing attributes are handled silently
#' (\code{on_missing = "silent"}).
#'
#' The full \code{\link{ClassificationResult}} objects (with the key trace,
#' evidence grade, qualifiers, ...) and the parsed \code{\link{PedonRecord}} are
#' attached as attributes \code{"results"} and \code{"pedon"} for anyone who
#' wants to dig deeper.
#'
#' @inheritParams read_pedon_csv
#' @param systems Character vector of systems to run; any of \code{"wrb"},
#'   \code{"sibcs"}, \code{"usda"} (default: all three).
#' @return A one-row \code{data.frame} with one column per system.
#' @seealso \code{\link{read_pedon_csv}}, \code{\link{classify_all}}.
#' @examples
#' f <- system.file("extdata", "perfil_exemplo.csv", package = "soilKey")
#' classify_csv(f)
#' @export
classify_csv <- function(file, site = NULL, sep = "auto",
                         systems = c("wrb", "sibcs", "usda")) {
  pedon <- read_pedon_csv(file, site = site, sep = sep)
  res <- classify_all(pedon, systems = systems, on_missing = "silent")
  out <- res$summary
  attr(out, "results") <- res
  attr(out, "pedon")   <- pedon
  out
}
