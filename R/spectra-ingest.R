# =============================================================================
# Spectral-dataset ingestion (v0.9.148)
#
# Scaffolding to turn an ARBITRARY Vis-NIR / MIR reflectance + lab-label table
# (e.g. a Brazilian spectral library) into the two objects the package's
# spectral engine already consumes:
#
#   * an OSSL-shaped calibration library  list(Xr, Yr, metadata)  -- the input
#     to fill_from_spectra(ossl_library=) / classify_by_spectral_neighbours();
#   * PedonRecords with $spectra$vnir attached -- the query objects for
#     classify_*(gapfill = list(method = "spectra", ossl_library = <lib>)).
#
# Plus benchmark_spectral_fill(): an honest ON/OFF measurement of the accuracy
# lift the spectra buy, the number that has been data-blocked until now.
#
# The engine (predict_ossl_*, preprocess_spectra, fill_from_spectra,
# classify_by_spectral_neighbours, the spectra$vnir slot, ossl_library_template)
# is unchanged; this file only adds the reader/binder/benchmark glue. All of it
# is opt-in -- no default classification path calls it, so classification stays
# byte-identical.
# =============================================================================

# Canonical continuous properties the spectral models calibrate (matches
# fill_from_spectra() defaults + ossl_demo_sa$Yr), with header aliases incl.
# Portuguese (a BR library is the motivating use-case).
.SPECTRA_PROPERTY_ALIASES <- list(
  clay_pct   = c("clay_pct", "clay", "argila", "clay_g_kg", "clay_percent"),
  sand_pct   = c("sand_pct", "sand", "areia", "sand_total", "areia_total"),
  silt_pct   = c("silt_pct", "silt", "silte"),
  cec_cmol   = c("cec_cmol", "cec", "ctc", "cec_nh4", "t_value"),
  bs_pct     = c("bs_pct", "bs", "v_pct", "v", "base_saturation", "sat_bases"),
  ph_h2o     = c("ph_h2o", "ph", "ph_water", "ph_agua", "ph_h2o_1_2_5"),
  oc_pct     = c("oc_pct", "oc", "soc", "carbono", "c_org", "organic_carbon",
                 "carbono_organico"),
  fe_dcb_pct = c("fe_dcb_pct", "fe_dcb", "fe_dith", "fed", "ferro_dcb"),
  caco3_pct  = c("caco3_pct", "caco3", "carbonates", "carbonato")
)

# Taxonomic label columns consumed by classify_by_spectral_neighbours().
.SPECTRA_LABEL_ALIASES <- list(
  wrb_rsg     = c("wrb_rsg", "wrb", "rsg", "wrb2022", "reference_wrb"),
  sibcs_ordem = c("sibcs_ordem", "sibcs", "ordem", "ordem_sibcs",
                  "reference_sibcs", "reference_sibcs_order"),
  usda_order  = c("usda_order", "usda", "order", "soil_order",
                  "reference_st", "reference_usda")
)

.SPECTRA_DEPTH_ALIASES <- list(
  top_cm    = c("top_cm", "top", "upper", "prof_sup", "topo", "hzn_top"),
  bottom_cm = c("bottom_cm", "bottom", "lower", "prof_inf", "base", "hzn_bot")
)

# Resolve user columns to canonical names: explicit `map` wins, else the alias
# table (case-insensitive, separators normalised). Returns a named character
# vector canonical -> actual-column (only those found).
.resolve_spectral_cols <- function(cols, aliases, map = NULL) {
  norm <- function(x) gsub("[^a-z0-9]", "", tolower(x))
  ncols <- norm(cols)
  out <- character(0)
  for (canon in names(aliases)) {
    actual <- NA_character_
    if (!is.null(map) && !is.null(map[[canon]]) && map[[canon]] %in% cols) {
      actual <- map[[canon]]
    } else {
      cand <- norm(aliases[[canon]])
      hit  <- which(ncols %in% cand)
      if (length(hit)) actual <- cols[hit[1L]]
    }
    if (!is.na(actual)) out[[canon]] <- actual
  }
  out
}

# Coerce a reflectance input (wide matrix/df with wavelength-named columns, OR
# a long data.frame id/wavelength_nm/reflectance) to list(ids, wavelengths, mat).
.coerce_reflectance <- function(reflectance, id_col = "id", wavelengths = NULL) {
  if (is.character(reflectance) && length(reflectance) == 1L &&
        file.exists(reflectance)) {
    reflectance <- utils::read.csv(reflectance, check.names = FALSE,
                                   stringsAsFactors = FALSE)
  }
  # Long format?
  if (is.data.frame(reflectance) &&
        all(c(id_col, "wavelength_nm", "reflectance") %in% names(reflectance))) {
    wl   <- sort(unique(as.numeric(reflectance$wavelength_nm)))
    ids  <- unique(reflectance[[id_col]])
    mat  <- matrix(NA_real_, nrow = length(ids), ncol = length(wl),
                   dimnames = list(as.character(ids), as.character(wl)))
    ir <- match(reflectance[[id_col]], ids)
    ic <- match(as.numeric(reflectance$wavelength_nm), wl)
    mat[cbind(ir, ic)] <- as.numeric(reflectance$reflectance)
    return(list(ids = ids, wavelengths = wl, mat = mat))
  }
  # Wide: a matrix or data.frame, rows = samples.
  df <- reflectance
  ids <- NULL
  if (is.data.frame(df)) {
    if (id_col %in% names(df)) { ids <- df[[id_col]]; df[[id_col]] <- NULL }
    # drop any non-numeric carrier columns
    num <- vapply(df, is.numeric, logical(1))
    df  <- df[, num, drop = FALSE]
    mat <- as.matrix(df)
  } else {
    mat <- as.matrix(df)
  }
  if (is.null(ids)) ids <- rownames(mat) %||% seq_len(nrow(mat))
  wl <- if (!is.null(wavelengths)) as.numeric(wavelengths)
        else suppressWarnings(as.numeric(colnames(mat)))
  if (is.null(wl) || all(is.na(wl)))
    rlang::abort(paste0("Cannot infer wavelengths: pass `wavelengths=` or give ",
                        "reflectance columns named by wavelength (nm)."))
  colnames(mat) <- as.character(wl)
  list(ids = ids, wavelengths = wl, mat = mat)
}

# Percent-reflectance (values plausibly in 0-100) -> fraction in [0,1].
.spectra_to_fraction <- function(mat, normalize = c("auto", "none", "percent")) {
  normalize <- match.arg(normalize)
  if (normalize == "none") return(mat)
  mx <- suppressWarnings(max(mat, na.rm = TRUE))
  if (normalize == "percent" || (normalize == "auto" && is.finite(mx) && mx > 1.5))
    mat <- mat / 100
  mat
}

# Linearly resample each spectrum (row) from `wl` to `target` (reuses the same
# rule(2)-style clamp as the depth interpolator). NA-safe per row.
.resample_spectra <- function(mat, wl, target) {
  if (is.null(target) || (length(target) == length(wl) &&
                            all(abs(target - wl) < 1e-9))) return(mat)
  out <- matrix(NA_real_, nrow = nrow(mat), ncol = length(target),
                dimnames = list(rownames(mat), as.character(target)))
  for (i in seq_len(nrow(mat))) {
    y <- mat[i, ]
    ok <- is.finite(y) & is.finite(wl)
    if (sum(ok) < 2L) next
    out[i, ] <- stats::approx(wl[ok], y[ok], xout = target, rule = 2)$y
  }
  out
}

#' Read a Vis-NIR / MIR reflectance + lab table into an OSSL-shaped library
#'
#' Turns an arbitrary spectral dataset (e.g. a Brazilian Vis-NIR/MIR library)
#' into the canonical \code{list(Xr, Yr, metadata)} object consumed by
#' \code{\link{fill_from_spectra}} and
#' \code{\link{classify_by_spectral_neighbours}}. Column names are mapped to the
#' package's canonical attributes (clay_pct, sand_pct, ..., and the taxonomic
#' label columns \code{wrb_rsg} / \code{sibcs_ordem} / \code{usda_order}) via a
#' built-in alias table (including Portuguese headers such as
#' \emph{argila} / \emph{silte} / \emph{carbono}) or an explicit
#' \code{property_map} / \code{label_map}.
#'
#' @param reflectance Reflectance data: a matrix / data.frame with rows =
#'        samples and columns named by wavelength (nm); OR a long data.frame with
#'        \code{id_col}, \code{wavelength_nm}, \code{reflectance}; OR a path to a
#'        CSV in either form.
#' @param metadata A data.frame with one row per sample carrying \code{id_col}
#'        plus lab attributes and optional taxonomic labels and \code{lat}/
#'        \code{lon}. Rows are aligned to \code{reflectance} by \code{id_col}.
#' @param id_col Sample identifier column shared by both tables (default
#'        \code{"id"}).
#' @param wavelengths Optional explicit wavelength vector (nm) when the
#'        reflectance columns are not wavelength-named.
#' @param resample_to Optional target wavelength grid (nm) to linearly resample
#'        every spectrum onto (e.g. \code{350:2500}); default keeps the native grid.
#' @param property_map,label_map Optional named lists overriding the alias
#'        auto-detection, e.g. \code{property_map = list(clay_pct = "ARGILA")}.
#' @param normalize One of \code{"auto"} (divide by 100 when values look like
#'        percent), \code{"percent"}, or \code{"none"}.
#' @param verbose Print a one-line summary (default \code{TRUE}).
#' @return A list with \code{Xr} (numeric reflectance matrix), \code{Yr} (data
#'         frame of mapped properties + labels + \code{lat}/\code{lon}), and
#'         \code{metadata} (provenance). Ready to pass as \code{ossl_library=}.
#' @seealso \code{\link{pedons_from_spectral_table}},
#'   \code{\link{benchmark_spectral_fill}}, \code{\link{fill_from_spectra}}
#' @export
read_spectral_library <- function(reflectance, metadata, id_col = "id",
                                  wavelengths = NULL, resample_to = NULL,
                                  property_map = NULL, label_map = NULL,
                                  normalize = c("auto", "none", "percent"),
                                  verbose = TRUE) {
  normalize <- match.arg(normalize)
  if (!is.data.frame(metadata))
    rlang::abort("read_spectral_library(): `metadata` must be a data.frame.")
  if (!id_col %in% names(metadata))
    rlang::abort(sprintf("read_spectral_library(): id_col '%s' not in metadata.",
                         id_col))

  ref <- .coerce_reflectance(reflectance, id_col = id_col,
                             wavelengths = wavelengths)
  mat <- .spectra_to_fraction(ref$mat, normalize)
  if (!is.null(resample_to)) {
    mat <- .resample_spectra(mat, ref$wavelengths, as.numeric(resample_to))
    ref$wavelengths <- as.numeric(resample_to)
  }

  # Align metadata rows to the reflectance row order by id.
  ord <- match(ref$ids, metadata[[id_col]])
  meta <- metadata[ord, , drop = FALSE]

  pcols <- .resolve_spectral_cols(names(meta), .SPECTRA_PROPERTY_ALIASES,
                                  property_map)
  lcols <- .resolve_spectral_cols(names(meta), .SPECTRA_LABEL_ALIASES, label_map)

  Yr <- data.frame(row.names = seq_len(nrow(mat)))
  for (canon in names(pcols))
    Yr[[canon]] <- suppressWarnings(as.numeric(meta[[pcols[[canon]]]]))
  for (canon in names(lcols))
    Yr[[canon]] <- as.character(meta[[lcols[[canon]]]])
  for (g in c("lat", "lon"))
    if (g %in% names(meta)) Yr[[g]] <- suppressWarnings(as.numeric(meta[[g]]))

  if (ncol(Yr) == 0L)
    rlang::abort(paste0("read_spectral_library(): no property or label columns ",
                        "could be mapped. Pass property_map / label_map."))

  lib <- list(
    Xr = mat,
    Yr = Yr,
    metadata = list(
      n_samples   = nrow(mat),
      n_bands     = ncol(mat),
      wavelengths = ref$wavelengths,
      properties  = names(pcols),
      labels      = names(lcols),
      normalize   = normalize,
      source      = "read_spectral_library"
    )
  )
  if (isTRUE(verbose))
    cli::cli_inform(c(
      "v" = "Spectral library: {nrow(mat)} samples x {ncol(mat)} bands",
      "i" = "properties: {paste(names(pcols), collapse = ', ') %||% '<none>'}",
      "i" = "labels: {paste(names(lcols), collapse = ', ') %||% '<none>'}"
    ))
  lib
}

#' Build PedonRecords with attached Vis-NIR/MIR spectra from a table
#'
#' Groups a reflectance + metadata table by profile and returns one
#' \code{\link{PedonRecord}} per profile, with each profile's sample rows stacked
#' into \code{$spectra$vnir} (rows = horizons, cols = wavelengths) and the lab
#' attributes / depths written to the horizons. Taxonomic labels are stored in
#' \code{$site} (\code{reference_wrb} / \code{reference_sibcs} /
#' \code{reference_st}). These pedons are the query objects for
#' \code{classify_*(gapfill = list(method = "spectra", ossl_library = <lib>))}.
#'
#' @inheritParams read_spectral_library
#' @param profile_col Column grouping samples into profiles (default
#'        \code{id_col}: one profile per sample, e.g. a topsoil library).
#' @param keep_properties If \code{TRUE}, also write the mapped lab attributes to
#'        the horizons (default \code{FALSE} -- a field pedon usually has only the
#'        scan, which is the scenario the spectral fill targets).
#' @return A list of \code{\link{PedonRecord}} objects.
#' @seealso \code{\link{read_spectral_library}}, \code{\link{benchmark_spectral_fill}}
#' @export
pedons_from_spectral_table <- function(reflectance, metadata, id_col = "id",
                                       profile_col = NULL, wavelengths = NULL,
                                       resample_to = NULL, property_map = NULL,
                                       label_map = NULL,
                                       normalize = c("auto", "none", "percent"),
                                       keep_properties = FALSE, verbose = TRUE) {
  normalize <- match.arg(normalize)
  if (is.null(profile_col)) profile_col <- id_col
  ref  <- .coerce_reflectance(reflectance, id_col = id_col,
                              wavelengths = wavelengths)
  mat  <- .spectra_to_fraction(ref$mat, normalize)
  if (!is.null(resample_to)) {
    mat <- .resample_spectra(mat, ref$wavelengths, as.numeric(resample_to))
    ref$wavelengths <- as.numeric(resample_to)
  }
  ord  <- match(ref$ids, metadata[[id_col]])
  meta <- metadata[ord, , drop = FALSE]

  pcols <- .resolve_spectral_cols(names(meta), .SPECTRA_PROPERTY_ALIASES,
                                  property_map)
  lcols <- .resolve_spectral_cols(names(meta), .SPECTRA_LABEL_ALIASES, label_map)
  dcols <- .resolve_spectral_cols(names(meta), .SPECTRA_DEPTH_ALIASES)

  if (!profile_col %in% names(meta))
    rlang::abort(sprintf("pedons_from_spectral_table(): profile_col '%s' missing.",
                         profile_col))
  groups <- split(seq_len(nrow(meta)), meta[[profile_col]])

  site_label <- c(wrb_rsg = "reference_wrb", sibcs_ordem = "reference_sibcs",
                  usda_order = "reference_st")
  peds <- lapply(names(groups), function(g) {
    idx <- groups[[g]]
    nh  <- length(idx)
    top <- if ("top_cm"    %in% names(dcols)) suppressWarnings(as.numeric(meta[[dcols[["top_cm"]]]][idx]))    else seq.int(0L, by = 20L, length.out = nh)
    bot <- if ("bottom_cm" %in% names(dcols)) suppressWarnings(as.numeric(meta[[dcols[["bottom_cm"]]]][idx])) else top + 20
    hz  <- data.frame(top_cm = top, bottom_cm = bot,
                      designation = paste0("S", seq_len(nh)))
    if (keep_properties)
      for (canon in names(pcols))
        hz[[canon]] <- suppressWarnings(as.numeric(meta[[pcols[[canon]]]][idx]))
    site <- list(id = as.character(g))
    for (canon in names(lcols))
      site[[site_label[[canon]]]] <- as.character(meta[[lcols[[canon]]]][idx][1L])
    if ("lat" %in% names(meta)) site$lat <- suppressWarnings(as.numeric(meta$lat[idx][1L]))
    if ("lon" %in% names(meta)) site$lon <- suppressWarnings(as.numeric(meta$lon[idx][1L]))
    vnir <- mat[idx, , drop = FALSE]
    rownames(vnir) <- NULL
    PedonRecord$new(site = site,
                    horizons = ensure_horizon_schema(data.table::as.data.table(hz)),
                    spectra  = list(vnir = vnir))
  })
  if (isTRUE(verbose))
    cli::cli_inform("Built {length(peds)} pedon{?s} with attached vnir spectra.")
  peds
}

#' Benchmark the accuracy lift of spectral gap-fill (ON vs OFF), k-fold
#'
#' The honest measurement that has been data-blocked until a spectra-bearing,
#' labelled dataset exists. For each cross-validation fold it calibrates a
#' spectral library on the training profiles, then classifies the held-out
#' profiles twice -- \strong{OFF} (spectra-only pedon, no lab attributes) and
#' \strong{ON} (\code{\link{fill_from_spectra}} predicts the lab attributes from
#' the scan first) -- and scores both against the reference label. Non-circular:
#' the calibration library never includes a test profile.
#'
#' @inheritParams read_spectral_library
#' @param system One of \code{"sibcs"} (default), \code{"wrb2022"}, \code{"usda"}.
#' @param profile_col Column grouping samples into profiles (default \code{id_col}).
#' @param folds Number of CV folds (default 5).
#' @param properties Attributes to predict from spectra (default the
#'        \code{\link{fill_from_spectra}} set).
#' @param method Spectral model: \code{"mbl"}, \code{"plsr_local"} or
#'        \code{"pretrained"} (passed to \code{\link{fill_from_spectra}}).
#' @param fold_id Optional integer vector (one per profile, in sorted-id order)
#'        to use fixed folds instead of the deterministic modulo split.
#' @return A list with \code{accuracy_off}, \code{accuracy_on}, \code{delta},
#'         \code{n}, per-fold rows, and the per-profile \code{predictions} frame.
#' @seealso \code{\link{read_spectral_library}}, \code{\link{fill_from_spectra}}
#' @export
benchmark_spectral_fill <- function(reflectance, metadata, id_col = "id",
                                    system = c("sibcs", "wrb2022", "usda"),
                                    profile_col = NULL, folds = 5L,
                                    properties = NULL,
                                    method = c("mbl", "plsr_local", "pretrained"),
                                    wavelengths = NULL, resample_to = NULL,
                                    property_map = NULL, label_map = NULL,
                                    normalize = c("auto", "none", "percent"),
                                    fold_id = NULL, verbose = TRUE) {
  system    <- match.arg(system)
  method    <- match.arg(method)
  normalize <- match.arg(normalize)
  if (is.null(profile_col)) profile_col <- id_col
  if (is.null(properties))
    properties <- c("clay_pct", "sand_pct", "silt_pct", "cec_cmol", "bs_pct",
                    "ph_h2o", "oc_pct")

  classer  <- switch(system, sibcs = classify_sibcs,
                     wrb2022 = classify_wrb2022, usda = classify_usda)
  ref_field <- switch(system, sibcs = "reference_sibcs",
                      wrb2022 = "reference_wrb", usda = "reference_st")

  lib_all <- read_spectral_library(reflectance, metadata, id_col = id_col,
                                   wavelengths = wavelengths,
                                   resample_to = resample_to,
                                   property_map = property_map,
                                   label_map = label_map, normalize = normalize,
                                   verbose = FALSE)
  peds <- pedons_from_spectral_table(reflectance, metadata, id_col = id_col,
                                     profile_col = profile_col,
                                     wavelengths = wavelengths,
                                     resample_to = resample_to,
                                     property_map = property_map,
                                     label_map = label_map, normalize = normalize,
                                     keep_properties = FALSE, verbose = FALSE)
  # Map each profile to its library rows (samples) for fold-disjoint calibration.
  ord  <- match(.coerce_reflectance(reflectance, id_col, wavelengths)$ids,
                metadata[[id_col]])
  prof_of_row <- as.character(metadata[[profile_col]][ord])
  prof_ids    <- vapply(peds, function(p) p$site$id, character(1))

  key <- function(lbl) {
    if (is.null(lbl) || is.na(lbl)) return(NA_character_)
    x <- tolower(trimws(iconv(as.character(lbl), to = "ASCII//TRANSLIT")))
    w <- strsplit(x, "[ ,;/]")[[1L]]; w <- w[nzchar(w)]
    if (!length(w)) return(NA_character_)
    sub("s$", "", w[1L])
  }
  eq <- function(a, b) isTRUE(!is.na(a) && !is.na(b) && a == b)

  np <- length(peds)
  if (is.null(fold_id)) fold_id <- (seq_len(np) - 1L) %% as.integer(folds) + 1L
  rows <- list(); n <- 0L; ok_off <- 0L; ok_on <- 0L

  for (f in sort(unique(fold_id))) {
    test_i  <- which(fold_id == f)
    test_id <- prof_ids[test_i]
    train_rows <- which(!(prof_of_row %in% test_id))
    if (length(train_rows) < 2L) next
    lib_f <- list(Xr = lib_all$Xr[train_rows, , drop = FALSE],
                  Yr = lib_all$Yr[train_rows, , drop = FALSE],
                  metadata = lib_all$metadata)
    for (i in test_i) {
      p   <- peds[[i]]
      ref <- key(p$site[[ref_field]])
      if (is.na(ref)) next
      off <- tryCatch(key(classer(p, on_missing = "silent")$rsg_or_order),
                      error = function(e) NA_character_)
      filled <- tryCatch(
        fill_from_spectra(p$clone(deep = TRUE), library = "ossl",
                          method = method, properties = properties,
                          ossl_library = lib_f, verbose = FALSE),
        error = function(e) NULL)
      on <- if (is.null(filled)) off
            else tryCatch(key(classer(filled, on_missing = "silent")$rsg_or_order),
                          error = function(e) NA_character_)
      n <- n + 1L
      if (eq(off, ref)) ok_off <- ok_off + 1L
      if (eq(on,  ref)) ok_on  <- ok_on  + 1L
      rows[[length(rows) + 1L]] <- data.frame(
        id = p$site$id, fold = f, ref = ref,
        pred_off = off %||% NA_character_, pred_on = on %||% NA_character_,
        stringsAsFactors = FALSE)
    }
  }
  pred_df <- if (length(rows)) do.call(rbind, rows) else
    data.frame(id = character(0), fold = integer(0), ref = character(0),
               pred_off = character(0), pred_on = character(0))
  list(
    system       = system,
    accuracy_off = if (n) ok_off / n else NA_real_,
    accuracy_on  = if (n) ok_on  / n else NA_real_,
    delta        = if (n) (ok_on - ok_off) / n else NA_real_,
    n            = n,
    n_changed    = sum(pred_df$pred_off != pred_df$pred_on, na.rm = TRUE),
    predictions  = pred_df
  )
}
