# =============================================================================
# v0.9.47 -- Vis-NIR spectra -> Munsell HVC via CIE 1931 colorimetry.
#
# Physical pipeline (no model training, no OSSL fit):
#
#   reflectance R(lambda) on user grid  --> resample to 380-780nm (5nm)
#   X = k * sum( S(lambda) * R(lambda) * xbar(lambda) )
#   Y = k * sum( S(lambda) * R(lambda) * ybar(lambda) )
#   Z = k * sum( S(lambda) * R(lambda) * zbar(lambda) )
#   k = 100 / sum( S(lambda) * ybar(lambda) )       (so Y = 100 for white)
#   Munsell HVC = munsellinterpol::XYZtoMunsell( XYZ, white = D65 )
#
# IMPORTANT (fixed v0.9.148, per G. Davis, munsellinterpol author): the
# Munsell renotation is anchored to *Illuminant C* (Munsell 1943), not
# D65. Our colorimetry is computed under D65, so a chromatic adaptation
# D65 -> C is mandatory before interpolating Munsell -- without it every
# sample picks up a slight green-yellow tint (a perfect neutral returns
# Chroma ~ 0.65 instead of 0). Since v0.9.183 we call the canonical
# munsellinterpol::XYZtoMunsell( XYZ, white = D65 ) (munsellinterpol
# >= 3.4-0, published 2026-07-03), which performs that adaptation
# internally -- the exact path G. Davis documents in its Examples. On
# older munsellinterpol we fall back to the numerically identical
# XYZ -> Lab (D65) -> LabToMunsell(). We do NOT feed D65 xyY straight to
# xyYtoMunsell() (with no white=), which would keep the green-yellow bias.
#
# CIE inputs are bundled as internal data .cie_d65_5nm (81 rows from
# 380 to 780 nm, columns: wavelength, xbar, ybar, zbar, D65) so no
# runtime dependency on colorscience or any other CMF/illuminant
# package. Munsell interpolation is delegated to munsellinterpol
# (CRAN, GPL; it Imports spacesXYZ, which performs the adaptation) -- if
# absent, predict_xyz_from_spectra() and predict_lab_from_spectra() still
# work, only the Munsell HVC step is skipped with a clear error.
# =============================================================================


#' Predict CIE XYZ tristimulus values from Vis-NIR reflectance spectra
#'
#' Numerically integrates user reflectance against the CIE 1931 2-degree
#' Standard Observer color-matching functions, weighted by the D65
#' illuminant. Returns the tristimulus values \eqn{X, Y, Z} on the
#' standard scale where \eqn{Y = 100} for a perfect diffuse white.
#'
#' @param spectra Reflectance values, in 0..1 or 0..100. A numeric
#'        vector (one sample), a numeric matrix (rows = samples,
#'        cols = wavelengths) or a data.frame.
#' @param wavelengths Numeric vector of the wavelengths (in nm)
#'        corresponding to the columns of \code{spectra}. Must cover
#'        at least 400-700 nm; values outside 380-780 are ignored.
#' @return A data.frame with columns \code{X}, \code{Y}, \code{Z},
#'         one row per sample.
#' @seealso \code{\link{predict_lab_from_spectra}},
#'          \code{\link{predict_munsell_from_spectra}}.
#' @export
predict_xyz_from_spectra <- function(spectra, wavelengths) {
  Xmat <- .as_spectra_matrix(spectra)
  if (length(wavelengths) != ncol(Xmat)) {
    stop(sprintf(
      "predict_xyz_from_spectra(): length(wavelengths)=%d != ncol(spectra)=%d",
      length(wavelengths), ncol(Xmat)
    ))
  }
  cie <- .cie_d65_5nm
  cie_wl <- cie$wavelength
  # Restrict + resample each sample's reflectance to the CIE grid.
  # Linear interpolation, NA outside the user range.
  xbar <- cie$xbar
  ybar <- cie$ybar
  zbar <- cie$zbar
  Sd65 <- cie$D65
  # Reflectance scale: support both 0..1 (decimal) and 0..100 (%)
  if (all(stats::na.omit(c(Xmat)) <= 1.5)) {
    Rscale <- 1
  } else {
    Rscale <- 0.01
  }

  out <- vapply(seq_len(nrow(Xmat)), function(i) {
    R_user <- Rscale * Xmat[i, ]
    f <- stats::approxfun(wavelengths, R_user, rule = 2L)
    R_cie <- f(cie_wl)
    R_cie[!is.finite(R_cie)] <- 0
    SR <- Sd65 * R_cie
    k  <- 100 / sum(Sd65 * ybar)
    Xv <- k * sum(SR * xbar)
    Yv <- k * sum(SR * ybar)
    Zv <- k * sum(SR * zbar)
    c(Xv, Yv, Zv)
  }, FUN.VALUE = numeric(3L))
  out <- t(out)
  colnames(out) <- c("X", "Y", "Z")
  as.data.frame(out)
}


#' Predict CIE Lab from Vis-NIR reflectance spectra
#'
#' Convenience wrapper: \code{\link{predict_xyz_from_spectra}}
#' followed by the standard CIE Lab transform under D65 / 2-degree
#' observer.
#'
#' @inheritParams predict_xyz_from_spectra
#' @return A data.frame with columns \code{L}, \code{a}, \code{b}.
#' @export
predict_lab_from_spectra <- function(spectra, wavelengths) {
  xyz <- predict_xyz_from_spectra(spectra, wavelengths)
  .cielab_from_xyz(xyz$X, xyz$Y, xyz$Z)
}


# CIE 1976 L*a*b* from XYZ under a given white point (default D65, Y=100).
# Vectorised over X/Y/Z. Verified identical to spacesXYZ::LabfromXYZ to
# ~1e-14, so it is reused for the Munsell adaptation chain without adding
# a direct spacesXYZ dependency.
.cielab_from_xyz <- function(X, Y, Z, white = c(95.047, 100.000, 108.883)) {
  fxyz <- function(t) ifelse(t > (6 / 29) ^ 3,
                              t ^ (1 / 3),
                              t * (29 / 6) ^ 2 / 3 + 4 / 29)
  fx <- fxyz(X / white[1L])
  fy <- fxyz(Y / white[2L])
  fz <- fxyz(Z / white[3L])
  data.frame(L = 116 * fy - 16,
             a = 500 * (fx - fy),
             b = 200 * (fy - fz))
}


#' Predict Munsell hue / value / chroma from Vis-NIR reflectance spectra
#'
#' Combines \code{\link{predict_xyz_from_spectra}} with the Munsell
#' renotation interpolation in \pkg{munsellinterpol} (CRAN, GPL).
#' Returns hue (e.g. \code{"7.5YR"}), value (0..10) and chroma
#' (0..20) per sample, plus the soilKey fields
#'
#' The Munsell renotation is defined under \emph{Illuminant C}, while
#' the colorimetry here is computed under D65, so the conversion adapts
#' D65 -> C. It calls \code{munsellinterpol::XYZtoMunsell(XYZ, white =)}
#' (munsellinterpol >= 3.4-0), which performs that chromatic adaptation
#' internally, and falls back to the numerically identical
#' XYZ -> CIELAB(D65) -> \code{LabToMunsell()} route on older versions.
#' Feeding D65 chromaticities straight to \code{xyYtoMunsell()} (with no
#' \code{white}) would bias every colour toward green-yellow (a perfect
#' neutral would return Chroma ~ 0.65 rather than 0); this routine avoids
#' that. The D65 reference white is derived from the same bundled CIE
#' table the colorimetry integrates against (so a constant-reflectance
#' spectrum maps to an exact neutral, and a perfect reflecting diffuser
#' to Munsell value 10), and the conversion is vectorised over all rows
#' of \code{spectra} at once. At zero Chroma the Munsell hue is undefined,
#' so a neutral is reported with hue \code{"N"} in both the rounded and the
#' continuous (\code{round_chip = FALSE}) notation.
#' \code{munsell_hue_moist}, \code{munsell_value_moist},
#' \code{munsell_chroma_moist} ready to write into a
#' \code{\link{PedonRecord}} via the pedon's \code{add_measurement}
#' method (see also \code{\link{fill_munsell_from_spectra}}).
#'
#' This is the v0.9.47 unblock for the v0.9.35 Argissolo Vermelho /
#' Amarelo / Vermelho-Amarelo color-confusion case: when a user has
#' Vis-NIR spectra (which Embrapa's BDsolos / FEBR do not include
#' but the OSSL does), the Munsell hue can be recovered physically
#' without waiting for the surveyor's morphological description.
#'
#' @inheritParams predict_xyz_from_spectra
#' @param round_chip If \code{TRUE} (default), snaps the predicted
#'        HVC to the nearest standard Munsell chip grid via
#'        \code{munsellinterpol::roundHVC()}. \code{FALSE} returns
#'        continuous HVC (useful for further numeric work).
#' @return A data.frame with columns \code{munsell_hue_moist},
#'         \code{munsell_value_moist}, \code{munsell_chroma_moist},
#'         \code{munsell_string} (e.g. \code{"7.5YR 4/6"}),
#'         \code{X}, \code{Y}, \code{Z}, one row per sample.
#'
#' @examples
#' \dontrun{
#' # White reflector across the visible: should map to a near-neutral
#' # high-value Munsell color.
#' wl <- seq(380, 780, by = 5)
#' R  <- rep(0.9, length(wl))
#' predict_munsell_from_spectra(R, wavelengths = wl)
#' }
#' @export
predict_munsell_from_spectra <- function(spectra, wavelengths,
                                            round_chip = TRUE) {
  if (!requireNamespace("munsellinterpol", quietly = TRUE)) {
    stop("Package 'munsellinterpol' is required for ",
         "predict_munsell_from_spectra(). Install with ",
         "`install.packages(\"munsellinterpol\")`.")
  }
  # Self-consistent D65 white point: the CIE colour-matching functions weighted
  # by the *same* table's D65 SPD that predict_xyz_from_spectra() integrates
  # against, scaled to Y = 100. Using this rather than the textbook
  # c(95.047, 100, 108.883) makes a constant-reflectance spectrum map to an exact
  # neutral (Chroma 0), because the Lab reference white then matches the white
  # the XYZ are implicitly relative to. (Suggested by G. Davis; his sketch
  # omitted the D65 weighting, which would instead give the equal-energy white.)
  cie <- .cie_d65_5nm
  white_D65 <- colSums(cbind(cie$xbar, cie$ybar, cie$zbar) * cie$D65)
  white_D65 <- 100 * white_D65 / white_D65[2L]

  xyz <- predict_xyz_from_spectra(spectra, wavelengths)
  n   <- nrow(xyz)

  hue <- rep(NA_character_, n); value <- rep(NA_real_, n)
  chroma <- rep(NA_real_, n);   ms <- rep(NA_character_, n)

  # Evaluate only rows with a usable colour (finite XYZ, positive Y). All the
  # munsellinterpol conversions are vectorised over a matrix of rows, so the
  # whole batch is a handful of calls rather than one-per-spectrum.
  valid <- is.finite(xyz$X) & is.finite(xyz$Y) & is.finite(xyz$Z) & xyz$Y > 0
  if (any(valid)) {
    XYZm <- cbind(xyz$X[valid], xyz$Y[valid], xyz$Z[valid])
    # Direct XYZ -> Munsell. munsellinterpol (>= 3.4-0) performs the mandatory
    # D65 -> Illuminant-C chromatic adaptation internally, given white=; this is
    # the canonical path documented in munsellinterpol::XYZtoMunsell() (G. Davis,
    # 2026). It is numerically identical to the older XYZ -> CIELAB(D65) ->
    # LabToMunsell() route, which is kept as a fallback for munsellinterpol
    # < 3.4-0 (where XYZtoMunsell() has no white= argument).
    hvc <- tryCatch(munsellinterpol::XYZtoMunsell(XYZm, white = white_D65),
                    error = function(e) NULL)
    if (is.null(hvc)) {
      lab <- .cielab_from_xyz(XYZm[, 1], XYZm[, 2], XYZm[, 3], white = white_D65)
      hvc <- tryCatch(munsellinterpol::LabToMunsell(
                        cbind(lab$L, lab$a, lab$b), white = white_D65),
                      error = function(e) NULL)
    }
    if (!is.null(hvc)) {
      Hk <- hvc[, "H"]; Vk <- hvc[, "V"]; Ck <- hvc[, "C"]
      fin <- is.finite(Hk) & is.finite(Vk) & is.finite(Ck)
      idx <- which(valid)[fin]
      if (length(idx) > 0L) {
        Hk <- Hk[fin]; Vk <- Vk[fin]; Ck <- Ck[fin]
        done <- FALSE
        if (isTRUE(round_chip)) {
          # Snap to the nearest *soil* Munsell book chip (one vectorised call).
          # roundHVC() returns the chip as the `MunsellRounded` string; parse
          # hue / value / chroma back out. books= has no default, hence "soil".
          rr <- tryCatch(munsellinterpol::roundHVC(cbind(Hk, Vk, Ck),
                                                     books = "soil"),
                         error = function(e) NULL)
          mr <- if (!is.null(rr)) as.character(rr$MunsellRounded) else NULL
          if (!is.null(mr) && length(mr) == length(idx)) {
            hue[idx]    <- sub("^[[:space:]]*([0-9.]*[A-Z]+|N).*", "\\1", mr)
            value[idx]  <- as.numeric(sub(".* ([0-9.]+)/.*$", "\\1", mr))
            cr <- sub(".*/([0-9.]*)$", "\\1", mr); cr[cr == ""] <- "0"
            chroma[idx] <- as.numeric(cr)
            ms[idx]     <- mr
            done <- TRUE
          }
        }
        if (!done) {
          # Continuous notation (or round-chip fall-through if rounding failed).
          hs <- tryCatch(munsellinterpol::HueStringFromNumber(Hk),
                         error = function(e) rep(NA_character_, length(Hk)))
          # Hue is undefined at Chroma 0 (G. Davis, munsellinterpol author): a
          # flat/neutral spectrum yields H = 0, which HueStringFromNumber() spells
          # "10RP" -- a spurious reddish-purple on a grey. Collapse those to the
          # neutral axis "N", exactly as roundHVC(books = "soil") already does on
          # the rounded path, so the continuous output -- and the WRB/USDA/SiBCS
          # hue-threshold predicates it can feed -- never sees a bogus hue on a
          # chroma-0 sample.
          neutral <- is.finite(Ck) & Ck < 1e-4
          hs[neutral] <- "N"
          hue[idx]    <- hs
          value[idx]  <- Vk
          chroma[idx] <- Ck
          ms[idx]     <- ifelse(is.na(hs), NA_character_,
                                ifelse(neutral, sprintf("N %g/", Vk),
                                       sprintf("%s %g/%g", hs, Vk, Ck)))
        }
      }
    }
  }

  data.frame(
    munsell_hue_moist    = hue,
    munsell_value_moist  = value,
    munsell_chroma_moist = chroma,
    munsell_string       = ms,
    X                    = xyz$X,
    Y                    = xyz$Y,
    Z                    = xyz$Z,
    stringsAsFactors = FALSE
  )
}


#' Fill missing Munsell colors on a PedonRecord from Vis-NIR spectra
#'
#' High-level helper that runs
#' \code{\link{predict_munsell_from_spectra}} per horizon over the
#' Vis-NIR spectra in \code{pedon$spectra$vnir} and writes the
#' resulting hue / value / chroma back to the matching horizon rows
#' via \code{pedon$add_measurement(..., source = "predicted_spectra")}.
#'
#' This is the operational answer to the v0.9.35 Argissolo color
#' confusion: when surveyor Munsell colors are missing and the user
#' has Vis-NIR (e.g. from OSSL), call this helper, then re-run
#' \code{\link{classify_sibcs}} -- the v0.9.45
#' "color-undetermined" fallback will lift, and the classification
#' will descend to subordem / grande grupo / subgrupo with proper
#' \code{evidence_grade}.
#'
#' @param pedon A \code{\link{PedonRecord}} that has
#'        \code{$spectra$vnir} populated (rows = horizons, cols =
#'        wavelengths).
#' @param overwrite If \code{TRUE}, overwrite existing Munsell
#'        measurements. Default \code{FALSE} (only fills
#'        horizons whose Munsell is currently NA).
#' @param verbose If \code{TRUE} (default), prints a per-horizon
#'        summary.
#' @return The pedon, invisibly. Provenance entries with
#'         \code{source = "predicted_spectra"} are appended.
#' @export
fill_munsell_from_spectra <- function(pedon,
                                        overwrite = FALSE,
                                        verbose   = TRUE) {
  if (!inherits(pedon, "PedonRecord")) {
    stop("fill_munsell_from_spectra(): 'pedon' must be a PedonRecord.")
  }
  if (is.null(pedon$spectra) || is.null(pedon$spectra$vnir)) {
    stop("fill_munsell_from_spectra(): pedon$spectra$vnir is missing.")
  }
  X <- pedon$spectra$vnir
  if (!is.matrix(X)) X <- as.matrix(X)
  if (nrow(X) != nrow(pedon$horizons)) {
    stop(sprintf(
      "fill_munsell_from_spectra(): nrow(spectra)=%d != nrow(horizons)=%d",
      nrow(X), nrow(pedon$horizons)
    ))
  }
  wl <- as.numeric(colnames(X))
  if (any(!is.finite(wl))) {
    stop("fill_munsell_from_spectra(): colnames(pedon$spectra$vnir) ",
         "must be numeric wavelengths in nm.")
  }
  preds <- predict_munsell_from_spectra(X, wavelengths = wl)
  n_written <- 0L
  for (i in seq_len(nrow(preds))) {
    for (col in c("munsell_hue_moist",
                    "munsell_value_moist",
                    "munsell_chroma_moist")) {
      val <- preds[[col]][i]
      if (is.na(val)) next
      before <- nrow(pedon$provenance)
      pedon$add_measurement(
        horizon_idx = i,
        attribute   = col,
        value       = val,
        source      = "predicted_spectra",
        confidence  = 0.7,
        notes       = sprintf("CIE-1931/D65 -> Lab -> munsellinterpol (adapted to Illuminant C); XYZ=(%.2f, %.2f, %.2f)",
                                preds$X[i], preds$Y[i], preds$Z[i]),
        overwrite   = overwrite
      )
      if (nrow(pedon$provenance) > before) n_written <- n_written + 1L
    }
  }
  if (verbose) {
    cli::cli_alert_success(sprintf(
      "fill_munsell_from_spectra(): wrote %d Munsell cell(s) across %d horizon(s).",
      n_written, nrow(preds)
    ))
  }
  invisible(pedon)
}


# ---- helpers -------------------------------------------------------------

.as_spectra_matrix <- function(x) {
  if (is.data.frame(x)) x <- as.matrix(x)
  if (is.numeric(x) && is.null(dim(x))) {
    x <- matrix(x, nrow = 1L)
  }
  if (!is.matrix(x) || !is.numeric(x)) {
    stop(".as_spectra_matrix(): expected a numeric matrix, data.frame ",
         "or vector.")
  }
  x
}
