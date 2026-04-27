# ================================================================
# Module 4 -- Spectra preprocessing
#
# Pure-R implementations of two staples of Vis-NIR / MIR soil
# spectroscopy:
#   * Standard Normal Variate (SNV) -- per-row centering & scaling
#   * Savitzky-Golay 1st derivative (SG1) -- 5-point quadratic kernel
#
# When the optional `prospectr` package is installed we delegate to it
# (Stevens & Ramirez-Lopez), because it is faster (Rcpp) and exposes
# proper window/polynomial controls. Without prospectr the native
# fallback keeps the package usable on minimal installs.
# ================================================================


#' Pre-process Vis-NIR or MIR spectra
#'
#' Applies a chosen pre-processing pipeline to a numeric matrix of
#' soil spectra. Rows are samples (typically horizons) and columns are
#' wavelengths. Returns a numeric matrix; SG-based methods shorten the
#' spectrum by \code{w - 1} columns at the edges (default \code{w = 5}
#' so two columns are dropped from each side).
#'
#' Supported \code{method} values:
#' \describe{
#'   \item{\code{"snv"}}{Standard Normal Variate. Each row is centered
#'         on its own mean and divided by its own standard deviation.}
#'   \item{\code{"sg1"}}{Savitzky-Golay 1st derivative with a window of
#'         five wavelengths and a quadratic polynomial.}
#'   \item{\code{"snv+sg1"}}{SNV followed by SG1 (default; the standard
#'         pipeline used by OSSL pretrained models for Vis-NIR).}
#' }
#'
#' If \code{prospectr} is available, we use
#' \code{prospectr::standardNormalVariate} and
#' \code{prospectr::savitzkyGolay} (Rcpp implementation, faster and
#' supports arbitrary window/polynomial). The native fallback uses the
#' classical 5-point first-derivative coefficients
#' \code{(-2, -1, 0, 1, 2) / 10}, which is the closed-form
#' Savitzky-Golay solution for window 5 / polynomial 2 / derivative 1.
#'
#' @param X Numeric matrix or data.frame of spectra (rows = samples,
#'        columns = wavelengths). Wavelengths should be evenly spaced.
#' @param method One of \code{"snv"}, \code{"sg1"}, \code{"snv+sg1"}.
#'        Default \code{"snv+sg1"}.
#' @param w Window size for the SG filter. Must be odd; default 5.
#' @param p Polynomial order for the SG filter. Default 2.
#' @return A numeric matrix. Column names (wavelengths) are preserved
#'         where possible; SG trimming drops \code{(w - 1) / 2}
#'         columns from each edge.
#'
#' @references
#' Savitzky, A., & Golay, M. J. E. (1964). Smoothing and differentiation
#' of data by simplified least squares procedures.
#' \emph{Analytical Chemistry}, 36(8), 1627--1639.
#'
#' Barnes, R. J., Dhanoa, M. S., & Lister, S. J. (1989). Standard
#' Normal Variate transformation and de-trending of near-infrared
#' diffuse reflectance spectra. \emph{Applied Spectroscopy}, 43(5),
#' 772--777.
#'
#' Stevens, A., & Ramirez-Lopez, L. (2024). \emph{prospectr}: Misc.
#' functions for processing and sample selection of spectroscopic data.
#' R package version 0.2.7.
#'
#' @export
#' @examples
#' set.seed(1)
#' X <- matrix(runif(5 * 2151, 0, 1), nrow = 5)
#' colnames(X) <- 350:2500
#' Xp <- preprocess_spectra(X, method = "snv+sg1")
#' dim(Xp)  # 5 x 2147 (4 columns dropped by SG window 5)
preprocess_spectra <- function(X,
                                method = c("snv+sg1", "snv", "sg1"),
                                w      = 5L,
                                p      = 2L) {
  method <- match.arg(method)

  if (is.null(X)) {
    rlang::abort("preprocess_spectra(): X is NULL")
  }
  if (is.data.frame(X)) X <- as.matrix(X)
  if (!is.matrix(X)) {
    if (is.numeric(X)) {
      X <- matrix(X, nrow = 1L)
    } else {
      rlang::abort("preprocess_spectra(): X must be a numeric matrix or data.frame")
    }
  }
  if (!is.numeric(X)) {
    rlang::abort("preprocess_spectra(): X must contain numeric values")
  }
  if (ncol(X) < w) {
    rlang::abort(sprintf(
      "preprocess_spectra(): need at least %d wavelengths for window w=%d, got %d",
      w, w, ncol(X)
    ))
  }
  if (w %% 2L == 0L) {
    rlang::abort("preprocess_spectra(): SG window 'w' must be odd")
  }

  out <- X
  if (method == "snv" || method == "snv+sg1") {
    out <- .snv(out)
  }
  if (method == "sg1" || method == "snv+sg1") {
    out <- .sg1(out, w = w, p = p)
  }

  out
}


# -- Standard Normal Variate ---------------------------------------- internal --

#' Standard Normal Variate transform
#'
#' Per-row centring and scaling: \code{(x - rowMeans) / rowSds}. Uses
#' \code{prospectr::standardNormalVariate} when available, otherwise a
#' native vectorised implementation. Returns a matrix of the same shape
#' as the input.
#'
#' @keywords internal
.snv <- function(X) {
  if (requireNamespace("prospectr", quietly = TRUE)) {
    out <- prospectr::standardNormalVariate(X)
    # prospectr emits NaN for constant rows (zero sd); zero-fill instead so
    # that downstream consumers (SG filter, predictors) don't propagate NaN.
    bad_rows <- which(!apply(out, 1, function(r) all(is.finite(r))))
    if (length(bad_rows) > 0L) out[bad_rows, ] <- 0
    return(out)
  }
  rm    <- rowMeans(X, na.rm = TRUE)
  centred <- X - rm
  rsd   <- sqrt(rowSums(centred^2, na.rm = TRUE) / pmax(ncol(X) - 1L, 1L))
  rsd[rsd == 0 | !is.finite(rsd)] <- 1
  centred / rsd
}


# -- Savitzky-Golay 1st derivative ---------------------------------- internal --

#' Savitzky-Golay 1st derivative
#'
#' Delegates to \code{prospectr::savitzkyGolay} when available
#' (\code{m = 1}, polynomial \code{p}, window \code{w}). The native
#' fallback uses the closed-form 5-point coefficients
#' \code{(-2, -1, 0, 1, 2) / 10}, which is the SG solution for
#' \code{w = 5}, \code{p = 2}, \code{m = 1}, and trims two columns
#' from each edge. For \code{w != 5} the native path falls back to a
#' generic SG coefficient computation via least squares.
#'
#' @keywords internal
.sg1 <- function(X, w = 5L, p = 2L) {
  if (requireNamespace("prospectr", quietly = TRUE)) {
    out <- prospectr::savitzkyGolay(X = X, m = 1L, p = p, w = w)
    return(as.matrix(out))
  }
  coefs <- .sg_coefficients(w = w, p = p, m = 1L)
  half  <- (w - 1L) %/% 2L

  n_rows <- nrow(X)
  n_cols <- ncol(X)
  out_cols <- n_cols - 2L * half
  out <- matrix(NA_real_, nrow = n_rows, ncol = out_cols)
  if (!is.null(colnames(X))) {
    colnames(out) <- colnames(X)[(half + 1L):(n_cols - half)]
  }
  for (j in seq_len(out_cols)) {
    win <- X[, j:(j + w - 1L), drop = FALSE]
    out[, j] <- as.numeric(win %*% coefs)
  }
  out
}


#' Compute Savitzky-Golay coefficients for a derivative
#'
#' Solves the standard SG least-squares system to derive the kernel
#' coefficients for a given window \code{w}, polynomial \code{p}, and
#' derivative order \code{m}. Used only when \code{prospectr} is
#' unavailable.
#'
#' @keywords internal
.sg_coefficients <- function(w, p, m) {
  if (w %% 2L == 0L || w < 3L) {
    rlang::abort("SG window must be odd and >= 3")
  }
  if (p >= w) {
    rlang::abort("SG polynomial p must be < window w")
  }
  half <- (w - 1L) %/% 2L
  z <- seq.int(-half, half)
  J <- outer(z, 0:p, `^`)
  C <- solve(t(J) %*% J) %*% t(J)
  # Row m+1 of C, multiplied by m! gives the kernel for the m-th derivative.
  as.numeric(C[m + 1L, ]) * factorial(m)
}
