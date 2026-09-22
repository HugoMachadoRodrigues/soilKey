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
#' @noRd
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
#' @noRd
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


#' Generic Savitzky-Golay filter (smoothing or derivative)
#'
#' Generalises \code{.sg1} to any derivative order \code{m} (0 = pure
#' smoothing, 1 = first derivative, 2 = second derivative). Delegates to
#' \code{prospectr::savitzkyGolay} when available, otherwise convolves
#' with coefficients from \code{.sg_coefficients}. Trims
#' \code{(w - 1) / 2} columns from each edge and carries the retained
#' wavelength column names through.
#'
#' @noRd
.sg_filter <- function(X, w, p, m) {
  if (requireNamespace("prospectr", quietly = TRUE)) {
    out <- tryCatch(as.matrix(prospectr::savitzkyGolay(X = X, m = m, p = p, w = w)),
                    error = function(e) NULL)
    if (!is.null(out)) return(out)
  }
  coefs  <- .sg_coefficients(w = w, p = p, m = m)
  half   <- (w - 1L) %/% 2L
  n_cols <- ncol(X)
  out_cols <- n_cols - 2L * half
  out <- matrix(NA_real_, nrow = nrow(X), ncol = out_cols)
  if (!is.null(colnames(X)))
    colnames(out) <- colnames(X)[(half + 1L):(n_cols - half)]
  for (j in seq_len(out_cols)) {
    win <- X[, j:(j + w - 1L), drop = FALSE]
    out[, j] <- as.numeric(win %*% coefs)
  }
  out
}


#' Apply a step-by-step Vis-NIR / MIR preprocessing pipeline
#'
#' Composes the canonical soil-spectroscopy sequence, each step optional
#' and applied in this fixed scientific order:
#' reflectance \eqn{\to} (absorbance) \eqn{\to} (Savitzky-Golay
#' smoothing) \eqn{\to} (Savitzky-Golay 1st or 2nd derivative). Each
#' Savitzky-Golay pass trims \code{(window - 1) / 2} columns from each
#' edge; the wavelength axis is trimmed to match and returned so callers
#' can plot the treated spectrum on the correct axis.
#'
#' The transform is robust: reflectance that looks like a percentage
#' (maximum \code{> 1.5}) is rescaled to a 0--1 fraction before the
#' absorbance log, values are clamped away from zero to avoid
#' \code{log(0)}, and a Savitzky-Golay step that cannot fit the requested
#' window into the available wavelengths is skipped (recorded in
#' \code{steps}) rather than erroring.
#'
#' @param X Numeric matrix (rows = samples/horizons, columns =
#'        wavelengths) or a numeric vector (treated as one sample).
#' @param wavelengths Optional numeric wavelength axis. Defaults to the
#'        numeric part of \code{colnames(X)}, else \code{1:ncol(X)}.
#' @param absorbance Logical; apply \eqn{A = \log_{10}(1 / R)}.
#' @param sg_smooth Logical; apply Savitzky-Golay smoothing
#'        (\code{m = 0}).
#' @param sg_derivative Integer \code{0}, \code{1} or \code{2};
#'        Savitzky-Golay derivative order (\code{0} = none).
#' @param window Odd Savitzky-Golay window (default \code{11});
#'        coerced to a valid odd value in \code{[3, ncol)}.
#' @param poly Savitzky-Golay polynomial order (default \code{2});
#'        clamped to \code{[1, window - 1]}.
#' @return A list with \code{X} (the treated numeric matrix, wavelength
#'         column names trimmed to match), \code{wavelengths} (numeric)
#'         and \code{steps} (an ordered character vector describing the
#'         transforms actually applied, starting with
#'         \code{"Reflectance"}).
#' @seealso \code{\link{preprocess_spectra}}
#' @export
#' @examples
#' X <- matrix(seq(0.1, 0.5, length.out = 3 * 60), nrow = 3, byrow = TRUE)
#' colnames(X) <- seq(400, 2400, length.out = 60)
#' res <- apply_spectral_preprocessing(X, absorbance = TRUE,
#'                                     sg_smooth = TRUE, sg_derivative = 1L)
#' res$steps          # ordered treatment labels
#' dim(res$X)         # columns trimmed by the two SG passes
apply_spectral_preprocessing <- function(X, wavelengths = NULL,
                                         absorbance = FALSE,
                                         sg_smooth = FALSE,
                                         sg_derivative = 0L,
                                         window = 11L, poly = 2L) {
  if (is.null(X)) rlang::abort("apply_spectral_preprocessing(): X is NULL")
  if (is.data.frame(X)) X <- as.matrix(X)
  if (!is.matrix(X)) X <- matrix(as.numeric(X), nrow = 1L)
  storage.mode(X) <- "double"

  if (is.null(wavelengths)) {
    wl <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", colnames(X))))
    if (length(wl) != ncol(X) || all(is.na(wl))) wl <- seq_len(ncol(X))
  } else {
    wl <- as.numeric(wavelengths)
  }
  colnames(X) <- as.character(wl)

  sg_derivative <- as.integer(sg_derivative %||% 0L)
  if (length(sg_derivative) != 1L || is.na(sg_derivative) ||
        !sg_derivative %in% 0:2)
    rlang::abort("apply_spectral_preprocessing(): sg_derivative must be 0, 1 or 2")
  steps <- "Reflectance"

  # ---- 1. absorbance A = log10(1/R) -------------------------------------
  if (isTRUE(absorbance)) {
    R <- X
    if (suppressWarnings(max(R, na.rm = TRUE)) > 1.5) R <- R / 100  # % -> fraction
    R <- pmin(pmax(R, 1e-5), 1)                                     # avoid log(0)
    X <- log10(1 / R)
    colnames(X) <- as.character(wl)
    steps <- c(steps, "Absorbance (log 1/R)")
  }

  # ---- validate the SG window/poly against what is available -------------
  # A window that cannot fit the current spectrum is reported as skipped (see
  # apply_sg) rather than silently resized -- the requested window is a
  # scientific choice, so we do not change it behind the user's back. Only an
  # even window is nudged to odd (SG requires odd), and poly is clamped < window.
  sg_ok <- function(nc) {
    w <- as.integer(round(window)); p <- as.integer(round(poly))
    if (w %% 2L == 0L) w <- w + 1L      # SG windows must be odd
    if (w < 3L || w >= nc) return(NULL) # cannot fit -> caller records "skipped"
    p <- max(1L, min(p, w - 1L))
    list(w = w, p = p)
  }
  apply_sg <- function(m, label) {
    cfg <- sg_ok(ncol(X))
    if (is.null(cfg)) { steps <<- c(steps, sprintf("%s skipped: too few bands", label)); return() }
    half <- (cfg$w - 1L) %/% 2L
    Xt <- .sg_filter(X, w = cfg$w, p = cfg$p, m = m)
    wl <<- wl[(half + 1L):(length(wl) - half)]
    colnames(Xt) <- as.character(wl)
    X  <<- Xt
    steps <<- c(steps, sprintf("%s (w=%d, p=%d)", label, cfg$w, cfg$p))
  }

  # ---- 2. Savitzky-Golay smoothing (m = 0) ------------------------------
  if (isTRUE(sg_smooth)) apply_sg(0L, "SG smoothing")

  # ---- 3. Savitzky-Golay derivative (m = 1 or 2) ------------------------
  if (sg_derivative %in% c(1L, 2L))
    apply_sg(sg_derivative,
             sprintf("SG %s derivative",
                     if (sg_derivative == 1L) "1st" else "2nd"))

  list(X = X, wavelengths = wl, steps = steps)
}


#' Compute Savitzky-Golay coefficients for a derivative
#'
#' Solves the standard SG least-squares system to derive the kernel
#' coefficients for a given window \code{w}, polynomial \code{p}, and
#' derivative order \code{m}. Used only when \code{prospectr} is
#' unavailable.
#'
#' @noRd
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
