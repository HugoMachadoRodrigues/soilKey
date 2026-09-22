# Apply a step-by-step Vis-NIR / MIR preprocessing pipeline

Composes the canonical soil-spectroscopy sequence, each step optional
and applied in this fixed scientific order: reflectance \\\to\\
(absorbance) \\\to\\ (Savitzky-Golay smoothing) \\\to\\ (Savitzky-Golay
1st or 2nd derivative). Each Savitzky-Golay pass trims
`(window - 1) / 2` columns from each edge; the wavelength axis is
trimmed to match and returned so callers can plot the treated spectrum
on the correct axis.

## Usage

``` r
apply_spectral_preprocessing(
  X,
  wavelengths = NULL,
  absorbance = FALSE,
  sg_smooth = FALSE,
  sg_derivative = 0L,
  window = 11L,
  poly = 2L
)
```

## Arguments

- X:

  Numeric matrix (rows = samples/horizons, columns = wavelengths) or a
  numeric vector (treated as one sample).

- wavelengths:

  Optional numeric wavelength axis. Defaults to the numeric part of
  `colnames(X)`, else `1:ncol(X)`.

- absorbance:

  Logical; apply \\A = \log\_{10}(1 / R)\\.

- sg_smooth:

  Logical; apply Savitzky-Golay smoothing (`m = 0`).

- sg_derivative:

  Integer `0`, `1` or `2`; Savitzky-Golay derivative order (`0` = none).

- window:

  Odd Savitzky-Golay window (default `11`); coerced to a valid odd value
  in `[3, ncol)`.

- poly:

  Savitzky-Golay polynomial order (default `2`); clamped to
  `[1, window - 1]`.

## Value

A list with `X` (the treated numeric matrix, wavelength column names
trimmed to match), `wavelengths` (numeric) and `steps` (an ordered
character vector describing the transforms actually applied, starting
with `"Reflectance"`).

## Details

The transform is robust: reflectance that looks like a percentage
(maximum `> 1.5`) is rescaled to a 0–1 fraction before the absorbance
log, values are clamped away from zero to avoid `log(0)`, and a
Savitzky-Golay step that cannot fit the requested window into the
available wavelengths is skipped (recorded in `steps`) rather than
erroring.

## See also

[`preprocess_spectra`](https://hugomachadorodrigues.github.io/soilKey/reference/preprocess_spectra.md)

## Examples

``` r
X <- matrix(seq(0.1, 0.5, length.out = 3 * 60), nrow = 3, byrow = TRUE)
colnames(X) <- seq(400, 2400, length.out = 60)
res <- apply_spectral_preprocessing(X, absorbance = TRUE,
                                    sg_smooth = TRUE, sg_derivative = 1L)
res$steps          # ordered treatment labels
#> [1] "Reflectance"                   "Absorbance (log 1/R)"         
#> [3] "SG smoothing (w=11, p=2)"      "SG 1st derivative (w=11, p=2)"
dim(res$X)         # columns trimmed by the two SG passes
#> [1]  3 40
```
