# Pre-process Vis-NIR or MIR spectra

Applies a chosen pre-processing pipeline to a numeric matrix of soil
spectra. Rows are samples (typically horizons) and columns are
wavelengths. Returns a numeric matrix; SG-based methods shorten the
spectrum by `w - 1` columns at the edges (default `w = 5` so two columns
are dropped from each side).

## Usage

``` r
preprocess_spectra(X, method = c("snv+sg1", "snv", "sg1"), w = 5L, p = 2L)
```

## Arguments

- X:

  Numeric matrix or data.frame of spectra (rows = samples, columns =
  wavelengths). Wavelengths should be evenly spaced.

- method:

  One of `"snv"`, `"sg1"`, `"snv+sg1"`. Default `"snv+sg1"`.

- w:

  Window size for the SG filter. Must be odd; default 5.

- p:

  Polynomial order for the SG filter. Default 2.

## Value

A numeric matrix. Column names (wavelengths) are preserved where
possible; SG trimming drops `(w - 1) / 2` columns from each edge.

## Details

Supported `method` values:

- `"snv"`:

  Standard Normal Variate. Each row is centered on its own mean and
  divided by its own standard deviation.

- `"sg1"`:

  Savitzky-Golay 1st derivative with a window of five wavelengths and a
  quadratic polynomial.

- `"snv+sg1"`:

  SNV followed by SG1 (default; the standard pipeline used by OSSL
  pretrained models for Vis-NIR).

If `prospectr` is available, we use
[`prospectr::standardNormalVariate`](https://rdrr.io/pkg/prospectr/man/standardNormalVariate.html)
and
[`prospectr::savitzkyGolay`](https://rdrr.io/pkg/prospectr/man/savitzkyGolay.html)
(Rcpp implementation, faster and supports arbitrary window/polynomial).
The native fallback uses the classical 5-point first-derivative
coefficients `(-2, -1, 0, 1, 2) / 10`, which is the closed-form
Savitzky-Golay solution for window 5 / polynomial 2 / derivative 1.

## References

Savitzky, A., & Golay, M. J. E. (1964). Smoothing and differentiation of
data by simplified least squares procedures. *Analytical Chemistry*,
36(8), 1627–1639.

Barnes, R. J., Dhanoa, M. S., & Lister, S. J. (1989). Standard Normal
Variate transformation and de-trending of near-infrared diffuse
reflectance spectra. *Applied Spectroscopy*, 43(5), 772–777.

Stevens, A., & Ramirez-Lopez, L. (2024). *prospectr*: Misc. functions
for processing and sample selection of spectroscopic data. R package
version 0.2.7.

## Examples

``` r
set.seed(1)
X <- matrix(runif(5 * 2151, 0, 1), nrow = 5)
colnames(X) <- 350:2500
Xp <- preprocess_spectra(X, method = "snv+sg1")
dim(Xp)  # 5 x 2147 (4 columns dropped by SG window 5)
#> [1]    5 2147
```
