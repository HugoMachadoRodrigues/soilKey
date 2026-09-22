# Predict CIE XYZ tristimulus values from Vis-NIR reflectance spectra

Numerically integrates user reflectance against the CIE 1931 2-degree
Standard Observer color-matching functions, weighted by the D65
illuminant. Returns the tristimulus values \\X, Y, Z\\ on the standard
scale where \\Y = 100\\ for a perfect diffuse white.

## Usage

``` r
predict_xyz_from_spectra(spectra, wavelengths)
```

## Arguments

- spectra:

  Reflectance values, in 0..1 or 0..100. A numeric vector (one sample),
  a numeric matrix (rows = samples, cols = wavelengths) or a data.frame.

- wavelengths:

  Numeric vector of the wavelengths (in nm) corresponding to the columns
  of `spectra`. Must cover at least 400-700 nm; values outside 380-780
  are ignored.

## Value

A data.frame with columns `X`, `Y`, `Z`, one row per sample.

## See also

[`predict_lab_from_spectra`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_lab_from_spectra.md),
[`predict_munsell_from_spectra`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_munsell_from_spectra.md).
