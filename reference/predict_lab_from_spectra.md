# Predict CIE Lab from Vis-NIR reflectance spectra

Convenience wrapper:
[`predict_xyz_from_spectra`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_xyz_from_spectra.md)
followed by the standard CIE Lab transform under D65 / 2-degree
observer.

## Usage

``` r
predict_lab_from_spectra(spectra, wavelengths)
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

A data.frame with columns `L`, `a`, `b`.
