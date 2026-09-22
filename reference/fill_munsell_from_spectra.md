# Fill missing Munsell colors on a PedonRecord from Vis-NIR spectra

High-level helper that runs
[`predict_munsell_from_spectra`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_munsell_from_spectra.md)
per horizon over the Vis-NIR spectra in `pedon$spectra$vnir` and writes
the resulting hue / value / chroma back to the matching horizon rows via
`pedon$add_measurement(..., source = "predicted_spectra")`.

## Usage

``` r
fill_munsell_from_spectra(pedon, overwrite = FALSE, verbose = TRUE)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
  that has `$spectra$vnir` populated (rows = horizons, cols =
  wavelengths).

- overwrite:

  If `TRUE`, overwrite existing Munsell measurements. Default `FALSE`
  (only fills horizons whose Munsell is currently NA).

- verbose:

  If `TRUE` (default), prints a per-horizon summary.

## Value

The pedon, invisibly. Provenance entries with
`source = "predicted_spectra"` are appended.

## Details

This is the operational answer to the v0.9.35 Argissolo color confusion:
when surveyor Munsell colors are missing and the user has Vis-NIR (e.g.
from OSSL), call this helper, then re-run
[`classify_sibcs`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_sibcs.md)
– the v0.9.45 "color-undetermined" fallback will lift, and the
classification will descend to subordem / grande grupo / subgrupo with
proper `evidence_grade`.
