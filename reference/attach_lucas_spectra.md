# Attach LUCAS 2018 Vis-NIR spectra to a list of PedonRecord objects

Joins the LUCAS Soil 2018 Spectral Library (separate ESDAC release, ~83
GB) onto the pedons returned by
[`load_lucas_soil_2018`](https://hugomachadorodrigues.github.io/soilKey/reference/load_lucas_soil_2018.md),
by matching the LUCAS `POINT_ID` of the spectra against `pedon$site$id`.
Each matched pedon gets `$spectra$vnir` populated as a numeric matrix
(rows = horizons, cols = wavelengths).

## Usage

``` r
attach_lucas_spectra(
  pedons,
  spectra,
  point_id_col = "POINT_ID",
  verbose = TRUE
)
```

## Arguments

- pedons:

  List of
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
  objects.

- spectra:

  A wide or long `data.frame` as described above.

- point_id_col:

  Name of the LUCAS point-id column in `spectra`. Default `"POINT_ID"`.

- verbose:

  If `TRUE` (default), reports the join hit rate.

## Value

The list of pedons (mutated in place; returned invisibly).

## Details

Two input shapes are accepted:

- A wide `data.frame` keyed by an integer `POINT_ID` column with one
  column per wavelength (column names parseable as numeric nm). One row
  per LUCAS point.

- A long `data.frame` with columns `POINT_ID`, `wavelength_nm`,
  `reflectance`.

Spectra are attached only to the topsoil horizon (row 1); the subsoil
horizon (if any) is left without spectra. After this call,
`benchmark_lucas_2018(..., fill_topsoil_from = "spectra", ossl_models = ...)`
feeds the spectra through
[`predict_from_spectra`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_from_spectra.md)
(v0.9.46) to fill any chemistry / texture gap not already populated by
SoilGrids.

## See also

[`predict_from_spectra`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_from_spectra.md),
[`predict_munsell_from_spectra`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_munsell_from_spectra.md),
[`load_lucas_soil_2018`](https://hugomachadorodrigues.github.io/soilKey/reference/load_lucas_soil_2018.md).
