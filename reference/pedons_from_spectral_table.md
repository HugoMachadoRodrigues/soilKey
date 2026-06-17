# Build PedonRecords with attached Vis-NIR/MIR spectra from a table

Groups a reflectance + metadata table by profile and returns one
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
per profile, with each profile's sample rows stacked into
`$spectra$vnir` (rows = horizons, cols = wavelengths) and the lab
attributes / depths written to the horizons. Taxonomic labels are stored
in `$site` (`reference_wrb` / `reference_sibcs` / `reference_st`). These
pedons are the query objects for
`classify_*(gapfill = list(method = "spectra", ossl_library = <lib>))`.

## Usage

``` r
pedons_from_spectral_table(
  reflectance,
  metadata,
  id_col = "id",
  profile_col = NULL,
  wavelengths = NULL,
  resample_to = NULL,
  property_map = NULL,
  label_map = NULL,
  normalize = c("auto", "none", "percent"),
  keep_properties = FALSE,
  verbose = TRUE
)
```

## Arguments

- reflectance:

  Reflectance data: a matrix / data.frame with rows = samples and
  columns named by wavelength (nm); OR a long data.frame with `id_col`,
  `wavelength_nm`, `reflectance`; OR a path to a CSV in either form.

- metadata:

  A data.frame with one row per sample carrying `id_col` plus lab
  attributes and optional taxonomic labels and `lat`/ `lon`. Rows are
  aligned to `reflectance` by `id_col`.

- id_col:

  Sample identifier column shared by both tables (default `"id"`).

- profile_col:

  Column grouping samples into profiles (default `id_col`: one profile
  per sample, e.g. a topsoil library).

- wavelengths:

  Optional explicit wavelength vector (nm) when the reflectance columns
  are not wavelength-named.

- resample_to:

  Optional target wavelength grid (nm) to linearly resample every
  spectrum onto (e.g. `350:2500`); default keeps the native grid.

- property_map, label_map:

  Optional named lists overriding the alias auto-detection, e.g.
  `property_map = list(clay_pct = "ARGILA")`.

- normalize:

  One of `"auto"` (divide by 100 when values look like percent),
  `"percent"`, or `"none"`.

- keep_properties:

  If `TRUE`, also write the mapped lab attributes to the horizons
  (default `FALSE` – a field pedon usually has only the scan, which is
  the scenario the spectral fill targets).

- verbose:

  Print a one-line summary (default `TRUE`).

## Value

A list of
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
objects.

## See also

[`read_spectral_library`](https://hugomachadorodrigues.github.io/soilKey/reference/read_spectral_library.md),
[`benchmark_spectral_fill`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_spectral_fill.md)
