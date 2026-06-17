# Read a Vis-NIR / MIR reflectance + lab table into an OSSL-shaped library

Turns an arbitrary spectral dataset (e.g. a Brazilian Vis-NIR/MIR
library) into the canonical `list(Xr, Yr, metadata)` object consumed by
[`fill_from_spectra`](https://hugomachadorodrigues.github.io/soilKey/reference/fill_from_spectra.md)
and
[`classify_by_spectral_neighbours`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_by_spectral_neighbours.md).
Column names are mapped to the package's canonical attributes (clay_pct,
sand_pct, ..., and the taxonomic label columns `wrb_rsg` / `sibcs_ordem`
/ `usda_order`) via a built-in alias table (including Portuguese headers
such as *argila* / *silte* / *carbono*) or an explicit `property_map` /
`label_map`.

## Usage

``` r
read_spectral_library(
  reflectance,
  metadata,
  id_col = "id",
  wavelengths = NULL,
  resample_to = NULL,
  property_map = NULL,
  label_map = NULL,
  normalize = c("auto", "none", "percent"),
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

- verbose:

  Print a one-line summary (default `TRUE`).

## Value

A list with `Xr` (numeric reflectance matrix), `Yr` (data frame of
mapped properties + labels + `lat`/`lon`), and `metadata` (provenance).
Ready to pass as `ossl_library=`.

## See also

[`pedons_from_spectral_table`](https://hugomachadorodrigues.github.io/soilKey/reference/pedons_from_spectral_table.md),
[`benchmark_spectral_fill`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_spectral_fill.md),
[`fill_from_spectra`](https://hugomachadorodrigues.github.io/soilKey/reference/fill_from_spectra.md)
