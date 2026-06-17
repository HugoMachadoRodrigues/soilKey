# Benchmark the accuracy lift of spectral gap-fill (ON vs OFF), k-fold

The honest measurement that has been data-blocked until a
spectra-bearing, labelled dataset exists. For each cross-validation fold
it calibrates a spectral library on the training profiles, then
classifies the held-out profiles twice – **OFF** (spectra-only pedon, no
lab attributes) and **ON**
([`fill_from_spectra`](https://hugomachadorodrigues.github.io/soilKey/reference/fill_from_spectra.md)
predicts the lab attributes from the scan first) – and scores both
against the reference label. Non-circular: the calibration library never
includes a test profile.

## Usage

``` r
benchmark_spectral_fill(
  reflectance,
  metadata,
  id_col = "id",
  system = c("sibcs", "wrb2022", "usda"),
  profile_col = NULL,
  folds = 5L,
  properties = NULL,
  method = c("mbl", "plsr_local", "pretrained"),
  wavelengths = NULL,
  resample_to = NULL,
  property_map = NULL,
  label_map = NULL,
  normalize = c("auto", "none", "percent"),
  fold_id = NULL,
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

- system:

  One of `"sibcs"` (default), `"wrb2022"`, `"usda"`.

- profile_col:

  Column grouping samples into profiles (default `id_col`).

- folds:

  Number of CV folds (default 5).

- properties:

  Attributes to predict from spectra (default the
  [`fill_from_spectra`](https://hugomachadorodrigues.github.io/soilKey/reference/fill_from_spectra.md)
  set).

- method:

  Spectral model: `"mbl"`, `"plsr_local"` or `"pretrained"` (passed to
  [`fill_from_spectra`](https://hugomachadorodrigues.github.io/soilKey/reference/fill_from_spectra.md)).

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

- fold_id:

  Optional integer vector (one per profile, in sorted-id order) to use
  fixed folds instead of the deterministic modulo split.

- verbose:

  Print a one-line summary (default `TRUE`).

## Value

A list with `accuracy_off`, `accuracy_on`, `delta`, `n`, per-fold rows,
and the per-profile `predictions` frame.

## See also

[`read_spectral_library`](https://hugomachadorodrigues.github.io/soilKey/reference/read_spectral_library.md),
[`fill_from_spectra`](https://hugomachadorodrigues.github.io/soilKey/reference/fill_from_spectra.md)
