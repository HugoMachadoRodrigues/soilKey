# Run the LUCAS Soil 2018 / ESDB WRB benchmark

For each pedon in `pedons`, attaches the canonical Reference Soil Group
at its coordinate via
[`lookup_esdb`](https://hugomachadorodrigues.github.io/soilKey/reference/lookup_esdb.md),
runs
[`classify_wrb2022`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_wrb2022.md)
(or
[`classify_sibcs`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_sibcs.md)),
and tabulates predicted vs reference. Optionally fills missing texture
from ISRIC SoilGrids 250m before classifying so that WRB diagnostic
horizons that depend on clay (argic, ferralic, nitic) are reachable.

## Usage

``` r
benchmark_lucas_2018(
  pedons,
  esdb_root,
  attribute = "WRBLV1",
  fill_texture_from = NULL,
  fill_topsoil_from = c("none", "soilgrids", "spectra"),
  fill_subsoil_from = c("none", "soilgrids"),
  fill_properties = c("clay", "sand", "silt", "phh2o", "soc", "cec", "bdod", "nitrogen",
    "cfvo"),
  ossl_models = NULL,
  classify_with = c("wrb2022", "sibcs"),
  max_n = NULL,
  soilgrids_lookup_fn = lookup_soilgrids,
  verbose = TRUE
)
```

## Arguments

- pedons:

  List of
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
  objects, typically from
  [`load_lucas_soil_2018`](https://hugomachadorodrigues.github.io/soilKey/reference/load_lucas_soil_2018.md).

- esdb_root:

  Path to the unpacked ESDB raster directory (containing the `WRBLV1/`
  sub-folder).

- attribute:

  ESDB attribute to use as reference. Default `"WRBLV1"` (Reference Soil
  Group, 31 codes). Other sensible choices: `"FAO90LV1"` (legacy FAO
  1990).

- fill_texture_from:

  Deprecated alias for `fill_topsoil_from` (v0.9.49 signature). When
  `"soilgrids"`, treated as `fill_topsoil_from = "soilgrids"` with
  `fill_properties = c("clay", "sand", "silt")` and
  `fill_subsoil_from = "none"`.

- fill_topsoil_from:

  One of `"none"` (default), `"soilgrids"` (fills topsoil 0-20 cm from
  SoilGrids 250m at 0-5 cm), or `"spectra"` (runs
  [`predict_from_spectra`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_from_spectra.md)
  with the supplied `ossl_models`; pedons must have `$spectra$vnir`
  attached, e.g. via
  [`attach_lucas_spectra`](https://hugomachadorodrigues.github.io/soilKey/reference/attach_lucas_spectra.md)).

- fill_subsoil_from:

  One of `"none"` (default) or `"soilgrids"` (synthesises a 30-60 cm B
  horizon from SoilGrids 250m). Unlocks WRB diagnostic horizons that
  depend on subsoil features (cambic, argic, mollic).

- fill_properties:

  Character vector of SoilGrids properties to fill when
  `fill_topsoil_from = "soilgrids"` or
  `fill_subsoil_from = "soilgrids"`. Default uses all 9 properties:
  clay, sand, silt, phh2o, soc, cec, bdod, nitrogen, cfvo. Set to
  `c("clay", "sand", "silt")` to recover the v0.9.49 behaviour. `cfvo`
  is mapped to `coarse_fragments_pct`, which drives the Leptosols
  diagnostic (\>= 90 within 25 cm).

- ossl_models:

  Required when `fill_topsoil_from = "spectra"`. A list of
  `soilKey_pls_model` objects from
  [`train_pls_from_ossl`](https://hugomachadorodrigues.github.io/soilKey/reference/train_pls_from_ossl.md)
  (v0.9.46).

- classify_with:

  One of `"wrb2022"` (default) or `"sibcs"`.

- max_n:

  Optional integer cap on the number of pedons benchmarked. Useful for
  quick development runs.

- soilgrids_lookup_fn:

  Internal: SoilGrids lookup function (defaults to
  [`lookup_soilgrids`](https://hugomachadorodrigues.github.io/soilKey/reference/lookup_soilgrids.md)).
  Override for unit tests to inject a deterministic stub.

- verbose:

  If `TRUE` (default), prints progress.

## Value

A list with elements:

- `predictions`:

  data.frame with one row per pedon:
  `point_id, lon, lat, country, predicted, reference_code, reference_name, agree`.

- `confusion`:

  Confusion table (predicted vs reference) over in-scope rows.

- `accuracy`:

  Overall fraction of correct classifications among in-scope rows.

- `per_rsg`:

  Per-RSG recall data.frame.

- `n_in_scope`:

  Number of pedons with both predicted and reference set.

- `n_total`:

  Total pedons benchmarked.

- `n_errors`:

  Number of pedons where the classifier errored out.

- `errors`:

  List of `(i, id, error)` tuples for classifier errors.

- `config`:

  Recap of arguments used.

## Details

This closes Route B of the v0.9.27 EU-LUCAS roadmap end-to-end: v0.9.44
[`lookup_esdb`](https://hugomachadorodrigues.github.io/soilKey/reference/lookup_esdb.md)
provides the reference label; v0.9.49 (this) provides the loader and the
comparison loop; v0.9.48
[`lookup_soilgrids`](https://hugomachadorodrigues.github.io/soilKey/reference/lookup_soilgrids.md)
fills texture; v0.9.46
[`predict_from_spectra`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_from_spectra.md)
and v0.9.47
[`predict_munsell_from_spectra`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_munsell_from_spectra.md)
can fill the chemistry / Munsell gaps when Vis-NIR is available.

## See also

[`load_lucas_soil_2018`](https://hugomachadorodrigues.github.io/soilKey/reference/load_lucas_soil_2018.md),
[`lookup_esdb`](https://hugomachadorodrigues.github.io/soilKey/reference/lookup_esdb.md),
[`lookup_soilgrids`](https://hugomachadorodrigues.github.io/soilKey/reference/lookup_soilgrids.md).

## Examples

``` r
if (FALSE) { # \dontrun{
pedons <- load_lucas_soil_2018(
  "soil_data/eu_lucas/LUCAS-SOIL-2018-data-report-readme-v2/LUCAS-SOIL-2018-v2",
  countries = c("ES"), max_n = 50)
bench <- benchmark_lucas_2018(
  pedons,
  esdb_root = "soil_data/eu_lucas/ESDB-Raster-Library-1k-GeoTIFF-20240507",
  fill_texture_from = "soilgrids")
bench$accuracy
bench$per_rsg
} # }
```
