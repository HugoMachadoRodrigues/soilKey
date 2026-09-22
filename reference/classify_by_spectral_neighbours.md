# Classify a soil by spectral similarity to OSSL reference profiles

Given a Vis-NIR (or MIR) spectrum and an OSSL reference library enriched
with WRB / SiBCS / USDA labels, returns the K most spectrally similar
profiles plus a probabilistic class prediction aggregated from their
labels.

## Usage

``` r
classify_by_spectral_neighbours(
  spectrum,
  ossl_library,
  system = c("wrb2022", "sibcs", "usda"),
  k = 25L,
  preprocess = "snv+sg1",
  region = NULL,
  verbose = TRUE
)
```

## Arguments

- spectrum:

  Numeric vector or 1-row matrix (the query spectrum). Must align (after
  preprocessing) with the column space of `ossl_library$Xr`.

- ossl_library:

  A list with `Xr` (numeric matrix, rows = OSSL training profiles, cols
  = wavelengths) and `Yr` (data frame keyed by property; *must include*
  a column named `wrb_rsg` and / or `sibcs_ordem` / `usda_order` for the
  labels to aggregate over). `ossl_library` may also carry `lat` and
  `lon` columns in `Yr` for the regional filter.

- system:

  One of `"wrb2022"` (default), `"sibcs"`, `"usda"`. Controls which
  label column of `Yr` is aggregated.

- k:

  Number of nearest neighbours (default 25).

- preprocess:

  Pre-processing pipeline; passed to
  [`preprocess_spectra`](https://hugomachadorodrigues.github.io/soilKey/reference/preprocess_spectra.md).
  Default `"snv+sg1"`.

- region:

  Optional `list(lat, lon, radius_km)` for a regional filter on
  `ossl_library$Yr$lat / lon`.

- verbose:

  Emit a `cli` summary.

## Value

A list with three elements:

- `distribution`:

  A `data.table` with columns `class`, `n_neighbours`, `probability` (=
  `n_neighbours / k`), sorted by probability.

- `neighbours`:

  A `data.table` with one row per neighbour (top K), columns `rank`,
  `distance`, `class`, plus any other columns present in
  `ossl_library$Yr`.

- `query`:

  The query metadata (system, k, region filter, n_library_rows,
  n_filtered).

## Details

This is the \*\*spectral analogy\*\* classifier. It does not replace the
deterministic key in
[`classify_wrb2022`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_wrb2022.md)
/
[`classify_sibcs`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_sibcs.md)
/
[`classify_usda`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_usda.md);
instead it provides a high-prior "expected class" before the user has
lab data, reducing the search space when collecting confirming
attributes.

## Distance metric

By default we compute distances on PLS scores (matching the resemble /
OSSL recipe), with PLS components fit on the OSSL reference Yr matrix.
When `resemble` is unavailable, we fall back to PCA scores from
[`stats::prcomp`](https://rdrr.io/r/stats/prcomp.html) on the
preprocessed Xr – a defensible-but-simpler heuristic.

## Region filter

Optional `lat / lon / radius_km` arguments filter the OSSL library to
profiles within `radius_km` (great-circle) of the query location before
computing distances. This implements the "biome-aware" use case the
architecture document calls for: a Cerrado profile shouldn't have its
class inferred from spectral neighbours in the Boreal taiga.

## See also

[`predict_ossl_mbl`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_ossl_mbl.md)
(predicts attributes),
[`classify_wrb2022`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_wrb2022.md)
(the deterministic key).

## Examples

``` r
if (FALSE) { # \dontrun{
# Toy run against the bundled demo library (synthetic):
data(ossl_demo_sa)
# Inject a fake label column for the demo (real OSSL has it):
ossl_demo_sa$Yr$wrb_rsg <- sample(c("FR", "AC", "LX", "AL"),
                                    nrow(ossl_demo_sa$Yr),
                                    replace = TRUE)
query <- ossl_demo_sa$Xr[1, ]
res <- classify_by_spectral_neighbours(query, ossl_demo_sa,
                                        k = 10)
res$distribution    # ranked classes
res$neighbours      # the 10 most similar profiles
} # }
```
