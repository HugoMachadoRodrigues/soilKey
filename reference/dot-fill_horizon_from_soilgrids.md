# Fill a horizon (or synthesise a new one) from SoilGrids 250m

Internal helper used by
[`benchmark_lucas_2018`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_lucas_2018.md).
For each requested property, calls `lookup_fn` (default
[`lookup_soilgrids`](https://hugomachadorodrigues.github.io/soilKey/reference/lookup_soilgrids.md))
at `soilgrids_depth`, converts to the soilKey unit and writes onto the
pedon's horizon `horizon_idx` via
`add_measurement(..., source = "inferred_prior")`. Synthesises the
horizon if it does not exist yet (geometry from `horizon_top_cm` /
`horizon_bottom_cm`).

## Usage

``` r
.fill_horizon_from_soilgrids(
  pedon,
  horizon_idx,
  properties,
  soilgrids_depth = "0-5cm",
  horizon_top_cm = 0,
  horizon_bottom_cm = 20,
  horizon_designation = "Ap",
  lookup_fn = lookup_soilgrids
)
```

## Details

Test injection: pass `lookup_fn = function(...) value` to bypass the
network when unit-testing.
