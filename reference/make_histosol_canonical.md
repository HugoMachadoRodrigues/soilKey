# Build the canonical Histosol fixture

Synthetic boreal-mire Histosol: thick (50 cm) surface organic horizon
with OC ~ 35%, low chroma, no exchangeable-base data reported (typical
of histic profiles where laboratory chemistry on organic material is
reported separately). By construction:

- [`histic_horizon`](https://hugomachadorodrigues.github.io/soilKey/reference/histic_horizon.md):
  PASSES on Oa.

- Mineral horizons below; mollic / umbric NA (no BS reported).

## Usage

``` r
make_histosol_canonical()
```

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
