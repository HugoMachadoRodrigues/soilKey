# Test the salic horizon EC \* thickness product (WRB 2022)

Tests whether each candidate layer's product
`ec_dS_m * (bottom_cm - top_cm)` reaches the canonical WRB 2022
threshold (Ch 3.1.20, p. 49):

- `>= 450` dS/m \* cm for the primary path (EC \\= 15);

- `>= 240` dS/m \* cm for the alkaline path (EC \\= 8 with pH(H2O) \\=
  8.5).

The path used per layer is taken from a prior
[`test_ec_concentration`](https://hugomachadorodrigues.github.io/soilKey/reference/test_ec_concentration.md)
result (its `details[[i]]\$path` field). When no prior is supplied,
every candidate is treated as "primary" and the 450 threshold is applied
uniformly.

## Usage

``` r
test_salic_product(
  h,
  min_product = 450,
  alkaline_min_product = 240,
  ec_path_lookup = NULL,
  candidate_layers = NULL
)
```

## Arguments

- h:

  Horizons table.

- min_product:

  Primary product threshold (default 450).

- alkaline_min_product:

  Alkaline-path product threshold (default 240).

- ec_path_lookup:

  Optional named list (keys = layer index as character) returning either
  "primary" or "alkaline" per layer – typically built by passing
  `test_ec_concentration(...)\$details`.

- candidate_layers:

  Layer index restriction (typically the layers that already passed the
  primary EC gate).
