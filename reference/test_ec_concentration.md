# Test for electrical conductivity above threshold (per layer)

Default 15 dS/m (salic horizon, WRB 2022 Ch 3.1.20). The WRB salic
horizon also accepts an alkaline alternate: EC \\= 8 dS/m if pH(H2O) \\=
8.5. Pass `alkaline_min_dS_m = 8` and `alkaline_min_pH = 8.5` to enable
that path – a layer is then \\qualifying\\ if it satisfies the primary
OR the alkaline gate. The `path` field in each `details` entry records
which gate carried the layer.

## Usage

``` r
test_ec_concentration(
  h,
  min_dS_m = 15,
  alkaline_min_dS_m = NA_real_,
  alkaline_min_pH = 8.5,
  candidate_layers = NULL
)
```

## Arguments

- h:

  Horizons table.

- min_dS_m:

  Primary EC threshold (default 15).

- alkaline_min_dS_m:

  Optional alkaline-path EC threshold (default `NA`: alkaline path
  disabled).

- alkaline_min_pH:

  Required pH(H2O) for the alkaline path (default 8.5; only used when
  `alkaline_min_dS_m` is set).

- candidate_layers:

  Optional layer index restriction.
