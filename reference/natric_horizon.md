# Natric horizon (WRB 2022)

Tests for the natric horizon: an argic horizon with diagnostic sodium
accumulation (ESP \>= 15%) within at least one argic layer. Diagnostic
of Solonetz.

## Usage

``` r
natric_horizon(pedon, min_esp = 15, min_pH_h2o = 7)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_esp:

  Minimum ESP % (default 15).

- min_pH_h2o:

  Minimum pH(H2O) for the ESP-only path (default 7.0; alkaline gate to
  exclude false-positive acidic Bt horizons).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## v0.9.76 designation + ESP-only inference (opt-in)

Field-described Solonetz profiles in NCSS / KSSL data routinely reach
the natric ESP threshold (computed from `na_cmol / cec_cmol`) without
satisfying the strict
[`argic()`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md)
clay-increase test, because surveyors record `Btk`-suffix designations
(carbonates dominate the horizon designation choice) rather than
`Btn`/`Bn` or `clay_pct` is missing.

With `options(soilKey.natric_designation_inference = TRUE)` the function
accepts a layer as natric when the canonical argic test returns NA or
FALSE AND *either*:

1.  the designation matches `[A-Z][a-z0-9]*n` (an `n` master-letter
    modifier in the horizon name – e.g.\\ `Btn`, `Btnz`, `Bn`, the
    curator's direct assertion that natric features are present), OR

2.  ESP \>= `min_esp` on a B-prefixed subsoil layer (`top_cm > 20`) AND
    the layer's pH(H2O) \>= 7 (alkaline – typical of true natric,
    excludes acidic Bt horizons that happen to read high Na from
    sea-spray).

Default is `FALSE` (canonical behaviour preserved).

## References

IUSS Working Group WRB (2022), Chapter 3, Natric horizon.
