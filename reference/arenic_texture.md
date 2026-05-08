# Arenic texture (WRB 2022)

Tests whether the upper 100 cm is uniformly coarser than sandy loam
(i.e., `silt + 2 * clay < 30` in every layer). Diagnostic of Arenosols.

## Usage

``` r
arenic_texture(pedon, max_top_cm = 100, engine = NULL)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_top_cm:

  Maximum top depth (cm) of layers to be tested (default 100, per WRB
  2022).

- engine:

  One of `"soilkey"` (default; strict WRB sand threshold via
  [`test_coarse_texture_throughout`](https://hugomachadorodrigues.github.io/soilKey/reference/test_coarse_texture_throughout.md))
  or `"aqp"` (LUCAS-friendly fallback: passes when sand \>= 70\\ reads
  `getOption("soilKey.diagnostic_engine")`.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Sub-test:
[`test_coarse_texture_throughout`](https://hugomachadorodrigues.github.io/soilKey/reference/test_coarse_texture_throughout.md).

v0.3 limitations: WRB 2022 Arenosol also requires that no other
diagnostic horizon (argic, ferralic, etc.) is present, but those
exclusions happen at the key level via canonical RSG order.

## References

IUSS Working Group WRB (2022), Chapter 5, Arenosols.
