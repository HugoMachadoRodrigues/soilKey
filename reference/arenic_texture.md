# Arenic texture (WRB 2022)

Tests the texture of the Arenosols (WRB 2022 Ch 4, p. 124): within 100
cm of the mineral soil surface, a weighted average texture of loamy sand
or sand (`silt + 2 * clay < 30`), layers of finer texture and layers
with \\\ge\\ 40% coarse fragments each with a combined thickness of \<
15 cm (v0.9.314; until then every layer had to be coarser than sandy
loam). Diagnostic of Arenosols.

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

  One of `"soilkey"` (default; the WRB 2022 test,
  `test_arenosol_texture`) or `"aqp"` (LUCAS-friendly fallback: passes
  when sand \>= 70\\ reads `getOption("soilKey.diagnostic_engine")`.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Sub-test: `test_arenosol_texture`. A part of the 100 cm whose texture or
coarse fragments are not recorded, or that is not described, leaves the
result undecided (NA) when it could change it.

v0.3 limitations: WRB 2022 Arenosol also requires that no other
diagnostic horizon (argic, ferralic, etc.) is present, but those
exclusions happen at the key level via canonical RSG order.

## References

IUSS Working Group WRB (2022), Chapter 5, Arenosols.
