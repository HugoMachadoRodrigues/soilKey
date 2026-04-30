# Salic horizon (WRB 2022)

Tests whether any horizon meets the salic horizon criteria. The salic
horizon is a horizon of soluble-salt accumulation, diagnostic for
Solonchaks.

## Usage

``` r
salic(
  pedon,
  min_thickness = 15,
  min_ec_dS_m = 15,
  alkaline_min_ec_dS_m = 8,
  alkaline_min_pH = 8.5,
  min_product = 450,
  alkaline_min_product = 240
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Minimum thickness in cm (default 15).

- min_ec_dS_m:

  Primary EC threshold (default 15 dS/m at 25C).

- alkaline_min_ec_dS_m:

  Alkaline-path EC threshold (default 8 dS/m, used when pH(H2O) \\=
  `alkaline_min_pH`).

- alkaline_min_pH:

  Required pH(H2O) for alkaline path (default 8.5).

- min_product:

  Primary path product (EC \* thickness in dS/m \* cm) threshold
  (default 450 per WRB 2022).

- alkaline_min_product:

  Alkaline-path product threshold (default 240).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Sub-tests called:

- [`test_ec_concentration`](https://hugomachadorodrigues.github.io/soilKey/reference/test_ec_concentration.md)
  – EC \\= 15 dS/m (primary) OR (EC \\= 8 dS/m AND pH(H2O) \\= 8.5)
  (alkaline).

- [`test_minimum_thickness`](https://hugomachadorodrigues.github.io/soilKey/reference/test_minimum_thickness.md)
  – thickness \\= 15 cm.

- [`test_salic_product`](https://hugomachadorodrigues.github.io/soilKey/reference/test_salic_product.md)
  – EC \* thickness product \\= 450 (primary) or \\= 240 (alkaline) per
  qualifying layer.

v0.3.1: alkaline-path and product test added (WRB 2022 Ch 3.1.20, p.
49). Earlier versions only enforced the primary EC + thickness gate.

## References

IUSS Working Group WRB (2022). *World Reference Base for Soil
Resources*, 4th edition. International Union of Soil Sciences, Vienna.
Chapter 3.1.20 – Salic horizon (p. 49).
