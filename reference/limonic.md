# Limonic horizon (WRB 2022 Ch 3.1)

From Greek *leimon* = meadow. A subaqueous / wet-meadow horizon showing
accumulation of secondary Fe/Mn (oxi)hydroxides from fluctuating redox
cycles. Distinct from *limnic material* (Ch 3.3.10), which is the parent
material; the limonic horizon is the *soil* horizon derived from such
material plus subsequent pedogenesis.

## Usage

``` r
limonic(pedon, min_thickness = 5, min_redox_pct = 5)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Numeric threshold or option (see Details).

- min_redox_pct:

  Numeric threshold or option (see Details).

## Details

v0.3.5 detection: redoximorphic_features_pct \\= 5 AND designation
pattern `Bm` / `Bjm` / `m` as proxy for past meadow wetness.
