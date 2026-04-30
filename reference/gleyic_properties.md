# Gleyic properties (WRB 2022)

Tests whether the profile shows gleyic properties – evidence of
prolonged saturation by groundwater – within the upper 50 cm. Gleyic
properties are diagnostic for Gleysols and qualify many other RSGs
(Endogleyic, Epigleyic qualifiers).

## Usage

``` r
gleyic_properties(
  pedon,
  max_top_cm = 50,
  min_redox_pct = 5,
  stagnic_decay_factor = 3
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_top_cm:

  Maximum top depth (cm) of a candidate layer (default 50, per WRB
  2022).

- min_redox_pct:

  Minimum `redoximorphic_features_pct` (default 5).

- stagnic_decay_factor:

  Numeric threshold or option (see Details).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Sub-test:
[`test_gleyic_features`](https://hugomachadorodrigues.github.io/soilKey/reference/test_gleyic_features.md)
– requires explicit `redoximorphic_features_pct` \>= 5% within the upper
50 cm.

v0.2 deliberately does NOT use the Munsell-based shortcut (chroma \<=
2 + value \>= 4) as a primary criterion: that pattern fits albic /
bleached horizons of Podzols just as well as truly reduced gleyic
horizons. v0.3 will add reductimorphic / oxidimorphic feature
discrimination once we model field-described mottle properties.

## References

IUSS Working Group WRB (2022), Chapter 3, Gleyic properties.
