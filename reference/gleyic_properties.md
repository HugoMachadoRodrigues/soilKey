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
discrimination once we model field-described mottle properties. v0.9.72
adds the designation-suffix path (opt-in).

## v0.9.72 designation morphological inference (opt-in)

Field-described Brazilian Gleissolos profiles (e.g.\\ the Embrapa Redape
curated dataset) routinely encode gleyic properties via the designation
suffix `g` (e.g.\\ `Cg`, `Cg1`, `Cgn`, `Apg`) plus low-chroma Munsell
colours (chroma \\= 2), without recording `redoximorphic_features_pct`
as a numeric percent. The strict canonical test then returns `NA` on
every horizon and Gleissolos cascade to other Orders.

With `options(soilKey.gleyic_designation_inference = TRUE)` the function
accepts a layer as gleyic when:

1.  the canonical `redoximorphic_features_pct` test is `NA` for that
    layer, AND

2.  the designation matches `[A-Z]+g[0-9a-z]?` (a horizon name with a
    `g` suffix in the master letter sequence, e.g.\\ `Cg`, `Bg2`, `Apg`,
    `Cgn`), AND

3.  the layer has `munsell_chroma_moist <= 2` (low-chroma reduced
    colour) when Munsell is recorded; if Munsell is missing on the layer
    the suffix alone is sufficient (designation suffix is the most
    direct signal of pedologist field judgment).

This is conservative: the suffix `g` is a master-letter modifier in the
FAO/Embrapa horizon nomenclature that explicitly means "gleyic-affected"
– the curator already made the call. Default is `FALSE` (canonical
behaviour preserved).

## References

IUSS Working Group WRB (2022), Chapter 3, Gleyic properties.
