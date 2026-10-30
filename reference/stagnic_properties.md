# Stagnic properties (WRB 2022)

Tests for stagnic properties (WRB 2022 Ch 3.2.14, p. 81) in a layer
starting within the upper `max_top_cm` (default 100).

## Usage

``` r
stagnic_properties(
  pedon,
  max_top_cm = 100,
  min_redox_pct = 5,
  decay_factor = 3
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_top_cm:

  Maximum top depth (cm) of candidate layers (default 100).

- min_redox_pct:

  The oximorphic features are more than this % of the layer (default 5).

- decay_factor:

  Not used since v0.9.312 (the decay test was removed); kept so that
  existing calls still work.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Since v0.9.312 the layer reading is that of
[`gleyic_properties`](https://hugomachadorodrigues.github.io/soilKey/reference/gleyic_properties.md)
(Hugo's reading for descriptions that do not say where the mottles sit):
a layer with more than 5% oximorphic features that are not gleyic
(criteria 1-3: the matrix has no reductimorphic colour, the layer does
not lie directly on a layer with such a matrix, and the site records no
groundwater within 150 cm that is not perched, or a claric layer lies
directly above it); a layer with claric colours in 95% or more of its
area above an abrupt textural difference or a layer of 1.5 kg/dm3 or
more (criterion 4), or above a layer with stagnic oximorphic features
(criterion 5). Mottles with neither colour nor water evidence leave the
layer `NA`. Until v0.9.311 a layer with 5% redox features passed when
the features decayed with depth, which is not in WRB 2022.

## References

IUSS Working Group WRB (2022), Chapter 3.2.14, Stagnic properties.
