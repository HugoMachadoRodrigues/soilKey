# Glacic layer (USDA Soil Taxonomy, 13th edition)

"A glacic layer is massive ice or ground ice in the form of ice lenses
or wedges. The layer is 30 cm or more thick and contains 75 percent or
more visible ice." – KST 13ed, Ch 3, p 45.

## Usage

``` r
glacic_layer_usda(pedon, max_top_cm = 100, min_thickness_cm = 30)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_top_cm:

  Maximum top depth (default 100 cm; subgroup-level depth bound).

- min_thickness_cm:

  Minimum thickness (default 30 cm).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Diagnostic for the Glacistels great group of Histels and the Glacic
subgroup modifier in Gelisols.

Implementation (v0.8.x): Detected via designation containing 'ff'
(massive ice) per KST notation, with thickness \>= 30 cm. Refinement to
use an `ice_pct` schema column is deferred to v0.9.

## References

Soil Survey Staff (2022), KST 13ed, Ch. 3, p 45.
