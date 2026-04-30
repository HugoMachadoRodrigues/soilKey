# Permafrost (USDA Soil Taxonomy, 13th edition)

"Permafrost is defined as a thermal condition in which a material
(including soil material) remains below 0 C for 2 or more years in
succession." – KST 13ed, Ch 3, p 47.

## Usage

``` r
permafrost_within_usda(pedon, max_top_cm = 100)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_top_cm:

  Maximum depth where permafrost must occur (default 100 cm – Gelisols
  criterion at Order level).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Permafrost is the defining characteristic of the Gelisols order (within
100 cm of the soil surface) and qualifies many subgroups across
Histosols (Histels), Inceptisols, and others.

Implementation: Uses `permafrost_temp_C` from schema. A layer qualifies
as permafrost when its `permafrost_temp_C` is \<= 0 C. The function
checks whether any qualifying layer occurs within `max_top_cm` of the
surface.

## References

Soil Survey Staff (2022), KST 13ed, Ch. 3, p 47.
