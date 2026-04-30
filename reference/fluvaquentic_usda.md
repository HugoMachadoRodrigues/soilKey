# Fluvaquentic Subgroup helper (irregular OC decrease + aquic)

Pass when:

- Irregular decrease in organic carbon between 25 cm and 125 cm (or to a
  densic/lithic/paralithic contact); AND

- Aquic conditions in some horizon within 75 cm
  (`aquic_conditions_usda(pedon, max_top_cm = 75)`).

## Usage

``` r
fluvaquentic_usda(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Implementation: tests whether OC values are non-monotonic (some upward
variation) within 25-125 cm.

## References

Soil Survey Staff (2022), KST 13ed, Ch. 9.
