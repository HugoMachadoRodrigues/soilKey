# Vitrandic Subgroup helper (USDA, KST 13ed)

Pass when, throughout one or more horizons with total thickness \>= 18
cm within 75 cm of the surface, BOTH:

- More than 35% (volume) particles \>= 2 mm of which \> 66% are
  cinders/pumice; OR fine-earth has \>= 30% particles 0.02-2 mm AND \>=
  5% volcanic glass (in 0.02-2 mm); AND

- (Al + 0.5 \* Fe) \* 60 + volcanic_glass_pct \>= 30.

KST 13ed, Ch 9 various.

## Usage

``` r
vitrandic_subgroup_usda(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Implementation simplified to the volcanic-glass branch:
volcanic_glass_pct \>= 5 AND (Al + 0.5 \* Fe) \* 60 + volcanic_glass_pct
\>= 30.
