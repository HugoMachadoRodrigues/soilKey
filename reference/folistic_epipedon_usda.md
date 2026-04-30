# Folistic epipedon (USDA Soil Taxonomy, 13th edition)

A freely-drained surface organic horizon. Differs from the histic
epipedon in that it is saturated for less than 30 days per year.
Diagnostic for the Folists suborder of Histosols and the Folistels great
group of Histels.

## Usage

``` r
folistic_epipedon_usda(pedon, min_oc_pct = 12, min_thickness_cm = 15)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_oc_pct:

  Minimum OC for organic soil material (default 12).

- min_thickness_cm:

  Minimum thickness (default 15 cm).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

KST 13ed required characteristics (Ch. 3, pp 13-14):

- Saturated \< 30 days/year (and not artificially drained); AND

- Organic soil material: 15+ cm thick (with Sphagnum-rich exception
  20-60 cm) OR Ap with OC \>= 8 percent.

## References

Soil Survey Staff (2022), KST 13ed, Ch. 3, pp 13-14.
