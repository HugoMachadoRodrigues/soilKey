# Thapto-Humic Subgroup helper

Pass when a buried layer meets criteria for histic, mollic, umbric, or
melanic epipedon within 200 cm of the soil surface, OR buried O and
dark-colored A horizons (V \<= 3 moist, combined thickness \>= 20 cm, OC
\>= 1 percent Holocene-age) within 200 cm (KST 13ed, p 189-191).

## Usage

``` r
thapto_humic_usda(
  pedon,
  max_top_cm = 200,
  min_thickness_cm = 20,
  min_oc_pct = 1
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_top_cm:

  Default 200.

- min_thickness_cm:

  Default 20.

- min_oc_pct:

  Default 1.0.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Implementation detects buried horizons via designation containing 'b'
(KST notation for buried) AND dark color (V \<= 3) within 200 cm.

## References

Soil Survey Staff (2022), KST 13ed, Ch. 9 various.
