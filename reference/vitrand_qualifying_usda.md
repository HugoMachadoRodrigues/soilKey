# Vitrands qualifier (Cap 6, pp 117-118) Pass when 1500 kPa water retention \< 15% (air-dried) and \< 30% (undried) throughout 60%+ of the thickness. v0.8 proxy: uses water_content_1500kpa \< 15%.

Vitrands qualifier (Cap 6, pp 117-118) Pass when 1500 kPa water
retention \< 15% (air-dried) and \< 30% (undried) throughout 60%+ of the
thickness. v0.8 proxy: uses water_content_1500kpa \< 15%.

## Usage

``` r
vitrand_qualifying_usda(pedon, max_top_cm = 60)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_top_cm:

  Numeric threshold or option (see Details).
