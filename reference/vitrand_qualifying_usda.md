# Vitrands qualifier (Cap 6, pp 117-118) Pass when 1500 kPa water retention \< 15% (air-dried) AND \< 30% (undried) throughout 60%+ of the thickness. The undried branch (KST 13ed crit) is enforced only on layers that carry `water_content_1500kpa_undried`; where that column is absent the air-dried branch alone is used, so existing data classifies identically (v0.9.128).

Vitrands qualifier (Cap 6, pp 117-118) Pass when 1500 kPa water
retention \< 15% (air-dried) AND \< 30% (undried) throughout 60%+ of the
thickness. The undried branch (KST 13ed crit) is enforced only on layers
that carry `water_content_1500kpa_undried`; where that column is absent
the air-dried branch alone is used, so existing data classifies
identically (v0.9.128).

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
