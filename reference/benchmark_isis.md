# Compare soilKey with ISRIC's names for ISIS profiles

Classifies each pedon read by
[`read_isis`](https://hugomachadorodrigues.github.io/soilKey/reference/read_isis.md)
with the WRB 2022 and Soil Taxonomy keys and sets the result beside the
names ISRIC gives the profile. The WRB comparison uses the Reference
Soil Group, and only when ISRIC's name is from the 2022 edition; the
USDA one uses the order (ISRIC's USDA names are from the 1975 to 1999
editions, so subgroups are shown but not scored).

## Usage

``` r
benchmark_isis(pedons)
```

## Arguments

- pedons:

  A list of
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)s
  from `read_isis`.

## Value

A data frame, one row per profile, with ISRIC's and soilKey's names,
`wrb_agree` and `usda_order_agree` (`NA` where ISRIC gives no comparable
name) and the number of close calls of each key.
