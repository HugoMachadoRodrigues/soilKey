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
benchmark_isis(pedons, quiet = FALSE)
```

## Arguments

- pedons:

  A list of
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)s
  from `read_isis`.

- quiet:

  If `TRUE`, no message.

## Value

A data frame, one row per profile, with ISRIC's and soilKey's names,
`wrb_agree` and `usda_order_agree` (`NA` where ISRIC gives no comparable
name) and the number of close calls of each key. The attribute
`"wrb_editions"` is a data frame with one row per edition of ISRIC's
latest name: the profiles, those soilKey named, those with the same
Reference Soil Group, and whether the edition is scored. The counts of
the editions before 2022 are for information only.

## Details

A profile whose latest ISRIC name is older than 2022 is not scored:
`wrb_agree` is `NA` and it counts neither for nor against the agreement.
A message says how many profiles were scored and how many were left out,
by edition.
