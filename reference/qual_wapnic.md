# Wapnic qualifier (wp): soft, moist limnic material \>= 80% CaCO3

WRB 2022 Ch 5 (Calcisols / Gleysols / Cryosols): "Having soft, moist
limnic material that contains \>= 80% by mass CaCO3 equivalent within
100 cm of the soil surface."

## Usage

``` r
qual_wapnic(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Details

Implementation: `caco3_pct` \>= 80 in any layer with top \<= 100.
