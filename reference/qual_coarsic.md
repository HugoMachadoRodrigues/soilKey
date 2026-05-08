# Coarsic qualifier (cr): \>= 70% coarse fragments by volume in upper 100 cm

WRB 2022 Ch 5: "Containing layers (in total \>= 30 cm thick) with \>=
70% by volume coarse fragments and/or technic hard material averaged
over a depth of 100 cm from the soil surface."

## Usage

``` r
qual_coarsic(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Details

Applies to: HISTOSOLS, TECHNOSOLS, CRYOSOLS, LEPTOSOLS, PODZOLS,
PLINTHOSOLS, DURISOLS, GYPSISOLS, CALCISOLS.

Implementation: weighted mean of `coarse_fragments_pct` over the upper
100 cm; passes if \\= 70 (or NA if no measurements).
