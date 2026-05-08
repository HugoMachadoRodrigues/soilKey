# Tonguic qualifier (tg): tongues of A horizon penetrating into B

WRB 2022 Ch 5 (Chernozems / Kastanozems / Phaeozems / Umbrisols):
"Showing tongues of an A horizon penetrating \>= 50 cm into the B
horizon (irregular boundary; A material in B-depth pockets)."

## Usage

``` r
qual_tonguic(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Details

Implementation: designation pattern `^A.*\+|A/B|B/A` OR
`transition_horizon_topography` (BDsolos column for "Transição de
horizonte subjacente - Topografia") matching irregular / tongued
patterns.
