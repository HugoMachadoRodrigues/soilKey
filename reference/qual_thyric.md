# Thyric qualifier (ty): organic technic material in upper 100 cm

WRB 2022 Ch 5 (Leptosols / Technosols): "Containing \>= 20% by volume
technic hard material with organic origin (waste organic refuse,
peat-like industrial residues) in upper 100 cm." Implementation:
`artefacts_industrial_pct` populated AND organic-rich (oc_pct \>= 5%).

## Usage

``` r
qual_thyric(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
