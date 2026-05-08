# Hydrophobic supplementary qualifier (hf): water-repellent surface

WRB 2022 Ch 5: "Surface horizon (0-5 cm) with hydrophobic character
measurable as MED (Molarity of an Ethanol Droplet) \>= 1 or WDPT (Water
Drop Penetration Time) \>= 60 s." Implementation: textual flag in
`vesicular_pores` (BDsolos: "hidrofóbico", "water repellent") OR a
`water_repellence` field if the loader supplies it.

## Usage

``` r
qual_hydrophobic(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
