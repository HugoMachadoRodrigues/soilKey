# Transportic qualifier (tr): transported material (Technosols / Regosols)

WRB 2022 Ch 5: "Soil material that has been moved by humans (mining
spoils, dredged sediments, roadside fill) covering \>= 100 cm of the
upper soil." Detection via `layer_origin` matching
`transport|fill|spoil|dredge|aterro|antropico`.

## Usage

``` r
qual_transportic(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
