# Artefacts (WRB 2022 Ch 3.3.2)

Per the canonical definition: human-made / human-altered / human-
excavated material. v0.3.3 returns the layers where
`artefacts_pct >= 1`.

## Usage

``` r
artefacts(pedon, min_pct = 1)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_pct:

  Numeric threshold or option (see Details).
