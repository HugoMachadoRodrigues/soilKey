# Aeolic material (WRB 2022 Ch 3.3.1)

Wind-deposited material in the upper 20 cm: rounded matt-surfaced sand
grains OR aeroturbation features, AND \< 1% SOC in the upper 10 cm.
v0.3.3 detects via `rock_origin == "aeolian"` OR
`layer_origin == "aeolic"`.

## Usage

``` r
aeolic_material(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
