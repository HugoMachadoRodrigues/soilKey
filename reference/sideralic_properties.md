# Sideralic properties (WRB 2022 Ch 3.2.13)

Mineral material with low CEC: clay \>= 8% AND CEC/clay \< 24, OR bulk
CEC \< 2 cmol_c/kg soil. Plus evidence of soil formation (cambic-style
criterion 3).

## Usage

``` r
sideralic_properties(pedon, max_cec_per_clay = 24, max_bulk_cec = 2)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_cec_per_clay:

  Numeric threshold or option (see Details).

- max_bulk_cec:

  Numeric threshold or option (see Details).
