# Vitric properties (WRB 2022 Ch 3.2.16)

Volcanic glass \\= 5% in 0.02-2 mm fraction, Al_ox + 1/2 Fe_ox \\= 0.4%,
phosphate retention \\= 25%.

## Usage

``` r
vitric_properties(
  pedon,
  min_glass_pct = 5,
  min_alfe = 0.4,
  min_p_retention = 25
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_glass_pct:

  Numeric threshold or option (see Details).

- min_alfe:

  Numeric threshold or option (see Details).

- min_p_retention:

  Numeric threshold or option (see Details).
