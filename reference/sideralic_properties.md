# Sideralic properties (WRB 2022 Ch 3.2.13)

Mineral material with a relatively low CEC. WRB 2022 (3.2.13) requires
BOTH:

1.  one or both of: clay \>= 8% AND CEC/clay \< 24 cmol_c/kg clay; OR
    bulk CEC \< 2 cmol_c/kg soil;

2.  evidence of soil formation as defined in criterion 3 of the cambic
    horizon (`test_cambic_soil_formation`).

Both must be met by the SAME layer. Criterion 2 was added in v0.9.127
(previously only criterion 1 was enforced); where the soil-formation
evidence cannot be assessed (no Munsell/clay/Fe/carbonate adjacency
data) the result is `NA` rather than a false positive.

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

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
recording whether the diagnostic is present, the qualifying layers, and
the supporting evidence.
