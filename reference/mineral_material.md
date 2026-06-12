# Mineral material (WRB 2022 Ch 3.3.11): \< 20% SOC AND \< 35% volume artefacts containing \>= 20% organic carbon. The complement of organic_material / organotechnic_material.

Mineral material (WRB 2022 Ch 3.3.11): \< 20% SOC AND \< 35% volume
artefacts containing \>= 20% organic carbon. The complement of
organic_material / organotechnic_material.

## Usage

``` r
mineral_material(pedon, max_oc = 20, max_organotechnic = 35)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_oc:

  Numeric threshold or option (see Details).

- max_organotechnic:

  Numeric threshold or option (see Details).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
recording whether the diagnostic is present, the qualifying layers, and
the supporting evidence.
