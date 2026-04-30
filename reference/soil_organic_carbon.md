# Soil organic carbon (WRB 2022 Ch 3.3.16): organic C that does NOT belong to artefacts. v0.3.3: any layer with oc_pct \>= 0.1 and artefacts_industrial_pct \< 35.

Soil organic carbon (WRB 2022 Ch 3.3.16): organic C that does NOT belong
to artefacts. v0.3.3: any layer with oc_pct \>= 0.1 and
artefacts_industrial_pct \< 35.

## Usage

``` r
soil_organic_carbon(pedon, min_oc = 0.1, max_artefacts = 35)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_oc:

  Numeric threshold or option (see Details).

- max_artefacts:

  Numeric threshold or option (see Details).
