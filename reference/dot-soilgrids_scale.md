# Canonical SoilGrids 250m unit-conversion factor per property

SoilGrids stores integer rasters; the published conversion factors are
documented in *Hengl et al. (2017)* and the SoilGrids README. This
internal lookup table applies the right factor so
[`lookup_soilgrids`](https://hugomachadorodrigues.github.io/soilKey/reference/lookup_soilgrids.md)
returns conventional units.

## Usage

``` r
.soilgrids_scale(property)
```

## Arguments

- property:

  One of the SoilGrids properties.

## Value

Numeric scalar. The native integer value times this scale yields the
conventional unit:

- clay/sand/silt – 0.1 (g/kg integer -\> percent)

- phh2o – 0.1 (pH \* 10 integer -\> pH)

- soc – 0.1 (dg/kg integer -\> g/kg)

- bdod – 0.01 (cg/cm^3 integer -\> g/cm^3)

- cec – 0.1 (mmol(c)/kg integer -\> cmol(c)/kg)

- nitrogen – 0.01 (cg/kg integer -\> g/kg)

- ocd – 0.1 (hg/m^3 integer -\> kg/m^3)

- ocs – 0.1 (hg/m^2 integer -\> kg/m^2)

- cfvo – 0.1 (cm^3/dm^3 integer -\> percent vol)
