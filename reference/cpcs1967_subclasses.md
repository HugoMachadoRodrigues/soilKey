# CPCS 1967 sub-class gates (internal)

The tests of the 35 sub-classes of the French soil classification (CPCS
1967), read by
[`classify_cpcs1967`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_cpcs1967.md)
in the order of `inst/rules/cpcs1967/subclasses/`. Each returns a
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Usage

``` r
cold_desert_raw_soils_cpcs(pedon)

hot_desert_raw_soils_cpcs(pedon)

permafrost_weakly_developed_soils_cpcs(pedon)

humic_weakly_developed_soils_cpcs(pedon)

xeric_weakly_developed_soils_cpcs(pedon)

vertisols_reduced_external_drainage_cpcs(pedon)

cold_country_andosols_cpcs(pedon)

gypseous_soils_cpcs(pedon)

carbonated_soils_cpcs(pedon)

isohumic_humid_cpcs(pedon)

isohumic_very_cold_cpcs(pedon)

isohumic_cool_rainy_cpcs(pedon)

isohumic_hot_rainy_cpcs(pedon)

brunified_tropical_cpcs(pedon)

brunified_boreal_cpcs(pedon)

brunified_continental_cpcs(pedon)

brunified_temperate_cpcs(pedon)

podzolised_hydromorphic_cpcs(pedon)

podzols_cold_climate_cpcs(pedon)

podzolised_temperate_cpcs(pedon)

ferruginous_tropical_soils_cpcs(pedon)

ferrallitic_weakly_desaturated_cpcs(pedon)

ferrallitic_moderately_desaturated_cpcs(pedon)

ferrallitic_strongly_desaturated_cpcs(pedon)

hydromorphic_organic_cpcs(pedon)

hydromorphic_moderately_organic_cpcs(pedon)

sodic_degraded_structure_cpcs(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).
