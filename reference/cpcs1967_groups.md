# CPCS 1967 group gates (internal)

The tests of the 101 groups of the French soil classification (CPCS
1967), read by
[`classify_cpcs1967`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_cpcs1967.md)
in the order of `inst/rules/cpcs1967/groups/`. Each returns a
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Usage

``` r
deposit_origin_cpcs(pedon, kind)

rock_near_surface_cpcs(pedon, within = 10)

patterned_ground_cpcs(pedon)

yermic_cpcs(pedon)

takyric_cpcs(pedon)

aeolian_sands_cpcs(pedon)

ice_segregation_cpcs(pedon, strong = TRUE, network = FALSE)

allophanic_weakly_developed_cpcs(pedon)

lithocalcic_cpcs(pedon)

subdesert_grey_cpcs(pedon)

rounded_surface_structure_cpcs(pedon)

saturated_andosol_cpcs(pedon)

cryptorendzina_cpcs(pedon)

rendzina_cpcs(pedon)

carbonated_humic_cpcs(pedon)

melanised_calcic_cpcs(pedon)

gypseous_rendziniform_cpcs(pedon)

isohumic_om20_cpcs(pedon, above = NA, at_least = NA)

textural_b_leached_cpcs(pedon)

sod_podzolic_cpcs(pedon)

podzol_ashy_a2_cpcs(pedon)

podzolic_cpcs(pedon)

ochre_podzolic_cpcs(pedon)

crypto_podzolic_cpcs(pedon)

boreal_podzol_cpcs(pedon)

alpine_podzol_cpcs(pedon)

molken_podzol_cpcs(pedon)

tropical_groundwater_podzol_cpcs(pedon)

ferruginous_leached_cpcs(pedon)

ferruginous_impoverished_cpcs(pedon)

fersiallitic_calcium_reserve_cpcs(pedon)

ferrallitic_humic_cpcs(pedon)

ferrallitic_impoverished_cpcs(pedon)

ferrallitic_reworked_cpcs(pedon)

ferrallitic_rejuvenated_cpcs(pedon)

peat_fibre_cpcs(pedon, kind)

humic_gley_cpcs(pedon)

iron_pan_hydromorphic_cpcs(pedon)

calcium_redistribution_cpcs(pedon)

amphigley_cpcs(pedon)

gley_cpcs(pedon)

stagnogley_cpcs(pedon)

solodised_cpcs(pedon)

solonetz_cpcs(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- kind, within, strong, network, above, at_least:

  Arguments written in the rule files.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).
