# CPCS 1967 class gates (internal)

The tests of the twelve classes of the French soil classification (CPCS
1967), read by
[`classify_cpcs1967`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_cpcs1967.md)
in the order of `inst/rules/cpcs1967/key.yaml`. Each returns a
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Usage

``` r
raw_mineral_soils_cpcs(pedon)

weakly_developed_soils_cpcs(pedon)

vertisols_cpcs(pedon)

sodic_soils_cpcs(pedon)

andosols_cpcs(pedon)

podzolised_soils_cpcs(pedon)

ferrallitic_soils_cpcs(pedon)

sesquioxide_soils_cpcs(pedon)

hydromorphic_soils_cpcs(pedon)

isohumic_soils_cpcs(pedon)

calcimagnesic_soils_cpcs(pedon)

brunified_soils_cpcs(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).
