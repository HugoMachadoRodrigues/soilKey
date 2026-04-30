# Spodic Subgroup helper for Psammorthels/Psammoturbels

Pass when a horizon \>= 5 cm thick has any of:

- In \>= 25% of pedon, extremely weakly coherent or more coherent due to
  pedogenic cementation by OM and Al (with or without Fe); OR

- Al + 0.5 \* Fe (oxalate) \>= 0.25, and half that or less in an
  overlying horizon; OR

- ODOE \>= 0.12, and value half as high or lower in an overlying
  horizon.

## Usage

``` r
spodic_subgroup_usda(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Implementation simplified to: any horizon with (al_ox_pct + 0.5 \*
fe_ox_pct) \>= 0.25 with an overlying layer having \<= half that value.
