# Cohesic horizon (WRB 2022, Chapter 3.1.7)

A subsurface horizon with a massive or weak subangular blocky structure,
poor in organic matter and iron oxides, with a kaolinite-dominated,
low-activity clay fraction, and hard when dry. It is the diagnostic
basis of the *Cohesic* qualifier and corresponds to the *caracter coeso*
of the Brazilian SiBCS (typical of the coastal "Tabuleiros" Latossolos
and Argissolos).

## Usage

``` r
cohesic(
  pedon,
  max_oc = 0.5,
  min_clay = 15,
  max_cec_per_clay = 24,
  min_thickness = 10
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_oc:

  Maximum soil organic carbon (%), default 0.5.

- min_clay:

  Minimum clay (%), default 15.

- max_cec_per_clay:

  Maximum CEC in cmol_c per kg clay, default 24.

- min_thickness:

  Minimum thickness in cm, default 10.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Diagnostic criteria (all required): mineral material with (1) \< 0.5%
soil organic carbon; (2) \>= 15% clay; (3) a CEC (by 1 M NH4OAc, pH 7)
of \< 24 cmol_c per kg clay; (4) a massive or weak subangular blocky
structure; (5) not cemented; (6) a rupture-resistance class, when dry,
of at least hard; and (7) a thickness of \>= 10 cm.

## References

IUSS Working Group WRB (2022), Chapter 3.1.7, Cohesic horizon.
