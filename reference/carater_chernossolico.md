# Carater chernossolico (SiBCS Cap 5; A chernozemico + Ta alta)

Solos com horizonte A chernozemico
([`horizonte_A_chernozemico`](https://hugomachadorodrigues.github.io/soilKey/reference/horizonte_A_chernozemico.md))
E atividade da argila \\\ge\\ `min_ta` (default 20 cmolc/kg argila) na
maior parte dos primeiros 100 cm do B (inclusive BA). Discrimina os
Subgrupos chernossolicos de Argissolos (Cap 5: PV, PVA).

## Usage

``` r
carater_chernossolico(pedon, min_ta = 20)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_ta:

  Threshold de atividade da argila (default 20).

## Value

[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

Embrapa (2018), SiBCS 5a ed., Cap 5, p 134; Cap 7 (Chernossolos).
