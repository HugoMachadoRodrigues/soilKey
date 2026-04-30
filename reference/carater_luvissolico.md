# Carater luvissolico (SiBCS Cap 5; Ta + S alta)

Solos com atividade da argila \\\ge\\ `min_ta` (default 20 cmolc/kg
argila) E soma de bases (S) \\\ge\\ `min_s` (default 5 cmolc/kg solo),
ambos na maior parte dos primeiros 100 cm do horizonte B. Discrimina os
Subgrupos luvissolicos de Argissolos (Cap 5: PV, PVA).

## Usage

``` r
carater_luvissolico(pedon, min_ta = 20, min_s = 5, max_depth_cm = 100)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_ta:

  Threshold de atividade da argila em cmolc/kg argila (default 20).

- min_s:

  Threshold de S em cmolc/kg solo (default 5).

- max_depth_cm:

  Profundidade maxima de B avaliado (default 100).

## Value

[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Note: o threshold de Ta para "luvissolico" e 20 (vs 27 para
`atividade_argila_alta` canonico). S = Ca + Mg + K + Na trocaveis.

## References

Embrapa (2018), SiBCS 5a ed., Cap 5, p 134; Cap 11 (Luvissolos).
