# Carater ferrico (SiBCS Cap 1, p 35; Cap 5 e Cap 10)

Teor de Fe2O3 (pelo ataque sulfurico-NaOH) entre 180 e 360 g/kg de solo
(= 18%-36% mass) na maior parte dos primeiros 100 cm do horizonte B.
Acima de 360 g/kg = "perferrico" (nao implementado aqui). Discrimina os
Grandes Grupos Eutroferricos / Distroferricos / Aluminoferricos de
Latossolos (Cap 10), Argissolos (Cap 5 Eutroferricos) e Cambissolos (Cap
6 Aluminoferricos).

## Usage

``` r
carater_ferrico(
  pedon,
  min_fe2o3_pct = 18,
  max_fe2o3_pct = 36,
  max_depth_cm = 100
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_fe2o3_pct:

  Limite inferior de Fe2O3 sulfurico em % mass (default 18 = 180 g/kg).

- max_fe2o3_pct:

  Limite superior (exclusivo) em % mass (default 36 = 360 g/kg).

- max_depth_cm:

  Profundidade maxima de B avaliado (default 100).

## Value

[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Implementacao v0.7.4: testa se *algum* horizonte B dentro de
`max_depth_cm` atende ao intervalo. SiBCS estrito ("na maior parte de")
seria uma media ponderada por espessura – refinamento planejado para
v0.7.5.

## References

Embrapa (2018), SiBCS 5a ed., Cap 1, p 35; Cap 5 (Argissolos
Eutroferricos, p 118); Cap 10 (Latossolos).
