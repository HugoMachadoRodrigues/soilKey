# Carater psamitico (SiBCS Cap 10)

Solos com conteudo de argila inferior a `max_clay_pct` (default 20% =
200 g/kg) na maior parte dos primeiros `max_depth_cm` (default 150 cm) a
partir da superficie do solo. Discrimina os Subgrupos psamiticos de
Latossolos Amarelos Distroficos (Cap 10 LA 2.6.1).

## Usage

``` r
carater_psamitico(pedon, max_clay_pct = 20, max_depth_cm = 150)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_clay_pct:

  Default 20% = 200 g/kg.

- max_depth_cm:

  Default 150 cm.

## Value

[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Implementacao: testa se a media ponderada por espessura de `clay_pct`
dentro de \[0, max_depth_cm\] esta abaixo de max_clay_pct.

## References

Embrapa (2018), SiBCS 5a ed., Cap 10 LA, p 203.
