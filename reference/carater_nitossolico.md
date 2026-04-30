# Carater nitossolico (SiBCS Cap 5)

Solos com morfologia (estrutura e cerosidade) semelhante aos Nitossolos,
mas diferindo por apresentar relacao textural B/A \\\>\\ 1,5 OU
policromia (multiplas matizes Munsell em horizontes B) dentro de
`max_depth_cm` cm. Discrimina os Subgrupos nitossolicos de Argissolos
(Cap 5: PV, PVA).

## Usage

``` r
carater_nitossolico(pedon, max_b_a_ratio = 1.5, max_depth_cm = 150)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_b_a_ratio:

  Razao maxima B/A para Nitossolos (default 1.5); Argissolos
  nitossolicos tem ratio \> 1.5.

- max_depth_cm:

  Profundidade maxima do B avaliado (default 150).

## Value

[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Implementacao v0.7.4 (aproximacao):

- [`cerosidade`](https://hugomachadorodrigues.github.io/soilKey/reference/cerosidade.md)
  \\\ge\\ comum + moderada, AND

- Razao textural B/A \> `max_b_a_ratio` (default 1.5), OR policromia
  (\\\ge\\ 2 matizes distintos em B).

## References

Embrapa (2018), SiBCS 5a ed., Cap 5, pp 129-131; Cap 13.
