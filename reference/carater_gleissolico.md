# Carater gleissolico (SiBCS Cap 5; horizonte_glei em posicao nao-Gleissolo)

Solos com horizonte glei
([`horizonte_glei`](https://hugomachadorodrigues.github.io/soilKey/reference/horizonte_glei.md))
em posicao nao diagnostica para Gleissolos (i.e., dentro de
`max_depth_cm` mas NAO satisfazendo os requisitos completos de
Gleissolo). Discrimina os Subgrupos gleissolicos de Argissolos (Cap 5
PA), Cambissolos (Cap 6) e outros.

## Usage

``` r
carater_gleissolico(pedon, max_depth_cm = 150)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_depth_cm:

  Profundidade maxima onde camadas qualificam (default 150).

## Value

[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

Embrapa (2018), SiBCS 5a ed., Cap 5, p 126; Cap 9 (Gleissolos).
