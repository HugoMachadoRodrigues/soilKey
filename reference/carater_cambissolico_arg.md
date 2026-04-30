# Carater cambissolico (Argissolos – Cap 5)

Solos com 4% ou mais de minerais alteraveis visiveis E/OU 5% ou mais de
fragmentos de rocha (`coarse_fragments_pct`) no horizonte B (exclusive
BC ou B/C), dentro de `max_depth_cm`. Discrimina os Subgrupos
cambissolicos de Argissolos PA (Cap 5, p 126) – DISTINTO do
[`carater_cambissolico`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_cambissolico.md)
(Cap 14 Organossolos Folicos: B incipiente abaixo de histico/A).

## Usage

``` r
carater_cambissolico_arg(pedon, min_coarse_pct = 5, max_depth_cm = 150)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_coarse_pct:

  Default 5% volume.

- max_depth_cm:

  Default 150 cm.

## Value

[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Implementacao v0.7.4 (aproximacao): apenas `coarse_fragments_pct`
\\\ge\\ `min_coarse_pct` (default 5) eh testado. O criterio "minerais
alteraveis visiveis" exigiria campo adicional no schema (e.g.
`weatherable_minerals_pct`) que sera adicionado em release futura.
Documentado como limitacao conhecida.

## References

Embrapa (2018), SiBCS 5a ed., Cap 5, p 126.
