# Subgrupo "espessos" de Planossolos (B planico profundo, \> 100 cm)

Discrimina os Subgrupos espessos de Planossolos (Cap 15: SNs Espessos,
SNo Espessos, SXs Espessos, SXal Espessos, SXd Espessos, SXe Espessos):
B planico cujo topo ocorre entre `min_top_cm` (exclusivo) e `max_top_cm`
(inclusivo).

## Usage

``` r
subgrupo_planossolo_espessos(pedon, min_top_cm = 100, max_top_cm = 200)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_top_cm:

  Profundidade minima exclusiva do topo do B planico (default 100; passa
  se top \> 100).

- max_top_cm:

  Profundidade maxima inclusiva (default 200).

## Value

[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Implementacao: identifica B planico via
[`B_planico`](https://hugomachadorodrigues.github.io/soilKey/reference/B_planico.md),
captura o topo (mais raso) das camadas que passam, e testa se cai em
`(min_top_cm, max_top_cm]`.

## References

Embrapa (2018), SiBCS 5a ed., Cap 15 (Planossolos), pp 251-260.
