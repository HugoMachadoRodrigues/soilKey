# Subgrupo "mesicos" de Planossolos (B planico topo em \[50, 100\] cm)

Discrimina os Subgrupos mesicos de Planossolos (Cap 15: SNs Mesicos, SNo
Mesicos, SXs Mesicos, SXal Mesicos, SXd Mesicos, SXe Mesicos): B planico
cujo topo ocorre entre `min_top_cm` (inclusivo) e `max_top_cm`
(inclusivo).

## Usage

``` r
subgrupo_planossolo_mesicos(pedon, min_top_cm = 50, max_top_cm = 100)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_top_cm:

  Profundidade minima inclusiva (default 50).

- max_top_cm:

  Profundidade maxima inclusiva (default 100).

## Value

[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

Embrapa (2018), SiBCS 5a ed., Cap 15 (Planossolos).
