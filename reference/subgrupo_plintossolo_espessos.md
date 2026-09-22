# Subgrupo "espessos" de Plintossolos (horizonte plintico topo \> 100 cm)

Discrimina os Subgrupos espessos de Plintossolos Argiluvicos (FT\*Es) e
Haplicos (FXacEs, FXdEs, FXeEs): horizonte plintico cujo topo ocorre
entre `min_top_cm` (exclusivo) e `max_top_cm` (inclusivo).

## Usage

``` r
subgrupo_plintossolo_espessos(pedon, min_top_cm = 100, max_top_cm = 200)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_top_cm:

  Profundidade minima exclusiva (default 100).

- max_top_cm:

  Profundidade maxima inclusiva (default 200).

## Value

[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

Embrapa (2018), SiBCS 5a ed., Cap 16 (Plintossolos), pp 261-272.
