# Carater leptico (SiBCS Cap 5; contato litico em 50-100 cm)

Solos com contato litico
([`contato_litico`](https://hugomachadorodrigues.github.io/soilKey/reference/contato_litico.md))
a profundidade entre 50 e 100 cm. Discrimina os Subgrupos lepticos de
Argissolos (Cap 5: PA, PV, PVA).

## Usage

``` r
carater_leptico(pedon, min_depth_cm = 50, max_depth_cm = 100)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_depth_cm:

  Default 50.

- max_depth_cm:

  Default 100.

## Value

[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Implementacao: chama `contato_litico(pedon)` sem bound, depois filtra
layers para top em \[`min_depth_cm`, `max_depth_cm`\].

## References

Embrapa (2018), SiBCS 5a ed., Cap 5, pp 127, 132.
