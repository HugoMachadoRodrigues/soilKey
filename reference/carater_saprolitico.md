# Carater saprolitico (SiBCS Cap 5)

Solos com horizonte Cr (brando) e ausencia de contato litico ou litico
fragmentario, todos dentro de `max_depth_cm` (default 100 cm).
Discrimina os Subgrupos saproliticos de Argissolos (Cap 5: PA, PV).

## Usage

``` r
carater_saprolitico(pedon, max_depth_cm = 100)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_depth_cm:

  Default 100.

## Value

[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Implementacao: requer (a) designation pattern `Cr`/`Crf` (sem `R`
continuo) em camada com `top < max_depth_cm`, e (b)
[`contato_litico`](https://hugomachadorodrigues.github.io/soilKey/reference/contato_litico.md)`(pedon)`
retorna FALSE.

## References

Embrapa (2018), SiBCS 5a ed., Cap 5, pp 122, 132.
