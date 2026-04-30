# Carater rubrico (SiBCS Cap 1; Cap 10 Latossolos Brunos)

Solos com matiz Munsell mais vermelho que 5YR (i.e., 2.5YR, 10R, 5R) E
chroma \\\ge\\ 4 em alguma parte do horizonte B (inclusive BA), dentro
de `max_depth_cm` (default 100). Discrimina os Subgrupos rubricos de
Latossolos Brunos (Cap 10 LB).

## Usage

``` r
carater_rubrico(pedon, min_chroma = 4, max_depth_cm = 100)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_chroma:

  Default 4.

- max_depth_cm:

  Default 100.

## Value

[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

Embrapa (2018), SiBCS 5a ed., Cap 1; Cap 10 LB, p 199-200.
