# Carater tionico (SiBCS Cap 9; Cap 1 thionic-related)

Solos com horizonte sulfurico
([`horizonte_sulfurico`](https://hugomachadorodrigues.github.io/soilKey/reference/horizonte_sulfurico.md))
OU materiais sulfidricos a profundidades entre `min_depth_cm` e
`max_depth_cm` (default 100-150 cm). Discrimina os Subgrupos tionicos de
Gleissolos (Cap 9 GZsd, GMtal, GMtd, GXte) – variante "tionico
subordinado" (vs Tiomorfico, que e a subordem completa).

## Usage

``` r
carater_tionico(pedon, min_depth_cm = 100, max_depth_cm = 150)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_depth_cm:

  Default 100 cm.

- max_depth_cm:

  Default 150 cm.

## Value

[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

Embrapa (2018), SiBCS 5a ed., Cap 9, pp 180-191.
