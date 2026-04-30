# Carater placico (SiBCS Cap 5; horizonte placico cementado por Fe/Mn)

Camada cimentada por Fe/Mn (geralmente fina, 1-25 mm), detectada via
`cementation_class %in% {"strongly", "indurated"}` dentro de
`max_depth_cm`. Discrimina os Subgrupos placicos de Argissolos PA (Cap
5).

## Usage

``` r
carater_placico(pedon, max_depth_cm = 150)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_depth_cm:

  Default 150 cm.

## Value

[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Implementacao v0.7.4 (aproximacao): `cementation_class` forte ou
indurada. SiBCS estrito requeria espessura minima e composicao Fe/Mn
confirmada. Refinamento planejado para v0.8.

## References

Embrapa (2018), SiBCS 5a ed., Cap 5, p 125.
