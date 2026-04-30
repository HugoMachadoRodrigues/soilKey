# Carater sombrico (SiBCS Cap 1; Cap 5 PV)

Camada subsuperficial com acumulacao iluvial de materia organica,
caracterizada por cores escuras (`munsell_value_moist` \\\le\\ 4,
`munsell_chroma_moist` \\\le\\ 3) e `oc_pct` \\\ge\\ 0.5%, em B abaixo
de A/E. Distinto de B espodico por nao requerer iluviacao Al/Fe. Tipico
de solos altitudinais (planaltos sul-brasileiros). Discrimina o Subgrupo
sombricos de Argissolos Vermelhos Aluminicos (Cap 5 PV 4.2.6).

## Usage

``` r
carater_sombrico(
  pedon,
  max_value = 4,
  max_chroma = 3,
  min_oc_pct = 0.5,
  max_depth_cm = 150
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_value:

  Default 4 (escuro).

- max_chroma:

  Default 3.

- min_oc_pct:

  Default 0.5%.

- max_depth_cm:

  Default 150.

## Value

[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Implementacao v0.7.4 (aproximacao):

- Camada B (`designation` matches `^B`) com value \\\le\\ max_value E
  chroma \\\le\\ max_chroma E oc_pct \\\ge\\ min_oc_pct, dentro de
  max_depth_cm.

## References

Embrapa (2018), SiBCS 5a ed., Cap 1; Cap 5 PV 4.2.6, p 130 (Lunardi
Neto, 2012, perfil PVa).
