# Carater coeso (SiBCS Cap 1, pp 32-33)

Solos com horizontes coesos: muito duros a extremamente duros quando
secos, friaveis a firmes quando umidos, decorrentes do empacotamento das
particulas e/ou cimentacao. Discrimina os Grandes Grupos Distrocoesos /
Eutrocoesos de Argissolos (Cap 5, pp 117-119) e Latossolos (Cap 10).

## Usage

``` r
carater_coeso(pedon, max_depth_cm = 150)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_depth_cm:

  Profundidade maxima onde camadas qualificam (default `150`, conforme
  SiBCS Cap 5: "dentro de 150 cm a partir da superficie").

## Value

[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md);
`passed = TRUE` se ao menos uma camada (com textura suficiente) atende
aos dois criterios de consistencia.

## Details

Criterios canonicos:

- `rupture_resistance` \\\in\\{"very hard", "extremely hard"} (em estado
  seco)

- `consistence_moist` \\\in\\{"friable", "firm"} (em estado umido)

- Excluido: textura areia / areia franca (`clay_pct` \< 15%)

## References

Embrapa (2018), SiBCS 5a ed., Cap 1, pp 32-33; Cap 5 (Argissolos), pp
117-119.
