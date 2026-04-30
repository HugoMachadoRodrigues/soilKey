# Carater durico (SiBCS Cap 1)

Solos com endurecimento por cimentacao parcial de silica (SiO2),
insuficiente para qualificar como horizonte durico
([`duripa`](https://hugomachadorodrigues.github.io/soilKey/reference/duripa.md))
completo. Detectado quando:

## Usage

``` r
carater_durico(pedon, max_depth_cm = 150)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_depth_cm:

  Profundidade maxima onde camadas qualificam (default 150, conforme
  SiBCS Cap 5: "dentro de 150 cm").

## Value

[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

- `duripan_pct` \> 0 (presenca de noduros / concrecoes de silica), OR

- `cementation_class` \\\in\\{"weakly", "moderately"} (cimentacao fraca
  a moderada, NAO indurada/strongly).

Discrimina os Subgrupos duricos / abrupticos duricos de Argissolos
Acinzentados (Cap 5 PAC) e Latossolos com caracter durico (Cap 10).

## References

Embrapa (2018), SiBCS 5a ed., Cap 1; Cap 5 (Argissolos Acinzentados
Distrocoesos abrupticos duricos), p 120.
