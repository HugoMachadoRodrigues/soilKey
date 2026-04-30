# Carater espesso-humico (SiBCS Cap 5, p 119)

Solos com horizonte A humico e conteudo de carbono \>= `min_oc_pct`
(default 1% = 10 g/kg) extendendo-se ate `min_depth_cm` (default 80 cm)
ou mais de profundidade. Discrimina os Subgrupos "espesso-humicos" de
Argissolos Bruno-Acinzentados Ta Aluminicos (Cap 5 PBAC 1.1.2) – camadas
humosas espessas tipicas de Argissolos do RS.

## Usage

``` r
carater_humico_espesso(pedon, min_oc_pct = 1, min_depth_cm = 80)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_oc_pct:

  Limite inferior de OC% nas camadas inferiores (default 1.0 = 10 g/kg).

- min_depth_cm:

  Profundidade minima de extensao do C alto (default 80 cm).

## Value

[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Implementacao: requer (1)
[`horizonte_A_humico`](https://hugomachadorodrigues.github.io/soilKey/reference/horizonte_A_humico.md)
passa AND (2) ha camada com `oc_pct` \>= `min_oc_pct` cuja `bottom_cm`
\>= `min_depth_cm`.

## References

Embrapa (2018), SiBCS 5a ed., Cap 5 (Argissolos), p 119.
