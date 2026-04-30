# Carater arenico (SiBCS Cap 5)

Solos com textura arenosa (clay% \< `max_clay_pct`, default 15%) desde a
superficie ate uma profundidade entre `min_depth_cm` e `max_depth_cm`
(default 50-100 cm). Discrimina os Subgrupos arenicos de Argissolos (Cap
5: PAC, PA, PV, PVA) e Neossolos (Cap 12).

## Usage

``` r
carater_arenico(
  pedon,
  max_clay_pct = 15,
  min_depth_cm = 50,
  max_depth_cm = 100
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_clay_pct:

  Limite superior de % argila para "arenoso" (default 15 = areia / areia
  franca).

- min_depth_cm:

  Profundidade minima do boundary (default 50).

- max_depth_cm:

  Profundidade maxima do boundary (default 100).

## Value

[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Implementacao: ordena horizontes por `top_cm`, identifica o PRIMEIRO
horizonte com `clay_pct >= max_clay_pct`, e verifica que (a) todos os
horizontes acima desse boundary sao arenosos (sem camada argilosa
intercalada acima) e (b) o boundary (`top_cm`) cai no intervalo
`[min_depth_cm, max_depth_cm]`.

Para "espessarenicos" (boundary 100-200 cm), use `carater_espessarenico`
(planejado v0.7.4.B.3).

## References

Embrapa (2018), SiBCS 5a ed., Cap 5 (Argissolos), pp 120-138.
