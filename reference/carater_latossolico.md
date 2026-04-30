# Carater latossolico (SiBCS Cap 5)

Solos com horizonte B latossolico
([`B_latossolico`](https://hugomachadorodrigues.github.io/soilKey/reference/B_latossolico.md))
abaixo do horizonte B textural
([`B_textural`](https://hugomachadorodrigues.github.io/soilKey/reference/B_textural.md)),
dentro de `max_depth_cm` (default 150 cm). Discrimina os Subgrupos
latossolicos de Argissolos (Cap 5: PAC, PA, PV, PVA) – transicao entre
Argissolo e Latossolo dentro do mesmo perfil.

## Usage

``` r
carater_latossolico(pedon, max_depth_cm = 150)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_depth_cm:

  Profundidade maxima do B latossolico (default 150).

## Value

[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Implementacao: requer (1)
[`B_textural()`](https://hugomachadorodrigues.github.io/soilKey/reference/B_textural.md)
passa, (2)
[`B_latossolico()`](https://hugomachadorodrigues.github.io/soilKey/reference/B_latossolico.md)
passa, e (3) ao menos uma camada com B latossolico tem `top_cm` maior
que o `top_cm` maximo das camadas com B textural (i.e., latossolico
ocorre abaixo).

## References

Embrapa (2018), SiBCS 5a ed., Cap 5 (Argissolos), pp 121-138.
