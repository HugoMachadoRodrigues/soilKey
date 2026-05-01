# Familia: mineralogia da fracao argila (geral, nao-Latossolos)

Classifica a mineralogia da argila para Argissolos, Cambissolos,
Plintossolos, Luvissolos, Nitossolos, Vertissolos, Chernossolos,
Planossolos, Gleissolos quando ha informacao quantitativa de atividade
da argila e/ou Ki/Kr. Cobre as classes nao endereçadas por
[`familia_mineralogia_argila_latossolo`](https://hugomachadorodrigues.github.io/soilKey/reference/familia_mineralogia_argila_latossolo.md):

- `esmectitica`: T_argila \>= `ta_threshold` (default 27 cmolc/kg
  argila), indicando dominancia de argilas 2:1 expansivas (esmectita /
  vermiculita / micas hidratadas).

- `caulinitica`: Ki \>= `ki_caulinitico_min` (default 0.75) e Kr \>=
  `kr_caulinitico_min` (default 0.75), alem de T_argila \<
  `ta_threshold`.

- `oxidica`: Kr \< `kr_caulinitico_min`, indicando predominancia de
  oxihidrooxidos de Fe e Al.

- `mista`: nenhum dos outros gates fechou conclusivamente – evidencia
  heterogenea ou incompleta.

Quando os tres atributos (T_argila, Ki, Kr) estiverem ausentes, o
resultado fica `NULL` e os atributos faltantes sao reportados.

## Usage

``` r
familia_mineralogia_argila_geral(
  pedon,
  max_depth_cm = 200,
  ta_threshold = 27,
  ki_caulinitico_min = 0.75,
  kr_caulinitico_min = 0.75
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_depth_cm:

  Profundidade da secao de controle (default 200).

- ta_threshold:

  Limite cmolc/kg argila para esmectitica (default 27).

- ki_caulinitico_min:

  Limite Ki para caulinitica (default 0.75).

- kr_caulinitico_min:

  Limite Kr para caulinitica vs oxidica (default 0.75).

## Value

[`FamilyAttribute`](https://hugomachadorodrigues.github.io/soilKey/reference/FamilyAttribute.md).

## References

Embrapa (2018), SiBCS 5a ed., Cap 18, p 286-287.
