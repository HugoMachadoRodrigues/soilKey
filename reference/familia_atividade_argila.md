# Familia: subgrupamento de atividade da fracao argila (Cap 18, p 287)

Classifica pela CTC da fracao argila T = (cec_cmol \* 100 / clay_pct):

- `Tmb`: T \< 8 cmolc/kg argila (muito baixa)

- `Tmob`: 8 \<= T \< 17 (moderadamente baixa)

- `Tm`: 17 \<= T \< 27 (media)

- `Tmoa`: 27 \<= T \< 40 (moderadamente alta)

- `Tma`: T \>= 40 (muito alta)

## Usage

``` r
familia_atividade_argila(pedon, max_depth_cm = 150)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_depth_cm:

  Profundidade da secao de controle (default 150).

## Value

[`FamilyAttribute`](https://hugomachadorodrigues.github.io/soilKey/reference/FamilyAttribute.md).

## Details

Considerada na maior parte do horizonte B (ou C, na ausencia de B). Nao
aplicavel a solos de classe textural areia ou areia franca (clay \< 15 g
kg-1 = 1,5%).

## References

Embrapa (2018), SiBCS 5a ed., Cap 18, p 287.
