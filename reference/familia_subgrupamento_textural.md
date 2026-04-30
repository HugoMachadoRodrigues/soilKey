# Familia: subgrupamento textural (Cap 18, p 283; em validacao)

Subgrupamento textural mais detalhado, aplicavel em substituicao ao
grupamento para Espodossolos, Latossolos psamiticos, Neossolos Fluvicos
Psamiticos, Neossolos Regoliticos, Neossolos Quartzarenicos, e SGs
arenicos / espessarenicos de Argissolos / Luvissolos / Planossolos /
Plintossolos. Tambem em solos com textura arenosa e/ou media.

## Usage

``` r
familia_subgrupamento_textural(pedon, max_depth_cm = 200)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_depth_cm:

  Profundidade da secao de controle (default 200 cm).

## Value

[`FamilyAttribute`](https://hugomachadorodrigues.github.io/soilKey/reference/FamilyAttribute.md).

## Details

Classes (em g kg-1; referidas a media ponderada da secao de controle):

- muito_arenosa: classe textural areia (sand \>= 85)

- arenosa-media: classe textural areia franca (sand \>= 70 e \<= 91;
  clay \<= 15)

- media-arenosa: francoarenosa, sand \> 52

- media-argilosa: franco-argiloarenosa (clay 20-35, sand \>= 45)

- media-siltosa: clay \< 35 e sand \> 15, excluindo as 4 classes acima

- siltosa: clay \< 35 e sand \< 15

- argilosa: clay 35-60

- muito_argilosa: clay \> 60

## References

Embrapa (2018), SiBCS 5a ed., Cap 18, p. 283.
