# Familia: grupamento textural (Cap 1, p 46)

Retorna o grupamento textural do solo na secao de controle. Classes (em
g kg-1):

- arenosa: areia + areia franca, i.e. (sand_pct - clay_pct) \> 70

- media: clay \< 35 e sand \> 15, exceto arenosa

- argilosa: clay entre 35 e 60

- muito_argilosa: clay \> 60

- siltosa: clay \< 35 e sand \< 15

## Usage

``` r
familia_grupamento_textural(pedon, max_depth_cm = 200)
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

Aplicavel a todas as ordens do SiBCS, exceto Neossolos Quartzarenicos
(RQ), nas quais o subgrupamento eh mais apropriado.

## References

Embrapa (2018), SiBCS 5a ed., Cap 1, p. 46-47; Cap 18, p. 281.
