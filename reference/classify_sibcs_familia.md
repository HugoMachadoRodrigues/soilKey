# Classifica um perfil no 5o nivel categorico do SiBCS (Familia)

Aplica as dimensoes pertinentes a ordem do solo e devolve uma lista
nomeada de
[`FamilyAttribute`](https://hugomachadorodrigues.github.io/soilKey/reference/FamilyAttribute.md).
O label textual da Familia eh formado adicionando-se cada `value`
nao-nulo apos a designacao do 4o nivel, separados por virgulas (Cap 18,
p 281).

## Usage

``` r
classify_sibcs_familia(
  pedon,
  ordem_code = NULL,
  sg_code = NULL,
  max_depth_cm = 200
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- ordem_code:

  Codigo da ordem (1 letra: "P", "L", ...). Se `NULL`, sera derivado de
  `sg_code`.

- sg_code:

  Codigo do subgrupo do 4o nivel (e.g. "PVdAr"). Opcional; usado para
  ajustes especificos por SG (e.g. forcar subgrupamento textural em
  arenicos/espessarenicos).

- max_depth_cm:

  Profundidade da secao de controle (default 200 cm).

## Value

Lista nomeada de
[`FamilyAttribute`](https://hugomachadorodrigues.github.io/soilKey/reference/FamilyAttribute.md).

## Details

Esta funcao NAO eh uma chave determinista: cada perfil recebe
SIMULTANEAMENTE todos os adjetivos pertinentes (multi-rotulo).

## Status v0.7.14.A

Implementadas 5 dimensoes – grupamento textural, subgrupamento textural,
distribuicao de cascalhos, constituicao esqueletica, tipo de horizonte
superficial. Outras dimensoes (prefixos epi/ meso/endo, saturacao de
bases, alico, mineralogia, atividade da argila, oxidos de ferro, andico,
especificos de Organossolos) adicionadas em sub-commits subsequentes.

## References

Embrapa (2018), SiBCS 5a ed., Cap 18, pp 281-288.
