# Familia: tipo de horizonte diagnostico superficial (Cap 2)

Retorna o tipo do horizonte A (ou H/O) presente, em ordem de
precedencia: `histico` \> `chernozemico` \> `humico` \> `proeminente` \>
`moderado` \> `fraco`. Se nenhum diagnostico passa, retorna NULL.

## Usage

``` r
familia_tipo_horizonte_superficial(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

[`FamilyAttribute`](https://hugomachadorodrigues.github.io/soilKey/reference/FamilyAttribute.md).

## Details

Aplica-se a TODAS as classes de solo, exceto para aquelas que ja
consideram o tipo de A em nivel categorico mais alto (e.g. Chernossolos,
Organossolos, Neossolos Litolicos Humicos / Histicos).

## References

Embrapa (2018), SiBCS 5a ed., Cap 2 (p 49-54); Cap 18, p 284.
