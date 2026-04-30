# Familia: distribuicao de cascalhos no perfil (Cap 1, p 47-48)

Utiliza coarse_fragments_pct (% volume de cascalhos 2 mm a 2 cm relativo
a terra fina) como modificador do grupamento textural.

## Usage

``` r
familia_distribuicao_cascalhos(pedon, max_depth_cm = 200)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_depth_cm:

  Profundidade da secao de controle (default 200).

## Value

[`FamilyAttribute`](https://hugomachadorodrigues.github.io/soilKey/reference/FamilyAttribute.md).

## Details

Classes (Santos et al., 2015; valores em g kg-1):

- pouco_cascalhenta: 8% \<= cascalho \< 15%

- cascalhenta: 15% \<= cascalho \<= 50%

- muito_cascalhenta: cascalho \> 50%

Aplica-se a TODAS as classes que apresentam cascalho \> 80 g/kg (8% do
volume) na secao de controle.

## References

Embrapa (2018), SiBCS 5a ed., Cap 1, p 47-48; Cap 18, p 284.
