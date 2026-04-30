# Familia: constituicao esqueletica (Cap 1, p 48)

Solo com mais de 35% e menos de 90% do volume constituido por material
mineral com diametro \> 2 cm. Acima de 90%, eh considerado tipo de
terreno (nao classificavel).

## Usage

``` r
familia_constituicao_esqueletica(pedon, max_depth_cm = 200)
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

O schema atual nao distingue cascalho (2 mm-2 cm) de calhaus (\> 2 cm).
Como aproximacao conservadora, esta funcao retorna "esqueletica" quando
`coarse_fragments_pct` esta no intervalo (35%, 90%). Refinamento futuro
requer adicionar uma coluna distinta para fragmentos \> 2 cm.

## References

Embrapa (2018), SiBCS 5a ed., Cap 1, p 48; Cap 18, p 284.
