# Familia: material subjacente em Organossolos (Cap 18, p 287)

Identifica a textura da primeira camada nao-organica abaixo das camadas
organicas, na secao de controle. Retorna o grupamento textural daquele
material como adjetivo (e.g. "arenoso", "argiloso").

## Usage

``` r
familia_organossolo_material_subjacente(pedon, max_depth_cm = 200)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_depth_cm:

  Profundidade da secao de controle (default 200).

## Value

[`FamilyAttribute`](https://hugomachadorodrigues.github.io/soilKey/reference/FamilyAttribute.md).

## References

Embrapa (2018), SiBCS 5a ed., Cap 18, p 287.
