# Familia: espessura \> 100 cm de material organico em Organossolos (Cap 18, p 287)

Retorna `"espesso"` quando a soma das espessuras de camadas organicas a
partir da superficie excede 100 cm (Cap 18 p 287: "Organossolos com mais
de 100 cm de material organico a partir da sua superficie").

## Usage

``` r
familia_organossolo_espessura(pedon, min_cm = 100)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_cm:

  Default 100 cm.

## Value

[`FamilyAttribute`](https://hugomachadorodrigues.github.io/soilKey/reference/FamilyAttribute.md).

## References

Embrapa (2018), SiBCS 5a ed., Cap 18, p 287.
