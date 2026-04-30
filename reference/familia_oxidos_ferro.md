# Familia: teor de oxidos de ferro (Cap 1, p 42)

Classifica pelo teor de Fe2O3 (g/kg de solo, equivalente a
fe2o3_sulfuric_pct \* 10) na maior parte do horizonte B:

- `hipoferrico`: \< 80 g/kg (= \< 8%)

- `mesoferrico`: 80 - 180 g/kg (\[8%, 18%))

- `ferrico`: 180 - 360 g/kg (\[18%, 36%))

- `perferrico`: \>= 360 g/kg (\>= 36%)

## Usage

``` r
familia_oxidos_ferro(pedon, max_depth_cm = 150)
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

Aplicavel a Argissolos, Cambissolos, Chernossolos, Latossolos, Neossolos
Litolicos, Neossolos Regoliticos, Nitossolos e Plintossolos. Quando o
atributo ja foi considerado em nivel categorico mais alto (e.g.
Latossolos Eutroferricos / Distroferricos / Acriferricos), o motor de
Familia pula esta dimensao.

## References

Embrapa (2018), SiBCS 5a ed., Cap 1, p 42.
