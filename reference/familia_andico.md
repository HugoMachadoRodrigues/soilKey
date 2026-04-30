# Familia: propriedades andicas (Cap 1, p 42-43)

Aplica o termo "andico" quando, em qualquer horizonte:

- densidade do solo \<= 0,9 g/cm3, E

- retencao de fosfato \>= 85%, E

- Alo + 0.5 \* Feo \>= 2% (oxalato extraivel)

## Usage

``` r
familia_andico(pedon, max_db = 0.9, min_pret = 85, min_aloxfeox = 2)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_db:

  Densidade maxima (default 0.9 g/cm3).

- min_pret:

  Retencao minima de fosfato (default 85%).

- min_aloxfeox:

  Limite de Alo + 0.5\*Feo (default 2%).

## Value

[`FamilyAttribute`](https://hugomachadorodrigues.github.io/soilKey/reference/FamilyAttribute.md)
com `value` = `"andico"` ou NULL.

## Details

Aplicavel para Cambissolos Histicos e Organossolos Folicos (Cap 18 p
287), em fase de validacao.

## References

Embrapa (2018), SiBCS 5a ed., Cap 1, p 42-43; Cap 18, p 287.
