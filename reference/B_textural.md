# Horizonte B textural (SiBCS Cap 2, p 54-57; v0.7 strict)

Horizonte mineral subsuperficial com incremento de argila + cerosidade
OR aumento gradativo, satisfazendo criterios de espessura e relacao
textural B/A. v0.7 enforce as alternativas (a)-(j) do SiBCS por
delegacao parcial ao WRB
[`argic`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md)
(criterios de clay-increase essencialmente identicos) acrescidos de:

- espessura \\= 7.5 cm OR \\= 10% da soma das espessuras dos
  sobrejacentes; e

- textura \\= francoarenosa.

Refinamentos pendentes para v0.8: cerosidade obrigatoria sob certas
estruturas (criterio i.1 / i.2 / i.3); lamelas \\= 15 cm combinadas.

## Usage

``` r
B_textural(pedon, ...)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- ...:

  Reserved for future arguments.
