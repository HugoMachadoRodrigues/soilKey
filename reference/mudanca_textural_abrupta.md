# Mudanca textural abrupta (SiBCS Cap 1, p 30-31)

"Consiste em um consideravel aumento no teor de argila em pequena
distancia na zona de transicao entre o horizonte A ou E e o horizonte
subjacente B":

- A or E with less than 200 g/kg of clay: the B has at least twice as
  much;

- A or E with 200 g/kg or more: the B has 200 g/kg more or above ("de
  300 para no minimo 500, ou de 220 para no minimo 420"),

"em uma distancia vertical \<= 7,5 cm".

## Usage

``` r
mudanca_textural_abrupta(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
recording whether the diagnostic is present, the qualifying layers, and
the supporting evidence.

## Details

Since v0.9.244 the test is that of the book: the first B horizon is
compared with the horizon immediately above it, which has to be an A or
an E; and the distance is read from the distinctness of the transition
(`boundary_distinctness` of the horizon above: abrupt, under 2.5 cm, or
clear, 2.5 to 7.5 cm). Where it is recorded as gradual or diffuse there
is no abrupt change; where it is not recorded the two horizons are
compared. The layer returned is the first B horizon.

Until v0.9.243 the function was the WRB
[`abrupt_textural_difference`](https://hugomachadorodrigues.github.io/soilKey/reference/abrupt_textural_difference.md),
which compares every pair of adjoining layers, in the order of the rows:
a jump of clay between two B horizons, or from a B to a C, was an abrupt
change. A profile with no horizon designation at all is still tested
that way.
