# Horizonte B espodico (SiBCS Cap 2, p 62-65; v0.7)

Subsuperficial com acumulo iluvial de Al + Fe + materia organica;
espessura \\= 2.5 cm. Tipos: Bs, Bhs, Bh, ortstein. Reuso de
[`spodic`](https://hugomachadorodrigues.github.io/soilKey/reference/spodic.md)
(WRB) que ja codifica criterios essencialmente identicos.

## Usage

``` r
B_espodico(pedon, ...)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- ...:

  Reserved for future arguments.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
recording whether the diagnostic is present, the qualifying layers, and
the supporting evidence.
