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

## Details

In the morphological mode the horizon is also read from its designation.
Since v0.9.247 that includes the "ir" of the older surveys (`Bir`,
`B2ir`, `B22irx`), the illuvial iron that the present notation writes
"s": the mode read "h" and "s" only.
