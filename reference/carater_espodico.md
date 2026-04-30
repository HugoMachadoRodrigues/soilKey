# Carater espodico (SiBCS Cap 1, p 35; Cap 8)

Evidencia iluvial de Al / Fe / materia organica em camada de pelo menos
2,5 cm de espessura, em quantidade insuficiente para qualificar como
horizonte B espodico
([`B_espodico`](https://hugomachadorodrigues.github.io/soilKey/reference/B_espodico.md)),
mas suficiente para indicar espodicidade incipiente. Usado em
Cambissolos / Argissolos / Plintossolos espodicos (Caps 5, 6 e 16) e em
Espodossolos rasos (Cap 8).

## Usage

``` r
carater_espodico(pedon, min_thickness = 2.5, min_oc_pct = 0.5)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Espessura minima da camada espodica incipiente em cm (default 2,5).

- min_oc_pct:

  OC% minimo em camada candidata (default 0,5).

## Value

[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Diferenca para
[`B_espodico`](https://hugomachadorodrigues.github.io/soilKey/reference/B_espodico.md):
thickness \>= 2,5 cm em vez de exigir o gate completo de espessura
espodica; OC \>= 0,5% em vez do gate de iluviacao quantitativa; sinais
de iluviacao Fe/Al (`al_ox_pct` ou `fe_ox_pct` ou `fe_dcb_pct`).

## References

Embrapa (2018), SiBCS 5a ed., Cap 1, p 35; Cap 8 (Espodossolos), pp
156-160.
