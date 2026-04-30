# Carater acrico (SiBCS Cap 1, p 31)

Indica solos com balanca de cargas predominante eletropositiva ou
eletricamente neutra. Discrimina Latossolos Acricos / Acriferricos no 3o
nivel (Cap 10).

## Usage

``` r
carater_acrico(pedon, max_ecec_clay = 1.5, min_delta_ph = 0)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_ecec_clay:

  Limite superior de CECef/argila em cmolc/kg argila (default 1.5).

- min_delta_ph:

  Limite inferior de \\\Delta pH\\ (default 0).

## Value

[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md);
`passed = TRUE` se pelo menos um horizonte B satisfaz ambos os
criterios.

## Details

Criterios canonicos (todos verificados em horizontes B):

1\. \\\Delta pH = pH(KCl) - pH(H_2O) \ge 0\\ 2. CECef por kg de argila
\\\le\\ 1.5 cmolc/kg argila

## References

Embrapa (2018), SiBCS 5a ed., Cap 1, p 31; Cap 10 (Latossolos), pp
173-176.
