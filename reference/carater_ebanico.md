# Carater ebanico (SiBCS Cap 1; Cap 7 e Cap 17)

Cor preta uniforme (value \\\le\\ 3 e chroma \\\le\\ 2 em umido) em TODO
o horizonte B + atividade da argila alta (Ta) + saturacao por bases V%
\\\ge\\ 65. Discrimina Chernossolos Ebanicos (Cap 7) e Vertissolos
Ebanicos (Cap 17) no 2o nivel.

## Usage

``` r
carater_ebanico(pedon, max_value = 3, max_chroma = 2, min_v = 65)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_value:

  Limite superior de Munsell value em umido (default 3).

- max_chroma:

  Limite superior de chroma em umido (default 2).

- min_v:

  Limite inferior de V% (default 65).

## Value

[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

Embrapa (2018), SiBCS 5a ed., Cap 1; Cap 7 (Chernossolos), pp 144-148;
Cap 17 (Vertissolos), pp 271-274.
