# Carater retratil (SiBCS Cap 1, p 33)

Solos com retracao significativa quando secos: COLE \\\ge\\ 0,06 sobre a
secao de controle, OU presenca de slickensides + fendas (cracks)
suficientemente desenvolvidas. Discrimina Cambissolos retrateis (Cap 6),
Vertissolos (Cap 17) e Argissolos retrateis (Cap 5).

## Usage

``` r
carater_retratil(pedon, min_cole = 0.06, min_crack_width = 1)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_cole:

  Limite inferior de COLE (default 0,06).

- min_crack_width:

  Largura minima de fenda em cm para o caminho slickensides+cracks
  (default 1).

## Value

[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

Embrapa (2018), SiBCS 5a ed., Cap 1, p 33.
