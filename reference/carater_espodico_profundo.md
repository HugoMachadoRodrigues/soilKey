# Carater B espodico profundo (SiBCS Cap 8)

Solos com horizonte B espodico cujo `top_cm` esta entre `min_top_cm`
(default 200) e `max_top_cm` (default 400). Discrimina os Grandes Grupos
Hiperespessos / Hidro-hiperespessos de Espodossolos (Cap 8 1.1, 1.3,
2.1, 2.3, 3.1, 3.3).

## Usage

``` r
carater_espodico_profundo(pedon, min_top_cm = 200, max_top_cm = 400)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_top_cm:

  Default 200.

- max_top_cm:

  Default 400.

## Value

[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Implementacao: chama
[`carater_espodico`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_espodico.md)
e filtra por `top_cm` no intervalo \[`min_top_cm`, `max_top_cm`\].

## References

Embrapa (2018), SiBCS 5a ed., Cap 8, pp 165-168.
