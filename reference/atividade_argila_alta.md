# Atividade da fracao argila (SiBCS Cap 1, p 30)

Calcula a atividade da fracao argila Ta = CEC \* 1000 / argila (em
cmolc/kg de argila, sem correcao para carbono) por horizonte e
classifica como \*\*alta (Ta)\*\* se \>= 27 cmolc/kg argila ou \*\*baixa
(Tb)\*\* se \< 27. Nao se aplica a texturas areia / areia franca.

## Usage

``` r
atividade_argila_alta(pedon, min_ta = 27)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_ta:

  Numeric threshold or option (see Details).

## Value

Um
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md);
`passed = TRUE` sse pelo menos um horizonte B ou C tem Ta. `layers` =
horizontes com atividade alta (Ta).

## Details

Para distincao de classes pelo SiBCS, considera-se a atividade no
horizonte B (incl. BA, exc. BC) ou no horizonte C (incl. CA), quando nao
existe B.

## References

Embrapa (2018), SiBCS 5a ed., Cap 1, "Atividade da fracao argila", p.
30.
