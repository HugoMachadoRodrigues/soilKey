# Carater argiluvico (SiBCS Cap 1; Cap 6)

Solos com B textural
([`B_textural`](https://hugomachadorodrigues.github.io/soilKey/reference/B_textural.md))
em posicao NAO diagnostica para Argissolos, dentro de `max_depth_cm`.
Discrimina os Subgrupos argissolicos de Cambissolos (Cap 6 CX 4.7.8,
4.10.5).

## Usage

``` r
carater_argiluvico(pedon, max_depth_cm = 150)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_depth_cm:

  Default 150 cm.

## Value

[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Implementacao v0.7.5: requer
[`B_textural`](https://hugomachadorodrigues.github.io/soilKey/reference/B_textural.md)
passa em alguma camada com `top_cm` \\\<\\ `max_depth_cm`. Distingue-se
de Argissolo pleno por contexto: chamado dentro de Cambissolos onde B
incipiente
([`B_incipiente`](https://hugomachadorodrigues.github.io/soilKey/reference/B_incipiente.md))
ja definiu a ordem como Cambissolo.

## References

Embrapa (2018), SiBCS 5a ed., Cap 1; Cap 6, p 153.
