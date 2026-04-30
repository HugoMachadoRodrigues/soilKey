# Carater cambissolico (SiBCS Cap 14)

Solos com B incipiente
([`B_incipiente`](https://hugomachadorodrigues.github.io/soilKey/reference/B_incipiente.md))
abaixo do horizonte hístico (H/O) ou A. Discrimina os Subgrupos
cambissolicos de Organossolos Folicos (Cap 14, pp 247-248): Folicos
Fibricos / Hemicos / Sapricos cambissolicos.

## Usage

``` r
carater_cambissolico(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Implementado como uma interseccao de duas condicoes:

1.  `B_incipiente` passa em ao menos um horizonte

2.  Esse horizonte B incipiente esta abaixo de um horizonte H/O
    (hístico) ou A

Em pedons sem H/O ou A acima do B incipiente, o teste falha (B
incipiente isolado nao caracteriza Organossolo Cambissolico).

## References

Embrapa (2018), SiBCS 5a ed., Cap 14, pp 247-248.
