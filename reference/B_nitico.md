# Horizonte B nitico (SiBCS Cap 2, p 61-62; v0.7)

Subsuperficial nao hidromorfico, textura argilosa/muito argilosa (clay
\\= 35% desde a superficie), com pequeno incremento de argila (B/A \\=
1.5), estrutura em blocos sub/angulares ou prismatica grau
moderado/forte, cerosidade no minimo comum + moderada, espessura \\= 30
cm. Argila ativ baixa OR ativ alta + carater alumínico.

## Usage

``` r
B_nitico(
  pedon,
  min_thickness = 30,
  min_clay_pct = 35,
  max_b_a_ratio = 1.5,
  min_cerosidade = c("common", "many", "abundant", "strong")
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Numeric threshold or option (see Details).

- min_clay_pct:

  Numeric threshold or option (see Details).

- max_b_a_ratio:

  Numeric threshold or option (see Details).

- min_cerosidade:

  Numeric threshold or option (see Details).
