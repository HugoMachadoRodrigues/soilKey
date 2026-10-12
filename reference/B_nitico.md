# Horizonte B nitico (SiBCS Cap 2, p 61-62; v0.7)

Subsuperficial nao hidromorfico, textura argilosa/muito argilosa (clay
\\= 35% desde a superficie), com pequeno incremento de argila (B/A \\=
1.5), estrutura em blocos sub/angulares ou prismatica grau
moderado/forte, cerosidade no minimo comum + moderada, espessura \\= 30
cm. Argila ativ baixa OR ativ alta + carater aluminico.

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

  Terms of the amount of cerosidade that satisfy criterion (c). The
  lowest of them on the scale none \< few \< common \< many sets the
  threshold (the default is "common", the "comum" of SiBCS), and an
  amount of that class or above passes, in English or Portuguese. A term
  the scale does not know passes only if it is literally listed.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
recording whether the diagnostic is present, the qualifying layers, and
the supporting evidence.

## Details

The amount of cerosidade (`clay_films_amount`) and the type of structure
(`structure_type`) are read as the field sheet words them, in English or
in Portuguese, whatever the case and the accents: "Comum", "Abundante"
and "Prismatica que se desfaz" (written with its accent) of the
Brazilian surveys count as "common", "many" and "prismatic" do. Until
v0.9.226 the amount was compared with the literal English terms of
`min_cerosidade`, so no profile described in Portuguese could have a B
nitico.

The criteria of the B nitico are minima ("cerosidade em quantidade no
minimo comum e com grau forte ou moderado", "estrutura ... de grau de
desenvolvimento moderado ou forte"), and a range meets a minimum only if
all of it does: an amount or a grade written as a range is read by its
lower end. "Pouca a comum" is below "comum" and, since v0.9.231, "Fraca
a moderada" is below "moderada" for the cerosidade and for the
structure.
