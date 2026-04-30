# Cerosidade quantitativa (SiBCS Cap 13, p 207; Cap 1)

Diagnostico parametrizado quantidade x intensidade de cerosidade (clay
films / cutans). Consume as colunas v0.7.2 `clay_films_amount` (ordinal:
few/pouca, common/comum, many/abundante, continuous/continua) e
`clay_films_strength` (ordinal: weak/fraca, moderate/moderada,
strong/forte; "shiny" mapeado a "strong"), introduzidas em substituicao
ao legado `clay_films`.

## Usage

``` r
cerosidade(pedon, min_amount = "common", min_strength = "moderate")
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_amount:

  Quantidade minima: `"few"`, `"common"`, `"many"`, `"continuous"` (ou
  equivalentes em PT-BR). Default `"common"`.

- min_strength:

  Intensidade minima: `"weak"`, `"moderate"`, `"strong"`. Default
  `"moderate"`. Pass `NULL` para ignorar a dimensao de intensidade.

## Value

[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md);
`passed = TRUE` se ao menos um horizonte B atende ambos os limiares.

## Details

Discriminante critico Nitossolos vs Argissolos no Cap 13: Nitossolos
exigem cerosidade \\\ge\\ comum + \\\ge\\ moderada (defaults).

## References

Embrapa (2018), SiBCS 5a ed., Cap 13 (Nitossolos), p 207; Cap 1
(atributos diagnosticos).
