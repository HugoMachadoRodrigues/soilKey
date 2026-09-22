# Roda a chave SiBCS 5a edicao sobre um pedon

Roda a chave SiBCS 5a edicao sobre um pedon

## Usage

``` r
run_sibcs_key(pedon, rules = NULL)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- rules:

  Conjunto de regras pre-carregado; se NULL, le
  `inst/rules/sibcs5/key.yaml`.

## Value

Lista com `assigned` (entrada YAML da ordem atribuida) e `trace`.
