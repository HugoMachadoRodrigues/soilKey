# Resolve a subordem de um pedon ja classificado em uma ordem SiBCS

Itera as subordens da ordem em ordem canonica via o engine generico
[`run_taxa_list`](https://hugomachadorodrigues.github.io/soilKey/reference/run_taxa_list.md);
a primeira cuja test-block passa captura o perfil. Se nenhuma passar,
retorna a ultima subordem (catch-all `tests:{default:true}`).

## Usage

``` r
run_sibcs_subordem(pedon, ordem_code, rules = NULL)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- ordem_code:

  Codigo de uma letra da ordem (e.g. "L" para Latossolos).

- rules:

  Lista de regras carregada via
  [`load_rules`](https://hugomachadorodrigues.github.io/soilKey/reference/load_rules.md).

## Value

Lista com `assigned` (entrada YAML da subordem ou `NULL` se a ordem nao
tiver bloco) e `trace`.
