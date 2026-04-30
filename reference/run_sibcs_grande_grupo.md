# Resolve o grande grupo (3o nivel) de um pedon classificado em uma subordem SiBCS

v0.7.3: itera os Grandes Grupos da subordem em ordem canonica via o
engine generico
[`run_taxa_list`](https://hugomachadorodrigues.github.io/soilKey/reference/run_taxa_list.md);
a primeira test-block que passa captura o perfil. Os Grandes Grupos sao
carregados de `inst/rules/sibcs5/grandes-grupos/<ordem>.yaml` (split por
ordem) e mergeados pelo
[`load_rules`](https://hugomachadorodrigues.github.io/soilKey/reference/load_rules.md).

## Usage

``` r
run_sibcs_grande_grupo(pedon, subordem_code, rules = NULL)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- subordem_code:

  Codigo da subordem (e.g. "OJ" para Organossolos Tiomorficos).

- rules:

  Lista de regras carregada via
  [`load_rules`](https://hugomachadorodrigues.github.io/soilKey/reference/load_rules.md).

## Value

Lista com `assigned` (entrada YAML do Grande Grupo ou `NULL`) e `trace`.

## Details

Quando a subordem nao tem bloco de Grandes Grupos definido (ainda nao
wirado para todas as ordens), retorna
`list(assigned = NULL, trace = list())` – comportamento nao-fatal que
permite
[`classify_sibcs`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_sibcs.md)
parar no 2o nivel sem erro.
