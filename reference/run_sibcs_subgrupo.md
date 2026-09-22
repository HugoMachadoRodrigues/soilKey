# Resolve o subgrupo (4o nivel) de um pedon classificado em um Grande Grupo SiBCS

v0.7.3.B: itera os Subgrupos do Grande Grupo em ordem canonica via o
engine generico
[`run_taxa_list`](https://hugomachadorodrigues.github.io/soilKey/reference/run_taxa_list.md);
a primeira test-block que passa captura o perfil. Os Subgrupos sao
carregados de `inst/rules/sibcs5/subgrupos/<ordem>.yaml` (split por
ordem) e mergeados pelo
[`load_rules`](https://hugomachadorodrigues.github.io/soilKey/reference/load_rules.md).

## Usage

``` r
run_sibcs_subgrupo(pedon, gg_code, rules = NULL)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- gg_code:

  Codigo do Grande Grupo (e.g. "OJF" para Organossolos Tiomorficos
  Fibricos).

- rules:

  Lista de regras carregada via
  [`load_rules`](https://hugomachadorodrigues.github.io/soilKey/reference/load_rules.md).

## Value

Lista com `assigned` (entrada YAML do Subgrupo ou `NULL`) e `trace`.

## Details

Em contraste com o 3o nivel (Grandes Grupos de Organossolos), Subgrupos
de Cap 14 SEMPRE tem catch-all `tests:{default:true}` como ultima
entrada de cada lista (subgrupo "tipico"), entao a classificacao sempre
desce ao 4o nivel quando o GG foi resolvido.
