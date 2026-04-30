# Classifica um pedon segundo o SiBCS 5a edicao (1o + 2o + 3o + 4o niveis)

v0.7 ligou as 13 ordens; v0.7.1 desce ao 2o nivel (subordens) via
[`run_sibcs_subordem`](https://hugomachadorodrigues.github.io/soilKey/reference/run_sibcs_subordem.md);
v0.7.3 desce ao 3o nivel (Grandes Grupos) via
[`run_sibcs_grande_grupo`](https://hugomachadorodrigues.github.io/soilKey/reference/run_sibcs_grande_grupo.md)
para as ordens progressivamente wiradas em
`inst/rules/sibcs5/grandes-grupos/<ordem>.yaml` (Cap 14 Organossolos
primeiro). Quando a subordem ainda nao tem bloco de Grandes Grupos, ou
quando nenhum Grande Grupo passa (e nao ha catch-all default), a
classificacao para no 2o nivel.

## Usage

``` r
classify_sibcs(
  pedon,
  rules = NULL,
  on_missing = c("warn", "silent", "error"),
  include_familia = FALSE
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- rules:

  Conjunto de regras pre-carregado.

- on_missing:

  Um de `"warn"` (default), `"silent"`, `"error"`.

- include_familia:

  Quando `TRUE` (default `FALSE`), adiciona o 5o nivel categorico
  (Familia) via
  [`classify_sibcs_familia`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_sibcs_familia.md).
  O label textual da Familia aparece em `$trace$familia_label`, e a
  lista de
  [`FamilyAttribute`](https://hugomachadorodrigues.github.io/soilKey/reference/FamilyAttribute.md)s
  em `$trace$familia`.

## Value

Um
[`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md)
cujo `name` eh o nome completo da classe atribuida no nivel mais
profundo (Grande Grupo \> Subordem \> Ordem) e `rsg_or_order` eh o nome
da ordem (e.g. "Organossolos"). Os codigos de cada nivel e o trace ficam
em `$trace`.
