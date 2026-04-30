# Constroi label textual de Familia a partir de `classify_sibcs_familia`

Concatena os `value` nao-nulos como string separada por virgulas,
conforme orientado no Cap 18, p 281: "as caracteristicas utilizadas para
identificacao do 5o nivel categorico devem ser acrescentadas apos a
designacao do 4o nivel categorico e separadas desta e entre si por
virgula".

## Usage

``` r
familia_label(familia)
```

## Arguments

- familia:

  Lista de
  [`FamilyAttribute`](https://hugomachadorodrigues.github.io/soilKey/reference/FamilyAttribute.md),
  retorno de
  [`classify_sibcs_familia`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_sibcs_familia.md).

## Value

String com adjetivos compostos separados por ", ", ou vazia se nenhum
adjetivo se aplica.
