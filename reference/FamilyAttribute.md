# Classe S4-like para atributos de Familia (5o nivel SiBCS)

Estrutura categorica (em vez de booleana) que representa um adjetivo
composto da Familia. `value` eh o adjetivo atribuido (string) ou `NULL`
quando a dimensao nao se aplica ou nao foi possivel determinar.

## Public fields

- `name`:

  Nome da dimensao (e.g. "grupamento_textural").

- `value`:

  Adjetivo atribuido (e.g. "argilosa") ou NULL.

- `evidence`:

  Lista nomeada com valores intermediarios.

- `missing`:

  Vetor de colunas necessarias mas indisponiveis.

- `reference`:

  String com referencia bibliografica.

## Methods

### Public methods

- [`FamilyAttribute$new()`](#method-FamilyAttribute-new)

- [`FamilyAttribute$print()`](#method-FamilyAttribute-print)

- [`FamilyAttribute$clone()`](#method-FamilyAttribute-clone)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    FamilyAttribute$new(
      name,
      value = NULL,
      evidence = list(),
      missing = character(0),
      reference = ""
    )

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

#### Usage

    FamilyAttribute$print(...)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FamilyAttribute$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
