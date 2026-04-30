# DiagnosticResult: structured outcome of a diagnostic test

DiagnosticResult: structured outcome of a diagnostic test

DiagnosticResult: structured outcome of a diagnostic test

## Details

Returned by every WRB or SiBCS diagnostic function (e.g.
[`argic`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md),
[`ferralic`](https://hugomachadorodrigues.github.io/soilKey/reference/ferralic.md),
[`mollic`](https://hugomachadorodrigues.github.io/soilKey/reference/mollic.md)).
A DiagnosticResult never reduces to a bare TRUE/FALSE — it always
carries (a) which layers satisfied the criteria, (b) the per-sub-test
evidence, (c) which attributes would have been required but are missing,
and (d) the literature reference for the diagnostic definition.

`passed` is `TRUE`/`FALSE`/`NA`; `NA` means the test could not be
evaluated because critical attributes were missing. This three-valued
semantics propagates through the rule engine — an indeterminate test
does not silently fail.

## Public fields

- `name`:

  Character. Name of the diagnostic (e.g. `"argic"`).

- `passed`:

  Logical. `TRUE`, `FALSE`, or `NA`.

- `layers`:

  Integer vector. Indices of horizons that satisfy the diagnostic.

- `evidence`:

  Named list. Sub-test results, each itself a list with at least
  `passed`, `layers`, and `missing`.

- `missing`:

  Character vector. Attribute names that would have been needed but were
  NA.

- `reference`:

  Character. Literature citation for this diagnostic.

- `notes`:

  Character. Free-form notes (interpretation choices, edge cases hit).

## Methods

### Public methods

- [`DiagnosticResult$new()`](#method-DiagnosticResult-new)

- [`DiagnosticResult$print()`](#method-DiagnosticResult-print)

- [`DiagnosticResult$as_list()`](#method-DiagnosticResult-as_list)

- [`DiagnosticResult$clone()`](#method-DiagnosticResult-clone)

------------------------------------------------------------------------

### Method `new()`

Build a DiagnosticResult.

#### Usage

    DiagnosticResult$new(
      name,
      passed = NA,
      layers = integer(0),
      evidence = list(),
      missing = character(0),
      reference = NA_character_,
      notes = NA_character_
    )

#### Arguments

- `name`:

  Diagnostic name.

- `passed`:

  `TRUE`/`FALSE`/`NA`.

- `layers`:

  Integer vector of horizon indices that satisfied.

- `evidence`:

  Named list of sub-test results.

- `missing`:

  Character vector of missing attribute names.

- `reference`:

  Citation string.

- `notes`:

  Free-form notes.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Pretty-print the result with sub-test breakdown.

#### Usage

    DiagnosticResult$print(...)

------------------------------------------------------------------------

### Method `as_list()`

Return the result as a plain list (for serialization).

#### Usage

    DiagnosticResult$as_list()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DiagnosticResult$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
