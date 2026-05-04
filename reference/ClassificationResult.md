# ClassificationResult: structured outcome of running a key

ClassificationResult: structured outcome of running a key

ClassificationResult: structured outcome of running a key

## Details

Returned by
[`classify_wrb2022`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_wrb2022.md)
(and the future `classify_sibcs`). Carries the full decision trace —
which RSGs were tested, which passed, which failed, which were
indeterminate because of missing data — plus the assigned class,
qualifiers, ambiguities (RSGs that nearly satisfied), missing data that
would refine the result, the provenance-aware evidence grade, and any
biogeographical or prior-based warnings.

## Public fields

- `system`:

  Character. `"WRB 2022"` or `"SiBCS 5"`.

- `name`:

  Character. Full classification name with qualifiers (e.g.
  `"Rhodic Ferralsol (Clayic, Humic, Dystric)"`).

- `rsg_or_order`:

  Character. Bare RSG (WRB) or order (SiBCS), e.g. `"Ferralsols"`.

- `qualifiers`:

  List. Principal and supplementary qualifiers in canonical order.

- `trace`:

  List. One element per RSG tested (in key order), each with `code`,
  `name`, `passed`, `evidence`, `missing`.

- `ambiguities`:

  List. RSGs that came close to passing — useful hints for follow-up
  measurements.

- `missing_data`:

  Character vector. Attributes whose measurement would refine or resolve
  the result.

- `evidence_grade`:

  Character. `"A"`, `"B"`, `"C"`, `"D"`, or `NA_character_`.

- `prior_check`:

  List or NULL. Result of the spatial-prior sanity check (consistent /
  inconsistent / not run).

- `warnings`:

  Character vector. Free-form warnings.

## Methods

### Public methods

- [`ClassificationResult$new()`](#method-ClassificationResult-new)

- [`ClassificationResult$print()`](#method-ClassificationResult-print)

- [`ClassificationResult$summary()`](#method-ClassificationResult-summary)

- [`ClassificationResult$report()`](#method-ClassificationResult-report)

- [`ClassificationResult$clone()`](#method-ClassificationResult-clone)

------------------------------------------------------------------------

### Method `new()`

Build a ClassificationResult.

#### Usage

    ClassificationResult$new(
      system,
      name,
      rsg_or_order = NA_character_,
      qualifiers = list(),
      trace = list(),
      ambiguities = list(),
      missing_data = character(0),
      evidence_grade = NA_character_,
      prior_check = NULL,
      warnings = character(0)
    )

#### Arguments

- `system`:

  System name.

- `name`:

  Classification name.

- `rsg_or_order`:

  RSG (WRB) or order (SiBCS).

- `qualifiers`:

  List of qualifier names.

- `trace`:

  List of per-RSG test entries.

- `ambiguities`:

  List of close-call RSGs.

- `missing_data`:

  Character vector.

- `evidence_grade`:

  Single character A/B/C/D or NA.

- `prior_check`:

  List or NULL.

- `warnings`:

  Character vector.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Pretty-print the result with key trace, ambiguities, and warnings.

#### Usage

    ClassificationResult$print(...)

#### Arguments

- `...`:

  Ignored (S3 print signature compatibility).

------------------------------------------------------------------------

### Method [`summary()`](https://rdrr.io/r/base/summary.html)

Compact summary list.

#### Usage

    ClassificationResult$summary(...)

#### Arguments

- `...`:

  Ignored (S3 summary signature compatibility).

------------------------------------------------------------------------

### Method [`report()`](https://hugomachadorodrigues.github.io/soilKey/reference/report.md)

Render this classification as a self-contained report (delegates to the
package-level
[`report`](https://hugomachadorodrigues.github.io/soilKey/reference/report.md)
generic). HTML output is dependency-free; PDF requires `rmarkdown` and a
working LaTeX engine.

#### Usage

    ClassificationResult$report(
      file,
      format = c("auto", "html", "pdf"),
      pedon = NULL,
      ...
    )

#### Arguments

- `file`:

  Output path. Format is inferred from the extension.

- `format`:

  One of "html" or "pdf" (defaults to "auto", which infers from the
  extension).

- `pedon`:

  Optional `PedonRecord` whose horizons / provenance are added to the
  report.

- `...`:

  Forwarded to
  [`report_html`](https://hugomachadorodrigues.github.io/soilKey/reference/report_html.md)
  or
  [`report_pdf`](https://hugomachadorodrigues.github.io/soilKey/reference/report_pdf.md).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ClassificationResult$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
