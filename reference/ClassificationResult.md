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
  `"Rhodic Ferralsol (Clayic, Dystric, Humic)"`).

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

  Character. `"A"` (measured), `"B"` (spectra-predicted), `"C"`
  (prior-inferred), `"D"` (VLM-extracted), `"E"` (user-assumed), or
  `NA_character_` when no horizon carries a soil property (only depths
  and designations; since v0.9.213), so nothing in the class was
  verified.

- `prior_check`:

  List or NULL. Result of the spatial-prior sanity check (consistent /
  inconsistent / not run).

- `warnings`:

  Character vector. Free-form warnings.

- `system_id`:

  Character. Registry id of the system (see
  [`soilkey_systems`](https://hugomachadorodrigues.github.io/soilKey/reference/soilkey_systems.md)),
  e.g. `"wrb2022"`; since v0.9.265.

- `levels`:

  `data.frame` with one row per level of the key that assigned a class
  (`level`, `code`, `name`), top level first; set by the generic
  classifier of ordered keys and `NULL` for WRB, SiBCS and USDA, whose
  levels are in `trace`.

- `provisional_because`:

  Character vector. In a first-match key a class that could not be
  decided for lack of data, tested before the one assigned, makes the
  assignment provisional; these are those classes, as
  `"<level>:<code>"`. Empty when the assignment is firm or the
  classifier does not report it.

- `phases`:

  Soil phases of the systems that have them (the SCSJ 2017: temperature,
  man-made and forest phases; the CSCJ 2011: temperature and man-made
  phases), a `data.frame` with `kind`, `code`, `name`, `name_native` and
  `basis`; `NULL` otherwise. Since v0.9.324.

- `candidates`:

  For the systems that attach a soil to several classes rather than key
  it to one (the French Referentiel pedologique 2008, since v0.9.346,
  [`classify_rp2008`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_rp2008.md)),
  every class that may attach it: a `data.frame` with `code`, `name`,
  `group`, `degree` (simple, simple_or_imperfect, imperfect, undecided;
  fail for the nearest classes when nothing attaches), `rank` (the order
  of precedence), `in_name`, `double_with`, `double_possible`,
  `n_failed`, `distance` (the size of the failures in tolerance units),
  `failed`, `imperfect_by`, `undecided_by`, `excluded_by` (the criteria,
  joined with "; ") and `nearest`. `NULL` for every other system, and
  then not printed. Since v0.9.340.

- `humus_form`:

  The humus form of the French Referentiel pedologique 2008 (Annex 1;
  [`humus_form_rp`](https://hugomachadorodrigues.github.io/soilKey/reference/humus_form_rp.md)),
  a one-row `data.frame` with `form`, `name`, `type`, `label`, `group`,
  `decided`, `route`, `candidates`, `recorded`, `conflict` and
  `qualifiers`; the whole reading is in `trace$humus_form`. `NULL` for
  every other system. Since v0.9.353.

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
      warnings = character(0),
      system_id = NULL,
      levels = NULL,
      provisional_because = character(0),
      candidates = NULL
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

- `system_id`:

  Registry id of the system, or NULL.

- `levels`:

  Assigned class per level (data.frame), or NULL.

- `provisional_because`:

  Character vector of undecided classes tested before the assigned ones.

- `candidates`:

  The classes that may attach the soil (a data.frame), or NULL; since
  v0.9.340.

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
