# Load a canonical reference dataset from soilKey or SoilTaxonomy

Resolution order:

1.  If the `SoilTaxonomy` package is installed AND the `prefer_pkg`
    argument is `TRUE` (default), load the dataset from the installed
    package (always fresh).

2.  Otherwise, load from the vendored copy at
    `inst/extdata/canonical/<name>.rda`.

## Usage

``` r
canonical_reference(
  name = c("WRB_4th_2022", "ST_criteria_13th", "ST_features"),
  prefer_pkg = TRUE
)
```

## Arguments

- name:

  One of `"WRB_4th_2022"`, `"ST_criteria_13th"`, `"ST_features"`.

- prefer_pkg:

  If `TRUE` (default), prefer the installed SoilTaxonomy package over
  the vendored copy. Set to `FALSE` to force the vendored copy (e.g. for
  reproducibility of a specific soilKey release).

## Value

The dataset as the original R object (list or data.frame).

## See also

[`wrb2022_canonical`](https://hugomachadorodrigues.github.io/soilKey/reference/wrb2022_canonical.md),
[`kst13_canonical`](https://hugomachadorodrigues.github.io/soilKey/reference/kst13_canonical.md),
[`st_features_canonical`](https://hugomachadorodrigues.github.io/soilKey/reference/st_features_canonical.md).
