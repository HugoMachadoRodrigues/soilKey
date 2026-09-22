# Load a canonical reference dataset from the SoilTaxonomy package

Reads one of the parsed WRB 2022 / Keys to Soil Taxonomy datasets from
the installed SoilTaxonomy package (NCSS-tech, GPL-3). soilKey ships no
copy of these data: install SoilTaxonomy from CRAN to use this function.

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

  Retained for compatibility and no longer used. Until v0.9.203 `FALSE`
  selected a copy bundled with soilKey; there is no bundled copy any
  more, so the data always come from SoilTaxonomy.

## Value

The dataset as the original R object (list or data.frame).

## See also

[`wrb2022_canonical`](https://hugomachadorodrigues.github.io/soilKey/reference/wrb2022_canonical.md),
[`kst13_canonical`](https://hugomachadorodrigues.github.io/soilKey/reference/kst13_canonical.md),
[`st_features_canonical`](https://hugomachadorodrigues.github.io/soilKey/reference/st_features_canonical.md).
