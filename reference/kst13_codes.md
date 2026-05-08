# Load the canonical KST 13ed code -\> taxon-name lookup table

Returns the 3,153-row data.frame from
`inst/rules/usda/canonical/2022_KST_codes.json`, vendored from
NCSS-tech/SoilKnowledgeBase. Each row is a (code, name) pair.

## Usage

``` r
kst13_codes()
```

## Value

A data.frame with columns `code, name`.

## Details

Code structure:

- Single letter (`"A"`-`"L"`): Soil Order (Gelisols, Histosols, ...,
  Entisols)

- Two letters (`"AB"`, `"AC"`, ...): Suborder

- Three letters: Great Group

- Four letters: Subgroup

## See also

[`kst13_criteria`](https://hugomachadorodrigues.github.io/soilKey/reference/kst13_criteria.md),
[`kst13_canonical`](https://hugomachadorodrigues.github.io/soilKey/reference/kst13_canonical.md).
