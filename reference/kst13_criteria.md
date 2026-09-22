# Load the canonical KST 13ed criteria for a single taxon code

Returns the parsed clause data.frame for one code (e.g. `"A"` for
Gelisols, `"ABA"` for Histels.Folistels, etc.). Each row is one clause
of the diagnostic text with `content`, `chapter`, `page` columns.

## Usage

``` r
kst13_criteria(code)
```

## Arguments

- code:

  Character. Taxon code in the KST 13ed code system (e.g. `"A"` for
  Gelisols, `"ABCDA"` for the Lithic Folistels subgroup).

## Value

A data.frame with the parsed clauses for that code, or `NULL` if the
code is not present.

## Details

For the full 3,153-element nested list (all codes), use
[`kst13_canonical`](https://hugomachadorodrigues.github.io/soilKey/reference/kst13_canonical.md)
(which loads the SoilTaxonomy R-package RDA equivalent).

## See also

[`kst13_codes`](https://hugomachadorodrigues.github.io/soilKey/reference/kst13_codes.md),
[`kst13_canonical`](https://hugomachadorodrigues.github.io/soilKey/reference/kst13_canonical.md).
