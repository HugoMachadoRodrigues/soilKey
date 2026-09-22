# Canonicalise FEBR SiBCS names to match soilKey rule outputs.

FEBR ships SiBCS labels in mixed legacy/modern form (`"Podzolicos"` for
old name of Argissolos, singular vs plural, Portuguese accents). This
helper folds them to the form produced by
[`run_sibcs_key()`](https://hugomachadorodrigues.github.io/soilKey/reference/run_sibcs_key.md)
so that benchmark accuracies can be computed without false negatives.

## Usage

``` r
normalise_febr_sibcs(x, level = c("order", "subordem"))
```

## Arguments

- x:

  Character vector of FEBR SiBCS names.

- level:

  One of `"order"` (default) or `"subordem"`.

## Value

Character vector of normalised SiBCS names; `NA` for labels that are
out-of-scope for the comparison (e.g.\\ legacy `"Solos"` category).

## See also

[`normalise_febr_wrb`](https://hugomachadorodrigues.github.io/soilKey/reference/normalise_febr_wrb.md),
[`normalise_febr_usda`](https://hugomachadorodrigues.github.io/soilKey/reference/normalise_febr_usda.md)
