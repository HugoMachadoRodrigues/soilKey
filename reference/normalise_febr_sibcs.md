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
out-of-scope for the comparison: any label that is not an order of SiBCS
5 after the legacy maps (old names without an entry there, "Solos ..."
followed by none of them, names of other systems, text that is not a
class).

## Details

A label is read by its first word, with any punctuation around it
dropped (`"ESPODOSSOLO."` is Espodossolos). Since v0.9.225 the result
must be one of the 13 orders of SiBCS 5 after the legacy map; otherwise
it is `NA`. Until then the first word came out whatever it was, so
`"CLASSIFICACAO ? CAMBISSOLO ..."` became the class "Classificacaos",
`"ALISSOLO CROMICO"` "Alissolos" and `"LATOSOL AMARELO"` "Latosois":
labels no prediction can equal, which the benchmarks counted as scored
profiles and as errors (631 of the 7,694 labels of the national BDsolos
export, in 79 forms).

Since v0.9.226, when the first word gives no order the label is read
once more as an old name: a leading "Solo" or "Solos" is set aside
("SOLO LITOLICO" is Neossolos, "SOLOS ORGANICOS" Organossolos), a hyphen
separates words as a space does ("LATOSSOLO-VERMELHO",
"SOLONETZ-SOLODIZADO"), and the names of `.SIBCS_LEGACY_TWO_WORD_MAP`
("TERRA ROXA ESTRUTURADA", "BRUNO NAO CALCICO") are looked up before
those of `.SIBCS_LEGACY_ORDER_MAP`. A label the first word already
places is read as before. At `level = "subordem"` an old name gives its
order followed by the next word of the label, which is not a suborder of
SiBCS 5 in general: the old names are for comparisons at the order
level.

## See also

[`normalise_febr_wrb`](https://hugomachadorodrigues.github.io/soilKey/reference/normalise_febr_wrb.md),
[`normalise_febr_usda`](https://hugomachadorodrigues.github.io/soilKey/reference/normalise_febr_usda.md)
