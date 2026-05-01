# Normalise a FEBR SiBCS taxon string to soilKey's plural Title Case

FEBR ships SiBCS names in ALL-CAPS Portuguese ("LATOSSOLO VERMELHO",
"NEOSSOLO LITOLICO", etc.) at the 2nd-level subordem granularity.
soilKey's
[`classify_sibcs()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_sibcs.md)
returns Title Case plural subordens ("Latossolos Vermelhos", "Neossolos
Litolicos"). This helper extracts the first word, plurals it, and
Title-Cases it, so the two can be matched at `level = "order"`.

## Usage

``` r
normalise_febr_sibcs(x, level = c("order", "subordem"))
```

## Arguments

- x:

  Character vector of FEBR SiBCS names.

- level:

  One of `"order"` (default; matches Latossolos / Argissolos / etc.) or
  `"subordem"` (Latossolos Vermelhos / Argissolos Vermelho-Amarelos /
  etc.).

## Value

Character vector of normalised soilKey-format names.

## Details

For `level = "order"` the comparison drops the second-level qualifier
entirely and matches on the Ordem (e.g. "Latossolos").
