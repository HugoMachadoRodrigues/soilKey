# Normalise FEBR WRB taxon strings to RSG-only

FEBR ships WRB names with full qualifier strings, e.g. "HUMIC
FERRALSOL", "HAPLIC ACRISOL (ALUMIC, HYPERDYSTRIC, ...)". The trailing
word (before any qualifier parens) is the RSG. This helper extracts and
normalises it to soilKey's plural Title Case form ("Ferralsols",
"Acrisols"), matching `ClassificationResult$rsg_or_order`.

## Usage

``` r
normalise_febr_wrb(x)
```

## Arguments

- x:

  Character vector of FEBR WRB names.

## Value

Character vector of normalised RSG names.
