# Normalise FEBR USDA taxon strings to USDA Soil Taxonomy Order

FEBR ships USDA Soil Taxonomy labels at the subgroup or great-group
granularity (e.g. "TYPIC HAPLUDULT", "ACRUSTOX"). The suffix of the
final word encodes the Order: `...OX` -\> Oxisols, `...ULT` -\>
Ultisols, `...EPT` -\> Inceptisols, etc. This helper extracts the Order
from the suffix so the benchmark can compare against
`classify_usda()$rsg_or_order` at `level = "order"`.

## Usage

``` r
normalise_febr_usda(x)
```

## Arguments

- x:

  Character vector of FEBR USDA names.

## Value

Character vector of normalised Order names ("Oxisols", "Ultisols",
"Inceptisols", ...).
