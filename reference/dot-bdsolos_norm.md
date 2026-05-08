# Strip Latin-1 diacritics + lowercase for fuzzy matching

iconv ASCII//TRANSLIT renders Portuguese diacritics as bigraphs (e.g.
a-tilde -\> ~a, c-cedilla -\> c') which then get collapsed into spurious
underscores. Pre-replace the common Portuguese diacritics by hand for
deterministic output.

## Usage

``` r
.bdsolos_norm(x)
```
