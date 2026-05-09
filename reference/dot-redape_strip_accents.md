# Strip Portuguese accents and lowercase / collapse whitespace

Internal helper used by the Redape benchmark to canonicalise SiBCS
labels before string comparison. Maps accented Latin letters to their
ASCII equivalents (`A`-acute, `O`-tilde, `C`-cedilla, etc., for all five
Portuguese vowel classes).

## Usage

``` r
.redape_strip_accents(s)
```
