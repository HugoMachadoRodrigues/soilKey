# Detect the line where the BDsolos CSV header starts

BDsolos exports prepend a 1-line preamble plus an empty line before the
actual schema header (a long quoted-string row with hundreds of fields).
This walks the first N lines and returns the 1-based index of the header
row.

## Usage

``` r
.bdsolos_find_header_line(path, n_probe = 10L)
```
