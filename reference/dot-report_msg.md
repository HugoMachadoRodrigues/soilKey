# Look up a fixed report label in the current report language.

Falls back to English, then to the key itself. `...` are passed to
`sprintf` when present (for labels with placeholders).

## Usage

``` r
.report_msg(key, ...)
```
