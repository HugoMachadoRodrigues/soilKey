# WRB 2022 qualifier coverage (canonical vs genuinely-implemented `qual_*`).

"covered" means a `qual_*` function exists AND has a real implementation
(not an unconditional `passed = NA` stub). Stubs are reported separately
so the headline is honest rather than counting inert functions.
(Specifier-prefixed forms such as *Endogleyic* are derived by the
specifier engine from their base qualifier and are not canonical
qualifier names, so they never enter this count.)

## Usage

``` r
.coverage_wrb_qualifiers()
```
