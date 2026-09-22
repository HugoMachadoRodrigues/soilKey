# Load the bundled AfSP stratified sample (v0.9.77)

Returns a 130-profile snapshot from AfSP v1.2 stratified by WRB RSG (5
profiles per RSG x 26 RSGs), pre-built so users can run the African WRB
benchmark offline without the 35 MB ZIP download.

## Usage

``` r
load_afsp_sample()
```

## Value

A list with `pedons`, `pulled_on`, `source`, `filter`.

## Details

This is the African analogue of
[`load_wosis_stratified_sample`](https://hugomachadorodrigues.github.io/soilKey/reference/load_wosis_stratified_sample.md)
(global WoSIS) and
[`load_kssl_nasis_sample`](https://hugomachadorodrigues.github.io/soilKey/reference/load_kssl_nasis_sample.md)
(US KSSL+NASIS).

## Reference

Leenaars, J. G. B., van Oostrum, A. J. M., & Ruiperez Gonzalez, M.
(2014). Africa Soil Profiles Database, Version 1.2. ISRIC Report
2014/01.
