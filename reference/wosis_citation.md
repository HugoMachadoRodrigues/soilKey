# Citation for data read from WoSIS-latest

Returns the citation ISRIC asks users of WoSIS-latest to give, with the
date the data were read. The ISRIC Data and Software Policy requires
products, web services, papers and reports that use the data to
reference the provider and acknowledge that the data were acquired
through ISRIC.

## Usage

``` r
wosis_citation(accessed = Sys.Date())
```

## Arguments

- accessed:

  Date the data were read (default today).

## Value

A character string.

## Examples

``` r
wosis_citation(as.Date("2026-10-06"))
#> [1] "Batjes NH, Calisto L and de Sousa LM, 2024. WoSIS-latest: Standardised world soil profile data. ISRIC Soil Data Hub resource identifier: https://tinyurl.com/39xhaa9d. Date downloaded: 2026-10-06."
```
