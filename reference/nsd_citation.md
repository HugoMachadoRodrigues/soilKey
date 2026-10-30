# Citation for data read from the New Zealand National Soils Database

Returns the acknowledgement for profiles of the National Soils Database
(NSD) of Manaaki Whenua - Landcare Research, as exported by the National
Soil Data Repository (NSDR) and read by
[`read_nsd_pedons`](https://hugomachadorodrigues.github.io/soilKey/reference/read_nsd_pedons.md).

## Usage

``` r
nsd_citation(accessed = NULL)
```

## Arguments

- accessed:

  Optional date the survey exports were downloaded.

## Value

A character string.

## Examples

``` r
nsd_citation()
#> [1] "Manaaki Whenua - Landcare Research (2020). National Soils Database (NSD). https://doi.org/10.26060/95m4-cz25. Survey exports of the National Soil Data Repository (NSDR). Licensed under CC BY-ND 4.0."
```
