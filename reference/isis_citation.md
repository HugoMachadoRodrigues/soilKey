# Citation for data read from ISIS

Returns the acknowledgement for profiles of the ISRIC World Soil
Reference Collection read through the ISIS GraphQL service, with the
date they were read. ISRIC asks users to cite the data it supplies in
their products, web services, papers and reports.

## Usage

``` r
isis_citation(accessed = Sys.Date())
```

## Arguments

- accessed:

  Date the data were read (default today).

## Value

A character string.

## Examples

``` r
isis_citation(as.Date("2026-10-29"))
#> [1] "ISRIC - World Soil Information. ISIS, the ISRIC Soil Information System: database of the World Soil Reference Collection. Wageningen, the Netherlands. Read through https://graphql.isric.org/isis/graphql on 2026-10-29."
```
