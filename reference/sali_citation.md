# Citation for data read from Queensland's SALI

Returns the attribution for the soil site data of Queensland's Soil and
Land Information system (SALI) read by
[`fetch_sali`](https://hugomachadorodrigues.github.io/soilKey/reference/fetch_sali.md)
and
[`read_sali_pedons`](https://hugomachadorodrigues.github.io/soilKey/reference/read_sali_pedons.md):
the site layer of Queensland Spatial, the site listing reports
(SALI4320) and the SALI soil chemistry API. The data are licensed under
CC BY 4.0; the State makes no representations or warranties about them.

## Usage

``` r
sali_citation(accessed = NULL)
```

## Arguments

- accessed:

  Optional date the data were downloaded (its year is the year of the
  attribution; the current year when `NULL`).

## Value

A character string.

## Examples

``` r
sali_citation()
#> [1] "(c) State of Queensland (Department of Environment, Tourism, Science and Innovation), 2026. Soil and Land Information (SALI): Queensland soil survey sites (Queensland Spatial, SoilsAndLandResource MapServer layer 101), site listing reports (SALI4320) and the SALI soil chemistry API. Based on or contains data provided by the State of Queensland 2026. Licensed under CC BY 4.0 (https://creativecommons.org/licenses/by/4.0/)."
```
