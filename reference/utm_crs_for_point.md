# UTM zone EPSG code for a lon/lat point

Picks the appropriate WGS84 UTM zone (32601..32660 northern,
32701..32760 southern) for a single coordinate. Used for metric
buffering.

## Usage

``` r
utm_crs_for_point(lon, lat)
```
