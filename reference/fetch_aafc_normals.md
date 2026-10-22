# Climate normals of Agriculture and Agri-Food Canada at given points

Queries the "Derived Normal Climate Data" of Agriculture and Agri-Food
Canada (30-year normals on a 5 km grid, Open Government Licence -
Canada) at each point: the monthly mean air temperature of every month,
and the monthly minimum and maximum temperature and the precipitation
from April to October (the growing season of these normals). The values
come from the AAFC map service (one request per variable and per
thousand points); no file is downloaded. Needs the httr and jsonlite
packages and an internet connection.

## Usage

``` r
fetch_aafc_normals(lat, lon, period = "1961-1990", id = NULL, verbose = TRUE)
```

## Arguments

- lat, lon:

  Decimal degrees (WGS 84).

- period:

  One of `"1961-1990"`, `"1971-2000"`, `"1981-2010"`, `"1991-2020"`.

- id:

  Optional identifiers, returned as the first column.

- verbose:

  Print progress.

## Value

A data frame with `id`, `lat`, `lon` and one column per variable
(`ta_m01` to `ta_m12`, `tn_m04` to `tn_m10`, `tx_m04` to `tx_m10`,
`ac_m04` to `ac_m10`; degrees Celsius and mm), with the period and the
source as attributes. Points outside the grid get `NA`.

## References

Agriculture and Agri-Food Canada. Derived Normal Climate Data. Open
Government Portal (dataset 3a060b8f-e662-4a60-b297-3bed859ffc8a), Open
Government Licence - Canada.

## See also

[`npdb_soil_climate`](https://hugomachadorodrigues.github.io/soilKey/reference/npdb_soil_climate.md),
[`npdb_scenario`](https://hugomachadorodrigues.github.io/soilKey/reference/npdb_scenario.md)

## Examples

``` r
if (FALSE) { # \dontrun{
nm <- fetch_aafc_normals(lat = 52.13, lon = -106.67)
npdb_soil_climate(nm)
} # }
```
