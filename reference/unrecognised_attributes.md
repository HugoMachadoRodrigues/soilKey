# Attributes that soilKey does not recognise

Lists the horizon columns and site properties that no classification key
reads: a horizon column that is neither a canonical name of
[`horizon_column_spec`](https://hugomachadorodrigues.github.io/soilKey/reference/horizon_column_spec.md)
nor one that
[`ensure_horizon_schema`](https://hugomachadorodrigues.github.io/soilKey/reference/ensure_horizon_schema.md)
renames onto it (a difference of capitalisation or separators, or a
common alias such as `SOC`), and a site property that is not in
[`site_attribute_spec`](https://hugomachadorodrigues.github.io/soilKey/reference/site_attribute_spec.md).
Such attributes are kept in a
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
but have no effect on a classification. `PedonRecord$new()` warns about
them when it is called from outside the package, and so does
[`read_pedon_csv`](https://hugomachadorodrigues.github.io/soilKey/reference/read_pedon_csv.md);
`options(soilKey.warn_unrecognised = FALSE)` turns the warning off.

## Usage

``` r
unrecognised_attributes(x, site = NULL)
```

## Arguments

- x:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md),
  or a `data.frame` of horizons.

- site:

  A list of site properties, when `x` is a `data.frame`; ignored for a
  `PedonRecord`, whose own site is read.

## Value

A `data.frame` with one row per attribute that is not recognised and the
columns `where` (`"horizons"` or `"site"`), `name`, `suggestion` (the
closest recognised names, comma-separated, empty when none) and `note`:
empty, or `"site property"` for a horizon column that is a site
property, `"horizon attribute"` for a site property that is a horizon
column, or `"already given"` when the column it would be renamed to is
also present. No rows when everything is recognised.

## See also

[`horizon_attribute_spec`](https://hugomachadorodrigues.github.io/soilKey/reference/horizon_attribute_spec.md),
[`site_attribute_spec`](https://hugomachadorodrigues.github.io/soilKey/reference/site_attribute_spec.md),
`inst/ATTRIBUTES.md`.

## Examples

``` r
h <- data.frame(top_cm = c(0, 20), bottom_cm = c(20, 60),
                clay = c(30, 45), organic_carbon_gkg = c(11, 4),
                soil_moisture_regime = "ustic")
unrecognised_attributes(h, site = list(id = "p1", drainge_class = "well"))
#>      where                 name           suggestion          note
#> 1 horizons   organic_carbon_gkg               oc_pct              
#> 2 horizons soil_moisture_regime soil_moisture_regime site property
#> 3     site        drainge_class       drainage_class              
```
