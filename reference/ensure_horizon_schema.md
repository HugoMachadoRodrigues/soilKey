# Coerce a horizons-like data.frame to the canonical schema

Adds any missing canonical columns as NAs of the right type and reorders
canonical columns first. Extra user-supplied columns are preserved at
the end. Coerces character values to numeric where the schema requires
it.

Column names are recognised tolerantly before coercion: an exact
canonical name always wins, but names differing only in capitalisation
or separators (`pH_H2O`, `PH.H2O` -\> `ph_h2o`) and a small table of
common analytical abbreviations (`Clay` -\> `clay_pct`, `SOC` -\>
`oc_pct`, `CEC` -\> `cec_cmol`, ...) are mapped to their canonical form
rather than silently ignored. See `inst/ATTRIBUTES.md` for the complete
list of recognised names and accepted aliases.

## Usage

``` r
ensure_horizon_schema(h)
```

## Arguments

- h:

  Input data.frame or data.table.

## Value

A `data.table` with the canonical horizon columns present, in canonical
order, with extra columns preserved at the end.

## Examples

``` r
h <- ensure_horizon_schema(data.frame(top_cm = 0, bottom_cm = 20))
"designation" %in% names(h)
#> [1] TRUE
# capitalisation and common abbreviations are recognised:
h2 <- ensure_horizon_schema(data.frame(pH_H2O = 5.4, Clay = 32, SOC = 1.1))
h2$ph_h2o
#> [1] 5.4
```
