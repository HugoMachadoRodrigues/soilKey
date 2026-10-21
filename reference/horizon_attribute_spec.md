# Metadata of the horizon attributes

One row per canonical horizon column (see
[`horizon_column_spec`](https://hugomachadorodrigues.github.io/soilKey/reference/horizon_column_spec.md)),
in its order, with the section it belongs to, its unit, a description
and the alternative column names that are recognised for it. The
metadata come from `inst/schemas/horizon-attributes.yaml`, which also
records, where known, which classification keys read an attribute, the
values a categorical attribute takes and the range of a numeric one.

## Usage

``` r
horizon_attribute_spec()
```

## Value

A `data.frame` with the columns `name`, `type` (`"numeric"` or
`"character"`), `section`, `unit`, `description`, `aliases`, `systems`
and `allowed` (comma-separated, empty when none), and `min` and `max`
(`NA` when not recorded).

## See also

[`horizon_column_spec`](https://hugomachadorodrigues.github.io/soilKey/reference/horizon_column_spec.md),
[`ensure_horizon_schema`](https://hugomachadorodrigues.github.io/soilKey/reference/ensure_horizon_schema.md).

## Examples

``` r
a <- horizon_attribute_spec()
a[a$name %in% c("clay_pct", "cec_cmol"), c("name", "unit", "description")]
#>        name       unit                                   description
#> 20 clay_pct   % (mass)      Clay content of the fine earth (< 2 um).
#> 28 cec_cmol cmol(+)/kg Cation exchange capacity (of the fine earth).
```
