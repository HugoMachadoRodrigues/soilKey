# Metadata of the site properties

One row per site property that soilKey recognises (the named elements of
`PedonRecord$site`), with the section it belongs to, its type, unit and
description, the values a categorical property takes where the keys
compare it with a fixed set, and the classification keys that read it.
The properties of the last section, *reference classes and provenance*,
are stored by the loaders and reports and read by no key. The metadata
come from `inst/schemas/site-attributes.yaml`; the attribute reference
`inst/ATTRIBUTES.md` lists them after the horizon attributes.

## Usage

``` r
site_attribute_spec()
```

## Value

A `data.frame` with the columns `name`, `type`, `section`, `unit`,
`description`, and `allowed` and `systems` (comma-separated, empty when
none).

## See also

[`horizon_attribute_spec`](https://hugomachadorodrigues.github.io/soilKey/reference/horizon_attribute_spec.md),
[`unrecognised_attributes`](https://hugomachadorodrigues.github.io/soilKey/reference/unrecognised_attributes.md).

## Examples

``` r
s <- site_attribute_spec()
s[s$name %in% c("drainage_class", "soil_moisture_regime"),
  c("name", "type", "description")]
#>                    name      type
#> 25       drainage_class character
#> 27 soil_moisture_regime character
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                description
#> 25 Drainage class, in English or Portuguese; a range ('imperfectly to poorly drained') is read by the end each test asks for. Read by the WRB Drainic, Subaquatic and Tidalic qualifiers (which also look for words such as 'artificial', 'submerged' and 'tidal'), the SiBCS horizonte glei (after drainage), the CSSC Folisol and Vertisol great groups, the ASC key (Hydrosols, organic materials, Folic Organosols, Aquic Vertosols, the Podosol and Hypersalic suborders) and the NZSC Organic Soils.
#> 27                                                                                                                                                                               USDA soil moisture regime, compared in lower case (aridic and torric are the same). Read by the USDA key (the Aridisols, and the ustic, xeric, udic and torric suborders, great groups and subgroups) and by the CSSC key when soil_moisture_subclass is absent (Chernozemic climate, the subhumid Solonetzic subgroups).
```
