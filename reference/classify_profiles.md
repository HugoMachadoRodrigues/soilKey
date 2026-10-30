# Classify many profiles from one horizon table

Builds one
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
per profile from a table with one row per horizon, and classifies each
with
[`classify_all`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_all.md).
The column named by `id` says which profile a horizon belongs to; the
other columns use soilKey's names
([`horizon_attribute_spec`](https://hugomachadorodrigues.github.io/soilKey/reference/horizon_attribute_spec.md)),
or are renamed and converted by `map`. Site properties
([`site_attribute_spec`](https://hugomachadorodrigues.github.io/soilKey/reference/site_attribute_spec.md)),
such as `soil_moisture_regime`, come from `sites`.

## Usage

``` r
classify_profiles(
  horizons,
  sites = NULL,
  id = "profile_id",
  map = NULL,
  systems = "all",
  on_missing = "silent",
  ...
)
```

## Arguments

- horizons:

  A `data.frame` with one row per horizon and the column named by `id`.

- sites:

  Optional site properties: a `data.frame` with one row per profile and
  the same `id` column, or a list of lists named by profile id. A
  missing value (`NA`) is read as not recorded. Profiles without an
  entry get only their id.

- id:

  Name of the column that identifies the profile, in `horizons` and in a
  `sites` table (default `"profile_id"`).

- map:

  Optional `data.frame` with the columns `from` and `to`, and optionally
  `factor`: the column `from` of `horizons` becomes the column `to`,
  multiplied by `factor` (default 1) to convert its unit. For example,
  organic carbon in g/kg: `from = "ORGC", to = "oc_pct", factor = 0.1`.

- systems, on_missing:

  Forwarded to
  [`classify_all`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_all.md);
  the default `on_missing = "silent"` keeps a batch quiet, and the
  `missing` column reports what the keys did not find.

- ...:

  Other arguments forwarded to
  [`classify_all`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_all.md),
  such as `include_family`.

## Value

A `data.frame` with one row per profile, in the order of their first
horizon in `horizons`: the `id` column, one column per system with the
name of the class (as in the `summary` of
[`classify_all`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_all.md)),
`missing` (the attributes the keys asked for and did not find,
comma-separated) and `error` (`NA`, or why the profile could not be
classified). The attributes `"results"` and `"pedons"` hold the full
[`classify_all`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_all.md)
results and the pedons, in lists named by profile id.

## Details

The names are checked once for the whole table: a horizon column or site
property that no key reads is named in one warning (see
[`unrecognised_attributes`](https://hugomachadorodrigues.github.io/soilKey/reference/unrecognised_attributes.md)),
not once per profile. Within a profile the horizons are put in order of
`top_cm`. A profile that cannot be built or classified gets its reason
in the `error` column, and the others are classified as usual.

## See also

[`classify_all`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_all.md)
for one pedon,
[`classify_csv`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_csv.md)
for one profile in a spreadsheet,
[`unrecognised_attributes`](https://hugomachadorodrigues.github.io/soilKey/reference/unrecognised_attributes.md)
to check the names first.

## Examples

``` r
h <- rbind(
  cbind(profile_id = "p1", as.data.frame(make_ferralsol_canonical()$horizons)),
  cbind(profile_id = "p2", as.data.frame(make_phaeozem_canonical()$horizons)))
s <- data.frame(profile_id = c("p1", "p2"),
                soil_moisture_regime = c("udic", "ustic"))
res <- classify_profiles(h, sites = s, systems = "usda")
res[, c("profile_id", "usda", "error")]
#>   profile_id              usda error
#> 1         p1 Rhodic Haploperox  <NA>
#> 2         p2 Typic Haplustolls  <NA>

# \donttest{
# Columns named and measured differently: rename and convert with `map`.
h2 <- h
names(h2)[names(h2) == "oc_pct"] <- "ORGC"
h2$ORGC <- h2$ORGC * 10                      # g/kg
m <- data.frame(from = "ORGC", to = "oc_pct", factor = 0.1)
classify_profiles(h2, sites = s, map = m)[, c("profile_id", "wrb", "sibcs", "usda")]
#>   profile_id                                                  wrb
#> 1         p1 Geric Rhodic Ferralsol (Clayic, Epic, Eutric, Humic)
#> 2         p2             Chernic Phaeozem (Loamic, Humic, Pachic)
#>                                                                    sibcs
#> 1 Latossolos Vermelhos Distróficos típicos, textura argilosa, A moderado
#> 2                   Chernossolos Háplicos Órticos típicos, textura média
#>                usda
#> 1 Rhodic Haploperox
#> 2 Typic Haplustolls
# }
```
