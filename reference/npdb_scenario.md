# A scenario for the NPDB: estimated dry colour and soil climate

Adds to NPDB pedons what the records lack and the Canadian key reads for
the Chernozems (and the Gray Brown Luvisols and some subgroups), each as
a range, for
[`benchmark_npdb`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_npdb.md):

- the dry value and chroma of the A horizons that record a moist colour
  and no dry one, from `dry_colour` (a
  [`npdb_dry_colour_model`](https://hugomachadorodrigues.github.io/soilKey/reference/npdb_dry_colour_model.md)),
  with the range of its error;

- the mean annual soil temperature, as the range of
  [`npdb_soil_climate`](https://hugomachadorodrigues.github.io/soilKey/reference/npdb_soil_climate.md)
  (from the air temperature to 4 degrees above it; kept as a range only
  where it crosses 0 or 8 degrees, the limits the key reads), and the
  moisture subclass by Thornthwaite, with the subclass by Hargreaves and
  every subclass in between where they differ.

The pedons hold the central estimate (the middle of the soil temperature
range) in the usual fields (`munsell_value_dry`, `munsell_chroma_dry`,
`site$mean_annual_soil_temp_C`, `site$soil_moisture_subclass`), and the
Thornthwaite moisture index in `site$npdb_cmi`; recorded values are
never replaced. The ranges are kept in `site$npdb_scenario` (a second
call adds to them); a pedon with no `site$id`, or one not in `climate`,
gets no climate. `benchmark_npdb` classifies such a pedon at every
combination of the ends of its ranges and counts a class as decided only
where they all give it; otherwise the result is provisional.

## Usage

``` r
npdb_scenario(pedons, climate = NULL, dry_colour = NULL)
```

## Arguments

- pedons:

  A list of
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
  from
  [`load_npdb_pedons`](https://hugomachadorodrigues.github.io/soilKey/reference/load_npdb_pedons.md).

- climate:

  A data frame from
  [`npdb_soil_climate`](https://hugomachadorodrigues.github.io/soilKey/reference/npdb_soil_climate.md)
  (its `id` matching `site$id`), or `NULL`.

- dry_colour:

  A
  [`npdb_dry_colour_model`](https://hugomachadorodrigues.github.io/soilKey/reference/npdb_dry_colour_model.md),
  or `NULL`.

## Value

The pedons (copies), with the estimates.

## Examples

``` r
if (FALSE) { # \dontrun{
peds <- load_npdb_pedons("path/to/npdb_v3_csv")
nm <- fetch_aafc_normals(vapply(peds, function(p) p$site$lat, 0),
                         vapply(peds, function(p) p$site$lon, 0),
                         id = names(peds))
sc <- npdb_scenario(peds, climate = npdb_soil_climate(nm),
                    dry_colour = npdb_dry_colour_model(peds))
benchmark_npdb(sc, level = "order", set_aside = character(0))
} # }
```
