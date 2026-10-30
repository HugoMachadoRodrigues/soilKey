# Turn one ISIS profile into a PedonRecord

Builds a
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
from one profile as the ISIS GraphQL service returns it (one element of
the `profiles` list; see
[`read_isis`](https://hugomachadorodrigues.github.io/soilKey/reference/read_isis.md),
which calls this for every profile it reads). Laboratory values are
converted to soilKey units, FAO field codes to soilKey's words, and each
value records its ISIS field in the provenance. The site carries the WRB
and USDA names ISRIC gives the profile (`reference_wrb`,
`reference_wrb_full`, `reference_usda`, `reference_usda_subgroup` and
their editions), the soil moisture and temperature regimes and the
groundwater (`groundwater_kind`, `groundwater_depth_cm`) when ISIS
records them, the licence and the citation.

## Usage

``` r
isis_profile_to_pedon(
  profile,
  absent_unless_recorded = TRUE,
  accessed = Sys.Date()
)
```

## Arguments

- profile:

  One ISIS profile, a list as parsed from the service's JSON.

- absent_unless_recorded:

  If `TRUE` (default), mottles, slickensides, pressure faces, plinthite,
  a duripan and artefacts that the field description of a horizon does
  not mention are recorded as absent in that horizon; rows with
  laboratory data only stay unknown. ISIS has one cutans field per
  layer, so slickensides and pressure faces are read as absent only
  where that field is free (v0.9.313). `FALSE` leaves everything not
  recorded unknown.

- accessed:

  Date the data were read (default today).

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
