# Read New Zealand National Soils Database survey exports

Reads the survey exports of the New Zealand National Soils Database
(NSD, Manaaki Whenua - Landcare Research), as the National Soil Data
Repository (NSDR) serves them (one zip of relational CSV tables per
survey), into
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
objects. The tables are read in memory from the zips (or from unzipped
folders); nothing is written to disk.

## Usage

``` r
read_nsd_pedons(
  path,
  absent_unless_recorded = TRUE,
  old_notation = c("translate", "asis"),
  classified_only = FALSE,
  accessed = NULL,
  verbose = FALSE
)
```

## Arguments

- path:

  One or more NSD survey zips, folders holding the unzipped CSV tables
  of a survey, or folders of either.

- absent_unless_recorded:

  If `TRUE` (default), a described horizon (one with a matrix colour)
  with no mottle record, in a survey that records mottles, has none,
  unless its designation marks gleying.

- old_notation:

  `"translate"` (default) or `"asis"`: how designations in the older New
  Zealand notation are read (see Details).

- classified_only:

  If `TRUE`, only profiles with an NZSC class.

- accessed:

  Optional date the exports were downloaded, kept in the site and the
  citation.

- verbose:

  Print the number of profiles read per survey.

## Value

A named list of
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
objects (names are the NSD site identifiers).

## Details

Each pedon carries the field description (designations, depths,
boundary, moist Munsell colour, field texture, strength, failure,
stickiness, plasticity, cementation, mottles, structure, clay coats and
the colour of coatings, pans, roots) and the laboratory results of its
samples (carbon, nitrogen, pH in water, KCl, CaCl2 and NaF, P retention,
CEC and exchangeable cations, base saturation, KCl-extractable Al,
oxalate, dithionite and pyrophosphate Fe and Al, particle size, bulk
density, volcanic glass in the sand, stones, CaCO3, EC of a 1:5 extract,
water at 1500 kPa, COLE), with the NSD table and column of every value
in the provenance. Codes are decoded through the survey's `cl_concept`
table. The site holds the surveyor's classes: `reference_nzsc` (the NZSC
code, e.g. `"MOT"`, recoded to the 4th edition where the 4th edition
renamed it), `reference_nzsc_name`, the order, group and subgroup codes
(`reference_nzsc_order`, `_group`, `_subgroup`), the record as written
(`reference_nzsc_raw`), `reference_usda` (the Soil Taxonomy order of the
recorded name), `reference_usda_grtgroup`, `reference_usda_subgroup`,
the NZ Genetic class and series; and the drainage class, the parent
material (the soil note "PARENT ROCK"), the landform, whether the land
is drained, the mean annual air temperature, latitude and longitude
(from NZTM2000), the licence and the citation
([`nsd_citation`](https://hugomachadorodrigues.github.io/soilKey/reference/nsd_citation.md)).
The header of `R/nsd.R` lists every reading.

Designations: the NSD gives many horizons a designation in the current
notation (`designation_fao`), which is read (an FAO H master as the New
Zealand O); the others keep the surveyor's designation, often in the
older New Zealand notation (Taylor and Pohlen 1962). With
`old_notation = "translate"`, (B) is read as Bw, the suffixes fe, ca and
cs as s, k and y, an upper-case G suffix as g, and digits written
between the master and its suffixes are moved to the end (B2g as Bg2).
A2 is read as E, with its subdivisions and suffixes (A21 as E1, A2g as
Eg). The old master G (a gley horizon) is read as Bg when a B horizon
lies below it in the profile, otherwise as Cg (GC as BCg or Cg); the old
master D (the substratum) as C, its prefixes and suffixes kept (BD as
BC, DG as Cg) except its old suffixes r and n, which are not read (Dr
and Dn as C; the NSD's own conversions give them as Cu, C, Bt or Cm). A
cemented sesquioxidic B written Bms is read as Bfm (a placic horizon)
when 10 mm or less thick and as Bsm (an ortstein-pan) when thicker, as
the NZSC Diagnostics define them. The provenance keeps the recorded
designation and what it was read as.

Structure: the older ped term nut (not a shape of Milne et al. 1995) is
granular or polyhedral, one of the two (NZ Soil Description Handbook
2024, Note 12.1). It is kept in `structure_type_one_of` with
`structure_type` empty:
[`classify_nzsc4`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_nzsc4.md)
reads a criterion under each shape, and the other keys read the shape as
not recorded. The older strength words moderately weak, moderately firm
and moderately strong (of the Taylor and Pohlen 1962 scale) are not
read.

Parent material: the soil note PARENT ROCK is also the `layer_origin` of
each horizon whose origin is not recorded (the NSD exports record none
per horizon). A note naming a sequence of materials ("loess over outwash
gravels") is split by the profile's lithologic groups (2C, IIC) when its
parts are as many; otherwise it stays the site's `parent_material` only.

The NSD records no field reactive-aluminium (NaF) test, Childs' test,
sensitivity, fluidity, packing or Ksat, which the NZSC key reads; the
laboratory pH in NaF is kept in `ph_naf`.

## References

Manaaki Whenua - Landcare Research (2020). National Soils Database
(NSD). [doi:10.26060/95m4-cz25](https://doi.org/10.26060/95m4-cz25) .
Milne, J.D.G., Clayden, B., Singleton, P.L. and Wilson, A.D. (1995).
Soil Description Handbook. Manaaki Whenua Press.

## See also

[`benchmark_nsd`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_nsd.md),
[`classify_nzsc4`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_nzsc4.md).

## Examples

``` r
if (FALSE) { # \dontrun{
peds <- read_nsd_pedons("path/to/nsd/surveys")
b <- benchmark_nsd(peds, level = "order")
b$accuracy
} # }
```
