# Read NatSoil profiles as pedons

Turns the profile files saved by
[`fetch_natsoil`](https://hugomachadorodrigues.github.io/soilKey/reference/fetch_natsoil.md)
(or any JSON of the NatSoil API with horizons, samples and laboratory
results) into
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
objects for
[`classify_asc3`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_asc3.md)
and the other keys. Sites whose licence does not allow external use are
left out.

## Usage

``` r
load_natsoil_pedons(
  path,
  classified_only = TRUE,
  max_n = NULL,
  seed = NULL,
  verbose = TRUE
)
```

## Arguments

- path:

  A folder (searched recursively for `.json` and `.json.gz` files) or a
  vector of files.

- classified_only:

  Keep only sites with an ASC order.

- max_n:

  Optional maximum number of files read (a random sample; a profile file
  of
  [`fetch_natsoil`](https://hugomachadorodrigues.github.io/soilKey/reference/fetch_natsoil.md)
  holds one site).

- seed:

  Optional seed for that sample.

- verbose:

  Print a summary.

## Value

A named list of
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
objects (names are `agency_project_site`).

## Details

The horizons keep their designation in the Australian notation
(lithologic prefix, master, subdivision and suffixes), depths in cm, the
field texture grade, boundary distinctness, the grade and type of the
first structure (lenticular when any structure is), the moist and the
dry matrix colours (a colour with no moisture status is read as moist),
slickensides, crack width, coarse fragments (abundance classes at their
middle, added up to at most 100 %; unknown when a record has no
abundance), calcareous segregations (a record with only its nature is a
template and is not read; one with a form or size but no abundance
counts as very few, 1 %), cementation of pans, effervescence, plasticity
type and the moisture when described.

Laboratory results are matched to the horizons by depth: a sample counts
for the horizon it is filed under where they overlap, and for another
one when it lies mostly within it, weighed by the overlap (a sample with
no usable depth counts for the horizon it is filed under). Codes are
matched whatever their case, the "not recorded" methods included. A
value below the detection limit, written "\<x", is read as half the
limit, unless the limit lies above a limit the keys read for that
property (0.5 % CaCO3, 0.18 cmol/kg Na, an ESP of 6, 5 % free Fe, 12 %
organic C, 10 % clay), where the value is unknown and the next method is
tried. pH in water is the lab 1:5 value (method 4A1, or a pH whose
method is not recorded, often the field pH itself); the field pH is used
only where the lab has no pH at all. Organic C is the measured value, or
an uncorrected Walkley-Black value times 1.3; half the loss on ignition
only in peat (an O or P horizon, or a peat field texture). Exchangeable
bases and CEC are read whatever the extraction method; a laboratory ESP
is kept as `esp_pct`. Free iron is read only from dithionite
extractions; most profiles have none, which leaves the Ferrosols
undecided.

The site keeps the surveyors' classification (`reference_asc_order`,
`reference_asc_suborder`, `reference_asc_great_group`,
`reference_asc_subgroup`, the confidence `reference_asc_confidence`),
the drainage class, the surface condition, the microrelief, coordinates
(GDA94), the licence and the citation.

## References

CSIRO. CSIRO National Soil Site Database (NatSoil).
https://doi.org/10.25919/sg5w-ny87

## See also

[`fetch_natsoil`](https://hugomachadorodrigues.github.io/soilKey/reference/fetch_natsoil.md),
[`benchmark_natsoil`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_natsoil.md).
