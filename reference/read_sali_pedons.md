# Read Queensland SALI sites as pedons

Turns the files saved by
[`fetch_sali`](https://hugomachadorodrigues.github.io/soilKey/reference/fetch_sali.md)
into
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
objects for
[`classify_asc3`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_asc3.md)
and the other keys: the site layer (`layer/layer101_off*.json`, or a
data frame saved as `layer/layer101.rds`), the site reports
(`pdf/<PROJECT>_<SITE>.pdf`) and the laboratory samples of the SALI soil
chemistry API (`lab/<PROJECT>_<SITE>_samples.json`). A site is read when
both its report and its laboratory file are on disk; sites whose report
was over the download cap, whose report has no morphology table, or
whose label names no ASC order (the layer's "Unclassified" sites) are
left out.

## Usage

``` r
read_sali_pedons(
  path,
  sites = NULL,
  cores = 1L,
  max_n = NULL,
  seed = NULL,
  accessed = NULL,
  verbose = TRUE
)
```

## Arguments

- path:

  The folder
  [`fetch_sali`](https://hugomachadorodrigues.github.io/soilKey/reference/fetch_sali.md)
  wrote.

- sites:

  Optional sites to read (a data frame with `project_code` and
  `site_id`, or `"<PROJECT>_<SITE>"`).

- cores:

  Number of processes
  ([`parallel::mclapply`](https://rdrr.io/r/parallel/mclapply.html);
  always 1 on Windows).

- max_n:

  Optional maximum number of sites read (a random sample).

- seed:

  Optional seed for that sample.

- accessed:

  Optional date the data were downloaded, for the citation.

- verbose:

  Print a summary.

## Value

A named list of
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
objects (names `SALI_<PROJECT>_<SITE>`), with an attribute
`"sali_status"`: a data frame of the sites considered and what became of
each.

## Details

The morphology exists only in the report (a PDF), read from the
coordinates of its words (pdftools). Each row of its Profile morphology
table is translated into the coded horizon of the NatSoil API (Field
Handbook codes) and read by the NatSoil reader's own conventions (see
[`load_natsoil_pedons`](https://hugomachadorodrigues.github.io/soilKey/reference/load_natsoil_pedons.md)),
the laboratory samples too, so both Australian panels are read alike.
These readings (the SALI pilot of 2026-10-30, kept by Hugo) apply:

- Observation 1 of the site is read (a report may hold several).

- The surveyors' label of the layer is read by position (order,
  suborder, great group, subgroup) and each name matched,
  case-insensitive, to the 3rd edition (`inst/rules/asc3`); a name the
  3rd edition does not have stays `NA`. "No Available Class" is `ZZ` and
  "Class Undetermined" `YY`. A suborder with no great groups
  (Hypergypsic, Lutic, Stratic Rudosols) has its subgroup in the
  great-group column, as NatSoil has it. The edition the report cites is
  kept in `site$sali_asc_edition`, without changing the codes. The
  report's confidence phrases are the Field Handbook's 1-4; other
  phrases (a conversion from another classification, a rapid check of
  key attributes) are `"C"`.

- An empty cell is not recorded; "No mottles", "No segregations" and "No
  coarse fragments" are recorded absent. A colour without its moisture
  status is moist. Mottles of biological mixing, mechanical mixing and
  substrate influence are not mottles; jarosite mottles are.

- Textures: "Loam, fine sandy" is a loam; sandy and silty clays are
  clays of unspecified grade; heavy or light before a grade lighter than
  a clay is the + or - qualifier. The first structure listed is read;
  "Blocky" alone names no type. "Not recorded (historical)" is `NA`. A
  strength without its moisture is not read.

- Field pH, by depth: Raupach and Tucker first, then an electrode in
  situ, then a 1:5 suspension by probe (the pH after peroxide and the
  field EC are not used); it is read only where the laboratory has no
  pH, as in NatSoil. The effervescence field test gives the horizon's
  effervescence.

- Laboratory samples (not linked to horizons in SALI) are filed under
  the horizon holding their middle, then read by NatSoil's overlap rule;
  bulked samples count; results flagged invalid are dropped; "\<x" is x
  / 2 with NatSoil's limit rule. A trailing letter of a method variant
  is dropped (6B2b is 6B2) and 2Z2_Clay is 2Z2_C.

- Tidal landform spellings are NatSoil's (inter-tidal is intertidal); no
  tidal flooding is inferred, nor the soil water status; the depth to
  free water and the substrate are kept in the site
  (`sali_depth_to_free_water`, `sali_substrate`) but not used.

And Hugo's decisions of 2026-10-30:

- MIR predictions (method codes ending in \_MC2, \_MC91, \_MQ01, \_MQ02
  and the like) are used only where nothing was measured for that
  attribute in that horizon (a field pH the horizon reads counts as
  measured), and the provenance marks them as predictions (source
  `"predicted_spectra"`, a note naming the MIR codes; two calibrations
  predicting the same method are averaged). MIR methods the NatSoil
  reader has no code for are read by the property their name gives:
  15C2_CEC as the CEC, 6B4 as organic carbon and 15J2 as the effective
  CEC.

- The lab table printed in the report is used only when the API holds no
  samples for the site's observation.

The site keeps the surveyors' classification (`reference_asc_order`,
`reference_asc_suborder`, `reference_asc_great_group`,
`reference_asc_subgroup`, `reference_asc_confidence`), as
[`benchmark_sali`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_sali.md)
and
[`benchmark_natsoil`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_natsoil.md)
read it, the label and the report's ASC name (`sali_asc_label`,
`sali_pdf_asc`), names left unmatched (`sali_unmapped`), the drainage,
landform, surface condition, microrelief and surface coarse fragments,
coordinates (GDA94), the licence and the citation
([`sali_citation`](https://hugomachadorodrigues.github.io/soilKey/reference/sali_citation.md)),
where the laboratory values came from (`sali_lab_source`: `"api"`,
`"report table"` or `"none"`) and how many values are MIR predictions
(`sali_n_predicted`). The provenance has one row per horizon for its
morphology and one per laboratory value, which names the source (the API
or the report's table) and the method; the report's notes are kept there
too.

## References

Queensland Government. Soil and Land Information (SALI). National
Committee on Soil and Terrain (2009). Australian Soil and Land Survey
Field Handbook, 3rd edition. CSIRO Publishing.

## See also

[`fetch_sali`](https://hugomachadorodrigues.github.io/soilKey/reference/fetch_sali.md),
[`benchmark_sali`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_sali.md),
[`sali_citation`](https://hugomachadorodrigues.github.io/soilKey/reference/sali_citation.md),
[`load_natsoil_pedons`](https://hugomachadorodrigues.github.io/soilKey/reference/load_natsoil_pedons.md).
