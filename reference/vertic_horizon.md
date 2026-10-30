# Vertic horizon (WRB 2022 Ch 3.1.40)

A clay-rich subsurface horizon with slickensides or wedge-shaped
aggregates, formed by shrinking and swelling. Used by Vertisols.

## Usage

``` r
vertic_horizon(
  pedon,
  min_clay = 30,
  min_thickness = 25,
  min_le_cm = 6,
  le_max_depth_cm = 100,
  min_crack_width_cm = 0.5,
  reading = c("wrb2022", "usda")
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_clay:

  Numeric threshold or option (see Details).

- min_thickness:

  Numeric threshold or option (see Details).

- min_le_cm:

  Minimum LE sum (cm) for the COLE-based path of `reading = "usda"`
  (default 6, per KST 13ed Ch 16).

- le_max_depth_cm:

  Depth window (cm) for the COLE-based path of `reading = "usda"`
  (default 100).

- min_crack_width_cm:

  Minimum shrink-swell crack width (cm). Defaults to 0.5 (WRB/USDA); the
  SiBCS `horizonte_vertico` wrapper passes 1.0 per Embrapa (2018) Cap 2
  p.73.

- reading:

  `"wrb2022"` (default): the criteria of WRB 2022 Ch 3.1.40. `"usda"`:
  the reading of the USDA Vertisols and the SiBCS horizonte vertico,
  with the KST 13 COLE path (see Details).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
recording whether the diagnostic is present, the qualifying layers, and
the supporting evidence.

## Details

Since v0.9.311 the default reading (`reading = "wrb2022"`) follows the
diagnostic criteria of WRB 2022 Ch 3.1.40 (p. 70). Mineral material
with:

1.  \\= 30% clay;

2.  wedge-shaped aggregates (a `structure_type` that names them) or
    slickensides on \\= 10% of the aggregate surfaces (`slickensides`
    "common" or more);

3.  shrink-swell cracks: `cracks_width_cm` \\= 0.5, or, where the width
    is not recorded, slickensides "common" or more or an `ss`
    designation as evidence of them;

4.  a thickness of \\= 25 cm, of adjoining layers that meet 1 to 3 taken
    together.

Each criterion is read per layer as met, not met or unrecorded. The
horizon is `NA` only when layers that might meet them (a criterion
unrecorded) could make up the thickness. Organic layers and continuous
rock are not part of it. WRB gives the COLE (\\= 0.06) as additional
information only, so this reading does not use `cole_value`.

`reading = "usda"` keeps the reading that the USDA Vertisols and the
SiBCS horizonte vertico are built on, as it was before v0.9.311:
slickensides "common" or more and cracks \\= `min_crack_width_cm` in one
layer of `min_thickness`, OR the KST 13 linear-extensibility
alternative: summed `cole_value * thickness` over the upper
`le_max_depth_cm` \\= `min_le_cm` (KST 13ed Ch 16, p. 343).

## v0.9.72 designation morphological inference (opt-in)

Field-described Brazilian Vertissolos profiles (e.g.\\ the Embrapa
Redape curated dataset) encode vertic morphology via a `v` master-letter
modifier in the horizon designation (`Bv`, `Bvk1`, `Cv`, `Cvz`) without
recording `slickensides` class or `shrink_swell_cracks_cm` as numeric
inputs. With `options(soilKey.vertic_designation_inference = TRUE)` the
function accepts a layer as vertic when the criteria of the reading fail
or are NA AND the layer has `clay_pct >= min_clay` AND its designation
matches a `v` master-letter modifier. Default is `FALSE`.
