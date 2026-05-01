# Load NCSS / KSSL pedons with reference USDA Soil Taxonomy classification

Reads the KSSL pedon CSV export (typically named `NCSS_Pedon_Layer.csv`
or similar) plus the lab-data CSV, joins on `pedon_key`, and assembles a
list of `PedonRecord` objects. The published USDA Soil Taxonomy
classification (from the `Series` or `Subgroup` field) is attached as
`pedon$site$reference_usda`.

## Usage

``` r
load_kssl_pedons(pedon_csv, layer_csv, head = NULL, verbose = TRUE)
```

## Arguments

- pedon_csv:

  Path to the pedon-level CSV (one row per profile, with site-level
  metadata + classification).

- layer_csv:

  Path to the layer-level CSV (one row per horizon, with horizon
  properties).

- head:

  Optional integer; if not `NULL`, returns only the first `head` pedons
  (useful for parser validation).

- verbose:

  If `TRUE` (default), emits a summary of the load.

## Value

A list of
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
objects.

## Details

KSSL is the de-facto standard for validating USDA Soil Taxonomy keys
(~50k profiles, lab-grade analytical data, professional pedon
descriptions). Get the export from
<https://ncsslabdatamart.sc.egov.usda.gov/>.
