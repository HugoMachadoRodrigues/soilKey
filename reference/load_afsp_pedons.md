# Load Africa Soil Profiles (AfSP) v1.2 as PedonRecord objects

Reads the AfSP DBase tables shipped inside `AF-AfSP1.2.zip`
(downloadable from `https://files.isric.org/public/afsp/AF-AfSP1.2.zip`)
and converts each profile + its horizons to a soilKey
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
Filters to profiles with a populated WRB 2006 RSG code (i.e.\\
classifiable; AfSP has ~7000 of these of the total 18,533).

## Usage

``` r
load_afsp_pedons(
  afsp_dir,
  max_n = NULL,
  countries = NULL,
  wrb_codes = NULL,
  verbose = TRUE
)
```

## Arguments

- afsp_dir:

  Directory containing the extracted AfSP DBase tables
  (`AfSP012Qry_Profiles.dbf`, `AfSP012Qry_Layers.dbf`).

- max_n:

  Optional integer; take a random sample of this size from the
  classifiable profiles.

- countries:

  Optional character vector of ISO country codes to keep (e.g.\\
  `c("MW", "ET", "TZ")`).

- wrb_codes:

  Optional character vector of WRB 2006 RSG codes to keep (e.g.\\
  `c("VR", "FR", "AC")`).

- verbose:

  Print progress.

## Value

A list of
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
objects.

## References

Leenaars, J. G. B., van Oostrum, A. J. M., & Ruiperez Gonzalez, M.
(2014). Africa Soil Profiles Database, Version 1.2. ISRIC Report
2014/01. ISRIC – World Soil Information, Wageningen. Project page:
`https://isric.org/projects/africa-soil-profiles-database-afsp`.
