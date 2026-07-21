# Read a horizon spreadsheet (CSV/TSV) into a PedonRecord

The everyday entry point for anyone who has a soil profile in a
spreadsheet: one row per horizon, one column per attribute, using
soilKey's canonical column names (`top_cm`, `bottom_cm`, `designation`,
`clay_pct`, `ph_h2o`, `munsell_hue_moist`, ...). The full list is
`names(horizon_column_spec())`; the quickest start is the bundled
template
`system.file("extdata", "perfil_exemplo.csv", package = "soilKey")`.

## Usage

``` r
read_pedon_csv(file, site = NULL, sep = "auto")
```

## Arguments

- file:

  Path to a `.csv` (comma) or `.tsv` (tab) file, one row per horizon.

- site:

  Optional named list of site metadata (see
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)).
  Defaults to `list(id = <file base name>)`.

- sep:

  Field separator. `"auto"` (default) uses a tab for `.tsv` files and a
  comma otherwise.

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Details

Only recognised columns are used (any extras are carried through
untouched), so a messy export still works - soilKey simply uses the
columns it understands. Site metadata (id, lat/lon, soil moisture /
temperature regime) is optional and passed via `site`; without it the
profile still classifies, just with less specificity where site data
would have refined the name.

## See also

[`classify_csv`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_csv.md)
to go straight from a file to the three classifications;
[`classify_all`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_all.md);
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Examples

``` r
f <- system.file("extdata", "perfil_exemplo.csv", package = "soilKey")
pedon <- read_pedon_csv(f)
classify_all(pedon)$summary
#>                                         wrb
#> 1 Chromic Ferralsol (Clayic, Ochric, Rubic)
#>                                                          sibcs           usda
#> 1 Latossolos Vermelhos Distróficos típicos, argilosa, moderado Typic Hapludox
```
