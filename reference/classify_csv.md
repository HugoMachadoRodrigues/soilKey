# Classify a horizon spreadsheet in all three systems - one file, one line

The shortest path from a spreadsheet to an answer. Reads `file` with
[`read_pedon_csv`](https://hugomachadorodrigues.github.io/soilKey/reference/read_pedon_csv.md)
and returns the WRB 2022 / SiBCS / USDA names as a one-row `data.frame`.
Missing attributes are handled silently (`on_missing = "silent"`).

## Usage

``` r
classify_csv(
  file,
  site = NULL,
  sep = "auto",
  systems = c("wrb", "sibcs", "usda")
)
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

- systems:

  Character vector of systems to run; any of `"wrb"`, `"sibcs"`,
  `"usda"` (default: all three).

## Value

A one-row `data.frame` with one column per system.

## Details

The full
[`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md)
objects (with the key trace, evidence grade, qualifiers, ...) and the
parsed
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
are attached as attributes `"results"` and `"pedon"` for anyone who
wants to dig deeper.

## See also

[`read_pedon_csv`](https://hugomachadorodrigues.github.io/soilKey/reference/read_pedon_csv.md),
[`classify_all`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_all.md).

## Examples

``` r
f <- system.file("extdata", "perfil_exemplo.csv", package = "soilKey")
classify_csv(f)
#>                                         wrb
#> 1 Chromic Ferralsol (Clayic, Ochric, Rubic)
#>                                                          sibcs           usda
#> 1 Latossolos Vermelhos Distróficos típicos, argilosa, moderado Typic Hapludox
```
