# Load FEBR datasets as a list of PedonRecord objects

Wraps `febr::readFEBR()` (CRAN package, FEBR v1.9.9+ recommended) and
adapts the returned `camada` (layer) + `observacao` tables to the
soilKey schema. Auto-detects Munsell columns across the ~6 distinct
conventions found in the 200 FEBR datasets that carry color data, parses
PT-BR Munsell strings (`"2,5YR 3/6"`) and converts FEBR's standard units
to soilKey conventions.

## Usage

``` r
read_febr_pedons(
  dataset_codes = c("ctb0039"),
  febr_repo = NULL,
  min_munsell_coverage = 0,
  verbose = TRUE
)
```

## Arguments

- dataset_codes:

  Character vector of FEBR dataset IDs (e.g. `c("ctb0032", "ctb0562")`).
  Pass `"all"` to download every Munsell-bearing dataset; this is heavy
  (network calls per dataset). Default: a small curated sample for
  development.

- febr_repo:

  Optional override for the FEBR repository location, forwarded to
  `febr::readFEBR`.

- min_munsell_coverage:

  Drop pedons whose horizons are *all* missing Munsell. Default 0 (keep
  all); set to 0.5 to keep only pedons with at least 50 horizons having
  a Munsell hue.

- verbose:

  If `TRUE` (default), prints per-dataset join statistics.

## Value

A list of
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
objects with `site$id` = FEBR `observacao_id`, `site$reference_sibcs` =
the surveyor's classification when available, and one horizon per FEBR
`camada` row.

## Details

Per the May 2026 scan, ~80
[`febr_index_munsell`](https://hugomachadorodrigues.github.io/soilKey/reference/febr_index_munsell.md)
to get the curated list of Munsell-bearing dataset IDs.

## See also

[`febr_index_munsell`](https://hugomachadorodrigues.github.io/soilKey/reference/febr_index_munsell.md),
[`load_bdsolos_csv`](https://hugomachadorodrigues.github.io/soilKey/reference/load_bdsolos_csv.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Single dataset (35 perfis, 100% Munsell coverage)
pedons <- read_febr_pedons("ctb0039")

# Multiple datasets
pedons <- read_febr_pedons(c("ctb0032", "ctb0562", "ctb0568"))

# All Munsell-bearing datasets (slow; 200 datasets, ~36k horizons)
all_pedons <- read_febr_pedons("all")
} # }
```
