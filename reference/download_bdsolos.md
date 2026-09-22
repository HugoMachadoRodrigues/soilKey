# Download the BDsolos consulta-publica CSV (experimental, requires chromote)

Drives the Embrapa BDsolos web form via headless Chrome (`chromote`) to
produce a CSV of all profiles + all attributes. Marked
\*\*experimental\*\*: heavy queries (no UF filter) frequently overload
the Embrapa server. Prefer `filter_uf =` batches of one or two states at
a time and stitch the resulting CSVs.

## Usage

``` r
download_bdsolos(
  out_path,
  accept_terms = FALSE,
  filter_uf = NULL,
  attributes = "default",
  timeout_seconds = 600,
  chromote_session = NULL,
  verbose = TRUE
)
```

## Arguments

- out_path:

  File path for the downloaded CSV.

- accept_terms:

  Logical. Must be `TRUE` to proceed; the function aborts otherwise.
  Documents informed consent to the BDsolos terms (personal/academic
  use, ABNT citation).

- filter_uf:

  Optional 2-letter UF code (e.g. `"RJ"`, `"SC"`). Strongly recommended
  – the full-table query often times out.

- attributes:

  Character vector. Which attribute groups to request. Defaults to the
  full SiBCS-classification-relevant set (Identificacao + Localizacao +
  Classificacao for Pontos de Amostragem, Identificacao + Morfologicas +
  Fisicas + Quimicas for Horizontes; Mineralogicas excluded for
  performance). Pass `"all"` to include Mineralogicas.

- timeout_seconds:

  Total timeout for the AJAX query. Default 600 (10 min).

- chromote_session:

  Optional pre-built
  [`chromote::ChromoteSession`](https://rstudio.github.io/chromote/reference/ChromoteSession.html).
  Useful to share a session across calls.

- verbose:

  If `TRUE` (default), prints progress.

## Value

File path to the downloaded CSV (invisible).

## Details

Per the Embrapa terms-of-use, the data is licensed for personal /
academic use and publications must cite the source per ABNT. **Set
`accept_terms = TRUE` to acknowledge this and let the function click
"Concordo" on your behalf.**

## See also

[`load_bdsolos_csv`](https://hugomachadorodrigues.github.io/soilKey/reference/load_bdsolos_csv.md),
[`inspect_bdsolos_csv`](https://hugomachadorodrigues.github.io/soilKey/reference/inspect_bdsolos_csv.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Single UF (fast, recommended)
download_bdsolos("soil_data/bdsolos/RJ.csv",
                  accept_terms = TRUE,
                  filter_uf    = "RJ")

# Stitch multiple UFs
for (uf in c("RJ", "SP", "MG", "ES")) {
  download_bdsolos(file.path("soil_data/bdsolos",
                               paste0(uf, ".csv")),
                    accept_terms = TRUE, filter_uf = uf)
}

# Then load all of them
csvs <- list.files("soil_data/bdsolos", "\\.csv$", full.names = TRUE)
all_pedons <- unlist(lapply(csvs, load_bdsolos_csv), recursive = FALSE)
length(all_pedons)
} # }
```
