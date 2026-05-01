# Load Embrapa dadosolos pedons with reference SiBCS classification

Reads the Embrapa BDsolos CSV export (or the dadosolos R package data
frame, if present). Assembles a list of `PedonRecord` objects with the
SiBCS classification attached as `pedon$site$reference_sibcs`.

## Usage

``` r
load_embrapa_pedons(csv_path, head = NULL, verbose = TRUE)
```

## Arguments

- csv_path:

  Path to the BDsolos CSV (long format: one row per horizon, with a
  profile-id key and per-profile classification).

- head:

  Optional integer for parser validation.

- verbose:

  If `TRUE` (default), emits a summary.

## Value

A list of
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
objects.

## Details

The dadosolos / BDsolos archive ships with ~5k profiles in PT-BR with
full SiBCS classification, lab data, and horizon morphology – the
primary validation set for Brazilian-context use. Available from
<https://www.bdsolos.cnptia.embrapa.br/>.
