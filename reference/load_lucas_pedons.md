# Load EU-LUCAS / ESDB pedons with reference WRB classification

Reads the EU-LUCAS topsoil dataset joined with the ESDB profile archive
(the v3 release produced by JRC). Assembles a list of `PedonRecord`
objects with the WRB Reference Soil Group attached as
`pedon$site$reference_wrb`.

## Usage

``` r
load_lucas_pedons(lucas_csv, head = NULL, verbose = TRUE)
```

## Arguments

- lucas_csv:

  Path to the LUCAS topsoil CSV.

- head:

  Optional integer for parser validation.

- verbose:

  If `TRUE` (default), emits a summary.

## Value

A list of
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
objects.

## Details

LUCAS is harvested every 3-6 years on a regular grid; the ESDB
classification is updated synchronously. ~28k profile cells with WRB
labels in the 2015-2018 release.
