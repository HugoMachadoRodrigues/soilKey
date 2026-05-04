# Launch the soilKey interactive classification Shiny app

Drag-and-drop a CSV (one row per horizon) and get all three
classifications side-by-side, with a downloadable HTML report. Designed
for non-R users (agronomists, students, field workers).

## Usage

``` r
run_classify_app(port = NULL, launch.browser = TRUE, ...)
```

## Arguments

- port:

  Port for the local server. Default lets Shiny choose.

- launch.browser:

  Whether to open the app in the default browser (default `TRUE`).

- ...:

  Additional arguments passed to
  [`runApp`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Value

Invisibly the value returned by
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Details

Requires the optional packages `shiny` and `DT` (both listed in
Suggests). The function raises a clear error if either is missing.

## Examples

``` r
if (FALSE) { # \dontrun{
run_classify_app()
} # }
```
