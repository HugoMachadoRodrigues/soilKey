# Launch the soilKey interactive classification Shiny app

Opens a local Shiny app that drives the soilKey pipeline from a browser
– no R code required. Two interfaces are available:

## Usage

``` r
run_classify_app(
  ui = c("pro", "classic"),
  port = NULL,
  launch.browser = TRUE,
  ...
)
```

## Arguments

- ui:

  One of `"pro"` (default) or `"classic"`. See above.

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

- `"pro"` (default):

  A professional multi-tab app: build a pedon from a canonical fixture,
  a CSV upload, or an interactive horizon editor; classify under WRB
  2022 / SiBCS 5 / USDA ST 13 with the full key trace; run VLM photo
  extraction, OSSL spectral gap-fill, the SoilGrids spatial prior, an
  interactive leaflet map that queries the class prior at a clicked
  point, and a Monte-Carlo robustness analysis; and download a
  cross-system HTML or PDF report. Needs the optional packages bslib,
  shinyWidgets, plotly and leaflet.

- `"classic"`:

  The original single-page uploader (v0.9.39): drag-and-drop a CSV and
  get the three classifications side-by-side. Needs only shiny and DT.

All optional packages are listed in `Suggests`; the function raises a
clear, copy-pasteable error if any are missing.

## Examples

``` r
if (FALSE) { # \dontrun{
run_classify_app()                 # professional multi-tab app
run_classify_app(ui = "classic")   # legacy single-page uploader
} # }
```
