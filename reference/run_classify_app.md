# Launch the soilKey interactive classification Shiny app

Opens a local Shiny app ("Pro") that drives the soilKey pipeline from a
browser – no R code required: build a pedon from a canonical fixture, a
CSV upload, or an interactive horizon editor; classify under WRB 2022 /
SiBCS 5 / USDA ST 13 with the full key trace; run VLM photo extraction,
OSSL spectral gap-fill, the SoilGrids spatial prior, an interactive
leaflet map that queries the class prior at a clicked point, and a
Monte-Carlo robustness analysis; and download a cross-system HTML or PDF
report. The interface is bilingual (English / Portuguese; see `lang`).

## Usage

``` r
run_classify_app(
  ui = c("pro", "classic"),
  lang = c("en", "pt"),
  port = NULL,
  launch.browser = TRUE,
  ...
)
```

## Arguments

- ui:

  Kept for back-compatibility. `"pro"` (default) launches the
  professional multi-tab app. `"classic"` – the original single-page
  uploader – was **retired in v0.9.117**; passing it now emits a
  deprecation warning and launches the Pro app instead.

- lang:

  Initial interface language: `"en"` (default) or `"pt"` (Brazilian
  Portuguese). Can also be switched live from the app's navbar.

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

Needs the optional packages bslib, shinyWidgets, plotly and leaflet (all
in `Suggests`); the function raises a clear, copy-pasteable error if any
are missing.

## Examples

``` r
if (FALSE) { # \dontrun{
run_classify_app()              # professional multi-tab app (English)
run_classify_app(lang = "pt")   # interface em portugues
} # }
```
