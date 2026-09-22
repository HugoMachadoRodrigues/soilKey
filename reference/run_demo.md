# Launch the soilKey Shiny demo (one-screen GUI)

Opens a Shiny app that lets a non-coder pick one of the 31 canonical
profiles or upload a small horizons CSV, click **Classify**, and read
the WRB / SiBCS / USDA names plus the deterministic key trace and the
evidence grade. Useful for live demos, classroom teaching, and for
pedologists who want to verify the package on a profile they already
know without writing R code.

## Usage

``` r
run_demo(...)
```

## Arguments

- ...:

  Forwarded to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html) (e.g.
  `port = 4321`, `launch.browser = FALSE`, `host = "0.0.0.0"`).

## Value

Invisibly, the value returned by
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Details

Requires the `shiny` package. The taxonomic key is still deterministic:
no VLM is invoked from the GUI.

## Examples

``` r
if (FALSE) { # \dontrun{
  soilKey::run_demo()
} # }
```
