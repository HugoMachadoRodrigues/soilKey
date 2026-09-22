# Load the LUCAS Soil 2018 Topsoil release as a list of PedonRecord objects

Reads the canonical European Soil Data Centre (ESDAC) release of LUCAS
Soil 2018 Topsoil chemistry as published in the JRC report (ESDAC
dataset
`https://esdac.jrc.ec.europa.eu/content/lucas-2018-topsoil-data`). The
release ships ~18,984 European topsoil samples at 0-20 cm with pH (H2O
and CaCl2), EC, OC, CaCO3, P, N, K and oxalate-extractable Al / Fe; a
separate `BulkDensity_2018_final-2.csv` carries bulk density at 0-10 /
10-20 / 20-30 / 0-20 cm for ~6,272 of those points and is joined
automatically when present.

## Usage

``` r
load_lucas_soil_2018(
  path,
  attach_bulk_density = TRUE,
  countries = NULL,
  max_n = NULL,
  verbose = TRUE
)
```

## Arguments

- path:

  Folder containing `LUCAS-SOIL-2018.csv` (typically
  `<root>/LUCAS-SOIL-2018-data-report-readme-v2/LUCAS-SOIL-2018-v2/`).

- attach_bulk_density:

  If `TRUE` (default), joins the `BulkDensity_2018_final-2.csv` sister
  file on `POINTID` when present.

- countries:

  Optional character vector of NUTS_0 codes (e.g. `c("ES", "FR")`) to
  filter pedons. Default `NULL` (all countries).

- max_n:

  Optional integer cap on the number of pedons returned (after country
  filter). Useful for development.

- verbose:

  If `TRUE` (default), prints a summary line.

## Value

A list of
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
objects (one per LUCAS point). Each pedon has a `site$id` matching the
LUCAS `POINTID`, `site$lat` / `site$lon` in WGS84, and either one or two
horizons (the second being 20-30 cm when the subsoil OC / CaCO3 columns
are populated). Provenance entries from the loader use
`source = "measured"`.

## Details

What's NOT in the release (and how to fill it):

- **Texture (clay / sand / silt)** – not in this CSV. Pass
  `benchmark_lucas_2018(..., fill_texture_from = "soilgrids")` to fill
  from ISRIC SoilGrids 250m via
  [`lookup_soilgrids`](https://hugomachadorodrigues.github.io/soilKey/reference/lookup_soilgrids.md).

- **Munsell colors** – not collected by LUCAS Soil 2018. If the user has
  Vis-NIR spectra (release separate ~83 GB), use
  [`predict_munsell_from_spectra`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_munsell_from_spectra.md)
  (v0.9.47).

- **Vis-NIR spectra** – distributed separately by ESDAC. Once downloaded
  and attached to the pedon's `$spectra`,
  [`predict_from_spectra`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_from_spectra.md)
  (v0.9.46) fills clay / sand / silt / pH / OC / CEC.

- **Taxonomic reference** – not in the LUCAS release;
  [`benchmark_lucas_2018`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_lucas_2018.md)
  attaches the canonical WRB Reference Soil Group via
  [`lookup_esdb`](https://hugomachadorodrigues.github.io/soilKey/reference/lookup_esdb.md)
  (v0.9.44) at the pedon's coordinates.

Unit conversions applied (LUCAS -\> soilKey schema):

- OC, N, CaCO3, Ox_Al, Ox_Fe: g/kg -\>

- EC: mS/m -\> dS/m (\* 0.01)

- P, K: mg/kg unchanged

- pH: unitless

Special LUCAS string values `"< LOD"`, `"<LOD"`, empty cells and
`"n.d."` / `"ND"` are converted to `NA` before numeric coercion.

## See also

[`benchmark_lucas_2018`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_lucas_2018.md),
[`lookup_esdb`](https://hugomachadorodrigues.github.io/soilKey/reference/lookup_esdb.md),
[`lookup_soilgrids`](https://hugomachadorodrigues.github.io/soilKey/reference/lookup_soilgrids.md).

## Examples

``` r
if (FALSE) { # \dontrun{
path <- "soil_data/eu_lucas/LUCAS-SOIL-2018-data-report-readme-v2/LUCAS-SOIL-2018-v2"
pedons <- load_lucas_soil_2018(path, countries = c("ES", "PT"),
                                 max_n = 100)
length(pedons)
pedons[[1]]
} # }
```
