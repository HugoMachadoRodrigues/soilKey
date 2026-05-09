# Load the bundled WoSIS stratified RSG-balanced sample (v0.9.73)

Returns a 130-profile snapshot of WoSIS GraphQL data pulled on
2026-05-09 with \*\*stratified sampling by WRB Reference Soil Group\*\*:
5 profiles per RSG across 26 RSGs (Acrisol, Andosol, Arenosol, Calcisol,
Cambisol, Chernozem, Cryosol, Ferralsol, Fluvisol, Gleysol, Gypsisol,
Histosol, Kastanozem, Leptosol, Luvisol, Nitisol, Phaeozem, Planosol,
Plinthosol, Podzol, Regosol, Solonchak, Solonetz, Stagnosol, Umbrisol,
Vertisol).

## Usage

``` r
load_wosis_stratified_sample()
```

## Value

A list with:

- `pedons`: list of 130 `PedonRecord` objects.

- `meta`: named integer vector of profiles per RSG.

- `pulled_on`: pull date.

- `endpoint`: ISRIC GraphQL endpoint URL.

- `filter`: pull strategy metadata.

- `n_pulled`: 130.

## Details

This is the recommended cache for global WRB benchmarking. Compared to
[`load_wosis_sample()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_wosis_sample.md)
(40 SA-only profiles, mostly Solonetz and Phaeozem from Argentina), the
stratified sample provides:

- Even coverage across the 26 most important RSGs.

- Richer analytical attributes – CEC available on 26 ECEC on 37 in the
  SA snapshot).

- Geographic diversity (Angola, Brazil, USA, China, Russia, South
  Africa, Indonesia, Argentina, etc.).

First-ever benchmark on this sample (soilKey v0.9.73, full v0.9.69-72
fallback stack):

- Overall top-1 accuracy: 16.2\\

- Histosol 100\\ from 20\\ Cambisol 60\\

- 18 RSGs at 0\\ expose (Munsell colours, base saturation, sodium for
  Solonetz, slickensides for Vertisols, etc.). Documented data ceiling.

For the live API, call `run_wosis_benchmark_graphql()` or the
`read_wosis_profiles_graphql(wrb_rsg = "...", n_max = N)` helper (small
RSG-filtered queries are tractable; large unfiltered pulls time out as
of 2026-05).

## Reference

Batjes, N. H., Ribeiro, E., van Oostrum, A. (2020). Standardised soil
profile data to support global mapping and modelling (WoSIS snapshot
2019). *Earth System Science Data*, 12, 299-320.
[doi:10.5194/essd-12-299-2020](https://doi.org/10.5194/essd-12-299-2020)
.

## Examples

``` r
if (FALSE) { # \dontrun{
s <- load_wosis_stratified_sample()
length(s$pedons)
#> 130
table(vapply(s$pedons, function(p) p$site$wosis_rsg, character(1)))
#> 5 of each: Acrisol, Andosol, ... Vertisol
} # }
```
