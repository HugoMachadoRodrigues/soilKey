# Download NatSoil sites and profiles (CSIRO)

Reads NatSoil, the national soil site database of CSIRO, through its
public API and saves the JSON it returns in `dir`: first every public
site (pages of `page_size` sites, without horizons, in `dir/sites`),
then, for each site whose licence allows external use (and, by default,
that carries an order of the Australian Soil Classification), its
horizons, samples and laboratory results (one file per site in
`dir/profiles`). Files already in `dir` are not downloaded again, so an
interrupted run can be resumed. The data are licensed per site, most
under CC BY 4.0; cite them as each site's `citation` field says (kept in
`site$citation` by
[`load_natsoil_pedons`](https://hugomachadorodrigues.github.io/soilKey/reference/load_natsoil_pedons.md)).

## Usage

``` r
fetch_natsoil(
  dir,
  classified_only = TRUE,
  max_sites = Inf,
  page_size = 200L,
  pause = 0.3,
  verbose = TRUE
)
```

## Arguments

- dir:

  Folder for the downloaded files (created when missing).

- classified_only:

  Download profiles only for sites with an ASC order.

- max_sites:

  Maximum number of profiles to download in this call.

- page_size:

  Sites per page in the site pass.

- pause:

  Seconds to wait between requests.

- verbose:

  Print progress.

## Value

Invisibly, a data frame of the sites selected for the profile pass:
`agency_code`, `proj_code`, `s_id`, the ASC order given (`asc_order`),
`file` and whether it is on disk (`fetched`).

## Details

The whole site pass is about 150 requests and the profile pass one
request per site (about 9,800 with an ASC order), with a pause between
them.

## References

CSIRO. CSIRO National Soil Site Database (NatSoil).
https://doi.org/10.25919/sg5w-ny87

## See also

[`load_natsoil_pedons`](https://hugomachadorodrigues.github.io/soilKey/reference/load_natsoil_pedons.md),
[`benchmark_natsoil`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_natsoil.md).

## Examples

``` r
if (FALSE) { # \dontrun{
fetch_natsoil("natsoil")
peds <- load_natsoil_pedons("natsoil")
benchmark_natsoil(peds)
} # }
```
