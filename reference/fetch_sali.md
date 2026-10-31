# Download Queensland SALI sites, reports and laboratory data

Reads Queensland's Soil and Land Information system (SALI) from its
public services and saves what they return in `dir`: the site layer
(Queensland Spatial, MapServer layer 101, the sites with an Australian
Soil Classification label and laboratory data, one JSON file per page in
`dir/layer`), then for each site its report (the SALI4320 site listing
report, a PDF, in `dir/pdf/<PROJECT>_<SITE>.pdf`) and its laboratory
samples with their results from the SALI soil chemistry API (OData
`Samples` with `SiteLabMethodResults`, in
`dir/lab/<PROJECT>_<SITE>_samples.json`).
[`read_sali_pedons`](https://hugomachadorodrigues.github.io/soilKey/reference/read_sali_pedons.md)
reads the folder.

## Usage

``` r
fetch_sali(
  dir,
  sites = NULL,
  cap_bytes = 150000,
  workers = 1L,
  pause = 1,
  max_sites = Inf,
  verbose = TRUE
)
```

## Arguments

- dir:

  Folder for the downloaded files (created when missing).

- sites:

  Optional sites to download: a data frame with `project_code` and
  `site_id`, or a character vector of `"<PROJECT>_<SITE>"`. By default
  every site of the layer whose label names an ASC order (the layer's
  "Unclassified" sites, which
  [`read_sali_pedons`](https://hugomachadorodrigues.github.io/soilKey/reference/read_sali_pedons.md)
  leaves out, are not downloaded).

- cap_bytes:

  Largest report downloaded, in bytes (150 KB).

- workers:

  Number of processes downloading at once
  ([`parallel::mclapply`](https://rdrr.io/r/parallel/mclapply.html),
  each taking every `workers`-th site; always 1 on Windows).

- pause:

  Seconds to wait after each request (in each process).

- max_sites:

  Maximum number of sites downloaded in this call.

- verbose:

  Print progress.

## Value

Invisibly, a data frame of the sites selected: `project_code`,
`site_id`, `asc_label`, `stem`, `pdf` (`"ok"`, `"oversize"`, `"failed"`
or `NA` when not attempted in this call) and `lab` (`"ok"`, `"failed"`
or `NA`).

## Details

Some reports embed full-size site photographs (one was over 50 MB): a
report is downloaded only up to `cap_bytes`; a larger one is aborted as
soon as it passes the cap, marked by a file
`<PROJECT>_<SITE>.pdf.oversize` and not tried again. Files already in
`dir` (and oversize marks) are not downloaded again, so an interrupted
run can be resumed; a failed request is tried again on the next run.
Each request waits `pause` seconds, retries with a growing backoff, and
the run stops (an error) on any page that asks for a login, a key or the
acceptance of terms, or that a web application firewall rejected. The
report host began refusing requests after some 14,000 of them (2
processes, a 1 s pause), so fetch in batches (`max_sites`) rather than
all at once. Requests go only to the layer of Queensland Spatial and,
per site, to the report host and the soil chemistry API; the report
address is built from the project code and site number, not taken from
the data.

The data are licensed under CC BY 4.0; cite them as
[`sali_citation`](https://hugomachadorodrigues.github.io/soilKey/reference/sali_citation.md)
says.

## References

Queensland Government. Soil and Land Information (SALI).
https://www.qld.gov.au/environment/land/soil/soil-data

## See also

[`read_sali_pedons`](https://hugomachadorodrigues.github.io/soilKey/reference/read_sali_pedons.md),
[`benchmark_sali`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_sali.md),
[`sali_citation`](https://hugomachadorodrigues.github.io/soilKey/reference/sali_citation.md).

## Examples

``` r
if (FALSE) { # \dontrun{
fetch_sali("sali", max_sites = 20)
peds <- read_sali_pedons("sali")
benchmark_sali(peds)
} # }
```
