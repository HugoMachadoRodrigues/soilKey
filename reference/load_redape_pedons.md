# Load curated soil profiles from the Embrapa Redape GeoTab dataset

Reads the structured JSON files (one profile per file) published by Vaz
et al. 2023 at the Embrapa Redape repository (DOI `10.48432/PYKKA7`) and
converts each one to a soilKey
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Usage

``` r
load_redape_pedons(json_dir, max_n = NULL, verbose = TRUE)
```

## Arguments

- json_dir:

  Directory containing the GeoTab JSON files (or a character vector of
  file paths).

- max_n:

  If non-`NULL`, take a random sample of this size.

- verbose:

  Print progress (default `TRUE`).

## Value

A list of
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
objects.

## Details

The dataset is unique in two ways:

- Every profile was hand-reviewed by experienced pedologists (the
  curation note and author list are preserved on each pedon site
  record), so it is suitable as a gold-standard benchmark.

- Unlike BDsolos, all profiles ship the full exchange complex (Ca, Mg,
  K, Na, Al *and H*), so `cec_cmol` (Valor T = S + H + Al) is computed
  directly without any fallback option.

## Reference

Vaz, G. J., Silva Jr, A. F., & Silva Neto, L. de F. da (2023). Brazilian
soil data for taxonomic classification. Redape, V1.
<https://doi.org/10.48432/PYKKA7>.

## See also

[`download_redape_dataset`](https://hugomachadorodrigues.github.io/soilKey/reference/download_redape_dataset.md),
[`benchmark_redape`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_redape.md).
