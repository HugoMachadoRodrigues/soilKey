# Curated index of FEBR datasets that carry Munsell colors

Returns a data.frame listing FEBR dataset IDs that have at least one
Munsell-related column populated in their `camada` table, with metadata:
`n_horizons`, `n_finite_munsell`, `coverage`, `column_pattern`.

## Usage

``` r
febr_index_munsell(min_coverage = 0.1, refresh = FALSE, verbose = TRUE)
```

## Arguments

- min_coverage:

  Drop datasets whose Munsell coverage (fraction of horizons with non-NA
  hue) is below this. Default 0.1.

- refresh:

  Logical. If `TRUE`, re-scan FEBR over the network instead of using the
  bundled May-2026 cache.

- verbose:

  If `TRUE` (default), prints a one-line summary.

## Value

A `data.frame` sorted by `n_finite_munsell` descending.

## Details

Backed by a precomputed cache shipped in `R/sysdata.rda`
(`.FEBR_MUNSELL_INDEX`; results of the May 2026 scan over 249 datasets).
On first call after install, returns the cache instantly. Pass
`refresh = TRUE` to re-scan FEBR live (slow, network-dependent; updates
the in-memory copy but does not modify the bundled cache).

## See also

[`read_febr_pedons`](https://hugomachadorodrigues.github.io/soilKey/reference/read_febr_pedons.md).
