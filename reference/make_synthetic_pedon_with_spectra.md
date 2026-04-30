# Build a synthetic PedonRecord with attached spectra (testing aid)

Generates a small, deterministic
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
with `n_horizons` horizons and a Vis-NIR spectral matrix (`350:2500`
nm). Useful for exercising
[`fill_from_spectra`](https://hugomachadorodrigues.github.io/soilKey/reference/fill_from_spectra.md)
in tests and vignettes.

## Usage

``` r
make_synthetic_pedon_with_spectra(
  n_horizons = 5L,
  wavelengths = 350:2500,
  seed = 1L
)
```

## Arguments

- n_horizons:

  Integer number of horizons (default 5).

- wavelengths:

  Integer vector of wavelengths (default `350:2500`).

- seed:

  Integer seed for the RNG used to generate the spectra.

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
with a `$spectra$vnir` matrix attached.
