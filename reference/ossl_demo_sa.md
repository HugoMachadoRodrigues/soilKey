# Synthetic OSSL South America demo subset

A small, deterministic, OSSL-shaped artefact for use in vignettes,
examples and tests when the real Open Soil Spectral Library data is not
available (no network, sensitive deployment, CI). The object has the
canonical `list(Xr, Yr, metadata)` shape consumed by
[`predict_ossl_mbl`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_ossl_mbl.md)
/
[`fill_from_spectra`](https://hugomachadorodrigues.github.io/soilKey/reference/fill_from_spectra.md),
so the in-package demo path is identical to the real-data path.

## Usage

``` r
ossl_demo_sa
```

## Format

A list with three elements:

- `Xr`:

  Numeric matrix, 80 rows (synthetic profiles) x 2151 columns
  (wavelengths 350-2500 nm). Reflectance values in \[0.05, 0.85\].

- `Yr`:

  Data frame, 80 rows x 9 columns (`clay_pct`, `sand_pct`, `silt_pct`,
  `cec_cmol`, `bs_pct`, `ph_h2o`, `oc_pct`, `fe_dcb_pct`, `caco3_pct`).
  Property ranges follow the OSSL global summary statistics.

- `metadata`:

  Named list with provenance information (`region`, `n_profiles`,
  `snapshot`, `seed`, `note`, ...).

## Source

Synthetic; built by `data-raw/build_ossl_demo.R` with seed 20260430. The
OSSL property ranges that drove the simulation come from Sanderman, J.
*et al.* (2024), *Open Soil Spectral Library*,
<https://soilspectroscopy.org/>.

## Details

This is a **synthetic** placeholder: the spectra are generated from a
tropical-soil baseline plus property-correlated absorption bands (1400
nm OH-water, 1900 nm clay-OH, 2200 nm Al-OH, 900 nm Fe-oxide) with
deterministic noise. It is *not* a substitute for real OSSL
measurements. For paper-grade work, populate a real OSSL artefact via:


      ossl_lib <- download_ossl_subset(region = "south_america")

Re-build the demo with `source("data-raw/build_ossl_demo.R")`.

## Examples

``` r
data(ossl_demo_sa)
dim(ossl_demo_sa$Xr)
#> [1]   80 2151
#> [1]   80 2151
head(ossl_demo_sa$Yr)
#>   clay_pct sand_pct  silt_pct  cec_cmol   bs_pct   ph_h2o    oc_pct fe_dcb_pct
#> 1 51.12727 26.36293  9.933215  1.709790 54.60047 6.786708 0.8673316   2.059861
#> 2 30.62185 27.79992 37.570483 14.413831 58.24193 6.281825 1.3189863   4.060600
#> 3 29.88843 30.69563 33.931396  4.113563 27.34664 5.104516 1.4601632   6.356726
#> 4 75.08242 13.04921 17.215618  2.705566 30.37244 6.384495 0.9846567   5.511491
#> 5 27.30663 40.20213 14.251883  3.807958 34.64959 5.553904 1.5314590   3.884158
#> 6 52.19720 22.55893 34.456830  4.106666 29.00926 5.789555 0.8038034   5.912470
#>   caco3_pct
#> 1 0.4831520
#> 2 0.3862360
#> 3 0.3492779
#> 4 0.7161103
#> 5 0.1526625
#> 6 0.1480898

if (FALSE) { # \dontrun{
# Use it as the ossl_library argument to predict_ossl_mbl():
pedon <- make_synthetic_pedon_with_spectra()
fill_from_spectra(pedon,
                  library      = "ossl",
                  method       = "mbl",
                  ossl_library = ossl_demo_sa)
} # }
```
