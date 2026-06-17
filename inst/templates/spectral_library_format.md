# Bringing a Brazilian Vis-NIR / MIR + labels dataset into soilKey

This is the input contract for the spectral-ingestion scaffolding
(`read_spectral_library()`, `pedons_from_spectral_table()`,
`benchmark_spectral_fill()`, and `gapfill = list(method = "spectra")`). When you
have a real dataset, shape it like this and everything downstream works with no
code change.

## Two tables, joined by an `id` column

### 1. Reflectance — one of:

**Wide** (one row per sample; columns named by wavelength in nm):

```
id,   350,  360,  ...,  2500
P001, 0.082,0.085,...,  0.412
P002, 0.075,0.079,...,  0.398
```

**Long** (one row per sample × wavelength):

```
id,   wavelength_nm, reflectance
P001, 350,           0.082
P001, 360,           0.085
```

- Reflectance may be a fraction (0–1) or percent (0–100); `normalize = "auto"`
  divides by 100 when it looks like percent.
- MIR works identically — just pass MIR wavenumbers/wavelengths and use a MIR
  calibration library.
- Heterogeneous instrument grids? pass `resample_to = 350:2500` (or any grid)
  to linearly resample every spectrum onto a common axis.

### 2. Metadata — one row per sample, an `id` plus lab attributes + label(s):

```
id,   argila, silte, areia, ph, carbono, ctc, reference_sibcs,            lat,    lon
P001, 42,     18,    40,    5.1, 1.8,    7.2, ARGISSOLO VERMELHO ...,     -21.5,  -41.8
```

**Header auto-mapping** (case-insensitive; Portuguese included) — override any of
these with `property_map` / `label_map`:

| canonical | accepted headers |
|---|---|
| `clay_pct`   | clay, **argila**, clay_g_kg |
| `sand_pct`   | sand, **areia** |
| `silt_pct`   | silt, **silte** |
| `cec_cmol`   | cec, **ctc**, t_value |
| `bs_pct`     | bs, **v**, v_pct, sat_bases |
| `ph_h2o`     | ph, ph_agua, ph_water |
| `oc_pct`     | oc, soc, **carbono**, c_org |
| `fe_dcb_pct` | fe_dcb, **ferro_dcb**, fed |
| `caco3_pct`  | caco3, **carbonato** |
| label `sibcs_ordem` | reference_sibcs, **ordem** |
| label `wrb_rsg`     | wrb, rsg, reference_wrb |
| label `usda_order`  | usda, order, reference_st |

Optional: `top_cm`/`bottom_cm` (or `prof_sup`/`prof_inf`, `topo`/`base`) for
multi-horizon profiles, and `lat`/`lon`.

## Usage once the data is in hand

```r
library(soilKey)

# (a) calibration library for the spectral models / neighbour search
lib <- read_spectral_library("refl.csv", read.csv("meta.csv"), id_col = "id")

# (b) measure the accuracy lift the spectra buy (the data-blocked number)
res <- benchmark_spectral_fill("refl.csv", read.csv("meta.csv"),
                               system = "sibcs", folds = 5, method = "mbl")
res$accuracy_off; res$accuracy_on; res$delta

# (c) classify a new field pedon that has only a scan
peds <- pedons_from_spectral_table("refl.csv", read.csv("meta.csv"))
classify_sibcs(peds[[1]],
               gapfill = list(method = "spectra", ossl_library = lib,
                              fill_method = "mbl"))
```

For multi-horizon profiles, add a `profile_col` (e.g. `profile_id`) so samples
stack into one pedon: `pedons_from_spectral_table(..., profile_col = "profile_id")`.

## Notes

- The taxonomic key is never delegated to the spectral model: spectra only fill
  *attributes* (carrying `source = "predicted_spectra"`, grade B), and the
  deterministic key then decides. Default classification is unchanged when
  `gapfill` is unset.
- The bundled `ossl_demo_sa` is **synthetic** — for paper-grade numbers use a
  real library (your BR dataset, or `download_ossl_subset()`).
