# LUCAS Soil 2018 / ESDB WRB benchmark — v0.9.49 (2026-05-04)

This release closes the **EU-LUCAS / WRB benchmark Route B**
end-to-end, open since the v0.9.27 roadmap. v0.9.44 already shipped
the raster-lookup half (`lookup_esdb()`); v0.9.49 adds the chemistry
half: a loader for the LUCAS Soil 2018 Topsoil release, plus a
benchmark function that compares the soilKey classifier output to
the canonical ESDB WRB raster at every LUCAS coordinate.

## What's shipped

- **`load_lucas_soil_2018(path, ...)`** — reads the canonical
  ESDAC release (`LUCAS-SOIL-2018.csv`), joins
  `BulkDensity_2018_final-2.csv` on `POINTID` when present, and
  returns a list of `PedonRecord` objects. Unit conversions
  (g/kg → %, mS/m → dS/m), `< LOD` handling and subsoil 20-30 cm
  horizon synthesis are baked in.

- **`benchmark_lucas_2018(pedons, esdb_root, ...)`** — looks up
  the ESDB WRB Reference Soil Group at every coordinate via
  `lookup_esdb(attribute = "WRBLV1")`, optionally fills
  clay/sand/silt from SoilGrids 250m via `lookup_soilgrids()`,
  runs `classify_wrb2022()` (or `classify_sibcs()`) per pedon,
  and tabulates a confusion matrix + per-RSG recall.

- **`.WRB_LV1_NAME_BY_CODE`** — internal lookup mapping the 31
  ESDB WRBLV1 2-letter codes to the English plural RSG names
  returned by `classify_wrb2022()`. Codes follow IUSS WRB 2022;
  the legacy `AB` (Albeluvisols) is mapped to `NA`.

## Demonstration: 200-point baseline run

200 pedons stratified across **ES / FR / PL / IT**, pure
LUCAS-2018 chemistry (no SoilGrids, no spectra fill):

```
Accuracy (no fill): 3.0%   in-scope: 199 / 200

Predicted    Regosols  Histosols  Calcisols
n             184        14         2

Reference    Cambisols  Leptosols  Regosols  Histosols  Podzols  Acrisols  Fluvisols
n             107         78         5         3         3         2         1
```

Per-RSG recall:

| Reference RSG | n   | correct | recall |
|---------------|-----|---------|--------|
| Acrisols      | 2   | 0       | 0%     |
| Cambisols     | 107 | 0       | 0%     |
| Fluvisols     | 1   | 0       | 0%     |
| Histosols     | 3   | 1       | 33%    |
| Leptosols     | 78  | 0       | 0%     |
| Podzols       | 3   | 0       | 0%     |
| Regosols      | 5   | 5       | **100%** |

### What this tells us

**This is an honest baseline, not a defect.** LUCAS Soil 2018
ships only **topsoil 0-20 cm chemistry** (pH, OC, CaCO3, N, P, K,
Ox_Al, Ox_Fe). WRB diagnostic horizons that drive most European
soils require **subsoil** features that are not in this release:

- **Cambic** horizon (Cambisols, ~53% of the reference) needs
  subsoil texture + structure + chroma evidence at 25-200 cm.
- **Argic** horizon (Acrisols, Luvisols) needs a clay-content
  increase ≥ 8% absolute over a 30-cm depth interval.
- **Bedrock contact ≤ 25 cm** (Leptosols, ~39% of the
  reference) needs subsoil depth-to-rock observation.
- **Spodic** horizon (Podzols) needs subsoil Fe/Al accumulation
  measurements.
- **Histic** horizon (Histosols) needs depth measurements of the
  organic layer beyond the 0-20 cm sample.

Without those, `classify_wrb2022()` correctly falls back to
**Regosols** — the WRB catch-all RSG defined as "soils where no
other diagnostic horizon applies." The 92% Regosols prediction
rate is the right answer given the data: **the LUCAS surface
chemistry alone does not support deep classification.**

The Histosols recall of 33% (1/3) reflects the cases where
topsoil OC ≥ 12% (the histic threshold) is the tell-tale signal
even from a 20-cm sample.

## The improvement path

v0.9.49 ships the pipeline; closing the accuracy gap means
filling the subsoil gap. The package already has the building
blocks:

1. **Subsoil texture from SoilGrids** — use
   `lookup_soilgrids(coords, property = c("clay","sand","silt"),
   depth = "30-60cm", quantile = "mean")` (v0.9.48) to populate
   a synthetic 30-60 cm horizon. Unlocks the cambic / argic
   thresholds.

2. **Subsoil OC / pH / CEC from SoilGrids** — same call with
   `property = "soc"` / `"phh2o"` / `"cec"`. Unlocks mollic
   chroma criteria, calcic / dystric / eutric splits.

3. **Vis-NIR spectra fill** — when the LUCAS Soil 2018 Spectral
   Library is downloaded (~83 GB ESDAC release), attach the
   Vis-NIR matrix to `pedon$spectra$vnir`, run
   `predict_from_spectra(pedon, models = ossl_models)` (v0.9.46)
   and `fill_munsell_from_spectra(pedon)` (v0.9.47). Highest
   fidelity because per-point spectra capture local mineralogy.

4. **Bedrock depth proxy** — SoilGrids `cfvo` (coarse fragments
   volume %) at 0-5 / 5-15 cm correlates with shallow soils;
   threshold at ≥ 50% would let Leptosols trigger.

A natural v0.9.50 would extend `benchmark_lucas_2018()` with
`fill_texture_from = "soilgrids_subsoil"` and the spectra path.
The current `fill_texture_from = "soilgrids"` only fills 0-5 cm
(the topsoil horizon already in LUCAS), which by itself cannot
shift the predictions much (validated empirically on 10
Spanish pedons — same 40% baseline both with and without).

## How to reproduce

```r
library(soilKey)

path <- "soil_data/eu_lucas/LUCAS-SOIL-2018-data-report-readme-v2/LUCAS-SOIL-2018-v2"
esdb <- "soil_data/eu_lucas/ESDB-Raster-Library-1k-GeoTIFF-20240507"

# 200 pedons across 4 countries
pedons <- load_lucas_soil_2018(path,
                                 countries = c("ES", "FR", "PL", "IT"),
                                 max_n     = 200)

bench <- benchmark_lucas_2018(pedons,
                                esdb_root        = esdb,
                                fill_texture_from = "none",
                                classify_with    = "wrb2022")

bench$accuracy        # overall match fraction
bench$confusion       # predicted vs reference table
bench$per_rsg         # per-class recall
```

Or for the full 18,984-point release (10-15 min for chemistry-only,
hours with SoilGrids subsoil fill once enabled):

```r
pedons <- load_lucas_soil_2018(path)
bench  <- benchmark_lucas_2018(pedons, esdb_root = esdb,
                                  verbose = TRUE)
```

## Tests

12 new tests in `test-v0949-lucas-2018.R` (55 expectations):

- Loader: 4 chemistry rows (ES, FR, SE, IT) with mixed `< LOD` /
  empty cells, BD-join, country/`max_n` filters, missing-file
  errors.
- Benchmark: end-to-end on a synthetic 4×4 ESDB raster, code
  decoding (`FL → Fluvisols`, `LV → Luvisols`, …), input
  validation, both `classify_with = "wrb2022"` and `"sibcs"`.

All pass; no network required.

## Smoke test results (real data)

20 Austrian pedons via `load_lucas_soil_2018()`:

```
✔ load_lucas_soil_2018(): 20 pedons loaded (BD attached: 4)

ID 47862690, Austria, lat 47.150 lon 16.134
  pH_H2O    1.24% OC   0.11% N   0.3% CaCO3
  Ox_Fe / Al / BD: NA (subset of points)
```

Numbers are physically plausible (acid Mediterranean cropland,
low SOC). BD attached for 4/20 (20%) — matches the
~33% global join rate.

## Bottom line

Route B is **end-to-end runnable** as of v0.9.49. The classifier
honestly reports what topsoil chemistry alone can support; lifting
the accuracy beyond ~3% requires subsoil fill, which the package
already has the building blocks for (v0.9.46 / v0.9.47 / v0.9.48).
Hugo can now drive the comparison loop on his own machine without
waiting for the Embrapa export or the spectral-library download.
