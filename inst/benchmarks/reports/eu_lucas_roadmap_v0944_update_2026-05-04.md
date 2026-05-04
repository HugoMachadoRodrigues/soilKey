# EU-LUCAS WRB benchmark — v0.9.44 update (2026-05-04)

Update to `inst/benchmarks/reports/eu_lucas_roadmap_2026-05-03.md`
after Hugo provided three new artefacts:

1. `febr-data-songchao.txt` — Songchao 2022 soil chemistry export
   (2 684 rows, NO Munsell colors, NO taxonomic reference)
2. `EU_LUCAS_2022.csv` (455 MB) + `EU_LUCAS_2022_updated.xlsx`
   (288 MB) — LUCAS 2022 point survey metadata (~338 000 points,
   lat/lon + 306 columns of land-use / photo / GPS metadata, but
   ZERO soil chemistry columns)
3. **`ESDB-Raster-Library-1k-GeoTIFF-20240507/`** — ESDB raster
   release (May 2024), 71 thematic rasters at 1 km resolution
   under LAEA Europe (EPSG:3035), including **`WRBLV1.tif`** and
   **`WRBFU.tif`** with full Value Attribute Tables.

## What v0.9.44 enables

The ESDB Raster Library was the **missing piece** identified in the
v0.9.27 roadmap (Route B: "LUCAS Soil 2018 + ESDB Raster v2 1km
spatial join"). v0.9.44 ships the spatial-join helper:

```r
lookup_esdb(coords, attribute, raster_root)
```

For any WGS84 lat/lon, this returns the value of any of the 71
ESDB attribute rasters (WRBLV1, WRBFU, FAO90LV1, plus 65 thematic
attributes like `OC_TOP`, `BS_SUB`, `PARMADO`, `TEXT`).

This makes ESDB raster a **fourth validation axis** for soilKey:
any European-coordinate `PedonRecord` can be cross-checked against
the canonical 1 km map at its location.

See `inst/benchmarks/reports/esdb_raster_lookup_2026-05-04.md` for
the demonstration on 12 European cities and the API reference.

## Songchao file analysis (no actionable change)

`febr-data-songchao.txt` was inspected for:
- Munsell color columns (hue / value / chroma)
- Taxonomic reference (`taxon_sibcs`, `taxon_wrb`, `taxon_st`)

**Neither is present.** The Songchao columns are:

```
dataset_id, Profile_ID, dataset_license, sampling_date,
Longitude, Latitude, coord_precision (m), sampling_area (m2),
Layer_ID, Upper_Depth (cm), Lower_Depth (cm),
clay, silt, sand, SOC, BD
```

Same data sparseness as the original `febr-superconjunto.txt`:
chemistry-only, no morphology, no surveyor classification.

**Implications:**

- **Cannot fix the v0.9.35 Argissolo Vermelho / Amarelo /
  Vermelho-Amarelo color confusion** (44 cases) -- the surveyor's
  Munsell hue identification is still not in the data. Would
  require a different export from BDsolos or a different dataset
  altogether.
- **Cannot use Songchao for benchmark validation** -- no taxonomic
  reference labels to compare against.
- **Could be useful** as a supplementary lab-data source for OSSL
  spectral-library training (Vis-NIR or MIR predictions for clay /
  sand / silt / OC / BD) if Songchao spectra exist somewhere; the
  CSV alone is not directly actionable for the soilKey classifier.

No code change in v0.9.44 / v0.9.45 for Songchao. The finding is
documented here and in NEWS.md so future contributors don't
redo the inspection.

## What's still missing for a chemistry-driven WRB benchmark

| Need | Source | Status |
|---|---|---|
| EU lat/lon points | EU_LUCAS_2022.csv (~338 000 points) | ✅ available |
| Reference WRB per coordinate | ESDB WRBLV1.tif via lookup_esdb() | ✅ v0.9.44 |
| Per-point chemistry (clay/sand/silt/pH/OC) | LUCAS Soil 2018 Component Survey CSV | ❌ separate ESDAC download |
| Profile-level full-WRB classification | SPADBE Access .mdb | ❌ separate ESDAC download |

To close the gap:

- **Path A (Route B from v0.9.27 roadmap)**: Hugo downloads
  [LUCAS Soil 2018 Topsoil](https://esdac.jrc.ec.europa.eu/content/lucas-2018-topsoil-data)
  (~21 859 samples, free with ESDAC registration). soilKey's
  existing `load_lucas_pedons()` reads it; combine with v0.9.44
  `lookup_esdb()` to attach reference WRB per point.
- **Path B (Route A from v0.9.27 roadmap)**: Hugo downloads
  [SPADBE / SPADE](https://esdac.jrc.ec.europa.eu/content/spade-1)
  (Access .mdb, ~1 000-5 000 profiles with horizon chemistry +
  `WRB-FULL` per profile).

Both are 5-10 min downloads behind ESDAC registration. The
v0.9.44 raster utility is the prerequisite for Path A; Path B
requires a separate `load_spadbe_pedons()` loader that's a fast
follow-up if Hugo prefers that route.

## Update to `reference_eu_lucas_wrb_benchmark.md` memory

The previous note stated that the EU-LUCAS WRB benchmark was
blocked. v0.9.44 unblocks the **raster-lookup half** of Route B;
chemistry remains the bottleneck. Updated reference document
attached.
