# ESDB Raster Library lookup utility — 2026-05-04 (v0.9.44)

The ESDB Raster Library 1km GeoTIFF release (May 2024) ships **71
thematic rasters** at 1 km resolution under LAEA Europe (EPSG:3035),
including:

- **`WRBLV1.tif`** — WRB Reference Soil Group (1 km, 23 RSG codes
  + 6 non-soil mask codes)
- **`WRBFU.tif`** — full WRB classification (RSG + qualifier)
- **`WRBADJ1.tif`** / `WRBADJ2.tif` — WRB qualifier 1 / 2
- **`FAO90LV1.tif`** / `FAO85LV1.tif` — FAO 1990 / 1985 cross-system reference
- 65 thematic rasters (clay/sand/silt sub + topsoil, OC, parent
  material, slope, depth-to-rock, texture, mineralogy, etc.)

v0.9.44 ships two new exported helpers:

  available_esdb_attributes(raster_root)
    -> character vector of attribute folder names

  lookup_esdb(coords, attribute, raster_root, decode = TRUE)
    -> WGS84 lat/lon -> reproject to LAEA Europe -> extract raster
       value -> decode via .vat.dbf to coded label

## Demonstration (12 European cities, WRBLV1 + WRBADJ1 + FAO90LV1)

| City | Coordinates (lon, lat) | WRBLV1 | WRBADJ1 | FAO90LV1 |
|------|---:|---|---|---|
| Wageningen NL | 5.66, 51.97 | **FL** Fluvisol  | eu Eutric  | (mask) |
| Helsinki FI   | 24.94, 60.17 | **LP** Leptosol  | dy Dystric | LP |
| Rovaniemi FI  | 25.73, 66.50 | **CM** Cambisol  | dy Dystric | CM |
| Athens GR     | 23.73, 37.98 | **LV** Luvisol   | ca Calcaric | (mask) |
| Stockholm SE  | 18.07, 59.33 | **CM** Cambisol  | dy Dystric | CM |
| Sevilla ES    | -5.99, 37.39 | **FL** Fluvisol  | ca Calcaric | FL |
| Vienna AT     | 16.37, 48.21 | **CH** Chernozem | ha Haplic  | CH |
| Lisbon PT, Berlin DE, Paris FR, Rome IT, Krakow PL | -- | (mask) | (mask) | (mask) |

The cities returning "(mask)" fall on 1 km pixels coded as urban /
non-soil (codes 1-6 in the WRBLV1 VAT, mostly artificial-surface
classes). For sub-urban and rural points the lookup returns the
correct RSG; the documented results match published soil maps for
boreal Europe (LP / CM dominant), pannonian basin (CH), and
Mediterranean coast (LV / FL).

## What this enables

For any European-coordinate `PedonRecord`:

```r
library(soilKey)

root <- "<path>/ESDB-Raster-Library-1k-GeoTIFF-20240507"

# 1. Look up the ESDB raster's expected RSG at the pedon's coordinates:
expected_rsg <- lookup_esdb(
  coords    = c(my_pedon$site$lon, my_pedon$site$lat),
  attribute = "WRBLV1",
  raster_root = root
)
expected_rsg
#> [1] "LV"

# 2. Run the deterministic key on the pedon:
res <- classify_wrb2022(my_pedon)
res$rsg_or_order
#> [1] "Luvisols"

# 3. Compare:
identical(sub("s$", "", res$rsg_or_order), expected_rsg)
#> [1] TRUE   # soilKey agrees with the ESDB 1km raster at this point
```

This becomes the **fourth validation axis** for soilKey, alongside:
- 31 canonical fixtures (synthetic, designed to fire each WRB RSG)
- KSSL+NASIS (USDA Soil Taxonomy 13ed, n=2 638)
- Embrapa FEBR (SiBCS, n=554)
- WoSIS GraphQL (WRB, n=40 bundled)
- **ESDB Raster (WRB, raster-vs-pedon, any European coordinate)** ← new

For users without the raster downloaded locally, `lookup_esdb()`
errors clearly with the path to the missing folder.

## What's still missing for a chemistry-driven WRB benchmark

The `EU_LUCAS_2022.csv` (455 MB) and `EU_LUCAS_2022_updated.xlsx`
(288 MB) ship lat/lon + point-survey metadata for ~338 000 European
points but **no per-point soil chemistry** (no clay / sand / silt /
pH / OC). Without chemistry we cannot build a `PedonRecord` that
the deterministic key can act on.

Three paths to close the gap:

1. **LUCAS Soil 2018 / 2022 Component Survey** -- separate ESDAC
   download, ~21 859 topsoil samples with full chemistry. Spatial-
   join LUCAS Soil chemistry against the WRBLV1 raster to get
   classifier-ready (chemistry + reference WRB) profiles.
2. **SPADBE / SPADE Access mdb** -- 1 000-5 000 European profiles
   with horizon-level chemistry AND `WRB-FULL` classification per
   profile. Mirrors the KSSL+NASIS / FEBR benchmark shape exactly.
3. **OSSL European subset** -- via Vis-NIR predictions plus the
   ESDB raster as ground-truth WRB labels.

All three require additional downloads (free + ESDAC registration
for #1 and #2). The ESDB raster lookup landed in v0.9.44 is a
prerequisite for paths #1 and #3 -- it's the function that does the
spatial join.

## Reproducibility

```r
library(soilKey)
root <- "<path>/ESDB-Raster-Library-1k-GeoTIFF-20240507"
attrs <- available_esdb_attributes(root)
length(attrs)
#> [1] 71

# All 23 WRB RSG codes are available via decoded lookup:
coords <- rbind(c(5.66, 51.97), c(24.94, 60.17), c(16.37, 48.21))
lookup_esdb(coords, "WRBLV1", root)
#> [1] "FL" "LP" "CH"
```
