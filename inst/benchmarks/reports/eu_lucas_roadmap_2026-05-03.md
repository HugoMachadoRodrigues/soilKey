# EU-LUCAS WRB benchmark — roadmap (2026-05-03)

Status of the EU-LUCAS WRB benchmark axis after inspection of the
`soil_data/eu_lucas/` folder shipped with the project repository.

## What's currently available locally

| Path                                            | Size  | Type                              |
|-------------------------------------------------|------:|-----------------------------------|
| `soil_data/eu_lucas/EU_LUCAS_2022.csv`          | -     | LUCAS 2022 **point survey**, NO soil chemistry, NO WRB |
| `soil_data/eu_lucas/EU_LUCAS_2022_updated.xlsx` | -     | same as above (xlsx variant)     |
| `soil_data/eu_lucas/LUCAS-Master-Grid.xlsx.xls` | -     | LUCAS master sampling grid       |
| `soil_data/eu_lucas/ESDBv2/`                    | 388 MB | ESDB v2 **Atlas / Browser** distribution |

## What's actually inside `ESDBv2/`

The 388 MB `ESDBv2` directory is the **Atlas / Browser** distribution
of the European Soil Database v2 — NOT the Database distribution.
Contents breakdown:

| Subdir / file                   | Size    | What it is                                    |
|---------------------------------|--------:|-----------------------------------------------|
| `maps/sgdbe/*.pdf` (113 files)  | ~370 MB | Pre-rendered cartographic outputs (A3 PDFs) for each SGDBE attribute (FAO85, FAO90, WRB-FULL, WRB-LEV1, WRB-ADJ1, ...). **Each is a finished MAP, not a record table.** |
| `maps/ptrdb/`                   | ~10 MB  | Pre-rendered PTRDB maps (same format)         |
| `esdb/sgdbe/SGDBE_*.txt/.htm`   | ~600 KB | **Field dictionary** + metadata + attribute codes (no records) |
| `esdb/sgdbe/metadata/html/*.txt`| ~200 KB | Per-field documentation (e.g. `WRB-FULL.txt` describes the WRB-FULL field; does not list the records) |
| `esdb/Spadbe/*.doc`             | ~700 KB | SPADBE README / Dictionary / Metadata / Attribute Codes (Word docs, no actual profile data) |
| `esdb/Hypres/*.doc`             | ~600 KB | HYPRES README + metadata + parameters (Word docs) |
| `esdb/Ptrdbe/*.doc`             | ~350 KB | PTRDBE documentation                          |
| `xls/est_prof.xls`              | 18 KB   | **Schema-only** (empty Excel template)        |
| `xls/mea_prof.xls`              | 19 KB   | **Schema-only** (empty Excel template)        |
| `autorun.exe` + `autorun.inf`   | 400 KB  | Windows-only launcher for the embedded HTML browser |
| `images/`, `css/`, `script/`, `popup/`, `*.htm` | ~500 KB | UI assets for the autorun browser |

There is **no `.dbf`, `.shp`, `.mdb`, or actual record-level CSV** anywhere
in the tree. `RemainingProblems.txt` (in `esdb/sgdbe/metadata/`) refers
to ~3 500 SMUs and ~4 125 STUs by record number, confirming that the
underlying database exists somewhere — but the records themselves are
NOT in this folder.

The Atlas / Browser distribution was designed for human visualisation
on Windows: insert the CD, autorun.exe opens an HTML interface, the
user clicks attribute buttons and the corresponding pre-rendered PDF
map opens. The data records that produced those maps live in a
separate ESDAC distribution (the **Database**, sometimes called
**ESDBv2 vector + attributes**), which requires registration at
[esdac.jrc.ec.europa.eu](https://esdac.jrc.ec.europa.eu) and is
distributed under a separate licence.

## What we need to run an EU-LUCAS WRB benchmark

To populate `pedon$site$reference_wrb` for LUCAS profiles, we need a
WRB classification per profile. Three feasible routes from public
ESDAC sources, in order of fidelity:

### Route A (best fidelity) — SPADBE profile-level Access database

ESDAC distributes [**SPADBE — Soil Profile Analytical Database
of Europe**](https://esdac.jrc.ec.europa.eu/content/spade-1)
(formerly SPADE-1, currently SPADE/M v2) as an Access `.mdb`
file with:

- ~1 000 to 5 000 actual soil profiles across Europe
- horizon-level chemistry (clay, sand, silt, OC, pH, CEC, BS, ...)
- **profile-level WRB classification** in a `WRB-FULL` column
- profile-level FAO85 / FAO90 classifications for cross-system check

This is the cleanest source: each row is one profile with both the
horizon analytical data soilKey needs AND the WRB reference label.
A SPADBE benchmark would be **directly comparable** to KSSL+NASIS
(USDA) and FEBR (SiBCS), giving us a balanced three-system real-data
validation matrix.

**Requires**: ESDAC account (free) + accept the ESDAC redistribution
licence. Distribution is `.mdb` (Access) — readable on macOS via
`Hmisc::mdb.get()` or `pacman::p_load("RODBC")` against `mdbtools`
(Homebrew: `brew install mdbtools`).

### Route B (medium fidelity) — LUCAS Soil 2018 + ESDB raster spatial join

1. Download [**LUCAS Soil 2018**](https://esdac.jrc.ec.europa.eu/content/lucas-2018-topsoil-data)
   (~21 859 topsoil samples with full chemistry, free, registration
   required). This gives us soil chemistry per LUCAS sampling point.
2. Download [**ESDB Raster v2 1km**](https://esdac.jrc.ec.europa.eu/content/european-soil-database-derived-data)
   (free, includes a **`WRB-LEV1.tif`** at 1 km resolution covering
   the EU). Each cell carries the dominant WRB Reference Soil Group.
3. Spatial-join LUCAS coordinates against the WRB-LEV1 raster to
   inherit the reference WRB label.

This route gives **20 000+ profiles with WRB labels**, but only
**topsoil chemistry** (one horizon, 0-30 cm) and only at the
**Reference Soil Group** level — no qualifiers. The classifier
side benefits from soilKey's existing `load_lucas_pedons()` (already
ships at [R/benchmark-loaders.R:123](R/benchmark-loaders.R)).

### Route C (lowest fidelity) — ESDB v2 vector STU/SMU records

Download the ESDB v2 vector + attribute distribution from ESDAC.
This ships SMU (mapping units) and STU (typological units) as
ESRI shapefile + DBF tables. STU records carry `WRB-FULL`,
`WRB-ADJ1`, and `WRB-ADJ2` fields, but the unit of analysis is a
**polygon**, not a profile — there's no horizon chemistry. We'd
need to combine with LUCAS Soil 2018 (Route B) to get analytical
data per record. Adds complexity for marginal value over Route B.

## Recommended path

**Route A (SPADBE)** is the best benchmark axis. It mirrors what we
already do for USDA (KSSL+NASIS join — analytical lab data + reference
classification per profile) and for SiBCS (FEBR — same shape). With
SPADBE we'd have:

| System  | Source              | Profiles | WRB reference at |
|---------|---------------------|---------:|------------------|
| USDA    | KSSL+NASIS          | ~36 000  | Subgroup         |
| SiBCS   | Embrapa FEBR        | ~2 400   | Subgrupo         |
| WRB     | SPADBE / SPADE      | ~1 000-5 000 | Reference Soil Group + qualifiers |

To kick off Route A, the next steps are:

1. Hugo registers at [esdac.jrc.ec.europa.eu](https://esdac.jrc.ec.europa.eu)
   and accepts the SPADBE redistribution licence (free, ~5 minutes).
2. Download the SPADBE `.mdb` to `soil_data/spadbe/`.
3. Implement `load_spadbe_pedons(mdb_path)` in
   `R/benchmark-loaders.R` — parallel to `load_kssl_pedons_with_nasis()`
   and `load_febr_pedons()`. Read the profile + horizon tables, attach
   `reference_wrb` from the `WRB-FULL` column.
4. Run `benchmark_run_classification(peds, system = "wrb2022", level = "order")`.
5. (Stretch) parse WRB-FULL into RSG + qualifiers and run a
   `level = "rsg"` + `level = "qualifier"` benchmark, mirroring our
   USDA Order/Suborder/GG/Subgroup hierarchy.

Estimated effort: ~1-2 days for Route A end-to-end (loader + benchmark
+ report). Deferred until the ESDAC download is in place.

## Update to the EU-LUCAS roadmap memory

The previous note on this topic ("EU-LUCAS new files: still no WRB
labels (need ESDAC LUCAS_2018_Soil release)") was correct in spirit
but pointed at the wrong artefact. The release that's missing is
**SPADBE**, not LUCAS Soil 2018 (which is missing too, but for a
different reason — Route B, not Route A). The shipped `ESDBv2/`
folder is the Atlas distribution; **the EU-LUCAS / WRB benchmark
needs Route A (SPADBE) or Route B (LUCAS Soil 2018 + ESDB raster)**,
not anything that's currently in `ESDBv2/`.
