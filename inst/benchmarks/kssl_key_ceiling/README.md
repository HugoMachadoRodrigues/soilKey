# The ceiling of the USDA key on KSSL pedons

How far can `classify_usda()` go on the KSSL/NASIS pedons of the
conterminous USA, and what stops it? The key is run on 26,726 KSSL pedons
(CONUS, with coordinates, linked to a NASIS pedon) under several
information scenarios, and each wrong order is traced to its cause in the
first-match key: another order fired first (pre-emption), the true order
failed its criteria, or the true order stayed undecided for want of data.

## Scenarios

| | Input to the key |
|---|---|
| A | KSSL lab data only, strict |
| B | A + NASIS morphology |
| C | B + morphological inference from horizon designations |
| D, E, F | C + temperature, moisture or both regimes read off the record (oracle, upper bound) |
| G, H | C + regimes from PRISM 1991-2020 climate (Thornthwaite / Hargreaves PET) |
| S | only the properties SOLUS100 maps, from the pedon's own lab data, + the H regimes |
| U | S + the lab horizon designations |
| T | S resampled to the SOLUS depth slices (0-5-15-30-60-100-150-200 cm) |

The SOLUS set is clay, silt, sand, organic C, pH (1:1 H2O), CEC at pH 7,
ECEC, CaCO3, gypsum, EC, SAR, bulk density and coarse fragments. It is a
perfect map: the values are the measured ones, so S, U and T bound from
above what a SOLUS-fed key could reach. The lab gives coarse fragments by
weight, SOLUS by volume. Restriction depth is mapped by SOLUS but not used.

## Results (order level, `results/`)

No-information rate (majority order): 27.6 %.

| Scenario | Order accuracy | Kappa | Suborder | Great group |
|---|---|---|---|---|
| A lab only | 35.9 % | 0.24 | 16.2 % | 9.2 % |
| C + morphology | 56.5 % | 0.47 | 22.9 % | 14.6 % |
| F + recorded regimes (oracle) | 61.3 % | 0.53 | 43.3 % | 28.9 % |
| H + PRISM regimes (Hargreaves) | 60.0 % | 0.52 | 35.6 % | 23.0 % |
| S SOLUS properties | 9.9 % | 0.00 | 5.2 % | 2.4 % |
| T S at SOLUS slices | 9.9 % | 0.00 | 5.1 % | 2.3 % |
| U S + designations | 31.3 % | 0.18 | 19.8 % | 12.6 % |

With SOLUS properties alone the key falls below the no-information rate:
82 % of the pedons leave their true order undecided, and the key keys out
almost everything as Entisols. The blocking attributes are base saturation,
Munsell colour, structure and rupture resistance (mollic, umbric and
cambic tests), clay films and exchangeable Na (argillic and natric),
oxalate Al and Fe (spodic, andic) and slickensides (Vertisols). Where the
true order is decidable, S is right on 55 % of the pedons.

## Running it

The scripts use the local paths of the package checkout and of the KSSL
GeoPackage (`soil_data/KSSL/`), as the other benchmarks here do.

```
Rscript 01_build_panel.R <cache> [n]           # panel (lab + NASIS)
Rscript 03_climate_regimes.R <cache> <out> <prism_dir>
Rscript 06_split_panel.R <cache> <chunk_root>  # 3000-pedon chunks
./run_full_chunks.sh                           # scenarios A-H
./run_solus_chunks.sh                          # scenarios S, U, T
Rscript 04_summarise.R <results>               # accuracy, anatomy, recall
Rscript 05_diagnose.R <results> <scenario>     # per-order bottlenecks
```

PRISM normals: <https://ftp.prism.oregonstate.edu/normals/us/4km/>.
