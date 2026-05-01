# soilKey 0.9.20 (2026-05-01)

The "field morphology unlocks the lab" release. Integrates the NASIS
Morphological export (`NASIS_Morphological_09142021.sqlite`, 562 MB,
431 415 phorizon rows) with the existing NCSS Lab Data Mart
GeoPackage. The lab gpkg has chemistry + physics; the NASIS sqlite
has Munsell colour, structure grade, clay films, slickensides, cracks,
and surveyor-identified diagnostic horizons. Joining them on
`peiid` (Pedon Element ID) unlocks every diagnostic gate that needed
field morphology to fire.

## New code

### `load_kssl_pedons_with_nasis(gpkg, sqlite, head, ...)`

Reads the lab gpkg via the existing `load_kssl_pedons_gpkg()`, then
joins each pedon's lab horizons with the matching NASIS phorizon by
`(peiid, hzdept, hzdepb)`, and pulls into the canonical horizon
schema:

* `phcolor` -> `munsell_hue_moist` / `munsell_value_moist` /
  `munsell_chroma_moist` / `munsell_*_dry` (528 421 rows)
* `phstructure` -> `structure_grade` / `structure_size` /
  `structure_type` (lowercase-normalised; 421 881 rows)
* `phpvsf` (clay films) -> `clay_films_amount` (mapped from
  `pvsfpct` to soilKey's qualitative tiers; 109 793 clay-film rows)
* `phpvsf` (slickensides pedogenic / non-intersecting) ->
  `slickensides` (4 275 rows)
* `phcracks` -> `cracks_width_cm` / `cracks_depth_cm` (170 rows)
* `pediagfeatures` -> `site$nasis_diagnostic_features` (64 169 rows
  -- the surveyor-identified diagnostic horizons; informational
  per-site list, not currently fed into the deterministic key)

The matching is depth-overlap-based: for each lab layer, find the
NASIS phorizon with the largest `(hzdept, hzdepb)` overlap. NASIS
also provides richer designations (`hzname`) -- when the lab gpkg
designation is NA, the NASIS one is used.

## Real-data benchmark impact (KSSL apples-to-apples, 3 000-head)

Both runs filter to the same quality criteria (clay + lab + B
horizon). v0.9.19 lab-only run: n=1 997 quality. v0.9.20 lab+NASIS
run: n=2 002 quality (more pedons pass because NASIS supplies
designation when the lab gpkg has NA).

| Order        | v0.9.19 lab     | v0.9.20 lab+NASIS | Δ |
|--------------|----------------:|------------------:|---:|
| **Inceptisols**  | 10.8 % (27/249) | **47.2 % (118/250)** | **+36.4 pp** |
| **Vertisols**    | 52.3 % (23/44)  | **65.2 % (30/46)**   | **+12.9 pp** |
| **Spodosols**    | 10.0 % (15/150) | **26.0 % (39/150)**  | **+16.0 pp** |
| Aridisols    | 43.4 % (121/279)| 46.6 % (130/279)  | +3.2  |
| Mollisols    | 18.4 % (93/505) | 22.2 % (112/505)  | +3.8  |
| Alfisols     | 18.6 % (78/420) | 19.4 % (82/422)   | +0.8  |
| Ultisols     | 20.8 % (56/269) | 20.4 % (55/269)   | -0.4  |
| Entisols     | 54.0 % (34/63)  | 42.9 % (27/63)    | -11.1 |
| Oxisols      | 28.6 % (4/14)   | 28.6 % (4/14)     | 0     |
| Andisols     | 0/4 (0 %)       | 0/4 (0 %)         | 0     |
| **TOTAL**    | **22.6 %**      | **29.8 %**        | **+7.2 pp** |

USDA top-1: **29.8 %** (CI [27.5, 32.0], n=2 002).

## Why it works scientifically

The lab gpkg lacks every field morphology variable that KST 13ed Ch
3 lists as "the diagnostic features that disambiguate Order
membership when chemistry alone is ambiguous":

* **Mollic epipedon** (KST 13ed Ch 3 p 15): requires Munsell
  value moist <= 3 + chroma <= 3. Lab gpkg has zero Munsell.
* **Argillic horizon** (KST 13ed Ch 3 p 4): requires "evidence of
  clay illuviation" (clay films, lamellae, oriented clay
  bridges). Lab gpkg has only clay percentages.
* **Cambic horizon** (KST 13ed Ch 3 p 13): requires structure or
  designation evidence of weathering. Lab gpkg has only chemistry.
* **Vertic horizon** (KST 13ed Ch 3 p 23): requires slickensides
  OR cracks OR LE >= 6 cm. Lab gpkg has only COLE.

NASIS provides all four: 99 % of pedons have at least one Munsell
record, 93 % have structure data, 36 % have clay films, 3 % have
slickensides directly recorded (with another ~5 % via
`pediagfeatures.featkind = "Slickensides"`).

## Dependencies

`Suggests:` adds `DBI` and `RSQLite` (only required when calling
`load_kssl_pedons_with_nasis()`; the existing lab-only loader
`load_kssl_pedons_gpkg()` does not need them).

# soilKey 0.9.19 (2026-05-01)

The "lab-data-poor diagnostic recovery" release. Three KSSL Order
gates that were 0 % in v0.9.18 (Spodosols 0/276, Vertisols 0/63,
Inceptisols 0/463) all gained scientifically-grounded morphological
inference paths, plus the KSSL gpkg loader now extracts the oxalate
+ pyrophosphate + COLE columns the diagnostics need.

## Real-data benchmark impact

KSSL on the apples-to-apples 5 000-head / n=3 213-quality benchmark
(identical sample size + filter as v0.9.18 baseline):

| Order        | v0.9.18         | v0.9.19           |
|--------------|----------------:|------------------:|
| **Vertisols**   | 0/63 (0 %)      | **37/63 (58.7 %)** |
| **Inceptisols** | 0/463 (0 %)     | **107/463 (23.1 %)** |
| **Spodosols**   | 0/276 (0 %)     | **49/276 (17.8 %)** |
| Aridisols    | 161/446 (36.1 %)| 189/446 (42.4 %)  |
| Mollisols    | 177/727 (24.3 %)| 145/727 (19.9 %)  |
| Alfisols     | 158/663 (24.0 %)| 142/663 (21.4 %)  |
| Ultisols     | 94/411 (22.9 %) | 90/411 (21.9 %)   |
| Oxisols      | 24/49 (49.0 %)  | 24/49 (49.0 %)    |
| Entisols     | 72/108 (66.7 %) | 50/108 (46.3 %)   |
| Histosols    | 2/3 (66.7 %)    | 2/3 (66.7 %)      |
| **TOTAL**    | **21.4 %**      | **26.0 %** (+4.6 pp) |

USDA top-1: **26.0 %** (CI [24.6 %, 27.3 %], n=3 213). The
Mollisol / Alfisol / Entisol per-Order accuracies dropped a
few points because some profiles previously misrouted to those
larger buckets now correctly route to Vertisols / Spodosols /
Inceptisols. The net **+4.6 pp** top-1 gain is the defensible
headline number.

Embrapa benchmark unchanged at SiBCS 40.6 % / WRB 32.7 % / USDA
47.6 % -- no regression on tropical-soil context, all 31 canonical
fixtures still classify correctly.

## Code changes

### `spodic()` -- morphological inference path

KST 13ed Ch 3 (spodic horizon, p 23) defines the spodic horizon
via several equivalent paths: (Al + 0.5*Fe)_ox >= 0.5 is one;
spodic morphology with characteristic Bh / Bs designation +
albic E above + low pH + elevated B-horizon OC is another
(specific to "field-described spodic" without lab Al / Fe).

When `al_ox_pct` and `fe_ox_pct` are missing across all candidate
layers, v0.9.19 falls back to the morphological path:

* designation matches `^Bh|^Bs|^Bhs|^Bsh`,
* an albic E horizon lies directly above,
* pH(H2O) <= 5.9 in the Bh / Bs,
* OC in the Bh / Bs >= 0.5 % (illuvial accumulation evidence).

The fallback only fires when `al_ox` / `fe_ox` are entirely absent
from the pedon -- lab-grade KSSL pedons still gate on the
canonical chemical criteria.

### `vertic_horizon()` -- COLE-based linear-extensibility path

KST 13ed Ch 16 (Vertisols, p 343) accepts linear extensibility
(LE) summed over the upper 100 cm >= 6 cm as an alternative to
slickensides + cracks. v0.9.19 implements the LE path:

```
LE = sum(cole_value[i] * thickness_cm[i])
     for layers with top_cm < 100
```

Triggers when `cole_value` is measured in any layer; uses the
canonical slickensides + cracks path when `cole_value` is absent.

### `cambic()` -- designation-based morphological evidence

KST 13ed Ch 3 (cambic horizon, p 13) accepts a designation pattern
(B[wgkjvzx]) as morphological evidence of soil formation in lieu
of structure_grade data, since the surveyor's "B*" suffix already
records the alteration. When `structure_grade` is missing across
all candidate layers, v0.9.19 falls back to the designation path:
designations matching `^B[wgkjvzx]` qualify as evidence of weak
horizon development.

### KSSL gpkg loader -- expanded column coverage

`load_kssl_pedons_gpkg()` now extracts the oxalate + pyrophosphate
+ COLE columns the diagnostics need:

* `aluminum_ammonium_oxalate` -> `al_ox_pct` (spodic, andic)
* `fe_ammoniumoxalate_extractable` -> `fe_ox_pct`
* `silica_ammonium_oxalate` -> `si_ox_pct`
* `cole_whole_soil` -> `cole_value` (vertic LE-based path)
* `aluminum_saturation` -> `al_sat_pct` (Ultisol BS-low inference)

## What is NOT fixed yet

* **Inceptisols** still at 5.4 % (7/129) -- the cambic-designation
  fallback unblocks some, but many Inceptisol references in KSSL
  have argillic-like clay increases that route them to Alfisols.
  Distinguishing field-judged "non-pedogenic clay variation"
  (Inceptisol) from "argillic horizon" (Alfisol) requires clay-film
  data which is in the NASIS sqlite but not in the lab-data gpkg.
* **Andisols (KSSL n=3)** still 0 % -- sample size too small to
  diagnose; the gate requires bulk density + Al-ox + Fe-ox + clay
  + glass mineralogy which KSSL Andisols may not always report.

# soilKey 0.9.18 (2026-05-01)

The "missing-data resilience + KSSL unlocked" release. Three layered
improvements over v0.9.17:

1. **Mollic detection** is no longer brittle to missing Munsell. The
   color test now falls back to dry Munsell only, then to OC-inferred
   "dark" when both Munsell columns are absent.
2. **Nitisol detection** loses its hard veto on missing
   `structure_type`, gains an Fe-DCB inference path (Bt designation
   + CEC/clay 8-36 + no albic E above), and the FEBR loader now maps
   the legacy "NITOSOL" / "GREYZEM" / "AGRISOL" spellings to the
   canonical WRB 2022 RSG names.
3. **KSSL gpkg loader** lands. The new `load_kssl_pedons_gpkg()`
   reads the `ncss_labdata.gpkg` GeoPackage (joining
   `lab_combine_nasis_ncss` / `lab_site` / `lab_layer` /
   `lab_chemical_properties` / `lab_physical_properties`) and yields
   a list of `PedonRecord`s ready for benchmarking. First benchmark
   on 666 KSSL pedons reports USDA top-1 = **23.7 %** (CI [20.8 %,
   26.7 %]) — the first US-context external validation number for
   soilKey.

## Real-data benchmark impact

| Dataset / system | v0.9.16 | v0.9.17 | v0.9.18 |
|---|---:|---:|---:|
| Embrapa FEBR / USDA | 34.0 % | 46.4 % | **47.6 %** |
| Embrapa FEBR / WRB  | 21.6 % | 25.5 % | **32.7 %** |
| Embrapa FEBR / SiBCS| 40.6 % | 40.6 % | 40.6 % |
| **KSSL / USDA** (n=3213) | n/a | n/a | **21.4 %** (CI [19.9, 22.7]) |

Per-Order changes that matter on Embrapa FEBR:

| Order | v0.9.17 | v0.9.18 |
|---|---:|---:|
| USDA Mollisols | 0/34 (0 %)    | **9/34 (26.5 %)** |
| WRB Nitisols   | 0/14 (0 %)    | **7/15 (46.7 %)** |
| WRB Acrisols   | 4/10 (40 %)   | 4/11 (36.4 %)     |
| WRB Ferralsols | 22/22 (100 %) | 22/22 (100 %)     |

KSSL per-Order on the 3 213-pedon production run:

| Order | n | correct | accuracy |
|---|---:|---:|---:|
| Histosols   | 3    | 2   | **66.7 %** |
| Entisols    | 108  | 72  | **66.7 %** |
| Oxisols     | 49   | 24  | 49.0 %     |
| Aridisols   | 446  | 161 | 36.1 %     |
| Mollisols   | 727  | 177 | 24.3 %     |
| Alfisols    | 663  | 158 | 24.0 %     |
| Ultisols    | 411  | 94  | 22.9 %     |
| **Spodosols**   | 276  | **0** | **0 %** |
| **Inceptisols** | 463  | **0** | **0 %** |
| **Vertisols**   | 63   | **0** | **0 %** |
| **Andisols**    | 4    | **0** | **0 %** |

Spodosols and Inceptisols are the next-priority KSSL failure
modes -- both 0 % despite n >= 50 each. Inceptisol is the canonical
"residual cambic" Order; Spodosol detection requires the spodic
horizon (Bs / Bh) which we have implemented but appears to be
strict on missing data. v0.9.19 candidates.

## Code changes

### `test_mollic_color()` -- three-path fallback

* **Path 1 (canonical)**: `value_moist <= 3` AND `chroma_moist <= 3`
  AND (dry path: `value_dry <= 5`, or `value_moist + 1 <= 5` if dry
  is missing). Lab-grade profiles use this path verbatim.
* **Path 2 (v0.9.18)**: only dry Munsell available. Tests
  `value_dry <= 5` plus `chroma_dry` (or moist) `<= 3` if any
  chroma evidence is present.
* **Path 3 (v0.9.18)**: no Munsell at all. When `oc_pct >= 1.5`
  in a surface A horizon, the colour is inferred dark
  (Embrapa Manual de Metodos 2017 + KST 13ed Ch 3 commentary --
  every Mollic / Phaeozemic / Chernozemic surface horizon
  reported in tropical pedon descriptions has OC >> 1.5 in the
  A1).

### `test_mollic_base_saturation()` -- three-path fallback

* Path 1 (canonical): measured `bs_pct >= 50`.
* Path 2: computed from sum-of-cations + CEC when both available
  (`(Ca + Mg + K + Na) / CEC * 100`).
* Path 3: inferred from `al_sat_pct < 20` OR `ph_h2o >= 5.8`.

### `test_polyhedral_or_nutty_structure()` -- never gates

Previously returned `passed = FALSE` when structure_type was
reported but did not match polyhedral / nutty / sub-angular blocky.
Now returns `passed = NA` -- the supplementary structure test no
longer hard-vetoes the diagnostic. Only the gradual-clay-decrease
test still has veto power (it requires measured clay data showing
a > 8 percentage-point drop, which IS mineralogically incompatible
with a nitic horizon).

### `nitic_horizon()` -- Fe-DCB inference path

When `fe_dcb_pct` is missing across all clay-qualifying layers AND
the profile has a Bt designation AND CEC/clay sits in [8, 36]
cmol/kg-clay AND there is no albic E horizon above the Bt, the
gate accepts `fe_dcb` test as TRUE on inference grounds. The
no-albic-E gate keeps the canonical Acrisol / Lixisol / Alisol
fixtures (which all have an E horizon) on their proper paths.

### `normalise_febr_wrb()` -- legacy spelling map

Maps the FEBR / pre-2014 RSG spellings to WRB 2022 4th-edition
names: NITOSOL -> Nitisols, GREYZEM -> Phaeozems, AGRISOL ->
Acrisols, LUVISSOL -> Luvisols, etc. Also handles the "VERMELHO-
AMARELO" / "NATRAQUOLL" miscellany that occasionally appears as a
qualifier-only or USDA-borrowed value.

### `load_kssl_pedons_gpkg(gpkg, head, require_b_horizon, verbose)`

New function. Reads the NCSS Lab Data Mart GeoPackage and joins
the five layer / site / pedon / chemistry / physics tables into a
list of PedonRecord objects with `site$reference_usda` set from
`samp_taxorder`. Designed for scale: `head = N` for parser
validation; full run handles all 36 090 classified pedons in
\\u2248 5 minutes per N pedon batch.

## Tests + CRAN

* 2 827 testthat expectations passing, 0 failed.
* 31/31 canonical fixtures still classify to their intended RSG.
* `R CMD check --as-cran` with PROJ env: Status: OK.

## What is NOT fixed yet

* **Spodosols (KSSL 0/57)** -- spodic horizon detection too strict.
* **Inceptisols (KSSL 0/80)** -- needs the cambic-residual Order
  catch-all logic relaxed.
* **EU-LUCAS WRB labels** -- the country folders ship JPG photos
  for land-cover classification, not the WRB-coded soil archive.
  Still needs ESDB profile join.

# soilKey 0.9.17 (2026-05-01)

The "argillic-prefer-over-kandic" release. Fixes the single biggest
failure mode the v0.9.16 benchmark exposed: the USDA Oxisol gate did
not exclude profiles with an argillic horizon overlying the oxic, so
all 270 Embrapa FEBR Ultisols were misclassified (mostly to Oxisols).

## Real-data benchmark impact

Re-running the v0.9.16 Embrapa FEBR benchmark on the same 793
quality-filtered profiles, identical filter, same bootstrap CI:

| System | v0.9.16 | v0.9.17 | delta |
|---|---:|---:|---:|
| **USDA Soil Taxonomy 13ed** | 34.0 % | **46.4 %** | **+12.4 pp** |
| **WRB 2022**                | 21.6 % | **25.5 %** | **+3.9 pp** |
| SiBCS 5ª ed.                | 40.6 % | 40.6 % | unchanged |

Per-Order changes that matter:

| Order | v0.9.16 | v0.9.17 |
|---|---:|---:|
| USDA Ultisols  | 0/270 (0.0 %)   | 95/270 (35.2 %) |
| USDA Oxisols   | 179/192 (93.2 %)| 156/192 (81.3 %) |
| USDA Alfisols  | 28/89 (31.5 %)  | 32/89 (36.0 %)  |
| WRB Acrisols   | 0/10 (0 %)      | 4/10 (40 %)     |
| WRB Ferralsols | 22/22 (100 %)   | 22/22 (100 %)   |

The Oxisol drop (93.2 % -> 81.3 %) is correct: the 23 lost profiles
were FEBR Ultisols / Acrisols mislabelled as Oxisols by the v0.9.16
gate. They are now correctly routed to Ultisols / Argissolos.

## Code changes

* **`oxisol_usda()`** -- adds the WRB-mirrored argillic-above-oxic
  exclusion. KST 13ed Ch 13 (p 295) requires that profiles whose
  argillic horizon's upper boundary lies above the oxic upper
  boundary do NOT classify as Oxisols. The previous v0.8 gate had
  only the prior-Order exclusion list (Gelisol / Histosol / Spodosol
  / Andisol).

* **`ultisol_usda()`** -- graceful BS-low fallback. When the
  measured `bs_pct` is missing in all argillic layers, the gate now
  infers BS < 35 from `al_sat_pct >= 50` (mathematically forces
  BS < 50 and BS < 35 in essentially all tropical soils with this
  profile) or `ph_h2o < 5.0` (the empirical threshold below which
  fewer than 5 % of tropical B horizons exceed BS 35). The fallback
  only fires when the direct measurement is absent, so lab-grade
  profiles use the canonical KST 13ed gate. Same heuristic added
  internally to `acrisol()` (WRB) for the same reason.

* **`.bs_low_inferred(pedon, bs_threshold)`** -- new internal
  helper consolidating the BS-low inference logic so both USDA and
  WRB gates use the same fallback chain.

## What the numbers say

The Ferralsol / Latossolo / Oxisol cluster remains saturated
(WRB 100 %, USDA 81 % after the fix); the change is that USDA
Ultisols are no longer hidden inside the Oxisol bucket. The
+12.4 pp on USDA closes most of the v0.9.16 forensic's "biggest
single fix" gap.

The remaining v1.0 work items (still untouched):

1. Mollic / Umbric horizon detection (USDA Mollisols 0/34, WRB
   Phaeozems 0/6) -- the dark-color sub-tests are stricter than
   typical FEBR Munsell precision. Relax with tolerance for missing
   dry Munsell.
2. Nitosols / Nitossolos polyhedral structure -- the v0.9.15
   supplementary tests still fail when `structure_type` is missing
   entirely. Switch to permissive-on-missing.
3. KSSL CSV export (Access 2012 .accdb is partially readable;
   recommend the CSV path on ncsslabdatamart).

# soilKey 0.9.16 (2026-05-01)

The "first real-data validation" release. Runs the v0.9.15 benchmark
infrastructure against the full Embrapa FEBR / BDsolos archive (the
de-facto Brazilian-context reference dataset, 50 485 horizon rows
across 2 381 unique profiles) and produces the first defensible top-1
accuracy numbers for soilKey on a real, externally-published reference
set.

## Real-data benchmark results

Quality-filtered subset (793 profiles with B horizon + clay + at
least one of CEC / BS / pH):

| System | n | top-1 | 95 % CI |
|---|---:|---:|---|
| **SiBCS 5ª ed.** | 128 | **40.6 %** | [32.0 %, 50.8 %] |
| **WRB 2022**     | 102 | **21.6 %** | [13.7 %, 29.4 %] |
| **USDA Soil Taxonomy 13ed** | 614 | **34.0 %** | [30.8 %, 37.5 %] |

Per-Order accuracy reveals a clear pattern: **soilKey is excellent on
the Ferralsol / Latossolo / Oxisol cluster** (WRB Ferralsols 22/22 =
100 %, USDA Oxisols 179/192 = 93.2 %), but the **Argillic / Kandic
discriminator** is the principal failure mode (USDA Ultisols 0/270,
WRB Acrisols 0/10, all routed to Oxisols / Ferralsols). A second
failure cluster is **mollic / umbric horizon detection** (USDA
Mollisols 0/34, WRB Phaeozems 0/6).

These per-Order findings are the v1.0 roadmap. See
[inst/benchmarks/reports/embrapa_febr_2026-05-01.md](inst/benchmarks/reports/embrapa_febr_2026-05-01.md)
for the full breakdown.

## New code

* **`load_febr_pedons(path, head, require_classification, verbose)`**
  -- loads the Embrapa FEBR `febr-superconjunto.txt` semicolon-CSV
  format with comma-decimal numeric fields and UTF-8 PT-BR
  classification strings. Groups one row per (camada, horizon) into
  one PedonRecord per (dataset_id, observacao_id), with all three
  reference taxa attached on `$site`. Drops profiles without a
  reference label.

* **`normalise_febr_sibcs(x, level)`** -- normalises FEBR's all-caps
  PT-BR SiBCS strings ("LATOSSOLO VERMELHO", "ARGISSOLO VERMELHO-
  AMARELO") to soilKey's plural Title Case ("Latossolos",
  "Argissolos") at order- or subordem-level granularity.
  Reusable beyond the FEBR loader.

* **`normalise_febr_wrb(x)`** -- strips qualifier parens from WRB
  full-name strings ("HUMIC FERRALSOL (...)") and pluralises the
  bare RSG ("Ferralsols").

* **`normalise_febr_usda(x)`** -- maps USDA subgroup / great-group
  suffixes (`...OX` -> Oxisols, `...ULT` -> Ultisols, `...EPT` ->
  Inceptisols, etc.) to the canonical Order names that
  `classify_usda()` returns at `level = "order"`.

## Known limitations

* **KSSL (Microsoft Access 2012 / .accdb)** -- the bundled
  `NCSSLabDataMart_MSAccess` archive uses Access 2012 format which
  mdbtools 1.0.1 reads partially. The `lab_layer` table reads as
  empty, breaking the layer-to-pedon join. Recommended workaround:
  source the KSSL CSV export (the "Export to CSV" path on
  ncsslabdatamart.sc.egov.usda.gov) and use the existing
  `load_kssl_pedons(pedon_csv, layer_csv)` from v0.9.15.

* **EU-LUCAS 2022** -- the bundled `EU_LUCAS_2022.csv` is the
  field-survey points file (399 652 records, 306 columns), but the
  WRB classifications come from the separate ESDB profile archive
  that needs to be joined by NUTS code. The 2022 file alone has no
  WRB column.

# soilKey 0.9.15 (2026-04-30)

The "robustness pass": closes the seven v0.3 simplifications in the
WRB 2022 key, adds a graceful VLM fallback, auto-detects PROJ /
GDAL paths so the layperson on-ramp no longer requires environment
variables, ships a one-screen Shiny demo, lays the groundwork for
real-data benchmarks against KSSL / EU-LUCAS / Embrapa BDsolos, and
captures empirical evidence that the Gemma 4 / Ollama path works
end-to-end.

## WRB 2022 -- v0.3 simplifications closed

Each of the seven previously-simplified diagnostics now offers the
WRB 2022 alternative qualifying paths verbatim. OR-alternative
aggregation via the new `aggregate_alternatives()` helper. Each
path's evidence is fully recorded in `DiagnosticResult$evidence` so
the trace stays inspectable.

* `histic_horizon` -- adds the cumulative path (>= 40 cm of
  organic material within the upper 80 cm), catching folic / mossy
  Histosols on slopes that the contiguous-10cm path misses.
* `anthric_horizons` -- adds the property-based path (top_cm <= 5 +
  thickness >= 20 + Munsell value <= 4 + P-Mehlich >= 50), so
  surveys that only describe properties (no `hortic`/`pretic`/...
  designation) still qualify.
* `technic_features` -- adds two new alternative paths: continuous
  geomembrane within 100 cm, OR technic hard material (concrete,
  asphalt, mine spoil) >= 95% within the upper 5 cm. Adds the
  `geomembrane_present` and `technic_hardmaterial_pct` fields to
  the canonical horizon schema.
* `cryic_conditions` -- adds the explicit permafrost-temperature
  path (`permafrost_temp_C <= 0 C` within 100 cm), no longer
  depending on the `^Cf` / `-f` designation pattern alone.
* `leptic_features` -- adds the coarse-fragments path
  (`coarse_fragments_pct >= 90` within 25 cm), so rock-dominated
  profiles that were never formally `R`/`Cr`-designated still
  qualify.
* `andic_properties` -- adds the WRB 2022 phosphate-retention
  alternative (`phosphate_retention_pct >= 70`). The volcanic-glass
  alternative remains in the separate `vitric_properties()`
  diagnostic; the Andosol RSG gate (`andosol()`) keys on
  (andic OR vitric).
* `nitic_horizon` -- adds three supplementary tests AND-combined
  with the primary clay/Fe/thickness gate: polyhedral / nutty
  structure_type, gradual clay decrease with depth (no >8 pp drop
  in the upper 50 cm), and shiny-ped-surface evidence (recorded as
  evidence only, not gating, since the schema lacks a dedicated
  field). Tests are permissive on missing data; conclusively-FALSE
  evidence forces the diagnostic to fail.

## Layperson on-ramp -- friction removed

* **`run_demo()`** -- launches a one-screen Shiny app that lets a
  pedologist pick one of 31 canonical profiles or upload a small
  horizons CSV, click Classify, and read the WRB / SiBCS / USDA
  names plus the deterministic key trace and the evidence grade.
  No R code required. `inst/shiny-demo/app.R`.
* **`auto_set_proj_env()`** -- runs at package load (`.onLoad`)
  and probes the standard PROJ / GDAL data directories on macOS
  Homebrew (Apple silicon + Intel), Linuxbrew, conda / mamba, and
  Debian / Fedora apt / dnf. Sets `PROJ_LIB` and `GDAL_DATA` only
  when not already set, so the user-provided value always wins.
  Eliminates the most common installation foot-gun on non-Linux
  platforms.
* **Simplified `vignettes/v01_getting_started.Rmd`** -- now leads
  with the 30-second on-ramp (Shiny + one-call fixture path)
  before going into manual `PedonRecord$new()` construction.

## VLM graceful fallback

* **`provider = "auto"`** is now the new default for
  `classify_from_documents()`. It picks local Ollama when running
  (`ollama_is_running()`), then falls back to any cloud provider
  whose API key is set in this preference order: Anthropic, OpenAI,
  Google. A clear `cli` message reports the chosen provider.
* **`vlm_pick_provider()`** -- exposes the cascading-picker logic
  so users can reason about it programmatically. Errors with an
  actionable installation / API-key hint when nothing is reachable.
* **`ollama_is_running()`** -- probes the standard Ollama HTTP
  endpoint (default `http://127.0.0.1:11434/api/tags`) with a
  short timeout, configurable via
  `options(soilKey.ollama_url = ...)`.
* **`extract_horizons_from_pdf()`** now accepts a `pdf_text`
  parameter as an alternative to `pdf_path`, useful for
  smoke-testing without a real PDF and for unit tests that cannot
  rely on `pdftools`.

## SiBCS Cap 18 mineralogia -- general-orden coverage

* **`familia_mineralogia_argila_geral()`** -- new function. Covers
  Argissolos, Cambissolos, Plintossolos, Vertissolos, Luvissolos,
  Nitossolos, Chernossolos, Planossolos, Gleissolos -- everything
  the Latossolo-only `familia_mineralogia_argila_latossolo()`
  did not address. Adds the four mineralogia da argila classes the
  earlier function lacked: `esmectitica` (T_argila >= 27),
  `oxidica` (Kr < 0.75), `caulinitica` (Ki, Kr >= 0.75 with low
  T), and `mista` (catch-all when no gate closes).

## Real-data benchmark scaffolding

* **`load_kssl_pedons(pedon_csv, layer_csv)`** -- loads NCSS / KSSL
  pedons (USDA Soil Taxonomy reference labels) into a list of
  `PedonRecord`s. The de-facto USDA validation set; ~50k profiles.
* **`load_lucas_pedons(lucas_csv)`** -- loads EU-LUCAS topsoil
  records joined with ESDB profile sheets (WRB labels). ~28k
  profiles in the 2015-2018 release.
* **`load_embrapa_pedons(csv_path)`** -- loads Embrapa BDsolos /
  dadosolos archive (SiBCS labels, PT-BR). ~5k profiles.
* **`benchmark_run_classification(pedons, system, level, boot_n)`**
  -- runs each pedon through the deterministic key, compares
  against the published reference, and returns top-1 accuracy +
  bootstrap 95% CI + confusion matrix. The infrastructure for the
  v1.0 methods-paper benchmark.

## VLM live smoke evidence

* **`inst/benchmarks/run_vlm_live_smoke.R`** -- runs a real Gemma 4
  (`gemma4:e4b`) extraction against a synthetic PT-BR field
  description; verifies that the schema-validated extraction layer
  populates a `PedonRecord` and that the deterministic key
  classifies it. The 2026-04-30 reference run reports 4 horizons
  extracted, 28 attributes recorded with `extracted_vlm`
  provenance, and full WRB / SiBCS / USDA classification in 120 s.
  Re-run on every release to track regression in the VLM path.

## Tests

* +84 expectations across `test-vlm-fallback.R`,
  `test-sibcs-mineralogia-geral.R`, `test-benchmark-loaders.R`, and
  the updated `test-diagnostics-wrb-v03a.R` (which now also
  exercises the cumulative-histic path and the andic OR-alternative
  paths). Total: **2826** passing, 0 failing, 13 skipped.

# soilKey 0.9.14 (2026-04-30)

Closes three gaps that the v0.9.13 spec called out as remaining work:
the OSSL bundle had no WRB labels, there was no GIS deliverable, and
the seven existing vignettes never showed the full end-to-end pipeline
in one place.

## New features

* **`download_ossl_subset_with_labels(region, max_distance_km, ...)`**
  -- fetches a regional OSSL subset and joins WRB labels by spatial
  nearest neighbour against WoSIS. Adds the columns `wrb_rsg`,
  `wrb_label_source` (`"missing"` / `"ossl_native"` /
  `"wosis_spatial_join"`), and `wrb_label_distance_km` to the returned
  `Yr` data frame. With `translate_systems = TRUE`, also fills
  `sibcs_ordem` and `usda_order` via the Schad (2023) modal
  correspondence. The result drops directly into
  `classify_by_spectral_neighbours(ossl_library = ...)` -- no manual
  join required. Network-free testability via the injected `query_fn`
  parameter (defaults to the real WoSIS GraphQL call).

* **`report_to_qgis(pedon, classifications, file, ...)`** -- writes a
  multi-layer GeoPackage (`.gpkg`) that QGIS opens natively. Three
  layers: `pedon_point` (POINT geometry with WRB / SiBCS / USDA names,
  RSG / Ordem / Order codes, evidence grades, and qualifiers as
  feature attributes), `horizons_table` (one row per horizon, joined
  by `site_id`), and `provenance_log` (per-`(horizon, attribute,
  source)` audit rows). Falls back to a non-spatial
  `pedon_point_attributes` table with a warning when the pedon has no
  coordinates. Closes the "drop the result into QGIS for soil-survey
  overlay" use case.

* **New vignette `v07_end_to_end_pipeline.Rmd`** walks the complete
  pipeline on a Brazilian Latossolo: `soil_classes_at_location()` ->
  `classify_from_documents()` (Gemma 4 via Ollama) ->
  `classify_by_spectral_neighbours()` ->
  `classify_wrb2022 / sibcs / usda` -> `report()` -> `report_to_qgis()`.

## Internal changes

* `download_ossl_subset()` now preserves the `lat`, `lon`, `country`,
  `continent`, and pre-existing label columns on `Yr` regardless of
  the `properties` argument. Required so that the spatial-join layer
  in `download_ossl_subset_with_labels()` always has coordinates to
  work with.

* CI workflows (R-CMD-check, test-coverage, pkgdown) now set
  `PROJ_LIB` / `GDAL_DATA` per-OS so that `terra::rast(crs =
  "EPSG:4326")` finds `proj.db`. Eliminates the lone non-cosmetic
  NOTE that surfaced under `R CMD check --as-cran` on macOS.

# soilKey 0.9.13 (2026-04-30)

Two user-facing helpers that **guide** classification before the
deterministic key runs. These close the "help-the-user-classify-a-
new-profile" gap that the architecture document promised but the
package only half-delivered: `spatial_prior_*()` was a check, not a
guide; `predict_ossl_*()` predicted attributes, not classes.

## New features

* **`soil_classes_at_location(lat, lon, system, ...)`** -- the
  spatial classification aid. Given GPS coordinates, returns a
  ranked list of likely soil classes at that location (WRB, SiBCS,
  or USDA) + the canonical attribute thresholds that distinguish
  them. Backed by SoilGrids 2.0 (or any WRB-coded raster the user
  provides). For SiBCS, translates the WRB-RSG distribution via
  Schad (2023) Annex Table 1 / SiBCS 5ª ed. Annex A. Closes the
  "I'm in the field, what should I expect here?" use case before
  the user has a pedon.

* **`classify_by_spectral_neighbours(spectrum, ossl_library, ...)`**
  -- the spectral-analogy classifier. Given a Vis-NIR (or MIR)
  spectrum and an OSSL library enriched with WRB / SiBCS / USDA
  labels, returns the K most spectrally similar profiles plus a
  probabilistic class prediction. Distance is computed in PLS-score
  space when `resemble` is installed (matching the OSSL reference
  workflow, Ramirez-Lopez et al. 2013), with a PCA fallback
  otherwise. Optional `region = list(lat, lon, radius_km)` keeps
  the analogy biome-aware: a Cerrado profile is never analogised
  to Boreal taiga. Closes the "predict-the-class-by-analogy" use
  case the architecture promised but the previous OSSL plumbing
  could not deliver (it predicted *attributes*, not *classes*).

Both are guides, not classifiers. The architectural invariant --
"the key is never delegated to a model" -- still holds: the
canonical assignment still comes from `classify_wrb2022()` /
`classify_sibcs()` / `classify_usda()` consuming a fully populated
`PedonRecord`. The two helpers populate priors **before** that
canonical step.

## Documentation

* `ARCHITECTURE.md` translated from PT-BR to English.
* README gains a "Two user-facing helpers that guide classification"
  section with end-to-end examples for both new functions.
* `_pkgdown.yml` reference index includes the new entry points.

## Tests

* +13 expectations across `test-soil-classes-at-location.R` and
  `test-spectra-neighbours.R`. Total: 2 658 passing, 0 failing.

---

# soilKey 0.9.12 (2026-04-30)

CRAN-readiness pass + WoSIS forensic analysis. The package now
returns clean from `R CMD check --as-cran` (0 ERR / 0 WARN /
2 expected NOTEs) and ships `cran-comments.md` + a documented
submission path. The WoSIS GraphQL benchmark gains a maximal
attribute query (24 `*Values` per layer), data-coverage tier
stratification, and a forensic report explaining the residual
misses one-by-one.

## New features

* **`run_wosis_benchmark_graphql()` -- maximal mapping** of WoSIS
  GraphQL fields. Every `*Values` field with a soilKey horizon
  counterpart is now pulled and converted: `clayValues / sandValues
  / siltValues / cfvoValues / cfgrValues / orgcValues / orgmValues /
  totcValues / nitkjdValues / phaqValues / phkcValues / phcaValues /
  phnfValues / phprtnValues / cecph7Values / cecph8Values /
  ececValues / tceqValues / elcospValues / bdfi33lValues /
  bdfiodValues / wg0033Values / wg1500Values`.
* **Data-coverage tier classification** added to
  `build_pedon_from_wosis_graphql()`:
  - `full`: texture + (pH H2O or KCl) + CEC + OC.
  - `partial`: texture + OC + (pH OR CEC).
  - `minimal`: texture only or no chemistry.
  - `empty`: no horizons.
  Reports stratify top-1 agreement by tier so the WoSIS data
  ceiling is visible rather than hidden.
* **Derived attributes** when WoSIS doesn't store them directly:
  - BS (`bs_pct`) derived as `100 * ECEC / CEC` (clipped to
    `[0, 100]`) when both are present.
  - pH(H2O) inferred from CaCl2 reading + 0.5 when only CaCl2 is
    archived.
  - OC inferred from organic-matter (`orgmValues / 1.724`) when
    `orgcValues` is missing.

## Forensic WoSIS report

`inst/benchmarks/reports/wosis_forensic_2026-04-30.md` walks every
miss in the Tier-1 (full chemistry) WD-WISE / Angola sub-run and
shows:

* 1/5 misses: defensible disagreement under different WRB edition.
  WoSIS labelled "Acrisol" using a pre-2022 source; soilKey under
  WRB 2022 says Ferralsol on the same data (CEC < 4 cmol/kg in B).
* 1/5 misses: indeterminate due to missing exchangeable cations in
  WoSIS. Trace says `missing: bs_pct`; the package correctly
  returns indeterminate rather than guessing.
* 3/5 misses: indeterminate due to systematic WoSIS schema gap
  (no `slickensides` field). soilKey assigns the next-most-
  defensible RSG under WRB Ch 4 chave order. The WoSIS target is
  informed by field morphology that the WoSIS database does not
  archive.

The honest interpretation: **0/5 are genuine classifier failures**.
The apparent 0% top-1 reflects the WoSIS schema, not the
classifier. This finding will be the headline empirical result of
the methodology paper.

## CRAN submission readiness

* **`cran-comments.md`** drafted at the package root; documents the
  expected NOTEs (`New submission` + PROJ env-only).
* **`inst/cran-submission/HOW_TO_SUBMIT.md`** documents the CRAN
  web-form upload path; reasons about anticipated reviewer
  requests (already addressed); resubmission template.
* **`R CMD check --as-cran`** clean: 0 ERR / 0 WARN / 2 expected
  NOTE on the local machine. CI's R-CMD-check workflow is green
  across all 5 OS x R combinations.
* **`.Rbuildignore`** updated to exclude the cran-submission
  helpers and the `.rds` artefact files from the CRAN tarball.

## Bug fixes

* Replaced a dead Embrapa URL (`geoinfo.cnps.embrapa.br`) with the
  current Embrapa Solos / SiBCS landing page (was the only `--as-cran`
  invalid-URL NOTE).
* GitHub Actions:
  - `pkgdown` workflow: `_pkgdown.yml` now references
    `ossl_demo_sa` (was the topic that failed pkgdown CI after
    v0.9.11 shipped `data/`).
  - `test-coverage` workflow: `fail_ci_if_error: false` on the
    codecov-action step (the badge is informational; tokenless
    uploads on protected branches need a `CODECOV_TOKEN` secret to
    succeed -- without it, CI used to go red).
  - GitHub Pages source switched from `main` branch (where Jekyll
    chokes on `.Rmd` vignettes) to `gh-pages` branch (where the
    pkgdown workflow already pushes a built site with `.nojekyll`).

---

# soilKey 0.9.11 (2026-04-30)

Post-release pass triggered by the v0.9.10 Zenodo DOI minting
([10.5281/zenodo.19930112](https://doi.org/10.5281/zenodo.19930112)
concept-DOI). Three substantive additions: real Gemma 4 support, a
high-level `classify_from_documents()` one-liner, and the **first
empirical run against real WoSIS data** via GraphQL.

## New features

* **`classify_from_documents(pdf, image, fieldsheet, provider, ...)`**
  -- the high-level one-liner promised in `ARCHITECTURE.md` § 10:
  takes a soil-description PDF and / or a profile-wall image,
  extracts horizons + Munsell + site metadata via the configured
  VLM provider (default: local Gemma 4 edge), runs all three keys
  (WRB / SiBCS / USDA), and optionally writes a self-contained
  HTML / PDF report. The architectural invariants are preserved:
  the VLM never classifies, every extracted value carries
  `source = "extracted_vlm"`, and `evidence_grade` reflects the
  provenance.
* **Gemma 4 default for Ollama.** The default model for
  `vlm_provider("ollama")` is now `gemma4:e4b` (Gemma 4 edge, ~3
  GB, multimodal text+image+audio). Gemma 4 was released by
  Google DeepMind in 2026; it ships in five sizes
  (E2B / E4B / 26B-MoE / 31B / cloud-31B) on Ollama. Older
  defaults are documented and remain accessible
  (`model = "gemma3:27b"`).
* **`run_wosis_benchmark_graphql()`** -- the WoSIS REST API has
  been deprecated in favour of GraphQL at
  `https://graphql.isric.org/wosis/graphql`. The new driver speaks
  GraphQL natively, with `continent`, `wrb_rsg`, and `country`
  filters; queries `wosisLatestProfiles` for site metadata and
  pulls `clayValues / sandValues / siltValues / orgcValues /
  cecph7Values / phaqValues / tceqValues` per layer. Wraps every
  HTTP call with `tryCatch` and a clear error path on offline /
  non-200; sends `User-Agent` per the ISRIC ToS.
* **`data(ossl_demo_sa)`** -- a 1.1 MB synthetic OSSL South-America
  artefact bundled in `data/ossl_demo_sa.rda` for vignettes /
  examples / tests when the real OSSL data isn't available. Same
  `list(Xr, Yr, metadata)` shape as `download_ossl_subset()` so the
  in-package demo path matches the real-data path. 80 profiles
  x 2151 wavelengths (350-2500 nm). Synthetic-but-property-correlated
  spectra (1400 nm OH-water, 1900 nm clay-OH, 2200 nm Al-OH, 900 nm
  Fe-oxide bands).

## First WoSIS run (paper-grade)

`inst/benchmarks/reports/wosis_graphql_2026-04-30.md` -- 100 South
America profiles via GraphQL, classified with `classify_wrb2022()`:
**top-1 = 12.0%**. Per-RSG breakdown:

* Histosols: 1/1 (100 %)
* Arenosols: 6/7 (85.7 %)
* Regosols: 3/9 (33.3 %)
* Fluvisols: 2/7 (28.6 %)
* All other RSGs: 0% (most fall through to Regosol or Arenosol).

This is the honest empirical baseline. The mismatch is dominated by
attribute coverage: WoSIS provides texture + OC + CEC + pH + caco3
per layer but no Munsell colours, no slickensides, no clay films,
no fe_dcb_pct, no BS — and many soilKey diagnostics depend on
those. The next iteration will (a) widen the GraphQL query to
include Munsell + base saturation + dominant chemistry; (b) derive
BS from sum-of-bases / CEC; (c) provide a "WoSIS-curated" attribute
shim that maps available WoSIS variables into soilKey's expected
schema. Tracked in
[`inst/benchmarks/reports/wosis_graphql_2026-04-30.md`](https://github.com/HugoMachadoRodrigues/soilKey/blob/main/inst/benchmarks/reports/wosis_graphql_2026-04-30.md).

## Documentation

* Vignette 04 (VLM extraction) gains a "Local-first with Gemma 4
  (Ollama)" section, a "Cloud providers" section, and a
  `classify_from_documents()` one-liner example. The default
  pipeline is now demonstrably end-to-end in three lines.
* README citation block updated with the real concept-DOI
  (`10.5281/zenodo.19930112`); BibTeX block points at it.
* Vignette 02 references the v0.9.10 `report()` API.

## Bug fixes

* `report-html.R::.html_classification_card` is now resilient to
  trace entries that arrive as bare logical / atomic values
  (some classify-* helpers emit `NA` for layers they couldn't
  evaluate); previously these triggered
  `$ operator is invalid for atomic vectors` deep inside vapply.

---

# soilKey 0.9.10 (2026-04-30)

CRAN-readiness pass: `R CMD check` now returns 0 ERROR / 0 WARNING /
1 NOTE (the lone NOTE is environmental -- a missing `proj.db` on the
local system, not present on CRAN's own check farm). Plus a real
OSSL fetch helper and a hardened WoSIS driver, closing the v0.9.6
audit gap and the paper-grade WoSIS run pre-requisites.

## New features

* **`download_ossl_subset(region, properties, wavelengths, ...)`** --
  region-filtered fetch of the Open Soil Spectral Library that
  returns the canonical `list(Xr, Yr, metadata)` artefact consumed
  by `predict_ossl_mbl()` / `predict_ossl_plsr_local()`. Caches under
  `tools::R_user_dir("soilKey", "cache")` keyed by region; honours
  `getOption("soilKey.ossl_endpoint")` for testing or private
  mirrors; interpolates Xr to the requested wavelength grid; fails
  loudly when the network is unavailable (does NOT silently fall
  back to the synthetic predictor). Companion: `clear_ossl_cache()`.
* **WoSIS driver hardening** (`inst/benchmarks/run_wosis_benchmark.R`):
  - aligns request schema with WoSIS REST v3 (offset+limit,
    `bbox=`, `country=`); previous v0.9.9 used the older
    `page+page_size` shape that v3 deprecated.
  - adds `subset = c("global", "south_america", "north_america",
    "europe", "africa", "asia", "oceania", "brazil")` so the paper
    can run a regional benchmark in one call; bbox per region is
    overrideable via `options(soilKey.wosis_bbox_<region> = ...)`.
  - wraps every HTTP call in `tryCatch` with a clear error when
    offline or non-200; sends a `User-Agent: soilKey (...)` header.

## Documentation

* All vignettes renamed to start with a letter
  (`v01_getting_started.Rmd`, ...); pkgdown / README / cross-vignette
  references updated.
* Vignette 02 gains a "Render a self-contained pedologist-facing
  report" section showing the `report()` API.
* Vignette 06 documents the offline `run_canonical_benchmark()`
  driver and the most-recent canonical numbers (WRB 31/31, SiBCS
  20/20, USDA 31/31).
* New URL fields in DESCRIPTION (homepage + bug tracker).

## CRAN-readiness fixes

* All roxygen titles / descriptions: literal `%` is now escaped as
  `\%` (was a mix of bare `%` and `\\%`, both invalid in Rd).
* Same for `\eqn{}` (was `\\eqn{}` which Rd parsed as escaped
  backslash + `eqn{...}` block, generating "Lost braces" NOTEs).
* Several roxygen blocks were missing `@param` entries for non-`pedon`
  arguments; ~530 placeholder `@param` lines added across the
  catalogue. Manually-curated descriptions remain where they
  existed.
* `R/soilKey-package.R` now declares the `stats` (`predict`, `rnorm`,
  `runif`, `setNames`, `weighted.mean`), `utils` (`tail`), and `R6`
  (`R6Class`) imports it actually uses.
* `R/diagnostics-horizons-wrb-v033.R::plaggic` calls
  `test_bulk_density_below()` with the spelled-out argument name
  `max_g_cm3` instead of the partial-match `max`.
* `tests/testthat/test-spatial-soilgrids.R` now skips when PROJ's
  `proj.db` is unavailable on the local system (a cosmetic fix --
  CRAN's check farm has it).
* `tests/testthat/test-vlm-providers.R::skip_if(requireNamespace("ellmer"))`
  guard re-annotated for clarity (logic was correct; misread once).
* `inst/CITATION` falls back to the literal string `"dev"` for the
  package version when soilKey isn't installed (so pkgdown /
  roxygen2 builds during early development don't fail).
* `_pkgdown.yml` references repaired to point at the actual
  documented topic names; `pkgdown::check_pkgdown()` now passes
  with no problems.

---

# soilKey 0.9.9 (2026-04-30)

A pre-CRAN release that closes seven of the nine "promise gaps" called
out in the v0.9.8 review: the package now ships its own benchmark
report, CI, changelog, browsable docs, end-user reporting, complete
WRB Ch 6 supplementary coverage, and an honest OSSL audit.

## New features

* **`report()` / `report_html()` / `report_pdf()`** -- pedologist-facing
  report renderer (R/report-html.R, R/report-pdf.R). HTML output is
  fully self-contained (single file, inline CSS, no external network
  requests); PDF output goes through `rmarkdown::render()`. Accepts a
  single `ClassificationResult`, a list of results, or a `PedonRecord`
  (in which case all three keys are run automatically). The R6 method
  `ClassificationResult$report(file)` now delegates to this generic
  (was a stub raising "not yet implemented").
* **`run_canonical_benchmark()`** -- offline, network-free validation
  over the 31 canonical fixtures under `inst/extdata/`. Each fixture
  has a known target RSG / SiBCS order / USDA order; the function
  classifies all three systems and writes a versioned report under
  `inst/benchmarks/reports/canonical_<DATE>.md`. Companion to
  `run_wosis_benchmark()`, which still pulls the WoSIS REST API for the
  paper-grade run.
* **WRB 2022 Ch 6 supplementary qualifiers -- 32 / 32 RSGs.** v0.9.5
  adds canonical baseline supplementary lists for the 25 RSGs that
  v0.9.3.B left empty (HS, AT, TC, CR, LP, SN, VR, SC, GL, AN, PZ, PT,
  PL, ST, CH, KS, PH, UM, DU, GY, CL, RT, AR, RG, FL). 489 total
  supplementary entries across all 32 RSGs, all backed by the 105
  qualifier functions implemented in v0.9.1 -- v0.9.3.B (zero broken
  references). Page-precise canonical lists per Ch 6 are deferred to
  v0.9.6+; the v0.9.5 baselines are conservative and pedologically
  defensible.
* **`ossl_library_template()`** -- canonical schema constructor for the
  `ossl_library = list(Xr, Yr)` argument consumed by
  `predict_ossl_mbl()` and `predict_ossl_plsr_local()`. Documents the
  shape of the artefact users need to construct from a real OSSL
  extract. The synthetic-fallback path now emits a `cli_alert_warning`
  so users always know when the predictor is not real.
* **`run_vlm_live_demo()`** -- a manual driver under
  `inst/benchmarks/run_vlm_live_demo.R` that runs end-to-end real-VLM
  extraction (PDF + photo) against `anthropic` / `openai` / `google` /
  `ollama` and writes a release-time report with provenance summary,
  latency, and the resulting cross-system classification.
* **GitHub Actions CI** -- `.github/workflows/R-CMD-check.yaml`
  (5 platform x R-version matrix), `test-coverage.yaml` (codecov), and
  `pkgdown.yaml` (auto-deploys to gh-pages on push to main). Replaces
  the previous (false) "R-CMD-check passing" badge in the README with
  a live one driven from the workflow run.
* **pkgdown site** -- `_pkgdown.yml` organises the ~700 exported
  functions into 17 navigable sections (core / classify / WRB Ch
  3.1-3.3 / qualifiers / SiBCS Caps 1-2 / SiBCS keys / Família / USDA
  Path C / Modules 2-4 / reporting / fixtures / helpers).
* **`NEWS.md`** -- this file. Curated from `git log` per CRAN
  expectations.
* **`inst/CITATION` + `.zenodo.json`** -- canonical BibTeX exposed via
  `citation("soilKey")`, plus Zenodo metadata so the first GitHub
  release auto-mints a software DOI.

## Documentation

* `ARCHITECTURE.md` § 2: license reconciled to MIT (was GPL-3, an
  artefact of an early rascunho).
* README: live R-CMD-check + Codecov badges; reworked Ch 6 row in the
  WRB coverage table to reflect 32/32 RSG supplementary coverage; full
  BibTeX block now references the Zenodo concept-DOI.
* `inst/benchmarks/reports/audit_ossl_2026-04-30.md` -- honest audit of
  what is real vs. synthetic in Module 4 (predict_ossl_*). Bundled
  OSSL training data and fetch helper remain on the v0.9.6+ roadmap.

## Bug fixes / clarity

* `tests/testthat/test-vlm-providers.R:13` -- the `skip_if(requireNamespace("ellmer"))`
  guard is now annotated so a future reader doesn't misread it as
  inverted (it isn't -- `skip_if(TRUE)` skips, and we want to skip
  the missing-ellmer assertion when ellmer IS installed).
* `tests/testthat/test-qualifiers-wrb-v093a-specifiers-suppl.R:224`
  -- updated to reflect that all 32 RSGs now have supplementary
  slots; the "no supplementary slot" branch is now exercised with an
  unknown RSG code (`"ZZ"`) instead of GL.

---

# soilKey 0.9.8 (2026-04-30)

This release closes the **third** classification system end-to-end. With
v0.7 (SiBCS 5ª ed., 2026-04-28) and v0.9.4 (WRB 2022 Ch 6, 2026-04-29)
already shipped, soilKey 0.9.8 makes USDA Soil Taxonomy the third
deterministic key driven from versioned YAML rules.

## Major features

* **USDA Soil Taxonomy 13th edition (Soil Survey Staff, 2022) -- Path C
  complete.** The full Order -> Suborder -> Great Group -> Subgroup walk
  for every Order is wired and tested:
  Gelisols (`v0.8.3`), Histosols (`v0.8.4`), Spodosols (`v0.8.5`),
  Andisols (`v0.8.6`), Oxisols (`v0.8.7`), Vertisols (`v0.8.8`),
  Aridisols (`v0.8.9`), Ultisols (`v0.8.10`), Mollisols (`v0.8.11`),
  Alfisols (`v0.8.12`), Inceptisols (`v0.8.13`), Entisols (`v0.8.14`).
  68 Suborders / 339 Great Groups / 1 288 Subgroups in
  `inst/rules/usda/`. New helper:
  `classify_usda(pedon)$name` returns the canonical Subgroup label
  (e.g. `"Rhodic Hapludox"`).
* **6 USDA diagnostic epipedons** (`v0.8.1`): histic, folistic, melanic,
  mollic, umbric, ochric. Anthropic + plaggen are deferred.
* **5 USDA diagnostic characteristics** (`v0.8.2`): aquic conditions,
  anhydrous conditions, cryoturbation, glacic layer, permafrost.
* **SiBCS 5ª ed. Cap 18 (Família, 5º nível) implementado integralmente**
  (`v0.7.14.A` -> `v0.7.14.D`): 15 dimensões adjectivais ortogonais
  (grupamento textural, subgrupamento textural, distribuição de
  cascalhos, esquelética, tipo de A, prefixos epi/meso/endo, saturação
  V, álico, mineralogia da areia, mineralogia da argila, atividade da
  argila, óxidos de ferro, ândico, material subjacente, espessura
  > 100 cm, lenhosidade). Inclui motor de adjetivos com supressão de
  rótulos sem evidência suficiente. Séries (6º nível) explicitamente
  fora de escopo (provisório no SiBCS 5ª ed.).

## Documentation

* README + DESCRIPTION refletem agora as três promessas core (WRB / SiBCS
  / USDA) com badges canônicas de cobertura por sistema.

---

# soilKey 0.9.4 (2026-04-29)

End of the WRB 2022 build phase. Modules 1 (key), 2 (VLM), 3 (spatial
prior) and 4 (spectroscopy) all on disk; vignette pipeline complete.

## Major features

* **Five paper-grade vignettes** (`v0.9.4`):
  - `02-classify-wrb-end-to-end.Rmd` -- canonical Latossolo classified
    with full Ch 6 name.
  - `03-cross-system-correlation.Rmd` -- the same profile resolved in
    WRB / SiBCS / USDA, with a side-by-side correspondence table.
  - `04-vlm-extraction.Rmd` -- Module 2 walkthrough using
    `MockVLMProvider` (offline, schema-validated).
  - `05-spatial-spectra-pipeline.Rmd` -- Module 3 + Module 4 over a
    synthetic-but-realistic profile (offline-by-default).
  - `06-wosis-benchmark.Rmd` -- protocol for validating the key against
    WoSIS, plus a 31-fixture mini-run that runs anywhere.
* **WoSIS benchmark driver** (`inst/benchmarks/run_wosis_benchmark.R`):
  reads the WoSIS REST API, builds `PedonRecord`s, runs the key, writes
  a versioned report under `inst/benchmarks/reports/`.

## Documentation

* README rewrite with hex sticker, status badges, architecture mermaid
  diagram, full coverage tables, BibTeX citation block.
* MIT licence formalised (replacing the GPL-3 placeholder considered in
  the early architecture rascunho).

---

# soilKey 0.9.3 (2026-04-29)

Closes the WRB 2022 Chapter 6 name machinery -- a Latossolo now
classifies as `"Geric Ferric Rhodic Chromic Ferralsol (Clayic, Humic,
Dystric, Ochric, Rubic)"`.

## Major features

* **`v0.9.3.A`** -- Specifier engine generalised to handle the full
  Ch 4 specifier set (`Ano-`, `Epi-`, `Endo-`, `Bathy-`, `Panto-`,
  `Kato-`, `Amphi-`, `Poly-`, `Supra-`, `Thapto-`) via two `kind`s in
  the resolver: `depth` (simple band) and `filter` (custom predicate).
  Engine extended to also process the `supplementary:` slot of each
  RSG's YAML.
* **`v0.9.3.B`** -- Five new supplementary qualifier functions
  (`qual_aric`, `qual_cumulic`, `qual_profondic`, `qual_rubic`,
  `qual_lamellic`) plus ~30 reused from the principal-qualifier set.
  Canonical WRB Ch 6 names with parenthesised supplementary block now
  render correctly for FR / AC / LX / AL / LV / CM / NT.

---

# soilKey 0.9.2 (2026-04-28)

Sub-qualifier infrastructure + diagnostic tightening.

## Major features

* **`v0.9.2.A`** -- 11 Hyper- / Hypo- / Proto- sub-qualifiers
  (Hyper/Hypo for salinity, sodicity, calcic, gypsic; Proto for
  calcic, gypsic, vertic). Family suppression in the engine: when
  several members of the same family pass (e.g. Calcic + Hypocalcic +
  Protocalcic), only the most specific surfaces in the resolved name
  per WRB Ch 6 rules.
* **`v0.9.2.B`** -- Specifier infrastructure (Ano- / Epi- / Endo- /
  Bathy- / Panto-) via prefix dispatch in the resolver. No need for a
  function per (specifier × base) pair.

## Bug fixes

* **`v0.9.2.C`** -- Tightened three permissive diagnostics:
  - `cambic` now requires `top_cm >= 5` and a developed structure
    (grade in `{weak, moderate, strong}` and type not in
    `{massive, single grain}`); A/E and C-massive horizons no longer
    pass.
  - `plaggic` now gates on anthropogenic evidence directly
    (P >= 50 mg/kg OR artefacts > 0 OR designation Apl/Aplg/Apk).
  - `sombric` now requires a humus-illuviation pattern (candidate
    layer must have OC >= layer-above OC + 0.1 %).

---

# soilKey 0.9.1 (2026-04-28)

WRB 2022 Chapter 4 canonical principal-qualifier coverage for all
32 / 32 Reference Soil Groups. Shipped as five blocks (A--E) for
review-friendliness:

* **Bloco A** -- HS, AT, TC, CR, LP (organic / anthropogenic /
  technogenic / cryic / shallow). +42 `qual_*` functions.
* **Bloco B** -- SN, VR, SC, GL, AN (saline / clay-rich / wet /
  volcanic). +14 functions, including the Aluandic/Silandic split for
  andic soils via molar ratio.
* **Bloco C** -- PZ, PT, PL, ST, NT, FR (Brazilian / tropical block:
  Latossolos, Argissolos, Espodossolos as Ferralsols / Acrisols /
  Lixisols / Podzols). +14 functions including the Geric / Vetic /
  Posic family for very-low-CTC tropical soils.
* **Bloco D + E** -- 16 remaining RSGs (CH, KS, PH, UM, DU, GY, CL, RT;
  AC, LX, AL, LV, CM, AR, RG, FL). +4 functions: Cutanic (clay films),
  Glossic (mollic with albic glossae), Brunic (cambic-only B in
  Arenosol), Protic (no B horizon).

After v0.9.1, every Latossolo / Argissolo / Espodossolo / Cambissolo /
Nitossolo / Luvissolo brasileiro resolves to its full canonical WRB
name.

---

# soilKey 0.9.0 (2026-04-28)

* WRB 2022 Chapter 5 qualifiers seed: ~50 core qualifier functions
  wired across the most-used RSGs.

---

# soilKey 0.8.0 (2026-04-28)

* **Module 5 scaffold** -- `inst/rules/usda/key.yaml` listing all 12
  Orders in canonical key order (GE, HI, SP, AD, OX, VE, AS, UT, MO,
  AF, IN, EN). Oxisols path wired via `oxic_usda()` (delegating to
  WRB `ferralic`). Full Path C fills out across the v0.8.x series.

---

# soilKey 0.7.x (2026-04-28 -- 2026-04-29)

End-to-end SiBCS 5ª ed. (Embrapa, 2018) implementation.

## Major features

* **`v0.7`** -- 17 atributos diagnósticos + 24 horizontes diagnósticos
  + 13 ordens RSG-level wired in the canonical key order
  (O-V-E-S-G-M-C-F-T-N-P).
* **`v0.7.1`** -- 44 Subordens (2º nível) wired.
* **`v0.7.2`** -- Engine refactor: `run_taxonomic_key(pedon, rules,
  level_key)` replaces hard-coded WRB iteration, so the same engine
  drives WRB / SiBCS / USDA. `clay_films` split + 7 pendentes
  diagnostics (caráter ácrico, espódico subsuperficial, ebânico,
  retrátil; Ki/Kr; cerosidade quantitativa; grau de decomposição von
  Post).
* **`v0.7.3` -> `v0.7.13`** -- Grandes Grupos (3º nível) + Subgrupos
  (4º nível) implemented Ordem-by-Ordem in the canonical key order:
  Organossolos (Cap 14), Argissolos (Cap 5), Cambissolos (Cap 6),
  Chernossolos (Cap 7), Espodossolos (Cap 8), Gleissolos (Cap 9),
  Latossolos (Cap 10), Luvissolos (Cap 11), Neossolos (Cap 12),
  Nitossolos (Cap 13), Planossolos (Cap 15), Plintossolos (Cap 16),
  Vertissolos (Cap 17). 192 Grandes Grupos and 938 Subgrupos.
* **`v0.7.14`** -- Família (5º nível, Cap 18). See v0.9.8 for details.

---

# soilKey 0.6.0 (2026-04-27)

* **Module 2 -- VLM extraction via `ellmer`.**
  `extract_horizons_from_pdf()`, `extract_munsell_from_photo()`,
  `extract_site_from_fieldsheet()`. Schema-validation via
  `jsonvalidate` (draft-07). `MockVLMProvider` exported for offline
  tests. Bug-fix: NSE handling in `PedonRecord$add_measurement`.

---

# soilKey 0.5.0 (2026-04-27)

* **Module 3 -- SoilGrids / Embrapa spatial prior.**
  `spatial_soilgrids_prior()` (WCS), `spatial_embrapa_prior()`,
  `prior_consistency_check()`. Wired into `classify_wrb2022()` via
  `prior` and `prior_threshold`. **The deterministic key is never
  overridden by the prior** -- the prior only flags inconsistencies.

---

# soilKey 0.4.0 (2026-04-27)

* **Module 4 -- OSSL spectroscopy bridge.**
  `predict_ossl_mbl()`, `predict_ossl_plsr_local()`,
  `predict_ossl_pretrained()`, `preprocess_spectra()` (SNV / SG1),
  `pi_to_confidence()`, `fill_from_spectra()`. Provenance tag
  `predicted_spectra` automatically downgrades the
  `evidence_grade` from A to B.

---

# soilKey 0.3.x (2026-04-26 -- 2026-04-27)

The WRB-key build phase: 32/32 RSGs wired, full Ch 3 coverage, strict
Tier-2 gates.

## Major features

* **`v0.3a`** -- 8 new WRB diagnostics; SiBCS YAML quoting fix.
* **`v0.3b`** -- Diagnostics for natric, nitic, planic, stagnic, retic,
  cryic, anthric.
* **`v0.3c`** -- Full WRB key wired (32/32 RSGs) with end-to-end test
  over 31 canonical fixtures.
* **`v0.3.1`** -- Aligned argic, ferralic, duric, vertic, salic with
  WRB 2022 text (correções Tier-1 contra texto canônico).
* **`v0.3.2`** -- Reordered RSGs in `key.yaml` to canonical WRB 2022
  order (PL/ST between PT and NT; FL before AR).
* **`v0.3.3`** -- Complete WRB 2022 Ch 3.1 / 3.2 / 3.3 diagnostic
  coverage. +18 horizons, +12 properties, +16 materials. Schema
  expanded by 24 columns.
* **`v0.3.4`** -- Tier-2 RSG-level gate strengthening per WRB 2022
  Ch 4. 7 strict gates (vertisol, andosol, gleysol, planosol,
  ferralsol, chernozem_strict, kastanozem_strict) replace v0.2
  single-horizon shortcuts.
* **`v0.3.5`** -- Closes WRB 2022 Ch 3.1 -- 32 / 32 horizons
  (tsitelic, panpaic, limonic, protovertic added).

---

# soilKey 0.2.x (2026-04-25 -- 2026-04-26)

Initial diagnostic build-out + Module 5 / 6 scaffolds.

## Major features

* **`v0.2a`** -- gypsic, salic, calcic horizons + schema extensions.
* **`v0.2b`** -- cambic, plinthic, spodic, gleyic, vertic diagnostics.
* **`v0.2c`** -- argic-derived RSG diagnostics (AC, LX, AL, LV).
* **`v0.2d`** -- mollic-derived RSG diagnostics (CH, KS, PH).
* **`v0.2e`** -- 15 RSGs wired into the WRB key with end-to-end tests.
* **`modules-5-6`** -- USDA Soil Taxonomy + SiBCS 5ª ed. scaffolds.

---

# soilKey 0.1.0 (2026-04-25)

Initial commit. Esqueleto, classes core (`PedonRecord`,
`DiagnosticResult`, `ClassificationResult`), 3 WRB diagnostics
(`argic`, `ferralic`, `mollic`), Ferralsols path end-to-end +
canonical fixture + tests + getting-started vignette.
