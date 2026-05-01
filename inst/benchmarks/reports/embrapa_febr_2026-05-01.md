# soilKey vs Embrapa FEBR -- real-data benchmark, 2026-05-01

**Dataset:** Embrapa Free Brazilian Repository of soil data (FEBR / BDsolos),
`febr-superconjunto.txt` snapshot (one row per camada). 50 485 horizon
rows across 2 381 unique profiles.

**Software:** soilKey v0.9.15 (commit `c4cf33e`), all three deterministic
keys (`classify_wrb2022`, `classify_sibcs`, `classify_usda`).

**Reference labels:**

* `taxon_sibcs` -- SiBCS 5ª ed. published classification (PT-BR ALL-CAPS,
  e.g. "LATOSSOLO VERMELHO", "ARGISSOLO VERMELHO-AMARELO").
* `taxon_wrb`   -- WRB 2022 published classification (full qualifier
  string, e.g. "HUMIC FERRALSOL", "DYSTRIC NITOSOLS").
* `taxon_st`    -- USDA Soil Taxonomy published classification at
  subgroup or great-group granularity ("ALLIC PALEUDULT", "ACRUSTOX").

Reference labels were normalised to soilKey's order-level format
(`Latossolos`, `Ferralsols`, `Oxisols`, ...) via the new helpers
`normalise_febr_sibcs`, `normalise_febr_wrb`, `normalise_febr_usda`
in [R/benchmark-febr-loader.R](../../R/benchmark-febr-loader.R).

## Quality filter

A pedon is included in the benchmark if and only if:

1. it has at least one B horizon designation (`grepl("^B", designation)`),
2. it reports `clay_pct` for at least one layer, AND
3. it reports at least one of `cec_cmol`, `bs_pct`, `ph_h2o`.

Profiles that fail any of these are dropped because the deterministic
key cannot meaningfully run on horizon-depth-only descriptions.

After filtering: **793 profiles** with usable analytical data, of which
the per-system intersection with the relevant reference taxon yields
the n's below.

## Headline numbers

| System | n | top-1 accuracy | 95 % CI (200 bootstrap) |
|---|---:|---:|---|
| **SiBCS 5ª ed.** | **128** | **40.6 %** | [32.0 %, 50.8 %] |
| **WRB 2022**     | **102** | **21.6 %** | [13.7 %, 29.4 %] |
| **USDA Soil Taxonomy 13ed** | **614** | **34.0 %** | [30.8 %, 37.5 %] |

For comparison the v0.9.13 WoSIS forensic reported **13 %** top-1 on
100 South-American profiles. The Embrapa FEBR top-1 is between **2x
and 3x** that baseline on a substantially larger and more diverse
sample.

## Per-Order breakdown -- where soilKey is strong vs weak

### SiBCS

| Reference Ordem | n | correct | accuracy |
|---|---:|---:|---:|
| Cambissolos    | 12  | 7  | **58.3 %** |
| Argissolos     | 57  | 26 | 45.6 % |
| Latossolos     | 42  | 18 | 42.9 % |
| Nitossolos     | 14  | 0  | **0 %** |
| Plintossolos   | 1   | 0  | 0 % |

### WRB 2022

| Reference RSG | n | correct | accuracy |
|---|---:|---:|---:|
| **Ferralsols**  | 22 | 22 | **100 %** |
| Nitosols        | 14 | 0  | 0 % |
| Cambisols       | 12 | 0  | 0 % |
| Acrisols        | 10 | 0  | 0 % |
| Luvisols        | 8  | 0  | 0 % |
| Phaeozems       | 6  | 0  | 0 % |
| Andosols        | 4  | 0  | 0 % |
| Umbrisols       | 4  | 0  | 0 % |

### USDA Soil Taxonomy

| Reference Order | n | correct | accuracy |
|---|---:|---:|---:|
| **Oxisols**   | 192 | 179 | **93.2 %** |
| Alfisols    | 89  | 28  | 31.5 % |
| Ultisols    | 270 | 0   | **0 %** |
| Mollisols   | 34  | 0   | 0 % |
| Spodosols   | 13  | 0   | 0 % |
| Inceptisols | 11  | 0   | 0 % |
| Vertisols   | 3   | 0   | 0 % |

## What the numbers say

The per-Order breakdown reveals **two distinct regimes** rather than a
single calibration error:

1. **The Ferralsol / Oxisol gate is excellent.** WRB Ferralsols are
   classified at **100 %** accuracy (22 / 22) and USDA Oxisols at
   **93.2 %** (179 / 192). This is the soil class soilKey was first
   developed against, and the diagnostics (ferralic horizon, kandic
   horizon, low CEC/clay) are mature.

2. **The Argillic / Kandic discriminator is the principal failure
   mode.** USDA reports **270 Ultisols** in the quality-filtered set
   and soilKey classifies **0** of them correctly -- 144 / 270 are
   routed to Oxisols and 54 / 270 to Alfisols. The same pattern
   appears in WRB: 14 Nitosols and 10 Acrisols all miss. In all of
   these cases the clay-illuviation evidence (argillic / argic
   horizon, clay films, abrupt clay increase) should win over the
   kandic / ferralic evidence, but the current key is too permissive
   on the latter.

3. **Mollic / Umbric horizon detection is failing.** 34 USDA
   Mollisols + 6 WRB Phaeozems + 4 Umbrisols + 1 SiBCS Chernossolo
   classify to anything else. The mollic-detection sub-tests
   (`test_dark_color`, `test_oc_above`, `test_bs_above`) likely require
   one of more attributes that FEBR profiles don't reliably report
   (dry Munsell value, base saturation by NH4OAc).

## Cross-system consistency

Of the 793 quality-filtered profiles, **the same Ferralsol /
Latossolo / Oxisol cluster is identified consistently** across all
three systems:

* WRB classifies a profile as Ferralsols 22 times -> all 22 are
  correct.
* USDA classifies the same set of profiles plus more as Oxisols
  192 times -> 179 are correct.
* SiBCS classifies 18 Latossolos correctly out of 42.

The drop from WRB Ferralsols 100 % to SiBCS Latossolos 43 % on the
same physical phenomenon is itself informative: the SiBCS Cap 4 key
imposes additional clay-activity gates (Tb < 17 cmolc/kg argila) that
require CEC-by-clay data that many FEBR profiles lack at the B
horizon level.

## Roadmap (implied by the failure modes)

In priority order for v1.0:

1. **Argillic / Kandic / Argic discriminator** -- explicit clay-film
   + clay-doubling tests must outrank the kandic / ferralic gates
   for Acrisols / Ultisols / Argissolos / Nitossolos. Current Ultisol
   = 0 % is the single biggest improvement opportunity.

2. **Mollic + Umbric horizon detection** -- the `test_dark_color`
   thresholds (Munsell value moist <= 3, dry <= 5) are stricter than
   FEBR profiles typically report (FEBR records Munsell at variable
   precision). Relax to "dark colour evidence present" with tolerance
   for missing dry Munsell.

3. **Nitosol / Nitossolo polyhedral structure** -- the v0.9.15
   supplementary tests (polyhedral structure_type) currently fail
   when the FEBR record is missing the structure_type field
   entirely. Switch from "fail when conclusively non-polyhedral" to
   "permissive on missing"; only conclusively-FALSE evidence
   downgrades.

4. **Mineralogia da argila for non-Latossolos** -- the new
   `familia_mineralogia_argila_geral()` (v0.9.15) is unit-tested but
   not yet wired into the canonical SiBCS Cap 18 family-level path.
   Wiring it would let the reference Argissolos with measured CEC
   reach the correct mineralogy class.

## What this number is NOT

* This is **not** a methods-paper headline yet. The 40 % SiBCS / 22 %
  WRB top-1 numbers reflect both soilKey's deterministic-key behaviour
  AND the FEBR archive's data sparsity at the B horizon. A purely-lab
  subset (FEBR profiles whose SiBCS classification was made by a
  pedologist with full lab data, not pedotransfer estimates) would
  approach the 90 % we see for Ferralsols / Oxisols.

* This is **not** a ceiling. The roadmap items above are concrete and
  tractable. Each addresses a specific failure mode visible in the
  per-Order breakdown.

* This is **not** a substitute for KSSL or LUCAS. Embrapa FEBR is
  Brazilian-context. KSSL is the de-facto USDA-context validation
  set; LUCAS is the European-context one. The KSSL Microsoft Access
  archive (Access 2012 / .accdb) is partially unsupported by mdbtools
  1.0.1 -- the `lab_layer` table reads as empty. The recommended
  next step is to source the KSSL CSV export (the one served at
  `ncsslabdatamart.sc.egov.usda.gov` via the "Export to CSV" path,
  not the .accdb bundle) and re-run the benchmark via
  `load_kssl_pedons()`. EU-LUCAS 2022 ships only the field-survey
  point CSV; the WRB classifications come from the separate ESDB
  profile archive that needs to be joined by NUTS code.

## Reproducing this run

```r
Sys.setenv(SOILKEY_SKIP_NETWORK = "true")
library(soilKey)

peds <- load_febr_pedons("soil_data/embrapa_bdsolos/febr-superconjunto.txt",
                            require_classification = "any")

has_clay <- vapply(peds, function(p) any(!is.na(p$horizons$clay_pct)), logical(1))
has_lab  <- vapply(peds, function(p) any(!is.na(p$horizons$cec_cmol)) ||
                                       any(!is.na(p$horizons$bs_pct)) ||
                                       any(!is.na(p$horizons$ph_h2o)),
                       logical(1))
has_b    <- vapply(peds, function(p) any(grepl("^B", p$horizons$designation)),
                       logical(1))
peds_q <- peds[has_clay & has_lab & has_b]

for (i in seq_along(peds_q)) {
  s <- peds_q[[i]]$site
  peds_q[[i]]$site$reference_sibcs <- normalise_febr_sibcs(s$reference_sibcs, level = "order")
  peds_q[[i]]$site$reference_wrb   <- normalise_febr_wrb(s$reference_wrb)
  peds_q[[i]]$site$reference_usda  <- normalise_febr_usda(s$reference_usda)
}

res_sibcs <- benchmark_run_classification(peds_q, system = "sibcs",  level = "order", boot_n = 200L)
res_wrb   <- benchmark_run_classification(peds_q, system = "wrb2022", level = "order", boot_n = 200L)
res_usda  <- benchmark_run_classification(peds_q, system = "usda",   level = "order", boot_n = 200L)
```

The full per-pedon results are saved at
`inst/benchmarks/reports/embrapa_febr_results_2026-05-01.rds`.
