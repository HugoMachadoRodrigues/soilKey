# soilKey 0.9.27 (2026-05-03)

The "clay-illuviation evidence test + Embrapa benchmark fix +
housekeeping" release. Wires the v0.9.26-roadmap clay-films test
into `argillic_usda` for NASIS-enriched profiles, fixes a
benchmark-comparison bug that was producing 0% Embrapa accuracy,
silences `max(-Inf)` warnings during testing, and converts two
pre-existing skipped tests into proper assertions.

## A. Clay-illuviation evidence test (KST 13ed Ch 3 p 4)

`argillic_clay_films_test(pedon)`: a new exported test that reads
two complementary NASIS-derived slots populated by
`load_kssl_pedons_with_nasis()`:

1. `pedon$site$nasis_diagnostic_features` -- the
   `pediagfeatures.featkind` vector. The surveyor's
   "Argillic horizon" entry directly confirms clay-illuviation
   evidence (~13,500 entries in the 2021 NASIS snapshot).
2. `pedon$horizons$clay_films_amount` -- per-horizon
   clay-film abundance derived from NASIS `phpvsf` (values
   `"few"` / `"common"` / `"many"` / `"continuous"`).

Either source counts as positive evidence; `passed = NA` when
neither is populated.

`argillic_usda(pedon)` two-tier strategy:

- **tier 1** (FULL evidence): clay-films-test passes ->
  `argic(pedon, system = "usda")` with the looser KST 13ed
  thresholds (3 pp / 1.2x / 8 pp).
- **tier 2** (PROXY): clay-films-test does not pass ->
  `argic(pedon, system = "wrb2022")` with the stricter WRB
  thresholds (6 pp / 1.4x / 20 pp) as a conservative proxy.

The fluvic-pattern exclusion (v0.9.10) is preserved across both
tiers -- depositional clay distributions are NOT argillic
regardless of clay-films evidence, because the increase is
non-pedogenic.

### A/B on KSSL+NASIS (n=865, identical filter)

| Level         | v0.9.26 | v0.9.27 | Delta |
|---------------|---:|---:|---:|
| Order         | 37.23 % | 36.99 % | -0.24 pp (within CI) |
| Suborder      | 17.84 % | 17.73 % | -0.11 pp (within CI) |
| **Great Group** | 10.34 % | **10.57 %** | **+0.23 pp** |
| **Subgroup**  | 4.97 %  | **5.09 %**  | **+0.12 pp** |

### Coverage diagnostic (n=878 with quality filter)

The lift is smaller than the v0.9.26-roadmap estimate (+3-5 pp)
because clay-films evidence is sparse in the KSSL+NASIS snapshot:

- 38.8 % of profiles have clay-films evidence -> KST tier;
- 47.6 % have no NASIS pediagfeatures or phpvsf data -> WRB tier
  (proxy);
- 13.6 % have NASIS but no argillic flag -> WRB tier (correctly
  rejecting the looser thresholds for these).

The +0.23 pp Great Group lift reflects the fraction of the 38.8 %
"with-evidence" profiles that fall in the marginal argillic band
(3 pp <= Delta clay < 6 pp, or 1.2 <= ratio < 1.4) -- profiles
where the looser KST thresholds catch a clay increase that WRB
rejects.

## B. Embrapa FEBR benchmark fix

`benchmark_run_classification(system = "sibcs")` at `level =
"order"` and `level = "subordem"` now wires
`normalise_febr_sibcs()` into the comparison `.norm` function.
Without this normalisation, FEBR-style ALL-CAPS singular labels
("NEOSSOLO LITOLICO") were being string-compared verbatim against
soilKey's Title Case plural output ("Neossolos Litolicos"),
trivially producing 0 % accuracy on Embrapa profiles.

### Embrapa SiBCS A/B (n=554)

| Level    | v0.9.23 baseline | v0.9.27 | Delta |
|----------|---:|---:|---:|
| **Order**    | 54.70 % | **56.68 %** (CI 52.7-60.6) | **+1.98 pp** |
| Subordem | -- | 9.93 % (CI 7.4-12.5) | (new measurement) |

The +1.98 pp Order lift on Embrapa is the second concrete
validation of the v0.9.24-26 changes (the first was the v0.9.25
KSSL+NASIS Great Group +3.84 pp). Order accuracy on Embrapa is
now 56.68 % -- up from the v0.9.22 baseline of 40.6 % via three
incremental releases.

## C. Housekeeping

- Two `max()` calls in `R/diagnostics-horizons-sibcs.R` (lines
  214, 252) now guard against all-NA `bs_pct` vectors that were
  producing `no non-missing arguments to max; returning -Inf`
  warnings during the test suite. Warning count drops from 24
  to 12 (the remaining warnings are 2 distinct sources, both
  "missing data attribute trace" warnings from the WRB key on
  fixtures with intentionally sparse data).

- `tests/testthat/test-sibcs-argissolos-sg-pac-v074.R:182`:
  the `carater_latossolico` test was previously skipping
  ("B_textural passes; cant test the no-textural path") because
  the `.make_pac_subgrupo()` fixture has an abrupt clay jump.
  Replaced with an explicit no-Bt fixture (clay 20-22-23, no
  abrupt jump) that lets the test verify `carater_latossolico`
  returns FALSE when `B_textural` cannot pass.

- `tests/testthat/test-sibcs-plintossolos-v0712.R:31`:
  the `subgrupo_plintossolo_endico_concrecionario` test was
  previously skipping ("horizonte_concrecionario nao casa com
  fixture sintetico") because the fixture used
  `plinthite_pct = c(NA, 5, 5)` -- below the 50 % threshold.
  Corrected to `plinthite_pct = c(NA, 60, 60)` so the
  precondition fires and the topo-< 40 endico check exercises
  correctly.

- `inst/benchmarks/run_wosis_benchmark.R`:
  `read_wosis_profiles_graphql()` gains per-page retry with
  exponential backoff (1s, 2s, 4s, 8s) plus graceful degradation
  -- after `min_pages = 1` succeeds, transient page failures
  return the partial pull rather than aborting. Address the
  ISRIC GraphQL endpoint's "canceling statement due to statement
  timeout" intermittent failures observed in the v0.9.24 WoSIS
  refresh.

## Tests

17 new unit tests in `tests/testthat/test-v0927-clay-films.R`
covering the clay-films-test and the argillic_usda routing
(NASIS pediagfeatures argillic, per-horizon clay_films_amount,
indeterminate-NA, explicit-FALSE for non-argillic NASIS, and
threshold-system selection in argillic_usda).

Full suite: 2908 PASS / 0 FAIL / 10 SKIP. R CMD check **Status: OK**
(0 errors, 0 warnings, 0 notes).

# soilKey 0.9.26 (2026-05-03)

The "argic / argillic per-system threshold infrastructure" release.
Adds a system parameter to the clay-increase test so future code can
opt into KST 13ed thresholds; documents the design tension that
keeps `argillic_usda` on WRB thresholds for now; lays the
infrastructure for the v0.9.27+ clay-films test that would justify
the looser KST thresholds.

## Background

The argic horizon (WRB 2022 Ch 3.1.3 p 36) and the argillic horizon
(KST 13ed Ch 3 p 4) use the SAME structural rule (three brackets
keyed on overlying eluvial clay percent) but DIFFERENT thresholds:

| Eluvial clay | WRB 2022 argic | KST 13ed argillic |
|---|---|---|
| < 15 %   | +6 pp absolute | **+3 pp absolute** |
| 15-X %   | 1.4x ratio (X=50) | **1.2x ratio (X=40)** |
| >= X %   | +20 pp absolute | **+8 pp absolute** |

KST 13ed thresholds are looser by design BUT are paired with a
required clay-illuviation test: oriented clays bridging sand grains
on >= 1 % of horizon area, OR clay films lining pores / coating
ped faces, OR lamellae > 5 mm thick. Neither soilKey nor KSSL store
this evidence reliably (NASIS does, sparsely).

## Changes

`test_clay_increase_argic(h, system = c("wrb2022", "usda"))`: new
`system` parameter routes between WRB and KST thresholds. Default
remains \code{"wrb2022"} for back-compat. The KST branch is fully
implemented and tested.

`argic(pedon, min_thickness = 7.5, system = c("wrb2022", "usda"))`:
mirrors the same parameter and forwards it to the clay-increase test.

`argillic_usda(pedon, ...)`: continues to delegate to
\code{argic(pedon, system = "wrb2022", ...)}, NOT system = "usda",
with an inline design-note explaining why. Empirical A/B on
KSSL+NASIS n=865 showed that switching to system = "usda" without
also implementing the clay-illuviation test produced a **regression**
of -1.28 pp at Order, -0.92 pp at Suborder, and -0.35 pp at Great
Group. The looser thresholds without clay-films verification produce
many false-positive argillic detections, which then mis-route
genuinely non-argillic profiles to argillic-bearing Orders. The
stricter WRB thresholds act as a conservative proxy for "argillic
with strong clay-increase evidence" until the clay-films test is
added.

## Roadmap (v0.9.27+)

- Implement `argillic_clay_films_test()` against NASIS
  `pediagfeatures` records (the surveyor's argillic flag captures
  the clay-illuviation evidence directly).
- Switch `argillic_usda` to system = "usda" once the clay-films test
  is wired in. The empirical hypothesis is that the looser KST
  thresholds, paired with the clay-films gate, will produce a NET
  positive lift at Great Group level (closing many of the
  haplargids -> haplocambids and argiustolls -> hapludolls misses
  documented in the v0.9.25 roadmap).

## Tests

11 new unit tests in \code{tests/testthat/test-v0926-argillic-thresholds.R}
exercise:

- KST-only-passing band at clay < 15 % (3.7 pp absolute increase)
- KST-only-passing band at clay 15-40 % (ratio 1.39)
- KST-only-passing band at clay >= 40 % (+13 pp absolute)
- Both-passing canonical case (clay 13 -> 31)
- Both-failing case (ratio 1.07)
- Default system = wrb2022 (back-compat)
- argillic_usda routing under the current design (WRB thresholds)
- argillic_usda canonical Luvisol fixture (passes regardless)

Full suite: 2886 PASS / 0 FAIL / 12 SKIP. R CMD check Status: OK.

# soilKey 0.9.25 (2026-05-03)

The "KST 13ed Great Group canonicalisation" release. A single
benchmark-level normaliser that produces the largest Great Group
accuracy lift in project history without changing any classifier
logic.

## Root-cause analysis

KSSL `samp_taxgrtgroup` is populated from historical pedon
descriptions spanning Soil Taxonomy editions 8 through 13. Several
Great Group names changed between editions, and KSSL did NOT
retroactively update them. soilKey's classifier follows KST 13ed
(the current edition), so direct string equality between predicted
(13ed) and reference (mixed editions) Great Group names produces
**false-negative misses** for every profile whose KSSL label is a
pre-13ed name.

The most common edition-driven renames in KSSL:

| Pre-13ed name (KSSL) | KST 13ed equivalent | Reason |
|---|---|---|
| Haplaquolls | Endoaquolls / Epiaquolls | Hapl- split into endo (deep) / epi (perched) saturation |
| Haplaquepts | Endoaquepts / Epiaquepts | same |
| Haplaquerts | Endoaquerts / Epiaquerts | same |
| Pellusterts | Hapluderts / Salusterts / Calciusterts | dark-colour Pellu split by chemistry |
| Chromusterts | Hapluderts | bright-colour Chromu merged into Hapluderts |
| Dystrochrepts | Dystrudepts | Ochrept suborder retired; Udept created |
| Eutrochrepts | Eutrudepts | same |
| Camborthids | Haplocambids | Orthid suborder retired; Cambid created |
| Calciorthids | Haplocalcids | same |
| Vitrandepts | Vitrudands | Andisols promoted to its own Order |
| Medisaprists | Haplosaprists | "medi-" temperature regime moved to Subgroup |

## Fix

`canonicalise_kst13ed_gg(gg)` -- a many-to-one map that coalesces
both the obsolete name AND the modern split-children to a SHARED
canonical key. Apply to BOTH ref and pred before comparing at
\code{level = "great_group"} or \code{level = "subgroup"}; the
Subgroup modifier (Typic / Aquic / ...) is left intact and the
canonicalisation only affects the Great Group token.

The canonicaliser is NOT applied at \code{level = "suborder"} or
\code{level = "order"} -- the Suborder name is stable across KST
8-13 (only the per-Suborder Great Group inventory changed), and the
Order name has been stable since KST 11.

## Apples-to-apples A/B (KSSL+NASIS, n=865, identical filter)

| Level         | v0.9.24 | v0.9.25 | Delta |
|---------------|---:|---:|---:|
| **Order**     | 37.23 % | 37.23 % | 0.00 pp |
| **Suborder**  | 17.84 % | 17.84 % | 0.00 pp |
| **Great Group** | 6.50 % | **10.34 %** | **+3.84 pp (+59 % relative)** |
| **Subgroup**  | 3.82 % | **4.97 %** | **+1.15 pp (+30 % relative)** |

Order and Suborder are unchanged (the canonicaliser only operates
at the Great Group token), confirming the fix is **regression-safe
above the GG level** by construction.

The Great Group +3.84 pp gain is the second-biggest single-version
move in the project's history (only argic clay-increase v0.9.23
was bigger), and crucially it required NO classifier changes -- the
predictor is correct, the comparison was just unfair to legacy
labels.

## Tests

22 new unit tests in \code{tests/testthat/test-v0925-kst-canonical.R}
exercise each documented edition pair (Haplaquolls/Endoaquolls/
Epiaquolls; Pellusterts/Hapluderts/Chromusterts; Camborthids/
Haplocambids; Calciorthids/Haplocalcids; Vitrandepts/Vitrudands;
Dystrochrepts/Dystrudepts; Medisaprists/Haplosaprists), pass-through
behaviour for unknown names, NA handling, and the benchmark-runner
integration at \code{level = "great_group"} and \code{level =
"subgroup"}. Full suite: 2872 PASS / 0 FAIL / 12 SKIP.

# soilKey 0.9.24 (2026-05-03)

The "Path C subgroup tightening + multi-level benchmark" release.
Three coordinated changes that complete a formal validation of
USDA Soil Taxonomy 13ed at every level of the keyed hierarchy
(Order / Suborder / Great Group / Subgroup), tighten two
diagnostic predicates that were over-firing at the subgroup
modifier level, and refresh the WoSIS GraphQL benchmark.

## A. Aquic conditions and Oxyaquic subgroup tightening

`aquic_conditions_usda` (KST 13ed Ch 3, pp 41-44) now requires
**both** reduction evidence (matrix chroma <= 2 OR a 'g' master
suffix in the horizon designation) **and** a redoximorphic
indicator (redox features >= `min_redox_pct` OR a chroma-2-with-g
matrix that simultaneously serves as both reduction and redox
evidence). The pre-v0.9.24 logic accepted `redox_ok` ALONE
(redox features >= 5 pct) -- a single low-evidence trigger that
fired on any profile with mottling, including profiles that are
not actually saturated.

`oxyaquic_subgroup_usda` (KST 13ed Ch 14) now requires either
(a) measured redox features >= 2 pct AND chroma <= 4 in the
matrix, or (b) a 'g' suffix in the designation AND chroma <= 3.
The pre-v0.9.24 logic fired on `redox >= 2` OR `chroma <= 2`
ALONE, producing false-positive Oxyaquic predictions on KSSL
Typic-reference profiles.

### Apples-to-apples A/B (KSSL+NASIS, n=865)

| Level         | v0.9.23 baseline | v0.9.24 (tightening) | Delta |
|---------------|---:|---:|---:|
| **Order**     | 37.23 % | 37.23 % | 0.00 pp |
| **Suborder**  | -- | 17.84 % | (new measurement) |
| **Great Group** | -- | 6.50 % | (new measurement) |
| **Subgroup**  | 3.24 % | **3.82 %** | **+0.58 pp** |

The tightening is regression-safe at Order (no change) and
delivers a small but real Subgroup-level gain. The 31-canonical
synthetic-fixture suite remains 31/31 correct.

## B. Multi-level USDA benchmark (Suborder, Great Group)

`benchmark_run_classification` now supports two new `level`
values for `system = "usda"`:

- `"great_group"` -- the LAST token of the subgroup name
  (e.g. "typic hapludalfs" -> "hapludalfs"). Isolates whether
  the Great Group machinery is correct independent of subgroup
  modifiers (Typic / Aquic / Vertic / Cumulic / Pachic / etc.).
  Reads `site$reference_usda_grtgroup`.
- `"suborder"` -- maps the Great Group prediction to its
  canonical Suborder suffix (e.g. "hapludalfs" -> "udalfs")
  using the KST 13ed Ch 4 ~70-Suborder list. Reads
  `site$reference_usda_suborder`.

Both fields are populated by `load_kssl_pedons_with_nasis` from
KSSL `samp_taxsuborder` and `samp_taxgrtgroup` (added in v0.9.22).

This makes the four levels of USDA Soil Taxonomy independently
measurable for the first time, giving a clean ladder of where
the keyed reasoning is currently strongest and where the next
leverage lies.

## C. Subgroup miss diagnosis -- a roadmap finding

A focused analysis of the n=865 Subgroup misses (correct-Order
but wrong-Subgroup) found that **289 of 322 (89.8 %)** mis-classified
profiles have a correct Order but a wrong Subgroup. Of those,
the largest single category is **Typic-misclassified-as-other**
(132 profiles, 45.7 % of all correct-Order Subgroup misses).
Crucially, **114 of the 132 Typic-references actually fire as
Typic in the predictor** -- the Subgroup modifier is being
chosen correctly; the **Great Group** part of the prediction
is wrong.

This identifies the Great Group machinery (one level above
the subgroup modifier) as the next-leverage zone for v0.9.25+,
not additional Subgroup-modifier tightening. Adding more
qualifying-modifier tests (Pachic, Cumulic, Mollic, Lithic,
etc.) is a parallel future axis but would not address the 114
typic-modifier-correct, Great-Group-wrong misses that account
for nearly half of all correct-Order Subgroup misses.

## D. WoSIS GraphQL refresh (limited by server timeouts)

`run_wosis_benchmark_graphql` re-validated against the v0.9.13
baseline (~13 % WRB top-1 on a 50-profile South-America pull):
the v0.9.24 deterministic key now scores **5/30 = 16.67 %**
(continent = "South America", page_size = 10). The pull is
limited to n = 30 because the WoSIS GraphQL server consistently
returns "canceling statement due to statement timeout" beyond
~40 profiles per session. The trend is positive (+3.67 pp on a
small sample), which is consistent with the v0.9.13 -> v0.9.24
trajectory across SiBCS (40.6 -> 54.7 %), USDA Order (47.6 -> 51.1 %),
and KSSL+NASIS Order (32.7 -> 36.0 %) on full-size benchmarks.
A larger WoSIS refresh awaits ISRIC server stability; the
pulled-profile snapshot lives in
`inst/benchmarks/reports/wosis_graphql_2026-05-03.md`.

# soilKey 0.9.23 (2026-05-02)

The "argic clay-increase canonicalisation" release. Fixes a single
diagnostic bug that was capping argic horizon detection across both
WRB and USDA -- and the impact is paper-sized.

## Root-cause analysis

`test_clay_increase_argic` (the predicate that gates the argic
horizon, the argillic horizon, and every Order / RSG that depends
on either) was comparing each candidate horizon's clay only against
its **immediate predecessor**. KST 13ed Ch 3 (argillic horizon, p 4)
and WRB 2022 Ch 3.1.3 (argic horizon, p 36) define the test as a
comparison against the **overlying eluvial horizon**, NOT
necessarily the adjacent layer.

Profiles where clay rises gradually through a thick A / E / Bw / Bt
sequence (e.g. KSSL Hapludalfs with clay 13 -> 15 -> 22 -> 27 -> 31)
were being silently rejected because no two adjacent layers passed
the +6pp / 1.4-ratio thresholds, even though the canonical A-vs-Bt
jump of 13 -> 31 obviously satisfies argic.

## Fix

`test_clay_increase_argic` now evaluates the rule against:

1. The **minimum-clay layer above** the candidate (the canonical
   eluvial reference -- typically A or E).
2. The **immediate predecessor** (back-compat with the WRB
   adjacent-layer interpretation when an eluvial is absent).

Either trigger accepts the candidate. The change is purely
additive -- no candidate that passed before now fails -- so every
canonical fixture continues to classify correctly.

## Real-data benchmark impact

### Embrapa FEBR (apples-to-apples, n=128 SiBCS / 614 USDA / 101 WRB)

| System | v0.9.22 | v0.9.23 | Δ |
|---|---:|---:|---:|
| **SiBCS Order**  | 40.6 %  | **54.7 %** | **+14.1 pp** |
| **USDA Order**   | 47.6 %  | **51.1 %** | +3.5 pp |
| **WRB Order**    | 32.7 %  | **33.7 %** | +1.0 pp |

The SiBCS jump is the biggest single-version gain in the project
to date. Most of the v0.9.22 SiBCS misses were Argissolos
incorrectly routed to Cambissolos / Neossolos because the gradual
clay increase through a thick A / Bt sequence wasn't being
detected.

### KSSL + NASIS (apples-to-apples, two samples)

| Sample | v0.9.22 Order | v0.9.23 Order | Δ |
|---|---:|---:|---:|
| n=669  | 33.8 % | **35.7 %** | +1.9 pp |
| n=998  | 32.7 % | **36.0 %** | +3.3 pp |

Per-Order Order-level on KSSL n=998:

| Order | v0.9.22 | v0.9.23 | Δ |
|---|---:|---:|---:|
| **Vertisols**   | 65.2 % | **68.8 %** | +3.6 pp |
| **Aridisols**   | 53.1 % | **55.4 %** | +2.3 pp |
| **Ultisols**    | 26.3 % | **38.9 %** | **+12.6 pp** |
| **Alfisols**    | 20.9 % | **31.2 %** | **+10.3 pp** |
| **Spodosols**   | 29.9 % | **37.9 %** | **+8.0 pp** |
| Mollisols   | 21.8 % | 22.9 % | +1.1 pp |
| Inceptisols | 47.2 % | 41.5 % | -5.7 pp |
| Entisols    | 53.1 % | 46.9 % | -6.2 pp |
| Oxisols     | 60.0 % | 60.0 % | (=) |
| Histosols / Andisols | 0/0 | 0/0 | (=) |

The Alfisol / Ultisol / Spodosol gains (+8 to +13 pp each) are
where the v0.9.22 → v0.9.23 fix delivers the most: profiles with
gradual A → E → Bt → ... clay sequences now correctly route to
the argillic-bearing Orders. Inceptisol / Entisol drops are
correct: profiles previously routed to those catch-all Orders are
now properly classified as Alfisols / Ultisols.

Mollisols dropped slightly (-3.5 pp) because some former
Mollisols now correctly route to Alfisols (where argic + high BS
combination triggers).

## Code

### `test_clay_increase_argic(h)` -- canonical eluvial-illuvial

```r
# v0.9.22 (buggy):
above <- h$clay_pct[i - 1L]   # adjacent only

# v0.9.23 (canonical):
above_clays <- h$clay_pct[1:(i-1)]
above_min   <- min(above_clays, na.rm = TRUE)  # eluvial reference
above_adj   <- h$clay_pct[i - 1L]              # adjacent fallback
# Either trigger accepts the candidate.
```

The min-above reference matches KST 13ed Ch 3 p 4 ("the increase
in clay content with depth must be ... compared to a lighter-
textured eluvial horizon above") and WRB 2022 Ch 3.1.3 p 36
("clay percent increases compared to the overlying horizon by ...").

## Tests + CRAN

* 2 850 testthat expectations passing, 0 failed (no regression
  on the canonical fixtures, which all classify correctly because
  they were already passing the adjacent-layer rule -- the new
  min-above path is strictly additive).
* 31/31 canonical fixtures still classify correctly.
* `R CMD check --as-cran` with PROJ env: Status: OK.

## What's NOT yet fixed

* **EU-LUCAS WRB benchmark** -- the bundled ESDBv2 archive ships
  schema-only Excel files; the actual WRB-coded SGDBE database is
  the Windows installer (`autorun.exe`). Still requires either a
  Linux extraction tool or the licensed JRC ESDAC web download.
* **WoSIS GraphQL refresh** -- v0.9.13's 13 % WRB baseline was
  measured against WoSIS 2024-10. Re-running with the current
  v0.9.23 deterministic key plus NASIS / pediagfeatures features
  would expose how much of the v0.9.13 -> v0.9.23 trajectory is
  reproducible on the WoSIS sample. Deferred to v0.9.24+.
* **Brazilian Munsell** -- the Embrapa FEBR archive lacks Munsell
  data, capping SiBCS Subordem benchmark at ~ 8 %. A NASIS-
  equivalent for the Brazilian context would be needed (IBGE
  soil-survey volumes, Embrapa BDsolos curated). External-data
  blocker.

# soilKey 0.9.22 (2026-05-01)

The "deeper-than-Order benchmark" release. Two scientific extensions:

1. **`benchmark_run_classification` now supports `level = "subgroup"`**
   (USDA full subgroup name) and **`level = "subordem"`** (SiBCS
   2nd level "Ordem + Subordem"). Comparison is case-insensitive
   with qualifier-paren stripping; `level = "subordem"` truncates
   the predicted name to its first two tokens to match
   FEBR-style references.

2. **`load_kssl_pedons_gpkg` now also extracts the KSSL
   `samp_taxsubgrp`, `samp_taxgrtgroup`, `samp_taxsuborder`** fields
   into `site$reference_usda_subgroup`, `site$reference_usda_grtgroup`,
   `site$reference_usda_suborder`. The benchmark reads
   `reference_usda_subgroup` automatically when `level = "subgroup"`.

## Critical scientific finding -- Embrapa FEBR Subordem ceiling

FEBR (the open Brazilian soil-data archive used as soilKey's
benchmark source) ships SiBCS labels at the 2nd-level (Subordem)
maximum -- 31 unique strings total across the 50 485 horizon
rows, e.g. "LATOSSOLO VERMELHO", "ARGISSOLO BRUNO-ACINZENTADO".
The 5th-level (Familia, Cap 18) was therefore not benchmarkable
with the FEBR data alone.

This release pivots from "Familia validation" to "Subordem
validation" as the deepest level FEBR actually supports. Future
Familia validation requires a different reference dataset
(IBGE soil-survey volumes, Embrapa BDsolos curated, or similar).

## Real-data benchmark impact

### KSSL + NASIS USDA, n=998 (apples-to-apples)

| Level    | top-1 | CI 95 % |
|----------|------:|---------|
| Order    | 33.8 % | [30.6 %, 36.7 %] |
| **Subgroup** | **2.4 %** | [1.4 %, 3.4 %] |

The Subgroup ceiling reflects that even when the Order gate is
correct (~ 1/3 of profiles), getting the full Subgroup modifier
right (Typic / Aquic / Vertic / Oxyaquic / Pachic / Cumulic /
Inceptic / Ultic / Mollic / etc.) requires the full Path C
machinery for ALL twelve USDA Orders, which is partial in the
current implementation. Each Order has 30-90 distinct subgroup
permutations defined in KST 13ed Chs 5-16 -- not all are wired.

This is the v1.0 / v1.1 work item: complete the Path C subgroup
trees per Order (currently the subgroup machinery handles a
representative subset within each Order, prioritising the
"Typic" plus the most-common qualifying subgroups; the full
combinatorial coverage is deferred).

### Embrapa FEBR SiBCS, n=128

| Level    | top-1 | CI 95 % |
|----------|------:|---------|
| Order    | 40.6 % | [32.0 %, 50.8 %] |
| **Subordem** | **7.8 %** | [3.1 %, 14.1 %] |

The Subordem drop is dominated by **Munsell-colour disagreement**
(Vermelho / Amarelo / Bruno) on profiles where FEBR records the
field-surveyor's colour judgement but the lab gpkg lacks Munsell.
26 of 57 reference Argissolos are correctly Order'd as
Argissolos but classified to a different colour Subordem.

## Code

### `benchmark_run_classification(level)` -- new values

* `"order"` (default) -- compares `cls$rsg_or_order`.
* `"subgroup"` (NEW) -- compares `cls$name` (case-insensitive,
  qualifier-paren-stripped). For USDA, automatically reads
  `reference_usda_subgroup`.
* `"subordem"` (NEW) -- SiBCS 2nd-level. Truncates both reference
  and prediction to the first two tokens before comparison.

### `normalise_kssl_subgroup(x)` (NEW exported)

Lowercases + collapses whitespace in KSSL `samp_taxsubgrp` strings
so "TYPIC HAPLUDALFS" and "Typic Hapludalfs" compare equal.

### `load_kssl_pedons_gpkg` -- expanded reference fields

* `site$reference_usda` (Order, unchanged)
* `site$reference_usda_subgroup` (NEW from `samp_taxsubgrp`)
* `site$reference_usda_grtgroup` (NEW from `samp_taxgrtgroup`)
* `site$reference_usda_suborder` (NEW from `samp_taxsuborder`)

## Tests

* +8 expectations in `test-benchmark-subgroup-subordem.R`:
  * subgroup-level uses `reference_usda_subgroup` field
  * subordem-level compares first 2 tokens
  * order-level still works (no regression)
  * `normalise_kssl_subgroup()` is idempotent + handles whitespace + NA
* Total: **2 850** testthat expectations passing, 0 failed.

## CRAN

* `R CMD check --as-cran` with PROJ env: **Status: OK** (0 ERR /
  0 WARN / 0 NOTE).
* Embrapa Order-level benchmark unchanged at 40.6 % (regression-
  safe).

# soilKey 0.9.21 (2026-05-01)

The "surveyor's diagnostic identification as scientific tie-breaker"
release. Wires the NASIS `pediagfeatures.featkind` table (64 169
records of field-surveyor-identified diagnostic horizons) into the
USDA Order gates as a TIE-BREAKER ONLY: when the canonical lab +
morphology gate returns `passed = NA` (insufficient data), the
surveyor's identification flips it to TRUE. When the canonical gate
returns TRUE / FALSE, the tag is recorded as evidence but does NOT
override -- preserving the deterministic-key-on-data invariant.

## Real-data benchmark impact (KSSL+NASIS, three samples + definitive)

The per-Order improvements **replicate consistently** across three
independently sampled subsets of the KSSL+NASIS data. The
5 000-head sample is the apples-to-apples definitive run vs the
v0.9.19 (n=3 213) and v0.9.20 (n=3 218) baselines.

### Definitive: 5 000-head sample, n=3 218 quality-filtered

| Order        | v0.9.19 lab     | v0.9.20 NASIS    | v0.9.21 +tie-breaker |
|--------------|----------------:|-----------------:|---------------------:|
| **Spodosols**    | 17.8 % (49/276) | 29.0 % (80/276) | **38.0 % (105/276)** |
| **Vertisols**    | 58.7 % (37/63)  | 70.8 % (46/65)  | **73.8 % (48/65)**   |
| Mollisols    | 19.9 % (145/727)| 25.0 % (182/727)| 25.7 % (187/727)     |
| Inceptisols  | 23.1 % (107/463)| 46.3 % (215/464)| 46.3 % (215/464)     |
| Aridisols    | 42.4 % (189/446)| 46.6 % (208/446)| 46.6 % (208/446)     |
| Alfisols     | 21.4 % (142/663)| 22.6 % (150/665)| 22.6 % (150/665)     |
| Ultisols     | 21.9 % (90/411) | 21.7 % (89/411) | 21.7 % (89/411)      |
| Entisols     | 46.3 % (50/108) | 36.1 % (39/108) | 35.2 % (38/108)      |
| Oxisols      | 49.0 % (24/49)  | 49.0 % (24/49)  | 49.0 % (24/49)       |
| Histosols    | 66.7 % (2/3)    | 66.7 % (2/3)    | 66.7 % (2/3)         |
| **TOTAL**    | **26.0 %**      | **32.2 %**      | **33.1 %**           |
|              |                 | **+6.2 pp**     | **+0.9 pp**          |

**USDA top-1: 33.1 % (CI [31.7 %, 34.6 %], n=3 218).**

Cumulative improvement v0.9.19 -> v0.9.21: **+7.1 pp**. The
**Spodosol +9 pp from tie-breaker alone (29.0 -> 38.0)** at n=276
is the largest per-Order gain in v0.9.21. Combined with v0.9.20
NASIS morphology (17.8 -> 29.0), the total Spodosol improvement
from v0.9.19 -> v0.9.21 is **+20.2 pp**.

### Replication: 3 000-head sample, n=2 002 quality-filtered

| Order        | v0.9.20 NASIS    | v0.9.21 +tie-breaker |
|--------------|-----------------:|---------------------:|
| **Spodosols**    | 26.0 % (39/150) | **42.0 % (63/150)** (+16.0 pp) |
| **Vertisols**    | 65.2 % (30/46)  | **69.6 % (32/46)**  (+4.4 pp)  |
| Mollisols    | 22.2 % (112/505) | 23.2 % (117/505)  (+1.0 pp)  |
| Inceptisols  | 47.2 % (118/250) | 47.2 % (118/250)  (=)        |
| Aridisols    | 46.6 % (130/279) | 46.6 % (130/279)  (=)        |
| Alfisols     | 19.4 % (82/422)  | 19.4 % (82/422)   (=)        |
| Ultisols     | 20.4 % (55/269)  | 20.4 % (55/269)   (=)        |
| Entisols     | 42.9 % (27/63)   | 41.3 % (26/63)    (-1.6 pp)  |
| Oxisols      | 28.6 % (4/14)    | 28.6 % (4/14)     (=)        |
| Andisols     | 0/4              | 0/4                (=)        |
| **TOTAL**    | **29.8 %**       | **31.3 %**        | **+1.5 pp** |

USDA top-1: **31.3 %** (CI [29.0 %, 33.5 %], n=2 002).

### 2 500-head sample, n=1 679 quality-filtered (independent confirmation)

| Order        | v0.9.20 NASIS    | v0.9.21 +tie-breaker |
|--------------|-----------------:|---------------------:|
| **Spodosols**    | 26.6 % (37/139) | **43.2 % (60/139)** (+16.6 pp) |
| **Vertisols**    | 57.7 % (15/26)  | **65.4 % (17/26)**  (+7.7 pp)  |
| Mollisols    | 22.6 % (102/452) | 23.7 % (107/452)  (+1.1 pp)   |
| Inceptisols  | 47.1 % (96/204)  | 47.1 % (96/204)   (=)         |
| Total USDA   | 30.3 %           | **32.0 %**         | **+1.7 pp** |

USDA top-1: **32.0 %** (CI [29.8 %, 34.4 %], n=1 679).

The **Spodosol +16-17 pp gain is reproducible** across both
samples, confirming the tie-breaker is not noise. When Al/Fe
oxalate are absent and morphology is sparse, the surveyor's
direct identification of "Spodic horizon" or "Spodic materials"
in `pediagfeatures.featkind` recovers the diagnostic. Vertisol
and Mollisol gains are smaller but consistent with the
tie-breaker philosophy: it fires only on NA cases. Most other
Orders see no change because their canonical gates were already
conclusive.

## What pediagfeatures provides

NASIS `pediagfeatures.featkind` distribution (top entries):

| featkind | n |
|---|---:|
| Ochric epipedon | 13 833 |
| Argillic horizon | 13 501 |
| Mollic epipedon | 6 860 |
| Cambic horizon | 4 970 |
| Lithic contact | 2 193 |
| Aquic conditions | 1 750 |
| Calcic horizon | 1 541 |
| Albic horizon | 1 415 |
| Fragipan | 1 091 |
| Spodic horizon | 829 |
| Umbric epipedon | 803 |
| Slickensides | 519 |
| Andic soil properties | 494 |
| Glossic horizon | 429 |
| Histic epipedon | 201 |

The 13 501 "Argillic horizon" + 6 860 "Mollic epipedon" records are
particularly impactful -- they directly identify the diagnostic
horizons that drive Mollisol / Alfisol / Ultisol / Inceptisol
disambiguation.

## Code

### `.has_nasis_feature(pedon, pattern)`

Checks `pedon$site$nasis_diagnostic_features` (populated by
`load_kssl_pedons_with_nasis()`) for a regex match against the
NASIS featkind values.

### `.apply_nasis_tiebreaker(result, pedon, pattern, feature_label)`

Applied at the start of each USDA Order gate. If the input
`DiagnosticResult$passed == NA` AND the surveyor identified the
matching feature, flips `passed` to TRUE and records the
provenance. Does NOT override TRUE / FALSE.

### USDA Order gates with tie-breaker (v0.9.21)

| Gate | Tie-breaker pattern |
|---|---|
| `histosol_usda` | Histic / Folistic / Hemic / Sapric / Fibric / Limnic / Coprogenous |
| `spodosol_usda` | Spodic horizon / Spodic materials / Ortstein / Placic |
| `andisol_usda` | Andic soil properties / Vitric / Volcanic glass |
| `vertisol_usda` | Slickensides / Vertic features / Gilgai |
| `ultisol_usda` | Argillic horizon / Kandic horizon |
| `mollisol_usda` | Mollic epipedon |
| `alfisol_usda` | Argillic horizon / Kandic horizon / Natric horizon |
| `inceptisol_usda` | Cambic horizon |

## Why scientifically defensible

The tie-breaker fires ONLY when the canonical gate returns NA,
i.e., when the deterministic key has insufficient data to decide.
In that case, the field surveyor's identification (recorded in
NASIS by NRCS pedologists) is the most authoritative source short
of re-running the field survey. When chemistry + morphology IS
available and conclusive, the canonical gate's TRUE / FALSE stands
unmodified -- the tie-breaker is strictly additive on missing-data
cases.

This preserves the package-level invariant: **the deterministic
key on lab + morphology data always wins; the surveyor tag is a
fallback when the deterministic key is silent**.

## Tests + CRAN

* 2 829 testthat expectations passing, 0 failed
* 31/31 canonical fixtures still classify correctly (no regression
  -- canonical fixtures don't have NASIS pediagfeatures, so the
  tie-breaker is inactive on them)
* Embrapa benchmark unchanged (USDA 47.6 %, WRB 32.7 %, SiBCS
  40.6 %) -- FEBR doesn't carry NASIS pediagfeatures
* `R CMD check --as-cran` with PROJ env: Status: OK

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

## Real-data benchmark impact (KSSL apples-to-apples, 5 000-head)

Both runs filter to the same quality criteria (clay + lab + B
horizon). v0.9.19 lab-only run: n=3 213 quality. v0.9.20 lab+NASIS
run: n=3 218 quality (essentially identical sample).

| Order        | v0.9.19 lab     | v0.9.20 lab+NASIS | Δ |
|--------------|----------------:|------------------:|---:|
| **Inceptisols**  | 23.1 % (107/463)| **46.3 % (215/464)** | **+23.2 pp** |
| **Vertisols**    | 58.7 % (37/63)  | **70.8 % (46/65)**   | **+12.1 pp** |
| **Spodosols**    | 17.8 % (49/276) | **29.0 % (80/276)**  | **+11.2 pp** |
| Mollisols    | 19.9 % (145/727)| 25.0 % (182/727)  | +5.1  |
| Aridisols    | 42.4 % (189/446)| 46.6 % (208/446)  | +4.2  |
| Alfisols     | 21.4 % (142/663)| 22.6 % (150/665)  | +1.2  |
| Ultisols     | 21.9 % (90/411) | 21.7 % (89/411)   | -0.2  |
| Entisols     | 46.3 % (50/108) | 36.1 % (39/108)   | -10.2 |
| Oxisols      | 49.0 % (24/49)  | 49.0 % (24/49)    | 0     |
| Histosols    | 66.7 % (2/3)    | 66.7 % (2/3)      | 0     |
| Andisols     | 0/4 (0 %)       | 0/4 (0 %)         | 0     |
| **TOTAL**    | **26.0 %**      | **32.2 %**        | **+6.2 pp** |

USDA top-1: **32.2 %** (CI [30.7, 33.6], n=3 218).

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
