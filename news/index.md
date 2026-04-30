# Changelog

## soilKey 0.9.10 (2026-04-30)

CRAN-readiness pass: `R CMD check` now returns 0 ERROR / 0 WARNING / 1
NOTE (the lone NOTE is environmental – a missing `proj.db` on the local
system, not present on CRAN’s own check farm). Plus a real OSSL fetch
helper and a hardened WoSIS driver, closing the v0.9.6 audit gap and the
paper-grade WoSIS run pre-requisites.

### New features

- **`download_ossl_subset(region, properties, wavelengths, ...)`** –
  region-filtered fetch of the Open Soil Spectral Library that returns
  the canonical `list(Xr, Yr, metadata)` artefact consumed by
  [`predict_ossl_mbl()`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_ossl_mbl.md)
  /
  [`predict_ossl_plsr_local()`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_ossl_plsr_local.md).
  Caches under `tools::R_user_dir("soilKey", "cache")` keyed by region;
  honours `getOption("soilKey.ossl_endpoint")` for testing or private
  mirrors; interpolates Xr to the requested wavelength grid; fails
  loudly when the network is unavailable (does NOT silently fall back to
  the synthetic predictor). Companion:
  [`clear_ossl_cache()`](https://hugomachadorodrigues.github.io/soilKey/reference/clear_ossl_cache.md).
- **WoSIS driver hardening** (`inst/benchmarks/run_wosis_benchmark.R`):
  - aligns request schema with WoSIS REST v3 (offset+limit, `bbox=`,
    `country=`); previous v0.9.9 used the older `page+page_size` shape
    that v3 deprecated.
  - adds
    `subset = c("global", "south_america", "north_america", "europe", "africa", "asia", "oceania", "brazil")`
    so the paper can run a regional benchmark in one call; bbox per
    region is overrideable via
    `options(soilKey.wosis_bbox_<region> = ...)`.
  - wraps every HTTP call in `tryCatch` with a clear error when offline
    or non-200; sends a `User-Agent: soilKey (...)` header.

### Documentation

- All vignettes renamed to start with a letter
  (`v01_getting_started.Rmd`, …); pkgdown / README / cross-vignette
  references updated.
- Vignette 02 gains a “Render a self-contained pedologist-facing report”
  section showing the
  [`report()`](https://hugomachadorodrigues.github.io/soilKey/reference/report.md)
  API.
- Vignette 06 documents the offline `run_canonical_benchmark()` driver
  and the most-recent canonical numbers (WRB 31/31, SiBCS 20/20, USDA
  31/31).
- New URL fields in DESCRIPTION (homepage + bug tracker).

### CRAN-readiness fixes

- All roxygen titles / descriptions: literal `%` is now escaped as `\%`
  (was a mix of bare `%` and `\\%`, both invalid in Rd).
- Same for `\eqn{}` (was `\\eqn{}` which Rd parsed as escaped
  backslash + `eqn{...}` block, generating “Lost braces” NOTEs).
- Several roxygen blocks were missing `@param` entries for non-`pedon`
  arguments; ~530 placeholder `@param` lines added across the catalogue.
  Manually-curated descriptions remain where they existed.
- `R/soilKey-package.R` now declares the `stats` (`predict`, `rnorm`,
  `runif`, `setNames`, `weighted.mean`), `utils` (`tail`), and `R6`
  (`R6Class`) imports it actually uses.
- `R/diagnostics-horizons-wrb-v033.R::plaggic` calls
  [`test_bulk_density_below()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_bulk_density_below.md)
  with the spelled-out argument name `max_g_cm3` instead of the
  partial-match `max`.
- `tests/testthat/test-spatial-soilgrids.R` now skips when PROJ’s
  `proj.db` is unavailable on the local system (a cosmetic fix – CRAN’s
  check farm has it).
- `tests/testthat/test-vlm-providers.R::skip_if(requireNamespace("ellmer"))`
  guard re-annotated for clarity (logic was correct; misread once).
- `inst/CITATION` falls back to the literal string `"dev"` for the
  package version when soilKey isn’t installed (so pkgdown / roxygen2
  builds during early development don’t fail).
- `_pkgdown.yml` references repaired to point at the actual documented
  topic names;
  [`pkgdown::check_pkgdown()`](https://pkgdown.r-lib.org/reference/check_pkgdown.html)
  now passes with no problems.

------------------------------------------------------------------------

## soilKey 0.9.9 (2026-04-30)

A pre-CRAN release that closes seven of the nine “promise gaps” called
out in the v0.9.8 review: the package now ships its own benchmark
report, CI, changelog, browsable docs, end-user reporting, complete WRB
Ch 6 supplementary coverage, and an honest OSSL audit.

### New features

- **[`report()`](https://hugomachadorodrigues.github.io/soilKey/reference/report.md)
  /
  [`report_html()`](https://hugomachadorodrigues.github.io/soilKey/reference/report_html.md)
  /
  [`report_pdf()`](https://hugomachadorodrigues.github.io/soilKey/reference/report_pdf.md)**
  – pedologist-facing report renderer (R/report-html.R, R/report-pdf.R).
  HTML output is fully self-contained (single file, inline CSS, no
  external network requests); PDF output goes through
  [`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html).
  Accepts a single `ClassificationResult`, a list of results, or a
  `PedonRecord` (in which case all three keys are run automatically).
  The R6 method `ClassificationResult$report(file)` now delegates to
  this generic (was a stub raising “not yet implemented”).
- **`run_canonical_benchmark()`** – offline, network-free validation
  over the 31 canonical fixtures under `inst/extdata/`. Each fixture has
  a known target RSG / SiBCS order / USDA order; the function classifies
  all three systems and writes a versioned report under
  `inst/benchmarks/reports/canonical_<DATE>.md`. Companion to
  `run_wosis_benchmark()`, which still pulls the WoSIS REST API for the
  paper-grade run.
- **WRB 2022 Ch 6 supplementary qualifiers – 32 / 32 RSGs.** v0.9.5 adds
  canonical baseline supplementary lists for the 25 RSGs that v0.9.3.B
  left empty (HS, AT, TC, CR, LP, SN, VR, SC, GL, AN, PZ, PT, PL, ST,
  CH, KS, PH, UM, DU, GY, CL, RT, AR, RG, FL). 489 total supplementary
  entries across all 32 RSGs, all backed by the 105 qualifier functions
  implemented in v0.9.1 – v0.9.3.B (zero broken references).
  Page-precise canonical lists per Ch 6 are deferred to v0.9.6+; the
  v0.9.5 baselines are conservative and pedologically defensible.
- **[`ossl_library_template()`](https://hugomachadorodrigues.github.io/soilKey/reference/ossl_library_template.md)**
  – canonical schema constructor for the `ossl_library = list(Xr, Yr)`
  argument consumed by
  [`predict_ossl_mbl()`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_ossl_mbl.md)
  and
  [`predict_ossl_plsr_local()`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_ossl_plsr_local.md).
  Documents the shape of the artefact users need to construct from a
  real OSSL extract. The synthetic-fallback path now emits a
  `cli_alert_warning` so users always know when the predictor is not
  real.
- **`run_vlm_live_demo()`** – a manual driver under
  `inst/benchmarks/run_vlm_live_demo.R` that runs end-to-end real-VLM
  extraction (PDF + photo) against `anthropic` / `openai` / `google` /
  `ollama` and writes a release-time report with provenance summary,
  latency, and the resulting cross-system classification.
- **GitHub Actions CI** – `.github/workflows/R-CMD-check.yaml` (5
  platform x R-version matrix), `test-coverage.yaml` (codecov), and
  `pkgdown.yaml` (auto-deploys to gh-pages on push to main). Replaces
  the previous (false) “R-CMD-check passing” badge in the README with a
  live one driven from the workflow run.
- **pkgdown site** – `_pkgdown.yml` organises the ~700 exported
  functions into 17 navigable sections (core / classify / WRB Ch 3.1-3.3
  / qualifiers / SiBCS Caps 1-2 / SiBCS keys / Família / USDA Path C /
  Modules 2-4 / reporting / fixtures / helpers).
- **`NEWS.md`** – this file. Curated from `git log` per CRAN
  expectations.
- **`inst/CITATION` + `.zenodo.json`** – canonical BibTeX exposed via
  `citation("soilKey")`, plus Zenodo metadata so the first GitHub
  release auto-mints a software DOI.

### Documentation

- `ARCHITECTURE.md` § 2: license reconciled to MIT (was GPL-3, an
  artefact of an early rascunho).
- README: live R-CMD-check + Codecov badges; reworked Ch 6 row in the
  WRB coverage table to reflect 32/32 RSG supplementary coverage; full
  BibTeX block now references the Zenodo concept-DOI.
- `inst/benchmarks/reports/audit_ossl_2026-04-30.md` – honest audit of
  what is real vs. synthetic in Module 4 (predict_ossl\_\*). Bundled
  OSSL training data and fetch helper remain on the v0.9.6+ roadmap.

### Bug fixes / clarity

- `tests/testthat/test-vlm-providers.R:13` – the
  `skip_if(requireNamespace("ellmer"))` guard is now annotated so a
  future reader doesn’t misread it as inverted (it isn’t –
  `skip_if(TRUE)` skips, and we want to skip the missing-ellmer
  assertion when ellmer IS installed).
- `tests/testthat/test-qualifiers-wrb-v093a-specifiers-suppl.R:224` –
  updated to reflect that all 32 RSGs now have supplementary slots; the
  “no supplementary slot” branch is now exercised with an unknown RSG
  code (`"ZZ"`) instead of GL.

------------------------------------------------------------------------

## soilKey 0.9.8 (2026-04-30)

This release closes the **third** classification system end-to-end. With
v0.7 (SiBCS 5ª ed., 2026-04-28) and v0.9.4 (WRB 2022 Ch 6, 2026-04-29)
already shipped, soilKey 0.9.8 makes USDA Soil Taxonomy the third
deterministic key driven from versioned YAML rules.

### Major features

- **USDA Soil Taxonomy 13th edition (Soil Survey Staff, 2022) – Path C
  complete.** The full Order -\> Suborder -\> Great Group -\> Subgroup
  walk for every Order is wired and tested: Gelisols (`v0.8.3`),
  Histosols (`v0.8.4`), Spodosols (`v0.8.5`), Andisols (`v0.8.6`),
  Oxisols (`v0.8.7`), Vertisols (`v0.8.8`), Aridisols (`v0.8.9`),
  Ultisols (`v0.8.10`), Mollisols (`v0.8.11`), Alfisols (`v0.8.12`),
  Inceptisols (`v0.8.13`), Entisols (`v0.8.14`). 68 Suborders / 339
  Great Groups / 1 288 Subgroups in `inst/rules/usda/`. New helper:
  `classify_usda(pedon)$name` returns the canonical Subgroup label
  (e.g. `"Rhodic Hapludox"`).
- **6 USDA diagnostic epipedons** (`v0.8.1`): histic, folistic, melanic,
  mollic, umbric, ochric. Anthropic + plaggen are deferred.
- **5 USDA diagnostic characteristics** (`v0.8.2`): aquic conditions,
  anhydrous conditions, cryoturbation, glacic layer, permafrost.
- **SiBCS 5ª ed. Cap 18 (Família, 5º nível) implementado integralmente**
  (`v0.7.14.A` -\> `v0.7.14.D`): 15 dimensões adjectivais ortogonais
  (grupamento textural, subgrupamento textural, distribuição de
  cascalhos, esquelética, tipo de A, prefixos epi/meso/endo, saturação
  V, álico, mineralogia da areia, mineralogia da argila, atividade da
  argila, óxidos de ferro, ândico, material subjacente, espessura \> 100
  cm, lenhosidade). Inclui motor de adjetivos com supressão de rótulos
  sem evidência suficiente. Séries (6º nível) explicitamente fora de
  escopo (provisório no SiBCS 5ª ed.).

### Documentation

- README + DESCRIPTION refletem agora as três promessas core (WRB /
  SiBCS / USDA) com badges canônicas de cobertura por sistema.

------------------------------------------------------------------------

## soilKey 0.9.4 (2026-04-29)

End of the WRB 2022 build phase. Modules 1 (key), 2 (VLM), 3 (spatial
prior) and 4 (spectroscopy) all on disk; vignette pipeline complete.

### Major features

- **Five paper-grade vignettes** (`v0.9.4`):
  - `02-classify-wrb-end-to-end.Rmd` – canonical Latossolo classified
    with full Ch 6 name.
  - `03-cross-system-correlation.Rmd` – the same profile resolved in WRB
    / SiBCS / USDA, with a side-by-side correspondence table.
  - `04-vlm-extraction.Rmd` – Module 2 walkthrough using
    `MockVLMProvider` (offline, schema-validated).
  - `05-spatial-spectra-pipeline.Rmd` – Module 3 + Module 4 over a
    synthetic-but-realistic profile (offline-by-default).
  - `06-wosis-benchmark.Rmd` – protocol for validating the key against
    WoSIS, plus a 31-fixture mini-run that runs anywhere.
- **WoSIS benchmark driver** (`inst/benchmarks/run_wosis_benchmark.R`):
  reads the WoSIS REST API, builds `PedonRecord`s, runs the key, writes
  a versioned report under `inst/benchmarks/reports/`.

### Documentation

- README rewrite with hex sticker, status badges, architecture mermaid
  diagram, full coverage tables, BibTeX citation block.
- MIT licence formalised (replacing the GPL-3 placeholder considered in
  the early architecture rascunho).

------------------------------------------------------------------------

## soilKey 0.9.3 (2026-04-29)

Closes the WRB 2022 Chapter 6 name machinery – a Latossolo now
classifies as
`"Geric Ferric Rhodic Chromic Ferralsol (Clayic, Humic, Dystric, Ochric, Rubic)"`.

### Major features

- **`v0.9.3.A`** – Specifier engine generalised to handle the full Ch 4
  specifier set (`Ano-`, `Epi-`, `Endo-`, `Bathy-`, `Panto-`, `Kato-`,
  `Amphi-`, `Poly-`, `Supra-`, `Thapto-`) via two `kind`s in the
  resolver: `depth` (simple band) and `filter` (custom predicate).
  Engine extended to also process the `supplementary:` slot of each
  RSG’s YAML.
- **`v0.9.3.B`** – Five new supplementary qualifier functions
  (`qual_aric`, `qual_cumulic`, `qual_profondic`, `qual_rubic`,
  `qual_lamellic`) plus ~30 reused from the principal-qualifier set.
  Canonical WRB Ch 6 names with parenthesised supplementary block now
  render correctly for FR / AC / LX / AL / LV / CM / NT.

------------------------------------------------------------------------

## soilKey 0.9.2 (2026-04-28)

Sub-qualifier infrastructure + diagnostic tightening.

### Major features

- **`v0.9.2.A`** – 11 Hyper- / Hypo- / Proto- sub-qualifiers (Hyper/Hypo
  for salinity, sodicity, calcic, gypsic; Proto for calcic, gypsic,
  vertic). Family suppression in the engine: when several members of the
  same family pass (e.g. Calcic + Hypocalcic + Protocalcic), only the
  most specific surfaces in the resolved name per WRB Ch 6 rules.
- **`v0.9.2.B`** – Specifier infrastructure (Ano- / Epi- / Endo- /
  Bathy- / Panto-) via prefix dispatch in the resolver. No need for a
  function per (specifier × base) pair.

### Bug fixes

- **`v0.9.2.C`** – Tightened three permissive diagnostics:
  - `cambic` now requires `top_cm >= 5` and a developed structure (grade
    in `{weak, moderate, strong}` and type not in
    `{massive, single grain}`); A/E and C-massive horizons no longer
    pass.
  - `plaggic` now gates on anthropogenic evidence directly (P \>= 50
    mg/kg OR artefacts \> 0 OR designation Apl/Aplg/Apk).
  - `sombric` now requires a humus-illuviation pattern (candidate layer
    must have OC \>= layer-above OC + 0.1 %).

------------------------------------------------------------------------

## soilKey 0.9.1 (2026-04-28)

WRB 2022 Chapter 4 canonical principal-qualifier coverage for all 32 /
32 Reference Soil Groups. Shipped as five blocks (A–E) for
review-friendliness:

- **Bloco A** – HS, AT, TC, CR, LP (organic / anthropogenic /
  technogenic / cryic / shallow). +42 `qual_*` functions.
- **Bloco B** – SN, VR, SC, GL, AN (saline / clay-rich / wet /
  volcanic). +14 functions, including the Aluandic/Silandic split for
  andic soils via molar ratio.
- **Bloco C** – PZ, PT, PL, ST, NT, FR (Brazilian / tropical block:
  Latossolos, Argissolos, Espodossolos as Ferralsols / Acrisols /
  Lixisols / Podzols). +14 functions including the Geric / Vetic / Posic
  family for very-low-CTC tropical soils.
- **Bloco D + E** – 16 remaining RSGs (CH, KS, PH, UM, DU, GY, CL, RT;
  AC, LX, AL, LV, CM, AR, RG, FL). +4 functions: Cutanic (clay films),
  Glossic (mollic with albic glossae), Brunic (cambic-only B in
  Arenosol), Protic (no B horizon).

After v0.9.1, every Latossolo / Argissolo / Espodossolo / Cambissolo /
Nitossolo / Luvissolo brasileiro resolves to its full canonical WRB
name.

------------------------------------------------------------------------

## soilKey 0.9.0 (2026-04-28)

- WRB 2022 Chapter 5 qualifiers seed: ~50 core qualifier functions wired
  across the most-used RSGs.

------------------------------------------------------------------------

## soilKey 0.8.0 (2026-04-28)

- **Module 5 scaffold** – `inst/rules/usda/key.yaml` listing all 12
  Orders in canonical key order (GE, HI, SP, AD, OX, VE, AS, UT, MO, AF,
  IN, EN). Oxisols path wired via
  [`oxic_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/oxic_usda.md)
  (delegating to WRB `ferralic`). Full Path C fills out across the
  v0.8.x series.

------------------------------------------------------------------------

## soilKey 0.7.x (2026-04-28 – 2026-04-29)

End-to-end SiBCS 5ª ed. (Embrapa, 2018) implementation.

### Major features

- **`v0.7`** – 17 atributos diagnósticos + 24 horizontes diagnósticos
  - 13 ordens RSG-level wired in the canonical key order
    (O-V-E-S-G-M-C-F-T-N-P).
- **`v0.7.1`** – 44 Subordens (2º nível) wired.
- **`v0.7.2`** – Engine refactor:
  `run_taxonomic_key(pedon, rules, level_key)` replaces hard-coded WRB
  iteration, so the same engine drives WRB / SiBCS / USDA. `clay_films`
  split + 7 pendentes diagnostics (caráter ácrico, espódico
  subsuperficial, ebânico, retrátil; Ki/Kr; cerosidade quantitativa;
  grau de decomposição von Post).
- **`v0.7.3` -\> `v0.7.13`** – Grandes Grupos (3º nível) + Subgrupos (4º
  nível) implemented Ordem-by-Ordem in the canonical key order:
  Organossolos (Cap 14), Argissolos (Cap 5), Cambissolos (Cap 6),
  Chernossolos (Cap 7), Espodossolos (Cap 8), Gleissolos (Cap 9),
  Latossolos (Cap 10), Luvissolos (Cap 11), Neossolos (Cap 12),
  Nitossolos (Cap 13), Planossolos (Cap 15), Plintossolos (Cap 16),
  Vertissolos (Cap 17). 192 Grandes Grupos and 938 Subgrupos.
- **`v0.7.14`** – Família (5º nível, Cap 18). See v0.9.8 for details.

------------------------------------------------------------------------

## soilKey 0.6.0 (2026-04-27)

- **Module 2 – VLM extraction via `ellmer`.**
  [`extract_horizons_from_pdf()`](https://hugomachadorodrigues.github.io/soilKey/reference/extract_horizons_from_pdf.md),
  [`extract_munsell_from_photo()`](https://hugomachadorodrigues.github.io/soilKey/reference/extract_munsell_from_photo.md),
  [`extract_site_from_fieldsheet()`](https://hugomachadorodrigues.github.io/soilKey/reference/extract_site_from_fieldsheet.md).
  Schema-validation via `jsonvalidate` (draft-07). `MockVLMProvider`
  exported for offline tests. Bug-fix: NSE handling in
  `PedonRecord$add_measurement`.

------------------------------------------------------------------------

## soilKey 0.5.0 (2026-04-27)

- **Module 3 – SoilGrids / Embrapa spatial prior.**
  `spatial_soilgrids_prior()` (WCS), `spatial_embrapa_prior()`,
  [`prior_consistency_check()`](https://hugomachadorodrigues.github.io/soilKey/reference/prior_consistency_check.md).
  Wired into
  [`classify_wrb2022()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_wrb2022.md)
  via `prior` and `prior_threshold`. **The deterministic key is never
  overridden by the prior** – the prior only flags inconsistencies.

------------------------------------------------------------------------

## soilKey 0.4.0 (2026-04-27)

- **Module 4 – OSSL spectroscopy bridge.**
  [`predict_ossl_mbl()`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_ossl_mbl.md),
  [`predict_ossl_plsr_local()`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_ossl_plsr_local.md),
  [`predict_ossl_pretrained()`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_ossl_pretrained.md),
  [`preprocess_spectra()`](https://hugomachadorodrigues.github.io/soilKey/reference/preprocess_spectra.md)
  (SNV / SG1),
  [`pi_to_confidence()`](https://hugomachadorodrigues.github.io/soilKey/reference/pi_to_confidence.md),
  [`fill_from_spectra()`](https://hugomachadorodrigues.github.io/soilKey/reference/fill_from_spectra.md).
  Provenance tag `predicted_spectra` automatically downgrades the
  `evidence_grade` from A to B.

------------------------------------------------------------------------

## soilKey 0.3.x (2026-04-26 – 2026-04-27)

The WRB-key build phase: 32/32 RSGs wired, full Ch 3 coverage, strict
Tier-2 gates.

### Major features

- **`v0.3a`** – 8 new WRB diagnostics; SiBCS YAML quoting fix.
- **`v0.3b`** – Diagnostics for natric, nitic, planic, stagnic, retic,
  cryic, anthric.
- **`v0.3c`** – Full WRB key wired (32/32 RSGs) with end-to-end test
  over 31 canonical fixtures.
- **`v0.3.1`** – Aligned argic, ferralic, duric, vertic, salic with WRB
  2022 text (correções Tier-1 contra texto canônico).
- **`v0.3.2`** – Reordered RSGs in `key.yaml` to canonical WRB 2022
  order (PL/ST between PT and NT; FL before AR).
- **`v0.3.3`** – Complete WRB 2022 Ch 3.1 / 3.2 / 3.3 diagnostic
  coverage. +18 horizons, +12 properties, +16 materials. Schema expanded
  by 24 columns.
- **`v0.3.4`** – Tier-2 RSG-level gate strengthening per WRB 2022 Ch 4.
  7 strict gates (vertisol, andosol, gleysol, planosol, ferralsol,
  chernozem_strict, kastanozem_strict) replace v0.2 single-horizon
  shortcuts.
- **`v0.3.5`** – Closes WRB 2022 Ch 3.1 – 32 / 32 horizons (tsitelic,
  panpaic, limonic, protovertic added).

------------------------------------------------------------------------

## soilKey 0.2.x (2026-04-25 – 2026-04-26)

Initial diagnostic build-out + Module 5 / 6 scaffolds.

### Major features

- **`v0.2a`** – gypsic, salic, calcic horizons + schema extensions.
- **`v0.2b`** – cambic, plinthic, spodic, gleyic, vertic diagnostics.
- **`v0.2c`** – argic-derived RSG diagnostics (AC, LX, AL, LV).
- **`v0.2d`** – mollic-derived RSG diagnostics (CH, KS, PH).
- **`v0.2e`** – 15 RSGs wired into the WRB key with end-to-end tests.
- **`modules-5-6`** – USDA Soil Taxonomy + SiBCS 5ª ed. scaffolds.

------------------------------------------------------------------------

## soilKey 0.1.0 (2026-04-25)

Initial commit. Esqueleto, classes core (`PedonRecord`,
`DiagnosticResult`, `ClassificationResult`), 3 WRB diagnostics (`argic`,
`ferralic`, `mollic`), Ferralsols path end-to-end + canonical fixture +
tests + getting-started vignette.
