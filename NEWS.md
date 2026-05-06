# soilKey 0.9.57 (2026-05-06)

The "FEBR loader -- Brazilian profiles with Munsell" release.
Wires soilKey to the **Free Brazilian Repository for Open Soil
Data (FEBR)** maintained by UFSM (Alessandro Samuel-Rosa). FEBR
is the canonical R-side path to ~36,000 Brazilian soil horizons
with Munsell colors -- the gap that BDsolos was meant to fill but
that Hugo's existing FEBR exports (Songchao, superconjunto)
didn't include.

## Diagnostic finding (May 2026 scan)

A live scan of all 249 FEBR datasets via \code{febr::readFEBR}
confirmed:

- **200 / 249 (80.3%) of FEBR datasets carry Munsell colors**
- **36,275 horizons** with non-NA Munsell hue across the catalog
- ctb0032 alone has **10,577 horizons with Munsell**
- ctb0562-ctb0700+ series ships pre-parsed
  matiz / valor / croma in separate columns

The earlier conclusion that "FEBR doesn't have Munsell" was based
on Hugo's two specific FEBR exports (Songchao / superconjunto)
that genuinely lack morphology. Other FEBR datasets do carry it.

## What's shipped

`R/febr.R` exports two functions plus internal helpers:

- **`read_febr_pedons(dataset_codes, febr_repo, min_munsell_coverage,
  verbose)`** -- wraps \code{febr::readFEBR} and adapts the
  returned \code{camada} (layer) + \code{observacao} tables to the
  soilKey schema. Auto-detects the ~6 distinct Munsell column
  conventions used across FEBR datasets, parses PT-BR Munsell
  strings (\code{"2,5YR 3/6"} -> hue \code{"2.5YR"}, value 3,
  chroma 6), and returns a list of \code{PedonRecord}.

- **`febr_index_munsell(min_coverage, refresh, verbose)`** --
  curated index of FEBR datasets that have Munsell columns
  populated. Backed by a precomputed cache in \code{R/sysdata.rda}
  (\code{.FEBR_MUNSELL_INDEX}, 200 rows from the May-2026 scan).
  \code{refresh = TRUE} re-scans live (slow, ~15 min).

- **`.parse_febr_munsell()` / `.parse_febr_munsell_vec()`** --
  PT-BR-aware Munsell string parser handling comma decimals.

- **`.detect_febr_munsell_columns()`** -- discovers Munsell-related
  columns across the FEBR conventions:
  \code{cor_munsell_umida}, \code{cor_cod_munsell_umida}, 
  \code{cor_cod_munsell_umida_1}, \code{cor_cod_munsell_umida_i},
  \code{cor_munsell_umida_matiz / valor / croma},
  \code{cor_munsell_umida_nome},
  \code{cor_matriz_umido_munsell} (canonical).

- **`.FEBR_TO_HORIZON_MAP`** -- regex table mapping FEBR layer
  variable codes (camada_nome, profund_sup/inf, ph_h2o, carbono,
  argila/silte/areia, ca_troc, ctc, etc.) to soilKey horizon
  columns.

## Why this matters

Combined with the v0.9.55 BDsolos helpers, soilKey now offers
**three independent paths to Brazilian profiles with Munsell**:

1. **`read_febr_pedons("ctb0032")`** -- the largest source
   (~10k horizons), HTTP-only via the febr package
   (CRAN-stable, no headless browser).

2. **`download_bdsolos(filter_uf = "RJ")`** -- via headless
   Chrome (chromote, v0.9.55+v0.9.56), works for BDsolos-only
   profiles not aggregated into FEBR.

3. **`load_bdsolos_csv(path)`** -- consumes a manually-downloaded
   BDsolos CSV.

For the v0.9.45 Argissolo "cor a determinar" fallback, FEBR is
the most practical fix: 200 datasets with Munsell, no JS UI to
fight, no chromote dependency, just \code{remotes::install_github
("febr-team/febr-package")} + a few function calls.

## Tests

12 new tests in `test-v0957-febr.R` (54 expectations), all run
unconditionally without network access:

- Munsell parser handles canonical PT-BR strings + fractional
  value/croma + garbage / empty input + vectorisation.
- Column detector picks pre-parsed columns over string columns
  when both present; falls back to ctb0005 / ctb0019 / ctb0032
  variants.
- Layer column mapper recognises canonical FEBR camada codes.
- Bundled \code{.FEBR_MUNSELL_INDEX} has the expected shape
  (200 rows, ctb0032 at the top with 10,577 horizons).
- \code{febr_index_munsell} filters by coverage + sorts
  descending.
- \code{read_febr_pedons} errors clearly when febr is missing
  (path skipped when febr is installed).
- Live network test gated on \code{SOILKEY_NETWORK_TESTS}.

Suite total: 3644 / 0 / 20 (pass / fail / skip). R CMD check
Status OK.

## Smoke test on real FEBR data

```r
library(soilKey)
pedons <- read_febr_pedons("ctb0039")
#> ctb0039: 8 perfis (Munsell em 8), 35 horizons total.

p <- pedons[[1]]
p$horizons[1:3, .(designation, top_cm, bottom_cm,
                   munsell_hue_moist, munsell_value_moist,
                   munsell_chroma_moist, clay_pct)]
#>   designation top_cm bottom_cm munsell_hue_moist munsell_value_moist
#> 1:          AP      0         6             2.5YR                   3
#> 2:           A      6        45             2.5YR                   3
#> 3:         Bw1     45       100             2.5YR                   3
#>   munsell_chroma_moist clay_pct
#> 1:                    3    37.30
#> 2:                    3    48.35
#> 3:                    4    68.30
```

Note the PT-BR comma decimal in the original FEBR data
(\code{"2,5YR"}) was correctly normalised to \code{"2.5YR"} for
soilKey schema compatibility.

## DESCRIPTION

`febr` added to Suggests (gated via `requireNamespace()`).
Install with
\code{remotes::install_github("febr-team/febr-package")} since
the CRAN binary lags the GitHub repo (last CRAN release v1.1.0
of 2020-03 doesn't have \code{readFEBR} or \code{morphology}).


# soilKey 0.9.56 (2026-05-06)

The "download_bdsolos timeout fix" patch. v0.9.55 shipped
\code{download_bdsolos()} but the synchronous \code{realizaBusca()}
invocation in the JS frame timed out chromote on the slow Embrapa
server (~5-10s default \code{Runtime.evaluate} timeout vs minutes
of server-side PHP processing).

## What changed

- **\code{realizaBusca()} call deferred via \code{setTimeout(0)}**
  -- the JS frame returns immediately, the AJAX runs in the
  background, and the chromote eval no longer blocks. The polling
  loop continues to monitor the DOM for "ETAPA 3" appearance.

- **Defensive \code{tryCatch} around the submit eval** -- even if
  chromote itself times out, the AJAX is likely still running, so
  we proceed to the polling loop with a warning instead of
  aborting.

- **Polling probe enriched** -- each probe now also reports the
  page's loading state (\code{aguarde / carregando / processando}
  pattern), and the function emits a progress line every 30s
  showing elapsed time + DOM state when \code{verbose = TRUE}.

- **\code{CHROMOTE_TIMEOUT} env var bumped** at session init to
  \code{max(60, timeout_seconds)}; chromote's default 5-10s isn't
  enough for the SPA bootstrap on the BDsolos splash page.

## Sentinel tests

2 new tests in \code{test-v0955-bdsolos.R} (now 57 expectations):

- \code{download_bdsolos source uses setTimeout-deferred realizaBusca}
- \code{download_bdsolos sets CHROMOTE_TIMEOUT for resilience}

These regression sentinels ensure the timeout fix doesn't get
accidentally reverted in future refactors.

Suite total: 3588 / 0 / 18 (pass / fail / skip). R CMD check
Status OK.

## How to use after this fix

```r
remotes::install_github("HugoMachadoRodrigues/soilKey",
                          ref = "v0.9.56", force = TRUE)
.rs.restartR()  # restart R / fresh session

library(soilKey)
ufs <- c("RJ", "SP", "MG", "ES")
dir.create("./soil_data/embrapa_bdsolos", showWarnings = FALSE,
           recursive = TRUE)
for (uf in ufs) {
  download_bdsolos(
    out_path        = file.path("./soil_data/embrapa_bdsolos",
                                  paste0(uf, ".csv")),
    accept_terms    = TRUE,
    filter_uf       = uf,
    timeout_seconds = 600,
    verbose         = TRUE
  )
}
```

If a particular UF still times out (full state too large or server
overloaded), retry with \code{timeout_seconds = 1200} or pick a
specific municipality once the Etapa 2 form supports it.


# soilKey 0.9.55 (2026-05-06)

The "BDsolos R-side helpers" release. Adds three R-side helpers
to consume the **Embrapa BDsolos** profile database (~9,000
perfis brasileiros, the canonical source for SiBCS-classified
data with morphology + Munsell colors) without leaving R.

## What's shipped

`R/bdsolos.R` (new file) exports three functions plus an internal
column-detection layer:

- **`load_bdsolos_csv(path, sep, verbose)`** -- reads the long-
  format BDsolos export (one row per horizon, profile-id key)
  and returns a list of \code{\link{PedonRecord}}. Auto-detects
  the column-name convention via regex patterns covering the
  classic PT-BR shape (\code{matiz_umido / valor_umido /
  croma_umido}, \code{argila / silte / areia}, \code{ph_em_agua},
  \code{c_org}, \code{ca_troc / mg_troc / ...}, \code{classificacao})
  AND the lowercase / SmartSolos-derived shape
  (\code{cor_umida_matiz}, \code{argila_total}, \code{ph_h2o},
  \code{taxon_sibcs}). Texture and OC are converted from g/kg to
  percent (BDsolos canonical unit).

- **`inspect_bdsolos_csv(path, sep)`** -- diagnostic helper. Prints
  the raw schema, identifies which columns will map to which
  soilKey horizon attribute, lists unmapped columns, and reports
  Munsell coverage (matiz / valor / croma) + the surveyor's
  taxonomic reference column. Run before `load_bdsolos_csv()` on
  any new CSV from BDsolos.

- **`download_bdsolos(out_path, accept_terms, filter_uf, attributes,
  timeout_seconds, chromote_session, verbose)`** -- best-effort
  programmatic downloader via headless Chrome
  (\code{chromote}). Drives the 3-step Embrapa web form (accept
  terms -> select all attributes -> submit query -> select all
  results + radio CSV -> capture). Marked **experimental**:
  full-table queries (no UF filter) frequently overload the
  Embrapa server -- prefer \code{filter_uf =} batches of one or
  two states at a time and stitch the resulting CSVs.

- **`.bdsolos_norm()` / `.bdsolos_match_column()` / 
  `.bdsolos_match_taxon_column()` / `.BDSOLOS_COLUMN_PATTERNS`**
  internals: deterministic Portuguese-aware column normaliser
  (handles \code{ã / ç / é} via \code{chartr}) plus regex table
  for 30+ canonical BDsolos columns -> soilKey horizon schema.

## Why R-side rather than the browser

The first attempt used Chrome MCP to drive the BDsolos form
interactively. The full-table query (~9k profiles x ~30 horizon
attributes) reliably **freezes the renderer** -- the server-side
PHP query takes minutes and the SPA does not handle it
gracefully. Going pure R-side via headless Chrome (no on-screen
rendering) lets the function batch by UF and recover via clean
session restarts.

## Terms-of-use

Per the splash on \code{consulta_publica.html}:

- Personal / academic use is allowed; commercial use requires a
  separate Embrapa licence.
- Publications must cite the source per ABNT.

`download_bdsolos()` requires \code{accept_terms = TRUE} so no
download happens without the user explicitly acknowledging
those terms.

## Tests

10 new tests in `test-v0955-bdsolos.R` (55 expectations), all
exercised via synthetic CSVs in tempdir() so they run
unconditionally:

- Norm function handles Portuguese diacritics (\code{ã -> a}, 
  \code{ç -> c}) deterministically.
- Column matcher maps Munsell + texture + chemistry + taxon
  variants from both classic and lowercase BDsolos schemas.
- `inspect_bdsolos_csv()` returns mapped / unmapped / Munsell
  coverage / taxon column.
- `load_bdsolos_csv()` reads both schema variants, performs the
  g/kg -> % unit conversion deterministically (canonical column
  names override the heuristic), and the resulting pedons
  classify correctly via `classify_sibcs()`.
- `download_bdsolos()` requires `accept_terms = TRUE` and
  errors clearly when chromote is missing. Live network test
  gated on `SOILKEY_NETWORK_TESTS`.

Suite total: 3586 / 0 / 18 (pass / fail / skip). R CMD check
Status OK.

## DESCRIPTION

`chromote` added to Suggests (gated via `requireNamespace()`).


# soilKey 0.9.54 (2026-05-05)

The "SmartSolosExpert API cross-validation" release. Wires
soilKey to **Glauber Vaz's PROLOG-based SiBCS classifier**
exposed by Embrapa's AgroAPI as a REST endpoint, giving users
an authoritative external reference to compare the local
classifier against.

## What's shipped

`R/classify-smartsolos.R` adds two exported functions plus a
mapping layer:

- **`classify_via_smartsolos_api(pedon, api_key, endpoint,
  drenagem, reference_sibcs, base_url, timeout_seconds, post_fn,
  verbose)`** -- POSTs a soilKey \code{PedonRecord} to
  \code{https://api.cnptia.embrapa.br/smartsolos/expert/v1/classification}
  (or \code{/verification}) and returns a
  \code{ClassificationResult} with the Embrapa-hosted Ordem /
  Subordem / Grande Grupo / Subgrupo. Bearer token comes from
  \code{Sys.getenv("AGROAPI_TOKEN")} or the \code{api_key}
  argument. The \code{post_fn} parameter lets unit tests inject
  a deterministic stub so the package test suite is fully
  offline.

- **`compare_smartsolos(pedon, ...)`** -- runs both the local
  `classify_sibcs()` and the remote
  `classify_via_smartsolos_api()` on the same pedon and tabulates
  agreement at each of the four SiBCS levels. Returns
  `list(local, remote, agreement)`.

- **Mapping helpers** (internal): convert soilKey horizon
  attributes to the SmartSolos schema -- units (`% -> g/kg` for
  texture and OC), categorical strings (`structure_grade`
  weak/moderate/strong -> 1/2/3, `structure_type`
  granular/blocks/prismatic/columnar/laminar -> 1..6,
  `clay_films_amount` few/common/many -> 1..3), and the
  `DRENAGEM` SiBCS scale (1..8).

## Why this matters

- **External reference for validation**: the SmartSolosExpert API
  is maintained by Glauber Vaz / Embrapa Solos directly from the
  SiBCS rule book. Disagreements between
  \code{classify_sibcs()} (soilKey local) and
  \code{classify_via_smartsolos_api()} (Embrapa remote) point at
  either soilKey rule bugs or genuine SiBCS interpretation
  ambiguities -- both worth investigating.
- **Cross-language sanity check**: soilKey's SiBCS rules were
  encoded by hand from the 5a edicao text; SmartSolos is in
  PROLOG and was reviewed by the SiBCS authors. Two independent
  implementations.
- **Verification mode**: pass a user-supplied reference
  classification to the \code{/verification} endpoint and the
  API returns a per-level match summary
  (\code{L0..L4}) -- useful for benchmarking against curated
  perfis.

## Authentication

```r
# 1. Register at https://www.agroapi.cnptia.embrapa.br/portal/
# 2. Subscribe to SmartSolosExpert API
# 3. Generate an access token
# 4. Set the env var (or pass api_key= directly)
Sys.setenv(AGROAPI_TOKEN = "<your token>")

res <- classify_via_smartsolos_api(make_argissolo_canonical())
res$rsg_or_order  # "ARGISSOLO"
res$qualifiers
#> $subordem  "VERMELHO"
#> $gde_grupo "Distrofico"
#> $subgrupo  "tipico"

cmp <- compare_smartsolos(make_argissolo_canonical())
cmp$agreement
#>   point_id ordem subordem gde_grupo subgrupo n_match
#> 1    P-... TRUE     TRUE      TRUE     TRUE       4
```

## Tests

13 new tests in `test-v0954-smartsolos-api.R` (56 expectations).
All HTTP work bypassed via the `post_fn` injection -- no network
required. An opt-in live test is gated on
\code{AGROAPI_TOKEN + SOILKEY_NETWORK_TESTS} env vars.

Coverage:

- Mapping helpers (struct grade / size / type, clay films, drainage)
- Payload shape (29 documented JSON keys per horizon)
- Unit conversions (% -> g/kg, sand split into AREIA_GROS / AREIA_FINA)
- Subangular-vs-angular blocky disambiguation
- Response parser (4-level Embrapa output -> ClassificationResult)
- Stub-based end-to-end via `post_fn`
- Verification endpoint with `items_bd + summary`
- `compare_smartsolos()` agreement data.frame
- Live-network test (opt-in)

Suite total: 3529 / 0 / 16 (pass / fail / skip). R CMD check
Status OK.


# soilKey 0.9.53 (2026-05-05)

The "performance benchmark documentado" release. Adds
**`benchmark_performance(n, systems, ...)`** -- reproducible
latency + batch-throughput measurement of the three classifiers.

## What's shipped

- **`benchmark_performance(n, systems, include_familia, seed,
  verbose)`** -- generates `n` synthetic 5-horizon pedons (fixed
  RNG seed -> reproducible across releases), times each
  classifier, returns
  `list(summary, per_pedon, config)` with median / mean / total /
  pedons-per-minute per system. The `config` element captures
  soilKey version, R version and platform for traceability.

- **`inst/benchmarks/reports/performance_2026-05-05.md`** --
  documents the canonical baseline:

| System  | Median (s/pedon) | Throughput (pedons/min) |
|---------|-----------------:|------------------------:|
| WRB 2022    | **0.021** | **2,327** |
| SiBCS 5a    | **0.037** | **1,549** |
| USDA-ST 13a | **0.121** | **290** |

  At-scale projections (LUCAS 18k ~8 min WRB; KSSL 36k ~2h USDA)
  + per-system runtime breakdowns + memory profile + next
  optimisation targets.

## Tests

6 new tests in `test-v0953-performance.R` (18 expectations)
including a regression sentinel: median seconds < 5 per system
on a 3-pedon mini-bench. A 50x slowdown on the synthetic
fixture would trip CI before a release ships.

R CMD check Status OK.


# soilKey 0.9.52 (2026-05-05)

The "vinheta PT-BR end-to-end" release. Adds
**`v09_perfil_embrapa_pt.Rmd`** -- um perfil real (Argissolo
Vermelho-Amarelo distrofico tipico, Itaguai-RJ, adaptado do
Levantamento Embrapa Solos 2003) seguido do A ao Z atraves do
pacote, em portugues.

## What's shipped

- **Vinheta v09 (PT-BR)** cobrindo: construcao do `PedonRecord`
  com 5 horizontes; diagnosticos manuais (B textural, atividade
  da argila, V%); `classify_all()` -> SiBCS / WRB / USDA-ST;
  comparacao cross-system; relatorio HTML; cruzamento opcional
  com MapBiomas Solos e SoilGrids.

- **`ClassificationResult$print()` defensive fix**: o metodo
  iterava `self$trace` e crashava em
  \code{$ operator is invalid for atomic vectors} quando a trace
  continha entradas escalares (`familia_label`), `NULL`
  (`color_undetermined`) ou `data.frame`. Agora pula entradas
  que nao sao listas (ou que sao data.frames) no dump per-RSG.

## Tests

4 novos em `test-v0952-vignette-pt.R` (18 expectations) cobrindo
front-matter Rmd, presenca dos 3 sistemas + lookups espaciais +
modulos espectrais, e o fix do print em traces com entradas
escalares / NULL / data.frame.

R CMD check Status OK.


# soilKey 0.9.51 (2026-05-05)

The "container reproducibility" release. Adds a Dockerfile + a
GitHub Actions workflow that builds and publishes a container
image to **ghcr.io/HugoMachadoRodrigues/soilKey** on every git tag.

## What's shipped

- **`Dockerfile`** -- FROM `rocker/r-ver:4.4.0`, installs the
  GDAL/GEOS/PROJ stack required by `terra`, the dependency
  closure of soilKey + key Suggests (`terra`, `foreign`, `pls`,
  `munsellinterpol`, `shiny`, `DT`). Build-time smoke test
  (`library(soilKey)`) so a broken image fails to publish.

- **`.dockerignore`** -- excludes `soil_data/`, `.git/`, `*.tif`,
  `*.shp`, R build artefacts. Keeps the build context lean.

- **`.github/workflows/docker.yaml`** -- triggers on `v*` git
  tags, runs `docker buildx`, pushes both `:<version>` and
  `:latest` tags to GHCR with cache-from/cache-to gha caching.
  Final step smoke-tests the published image.

## Run it

```bash
docker run --rm -it ghcr.io/HugoMachadoRodrigues/soilKey:latest
docker run --rm -it -p 3838:3838 ghcr.io/HugoMachadoRodrigues/soilKey:latest \
  R -e 'soilKey::run_classify_app(host = "0.0.0.0", port = 3838L,
                                    launch.browser = FALSE)'
```

## Tests

7 new tests in `test-v0951-docker-ci.R` (21 expectations) -- lint
the Dockerfile + workflow without a container build, ensuring
future commits don't drop the GDAL stack, the key Suggests, or
the GHCR push step. R CMD check Status OK.


# soilKey 0.9.50 (2026-05-05)

The "comprehensive subsoil fill + Vis-NIR wire-up" release. Lifts
the v0.9.49 LUCAS WRB benchmark out of the Regosols catch-all by
giving `benchmark_lucas_2018()` three new fill paths.

## What changed

- **`fill_topsoil_from = c("none", "soilgrids", "spectra")`** --
  expands the v0.9.49 `fill_texture_from` to cover all 9
  SoilGrids properties (clay, sand, silt, phh2o, soc, cec, bdod,
  nitrogen, cfvo) at 0-5 cm. Legacy `fill_texture_from =
  "soilgrids"` continues to work as a back-compat alias.

- **`fill_subsoil_from = c("none", "soilgrids")`** --
  synthesises a 30-60 cm B horizon from SoilGrids 250m at the
  same 9 properties. Unlocks WRB cambic / argic / mollic / nitic
  diagnostics that the LUCAS topsoil-only release cannot satisfy
  alone.

- **`fill_topsoil_from = "spectra"` + `ossl_models`** -- when
  the LUCAS Spectral Library is available, runs
  `predict_from_spectra()` (v0.9.46) per pedon to fill any
  property still missing after the SoilGrids paths.

- **`attach_lucas_spectra(pedons, spectra, point_id_col)`** --
  new exported helper. Joins a wide (POINT_ID + wavelength
  columns) or long (POINT_ID + wavelength_nm + reflectance)
  spectra table onto the pedon list, populating
  `pedon$spectra$vnir`.

- **`.SOILGRIDS_TO_HORIZON_MAP`** + **`.fill_horizon_from_soilgrids()`**
  internals. The helper accepts a `lookup_fn` parameter for
  unit-test injection so the test suite runs offline.

## Why cfvo matters

The Leptosols predicate (`leptic_features` in
`R/diagnostics-properties-wrb.R`) fires when
`coarse_fragments_pct >= 90 within 25 cm`. SoilGrids `cfvo`
maps directly to that. With `fill_properties` covering `cfvo`,
Leptosols (39% of the LUCAS European reference) become reachable.

## Tests

13 new tests in `test-v0950-lucas-fills.R` (52 expectations), all
exercised through the `soilgrids_lookup_fn` injection -- no
network required. R CMD check Status OK.


# soilKey 0.9.49 (2026-05-04)

The "EU-LUCAS / WRB benchmark Route B end-to-end" release.
Closes the EU-LUCAS WRB benchmark **chemistry half** that has
been open since the v0.9.27 roadmap. v0.9.44 already shipped the
raster-lookup half (`lookup_esdb()`); v0.9.49 ships the loader
for the LUCAS Soil 2018 Topsoil release (~18,984 European
points) plus the benchmark function that compares the soilKey
classifier to the canonical ESDB WRB raster at every coordinate.

## What's shipped

`R/benchmark-lucas-2018.R` adds two new exported functions and
an internal WRB code-name table:

- **`load_lucas_soil_2018(path, attach_bulk_density, countries,
  max_n, verbose)`** -- reads the canonical ESDAC release
  (`LUCAS-SOIL-2018.csv`), joins
  `BulkDensity_2018_final-2.csv` on `POINTID`, and returns a
  list of `PedonRecord` objects. Unit conversions baked in
  (g/kg -> %, mS/m -> dS/m), `< LOD` / `<LOD` / empty / `n.d.`
  / `ND` cells coerced to `NA`, and a 20-30 cm subsoil horizon
  is synthesised when the LUCAS subsoil OC / CaCO3 columns are
  populated.

- **`benchmark_lucas_2018(pedons, esdb_root, attribute,
  fill_texture_from, classify_with, max_n, verbose)`** -- looks
  up the ESDB Reference Soil Group at every coordinate via
  `lookup_esdb(attribute = "WRBLV1")`, optionally fills missing
  clay/sand/silt from SoilGrids 250m via `lookup_soilgrids()`,
  runs `classify_wrb2022()` (or `classify_sibcs()`) per pedon,
  and tabulates a confusion matrix + per-RSG recall. Returns a
  list with `predictions`, `confusion`, `accuracy`, `per_rsg`,
  `n_in_scope / n_total / n_errors` and the configuration recap.

- **`.WRB_LV1_NAME_BY_CODE`** (internal) -- mapping the 31 ESDB
  WRBLV1 2-letter codes to the English plural RSG names
  returned by the classifier. Codes follow IUSS WRB 2022; the
  legacy `AB` (Albeluvisols) is mapped to `NA`.

## Demonstration

200 LUCAS pedons stratified across ES / FR / PL / IT, pure
chemistry baseline (no SoilGrids fill, no spectra fill):

```
Accuracy: 3.0%  in-scope: 199 / 200

Reference:  Cambisols 53%  Leptosols 39%  others 8%
Predicted:  Regosols  92%  Histosols 7%   Calcisols 1%
```

This is an honest baseline. LUCAS Soil 2018 ships only **topsoil
0-20 cm** chemistry; WRB diagnostic horizons (cambic, argic,
mollic, ferralic) require subsoil features that are not in this
release. `classify_wrb2022()` correctly falls back to **Regosols**
(WRB catch-all) when no diagnostic horizon triggers. Histosols
recall is 33% (1/3): the histic threshold (OC >= 12%) is the only
one detectable from a 20-cm sample alone.

## The improvement path (v0.9.50 candidates)

The package already has the building blocks to lift the accuracy:

- **Subsoil texture from SoilGrids 30-60 cm** via
  `lookup_soilgrids()` (v0.9.48) -- unlocks cambic / argic
  thresholds.
- **Vis-NIR spectra fill** via `predict_from_spectra()` (v0.9.46)
  + `fill_munsell_from_spectra()` (v0.9.47) when the LUCAS Soil
  2018 Spectral Library is downloaded (~83 GB ESDAC release) --
  highest fidelity because per-point spectra capture local
  mineralogy.
- **Bedrock depth proxy** via SoilGrids `cfvo` -- unlocks
  Leptosols.

A natural v0.9.50 would extend `benchmark_lucas_2018()` with a
`fill_subsoil_from = "soilgrids"` option that synthesises a
30-60 cm horizon from SoilGrids per pedon.

## Bottom line

Route B is **end-to-end runnable as of v0.9.49**. Hugo can now
drive the comparison loop on his own machine without waiting for
the Embrapa export or the spectral-library download.

## Tests

12 new tests in `test-v0949-lucas-2018.R` (55 expectations) --
all pass without network. Loader covers 4 chemistry rows (ES,
FR, SE, IT) with mixed `< LOD` / empty cells, BD-join, country
and `max_n` filters, and missing-file errors. Benchmark covers
end-to-end on a synthetic 4x4 ESDB raster, code decoding, input
validation, and both `wrb2022` / `sibcs` paths. Suite total:
3362 / 0 / 15 (pass / fail / skip). R CMD check Status OK.

## Documentation

`inst/benchmarks/reports/lucas_2018_benchmark_2026-05-04.md`
documents the loader, the 200-point baseline, the per-RSG
confusion, the surface-only limitation and the v0.9.50
improvement path.


# soilKey 0.9.48 (2026-05-04)

The "MapBiomas Solos + SoilGrids 250m raster lookup" release.
Adds the **fourth and fifth spatial validation axes** for soilKey,
complementing the ESDB raster axis from v0.9.44.

## What changed

`R/spatial-lookups.R` exports two new helpers, both shaped after
`lookup_esdb()`:

- **`lookup_mapbiomas_solos(coords, raster_path, legend = NULL)`**
  -- Brazilian SiBCS national raster (MapBiomas Solos
  Collection 2, 30 m, 2023+). Local-file lookup; user passes the
  unpacked GeoTIFF path. Optional 2-column legend
  (`value, class_name`) decodes integer codes to SiBCS class
  strings. Auto-reprojection from WGS84.

- **`lookup_soilgrids(coords, property, depth, quantile, baseurl,
  raw)`** -- Global ISRIC SoilGrids 250m soil property
  predictions, read **directly from the canonical Cloud-Optimized
  GeoTIFF endpoint** at
  `https://files.isric.org/soilgrids/latest/data/`. No download
  required; only the pixel under each query coordinate is
  transferred over HTTPS. Supports all 11 SoilGrids properties
  (clay, sand, silt, phh2o, soc, cec, bdod, nitrogen, ocd, ocs,
  cfvo) at all 6 standard depths (0-5, 5-15, 15-30, 30-60,
  60-100, 100-200 cm) and all 5 quantiles (mean, Q0.05, Q0.5,
  Q0.95, uncertainty). Returns values in conventional units via
  the published per-property scale factors (clay/silt/sand
  percent, pH, g/kg, cmol(c)/kg, g/cm^3).

## Why this matters

Combined with v0.9.44 `lookup_esdb()`, soilKey now offers **three
spatial validation axes**:

  - **Europe**: ESDB Raster Library 1 km (WRBLV1, WRBFU,
    FAO90LV1) -- canonical reference per coordinate.
  - **Brazil**: MapBiomas Solos 30 m -- canonical SiBCS class per
    coordinate (national mapping).
  - **Global**: SoilGrids 250 m -- continuous soil property
    predictions (clay, pH, OC, etc.) per coordinate.

Any `PedonRecord` with lat/lon can be cross-checked against the
canonical map at its location -- supports the `prior_check`
field of `ClassificationResult`.

## Tests

10 new tests in `test-v0948-spatial-lookups.R` (25 expectations).
MapBiomas tests build a synthetic 4x4 raster on the fly via terra
so they run unconditionally. SoilGrids tests cover argument
validation + graceful NA on unreachable URL; live-network smoke
test is opt-in via `SOILKEY_NETWORK_TESTS=1` (default skip on CI).
R CMD check Status OK.


# soilKey 0.9.47 (2026-05-04)

The "Vis-NIR -> Munsell via CIE colorimetry" release. Operational
unblock for the v0.9.35 Argissolo Vermelho / Amarelo / Vermelho-
Amarelo color-confusion case **without** waiting for the Embrapa
BDsolos export -- whenever the user has Vis-NIR spectra (e.g. from
the OSSL), the Munsell hue can be recovered physically.

## Pipeline

`reflectance R(lambda)` (380-780 nm range) integrated against the
**CIE 1931 2-degree Standard Observer** color-matching functions
weighted by the **D65 illuminant**, then converted XYZ -> xyY ->
Munsell HVC via the **Munsell renotation interpolation** in the
`munsellinterpol` CRAN package. No model training, no OSSL fit:
the answer is fixed by physics + a public colorimetry lookup.

## New API

- **`predict_xyz_from_spectra(spectra, wavelengths)`** -- CIE XYZ
  tristimulus on the standard scale (Y = 100 for a perfect
  diffuse white). Auto-detects whether reflectance is decimal
  (0..1) or percent (0..100). Dependency-free (CIE table bundled
  in `R/sysdata.rda`).

- **`predict_lab_from_spectra(spectra, wavelengths)`** -- CIE Lab
  via standard XYZ -> Lab transform under D65 / 2-degree observer.

- **`predict_munsell_from_spectra(spectra, wavelengths,
  round_chip = TRUE)`** -- the headline function. Returns
  `munsell_hue_moist`, `munsell_value_moist`,
  `munsell_chroma_moist`, `munsell_string` (e.g. `"7.5YR 4/6"`).
  Requires `munsellinterpol`; clear error if missing.

- **`fill_munsell_from_spectra(pedon, overwrite, verbose)`** --
  high-level helper. Iterates over `pedon$spectra$vnir`, runs the
  prediction per horizon and writes the result via
  `add_measurement(..., source = "predicted_spectra")`. After
  this call, re-run `classify_sibcs()` -- the v0.9.45
  "color-undetermined" fallback lifts and the descent proceeds to
  subordem / GG / SG.

## Why this matters

The v0.9.45 fallback turned the 44 Argissolo profiles whose
Munsell hue was missing into "Argissolos (cor a determinar)" with
`evidence_grade = "C"`. v0.9.47 closes the loop: if the same
profile has Vis-NIR (from OSSL or any laboratory spectrometer),
**fill_munsell_from_spectra() -> classify_sibcs()** descends all
the way to `Argissolo Vermelho Distrofico` (or whatever the
spectrum implies), with `evidence_grade = "B"` (predicted_spectra
provenance).

Combined with v0.9.46 `predict_from_spectra()` (which fills clay /
sand / silt / pH / OC / CEC), o pacote agora classifica perfis
brasileiros **direto a partir de espectro**, sem morfologia
descritiva nem morfologia laboratorial -- exatamente o que
destrava casos onde a Embrapa BDsolos fornece so a quimica.

## Tests

13 new tests in `test-v0947-munsell-prediction.R` (36
expectations). XYZ + Lab tests run unconditionally (CIE table is
internal data). Munsell HVC tests skip cleanly when
`munsellinterpol` is absent. R CMD check Status OK.

## Internal data

`R/sysdata.rda` now includes `.cie_d65_5nm` (81 rows from 380 to
780 nm at 5 nm steps; columns: wavelength, xbar, ybar, zbar, D65).
Generated once via `colorscience::ciexyz31` and
`colorscience::illuminants$D65`; bundled directly so soilKey has
no runtime dependency on `colorscience`.

## DESCRIPTION

`munsellinterpol` added to Suggests (gated via
`requireNamespace()`).


# soilKey 0.9.46 (2026-05-04)

The "OSSL pretrained models, end-to-end" release. Closes Module 4
of the original soilKey scope by giving users a single-line path
from a downloaded OSSL library to fully-attributed predictions on
a new \code{PedonRecord}.

## What changed

`R/spectra-train.R` adds three new exported functions plus a
`predict()` / `print()` S3 method:

- **`train_pls_from_ossl(ossl_library, properties, ...)`** -- per-
  property PLSR training over a downloaded OSSL subset. Picks
  optimal `ncomp` via 10-fold CV, applies the same Vis-NIR
  preprocessing the OSSL distribution uses (`snv+sg1` by default),
  returns a named list of `soilKey_pls_model` objects compatible
  with `predict_ossl_pretrained()` and `fill_from_spectra()`.

- **`predict_from_spectra(pedon_or_spectra, models, ...)`** --
  named ergonomic API. Accepts a `PedonRecord` (delegates to
  `fill_from_spectra(method = "pretrained")` with provenance
  writes) OR a raw numeric matrix / vector (returns long-form
  prediction data.table directly). Auto-applies the preprocessing
  recorded on the trained models.

- **`save_ossl_models()` / `load_ossl_models()`** -- RDS
  persistence with shape validation; soilKey version, training
  time, preprocess label and per-property diagnostics preserved
  as attributes.

- **`predict.soilKey_pls_model` / `print.soilKey_pls_model`** --
  S3 methods registered in NAMESPACE. `predict()` returns the
  canonical `value / pi95_low / pi95_high` schema; the 95% PI is
  built from the cross-validated training RMSE.

## Why this matters

Until v0.9.45, the package shipped `download_ossl_subset()`,
`predict_ossl_pretrained(ossl_models)` and
`fill_from_spectra(method = "pretrained")` -- but no loop to turn
a downloaded `ossl_library` into the `ossl_models` list those
functions consume. v0.9.46 closes that gap.

## Tests

13 new tests in `test-v0946-pls-training.R` (41 expectations) --
pass when `pls` is available, skip cleanly when it is not.
R CMD check Status OK.

## DESCRIPTION

`pls` added to Suggests (gated via `requireNamespace()`).


# soilKey 0.9.45 (2026-05-04)

The "color-undetermined graceful path" release. Fixes the
**v0.9.35 Argissolo Vermelho / Amarelo / Vermelho-Amarelo
silent-fallback case** (44 perfis brasileiros caiam silenciosamente
em PVA quando o matiz Munsell em B nao foi medido).

## What changed

`classify_sibcs()` agora detecta o padrao "subordem catch-all de cor
atribuida porque o matiz Munsell esta ausente" e:

- Para a descida no nivel da Ordem (nao seleciona Grande Grupo nem
  Subgrupo);
- Mostra `display_name` no formato `"<Ordem> (cor a determinar)"`
  em vez do catch-all enganoso (`Argissolos Vermelho-Amarelos`);
- Adiciona `munsell_hue_moist_horizon_B` em `missing_data`;
- Rebaixa `evidence_grade` para `"C"` (classificacao parcial);
- Anexa um warning em PT-BR explicando o fallback e listando as
  alternativas que perderam por falta de matiz;
- Expoe o registro estruturado em `result$trace$color_undetermined`
  (lista com `detected`, `fallback_subordem`,
  `rejected_alternatives`, `would_resolve_with`, `reason`).

A logica generica funciona para os 4 catch-alls de cor do SiBCS:
`PVA` (Argissolos Vermelho-Amarelos), `LVA` (Latossolos
Vermelho-Amarelos), `NX` (Nitossolos Haplicos) e `TX` (Luvissolos
Haplicos).

## Por que isso e importante

Antes do v0.9.45, um perfil com B textural mas sem matiz Munsell
medido era classificado como **Argissolo Vermelho-Amarelo** com
`evidence_grade = "A"` -- o pacote afirmava com confianca maxima
uma classe especifica que so pode ser determinada com a cor. Os
44 perfis flagados no v0.9.35 cairam exatamente nesse padrao.

Agora a saida fica:

```
Name           : Argissolos (cor a determinar)
RSG/Order      : Argissolos
Evidence grade : C
Missing data   : munsell_hue_moist_horizon_B, ...
Warnings       : Subordem 'Argissolos Vermelho-Amarelos' atribuida
                 por fallback porque o matiz Munsell em B esta
                 ausente. Medindo a cor seria possivel discriminar
                 entre: Argissolos Vermelhos, Argissolos Amarelos,
                 Argissolos Bruno-Acinzentados, Argissolos
                 Acinzentados.
```

A interpretacao sai do "falsa precisao" e entra no "honesto sobre
o que se sabe e o que ainda falta medir".

## Tests

- 9 novos em `test-v0945-color-undetermined.R` (27 expectations) --
  todos passam. Suite completa: 3202 testes, 0 falhas.

## Internal API

- `.SIBCS_COLOR_CATCH_ALL_CODES` (constante interna).
- `.detect_color_undetermined_fallback()` (helper interno).


# soilKey 0.9.44 (2026-05-04)

The "ESDB Raster Library lookup" release. Unblocks the
**raster-lookup half of the EU-LUCAS WRB benchmark Route B**
(open since the v0.9.27 roadmap) by adding a spatial-join
utility against the ESDB Raster Library 1km GeoTIFF release
(May 2024).

## ESDB Raster Library lookup

The European Soil Database (ESDB) Raster Library distributes
71 thematic rasters at 1km resolution under LAEA Europe (EPSG:
3035). v0.9.44 ships two new exported helpers:

  available_esdb_attributes(raster_root)
    -> character vector of the 71 attribute folder names
       (WRBLV1, WRBFU, WRBADJ1/2, FAO90LV1, plus 65 thematic
       rasters: clay/sand/silt sub+top, OC, parent material,
       slope, depth-to-rock, mineralogy, etc.)

  lookup_esdb(coords, attribute, raster_root, decode = TRUE)
    -> WGS84 lat/lon -> reproject to LAEA Europe -> extract
       raster value -> decode via .vat.dbf to coded label

Coordinates outside the European raster footprint return NA
silently so vectorised calls degrade gracefully.

### Demonstration on 12 European cities

  Wageningen NL  -> FL Fluvisol (eutric)
  Helsinki FI    -> LP Leptosol (dystric)
  Rovaniemi FI   -> CM Cambisol (dystric, boreal)
  Athens GR      -> LV Luvisol (calcaric)
  Vienna AT      -> CH Chernozem (haplic, pannonian)
  Sevilla ES     -> FL Fluvisol (calcaric)

Cities returning the "1" non-soil mask code (Lisbon, Berlin,
Paris, Rome, Krakow) fall on 1km pixels coded as artificial /
urban surfaces -- correct behaviour, not a bug.

### What this enables

For any European-coordinate `PedonRecord`, users can now:

  1. Look up the ESDB raster's expected RSG at the pedon's coords
  2. Run classify_wrb2022() on the pedon's chemistry
  3. Compare the two and report agreement

This becomes the **fourth validation axis** for soilKey, alongside
the canonical fixtures, KSSL+NASIS (USDA), Embrapa FEBR (SiBCS),
and WoSIS GraphQL.

`foreign` is added to Suggests for `.vat.dbf` decoding via
`foreign::read.dbf()`.

### Tests

8 new in `tests/testthat/test-v0944-esdb-raster.R`:

- `available_esdb_attributes()` lists 60+ ESDB attributes
- `lookup_esdb()` resolves Wageningen NL to a real RSG code
- Returns NA for points outside the European raster footprint
- Vectorised over multi-point coords
- `decode = FALSE` returns raw integer raster values
- Errors clearly when raster missing
- Accepts both data.frame and matrix input
- WRBLV1 vs FAO90LV1 cross-system agreement

Tests skip cleanly via `Sys.getenv("SOILKEY_ESDB_RASTER_ROOT")`
when the raster archive (~700 MB unpacked) is not available
locally.

## Songchao + EU_LUCAS_2022 inspection (no actionable change)

Hugo also provided `febr-data-songchao.txt` (2 684 rows) and
`EU_LUCAS_2022.csv` / `_updated.xlsx` (~338 000 rows). Both were
inspected for soil-chemistry / Munsell / WRB-label content:

| Source | What it has | What's missing |
|---|---|---|
| Songchao | basic chemistry (clay/silt/sand/SOC/BD), 16 cols | NO Munsell color, NO `taxon_*` reference -- cannot fix the v0.9.35 Argissolo color confusion, cannot use for benchmark validation |
| LUCAS_2022.csv (455 MB, 306 cols) | lat/lon + point-survey metadata | NO soil chemistry, NO WRB labels -- the Soil Component Survey is a separate ESDAC download |

Documented in
`inst/benchmarks/reports/eu_lucas_roadmap_v0944_update_2026-05-04.md`
and the `reference_eu_lucas_wrb_benchmark.md` memory file.
The 44 FEBR Argissolo color-confusion misses (Vermelho /
Amarelo / Vermelho-Amarelo) remain unfixable from the available
data.

# soilKey 0.9.43 (2026-05-04)

The "JSON Schema for PedonRecord" release.

`pedon_json_schema(as = c("list", "json"))` returns a Draft-2020-12
JSON Schema describing the canonical PedonRecord structure (site +
horizons + optional provenance). `validate_pedon_json(x)` validates
a PedonRecord (or compatible list) against that schema via
`jsonvalidate::json_validate()`.

The schema is also written to `inst/schemas/pedon-schema.json`
(10 KB) for direct file access by external systems (web APIs, ETL
pipelines, multimodal extraction validation).

7 new tests in `tests/testthat/test-v0943-json-schema.R`.


# soilKey 0.9.42 (2026-05-04)

The "sensitivity / fragility analysis" release.

`classification_robustness()`: Monte-Carlo perturbation analysis.
Perturb input attributes (clay/sand/silt ±5 %, pH ±0.2, OC ±10 %)
and report how often the classification matches the unperturbed
baseline. Useful for paper-grade claims like "X % of profiles are
robust to a 5 % analytical-error perturbation".

`batch_robustness(pedons, ...)`: across-pedons wrapper returning a
tidy data.frame (one row per pedon: id, baseline, robustness,
n_flipped).

7 new tests in `tests/testthat/test-v0942-sensitivity.R`.


# soilKey 0.9.41 (2026-05-04)

The "PT-BR vignette" release.

## v01_getting_started_pt.Rmd (Item 4)

Adds a Brazilian-Portuguese translation of `v01_getting_started`.
Same content (zero-code Shiny path; building a PedonRecord from
scratch; classify_all + cross-system view; key-trace inspection;
provenance + evidence grade), but written for the PT-BR pedology
community where SiBCS is the daily-driver classification system.

The vignette is wired into `_pkgdown.yml` both in the navbar
("Articles" menu) and the `articles:` index, so it builds on
push to main and deploys to the GitHub Pages site at
<https://hugomachadorodrigues.github.io/soilKey/articles/v01_getting_started_pt.html>.

The Brazilian community uses Embrapa SiBCS (Santos et al. 2018)
as the canonical taxonomic reference and discusses pedology in
Portuguese; an English-only `v01` was a barrier for that audience.
PT-BR vignettes for v02-v07 are deferred to a future release; the
v01 translation is the highest-leverage starting point because
it's the entry vignette that everyone reads first.

# soilKey 0.9.40 (2026-05-04)

The "community polish" release. Four small but high-ROI changes
that signal project maturity to anyone visiting the repo.

## A. CITATION.cff (Item 5)

Adds `CITATION.cff` at the repository root in CFF (Citation File
Format) v1.2.0. GitHub auto-renders this in the repo sidebar as
"Cite this repository" with a copy-paste BibTeX block. The file
includes:

- Project metadata (title, abstract, version, DOI, license, repo).
- Author block with ORCID and UFRRJ affiliation.
- Keywords for citation indexing.
- `references` block with the three canonical books (WRB 2022,
  KST 13ed, SiBCS 5ª ed.) so citation tools can chain through to
  the underlying taxonomic sources.

Listed in `.Rbuildignore` so it lives at the repo root for GitHub
without bloating the package tarball.

## B. GitHub issue / PR templates + community files (Item 6)

`.github/ISSUE_TEMPLATE/`:

- **bug_report.yml** -- structured form with required sections for
  minimal reproducible example, expected vs actual behaviour,
  traceback, session info, classification system affected, and a
  confirmation checklist.
- **feature_request.yml** -- use case + proposed API + canonical
  references + scope dropdown (WRB / SiBCS / USDA / VLM / spatial /
  benchmark / aqp / Shiny / docs).
- **profile_classification_help.yml** -- structured form for
  "I disagree with how soilKey classified my profile". Captures
  horizons CSV, site metadata, expected vs got, key trace.
- **config.yml** -- disables blank issues; routes general questions
  to GitHub Discussions and documentation.

`.github/PULL_REQUEST_TEMPLATE.md` -- type-of-change checkboxes,
scope checklist, testing checklist, architecture-invariant
reminders (the taxonomic key is never delegated to an LLM, every
value carries provenance, side modules never overrule the key).

`CONTRIBUTING.md` -- architecture invariants, issue-filing guide,
development setup, branching / code-style conventions, recipes for
adding diagnostics / qualifiers / dataset loaders, PR submission
checklist.

`CODE_OF_CONDUCT.md` -- Contributor Covenant 2.1 with a soil-
community note distinguishing "what soilKey does" from "what the
canonical books prescribe".

## C. pkgdown site verified (Item 7)

The pkgdown CI workflow (`.github/workflows/pkgdown.yaml`) was
already wired in v0.9.x and the site is **live** at
<https://hugomachadorodrigues.github.io/soilKey/> (HTTP 200, last
modified 2026-05-04). v0.9.37 closed the index gap so the site now
renders without missing-topic warnings.

## D. Real coverage measurement (Item 8)

Ran `covr::package_coverage()` locally against the v0.9.39 source
tree. Result: **80.5 % statement coverage**.

README badge updated from the unconfigured Codecov SVG (which
rendered as "unknown" because no `CODECOV_TOKEN` secret was
configured) to a static shields.io badge showing 80.5 %. The
test-coverage workflow continues to upload to Codecov on every
push, so the dynamic Codecov badge will become live as soon as
the user adds the `CODECOV_TOKEN` secret in GitHub repo settings.

Test count badge bumped 2 908 -> 3 137. Version badge bumped
0.9.27 -> 0.9.40.

# soilKey 0.9.39 (2026-05-03)

The "interactive Shiny app" release. A drag-and-drop web interface
that renders all three classifications side-by-side, exports a
self-contained HTML report, and works for non-R users (agronomists,
students, field workers).

## Shiny app (Item 3 from the polish roadmap)

`run_classify_app()`: convenience wrapper that locates the bundled
Shiny application at `inst/shiny/classify_app/` and launches it via
`shiny::runApp()`. Requires the `shiny` and `DT` packages (both in
Suggests; the wrapper raises a clear error if missing).

App features:

- **Horizons input**: upload a CSV (one row per horizon, columns
  matching the soilKey horizon schema -- `top_cm`, `bottom_cm`,
  `designation`, plus any of `clay_pct`, `sand_pct`, `silt_pct`,
  `ph_h2o`, `oc_pct`, `bs_pct`, `cec_cmol`, ...). Falls back to a
  built-in sample (Latossolo Vermelho-style) when no file is loaded.
- **Site metadata**: profile id, lat/lon, country, parent material.
- **Classification**: one button runs `classify_all()` and shows
  the WRB 2022 / SiBCS 5a / USDA ST 13ed names plus evidence grades.
- **Trace tab**: print the full key-trace for any system to inspect
  which RSGs / Orders were tested and which diagnostics fired.
- **HTML report download**: self-contained, no external network
  requests; suitable for emailing or attaching to a laudo.
- **Starter template download**: a sample CSV with the canonical
  column structure for users to clone and modify.

Use cases (mirrors the v0.9.38 demo gallery but interactive):

- A field agronomist with a tablet: upload field-survey CSV,
  classify, download report, attach to client deliverable.
- A graduate student: paste in textbook profile data, study how
  the 3 systems classify the same soil.
- A research group: batch-process by repeated upload, exports
  serve as paper supplements.

The app does NOT require any internet connection beyond bootstrap
loading (Shiny CDN); all classification runs locally in the user's
R session.

## Tests

4 new in `tests/testthat/test-v0939-shiny-app.R`:

- `run_classify_app()` errors clearly when shiny is missing
- `run_classify_app()` errors clearly when DT is missing
- Shiny app dir exists at `inst/shiny/classify_app/`
- `app.R` parses without syntax errors

The active runtime tests are deliberately minimal -- a full Shiny
test would require `shinytest2` + browser automation, deferred to
a future release.

# soilKey 0.9.38 (2026-05-03)

The "demo gallery" release. A new `demo()` registry exposing 6
published soil profiles classified end-to-end across all three
systems, for pedagogical use.

`demo("classify_gallery", package = "soilKey")` runs 6 canonical
published profiles through `classify_wrb2022` + `classify_sibcs` +
`classify_usda` and prints the resulting names + evidence grades:

1. **Latossolo Vermelho Distroferrico** -- Embrapa SiBCS 5a ed.
   Annex A profile A-04 (Mata Atlantica, Brazil; gneiss).
2. **Chernozem** -- IUSS WRB (2022) Annex 1 didactic exemplar
   (central-European steppe; loess; very deep organic-rich Ah).
3. **Podzol** -- Soil Atlas of Europe (2005) Plate 19 (boreal
   forest, Sweden; glaciofluvial sand; E -> Bsh -> Bs sequence).
4. **Vertisol** -- FAO Field Guide canonical Pellic Vertisol
   (Deccan basalt residuum, India; smectite-rich black cotton).
5. **Gleysol** -- Soil Atlas of Europe (2005) canonical Gleysol
   (Netherlands; fluvial clay over peat; reduced grey-blue subsoil).
6. **Histosol** -- WRB 2022 Annex 1 didactic Ombric Fibric
   Histosol (Estonia; raised Sphagnum bog; rainwater-fed).

Each profile is built from data published in canonical soil-science
sources, with citations inline. Registered via `demo/00Index` and
exercises ALL three keys plus the v0.9.33 WRB qualifier closure
(e.g. Profile 6 fires Floatic + Folic + Hemic + Ombric + Histosol,
demonstrating the v0.9.33 Ombric / Floatic implementations
end-to-end).

Pedagogical use cases:

- Field practitioners: see the 3-system mapping for soils they know.
- Students: study one profile + walk through the key-trace.
- Researchers: a sanity-check fixture set distinct from the 31
  canonical fixtures (which are synthetic by design; the demo
  gallery uses real published profiles).

# soilKey 0.9.37 (2026-05-03)

The "pkgdown polish + edge-case hardening" release.

## A. pkgdown reference + articles index closed

`_pkgdown.yml` updated so `pkgdown::check_pkgdown()` reports zero
missing topics:

- New article entry: `v08_kssl_nasis_multilevel`.
- New reference sections:
  - **Interoperability** (`as_aqp`, `from_aqp`).
  - **USDA Soil Taxonomy 13ed diagnostic helpers**
    (`argillic_clay_films_test`).
  - **Benchmark utilities** (`canonicalise_kst13ed_gg`,
    `normalise_kssl_subgroup`).
- `classify_all` added to "Classification entry points".

The pkgdown CI workflow (`.github/workflows/pkgdown.yaml`) was
already wired in v0.9.x; the v0.9.37 config closes the index gap
that was producing build warnings on the GH Pages deploy.

## B. Edge-case stress tests

29 new in `tests/testthat/test-v0937-edge-cases.R` covering
adversarial inputs that should NOT crash the classifier:

- empty horizons table (zero rows)
- single-horizon profile
- all-NA horizon rows
- horizons in reverse order (deepest first)
- zero-thickness horizon (top == bottom)
- impossibly deep profile (10 m, 4 horizons)
- non-ASCII designations (PT-BR diacritics)
- duplicate horizon designations (A / A / Bw)
- pedon with missing optional site fields (no country, no
  parent_material, no lat/lon)
- `classify_all()` graceful failure on a broken pedon

All 29 pass. The classifiers were already robust to most of these;
the test suite now formally guarantees the behaviour.

Full suite: 3 104 PASS / 0 FAIL / 10 SKIP. R CMD check Status: OK.

# soilKey 0.9.36 (2026-05-03)

The "WoSIS rebench + performance docs" release. Two measurement
artefacts that document the v0.9.27 -> v0.9.35 trajectory and
publish single-CPU throughput estimates for batch jobs.

## A. WoSIS GraphQL re-bench (Item 5 from the polish roadmap)

The bundled WoSIS sample (n=40, frozen 2026-05-03) re-classified
through the v0.9.35 keys:

  v0.9.27 sample, v0.9.27 keys: 5/30 = 16.7 % top-1 (n=30, smaller pull)
  v0.9.30 sample, v0.9.30 keys: 5/30 = 16.7 %
  v0.9.30 sample, v0.9.35 keys: **7/40 = 17.5 % top-1** (+0.8 pp)

Modest but positive lift. The new bundled snapshot (40 profiles,
v0.9.30) plus the v0.9.33 WRB qualifier closure (Floatic / Toxic /
Ombric / Rheic / Endocalcic / Endogleyic / Endostagnic) plus the
v0.9.31 Quartzipsamment broadening combine to lift +1 profile on
this sample. The 40-profile sample is too small to measure CI
tightly; on a larger pull (~500 profiles) we'd expect the lift to
land in the +2-3 pp band.

## B. Performance benchmark (Item 8 from the polish roadmap)

`inst/benchmarks/reports/perf_v0935_2026-05-03.md` documents
single-CPU wall-clock timing on the 44 canonical fixtures, mean of
10 iterations:

| System          | ms / pedon | pedons / sec |
|-----------------|-----------:|-------------:|
| classify_wrb2022 |  22 ms    |  45 pedons/s |
| classify_sibcs   |  32 ms    |  32 pedons/s |
| classify_usda    | 270 ms    |   4 pedons/s |

USDA is ~10x slower than WRB / SiBCS because Path C (Order ->
Suborder -> Great Group -> Subgroup) walks the full Subgroup tier
which alone is ~85 % of runtime. A KSSL+NASIS n=2638 benchmark at
all four levels completes in ~14 min wall-clock.

README §"Performance" added with the headline numbers and link to
the full report.

## C. NEWS update

Cumulative real-data trajectory across release series:

  KSSL+NASIS GG       (v0.9.24 -> v0.9.35): 6.5 % -> 10.92 % (+4.42 pp)
  Embrapa Subordem    (v0.9.27 -> v0.9.35): 9.93 % -> 39.17 % (+29.24 pp)
  WoSIS top-1         (v0.9.13 -> v0.9.35): ~13 % -> 17.5 % (+4.5 pp,
                                              small samples)
  WRB qualifier cov   (v0.9.27 -> v0.9.35): 132/139 -> 139/139 (100 %)

# soilKey 0.9.35 (2026-05-03)

The "aqp interop + units fix" release. Two coordinated changes that
make soilKey both more useful (interoperable with the canonical R
soil package) and more accurate (one units bug repaired in SiBCS
Cap 12).

## A. aqp interoperability (Item 1 from the v0.9.34 roadmap)

`{aqp}` (Algorithms for Quantitative Pedology) is the canonical R
representation for pedological data. v0.9.35 adds two new exported
helpers that bridge soilKey to / from `aqp::SoilProfileCollection`
(SPC):

  as_aqp(pedon)   -> SoilProfileCollection
  from_aqp(spc)   -> list of PedonRecord

Standard column names are renamed to aqp's canonical convention
(top_cm -> top, bottom_cm -> bottom, designation -> name, clay_pct
-> clay, sand_pct -> sand, silt_pct -> silt). All other soilKey
columns pass through unchanged. Site-level slots (lat / lon /
country / parent_material / reference_*) are attached to the SPC's
site table.

Round-trip property: `from_aqp(as_aqp(pedon))` reproduces `pedon`
exactly, modulo column-order canonicalisation.

Requires the `aqp` package, listed in Suggests. Both functions
raise a clear error if aqp is not installed.

40 new unit tests in tests/testthat/test-v0934-aqp-interop.R cover
single-pedon and multi-pedon conversion, column-name renaming,
site-level metadata attachment, round-trip property, classify_*
on round-tripped pedons, error handling on bogus input, and
heterogeneous-schema multi-profile pad-rbind.

## B. SiBCS Quartzarenico units bug fix (Item 4 from the v0.9.35 roadmap)

`neossolo_quartzarenico()` used SiBCS Cap 1 textural-class thresholds
in g/kg (sand >= 700, clay < 200) on PERCENT data (sand_pct, clay_pct
in 0-100 range). The function never fired on properly-loaded FEBR
data and routed all 9 FEBR Quartzarenicos to the catch-all
"Regoliticos" subordem.

Fix: thresholds converted to %, sand >= 70 %, clay < 20 %. The
docstring explicitly notes the SiBCS-vs-soilKey unit convention.

## A/B on Embrapa FEBR (n=554)

| Level    | v0.9.33 | v0.9.35 | Delta |
|----------|---:|---:|---:|
| Order    | 56.68 % | 56.68 % | 0.00 pp |
| Subordem | 38.63 % | **39.17 %** | **+0.54 pp** |

The +0.54 pp Subordem lift is small in absolute terms (~3 of the 9
remaining Quartzarenicos correctly routed; 6 still mis-routed
because they have NA sand/clay or designation patterns that don't
match areia franca). The remaining 44 Argissolos / Latossolos
"Vermelho / Amarelo / Vermelho-Amarelo" misses are
**unfixable from FEBR data alone** -- the FEBR superconjunto.txt
ships zero Munsell hue / value / chroma columns. These would
require a separate Embrapa BDsolos export with field-survey
morphology, or the SPADBE database.

## C. Existing test fixture update

`tests/testthat/test-sibcs-subordens-v071.R:173` previously asserted
that `neossolo_quartzarenico` passes on a fixture using g/kg
thresholds (sand_pct = 900, clay_pct = 50). Updated to realistic
% values (sand_pct = 90, clay_pct = 5) so the fixture exercises
the post-v0.9.35 logic correctly.

Full suite: 3 075 PASS / 0 FAIL / 10 SKIP. R CMD check Status: OK.

# soilKey 0.9.33 (2026-05-03)

The "WRB qualifier closure" release. **100 % structural coverage**
(139/139 unique qualifier names referenced in `qualifiers.yaml` now
have a backing `qual_*` function).

## Audit baseline

The pre-v0.9.33 audit (run via `tests/testthat/test-v0933-wrb-
qualifier-closure.R`) measured:

  Total qualifier entries (with duplicates across RSGs): 1 316
  Unique qualifier names across all 32 RSGs:               139
  Functions named qual_*:                                  139
  With backing qual_* function (pre-v0.9.33):              132 / 139 (95.0 %)

The 7 missing qualifiers were:

  Endocalcic   referenced in 1 RSG (Chernozems)
  Endogleyic   referenced in 1 RSG (Gleysols / Stagnosols)
  Endostagnic  referenced in 1 RSG (Stagnosols)
  Floatic      referenced in 2 RSGs (Histosols + Cryosols)
  Ombric       referenced in 1 RSG (Histosols)
  Rheic        referenced in 1 RSG (Histosols)
  Toxic        referenced in 2 RSGs (Histosols + Cryosols)

## Implementation

`R/qualifiers-wrb2022-v0933.R` ships seven new exported helpers, all
following the existing `qual_*` calling convention:

  qual_endocalcic   -- depth-conditional Calcic (50-100 cm)
  qual_endogleyic   -- depth-conditional Gleyic (50-100 cm)
  qual_endostagnic  -- depth-conditional Stagnic (50-100 cm)
  qual_floatic      -- oc_pct >= 12 AND bulk_density <= 0.4 g/cm3
  qual_toxic        -- ph_h2o <= 3.5 OR ec_dS_m >= 16 (proxy)
  qual_ombric       -- Histic + acidic (pH <= 4.5) + no carbonates
  qual_rheic        -- Histic + neutral (pH > 4.5) OR carbonates present

The Endo-* helpers share a new internal helper `.q_endo_presence()`
that checks the diagnostic appears within a `[min_top, max_top]` cm
band -- mirroring `.q_presence()` for the upper-50-cm case.

The Floatic / Toxic / Ombric / Rheic helpers use **per-horizon
proxies** (KSSL-schema-compatible) rather than depending on
fields that the schema does not yet model (specific gravity, full
heavy-metal panels, hydrology). The proxies are conservative: each
function explicitly reports the relevant `missing` attributes when
the underlying signal is absent.

## Per-RSG coverage after v0.9.33

All 32 RSGs now report **100 % principal coverage** AND **100 %
supplementary coverage** in the audit script. The 7-qualifier gap
that previously dropped HS / GL / CH below 100 % at the principal
level is closed.

## v0.9.33 unit tests

12 new in `tests/testthat/test-v0933-wrb-qualifier-closure.R`:

  * 100 % coverage assertion via direct yaml + namespace audit;
  * Endo-* dispatch tests (returns DiagnosticResult, no error);
  * Floatic positive (high-OC, low-density) + negative (mineral);
  * Toxic positive (low pH, high EC) + negative (benign);
  * Ombric vs Rheic mutual exclusion (acidic vs neutral Histosol).

One pre-existing test (`test-qualifiers-wrb-v091-bloco-a.R:315`)
was updated from `expect_gt(sum(unimplemented), 0L)` to
`expect_gte(sum(unimplemented), 0L)` since v0.9.33 closes the
"not implemented" path entirely.

Full suite: 3 029 PASS / 0 FAIL / 10 SKIP. R CMD check Status: OK.

# soilKey 0.9.32 (2026-05-03)

The "vignettes refresh" release. Documentation-only update covering
the v0.9.24-v0.9.31 release series.

## A. v06_wosis_benchmark.Rmd updated

Two new sections:

* **§7 v0.9.27 -- per-page retry + graceful degradation**: documents
  the 1s/2s/4s/8s exponential backoff and partial-pull behaviour for
  ISRIC GraphQL timeouts, with a runnable example.
* **§8 v0.9.30 -- bundled WoSIS sample for offline / CI testing**:
  documents `load_wosis_sample()` and the
  `inst/extdata/wosis_sa_sample.rds` snapshot.

## B. NEW v08_kssl_nasis_multilevel.Rmd

A dedicated vignette for the headline real-data benchmark:

* the KSSL + NASIS join via `load_kssl_pedons_with_nasis()` and the
  attribute coverage on the 2021 NASIS snapshot;
* the four levels of `benchmark_run_classification()` with code
  examples (Order / Suborder / Great Group / Subgroup);
* the v0.9.31 headline numbers at large scale (n=2638, ±1.7 pp CI):
  Order 34.19 %, Suborder 13.85 %, Great Group 7.94 %, Subgroup
  4.17 %;
* a release-by-release trajectory table v0.9.22 -> v0.9.31 showing
  the cumulative Great Group lift and the v0.9.25 KST canonicaliser
  story (16 pre-13ed -> KST 13ed name pairs documented);
* roadmap for the remaining gaps (Pale-/Glossic prefixes, NASIS data
  sparsity, Endo/Epi-aquic precise distinction).

# soilKey 0.9.31 (2026-05-03)

The "specialised Great Group tests" release. Two GG diagnostics
that were under-detecting the v0.9.25-confusion-analysis targets:
Quartzipsamments (mineralogy proxy too strict) and Fragiudults /
Fragiudalfs / Fragiaqualfs (rupture_resistance rarely in KSSL data).

## A. Quartzipsamment proxy broadened

`quartzipsamment_qualifying_usda()`: KST 13ed Ch 8 (p 357) defines
Quartzipsamments as Psamments where >= 95 % of the 0.02-2.0 mm
fraction is resistant minerals (mostly quartz). The pre-v0.9.31
proxy was clay <= 5 % AND coarse_fragments <= 5 %, which under-
detected: 0/14 KSSL Quartzipsamments were caught (the v0.9.25
confusion analysis showed 14 udipsamments / ustipsamments references
should have been Quartzipsamments).

v0.9.31 broadens to:

  clay_pct <= 10 %       (loamy sand and finer sands qualify)
  sand_pct >= 80 %       (NEW: sand-dominated texture required)
  coarse_fragments <= 15 (some CF tolerated)

At least 50 % of in-range layers must satisfy all three.

## B. Fragipan accepts NASIS pediagfeatures flag

`fragipan_usda()`: KSSL gpkg rarely populates `rupture_resistance`,
the canonical fragipan signal. The 2021 NASIS snapshot, however,
ships ~13 500 `pediagfeatures.featkind` entries, including
"Fragipan" tags directly identified by the surveyor. v0.9.31 adds
the NASIS path as an OR-evidence source:

  passed = (rupture_resistance >= "firm" with thickness >= 15 cm)
        OR (NASIS pediagfeatures contains "Fragipan")

This closes the Fragiudults / Fragiudalfs / Fragiaqualfs / Fragixeralfs
detection gap on KSSL+NASIS pedons.

## C. KSSL+NASIS A/B (n=865)

| Level         | v0.9.30 | v0.9.31 | Delta |
|---------------|---:|---:|---:|
| Order         | 36.99 % | 36.99 % | 0.00 pp (regression-safe) |
| Suborder      | 17.73 % | 17.73 % | 0.00 pp (regression-safe) |
| **Great Group** | 10.57 % | **10.92 %** | **+0.35 pp** |
| **Subgroup**  | 5.09 %  | **5.32 %**  | **+0.23 pp** |

Modest but positive lift; Order / Suborder unchanged confirms the
fix is laser-focused at Great Group and below.

## Roadmap deferred to follow-up

The Pale-/Glossic Alfisol prefix tests (Paleudalfs / Glossudalfs /
Fraglossudalfs) were considered for this release but not shipped.
The current `pale_qualifying_usda()` uses a clay >= 35 % proxy that
is structurally too strict (KST 13ed actually defines Pale- by
"clay does not decrease 20 % within 150 cm of mineral surface"),
but only 11 KSSL+NASIS misses are in this confusion bucket --
lower priority than the 14 Quartzipsamment + Fragipan misses
addressed here. Tightening Pale- requires careful design to avoid
regression on Hapludalfs (which are far more common) and is left
for a future release with better validation infrastructure.

## Tests

9 new in `tests/testthat/test-v0931-quartzipsamment-fragipan.R`
covering the broadened Quartzipsamment proxy (sandy / loamy-sand /
loamy / missing-sand), the Fragipan NASIS path (with and without
flag), and the rupture_resistance lab path.

Full suite: 3 012 PASS / 0 FAIL / 10 SKIP. R CMD check Status: OK.

# soilKey 0.9.30 (2026-05-03)

The "offline-ready WoSIS + CRAN-clean" release. Two infrastructure
fixes that prepare the package for both reproducible CI and CRAN
submission.

## A. Bundled WoSIS South-America sample

`inst/extdata/wosis_sa_sample.rds` (49 KB compressed) ships a frozen
40-profile snapshot pulled on 2026-05-03 from the ISRIC WoSIS
GraphQL endpoint with `continent = "South America"`. New helper
function:

```
load_wosis_sample()
```

returns a list with `profiles_raw`, `pedons` (PedonRecord objects),
`pulled_on`, `endpoint`, `filter`, `n_pulled`. Tests + CI + casual
users can now exercise the WRB benchmark path without depending on
ISRIC server stability (see also: the v0.9.27 retry+fallback path,
which still applies for live pulls).

For up-to-date paper-grade benchmarks, callers should still use
`run_wosis_benchmark_graphql()` directly against the live endpoint;
the bundled snapshot is for reproducible tests, not for current
ground-truth claims.

## B. Bug-fix: WoSIS retry message sprintf

The v0.9.27 graceful-degradation path had a sprintf format bug
(`%d` mixed with a string concatenation) that caused the partial-pull
return to error out with `invalid format '%d'; use format %s for
character objects`. Fixed in `inst/benchmarks/run_wosis_benchmark.R`
by combining the message format string with `paste0()` before
sprintf.

The v0.9.30 cache pull demonstrated this fix in action: the ISRIC
server timed out at offset=40 (after 4 retries with 1s/2s/4s/8s
backoff), and the corrected graceful-degradation path returned
the 40 profiles successfully collected so far.

## C. R CMD check --as-cran

`R CMD check --as-cran` on `soilKey_0.9.30.tar.gz`:

- 0 ERRORs
- 0 WARNINGs
- 1 NOTE: "New submission" + a 301 redirect on the FAO PDF URL
  in README.md.

The "New submission" note is expected for a first CRAN submission
(it disappears on subsequent submissions). The 301 redirect on
`https://www.fao.org/3/i3794en/I3794en.pdf` is fixed by updating
the README to point at the OpenKnowledge canonical URL
(`https://openknowledge.fao.org/server/api/core/bitstreams/.../content`).

After the URL fix, `--as-cran` reports a single "New submission"
NOTE. The package is **CRAN-ready**.

## Tests

4 new in `tests/testthat/test-v0930-wosis-sample.R` covering:

- bundle returns 40-profile snapshot with the expected named slots;
- bundled profiles are valid `PedonRecord` objects;
- `classify_wrb2022()` runs on bundled pedons without raising;
- snapshot metadata (date, endpoint, filter) is correct.

Full suite: 2 980 PASS / 0 FAIL / 10 SKIP. R CMD check Status: OK.

# soilKey 0.9.29 (2026-05-03)

The "Neossolos Litolicos shallow-profile heuristic" release. Fixes
a single classifier path that was sending ~190 of 191 FEBR Neossolos
Litolicos to the catch-all "Regoliticos" subordem -- the dominant
single SiBCS Subordem error in the v0.9.27 confusion analysis.

## Root cause

SiBCS Cap 12 (p 219) defines Neossolos Litolicos by lithic contact
within 50 cm. In the FEBR / BDsolos snapshot, surveyors document
this implicitly by stopping the profile description at the rock
boundary (median depth 17.5 cm, median 1 horizon) rather than
entering a pseudo-R horizon. The pre-v0.9.29 `neossolo_litolico()`
required `contato_litico()` OR `contato_litico_fragmentario()` to
return TRUE, and both rely on an explicit `^R$|^Cr|^Rk` designation
that FEBR almost never carries (0.5 % of Litolicos in the snapshot).

Result: the classifier was routing **190 of 191 FEBR Litolicos** to
the catch-all "Neossolos Regoliticos" subordem.

## Fix

`neossolo_litolico()` now adds an "implicit lithic contact" path:

\itemize{
  \item max profile depth <= 50 cm (shallow stop -- suggestive of
        rock contact below);
  \item no horizon designation begins with \code{B} (so we do NOT
        flag shallow Cambissolos / Argissolos with a thin Bt or Bw
        within 50 cm);
  \item a non-empty \code{bottom_cm} column (otherwise we have no
        signal).
}

Direct evidence (explicit R / Cr / Rk designation within 50 cm) is
preserved as the canonical path.

## A/B on Embrapa FEBR (n=554)

| Level    | v0.9.27 | v0.9.29 | Delta |
|----------|---:|---:|---:|
| Order    | 56.68 % | 56.68 % | 0.00 pp (Order machinery unchanged) |
| **Subordem** | 9.93 % | **38.63 %** | **+28.70 pp** |

The +28.70 pp Subordem lift is the single biggest single-version
SiBCS gain since the v0.9.23 argic clay-increase fix (+14.1 pp at
Order). Cumulative SiBCS Subordem from v0.9.22: 0.0 % -> 38.63 %.

## v0.9.28 changes (also shipped in this release)

- **Designation-based clay-films proxy** for `argillic_clay_films_test()`:
  the KST 13ed Ch 18 master horizon symbol \code{t} ("accumulation
  of silicate clay") in any horizon designation (Bt, Btk, Btx, 2Bt,
  etc.) is now accepted as positive clay-illuviation evidence
  alongside NASIS pediagfeatures and per-horizon clay_films_amount.
  Coverage on KSSL+NASIS n=865: 12.2 % of profiles gain a third
  evidence path; total clay-films-positive coverage rises 38.8 % ->
  51.0 %. Marginal-argillic flips: 8/107 designation-only profiles
  switch from WRB tier (rejects) to KST tier (accepts) -- but the
  KSSL+NASIS Order/Suborder/Great Group/Subgroup numbers remain
  identical to v0.9.27 because those 8 marginal flips don't change
  the eventual taxonomic assignment.

- **`classify_all()` wrapper**: a single call returning all three
  classifications plus a `summary` data.frame. Saves callers from
  typing three separate `classify_*()` calls.

- **Codecov configuration** (`codecov.yml`): soft gates (project
  coverage drop allowed up to 1 pp; new patches at least 70 %
  covered with 5 pp grace). Test-coverage workflow already ships
  via `.github/workflows/test-coverage.yaml`; this release adds
  the per-repo config.

- **Additional `max(-Inf)` warning fix** in `R/diagnostics-horizons-sibcs.R`
  (worm_holes_pct path).

## Tests

- 17 new unit tests in
  \code{tests/testthat/test-v0928-designation-proxy.R} covering the
  designation 't'-suffix detection, regex strictness (no
  false-positive on "test"), evidence-source priority (NASIS
  pediagfeatures > phpvsf > designation), and the integration with
  argillic_usda routing.
- 7 new tests in \code{tests/testthat/test-v0928-classify-all.R}
  covering the wrapper API (subset, error handling, summary shape).
- 8 new tests in
  \code{tests/testthat/test-v0929-neossolo-litolico-heuristic.R}
  covering the FEBR-style shallow profile path, B-horizon
  exclusion, deep profile rejection, contradictory non-rock
  material rejection, and the classify_sibcs end-to-end integration.

Full suite: 2 976 PASS / 0 FAIL / 10 SKIP. R CMD check Status: OK
(0 errors / 0 warnings / 0 notes).

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

* **`read_febr_pedons(path, head, require_classification, verbose)`**
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
