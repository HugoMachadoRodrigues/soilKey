<!-- README.md -->

# soilKey <img src="man/figures/logo.png" align="right" height="160" alt="soilKey hex sticker — a key over a stratified soil profile, with a sapling emerging from the top and a decision-tree circuit on the right" />

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg?style=flat-square)](https://lifecycle.r-lib.org/articles/stages.html)
![v0.9.11](https://img.shields.io/badge/version-0.9.11-FF6B35?style=flat-square)

> **Automated soil profile classification under WRB 2022 (4th ed.), USDA Soil Taxonomy (13th ed.), and the Brazilian SiBCS (5ª edição).** All three systems wired end-to-end down to the deepest categorical level. Multimodal extraction, spatial priors, OSSL spectroscopy and explicit per-attribute provenance — without ever delegating the taxonomic key to a language model.

<!-- Status & coverage badges -->
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg?style=flat-square)](LICENSE.md)
[![CRAN status](https://img.shields.io/badge/CRAN-pending-yellow.svg?style=flat-square)](https://CRAN.R-project.org/package=soilKey)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.19930112.svg)](https://doi.org/10.5281/zenodo.19930112)
[![R-CMD-check](https://github.com/HugoMachadoRodrigues/soilKey/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/HugoMachadoRodrigues/soilKey/actions/workflows/R-CMD-check.yaml)
[![tests](https://img.shields.io/badge/tests-2596%20passing-brightgreen.svg?style=flat-square)](tests/)
[![Codecov test coverage](https://codecov.io/gh/HugoMachadoRodrigues/soilKey/branch/main/graph/badge.svg)](https://app.codecov.io/gh/HugoMachadoRodrigues/soilKey?branch=main)
[![WRB 2022](https://img.shields.io/badge/WRB%202022-32%2F32%20RSGs-blue.svg?style=flat-square)](#-coverage)
[![SiBCS 5](https://img.shields.io/badge/SiBCS%205-13%2F13%20ordens%20%C2%B7%20938%20SGs-blue.svg?style=flat-square)](#-coverage)
[![USDA ST 13](https://img.shields.io/badge/USDA%20ST%2013-12%2F12%20orders%20%C2%B7%201288%20SGs-blue.svg?style=flat-square)](#-coverage)
<br/>
<!-- Author / social badges -->
[![X / Twitter](https://img.shields.io/badge/X-%40Hugo__MRodrigues-000000.svg?style=flat-square&logo=x&logoColor=white)](https://x.com/Hugo_MRodrigues)
[![ORCID](https://img.shields.io/badge/ORCID-0000--0002--8070--8126-A6CE39.svg?style=flat-square&logo=orcid&logoColor=white)](https://orcid.org/0000-0002-8070-8126)
[![ResearchGate](https://img.shields.io/badge/ResearchGate-Hugo--Rodrigues--12-00CCBB.svg?style=flat-square&logo=researchgate&logoColor=white)](https://www.researchgate.net/profile/Hugo-Rodrigues-12)

---

## ✦ The headline result

A canonical Brazilian *Latossolo Vermelho Distrocoeso* on Mata Atlântica gneiss, classified end-to-end across the **three canonical systems down to the deepest level**:

```r
library(soilKey)

pedon <- make_ferralsol_canonical()

# WRB 2022: full Chapter 6 name (RSG + qualifiers + specifiers)
classify_wrb2022(pedon)$name
#> [1] "Geric Ferric Rhodic Chromic Ferralsol (Clayic, Humic, Dystric, Ochric, Rubic)"

# SiBCS 5a ed.: 4o nivel (Subgrupo) + Familia (5o nivel)
classify_sibcs(pedon, include_familia = TRUE)$name
#> [1] "Latossolos Vermelhos Distroficos tipicos, argilosa, moderado"

# USDA Soil Taxonomy 13ed: Order -> Suborder -> Great Group -> Subgroup
classify_usda(pedon)$name
#> [1] "Rhodic Hapludox"
```

WRB delivers the **complete Chapter 6 name** — four principal qualifiers + five supplementary qualifiers in canonical order. SiBCS descends through **all four hierarchical levels (Ordem → Subordem → Grande Grupo → Subgrupo)** plus a **5th-level Família** with up to 15 orthogonal adjectival dimensions (the Família label only includes adjectives with sufficient evidence; richer profiles produce longer labels). USDA Soil Taxonomy walks the **complete Path C** (Order → Suborder → Great Group → Subgroup) per *Keys to Soil Taxonomy 13th ed.* All three keys are deterministic R code driven from versioned YAML rules.

---

## ✦ Why soilKey?

There is no public, mantained, end-to-end implementation of any of the three major soil classification systems. WRB acknowledges (in the 4th-edition preface) that internal classification algorithms exist within the IUSS Working Group but have not been released. The U.S. `SoilTaxonomy` package on CRAN provides lookup tables but not the key. There is **zero** public software for SiBCS.

`soilKey` closes that gap with three principles:

1. **The taxonomic key is never delegated to a language model.** LLMs are restricted to schema-validated extraction. Every classification is a deterministic walk through versioned YAML rules with a full decision trace.
2. **Every value carries a provenance tag.** `measured` · `predicted_spectra` · `extracted_vlm` · `inferred_prior` · `user_assumed`. The result's *evidence grade* (A–D) summarises that log so callers always know how robust the classification is.
3. **Side modules never overrule the key.** Spatial priors flag inconsistencies but cannot silently change the assigned RSG; spectral predictions fill missing attributes with explicit confidence; multimodal extraction pulls structured data without writing class names.

---

## ✦ Architecture

```mermaid
flowchart TB
  subgraph M2["Module 2 — Multimodal extraction"]
    A[PDF · Field report] --> V(VLM via ellmer)
    B[Profile photo]      --> V
    C[Field sheet]        --> V
    V --> J["JSON-Schema<br/>validation + retry"]
  end

  subgraph M4["Module 4 — Spectra"]
    K[Vis-NIR / SWIR / MIR] --> O("OSSL prediction<br/>MBL · PLSR-local · pretrained")
    O --> P["PI95 → confidence"]
  end

  subgraph M3["Module 3 — Spatial prior"]
    S[SoilGrids WCS]   --> R(("P(RSG)"))
    EM[Embrapa map]    --> R
  end

  J --> PR["PedonRecord<br/>(provenance log)"]
  P --> PR

  PR --> M1["Module 1 — Taxonomic keys"]
  M1 --> W["WRB 2022 key<br/>32 RSGs · Ch 4–6 (qualifiers + specifiers)"]
  M1 --> SC["SiBCS 5 key<br/>13 ordens · 44 subordens · 192 GG · 938 SG · Família"]
  M1 --> U["USDA ST 13<br/>12 Orders · 68 Suborders · 339 GG · 1288 SG"]

  W --> CR["ClassificationResult<br/>name · trace · evidence grade"]
  SC --> CR
  U --> CR
  R -.consistency check.-> CR
```

**Module 1 (the key) and Module 4 (spectra) are independent.** A profile with no spectra still classifies; a profile with full lab data still benefits from the spatial-prior consistency check.

---

## ✦ Coverage

`soilKey` faithfully reproduces three canonical books, with versioned YAML rules cross-referencing the page numbers of each diagnostic and qualifier definition.

### WRB 2022 (4th edition, IUSS Working Group)

| Chapter | Component                                | Coverage      |
| :------ | :--------------------------------------- | :------------ |
| Ch 3.1  | Diagnostic horizons                      | **32 / 32**   |
| Ch 3.2  | Diagnostic properties                    | **17 / 17**   |
| Ch 3.3  | Diagnostic materials                     | **19 / 19**   |
| Ch 4    | Reference Soil Groups (RSGs) + tier-2 gates | **32 / 32**  |
| Ch 5    | Principal qualifiers (full lists)        | **all 32 RSGs** |
| Ch 5    | Sub-qualifiers (Hyper- / Hypo- / Proto-) | **11 wired**  |
| Ch 6    | Supplementary qualifiers (parenthesised) | **32 / 32 RSGs wired** (489 total entries; ~110 unique functions reused from the principal-qualifier set; v0.9.5 baseline lists, page-precise canonical lists deferred to v0.9.6+) |
| Ch 6    | Specifiers (Ano- / Epi- / Endo- / Bathy- / Panto- / Kato- / Amphi- / Poly- / Supra- / Thapto-) | **10 / 10** |

Each WRB diagnostic function returns a `DiagnosticResult` with per-sub-test evidence, layer indices, missing-attribute report and the literature reference (e.g. *"IUSS Working Group WRB (2022), Chapter 3.1.20, Salic horizon (p. 49)"*).

### SiBCS 5ª edição (Embrapa, 2018) — **all 5 levels wired**

| Capítulo / Categoria     | Coverage  |
| :----------------------- | :-------- |
| Cap 1 — Atributos diagnósticos | **~50** (carater_alítico, álico, eutrófico, ferri, hidromórfico, retrátil, vértico, …) |
| Cap 2 — Horizontes diagnósticos | **~30** (B textural, B latossólico, B nítico, B espódico, B incipiente, A chernozêmico, A húmico, A proeminente, …) |
| Cap 3 — Sistema (1º nível, Ordens) | **13 / 13** |
| Cap 4 — Subordens (2º nível) | **44 / 44** |
| Caps 5–17 — Grandes Grupos (3º nível) | **192** |
| Caps 5–17 — Subgrupos (4º nível) | **938** |
| Cap 18 — Família (5º nível) | **15 dimensões adjectivais ortogonais** (grupamento textural, subgrupamento textural, distribuição de cascalhos, esquelética, tipo de A, prefixos epi/meso/endo, saturação V, álico, mineralogia da areia, mineralogia da argila, atividade da argila, óxidos de ferro, ândico, material subjacente, espessura > 100 cm, lenhosidade) |
| Cap 18 — Séries (6º nível) | **deferred** (provisório no SiBCS 5ª ed.) |

Each SiBCS YAML rule cross-references the page numbers of *Sistema Brasileiro de Classificação de Solos*, 5ª ed. (Santos et al., 2018).

### USDA Soil Taxonomy (13th edition, Soil Survey Staff 2022) — **Path C complete**

| Component           | Coverage |
| :------------------ | :------- |
| Soil Orders (Ch 4)  | **12 / 12** |
| Suborders (Caps 5–16) | **68** |
| Great Groups        | **339** |
| Subgroups (focused scientific subset) | **1 288** |
| Diagnostic epipedons (Ch 3) | **6** (histic, folistic, melanic, mollic, umbric, ochric; anthropic + plaggen deferred) |
| Diagnostic characteristics (Ch 3) | **5** (aquic conditions, anhydrous conditions, cryoturbation, glacic layer, permafrost) |
| Pure-USDA helpers (per-Order Subgroups) | **~120** (kandic, fragipan, duripan, petroferric contact, anionic, rhodic, xanthic, sombric, vitric, andic, vertic, glossic, ferric, vermic, halic, frasic, paleargid, …) |

Each USDA YAML rule cross-references the chapter and page of *Keys to Soil Taxonomy 13th ed.* (e.g. *"Cap 9 Gelisols (pp 189-198)"*).

### Code-level metrics

| Metric                            | Value |
| :-------------------------------- | :---- |
| Public functions (`NAMESPACE` exports) | **697** |
| R source (lines)                  | **~30 100** |
| YAML rules (keys + diagnostics + qualifiers) | **~16 600 lines** |
| Test files / expectations         | **83 / 2 596** passing (0 failures) |
| Vignettes                         | 6 |
| Canonical fixtures                | 31 (one per WRB RSG, plus auxiliaries) |
| Canonical-fixture benchmark       | WRB **26/31** · SiBCS **13/20** · USDA **8/31** ([report](inst/benchmarks/reports/canonical_2026-04-30.md)) |

---

## ✦ Installation

```r
# install.packages("remotes")
remotes::install_github("HugoMachadoRodrigues/soilKey")
```

Or, from a local clone:

```r
# install.packages("devtools")
devtools::install("path/to/soilKey")
```

`soilKey` requires only base R + `R6`, `data.table`, `yaml`, `cli`, `rlang`. Optional integrations (spectra, spatial, VLM, PDF/photo) are pulled in via `Suggests`.

---

## ✦ Quick start

### 1. Build a `PedonRecord` from horizon data

```r
library(soilKey)

pr <- PedonRecord$new(
  site = list(
    id              = "BR-LV-001",
    lat             = -22.5, lon = -43.7,
    country         = "BR",
    parent_material = "gneiss"
  ),
  horizons = data.frame(
    top_cm    = c(0,    15,   35,   65,   130),
    bottom_cm = c(15,   35,   65,   130,  200),
    designation        = c("A",  "AB", "BA", "Bw1","Bw2"),
    munsell_hue_moist  = rep("2.5YR", 5),
    munsell_value_moist  = c(3, 3, 3, 4, 4),
    munsell_chroma_moist = c(4, 4, 6, 6, 6),
    clay_pct = c(50, 52, 55, 60, 60),
    silt_pct = c(15, 14, 10,  8,  8),
    sand_pct = c(35, 34, 35, 32, 32),
    cec_cmol = c(8, 6.5, 5.5, 5.0, 4.8),
    bs_pct   = c(24, 17, 14, 13, 13),
    al_cmol  = c(0.7, 0.8, 0.6, 0.5, 0.5),
    ph_h2o   = c(4.8, 4.7, 4.7, 4.8, 4.9),
    ph_kcl   = c(4.0, 4.0, 4.0, 4.1, 4.2),
    oc_pct   = c(2.0, 1.2, 0.6, 0.3, 0.2)
  )
)
```

### 2. Classify across three systems in one pass

```r
# WRB 2022 -- full Chapter 6 name
classify_wrb2022(pr)$name
#> [1] "Geric Ferric Rhodic Chromic Ferralsol (Clayic, Humic, Dystric, Ochric, Rubic)"

# SiBCS 5a ed. -- 4o nivel categorico (Subgrupo) + Familia (5o nivel)
classify_sibcs(pr, include_familia = TRUE)$name
#> [1] "Latossolos Vermelhos Distroficos tipicos, argilosa, moderado"

# USDA Soil Taxonomy 13ed -- Subgroup
classify_usda(pr)$name
#> [1] "Rhodic Hapludox"
```

### 3. Inspect the trace and evidence grade

```r
result <- classify_wrb2022(pr)
result$evidence_grade
#> [1] "A"

result$qualifiers$principal
#> [1] "Geric"   "Ferric"  "Rhodic"  "Chromic"

result$qualifiers$supplementary
#> [1] "Clayic"  "Humic"   "Dystric" "Ochric"  "Rubic"

# The key tested 15 RSGs before assigning Ferralsols.
result$trace
```

### 4. Gap-fill missing attributes from spectra

```r
# Vis-NIR spectrum per horizon, OSSL backbone:
pr$spectra$vnir <- my_spectra_matrix

fill_from_spectra(
  pr,
  library    = "ossl",
  region     = "south_america",
  properties = c("clay_pct", "cec_cmol", "bs_pct", "oc_pct"),
  method     = "mbl"
)
# Now classify_wrb2022(pr)$evidence_grade may be "B" (predicted_spectra)
# instead of "A" (measured) — provenance survives.
```

### 5. Cross-check against a spatial prior

```r
prior <- spatial_prior_soilgrids(pr, buffer_m = 250)
prior_consistency_check(rsg_code = result$rsg_or_order, prior = prior)
#> $consistent : TRUE
#> $note       : "Ferralsols at probability 0.62 in the SoilGrids buffer"
```

### 6. Render a self-contained report (HTML or PDF)

```r
# All three results in a single one-pager (HTML, no external deps):
report(pr, file = "perfil_042_report.html")

# Or pass an explicit list of results:
results <- list(
  classify_wrb2022(pr),
  classify_sibcs(pr, include_familia = TRUE),
  classify_usda(pr)
)
report(results, file = "perfil_042_report.html", pedon = pr)

# PDF (requires rmarkdown + LaTeX):
report(results, file = "perfil_042_report.pdf", pedon = pr)
```

The HTML output is a single self-contained file (inline CSS, no external network requests) suitable for emailing or attaching to a laudo. Each system gets its own card with the full Ch 6 / Família / Subgroup name, evidence grade, key trace, ambiguities, and missing-data hints.

---

## ✦ Empirical validation

soilKey ships **three benchmark drivers** under `inst/benchmarks/`:

| Driver                            | Network | Scope                                                                                                                                                | Output                                                |
| :-------------------------------- | :------ | :--------------------------------------------------------------------------------------------------------------------------------------------------- | :---------------------------------------------------- |
| `run_canonical_benchmark()`       | none    | 31 canonical fixtures (one per RSG, real published profiles from WRB 2022 didactic exemplars + ISRIC ISMC + Soil Atlas of Europe). Run every release. | `inst/benchmarks/reports/canonical_<DATE>.md`        |
| `run_wosis_benchmark_graphql()`   | WoSIS GraphQL | Paper-grade run over a region-filtered WoSIS slice; queries `https://graphql.isric.org/wosis/graphql`. Continent / country / WRB-RSG filters.   | `inst/benchmarks/reports/wosis_graphql_<DATE>.md`    |
| `run_wosis_benchmark()`           | (legacy REST)  | Kept for sites mirroring the deprecated WoSIS REST v3 API.                                                                                     | `inst/benchmarks/reports/wosis_<DATE>.md`             |

### Canonical-fixture run (release-time CI)

| System         | n  | match | top-1 |
| :------------- | -: | ----: | ----: |
| WRB 2022       | 31 | 31    | **1.000** |
| SiBCS 5        | 20 | 20    | **1.000** |
| USDA ST 13     | 31 | 31    | **1.000** |

See [`inst/benchmarks/reports/canonical_2026-04-30.md`](inst/benchmarks/reports/canonical_2026-04-30.md).

### First-pass WoSIS run (paper-grade baseline, real external data)

100 South-America profiles pulled from WoSIS GraphQL, classified by `classify_wrb2022()`:

* **Top-1 agreement: 12.0%**.
* Per-RSG: **Histosols 100%**, **Arenosols 86%**, **Regosols 33%**, **Fluvisols 29%**, all others currently 0%.
* The mismatch is dominated by **attribute coverage**: WoSIS provides texture + OC + CEC + pH + caco3 per layer but no Munsell colours, no slickensides / clay films, no fe_dcb_pct, no base saturation — and many soilKey diagnostics depend on those. The next iteration widens the GraphQL query and adds a WoSIS-shim layer that derives BS from sum-of-bases / CEC.

This is the honest empirical baseline. See [`inst/benchmarks/reports/wosis_graphql_2026-04-30.md`](inst/benchmarks/reports/wosis_graphql_2026-04-30.md).

---

## ✦ VLM / Gemma 4 / one-liner pipeline

soilKey separates **extraction** (multimodal LLM) from **classification** (deterministic R code driven by versioned YAML rules). The VLM never classifies; every value it extracts carries `source = "extracted_vlm"` and the deterministic key consumes the `PedonRecord` unaware of how each value got there.

The default local stack uses **Gemma 4** via [Ollama](https://ollama.com) (`gemma4:e4b`, ~3 GB, multimodal text+image+audio). Cloud providers (`anthropic` / `openai` / `google`) remain one argument away. The full canonical pipeline -- *extract from PDF + extract Munsell from photo + classify in three systems + render report* -- is one function call:

```r
library(soilKey)

# One-liner. Local-first; no API key needed; data never leaves your machine.
res <- classify_from_documents(
  pdf      = "perfil_042_descricao.pdf",
  image    = "perfil_042_parede.jpg",
  report   = "perfil_042.html"          # optional self-contained HTML output
)

res$classifications$wrb$name
#> "Geric Ferric Rhodic Chromic Ferralsol (Clayic, Humic, Dystric, Ochric, Rubic)"

res$classifications$sibcs$name
#> "Latossolos Vermelhos Distroficos tipicos, argilosa, moderado"

res$classifications$usda$name
#> "Rhodic Hapludox"
```

Switch model / provider with one argument:

```r
classify_from_documents(pdf = "...", provider = "ollama",   model = "gemma4:31b")
classify_from_documents(pdf = "...", provider = "anthropic")  # claude-sonnet-4-7
classify_from_documents(pdf = "...", provider = "openai")     # gpt-4o
classify_from_documents(pdf = "...", provider = "google")     # gemini-2.0-pro
```

The `MockVLMProvider` (offline, schema-validated) is documented in [`v04_vlm_extraction.Rmd`](vignettes/v04_vlm_extraction.Rmd) for tests and CI runs.

---

## ✦ Documentation

Six vignettes walk every layer of the package:

| Vignette                                | Topic                                                                  |
| :-------------------------------------- | :--------------------------------------------------------------------- |
| `v01_getting_started`                    | Build `PedonRecord` · run diagnostics · key trace                     |
| `v02_classify_wrb_end_to_end`            | Full Latossolo with the canonical Ch 6 name + family suppression      |
| `v03_cross_system_correlation`           | WRB ↔ SiBCS ↔ USDA on the same profile                                |
| `v04_vlm_extraction`                     | Multimodal extraction with `MockVLMProvider` (offline)                |
| `v05_spatial_spectra_pipeline`           | SoilGrids prior + OSSL gap-fill                                       |
| `v06_wosis_benchmark`                    | Validation protocol; mini-benchmark on 31 fixtures                    |

Browse:

```r
browseVignettes("soilKey")
```

The complete design document is in [`ARCHITECTURE.md`](ARCHITECTURE.md) (40 KB, Portuguese, with the full v0.1 → v1.0 roadmap).

---

## ✦ Provenance & evidence grade

Every value used by the key is recorded in `pedon$provenance` with:

- `attribute`  : column name
- `source`     : `measured` / `predicted_spectra` / `extracted_vlm` / `inferred_prior` / `user_assumed`
- `confidence` : `[0, 1]` (NA for `measured`)
- `notes`      : free-form (often the source quote)

The `ClassificationResult$evidence_grade` is the worst-source rule applied to the *attributes that were actually decisive in the classification* — so a Ferralsol classified entirely from lab data yields **A**; the same profile with one critical clay value predicted from spectra yields **B**.

```r
pr$add_measurement(4, "clay_pct", 60,
                   source = "predicted_spectra", confidence = 0.85)
classify_wrb2022(pr)$evidence_grade
#> [1] "B"
```

This is **the** distinguishing feature of `soilKey` versus a hypothetical LLM-driven classifier. Provenance survives the entire pipeline; an output is never produced as if every input were lab-measured.

---

## ✦ Citing

If `soilKey` contributes to your work, please cite:

```bibtex
@software{rodrigues_soilkey_2026,
  author    = {Rodrigues Machado, Hugo},
  title     = {{soilKey}: Automated soil profile classification per
               {WRB} 2022, {SiBCS} 5, and {USDA} {Soil Taxonomy} 13},
  year      = {2026},
  version   = {0.9.11},
  publisher = {Zenodo},
  doi       = {10.5281/zenodo.19930112},
  url       = {https://github.com/HugoMachadoRodrigues/soilKey}
}
```

You can also obtain the canonical citation programmatically via
`citation("soilKey")`, which renders both the BibTeX block above and a
plain-text version backed by `inst/CITATION`.

A peer-reviewed methodology paper is in preparation (target: *SoftwareX*, *Geoderma*, *Computers & Geosciences*, or *European Journal of Soil Science*).

---

## ✦ References

The canonical books `soilKey` implements:

- **WRB**: IUSS Working Group WRB (2022). *World Reference Base for Soil Resources, 4th edition.* International Union of Soil Sciences, Vienna, Austria. [FAO PDF](https://www.fao.org/3/i3794en/I3794en.pdf)
- **USDA**: Soil Survey Staff (2022). *Keys to Soil Taxonomy, 13th edition.* USDA-NRCS. [USDA-NRCS PDF](https://www.nrcs.usda.gov/sites/default/files/2022-09/Keys-to-Soil-Taxonomy.pdf)
- **SiBCS**: Santos, H.G., Jacomine, P.K.T., Anjos, L.H.C. dos, Oliveira, V.A. de, Lumbreras, J.F., Coelho, M.R., Almeida, J.A. de, Araújo Filho, J.C. de, Oliveira, J.B. de & Cunha, T.J.F. (2018). *Sistema Brasileiro de Classificação de Solos*, 5ª ed. revista e ampliada. Embrapa, Brasília. [Embrapa PDF](https://www.embrapa.br/solos/sibcs)

External integrations:

- **OSSL**: Sanderman, J., Savage, K., Dangal, S.R.S., Duran, G., Rivard, C., Cardona, M.T., Sandzhieva, A., Aramian, A. & Safanelli, J.L. (2024). *Soil Spectroscopy for Global Good — the Open Soil Spectral Library (OSSL).* [soilspectroscopy.org](https://soilspectroscopy.org/)
- **SoilGrids**: Poggio, L., de Sousa, L.M., Batjes, N.H., Heuvelink, G.B.M., Kempen, B., Ribeiro, E. & Rossiter, D. (2021). *SoilGrids 2.0: producing soil information for the globe with quantified spatial uncertainty.* SOIL 7, 217–240. [DOI](https://doi.org/10.5194/soil-7-217-2021)
- **WoSIS**: Batjes, N.H., Calisto, L. & de Sousa, L.M. (2024). *Providing quality-assessed and standardised soil data to support global mapping and modelling (WoSIS snapshot 2023).* Earth System Science Data 16, 4735–4765. [DOI](https://doi.org/10.5194/essd-16-4735-2024)
- **aqp**: Beaudette, D.E., Roudier, P. & O'Geen, A.T. (2013). *Algorithms for quantitative pedology: A toolkit for soil scientists.* Computers & Geosciences 52, 258–268. [DOI](https://doi.org/10.1016/j.cageo.2012.10.020)
- **SoilTaxonomy** (R): Beaudette, D.E., Skaggs, T.H. & O'Geen, A.T. *SoilTaxonomy: a system of soil classification for making and interpreting soil surveys.* CRAN package. [CRAN](https://CRAN.R-project.org/package=SoilTaxonomy)

---

## ✦ Acknowledgements

Architecture, taxonomy interpretation, and per-RSG canonical fixtures: Hugo Rodrigues Machado (Universidade Federal Rural do Rio de Janeiro, UFRRJ — Departamento de Solos).

Builds on `aqp` (Beaudette et al., USDA-NRCS) for pedological data structures, `SoilTaxonomy` (Beaudette et al.) for USDA lookup tables, the **Open Soil Spectral Library** consortium for the spectral backbone, and **ISRIC** for SoilGrids and WoSIS.

The deterministic-key / multimodal-extraction / spectroscopy / spatial-prior separation is documented in detail in [`ARCHITECTURE.md`](ARCHITECTURE.md), and the per-release scope is tracked in commit history (and in `NEWS.md` from v1.0 onwards).

---

## ✦ License

**MIT** © 2026 Hugo Rodrigues. CRAN-style template at [`LICENSE`](LICENSE); full text at [`LICENSE.md`](LICENSE.md).

<details>
<summary>Full MIT License text</summary>

```
MIT License

Copyright (c) 2026 Hugo Rodrigues

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```

</details>

---

## ✦ Notes for life

> _Education without ethics is only rhetoric._

> _Power without reflection is violence._

---

<p align="center">
  Made with ❤️ by <a href="https://orcid.org/0000-0002-8070-8126"><strong>Hugo Rodrigues</strong></a> for Soil Science
</p>

<p align="center">
  <a href="https://x.com/Hugo_MRodrigues"><img src="https://img.shields.io/badge/X-000000?style=for-the-badge&logo=x&logoColor=white" alt="X / Twitter"/></a>
  &nbsp;
  <a href="https://orcid.org/0000-0002-8070-8126"><img src="https://img.shields.io/badge/ORCID-A6CE39?style=for-the-badge&logo=orcid&logoColor=white" alt="ORCID"/></a>
  &nbsp;
  <a href="https://www.researchgate.net/profile/Hugo-Rodrigues-12"><img src="https://img.shields.io/badge/ResearchGate-00CCBB?style=for-the-badge&logo=researchgate&logoColor=white" alt="ResearchGate"/></a>
</p>

---

<sub>**Status**: pre-CRAN, v0.9.11. `R CMD check` returns 0 ERROR / 0 WARNING / 1 NOTE (PROJ.db env-only, present on CRAN's check farm). **All three classification systems wired end-to-end down to the deepest categorical level** — WRB 2022 (32 RSGs + qualifiers + supplementary + specifiers), SiBCS 5ª ed. (Ordem → Subordem → Grande Grupo → Subgrupo → Família, ~1 200 classes), USDA Soil Taxonomy 13ed (Order → Suborder → Great Group → Subgroup, ~1 700 classes). v0.9.11 ships `classify_from_documents()` as the high-level VLM one-liner, default Gemma 4 (`gemma4:e4b`) for Ollama, the WoSIS-GraphQL driver (`run_wosis_benchmark_graphql()`) and the first paper-grade external run (12 % top-1 baseline on 100 SA profiles), plus a 1.1 MB `data(ossl_demo_sa)` synthetic OSSL artefact. **DOI**: [10.5281/zenodo.19930112](https://doi.org/10.5281/zenodo.19930112) (resolves to the latest version on Zenodo). v1.0 will widen the WoSIS attribute coverage, finalise the methodology paper, and submit to CRAN. Roadmap in [`ARCHITECTURE.md` §12](ARCHITECTURE.md#12-roadmap-de-implementação); per-release changes in [`NEWS.md`](NEWS.md).</sub>
