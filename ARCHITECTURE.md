# soilKey — Package architecture

**An R package for automated classification of soil profiles under WRB
2022 (4th ed.) and SiBCS 5th ed., with multimodal extraction, spatial
priors (SoilGrids) and attribute prediction via spectroscopy (OSSL).**

> Migrated from `pedokey_architecture_1.md` (original draft) on
> 2026-04-25 after the package-name decision. This is the living
> architecture document — update it as decisions change throughout
> implementation.

------------------------------------------------------------------------

## 1. Name and identity

- **Name:** `soilKey`. Decided on 2026-04-25, after evaluating
  alternatives (`pedokey`, `pedorules`, `autopedo`).
- **License:** MIT (see `LICENSE` / `LICENSE.md`). Choice reconciled
  with `DESCRIPTION` on 2026-04-30. MIT (rather than GPL-3, considered
  in early drafts) maximises interoperability with academic and
  government pipelines — without copyleft virality that would limit
  embedded use in internal tools at Embrapa, FAO or IUSS-WRB.
- **Target audience:** pedologists, agronomists, environmental
  consultants, research institutions. No ML expertise required.

------------------------------------------------------------------------

## 2. Design principles

1.  **Strict separation of concerns.** Extraction (VLM), attribute
    inference (spectra, prior), and classification (key) are independent
    stages. **The key is never delegated to an LLM.** The LLM only
    serves to transform unstructured data into schema-validated
    structured data.
2.  **Full traceability.** Each assigned class carries the complete
    trace: which diagnostics were tested, which attributes were used,
    the origin of each attribute (measured / predicted / extracted /
    assumed), and the chain of decisions that led to the result.
3.  **Declarative where possible.** The key rules live in versioned
    YAML, not scattered code. This enables auditing by pedologists who
    do not program, and turns updates (e.g., a future WRB 5th ed.) into
    a matter of editing YAML + adjusting diagnostic functions.
4.  **Tests as scientific documentation.** Each diagnostic has fixtures
    based on published canonical profiles (didactic examples from WRB,
    RADAMBRASIL, Soil Atlas of Europe, WoSIS).
5.  **Interoperability with the existing ecosystem.** `PedonRecord`
    converts to/from
    [`aqp::SoilProfileCollection`](https://ncss-tech.github.io/aqp/reference/SoilProfileCollection-class.html);
    spectra follow the conventions of `prospectr`/`resemble`/`ossl`;
    spatial follows `terra`/`sf`.
6.  **Explicit grades of evidence.** The final result includes an
    `evidence_grade` (A/B/C/D) based on the provenance of the critical
    attributes. This is more important than an opaque numeric
    “confidence”.

------------------------------------------------------------------------

## 3. Dependencies

| Domain       | Packages                               |
|--------------|----------------------------------------|
| Pedology     | aqp, SoilTaxonomy, mpspline2           |
| Spatial      | terra, sf, gdalcubes                   |
| Spectra      | prospectr, resemble, ossl              |
| VLM/LLM      | ellmer, httr2, jsonlite, jsonvalidate  |
| Document I/O | pdftools, magick, tesseract (optional) |
| Core         | R6, cli, rlang, yaml, data.table       |
| Tests/docs   | testthat, roxygen2, rmarkdown, knitr   |

------------------------------------------------------------------------

## 4. Package structure

    soilKey/
    ├── DESCRIPTION
    ├── NAMESPACE
    ├── R/
    │   ├── soilKey-package.R
    │   ├── class-PedonRecord.R
    │   ├── class-DiagnosticResult.R
    │   ├── class-ClassificationResult.R
    │   │
    │   │   # ───── Module 1: Deterministic keys ─────
    │   ├── diagnostics-horizons-wrb.R      # argic(), ferralic(), mollic(), nitic()...
    │   ├── diagnostics-properties-wrb.R    # stagnic(), gleyic(), andic(), vertic()...
    │   ├── diagnostics-materials-wrb.R     # fluvic(), tephric(), sulfidic(), organic()...
    │   ├── diagnostics-horizons-sibcs.R    # B_latossolico(), B_textural(), B_nitico()...
    │   ├── key-wrb2022.R                   # classify_wrb2022()
    │   ├── key-sibcs.R                     # classify_sibcs()
    │   ├── qualifiers-wrb2022.R
    │   ├── subgroups-sibcs.R
    │   ├── rule-engine.R                   # consumes YAML
    │   │
    │   │   # ───── Module 2: VLM ─────
    │   ├── vlm-extract.R
    │   ├── vlm-providers.R                 # adapters via ellmer
    │   ├── vlm-schemas.R
    │   ├── vlm-prompts.R
    │   ├── vlm-validate.R
    │   │
    │   │   # ───── Module 3: Spatial prior ─────
    │   ├── spatial-prior.R
    │   ├── spatial-soilgrids.R
    │   ├── spatial-embrapa.R
    │   ├── spatial-combine.R
    │   │
    │   │   # ───── Module 4: Spectroscopy ─────
    │   ├── spectra-ossl.R
    │   ├── spectra-preprocess.R
    │   ├── spectra-predict-mbl.R
    │   ├── spectra-predict-pretrained.R
    │   ├── spectra-fill-attributes.R
    │   │
    │   │   # ───── Orchestration and reports ─────
    │   ├── classify.R                      # high-level wrapper
    │   ├── report-html.R
    │   ├── report-pdf.R
    │   └── utils-*.R
    │
    ├── inst/
    │   ├── rules/
    │   │   ├── wrb2022/
    │   │   │   ├── key.yaml                # ordering of the 32 RSGs
    │   │   │   ├── diagnostics.yaml        # diagnostic definitions
    │   │   │   └── qualifiers.yaml         # 202 qualifiers per RSG
    │   │   └── sibcs5/
    │   │       ├── key.yaml                # ordering of the 13 orders
    │   │       ├── diagnostics.yaml
    │   │       └── subgroups.yaml
    │   ├── prompts/
    │   │   ├── extract_horizons.md
    │   │   ├── extract_munsell_from_photo.md
    │   │   ├── extract_structure.md
    │   │   └── extract_site_metadata.md
    │   ├── schemas/
    │   │   ├── horizon.json
    │   │   ├── site.json
    │   │   └── pedon.json
    │   └── examples/
    │       ├── profiles_wrb/               # 32 canonical profiles, one per RSG
    │       └── profiles_sibcs/             # 13 canonical profiles, one per order
    │
    ├── data/                               # internal tables (.rda)
    │   ├── wrb_rsg_codes.rda
    │   ├── wrb_qualifier_codes.rda
    │   ├── sibcs_classes.rda
    │   └── munsell_lookup.rda
    │
    ├── data-raw/                           # reproducible scripts that generate data/
    │
    ├── tests/testthat/
    │   ├── test-diagnostics-wrb-argic.R
    │   ├── test-diagnostics-wrb-ferralic.R
    │   ├── test-diagnostics-wrb-mollic.R
    │   ├── ...                             # one test per diagnostic
    │   ├── test-key-wrb-ferralsol.R        # end-to-end per RSG
    │   ├── ...
    │   ├── test-key-sibcs-latossolo.R
    │   ├── ...
    │   ├── test-vlm-schema.R
    │   ├── test-spatial-soilgrids.R
    │   └── test-spectra-ossl.R
    │
    ├── vignettes/
    │   ├── 01-getting-started.Rmd
    │   ├── 02-wrb-classification.Rmd
    │   ├── 03-sibcs-classification.Rmd
    │   ├── 04-extraction-from-reports.Rmd
    │   ├── 05-spatial-prior.Rmd
    │   ├── 06-spectra-ossl.Rmd
    │   ├── 07-cross-system-correlation.Rmd
    │   ├── 08-custom-rules.Rmd
    │   └── 09-wosis-benchmark.Rmd
    │
    └── README.md

------------------------------------------------------------------------

## 5. Data model

### 5.1 `PedonRecord` (R6 class)

The central structure carries everything about a profile — site,
horizons, spectra, images, documents, and the provenance of each
attribute.

``` r

PedonRecord <- R6::R6Class("PedonRecord",
  public = list(

    # ---- data ----
    site = NULL,        # list:
                        #   lat, lon, crs (default 4326), date, country,
                        #   elevation_m, slope_pct, aspect_deg,
                        #   landform, parent_material,
                        #   land_use, vegetation, drainage_class

    horizons = NULL,    # data.table with columns (fixed schema):
                        #   top_cm, bottom_cm, designation,
                        #   boundary_distinctness, boundary_topography,
                        #   munsell_hue_moist, munsell_value_moist, munsell_chroma_moist,
                        #   munsell_hue_dry, munsell_value_dry, munsell_chroma_dry,
                        #   structure_grade, structure_size, structure_type,
                        #   consistence_moist, consistence_wet, clay_films,
                        #   coarse_fragments_pct,
                        #   clay_pct, silt_pct, sand_pct,
                        #   ph_h2o, ph_kcl, ph_cacl2,
                        #   oc_pct, n_total_pct,
                        #   cec_cmol, ecec_cmol, bs_pct, al_sat_pct,
                        #   ca_cmol, mg_cmol, k_cmol, na_cmol, al_cmol,
                        #   caco3_pct, caso4_pct,
                        #   fe_dcb_pct, fe_ox_pct, al_ox_pct, si_ox_pct,
                        #   bulk_density_g_cm3, water_content_33kpa, water_content_1500kpa

    spectra = NULL,     # list:
                        #   vnir = matrix (rows = horizons, cols = 350:2500 nm),
                        #   mir  = matrix (rows = horizons, cols = 4000:600 cm^-1),
                        #   metadata = list(instrument, date, preprocess_applied)

    images = NULL,      # list of:
                        #   list(type="profile_wall", path=..., reference_card=TRUE, annotations=...)

    documents = NULL,   # list of:
                        #   list(type="field_sheet"|"lab_report"|"survey_report", path=..., text=...)

    # ---- provenance ----
    provenance = NULL,  # data.table: horizon_idx, attribute, source, confidence, notes
                        # source ∈ {measured, extracted_vlm, predicted_spectra,
                        #           inferred_prior, user_assumed}

    # ---- methods ----
    initialize = function(site = NULL, horizons = NULL,
                          spectra = NULL, images = NULL, documents = NULL) { ... },
    validate   = function() { ... },   # check sanity: top<bottom, %sand+silt+clay≈100,
                                        # pH in plausible range, CEC >= sum of bases, etc.
    to_aqp     = function() { ... },   # coerce to SoilProfileCollection
    from_aqp   = function(spc) { ... },
    add_measurement = function(horizon_idx, attribute, value, source, confidence = 1.0) { ... },
    summary    = function() { ... },
    print      = function() { ... }
  )
)
```

### 5.2 `DiagnosticResult`

Returned by every diagnostic function. Never just `TRUE`/`FALSE` — it
always carries evidence.

``` r

DiagnosticResult <- R6::R6Class("DiagnosticResult",
  public = list(
    name      = NULL,   # "argic"
    passed    = NULL,   # TRUE/FALSE/NA (NA = missing attributes prevented test)
    layers    = NULL,   # which horizons (indices) satisfy
    evidence  = NULL,   # list with sub-tests and their results
    missing   = NULL,   # which attributes would be needed and are absent
    reference = NULL,   # literature citation
    notes     = NULL
  )
)
```

### 5.3 `ClassificationResult`

Final output of
[`classify_wrb2022()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_wrb2022.md)
or
[`classify_sibcs()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_sibcs.md).

``` r

ClassificationResult <- R6::R6Class("ClassificationResult",
  public = list(
    system         = NULL,   # "WRB 2022" | "SiBCS 5"
    name           = NULL,   # "Rhodic Ferralsol (Clayic, Humic, Dystric)"
    rsg_or_order   = NULL,   # "Ferralsols"
    qualifiers     = NULL,   # ordered list principal + supplementary
    trace          = NULL,   # list of DiagnosticResult in order of testing
    ambiguities    = NULL,   # alternative RSGs that nearly passed (with delta)
    missing_data   = NULL,   # attributes whose measurement would refine the result
    evidence_grade = NULL,   # A/B/C/D
    prior_check    = NULL,   # consistency with spatial prior
    warnings       = NULL,

    print   = function() { ... },
    summary = function() { ... },
    report  = function(file, format = c("html","pdf","md")) { ... }
  )
)
```

**Evidence grades:**

| Grade | Criterion |
|----|----|
| A | All critical attributes come from the laboratory; diagnostics passed unambiguously. |
| B | Critical attributes measured; secondary attributes predicted from spectra with narrow PI95%. |
| C | Some critical attributes predicted from spectra; spatial prior consistent with the result. |
| D | Multiple critical attributes extracted by VLM or inferred from prior; tentative result. |

------------------------------------------------------------------------

## 6. Module 1 — Deterministic keys

### 6.1 Diagnostics as pure functions

Each diagnostic is a function that takes a `PedonRecord` and returns a
`DiagnosticResult`. Example:

``` r

#' Argic horizon (WRB 2022)
#'
#' Tests whether any horizon or combination meets the argic horizon criteria
#' per Chapter 3 of WRB 2022 (IUSS Working Group WRB, 2022).
#'
#' @param pedon A PedonRecord.
#' @param min_thickness Minimum thickness in cm (default 7.5).
#' @return A DiagnosticResult.
#' @references IUSS Working Group WRB (2022), Chapter 3, Argic horizon.
#' @export
argic <- function(pedon, min_thickness = 7.5) {

  h <- pedon$horizons
  tests <- list()

  # Test 1: qualified textural increment (one of three conditions)
  tests$clay_increase <- test_clay_increase_argic(h)

  # Test 2: minimum thickness of the candidate argic horizon
  tests$thickness <- test_minimum_thickness(h, min_thickness,
                                             candidate_layers = tests$clay_increase$layers)

  # Test 3: minimum texture (cannot be sand)
  tests$texture <- test_texture_argic(h, candidate_layers = tests$clay_increase$layers)

  # Test 4: exclusions (albeluvic glossic -> Retisol instead of Luvisol/Acrisol)
  tests$not_albeluvic <- test_not_albeluvic(h)

  # Required attributes that are missing
  missing <- attributes_missing_for_tests(tests, h)

  all_ok <- all(vapply(tests, function(t) isTRUE(t$passed), logical(1)))

  DiagnosticResult$new(
    name      = "argic",
    passed    = if (length(missing) > 0) NA else all_ok,
    layers    = if (isTRUE(all_ok)) tests$clay_increase$layers else integer(0),
    evidence  = tests,
    missing   = missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3, Argic horizon"
  )
}
```

Sub-tests (`test_clay_increase_argic` etc.) live in
`R/utils-diagnostic-tests.R` and are also exported — pedologists can
call them in isolation for auditing.

### 6.2 The WRB 2022 key in YAML

The testing order of the 32 RSGs is explicit in
`inst/rules/wrb2022/key.yaml`:

``` yaml
version: "WRB 2022 (4th edition)"
source: "IUSS Working Group WRB (2022). World Reference Base for Soil Resources.
        International soil classification system for naming soils and creating
        legends for soil maps. 4th edition. IUSS, Vienna."
checksum: "sha256:..."  # verifiable against the edition

# Canonical key order; excludes_previous is implicit (if not satisfied here,
# falls through to the next RSG).

rsgs:
  - code: HS
    name: Histosols
    tests:
      any_of:
        - organic_material: {thickness_gte: 10, from_surface: true}
        - organic_material: {thickness_gte: 40, within_top_cm: 80, cumulative: true}

  - code: AT
    name: Anthrosols
    tests:
      any_of:
        - hortic_horizon: {}
        - irragric_horizon: {}
        - plaggic_horizon: {}
        - pretic_horizon: {}
        - terric_horizon: {}

  - code: TC
    name: Technosols
    tests:
      any_of:
        - artefacts: {volume_pct_gte: 20, within_top_cm: 100}
        - geomembrane: {within_top_cm: 100}
        - technic_hard_material: {depth_lte: 5, coverage_pct_gte: 95}

  - code: CR
    name: Cryosols
    tests:
      all_of:
        - cryic_conditions: {within_top_cm: 100}

  - code: LP
    name: Leptosols
    tests:
      any_of:
        - continuous_rock: {within_cm: 25}
        - coarse_fragments: {pct_gte: 90, within_top_cm: 75}

  # ... (continues through the 32 RSGs in order: SN, VR, SC, GL, AN, PZ, PT, NT, FR,
  #      PL, ST, CH, KS, PH, UM, DU, GY, CL, RT, AC, LX, AL, LV, CM, AR, FL, RG)

  - code: RG
    name: Regosols
    tests:
      default: true   # catch-all — all mineral soils that did not key out earlier
```

### 6.3 The rule engine

`R/rule-engine.R` reads the YAML, resolves the references to diagnostic
functions, and executes:

``` r

run_wrb_key <- function(pedon, rules = NULL) {
  rules <- rules %||% load_rules("wrb2022")
  trace <- list()

  for (rsg in rules$rsgs) {
    result <- evaluate_rsg_tests(pedon, rsg$tests)
    trace[[rsg$code]] <- list(
      code = rsg$code, name = rsg$name,
      passed = result$passed,
      evidence = result$evidence,
      missing = result$missing
    )
    if (isTRUE(result$passed)) {
      return(list(assigned = rsg, trace = trace))
    }
    # NA (insufficient attributes) — annotate but continue; the final result
    # flags the ambiguity.
  }
  # default: Regosols (catch-all)
  list(assigned = rules$rsgs[[length(rules$rsgs)]], trace = trace)
}

evaluate_rsg_tests <- function(pedon, tests) {
  if (!is.null(tests$all_of)) {
    results <- lapply(tests$all_of, run_single_test, pedon = pedon)
    passed <- all(vapply(results, `[[`, logical(1), "passed"))
  } else if (!is.null(tests$any_of)) {
    results <- lapply(tests$any_of, run_single_test, pedon = pedon)
    passed <- any(vapply(results, `[[`, logical(1), "passed"))
  } else if (isTRUE(tests$default)) {
    return(list(passed = TRUE, evidence = list(), missing = character(0)))
  }
  missing <- unique(unlist(lapply(results, `[[`, "missing")))
  list(passed = passed, evidence = results, missing = missing)
}
```

### 6.4 Qualifiers (WRB)

After the RSG is assigned, `qualifiers-wrb2022.R` runs the ~202
qualifiers, filtering those available for that RSG (Chapter 4 of WRB
2022). Each qualifier is also a pure function with the same pattern.
Principal qualifiers are ordered as specified; supplementary qualifiers
are listed alphabetically.

Final name output: `"Rhodic Ferralsol (Clayic, Humic, Dystric)"`.

#### 6.4.1 Canonical inventory for v0.9 (WRB 2022 Ch 5, pp 127-156)

The full code table is in **Ch 6 (pp 152-153)** and contains 4 groups:

| Group | Content | n |
|----|----|----|
| **Reference Soil Groups** | 32 RSGs with their 2-letter codes (HS, AT, …, RG) | 32 |
| **Qualifiers** | Principal and supplementary qualifiers + sub-qualifiers (Hyper-, Hypo-, Proto-, Neo-, Orto-, Endoab-, Subaq-, etc.) | ~190 |
| **Specifiers** | Depth/position modifiers: `..a` Ano- (≤50 cm), `..n` Endo- (50-100 cm), `..p` Epi-, `..k` Kato- (lower part), `..e` Panto- (entire profile), `..y` Poly-, `..d` Bathy- (\>100 cm), `..s` Supra- (above a barrier), `..b` Thapto- (in buried soil), `..m` Amphi- | 10 |
| **Novic combinations** | New material over a buried soil, e.g. `nva` Aeoli-Novic, `nvf` Fluvi-Novic | 5+ |

**Implementation strategy for v0.9:**

Most qualifiers reduce to 4 structural patterns:

1.  **“Having X horizon at depth ≤ Y cm”** (\>= 60% of qualifiers):
    `Calcic`, `Petrocalcic`, `Argic` (already indirect via RSG),
    `Spodic`, `Histic`, `Plinthic`, `Andic`, etc. Implementation: thin
    wrapper `qualifier_has_horizon(horizon_diagnostic_fn, max_top_cm)`.
2.  **“Having layer with Z properties at depth ≤ Y cm”** (~25%):
    `Stagnic`, `Gleyic`, `Vitric`, `Vertic`, `Sodic`, `Reducing`, etc.
    Analogous wrapper with `property_diagnostic_fn`.
3.  **“Material gating”** (~10%): `Calcaric`, `Dolomitic`, `Tephric`,
    `Fluvic`, `Sulfidic` etc. Wrapper over `material_diagnostic_fn`.
4.  **“Specific chemical/physical composition”** (~5%): `Dystric`,
    `Eutric`, `Hyperdystric`, `Hypereutric` (BS thresholds), `Magnesic`,
    `Hyperalic`, `Geric`, `Pellic`, `Rubic`, `Xanthic` (Munsell),
    `Vermic` (worm features), `Skeletic` (coarse fragments), etc.
    Implemented case by case.

Sub-qualifiers (`Hyper-`, `Hypo-`, `Proto-`, `Neo-`) are parametric
variants of the base qualifier — they fit naturally as suffixes in the
same YAML file, with modified thresholds.

**Specifiers** apply compositionally over qualifiers (e.g. `Epileptic` =
leptic in the 0-50 cm band; `Endoleptic` = leptic in the 50-100 cm band)
— implemented via decorator.

**Proposed YAML schema** for `inst/rules/wrb2022/qualifiers.yaml`:

``` yaml
qualifiers:
  - code: ap
    name: Abruptic
    type: principal
    applicable_rsgs: [PL, ST, LV, AC, LX, AL, RT]  # from Ch 4
    test:
      diagnostic: abrupt_textural_difference
      max_top_cm: 100
    references: "Ch 5, p 127"
    sub_qualifiers:
      - code: go
        name: Geoabruptic
        condition: "abrupt_textural_difference NOT associated with argic/natric/spodic upper limit"
```

This format makes expanding the 200+ qualifiers a matter of generating
YAML programmatically from the canonical text (which I already have
indexed).

**Risks for v0.9:** - Several qualifiers require Ch 3 diagnostics not
yet implemented (chernic, hortic, irragric, plaggic, pretic, terric,
anthraquic, hydragric, hortic, panpaic, etc.). v0.9 must include those
~12 missing diagnostics before resolving qualifiers. - The order of
principal qualifiers in the soil name matters: WRB lists principal
qualifiers “according to the list from top to bottom” of Ch 4 — i.e.,
the order listed per RSG is canonical and must be preserved in the YAML.

### 6.5 SiBCS (parallel key, 13 orders)

Structurally identical implementation, with system-specific diagnostics.
Example — Latossolo:

``` r

#' Horizonte B latossólico (SiBCS)
#'
#' @references SiBCS 5ª ed. (Embrapa, 2018), Capítulo ...
#' @export
B_latossolico <- function(pedon) {
  h <- pedon$horizons
  tests <- list()
  tests$minimum_thickness <- test_thickness(h, min_cm = 50)
  tests$ki_lte_22 <- test_ki(h, max = 2.2)
  tests$kr_lte_17 <- test_kr(h, max = 1.7)
  tests$cec_by_clay <- test_cec_by_clay(h, max_cmol_per_kg_clay = 17)
  tests$no_textural_gradient <- test_no_textural_gradient_b_latossolico(h)
  tests$no_clay_films <- test_no_clay_films(h)
  # ...
  DiagnosticResult$new(
    name = "B latossólico",
    passed = all(vapply(tests, `[[`, logical(1), "passed")),
    layers = tests$minimum_thickness$layers,
    evidence = tests,
    missing = attributes_missing_for_tests(tests, h),
    reference = "SiBCS 5ª ed. (Embrapa)"
  )
}
```

The SiBCS key (`inst/rules/sibcs5/key.yaml`) tests in the order:
Latossolos, Argissolos, Neossolos, Plintossolos, Chernossolos,
Vertissolos, Gleissolos, Nitossolos, Cambissolos, Espodossolos,
Luvissolos, Organossolos, Planossolos (with adjustments per the 5th
edition).

The vignette **07-cross-system-correlation.Rmd** classifies the same
profile in both systems and documents known correspondence patterns
(Latossolo ↔︎ Ferralsol, Argissolo ↔︎ Acrisol/Lixisol/Alisol/Luvisol
depending on CEC and BS%, Nitossolo Vermelho eutrófico ↔︎ Rhodic Nitisol,
etc.).

------------------------------------------------------------------------

## 7. Module 2 — Multimodal extraction (VLM)

### 7.1 Operating principle

The VLM **never classifies**. It does three things:

1.  Transforms textual descriptions (survey PDFs, field sheets, reports)
    into structured data validated against a JSON schema.
2.  Extracts Munsell colour from photos when a reference card is
    present.
3.  Approximates horizon boundaries in profile photos (with explicitly
    low-flagged confidence).

Every extracted value receives (a) a provenance tag `extracted_vlm`, (b)
a confidence score provided by the model, (c) a textual citation of the
source where applicable.

### 7.2 Provider abstraction via `ellmer`

``` r

vlm_provider <- function(name = c("anthropic", "openai", "google", "ollama"),
                         model = NULL, ...) {
  name <- match.arg(name)
  model <- model %||% default_model(name)
  switch(name,
    anthropic = ellmer::chat_anthropic(model = model, ...),
    openai    = ellmer::chat_openai(model = model, ...),
    google    = ellmer::chat_gemini(model = model, ...),
    ollama    = ellmer::chat_ollama(model = model, ...)
  )
}
```

This gives you the important option: **run everything locally via
Ollama** (Gemma 3, Qwen2-VL, LLaVA) to preserve institutional
independence and protect sensitive data. Particularly relevant given the
Rosa do Deserto documentation context — all extraction can run on
infrastructure you control.

### 7.3 Extraction tasks

``` r

extract_horizons_from_pdf(pedon, pdf_path,  provider)
extract_munsell_from_photo(pedon, image,    provider)
extract_structure_from_photo(pedon, image,  provider)
extract_site_from_fieldsheet(pedon, image,  provider)
```

Each function: 1. Reads the document/image. 2. Loads a prompt template
(`inst/prompts/`) and a JSON schema (`inst/schemas/`). 3. Calls the
provider with `type = "json"` enforced. 4. Validates the return with
`jsonvalidate`. 5. If validation fails, re-runs appending the error to
the prompt (up to 3 attempts). 6. Reconciles with existing data in the
`PedonRecord`: never overwrites `measured` values; the VLM only fills in
`NA` or fields with lower-authority provenance, unless
`overwrite = TRUE`.

### 7.4 Example prompt (horizon extraction)

``` markdown
# inst/prompts/extract_horizons.md

You are a pedologist extracting structured data from a soil profile description
document.

Extract every horizon of the profile with the following fields. For each field,
also provide:
- a confidence score (0.0 to 1.0),
- a short textual citation (<= 20 words) from the document supporting the
  extraction.

If a field is absent, return null. DO NOT INFER OR GUESS.

Return valid JSON conforming to this schema:
{schema_json}

Source document:
---
{document_text}
---
```

### 7.5 Horizon schema (excerpt)

``` json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "properties": {
    "horizons": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "top_cm": {"type": ["number", "null"]},
          "bottom_cm": {"type": ["number", "null"]},
          "designation": {"type": ["string", "null"], "pattern": "^[A-Z][a-zA-Z0-9]*$"},
          "munsell_moist": {
            "type": ["object", "null"],
            "properties": {
              "hue": {"type": "string"},
              "value": {"type": "integer", "minimum": 2, "maximum": 8},
              "chroma": {"type": "integer", "minimum": 1, "maximum": 8},
              "confidence": {"type": "number", "minimum": 0, "maximum": 1},
              "source_quote": {"type": "string"}
            }
          },
          "clay_pct": {
            "type": ["object", "null"],
            "properties": {
              "value": {"type": "number", "minimum": 0, "maximum": 100},
              "confidence": {"type": "number"},
              "source_quote": {"type": "string"}
            }
          }
        },
        "required": ["top_cm", "bottom_cm"]
      }
    }
  },
  "required": ["horizons"]
}
```

### 7.6 Profile photo: realistic limits

The SoilNet paper (Feldmann et al., 2025) shows that generic VLMs do not
perform horizon classification well — it requires a specialised model
with sequential segmentation and hierarchical embeddings. Conservative
position for `soilKey`:

- **Munsell from photo with reference card**: testable, accepted with
  moderate `confidence`.
- **Horizon boundaries from photo**: extracted as a *proposal*, always
  with `confidence <= 0.4`, flagged for human review.
- **Quantitative attributes (clay %, CEC, etc.) from photo**: **never**.
  These come from the laboratory or from Module 4.

------------------------------------------------------------------------

## 8. Module 3 — Spatial prior

### 8.1 SoilGrids via WCS

ISRIC publishes SoilGrids as Cloud-Optimized GeoTIFFs accessible via
WCS/WMS. For WRB, SoilGrids directly provides P(RSG \| pixel) for the
principal RSGs.

``` r

spatial_prior_soilgrids <- function(pedon,
                                     system  = c("wrb2022", "usda"),
                                     buffer_m = 250,
                                     source_url = soilgrids_wcs_url()) {

  coord <- sf::st_sfc(sf::st_point(c(pedon$site$lon, pedon$site$lat)), crs = 4326)
  coord_utm <- sf::st_transform(coord, utm_crs_for(pedon$site))
  buf <- sf::st_buffer(coord_utm, dist = buffer_m)

  rst <- terra::rast(source_url)
  vals <- terra::extract(rst, terra::vect(buf), fun = mean, na.rm = TRUE)

  # Normalise to a probability over the RSGs
  probs <- normalize_rsg_probs(vals, system = system)
  data.table::data.table(rsg_code = names(probs), probability = probs)
}
```

### 8.2 Use of the prior

The prior does not enter the deterministic key (which remains the
authority). It is used in three situations:

1.  **Tie-breaking under ambiguity**: when two RSGs nearly pass with
    missing attributes, the prior orders the candidates.
2.  **Sanity check**: if the final classification has P_prior \< 0.01 in
    the region, a `warning` is emitted in the `ClassificationResult`.
    Example: classifying as Cryosol in Gainesville triggers an immediate
    alert.
3.  **Optional Bayesian posterior**: a separate
    [`posterior_classify()`](https://hugomachadorodrigues.github.io/soilKey/reference/posterior_classify.md)
    function for when the user explicitly wants a probabilistic rather
    than deterministic result.

### 8.3 For Brazil / SiBCS

The Mapa de Solos do Brasil 1:5,000,000 (Embrapa 2021, with style
available for QGIS) can be reprojected as a raster of SiBCS classes. It
does not natively provide probabilities, but an approximation via a
neighbourhood window (15×15 cells) gives an approximate P based on local
frequencies.

------------------------------------------------------------------------

## 9. Module 4 — Spectroscopy via OSSL

### 9.1 Context

The Open Soil Spectral Library (OSSL), maintained by the Soil
Spectroscopy for Global Good consortium (Woodwell Climate Research
Center, OpenGeoHub, ISRIC), provides \>100k spectra paired with
laboratory data in the Vis-NIR (350–2500 nm) and MIR (4000–600 cm⁻¹)
ranges, under an open licence. R infrastructure is documented at
`https://soilspectroscopy.github.io/ossl-manual/` with workflows for
PLSR, Cubist and memory-based learning.

This is terrain where you have a direct comparative advantage — Module 4
is essentially a pedagogical formalisation of what you already do with
soilVAE and Vis-NIR/SWIR pipelines.

### 9.2 Flow

``` r

fill_from_spectra <- function(pedon,
                              library    = "ossl",
                              region     = c("global", "south_america",
                                             "north_america", "europe", "africa"),
                              properties = c("clay_pct", "sand_pct", "silt_pct",
                                             "cec_cmol", "bs_pct", "ph_h2o",
                                             "oc_pct", "caco3_pct", "fe_dcb_pct"),
                              method     = c("pretrained", "mbl", "plsr_local"),
                              preprocess = "snv+sg1",
                              k_neighbors = 100,
                              overwrite  = FALSE) {

  region <- match.arg(region); method <- match.arg(method)

  # 1. Pre-processing of the user's spectrum
  X <- preprocess_spectra(pedon$spectra$vnir, method = preprocess)

  # 2. Prediction
  preds <- switch(method,
    pretrained  = predict_ossl_pretrained(X, properties = properties, region = region),
    mbl         = predict_ossl_mbl(X, properties = properties, region = region,
                                   k = k_neighbors),
    plsr_local  = predict_ossl_plsr_local(X, properties = properties, region = region,
                                           k = k_neighbors)
  )
  # preds: data.table with columns property, value, pi95_low, pi95_high, n_neighbors

  # 3. Sanity-checked merge
  for (i in seq_len(nrow(preds))) {
    r <- preds[i]
    horizon_idx <- r$horizon_idx
    if (r$property %in% names(pedon$horizons) &&
        (is.na(pedon$horizons[[r$property]][horizon_idx]) || overwrite)) {
      pedon$horizons[[r$property]][horizon_idx] <- r$value
      pedon$add_measurement(horizon_idx = horizon_idx,
                            attribute = r$property,
                            value = r$value,
                            source = "predicted_spectra",
                            confidence = pi_to_confidence(r$pi95_low, r$pi95_high))
    }
  }
  pedon
}
```

### 9.3 Memory-based learning (recommended)

MBL via
[`resemble::mbl`](https://l-ramirez-lopez.github.io/resemble/reference/mbl.html)
tends to outperform global PLSR on heterogeneous libraries such as OSSL,
and integrates naturally with regional filtering (South America subset
for Brazilian profiles). Suggested defaults: k = 100 neighbours,
PLS-scores distance, 5 local components, internal LOO validation.

### 9.4 Interaction with Module 1

The WRB/SiBCS key has critical quantitative attributes: % clay (for the
textural gradient of argic / B textural), CEC per 1 kg of clay (for
Ferralsol/Latossolo, Nitisol), base saturation (Acrisol vs. Lixisol
vs. Luvisol), % Fe DCB (plinthic, ferralic).

If those attributes come from spectral prediction with narrow PI95%, the
`evidence_grade` drops from A to B. If the PI95% is wide enough to cross
a critical diagnostic threshold (e.g., predicted CEC = 15 ± 6 cmol/kg
with a threshold of 16), the system reports the ambiguity in
`ClassificationResult$ambiguities` and suggests a specific lab
measurement in `missing_data`.

### 9.5 Biogeographic sanity

Predictions outside plausible ranges for the biome (cross-checking
SoilGrids climate data + WoSIS data summarised by biome) raise a
`warning`. Example: a CaCO₃ prediction = 12% in a humid Atlantic Forest
profile is almost certainly an error of the spectral model or a problem
in acquisition.

------------------------------------------------------------------------

## 10. Orchestration — full flow

``` r

library(soilKey)

# 1. Build record
pedon <- PedonRecord$new(
  site = list(lat = -22.5, lon = -43.7, date = "2024-03-10",
              country = "BR", parent_material = "gneiss",
              elevation_m = 180, slope_pct = 8),
  documents = "perfil_042_descricao.pdf",
  spectra   = list(vnir = vnir_matrix),   # matrix 6 horizons × 2151 bands
  images    = "perfil_042_parede.jpg"
)

# 2. Extract from the PDF and the photo
pedon <- pedon |>
  extract_horizons_from_pdf(
    provider = vlm_provider("ollama", model = "gemma3:27b")
  ) |>
  extract_munsell_from_photo(
    provider = vlm_provider("ollama", model = "gemma3:27b")
  )

pedon$validate()

# 3. Fill gaps with spectra via OSSL
pedon <- fill_from_spectra(
  pedon,
  method     = "mbl",
  region     = "south_america",
  properties = c("clay_pct", "silt_pct", "sand_pct",
                 "cec_cmol", "bs_pct", "ph_h2o", "oc_pct", "fe_dcb_pct")
)

# 4. Spatial prior
prior <- spatial_prior(pedon, source = "soilgrids", system = "wrb2022")

# 5. Classify in both systems
res_wrb   <- classify_wrb2022(pedon, prior = prior, on_missing = "warn")
res_sibcs <- classify_sibcs(pedon)

# 6. Inspect
print(res_wrb)
# ClassificationResult (WRB 2022)
#   Name: Rhodic Ferralsol (Clayic, Humic, Dystric)
#   Evidence grade: B (clay %, CEC predicted by spectra; pi95 narrow)
#   Prior check: consistent (P_prior = 0.41)
#   Ambiguities: none
#   Missing data to refine: none
#
# Key trace (abbreviated):
#   HS (Histosols)     — failed (no organic material >= 10 cm)
#   AT (Anthrosols)    — failed (no anthric horizons)
#   ...
#   FR (Ferralsols)    — PASSED (ferralic horizon, 25–180 cm; CEC 12 cmol/kg clay)

# 7. Report
report(list(res_wrb, res_sibcs), file = "perfil_042_report.html")
```

------------------------------------------------------------------------

## 11. Scientific validation strategy

### 11.1 Canonical fixtures

`inst/examples/profiles_wrb/` contains **one profile per RSG** (32 in
total) extracted from: - Didactic examples from WRB 2022 itself (Annex
of Schad 2023, Table 1). - *Soil Atlas of Europe* (JRC, 2005). - ISRIC’s
soil monolith collection (re-classified to WRB 2022).

`inst/examples/profiles_sibcs/` contains **one profile per order** (13)
extracted from: - RADAMBRASIL (volumes available digitally). - Embrapa
Solos technical bulletins. - SiBCS 5th ed. reference profiles.

### 11.2 Tests

Each diagnostic has a dedicated test file:

``` r

# tests/testthat/test-diagnostics-wrb-argic.R
test_that("argic passes on canonical Luvisol profile", {
  pedon <- readRDS(test_path("fixtures", "luvisol_canonical.rds"))
  res <- argic(pedon)
  expect_true(res$passed)
  expect_gte(length(res$layers), 1)
})

test_that("argic fails on Ferralsol", {
  pedon <- readRDS(test_path("fixtures", "ferralsol_canonical.rds"))
  res <- argic(pedon)
  expect_false(isTRUE(res$passed))
})

test_that("argic returns NA when clay data missing", {
  pedon <- readRDS(test_path("fixtures", "no_clay_data.rds"))
  res <- argic(pedon)
  expect_true(is.na(res$passed))
  expect_true(length(res$missing) > 0)
})

test_that("argic discriminates from natric (ESP)", {
  pedon <- readRDS(test_path("fixtures", "solonetz_with_clay_increase.rds"))
  res_argic <- argic(pedon)
  res_natric <- natric(pedon)
  expect_false(isTRUE(res_argic$passed))  # excluded by ESP
  expect_true(res_natric$passed)
})
```

### 11.3 WoSIS benchmark (vignette 09)

Run `soilKey` on the WoSIS subset that has an original WRB
classification and compute: - Exact agreement rate (same RSG). - Partial
agreement rate (same “guide” RSG per the correspondence tables in Schad
2023). - Confusion matrix per RSG. - Systematic patterns of disagreement
— these provide useful feedback to the IUSS WRB Working Group and are
direct material for the methodological paper.

This benchmark is the empirical core of the paper (candidates:
*SoftwareX*, *Geoderma*, *Computers & Geosciences*, *European Journal of
Soil Science*).

------------------------------------------------------------------------

## 12. Implementation roadmap

| Version | Scope | Status |
|----|----|----|
| v0.1 | Skeleton, core classes, 3 WRB diagnostics (argic, ferralic, mollic), Ferralsols end-to-end. | **shipped (commit `613c3f2`)** |
| v0.2 | +8 WRB diagnostics (calcic, gypsic, salic, cambic, plinthic, spodic, gleyic_properties, vertic_properties); +7 RSG-derived diagnostics (acrisol, lixisol, alisol, luvisol, chernozem, kastanozem, phaeozem); 16/32 RSGs wired into the key; Module 5 (USDA) and Module 6 (SiBCS) scaffolds. | **shipped (5 commits, `8077adb`…`8a7d6d9`)** |
| v0.3 | +15 WRB diagnostics (histic, leptic, arenic, umbric, duric, technic, andic, fluvic, natric, nitic, planic, stagnic, retic, cryic, anthric); **WRB key 32/32 RSGs wired end-to-end**; 31 canonical fixtures; gleyic/stagnic refinement via decay pattern; nitic/ferralic exclusion. | **shipped (3 commits, v0.3a-c)** |
| **v0.3.1** | **Tier-1 corrections against the canonical text of WRB 2022 Ch 3.1**: argic 6/1.4/20 + 50 band, ferralic removes ECEC, duric 10/10, vertic ≥25 cm, salic alkaline + product. | **shipped (`a63925b`)** |
| **v0.3.2** | **RSG ordering fix**: PL/ST between PT and NT (canonical pp 95-126); FL before AR. The 31 fixtures still classify correctly. | **shipped (`27d8f14`)** |
| **v0.4** | **Module 4 — OSSL integration**: [`predict_ossl_mbl()`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_ossl_mbl.md), [`predict_ossl_plsr_local()`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_ossl_plsr_local.md), [`predict_ossl_pretrained()`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_ossl_pretrained.md), [`preprocess_spectra()`](https://hugomachadorodrigues.github.io/soilKey/reference/preprocess_spectra.md) (SNV / SG1), [`pi_to_confidence()`](https://hugomachadorodrigues.github.io/soilKey/reference/pi_to_confidence.md), [`fill_from_spectra()`](https://hugomachadorodrigues.github.io/soilKey/reference/fill_from_spectra.md). The provenance tag `predicted_spectra` downgrades evidence_grade A→B. | **shipped (`381c3ce`)** |
| **v0.5** | **Module 3 — SoilGrids/Embrapa prior**: `spatial_soilgrids_prior()` (WCS), `spatial_embrapa_prior()`, [`prior_consistency_check()`](https://hugomachadorodrigues.github.io/soilKey/reference/prior_consistency_check.md). Wired into [`classify_wrb2022()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_wrb2022.md) via `prior` and `prior_threshold`. **The deterministic key is never overwritten by the prior.** | **shipped (`98e524e`)** |
| **v0.6** | **Module 2 — VLM extraction via ellmer**: [`extract_horizons_from_pdf()`](https://hugomachadorodrigues.github.io/soilKey/reference/extract_horizons_from_pdf.md), [`extract_munsell_from_photo()`](https://hugomachadorodrigues.github.io/soilKey/reference/extract_munsell_from_photo.md), [`extract_site_from_fieldsheet()`](https://hugomachadorodrigues.github.io/soilKey/reference/extract_site_from_fieldsheet.md). Schema validation via jsonvalidate (draft-07). `MockVLMProvider` exported for tests. NSE bug-fix in `PedonRecord$add_measurement`. | **shipped (`80735f6`)** |
| **v0.3.3** | **Full coverage of WRB Ch 3.1 / 3.2 / 3.3**: +18 horizons (albic, ferric, petrocalcic/petroduric/petrogypsic/petroplinthic/pisoplinthic, vertic_horizon, thionic, fragic, sombric, chernic, anthraquic, hydragric, hortic, irragric, plaggic, pretic, terric); +12 properties (abrupt_textural_difference, albeluvic_glossae, continuous_rock, lithic_discontinuity, protocalcic_properties, protogypsic_properties, reducing_conditions, shrink_swell_cracks, sideralic_properties, takyric_properties, vitric_properties, yermic_properties); +16 materials (aeolic, artefacts, calcaric, claric, dolomitic, gypsiric, hypersulfidic, hyposulfidic, limnic, mineral, mulmic, organic, organotechnic, ornithogenic, soil_organic_carbon, solimovic, technic_hard, tephric). Schema +24 columns. | **shipped (`aa4afd7`)** |
| **v0.3.4** | **Tier-2 gate strengthening WRB Ch 4**: 7 strict RSG-level diagnostics – vertisol (cracks), andosol (andic OR vitric, exclusions), gleysol (multi-path), planosol (abrupt + stagnic + reducing), ferralsol (argic-exclusion with WDC/dpH/SOC paths), chernozem_strict (chernic + protocalcic + BS), kastanozem_strict (mollic + carbonate \<= 70 cm + BS). Includes refined spodic to avoid conflict with andic. VR/AN/CH fixtures enriched with cracks_width_cm, volcanic_glass_pct, worm_holes_pct. | **shipped (`a402606`)** |
| **v0.3.5** | **Final coverage of Ch 3.1 – 32/32 horizons**: +tsitelic (Caucasian red), +panpaic (buried-soil pattern), +limonic (meadow redox), +protovertic (vertic-spectrum lower bound, mutually exclusive with strict vertic_horizon). | **shipped** |
| **v0.7** | **Module 6 SiBCS 5ª ed. (Embrapa, 2018) implemented in full**: +17 diagnostic attributes (atividade_argila_alta, eutrofico, carater_alitico, carater_carbonatico, carater_redoxico, …); +24 diagnostic horizons (horizonte_histico, A_chernozemico/humico/proeminente, B_textural/latossolico/incipiente/nitico/espodico/planico, plintico, glei, fragipa, duripa, …); +13 RSG-level diagnostics following the canonical key in Ch 4 (organossolo, neossolo, vertissolo, …, argissolo); +13 canonical fixtures + 30 tests; key.yaml rewritten in canonical order O-R-V-E-S-G-L-M-C-F-T-N-P. | **shipped (`929b4dd`)** |
| v0.8 | **Module 5 — parallel USDA Soil Taxonomy** (12 orders + suborders + great groups + USDA diagnostics). Requires “Keys to Soil Taxonomy” (USDA-NRCS, 12th ed. 2014). | 3–4 weeks |
| v0.9 | **Full WRB qualifiers** — ~202 qualifiers + 10 specifiers (Ano-, Bathy-, Endo-, Epi-, Kato-, Panto-, Poly-, Supra-, Thapto-, Amphi-) + sub-qualifiers (Hyper-, Hypo-, Proto-, Neo-, Orto-). See inventory in §6.4. Vignettes 05–09; WoSIS benchmark. | 1 month |
| **v0.9.1 (Block A)** | **Canonical Ch 4 principal-qualifier coverage** for the first 5 RSGs of the key (HS / AT / TC / CR / LP). +42 `qual_*` functions in `R/qualifiers-wrb2022-v091.R` (Calcaric, Dolomitic, Gypsiric, Tephric, Limnic, Solimovic, Ornithic, Sulfidic, Mulmic; Hortic, Irragric, Plaggic, Pretic, Terric, Hydragric, Anthraquic; Technic, Hyperartefactic, Urbic, Spolic, Garbic, Ekranic, Linic; Yermic, Takyric, Glacic, Turbic; Lithic, Nudilithic, Hyperskeletic, Rendzic, Vermic; Petric, Thionic, Sapric/Hemic/Fibric mutually exclusive via thickness dominance, Drainic, Subaquatic, Tidalic, Reductic, Organotechnic). YAML expanded with canonical Ch 4 lists for Block A; AT fixture enriched with `p_mehlich3_mg_kg`. Tests: +87 expectations. | **shipped** |
| **v0.9.1 (Block B)** | **Coverage for SN / VR / SC / GL / AN** (saline / clay-rich / wet / volcanic). +14 `qual_*` functions in `R/qualifiers-wrb2022-v091b.R`: Chernic, Pisoplinthic, Abruptic, Aceric; Mazic, Grumic, Pellic (surface structure/colour of Vertisols); Aluandic vs Silandic (Al/Si split of the active andic component via the molar ratio Al/(Al+0.5\*Si)), Hydric (water retention at 1500 kPa), Melanic (dark high-OC andic), Acroxic (ECEC \<= 2 cmol+/kg in andic layer), Pachic (mollic/umbric \>= 50 cm), Eutrosilic (silandic + BS \>= 50%). Canonical Ch 4 YAML for the 5 RSGs (SN 22 principals, VR 23, SC 22, GL 32, AN 31). AN fixture enriched with `si_ox_pct` and `water_content_1500kpa`. **v0.9.1.A refinement**: `qual_plaggic` gained an anthropogenic-evidence gate (P \>= 50 mg/kg, artefacts \>= 0.5%, or designation `Apl/Aplg`) to avoid false positives from the v0.3.3 diagnostic on any thick A horizon. Tests: +103 expectations. | **shipped** |
| **v0.9.1 (Block C)** | **Coverage for PZ / PT / PL / ST / NT / FR** – *Brazilian / tropical block*: SiBCS Latossolos, Argissolos and Espodossolos live here as Ferralsols / Acrisols / Lixisols / Podzols. +14 `qual_*` functions in `R/qualifiers-wrb2022-v091c.R`: **spodic family** (Hyperspodic, Carbic = humus-dominated, Rustic = Fe-dominated, Ortsteinic, Placic \<= 25 mm, Densic \>= 1.8 g/cm³); **very-low-CEC family of highly weathered tropical soils** (Geric ECEC \<= 1.5 cmol+/kg or ()pH \> 0; Vetic CEC \<= 6 cmol+/kg clay; Posic ()pH \> 0; Hyperdystric BS \< 5%; Hypereutric BS \>= 80%; Hyperalic Al sat \>= 50% in argic); Hyperalbic (contiguous albic \>= 100 cm with eluvial-evidence guard); Sombric (with spodic / ferralic exclusion). **v0.3.3 bug-fix**: [`sombric()`](https://hugomachadorodrigues.github.io/soilKey/reference/sombric.md) was calling `test_top_at_or_above(min_top_cm=...)` with an invalid argument name – corrected to use a direct depth filter. Canonical Ch 4 YAML for the 6 RSGs (PZ 23 principals, PT 24, PL 30, ST 29, NT 23, FR 30). The canonical FR now classifies as **Geric Ferric Rhodic Chromic Ferralsol** – the full WRB name of a Latossolo Distrocoeso. Tests: +111 expectations. | **shipped** |
| **v0.9.1 (Block D + E)** | **Coverage for the remaining 16 RSGs** – closure of the 32 / 32 WRB 2022 RSGs. Block D: CH, KS, PH, UM, DU, GY, CL, RT (steppe / arid / cold-humid). Block E: AC, LX, AL, LV, CM, AR, RG, FL (argic-rich / cambic / sandy / alluvial). +4 `qual_*` functions in `R/qualifiers-wrb2022-v091de.R`: Cutanic (visible clay films in argic, hallmark of Argissolos / Luvisols), Glossic (mollic with albic glossae, Chernozems / Phaeozems of the steppe-forest transition), Brunic (cambic-only B in Arenosol – excludes argic/spodic/ferralic/nitic), Protic (no B horizon at all). Canonical Ch 4 YAML for the 16 RSGs (CH 21, KS 21, PH 21, UM 15, DU 18, GY 18, CL 18, RT 18, AC 24, LX 23, AL 19, LV 21, CM 29, AR 24, RG 24, FL 25). Canonical resolutions: AC/LX/LV → “Albic Cutanic ”, AL → “Hyperalic Albic Cutanic Alisol”, CM → “Eutric Cambisol”, AR → “Protic Albic Arenosol”, FL → “Haplic Fluvisol”, CH → “Vermic Chernic Cambic Chernozem”. Tests: +115 expectations. **Milestone**: complete RSG-level coverage of WRB 2022 Ch 4 – 32 / 32 RSGs with canonical principal lists. | **shipped** |
| **v0.9.2.A** | **Hyper- / Hypo- / Proto- sub-qualifiers + family suppression**. +11 functions (Hypersalic / Hyposalic; Hypersodic / Hyposodic; Hypercalcic / Hypocalcic / Protocalcic; Hypergypsic / Hypogypsic / Protogypsic; Protovertic). The engine gains `.suppress_qualifier_siblings()`: when several members of a family pass (e.g. Calcic + Hypocalcic + Protocalcic), only the most specific appears in the resolved name (per WRB Ch 6). Families: salinity, sodicity, calcic, gypsic, vertic, albic, skeletic, eutric, dystric, alic. YAML expanded: SN/VR/SC/CH/KS/PH/DU/GY/CL gain the Hyper-/Hypo-/Proto- variants in canonical positions. Tests: +52 expectations. | **shipped** |
| **v0.9.2.B** | **Specifier infrastructure** (Ano- / Epi- / Endo- / Bathy- / Panto- via prefix dispatch in the resolver). `.detect_specifier()` recognises the prefix, `.apply_specifier()` calls the base `qual_*` and filters layers by depth band. No need to define one function per (specifier × base) – the system is generic. CH gains Endogleyic / Endostagnic / Endocalcic in canonical positions. Specifiers Kato- / Poly- / Supra- / Thapto- / Amphi- deferred to v0.9.3 (require buried-horizon flags / chains of designations). Tests: +55 expectations. | **shipped** |
| **v0.9.2.C** | **v0.3.x diagnostic corrections** – false-positive reduction. **cambic** gains a depth gate (`min_top_cm = 5`) and a structural-development gate (`structure_grade ∈ {weak, moderate, strong}` AND `structure_type ∉ {massive, single grain}`); A horizons and massive-C no longer pass. **plaggic** gains an anthropogenic-evidence gate directly in the diagnostic (P \>= 50 mg/kg OR artefacts \> 0 OR designation Apl/Aplg/Apk); the v0.9.1 gate in `qual_plaggic` was removed (now direct delegation). **sombric** gains a humus-illuviation gate (the candidate layer must have OC ≥ OC_layer_above + 0.1%); the v0.3.3 permissiveness is eliminated. Resulting canonical classification change: DU → “Duric Skeletic Durisol” (loses Cambic from the massive BC1). FR (Latossolo) and the other 30 fixtures unchanged. Tests: +43 expectations. | **shipped** |
| **v0.9.3.A** | **Remaining specifiers (Kato/Amphi/Poly/Supra/Thapto) + supplementary engine**. Refactor of `.wrb_specifiers` to support two `kind`s – `depth` (simple depth band; reuses the v0.9.2.B path) and `filter` (custom function). Helpers: `.kato_filter` (top_cm \>= 50), `.amphi_filter` (Epi AND Endo), `.poly_filter` (\>= 2 disjoint runs), `.supra_filter` (above a barrier: continuous_rock / petric / technic_hard), `.thapto_filter` (designation ending in `b`). Engine: `resolve_wrb_qualifiers` now also processes the `supplementary:` slot of the YAML, returning `principal` + `supplementary` with families suppressed in both. `classify_wrb2022` renders the full WRB Ch 6 name with parenthesised tags. Tests: +66 expectations. | **shipped** |
| **v0.9.3.B** | **Supplementary qualifier seed** – 5 new functions (Aric: Ap designation; Cumulic: recent fluvic/aeolic/cumulic-style cover; Profondic: argic continues \>= 150 cm; Rubic: hue \<= 5YR + chroma \>= 4 less strict than Rhodic; Lamellic: Bt lamellae via designation proxy) in `R/qualifiers-wrb2022-v093b.R`. YAML expanded with canonical `supplementary:` slot for FR / AC / LX / AL / LV / CM / NT. **Canonical result of the Brazilian Latossolo**: FR now classifies as **“Geric Ferric Rhodic Chromic Ferralsol (Clayic, Humic, Dystric, Ochric, Rubic)”** – the full WRB Ch 6 name with principal and supplementary qualifiers. Same structure for AC/LX/LV/AL/NT/CM (Brazilian Argissolos / Cambissolos / Nitossolos). Tests: +51 expectations. | **shipped** |
| **v0.9.4** | **Vignettes 02-06 + WoSIS benchmark**. Five new vignettes: `02-classify-wrb-end-to-end` (Latossolo end-to-end with full Ch 6 name); `03-cross-system-correlation` (same profile resolved in WRB / SiBCS / USDA, with correspondence table); `04-vlm-extraction` (multimodal extraction via Module 2 with `MockVLMProvider`, schema validation, retry loop, provenance); `05-spatial-spectra-pipeline` (Module 3 SoilGrids prior + Module 4 OSSL gap-fill with synthetic fixtures for offline reproducibility); `06-wosis-benchmark` (validation protocol against WoSIS, mini-benchmark online on the 31 canonical fixtures, instructions for the paper-grade run). Driver script `inst/benchmarks/run_wosis_benchmark.R` for the full pipeline (read_wosis_profiles + build_pedon_from_wosis + run_wosis_benchmark) producing versioned reports in `inst/benchmarks/reports/wosis_<DATE>.md`. All vignettes build without error via [`rmarkdown::render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html) – none require network or external data for the default path. | **shipped** |
| v1.0 | CRAN submission, methodological paper submitted (SoftwareX / Geoderma / C&G / EJSS). | \+ 1 month review |

Total estimate: **9–14 months** to v1.0 with part-time dedication.

### Factual state on 2026-04-27 (after `929b4dd` – v0.7 SiBCS shipped)

- **20 commits** on `main`. All 4 modules of the original scope
  implemented (Module 1 WRB key, Module 2 VLM, Module 3 spatial prior,
  Module 4 OSSL). v0.3.3 closed WRB coverage of Ch 3.1/3.2/3.3, v0.3.4
  brought strict RSG-level gates, v0.3.5 closed the 32 WRB horizons.
  **v0.7 delivers the full Module 6 (SiBCS 5ª ed.)**: 17 diagnostic
  attributes + 24 horizons + 13 RSG-level orders wired into the
  canonical key of Ch 4.
- **368 test_that blocks / 830 expectations** passing. **1 deliberate
  skip** (a test that requires ellmer installed).
- **WRB 2022 + SiBCS 5ª ed. both with first-level key complete**: 32/32
  WRB RSGs + 13/13 SiBCS orders, in the canonical order of each system.
  31 WRB fixtures + 13 SiBCS fixtures, all classifying to the intended
  class.
- **WRB 2022 key 32/32 RSGs in canonical order** with strict Tier-2
  gates for VR/AN/GL/PL/FR/CH/KS. 31 canonical fixtures, each
  classifying to the intended RSG.
- **Coverage of Ch 3.1 (diagnostic horizons):** **32 of 32 horizons
  implemented** (full coverage). Includes all petric variants
  (petrocalcic, petroduric, petrogypsic, petroplinthic), the
  anthropogenic family (anthraquic, hydragric, hortic, irragric,
  plaggic, pretic, terric), vertic_horizon (strict), thionic, fragic,
  sombric, chernic, and the four historic niches of v0.3.5: tsitelic
  (red Caucasian), panpaic (buried), limonic (meadow redox), protovertic
  (vertic-spectrum lower bound).
- **Coverage of Ch 3.2 (properties):** **17 of 17 properties
  implemented** (full coverage): abrupt_textural_difference,
  albeluvic_glossae, andic_properties, anthric_properties (via
  anthric_horizons), continuous_rock, gleyic_properties,
  lithic_discontinuity, protocalcic_properties, protogypsic_properties,
  reducing_conditions, retic_properties, shrink_swell_cracks,
  sideralic_properties, stagnic_properties, takyric_properties,
  vitric_properties, yermic_properties. Plus cryic_conditions,
  planic_features, leptic_features (which do not belong to Ch 3.2 but
  are operational properties).
- **Coverage of Ch 3.3 (materials):** **19 of 19 materials implemented**
  (full coverage): aeolic, artefacts, calcaric, claric, dolomitic,
  fluvic, gypsiric, hypersulfidic, hyposulfidic, limnic, mineral,
  mulmic, organic, organotechnic, ornithogenic, soil_organic_carbon,
  solimovic, technic_hard, tephric. Histic_horizon (Ch 3.1) also serves
  as a gate for organic_material.
- **Expanded horizon schema** (24 new columns in v0.3.3):
  cementation_class, p_mehlich3_mg_kg, worm_holes_pct,
  water_dispersible_clay_pct, sulfidic_s_pct, volcanic_glass_pct,
  phosphate_retention_pct, artefacts_industrial_pct,
  artefacts_urbic_pct, rock_origin, permafrost_temp_C, cracks_width_cm,
  cracks_depth_cm, polygonal_cracks_spacing_cm, desert_pavement_pct,
  varnish_pct, ventifact_pct, vesicular_pores, rupture_resistance,
  plasticity, al_kcl_cmol, layer_origin.

### Module 5 — USDA Soil Taxonomy (v0.8)

Added to scope on 2026-04-25 at the user’s request. v0.2 delivers the
structural scaffold: - `inst/rules/usda/key.yaml` — 12 Orders in the
canonical key order (GE, HI, SP, AD, OX, VE, AS, UT, MO, AF, IN, EN).
Only Oxisols wired in v0.2 via
[`oxic_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/oxic_usda.md)
(delegating to WRB ferralic). - `R/key-usda.R` —
[`classify_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_usda.md),
[`run_usda_key()`](https://hugomachadorodrigues.github.io/soilKey/reference/run_usda_key.md). -
`R/diagnostics-horizons-usda.R` —
[`oxic_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/oxic_usda.md)
and
[`argillic_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/argillic_usda.md)
as delegations to WRB ferralic / argic in v0.2 (the central criteria
coincide; edge differences scheduled for v0.8).

v0.8 will implement: mollic_usda, umbric_usda, ochric, kandic,
spodic_usda, cambic_usda, calcic_usda, gypsic_usda, duripan, fragipan,
placic, albic, petrocalcic, petrogypsic, gelic conditions, USDA organic
materials, USDA andic properties, USDA vertic features, aridic moisture
regime, argillic_low_bs, argillic_high_bs.

Positioning: USDA Soil Taxonomy has no public maintained implementation
of the key; the `SoilTaxonomy` package on CRAN covers lookup tables.
soilKey + Module 5 will be the first public implementation of the USDA
key.

### Module 6 — SiBCS 5ª edição (v0.7)

v0.2 delivers the structural scaffold: - `inst/rules/sibcs5/key.yaml` —
13 orders in a provisional key order (O, V, E, F, G, M, P, T, L, N, C,
S, R). Latossolos wired via
[`B_latossolico()`](https://hugomachadorodrigues.github.io/soilKey/reference/B_latossolico.md)
(delegating to ferralic). Argissolos wired via
[`B_textural()`](https://hugomachadorodrigues.github.io/soilKey/reference/B_textural.md)
(delegating to argic). - `R/key-sibcs.R` —
[`classify_sibcs()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_sibcs.md),
[`run_sibcs_key()`](https://hugomachadorodrigues.github.io/soilKey/reference/run_sibcs_key.md). -
`R/diagnostics-horizons-sibcs.R` —
[`B_latossolico()`](https://hugomachadorodrigues.github.io/soilKey/reference/B_latossolico.md)
and
[`B_textural()`](https://hugomachadorodrigues.github.io/soilKey/reference/B_textural.md)
as delegations in v0.2.

v0.7 will implement: B nítico, B espódico, B incipiente, B plânico, B
textural with high activity, A chernozêmico, A húmico, A proeminente,
vertic character, glei within top 50, plinthic horizon, organic
hydromorphic horizon, hortic horizon, alitic character, aluminic
character, acric character. Direct validation with Embrapa Solos
(partnership via UFRRJ).

Positioning: no public R/Python package implements SiBCS today.
soilKey + Module 6 will be the first public implementation.

------------------------------------------------------------------------

## 13. Known risks and mitigations

| Risk | Mitigation |
|----|----|
| WRB 2022 has ambiguous definitions in some diagnostics. | Document the interpretation adopted in each function; open issues that reference the exact chapter; propose clarifications to the IUSS WRB WG when applicable. |
| SiBCS 5ª ed. may have tacit updates (Embrapa does not always publish errata). | Explicit version tagging in `rules/sibcs5/`; direct partnership with Embrapa Solos (proposed via the UFRRJ network) for validation. |
| OSSL predictions may fail on under-represented Brazilian soils (e.g., Amazonia). | Allow local spectral libraries (partnership with national networks); report PI95% explicitly; warn when n_neighbors in the filtered region is low. |
| VLMs evolve quickly — models named today may be obsolete in a year. | `ellmer` abstraction + external configuration; regression tests over textual fixtures comparing the resulting JSON independently of the provider. |
| Licensing of training data (WoSIS, RADAMBRASIL). | Use only subsets with a documented open licence; canonical fixtures in the package are ≤ 50 profiles, academic fair use. |

------------------------------------------------------------------------

## 14. Scientific positioning

The package fills three identifiable gaps in the literature:

1.  **There is no public, maintained implementation of the complete WRB
    2022 key.** The preface to the 4th edition itself acknowledges that
    automated classification algorithms were developed internally within
    the IUSS WRB WG during the drafting, but no public release exists.
    `SoilTaxonomy` on CRAN contains only lookup tables.
2.  **There is no package for SiBCS.** Embrapa makes the book and QGIS
    styles available, but no inference library.
3.  **There is no tool that integrates multimodal extraction + spectral
    prediction + taxonomic key in a single traceable workflow.**

The methodological paper can position `soilKey` as open infrastructure
for digital pedology, complementary to the SoilGrids mapping work (which
classifies from *covariates* at pixel scale) — while `soilKey`
classifies from *profile data* at pedon scale, using the canonical key.
These are distinct and complementary inferences.

------------------------------------------------------------------------

## 15. Immediate next steps

1.  **Spec migrated.** This document is the updated version with the
    name `soilKey`, living in `ARCHITECTURE.md` in the project
    directory. The original draft (`pedokey_architecture_1.md` on the
    Desktop) remains as historical record.
2.  **Package skeleton created** — DESCRIPTION, NAMESPACE, structure of
    `R/`, `inst/rules/`, `tests/`, `vignettes/`. `devtools::check()`
    passes empty.
3.  **v0.1 implemented** — `PedonRecord` + `DiagnosticResult` +
    `ClassificationResult` + 3 WRB diagnostics (argic, ferralic,
    mollic) + Ferralsols end-to-end + canonical fixture + tests +
    vignette 01. A viable preprint with a limited Ferralsol benchmark.
4.  **Probe partnerships** — IUSS WRB Working Group (Peter Schad,
    Stephan Mantel) and Embrapa Solos (institutional contact via UFRRJ)
    for validation and possible official endorsement. IUSS endorsement
    would be invaluable for international adoption.
