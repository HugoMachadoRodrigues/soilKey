# Vision-language extraction of pedon data (Module 2)

Module 2 (`vlm-*`) lets `soilKey` build a `PedonRecord` from a
field-description PDF, a profile photo, or a fieldsheet image – using a
vision-language model (VLM) for the extraction and JSON-Schema
validation as a hard gate. **The taxonomic key itself is never delegated
to the LLM**: the VLM is restricted to extraction, and every extracted
value is recorded in the provenance log so the evidence grade reflects
the lower confidence of VLM-sourced data.

This vignette walks the extraction loop end to end, using a
`MockVLMProvider` so the example runs offline and without API keys.
Swapping the mock for a real `ellmer` chat (Anthropic, OpenAI, etc.) is
a one-line change.

## 1. The mock provider

`MockVLMProvider` exposes the same `$chat()` method as an `ellmer` chat
object, but pops responses from a pre-loaded queue. Because all of
`soilKey`’s VLM logic talks to the provider through `$chat()`, swapping
it for a real chat (or for any custom backend) is transparent.

``` r

mock <- MockVLMProvider$new(responses = list())
class(mock)
#> [1] "MockVLMProvider" "R6"
```

## 2. Schemas and prompts

VLM responses are constrained by a JSON Schema (Draft-07). The schema
for horizon extraction is shipped at `inst/schemas/horizon.json`; the
rendered prompt template lives at `inst/prompts/extract_horizons.md`.

``` r

sch <- jsonlite::fromJSON(soilKey:::load_schema("horizon"),
                            simplifyVector = FALSE)
length(sch$properties$horizons$items$properties)
#> [1] 35
head(names(sch$properties$horizons$items$properties), 12)
#>  [1] "top_cm"                "bottom_cm"             "designation"          
#>  [4] "boundary_distinctness" "boundary_topography"   "munsell_moist"        
#>  [7] "munsell_dry"           "structure_grade"       "structure_size"       
#> [10] "structure_type"        "consistence_moist"     "clay_films_amount"
```

The schema enforces strict types – `top_cm` must be numeric,
`designation` must be a string, Munsell colours come as
`{hue, value, chroma, confidence, source_quote}` triples, etc. Any
response that fails validation triggers the retry loop with the
validation error included in the next prompt.

## 3. Walking the extraction loop with the mock

Suppose the model returns the following horizon-level JSON for a
synthetic Latossolo description:

``` r

horizon_json <- '{
  "horizons": [
    {
      "top_cm": 0,
      "bottom_cm": 15,
      "designation": "A",
      "munsell_moist": {"hue": "2.5YR", "value": 3, "chroma": 4,
                          "confidence": 0.85, "source_quote": "vermelho-escuro"},
      "clay_pct": {"value": 50, "confidence": 0.9, "source_quote": "muito argilosa (50%)"},
      "oc_pct"  : {"value": 2.0, "confidence": 0.85, "source_quote": "C org. 2.0%"}
    },
    {
      "top_cm": 15,
      "bottom_cm": 65,
      "designation": "Bw1",
      "munsell_moist": {"hue": "2.5YR", "value": 3, "chroma": 6,
                          "confidence": 0.85, "source_quote": "vermelho"},
      "clay_pct": {"value": 60, "confidence": 0.9, "source_quote": "muito argilosa"},
      "oc_pct"  : {"value": 1.2, "confidence": 0.85, "source_quote": "C org. 1.2%"}
    }
  ]
}'
```

Wire the mock provider to return this JSON, send a prompt through
`validate_or_retry`, and watch the loop:

``` r

mock <- MockVLMProvider$new(responses = list(horizon_json))

res <- soilKey:::validate_or_retry(
  provider    = mock,
  prompt      = "extract horizons from <fake document>",
  schema      = "horizon",
  max_retries = 0L
)

str(res, max.level = 2)
#> List of 3
#>  $ data    :List of 1
#>   ..$ horizons:List of 2
#>  $ raw     : chr "{\n  \"horizons\": [\n    {\n      \"top_cm\": 0,\n      \"bottom_cm\": 15,\n      \"designation\": \"A\",\n   "| __truncated__
#>  $ attempts: int 1
```

The returned `res$data` is the parsed (validated) R object.
`res$attempts == 1` because the canned response passed validation on the
first try. The `validation_error_at` argument of `MockVLMProvider$new()`
forces an attempt to return malformed JSON, exercising the retry path –
see `tests/testthat/test-vlm-extract.R` for a worked retry-loop test.

## 4. Merging extraction into a `PedonRecord`

[`apply_horizons_extraction()`](https://hugomachadorodrigues.github.io/soilKey/reference/apply_horizons_extraction.md)
consumes the parsed JSON and merges it into a `PedonRecord`, recording
each value in the provenance log with `source = "extracted_vlm"`.

``` r

pr <- PedonRecord$new(
  site = list(id = "VLM-demo", lat = -22.5, lon = -43.7,
                country = "BR", parent_material = "gneiss"),
  horizons = data.table::data.table(top_cm = numeric(0), bottom_cm = numeric(0))
)

added <- soilKey:::apply_horizons_extraction(pr, res$data, overwrite = TRUE)
cat("Provenance entries added:", added, "\n")
#> Provenance entries added: 12

# Inspect what landed in the horizons table.
pr$horizons[, .(top_cm, bottom_cm, designation,
                munsell_hue_moist, munsell_value_moist, munsell_chroma_moist,
                clay_pct, oc_pct)]
#>    top_cm bottom_cm designation munsell_hue_moist munsell_value_moist
#>     <num>     <num>      <char>            <char>               <num>
#> 1:      0        15           A             2.5YR                   3
#> 2:     15        65         Bw1             2.5YR                   3
#>    munsell_chroma_moist clay_pct oc_pct
#>                   <num>    <num>  <num>
#> 1:                    4       50    2.0
#> 2:                    6       60    1.2
```

## 5. Provenance and evidence grade

Every value that came from the VLM carries `source = "extracted_vlm"` in
the provenance log. The classification’s evidence grade reacts:

``` r

prov <- pr$provenance
head(prov[, .(horizon_idx, attribute, source, confidence)])
#>    horizon_idx            attribute        source confidence
#>          <int>               <char>        <char>      <num>
#> 1:           1          designation extracted_vlm         NA
#> 2:           1    munsell_hue_moist extracted_vlm       0.85
#> 3:           1  munsell_value_moist extracted_vlm       0.85
#> 4:           1 munsell_chroma_moist extracted_vlm       0.85
#> 5:           1             clay_pct extracted_vlm       0.90
#> 6:           1               oc_pct extracted_vlm       0.85
```

Even the simplest call to
[`classify_wrb2022()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_wrb2022.md)
will reflect the lower-confidence VLM source via a lower evidence grade
than the same profile assembled from lab data. (See vignette 02 for the
grade ladder.)

## 6. Production: swap the mock for an `ellmer` chat

In production, replace `MockVLMProvider` with a chat object from
`ellmer`. The rest of the pipeline is unchanged.

### 6a. Local-first with Gemma 4 (Ollama)

For sensitive field descriptions, governmental surveys or just for
reproducibility without an API key, the recommended path is a **local
Gemma 4** model via [Ollama](https://ollama.com). The data never leaves
your machine.

``` bash
# Install once (any platform)
ollama pull gemma4:e4b           # ~3 GB, multimodal, fits a laptop
# ollama pull gemma4:31b         # frontier dense, best quality
# ollama serve                   # background server
```

``` r

# Local Gemma 4 edge -- multimodal text + image (and audio).
provider <- vlm_provider("ollama")             # default: gemma4:e4b
# provider <- vlm_provider("ollama", model = "gemma4:31b")  # frontier

extract_horizons_from_pdf(
  pedon       = pr,
  pdf_path    = "field-reports/perfil-LV-001.pdf",
  provider    = provider,
  max_retries = 3L
)
```

### 6b. Cloud providers (Anthropic, OpenAI, Google)

``` r

# install.packages("ellmer")

# Anthropic Claude (needs ANTHROPIC_API_KEY in the environment).
provider <- vlm_provider("anthropic")          # default: claude-sonnet-4-7

# Or OpenAI / Google in the same one-liner shape:
# provider <- vlm_provider("openai")           # default: gpt-4o
# provider <- vlm_provider("google")           # default: gemini-2.0-pro

extract_horizons_from_pdf(
  pedon       = pr,
  pdf_path    = "field-reports/perfil-LV-001.pdf",
  provider    = provider,
  max_retries = 3L
)
```

### 6c. The one-liner: `classify_from_documents()`

For the canonical case – “I have a PDF and (optionally) a profile photo,
give me the three classifications and a one-pager report” – chain
everything in a single call. The default provider is local Gemma 4 edge:

``` r

res <- classify_from_documents(
  pdf      = "perfil_042_descricao.pdf",
  image    = "perfil_042_parede.jpg",
  report   = "perfil_042.html"            # optional output report
)

res$classifications$wrb$name
#> "Geric Ferric Rhodic Chromic Ferralsol (Clayic, Humic, Dystric, Ochric, Rubic)"

res$classifications$sibcs$name
#> "Latossolos Vermelhos Distroficos tipicos, argilosa, moderado"

res$classifications$usda$name
#> "Rhodic Hapludox"
```

Photos go through
[`extract_munsell_from_photo()`](https://hugomachadorodrigues.github.io/soilKey/reference/extract_munsell_from_photo.md);
site metadata through
[`extract_site_from_fieldsheet()`](https://hugomachadorodrigues.github.io/soilKey/reference/extract_site_from_fieldsheet.md).
All three follow the same validate-or-retry contract so they accept any
provider that exposes `$chat(prompt, ...)`.

The next vignette (`v05_spatial_spectra_pipeline`) shows how the
SoilGrids spatial prior and OSSL Vis-NIR predictions can fill remaining
gaps in the pedon before classification.
