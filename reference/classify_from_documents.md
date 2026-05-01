# Build a fully-classified \`PedonRecord\` from documents in one call

Highest-level entry point of the soilKey VLM pipeline. Given a
soil-description PDF and / or a profile-wall photograph, this function:

## Usage

``` r
classify_from_documents(
  pdf = NULL,
  image = NULL,
  fieldsheet = NULL,
  pedon = NULL,
  provider = "auto",
  model = NULL,
  systems = c("wrb", "sibcs", "usda"),
  report = NULL,
  overwrite = FALSE,
  verbose = TRUE
)
```

## Arguments

- pdf:

  Optional path to a soil-description PDF.

- image:

  Optional path to a profile-wall image (JPG / PNG); if supplied,
  Munsell extraction is attempted with the configured provider.

- fieldsheet:

  Optional path to a site-metadata field sheet (image or PDF).

- pedon:

  Optional existing
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md);
  when supplied, the function fills only the fields VLM extraction can
  fill (subject to the provenance-authority order).

- provider:

  Either a provider name passed to
  [`vlm_provider`](https://hugomachadorodrigues.github.io/soilKey/reference/vlm_provider.md)
  (default `"ollama"`) OR a pre-built ellmer chat object (when you want
  full control over `system_prompt`, `api_key`, ...).

- model:

  Optional model identifier; passed through to
  [`vlm_provider()`](https://hugomachadorodrigues.github.io/soilKey/reference/vlm_provider.md)
  when `provider` is a string. Defaults to the per-provider default from
  [`default_model`](https://hugomachadorodrigues.github.io/soilKey/reference/default_model.md).

- systems:

  Character vector listing which classification systems to run; subset
  of `c("wrb", "sibcs", "usda")`. Default: all three.

- report:

  Optional output path for a self-contained report (`.html` or `.pdf`).
  When supplied,
  [`report`](https://hugomachadorodrigues.github.io/soilKey/reference/report.md)
  is called on the classification results + pedon. Default `NULL` (no
  report file).

- overwrite:

  When merging extracted values into an existing pedon, allow
  VLM-extracted attributes to clobber already-recorded ones. Default
  `FALSE` – the provenance authority order (`measured > extracted_vlm`)
  is enforced by `PedonRecord$add_measurement()`.

- verbose:

  Emit cli progress messages. Default `TRUE`.

## Value

A list with elements:

- `pedon`:

  The (mutated)
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- `classifications`:

  Named list with up to three
  [`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md)
  objects keyed by `wrb`, `sibcs`, `usda`.

- `report`:

  Path to the rendered report file (if `report = ...` was supplied),
  else `NULL`.

- `provider`:

  The chat-provider object actually used (useful for downstream
  debugging or cost accounting).

## Details

1.  Constructs a vision-language provider chat object via
    [`vlm_provider`](https://hugomachadorodrigues.github.io/soilKey/reference/vlm_provider.md)
    (defaults to local Ollama with Gemma 4 edge for institutional
    independence and data sovereignty).

2.  Extracts horizons from `pdf` via
    [`extract_horizons_from_pdf`](https://hugomachadorodrigues.github.io/soilKey/reference/extract_horizons_from_pdf.md),
    Munsell colours from `image` via
    [`extract_munsell_from_photo`](https://hugomachadorodrigues.github.io/soilKey/reference/extract_munsell_from_photo.md),
    and site metadata from `fieldsheet` via
    [`extract_site_from_fieldsheet`](https://hugomachadorodrigues.github.io/soilKey/reference/extract_site_from_fieldsheet.md).
    Every extracted attribute is stamped `source = "extracted_vlm"` in
    the PedonRecord's provenance log.

3.  Runs the three deterministic keys
    ([`classify_wrb2022`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_wrb2022.md),
    [`classify_sibcs`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_sibcs.md),
    [`classify_usda`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_usda.md)).
    The VLM never classifies – the package's architectural invariant is
    preserved.

4.  Optionally renders a one-pager HTML / PDF report via
    [`report`](https://hugomachadorodrigues.github.io/soilKey/reference/report.md).

At least one of `pdf`, `image` or `fieldsheet` must be supplied; you can
also pass an existing partially-filled `PedonRecord` via `pedon` and let
this function fill the gaps.

## Why local-first by default

The default `provider = "ollama"` runs the entire VLM pipeline on the
user's machine via Gemma 4 (edge variant, ~3 GB, multimodal text+image).
No part of the soil description, photograph or field sheet ever leaves
the local network. This is the recommended configuration for
governmental surveys, indigenous land studies, and unpublished research
data; it also makes the pipeline reproducible without an internet
connection. Cloud providers (`"anthropic"`, `"openai"`, `"google"`)
remain one argument away when they are the right call.

## Architectural invariants preserved

- The VLM never classifies. Every extracted value carries
  `source = "extracted_vlm"`; the deterministic keys consume the
  resulting `PedonRecord` unaware of how each value was obtained.

- Provenance is preserved end-to-end. The `evidence_grade` on each
  `ClassificationResult` reflects whether decisive attributes came from
  `measured`, `predicted_spectra`, `extracted_vlm`, `inferred_prior`, or
  `user_assumed` – so a caller always knows how robust the
  classification is.

- Authority order is enforced. A pre-existing `measured` value is never
  silently overwritten by a later `extracted_vlm` value (unless
  `overwrite = TRUE`).

## See also

[`vlm_provider`](https://hugomachadorodrigues.github.io/soilKey/reference/vlm_provider.md),
[`extract_horizons_from_pdf`](https://hugomachadorodrigues.github.io/soilKey/reference/extract_horizons_from_pdf.md),
[`classify_wrb2022`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_wrb2022.md),
[`report`](https://hugomachadorodrigues.github.io/soilKey/reference/report.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# The simplest possible end-to-end call -- local Gemma 4 edge.
res <- classify_from_documents(
  pdf      = "perfil_042_descricao.pdf",
  image    = "perfil_042_parede.jpg",
  report   = "perfil_042.html"
)
res$classifications$wrb$name
#> "Geric Ferric Rhodic Chromic Ferralsol (Clayic, Humic, Dystric, Ochric, Rubic)"

# Cloud provider for a one-shot, production run
res <- classify_from_documents(
  pdf      = "perfil_042_descricao.pdf",
  provider = "anthropic"
)

# Different Gemma 4 size on Ollama
res <- classify_from_documents(
  pdf      = "perfil_042_descricao.pdf",
  provider = "ollama",
  model    = "gemma4:31b"
)
} # }
```
