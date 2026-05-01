# Extract horizons from a soil description PDF

Reads a PDF (typically a soil survey chapter, field-sheet scan, or
thesis appendix), prompts the configured VLM to extract horizon
attributes against `inst/schemas/horizon.json`, and merges the result
into `pedon`. Every extracted attribute is recorded with
`source = "extracted_vlm"` and the model's reported confidence and
verbatim source quote.

## Usage

``` r
extract_horizons_from_pdf(
  pedon,
  pdf_path = NULL,
  provider,
  max_retries = 3L,
  overwrite = FALSE,
  prompt_name = "extract_horizons",
  schema_name = "horizon",
  pdf_text = NULL
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
  to merge into. Mutated in place AND returned invisibly.

- pdf_path:

  Path to the PDF file. Either `pdf_path` or `pdf_text` must be
  supplied.

- provider:

  A chat provider from
  [`vlm_provider`](https://hugomachadorodrigues.github.io/soilKey/reference/vlm_provider.md)
  (or a
  [`MockVLMProvider`](https://hugomachadorodrigues.github.io/soilKey/reference/MockVLMProvider.md)
  for testing).

- max_retries:

  Integer; how many times to re-prompt on validation failure. Default 3.

- overwrite:

  If `TRUE`, lower-authority values are allowed to clobber
  higher-authority ones. Default `FALSE`.

- prompt_name:

  Override the default prompt template (`"extract_horizons"`).

- schema_name:

  Override the default schema (`"horizon"`).

- pdf_text:

  Optional alternative to `pdf_path`: the already-extracted description
  text. Useful for smoke tests, unit tests without `pdftools`, and for
  already-OCR'd field-sheet text.

## Value

Invisibly, the (mutated) `pedon`. Carries a `"vlm_extraction"` attribute
with the parsed response, number of attempts, and number of provenance
entries added.

## Details

The PedonRecord's authority order guarantees that values already tagged
`"measured"` are never silently overwritten by VLM extraction unless
`overwrite = TRUE`.

If the PDF is long (more than ~30,000 characters), it is chunked
page-by-page and each page is sent independently. This is a
conservative-but-simple strategy; for very long surveys callers should
pre-chunk and call this function once per profile.

## Failure modes

- If `pdftools` is not installed -\> error.

- If the PDF cannot be read -\> error.

- If the VLM response fails JSON parse / schema validation after
  `max_retries + 1` attempts -\> error from `validate_or_retry`.
