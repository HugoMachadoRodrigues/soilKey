# Extract Munsell color from a profile photo

Sends the photo to a multimodal VLM with a prompt that asks the model to
estimate Munsell hue / value / chroma per visible horizon (when a
Munsell reference card is in frame). Recorded as `extracted_vlm` with
the model's self-reported confidence; photos without a reference card
should yield confidence below 0.5 per the prompt specification.

## Usage

``` r
extract_munsell_from_photo(
  pedon,
  image_path,
  provider,
  max_retries = 3L,
  overwrite = FALSE,
  prompt_name = "extract_munsell_from_photo",
  schema_name = "munsell"
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- image_path:

  Path to the image file (JPG / PNG).

- provider:

  A chat provider from
  [`vlm_provider`](https://hugomachadorodrigues.github.io/soilKey/reference/vlm_provider.md)
  (or a `MockVLMProvider` for testing).

- max_retries:

  Integer; how many times to re-prompt on validation failure. Default 3.

- overwrite:

  If `TRUE`, lower-authority values are allowed to clobber
  higher-authority ones. Default `FALSE`.

- prompt_name:

  Override the default prompt template (`"extract_horizons"`).

- schema_name:

  Override the default schema (`"munsell"`, a colour-only subset of the
  horizon schema: geometry, designation and Munsell only). Since
  v0.9.194 – the full horizon schema was injected into the prompt and
  all but those fields discarded, which on a token-metered endpoint cost
  roughly 2,500 tokens per call for attributes a photograph cannot show.

## Value

Invisibly, the mutated `pedon`, with the photo added to `pedon$images`.

## Details

Quantitative non-color attributes (clay %, CEC, pH, etc.) are **never**
extracted from photos, by prompt-level instruction. If the model returns
one anyway, it is silently dropped.
