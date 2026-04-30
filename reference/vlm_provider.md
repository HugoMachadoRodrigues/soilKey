# Construct a VLM provider chat object

Returns an `ellmer` chat object configured for the given provider, ready
to be passed to the extraction functions
([`extract_horizons_from_pdf`](https://hugomachadorodrigues.github.io/soilKey/reference/extract_horizons_from_pdf.md),
etc.). The chat object wraps API credentials and model selection; it
does not itself send any request.

## Usage

``` r
vlm_provider(
  name = c("anthropic", "openai", "google", "ollama"),
  model = NULL,
  ...
)
```

## Arguments

- name:

  Provider name. One of `"anthropic"` (Claude), `"openai"` (GPT-4o
  family), `"google"` (Gemini), `"ollama"` (local).

- model:

  Optional model identifier; defaults to `default_model(name)`.

- ...:

  Additional arguments forwarded to the corresponding `ellmer::chat_*`
  constructor (e.g. `system_prompt`, `api_key`, `base_url`, `params`).

## Value

An `ellmer` `Chat` object exposing a `$chat()` method for sending
prompts.

## Details

This is purely a convenience wrapper: it picks a default model per
provider and forwards remaining arguments (e.g. `system_prompt`,
`api_key`) to the underlying ellmer constructor. `ellmer` must be
installed.

## Local-first option

Passing `name = "ollama"` runs every extraction locally via an Ollama
server (default `gemma3:27b`). No data leaves the machine, which is the
recommended setting for sensitive field descriptions (e.g. governmental
surveys, indigenous land studies) where institutional independence and
data sovereignty matter.

## Examples

``` r
if (FALSE) { # \dontrun{
# Cloud provider (needs ANTHROPIC_API_KEY)
provider <- vlm_provider("anthropic")

# Local provider (needs a running Ollama server)
provider <- vlm_provider("ollama", model = "gemma3:27b")
} # }
```
