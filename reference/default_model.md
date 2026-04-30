# Default VLM model per provider

Returns a sensible default model name for the requested provider. These
defaults are picked for \*\*vision capability\*\* (multimodal) AND
\*\*structured-extraction reliability\*\* – the two things the soilKey
extraction layer needs.

## Usage

``` r
default_model(name)
```

## Arguments

- name:

  Provider name; one of `"anthropic"`, `"openai"`, `"google"`,
  `"ollama"`.

## Value

Character scalar with the default model identifier.

## Details

Defaults (as of v0.9.11):

- `anthropic = "claude-sonnet-4-7"` – the strongest Claude vision model
  at our 2026 cutoff for document / photo extraction.

- `openai = "gpt-4o"` – text + vision.

- `google = "gemini-2.0-pro"` – successor to 1.5 with longer context +
  better multimodal grounding.

- `ollama = "gemma4:e4b"` – Gemma 4 edge multimodal (text + image; audio
  also). For larger contexts use `"gemma4:31b"`; for cloud-only offload
  via Ollama, `"gemma4-cloud:31b"`. Pull the desired size first with
  `ollama pull gemma4:e4b`.

Users can override at any time:


    vlm_provider("ollama", model = "gemma4:31b")
    vlm_provider("ollama", model = "gemma3:27b")  # back-compat
    vlm_provider("ollama", model = "qwen2.5vl:32b")  # any pulled model
