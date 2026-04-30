# Default VLM model per provider

Returns a sensible default model name for the requested provider. These
defaults are chosen for vision capability (multimodal) and general
structured-extraction reliability; users can override via the `model`
argument of
[`vlm_provider`](https://hugomachadorodrigues.github.io/soilKey/reference/vlm_provider.md).

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
