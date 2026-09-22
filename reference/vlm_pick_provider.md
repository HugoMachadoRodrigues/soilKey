# Pick the best available VLM provider

Selects a provider based on what is reachable in the user's environment,
in this preference order: local Ollama (if
[`ollama_is_running()`](https://hugomachadorodrigues.github.io/soilKey/reference/ollama_is_running.md)),
then Anthropic, OpenAI, and Google (each requires the relevant
`*_API_KEY` environment variable). Errors with an actionable
installation / API-key hint when no provider is reachable.

## Usage

``` r
vlm_pick_provider(verbose = TRUE)
```

## Arguments

- verbose:

  If `TRUE` (default), emits a one-line `cli` message explaining the
  chosen provider.

## Value

Character scalar: one of `"ollama"`, `"anthropic"`, `"openai"`,
`"google"`.
