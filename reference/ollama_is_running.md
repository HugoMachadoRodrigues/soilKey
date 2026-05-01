# Is the local Ollama HTTP API reachable?

Probes `http://127.0.0.1:11434/api/tags` (the standard Ollama endpoint)
with a short HTTP HEAD-style GET. Returns `TRUE` only if the request
returns HTTP 200 in under `timeout_s` seconds. Used by
[`vlm_pick_provider`](https://hugomachadorodrigues.github.io/soilKey/reference/vlm_pick_provider.md)
for the `provider = "auto"` fallback chain. Override the URL via
`options(soilKey.ollama_url = "http://host:port")`.

## Usage

``` r
ollama_is_running(url = NULL, timeout_s = 1.5)
```

## Arguments

- url:

  Override URL to probe (default reads
  `getOption("soilKey.ollama_url", default = "http://127.0.0.1:11434/api/tags")`).

- timeout_s:

  Request timeout in seconds (default 1.5).

## Value

Logical scalar.
