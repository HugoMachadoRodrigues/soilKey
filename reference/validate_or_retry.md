# Call a provider, validate JSON output, retry on failure

Sends `prompt` to `provider`, parses the response as JSON, and validates
it against `schema` (a short schema name resolved via
[`load_schema`](https://hugomachadorodrigues.github.io/soilKey/reference/load_schema.md)).
If validation fails, the error message is appended to the prompt and the
call is retried up to `max_retries` times.

## Usage

``` r
validate_or_retry(provider, prompt, schema, max_retries = 3L, image = NULL)
```

## Arguments

- provider:

  An `ellmer` chat object (from
  [`vlm_provider`](https://hugomachadorodrigues.github.io/soilKey/reference/vlm_provider.md))
  or a
  [`MockVLMProvider`](https://hugomachadorodrigues.github.io/soilKey/reference/MockVLMProvider.md)
  instance. Must expose a `$chat(prompt, ...)` method returning text (or
  a character vector of length 1).

- prompt:

  Character scalar with the initial prompt.

- schema:

  Short schema name (`"horizon"`, `"site"`).

- max_retries:

  Integer; total attempts will be at most `1 + max_retries`.

- image:

  Optional `ellmer` image content object (e.g. from
  [`ellmer::content_image_file`](https://ellmer.tidyverse.org/reference/content_image_url.html))
  to pass alongside the prompt for multimodal calls.

## Value

A list with elements `data` (parsed R object), `raw` (character scalar),
`attempts` (integer).

## Details

On success, returns a list with the parsed JSON, the raw text, and the
number of attempts taken. On terminal failure, throws.

This is the single place where the VLM-call -\> validate -\> retry
contract is implemented; every user-facing extractor delegates here.
