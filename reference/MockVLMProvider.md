# Mock VLM provider for testing

Mock VLM provider for testing

Mock VLM provider for testing

## Details

A stand-in for an `ellmer` chat object. Exposes the same
`$chat(prompt, ...)` method, but instead of calling a model it pops the
next response from a pre-loaded queue. Designed for testthat unit tests
that exercise extraction logic without API keys or network access.

Each call to `$chat()` returns the next element of the `responses` list.
If the call number matches `validation_error_at`, that response is
replaced with a deliberately malformed JSON string, allowing tests to
exercise the retry-on-validation-failure path implemented in
[`validate_or_retry`](https://hugomachadorodrigues.github.io/soilKey/reference/validate_or_retry.md).

## Example


    good_json <- '{"horizons": [...]}'
    mock <- MockVLMProvider$new(responses = list(good_json))
    result <- mock$chat("any prompt")  # returns good_json

    # Simulate one validation error before success.
    mock <- MockVLMProvider$new(
      responses = list("not really json", good_json),
      validation_error_at = NULL  # already invalid as-is
    )

    # Or force an attempt to be invalid via the helper.
    mock <- MockVLMProvider$new(
      responses = list(good_json, good_json),
      validation_error_at = 1L
    )

## Inspection

After use, the mock exposes `$call_count` (integer) and
`$prompts_received` (list of every prompt passed to `$chat()`), which
lets tests assert that retry prompts include the previous validation
error.

## Public fields

- `responses`:

  List of canned responses (character scalars or R objects to be
  JSON-serialised).

- `validation_error_at`:

  Optional integer; when the call number matches, the returned text is
  replaced with a malformed JSON string.

- `call_count`:

  Integer counter (0 before any call).

- `prompts_received`:

  List recording every prompt passed to `$chat()`.

## Methods

### Public methods

- [`MockVLMProvider$new()`](#method-MockVLMProvider-new)

- [`MockVLMProvider$chat()`](#method-MockVLMProvider-chat)

- [`MockVLMProvider$reset()`](#method-MockVLMProvider-reset)

- [`MockVLMProvider$clone()`](#method-MockVLMProvider-clone)

------------------------------------------------------------------------

### Method `new()`

Construct a mock provider.

#### Usage

    MockVLMProvider$new(responses = list(), validation_error_at = NULL)

#### Arguments

- `responses`:

  List of canned responses. Strings are returned verbatim; non-string
  elements are JSON-serialised via
  [`jsonlite::toJSON`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html).

- `validation_error_at`:

  Optional integer giving the 1-based index of an attempt that should
  return malformed JSON (to test the retry path). Use `NULL` (default)
  to always return the queued response unchanged.

------------------------------------------------------------------------

### Method `chat()`

Send a prompt; returns the next queued response.

#### Usage

    MockVLMProvider$chat(prompt, ...)

#### Arguments

- `prompt`:

  Character scalar (the rendered prompt). Stored in `$prompts_received`.

- `...`:

  Additional arguments are accepted (and ignored) so the signature
  matches multimodal calls that pass an image content object after the
  prompt.

#### Returns

Character scalar with the response text.

------------------------------------------------------------------------

### Method `reset()`

Reset the mock (call count and prompt log).

#### Usage

    MockVLMProvider$reset()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MockVLMProvider$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
