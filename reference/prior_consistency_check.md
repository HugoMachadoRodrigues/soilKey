# Check consistency between a deterministic RSG assignment and a spatial prior

Returns a list describing whether the assigned RSG is plausible under
the given prior. The deterministic classification is never overridden –
this is purely a sanity-check signal.

## Usage

``` r
prior_consistency_check(rsg_code, prior, threshold = 0.01)
```

## Arguments

- rsg_code:

  Two-letter RSG code (e.g. `"FR"`). Either the `rsg_or_order` from a
  [`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md)
  (in which case it must be the RSG name; we try to translate via the
  trace) or the bare code from a key trace entry.

- prior:

  A spatial-prior data.table from
  [`spatial_prior`](https://hugomachadorodrigues.github.io/soilKey/reference/spatial_prior.md).

- threshold:

  Probability below which an assignment is flagged inconsistent (default
  0.01).

## Value

A list with elements:

- `consistent`: `TRUE` / `FALSE` / `NA`.

- `p`: probability of the assigned RSG in the prior (or `NA_real_` if
  not found).

- `threshold`: the threshold used.

- `status`: a short status string – `"consistent"`, `"inconsistent"`, or
  `"no_data"`.

- `note`: human-readable explanation.

- `top_prior`: `data.table` with the top three classes from the prior
  (for messages).
