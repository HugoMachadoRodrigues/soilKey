# Classify a pedon under WRB 2022

High-level classification entry point. Pre-computes the implemented
diagnostic horizons (argic, ferralic, mollic) for transparent reporting,
runs the key, and assembles a
[`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md)
with the trace, ambiguities, missing-data hints, evidence grade, and (in
future) prior sanity check.

## Usage

``` r
classify_wrb2022(
  pedon,
  prior = NULL,
  prior_threshold = 0.01,
  on_missing = c("warn", "silent", "error"),
  rules = NULL
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- prior:

  Optional spatial prior – a `data.table` with columns `rsg_code` and
  `probability`, typically the return value of
  [`spatial_prior`](https://hugomachadorodrigues.github.io/soilKey/reference/spatial_prior.md).
  If supplied, the result records a `prior_check` entry from
  [`prior_consistency_check`](https://hugomachadorodrigues.github.io/soilKey/reference/prior_consistency_check.md);
  an inconsistent prior also emits a warning. The deterministic key is
  NEVER overridden by the prior.

- prior_threshold:

  Probability below which the prior triggers an "inconsistent" warning
  (default 0.01).

- on_missing:

  One of `"warn"` (default), `"silent"`, `"error"`. Behaviour when the
  trace reports missing attributes.

- rules:

  Optional pre-loaded rule set.

## Value

A
[`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md).
