# Bayesian posterior classifier (optional)

Combines a deterministic
[`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md)
with a spatial prior. The deterministic key remains authoritative – this
function reports only an alternative probabilistic view useful for
downstream uncertainty quantification.

## Usage

``` r
posterior_classify(result, prior, epsilon = 0.001)
```

## Arguments

- result:

  A
  [`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md)
  from
  [`classify_wrb2022`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_wrb2022.md).

- prior:

  A spatial-prior data.table (as returned by
  [`spatial_prior`](https://hugomachadorodrigues.github.io/soilKey/reference/spatial_prior.md)).

- epsilon:

  Small smoothing constant added to all prior entries before
  normalising, so RSGs unseen by the prior do not receive zero
  posterior.

## Value

A `data.table` with columns `rsg_code`, `prior`, `likelihood`,
`posterior`.

## Details

Posterior is computed under the simple model: \$\$P(rsg \| site,
evidence) \propto L(rsg \| evidence) \times P(rsg \| site)\$\$ where the
likelihood `L` is concentrated on the deterministic assignment (delta-1
at that code) by default, optionally smoothed if `key_passed_others` is
supplied.
