# Validate / normalise a prior data.table

Internal helper used by all backends. Coerces input to data.table with
canonical columns, drops NA codes, and renormalises so that
probabilities sum to 1.

## Usage

``` r
normalize_prior(prior)
```
