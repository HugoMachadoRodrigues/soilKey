# Test that a layer's top is at or below a target depth

Inverse of
[`test_top_at_or_above`](https://hugomachadorodrigues.github.io/soilKey/reference/test_top_at_or_above.md):
returns layers whose top is shallower than or equal to `max_top_cm`,
i.e. that start within the upper part of the profile.

## Usage

``` r
test_starts_within(h, max_top_cm, candidate_layers = NULL)
```
