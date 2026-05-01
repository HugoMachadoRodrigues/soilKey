# Test that clay does NOT decrease abruptly with depth (nitic)

WRB 2022 supplementary criterion for the nitic horizon: clay percent
should NOT show a maximum at the top of the B with abrupt decrease
below. Operationally: across the candidate layers, clay_pct must not
drop by more than `max_drop_pct` between consecutive layers within 50 cm
depth. Returns NA when clay is missing in fewer than two candidate
layers.

## Usage

``` r
test_clay_decreases_with_depth(
  h,
  candidate_layers = NULL,
  max_drop_pct = 8,
  max_depth_cm = 50
)
```
