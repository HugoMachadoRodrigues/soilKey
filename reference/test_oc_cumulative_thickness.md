# Test cumulative organic-carbon thickness within a depth window

WRB 2022 alternative criterion for the histic horizon: organic material
\>= `min_oc` % summing to `min_thickness_cm` cumulative thickness within
the upper `max_depth_cm`, even if no single contiguous layer reaches the
standard 10 cm. Relevant for folic / mossy Histosols on slopes.

## Usage

``` r
test_oc_cumulative_thickness(
  h,
  min_oc = 12,
  min_thickness_cm = 40,
  max_depth_cm = 80
)
```
