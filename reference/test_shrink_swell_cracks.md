# Test for shrink-swell cracks meeting the WRB 2022 Ch 3.2.12 width (\>= 0.5 cm when soil is dry)

If `cracks_width_cm` is missing, the test falls back to designation
pattern matching (`Bss`, `Css`, etc.) and `slickensides` \>= "common" as
proxy evidence.

## Usage

``` r
test_shrink_swell_cracks(
  h,
  min_width_cm = 0.5,
  min_depth_cm = 0,
  candidate_layers = NULL
)
```
