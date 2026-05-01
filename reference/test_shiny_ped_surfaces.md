# Test for shiny ped surfaces (informational only)

WRB 2022 mentions shiny faces of polyhedral peds as a supportive
criterion for the nitic horizon. The horizon schema does not carry a
dedicated "shiny_peds" field; `slickensides` is a poor proxy
(slickensides are shrink-swell features, distinct from Fe-oxide-coated
polyhedral ped faces). This predicate therefore returns the slickensides
evidence non-gating: the result is always `passed = NA` when the field
is missing or "absent" (no evidence either way) and `TRUE` when
slickensides is present (taken as suggestive). The diagnostic does not
fail on this test.

## Usage

``` r
test_shiny_ped_surfaces(h, candidate_layers = NULL)
```
