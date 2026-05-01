# Test for polyhedral / nutty structure type

WRB 2022 supplementary criterion for the nitic horizon: `structure_type`
matches "polyhedral" or "nutty" (case-insensitive). v0.9.18: now PURELY
PERMISSIVE on missing data. The function returns:

- `passed = TRUE` when at least one candidate layer's `structure_type`
  matches polyhedral / nutty / (sub)angular blocky;

- `passed = NA` when `structure_type` is missing in all candidate layers
  (no evidence either way – never gates a conclusively-FALSE
  supplementary test);

- `passed = NA` (NOT FALSE) when structure is reported but NEITHER
  polyhedral NOR (sub)angular blocky (legacy "granular" / "massive"
  descriptions are too coarse to conclusively contradict). The Nitisol /
  Nitossolo gates still fail when they have stronger contradicting
  evidence elsewhere – this test is no longer a hard veto.

## Usage

``` r
test_polyhedral_or_nutty_structure(h, candidate_layers = NULL)
```
