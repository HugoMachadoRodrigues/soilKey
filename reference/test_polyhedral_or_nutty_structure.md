# Test for polyhedral / nutty structure type

WRB 2022 supplementary criterion for the nitic horizon: `structure_type`
matches "polyhedral" or "nutty" (case-insensitive). Treated as
evidence-supportive: when the field is missing entirely, the diagnostic
does not fail; when present and clearly NOT polyhedral/nutty, the
diagnostic fails.

## Usage

``` r
test_polyhedral_or_nutty_structure(h, candidate_layers = NULL)
```
