# Test for WRB 2022 reducing conditions (Ch 3.2.10) per layer

Reducing conditions show one or more of:

- rH \< 20 (we don't carry rH so this is deferred);

- presence of free Fe2+ (alpha,alpha-dipyridyl test) – detected via
  designation `r`, `g`, `Br`, etc., or via the
  `redoximorphic_features_pct` \>= 5%;

- iron sulfide (designation pattern `S`, `Aj`, `Ar`);

- methane (not in schema, deferred).

## Usage

``` r
test_reducing_conditions(h, min_redox_pct = 5, candidate_layers = NULL)
```
