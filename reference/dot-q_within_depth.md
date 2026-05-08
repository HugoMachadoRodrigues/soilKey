# Test "X within depth d cm" given an existing diagnostic

Many WRB sub-qualifiers (Endo-, Bathy-, Hyper-, Pano-, Ortho-, Ano-,
etc.) are depth-bounded modifiers of an existing principal qualifier or
diagnostic horizon. This helper tests whether the base diagnostic fires
AND has any of its passing layers in the given depth window.

## Usage

``` r
.q_within_depth(name, base_diag, pedon, top_cm, bottom_cm)
```
