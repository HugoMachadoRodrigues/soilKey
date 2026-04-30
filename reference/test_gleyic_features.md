# Test for gleyic redoximorphic features within top 50 cm

v0.2 implementation: requires `redoximorphic_features_pct` to be
reported and \>= `min_redox_pct` (default 5%) within `max_top_cm`
(default 50). The Munsell-color proxy (chroma \<= 2, value \>= 4) is too
inclusive for albic / bleached horizons and is therefore not used as a
primary criterion in v0.2; v0.3 will distinguish reductimorphic from
albic via additional indicators. If `redoximorphic_features_pct` is
missing for all candidate layers, returns NA.

## Usage

``` r
test_gleyic_features(
  h,
  max_top_cm = 50,
  min_redox_pct = 5,
  candidate_layers = NULL
)
```

## Arguments

- h:

  Numeric threshold or option (see Details).

- max_top_cm:

  Numeric threshold or option (see Details).

- min_redox_pct:

  Numeric threshold or option (see Details).

- candidate_layers:

  Numeric threshold or option (see Details).
