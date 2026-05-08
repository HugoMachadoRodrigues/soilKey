# Test for gleyic redoximorphic features within top 50 cm

Two evidence paths (any qualifies):

1.  **Mottle percent** (primary): explicit `redoximorphic_features_pct`
    \>= `min_redox_pct` (default 5\\ is the v0.2 path.

2.  **Gleyic Munsell hue** (v0.9.61, secondary): the horizon Munsell hue
    matches gleyic patterns (N / 5GY / 10G / 5BG / 10B etc.) AND chroma
    \<= 2. Used when mottle percent is not reported. Common in BDsolos
    exports where surveyors fill matiz/valor/croma but leave mottle
    quantity empty.

Either path qualifies. If neither is determinable for any candidate
layer (mottle pct AND hue both NA), returns NA. If both are determinable
but neither passes, returns FALSE.

## Usage

``` r
test_gleyic_features(
  h,
  max_top_cm = 50,
  min_redox_pct = 5,
  candidate_layers = NULL,
  max_chroma = 2
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

- max_chroma:

  Numeric threshold; gleyic-hue path requires
  `munsell_chroma_moist <= max_chroma` (default 2).
