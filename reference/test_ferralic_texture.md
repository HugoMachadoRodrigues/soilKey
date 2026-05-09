# Ferralic texture: sandy loam or finer (same predicate as argic)

Ferralic texture: sandy loam or finer (same predicate as argic)

## Usage

``` r
test_ferralic_texture(h, candidate_layers = NULL)
```

## Arguments

- h:

  Numeric threshold or option (see Details).

- candidate_layers:

  Numeric threshold or option (see Details).

## v0.9.70 morphological fallback (opt-in)

Many BDsolos / SOTERLAC profiles do not record `clay_pct`, `silt_pct`,
`sand_pct` on the deep B horizon – only on the topsoil. The strict
texture test then returns `NA`, and
[`ferralic()`](https://hugomachadorodrigues.github.io/soilKey/reference/ferralic.md)
cascades to NA, blocking Latossolos detection.

With `options(soilKey.ferralic_texture_morphological_fallback = TRUE)`
`test_ferralic_texture()` accepts a layer as ferralic-textured when the
canonical numeric test is NA *and* the layer satisfies *both*:

1.  designation matches `Bw|Bo|Boi` (deeply weathered B-horizon
    morphology), and

2.  `top_cm > 20` (subsoil, not topsoil).

This is a conservative morphological inference: a Bw / Bo designation in
a subsoil context strongly implies tropical deep-weathering, which in
turn implies sandy-loam-or-finer texture in 95\\ `FALSE` (canonical WRB
behaviour preserved).
