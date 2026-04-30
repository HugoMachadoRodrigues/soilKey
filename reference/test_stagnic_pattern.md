# Test for stagnic redox features (perched water signature)

Distinct from gleyic (groundwater): stagnic = redoximorphic features in
some layer within the upper `max_top_cm` (default 100) AND redox in
deeper layers DROPS substantially (decay to \< third of the shallow
value). The decay condition is what separates perched water (sits above
an impermeable layer; deeper soil is not saturated) from
groundwater-driven gleying (saturation continues with depth).

## Usage

``` r
test_stagnic_pattern(h, max_top_cm = 100, min_redox_pct = 5, decay_factor = 3)
```

## Arguments

- h:

  Numeric threshold or option (see Details).

- max_top_cm:

  Numeric threshold or option (see Details).

- min_redox_pct:

  Numeric threshold or option (see Details).

- decay_factor:

  Numeric threshold or option (see Details).
