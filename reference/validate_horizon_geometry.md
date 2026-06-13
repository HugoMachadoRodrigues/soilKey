# Validate horizon depth geometry

A pure, side-effect-free check of a horizon table's depth geometry,
independent of any
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
The Pro app's Pedon builder calls it to give immediate feedback while
horizons are edited, and it is a handy guard before constructing a
profile from an untrusted CSV.

## Usage

``` r
validate_horizon_geometry(horizons)
```

## Arguments

- horizons:

  A data frame with at least numeric `top_cm` and `bottom_cm` columns
  (and optionally a `designation` column).

## Value

A list with `valid` (logical; `TRUE` when there are no errors), `errors`
and `warnings` (character vectors of human-readable English messages),
and `details` – a named list of the offending row indices (or values)
per check, so a caller can compose its own (e.g. localised) messages.

## Details

It reports two severities:

- errors (these make a sane classification impossible):

  a missing or non-numeric `top_cm`/`bottom_cm`; a negative depth; a
  horizon whose `top_cm >= bottom_cm` (inverted or zero thickness); two
  horizons whose depths overlap.

- warnings (allowed, but worth surfacing):

  the shallowest horizon not starting at the surface (0 cm); a gap
  between consecutive horizons; horizons entered out of increasing-depth
  order; a duplicated horizon designation.

This complements `PedonRecord$validate()`, which additionally checks
chemistry (texture sums, pH, CEC vs bases, Munsell ranges); use that for
a built record and this for a raw table.

## Examples

``` r
h <- data.frame(top_cm = c(0, 20, 55), bottom_cm = c(20, 55, 90),
                designation = c("A", "AB", "Bt"))
validate_horizon_geometry(h)$valid          # TRUE
#> [1] TRUE

bad <- data.frame(top_cm = c(0, 40), bottom_cm = c(50, 30))  # overlap+inverted
validate_horizon_geometry(bad)$errors
#> [1] "top_cm >= bottom_cm (inverted or zero-thickness) in row(s) 2."
```
