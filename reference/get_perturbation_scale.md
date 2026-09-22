# Monte-Carlo perturbation scale for an evidence grade

Returns the noise magnitudes used by
[`classify_with_uncertainty`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_with_uncertainty.md)
for a cell of the given evidence grade. A measurement (grade A) is
perturbed only slightly; a user-assumed value (grade E) is perturbed
heavily, reflecting how little is actually known about it.

## Usage

``` r
get_perturbation_scale(grade = c("A", "B", "C", "D", "E"))
```

## Arguments

- grade:

  One of `"A"` (measured), `"B"` (spectra-predicted), `"C"`
  (prior-inferred), `"D"` (VLM-extracted) or `"E"` (user-assumed).

## Value

A list with three elements: `pct` (the half-width of the multiplicative
perturbation, applied to most numeric attributes), `ph_abs` (the
half-width of the additive perturbation applied to pH columns) and
`munsell_abs` (the additive half-width for Munsell value / chroma
columns).

## Examples

``` r
get_perturbation_scale("A")$pct   # 0.03 -- measured values barely move
#> [1] 0.03
get_perturbation_scale("E")$pct   # 0.30 -- assumptions move a lot
#> [1] 0.3
```
