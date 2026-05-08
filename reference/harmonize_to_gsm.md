# Harmonise pedons to GlobalSoilMap depth intervals

Runs
[`mpspline2::mpspline_tidy()`](https://rdrr.io/pkg/mpspline2/man/mpspline-wrappers.html)
on each requested numeric horizon attribute, producing a new PedonRecord
per input pedon whose horizons table covers the canonical GSM intervals
([`GSM_DEPTHS`](https://hugomachadorodrigues.github.io/soilKey/reference/GSM_DEPTHS.md)).
Categorical attributes (designation, Munsell hue) are propagated by
mode-over-depth-overlap.

## Usage

``` r
harmonize_to_gsm(
  pedons,
  attributes = c("clay_pct", "silt_pct", "sand_pct", "ph_h2o", "oc_pct", "cec_cmol",
    "base_saturation_pct", "munsell_value_moist", "munsell_chroma_moist",
    "redoximorphic_features_pct"),
  depths = GSM_DEPTHS,
  lam = 0.1,
  verbose = TRUE
)
```

## Arguments

- pedons:

  A list of
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
  objects.

- attributes:

  Character vector of numeric horizon column names to harmonise. Default
  covers the chemistry / texture / Munsell numeric columns the soilKey
  diagnostics use.

- depths:

  Numeric vector of GSM depth boundaries (n+1 values for n intervals).
  Default
  [`GSM_DEPTHS`](https://hugomachadorodrigues.github.io/soilKey/reference/GSM_DEPTHS.md).

- lam:

  Smoothing parameter for the spline (default 0.1, per Bishop et al.
  1999 recommendation).

- verbose:

  If `TRUE` (default), emits cli progress.

## Value

A list of new
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
objects with harmonised horizons.

## Why mass-preserving

The Bishop et al. (1999) spline conserves the integral of the attribute
over depth: if the original pedon has 30 g/kg OC over 0-15 cm, the
harmonised pedon will report 30 g/kg integrated over 0-15 cm (split
between 0-5 and 5-15 in proportion to the spline-implied gradient). This
is a critical property for benchmark integrity: simple linear
interpolation does not preserve mass and biases means upward / downward
systematically.

## Categorical handling

`designation` and `munsell_hue_moist` (and other character columns in
the horizon schema) cannot be splined. Instead, for each target GSM
interval, we pick the modal value weighted by the depth-overlap fraction
with the input horizons. Ties broken by uppermost-input-horizon
precedence.

## References

Bishop, T.F.A., McBratney, A.B., Laslett, G.M. (1999). "Modelling soil
attribute depth functions with equal-area quadratic smoothing splines."
*Geoderma* 91: 27-45.

Arrouays, D. et al. (2014). "GlobalSoilMap: Toward a fine-resolution
global grid of soil properties." *Advances in Agronomy* 125: 93-134.

## See also

[`mpspline2::mpspline_tidy`](https://rdrr.io/pkg/mpspline2/man/mpspline-wrappers.html),
[`GSM_DEPTHS`](https://hugomachadorodrigues.github.io/soilKey/reference/GSM_DEPTHS.md).
