# Histic epipedon (USDA Soil Taxonomy, 13th edition)

A surface horizon (or layers within 40 cm of the surface) that is
periodically saturated with water and has sufficiently high organic
carbon to be considered organic soil material. Diagnostic for the
Histosols order, the Histels suborder of Gelisols, and the Hist-
modifier in many other taxa.

## Usage

``` r
histic_epipedon_usda(
  pedon,
  min_oc_pct = 12,
  min_thickness_cm = 20,
  min_ap_oc_pct = 8
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_oc_pct:

  Minimum organic carbon percent for organic soil material (default 12;
  equivalent to ~20% organic matter per KST conversion factor 0.58).

- min_thickness_cm:

  Minimum thickness (default 20 cm).

- min_ap_oc_pct:

  Minimum OC for the Ap-horizon shortcut (default 8 percent).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

KST 13ed required characteristics (Ch. 3, pp 13-15):

- Saturated 30+ days/year (or artificially drained); AND

- Organic soil material that is either:

  - 20-60 cm thick AND (Sphagnum \>= 75 percent OR bulk_density \< 0.1
    g/cm3); OR

  - 20-40 cm thick (general); OR

- OR Ap horizon mixed to 25 cm with OC \>= 8 percent by weight.

Implementation notes (v0.8.x):

- Saturation is detected via a horizon designation starting with H (per
  KST notation) or via the WRB `horizonte_glei` as fallback when
  redoximorphic features are present.

- Sphagnum content uses the WRB `fiber_content_rubbed_pct` column (\>=
  75 means very fibrous); refinement to a true Sphagnum-specific column
  is deferred.

## References

Soil Survey Staff (2022). *Keys to Soil Taxonomy*, 13th edition,
USDA-NRCS, Washington DC. Ch. 3, pp. 13-15.
