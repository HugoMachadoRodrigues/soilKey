# Aquic conditions (USDA Soil Taxonomy, 13th edition)

"Soils with aquic conditions are those that currently undergo continuous
or periodic saturation and reduction. The presence of these conditions
is indicated by redoximorphic features, except in Histosols and
Histels." – KST 13ed, Ch 3, p 41.

## Usage

``` r
aquic_conditions_usda(
  pedon,
  max_top_cm = 100,
  min_redox_pct = 5,
  max_chroma = 2
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_top_cm:

  Maximum depth at which saturation must occur (default 100 – typical
  for Suborder keys; 200 for some).

- min_redox_pct:

  Threshold for redoximorphic features (default 5 percent).

- max_chroma:

  Maximum chroma indicating reduction (default 2).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
with `evidence$saturation_type` = "endo" / "epi" / NA.

## Details

Three types of saturation are defined:

- **Endosaturation**: saturated in all layers from the upper boundary of
  saturation to \>=200 cm.

- **Episaturation**: saturated in one or more layers within 200 cm with
  unsaturated layer(s) below.

- **Anthric saturation**: cultivated/flood-irrigated.

Implementation (v0.8.x):

- Saturation is inferred from the presence of redoximorphic features
  (`redoximorphic_features_pct >= 5`) and/or a glei horizon (designation
  containing 'g').

- Reduction is inferred when chroma \<= 2 in the matrix.

- Artificial drainage is treated as positive aquic when
  `site$artificially_drained == TRUE` (deferred – not in current
  schema).

## References

Soil Survey Staff (2022), KST 13ed, Ch. 3, pp 41-44.
