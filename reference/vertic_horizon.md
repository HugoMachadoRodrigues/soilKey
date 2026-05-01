# Vertic horizon (WRB 2022 Ch 3.1)

Stricter than the vertic \*properties\*: the vertic \*horizon\* requires
\\= 30% clay throughout, slickensides at \\= "common" level, AND
shrink-swell cracks \\= 0.5 cm wide. Used by Vertisols. v0.9.19 adds an
OR-alternative COLE-based linear-extensibility path: summed
(`cole_value * thickness`) over the upper 100 cm \\= 6 cm passes the
diagnostic even when slickensides + cracks are not recorded (KST 13ed Ch
16 LE alternative, p 343).

## Usage

``` r
vertic_horizon(
  pedon,
  min_clay = 30,
  min_thickness = 25,
  min_le_cm = 6,
  le_max_depth_cm = 100
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_clay:

  Numeric threshold or option (see Details).

- min_thickness:

  Numeric threshold or option (see Details).

- min_le_cm:

  Minimum LE sum (cm) for the COLE-based path (default 6, per KST 13ed
  Ch 16).

- le_max_depth_cm:

  Depth window (cm) for the COLE-based path (default 100).
