# Sombric horizon (WRB 2022): subsurface accumulation of humus that qualified neither as spodic nor as a true mollic-like horizon (low-base-saturation cool tropical highlands). v0.3.3 detects via designation pattern + OC criteria (BS \< 50, OC \> 0.6, depth \> 25 cm).

Sombric horizon (WRB 2022): subsurface accumulation of humus that
qualified neither as spodic nor as a true mollic-like horizon
(low-base-saturation cool tropical highlands). v0.3.3 detects via
designation pattern + OC criteria (BS \< 50, OC \> 0.6, depth \> 25 cm).

## Usage

``` r
sombric(
  pedon,
  min_thickness = 15,
  min_oc = 0.6,
  max_bs = 50,
  min_top_cm = 25,
  min_oc_increase = 0.1
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Numeric threshold or option (see Details).

- min_oc:

  Numeric threshold or option (see Details).

- max_bs:

  Numeric threshold or option (see Details).

- min_top_cm:

  Numeric threshold or option (see Details).

- min_oc_increase:

  Numeric threshold or option (see Details).
