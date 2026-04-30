# Plaggic horizon (WRB 2022): sod-derived topsoil \>= 20 cm with low BD AND independent evidence of human input.

v0.9.2.C tightening: the v0.3.3 implementation accepted ANY thick,
low-BD, OC-rich A horizon, which over-fired across natural mollic /
umbric / chernic surfaces. The diagnostic now requires, in addition to
the OC + BD + thickness baseline, at least one independent
anthropogenic-input marker:

- `p_mehlich3_mg_kg >= 50` (sustained sod / manure additions concentrate
  Mehlich-3 P in the topsoil), OR

- `artefacts_pct > 0` (any human artefact volume fraction is sufficient
  as a presence signal), OR

- designation pattern `Apl` / `Aplg` / `Apk` / explicit "plagg".

Without one of those markers the diagnostic returns FALSE even when OC +
BD + thickness pass. This mirrors the v0.9.1 `qual_plaggic` gate but
enforces the rule at the diagnostic level so any caller (SiBCS, USDA,
future modules) inherits the protection.

## Usage

``` r
plaggic(
  pedon,
  min_thickness = 20,
  max_bd = 1.5,
  min_oc = 0.6,
  min_p_mehlich3 = 50
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Numeric threshold or option (see Details).

- max_bd:

  Numeric threshold or option (see Details).

- min_oc:

  Numeric threshold or option (see Details).

- min_p_mehlich3:

  Numeric threshold or option (see Details).
