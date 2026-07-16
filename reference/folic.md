# Folic horizon (WRB 2022, Chapter 3.1.12)

A surface horizon of **well-aerated** organic material, i.e. organic
material that is *not* water-saturated for long periods. It is the
aerobic twin of the
[`histic_horizon`](https://hugomachadorodrigues.github.io/soilKey/reference/histic_horizon.md):
both consist of organic material, but the histic horizon is
water-saturated for \>= 30 consecutive days in most years while the
folic horizon is saturated for \< 30 days and is not drained.

## Usage

``` r
folic(pedon, min_oc = 12, max_saturation_days = 30, min_thickness = 10)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_oc:

  Minimum soil organic carbon (%) for organic material, default 12
  (matching
  [`histic_horizon`](https://hugomachadorodrigues.github.io/soilKey/reference/histic_horizon.md)).

- max_saturation_days:

  Maximum consecutive water-saturation days that still count as
  well-aerated, default 30.

- min_thickness:

  Minimum thickness in cm, default 10.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Diagnostic criteria (all required): organic material that (1) is
saturated with water for \< 30 consecutive days in most years and is not
drained; and (2) has a thickness of \>= 10 cm.

## References

IUSS Working Group WRB (2022), Chapter 3.1.12, Folic horizon.
