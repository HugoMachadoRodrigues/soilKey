# Protovertic horizon (WRB 2022 Ch 3.1)

A weakly developed vertic horizon – the swelling/shrinking machinery is
present but does not reach the full vertic intensity (cracks too narrow,
or slickensides only "few", or thickness too small). Used by the
Protovertic qualifier; relevant for soils that would be Vertisols if the
cracks/slickensides were a notch stronger.

## Usage

``` r
protovertic(pedon, min_clay = 30, min_thickness = 15)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_clay:

  Numeric threshold or option (see Details).

- min_thickness:

  Numeric threshold or option (see Details).

## Details

v0.3.5 detection: clay \\= 30% AND any positive vertic evidence
(slickensides at \\= "few" OR cracks_width_cm \\= 0.2 OR a
wedge/lenticular structure_type) AND thickness \\= 15 cm. The positive
cases that pass the strict
[`vertic_horizon`](https://hugomachadorodrigues.github.io/soilKey/reference/vertic_horizon.md)
test are explicitly excluded so the two diagnostics partition the
vertic-spectrum cleanly.
