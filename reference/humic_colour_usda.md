# Humic colour-value intergrade (KST 13ed) — Hum\* Inceptisol subgroups

The "Humic" subgroup differentia for Udept / Xerept / Ustept great
groups (verbatim, e.g. KST-13 Ch. 11 Humic Eutrudepts): a colour value,
moist, of `max_value_moist` (3) or less AND a colour value, dry, of
`max_value_dry` (5) or less (crushed and smoothed sample) throughout the
upper `depth_cm` (18) cm of the mineral soil. This is the dark-coloured
intergrade that does NOT reach an umbric / mollic epipedon (so the order
has already keyed without one) — distinct from
[`humic_inceptisol_usda`](https://hugomachadorodrigues.github.io/soilKey/reference/humic_inceptisol_usda.md)
(the epipedon-based suborder helper). Conservative: requires BOTH the
moist and dry value recorded for every layer overlapping the window (a
missing dry value cannot confirm the criterion), so it never over-fires
on a dark A alone.

## Usage

``` r
humic_colour_usda(pedon, max_value_moist = 3, max_value_dry = 5, depth_cm = 18)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_value_moist, max_value_dry:

  Colour-value ceilings (default 3 / 5).

- depth_cm:

  Top-of-soil window in cm (default 18).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).
