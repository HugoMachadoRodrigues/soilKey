# Argic horizon (WRB 2022)

Tests whether any horizon meets the argic horizon criteria per Chapter 3
of the WRB 2022 (4th edition). Argic is a subsurface horizon with
distinctly higher clay content than the overlying horizon, qualified by
three depth-conditional clay-increase rules; it must also have texture
of sandy loam or finer, satisfy a minimum thickness, and not exhibit
albeluvic glossic features (which would direct the profile to the
Retisol path).

## Usage

``` r
argic(pedon, min_thickness = 7.5)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Minimum thickness in cm (default 7.5).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Sub-tests called (each a list with `passed`, `layers`, `missing`,
`details`, `notes`):

- [`test_clay_increase_argic`](https://hugomachadorodrigues.github.io/soilKey/reference/test_clay_increase_argic.md)
  – the three-pronged WRB 2022 clay-increase rule.

- [`test_minimum_thickness`](https://hugomachadorodrigues.github.io/soilKey/reference/test_minimum_thickness.md)
  – thickness \>= 7.5 cm (configurable via `min_thickness`).

- [`test_texture_argic`](https://hugomachadorodrigues.github.io/soilKey/reference/test_texture_argic.md)
  – texture of sandy loam or finer (`silt + 2 * clay >= 30`).

- `test_not_albeluvic` – excludes profiles with glossic tongues (Retisol
  path).

v0.1 limitations: clay-increase distance (\<= 30 cm vertical, or \<= 15
cm with abrupt textural change) is not yet enforced; that is scheduled
for v0.2 and depends on horizon boundary descriptions.

## References

IUSS Working Group WRB (2022). *World Reference Base for Soil
Resources*, 4th edition. International Union of Soil Sciences, Vienna.
Chapter 3 – Argic horizon.
