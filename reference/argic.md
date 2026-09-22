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
argic(
  pedon,
  min_thickness = 7.5,
  system = c("wrb2022", "usda"),
  engine = NULL,
  require_t = NULL
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Minimum thickness in cm (default 7.5).

- system:

  One of `"wrb2022"` (default) or `"usda"`. Selects the clay-increase
  threshold set: WRB uses 6/1.4/20 pp/ratio/pp; KST 13ed uses 3/1.2/8
  (looser). See `test_clay_increase_argic` for the table.

- engine:

  v0.9.63+. One of `"soilkey"` (the hand-coded path, default for
  back-compat) or `"aqp"` (canonical NRCS dispatch via
  [`aqp::getArgillicBounds`](https://ncss-tech.github.io/aqp/reference/getArgillicBounds.html)).
  When `NULL` (the new default) the function reads
  `getOption("soilKey.diagnostic_engine", "soilkey")` so a global
  `options(soilKey.diagnostic_engine = "aqp")` flips every `argic()`
  call without modifying call sites. See
  [`argic_aqp`](https://hugomachadorodrigues.github.io/soilKey/reference/argic_aqp.md).

- require_t:

  v0.9.63+. Forwarded to
  [`aqp::getArgillicBounds`](https://ncss-tech.github.io/aqp/reference/getArgillicBounds.html)
  when `engine = "aqp"`: `TRUE` requires a "t" suffix in the horizon
  designation (the strict KST 13ed text); `FALSE` accepts argic by
  clay-increase alone (more permissive on data-sparse profiles). `NULL`
  (default) auto-picks: `TRUE` for `system = "usda"`, `FALSE` for
  `system = "wrb2022"`. Ignored when `engine = "soilkey"`.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Sub-tests called (each a list with `passed`, `layers`, `missing`,
`details`, `notes`):

- `test_clay_increase_argic` – the three-pronged WRB 2022 clay-increase
  rule.

- `test_minimum_thickness` – thickness \>= 7.5 cm (configurable via
  `min_thickness`).

- `test_texture_argic` – texture of sandy loam or finer
  (`silt + 2 * clay >= 30`).

- `test_not_albeluvic` – excludes profiles with glossic tongues (Retisol
  path).

v0.1 limitations: clay-increase distance (\<= 30 cm vertical, or \<= 15
cm with abrupt textural change) is not yet enforced; that is scheduled
for v0.2 and depends on horizon boundary descriptions.

## References

IUSS Working Group WRB (2022). *World Reference Base for Soil
Resources*, 4th edition. International Union of Soil Sciences, Vienna.
Chapter 3 – Argic horizon.
