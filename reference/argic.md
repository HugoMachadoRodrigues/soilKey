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
  require_t = NULL,
  lithic_discontinuity = NULL
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

- lithic_discontinuity:

  v0.9.220. Apply WRB 2022 criterion 2.a.i: a clay increase over a
  coarser layer of another material (Ap over 2Bt, read from the
  designations' material numbers) counts only with clay films of
  "common" or more (illuvial clay, criterion 2.b). `NULL` (default):
  `TRUE` for `system = "wrb2022"`. The SiBCS B textural, whose item (j)
  accepts such an increase by its textural ratio, and the USDA keys pass
  `FALSE`.

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

Until v0.9.219 a glossic, albeluvic or retic designation anywhere voided
the argic horizon (the WRB 2014 Albeluvisol logic); in WRB 2022 retic
properties belong to an argic or natric horizon.

v0.9.310, for `system = "wrb2022"` (WRB 2022 Ch 3.1.3, p. 35-36):

- criterion 1: the layers have \\\ge\\ 8% clay;

- criterion 2.a.i (v0.9.317): the coarser-textured layer is not
  separated from the layer by a lithic discontinuity, read from the
  material numbers of the designations and from the data of each pair of
  layers in between (Ch 3.2.7, p. 76-77: the ratio sand to silt,
  criterion 2.a.iv; coarse fragments, criterion 6);

- criterion 2.a.ii (v0.9.317): no clay increase counts below a plough
  layer (Ap) that directly overlies the argic horizon;

- criterion 2.a.iii: a layer is compared with a coarser-textured layer
  above it only across a transitional horizon of \\\le\\ 15 cm; layers
  already accepted and layers whose designation has the suffix t are
  part of the argic horizon, not transitional;

- criterion 2.b.ii: clay films of "common" or more (clay coatings on
  \\\ge\\ 15% of the aggregate surfaces) make a layer argic without a
  clay increase;

- a subsurface horizon: the layer at the mineral soil surface and the A
  horizons are not part of it.

The USDA argillic and order keys and the SiBCS B textural keep the
earlier reading.

## References

IUSS Working Group WRB (2022). *World Reference Base for Soil
Resources*, 4th edition. International Union of Soil Sciences, Vienna.
Chapter 3 – Argic horizon.
