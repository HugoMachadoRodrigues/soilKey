# Andosol RSG gate (WRB 2022 Ch 4, p 104)

WRB-canonical: layer(s) with *andic* OR *vitric* properties, combined
thickness \\= 30 cm within 100 cm starting \\= 25 cm; OR \\= 60% of the
entire soil thickness when a limiting layer starts 25-50 cm. Plus: no
argic, ferralic, petroplinthic, pisoplinthic, plinthic or spodic horizon
\\= 100 cm (unless buried below 50 cm).

## Usage

``` r
andosol(
  pedon,
  min_thickness = 30,
  max_top_cm = 25,
  buried_below_cm = 50,
  strict = NULL
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Numeric threshold or option (see Details).

- max_top_cm:

  Numeric threshold or option (see Details).

- buried_below_cm:

  Numeric: layers of the exclusion diagnostics whose top_cm \\= this
  depth are treated as buried and do NOT exclude the Andosol (default
  50, per WRB 2022 Ch 4 p 104).

- strict:

  Logical or `NULL`. When `NULL` (default) it resolves via
  `getOption("soilKey.rsg_strict", FALSE)`. `TRUE` disables the
  buried-exclusion tolerance.

## Details

v0.3.4 enforces (1) andic OR vitric AND (2) combined thickness \\= 30 cm
starting in the upper 25 cm AND (3) the negative-list exclusions on
argic / ferralic / plinthic / spodic.

## v0.9.85 buried-exclusion fix

WRB 2022 Ch 4 p 104 specifies the Andosol exclusion list (argic /
ferralic / petroplinthic / pisoplinthic / plinthic / spodic) as "\<= 100
cm *unless buried below 50 cm*". The earlier implementation excluded an
Andosol whenever any of those diagnostics passed anywhere in the
profile, including on layers starting deeper than 50 cm – which
mis-fires on AfSP Andosol references like `CM W3_0047`, where an argic
layer at 56-72 cm wrongly excluded the andic surface stack. v0.9.85
restricts the exclusion check to layers starting \<= 50 cm: a buried
argic / ferralic / plinthic / spodic at deeper levels no longer
disqualifies the surface andic stack from Andosol.

## Tier-3 strict mode (v0.9.98)

With `strict = TRUE` the v0.9.85 buried-exclusion tolerance is switched
off: *any* argic / ferralic / plinthic / spodic horizon anywhere in the
profile excludes the Andosol, regardless of depth.
