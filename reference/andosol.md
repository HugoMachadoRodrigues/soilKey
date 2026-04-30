# Andosol RSG gate (WRB 2022 Ch 4, p 104)

WRB-canonical: layer(s) with *andic* OR *vitric* properties, combined
thickness \\= 30 cm within 100 cm starting \\= 25 cm; OR \\= 60% of the
entire soil thickness when a limiting layer starts 25-50 cm. Plus: no
argic, ferralic, petroplinthic, pisoplinthic, plinthic or spodic horizon
\\= 100 cm (unless buried below 50 cm).

## Usage

``` r
andosol(pedon, min_thickness = 30, max_top_cm = 25)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Numeric threshold or option (see Details).

- max_top_cm:

  Numeric threshold or option (see Details).

## Details

v0.3.4 enforces (1) andic OR vitric AND (2) combined thickness \\= 30 cm
starting in the upper 25 cm AND (3) the negative-list exclusions on
argic / ferralic / plinthic / spodic.
