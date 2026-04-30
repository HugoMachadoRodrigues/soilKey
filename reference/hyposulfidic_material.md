# Hyposulfidic material (WRB 2022 Ch 3.3.9): same S and pH as hypersulfidic but does NOT consist of hypersulfidic (i.e. not capable of severe acidification). v0.3.3: returns sulfidic layers that don't meet hypersulfidic.

Hyposulfidic material (WRB 2022 Ch 3.3.9): same S and pH as
hypersulfidic but does NOT consist of hypersulfidic (i.e. not capable of
severe acidification). v0.3.3: returns sulfidic layers that don't meet
hypersulfidic.

## Usage

``` r
hyposulfidic_material(pedon, min_s_pct = 0.01, min_pH = 4)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_s_pct:

  Numeric threshold or option (see Details).

- min_pH:

  Numeric threshold or option (see Details).
