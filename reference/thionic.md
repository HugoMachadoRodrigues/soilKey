# Thionic horizon (WRB 2022): post-oxidation acid sulfate horizon. Requires sulfidic_s_pct \>= 0.01 AND pH(H2O) \<= 4.

Thionic horizon (WRB 2022): post-oxidation acid sulfate horizon.
Requires sulfidic_s_pct \>= 0.01 AND pH(H2O) \<= 4.

## Usage

``` r
thionic(pedon, min_thickness = 15, max_pH = 4, min_sulfidic_s = 0.01)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Numeric threshold or option (see Details).

- max_pH:

  Numeric threshold or option (see Details).

- min_sulfidic_s:

  Numeric threshold or option (see Details).
