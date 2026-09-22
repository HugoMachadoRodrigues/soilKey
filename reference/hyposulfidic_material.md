# Hyposulfidic material (WRB 2022 Ch 3.3.9): same inorganic sulfidic S and field pH as hypersulfidic but does NOT consist of hypersulfidic (criterion 3 – does not acidify to pH \< 4 on aerobic incubation, usually self-neutralised by carbonate). Reachable from v0.9.128: when `incubation_ph` is measured, a sulfidic + pH\>=4 layer that stays \>= 4 on incubation is the set-complement of [`hypersulfidic_material`](https://hugomachadorodrigues.github.io/soilKey/reference/hypersulfidic_material.md) and is reported here. Without an incubation pH the two cannot be told apart, so this returns empty (the layer is reported as potential hypersulfidic instead).

Hyposulfidic material (WRB 2022 Ch 3.3.9): same inorganic sulfidic S and
field pH as hypersulfidic but does NOT consist of hypersulfidic
(criterion 3 – does not acidify to pH \< 4 on aerobic incubation,
usually self-neutralised by carbonate). Reachable from v0.9.128: when
`incubation_ph` is measured, a sulfidic + pH\>=4 layer that stays \>= 4
on incubation is the set-complement of
[`hypersulfidic_material`](https://hugomachadorodrigues.github.io/soilKey/reference/hypersulfidic_material.md)
and is reported here. Without an incubation pH the two cannot be told
apart, so this returns empty (the layer is reported as potential
hypersulfidic instead).

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

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
recording whether the diagnostic is present, the qualifying layers, and
the supporting evidence.
