# Andic properties (WRB 2022)

Tests for the andic property complex – volcanic-ash-derived allophanic /
imogolitic / Al-humus material. Diagnostic of Andosols. Two alternative
qualifying paths per WRB 2022 Ch 3.2:

1.  **Al-Fe oxalate + low BD**: (Al_ox + 0.5\*Fe_ox) \>= `min_alfe`
    (default 2.0%) AND bulk_density \<= `max_bd` (default 0.9 g/cm^3) on
    the same layer.

2.  **Phosphate retention**: phosphate_retention_pct \>=
    `min_p_retention` (default 70%).

Either path qualifies. The volcanic-glass criterion is the separate
[`vitric_properties`](https://hugomachadorodrigues.github.io/soilKey/reference/vitric_properties.md)
diagnostic; Andosols key on (andic OR vitric) at the RSG-gate level
([`andosol`](https://hugomachadorodrigues.github.io/soilKey/reference/andosol.md)).

## Usage

``` r
andic_properties(pedon, min_alfe = 2, max_bd = 0.9, min_p_retention = 70)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_alfe:

  Minimum (Al_ox + 0.5\*Fe_ox) percent for the Al-Fe path (default 2.0).

- max_bd:

  Maximum bulk density g/cm^3 for the Al-Fe path (default 0.9).

- min_p_retention:

  Minimum phosphate retention % for the P path (default 70).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

IUSS Working Group WRB (2022), Chapter 3, Andic properties.
