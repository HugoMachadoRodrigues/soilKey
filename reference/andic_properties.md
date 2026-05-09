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
andic_properties(
  pedon,
  min_alfe = 2,
  max_bd = 0.9,
  min_p_retention = 70,
  min_oc_proxy = 4,
  max_bd_proxy = 0.9
)
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

- min_oc_proxy:

  Minimum SOC % for the v0.9.80 OC+BD proxy path (default 4.0). Only
  consulted when the proxy is enabled via
  `options(soilKey.andic_oc_bd_proxy = TRUE)`.

- max_bd_proxy:

  Maximum bulk density g/cm^3 for the v0.9.80 OC+BD proxy path (default
  0.9). Only consulted when the proxy is enabled.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## v0.9.80 OC + BD proxy (opt-in)

Field-described volcanic-ash soils (e.g.\\ AfSP, KSSL/NASIS, SOTER)
routinely lack oxalate Al/Fe and phosphate retention measurements, so
the canonical paths return `NA` and Andosols cascade to other RSGs. The
genetic signature is still detectable from coarser data: very high SOC
(\>= 4-5%) plus low bulk density (\<= 0.9 g/cm^3) typical of allophanic
/ Al-humus complexation.

With `options(soilKey.andic_oc_bd_proxy = TRUE)` the function adds a
third path that fires when both canonical paths fail and the surface
horizon shows `oc_pct >= min_oc_proxy` AND
`bulk_density_g_cm3 <= max_bd_proxy` (or OC alone \>= 5% when BD is
missing). Default is `FALSE` (canonical behaviour preserved).

## References

IUSS Working Group WRB (2022), Chapter 3, Andic properties.
