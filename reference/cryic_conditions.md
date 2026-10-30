# Cryic conditions (WRB 2022)

Tests whether continuous frozen / permafrost material occurs within the
upper `max_top_cm`. Two alternative paths qualify per WRB 2022:

1.  **Permafrost temperature**: a layer at top_cm \<= `max_top_cm`
    (default 100) with `permafrost_temp_C <= max_temp_C` (default 0 C).

2.  **Designation pattern**: a layer at top_cm \<= `max_top_cm` with
    designation containing suffix `"f"` (frozen) or matching `"^Cf"` /
    `"perma"`. A site that declares the Canadian notation
    (`site$designation_system = "cssc"`) writes a frozen layer `z`; the
    New Zealand and Australian notations have no frozen suffix, so there
    only `"perma"` counts. Used as a fallback when the temperature field
    is not in the pedon (typical of legacy survey data).

Either path qualifies. Diagnostic of Cryosols. When neither passes and
the site has a soil temperature regime warmer than gelic (cryic, frigid,
mesic, thermic, hyperthermic or an iso form;
`site$soil_temperature_regime`), the answer is no: those regimes have no
permafrost (KST 13, Ch. 3, p. 50).

## Usage

``` r
cryic_conditions(pedon, max_top_cm = 100, max_temp_C = 0)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_top_cm:

  Maximum top depth (cm) (default 100).

- max_temp_C:

  Maximum mean annual permafrost-zone temperature (deg C) for the
  temperature path (default 0).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

IUSS Working Group WRB (2022), Chapter 5, Cryosols.
