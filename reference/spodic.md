# Spodic horizon (WRB 2022)

Tests whether any horizon meets the spodic horizon criteria. The spodic
horizon is an illuvial horizon with active Al + Fe oxalate- extractable
material plus organic matter; diagnostic of Podzols.

## Usage

``` r
spodic(
  pedon,
  min_thickness = 2.5,
  min_alfe = 0.5,
  max_ph = 5.9,
  min_oc_in_b = 0.5,
  engine = NULL
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Minimum thickness in cm (default 2.5).

- min_alfe:

  Minimum (Al_ox + 0.5 \* Fe_ox) percent (default 0.5).

- max_ph:

  Maximum ph_h2o (default 5.9).

- min_oc_in_b:

  Minimum OC % in the candidate Bh / Bs layer for the v0.9.19
  morphological inference path when Al / Fe oxalate are missing (default
  0.5).

- engine:

  One of `"soilkey"` (default; strict v0.9.19 morphological path
  requires Bh / Bs / Bhs designation + albic E above) or `"aqp"`
  (relaxed v0.9.84 path: any B\* below E\* with OC translocation peak).
  When `NULL`, reads `getOption("soilKey.diagnostic_engine")`.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Sub-tests:

- `test_spodic_aluminum_iron` – (Al_ox + 0.5\*Fe_ox) \>= 0.5%

- `test_ph_below` – ph_h2o \<= 5.9

- `test_minimum_thickness` – thickness \>= 2.5 cm

v0.2 limitations: the WRB color criterion (hue 5YR or yellower with
chroma \<= 5, or specific dark colors) is not enforced. The (Al_ox +
Fe_ox)/clay \>= 0.05 alternative ratio test is not yet wired. Both
deferred to v0.3.

## v0.9.84 engine="aqp" relaxation

KSSL+NASIS Spodosols routinely use generic "B1" / "B2" / "Bw"
designations rather than the specific Bh / Bs / Bhs that the v0.9.19
morphological-inference path requires. Of 14 KSSL+NASIS Podzol
references, only 1 / 14 passes spodic via the v0.9.19 path; 7 / 14 have
BOTH an E-designated albic-eligible horizon above AND an OC peak in a B
horizon below (the canonical Podzol illuviation signature) but use
generic B / Bw designations and so fail strict morph.

When `engine = "aqp"` (read from
`getOption("soilKey.diagnostic_engine", "soilkey")` when `engine` is
`NULL`) AND Al / Fe oxalate is unmeasured AND the v0.9.19 strict path
did not fire, accept any `B*` designation below an `E*`-designated
horizon when:

- `ph_h2o <= max_ph` in the B horizon, AND

- `oc_pct >= min_oc_in_b` in the B horizon, AND

- OC in the B is greater than the maximum OC in any horizon above (the
  translocation signature).

Default `engine` is `"soilkey"` – canonical behaviour bit-for-bit
preserved.

## References

IUSS Working Group WRB (2022), Chapter 3, Spodic horizon.
