# Calcic horizon (WRB 2022)

Tests whether any horizon meets the calcic horizon criteria. The calcic
horizon is a horizon of secondary carbonate accumulation, diagnostic for
Calcisols and qualifying many other RSGs.

## Usage

``` r
calcic(pedon, min_thickness = 15, min_caco3_pct = 15)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Minimum thickness in cm (default 15).

- min_caco3_pct:

  Minimum CaCO3 percent in fine earth (default 15).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Sub-tests called:

- `test_caco3_concentration` – CaCO3 \>= 15%.

- `test_minimum_thickness` – thickness \>= 15 cm.

v0.2 limitations: the WRB criterion of "5% absolute or relative more
CaCO3 than the underlying horizon" is not enforced; this captures true
calcic horizons but may also mark uniformly carbonate-rich substrates
that are not pedologically calcic. Cementation (petrocalcic) is not yet
detected. Both refinements are scheduled for v0.3.

## v0.9.314

The WRB calcic horizon follows WRB 2022 Ch 3.1.4 (p. 37-38) criterion by
criterion: criterion 2 is protocalcic properties (secondary carbonates
on \\\ge\\ 5% of the exposed area, `secondary_carbonates_pct`) or
\\\ge\\ 5% more CaCO3 than an underlying layer with no lithic
discontinuity between; a layer of a petrocalcic horizon is not calcic;
the 15 cm are those of the adjoining layers. A layer whose criterion 2
cannot be decided (secondary carbonates not recorded) is undecided, not
calcic. The USDA calcic horizon and the SiBCS horizonte calcico keep the
earlier reading.

## References

IUSS Working Group WRB (2022). *World Reference Base for Soil
Resources*, 4th edition. International Union of Soil Sciences, Vienna.
Chapter 3 – Calcic horizon.
