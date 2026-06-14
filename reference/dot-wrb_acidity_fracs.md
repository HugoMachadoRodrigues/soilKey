# WRB 2022 exchangeable-acidity status over a depth window.

WRB 2022 (Ch 5) defines Dystric/Eutric (and the Hyper-/Ortho- variants)
by **exchangeable Al vs exchangeable bases**, NOT by base saturation: a
layer is "dystric-side" when exch. Al \\\>\\ `factor` times exch.
(Ca+Mg+K+Na), and "eutric-side" when exch. bases \\\ge\\ `factor` times
exch. Al (`factor = 1` for Dystric/Eutric, `4` for the Hyper- variants).
Mineral layers use `al_sat_pct` (primary) or `al_cmol` against the
summed base cations; organic layers (`oc_pct >= 20`) use the WRB
Histosol pH branch (pH_water cutoffs 5.5, or 4.5/6.5 when
`factor >= 4`). Per the user's "strict" policy there is NO
base-saturation fallback: a layer with neither Al datum nor (for organic
layers) pH is undeterminable and counts as NA thickness.

## Usage

``` r
.wrb_acidity_fracs(pedon, dmin = 20, dmax = 100, factor = 1)
```

## Details

Returns thickness-weighted fractions of the candidate layers (those
overlapping `[dmin, dmax]`) that are dystric-side / eutric-side / NA,
plus the qualifying layer indices.
