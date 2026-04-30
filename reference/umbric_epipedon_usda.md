# Umbric epipedon (USDA Soil Taxonomy, 13th edition)

A thick, dark-colored, base-poor (BS \< 50 percent) mineral surface
horizon. Differs from mollic in low base saturation; qualifies the
Humults / Humic / Umbric subgroups in many orders.

## Usage

``` r
umbric_epipedon_usda(pedon, max_bs = 50, min_oc_pct = 0.6)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_bs:

  Maximum BS (default 50 – "less than 50 percent").

- min_oc_pct:

  Minimum OC (default 0.6).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

KST 13ed required characteristics (Ch. 3, pp 18-20):

- Color: same as mollic (V\<=3 moist, V\<=5 dry, chroma\<=3);

- Base saturation (NH4OAc) \< 50 percent in some part;

- Organic carbon \>= 0.6 percent (or 0.6 absolute \> C);

- Thickness: same rules as mollic (18 / 25 / 10 cm);

- Structure: peds \<= 30 cm OR rupture-resistance \<= moderately hard.

## References

Soil Survey Staff (2022), KST 13ed, Ch. 3, pp 18-20.
