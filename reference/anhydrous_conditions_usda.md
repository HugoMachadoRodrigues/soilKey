# Anhydrous conditions (USDA Soil Taxonomy, 13th edition)

"Anhydrous conditions refer to the moisture condition of soils in very
cold deserts and other areas with permafrost (often dry permafrost).
These soils typically have low precipitation (usually less than 50 mm
water equivalent per year) and a moisture content of less than 3 percent
by weight." – KST 13ed, Ch 3, p 33.

## Usage

``` r
anhydrous_conditions_usda(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Required characteristics:

- Mean annual soil temperature \<= 0 C; AND

- Layer 10-70 cm with soil temperature \< 5 C throughout the year; AND

- No ice-impregnated permafrost in that layer; AND

- One of:

  - Dry (\>= 1500 kPa) in 1/2+ of soil for 1/2+ of time above 0 C; OR

  - Rupture-resistance class loose to slightly hard throughout when temp
    \<= 0 C (except where pedogenically cemented).

Implementation (v0.8.x): Uses `permafrost_temp_C` from schema to flag
layers below freezing; checks rupture_resistance for "loose" / "soft" /
"slightly hard" in the 10-70 cm layer. Precipitation criterion is
deferred to v0.9 (climatic data).

## References

Soil Survey Staff (2022), KST 13ed, Ch. 3, p 33.
