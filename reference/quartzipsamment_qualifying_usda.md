# Quartzipsamment helper (Quartzipsamments: \>= 95% resistant minerals)

KST 13ed Ch 8 (p 357) defines Quartzipsamments as Psamments where "a
weighted average of the resistant minerals in the 0.02-2.0 mm fraction
is at least 95 percent". Resistant minerals are dominated by quartz; the
practical proxy is a profile that is uniformly sandy with very little
clay AND minimal coarse fragments AND no explicit mineralogical evidence
of weatherable minerals.

## Usage

``` r
quartzipsamment_qualifying_usda(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Details

v0.9.31 broadens the proxy from "clay \<= 5 \<= 5 were caught) to:

- `clay_pct <= 10` (loamy sands and finer sands all qualify – the 5

- `sand_pct >= 80` (sand-dominated texture – a NEW requirement, since
  clay alone is not sufficient);

- `coarse_fragments_pct <= 15` (some coarse fragments tolerated; 5

- at least 50 (preserved from v0.8).

This still excludes Loamy Psamments and Sandy-Loamy Psamments
(Udipsamments / Ustipsamments fallthroughs) by requiring sand \>= 80
near-pure-sand texture.
