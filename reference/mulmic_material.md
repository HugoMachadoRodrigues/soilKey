# Mulmic material (WRB 2022 Ch 3.3.12): mineral material developed from drained organic material; \\= 8% SOC, single grain or fine blocky structure, chroma \\= 2.

Since v0.9.411 every criterion of Ch 3.3.12 (p. 90) is read, layer by
layer: mineral material (\< 20% SOC) with \\= `min_oc` SOC; a single
grain structure or a subangular or angular blocky one of average size
\\= 2 cm (a structure not recorded counts as absent); a moist chroma \\=
`max_chroma`; and recorded evidence that it "has developed from
water-saturated organic material after drainage": organic material below
the layer, or a record of artificial drainage of the site (none counts
as absent). Until v0.9.410 only the SOC and the chroma were read, so the
dark humus-rich topsoils of Andosols were mulmic.

## Usage

``` r
mulmic_material(pedon, min_oc = 8, max_chroma = 2)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_oc:

  Numeric threshold or option (see Details).

- max_chroma:

  Numeric threshold or option (see Details).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
recording whether the diagnostic is present, the qualifying layers, and
the supporting evidence.
