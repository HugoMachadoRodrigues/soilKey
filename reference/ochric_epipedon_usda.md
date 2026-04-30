# Ochric epipedon (USDA Soil Taxonomy, 13th edition)

The catch-all surface epipedon: any A horizon (or surface horizon with
pedogenic alteration) that does NOT meet the specific requirements of
histic, folistic, melanic, mollic, umbric, anthropic or plaggen.

## Usage

``` r
ochric_epipedon_usda(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

KST 13ed (Ch 3, p 17): "The ochric epipedon fails to meet the
definitions for any of the other seven epipedons because it is too thin
or too dry, has too high a color value or chroma, contains too little
organic carbon, has too high an n value, has too high a fluidity class
or melanic index, or is both massive and hard or harder when dry."

Implementation: pass when none of the 6 implemented epipedons (histic,
folistic, melanic, mollic, umbric – v0.8 implements 5; anthropic /
plaggen are deferred to v0.9 but rare) pass AND the profile has at least
one surface A horizon.

## References

Soil Survey Staff (2022), KST 13ed, Ch. 3, p 17.
