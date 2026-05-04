# Fragipan (USDA, KST 13ed Ch 3, p 38)

Pass when a horizon has fragic soil properties:

- rupture_resistance class \>= "firm" (firm, very firm, extremely firm);
  OR

- NASIS pediagfeatures has a "Fragipan" entry (v0.9.31: the surveyor's
  field-identified fragipan – direct evidence, used as a tie-breaker
  when rupture_resistance is missing from the lab data); AND

- thickness \>= 15 cm.

KSSL pedons rarely carry rupture_resistance; NASIS pediagfeatures
carries 13 500 entries including "Fragipan" tags from surveyors. v0.9.31
adds the NASIS path so fragipan can be detected on KSSL+ NASIS pedons
(closing the Fragiudults / Fragiudalfs / Fragiaqualfs confusion
documented in the v0.9.25 Great Group analysis).

## Usage

``` r
fragipan_usda(pedon, max_top_cm = 100)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_top_cm:

  Default 100.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).
