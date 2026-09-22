# Test for clay-illuviation evidence (KST 13ed Ch 3 p 4)

KST 13ed argillic horizon requires "evidence of illuvial accumulation of
clay" alongside the clay-increase rule. Acceptable evidence:

- oriented clays bridging sand grains in \>= 1% of the horizon;

- clay films lining pores or coating ped faces;

- lamellae more than 5 mm thick.

## Usage

``` r
argillic_clay_films_test(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

This test reads three complementary slots, in order of evidence
strength:

1.  `pedon$site$nasis_diagnostic_features` – the NASIS
    `pediagfeatures.featkind` vector. The surveyor's explicit "Argillic
    horizon" entry directly confirms clay-illuviation evidence (~13 500
    entries in the 2021 NASIS snapshot). Strongest evidence.

2.  `pedon$horizons$clay_films_amount` – per-horizon clay-film abundance
    derived from NASIS `phpvsf`. Values: `"few"`, `"common"`, `"many"`,
    `"continuous"`. Direct measurement.

3.  `pedon$horizons$designation` containing a 't' master suffix (e.g.
    `Bt`, `Btk`, `Btx`, `Bt1`, `2Bt`). v0.9.28: the pedologist who wrote
    that designation explicitly identified the horizon as clay-illuvial
    – per KST 13ed Ch 18, the 't' suffix means "accumulation of silicate
    clay" – so it counts as positive evidence even when NASIS records
    are absent. This unlocks the KST 13ed argillic thresholds for the
    ~47 pediagfeatures and phpvsf records.

Any of the three sources counts as positive evidence (logical OR).
`passed = NA` when none is populated AND no horizon designation field is
present at all (lab-only loaders without horizon descriptions).
`passed = FALSE` when designations exist but none has a 't' suffix and
NASIS slots are empty.

## References

Soil Survey Staff (2022), Keys to Soil Taxonomy 13th ed., Ch. 3,
argillic horizon (clay-illuviation criteria, p. 4); Ch. 18, master
horizon symbols (`t`: silicate-clay accumulation, p. 332).
