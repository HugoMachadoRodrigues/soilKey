# Cryoturbation (USDA Soil Taxonomy, 13th edition)

"Cryoturbation (frost churning) is the mixing of the soil matrix within
the pedon that results in irregular or broken horizons, involutions,
accumulation of organic matter on the permafrost table, oriented rock
fragments, and silt caps on rock fragments." – KST 13ed, Ch 3, p 43.

## Usage

``` r
cryoturbation_usda(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Diagnostic for the Turbels suborder of Gelisols.

Implementation (v0.8.x): Uses heuristics from horizon designations and
morphology data:

- Designation contains 'jj' (cryoturbation symbol) per KST notation;

- OR boundary_topography in {"irregular", "broken", "involuted"};

- OR coarse_fragments_pct varying non-monotonically with depth (proxy
  for "oriented rock fragments");

- OR designation contains 'f' (frozen) AND irregular
  boundary_distinctness.

Refinement to incorporate explicit `cryoturbation_evidence` column is
deferred to v0.9.

## References

Soil Survey Staff (2022), KST 13ed, Ch. 3, p 43.
