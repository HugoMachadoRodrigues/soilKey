## Summary

<!-- 1-3 lines: what does this PR change and why? -->

## Type of change

<!-- Check all that apply. -->

- [ ] Bug fix (non-breaking change which fixes an issue)
- [ ] New feature (non-breaking change which adds functionality)
- [ ] Breaking change (would cause existing code to behave differently)
- [ ] Documentation update (vignette, README, NEWS, references)
- [ ] Test-only change (no source code modified)
- [ ] Refactor / internal cleanup (no behaviour change)

## Scope

<!-- Which parts of soilKey does this PR touch? -->

- [ ] WRB 2022 key / qualifiers / diagnostics
- [ ] SiBCS 5ª edição key / atributos / família
- [ ] USDA Soil Taxonomy 13ed key
- [ ] Multimodal extraction (VLM)
- [ ] Spectra / OSSL pipeline
- [ ] Spatial prior (SoilGrids)
- [ ] Benchmark drivers / loaders (KSSL+NASIS / Embrapa FEBR / WoSIS)
- [ ] aqp interop
- [ ] Shiny app
- [ ] Documentation only

## How was this tested?

<!-- 1-3 lines per test. -->

- [ ] Existing test suite still passes (`testthat::test_local()`)
- [ ] New unit tests added in `tests/testthat/test-*.R`
- [ ] Benchmark A/B run (KSSL+NASIS / Embrapa / canonical fixtures)
- [ ] R CMD check Status: OK
- [ ] Vignettes still build (if vignettes were touched)

## Checklist

- [ ] I have read [CONTRIBUTING.md](../CONTRIBUTING.md)
- [ ] My change does NOT delegate the taxonomic key to a language model
- [ ] My change preserves the per-attribute provenance log
- [ ] I have updated `NEWS.md` for user-facing changes
- [ ] I have updated relevant vignettes / README / pkgdown reference
- [ ] All YAML rule changes cite the canonical book + page number

## References

<!-- Cite WRB 2022 / KST 13ed / SiBCS 5ª ed. pages where applicable. -->
