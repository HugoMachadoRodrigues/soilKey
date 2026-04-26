# soilKey

> Automated soil profile classification per WRB 2022 (4th ed.) and SiBCS (5th ed.), with multimodal extraction, spatial priors, and OSSL-based attribute prediction.

`soilKey` is an R package for end-to-end pedological classification of soil profiles. It separates concerns strictly:

1. A **deterministic taxonomic key** consumes a structured profile and returns a classified result with a full decision trace.
2. **Vision-language extraction** transforms field reports and photos into structured data validated by JSON schemas — never into a class.
3. **Spatial priors** from SoilGrids and national soil maps provide tie-breaking and sanity checks, never override the key.
4. **Spectroscopy via OSSL** fills missing soil attributes with explicit prediction intervals; the resulting evidence grade reflects the provenance of every value used by the key.

Every attribute carries a provenance tag (`measured`, `extracted_vlm`, `predicted_spectra`, `inferred_prior`, `user_assumed`) and the final classification carries an evidence grade (A/B/C/D) summarizing the proveniências de seus atributos críticos.

## Status

This is **v0.1** — a proof-of-concept covering:

- `PedonRecord`, `DiagnosticResult`, and `ClassificationResult` core classes
- Three WRB 2022 diagnostic horizons (argic, ferralic, mollic)
- The Ferralsols path of the WRB 2022 key, end-to-end
- A canonical Ferralsol fixture (Brazilian Latossolo Vermelho) and tests
- The YAML rule engine that consumes `inst/rules/wrb2022/key.yaml`

Subsequent versions will add the remaining 29 RSGs, all 202 qualifiers, the SiBCS key, the multimodal-extraction layer, the spatial-prior layer, and the OSSL spectroscopy bridge. See `ARCHITECTURE.md` for the full design and `NEWS.md` for per-release scope.

## Installation

```r
# install.packages("remotes")
remotes::install_local("path/to/soilKey")
```

## Quick start

```r
library(soilKey)

# Build the canonical Ferralsol fixture
pedon <- make_ferralsol_canonical()

# Inspect
print(pedon)

# Test individual diagnostics
ferralic(pedon)
argic(pedon)
mollic(pedon)

# Run the WRB 2022 key
result <- classify_wrb2022(pedon)
print(result)
#> ClassificationResult (WRB 2022)
#>   Name: Ferralsol
#>   Evidence grade: A
#>   Key trace (8 RSGs tested before assignment):
#>     HS — NA (histic_horizon not implemented in v0.1)
#>     ...
#>     FR — PASSED (ferralic horizon present, no argic, no albeluvic)
```

## Citing

If `soilKey` contributes to your work, please cite:

> Rodrigues Machado, H. (2026). soilKey: Automated soil profile classification per WRB 2022 and SiBCS. R package version 0.1.0.

## Acknowledgements

Builds on `aqp` (Beaudette et al., USDA-NRCS), `SoilTaxonomy` (Beaudette et al.), the Open Soil Spectral Library (Soil Spectroscopy for Global Good consortium), and SoilGrids (ISRIC). Architecture decisions are documented in `ARCHITECTURE.md`.

## License

GPL (>= 3).
