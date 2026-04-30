# soilKey: Automated Soil Profile Classification per WRB 2022 and SiBCS

soilKey implements deterministic classification keys for the World
Reference Base for Soil Resources 2022 (4th edition) and the Brazilian
System of Soil Classification (SiBCS, 5th edition). It separates
concerns strictly: the taxonomic key is a pure function of structured
profile data, while optional modules provide vision-language extraction,
spatial priors from SoilGrids, and gap-filling of soil attributes from
Vis-NIR or MIR spectra via the Open Soil Spectral Library (OSSL).

## Design principle

never delegate the key. Vision-language models are restricted to
schema-validated extraction of soil attributes from unstructured sources
(PDFs, photos, field sheets). The taxonomic key itself is always
evaluated by deterministic R code driven by versioned YAML rules.

## Core types

- [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
  — site, horizons, spectra, images, documents, and a per-attribute
  provenance log.

- [`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
  — return type of every diagnostic function (e.g.
  [`argic`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md),
  [`ferralic`](https://hugomachadorodrigues.github.io/soilKey/reference/ferralic.md),
  [`mollic`](https://hugomachadorodrigues.github.io/soilKey/reference/mollic.md));
  always carries the sub-test evidence and missing-attribute report
  alongside the boolean.

- [`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md)
  — return type of
  [`classify_wrb2022`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_wrb2022.md);
  carries the full key trace, ambiguities, missing-data hints, and a
  provenance-aware evidence grade.

## Provenance and evidence grade

Every attribute used by the key carries a provenance tag from
`c("measured", "extracted_vlm", "predicted_spectra", "inferred_prior", "user_assumed")`.
The final classification evidence grade is one of
`c("A", "B", "C", "D")` where A is fully laboratory-measured and
unambiguous and D is tentative or multimodal.

## v0.1 scope

v0.1 implements three WRB 2022 horizon diagnostics — argic, ferralic,
mollic — and the Ferralsols path of the WRB key end-to-end. The full
32-RSG key, 202 qualifiers, the SiBCS key, and the multimodal
extraction, spatial-prior, and OSSL-spectroscopy modules are scheduled
for subsequent releases. See `ARCHITECTURE.md`.

## References

IUSS Working Group WRB (2022). *World Reference Base for Soil
Resources*, 4th edition. International Union of Soil Sciences, Vienna.

Embrapa (2018). *Sistema Brasileiro de Classificação de Solos*, 5ª
edição. Embrapa Solos, Brasília.

Beaudette, D. E., Roudier, P., & O'Geen, A. T. (2013). Algorithms for
Quantitative Pedology: A toolkit for soil scientists. *Computers &
Geosciences*, 52, 258–268.

## See also

Useful links:

- <https://github.com/HugoMachadoRodrigues/soilKey>

- <https://hugomachadorodrigues.github.io/soilKey/>

- Report bugs at
  <https://github.com/HugoMachadoRodrigues/soilKey/issues>

## Author

**Maintainer**: Hugo Rodrigues <rodrigues.machado.hugo@gmail.com>
([ORCID](https://orcid.org/0000-0002-8070-8126))
