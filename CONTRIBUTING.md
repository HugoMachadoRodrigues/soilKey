# Contributing to soilKey

Thank you for considering contributing to soilKey. This document
explains how to file an issue, develop a change, and submit a pull
request.

## Architecture invariants (please read first)

soilKey enforces three architectural invariants. Contributions must
not violate them:

1. **The taxonomic key is never delegated to a language model.**
   LLMs are restricted to schema-validated extraction (Module 2).
   Every classification is a deterministic walk through versioned
   YAML rules with a full decision trace.
2. **Every value carries a provenance tag.** When a contribution
   adds a new attribute or workflow, it must populate
   `pedon$provenance` with `attribute`, `source`, `confidence`, and
   `notes`.
3. **Side modules never overrule the key.** Spatial priors flag
   inconsistencies but cannot silently change the assigned RSG;
   spectral predictions fill missing attributes with explicit
   confidence; multimodal extraction pulls structured data without
   writing class names.

If your change requires reviewing these invariants (e.g. you have a
strong reason to delegate part of the key to an LLM), please open a
discussion issue first to talk through the design.


## Filing an issue

Please use the issue templates at
[`.github/ISSUE_TEMPLATE/`](.github/ISSUE_TEMPLATE/):

- 🐛 **Bug report** -- a reproducible problem with classification
  output, error messages, or unexpected behaviour.
- 💡 **Feature request** -- a new diagnostic, qualifier, loader,
  or workflow.
- 🌱 **Soil profile classification help** -- you classified a real
  profile and disagree with the result; we'll trace why.

For general "how do I use soilKey for X" questions, please start a
[GitHub Discussion](https://github.com/HugoMachadoRodrigues/soilKey/discussions)
rather than opening an issue.


## Development workflow

### Setup

```r
# 1. Fork the repo and clone your fork
# 2. Install dev dependencies
install.packages(c("devtools", "roxygen2", "testthat", "pkgdown"))

# 3. Install soilKey from the local checkout
devtools::install("path/to/soilKey", dependencies = TRUE)

# 4. Run the test suite to confirm everything works
testthat::test_local()
# Expected: ~3,100+ PASS / 0 FAIL / ~10 SKIP
```

### Branching

- Branch off `main` (or `master`) for every feature / fix.
- Branch names: `feature/<short-description>` or `fix/<short>`.
- Keep branches focused: one logical change per PR.

### Code style

- Follow the existing style: 2-space indent, snake_case,
  data.table for large tables, R6 for stateful objects.
- Roxygen2 docstrings for all exported functions.
- Cite the canonical book + page number in YAML rule comments
  (e.g. `# WRB 2022 Ch 3.1.20, Salic horizon, p 49`).
- Tests live in `tests/testthat/` and use `testthat::test_that()`.

### Adding a new diagnostic / qualifier

1. Define the function in `R/diagnostics-<scope>.R` with full
   roxygen documentation including page-precise references.
2. Add unit tests in `tests/testthat/test-<scope>-<diagnostic>.R`
   covering: positive case, negative case, NA handling, edge cases.
3. Wire the function into the relevant `inst/rules/*.yaml` file.
4. Run `roxygen2::roxygenise()` to regenerate `man/*.Rd` and
   `NAMESPACE`.
5. Run the full test suite + R CMD check.
6. Update `NEWS.md` with a one-line summary of the change.

### Adding a new dataset loader

1. Define `load_<dataset>_pedons()` in `R/benchmark-<dataset>-loader.R`
   that reads the source format and returns a list of `PedonRecord`s
   with `reference_<system>` populated on `$site`.
2. Add tests using a tiny synthetic example or a `head = N` slice.
3. Document the loader's input schema, license, and download
   instructions in roxygen.
4. Add a benchmark report in `inst/benchmarks/reports/<dataset>_<DATE>.md`
   with A/B numbers vs the previous release.


## Submitting a pull request

Please use the PR template at
[`.github/PULL_REQUEST_TEMPLATE.md`](.github/PULL_REQUEST_TEMPLATE.md).
Specifically:

1. **Run the full test suite + R CMD check** before submitting.
2. **Update `NEWS.md`** with a user-facing summary.
3. **Update the relevant vignette / README / pkgdown reference**
   if user-facing API changed.
4. **Cite WRB 2022 / KST 13ed / SiBCS 5ª ed. pages** in any YAML
   rule changes.
5. Make sure CI passes (R CMD check matrix + test-coverage +
   pkgdown).


## Code of Conduct

By contributing, you agree to follow our
[Code of Conduct](CODE_OF_CONDUCT.md). In short: be kind,
be specific, and assume good faith.


## Questions?

Open a [GitHub Discussion](https://github.com/HugoMachadoRodrigues/soilKey/discussions)
or email Hugo Rodrigues at
<rodrigues.machado.hugo@gmail.com>.
