# How to submit soilKey to CRAN

This file documents the submission steps. CRAN submissions cannot be
automated from the package — they go through the CRAN web form
(https://cran.r-project.org/submit.html) and require the maintainer
to confirm an email link.

## Pre-flight checklist (already done as of v0.9.11)

- [x] `R CMD check --as-cran` returns 0 ERR / 0 WARN / 2 NOTE
      (both expected: "New submission" + PROJ env-only).
- [x] `cran-comments.md` drafted at the package root.
- [x] DESCRIPTION URL + BugReports populated.
- [x] License = MIT + LICENSE file in the conventional CRAN form.
- [x] All vignettes start with a letter (CRAN rule for `inst/doc`).
- [x] `tests/testthat.R` runs in < 5 minutes on CI.
- [x] R CMD check passes on the GitHub Actions matrix
      (Linux R-release / R-devel / R-oldrel-1; macOS R-release;
      Windows R-release).

## Submission steps

1. **Tag the release** (already done for v0.9.10 and v0.9.11).

2. **Build the source tarball** at the tag:
   ```bash
   cd ..
   R CMD build soilKey
   # produces soilKey_0.9.11.tar.gz (or the latest version)
   ```

3. **Run final CRAN check** on the tarball:
   ```bash
   _R_CHECK_FORCE_SUGGESTS_=false \
   R CMD check --as-cran --no-manual soilKey_0.9.11.tar.gz
   ```
   Confirm: `Status: 2 NOTEs` (or fewer) -- the only acceptable
   NOTEs are "New submission" and the PROJ environmental NOTE
   (which CRAN's farm doesn't have).

4. **Upload via the CRAN web form**:
   https://xmpalantir.wu.ac.at/cransubmit/

   - Name:       Hugo Rodrigues
   - Email:      rodrigues.machado.hugo@gmail.com
   - Package:    soilKey_0.9.11.tar.gz
   - Comments:   paste the contents of `cran-comments.md`.

5. **Confirm via email link** -- CRAN sends a confirmation link to
   the maintainer email. Click it within 24 h.

6. **Wait for review** -- typically 1-7 days for first submissions.
   CRAN reviewers may request changes; if so, address them and
   re-submit (incrementing version, e.g. 0.9.11 -> 0.9.12).

## After acceptance

Once accepted, CRAN posts the package at
https://cran.r-project.org/package=soilKey. The Zenodo concept-DOI
(10.5281/zenodo.19930112) and the CRAN page are independent: both
remain canonical citation targets, and `citation("soilKey")` keeps
returning the Zenodo DOI as the primary handle.

## Common reviewer requests (anticipate)

CRAN reviewers tend to request:

- **Examples wrapped in `\dontrun{}`** if they require API keys or
  network -- already done across `extract_*` family,
  `download_ossl_subset()`, `run_wosis_benchmark*()`.
- **Tests skipped on CRAN** if they require optional deps -- already
  done with `skip_if_not_installed(...)` for terra / prospectr /
  ellmer / jsonvalidate / pdftools.
- **No `<<-` global modification** -- soilKey uses only function-
  scoped state plus R6 instance state, no globals.
- **No writing outside `tempdir()`** in examples -- verified.

If reviewers find anything else, fix and resubmit.

## Resubmission template

```
Dear CRAN maintainers,

Thanks for the review. Here is a revised submission addressing the
points raised:

  - <point 1>: <fix>
  - <point 2>: <fix>

Tested on the same matrix as the previous submission; R CMD check
status unchanged (0 ERR / 0 WARN / 1 NOTE -- New submission).

Best,
Hugo Rodrigues
```
