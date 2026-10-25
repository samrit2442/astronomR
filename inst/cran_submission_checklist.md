CRAN submission checklist for astronomR v0.4.0

1. Run full package checks
   - devtools::document()
   - Sys.setenv(R_CHECK_CRAN_INCOMING = "TRUE")
   - devtools::check()
   - Address any ERROR/WARNING/NOTE.

2. Examples and tests
   - Ensure examples run quickly; wrap network or long-running examples with \donttest{} or skip_on_cran().
   - Confirm tests pass under testthat edition 3.

3. Metadata
   - AUTHORS/Authors@R includes a valid maintainer with email (done).
   - DESCRIPTION fields: Title, Description, License, URL, BugReports set (done).
   - License file present (LICENSE) and matches DESCRIPTION.

4. Documentation
   - All exported functions have Rd topics and examples (checked).
   - Rebuild NAMESPACE via devtools::document().
   - Update pkgdown site if present (pkgdown::build_site()).

5. NEWS and version
   - NEWS.md contains entry for 0.4.0 (done).
   - Version in DESCRIPTION matches NEWS (0.4.0).

6. Data and inst/
   - Remove unused large data files; document any data in man/.

7. Vignettes
   - Ensure vignettes build without external network calls.

8. CRAN policies
   - No hard-coded external credentials.
   - No examples that modify user files outside tempdir().

9. Final submission
   - Build tarball: R CMD build .
   - Submit via devtools::release() or CRAN submission page.

Notes:
- This environment cannot perform outbound network operations; run final devtools::check() on a machine with network access to validate CRAN incoming checks (virus scan, reverse dep checks if applicable).
