## R CMD check results (astronomR 0.3.0)

0 errors | 0 warnings | 0 notes

* Checked on Windows 11 x64 (local R 4.6.0 ucrt, `devtools::check(cran = TRUE)`).
* All 47 unit tests pass (`FAIL 0 | WARN 0 | SKIP 0 | PASS 47`).
* The `get_gaia_data()` function requires an internet connection; its example
  is wrapped in `\donttest{}`.
* `solve_relic_abundance()` optionally uses the `deSolve` package (listed in
  `Suggests`); the function gracefully stops with an informative message if
  `deSolve` is not installed.

## Changes since last submission (0.2.0 -> 0.3.0)

### New features
* Added `08_thermal_cosmology.R` with nine new exported functions covering the
  thermal history of the early universe:
  `hubble_radiation()`, `g_star_eff()`, `entropy_density()`,
  `equilibrium_number_density()`, `equilibrium_yield()`,
  `boltzmann_pebble_rhs()`, `solve_relic_abundance()`, `freeze_out_xf()`,
  and `peebles_rhs()`.

### Bug fixes / policy compliance
* Fixed pre-existing `@value` roxygen2 tag (now `@return`) in `constants_df`
  documentation (`00_constants.R`).
* Replaced non-ASCII `≈` characters in `07_drake_equation.R` with the
  portable `\u2248` R unicode escape.
* Added `.posit` and `.vscode` to `.Rbuildignore` to exclude IDE configuration
  directories from the package bundle.
* Added `deSolve` to `Suggests` for the `solve_relic_abundance()` ODE solver.
* `NAMESPACE` regenerated via `devtools::document()` to export all nine new
  functions.
