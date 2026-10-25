## R CMD check results (astronomR 0.4.0)

0 errors | 0 warnings | 1 note

* Possibly misspelled words in DESCRIPTION: "comoving". This is the
  standard cosmology term (as in "comoving distance") and is spelled
  correctly.

## Test environments

* win-builder, R-devel (2026-10-24 r90762 ucrt), Windows Server 2022 x64.
* local Windows 11 x64, R 4.6.1 ucrt, with `R CMD check --as-cran` and all
  Suggests installed.

## Notes

* All tests pass (`FAIL 0 | WARN 0 | SKIP 0 | PASS 1518`).
* `get_gaia_data()` needs an internet connection; its example is wrapped in
  `\donttest{}`. If the Gaia Archive is unavailable, the function now gives an
  informative message and returns `NULL` instead of an error, as the CRAN
  policy for Internet resources requires. Its tests never use the network.
* `solve_relic_abundance()` uses deSolve, which is in Suggests. Its example
  runs only when deSolve is installed (`@examplesIf`) and its tests use
  `skip_if_not_installed("deSolve")`.

## Reverse dependencies

There are no reverse dependencies on CRAN
(`tools::package_dependencies("astronomR", reverse = TRUE, which = "all")`).

## Changes since last submission (0.3.0 -> 0.4.0)

* Breaking change: `deg2rad()` and `rad2deg()` were renamed to `deg_to_rad()`
  and `rad_to_deg()`, and the old names were removed.
* New exported functions: `julian_day()` and `radec_to_altaz()` for
  coordinates; `cosmology_model()`, `age_of_universe()`, `cosmic_time()`,
  `comoving_distance()`, `transverse_comoving_distance()`,
  `luminosity_distance()`, `angular_diameter_distance()`,
  `radius_of_curvature()`, `hubble_distance()`, `hubble_ratio()`,
  `redshift_to_scale_factor()` and `scale_factor_to_redshift()` for cosmology.
* Bug fixes in the angle conversions, `freeze_out_xf()`, `g_star_eff()`,
  `peebles_rhs()`, `Saha_Xe()` and `constant_value()`; physical constants
  updated to CODATA 2018. Some documented results change; NEWS.md lists them.
* Imports: dropped dplyr, pracma, readr and tidyr; added stats.
