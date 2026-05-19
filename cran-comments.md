## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a resubmission addressing CRAN reviewer feedback.
* Checked on Windows (local R 4.6.0), Windows win-builder (devel and release).
* The `get_gaia_data()` function requires an internet connection; its example is
  wrapped in `\donttest{}`.

## Changes since last submission

* Removed redundant "in R" from the package Title.
* Expanded the TAP (Table Access Protocol) acronym in the Description.
* Renamed parameter `T` to `temp` in `photon_energy_density_fn_T()` and
  `photon_number_density_fn_T()` to comply with CRAN policy on T/F usage.
* Added `\value` tag to `constants_df.Rd`.
* Replaced `\dontrun{}` with `\donttest{}` in `get_gaia_data()` examples.


