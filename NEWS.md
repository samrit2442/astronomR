# astronomR 0.4.0

## Breaking changes

* `deg2rad()` and `rad2deg()` are renamed to `deg_to_rad()` and
  `rad_to_deg()`, to match the package's other converters (`deg_to_dms()`,
  `hms_to_deg()`, `km_to_Mpc()`, ...). The old names have been removed, so
  code that calls them must be updated. This also ends the clash with
  `pracma::deg2rad()` and `pracma::rad2deg()`.

## New features

* Coordinates: `julian_day()` gives the Julian Day of `POSIXct`, `POSIXlt`,
  `Date` or character (UTC) date-times, and `radec_to_altaz()` converts right
  ascension and declination to altitude and azimuth for an observer at a given
  place and time. Both are vectorised. `radec_to_altaz()` replaces the
  unexported `RA_dec2Alt_azi()` that the README used, returns a data frame
  (`res$altitude` and `res$azimuth` still work) and no longer gives a `NaN`
  azimuth for objects on the meridian.

* Cosmology: `cosmology_model()` creates a Lambda-CDM model (with a `print()`
  method) that is passed as `cosmo` to `age_of_universe()`, `cosmic_time()`,
  `comoving_distance()`, `transverse_comoving_distance()`,
  `luminosity_distance()`, `angular_diameter_distance()`,
  `radius_of_curvature()`, `hubble_distance()` and `hubble_ratio()`. Distances
  are in Mpc by default (`unit = "km"` is available) and times in Gyr
  (`unit = "year"`); every function is vectorised over the redshift `z`. New
  `redshift_to_scale_factor()` and `scale_factor_to_redshift()`. These replace
  the unexported `FlatLCDM()`, `ClosedLCDM()`, `OpenLCDM()`,
  `t_as_func_a_in_Year()` and related helpers shown in the old README.
  `radius_of_curvature()` is now `Inf` for a flat universe (it returned 0).

* `g_star_eff()` gains `type = c("energy", "entropy")`, because below e+e-
  annihilation g* (3.36) and g*S (3.91) differ.

* `get_gaia_data()` gains a `timeout` argument (default 60 seconds).

* New package help page, `?astronomR`.

## Bug fixes and changed results

* `deg_to_dms()` and `deg_to_hms()` no longer print 60 seconds:
  `deg_to_hms(20)` now prints `1H20M0S` (was `1H19M60S`) and
  `deg_to_dms(10.1)` prints `+10°6'0"` (was `+10°5'60"`). Printed output ends
  with a newline, seconds are never in scientific notation, an unknown `type`
  is an error and `NA` input gives `NA`.

* `dms_to_deg()` no longer returns 0 for angles under one degree
  (`dms_to_deg(0, 30, 0)` is 0.5; use `-0` for a negative sign, as in
  `dms_to_deg(-0, 30, 0)`), reads zero-padded strings correctly
  (`"12°04'05\""` is 12.06806, was 12.06667) and accepts more formats
  (`"12d34m56s"`, `"12:34:56"`, `"12°34'"`, `"12.5°"`). It is vectorised, `m`
  and `s` default to 0, and minutes or seconds outside [0, 60) are an error.

* `hms_to_deg()` reads `"3h 15m 30s"`, `"03:15:30"`, `"3h15m"` and similar
  strings, which used to give `NA` silently, and range-checks numeric input.
  Unparsable strings in either function give `NA` with a warning.

* `freeze_out_xf()` now uses the full Planck mass, for which the Kolb & Turner
  formula is defined; it used the reduced mass and underestimated x_f by 1.61.
  `freeze_out_xf(100, 2.2e-9)` is now 22.38 (was 20.81).

* `g_star_eff()` follows the Standard Model step table (Husdal 2016), with
  steps at the particle masses. Its results change, for example
  `g_star_eff(100)` is 95.25 (was 75.75) and `g_star_eff(1e-3)` is 10.75 (was
  3.91), and so do the defaults of `entropy_density()`, `equilibrium_yield()`,
  `boltzmann_pebble_rhs()` and `solve_relic_abundance()`, which now use the
  entropy g*S where appropriate. `solve_relic_abundance(100, 2.2e-9)$Omega_h2`
  is now 0.1044 (was 0.1068).

* `peebles_rhs()` now includes the Boltzmann factor exp(-E_21 / kT) on the
  photoionization term, as in Peebles (1968) and RECFAST. The temperature comes
  from `lna`, which used to be ignored, and the new `T0` argument (default
  2.7255 K). All numeric arguments must now be single numbers; a vector `n_H`
  or `H` used to give silently wrong results.

* `Saha_Xe()` no longer returns `NaN` for z <= 80 or values above 1 at high
  redshift. It now uses the NIST hydrogen ionization energy (13.598434 eV) and
  T_CMB = 2.7255 K, so `Saha_Xe(1100)` is 4.788e-3 (was 4.751e-3).

* The photon density functions use the exact Boltzmann constant and
  T_CMB = 2.7255 K (was 2.725 K), and their help pages state the units.

* `constants_df` holds CODATA 2018 values (it had CODATA 2014), including the
  exact SI values of h, e, k and N_A. The `value_Natural` column now holds
  real values in natural units (hbar = c = k_B = epsilon_0 = 1, energies in
  eV) instead of a placeholder 1, or `NA` where there is no natural-unit value.
  "Boltzman" is corrected to "Boltzmann" in two names.

* `constant_value()` finds the Boltzmann constant by its correct name (the old
  spelling still works), matches exact names and symbols before partial names
  (`constant_value("k")` used to return the Planck constant), treats its input
  as plain text rather than a regular expression, and lists the candidates
  when a name is ambiguous.

* `km_to_Mpc()` and `Mpc_to_km()` use the exact IAU 2015 parsec, which changes
  their results by about 0.016%.

* `get_gaia_data()` fails gracefully, as CRAN policy requires: if the Gaia
  Archive cannot be reached, times out, returns an HTTP error or an unreadable
  response, it gives a message and returns `NULL`. A query with no matching
  rows returns a zero-row data frame instead of an error.

* `solve_relic_abundance()` validates its inputs and stops if the ODE solver
  does not reach `x_fin` (it used to return a truncated solution). Its example
  only runs when deSolve is installed.

* `library(astronomR)` now loads tibble, so `constants_df` behaves as a tibble
  from the start.

## Other changes

* Imports: dropped dplyr, pracma, readr and tidyr; added stats; stringr must be
  at least 1.5.0. Suggests testthat (>= 3.2.0).

* Help pages: formulas are readable in console help, units are stated
  throughout, and literal markdown asterisks are gone.

* The test suite grew from 91 to over 1,500 expectations, including tests of
  the exported API.

---

# astronomR 0.3.0

## New features

* Added a full **thermal cosmology module** with
  nine new exported functions covering the complete early-universe thermal
  history:

  - `hubble_radiation()` — Hubble rate H(T) in the radiation-dominated era
    via the Friedmann equation (natural units, GeV).
  - `g_star_eff()` — step-function approximation of effective relativistic
    degrees of freedom g★(T) across Standard Model epochs.
  - `entropy_density()` — entropy density s(T) = (2π²/45) g★_S T³.
  - `equilibrium_number_density()` — Maxwell-Boltzmann equilibrium number
    density n_eq(T).
  - `equilibrium_yield()` — equilibrium yield Y_eq(x) = n_eq / s using
    the modified Bessel function K₂.
  - `boltzmann_pebble_rhs()` — the Boltzmann relic-abundance **pebble
    equation** dY/dx governing WIMP thermal freeze-out.
  - `solve_relic_abundance()` — full ODE integration of the pebble equation
    (via `deSolve`) returning Y(x) and Ω h². Uses a log(Y) substitution to
    handle stiffness.
  - `freeze_out_xf()` — iterative solver for the freeze-out parameter
    x_f = m/T_f.
  - `peebles_rhs()` — Peebles equation for hydrogen recombination: returns
    dx_e/d(ln a), the Peebles C-factor, and the Lyman-alpha escape rate
    (SI units).

## Minor changes

* `DESCRIPTION` updated to document the new thermal-cosmology and
  recombination capabilities; `deSolve` added to `Suggests`.
* `NAMESPACE` regenerated via `devtools::document()` to export all nine
  new functions.
* Version bumped to **0.3.0**.

---

# astronomR 0.2.0


## New features

* Added `drake_equation()` — computes **N**, the estimated number of active,
  communicating civilisations in the Milky Way, from the seven classic Drake
  equation parameters (`R_star`, `fp`, `ne`, `fl`, `fi`, `fc`, `L`).
  Returns a named list with the numeric result, a substituted equation string,
  all input values, and a qualitative six-tier **interpretation** of N ranging
  from *"effectively zero"* (N < 0.001) to *"galaxy teeming with life"
  (N ≥ 10 000).

## Minor changes

* `DESCRIPTION` updated to mention the Drake equation capability.
* Version bumped to **0.2.0**.

---

# astronomR 0.1.0

* Initial release of astronomR.
* Angular conversion functions: `deg_to_dms()`, `dms_to_deg()`, `deg_to_hms()`, `hms_to_deg()`, `deg2rad()`, `rad2deg()`.
* Physical constants dataset `constants_df` and retrieval function `constant_value()`.
* Gaia Archive query function `get_gaia_data()`.
* Distance conversion functions `km_to_Mpc()` and `Mpc_to_km()`.
* Early-universe thermal physics functions: `photon_energy_density_fn_T()`, `photon_energy_density_fn_z()`, `photon_number_density_fn_T()`, `photon_number_density_fn_z()`, `Saha_Xe()`, and `soln_saha()`
