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
  from *"effectively zero"* (N < 0.001) to *"galaxy teeming with life"*
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
* Early-universe thermal physics functions: `photon_energy_density_fn_T()`, `photon_energy_density_fn_z()`, `photon_number_density_fn_T()`, `photon_number_density_fn_z()`, `Saha_Xe()`, and `soln_saha()`.
