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
