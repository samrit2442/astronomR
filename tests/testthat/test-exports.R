# testthat runs inside the package namespace, so a missing @export would not
# make any other test fail. These tests pin down the public API.

test_that("the exported API is exactly the documented set", {
  expected <- c(
    "Mpc_to_km", "Saha_Xe", "age_of_universe", "angular_diameter_distance",
    "boltzmann_pebble_rhs", "comoving_distance", "constant_value",
    "constants_df", "cosmic_time", "cosmology_model", "deg_to_dms",
    "deg_to_hms", "deg_to_rad", "dms_to_deg", "drake_equation",
    "entropy_density", "equilibrium_number_density", "equilibrium_yield",
    "freeze_out_xf", "g_star_eff", "get_gaia_data", "hms_to_deg",
    "hubble_distance", "hubble_radiation", "hubble_ratio", "julian_day",
    "km_to_Mpc", "luminosity_distance", "peebles_rhs",
    "photon_energy_density_fn_T", "photon_energy_density_fn_z",
    "photon_number_density_fn_T", "photon_number_density_fn_z",
    "rad_to_deg", "radec_to_altaz", "radius_of_curvature",
    "redshift_to_scale_factor", "scale_factor_to_redshift", "soln_saha",
    "solve_relic_abundance", "transverse_comoving_distance"
  )
  expect_setequal(getNamespaceExports("astronomR"), expected)
})

test_that("removed and internal names are not exported", {
  exports <- getNamespaceExports("astronomR")
  expect_false(any(c("deg2rad", "rad2deg") %in% exports))
  expect_false(any(c("RA_dec2Alt_azi", "FlatLCDM", "ClosedLCDM", "OpenLCDM",
                     "t_as_func_a_in_Year", "a", "redshift") %in% exports))
})

test_that("the cosmology print method is registered", {
  s3 <- getNamespaceInfo("astronomR", "S3methods")
  expect_true(any(s3[, 1] == "print" & s3[, 2] == "astronomR_cosmology"))
})

test_that("tibble is imported, so constants_df behaves as a tibble", {
  expect_true("tibble" %in% names(getNamespaceImports("astronomR")))
})
