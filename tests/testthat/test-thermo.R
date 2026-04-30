test_that("photon_energy_density_fn_T is positive for positive T", {
  expect_gt(photon_energy_density_fn_T(1, "eV"), 0)
  expect_gt(photon_energy_density_fn_T(300, "K"), 0)
})

test_that("photon_number_density_fn_T is positive for positive T", {
  expect_gt(photon_number_density_fn_T(1, "eV"), 0)
  expect_gt(photon_number_density_fn_T(300, "K"), 0)
})

test_that("photon_energy_density_fn_T errors on invalid unit", {
  expect_error(photon_energy_density_fn_T(1, "J"))
})

test_that("photon_number_density_fn_T errors on invalid unit", {
  expect_error(photon_number_density_fn_T(1, "J"))
})

test_that("photon_energy_density_fn_z is positive for positive redshift", {
  expect_gt(photon_energy_density_fn_z(1300), 0)
})

test_that("photon_number_density_fn_z is positive for positive redshift", {
  expect_gt(photon_number_density_fn_z(1300), 0)
})

test_that("Saha_Xe returns value between 0 and 1", {
  xe <- Saha_Xe(1300)
  expect_gte(xe, 0)
  expect_lte(xe, 1)
})

test_that("soln_saha returns approximately zero near recombination redshift", {
  # Recombination happens around z ~ 1100-1200
  expect_lt(abs(soln_saha(1150)), 0.5)
})
