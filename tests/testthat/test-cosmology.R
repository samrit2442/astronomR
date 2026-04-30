test_that("km_to_Mpc and Mpc_to_km are inverses", {
  expect_equal(km_to_Mpc(Mpc_to_km(1)), 1)
  expect_equal(Mpc_to_km(km_to_Mpc(3.086e19)), 3.086e19)
})

test_that("km_to_Mpc gives approximately 1 for 1 Mpc in km", {
  one_mpc_in_km <- 3.262e6 * 9.461e12
  expect_equal(km_to_Mpc(one_mpc_in_km), 1)
})
