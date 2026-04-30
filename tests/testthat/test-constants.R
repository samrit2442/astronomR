test_that("constants_df has expected dimensions and columns", {
  expect_equal(nrow(constants_df), 19)
  expect_named(constants_df, c("name", "symbol", "value_SI", "unit_SI",
                               "value_Natural", "unit_Natural"))
})

test_that("constant_value returns correct SI value for speed of light", {
  result <- constant_value("speed of light", unit = "SI")
  expect_equal(result$value, 299792458)
  expect_equal(result$unit, "m/s")
})

test_that("constant_value returns correct Natural value for electron mass", {
  result <- constant_value("electron mass", unit = "Natural")
  expect_equal(result$value, 5.10999e5)
  expect_equal(result$unit, "eV")
})

test_that("constant_value errors on unknown constant name", {
  expect_error(constant_value("dark matter"))
})

test_that("constant_value errors on ambiguous name", {
  expect_error(constant_value("constant"))
})

test_that("constant_value errors on invalid unit", {
  expect_error(constant_value("planck", unit = "CGS"))
})
