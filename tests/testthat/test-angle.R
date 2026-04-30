test_that("deg2rad and rad2deg are inverses", {
  expect_equal(deg2rad(180), pi)
  expect_equal(deg2rad(0), 0)
  expect_equal(rad2deg(pi), 180)
  expect_equal(rad2deg(0), 0)
  expect_equal(rad2deg(deg2rad(45)), 45)
})

test_that("deg2rad is vectorized", {
  expect_equal(deg2rad(c(0, 90, 180)), c(0, pi / 2, pi))
})

test_that("hms_to_deg gives correct result with numeric inputs", {
  expect_equal(hms_to_deg(h = 3, m = 15, s = 30), 48.875)
  expect_equal(hms_to_deg(h = 0, m = 0, s = 0), 0)
})

test_that("hms_to_deg parses string input", {
  expect_equal(hms_to_deg(h = "03h15m30s"), 48.875)
})

test_that("dms_to_deg gives correct result with numeric inputs", {
  expect_equal(dms_to_deg(d = 12, m = 34, s = 56), 12.58222)
  expect_equal(dms_to_deg(d = 0, m = 0, s = 0), 0)
})

test_that("dms_to_deg errors on out-of-range inputs", {
  expect_error(dms_to_deg(d = 91, m = 0, s = 0))
  expect_error(dms_to_deg(d = 0, m = 60, s = 0))
})

test_that("deg_to_dms errors on out-of-range input", {
  expect_error(deg_to_dms(91))
  expect_error(deg_to_dms(-91))
})

test_that("deg_to_dms mat output has correct structure", {
  result <- deg_to_dms(45.5, type = "mat")
  expect_true(is.matrix(result))
  expect_equal(ncol(result), 4)
  expect_equal(colnames(result), c("SIGN", "DEG", "MIN", "SEC"))
})

test_that("deg_to_hms mat output has correct structure", {
  result <- deg_to_hms(45, type = "mat")
  expect_true(is.matrix(result))
  expect_equal(ncol(result), 3)
  expect_equal(colnames(result), c("HRS", "MIN", "SEC"))
})
