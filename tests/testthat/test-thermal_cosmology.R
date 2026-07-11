# tests/testthat/test-thermal_cosmology.R
# Tests for 08_thermal_cosmology.R  (astronomR 0.3.0)

# ---- 1. hubble_radiation -----------------------------------------------
test_that("hubble_radiation returns positive value", {
  expect_gt(hubble_radiation(100), 0)
  expect_gt(hubble_radiation(1e-3, g_star = 10.75), 0)
})

test_that("hubble_radiation scales as T^2", {
  # H(T) propto T^2, so H(2T) / H(T) == 4
  ratio <- hubble_radiation(200) / hubble_radiation(100)
  expect_equal(ratio, 4, tolerance = 1e-10)
})

test_that("hubble_radiation errors on non-positive inputs", {
  expect_error(hubble_radiation(-1))
  expect_error(hubble_radiation(100, g_star = 0))
})

# ---- 2. g_star_eff --------------------------------------------------------
test_that("g_star_eff returns correct SM plateau values", {
  expect_equal(g_star_eff(500),  106.75)   # full SM
  expect_equal(g_star_eff(250),   96.25)
  expect_equal(g_star_eff(150),   86.25)
  expect_equal(g_star_eff(1),     75.75)   # above QCD
  expect_equal(g_star_eff(0.3),   17.25)
  expect_equal(g_star_eff(0.1),   10.75)   # neutrino era
  expect_equal(g_star_eff(0.01),   3.91)   # post e+e- ann.
})

test_that("g_star_eff is non-increasing with decreasing T", {
  temps <- c(1000, 500, 200, 100, 1, 0.3, 0.1, 0.01)
  gvals <- sapply(temps, g_star_eff)
  expect_true(all(diff(gvals) <= 0))
})

# ---- 3. entropy_density ---------------------------------------------------
test_that("entropy_density is positive and scales as T^3", {
  expect_gt(entropy_density(1), 0)
  ratio <- entropy_density(2, g_star_S = 106.75) /
           entropy_density(1, g_star_S = 106.75)
  expect_equal(ratio, 8, tolerance = 1e-10)
})

test_that("entropy_density errors on non-positive T", {
  expect_error(entropy_density(-0.1))
})

test_that("entropy_density uses g_star_eff when g_star_S is NULL", {
  manual <- (2 * pi^2 / 45) * g_star_eff(1) * 1^3
  expect_equal(entropy_density(1), manual, tolerance = 1e-10)
})

# ---- 4. equilibrium_number_density ----------------------------------------
test_that("equilibrium_number_density is positive", {
  expect_gt(equilibrium_number_density(1, 10, 2), 0)
})

test_that("equilibrium_number_density is Boltzmann suppressed", {
  # Higher x = m/T -> more suppression
  n_hot  <- equilibrium_number_density(T_GeV = 10, m_GeV = 10)
  n_cold <- equilibrium_number_density(T_GeV = 1,  m_GeV = 10)
  expect_gt(n_hot, n_cold)
})

test_that("equilibrium_number_density errors on bad inputs", {
  expect_error(equilibrium_number_density(-1, 10))
  expect_error(equilibrium_number_density(1, -10))
})

# ---- 5. equilibrium_yield -------------------------------------------------
test_that("equilibrium_yield is positive for positive x", {
  expect_gt(equilibrium_yield(3, 100), 0)
})

test_that("equilibrium_yield decreases monotonically with x", {
  xs   <- c(1, 3, 10, 20, 30)
  Yeqs <- sapply(xs, function(x) equilibrium_yield(x, 100))
  expect_true(all(diff(Yeqs) < 0))
})

test_that("equilibrium_yield errors on non-positive x or m", {
  expect_error(equilibrium_yield(0, 100))
  expect_error(equilibrium_yield(3, 0))
})

# ---- 6. boltzmann_pebble_rhs ----------------------------------------------
test_that("boltzmann_pebble_rhs returns zero at exact equilibrium", {
  sv  <- 2.2e-9
  Yeq <- equilibrium_yield(20, 100)
  dY  <- boltzmann_pebble_rhs(20, Yeq, 100, sv)
  expect_equal(dY, 0, tolerance = 1e-30)
})

test_that("boltzmann_pebble_rhs is negative when Y > Y_eq", {
  sv  <- 2.2e-9
  Yeq <- equilibrium_yield(20, 100)
  dY  <- boltzmann_pebble_rhs(20, 10 * Yeq, 100, sv)
  expect_lt(dY, 0)
})

test_that("boltzmann_pebble_rhs errors on non-positive x", {
  expect_error(boltzmann_pebble_rhs(0, 1e-10, 100, 2.2e-9))
})

# ---- 7. solve_relic_abundance ---------------------------------------------
test_that("solve_relic_abundance returns a list with expected names", {
  skip_if_not_installed("deSolve")
  res <- suppressWarnings(
    solve_relic_abundance(100, 2.2e-9, x_ini = 5, x_fin = 100, n_steps = 200)
  )
  expect_named(res, c("x", "Y", "Y_eq", "Omega_h2"))
})

test_that("solve_relic_abundance Y is positive", {
  skip_if_not_installed("deSolve")
  res <- suppressWarnings(
    solve_relic_abundance(100, 2.2e-9, x_ini = 5, x_fin = 100, n_steps = 200)
  )
  expect_true(all(res$Y > 0))
})

test_that("solve_relic_abundance Omega_h2 is in plausible WIMP range", {
  skip_if_not_installed("deSolve")
  res <- suppressWarnings(
    solve_relic_abundance(100, 2.2e-9, x_ini = 5, x_fin = 500, n_steps = 1000)
  )
  # Planck 2018: Omega_DM h^2 ~ 0.120; typical WIMP range 0.01 - 1
  expect_gt(res$Omega_h2, 0.01)
  expect_lt(res$Omega_h2, 2)
})

# ---- 8. freeze_out_xf -----------------------------------------------------
test_that("freeze_out_xf returns value in classic WIMP range 15-35", {
  xf <- freeze_out_xf(100, 2.2e-9)
  expect_gt(xf, 15)
  expect_lt(xf, 35)
})

test_that("freeze_out_xf increases with particle mass", {
  xf1 <- freeze_out_xf(10,   2.2e-9)
  xf2 <- freeze_out_xf(100,  2.2e-9)
  xf3 <- freeze_out_xf(1000, 2.2e-9)
  expect_lt(xf1, xf2)
  expect_lt(xf2, xf3)
})

test_that("freeze_out_xf errors on non-positive inputs", {
  expect_error(freeze_out_xf(-1,  2.2e-9))
  expect_error(freeze_out_xf(100, 0))
})

# ---- 9. peebles_rhs -------------------------------------------------------
test_that("peebles_rhs returns named vector with three elements", {
  res <- peebles_rhs(
    lna = log(1 / 1101), xe = 0.5,
    H = 3.3e-15, n_H = 400e6,
    alpha_B = 2.6e-19, beta_B = 4e-15
  )
  expect_named(res, c("dxe_dlna", "C", "lambda_alpha_escape"))
})

test_that("peebles_rhs C-factor is between 0 and 1", {
  res <- peebles_rhs(
    lna = log(1 / 1101), xe = 0.5,
    H = 3.3e-15, n_H = 400e6,
    alpha_B = 2.6e-19, beta_B = 4e-15
  )
  expect_gte(res["C"], 0)
  expect_lte(res["C"], 1)
})

test_that("peebles_rhs dxe_dlna is negative for high xe (recombination)", {
  # At xe = 0.9, recombination dominates: dxe/dlna < 0
  res <- peebles_rhs(
    lna = log(1 / 1101), xe = 0.9,
    H = 3.3e-15, n_H = 400e6,
    alpha_B = 2.6e-19, beta_B = 4e-15
  )
  expect_lt(res["dxe_dlna"], 0)
})

test_that("peebles_rhs lambda_alpha_escape is positive", {
  res <- peebles_rhs(
    lna = log(1 / 1101), xe = 0.5,
    H = 3.3e-15, n_H = 400e6,
    alpha_B = 2.6e-19, beta_B = 4e-15
  )
  expect_gt(res["lambda_alpha_escape"], 0)
})

test_that("peebles_rhs stopifnot catches invalid xe", {
  expect_error(peebles_rhs(0, xe = -0.1, H = 3.3e-15, n_H = 400e6,
                            alpha_B = 2.6e-19, beta_B = 4e-15))
  expect_error(peebles_rhs(0, xe = 1.1,  H = 3.3e-15, n_H = 400e6,
                            alpha_B = 2.6e-19, beta_B = 4e-15))
})

test_that("peebles_rhs stopifnot catches non-positive H or n_H", {
  expect_error(peebles_rhs(0, xe = 0.5, H = 0,    n_H = 400e6,
                            alpha_B = 2.6e-19, beta_B = 4e-15))
  expect_error(peebles_rhs(0, xe = 0.5, H = 3.3e-15, n_H = 0,
                            alpha_B = 2.6e-19, beta_B = 4e-15))
})
