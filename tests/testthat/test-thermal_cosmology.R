# tests/testthat/test-thermal_cosmology.R
# Tests for R/08_thermal_cosmology.R

M_Pl_red  <- 2.435e18                 # reduced Planck mass used by the package [GeV]
hbar_c_GeV_cm <- 1.973269804e-14      # hbar c [GeV cm]
sv_wimp   <- 2.2e-9                   # typical WIMP <sigma v> [GeV^-2]

# Element-wise relative comparison. expect_equal() averages differences over
# a vector and compares absolutely for values below its tolerance, which would
# hide errors in the very small numbers (H ~ 1e-14 GeV, dY/dx ~ 1e-79) here.
expect_rel <- function(object, expected, tol) {
  expect_length(object, length(expected))
  expect_lt(max(abs(object / expected - 1)), tol)
}

# ---- 1. hubble_radiation ----------------------------------------------------

test_that("hubble_radiation equals the radiation-era Friedmann formula", {
  expect_rel(hubble_radiation(100),
             sqrt(pi^2 / 90 * 106.75) * 100^2 / 2.435e18, 1e-14)
  expect_rel(hubble_radiation(1e-3, g_star = 10.75),
             sqrt(pi^2 / 90 * 10.75) * 1e-6 / 2.435e18, 1e-14)
  # Kolb & Turner form H = 1.66 sqrt(g*) T^2 / m_Pl, m_Pl = 1.2209e19 GeV
  expect_rel(hubble_radiation(100), 1.66 * sqrt(106.75) * 1e4 / 1.2209e19, 2e-3)
})

test_that("hubble_radiation scales as T^2 sqrt(g*) and is vectorised", {
  expect_equal(hubble_radiation(200) / hubble_radiation(100), 4, tolerance = 1e-14)
  expect_equal(hubble_radiation(1, g_star = 40) / hubble_radiation(1, g_star = 10),
               2, tolerance = 1e-14)
  T_vec <- c(1e-3, 1, 100)
  expect_identical(hubble_radiation(T_vec),
                   vapply(T_vec, hubble_radiation, numeric(1)))
  out <- hubble_radiation(c(100, NA))
  expect_identical(out[1], hubble_radiation(100))
  expect_true(is.na(out[2]))
})

test_that("hubble_radiation errors on non-positive inputs", {
  expect_error(hubble_radiation(-1), "'T_GeV' must be positive")
  expect_error(hubble_radiation(c(1, 0)), "'T_GeV' must be positive")
  expect_error(hubble_radiation(100, g_star = 0), "'g_star' must be positive")
})

# ---- 2. g_star_eff ----------------------------------------------------------

# One temperature inside each bin, with the expected g_* and g_*S
g_bins <- data.frame(
  T_GeV = c(1000, 150, 100, 10, 3, 1.5, 0.5, 0.145, 0.12, 0.01, 1e-4),
  g     = c(106.75, 96.25, 95.25, 86.25, 75.75, 72.25, 61.75, 17.25, 14.25,
            10.75, 3.36),
  g_S   = c(106.75, 96.25, 95.25, 86.25, 75.75, 72.25, 61.75, 17.25, 14.25,
            10.75, 3.91)
)
# Thresholds [GeV]: top, Higgs, W/Z, bottom, tau, charm, QCD, pions, muon, e
g_edges <- c(173, 125, 80.4, 4.18, 1.777, 1.27, 0.15, 0.1396, 0.1057, 5.11e-4)

test_that("g_star_eff reproduces the Standard Model step table", {
  expect_identical(g_star_eff(g_bins$T_GeV), g_bins$g)
  expect_identical(g_star_eff(g_bins$T_GeV, type = "energy"), g_bins$g)
  expect_identical(g_star_eff(g_bins$T_GeV, type = "entropy"), g_bins$g_S)
  expect_identical(g_star_eff(100), 95.25)
  expect_identical(g_star_eff(1e-3), 10.75)
  expect_identical(g_star_eff(1e-4), 3.36)
  expect_identical(g_star_eff(1e-4, "entropy"), 3.91)
})

test_that("g_star_eff bins are half-open (lower, upper]", {
  # at a threshold the lower bin applies; just above it the upper bin
  expect_identical(g_star_eff(g_edges), g_bins$g[-1])
  expect_identical(g_star_eff(g_edges, "entropy"), g_bins$g_S[-1])
  expect_identical(g_star_eff(g_edges * (1 + 1e-9)), g_bins$g[-11])
  expect_identical(g_star_eff(g_edges * (1 + 1e-9), "entropy"), g_bins$g_S[-11])
})

test_that("g_star_eff steps match Standard Model degree-of-freedom counting", {
  f <- 7 / 8  # fermion weight
  # bosons: photon 2, gluons 16, W+- 6, Z 3, Higgs 1; fermions: quarks 72,
  # charged leptons 12, neutrinos 6 (Husdal 2016, Table 1)
  g_all   <- (2 + 16 + 6 + 3 + 1) + f * (72 + 12 + 6)
  removed <- c(top = f * 12, higgs = 1, W_Z = 9, bottom = f * 12,
               tau = f * 4, charm = f * 12)
  g_hadron <- (2 + 3) + f * (4 + 4 + 6)  # photons + 3 pions + e, mu, nu
  g_counted <- c(g_all, g_all - unname(cumsum(removed)), g_hadron,
                 g_hadron - 3, g_hadron - 3 - f * 4)
  expect_equal(g_counted, g_bins$g[1:10])
  # after e+e- annihilation neutrinos are colder by (4/11)^(1/3)
  expect_lt(abs(g_star_eff(1e-4) - (2 + f * 6 * (4 / 11)^(4 / 3))), 0.005)
  expect_lt(abs(g_star_eff(1e-4, "entropy") - (2 + f * 6 * 4 / 11)), 0.005)
  # above e+e- annihilation g_*S = g_*
  T_hot <- 10^seq(-3, 3, by = 0.25)
  expect_identical(g_star_eff(T_hot, "entropy"), g_star_eff(T_hot))
})

test_that("g_star_eff is non-increasing as T decreases", {
  T_desc <- 10^seq(3, -5, by = -0.05)
  expect_true(all(diff(g_star_eff(T_desc)) <= 0))
  expect_true(all(diff(g_star_eff(T_desc, "entropy")) <= 0))
})

test_that("g_star_eff errors on invalid input", {
  expect_error(g_star_eff("1"), "'T_GeV' must be numeric")
  expect_error(g_star_eff(TRUE), "'T_GeV' must be numeric")
  expect_error(g_star_eff(NULL), "'T_GeV' must be numeric")
  expect_error(g_star_eff(NA), "'T_GeV' must be numeric")
  expect_error(g_star_eff(NA_real_), "missing values")
  expect_error(g_star_eff(c(1, NA)), "missing values")
  expect_error(g_star_eff(NaN), "missing values")
  expect_error(g_star_eff(0), "'T_GeV' must be positive")
  expect_error(g_star_eff(c(1, -1)), "'T_GeV' must be positive")
  expect_error(g_star_eff(1, type = "number"))
})

# ---- 3. entropy_density -----------------------------------------------------

test_that("entropy_density equals (2 pi^2 / 45) g_*S T^3", {
  expect_equal(entropy_density(2, g_star_S = 106.75) /
                 entropy_density(1, g_star_S = 106.75), 8, tolerance = 1e-14)
  expect_equal(entropy_density(1), 2 * pi^2 / 45 * 61.75, tolerance = 1e-14)
  T_vec <- c(1e-4, 0.1, 1000)
  expect_rel(entropy_density(T_vec),
             2 * pi^2 / 45 * c(3.91, 10.75, 106.75) * T_vec^3, 1e-14)
})

test_that("entropy_density defaults to the entropy g_*S (3.91, not 3.36)", {
  expect_rel(entropy_density(1e-4), 2 * pi^2 / 45 * 3.91 * 1e-12, 1e-14)
  expect_identical(entropy_density(1e-4),
                   entropy_density(1e-4, g_star_S = g_star_eff(1e-4, "entropy")))
  expect_rel(entropy_density(1e-4) / entropy_density(1e-4, g_star_S = 3.36),
             3.91 / 3.36, 1e-14)
})

test_that("entropy_density reproduces today's entropy density", {
  # PDG: s_0 = 2891.2 cm^-3 for T_0 = 2.7255 K and g_*S = 3.909
  T0_GeV <- 2.7255 * 8.617333262e-5 * 1e-9
  expect_equal(entropy_density(T0_GeV) / hbar_c_GeV_cm^3, 2891.2,
               tolerance = 5e-4)
})

test_that("entropy_density handles NA and errors on non-positive T", {
  out <- entropy_density(c(1, NA), g_star_S = 10)
  expect_equal(out[1], 2 * pi^2 / 45 * 10, tolerance = 1e-14)
  expect_true(is.na(out[2]))
  # without g_star_S, g_*S(NA) is undefined
  expect_error(entropy_density(NA_real_), "missing values")
  expect_error(entropy_density(-0.1), "'T_GeV' must be positive")
  expect_error(entropy_density(c(1, 0)), "'T_GeV' must be positive")
})

# ---- 4. equilibrium_number_density ------------------------------------------

test_that("equilibrium_number_density equals g (mT / 2 pi)^(3/2) exp(-m/T)", {
  expect_equal(equilibrium_number_density(T_GeV = 5, m_GeV = 100, g_dof = 2),
               2 * (500 / (2 * pi))^1.5 * exp(-20), tolerance = 1e-14)
  expect_equal(equilibrium_number_density(5, 100, g_dof = 4) /
                 equilibrium_number_density(5, 100, g_dof = 2), 2,
               tolerance = 1e-14)
  # same x = m/T: n scales as m^3
  expect_equal(equilibrium_number_density(5, 100) /
                 equilibrium_number_density(0.5, 10), 1000, tolerance = 1e-12)
})

test_that("equilibrium_number_density is the x >> 1 limit of Maxwell-Boltzmann", {
  # n_MB = g m^2 T K_2(m/T) / (2 pi^2); K_2(x) ~ sqrt(pi / 2x) exp(-x)
  # (1 + 15/(8x) + 105/(128 x^2) + ...)  (Abramowitz & Stegun 9.7.2)
  x <- 50; m <- 100; T <- m / x
  n_MB <- 2 * m^2 * T * besselK(x, 2) / (2 * pi^2)
  expect_equal(n_MB / equilibrium_number_density(T, m),
               1 + 15 / (8 * x) + 105 / (128 * x^2), tolerance = 1e-5)
})

test_that("equilibrium_number_density is Boltzmann suppressed and vectorised", {
  n_hot  <- equilibrium_number_density(T_GeV = 10, m_GeV = 10)
  n_cold <- equilibrium_number_density(T_GeV = 1,  m_GeV = 10)
  expect_gt(n_hot, n_cold)
  out <- equilibrium_number_density(c(10, 1, NA), 10)
  expect_identical(out[1:2], c(n_hot, n_cold))
  expect_true(is.na(out[3]))
})

test_that("equilibrium_number_density errors on bad inputs", {
  expect_error(equilibrium_number_density(-1, 10), "'T_GeV' must be positive")
  expect_error(equilibrium_number_density(1, -10), "'m_GeV' must be positive")
  expect_error(equilibrium_number_density(c(1, 0), 10), "'T_GeV' must be positive")
})

# ---- 5. equilibrium_yield ---------------------------------------------------

test_that("equilibrium_yield equals 45 g / (4 pi^4 g_*S) x^2 K_2(x)", {
  # x = 20, m = 100 GeV: T = 5 GeV, g_*S = 86.25
  expect_equal(equilibrium_yield(x = 20, m_GeV = 100),
               45 * 2 / (4 * pi^4 * 86.25) * 400 * besselK(20, 2),
               tolerance = 1e-14)
  expect_equal(equilibrium_yield(3, 100, g_dof = 4, g_star_S = 100),
               45 * 4 / (4 * pi^4 * 100) * 9 * besselK(3, 2), tolerance = 1e-14)
})

test_that("equilibrium_yield defaults to the entropy g_*S", {
  # x = 10, m = 1e-3 GeV: T = 1e-4 GeV, after e+e- annihilation
  expect_equal(equilibrium_yield(10, 1e-3),
               45 * 2 / (4 * pi^4 * 3.91) * 100 * besselK(10, 2),
               tolerance = 1e-14)
  expect_equal(equilibrium_yield(10, 1e-3, g_star_S = 3.36) /
                 equilibrium_yield(10, 1e-3), 3.91 / 3.36, tolerance = 1e-14)
  # entropy type, not the energy g_* that 0.3.0 used for both
  expect_identical(equilibrium_yield(10, 1e-3),
                   equilibrium_yield(10, 1e-3,
                                     g_star_S = g_star_eff(1e-4, "entropy")))
  expect_false(identical(equilibrium_yield(10, 1e-3),
                         equilibrium_yield(10, 1e-3, g_star_S = g_star_eff(1e-4))))
})

test_that("equilibrium_yield has the right relativistic and NR limits", {
  # x -> 0: x^2 K_2(x) -> 2, so Y_eq -> 45 g / (2 pi^4 g_*S)
  expect_equal(equilibrium_yield(1e-4, 1e-8), 45 * 2 / (2 * pi^4 * 3.91),
               tolerance = 1e-6)
  # x >> 1: Y_eq = n_eq / s up to the K_2 asymptotic series
  x <- 50; m <- 100
  ratio <- equilibrium_yield(x, m, g_star_S = 86.25) /
    (equilibrium_number_density(m / x, m) / entropy_density(m / x, 86.25))
  expect_equal(ratio, 1 + 15 / (8 * x) + 105 / (128 * x^2), tolerance = 1e-5)
})

test_that("equilibrium_yield decreases with x and is vectorised", {
  xs   <- c(1, 3, 10, 20, 30)
  Yeqs <- equilibrium_yield(xs, 100)
  expect_true(all(diff(Yeqs) < 0))
  expect_identical(Yeqs, vapply(xs, equilibrium_yield, numeric(1), m_GeV = 100))
})

test_that("equilibrium_yield handles NA and errors on bad x or m", {
  out <- equilibrium_yield(c(20, NA), 100, g_star_S = 86.25)
  expect_identical(out[1], equilibrium_yield(20, 100))
  expect_true(is.na(out[2]))
  expect_error(equilibrium_yield(0, 100), "'x' must be positive")
  expect_error(equilibrium_yield(c(3, -3), 100), "'x' must be positive")
  expect_error(equilibrium_yield(3, 0), "'m_GeV' must be positive")
})

# ---- 6. boltzmann_pebble_rhs ------------------------------------------------

test_that("boltzmann_pebble_rhs is zero at equilibrium", {
  x   <- c(10, 20, 30)
  Yeq <- equilibrium_yield(x, 100)
  expect_identical(boltzmann_pebble_rhs(20, Yeq[2], 100, sv_wimp), 0)
  expect_identical(boltzmann_pebble_rhs(x, Yeq, 100, sv_wimp), c(0, 0, 0))
  expect_identical(boltzmann_pebble_rhs(20, 5 * Yeq[2], 100, 0), 0)
})

test_that("boltzmann_pebble_rhs has the restoring sign around equilibrium", {
  Yeq <- equilibrium_yield(20, 100)
  expect_lt(boltzmann_pebble_rhs(20, 10 * Yeq, 100, sv_wimp), 0)
  expect_gt(boltzmann_pebble_rhs(20, 0.1 * Yeq, 100, sv_wimp), 0)
})

test_that("boltzmann_pebble_rhs equals -(<sigma v> s / (H x)) (Y^2 - Y_eq^2)", {
  # x = 20, m = 100 GeV: T = 5 GeV, g_* = g_*S = 86.25 (hand calculation)
  s   <- 2 * pi^2 / 45 * 86.25 * 5^3
  H   <- sqrt(pi^2 / 90 * 86.25) * 5^2 / M_Pl_red
  Yeq <- 45 * 2 / (4 * pi^4 * 86.25) * 20^2 * besselK(20, 2)
  expect_equal(boltzmann_pebble_rhs(20, 10 * Yeq, 100, sv_wimp),
               -(sv_wimp * s / (H * 20)) * 99 * Yeq^2, tolerance = 1e-12)
})

test_that("boltzmann_pebble_rhs uses g_* for H and g_*S for s and Y_eq", {
  # x = 100, m = 0.01 GeV: T = 1e-4 GeV, g_* = 3.36 but g_*S = 3.91
  s   <- 2 * pi^2 / 45 * 3.91 * 1e-12
  H   <- sqrt(pi^2 / 90 * 3.36) * 1e-8 / M_Pl_red
  Yeq <- 45 * 2 / (4 * pi^4 * 3.91) * 100^2 * besselK(100, 2)
  expected <- -(sv_wimp * s / (H * 100)) * 3 * Yeq^2
  expect_rel(boltzmann_pebble_rhs(100, 2 * Yeq, 0.01, sv_wimp), expected, 1e-12)
  expect_rel(boltzmann_pebble_rhs(100, 2 * Yeq, 0.01, sv_wimp,
                                  g_star_val = 3.36, g_star_S_val = 3.91),
             expected, 1e-12)
  # the 0.3.0 default (g_*S = g_*) gives a different rate
  old <- boltzmann_pebble_rhs(100, 2 * Yeq, 0.01, sv_wimp, g_star_S_val = 3.36)
  expect_gt(abs(old / expected - 1), 0.1)
})

test_that("boltzmann_pebble_rhs is vectorised and handles NA", {
  x <- c(10, 20, 30)
  Y <- 2 * equilibrium_yield(x, 100)
  expect_identical(boltzmann_pebble_rhs(x, Y, 100, sv_wimp),
                   mapply(boltzmann_pebble_rhs, x, Y,
                          MoreArgs = list(m_GeV = 100, sigmav_GeV2 = sv_wimp)))
  out <- boltzmann_pebble_rhs(c(20, NA), 1e-9, 100, sv_wimp,
                              g_star_val = 86.25, g_star_S_val = 86.25)
  expect_identical(out[1], boltzmann_pebble_rhs(20, 1e-9, 100, sv_wimp))
  expect_true(is.na(out[2]))
  expect_error(boltzmann_pebble_rhs(c(20, NA), 1e-9, 100, sv_wimp),
               "missing values")
})

test_that("boltzmann_pebble_rhs errors on invalid inputs (vector-safe)", {
  expect_error(boltzmann_pebble_rhs(0, 1e-10, 100, sv_wimp), "'x' must be positive")
  expect_error(boltzmann_pebble_rhs(c(10, -1), 1e-10, 100, sv_wimp),
               "'x' must be positive")
  expect_error(boltzmann_pebble_rhs(20, 1e-10, 0, sv_wimp),
               "'m_GeV' must be positive")
  expect_error(boltzmann_pebble_rhs(20, 1e-10, c(100, -100), sv_wimp),
               "'m_GeV' must be positive")
  expect_error(boltzmann_pebble_rhs(20, 1e-10, 100, -1e-9),
               "'sigmav_GeV2' must be non-negative")
  expect_error(boltzmann_pebble_rhs(c(10, 20), 1e-10, 100, c(1e-9, -1e-9)),
               "'sigmav_GeV2' must be non-negative")
})

# ---- 7. solve_relic_abundance -----------------------------------------------

test_that("solve_relic_abundance validates its inputs", {
  expect_error(solve_relic_abundance(-1, sv_wimp), "'m_GeV' must be")
  expect_error(solve_relic_abundance(c(10, 100), sv_wimp), "'m_GeV' must be")
  expect_error(solve_relic_abundance(NA_real_, sv_wimp), "'m_GeV' must be")
  expect_error(solve_relic_abundance(100, 0), "'sigmav_GeV2' must be")
  expect_error(solve_relic_abundance(100, -sv_wimp), "'sigmav_GeV2' must be")
  expect_error(solve_relic_abundance(100, "2.2e-9"), "'sigmav_GeV2' must be")
  expect_error(solve_relic_abundance(100, sv_wimp, g_dof = 0), "'g_dof' must be")
  expect_error(solve_relic_abundance(100, sv_wimp, x_ini = 0), "'x_ini' must be")
  expect_error(solve_relic_abundance(100, sv_wimp, x_ini = -5), "'x_ini' must be")
  expect_error(solve_relic_abundance(100, sv_wimp, x_ini = 50, x_fin = 50),
               "'x_fin' must be a single number greater than 'x_ini'")
  expect_error(solve_relic_abundance(100, sv_wimp, x_ini = 50, x_fin = 20),
               "'x_fin' must be a single number greater than 'x_ini'")
  expect_error(solve_relic_abundance(100, sv_wimp, x_fin = Inf), "'x_fin' must be")
  expect_error(solve_relic_abundance(100, sv_wimp, n_steps = 1), "'n_steps' must be")
  expect_error(solve_relic_abundance(100, sv_wimp, n_steps = c(10, 20)),
               "'n_steps' must be")
})

test_that("solve_relic_abundance needs deSolve", {
  local_mocked_bindings(.relic_has_deSolve = function() FALSE)
  expect_error(solve_relic_abundance(100, sv_wimp),
               "Package 'deSolve' is required")
})

test_that("solve_relic_abundance stops if the ODE solver does not reach x_fin", {
  local_mocked_bindings(
    .relic_has_deSolve = function() TRUE,
    # mimic deSolve returning early (as it does after an integration failure)
    .relic_ode = function(y, times, ...) {
      cbind(time = times[1:3], W = rep(unname(y), 3))
    }
  )
  expect_error(solve_relic_abundance(100, sv_wimp),
               "stopped at x = .* before reaching 'x_fin' = 1000")
})

test_that("solve_relic_abundance stops on a non-finite ODE solution", {
  local_mocked_bindings(
    .relic_has_deSolve = function() TRUE,
    .relic_ode = function(y, times, ...) {
      cbind(time = times, W = c(rep(unname(y), length(times) - 1), NaN))
    }
  )
  expect_error(solve_relic_abundance(100, sv_wimp), "non-finite")
})

test_that("solve_relic_abundance integrates from equilibrium to x_fin", {
  skip_if_not_installed("deSolve")
  expect_no_warning(res <- solve_relic_abundance(100, sv_wimp))
  expect_named(res, c("x", "Y", "Y_eq", "Omega_h2"))
  expect_length(res$x, 2000)
  expect_identical(res$x[1], 5)
  expect_identical(res$x[2000], 1000)
  expect_true(all(is.finite(res$Y) & res$Y > 0))
  # starts in equilibrium and still tracks it closely at x ~ 10
  expect_equal(res$Y[1], res$Y_eq[1], tolerance = 1e-12)
  i10 <- which.min(abs(res$x - 10))
  expect_lt(abs(res$Y[i10] / res$Y_eq[i10] - 1), 1e-4)
  # Y_eq column is the package's equilibrium yield on the same grid
  expect_identical(res$Y_eq, equilibrium_yield(res$x, 100))
  # frozen out: Y far above Y_eq by x = 100, and Y only decreases
  i100 <- which.min(abs(res$x - 100))
  expect_gt(res$Y[i100] / res$Y_eq[i100], 1e10)
  expect_true(all(diff(res$Y) < 0))
})

test_that("solve_relic_abundance relic density (regression and analytic check)", {
  skip_if_not_installed("deSolve")
  res <- solve_relic_abundance(100, sv_wimp)
  expect_equal(res$Omega_h2, 2.755e8 * 100 * res$Y[length(res$Y)])
  # regression value for m = 100 GeV, <sigma v> = 2.2e-9 GeV^-2 (defaults)
  expect_equal(res$Omega_h2, 0.104442, tolerance = 1e-4)
  # Kolb & Turner (5.45): Y_inf = sqrt(45 / pi) x_f sqrt(g*) / (g_*S m_Pl m
  # <sigma v>), with g* = g_*S = 86.25 and the full Planck mass
  m_Pl <- M_Pl_red * sqrt(8 * pi)
  xf   <- freeze_out_xf(100, sv_wimp, g_star = 86.25)
  Y_kt <- sqrt(45 / pi) * xf * sqrt(86.25) / (86.25 * m_Pl * 100 * sv_wimp)
  expect_equal(res$Omega_h2, 2.755e8 * 100 * Y_kt, tolerance = 0.2)
})

test_that("ODE freeze-out (Y = 1.5 Y_eq) agrees with freeze_out_xf()", {
  skip_if_not_installed("deSolve")
  res   <- solve_relic_abundance(100, sv_wimp)
  ratio <- res$Y / res$Y_eq
  keep  <- ratio > 1.1 & ratio < 3
  x_ode <- stats::approx(ratio[keep], res$x[keep], xout = 1.5)$y
  # c = 0.5 means Y - Y_eq = 0.5 Y_eq; g* = 86.25 at T_f ~ 4.5 GeV
  expect_lt(abs(x_ode - freeze_out_xf(100, sv_wimp, g_star = 86.25)), 0.3)
})

# ---- 8. freeze_out_xf -------------------------------------------------------

test_that("freeze_out_xf matches the regression value with the full Planck mass", {
  xf <- freeze_out_xf(100, sv_wimp)
  expect_lt(abs(xf - 22.382114), 1e-5)
  # the reduced Planck mass would give x_f ~ 20.81, lower by ~ln(sqrt(8 pi))
  expect_gt(xf - 20.81, 1.5)
})

test_that("freeze_out_xf solves the Kolb & Turner fixed-point equation", {
  # independent root with the CODATA Planck mass 1.220890e19 GeV
  A_ref <- 0.5 * 2.5 * sqrt(45 / 8) * 2 * 100 * 1.220890e19 * sv_wimp /
    (2 * pi^3 * sqrt(106.75))
  x_ref <- uniroot(function(x) x + 0.5 * log(x) - log(A_ref), c(1, 100),
                   tol = 1e-10)$root
  expect_lt(abs(freeze_out_xf(100, sv_wimp) - x_ref), 1e-3)
  # the returned value is a fixed point (to the iteration tolerance)
  A  <- 0.5 * 2.5 * sqrt(45 / 8) * 2 * 1000 * M_Pl_red * sqrt(8 * pi) *
    sv_wimp / (2 * pi^3 * sqrt(106.75))
  xf <- freeze_out_xf(1000, sv_wimp, tol = 1e-12)
  expect_lt(abs(xf - log(A / sqrt(xf))), 1e-10)
})

test_that("freeze_out_xf grows with mass and cross section, and is vectorised", {
  m  <- c(10, 100, 1000)
  xf <- freeze_out_xf(m, sv_wimp)
  expect_true(all(diff(xf) > 0))
  expect_equal(xf, vapply(m, freeze_out_xf, numeric(1), sigmav_GeV2 = sv_wimp),
               tolerance = 1e-6)
  # A is proportional to m <sigma v>: x2 - x1 = ln 10 - (ln x2 - ln x1) / 2
  expect_lt(abs((xf[2] - xf[1]) - (log(10) - 0.5 * log(xf[2] / xf[1]))), 1e-5)
  expect_equal(freeze_out_xf(100, c(sv_wimp, 10 * sv_wimp)), xf[2:3],
               tolerance = 1e-6)
  out <- freeze_out_xf(c(100, NA), sv_wimp)
  expect_equal(out[1], xf[2], tolerance = 1e-6)
  expect_true(is.na(out[2]))
})

test_that("freeze_out_xf errors on invalid inputs (vector-safe)", {
  expect_error(freeze_out_xf(-1, sv_wimp), "'m_GeV' must be positive")
  expect_error(freeze_out_xf(c(100, 0), sv_wimp), "'m_GeV' must be positive")
  expect_error(freeze_out_xf(100, 0), "'sigmav_GeV2' must be positive")
  expect_error(freeze_out_xf(c(10, 100), c(sv_wimp, -1)),
               "'sigmav_GeV2' must be positive")
  expect_error(freeze_out_xf(100, sv_wimp, g_dof = 0), "'g_dof' must be positive")
  expect_error(freeze_out_xf(100, sv_wimp, c_coeff = -0.5),
               "'c_coeff' must be positive")
  expect_error(freeze_out_xf(100, sv_wimp, g_star = 0), "'g_star' must be positive")
  expect_error(freeze_out_xf(100, sv_wimp, tol = 0), "'tol' must be")
  expect_error(freeze_out_xf(100, sv_wimp, max_iter = 0), "'max_iter' must be")
})

test_that("freeze_out_xf errors when the log argument is <= 1", {
  # m = 1e-12 GeV: the log argument is ~2.5e-4 from the start
  expect_error(freeze_out_xf(1e-12, sv_wimp), "log argument well above e")
  # argument 1.01 at the start: the iteration would take log() of a value < 1
  expect_error(freeze_out_xf(4.063891e-9, sv_wimp), "log argument well above e")
  expect_error(freeze_out_xf(c(100, 1e-12), sv_wimp), "log argument well above e")
})

test_that("freeze_out_xf warns when it does not converge", {
  expect_warning(xf <- freeze_out_xf(100, sv_wimp, max_iter = 1),
                 "did not converge in 1 iterations")
  # one step from x_0 = ln A, with A = 2.485303e10 for m = 100 GeV
  log_A <- log(2.485303e10)
  expect_equal(xf, log_A - 0.5 * log(log_A), tolerance = 1e-6)
})

# ---- 9. peebles_rhs ---------------------------------------------------------

# Example inputs at z = 1100 (see ?peebles_rhs for their derivation)
peebles_z  <- 1100
peebles_in <- list(H = 5.14e-14, n_H = 2.53e8, alpha_B = 7.62e-19, beta_B = 590)
E21_eV     <- 1.23984198433e-6 / 121.567e-9   # h c / lambda_alpha [eV]
kB_eV      <- 8.617333262e-5                  # [eV K^-1]

peebles_hand <- function(lna, xe, H, n_H, alpha_B, beta_B, T0 = 2.7255) {
  T_K   <- T0 * exp(-lna)
  l_esc <- 8 * pi * H / ((121.567e-9)^3 * n_H * (1 - xe))
  C     <- (8.22458 + l_esc) / (8.22458 + l_esc + beta_B)
  dxe   <- C / H * (beta_B * (1 - xe) * exp(-E21_eV / (kB_eV * T_K)) -
                      n_H * alpha_B * xe^2)
  c(dxe_dlna = dxe, C = C, lambda_alpha_escape = l_esc)
}

# element-wise relative comparison of two named vectors
expect_equal_each <- function(object, expected, tolerance) {
  expect_named(object, names(expected))
  for (nm in names(expected)) {
    expect_equal(object[[nm]], expected[[nm]], tolerance = tolerance)
  }
}

test_that("peebles_rhs gives realistic values at z = 1100", {
  res <- do.call(peebles_rhs, c(list(lna = -log(1 + peebles_z), xe = 0.5),
                                peebles_in))
  expect_named(res, c("dxe_dlna", "C", "lambda_alpha_escape"))
  # hand calculation of the same formulas with independent constants
  expect_equal_each(res, do.call(peebles_hand,
                                 c(list(lna = -log(1 + peebles_z), xe = 0.5),
                                   peebles_in)),
                    tolerance = 1e-9)
  # C ~ 0.023 and dx_e/dln a ~ -21.6 (within 1 percent)
  expect_rel(res[["C"]], 0.02303, 0.01)
  expect_rel(res[["dxe_dlna"]], -21.59, 0.01)
  expect_rel(res[["lambda_alpha_escape"]], 5.684, 0.01)
})

test_that("peebles_rhs example inputs follow from first principles", {
  # Planck 2018: H0 = 67.66 km/s/Mpc, Omega_m = 0.3111, Omega_r = 9.14e-5,
  # Omega_b h^2 = 0.02242, Y_p = 0.2454; T = 2.7255 K * 1101 = 3000.8 K
  z     <- peebles_z
  T_K   <- 2.7255 * (1 + z)
  Mpc_m <- 3.0856775814913673e22
  H     <- 67.66e3 / Mpc_m *
    sqrt(0.3111 * (1 + z)^3 + 9.14e-5 * (1 + z)^4 + 0.6888086)
  rho_c_h2 <- 3 * (1e5 / Mpc_m)^2 / (8 * pi * 6.67430e-11)    # kg m^-3
  n_H   <- (1 - 0.2454) * 0.02242 * rho_c_h2 / 1.6735328e-27 * (1 + z)^3
  # case-B fit (Pequignot et al. 1991) with the RECFAST fudge factor 1.14
  t4      <- T_K / 1e4
  alpha_B <- 1.14e-19 * 4.309 * t4^-0.6166 / (1 + 0.6703 * t4^0.5300)
  # photoionization from n = 2 (binding energy E_I / 4 = 3.3996 eV)
  beta_B  <- alpha_B * (2 * pi * 9.1093837015e-31 * 1.380649e-23 * T_K /
                          6.62607015e-34^2)^1.5 *
    exp(-13.598434 / 4 / (kB_eV * T_K))
  # each derived value agrees with the rounded example input
  expect_rel(H,       peebles_in$H,       1e-3)
  expect_rel(n_H,     peebles_in$n_H,     3e-3)
  expect_rel(alpha_B, peebles_in$alpha_B, 1e-3)
  expect_rel(beta_B,  peebles_in$beta_B,  1e-3)
  res <- peebles_rhs(-log(1 + z), 0.5, H, n_H, alpha_B, beta_B)
  expect_rel(res[["dxe_dlna"]], -21.6, 0.01)
  expect_rel(res[["C"]], 0.0230, 0.01)
})

test_that("peebles_rhs includes the Boltzmann factor exp(-E_21 / kT)", {
  # detailed balance: with beta_B = n_H alpha_B x^2 exp(E_21/kT) / (1 - x)
  # the two terms cancel
  lna <- -log(1 + peebles_z); xe <- 0.3
  T_K <- 2.7255 * (1 + peebles_z)
  b_eq <- peebles_in$n_H * peebles_in$alpha_B * xe^2 *
    exp(E21_eV / (kB_eV * T_K)) / (1 - xe)
  res <- peebles_rhs(lna, xe, peebles_in$H, peebles_in$n_H,
                     peebles_in$alpha_B, b_eq)
  rec_only <- res[["C"]] / peebles_in$H * peebles_in$n_H *
    peebles_in$alpha_B * xe^2
  expect_lt(abs(res[["dxe_dlna"]]), 1e-6 * rec_only)
})

test_that("peebles_rhs depends on the temperature through lna and T0", {
  args <- c(list(xe = 0.01), peebles_in)
  hot  <- do.call(peebles_rhs, c(list(lna = -log(1501)), args))
  cold <- do.call(peebles_rhs, c(list(lna = -log(1101)), args))
  # photoionization wins at T = 4091 K, recombination at T = 3001 K
  expect_gt(hot[["dxe_dlna"]], 0)
  expect_lt(cold[["dxe_dlna"]], 0)
  expect_equal(hot[["C"]], cold[["C"]])
  expect_equal_each(hot, do.call(peebles_hand, c(list(lna = -log(1501)), args)),
                    tolerance = 1e-9)
  # only T = T0 exp(-lna) matters
  expect_equal_each(do.call(peebles_rhs,
                            c(list(lna = 0, T0 = 2.7255 * 1101), args)),
                    cold, tolerance = 1e-12)
})

test_that("peebles_rhs handles the end points xe = 0 and xe = 1", {
  lna <- -log(1101)
  r0 <- peebles_rhs(lna, 0, peebles_in$H, peebles_in$n_H,
                    peebles_in$alpha_B, peebles_in$beta_B)
  expect_gt(r0[["dxe_dlna"]], 0)
  r1 <- peebles_rhs(lna, 1, peebles_in$H, peebles_in$n_H,
                    peebles_in$alpha_B, peebles_in$beta_B)
  expect_true(all(is.finite(r1)))
  expect_equal(r1[["C"]], 1, tolerance = 1e-12)
  expect_equal(r1[["dxe_dlna"]],
               -peebles_in$n_H * peebles_in$alpha_B / peebles_in$H,
               tolerance = 1e-9)
})

test_that("peebles_rhs accepts only single finite numbers", {
  p <- function(...) {
    a <- utils::modifyList(c(list(lna = -log(1101), xe = 0.5), peebles_in),
                           list(...))
    do.call(peebles_rhs, a)
  }
  expect_error(p(n_H = c(2.53e8, 2.6e8)), "'n_H' must be a single positive")
  expect_error(p(H = c(5e-14, 6e-14)), "'H' must be a single positive")
  expect_error(p(lna = c(-7, -6)), "'lna' must be a single finite")
  expect_error(p(lna = Inf), "'lna' must be a single finite")
  expect_error(p(lna = NA_real_), "'lna' must be a single finite")
  expect_error(p(xe = c(0.1, 0.2)), "'xe' must be a single number between")
  expect_error(p(xe = -0.1), "'xe' must be a single number between")
  expect_error(p(xe = 1.1), "'xe' must be a single number between")
  expect_error(p(xe = NA_real_), "'xe' must be a single number between")
  expect_error(p(H = 0), "'H' must be a single positive")
  expect_error(p(n_H = 0), "'n_H' must be a single positive")
  expect_error(p(n_H = "2.53e8"), "'n_H' must be a single positive")
  expect_error(p(alpha_B = -1), "'alpha_B' must be a single non-negative")
  expect_error(p(alpha_B = c(1e-19, 2e-19)), "'alpha_B' must be")
  expect_error(p(beta_B = -1), "'beta_B' must be a single non-negative")
  expect_error(p(beta_B = c(1, 2)), "'beta_B' must be")
  expect_error(p(lambda_alpha = 0), "'lambda_alpha' must be")
  expect_error(p(Lambda_2s1s = c(8, 9)), "'Lambda_2s1s' must be")
  expect_error(p(neutral_floor = 0), "'neutral_floor' must be")
  expect_error(p(T0 = 0), "'T0' must be a single positive")
  expect_error(p(T0 = c(2.7, 2.8)), "'T0' must be a single positive")
})
