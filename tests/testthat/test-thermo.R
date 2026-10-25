# tests/testthat/test-thermo.R
# Tests for R/06_thermo2.R: photon densities and the Saha equation.

# Reference constants written out independently of the package internals
# (CODATA 2018: k_B and e are exact; hbar c = 197.3269804 MeV fm).
k_B_eV   <- 1.380649e-23 / 1.602176634e-19   # Boltzmann constant [eV K^-1]
hbar_c   <- 1.973269804e-5                   # hbar c [eV cm]
T_cmb_K  <- 2.7255                           # CMB temperature today [K]
zeta3    <- 1.2020569031595942               # Riemann zeta(3)

# Element-wise relative comparison. expect_equal() averages differences over
# a vector and compares absolutely for values below its tolerance, which would
# hide errors in the small numbers tested here.
expect_rel <- function(object, expected, tol) {
  expect_length(object, length(expected))
  expect_lt(max(abs(object / expected - 1)), tol)
}

# ---- photon_energy_density_fn_T -------------------------------------------

test_that("photon_energy_density_fn_T equals pi^2/15 T^4 (T in eV)", {
  expect_equal(photon_energy_density_fn_T(1, "eV"), pi^2 / 15, tolerance = 1e-14)
  expect_equal(photon_energy_density_fn_T(2), 16 * pi^2 / 15, tolerance = 1e-14)
  expect_identical(photon_energy_density_fn_T(0), 0)
  expect_equal(photon_energy_density_fn_T(2) / photon_energy_density_fn_T(1),
               16, tolerance = 1e-14)
})

test_that("photon_energy_density_fn_T converts Kelvin with the exact k_B", {
  T_K <- c(1, 300, 1e4)
  expect_rel(photon_energy_density_fn_T(T_K, "K"),
             pi^2 / 15 * (k_B_eV * T_K)^4, 1e-12)
  expect_rel(photon_energy_density_fn_T(T_K, "K"),
             photon_energy_density_fn_T(k_B_eV * T_K, "eV"), 1e-14)
  # 1 K is 8.617333e-5 eV (the old truncated 8.6173e-5 is 4e-6 too small)
  expect_rel(photon_energy_density_fn_T(1, "K")^(1 / 4) / sqrt(sqrt(pi^2 / 15)),
             8.617333262e-5, 1e-9)
})

test_that("photon_energy_density_fn_T reproduces the CMB energy density", {
  # PDG: rho_gamma = 4.64511e-34 g cm^-3 for T = 2.7255 K, i.e.
  # rho_gamma c^2 = 0.260572 eV cm^-3
  rho_pdg <- 4.64511e-34 * 2.99792458e10^2 / 1.602176634e-12
  rho_pkg <- photon_energy_density_fn_T(T_cmb_K, "K") / hbar_c^3
  expect_equal(rho_pkg, rho_pdg, tolerance = 1e-4)
})

test_that("photon_energy_density_fn_T is vectorised and propagates NA", {
  out <- photon_energy_density_fn_T(c(1, NA, 2))
  expect_length(out, 3)
  expect_equal(out[c(1, 3)], c(1, 16) * pi^2 / 15, tolerance = 1e-14)
  expect_true(is.na(out[2]))
})

test_that("photon_energy_density_fn_T validates temp and unit", {
  expect_error(photon_energy_density_fn_T(-1), "'temp' must be non-negative")
  expect_error(photon_energy_density_fn_T(c(1, -1), "K"),
               "'temp' must be non-negative")
  expect_error(photon_energy_density_fn_T("1"), "'temp' must be numeric")
  expect_error(photon_energy_density_fn_T(1, "J"), "'unit' must be either")
  expect_error(photon_energy_density_fn_T(1, "ev"), "'unit' must be either")
  expect_error(photon_energy_density_fn_T(1, c("eV", "K")),
               "'unit' must be either")
  expect_error(photon_energy_density_fn_T(1, NA_character_),
               "'unit' must be either")
  expect_error(photon_energy_density_fn_T(1, NA), "'unit' must be either")
})

# ---- photon_number_density_fn_T -------------------------------------------

test_that("photon_number_density_fn_T equals 2 zeta(3) / pi^2 T^3", {
  expect_equal(photon_number_density_fn_T(1, "eV"), 2 * zeta3 / pi^2,
               tolerance = 1e-10)
  expect_equal(photon_number_density_fn_T(3) / photon_number_density_fn_T(1),
               27, tolerance = 1e-14)
  expect_identical(photon_number_density_fn_T(0), 0)
  T_K <- c(1, 300, 1e4)
  expect_rel(photon_number_density_fn_T(T_K, "K"),
             2 * zeta3 / pi^2 * (k_B_eV * T_K)^3, 1e-10)
})

test_that("photon_number_density_fn_T reproduces the CMB photon density", {
  # PDG: n_gamma = 410.7 cm^-3 for T = 2.7255 K
  n_pkg <- photon_number_density_fn_T(T_cmb_K, "K") / hbar_c^3
  expect_equal(n_pkg, 410.7, tolerance = 2e-4)
})

test_that("photon_number_density_fn_T is vectorised and propagates NA", {
  out <- photon_number_density_fn_T(c(NA, 2), "eV")
  expect_true(is.na(out[1]))
  expect_equal(out[2], 8 * 2 * zeta3 / pi^2, tolerance = 1e-10)
})

test_that("photon_number_density_fn_T validates temp and unit", {
  expect_error(photon_number_density_fn_T(-0.1), "'temp' must be non-negative")
  expect_error(photon_number_density_fn_T(TRUE), "'temp' must be numeric")
  expect_error(photon_number_density_fn_T(1, "J"), "'unit' must be either")
  expect_error(photon_number_density_fn_T(1, c("K", "K")),
               "'unit' must be either")
  expect_error(photon_number_density_fn_T(1, 1), "'unit' must be either")
})

# ---- photon_*_fn_z --------------------------------------------------------

test_that("photon densities at redshift z use T = 2.7255 K (1 + z)", {
  z <- c(0, 1, 1100)
  expect_rel(photon_energy_density_fn_z(z),
             photon_energy_density_fn_T(T_cmb_K * (1 + z), "K"), 1e-12)
  expect_rel(photon_number_density_fn_z(z),
             photon_number_density_fn_T(T_cmb_K * (1 + z), "K"), 1e-12)
  # today: 410.7 photons cm^-3 and 0.2606 eV cm^-3 (PDG)
  expect_equal(photon_number_density_fn_z(0) / hbar_c^3, 410.7, tolerance = 2e-4)
  expect_equal(photon_energy_density_fn_z(0) / hbar_c^3, 0.260572,
               tolerance = 1e-4)
})

test_that("photon densities scale as (1 + z)^4 and (1 + z)^3", {
  expect_equal(photon_energy_density_fn_z(1100) / photon_energy_density_fn_z(0),
               1101^4, tolerance = 1e-12)
  expect_equal(photon_number_density_fn_z(1100) / photon_number_density_fn_z(0),
               1101^3, tolerance = 1e-12)
})

test_that("photon_*_fn_z propagate NA and validate z", {
  expect_true(is.na(photon_energy_density_fn_z(NA_real_)))
  expect_true(is.na(photon_number_density_fn_z(c(0, NA))[2]))
  expect_error(photon_energy_density_fn_z(-1), "'z' must be greater than -1")
  expect_error(photon_number_density_fn_z(c(0, -2)),
               "'z' must be greater than -1")
  expect_error(photon_energy_density_fn_z("0"), "'z' must be numeric")
  expect_error(photon_number_density_fn_z("0"), "'z' must be numeric")
})

# ---- Saha_Xe / soln_saha --------------------------------------------------

# Independent evaluation of the right-hand side of the Saha equation in SI
# units, X_e^2 / (1 - X_e) = (m_e k T / (2 pi hbar^2))^(3/2) exp(-E_I / kT)
# / n_H, with n_H = 0.76 eta n_gamma and eta = 6e-10 (the package's model).
saha_rhs_SI <- function(z) {
  m_e  <- 9.1093837015e-31             # kg
  hbar <- 6.62607015e-34 / (2 * pi)    # J s
  c0   <- 299792458                    # m s^-1
  kT   <- 1.380649e-23 * T_cmb_K * (1 + z)            # J
  E_I  <- 13.598434 * 1.602176634e-19                 # J (NIST)
  n_gamma <- 2 * zeta3 / pi^2 * (kT / (hbar * c0))^3  # m^-3
  n_H  <- 0.76 * 6e-10 * n_gamma
  (m_e * kT / (2 * pi * hbar^2))^1.5 * exp(-E_I / kT) / n_H
}

test_that("Saha_Xe has no NaN and stays in [0, 1] over a wide z range", {
  z  <- c(0, 10, 80, 500, 1100, 3000, 1e4, 5e4)
  xe <- Saha_Xe(z)
  expect_length(xe, length(z))
  expect_false(anyNA(xe))
  expect_true(all(xe >= 0 & xe <= 1))
  # fully neutral at low z (f overflows) and fully ionized at high z
  expect_identical(Saha_Xe(c(0, 10, 80)), c(0, 0, 0))
  expect_lt(abs(Saha_Xe(5e4) - 1), 1e-12)
  expect_lt(abs(Saha_Xe(1e4) - 1), 1e-12)
})

test_that("Saha_Xe decreases with decreasing redshift", {
  z  <- c(0, 10, 80, 500, 1100, 3000, 1e4, 5e4)
  expect_true(all(diff(Saha_Xe(z)) >= 0))
  z_rec <- seq(1000, 1800, by = 50)
  expect_true(all(diff(Saha_Xe(z_rec)) > 0))
})

test_that("Saha_Xe solves the Saha equation (independent SI evaluation)", {
  z  <- c(900, 1100, 1300, 1368.6, 1500, 2000)
  xe <- Saha_Xe(z)
  expect_rel(xe^2 / (1 - xe), saha_rhs_SI(z), 1e-7)
})

test_that("Saha_Xe matches regression values at z = 1100 and 1300", {
  # With E_I = 13.598434 eV, T_0 = 2.7255 K, m_e = 0.51099895 MeV,
  # eta = 6e-10: X_e(1100) = 4.787878e-3, X_e(1300) = 0.2138134
  expect_equal(Saha_Xe(1100), 4.787878e-3, tolerance = 1e-6)
  expect_equal(Saha_Xe(1300), 0.2138134, tolerance = 1e-6)
})

test_that("soln_saha is X_e - 0.5 and its root is z ~ 1368.6", {
  z <- c(1000, 1368, 1500)
  expect_equal(soln_saha(z), Saha_Xe(z) - 0.5)
  root <- uniroot(soln_saha, c(1000, 2000), tol = 1e-8)$root
  # independent root of the SI Saha equation at X_e = 0.5 (rhs = 0.5)
  root_SI <- uniroot(function(z) saha_rhs_SI(z) - 0.5, c(1000, 2000),
                     tol = 1e-8)$root
  expect_lt(abs(root - root_SI), 1e-4)
  expect_lt(abs(root - 1368.587), 1e-2)
})

test_that("Saha_Xe and soln_saha propagate NA and validate z", {
  out <- Saha_Xe(c(1100, NA))
  expect_equal(out[1], Saha_Xe(1100))
  expect_true(is.na(out[2]))
  expect_true(is.na(soln_saha(NA_real_)))
  expect_error(Saha_Xe(-1), "'z' must be greater than -1")
  expect_error(Saha_Xe(c(1100, -2)), "'z' must be greater than -1")
  expect_error(Saha_Xe("1100"), "'z' must be numeric")
  expect_error(soln_saha(-1), "'z' must be greater than -1")
})
