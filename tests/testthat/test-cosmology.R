# tests/testthat/test-cosmology.R
# Tests for R/04_cosmology.R. Reference values are analytic solutions of the
# Friedmann equation or independent numerical integrations, and use the
# constants below rather than package internals.

km_per_Mpc <- 648000 / pi * 149597870.7 * 1e6 # IAU 2012 au, IAU 2015 parsec
c_km_s     <- 299792.458                      # speed of light [km/s]
Gyr_in_s   <- 365.25 * 86400 * 1e9            # Julian gigayear [s]
D_H <- function(h) c_km_s / (100 * h)                # Hubble distance [Mpc]
t_H <- function(h) km_per_Mpc / (100 * h) / Gyr_in_s # Hubble time [Gyr]

# largest element-wise relative error (zero where both values are equal)
rel_err <- function(x, ref) max(ifelse(x == ref, 0, abs(x / ref - 1)))

# Models with closed-form solutions; all have Omega_total = 1 and h = 0.7
model_eds       <- function() cosmology_model(h = 0.7, Omega_m = 1, Omega_Lambda = 0)
model_milne     <- function() cosmology_model(h = 0.7, Omega_m = 0, Omega_Lambda = 0,
                                              Omega_k = 1)
model_de_sitter <- function() cosmology_model(h = 0.7, Omega_m = 0, Omega_Lambda = 1)
model_radiation <- function() cosmology_model(h = 0.7, Omega_m = 0, Omega_Lambda = 0,
                                              Omega_r = 1)
model_open      <- function() cosmology_model(h = 0.7, Omega_m = 0.3, Omega_Lambda = 0,
                                              Omega_k = 0.7)
model_closed    <- function() cosmology_model(h = 0.7, Omega_m = 2, Omega_Lambda = 0,
                                              Omega_k = -1)
# E^2 = 1.5 - 0.5 (1 + z)^2: no big bang, bounce at z = sqrt(3) - 1
model_bounce    <- function() cosmology_model(h = 0.7, Omega_m = 0, Omega_Lambda = 1.5,
                                              Omega_k = -0.5)
readme_model    <- function() cosmology_model(h = 0.6774, Omega_m = 0.3089,
                                              Omega_Lambda = 0.6911, Omega_r = 8.4e-5)

z_pos <- c(0.01, 0.5, 1, 3, 10, 1100)


# ---- km_to_Mpc() / Mpc_to_km() --------------------------------------------

test_that("Mpc_to_km() uses the exact IAU 2015 parsec", {
  # 1 pc = 648000 / pi au and 1 au = 149597870.7 km exactly
  expect_equal(Mpc_to_km(1), 648000 / pi * 149597870.7 * 1e6, tolerance = 1e-14)
  # 1 pc = 3.0856775814913673e16 m
  expect_equal(Mpc_to_km(1), 3.0856775814913673e19, tolerance = 1e-14)
  expect_equal(km_to_Mpc(3.0856775814913673e19), 1, tolerance = 1e-14)
})

test_that("km_to_Mpc() and Mpc_to_km() are vectorized inverses", {
  mpc <- c(0, 1e-6, 1, 4425.634, 1e4)
  expect_equal(Mpc_to_km(mpc), mpc * km_per_Mpc)
  expect_equal(km_to_Mpc(mpc * km_per_Mpc), mpc)
  km <- c(1, 1e19, 1e23)
  expect_lt(rel_err(Mpc_to_km(km_to_Mpc(km)), km), 1e-14)
  expect_identical(km_to_Mpc(numeric(0)), numeric(0))
})

test_that("km_to_Mpc() and Mpc_to_km() keep NA and reject non-numeric input", {
  expect_equal(km_to_Mpc(c(km_per_Mpc, NA)), c(1, NA))
  expect_identical(Mpc_to_km(NA), NA_real_)
  expect_error(km_to_Mpc("1e19"), "'km' must be numeric.", fixed = TRUE)
  expect_error(km_to_Mpc(TRUE), "'km' must be numeric.", fixed = TRUE)
  expect_error(Mpc_to_km("1"), "'mpc' must be numeric.", fixed = TRUE)
  expect_error(Mpc_to_km(factor(1)), "'mpc' must be numeric.", fixed = TRUE)
})


# ---- cosmology_model() ------------------------------------------------------

test_that("cosmology_model() defaults to Planck 2015 flat Lambda-CDM", {
  cosmo <- cosmology_model()
  expect_s3_class(cosmo, "astronomR_cosmology")
  expect_type(cosmo, "list")
  expect_named(cosmo, c("h", "H0", "Omega_m", "Omega_Lambda", "Omega_r",
                        "Omega_k", "T_cmb", "curvature"))
  expect_equal(cosmo$h, 0.6774)
  expect_equal(cosmo$H0, 67.74)
  expect_equal(cosmo$Omega_m, 0.3089)
  expect_equal(cosmo$Omega_Lambda, 0.6911)
  expect_identical(cosmo$Omega_r, 0)
  expect_identical(cosmo$Omega_k, 0)
  expect_equal(cosmo$T_cmb, 2.7255)
  expect_identical(cosmo$curvature, "flat")
})

test_that("cosmology_model() stores its arguments and classifies curvature", {
  cosmo <- cosmology_model(h = 0.7, Omega_m = 0.25, Omega_Lambda = 0.65,
                           Omega_r = 1e-4, Omega_k = 0.0999, T_cmb = 3)
  expect_equal(unclass(cosmo), list(
    h = 0.7, H0 = 70, Omega_m = 0.25, Omega_Lambda = 0.65, Omega_r = 1e-4,
    Omega_k = 0.0999, T_cmb = 3, curvature = "open"
  ))
  closed <- cosmology_model(Omega_m = 0.4, Omega_Lambda = 0.7, Omega_k = -0.1)
  expect_identical(closed$curvature, "closed")
  # Omega_Lambda may be negative; integers are stored as doubles
  neg <- cosmology_model(h = 1L, Omega_m = 2L, Omega_Lambda = -1L)
  expect_identical(neg$curvature, "flat")
  expect_identical(neg$Omega_Lambda, -1)
  expect_identical(neg$H0, 100)
  expect_null(names(cosmology_model(h = c(h = 0.7))$h))
})

test_that("cosmology_model() rejects anything but a single finite number", {
  bad_values <- list("0.3", c(0.3, 0.3), numeric(0), NA, NA_real_, NaN, Inf,
                     -Inf, TRUE, NULL)
  for (arg in c("h", "Omega_m", "Omega_Lambda", "Omega_r", "Omega_k", "T_cmb")) {
    for (bad in bad_values) {
      expect_error(
        do.call(cosmology_model, stats::setNames(list(bad), arg)),
        sprintf("'%s' must be a single finite number.", arg), fixed = TRUE
      )
    }
  }
})

test_that("cosmology_model() checks parameter ranges", {
  expect_error(cosmology_model(h = 0), "'h' must be positive.", fixed = TRUE)
  expect_error(cosmology_model(h = -0.7), "'h' must be positive.", fixed = TRUE)
  expect_error(cosmology_model(h = 2), "'h' must be less than 2", fixed = TRUE)
  # a Hubble constant passed as h gets a hint
  expect_error(cosmology_model(h = 67.74), "e.g. 0.6774 rather than 67.74",
               fixed = TRUE)
  expect_error(cosmology_model(Omega_m = -0.1, Omega_Lambda = 1.1),
               "'Omega_m' must be non-negative.", fixed = TRUE)
  expect_error(cosmology_model(Omega_r = -1e-5),
               "'Omega_r' must be non-negative.", fixed = TRUE)
  expect_error(cosmology_model(T_cmb = -1), "'T_cmb' must be non-negative.",
               fixed = TRUE)
  expect_no_error(cosmology_model(h = 1.99, Omega_m = 0, Omega_Lambda = 1,
                                  T_cmb = 0))
})

test_that("cosmology_model() warns when the density parameters do not sum to 1", {
  expect_warning(
    m <- cosmology_model(Omega_m = 0.3, Omega_Lambda = 0.6),
    "Omega_m + Omega_Lambda + Omega_r + Omega_k = 0.9, not 1", fixed = TRUE
  )
  expect_s3_class(m, "astronomR_cosmology")
  expect_warning(cosmology_model(Omega_m = 1, Omega_Lambda = 0.7),
                 "use Omega_k = 1 - Omega_m - Omega_Lambda - Omega_r",
                 fixed = TRUE)
  expect_warning(cosmology_model(Omega_r = 1.1e-3), "= 1.0011, not 1",
                 fixed = TRUE)
  expect_no_warning(cosmology_model())
  expect_no_warning(cosmology_model(Omega_r = 8.4e-5)) # sum off by 8.4e-5
  expect_no_warning(cosmology_model(Omega_m = 0.3, Omega_Lambda = 0.6,
                                    Omega_k = 0.1))
})

test_that("print() shows the curvature and parameters and returns the model invisibly", {
  cosmo <- readme_model()
  expect_output(print(cosmo), "astronomR cosmology (flat)", fixed = TRUE)
  out <- capture.output(res <- withVisible(print(cosmo)))
  expect_identical(out, c(
    "astronomR cosmology (flat)",
    "  H0           = 67.74 km/s/Mpc (h = 0.6774)",
    "  Omega_m      = 0.3089",
    "  Omega_Lambda = 0.6911",
    "  Omega_r      = 8.4e-05",
    "  Omega_k      = 0",
    "  T_cmb        = 2.7255 K"
  ))
  expect_false(res$visible)
  expect_identical(res$value, cosmo)
  expect_output(print(cosmology_model(Omega_m = 0.4, Omega_Lambda = 0.7,
                                      Omega_k = -0.1)),
                "astronomR cosmology (closed)", fixed = TRUE)
  open_model <- cosmology_model(h = 0.7, Omega_m = 0.3, Omega_Lambda = 0.6,
                                Omega_k = 0.1)
  expect_output(print(open_model), "astronomR cosmology (open)", fixed = TRUE)
  expect_output(print(open_model), "H0           = 70 km/s/Mpc (h = 0.7)",
                fixed = TRUE)
  expect_output(print(open_model), "Omega_k      = 0.1", fixed = TRUE)
})


# ---- Argument checks shared by every consumer -----------------------------

test_that("every consumer requires a cosmology_model() object", {
  fakes <- list(
    unclass(cosmology_model()),
    list(hubble_constant_fact = 0.6774, dark_matter_crit = 0.6911,
         matter_crit = 0.3089, radiation_crit = 0, type = "FlatLCDM"),
    0.7,
    NULL
  )
  msg <- "'cosmo' must be a cosmological model created by cosmology_model()."
  for (fake in fakes) {
    expect_error(hubble_ratio(fake, 1), msg, fixed = TRUE)
    expect_error(hubble_distance(fake), msg, fixed = TRUE)
    expect_error(comoving_distance(fake, 1), msg, fixed = TRUE)
    expect_error(transverse_comoving_distance(fake, 1), msg, fixed = TRUE)
    expect_error(luminosity_distance(fake, 1), msg, fixed = TRUE)
    expect_error(angular_diameter_distance(fake, 1), msg, fixed = TRUE)
    expect_error(radius_of_curvature(fake), msg, fixed = TRUE)
    expect_error(cosmic_time(fake, 1), msg, fixed = TRUE)
    expect_error(age_of_universe(fake), msg, fixed = TRUE)
  }
})

test_that("invalid units are rejected", {
  cosmo <- cosmology_model()
  expect_error(hubble_distance(cosmo, unit = "pc"), "should be one of")
  expect_error(radius_of_curvature(cosmo, unit = "pc"), "should be one of")
  for (f in list(comoving_distance, transverse_comoving_distance,
                 luminosity_distance, angular_diameter_distance)) {
    expect_error(f(cosmo, 1, unit = "pc"), "should be one of")
  }
  expect_error(cosmic_time(cosmo, 1, unit = "s"), "should be one of")
  expect_error(age_of_universe(cosmo, unit = "GY"), "should be one of")
  expect_error(comoving_distance(cosmo, 1, unit = c("km", "Mpc")),
               "must be of length 1")
})

test_that("redshift arguments are validated", {
  cosmo <- cosmology_model()
  for (f in list(comoving_distance, transverse_comoving_distance,
                 luminosity_distance, angular_diameter_distance)) {
    expect_error(f(cosmo, c(0.5, -0.1)), "'z' must be non-negative.",
                 fixed = TRUE)
    expect_error(f(cosmo, "1"), "'z' must be numeric.", fixed = TRUE)
    expect_error(f(cosmo, Inf), "'z' must be finite.", fixed = TRUE)
  }
  for (f in list(hubble_ratio, cosmic_time)) {
    expect_error(f(cosmo, -1), "'z' must be greater than -1.", fixed = TRUE)
    expect_error(f(cosmo, c(0, -2)), "'z' must be greater than -1.",
                 fixed = TRUE)
    expect_error(f(cosmo, "0"), "'z' must be numeric.", fixed = TRUE)
    expect_error(f(cosmo, -Inf), "'z' must be finite.", fixed = TRUE)
  }
})

test_that("NA, empty and named redshift vectors are handled", {
  cosmo <- cosmology_model()
  d1 <- comoving_distance(cosmo, 1)
  expect_identical(comoving_distance(cosmo, c(1, NA)), c(d1, NA))
  expect_identical(comoving_distance(cosmo, NA), NA_real_)
  expect_identical(luminosity_distance(cosmo, c(NA_real_, NA_real_)),
                   c(NA_real_, NA_real_))
  expect_identical(comoving_distance(cosmo, numeric(0)), numeric(0))
  expect_equal(hubble_ratio(cosmo, c(NA, 0)), c(NA, 1))
  expect_equal(cosmic_time(cosmo, c(0, NA)), c(age_of_universe(cosmo), NA))
  expect_identical(cosmic_time(cosmo, NA), NA_real_)
  expect_identical(cosmic_time(cosmo, numeric(0)), numeric(0))
  expect_null(names(comoving_distance(cosmo, c(a = 0.5, b = 1))))
  expect_null(names(hubble_ratio(cosmo, c(a = 0.5))))
  expect_null(names(cosmic_time(cosmo, c(a = 0.5))))
})


# ---- hubble_ratio() -----------------------------------------------------------

test_that("hubble_ratio() matches E(z) of single-component models", {
  z <- c(-0.5, 0, 0.5, 1, 3, 1100)
  expect_equal(hubble_ratio(model_eds(), z), (1 + z)^1.5)    # matter
  expect_equal(hubble_ratio(model_radiation(), z), (1 + z)^2) # radiation
  expect_equal(hubble_ratio(model_milne(), z), 1 + z)         # curvature
  expect_equal(hubble_ratio(model_de_sitter(), z), rep(1, 6)) # Lambda
})

test_that("hubble_ratio() combines the components and is 1 today", {
  closed <- cosmology_model(Omega_m = 0.4, Omega_Lambda = 0.7, Omega_k = -0.1)
  expect_equal(hubble_ratio(closed, 0), 1)
  # hand calculation: 0.4 * 3^3 - 0.1 * 3^2 + 0.7 = 10.6
  expect_equal(hubble_ratio(closed, 2), sqrt(10.6))
  expect_equal(hubble_ratio(cosmology_model(), 0), 1)
  # H(z = 1) = 67.74 * sqrt(0.3089 * 2^3 + 0.6911) km/s/Mpc
  expect_equal(cosmology_model()$H0 * hubble_ratio(cosmology_model(), 1),
               67.74 * sqrt(3.1623))
})

test_that("hubble_ratio() stops where E(z)^2 <= 0", {
  # Omega_m = 2, Omega_k = -1: E^2 = (1 + z)^2 (1 + 2 z), zero at z = -0.5
  expect_equal(hubble_ratio(model_closed(), -0.4), 0.6 * sqrt(0.2))
  expect_error(hubble_ratio(model_closed(), -0.5), "E(z)^2 <= 0 at z = -0.5",
               fixed = TRUE)
  expect_error(hubble_ratio(model_closed(), c(0, -0.6)),
               "E(z)^2 <= 0 at z = -0.6", fixed = TRUE)
})


# ---- hubble_distance() and radius_of_curvature() --------------------------

test_that("hubble_distance() is c / H0", {
  expect_equal(hubble_distance(cosmology_model()), 299792.458 / 67.74)
  expect_equal(hubble_distance(model_eds(), unit = "km"),
               299792.458 / 70 * km_per_Mpc)
  expect_length(hubble_distance(cosmology_model()), 1)
})

test_that("radius_of_curvature() is Inf when flat and D_H / sqrt(|Omega_k|) otherwise", {
  expect_identical(radius_of_curvature(cosmology_model()), Inf)
  expect_identical(radius_of_curvature(model_eds(), unit = "km"), Inf)
  expect_equal(radius_of_curvature(model_milne()), D_H(0.7))
  expect_equal(radius_of_curvature(model_open()), D_H(0.7) / sqrt(0.7))
  expect_equal(radius_of_curvature(model_closed(), unit = "km"),
               D_H(0.7) * km_per_Mpc)
})


# ---- Distances ----------------------------------------------------------------

test_that("comoving_distance() matches analytic solutions", {
  z <- z_pos
  # Einstein-de Sitter: D_C = 2 D_H (1 - 1 / sqrt(1 + z))
  expect_lt(rel_err(comoving_distance(model_eds(), z),
                    2 * D_H(0.7) * (1 - 1 / sqrt(1 + z))), 1e-9)
  # empty open (Milne): D_C = D_H ln(1 + z)
  expect_lt(rel_err(comoving_distance(model_milne(), z),
                    D_H(0.7) * log(1 + z)), 1e-9)
  # de Sitter: D_C = D_H z
  expect_lt(rel_err(comoving_distance(model_de_sitter(), z), D_H(0.7) * z),
            1e-9)
  # radiation only: D_C = D_H z / (1 + z)
  expect_lt(rel_err(comoving_distance(model_radiation(), z),
                    D_H(0.7) * z / (1 + z)), 1e-9)
  # closed, Omega_m = 2: D_C = D_H (2 atan(sqrt(1 + 2 z)) - pi / 2)
  expect_lt(rel_err(comoving_distance(model_closed(), z),
                    D_H(0.7) * (2 * atan(sqrt(1 + 2 * z)) - pi / 2)), 1e-9)
  # bouncing model below its bounce: D_C = D_H sqrt(2) [asin((1 + z) / sqrt(3))
  # - asin(1 / sqrt(3))], and (1 + 0.5) / sqrt(3) = sin(pi / 3)
  expect_equal(comoving_distance(model_bounce(), 0.5),
               D_H(0.7) * sqrt(2) * (pi / 3 - asin(1 / sqrt(3))),
               tolerance = 1e-9)
})

test_that("transverse_comoving_distance() matches analytic solutions", {
  z <- z_pos
  cosmo <- cosmology_model()
  expect_identical(transverse_comoving_distance(cosmo, z),
                   comoving_distance(cosmo, z))
  # Milne: D_M = D_H sinh(ln(1 + z)) = D_H z (2 + z) / (2 (1 + z))
  expect_lt(rel_err(transverse_comoving_distance(model_milne(), z),
                    D_H(0.7) * z * (2 + z) / (2 * (1 + z))), 1e-9)
  # open, matter only (Mattig relation with Omega_m = 0.3)
  Om <- 0.3
  mattig <- 2 * D_H(0.7) * (Om * z + (Om - 2) * (sqrt(1 + Om * z) - 1)) /
    (Om^2 * (1 + z))
  expect_lt(rel_err(transverse_comoving_distance(model_open(), z), mattig),
            1e-9)
  # closed, matter only (Mattig relation with Omega_m = 2): D_M = D_H z / (1 + z)
  expect_lt(rel_err(transverse_comoving_distance(model_closed(), z),
                    D_H(0.7) * z / (1 + z)), 1e-9)
})

test_that("luminosity and angular diameter distances match analytic solutions", {
  z <- z_pos
  # Einstein-de Sitter: D_L = 2 D_H (1 + z - sqrt(1 + z))
  expect_lt(rel_err(luminosity_distance(model_eds(), z),
                    2 * D_H(0.7) * (1 + z - sqrt(1 + z))), 1e-9)
  # Milne: D_L = D_H z (1 + z / 2)
  expect_lt(rel_err(luminosity_distance(model_milne(), z),
                    D_H(0.7) * z * (1 + z / 2)), 1e-9)
  # closed, Omega_m = 2: D_A = D_H z / (1 + z)^2
  expect_lt(rel_err(angular_diameter_distance(model_closed(), z),
                    D_H(0.7) * z / (1 + z)^2), 1e-9)
  # Einstein-de Sitter D_A peaks at z = 5 / 4, where D_A = (8 / 27) D_H
  expect_equal(angular_diameter_distance(model_eds(), 1.25), 8 / 27 * D_H(0.7),
               tolerance = 1e-9)
  da <- angular_diameter_distance(model_eds(), c(1.2, 1.25, 1.3))
  expect_gt(da[2], da[1])
  expect_gt(da[2], da[3])
})

test_that("distance identities hold for every curvature", {
  z <- c(0.1, 0.5, 2, 5)
  models <- list(readme_model(), model_open(), model_closed(),
                 cosmology_model(Omega_m = 0.3, Omega_Lambda = 0.8,
                                 Omega_k = -0.1))
  for (cosmo in models) {
    dm <- transverse_comoving_distance(cosmo, z)
    dl <- luminosity_distance(cosmo, z)
    da <- angular_diameter_distance(cosmo, z)
    expect_equal(dl, (1 + z)^2 * da) # Etherington reciprocity
    expect_equal(dl, (1 + z) * dm)
    expect_equal(da, dm / (1 + z))
  }
})

test_that("distances in km are distances in Mpc times the IAU Mpc", {
  cosmo <- readme_model()
  z <- c(0.5, 2)
  for (f in list(comoving_distance, transverse_comoving_distance,
                 luminosity_distance, angular_diameter_distance)) {
    expect_equal(f(cosmo, z, unit = "km"), f(cosmo, z) * km_per_Mpc)
  }
  expect_equal(hubble_distance(cosmo, unit = "km"),
               hubble_distance(cosmo) * km_per_Mpc)
  expect_equal(radius_of_curvature(model_open(), unit = "km"),
               radius_of_curvature(model_open()) * km_per_Mpc)
})

test_that("distances are vectorized over z and vanish at z = 0", {
  cosmo <- readme_model()
  z <- c(2, 0, 0.3, 1100, 0.3)
  for (f in list(comoving_distance, transverse_comoving_distance,
                 luminosity_distance, angular_diameter_distance)) {
    expect_equal(f(cosmo, z), vapply(z, function(zi) f(cosmo, zi), numeric(1)))
    expect_identical(f(cosmo, 0), 0)
  }
  expect_identical(transverse_comoving_distance(model_closed(), 0), 0)
  expect_identical(luminosity_distance(model_open(), 0), 0)
})

test_that("README-model distances agree with an independent integration", {
  # composite Simpson rule in z with 2e6 intervals, computed separately
  expect_equal(comoving_distance(readme_model(), 1), 3395.8184916341,
               tolerance = 1e-10)
  expect_equal(comoving_distance(cosmology_model(), c(0.5, 1, 2)),
               c(1945.6515917117, 3396.2109708407, 5312.3782513822),
               tolerance = 1e-10)
})

test_that("distances stop where E(z)^2 <= 0 is reached", {
  bounce <- model_bounce()
  expect_error(comoving_distance(bounce, c(0.5, 1)),
               "E(z)^2 <= 0 somewhere in the range 0 <= z <= 1", fixed = TRUE)
  expect_error(transverse_comoving_distance(bounce, 1), "E(z)^2 <= 0",
               fixed = TRUE)
  expect_error(luminosity_distance(bounce, 1), "E(z)^2 <= 0", fixed = TRUE)
  expect_error(angular_diameter_distance(bounce, 1), "E(z)^2 <= 0",
               fixed = TRUE)
  # a model with E(0)^2 = 0 fails everywhere
  expect_warning(empty <- cosmology_model(Omega_m = 0, Omega_Lambda = 0),
                 "not 1")
  expect_error(comoving_distance(empty, 0), "E(z)^2 <= 0", fixed = TRUE)
  expect_error(hubble_ratio(empty, 0), "E(z)^2 <= 0", fixed = TRUE)
  expect_error(age_of_universe(empty), "E(z)^2 <= 0", fixed = TRUE)
})

test_that("the E(z)^2 check finds interior minima, not only the end points", {
  # E^2 = 0.1 x^3 - 1.3 x^2 + 2.2 (x = 1 + z) is positive at z = 0 and z = 19
  # but negative around its minimum at x = 26 / 3
  m <- cosmology_model(Omega_m = 0.1, Omega_Lambda = 2.2, Omega_k = -1.3)
  expect_gt(hubble_ratio(m, 19), 0)
  expect_error(comoving_distance(m, 19), "E(z)^2 <= 0", fixed = TRUE)
  # with radiation: E^2 = 0.01 x^4 - x^2 + 1.99, minimum at x = sqrt(50)
  m2 <- cosmology_model(Omega_m = 0, Omega_Lambda = 1.99, Omega_k = -1,
                        Omega_r = 0.01)
  expect_gt(hubble_ratio(m2, 14), 0)
  expect_error(comoving_distance(m2, 14), "E(z)^2 <= 0", fixed = TRUE)
  expect_error(cosmic_time(m2, 0), "E(z)^2 <= 0", fixed = TRUE)
})

test_that("a failed numerical integration gives a clear error", {
  local_mocked_bindings(.cosmo_a2E2 = function(cosmo, a) 0 * a)
  expect_error(comoving_distance(cosmology_model(), 1),
               "Numerical integration failed: non-finite function value",
               fixed = TRUE)
  expect_error(cosmic_time(cosmology_model(), 1),
               "Numerical integration failed", fixed = TRUE)
})


# ---- cosmic_time() and age_of_universe() -----------------------------------

test_that("cosmic_time() matches the flat Lambda-CDM closed form", {
  # Omega_r = 0: t(z) = 2 / (3 H0 sqrt(OL)) asinh(sqrt(OL / Om) (1 + z)^(-3/2))
  z <- c(-0.9, -0.5, 0, 0.5, 1, 3, 10, 1100, 1e6)
  ref <- 2 / (3 * sqrt(0.6911)) *
    asinh(sqrt(0.6911 / 0.3089) * (1 + z)^-1.5) * t_H(0.6774)
  expect_lt(rel_err(cosmic_time(cosmology_model(), z), ref), 1e-9)
  expect_equal(age_of_universe(cosmology_model()),
               2 / (3 * sqrt(0.6911)) * asinh(sqrt(0.6911 / 0.3089)) *
                 t_H(0.6774),
               tolerance = 1e-10)
})

test_that("cosmic_time() matches matter, curvature and radiation solutions", {
  z <- c(-0.5, 0, 0.5, 1, 10, 1100)
  # Einstein-de Sitter: t = 2 / (3 H0) (1 + z)^(-3/2)
  expect_lt(rel_err(cosmic_time(model_eds(), z),
                    2 / 3 * t_H(0.7) * (1 + z)^-1.5), 1e-9)
  # Milne: t = 1 / (H0 (1 + z))
  expect_lt(rel_err(cosmic_time(model_milne(), z), t_H(0.7) / (1 + z)), 1e-9)
  # radiation only: t = 1 / (2 H0 (1 + z)^2)
  expect_lt(rel_err(cosmic_time(model_radiation(), z),
                    t_H(0.7) / (2 * (1 + z)^2)), 1e-9)
  # open, matter only: H0 t0 = Om / (2 (1 - Om)^(3/2)) (sinh(eta) - eta)
  # with cosh(eta) = 1 + 2 (1 - Om) / Om
  Om <- 0.3
  eta <- acosh(1 + 2 * (1 - Om) / Om)
  expect_equal(age_of_universe(model_open()),
               Om / (2 * (1 - Om)^1.5) * (sinh(eta) - eta) * t_H(0.7),
               tolerance = 1e-10)
  # closed, Omega_m = 2: a = 1 - cos(theta), H0 t = theta - sin(theta)
  expect_equal(age_of_universe(model_closed()), (pi / 2 - 1) * t_H(0.7),
               tolerance = 1e-10)
  theta <- acos(1 - 1 / 0.6) # z = -0.4, a = 5 / 3 (future)
  expect_equal(cosmic_time(model_closed(), -0.4),
               (theta - sin(theta)) * t_H(0.7), tolerance = 1e-10)
})

test_that("cosmic_time() returns Inf for de Sitter and stops where E(z)^2 <= 0", {
  expect_identical(age_of_universe(model_de_sitter()), Inf)
  expect_identical(cosmic_time(model_de_sitter(), c(1, NA, -0.5)),
                   c(Inf, NA, Inf))
  # the closed Omega_m = 2 model stops expanding at a = 2 (z = -0.5)
  expect_error(cosmic_time(model_closed(), -0.5),
               "E(z)^2 <= 0 somewhere in the range z >= -0.5", fixed = TRUE)
  expect_error(cosmic_time(model_closed(), c(1, -0.6)), "E(z)^2 <= 0",
               fixed = TRUE)
  # a bouncing model never had a = 0
  expect_error(age_of_universe(model_bounce()),
               "E(z)^2 <= 0 somewhere in the range z >= 0", fixed = TRUE)
  expect_error(cosmic_time(model_bounce(), 0.5), "E(z)^2 <= 0", fixed = TRUE)
  # negative Lambda: recollapse where 1.2 (1 + z)^3 = 0.2, i.e. z = -0.45
  recollapse <- cosmology_model(Omega_m = 1.2, Omega_Lambda = -0.2)
  expect_gt(cosmic_time(recollapse, -0.4), age_of_universe(recollapse))
  expect_error(cosmic_time(recollapse, -0.5), "E(z)^2 <= 0", fixed = TRUE)
})

test_that("cosmic_time() units and vectorization agree with age_of_universe()", {
  cosmo <- readme_model()
  z <- c(3, 0, 0.5, -0.5, 1100)
  t_gyr <- cosmic_time(cosmo, z)
  expect_equal(cosmic_time(cosmo, z, unit = "year"), t_gyr * 1e9)
  expect_equal(t_gyr, vapply(z, function(zi) cosmic_time(cosmo, zi),
                             numeric(1)))
  expect_identical(age_of_universe(cosmo), cosmic_time(cosmo, 0))
  expect_identical(age_of_universe(cosmo, unit = "year"),
                   cosmic_time(cosmo, 0, unit = "year"))
  expect_true(all(diff(cosmic_time(cosmo, c(1100, 10, 1, 0, -0.5))) > 0))
})

test_that("README-model age agrees with independent integrations", {
  # 13.7972897813562 Gyr from a composite Simpson rule in sqrt(a), from
  # stats::integrate over z and from a deSolve ODE in ln(a), computed
  # separately with the IAU parsec and the Julian year (agreement < 1e-12)
  expect_equal(age_of_universe(readme_model()), 13.7972897813562,
               tolerance = 1e-10)
  expect_equal(age_of_universe(readme_model(), unit = "year"),
               13.7972897813562e9, tolerance = 1e-10)
})


# ---- redshift_to_scale_factor() / scale_factor_to_redshift() -------------

test_that("redshift and scale factor convert both ways", {
  expect_equal(redshift_to_scale_factor(c(0, 1, 3, 1100, -0.5)),
               c(1, 0.5, 0.25, 1 / 1101, 2))
  expect_equal(scale_factor_to_redshift(c(1, 0.5, 0.25, 2)), c(0, 1, 3, -0.5))
  z <- c(-0.9, 0.1, 2, 1e4)
  expect_lt(rel_err(scale_factor_to_redshift(redshift_to_scale_factor(z)), z),
            1e-12)
  a <- c(1e-4, 0.3, 1, 5)
  expect_lt(rel_err(redshift_to_scale_factor(scale_factor_to_redshift(a)), a),
            1e-12)
  expect_identical(redshift_to_scale_factor(c(1, NA)), c(0.5, NA))
  expect_identical(scale_factor_to_redshift(NA), NA_real_)
  expect_null(names(redshift_to_scale_factor(c(z1 = 1))))
  expect_null(names(scale_factor_to_redshift(c(a1 = 0.5))))
})

test_that("redshift and scale factor conversions validate input", {
  expect_error(redshift_to_scale_factor(-1), "'z' must be greater than -1.",
               fixed = TRUE)
  expect_error(redshift_to_scale_factor(c(0, -3)),
               "'z' must be greater than -1.", fixed = TRUE)
  expect_error(redshift_to_scale_factor("1"), "'z' must be numeric.",
               fixed = TRUE)
  expect_error(scale_factor_to_redshift(0), "'a' must be positive.",
               fixed = TRUE)
  expect_error(scale_factor_to_redshift(c(1, -0.5)), "'a' must be positive.",
               fixed = TRUE)
  expect_error(scale_factor_to_redshift(list(1)), "'a' must be numeric.",
               fixed = TRUE)
})


# ---- Removed internals --------------------------------------------------------

test_that("the pre-0.4.0 cosmology internals are gone", {
  ns <- asNamespace("astronomR")
  old <- c("FlatLCDM", "ClosedLCDM", "OpenLCDM", "H_by_H0", "tH0_as_fn_a",
           "t_as_func_a_in_Year", "comv_dist", "metric_distance", "a",
           "redshift", "sec_to_year", "a_of_t")
  for (nm in old) {
    expect_false(exists(nm, envir = ns, inherits = FALSE), label = nm)
  }
})
