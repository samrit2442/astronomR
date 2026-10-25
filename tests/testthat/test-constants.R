# Reference values are CODATA 2018 (Tiesinga et al. 2021, Rev. Mod. Phys. 93,
# 025010; https://physics.nist.gov/cuu/Constants/), typed in independently of
# the package code. Values that CODATA prints as "8.314 462 618..." are exact
# but truncated.

si  <- function(sym) constants_df$value_SI[constants_df$symbol == sym]
nat <- function(sym) constants_df$value_Natural[constants_df$symbol == sym]

# expect_equal()'s tolerance turns into an absolute difference when
# |expected| < tolerance, so it cannot tell h = 6.6e-34 from 0. Compare
# relative errors instead.
rel_err <- function(x, ref) abs(x / ref - 1)

c_ref   <- 299792458        # m s^-1
h_ref   <- 6.62607015e-34   # J s
e_ref   <- 1.602176634e-19  # C
k_ref   <- 1.380649e-23     # J K^-1
N_A_ref <- 6.02214076e23    # mol^-1


# ---- constants_df: structure ------------------------------------------------

test_that("constants_df is a 19 x 6 tibble with the expected column types", {
  expect_s3_class(constants_df, "tbl_df")
  expect_identical(dim(constants_df), c(19L, 6L))
  expect_named(constants_df, c("name", "symbol", "value_SI", "unit_SI",
                               "value_Natural", "unit_Natural"))
  expect_type(constants_df$name, "character")
  expect_type(constants_df$symbol, "character")
  expect_type(constants_df$value_SI, "double")
  expect_type(constants_df$unit_SI, "character")
  expect_type(constants_df$value_Natural, "double")
  expect_type(constants_df$unit_Natural, "character")
})

test_that("constants_df has the expected names, symbols and units, in order", {
  expect_identical(constants_df$name, c(
    "Speed of Light", "Magnetic Constant", "Electric Constant",
    "Gravitational Constant", "Planck Constant", "Elementary Charge",
    "Magnetic Flux Quantum", "Conductance Quantum", "Electron Mass",
    "Proton Mass", "Fine-Structure Constant", "Rydberg Constant",
    "Avogadro Constant", "Faraday Constant", "Molar Gas Constant",
    "Boltzmann Constant", "Stefan-Boltzmann Constant", "Electron Volt",
    "Atomic Mass Unit"
  ))
  expect_identical(constants_df$symbol, c(
    "c0", "mu0", "epsilon0", "G", "h", "e", "phi0", "G0", "m_e", "m_p",
    "alpha", "R_inf", "N_A", "F", "R", "k", "sigma", "eV", "u"
  ))
  expect_identical(constants_df$unit_SI, c(
    "m/s", "N A^(-2)", "F/m", "m^3 kg^(-1) s^(-2)", "J s", "C", "Wb", "S",
    "kg", "kg", "-", "m^(-1)", "mol^(-1)", "C mol^(-1)", "J mol^(-1) K^(-1)",
    "J K^(-1)", "W m^(-2) K^(-4)", "J", "kg"
  ))
  expect_identical(constants_df$unit_Natural, c(
    "-", "-", "-", "eV^(-2)", "-", "-", "-", "-", "eV", "eV", "-", "eV",
    NA, NA, NA, "-", "-", "eV", "eV"
  ))
  # Lookups rely on names and symbols being unique regardless of case
  expect_identical(anyDuplicated(tolower(constants_df$name)), 0L)
  expect_identical(anyDuplicated(tolower(constants_df$symbol)), 0L)
  expect_false(anyNA(constants_df$value_SI))
  expect_true(all(constants_df$value_SI > 0))
})


# ---- constants_df: SI values -------------------------------------------------

test_that("the defining constants have their exact SI values", {
  expect_identical(si("c0"), c_ref)
  expect_identical(si("h"), h_ref)
  expect_identical(si("e"), e_ref)
  expect_identical(si("k"), k_ref)
  expect_identical(si("N_A"), N_A_ref)
  expect_identical(si("eV"), e_ref)   # 1 eV = e joules
})

test_that("the exact derived constants follow from the defining constants", {
  expect_lt(rel_err(si("R"), N_A_ref * k_ref), 1e-15)
  expect_lt(rel_err(si("F"), N_A_ref * e_ref), 1e-15)
  expect_lt(rel_err(si("phi0"), h_ref / (2 * e_ref)), 1e-15)
  expect_lt(rel_err(si("G0"), 2 * e_ref^2 / h_ref), 1e-15)
  expect_lt(rel_err(si("sigma"),
                    2 * pi^5 * k_ref^4 / (15 * h_ref^3 * c_ref^2)), 1e-14)
  # CODATA 2018 printed values (exact, truncated to 10 significant digits)
  expect_lt(rel_err(si("R"), 8.314462618), 1e-9)
  expect_lt(rel_err(si("F"), 96485.33212), 1e-9)
  expect_lt(rel_err(si("phi0"), 2.067833848e-15), 1e-9)
  expect_lt(rel_err(si("G0"), 7.748091729e-5), 1e-9)
  expect_lt(rel_err(si("sigma"), 5.670374419e-8), 1e-9)
})

test_that("the measured constants have their CODATA 2018 values", {
  expect_identical(si("mu0"), 1.25663706212e-6)
  expect_identical(si("epsilon0"), 8.8541878128e-12)
  expect_identical(si("G"), 6.67430e-11)
  expect_identical(si("m_e"), 9.1093837015e-31)
  expect_identical(si("m_p"), 1.67262192369e-27)
  expect_identical(si("alpha"), 7.2973525693e-3)
  expect_identical(si("R_inf"), 10973731.568160)
  expect_identical(si("u"), 1.66053906660e-27)
})

test_that("the measured constants agree with other CODATA 2018 values", {
  # mu0 is no longer exactly 4 pi 1e-7: mu0 / (4 pi 1e-7) = 1.00000000055(15)
  expect_lt(abs(si("mu0") / (4e-7 * pi) - 1 - 5.5e-10), 1.5e-10)
  expect_lt(rel_err(si("mu0") * si("epsilon0") * c_ref^2, 1), 1e-12)
  expect_lt(rel_err(si("mu0"), 2 * si("alpha") * h_ref / (e_ref^2 * c_ref)),
            1e-10)
  expect_lt(rel_err(1 / si("alpha"), 137.035999084), 1e-11)
  expect_lt(rel_err(si("R_inf"),
                    si("alpha")^2 * si("m_e") * c_ref / (2 * h_ref)), 1e-10)
  expect_lt(rel_err(si("m_p") / si("m_e"), 1836.15267343), 1e-10)
  expect_lt(rel_err(si("m_e") / si("u"), 5.48579909065e-4), 1e-10)
  # Planck mass sqrt(hbar c / G) = 2.176434e-8 kg
  expect_lt(rel_err(sqrt(h_ref / (2 * pi) * c_ref / si("G")), 2.176434e-8),
            1e-6)
})


# ---- constants_df: natural-unit values ---------------------------------------

test_that("natural-unit values use hbar = c = k_B = epsilon0 = 1", {
  alpha <- 7.2973525693e-3
  hbar  <- h_ref / (2 * pi)
  for (sym in c("c0", "mu0", "epsilon0", "k")) {
    expect_identical(nat(sym), 1)
  }
  expect_lt(rel_err(nat("h"), 2 * pi), 1e-15)
  # Heaviside-Lorentz: alpha = e^2 / (4 pi), and e = e_SI / sqrt(eps0 hbar c)
  expect_lt(rel_err(nat("e")^2 / (4 * pi), alpha), 1e-14)
  expect_lt(rel_err(nat("e"),
                    e_ref / sqrt(si("epsilon0") * hbar * c_ref)), 1e-10)
  expect_identical(nat("alpha"), alpha)
  expect_lt(rel_err(nat("phi0"), nat("h") / (2 * nat("e"))), 1e-14)
  expect_lt(rel_err(nat("G0"), 2 * nat("e")^2 / nat("h")), 1e-14)
  expect_lt(rel_err(nat("G0"), 4 * alpha), 1e-14)
  # sigma = pi^2 k^4 / (60 hbar^3 c^2)
  expect_lt(rel_err(nat("sigma"), pi^2 / 60), 1e-14)
  expect_lt(rel_err(si("sigma") * hbar^3 * c_ref^2 / k_ref^4, nat("sigma")),
            1e-14)
  expect_identical(nat("eV"), 1)
})

test_that("natural-unit energies match the CODATA 2018 energy equivalents", {
  # m c^2: m_e 0.51099895000 MeV, m_p 938.27208816 MeV, u 931.49410242 MeV
  expect_lt(rel_err(nat("m_e"), 0.51099895000e6), 1e-10)
  expect_lt(rel_err(nat("m_p"), 938.27208816e6), 1e-10)
  expect_lt(rel_err(nat("u"), 931.49410242e6), 1e-10)
  # hbar c R_inf = Rydberg energy h c R_inf (13.605693122994 eV) / (2 pi)
  expect_lt(rel_err(nat("R_inf") * 2 * pi, 13.605693122994), 1e-12)
  # G / (hbar c) = 6.70883e-39 GeV^-2 = 1 / M_Pl^2, M_Pl c^2 = 1.220890e19 GeV
  expect_lt(rel_err(nat("G"), 6.70883e-57), 1e-6)
  expect_lt(rel_err(1 / sqrt(nat("G")), 1.220890e28), 1e-6)
})

test_that("mole-based constants have no natural-unit value", {
  for (sym in c("N_A", "F", "R")) {
    expect_identical(nat(sym), NA_real_)
    expect_identical(
      constants_df$unit_Natural[constants_df$symbol == sym], NA_character_
    )
  }
  expect_identical(sum(is.na(constants_df$value_Natural)), 3L)
})


# ---- constant_value(): lookups -----------------------------------------------

test_that("constant_value() returns the name, value and unit", {
  expect_identical(
    constant_value("speed of light"),
    list(name = "Speed of Light", value = 299792458, unit = "m/s")
  )
  expect_identical(
    constant_value("Planck Constant", unit = "Natural"),
    list(name = "Planck Constant", value = 2 * pi, unit = "-")
  )
  expect_identical(
    constant_value("Gravitational Constant", unit = "Natural")$unit, "eV^(-2)"
  )
  expect_identical(constant_value("h"), constant_value("h", unit = "SI"))
  expect_identical(constant_value("h", unit = c("SI", "Natural")),
                   constant_value("h", unit = "SI"))
})

test_that("constant_value() matches full names whatever their case", {
  for (nm in c("Speed of Light", "speed of light", "SPEED OF LIGHT",
               "sPeEd Of LiGhT")) {
    expect_identical(constant_value(nm)$name, "Speed of Light")
  }
  # An exact name wins although "Stefan-Boltzmann Constant" also contains it
  expect_identical(constant_value("Boltzmann Constant")$name,
                   "Boltzmann Constant")
  expect_identical(constant_value("boltzmann constant")$value, k_ref)
  expect_identical(constant_value("STEFAN-BOLTZMANN CONSTANT")$name,
                   "Stefan-Boltzmann Constant")
})

test_that("every constant is found by its own name and by its symbol", {
  for (i in seq_len(nrow(constants_df))) {
    by_name   <- constant_value(constants_df$name[i])
    by_symbol <- constant_value(constants_df$symbol[i])
    expect_identical(by_name$name, constants_df$name[i])
    expect_identical(by_name$value, constants_df$value_SI[i])
    expect_identical(by_symbol$name, constants_df$name[i])
  }
})

test_that("constant_value() matches symbols exactly, ignoring case", {
  # A partial match on "k" would only find "Planck Constant"
  expect_identical(constant_value("k")$name, "Boltzmann Constant")
  expect_identical(constant_value("h")$name, "Planck Constant")
  expect_identical(constant_value("G")$name, "Gravitational Constant")
  expect_identical(constant_value("g")$name, "Gravitational Constant")
  expect_identical(constant_value("G0")$name, "Conductance Quantum")
  expect_identical(constant_value("e")$name, "Elementary Charge")
  expect_identical(constant_value("EV")$name, "Electron Volt")
  expect_identical(constant_value("R")$name, "Molar Gas Constant")
  expect_identical(constant_value("r_inf")$name, "Rydberg Constant")
  expect_identical(constant_value("u")$value, 1.66053906660e-27)
})

test_that("constant_value() falls back to a partial match on the name", {
  expect_identical(constant_value("planck")$name, "Planck Constant")
  expect_identical(constant_value("RYDBERG")$name, "Rydberg Constant")
  expect_identical(constant_value("flux")$name, "Magnetic Flux Quantum")
  expect_identical(constant_value("fine-structure")$name,
                   "Fine-Structure Constant")
  expect_identical(constant_value("Stefan-Boltzmann")$name,
                   "Stefan-Boltzmann Constant")
  expect_identical(constant_value("proton")$value, 1.67262192369e-27)
})

test_that("constant_value() accepts the pre-0.4.0 spelling 'Boltzman'", {
  expect_identical(constant_value("Boltzman Constant"),
                   constant_value("Boltzmann Constant"))
  expect_identical(constant_value("boltzman constant")$value, k_ref)
  expect_identical(constant_value("Stefan-Boltzman Constant")$name,
                   "Stefan-Boltzmann Constant")
  expect_identical(constant_value("stefan-boltzman")$name,
                   "Stefan-Boltzmann Constant")
})

test_that("constant_value() ignores leading and trailing white space", {
  # Untrimmed, "k " and "G " are only parts of "Planck Constant" and
  # "Rydberg Constant", and " G" is part of "Molar Gas Constant"
  expect_identical(constant_value("k "), constant_value("k"))
  expect_identical(constant_value("k ")$value, k_ref)
  expect_identical(constant_value("G ")$name, "Gravitational Constant")
  expect_identical(constant_value(" G")$name, "Gravitational Constant")
  expect_identical(constant_value("\th\r\n")$name, "Planck Constant")
  expect_identical(constant_value(" e ", unit = "Natural"),
                   constant_value("e", unit = "Natural"))
  expect_identical(constant_value("  Boltzmann Constant  ")$name,
                   "Boltzmann Constant")
  expect_identical(constant_value(" Boltzman Constant")$value, k_ref)
  expect_identical(constant_value(" planck ")$name, "Planck Constant")
  # White space inside the name is kept
  expect_identical(constant_value(" of light ")$name, "Speed of Light")
  # Messages quote the trimmed name
  expect_error(constant_value(" dark matter\t"),
               "No constant found matching 'dark matter'.", fixed = TRUE)
  expect_error(constant_value("electron "),
               "Multiple constants match 'electron': Electron Mass, Electron Volt.",
               fixed = TRUE)
})


# ---- constant_value(): errors and warnings -----------------------------------

test_that("an ambiguous name is an error listing the candidates", {
  expect_error(constant_value("electron"),
               "Multiple constants match 'electron': Electron Mass, Electron Volt.",
               fixed = TRUE)
  expect_error(constant_value("mass"),
               "Electron Mass, Proton Mass, Atomic Mass Unit.", fixed = TRUE)
  expect_error(constant_value("boltzmann"),
               "Boltzmann Constant, Stefan-Boltzmann Constant.", fixed = TRUE)
  expect_error(constant_value("Boltzman"),
               "Boltzmann Constant, Stefan-Boltzmann Constant.", fixed = TRUE)
  expect_error(constant_value("constant"), "Multiple constants match",
               fixed = TRUE)
})

test_that("an unknown name is an error", {
  expect_error(constant_value("dark matter"),
               "No constant found matching 'dark matter'.", fixed = TRUE)
})

test_that("regex metacharacters in the name are matched literally", {
  # As regexes, "(" and "+" are invalid and "." matches every name
  expect_error(constant_value("("), "No constant found matching '('",
               fixed = TRUE)
  expect_error(constant_value("+"), "No constant found matching '+'",
               fixed = TRUE)
  expect_error(constant_value("."), "No constant found matching '.'",
               fixed = TRUE)
  expect_error(constant_value("speed.of.light"), "No constant found",
               fixed = TRUE)
  expect_error(constant_value("[a-z]"), "No constant found", fixed = TRUE)
  # Only the two hyphenated names contain a literal "-"
  expect_error(constant_value("-"),
               "Fine-Structure Constant, Stefan-Boltzmann Constant.",
               fixed = TRUE)
})

test_that("an invalid 'unit' is an error", {
  expect_error(constant_value("planck", unit = "CGS"), "should be one of")
  expect_error(constant_value("planck", unit = "si"), "should be one of")
  expect_error(constant_value("planck", unit = c("Natural", "SI")),
               "must be of length 1")
  expect_error(constant_value("planck", unit = NA), "character vector")
})

test_that("'constant_name' must be a single non-NA, non-empty string", {
  msg <- "'constant_name' must be a single non-NA, non-empty string."
  expect_error(constant_value(NA), msg, fixed = TRUE)
  expect_error(constant_value(NA_character_), msg, fixed = TRUE)
  expect_error(constant_value(""), msg, fixed = TRUE)
  # Blank strings are empty once trimmed (untrimmed, " " is part of every
  # name with two or more words)
  expect_error(constant_value(" "), msg, fixed = TRUE)
  expect_error(constant_value(" \t\r\n "), msg, fixed = TRUE)
  expect_error(constant_value(c("h", "k")), msg, fixed = TRUE)
  expect_error(constant_value(character(0)), msg, fixed = TRUE)
  expect_error(constant_value(NULL), msg, fixed = TRUE)
  expect_error(constant_value(42), msg, fixed = TRUE)
})

test_that("a missing natural-unit value is NA with a warning", {
  for (nm in c("Avogadro Constant", "Faraday Constant", "Molar Gas Constant")) {
    expect_warning(res <- constant_value(nm, unit = "Natural"),
                   paste0("'", nm, "' has no value in natural units"),
                   fixed = TRUE)
    expect_identical(res, list(name = nm, value = NA_real_,
                               unit = NA_character_))
  }
  expect_warning(constant_value("N_A", unit = "Natural"), "returning NA",
                 fixed = TRUE)
  # SI values exist for every constant, so no warning there
  expect_no_warning(res <- constant_value("N_A"))
  expect_identical(res$value, N_A_ref)
  expect_no_warning(constant_value("electron mass", unit = "Natural"))
})
