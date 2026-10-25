# ---- Internal physical constants shared across the package -----------------
# CODATA 2018 values (h, e, k_B and N_A are exact by definition since 2019)
# and the IAU 2012/2015 astronomical unit and parsec.
.c_SI       <- 299792458                # speed of light [m s^-1], exact
.h_SI       <- 6.62607015e-34           # Planck constant [J s], exact
.e_SI       <- 1.602176634e-19          # elementary charge [C], exact
.k_B_SI     <- 1.380649e-23             # Boltzmann constant [J K^-1], exact
.N_A        <- 6.02214076e23            # Avogadro constant [mol^-1], exact
.k_B_eV     <- .k_B_SI / .e_SI          # Boltzmann constant [eV K^-1]
.m_e_eV     <- 0.51099895000e6          # electron rest energy [eV]
.c_km_s     <- .c_SI / 1000             # speed of light [km s^-1]
.km_per_Mpc <- 648000 / pi * 149597870.7 * 1e6  # 1 Mpc [km], IAU parsec
.s_per_yr   <- 365.25 * 86400           # Julian year [s]
.T_cmb0     <- 2.7255                   # CMB temperature today [K] (Fixsen 2009)


#' Fundamental Physical Constants in SI and Natural Units
#'
#' A table of 19 fundamental physical constants with their CODATA 2018
#' recommended values in SI units and in natural units.
#'
#' The SI values are the CODATA 2018 values, published by NIST in May 2019.
#' The speed of light \eqn{c} has been exact since 1983. Since the revision
#' of the SI on 20 May 2019, the Planck constant \eqn{h}, the elementary
#' charge \eqn{e}, the Boltzmann constant \eqn{k} and the Avogadro constant
#' \eqn{N_A} are exact as well, and so are the constants derived from these
#' five alone: the molar gas constant \eqn{R = N_A k}, the Faraday constant
#' \eqn{F = N_A e}, the Stefan-Boltzmann constant
#' \eqn{\sigma = 2\pi^5 k^4 / (15 h^3 c^2)}{sigma = 2 pi^5 k^4 / (15 h^3 c^2)},
#' the magnetic flux quantum \eqn{\Phi_0 = h / (2e)}{phi0 = h / (2 e)}, the
#' conductance quantum \eqn{G_0 = 2e^2 / h}{G0 = 2 e^2 / h} and the electron
#' volt (\eqn{e} joules). The magnetic constant \eqn{\mu_0}{mu0} is now
#' measured and is no longer exactly
#' \eqn{4\pi \times 10^{-7}}{4 pi x 10^-7} N A^-2.
#'
#' The natural-unit values use
#' \eqn{\hbar = c = k_B = \epsilon_0 = 1}{hbar = c = k_B = epsilon0 = 1}
#' (the Heaviside-Lorentz convention), with energies in electron volts. In
#' this system \eqn{h = 2\pi}{h = 2 pi} and the elementary charge is
#' \eqn{e = \sqrt{4\pi\alpha}}{e = sqrt(4 pi alpha)}, about 0.303, so that
#' \eqn{\Phi_0 = \pi / e}{phi0 = pi / e}, \eqn{G_0 = 4\alpha}{G0 = 4 alpha}
#' and \eqn{\sigma = \pi^2 / 60}{sigma = pi^2 / 60}. Masses are given as rest
#' energies \eqn{m c^2} in eV, and the Rydberg constant as the energy
#' \eqn{\hbar c R_\infty}{hbar c R_inf} in eV (the Rydberg energy
#' \eqn{h c R_\infty}{h c R_inf}, about 13.6 eV, is \eqn{2\pi}{2 pi} times
#' larger). The gravitational constant becomes
#' \eqn{G = 1 / M_{\mathrm{Pl}}^2}{G = 1 / M_Pl^2} in eV^-2, where
#' \eqn{M_{\mathrm{Pl}}}{M_Pl} is the Planck mass.
#'
#' @docType data
#' @keywords datasets
#' @format A tibble with 19 rows and 6 columns:
#' \describe{
#'   \item{name}{Character. Full name of the constant, e.g.
#'     \code{"Planck Constant"}.}
#'   \item{symbol}{Character. Short symbol, e.g. \code{"h"}.}
#'   \item{value_SI}{Numeric. CODATA 2018 value in SI units.}
#'   \item{unit_SI}{Character. SI unit of \code{value_SI}; \code{"-"} means
#'     dimensionless.}
#'   \item{value_Natural}{Numeric. Value in natural units with
#'     \eqn{\hbar = c = k_B = \epsilon_0 = 1}{hbar = c = k_B = epsilon0 = 1}
#'     (Heaviside-Lorentz) and energies in eV; see Details. \code{NA} for the
#'     Avogadro constant, the Faraday constant and the molar gas constant,
#'     which are defined per mole and have no natural-unit value.}
#'   \item{unit_Natural}{Character. Unit of \code{value_Natural}:
#'     \code{"-"} (dimensionless), \code{"eV"} or \code{"eV^(-2)"};
#'     \code{NA} where \code{value_Natural} is \code{NA}.}
#' }
#' @return A tibble; see the Format section.
#' @source CODATA 2018 recommended values of the fundamental physical
#'   constants: E. Tiesinga, P. J. Mohr, D. B. Newell and B. N. Taylor (2021),
#'   Rev. Mod. Phys. 93, 025010. \url{https://physics.nist.gov/cuu/Constants/}
#' @seealso \code{\link{constant_value}} to look up a single constant.
#' @examples
#' constants_df
#'
#' # The mole-based constants have no natural-unit value
#' constants_df[is.na(constants_df$value_Natural), c("name", "value_SI")]
#' @export
constants_df <- local({
  # CODATA 2018 measured values
  mu0   <- 1.25663706212e-6    # magnetic constant [N A^-2]
  eps0  <- 8.8541878128e-12    # electric constant [F m^-1]
  G     <- 6.67430e-11         # Newtonian constant of gravitation [m^3 kg^-1 s^-2]
  m_e   <- 9.1093837015e-31    # electron mass [kg]
  m_p   <- 1.67262192369e-27   # proton mass [kg]
  alpha <- 7.2973525693e-3     # fine-structure constant [-]
  R_inf <- 10973731.568160     # Rydberg constant [m^-1]
  m_u   <- 1.66053906660e-27   # atomic mass constant [kg]

  # Exact, derived from the defining constants
  phi0  <- .h_SI / (2 * .e_SI)
  G0    <- 2 * .e_SI^2 / .h_SI
  sigma <- 2 * pi^5 * .k_B_SI^4 / (15 * .h_SI^3 * .c_SI^2)

  # Natural units: hbar = c = k_B = epsilon0 = 1 (Heaviside-Lorentz), eV
  hbar      <- .h_SI / (2 * pi)
  e_nat     <- sqrt(4 * pi * alpha)              # alpha = e^2 / (4 pi)
  G_nat     <- G * .e_SI^2 / (hbar * .c_SI^5)    # [eV^-2], 1 / M_Pl^2
  R_inf_nat <- hbar * .c_SI * R_inf / .e_SI      # [eV]
  mc2_eV    <- function(m) m * .c_SI^2 / .e_SI   # rest energy [eV]

  tibble::tribble(
    ~name,                       ~symbol,    ~value_SI,      ~unit_SI,             ~value_Natural, ~unit_Natural,
    "Speed of Light",            "c0",       .c_SI,          "m/s",                1,              "-",
    "Magnetic Constant",         "mu0",      mu0,            "N A^(-2)",           1,              "-",
    "Electric Constant",         "epsilon0", eps0,           "F/m",                1,              "-",
    "Gravitational Constant",    "G",        G,              "m^3 kg^(-1) s^(-2)", G_nat,          "eV^(-2)",
    "Planck Constant",           "h",        .h_SI,          "J s",                2 * pi,         "-",
    "Elementary Charge",         "e",        .e_SI,          "C",                  e_nat,          "-",
    "Magnetic Flux Quantum",     "phi0",     phi0,           "Wb",                 pi / e_nat,     "-",
    "Conductance Quantum",       "G0",       G0,             "S",                  e_nat^2 / pi,   "-",
    "Electron Mass",             "m_e",      m_e,            "kg",                 mc2_eV(m_e),    "eV",
    "Proton Mass",               "m_p",      m_p,            "kg",                 mc2_eV(m_p),    "eV",
    "Fine-Structure Constant",   "alpha",    alpha,          "-",                  alpha,          "-",
    "Rydberg Constant",          "R_inf",    R_inf,          "m^(-1)",             R_inf_nat,      "eV",
    "Avogadro Constant",         "N_A",      .N_A,           "mol^(-1)",           NA_real_,       NA_character_,
    "Faraday Constant",          "F",        .N_A * .e_SI,   "C mol^(-1)",         NA_real_,       NA_character_,
    "Molar Gas Constant",        "R",        .N_A * .k_B_SI, "J mol^(-1) K^(-1)",  NA_real_,       NA_character_,
    "Boltzmann Constant",        "k",        .k_B_SI,        "J K^(-1)",           1,              "-",
    "Stefan-Boltzmann Constant", "sigma",    sigma,          "W m^(-2) K^(-4)",    pi^2 / 60,      "-",
    "Electron Volt",             "eV",       .e_SI,          "J",                  1,              "eV",
    "Atomic Mass Unit",          "u",        m_u,            "kg",                 mc2_eV(m_u),    "eV"
  )
})


#' Retrieve the Value of a Physical Constant
#'
#' Looks up a physical constant in \code{\link{constants_df}} by name or
#' symbol and returns its value and unit in SI or natural units.
#'
#' Matching ignores case and leading or trailing white space, and is tried
#' in three steps: an exact match on the full name (e.g.
#' \code{"Boltzmann Constant"}), then an exact match on the symbol (e.g.
#' \code{"k"}), then a partial match on the name (e.g. \code{"planck"}). The
#' first step that finds anything decides the result: a single match is
#' returned, and several matches are an error that lists them. The partial
#' match looks for \code{constant_name} as a fixed string, not a regular
#' expression, so characters such as \code{"."}, \code{"+"} or \code{"("}
#' have no special meaning. The spelling \code{"Boltzman"} used for two
#' constants before astronomR 0.4.0 is still accepted.
#'
#' Natural units are
#' \eqn{\hbar = c = k_B = \epsilon_0 = 1}{hbar = c = k_B = epsilon0 = 1}
#' with energies in eV (see \code{\link{constants_df}}). The Avogadro
#' constant, the Faraday constant and the molar gas constant have no
#' natural-unit value: for these, \code{unit = "Natural"} gives \code{NA} for
#' both \code{value} and \code{unit}, with a warning.
#'
#' @param constant_name Character string. Name, symbol or part of the name of
#'   the constant, e.g. \code{"Planck Constant"}, \code{"h"} or
#'   \code{"planck"}. Case and leading or trailing white space are ignored.
#' @param unit Character. Unit system of the result: \code{"SI"} (default) or
#'   \code{"Natural"}.
#' @return A list with elements \code{name} (character, full name of the
#'   constant), \code{value} (numeric) and \code{unit} (character, unit of
#'   \code{value}; \code{"-"} if dimensionless). \code{value} and \code{unit}
#'   are \code{NA}, with a warning, when the constant has no value in the
#'   requested unit system.
#' @seealso \code{\link{constants_df}} for the full table.
#' @examples
#' constant_value("speed of light")
#' constant_value("k")                          # by symbol
#' constant_value("planck", unit = "Natural")   # h = 2 pi
#' constant_value("electron mass", unit = "Natural")
#'
#' # Several partial matches are an error that lists them
#' try(constant_value("electron"))
#'
#' # Mole-based constants have no natural-unit value (NA, with a warning)
#' constant_value("Avogadro", unit = "Natural")
#' @export
constant_value <- function(constant_name, unit = c("SI", "Natural")) {
  if (!is.character(constant_name) || length(constant_name) != 1L ||
      is.na(constant_name) || !nzchar(trimws(constant_name))) {
    stop("'constant_name' must be a single non-NA, non-empty string.")
  }
  unit <- match.arg(unit)

  # Ignore case and surrounding white space ("k " must not partially match
  # "Planck Constant"); "boltzmann?" also accepts the pre-0.4.0 "Boltzman".
  constant_name <- trimws(constant_name)
  query   <- gsub("boltzmann?", "boltzmann", tolower(constant_name))
  name_lc <- tolower(constants_df$name)
  hits <- which(name_lc == query)
  if (length(hits) == 0L) hits <- which(tolower(constants_df$symbol) == query)
  if (length(hits) == 0L) hits <- which(grepl(query, name_lc, fixed = TRUE))

  if (length(hits) == 0L) {
    stop("No constant found matching '", constant_name,
         "'. See 'constants_df' for the available names and symbols.")
  }
  if (length(hits) > 1L) {
    stop("Multiple constants match '", constant_name, "': ",
         paste(constants_df$name[hits], collapse = ", "),
         ". Please specify further.")
  }

  if (unit == "SI") {
    value <- constants_df$value_SI[hits]
    unit_value <- constants_df$unit_SI[hits]
  } else {
    value <- constants_df$value_Natural[hits]
    unit_value <- constants_df$unit_Natural[hits]
    if (is.na(value)) {
      warning("'", constants_df$name[hits], "' has no value in natural ",
              "units; returning NA.")
    }
  }

  list(name = constants_df$name[hits], value = value, unit = unit_value)
}
