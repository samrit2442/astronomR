#' Fundamental Physical Constants in SI and Natural Units
#'
#' A dataset containing commonly used physical constants in both SI units and
#' natural units. The dataset includes the constant's name, symbol, value in
#' SI units, SI unit, value in natural units, and natural unit representation.
#'
#' @format A tibble with 19 rows and 6 columns:
#' \describe{
#'   \item{name}{Character. Name of the physical constant.}
#'   \item{symbol}{Character. Symbol representing the constant.}
#'   \item{value_SI}{Numeric. The value of the constant in SI units.}
#'   \item{unit_SI}{Character. The SI unit for the constant.}
#'   \item{value_Natural}{Numeric. The value of the constant in natural units.}
#'   \item{unit_Natural}{Character. The unit of the constant in natural units.}
#' }
#' @return A \code{tibble} (data frame) with 19 rows and 6 columns:
#' \describe{
#'   \item{name}{Character. Full name of the physical constant.}
#'   \item{symbol}{Character. Symbol representing the constant.}
#'   \item{value_SI}{Numeric. Value of the constant in SI units.}
#'   \item{unit_SI}{Character. The SI unit string for the constant.}
#'   \item{value_Natural}{Numeric. Value of the constant in natural units.}
#'   \item{unit_Natural}{Character. The natural unit string for the constant.}
#' }
#' @examples
#' constants_df
#' @export
constants_df <- tibble::tribble(
  ~name,                      ~symbol,    ~value_SI,        ~unit_SI,  ~value_Natural, ~unit_Natural,
  "Speed of Light",           "c0",       299792458,        "m/s",                1, "-",
  "Magnetic Constant",        "mu0",      4e-7 * pi,        "N A^(-2)",           1, "-",
  "Electric Constant",        "epsilon0", 8.854187817e-12,  "F/m",                1, "-",
  "Gravitational Constant",   "G",        6.67408e-11,      "m^3 kg^(-1) s^(-2)", 1, "-",
  "Planck Constant",          "h",        6.626070040e-34,  "J s",                1, "-",
  "Elementary Charge",        "e",        1.6021766208e-19, "C",                  1, "-",
  "Magnetic Flux Quantum",    "phi0",     2.067833831e-15,  "Wb",                 1, "-",
  "Conductance Quantum",      "G0",       7.7480917310e-5,  "S",                  1, "-",
  "Electron Mass",            "m_e",      9.10938356e-31,   "kg",                 5.10999e5, "eV",
  "Proton Mass",              "m_p",      1.672621898e-27,  "kg",                 1, "-",
  "Fine-Structure Constant",  "alpha",    7.2973525664e-3,  "-",                  1, "-",
  "Rydberg Constant",         "R_inf",    10973731.568508,  "m^(-1)",             1, "-",
  "Avogadro Constant",        "N_A",      6.022140857e23,   "mol^(-1)",           1, "-",
  "Faraday Constant",         "F",        96485.33289,      "C mol^(-1)",         1, "-",
  "Molar Gas Constant",       "R",        8.3144598,        "J mol^(-1) K^(-1)",  1, "-",
  "Boltzman Constant",        "k",        1.38064852e-23,   "J K^(-1)",           1, "-",
  "Stefan-Boltzman Constant", "sigma",    5.670367e-8,      "W m^(-2) K^(-4)",    1, "-",
  "Electron Volt",            "eV",       1.6021766208e-19, "J",                  1, "-",
  "Atomic Mass Unit",         "u",        1.660539040e-27,  "kg",                 1, "-"
)


#' Retrieve the Value of a Physical Constant
#'
#' Retrieves the value and unit of a specified physical constant from the
#' \code{constants_df} dataset. You can search by the constant's name and
#' choose whether to retrieve the value in SI units or natural units.
#'
#' @param constant_name Character. The name (or part of the name) of the
#'   constant to search for. Case-insensitive and partial matches are allowed.
#' @param unit Character. The unit system to retrieve the constant in. Options
#'   are:
#'   \itemize{
#'     \item \code{"SI"}: Retrieves the value in SI units (default).
#'     \item \code{"Natural"}: Retrieves the value in natural units.
#'   }
#' @return A list containing:
#'   \itemize{
#'     \item \code{name}: The full name of the constant.
#'     \item \code{value}: The numeric value of the constant.
#'     \item \code{unit}: The unit string for the selected unit system.
#'   }
#' @examples
#' constant_value("speed of light", unit = "SI")
#' constant_value("planck", unit = "SI")
#' constant_value("electron mass", unit = "Natural")
#' @export
constant_value <- function(constant_name, unit = "SI") {
  matching_rows <- constants_df[
    stringr::str_detect(tolower(constants_df$name), tolower(constant_name)),
  ]

  if (nrow(matching_rows) == 1) {
    constant_row <- matching_rows
  } else if (nrow(matching_rows) == 0) {
    stop("No constant found matching that name.")
  } else {
    stop("Multiple constants match that name. Please specify further.")
  }

  if (unit == "SI") {
    value <- constant_row$value_SI
    unit_value <- constant_row$unit_SI
  } else if (unit == "Natural") {
    value <- constant_row$value_Natural
    unit_value <- constant_row$unit_Natural
  } else {
    stop("Invalid unit. Use 'SI' or 'Natural'.")
  }

  list(
    name = constant_row$name,
    value = value,
    unit = unit_value
  )
}
