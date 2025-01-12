# Create the object
# const_m_e_


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
  "Electron Mass",            "m_e",      9.10938356e-31,   "kg",                 1, "-",
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


constant_value <- function(constant_name, unit = "SI") {
  # Filter the tibble to get rows where constant_name contains the user input
  matching_rows <- constants_df[stringr::str_detect(tolower(constants_df$name), tolower(constant_name)), ]

  # Check if there's an exact match or one matching row
  if (nrow(matching_rows) == 1) {
    constant_row <- matching_rows
  } else if (nrow(matching_rows) == 0) {
    stop("No constant found matching that name.")
  } else {
    stop("Multiple constants match that name. Please specify further.")
  }

  # Select the value and unit based on the unit argument
  if (unit == "SI") {
    value <- constant_row$value_SI
    unit_value <- constant_row$unit_SI
  } else if (unit == "Natural") {
    value <- constant_row$value_natural
    unit_value <- constant_row$unit_natural
  } else {
    stop("Invalid unit. Use 'SI' or 'Natural'.")
  }

  # Return the result as a list
  return(list(
    name = constant_row$name,
    value = value,
    unit = unit_value
  ))
}

constant_value("stefan")
















