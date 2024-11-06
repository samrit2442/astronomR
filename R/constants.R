# Create the object
const_c0 <- 299792458
const_mu0 <- 4e-7 * pi
const_epsilon0 <- 8.854187817e-12
const_G <- 6.67408e-11
const_h <- 6.626070040e-34
const_e <- 1.6021766208e-19
const_phi0 <- 2.067833831e-15
const_G0 <- 7.7480917310e-5
const_m_e <- 9.10938356e-31
const_m_p <- 1.672621898e-27
# const_m_e_
const_alpha <- 7.2973525664e-3
const_alpha_inverse <- 1/const_alpha
const_R_infinity <- 10973731.568508
const_L <- 6.022140857e23
const_F <- 96485.33289
const_R <- 8.3144598
cosnt_k <- 1.38064852e-23
const_sigma <- 5.670367e-8
const_eV <- 1.6021766208e-19
const_u <- 1.660539040e-27


constants_df <- tibble::tribble(
  ~name, ~symbol, ~value_SI, ~unit_SI, ~value_Natural, ~unit_Natural,
  "Speed of Light", "c0", 299792458, "m/s", 1, "m/s"

)


constant_value <- function(constant_name, unit = "SI") {
  # Filter the tibble to get rows where constant_name contains the user input
  matching_rows <- constants_df[stringr::str_detect(constants_df$name, constant_name), ]

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

# constant_value("Light")
















