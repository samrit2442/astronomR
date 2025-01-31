zeta_3 <- 1.20205690316
EI <- 13.6 # Hydrogen Binding (eV)
Me <- 5.10999e5 # Mass of electron use the table
zeq <- 3395 # Redshift for matter-radiation equality
H0 <- 1.5e-33
z0 <- 2.725 / (1.160451812e4)
baryon_to_photon_ratio <- 6e-10
nb <- 0.76 * baryon_to_photon_ratio * (2 * zeta_3) / (pi^2) # Baryon number density

# Conversion function from z to T (in eV)
T <- function(z) {
  z0 <- 2.725 / (1.160451812e4)
  return(z0 * (1 + z)) # BigFloat equivalent in R is not needed for this calculation
}

# Pre-factor for the Saha equation
pre_fac_saha <- nb * (2 * pi / Me)^(3 / 2)


#' Photon Energy Density as a Function of Temperature
#'
#' This function calculates the photon energy density for a given temperature \(T\).
#'
#' @param T Numeric. Temperature in either eV or Kelvin.
#' @param unit Character. The unit of temperature, either "eV" or "K". Defaults to "eV".
#'
#' @return Numeric. The photon energy density.
#'
#' @examples
#' photon_energy_density_fn_T(1, "eV")
#' photon_energy_density_fn_T(300, "K")
#' @export
photon_energy_density_fn_T <- function(T, unit = "eV") { # T is in eV
  if (unit == "eV") {
    a <- (pi^2 / 15) * T^4
  } else if (unit == "K") {
    a <- (pi^2 / 15) * (8.6173e-5 * T)^4
  } else {
    print("error unit can only take eV or K")
    a <- NULL
  }
  return(a)
}


#' Photon Number Density as a Function of Temperature
#'
#' This function calculates the photon number density for a given temperature \(T\).
#'
#' @param T Numeric. Temperature in either eV or Kelvin.
#' @param unit Character. The unit of temperature, either "eV" or "K". Defaults to "eV".
#'
#' @return Numeric. The photon number density.
#'
#' @examples
#' photon_number_density_fn_T(1, "eV")
#' photon_number_density_fn_T(300, "K")
#' @export
photon_number_density_fn_T <- function(T, unit = "eV") { # T is in eV
  if (unit == "eV") {
    a <- (2 * zeta_3 / pi^2) * T^3
  } else if (unit == "K") {
    a <- (2 * zeta_3 / pi^2) * (8.6173e-5 * T)^3
  } else {
    print("error unit can only take eV or K")
    a <- NULL
  }
  return(a)
}


#' Photon Energy Density as a Function of Redshift
#'
#' This function calculates the photon energy density for a given redshift \(z\).
#'
#' @param z Numeric. The redshift value.
#'
#' @return Numeric. The photon energy density.
#'
#' @examples
#' photon_energy_density_fn_z(1300)
#' @export
photon_energy_density_fn_z <- function(z) {
  return((pi^2 / 15) * T(z)^4)
}


#' Photon Number Density as a Function of Redshift
#'
#' This function calculates the photon number density for a given redshift \(z\).
#'
#' @param z Numeric. The redshift value.
#'
#' @return Numeric. The photon number density.
#'
#' @examples
#' photon_number_density_fn_z(1300)
#' @export
photon_number_density_fn_z <- function(z) {
  return((2 * zeta_3 / pi^2) * T(z)^3)
}


#' Ionization Fraction Using the Saha Equation
#'
#' This function calculates the ionization fraction \(X_e\) as a function of redshift \(z\) using the Saha equation.
#'
#' @param z Numeric. The redshift value.
#'
#' @return Numeric. The ionization fraction \(X_e\).
#'
#' @examples
#' Saha_Xe(1300)
#' @export
Saha_Xe <- function(z) {
  f <- pre_fac_saha * T(z)^(3 / 2) * exp(EI / T(z))
  return((-1 + sqrt(1 + 4 * f)) / (2 * f))
}


#' Solution for Ionization Fraction \(X_e = 0.5\)
#'
#' This function solves the equation \(X_e - 0.5 = 0\) for the redshift \(z\).
#'
#' @param z Numeric. The redshift value.
#'
#' @return Numeric. The result of the equation \(X_e - 0.5\).
#'
#' @examples
#' soln_saha(1350)
#' @export
soln_saha <- function(z) {
  return(Saha_Xe(z) - 0.5)
}
