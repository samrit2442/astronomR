zeta_3 <- 1.20205690316
EI <- 13.6         # Hydrogen ionization energy (eV)
Me <- 5.10999e5    # Electron mass (eV)
baryon_to_photon_ratio <- 6e-10
nb <- 0.76 * baryon_to_photon_ratio * (2 * zeta_3) / (pi^2)

# Convert redshift to temperature in eV
z_to_temp_eV <- function(z) {
  z0 <- 2.725 / (1.160451812e4)
  z0 * (1 + z)
}

pre_fac_saha <- nb * (2 * pi / Me)^(3 / 2)


#' Photon Energy Density as a Function of Temperature
#'
#' Calculates the photon energy density for a given temperature \eqn{T},
#' using \eqn{\rho_\gamma = \frac{\pi^2}{15} T^4}.
#'
#' @param temp Numeric. Temperature in either eV or Kelvin.
#' @param unit Character. The unit of the temperature input. Either
#'   \code{"eV"} (default) or \code{"K"}.
#' @return Numeric. The photon energy density in natural units.
#' @examples
#' photon_energy_density_fn_T(1, "eV")
#' photon_energy_density_fn_T(300, "K")
#' @export
photon_energy_density_fn_T <- function(temp, unit = "eV") {
  if (unit == "eV") {
    (pi^2 / 15) * temp^4
  } else if (unit == "K") {
    (pi^2 / 15) * (8.6173e-5 * temp)^4
  } else {
    stop("'unit' must be either 'eV' or 'K'.")
  }
}


#' Photon Number Density as a Function of Temperature
#'
#' Calculates the photon number density for a given temperature \eqn{T},
#' using \eqn{n_\gamma = \frac{2\zeta(3)}{\pi^2} T^3}.
#'
#' @param temp Numeric. Temperature in either eV or Kelvin.
#' @param unit Character. The unit of the temperature input. Either
#'   \code{"eV"} (default) or \code{"K"}.
#' @return Numeric. The photon number density in natural units.
#' @examples
#' photon_number_density_fn_T(1, "eV")
#' photon_number_density_fn_T(300, "K")
#' @export
photon_number_density_fn_T <- function(temp, unit = "eV") {
  if (unit == "eV") {
    (2 * zeta_3 / pi^2) * temp^3
  } else if (unit == "K") {
    (2 * zeta_3 / pi^2) * (8.6173e-5 * temp)^3
  } else {
    stop("'unit' must be either 'eV' or 'K'.")
  }
}


#' Photon Energy Density as a Function of Redshift
#'
#' Calculates the photon energy density at a given cosmological redshift
#' \eqn{z} by first converting to temperature in eV.
#'
#' @param z Numeric. The redshift value.
#' @return Numeric. The photon energy density in natural units.
#' @examples
#' photon_energy_density_fn_z(1300)
#' @export
photon_energy_density_fn_z <- function(z) {
  (pi^2 / 15) * z_to_temp_eV(z)^4
}


#' Photon Number Density as a Function of Redshift
#'
#' Calculates the photon number density at a given cosmological redshift
#' \eqn{z} by first converting to temperature in eV.
#'
#' @param z Numeric. The redshift value.
#' @return Numeric. The photon number density in natural units.
#' @examples
#' photon_number_density_fn_z(1300)
#' @export
photon_number_density_fn_z <- function(z) {
  (2 * zeta_3 / pi^2) * z_to_temp_eV(z)^3
}


#' Ionization Fraction Using the Saha Equation
#'
#' Calculates the ionization fraction \eqn{X_e} as a function of redshift
#' \eqn{z} using the Saha equation for hydrogen recombination.
#'
#' @param z Numeric. The redshift value.
#' @return Numeric. The ionization fraction \eqn{X_e} (between 0 and 1).
#' @examples
#' Saha_Xe(1300)
#' Saha_Xe(1100)
#' @export
Saha_Xe <- function(z) {
  temp <- z_to_temp_eV(z)
  f <- pre_fac_saha * temp^(3 / 2) * exp(EI / temp)
  (-1 + sqrt(1 + 4 * f)) / (2 * f)
}


#' Deviation of Ionization Fraction from 0.5
#'
#' Returns \eqn{X_e(z) - 0.5}, useful for finding the redshift at which the
#' ionization fraction equals 0.5 (e.g., via root-finding).
#'
#' @param z Numeric. The redshift value.
#' @return Numeric. \eqn{X_e - 0.5}.
#' @examples
#' soln_saha(1350)
#' @export
soln_saha <- function(z) {
  Saha_Xe(z) - 0.5
}
