zeta_3 <- 1.20205690316
EI <- 13.598434    # Hydrogen ionization energy [eV] (NIST)
# Model parameters (not constants): baryon-to-photon ratio eta and hydrogen
# mass fraction, giving n_H = 0.76 * eta * n_gamma
baryon_to_photon_ratio <- 6e-10
nb <- 0.76 * baryon_to_photon_ratio * (2 * zeta_3) / (pi^2)

# Convert redshift to the radiation temperature in eV, T = T_0 (1 + z)
z_to_temp_eV <- function(z) {
  .k_B_eV * .T_cmb0 * (1 + z)
}

pre_fac_saha <- nb * (2 * pi / .m_e_eV)^(3 / 2)


#' Photon Energy Density as a Function of Temperature
#'
#' Calculates the energy density of blackbody photons at temperature
#' \eqn{T}, using
#' \deqn{\rho_\gamma = \frac{\pi^2}{15} T^4}{rho_gamma = (pi^2 / 15) T^4}
#' in natural units (\eqn{\hbar = c = k_B = 1}{hbar = c = k_B = 1}).
#'
#' A temperature given in Kelvin is first converted to eV with the exact
#' Boltzmann constant \eqn{k_B = 8.617333 \times 10^{-5}}{k_B = 8.617333e-5}
#' eV/K. To obtain an energy density per volume, divide the result by
#' \eqn{(\hbar c)^3}{(hbar c)^3}, with \eqn{\hbar c = 1.973270 \times
#' 10^{-5}}{hbar c = 1.973270e-5} eV cm: for the CMB today (2.7255 K) this
#' gives about 0.2606 eV cm\eqn{^{-3}}{^-3}.
#'
#' @param temp Numeric vector of non-negative temperatures, in eV or Kelvin.
#' @param unit Character. The unit of the temperature input. Either
#'   \code{"eV"} (default) or \code{"K"}.
#' @return Numeric vector, the same length as \code{temp}. The photon energy
#'   density in natural units, \eqn{\mathrm{eV}^4}{eV^4} (with \eqn{T} in
#'   eV).
#' @seealso \code{\link{photon_energy_density_fn_z}},
#'   \code{\link{photon_number_density_fn_T}}
#' @examples
#' photon_energy_density_fn_T(1, "eV")
#' photon_energy_density_fn_T(300, "K")
#' # CMB today in eV cm^-3
#' photon_energy_density_fn_T(2.7255, "K") / 1.973270e-5^3
#' @export
photon_energy_density_fn_T <- function(temp, unit = "eV") {
  if (!is.numeric(temp)) stop("'temp' must be numeric.")
  if (any(temp < 0, na.rm = TRUE)) stop("'temp' must be non-negative.")
  if (!is.character(unit) || length(unit) != 1L || !(unit %in% c("eV", "K"))) {
    stop("'unit' must be either 'eV' or 'K'.")
  }
  if (unit == "K") temp <- .k_B_eV * temp
  (pi^2 / 15) * temp^4
}


#' Photon Number Density as a Function of Temperature
#'
#' Calculates the number density of blackbody photons at temperature
#' \eqn{T}, using
#' \deqn{n_\gamma = \frac{2\zeta(3)}{\pi^2} T^3}{n_gamma = (2 zeta(3) / pi^2) T^3}
#' in natural units (\eqn{\hbar = c = k_B = 1}{hbar = c = k_B = 1}).
#'
#' A temperature given in Kelvin is first converted to eV with the exact
#' Boltzmann constant \eqn{k_B = 8.617333 \times 10^{-5}}{k_B = 8.617333e-5}
#' eV/K. To obtain a number per volume, divide the result by
#' \eqn{(\hbar c)^3}{(hbar c)^3}, with \eqn{\hbar c = 1.973270 \times
#' 10^{-5}}{hbar c = 1.973270e-5} eV cm: for the CMB today (2.7255 K) this
#' gives about 410.7 photons cm\eqn{^{-3}}{^-3}.
#'
#' @param temp Numeric vector of non-negative temperatures, in eV or Kelvin.
#' @param unit Character. The unit of the temperature input. Either
#'   \code{"eV"} (default) or \code{"K"}.
#' @return Numeric vector, the same length as \code{temp}. The photon number
#'   density in natural units, \eqn{\mathrm{eV}^3}{eV^3} (with \eqn{T} in
#'   eV).
#' @seealso \code{\link{photon_number_density_fn_z}},
#'   \code{\link{photon_energy_density_fn_T}}
#' @examples
#' photon_number_density_fn_T(1, "eV")
#' photon_number_density_fn_T(300, "K")
#' # CMB today in photons per cm^3
#' photon_number_density_fn_T(2.7255, "K") / 1.973270e-5^3
#' @export
photon_number_density_fn_T <- function(temp, unit = "eV") {
  if (!is.numeric(temp)) stop("'temp' must be numeric.")
  if (any(temp < 0, na.rm = TRUE)) stop("'temp' must be non-negative.")
  if (!is.character(unit) || length(unit) != 1L || !(unit %in% c("eV", "K"))) {
    stop("'unit' must be either 'eV' or 'K'.")
  }
  if (unit == "K") temp <- .k_B_eV * temp
  (2 * zeta_3 / pi^2) * temp^3
}


#' Photon Energy Density as a Function of Redshift
#'
#' Calculates the energy density of the cosmic microwave background photons
#' at redshift \eqn{z}, with temperature \eqn{T = T_0 (1 + z)}{T = T_0 (1 + z)}
#' and \eqn{T_0 = 2.7255} K (Fixsen 2009), using
#' \eqn{\rho_\gamma = \pi^2 T^4 / 15}{rho_gamma = pi^2 T^4 / 15}.
#'
#' @param z Numeric vector of redshifts, \eqn{z > -1}.
#' @return Numeric vector, the same length as \code{z}. The photon energy
#'   density in natural units, \eqn{\mathrm{eV}^4}{eV^4}; see
#'   \code{\link{photon_energy_density_fn_T}} for the conversion to
#'   eV cm\eqn{^{-3}}{^-3}.
#' @references Fixsen, D. J. (2009). The Temperature of the Cosmic Microwave
#'   Background. Astrophysical Journal, 707, 916.
#' @seealso \code{\link{photon_energy_density_fn_T}},
#'   \code{\link{photon_number_density_fn_z}}
#' @examples
#' photon_energy_density_fn_z(1300)
#' photon_energy_density_fn_z(c(0, 1100))
#' @export
photon_energy_density_fn_z <- function(z) {
  if (!is.numeric(z)) stop("'z' must be numeric.")
  if (any(z <= -1, na.rm = TRUE)) stop("'z' must be greater than -1.")
  (pi^2 / 15) * z_to_temp_eV(z)^4
}


#' Photon Number Density as a Function of Redshift
#'
#' Calculates the number density of the cosmic microwave background photons
#' at redshift \eqn{z}, with temperature \eqn{T = T_0 (1 + z)}{T = T_0 (1 + z)}
#' and \eqn{T_0 = 2.7255} K (Fixsen 2009), using
#' \eqn{n_\gamma = 2\zeta(3) T^3 / \pi^2}{n_gamma = 2 zeta(3) T^3 / pi^2}.
#'
#' @param z Numeric vector of redshifts, \eqn{z > -1}.
#' @return Numeric vector, the same length as \code{z}. The photon number
#'   density in natural units, \eqn{\mathrm{eV}^3}{eV^3}; see
#'   \code{\link{photon_number_density_fn_T}} for the conversion to
#'   cm\eqn{^{-3}}{^-3}.
#' @references Fixsen, D. J. (2009). The Temperature of the Cosmic Microwave
#'   Background. Astrophysical Journal, 707, 916.
#' @seealso \code{\link{photon_number_density_fn_T}},
#'   \code{\link{photon_energy_density_fn_z}}
#' @examples
#' photon_number_density_fn_z(1300)
#' photon_number_density_fn_z(c(0, 1100))
#' @export
photon_number_density_fn_z <- function(z) {
  if (!is.numeric(z)) stop("'z' must be numeric.")
  if (any(z <= -1, na.rm = TRUE)) stop("'z' must be greater than -1.")
  (2 * zeta_3 / pi^2) * z_to_temp_eV(z)^3
}


#' Hydrogen Ionization Fraction from the Saha Equation
#'
#' \code{Saha_Xe()} calculates the equilibrium ionization fraction
#' \eqn{X_e} of hydrogen at redshift \eqn{z} from the Saha equation.
#' \code{soln_saha()} returns \eqn{X_e - 0.5}, whose root (for example via
#' \code{\link[stats]{uniroot}}) is the redshift at which hydrogen is half
#' ionized.
#'
#' In natural units the Saha equation for hydrogen reads
#' \deqn{\frac{X_e^2}{1 - X_e} = \frac{1}{n_H}
#'   \left(\frac{m_e T}{2\pi}\right)^{3/2} e^{-E_I/T}}{X_e^2 / (1 - X_e) =
#'   (m_e T / (2 pi))^(3/2) exp(-E_I / T) / n_H}
#' with \eqn{T = T_0 (1 + z)}{T = T_0 (1 + z)}, \eqn{T_0 = 2.7255} K,
#' \eqn{E_I = 13.598434} eV (NIST), \eqn{m_e = 0.51099895} MeV and the
#' hydrogen density \eqn{n_H = 0.76\, \eta\, n_\gamma(T)}{n_H = 0.76 eta
#' n_gamma(T)}, where the baryon-to-photon ratio is
#' \eqn{\eta = 6 \times 10^{-10}}{eta = 6e-10}. Writing the right-hand side
#' as \eqn{1/f}, the physical root of \eqn{f X_e^2 + X_e - 1 = 0} is
#' evaluated in the cancellation-free form
#' \deqn{X_e = \frac{2}{1 + \sqrt{1 + 4f}}}{X_e = 2 / (1 + sqrt(1 + 4 f))}
#' so \eqn{X_e} stays in \eqn{[0, 1]}: it tends to 1 at high redshift and is
#' exactly 0 once \eqn{f} overflows (below \eqn{z \approx 80}{z ~ 80}).
#'
#' Saha equilibrium describes the start of recombination but predicts it to
#' finish too early; see \code{\link{peebles_rhs}} for the
#' out-of-equilibrium Peebles equation.
#'
#' @param z Numeric vector of redshifts, \eqn{z > -1}.
#' @return For \code{Saha_Xe()}, a numeric vector the same length as
#'   \code{z}: the dimensionless ionization fraction \eqn{X_e}, between 0
#'   and 1. For \code{soln_saha()}, the numeric vector \eqn{X_e - 0.5}.
#' @seealso \code{\link{peebles_rhs}}, \code{\link{photon_number_density_fn_z}}
#' @examples
#' Saha_Xe(1300)
#' Saha_Xe(c(1100, 1300, 1500))
#' soln_saha(1350)
#' # Redshift at which hydrogen is half ionized in Saha equilibrium
#' uniroot(soln_saha, c(1000, 2000))$root
#' @export
Saha_Xe <- function(z) {
  if (!is.numeric(z)) stop("'z' must be numeric.")
  if (any(z <= -1, na.rm = TRUE)) stop("'z' must be greater than -1.")
  temp <- z_to_temp_eV(z)
  f <- pre_fac_saha * temp^(3 / 2) * exp(EI / temp)
  2 / (1 + sqrt(1 + 4 * f))
}


#' @rdname Saha_Xe
#' @export
soln_saha <- function(z) {
  Saha_Xe(z) - 0.5
}
