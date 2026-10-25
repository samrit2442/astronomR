# ============================================================
# 08_thermal_cosmology.R
# Thermal Cosmology of the Early Universe
#
# Functions implement the key equations governing the thermal
# history of the early universe, including:
#   - Hubble rate in the radiation-dominated era
#   - Effective relativistic degrees of freedom (g_*)
#   - Entropy density and entropy degrees of freedom (g_*S)
#   - Equilibrium number & yield densities
#   - The Boltzmann relic-abundance (Pebble) equation  dY/dx
#   - Freeze-out temperature solver
#   - Peebles equation for hydrogen recombination
#
# Sections 1-8 use natural units (hbar = c = k_B = 1) with
# temperature T in GeV and mass m in GeV.
# Section 9 (peebles_rhs) uses SI units (metres, seconds).
# ============================================================

# ---- Physical constants in natural units -------------------
# Reduced Planck mass  M_Pl = 1 / sqrt(8 pi G)  [GeV]
M_Pl_reduced <- 2.435e18   # GeV  (M_Pl = 1.221e19 / sqrt(8 pi))
GeV_to_inv_s <- 1.519e24   # 1 GeV  = 1.519e24  s^{-1}  (hbar = 1)

# ---- Standard Model g_*(T) step table ------------------------
# Particle-mass thresholds [GeV] and the g_* value in each half-open bin
# (lower, upper]; the value for T <= 5.11e-4 GeV depends on the type.
.g_star_T_edges <- c(5.11e-4, 0.1057, 0.1396, 0.15, 1.27, 1.777, 4.18,
                     80.4, 125, 173)
.g_star_steps   <- c(10.75, 14.25, 17.25, 61.75, 72.25, 75.75, 86.25,
                     95.25, 96.25, 106.75)

# TRUE for a single finite number
.thermal_is_number <- function(x) {
  is.numeric(x) && length(x) == 1L && is.finite(x)
}


# ============================================================
#  1.  HUBBLE RATE IN THE RADIATION-DOMINATED ERA
# ============================================================

#' Hubble Rate in the Radiation-Dominated Era
#'
#' Computes the Hubble expansion rate \eqn{H(T)} during the
#' radiation-dominated epoch using the Friedmann equation:
#' \deqn{H(T) = \sqrt{\frac{\pi^2}{90}\, g_*(T)}\;
#'              \frac{T^2}{M_{\mathrm{Pl}}}}{H(T) = sqrt(pi^2 g_*(T) / 90)
#'              T^2 / M_Pl}
#' where \eqn{M_{\mathrm{Pl}} = 2.435 \times 10^{18}\ \mathrm{GeV}}{M_Pl =
#' 2.435e18 GeV} is the reduced Planck mass and \eqn{g_*(T)} is the
#' effective number of relativistic degrees of freedom.
#'
#' @param T_GeV  Numeric.  Plasma temperature in GeV.
#' @param g_star Numeric.  Effective relativistic degrees of freedom
#'   \eqn{g_*(T)}.  Defaults to 106.75 (Standard Model value above
#'   the electroweak scale); see \code{\link{g_star_eff}} for other
#'   temperatures.
#'
#' @return Numeric.  Hubble rate \eqn{H} in GeV (natural units).
#'
#' @references
#'   Kolb, E. W. & Turner, M. S. (1990). \emph{The Early Universe}.
#'   Addison-Wesley, Eq. (3.46).
#'
#' @seealso \code{\link{g_star_eff}}, \code{\link{boltzmann_pebble_rhs}}
#'
#' @examples
#' # H at T = 100 GeV with SM degrees of freedom
#' hubble_radiation(100)
#'
#' # H at T = 1 MeV (neutrino decoupling era), g* ~ 10.75
#' hubble_radiation(1e-3, g_star = 10.75)
#' @export
hubble_radiation <- function(T_GeV, g_star = 106.75) {
  if (any(T_GeV  <= 0, na.rm = TRUE)) stop("'T_GeV' must be positive.")
  if (any(g_star <= 0, na.rm = TRUE)) stop("'g_star' must be positive.")
  sqrt((pi^2 / 90) * g_star) * T_GeV^2 / M_Pl_reduced
}


# ============================================================
#  2.  EFFECTIVE RELATIVISTIC DEGREES OF FREEDOM
# ============================================================

#' Effective Relativistic Degrees of Freedom g_*(T)
#'
#' Returns a step-function approximation of the Standard Model effective
#' number of relativistic degrees of freedom as a function of temperature:
#' \eqn{g_*(T)}{g_*(T)}, which sets the radiation energy density
#' \eqn{\rho = (\pi^2/30)\, g_* T^4}{rho = (pi^2 / 30) g_* T^4}, or
#' \eqn{g_{*S}(T)}{g_*S(T)}, which sets the entropy density
#' \eqn{s = (2\pi^2/45)\, g_{*S} T^3}{s = (2 pi^2 / 45) g_*S T^3}.
#'
#' Each relativistic species \eqn{i} with \eqn{g_i} internal degrees of
#' freedom and temperature \eqn{T_i} contributes
#' \deqn{g_* = \sum_{\rm bosons} g_i \left(\frac{T_i}{T}\right)^4 +
#'   \frac{7}{8} \sum_{\rm fermions} g_i \left(\frac{T_i}{T}\right)^4}{g_* =
#'   sum_bosons g_i (T_i / T)^4 + (7/8) sum_fermions g_i (T_i / T)^4}
#' and likewise for \eqn{g_{*S}}{g_*S} with the third power of
#' \eqn{T_i/T}. This function uses the instantaneous-decoupling step
#' approximation: a species counts fully while \eqn{T} is above its mass
#' threshold and not at all below it. The bins are half-open,
#' \eqn{T_{\rm low} < T \le T_{\rm high}}{T_low < T <= T_high}, with
#' \eqn{T} in GeV:
#' \tabular{lrrl}{
#'   T (GeV) \tab g_* \tab g_*S \tab removed relative to the row above \cr
#'   above 173 \tab 106.75 \tab 106.75 \tab none (all SM particles) \cr
#'   125 to 173 \tab 96.25 \tab 96.25 \tab top quark (173 GeV) \cr
#'   80.4 to 125 \tab 95.25 \tab 95.25 \tab Higgs boson (125 GeV) \cr
#'   4.18 to 80.4 \tab 86.25 \tab 86.25 \tab W and Z bosons (both at 80.4 GeV) \cr
#'   1.777 to 4.18 \tab 75.75 \tab 75.75 \tab bottom quark (4.18 GeV) \cr
#'   1.27 to 1.777 \tab 72.25 \tab 72.25 \tab tau lepton (1.777 GeV) \cr
#'   0.15 to 1.27 \tab 61.75 \tab 61.75 \tab charm quark (1.27 GeV) \cr
#'   0.1396 to 0.15 \tab 17.25 \tab 17.25 \tab QCD transition (150 MeV) \cr
#'   0.1057 to 0.1396 \tab 14.25 \tab 14.25 \tab pions (139.6 MeV) \cr
#'   5.11e-4 to 0.1057 \tab 10.75 \tab 10.75 \tab muon (105.7 MeV) \cr
#'   up to 5.11e-4 \tab 3.36 \tab 3.91 \tab electron and positron (511 keV)
#' }
#' At the QCD transition the u, d and s quarks and the gluons are replaced
#' by the three pions.
#' The last row counts photons and three neutrino species that decoupled
#' before e+e- annihilation and are colder by \eqn{(4/11)^{1/3}}{(4/11)^(1/3)}:
#' \eqn{g_* = 2 + \frac{7}{8} \cdot 6 \cdot (4/11)^{4/3} = 3.36}{g_* = 2 +
#' (7/8) 6 (4/11)^(4/3) = 3.36} and
#' \eqn{g_{*S} = 2 + \frac{7}{8} \cdot 6 \cdot \frac{4}{11} = 3.91}{g_*S =
#' 2 + (7/8) 6 (4/11) = 3.91}. At higher temperatures all species share one
#' temperature and \eqn{g_{*S} = g_*}{g_*S = g_*}. The step values agree
#' with the instantaneous-decoupling values of Husdal (2016); the true
#' \eqn{g_*(T)} is smooth across each threshold, most visibly around the
#' QCD transition.
#'
#' @param T_GeV Numeric vector of positive temperatures in GeV, without
#'   missing values.
#' @param type Character. \code{"energy"} (default) for \eqn{g_*}{g_*}, the
#'   degrees of freedom in the energy density, or \code{"entropy"} for
#'   \eqn{g_{*S}}{g_*S}, those in the entropy density.
#'
#' @return Numeric vector, the same length as \code{T_GeV}: the
#'   dimensionless \eqn{g_*}{g_*} or \eqn{g_{*S}}{g_*S}.
#'
#' @references
#'   Husdal, L. (2016). On Effective Degrees of Freedom in the Early
#'   Universe. Galaxies, 4, 78. arXiv:1609.04979.
#'
#'   Kolb, E. W. & Turner, M. S. (1990). \emph{The Early Universe}.
#'   Addison-Wesley, Chapter 3.
#'
#' @seealso \code{\link{hubble_radiation}}, \code{\link{entropy_density}},
#'   \code{\link{equilibrium_yield}}, \code{\link{boltzmann_pebble_rhs}}
#'
#' @examples
#' g_star_eff(100)                     # 95.25: below the Higgs, above W/Z
#' g_star_eff(1e-3)                    # 10.75: photons, e+-, 3 neutrinos
#' g_star_eff(1e-4)                    # 3.36: after e+e- annihilation
#' g_star_eff(1e-4, type = "entropy")  # 3.91
#' g_star_eff(c(500, 1, 0.01))         # 106.75 61.75 10.75
#' @export
g_star_eff <- function(T_GeV, type = c("energy", "entropy")) {
  type <- match.arg(type)
  if (!is.numeric(T_GeV)) stop("'T_GeV' must be numeric.")
  if (anyNA(T_GeV))       stop("'T_GeV' must not contain missing values.")
  if (any(T_GeV <= 0))    stop("'T_GeV' must be positive.")
  g_low <- if (type == "energy") 3.36 else 3.91
  bin <- findInterval(T_GeV, .g_star_T_edges, left.open = TRUE)
  c(g_low, .g_star_steps)[bin + 1L]
}


# ============================================================
#  3.  ENTROPY DENSITY
# ============================================================

#' Entropy Density of the Thermal Bath
#'
#' Returns the entropy density \eqn{s(T)} of the cosmological
#' radiation bath:
#' \deqn{s(T) = \frac{2\pi^2}{45}\, g_{*S}(T)\, T^3}{s(T) = (2 pi^2 / 45)
#'   g_*S(T) T^3}
#' where \eqn{g_{*S}}{g_*S} is the effective number of entropy degrees of
#' freedom.
#'
#' @param T_GeV  Numeric.  Temperature in GeV.
#' @param g_star_S Numeric.  Effective entropy degrees of freedom
#'   \eqn{g_{*S}(T)}{g_*S(T)}.  If \code{NULL} (default), computed with
#'   \code{\link{g_star_eff}(T_GeV, type = "entropy")}.
#'
#' @return Numeric.  Entropy density \eqn{s} in
#'   \eqn{\mathrm{GeV}^3}{GeV^3}.
#'
#' @references
#'   Kolb & Turner (1990), Eq. (3.65).
#'
#' @seealso \code{\link{g_star_eff}}, \code{\link{equilibrium_yield}}
#'
#' @examples
#' entropy_density(0.1)
#' entropy_density(1, g_star_S = 106.75)
#' @export
entropy_density <- function(T_GeV, g_star_S = NULL) {
  if (any(T_GeV <= 0, na.rm = TRUE)) stop("'T_GeV' must be positive.")
  if (is.null(g_star_S)) g_star_S <- g_star_eff(T_GeV, type = "entropy")
  (2 * pi^2 / 45) * g_star_S * T_GeV^3
}


# ============================================================
#  4.  EQUILIBRIUM NUMBER DENSITY (Maxwell-Boltzmann)
# ============================================================

#' Equilibrium Number Density of a Massive Particle Species
#'
#' Computes the equilibrium number density \eqn{n^{\rm eq}(T)}{n_eq(T)}
#' for a non-relativistic species in the Maxwell-Boltzmann limit:
#' \deqn{n^{\rm eq}(T) = g\,\left(\frac{mT}{2\pi}\right)^{3/2}
#'                        e^{-m/T}}{n_eq(T) = g (m T / (2 pi))^(3/2)
#'                        exp(-m / T)}
#'
#' @param T_GeV Numeric.  Temperature in GeV.
#' @param m_GeV Numeric.  Particle mass in GeV.
#' @param g_dof Integer.  Internal degrees of freedom. Default 2.
#'
#' @return Numeric.  Equilibrium number density in
#'   \eqn{\mathrm{GeV}^3}{GeV^3}.
#'
#' @references
#'   Kolb & Turner (1990), Eq. (5.25).
#'
#' @seealso \code{\link{equilibrium_yield}}
#'
#' @examples
#' equilibrium_number_density(T_GeV = 0.01, m_GeV = 0.1, g_dof = 2)
#' @export
equilibrium_number_density <- function(T_GeV, m_GeV, g_dof = 2) {
  if (any(T_GeV <= 0, na.rm = TRUE)) stop("'T_GeV' must be positive.")
  if (any(m_GeV <= 0, na.rm = TRUE)) stop("'m_GeV' must be positive.")
  x <- m_GeV / T_GeV
  g_dof * (m_GeV * T_GeV / (2 * pi))^(3 / 2) * exp(-x)
}


# ============================================================
#  5.  EQUILIBRIUM YIELD  Y_eq(x)
# ============================================================

#' Equilibrium Yield Y_eq as a Function of x = m/T
#'
#' The yield \eqn{Y \equiv n / s}{Y = n / s} is the comoving number density
#' (number per comoving entropy).  In equilibrium:
#' \deqn{Y^{\rm eq}(x) = \frac{45\, g}{4\pi^4\, g_{*S}}\; x^2\, K_2(x)}{Y_eq(x)
#'   = 45 g / (4 pi^4 g_*S) x^2 K_2(x)}
#' where \eqn{x = m/T} and \eqn{K_2} is the modified Bessel function
#' of the second kind of order 2.
#'
#' @param x       Numeric.  Dimensionless parameter \eqn{x = m/T}.
#' @param m_GeV   Numeric.  Particle mass in GeV.
#' @param g_dof   Integer.  Internal degrees of freedom. Default 2.
#' @param g_star_S Numeric.  Entropy degrees of freedom.  If
#'   \code{NULL} (default), computed with
#'   \code{\link{g_star_eff}(m_GeV / x, type = "entropy")}.
#'
#' @return Numeric.  Dimensionless equilibrium yield
#'   \eqn{Y^{\rm eq}}{Y_eq}.
#'
#' @references
#'   Kolb & Turner (1990), Eq. (5.34).
#'   Gondolo & Gelmini, Nucl. Phys. B 360 (1991) 145.
#'
#' @seealso \code{\link{equilibrium_number_density}},
#'   \code{\link{entropy_density}}, \code{\link{boltzmann_pebble_rhs}}
#'
#' @examples
#' equilibrium_yield(x = 3, m_GeV = 100, g_dof = 2)
#' equilibrium_yield(x = 25, m_GeV = 100)
#' @export
equilibrium_yield <- function(x, m_GeV, g_dof = 2, g_star_S = NULL) {
  if (any(x <= 0, na.rm = TRUE))     stop("'x' must be positive.")
  if (any(m_GeV <= 0, na.rm = TRUE)) stop("'m_GeV' must be positive.")
  T_GeV <- m_GeV / x
  if (is.null(g_star_S)) g_star_S <- g_star_eff(T_GeV, type = "entropy")
  (45 * g_dof / (4 * pi^4 * g_star_S)) * x^2 * besselK(x, 2)
}


# ============================================================
#  6.  THE BOLTZMANN RELIC-ABUNDANCE (PEBBLE) EQUATION
# ============================================================

#' Boltzmann Relic-Abundance (Pebble) Equation dY/dx
#'
#' Returns \eqn{dY/dx}, the right-hand side of the Boltzmann equation
#' governing the evolution of the comoving yield \eqn{Y = n/s} of a
#' thermally produced relic species:
#' \deqn{\frac{dY}{dx} = -\frac{\langle\sigma v\rangle\, s}{H\, x}
#'                         \left(Y^2 - Y_{\rm eq}^2\right)}{dY/dx = -(<sigma
#'                         v> s / (H x)) (Y^2 - Y_eq^2)}
#'
#' This equation is commonly called the \strong{"pebble equation"} in
#' thermal dark-matter / freeze-out literature because the relic
#' density drops away from equilibrium like a stone and then plateaus.
#'
#' The variable \eqn{x \equiv m/T}{x = m/T} is used as the time variable so
#' that the equation is well-behaved through freeze-out
#' (\eqn{x \sim 20\text{--}30}{x ~ 20-30}). \eqn{H} is evaluated with
#' \eqn{g_*}{g_*} and both \eqn{s} and \eqn{Y_{\rm eq}}{Y_eq} with
#' \eqn{g_{*S}}{g_*S}.
#'
#' @param x            Numeric.  Dimensionless inverse temperature
#'   \eqn{x = m/T}.
#' @param Y            Numeric.  Current yield \eqn{Y = n/s}.
#' @param m_GeV        Numeric.  Particle mass in GeV.
#' @param sigmav_GeV2  Numeric.  Thermally averaged annihilation cross
#'   section \eqn{\langle\sigma v\rangle}{<sigma v>} in
#'   \eqn{\mathrm{GeV}^{-2}}{GeV^-2}, non-negative.
#'   A typical WIMP value is \eqn{\approx 2.2 \times 10^{-9}}{~2.2e-9}
#'   GeV\eqn{^{-2}}{^-2}.
#' @param g_dof        Integer.  Internal degrees of freedom. Default 2.
#' @param g_star_val   Numeric.  \eqn{g_*} at \eqn{T=m/x}.
#'   If \code{NULL}, computed with \code{\link{g_star_eff}(m_GeV / x)}.
#' @param g_star_S_val Numeric.  \eqn{g_{*S}}{g_*S} at \eqn{T=m/x}.
#'   If \code{NULL}, computed with
#'   \code{\link{g_star_eff}(m_GeV / x, type = "entropy")}.
#'
#' @return Numeric.  \eqn{dY/dx} (dimensionless).
#'
#' @references
#'   Kolb & Turner (1990), Eq. (5.42).
#'   Gondolo & Gelmini, Nucl. Phys. B 360 (1991) 145.
#'
#' @seealso \code{\link{solve_relic_abundance}}, \code{\link{freeze_out_xf}},
#'   \code{\link{equilibrium_yield}}, \code{\link{g_star_eff}}
#'
#' @examples
#' sigmav <- 2.2e-9  # GeV^-2 (typical WIMP value)
#' Y_eq   <- equilibrium_yield(x = 20, m_GeV = 100)
#' boltzmann_pebble_rhs(x = 20, Y = Y_eq, m_GeV = 100,
#'                      sigmav_GeV2 = sigmav)
#' # Above equilibrium the yield is driven back down
#' boltzmann_pebble_rhs(x = 20, Y = 10 * Y_eq, m_GeV = 100,
#'                      sigmav_GeV2 = sigmav)
#' @export
boltzmann_pebble_rhs <- function(x, Y, m_GeV, sigmav_GeV2,
                                 g_dof = 2,
                                 g_star_val   = NULL,
                                 g_star_S_val = NULL) {
  if (any(x <= 0, na.rm = TRUE))          stop("'x' must be positive.")
  if (any(m_GeV <= 0, na.rm = TRUE))      stop("'m_GeV' must be positive.")
  if (any(sigmav_GeV2 < 0, na.rm = TRUE)) {
    stop("'sigmav_GeV2' must be non-negative.")
  }

  T_GeV <- m_GeV / x

  if (is.null(g_star_val))   g_star_val   <- g_star_eff(T_GeV)
  if (is.null(g_star_S_val)) g_star_S_val <- g_star_eff(T_GeV, type = "entropy")

  H   <- hubble_radiation(T_GeV, g_star = g_star_val)
  s   <- entropy_density(T_GeV,  g_star_S = g_star_S_val)
  Yeq <- equilibrium_yield(x, m_GeV, g_dof = g_dof,
                           g_star_S = g_star_S_val)

  -(sigmav_GeV2 * s) / (H * x) * (Y^2 - Yeq^2)
}


# ============================================================
#  7.  INTEGRATE THE PEBBLE EQUATION (ODE solver wrapper)
# ============================================================

# Thin wrappers around deSolve (seams that the tests can mock)
.relic_has_deSolve <- function() {
  requireNamespace("deSolve", quietly = TRUE)
}
.relic_ode <- function(...) {
  deSolve::ode(...)
}

#' Solve the Boltzmann Relic-Abundance (Pebble) Equation
#'
#' Numerically integrates the thermal relic Boltzmann equation
#' (\code{\link{boltzmann_pebble_rhs}}) from \eqn{x_{\rm ini}}{x_ini} to
#' \eqn{x_{\rm fin}}{x_fin} using \pkg{deSolve}, starting in equilibrium,
#' \eqn{Y(x_{\rm ini}) = Y_{\rm eq}(x_{\rm ini})}{Y(x_ini) = Y_eq(x_ini)}.
#' Returns the comoving yield \eqn{Y(x)} and the final relic density
#' parameter \eqn{\Omega h^2}{Omega h^2}.
#'
#' The relic abundance today is:
#' \deqn{\Omega_{\chi} h^2 \approx
#'        2.755 \times 10^{8}\; \frac{m}{\mathrm{GeV}}\;
#'        Y_{\infty}}{Omega_chi h^2 ~ 2.755e8 (m / GeV) Y_inf}
#' where \eqn{Y_{\infty}}{Y_inf} is the yield at \eqn{x_{\rm fin}}{x_fin}.
#'
#' @param m_GeV        Numeric.  Particle mass in GeV (a single positive
#'   number).
#' @param sigmav_GeV2  Numeric.  \eqn{\langle\sigma v\rangle}{<sigma v>} in
#'   GeV\eqn{^{-2}}{^-2} (a single positive number).
#' @param g_dof        Integer.  Internal DOF. Default 2.
#' @param x_ini        Numeric.  Initial \eqn{x = m/T}, where the yield
#'   starts at its equilibrium value; must be positive. Default 5.
#' @param x_fin        Numeric.  Final \eqn{x = m/T}; must be greater than
#'   \code{x_ini}. Default 1000.
#' @param n_steps      Integer.  Number of output grid points, at least 2.
#'   Default 2000.
#'
#' @return A list with:
#'   \describe{
#'     \item{\code{x}}{Numeric vector of \eqn{x} values.}
#'     \item{\code{Y}}{Numeric vector of yield \eqn{Y(x)}.}
#'     \item{\code{Y_eq}}{Numeric vector of equilibrium yield
#'       \eqn{Y^{\rm eq}(x)}{Y_eq(x)}.}
#'     \item{\code{Omega_h2}}{Numeric. Relic density
#'       \eqn{\Omega_\chi h^2}{Omega_chi h^2}.}
#'   }
#'
#' @details Requires the \pkg{deSolve} package. The equation is solved for
#'   \eqn{W = \log Y}{W = log(Y)} with the \code{"lsoda"} method
#'   (\code{rtol = 1e-8}, \code{atol = 1e-10}), using
#'   \code{\link{g_star_eff}} for \eqn{g_*}{g_*} and \eqn{g_{*S}}{g_*S}.
#'   An error is raised if the integration stops before \code{x_fin}.
#'
#' @references Kolb & Turner (1990), Chapter 5.
#'
#' @seealso \code{\link{boltzmann_pebble_rhs}}, \code{\link{freeze_out_xf}},
#'   \code{\link{equilibrium_yield}}
#'
#' @examplesIf requireNamespace("deSolve", quietly = TRUE)
#' sigmav <- 2.2e-9  # GeV^-2
#' result <- solve_relic_abundance(m_GeV = 100, sigmav_GeV2 = sigmav)
#' cat("Omega h^2 =", result$Omega_h2, "\n")
#' @export
solve_relic_abundance <- function(m_GeV, sigmav_GeV2,
                                  g_dof   = 2,
                                  x_ini   = 5,
                                  x_fin   = 1000,
                                  n_steps = 2000) {
  if (!.thermal_is_number(m_GeV) || m_GeV <= 0) {
    stop("'m_GeV' must be a single positive number.")
  }
  if (!.thermal_is_number(sigmav_GeV2) || sigmav_GeV2 <= 0) {
    stop("'sigmav_GeV2' must be a single positive number.")
  }
  if (!.thermal_is_number(g_dof) || g_dof <= 0) {
    stop("'g_dof' must be a single positive number.")
  }
  if (!.thermal_is_number(x_ini) || x_ini <= 0) {
    stop("'x_ini' must be a single positive number.")
  }
  if (!.thermal_is_number(x_fin) || x_fin <= x_ini) {
    stop("'x_fin' must be a single number greater than 'x_ini'.")
  }
  if (!.thermal_is_number(n_steps) || n_steps < 2) {
    stop("'n_steps' must be a single number of at least 2.")
  }
  if (!.relic_has_deSolve()) {
    stop("Package 'deSolve' is required. Install with: install.packages('deSolve')")
  }

  x_grid <- seq(x_ini, x_fin, length.out = n_steps)

  # --- Log-Y substitution: W = log(Y) -------------------------------------------
  # Substituting W = log(Y)  =>  dW/dx = (1/Y) dY/dx
  # This collapses the ~50 orders-of-magnitude span of Y into a moderate range
  # and eliminates the stiffness that causes lsoda to fail on the direct equation.
  # ------------------------------------------------------------------------------
  ode_rhs_logY <- function(x, state, parms) {
    W   <- state["W"]           # W = log(Y)
    Y   <- exp(W)               # recover Y
    dY  <- boltzmann_pebble_rhs(x, Y, m_GeV, sigmav_GeV2, g_dof)
    dW  <- dY / Y               # chain rule
    list(dW)
  }

  W0 <- log(equilibrium_yield(x_ini, m_GeV, g_dof))

  out <- .relic_ode(
    y        = c(W = W0),
    times    = x_grid,
    func     = ode_rhs_logY,
    parms    = NULL,
    method   = "lsoda",
    rtol     = 1e-8,
    atol     = 1e-10,
    maxsteps = 50000
  )

  x_out <- out[, "time"]
  x_end <- x_out[length(x_out)]
  if (!isTRUE(x_end >= x_fin)) {
    stop("The ODE integration stopped at x = ", format(x_end),
         " before reaching 'x_fin' = ", format(x_fin), ".")
  }
  if (!all(is.finite(out[, "W"]))) {
    stop("The ODE integration returned non-finite values.")
  }

  Y_out   <- exp(out[, "W"])   # back-transform to Y
  Yeq_out <- equilibrium_yield(x_out, m_GeV, g_dof)

  Y_inf    <- Y_out[length(Y_out)]
  Omega_h2 <- 2.755e8 * m_GeV * Y_inf

  list(
    x        = x_out,
    Y        = Y_out,
    Y_eq     = Yeq_out,
    Omega_h2 = Omega_h2
  )
}


# ============================================================
#  8.  FREEZE-OUT TEMPERATURE (Iterative Approximation)
# ============================================================

#' Approximate Freeze-Out Parameter x_f = m / T_f
#'
#' Estimates the freeze-out inverse temperature \eqn{x_f = m/T_f}
#' using the iterative approximation from the condition
#' \eqn{\Gamma \sim H}{Gamma ~ H} (Kolb & Turner 1990):
#' \deqn{x_f = \ln\!\left(
#'   c(c+2)\;\sqrt{\frac{45}{8}}\;\frac{g\,m\,m_{\rm Pl}\,
#'   \langle\sigma v\rangle}{2\pi^3\,\sqrt{g_*}\,x_f^{1/2}}
#' \right)}{x_f = ln( c (c + 2) sqrt(45/8) g m m_Pl <sigma v> /
#'   (2 pi^3 sqrt(g_*) sqrt(x_f)) )}
#'
#' Here \eqn{m_{\rm Pl} = G^{-1/2} = \sqrt{8\pi}\, M_{\rm Pl} \approx
#' 1.2207 \times 10^{19}}{m_Pl = G^(-1/2) = sqrt(8 pi) M_Pl ~ 1.2207e19}
#' GeV is the full (non-reduced) Planck mass, for which the coefficient
#' \eqn{\sqrt{45/8}/(2\pi^3)}{sqrt(45/8) / (2 pi^3)} is defined;
#' \eqn{M_{\rm Pl} = 2.435 \times 10^{18}}{M_Pl = 2.435e18} GeV is the
#' reduced Planck mass used by \code{\link{hubble_radiation}}.
#'
#' The equation is solved by fixed-point iteration, starting from the
#' logarithm of the argument without the \eqn{x_f^{-1/2}}{x_f^(-1/2)}
#' factor. The approximation describes non-relativistic freeze-out and
#' needs a log argument well above \eqn{e} (\eqn{x_f \gg 1}{x_f >> 1}); an
#' error is raised if the argument would be \eqn{\le 1}{<= 1}, which happens
#' for very small masses or cross sections. Typical WIMP values lie in the
#' range \eqn{x_f \approx 20\text{--}30}{x_f ~ 20-30}: with the defaults,
#' \eqn{m = 100} GeV and
#' \eqn{\langle\sigma v\rangle = 2.2 \times 10^{-9}}{<sigma v> = 2.2e-9}
#' GeV\eqn{^{-2}}{^-2} give \eqn{x_f \approx 22.38}{x_f ~ 22.38}, i.e.
#' \eqn{T_f \approx 4.47}{T_f ~ 4.47} GeV.
#'
#' @param m_GeV        Numeric.  Particle mass in GeV.
#' @param sigmav_GeV2  Numeric.  \eqn{\langle\sigma v\rangle}{<sigma v>} in
#'   GeV\eqn{^{-2}}{^-2}.
#' @param g_dof        Integer.  Internal degrees of freedom. Default 2.
#' @param c_coeff      Numeric.  Order-unity coefficient (\eqn{c \approx 0.5}{c ~ 0.5}
#'   for \eqn{s}-wave). Default 0.5.
#' @param g_star       Numeric.  \eqn{g_*} at freeze-out. Default 106.75.
#' @param tol          Numeric.  Convergence tolerance. Default 1e-6.
#' @param max_iter     Integer.  Maximum iterations. Default 100.
#'
#' @return Numeric vector of the dimensionless freeze-out parameter
#'   \eqn{x_f}, recycled to the common length of \code{m_GeV},
#'   \code{sigmav_GeV2}, \code{g_dof}, \code{c_coeff} and \code{g_star}.
#'   A warning is given if the iteration has not converged after
#'   \code{max_iter} steps.
#'
#' @references
#'   Kolb & Turner (1990), Eq. (5.44).
#'   Jungman, Kamionkowski & Griest, Phys. Rep. 267 (1996) 195.
#'
#' @seealso \code{\link{solve_relic_abundance}},
#'   \code{\link{boltzmann_pebble_rhs}}
#'
#' @examples
#' sigmav <- 2.2e-9  # GeV^-2
#' x_f <- freeze_out_xf(m_GeV = 100, sigmav_GeV2 = sigmav)
#' cat("x_f =", x_f, "  T_f =", 100 / x_f, "GeV\n")
#' freeze_out_xf(m_GeV = c(10, 100, 1000), sigmav_GeV2 = sigmav)
#' @export
freeze_out_xf <- function(m_GeV, sigmav_GeV2,
                          g_dof    = 2,
                          c_coeff  = 0.5,
                          g_star   = 106.75,
                          tol      = 1e-6,
                          max_iter = 100) {
  if (any(m_GeV <= 0, na.rm = TRUE))       stop("'m_GeV' must be positive.")
  if (any(sigmav_GeV2 <= 0, na.rm = TRUE)) stop("'sigmav_GeV2' must be positive.")
  if (any(g_dof <= 0, na.rm = TRUE))       stop("'g_dof' must be positive.")
  if (any(c_coeff <= 0, na.rm = TRUE))     stop("'c_coeff' must be positive.")
  if (any(g_star <= 0, na.rm = TRUE))      stop("'g_star' must be positive.")
  if (!.thermal_is_number(tol) || tol <= 0) {
    stop("'tol' must be a single positive number.")
  }
  if (!.thermal_is_number(max_iter) || max_iter < 1) {
    stop("'max_iter' must be a single number of at least 1.")
  }

  m_Pl <- M_Pl_reduced * sqrt(8 * pi)   # full Planck mass G^(-1/2) [GeV]
  A <- c_coeff * (c_coeff + 2) * sqrt(45 / 8) *
    (g_dof * m_GeV * m_Pl * sigmav_GeV2) /
    (2 * pi^3 * sqrt(g_star))

  log_arg_error <- paste(
    "The freeze-out approximation needs a log argument well above e,",
    "but it is <= 1 for these inputs ('m_GeV' or 'sigmav_GeV2' too small?)."
  )
  if (any(A <= 1, na.rm = TRUE)) stop(log_arg_error)

  xf <- log(A)   # initial guess (no sqrt(xf) correction)

  for (i in seq_len(max_iter)) {
    xf_new <- log(A / sqrt(xf))
    if (any(xf_new <= 0, na.rm = TRUE)) stop(log_arg_error)
    if (all(abs(xf_new - xf) < tol, na.rm = TRUE)) return(xf_new)
    xf <- xf_new
  }

  warning("freeze_out_xf: did not converge in ", max_iter,
          " iterations. Returning last estimate.")
  xf
}


# ============================================================
#  9.  PEEBLES EQUATION FOR HYDROGEN RECOMBINATION
# ============================================================

#' Peebles Equation for Hydrogen Recombination
#'
#' Computes the derivative of the free-electron fraction during
#' cosmological hydrogen recombination from the Peebles (1968) equation,
#' with the Peebles C-factor in the form used by RECFAST (Seager, Sasselov
#' & Scott 1999).
#'
#' @param lna Natural logarithm of the scale factor,
#'   \code{log(a) = -log(1 + z)}: a single finite number. It sets the
#'   radiation temperature \eqn{T = T_0 e^{-\ln a}}{T = T0 exp(-lna)}.
#' @param xe Free-electron fraction per hydrogen nucleus,
#'   \eqn{0 \le x_e \le 1}{0 <= x_e <= 1}.
#' @param H Hubble expansion rate at \code{lna}, in s\eqn{^{-1}}{^-1}.
#' @param n_H Total hydrogen-nuclei number density at \code{lna}, in
#'   m\eqn{^{-3}}{^-3}.
#' @param alpha_B Case-B recombination coefficient at temperature \eqn{T},
#'   in m\eqn{^3}{^3} s\eqn{^{-1}}{^-1}.
#' @param beta_B Photoionization rate from the \eqn{n = 2} state at
#'   temperature \eqn{T}, in s\eqn{^{-1}}{^-1}.
#' @param lambda_alpha Lyman-alpha wavelength in metres; it also sets the
#'   \eqn{n = 2} excitation energy \eqn{E_{21} = hc/\lambda_\alpha}{E_21 =
#'   h c / lambda_alpha}. Default is 121.567 nm (\code{121.567e-9} m).
#' @param Lambda_2s1s Hydrogen \eqn{2s \to 1s}{2s -> 1s} two-photon decay rate
#'   in s\eqn{^{-1}}{^-1}.  The standard value is approximately 8.22458
#'   s\eqn{^{-1}}{^-1}.
#' @param neutral_floor Small lower limit for neutral-hydrogen density,
#'   used only to avoid division by zero at \eqn{x_e = 1}.
#'   Default \code{1e-30}.
#' @param T0 Radiation (CMB) temperature today in K. Default 2.7255
#'   (Fixsen 2009).
#'
#' @return A named numeric vector containing:
#' \describe{
#'   \item{\code{dxe_dlna}}{Derivative of \eqn{x_e} with respect to
#'     \eqn{\ln a}{ln a} (dimensionless).}
#'   \item{\code{C}}{Peebles C-factor (dimensionless probability that an
#'     excited atom reaches the ground state before being re-ionized).}
#'   \item{\code{lambda_alpha_escape}}{Effective Lyman-alpha escape rate
#'     in s\eqn{^{-1}}{^-1}.}
#' }
#'
#' @details
#' The Peebles equation for the evolution of the free-electron fraction is
#' \deqn{
#'   \frac{d x_e}{d \ln a} =
#'   \frac{C}{H}\left[\beta_B (1 - x_e)\, e^{-E_{21}/(k_B T)}
#'   - n_H \alpha_B x_e^2\right]
#' }{dx_e/dln(a) = (C / H) [beta_B (1 - x_e) exp(-E_21 / (k_B T)) -
#'   n_H alpha_B x_e^2]}
#' where \eqn{T = T_0 e^{-\ln a} = T_0 (1 + z)}{T = T0 exp(-lna) = T0 (1 + z)}
#' and \eqn{E_{21} = hc/\lambda_\alpha \approx 10.2}{E_21 = h c /
#' lambda_alpha ~ 10.2} eV. The Boltzmann factor gives the population of
#' the \eqn{n = 2} level, from which \eqn{\beta_B}{beta_B} photoionizes,
#' relative to the ground state.
#'
#' The Peebles C-factor
#' \deqn{
#'   C = \frac{\Lambda_{2s1s} + \lambda_\alpha^{\rm esc}}
#'             {\Lambda_{2s1s} + \lambda_\alpha^{\rm esc} + \beta_B}
#' }{C = (Lambda_2s1s + lambda_esc) / (Lambda_2s1s + lambda_esc + beta_B)}
#' accounts for the fact that a hydrogen atom formed in an excited state
#' can be photoionized again before reaching the stable ground state via
#' the two-photon or Lyman-alpha escape channels.
#'
#' The Lyman-alpha escape rate is
#' \deqn{
#'   \lambda_\alpha^{\rm esc} = \frac{8\pi H}{\lambda_\alpha^3 \, n_{1s}}
#' }{lambda_esc = 8 pi H / (lambda_alpha^3 n_1s)}
#' where \eqn{n_{1s} \approx n_H (1 - x_e)}{n_1s ~ n_H (1 - x_e)} is the
#' ground-state hydrogen density.
#'
#' The matter temperature is assumed equal to the radiation temperature,
#' which holds while Compton scattering keeps them coupled (\eqn{z \gtrsim
#' 200}{z >~ 200}); \code{alpha_B} and \code{beta_B} should be evaluated at
#' that \eqn{T}. All inputs are single numbers in SI units (metres,
#' seconds, Kelvin). Note that \eqn{H} here is in s\eqn{^{-1}}{^-1},
#' whereas \code{\link{hubble_radiation}} returns it in GeV.
#'
#' The example uses inputs at \eqn{z = 1100} (\eqn{T = 3001} K) for a
#' flat cosmology with \eqn{H_0 = 67.66}{H0 = 67.66} km s\eqn{^{-1}}{^-1}
#' Mpc\eqn{^{-1}}{^-1}, \eqn{\Omega_m = 0.3111}{Omega_m = 0.3111},
#' \eqn{\Omega_r = 9.14 \times 10^{-5}}{Omega_r = 9.14e-5},
#' \eqn{\Omega_b h^2 = 0.02242}{Omega_b h^2 = 0.02242} and helium mass
#' fraction \eqn{Y_p = 0.2454} (Planck 2018): \eqn{H = H_0 \sqrt{\Omega_m
#' (1+z)^3 + \Omega_r (1+z)^4}}{H = H0 sqrt(Omega_m (1+z)^3 + Omega_r
#' (1+z)^4)}, \eqn{n_H = (1 - Y_p) \rho_{b,0} (1+z)^3 / m_H}{n_H = (1 -
#' Y_p) rho_b0 (1+z)^3 / m_H}, \eqn{\alpha_B}{alpha_B} from the case-B fit
#' of Pequignot, Petitjean & Boisson (1991) times the RECFAST fudge factor
#' 1.14, and \eqn{\beta_B = \alpha_B (2\pi m_e k_B T/h^2)^{3/2}
#' e^{-E_I/(4 k_B T)}}{beta_B = alpha_B (2 pi m_e k_B T / h^2)^(3/2)
#' exp(-E_I / (4 k_B T))} with \eqn{E_I = 13.598} eV.
#'
#' @references
#'   Peebles, P. J. E. (1968). Recombination of the Primeval Plasma.
#'   Astrophysical Journal, 153, 1.
#'
#'   Seager, S., Sasselov, D. D. & Scott, D. (1999). A New Calculation of
#'   the Recombination Epoch. Astrophysical Journal Letters, 523, L1
#'   (RECFAST).
#'
#'   Pequignot, D., Petitjean, P. & Boisson, C. (1991). Total and effective
#'   radiative recombination coefficients. Astronomy and Astrophysics, 251,
#'   680.
#'
#' @seealso \code{\link{Saha_Xe}} for the equilibrium (Saha) ionization
#'   fraction.
#'
#' @examples
#' # Physically consistent inputs at z = 1100 (see Details)
#' z <- 1100
#' peebles_rhs(
#'   lna     = -log(1 + z),
#'   xe      = 0.5,
#'   H       = 5.14e-14,  # s^-1
#'   n_H     = 2.53e8,    # m^-3
#'   alpha_B = 7.62e-19,  # m^3 s^-1
#'   beta_B  = 590        # s^-1
#' )
#' @export
peebles_rhs <- function(
    lna,
    xe,
    H,
    n_H,
    alpha_B,
    beta_B,
    lambda_alpha  = 121.567e-9,
    Lambda_2s1s   = 8.22458,
    neutral_floor = 1e-30,
    T0            = 2.7255
) {
  ok <- .thermal_is_number
  stopifnot(
    "'lna' must be a single finite number." = ok(lna),
    "'xe' must be a single number between 0 and 1." =
      ok(xe) && xe >= 0 && xe <= 1,
    "'H' must be a single positive number." = ok(H) && H > 0,
    "'n_H' must be a single positive number." = ok(n_H) && n_H > 0,
    "'alpha_B' must be a single non-negative number." =
      ok(alpha_B) && alpha_B >= 0,
    "'beta_B' must be a single non-negative number." =
      ok(beta_B) && beta_B >= 0,
    "'lambda_alpha' must be a single positive number." =
      ok(lambda_alpha) && lambda_alpha > 0,
    "'Lambda_2s1s' must be a single positive number." =
      ok(Lambda_2s1s) && Lambda_2s1s > 0,
    "'neutral_floor' must be a single positive number." =
      ok(neutral_floor) && neutral_floor > 0,
    "'T0' must be a single positive number." = ok(T0) && T0 > 0
  )

  # Radiation (= matter) temperature [K] and n = 2 excitation energy [eV]
  T_K    <- T0 * exp(-lna)
  E_21   <- .h_SI * .c_SI / (lambda_alpha * .e_SI)
  boltz  <- exp(-E_21 / (.k_B_eV * T_K))

  # Ground-state hydrogen number density  n_{1s} = n_H (1 - x_e)
  # The neutral_floor prevents division by zero when xe -> 1 exactly.
  n_1s <- max(n_H * (1 - xe), neutral_floor)

  # Lyman-alpha escape rate  [s^{-1}]
  lambda_alpha_escape <- 8 * pi * H / (lambda_alpha^3 * n_1s)

  # Peebles C-factor  (dimensionless, 0 < C <= 1)
  C <- (Lambda_2s1s + lambda_alpha_escape) /
    (Lambda_2s1s + lambda_alpha_escape + beta_B)

  # d x_e / d ln a
  dxe_dlna <- (C / H) *
    (beta_B * (1 - xe) * boltz - n_H * alpha_B * xe^2)

  c(
    dxe_dlna            = dxe_dlna,
    C                   = C,
    lambda_alpha_escape = lambda_alpha_escape
  )
}
