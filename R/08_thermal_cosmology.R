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
GeV_to_inv_s  <- 1.519e24  # 1 GeV  = 1.519e24  s^{-1}  (hbar = 1)


# ============================================================
#  1.  HUBBLE RATE IN THE RADIATION-DOMINATED ERA
# ============================================================

#' Hubble Rate in the Radiation-Dominated Era
#'
#' Computes the Hubble expansion rate \eqn{H(T)} during the
#' radiation-dominated epoch using the Friedmann equation:
#' \deqn{H(T) = \sqrt{\frac{\pi^2}{90}\, g_*(T)}\;
#'              \frac{T^2}{M_{\mathrm{Pl}}}}
#' where \eqn{M_{\mathrm{Pl}} = 2.435 \times 10^{18}\ \mathrm{GeV}} is
#' the reduced Planck mass and \eqn{g_*(T)} is the effective number of
#' relativistic degrees of freedom.
#'
#' @param T_GeV  Numeric.  Plasma temperature in GeV.
#' @param g_star Numeric.  Effective relativistic degrees of freedom
#'   \eqn{g_*(T)}.  Defaults to 106.75 (Standard Model value above
#'   the electroweak scale).
#'
#' @return Numeric.  Hubble rate \eqn{H} in GeV (natural units).
#'
#' @references
#'   Kolb & Turner, *The Early Universe* (1990), Eq. (3.46).
#'
#' @examples
#' # H at T = 100 GeV with SM degrees of freedom
#' hubble_radiation(100)
#'
#' # H at T = 1 MeV (neutrino decoupling era), g* ~ 10.75
#' hubble_radiation(1e-3, g_star = 10.75)
#' @export
hubble_radiation <- function(T_GeV, g_star = 106.75) {
  if (any(T_GeV <= 0)) stop("'T_GeV' must be positive.")
  if (any(g_star <= 0)) stop("'g_star' must be positive.")
  sqrt((pi^2 / 90) * g_star) * T_GeV^2 / M_Pl_reduced
}


# ============================================================
#  2.  EFFECTIVE RELATIVISTIC DEGREES OF FREEDOM
# ============================================================

#' Effective Relativistic Degrees of Freedom g_*(T)
#'
#' Returns a step-function approximation of \eqn{g_*(T)}, the effective
#' number of relativistic degrees of freedom as a function of
#' temperature, based on Standard Model particle content.
#'
#' @param T_GeV Numeric.  Temperature in GeV.
#'
#' @return Numeric.  Effective relativistic degrees of freedom \eqn{g_*}.
#'
#' @references
#'   Husdal (2016), arXiv:1609.04979.
#'
#' @examples
#' g_star_eff(100)   # ~ 86.25 (above QCD transition)
#' g_star_eff(1e-3)  # ~ 10.75 (neutrino era)
#' g_star_eff(1e-4)  # ~ 3.91  (after e+e- annihilation)
#' @export
g_star_eff <- function(T_GeV) {
  dplyr::case_when(
    T_GeV  > 300   ~ 106.75,
    T_GeV  > 200   ~  96.25,
    T_GeV  > 100   ~  86.25,
    T_GeV  > 0.5   ~  75.75,
    T_GeV  > 0.15  ~  17.25,
    T_GeV  > 0.05  ~  10.75,
    TRUE           ~   3.91
  )
}


# ============================================================
#  3.  ENTROPY DENSITY
# ============================================================

#' Entropy Density of the Thermal Bath
#'
#' Returns the entropy density \eqn{s(T)} of the cosmological
#' radiation bath:
#' \deqn{s(T) = \frac{2\pi^2}{45}\, g_{*S}(T)\, T^3}
#' where \eqn{g_{*S}} is the effective number of entropy degrees of
#' freedom.
#'
#' @param T_GeV  Numeric.  Temperature in GeV.
#' @param g_star_S Numeric.  Effective entropy degrees of freedom
#'   \eqn{g_{*S}(T)}.  If \code{NULL} (default), computed from
#'   \code{\link{g_star_eff}}.
#'
#' @return Numeric.  Entropy density \eqn{s} in \eqn{\mathrm{GeV}^3}.
#'
#' @references
#'   Kolb & Turner (1990), Eq. (3.65).
#'
#' @examples
#' entropy_density(0.1)
#' entropy_density(1, g_star_S = 106.75)
#' @export
entropy_density <- function(T_GeV, g_star_S = NULL) {
  if (any(T_GeV <= 0)) stop("'T_GeV' must be positive.")
  if (is.null(g_star_S)) g_star_S <- g_star_eff(T_GeV)
  (2 * pi^2 / 45) * g_star_S * T_GeV^3
}


# ============================================================
#  4.  EQUILIBRIUM NUMBER DENSITY (Maxwell-Boltzmann)
# ============================================================

#' Equilibrium Number Density of a Massive Particle Species
#'
#' Computes the equilibrium number density \eqn{n^{\rm eq}(T)} for a
#' non-relativistic species in the Maxwell-Boltzmann limit:
#' \deqn{n^{\rm eq}(T) = g\,\left(\frac{mT}{2\pi}\right)^{3/2}
#'                        e^{-m/T}}
#'
#' @param T_GeV Numeric.  Temperature in GeV.
#' @param m_GeV Numeric.  Particle mass in GeV.
#' @param g_dof Integer.  Internal degrees of freedom. Default 2.
#'
#' @return Numeric.  Equilibrium number density in \eqn{\mathrm{GeV}^3}.
#'
#' @references
#'   Kolb & Turner (1990), Eq. (5.25).
#'
#' @examples
#' equilibrium_number_density(T_GeV = 0.01, m_GeV = 0.1, g_dof = 2)
#' @export
equilibrium_number_density <- function(T_GeV, m_GeV, g_dof = 2) {
  if (any(T_GeV <= 0)) stop("'T_GeV' must be positive.")
  if (any(m_GeV <= 0)) stop("'m_GeV' must be positive.")
  x <- m_GeV / T_GeV
  g_dof * (m_GeV * T_GeV / (2 * pi))^(3 / 2) * exp(-x)
}


# ============================================================
#  5.  EQUILIBRIUM YIELD  Y_eq(x)
# ============================================================

#' Equilibrium Yield Y_eq as a Function of x = m/T
#'
#' The yield \eqn{Y \equiv n / s} is the comoving number density
#' (number per comoving entropy).  In equilibrium:
#' \deqn{Y^{\rm eq}(x) = \frac{45\, g}{4\pi^4\, g_{*S}}\; x^2\, K_2(x)}
#' where \eqn{x = m/T} and \eqn{K_2} is the modified Bessel function
#' of the second kind of order 2.
#'
#' @param x       Numeric.  Dimensionless parameter \eqn{x = m/T}.
#' @param m_GeV   Numeric.  Particle mass in GeV.
#' @param g_dof   Integer.  Internal degrees of freedom. Default 2.
#' @param g_star_S Numeric.  Entropy degrees of freedom.  If
#'   \code{NULL} (default), computed from \code{\link{g_star_eff}}.
#'
#' @return Numeric.  Equilibrium yield \eqn{Y^{\rm eq}}.
#'
#' @references
#'   Kolb & Turner (1990), Eq. (5.34).
#'   Gondolo & Gelmini, Nucl. Phys. B 360 (1991) 145.
#'
#' @examples
#' equilibrium_yield(x = 3, m_GeV = 100, g_dof = 2)
#' equilibrium_yield(x = 25, m_GeV = 100)
#' @export
equilibrium_yield <- function(x, m_GeV, g_dof = 2, g_star_S = NULL) {
  if (any(x <= 0))     stop("'x' must be positive.")
  if (any(m_GeV <= 0)) stop("'m_GeV' must be positive.")
  T_GeV <- m_GeV / x
  if (is.null(g_star_S)) g_star_S <- g_star_eff(T_GeV)
  (45 * g_dof / (4 * pi^4 * g_star_S)) * x^2 * besselK(x, 2)
}


# ============================================================
#  6.  THE BOLTZMANN RELIC-ABUNDANCE (PEBBLE) EQUATION
# ============================================================

#' Boltzmann Relic-Abundance (Pebble) Equation  dY/dx
#'
#' Returns \eqn{dY/dx}, the right-hand side of the Boltzmann equation
#' governing the evolution of the comoving yield \eqn{Y = n/s} of a
#' thermally produced relic species:
#' \deqn{\frac{dY}{dx} = -\frac{\langle\sigma v\rangle\, s}{H\, x}
#'                         \left(Y^2 - Y_{\rm eq}^2\right)}
#'
#' This equation is commonly called the \strong{"pebble equation"} in
#' thermal dark-matter / freeze-out literature because the relic
#' density drops away from equilibrium like a stone and then plateaus.
#'
#' The variable \eqn{x \equiv m/T} is used as the time variable so
#' that the equation is well-behaved through freeze-out
#' (\eqn{x \sim 20\text{--}30}).
#'
#' @param x            Numeric.  Dimensionless inverse temperature
#'   \eqn{x = m/T}.
#' @param Y            Numeric.  Current yield \eqn{Y = n/s}.
#' @param m_GeV        Numeric.  Particle mass in GeV.
#' @param sigmav_GeV2  Numeric.  Thermally averaged annihilation cross
#'   section \eqn{\langle\sigma v\rangle} in \eqn{\mathrm{GeV}^{-2}}.
#'   A typical WIMP value is \eqn{\approx 2.2 \times 10^{-9}}
#'   GeV\eqn{^{-2}}.
#' @param g_dof        Integer.  Internal degrees of freedom. Default 2.
#' @param g_star_val   Numeric.  \eqn{g_*} at \eqn{T=m/x}.
#'   If \code{NULL}, computed from \code{\link{g_star_eff}}.
#' @param g_star_S_val Numeric.  \eqn{g_{*S}} at \eqn{T=m/x}.
#'   If \code{NULL}, set equal to \code{g_star_val}.
#'
#' @return Numeric.  \eqn{dY/dx}.
#'
#' @references
#'   Kolb & Turner (1990), Eq. (5.42).
#'   Gondolo & Gelmini, Nucl. Phys. B 360 (1991) 145.
#'
#' @examples
#' sigmav <- 2.2e-9  # GeV^-2 (typical WIMP value)
#' Y_eq   <- equilibrium_yield(x = 20, m_GeV = 100)
#' boltzmann_pebble_rhs(x = 20, Y = Y_eq, m_GeV = 100,
#'                      sigmav_GeV2 = sigmav)
#' @export
boltzmann_pebble_rhs <- function(x, Y, m_GeV, sigmav_GeV2,
                                  g_dof = 2,
                                  g_star_val   = NULL,
                                  g_star_S_val = NULL) {
  if (x <= 0)      stop("'x' must be positive.")
  if (m_GeV <= 0)  stop("'m_GeV' must be positive.")

  T_GeV <- m_GeV / x

  if (is.null(g_star_val))   g_star_val   <- g_star_eff(T_GeV)
  if (is.null(g_star_S_val)) g_star_S_val <- g_star_val

  H   <- hubble_radiation(T_GeV, g_star = g_star_val)
  s   <- entropy_density(T_GeV,  g_star_S = g_star_S_val)
  Yeq <- equilibrium_yield(x, m_GeV, g_dof = g_dof,
                            g_star_S = g_star_S_val)

  -(sigmav_GeV2 * s) / (H * x) * (Y^2 - Yeq^2)
}


# ============================================================
#  7.  INTEGRATE THE PEBBLE EQUATION (ODE solver wrapper)
# ============================================================

#' Solve the Boltzmann Relic-Abundance (Pebble) Equation
#'
#' Numerically integrates the thermal relic Boltzmann equation from
#' \eqn{x_{\rm ini}} to \eqn{x_{\rm fin}} using \pkg{deSolve}.
#' Returns the comoving yield \eqn{Y(x)} and the final relic density
#' parameter \eqn{\Omega h^2}.
#'
#' The relic abundance today is:
#' \deqn{\Omega_{\chi} h^2 \approx
#'        2.755 \times 10^{8}\; \frac{m}{\mathrm{GeV}}\; Y_{\infty}}
#'
#' @param m_GeV        Numeric.  Particle mass in GeV.
#' @param sigmav_GeV2  Numeric.  \eqn{\langle\sigma v\rangle} in GeV\eqn{^{-2}}.
#' @param g_dof        Integer.  Internal DOF. Default 2.
#' @param x_ini        Numeric.  Initial \eqn{x = m/T}. Default 1.
#' @param x_fin        Numeric.  Final \eqn{x = m/T}. Default 1000.
#' @param n_steps      Integer.  Number of output grid points. Default 2000.
#'
#' @return A list with:
#'   \describe{
#'     \item{\code{x}}{Numeric vector of \eqn{x} values.}
#'     \item{\code{Y}}{Numeric vector of yield \eqn{Y(x)}.}
#'     \item{\code{Y_eq}}{Numeric vector of equilibrium yield \eqn{Y^{\rm eq}(x)}.}
#'     \item{\code{Omega_h2}}{Numeric. Relic density \eqn{\Omega_\chi h^2}.}
#'   }
#'
#' @details Requires the \pkg{deSolve} package.
#'
#' @references Kolb & Turner (1990), Chapter 5.
#'
#' @examples
#' sigmav <- 2.2e-9  # GeV^-2
#' result <- solve_relic_abundance(m_GeV = 100, sigmav_GeV2 = sigmav)
#' cat("Omega h^2 =", result$Omega_h2, "\n")
#' @export
solve_relic_abundance <- function(m_GeV, sigmav_GeV2,
                                   g_dof   = 2,
                                   x_ini   = 5,
                                   x_fin   = 1000,
                                   n_steps = 2000) {
  if (!requireNamespace("deSolve", quietly = TRUE)) {
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

  out <- deSolve::ode(
    y        = c(W = W0),
    times    = x_grid,
    func     = ode_rhs_logY,
    parms    = NULL,
    method   = "lsoda",
    rtol     = 1e-8,
    atol     = 1e-10,
    maxsteps = 50000
  )

  x_out   <- out[, "time"]
  Y_out   <- exp(out[, "W"])   # back-transform to Y
  Yeq_out <- vapply(x_out, function(xi)
    equilibrium_yield(xi, m_GeV, g_dof), numeric(1))

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
#' using the iterative approximation from the condition \eqn{\Gamma \sim H}:
#' \deqn{x_f = \ln\!\left(
#'   c(c+2)\;\sqrt{\frac{45}{8}}\;\frac{g\,m\,M_{\rm Pl}\,
#'   \langle\sigma v\rangle}{2\pi^3\,\sqrt{g_*}\,x_f^{1/2}}
#' \right)}
#'
#' Typical WIMP values lie in the range \eqn{x_f \approx 20\text{--}30}.
#'
#' @param m_GeV        Numeric.  Particle mass in GeV.
#' @param sigmav_GeV2  Numeric.  \eqn{\langle\sigma v\rangle} in GeV\eqn{^{-2}}.
#' @param g_dof        Integer.  Internal degrees of freedom. Default 2.
#' @param c_coeff      Numeric.  Order-unity coefficient (\eqn{c \approx 0.5}
#'   for \eqn{s}-wave). Default 0.5.
#' @param g_star       Numeric.  \eqn{g_*} at freeze-out. Default 106.75.
#' @param tol          Numeric.  Convergence tolerance. Default 1e-6.
#' @param max_iter     Integer.  Maximum iterations. Default 100.
#'
#' @return Numeric.  Freeze-out parameter \eqn{x_f}.
#'
#' @references
#'   Kolb & Turner (1990), Eq. (5.44).
#'   Jungman, Kamionkowski & Griest, Phys. Rep. 267 (1996) 195.
#'
#' @examples
#' sigmav <- 2.2e-9  # GeV^-2
#' x_f <- freeze_out_xf(m_GeV = 100, sigmav_GeV2 = sigmav)
#' cat("x_f =", x_f, "  T_f =", 100 / x_f, "GeV\n")
#' @export
freeze_out_xf <- function(m_GeV, sigmav_GeV2,
                           g_dof    = 2,
                           c_coeff  = 0.5,
                           g_star   = 106.75,
                           tol      = 1e-6,
                           max_iter = 100) {
  if (m_GeV <= 0)       stop("'m_GeV' must be positive.")
  if (sigmav_GeV2 <= 0) stop("'sigmav_GeV2' must be positive.")

  A <- c_coeff * (c_coeff + 2) * sqrt(45 / 8) *
    (g_dof * m_GeV * M_Pl_reduced * sigmav_GeV2) /
    (2 * pi^3 * sqrt(g_star))

  xf <- log(A)   # initial guess (no sqrt(xf) correction)

  for (i in seq_len(max_iter)) {
    xf_new <- log(A / sqrt(xf))
    if (abs(xf_new - xf) < tol) return(xf_new)
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
#' cosmological hydrogen recombination using the Peebles C-factor.
#'
#' @param lna Natural logarithm of the scale factor, \code{log(a)}.
#' @param xe Free-electron fraction per hydrogen nucleus,
#'   \eqn{0 \le x_e \le 1}.
#' @param H Hubble expansion rate in s\eqn{^{-1}}.
#' @param n_H Total hydrogen-nuclei number density in m\eqn{^{-3}}.
#' @param alpha_B Case-B recombination coefficient in m\eqn{^3} s\eqn{^{-1}}.
#' @param beta_B Photoionization coefficient from the \eqn{n = 2} state
#'   in s\eqn{^{-1}}.
#' @param lambda_alpha Lyman-alpha wavelength in metres.
#'   Default is 121.567 nm (\code{121.567e-9} m).
#' @param Lambda_2s1s Hydrogen \eqn{2s \to 1s} two-photon decay rate
#'   in s\eqn{^{-1}}.  The standard value is approximately 8.22458 s\eqn{^{-1}}.
#' @param neutral_floor Small lower limit for neutral-hydrogen density,
#'   used only to avoid division by zero at \eqn{x_e = 1}.
#'   Default \code{1e-30}.
#'
#' @return A named numeric vector containing:
#' \describe{
#'   \item{\code{dxe_dlna}}{Derivative of \eqn{x_e} with respect to
#'     \eqn{\ln a}.}
#'   \item{\code{C}}{Peebles C-factor (probability that an excited atom
#'     reaches the ground state before being re-ionized).}
#'   \item{\code{lambda_alpha_escape}}{Effective Lyman-alpha escape rate
#'     in s\eqn{^{-1}}.}
#' }
#'
#' @details
#' The Peebles equation for the evolution of the free-electron fraction is
#' \deqn{
#'   \frac{d x_e}{d \ln a} =
#'   \frac{C}{H}\left[\beta_B(1 - x_e) - n_H \alpha_B x_e^2\right].
#' }
#'
#' The Peebles C-factor
#' \deqn{
#'   C = \frac{\Lambda_{2s1s} + \lambda_\alpha^{\rm esc}}
#'             {\Lambda_{2s1s} + \lambda_\alpha^{\rm esc} + \beta_B}
#' }
#' accounts for the fact that a hydrogen atom formed in an excited state
#' can be photoionized again before reaching the stable ground state via
#' the two-photon or Lyman-alpha escape channels.
#'
#' The Lyman-alpha escape rate is
#' \deqn{
#'   \lambda_\alpha^{\rm esc} = \frac{8\pi H}{\lambda_\alpha^3 \, n_{1s}}
#' }
#' where \eqn{n_{1s} \approx n_H (1 - x_e)} is the ground-state hydrogen
#' density.
#'
#' All quantities are in SI units (metres, seconds).  Note that \eqn{H}
#' here is in s\eqn{^{-1}}, not GeV as in the rest of this file.
#'
#' @references
#'   Peebles, P. J. E. (1968). Recombination of the Primeval Plasma.
#'   Astrophysical Journal, 153, 1.
#'
#' @examples
#' # Evaluate at z ~ 1100 (recombination epoch)
#' # Approximate inputs at z = 1100:
#' H_rec      <- 3.3e-15   # s^-1  (Hubble rate at recombination)
#' n_H_rec    <- 400e6     # m^-3  (hydrogen number density)
#' alpha_B    <- 2.6e-19   # m^3 s^-1  (case-B recombination coeff)
#' beta_B     <- 4e-15     # s^-1  (photoionization rate)
#' peebles_rhs(
#'   lna          = log(1 / 1101),
#'   xe           = 0.5,
#'   H            = H_rec,
#'   n_H          = n_H_rec,
#'   alpha_B      = alpha_B,
#'   beta_B       = beta_B
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
    neutral_floor = 1e-30
) {
  stopifnot(
    length(lna) == 1L,
    length(xe)  == 1L,
    is.finite(xe),
    xe           >= 0,
    xe           <= 1,
    H            >  0,
    n_H          >  0,
    alpha_B      >= 0,
    beta_B       >= 0,
    lambda_alpha >  0,
    Lambda_2s1s  >  0
  )

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
    (beta_B * (1 - xe) - n_H * alpha_B * xe^2)

  c(
    dxe_dlna            = dxe_dlna,
    C                   = C,
    lambda_alpha_escape = lambda_alpha_escape
  )
}
