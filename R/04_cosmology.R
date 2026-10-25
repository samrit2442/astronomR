# ============================================================
# 04_cosmology.R
# Background cosmology of FLRW models: unit conversion, the
# model constructor, the expansion rate E(z), the distance
# measures of Hogg (1999) and cosmic time.
# ============================================================

# ---- Internal helpers ---------------------------------------

# TRUE for numeric vectors and for all-NA logical vectors such as a bare NA.
.cosmo_is_numeric <- function(x) {
  is.numeric(x) || (is.logical(x) && all(is.na(x)))
}

.cosmo_check_model <- function(cosmo) {
  if (!inherits(cosmo, "astronomR_cosmology")) {
    stop("'cosmo' must be a cosmological model created by cosmology_model().",
         call. = FALSE)
  }
  invisible(cosmo)
}

# Validates a redshift vector and returns it as an unnamed double vector.
# Distances need z >= 0; everything else needs z > -1.
.cosmo_check_z <- function(z, allow_negative) {
  if (!.cosmo_is_numeric(z)) stop("'z' must be numeric.", call. = FALSE)
  z <- as.numeric(z)
  if (any(is.infinite(z))) stop("'z' must be finite.", call. = FALSE)
  if (allow_negative) {
    if (any(z <= -1, na.rm = TRUE)) {
      stop("'z' must be greater than -1.", call. = FALSE)
    }
  } else if (any(z < 0, na.rm = TRUE)) {
    stop("'z' must be non-negative.", call. = FALSE)
  }
  z
}

# E(z)^2 as a function of x = 1 + z.
.cosmo_E2 <- function(cosmo, x) {
  cosmo$Omega_r * x^4 + cosmo$Omega_m * x^3 + cosmo$Omega_k * x^2 +
    cosmo$Omega_Lambda
}

# a^2 E^2 as a function of the scale factor a = 1 / (1 + z). Zero terms are
# dropped so that a -> 0 gives Inf instead of 0 / 0.
.cosmo_a2E2 <- function(cosmo, a) {
  s <- cosmo$Omega_k + cosmo$Omega_Lambda * a^2
  if (cosmo$Omega_m > 0) s <- s + cosmo$Omega_m / a
  if (cosmo$Omega_r > 0) s <- s + cosmo$Omega_r / a^2
  s
}

# Stops unless E^2 > 0 for every x = 1 + z in [x_lo, x_hi] (x_hi may be Inf).
# The minimum of the polynomial E^2(x) on the interval is at an end point or
# at a stationary point, i.e. a root of 4 Or x^2 + 3 Om x + 2 Ok = 0.
.cosmo_check_expansion <- function(cosmo, x_lo, x_hi, range_txt) {
  Or <- cosmo$Omega_r
  Om <- cosmo$Omega_m
  Ok <- cosmo$Omega_k
  xs <- numeric(0)
  if (Or > 0) {
    disc <- 9 * Om^2 - 32 * Or * Ok
    if (disc >= 0) xs <- (-3 * Om + c(-1, 1) * sqrt(disc)) / (8 * Or)
  } else if (Om > 0) {
    xs <- -2 * Ok / (3 * Om)
  }
  pts <- c(x_lo, xs[xs > x_lo & xs < x_hi], if (is.finite(x_hi)) x_hi)
  e2 <- .cosmo_E2(cosmo, pts)
  if (is.infinite(x_hi) && Or == 0 && Om == 0) {
    # as x -> Inf, E^2 -> Omega_k x^2 + Omega_Lambda
    e2 <- c(e2, if (Ok != 0) sign(Ok) * Inf else cosmo$Omega_Lambda)
  }
  if (any(e2 <= 0)) {
    stop(sprintf(paste0(
      "E(z)^2 <= 0 somewhere in the range %s: the model does not expand ",
      "continuously over this range (e.g. a bouncing or recollapsing ",
      "universe)."), range_txt), call. = FALSE)
  }
  invisible(TRUE)
}

.cosmo_integrate <- function(f, lower, upper) {
  tryCatch(
    stats::integrate(f, lower, upper, rel.tol = 1e-10, abs.tol = 0,
                     subdivisions = 1000L)$value,
    error = function(e) {
      stop("Numerical integration failed: ", conditionMessage(e),
           call. = FALSE)
    }
  )
}

# Line-of-sight comoving distance in units of D_H. Integrating in
# u = ln(1 + z) with a = exp(-u), dz / E(z) = du / sqrt(a^2 E^2), which
# keeps the integrand smooth and bounded at high redshift.
.cosmo_comoving <- function(cosmo, z) {
  ok <- !is.na(z)
  if (any(ok)) {
    z_max <- max(z[ok])
    .cosmo_check_expansion(cosmo, 1, 1 + z_max,
                           sprintf("0 <= z <= %s", format(z_max)))
  }
  integrand <- function(u) 1 / sqrt(.cosmo_a2E2(cosmo, exp(-u)))
  vapply(z, function(zi) {
    if (is.na(zi)) return(NA_real_)
    if (zi == 0) return(0)
    .cosmo_integrate(integrand, 0, log1p(zi))
  }, numeric(1), USE.NAMES = FALSE)
}

# Transverse comoving distance in units of D_H.
.cosmo_transverse <- function(cosmo, z) {
  dc <- .cosmo_comoving(cosmo, z)
  Ok <- cosmo$Omega_k
  if (Ok > 0) {
    sinh(sqrt(Ok) * dc) / sqrt(Ok)
  } else if (Ok < 0) {
    sin(sqrt(-Ok) * dc) / sqrt(-Ok)
  } else {
    dc
  }
}

# Converts a distance in units of D_H = c / H0 to Mpc or km.
.cosmo_distance_unit <- function(cosmo, d, unit) {
  d_mpc <- d * .c_km_s / cosmo$H0
  if (unit == "km") d_mpc * .km_per_Mpc else d_mpc
}

# H0 * t(z). With u = ln(1 + z') and a' = exp(-u), da' / (a' E) = du / E and
# 1 / E = a' / sqrt(a'^2 E^2), so H0 t = integral_{ln(1 + z)}^Inf of that.
.cosmo_time_H0 <- function(cosmo, z) {
  ok <- !is.na(z)
  if (!any(ok)) return(rep(NA_real_, length(z)))
  z_min <- min(z[ok])
  .cosmo_check_expansion(cosmo, 1 + z_min, Inf,
                         sprintf("z >= %s", format(z_min)))
  if (cosmo$Omega_r == 0 && cosmo$Omega_m == 0 && cosmo$Omega_k == 0) {
    # de Sitter: no big bang, the integral diverges logarithmically
    return(ifelse(ok, Inf, NA_real_))
  }
  integrand <- function(u) {
    a <- exp(-u)
    a / sqrt(.cosmo_a2E2(cosmo, a))
  }
  vapply(z, function(zi) {
    if (is.na(zi)) return(NA_real_)
    .cosmo_integrate(integrand, log1p(zi), Inf)
  }, numeric(1), USE.NAMES = FALSE)
}


# ---- Distance unit conversion -------------------------------

#' Convert Kilometers to Megaparsecs
#'
#' Converts distances from kilometers (km) to megaparsecs (Mpc) using the exact
#' IAU definitions of the astronomical unit, 1 au = 149597870.7 km (IAU 2012
#' Resolution B2), and of the parsec, 1 pc = \eqn{648000 / \pi}{648000 / pi} au
#' (IAU 2015 Resolution B2):
#' \deqn{1\ \mathrm{Mpc} = 10^6 \times \frac{648000}{\pi} \times
#'   149597870.7\ \mathrm{km} = 3.0856775814913673 \times 10^{19}\ \mathrm{km}.}{
#' 1 Mpc = 1e6 * (648000 / pi) * 149597870.7 km = 3.0856775814913673e19 km.}
#'
#' @param km Numeric vector of distances in kilometers.
#'
#' @return Numeric vector of the same length as \code{km}: the distances in
#'   megaparsecs (Mpc).
#' @seealso \code{\link{Mpc_to_km}} for the inverse conversion;
#'   \code{\link{hubble_distance}} and the other distance functions, which
#'   return Mpc by default.
#' @export
#'
#' @examples
#' km_to_Mpc(3.0856775814913673e19) # 1 Mpc
#' km_to_Mpc(c(1e19, 1e20, 1e21))
km_to_Mpc <- function(km) {
  if (!.cosmo_is_numeric(km)) stop("'km' must be numeric.")
  km / .km_per_Mpc
}

#' Convert Megaparsecs to Kilometers
#'
#' Converts distances from megaparsecs (Mpc) to kilometers (km) using the exact
#' IAU definitions of the astronomical unit, 1 au = 149597870.7 km (IAU 2012
#' Resolution B2), and of the parsec, 1 pc = \eqn{648000 / \pi}{648000 / pi} au
#' (IAU 2015 Resolution B2):
#' \deqn{1\ \mathrm{Mpc} = 10^6 \times \frac{648000}{\pi} \times
#'   149597870.7\ \mathrm{km} = 3.0856775814913673 \times 10^{19}\ \mathrm{km}.}{
#' 1 Mpc = 1e6 * (648000 / pi) * 149597870.7 km = 3.0856775814913673e19 km.}
#'
#' @param mpc Numeric vector of distances in megaparsecs.
#'
#' @return Numeric vector of the same length as \code{mpc}: the distances in
#'   kilometers (km).
#' @seealso \code{\link{km_to_Mpc}} for the inverse conversion.
#' @export
#'
#' @examples
#' Mpc_to_km(1) # 3.0856775814913673e19 km
#' Mpc_to_km(c(0.5, 1, 4425.6))
Mpc_to_km <- function(mpc) {
  if (!.cosmo_is_numeric(mpc)) stop("'mpc' must be numeric.")
  mpc * .km_per_Mpc
}


# ---- Cosmological model -------------------------------------

#' Define a Cosmological Model
#'
#' Creates a homogeneous Friedmann-Lemaitre-Robertson-Walker (FLRW) model of
#' the universe with matter, radiation, spatial curvature and a cosmological
#' constant, whose expansion rate relative to today is
#' \deqn{E(z) = \frac{H(z)}{H_0} = \sqrt{\Omega_r (1 + z)^4 +
#'   \Omega_m (1 + z)^3 + \Omega_k (1 + z)^2 + \Omega_\Lambda}}{
#' E(z) = H(z) / H0 = sqrt(Omega_r (1 + z)^4 + Omega_m (1 + z)^3
#'                         + Omega_k (1 + z)^2 + Omega_Lambda)}
#' with \eqn{H_0 = 100\,h}{H0 = 100 h} km/s/Mpc. The model is the
#' \code{cosmo} argument of \code{\link{hubble_ratio}},
#' \code{\link{cosmic_time}} and the distance functions such as
#' \code{\link{comoving_distance}}. The defaults are the Planck 2015 flat
#' Lambda-CDM parameters (Planck Collaboration 2016, Table 4, last column),
#' without radiation.
#'
#' The curvature type follows the sign of \code{Omega_k}: \code{"flat"}
#' (\code{Omega_k = 0}), \code{"open"} (\code{Omega_k > 0}) or
#' \code{"closed"} (\code{Omega_k < 0}). \code{Omega_k} is not derived from
#' the other parameters: for a consistent model choose it so that
#' \code{Omega_m + Omega_Lambda + Omega_r + Omega_k = 1}, which makes
#' \eqn{E(0) = 1}. A warning is given when this sum differs from 1 by more
#' than 0.001.
#'
#' \code{T_cmb} is stored and printed for reference only. It does not set
#' \code{Omega_r}, which must be given explicitly (CMB photons alone give
#' \eqn{\Omega_\gamma h^2 \approx 2.47 \times 10^{-5}}{Omega_gamma h^2 = 2.47e-5}
#' for \code{T_cmb = 2.7255}).
#'
#' @param h Dimensionless Hubble parameter, \code{H0 / (100 km/s/Mpc)}: for
#'   example 0.6774 for a Hubble constant of 67.74 km/s/Mpc. Must satisfy
#'   \code{0 < h < 2}.
#' @param Omega_m Matter density parameter today (non-negative).
#' @param Omega_Lambda Cosmological-constant (dark-energy) density parameter
#'   today. May be negative.
#' @param Omega_r Radiation density parameter today (non-negative).
#' @param Omega_k Curvature density parameter today: 0 for a flat, positive
#'   for an open and negative for a closed universe.
#' @param T_cmb CMB temperature today in kelvin (non-negative). Stored for
#'   reference only; it is not used in any calculation.
#' @param x An object of class \code{astronomR_cosmology}.
#' @param ... Ignored.
#'
#' @return \code{cosmology_model()} returns a list of class
#'   \code{astronomR_cosmology} with elements \code{h}, \code{H0} (the Hubble
#'   constant in km/s/Mpc), \code{Omega_m}, \code{Omega_Lambda},
#'   \code{Omega_r}, \code{Omega_k}, \code{T_cmb} (in K) and \code{curvature}
#'   (\code{"flat"}, \code{"open"} or \code{"closed"}). The \code{print()}
#'   method prints a summary of the model and returns \code{x} invisibly.
#'
#' @references
#'   Planck Collaboration (2016). Planck 2015 results. XIII. Cosmological
#'   parameters. Astronomy and Astrophysics, 594, A13.
#'   \url{https://arxiv.org/abs/1502.01589}
#'
#'   Fixsen, D. J. (2009). The temperature of the cosmic microwave
#'   background. The Astrophysical Journal, 707, 916.
#'   \url{https://arxiv.org/abs/0911.1955}
#'
#' @seealso \code{\link{hubble_ratio}}, \code{\link{comoving_distance}},
#'   \code{\link{luminosity_distance}}, \code{\link{cosmic_time}},
#'   \code{\link{age_of_universe}}
#'
#' @examples
#' # Planck 2015 flat Lambda-CDM (the defaults)
#' cosmo <- cosmology_model()
#' cosmo
#'
#' # the same model with radiation
#' cosmology_model(h = 0.6774, Omega_m = 0.3089, Omega_Lambda = 0.6911,
#'                 Omega_r = 8.4e-5)
#'
#' # an open model: Omega_k = 1 - Omega_m - Omega_Lambda
#' open_model <- cosmology_model(h = 0.7, Omega_m = 0.3, Omega_Lambda = 0.6,
#'                               Omega_k = 0.1)
#' open_model$curvature
#' @export
cosmology_model <- function(h = 0.6774, Omega_m = 0.3089, Omega_Lambda = 0.6911,
                            Omega_r = 0, Omega_k = 0, T_cmb = 2.7255) {
  pars <- list(h = h, Omega_m = Omega_m, Omega_Lambda = Omega_Lambda,
               Omega_r = Omega_r, Omega_k = Omega_k, T_cmb = T_cmb)
  for (nm in names(pars)) {
    p <- pars[[nm]]
    if (!is.numeric(p) || length(p) != 1L || !is.finite(p)) {
      stop(sprintf("'%s' must be a single finite number.", nm))
    }
  }
  h <- as.numeric(h)
  Omega_m <- as.numeric(Omega_m)
  Omega_Lambda <- as.numeric(Omega_Lambda)
  Omega_r <- as.numeric(Omega_r)
  Omega_k <- as.numeric(Omega_k)
  T_cmb <- as.numeric(T_cmb)

  if (h <= 0) stop("'h' must be positive.")
  if (h >= 2) {
    stop("'h' must be less than 2: it is H0 / (100 km/s/Mpc), ",
         "e.g. 0.6774 rather than 67.74.")
  }
  if (Omega_m < 0) stop("'Omega_m' must be non-negative.")
  if (Omega_r < 0) stop("'Omega_r' must be non-negative.")
  if (T_cmb < 0) stop("'T_cmb' must be non-negative.")

  total <- Omega_m + Omega_Lambda + Omega_r + Omega_k
  if (abs(total - 1) > 1e-3) {
    warning(sprintf(paste0(
      "Omega_m + Omega_Lambda + Omega_r + Omega_k = %s, not 1, so ",
      "E(0) = H(z = 0) / H0 is not 1. For a curved model use ",
      "Omega_k = 1 - Omega_m - Omega_Lambda - Omega_r."),
      format(total, digits = 6)))
  }

  curvature <- if (Omega_k == 0) "flat" else if (Omega_k > 0) "open" else "closed"
  structure(
    list(h = h, H0 = 100 * h, Omega_m = Omega_m, Omega_Lambda = Omega_Lambda,
         Omega_r = Omega_r, Omega_k = Omega_k, T_cmb = T_cmb,
         curvature = curvature),
    class = "astronomR_cosmology"
  )
}

#' @rdname cosmology_model
#' @export
print.astronomR_cosmology <- function(x, ...) {
  vals <- c(
    H0           = paste0(format(x$H0), " km/s/Mpc (h = ", format(x$h), ")"),
    Omega_m      = format(x$Omega_m),
    Omega_Lambda = format(x$Omega_Lambda),
    Omega_r      = format(x$Omega_r),
    Omega_k      = format(x$Omega_k),
    T_cmb        = paste0(format(x$T_cmb), " K")
  )
  cat("astronomR cosmology (", x$curvature, ")\n", sep = "")
  cat(sprintf("  %-12s = %s\n", names(vals), vals), sep = "")
  invisible(x)
}


# ---- Expansion rate -----------------------------------------

#' Dimensionless Hubble Parameter E(z)
#'
#' Computes the expansion rate relative to today for a model created by
#' \code{\link{cosmology_model}}, that is
#' \eqn{E(z) = H(z) / H_0}{E(z) = H(z) / H0} with
#' \deqn{E(z) = \sqrt{\Omega_r (1 + z)^4 + \Omega_m (1 + z)^3 +
#'   \Omega_k (1 + z)^2 + \Omega_\Lambda}.}{
#' E(z) = sqrt(Omega_r (1 + z)^4 + Omega_m (1 + z)^3
#'             + Omega_k (1 + z)^2 + Omega_Lambda).}
#' An error is raised where \eqn{E(z)^2 \le 0}{E(z)^2 <= 0}, i.e. where the
#' model has no real expansion rate.
#'
#' @param cosmo A cosmological model created by \code{\link{cosmology_model}}.
#' @param z Numeric vector of finite redshifts, each greater than -1
#'   (negative values lie in the future). \code{NA} values give \code{NA}.
#'
#' @return Numeric vector of the same length as \code{z} (no names): the
#'   dimensionless ratio \eqn{H(z) / H_0}{H(z) / H0}. Multiply by
#'   \code{cosmo$H0} to get \eqn{H(z)}{H(z)} in km/s/Mpc.
#'
#' @references
#'   Hogg, D. W. (1999). Distance measures in cosmology.
#'   \url{https://arxiv.org/abs/astro-ph/9905116}
#'
#' @seealso \code{\link{cosmology_model}}, \code{\link{hubble_distance}},
#'   \code{\link{comoving_distance}}, \code{\link{cosmic_time}}
#'
#' @examples
#' cosmo <- cosmology_model()
#' hubble_ratio(cosmo, c(0, 0.5, 1, 2))
#' cosmo$H0 * hubble_ratio(cosmo, 1) # H(z = 1) in km/s/Mpc
#' @export
hubble_ratio <- function(cosmo, z) {
  .cosmo_check_model(cosmo)
  z <- .cosmo_check_z(z, allow_negative = TRUE)
  e2 <- .cosmo_E2(cosmo, 1 + z)
  bad <- !is.na(e2) & e2 <= 0
  if (any(bad)) {
    stop(sprintf(paste0(
      "E(z)^2 <= 0 at z = %s: the model has no real expansion rate there ",
      "(e.g. a bouncing or recollapsing universe)."), format(z[bad][1])))
  }
  sqrt(e2)
}


# ---- Distances ----------------------------------------------

#' Cosmological Distances
#'
#' Distance measures of Hogg (1999) for a model created by
#' \code{\link{cosmology_model}}, vectorized over the redshift \code{z}.
#' \itemize{
#'   \item \code{hubble_distance()}: the Hubble distance
#'     \deqn{D_H = \frac{c}{H_0}.}{D_H = c / H0.}
#'   \item \code{comoving_distance()}: the line-of-sight comoving distance
#'     \deqn{D_C(z) = D_H \int_0^z \frac{dz'}{E(z')},}{
#'     D_C(z) = D_H * integral from 0 to z of dz' / E(z'),}
#'     with \eqn{E(z)}{E(z)} from \code{\link{hubble_ratio}}.
#'   \item \code{transverse_comoving_distance()}: the transverse comoving
#'     distance \eqn{D_M}{D_M}. It equals \eqn{D_C}{D_C} in a flat model; in
#'     an open model (\eqn{\Omega_k > 0}{Omega_k > 0})
#'     \deqn{D_M = \frac{D_H}{\sqrt{\Omega_k}}
#'       \sinh\left(\sqrt{\Omega_k} \frac{D_C}{D_H}\right)}{
#'     D_M = D_H sinh(sqrt(Omega_k) D_C / D_H) / sqrt(Omega_k)}
#'     and in a closed model (\eqn{\Omega_k < 0}{Omega_k < 0})
#'     \deqn{D_M = \frac{D_H}{\sqrt{|\Omega_k|}}
#'       \sin\left(\sqrt{|\Omega_k|} \frac{D_C}{D_H}\right).}{
#'     D_M = D_H sin(sqrt(|Omega_k|) D_C / D_H) / sqrt(|Omega_k|).}
#'   \item \code{luminosity_distance()}:
#'     \eqn{D_L = (1 + z) D_M}{D_L = (1 + z) D_M}.
#'   \item \code{angular_diameter_distance()}:
#'     \eqn{D_A = D_M / (1 + z)}{D_A = D_M / (1 + z)}.
#'   \item \code{radius_of_curvature()}: the curvature radius today,
#'     \eqn{R_0 = D_H / \sqrt{|\Omega_k|}}{R0 = D_H / sqrt(|Omega_k|)}, which
#'     is \code{Inf} for a flat model.
#' }
#'
#' The integral for \eqn{D_C}{D_C} is evaluated numerically with
#' \code{\link[stats]{integrate}} (relative tolerance \code{1e-10}). An error
#' is raised if \eqn{E(z)^2 \le 0}{E(z)^2 <= 0} anywhere between 0 and the
#' largest requested redshift, e.g. beyond the bounce of a bouncing model. In
#' a closed model \eqn{D_M}{D_M} is not monotonic: it falls back to zero at
#' the antipode, where \eqn{\sqrt{|\Omega_k|} D_C / D_H = \pi}{sqrt(|Omega_k|)
#' D_C / D_H = pi}, and is negative beyond it.
#'
#' @param cosmo A cosmological model created by \code{\link{cosmology_model}}.
#' @param z Numeric vector of finite, non-negative redshifts. \code{NA}
#'   values give \code{NA}; \code{z = 0} gives 0.
#' @param unit Unit of the result: \code{"Mpc"} (default) or \code{"km"}.
#'
#' @return A single number for \code{hubble_distance()} and
#'   \code{radius_of_curvature()}; otherwise an unnamed numeric vector with
#'   one element per redshift. Distances are in \code{unit}: megaparsecs (Mpc)
#'   by default, or kilometers (km).
#'
#' @references
#'   Hogg, D. W. (1999). Distance measures in cosmology.
#'   \url{https://arxiv.org/abs/astro-ph/9905116}
#'
#' @seealso \code{\link{cosmology_model}}, \code{\link{hubble_ratio}},
#'   \code{\link{cosmic_time}}, \code{\link{km_to_Mpc}}
#'
#' @examples
#' cosmo <- cosmology_model()
#' z <- c(0.1, 0.5, 1, 2)
#' hubble_distance(cosmo)
#' comoving_distance(cosmo, z)
#' transverse_comoving_distance(cosmo, z) # equal to D_C: the model is flat
#' luminosity_distance(cosmo, z)
#' angular_diameter_distance(cosmo, z)
#' luminosity_distance(cosmo, 1, unit = "km")
#' radius_of_curvature(cosmo) # Inf for a flat model
#'
#' open_model <- cosmology_model(Omega_m = 0.3, Omega_Lambda = 0.6,
#'                               Omega_k = 0.1)
#' radius_of_curvature(open_model)
#' transverse_comoving_distance(open_model, z)
#' @name cosmological_distances
NULL

#' @rdname cosmological_distances
#' @export
hubble_distance <- function(cosmo, unit = c("Mpc", "km")) {
  .cosmo_check_model(cosmo)
  unit <- match.arg(unit)
  .cosmo_distance_unit(cosmo, 1, unit)
}

#' @rdname cosmological_distances
#' @export
comoving_distance <- function(cosmo, z, unit = c("Mpc", "km")) {
  .cosmo_check_model(cosmo)
  unit <- match.arg(unit)
  z <- .cosmo_check_z(z, allow_negative = FALSE)
  .cosmo_distance_unit(cosmo, .cosmo_comoving(cosmo, z), unit)
}

#' @rdname cosmological_distances
#' @export
transverse_comoving_distance <- function(cosmo, z, unit = c("Mpc", "km")) {
  .cosmo_check_model(cosmo)
  unit <- match.arg(unit)
  z <- .cosmo_check_z(z, allow_negative = FALSE)
  .cosmo_distance_unit(cosmo, .cosmo_transverse(cosmo, z), unit)
}

#' @rdname cosmological_distances
#' @export
luminosity_distance <- function(cosmo, z, unit = c("Mpc", "km")) {
  .cosmo_check_model(cosmo)
  unit <- match.arg(unit)
  z <- .cosmo_check_z(z, allow_negative = FALSE)
  .cosmo_distance_unit(cosmo, (1 + z) * .cosmo_transverse(cosmo, z), unit)
}

#' @rdname cosmological_distances
#' @export
angular_diameter_distance <- function(cosmo, z, unit = c("Mpc", "km")) {
  .cosmo_check_model(cosmo)
  unit <- match.arg(unit)
  z <- .cosmo_check_z(z, allow_negative = FALSE)
  .cosmo_distance_unit(cosmo, .cosmo_transverse(cosmo, z) / (1 + z), unit)
}

#' @rdname cosmological_distances
#' @export
radius_of_curvature <- function(cosmo, unit = c("Mpc", "km")) {
  .cosmo_check_model(cosmo)
  unit <- match.arg(unit)
  if (cosmo$Omega_k == 0) return(Inf)
  .cosmo_distance_unit(cosmo, 1 / sqrt(abs(cosmo$Omega_k)), unit)
}


# ---- Cosmic time --------------------------------------------

#' Cosmic Time and Age of the Universe
#'
#' \code{cosmic_time()} gives the age of the universe at redshift \code{z}:
#' the proper time since the big bang (\eqn{a = 0}{a = 0}) for a model created
#' by \code{\link{cosmology_model}},
#' \deqn{t(z) = \frac{1}{H_0} \int_0^{a} \frac{a'\,da'}{\sqrt{\Omega_r +
#'   \Omega_m a' + \Omega_k a'^2 + \Omega_\Lambda a'^4}}, \qquad
#'   a = \frac{1}{1 + z}.}{
#' t(z) = (1 / H0) * integral from 0 to a of a' da' /
#'        sqrt(Omega_r + Omega_m a' + Omega_k a'^2 + Omega_Lambda a'^4),
#' with a = 1 / (1 + z).}
#' \code{age_of_universe()} is the age today, \code{cosmic_time(cosmo, 0)}.
#'
#' Negative redshifts (\code{-1 < z < 0}) give times in the future. The
#' integral is evaluated numerically with \code{\link[stats]{integrate}}
#' (relative tolerance \code{1e-10}). The Hubble time \eqn{1 / H_0}{1 / H0}
#' is converted to years with the IAU megaparsec (see \code{\link{Mpc_to_km}})
#' and the Julian year of 365.25 days.
#'
#' A pure de Sitter model (\code{Omega_m = Omega_r = Omega_k = 0}) has no big
#' bang, so its age is infinite and \code{Inf} is returned. An error is raised
#' if \eqn{E(z)^2 \le 0}{E(z)^2 <= 0} anywhere between the big bang and
#' \code{z}: for example a bouncing model that never had \eqn{a = 0}{a = 0},
#' or a future redshift that a recollapsing model never reaches.
#'
#' @param cosmo A cosmological model created by \code{\link{cosmology_model}}.
#' @param z Numeric vector of finite redshifts, each greater than -1.
#'   \code{NA} values give \code{NA}.
#' @param unit Unit of the result: \code{"Gyr"} (default, \eqn{10^9}{10^9}
#'   Julian years) or \code{"year"} (Julian years).
#'
#' @return Times since the big bang in \code{unit}: gigayears (Gyr) by
#'   default, or years. \code{cosmic_time()} returns an unnamed numeric vector
#'   with one element per redshift; \code{age_of_universe()} returns a single
#'   number.
#'
#' @references
#'   Hogg, D. W. (1999). Distance measures in cosmology.
#'   \url{https://arxiv.org/abs/astro-ph/9905116}
#'
#' @seealso \code{\link{cosmology_model}}, \code{\link{hubble_ratio}},
#'   \code{\link{redshift_to_scale_factor}}, \code{\link{comoving_distance}}
#'
#' @examples
#' cosmo <- cosmology_model()
#' age_of_universe(cosmo)
#' age_of_universe(cosmo, unit = "year")
#' cosmic_time(cosmo, c(1100, 10, 1, 0)) # Gyr after the big bang
#' cosmic_time(cosmo, -0.5)               # when the universe is twice as big
#' @export
cosmic_time <- function(cosmo, z, unit = c("Gyr", "year")) {
  .cosmo_check_model(cosmo)
  unit <- match.arg(unit)
  z <- .cosmo_check_z(z, allow_negative = TRUE)
  t_yr <- .cosmo_time_H0(cosmo, z) * .km_per_Mpc / cosmo$H0 / .s_per_yr
  if (unit == "Gyr") t_yr / 1e9 else t_yr
}

#' @rdname cosmic_time
#' @export
age_of_universe <- function(cosmo, unit = c("Gyr", "year")) {
  .cosmo_check_model(cosmo)
  unit <- match.arg(unit)
  cosmic_time(cosmo, 0, unit)
}


# ---- Redshift and scale factor ------------------------------

#' Convert Between Redshift and Scale Factor
#'
#' \code{redshift_to_scale_factor()} returns the scale factor
#' \deqn{a = \frac{1}{1 + z}}{a = 1 / (1 + z)}
#' (\eqn{a = 1}{a = 1} today) and \code{scale_factor_to_redshift()} the
#' inverse, \eqn{z = 1 / a - 1}{z = 1 / a - 1}.
#'
#' @param z Numeric vector of redshifts, each greater than -1. \code{NA}
#'   values give \code{NA}.
#' @param a Numeric vector of positive scale factors. \code{NA} values give
#'   \code{NA}.
#'
#' @return Numeric vector of the same length as the input (no names):
#'   dimensionless scale factors from \code{redshift_to_scale_factor()},
#'   dimensionless redshifts from \code{scale_factor_to_redshift()}.
#'
#' @seealso \code{\link{cosmic_time}}, \code{\link{hubble_ratio}}
#'
#' @examples
#' redshift_to_scale_factor(c(0, 1, 3, 1100))
#' scale_factor_to_redshift(c(1, 0.5, 0.25))
#' @export
redshift_to_scale_factor <- function(z) {
  if (!.cosmo_is_numeric(z)) stop("'z' must be numeric.")
  if (any(z <= -1, na.rm = TRUE)) stop("'z' must be greater than -1.")
  1 / (1 + as.numeric(z))
}

#' @rdname redshift_to_scale_factor
#' @export
scale_factor_to_redshift <- function(a) {
  if (!.cosmo_is_numeric(a)) stop("'a' must be numeric.")
  if (any(a <= 0, na.rm = TRUE)) stop("'a' must be positive.")
  1 / as.numeric(a) - 1
}
