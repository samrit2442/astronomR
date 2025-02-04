#' Convert Kilometers to Megaparsecs
#'
#' This function converts a distance value from kilometers (km) to megaparsecs (Mpc).
#' The conversion factor is based on 1 parsec being equivalent to 3.262 light-years,
#' and 1 light-year being approximately \eqn{9.461 \times 10^{12}} kilometers.
#'
#' @param km A numeric value representing the distance in kilometers.
#'
#' @return A numeric value representing the equivalent distance in megaparsecs (Mpc).
#' @export
#'
#' @examples
#' km_to_Mpc(3.086e19) # Converts 3.086e19 km (approx. 1 Mpc) to megaparsecs
km_to_Mpc <- function(km) {
  km / (3.262e6 * 9.461e12)
}

#' Convert Megaparsecs to Kilometers
#'
#' This function converts a distance value from megaparsecs (Mpc) to kilometers (km).
#' The conversion factor is based on 1 parsec being equivalent to 3.262 light-years,
#' and 1 light-year being approximately \eqn{9.461 \times 10^{12}} kilometers.
#'
#' @param mpc A numeric value representing the distance in megaparsecs.
#'
#' @return A numeric value representing the equivalent distance in kilometers (km).
#' @export
#'
#' @examples
#' Mpc_to_km(1) # Converts 1 Mpc to kilometers
Mpc_to_km <- function(mpc) {
  mpc * 3.262e6 * 9.461e12
}

sec_to_year <- function(sec_ye) {
  3.1709791984e-8 * sec_ye
}

FlatLCDM <- function(hubble_constant_fact, dark_matter_crit, matter_crit, radiation_crit, T_cmb = 2.725) {
  h_per_s <- hubble_constant_fact * 1 / Mpc_to_km(1.0)
  return(list(hubble_constant_fact = hubble_constant_fact, dark_matter_crit = dark_matter_crit, matter_crit = matter_crit, radiation_crit = radiation_crit, type = "FlatLCDM", h_per_s = h_per_s, T_cmb = T_cmb))
}


ClosedLCDM <- function(hubble_constant_fact, curvature_crit, dark_matter_crit, matter_crit, radiation_crit, T_cmb = 2.725) {
  h_per_s <- hubble_constant_fact * 1 / Mpc_to_km(1.0)
  return(list(hubble_constant_fact = hubble_constant_fact, curvature_crit = curvature_crit, dark_matter_crit = dark_matter_crit, matter_crit = matter_crit, radiation_crit = radiation_crit, type = "ClosedLCDM", h_per_s = h_per_s, T_cmb = 2.725))
}


OpenLCDM <- function(hubble_constant_fact, curvature_crit, dark_matter_crit, matter_crit, radiation_crit, T_cmb = 2.725) {
  h_per_s <- hubble_constant_fact * 1 / Mpc_to_km(1.0)
  return(list(hubble_constant_fact = hubble_constant_fact, curvature_crit = curvature_crit, dark_matter_crit = dark_matter_crit, matter_crit = matter_crit, radiation_crit = radiation_crit, type = "OpenLCDM", h_per_s = h_per_s, T_cmb = 2.725))
}

### Only export the function that the user should interact with

cosmology_model <- function(hubble_constant_fact = 0.6774, curvature_crit = 0, dark_matter_crit = 0.6911, matter_crit = 0.3089, radiation_crit = 0, T_cmb = 2.725) {
  if (curvature_crit < 0) {
    return(ClosedLCDM(hubble_constant_fact, curvature_crit, dark_matter_crit, matter_crit, radiation_crit, T_cmb))
  } else if (curvature_crit > 0) {
    return(OpenLCDM(hubble_constant_fact, curvature_crit, dark_matter_crit, matter_crit, radiation_crit, T_cmb))
  } else {
    return(FlatLCDM(hubble_constant_fact, dark_matter_crit, matter_crit, radiation_crit, T_cmb))
  }
}

a <- function(z) {
  1 / (1 + z)
} # a= expansion factor; today it's 1
redshift <- function(a) {
  if (a < 0) {
    print("error can't be negative")
  } else {
    1 / a - 1
  }
}

H_by_H0 <- function(c, a) {
  if (c$type == "FlatLCDM") {
    return(sqrt(c$radiation_crit * a^(-4) + c$matter_crit * a^(-3) + c$dark_matter_crit))
  } else {
    return(sqrt(c$radiation_crit * a^(-4) + c$matter_crit * a^(-3) + c$curvature_crit * a^(-2) + c$dark_matter_crit))
  }
}

tH0_as_fn_a <- function(c, a) {
  helper_fun <- function(a_b) {
    if (c$type == "FlatLCDM") {
      return(a_b / (sqrt(c$radiation_crit + c$matter_crit * a_b + c$dark_matter_crit * a_b^4)))
    } else {
      return(a_b / (sqrt(c$radiation_crit + c$matter_crit * a_b + c$curvature_crit * a_b^2 + c$dark_matter_crit * a_b^4)))
    }
  }
  return(pracma::quad(helper_fun, 0, a))
}

t_as_func_a_in_Year <- function(c, a) {
  k <- (100.0 * c$h_per_s) / (sec_to_year(1.0))
  return(tH0_as_fn_a(c, a) / k)
}


age_of_universe <- function(c, unit = "year") {
  age <- t_as_func_a_in_Year(c, 1.0)

  if (unit == "year") {
    return(age)
  } else if (unit == "GY") {
    return(age / 10^9)
  } else {
    print("Error: unit must be either 'year' or 'GY'")
  }
}

comv_dist <- function(c) {
  return(2997.92458 / c$h_per_s) # in km
}

comoving_distance <- function(c, z) {
  help_funct <- function(z_1) {
    return(1 / H_by_H0(c, a(z_1)))
  }
  return(pracma::quad(help_funct, 0, z) * comv_dist(c))
}

radius_of_curvature <- function(c) {
  if (c$type == "FlatLCDM") {
    k <- 0
    return(0.0)
  } else {
    if (c$type == "ClosedLCDM") {
      k <- 1
    } else {
      k <- -1
    }
    return(sqrt(-(k * 299792.458^2) / (c$curvature_crit * (100 * c$h_per_s)^2)))
  }
}

metric_distance <- function(c, z) {
  if (c$type == "FlatLCDM") {
    return(comoving_distance(c, z))
  } else if (c$type == "ClosedLCDM") {
    R0 <- radius_of_curvature(c)
    return(R0 * sin(comoving_distance(c, z) / R0))
  } else {
    R0 <- radius_of_curvature(c)
    return(R0 * sinh(comoving_distance(c, z) / R0))
  }
}

luminosity_distance <- function(c, z) {
  return((1 + z) * metric_distance(c, z))
}

angular_diameter_distance <- function(c, z) {
  return(metric_distance(c, z) / (1 + z))
}


a_of_t <- function(c, t) {
  f <- function(a) t_as_func_a_in_Year(c, a) - t # Root finding for t(a) = t
  a <- pracma::fzero(f, 1)$x # Numerical root finding starting from a=1
  return(a)
}
