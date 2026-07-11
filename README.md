# astronomR: Astronomy and Cosmology Analysis in R 🌌✨

<!-- badges: start -->
[![CRAN version](https://www.r-pkg.org/badges/version/astronomR)](https://cran.r-project.org/package=astronomR)
[![Dev version](https://img.shields.io/badge/dev%20version-0.3.0-blue?logo=github)](https://github.com/samrit2442/astronomR)
[![CRAN checks](https://badges.cranchecks.info/summary/astronomR.svg)](https://cran.r-project.org/web/checks/check_results_astronomR.html)
<!-- [![R-CMD-check](https://github.com/samrit2442/astronomR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/samrit2442/astronomR/actions/workflows/R-CMD-check.yaml) -->
<!-- [![Codecov test coverage](https://codecov.io/gh/samrit2442/astronomR/graph/badge.svg)](https://app.codecov.io/gh/samrit2442/astronomR) -->
<!-- [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) -->
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![CRAN downloads (grand total)](https://cranlogs.r-pkg.org/badges/grand-total/astronomR)](https://cran.r-project.org/package=astronomR)
[![CRAN downloads (last month)](https://cranlogs.r-pkg.org/badges/last-month/astronomR)](https://cran.r-project.org/package=astronomR)
[![GitHub stars](https://img.shields.io/github/stars/samrit2442/astronomR?style=social)](https://github.com/samrit2442/astronomR/stargazers)
[![Last commit](https://img.shields.io/github/last-commit/samrit2442/astronomR)](https://github.com/samrit2442/astronomR/commits/master)
<!-- badges: end -->

An R package designed to bridge the gap between data science and the cosmos!

## Why {astronomR}?

While R is widely used in data science and statistical computing, there has been a lack of tools specifically tailored for astronomical data. {astronomR} aims to fill this gap, offering easy-to-use functions and tools for anyone looking to explore space and cosmology through data. Whether you're an astrophysicist or just a data enthusiast, {astronomR} is here to help you navigate the universe.

## Installing this Package

Install the stable release from CRAN:

``` r
install.packages("astronomR")
```

Or install the development version from GitHub:

``` r
# Install devtools package if you don't have it
# install.packages("devtools")

devtools::install_github("samrit2442/astronomR")
```

## Introduction

This package was developed for astronomy, cosmological computation, and analysis with R. The source code can be found here <https://github.com/samrit2442/astronomR>

## Function Overview

| Category | Functions |
|---|---|
| **Angular Conversions** | `deg_to_hms()`, `hms_to_deg()`, `deg_to_dms()`, `dms_to_deg()`, `deg2rad()`, `rad2deg()` |
| **Physical Constants** | `constants_df`, `constant_value()` |
| **Gaia Archive** | `get_gaia_data()` |
| **Cosmology** | `km_to_Mpc()`, `Mpc_to_km()`, `cosmology_model()`, `age_of_universe()`, `comoving_distance()`, `luminosity_distance()`, `angular_diameter_distance()` |
| **Thermal Physics** | `photon_energy_density_fn_T()`, `photon_energy_density_fn_z()`, `photon_number_density_fn_T()`, `photon_number_density_fn_z()`, `Saha_Xe()`, `soln_saha()` |
| **Thermal Cosmology** | `hubble_radiation()`, `g_star_eff()`, `entropy_density()`, `equilibrium_number_density()`, `equilibrium_yield()`, `boltzmann_pebble_rhs()`, `solve_relic_abundance()`, `freeze_out_xf()`, `peebles_rhs()` |
| **Drake Equation** | `drake_equation()` |

## Some Usage and Example Codes

Normally in astronomy, we use a different sort of angular system for location. We can easily do that in our package. Let's see how. Suppose, we have an angular value of `d = 177.74208°` We want to convert it into an hour-minute-second. There is a very simple function.

``` r
library(astronomR)

deg_to_hms(177.74208)
#> [1] 11H50M58.0992S
hms_to_deg(11, 50, 58.09925)
#> [1] 177.7421
```

What else can be done? Let's say we want to find the path of some star in a particular location for a time interval. This can also be done using our package.

To do that, first, we need to define the RA and Dec value of the star. Let's see to do this. Also, let's define the time of observation and its location.

``` r
ra_hour  <- 16.695  # RA in hours
dec_deg  <- 36.466667  # Dec in degrees
lat_obs  <- 52.5  # Observer's latitude
lon_obs  <- -1.9166667  # Observer's longitude
datetime <- as.POSIXct("1998-08-10 23:10:00", tz = "UTC")  # Observation time
```

Now, create the star location as seen from that location on the time mentioned. This returns Altitude and Azimuth as normally used.

``` r
star_location <- RA_dec2Alt_azi(ra_hour, dec_deg, lat_obs, lon_obs, datetime)
print(paste("Altitude:", star_location$altitude, "degrees"))
#> [1] "Altitude: 49.1688687197424 degrees"

print(paste("Azimuth:", star_location$azimuth, "degrees"))
#> [1] "Azimuth: 269.146669462321 degrees"
```

This tells us from some location, what is the position of the star so that we can use a telescope to watch it! Using this simple function, we can trace out the path any star travels. Let's see how and also maybe plot it. For that, let's first make a time range, for which we want to see the location.

``` r
library(ggplot2)

# Plotting Rigel's Motion
start_datetime <- as.POSIXct("2024-10-02 00:00:00", tz = "UTC")
end_datetime   <- as.POSIXct("2024-10-03 00:00:00", tz = "UTC")

# Generate timestamps at 20-minute intervals
timestamps <- seq(from = start_datetime, to = end_datetime, by = "20 mins")
altitude   <- numeric(length(timestamps))
azimuth    <- numeric(length(timestamps))

observer_lat <- 43.1566  # Latitude in degrees
observer_lon <- -77.6088  # Longitude in degrees

# RA and Dec for Rigel (converted RA to hours)
rigel_ra  <- 78.634467 / 15  # RA in hours
rigel_dec <- -8.20164  # Dec in degrees

# Calculate altitude and azimuth for each timestamp
for (i in seq_along(timestamps)) {
  datetime    <- timestamps[i]
  result      <- RA_dec2Alt_azi(rigel_ra, rigel_dec, observer_lat, observer_lon, datetime)
  altitude[i] <- result$altitude
  azimuth[i]  <- result$azimuth
}

# Create a data frame with results
rigel_positions <- data.frame(datetime = timestamps, altitude = altitude, azimuth = azimuth)

# 2D Plot for Altitude over Time
ggplot(rigel_positions, aes(x = datetime, y = altitude)) +
  geom_line() +
  labs(x = "Time", y = "Altitude (degrees)", title = "Altitude of Rigel over 24 hours")
```

![image](https://github.com/user-attachments/assets/e7052a09-3142-4796-a505-d60c65a991f7)

``` r
# 2D Plot for Azimuth over Time
ggplot(rigel_positions, aes(x = datetime, y = azimuth)) +
  geom_line() +
  labs(x = "Time", y = "Azimuth (degrees)", title = "Azimuth of Rigel over 24 hours")
```

![image](https://github.com/user-attachments/assets/80dd618d-d7fc-40b3-af08-606ed7d9846c)

``` r
# 2D Scatter Plot for Azimuth vs Altitude
ggplot(rigel_positions, aes(x = azimuth, y = altitude)) +
  geom_point(color = "firebrick") +
  labs(x = "Azimuth (degrees)", y = "Altitude (degrees)", title = "Azimuth vs Altitude for Rigel over 24 hours")
```

![image](https://github.com/user-attachments/assets/46e75c1b-79c8-4163-bdd1-6a2d74311f91)

## Gaia Data Archive

The **Gaia Data Archive** is a comprehensive database that houses the data collected by the European Space Agency's Gaia mission. Launched in December 2013, Gaia is designed to create the most accurate three-dimensional map of the Milky Way galaxy by observing and cataloguing the positions, distances, and motions of over a billion stars. In Python **Astropy** and **Astroquery** is used to import data directly. We can do the same thing in our package. Let's see how:

``` r
df <- get_gaia_data(vars = "ra, dec, parallax", condition = "parallax > 50")
head(df)

#>         ra       dec  parallax
#> 1 316.7537  38.75607 286.00534
#> 2 316.7485  38.76386 285.99493
#> 3 298.4819  44.41291 214.57451
#> 4 249.3875 -53.69952  53.22840
#> 5 243.4453 -57.57679  73.54824
#> 6 312.2788  37.47123  56.86465
```

Nice! Isn't it? Let's use this for some analysis. Why not create an H-R diagram.

For this first, let's convert parallax to absolute magnitude.

``` r
library(ggplot2)
library(dplyr)

# Helper function to convert parallax (mas) and apparent magnitude to absolute magnitude
calculate_absolute_magnitude <- function(parallax, g_mag) {
  distance_pc <- 1 / (parallax / 1000)
  abs_mag     <- g_mag - 5 * (log10(distance_pc) - 1)
  return(abs_mag)
}

# Let's import the data
df <- get_gaia_data(vars = "ra, dec, parallax, phot_g_mean_mag, phot_bp_mean_mag, phot_rp_mean_mag", 
                    condition = "parallax > 50")
# Data processing
df <- df %>%
  mutate(color_index = phot_bp_mean_mag - phot_rp_mean_mag,
         abs_mag = calculate_absolute_magnitude(parallax, phot_g_mean_mag))
# Filter out any rows with NA values
df <- df %>% filter(!is.na(color_index), !is.na(abs_mag))

# Plot an H-R diagram
ggplot(df, aes(x = color_index, y = abs_mag)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(x = "Color Index (G_BP - G_RP)", y = "Absolute Magnitude (M)", 
       title = "Hertzsprung-Russell Diagram") +
  theme_minimal() +
  scale_y_reverse()  # Reverse the y-axis for magnitude
```

![image](https://github.com/user-attachments/assets/a29b22c9-9ca0-4048-9bf4-8e50847157c7)

## Cosmological Calculation

We can do many things related to cosmology using this. Let's see a few of them. Before that, I will suggest to run this code. As we know, there are many cosmological models. You can define these in our package.

``` r
cosmo <- FlatLCDM(0.6774, 0.6911, 0.3089, 8.4e-5)
cosmo
#> $hubble_constant_fact
#> [1] 0.6774
#> 
#> $dark_matter_crit
#> [1] 0.69110000000000005
#> 
#> $matter_crit
#> [1] 0.30890000000000001
#> 
#> $radiation_crit
#> [1] 8.3999999999999995e-05
#> 
#> $type
#> [1] "FlatLCDM"
#> 
#> $h_per_s
#> [1] 2.1949477836373805e-20
```

Now, we can directly use these models and inputs of other functions. As we know scale factor of our universe `a = 1` today. We can directly use our function to calculate time at any given `a` value.

``` r
t_as_func_a_in_Year(cosmo, 1) # in year
#> [1] 13808979942.748983
```

We can find the age of our universe using this or use the function `age_of_universe()`.

``` r
age_of_universe(cosmo, unit = "GY") # our universe age !!!!!
#> [1] 13.808979942748984
```

Our package can find the radius of curvature of our universe. Let's see:

``` r
radius_of_curvature(cosmo)
#> [1] 0
```

## Thermal Cosmology of the Early Universe

The thermal cosmology module covers the complete early-universe thermal history
from the WIMP freeze-out epoch all the way through hydrogen recombination.
All functions in this section use **natural units** (ħ = c = k_B = 1) with
temperature and mass in GeV, except `peebles_rhs()` which uses SI units.

### Hubble Rate & Degrees of Freedom

``` r
# Hubble rate H(T) in the radiation-dominated era [GeV]
hubble_radiation(100)           # T = 100 GeV, full SM  g* = 106.75
#> [1] 1.405077e-14

hubble_radiation(1e-3, g_star = 10.75)   # T = 1 MeV, neutrino era
#> [1] 4.458986e-25

# Effective relativistic degrees of freedom g*(T)
g_star_eff(500)   # > 300 GeV: full Standard Model
#> [1] 106.75
g_star_eff(0.1)   # neutrino era
#> [1] 10.75
g_star_eff(0.01)  # after e+e- annihilation
#> [1] 3.91
```

### Entropy Density & Equilibrium Distributions

``` r
# Entropy density s(T) = (2π²/45) g*S T³  [GeV³]
entropy_density(0.1)                    # T = 100 MeV
#> [1] 0.004715535

# Maxwell-Boltzmann equilibrium number density n_eq(T)
equilibrium_number_density(T_GeV = 5, m_GeV = 100, g_dof = 2)
#> [1] 2.926338e-06

# Equilibrium yield Y_eq(x)  [x = m/T]
equilibrium_yield(x = 20, m_GeV = 100)   # near freeze-out
#> [1] 7.720313e-10
```

### WIMP Freeze-Out: The Boltzmann Pebble Equation

The **pebble equation** governs the evolution of the comoving yield Y = n/s
of a thermally produced dark-matter relic:

$$\frac{dY}{dx} = -\frac{\langle\sigma v\rangle\, s}{H\, x}\left(Y^2 - Y_{\rm eq}^2\right)$$

``` r
sigmav <- 2.2e-9   # GeV^-2  (typical WIMP <sigma v>)

# Evaluate dY/dx — zero at equilibrium (fixed point), negative above it
Yeq <- equilibrium_yield(x = 20, m_GeV = 100)
boltzmann_pebble_rhs(x = 20, Y = 10 * Yeq, m_GeV = 100, sigmav_GeV2 = sigmav)
#> [1] -9.110522e-07   # above equilibrium: strong restoring force

# Iterative freeze-out temperature solver
freeze_out_xf(m_GeV = 100, sigmav_GeV2 = sigmav)
#> [1] 20.8065           # x_f = m/T_f  (classic WIMP range: 20-25)

# Full ODE integration of the pebble equation (requires deSolve)
# install.packages("deSolve")
res <- solve_relic_abundance(m_GeV = 100, sigmav_GeV2 = sigmav)
cat("Omega_chi * h^2 =", res$Omega_h2, "\n")
#> Omega_chi * h^2 = 0.1068
#> # Planck 2018 measured: Omega_DM * h^2 = 0.1200
```

### Hydrogen Recombination: The Peebles Equation

The Peebles equation tracks the free-electron fraction x_e during
cosmological hydrogen recombination (z ~ 1100). It uses **SI units**.

$$\frac{dx_e}{d\ln a} = \frac{C}{H}\left[\beta_B(1-x_e) - n_H\,\alpha_B\,x_e^2\right]$$

The Peebles C-factor accounts for the probability that an excited hydrogen
atom reaches the ground state before being re-ionized.

``` r
# Approximate inputs at z ~ 1100 (recombination epoch)
H_rec   <- 3.3e-15   # s^-1
n_H_rec <- 400e6     # m^-3
alpha_B <- 2.6e-19   # m^3 s^-1  (case-B recombination coefficient)
beta_B  <- 4.0e-15   # s^-1      (photoionization rate from n = 2)

res <- peebles_rhs(
  lna     = log(1 / 1101),
  xe      = 0.5,
  H       = H_rec,
  n_H     = n_H_rec,
  alpha_B = alpha_B,
  beta_B  = beta_B
)

res
#>          dxe_dlna                 C lambda_alpha_escape 
#>      -7878.224491          1.000000          0.230818
```

The negative `dxe_dlna` confirms that at x_e = 0.5 the plasma is actively
recombining. The C-factor ≈ 1 means the Lyman-alpha escape and two-photon
channels dominate over photoionization — recombination is efficient.

## What's New in v0.3.0

- **Thermal Cosmology Module** — 9 new exported functions (`hubble_radiation`,
  `g_star_eff`, `entropy_density`, `equilibrium_number_density`,
  `equilibrium_yield`, `boltzmann_pebble_rhs`, `solve_relic_abundance`,
  `freeze_out_xf`, `peebles_rhs`) covering the full early-universe thermal history.
- **47 new unit tests** — all functions tested for physical correctness,
  scaling laws, and error handling.
- **CRAN check**: `0 errors | 0 warnings | 0 notes`.

#### *Many more functions are coming soon!*
