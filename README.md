# astronomR: Astronomy and Cosmology Analysis in R 🌌✨

[![CRAN status](https://www.r-pkg.org/badges/version/astronomR)](https://cran.r-project.org/package=astronomR)
[![Dev version](https://img.shields.io/badge/dev%20version-0.4.0-blue?logo=github)](https://github.com/samrit2442/astronomR)
[![CRAN checks](https://badges.cranchecks.info/summary/astronomR.svg)](https://cran.r-project.org/web/checks/check_results_astronomR.html)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

[![CRAN downloads (grand total)](https://cranlogs.r-pkg.org/badges/grand-total/astronomR)](https://cran.r-project.org/package=astronomR)
[![CRAN downloads (last month)](https://cranlogs.r-pkg.org/badges/last-month/astronomR)](https://cran.r-project.org/package=astronomR)
[![GitHub stars](https://img.shields.io/github/stars/samrit2442/astronomR?style=social)](https://github.com/samrit2442/astronomR)
[![Last commit](https://img.shields.io/github/last-commit/samrit2442/astronomR)](https://github.com/samrit2442/astronomR/commits/master)

An R package designed to bridge the gap between data science and the cosmos!

## Why {astronomR}?

While R is widely used in data science and statistical computing, there has been a lack of tools specifically tailored for astronomical data. {astronomR} aims to fill this gap, offering easy-to-use functions and tools for anyone looking to explore space and cosmology through data. Whether you're an astrophysicist or just a data enthusiast, {astronomR} is here to help you navigate the universe.

## Installing this Package

Install the stable release from CRAN:

```r
install.packages("astronomR")
```

Or install the development version from GitHub:

```r
# Install devtools package if you don't have it
# install.packages("devtools")

devtools::install_github("samrit2442/astronomR")
```

## Introduction

This package was developed for astronomy, cosmological computation, and analysis with R. The source code can be found here <https://github.com/samrit2442/astronomR>.

## Function Overview

| Category                | Functions                                                                                                                                                                                                   |
| ------------------------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **Angular Conversions**  | `deg_to_hms()`, `hms_to_deg()`, `deg_to_dms()`, `dms_to_deg()`, `deg_to_rad()`, `rad_to_deg()`                                                                                                              |
| **Coordinates**          | `julian_day()`, `radec_to_altaz()`                                                                                                                                                                          |
| **Physical Constants**   | `constants_df`, `constant_value()`                                                                                                                                                                          |
| **Gaia Archive**         | `get_gaia_data()`                                                                                                                                                                                            |
| **Cosmology**            | `cosmology_model()`, `age_of_universe()`, `cosmic_time()`, `comoving_distance()`, `transverse_comoving_distance()`, `luminosity_distance()`, `angular_diameter_distance()`, `radius_of_curvature()`, `hubble_distance()`, `hubble_ratio()`, `redshift_to_scale_factor()`, `scale_factor_to_redshift()`, `km_to_Mpc()`, `Mpc_to_km()` |
| **Thermal Physics**      | `photon_energy_density_fn_T()`, `photon_energy_density_fn_z()`, `photon_number_density_fn_T()`, `photon_number_density_fn_z()`, `Saha_Xe()`, `soln_saha()`                                                 |
| **Thermal Cosmology**    | `hubble_radiation()`, `g_star_eff()`, `entropy_density()`, `equilibrium_number_density()`, `equilibrium_yield()`, `boltzmann_pebble_rhs()`, `solve_relic_abundance()`, `freeze_out_xf()`, `peebles_rhs()`  |
| **Drake Equation**       | `drake_equation()`                                                                                                                                                                                           |

## Some Usage and Example Codes

Normally in astronomy, we use a different sort of angular system for location. We can easily do that in our package. Let's see how. Suppose, we have an angular value of `d = 177.74208°` We want to convert it into an hour-minute-second. There is a very simple function.

```r
library(astronomR)

deg_to_hms(177.74208)
#> 11H50M58.0992S
hms_to_deg(11, 50, 58.09925)
#> [1] 177.7421
hms_to_deg(c("11h50m58.09925s", "11:50:58.09925", "11 50 58.09925"))
#> [1] 177.7421 177.7421 177.7421
```

Declinations work the same way in degrees, minutes and seconds:

```r
deg_to_dms(c(-0.5, 45.5042))
#> -0°30'0" +45°30'15.12"
dms_to_deg(c("-0°30'00\"", "+45d30m15.12s", "12:34:56", "12.5°"))
#> [1] -0.50000 45.50420 12.58222 12.50000

# A negative zero carries the sign of an angle between -1 and 0 degrees
dms_to_deg(-0, 30)
#> [1] -0.5

deg_to_rad(180)
#> [1] 3.141593
rad_to_deg(pi / 2)
#> [1] 90
```

What else can be done? Let's say we want to find the path of some star in a particular location for a time interval. This can also be done using our package.

To do that, first, we need to define the RA and Dec value of the star. Let's see how to do this. Also, let's define the time of observation and its location.

```r
ra_hour  <- 16.695      # RA in hours
dec_deg  <- 36.466667   # Dec in degrees
lat_obs  <- 52.5        # Observer's latitude (degrees, north positive)
lon_obs  <- -1.9166667  # Observer's longitude (degrees, east positive)
datetime <- as.POSIXct("1998-08-10 23:10:00", tz = "UTC")  # Observation time
```

Now, create the star location as seen from that location at the time mentioned. `radec_to_altaz()` returns the altitude and azimuth in degrees, with the azimuth measured from North through East. RA and Dec should be apparent (of-date) coordinates, and no atmospheric refraction is applied.

```r
star_location <- radec_to_altaz(ra_hour, dec_deg, lat_obs, lon_obs, datetime)
star_location
#>   altitude  azimuth
#> 1 49.16887 269.1467
print(paste("Altitude:", star_location$altitude, "degrees"))
#> [1] "Altitude: 49.1688687197424 degrees"

print(paste("Azimuth:", star_location$azimuth, "degrees"))
#> [1] "Azimuth: 269.146669462321 degrees"
```

The sidereal time behind this comes from the Julian Day, which is also available on its own:

```r
julian_day(as.POSIXct("2000-01-01 12:00:00", tz = "UTC"))
#> [1] 2451545
```

This tells us from some location, what is the position of the star so that we can use a telescope to watch it! Using this simple function, we can trace out the path any star travels. `radec_to_altaz()` is vectorised, so one call gives the whole track. Let's see how and also maybe plot it. For that, let's first make a time range for which we want to see the location.

```r
# install.packages("ggplot2")
library(ggplot2)

# Plotting Rigel's Motion
start_datetime <- as.POSIXct("2024-10-02 00:00:00", tz = "UTC")
end_datetime   <- as.POSIXct("2024-10-03 00:00:00", tz = "UTC")

# Generate timestamps at 20-minute intervals
timestamps <- seq(from = start_datetime, to = end_datetime, by = "20 mins")

observer_lat <- 43.1566   # Latitude in degrees
observer_lon <- -77.6088  # Longitude in degrees (west is negative)

# RA and Dec for Rigel (converted RA to hours)
rigel_ra  <- 78.634467 / 15  # RA in hours
rigel_dec <- -8.20164         # Dec in degrees

# One call gives the altitude and azimuth for every timestamp
rigel_positions <- data.frame(
  datetime = timestamps,
  radec_to_altaz(rigel_ra, rigel_dec, observer_lat, observer_lon, timestamps)
)
head(rigel_positions)
#>              datetime  altitude  azimuth
#> 1 2024-10-02 00:00:00 -43.57454 51.52475
#> 2 2024-10-02 00:20:00 -40.60758 56.88793
#> 3 2024-10-02 00:40:00 -37.46073 61.83007
#> 4 2024-10-02 01:00:00 -34.17019 66.41278
#> 5 2024-10-02 01:20:00 -30.76627 70.69434
#> 6 2024-10-02 01:40:00 -27.27446 74.72797

# 2D Plot for Altitude over Time
ggplot(rigel_positions, aes(x = datetime, y = altitude)) +
  geom_line() +
  labs(x = "Time", y = "Altitude (degrees)", title = "Altitude of Rigel over 24 hours")
```

![Altitude of Rigel over 24 hours](https://github.com/user-attachments/assets/e7052a09-3142-4796-a505-d60c65a991f7)

```r
# 2D Plot for Azimuth over Time
ggplot(rigel_positions, aes(x = datetime, y = azimuth)) +
  geom_line() +
  labs(x = "Time", y = "Azimuth (degrees)", title = "Azimuth of Rigel over 24 hours")
```

![Azimuth of Rigel over 24 hours](https://github.com/user-attachments/assets/80dd618d-d7fc-40b3-af08-606ed7d9846c)

```r
# 2D Scatter Plot for Azimuth vs Altitude
ggplot(rigel_positions, aes(x = azimuth, y = altitude)) +
  geom_point(color = "firebrick") +
  labs(x = "Azimuth (degrees)", y = "Altitude (degrees)", title = "Azimuth vs Altitude for Rigel over 24 hours")
```

![Azimuth vs altitude of Rigel over 24 hours](https://github.com/user-attachments/assets/46e75c1b-79c8-4163-bdd1-6a2d74311f91)

## Gaia Data Archive

The **Gaia Data Archive** is a comprehensive database that houses the data collected by the European Space Agency's Gaia mission. Launched in December 2013, Gaia is designed to create the most accurate three-dimensional map of the Milky Way galaxy by observing and cataloguing the positions, distances, and motions of over a billion stars. In Python, **Astropy** and **Astroquery** are used to import data directly. We can do the same thing in our package. `get_gaia_data()` queries the Gaia Catalogue of Nearby Stars (built from Gaia EDR3). Let's see how:

```r
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

If the archive can't be reached or doesn't answer within `timeout` seconds (60 by default), `get_gaia_data()` prints a message saying why and returns `NULL` instead of stopping with an error.

Nice, isn't it? Let's use this for some analysis. Why not create an H-R diagram?

For this, first let's convert parallax to absolute magnitude.

```r
# install.packages(c("ggplot2", "dplyr"))
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

![Hertzsprung-Russell diagram of nearby stars](https://github.com/user-attachments/assets/a29b22c9-9ca0-4048-9bf4-8e50847157c7)

## Cosmological Calculation

We can do many things related to cosmology using this package. Let's see a few of them. As we know, there are many cosmological models, and you can define these in our package with `cosmology_model()`. Here is the Planck 2015 flat Lambda-CDM model, with radiation:

```r
cosmo <- cosmology_model(h = 0.6774, Omega_m = 0.3089, Omega_Lambda = 0.6911,
                         Omega_r = 8.4e-5)
cosmo
#> astronomR cosmology (flat)
#>   H0           = 67.74 km/s/Mpc (h = 0.6774)
#>   Omega_m      = 0.3089
#>   Omega_Lambda = 0.6911
#>   Omega_r      = 8.4e-05
#>   Omega_k      = 0
#>   T_cmb        = 2.7255 K
```

Now we can directly use this model as input to other functions. We can find the age of our universe, or the cosmic time at any redshift (in Gyr after the Big Bang):

```r
age_of_universe(cosmo)                    # our universe's age, in Gyr
#> [1] 13.79729
cosmic_time(cosmo, z = c(1100, 10, 1, 0))
#> [1] 3.720316e-04 4.724720e-01 5.862440e+00 1.379729e+01
hubble_ratio(cosmo, z = c(0.5, 1, 2))     # H(z) / H0
#> [1] 1.316838 1.778664 3.006361
```

Distances come in Mpc by default:

```r
z <- c(0.1, 0.5, 1, 2)
comoving_distance(cosmo, z)
#> [1]  432.1073 1945.4991 3395.8185 5311.4318
luminosity_distance(cosmo, z)
#> [1]   475.318  2918.249  6791.637 15934.295
angular_diameter_distance(cosmo, z)
#> [1]  392.8248 1296.9994 1697.9092 1770.4773
luminosity_distance(cosmo, 1, unit = "km")
#> [1] 2.09568e+23
```

Our package can also find the radius of curvature of the universe. It is infinite for a flat model like ours, and finite for an open or closed one:

```r
radius_of_curvature(cosmo)
#> [1] Inf

open_model <- cosmology_model(Omega_m = 0.3, Omega_Lambda = 0.6, Omega_k = 0.1)
radius_of_curvature(open_model)           # Mpc
#> [1] 13995.08
```

## Thermal Cosmology of the Early Universe

The thermal cosmology module covers the complete early-universe thermal history, from the WIMP freeze-out epoch all the way through hydrogen recombination. The photon-density and Saha functions work in natural units with energies in eV; the functions below them use **natural units** (ħ = c = k_B = 1) with temperature and mass in GeV, except `peebles_rhs()`, which uses SI units.

### Photons and the Saha Equation

```r
# CMB photons today (T = 2.7255 K): number density in eV^3, and per cm^3
photon_number_density_fn_T(2.7255, "K")
#> [1] 3.155822e-12
photon_number_density_fn_T(2.7255, "K") / 1.973270e-5^3
#> [1] 410.7267

# Saha equilibrium ionization fraction X_e(z)
Saha_Xe(c(0, 1100, 1300, 1500, 1e4))
#> [1] 0.000000000 0.004787878 0.213813382 0.948969960 1.000000000

# redshift at which hydrogen is half ionized in Saha equilibrium
uniroot(soln_saha, c(1000, 2000))$root
#> [1] 1368.587
```

### Hubble Rate & Degrees of Freedom

```r
# Hubble rate H(T) in the radiation-dominated era [GeV]
hubble_radiation(100)                    # T = 100 GeV, full SM  g* = 106.75
#> [1] 1.405121e-14

hubble_radiation(1e-3, g_star = 10.75)   # T = 1 MeV, neutrino era
#> [1] 4.458963e-25

# Effective relativistic degrees of freedom g*(T), Standard Model steps
g_star_eff(c(500, 100, 1, 0.1, 1e-3))
#> [1] 106.75  95.25  61.75  10.75  10.75
g_star_eff(1e-4)                         # g*  after e+e- annihilation
#> [1] 3.36
g_star_eff(1e-4, type = "entropy")       # g*S after e+e- annihilation
#> [1] 3.91
```

### Entropy Density & Equilibrium Distributions

```r
# Entropy density s(T) = (2 pi^2 / 45) g*S T^3  [GeV^3]
entropy_density(0.1)                     # T = 100 MeV
#> [1] 0.004715478

# Maxwell-Boltzmann equilibrium number density n_eq(T)
equilibrium_number_density(T_GeV = 5, m_GeV = 100, g_dof = 2)
#> [1] 2.926345e-06

# Equilibrium yield Y_eq(x)  [x = m/T]
equilibrium_yield(x = 20, m_GeV = 100)   # near freeze-out
#> [1] 6.780416e-10
```

### WIMP Freeze-Out: The Boltzmann Pebble Equation

The **pebble equation** governs the evolution of the comoving yield Y = n/s of a thermally produced dark-matter relic:

$$\frac{dY}{dx} = -\frac{\langle\sigma v\rangle\, s}{H\, x}\left(Y^2 - Y_{\rm eq}^2\right)$$

```r
sigmav <- 2.2e-9   # GeV^-2  (typical WIMP <sigma v>)

# Evaluate dY/dx -- zero at equilibrium (fixed point), negative above it
Yeq <- equilibrium_yield(x = 20, m_GeV = 100)
boltzmann_pebble_rhs(x = 20, Y = 10 * Yeq, m_GeV = 100, sigmav_GeV2 = sigmav)
#> [1] -7.498556e-07

# Iterative freeze-out estimate x_f = m / T_f
freeze_out_xf(m_GeV = 100, sigmav_GeV2 = sigmav)
#> [1] 22.38211

# Full ODE integration of the pebble equation (requires deSolve)
# install.packages("deSolve")
res <- solve_relic_abundance(m_GeV = 100, sigmav_GeV2 = sigmav)
res$Omega_h2
#> [1] 0.104442
```

Planck 2018 measured Omega_DM h^2 = 0.120 for comparison.

### Hydrogen Recombination: The Peebles Equation

The Peebles equation tracks the free-electron fraction x_e during cosmological hydrogen recombination (z ~ 1100). It uses **SI units**. The temperature follows from the scale factor, T = T0 / a with T0 = 2.7255 K:

$$\frac{dx_e}{d\ln a} = \frac{C}{H}\left[\beta_B(1-x_e)\,e^{-E_{21}/k_B T} - n_H\,\alpha_B\,x_e^2\right]$$

The Peebles C-factor accounts for the probability that an excited hydrogen atom reaches the ground state before being re-ionized.

```r
# Physically consistent inputs at z = 1100 (T = 3001 K; Planck 2018
# cosmology, see ?peebles_rhs for how they are derived)
res <- peebles_rhs(
  lna     = -log(1 + 1100),
  xe      = 0.5,
  H       = 5.14e-14,  # s^-1
  n_H     = 2.53e8,    # m^-3
  alpha_B = 7.62e-19,  # m^3 s^-1  (case-B recombination coefficient)
  beta_B  = 590        # s^-1      (photoionization rate from n = 2)
)

res
#>            dxe_dlna                   C lambda_alpha_escape
#>        -21.59477699          0.02303118          5.68414771
```

The negative `dxe_dlna` confirms that at x_e = 0.5 the plasma is actively recombining. The small C-factor (about 0.02) is the Peebles bottleneck: most atoms that reach the n = 2 level are photoionized again before they can decay to the ground state, so recombination runs much slower than the Saha equation alone would suggest.

## What's New in v0.4.0

- **Renamed** `deg2rad()` and `rad2deg()` to `deg_to_rad()` and `rad_to_deg()`. The old names have been removed.
- **New coordinate functions:** `julian_day()` and `radec_to_altaz()`.
- **New cosmology functions:** `cosmology_model()` and the distance, time and curvature functions shown above.
- **Bug fixes:** angle conversions, `freeze_out_xf()`, `g_star_eff()`, `peebles_rhs()`, `Saha_Xe()`, `constant_value()` and `get_gaia_data()`. Physical constants are now CODATA 2018. See [NEWS.md](https://github.com/samrit2442/astronomR/blob/master/NEWS.md) for the full list, including results that changed.

#### *Many more functions are coming soon!*

## License

MIT © [Samrit Pramanik](https://github.com/samrit2442) -- see [LICENSE.md](https://github.com/samrit2442/astronomR/blob/master/LICENSE.md) for details.
