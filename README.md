# astronomR: Astronomy and Cosmology Analysis in R 🌌✨

<!-- badges: start -->
[![CRAN version](https://img.shields.io/badge/CRAN-v0.3.0-blue)](https://cran.r-project.org/package=astronomR)
[![Dev version](https://img.shields.io/badge/dev%20version-0.3.0-blue?logo=github)](https://github.com/samrit2442/astronomR)
[![CRAN checks](https://badges.cranchecks.info/summary/astronomR.svg)](https://cran.r-project.org/web/checks/check_results_astronomR.html)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![CRAN downloads (grand total)](https://cranlogs.r-pkg.org/badges/grand-total/astronomR)](https://cran.r-project.org/package=astronomR)
[![CRAN downloads (last month)](https://cranlogs.r-pkg.org/badges/last-month/astronomR)](https://cran.r-project.org/package=astronomR)
[![GitHub stars](https://img.shields.io/github/stars/samrit2442/astronomR?style=social)](https://github.com/samrit2442/astronomR)
[![Last commit](https://img.shields.io/github/last-commit/samrit2442/astronomR)](https://github.com/samrit2442/astronomR/commits/master)
<!-- badges: end -->

{astronomR} provides a lightweight set of astronomy and cosmology tools for R. The package includes angular coordinate conversions, physical constants, Gaia Archive queries, thermal physics functions, early-universe cosmology utilities, and the Drake equation.

## Why {astronomR}?

Astronomy and cosmology often require domain-specific conversions and physics helpers. {astronomR} is designed to make these operations accessible from R, so you can work with angles, constants, Gaia data, and thermal-cosmology calculations without leaving your data-science workflow.

## Installing this package

Install the stable release from CRAN:

```r
install.packages("astronomR")
```

Or install the development version from GitHub:

```r
# Install remotes package if needed
# install.packages("remotes")

remotes::install_github("samrit2442/astronomR")
```

## What {astronomR} provides

| Category | Exported functions |
|---|---|
| Angular conversions | `deg_to_hms()`, `hms_to_deg()`, `deg_to_dms()`, `dms_to_deg()`, `deg2rad()`, `rad2deg()` |
| Physical constants | `constants_df`, `constant_value()` |
| Gaia Archive | `get_gaia_data()` |
| Cosmological unit conversions | `km_to_Mpc()`, `Mpc_to_km()` |
| Thermal physics | `photon_energy_density_fn_T()`, `photon_number_density_fn_T()`, `photon_energy_density_fn_z()`, `photon_number_density_fn_z()`, `Saha_Xe()`, `soln_saha()` |
| Thermal cosmology | `hubble_radiation()`, `g_star_eff()`, `entropy_density()`, `equilibrium_number_density()`, `equilibrium_yield()`, `boltzmann_pebble_rhs()`, `solve_relic_abundance()`, `freeze_out_xf()`, `peebles_rhs()` |
| Drake equation | `drake_equation()` |

## Examples

### Angular conversions

```r
library(astronomR)

deg_to_hms(177.74208)
#> 11H50M58.0992S

hms_to_deg(11, 50, 58.09925)
#> 177.7421

print(deg_to_dms(36.466667, type = "mat"))
#>      SIGN DEG MIN     SEC
#> [1,] "+"  36  28 0.0012

print(dms_to_deg(36, 28, 0.0012))
#> 36.46667
```

### Physical constants

```r
constants_df
constant_value("speed of light", unit = "SI")
#> $name
#> [1] "Speed of Light"
#>
#> $value
#> [1] 299792458
#>
#> $unit
#> [1] "m/s"
```

### Gaia Archive queries

```r
# Query Gaia EDR3 stars with good parallax
vars <- "source_id, ra, dec, parallax, phot_g_mean_mag, phot_bp_mean_mag, phot_rp_mean_mag"
condition <- "parallax > 50"
df <- get_gaia_data(vars, condition)
head(df)
```

### Thermal physics and recombination

```r
photon_energy_density_fn_T(2.725, unit = "K")
photon_number_density_fn_z(1100)
Saha_Xe(1300)
soln_saha(1350)
```

### Thermal cosmology and relic abundances

```r
hubble_radiation(100)              # H(T) in the radiation era [GeV]
g_star_eff(0.1)                     # effective relativistic dof
entropy_density(0.1)               # entropy density [GeV^3]
equilibrium_number_density(5, 100)
equilibrium_yield(20, 100)

sigmav <- 2.2e-9
freeze_out_xf(m_GeV = 100, sigmav_GeV2 = sigmav)
#> [1] 20.8

# Full relic integration requires deSolve
# install.packages("deSolve")
res <- solve_relic_abundance(m_GeV = 100, sigmav_GeV2 = sigmav)
res$Omega_h2
```

### Hydrogen recombination via the Peebles equation

```r
H_rec   <- 3.3e-15   # s^-1
n_H_rec <- 400e6     # m^-3
alpha_B <- 2.6e-19   # m^3 s^-1
beta_B  <- 4.0e-15   # s^-1

peebles_rhs(
  lna     = log(1 / 1101),
  xe      = 0.5,
  H       = H_rec,
  n_H     = n_H_rec,
  alpha_B = alpha_B,
  beta_B  = beta_B
)
```

### Drake equation

```r
result <- drake_equation()
result$N
#> [1] 1500
result$interpretation
```

## Notes

- `get_gaia_data()` requires an active internet connection.
- `solve_relic_abundance()` requires the `deSolve` package.
- `peebles_rhs()` uses SI units for all inputs.

## Current version

This README describes version 0.3.0 of {astronomR}.
