#' @section Angle conversions:
#' \code{\link{deg_to_dms}}, \code{\link{dms_to_deg}},
#' \code{\link{deg_to_hms}} and \code{\link{hms_to_deg}} convert between
#' decimal degrees and degrees-minutes-seconds or hours-minutes-seconds;
#' \code{\link{deg_to_rad}} and \code{\link{rad_to_deg}} convert between
#' degrees and radians.
#'
#' @section Coordinates:
#' \code{\link{julian_day}} gives the Julian Day of a date-time, and
#' \code{\link{radec_to_altaz}} converts right ascension and declination to
#' altitude and azimuth for an observer at a given place and time.
#'
#' @section Physical constants:
#' \code{\link{constants_df}} is a table of physical constants and
#' \code{\link{constant_value}} looks a constant up by name.
#'
#' @section Gaia archive:
#' \code{\link{get_gaia_data}} queries the Gaia archive TAP service (an
#' internet connection is required).
#'
#' @section Cosmology:
#' \code{\link{cosmology_model}} builds a cosmological model that the other
#' functions take as their \code{cosmo} argument:
#' \code{\link{age_of_universe}}, \code{\link{cosmic_time}},
#' \code{\link{comoving_distance}}, \code{\link{luminosity_distance}},
#' \code{\link{angular_diameter_distance}},
#' \code{\link{transverse_comoving_distance}},
#' \code{\link{radius_of_curvature}}, \code{\link{hubble_distance}} and
#' \code{\link{hubble_ratio}}. Distances default to Mpc and times to Gyr.
#' \code{\link{redshift_to_scale_factor}} and
#' \code{\link{scale_factor_to_redshift}} convert between redshift and scale
#' factor; \code{\link{km_to_Mpc}} and \code{\link{Mpc_to_km}} convert
#' distances.
#'
#' @section Early-universe thermal physics:
#' Photon energy and number densities as functions of temperature or redshift
#' (\code{\link{photon_energy_density_fn_T}},
#' \code{\link{photon_energy_density_fn_z}},
#' \code{\link{photon_number_density_fn_T}},
#' \code{\link{photon_number_density_fn_z}}); hydrogen ionization from the
#' Saha equation (\code{\link{Saha_Xe}}, \code{\link{soln_saha}}); the
#' radiation-era Hubble rate and thermal bath (\code{\link{hubble_radiation}},
#' \code{\link{g_star_eff}}, \code{\link{entropy_density}},
#' \code{\link{equilibrium_number_density}}, \code{\link{equilibrium_yield}});
#' thermal relic freeze-out (\code{\link{boltzmann_pebble_rhs}},
#' \code{\link{solve_relic_abundance}}, \code{\link{freeze_out_xf}}); and
#' hydrogen recombination with the Peebles equation
#' (\code{\link{peebles_rhs}}).
#'
#' @section Drake equation:
#' \code{\link{drake_equation}} estimates the number of communicating
#' civilizations in the Milky Way.
#'
#' @importFrom tibble tibble
"_PACKAGE"
