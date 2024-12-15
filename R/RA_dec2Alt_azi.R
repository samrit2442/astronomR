#' Calculate Julian Day from Datetime
#'
#' This function calculates the Julian Day (JD) for a given datetime. 
#' The Julian Day is a continuous count of days and fractions of a day since January 1, 4713 BC (noon UTC).
#'
#' @param datetime A `POSIXct` object representing the datetime for which the Julian Day is calculated.
#'
#' @return A numeric value representing the Julian Day (JD).
#' @export
#'
#' @examples
#' julian_day(as.POSIXct("2024-01-01 12:00:00", tz = "UTC"))
#' # Returns the Julian Day for January 1, 2024, at noon UTC
julian_day <- function(datetime) {
  year <- as.numeric(format(datetime, "%Y"))
  month <- as.numeric(format(datetime, "%m"))
  day <- as.numeric(format(datetime, "%d"))
  hour <- as.numeric(format(datetime, "%H")) + as.numeric(format(datetime, "%M")) / 60

  if (month <= 2) {
    year <- year - 1
    month <- month + 12
  }

  A <- floor(year / 100)
  B <- 2 - A + floor(A / 4)

  jd <- floor(365.25 * (year + 4716)) + floor(30.6001 * (month + 1)) + day + B - 1524.5 + (hour / 24)

  return(jd)
}

#' Convert Right Ascension and Declination to Altitude and Azimuth
#'
#' This function converts celestial coordinates (Right Ascension and Declination) into 
#' horizontal coordinates (Altitude and Azimuth) for a given observer's latitude, longitude, and datetime.
#'
#' @param ra A numeric value representing the Right Ascension (RA) in hours.
#' @param dec A numeric value representing the Declination (Dec) in degrees.
#' @param lat A numeric value representing the observer's latitude in degrees.
#' @param lon A numeric value representing the observer's longitude in degrees.
#' @param datetime A `POSIXct` object representing the datetime of observation.
#'
#' @return A list with two elements:
#' \itemize{
#'   \item \code{altitude}: The Altitude of the object in degrees.
#'   \item \code{azimuth}: The Azimuth of the object in degrees.
#' }
#' @export
#'
#' @examples
#' RA_dec2Alt_azi(ra = 5.0, dec = -5.0, lat = 40.0, lon = -75.0, datetime = as.POSIXct("2024-01-01 00:00:00", tz = "UTC"))
#' # Returns the Altitude and Azimuth for the given parameters
RA_dec2Alt_azi <- function(ra, dec, lat, lon, datetime) {

  jd <- julian_day(datetime)
  # Calculate sidereal time at Greenwich
  gst <- 18.697374558 + 24.06570982441908 * (jd - 2451545.0)
  gst <- gst %% 24  # Convert to 0-24 hour range
  # Convert GST to LST
  lst_hours <- gst + lon / 15.0
  ra_deg <- ra * 15.0
  ha_deg <- lst_hours * 15.0 - ra_deg
  ha_rad <- deg2rad(ha_deg)
  dec_rad <- deg2rad(dec)
  lat_rad <- deg2rad(lat)
  sin_alt <- sin(dec_rad) * sin(lat_rad) + cos(dec_rad) * cos(lat_rad) * cos(ha_rad)
  alt_rad <- asin(sin_alt)

  cos_az <- (sin(dec_rad) - sin(alt_rad) * sin(lat_rad)) / (cos(alt_rad) * cos(lat_rad))
  az_rad <- acos(cos_az)
  if (sin(ha_rad) > 0) {
    az_rad <- 2 * pi - az_rad
  }
  alt_deg <- rad2deg(alt_rad)
  az_deg <- rad2deg(az_rad)
  return(list(altitude = alt_deg, azimuth = az_deg))
}
