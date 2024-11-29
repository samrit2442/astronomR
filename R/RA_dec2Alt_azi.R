#'@export

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


#'@export

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
